;;; superchat-agent.el --- The loop: a request, its tool rounds, cancel, handoff -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronous: `superchat-run' returns after the first request is started and
;; the rest happens in the callbacks of `superchat-llm-request'.  A scripted
;; model answers inside the request call, so every callback must be correct
;; when it fires before the request function has returned.  The Tape is the
;; only state of a conversation; a run is a plist (the handle, the callbacks,
;; the round count, the status) and nothing about earlier rounds.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-llm)
(require 'superchat-tools)
(require 'superchat-recall)
(require 'superchat-view)
(require 'superchat-context)

(defvar superchat-agent--runs (make-hash-table :test #'equal)
  "Timeline name to its run, while it has one.")

(defun superchat-agent--message (entry)
  "The neutral message of ENTRY, or nil when the model is told nothing."
  (let ((text (plist-get entry :text))
        (data (plist-get entry :data)))
    (pcase (plist-get entry :kind)
      ('user (list :role 'user :text text))
      ('assistant (list :role 'assistant :text text :calls (plist-get data :calls)))
      ('tool (list :role 'tool :text text :call-id (plist-get data :call-id)
                   :name (plist-get data :name)))
      ('anchor (unless (string-empty-p text)
                 (list :role 'user
                       :text (concat "Summary of the conversation so far:\n" text
                                     (if-let* ((next (plist-get data :next))
                                               ((not (string-empty-p next))))
                                         (concat "\n\nWhat comes next:\n" next)
                                       ""))))))))

(defun superchat-agent--budget (&optional model)
  "The budget in characters of a run of MODEL (default `superchat-llm-model').
`superchat-context-chars' when a number; else half the model's context
window, a token counting as a character; else 120000."
  (cond ((numberp superchat-context-chars) superchat-context-chars)
        ((when-let* ((window (superchat-llm-context-window model))) (floor window 2)))
        (t 120000)))

(defun superchat-agent--messages (entries)
  "The messages for ENTRIES, as a request sends them when nothing is left out."
  (cl-loop for g in (superchat-agent--grouped entries) append (cadr g)))

(defun superchat-agent--size (messages)
  "The size of MESSAGES: their texts and the printed arguments of their calls.
The handoff check and the budget of a request both use this."
  (cl-loop for m in messages
           sum (+ (length (plist-get m :text))
                  (cl-loop for c in (plist-get m :calls)
                           sum (length (format "%S" (plist-get c :args)))))))

(defun superchat-agent--groups (entries)
  "ENTRIES in lists: an assistant entry with the tool entries that answer its
calls and every event and note entry up to the next entry of another kind, and
every other entry alone.  A tool entry that answers no call of the assistant
entry heading its group is in no group: no request ever has it."
  (let (groups)
    (dolist (e entries)
      (let* ((kind (plist-get e :kind)) (head (car (last (car groups)))))
        (cond ((not (and (memq kind '(tool event note)) (eq (plist-get head :kind) 'assistant)))
               (unless (eq kind 'tool) (push (list e) groups)))
              ((or (not (eq kind 'tool))
                   (cl-some (lambda (c) (equal (plist-get c :id)
                                               (plist-get (plist-get e :data) :call-id)))
                            (plist-get (plist-get head :data) :calls)))
               (push e (car groups))))))
    (nreverse (mapcar #'nreverse groups))))

(defun superchat-agent--answered (group)
  "The messages of GROUP.  A call its tool entries do not answer (Emacs stopped
while it ran) is not sent, nor an assistant message left with nothing."
  (let* ((ids (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id)) (cdr group)))
         (messages (delq nil (mapcar #'superchat-agent--message group)))
         (head (car messages)))
    (if (not (eq (plist-get head :role) 'assistant))
        messages
      (let ((calls (cl-remove-if-not (lambda (c) (member (plist-get c :id) ids))
                                     (plist-get head :calls))))
        (if (and (null calls) (string-empty-p (plist-get head :text)))
            (cdr messages)
          (cons (plist-put (copy-sequence head) :calls calls) (cdr messages)))))))

(defun superchat-agent--grouped (entries)
  "The groups of ENTRIES, each as (ENTRIES MESSAGES)."
  (mapcar (lambda (g) (list g (superchat-agent--answered g)))
          (superchat-agent--groups entries)))

(defun superchat-agent--never-left-out (groups user-id)
  "The members of GROUPS, from `superchat-agent--grouped', that a request
always keeps: those with an anchor or the entry USER-ID, and the latest
assistant entry with its results that a request sends at all."
  (let ((latest (cl-find-if (lambda (g) (eq (plist-get (car (cadr g)) :role) 'assistant))
                            groups :from-end t)))
    (cl-remove-if-not
     (lambda (g) (or (eq g latest)
                     (cl-some (lambda (e) (or (eq (plist-get e :kind) 'anchor)
                                              (eql (plist-get e :id) user-id)))
                              (car g))))
     groups)))

(defun superchat-agent--standin (n)
  "The message that stands for N entries left out of a request."
  (list :role 'user
        :text (format (concat "[%d earlier entries of this conversation are left out of "
                             "this request; recall and tape_query can read them.]")
                       n)))

(defun superchat-agent--fit (entries user-id budget)
  "The messages for ENTRIES within BUDGET.
The oldest groups go first, except an anchor, the entry USER-ID and the latest
assistant entry with its results.  One stand-in message takes the place of the
oldest group left out, so every call stays with its results and the list
still starts with what the first entry said.  The stand-in counts toward the
size; when leaving out what can be left out would not make the request
smaller than it is, nothing is left out."
  (let* ((groups (superchat-agent--grouped entries))
         (kept (superchat-agent--never-left-out groups user-id))
         (whole (cl-loop for g in groups sum (superchat-agent--size (cadr g))))
         (size whole) (dropped 0) gone)
    (cl-flet ((total ()
                (+ size (if (> dropped 0)
                            (superchat-agent--size (list (superchat-agent--standin dropped)))
                          0))))
      (dolist (g groups)
        (when (and (cadr g) (not (memq g kept)) (> (total) budget))
          (cl-decf size (superchat-agent--size (cadr g)))
          (cl-incf dropped (length (car g)))
          (push g gone)))
      (if (>= (total) whole)
          (cl-loop for g in groups append (cadr g))
        (cl-loop for g in groups
                 if (not (memq g gone)) append (cadr g)
                 else if (eq g (car (last gone))) collect (superchat-agent--standin dropped))))))

(defun superchat-agent--emit (run &optional final)
  "Give ON-ENTRY every entry appended to the timeline since the last call.
Nothing once RUN has ended, except when the ending itself says FINAL.  Then
a signal of ON-ENTRY does not keep it from the entries after that one: the
first such error is returned, for the ending to raise after ON-DONE."
  (let (failed)
    (when (or final (not (plist-get run :done)))
      (dolist (entry (superchat-tape-entries :tape (plist-get run :tape)
                                             :after (plist-get run :seen)))
        ;; ON-ENTRY may cancel, which emits the rest of this list itself.
        (when (> (plist-get entry :id) (plist-get run :seen))
          (setf (plist-get run :seen) (plist-get entry :id))
          (when-let* ((on-entry (plist-get run :on-entry)))
            (if (not final)
                (funcall on-entry entry)
              (condition-case err (funcall on-entry entry)
                ;; The first goes to whoever ended the run; no entry can hold the rest.
                (error (if failed
                           (message "Superchat: ON-ENTRY failed again: %s"
                                    (error-message-string err))
                         (setq failed err)))))))))
    failed))

(defun superchat-agent--live (run)
  "Non-nil while RUN has not ended."
  (eq (plist-get run :status) 'running))

(defun superchat-agent--try (tape thunk)
  "Call THUNK, a step of ending a run on TAPE.  What it signals is recorded
as an error event, so that no step can keep the others from happening."
  (condition-case err (funcall thunk)
    (error (superchat-tape-append tape 'event (error-message-string err)
                                  :data (list :type "error")))))

(defun superchat-agent--unanswered (run)
  "The calls of RUN's own responses that have no result on the Tape."
  (let* ((tape (plist-get run :tape)) (first (plist-get run :first))
         (answered (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id))
                           (superchat-tape-entries :tape tape :kinds '(tool) :after first))))
    (cl-loop for e in (superchat-tape-entries :tape tape :kinds '(assistant) :after first)
             append (cl-remove-if (lambda (call) (member (plist-get call :id) answered))
                                  (plist-get (plist-get e :data) :calls)))))

(defun superchat-agent--end (run status &optional why)
  "End RUN with STATUS (done, cancelled, error): the one way a run ends.
WHY is the text of the error event.  In order, each step guarded by
`superchat-agent--try': stop a tool still running; append `Cancelled.' for
each call of this run without a result; append the user entry if it was never
sent; append the event; give ON-ENTRY the new entries; free the timeline;
call ON-DONE.  Afterwards ON-ENTRY gets nothing."
  (unless (plist-get run :done)
    (let ((tape (plist-get run :tape)) (failed nil)
          (event (cond ((eq status 'cancelled) '("cancelled" . "Cancelled by the user."))
                       (why (cons "error" why)))))
      (setf (plist-get run :done) t (plist-get run :status) status
            (plist-get run :error) why)
      (cl-flet ((user ()
                  (superchat-agent--try tape (lambda () (superchat-agent--add-user run)))))
        (unwind-protect
            (progn
              (when-let* ((stop (plist-get run :stop)))
                (setf (plist-get run :stop) nil)
                (superchat-agent--try tape stop))
              (superchat-agent--try
               tape (lambda ()
                      (dolist (call (superchat-agent--unanswered run))
                        (superchat-tape-append tape 'tool "Cancelled."
                                               :data (list :call-id (plist-get call :id)
                                                           :name (plist-get call :name)
                                                           :error t)))))
              ;; The user's words come before a cancel, after a failed handoff.
              (unless (eq status 'error) (user))
              (when event
                (superchat-agent--try
                 tape (lambda () (superchat-tape-append tape 'event (cdr event)
                                                        :data (list :type (car event))))))
              (user))
          ;; Free only now: ON-DONE may start the next run, ON-ENTRY may not.
          (unwind-protect (setq failed (superchat-agent--emit run t))
            (remhash tape superchat-agent--runs)
            (when-let* ((on-done (plist-get run :on-done)))
              (let ((ok nil))
                ;; ON-DONE's own error goes to the caller; this one is shown.
                (unwind-protect (progn (funcall on-done run) (setq ok t))
                  (when (and failed (not ok))
                    (message "Superchat: ON-ENTRY failed: %s"
                             (error-message-string failed))))))))
        ;; No entry can record it now: it goes to whoever ended the run.
        (when failed (signal (car failed) (cdr failed)))))))

(defun superchat-agent--guard (run thunk)
  "Call THUNK; an error it signals ends RUN with an error event, so a run
always ends.  When RUN has already ended the error belongs to the caller."
  (condition-case err (funcall thunk)
    (error (if (superchat-agent--live run)
               (superchat-agent--end run 'error (error-message-string err))
             (signal (car err) (cdr err))))))

(defun superchat-agent--track (run thunk)
  "Call THUNK, which starts a request; keep its handle for `superchat-cancel'.
The request may already have answered (see `superchat-agent--answered-request');
then this handle is stale and is not kept."
  (let* ((seq (cl-incf (plist-get run :seq)))
         (handle (funcall thunk)))
    (when (and (= seq (plist-get run :seq)) (superchat-agent--live run))
      (setf (plist-get run :handle) handle))))

(defun superchat-agent--answered-request (run)
  "Forget the handle of RUN's request, which has answered: a later cancel must
not cancel it, and `superchat-agent--track' must not keep it."
  (cl-incf (plist-get run :seq))
  (setf (plist-get run :handle) nil))

(defun superchat-agent--respond (run response)
  "Record RESPONSE, run its calls in order, make the next request."
  (let ((calls (plist-get response :calls)))
    (cl-incf (plist-get run :rounds))
    (setf (plist-get run :text) (plist-get response :text))
    (superchat-tape-append (plist-get run :tape) 'assistant (plist-get response :text)
                           :data (list :model (plist-get response :model) :calls calls))
    (superchat-agent--emit run)
    (when (superchat-agent--live run)
      (if calls (superchat-agent--calls run calls) (superchat-agent--end run 'done)))))

(defun superchat-agent--calls (run calls)
  "Run CALLS one after the other, then make the next request.  A loop, so that
tools that answer at once do not deepen the stack; a tool that answers later
resumes it from its callback, and Emacs is free meanwhile.  :calling is non-nil
while `superchat-tool-call' has not returned: a cancel then waits for it, and
so does a non-local exit out of it, which ends the run as an error.  An error
signalled inside it is that error event and goes no further.  :stop stops a
tool that has not answered yet."
  (let ((waiting nil))
    (while (and calls (not waiting) (superchat-agent--live run))
      (let ((call (pop calls)) (rest calls) (returned nil) (answered nil) (stop nil)
            (ok nil) (why nil))
        (setf (plist-get run :calling) t)
        (unwind-protect
            (condition-case err
                (setq stop (superchat-tool-call
                            (plist-get run :tape) call
                            (lambda (_)
                              (if (not returned)
                                  (setq answered t)
                                (setf (plist-get run :stop) nil)
                                (when (superchat-agent--live run)
                                  (superchat-agent--guard
                                   run (lambda () (superchat-agent--emit run)
                                         (superchat-agent--calls run rest))))))
                              (lambda () (not (superchat-agent--live run))))
                      ok t)
              (error (setq why (error-message-string err))))
          (setq returned t)
          (setf (plist-get run :calling) nil (plist-get run :stop) (unless answered stop))
          (cond (why (superchat-agent--end run 'error why))
                ((eq (plist-get run :status) 'cancelled) (superchat-agent--end run 'cancelled))
                ((and (not ok) (superchat-agent--live run))
                 (superchat-agent--end
                  run 'error "A tool call was interrupted before it returned."))))
        (superchat-agent--emit run)
        (setq waiting (not answered))))
    (when (and (not waiting) (superchat-agent--live run))
      (superchat-agent--step run))))

(defun superchat-agent--step (run)
  "Make the next request of RUN."
  (let* ((tape (plist-get run :tape))
         (budget (superchat-agent--budget (plist-get run :model)))
         (entries (superchat-view 'conversation :tape tape))
         (messages (superchat-agent--fit entries (plist-get run :user-id) budget))
         (size (superchat-agent--size
                (cl-loop for g in (superchat-agent--never-left-out
                                   (superchat-agent--grouped entries) (plist-get run :user-id))
                         append (cadr g))))
         (model (or (plist-get run :model) superchat-llm-model))
         (tools (plist-get run :tools)))
    (cond
     ((>= (plist-get run :rounds) superchat-max-rounds)
      (superchat-agent--end
       run 'error (format "Stopped: %d model responses without an answer." superchat-max-rounds)))
     ((> size (* 2 budget))
      (superchat-agent--end
       run 'error
       (format "This request needs %d characters; the budget %s is %d. %s"
               size (if model (format "for %s" model) "with no model set") budget
               (if (numberp superchat-context-chars)
                   "Raise `superchat-context-chars' or start a new conversation."
                 "Start a new conversation."))))
     (t
      (superchat-agent--guard
       run (lambda ()
             (superchat-agent--track
              run (lambda ()
                    (superchat-llm-request
                     messages
                     :system (superchat-context-system tape (plist-get run :system))
                     :tools (unless (eq tools 'none) (superchat-tool-specs tools))
                     :model (plist-get run :model)
                     :on-text (lambda (text)
                                (when-let* (((superchat-agent--live run))
                                            (on-text (plist-get run :on-text)))
                                  (funcall on-text text)))
                     :on-done (lambda (response)
                                (superchat-agent--answered-request run)
                                (when (superchat-agent--live run)
                                  (superchat-agent--guard
                                   run (lambda () (superchat-agent--respond run response)))))
                     :on-error (lambda (failure)
                                 (superchat-agent--answered-request run)
                                 (when (superchat-agent--live run)
                                   (superchat-agent--end
                                    run 'error (plist-get failure :message)))))))))))))

(defun superchat-agent--add-user (run)
  "Append the user entry of RUN, if it is unsent."
  (when (plist-get run :unsent)
    (setf (plist-get run :unsent) nil)
    (setf (plist-get run :user-id)
          (plist-get (superchat-tape-append
                      (plist-get run :tape) 'user (plist-get run :request)
                      :scope (plist-get run :scope)
                      :data (append (and (plist-get run :origin)
                                         (list :origin (plist-get run :origin)))
                                    (and (plist-get run :ephemeral) (list :ephemeral t))))
                     :id))))

(defun superchat-agent--begin (run)
  "Append the user entry of RUN and make its first request, unless ON-ENTRY
ended the run on that entry."
  (superchat-agent--add-user run)
  (superchat-agent--emit run)
  (when (superchat-agent--live run)
    (superchat-agent--step run)))

(cl-defun superchat-run (tape text &key scope origin tools system model ephemeral
                              on-text on-entry on-done)
  "Append TEXT as a user entry of timeline TAPE and answer it; return the run.
SCOPE is that of a new timeline's first entry, by default that of
`default-directory'.
TOOLS is a list of tool names, nil for every tool, or `none'.  SYSTEM is
extra text for the system prompt.  ON-TEXT gets streamed text, ON-ENTRY each
entry appended, ON-DONE the run, once, with :status done, cancelled or error,
:text the last answer of this run and, for error, :error.  EPHEMERAL marks
the user entry: the timeline is one question, not a conversation."
  (when (gethash tape superchat-agent--runs)
    (error "Timeline %s is busy with a run or a handoff" tape))
  (let* ((last (car (superchat-tape-entries :tape tape :newest t :limit 1)))
         (id (if last (plist-get last :id) 0))
         ;; Every key is there from the start: `setf' of a missing one makes a new list.
         (run (list :tape tape :status 'running :rounds 0 :seq 0 :handle nil :calling nil
                    ;; Unsent from the start: whatever signals, the user's text is written.
                    :stop nil :unsent t :done nil :text nil :error nil :user-id nil
                    :scope (unless last (or scope (superchat-tape-scope-of default-directory)))
                    :request text :origin origin :ephemeral ephemeral :first id :seen id
                    :tools tools :system system :model model
                    :on-text on-text :on-entry on-entry :on-done on-done)))
    (puthash tape run superchat-agent--runs)
    (superchat-agent--guard
     run (lambda ()
           (if (and last (> (superchat-agent--size
                             (superchat-agent--messages (superchat-view 'conversation :tape tape)))
                            (superchat-agent--budget model)))
               ;; The anchor goes in before the user entry, so the request stays after it.
               (superchat-agent--track
                run (lambda ()
                      (superchat-agent--summarise
                       tape nil run
                       (lambda (anchor &optional why)
                         (superchat-agent--answered-request run)
                         (superchat-agent--guard
                          run (lambda ()
                                (if anchor
                                    (superchat-agent--begin run)
                                  (superchat-agent--end
                                   run 'error (format "Handoff failed: %s" why))))))
                       model)))
             (superchat-agent--begin run))))
    run))

(defun superchat-run-active (tape)
  "The run of timeline TAPE, or nil."
  (gethash tape superchat-agent--runs))

(defun superchat-cancel (tape)
  "Cancel the run or the pending handoff of timeline TAPE, if it has one.
A run with a call inside `superchat-tool-call' ends when that call returns."
  (when-let* ((run (superchat-run-active tape)))
    (let ((handoff (eq (plist-get run :status) 'handoff)))
      (when (or handoff (superchat-agent--live run))
        (when handoff
          (remhash tape superchat-agent--runs)
          (superchat-tape-append tape 'event "Cancelled by the user."
                                 :data (list :type "cancelled")))
        (when-let* ((handle (plist-get run :handle)))
          (superchat-agent--try tape (lambda () (superchat-llm-cancel handle))))
        (unless handoff
          (if (plist-get run :calling)
              (setf (plist-get run :status) 'cancelled)
            (superchat-agent--end run 'cancelled)))))))

(defun superchat-agent--summary-question (budget)
  "What a handoff asks the model: a summary of at most a quarter of BUDGET."
  (format "Summarise this conversation so that someone who has not seen it can
carry on: what was asked, what was done, what was found, what is open.
Keep the summary under %d characters.
Then write a line that starts with NEXT: and say what comes next."
          (floor budget 4)))

(defun superchat-agent--handoff-text (entries budget)
  "ENTRIES rendered for a handoff within BUDGET, as (TEXT . KEPT), KEPT the
entries TEXT was made from.  The current anchor's message, when there is
one, comes first and is never dropped; the entries after it are rendered to
what is left, oldest dropped first."
  (let* ((anchor (cl-find-if (lambda (e) (and (eq (plist-get e :kind) 'anchor)
                                              (not (string-empty-p (plist-get e :text)))))
                             entries :from-end t))
         (head (and anchor (superchat-view-clip
                            (plist-get (superchat-agent--message anchor) :text) budget)))
         (rest (if anchor (cdr (memq anchor entries)) entries))
         (body (and rest (superchat-view-render rest (- budget (length head) 2))))
         ;; The render says how many of the oldest it left out, on its first line.
         (gone (if (and body (string-match "\\`\\[\\([0-9]+\\) earlier entries left out\\]"
                                          body))
                   (string-to-number (match-string 1 body))
                 0)))
    (cons (string-join (delq nil (list head body)) "\n\n")
          (append (and anchor (list anchor)) (nthcdr gone rest)))))

(defun superchat-agent--anchor (tape name answer sources budget)
  "Append to TAPE the anchor NAME made from ANSWER, citing SOURCES; nil when
ANSWER has no summary.  The summary and the next step are each cut to a
quarter of BUDGET."
  (let* ((case-fold-search nil)
         (split (string-match "^NEXT:[ \t]*" answer))
         (after (and split (match-end 0)))
         (summary (superchat-view-clip
                   (string-trim (if split (substring answer 0 split) answer))
                   (floor budget 4)))
         (next (superchat-view-clip
                (if split (string-trim (substring answer after)) "")
                (floor budget 4))))
    (unless (string-empty-p summary)
      (superchat-tape-append tape 'anchor summary
                             :data (list :name (or name "handoff") :summary summary :next next)
                             :refs (mapcar (lambda (e) (plist-get e :id)) sources)))))

(defun superchat-agent--summarise (tape name owner on-done &optional model)
  "Ask MODEL for the summary of TAPE and append the anchor NAME.
Return the handle.  The conversation goes as one user message: a provider
refuses tool turns when the request has no tools.  ON-DONE gets the anchor, or
nil and why.  An answer that comes after OWNER, the run or handoff that asked,
left `superchat-agent--runs' (it was cancelled) is dropped."
  (let* ((budget (superchat-agent--budget model))
         (text (superchat-agent--handoff-text (superchat-view 'conversation :tape tape) budget)))
    (superchat-llm-request
     (list (list :role 'user
                 :text (concat (car text) "\n\n" (superchat-agent--summary-question budget))))
     :model model
     :on-done (lambda (response)
                (when (eq (gethash tape superchat-agent--runs) owner)
                  (let ((why nil) (anchor nil))
                    ;; A failure to write the anchor is a failed handoff, so it ends.
                    (condition-case err
                        (setq anchor (superchat-agent--anchor tape name (plist-get response :text)
                                                              (cdr text) budget)
                              why (unless anchor "The model gave no summary."))
                      (error (setq why (error-message-string err))))
                    (funcall on-done anchor why))))
     :on-error (lambda (failure)
                 (when (eq (gethash tape superchat-agent--runs) owner)
                   (funcall on-done nil (plist-get failure :message)))))))

(defun superchat-handoff (tape &optional name on-done)
  "Ask the model, with no tools, to summarise TAPE and say what comes next;
append an anchor named NAME (default \"handoff\") with that.  Return the handle.
ON-DONE gets the anchor, or nil when the model failed (an error event says so).
The timeline is busy until then."
  (when (gethash tape superchat-agent--runs)
    (error "Timeline %s is busy with a run or a handoff" tape))
  (let ((entry (list :tape tape :status 'handoff :handle nil)))
    (puthash tape entry superchat-agent--runs)
    (condition-case err
        (setf (plist-get entry :handle)
              (superchat-agent--summarise
               tape name entry
               (lambda (anchor &optional why)
                 ;; Free whatever happens: a handoff always ends.
                 (unwind-protect
                     (unless anchor
                       (superchat-tape-append tape 'event (format "Handoff failed: %s" why)
                                              :data (list :type "error")))
                   (remhash tape superchat-agent--runs)
                   (when on-done (funcall on-done anchor))))))
      (error (remhash tape superchat-agent--runs)
             (signal (car err) (cdr err))))))

(defun superchat-agent--fork-entries (tape entry-id)
  "The conversation entries of TAPE up to ENTRY-ID, and the results of the
calls of every assistant entry among them.  ENTRY-ID must be one of them."
  (let* ((entries (superchat-view 'conversation :tape tape))
         (calls (cl-loop for e in entries
                         when (and (eq (plist-get e :kind) 'assistant)
                                   (<= (plist-get e :id) entry-id))
                         append (mapcar (lambda (c) (plist-get c :id))
                                        (plist-get (plist-get e :data) :calls)))))
    (unless (cl-find entry-id entries :key (lambda (e) (plist-get e :id)))
      (error "Entry %s is not in the conversation of %s" entry-id tape))
    (cl-remove-if-not
     (lambda (e) (or (<= (plist-get e :id) entry-id)
                     (and (eq (plist-get e :kind) 'tool)
                          (member (plist-get (plist-get e :data) :call-id) calls))))
     entries)))

(defun superchat-fork (tape entry-id)
  "A new timeline, of TAPE's scope, that starts from TAPE's conversation up to
ENTRY-ID.  Its first entry is an anchor with no summary that refers to those
entries."
  (let ((refs (mapcar (lambda (e) (plist-get e :id))
                      (superchat-agent--fork-entries tape entry-id)))
        (name (superchat-tape-new-name)))
    (superchat-tape-append
     name 'anchor ""
     :data (list :name "fork" :summary "" :next "")
     :refs refs
     :scope (plist-get (car (superchat-tape-entries :tape tape :limit 1)) :scope))
    name))

(provide 'superchat-agent)
;;; superchat-agent.el ends here
