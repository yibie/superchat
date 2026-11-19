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
(require 'superchat-permission)
(require 'superchat-tools)
(require 'superchat-recall)
(require 'superchat-view)
(require 'superchat-context)
(require 'superchat-request)

(defvar superchat-agent--runs (make-hash-table :test #'equal)
  "Timeline name to its run, while it has one.")

(defvar superchat-agent-retry-waits '(2 4 8 16 32)
  "Seconds to wait before each retry of a failed request; their number is
the number of retries.")

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
  "End RUN with STATUS (done, cancelled, error, waiting): the one way a run ends.
A run that waits for an answer leaves its calls without results.
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
              (when-let* ((timer (plist-get run :timer)))
                (setf (plist-get run :timer) nil)
                (cancel-timer timer))
              (when-let* ((stop (plist-get run :stop)))
                (setf (plist-get run :stop) nil)
                (superchat-agent--try tape stop))
              (superchat-agent--try
               tape (lambda ()
                      (dolist (call (unless (eq status 'waiting)
                                      (superchat-agent--unanswered run)))
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
  "Record RESPONSE, run its calls in order, make the next request; at the round
limit, record the answer and end RUN as stopped."
  (let ((calls (plist-get response :calls))
        (last (plist-get run :last)))
    (cl-incf (plist-get run :rounds))
    (setf (plist-get run :text) (plist-get response :text))
    ;; A call of the last response is not run and not recorded: no tool was
    ;; called, and the person's answer must not claim otherwise.
    (superchat-tape-append (plist-get run :tape) 'assistant (plist-get response :text)
                           :data (list :model (plist-get response :model)
                                       :calls (unless last calls)))
    (superchat-agent--emit run)
    (when (superchat-agent--live run)
      (cond (last (superchat-agent--end run 'error (superchat-agent--stopped run)))
            (calls (superchat-agent--calls run calls))
            (t (superchat-agent--end run 'done))))))

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
            (ok nil) (why nil) (result nil)
            (superchat-permission-ask-function
             (if (plist-get run :unattended) (lambda (&rest _) 'later)
               superchat-permission-ask-function)))
        (setf (plist-get run :calling) t)
        (unwind-protect
            (condition-case err
                (setq stop (superchat-tool-call
                            (plist-get run :tape) call
                            (lambda (entry)
                              (if (not returned)
                                  (setq answered t result entry)
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
                ;; Nobody can answer now: the run ends and the call waits.
                ((eq (plist-get result :kind) 'event) (superchat-agent--end run 'waiting))
                ((eq (plist-get run :status) 'cancelled) (superchat-agent--end run 'cancelled))
                ((and (not ok) (superchat-agent--live run))
                 (superchat-agent--end
                  run 'error "A tool call was interrupted before it returned."))))
        (superchat-agent--emit run)
        (setq waiting (not answered))))
    (when (and (not waiting) (superchat-agent--live run))
      (superchat-agent--step run))))

(defconst superchat-agent--limit-note
  "The limit of model responses in one run is reached. Call no tool now, even if one would help: answer with what you have, saying what you did, what is left, and what the person should check."
  "What the system prompt says of a run's last request, the round limit reached.")

(defun superchat-agent--stopped (run)
  "The `error' event of a run RUN the round limit stopped: what it did, asked
of the Tape, since the model's own account may be empty."
  (let* ((entries (seq-filter (lambda (e) (> (plist-get e :id) (plist-get run :user-id)))
                              (superchat-view 'conversation :tape (plist-get run :tape))))
         (calls (cl-loop for e in entries when (eq (plist-get e :kind) 'assistant)
                         sum (length (plist-get (plist-get e :data) :calls)))))
    (format "Stopped: %d model responses without an answer; above is what it did: %d tool call%s."
            superchat-max-rounds calls (if (= calls 1) "" "s"))))

(defun superchat-agent--step (run)
  "Make the next request of RUN; at the round limit, the last one, which asks
for an answer."
  (let* ((tape (plist-get run :tape))
         (budget (superchat-request-budget (plist-get run :model)))
         (entries (superchat-view 'conversation :tape tape))
         (messages (superchat-request-fit entries (plist-get run :user-id) budget))
         (size (superchat-request-kept-size entries (plist-get run :user-id)))
         (model (or (plist-get run :model) superchat-llm-model))
         (tools (plist-get run :tools))
         (last (>= (plist-get run :rounds) superchat-max-rounds)))
    (setf (plist-get run :last) last)
    (cond
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
                     :system (concat (superchat-context-system tape (plist-get run :system))
                                     (and last (concat "\n\n" superchat-agent--limit-note)))
                     ;; The tools stay: a provider that has no tool in the
                     ;; request may leak its call syntax as text. A call of
                     ;; this answer is dropped by --respond.
                     :tools (unless (eq tools 'none) (superchat-tool-specs tools))
                     :model (plist-get run :model)
                     :on-text (lambda (text)
                                (when-let* (((superchat-agent--live run))
                                            (on-text (plist-get run :on-text)))
                                  (funcall on-text text)))
                     :on-done (lambda (response)
                                (superchat-agent--answered-request run)
                                (setf (plist-get run :tries) 0)
                                (when (superchat-agent--live run)
                                  (superchat-agent--guard
                                   run (lambda () (superchat-agent--respond run response)))))
                     :on-error (lambda (failure)
                                 (superchat-agent--answered-request run)
                                 (when (superchat-agent--live run)
                                   (superchat-agent--guard
                                    run (lambda ()
                                          (unless (superchat-agent--retry run failure)
                                            (superchat-agent--end
                                             run 'error (plist-get failure :message))))))))))))))))

(defun superchat-agent--retry (run failure)
  "Make RUN's failed request again after a wait, when FAILURE may pass by
waiting and retries are left; nil otherwise.  Nothing of the failed request
is on the Tape, so the next request is built from it as the first was."
  (let* ((tries (plist-get run :tries))
         (wait (nth tries superchat-agent-retry-waits))
         (kind (plist-get failure :kind))
         (status (plist-get failure :status)))
    (when (and wait (memq kind '(rate-limit server timeout network empty)))
      (setf (plist-get run :tries) (1+ tries))
      (superchat-tape-append
       (plist-get run :tape) 'event
       (format "Request failed: %s. Trying again in %s s (%d/%d)."
               (truncate-string-to-width
                (car (split-string (plist-get failure :message) "\n")) 120 nil nil "...")
               wait (1+ tries) (length superchat-agent-retry-waits))
       :data (list :type "retry" :kind (symbol-name kind) :status status :wait wait
                   :attempt (1+ tries) :of (length superchat-agent-retry-waits)))
      (superchat-agent--emit run)
      (when (superchat-agent--live run)
        (setf (plist-get run :timer)
              (run-at-time wait nil
                           (lambda ()
                             (setf (plist-get run :timer) nil)
                             (when (superchat-agent--live run)
                               (superchat-agent--guard
                                run (lambda () (superchat-agent--step run))))))))
      t)))

(defun superchat-agent--add-user (run)
  "Append the user entry of RUN, if it is unsent."
  (when (plist-get run :unsent)
    (setf (plist-get run :unsent) nil
          (plist-get run :user-id)
          (plist-get (superchat-tape-append
                      (plist-get run :tape) 'user (plist-get run :request)
                      :scope (plist-get run :scope) :refs (plist-get run :refs)
                      :data (append (and (plist-get run :origin)
                                         (list :origin (plist-get run :origin)))
                                    (and (plist-get run :ephemeral) (list :ephemeral t))))
                     :id))))

(defun superchat-agent--mark-interrupted (tape)
  "When the last run of TAPE never ended (Emacs stopped during it), append a
result for each call it left unanswered and an `interrupted' event, so the
model and the person know that a call may have had its effect."
  (when-let* ((user (car (superchat-tape-entries :tape tape :kinds '(user) :newest t :limit 1))))
    (let* ((tail (superchat-tape-entries :tape tape :after (plist-get user :id)))
           (before (car (superchat-tape-entries :tape tape :before (plist-get user :id)
                                                :newest t :limit 1)))
           (last (or (car (last (seq-remove (lambda (e) (memq (plist-get e :kind) '(event note)))
                                            tail)))
                     user))
           (answered (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id))
                             (seq-filter (lambda (e) (eq (plist-get e :kind) 'tool)) tail))))
      (when (and (not (seq-find (lambda (e) (member (plist-get (plist-get e :data) :type)
                                                    '("error" "cancelled")))
                                tail))
                 (pcase (plist-get last :kind)
                   ;; A failed handoff ends with its error event, then the user entry.
                   ('user (not (equal (plist-get (plist-get before :data) :type) "error")))
                   ('assistant (plist-get (plist-get last :data) :calls))
                   ('tool t)))
        (dolist (e tail)
          (dolist (call (and (eq (plist-get e :kind) 'assistant) (plist-get (plist-get e :data) :calls)))
            (unless (member (plist-get call :id) answered)
              (superchat-tape-append
               tape 'tool "Interrupted: Emacs stopped while this call ran; it may have had effects."
               :data (list :call-id (plist-get call :id) :name (plist-get call :name) :error t)))))
        (superchat-tape-append tape 'event "The last run was cut off: Emacs stopped before it ended."
                               :data (list :type "interrupted"))))))

(defun superchat-agent--begin (run)
  "Append the user entry of RUN and make its first request, unless ON-ENTRY
ended the run on that entry."
  (superchat-agent--add-user run)
  (superchat-agent--emit run)
  (when (superchat-agent--live run)
    (superchat-agent--step run)))

(defun superchat-agent--make-run (tape &rest keys)
  "A run of TAPE with KEYS.  Every key is there from the start: `setf' of a
missing one would make a new list."
  (append keys (list :tape tape :status 'running :rounds 0 :seq 0 :handle nil :calling nil
                     :stop nil :done nil :text nil :error nil :tries 0 :timer nil)))

(defun superchat-question (tape)
  "The `question' event of timeline TAPE that waits for its answer, or nil."
  (when-let* ((question (seq-find (lambda (e) (equal (plist-get (plist-get e :data) :type)
                                                     "question"))
                                  (superchat-tape-entries :tape tape :kinds '(event)
                                                          :newest t :limit 50)))
              (id (plist-get (plist-get question :data) :call-id)))
    (unless (seq-find (lambda (e) (equal (plist-get (plist-get e :data) :call-id) id))
                      (superchat-tape-entries :tape tape :after (plist-get question :id)))
      question)))

(cl-defun superchat-run (tape text &key scope origin tools system model ephemeral
                              unattended refs on-text on-entry on-done)
  "Append TEXT as a user entry of timeline TAPE and answer it; return the run.
SCOPE is that of a new timeline's first entry, by default that of
`default-directory'.
TOOLS is a list of tool names, nil for every tool, or `none'.  SYSTEM is
extra text for the system prompt.  ON-TEXT gets streamed text, ON-ENTRY each
entry appended, ON-DONE the run, once, with :status done, cancelled or error,
:text the last answer of this run and, for error, :error.  EPHEMERAL marks
the user entry: the timeline is one question, not a conversation; REFS go on
it too.  An UNATTENDED run asks nobody: it ends `waiting' at a question."
  (when (gethash tape superchat-agent--runs)
    (error "Timeline %s is busy with a run or a handoff" tape))
  (when (superchat-question tape)
    (error "Timeline %s waits for an answer; give it with `superchat-answer'" tape))
  (superchat-agent--mark-interrupted tape)
  (let* ((last (car (superchat-tape-entries :tape tape :newest t :limit 1)))
         (id (if last (plist-get last :id) 0))
         ;; Every key is there from the start: `setf' of a missing one makes a new list.
         (run (superchat-agent--make-run
               tape
               ;; Unsent from the start: whatever signals, the user's text is written.
               :unsent t :user-id nil
               :scope (unless last (or scope (superchat-tape-scope-of default-directory)))
               :request text :origin origin :ephemeral ephemeral :first id :seen id
               :tools tools :system system :model model :unattended unattended :refs refs
               :on-text on-text :on-entry on-entry :on-done on-done)))
    (puthash tape run superchat-agent--runs)
    (superchat-agent--guard
     run (lambda ()
           (if (and last (> (superchat-request-size
                             (superchat-request-messages (superchat-view 'conversation :tape tape)))
                            (superchat-request-budget model)))
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

(cl-defun superchat-answer (tape answer &key unattended on-text on-entry on-done)
  "Answer the open question of timeline TAPE with ANSWER (`allow', `deny' or
`always') and go on with the run it ended: that call, the calls after it in
its response, then the next request.  Return the run."
  (when (gethash tape superchat-agent--runs)
    (error "Timeline %s is busy with a run or a handoff" tape))
  (let* ((question (or (superchat-question tape)
                       (error "Timeline %s has no question waiting" tape)))
         (id (plist-get (plist-get question :data) :call-id))
         (head (car (superchat-tape-entries :tape tape :kinds '(assistant) :newest t
                                            :before (plist-get question :id) :limit 1)))
         (calls (seq-drop-while (lambda (c) (not (equal (plist-get c :id) id)))
                                (plist-get (plist-get head :data) :calls)))
         (user (car (superchat-tape-entries :tape tape :kinds '(user) :newest t :limit 1)))
         (run nil))
    (superchat-permission-answer tape id answer)
    (setq run (superchat-agent--make-run
               tape :unsent nil :user-id (plist-get user :id)
               ;; The calls of the response it goes on with are its own.
               :first (1- (plist-get head :id))
               :seen (plist-get (car (superchat-tape-entries :tape tape :newest t :limit 1)) :id)
               :unattended unattended :on-text on-text :on-entry on-entry :on-done on-done))
    (puthash tape run superchat-agent--runs)
    (superchat-agent--guard run (lambda () (superchat-agent--calls run calls)))
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
                            (plist-get (superchat-request-message anchor) :text) budget)))
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
  (let* ((budget (superchat-request-budget model))
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
