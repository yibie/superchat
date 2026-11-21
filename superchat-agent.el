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
(require 'superchat-guard)

(defvar superchat-agent--runs (make-hash-table :test #'equal)
  "Timeline name to its run, while it has one.")

(defvar superchat-agent-continues 2
  "At the `auto' level, how many times a run that reached the round limit goes
on after its look-back, its count of responses starting again.")

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

(defun superchat-agent--end (run status &optional why type)
  "End RUN with STATUS (done, cancelled, error, waiting): the one way a run ends.
A run that waits for an answer leaves its calls without results.
WHY is the text of the event, of TYPE, by default \"error\".  In order,
each step guarded by `superchat-agent--try': stop a tool still running;
append `Cancelled.' for each call of this run without a result; append the
user entry if it was never sent; append the event; give ON-ENTRY the new
entries; free the timeline; call ON-DONE.  Afterwards ON-ENTRY gets nothing."
  (unless (plist-get run :done)
    (let ((tape (plist-get run :tape)) (failed nil)
          (event (cond ((eq status 'cancelled) '("cancelled" . "Cancelled by the user."))
                       (why (cons (or type "error") why)))))
      (setf (plist-get run :done) t (plist-get run :status) status
            (plist-get run :error) (and (eq status 'error) why))
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

;;; The round limit: the look-back, the judge, the pause

(defconst superchat-agent--limit-note
  "The limit of model responses in one run is reached. Call no tool now, even if one would help. Begin with one line: \"Status: finished\" when the task is done (what follows is then your answer to the person), \"Status: blocked\" when you need the person, or \"Status: continue\" when there is more to do. Then look back over the calls of this run, with the facts below: what is done, what failed and why, what is left, the plan for the next step, and what the person should check."
  "What the system prompt says of a run's last request, the round limit reached.")

(defconst superchat-agent--resume-note
  "You reached the limit of model responses and looked back; your look-back is below, after the facts. Go on: follow its plan, and change the approach where it or the facts show what went wrong."
  "What the system prompt says of the first request after a run went on.")

(defun superchat-agent--facts (entries note)
  "NOTE, then the facts of the calls in ENTRIES, a run's entries after its
`user' entry, counted from the Tape."
  (concat "\n\n" note "\n\nFacts of this run, counted from the Tape:\n"
          (superchat-request-digest entries)))

(defun superchat-agent--status (text)
  "The status a look-back TEXT begins with, `finished', `blocked' or
`continue', and TEXT without that line: (STATUS . REST).  STATUS is nil when
the first line names none."
  ;; A model may set the line in Markdown: **Status: finished**.
  (if (let ((case-fold-search t))
        (string-match "\\`[ \t\n*`]*Status:[ \t*`]*\\(finished\\|blocked\\|continue\\)[ \t.*`]*\\(?:\n\\|\\'\\)"
                      (or text "")))
      (cons (intern (downcase (match-string 1 text))) (string-trim (substring text (match-end 0))))
    (cons nil text)))

(defun superchat-agent--paused-text (entries)
  "The text of the `paused' event of a run the round limit stopped, whose
entries after its `user' entry are ENTRIES: what it did, counted from the
Tape, since the model's own account may be empty."
  (let ((calls (cl-loop for e in entries when (eq (plist-get e :kind) 'assistant)
                        sum (length (plist-get (plist-get e :data) :calls)))))
    (format "Paused at the round limit: %d model responses, %d tool call%s. Send a message to go on."
            superchat-max-rounds calls (if (= calls 1) "" "s"))))

(defvar superchat-agent-pending-functions nil
  "Functions called with a timeline's name: non-nil when input waits for it
outside the run, so that the run, at the round limit, pauses for it.")

(defun superchat-agent--pending-p (tape steers)
  "Non-nil when input waits for timeline TAPE: STEERS, the text steered to its
run, or what `superchat-agent-pending-functions' report."
  (or steers (run-hook-with-args-until-success 'superchat-agent-pending-functions tape)))

(defconst superchat-agent--judge-prompt
  "You judge whether an agent's task is finished. A limit on its number of responses stopped the agent, not its own choice: that it stopped says nothing. You see the task, facts counted from the record of the agent's calls, and the agent's own look-back, which may be empty, wrong or hopeful: weigh the facts above it. Begin with one line: DONE when the task as asked is done; CONTINUE when the agent can do more by itself; BLOCKED only when it cannot go on without the person (a decision only they can make, a permission, information it cannot find). Then one line why."
  "The system prompt of the judge.")

(defun superchat-agent--verdict (text)
  "The verdict the judge's TEXT begins with: `done', `blocked', or `continue'
also when it names none."
  (let ((case-fold-search t))
    (if (string-match "\\`[ \t\n*`#]*\\(done\\|blocked\\|continue\\)\\b" (or text ""))
        (intern (downcase (match-string 1 text)))
      'continue)))

(defun superchat-agent--ask-judge (entries look-back model on-verdict)
  "Ask MODEL, apart from the run and with no tool, whether the task of ENTRIES,
a run's entries from its `user' entry on, is finished: its `user' entries are
the task, its calls the facts; LOOK-BACK is the run's own.  Call ON-VERDICT
with the verdict and the judge's text; a failed request is `continue'.
Return the request's handle."
  (superchat-llm-request
   (list (list :role 'user
               :text (format "Task:\n%s\n\nFacts of the run, counted from the record:\n%s\n\nThe agent's look-back:\n%s"
                             (mapconcat (lambda (e) (plist-get e :text))
                                        (seq-filter (lambda (e) (eq (plist-get e :kind) 'user)) entries)
                                        "\n\nThen the person added:\n")
                             (superchat-request-digest entries) look-back)))
   :system superchat-agent--judge-prompt :model model
   :on-done (lambda (response)
              (let ((text (string-trim (or (plist-get response :text) ""))))
                (funcall on-verdict (superchat-agent--verdict text) text)))
   :on-error (lambda (failure)
               (funcall on-verdict 'continue
                        (concat "The judge's request failed: " (plist-get failure :message))))))

(defun superchat-agent--respond (run response)
  "Record RESPONSE, run its calls in order, make the next request; at the round
limit, let the judge decide when RUN may go on, else end it."
  (let* ((calls (plist-get response :calls))
         (last (plist-get run :last))
         ;; A look-back begins with its status; the entry keeps the rest.
         (status (and last (car (superchat-agent--status (plist-get response :text)))))
         (text (if last (cdr (superchat-agent--status (plist-get response :text)))
                 (plist-get response :text))))
    (cl-incf (plist-get run :rounds))
    (if (and last (eq (superchat-permission-level (plist-get run :tape)) 'auto)
             (< (plist-get run :continues) superchat-agent-continues)
             (not (superchat-agent--pending-p (plist-get run :tape) (plist-get run :steers))))
        (superchat-agent--judge run text)
      ;; A call of the last response is not run and not recorded: no tool was
      ;; called, and the person's answer must not claim otherwise.
      (superchat-agent--answer run text (plist-get response :model) (unless last calls))
      (when (superchat-agent--live run)
        (cond ((and last (eq status 'finished)) (superchat-agent--end run 'done))
              ;; Not a failure: the look-back is its answer, and the person goes on.
              (last (superchat-agent--pause run))
              (calls (superchat-agent--calls run calls))
              (t (superchat-agent--end run 'done)))))))

(defun superchat-agent--answer (run text model calls)
  "Append RUN's `assistant' entry of TEXT, MODEL and CALLS, and give it out.
An answer's Sources: line becomes its :refs."
  (pcase-let ((`(,text ,refs ,dropped)
               (if calls (list text nil 0)
                 (superchat-request-sources text (plist-get run :tape) (or (plist-get run :user-id) 0)))))
    (setf (plist-get run :text) text)
    (superchat-tape-append (plist-get run :tape) 'assistant text :refs refs
                           :data (append (list :model model :calls calls)
                                         (and (> dropped 0) (list :refs-dropped dropped)))))
  (superchat-agent--emit run))

(defun superchat-agent--judge (run look-back)
  "RUN could go on after LOOK-BACK: a judge apart from it decides.  `continue'
goes on, with the look-back an event and out of later requests; `done' ends
RUN with the look-back its answer; `blocked' pauses it."
  (let ((tape (plist-get run :tape)))
    (superchat-agent--track
     run (lambda ()
           (superchat-agent--ask-judge
            (superchat-agent--since run) look-back (plist-get run :model)
            (lambda (verdict why)
              (superchat-agent--answered-request run)
              (when (superchat-agent--live run)
                (superchat-agent--guard
                 run (lambda ()
                       (superchat-tape-append tape 'event (format "Judged %s: %s" verdict why)
                                              :data (list :type "verdict" :verdict (symbol-name verdict)))
                       (superchat-agent--emit run)
                       (if (and (eq verdict 'continue)
                                (not (superchat-agent--pending-p tape (plist-get run :steers))))
                           (superchat-agent--go-on run look-back)
                         (superchat-agent--answer run look-back nil nil)
                         (if (eq verdict 'done) (superchat-agent--end run 'done)
                           (superchat-agent--pause run))))))))))))

(defun superchat-agent--go-on (run look-back)
  "RUN reached the round limit at the `auto' level and the judge said go on:
record LOOK-BACK as an event, start its count of responses again, and make
the next request, which alone sees the look-back."
  (cl-incf (plist-get run :continues))
  (setf (plist-get run :rounds) 0 (plist-get run :resumed) look-back)
  (superchat-tape-append (plist-get run :tape) 'event (concat "Look-back: " look-back)
                         :data '(:type "look-back"))
  (superchat-tape-append (plist-get run :tape) 'event
                         (format "Round limit reached; going on (%d/%d)."
                                 (plist-get run :continues) superchat-agent-continues)
                         :data (list :type "continued" :count (plist-get run :continues)))
  (superchat-agent--emit run)
  (superchat-agent--step run))

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
                              (lambda () (not (superchat-agent--live run)))
                              (plist-get run :offered))
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

(defun superchat-agent--since (run &optional entries)
  "The entries of RUN from its `user' entry on, of ENTRIES or of its timeline."
  (seq-filter (lambda (e) (>= (plist-get e :id) (plist-get run :user-id)))
              (or entries (superchat-view 'conversation :tape (plist-get run :tape)))))

(defun superchat-agent--pause (run)
  "End RUN, at the round limit, `done' and paused: the person goes on."
  (setf (plist-get run :paused) t)
  (superchat-agent--end run 'done (superchat-agent--paused-text (superchat-agent--since run)) "paused"))

(defun superchat-agent--step (run)
  "Make the next request of RUN; at the round limit, the last one, which asks
for an answer.  First the steered texts go in as `user' entries; at the limit
with input waiting, the run pauses instead, so that the input drives the next."
  (if (not (and superchat-max-rounds (>= (plist-get run :rounds) superchat-max-rounds)))
      (when-let* ((steers (plist-get run :steers)))
        (setf (plist-get run :steers) nil)
        (dolist (text steers)
          (superchat-tape-append (plist-get run :tape) 'user text :data (list :steer t)))
        (superchat-agent--emit run))
    (when (superchat-agent--pending-p (plist-get run :tape) (plist-get run :steers)) (superchat-agent--pause run)))
  (when (superchat-agent--live run) (superchat-agent--request run)))

(defun superchat-agent--request (run)
  "Make the next request of RUN, the steered texts in."
  (let* ((tape (plist-get run :tape))
         (budget (superchat-request-budget (plist-get run :model)))
         (entries (superchat-view 'conversation :tape tape))
         (messages (superchat-request-fit entries (plist-get run :user-id) budget))
         (size (superchat-request-kept-size entries (plist-get run :user-id)))
         (model (or (plist-get run :model) superchat-llm-model))
         (tools (plist-get run :tools))
         ;; The skills' interfaces are read again, so an edited one counts.
         (_ (superchat-context-interfaces tape))
         ;; Worked out for each request: a tool loaded in this run is offered next.
         (names (pcase tools
                  ('none nil)
                  ('nil (superchat-tool-offered tape))
                  (`(except . ,except) (superchat-tool-offered tape except))
                  (_ tools)))
         (catalog (and (or (null tools) (eq (car-safe tools) 'except)) (superchat-tool-catalog tape)))
         (last (and superchat-max-rounds (>= (plist-get run :rounds) superchat-max-rounds)))
         ;; The note after a look-back goes with the first request only.
         (resumed (prog1 (plist-get run :resumed) (setf (plist-get run :resumed) nil)))
         ;; A run going round is redirected once (superchat-guard.el).
         (signals (unless (or last (plist-get run :redirected))
                    (superchat-guard-signals (superchat-agent--since run entries))))
         (redirect (and signals (superchat-guard-going-round-p signals))))
    (setf (plist-get run :last) last (plist-get run :offered) names)
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
      (when redirect
        (setf (plist-get run :redirected) t)
        (superchat-tape-append tape 'event (format "Going round: %s; told once to change the approach."
                                                   (superchat-guard-counts signals))
                               :data (list :type "redirect"))
        (superchat-agent--emit run))
      (superchat-agent--guard
       run (lambda ()
             (superchat-agent--track
              run (lambda ()
                    (superchat-llm-request
                     messages
                     :system (concat (superchat-context-system tape (plist-get run :system))
                                     (and last (superchat-agent--facts (superchat-agent--since run) superchat-agent--limit-note))
                                     (and resumed (concat (superchat-agent--facts (superchat-agent--since run) superchat-agent--resume-note)
                                                          "\n\nYour look-back:\n" resumed))
                                     (and catalog (concat "\n\n" catalog))
                                     (and redirect (concat "\n\n" (superchat-guard-redirect signals))))
                     ;; The tools stay: a provider that has no tool in the
                     ;; request may leak its call syntax as text. A call of
                     ;; this answer is dropped by --respond.
                     :tools (and names (superchat-tool-specs names))
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
                                    (and (plist-get run :ephemeral) (list :ephemeral t))
                                    (and (plist-get run :directory) (list :directory (plist-get run :directory)))))
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
                     :stop nil :done nil :text nil :error nil :tries 0 :timer nil
                     :continues 0 :resumed nil :paused nil :redirected nil :steers nil :offered nil)))

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

(cl-defun superchat-run (tape text &key scope directory origin tools system model ephemeral
                              unattended refs on-text on-entry on-done)
  "Append TEXT as a user entry of timeline TAPE and answer it; return the run.
SCOPE is that of a new timeline's first entry, by default that of
`default-directory'; DIRECTORY, outside a project, its working directory.
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
               ;; Only the first user entry of a timeline holds it.
               :directory (unless (superchat-tape-entries :tape tape :kinds '(user) :limit 1) directory)
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

(defun superchat-steer (tape text)
  "Give the run going on timeline TAPE the person's TEXT: a `user' entry
before its next request.  Text a run never took is its :steers at the end.
With no run going, an error."
  (let ((run (gethash tape superchat-agent--runs)))
    (unless (and run (eq (plist-get run :status) 'running))
      (error "Timeline %s has no run going" tape))
    (setf (plist-get run :steers) (append (plist-get run :steers) (list text)))))

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
