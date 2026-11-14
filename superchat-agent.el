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

(defun superchat-agent--messages (entries)
  "The messages for ENTRIES; events and notes are left out."
  (delq nil (mapcar #'superchat-agent--message entries)))

(defun superchat-agent--emit (run)
  "Give ON-ENTRY every entry appended to the timeline since the last call."
  (dolist (entry (superchat-tape-entries :tape (plist-get run :tape)
                                         :after (plist-get run :seen)))
    ;; ON-ENTRY may cancel, which emits the rest of this list itself.
    (when (> (plist-get entry :id) (plist-get run :seen))
      (setf (plist-get run :seen) (plist-get entry :id))
      (when-let* ((on-entry (plist-get run :on-entry)))
        (funcall on-entry entry)))))

(defun superchat-agent--add (run kind text &rest keys)
  "Append an entry to the timeline of RUN and show it to ON-ENTRY."
  (prog1 (apply #'superchat-tape-append (plist-get run :tape) kind text keys)
    (superchat-agent--emit run)))

(defun superchat-agent--live (run)
  "Non-nil while RUN has not ended."
  (eq (plist-get run :status) 'running))

(defun superchat-agent--finish (run status)
  "End RUN with STATUS and call ON-DONE.  The run is gone before ON-DONE, so
that ON-DONE may start the next one."
  (setf (plist-get run :status) status)
  (remhash (plist-get run :tape) superchat-agent--runs)
  ;; ON-ENTRY may signal; the caller's ON-DONE still hears that the run ended.
  (unwind-protect (superchat-agent--emit run)
    (when-let* ((on-done (plist-get run :on-done)))
      (funcall on-done run))))

(defun superchat-agent--fail (run why)
  "Record WHY as an error event and end RUN with status error."
  (when (superchat-agent--live run)
    (unwind-protect (superchat-agent--add run 'event why :data (list :type "error"))
      (superchat-agent--finish run 'error))))

(defun superchat-agent--guard (run thunk)
  "Call THUNK; an error it signals ends RUN with an error event, so a run
always ends.  When RUN has already ended the error belongs to the caller."
  (condition-case err (funcall thunk)
    (error (if (superchat-agent--live run)
               (superchat-agent--fail run (error-message-string err))
             (signal (car err) (cdr err))))))

(defun superchat-agent--track (run thunk)
  "Call THUNK, which starts a request, and keep its handle for `superchat-cancel'.
The request may already have answered, and the answer may have started
another request; then this handle is stale and is not kept."
  (let* ((seq (cl-incf (plist-get run :seq)))
         (handle (funcall thunk)))
    (when (and (= seq (plist-get run :seq)) (superchat-agent--live run))
      (setf (plist-get run :handle) handle))))

(defun superchat-agent--done (run response)
  "RUN's model answered with RESPONSE: record it, run its calls, go on."
  (when (superchat-agent--live run)
    (superchat-agent--guard run (lambda () (superchat-agent--respond run response)))))

(defun superchat-agent--respond (run response)
  "Record RESPONSE, run its calls in order, make the next request."
  (let ((calls (plist-get response :calls)))
    (cl-incf (plist-get run :rounds))
    (superchat-agent--add run 'assistant (plist-get response :text)
                          :data (list :model (plist-get response :model) :calls calls))
    (if (null calls)
        (when (superchat-agent--live run)
          (superchat-agent--finish run 'done))
      (superchat-agent--calls run calls))))

(defun superchat-agent--calls (run calls)
  "Run CALLS one after the other, each started from the result of the one
before, then make the next request.  A tool may answer later; Emacs is free
meanwhile.  :calling is the id of a call whose function has not returned, whose
entry will be appended by `superchat-tool-call' even if the run is cancelled
meanwhile; :stop stops a tool that is still running."
  (when (superchat-agent--live run)
    (if (null calls)
        (superchat-agent--step run)
      (let* ((call (car calls))
             (seq (cl-incf (plist-get run :seq)))
             (stop (progn
                     (setf (plist-get run :calling) (plist-get call :id))
                     (superchat-tool-call
                      (plist-get run :tape) call
                      (lambda (_)
                        (when (= seq (plist-get run :seq))
                          (cl-incf (plist-get run :seq))
                          (setf (plist-get run :calling) nil)
                          (setf (plist-get run :stop) nil)
                          (if (superchat-agent--live run)
                              (superchat-agent--guard
                               run (lambda ()
                                     (superchat-agent--emit run)
                                     (superchat-agent--calls run (cdr calls))))
                            ;; Cancelled while the function ran: its entry is the only result.
                            (superchat-agent--emit run))))))))
        (when (and stop (= seq (plist-get run :seq)))
          (setf (plist-get run :calling) nil)
          (if (superchat-agent--live run)
              (setf (plist-get run :stop) stop)
            ;; Cancelled while the tool was starting: nobody else will stop it.
            (funcall stop)
            (superchat-agent--add run 'tool "Cancelled."
                                  :data (list :call-id (plist-get call :id)
                                              :name (plist-get call :name) :error t))))))))

(defun superchat-agent--step (run)
  "Make the next request of RUN."
  (let* ((tape (plist-get run :tape))
         (messages (superchat-agent--messages (superchat-view 'conversation :tape tape)))
         (tools (plist-get run :tools)))
    (superchat-agent--emit run)
    (cond
     ((>= (plist-get run :rounds) superchat-max-rounds)
      (superchat-agent--fail
       run (format "Stopped: %d model responses without an answer." superchat-max-rounds)))
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
                     :on-done (lambda (response) (superchat-agent--done run response))
                     :on-error (lambda (why) (when (superchat-agent--live run)
                                               (superchat-agent--fail run why))))))))))))

(defun superchat-agent--begin (run text origin scope &optional failed)
  "Append the user entry of RUN and make its first request, or end the run
with an error when FAILED.  SCOPE is that of the first entry of a timeline."
  (setf (plist-get run :unsent) nil)
  (superchat-agent--add run 'user text
                        :data (and origin (list :origin origin)) :scope scope)
  (if failed
      (superchat-agent--finish run 'error)
    (superchat-agent--step run)))

(cl-defun superchat-run (tape text &key scope origin tools system model on-text on-entry on-done)
  "Append TEXT as a user entry of timeline TAPE and answer it; return the run.
SCOPE is that of a new timeline's first entry, by default that of
`default-directory'.
TOOLS is a list of tool names, nil for every tool, or `none'.  SYSTEM is
extra text for the system prompt.  ON-TEXT gets streamed text, ON-ENTRY each
entry appended, ON-DONE the run, once, with :status done, cancelled or error."
  (when (gethash tape superchat-agent--runs)
    (error "Timeline %s is busy with a run or a handoff" tape))
  (let* ((last (car (superchat-tape-entries :tape tape :newest t :limit 1)))
         (run (list :tape tape :status 'running :rounds 0 :seq 0 :handle nil :calling nil :stop nil
                    :unsent nil :text text :origin origin
                    :seen (if last (plist-get last :id) 0)
                    :tools tools :system system :model model
                    :on-text on-text :on-entry on-entry :on-done on-done)))
    (puthash tape run superchat-agent--runs)
    (superchat-agent--guard
     run (lambda ()
           (if (and last (> (cl-loop for m in (superchat-agent--messages
                                               (superchat-view 'conversation :tape tape))
                                     sum (length (plist-get m :text)))
                            superchat-context-chars))
               ;; The anchor goes in before the user entry, so the request stays after it.
               (progn
                 (setf (plist-get run :unsent) t)
                 (superchat-agent--track
                  run (lambda ()
                        (superchat-agent--summarise
                         tape nil
                         (lambda (anchor)
                           (when (superchat-agent--live run)
                             (superchat-agent--guard
                              run (lambda ()
                                    (superchat-agent--begin run text origin nil
                                                            (not anchor))))))))))
             (superchat-agent--begin
              run text origin
              (unless last (or scope (superchat-tape-scope-of default-directory)))))))
    run))

(defun superchat-run-active (tape)
  "The run of timeline TAPE, or nil."
  (gethash tape superchat-agent--runs))

(defun superchat-agent--unanswered (tape &optional except)
  "The calls of TAPE's latest assistant entry that have no tool entry yet,
apart from the call with id EXCEPT, which is running and will have one."
  (when-let* ((entry (car (superchat-tape-entries :tape tape :kinds '(assistant)
                                                  :newest t :limit 1))))
    (let ((answered (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id))
                            (superchat-tape-entries :tape tape :kinds '(tool)
                                                    :after (plist-get entry :id)))))
      (cl-remove-if (lambda (call) (member (plist-get call :id) (cons except answered)))
                    (plist-get (plist-get entry :data) :calls)))))

(defun superchat-cancel (tape)
  "Cancel the run or the pending handoff of timeline TAPE, if it has one."
  (when-let* ((run (superchat-run-active tape)))
    (cond
     ((eq (plist-get run :status) 'handoff)
      (when-let* ((handle (plist-get run :handle)))
        (superchat-llm-cancel handle))
      (remhash tape superchat-agent--runs)
      (superchat-tape-append tape 'event "Cancelled by the user."
                             :data (list :type "cancelled")))
     ((superchat-agent--live run)
      (setf (plist-get run :status) 'cancelled)
      (when-let* ((handle (plist-get run :handle)))
        (superchat-llm-cancel handle))
      (when-let* ((stop (plist-get run :stop)))
        (setf (plist-get run :stop) nil)
        (funcall stop))
      ;; The run ends even when a callback signals on one of these entries.
      (unwind-protect
          (progn
            ;; Cancelled during the handoff before the user entry: write it down first.
            (when (plist-get run :unsent)
              (superchat-agent--add run 'user (plist-get run :text)
                                    :data (and (plist-get run :origin)
                                               (list :origin (plist-get run :origin)))))
            (dolist (call (superchat-agent--unanswered tape (plist-get run :calling)))
              (superchat-agent--add run 'tool "Cancelled."
                                    :data (list :call-id (plist-get call :id)
                                                :name (plist-get call :name) :error t)))
            (superchat-agent--add run 'event "Cancelled by the user."
                                  :data (list :type "cancelled")))
        (superchat-agent--finish run 'cancelled))))))

(defconst superchat-agent--summary-question
  "Summarise this conversation so that someone who has not seen it can
carry on: what was asked, what was done, what was found, what is open.
Then write a line that starts with NEXT: and say what comes next.")

(defun superchat-agent--summarise (tape name on-done)
  "Ask for the summary of TAPE and append the anchor NAME; return the handle.
The conversation goes as one user message: a provider refuses tool turns
when the request has no tools.  ON-DONE gets the anchor, or nil."
  (let ((entries (superchat-view 'conversation :tape tape)))
    (superchat-llm-request
     (list (list :role 'user
                 :text (concat (superchat-view-render entries superchat-context-chars) "\n\n"
                               superchat-agent--summary-question)))
     :on-done (lambda (response)
                (let* ((text (plist-get response :text))
                       (split (string-match "^NEXT:[ \t]*" text))
                       (summary (string-trim (if split (substring text 0 split) text)))
                       (next (if split (string-trim (substring text (match-end 0))) ""))
                       (anchor (superchat-tape-append
                                tape 'anchor summary
                                :data (list :name (or name "handoff") :summary summary :next next)
                                :refs (mapcar (lambda (e) (plist-get e :id)) entries))))
                  (funcall on-done anchor)))
     :on-error (lambda (why)
                 (superchat-tape-append tape 'event (format "Handoff failed: %s" why)
                                        :data (list :type "error"))
                 (funcall on-done nil)))))

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
               tape name (lambda (anchor)
                           (remhash tape superchat-agent--runs)
                           (when on-done (funcall on-done anchor)))))
      (error (remhash tape superchat-agent--runs)
             (signal (car err) (cdr err))))))

(defun superchat-agent--fork-entries (tape entry-id)
  "The conversation entries of TAPE up to ENTRY-ID, and the results of the
calls of every assistant entry among them."
  (let* ((entries (superchat-view 'conversation :tape tape))
         (calls (cl-loop for e in entries
                         when (and (eq (plist-get e :kind) 'assistant)
                                   (<= (plist-get e :id) entry-id))
                         append (mapcar (lambda (c) (plist-get c :id))
                                        (plist-get (plist-get e :data) :calls)))))
    (cl-remove-if-not
     (lambda (e) (or (<= (plist-get e :id) entry-id)
                     (and (eq (plist-get e :kind) 'tool)
                          (member (plist-get (plist-get e :data) :call-id) calls))))
     entries)))

(defun superchat-fork (tape entry-id)
  "A new timeline that starts from TAPE's conversation up to ENTRY-ID.
Its first entry is an anchor with no summary that refers to those entries."
  (let ((name (superchat-tape-new-name)))
    (superchat-tape-append
     name 'anchor ""
     :data (list :name "fork" :summary "" :next "")
     :refs (mapcar (lambda (e) (plist-get e :id))
                   (superchat-agent--fork-entries tape entry-id))
     :scope (plist-get (car (superchat-tape-entries :tape tape :newest t :limit 1)) :scope))
    name))

(provide 'superchat-agent)
;;; superchat-agent.el ends here
