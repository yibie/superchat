;;; object-scope-regression.el --- work records and timers belong to a workspace -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-work-records-and-timers-belong-to-a-workspace.md.
;; The model's tools reach a Work Record or a timer only from a conversation
;; of the Workspace it belongs to; a conversation of no Workspace reaches only
;; what was made in that very conversation.  The user's own commands and the
;; product's own writes still reach everything.  The headline tests go through
;; the real tool loop with a scripted model; the rest call the tool functions
;; as a tool call of a given conversation.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/object-scope-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^object-scope/")'

;;; Code:

(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-timer)
(defvar superchat--tool-execution-context)

(defconst oscope--refusal "belongs to another workspace"
  "What a refused tool call says.")

(defmacro oscope--as (session &rest body)
  "Run BODY as a tool call of a Run in SESSION, from a buffer of no conversation."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((superchat--tool-execution-context (list :session-id ,session)))
       ,@body)))

(defun oscope--work-rows ()
  "How many Work Record facts every Tape holds."
  (length (superchat-work--rows)))

(defun oscope--timer-rows ()
  "How many timer facts every Tape holds."
  (length (superchat-timer--rows)))

(defun oscope--pending-p (id)
  "Non-nil when the timer ID is pending."
  (cl-find id (superchat-timer-list)
           :key (lambda (timer) (plist-get timer :id)) :test #'equal))

(defun oscope--fired (session id kind text occurrence)
  "Write that the timer ID of KIND with TEXT fired in SESSION at OCCURRENCE."
  (superchat-tape-append-event
   (list :type "timer.fired" :session-id session :actor-id "timers"
         :summary text
         :payload (list :timer-id id :occurrence occurrence
                        :kind (symbol-name kind) :text text :late nil
                        :awaits-start (eq kind 'task)))))

;;;; Work Records -------------------------------------------------------------

(ert-deftest object-scope/another-workspaces-model-cannot-touch-a-record ()
  "The review's F1, through the real tool loop."
  (wrec--with nil (p q)
    (let* ((a (wrec--view p))
           (sa (hm--session a))
           (id (superchat-work-start sa "OSCOPE-PRIVATE-GOAL"))
           (b (wrec--view q))
           (rows (oscope--work-rows)))
      (setq smoke--script
            (list (wrec--call '(action . "read") (cons 'id id))
                  (wrec--call '(action . "update") (cons 'id id)
                              '(progress . "WRITTEN-FROM-Q"))
                  (wrec--call '(action . "done") (cons 'id id)
                              '(conclusion . "COMPLETED-FROM-Q"))
                  '(:text "finished")))
      (hm--turn b "use the supplied record id")
      (let ((seen (prin1-to-string hm--wires)))
        (should (string-match-p oscope--refusal seen))
        ;; The refusal says nothing of what the record holds.
        (should-not (string-match-p "OSCOPE-PRIVATE-GOAL" seen)))
      (let ((record (superchat-work-get id)))
        (should-not (plist-get record :progress))
        (should-not (plist-get record :conclusion)))
      ;; Nothing was written anywhere.
      (should (= rows (oscope--work-rows))))))

(ert-deftest object-scope/every-action-on-another-workspaces-record-is-refused ()
  (wrec--with nil (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "OSCOPE-PRIVATE-GOAL"))
           (sq (hm--session (wrec--view q)))
           (rows (oscope--work-rows)))
      (dolist (answer
               (oscope--as sq
                 (list (superchat-work-tool "read" id)
                       (superchat-work-tool "update" id nil "P" nil nil)
                       (superchat-work-tool "wait" id nil nil nil nil "user" "D")
                       (superchat-work-tool "wait" id nil nil nil nil "time" "D" nil nil 10)
                       (superchat-work-tool "done" id nil nil nil nil nil nil "C")
                       ;; A new record may not follow another Workspace's.
                       (superchat-work-tool "start" nil "NEW-GOAL" nil nil nil nil nil nil id))))
        (should (string-match-p oscope--refusal answer))
        (should-not (string-match-p "OSCOPE-PRIVATE-GOAL" answer)))
      (should (= rows (oscope--work-rows)))
      (should-not (plist-get (superchat-work-get id) :waiting))
      (should-not (superchat-work-current sq)))))

(ert-deftest object-scope/another-conversation-of-the-workspace-reaches-the-record ()
  "Guard: the Workspace is the boundary, not the conversation."
  (wrec--with nil (p)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "OSCOPE-SHARED-GOAL"))
           (other (with-current-buffer (wrec--file p "b.txt") (superchat-new-session)))
           (so (hm--session other)))
      (should-not (equal sa so))
      (oscope--as so
        (should (string-match-p "OSCOPE-SHARED-GOAL" (superchat-work-tool "read" id)))
        (should-not (string-match-p "\\`Error" (superchat-work-tool "update" id nil "HALF"))))
      (should (equal "HALF" (plist-get (superchat-work-get id) :progress))))))

(ert-deftest object-scope/the-conversation-a-record-wakes-in-reaches-it ()
  "Guard: the woken model updates and completes its own record."
  (wrec--with (list (wrec--call '(action . "update") '(progress . "WOKEN-PROGRESS"))
                    (wrec--call '(action . "done") '(conclusion . "WOKEN-CONCLUSION"))
                    '(:text "taken up and finished"))
      (p)
    (wrec--with-clock
      (let* ((main (hm--session (wrec--view p)))
             (id (superchat-work-start main "OSCOPE-WAKE-GOAL")))
        (superchat-work-wait main id "time" "later" (+ wrec--now 600))
        (setq wrec--now (+ wrec--now 600))
        (should (= 1 (length (superchat-work-wake-due))))
        (wrec--settled)
        (let ((record (superchat-work-get id)))
          (should (equal "WOKEN-PROGRESS" (plist-get record :progress)))
          (should (equal "WOKEN-CONCLUSION" (plist-get record :conclusion))))))))

(ert-deftest object-scope/a-sub-agent-is-told-of-its-record-and-cannot-change-it ()
  "A sub-agent is given a snapshot of the record and told it cannot change it
\(K14); its conversation is of no Workspace, so the tool refuses it too.  What
the product writes when the sub-agent hands back is not the model's way in."
  (wrec--with (list (wrec--call '(action . "start") '(goal . "OSCOPE-SUB-GOAL"))
                    '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                   (task . "SUB-TASK-MARKER look into it"))))
                    '(:text "SUB-REPORT found three things")
                    '(:text "done"))
      (p)
    (setq superchat-llm-tool-names
          (append superchat-llm-tool-names '("delegate_to_subagent")))
    (let* ((view (wrec--view p))
           (session (hm--session view)))
      (hm--turn view "start it and delegate the research")
      (let* ((id (superchat-work-current session))
             (task (car (plist-get (superchat-work-get id) :tasks)))
             (child (plist-get task :conversation)))
        ;; The product recorded the delegated task under the record.
        (should (equal "completed" (plist-get task :status)))
        (should (string-match-p "SUB-REPORT" (plist-get task :report)))
        (should (stringp child))
        (should-not (equal child session))
        ;; The sub-agent was shown the record ...
        (should (string-match-p "OSCOPE-SUB-GOAL"
                                (hm--text (car (wrec--subagent-wire)))))
        ;; ... and its own tool call on it is refused, writing nothing.
        (let ((rows (oscope--work-rows)))
          (oscope--as child
            (should (string-match-p oscope--refusal
                                    (superchat-work-tool "update" id nil "FROM-CHILD"))))
          (should (= rows (oscope--work-rows))))))))

(ert-deftest object-scope/a-conversation-of-no-workspace-reaches-no-record ()
  (wrec--with nil (p)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "OSCOPE-PRIVATE-GOAL"))
           (temporary (hm--session (smoke--open-chat)))
           (rows (oscope--work-rows)))
      (should-not (superchat-work--workspace temporary))
      (oscope--as temporary
        (dolist (answer (list (superchat-work-tool "read" id)
                              (superchat-work-tool "done" id nil nil nil nil nil nil "C")))
          (should (string-match-p oscope--refusal answer))
          (should-not (string-match-p "OSCOPE-PRIVATE-GOAL" answer))))
      (should (= rows (oscope--work-rows))))))

(ert-deftest object-scope/a-record-focused-before-the-check-is-refused-the-same-way ()
  "A Tape written before this check may name another Workspace's record current."
  (wrec--with nil (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "OSCOPE-PRIVATE-GOAL"))
           (sq (hm--session (wrec--view q))))
      (superchat-work--append sq "work.focused" "legacy focus" (list :work-id id))
      (should (equal id (superchat-work-current sq)))
      (let ((rows (oscope--work-rows)))
        (oscope--as sq
          (should (string-match-p oscope--refusal
                                  (superchat-work-tool "update" nil nil "P"))))
        (should (= rows (oscope--work-rows)))))))

(ert-deftest object-scope/the-users-list-still-shows-every-workspaces-records ()
  "Guard: what the user opens is not the model's view."
  (wrec--with nil (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "OSCOPE-PRIVATE-GOAL"))
           (b (wrec--view q)))
      (with-current-buffer b
        (should (assoc id (superchat-work--list-entries)))
        ;; And the user's own command completes it from anywhere.
        (superchat-work-complete-by-user (superchat-work--require id) "CLOSED-BY-USER"))
      (should (equal "CLOSED-BY-USER" (plist-get (superchat-work-get id) :conclusion))))))

;;;; Timers -------------------------------------------------------------------

(ert-deftest object-scope/another-workspaces-model-cannot-see-or-cancel-a-timer ()
  "The review's F2, through the real tool loop."
  (wrec--with nil (p q)
    (let* ((a (wrec--view p))
           (id (plist-get (superchat-timer-create (hm--session a) 'reminder
                                                  "OSCOPE-PRIVATE-REMINDER"
                                                  (+ (float-time) 3600))
                          :id))
           (b (wrec--view q))
           (rows (oscope--timer-rows)))
      (unwind-protect
          (progn
            (setq smoke--script
                  (list '(:tool-uses (("timer_list")))
                        `(:tool-uses (("timer_cancel" (id . ,id))))
                        '(:text "finished")))
            (hm--turn b "list then cancel the supplied timer")
            (let ((seen (prin1-to-string hm--wires)))
              (should-not (string-match-p "OSCOPE-PRIVATE-REMINDER" seen))
              (should (string-match-p "No pending timers" seen))
              (should (string-match-p oscope--refusal seen)))
            (should (oscope--pending-p id))
            (should (= rows (oscope--timer-rows))))
        (superchat-timer-disarm-all)))))

(ert-deftest object-scope/a-workspaces-conversations-share-its-timers ()
  "Guard: the Workspace is the boundary, not the conversation."
  (wrec--with nil (p)
    (let* ((sa (hm--session (wrec--view p)))
           (id (plist-get (superchat-timer-create sa 'reminder "OSCOPE-SHARED-REMINDER"
                                                  (+ (float-time) 3600))
                          :id))
           (so (hm--session (with-current-buffer (wrec--file p "b.txt")
                              (superchat-new-session)))))
      (unwind-protect
          (progn
            (should-not (equal sa so))
            (oscope--as so
              (should (string-match-p "OSCOPE-SHARED-REMINDER" (superchat-timer-tool-list)))
              (should (string-match-p "cancelled" (superchat-timer-tool-cancel id))))
            (should-not (oscope--pending-p id)))
        (superchat-timer-disarm-all)))))

(ert-deftest object-scope/fired-timers-are-listed-for-their-workspace-only ()
  (wrec--with nil (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (so (hm--session (with-current-buffer (wrec--file p "b.txt")
                              (superchat-new-session))))
           (sq (hm--session (wrec--view q))))
      (oscope--fired sa "timer-oscope-fired" 'reminder "OSCOPE-FIRED-REMINDER"
                     (- (float-time) 60))
      (oscope--as sq
        (should-not (string-match-p "OSCOPE-FIRED-REMINDER" (superchat-timer-tool-list))))
      ;; Another conversation of the same Workspace is told.
      (oscope--as so
        (should (string-match-p "OSCOPE-FIRED-REMINDER" (superchat-timer-tool-list)))))))

(ert-deftest object-scope/a-fired-task-not-yet-started-is-its-workspaces-to-cancel ()
  (wrec--with nil (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (sq (hm--session (wrec--view q)))
           (id "timer-oscope-task"))
      (oscope--fired sa id 'task "OSCOPE-TASK" (- (float-time) 60))
      (should (= 1 (length (superchat-timer-unstarted-tasks))))
      (let ((rows (oscope--timer-rows)))
        (oscope--as sq
          (should (string-match-p oscope--refusal (superchat-timer-tool-cancel id))))
        (should (= rows (oscope--timer-rows)))
        (should (= 1 (length (superchat-timer-unstarted-tasks)))))
      (oscope--as sa
        (should (string-match-p "cancelled" (superchat-timer-tool-cancel id))))
      (should-not (superchat-timer-unstarted-tasks)))))

(ert-deftest object-scope/a-conversation-of-no-workspace-has-only-its-own-timers ()
  (wrec--with nil (p)
    (let* ((sp (hm--session (wrec--view p)))
           (theirs (plist-get (superchat-timer-create sp 'reminder "OSCOPE-P-REMINDER"
                                                      (+ (float-time) 3600))
                              :id))
           (temporary (hm--session (smoke--open-chat)))
           (mine (plist-get (superchat-timer-create temporary 'reminder
                                                    "OSCOPE-TEMPORARY-REMINDER"
                                                    (+ (float-time) 3600))
                            :id)))
      (unwind-protect
          (progn
            (should-not (superchat-timer--scope-of temporary))
            (oscope--as temporary
              (let ((listed (superchat-timer-tool-list)))
                (should (string-match-p "OSCOPE-TEMPORARY-REMINDER" listed))
                (should-not (string-match-p "OSCOPE-P-REMINDER" listed)))
              (should (string-match-p oscope--refusal (superchat-timer-tool-cancel theirs)))
              (should (string-match-p "cancelled" (superchat-timer-tool-cancel mine))))
            (should (oscope--pending-p theirs))
            (should-not (oscope--pending-p mine))
            ;; And the Workspace's conversation does not see the temporary one's.
            (let ((again (plist-get (superchat-timer-create
                                     temporary 'reminder "OSCOPE-TEMPORARY-AGAIN"
                                     (+ (float-time) 3600))
                                    :id)))
              (oscope--as sp
                (should-not (string-match-p "OSCOPE-TEMPORARY-AGAIN"
                                            (superchat-timer-tool-list)))
                (should (string-match-p oscope--refusal
                                        (superchat-timer-tool-cancel again))))))
        (superchat-timer-disarm-all)))))

(ert-deftest object-scope/a-timer-that-does-not-exist-is-said-not-to-exist ()
  "Guard: the refusal is for another Workspace's timer, not for a wrong id."
  (wrec--with nil (p)
    (oscope--as (hm--session (wrec--view p))
      (let ((answer (superchat-timer-tool-cancel "timer-no-such")))
        (should (string-match-p "no pending timer" answer))
        (should-not (string-match-p oscope--refusal answer))))))

(ert-deftest object-scope/the-users-own-cancel-reaches-any-timer ()
  "Guard: `superchat-timer-cancel' is what the user's commands call."
  (wrec--with nil (p q)
    (let* ((sp (hm--session (wrec--view p)))
           (id (plist-get (superchat-timer-create sp 'reminder "OSCOPE-P-REMINDER"
                                                  (+ (float-time) 3600))
                          :id)))
      (unwind-protect
          (with-current-buffer (wrec--view q)
            (should (superchat-timer-cancel id))
            (should-not (oscope--pending-p id)))
        (superchat-timer-disarm-all)))))

(provide 'object-scope-regression)

;;; object-scope-regression.el ends here
