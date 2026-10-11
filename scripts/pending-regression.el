;;; pending-regression.el --- one list of what is under way and what waits -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-11-one-list-of-what-is-pending.md.
;; `superchat-pending-items' reads, from what the other modules already keep,
;; everything Superchat is doing and everything it waits for.  Each kind of
;; row is produced here the way the product produces it -- a message really
;; sent to a scripted model, a timer that really comes due, a buffer a tool
;; really edited -- and then looked for in the list.  Reading writes nothing.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/pending-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^pending/")'

;;; Code:

(load (expand-file-name "unattended-notice-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
;; The second fixture chain loads files the first already loaded, and ERT
;; refuses a test defined twice: skip a file whose feature is provided.
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "timer-start-fault-regression.el"
                              (file-name-directory (or load-file-name buffer-file-name)))
            nil t)
    (advice-remove 'load skip-loaded)))
(require 'superchat-pending)

(defun pend--of (kind)
  "The rows of KIND now in the list."
  (cl-remove-if-not (lambda (item) (eq kind (plist-get item :kind)))
                    (superchat-pending-items)))

(defun pend--kinds ()
  "The kinds now in the list, in order, each once."
  (delete-dups (mapcar (lambda (item) (plist-get item :kind))
                       (superchat-pending-items))))

(defun pend--tape-rows ()
  "How many rows the whole Tape holds."
  (let ((row (car (superchat-db-tape-select "SELECT COUNT(*) FROM tape"))))
    (if (consp row) (car row) row)))

(defmacro pend--fresh (&rest body)
  "Run BODY with no notice pending and no list buffer left behind."
  (declare (indent 0) (debug t))
  `(un--fresh
     (unwind-protect (progn ,@body)
       (when-let* ((list (get-buffer superchat-pending-buffer-name)))
         (kill-buffer list)))))

;;;; Work Records ---------------------------------------------------------------

(ert-deftest pending/a-record-waiting-for-the-user-says-what-it-asks ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "PEND-GOAL")))
        (should (equal '(continuable) (pend--kinds)))
        (superchat-work-wait session id "user" "PEND-QUESTION")
        (let ((item (car (pend--of 'ask-user))))
          (should (equal '(ask-user) (pend--kinds)))
          (should (equal id (plist-get item :key)))
          (should (equal "PEND-GOAL" (plist-get item :what)))
          (should (equal "PEND-QUESTION" (plist-get item :detail)))
          (should (equal session (plist-get item :session))))))))

(ert-deftest pending/a-record-waiting-for-a-time-then-due-and-not-taken-up ()
  (pend--fresh
    (wrec--with nil (p)
      (wrec--with-clock
        (let* ((session (hm--session (wrec--view p)))
               (id (superchat-work-start session "PEND-LATER"))
               (until (+ wrec--now 600)))
          (superchat-work-wait session id "time" "continue" until)
          (should (equal '(wait-time) (pend--kinds)))
          (should (string-match-p "\\`until " (plist-get (car (pend--of 'wait-time))
                                                         :detail)))
          ;; Its time comes and nothing took it up.
          (superchat-work-disarm-all)
          (setq wrec--now (+ until 1))
          (let ((item (car (pend--of 'due))))
            (should (equal '(due) (pend--kinds)))
            (should (equal id (plist-get item :key)))
            (should (string-match-p "not taken up" (plist-get item :detail)))))))))

(ert-deftest pending/a-completed-record-is-not-listed ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "PEND-DONE")))
        (superchat-work-complete session id "finished")
        (should-not (superchat-pending-items))))))

;;;; Timers and scheduled tasks -------------------------------------------------

(ert-deftest pending/a-timer-to-come-and-a-task-that-did-not-start ()
  (pend--fresh
    (wtimer--with nil (p)
      (let* ((session (cdr (tsf--task p "PEND-TASK do the thing")))
             (id (plist-get (car (superchat-timer-list)) :id)))
        (let ((item (car (pend--of 'timer))))
          (should (equal '(timer) (pend--kinds)))
          (should (equal id (plist-get item :key)))
          (should (equal "PEND-TASK do the thing" (plist-get item :what))))
        ;; It comes due and the send fails.
        (tsf--send-failing (superchat-timer-catch-up))
        (let ((item (car (pend--of 'due))))
          (should (equal '(due) (pend--kinds)))
          (should (equal id (car (plist-get item :key))))
          (should (string-match-p "not started" (plist-get item :detail)))
          (should (equal session (plist-get item :session))))))))

(ert-deftest pending/a-task-that-may-have-started-says-so ()
  "The fact the product writes when it cannot tell is what the row reads."
  (pend--fresh
    (wtimer--with nil (p)
      (let* ((session (cdr (tsf--task p "PEND-UNSURE")))
             (timer (car (superchat-timer-list)))
             (id (plist-get timer :id))
             (occurrence (plist-get timer :next-due)))
        (tsf--send-failing (superchat-timer-catch-up))
        (should (equal '(due) (pend--kinds)))
        (superchat-timer--fact session "timer.unconfirmed" id occurrence)
        (let ((item (car (pend--of 'unconfirmed))))
          (should (equal '(unconfirmed) (pend--kinds)))
          (should (equal "PEND-UNSURE" (plist-get item :what)))
          (should (string-match-p "not started again" (plist-get item :detail))))
        ;; Older than the days it stays listed, it is no longer shown.
        (setq wtimer--now (+ wtimer--now
                             (* (1+ superchat-pending-unconfirmed-days) 86400)))
        (should-not (pend--of 'unconfirmed))))))

(ert-deftest pending/a-task-sent-while-another-execution-began-is-unsure ()
  "The timer module's own rule: being sent, and a Run began there afterwards."
  (pend--fresh
    (wtimer--with '((:text "AN ANSWER TO THE USER")) (p)
      (let* ((pair (tsf--task p "PEND-MID-SEND"))
             (view (car pair)))
        (tsf--send-failing (superchat-timer-catch-up))
        ;; A send that only failed is plainly not started.
        (should (equal '(due) (pend--kinds)))
        (hm--turn view "Something else, typed by the user")
        (let ((item (car (pend--of 'unconfirmed))))
          (should item)
          (should-not (pend--of 'due))
          (should (equal "PEND-MID-SEND" (plist-get item :what)))
          (should (string-match-p "while it was being sent"
                                  (plist-get item :detail))))))))

;;;; Executions -------------------------------------------------------------------

(ert-deftest pending/a-running-execution-says-who-started-it ()
  (pend--fresh
    (wrec--with '((:text "SLOW" :delay 0.4)) (p)
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (run (smoke--send view "Take your time")))
        (should (stringp run))
        (let ((item (car (pend--of 'running))))
          (should (equal run (plist-get item :key)))
          (should (equal "started by you" (plist-get item :what)))
          (should (equal session (plist-get item :session))))
        (wrec--settled)
        (should-not (pend--of 'running))))))

(ert-deftest pending/work-superchat-did-by-itself-is-listed-until-looked-at ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (let ((item (car (pend--of 'unseen))))
        (should item)
        (should (equal (plist-get fired :run-id) (plist-get item :key)))
        (should (string-match-p "\\`scheduled task " (plist-get item :what)))
        (should (string-match-p "completed" (plist-get item :detail)))
        (should (equal session (plist-get item :session))))
      ;; Going to it from the list is looking at it.
      (let ((list (save-window-excursion (superchat-pending))))
        (with-current-buffer list
          (goto-char (point-min))
          (should (search-forward "Ended, not looked at" nil t))
          (beginning-of-line)
          (save-window-excursion (superchat-pending-open (tabulated-list-get-id)))))
      (should-not superchat-changes--notices)
      (should-not (pend--of 'unseen)))))

;;;; What the review of the list found (FR-04 to FR-07) ---------------------------

(ert-deftest pending/an-execution-that-asks-is-listed-as-asking ()
  "While a tool waits for the user's yes or no, the row says so; answered, it
is a running execution again and then gone."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             during run)
        (unless (member "eval_elisp" superchat-llm-tool-names)
          (setq superchat-llm-tool-names (append superchat-llm-tool-names '("eval_elisp"))))
        (setq smoke--script '((:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                              (:text "AFTER-THE-DENIAL")))
        (let ((superchat-approval-function
               (lambda (_request)
                 (setq during (superchat-pending-items))
                 nil)))
          (setq run (smoke--send view "Use eval_elisp to add 1 and 2"))
          (should (stringp run))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
        (should (= 1 (length during)))
        (let ((item (car during)))
          (should (eq 'asking (plist-get item :kind)))
          (should (equal run (plist-get item :key)))
          (should (equal "started by you" (plist-get item :what)))
          (should (string-match-p "waits for your answer" (plist-get item :detail)))
          (should (equal session (plist-get item :session))))
        (should-not (pend--of 'asking))
        (should-not (pend--of 'running))))))

(ert-deftest pending/asking-comes-before-everything-and-can-be-stopped ()
  (should (eq 'asking (caar superchat-pending--kinds)))
  (should (equal "Asks you now" (cdar superchat-pending--kinds)))
  (let (cancelled)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'superchat-runtime-cancel)
               (lambda (run &rest _) (setq cancelled run))))
      (superchat-pending-stop (list :kind 'asking :key "run-pend-asking"))
      (should (equal "run-pend-asking" cancelled))
      (should-error (superchat-pending-stop (list :kind 'unseen :key "run-x"))
                    :type 'user-error))))

(ert-deftest pending/a-sub-agents-execution-names-the-execution-that-started-it ()
  (pend--fresh
    (wrec--with (list '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                     (task . "PEND-SUB-TASK look into it"))))
                      '(:text "PEND-SUB-REPORT" :delay 0.8)
                      '(:text "done"))
        (p)
      (setq superchat-llm-tool-names
            (append superchat-llm-tool-names '("delegate_to_subagent")))
      (let* ((view (wrec--view p))
             (run (smoke--send view "delegate the research")))
        (should (smoke--wait (lambda () (>= (length (superchat-runtime-active-runs)) 2)) 5))
        (let* ((items (pend--of 'running))
               (lead (cl-find run items :key (lambda (i) (plist-get i :key)) :test #'equal))
               (sub (car (remq lead items))))
          (should (= 2 (length items)))
          (should (equal "started by you" (plist-get lead :what)))
          (should (equal (format "started by execution %s" run) (plist-get sub :what))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))))

(ert-deftest pending/a-record-being-worked-on-is-not-also-continuable ()
  "While an execution runs in the conversation whose current record it is,
the record is that execution's row; when it ends the record is listed again."
  (pend--fresh
    (wrec--with '((:text "SLOW" :delay 0.4)) (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session "PEND-UNDER-WAY")))
        (should (equal id (plist-get (car (pend--of 'continuable)) :key)))
        (let ((run (smoke--send view "Carry on")))
          (should (stringp run))
          (should (= 1 (length (pend--of 'running))))
          (should-not (pend--of 'continuable))
          (wrec--settled))
        (should (equal id (plist-get (car (pend--of 'continuable)) :key)))))))

(ert-deftest pending/a-woken-record-being-worked-on-is-not-also-continuable ()
  (pend--fresh
    (wrec--with '((:text "SLOW" :delay 0.4)) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((main (hm--session (wrec--view p)))
                 (id (superchat-work-start main "PEND-WOKEN")))
            (superchat-work-wait main id "time" nil (+ wrec--now 60))
            (superchat-work-disarm-all)
            (setq wrec--now (+ wrec--now 120))
            (should (= 1 (length (superchat-work-wake-due))))
            (let ((running (pend--of 'running)))
              (should (= 1 (length running)))
              (should (string-match-p (regexp-quote id) (plist-get (car running) :what))))
            (should-not (pend--of 'continuable))
            (wrec--settled)
            (should (equal id (plist-get (car (pend--of 'continuable)) :key)))))))))

(ert-deftest pending/an-ended-row-is-kept-when-its-conversation-cannot-be-shown ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (let ((item (car (pend--of 'unseen))))
        (should item)
        (when-let* ((view (superchat-timer--live-view session))) (kill-buffer view))
        (cl-letf (((symbol-function 'superchat--render-session-view)
                   (lambda (&rest _) (error "PEND injected: the View cannot be rebuilt"))))
          (should-error (save-window-excursion (superchat-pending-open item))))
        (should (= 1 (length superchat-changes--notices)))
        (should (pend--of 'unseen))))))

;;;; Unsaved buffers --------------------------------------------------------------

(ert-deftest pending/a-buffer-a-tool-edited-is-listed-while-unsaved ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (buffer (get-file-buffer (expand-file-name "a.txt" p))))
        (setq smoke--script (list (bu--edit-round (buffer-name buffer) "hello" "HELLO")
                                  '(:text "EDITED")))
        (hm--turn view "Change it")
        (should (buffer-modified-p buffer))
        (let ((item (car (pend--of 'unsaved))))
          (should (equal (buffer-name buffer) (plist-get item :what)))
          (should (eq buffer (plist-get item :buffer)))
          (should (equal session (plist-get item :session))))
        ;; No longer unsaved, by whatever way: the tool's mark alone is
        ;; not a reason to list it.
        (with-current-buffer buffer
          (should (superchat-changes--marks buffer))
          (set-buffer-modified-p nil))
        (should-not (pend--of 'unsaved))
        (with-current-buffer buffer (set-buffer-modified-p t))
        (should (pend--of 'unsaved))
        (with-current-buffer buffer (save-buffer))
        (should-not (pend--of 'unsaved))))))

;;;; The whole list ---------------------------------------------------------------

(ert-deftest pending/the-kinds-come-in-the-order-that-needs-the-user-first ()
  (pend--fresh
    (wrec--with nil (p)
      ;; Made in the other order: the record that can be continued first,
      ;; the one that asks the user second.
      (let* ((session (hm--session (wrec--view p)))
             (open (superchat-work-start session "PEND-OPEN"))
             (asking (superchat-work-start session "PEND-ASKING")))
        (ignore open)
        (superchat-work-wait session asking "user" "PEND-QUESTION")
        (superchat-timer-create session 'reminder "PEND-REMINDER" (+ (float-time) 3600))
        (unwind-protect
            (should (equal '(ask-user continuable timer) (pend--kinds)))
          (superchat-timer-disarm-all))))))

(ert-deftest pending/reading-the-list-writes-nothing ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "PEND-GOAL")))
        (superchat-work-wait session id "user" "PEND-QUESTION")
        (let ((rows (pend--tape-rows)))
          (superchat-pending-items)
          (kill-buffer (save-window-excursion (superchat-pending)))
          (should (= rows (pend--tape-rows))))))))

(ert-deftest pending/the-list-shows-a-row-for-each-item-and-leads-to-it ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (id (superchat-work-start session "PEND-GOAL")))
        (superchat-work-wait session id "user" "PEND-QUESTION")
        (let ((list (save-window-excursion (superchat-pending))))
          (with-current-buffer list
            (should (derived-mode-p 'superchat-pending-mode))
            (goto-char (point-min))
            (should (search-forward "Waiting for you" nil t))
            (should (search-forward "PEND-GOAL" nil t))
            (should (search-forward "PEND-QUESTION" nil t))
            (beginning-of-line)
            (should (equal id (plist-get (tabulated-list-get-id) :key)))
            ;; RET leads to the conversation the record is in.
            (should (eq view (save-window-excursion
                               (superchat-pending-open (tabulated-list-get-id))
                               (current-buffer))))
            ;; A row that is not a running execution cannot be stopped.
            (should-error (superchat-pending-stop (tabulated-list-get-id))
                          :type 'user-error)))))))

(provide 'pending-regression)
;;; pending-regression.el ends here
