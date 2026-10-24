;;; fr-pending-probes.el --- what M-x superchat-pending shows in the checklist's states -*- lexical-binding: t; -*-
;; Review of 437ca4e8.  The states of docs/reports/2026-10-07-hands-on-checklist.md
;; steps 5, 6, 9, 12 and 13 are produced the way the product produces them
;; (a record really woken at its time, a tool really asking for approval, an
;; execution really stopped), and the list's own text is printed as it is.
;; Observations, defects included; tests named "guard-" assert a promise.
(load (expand-file-name "scripts/pending-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t)
    (advice-remove 'load skip-loaded)))
(require 'superchat-pending)

(defun fr-pend--table-text ()
  "The list as the user sees it: header line, then the rows."
  (let (text)
    (save-window-excursion
      (let ((buffer (superchat-pending)))
        (with-current-buffer buffer
          (setq text (concat
                      (condition-case nil
                          (substring-no-properties (format-mode-line header-line-format))
                        (error "<header unavailable>"))
                      "\n"
                      (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer buffer)))
    text))

(defun fr-pend--show (label)
  (princ (format "\n===== %s =====\n%s=====\n" label (fr-pend--table-text))))

;;;; Steps 5 and 6: a record woken at its time, then found afterwards ---------

(ert-deftest fr-pending/step-5-and-6-a-record-woken-at-its-time ()
  (pend--fresh
    (wrec--with nil (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((pair (wwf--due-record p "Step 5: add '- 检查完毕' to log.org two minutes later"))
                 (main (car pair)) (id (cdr pair))
                 (buffer (get-file-buffer (expand-file-name "a.txt" p))))
            (superchat-work-update main id "'- 开始检查' added" nil "add '- 检查完毕' at the end")
            (setq smoke--script
                  (list (append (bu--edit-round (buffer-name buffer) "hello" "HELLO") '(:delay 0.4))
                        '(:text "WOKEN-DONE")))
            (let ((views (superchat-work-wake-due)))
              (should (= 1 (length views)))
              (fr-pend--show "step 5, just after the wake was sent: execution running, nothing edited yet")
              (princ (format "kinds: %S\n" (pend--kinds)))
              (should (smoke--wait (lambda () (buffer-modified-p buffer)) 5))
              (fr-pend--show "step 5, the woken execution still running, a.txt edited and unsaved")
              (let ((kinds (pend--kinds)))
                (princ (format "kinds: %S\n" kinds))
                (should (memq 'running kinds))
                ;; The record being continued right now is listed as one
                ;; that "can be continued".
                (should (memq 'continuable kinds))
                (should (equal id (plist-get (car (pend--of 'continuable)) :key))))
              (wrec--settled)
              (fr-pend--show "step 6, after the woken execution ended, nothing looked at yet")
              (princ (format "kinds: %S; outcomes=%S\n" (pend--kinds)
                             (plist-get (superchat-work-get id) :outcomes)))
              (should (memq 'unseen (pend--kinds)))
              (should (memq 'unsaved (pend--kinds)))
              (should (memq 'continuable (pend--kinds))))))))))

;;;; Step 9: an approval question left open ------------------------------------

(ert-deftest fr-pending/step-9-an-open-approval-shows-as-running-only ()
  "While a tool waits for the user's yes or no, the Runtime knows
\(`superchat-runtime--asking-p') and the Tape holds `approval.requested';
the list says only that an execution is running."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             snapshot items asking requested)
        (unless (member "eval_elisp" superchat-llm-tool-names)
          (setq superchat-llm-tool-names (append superchat-llm-tool-names '("eval_elisp"))))
        (setq smoke--script '((:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                              (:text "AFTER-THE-DENIAL")))
        (let ((superchat-approval-function
               (lambda (request)
                 ;; The question is open: what does the list say?
                 (setq items (superchat-pending-items)
                       snapshot (fr-pend--table-text)
                       asking (superchat-runtime--asking-p
                               (superchat-runtime--entry (plist-get request :run-id)))
                       requested (length (hm--events session "approval.requested")))
                 nil)))
          (let ((run (smoke--send view "Use eval_elisp to add 1 and 2")))
            (should (stringp run))
            (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))
        (princ (format "\n===== step 9, while the approval question is open =====\n%s=====\nitems=%S\nruntime asking-p=%S  approval.requested on Tape=%S\n"
                       snapshot
                       (mapcar (lambda (i) (list (plist-get i :kind) (plist-get i :what) (plist-get i :detail))) items)
                       asking requested))
        (should items)
        (should (equal '(running) (delete-dups (mapcar (lambda (i) (plist-get i :kind)) items))))
        (should (equal "started by you" (plist-get (car items) :what)))
        (should asking)
        (should (= 1 requested))
        (should (= 1 (length (hm--events session "approval.decided"))))
        (fr-pend--show "step 9, after answering no")))))

;;;; Step 12: stopped by the user ---------------------------------------------

(ert-deftest fr-pending/step-12-stopped-from-the-list ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (buffer (get-file-buffer (expand-file-name "a.txt" p)))
             (id (superchat-work-start session "Step 12: add three lines to log.org")))
        (superchat-work-update session id "line 甲1 added" nil "add 甲2 and 甲3")
        (setq smoke--script (list (bu--edit-round (buffer-name buffer) "hello" "HELLO")
                                  '(:text "NEVER-DELIVERED" :delay 5)))
        (let ((run (smoke--send view "Add three lines one at a time")))
          (should (smoke--wait (lambda () (buffer-modified-p buffer)) 5))
          (fr-pend--show "step 12, before stopping: running, a.txt edited")
          (let ((item (car (pend--of 'running))))
            (should (equal run (plist-get item :key)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (superchat-pending-stop item)))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
          (fr-pend--show "step 12, after k (stop) on the list")
          (princ (format "run status=%S outcomes=%S view's active run=%S\n"
                         (plist-get (superchat-runtime-status run) :status)
                         (plist-get (superchat-work-get id) :outcomes)
                         (buffer-local-value 'superchat--active-run-id view)))
          (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
          (should (equal '("failed") (plist-get (superchat-work-get id) :outcomes)))
          (should-not (string-match-p "stop\\|cancel\\|interrupt" (fr-pend--table-text))))))))

;;;; Step 13: due but not started, then the row mounts --------------------------

(ert-deftest fr-pending/step-13-due-but-not-started-then-the-row-mounts ()
  (pend--fresh
    (wrec--with '((:text "WOKEN-AFTER-RESTART")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((pair (wwf--due-record p "Step 13: add '- 重启后' after the restart"))
                 (id (cdr pair)))
            (fr-pend--show "step 13, Emacs started and nothing took the record up yet")
            (should (equal '(due) (pend--kinds)))
            (superchat-work-arm-all)
            (wrec--settled)
            (fr-pend--show "step 13, after the row mounted and the wake ran")
            (princ (format "kinds: %S outcomes=%S\n" (pend--kinds) (plist-get (superchat-work-get id) :outcomes)))))))))

;;;; Who started it: a sub-agent's execution ------------------------------------

(ert-deftest fr-pending/a-running-sub-agent-is-started-by-you ()
  (pend--fresh
    (wrec--with (list '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                     (task . "SUB-TASK-MARKER look into it"))))
                      '(:text "SUB-REPORT found three things" :delay 0.8)
                      '(:text "done"))
        (p)
      (setq superchat-llm-tool-names
            (append superchat-llm-tool-names '("delegate_to_subagent")))
      (let* ((view (wrec--view p))
             (run (smoke--send view "delegate the research")))
        (should (smoke--wait (lambda () (>= (length (superchat-runtime-active-runs)) 2)) 5))
        (let ((items (pend--of 'running)))
          (fr-pend--show "a sub-agent's execution under way (two Runs active)")
          (princ (format "running rows: %S\n"
                         (mapcar (lambda (i) (list (plist-get i :what) (plist-get i :detail) (plist-get i :session))) items)))
          (should (= 2 (length items)))
          (should (cl-every (lambda (i) (equal "started by you" (plist-get i :what))) items)))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))))

;;;; Closed loop: a user's execution that ended with no View ---------------------

(ert-deftest fr-pending/a-users-execution-that-ended-while-its-view-was-killed-is-listed-nowhere ()
  (pend--fresh
    (wrec--with '((:text "LATE-ANSWER" :delay 0.4)) (p)
      (let* ((view (wrec--view p))
             (run (smoke--send view "a slow question")))
        (kill-buffer view)
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
        (princ (format "run status after its View was killed: %S; pending items=%S; notices=%S\n"
                       (plist-get (superchat-runtime-status run) :status)
                       (superchat-pending-items) superchat-changes--notices))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (should-not (superchat-pending-items))
        (should-not superchat-changes--notices)))))

;;;; RET on an ended row forgets it before the conversation is shown -------------

(ert-deftest fr-pending/opening-an-ended-row-forgets-it-before-the-conversation-is-shown ()
  "superchat-pending.el:257-259 dismisses, then looks the View up;
superchat-changes-notices-open (superchat-changes.el:767-781) looks first."
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (should (= 1 (length superchat-changes--notices)))
      (let ((item (car (pend--of 'unseen))))
        (should item)
        (when-let* ((view (superchat-timer--live-view session))) (kill-buffer view))
        (cl-letf (((symbol-function 'superchat--render-session-view)
                   (lambda (&rest _) (error "FR injected: the View cannot be rebuilt"))))
          (condition-case err (save-window-excursion (superchat-pending-open item))
            (error (princ (format "superchat-pending-open signalled: %s\n" (error-message-string err))))))
        (princ (format "notices left after RET failed to show the conversation: %d\n"
                       (length superchat-changes--notices)))
        (should (= 0 (length superchat-changes--notices)))))))

(ert-deftest fr-pending/guard-the-notices-list-keeps-the-notice-when-the-conversation-cannot-be-shown ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (when-let* ((view (superchat-timer--live-view session))) (kill-buffer view))
      (cl-letf (((symbol-function 'superchat--render-session-view)
                 (lambda (&rest _) (error "FR injected: the View cannot be rebuilt"))))
        (condition-case err
            (save-window-excursion (superchat-changes-notices-open (plist-get fired :run-id)))
          (error (princ (format "superchat-changes-notices-open signalled: %s\n" (error-message-string err))))))
      (should (= 1 (length superchat-changes--notices))))))

;;;; The Unsaved row names the first conversation that edited the buffer --------

(ert-deftest fr-pending/an-unsaved-buffer-two-conversations-edited-names-the-first ()
  (pend--fresh
    (wrec--with nil (p q)
      (let* ((vp (wrec--view p)) (sp (hm--session vp))
             (vq (wrec--view q)) (sq (hm--session vq))
             (buffer (get-file-buffer (expand-file-name "a.txt" p))))
        (should-not (equal sp sq))
        (should (buffer-live-p buffer))
        (setq smoke--script (list (bu--edit-round (buffer-name buffer) "hello" "HELLO") '(:text "P-EDITED")))
        (hm--turn vp "Change it")
        (setq smoke--script (list (bu--edit-round (buffer-name buffer) "HELLO" "HELLO!") '(:text "Q-EDITED")))
        (hm--turn vq "Change it again")
        (let ((marks (superchat-changes--marks buffer))
              (item (car (pend--of 'unsaved))))
          (princ (format "marks newest first=%S\nrow's conversation=%S (first editor=%S, latest editor=%S)\n"
                         marks (plist-get item :session) sp sq))
          (should (= 2 (length marks)))
          (should (equal sq (car (car marks))))
          (should (equal sp (plist-get item :session))))))))

;;;; "Due, not started" reads the same with a retry armed and with none ----------

(ert-deftest fr-pending/a-failed-task-reads-the-same-whether-a-retry-is-armed-or-not ()
  (pend--fresh
    (wtimer--with nil (p)
      (tsf--task p "FR-TASK whose send fails")
      (let ((superchat-timer-retry-limit 3))
        (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)
        (tsf--send-failing (superchat-timer-catch-up))
        (let ((item (car (pend--of 'due))))
          (princ (format "retry armed (%s s from now): detail=%S\n"
                         (and superchat-timer--retry-at (round (- superchat-timer--retry-at (float-time))))
                         (plist-get item :detail)))
          (should (timerp superchat-timer--retry-timer))
          (should (string-match-p "not started\\'" (plist-get item :detail)))))
      (let ((superchat-timer-retry-limit 0))
        (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)
        (superchat-timer-disarm-all)
        (tsf--send-failing (superchat-timer-retry-tasks))
        (let ((item (car (pend--of 'due))))
          (princ (format "out of tries (retry timer=%S): detail=%S\n"
                         superchat-timer--retry-timer (plist-get item :detail)))
          (should-not superchat-timer--retry-timer)
          (should (string-match-p "not started\\'" (plist-get item :detail)))))
      (fr-pend--show "a scheduled task that came due, whose send failed, and that this Emacs has given up on")
      (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before))))

(ert-run-tests-batch-and-exit "^fr-pending/")
;;; fr-pending-probes.el ends here
