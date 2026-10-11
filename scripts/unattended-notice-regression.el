;;; unattended-notice-regression.el --- work Superchat did by itself leaves a notice -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-a-notice-for-work-nobody-watched.md.
;; The scheduled task and the woken Work Record below take the real path:
;; the time comes and Superchat starts the execution.  No real model and no
;; protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/unattended-notice-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^unattended-notice/")'

;;; Code:

(load (expand-file-name "beside-user-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-changes--notices)
(defvar superchat-changes-notice-mode-line)

(defmacro un--fresh (&rest body)
  "Run BODY with no notice pending and a mode line of its own."
  (declare (indent 0) (debug t))
  `(let ((superchat-changes--notices nil)
         (superchat-changes-notice-mode-line nil)
         (global-mode-string (copy-sequence global-mode-string)))
     (unwind-protect (progn ,@body)
       (when-let* ((list (get-buffer superchat-changes-notices-buffer-name)))
         (kill-buffer list)))))

(defun un--marks ()
  "How many times the mode line holds the notices' mark."
  (cl-count 'superchat-changes-notice-mode-line global-mode-string))

(defun un--shown ()
  "What the mode line shows for the notices, or nil."
  (and (memq 'superchat-changes-notice-mode-line global-mode-string)
       superchat-changes-notice-mode-line
       (substring-no-properties superchat-changes-notice-mode-line)))

(defmacro un--with-task (script vars &rest body)
  "Run BODY after a scheduled task came due and its execution ended.
SCRIPT is the model's; VARS is (SESSION FIRED), bound for BODY."
  (declare (indent 2) (debug t))
  `(wrec--with ,script (p)
     (let* ((now 1800000000.0)
            (superchat-timer-now-function (lambda () now))
            (superchat-timer-notify-function #'ignore)
            (,(car vars) (hm--session (wrec--view p))))
       (unwind-protect
           (progn
             (superchat-timer-create ,(car vars) 'task "UN-TASK do the thing"
                                     (+ now 60))
             (setq now (+ now 60))
             (let ((,(cadr vars) (car (superchat-timer-catch-up))))
               (wrec--settled)
               ,@body))
         (superchat-timer-disarm-all)))))

(ert-deftest unattended-notice/a-scheduled-task-that-ended-leaves-a-notice ()
  (un--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (should (= 1 (length superchat-changes--notices)))
      (let ((notice (car superchat-changes--notices)))
        (should (equal session (plist-get notice :session)))
        (should (equal (plist-get fired :run-id) (plist-get notice :run)))
        (should (eq 'completed (plist-get notice :status)))
        (should (string-match-p "\\`scheduled task " (plist-get notice :what)))
        (should (= 0 (plist-get notice :unsaved))))
      (should (= 1 (un--marks)))
      (should (string-match-p "1" (un--shown))))))

(ert-deftest unattended-notice/a-scheduled-task-that-failed-leaves-a-notice ()
  ;; Nothing was edited, so the unsaved-changes line says nothing: this is
  ;; the ending most easily missed.
  (un--fresh
    (un--with-task '((:error (error . "UN provider down"))) (_session fired)
      (should (stringp (plist-get fired :run-id)))
      (should (= 1 (length superchat-changes--notices)))
      (should (eq 'failed (plist-get (car superchat-changes--notices) :status)))
      (should (string-match-p "1" (un--shown))))))

;;;; An execution that ended before it was on record as a task's ---------------

(defmacro un--with-task-answered-at-once (script vars &rest body)
  "Like `un--with-task', with the model answering inside the send itself.
The answer is delivered in the call that asks for it, so the execution has
ended before `timer.started' is written.  Everything else is the real path."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'smoke--fake-network)
              (lambda (_mode provider prompt partial response error multi)
                (push (hm--wire-of provider prompt) hm--wires)
                (smoke--deliver provider prompt partial response error multi
                                (or (pop smoke--script) '(:text "DONE")))
                nil)))
     (un--with-task ,script ,vars ,@body)))

(ert-deftest unattended-notice/a-task-that-ended-inside-its-own-send-leaves-a-notice ()
  "The review's N2."
  (un--fresh
    (un--with-task-answered-at-once '((:text "FAST-ANSWER")) (session fired)
      (should (= 1 (length (hm--events session "run.completed"))))
      (should (= 1 (length (hm--events session "timer.started"))))
      (should (= 1 (length superchat-changes--notices)))
      (let ((notice (car superchat-changes--notices)))
        (should (equal session (plist-get notice :session)))
        (should (equal (plist-get fired :run-id) (plist-get notice :run)))
        (should (eq 'completed (plist-get notice :status)))
        (should (string-match-p "\\`scheduled task " (plist-get notice :what))))
      (should (= 1 (un--marks)))
      ;; Told again, it is still one notice.
      (superchat-changes--note-started-late session (plist-get fired :run-id))
      (should (= 1 (length superchat-changes--notices))))))

(ert-deftest unattended-notice/a-task-that-failed-inside-its-own-send-leaves-a-notice ()
  (un--fresh
    (un--with-task-answered-at-once '((:error (error . "UN provider down"))) (session _fired)
      (should (= 1 (length (hm--events session "timer.started"))))
      (should (= 1 (length superchat-changes--notices)))
      (should (eq 'failed (plist-get (car superchat-changes--notices) :status))))))

(ert-deftest unattended-notice/a-start-on-record-while-the-execution-runs-leaves-none-yet ()
  "Guard: being told of a start is not an ending."
  (un--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (run (hm--turn view "one")))
        (bu--fact session "timer.started"
                  (list :timer-id "t1" :occurrence 1 :run-id run))
        (cl-letf (((symbol-function 'superchat-runtime-status)
                   (lambda (_run) (list :status 'running :run-id run
                                        :session-id session))))
          (superchat-changes--note-started-late session run))
        (should-not superchat-changes--notices)))))

(ert-deftest unattended-notice/a-woken-record-names-itself-and-opens-its-conversation ()
  (un--fresh
    (wrec--with '((:text "WOKEN-ANSWER")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((due (wwf--due-record p "UN woken"))
                 (view (car (superchat-work-wake-due)))
                 (session (hm--session view)))
            (wrec--settled)
            (should (= 1 (length superchat-changes--notices)))
            (let ((notice (car superchat-changes--notices)))
              (should (equal session (plist-get notice :session)))
              (should (equal (format "work record %s" (cdr due))
                             (plist-get notice :what)))
              ;; The list shows it; RET goes to that conversation and the
              ;; notice is gone.
              (with-current-buffer (superchat-changes-notices)
                (should (string-match-p (regexp-quote (cdr due))
                                        (buffer-string))))
              (superchat-changes-notices-open (plist-get notice :run))
              (should (eq view (current-buffer)))
              (should-not superchat-changes--notices)
              (should-not (un--shown)))))))))

(ert-deftest unattended-notice/the-users-own-execution-leaves-none ()
  (un--fresh
    (hm--fixture (list '(:text "ONE") '(:error (error . "UN down")))
      (let ((view (smoke--open-chat)))
        (hm--turn view "one")
        (ignore-errors (hm--turn view "two"))
        (wrec--settled)
        (should-not superchat-changes--notices)
        (should-not (un--shown))))))

(ert-deftest unattended-notice/the-users-next-execution-does-not-clear-a-notice ()
  (un--fresh
    (un--with-task '((:text "ANSWER") (:text "MINE")) (session _fired)
      (should (= 1 (length superchat-changes--notices)))
      (hm--turn (get-buffer (superchat--session-buffer-name session)) "and now me")
      (wrec--settled)
      (should (= 1 (length superchat-changes--notices)))
      (should (string-match-p "1" (un--shown))))))

(ert-deftest unattended-notice/several-notices-are-one-mark-with-their-number ()
  (un--fresh
    (hm--fixture (list '(:text "ONE") '(:text "TWO"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (first (hm--turn view "one"))
             (second (hm--turn view "two")))
        (dolist (run (list first second))
          (bu--fact session "timer.started"
                    (list :timer-id (concat "t-" run) :occurrence 1 :run-id run))
          (superchat-changes--note-unattended
           nil (list :status 'completed :session-id session :run-id run))
          ;; Told twice of one ending, it is still one notice.
          (superchat-changes--note-unattended
           nil (list :status 'completed :session-id session :run-id run)))
        (should (= 2 (length superchat-changes--notices)))
        (should (= 1 (un--marks)))
        (should (string-match-p "2" (un--shown)))
        (superchat-changes-notice-dismiss first)
        (should (= 1 (length superchat-changes--notices)))
        (should (equal second (plist-get (car superchat-changes--notices) :run)))
        (should (string-match-p "1" (un--shown)))
        (superchat-changes-notices-dismiss-all)
        (should-not (un--shown))
        (should (= 1 (un--marks)))))))

(ert-deftest unattended-notice/a-notice-counts-what-was-left-unsaved ()
  (un--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (run (hm--turn view "one")))
        (bu--with-candidate file
          (bu--fact session "timer.started"
                    (list :timer-id "t1" :occurrence 1 :run-id run))
          (superchat-changes--note-unattended
           nil (list :status 'completed :session-id session :run-id run))
          (should (= 1 (plist-get (car superchat-changes--notices) :unsaved)))
          (with-current-buffer (superchat-changes-notices)
            (should (string-match-p "1 unsaved" (buffer-string)))))))))

(ert-deftest unattended-notice/an-execution-still-running-leaves-none ()
  (un--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (run (hm--turn view "one")))
        (bu--fact session "timer.started"
                  (list :timer-id "t1" :occurrence 1 :run-id run))
        (superchat-changes--note-unattended
         nil (list :status 'running :session-id session :run-id run))
        (should-not superchat-changes--notices)))))

(provide 'unattended-notice-regression)

;;; unattended-notice-regression.el ends here
