;;; shared-resource-regression.el --- one global resource, many installations -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-11-a-shared-resource-outlives-one-installation.md
;; (review round 3, R3-08 and R3-03).  A plugin may be installed in more than
;; one Scope.  A hook on a global variable and a table of Emacs timers are one
;; thing however many installations want them: disposing one installation
;; leaves them to the others, and the last one out takes them down.  Turning
;; a row off and on in the same Emacs does not forget how often a task was
;; tried or how long it still waits.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/shared-resource-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^shared-resource/")'

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
(require 'superchat-work)

(defmacro shres--second-installation (rows &rest body)
  "Install ROWS a second time in a child Scope, dispose only that, run BODY."
  (declare (indent 1) (debug t))
  `(let ((scope (superchat-scope-create "shres-second" superchat-scope-global)))
     (unwind-protect
         (progn
           (dolist (row ,rows) (superchat-plugin-mount row nil scope))
           ;; The zero-delay set-up of an installation, were there one.
           (sleep-for 0.03)
           (superchat-scope-dispose scope)
           ,@body)
       (ignore-errors (superchat-scope-dispose scope)))))

;;;; The helper ---------------------------------------------------------------

(ert-deftest shared-resource/the-first-holder-acquires-and-the-last-releases ()
  (let ((a (superchat-scope-create "shres-a" superchat-scope-global))
        (b (superchat-scope-create "shres-b" superchat-scope-global))
        (log nil))
    (unwind-protect
        (let* ((take (lambda () (push 'acquire log)))
               (give (lambda () (push 'release log)))
               (undo-a (superchat-scope-shared-effect a 'shres-key take give)))
          (superchat-scope-shared-effect b 'shres-key take give)
          (should (equal '(acquire) log))
          (funcall undo-a)
          ;; Undoing the same installation twice lets go once.
          (funcall undo-a)
          (should (equal '(acquire) log))
          (superchat-scope-dispose b)
          (should (equal '(release acquire) log))
          ;; Held again afterwards, it is acquired again.
          (superchat-scope-shared-effect a 'shres-key take give)
          (should (equal '(acquire release acquire) log)))
      (ignore-errors (superchat-scope-dispose a))
      (ignore-errors (superchat-scope-dispose b)))))

;;;; R3-08: disposing a second installation -----------------------------------

(ert-deftest shared-resource/a-second-changes-installation-leaves-the-notice-hook ()
  "The task that ends inside its own send still leaves its notice (N2)."
  (should (superchat-plugin-row-active-p 'changes))
  (shres--second-installation '(changes)
    (should (memq #'superchat-changes--note-started-late
                  superchat-timer-started-functions))
    (un--fresh
      (un--with-task-answered-at-once '((:text "SYNC")) (session _fired)
        (should (= 1 (length (hm--events session "timer.started"))))
        (should (= 1 (length superchat-changes--notices)))))))

(ert-deftest shared-resource/a-second-timing-installation-leaves-what-is-armed ()
  "A reminder and a wait for a time stay armed for the installation still there."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "SHRES-PROMISED-WORK"))
           (due (+ (float-time) 3600))
           (timer (superchat-timer-create session 'reminder "SHRES-REMINDER" due)))
      (unwind-protect
          (progn
            (superchat-work-wait session id "time" "continue" due)
            (should (gethash id superchat-work--armed))
            (should (gethash (plist-get timer :id) superchat-timer--armed))
            (shres--second-installation '(timers work-records)
              (should (timerp (gethash id superchat-work--armed)))
              (should (timerp (gethash (plist-get timer :id) superchat-timer--armed)))
              (should (memq #'superchat-work--on-user-input
                            superchat-user-input-functions))))
        (superchat-work-disarm-all)
        (superchat-timer-disarm-all)))))

(ert-deftest shared-resource/the-last-installation-out-takes-everything-down ()
  "The guard: with no installation left, nothing stays armed or hooked."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "SHRES-LAST-OUT"))
           (due (+ (float-time) 3600))
           (timer (superchat-timer-create session 'reminder "SHRES-LAST-REMINDER" due)))
      (superchat-work-wait session id "time" "continue" due)
      (unwind-protect
          (progn
            (dolist (row '(timers work-records changes))
              (superchat-plugin-unmount row))
            (should-not (gethash id superchat-work--armed))
            (should-not (gethash (plist-get timer :id) superchat-timer--armed))
            (should-not (memq #'superchat-work--on-user-input
                              superchat-user-input-functions))
            (should-not (memq #'superchat-changes--note-started-late
                              superchat-timer-started-functions)))
        (dolist (row '(timers work-records changes))
          (unless (superchat-plugin-row-active-p row)
            (superchat-plugin-mount row)))
        (sleep-for 0.03)
        (should (memq #'superchat-changes--note-started-late
                      superchat-timer-started-functions))
        (should (memq #'superchat-work--on-user-input
                      superchat-user-input-functions))
        (superchat-work-disarm-all)
        (superchat-timer-disarm-all)))))

;;;; R3-03: off and on again is not a new Emacs -------------------------------

(defmacro shres--failing-sends (tries &rest body)
  "Run BODY while every send fails, counting the attempts in TRIES."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (cl-letf (((symbol-function 'superchat-send-input)
                  (lambda () (cl-incf ,tries) (error "SHRES start unavailable"))))
         ,@body)
     (clrhash superchat-timer--retries)
     (clrhash superchat-timer--not-before)
     (unless (superchat-plugin-row-active-p 'timers)
       (superchat-plugin-mount 'timers))
     (sleep-for 0.03)
     (superchat-timer-disarm-all)))

(ert-deftest shared-resource/remounting-does-not-grant-another-try ()
  (wtimer--with nil (p)
    (let ((session (cdr (tsf--task p "SHRES-RETRY")))
          (superchat-timer-retry-limit 0)
          (tries 0))
      (clrhash superchat-timer--retries)
      (clrhash superchat-timer--not-before)
      (shres--failing-sends tries
        (superchat-timer-catch-up)
        (should (= tries 1))
        (superchat-plugin-unmount 'timers)
        (superchat-plugin-mount 'timers)
        (should-not (smoke--wait (lambda () (> tries 1)) 0.5))
        (should (= 1 (tsf--count session "timer.sending")))))))

(ert-deftest shared-resource/remounting-does-not-skip-the-wait ()
  (wtimer--with nil (p)
    (let ((session (cdr (tsf--task p "SHRES-WAIT")))
          (superchat-timer-retry-limit 2)
          (tries 0))
      (clrhash superchat-timer--retries)
      (clrhash superchat-timer--not-before)
      (shres--failing-sends tries
        (superchat-timer-catch-up)
        (should (= tries 1))
        (superchat-plugin-unmount 'timers)
        (superchat-plugin-mount 'timers)
        (should-not (smoke--wait (lambda () (> tries 1)) 0.5))
        ;; Once the wait is over the retry it is owed is still made.
        (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds))
        (superchat-timer-retry-tasks)
        (should (= tries 2))
        (should (= 2 (tsf--count session "timer.sending")))))))

(provide 'shared-resource-regression)
;;; shared-resource-regression.el ends here
