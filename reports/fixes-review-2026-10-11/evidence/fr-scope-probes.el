;;; fr-scope-probes.el --- shared resources and doubled rows -*- lexical-binding: t; -*-
;; Review of abb4f199 (R3-08 / R3-03).  Each test asserts what is OBSERVED on
;; today's code, defects included; a test named "...guard..." asserts the
;; behavior the commit promises.  Scripted provider and isolated DB only.
;; Loaded the way scripts/shared-resource-regression.el loads its fixtures.
(load (expand-file-name "scripts/unattended-notice-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (progn
        (load (expand-file-name "scripts/timer-start-fault-regression.el" default-directory) nil t)
        (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t))
    (advice-remove 'load skip-loaded)))
(require 'superchat-work)

;;;; superchat-scope-shared-effect ------------------------------------------------

(ert-deftest fr-scope/an-acquire-that-signals-leaves-a-holder-nobody-owns ()
  "superchat-plugin.el:115-131: ACQUIRE runs before the undo is registered.
When ACQUIRE signals, the count stays at 1 with no effect to bring it back,
so the next holder is counted as the second and the resource is never set up."
  (let ((a (superchat-scope-create "fr-a" superchat-scope-global))
        (b (superchat-scope-create "fr-b" superchat-scope-global))
        (acquired 0))
    (unwind-protect
        (progn
          (remhash 'fr-key superchat-scope--shared)
          (should-error (superchat-scope-shared-effect
                         a 'fr-key (lambda () (error "FR injected: acquire fails")) #'ignore))
          ;; The phantom holder.
          (should (equal 1 (gethash 'fr-key superchat-scope--shared)))
          (should-not (superchat-scope-effects a))
          ;; A later installation whose acquire works never acquires.
          (superchat-scope-shared-effect b 'fr-key (lambda () (cl-incf acquired)) #'ignore)
          (should (= 0 acquired))
          (should (equal 2 (gethash 'fr-key superchat-scope--shared)))
          (superchat-scope-dispose b)
          (should (equal 1 (gethash 'fr-key superchat-scope--shared)))
          (princ "FR-SCOPE OBSERVED: acquire signalled once; count stays 1 with no undo; next holder counted as 2nd, acquire never called; dispose leaves 1.\n"))
      (remhash 'fr-key superchat-scope--shared)
      (ignore-errors (superchat-scope-dispose a))
      (ignore-errors (superchat-scope-dispose b)))))

(superchat-define-plugin fr-failing-row
  :doc "A row whose apply fails after it held a shared resource."
  :apply (lambda (scope _config)
           (superchat-scope-shared-effect
            scope 'fr-key2
            (lambda () (push 'acquire fr-scope--log))
            (lambda () (push 'release fr-scope--log)))
           (error "FR injected: apply fails after holding")))

(defvar fr-scope--log nil)

(ert-deftest fr-scope/guard-an-apply-that-fails-after-holding-lets-go ()
  "The commit's own path: the child Scope is disposed when apply fails, so a
hold registered before the failure is released and the count returns to 0."
  (let ((scope (superchat-scope-create "fr-c" superchat-scope-global)))
    (setq fr-scope--log nil)
    (unwind-protect
        (progn
          (remhash 'fr-key2 superchat-scope--shared)
          (superchat-plugin-mount 'fr-failing-row nil scope)
          (should (eq 'failed (superchat-plugin-state 'fr-failing-row scope)))
          (should (equal '(release acquire) fr-scope--log))
          (should-not (gethash 'fr-key2 superchat-scope--shared)))
      (ignore-errors (superchat-scope-dispose scope))
      (remhash 'fr-key2 superchat-scope--shared))))

;;;; Two installations of one row: what the per-Scope observers do ---------------

(defmacro fr-scope--with-second (rows &rest body)
  "Install ROWS a second time in a child Scope and run BODY while both are up."
  (declare (indent 1) (debug t))
  `(let ((scope (superchat-scope-create "fr-second" superchat-scope-global)))
     (unwind-protect
         (progn
           (dolist (row ,rows) (superchat-plugin-mount row nil scope))
           (sleep-for 0.03)
           ,@body)
       (ignore-errors (superchat-scope-dispose scope)))))

(ert-deftest fr-scope/guard-a-doubled-changes-row-announces-and-reviews-once ()
  "The decision says this was not checked.  Tried: a second `changes'
installation in a child Scope of the global one, a user Run with an edit.
Observed on the first run: announced=1, review screen=1 -- a listener
registered in a child Scope is not visible from the global Scope a chat
Run executes under (`superchat-scope-merge' walks up, not down), so the
second installation's observers do not fire for it."
  (wrec--with nil (p)
    (let* ((view (wrec--view p))
           (buffer (get-file-buffer (expand-file-name "a.txt" p)))
           (announced 0) (reviewed 0))
      (setq smoke--script (list (bu--edit-round (buffer-name buffer) "hello" "HELLO")
                                '(:text "EDITED")))
      (fr-scope--with-second '(changes)
        (cl-letf (((symbol-function 'superchat-changes--announce)
                   (lambda (_text) (cl-incf announced)))
                  ((symbol-function 'superchat-changes-review)
                   (lambda (&rest _) (cl-incf reviewed) nil)))
          (hm--turn view "Change it")
          (should (buffer-modified-p buffer))
          ;; The screen is shown from a 0-delay timer.
          (smoke--wait (lambda () (>= reviewed 2)) 0.5)
          (princ (format "FR-SCOPE DOUBLED changes: announced=%d review-screen calls=%d notices=%d listeners visible from global=%d\n"
                         announced reviewed (length superchat-changes--notices)
                         (length (superchat-event-listeners superchat-scope-global 'turn-completed))))
          (should (= 1 announced))
          (should (= 1 reviewed)))))))

(ert-deftest fr-scope/guard-a-doubled-work-records-row-writes-one-outcome ()
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "FR-DOUBLED-GOAL")))
      (fr-scope--with-second '(work-records)
        (hm--turn view "Do this step")
        (should (equal '("continuable") (plist-get (superchat-work-get id) :outcomes)))
        (should (= 1 (length (hm--events session "work.outcome"))))))))

;;;; R3-03's work-record analogue -------------------------------------------------

(ert-deftest fr-scope/remounting-work-records-grants-a-failed-wake-another-try-at-once ()
  "superchat-work--try-wake (superchat-work.el:817-836) consults the retry
count only after a failure; superchat-work-wake-due tries every due record.
So remounting the row, or any arm-all, wakes a record that is out of tries
or still inside its 300 s wait: the count the commit keeps is never read."
  (wrec--with nil (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-limit 0)
          (tries 0))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "FR-REMOUNT-WAKE"))
               (id (cdr pair)))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _) (cl-incf tries) (error "FR injected: send fails"))))
            (superchat-work-wake-due)
            (should (= 1 tries))
            (should (equal 1 (gethash id superchat-work--retries)))
            ;; Out of tries: nothing is armed for it.
            (should-not (gethash id superchat-work--armed))
            ;; The same virtual time; the row is turned off and on.
            (superchat-plugin-unmount 'work-records)
            (superchat-plugin-mount 'work-records)
            (sleep-for 0.05)
            (should (smoke--wait (lambda () (>= tries 2)) 2))
            (princ (format "FR-SCOPE WAKE: retry-limit=0; first wake failed; unmount/remount at +0 s => %d sends, work.woken facts=%d (each try opens a new conversation)\n"
                           tries (length (plist-get (superchat-work-get id) :wakes))))
            (should (= 2 tries))))))))

(ert-deftest fr-scope/remounting-work-records-skips-the-wake-wait ()
  (wrec--with nil (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-limit 3)
          (tries 0))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "FR-REMOUNT-WAIT"))
               (id (cdr pair)))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _) (cl-incf tries) (error "FR injected: send fails"))))
            (superchat-work-wake-due)
            (should (= 1 tries))
            ;; A retry is armed 300 s out.
            (should (timerp (gethash id superchat-work--armed)))
            (superchat-plugin-unmount 'work-records)
            (superchat-plugin-mount 'work-records)
            (sleep-for 0.05)
            (should (smoke--wait (lambda () (>= tries 2)) 2))
            (princ (format "FR-SCOPE WAKE WAIT: limit=3, wait=300 s; remount at +0 s => %d sends\n" tries))
            (should (= 2 tries))))))))

(ert-deftest fr-scope/guard-the-timer-row-keeps-its-count-across-a-remount ()
  "The side the commit fixed, for contrast."
  (wtimer--with nil (p)
    (let ((session (cdr (tsf--task p "FR-TIMER-RETRY")))
          (superchat-timer-retry-limit 0)
          (tries 0))
      (clrhash superchat-timer--retries)
      (clrhash superchat-timer--not-before)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda () (cl-incf tries) (error "FR start unavailable"))))
            (superchat-timer-catch-up)
            (should (= tries 1))
            (superchat-plugin-unmount 'timers)
            (superchat-plugin-mount 'timers)
            (should-not (smoke--wait (lambda () (> tries 1)) 0.5))
            (should (= 1 (tsf--count session "timer.sending"))))
        (clrhash superchat-timer--retries)
        (clrhash superchat-timer--not-before)
        (unless (superchat-plugin-row-active-p 'timers) (superchat-plugin-mount 'timers))
        (sleep-for 0.03)
        (superchat-timer-disarm-all)))))

(ert-run-tests-batch-and-exit "^fr-scope/")
;;; fr-scope-probes.el ends here
