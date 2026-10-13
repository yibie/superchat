;;; emacsclient-cancel-regression.el --- what a cancel through emacsclient says -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 8d (architecture map of 2026-10-13, contradiction C20).
;; `superchat-emacsclient-run-cancel' answered "status": "cancelled" whenever
;; the Runtime's cancel returned non-nil.  The Runtime returns non-nil also
;; for a Run that had already ended (it then only stops input control), and
;; a cancelled Run with unsettled work ends `audit-incomplete'.  The answer
;; now carries the status the Runtime holds when the answer is made, and
;; whether that status is final.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/emacsclient-cancel-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^emacsclient-cancel/")'

;;; Code:

(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-emacsclient)

(defun eccl--cancel (run)
  "Cancel RUN through the adapter; return the answer as an alist."
  (json-parse-string (superchat-emacsclient-run-cancel run)
                     :object-type 'alist :null-object nil :false-object nil))

(defmacro eccl--with (script &rest body)
  (declare (indent 1) (debug t))
  `(hm--fixture ,script
     (let ((mounted (superchat-plugin-state 'emacsclient)))
       (unless mounted (superchat-plugin-mount 'emacsclient))
       (unwind-protect (progn ,@body)
         (unless mounted (superchat-plugin-unmount 'emacsclient))))))

(ert-deftest emacsclient-cancel/a-run-that-had-ended-is-not-said-to-be-cancelled ()
  (eccl--with (list '(:text "ECCL-DONE"))
    (let* ((run (hm--turn (smoke--open-chat) "hello"))
           (answer (eccl--cancel run)))
      ;; Guard: the Run completed, and still is completed.
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (when (alist-get 'ok answer)
        (should (equal "completed" (alist-get 'status answer)))
        (should (eq t (alist-get 'terminal answer))))
      (should-not (equal "cancelled" (alist-get 'status answer))))))

(ert-deftest emacsclient-cancel/the-answer-is-the-status-the-runtime-holds ()
  (eccl--with (list '(:text "NEVER-DELIVERED" :delay 5))
    (let* ((run (smoke--send (smoke--open-chat) "hang")))
      (should (smoke--wait (lambda () (member run (superchat-runtime-active-runs))) 10))
      (let ((held nil) answer)
        ;; What the Runtime says at the moment the adapter answers.
        (cl-letf* ((cancel (symbol-function 'superchat-runtime-cancel))
                   ((symbol-function 'superchat-runtime-cancel)
                    (lambda (&rest args)
                      (prog1 (apply cancel args)
                        (setq held (plist-get (superchat-runtime-status run) :status))))))
          (setq answer (eccl--cancel run)))
        (should held)
        (should (eq t (alist-get 'ok answer)))
        (should (equal (symbol-name held) (alist-get 'status answer)))
        (should (eq (and (superchat-runtime--terminal-p held) t)
                    (alist-get 'terminal answer)))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))))))

(defun eccl--answer-when-runtime-says (after)
  "Cancel a hanging Run whose status reads AFTER once the cancel was asked for."
  (let* ((run (smoke--send (smoke--open-chat) "hang")))
    (should (smoke--wait (lambda () (member run (superchat-runtime-active-runs))) 10))
    (let* ((status (symbol-function 'superchat-runtime-status))
           (cancelled nil)
           answer)
      (cl-letf (((symbol-function 'superchat-runtime-cancel)
                 (lambda (&rest _) (setq cancelled t)))
                ((symbol-function 'superchat-runtime-status)
                 (lambda (id)
                   (let ((real (funcall status id)))
                     (if (and cancelled (equal id run))
                         (plist-put (copy-sequence real) :status after)
                       real)))))
        (setq answer (eccl--cancel run)))
      (superchat-runtime-cancel run)
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
      answer)))

(ert-deftest emacsclient-cancel/an-audit-incomplete-ending-is-said-as-it-is ()
  "The Runtime's word is passed on, whatever it is."
  (eccl--with (list '(:text "NEVER-DELIVERED" :delay 5))
    (let ((answer (eccl--answer-when-runtime-says 'audit-incomplete)))
      (should (equal "audit-incomplete" (alist-get 'status answer)))
      (should (eq t (alist-get 'terminal answer))))))

(ert-deftest emacsclient-cancel/a-run-still-ending-is-not-said-to-have-ended ()
  (eccl--with (list '(:text "NEVER-DELIVERED" :delay 5))
    (let ((answer (eccl--answer-when-runtime-says 'running)))
      (should (eq t (alist-get 'ok answer)))
      (should (equal "running" (alist-get 'status answer)))
      (should (assq 'terminal answer))
      (should-not (alist-get 'terminal answer)))))

(ert-deftest emacsclient-cancel/an-unknown-run-is-not-found ()
  "Guard: green before and after."
  (eccl--with nil
    (let ((answer (eccl--cancel "run-that-never-was")))
      (should-not (alist-get 'ok answer))
      (should (string-match-p "run-not-found" (json-encode answer))))))

(provide 'emacsclient-cancel-regression)
;;; emacsclient-cancel-regression.el ends here
