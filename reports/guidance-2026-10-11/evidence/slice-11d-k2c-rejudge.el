;; Partial offline re-judge of K2C: the two event-based conditions only,
;; with the very defuns of checks-goals.el.
(require 'cl-lib)
(defvar ts-evidence nil)
(with-temp-buffer
  (insert-file-contents (car command-line-args-left))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'defun)
                     (memq (nth 1 form) '(tsj-phase1-events tsj-phase1-outcome)))
            (eval form t))))
    (end-of-file nil)))
(dolist (run (cdr command-line-args-left))
  (let* ((ts-evidence (expand-file-name "evidence/" run))
         (events (tsj-phase1-events))
         (outcome (tsj-phase1-outcome events))
         (round (cl-some (lambda (e) (member (car e) '("queue.queued" "work.round"))) events))
         (later (and events
                     (or (and (equal "waiting" outcome)
                              (cl-some (lambda (e) (and (equal (car e) "work.waiting")
                                                        (equal "user" (plist-get (cdr e) :for))))
                                       events))
                         (and (equal "continuable" outcome) (not round)))
                     t)))
    (princ (format "%s\t%s\tleft-for-later=%s\tno-round=%s\tdid-first-three=not-gathered\n"
                   (file-name-nondirectory (directory-file-name run)) outcome
                   (if later "pass" "FAIL") (if (and events (not round)) "pass" "FAIL")))))
(setq command-line-args-left nil)
