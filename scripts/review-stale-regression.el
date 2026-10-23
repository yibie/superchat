;;; review-stale-regression.el --- s saves only what the review showed -*- lexical-binding: t; -*-

;;; Commentary:
;; G27 of the llm-pi-ai field test (2026-10-23 00:05): the review screen
;; drew its diff once, the model went on editing the buffer, and the screen
;; still showed the old diff.  s saves the buffer as it is, so the user
;; would have saved text they had not seen.  Now s saves only when the
;; buffer is as it was when the diff was drawn; otherwise the screen is
;; drawn again, says so, and waits for another s.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/review-stale-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^review-stale/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro rs--with-review (var &rest body)
  "Open a review screen for one edited file buffer bound to VAR, run BODY."
  (declare (indent 1) (debug t))
  `(let* ((dir (make-temp-file "rs-project" t))
          (file (expand-file-name "lib.el" dir))
          (superchat-changes-review-use-diffs nil)
          ,var review)
     (with-temp-file file (insert "old\n"))
     (setq ,var (find-file-noselect file))
     (unwind-protect
         (progn
           (with-current-buffer ,var
             (goto-char (point-max))
             (insert "seen\n"))
           (setq review (superchat-changes-review (list (list :buffer ,var))))
           ,@body)
       (when (buffer-live-p review) (kill-buffer review))
       (with-current-buffer ,var (set-buffer-modified-p nil))
       (kill-buffer ,var)
       (delete-directory dir t))))

(defun rs--disk (buffer)
  (with-temp-buffer
    (insert-file-contents (buffer-file-name buffer))
    (buffer-string)))

(defun rs--screen ()
  (with-current-buffer superchat-changes-review-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest review-stale/s-saves-what-the-screen-shows ()
  "Guard: no change after the diff was drawn, s saves at once."
  (rs--with-review buffer
    (with-current-buffer superchat-changes-review-buffer-name
      (superchat-changes-review-save))
    (should (equal "old\nseen\n" (rs--disk buffer)))
    (should-not (buffer-modified-p buffer))))

(ert-deftest review-stale/a-change-after-the-diff-is-not-saved-unseen ()
  (rs--with-review buffer
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "unseen\n"))
    (with-current-buffer superchat-changes-review-buffer-name
      (superchat-changes-review-save))
    (should (equal "old\n" (rs--disk buffer)))
    (should (buffer-modified-p buffer))
    ;; The screen now shows the change and is still up.
    (should (get-buffer superchat-changes-review-buffer-name))
    (should (string-match-p "^\\+unseen$" (rs--screen)))
    (should (string-match-p "changed after" (rs--screen)))))

(ert-deftest review-stale/a-second-s-saves-the-redrawn-text ()
  (rs--with-review buffer
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "unseen\n"))
    (with-current-buffer superchat-changes-review-buffer-name
      (superchat-changes-review-save))
    (with-current-buffer superchat-changes-review-buffer-name
      (superchat-changes-review-save))
    (should (equal "old\nseen\nunseen\n" (rs--disk buffer)))
    (should-not (buffer-modified-p buffer))))

(ert-deftest review-stale/g-redraws-and-then-s-saves ()
  (rs--with-review buffer
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "unseen\n"))
    (with-current-buffer superchat-changes-review-buffer-name
      (superchat-changes-review-refresh)
      (superchat-changes-review-save))
    (should (equal "old\nseen\nunseen\n" (rs--disk buffer)))))

(provide 'review-stale-regression)

;;; review-stale-regression.el ends here
