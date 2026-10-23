;;; review-wait-regression.el --- rounds wait for the user's review -*- lexical-binding: t; -*-

;;; Commentary:
;; Found in the user's GUI field test of 2026-10-23.  A round of a work record
;; left an edit unsaved, so the review screen came up ("Review 1/2 ...
;; Unsaved.  The diff below is what saving would write to disk"), and the next
;; round started at once behind it.  While the user read the diff, the model
;; changed the buffer again, so the screen no longer showed what `s' would
;; save (G27), and the work went on with nothing approved.
;;
;; The user's ruling (2026-10-23): where an approval comes up, the work stops
;; until it is given.  A round that leaves edits for the review screen queues
;; no next round; the conversation says the work waits for the review.  The
;; rounds go on once the user has been through the screen (`s' saves, `n'
;; leaves unsaved); `q' stops reviewing and leaves the work paused, and
;; `/work resume' goes on.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/review-wait-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^review-wait/")'

;;; Code:

(load (expand-file-name "braked-round-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-changes-review-after-run)

(defun rw--script (buffer-name)
  "The record is started, round 1 edits BUFFER-NAME, round 2 completes it."
  (list (gr--start) '(:text "The record is started.")
        `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                       (old_string . "hello") (new_string . "RW-EDIT"))))
        '(:text "Edited.")
        (wrec--call '(action . "done") '(conclusion . "RW-DONE"))
        '(:text "All ticked.")))

(defmacro rw--with (&rest body)
  "BODY once round 1 has edited FILE and ended, with the review screen up."
  (declare (indent 0) (debug t))
  `(let ((superchat-changes-review-after-run t))
     (unwind-protect
         (br--with #'rw--script
           (let ((id (gr--record)))
             ;; The timer that shows the screen has run.
             (accept-process-output nil 0.1)
             ,@body))
       (when-let* ((review (get-buffer "*superchat review*")))
         (kill-buffer review)))))

(defun rw--review-key (command)
  "Press the review screen's key for COMMAND, then let what follows run."
  (with-current-buffer (get-buffer "*superchat review*")
    (funcall command))
  (gr--quiet))

(ert-deftest review-wait/a-round-that-leaves-an-edit-waits-for-the-review ()
  (rw--with
    (should (= 1 (gr--count session "work.round")))
    (should-not (superchat-work-continuing-p id))
    (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
    (should (buffer-live-p (get-buffer "*superchat review*")))
    (should (string-match-p
             (format "Superchat paused work record %s: it waits for your review of 1 unsaved buffer"
                     (regexp-quote id))
             (br--text view)))))

(ert-deftest review-wait/saving-on-the-review-goes-on ()
  (rw--with
    (rw--review-key #'superchat-changes-review-save)
    (should-not (buffer-modified-p file))
    (should (= 2 (gr--count session "work.round")))
    (should (eq 'completed (superchat-work-status (superchat-work-get id))))))

(ert-deftest review-wait/leaving-unsaved-on-the-review-goes-on ()
  "`n' is the user's answer too: the edit stays unsaved and the work goes on."
  (rw--with
    (rw--review-key #'superchat-changes-review-next)
    (should (buffer-modified-p file))
    (should (= 2 (gr--count session "work.round")))
    (should (eq 'completed (superchat-work-status (superchat-work-get id))))))

(ert-deftest review-wait/stopping-the-review-keeps-the-work-paused ()
  (rw--with
    (rw--review-key #'superchat-changes-review-quit)
    (should (= 1 (gr--count session "work.round")))
    (should-not (superchat-work-continuing-p id))
    (should (string-match-p "/work resume goes on" (br--text view)))))

(ert-deftest review-wait/no-review-screen-no-wait ()
  "Guard: with the review screen turned off nothing waits for it."
  (let ((superchat-changes-review-after-run nil))
    (br--with #'rw--script
      (let ((id (gr--record)))
        (should (= 2 (gr--count session "work.round")))
        (should (eq 'completed (superchat-work-status (superchat-work-get id))))))))

(provide 'review-wait-regression)

;;; review-wait-regression.el ends here
