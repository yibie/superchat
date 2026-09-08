;;; test-change-set.el --- ERT tests for superchat-change-set -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the atomic buffer Change Set primitive: single-undo-step
;; application, overlap rejection, mid-way error cancellation, Tape
;; recording, and marker-based EDITS input.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-change-set)
(require 'superchat-tape)

(ert-deftest change-set/applies-multiple-edits-in-one-undo-step ()
  "Three edits apply atomically; one `undo' step restores the buffer."
  (let ((buf (generate-new-buffer " *change-set-undo*")))
    (unwind-protect
        (with-current-buffer buf
          (buffer-enable-undo)
          (insert "AAA BBB CCC")
          (setq buffer-undo-list nil)
          (let* ((original (buffer-string))
                 (result (superchat-change-set-apply
                          buf
                          (list (list 1 4 "111")
                                (list 5 8 "222")
                                (list 9 12 "333")))))
            (should (equal result '(:applied 3)))
            (should (equal (buffer-string) "111 222 333"))
            (should (consp buffer-undo-list))
            (primitive-undo 1 buffer-undo-list)
            (should (equal (buffer-string) original))))
      (kill-buffer buf))))

(ert-deftest change-set/overlap-is-rejected-before-any-change ()
  "Overlapping edits signal an error and leave the buffer untouched."
  (let ((buf (generate-new-buffer " *change-set-overlap*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "ABCDEFGH")
          (let ((before (buffer-string)))
            (should-error
             (superchat-change-set-apply
              buf (list (list 1 5 "xx") (list 4 7 "yy"))))
            (should (equal (buffer-string) before))))
      (kill-buffer buf))))

(ert-deftest change-set/error-mid-way-cancels-group ()
  "An error partway through the edits cancels the whole change group,
undoing whichever edits already succeeded."
  (let ((buf (generate-new-buffer " *change-set-mid-error*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "AAAA BBBB")
          (let ((before (buffer-string)))
            ;; Edits apply in descending-BEG order, so (6 10 "1111")
            ;; succeeds first and (1 5 'boom) fails on the malformed,
            ;; non-string/non-character replacement text.
            (should-error
             (superchat-change-set-apply
              buf (list (list 1 5 'boom)
                        (list 6 10 "1111"))))
            (should (equal (buffer-string) before))))
      (kill-buffer buf))))

(ert-deftest change-set/records-edit-applied-with-counts ()
  "Appends `edit.applied' with removed/added counts when SESSION-ID is given."
  (let* ((session-id (format "change-set-counts-%s" (gensym)))
         (buf (generate-new-buffer " *change-set-counts*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "0123456789")
            (let ((result (superchat-change-set-apply
                           buf (list (list 1 4 "abcde") (list 8 11 "Z"))
                           :session-id session-id
                           :run-id "run-1"
                           :source 'unit-test)))
              (should (equal result '(:applied 2)))))
          (let* ((events (superchat-tape-replay-events session-id))
                 (event (car events))
                 (payload (plist-get event :payload)))
            (should (= (length events) 1))
            (should (equal (plist-get event :type) "edit.applied"))
            (should (equal (plist-get event :run-id) "run-1"))
            (should (equal (plist-get payload :buffer) (buffer-name buf)))
            (should (equal (plist-get payload :edits) 2))
            (should (equal (plist-get payload :removed) 6))
            (should (equal (plist-get payload :added) 6))
            (should (equal (plist-get payload :source) "unit-test"))))
      (kill-buffer buf))))

(ert-deftest change-set/markers-are-accepted ()
  "EDITS given as markers (not bare integers) apply correctly."
  (let ((buf (generate-new-buffer " *change-set-markers*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "hello world")
          (let* ((beg (copy-marker 1))
                 (end (copy-marker 6))
                 (result (superchat-change-set-apply
                          buf (list (list beg end "goodbye")))))
            (should (equal result '(:applied 1)))
            (should (equal (buffer-string) "goodbye world"))))
      (kill-buffer buf))))

;;; test-change-set.el ends here
