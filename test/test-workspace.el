;;; test-workspace.el --- Tests for superchat-workspace -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-workspace)

(defmacro superchat-test--with-workspace (&rest body)
  "Execute BODY in a fresh temp buffer with workspace region set."
  `(let ((superchat-workspace-auto-create nil)
         (superchat-workspace--start nil)
         (superchat-workspace--end nil))
     (with-temp-buffer
       (insert "before region\n"
               "line one\n"
               "line two\n"
               "after region\n")
       (superchat-workspace-set-region 15 33)  ;; "line one\nline two\n" = 18 chars
       ,@body)))

(ert-deftest test-workspace-region-read ()
  "Workspace read returns region content."
  (superchat-test--with-workspace
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should (string-match-p "line two" content))
     (should-not (string-match-p "before region" content))
     (should-not (string-match-p "after region" content)))))

(ert-deftest test-workspace-region-write-replace ()
  "Workspace write replaces region content."
  (superchat-test--with-workspace
   (superchat-workspace-write "new content")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "new content" content))
     (should-not (string-match-p "line one" content))
     (should-not (string-match-p "line two" content)))))

(ert-deftest test-workspace-region-write-append ()
  "Workspace write with append adds content at end of region."
  (superchat-test--with-workspace
   (superchat-workspace-write "appended" t)
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should (string-match-p "line two" content))
     (should (string-match-p "appended" content)))))

(ert-deftest test-workspace-region-markers-track-edits ()
  "Markers auto-track buffer changes."
  (superchat-test--with-workspace
   ;; Insert before region — markers move
   (goto-char 1)
   (insert "PREFIX\n")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should-not (string-match-p "PREFIX" content)))
   ;; Insert after region — markers unaffected
   (goto-char (point-max))
   (insert "\nSUFFIX")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line two" content))
     (should-not (string-match-p "SUFFIX" content)))))

(ert-deftest test-workspace-info-active-region ()
  "Workspace info reports active region details."
  (superchat-test--with-workspace
   (let ((info (superchat-workspace-info)))
     (should (string-match-p "active region" info)))))

(ert-deftest test-workspace-clear-deactivates ()
  "Clearing workspace makes it inactive."
  (superchat-test--with-workspace
   (should (superchat-workspace--active-p))
   (superchat-workspace-clear)
   (should-not (superchat-workspace--active-p))))

(ert-deftest test-workspace-read-nil-when-inactive ()
  "Workspace read returns nil when no region is active and no fallback buffer."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--start nil)
        (superchat-workspace--end nil))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (should (null (superchat-workspace-read)))))

(ert-deftest test-workspace-fallback-write-replaces ()
  "Non-append writes to the fallback buffer replace its content."
  (let ((superchat-workspace-auto-create t)
        (superchat-workspace--start nil)
        (superchat-workspace--end nil))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (unwind-protect
        (progn
          (superchat-workspace-write "first")
          (superchat-workspace-write "second")
          (let ((content (superchat-workspace-read)))
            (should (string-match-p "second" content))
            (should-not (string-match-p "first" content)))
          (superchat-workspace-write "third" t)
          (let ((content (superchat-workspace-read)))
            (should (string-match-p "second" content))
            (should (string-match-p "third" content))))
      (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
        (kill-buffer superchat-workspace-fallback-buffer)))))

(ert-deftest test-workspace-no-region-no-fallback-errors ()
  "Write errors when no region, no auto-create, and no fallback buffer."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--start nil)
        (superchat-workspace--end nil))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (should-error (superchat-workspace-write "test"))))

(provide 'test-workspace)

;;; test-workspace.el ends here
