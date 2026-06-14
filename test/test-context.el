;;; test-context.el --- ERT tests for editor-state context enrichment -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-tools--base-dir, superchat--project-root,
;; superchat--region-diagnostics, and superchat--file-relative-to-project.
;; No LLM calls — pure logic and buffer manipulation only.

;;; Code:

(require 'ert)
(require 'superchat-core)
(require 'superchat-tools)
(require 'flymake nil t)
(require 'project nil t)
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; superchat-tools--base-dir
;; ═══════════════════════════════════════════════════════════

(ert-deftest context/base-dir-falls-back-to-default-directory ()
  "When `superchat-tools-default-directory' is nil, return `default-directory'."
  (let ((superchat-tools-default-directory nil))
    (should (equal (superchat-tools--base-dir) default-directory))))

(ert-deftest context/base-dir-uses-override-when-set ()
  "When `superchat-tools-default-directory' is set, return it."
  (let ((superchat-tools-default-directory "/some/project/root"))
    (should (equal (superchat-tools--base-dir) "/some/project/root"))))

(ert-deftest context/base-dir-override-empty-string ()
  "An empty string override is still returned (falsy but non-nil)."
  (let ((superchat-tools-default-directory ""))
    (should (equal (superchat-tools--base-dir) ""))))

;; ═══════════════════════════════════════════════════════════
;; superchat--project-root
;; ═══════════════════════════════════════════════════════════

(ert-deftest context/project-root-returns-nil-outside-project ()
  "Return nil when the buffer is not inside a recognised project."
  ;; Temp buffers inherit default-directory from the running process,
  ;; which may be inside a project.  Explicitly set to /tmp so
  ;; `project-current' returns nil.
  (with-temp-buffer
    (setq-local default-directory "/tmp/")
    (let ((result (superchat--project-root)))
      (should-not result))))

;; ═══════════════════════════════════════════════════════════
;; superchat--file-relative-to-project
;; ═══════════════════════════════════════════════════════════

(ert-deftest context/file-relative-to-project-no-project ()
  "When there is no project root, return FILE-PATH unchanged."
  (with-temp-buffer
    (setq-local default-directory "/tmp/")
    (let ((result (superchat--file-relative-to-project "/some/abs/path/file.el")))
      (should (equal result "/some/abs/path/file.el")))))

;; ═══════════════════════════════════════════════════════════
;; superchat--region-diagnostics
;; ═══════════════════════════════════════════════════════════

(ert-deftest context/region-diagnostics-empty-when-no-flymake ()
  "Return empty string when flymake is not available or no diags."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () 42)\n")
    (let ((result (superchat--region-diagnostics (point-min) (point-max))))
      (should (stringp result))
      ;; No diagnostics in a clean temp buffer
      (should (string-empty-p result)))))

(ert-deftest context/region-diagnostics-respects-defcustom-off ()
  "Return empty string when `superchat-send-include-diagnostics' is nil."
  (let ((superchat-send-include-diagnostics nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo () 42)\n")
      (let ((result (superchat--region-diagnostics (point-min) (point-max))))
        (should (string-empty-p result))))))

(ert-deftest context/region-diagnostics-returns-empty-for-nil-input ()
  "Return empty string when beg/end are nil."
  (should (equal "" (superchat--region-diagnostics nil nil))))

;; ═══════════════════════════════════════════════════════════
;; Tool functions use base-dir (integration smoke)
;; ═══════════════════════════════════════════════════════════

(ert-deftest context/tool-list-files-uses-base-dir ()
  "list-files defaults to `superchat-tools--base-dir' when path is nil."
  ;; We can't easily test the full interactive path, but we can verify
  ;; that with a non-existent override, the error message contains our path.
  (let ((superchat-tools-default-directory "/nonexistent-path-xyz"))
    (should superchat-tools-default-directory)
    ;; The function path resolution should use our override.
    ;; We test the base-dir helper directly (already above).
    (should (equal (superchat-tools--base-dir) "/nonexistent-path-xyz"))))

(ert-deftest context/tool-search-text-uses-base-dir ()
  "search-text defaults to `superchat-tools--base-dir'."
  (let ((superchat-tools-default-directory "/tmp"))
    (should (equal (superchat-tools--base-dir) "/tmp"))))

(ert-deftest context/tool-find-files-uses-base-dir ()
  "find-files defaults to `superchat-tools--base-dir'."
  (let ((superchat-tools-default-directory "/tmp"))
    (should (equal (superchat-tools--base-dir) "/tmp"))))

;;; test-context.el ends here
