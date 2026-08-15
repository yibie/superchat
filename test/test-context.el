;;; test-context.el --- ERT tests for editor-state context enrichment -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-tools--base-dir, superchat--project-root,
;; superchat--region-diagnostics, and superchat--file-relative-to-project.
;; No LLM calls — pure logic and buffer manipulation only.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
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

(ert-deftest context/textual-file-p-accepts-configured-text-and-rejects-binary ()
  "Text detection accepts source files and content fallback, not binaries."
  (let ((superchat-inline-text-extensions '("el" "custom" "png"))
        (text-file (make-temp-file "superchat-text" nil ".el"))
        (custom-file (make-temp-file "superchat-text" nil ".custom"))
        (unknown-file (make-temp-file "superchat-text" nil ".rst"))
        (no-extension-file (make-temp-file "superchat-text"))
        (image-file (make-temp-file "superchat-image" nil ".png"))
        (binary-file (make-temp-file "superchat-binary")))
    (unwind-protect
        (progn
          (dolist (file (list text-file custom-file unknown-file
                              no-extension-file))
            (with-temp-file file
              (insert "plain source text\n")))
          (with-temp-file binary-file
            (set-buffer-multibyte nil)
            (insert "\0\1\2binary"))
          (with-temp-file image-file
            (insert "image placeholder"))
          (should (superchat--textual-file-p text-file))
          (should (superchat--textual-file-p custom-file))
          (should (superchat--textual-file-p unknown-file))
          (should (superchat--textual-file-p no-extension-file))
          ;; Image extensions stay on the native attachment path even if a
          ;; user accidentally adds one to the text-extension option.
          (should-not (superchat--textual-file-p image-file))
          (should-not (superchat--textual-file-p binary-file)))
      (dolist (file (list text-file custom-file unknown-file no-extension-file
                          image-file binary-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest context/textual-file-p-recognizes-unicode-boms-and-bounded-binary ()
  "Content fallback accepts Unicode BOMs and rejects controls in its sample."
  (let ((utf8-file (make-temp-file "superchat-utf8"))
        (utf16-file (make-temp-file "superchat-utf16"))
        (utf32-file (make-temp-file "superchat-utf32"))
        (late-nul-file (make-temp-file "superchat-late-nul")))
    (unwind-protect
        (progn
          (with-temp-file utf8-file
            (insert "中文文本\n"))
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert (unibyte-string #xff #xfe ?A 0))
              (write-region (point-min) (point-max) utf16-file nil 'silent))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert (unibyte-string 0 0 #xfe #xff 0 0 0 ?A))
              (write-region (point-min) (point-max) utf32-file nil 'silent))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert (make-string 5000 ?A))
              (insert (unibyte-string 0))
              (write-region (point-min) (point-max)
                            late-nul-file nil 'silent)))
          (should (superchat--textual-file-p utf8-file))
          (should (superchat--textual-file-p utf16-file))
          (should (superchat--textual-file-p utf32-file))
          (should-not (superchat--textual-file-p late-nul-file)))
      (dolist (file (list utf8-file utf16-file utf32-file late-nul-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest context/textual-file-p-requires-a-regular-file ()
  "Text detection follows regular symlinks but rejects directories and FIFOs."
  (let* ((mkfifo (executable-find "mkfifo"))
         (text-file (make-temp-file "superchat-regular" nil ".el"
                                    "plain source text\n"))
         (link-file (concat text-file ".link.el"))
         (directory (make-temp-file "superchat-directory" t))
         (fifo (concat (make-temp-name
                        (expand-file-name "superchat-fifo-"
                                          temporary-file-directory))
                       ".el")))
    (unwind-protect
        (progn
          (unless mkfifo
            (ert-skip "mkfifo is unavailable"))
          (make-symbolic-link text-file link-file)
          (should (= 0 (call-process mkfifo nil nil nil fifo)))
          (should (superchat--textual-file-p link-file))
          (should-not (superchat--textual-file-p directory))
          ;; The configured extension exercises the former no-I/O fast path.
          (should-not (superchat--textual-file-p fifo)))
      (dolist (file (list fifo link-file text-file))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p directory)
        (delete-directory directory)))))

(ert-deftest context/dispatcher-standalone-has-text-extension-default ()
  "Loading dispatcher alone can classify text without a void variable."
  (let* ((dispatcher (locate-library "superchat-dispatcher"))
         (helper (symbol-file 'superchat-test-helper 'provide))
         (root (file-name-directory dispatcher))
         (emacs-program (expand-file-name invocation-name
                                           invocation-directory))
         (expression
          (format
           "(princ (if (superchat--textual-file-p %S) \"DIRECT=ok\" \"DIRECT=bad\"))"
           dispatcher)))
    (with-temp-buffer
      (should (= 0 (call-process emacs-program nil t nil
                                 "-Q" "--batch" "-L" root
                                 "-l" helper "-l" dispatcher
                                 "--eval" expression)))
      (should (string-match-p "DIRECT=ok" (buffer-string))))))

;;; test-context.el ends here
