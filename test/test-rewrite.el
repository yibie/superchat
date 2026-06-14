;;; test-rewrite.el --- ERT tests for superchat-rewrite -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for region rewrite (code block extraction) and context-sending
;; helpers.  No LLM calls — pure logic and buffer manipulation only.

;;; Code:

(require 'ert)
(require 'superchat-core)
(require 'superchat-rewrite)
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; Code block extraction
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/extract-code-block-with-matching-tag ()
  "Extract a fenced code block whose language tag matches LANG-TAG."
  (let* ((text "Here is the code:\n```emacs-lisp\n(defun foo ()\n 42)\n```\nDone.")
         (result (superchat-rewrite--extract-code-block text "emacs-lisp")))
    (should result)
    (should (string-match-p "(defun foo" result))
    (should-not (string-match-p "```" result))))

(ert-deftest rewrite/extract-code-block-falls-back-to-any-fence ()
  "When the matching tag isn't found, fall back to the first fenced block."
  (let* ((text "```python\ndef foo():\n    return 42\n```\nExtra text.")
         (result (superchat-rewrite--extract-code-block text "emacs-lisp")))
    (should result)
    (should (string-match-p "def foo():" result))
    (should-not (string-match-p "```" result))))

(ert-deftest rewrite/extract-code-block-no-fence-returns-nil ()
  "Return nil when there is no fenced code block."
  (should-not (superchat-rewrite--extract-code-block "Just plain text." "emacs-lisp")))

(ert-deftest rewrite/extract-code-block-nil-input ()
  "Return nil when input is nil."
  (should-not (superchat-rewrite--extract-code-block nil "emacs-lisp")))

(ert-deftest rewrite/extract-code-block-first-matching-over-fallback ()
  "Prefer the matching tag even when a fallback block appears first."
  (let* ((text "```python\nprint('first')\n```\n```emacs-lisp\n(message \"second\")\n```")
         (result (superchat-rewrite--extract-code-block text "emacs-lisp")))
    (should result)
    (should (string-match-p "message" result))
    (should-not (string-match-p "print" result))))

;; ═══════════════════════════════════════════════════════════
;; Defun bounds detection
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/traditional-defun-bounds-in-elisp ()
  "Find enclosing defun bounds in Emacs Lisp buffer via traditional navigation."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; A comment\n\n(defun my-test-fn (x)\n  \"A docstring.\"\n  (+ x 1))\n\n(defun another-fn (y)\n  (* y 2))\n")
    (goto-char (point-min))
    ;; Navigate to inside my-test-fn (line 3, after the leading comment + blank)
    (re-search-forward "(defun my-test-fn")
    (let ((bounds (superchat--traditional-defun-bounds)))
      (should bounds)
      ;; bounds should span the whole defun
      (should (car bounds))
      (should (cdr bounds))
      (should (< (car bounds) (cdr bounds))))))

(ert-deftest rewrite/traditional-defun-bounds-at-top-level ()
  "Traditional bounds in a buffer with just comments returns something non-nil.
In `emacs-lisp-mode', `beginning-of-defun' goes to the top of the
buffer when there are no defuns — it does NOT signal an error."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; Just comments\n;; No defuns here\n")
    (goto-char (point-max))
    (let ((bounds (superchat--traditional-defun-bounds)))
      ;; `beginning-of-defun' finds point-min, `end-of-defun' finds
      ;; whatever it can; the result is well-defined but not nil.
      ;; The behaviour is acceptable — this is a best-effort fallback.
      (should bounds))))

(ert-deftest rewrite/get-enclosing-defun-bounds-fallback ()
  "Falls back to traditional bounds when tree-sitter is unavailable."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test-fn ()\n  'hello)\n")
    (goto-char 5)  ;; inside the defun
    (let ((bounds (superchat--get-enclosing-defun-bounds)))
      (should bounds)
      (should (<= (car bounds) 1))
      (should (>= (cdr bounds) (point-max))))))

;; ═══════════════════════════════════════════════════════════
;; Completion: # file path (capf)
;; ═══════════════════════════════════════════════════════════

(ert-deftest completion/hash-file-capf-at-point ()
  "completion-at-point returns bounds & table for # prefix in prompt area."
  (let ((superchat-buffer-name "*test-superchat-completion*"))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (org-mode)
      (erase-buffer)
      (insert "Before prompt\n")
      (setq-local superchat--prompt-start (point-marker))
      (insert "#/tmp/tes")
      (let ((result (superchat--completion-at-point)))
        (should result)
        ;; Bounds: start after #, end at point
        (should (= (nth 0 result) (1+ (marker-position superchat--prompt-start))))
        (should (= (nth 1 result) (point)))
        ;; Collection is a completion table
        (should (nth 2 result))))
    (kill-buffer superchat-buffer-name)))

(ert-deftest completion/hash-empty-returns-table ()
  "Bare # returns a completion table for current directory."
  (let ((superchat-buffer-name "*test-superchat-completion-2*"))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (org-mode)
      (erase-buffer)
      (setq-local superchat--prompt-start (point-marker))
      (insert "#")
      (let ((result (superchat--completion-at-point)))
        (should result)
        (should (= (nth 0 result) (1+ (marker-position superchat--prompt-start))))
        (should (= (nth 1 result) (point)))
        (should (nth 2 result))))
    (kill-buffer superchat-buffer-name)))

(ert-deftest completion/hash-only-triggers-in-prompt ()
  "Hash completion does not fire before `superchat--prompt-start'."
  (let ((superchat-buffer-name "*test-superchat-completion-3*"))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (org-mode)
      (erase-buffer)
      (insert "#not-a-prompt")
      (setq-local superchat--prompt-start (point-marker))
      (insert "Some user text after prompt\n")
      (goto-char 5) ;; inside "#not-a-prompt"
      (let ((result (superchat--completion-at-point)))
        (should-not result)))
    (kill-buffer superchat-buffer-name)))

;; ═══════════════════════════════════════════════════════════
;; Context insertion helper
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/insert-context-into-chat ()
  "Context insertion creates buffer and inserts text at prompt.
Uses an explicit prompt marker to avoid depending on superchat-mode
initialization (which requires corfu/company in interactive Emacs)."
  (let ((superchat-buffer-name "*test-superchat-context*"))
    (unwind-protect
        (progn
          ;; Pre-create the buffer with a prompt marker so the helper
          ;; skips its own setup path.
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (org-mode)
            (erase-buffer)
            (setq-local superchat--prompt-start (point-marker)))
          ;; Now call the helper — it should see the existing marker.
          (superchat--insert-context-into-chat
           "```elisp\n(+ 1 2)\n```\n"
           "What does this do?"
           "test.el" "emacs-lisp-mode")
          (with-current-buffer superchat-buffer-name
            (should superchat--prompt-start)
            (goto-char (point-min))
            (should (search-forward "Context from buffer" nil t))
            (should (search-forward "(+ 1 2)" nil t))
            (should (search-forward "What does this do?" nil t))))
      (ignore-errors (kill-buffer superchat-buffer-name)))))

;; ═══════════════════════════════════════════════════════════
;; Multi-block extraction (v1 extension)
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/extract-all-code-blocks-multiple ()
  "Extract all fenced code blocks from text."
  (let* ((text "```emacs-lisp\n(defun a ())\n```\nText\n```emacs-lisp\n(defun b ())\n```")
         (blocks (superchat-rewrite--extract-all-code-blocks text "emacs-lisp")))
    (should blocks)
    (should (= (length blocks) 2))
    (should (string-match-p "(defun a" (nth 0 blocks)))
    (should (string-match-p "(defun b" (nth 1 blocks)))))

(ert-deftest rewrite/extract-all-code-blocks-single ()
  "Single block returns a one-element list."
  (let* ((text "```python\nprint(1)\n```")
         (blocks (superchat-rewrite--extract-all-code-blocks text "python")))
    (should blocks)
    (should (= (length blocks) 1))
    (should (string-match-p "print" (car blocks)))))

(ert-deftest rewrite/extract-all-code-blocks-none ()
  "No blocks returns nil."
  (should-not (superchat-rewrite--extract-all-code-blocks "plain text" "python")))

(ert-deftest rewrite/extract-all-code-blocks-nil-input ()
  "Nil input returns nil."
  (should-not (superchat-rewrite--extract-all-code-blocks nil "python")))

;; ═══════════════════════════════════════════════════════════
;; Org source block at point (apply-code-block-at-point)
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/code-block-at-point-finds-org-src ()
  "Find #+begin_src ... #+end_src block at point."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src emacs-lisp\n(message \"hello\")\n#+end_src\n")
    (goto-char 5)  ;; inside the block
    (let ((result (superchat-rewrite--code-block-at-point)))
      (should result)
      (should (equal (cdr result) "emacs-lisp"))
      (should (string-match-p "message" (car result))))))

(ert-deftest rewrite/code-block-at-point-returns-nil-outside ()
  "Return nil when point is not on a source block."
  (with-temp-buffer
    (org-mode)
    (insert "Plain text, no blocks here.\n")
    (let ((result (superchat-rewrite--code-block-at-point)))
      (should-not result))))

(ert-deftest rewrite/code-block-at-point-finds-from-body ()
  "Find block when point is inside the body (not on begin_src line)."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src python\nprint(42)\n#+end_src\n")
    (goto-char (point-min))
    (forward-line 1)  ;; on the print(42) line
    (let ((result (superchat-rewrite--code-block-at-point)))
      (should result)
      (should (equal (cdr result) "python"))
      (should (string-match-p "print(42)" (car result))))))

;; ═══════════════════════════════════════════════════════════
;; Source recording (for apply-code-block-at-point)
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/record-source-sets-last-buffer ()
  "Recording source updates the last-source-buffer and region."
  (let ((superchat-rewrite--last-source-buffer nil)
        (superchat-rewrite--last-source-region nil))
    (superchat-rewrite--record-source "test-buf.el" 10 50)
    (should (equal superchat-rewrite--last-source-buffer "test-buf.el"))
    (should (equal superchat-rewrite--last-source-region '(10 . 50)))))

;; ═══════════════════════════════════════════════════════════
;; Keybinding (Part A)
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/keymap-has-apply-binding ()
  "C-c C-a is bound to apply-code-block-at-point in superchat-mode-map."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-a"))
              'superchat-rewrite-apply-code-block-at-point)))

;;; test-rewrite.el ends here
