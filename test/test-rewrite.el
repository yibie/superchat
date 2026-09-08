;;; test-rewrite.el --- ERT tests for superchat-rewrite -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for region rewrite (code block extraction) and context-sending
;; helpers.  No LLM calls — pure logic and buffer manipulation only.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'seq)
(require 'superchat-core)
(require 'superchat-tape)
(require 'superchat-rewrite)
(require 'superchat nil t)
(require 'journey-fixture
         (expand-file-name "journey-fixture" (file-name-directory load-file-name)))

(defun rewrite-test--assert-no-buffer-residue (before-buffers)
  "Assert the live buffer list matches BEFORE-BUFFERS.
Ignores the journey fixture's own transport buffers (created for the LLM
round-trip itself, present regardless of what the rewrite path under
test does) and Emacs's own `*Warnings*' buffer (lazily created the first
time any code in the process calls `display-warning', e.g. the fixture's
own \"no model predefined\" notice — a process-wide side effect with
nothing to do with the rewrite/confirm/preview path), so only buffers
that path itself created or destroyed can make this fail — e.g. the S16
preview help buffer, which is exactly the leak this check exists to
catch."
  (let* ((ignored (cons (get-buffer "*Warnings*")
                        (journey-fixture-state-transport-buffers
                         journey-fixture--state)))
         (now-diff (cl-set-difference (buffer-list) ignored :test #'eq))
         (before-diff (cl-set-difference before-buffers ignored :test #'eq)))
    (should-not (cl-set-exclusive-or now-diff before-diff :test #'eq))))

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

;; ═══════════════════════════════════════════════════════════
;; apply-code-block-at-point uses superchat-change-set-apply
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/apply-code-block-uses-change-set-and-records-tape ()
  "With a known target region, apply-code-block-at-point previews a
diff, applies the replacement via `superchat-change-set-apply' after
confirmation, and records `edit.applied' tagged with the calling
buffer's session."
  (let* ((session-id (format "rewrite-apply-%s" (gensym)))
         (target (generate-new-buffer " *rewrite-apply-target*"))
         (reply (generate-new-buffer "*rewrite-apply-reply*"))
         (superchat-rewrite--last-source-buffer nil)
         (superchat-rewrite--last-source-region nil))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "before\nOLD-TEXT\nafter\n")
            (superchat-rewrite--record-source (buffer-name target) 8 16))
          (with-current-buffer reply
            (org-mode)
            (setq-local superchat--session-id session-id)
            (insert "#+begin_src text\nNEW-TEXT\n#+end_src\n")
            (goto-char (point-min))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (superchat-rewrite-apply-code-block-at-point)))
          (should (equal (with-current-buffer target (buffer-string))
                         "before\nNEW-TEXT\nafter\n"))
          (let* ((events (superchat-tape-replay-events session-id))
                 (event (car events))
                 (payload (plist-get event :payload)))
            (should (= (length events) 1))
            (should (equal (plist-get event :type) "edit.applied"))
            (should (equal (plist-get payload :buffer) (buffer-name target)))
            (should (equal (plist-get payload :removed) 8))
            (should (equal (plist-get payload :added) 8))
            (should (equal (plist-get payload :source) "chat-apply-code-block"))))
      (kill-buffer target)
      (kill-buffer reply))))

(ert-deftest rewrite/apply-code-block-without-region-appends ()
  "Without a known target region, apply-code-block-at-point appends the
code block via `superchat-change-set-apply' with no confirmation prompt."
  (let* ((session-id (format "rewrite-append-%s" (gensym)))
         (target (generate-new-buffer " *rewrite-append-target*"))
         (reply (generate-new-buffer "*rewrite-append-reply*"))
         (superchat-rewrite--last-source-buffer (buffer-name target))
         (superchat-rewrite--last-source-region nil))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "existing content"))
          (with-current-buffer reply
            (org-mode)
            (setq-local superchat--session-id session-id)
            (insert "#+begin_src text\nAPPENDED\n#+end_src\n")
            (goto-char (point-min))
            (superchat-rewrite-apply-code-block-at-point))
          (should (equal (with-current-buffer target (buffer-string))
                         "existing content\nAPPENDED\n"))
          (let* ((events (superchat-tape-replay-events session-id))
                 (event (car events))
                 (payload (plist-get event :payload)))
            (should (= (length events) 1))
            (should (equal (plist-get event :type) "edit.applied"))
            (should (equal (plist-get payload :removed) 0))
            (should (equal (plist-get payload :added) 10))))
      (kill-buffer target)
      (kill-buffer reply))))

;; ═══════════════════════════════════════════════════════════
;; Region rewrite confirmation (S16-fix-1)
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/apply-code-block-preview-does-not-destroy-pre-existing-same-name-buffer ()
  "Known-region `C-c C-a' previews through the exact same
`superchat-rewrite--preview-and-confirm' seam as region rewrite; a buffer
that already has the preview's exact name survives unchanged whether the
user answers yes, no, or quits."
  (dolist (answer '(yes no quit))
    (let* ((session-id (format "rewrite-apply-owner-%s-%s" answer (gensym)))
           (target (generate-new-buffer
                    (format " *rewrite-apply-owner-target-%s*" answer)))
           (reply (generate-new-buffer
                   (format "*rewrite-apply-owner-reply-%s*" answer)))
           (preview-name (format "*Superchat Rewrite Preview <%s>*" (buffer-name target)))
           (collider (generate-new-buffer preview-name))
           (collider-text "PRE-EXISTING USER DATA")
           (superchat-rewrite--last-source-buffer nil)
           (superchat-rewrite--last-source-region nil)
           buffers-before)
      (unwind-protect
          (progn
            (with-current-buffer collider (insert collider-text))
            (with-current-buffer target
              (insert "before\nOLD-TEXT\nafter\n")
              (superchat-rewrite--record-source (buffer-name target) 8 16))
            (with-current-buffer reply
              (org-mode)
              (setq-local superchat--session-id session-id)
              (insert "#+begin_src text\nNEW-TEXT\n#+end_src\n")
              (goto-char (point-min)))
            (setq buffers-before (buffer-list))
            (with-current-buffer reply
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _)
                           (pcase answer
                             ('yes t) ('no nil) ('quit (signal 'quit nil))))))
                (condition-case nil
                    (superchat-rewrite-apply-code-block-at-point)
                  (quit nil))))
            (should (buffer-live-p collider))
            (should (eq collider (get-buffer preview-name)))
            (should (equal collider-text (with-current-buffer collider (buffer-string))))
            (let* ((ignored (delq nil (list (get-buffer "*Warnings*"))))
                   (now-diff (cl-set-difference (buffer-list) ignored :test #'eq))
                   (before-diff (cl-set-difference buffers-before ignored :test #'eq)))
              (should-not (cl-set-exclusive-or now-diff before-diff :test #'eq))))
        (when (buffer-live-p target) (kill-buffer target))
        (when (buffer-live-p reply) (kill-buffer reply))
        (when (buffer-live-p collider) (kill-buffer collider))))))

;;; test-rewrite.el ends here

;; ═══════════════════════════════════════════════════════════
;; S17b: conflict-marker builder and overlay navigation order
;; ═══════════════════════════════════════════════════════════

(ert-deftest rewrite/conflict-text-has-three-sections ()
  "The merge text carries original / ======= / rewrite sections, each
newline-terminated exactly once."
  (should (equal "<<<<<<< original\nold\n=======\nnew\n>>>>>>> rewrite\n"
                 (superchat-rewrite--conflict-text "old" "new")))
  (should (equal "<<<<<<< original\nold\n=======\nnew\n>>>>>>> rewrite\n"
                 (superchat-rewrite--conflict-text "old\n" "new\n"))))

(ert-deftest rewrite/navigation-visits-overlays-in-buffer-order-and-wraps ()
  "`superchat-rewrite-next' / `-previous' move through previews by position."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let* ((state (lambda () (list :status 'ready :original "x" :text "y"
                                   :session-id "s" :lang "text-mode")))
           (o3 (superchat-rewrite--make-overlay 9 14 (funcall state)))
           (o1 (superchat-rewrite--make-overlay 1 4 (funcall state)))
           (o2 (superchat-rewrite--make-overlay 5 8 (funcall state))))
      (should (equal (list o1 o2 o3) (superchat-rewrite--sorted-overlays)))
      (goto-char (point-min))
      (should (eq o2 (superchat-rewrite-next)))
      (should (eq o3 (superchat-rewrite-next)))
      (should (eq o1 (superchat-rewrite-next)))   ; wraps
      (should (eq o3 (superchat-rewrite-previous))) ; wraps back
      (should (eq o2 (superchat-rewrite-previous)))
      (should (= 3 (length (superchat-rewrite-overlays))))
      (superchat-rewrite-reject-all)
      (should-not (superchat-rewrite-overlays)))))
