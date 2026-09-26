;;; review-screen-regression.el --- M10 slice 1a: the review screen -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for slice 1a of the editor-mode/agent-mode decision
;; (docs/decisions/accepted/2026-09-24-editor-mode-agent-mode-and-review-screen.md):
;; the review screen that shows, one buffer at a time, how an unsaved buffer
;; a Run edited differs from its file, with `s' to save, `n' to leave it
;; unsaved and see the next, and `q' to stop.  It is the screen the attended
;; acceptance runs used (`ca-reviewer-saves-p'), brought into the product and
;; colored: `diff-mode' always, `diffs-minor-mode' when diffs.el is present.
;; It appears by itself when a Run leaves unsaved buffers, without taking
;; focus (since 2026-09-25 it takes the cursor); `/changes' and\n;; `superchat-changes-diff' open it selected.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets, and needs no scratch
;; files.  All model calls are scripted.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/review-screen-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^review/")'
;;
;; Set DIFFS_EL_DIR to a diffs.el checkout to also run the diffs.el renderer
;; test; without it that test is skipped.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Review screen regression tests are offline; unset SMOKE_REAL"))

(when-let* ((dir (getenv "DIFFS_EL_DIR")))
  (add-to-list 'load-path (expand-file-name dir)))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;; Declared so a `let' binding below is dynamic even before the option exists.
(defvar superchat-changes-review-after-run)

(defconst review--buffer-name "*superchat review*"
  "The review screen's buffer name, as the decision record describes it.")

(defmacro review--with-dir (dir &rest body)
  "Bind DIR to a fresh canonical directory; kill the buffers visiting it after."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "review-" t)))))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,dir (file-truename file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (when (get-buffer review--buffer-name)
         (kill-buffer review--buffer-name))
       (delete-directory ,dir t))))

(defun review--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent))
  file)

(defun review--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro review--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-approval-default-policy 'ask)
         (superchat-sandbox-default-mode 'edit-buffers)
         (superchat-llm-tool-names
          (append superchat-llm-default-tool-names '("append_to_buffer")))
         (superchat-llm-tools-list nil))
     (unwind-protect (smoke--with-fixture ,script ,@body)
       (setq superchat-llm-tools-list nil))))

(defun review--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun review--open-round (file)
  `(:tool-uses (("open_file" (path . ,file)))))

(defun review--edit-round (buffer-name old new)
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . ,old) (new_string . ,new)))))

(defun review--two-file-script (a b)
  "Open and edit files A and B, then answer."
  (list (review--open-round a)
        (review--edit-round (file-name-nondirectory a) "one" "ONE")
        (review--open-round b)
        (review--edit-round (file-name-nondirectory b) "two" "TWO")
        '(:text "done")))

(defun review--buffer ()
  (get-buffer review--buffer-name))

(defun review--shown ()
  "The window showing the review screen, once it appears."
  (and (review--buffer) (get-buffer-window review--buffer-name t)))

(defun review--text ()
  (with-current-buffer (review--buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun review--press (key)
  "Run the command KEY is bound to in the review buffer."
  (with-current-buffer (review--buffer)
    (let ((command (key-binding (kbd key))))
      (should (commandp command))
      (call-interactively command))))

(defun review--line-face (text)
  "The faces on the review line that starts with TEXT, after fontification."
  (with-current-buffer (review--buffer)
    (font-lock-ensure)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (should (re-search-forward (concat "^" (regexp-quote text)) nil t)))
      (let ((pos (line-beginning-position)) faces)
        (dolist (prop '(face font-lock-face))
          (let ((v (get-text-property (1+ pos) prop)))
            (when v (push v faces))))
        (flatten-tree faces)))))

(defmacro review--after-two-file-run (bindings &rest body)
  "Run the two-file script from a chat View; BINDINGS name dir, a, b, view.
BODY runs once the Run is terminal, with `selected' bound to the window that
was selected when the message was sent."
  (declare (indent 1) (debug t))
  (let ((dir (nth 0 bindings)) (a (nth 1 bindings)) (b (nth 2 bindings))
        (view (nth 3 bindings)))
    `(review--with-dir ,dir
       (let* ((,a (review--write (expand-file-name "a.txt" ,dir) "one\n"))
              (,b (review--write (expand-file-name "b.txt" ,dir) "two\n")))
         (review--fixture (review--two-file-script ,a ,b)
           (let* ((,view (smoke--open-chat))
                  (selected (selected-window)))
             (ignore selected)
             (review--settle (smoke--send ,view "edit both"))
             ,@body))))))

;;;; The screen appears by itself and the cursor goes to it

(defmacro review--typing-ahead (key &rest body)
  "Run BODY with KEY typed just before the review screen appears after a Run."
  (declare (indent 1) (debug t))
  `(let ((type-ahead (lambda (&rest _) (setq unread-command-events (list ,key)))))
     (advice-add 'superchat-changes--review-after-run :before type-ahead)
     (unwind-protect (progn ,@body)
       (advice-remove 'superchat-changes--review-after-run type-ahead))))

(ert-deftest review/after-run/takes-the-cursor-and-drops-typed-ahead-keys ()
  "A Run that leaves unsaved buffers shows the review screen in another
window and moves the cursor into it, with no key needed (M10 record, amended
2026-09-25); a key typed just before it appeared saves nothing."
  (review--typing-ahead ?s
    (review--after-two-file-run (dir a b view)
      (should (smoke--wait #'review--shown 5))
      (should (eq (window-buffer (selected-window)) (review--buffer)))
      (should-not (eq selected (selected-window)))
      (should-not unread-command-events)
      (should (string-match-p "1/2" (review--text)))
      (should (buffer-modified-p (get-file-buffer a)))
      (should (equal "one\n" (review--disk a))))))

(ert-deftest review/after-run/waits-while-the-minibuffer-is-in-use ()
  "When the screen appears while the minibuffer is in use, it is shown but
not selected; the cursor goes to it once the minibuffer is done."
  (let* ((busy t)
         (real (symbol-function 'active-minibuffer-window)))
    (cl-letf (((symbol-function 'active-minibuffer-window)
               (lambda () (if busy (minibuffer-window) (funcall real)))))
      (review--after-two-file-run (dir a b view)
        (should (smoke--wait #'review--shown 5))
        (should (eq selected (selected-window)))
        (setq busy nil)
        (run-hooks 'minibuffer-exit-hook)
        (should (smoke--wait (lambda () (eq (window-buffer (selected-window))
                                            (review--buffer)))
                             5))))))

(ert-deftest review/after-run/q-gives-the-cursor-back ()
  "When the review ends, the cursor is back in the window it was in before
the screen appeared."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (should (eq (window-buffer (selected-window)) (review--buffer)))
    (review--press "q")
    (should-not (review--shown))
    (should (eq selected (selected-window)))))

(ert-deftest review/option/off-means-no-screen-but-the-notice-stays ()
  "Guard (green before and after): with the option off, no review screen
appears by itself, and the Run-end notice is still given."
  (let ((superchat-changes-review-after-run nil)
        (noticed nil))
    (cl-letf (((symbol-function 'superchat-changes--announce)
               (lambda (text) (push text noticed))))
      (review--after-two-file-run (dir a b view)
        (should (smoke--wait (lambda () noticed) 5))
        (sit-for 0.1)
        (should-not (review--shown))))))

;;;; What the screen shows

(ert-deftest review/content/one-buffer-at-a-time-in-color ()
  "The screen names its position and buffer, and shows the file-to-buffer
diff of that one buffer with the standard diff colors."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (let ((text (review--text)))
      (should (string-match-p "1/2" text))
      (should (string-match-p "a\\.txt" text))
      (should-not (string-match-p "b\\.txt" text))
      (should (string-match-p "^-one$" text))
      (should (string-match-p "^\\+ONE$" text)))
    (should (with-current-buffer (review--buffer) (derived-mode-p 'diff-mode)))
    (should (memq 'diff-removed (review--line-face "-one")))
    (should (memq 'diff-added (review--line-face "+ONE")))))

;;;; The keys

(ert-deftest review/keys/s-saves-this-buffer-and-shows-the-next ()
  "`s' saves the buffer on screen -- the user's own operation -- and moves on."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (review--press "s")
    (should (equal "ONE\n" (review--disk a)))
    (should-not (buffer-modified-p (get-file-buffer a)))
    (should (string-match-p "2/2" (review--text)))
    (should (string-match-p "b\\.txt" (review--text)))
    (should (buffer-modified-p (get-file-buffer b)))
    (should (equal "two\n" (review--disk b)))))

(ert-deftest review/keys/n-leaves-the-buffer-unsaved ()
  "`n' leaves the buffer unsaved; after the last one the screen closes."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (review--press "n")
    (should (string-match-p "2/2" (review--text)))
    (review--press "n")
    (should-not (review--shown))
    (should (buffer-modified-p (get-file-buffer a)))
    (should (buffer-modified-p (get-file-buffer b)))
    (should (equal "one\n" (review--disk a)))
    (should (equal "two\n" (review--disk b)))))

(ert-deftest review/keys/q-stops-and-saves-nothing ()
  "`q' ends the review; every buffer stays as it was, unsaved."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (review--press "q")
    (should-not (review--shown))
    (should (buffer-modified-p (get-file-buffer a)))
    (should (buffer-modified-p (get-file-buffer b)))
    (should (equal "one\n" (review--disk a)))
    (should (equal "two\n" (review--disk b)))))

(ert-deftest review/keys/source-changing-keys-are-inert ()
  "Keys that would change the source buffer or start a per-block flow do
nothing on the review screen: diff-mode's hunk application, diffs.el's
accept/reject/apply and annotation keys."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (let ((before (with-current-buffer (get-file-buffer a) (buffer-string))))
      (with-current-buffer (review--buffer)
        (dolist (key '("a" "A" "R" "U" "C-c C-a" "C-c C-c"))
          (let ((command (key-binding (kbd key))))
            (should-not (memq command '(diff-apply-hunk diff-apply-buffer
                                        diffs-review-accept-change diffs-review-reject-change
                                        diffs-review-reset-change diffs-review-apply-decisions
                                        diffs-review-add-annotation))))))
      (dolist (key '("a" "C-c C-a"))
        (review--press key))
      (should (equal before (with-current-buffer (get-file-buffer a) (buffer-string))))
      (should (buffer-modified-p (get-file-buffer a)))
      (should (equal "one\n" (review--disk a))))))

(ert-deftest review/keys/s-on-a-buffer-that-is-gone-moves-on-without-saving ()
  "When the buffer on screen was killed after the screen appeared, `s' saves
nothing, signals nothing, and shows the next buffer."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (let ((buffer-a (get-file-buffer a)))
      (with-current-buffer buffer-a (set-buffer-modified-p nil))
      (kill-buffer buffer-a))
    (review--press "s")
    (should (equal "one\n" (review--disk a)))
    (should (string-match-p "2/2" (review--text)))
    (should (string-match-p "b\\.txt" (review--text)))))

(ert-deftest review/keys/g-redraws-from-the-buffer-as-it-is-now ()
  "`g' redraws the same entry from the buffer's current text: the screen
keeps its place across the redraw, and a later change shows up."
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (with-current-buffer (get-file-buffer a)
      (goto-char (point-min))
      (search-forward "ONE")
      (replace-match "ONE-AGAIN" t t))
    (review--press "g")
    (should (string-match-p "1/2" (review--text)))
    (should (string-match-p "^\\+ONE-AGAIN$" (review--text)))
    (review--press "n")
    (should (string-match-p "2/2" (review--text)))))

;;;; Which buffers are reviewed

(ert-deftest review/candidates/a-buffer-without-a-file-is-listed-not-reviewed ()
  "A buffer that visits no file has nothing to save to: it is not on the
screen, and `/changes' still lists it."
  (review--with-dir dir
    (let ((a (review--write (expand-file-name "a.txt" dir) "one\n"))
          (notes (generate-new-buffer "review-notes")))
      (with-current-buffer notes (insert "alpha\n"))
      (review--fixture
          (list (review--open-round a)
                (review--edit-round "a.txt" "one" "ONE")
                (review--edit-round (buffer-name notes) "alpha" "beta")
                '(:text "done"))
        (let ((view (smoke--open-chat)))
          (review--settle (smoke--send view "edit"))
          (should (smoke--wait #'review--shown 5))
          (should (string-match-p "1/1" (review--text)))
          (should-not (string-match-p "review-notes" (review--text)))
          (with-current-buffer view
            (let ((content (plist-get (superchat--cmd-changes "changes" "" nil nil nil)
                                      :content)))
              (should (string-match-p "review-notes" content))
              (should (string-match-p "a\\.txt" content)))))))))

(ert-deftest review/candidates/a-new-file-is-shown-whole-and-saving-creates-it ()
  "A buffer visiting a file that is not on disk yet is shown as one whole
addition; `s' writes it."
  (review--with-dir dir
    (let ((new (expand-file-name "new.txt" dir)))
      (review--fixture
          (list (review--open-round new)
                '(:tool-uses (("append_to_buffer" (buffer . "new.txt") (text . "hello\n"))))
                '(:text "done"))
        (let ((view (smoke--open-chat)))
          (review--settle (smoke--send view "create"))
          (should (smoke--wait #'review--shown 5))
          (should (string-match-p "^\\+hello$" (review--text)))
          (should-not (file-exists-p new))
          (review--press "s")
          (should (equal "hello\n" (review--disk new))))))))

;;;; Opening it on purpose

(ert-deftest review/entry/changes-opens-the-screen-selected-and-drops-typed-ahead-keys ()
  "`/changes' opens the review screen and selects it; a key typed before it
opened does not save anything."
  (let ((superchat-changes-review-after-run nil))
    (review--after-two-file-run (dir a b view)
      ;; A key typed ahead, before the screen opened.
      (setq unread-command-events (list ?s))
      (with-current-buffer view
        (superchat--cmd-changes "changes" "" nil nil nil))
      (should (smoke--wait #'review--shown 5))
      (should (eq (window-buffer (selected-window)) (review--buffer)))
      (should-not unread-command-events)
      (should (string-match-p "1/2" (review--text)))
      (should (buffer-modified-p (get-file-buffer a)))
      (should (equal "one\n" (review--disk a))))))

(ert-deftest review/entry/changes-diff-reviews-that-one-buffer ()
  "`superchat-changes-diff' -- which inline act's `d' calls -- opens the
review screen for exactly that buffer."
  (let ((superchat-changes-review-after-run nil))
    (review--after-two-file-run (dir a b view)
      (superchat-changes-diff (get-file-buffer b))
      (should (smoke--wait #'review--shown 5))
      (should (string-match-p "1/1" (review--text)))
      (should (string-match-p "b\\.txt" (review--text)))
      (should-not (string-match-p "a\\.txt" (review--text))))))

;;;; diffs.el renderer

(ert-deftest review/renderer/diffs-minor-mode-when-diffs-el-is-present ()
  "With diffs.el on the load-path, the same diff is drawn by
`diffs-minor-mode': a line-number gutter, and the diff text unchanged."
  (skip-unless (require 'diffs nil t))
  (review--after-two-file-run (dir a b view)
    (should (smoke--wait #'review--shown 5))
    (with-current-buffer (review--buffer)
      (should (bound-and-true-p diffs-minor-mode))
      (font-lock-ensure)
      (should (string-match-p "^\\+ONE$" (buffer-substring-no-properties
                                          (point-min) (point-max))))
      (goto-char (point-min))
      (should (re-search-forward "^\\+ONE$" nil t))
      (let ((pos (line-beginning-position)))
        (should (or (get-text-property pos 'line-prefix)
                    (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                             (overlays-at pos))))))))

(provide 'review-screen-regression)

;;; review-screen-regression.el ends here
