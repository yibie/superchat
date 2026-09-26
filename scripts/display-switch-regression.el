;;; display-switch-regression.el --- one key between editor and agent mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for M10 slice 1c
;; (docs/decisions/accepted/2026-09-24-editor-mode-agent-mode-and-review-screen.md,
;; "切换键"): in the chat View `C-c C-w' moves a conversation shown beside the
;; file into its own tab, filling it, and back into the editing tab's side
;; window, with the unsent input kept; within one Emacs session the next
;; opening uses the last mode, `superchat-display-style' giving only the first.
;;
;; It reuses the editor-mode suite's fixture (one tab, one window on a file).
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/display-switch-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^display-switch/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "editor-mode-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Declared so a `let' binding below is dynamic even before it exists.
(defvar superchat--display-style-current)

(defmacro display-switch--fixture (bindings &rest body)
  "Run BODY in editor mode with the chat of a new Session open beside a file.
BINDINGS is (CODE VIEW SESSION): the file's buffer, the chat View and its
Session id.  The last-used mode starts unset."
  (declare (indent 1) (debug t))
  (let ((code (nth 0 bindings)) (view (nth 1 bindings)) (session (nth 2 bindings)))
    `(let ((superchat-display-style 'editor)
           (superchat--display-style-current nil))
       (editor-mode--fixture nil (dir ,code code-window)
         (ignore dir code-window)
         (let* ((,view (with-current-buffer ,code (superchat-new-session)))
                (,session (buffer-local-value 'superchat--session-id ,view)))
           (ignore ,session)
           (should (editor-mode--side-p (get-buffer-window ,view)))
           ,@body)))))

(defun display-switch--toggle (buffer)
  "Press the switch key in BUFFER, as the user would."
  (with-current-buffer buffer
    (call-interactively (key-binding (kbd "C-c C-w")))))

(defun display-switch--input (view)
  "The unsent text in VIEW's input area."
  (with-current-buffer view
    (buffer-substring-no-properties superchat--prompt-start (point-max))))

(ert-deftest display-switch/the-key-is-in-the-chat ()
  "`C-c C-w' in a chat View is the switch."
  (display-switch--fixture (code view session)
    (with-current-buffer view
      (should (eq 'superchat-toggle-display-style (key-binding (kbd "C-c C-w")))))))

(ert-deftest display-switch/to-the-own-tab-and-back-keeping-the-input ()
  "From beside the file the key moves the chat into its own tab, filling it;
pressed again it returns to the editing tab, the chat beside the file and
selected, and the unsent input is still there."
  (display-switch--fixture (code view session)
    (let ((editing-tab (tab-bar--current-tab-index)))
      (with-current-buffer view
        (goto-char (point-max))
        (insert "UNSENT-WORDS"))
      (display-switch--toggle view)
      (should (equal session (editor-mode--tab-session)))
      (should (eq view (window-buffer (selected-window))))
      (should-not (editor-mode--side-p (selected-window)))
      (should (equal (list (selected-window)) (editor-mode--windows)))
      (display-switch--toggle view)
      (should (= editing-tab (tab-bar--current-tab-index)))
      (should-not (editor-mode--tab-session))
      (should (get-buffer-window code))
      (should (editor-mode--side-p (selected-window)))
      (should (eq view (window-buffer (selected-window))))
      (should (string-suffix-p "UNSENT-WORDS" (display-switch--input view))))))

(ert-deftest display-switch/opening-again-uses-the-last-mode ()
  "Within one Emacs session the next opening uses the mode last switched to,
whatever `superchat-display-style' says; switching back restores the other."
  (display-switch--fixture (code view session)
    (let ((editing-tab (tab-bar--current-tab-index)))
      (display-switch--toggle view)
      (tab-bar-select-tab (1+ editing-tab))
      (with-current-buffer code (superchat-open-session session))
      (should (equal session (editor-mode--tab-session)))
      (should-not (editor-mode--side-p (selected-window)))
      (display-switch--toggle view)
      (should (editor-mode--side-p (selected-window)))
      (with-current-buffer code (superchat-open-session session))
      (should-not (editor-mode--tab-session))
      (should (editor-mode--side-p (get-buffer-window view))))))

(ert-deftest display-switch/back-to-the-tab-it-came-from ()
  "Back from agent mode, the chat goes to the tab the switch left, even when
another tab was used more recently."
  (display-switch--fixture (code view session)
    (let ((editing-tab (tab-bar--current-tab-index)))
      (display-switch--toggle view)
      (tab-bar-new-tab)
      (switch-to-buffer (get-scratch-buffer-create))
      (let ((other-tab (tab-bar--current-tab-index)))
        (superchat-editor-workspace--select-session-tab session)
        (display-switch--toggle view)
        (should-not (= other-tab (tab-bar--current-tab-index)))
        (should (= editing-tab (tab-bar--current-tab-index)))
        (should (get-buffer-window code))
        (should (eq view (window-buffer (selected-window))))))))

(ert-deftest display-switch/back-when-the-editing-tab-was-closed ()
  "If the editing tab was closed meanwhile, the chat goes beside a file in a
tab that is not its own: here a new one, since its own tab is the only one."
  (display-switch--fixture (code view session)
    (let ((editing-tab (tab-bar--current-tab-index)))
      (display-switch--toggle view)
      (let ((tab-bar-tab-prevent-close-functions nil))
        (tab-bar-close-tab (1+ editing-tab)))
      (should (= 1 (length (tab-bar-tabs))))
      (display-switch--toggle view)
      (should (= 2 (length (tab-bar-tabs))))
      (should-not (editor-mode--tab-session))
      (should (editor-mode--side-p (selected-window)))
      (should (eq view (window-buffer (selected-window))))
      (should (get-buffer-window code)))))

(ert-deftest display-switch/from-the-file-window-by-name ()
  "Called by name from the file's window, the switch acts on the chat shown
beside it."
  (display-switch--fixture (code view session)
    (select-window (get-buffer-window code))
    (with-current-buffer code
      (call-interactively #'superchat-toggle-display-style))
    (should (equal session (editor-mode--tab-session)))
    (should (eq view (window-buffer (selected-window))))))

(provide 'display-switch-regression)

;;; display-switch-regression.el ends here
