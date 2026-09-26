;;; changes-entry-regression.el --- the review screen by command from a file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for opening the review screen by command outside a chat
;; View (docs/decisions/implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md,
;; 审阅屏, "从代码 Buffer 用命令打开"; issue
;; .scratch/issues/2026-09-25-superchat-changes-outside-a-chat-view.md).
;; `M-x superchat-changes' and `superchat-changes-diff''s buffer prompt use
;; the current buffer's own conversation; from any other buffer, the one shown
;; in this tab's right side window, as the display switch finds it; with
;; neither, a `user-error' that says so, never a type error.
;;
;; It reuses the editor-mode suite's fixture (one tab, one window on a file).
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/changes-entry-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^changes-entry/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "editor-mode-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst changes-entry--review "*superchat review*")

(defmacro changes-entry--after-edit (bindings &rest body)
  "Run BODY once a Run from the chat beside the file left other.txt unsaved.
BINDINGS is (CODE CODE-WINDOW VIEW): the file's buffer and window, and the
chat View in the side window.  The file's window is selected when BODY runs.
The review screen does not appear by itself."
  (declare (indent 1) (debug t))
  (let ((code (nth 0 bindings)) (code-window (nth 1 bindings))
        (view (nth 2 bindings)))
    `(let ((superchat-changes-review-after-run nil))
       (editor-mode--fixture (editor-mode--edit-other-script dir) (dir ,code ,code-window)
         (let* ((,view (superchat--ensure-chat-view superchat-buffer-name))
                (chat (editor-mode--show-beside ,view)))
           (select-window chat)
           (smoke--send ,view "edit the other file")
           (should (editor-mode--settled))
           (should (buffer-modified-p
                    (get-file-buffer (expand-file-name "other.txt" dir))))
           (select-window ,code-window)
           (unwind-protect (progn ,@body)
             (when (get-buffer changes-entry--review)
               (kill-buffer changes-entry--review))))))))

(defun changes-entry--review-text ()
  "The review screen's text, once it is shown and selected."
  (should (smoke--wait (lambda () (get-buffer-window changes-entry--review)) 5))
  (should (eq (get-buffer changes-entry--review) (window-buffer (selected-window))))
  (with-current-buffer changes-entry--review
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest changes-entry/from-the-file-the-chat-beside-it ()
  "`M-x superchat-changes' in the file's window reviews what the conversation
shown beside it left unsaved."
  (changes-entry--after-edit (code code-window view)
    (with-current-buffer code
      (call-interactively #'superchat-changes))
    (let ((text (changes-entry--review-text)))
      (should (string-match-p "1/1" text))
      (should (string-match-p "other\\.txt" text)))))

(ert-deftest changes-entry/diff-offers-the-chat-beside-it ()
  "`superchat-changes-diff' called in the file's window offers the buffers the
conversation beside it left unsaved, and reviews the one chosen."
  (changes-entry--after-edit (code code-window view)
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   "other.txt")))
        (with-current-buffer code
          (call-interactively #'superchat-changes-diff)))
      (should (equal '("other.txt") offered))
      (let ((text (changes-entry--review-text)))
        (should (string-match-p "1/1" text))
        (should (string-match-p "other\\.txt" text))))))

(ert-deftest changes-entry/with-no-chat-beside-the-main-conversation ()
  "In a code buffer with no chat beside it, the commands act on the main
conversation, the one an in-place command from here joins (M10 slice 2):
no type error, as the reproduction had it; with nothing left unsaved there,
`superchat-changes' says so and `superchat-changes-diff' refuses plainly."
  (editor-mode--fixture nil (dir code code-window)
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal (buffer-local-value
                      'superchat--session-id (superchat--ensure-request-chat-buffer))
                     (superchat-changes--session)))
      (call-interactively #'superchat-changes)
      (should-not (get-buffer-window changes-entry--review))
      (should-error (call-interactively #'superchat-changes-diff) :type 'user-error))))

(ert-deftest changes-entry/save-and-revert-offer-the-chat-beside-it ()
  "`superchat-changes-save' and `-revert' called in the file's window offer
the buffers the conversation beside it left unsaved."
  (changes-entry--after-edit (code code-window view)
    (dolist (command '(superchat-changes-save superchat-changes-revert))
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq offered collection)
                     (throw 'offered nil))))
          (with-current-buffer code
            (catch 'offered (call-interactively command))))
        (should (equal '("other.txt") offered))))))

(ert-deftest changes-entry/the-chat-s-own-session-comes-first ()
  "Guard (green before and after): called in a chat View, the command uses
that View's own conversation, not the one shown in the side window."
  (changes-entry--after-edit (code code-window view)
    (let ((other (editor-mode--new-view)))
      (unwind-protect
          (with-current-buffer other
            (call-interactively #'superchat-changes)
            (should-not (get-buffer-window changes-entry--review)))
        (kill-buffer other)))))

(provide 'changes-entry-regression)

;;; changes-entry-regression.el ends here
