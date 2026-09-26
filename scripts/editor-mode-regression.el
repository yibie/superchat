;;; editor-mode-regression.el --- M10 slice 1b: editor mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for slice 1b of the editor-mode/agent-mode decision
;; (docs/decisions/implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md):
;; in editor mode, the default, the chat View opens in a side window on the
;; right of the tab being edited, beside the code, instead of in the Session's
;; own tab over the whole frame; `superchat-display-style' `agent' keeps that
;; older layout.  A Session shown only in the side window has no tab, so
;; closing it from its chat View finds the Session by the View's own ownership
;; marker (step 1 of docs/protocols/session-editor-workspace-close.md).
;;
;; Batch Emacs has one 80x25 frame whose tabs and side windows work; every
;; test starts from one tab holding one window on a file.
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
;;     -l scripts/editor-mode-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^editor-mode/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'tab-bar)

(when (getenv "SMOKE_REAL")
  (error "Editor mode regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(require 'superchat-widgets)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;; Declared so a `let' binding below is dynamic even before the option exists.
(defvar superchat-display-style)
(defvar superchat-changes-review-after-run)

(defun editor-mode--reset-frame ()
  "Leave the frame with one untagged tab holding one ordinary window."
  (let ((tab-bar-tab-prevent-close-functions nil))
    (tab-bar-close-other-tabs))
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (superchat-editor-workspace--clear-tab-session-id tab))
    (tab-bar-tabs-set tabs))
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window)))
  (delete-other-windows))

(defun editor-mode--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent))
  file)

(defun editor-mode--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro editor-mode--fixture (script bindings &rest body)
  "Run BODY from one tab holding one selected window on a file.
BINDINGS is (DIR CODE CODE-WINDOW): a fresh directory, the buffer visiting
DIR/code.el, and the window showing it.  SCRIPT, the scripted model's
answers, is evaluated with DIR bound.  BODY runs in the canonical fixture."
  (declare (indent 2) (debug t))
  (let ((dir (nth 0 bindings)) (code (nth 1 bindings))
        (code-window (nth 2 bindings)))
    `(let ((,dir (file-name-as-directory
                  (file-truename (make-temp-file "editor-mode-" t))))
           (superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil))
       (editor-mode--reset-frame)
       (unwind-protect
           (smoke--with-fixture ,script
             (let* ((,code (find-file-noselect
                            (editor-mode--write (expand-file-name "code.el" ,dir)
                                                ";; code\n(one)\n")))
                    (,code-window (selected-window)))
               (switch-to-buffer ,code)
               ,@body))
         (setq superchat-llm-tools-list nil)
         (editor-mode--reset-frame)
         (dolist (buffer (buffer-list))
           (when-let* ((file (buffer-file-name buffer)))
             (when (string-prefix-p ,dir (file-truename file))
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
         (delete-directory ,dir t)))))

(defun editor-mode--tab-session ()
  "The Session parameter of the current tab, or nil."
  (superchat-editor-workspace--tab-session-id
   (superchat-editor-workspace--current-tab)))

(defun editor-mode--tag-current-tab (session)
  "Make the current tab SESSION's own tab."
  (let ((tabs (tab-bar-tabs)))
    (superchat-editor-workspace--set-tab-session-id
     (superchat-editor-workspace--current-tab) session)
    (tab-bar-tabs-set tabs)))

(defun editor-mode--view ()
  "The primary chat View."
  (get-buffer superchat-buffer-name))

(defun editor-mode--side-p (window)
  (and (window-live-p window)
       (eq 'right (window-parameter window 'window-side))))

(defun editor-mode--windows ()
  (window-list nil 'no-minibuf))

(defun editor-mode--show-beside (view)
  "Show VIEW in a right side window without the product's display code.
Tests of what happens once a View is there use this, so that they do not
depend on how it got there."
  (display-buffer view '((display-buffer-in-side-window)
                         (side . right) (slot . -1) (window-width . 0.42))))

(defun editor-mode--new-view ()
  "Create and return the chat View of a new Session, not displayed."
  (let ((session (superchat--new-session-id)))
    (superchat--ensure-chat-view (superchat--session-buffer-name session)
                                 session)))

(defun editor-mode--settled ()
  (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 10))

;;;; Opening the chat beside the file

(ert-deftest editor-mode/open/the-chat-opens-beside-the-file ()
  "`M-x superchat' from a file shows the chat View in a side window on the
right of this tab and selects it; the file's window stays, no tab is added,
and this tab does not become a Session tab.  Opening it again from the file
reuses that window."
  (editor-mode--fixture nil (dir code code-window)
    (let ((tabs (length (tab-bar-tabs))))
      (superchat)
      (let* ((view (editor-mode--view))
             (chat (get-buffer-window view)))
        (should (window-live-p chat))
        (should (editor-mode--side-p chat))
        (should (eq chat (selected-window)))
        (should (window-live-p code-window))
        (should (eq code (window-buffer code-window)))
        (should (= tabs (length (tab-bar-tabs))))
        (should-not (editor-mode--tab-session))
        (select-window code-window)
        (superchat)
        (should (equal (list chat) (get-buffer-window-list view nil t)))
        (should (eq chat (selected-window)))
        (should (= 2 (length (editor-mode--windows))))
        (should (= tabs (length (tab-bar-tabs))))))))

(ert-deftest editor-mode/open/another-conversation-takes-the-same-window ()
  "From the chat View in the side window, a new conversation and a reopened
one are each shown in that same window; the file stays where it was and no
tab is added."
  (editor-mode--fixture nil (dir code code-window)
    (let ((tabs (length (tab-bar-tabs))))
      (superchat)
      (let* ((first (editor-mode--view))
             (first-session (buffer-local-value 'superchat--session-id first))
             (chat (get-buffer-window first)))
        (should (editor-mode--side-p chat))
        (let ((second (with-current-buffer first (superchat-new-session))))
          (should (buffer-live-p second))
          (should-not (eq second first))
          (should (eq second (window-buffer chat)))
          (should (eq chat (selected-window)))
          (should (eq code (window-buffer code-window)))
          (should (= tabs (length (tab-bar-tabs))))
          (with-current-buffer second (superchat-open-session first-session))
          (should (eq first (window-buffer chat)))
          (should (eq chat (selected-window)))
          (should (eq code (window-buffer code-window)))
          (should (= tabs (length (tab-bar-tabs))))
          (should-not (editor-mode--tab-session)))))))

(ert-deftest editor-mode/act/the-chat-appears-beside-the-file-which-keeps-focus ()
  "`superchat-act' from a file shows the chat View in the side window without
selecting it: the file's window stays selected and keeps showing the file."
  (editor-mode--fixture (list '(:text "acted")) (dir code code-window)
    (superchat-act "say something")
    (let ((chat (get-buffer-window (editor-mode--view))))
      (should (window-live-p chat))
      (should (editor-mode--side-p chat))
      (should (eq code-window (selected-window)))
      (should (eq code (window-buffer code-window)))
      (should (= 2 (length (editor-mode--windows))))
      (should (editor-mode--settled))
      (should (eq code-window (selected-window)))
      (should (eq code (window-buffer code-window))))))

(ert-deftest editor-mode/send-region/the-region-goes-to-the-chat-beside-the-file ()
  "`superchat-send-region' puts the region into the chat View in the side
window and selects it; the file stays visible."
  (editor-mode--fixture (list '(:text "looked")) (dir code code-window)
    (superchat-send-region (point-min) (point-max) "look at this")
    (let ((chat (get-buffer-window (editor-mode--view))))
      (should (window-live-p chat))
      (should (editor-mode--side-p chat))
      (should (eq chat (selected-window)))
      (should (eq code (window-buffer code-window)))
      (should (= 2 (length (editor-mode--windows))))
      (should (editor-mode--settled))
      (should (eq code (window-buffer code-window))))))

;;;; The right side, shared

(ert-deftest editor-mode/side/the-chat-and-the-plan-panel-each-keep-a-place ()
  "With the plan panel open on the right, opening the chat adds its own
window above the panel instead of taking the panel's place."
  (editor-mode--fixture nil (dir code code-window)
    (let ((plan (get-buffer-create " *editor-mode-plan*")))
      (superchat-side-window-show plan 'plan)
      (select-window code-window)
      (superchat)
      (let ((chat (get-buffer-window (editor-mode--view)))
            (panel (get-buffer-window plan)))
        (should (editor-mode--side-p chat))
        (should (editor-mode--side-p panel))
        (should-not (eq chat panel))
        (should (< (nth 1 (window-edges chat)) (nth 1 (window-edges panel))))
        (should (eq code (window-buffer code-window)))))))

(ert-deftest editor-mode/side/a-window-already-showing-the-chat-is-left-alone ()
  "The same chat View may be shown in several places: opening it beside the
file does not close another window that already shows it.  The shared
side-window function closes such windows for the other panels, and only in an
interactive Emacs, so the opening runs as in one."
  (editor-mode--fixture nil (dir code code-window)
    (let* ((view (superchat--ensure-chat-view superchat-buffer-name))
           (below (split-window code-window nil 'below)))
      (set-window-buffer below view)
      (select-window code-window)
      (let ((noninteractive nil))
        (superchat))
      (should (window-live-p below))
      (should (eq view (window-buffer below)))
      (should (cl-some #'editor-mode--side-p (get-buffer-window-list view nil t))))))

;;;; Agent mode is today's layout

(ert-deftest editor-mode/agent/the-conversation-gets-its-own-tab-as-before ()
  "Guard (green before and after): with `superchat-display-style' `agent',
`M-x superchat' from a file selects a new tab tagged with the Session, where
the chat View is the only window."
  (let ((superchat-display-style 'agent))
    (editor-mode--fixture nil (dir code code-window)
      (let ((tabs (length (tab-bar-tabs))))
        (superchat)
        (let ((view (editor-mode--view)))
          (should (= (1+ tabs) (length (tab-bar-tabs))))
          (should (equal (buffer-local-value 'superchat--session-id view)
                         (editor-mode--tab-session)))
          (should (equal (list (selected-window)) (editor-mode--windows)))
          (should (eq view (window-buffer (selected-window))))
          (should-not (window-parameter (selected-window) 'window-side)))))))

(ert-deftest editor-mode/agent/in-the-conversation-s-own-tab-it-stays-whole ()
  "Guard (green before and after): in editor mode, `M-x superchat' from the
conversation's own tab shows it there as agent mode does -- the only window --
and not a second time in a side window of its own tab."
  (editor-mode--fixture nil (dir code code-window)
    (let ((superchat-display-style 'agent))
      (superchat))
    (let* ((view (editor-mode--view))
           (session (buffer-local-value 'superchat--session-id view))
           (tabs (length (tab-bar-tabs))))
      (should (equal session (editor-mode--tab-session)))
      (let ((superchat-display-style 'editor))
        (superchat))
      (should (equal session (editor-mode--tab-session)))
      (should (equal (list (selected-window)) (editor-mode--windows)))
      (should (eq view (window-buffer (selected-window))))
      (should-not (window-parameter (selected-window) 'window-side))
      (should (= tabs (length (tab-bar-tabs)))))))

;;;; A Run sent from the chat beside the file

(defun editor-mode--edit-other-script (dir)
  "Open DIR/other.txt, edit it, and answer."
  (let ((other (editor-mode--write (expand-file-name "other.txt" dir) "two\n")))
    (list `(:tool-uses (("open_file" (path . ,other))))
          '(:tool-uses (("edit_buffer" (buffer_name . "other.txt")
                         (old_string . "two") (new_string . "TWO"))))
          '(:text "done"))))

(ert-deftest editor-mode/run/a-run-from-the-chat-leaves-the-file-window-alone ()
  "Guard (green before and after): a Run sent from the chat View in the side
window that opens and edits another file leaves the screen as it was: the
file's window still shows the file and the chat is still beside it."
  (let ((superchat-changes-review-after-run nil))
    (editor-mode--fixture (editor-mode--edit-other-script dir) (dir code code-window)
      (let* ((view (superchat--ensure-chat-view superchat-buffer-name))
             (chat (editor-mode--show-beside view)))
        (select-window chat)
        (smoke--send view "edit the other file")
        (should (editor-mode--settled))
        (should (buffer-modified-p (get-file-buffer (expand-file-name "other.txt" dir))))
        (should (eq code (window-buffer code-window)))
        (should (eq view (window-buffer chat)))
        (should (eq chat (selected-window)))
        (should (= 2 (length (editor-mode--windows))))))))

(ert-deftest editor-mode/run/the-review-screen-takes-the-file-window-and-q-gives-it-back ()
  "When a Run sent from the chat View in the side window leaves a buffer
unsaved, the review screen appears in the file's window, not beside it, and
the cursor goes to it (M10 record, amended 2026-09-25); `q' puts the file back
and the cursor back in the chat."
  (let ((superchat-changes-review-after-run t))
    (editor-mode--fixture (editor-mode--edit-other-script dir) (dir code code-window)
      (let* ((view (superchat--ensure-chat-view superchat-buffer-name))
             (chat (editor-mode--show-beside view)))
        (select-window chat)
        (smoke--send view "edit the other file")
        (should (editor-mode--settled))
        (should (smoke--wait (lambda () (get-buffer-window "*superchat review*")) 5))
        (let ((review (get-buffer-window "*superchat review*")))
          (should (eq code-window review))
          (should-not (window-parameter review 'window-side))
          (should (eq view (window-buffer chat)))
          (should (eq review (selected-window)))
          (with-current-buffer "*superchat review*"
            (call-interactively (key-binding (kbd "q"))))
          (should (eq code (window-buffer code-window)))
          (should (eq view (window-buffer chat)))
          (should (eq chat (selected-window))))))))

;;;; Closing a conversation shown only beside the file

(ert-deftest editor-mode/close/from-the-chat-view-closes-its-session ()
  "A conversation shown only in a side window has no Session tab.  Closing it
from its chat View finds the Session by the View's own ownership marker and
closes it by the protocol: the View is killed, the Session is `closed', its
side window goes, and the file's window and the tabs are left alone."
  (editor-mode--fixture nil (dir code code-window)
    (let* ((tabs (length (tab-bar-tabs)))
           (view (superchat--ensure-chat-view superchat-buffer-name))
           (session (buffer-local-value 'superchat--session-id view))
           (chat (editor-mode--show-beside view)))
      (should-not (editor-mode--tab-session))
      (select-window chat)
      (should (eq 'closed (call-interactively #'superchat-close-session-workspace)))
      (should-not (buffer-live-p view))
      (should (eq 'closed (superchat-editor-workspace-state session)))
      (should-not (window-live-p chat))
      (should (window-live-p code-window))
      (should (eq code (window-buffer code-window)))
      (should (= tabs (length (tab-bar-tabs)))))))

(ert-deftest editor-mode/close/an-active-run-is-asked-about-once-each-time ()
  "Closing from the chat View while its Run is active asks once: no leaves
the Run, the View and its side window as they were; yes cancels the Run and
closes the Session."
  (editor-mode--fixture (list '(:hang t)) (dir code code-window)
    (let* ((view (superchat--ensure-chat-view superchat-buffer-name))
           (session (buffer-local-value 'superchat--session-id view))
           (chat (editor-mode--show-beside view))
           (asked 0)
           (answer nil))
      (select-window chat)
      (let ((run (smoke--send view "work")))
        (should (member run (superchat-runtime-active-runs)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (cl-incf asked) answer)))
          (should (eq 'rejected (call-interactively #'superchat-close-session-workspace)))
          (should (= 1 asked))
          (should (member run (superchat-runtime-active-runs)))
          (should (buffer-live-p view))
          (should (eq view (window-buffer chat)))
          (setq answer t)
          (should (eq 'closed (call-interactively #'superchat-close-session-workspace)))
          (should (= 2 asked))
          (should (editor-mode--settled))
          (should-not (buffer-live-p view))
          (should (eq 'closed (superchat-editor-workspace-state session))))))))

(ert-deftest editor-mode/close/the-chat-view-s-own-session-comes-before-the-tab-s ()
  "In Session A's tab, closing from Session B's chat View in the side window
closes B, not A: the View's own marker comes before the tab's parameter."
  (editor-mode--fixture nil (dir code code-window)
    (let* ((a-view (superchat--ensure-chat-view superchat-buffer-name))
           (a (buffer-local-value 'superchat--session-id a-view))
           (b-view (editor-mode--new-view))
           (b (buffer-local-value 'superchat--session-id b-view)))
      (should-not (equal a b))
      (editor-mode--tag-current-tab a)
      (select-window (editor-mode--show-beside b-view))
      (should (eq 'closed (call-interactively #'superchat-close-session-workspace)))
      (should-not (buffer-live-p b-view))
      (should (eq 'closed (superchat-editor-workspace-state b)))
      (should (buffer-live-p a-view))
      (should (eq 'open (superchat-editor-workspace-state a)))
      (should (equal a (editor-mode--tab-session))))))

(provide 'editor-mode-regression)

;;; editor-mode-regression.el ends here
