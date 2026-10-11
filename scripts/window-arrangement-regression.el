;;; window-arrangement-regression.el --- the model is told the window arrangement -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 5 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-5-window-arrangement.md,
;; contract W8 and W9): a request carries the selected frame's arrangement --
;; tabs, and which window shows which buffer where -- taken when it is
;; collected; and a file the model edited outside the conversation's
;; Workspace is pointed out on the review screen, not stopped.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/window-arrangement-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^window-arrangement/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-inline)
(require 'superchat-changes)
(require 'tab-bar)
(defvar superchat-display-style)

(defvar warr--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun warr--finder (dir)
  (cl-loop for root in warr--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun warr--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-windows-" t))))

(defmacro warr--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((warr--roots nil)
          ,@(mapcar (lambda (v) `(,v (warr--temp-dir))) vars)
          (project-find-functions (list #'warr--finder))
          (superchat-display-style 'editor))
     (setq warr--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun warr--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun warr--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun warr--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun warr--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(defun warr--user-text (n)
  "All non-system message text of the Nth provider request."
  (mapconcat #'hm--text (hm--body (hm--wire n)) "\n"))

(defun warr--one-window ()
  "Leave the selected frame with one ordinary window."
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window)))
  (delete-other-windows))

(defmacro warr--in-frame (&rest body)
  "Run BODY, then put the selected frame's windows and tabs back."
  (declare (indent 0))
  `(let ((config (current-window-configuration))
         (tabs (and (fboundp 'tab-bar-tabs) (length (tab-bar-tabs))))
         (mode (bound-and-true-p tab-bar-mode)))
     (unwind-protect (progn ,@body)
       (while (and tabs (> (length (tab-bar-tabs)) tabs)) (tab-bar-close-tab))
       (tab-bar-mode (if mode 1 -1))
       (set-window-configuration config))))

;;;; The arrangement as data and text ----------------------------------------------------

(ert-deftest window-arrangement/windows-are-listed-from-the-top-left ()
  (warr--in-frame
    (let ((a (get-buffer-create "warr-a"))
          (b (get-buffer-create "warr-b"))
          (chat (get-buffer-create "warr-chat")))
      (warr--one-window)
      (switch-to-buffer a)
      (let ((right (split-window-right)))
        (set-window-buffer right b)
        (let ((below (split-window right nil 'below)))
          (set-window-buffer below chat)
          (select-window right)))
      (let* ((arrangement (superchat-prompt--window-arrangement (selected-frame) chat))
             (windows (plist-get arrangement :windows)))
        (should (equal '("warr-a" "warr-b" "warr-chat")
                       (mapcar (lambda (w) (plist-get w :buffer)) windows)))
        (should (equal '(nil t nil) (mapcar (lambda (w) (plist-get w :selected)) windows)))
        (should (equal '(nil nil t) (mapcar (lambda (w) (plist-get w :conversation)) windows)))
        (should (= 0 (plist-get (nth 0 windows) :left)))
        (should (> (plist-get (nth 1 windows) :left) 0))
        (should (> (plist-get (nth 2 windows) :top) (plist-get (nth 1 windows) :top)))
        (let ((text (superchat-prompt--window-arrangement-render arrangement)))
          (should (string-prefix-p "[Emacs windows]\nWindows of the current tab (3), from the top left:\n- warr-a, " text))
          (should (string-match-p "^- warr-b, [0-9]+ columns wide at column [1-9][0-9]*, line 0, selected$" text))
          (should (string-match-p "^- this conversation, [0-9]+ columns wide at column [1-9][0-9]*, line [1-9][0-9]*$" text))
          ;; The chat window is not named by its buffer.
          (should-not (string-match-p "warr-chat" text))
          (should-not (string-match-p "\n\\'" text)))))))

(ert-deftest window-arrangement/a-side-window-is-said-to-be-one ()
  (let ((text (superchat-prompt--window-arrangement-render
               (list :windows (list (list :buffer "main" :width 80 :height 20 :left 0 :top 0)
                                    (list :buffer "chat" :width 40 :height 20 :left 80 :top 0
                                          :side 'right :selected t :conversation t))))))
    (should (string-match-p "^- main, 80 columns wide at column 0, line 0$" text))
    (should (string-match-p "^- this conversation, 40 columns wide at column 80, line 0, side window (right), selected$" text)))
  (warr--with '((:text "ok")) (p)
    (warr--in-frame
      (let* ((src (warr--file p))
             (view (with-current-buffer src (superchat))))
        (should (window-parameter (get-buffer-window view) 'window-side))
        (hm--turn view "where am I")
        (should (string-match-p "^- this conversation, .*, side window (right)" (warr--user-text 1)))))))

(ert-deftest window-arrangement/a-file-is-shown-beside-its-buffer ()
  (warr--in-frame
    (let ((root (warr--temp-dir)))
      (unwind-protect
          (let ((buffer (find-file-noselect (expand-file-name "notes.org" root))))
            (warr--one-window)
            (switch-to-buffer buffer)
            (let ((text (superchat-prompt--window-arrangement-render
                         (superchat-prompt--window-arrangement (selected-frame) nil))))
              (should (string-match-p
                       (concat "^- notes\\.org (" (regexp-quote (abbreviate-file-name
                                                                (expand-file-name "notes.org" root)))
                               "), ")
                       text)))
            (kill-buffer buffer))
        (delete-directory root t)))))

(ert-deftest window-arrangement/tabs-are-listed-when-there-is-more-than-one ()
  (warr--in-frame
    (warr--one-window)
    (tab-bar-mode -1)
    (should-not (plist-get (superchat-prompt--window-arrangement (selected-frame) nil) :tabs))
    (tab-bar-new-tab)
    (tab-bar-rename-tab "warr-second")
    (let* ((arrangement (superchat-prompt--window-arrangement (selected-frame) nil))
           (text (superchat-prompt--window-arrangement-render arrangement)))
      (should (= 2 (length (plist-get arrangement :tabs))))
      (should (string-match-p "^Tabs (2): .*\\[warr-second\\] (current)$" text)))))

(ert-deftest window-arrangement/long-lists-are-cut-and-counted ()
  (let* ((windows (mapcar (lambda (i) (list :buffer (format "b%d" i) :width 10 :height 5
                                            :left 0 :top i))
                          (number-sequence 1 15)))
         (tabs (mapcar (lambda (i) (list :name (format "t%d" i) :current (= i 1)))
                       (number-sequence 1 13)))
         (text (superchat-prompt--window-arrangement-render
                (list :frames 3 :tabs tabs :windows windows))))
    (should (string-match-p "^Frames: 3 visible; these are the selected frame's\\.$" text))
    (should (string-match-p "^Tabs (13): \\[t1\\] (current), t2, .*t10, and 3 more$" text))
    (should (string-match-p "^Windows of the current tab (15)" text))
    (should (string-match-p "^- b12, " text))
    (should-not (string-match-p "^- b13, " text))
    (should (string-match-p "\n- and 3 more\\'" text))
    (should (string-match-p "^- \\(x\\{57\\}\\)\\.\\.\\., "
                            (superchat-prompt--window-arrangement-render
                             (list :windows (list (list :buffer (make-string 80 ?x)
                                                        :width 1 :height 1 :left 0 :top 0))))))
    (should-not (superchat-prompt--window-arrangement-render nil))))

;;;; The arrangement in requests ---------------------------------------------------------

(ert-deftest window-arrangement/a-chat-request-carries-the-arrangement ()
  (warr--with '((:text "ok")) (p)
    (warr--in-frame
      (let* ((src (warr--file p))
             (view (with-current-buffer src (superchat))))
        (warr--one-window)
        (switch-to-buffer src)
        (set-window-buffer (split-window-right) view)
        (hm--turn view "arrange my windows")
        (let ((text (warr--user-text 1)))
          (should (string-match-p "\\[Emacs windows\\]" text))
          (should (string-match-p "^- a\\.txt (" text))
          (should (string-match-p "^- this conversation, " text)))))))

(ert-deftest window-arrangement/in-place-requests-carry-it-too ()
  (warr--with '((:text "ok")) (p)
    (warr--in-frame
      (let ((src (warr--file p)))
        (warr--one-window)
        (switch-to-buffer src)
        (with-current-buffer src (superchat-inline-act "change it"))
        (warr--settled)
        (should (string-match-p "\\[Emacs windows\\]\n.*\n- a\\.txt (" (warr--user-text 1)))))))

(ert-deftest window-arrangement/other-surfaces-and-confirmed-text-get-none ()
  (should-not (superchat-prompt-collector--windows (list :surface 'subagent :context nil) nil nil))
  (should-not (superchat-prompt-collector--windows (list :surface 'compaction :context nil) nil nil))
  ;; Text the user previewed and confirmed is left as it was confirmed.
  (should-not (superchat-prompt-collector--windows
               (list :surface 'chat :context (list :chat-prompt-override "confirmed"
                                                   :chat-prepared-context t))
               nil nil))
  ;; Text a skill fixed was reviewed by nobody: it is told the windows
  ;; (decision 2026-10-10, a skill message says where the user is).
  (should (superchat-prompt-collector--windows
           (list :surface 'chat :context (list :chat-prompt-override "from a skill"))
           nil nil))
  (should (superchat-prompt-collector--windows (list :surface 'chat :context nil) nil nil)))

;;;; Files outside the workspace on the review screen ------------------------------------

(defun warr--edit-round (buffer-name)
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . "hello") (new_string . "changed")))))

(ert-deftest window-arrangement/an-edit-outside-the-workspace-is-pointed-out ()
  (warr--with nil (p q)
    (let* ((inside (warr--file p))
           (outside (let ((file (expand-file-name "b.txt" q)))
                      (with-temp-file file (insert "hello\n"))
                      (find-file-noselect file)))
           (view (with-current-buffer inside (superchat))))
      (setq smoke--script (list (warr--edit-round (buffer-name inside))
                                (warr--edit-round (buffer-name outside))
                                '(:text "done")))
      (let* ((run (hm--turn view "change both"))
             (rows (superchat-changes-candidates (hm--session view) run))
             (row-in (cl-find inside rows :key (lambda (r) (plist-get r :buffer))))
             (row-out (cl-find outside rows :key (lambda (r) (plist-get r :buffer)))))
        (should row-in)
        (should row-out)
        (should-not (plist-get row-in :outside))
        (should (equal p (plist-get row-out :outside)))
        ;; Not stopped: the outside buffer was edited like the other.
        (should (string-match-p "changed" (with-current-buffer outside (buffer-string))))
        (should (string-match-p (concat "b\\.txt -> .* \\[outside this workspace, "
                                        (regexp-quote (abbreviate-file-name p)) "\\]")
                                (superchat-changes--lines rows)))
        (should-not (string-match-p "a\\.txt -> [^\n]*outside" (superchat-changes--lines rows)))
        (let ((review (superchat-changes-review (list row-out row-in))))
          (unwind-protect
              (progn
                (should (string-match-p
                         (concat "^Outside this conversation's workspace ("
                                 (regexp-quote (abbreviate-file-name p)) ")\\.$")
                         (with-current-buffer review (buffer-string))))
                (superchat-changes-review-next)
                (should-not (string-match-p "Outside this conversation"
                                            (with-current-buffer review (buffer-string)))))
            (when (buffer-live-p review) (kill-buffer review))))))))

(ert-deftest window-arrangement/a-temporary-conversation-has-no-outside ()
  (warr--with nil (q)
    (let* ((outside (let ((file (expand-file-name "b.txt" q)))
                      (with-temp-file file (insert "hello\n"))
                      (find-file-noselect file)))
           (view (smoke--open-chat)))
      (setq smoke--script (list (warr--edit-round (buffer-name outside)) '(:text "done")))
      (let* ((run (hm--turn view "change it"))
             (rows (superchat-changes-candidates (hm--session view) run)))
        (should (= 1 (length rows)))
        (should-not (plist-get (car rows) :outside))))))

(provide 'window-arrangement-regression)
;;; window-arrangement-regression.el ends here
