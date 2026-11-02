;;; keys-land-regression.el --- what the user types lands where they type -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-what-you-type-lands-where-you-type.md:
;; the review screen, a system line, a task and `eval_elisp' leave the cursor of
;; someone who is working where it is, and no key on the review screen changes
;; anything by accident.
;;
;; All four were measured with real keys in a real terminal frame
;; (.scratch/super-agent-exp, docs/research/super-agent.md).  A batch Emacs has
;; nobody at the keyboard: "the cursor" here is point in the selected window,
;; and "typing" is `self-insert-command' in it.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/keys-land-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^keys-land/")'

;;; Code:

(load (expand-file-name "presence-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun keys-land--press (char)
  "Press the printable CHAR in the selected window, as the command loop does.
`command-execute' stops at a disabled command, which `call-interactively' does not."
  (with-current-buffer (window-buffer (selected-window))
    (let ((last-command-event char))
      (command-execute (or (key-binding (vector char)) #'self-insert-command)))))

(defun keys-land--type (text)
  (mapc #'keys-land--press text))

;;;; The review screen's keys ------------------------------------------------------

(defmacro keys-land--on-the-screen (name &rest body)
  "Run BODY with the review screen up after a Run edited the buffer named NAME."
  (declare (indent 1) (debug t))
  `(presence--with-file ,name
     (hm--fixture (list (presence--edit-round ,name) '(:text "DONE"))
       (let* ((view (smoke--open-chat))
              (run (smoke--send view "fix it")))
         (ignore view)
         (presence--ended run)
         (should (get-buffer-window superchat-changes-review-buffer-name t))
         ,@body))))

(defconst keys-land--looking-keys
  '((?p . diff-hunk-prev)
    (?N . diff-file-next) (?\} . diff-file-next)
    (?P . diff-file-prev) (?\{ . diff-file-prev)
    (?< . beginning-of-buffer) (?> . end-of-buffer)
    (?r . superchat-changes-review-narrow) (?W . widen)
    (?w . diff-kill-ring-save)
    (?h . describe-mode) (?? . describe-mode))
  "The keys of `diff-mode' the review screen lets through: they only look or copy.")

(ert-deftest keys-land/a-letter-that-is-not-the-screen-s-does-nothing ()
  ;; Measured 2026-10-31: u reverted the hunk, o took the cursor to the source.
  (keys-land--on-the-screen name
    (with-current-buffer superchat-changes-review-buffer-name
      (dotimes (i 94)
        (let* ((char (+ 33 i))
               (command (key-binding (vector char)))
               (looking (alist-get char keys-land--looking-keys)))
          (ert-info ((format "%c is %S" char command))
            (pcase char
              (?s (should (eq command 'superchat-changes-review-save)))
              (?n (should (eq command 'superchat-changes-review-next)))
              (?q (should (eq command 'superchat-changes-review-quit)))
              (?g (should (eq command 'superchat-changes-review-refresh)))
              (_ (should (eq command (if (commandp looking)
                                         looking
                                       'superchat-changes-review-unavailable)))))))))))

(defun keys-land--press-quietly (char)
  "Press CHAR; a command that only says it cannot, as `diff-file-next' does, is fine."
  (condition-case nil
      (keys-land--press char)
    (error nil)))

(ert-deftest keys-land/the-keys-that-look-leave-the-text-and-the-cursor ()
  ;; What the choice to put them back rests on: a slip costs no text.
  (keys-land--on-the-screen name
    (let* ((buffer (get-buffer name))
           (file (buffer-file-name buffer))
           (text (with-current-buffer buffer (buffer-string)))
           (kill-ring nil)
           (screen (get-buffer-window superchat-changes-review-buffer-name t)))
      (select-window screen)
      (dolist (entry keys-land--looking-keys)
        (ert-info ((format "after %c" (car entry)))
          (keys-land--press-quietly (car entry))
          (should (eq screen (selected-window)))
          (should (eq (get-buffer superchat-changes-review-buffer-name)
                      (window-buffer screen)))
          (should (equal text (with-current-buffer buffer (buffer-string))))
          (should (buffer-modified-p buffer))
          (should (equal "hello\n" (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string)))))))))

(ert-deftest keys-land/the-keys-that-look-do-what-diff-mode-does ()
  (keys-land--on-the-screen name
    (let ((kill-ring nil)
          (screen (get-buffer-window superchat-changes-review-buffer-name t)))
      (select-window screen)
      (with-current-buffer superchat-changes-review-buffer-name
        (keys-land--press ?>)
        (should (= (point) (point-max)))
        (keys-land--press ?p)
        (should (looking-at-p "@@ "))
        (keys-land--press ?<)
        (should (= (point) (point-min)))
        (when (commandp 'diff-kill-ring-save)
          (keys-land--press ?w)
          (should (string-match-p "HELLO" (car kill-ring))))
        (keys-land--press ?h))
      (should (get-buffer-window "*Help*"))
      (should (eq screen (selected-window))))))

(ert-deftest keys-land/no-key-on-the-screen-asks-first ()
  ;; Measured 2026-10-31: r was `diff-restrict-view', which Emacs ships disabled.
  ;; Its question took the keys that came next.
  (keys-land--on-the-screen name
    (with-current-buffer superchat-changes-review-buffer-name
      (dotimes (i 94)
        (let ((command (key-binding (vector (+ 33 i)))))
          (ert-info ((format "%c is %S" (+ 33 i) command))
            (should-not (get command 'disabled))))))))

(ert-deftest keys-land/a-phrase-landing-on-the-screen-writes-no-init-file ()
  ;; Measured 2026-10-31: "try to" answered that question with y and a space, and
  ;; (put 'diff-restrict-view 'disabled nil) was written to the init file.
  (keys-land--on-the-screen name
    (let ((user-init-file (make-temp-file "keys-land-init-" nil ".el" ";; mine\n")))
      (unwind-protect
          (progn
            (select-window (get-buffer-window superchat-changes-review-buffer-name t))
            (with-current-buffer superchat-changes-review-buffer-name
              (goto-char (point-min))
              (re-search-forward "^@@ "))
            (mapc #'keys-land--press-quietly "try to")
            (should (equal ";; mine\n" (with-temp-buffer
                                         (insert-file-contents user-init-file)
                                         (buffer-string))))
            (should (get 'diff-restrict-view 'disabled))
            (should (with-current-buffer superchat-changes-review-buffer-name
                      (buffer-narrowed-p))))
        (delete-file user-init-file)))))

(ert-deftest keys-land/o-still-leaves-the-cursor-on-the-screen ()
  ;; It takes the cursor to the source, and the next letters land in the code.
  (keys-land--on-the-screen name
    (let ((screen (get-buffer-window superchat-changes-review-buffer-name t))
          (text (with-current-buffer (get-buffer name) (buffer-string))))
      (select-window screen)
      (keys-land--type "okay")
      (should (eq screen (selected-window)))
      (should (equal text (with-current-buffer (get-buffer name) (buffer-string)))))))

(ert-deftest keys-land/a-screen-narrowed-to-one-change-still-reviews ()
  (keys-land--on-the-screen name
    (let* ((buffer (get-buffer name))
           (file (buffer-file-name buffer)))
      (select-window (get-buffer-window superchat-changes-review-buffer-name t))
      (with-current-buffer superchat-changes-review-buffer-name
        (goto-char (point-min))
        (re-search-forward "^@@ ")
        (keys-land--press ?r)
        (should (buffer-narrowed-p))
        (should-not (save-excursion (goto-char (point-min)) (looking-at-p "Review ")))
        (keys-land--press ?W)
        (should-not (buffer-narrowed-p))
        (goto-char (point-min))
        (re-search-forward "^@@ ")
        (keys-land--press ?r)
        (superchat-changes-review-refresh)
        (should-not (buffer-narrowed-p))
        (should (save-excursion (goto-char (point-min)) (looking-at-p "Review ")))
        (goto-char (point-min))
        (re-search-forward "^@@ ")
        (keys-land--press ?r))
      (keys-land--press ?s)
      (should-not (buffer-modified-p buffer))
      (should (string-match-p "HELLO" (with-temp-buffer
                                        (insert-file-contents file)
                                        (buffer-string))))
      (should-not (get-buffer superchat-changes-review-buffer-name)))))

(ert-deftest keys-land/r-with-a-prefix-shows-the-whole-file-s-changes ()
  ;; What `diff-restrict-view' does with its argument, kept through the wrapper.
  (keys-land--on-the-screen name
    (select-window (get-buffer-window superchat-changes-review-buffer-name t))
    (with-current-buffer superchat-changes-review-buffer-name
      (goto-char (point-min))
      (re-search-forward "^@@ ")
      ;; `command-execute' takes the argument from `prefix-arg'.
      (let ((prefix-arg '(4)))
        (keys-land--press ?r))
      (should (buffer-narrowed-p))
      (should (save-excursion (goto-char (point-min)) (looking-at-p "--- "))))))

(ert-deftest keys-land/a-help-window-does-not-get-in-the-way-of-saving ()
  (keys-land--on-the-screen name
    (let ((buffer (get-buffer name)))
      (select-window (get-buffer-window superchat-changes-review-buffer-name t))
      (keys-land--press ?h)
      (should (get-buffer-window "*Help*"))
      (keys-land--press ?s)
      (should-not (buffer-modified-p buffer))
      (should-not (get-buffer superchat-changes-review-buffer-name))
      (should (eq (window-buffer (selected-window)) view)))))

(ert-deftest keys-land/a-later-meaning-of-a-looking-key-is-not-reached ()
  ;; `u' reached someone typing because a later `diff-mode' added it.  The same
  ;; must not happen through a letter the screen lets through.
  (keys-land--on-the-screen name
    (with-current-buffer superchat-changes-review-buffer-name
      (let ((before (lookup-key diff-mode-shared-map "p")))
        (unwind-protect
            (progn
              (define-key diff-mode-shared-map "p" #'diff-revert-and-kill-hunk)
              (should (eq 'diff-hunk-prev (key-binding "p"))))
          (define-key diff-mode-shared-map "p" before))))))

(ert-deftest keys-land/a-key-whose-command-this-emacs-lacks-stays-refused ()
  ;; `diff-kill-ring-save' is new in Emacs 31, and Superchat supports Emacs 30.
  (cl-letf (((symbol-function 'diff-kill-ring-save) nil))
    (should (eq 'superchat-changes-review-unavailable
                (lookup-key (superchat-changes--review-keymap) "w"))))
  (should (eq 'diff-hunk-prev
              (lookup-key (superchat-changes--review-keymap) "p"))))

(ert-deftest keys-land/u-and-a-space-leave-the-edit-and-the-file-alone ()
  ;; Measured 2026-10-31: the hunk was reverse-applied and the file written.
  (keys-land--on-the-screen name
    (let* ((buffer (get-buffer name))
           (file (buffer-file-name buffer))
           (text (with-current-buffer buffer (buffer-string))))
      (should (string-match-p "HELLO" text))
      (select-window (get-buffer-window superchat-changes-review-buffer-name t))
      (keys-land--press ?u)
      ;; Space scrolls; at the end of a short diff it says so.
      (condition-case nil (keys-land--press ?\s) (end-of-buffer nil))
      (should (equal text (with-current-buffer buffer (buffer-string))))
      (should (buffer-modified-p buffer))
      (should (equal "hello\n" (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(defconst keys-land--source-changing
  '(diff-apply-hunk diff-apply-buffer diff-revert-and-kill-hunk diff-ediff-patch
    diff-hunk-kill diff-file-kill diff-delete-other-hunks diff-split-hunk
    diff-reverse-direction diff-context->unified diff-unified->context
    diff-add-change-log-entries-other-window diff-delete-trailing-whitespace
    diff-ignore-whitespace-hunk diff-refresh-hunk)
  "Commands of `diff-mode' that change a source buffer or the view.")

(defun keys-land--keys-in (map prefix)
  "Every key sequence MAP binds under PREFIX, leaving the menu bar out."
  (let (keys)
    (map-keymap
     (lambda (event binding)
       (let ((sequence (vconcat prefix (vector event))))
         (cond ((memq event '(menu-bar tool-bar remap)) nil)
               ((keymapp binding)
                (when (< (length sequence) 4)
                  (setq keys (append (keys-land--keys-in binding sequence) keys))))
               (binding (push sequence keys)))))
     map)
    keys))

(ert-deftest keys-land/no-key-reaches-a-command-that-changes-the-source ()
  ;; Walks what is bound in this Emacs, so a key a later `diff-mode' adds is caught.
  (keys-land--on-the-screen name
    (with-current-buffer superchat-changes-review-buffer-name
      (let (reached)
        (dolist (map (current-active-maps))
          (dolist (keys (keys-land--keys-in map []))
            (let ((command (ignore-errors (key-binding keys))))
              (when (memq command keys-land--source-changing)
                (push (cons (key-description keys) command) reached)))))
        (should-not reached)))))

;;;; Whether the cursor goes to the screen -----------------------------------------

(ert-deftest keys-land/quiet-keys-are-two-seconds-without-one ()
  (let ((noninteractive nil)
        (superchat-changes-review-quiet-seconds 2))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () nil)))
      (should-not (superchat-changes--keys-quiet-p)))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 1))))
      (should-not (superchat-changes--keys-quiet-p)))
    (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 3))))
      (should (superchat-changes--keys-quiet-p)))))

(ert-deftest keys-land/writing-the-next-message-keeps-the-cursor ()
  ;; Measured 2026-10-31: the keys went to the screen; s saved, u reverted.
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "fix it")))
        (presence--type view "NEXT-MESSAGE")
        (presence--ended run)
        (should (get-buffer-window superchat-changes-review-buffer-name t))
        (should (eq view (window-buffer (selected-window))))
        (keys-land--type "!")
        (should (equal "NEXT-MESSAGE!" (presence--input view)))
        (should (buffer-modified-p (get-buffer name)))))))

(ert-deftest keys-land/a-key-just-now-keeps-the-cursor-with-an-empty-input-area ()
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name) '(:text "DONE"))
      (cl-letf (((symbol-function 'superchat-changes--keys-quiet-p) (lambda () nil)))
        (let* ((view (smoke--open-chat))
               (run (smoke--send view "fix it")))
          (presence--ended run)
          (should (get-buffer-window superchat-changes-review-buffer-name t))
          (should (eq view (window-buffer (selected-window)))))))))

(ert-deftest keys-land/a-request-in-place-is-not-judged-by-an-older-message ()
  ;; A stale place from an earlier message says nothing about a request made
  ;; from a buffer in place: only the keys and the minibuffer count there.
  (let ((superchat--asked-from (make-hash-table :test #'equal)))
    (puthash "s" (get-buffer-create " *keys-land-older-place*") superchat--asked-from)
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run) (list :surface 'inline-act))))
      (should (superchat-changes--review-takes-cursor-p "s" "r")))
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run) (list :surface 'chat))))
      (should-not (superchat-changes--review-takes-cursor-p "s" "r")))))

(ert-deftest keys-land/in-the-code-window-the-screen-comes-and-the-cursor-stays ()
  ;; User, 2026-10-31: 弹出来，光标不动.
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (code (get-buffer-create "*keys-land-code*"))
             (run (smoke--send view "fix it")))
        (with-current-buffer code (insert "abcdef"))
        (presence--go-to code)
        (goto-char 4)
        (let ((windows (length (window-list nil 'no-minibuf))))
          (presence--ended run)
          (should (get-buffer-window superchat-changes-review-buffer-name))
          (should (eq code (window-buffer (selected-window))))
          (should (= 4 (point)))
          (keys-land--press ?X)
          (should (equal "abcXdef" (with-current-buffer code (buffer-string))))
          ;; Ending the review gives the window back.
          (select-window (get-buffer-window superchat-changes-review-buffer-name))
          (superchat-changes-review-quit)
          (should (= windows (length (window-list nil 'no-minibuf))))
          (should (get-buffer-window code)))))))

(ert-deftest keys-land/with-the-chat-closed-no-screen-comes ()
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (code (get-buffer-create "*keys-land-code*"))
             (run (smoke--send view "fix it")))
        (dolist (w (get-buffer-window-list view nil t))
          (ignore-errors (delete-window w)))
        (presence--go-to code)
        (presence--ended run)
        (should-not (get-buffer-window superchat-changes-review-buffer-name t))
        (should (eq code (window-buffer (selected-window))))))))

(ert-deftest keys-land/a-window-in-another-frame-or-tab-is-not-seen ()
  ;; Seen means a window of the selected frame.
  (let ((superchat--asked-from (make-hash-table :test #'equal))
        (place (get-buffer-create " *keys-land-place*")))
    (unwind-protect
        (progn
          (puthash "s" place superchat--asked-from)
          (should (superchat-asker-left-p "s"))
          (set-window-buffer (selected-window) place)
          (should-not (superchat-asker-left-p "s")))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (kill-buffer place))))

(ert-deftest keys-land/a-killed-place-is-left ()
  (let ((superchat--asked-from (make-hash-table :test #'equal))
        (place (generate-new-buffer " *keys-land-gone*")))
    (puthash "s" place superchat--asked-from)
    (kill-buffer place)
    (should (superchat-asker-left-p "s"))))

;;;; A system line -----------------------------------------------------------------

(defun keys-land--system-line (view text)
  (with-current-buffer view
    (let ((superchat-buffer-name (buffer-name view)))
      (superchat--insert-persistent-system-message text))))

(ert-deftest keys-land/a-system-line-leaves-the-cursor-in-the-input-area ()
  ;; Measured 2026-10-31: every key after it said "Text is read-only".
  (hm--fixture (list '(:text "ONE"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "first")
      (presence--type view "abcdef")
      (with-current-buffer view (goto-char (- (point-max) 3)))
      (keys-land--system-line view "A-SYSTEM-LINE")
      (with-current-buffer view
        (should (string-match-p "A-SYSTEM-LINE" (buffer-string))))
      (keys-land--press ?X)
      (should (equal "abcXdef" (presence--input view))))))

(ert-deftest keys-land/a-system-line-with-an-empty-input-area-leaves-it-ready ()
  (hm--fixture (list '(:text "ONE"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "first")
      (with-current-buffer view (goto-char (point-max)))
      (keys-land--system-line view "A-SYSTEM-LINE")
      (keys-land--press ?X)
      (should (equal "X" (presence--input view))))))

(ert-deftest keys-land/a-system-line-with-no-input-area-yet-ends-in-the-new-one ()
  ;; Guard: a View that gets its input area from this line, as when it opens.
  (hm--fixture (list '(:text "ONE"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "first")
      (with-current-buffer view
        (let ((superchat-buffer-name (buffer-name view)))
          (superchat--response-region-consume-prompt)
          (goto-char (point-min))))
      (keys-land--system-line view "A-SYSTEM-LINE")
      (keys-land--press ?X)
      (should (equal "X" (presence--input view))))))

(ert-deftest keys-land/a-system-line-leaves-someone-reading-above-where-they-are ()
  (hm--fixture (list '(:text "ONE-ANSWER"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "first")
      (with-current-buffer view
        (goto-char (point-min))
        (search-forward "ONE-ANSWER")
        (goto-char (match-beginning 0)))
      (keys-land--system-line view "A-SYSTEM-LINE")
      (with-current-buffer view
        (should (looking-at-p "ONE-ANSWER"))))))

(ert-deftest keys-land/an-execution-ending-leaves-the-cursor-in-the-input-area ()
  ;; Measured 2026-10-31, also on 2abee613: the end of the Run moved it out.
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((superchat-changes-review-after-run nil)
             (view (smoke--open-chat))
             (run (smoke--send view "fix it")))
        (presence--type view "abc")
        (presence--ended run)
        (keys-land--press ?X)
        (should (equal "abcX" (presence--input view)))))))

(ert-deftest keys-land/an-execution-that-only-read-leaves-it-too ()
  ;; The measured case: one read, one answer, and a line about what it changed.
  (hm--fixture (list '(:tool-uses (("read_buffer" (buffer . "hm-target"))) :delay 0.2)
                     '(:text "ANSWER"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "a question")))
      (presence--type view "abc")
      (presence--ended run)
      (keys-land--press ?X)
      (should (equal "abcX" (presence--input view))))))

(ert-deftest keys-land/an-execution-ending-with-no-edit-leaves-it-too ()
  (hm--fixture (list '(:text "JUST-AN-ANSWER" :delay 0.2))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "a question")))
      (presence--type view "abc")
      (presence--ended run)
      (keys-land--press ?X)
      (should (equal "abcX" (presence--input view))))))

;;;; A task ------------------------------------------------------------------------

(ert-deftest keys-land/after-a-task-the-cursor-comes-back-to-the-input-area ()
  ;; Measured 2026-10-31, also on 2abee613: 20 of 20 questions went into the
  ;; message the task had sent, and none reached the model.
  (wtimer--with '((:text "TASK-ANSWER") (:text "QUESTION-ANSWER")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (let ((chat (get-buffer-window view))
            (code (presence--code-window)))
        (should chat)
        (select-window code)
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (select-window chat)
        (keys-land--type "QUESTION")
        (should (equal "QUESTION" (presence--input view)))
        (let ((superchat-buffer-name (buffer-name view)))
          (with-current-buffer view (superchat-send-input)))
        (should (smoke--wait (lambda () (= 2 (length hm--wires))) 5))
        (should (cl-some (lambda (m) (string-match-p "QUESTION" (hm--text m)))
                         (hm--wire 2)))
        (with-current-buffer view
          (should-not (string-match-p "QUESTIONTASK-MARKER\\|TASK-MARKERQUESTION"
                                      (buffer-string))))
        (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 5))))))

(ert-deftest keys-land/a-task-leaves-someone-reading-above-where-they-are ()
  (wtimer--with '((:text "FIRST-ANSWER") (:text "TASK-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (chat (get-buffer-window view)))
      (hm--turn view "first")
      (let* ((session (wtimer--session view))
             (timer (superchat-timer-create session 'task "TASK-MARKER" (+ wtimer--now 60))))
        (setq wtimer--now (+ wtimer--now 60))
        (with-current-buffer view
          (goto-char (point-min))
          (search-forward "FIRST-ANSWER")
          (goto-char (match-beginning 0))
          (set-window-point chat (point)))
        (select-window (presence--code-window))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (with-current-buffer view
          (goto-char (window-point chat))
          (should (looking-at-p "FIRST-ANSWER")))))))

;;;; eval_elisp --------------------------------------------------------------------

(defconst keys-land--moving-expression
  "(with-current-buffer \"*keys-land-code*\" (goto-char (point-min)) (search-forward \"b\"))")

(defmacro keys-land--in-code (&rest body)
  "Run BODY with the selected window on a buffer whose point is at 4."
  (declare (indent 0) (debug t))
  `(let ((code (get-buffer-create "*keys-land-code*"))
         (before (window-buffer (selected-window))))
     (unwind-protect
         (progn
           (with-current-buffer code (erase-buffer) (insert "abcdef"))
           (set-window-buffer (selected-window) code)
           (set-buffer code)
           (goto-char 4)
           ,@body)
       (set-window-buffer (selected-window) before)
       (kill-buffer code))))

(defun keys-land--eval-in-run (surface expression)
  "Run the tool on EXPRESSION as a call of a Run whose surface is SURFACE."
  (let ((superchat--tool-execution-context (list :run-id "keys-land-run")))
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run) (list :surface surface))))
      (superchat-tool-eval-elisp expression))))

(ert-deftest keys-land/eval-puts-the-cursor-back-where-the-user-is ()
  ;; Measured 2026-10-31: 10 of 49 runs, `(defun foo-addumps over the lazy dog'.
  (dolist (surface '(chat subagent workflow timers))
    (ert-info ((format "%S" surface))
      (keys-land--in-code
        (keys-land--eval-in-run surface keys-land--moving-expression)
        (should (eq code (window-buffer (selected-window))))
        (should (= 4 (window-point (selected-window))))
        (keys-land--press ?X)
        (should (equal "abcXdef" (with-current-buffer code (buffer-string))))))))

(ert-deftest keys-land/eval-that-reads-the-buffer-puts-it-back-too ()
  ;; `read' moves point: found in the run e3-isolated-7.
  (keys-land--in-code
    (with-current-buffer code (erase-buffer) (insert "(a b) (c d)") (goto-char 7))
    (keys-land--eval-in-run
     'chat "(with-current-buffer \"*keys-land-code*\" (goto-char (point-min)) (read (current-buffer)))")
    (should (= 7 (window-point (selected-window))))))

(ert-deftest keys-land/eval-that-edits-before-the-cursor-keeps-it-on-the-same-text ()
  (keys-land--in-code
    (keys-land--eval-in-run
     'chat "(with-current-buffer \"*keys-land-code*\" (goto-char (point-min)) (insert \"ZZ\"))")
    (should (equal "ZZabcdef" (with-current-buffer code (buffer-string))))
    (should (= 6 (window-point (selected-window))))))

(ert-deftest keys-land/eval-that-inserts-at-the-cursor-leaves-it-after-the-text ()
  ;; What the user types next follows what the model put there.
  (keys-land--in-code
    (keys-land--eval-in-run
     'chat "(with-current-buffer \"*keys-land-code*\" (goto-char 4) (insert \"ZZ\"))")
    (should (equal "abcZZdef" (with-current-buffer code (buffer-string))))
    (should (= 6 (window-point (selected-window))))))

(ert-deftest keys-land/eval-from-a-request-in-place-may-move-the-cursor ()
  ;; The user is in that buffer and asked there.
  (dolist (surface '(inline-act inline-ask))
    (ert-info ((format "%S" surface))
      (keys-land--in-code
        (keys-land--eval-in-run surface keys-land--moving-expression)
        (should (= 3 (window-point (selected-window))))))))

(ert-deftest keys-land/eval-outside-an-execution-is-left-as-it-was ()
  (keys-land--in-code
    (let ((superchat--tool-execution-context nil))
      (superchat-tool-eval-elisp keys-land--moving-expression))
    (should (= 3 (window-point (selected-window))))))

(ert-deftest keys-land/eval-that-chooses-another-window-is-meant ()
  (keys-land--in-code
    (let* ((first (selected-window))
           (second (split-window first))
           (other (get-buffer-create "*keys-land-other*")))
      (unwind-protect
          (progn
            (set-window-buffer second other)
            (keys-land--eval-in-run
             'chat "(select-window (get-buffer-window \"*keys-land-other*\"))")
            (should (eq second (selected-window)))
            (select-window first)
            (should (= 4 (window-point first))))
        (select-window first)
        (delete-window second)
        (kill-buffer other)))))

(ert-deftest keys-land/eval-that-selects-a-window-on-the-same-buffer-is-meant ()
  (keys-land--in-code
    (let* ((first (selected-window))
           (second (split-window first)))
      (unwind-protect
          (progn
            (set-window-buffer second code)
            (set-window-point second 4)
            (keys-land--eval-in-run
             'chat (format "(progn (select-window (nth 1 (get-buffer-window-list \"*keys-land-code*\"))) (goto-char (point-min)))"))
            (should-not (eq first (selected-window)))
            (should (= 1 (point))))
        (select-window first)
        (delete-window second)))))

(ert-deftest keys-land/eval-that-shows-another-buffer-in-the-window-is-meant ()
  (keys-land--in-code
    (let ((other (get-buffer-create "*keys-land-other*")))
      (unwind-protect
          (progn
            (keys-land--eval-in-run
             'chat "(progn (with-current-buffer \"*keys-land-code*\" (goto-char 1)) (switch-to-buffer \"*keys-land-other*\"))")
            (should (eq other (window-buffer (selected-window))))
            ;; Nothing is put back in a buffer the user is no longer looking at.
            (should (= 1 (with-current-buffer code (point)))))
        (kill-buffer other)))))

(ert-deftest keys-land/eval-that-kills-the-user-s-buffer-does-not-fail ()
  (let ((code (get-buffer-create "*keys-land-doomed*"))
        (before (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) code)
          (set-buffer code)
          (should (stringp (keys-land--eval-in-run
                            'chat "(kill-buffer \"*keys-land-doomed*\")"))))
      (when (buffer-live-p code) (kill-buffer code))
      (set-window-buffer (selected-window) before))))

(ert-deftest keys-land/eval-the-window-of-a-buffer-not-selected-is-not-touched ()
  ;; Guard: Emacs keeps a window's own point for a window not selected.
  (keys-land--in-code
    (let* ((first (selected-window))
           (second (split-window first))
           (other (get-buffer-create "*keys-land-other*")))
      (unwind-protect
          (progn
            (set-window-buffer second other)
            (select-window second)
            (keys-land--eval-in-run 'chat keys-land--moving-expression)
            (should (= 4 (window-point first))))
        (select-window first)
        (delete-window second)
        (kill-buffer other)))))

(provide 'keys-land-regression)
;;; keys-land-regression.el ends here
