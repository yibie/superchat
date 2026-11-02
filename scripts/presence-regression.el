;;; presence-regression.el --- work that came by itself stays out of what the user is doing -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-the-cursor-stays-where-you-went.md:
;; a task that comes due does not take the half message in the input area, and
;; the review screen comes up by itself only for someone who asked and can still
;; see where they asked from
;; (docs/decisions/accepted/2026-10-31-what-you-type-lands-where-you-type.md).
;;
;; A batch Emacs has one frame and nobody at the keyboard: "the cursor" here is
;; `selected-window'.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/presence-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^presence/")'

;;; Code:

(load (expand-file-name "timer-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-subagent)
(require 'superchat-changes)
(require 'superchat-pending)

;;;; A half-written message --------------------------------------------------------

(defun presence--input (view)
  (with-current-buffer view
    (buffer-substring-no-properties superchat--prompt-start (point-max))))

(defun presence--type (view text)
  (with-current-buffer view
    (goto-char (point-max))
    (insert text)))

(defun presence--one-message-holds (&rest markers)
  "Non-nil when one message of the first request holds every one of MARKERS."
  (cl-some (lambda (message)
             (cl-every (lambda (marker) (string-match-p marker (hm--text message))) markers))
           (hm--wire 1)))

(defun presence--task (root text)
  "Set a task with TEXT due now under ROOT; return (VIEW SESSION TIMER)."
  (let* ((view (with-current-buffer (wtimer--file root) (superchat)))
         (session (wtimer--session view))
         (timer (superchat-timer-create session 'task text (+ wtimer--now 60))))
    (setq wtimer--now (+ wtimer--now 60))
    (list view session timer)))

(ert-deftest presence/a-task-leaves-a-half-written-message-alone ()
  ;; Measured 2026-10-31: the draft and the task went out as one message and the
  ;; input area was emptied.  A task runs in its own conversation; there someone
  ;; can be writing too, and it waits.
  (wtimer--with '((:text "ONE") (:text "TWO")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let* ((home (plist-get (superchat-timer-fire timer due) :session-id))
             (home-view (superchat-timer--live-view home)))
        (wtimer--settled)
        (presence--type home-view "DRAFT-MARKER half a thou")
        (setq wtimer--now (+ wtimer--now 3600))
        (let ((fired (superchat-timer-fire timer (+ due 3600))))
          (should-not (plist-get fired :run-id)))
        (should (= 1 (length hm--wires)))
        (should (equal "DRAFT-MARKER half a thou" (presence--input home-view)))
        ;; It waits on the record, as a task does in a busy conversation.
        (should (= 1 (length (superchat-timer-unstarted-tasks))))))))

(ert-deftest presence/the-task-starts-once-the-message-is-gone ()
  (wtimer--with '((:text "ONE") (:text "TWO")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let* ((home (plist-get (superchat-timer-fire timer due) :session-id))
             (home-view (superchat-timer--live-view home)))
        (wtimer--settled)
        (presence--type home-view "DRAFT-MARKER")
        (setq wtimer--now (+ wtimer--now 3600))
        (superchat-timer-fire timer (+ due 3600))
        (should (= 1 (length hm--wires)))
        (with-current-buffer home-view
          (let ((inhibit-read-only t))
            (delete-region superchat--prompt-start (point-max))))
        (setq wtimer--now (+ wtimer--now superchat-timer-busy-retry-seconds 1))
        (superchat-timer-retry-tasks)
        (wtimer--settled)
        (should (= 2 (length hm--wires)))
        (should-not (cl-some (lambda (message)
                               (string-match-p "DRAFT-MARKER" (hm--text message)))
                             (hm--wire 2)))))))

(ert-deftest presence/a-task-starts-as-before-with-an-empty-input-area ()
  ;; Guard: green before and after.
  (wtimer--with '((:text "ANSWER")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((fired (superchat-timer-fire timer (plist-get timer :due))))
        (should (stringp (plist-get fired :run-id))))
      (wtimer--settled)
      (should (presence--one-message-holds "TASK-MARKER")))))

;;;; The review screen -------------------------------------------------------------

(defmacro presence--with-file (name-var &rest body)
  "Run BODY with NAME-VAR the name of a buffer visiting a fresh file."
  (declare (indent 1) (debug t))
  (let ((dir (make-symbol "dir")) (buffer (make-symbol "buffer")))
    `(let* ((,dir (file-name-as-directory (make-temp-file "presence-" t)))
            (,buffer (progn
                       (with-temp-file (expand-file-name "a.txt" ,dir) (insert "hello\n"))
                       (find-file-noselect (expand-file-name "a.txt" ,dir))))
            (,name-var (buffer-name ,buffer)))
       (unwind-protect (progn ,@body)
         (when-let* ((review (get-buffer superchat-changes-review-buffer-name)))
           (let ((kill-buffer-query-functions nil)) (kill-buffer review)))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer (set-buffer-modified-p nil))
           (kill-buffer ,buffer))
         (delete-directory ,dir t)))))

(defun presence--edit-round (name &optional delay)
  `(:tool-uses (("edit_buffer" (buffer_name . ,name)
                 (old_string . "hello") (new_string . "HELLO")))
               ,@(when delay (list :delay delay))))

(defun presence--selected ()
  (buffer-name (window-buffer (selected-window))))

(defun presence--code-window ()
  (cl-find-if-not (lambda (w) (window-parameter w 'window-side))
                  (window-list nil 'no-minibuf)))

(defun presence--go-to (buffer)
  "The user goes to BUFFER in the code window."
  (let ((code (presence--code-window)))
    (select-window code)
    (set-window-buffer code buffer)
    (set-buffer buffer)))

(defun presence--ended (run)
  (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
  ;; The screen is shown from a timer once the execution has ended.
  (sit-for 0.1))

(ert-deftest presence/whoever-waits-where-they-asked-gets-the-screen ()
  ;; Guard: green before and after.  User, 2026-09-25: the cursor goes to it.
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "fix it")))
        (presence--ended run)
        (should (equal superchat-changes-review-buffer-name (presence--selected)))))))

(ert-deftest presence/whoever-went-to-another-window-keeps-the-cursor ()
  ;; The chat is still beside them, so the screen comes up: 2026-10-31,
  ;; docs/decisions/accepted/2026-10-31-what-you-type-lands-where-you-type.md.
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (elsewhere (get-buffer-create "*presence-elsewhere*"))
             (run (smoke--send view "fix it")))
        (presence--go-to elsewhere)
        (presence--ended run)
        (should (equal "*presence-elsewhere*" (presence--selected)))
        (should (get-buffer-window superchat-changes-review-buffer-name t))
        ;; What it left is not lost: the list says so.
        (should (cl-some (lambda (item) (eq 'unsaved (plist-get item :kind)))
                         (superchat-pending-items)))))))

(ert-deftest presence/whoever-closed-the-chat-window-keeps-the-cursor ()
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (elsewhere (get-buffer-create "*presence-elsewhere*"))
             (run (smoke--send view "do the long job")))
        (dolist (w (get-buffer-window-list view nil t))
          (ignore-errors (delete-window w)))
        (presence--go-to elsewhere)
        (presence--ended run)
        (should (equal "*presence-elsewhere*" (presence--selected)))
        (should-not (get-buffer-window superchat-changes-review-buffer-name t))))))

(ert-deftest presence/another-buffer-in-the-same-window-is-going-away-too ()
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.3) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (elsewhere (get-buffer-create "*presence-elsewhere*"))
             (run (smoke--send view "fix it")))
        (set-window-buffer (selected-window) elsewhere)
        (presence--ended run)
        (should-not (get-buffer-window superchat-changes-review-buffer-name t))))))

(ert-deftest presence/the-same-buffer-in-another-window-is-still-there ()
  ;; Switching between editor mode and agent mode is not going away.
  (let* ((superchat--asked-from (make-hash-table :test #'equal))
         (buffer (get-buffer-create " *presence-same*"))
         (first (selected-window))
         (second (split-window first)))
    (unwind-protect
        (progn
          (set-window-buffer first buffer)
          (set-window-buffer second buffer)
          (select-window first)
          (superchat--note-asked-from "s")
          (set-window-buffer first (get-buffer-create "*scratch*"))
          (select-window second)
          (should-not (superchat-asker-left-p "s")))
      (select-window first)
      (delete-window second)
      (kill-buffer buffer))))

(ert-deftest presence/a-message-superchat-sends-is-not-the-user-asking ()
  (hm--fixture (list '(:text "ANSWER"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (here (window-buffer (selected-window)))
           (elsewhere (get-buffer-create "*presence-elsewhere*")))
      (puthash session elsewhere superchat--asked-from)
      (with-current-buffer view
        (goto-char (point-max))
        (insert "NOBODY-TYPED-THIS")
        (let ((superchat-automated-input 'timers))
          (superchat-send-input)))
      (should (eq elsewhere (gethash session superchat--asked-from)))
      (should-not (eq here elsewhere))
      (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 5)))))

(ert-deftest presence/coming-back-before-it-ends-counts-as-waiting ()
  (presence--with-file name
    (hm--fixture (list (presence--edit-round name 0.5) '(:text "DONE"))
      (let* ((view (smoke--open-chat))
             (asked-from (selected-window))
             (elsewhere (get-buffer-create "*presence-elsewhere*"))
             (run (smoke--send view "fix it")))
        (presence--go-to elsewhere)
        (sit-for 0.05)
        (select-window asked-from)
        (presence--ended run)
        (should (equal superchat-changes-review-buffer-name (presence--selected)))))))

(ert-deftest presence/a-background-report-does-not-take-the-cursor ()
  ;; Measured 2026-10-31 on the first version of background delegation.
  (presence--with-file name
    (hm--fixture (list '(:tool-uses (("delegate_to_subagent"
                                      (preset . "researcher")
                                      (task . "SUB-TASK-MARKER look into it")
                                      (background . t))))
                       '(:text "SUB-REPORT-MARKER" :delay 0.4)
                       '(:text "PARENT-CARRIES-ON")
                       (presence--edit-round name)
                       '(:text "REPORT-READ"))
      (setq superchat-llm-tool-names
            (append superchat-llm-tool-names '("delegate_to_subagent")))
      (setq superchat-llm-tools-list nil)
      (let* ((superchat--subagent-running nil)
             (view (smoke--open-chat))
             (run (smoke--send view "please look into it")))
        ;; The user asked and stayed: still it is not an answer they wait for.
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
        (should (smoke--wait
                 (lambda ()
                   (and (null superchat--subagent-running)
                        (null (superchat-runtime-active-runs))
                        (zerop (superchat-runtime-pending-follow-ups (hm--session view)))))
                 8))
        (sit-for 0.1)
        (should-not (get-buffer-window superchat-changes-review-buffer-name t))
        (should (cl-some (lambda (item) (eq 'unsaved (plist-get item :kind)))
                         (superchat-pending-items)))))))

(ert-deftest presence/the-minibuffer-is-not-going-away ()
  ;; The buffer they asked from is still shown.
  (let* ((session "presence-session")
         (superchat--asked-from (make-hash-table :test #'equal)))
    (superchat--note-asked-from session)
    (should-not (superchat-asker-left-p session))
    (cl-letf (((symbol-function 'selected-window) (lambda () (minibuffer-window))))
      (should-not (superchat-asker-left-p session)))))

(ert-deftest presence/only-a-conversation-s-execution-is-judged-by-it ()
  ;; A stale place from an earlier message says nothing about a request made
  ;; from a buffer in place: its screen stays as it was.
  (let ((superchat--asked-from (make-hash-table :test #'equal)))
    (puthash "s" (get-buffer-create " *presence-asked-from-here*") superchat--asked-from)
    (should (superchat-asker-left-p "s"))
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run) (list :surface 'inline-act :actor-id "lead"))))
      (should-not (superchat-changes--nobody-waits-p "s" "r")))
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run) (list :surface 'chat :actor-id "lead"))))
      (should (superchat-changes--nobody-waits-p "s" "r")))))

(ert-deftest presence/a-send-from-the-request-inspector-is-not-judged ()
  ;; The inspector closes on an accepted send, which is no sign of leaving.
  (hm--fixture (list '(:text "DONE"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view)))
      (superchat--note-asked-from session)
      (should (gethash session superchat--asked-from))
      (with-current-buffer view
        (let ((plan (superchat-build-request-plan "INSPECTED" session)))
          (cl-letf (((symbol-function 'superchat--request-plan-with-edited-prompt)
                     (lambda () plan))
                    ((symbol-function 'superchat--ensure-request-chat-buffer)
                     (lambda () view)))
            (superchat-request-send))))
      (should-not (gethash session superchat--asked-from))
      (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 5)))))

(provide 'presence-regression)
;;; presence-regression.el ends here
