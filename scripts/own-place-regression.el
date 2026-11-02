;;; own-place-regression.el --- work nobody asked for has its own place -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-work-nobody-asked-for-has-its-own-place.md:
;; a scheduled task runs in a conversation of its own, a conversation nobody
;; opened is not the Workspace's main one and is not recalled from, and what such
;; work concluded is a line in the butler and a line in the conversation in use.
;;
;; A batch Emacs has one frame and nobody at the keyboard: "the cursor" here is
;; `selected-window'.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/own-place-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^own-place/")'

;;; Code:

(load (expand-file-name "presence-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-work)
(require 'superchat-pending)
(require 'superchat-butler nil t)

(defun own-place--rows (session)
  "How many rows SESSION has on the Tape."
  (caar (superchat-db-tape-select
         "SELECT COUNT(*) FROM tape WHERE session_id = ?" (list session))))

(defun own-place--workspace (view)
  (buffer-local-value 'superchat--workspace-binding view))

(defun own-place--background (view)
  "A conversation nobody opened in VIEW's Workspace, with a row newer than VIEW's."
  (let* ((other (superchat-workspace-new-conversation (own-place--workspace view) t))
         (session (hm--session other)))
    (superchat-tape-append-event
     (list :type "run.seen" :session-id session :actor-id "test" :summary "newest"
           :payload (list :run-id "none")))
    other))

(defun own-place--text (view)
  (with-current-buffer view (buffer-string)))

(defun own-place--joined (view)
  "VIEW's text with its lines joined: a doorbell is broken to fit the window."
  (replace-regexp-in-string "\n" " " (own-place--text view)))

(defun own-place--kill (view)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer view)))

;;;; A conversation nobody opened ---------------------------------------------------

(ert-deftest own-place/a-background-conversation-is-not-the-main-one ()
  ;; Measured 2026-10-31 (B4): a woken conversation was the main one while it ran.
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (main (hm--session view))
           (other (own-place--background view))
           (id (plist-get (own-place--workspace view) :id)))
      (should (equal (list main) (superchat-view-workspace-session-ids id)))
      (should (eq view (superchat--workspace-main-view (own-place--workspace view))))
      (should-not (equal (hm--session other) main)))))

(ert-deftest own-place/a-file-of-the-project-still-goes-to-the-main-conversation ()
  (wtimer--with nil (p)
    (let* ((file (wtimer--file p))
           (view (with-current-buffer file (superchat)))
           (main (hm--session view))
           (other (own-place--background view)))
      (should-not (equal main (hm--session other)))
      (should (equal main (with-current-buffer file
                            (superchat-conversation-session-id))))
      ;; The window beside the code was not switched to the other one.
      (should (eq view (window-buffer (get-buffer-window view)))))))

(ert-deftest own-place/recall-does-not-reach-into-a-background-conversation ()
  ;; Measured 2026-10-31 (B4): its text came back in the main conversation's recall.
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (main (hm--session view))
           (session (hm--session (own-place--background view))))
      (should (equal (list main) (superchat-view-recall-session-ids main)))
      ;; The other way round it may: the work needs what the user said.
      (should (equal (sort (list main session) #'string<)
                     (sort (superchat-view-recall-session-ids session) #'string<))))))

(ert-deftest own-place/it-stays-in-the-background-when-read-back-from-the-tape ()
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (main (hm--session view))
           (other (own-place--background view))
           (session (hm--session other))
           (id (plist-get (own-place--workspace view) :id)))
      (own-place--kill other)
      (should (buffer-live-p (superchat--render-session-view session)))
      (should (equal (list main) (superchat-view-workspace-session-ids id))))))

(ert-deftest own-place/saying-something-there-does-not-make-it-the-main-one ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (main (hm--session view))
           (other (own-place--background view))
           (id (plist-get (own-place--workspace view) :id)))
      (hm--turn other "a question")
      (should (equal (list main) (superchat-view-workspace-session-ids id))))))

(ert-deftest own-place/a-workspace-with-only-background-conversations-gets-a-main-one ()
  (wtimer--with nil (p)
    (let* ((file (wtimer--file p))
           (view (with-current-buffer file (superchat)))
           (workspace (own-place--workspace view))
           (first (superchat-workspace-new-conversation workspace t))
           (id (plist-get workspace :id)))
      ;; Everything else in this Workspace goes away from the record.
      (with-current-buffer view (superchat--apply-workspace-binding nil))
      (should-not (superchat-view-workspace-session-ids id))
      (let ((main (superchat--workspace-main-view workspace)))
        (should-not (eq first main))
        (should (equal (list (hm--session main))
                       (superchat-view-workspace-session-ids id)))))))

(ert-deftest own-place/an-ordinary-new-conversation-is-still-the-main-one ()
  ;; Guard: green before and after.
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (other (superchat-workspace-new-conversation (own-place--workspace view)))
           (id (plist-get (own-place--workspace view) :id)))
      (should (equal (hm--session other)
                     (car (superchat-view-workspace-session-ids id)))))))

;;;; A scheduled task -----------------------------------------------------------------

(ert-deftest own-place/a-task-runs-in-a-conversation-of-its-own ()
  ;; Measured 2026-10-31: 30 of 45 of the user's requests carried the briefing.
  (wtimer--with '((:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
      (let* ((rows (own-place--rows session))
             (fired (superchat-timer-fire timer (plist-get timer :due)))
             (home (plist-get fired :session-id)))
        (should (stringp (plist-get fired :run-id)))
        (should-not (equal session home))
        (wtimer--settled)
        (should (presence--one-message-holds "TASK-MARKER"))
        (should (hm--events home "run.completed"))
        ;; Nothing of it is on the main conversation's Tape.
        (should (= rows (own-place--rows session)))
        (should-not (hm--events session "timer.fired"))
        (should-not (hm--events session "run.started"))))))

(ert-deftest own-place/the-main-conversation-stays-the-main-one-while-a-task-runs ()
  ;; Measured 2026-10-31 (B4), with a wake: in-place commands went to it.
  (wtimer--with '((:text "TASK-ANSWER" :delay 0.3)) (p)
    (cl-destructuring-bind (view session timer) (presence--task p "TASK-MARKER")
      (let ((file (get-file-buffer (expand-file-name "a.txt" p))))
        (superchat-timer-fire timer (plist-get timer :due))
        (should (superchat-runtime-active-runs))
        (should (equal session (with-current-buffer file
                                 (superchat-conversation-session-id))))
        (should (eq view (superchat--workspace-main-view (own-place--workspace view))))
        (wtimer--settled)
        (should (equal session (with-current-buffer file
                                 (superchat-conversation-session-id))))))))

(ert-deftest own-place/a-half-written-message-holds-nothing-up-and-is-left-alone ()
  ;; Measured 2026-10-31 (P1): it went out with the task.  Then a task waited for it.
  (wtimer--with '((:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (presence--type view "DRAFT-MARKER half a thou")
      (should (stringp (plist-get (superchat-timer-fire timer (plist-get timer :due))
                                  :run-id)))
      (wtimer--settled)
      (should (equal "DRAFT-MARKER half a thou" (presence--input view)))
      (should-not (presence--one-message-holds "DRAFT-MARKER")))))

(ert-deftest own-place/a-task-leaves-the-windows-and-the-cursor-alone ()
  (wtimer--with '((:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((selected (selected-window))
            (windows (window-list))
            (buffers (mapcar #'window-buffer (window-list))))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (should (eq selected (selected-window)))
        (should (equal windows (window-list)))
        (should (equal buffers (mapcar #'window-buffer (window-list))))))))

(ert-deftest own-place/every-occurrence-runs-in-the-same-conversation ()
  (wtimer--with '((:text "ONE-ANSWER") (:text "TWO-ANSWER") (:text "THREE-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let ((home (plist-get (superchat-timer-fire timer due) :session-id)))
        (wtimer--settled)
        (setq wtimer--now (+ wtimer--now 3600))
        (should (equal home (plist-get (superchat-timer-fire timer (+ due 3600)) :session-id)))
        (wtimer--settled)
        ;; An Emacs that started again finds it on the Tape.
        (own-place--kill (superchat-timer--live-view home))
        (setq wtimer--now (+ wtimer--now 3600))
        (should (equal home (plist-get (superchat-timer-fire timer (+ due 7200)) :session-id)))
        (wtimer--settled)
        ;; Its history is the earlier results.
        (should (cl-some (lambda (m) (string-match-p "ONE-ANSWER" (hm--text m)))
                         (hm--wire 3)))))))

(ert-deftest own-place/two-timers-have-two-conversations ()
  (wtimer--with '((:text "ONE-ANSWER") (:text "TWO-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (hm--session view))
           (one (superchat-timer-create session 'task "ONE" (+ wtimer--now 60)))
           (two (superchat-timer-create session 'task "TWO" (+ wtimer--now 60))))
      (setq wtimer--now (+ wtimer--now 60))
      (let ((first (superchat-timer-fire one (plist-get one :due))))
        (wtimer--settled)
        (should-not (equal (plist-get first :session-id)
                           (plist-get (superchat-timer-fire two (plist-get two :due))
                                      :session-id)))
        (wtimer--settled)))))

(ert-deftest own-place/an-occurrence-waits-for-the-one-before-it ()
  (wtimer--with '((:text "ONE-ANSWER" :delay 0.4) (:text "TWO-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (superchat-timer-fire timer due)
      (should-not (plist-get (superchat-timer-fire timer (+ due 3600)) :run-id))
      (should (= 1 (length (superchat-timer-unstarted-tasks))))
      (wtimer--settled)
      (setq wtimer--now (+ wtimer--now superchat-timer-busy-retry-seconds 1))
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 2 (length hm--wires)))
      (should-not (superchat-timer-unstarted-tasks)))))

(ert-deftest own-place/someone-writing-in-its-conversation-holds-it-up ()
  (wtimer--with '((:text "ONE-ANSWER") (:text "TWO-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let ((home (plist-get (superchat-timer-fire timer due) :session-id)))
        (wtimer--settled)
        (presence--type (superchat-timer--live-view home) "DRAFT-MARKER")
        (should-not (plist-get (superchat-timer-fire timer (+ due 3600)) :run-id))
        (should (= 1 (length hm--wires)))
        (should (equal "DRAFT-MARKER" (presence--input (superchat-timer--live-view home))))))))

(ert-deftest own-place/a-reminder-still-speaks-in-the-main-conversation ()
  ;; T2 is not changed.
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (hm--session view))
           (timer (superchat-timer-create session 'reminder "REMINDER-MARKER"
                                          (+ wtimer--now 60))))
      (setq wtimer--now (+ wtimer--now 60))
      (should (equal session (plist-get (superchat-timer-fire timer (plist-get timer :due))
                                        :session-id)))
      (should (equal '("REMINDER-MARKER") (wtimer--fired-in session)))
      (should (string-match-p "REMINDER-MARKER" (own-place--text view)))
      (should (= 0 (length hm--wires))))))

(ert-deftest own-place/a-task-set-in-a-temporary-conversation-has-its-own-too ()
  (wtimer--with '((:text "TASK-ANSWER")) (_p)
    (let* ((view (superchat--temporary-view))
           (session (hm--session view))
           (timer (superchat-timer-create session 'task "TASK-MARKER" (+ wtimer--now 60))))
      (setq wtimer--now (+ wtimer--now 60))
      (let* ((rows (own-place--rows session))
             (home (plist-get (superchat-timer-fire timer (plist-get timer :due))
                              :session-id)))
        (wtimer--settled)
        (should-not (equal session home))
        (should (= rows (own-place--rows session)))
        ;; It is not taken for the user's temporary conversation.
        (should-not (superchat--view-temporary-p (superchat-timer--live-view home)))
        (should (eq view (superchat--temporary-view)))))))

(ert-deftest own-place/a-task-reads-whatever-level-it-was-set-at ()
  ;; Measured 2026-11-01: a task edited the buffer of a file, and the user's model
  ;; read that back as the old value and answered wrongly (1 run of 5).
  (dolist (level '(edit-buffers auto))
    (wtimer--with '((:text "TASK-ANSWER")) (p)
      (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
        (superchat-permission-set session level t)
        (let ((home (plist-get (superchat-timer-fire timer (plist-get timer :due)) :session-id)))
          (should (eq 'read-only (superchat-sandbox-effective-mode home)))
          (wtimer--settled))))))

(ert-deftest own-place/a-task-changes-no-buffer-and-asks-nothing ()
  (wtimer--with '((:tool-uses (("edit_buffer" (buffer_name . "note.txt") (old_text . "OLD")
                                (new_text . "NEW"))))
                  (:tool-uses (("eval_elisp" (expression . "(setq own-place--ran t)"))))
                  (:text "TASK-ANSWER"))
      (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let* ((note (get-buffer-create "note.txt"))
             (asked nil)
             (superchat-approval-function (lambda (request) (push request asked) t)))
        (defvar own-place--ran nil)
        (setq own-place--ran nil)
        (with-current-buffer note (erase-buffer) (insert "OLD"))
        (unwind-protect
            (progn
              (own-place--fire-and-settle timer)
              (should (equal "OLD" (with-current-buffer note (buffer-string))))
              (should-not own-place--ran)
              (should-not asked))
          (kill-buffer note))))))

(ert-deftest own-place/a-level-chosen-in-a-tasks-conversation-lasts ()
  ;; The way to let one task change things: its conversation is the place.
  (wtimer--with '((:text "ONE") (:text "TWO")) (p)
    (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
      (let* ((due (plist-get timer :due))
             (home (plist-get (superchat-timer-fire timer due) :session-id)))
        (wtimer--settled)
        (superchat-permission-set home 'edit-buffers)
        (should (equal home (plist-get (superchat-timer-fire timer (+ due 3600)) :session-id)))
        (wtimer--settled)
        (should (eq 'edit-buffers (superchat-permission-current home)))
        (ignore session)))))

(ert-deftest own-place/the-level-of-tasks-can-be-configured ()
  (wtimer--with '((:text "ONE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let* ((superchat-timer-task-level 'edit-buffers)
             (home (plist-get (superchat-timer-fire timer (plist-get timer :due)) :session-id)))
        (should (eq 'edit-buffers (superchat-permission-current home)))
        (wtimer--settled))))
  ;; Nil is the level of the conversation it was set in.
  (wtimer--with '((:text "ONE")) (p)
    (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
      (superchat-permission-set session 'auto t)
      (let* ((superchat-timer-task-level nil)
             (home (plist-get (superchat-timer-fire timer (plist-get timer :due)) :session-id)))
        (should (eq 'auto (superchat-permission-current home)))
        (wtimer--settled)))))

(ert-deftest own-place/the-model-is-told-a-task-only-reads ()
  (wtimer--with (list '(:tool-uses (("timer_create" (text . "LOOK AT IT") (kind . "task")
                                     (in_minutes . 60))))
                      '(:text "finished"))
      (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (hm--turn view "later, look at it")
      ;; When it chooses the kind, and in what it can tell the user.
      (should (string-match-p
               "starts you on the text[^.]*read-only"
               (llm-tool-description
                (cl-find "timer_create" superchat-llm-tools-list
                         :key #'llm-tool-name :test #'equal))))
      (should (string-match-p "the task will run at .*It runs read-only"
                              (prin1-to-string hm--wires))))))

(ert-deftest own-place/a-wake-does-not-only-read ()
  ;; A record is work the user started and came back to; K10's conversation keeps
  ;; the level a new conversation has.
  (wtimer--with '((:text "WOKEN-ANSWER")) (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (own-place--with-due-record view "A goal"
        (ignore main)
        (let ((woken (car (superchat-work-wake-due))))
          (should-not (eq 'read-only (superchat-sandbox-effective-mode (hm--session woken))))
          (wtimer--settled))))))

(ert-deftest own-place/a-question-in-the-background-holds-up-nothing-the-user-does ()
  ;; Measured 2026-10-31 (P2): the user's question waited behind the approval.
  (wtimer--with '((:tool-uses (("eval_elisp" (expression . "1"))))
                  (:text "MAIN-ANSWER")
                  (:text "TASK-ANSWER"))
      (p)
    (cl-destructuring-bind (view session timer) (presence--task p "TASK-MARKER")
      (superchat-permission-set session 'edit-buffers t)
      ;; The fixture answers every question; here it is published, as for a
      ;; person.  In batch the question's wait polls and timers run in it, so what
      ;; the user does meanwhile is on timers.  Only a task someone let do more
      ;; asks at all.
      (let ((superchat-timer-task-level 'edit-buffers)
            (superchat-approval-function #'superchat-approval-publish)
            (open-when-sent nil) (main-run nil) (done-while-open nil))
        (run-at-time 0.2 nil (lambda ()
                               (setq open-when-sent (superchat-approval-open-questions)
                                     main-run (smoke--send view "MAIN-QUESTION"))))
        (run-at-time 0.8 nil (lambda ()
                               (setq done-while-open
                                     (and main-run (smoke--run-done-p main-run)))
                               (superchat-approval-answer
                                (plist-get (car (superchat-approval-open-questions)) :handle)
                                nil)))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (should open-when-sent)
        (should done-while-open)
        (should (eq 'completed (plist-get (superchat-runtime-status main-run) :status)))
        (should (= 3 (length hm--wires)))))))

(ert-deftest own-place/a-question-in-the-background-does-not-make-the-user-s-unclear ()
  ;; Measured 2026-11-01, real keys: with the task's question open too,
  ;; M-x superchat-approval-allow from the code window only said two were open, and
  ;; 3 of 5 runs waited on the user's own question until the script gave up.
  (wtimer--with nil (p)
    (let* ((file (wtimer--file p))
           (view (with-current-buffer file (superchat)))
           (other (own-place--background view))
           (another (own-place--background view))
           (mine (list :handle 'mine :session-id (hm--session view)))
           (theirs (list :handle 'theirs :session-id (hm--session other)))
           (others (list :handle 'others :session-id (hm--session another)))
           (open nil))
      (cl-letf (((symbol-function 'superchat-approval-open-questions) (lambda () open)))
        (setq open (list theirs mine))
        (with-current-buffer file
          (should (eq 'mine (plist-get (superchat-approval--question-here) :handle))))
        ;; In its own conversation, it is that one.
        (with-current-buffer other
          (should (eq 'theirs (plist-get (superchat-approval--question-here) :handle))))
        ;; The only one open is meant, as before.
        (setq open (list theirs))
        (with-current-buffer file
          (should (eq 'theirs (plist-get (superchat-approval--question-here) :handle))))
        ;; Two nobody opened: which one is not for the program to guess.
        (setq open (list theirs others))
        (with-current-buffer file
          (should-error (superchat-approval--question-here) :type 'user-error))
        ;; Two of the user's: neither.
        (let* ((second (superchat-workspace-new-conversation (own-place--workspace view))))
          (setq open (list mine (list :handle 'second :session-id (hm--session second))))
          (with-current-buffer file
            (should-error (superchat-approval--question-here) :type 'user-error)))))))

(ert-deftest own-place/what-a-task-wrote-down-stays-out-of-the-main-conversation ()
  ;; Measured 2026-11-01, real DeepSeek: the task started a work record, and its goal
  ;; went into 6 of the user's 10 requests.
  (wtimer--with '((:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
      (let ((home (plist-get (own-place--fire-and-settle timer) :session-id)))
        (superchat-work-start home "BACKGROUND-GOAL")
        ;; So the first is one of the task's other records, not its current one.
        (superchat-work-start home "ANOTHER-GOAL")
        (superchat-work-start session "MAIN-GOAL")
        (let ((main-text (superchat-work-request-text session))
              (home-text (superchat-work-request-text home)))
          (should-not (string-match-p "BACKGROUND-GOAL" (or main-text "")))
          (should (string-match-p "MAIN-GOAL" main-text))
          (should (string-match-p "BACKGROUND-GOAL" home-text))
          (should (string-match-p "MAIN-GOAL" home-text)))
        ;; The user can still see it.
        (should (cl-find "BACKGROUND-GOAL" (superchat-pending-items)
                         :key (lambda (item) (plist-get item :what)) :test #'equal))))))

(defun own-place--tool-results (n)
  (mapcar #'hm--text
          (cl-remove-if-not (lambda (m) (equal "tool" (alist-get 'role m)))
                            (hm--body (hm--wire n)))))

(ert-deftest own-place/a-task-does-not-ask-when-nobody-has-its-conversation-open ()
  ;; Measured 2026-11-01, real DeepSeek: 1 of 5 runs asked, nobody could see it, and
  ;; every later occurrence waited behind it until the end of the morning.
  (wtimer--with '((:tool-uses (("ask_user" (question . "WHICH-ONE?"))))
                  (:text "TASK-ANSWER"))
      (p)
    (setq superchat-llm-tool-names (append superchat-llm-tool-names '("ask_user")))
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((fired (own-place--fire-and-settle timer)))
        (should (hm--events (plist-get fired :session-id) "run.completed"))
        (should (= 2 (length hm--wires)))
        (should (cl-some (lambda (text)
                           (string-match-p "The user cannot be asked: nobody has this conversation open"
                                           text))
                         (own-place--tool-results 2)))
        (should (equal "TASK-ANSWER" (plist-get (car (own-place--lines)) :text)))))))

(ert-deftest own-place/someone-looking-at-its-conversation-can-be-asked ()
  (wtimer--with '((:text "ONE-ANSWER")
                  (:tool-uses (("ask_user" (question . "WHICH-ONE?"))))
                  (:text "TWO-ANSWER"))
      (p)
    (setq superchat-llm-tool-names (append superchat-llm-tool-names '("ask_user")))
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let ((home (plist-get (superchat-timer-fire timer due) :session-id)))
        (wtimer--settled)
        (set-window-buffer (get-buffer-window view) (superchat-timer--live-view home))
        (setq wtimer--now (+ wtimer--now 3600))
        (let ((run (plist-get (superchat-timer-fire timer (+ due 3600)) :run-id))
              pending)
          (should (smoke--wait
                   (lambda () (setq pending (superchat-runtime-pending-input run))) 10))
          (should (superchat-runtime-answer-input run (plist-get pending :handle) "this"))
          (wtimer--settled)
          (should (= 3 (length hm--wires))))))))

;;;; The butler --------------------------------------------------------------------------

(defun own-place--lines ()
  (superchat-butler-lines))

(defun own-place--fire-and-settle (timer)
  (prog1 (superchat-timer-fire timer (plist-get timer :due))
    (wtimer--settled)))

(ert-deftest own-place/a-task-s-end-is-one-line-in-the-butler ()
  (wtimer--with '((:text "FIRST-LINE\n\nmore of the answer")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let* ((fired (own-place--fire-and-settle timer))
             (lines (own-place--lines))
             (line (car lines)))
        (should (= 1 (length lines)))
        (should (equal (plist-get fired :run-id) (plist-get line :run)))
        (should (equal (plist-get fired :session-id) (plist-get line :session)))
        (should (equal "FIRST-LINE" (plist-get line :text)))
        (should (eq 'completed (plist-get line :status)))
        (should (string-match-p "TASK-MARKER" (plist-get line :what)))
        (with-current-buffer (superchat-butler)
          (should (string-match-p "TASK-MARKER.*completed.*FIRST-LINE" (buffer-string)))
          (should-not (string-match-p "more of the answer" (buffer-string)))
          (should buffer-read-only))))))

(ert-deftest own-place/a-long-answer-is-cut-to-one-line ()
  (wtimer--with `((:text ,(make-string 2000 ?x))) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (own-place--fire-and-settle timer)
      (should (< (length (plist-get (car (own-place--lines)) :text)) 300)))))

(ert-deftest own-place/the-butler-is-there-after-a-restart ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (own-place--fire-and-settle timer)
      (superchat-butler)
      (own-place--kill (get-buffer superchat-butler-buffer-name))
      (with-current-buffer (superchat-butler)
        (should (string-match-p "FIRST-LINE" (buffer-string)))))))

(ert-deftest own-place/an-open-butler-shows-a-new-line-by-itself ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((butler (superchat-butler)))
        (own-place--fire-and-settle timer)
        (should (string-match-p "FIRST-LINE" (own-place--text butler)))))))

(ert-deftest own-place/a-failed-task-says-so-in-the-butler ()
  (wtimer--with '((:error (error . "BOOM-FROM-THE-SERVER"))) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (own-place--fire-and-settle timer)
      (let ((line (car (own-place--lines))))
        (should (eq 'failed (plist-get line :status)))
        (should (string-match-p "failed.*BOOM-FROM-THE-SERVER"
                                (superchat-butler-line-text line)))))))

(ert-deftest own-place/what-the-user-started-leaves-no-line ()
  (wtimer--with '((:text "USER-ANSWER")) (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (hm--turn view "a question")
      (wtimer--settled)
      (should-not (own-place--lines))
      (should-not (string-match-p "Butler:" (own-place--text view))))))

(ert-deftest own-place/the-butler-goes-to-the-conversation-and-counts-it-as-seen ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let* ((fired (own-place--fire-and-settle timer))
             (run (plist-get fired :run-id)))
        (should (cl-find run superchat-changes--notices
                         :key (lambda (n) (plist-get n :run)) :test #'equal))
        (pop-to-buffer (superchat-butler))
        (goto-char (point-min))
        (search-forward "FIRST-LINE")
        (superchat-butler-open)
        (should (eq (window-buffer (selected-window))
                    (superchat-timer--live-view (plist-get fired :session-id))))
        (should-not (cl-find run superchat-changes--notices
                             :key (lambda (n) (plist-get n :run)) :test #'equal))))))

(ert-deftest own-place/the-butler-is-not-a-session-the-user-can-open ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (own-place--fire-and-settle timer)
      (should (own-place--lines))
      (should-not (member superchat-butler-session-id (superchat-view-session-ids))))))

(ert-deftest own-place/what-an-earlier-emacs-left-gets-a-line-when-superchat-starts ()
  ;; A task cut off when Emacs quit has no end to write a line at.
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((fired (own-place--fire-and-settle timer)))
        (superchat-tape-append-event
         (list :type "run.started" :session-id (plist-get fired :session-id)
               :run-id "run-of-an-earlier-emacs" :turn-id "turn-of-an-earlier-emacs"
               :actor-id "timers" :summary "run.started"
               :payload (list :run-id "run-of-an-earlier-emacs")))
        (superchat-changes-notices-reload)
        (superchat-butler-catch-up)
        (superchat-butler-catch-up)
        (let ((line (cl-find "run-of-an-earlier-emacs" (own-place--lines)
                             :key (lambda (l) (plist-get l :run)) :test #'equal)))
          (should line)
          (should (eq 'cut-off (plist-get line :status))))
        (should (= 2 (length (own-place--lines))))))))

(ert-deftest own-place/the-butler-buffer-is-there-from-the-start ()
  (wtimer--with nil (_p)
    (when-let* ((buffer (get-buffer superchat-butler-buffer-name)))
      (own-place--kill buffer))
    (superchat-plugin-unmount 'butler)
    (superchat-plugin-mount 'butler)
    (should (get-buffer superchat-butler-buffer-name))
    (should-not (get-buffer-window superchat-butler-buffer-name t))))

(ert-deftest own-place/an-occurrence-does-not-say-the-last-one-s-answer ()
  ;; Its conversation holds every earlier answer.
  (wtimer--with '((:text "ONE-ANSWER") (:hang t)) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (superchat-timer-fire timer due)
      (wtimer--settled)
      (setq wtimer--now (+ wtimer--now 3600))
      (let ((run (plist-get (superchat-timer-fire timer (+ due 3600)) :run-id)))
        (should (smoke--wait (lambda () (= 2 (length hm--wires))) 5))
        (superchat-runtime-cancel run))
      (wtimer--settled)
      (let ((lines (own-place--lines)))
        (should (= 2 (length lines)))
        (should (equal "ONE-ANSWER" (plist-get (car lines) :text)))
        (should (eq 'cancelled (plist-get (cadr lines) :status)))
        (should (equal "stopped" (plist-get (cadr lines) :ending)))
        (should-not (equal "ONE-ANSWER" (plist-get (cadr lines) :text)))))))

(ert-deftest own-place/a-task-that-ended-inside-its-send-has-one-line ()
  ;; A task's start is on record after its send returns (the review's N2); its
  ;; Run says who started it all the same.
  (wtimer--with '((:text "FAST-ANSWER")) (p)
    (cl-letf (((symbol-function 'smoke--fake-network)
               (lambda (_mode provider prompt partial response error multi)
                 (push (hm--wire-of provider prompt) hm--wires)
                 (smoke--deliver provider prompt partial response error multi
                                 (or (pop smoke--script) '(:text "DONE")))
                 nil)))
      (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
        (let ((fired (own-place--fire-and-settle timer)))
          (should (hm--events (plist-get fired :session-id) "timer.started"))
          (let ((lines (own-place--lines)))
            (should (= 1 (length lines)))
            (should (equal "FAST-ANSWER" (plist-get (car lines) :text)))))))))

(ert-deftest own-place/a-new-emacs-still-lists-the-conclusion ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let ((run (plist-get (own-place--fire-and-settle timer) :run-id)))
        (clrhash superchat-butler--conclusions)
        (should-not (superchat-butler-text-of run))
        (superchat-butler-catch-up)
        (should (equal "FIRST-LINE" (superchat-butler-text-of run)))
        ;; Reading did not write it again.
        (should (= 1 (length (own-place--lines))))))))

;;;; The doorbell -----------------------------------------------------------------------

(ert-deftest own-place/the-doorbell-rings-in-the-selected-conversation-first ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (let ((other (superchat--temporary-view))
            (code (presence--code-window)))
        (select-window code)
        (set-window-buffer code other)
        (should (get-buffer-window view))
        (own-place--fire-and-settle timer)
        (should (string-match-p "Butler:" (own-place--text other)))
        (should-not (string-match-p "Butler:" (own-place--text view)))))))

(ert-deftest own-place/the-doorbell-is-a-line-in-the-conversation-in-use ()
  (wtimer--with '((:text "FIRST-LINE") (:text "MAIN-ANSWER")) (p)
    (cl-destructuring-bind (view session timer) (presence--task p "TASK-MARKER")
      (let ((rows (own-place--rows session)))
        (own-place--fire-and-settle timer)
        (should (string-match-p "Butler:.*FIRST-LINE" (own-place--joined view)))
        ;; Not on its Tape, so not in a request and gone when it is read back.
        (should (= rows (own-place--rows session)))
        (hm--turn view "MAIN-QUESTION")
        (wtimer--settled)
        (should-not (cl-some (lambda (m) (string-match-p "FIRST-LINE" (hm--text m)))
                             (hm--wire 2)))
        (should (cl-some (lambda (m) (string-match-p "MAIN-QUESTION" (hm--text m)))
                         (hm--wire 2)))
        (own-place--kill view)
        (let ((default-directory temporary-file-directory))
          (should-not (string-match-p "Butler:"
                                      (own-place--text
                                       (superchat--render-session-view session)))))))))

(ert-deftest own-place/the-doorbell-leaves-the-message-being-written-and-the-cursor ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (let ((chat (get-buffer-window view)))
        (select-window chat)
        (presence--type view "half a thou")
        (with-current-buffer view
          (goto-char (- (point-max) 3))
          (set-window-point chat (point)))
        (let ((point-from-end (with-current-buffer view (- (point-max) (window-point chat)))))
          (own-place--fire-and-settle timer)
          (should (eq chat (selected-window)))
          (should (equal "half a thou" (presence--input view)))
          (should (= point-from-end
                     (with-current-buffer view (- (point-max) (window-point chat))))))))))

(ert-deftest own-place/no-doorbell-where-no-conversation-is-shown ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (delete-window (get-buffer-window view))
      (own-place--fire-and-settle timer)
      (should-not (string-match-p "Butler:" (own-place--text view)))
      ;; It is still in the butler and counted.
      (should (own-place--lines))
      (should (string-match-p "\\[Superchat: 1\\]" superchat-changes-notice-mode-line)))))

(ert-deftest own-place/no-doorbell-in-the-conversation-that-ended ()
  (wtimer--with '((:text "ONE-ANSWER") (:text "TWO-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60) 3600))
           (due (plist-get timer :due)))
      (setq wtimer--now (+ wtimer--now 60))
      (let* ((home (plist-get (superchat-timer-fire timer due) :session-id))
             (home-view (progn (wtimer--settled) (superchat-timer--live-view home))))
        (set-window-buffer (get-buffer-window view) home-view)
        (superchat-timer-fire timer (+ due 3600))
        (wtimer--settled)
        (should-not (string-match-p "Butler:" (own-place--text home-view)))
        (should-not (string-match-p "TWO-ANSWER" (own-place--text view)))))))

(defmacro own-place--with-due-record (view goal &rest body)
  "Run BODY with a record GOAL of the conversation of VIEW waiting for a time now due."
  (declare (indent 2) (debug t))
  `(let ((superchat-work-now-function (lambda () wtimer--now)))
     (clrhash superchat-work--retries)
     (unwind-protect
         (let* ((main (hm--session ,view))
                (id (superchat-work-start main ,goal)))
           (superchat-work-wait main id "time" nil (+ wtimer--now 60))
           (superchat-work-disarm-all)
           (setq wtimer--now (+ wtimer--now 120))
           ,@body)
       (superchat-work-disarm-all))))

(ert-deftest own-place/a-wake-is-a-background-conversation ()
  (wtimer--with '((:text "WOKEN-ANSWER" :delay 0.3)) (p)
    (let* ((file (wtimer--file p))
           (view (with-current-buffer file (superchat))))
      (own-place--with-due-record view "A goal"
        (let ((woken (car (superchat-work-wake-due))))
          (should-not (eq woken view))
          ;; While it runs and after, a file of the project goes to the main one.
          (should (equal main (with-current-buffer file
                                (superchat-conversation-session-id))))
          (should (equal (list main)
                         (superchat-view-recall-session-ids main)))
          (wtimer--settled)
          (should (equal main (with-current-buffer file
                                (superchat-conversation-session-id)))))))))

(ert-deftest own-place/a-wake-rings-and-writes-nothing-in-the-main-conversation ()
  (wtimer--with '((:text "WOKEN-ANSWER") (:text "MAIN-ANSWER")) (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (own-place--with-due-record view "A goal"
        (let* ((rows (own-place--rows main))
               (woken (car (superchat-work-wake-due)))
               (session (hm--session woken)))
          (should (get-buffer-window view))
          (wtimer--settled)
          ;; Once, on no Tape of the main conversation.
          (should (= rows (own-place--rows main)))
          (should (hm--events session "work.reported"))
          (should (= 1 (with-temp-buffer
                         (insert (own-place--text view))
                         (goto-char (point-min))
                         (how-many "Butler:" (point-min) (point-max)))))
          (should-not (string-match-p "was taken up at its time" (own-place--text view)))
          (let ((line (car (own-place--lines))))
            (should (equal session (plist-get line :session)))
            (should (string-match-p "A goal" (plist-get line :what)))
            (should (string-match-p "WOKEN-ANSWER" (plist-get line :text))))
          ;; Not in a request, and gone when the conversation is read back.
          (hm--turn view "MAIN-QUESTION")
          (wtimer--settled)
          (should-not (cl-some (lambda (m) (string-match-p "WOKEN-ANSWER" (hm--text m)))
                               (hm--wire 2)))
          (own-place--kill view)
          (let ((default-directory temporary-file-directory))
            (should-not (string-match-p "Butler:"
                                        (with-current-buffer (superchat--render-session-view main)
                                          (buffer-string))))))))))

(ert-deftest own-place/a-wake-cut-off-by-exiting-emacs-has-its-line ()
  ;; The line said in the main conversation was how the user learned of it.
  (wtimer--with nil (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (own-place--with-due-record view "A goal"
        (let ((rows (own-place--rows main))
              (woken nil))
          ;; Sending leaves a started Run on the Tape and nothing else.
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _)
                       (setq woken superchat--session-id)
                       (superchat-tape-append-event
                        (list :type "run.started" :session-id superchat--session-id
                              :run-id "run-own-place-cut-off" :turn-id "turn-own-place-cut-off"
                              :actor-id "work-records"
                              :summary "run.started: run-own-place-cut-off")))))
            (superchat-work-wake-due))
          ;; Emacs starts again.
          (superchat-work-arm-all)
          (should (= rows (own-place--rows main)))
          (should (hm--events woken "work.reported"))
          (should-not (string-match-p "was taken up at its time" (own-place--text view)))
          (cl-flet ((lines ()
                      (cl-remove woken (own-place--lines)
                                 :key (lambda (l) (plist-get l :session))
                                 :test-not #'equal)))
            (should (= 1 (length (lines))))
            (should (string-match-p "A goal" (plist-get (car (lines)) :what)))
            (should (string-match-p "cut off" (plist-get (car (lines)) :ending)))
            ;; The doorbell says it where the user is.
            (should (string-match-p "Butler:.*cut off" (own-place--joined view)))
            ;; Reading what ended earlier does not say it twice.
            (superchat-changes-notices-reload)
            (superchat-butler-catch-up)
            (should (= 1 (length (lines))))))))))

(ert-deftest own-place/a-wake-that-ended-is-not-said-to-be-cut-off ()
  ;; Emacs exited after it ended and before it was noted: nothing was cut off.
  (wtimer--with nil (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (own-place--with-due-record view "A goal"
        (ignore main)
        (let ((woken nil))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _)
                       (setq woken superchat--session-id)
                       (dolist (type '("run.started" "run.completed"))
                         (superchat-tape-append-event
                          (list :type type :session-id superchat--session-id
                                :run-id "run-own-place-ended" :turn-id "turn-own-place-ended"
                                :actor-id "work-records"
                                :summary (format "%s: run-own-place-ended" type)))))))
            (superchat-work-wake-due))
          (superchat-work-arm-all)
          (should (hm--events woken "work.reported"))
          (should-not (cl-some (lambda (line)
                                 (string-match-p "cut off" (or (plist-get line :ending) "")))
                               (own-place--lines))))))))

(ert-deftest own-place/a-wake-rings-in-another-conversation-in-use ()
  (wtimer--with '((:text "WOKEN-ANSWER")) (p)
    (let ((view (with-current-buffer (wtimer--file p) (superchat))))
      (own-place--with-due-record view "A goal"
        ;; Bound to no Workspace: the conversation in use need not be the main one.
        (let ((other (superchat--temporary-view)))
          (ignore main)
          (set-window-buffer (get-buffer-window view) other)
          (superchat-work-wake-due)
          (wtimer--settled)
          (should (string-match-p "Butler:.*WOKEN-ANSWER" (own-place--joined other))))))))

;;;; The mode line and the list ---------------------------------------------------------

(ert-deftest own-place/the-mode-line-opens-the-list ()
  (should (eq 'superchat-pending
              (lookup-key superchat-changes-notice-mode-line-map [mode-line mouse-1]))))

(ert-deftest own-place/the-list-says-what-it-concluded ()
  (wtimer--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (let* ((fired (own-place--fire-and-settle timer))
             (row (cl-find (plist-get fired :run-id) (superchat-pending-items)
                           :key (lambda (i) (plist-get i :key)) :test #'equal)))
        (should row)
        (should (eq 'unseen (plist-get row :kind)))
        (should (string-match-p "completed: FIRST-LINE" (plist-get row :detail)))))))

(ert-deftest own-place/an-approval-in-the-background-is-the-first-row ()
  (wtimer--with '((:tool-uses (("eval_elisp" (expression . "1")))) (:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (_view session timer) (presence--task p "TASK-MARKER")
      (superchat-permission-set session 'edit-buffers t)
      (let ((superchat-timer-task-level 'edit-buffers)
            (superchat-approval-function #'superchat-approval-publish)
            (first-row nil))
        (run-at-time 0.2 nil (lambda ()
                               (setq first-row (car (superchat-pending-items)))
                               (superchat-approval-answer
                                (plist-get (car (superchat-approval-open-questions)) :handle)
                                nil)))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (should (eq 'asking (plist-get first-row :kind)))))))

(provide 'own-place-regression)
;;; own-place-regression.el ends here
