;;; work-record-regression.el --- Work Records -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 7 of M11
;; (docs/decisions/accepted/2026-10-03-m11-slice-7-work-records.md, contract
;; K1-K9, K11, K12): a Work Record is facts on the Tape; the model keeps it
;; with the `work_record' tool; at the end of every execution the product
;; attaches what changed and decides the outcome from facts, never from the
;; answer's words; a request carries the current record in full and the
;; Workspace's other records by status, and no other Workspace's.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/work-record-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-record/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-work)
(require 'superchat-subagent)
(require 'superchat-timer)
(defvar superchat-display-style)

(defvar wrec--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wrec--finder (dir)
  (cl-loop for root in wrec--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wrec--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-work-" t))))

(defmacro wrec--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wrec--roots nil)
          ,@(mapcar (lambda (v) `(,v (wrec--temp-dir))) vars)
          (project-find-functions (list #'wrec--finder))
          (superchat-display-style 'editor))
     (setq wrec--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wrec--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wrec--file (root &optional name)
  "Return a live buffer visiting ROOT/NAME (default a.txt)."
  (let ((file (expand-file-name (or name "a.txt") root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wrec--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wrec--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(defun wrec--call (&rest args)
  "A model round that calls `work_record' with the alist ARGS."
  `(:tool-uses (("work_record" ,@args))))

(defun wrec--edit-round (buffer-name)
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . "hello") (new_string . "changed")))))

(defun wrec--view (root)
  (with-current-buffer (wrec--file root) (superchat)))

(defun wrec--user-text (n)
  (mapconcat #'hm--text (hm--body (hm--wire n)) "\n"))

(defun wrec--outcomes (id)
  (plist-get (superchat-work-get id) :outcomes))

;;;; The record -----------------------------------------------------------------------

(ert-deftest work-record/a-record-answers-the-four-questions ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "Translate the manual into Chinese")))
      (should (equal id (superchat-work-current session)))
      (superchat-work-update session id "Chapters 1-3 done" nil "Chapter 4")
      (superchat-work-update session id nil "Waiting for the glossary" nil)
      (let ((record (superchat-work-get id)))
        (should (equal "Translate the manual into Chinese" (plist-get record :goal)))
        ;; A field left out keeps its value.
        (should (equal "Chapters 1-3 done" (plist-get record :progress)))
        (should (equal "Waiting for the glossary" (plist-get record :blocked)))
        (should (equal "Chapter 4" (plist-get record :resume)))
        (should (eq 'continuable (superchat-work-status record)))
        (should (equal (plist-get (superchat-workspace-of-directory p) :id)
                       (plist-get record :workspace-id))))
      (let ((text (superchat-work-render (superchat-work-get id))))
        (should (string-match-p (concat "\\`Work record " id " (continuable)$") text))
        (should (string-match-p "^To deliver: Translate the manual into Chinese$" text))
        (should (string-match-p "^Done so far: Chapters 1-3 done$" text))
        (should (string-match-p "^Why the next step waits: Waiting for the glossary$" text))
        (should (string-match-p "^Pick up from: Chapter 4$" text))))))

(ert-deftest work-record/waiting-and-completing ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "Ship the release")))
      (superchat-work-wait session id "user" "which version number")
      (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
      (should (string-match-p "(waiting for the user: which version number)"
                              (superchat-work-render (superchat-work-get id))))
      (superchat-work-wait session id "agent" "the test run")
      (should (eq 'waiting-other (superchat-work-status (superchat-work-get id))))
      (superchat-work-wait session id nil)
      (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
      (should-error (superchat-work-wait session id "tomorrow"))
      (should-error (superchat-work-complete session id "  "))
      (superchat-work-update session id "Built" "Waiting for sign-off" "Tag it")
      (superchat-work-complete session id "Released 1.2")
      (let ((record (superchat-work-get id)))
        (should (eq 'completed (superchat-work-status record)))
        ;; A completed record shows no next step (found in the real task K2C).
        (let ((text (superchat-work-render record)))
          (should (string-match-p "^Conclusion: Released 1\\.2$" text))
          (should (string-match-p "^Done so far: Built$" text))
          (should-not (string-match-p "Why the next step waits" text))
          (should-not (string-match-p "Pick up from" text)))
        (should (equal "model" (plist-get record :completed-by))))
      ;; The current record ends with it; nothing reopens it.
      (should-not (superchat-work-current session))
      (should-error (superchat-work-update session id "more"))
      (let ((next (superchat-work-start session "Patch 1.2.1" id)))
        (should (equal id (plist-get (superchat-work-get next) :follows)))))))

(ert-deftest work-record/a-temporary-conversation-keeps-none ()
  (wrec--with nil ()
    (let ((session (hm--session (smoke--open-chat))))
      (should-error (superchat-work-start session "Anything") :type 'error)
      (should (string-match-p "belongs to no workspace.*/workspace DIRECTORY"
                              (let ((superchat--tool-execution-context (list :session-id session)))
                                (superchat-work-tool "start" nil "Anything")))))))

;;;; The tool -------------------------------------------------------------------------

(ert-deftest work-record/the-tool-is-built-in-and-on-the-default-surface ()
  (should (member "work_record" superchat-llm-default-tool-names))
  (should (eq 'builtin (superchat-sandbox-tool-class "work_record"))))

(ert-deftest work-record/the-tool-keeps-the-record-through-a-conversation ()
  (wrec--with (list (wrec--call '(action . "start") '(goal . "GOAL-MARKER: tidy the notes"))
                    (wrec--call '(action . "update") '(progress . "PROGRESS-MARKER")
                                '(resume . "RESUME-MARKER"))
                    '(:text "I have done part of it.")
                    '(:text "ok"))
      (p)
    (let* ((view (wrec--view p))
           (session (hm--session view)))
      (hm--turn view "tidy my notes, we will finish later")
      (let ((id (superchat-work-current session)))
        (should id)
        (should (equal "PROGRESS-MARKER" (plist-get (superchat-work-get id) :progress)))
        ;; The product decided the outcome from facts.
        (should (equal '("continuable") (wrec--outcomes id)))
        ;; The next request carries the record in full.
        (hm--turn view "how far are we")
        (let ((text (wrec--user-text 4)))
          (should (string-match-p "\\[Work records\\]" text))
          (should (string-match-p (concat "^Current work record:\nWork record "
                                          (regexp-quote id) " (continuable)$")
                                  text))
          (should (string-match-p "To deliver: GOAL-MARKER: tidy the notes" text))
          (should (string-match-p "Pick up from: RESUME-MARKER" text))
          (should (string-match-p (regexp-quote superchat-work-background-text) text)))))))

;;;; The end of an execution ----------------------------------------------------------

(ert-deftest work-record/the-outcome-never-reads-the-answer ()
  "An answer saying it is done does not complete the record; done does."
  (wrec--with (list (wrec--call '(action . "start") '(goal . "Write the report"))
                    '(:text "All done, the report is finished!")
                    (wrec--call '(action . "done") '(conclusion . "Report written"))
                    '(:text "ok"))
      (p)
    (let* ((view (wrec--view p))
           (session (hm--session view)))
      (hm--turn view "write the report")
      (let ((id (superchat-work-current session)))
        (should (equal '("continuable") (wrec--outcomes id)))
        (hm--turn view "mark it finished")
        (should (equal '("continuable" "completed") (wrec--outcomes id)))
        (should (equal "Report written" (plist-get (superchat-work-get id) :conclusion)))))))

(ert-deftest work-record/a-registered-wait-is-the-outcome ()
  (wrec--with (list (wrec--call '(action . "start") '(goal . "Deploy"))
                    (wrec--call '(action . "wait") '(wait_for . "user") '(detail . "the password"))
                    '(:text "I need the password."))
      (p)
    (let* ((view (wrec--view p))
           (session (hm--session view)))
      (hm--turn view "deploy it")
      (should (equal '("waiting") (wrec--outcomes (superchat-work-current session)))))))

(ert-deftest work-record/the-product-attaches-what-changed ()
  (wrec--with nil (p)
    (let* ((src (wrec--file p))
           (view (with-current-buffer src (superchat)))
           (session (hm--session view)))
      (setq smoke--script
            (list (wrec--call '(action . "start") '(goal . "Change a.txt"))
                  (wrec--edit-round (buffer-name src))
                  '(:text "changed")))
      (hm--turn view "change it")
      (let* ((id (superchat-work-current session))
             (facts (plist-get (superchat-work-get id) :facts)))
        (should (= 1 (length facts)))
        (should (member (buffer-file-name src) (append (plist-get (car facts) :files) nil)))
        (should (string-match-p (concat "Files its executions changed (recorded by Superchat): .*"
                                        (regexp-quote (buffer-file-name src)))
                                (superchat-work-render (superchat-work-get id))))))))

;;;; Workspaces -----------------------------------------------------------------------

(ert-deftest work-record/a-request-lists-this-workspace-only ()
  (wrec--with '((:text "ok") (:text "ok")) (p q)
    (let* ((vp (wrec--view p)) (sp (hm--session vp))
           (vq (wrec--view q)) (sq (hm--session vq)))
      (let ((open (superchat-work-start sp "P-OPEN-MARKER"))
            (waiting (superchat-work-start sp "P-WAITING-MARKER"))
            (done (superchat-work-start sp "P-DONE-MARKER")))
        (superchat-work-wait sp waiting "user" "an answer")
        (superchat-work-complete sp done "finished")
        (superchat-work-start sq "Q-MARKER")
        ;; A new conversation in P has no current record but sees the list.
        (let ((vp2 (with-current-buffer (wrec--file p "b.txt") (superchat-new-session))))
          (hm--turn vp2 "what is open")
          (let ((text (wrec--user-text 1)))
            (should (string-match-p "No current work record in this conversation\\." text))
            (should (string-match-p (concat "Continuable:\n- " open ": P-OPEN-MARKER") text))
            (should (string-match-p (concat "Waiting for the user:\n- " waiting ": P-WAITING-MARKER") text))
            (should (string-match-p (concat "Recently completed:\n- " done ": P-DONE-MARKER") text))
            (should-not (string-match-p "Q-MARKER" text))))
        (hm--turn vq "and here")
        (let ((text (wrec--user-text 2)))
          (should (string-match-p "Q-MARKER" text))
          (should-not (string-match-p "P-OPEN-MARKER" text)))))))

(ert-deftest work-record/a-temporary-request-has-no-records-block ()
  (wrec--with '((:text "ok")) (p)
    (superchat-work-start (hm--session (wrec--view p)) "P-MARKER")
    (hm--turn (smoke--open-chat) "hello")
    (should-not (string-match-p "\\[Work records\\]" (wrec--user-text 1)))))

;;;; For the user ------------------------------------------------------------------

(ert-deftest work-record/the-user-lists-and-completes-records ()
  (wrec--with nil (p q)
    (let* ((sp (hm--session (wrec--view p)))
           (sq (hm--session (wrec--view q)))
           (a (superchat-work-start sp "LIST-A"))
           (b (superchat-work-start sq "LIST-B")))
      (superchat-work-complete sq b "done before")
      (let ((buffer (superchat-work-list)))
        (unwind-protect
            (let ((text (with-current-buffer buffer (buffer-string))))
              ;; Every Workspace's records, open ones first.
              (should (string-match-p (concat (regexp-quote a) " +continuable +[^ ]+ +LIST-A") text))
              (should (string-match-p (concat (regexp-quote b) " +completed +[^ ]+ +LIST-B") text))
              (should (< (string-match (regexp-quote a) text) (string-match (regexp-quote b) text)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward a)
                (superchat-work-list-complete a "USER-CONCLUSION")))
          (kill-buffer buffer)))
      (let ((record (superchat-work-get a)))
        (should (equal "USER-CONCLUSION" (plist-get record :conclusion)))
        (should (equal "user" (plist-get record :completed-by))))
      (should-error (superchat-work-mark-complete a "again")))))

;;;; Waking a record whose time has come (slice 8.1) -------------------------------

(defvar wrec--now 1800000000.0)

(defmacro wrec--with-clock (&rest body)
  "Run BODY with the Work Record clock at `wrec--now', disarming afterwards."
  (declare (indent 0))
  `(let ((superchat-work-now-function (lambda () wrec--now)))
     (unwind-protect (progn ,@body)
       (superchat-work-disarm-all))))

(defun wrec--facts (session type)
  (mapcar (lambda (event) (plist-get event :payload)) (hm--events session type)))

(ert-deftest work-record/a-wait-for-a-time-names-the-time ()
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "Check the build later")))
        (should-error (superchat-work-wait session id "time" "the build"))
        (let ((superchat--tool-execution-context (list :session-id session)))
          (superchat-work-tool "wait" nil nil nil nil nil "time" "the build" nil nil 30))
        (let ((record (superchat-work-get id)))
          (should (= (+ wrec--now 1800) (plist-get (plist-get record :waiting) :until)))
          (should (eq 'waiting-other (superchat-work-status record)))
          (should (string-match-p
                   (regexp-quote (format "(waiting for %s: the build)"
                                         (format-time-string "%Y-%m-%d %H:%M"
                                                             (seconds-to-time (+ wrec--now 1800)))))
                   (superchat-work-render record))))))))

(ert-deftest work-record/nothing-wakes-before-its-time ()
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "Later")))
        (superchat-work-wait session id "time" nil (+ wrec--now 600))
        (setq wrec--now (+ wrec--now 599))
        (should-not (superchat-work-wake-due))
        ;; A wait for the user never wakes by itself.
        (superchat-work-wait session id "user" "an answer")
        (setq wrec--now (+ wrec--now 10000))
        (should-not (superchat-work-wake-due))))))

(ert-deftest work-record/at-its-time-a-record-goes-on-in-a-new-conversation ()
  (wrec--with '((:text "CONTINUED-ANSWER")) (p)
    (wrec--with-clock
      (let* ((main-view (wrec--view p))
             (main (hm--session main-view))
             (id (superchat-work-start main "WAKE-GOAL-MARKER")))
        (superchat-work-update main id "half done" nil "the second half")
        (superchat-work-wait main id "time" "tonight" (+ wrec--now 600))
        (setq wrec--now (+ wrec--now 600))
        (let* ((views (superchat-work-wake-due))
               (view (car views))
               (session (hm--session view)))
          (should (= 1 (length views)))
          (should-not (equal main session))
          ;; The new conversation is in the same Workspace, on this record.
          (should (equal (plist-get (superchat-workspace-of-directory p) :id)
                         (plist-get (buffer-local-value 'superchat--workspace-binding view) :id)))
          (should (equal id (superchat-work-current session)))
          (wrec--settled)
          (let ((text (mapconcat #'hm--text (hm--wire 1) "\n")))
            (should (string-match-p (regexp-quote (format superchat-work-wake-prompt id)) text))
            (should (string-match-p "To deliver: WAKE-GOAL-MARKER" text))
            (should (string-match-p "Pick up from: the second half" text)))
          ;; Woken once, the wait cleared, the outcome decided as for any run.
          (let ((record (superchat-work-get id)))
            (should (= 1 (length (plist-get record :woken))))
            (should-not (plist-get record :waiting))
            (should (equal "continuable" (car (last (plist-get record :outcomes))))))
          ;; The main conversation has a line saying so; the model's words are not in it.
          (let ((reported (wrec--facts main "work.reported")))
            (should (= 1 (length reported)))
            (should (equal session (plist-get (car reported) :conversation))))
          (should (string-match-p (concat "Work record " (regexp-quote id)
                                          " was taken up at its time in conversation "
                                          (regexp-quote session) ": continuable\\.")
                                  (with-current-buffer main-view (buffer-string))))
          (should-not (superchat-work-wake-due)))))))

(ert-deftest work-record/a-wait-missed-while-emacs-was-closed-wakes-once-after-loading ()
  (wrec--with '((:text "ok")) (p)
    (wrec--with-clock
      (let* ((main (hm--session (wrec--view p)))
             (id (superchat-work-start main "Missed")))
        (superchat-work-wait main id "time" nil (+ wrec--now 60))
        (superchat-work-disarm-all)
        (setq wrec--now (+ wrec--now 7200))
        (superchat-work-arm-all)
        (wrec--settled)
        (should (= 1 (length (plist-get (superchat-work-get id) :woken))))
        (superchat-work-arm-all)
        (should (= 1 (length (plist-get (superchat-work-get id) :woken))))))))

(ert-deftest work-record/a-wait-is-armed-while-the-row-is-mounted ()
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((main (hm--session (wrec--view p)))
             (id (superchat-work-start main "Armed")))
        (superchat-work-wait main id "time" nil (+ wrec--now 600))
        (should (timerp (gethash id superchat-work--armed)))
        (unwind-protect
            (progn
              (superchat-plugin-unmount 'work-records)
              (should (= 0 (hash-table-count superchat-work--armed))))
          (superchat-plugin-mount 'work-records))
        (should (smoke--wait (lambda () (gethash id superchat-work--armed)) 5))))))

;;;; Delegation under a record (slice 8.2) -----------------------------------------

(defun wrec--subagent-wire ()
  "The provider request whose user text holds the delegated task."
  (let ((wire (cl-find-if
               (lambda (wire)
                 (cl-some (lambda (message)
                            (and (equal "user" (alist-get 'role message))
                                 (string-match-p "SUB-TASK-MARKER" (hm--text message))))
                          wire))
               (reverse hm--wires))))
    (should wire)
    wire))

(ert-deftest work-record/a-delegated-task-is-recorded-under-its-record ()
  (wrec--with (list (wrec--call '(action . "start") '(goal . "GOAL-SUB-MARKER"))
                    '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                   (task . "SUB-TASK-MARKER look into it"))))
                    '(:text "SUB-REPORT-MARKER found three things")
                    '(:text "done"))
      (p)
    (setq superchat-llm-tool-names
          (append superchat-llm-tool-names '("delegate_to_subagent")))
    (let* ((view (wrec--view p))
           (session (hm--session view)))
      (hm--turn view "start it and delegate the research")
      (wrec--settled)
      (let* ((id (superchat-work-current session))
             (record (superchat-work-get id))
             (tasks (plist-get record :tasks))
             (system (hm--text (car (wrec--subagent-wire)))))
        ;; The sub-agent got a snapshot of the record, not the main conversation.
        (should (string-match-p (regexp-quote superchat-subagent-work-record-preamble) system))
        (should (string-match-p "To deliver: GOAL-SUB-MARKER" system))
        ;; The task and its result are on the record.
        (should (= 1 (length tasks)))
        (should (string-match-p "SUB-TASK-MARKER" (plist-get (car tasks) :task)))
        (should (string-match-p "SUB-REPORT-MARKER" (plist-get (car tasks) :report)))
        (should (equal "completed" (plist-get (car tasks) :status)))
        (should-not (equal session (plist-get (car tasks) :conversation)))
        (should (string-match-p "^Delegated tasks (recorded by Superchat):\n- SUB-TASK-MARKER.* -- completed: SUB-REPORT-MARKER"
                                (superchat-work-render record)))))))

(ert-deftest work-record/a-task-without-a-record-records-nothing ()
  (wrec--with (list '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                   (task . "SUB-TASK-MARKER look into it"))))
                    '(:text "SUB-REPORT")
                    '(:text "done"))
      (p)
    (setq superchat-llm-tool-names
          (append superchat-llm-tool-names '("delegate_to_subagent")))
    (hm--turn (wrec--view p) "delegate")
    (wrec--settled)
    (should-not (string-match-p (regexp-quote superchat-subagent-work-record-preamble)
                                (hm--text (car (wrec--subagent-wire)))))
    (should-not (superchat-work-records))))

;;;; jj changes (slice 8.3) ------------------------------------------------------------

(ert-deftest work-record/in-a-jj-repository-the-change-is-recorded ()
  (unless (executable-find "jj") (ert-skip "jj is not installed"))
  (wrec--with nil (p)
    (let ((default-directory p))
      (should (eq 0 (call-process "jj" nil nil nil "git" "init" "--quiet"))))
    (let ((change (superchat-work-jj-change p)))
      (should (string-match-p "\\`[a-z]+\\'" change))
      (should-not (superchat-work-jj-change (wrec--temp-dir)))
      (should-not (superchat-work-jj-change nil))
      (let* ((src (wrec--file p))
             (view (with-current-buffer src (superchat)))
             (session (hm--session view)))
        (setq smoke--script
              (list (wrec--call '(action . "start") '(goal . "Change a.txt"))
                    (wrec--edit-round (buffer-name src))
                    '(:text "changed")))
        (hm--turn view "change it")
        (let ((record (superchat-work-get (superchat-work-current session))))
          (should (equal change (plist-get (car (plist-get record :facts)) :jj-change)))
          (should (string-match-p (concat "^jj changes those executions left in the workspace: "
                                          (regexp-quote change) "$")
                                  (superchat-work-render record))))))))

;;;; A task timer is not a wait (slice 8.4) -----------------------------------------

(ert-deftest work-record/with-a-current-record-a-task-timer-is-refused ()
  "Contract T4; in the real task T4W the model set a task timer to continue
the work, and it ran in the old conversation."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (superchat--tool-execution-context (list :session-id session)))
      ;; No current record: a task timer is a separate job and is fine.
      (should (string-match-p "the task will run"
                              (superchat-timer-tool-create "daily digest" "task" 5)))
      (superchat-work-start session "Check twice")
      (let ((result (superchat-timer-tool-create "continue the check" "task" 2)))
        (should (string-match-p "\\`Error: This conversation has a current work record; to take that work up again later, use work_record with action wait" result)))
      ;; A plain reminder is still a reminder.
      (should (string-match-p "the user will be reminded"
                              (superchat-timer-tool-create "stretch" "reminder" 30)))
      (superchat-timer-disarm-all))))

(ert-deftest work-record/a-wait-given-with-an-update-is-registered ()
  "In the real task T4W the model put wait_for and in_minutes on an update."
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (superchat--tool-execution-context (list :session-id session))
             (id (superchat-work-start session "Two steps")))
        (superchat-work-tool "update" nil nil "step one" "two minutes" "step two"
                             "time" "the second step" nil nil 2)
        (let ((record (superchat-work-get id)))
          (should (equal "step one" (plist-get record :progress)))
          (should (= (+ wrec--now 120) (plist-get (plist-get record :waiting) :until))))
        ;; An update with nothing in it is still refused.
        (should (string-prefix-p "Error:" (superchat-work-tool "update")))))))

;;;; The user's answer (slice 9) ------------------------------------------------------

(defun wrec--waiting-facts (session)
  (cl-remove-if-not (lambda (payload) (plist-get payload :answered))
                    (wrec--facts session "work.waiting")))

(ert-deftest work-record/the-users-next-message-ends-the-wait-for-the-user ()
  (wrec--with '((:text "thanks")) (p)
    (let* ((view (wrec--view p))
           (session (hm--session view))
           (id (superchat-work-start session "Pick a plan")))
      (superchat-work-wait session id "user" "which option")
      (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
      (hm--turn view "option B")
      ;; The product cleared it, before the request was built.
      (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
      (should (= 1 (length (wrec--waiting-facts session))))
      (let ((text (wrec--user-text 1)))
        (should (string-match-p (concat "Work record " (regexp-quote id) " (continuable)") text))
        (should-not (string-match-p "waiting for the user" text))))))

(ert-deftest work-record/only-the-user-in-that-conversation-ends-the-wait ()
  (wrec--with '((:text "ok") (:text "ok")) (p)
    (wrec--with-clock
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (id (superchat-work-start session "Pick a plan")))
        (superchat-work-wait session id "user" "which option")
        ;; Another conversation of the Workspace: the record is listed as
        ;; waiting, and stays so.
        (let ((other (with-current-buffer (wrec--file p "b.txt") (superchat-new-session))))
          (hm--turn other "what is open")
          (should (string-match-p (concat "Waiting for the user:\n- " (regexp-quote id))
                                  (wrec--user-text 1))))
        (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
        ;; A message the user did not type, and a command, do not end it either.
        (let ((superchat-automated-input t))
          (hm--turn view "a timer speaking"))
        (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
        (with-current-buffer view
          (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "/changes"))
          (superchat-send-input))
        (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
        (should-not (wrec--waiting-facts session))))))

(ert-deftest work-record/a-wait-for-a-time-is-not-ended-by-the-user-speaking ()
  (wrec--with '((:text "ok")) (p)
    (wrec--with-clock
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (id (superchat-work-start session "Later")))
        (superchat-work-wait session id "time" nil (+ wrec--now 600))
        (hm--turn view "by the way")
        (should (eq 'waiting-other (superchat-work-status (superchat-work-get id))))))))

(ert-deftest work-record/the-two-ways-to-wait-for-the-user-are-told-apart ()
  (let* ((tools (let ((superchat-llm-tools-list nil)) (superchat-get-llm-tools)))
         (work (llm-tool-description (cl-find "work_record" tools :key #'llm-tool-name :test #'equal))))
    (should (string-match-p "register wait_for user and end your turn" work))
    (should (string-match-p "the user's next message in this conversation clears the wait" work))
    (should (string-match-p "use ask_user only for a question the user can answer right now" work))))

(provide 'work-record-regression)
;;; work-record-regression.el ends here
