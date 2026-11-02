;;; subagent-background-regression.el --- a delegation that does not hold the conversation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-a-sub-agent-that-works-in-the-background.md:
;; `delegate_to_subagent' with `background' answers at once; the execution that
;; asked can end and the user can be answered while the sub-agent works; the
;; sub-agent's report then starts a new execution in the conversation.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/subagent-background-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^subagent-background/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-subagent)
(require 'superchat-pending)

(defvar subbg--sub-delay 0.8
  "Seconds the scripted sub-agent takes: long enough for a turn to fit in it.")

(defun subbg--delegate-round (&optional preset)
  "A model round that delegates a task to run in the background."
  `(:tool-uses (("delegate_to_subagent"
                 (preset . ,(or preset "researcher"))
                 (task . "SUB-TASK-MARKER look into it")
                 (background . t)))))

(defun subbg--sub-step (&optional delay)
  `(:text "SUB-REPORT-MARKER" :delay ,(or delay subbg--sub-delay)))

(defmacro subbg--with (script &rest body)
  "Run BODY in the fixture with SCRIPT and delegation offered."
  (declare (indent 1) (debug t))
  `(hm--fixture ,script
     (setq superchat-llm-tool-names
           (append superchat-llm-tool-names '("delegate_to_subagent")))
     (setq superchat-llm-tools-list nil)
     (let ((superchat--subagent-running nil))
       ,@body)))

(defun subbg--wire-with (marker)
  "The provider requests, oldest first, whose user text holds MARKER."
  (cl-remove-if-not
   (lambda (wire)
     (cl-some (lambda (message)
                (and (equal "user" (alist-get 'role message))
                     (string-match-p marker (hm--text message))))
              wire))
   (reverse hm--wires)))

(defun subbg--last-user-text (wire)
  (hm--text (car (last (cl-remove-if-not
                        (lambda (message) (equal "user" (alist-get 'role message)))
                        wire)))))

(defun subbg--tool-result-text (session)
  "The result text of the one finished tool call in SESSION."
  (let ((event (car (hm--events session "tool.completed"))))
    (should event)
    (format "%S" event)))

(defun subbg--settled ()
  (should (smoke--wait (lambda ()
                         (and (null (superchat-runtime-active-runs))
                              (null superchat--subagent-running)
                              (not (cl-some (lambda (view)
                                              (and (buffer-live-p view)
                                                   (> (superchat-runtime-pending-follow-ups
                                                       (or (buffer-local-value
                                                            'superchat--session-id view)
                                                           "none"))
                                                      0)))
                                            (buffer-list)))))
                       20)))

;;;; It answers at once and outlives the execution that asked ---------------------

(ert-deftest subagent-background/the-execution-that-asked-ends-and-the-sub-agent-goes-on ()
  (subbg--with (list (subbg--delegate-round) (subbg--sub-step)
                     '(:text "PARENT-CARRIES-ON") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      ;; The asking execution ended well, and the sub-agent was not cancelled
      ;; with it (`superchat-runtime--cancel-children').
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (sit-for 0.05)
      (let ((entry (cdar superchat--subagent-running)))
        (should entry)
        (should (plist-get entry :background))
        (should-not (plist-get entry :parent-run-id))
        (should (string-match-p (regexp-quote (plist-get entry :id))
                                (subbg--tool-result-text (hm--session view)))))
      (subbg--settled))))

(ert-deftest subagent-background/the-report-starts-one-execution-in-the-conversation ()
  (subbg--with (list (subbg--delegate-round) (subbg--sub-step 0.2)
                     '(:text "PARENT-CARRIES-ON") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view)))
      (smoke--send view "please look into it")
      (subbg--settled)
      (let ((wires (subbg--wire-with "SUB-REPORT-MARKER")))
        (should (= 1 (length wires)))
        (should (string-match-p "SUB-TASK-MARKER"
                                (subbg--last-user-text (car wires)))))
      ;; Two executions in the conversation: the one the user started and the
      ;; one the report started, whose Actor says so.
      (let ((started (hm--events session "run.started")))
        (should (= 2 (length started)))
        (should (member "subagent-report"
                        (mapcar (lambda (event) (plist-get event :actor-id)) started))))
      ;; The conversation's screen says where the message came from.
      (with-current-buffer view
        (should (string-match-p "reported" (buffer-string)))))))

(ert-deftest subagent-background/the-user-is-answered-before-the-report-arrives ()
  (subbg--with (list (subbg--delegate-round) (subbg--sub-step)
                     '(:text "PARENT-CARRIES-ON")
                     '(:text "ANSWER-TO-THE-USER") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (should superchat--subagent-running)
      (let ((second (smoke--send view "USER-SECOND-MARKER a quick question")))
        (should (stringp second))
        (should (smoke--wait (lambda () (smoke--run-done-p second)) 5))
        ;; Answered while the sub-agent still works.
        (should superchat--subagent-running)
        (should (eq 'completed (plist-get (superchat-runtime-status second) :status))))
      (subbg--settled)
      (let* ((wires (reverse hm--wires))
             (user (cl-position-if
                    (lambda (w) (member w (subbg--wire-with "USER-SECOND-MARKER")))
                    wires))
             (report (cl-position-if
                      (lambda (w) (member w (subbg--wire-with "SUB-REPORT-MARKER")))
                      wires)))
        (should user)
        (should report)
        (should (< user report))))))

(ert-deftest subagent-background/a-report-that-comes-during-an-execution-waits-for-its-end ()
  (subbg--with (list (subbg--delegate-round) (subbg--sub-step 0.3)
                     '(:text "PARENT-CARRIES-ON")
                     '(:text "SLOW-ANSWER" :delay 0.8) '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (let ((second (smoke--send view "USER-SECOND-MARKER")))
        (should (smoke--wait (lambda () (null superchat--subagent-running)) 5))
        ;; The report came while the second execution ran: it did not join it.
        (should-not (smoke--run-done-p second))
        (should (= 1 (superchat-runtime-pending-follow-ups (hm--session view)))))
      (subbg--settled)
      (should (= 1 (length (subbg--wire-with "SUB-REPORT-MARKER")))))))

;;;; When it goes wrong -----------------------------------------------------------

(ert-deftest subagent-background/a-stopped-sub-agent-still-reports ()
  (subbg--with (list (subbg--delegate-round) '(:hang t)
                     '(:text "PARENT-CARRIES-ON") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (should superchat--subagent-running)
      (superchat-subagent-cancel (caar superchat--subagent-running)
                                 "[STOPPED-MARKER]")
      (subbg--settled)
      (should (= 1 (length (subbg--wire-with "STOPPED-MARKER")))))))

(ert-deftest subagent-background/an-unknown-preset-is-the-call-s-own-failure ()
  (subbg--with (list (subbg--delegate-round "no-such-preset")
                     '(:text "TOLD-OF-THE-ERROR"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (subbg--settled)
      (should (string-match-p "unknown sub-agent preset"
                              (subbg--tool-result-text session)))
      (should-not (string-match-p "Started sub-agent"
                                  (subbg--tool-result-text session)))
      ;; Nothing was started, so nothing reports.
      (should (= 1 (length (hm--events session "run.started")))))))

(ert-deftest subagent-background/more-than-the-limit-is-refused-and-says-so ()
  (let ((superchat-subagent-parallel-max 1))
    (subbg--with (list `(:tool-uses (("delegate_to_subagent"
                                      (preset . "researcher")
                                      (task . "SUB-TASK-MARKER one")
                                      (background . t))))
                       '(:hang t)
                       `(:tool-uses (("delegate_to_subagent"
                                      (preset . "researcher")
                                      (task . "SUB-TASK-MARKER two")
                                      (background . t))))
                       '(:text "TOLD-OF-THE-LIMIT"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (run (smoke--send view "two things")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
        (should (= 1 (length superchat--subagent-running)))
        (should (string-match-p
                 "already"
                 (format "%S" (car (last (hm--events session "tool.completed"))))))
        (superchat-subagent-cancel (caar superchat--subagent-running) "[done]")))))

(ert-deftest subagent-background/a-closed-conversation-is-opened-for-the-report ()
  (subbg--with (list (subbg--delegate-round) (subbg--sub-step 0.5)
                     '(:text "PARENT-CARRIES-ON") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer view))
      (subbg--settled)
      (should (= 2 (length (hm--events session "run.started"))))
      (should (= 1 (length (subbg--wire-with "SUB-REPORT-MARKER")))))))

;;;; What stays as it was ---------------------------------------------------------

(ert-deftest subagent-background/without-it-the-call-waits-for-the-report ()
  (subbg--with (list '(:tool-uses (("delegate_to_subagent"
                                    (preset . "researcher")
                                    (task . "SUB-TASK-MARKER look into it"))))
                     '(:text "SUB-REPORT-MARKER" :delay 0.1)
                     '(:text "DONE"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view)))
      (hm--turn view "please look into it")
      (subbg--settled)
      (should (= 1 (length (hm--events session "run.started"))))
      (should (string-match-p "SUB-REPORT-MARKER"
                              (subbg--tool-result-text session))))))

;;;; What the list says -----------------------------------------------------------

(ert-deftest subagent-background/the-pending-list-says-who-is-working-for-whom ()
  (subbg--with (list (subbg--delegate-round) '(:hang t)
                     '(:text "PARENT-CARRIES-ON") '(:text "REPORT-READ"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "please look into it")))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 5))
      (sit-for 0.05)
      (let* ((text (format "%S" (superchat-pending--running))))
        (should (string-match-p "in the background" text))
        (should-not (string-match-p "started by you" text)))
      (superchat-subagent-cancel (caar superchat--subagent-running) "[done]"))))

(ert-deftest subagent-background/the-report-s-execution-is-not-work-nobody-watched ()
  (should-not (member "subagent-report" superchat-changes--unattended-actors)))

(provide 'subagent-background-regression)
;;; subagent-background-regression.el ends here
