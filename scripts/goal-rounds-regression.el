;;; goal-rounds-regression.el --- goal rounds for a Work Record -*- lexical-binding: t; -*-

;;; Commentary:
;; The eleven tests of reports/guidance-2026-10-11/GOAL-ROUNDS.md, written
;; before any product code (proposal:
;; docs/decisions/proposed/2026-10-11-goal-rounds.md).  Nothing here is
;; marked expected-fail: a test that is red is red.
;;
;; Names the proposal has not fixed are placeholders, and the product is free
;; to change them with the tests: the fact `work.round' with `:round', the
;; Actor "goal-rounds", the tool action "resume", the option
;; `superchat-work-round-limit', the in-process table
;; `superchat-work--continuing', and the reader `superchat-work-rounds'.
;;
;; The model's answers here state what was done and announce nothing: an
;; answer that announces a next step makes the finish check ask once more in
;; the same execution, and that request would take the script step meant for
;; the round.
;;
;; The user's rulings (2026-10-12, through the leader): all nine as proposed,
;; the eighth left open.  12 and 13 are for rulings 2 and 4 (the user's own
;; command), which the eleven do not cover.
;;
;; Where each stands on d051f21c, before any product code (10 red, 3 pass):
;;   1  red   (= 1 (gr--count session "queue.queued")), was 0
;;   2  red   (eq (quote completed) (superchat-work-status record)), was continuable
;;   3  red   (eq (quote waiting-user) (superchat-work-status ...)), was continuable
;;   4  red   no Run started by "goal-rounds" within 10 s
;;   5  red   (= 1 (gr--count session "run.failed")), was 0: no round ran to fail
;;   6  red   (= 2 (gr--count session "work.round")), was 0
;;   7  red   no Run started by "goal-rounds" within 10 s
;;   8  pass  a guard: nothing queues a round today, whoever started the Run
;;   9  pass  a guard, for the same reason
;;   10 pass  a guard, for the same reason
;;   11 red   (= 1 (gr--count session "queue.dropped")), was 0
;;   12 red   (stringp run) in `hm--turn', was nil: "/work ..." starts no Run
;;   13 red   no Run started by "goal-rounds" within 10 s
;; 8, 9 and 10 tell nothing apart today: they say a round is not queued, and
;; nothing queues one yet.  They start to discriminate once the driver exists.
;; 8 covers a scheduled task, a woken record and a sub-agent.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/goal-rounds-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^goal-rounds/")'

;;; Code:

(load (expand-file-name "scripts/pending-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t)
    (advice-remove 'load skip-loaded)))
(require 'superchat-inline)

(defvar superchat-work-round-limit)
(defvar superchat-work--continuing)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)

(defconst gr--goal "GR-GOAL tick the ninety-one items")

(defun gr--start () (wrec--call '(action . "start") `(goal . ,gr--goal)))

(defun gr--quiet ()
  "Wait until no Run is under way and none follows the one that ended."
  (let ((still 0) (tries 0))
    (while (and (< still 3) (< tries 150))
      (accept-process-output nil 0.2)
      (setq tries (1+ tries)
            still (if (or (superchat-runtime-active-runs)
                          (cl-plusp (hash-table-count superchat-runtime--queue-timers)))
                      0
                    (1+ still))))
    (wrec--settled)))

(defun gr--record ()
  "The id of the one record this test made.
A completed record is no longer its conversation's current one."
  (plist-get (car (last (superchat-work-records))) :id))

(defun gr--count (session type) (length (hm--events session type)))

(defun gr--actors (session)
  "The :actor-id of every `run.started' of SESSION, oldest first."
  (mapcar (lambda (e) (plist-get e :actor-id)) (hm--events session "run.started")))

(defun gr--queued-anywhere ()
  "How many `queue.queued' facts the whole Tape holds."
  (let ((row (car (superchat-db-tape-select
                   "SELECT COUNT(*) FROM tape WHERE json_extract(meta,'$.event_type')='queue.queued'"))))
    (if (consp row) (car row) row)))

(defun gr--first-turn (view)
  "Send the user's message from VIEW; the model starts the record and stops."
  (hm--turn view "tick them all off")
  (gr--quiet))

(defun gr--round-run (session)
  "The id of the Run of SESSION a round started, once there is one."
  (should (smoke--wait
           (lambda () (member "goal-rounds" (gr--actors session))) 10))
  (plist-get (car (last (hm--events session "run.started"))) :run-id))

;;;; 1 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/1-a-continuable-ending-queues-one-round ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      (wrec--call '(action . "done") '(conclusion . "GR-ALL-TICKED"))
                      '(:text "All ticked."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (should (= 1 (gr--count session "queue.queued")))
        (should (= 1 (gr--count session "queue.dispatched")))
        (should (equal '("lead" "goal-rounds") (gr--actors session)))
        (let ((round (wrec--user-text 3)))
          (should (string-match-p (regexp-quote gr--goal) round))
          (should (string-match-p "1/256" round)))
        (should (equal '(1) (mapcar (lambda (e) (plist-get (plist-get e :payload) :round))
                                    (hm--events session "work.round"))))
        ;; The conversation says a round began; nobody typed its message.
        (should (string-match-p "^\\* Round 1/256 on work record "
                                (with-current-buffer view
                                  (buffer-substring-no-properties (point-min) (point-max)))))))))

;;;; 2 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/2-a-round-that-completes-the-record-queues-no-more ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      (wrec--call '(action . "done") '(conclusion . "GR-ALL-TICKED"))
                      '(:text "All ticked."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (should (eq 'completed (superchat-work-status (superchat-work-get (gr--record)))))
        (should-not (superchat-work-current session))
        (should (= 1 (gr--count session "queue.queued")))))))

;;;; 3 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/3-a-round-that-waits-for-the-user-queues-no-more ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      (wrec--call '(action . "wait") '(wait_for . "user")
                                  '(detail . "GR-WHICH-RULE"))
                      '(:text "I need the rule."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (should (eq 'waiting-user
                    (superchat-work-status
                     (superchat-work-get (superchat-work-current session)))))
        (should (= 1 (gr--count session "queue.queued")))
        (should (= 1 (length (pend--of 'ask-user))))))))

;;;; 4 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/4-a-stopped-round-pauses-until-resumed ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      '(:text "NEVER-DELIVERED" :delay 5)
                      (wrec--call '(action . "resume"))
                      '(:text "Resuming.")
                      (wrec--call '(action . "done") '(conclusion . "GR-DONE"))
                      '(:text "Done."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (hm--turn view "tick them all off")
        (let ((run (gr--round-run session))
              (id (superchat-work-current session)))
          (should (superchat-runtime-cancel run))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
          (gr--quiet)
          (should (equal "stopped" (car (last (wrec--outcomes id)))))
          (should (= 1 (gr--count session "queue.queued")))
          (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
          ;; The user says to go on; the model resumes in the user's own Run.
          (hm--turn view "go on")
          (gr--quiet)
          (should (= 2 (gr--count session "queue.queued")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

;;;; 5 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/5-a-failed-or-budget-ended-round-queues-no-more ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      '(:error (error . "GR_PROVIDER_DOWN")))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (should (= 1 (gr--count session "run.failed")))
        (should (= 1 (gr--count session "queue.queued")))))
    (let ((superchat-llm-max-tool-rounds 2)
          (superchat-llm-round-limit-action 'finalize))
      (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                        (wrec--call '(action . "update") '(progress . "one"))
                        (wrec--call '(action . "update") '(progress . "two"))
                        '(:text "Out of rounds."))
          (p)
        (let* ((view (wrec--view p)) (session (hm--session view)))
          (gr--first-turn view)
          (let ((terminal (car (last (hm--events session "run.completed")))))
            (should (equal "finalize"
                           (format "%s" (plist-get (plist-get (plist-get terminal :payload)
                                                              :tool-budget)
                                                   :action)))))
          (should (= 1 (gr--count session "queue.queued"))))))))

;;;; 6 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/6-the-round-limit-ends-in-a-wait-for-the-user ()
  (pend--fresh
    (let ((superchat-work-round-limit 2))
      (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.") '(:text "ROUND-ONE ticked ten more.")
                        '(:text "ROUND-TWO ticked ten more."))
          (p)
        (let* ((view (wrec--view p)) (session (hm--session view)))
          (gr--first-turn view)
          (should (= 2 (gr--count session "work.round")))
          (should (= 2 (gr--count session "queue.queued")))
          (let ((waiting (plist-get (superchat-work-get (superchat-work-current session))
                                    :waiting)))
            (should (equal "user" (plist-get waiting :for)))
            (should (string-match-p "round limit" (or (plist-get waiting :detail) ""))))
          (should (= 1 (length (pend--of 'ask-user))))
          ;; A round is the user's own conversation going on: no notice.
          (should-not superchat-changes--notices)
          (should-not (pend--of 'unseen)))))))

;;;; 7 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/7-a-new-emacs-does-not-go-on ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.") '(:text "ROUND-ONE ticked ten more." :delay 0.4))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (hm--turn view "tick them all off")
        (gr--round-run session)
        ;; While the round runs the list says which round it is.
        (should (string-match-p "round 1/256 on work record"
                                (plist-get (car (pend--of 'running)) :what)))
        ;; What one Emacs keeps in memory is gone in the next.
        (when (and (boundp 'superchat-work--continuing)
                   (hash-table-p superchat-work--continuing))
          (clrhash superchat-work--continuing))
        (gr--quiet)
        (let ((id (superchat-work-current session)))
          (should (= 1 (gr--count session "queue.queued")))
          (should (= 1 (length (pend--of 'continuable))))
          (should (string-match-p "rounds paused at 1/256"
                                  (plist-get (car (pend--of 'continuable)) :detail)))
          (should (= 1 (superchat-work-rounds id))))))))

;;;; 8 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/8-only-the-users-own-execution-opens-rounds ()
  (pend--fresh
    ;; A scheduled task's execution starts a record and stops.
    (un--with-task (list (gr--start) '(:text "Ten of the items are ticked.")) (session fired)
      (ignore fired)
      (gr--quiet)
      (should (equal '("timers") (gr--actors session)))
      ;; The record exists and its rounds were not opened.
      (should (gr--record))
      (should (= 0 (hash-table-count superchat-work--continuing)))
      (should (= 0 (gr--count session "queue.queued")))))
  (pend--fresh
    ;; A woken record's execution ends with the record still continuable.
    (wrec--with '((:text "WOKEN ticked ten more.")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((_pair (wwf--due-record p "GR woken"))
                 (views (superchat-work-wake-due)))
            (gr--quiet)
            (let ((session (hm--session (car views))))
              (should (equal '("work-records") (gr--actors session)))
              (should (= 0 (gr--count session "queue.queued")))))))))
  (pend--fresh
    ;; A sub-agent's execution asks to start a record, reports and ends.
    (wrec--with (list '(:tool-uses (("delegate_to_subagent" (preset . "researcher")
                                     (task . "GR-SUB-TASK look into it"))))
                      (gr--start)
                      '(:text "GR-SUB-REPORT")
                      '(:text "The research is back."))
        (p)
      (setq superchat-llm-tool-names
            (append superchat-llm-tool-names '("delegate_to_subagent")))
      (let* ((view (wrec--view p))
             (run (smoke--send view "delegate the research")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (gr--quiet)
        (should (= 4 (length hm--wires)))
        (should (= 0 (hash-table-count superchat-work--continuing)))
        (should (= 0 (gr--queued-anywhere)))))))

;;;; 9 ----------------------------------------------------------------------------

(ert-deftest goal-rounds/9-no-round-while-the-conversation-has-something-queued ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked." :delay 0.4)
                      (wrec--call '(action . "done") '(conclusion . "GR-DONE"))
                      '(:text "Done."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (run (smoke--send view "tick them all off")))
        ;; The user types again while the first execution is under way.
        (smoke--send view "and one more thing")
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
        (gr--quiet)
        ;; The one queued item is the user's; no round was queued beside it.
        (should (= 1 (gr--count session "queue.queued")))
        (should-not (member "goal-rounds" (gr--actors session)))))))

;;;; 10 ---------------------------------------------------------------------------

(ert-deftest goal-rounds/10-an-inline-request-never-queues-a-round ()
  (pend--fresh
    (wrec--with '((:text "INLINE-ANSWER") (:text "INLINE-ACT-ANSWER")) (p)
      (let* ((source (wrec--file p)))
        ;; The record's rounds go on; only the kind of request stands in the way.
        (superchat-work-continue
         (superchat-work-start
          (hm--session (with-current-buffer source (superchat))) gr--goal))
        (with-current-buffer source
          (superchat-inline-ask "what is this" (cons (point-min) (point-max))))
        (should (smoke--wait (lambda () (= 1 (length hm--wires))) 10))
        (gr--quiet)
        (with-current-buffer source (superchat-inline-act "read it"))
        (should (smoke--wait (lambda () (= 2 (length hm--wires))) 10))
        (gr--quiet)
        (should (= 0 (gr--queued-anywhere)))))))

;;;; 11 ---------------------------------------------------------------------------

(ert-deftest goal-rounds/11-a-round-whose-conversation-was-closed-is-dropped ()
  "A known limit of v1, measured here and not counted as fixed."
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked." :delay 0.4)) (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (run (smoke--send view "tick them all off")))
        (should (stringp run))
        (kill-buffer view)
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
        (gr--quiet)
        (should (= 1 (gr--count session "queue.dropped")))
        (should-not (member "goal-rounds" (gr--actors session)))))))

;;;; 12, 13: the user's command (rulings 2 and 4; not among the eleven) ----------

(ert-deftest goal-rounds/12-the-users-command-starts-a-record-and-opens-rounds ()
  (pend--fresh
    (wrec--with (list '(:text "Ten of the items are ticked.")
                      (wrec--call '(action . "done") '(conclusion . "GR-ALL-TICKED"))
                      '(:text "All ticked."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (hm--turn view (concat "/work " gr--goal))
        (gr--quiet)
        (let* ((id (gr--record))
               (record (and id (superchat-work-get id))))
          (should record)
          (should (equal gr--goal (plist-get record :goal)))
          ;; The model was sent the goal, not the command.
          (should (string-match-p (regexp-quote gr--goal) (wrec--user-text 1)))
          (should-not (string-match-p "/work" (wrec--user-text 1)))
          (should (equal '("lead" "goal-rounds") (gr--actors session)))
          (should (= 1 (gr--count session "queue.queued")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest goal-rounds/13-the-users-command-resumes-a-stopped-record ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")
                      '(:text "NEVER-DELIVERED" :delay 5)
                      (wrec--call '(action . "done") '(conclusion . "GR-DONE"))
                      '(:text "Done."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (hm--turn view "tick them all off")
        (let ((run (gr--round-run session))
              (id (superchat-work-current session))
              (wires (length hm--wires)))
          (should (superchat-runtime-cancel run))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
          (gr--quiet)
          (should (= 1 (gr--count session "queue.queued")))
          ;; The command itself asks the model nothing; it queues the round.
          (smoke--send view "/work resume")
          (gr--quiet)
          (should (= 2 (gr--count session "queue.queued")))
          (should (> (length hm--wires) wires))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

;;;; 14, 15: added with the mutation pass --------------------------------------

(defvar superchat-work-rounds-when-started)

(ert-deftest goal-rounds/14-the-option-keeps-a-started-record-out-of-rounds ()
  (pend--fresh
    (let ((superchat-work-rounds-when-started nil))
      (wrec--with (list (gr--start) '(:text "Ten of the items are ticked.")) (p)
        (let* ((view (wrec--view p)) (session (hm--session view)))
          (gr--first-turn view)
          (should (gr--record))
          (should (= 0 (gr--count session "queue.queued")))
          (should (equal '("lead") (gr--actors session))))))))

(ert-deftest goal-rounds/15-rounds-are-resumed-only-in-the-users-own-execution ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session gr--goal))
             (superchat--tool-execution-context (list :session-id session :run-id nil)))
        ;; Outside any execution of the user's, the tool refuses.
        (should (string-match-p "only in an execution the user started"
                                (superchat-work-tool "resume")))
        (should-not (superchat-work-continuing-p id))))))

(ert-deftest goal-rounds/16-rounds-are-counted-for-each-record ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (first (superchat-work-start session "GR-FIRST"))
             (second (superchat-work-start session "GR-SECOND")))
        (superchat-work--append session "work.round" "a round on the first"
                                (list :work-id first :round 1 :limit 256))
        (should (= 1 (superchat-work-rounds first)))
        (should (= 0 (superchat-work-rounds second)))))))

;;; goal-rounds-regression.el ends here
