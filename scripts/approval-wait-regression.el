;;; approval-wait-regression.el --- A person deciding is not a stalled model -*- lexical-binding: t; -*-

;;; Commentary:

;; While an approval question is open the provider attempt's inactivity clock
;; does not run; after the decision it continues from what remained.  Contract:
;; docs/decisions/accepted/2026-09-18-approval-wait-pauses-the-inactivity-clock.md
;;
;; Every test drives a scripted model through the real execution pipeline with
;; an inactivity timeout far shorter than the time the answerer takes.
;;
;;   emacs -Q --batch ... -l scripts/approval-wait-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^approval-wait/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar aw--effect 0)
(defvar aw--probe nil "What `aw--probe-clock' saw from inside the approved tool body.")
(defvar aw--run nil)

(defconst aw--timeout 1
  "Inactivity timeout in seconds.  An integer: a tool attempt gets
(floor (* 1.5 this)), so a fraction below 1 would mean no window at all.")
(defconst aw--read-seconds 3.5 "How long the answerer takes: several inactivity windows.")

(defmacro aw--pipeline (script &rest body)
  "Run BODY in the scripted fixture, default tools offered, the watchdog ON."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-function nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'edit-buffers))
       (smoke--with-fixture ,script
         ;; The fixture turns the watchdog off; these tests are about it.
         (let ((superchat-response-timeout aw--timeout)
               (superchat-response-timeout-hard-limit 60)
               (superchat-llm-tool-names superchat-llm-default-tool-names)
               (superchat-llm-tools-list nil)
               (aw--effect 0) (aw--probe nil) (aw--run nil))
           (unwind-protect (progn ,@body)
             (setq superchat-llm-tools-list nil)))))))

(defun aw--read-for (seconds)
  "Be a person reading for SECONDS: timers run, as they do under a minibuffer."
  (let ((until (+ (float-time) seconds)))
    (while (< (float-time) until)
      (accept-process-output nil 0.05))))

(defun aw--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun aw--count (session type)
  (cl-count-if (lambda (event) (equal type (plist-get event :type)))
               (superchat-tape-replay-events session)))

(defun aw--shown (view)
  "Return everything VIEW shows the user."
  (with-current-buffer view
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aw--clock (run)
  "Return (ARMED-P . SECONDS-LEFT) of RUN's inactivity timer."
  (let* ((owner (superchat-runtime-transport-timer-owner run))
         (timer (and (superchat-llm--transport-timer-owner-p owner)
                     (superchat-llm--transport-timer-owner-wait-timer owner))))
    (cons (and (timerp timer) (memq timer timer-list) t)
          (and (timerp timer)
               (float-time (time-subtract (timer--time timer) (current-time)))))))

(defun aw--probe-clock ()
  "Called by the approved tool body: record the clock, do the visible effect."
  (setq aw--probe (aw--clock aw--run))
  (cl-incf aw--effect))

(defconst aw--eval-round
  '(:tool-uses (("eval_elisp" (expression . "(aw--probe-clock)")))))

(ert-deftest approval-wait/allowed-after-a-long-read-the-tool-runs ()
  "The person reads for several inactivity windows and allows: no timeout."
  (aw--pipeline (list aw--eval-round '(:text "AW_DONE"))
    (let* ((superchat-approval-function
            (lambda (_public) (aw--read-for aw--read-seconds) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (aw--settle (smoke--send view "go"))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 aw--effect))
      (should (= 1 (aw--count session "tool.completed")))
      (should-not (string-match-p "Response timeout" (aw--shown view))))))

(ert-deftest approval-wait/rejected-after-a-long-read-is-a-rejection-not-a-timeout ()
  (aw--pipeline (list aw--eval-round '(:text "AW_DECLINED"))
    (let* ((superchat-approval-function
            (lambda (_public) (aw--read-for aw--read-seconds) nil))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (aw--settle (smoke--send view "go"))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 0 aw--effect))
      (should (= 2 (length smoke--requests)))
      (should (string-match-p "Tool call denied by user"
                              (or (plist-get (car smoke--requests) :prompt-text) "")))
      (should-not (string-match-p "Response timeout" (aw--shown view))))))

(ert-deftest approval-wait/a-cancel-during-the-read-is-not-undone-by-a-late-yes ()
  "Guard (M6 rule): the Run is cancelled while the person reads; the late yes
starts nothing, requests nothing, and re-arms nothing."
  (aw--pipeline (list aw--eval-round '(:text "must not be requested"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (stopped nil) (cancelled nil) (revived 0)
           (watch (lambda (timer &rest _)
                    (when (and cancelled (eq timer stopped)) (cl-incf revived))))
           (superchat-approval-function
            (lambda (public)
              ;; The very timer the question stopped: it must never run again.
              (setq stopped (superchat-llm--transport-timer-owner-wait-timer
                             (superchat-runtime-transport-timer-owner
                              (plist-get public :run-id))))
              (aw--read-for 0.3)
              (superchat-runtime-cancel (plist-get public :run-id) "[aw cancel while reading]")
              (setq cancelled t)
              (aw--read-for aw--read-seconds)
              t))
           (run (unwind-protect
                    (progn (advice-add 'timer-activate :before watch)
                           (aw--settle (smoke--send view "go")))
                  (advice-remove 'timer-activate watch))))
      (should (= 0 aw--effect))
      (should (= 0 (aw--count session "tool.execution.started")))
      (should (= 1 (aw--count session "approval.decided")))
      (should (= 1 (length smoke--requests)))
      (should (timerp stopped))
      ;; A timer revived by the late yes would fire harmlessly and be gone by
      ;; now, so the re-activation itself is what is watched.
      (should (= 0 revived))
      (should-not (memq stopped timer-list))
      (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest approval-wait/provider-silence-with-nobody-waiting-still-times-out ()
  "Guard against over-fixing: the watchdog itself is untouched."
  (aw--pipeline (list (list :chunks '("AW_PARTIAL") :hang t))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (aw--settle (smoke--send view "go"))))
      (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
      (should (string-match-p "Response timeout: no progress" (aw--shown view)))
      (should (= 0 (aw--count session "approval.requested"))))))

(ert-deftest approval-wait/the-clock-stops-for-the-question-and-continues-after ()
  "While the question is open the attempt's inactivity timer is not queued;
inside the approved tool body -- same attempt -- it is queued again and the
person's reading time has not been taken out of what was left."
  (aw--pipeline (list aw--eval-round '(:text "AW_DONE"))
    (let* ((during nil)
           (superchat-approval-function
            (lambda (public)
              (setq aw--run (plist-get public :run-id)
                    during (aw--clock aw--run))
              ;; Read for longer than the whole window that was left.
              (aw--read-for (+ 1.0 (cdr during)))
              t))
           (view (smoke--open-chat))
           (run (aw--settle (smoke--send view "go"))))
      (should (equal run aw--run))
      (should-not (car during))
      (should (car aw--probe))
      ;; What was left when the question opened is still there, plus the one
      ;; renewal M6 always gave; the reading time was not charged.
      (should (>= (cdr aw--probe)
                  (+ (cdr during)
                     (buffer-local-value 'superchat--timeout-extension-amount view)
                     -0.5))))))

(ert-deftest approval-wait/a-stopped-clock-is-not-revived-once-its-slot-moved-on ()
  "The resume step itself: it continues only the very timer it stopped, and
only while the owner still holds it.  An attempt that ended (slot emptied) or a
newer attempt (slot replaced) is left alone; a second, nested stop is a no-op."
  (with-temp-buffer
    (let* ((owner (superchat-llm--make-transport-timer-owner :buffer (current-buffer)))
           (fired 0)
           (arm (lambda ()
                  (let ((timer (run-at-time 60 nil (lambda () (cl-incf fired)))))
                    (setf (superchat-llm--transport-timer-owner-wait-timer owner) timer)
                    timer)))
           (first (funcall arm)))
      (unwind-protect
          (progn
            ;; Stop and continue: same timer, time left carried over.
            (let ((token (superchat-llm--transport-timer-owner-suspend-wait owner)))
              (should (eq first (car token)))
              (should (> (cdr token) 59))
              (should-not (memq first timer-list))
              (should-not (superchat-llm--transport-timer-owner-suspend-wait owner))
              (should (superchat-llm--transport-timer-owner-resume-wait owner token))
              (should (memq first timer-list)))
            ;; The attempt ended while the question was open.
            (let ((token (superchat-llm--transport-timer-owner-suspend-wait owner)))
              (superchat-llm--transport-timer-owner-cancel-wait owner)
              (should-not (superchat-llm--transport-timer-owner-resume-wait owner token))
              (should-not (memq first timer-list)))
            ;; A newer attempt took the slot while the question was open.
            (let* ((second (funcall arm))
                   (token (superchat-llm--transport-timer-owner-suspend-wait owner))
                   (third (funcall arm)))
              (should (eq second (car token)))
              (should-not (superchat-llm--transport-timer-owner-resume-wait owner token))
              (should-not (memq second timer-list))
              (should (memq third timer-list)))
            (should (= 0 fired)))
        (dolist (timer timer-list)
          (when (memq timer (list first (superchat-llm--transport-timer-owner-wait-timer owner)))
            (cancel-timer timer)))
        (superchat-llm--transport-timer-owner-cancel owner)))))

;;;; The question is dismissed when its Run ends -----------------------------------
;; docs/decisions/accepted/2026-09-19-a-question-is-dismissed-when-its-run-ends.md

(defun aw--decided (session)
  (cl-loop for event in (superchat-tape-replay-events session)
           when (equal "approval.decided" (plist-get event :type))
           collect (plist-get event :payload)))

(defmacro aw--with-announcements (var &rest body)
  (declare (indent 1) (debug t))
  `(let* ((,var nil)
          (note (lambda (text) (push text ,var))))
     (unwind-protect
         (progn (advice-add 'superchat-approval--announce :before note) ,@body)
       (advice-remove 'superchat-approval--announce note))))

;; 2026-09-20: the Run deadline waits for an open question
;; (docs/decisions/accepted/2026-09-20-the-run-deadline-waits-for-an-open-question.md).
;; The case this test used to pin -- the deadline arriving with the question
;; open, and the question being dismissed for it -- cannot happen any more.

(ert-deftest approval-wait/the-run-deadline-waits-for-the-question ()
  "The person reads for twice the Run's deadline and allows: the question is
still theirs to answer, the tool runs, the Run completes, nothing is dismissed."
  (aw--pipeline (list aw--eval-round '(:text "AW_DONE"))
    (aw--with-announcements told
      (let* ((superchat-response-timeout nil)
             (superchat-runtime-default-deadline 2)
             (superchat-approval-function
              (lambda (_public) (aw--read-for 4) t))
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (aw--settle (smoke--send view "go"))))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (should (= 1 aw--effect))
        (should (equal "allowed-once" (plist-get (car (aw--decided session)) :outcome)))
        (should-not told)))))

(ert-deftest approval-wait/the-run-deadline-continues-with-what-was-left ()
  "Postponed, not switched off: after the answer the model goes silent and the
Run times out on the deadline, by what remained of it."
  (aw--pipeline (list aw--eval-round (list :chunks '("AW_PARTIAL") :hang t))
    (let* ((superchat-response-timeout nil)
           (superchat-runtime-default-deadline 2)
           (answered-at nil)
           (superchat-approval-function
            (lambda (_public) (aw--read-for 3) (setq answered-at (float-time)) t))
           (view (smoke--open-chat))
           (run (aw--settle (smoke--send view "go")))
           (ended-after (and answered-at (- (float-time) answered-at))))
      ;; The person did get to answer: the question was not given up.
      (should answered-at)
      (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 aw--effect))
      (should (string-match-p "Runtime deadline exceeded" (aw--shown view)))
      ;; About two seconds were left; none of the three spent reading came back.
      (should (< 1.0 ended-after 3.5)))))

(ert-deftest approval-wait/without-a-question-the-run-deadline-is-what-it-was ()
  "Guard: nobody is asked anything; the model goes silent; two seconds."
  (aw--pipeline (list (list :chunks '("AW_PARTIAL") :hang t))
    (let* ((superchat-response-timeout nil)
           (superchat-runtime-default-deadline 2)
           (start (float-time))
           (view (smoke--open-chat))
           (run (aw--settle (smoke--send view "go"))))
      (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
      (should (string-match-p "Runtime deadline exceeded" (aw--shown view)))
      (should (< 1.5 (- (float-time) start) 4)))))

(ert-deftest approval-wait/a-cancel-with-the-question-open-dismisses-it ()
  "The same when the Run is cancelled from somewhere else while the person reads."
  (aw--pipeline (list aw--eval-round '(:text "must not be requested"))
    (aw--with-announcements told
      (let* ((returned nil)
             (superchat-approval-function
              (lambda (public)
                (run-at-time 0.5 nil #'superchat-runtime-cancel
                             (plist-get public :run-id) "[aw cancelled elsewhere]")
                (aw--read-for 8) (setq returned t) t))
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (start (float-time)))
        (aw--settle (smoke--send view "go"))
        (should (smoke--wait (lambda () (aw--decided session)) 3))
        (should (< (- (float-time) start) 4))
        (should-not returned)
        (should (equal "run-ended" (plist-get (car (aw--decided session)) :interruption)))
        (should (= 0 aw--effect))
        (should told)))))

;;;; ask_user waits for the user too ---------------------------------------------
;; docs/decisions/accepted/2026-09-20-ask-user-waits-for-the-user-too.md

(defconst aw--ask-round '(:tool-uses (("ask_user" (question . "Which key?")))))

(defun aw--pending (run)
  (let (pending)
    (should (smoke--wait (lambda () (setq pending (superchat-runtime-pending-input run))) 5))
    pending))

(ert-deftest approval-wait/ask-user-waits-past-the-run-deadline ()
  "The model asks; the person answers after twice the Run's deadline; the
answer is accepted and the Run completes."
  (aw--pipeline (list aw--ask-round '(:text "AW_DONE"))
    (let* ((superchat-response-timeout nil)
           (superchat-runtime-default-deadline 2)
           (view (smoke--open-chat))
           (run (smoke--send view "go"))
           (pending (aw--pending run)))
      (aw--read-for 4)
      (should (superchat-runtime-pending-input run))
      (should-not (superchat-runtime--terminal-p (plist-get (superchat-runtime-status run) :status)))
      (should (superchat-runtime-answer-input run (plist-get pending :handle) "C-c t h"))
      (aw--settle run)
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 2 (length smoke--requests)))
      (should (string-match-p "C-c t h" (or (plist-get (car smoke--requests) :prompt-text) ""))))))

(ert-deftest approval-wait/ask-user-hands-the-deadline-back-with-what-was-left ()
  "Postponed, not switched off: after the answer the model goes silent and the
Run times out on the deadline."
  (aw--pipeline (list aw--ask-round (list :chunks '("AW_PARTIAL") :hang t))
    (let* ((superchat-response-timeout nil)
           (superchat-runtime-default-deadline 2)
           (view (smoke--open-chat))
           (run (smoke--send view "go"))
           (pending (aw--pending run))
           answered-at)
      (aw--read-for 3)
      (should (superchat-runtime-answer-input run (plist-get pending :handle) "C-c t h"))
      (setq answered-at (float-time))
      (aw--settle run)
      (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
      (should (string-match-p "Runtime deadline exceeded" (aw--shown view)))
      (should (< 1.0 (- (float-time) answered-at) 3.5)))))

(ert-deftest approval-wait/ask-user-stopped-while-waiting-ends-and-stays-ended ()
  "Guard: the person stops the Run instead of answering; a late answer is not
taken and nothing continues."
  (aw--pipeline (list aw--ask-round '(:text "must not be requested"))
    (let* ((superchat-response-timeout nil)
           (superchat-runtime-default-deadline 2)
           (view (smoke--open-chat))
           (run (smoke--send view "go"))
           (pending (aw--pending run)))
      (aw--read-for 3)
      (superchat-runtime-cancel run "[aw stopped while asked]")
      (aw--settle run)
      (should-not (superchat-runtime-answer-input run (plist-get pending :handle) "too late"))
      (should (memq (plist-get (superchat-runtime-status run) :status) '(cancelled audit-incomplete)))
      (should (= 1 (length smoke--requests))))))

(provide 'approval-wait-regression)

;;; approval-wait-regression.el ends here
