;;; m5-execution-regression.el --- Execution reliability regression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Covers the execution reliability fixes identified by the M5 operating
;; experiment: per-attempt silence windows, explicit reasoning configuration
;; in provider requests, and truthful tool-budget finalization.
;;
;; This is a standalone regression set. It reuses the canonical smoke fixture
;; as a library and does not load or modify scripts/m3-regression.el or
;; scripts/m5-regression.el. It requires no files from .scratch/.
;;
;; Every provider is scripted or only serializes a request locally. Timer
;; checks use a virtual clock plus one short real Emacs timer check. Authority,
;; Runtime, Tape, final delivery and View rebuilding are exercised by the
;; tool-budget cases. These tests do not call a real model.
;;
;; Run from the repository root, with an existing prepared dependency directory:
;;
;;   SUPERCHAT_TEST_ELPA=/path/to/prepared/elpa
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$SUPERCHAT_TEST_ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/m5-execution-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^m5-execution/")'
;;
;; Area selectors:
;;   ^m5-execution/wait/       silence, progress, retries and clock ownership
;;   ^m5-execution/reasoning/  mapping, raw overrides and final request dispatch
;;   ^m5-execution/budget/     ask/continue/finalize/stop and terminal evidence
;;
;; The protected M3/M5 sets and canonical smoke baselines remain separate gates.
;; Use the current request lifecycle contract and docs/roadmap.md for scope.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'llm-openai)
(require 'llm-ollama)

(when (getenv "SMOKE_REAL")
  (error "Execution regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Scope graphs contain parent/child cycles. Keep failures readable.
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; Provider attempt inactivity and clock ownership

(defvar m5w-now)
(defvar m5w-timers)
(defvar m5w-owner)
(defvar m5w-requests)
(defvar m5w-events)
(defvar m5w-final)
(defvar m5w-live)
(defvar m5w-output)
(defvar m5w-activity-hook nil)

(defun m5w--fire (timer)
  "Call TIMER's captured callback, including for deliberately stale timers."
  (apply (timer--function timer) (timer--args timer)))

(defun m5w--advance (seconds)
  "Advance the virtual clock and run eligible timers in deadline order."
  (let ((until (+ m5w-now seconds)))
    (while (let ((next (car (sort (copy-sequence m5w-timers)
                                 (lambda (a b)
                                   (time-less-p (timer--time a) (timer--time b)))))))
             (when (and next (<= (float-time (timer--time next)) until))
               (setq m5w-now (float-time (timer--time next))
                     m5w-timers (delq next m5w-timers))
               (m5w--fire next)
               t)))
    (setq m5w-now until)))

(defun m5w--request (ordinal)
  (nth (1- ordinal) m5w-requests))

(defun m5w--partial (ordinal value)
  (funcall (plist-get (m5w--request ordinal) :partial) value))

(defun m5w--response (ordinal value)
  (funcall (plist-get (m5w--request ordinal) :response) value))

(defun m5w--tools (ordinal)
  (m5w--response ordinal '(:tool-results (("noop" . "ok")))))

(defun m5w--status ()
  (plist-get (cdar (cl-remove-if-not
                   (lambda (event) (eq (car event) 'terminal)) m5w-events))
             :status))

(defun m5w--start ()
  (superchat--llm-generate-answer
   "Continue the requested work."
   (lambda (value) (when (stringp value) (setq m5w-final value)))
   (lambda (value) (push value m5w-output))
   nil nil 'none))

(defmacro m5w-with-run (&rest body)
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((m5w-now 1000.0) (m5w-timers nil) (m5w-owner nil)
            (m5w-requests nil) (m5w-events nil) (m5w-final nil)
            (m5w-live t) (m5w-output nil) (m5w-activity-hook nil)
            (real-float-time (symbol-function 'float-time))
            (superchat-llm-backend
             (make-llm-openai-compatible :url "http://127.0.0.1:9"
                                         :chat-model "m5-wait-clock"))
            (superchat-llm-streaming t)
            (superchat-llm-reasoning nil)
            (superchat-show-ttft nil)
            (superchat-show-ttft-breakdown nil)
            (superchat-response-timeout 10)
            (superchat-response-timeout-hard-limit 60)
            (superchat-tool-timeout-multiplier 1)
            (superchat-llm-max-tool-rounds nil)
            (superchat--llm-tools-prepared-p t)
            (superchat--llm-prepared-tools nil)
            (superchat--llm-evidence-context nil)
            (superchat--llm-request-context nil)
            (superchat--llm-run-active-p (lambda () m5w-live))
            (superchat--llm-transport-timer-owner-function
             (lambda (owner) (setq m5w-owner owner) "m5-wait-run"))
            (superchat--llm-runtime-observer
             (lambda (event value)
               (push (cons event value) m5w-events)
               (pcase event
                 ('before-input-dispatch t)
                 (_ nil))))
            (superchat--pause-requested nil)
            (superchat--pause-run-id nil)
            (superchat--pending-send-request nil)
            (superchat-llm--distribution-adapters
             (list :adjusted-timeout-function
                   (lambda (_mode) superchat-response-timeout)
                   :activity-function
                   (lambda (owner)
                     ;; Preserve the distribution's current +30-second
                     ;; adapter while checking transport-owned inactivity.
                     (superchat-llm--transport-timer-owner-extend-wait owner 30)
                     (when m5w-activity-hook (funcall m5w-activity-hook owner))))))
       (cl-letf (((symbol-function 'current-time)
                  (lambda () (seconds-to-time m5w-now)))
                 ((symbol-function 'float-time)
                  (lambda (&optional time)
                    (if time (funcall real-float-time time) m5w-now)))
                 ((symbol-function 'timer-activate)
                  (lambda (timer &rest _)
                    (cl-pushnew timer m5w-timers :test #'eq) timer))
                 ((symbol-function 'run-with-timer)
                  (lambda (seconds repeat function &rest args)
                    (should-not repeat)
                    (let ((timer (timer-create)))
                      (timer-set-time timer (seconds-to-time (+ m5w-now seconds)))
                      (timer-set-function timer function args)
                      (timer-activate timer)
                      timer)))
                 ((symbol-function 'cancel-timer)
                  (lambda (timer)
                    (setq m5w-timers (delq timer m5w-timers)) nil))
                 ((symbol-function 'superchat--llm-estimate-context-tokens)
                  (lambda (&rest _) 1))
                 ((symbol-function 'superchat--llm-record-request-evidence)
                  (lambda (&rest _) nil))
                 ((symbol-function 'llm-chat-async)
                  (lambda (_provider prompt response error &optional _multi)
                    (setq m5w-requests
                          (append m5w-requests
                                  (list (list :prompt prompt :response response
                                              :error error :started m5w-now))))
                    nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt partial response error &optional _multi)
                    (setq m5w-requests
                          (append m5w-requests
                                  (list (list :prompt prompt :partial partial
                                              :response response :error error
                                              :started m5w-now))))
                    nil)))
         (unwind-protect
             (progn ,@body)
           (when m5w-owner
             (superchat-llm--transport-timer-owner-cancel m5w-owner)))))))

(ert-deftest m5-execution/wait/each-tool-followup-starts-a-new-window ()
  (m5w-with-run
    (m5w--start)
    (dotimes (index 3)
      (m5w--advance 8)
      (m5w--tools (1+ index)))
    (should (= 4 (length m5w-requests)))
    (should-not m5w-final)
    (m5w--response 4 '(:text "done"))
    (should (eq 'completed (m5w--status)))))

(ert-deftest m5-execution/wait/preface-does-not-disable-next-attempt ()
  (m5w-with-run
    (m5w--start)
    (m5w--partial 1 '(:text "I will check."))
    (m5w--tools 1)
    (m5w--advance 11)
    (should (= 2 (length m5w-requests)))
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/stale-timer-cannot-end-a-later-attempt ()
  (m5w-with-run
    (m5w--start)
    (let ((old (superchat-llm--transport-timer-owner-wait-timer m5w-owner)))
      (m5w--advance 8)
      (m5w--tools 1)
      (let ((current (superchat-llm--transport-timer-owner-wait-timer m5w-owner)))
        (should-not (eq old current))
        (m5w--fire old)
        (should-not m5w-final)
        (should (eq current (superchat-llm--transport-timer-owner-wait-timer m5w-owner)))))))

(ert-deftest m5-execution/wait/text-keeps-only-a-sliding-idle-window ()
  (m5w-with-run
    (m5w--start)
    (m5w--advance 8)
    (m5w--partial 1 '(:text "one"))
    (m5w--advance 8)
    (m5w--partial 1 '(:text "one two"))
    (should-not m5w-final)
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))
    (should (< m5w-now 1060))))

(ert-deftest m5-execution/wait/reasoning-with-empty-text-counts-as-progress ()
  (m5w-with-run
    (m5w--start)
    (m5w--advance 8)
    (m5w--partial 1 '(:text "" :reasoning "one"))
    (m5w--advance 8)
    (m5w--partial 1 '(:text "" :reasoning "one two"))
    (should-not m5w-final)
    (should (member '(:reasoning " two") m5w-output))
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/raw-tool-argument-progress-refreshes-wait ()
  (m5w-with-run
    (m5w--start)
    (m5w--advance 8)
    (m5w--partial 1 '(:tool-uses-raw ((:index 0 :arguments "("))))
    (m5w--advance 8)
    (m5w--partial 1 '(:tool-uses-raw ((:index 0 :arguments "(foo)"))))
    (should-not m5w-final)
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/repeated-or-empty-chunks-do-not-refresh-wait ()
  (m5w-with-run
    (m5w--start)
    (m5w--partial 1 '(:text "one" :reasoning "a"))
    (m5w--advance 8)
    (m5w--partial 1 '(:text "one" :reasoning "a"))
    (m5w--partial 1 '(:text ""))
    (m5w--advance 3)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/progress-cannot-extend-the-hard-cap ()
  (m5w-with-run
    (let ((superchat-response-timeout-hard-limit 30))
      (m5w--start)
      (let ((hard (superchat-llm--transport-timer-owner-hard-timer m5w-owner)))
        (dotimes (index 3)
          (m5w--advance 8)
          (m5w--partial (1+ index) '(:reasoning "still working"))
          (m5w--tools (1+ index))
          (should (eq hard (superchat-llm--transport-timer-owner-hard-timer m5w-owner))))
        (m5w--advance 7)
        (should (eq 'timed-out (m5w--status)))
        (should (string-match-p "hard timeout" m5w-final))))))

(ert-deftest m5-execution/wait/cancelled-hard-callback-is-inert ()
  (m5w-with-run
    (m5w--start)
    (let ((hard (superchat-llm--transport-timer-owner-hard-timer m5w-owner)))
      (superchat-llm--transport-timer-owner-cancel m5w-owner)
      (m5w--fire hard)
      (should-not m5w-final))))

(ert-deftest m5-execution/wait/ordinary-pause-resume-rearms-only-on-dispatch ()
  (m5w-with-run
    (m5w--start)
    (setq superchat--pause-requested t superchat--pause-run-id "m5-wait-run")
    (cl-letf (((symbol-function 'superchat--pause-boundary)
               (lambda (_run owner)
                 (superchat-llm--transport-timer-owner-cancel-wait owner) t)))
      (m5w--tools 1))
    (should (= 1 (length m5w-requests)))
    (should (superchat-llm--pending-pause-p superchat--pending-send-request))
    (should-not (superchat-llm--transport-timer-owner-wait-timer m5w-owner))
    (m5w--advance 15)
    (should-not m5w-final)
    (setq superchat--pause-requested nil)
    (funcall (superchat-llm--pending-pause-continuation superchat--pending-send-request)
             'tool-followup)
    (should (= 2 (length m5w-requests)))
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/activity-reentry-cannot-rearm-a-retired-attempt ()
  (m5w-with-run
    (m5w--start)
    (setq m5w-activity-hook
          (lambda (_owner)
            (setq m5w-activity-hook nil)
            (m5w--tools 1)))
    (m5w--partial 1 '(:reasoning "checking"))
    (should (= 2 (length m5w-requests)))
    (should (= 2 (superchat-llm--transport-timer-owner-wait-generation m5w-owner)))
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/input-clock-resumes-remaining-hard-budget ()
  (m5w-with-run
    (let* ((owner (superchat-llm--make-transport-timer-owner
                   :buffer (current-buffer) :input-clocks-p t))
           (fired nil))
      (superchat-llm--input-arm-hard owner 30 (lambda () (setq fired t)))
      (let ((old (superchat-llm--transport-timer-owner-hard-timer owner)))
        (m5w--advance 8)
        (superchat-llm--input-pause-clock owner (make-symbol "question"))
        (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner)))
        (should (= 22 (superchat-llm--transport-timer-owner-hard-remaining owner)))
        (m5w--advance 100)
        (should-not fired)
        (superchat-llm--input-arm-hard
         owner (superchat-llm--transport-timer-owner-hard-remaining owner)
         (lambda () (setq fired t)))
        (m5w--fire old)
        (should-not fired)
        (m5w--advance 23)
        (should fired)
        (superchat-llm--transport-timer-owner-cancel owner)))))


(ert-deftest m5-execution/wait/old-text-after-activity-reentry-is-not-a-final-answer ()
  (m5w-with-run
    (m5w--start)
    (setq m5w-activity-hook
          (lambda (_owner)
            (setq m5w-activity-hook nil)
            (m5w--tools 1)))
    (m5w--partial 1 '(:text "OLD-PREFACE"))
    (m5w--response 2 nil)
    (should (equal "[Empty response from llm]" m5w-final))
    (should-not (member "OLD-PREFACE" m5w-output))))

(ert-deftest m5-execution/wait/repeated-legacy-tool-marker-does-not-refresh ()
  (m5w-with-run
    (m5w--start)
    (m5w--partial 1 '(tool-call . "same-event"))
    (m5w--advance 8)
    (m5w--partial 1 '(tool-call . "same-event"))
    (m5w--advance 3)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/repeated-raw-tool-data-does-not-refresh ()
  (m5w-with-run
    (m5w--start)
    (m5w--partial 1 '(:tool-uses-raw [((index . 0) (arguments . "(foo)"))]))
    (m5w--advance 8)
    (m5w--partial 1 '(:tool-uses-raw [((index . 0) (arguments . "(foo)"))]))
    (m5w--advance 3)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/cancel-reentry-cannot-replace-next-attempt-clock ()
  (m5w-with-run
    (setq superchat-llm--distribution-adapters
          (plist-put superchat-llm--distribution-adapters :activity-function nil))
    (m5w--start)
    (let ((base-cancel (symbol-function 'cancel-timer))
          once)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (funcall base-cancel timer)
                   (unless once
                     (setq once t)
                     (m5w--tools 1)))))
        (m5w--partial 1 '(:reasoning "new"))))
    (should (= 2 (length m5w-requests)))
    (should (= 2 (superchat-llm--transport-timer-owner-wait-generation m5w-owner)))
    (m5w--advance 11)
    (should (eq 'timed-out (m5w--status)))))

(ert-deftest m5-execution/wait/input-dispatch-resumes-only-the-remaining-budget ()
  (m5w-with-run
    (let ((superchat-response-timeout-hard-limit 30)
          ;; This fixture supplies Runtime's private opt-in identity; no
          ;; actual model tool or Authority operation is invoked.
          (superchat--tool-execution-context
           '(:input-core-token test-input :input-core-tool-name "ask_user")))
      (m5w--start)
      (should (superchat-llm--transport-timer-owner-input-clocks-p m5w-owner))
      (m5w--advance 8)
      (superchat-llm--input-pause-clock m5w-owner (make-symbol "accepted-question"))
      (should-not (superchat-llm--transport-timer-owner-wait-timer m5w-owner))
      (m5w--advance 100)
      (should-not m5w-final)
      (m5w--tools 1)
      (should (= 2 (length m5w-requests)))
      (should (= 22 (superchat-llm--transport-timer-owner-hard-remaining m5w-owner)))
      (let ((deadline (superchat-llm--transport-timer-owner-hard-deadline m5w-owner)))
        (m5w--advance 8)
        (m5w--tools 2)
        (m5w--advance 8)
        (m5w--partial 3 '(:reasoning "still progressing"))
        (should (= deadline (superchat-llm--transport-timer-owner-hard-deadline m5w-owner)))
        (m5w--advance 7)
        (should (eq 'timed-out (m5w--status)))
        (should (string-match-p "hard timeout" m5w-final))))))

(ert-deftest m5-execution/wait/real-emacs-timer-expires-the-next-attempt ()
  (let ((real-current (symbol-function 'current-time))
        (real-float (symbol-function 'float-time))
        (real-activate (symbol-function 'timer-activate))
        (real-cancel (symbol-function 'cancel-timer))
        (real-run (symbol-function 'run-with-timer)))
    (m5w-with-run
      (cl-letf (((symbol-function 'current-time) real-current)
                ((symbol-function 'float-time) real-float)
                ((symbol-function 'timer-activate) real-activate)
                ((symbol-function 'cancel-timer) real-cancel)
                ((symbol-function 'run-with-timer) real-run))
        (unwind-protect
            (let ((superchat-response-timeout 0.05)
                  (superchat-response-timeout-hard-limit 2))
              (m5w--start)
              (m5w--partial 1 '(:text "checking"))
              (m5w--tools 1)
              (let ((until (+ (float-time) 1)))
                (while (and (not m5w-final) (< (float-time) until))
                  (accept-process-output nil 0.01)))
              (should (= 2 (length m5w-requests)))
              (should (eq 'timed-out (m5w--status)))
              (should (string-match-p "attempt: 2" m5w-final)))
          (when m5w-owner
            (superchat-llm--transport-timer-owner-cancel m5w-owner)))))))

(ert-deftest m5-execution/wait/disabled-timeout-keeps-activity-notifications ()
  (m5w-with-run
    (let ((superchat-response-timeout nil)
          (notifications 0))
      (setq m5w-activity-hook
            (lambda (_owner) (cl-incf notifications)))
      (m5w--start)
      (m5w--partial 1 '(:text "one" :reasoning "a"))
      (should (= notifications 2))
      (should-not (superchat-llm--transport-timer-owner-wait-timer m5w-owner))
      (should-not (superchat-llm--transport-timer-owner-hard-timer m5w-owner))
      (m5w--advance 1000)
      (should-not m5w-final)
      (m5w--response 1 '(:text "done"))
      (should (equal "done" m5w-final)))))

(ert-deftest m5-execution/wait/callback-only-provider-also-rearms-per-attempt ()
  (m5w-with-run
    (let ((superchat-llm-streaming nil))
      (m5w--start)
      (m5w--advance 8)
      (m5w--tools 1)
      (m5w--advance 8)
      (m5w--tools 2)
      (should-not m5w-final)
      (m5w--advance 11)
      (should (= 3 (length m5w-requests)))
      (should (eq 'timed-out (m5w--status))))))


(ert-deftest m5-execution/wait/empty-tool-fields-cannot-refresh-or-change-phase ()
  (m5w-with-run
    (m5w--start)
    (m5w--advance 8)
    (m5w--partial 1 (list :tool-uses-raw [] :tool-uses ""
                         :tool-results (make-hash-table)))
    (m5w--advance 1)
    ;; Moving the same empty values between fields must not look like a
    ;; different cumulative tool stream.
    (m5w--partial 1 (list :tool-uses-raw "" :tool-uses (make-hash-table)
                         :tool-results []))
    (m5w--advance 2)
    (should (eq 'timed-out (m5w--status)))
    (should-not (member '(model-phase . tool-streaming) m5w-events))
    (should (string-match-p "waiting-first-token" m5w-final))))

(ert-deftest m5-execution/wait/tool-progress-reports-its-own-phase ()
  (dolist (payload '((:tool-uses-raw [((index . 0) (arguments . "(foo)"))])
                     (tool-call . "entered-tool")))
    (m5w-with-run
      (m5w--start)
      (m5w--partial 1 payload)
      (should (member '(model-phase . tool-streaming) m5w-events))
      (m5w--advance 11)
      (should (eq 'timed-out (m5w--status)))
      (should (string-match-p "last phase: tool-streaming" m5w-final)))))

(ert-deftest m5-execution/wait/tool-phase-observer-reentry-keeps-next-stream-fresh ()
  (m5w-with-run
    (let ((base-observer superchat--llm-runtime-observer)
          reentered
          (payload '(:tool-uses-raw [((index . 0) (arguments . "(foo)"))])))
      (setq superchat--llm-runtime-observer
            (lambda (event value)
              (prog1 (funcall base-observer event value)
                (when (and (eq event 'model-phase) (eq value 'tool-streaming)
                           (not reentered))
                  (setq reentered t)
                  (m5w--tools 1)))))
      (m5w--start)
      (m5w--advance 8)
      (m5w--partial 1 payload)
      (should (= 2 (length m5w-requests)))
      (m5w--advance 8)
      ;; This is the new attempt's first real data.  The retired callback
      ;; must not have installed its identical payload into the new cursor.
      (m5w--partial 2 payload)
      (m5w--advance 3)
      (should-not m5w-final)
      (m5w--response 2 '(:text "new attempt finished"))
      (should (equal "new attempt finished" m5w-final)))))

;;;; Reasoning configuration in actual request JSON

(defconst m5-reasoning--url "http://localhost:1234/v1/chat/completions")
(defconst m5-reasoning--model "qwen/qwen3.8-27b")

(defun m5-reasoning--provider (&optional params url model)
  (make-llm-openai-compatible
   :url (or url "http://localhost:1234/v1/")
   :chat-model (or model m5-reasoning--model)
   :default-chat-non-standard-params (copy-tree params t)))

(defun m5-reasoning--declaration (&optional maximum)
  (list
   (list
    (list 'llm-openai-compatible m5-reasoning--url m5-reasoning--model)
    'reasoning_effort
    '(none . "none") '(light . "low") '(medium . "medium")
    (cons 'maximum (or maximum "high")))))

(defun m5-reasoning--wire-prompt (provider prompt)
  (superchat--llm-prepare-reasoning provider prompt)
  (json-parse-string
   (llm-provider-utils-json-serialize
    (llm-provider-chat-request provider prompt nil))
   :object-type 'alist :array-type 'list
   :null-object nil :false-object :false))

(defun m5-reasoning--wire (provider effort &optional params)
  (m5-reasoning--wire-prompt
   provider (llm-make-chat-prompt
             "Only calibrate request construction."
             :reasoning effort :non-standard-params (copy-tree params t))))

(ert-deftest m5-execution/reasoning/four-efforts-reach-final-request-json ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (dolist (case '((none . "none") (light . "low")
                    (medium . "medium") (maximum . "high")))
      (let ((body (m5-reasoning--wire (m5-reasoning--provider) (car case))))
        (should (equal (alist-get 'reasoning_effort body) (cdr case)))
        (should-not (assq 'reasoning body))))))

(ert-deftest m5-execution/reasoning/declaration-can-name-an-endpoints-maximum ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration "xhigh")))
    (should
     (equal "xhigh"
            (alist-get
             'reasoning_effort
             (m5-reasoning--wire (m5-reasoning--provider) 'maximum))))))

(ert-deftest m5-execution/reasoning/native-off-is-not-guessed-as-chat-none ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (list (list (list 'llm-openai-compatible
                          m5-reasoning--url m5-reasoning--model)
                     'reasoning_effort '(none . "off")))))
    (should
     (equal "off"
            (alist-get 'reasoning_effort
                       (m5-reasoning--wire (m5-reasoning--provider) 'none))))))

(ert-deftest m5-execution/reasoning/provider-default-means-no-automatic-field ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (let ((body (m5-reasoning--wire (m5-reasoning--provider) nil)))
      (should-not (assq 'reasoning_effort body))
      (should-not (assq 'reasoning body)))))

(ert-deftest m5-execution/reasoning/preset-inherit-follows-global ()
  (let* ((superchat-llm-reasoning 'light)
         (superchat-llm-reasoning-parameter-overrides
          (m5-reasoning--declaration))
         (preset (superchat-preset--create :reasoning 'inherit))
         (prompt (superchat--build-llm-prompt "calibrate" nil nil preset))
         (body (m5-reasoning--wire-prompt (m5-reasoning--provider) prompt)))
    (should (equal (alist-get 'reasoning_effort body) "low"))))

(ert-deftest m5-execution/reasoning/preset-explicit-none-beats-global ()
  (let* ((superchat-llm-reasoning 'maximum)
         (superchat-llm-reasoning-parameter-overrides
          (m5-reasoning--declaration))
         (preset (superchat-preset--create :reasoning 'none))
         (prompt (superchat--build-llm-prompt "calibrate" nil nil preset)))
    (should
     (equal "none"
            (alist-get 'reasoning_effort
                       (m5-reasoning--wire-prompt
                        (m5-reasoning--provider) prompt))))))

(ert-deftest m5-execution/reasoning/global-true-retains-medium-alias ()
  (let* ((superchat-llm-reasoning t)
         (superchat-llm-reasoning-parameter-overrides
          (m5-reasoning--declaration))
         (prompt (superchat--build-llm-prompt "calibrate" nil)))
    (should
     (equal "medium"
            (alist-get 'reasoning_effort
                       (m5-reasoning--wire-prompt
                        (m5-reasoning--provider) prompt))))))

(ert-deftest m5-execution/reasoning/raw-provider-setting-beats-preset ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (let ((body (m5-reasoning--wire
                 (m5-reasoning--provider '((reasoning_effort . "high")))
                 'none)))
      (should (equal (alist-get 'reasoning_effort body) "high"))
      (should-not (assq 'reasoning body)))))

(ert-deftest m5-execution/reasoning/raw-prompt-keeps-llm-precedence-over-provider ()
  ;; llm.el 0.32.1's actual merge gives prompt values precedence despite the
  ;; provider struct's misleading docstring.  This test checks serialized JSON.
  (let ((superchat-llm-reasoning-parameter-overrides nil))
    (let ((body (m5-reasoning--wire
                 (m5-reasoning--provider '((reasoning_effort . "high")))
                 'none '((reasoning_effort . "low")))))
      (should (equal (alist-get 'reasoning_effort body) "low")))))

(ert-deftest m5-execution/reasoning/raw-settings-do-not-require-a-declaration ()
  (let ((superchat-llm-reasoning-parameter-overrides nil))
    (dolist (pair '((reasoning_effort . "none")
                    ("reasoning_effort" . "none")
                    (:reasoning_effort . "none")))
      (let ((body (m5-reasoning--wire
                   (m5-reasoning--provider (list pair)) 'maximum)))
        (should (equal (alist-get 'reasoning_effort body) "none"))))))

(ert-deftest m5-execution/reasoning/explicit-template-thinking-is-preserved ()
  (let ((superchat-llm-reasoning-parameter-overrides nil))
    (let ((body (m5-reasoning--wire
                 (m5-reasoning--provider
                  '((chat_template_kwargs . ((enable_thinking . :false)))))
                 'maximum)))
      (should (eq :false
                  (alist-get 'enable_thinking
                             (alist-get 'chat_template_kwargs body))))
      (should-not (assq 'reasoning_effort body))
      (should-not (assq 'reasoning body)))))

(ert-deftest m5-execution/reasoning/unrelated-template-option-does-not-hide-gap ()
  (let ((superchat-llm-reasoning-parameter-overrides nil))
    (should-error
     (m5-reasoning--wire
      (m5-reasoning--provider
       '((chat_template_kwargs . ((date_string . "2026-09-17")))))
      'none)
     :type 'superchat-llm-reasoning-unsupported)))

(ert-deftest m5-execution/reasoning/unmapped-request-fails-with-actionable-message ()
  (let ((superchat-llm-reasoning-parameter-overrides nil))
    (let ((err (should-error
                (m5-reasoning--wire (m5-reasoning--provider) 'none)
                :type 'superchat-llm-reasoning-unsupported)))
      (should (string-match-p "qwen/qwen3.8-27b" (error-message-string err)))
      (should (string-match-p "server defaults" (error-message-string err)))
      (should (string-match-p
               "superchat-llm-reasoning-parameter-overrides"
               (error-message-string err))))))

(ert-deftest m5-execution/reasoning/declaration-is-endpoint-and-model-specific ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (dolist (provider
             (list
              (m5-reasoning--provider nil "http://localhost:8000/v1/")
              (m5-reasoning--provider nil nil "qwen/qwen3-8b")
              (m5-reasoning--provider nil "http://127.0.0.1:1234/v1/")))
      (should-error (m5-reasoning--wire provider 'none)
                    :type 'superchat-llm-reasoning-unsupported))))

(ert-deftest m5-execution/reasoning/missing-or-invalid-effort-is-not-downgraded ()
  (dolist (declaration
           (list
            (list (list (list 'llm-openai-compatible
                             m5-reasoning--url m5-reasoning--model)
                        'reasoning_effort '(none . "none")))
            (list (list (list 'llm-openai-compatible
                             m5-reasoning--url m5-reasoning--model)
                        'reasoning_effort '(maximum . nil)))
            (list (list (list 'llm-openai-compatible
                             m5-reasoning--url m5-reasoning--model)
                        'reasoning_effort '(maximum . "")))))
    (let ((superchat-llm-reasoning-parameter-overrides declaration))
      (should-error
       (m5-reasoning--wire (m5-reasoning--provider) 'maximum)
       :type 'superchat-llm-reasoning-unsupported))))

(ert-deftest m5-execution/reasoning/provider-and-other-request-stay-unchanged ()
  (let* ((superchat-llm-reasoning-parameter-overrides
          (m5-reasoning--declaration))
         (provider
          (m5-reasoning--provider '((seed . 17)
                                    (metadata . ((owner . "calibration"))))))
         (before (copy-tree provider t))
         (other (llm-make-chat-prompt "other" :reasoning 'maximum))
         (other-before (copy-tree other t))
         (body (m5-reasoning--wire provider 'none)))
    (should (equal (alist-get 'reasoning_effort body) "none"))
    (should (= (alist-get 'seed body) 17))
    (should (equal provider before))
    (should (equal other other-before))
    (should
     (equal "high" (alist-get 'reasoning_effort
                              (m5-reasoning--wire-prompt provider other))))
    (should (equal provider before))))

(ert-deftest m5-execution/reasoning/raw-provider-tree-stays-unchanged ()
  (let* ((superchat-llm-reasoning-parameter-overrides nil)
         (provider (m5-reasoning--provider
                    '((reasoning . ((effort . "high")))
                      (metadata . ((owner . "calibration"))))))
         (before (copy-tree provider t)))
    (should
     (equal "high"
            (alist-get 'effort
                       (alist-get 'reasoning
                                  (m5-reasoning--wire provider 'none)))))
    (should (equal before provider))))

(ert-deftest m5-execution/reasoning/repeated-request-does-not-duplicate-the-field ()
  (let* ((superchat-llm-reasoning-parameter-overrides
          (m5-reasoning--declaration))
         (provider (m5-reasoning--provider))
         (prompt (llm-make-chat-prompt "calibrate" :reasoning 'none)))
    (dotimes (_ 3)
      (should
       (equal "none" (alist-get
                       'reasoning_effort
                       (m5-reasoning--wire-prompt provider prompt)))))
    (should (= 1 (length (llm-chat-prompt-non-standard-params prompt))))))

(ert-deftest m5-execution/reasoning/native-openai-responses-remains-native ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (dolist (case '((none . "none") (light . "low")
                    (medium . "medium") (maximum . "xhigh")))
      (let* ((provider (make-llm-openai :key "calibration" :chat-model "gpt-5.2"))
             (body (m5-reasoning--wire provider (car case))))
        (should (equal (alist-get 'effort (alist-get 'reasoning body)) (cdr case)))
        (should-not (assq 'reasoning_effort body))
        (should (assq 'input body))))))

(ert-deftest m5-execution/reasoning/native-ollama-thinking-remains-native ()
  (let ((superchat-llm-reasoning-parameter-overrides
         (m5-reasoning--declaration)))
    (dolist (effort '(none light medium maximum))
      (let ((body (m5-reasoning--wire
                   (make-llm-ollama :chat-model "qwen3.6") effort)))
        (should (eq (alist-get 'think body)
                    (if (eq effort 'none) :false t)))
        (should-not (assq 'reasoning_effort body))
        (should-not (assq 'reasoning body))))))

(ert-deftest m5-execution/reasoning/specialized-compatible-subtypes-are-untouched ()
  (let* ((superchat-llm-reasoning-parameter-overrides nil)
         (provider (make-llm-openrouter :key "calibration"
                                       :chat-model "qwen/qwen3-8b"))
         (prompt (llm-make-chat-prompt "calibrate" :reasoning 'none))
         (before (copy-tree prompt t)))
    (should (eq prompt (superchat--llm-prepare-reasoning provider prompt)))
    (should (equal prompt before))))


;; These two cases enter the actual product adapter.  The fake transport
;; constructs the provider body; neither test calls the reasoning helper.
(defvar superchat--pause-requested nil)
(defvar superchat--pause-run-id nil)
(defvar superchat--pending-send-request nil)

(defmacro m5-reasoning--with-dispatch (tools &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let* ((requests nil) (answer nil) (owner nil)
            (superchat-llm-backend (m5-reasoning--provider))
            (provider-before (copy-tree superchat-llm-backend t))
            (superchat-llm-reasoning-parameter-overrides
             (m5-reasoning--declaration))
            (superchat-llm-reasoning 'none)
            (superchat-llm-streaming t)
            (superchat-show-ttft nil)
            (superchat-show-ttft-breakdown nil)
            (superchat-response-timeout nil)
            (superchat-response-timeout-hard-limit 30)
            (superchat-llm-max-tool-rounds 3)
            (superchat--llm-tools-prepared-p t)
            (superchat--llm-prepared-tools ,tools)
            (superchat--llm-evidence-context nil)
            (superchat--llm-request-context nil)
            (superchat--llm-runtime-observer nil)
            (superchat--llm-run-active-p (lambda () t))
            (superchat--llm-transport-timer-owner-function
             (lambda (value) (setq owner value) "m5-reasoning-dispatch"))
            (superchat--pause-requested nil)
            (superchat--pause-run-id nil)
            (superchat--pending-send-request nil)
            (superchat-llm--distribution-adapters nil))
       (cl-letf
           (((symbol-function 'superchat--llm-context-capability)
             (lambda (&rest _) nil))
            ((symbol-function 'superchat--llm-estimate-context-tokens)
             (lambda (&rest _) 1))
            ((symbol-function 'superchat--llm-record-request-evidence)
             (lambda (&rest _) nil))
            ((symbol-function 'llm-chat-streaming)
             (lambda (provider prompt _partial response error &optional multi)
               (setq requests
                     (append
                      requests
                      (list
                       (list :provider provider :prompt prompt
                             :response response :error error :multi multi
                             :body
                             (json-parse-string
                              (llm-provider-utils-json-serialize
                               (llm-provider-chat-request provider prompt t))
                              :object-type 'alist :array-type 'list
                              :null-object nil :false-object :false)))))
               nil)))
         (unwind-protect
             (progn
               (superchat--llm-generate-answer
                "Calibrate the actual request dispatch."
                (lambda (value) (when (stringp value) (setq answer value)))
                nil nil nil 'none)
               ,@body
               (should (equal superchat-llm-backend provider-before)))
           (when owner
             (superchat-llm--transport-timer-owner-cancel owner)))))))

(defun m5-reasoning--deliver-dispatch (request result)
  "Deliver RESULT through llm.el for the captured REQUEST."
  (llm-provider-utils-process-result
   (plist-get request :provider) (plist-get request :prompt) result
   (plist-get request :multi) (plist-get request :response)
   (plist-get request :error)))

(ert-deftest m5-execution/reasoning/product-initial-dispatch-serializes-declaration ()
  (m5-reasoning--with-dispatch nil
    (should (= (length requests) 1))
    (let ((wire (plist-get (car requests) :body)))
      (should (equal (alist-get 'reasoning_effort wire) "none"))
      (should-not (assq 'reasoning wire)))
    (m5-reasoning--deliver-dispatch (car requests) '(:text "initial done"))
    (should (equal answer "initial done"))))

(ert-deftest m5-execution/reasoning/product-tool-followup-retains-request-setting ()
  (let ((executions 0))
    (m5-reasoning--with-dispatch
        (list
         (make-llm-tool
          :name "m5_reasoning_probe"
          :description "Return calibration text without changing editor state."
          :args nil
          :function (lambda () (cl-incf executions) "probe-result")))
      (should (= (length requests) 1))
      (m5-reasoning--deliver-dispatch
       (car requests)
       (list
        :tool-uses
        (list (make-llm-provider-utils-tool-use
               :id "m5-reasoning-call" :name "m5_reasoning_probe" :args nil))))
      (should (= executions 1))
      (should (= (length requests) 2))
      (dolist (request requests)
        (should (equal "none"
                       (alist-get 'reasoning_effort
                                  (plist-get request :body))))
        (should-not (assq 'reasoning (plist-get request :body))))
      (let ((messages (alist-get 'messages
                                 (plist-get (cadr requests) :body))))
        (should
         (cl-some (lambda (message)
                    (and (equal (alist-get 'role message) "tool")
                         (equal (alist-get 'tool_call_id message) "m5-reasoning-call")
                         (string-match-p "probe-result"
                                         (alist-get 'content message))))
                  messages)))
      (m5-reasoning--deliver-dispatch (cadr requests) '(:text "followup done"))
      (should (equal answer "followup done")))))


(ert-deftest m5-execution/reasoning/hash-table-template-setting-is-preserved ()
  (let* ((superchat-llm-reasoning-parameter-overrides nil)
         ;; json-serialize requires string keys in hash-table objects.
         (settings (make-hash-table :test #'equal))
         (_ (progn (puthash "enable_thinking" :false settings)
                   (puthash "date_string" "2026-09-17" settings)))
         (provider (m5-reasoning--provider
                    (list (cons 'chat_template_kwargs settings))))
         (body (m5-reasoning--wire provider 'maximum)))
    (should (eq :false
                (alist-get 'enable_thinking
                           (alist-get 'chat_template_kwargs body))))
    (should-not (assq 'reasoning_effort body))
    (should-not (assq 'reasoning body))
    (should (= (hash-table-count settings) 2))
    (should (eq (gethash "enable_thinking" settings) :false))
    (should (equal (gethash "date_string" settings) "2026-09-17"))))

(ert-deftest m5-execution/reasoning/explicit-preset-error-explains-inherit ()
  (let* ((superchat-llm-reasoning nil)
         (superchat-llm-reasoning-parameter-overrides nil)
         (preset (superchat-preset--create :reasoning 'none))
         (prompt (superchat--build-llm-prompt "calibrate" nil nil preset))
         (err (should-error
               (m5-reasoning--wire-prompt (m5-reasoning--provider) prompt)
               :type 'superchat-llm-reasoning-unsupported)))
    ;; Setting the global value to nil alone cannot override an explicit
    ;; Preset.  The error must include the missing half of that instruction.
    (should (string-match-p "Preset reasoning inherit"
                            (error-message-string err)))
    (should (string-match-p "superchat-llm-reasoning nil"
                            (error-message-string err)))))

(ert-deftest m5-execution/reasoning/product-unmapped-effort-never-enters-provider ()
  ;; Keep the real generate-answer path and its error delivery; only remove
  ;; the fixture's endpoint declaration.  Without the production integration
  ;; this request would enter the fake provider and fail the zero-call check.
  (cl-letf (((symbol-function 'm5-reasoning--declaration)
             (lambda (&optional _maximum) nil)))
    (m5-reasoning--with-dispatch nil
      (should-not requests)
      (should (stringp answer))
      (should (string-match-p "no declared mapping" answer))
      (should (string-match-p "qwen/qwen3.8-27b" answer))
      (should (string-match-p "Preset reasoning inherit" answer))
      (should (string-match-p "superchat-llm-reasoning nil" answer)))))

;;;; Tool budget finalization through Runtime and Tape

(defvar m5b-results nil)
(defvar m5b-response-callbacks nil)
(defvar m5b-after-terminal nil)
(defvar m5b-view-owners nil)

(defmacro m5b-with-fixture (script &rest body)
  "Run BODY using SCRIPT and isolated product state."
  (declare (indent 1) (debug t))
  `(progn
     (when (getenv "SMOKE_REAL")
       (error "Tool-budget tests require a scripted provider; unset SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-max-tool-rounds 1)
           (superchat-llm-round-limit-action 'ask)
           (superchat-llm-reasoning nil)
           (m5b-results nil)
           (m5b-response-callbacks nil)
           (m5b-after-terminal nil)
           (m5b-view-owners nil))
       (smoke--with-fixture ,script
         (let ((network (symbol-function 'smoke--fake-network)))
           (cl-letf (((symbol-function 'smoke--fake-network)
                      (lambda (&rest arguments)
                        (push (nth 4 arguments) m5b-response-callbacks)
                        (apply network arguments))))
             ,@body))))))

(defun m5b-read-step ()
  "One actual read_buffer invocation, not a fabricated tool result."
  '(:tool-uses (("read_buffer" (buffer . "*scratch*")))))

(defun m5b--submit (view &optional text)
  "Submit through the real driver and capture its exact Sink terminal payload.
Also forward to an ordinary View Sink so rendering is exercised."
  (let* ((session (buffer-local-value 'superchat--session-id view))
         (owner (format "m5b-%08x" (random #x100000000)))
         (view-sink (superchat-render-view-sink view session owner))
         (sink
          (superchat-sink-create
           :stream (lambda (run payload)
                     (superchat-sink-emit view-sink 'stream run payload))
           :status (lambda (run payload)
                     (superchat-sink-emit view-sink 'status run payload))
           :final (lambda (run payload)
                    (push (list :run-id (plist-get run :run-id)
                                :slot 'final :payload (copy-tree payload))
                          m5b-results)
                    (when m5b-after-terminal
                      (funcall m5b-after-terminal run payload))
                    (superchat-sink-emit view-sink 'final run payload))
           :error (lambda (run payload)
                    (push (list :run-id (plist-get run :run-id)
                                :slot 'error :payload (copy-tree payload))
                          m5b-results)
                    (when m5b-after-terminal
                      (funcall m5b-after-terminal run payload))
                    (superchat-sink-emit view-sink 'error run payload)))))
    (let* ((admission
            (superchat-service-call
             nil 'driver 'submit
             (list :surface 'external :session-id session
                   :content (or text "Read scratch and report only verified results.")
                   :tools '("read_buffer") :attendance 'attended
                   :context (list :host-buffer view)
                   :runtime-extras '(:record-conversation nil)
                   :sink sink)))
           (run-id (plist-get admission :run-id)))
      (should (stringp run-id))
      (push (cons run-id owner) m5b-view-owners)
      run-id)))

(defun m5b--settle (run-id)
  "Wait for this exact Run and its Turn seal."
  (should
   (smoke--wait
    (lambda ()
      (let* ((status (superchat-runtime-status run-id))
             (session (plist-get status :session-id)))
        (and (superchat-runtime--terminal-p (plist-get status :status))
             (cl-some (lambda (event)
                        (and (equal run-id (plist-get event :run-id))
                             (equal "turn.completed" (plist-get event :type))))
                      (superchat-tape-replay-events session)))))
    30))
  run-id)

(defun m5b--events (run-id &optional type)
  "Return exact RUN-ID events, optionally filtered by TYPE."
  (let ((session (plist-get (superchat-runtime-status run-id) :session-id)))
    (cl-remove-if-not
     (lambda (event)
       (and (equal run-id (plist-get event :run-id))
            (or (null type) (equal type (plist-get event :type)))))
     (superchat-tape-replay-events session))))

(defun m5b--terminal-events (run-id)
  (cl-remove-if-not
   (lambda (event)
     (member (plist-get event :type)
             '("run.completed" "run.failed" "run.cancelled")))
   (m5b--events run-id)))

(defun m5b--result (run-id)
  (let ((results
         (cl-remove-if-not
          (lambda (result) (equal run-id (plist-get result :run-id)))
          m5b-results)))
    (should (= 1 (length results)))
    (car results)))

(defun m5b--budget (run-id)
  (plist-get (plist-get (m5b--result run-id) :payload) :tool-budget))

(defun m5b--assert-budget (budget rounds limit action)
  "Assert the small public schema without relying on plist key ordering."
  (should (listp budget))
  (should (= rounds (plist-get budget :rounds)))
  (should (= limit (plist-get budget :limit)))
  (should (equal (symbol-name action)
                 (format "%s" (plist-get budget :action)))))

(defun m5b--assert-recorded-budget (run-id rounds limit action)
  "Check agreement between public status, exact Sink and durable event payloads."
  (m5b--assert-budget (m5b--budget run-id) rounds limit action)
  (m5b--assert-budget
   (plist-get (superchat-runtime-status run-id) :tool-budget)
   rounds limit action)
  (let ((responses (m5b--events run-id "response.completed"))
        (terminals (m5b--terminal-events run-id)))
    (should (= 1 (length responses)))
    (should (= 1 (length terminals)))
    (dolist (event (append responses terminals))
      (m5b--assert-budget (plist-get (plist-get event :payload) :tool-budget)
                         rounds limit action))))

(defun m5b--tape-answer (run-id)
  "Read the persisted response summary by identity.
Full model text lives on the Runtime Turn and exact Sink; Tape retains its
size/hash plus this bounded summary. Short-answer tests can compare it directly."
  (let* ((event (car (m5b--events run-id "response.completed")))
         (session (plist-get (superchat-runtime-status run-id) :session-id))
         (row (cl-find (plist-get event :storage-id)
                       (superchat-db-tape-replay session)
                       :key #'car :test #'equal)))
    (should row)
    (nth 3 row)))

(defun m5b--requests ()
  "Return captured provider requests in send order."
  (reverse smoke--requests))

(defun m5b--assert-final-request (request)
  "Check the actual tool-free request and its truthful boundary instruction."
  (let ((text (plist-get request :prompt-text))
        (case-fold-search t))
    (should-not (plist-get request :tools))
    (should (string-match-p "budget\\|round limit" text))
    (should (string-match-p "unverified\\|not verified" text))
    (should (string-match-p "unfinished\\|not completed\\|remaining\\|incomplete" text))
    (should-not
     (string-match-p "The requested tool work is complete\\." text))))

(defun m5b--view-text (view)
  (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max))))

(defun m5b--assert-notice (text)
  (let ((case-fold-search t))
    (should (string-match-p "tool budget\\|tool round limit" text))
    (should (string-match-p "unfinished\\|unverified" text))))

(ert-deftest m5-execution/budget/batch-ask-finalizes-once-with-truthful-prompt ()
  (m5b-with-fixture (list (m5b-read-step) '(:text "Read confirmed; no edit attempted."))
    (let ((noninteractive t))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Batch mode must not ask"))))
        (let* ((view (smoke--open-chat))
               (run (m5b--settle (m5b--submit view))))
          (should (= 2 (length smoke--requests)))
          (should (= 1 (length (m5b--events run "tool.completed"))))
          (m5b--assert-final-request (car (last (m5b--requests))))
          (m5b--assert-recorded-budget run 1 1 'finalize)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (should (eq 'final (plist-get (m5b--result run) :slot)))
          (should (equal "Read confirmed; no edit attempted."
                         (plist-get (plist-get (m5b--result run) :payload) :text)))
          (m5b--assert-notice (m5b--view-text view)))))))

(ert-deftest m5-execution/budget/finalize-does-not-ask ()
  (m5b-with-fixture (list (m5b-read-step) '(:text "Summary only."))
    (let ((superchat-llm-round-limit-action 'finalize))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Explicit finalize must not ask"))))
        (let* ((view (smoke--open-chat))
               (run (m5b--settle (m5b--submit view))))
          (should (= 2 (length smoke--requests)))
          (m5b--assert-recorded-budget run 1 1 'finalize))))))

(ert-deftest m5-execution/budget/interactive-continue-grants-twelve-more-rounds ()
  (m5b-with-fixture
      (append (cl-loop repeat 24 collect (m5b-read-step))
              '((:text "After two allowed budgets.")))
    (let ((superchat-llm-max-tool-rounds 12)
          (decisions '(t nil))
          (questions nil))
      ;; Build the View in the actual batch environment, then simulate only
      ;; the interactive decision boundary while the async loop is driven.
      (let ((view (smoke--open-chat))
            (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (question)
                     (push question questions)
                     (pop decisions))))
          (let ((run (m5b--settle (m5b--submit view))))
            (should (= 2 (length questions)))
            (should (= 25 (length smoke--requests)))
            (should (= 24 (length (m5b--events run "tool.completed"))))
            (dolist (request (butlast (m5b--requests)))
              (should (equal '("read_buffer") (plist-get request :tools))))
            (m5b--assert-final-request (car (last (m5b--requests))))
            (m5b--assert-recorded-budget run 24 24 'finalize)
            (should (equal "After two allowed budgets." (m5b--tape-answer run)))))))))

(ert-deftest m5-execution/budget/ordinary-completion-has-no-budget-annotation ()
  (m5b-with-fixture '((:text "A plain answer."))
    (let* ((view (smoke--open-chat))
           (run (m5b--settle (m5b--submit view))))
      (should-not (m5b--budget run))
      (should-not (plist-get (superchat-runtime-status run) :tool-budget))
      (dolist (event (append (m5b--events run "response.completed")
                            (m5b--terminal-events run)))
        (should-not (plist-get (plist-get event :payload) :tool-budget)))
      (let ((case-fold-search t))
        (should-not (string-match-p "tool budget\\|tool round limit"
                                   (m5b--view-text view)))))))

(ert-deftest m5-execution/budget/stop-keeps-failed-and-sends-no-final-request ()
  (m5b-with-fixture (list (m5b-read-step) '(:text "Must never be requested."))
    (let ((superchat-llm-round-limit-action 'stop))
      (let* ((view (smoke--open-chat))
             (run (m5b--settle (m5b--submit view))))
        (should (= 1 (length smoke--requests)))
        (should (= 1 (length (m5b--events run "tool.completed"))))
        (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
        (should (eq 'error (plist-get (m5b--result run) :slot)))
        (m5b--assert-recorded-budget run 1 1 'stop)
        (should (string-match-p "Stopped after 1 tool rounds"
                                (m5b--tape-answer run)))))))

(ert-deftest m5-execution/budget/long-tool-shaped-prose-stays-exact-and-inert ()
  (let ((answer (concat "MODEL_BODY_START\n" (make-string 800 ?x)
                        "\n<tool_call>{\"name\":\"eval_elisp\","
                        "\"arguments\":{\"code\":\"(setq m5b-should-not-run t)\"}}"
                        "</tool_call>\nMODEL_BODY_END")))
    (m5b-with-fixture (list (m5b-read-step) (list :text answer))
      (let* ((view (smoke--open-chat))
             (run (m5b--settle (m5b--submit view))))
        (should (= 2 (length smoke--requests)))
        (should (= 1 (length (m5b--events run "tool.completed"))))
        (should (= 1 (length (m5b--events run "tool.requested"))))
        (should (equal answer (plist-get (plist-get (m5b--result run) :payload) :text)))
        (should (equal answer
                       (superchat-turn-llm-result
                        (plist-get (superchat-runtime--entry run) :turn))))
        (let* ((event (car (m5b--events run "response.completed")))
               (payload (plist-get event :payload))
               (bytes (encode-coding-string answer 'utf-8)))
          (should (= (string-bytes bytes) (plist-get payload :content-size)))
          (should (equal (concat "sha256:" (secure-hash 'sha256 bytes))
                         (plist-get payload :content-hash))))
        (should-not (bound-and-true-p m5b-should-not-run))
        (should (string-match-p "MODEL_BODY_END" (m5b--view-text view)))
        (m5b--assert-notice (m5b--view-text view))))))

(ert-deftest m5-execution/budget/structured-call-after-finalization-cannot-execute ()
  (m5b-with-fixture (list (m5b-read-step) (m5b-read-step)
                         '(:text "Must not retry invalid final tools."))
    (let* ((view (smoke--open-chat))
           (run (m5b--settle (m5b--submit view))))
      (should (= 2 (length smoke--requests)))
      (should-not (plist-get (car smoke--requests) :tools))
      (should (= 1 (length (m5b--events run "tool.completed"))))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (m5b--assert-recorded-budget run 1 1 'finalize))))

(ert-deftest m5-execution/budget/provider-failure-retains-the-exhausted-budget ()
  (m5b-with-fixture
      (list (m5b-read-step) '(:error (error . "m5b provider failure")))
    (let* ((view (smoke--open-chat))
           (run (m5b--settle (m5b--submit view))))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (should (eq 'error (plist-get (m5b--result run) :slot)))
      (should (string-match-p "m5b provider failure" (m5b--tape-answer run)))
      (m5b--assert-recorded-budget run 1 1 'finalize)
      (m5b--assert-notice (m5b--view-text view)))))

(ert-deftest m5-execution/budget/empty-final-response-preserves-current-outcome ()
  (m5b-with-fixture (list (m5b-read-step) '(:text ""))
    (let* ((view (smoke--open-chat))
           (run (m5b--settle (m5b--submit view))))
      ;; This change must not silently invent a new completed/failed policy.
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (equal "[Empty response from llm]" (m5b--tape-answer run)))
      (m5b--assert-recorded-budget run 1 1 'finalize)
      (m5b--assert-notice (m5b--view-text view)))))

(ert-deftest m5-execution/budget/reentrant-stop-and-late-callback-cannot-redeliver ()
  (m5b-with-fixture (list (m5b-read-step) '(:text "One real ending."))
    (let ((m5b-after-terminal
           (lambda (run _payload)
             (superchat-runtime-cancel (plist-get run :run-id)
                                       "[m5b reentrant stop]"))))
      (let* ((view (smoke--open-chat))
             (run (m5b--settle (m5b--submit view))))
        (funcall (car m5b-response-callbacks) '(:text "Late forged ending."))
        (should (= 2 (length smoke--requests)))
        (should (= 1 (length (m5b--terminal-events run))))
        (should (= 1 (length (m5b--events run "response.completed"))))
        (should (equal "One real ending." (m5b--tape-answer run)))
        (m5b--assert-recorded-budget run 1 1 'finalize)))))

(ert-deftest m5-execution/budget/reopen-renders-budget-without-changing-history-text ()
  (m5b-with-fixture (list (m5b-read-step) '(:text "Reopen answer remains exact."))
    (let* ((view (smoke--open-chat))
           (run (m5b--settle (m5b--submit view)))
           (session (plist-get (superchat-runtime-status run) :session-id))
           (rows (superchat-db-tape-replay session)))
      (with-temp-buffer
        (org-mode)
        (let ((superchat--conversation-history nil))
          (superchat--render-session-rows (current-buffer) rows)
          (should (string-match-p "Reopen answer remains exact." (buffer-string)))
          (m5b--assert-notice (buffer-string))))
      (should (equal "Reopen answer remains exact." (m5b--tape-answer run))))))

(ert-deftest m5-execution/budget/context-recovery-keeps-final-request-boundary ()
  "A real native recovery/retry must retain the exhausted-budget boundary.
Supply a minimal known context prefix at the adapter boundary: this tests the
model-loop rebuild, not external-surface history collection."
  (m5b-with-fixture
      (list (m5b-read-step)
            '(:error (llm-context-length-exceeded . "context length exceeded"))
            '(:text "Recovered summary, still without tools."))
    (let ((superchat-handoff-auto-enabled nil)
          (generate (symbol-function 'superchat--llm-generate-answer))
          (recoveries 0)
          undo)
      (unwind-protect
          (let ((view (smoke--open-chat)))
            (setq undo
                  (superchat-on
                   nil 'context-budget-exceeded 'm5b-scripted-context-recovery
                   (lambda (details)
                     (cl-incf recoveries)
                     (funcall (plist-get details :resume-function)
                              "M5B_RECOVERED_CONTEXT")
                     t)))
            (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest arguments)
                         (let ((superchat--llm-context-envelope
                                (plist-put
                                 (copy-tree superchat--llm-context-envelope)
                                 :prompt "M5B_INITIAL_CONTEXT")))
                           (apply generate
                                  (cons (concat "M5B_INITIAL_CONTEXT\\n"
                                                (car arguments))
                                        (cdr arguments)))))))
              (let ((run (m5b--settle (m5b--submit view))))
                (should (= 1 recoveries))
                (should (= 3 (length smoke--requests)))
                (dolist (request (last (m5b--requests) 2))
                  (m5b--assert-final-request request))
                (should (string-match-p "M5B_RECOVERED_CONTEXT"
                                        (plist-get (car smoke--requests) :prompt-text)))
                (should (= 1 (length (m5b--events run "tool.completed"))))
                (m5b--assert-recorded-budget run 1 1 'finalize)
                (should (equal "Recovered summary, still without tools."
                               (m5b--tape-answer run))))))
        (when undo (funcall undo))))))

(ert-deftest m5-execution/budget/cancel-after-budget-retains-the-observed-boundary ()
  (m5b-with-fixture (list (m5b-read-step) '(:hang t))
    (let* ((view (smoke--open-chat))
           (run (m5b--submit view)))
      (should (smoke--wait (lambda () (= 2 (length smoke--requests))) 3))
      (m5b--assert-budget
       (plist-get (superchat-runtime-status run) :tool-budget) 1 1 'finalize)
      (superchat-runtime-cancel run "[m5b explicit cancellation]")
      (m5b--settle run)
      (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
      (m5b--assert-budget (m5b--budget run) 1 1 'finalize)
      (should-not (m5b--events run "response.completed"))
      (let ((terminals (m5b--terminal-events run)))
        (should (= 1 (length terminals)))
        (m5b--assert-budget
         (plist-get (plist-get (car terminals) :payload) :tool-budget)
         1 1 'finalize))
      (m5b--assert-notice (m5b--view-text view)))))

(provide 'superchat-m5-execution-regression)
;;; m5-execution-regression.el ends here
