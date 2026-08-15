;;; test-workflow.el — Workflow parser, substitution, and async engine tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-workflow.el:
;;   - Parsing (steps, per-line annotations, variable substitution)
;;   - File loading (.workflow discovery)
;;   - Async execution chain (mock Runtime, variable flow, error handling)

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-workflow)
(require 'superchat-skills)

;; ── Mock externals (no superchat.el loaded) ──
(defvar superchat-buffer-name "*superchat-test*")
(defvar superchat-lang "English")
(unless (fboundp 'superchat--record-message)
  (defun superchat--record-message (_role _content &optional _tape-id) nil))
(unless (fboundp 'superchat--insert-prompt)
  (defun superchat--insert-prompt () nil))

;; ═══════════════════════════════════════════════════════
;; Part A — File loading
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-directory-defcustom ()
  "superchat-workflow-directory is a string ending in workflow/."
  (should (stringp superchat-workflow-directory))
  (should (string-suffix-p "workflow/" superchat-workflow-directory)))

(ert-deftest workflow--exists-p-returns-nil-for-unknown ()
  "exists-p returns nil for a name that certainly does not exist."
  (should-not (superchat-workflow--exists-p "__nonexistent_workflow_test__")))

(ert-deftest workflow--file-path-ends-with-.workflow ()
  "file-path appends .workflow to the name."
  (let ((path (superchat-workflow--file-path "test")))
    (should (string-suffix-p "test.workflow" path))))

(ert-deftest workflow--load-returns-nil-for-missing ()
  "load returns nil when the file does not exist."
  (should-not (superchat-workflow--load "__nonexistent_workflow_test__")))

(ert-deftest workflow--load-reads-content ()
  "load returns file content when the file exists."
  (let* ((tmp-dir (make-temp-file "wf-load" t))
         (orig-val superchat-workflow-directory))
    (unwind-protect
        (progn
          (setq superchat-workflow-directory tmp-dir)
          (with-temp-file (expand-file-name "testload.workflow" tmp-dir)
            (insert "step1\nstep2\n"))
          (let ((body (superchat-workflow--load "testload")))
            (should body)
            (should (string-match-p "step1" body))
            (should (string-match-p "step2" body))))
      (setq superchat-workflow-directory orig-val)
      (ignore-errors (delete-directory tmp-dir t)))))

;; ═══════════════════════════════════════════════════════
;; Part B — Parsing
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-parse-skips-blank-and-comments ()
  "Parse returns only executable steps, skipping blanks and comments."
  (let ((body "/web-search \"news\"\n\n# this is a comment\n\n/summarize\n"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal 2 (length steps)))
      (should (equal "/web-search \"news\"" (car steps)))
      (should (equal "/summarize" (cadr steps))))))

(ert-deftest workflow-parse-preserves-step-order ()
  "Steps preserve their original order."
  (let ((body "step1\nstep2\nstep3"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal '("step1" "step2" "step3") steps)))))

(ert-deftest workflow--parse-line-extracts-model ()
  "parse-line extracts @model-name."
  (let ((result (superchat-workflow--parse-line "@gpt-4o-mini hello world")))
    (should (equal "gpt-4o-mini" (plist-get result :model)))
    (should (string-match-p "hello world" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-extracts-command ()
  "parse-line extracts /command args."
  (let ((result (superchat-workflow--parse-line "/web-search emacs workflow")))
    (should (equal "web-search emacs workflow" (plist-get result :command)))
    ;; When prompt is empty and command is set, prompt defaults to command
    (should (equal "web-search emacs workflow" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-extracts-contexts ()
  "parse-line extracts #file references."
  (let ((result (superchat-workflow--parse-line "check the code #src/main.el")))
    (should (equal '("src/main.el") (plist-get result :contexts)))
    (should (string-match-p "check the code" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-plain-prompt ()
  "parse-line returns prompt only when no annotations present."
  (let ((result (superchat-workflow--parse-line "just a plain prompt")))
    (should-not (plist-get result :model))
    (should-not (plist-get result :command))
    (should-not (plist-get result :contexts))
    (should (equal "just a plain prompt" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-empty-after-annotations ()
  "parse-line returns empty prompt when all content is annotations."
  (let ((result (superchat-workflow--parse-line "@gpt-4o /summarize")))
    (should (equal "gpt-4o" (plist-get result :model)))
    (should (equal "summarize" (plist-get result :command)))
    (should (equal "summarize" (plist-get result :prompt)))))

;; ═══════════════════════════════════════════════════════
;; Part B — Variable substitution
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-substitute-expands-input-lang-date ()
  "Substitute replaces $input, $lang, $date."
  (let ((result (superchat-workflow--substitute-vars
                 "Hello $input, lang=$lang, date=$date"
                 '(:input "world" :lang "French" :date "2026-01-01")
                 nil)))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "lang=French" result))
    (should (string-match-p "date=2026-01-01" result))))

;; Need a separate test for leaving unknown vars alone (not tangled with :input)
(ert-deftest workflow-substitute-leaves-unknown-vars-alone ()
  "Unknown $var references are left unchanged."
  (let ((result (superchat-workflow--substitute-vars
                 "Hello $input, $unknown"
                 '(:input "world" :lang "en" :date "2026-01-01")
                 nil)))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "\\$unknown" result))))

(ert-deftest workflow-substitute-expands-result ()
  "Substitute replaces $result with the most recent step output."
  (let ((result (superchat-workflow--substitute-vars
                 "Previous: $result. More: $result."
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("output of step 1" "output of step 2"))))
    (should (string-match-p "output of step 2" result))
    (should-not (string-match-p "\\$result" result))))

(ert-deftest workflow-substitute-expands-stepN ()
  "Substitute replaces $step1, $step2 with respective outputs."
  (let ((result (superchat-workflow--substitute-vars
                 "Step1: $step1 | Step2: $step2"
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("first" "second"))))
    (should (string-match-p "Step1: first" result))
    (should (string-match-p "Step2: second" result))
    (should-not (string-match-p "\\$step1" result))
    (should-not (string-match-p "\\$step2" result))))

(ert-deftest workflow-substitute-empty-result-when-no-steps ()
  "Substitute replaces $result with empty string when there are no previous steps."
  (let ((result (superchat-workflow--substitute-vars
                 "Summary: $result"
                 '(:input "" :lang "en" :date "2026-01-01")
                 nil)))
    (should (string= "Summary: " result))))

(ert-deftest workflow-substitute-backward-compat-alias ()
  "superchat-workflow-substitute is an alias — calling it works the same way."
  (should (fboundp 'superchat-workflow-substitute))
  ;; Verify it produces the same output as the canonical function
  (let ((canonical (superchat-workflow--substitute-vars
                    "Hello $input"
                    '(:input "world" :lang "en" :date "2026-01-01")
                    nil))
        (aliased (superchat-workflow-substitute
                  "Hello $input"
                  '(:input "world" :lang "en" :date "2026-01-01")
                  nil)))
    (should (string= canonical aliased))))

;; ═══════════════════════════════════════════════════════
;; Part C — Async execution chain (mock Runtime)
;; ═══════════════════════════════════════════════════════

(defvar workflow-test--mock-calls nil
  "List of prompts sent to the mock Runtime during a test run.")
(defvar workflow-test--mock-requests nil
  "List of requests sent to the mock Runtime during a test run.")
(defvar workflow-test--mock-answers nil
  "List of answers the mock Runtime returns, consumed in order.")
(defvar workflow-test--mock-statuses nil
  "List of statuses the mock Runtime returns, consumed in order.")
(defvar workflow-test--mock-step-count 0
  "Number of times the mock Runtime was called.")
(defvar workflow-test--mock-resume-action 'advance
  "Resume action returned while the mock Runtime delivers a result.")

(defun workflow-test--mock-runtime-submit (request)
  "Mock `superchat-runtime-submit'.
Pushes REQUEST onto `workflow-test--mock-requests' and delivers the
next configured answer/status synchronously."
  (push request workflow-test--mock-requests)
  (push (plist-get request :prompt) workflow-test--mock-calls)
  (cl-incf workflow-test--mock-step-count)
  (let ((answer (or (pop workflow-test--mock-answers)
                    "[mock: no answer configured]"))
        (status (or (pop workflow-test--mock-statuses) 'completed)))
    (cl-letf (((symbol-function
                'superchat-workflow--resume-decision-for-step)
               (lambda (&rest _args)
                 (list :action workflow-test--mock-resume-action))))
      (funcall (plist-get request :delivery-function)
               answer status workflow-test--mock-step-count)))
  (list :run-id (format "run-%d" workflow-test--mock-step-count)
        :status 'accepted))

(ert-deftest workflow/resume-decision-maps-closure-verdicts ()
  "Workflow maps Closure evidence to advance, wait, review, or stop."
  (let (well-formed terminal)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (&rest _args) '(:trace)))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     well-formed
                   terminal))))
      (dolist
          (case
           '(((:verdict valid)
              (:verdict proved :judgment
                        (:terminal-kind completed))
              advance)
             ((:verdict valid) (:verdict open) wait)
             ((:verdict valid) (:verdict unknown) manual-review)
             ((:verdict valid)
              (:verdict proved :judgment (:terminal-kind failed))
              stop)
             ((:verdict invalid) nil stop)
             ((:verdict unknown) nil manual-review)))
        (setq well-formed (nth 0 case)
              terminal (nth 1 case))
        (should
         (eq (nth 2 case)
             (plist-get
              (superchat-workflow--resume-decision
               "session-1" "run-1")
              :action)))))))

(ert-deftest workflow/instance-projection-selects-latest-step-run ()
  "A Workflow Instance view derives step, Run, judgment, and decision."
  (let ((trace
         '((:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-1"
            :run-id "run-old"
            :turn-id "turn-old")
           (:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-2"
            :run-id "run-2"
            :turn-id "turn-2")
           (:type "run.started"
            :correlation-id "other-workflow"
            :actor-id "workflow-step-1"
            :run-id "run-other")
           (:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-1"
            :run-id "run-new"
            :turn-id "turn-new")))
        judged-runs)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (_session-id) trace))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     '(:verdict valid)
                   (push (plist-get query :run-id) judged-runs)
                   '(:verdict proved
                     :judgment (:terminal-kind completed))))))
      (let* ((view
              (superchat-workflow-instance-view
               "session-1" "workflow-1"))
             (steps (plist-get view :steps)))
        (should (equal '(1 2) (mapcar (lambda (step)
                                       (plist-get step :step-id))
                                     steps)))
        (should (equal '("run-new" "run-2")
                       (mapcar (lambda (step) (plist-get step :run-id))
                               steps)))
        (should (equal '(advance advance)
                       (mapcar (lambda (step)
                                 (plist-get step :resume-decision))
                               steps)))
        (should (cl-every
                 (lambda (step)
                   (eq 'proved
                       (plist-get (plist-get step :closure-judgment)
                                  :verdict)))
                 steps))
        (should (equal '("run-2" "run-new") judged-runs))))))

(ert-deftest workflow/open-resume-gate-does-not-advance ()
  "A completed callback cannot advance while Closure still reports open."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("step1 answer" "step2 answer"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             (workflow-test--mock-resume-action 'wait)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--fail-step)
              (lambda (&rest _args) nil)))
    (superchat-workflow--run-step
     0 (mapcar #'superchat-workflow--parse-line '("step1" "step2"))
     '(:input "" :lang "en" :date "2026-01-01") nil "workflow-test")
    (should (= 1 workflow-test--mock-step-count))))

(ert-deftest workflow/research-resumes-from-instance-projection ()
  "The sample Research Workflow resumes from persisted Closure proofs."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-workflow-resume-" t))
         (superchat-data-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled t)
         (superchat-buffer-name " *workflow-durable-resume*")
         (workflow-file
          (expand-file-name
           "examples/research.workflow"
           (file-name-directory (locate-library "superchat-workflow"))))
         (calls 0)
         prompts
         finalized)
    (unwind-protect
        (progn
          (superchat-db-close)
          (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _args)
                       (cl-incf calls)
                       (push prompt prompts)
                       (funcall superchat--llm-runtime-observer
                                'terminal
                                (list :status 'completed
                                      :response
                                      (format "research-result-%d" calls)))))
                    ((symbol-function 'superchat-workflow--render-step-header)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat-workflow--finalize)
                     (lambda (&rest _args) (setq finalized t))))
            (let* ((body
                    (with-temp-buffer
                      (insert-file-contents workflow-file)
                      (buffer-string)))
                   (steps
                    (mapcar #'superchat-workflow--parse-line
                            (superchat-workflow-parse-steps body))))
              (superchat-workflow--run-step
               0 steps
               '(:input "event-sourced agents"
                 :lang "en"
                 :date "2026-01-01")
               nil "research-e2e")))
          ;; Simulate process-local loss: the resume view must come from Tape.
          (clrhash superchat-runtime--runs)
          (superchat-db-close)
          (let* ((view
                  (superchat-workflow-instance-view
                   "research-e2e" "research-e2e"))
                 (steps (plist-get view :steps)))
            (should (= 3 calls))
            (should finalized)
            (should (equal '(1 2 3)
                           (mapcar (lambda (step)
                                     (plist-get step :step-id))
                                   steps)))
            (should (cl-every
                     (lambda (step)
                       (and (stringp (plist-get step :run-id))
                            (eq 'advance
                                (plist-get step :resume-decision))
                            (eq 'proved
                                (plist-get
                                 (plist-get step :closure-judgment)
                                 :verdict))))
                     steps))
            (setq prompts (nreverse prompts))
            (should (string-match-p "event-sourced agents" (nth 0 prompts)))
            (should (string-match-p "research-result-1" (nth 1 prompts)))
            (should (string-match-p "research-result-2" (nth 2 prompts)))))
      (superchat-db-close)
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name))
      (delete-directory directory t))))

(ert-deftest workflow-execute-targets-current-session-view ()
  "Direct workflow execution should use the current Session View."
  (let ((target-name (generate-new-buffer-name " *workflow-session*"))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-workflow--load)
                   (lambda (_) "one step"))
                  ((symbol-function 'superchat-workflow--render-workflow-header)
                   (lambda (&rest _) (push superchat-buffer-name seen)))
                  ((symbol-function 'superchat-workflow--run-step)
                   (lambda (&rest _) (push superchat-buffer-name seen))))
          (with-current-buffer (get-buffer-create target-name)
            (setq-local superchat-mode t)
            (superchat-workflow-execute-async "test"))
          (should (equal seen (list target-name target-name))))
      (when (get-buffer target-name)
        (kill-buffer target-name)))))

(ert-deftest workflow-step-submits-runtime-request ()
  "Workflow steps submit through Runtime with workflow-scoped metadata."
  (cl-letf* ((superchat-buffer-name " *workflow-step-request*")
             (workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("ok"))
             (workflow-test--mock-statuses '(completed))
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil)))
    (superchat-workflow--run-step
     0 (list (superchat-workflow--parse-line "@gpt-4o-mini step #docs/a.md"))
     '(:input "" :lang "en" :date "2026-01-01") nil "workflow-test")
    (let ((request (car workflow-test--mock-requests)))
      (should (equal "step" (plist-get request :prompt)))
      (should (equal "gpt-4o-mini" (plist-get request :target-model)))
      (should (equal '("docs/a.md") (plist-get request :context-files)))
      (should (equal "workflow-step-1" (plist-get request :actor-id)))
      (should (equal "workflow-test" (plist-get request :correlation-id)))
      (should (equal "workflow-test" (plist-get request :session-id)))
      (should (equal "workflow" (plist-get request :capability)))
      (should (eq 'workflow (plist-get request :topology)))
      (should (equal "default" (plist-get request :authority-policy)))
      (should (eq 'workflow (plist-get request :surface)))
      (should (eq nil (plist-get request :record-conversation)))
      (should (functionp (plist-get request :stream-function)))
      (should (functionp (plist-get request :delivery-function))))))

(ert-deftest workflow-async-chain-executes-all-steps ()
  "Async executor submits Runtime requests for each step in sequence."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("step1 answer" "step2 answer" "step3 answer"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ;; Prevent buffer operations from failing
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results) nil)))
    (let* ((raw-steps '("step1" "step2" "step3"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step 0 parsed vars nil))
    (should (= 3 workflow-test--mock-step-count))
    ;; Calls are pushed in reverse order; check we got all three prompts
    (should (= 3 (length workflow-test--mock-calls)))))

(ert-deftest workflow-async-chain-passes-result-variable ()
  "Async executor injects $result from previous step into next prompt."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("result of step one" "result of step two"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results) nil)))
    (let* ((raw-steps '("step one" "continue: $result"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step 0 parsed vars nil))
    (should (= 2 workflow-test--mock-step-count))
    ;; The second prompt should have $result expanded to "result of step one"
    ;; (calls are in reverse order, so second call is (car workflow-test--mock-calls))
    (let ((second-prompt (car workflow-test--mock-calls)))
      (should second-prompt)
      (should (string-match-p "result of step one" second-prompt))
      (should-not (string-match-p "\\$result" second-prompt)))))

(ert-deftest workflow-async-chain-passes-stepN-variables ()
  "Async executor injects $step1, $step2 from prior step outputs."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("first output" "second output" "third output"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results) nil)))
    (let* ((raw-steps '("step1" "step2" "summary: $step1 vs $step2"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step 0 parsed vars nil))
    (should (= 3 workflow-test--mock-step-count))
    ;; Third prompt should reference both previous outputs
    (let ((third-prompt (car workflow-test--mock-calls)))
      (should third-prompt)
      (should (string-match-p "first output" third-prompt))
      (should (string-match-p "second output" third-prompt)))))

(ert-deftest workflow-async-chain-model-override ()
  "Async executor passes @model override to the Runtime request."
  (let ((captured-model nil))
    (cl-letf* ((workflow-test--mock-calls nil)
               (workflow-test--mock-requests nil)
               (workflow-test--mock-answers '("answer"))
               (workflow-test--mock-statuses nil)
               (workflow-test--mock-step-count 0)
               ((symbol-function 'superchat-runtime-submit)
                (lambda (request)
                  (setq captured-model (plist-get request :target-model))
                  (workflow-test--mock-runtime-submit request)))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) nil)))
      (let* ((raw-steps '("@gpt-4o-mini hello"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil))
      (should (equal "gpt-4o-mini" captured-model)))))

(ert-deftest workflow-async-chain-reuses-correlation-id ()
  "All steps in one workflow share one Runtime correlation id."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("one" "two"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results) nil)))
    (let* ((raw-steps '("first" "second"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step 0 parsed vars nil "workflow-test"))
    (should (= 2 (length workflow-test--mock-requests)))
    (should
     (equal '("workflow-test" "workflow-test")
            (mapcar (lambda (request)
                      (plist-get request :correlation-id))
                    workflow-test--mock-requests)))
    (should
     (equal '("workflow-test" "workflow-test")
            (mapcar (lambda (request)
                      (plist-get request :session-id))
                    workflow-test--mock-requests)))))

(ert-deftest workflow-async-chain-sync-error-stops-chain ()
  "A synchronous Runtime error triggers fail-step and stops the chain."
  (let ((runtime-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat-runtime-submit)
                (lambda (_request)
                  (cl-incf runtime-calls)
                  (error "simulated Runtime failure")))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num total error-msg _step-results)
                  (setq fail-called (list step-num total error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil)))
    ;; Error on step 1: exactly one Runtime call, fail-step invoked, no finalize.
    (should (= 1 runtime-calls))
    (should (equal '(1 3) (list (nth 0 fail-called) (nth 1 fail-called))))
    (should (string-match-p "simulated Runtime failure" (nth 2 fail-called)))
    (should-not finalized)))

(ert-deftest workflow-async-chain-failed-status-stops-chain ()
  "A failed Runtime status stops the chain via fail-step."
  (let ((runtime-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat-runtime-submit)
                (lambda (request)
                  (cl-incf runtime-calls)
                  (funcall (plist-get request :delivery-function)
                           "[Error: backend unreachable]" 'failed 9)))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num _total error-msg _step-results)
                  (setq fail-called (list step-num error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil)))
    (should (= 1 runtime-calls))
    (should (equal 1 (car fail-called)))
    (should (string-match-p "backend unreachable" (cadr fail-called)))
    (should-not finalized)))

;; ═══════════════════════════════════════════════════════
;; `>>' prefix parsing (>> = workflow, > = skill)
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-double-arrow-prefix-parses-to-workflow-skill ()
  "\">>name args\" parses to the reserved `workflow' skill token."
  (let ((turn (superchat-core--parse-input
               (superchat-turn-new ">>research emacs news"))))
    (should (equal "workflow" (superchat-turn-skill turn)))
    (should (equal "research emacs news" (superchat-turn-clean-input turn)))))

(ert-deftest workflow-double-arrow-without-name ()
  "\">>\" with no name still routes to the workflow branch."
  (let ((turn (superchat-core--parse-input (superchat-turn-new ">>"))))
    (should (equal "workflow" (superchat-turn-skill turn)))
    (should (equal "" (superchat-turn-clean-input turn)))))

(ert-deftest workflow-single-arrow-still-parses-skill ()
  "\">name\" remains a skill invocation, untouched by the workflow prefix."
  (let ((turn (superchat-core--parse-input
               (superchat-turn-new ">coder write a fn"))))
    (should (equal "coder" (superchat-turn-skill turn)))
    (should (equal "write a fn" (superchat-turn-clean-input turn)))))

;; ═══════════════════════════════════════════════════════
;; Regression tests — parser and substitution edge cases
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-substitute-step10-not-clobbered-by-step1 ()
  "$step10 must not be corrupted by the $step1 replacement."
  (let ((result (superchat-workflow--substitute-vars
                 "A=$step1 B=$step10"
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("one" "2" "3" "4" "5" "6" "7" "8" "9" "ten"))))
    (should (string= "A=one B=ten" result))))

(ert-deftest workflow--parse-line-ignores-inline-at-mention ()
  "A mid-line @word is prose, not a model override."
  (let ((result (superchat-workflow--parse-line
                 "Summarize feedback from @john.doe about the release")))
    (should-not (plist-get result :model))
    (should (string-match-p "@john\\.doe" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-ignores-inline-hash-number ()
  "A #token without `.' or `/' is prose, not a context file."
  (let ((result (superchat-workflow--parse-line
                 "Translate issue #42 into French")))
    (should-not (plist-get result :contexts))
    (should (string-match-p "#42" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-ignores-mid-line-slash ()
  "A mid-line /word is prose, not a command."
  (let ((result (superchat-workflow--parse-line
                 "Compare foo/bar with the baseline")))
    (should-not (plist-get result :command))
    (should (string-match-p "foo/bar" (plist-get result :prompt)))))

(ert-deftest workflow-resolve-prompt-expands-command-template ()
  "/command steps expand the command's prompt template with $input."
  (cl-letf (((symbol-function 'superchat--lookup-command-template)
             (lambda (name)
               (when (string= name "summarize")
                 "Summarize the following: $input"))))
    (let ((prompt (superchat-workflow--resolve-prompt
                   "summarize $result" "summarize $result"
                   '(:input "" :lang "en" :date "2026-01-01")
                   '("step one output"))))
      (should (string= "Summarize the following: step one output" prompt)))))

(ert-deftest workflow-resolve-prompt-falls-back-without-template ()
  "Steps whose /command has no template fall back to the raw prompt."
  (cl-letf (((symbol-function 'superchat--lookup-command-template)
             (lambda (_name) nil)))
    (let ((prompt (superchat-workflow--resolve-prompt
                   "unknown-cmd foo" "unknown-cmd foo"
                   '(:input "" :lang "en" :date "2026-01-01")
                   nil)))
      (should (string= "unknown-cmd foo" prompt)))))

;; ═══════════════════════════════════════════════════════
;; Legacy import tests (v0.7)
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-import-legacy-synthesises-skill ()
  "Importing a .workflow file creates a valid SKILL.md."
  (let* ((tmp-dir (make-temp-file "wf-import" t))
         (legacy-file (expand-file-name "test.workflow" tmp-dir))
         (target-dir (expand-file-name "skills" tmp-dir)))
    (unwind-protect
        (progn
          ;; Write a fake legacy .workflow file
          (with-temp-file legacy-file
            (insert "/web-search \"news\"\n/summarize\n"))
          ;; Import it
          (let ((names (superchat-workflow-import-legacy-dir
                        tmp-dir target-dir)))
            (should (equal 1 (length names)))
            (let ((skill-file (expand-file-name
                               "skill-test/SKILL.md"
                               target-dir)))
              ;; Check SKILL.md was created
              (should (file-exists-p skill-file))
              (with-temp-buffer
                (insert-file-contents skill-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "name: test" content))
                  (should (string-match-p "type: workflow" content))
                  (should (string-match-p "/web-search" content))
                  (should (string-match-p "/summarize" content)))))))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest workflow/research-recovery-commands-discover-persisted-instances ()
  "Research recovery commands complete persisted instance ids."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((directory (make-temp-file "superchat-research-discovery-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-workspace--registry nil)
         (superchat--subagent-running nil)
         (superchat-subagent-timeout nil)
         first second choices)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (cl-letf (((symbol-function 'superchat--execute-llm-query)
                     (lambda (turn)
                       (list :prompt (superchat-turn-prompt turn)
                             :system-prompt "system"
                             :tools 'none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _) nil)))
            (setq first (superchat-research-workflow "First persisted study")
                  second (superchat-research-workflow "Second persisted study")))
          (clrhash superchat-runtime--runs)
          (setq superchat-workspace--registry nil
                superchat--subagent-running nil)
          (superchat-db-close)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (push collection choices)
                       (car collection))))
            (call-interactively #'superchat-research-workflow-status)
            (call-interactively #'superchat-research-workflow-resume))
          (should (= 2 (length choices)))
          (dolist (collection choices)
            (should (equal (sort (copy-sequence collection) #'string<)
                           (sort (list first second) #'string<)))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest workflow/research-team-survives-runtime-restart ()
  "Collector, analyst, and reviewer recover from Tape and durable Artifacts."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((directory (make-temp-file "superchat-research-team-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat--subagent-running nil)
         (superchat-subagent-timeout nil)
         (superchat-closure-terminal-guard-enabled t)
         (calls 0)
         prompts
         instance-id)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (cl-letf (((symbol-function 'superchat--execute-llm-query)
                     (lambda (turn)
                       (list :prompt (superchat-turn-prompt turn)
                             :system-prompt "system"
                             :tools 'none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _args)
                       (cl-incf calls)
                       (push prompt prompts)
                       (let* ((role
                               (cond
                                ((string-match-p "\\[research/collector\\]"
                                                 prompt)
                                 "collector")
                                ((string-match-p "\\[research/analyst\\]"
                                                 prompt)
                                 "analyst")
                                ((string-match-p "\\[research/reviewer\\]"
                                                 prompt)
                                 "reviewer")
                                (t (error "Unknown research stage"))))
                              (context
                               (plist-put
                                (copy-sequence
                                 superchat--tool-execution-context)
                                :approval-function (lambda (_request) t))))
                         (superchat-tool-execute
                          "workspace_publish"
                          #'superchat-workspace-publish
                          (list (format "%s artifact" role) role)
                          :context context)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  (list :status 'completed
                                        :response
                                        (format "%s complete" role)))))))
            ;; Drop the first completion callback to model Emacs exiting after
            ;; Collector committed its Run and Artifact but before Analyst was
            ;; launched.  Resume must derive the next action from Tape.
            (let ((run-subagent
                   (symbol-function 'superchat--subagent-run-async))
                  (drop-next-callback t))
              (cl-letf
                  (((symbol-function 'superchat--subagent-run-async)
                    (lambda (preset task context callback &optional depth)
                      (funcall
                       run-subagent preset task context
                       (lambda (report)
                         (if drop-next-callback
                             (setq drop-next-callback nil)
                           (funcall callback report)))
                       depth))))
                (setq instance-id
                      (superchat-research-workflow
                       "How can event-sourced agents resume safely?"))))
            (should (= 1 calls))
            ;; Simulate an Emacs restart: only SQLite-backed facts survive.
            (clrhash superchat-runtime--runs)
            (setq superchat-workspace--registry nil
                  superchat--subagent-running nil)
            (superchat-db-close)
            (let* ((view (superchat-research-workflow-view instance-id))
                   (steps (plist-get view :steps)))
              (should
               (equal '(advance start blocked)
                      (mapcar
                       (lambda (step) (plist-get step :resume-decision))
                       steps))))
            (should (eq 'completed
                        (plist-get
                         (superchat-research-workflow-resume instance-id)
                         :status)))
            (should (= 3 calls))
            ;; The model-facing reference snapshot must preserve JSON array
            ;; semantics at every cardinality, including zero and one.
            (dolist (expected '((collector)
                                (analyst "collector")
                                (reviewer "collector" "analyst")))
              (let* ((role (car expected))
                     (prompt
                      (cl-find-if
                       (lambda (item)
                         (string-match-p
                          (format "\\[research/%s\\]" role) item))
                       prompts)))
                (should
                 (string-match
                  "Prior Artifact references: \\(.*\\)\\'" prompt))
                (let* ((json (match-string 1 prompt))
                       (references
                        (json-parse-string json
                                           :object-type 'plist
                                           :array-type 'list)))
                  (should (string-prefix-p "[" json))
                  (should
                   (equal (cdr expected)
                          (mapcar
                           (lambda (reference)
                             (plist-get reference :role))
                           references)))
                  (dolist (reference references)
                    (should (stringp (plist-get reference :artifact-id)))
                    (should (stringp (plist-get reference :revision)))))))
            ;; A second restart must still answer all three recovery questions.
            (clrhash superchat-runtime--runs)
            (setq superchat-workspace--registry nil
                  superchat--subagent-running nil)
            (superchat-db-close)
            (let* ((view (superchat-research-workflow-view instance-id))
                   (steps (plist-get view :steps)))
              (should (equal '(collector analyst reviewer)
                             (mapcar (lambda (step) (plist-get step :role))
                                     steps)))
              (should (cl-every
                       (lambda (step)
                         (and (eq 'advance (plist-get step :resume-decision))
                              (eq 'deliver (plist-get step :run-decision))
                              (cl-every
                               (lambda (artifact)
                                 (and (eq 'deliver
                                          (plist-get artifact
                                                     :delivery-decision))
                                      (plist-get artifact :available)))
                               (plist-get step :artifacts))))
                       steps))
              ;; A trusted Run does not make externally changed bytes safe.
              (let* ((reviewer (car (last steps)))
                     (artifact (car (plist-get reviewer :artifacts)))
                     (path (substring (plist-get artifact :uri)
                                      (length "file://"))))
                (with-temp-file path (insert "tampered"))
                (let* ((changed
                        (superchat-research-workflow-view instance-id))
                       (changed-reviewer
                        (car (last (plist-get changed :steps)))))
                  (should (eq 'deliver
                              (plist-get changed-reviewer :run-decision)))
                  (should (eq 'manual-review
                              (plist-get changed-reviewer :resume-decision)))
                  (should-not
                   (plist-get (car (plist-get changed-reviewer :artifacts))
                              :available)))))))
      (superchat-db-close)
      (delete-directory directory t))))

(provide 'test-workflow)
;;; test-workflow.el ends here
