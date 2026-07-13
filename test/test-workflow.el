;;; test-workflow.el — Workflow parser, substitution, and async engine tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-workflow.el:
;;   - Parsing (steps, per-line annotations, variable substitution)
;;   - File loading (.workflow discovery)
;;   - Async execution chain (mock LLM, variable flow, error handling)

;;; Code:

(require 'ert)
(require 'superchat-workflow)

;; ── Mock externals (no superchat.el loaded) ──
(defvar superchat-buffer-name "*superchat-test*")
(defvar superchat-lang "English")
(defun superchat--record-message (_role _content) nil)
(defun superchat--insert-prompt () nil)

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
;; Part C — Async execution chain (mock LLM)
;; ═══════════════════════════════════════════════════════

(defvar workflow-test--mock-calls nil
  "List of prompts sent to the mock LLM during a test run.")
(defvar workflow-test--mock-answers nil
  "List of answers the mock LLM returns, consumed in order.")
(defvar workflow-test--mock-step-count 0
  "Number of times the mock LLM was called.")

(defun workflow-test--mock-llm (prompt callback _stream-cb &optional _model _context-files _tools _agent-mode)
  "Mock superchat--llm-generate-answer.
Pushes PROMPT onto workflow-test--mock-calls and calls CALLBACK
with the next answer from workflow-test--mock-answers."
  (push prompt workflow-test--mock-calls)
  (cl-incf workflow-test--mock-step-count)
  (let ((answer (or (pop workflow-test--mock-answers)
                    "[mock: no answer configured]")))
    (funcall callback answer)))

(ert-deftest workflow-steps-enable-agent-tool-wrapping ()
  "Workflow steps opt into the shared agent-loop wrapper."
  (let (captured)
    (cl-letf (((symbol-function 'superchat--llm-generate-answer)
               (lambda (_prompt callback _stream &rest args)
                 (setq captured args)
                 (funcall callback "ok")))
              ((symbol-function 'superchat-workflow--render-step-header)
               (lambda (&rest _args) nil))
              ((symbol-function 'superchat-workflow--finalize)
               (lambda (&rest _args) nil))
              ((symbol-function 'superchat--record-message)
               (lambda (&rest _args) nil)))
      (superchat-workflow--run-step
       0 (list (superchat-workflow--parse-line "step"))
       '(:input "" :lang "en" :date "2026-01-01") nil))
    (should (eq t (nth 3 captured)))))

(ert-deftest workflow-async-chain-executes-all-steps ()
  "Async executor calls LLM for each step in sequence."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-answers
              '("step1 answer" "step2 answer" "step3 answer"))
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat--llm-generate-answer)
              #'workflow-test--mock-llm)
             ;; Prevent buffer operations from failing
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (_role _content) nil))
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
             (workflow-test--mock-answers
              '("result of step one" "result of step two"))
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat--llm-generate-answer)
              #'workflow-test--mock-llm)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (_role _content) nil))
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
             (workflow-test--mock-answers
              '("first output" "second output" "third output"))
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat--llm-generate-answer)
              #'workflow-test--mock-llm)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (_role _content) nil))
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
  "Async executor passes @model override to the LLM call."
  (let ((captured-model nil))
    (cl-letf* ((workflow-test--mock-calls nil)
               (workflow-test--mock-answers '("answer"))
               (workflow-test--mock-step-count 0)
               ((symbol-function 'superchat--llm-generate-answer)
                (lambda (prompt callback _stream-cb &optional model _context-files _tools _agent-mode)
                  (push prompt workflow-test--mock-calls)
                  (cl-incf workflow-test--mock-step-count)
                  (setq captured-model model)
                  (funcall callback (or (pop workflow-test--mock-answers)
                                        "[mock]"))))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (_role _content) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) nil)))
      (let* ((raw-steps '("@gpt-4o-mini hello"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil))
      (should (equal "gpt-4o-mini" captured-model)))))

(ert-deftest workflow-async-chain-sync-error-stops-chain ()
  "A synchronous LLM error triggers fail-step and stops the chain."
  (let ((llm-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat--llm-generate-answer)
                (lambda (_prompt _callback _stream-cb &optional _model _context-files _tools _agent-mode)
                  (cl-incf llm-calls)
                  (error "simulated LLM failure")))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (_role _content) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num total error-msg _step-results)
                  (setq fail-called (list step-num total error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil)))
    ;; Error on step 1: exactly one LLM call, fail-step invoked, no finalize.
    (should (= 1 llm-calls))
    (should (equal '(1 3) (list (nth 0 fail-called) (nth 1 fail-called))))
    (should (string-match-p "simulated LLM failure" (nth 2 fail-called)))
    (should-not finalized)))

(ert-deftest workflow-async-chain-error-string-stops-chain ()
  "An \"[Error: ...]\" answer string stops the chain via fail-step.
`superchat--llm-generate-answer' reports async failures as error
strings through the normal completion callback."
  (let ((llm-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat--llm-generate-answer)
                (lambda (_prompt callback _stream-cb &optional _model _context-files _tools _agent-mode)
                  (cl-incf llm-calls)
                  (funcall callback "[Error: backend unreachable]")))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (_role _content) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num _total error-msg _step-results)
                  (setq fail-called (list step-num error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step 0 parsed vars nil)))
    (should (= 1 llm-calls))
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

(provide 'test-workflow)
;;; test-workflow.el ends here
