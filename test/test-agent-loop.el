;;; test-agent-loop.el --- Tests for superchat-agent-loop -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Runtime tool wrapper, safety guardrails, and
;; per-tool lifecycle hooks.

;;; Code:

(require 'ert)
(require 'llm nil t)
(require 'superchat-preset)
(require 'superchat-agent-loop)

(defvar superchat-test--approval-function (lambda (_request) t))

(defmacro superchat-test--mock-render (&rest body)
  "Execute BODY with rendering and persistence isolated."
  `(let ((superchat-authority-approval-function
          superchat-test--approval-function))
     (cl-letf (((symbol-function 'superchat--agent-render-tool-call) #'ignore)
               ((symbol-function 'superchat--agent-render-tool-result) #'ignore)
               ((symbol-function 'superchat-tape-append-event)
                (lambda (_event) 1)))
       ,@body)))

(ert-deftest test-agent-wrap-sync-tool-calls-original ()
  "A wrapped sync tool should call the original function and return its result."
  (let* ((superchat--agent-tool-call-count 0)
         (superchat-agent-max-tool-calls 10)
         (superchat-agent-confirm-destructive nil)
         (original-called nil)
         (orig-fn (lambda (&rest args)
                    (setq original-called args)
                    "mock-result"))
         (wrapped (superchat--agent-wrap-function "mock-tool" orig-fn nil)))
    (superchat-test--mock-render
     (should (functionp wrapped))
     (let ((result (apply wrapped '("arg1" "arg2"))))
       (should (string= result "mock-result"))
       (should (equal original-called '("arg1" "arg2")))
       (should (= superchat--agent-tool-call-count 1))))))

(ert-deftest test-agent-wrap-respects-max-tool-calls ()
  "A wrapped tool should stop after `superchat-agent-max-tool-calls'."
  (let ((superchat--agent-tool-call-count 0)
        (superchat-agent-max-tool-calls 2)
        (superchat-agent-confirm-destructive nil)
        (wrapped (superchat--agent-wrap-function
                  "mock-tool"
                  (lambda (&rest _args) "ok")
                  nil)))
    (superchat-test--mock-render
     (apply wrapped '("a"))
     (apply wrapped '("b"))
     (let ((result (apply wrapped '("c"))))
       (should (string-match-p "exceeded maximum" result))
       (should (= superchat--agent-tool-call-count 2))))))

(ert-deftest test-agent-profile-guardrails-only-tighten-globals ()
  "Profile limits compose with globals using min/OR semantics."
  (let ((superchat-agent-max-tool-calls 50)
        (superchat-agent-confirm-destructive t)
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer)
                 (push message warnings))))
      (let* ((preset (superchat-preset-from-plist
                      (list :name "unsafe" :max-tool-calls 80
                            :confirm-destructive nil)))
             (effective (superchat--agent-effective-guardrails preset)))
        (should (= 50 (plist-get effective :max-tool-calls)))
        (should (eq t (plist-get effective :confirm-destructive)))))
    (should (= 2 (length warnings))))
  (let* ((superchat-agent-max-tool-calls 50)
         (superchat-agent-confirm-destructive nil)
         (preset (superchat-preset-from-plist
                  (list :name "strict" :max-tool-calls 20
                        :confirm-destructive t)))
         (effective (superchat--agent-effective-guardrails preset)))
    (should (= 20 (plist-get effective :max-tool-calls)))
    (should (eq t (plist-get effective :confirm-destructive)))))

(ert-deftest test-agent-wrapper-captures-profile-tool-budget ()
  "A wrapped main-agent tool enforces its captured profile budget."
  (let* ((superchat--agent-tool-call-count 0)
         (guardrails '(:max-tool-calls 2 :confirm-destructive nil))
         (wrapped (superchat--agent-wrap-function
                   "mock-tool" (lambda (&rest _args) "ok") nil guardrails)))
    (superchat-test--mock-render
     (funcall wrapped)
     (funcall wrapped)
     (should (string-match-p "exceeded maximum" (funcall wrapped))))))

(ert-deftest test-agent-wrapper-captures-profile-confirmation-policy ()
  "A main-agent profile can require destructive-tool confirmation."
  (let* ((superchat--agent-tool-call-count 0)
         (superchat-agent-destructive-tools '("write-file"))
         (guardrails '(:max-tool-calls 10 :confirm-destructive t))
         (called nil)
         (wrapped (superchat--agent-wrap-function
                   "write-file" (lambda (&rest _) (setq called t))
                   nil guardrails)))
    (let ((superchat-test--approval-function #'ignore))
      (superchat-test--mock-render
        (should (string-match-p "cancelled" (funcall wrapped "file" "body")))))
    (should-not called)))

(ert-deftest test-agent-wrapper-captures-runtime-correlation ()
  "Delayed tools retain the Runtime IDs present when wrapping began."
  (let* ((runtime-context
          '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :correlation-id "correlation-1" :causation-id 9
            :attendance attended))
         (superchat--tool-execution-context runtime-context)
         (wrapper (superchat--agent-make-wrapper nil))
         captured)
    (cl-letf (((symbol-function 'superchat--agent-wrap-tool)
               (lambda (_tool _guardrails _counter context)
                 (setq captured context))))
      (funcall wrapper 'tool))
    (should (equal captured runtime-context))))

(ert-deftest test-agent-wrapper-refreshes-runtime-envelope ()
  "Delayed tools observe a recovery policy selected after wrapping."
  (let* ((runtime-context
          '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :context-envelope (:status satisfied) :attendance attended))
         (wrapped
          (superchat--agent-wrap-function
           "workspace_read" (lambda (&rest _) "unused") nil nil nil nil
           runtime-context))
         captured)
    (cl-letf (((symbol-function 'superchat-runtime--entry)
               (lambda (_run-id)
                 '(:context-envelope
                   (:status blocked
                    :experience-action-policy
                    (:dependent-tools ("workspace_read"))))))
              ((symbol-function 'superchat--agent-render-tool-call) #'ignore)
              ((symbol-function 'superchat--agent-render-tool-result) #'ignore)
              ((symbol-function 'superchat-tool-execute)
               (lambda (_name _function _args &rest options)
                 (setq captured (plist-get options :context))
                 "value")))
      (funcall wrapped))
    (should
     (equal '(:dependent-tools ("workspace_read"))
            (plist-get (plist-get captured :context-envelope)
                       :experience-action-policy)))))

(ert-deftest test-agent-wrapper-propagates-immutable-tool-origin ()
  "Attempt-bound llm wrappers pass their snapshot origin into Authority."
  (let* ((runtime-context
          '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :evidence-contract "epistemic-learning-v0"
            :attendance attended))
         (wrapped
          (superchat--agent-wrap-function
           "lookup" (lambda (_key) "unused") nil nil nil nil
           runtime-context))
         captured)
    (cl-letf (((symbol-function 'superchat--agent-render-tool-call) #'ignore)
              ((symbol-function 'superchat--agent-render-tool-result) #'ignore)
              ((symbol-function 'superchat-tool-execute)
               (lambda (_name _function _args &rest options)
                 (setq captured (copy-tree (plist-get options :context) t))
                 "value")))
      (let ((superchat--llm-tool-origin
             '(:evidence-contract "epistemic-learning-v0"
               :evidence-attempt 2
               :decision-snapshot-event-id "evt_snapshot_2"
               :tool-call-id "call-9"
               :tool-call-name "lookup"
               :tool-call-ordinal 1
               :tool-call-arguments ((key . "alpha")))))
        (should (equal "value" (funcall wrapped "alpha")))))
    (should (= 2 (plist-get captured :evidence-attempt)))
    (should (equal "evt_snapshot_2"
                   (plist-get captured :decision-snapshot-event-id)))
    (should (equal "call-9" (plist-get captured :tool-call-id)))
    (should (equal '((key . "alpha"))
                   (plist-get captured :tool-call-arguments)))
    (should (equal "run-1" (plist-get captured :run-id)))))

(ert-deftest test-agent-wrapper-renders-eligible-call-after-authority-preflight ()
  "An eligible call is not rendered when its evidence preflight fails."
  (let* ((runtime-context
          '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :evidence-contract "epistemic-learning-v0"
            :attendance attended))
         (wrapped
          (superchat--agent-wrap-function
           "lookup" (lambda (_key) "unused") nil nil nil nil
           runtime-context))
         rendered)
    (cl-letf (((symbol-function 'superchat--agent-render-tool-call)
               (lambda (&rest _args) (setq rendered t)))
              ((symbol-function 'superchat-tool-execute)
               (lambda (&rest _args) (error "evidence preflight failed"))))
      (let ((superchat--llm-tool-origin
             '(:evidence-contract "epistemic-learning-v0"
               :evidence-attempt 1
               :decision-snapshot-event-id "evt_snapshot_1"
               :tool-call-id "call-1"
               :tool-call-name "lookup"
               :tool-call-ordinal 1
               :tool-call-arguments ((api_key . "secret")))))
        (should-error (funcall wrapped "secret"))))
    (should-not rendered)))

(ert-deftest test-agent-eval-elisp-is-destructive-by-default ()
  "Arbitrary Elisp evaluation must pass the destructive confirmation gate."
  (should (member "eval_elisp" (default-value
                                 'superchat-agent-destructive-tools)))
  (let ((superchat-agent-confirm-destructive t))
    (should (superchat--agent-confirm-p "eval_elisp" '((expression . "(+ 1 1)"))))))

(ert-deftest test-agent-eshell-command-is-destructive-by-default ()
  "Eshell can mutate files and must pass the destructive confirmation gate."
  (should (member "eshell-command" (default-value
                                     'superchat-agent-destructive-tools)))
  (let ((superchat-agent-confirm-destructive t))
    (should (superchat--agent-confirm-p "eshell-command" '((command . "pwd"))))))

(ert-deftest test-agent-wrap-async-tool-calls-original ()
  "A wrapped async tool should call the original with a callback."
  (let ((superchat--agent-tool-call-count 0)
        (superchat-agent-max-tool-calls 10)
        (superchat-agent-confirm-destructive nil)
        (callback-result nil)
        (wrapped (superchat--agent-wrap-function
                  "async-mock"
                  (lambda (callback &rest args)
                    (funcall callback (concat "result:" (car args))))
                  t)))
    (superchat-test--mock-render
     (funcall wrapped (lambda (result) (setq callback-result result)) "arg")
     (should (string= callback-result "result:arg"))
     (should (= superchat--agent-tool-call-count 1)))))

(ert-deftest test-agent-wrap-tool-returns-llm-tool ()
  "`superchat--agent-wrap-tool' should return an llm tool struct."
  (skip-unless (fboundp 'llm-make-tool))
  (let* ((tool (llm-make-tool
                :function (lambda (&rest _args) "ok")
                :name "test-tool"
                :description "A test tool"
                :args nil
                :async nil))
         (wrapped (superchat--agent-wrap-tool tool)))
    (should wrapped)
    (should (string= (llm-tool-name wrapped) "test-tool"))
    (should (functionp (llm-tool-function wrapped)))))

;; ═══════════════════════════════════════════════════════════
;; Per-tool lifecycle hook tests
;; ═══════════════════════════════════════════════════════════

(ert-deftest test-agent-hooks-pre-called-before-execution ()
  "Pre-tool hook fires before the tool runs."
  (let ((called nil))
    (let ((superchat-agent-pre-tool-functions
           (list (lambda (name args)
                   (setq called (list name (car args)))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (x) (format "got %s" x)))
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (funcall wrapped "hello")
         (should (equal called '("test-tool" "hello"))))))))

(ert-deftest test-agent-hooks-permission-deny-blocks ()
  "Permission hook returning 'deny blocks the tool."
  (let ((superchat-agent-permission-functions
         (list (lambda (_name _args) 'deny))))
    (superchat-test--mock-render
     (let* ((fn (lambda (x) (format "got %s" x)))
            (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
       (let ((result (funcall wrapped "hello")))
         (should (string-match-p "denied by authority policy" result)))))))

(ert-deftest test-agent-hooks-permission-allow-skips-confirm ()
  "Permission hook returning 'allow skips user confirmation."
  (let ((superchat-agent-permission-functions
         (list (lambda (_name _args) 'allow)))
        (superchat-agent-confirm-destructive t)
        (superchat-agent-destructive-tools '("test-tool"))
        (confirmed nil))
    (let ((superchat-test--approval-function
           (lambda (_request) (setq confirmed t) t)))
      (superchat-test--mock-render
       (let* ((fn (lambda (x) (format "got %s" x)))
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (funcall wrapped "hello")
         (should-not confirmed))))))

(ert-deftest test-agent-hooks-post-called-after-success ()
  "Post-tool hook fires after successful execution."
  (let ((post-called nil))
    (let ((superchat-agent-post-tool-functions
           (list (lambda (name args result)
                   (setq post-called (list name (car args) result))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (x) (format "got %s" x)))
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (funcall wrapped "hello")
         (should (equal post-called '("test-tool" "hello" "got hello"))))))))

(ert-deftest test-agent-hooks-failure-called-on-error ()
  "Post-tool-failure hook fires and the model receives the error."
  (let ((failure-called nil))
    (let ((superchat-agent-post-tool-failure-functions
           (list (lambda (name args err)
                   (setq failure-called (list name (car args) (car err)))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (_x) (error "boom")))
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (should (string-match-p "Tool call failed: boom"
                                 (funcall wrapped "hello")))
         (should (equal failure-called '("test-tool" "hello" error))))))))

(ert-deftest test-agent-hooks-all-hooks-run ()
  "Multiple hooks in each category all fire."
  (let ((pre-count 0)
        (post-count 0))
    (let ((superchat-agent-pre-tool-functions
           (list (lambda (_n _a) (cl-incf pre-count))
                 (lambda (_n _a) (cl-incf pre-count))))
          (superchat-agent-post-tool-functions
           (list (lambda (_n _a _r) (cl-incf post-count))
                 (lambda (_n _a _r) (cl-incf post-count)))))
      (superchat-test--mock-render
       (let* ((fn #'identity)
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (funcall wrapped "hello")
         (should (= pre-count 2))
         (should (= post-count 2)))))))

(ert-deftest test-agent-hooks-async-failure-called-on-error ()
  "Post-tool-failure hook fires when an async tool throws synchronously."
  (let ((failure-called nil)
        (callback-result nil)
        (superchat--agent-tool-call-count 0)
        (superchat-agent-max-tool-calls 10)
        (superchat-agent-confirm-destructive nil))
    (let ((superchat-agent-post-tool-failure-functions
           (list (lambda (name args err)
                   (setq failure-called (list name (car args) (car err)))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (_callback &rest _args) (error "async-boom")))
              (wrapped (superchat--agent-wrap-function "async-tool" fn t)))
         (funcall wrapped (lambda (result) (setq callback-result result)) "hello")
         (should (string-match-p "Tool call failed: async-boom" callback-result))
         (should (equal failure-called (list "async-tool" "hello" 'error))))))))

;; ═══════════════════════════════════════════════════════════
;; Call interception — rewrite / block / result filters
;; ═══════════════════════════════════════════════════════════

(defmacro test-agent--sync-env (&rest body)
  "Common bindings for interception tests on the sync wrapper path."
  (declare (indent 0))
  `(let ((superchat--agent-tool-call-count 0)
         (superchat-agent-max-tool-calls 10)
         (superchat-agent-confirm-destructive nil)
         (superchat-agent-tool-rewrite-functions nil)
         (superchat-agent-tool-result-filter-functions nil))
     (superchat-test--mock-render ,@body)))

(ert-deftest test-agent-intercept-blocks-with-custom-reason ()
  "A blocking rewrite hook stops the call; its reason reaches the model."
  (test-agent--sync-env
    (let* ((called nil)
           (superchat-agent-tool-rewrite-functions
            (list (lambda (name args)
                    (when (and (equal name "write-file")
                               (string-match-p "\\.env\\'" (car args)))
                      '(:block "Writing .env files is not allowed.")))))
           (wrapped (superchat--agent-wrap-function
                     "write-file" (lambda (&rest a) (setq called a) "wrote") nil)))
      (let ((result (funcall wrapped "/tmp/secrets.env" "x")))
        (should-not called)
        (should (string-match-p "not allowed" result))
        (should (string-prefix-p "[Tool call blocked:" result)))
      ;; A non-matching call passes untouched.
      (should (string= "wrote" (funcall wrapped "/tmp/notes.txt" "x")))
      (should (equal called '("/tmp/notes.txt" "x"))))))

(ert-deftest test-agent-intercept-rewrites-args-in-hook-order ()
  "Rewrite hooks compose; the tool sees the final arguments."
  (test-agent--sync-env
    (let* ((seen nil)
           (superchat-agent-tool-rewrite-functions
            (list (lambda (_name args)
                    (list :args (cons (concat (car args) "-a") (cdr args))))
                  (lambda (_name args)
                    (list :args (cons (concat (car args) "-b") (cdr args))))))
           (wrapped (superchat--agent-wrap-function
                     "t" (lambda (&rest a) (setq seen a) "ok") nil)))
      (should (string= "ok" (funcall wrapped "base")))
      (should (equal seen '("base-a-b"))))))

(ert-deftest test-agent-result-filter-transforms-what-model-sees ()
  "Result filters chain and replace the tool result."
  (test-agent--sync-env
    (let* ((superchat-agent-tool-result-filter-functions
            (list (lambda (_name _args result)
                    (when (string-match-p "SECRET" result)
                      (replace-regexp-in-string "SECRET-[0-9]+" "[REDACTED]"
                                                result)))))
           (wrapped (superchat--agent-wrap-function
                     "read-file" (lambda (&rest _) "token: SECRET-42 end") nil)))
      (should (string= "token: [REDACTED] end" (funcall wrapped "f"))))))

(ert-deftest test-agent-intercept-async-path ()
  "Block and filter also apply on the async wrapper path."
  (test-agent--sync-env
    (let* ((superchat-agent-tool-rewrite-functions
            (list (lambda (name _args)
                    (when (equal name "bad") '(:block "no")))))
           (superchat-agent-tool-result-filter-functions
            (list (lambda (_n _a result) (upcase result))))
           (results nil)
           (blocked-wrapped (superchat--agent-wrap-function
                             "bad" (lambda (cb &rest _) (funcall cb "ran")) t))
           (ok-wrapped (superchat--agent-wrap-function
                        "good" (lambda (cb &rest _) (funcall cb "ran")) t)))
      (funcall blocked-wrapped (lambda (r) (push r results)) "x")
      (funcall ok-wrapped (lambda (r) (push r results)) "x")
      (should (equal (nreverse results)
                     '("[Tool call blocked: no]" "RAN"))))))

(ert-deftest test-agent-intercept-erroring-hook-is-inert ()
  "A hook that signals must not break the call."
  (test-agent--sync-env
    (let* ((superchat-agent-tool-rewrite-functions
            (list (lambda (&rest _) (error "boom"))))
           (wrapped (superchat--agent-wrap-function
                     "t" (lambda (&rest _) "ok") nil)))
      (should (string= "ok" (funcall wrapped "x"))))))

(provide 'test-agent-loop)

;;; test-agent-loop.el ends here
