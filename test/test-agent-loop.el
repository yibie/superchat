;;; test-agent-loop.el --- Tests for superchat-agent-loop -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Runtime tool wrapper, safety guardrails, and
;; per-tool lifecycle hooks.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'llm nil t)
(require 'superchat-preset)
(require 'superchat-agent-loop)
(require 'superchat-db)

(defvar superchat-test--approval-function (lambda (_request) t))
(defvar superchat--session-id nil)
(defvar superchat-buffer-name)

(defmacro superchat-test--mock-render (&rest body)
  "Execute BODY with rendering and persistence isolated."
  `(let ((superchat-authority-approval-function
          superchat-test--approval-function)
         (superchat--session-id "test-agent-session"))
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
  (require 'superchat)
  (let* ((directory (make-temp-file "superchat-preflight-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal)))
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat-buffer-name (buffer-name)
                      superchat-mode t superchat--session-id "preflight")
          (superchat-db-open)
          (let* ((runtime-context
                  (agent-render-test--published-context "preflight"))
                 (status (superchat-runtime-status
                          (plist-get runtime-context :run-id))))
            (should (equal "preflight" (plist-get status :session-id)))
            (should (equal (plist-get runtime-context :run-id)
                           (plist-get status :run-id)))
            (should (equal "AGENT-CUSTOM-OWNER"
                           (plist-get status :transcript-owner)))
            (setq runtime-context
                  (plist-put runtime-context :evidence-contract
                             "epistemic-learning-v0"))
            (let* ((wrapped
                    (superchat--agent-wrap-function
                     "lookup" (lambda (_key) "unused") nil nil nil nil
                     runtime-context))
                   rendered warnings)
              (cl-letf (((symbol-function 'superchat--agent-render-tool-call)
                         (lambda (&rest _args) (setq rendered t)))
                        ((symbol-function 'display-warning)
                         (lambda (&rest args) (push args warnings)))
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
                  (should (equal '(error "evidence preflight failed")
                                 (should-error (funcall wrapped "secret"))))))
              (should-not warnings)
              (should-not rendered))))
      (maphash (lambda (run _) (superchat-runtime-cancel run "test cleanup"))
               superchat-runtime--runs)
      (superchat-db-close)
      (delete-directory directory t))))

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

(ert-deftest test-agent-tool-result-normalized-for-json-transport ()
  "Raw-byte and unibyte tool results become serializable Unicode.
A byte-level file read or an encoding artifact leaves raw bytes in a
result string; Emacs 31's json-serialize rejects them.  The wrapper
seam must decode them back to ordinary Unicode."
  (let* ((text "superchat 全阶段演示\n这行是占位内容。\n")
         (fixture (make-temp-file "sc-tool-result" nil ".txt" text))
         (unibyte (encode-coding-string text 'utf-8))
         (raw-read
          (with-temp-buffer
            (let ((coding-system-for-read 'binary))
              (insert-file-contents fixture))
            (buffer-string)))
         (plain "plain ascii result")
         (n1 (superchat--normalize-tool-result-text unibyte))
         (n2 (superchat--normalize-tool-result-text raw-read)))
    (delete-file fixture)
    ;; both damaged forms serialize after normalization
    (should (stringp (json-serialize (list :content n1))))
    (should (stringp (json-serialize (list :content n2))))
    ;; ordinary strings pass through unchanged
    (should (equal plain (superchat--normalize-tool-result-text plain)))
    ;; non-strings pass through
    (should (eq :sym (superchat--normalize-tool-result-text :sym)))
    ;; the unibyte decode matches the original text
    (should (string= text n1))))

(ert-deftest test-agent-wrapper-does-not-mint-session-id ()
  "Real Authority rejects a nil-session wrapper without side effects."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-agent-authority-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (session-id "agent-nil-session")
         (superchat--session-id nil)
         (superchat-buffer-name " *agent-nil-session-no-view*")
         (superchat--llm-tool-origin nil)
         (superchat--tool-execution-context nil)
         (superchat--agent-tool-call-count 0)
         (superchat-agent-max-tool-calls 5)
         (superchat-agent-confirm-destructive nil)
         (superchat-agent-pre-tool-functions nil)
         (superchat-agent-permission-functions nil)
         (superchat-agent-tool-rewrite-functions nil)
         (superchat-agent-post-tool-functions nil)
         (superchat-agent-post-tool-failure-functions nil)
         (sync-called nil)
         (async-called nil)
         (callback-count 0)
         callback-result)
    (unwind-protect
        (progn
          (superchat-db-open)
          (with-temp-buffer
            (setq-local superchat--session-id nil)
            (setq-local superchat-buffer-name (buffer-name))
            (let* ((sync-wrapper
                    (superchat--agent-wrap-function
                     "mock-tool"
                     (lambda (&rest _args)
                       (setq sync-called t)
                       "unexpected")
                     nil))
                   (sync-error
                    (condition-case err
                        (progn (funcall sync-wrapper "arg") nil)
                      (error err)))
                   (async-wrapper
                    (superchat--agent-wrap-function
                     "mock-tool"
                     (lambda (callback &rest _args)
                       (setq async-called t)
                       (funcall callback "unexpected"))
                     t))
                   (async-error
                    (condition-case err
                        (progn
                          (funcall async-wrapper
                                   (lambda (result)
                                     (cl-incf callback-count)
                                     (setq callback-result result))
                                   "arg")
                          nil)
                      (error err))))
              (should sync-error)
              (should (string-match-p
                       "Tool execution requires context :session-id"
                       (error-message-string sync-error)))
              (should-not (string-match-p "\\*\\* Tool call:"
                                          (buffer-string)))
              (should-not sync-called)
              (should-not superchat--session-id)
              (should-not (superchat-tape-replay-events session-id))
              (should-not async-error)
              (should (= 1 callback-count))
              (should (string-match-p
                       "Tool execution requires context :session-id"
                       callback-result))
              (should-not async-called)
              (should-not (superchat-tape-replay-events session-id)))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest agent-render/session-selection-matches-runtime-view-contract ()
  "Presentation uses captured Session, exact object, and borrowed compatibility."
  (require 'superchat)
  (with-temp-buffer
    (let ((view (current-buffer)))
      (setq-local superchat-mode t superchat--session-id "new")
      (should-not (superchat--agent-render-buffer view "old"))
      (should (eq view (superchat--agent-render-buffer view "new")))
      (should (eq view (superchat--agent-render-buffer view nil)))
      (rename-buffer (generate-new-buffer-name " *renamed-gate*"))
      (should (eq view (superchat--agent-render-buffer view "new")))
      (setq-local superchat-mode nil)
      (should (eq view (superchat--agent-render-buffer view "old")))
      (setq-local superchat-mode t superchat--session-id nil)
      (should (eq view (superchat--agent-render-buffer view "old")))))
  (let ((dead (generate-new-buffer " *dead-gate*")))
    (kill-buffer dead)
    (should-not (superchat--agent-render-buffer dead "old"))))

(defun agent-render-test--published-context (session)
  "Return actual Runtime context with a published custom presentation owner."
  (let (context)
    (cl-letf (((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _) (setq context (copy-sequence superchat--tool-execution-context)))))
      (superchat-runtime-submit
       (list :prompt "CAPTURE" :session-id session :buffer (current-buffer)
             :tools 'none :deadline nil :transcript-owner "AGENT-CUSTOM-OWNER"
             :approval-function (lambda (_) t) :delivery-function #'ignore)))
    (should (equal "AGENT-CUSTOM-OWNER"
                   (plist-get (superchat-runtime-status (plist-get context :run-id))
                              :transcript-owner)))
    context))

(ert-deftest output-context/result-first-diagnostic-keeps-async-error-delivery ()
  "Missing execution Session can reach async result without a call presentation."
  (require 'superchat)
  (dolist (fault '(error quit))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name))
            (superchat--session-id nil) (warnings 0) (deliveries 0) result)
        (let ((wrapped (superchat--agent-wrap-function
                        "read_buffer" (lambda (&rest _) (ert-fail "Missing Session effect"))
                        t nil (cons 0 nil) (current-buffer) '(:run-id "UNKNOWN"))))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest _) (cl-incf warnings) (signal fault nil))))
            (funcall wrapped (lambda (value) (cl-incf deliveries) (setq result value)))))
        (should (= 1 warnings deliveries))
        (should (string-match-p "requires context :session-id" result))
        (should (equal "" (buffer-string)))))))

(ert-deftest agent-render/session-gate-keeps-execution-and-delayed-delivery ()
  "A hidden tool still executes; delayed result rechecks Session and delivers once."
  (require 'superchat)
  (let* ((directory (make-temp-file "superchat-render-gate-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-agent-confirm-destructive nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         context
         (counter (cons 0 nil))
         (effects 0) (deliveries 0) done result)
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat-buffer-name (buffer-name)
                      superchat-mode t superchat--session-id "captured")
          (superchat-db-open)
          (setq context (agent-render-test--published-context "captured"))
          (let ((wrapped
                 (superchat--agent-wrap-function
                  "read_buffer" (lambda (callback &rest _args)
                                  (cl-incf effects) (setq done callback))
                  t nil counter (current-buffer) context)))
            (funcall wrapped (lambda (value) (cl-incf deliveries) (setq result value)))
            (should (string-match-p "Tool call" (buffer-string)))
            (setq-local superchat--session-id "new")
            (let ((before (buffer-string)))
              (funcall done "DELAYED-RESULT")
              (funcall done "DUPLICATE")
              (should (equal before (buffer-string))))
            (should (= 1 deliveries))
            (should (equal "DELAYED-RESULT" result))
            (should (= 1 effects)))
          (let* ((before (buffer-string))
                 (wrapped (superchat--agent-wrap-function
                           "read_buffer" (lambda () (cl-incf effects) "SYNC")
                           nil nil counter (current-buffer) context)))
            (should (equal "SYNC" (funcall wrapped)))
            (should (equal before (buffer-string)))
            (should (= 2 effects))
            (should (= 2 (car counter)))))
      (maphash (lambda (run _) (superchat-runtime-cancel run "test cleanup")) superchat-runtime--runs)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest agent-render/make-wrapper-wrap-tool-captures-original-session ()
  "The real two-stage wrapper producer preserves creation-time Session identity."
  (require 'superchat)
  (skip-unless (featurep 'llm))
  (let* ((directory (make-temp-file "superchat-wrapper-capture-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (effects 0)
         context)
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat-buffer-name (buffer-name)
                      superchat-mode t superchat--session-id "captured")
          (superchat-db-open)
          (setq context (agent-render-test--published-context "captured"))
          (let* ((before (buffer-string))
                 (superchat--tool-execution-context context)
                 (make-wrapper (superchat--agent-make-wrapper))
                 (_ (setq-local superchat--session-id "new"))
                 (tool (funcall make-wrapper
                                (make-llm-tool :name "read_buffer" :args nil
                                               :description "capture test"
                                               :function (lambda ()
                                                           (cl-incf effects)
                                                           "DELIVERED")))))
            (should (equal "DELIVERED" (funcall (llm-tool-function tool))))
            (should (= 1 effects))
            (should (equal before (buffer-string)))))
      (maphash (lambda (run _) (superchat-runtime-cancel run "test cleanup")) superchat-runtime--runs)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest agent-render/same-session-inactive-refusal-remains-visible ()
  "Session presentation is not a Runtime liveness filter."
  (require 'superchat)
  (let* ((directory (make-temp-file "superchat-render-refusal-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal)))
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat-buffer-name (buffer-name)
                      superchat-mode t superchat--session-id "same")
          (superchat-db-open)
          (let* ((context (agent-render-test--published-context "same"))
                 (_ (superchat-runtime-cancel (plist-get context :run-id)))
                 (wrapped
                 (superchat--agent-wrap-function
                  "read_buffer" (lambda () (ert-fail "Must not execute")) nil
                  nil (cons 0 nil) (current-buffer)
                  context)))
            (should (string-match-p "no longer active" (funcall wrapped)))
            (should (string-match-p "Tool result" (buffer-string)))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest agent-render/owner-projection-restores-after-error-and-quit ()
  "Late rendering restores the full prior owner projection on nonlocal exits."
  (require 'superchat)
  (dolist (failure '(error quit))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name))
            (superchat--runtime-transcript-owner "A"))
        (setq-local superchat-mode t superchat--session-id "S")
        (superchat--response-region-stream "A-STREAM")
        (superchat--response-region-close "A-FINAL" t)
        (let ((superchat--runtime-transcript-owner "B"))
          (superchat--response-region-stream "B-STREAM"))
        (let* ((variables '(superchat--response-region-current-owner
                            superchat--response-start-marker
                            superchat--assistant-response-start-marker
                            superchat--assistant-response-end-marker
                            superchat--response-region-persistent-marker
                            superchat--prompt-start superchat--current-response-parts))
               (projection (mapcar #'symbol-value variables))
               (original (symbol-function 'superchat--response-region-insert-transcript))
               caught)
          (cl-letf (((symbol-function 'superchat--response-region-insert-transcript)
                     (lambda (&rest args)
                       (apply original args)
                       (signal failure '("late render fault")))))
            (condition-case condition
                (superchat--agent-render-tool-call "read_buffer" nil (current-buffer) "S" "A")
              ((error quit) (setq caught (car condition)))))
          (should (eq failure caught))
          (should (equal projection (mapcar #'symbol-value variables)))
          (should (eq (current-buffer) (marker-buffer superchat--assistant-response-start-marker)))
          (should (equal "B" superchat--response-region-current-owner)))))))

(ert-deftest agent-loop/tool-heading-updates-in-place-with-duration ()
  "A tool-call heading updates in place to done/failed with its duration
\(S21), keyed by its own execution id (S21-fix-1), and the `Tool call:
NAME'/`Tool result:' literal texts stay inside the example blocks for
compatibility with pre-S21 assertions."
  (require 'superchat)
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (insert "* User: hi\n")
      (superchat--response-region-prepare)
      (superchat--agent-render-tool-call
       "read_buffer" '(:buffer "x") (current-buffer) nil nil "call-1")
      (let ((text (buffer-string)))
        (should (string-match-p "^\\*\\*\\* ⏳ read_buffer$" text))
        (should (string-match-p "Tool call: read_buffer" text))
        (should (equal "call-1"
                       (get-text-property (string-match "⏳" text) 'superchat-tool-call-id text))))
      (superchat--agent-render-tool-result
       "read_buffer" "contents" (current-buffer) nil nil "call-1")
      (let ((text (buffer-string)))
        (should-not (string-match-p "⏳" text))
        (should (string-match-p "^\\*\\*\\* ✓ read_buffer ([0-9.]+s)$" text))
        (should (string-match-p "Tool result:" text))
        (should (string-match-p "contents" text))
        ;; The id is preserved unchanged across the in-place rewrite.
        (should (equal "call-1"
                       (get-text-property (string-match "✓" text) 'superchat-tool-call-id text)))
        (should-not (superchat--response-region-untrack-pending-tool-call nil "call-1")))
      ;; A second call/result pair, its own distinct id, updates its OWN
      ;; heading -- not the first.
      (superchat--agent-render-tool-call
       "read_buffer" '(:buffer "y") (current-buffer) nil nil "call-2")
      (superchat--agent-render-tool-result
       "read_buffer" "[Tool call blocked: denied]" (current-buffer) nil nil "call-2")
      (let ((text (buffer-string)))
        (should-not (string-match-p "⏳" text))
        (should (= 1 (how-many "✓ read_buffer" (point-min) (point-max))))
        (should (string-match-p "^\\*\\*\\* ✗ read_buffer ([0-9.]+s)$" text))
        (should-not (superchat--response-region-untrack-pending-tool-call nil "call-2"))))))

(defun agent-loop-test--buffer-position-of (needle)
  "Return NEEDLE's first buffer position in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward needle nil t) (match-beginning 0))))

(defun agent-loop-test--heading-id-at (position)
  "Return the `superchat-tool-call-id' of the org heading enclosing POSITION."
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (get-text-property (point) 'superchat-tool-call-id)))

(ert-deftest agent-loop/overlapping-same-name-calls-complete-independently ()
  "Two overlapping same-name calls through the real wrapper resolve their
own heading, and their result lands structurally under that same heading
-- never the other call's -- in either completion order (S21-fix-2, Spec
1 and 3a: keyed by the wrapper's real execution id, never by tool name;
result placed under its own call, not the owner's current tail)."
  (require 'superchat)
  (dolist (order '(in-order out-of-order))
    (with-temp-buffer
      (org-mode)
      (setq-local superchat-buffer-name (buffer-name)
                  superchat--session-id "overlap-session")
      (insert "* User: hi\n")
      (superchat--response-region-prepare)
      (let ((wrapped
             (superchat--agent-wrap-function
              "read_buffer" #'ignore t nil (cons 0 nil) (current-buffer) nil))
            captured-callbacks results)
        (cl-letf (((symbol-function 'superchat-tool-execute)
                   (lambda (_name _fn _args &rest opts)
                     (push (plist-get opts :callback) captured-callbacks))))
          ;; Issue both calls before either resolves -- no close happens
          ;; between them since neither call renders any narration text.
          (let ((superchat--llm-tool-origin '(:tool-call-id "call-1")))
            (funcall wrapped (lambda (r) (push (cons 1 r) results)) "FIRST-ARGS"))
          (let ((superchat--llm-tool-origin '(:tool-call-id "call-2")))
            (funcall wrapped (lambda (r) (push (cons 2 r) results)) "SECOND-ARGS")))
        (should (= 2 (length captured-callbacks)))
        (should (= 2 (how-many "^\\*\\*\\* ⏳ read_buffer" (point-min) (point-max))))
        ;; `captured-callbacks' is newest-first (`push'): index 0 is call 2's
        ;; own `done', index 1 is call 1's.
        (if (eq order 'in-order)
            (progn
              (funcall (nth 1 captured-callbacks) "FIRST-RESULT")
              (funcall (nth 0 captured-callbacks) "SECOND-RESULT"))
          (funcall (nth 0 captured-callbacks) "SECOND-RESULT")
          (funcall (nth 1 captured-callbacks) "FIRST-RESULT"))
        (should (= 2 (length results)))
        (should-not (string-match-p "⏳" (buffer-string)))
        (should (= 2 (how-many "^\\*\\*\\* ✓ read_buffer ([0-9.]+s)"
                               (point-min) (point-max))))
        ;; Structural containment (S21-fix-2, Spec 3a): the org heading
        ;; enclosing each result -- and each call's own args block -- is
        ;; that same call's own heading, identified by its id, never the
        ;; other call's, regardless of delivery order.
        (should (equal "call-1"
                       (agent-loop-test--heading-id-at
                        (agent-loop-test--buffer-position-of "FIRST-ARGS"))))
        (should (equal "call-2"
                       (agent-loop-test--heading-id-at
                        (agent-loop-test--buffer-position-of "SECOND-ARGS"))))
        (should (equal "call-1"
                       (agent-loop-test--heading-id-at
                        (agent-loop-test--buffer-position-of "FIRST-RESULT"))))
        (should (equal "call-2"
                       (agent-loop-test--heading-id-at
                        (agent-loop-test--buffer-position-of "SECOND-RESULT"))))
        ;; call-1's result sits before call-2's own heading either way --
        ;; the sole content order this fix guarantees; the two calls' own
        ;; issue order is independent of which one's result is inserted
        ;; first, so it is not itself asserted here.
        (should (< (agent-loop-test--buffer-position-of "FIRST-RESULT")
                   (agent-loop-test--buffer-position-of "SECOND-ARGS")))
        (should-not (superchat--response-region-untrack-pending-tool-call nil "call-1"))
        (should-not (superchat--response-region-untrack-pending-tool-call nil "call-2"))))))

(ert-deftest agent-loop/uncompleted-call-becomes-stopped-on-final-close ()
  "A call whose result never arrives is rewritten to `stopped' by a REAL
Runtime cancellation reaching the real View Sink's terminal close -- not a
direct `superchat--response-region-close' call -- and no pending record is
left behind (S21-fix-2, Spec 2 and 3b: the cancelled-call test must exercise
the actual cancellation path, not simulate its render side effect)."
  (require 'superchat)
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (ert-skip "SQLite unavailable"))
  (let* ((directory (make-temp-file "superchat-s21fix2-cancel-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         run-id captured)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local superchat-buffer-name (buffer-name)
                      superchat-mode t
                      superchat--session-id "s21fix2-cancel")
          (superchat-db-open)
          ;; No `:delivery-function'/`:stream-function'/`:sink': a live
          ;; `superchat-mode' buffer with no legacy callback routes to the
          ;; REAL registered View Sink (contract §2.3), so this Run's
          ;; eventual cancellation drives the same
          ;; `superchat--response-region-close' call production code takes,
          ;; not one this test invokes itself.
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _complete stream &rest _)
                       (setq captured
                             (list :stream stream
                                   :context (copy-sequence
                                             superchat--tool-execution-context))))))
            (setq run-id
                  (plist-get
                   (superchat-runtime-submit
                    (list :prompt "Q" :session-id "s21fix2-cancel"
                          :buffer (current-buffer) :tools 'none :deadline nil
                          :transcript-owner "S21FIX2-OWNER"
                          :approval-function (lambda (_request) t)))
                   :run-id)))
          (let* ((context (plist-get captured :context))
                 (wrapped
                  (superchat--agent-wrap-function
                   "read_buffer" (lambda (_done &rest _args) nil) ; never calls done: hangs
                   t nil (cons 0 nil) (current-buffer) context)))
            (funcall wrapped #'ignore "target")
            (should (string-match-p "^\\*\\*\\* ⏳ read_buffer$" (buffer-string)))
            ;; The wrapper minted this call's execution id itself (no
            ;; `:tool-call-id' in CONTEXT); read it back off the heading's
            ;; own property rather than predicting the generated value.
            (let ((execution-id
                   (get-text-property
                    (agent-loop-test--buffer-position-of "⏳ read_buffer")
                    'superchat-tool-call-id)))
              (should (stringp execution-id))
              (superchat-runtime-cancel run-id "S21-fix-2 test cancellation")
              (let ((text (buffer-string)))
                (should-not (string-match-p "⏳" text))
                (should (string-match-p "^\\*\\*\\* ✗ read_buffer (stopped)$" text)))
              (should-not
               (superchat--response-region-untrack-pending-tool-call
                "S21FIX2-OWNER" execution-id)))))
      (maphash (lambda (r _) (ignore-errors (superchat-runtime-cancel r "cleanup")))
               superchat-runtime--runs)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest agent-loop/uncompleted-call-becomes-stopped-on-reset ()
  "The same cleanup runs on a full response-region reset, isolated to the
owner whose call never resolved (S21-fix-1, Spec 2)."
  (require 'superchat)
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (insert "* User: hi\n")
      (superchat--response-region-prepare)
      (superchat--agent-render-tool-call
       "read_buffer" '("target") (current-buffer) nil nil "call-stuck")
      (should (string-match-p "^\\*\\*\\* ⏳ read_buffer$" (buffer-string)))
      (superchat--response-region-reset)
      (let ((text (buffer-string)))
        (should-not (string-match-p "⏳" text))
        (should (string-match-p "^\\*\\*\\* ✗ read_buffer (stopped)$" text)))
      (should-not (superchat--response-region-untrack-pending-tool-call nil "call-stuck")))))

(provide 'test-agent-loop)

;;; test-agent-loop.el ends here
