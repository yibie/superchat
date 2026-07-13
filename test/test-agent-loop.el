;;; test-agent-loop.el --- Tests for superchat-agent-loop -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the agent-mode tool wrapper, safety guardrails, and
;; per-tool lifecycle hooks.

;;; Code:

(require 'ert)
(require 'llm nil t)
(require 'superchat-preset)
(require 'superchat-agent-loop)

(defmacro superchat-test--mock-render (&rest body)
  "Execute BODY with agent render/log functions mocked out."
  `(cl-letf (((symbol-function 'superchat--agent-render-tool-call) #'ignore)
             ((symbol-function 'superchat--agent-render-tool-result) #'ignore)
             ((symbol-function 'superchat--agent-log-tool-call) #'ignore)
             ((symbol-function 'superchat--agent-log-tool-result) #'ignore))
     ,@body))

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
    (cl-letf (((symbol-function 'superchat--agent-ask-confirm)
               (lambda (_name _args) nil)))
      (superchat-test--mock-render
       (should (string-match-p "cancelled" (funcall wrapped "file" "body")))))
    (should-not called)))

(ert-deftest test-agent-eval-elisp-is-destructive-by-default ()
  "Arbitrary Elisp evaluation must pass the destructive confirmation gate."
  (should (member "eval-elisp" (default-value
                                 'superchat-agent-destructive-tools)))
  (let ((superchat-agent-confirm-destructive t))
    (should (superchat--agent-confirm-p "eval-elisp" '((expression . "(+ 1 1)"))))))

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
         (should (string-match-p "denied by permission hook" result)))))))

(ert-deftest test-agent-hooks-permission-allow-skips-confirm ()
  "Permission hook returning 'allow skips user confirmation."
  (let ((superchat-agent-permission-functions
         (list (lambda (_name _args) 'allow)))
        (superchat-agent-confirm-destructive t)
        (superchat-agent-destructive-tools '("test-tool"))
        (confirmed nil))
    (cl-letf (((symbol-function 'superchat--agent-ask-confirm)
               (lambda (_name _args) (setq confirmed t) t)))
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
  "Post-tool-failure hook fires on error, then re-signals."
  (let ((failure-called nil))
    (let ((superchat-agent-post-tool-failure-functions
           (list (lambda (name args err)
                   (setq failure-called (list name (car args) (car err)))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (_x) (error "boom")))
              (wrapped (superchat--agent-wrap-function "test-tool" fn nil)))
         (condition-case nil
             (funcall wrapped "hello")
           (error nil))
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
        (superchat--agent-tool-call-count 0)
        (superchat-agent-max-tool-calls 10)
        (superchat-agent-confirm-destructive nil))
    (let ((superchat-agent-post-tool-failure-functions
           (list (lambda (name args err)
                   (setq failure-called (list name (car args) (car err)))))))
      (superchat-test--mock-render
       (let* ((fn (lambda (_callback &rest _args) (error "async-boom")))
              (wrapped (superchat--agent-wrap-function "async-tool" fn t)))
         (condition-case nil
             (funcall wrapped 'ignore "hello")
           (error nil))
         (should (equal failure-called (list "async-tool" "hello" 'error))))))))

(provide 'test-agent-loop)

;;; test-agent-loop.el ends here
