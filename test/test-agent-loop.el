;;; test-agent-loop.el --- Tests for superchat-agent-loop -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the agent-mode tool wrapper and safety guardrails.

;;; Code:

(require 'ert)
(require 'llm nil t)
(require 'superchat-preset)
(require 'superchat-agent-loop)

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
    (should (functionp wrapped))
    (let ((result (apply wrapped '("arg1" "arg2"))))
      (should (string= result "mock-result"))
      (should (equal original-called '("arg1" "arg2")))
      (should (= superchat--agent-tool-call-count 1)))))

(ert-deftest test-agent-wrap-respects-max-tool-calls ()
  "A wrapped tool should stop after `superchat-agent-max-tool-calls'."
  (let ((superchat--agent-tool-call-count 0)
        (superchat-agent-max-tool-calls 2)
        (superchat-agent-confirm-destructive nil)
        (wrapped (superchat--agent-wrap-function
                  "mock-tool"
                  (lambda (&rest _args) "ok")
                  nil)))
    (apply wrapped '("a"))
    (apply wrapped '("b"))
    (let ((result (apply wrapped '("c"))))
      (should (string-match-p "exceeded maximum" result))
      (should (= superchat--agent-tool-call-count 2)))))

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
    (funcall wrapped (lambda (result) (setq callback-result result)) "arg")
    (should (string= callback-result "result:arg"))
    (should (= superchat--agent-tool-call-count 1))))

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

(provide 'test-agent-loop)

;;; test-agent-loop.el ends here
