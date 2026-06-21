;;; test-compact.el --- Tests for superchat-compact -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for session compaction and anchor-based prompt assembly.

;;; Code:

(require 'ert)
(require 'superchat-compact)

(ert-deftest test-compact-collect-messages ()
  "Collect recent conversation messages for compaction."
  ;; `superchat--conversation-history' is newest-first (head = latest).
  (let ((superchat--conversation-history
         '((:role user :content "What is 2+2?")
           (:role assistant :content "Hi there")
           (:role user :content "Hello")))
        (superchat-compact-message-count 2))
    (let ((messages (superchat-compact--collect-messages)))
      (should (= (length messages) 2))
      ;; Compaction returns the N most recent messages in chronological
      ;; order (oldest of the recent first).
      (should (string= (plist-get (car messages) :content) "Hi there"))
      (should (string= (plist-get (cadr messages) :content) "What is 2+2?")))))

(ert-deftest test-compact-build-prompt-includes-messages ()
  "Compaction prompt should include the conversation text."
  (let ((messages '((:role user :content "Hello")))
        (superchat-compact-prompt "SUMMARY PROMPT"))
    (let ((prompt (superchat-compact--build-compaction-prompt messages)))
      (should (string-match-p "SUMMARY PROMPT" prompt))
      (should (string-match-p "User: Hello" prompt)))))

(ert-deftest test-compact-session-writes-anchor-and-resets-history ()
  "`superchat-compact-session' should summarize, write anchor, and reset history."
  (let ((superchat--conversation-history
         '((:role user :content "Hello")
           (:role assistant :content "Hi")
           (:role user :content "What is Emacs?")))
        (superchat--session-id "test-compact-session")
        (superchat-compact-message-count 10)
        (superchat-buffer-name "*superchat-compact-test*")
        (anchors '()))
    (unwind-protect
        (progn
          (get-buffer-create superchat-buffer-name)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer-sync)
                     (lambda (_prompt &optional _target-model _tools _agent-mode)
                       "Summary of the conversation."))
                    ((symbol-function 'superchat-db-tape-append)
                     (lambda (_session-id kind content &rest _args)
                       (when (string= kind "anchor")
                         (push content anchors))
                       42)))
            (let ((result (superchat-compact-session)))
              (should (string= result "Summary of the conversation."))
              (should (= (length anchors) 1))
              (should (string= (car anchors) "Summary of the conversation."))
              (should (= (length superchat--conversation-history) 1))
              (should (plist-get (car superchat--conversation-history) :anchor))
              (should (string-match-p "Summary of the conversation"
                                      (plist-get (car superchat--conversation-history) :content))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-prompt-hook-includes-anchor ()
  "`superchat-prompt-hook--conversation-history' should prepend an anchor."
  (require 'superchat-prompt-hooks)
  (let ((superchat--session-id "test-anchor-session")
        (superchat--conversation-history nil)
        (anchor-called nil))
    (cl-letf (((symbol-function 'superchat-compact--anchor-summary)
               (lambda () "ANCHOR SUMMARY")))
      (let ((turn (superchat-turn-new "test")))
        (superchat-prompt-hook--conversation-history turn)
        (should (string-match-p "Session context (anchor):" (superchat-turn-prompt turn)))
        (should (string-match-p "ANCHOR SUMMARY" (superchat-turn-prompt turn)))))))

(provide 'test-compact)

;;; test-compact.el ends here
