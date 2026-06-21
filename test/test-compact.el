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

(ert-deftest test-expand-anchor-restores-source-messages ()
  "`/expand' should restore the source entries behind the latest anchor."
  (let ((superchat--session-id "test-expand-session")
        (superchat--conversation-history
         '((:role user :content "Hello" :id 1)
           (:role assistant :content "Hi" :id 2)
           (:role user :content "What is Emacs?" :id 3)))
        (superchat-buffer-name "*superchat-expand-test*")
        (anchor-content nil))
    (unwind-protect
        (progn
          (get-buffer-create superchat-buffer-name)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer-sync)
                     (lambda (_prompt &optional _target-model _tools _agent-mode)
                       "Summary of Emacs discussion."))
                    ((symbol-function 'superchat-db-tape-append)
                     (lambda (session-id kind content &rest args)
                       (when (string= kind "anchor")
                         (setq anchor-content content))
                       42))
                    ((symbol-function 'superchat-view-anchor-latest)
                     (lambda (_session-id)
                       `(42 nil ,anchor-content
                            ((source_ids . [1 2 3]))
                            nil)))
                    ((symbol-function 'superchat-view-expand-anchor)
                     (lambda (_session-id _anchor)
                       '((1 nil "user" "Hello" "{}" "t0")
                         (2 nil "assistant" "Hi" "{}" "t1")
                         (3 nil "user" "What is Emacs?" "{}" "t2")))))
            (superchat-compact-session)
            (should (= (length superchat--conversation-history) 1))
            (should (plist-get (car superchat--conversation-history) :anchor))
            (let ((count (superchat-compact--expand-anchor)))
              (should (= count 3))
              (should (= (length superchat--conversation-history) 3))
              ;; History is newest-first; the last source message is at car.
              (should (string= "What is Emacs?"
                               (plist-get (car superchat--conversation-history) :content))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-auto-handoff-compacts-long-prompt ()
  "`superchat-handoff--maybe-auto-compact' compacts when prompt is long."
  (let ((superchat-handoff-auto-enabled t)
        (superchat-handoff-char-threshold 10)
        (superchat--session-id "handoff-test")
        (compact-called nil)
        (turn (superchat-turn-new "short")))
    (setf (superchat-turn-prompt turn) "this prompt is definitely longer than ten characters")
    (cl-letf (((symbol-function 'superchat-compact-session)
               (lambda () (setq compact-called t) "summary")))
      (let ((result (superchat-handoff--maybe-auto-compact turn)))
        (should compact-called)
        (should (superchat-turn-p result))))))

(ert-deftest test-auto-handoff-skips-short-prompt ()
  "`superchat-handoff--maybe-auto-compact' does nothing when prompt is short."
  (let ((superchat-handoff-auto-enabled t)
        (superchat-handoff-char-threshold 1000)
        (superchat--session-id "handoff-test")
        (compact-called nil)
        (turn (superchat-turn-new "short")))
    (setf (superchat-turn-prompt turn) "hi")
    (cl-letf (((symbol-function 'superchat-compact-session)
               (lambda () (setq compact-called t) "summary")))
      (let ((result (superchat-handoff--maybe-auto-compact turn)))
        (should (not compact-called))
        (should (superchat-turn-p result))))))

(provide 'test-compact)

;;; test-compact.el ends here
