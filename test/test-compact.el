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
  (with-temp-buffer
    (let ((superchat--conversation-history
           '((:role user :content "Hello")
             (:role assistant :content "Hi")
             (:role user :content "What is Emacs?")))
          (superchat--session-id "test-compact-session")
          (superchat-compact-message-count 10)
          (superchat-buffer-name "*superchat-compact-test*")
          (anchors '())
          captured-request)
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (setq captured-request request)
                   (funcall (plist-get request :delivery-function)
                            "Summary of the conversation." 'completed nil)
                   '(:run-id "run-compact" :turn-id "turn-compact"
                     :session-id "test-compact-session" :status accepted)))
                ((symbol-function 'superchat-db-tape-append)
                 (lambda (_session-id kind content &rest _args)
                   (when (string= kind "anchor")
                     (push content anchors))
                   42)))
        (let ((handle (superchat-compact-session)))
          (should (equal "run-compact" (plist-get handle :run-id)))
          (should-not superchat-compact--in-flight)
          (should (equal (plist-get captured-request :capability)
                         "compaction"))
          (should (equal (plist-get captured-request :actor-id)
                         "compactor"))
          (should-not (plist-get captured-request :record-conversation))
          (should (= (length anchors) 1))
          (should (string= (car anchors) "Summary of the conversation."))
          (should (= (length superchat--conversation-history) 1))
          (should (plist-get (car superchat--conversation-history) :anchor))
          (should (string-match-p "Summary of the conversation"
                                  (plist-get (car superchat--conversation-history) :content))))))))

(ert-deftest test-compact-session-preserves-messages-added-in-flight ()
  "Messages recorded while compaction runs must survive in front of the anchor."
  (with-temp-buffer
    (let ((superchat--conversation-history
           '((:role assistant :content "old answer")
             (:role user :content "old question")))
          (superchat--session-id "test-compact-flight")
          (superchat-compact-message-count 10)
          (superchat-buffer-name "*superchat-compact-flight*")
          pending-delivery)
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (setq pending-delivery (plist-get request :delivery-function))
                   '(:run-id "run-flight" :status accepted)))
                ((symbol-function 'superchat-db-tape-append)
                 (lambda (&rest _args) 42)))
        (should (superchat-compact-session))
        (should superchat-compact--in-flight)
        ;; A second request while one is pending must refuse to start.
        (should-not (superchat-compact-session))
        ;; A new message arrives while the compaction Run is in flight.
        (push '(:role user :content "during-flight")
              superchat--conversation-history)
        (funcall pending-delivery "Summary." 'completed nil)
        (should-not superchat-compact--in-flight)
        (should (= 2 (length superchat--conversation-history)))
        (should (string= "during-flight"
                         (plist-get (car superchat--conversation-history)
                                    :content)))
        (should (plist-get (cadr superchat--conversation-history) :anchor))))))

(ert-deftest test-compact-session-failed-run-leaves-history-alone ()
  "A failed compaction Run must clear the pending flag and change nothing."
  (with-temp-buffer
    (let ((superchat--conversation-history
           '((:role user :content "Hello")))
          (superchat--session-id "test-compact-failed")
          (superchat-compact-message-count 10)
          (superchat-buffer-name "*superchat-compact-failed*")
          (anchors '()))
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (funcall (plist-get request :delivery-function)
                            "[API error]" 'failed nil)
                   '(:run-id "run-failed" :status accepted)))
                ((symbol-function 'superchat-db-tape-append)
                 (lambda (_session-id kind content &rest _args)
                   (when (string= kind "anchor") (push content anchors))
                   42)))
        (should (superchat-compact-session))
        (should-not superchat-compact--in-flight)
        (should (null anchors))
        (should (= 1 (length superchat--conversation-history)))
        (should-not (plist-get (car superchat--conversation-history)
                               :anchor))))))

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
  (with-temp-buffer
    (let ((superchat--session-id "test-expand-session")
          (superchat--conversation-history
           '((:role user :content "Hello" :id 1)
             (:role assistant :content "Hi" :id 2)
             (:role user :content "What is Emacs?" :id 3)))
          (superchat-buffer-name "*superchat-expand-test*")
          (anchor-content nil))
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (funcall (plist-get request :delivery-function)
                            "Summary of Emacs discussion." 'completed nil)
                   '(:run-id "run-expand" :status accepted)))
                ((symbol-function 'superchat-db-tape-append)
                 (lambda (_session-id kind content &rest _args)
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
                           (plist-get (car superchat--conversation-history)
                                      :content))))))))

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

(ert-deftest test-auto-handoff-skips-while-compaction-pending ()
  "`superchat-handoff--maybe-auto-compact' must not stack compaction Runs."
  (with-temp-buffer
    (let ((superchat-handoff-auto-enabled t)
          (superchat-handoff-char-threshold 10)
          (superchat--session-id "handoff-test")
          (superchat-compact--in-flight t)
          (compact-called nil)
          (turn (superchat-turn-new "short")))
      (setf (superchat-turn-prompt turn)
            "this prompt is definitely longer than ten characters")
      (cl-letf (((symbol-function 'superchat-compact-session)
                 (lambda () (setq compact-called t) "summary")))
        (let ((result (superchat-handoff--maybe-auto-compact turn)))
          (should-not compact-called)
          (should (superchat-turn-p result)))))))

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
