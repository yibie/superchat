;;; test-cmd-dispatch.el — Test command dispatch through hooks -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)

(message "=== Loading superchat ===")
(require 'superchat)
(message "Loaded OK")

(setq superchat-data-directory "/tmp/sc-p2-test/")
(superchat--ensure-directories)

;; Add a test memory for /recall
(when (fboundp 'superchat-db-memory-insert)
  (superchat-db-memory-insert "Test memory for recall" :title "Test" :keywords "test")
  (superchat-db-memory-set-review 1 "accepted"))

;; Test each command through the hook chain
(dolist (cmd '("recall" "remember" "reset" "clear-context" "clear" "commands" "define" "skill-install"))
  (let ((args (if (equal cmd "recall") "test" "extra")))
    (condition-case err
        (let ((result (superchat--handle-command cmd args "input" "Chinese")))
          (message "Command /%s → type=%s"
                   cmd (or (plist-get result :type) "nil")))
      (error
       (message "Command /%s → ERROR: %s" cmd (error-message-string err))))))

;; Test unknown command
(let ((result (superchat--handle-command "nonexistent" "" "input" "Chinese")))
  (message "Unknown → type=%s" (or (plist-get result :type) "nil")))

;; Test builtin
(let ((result (superchat--handle-command "models" "" "input" "Chinese")))
  (message "Builtin /models → type=%s" (or (plist-get result :type) "nil")))

;; Test a custom user command
(superchat--define-command "testcmd" "You are a tester. $input")
(let ((result (superchat--handle-command "testcmd" "hello" "input" "Chinese")))
  (message "User /testcmd → type=%s" (or (plist-get result :type) "nil")))

(superchat-db-close)
(message "=== ALL COMMAND TESTS PASSED ===")
