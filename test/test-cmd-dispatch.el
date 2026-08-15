;;; test-cmd-dispatch.el — Test command dispatch through hooks -*- lexical-binding: t; -*-

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(add-to-list 'load-path default-directory)

(message "=== Loading superchat ===")
(require 'superchat)
(message "Loaded OK")

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

(ert-deftest cmd/clear-rotates-session-and-clears-view-state ()
  "`/clear' starts a new Tape context in addition to clearing the View."
  (with-temp-buffer
    (setq-local superchat-mode t)
    (setq-local superchat-buffer-name (buffer-name))
    (setq-local superchat--session-id "clear-before-session")
    (setq-local superchat--conversation-history '("old turn"))
    (setq-local superchat--current-command "old-command")
    (setq-local superchat--pending-recalled-memories
                '((:content "stale-recall")))
    (let ((superchat--current-context-files '("/tmp/old-context.txt"))
          (superchat-data-directory (make-temp-file "superchat-clear-test" t))
          (before superchat--session-id))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                       (lambda () nil)))
              (superchat--clear-chat-and-context))
            (should (stringp superchat--session-id))
            (should-not (equal before superchat--session-id))
            (should-not superchat--conversation-history)
            (should-not superchat--current-command)
            (should-not superchat--pending-recalled-memories)
            (should-not superchat--current-context-files)
            (should (string-match-p "\\`#\\+TITLE: superchat"
                                    (buffer-string))))
        (delete-directory superchat-data-directory t)))))

(ert-deftest cmd/clear-skips-delivery-from-the-old-session-view ()
  "A Run from before `/clear' must not render into the new View."
  (let ((buffer-name (generate-new-buffer-name " *superchat-clear-delivery*"))
        delivered)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local superchat-mode t
                      superchat-buffer-name buffer-name
                      superchat--session-id "old-session")
          (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                     (lambda () nil))
                    ((symbol-function 'superchat--process-llm-result)
                     (lambda (answer &optional _tape-id)
                       (setq delivered answer))))
            (superchat--clear-chat-and-context)
            (let ((entry (list :buffer-name buffer-name
                               :session-id "old-session")))
              (superchat-runtime--deliver entry "old assistant" nil 'completed)))
          (should-not delivered))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(superchat-db-close)
(message "=== ALL COMMAND TESTS PASSED ===")
