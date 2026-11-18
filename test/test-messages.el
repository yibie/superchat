;;; test-messages.el --- tests for superchat-messages -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-chat)
(require 'superchat-messages)

(ert-deftest superchat-messages-lists-what-was-sent-and-opens-it ()
  (superchat-test-with-tape
    (let ((default-directory (file-name-as-directory (make-temp-file "sc-messages-" t))))
      (unwind-protect
          (save-window-excursion
            (superchat-messages)
            (should (equal (with-current-buffer "*superchat: messages*" (buffer-string))
                           "Nothing was sent.\n"))
            (superchat-tape-append "a" 'user "go" :scope "home")
            (superchat-tape-append "a" 'event "Build broke\nlog" :data '(:type "sent"))
            (superchat-tape-append "a" 'event "Cancelled." :data '(:type "cancelled"))
            (superchat-tape-append "b" 'user "go" :scope "home")
            (superchat-tape-append "b" 'event "Brief ready" :data '(:type "sent"))
            (superchat-messages)
            (with-current-buffer "*superchat: messages*"
              (should (string-match-p "\\`[-0-9]+ [0-9:]+  a  Build broke\n[-0-9]+ [0-9:]+  b  Brief ready\n\\'"
                                      (buffer-string)))
              ;; Point is on the newest; RET opens its conversation.
              (superchat-messages-open)
              (should (equal (buffer-local-value 'superchat-chat--tape (current-buffer)) "b")))
            ;; A question shows while it waits, and not once answered.
            (superchat-tape-append "c" 'user "go" :scope "home")
            (superchat-tape-append "c" 'event "shell (:command \"ls\")"
                                   :data '(:type "question" :call-id "q1"))
            (superchat-messages)
            (should (string-match-p "  c  shell (:command \"ls\")  \\? waits for y, n or a\n\\'"
                                    (with-current-buffer "*superchat: messages*" (buffer-string))))
            (superchat-tape-append "c" 'event "yes" :data '(:type "answer" :call-id "q1"))
            (superchat-messages)
            (should-not (string-search "waits for"
                                       (with-current-buffer "*superchat: messages*" (buffer-string)))))
        (dolist (b (buffer-list))
          (when (or (buffer-local-value 'superchat-chat--tape b)
                    (equal (buffer-name b) "*superchat: messages*"))
            (kill-buffer b)))
        (delete-directory default-directory t)))))

(provide 'test-messages)
;;; test-messages.el ends here
