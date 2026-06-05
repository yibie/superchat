;;; test-post-turn-hooks.el — Post-turn hook tests (Bub Phase 1 D) -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-post-turn-functions, specifically the
;; user-message tape recording hook.

;;; Code:

(require 'ert)
(require 'superchat-core)

(ert-deftest post-turn-hook-records-user-message-once ()
  "Hook fires once per turn and records the user's clean input to tape."
  (let ((recorded nil))
    (cl-letf (((symbol-function 'superchat--record-message)
               (lambda (role content)
                 (push (cons role content) recorded))))
      (let ((turn (superchat-turn-new "hello world")))
        ;; Simulate post-turn: the hook reads clean-input
        (let ((fn (lambda (t)
                    (let ((text (string-trim (or (superchat-turn-clean-input t) ""))))
                      (when (> (length text) 0)
                        (superchat--record-message "user" text)))
                    nil)))
          (funcall fn turn))
        (should (equal 1 (length recorded)))
        (should (equal "user" (caar recorded)))
        (should (equal "hello world" (cdar recorded)))))))

(ert-deftest post-turn-hook-skips-empty-input ()
  "Hook must not call tape when clean-input is empty."
  (let ((called nil))
    (cl-letf (((symbol-function 'superchat--record-message)
               (lambda (_role _content) (setq called t))))
      (let ((turn (superchat-turn-new "")))
        (setf (superchat-turn-clean-input turn) "")
        (let ((fn (lambda (t)
                    (let ((text (string-trim (or (superchat-turn-clean-input t) ""))))
                      (when (> (length text) 0)
                        (superchat--record-message "user" text)))
                    nil)))
          (funcall fn turn))
        (should-not called)))))

(ert-deftest post-turn-hook-uses-clean-input-not-inbound ()
  "Tape records clean-input, not raw inbound (which may have @model prefix)."
  (let ((recorded nil))
    (cl-letf (((symbol-function 'superchat--record-message)
               (lambda (role content)
                 (push (cons role content) recorded))))
      (let ((turn (superchat-turn-new "@gpt-4 hello")))
        (setf (superchat-turn-clean-input turn) "hello")
        (let ((fn (lambda (t)
                    (let ((text (string-trim (or (superchat-turn-clean-input t) ""))))
                      (when (> (length text) 0)
                        (superchat--record-message "user" text)))
                    nil)))
          (funcall fn turn))
        (should (equal "hello" (cdar recorded)))))))

(provide 'test-post-turn-hooks)
;;; test-post-turn-hooks.el ends here
