;;; test-core-pipeline.el — ERT tests for superchat-core pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for superchat-core-run-turn with hooks.

;;; Code:

(require 'ert)
(require 'superchat-core)
(require 'superchat-prompt-hooks)
(require 'superchat nil t)          ; for defcustom defaults
(require 'superchat-parser nil t)   ; for parser functions

;; ═══════════════════════════════════════════════════════════
;; Pipeline basics
;; ═══════════════════════════════════════════════════════════

(ert-deftest pipeline/empty-input-no-crash ()
  "core-run-turn with empty input returns a valid turn."
  (let ((turn (superchat-turn-new "")))
    (let ((result (superchat-core-run-turn turn)))
      (should (superchat-turn-p result))
      (should (equal (superchat-turn-inbound result) "")))))

(ert-deftest pipeline/parses-model-switch ()
  "@gpt-4 hello → target-model = \"gpt-4\"."
  (let ((turn (superchat-turn-new "@gpt-4 hello")))
    (let ((result (superchat-core-run-turn turn)))
      (should (equal (superchat-turn-target-model result) "gpt-4"))
      ;; clean-input should have the @model prefix stripped
      (should (string-match-p "hello" (superchat-turn-clean-input result))))))

(ert-deftest pipeline/all-hooks-chain ()
  "With all 5 hooks registered, turn.prompt contains contributions."
  (let* ((superchat-lang "Chinese")
         (turn (superchat-turn-new "explain Lisp macros"))
         (result (superchat-core-run-turn turn)))
    (should (superchat-turn-p result))
    ;; system-prompt should have language instruction
    (should (string-match-p "Chinese"
                            (superchat-turn-system-prompt result)))
    ;; prompt should contain the user query (from template-substitution)
    (should (string-match-p "Lisp macros"
                            (superchat-turn-prompt result)))))

(ert-deftest pipeline/hook-erroring-is-isolated ()
  "A hook that signals an error does not abort the chain."
  (let ((errored nil))
    (unwind-protect
        (progn
          ;; Register a broken hook
          (add-hook 'superchat-build-prompt-functions
                    (lambda (_turn)
                      (setq errored t)
                      (error "boom"))
                    90) ; append near end
          (let* ((turn (superchat-turn-new "hello"))
                 (result (superchat-core-run-turn turn)))
            (should (superchat-turn-p result))
            (should errored) ; it was invoked
            ;; prompt should still have template-substitution output
            (should (string-match-p "hello"
                                    (superchat-turn-prompt result)))))
      ;; Cleanup: remove the broken hook
      (remove-hook 'superchat-build-prompt-functions
                   (lambda (_turn)
                     (setq errored t)
                     (error "boom"))))))

(provide 'test-core-pipeline)
;;; test-core-pipeline.el ends here
