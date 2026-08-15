;;; test-core-pipeline.el — ERT tests for superchat-core pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for superchat-core-run-turn with hooks.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
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

(ert-deftest pipeline/file-reference-inlines-source-and-reports-actual-status ()
  "A #file source ref contributes bytes and binary refs do not lie."
  (let ((token "PIPELINE_FILE_PROBE_TOKEN")
        (superchat-inline-file-content t)
        (superchat-inline-metadata-threshold nil)
        (superchat--current-context-files nil)
        (superchat--context-file-ranges nil)
        (superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions
         (list #'superchat-prompt-hook--file-inline
               #'superchat-prompt-hook--template-substitution)))
    (dolist (suffix '(".el" ".py" ".md" ".rst" ".tex" ".csv" ".diff"
                      ".patch" ".vue" ".proto" nil ".png"))
      (let ((file (make-temp-file "superchat-file-probe" nil suffix))
            messages)
        (unwind-protect
            (progn
              (with-temp-file file
                (insert token))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args)
                                 messages))))
                (let* ((turn (superchat-core-run-turn
                              (superchat-turn-new
                               (format "#%s What is the token?" file)
                               "pipeline-file-test")))
                       (prompt (superchat-turn-prompt turn)))
                  (if (string= suffix ".png")
                      (progn
                        (should-not (string-match-p
                                     (regexp-quote token) prompt))
                        (should (cl-some
                                 (lambda (message)
                                   (string-match-p "[Ii]mage.*attach" message))
                                 messages))
                        (should-not
                         (cl-some
                          (lambda (message)
                            (string-match-p "will be included in context"
                                            message))
                          messages)))
                    (should (string-match-p (regexp-quote token) prompt))
                    (should (cl-some
                             (lambda (message)
                               (string-match-p "Inlined" message))
                             messages)))))))
          (when (file-exists-p file)
            (delete-file file))))))

(ert-deftest pipeline/file-reference-metadata-status-is-honest ()
  "Large #file refs disclose metadata without claiming inline content."
  (let ((file (make-temp-file "superchat-large-file" nil ".el"))
        (token "METADATA_ONLY_FILE_TOKEN")
        messages)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (make-string 700 ?x))
            (insert token))
          (let ((superchat-inline-file-content t)
                (superchat-inline-metadata-threshold 32)
                (superchat--current-context-files nil)
                (superchat--context-file-ranges nil)
                (superchat-system-prompt-functions nil)
                (superchat-build-prompt-functions
                 (list #'superchat-prompt-hook--file-inline
                       #'superchat-prompt-hook--template-substitution)))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args)
                               messages))))
              (let* ((turn (superchat-core-run-turn
                            (superchat-turn-new
                             (format "#%s What is the token?" file)
                             "pipeline-metadata-test")))
                     (prompt (superchat-turn-prompt turn)))
                (should-not (string-match-p (regexp-quote token) prompt))
                (should (string-match-p "NOT inlined" prompt))
                (should (cl-some
                         (lambda (message)
                           (string-match-p "metadata.*not inlined" message))
                         messages))
                (should-not
                 (cl-some
                  (lambda (message)
                    (string-match-p "will be included in context" message))
                  messages))))))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'test-core-pipeline)
;;; test-core-pipeline.el ends here
