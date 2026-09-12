;;; test-core-pipeline.el — ERT tests for superchat-core pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for superchat-core-prepare-turn with hooks.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-core)
(require 'superchat-prompt-hooks)
(require 'superchat-skills)
(require 'superchat-db)
(require 'superchat nil t)          ; for defcustom defaults
(require 'superchat-parser nil t)   ; for parser functions

;; ═══════════════════════════════════════════════════════════
;; Pipeline basics
;; ═══════════════════════════════════════════════════════════

(ert-deftest pipeline/empty-input-no-crash ()
  "core-run-turn with empty input returns a valid turn."
  (let ((turn (superchat-turn-new "")))
    (let ((result (superchat-core-prepare-turn turn)))
      (should (superchat-turn-p result))
      (should (equal (superchat-turn-inbound result) "")))))

(ert-deftest pipeline/parses-model-switch ()
  "@gpt-4 hello → target-model = \"gpt-4\"."
  (let ((turn (superchat-turn-new "@gpt-4 hello")))
    (let ((result (superchat-core-prepare-turn turn)))
      (should (equal (superchat-turn-target-model result) "gpt-4"))
      ;; clean-input should have the @model prefix stripped
      (should (string-match-p "hello" (superchat-turn-clean-input result))))))

(ert-deftest pipeline/body-lookalikes-do-not-route-model-or-file ()
  "Pasted source @/# lookalikes after prose remain ordinary body text."
  (let* ((input (concat "Review this pasted source:\n"
                        "const email = user@outlook.com;\n"
                        "const path = \"#/etc/hosts\";"))
         (result (superchat-core-prepare-turn (superchat-turn-new input))))
    (should-not (superchat-turn-target-model result))
    (should-not (superchat-turn-context-files result))
    (should (string-match-p "user@outlook\\.com"
                            (superchat-turn-clean-input result)))
    (should (string-match-p "#/etc/hosts"
                            (superchat-turn-clean-input result)))))

(ert-deftest pipeline/all-hooks-chain ()
  "With all 5 hooks registered, turn.prompt contains contributions."
  (let* ((superchat-lang "Chinese")
         (turn (superchat-turn-new "explain Lisp macros"))
         (result (superchat-core-prepare-turn turn)))
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
                 (result (superchat-core-prepare-turn turn)))
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
        (superchat--context-file-ranges nil))
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
                (let* ((turn (superchat-core-prepare-turn
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
                             messages))))))
          (when (file-exists-p file)
            (delete-file file)))))))

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
                (superchat--context-file-ranges nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args)
                               messages))))
              (let* ((turn (superchat-core-prepare-turn
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

(ert-deftest pipeline/file-reference-without-range-clears-stale-range ()
  "A plain #file after a ranged attachment removes only that file's range."
  (let* ((file (make-temp-file "superchat-stale-range" nil ".el"))
         (other (expand-file-name "other.el" temporary-file-directory))
         (superchat--context-file-ranges
          (list (cons file '(2 . 4))
                (cons other '(5 . 6)))))
    (unwind-protect
        (progn
          (should (equal file
                         (superchat--extract-file-path (format "#%s" file))))
          (should-not (assoc file superchat--context-file-ranges))
          (should (equal '(5 . 6)
                         (cdr (assoc other
                                     superchat--context-file-ranges)))))
      (delete-file file))))

(ert-deftest pipeline/skill-precedes-command ()
  "A `>skill' Turn does not also parse its remainder as a command."
  (let ((turn (superchat-core-prepare-turn
               (superchat-turn-new ">planning /reset hello"
                                    "pipeline-skill-command"))))
    (should (equal "planning" (superchat-turn-skill turn)))
    (should-not (superchat-turn-command turn))
    (should (equal "/reset hello"
                   (superchat-turn-clean-input turn)))))

(ert-deftest pipeline/plain-command-still-parses-without-skill ()
  "A plain slash command still populates command and its arguments."
  (let ((turn (superchat-core-prepare-turn
               (superchat-turn-new "/reset hello"
                                    "pipeline-plain-command"))))
    (should-not (superchat-turn-skill turn))
    (should (equal "reset" (superchat-turn-command turn)))
    (should (equal "hello" (superchat-turn-command-args turn)))
    (should (equal "/reset hello"
                   (superchat-turn-clean-input turn)))))

;; ═══════════════════════════════════════════════════════════
;; S06b — collect-context / assemble-prompt cutover
;; ═══════════════════════════════════════════════════════════

(ert-deftest core-pipeline/prepare-turn-uses-collect-and-assemble ()
  "No hook list is consulted directly any more: with both retired,
built-in collectors alone still build a prompt through `collect-context'
and `assemble-prompt'."
  (let ((superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions nil))
    (let* ((turn (superchat-turn-new "explain lisp macros"))
           (result (superchat-core-prepare-turn turn)))
      (should (superchat-turn-p result))
      (should-not (superchat-turn-error result))
      (should (string-match-p "lisp macros"
                              (downcase (superchat-turn-prompt result)))))))

;; ═══════════════════════════════════════════════════════════
;; Golden before/after — three scenarios, captured against the retired
;; hook-chain code (S06b-fix-1 item 3)
;; ═══════════════════════════════════════════════════════════

(defmacro core-pipeline-golden--with-temp-db (body)
  "Run BODY with a fresh temporary Superchat database.

Also isolates `superchat--current-context-files'/`superchat--context-file-ranges':
`superchat-core-prepare-turn' on a `#file' turn calls
`superchat--add-file-to-context' through the `file-inline' collector, same
as production, and outside a real chat View (`superchat-mode' is not
active in a plain ERT test buffer) that write is NOT buffer-local — it
lands on `superchat--current-context-files''s *global default*, which
nothing in this file's own tests would ever see or clean up, but a
journey test running later in the same batch process (`journey-fixture-with'
snapshots that global default before its body and asserts it is
unchanged after) will.  `let'-binding both here, once, covers every
golden test that goes through this macro instead of repeating it in each
one; existing tests such as `pipeline/file-reference-inlines-source-and-reports-actual-status'
bind the same pair for the same reason."
  (declare (indent 0))
  `(let* ((tmp-dir (make-temp-file "superchat-golden-test-" t))
          (db-file (expand-file-name "superchat.db" tmp-dir))
          (superchat-data-directory tmp-dir)
          (superchat-db--connection nil)
          (superchat-db--path nil)
          (superchat--current-context-files nil)
          (superchat--context-file-ranges nil))
     (unwind-protect
         (progn
           (superchat-db-open db-file)
           ,body)
       (superchat-db-close)
       (delete-directory tmp-dir t))))

(ert-deftest core-pipeline/golden-plain-question-matches-pre-s06b ()
  "A plain question assembles byte-for-byte the same prompt the retired
`superchat-system-prompt-functions'/`superchat-build-prompt-functions'
hook chain produced (captured by running the pre-S06b code directly:
`jj file show -r @ --at-op 036943a9066d --ignore-working-copy
superchat-core.el`/`superchat-prompt-hooks.el`/`superchat-dispatcher.el',
same bindings, same fresh empty session).  No intended difference here."
  (core-pipeline-golden--with-temp-db
   (let ((superchat-lang "English")
         (superchat-llm-tools-enabled nil)
         (superchat--active-preset nil)
         (superchat--active-plan nil)
         (superchat-llm-tool-names nil))
     (let* ((turn (superchat-turn-new "What is a monad?" "golden-plain"))
            (result (superchat-core-prepare-turn turn)))
       (should (equal "" (superchat-turn-system-prompt result)))
       (should
        (equal
         "You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: What is a monad?"
         (superchat-turn-prompt result)))))))

(ert-deftest core-pipeline/golden-file-question-drops-the-ref-token ()
  "A `#file' question assembles the same attachment block as pre-S06b,
but the trailing template text no longer embeds the raw `#path' token —
that is this slice's own goal (`docs/design/turn-pipeline-contract.md'
§4.3's `file-inline' collector, and S06b's stated goal: \"the `#path'
token never reaches the model\").  Everything else about the captured
pre-S06b string is unchanged, including the exact attachment block
wording/spacing."
  (core-pipeline-golden--with-temp-db
   (let ((superchat-lang "English")
         (superchat-llm-tools-enabled nil)
         (superchat--active-preset nil)
         (superchat--active-plan nil)
         (superchat-llm-tool-names nil)
         (path (make-temp-file "golden-file" nil ".txt")))
     (unwind-protect
         (progn
           (with-temp-file path (insert "GOLDEN_FILE_CONTENT_1\nsecond line\n"))
           (let* ((turn (superchat-turn-new
                         (format "#%s summarize it" path) "golden-file"))
                  (result (superchat-core-prepare-turn turn))
                  (expected-attachment-block
                   (format
                    (concat "You are provided with the content of a local file for context.\n"
                            "Use this context to answer the user's question.\n"
                            "File: %s\n"
                            "----- BEGIN FILE CONTENT -----\n"
                            "GOLDEN_FILE_CONTENT_1\n"
                            "second line\n"
                            "----- END FILE CONTENT -----\n")
                    path))
                  (expected-template-tail
                   "You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: summarize it"))
             (should (equal "" (superchat-turn-system-prompt result)))
             ;; Pre-S06b captured value for comparison (D3, now fixed):
             ;; "...the user's question: #<path> summarize it" — the ref
             ;; token appeared in the prompt sent to the model.  Assert its
             ;; absence directly, then the full expected string.
             (should-not (string-match-p (regexp-quote (format "#%s" path))
                                         (superchat-turn-prompt result)))
             (should
              (equal
               (concat expected-attachment-block "\n\n" expected-template-tail)
               (superchat-turn-prompt result)))))
       (delete-file path)))))

(ert-deftest core-pipeline/golden-command-template-matches-pre-s06b ()
  "A `/command' template path (`superchat--execute-llm-query' called with
an explicit TEMPLATE, as the dispatcher does for a slash command) still
assembles byte-for-byte the same prompt as pre-S06b."
  (core-pipeline-golden--with-temp-db
   (let ((superchat-lang "English")
         (superchat-llm-tools-enabled nil)
         (superchat--active-preset nil)
         (superchat--active-plan nil)
         (superchat-llm-tool-names nil))
     (let* ((turn (superchat-turn-new "renewable energy" "golden-template"))
            (prepared (superchat-core-prepare-turn turn))
            (plan (superchat--execute-llm-query
                   prepared "Summarize in one paragraph: $input" nil)))
       (should (equal "" (superchat-turn-system-prompt prepared)))
       (should (equal "Summarize in one paragraph: renewable energy"
                      (plist-get plan :prompt)))))))

(provide 'test-core-pipeline)
;;; test-core-pipeline.el ends here
