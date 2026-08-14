;;; test-prompt-hooks.el — ERT tests for superchat-prompt-hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests each prompt hook function in isolation.  No buffer, no LLM, no async.

;;; Code:

(require 'ert)
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-prompt-hooks)
;; Load superchat.el to get defcustom default values
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════

(defmacro test-prompt-hooks--with-turn (inbound &rest body)
  "Create a fresh turn from INBOUND and execute BODY."
  (declare (indent 1))
  `(let ((turn (superchat-turn-new ,inbound "test-session")))
     ,@body))

(defun test-prompt-hooks--with-temp-db (body)
  "Run BODY with a fresh temporary Superchat database."
  (let* ((tmp-dir (make-temp-file "superchat-prompt-hooks-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (funcall body))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

;; ═══════════════════════════════════════════════════════════
;; language-instruction
;; ═══════════════════════════════════════════════════════════

(ert-deftest language-instruction/english-noop ()
  "English default: turn.system-prompt stays empty."
  (let ((superchat-lang "English"))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (equal (superchat-turn-system-prompt result) ""))))))

(ert-deftest language-instruction/chinese-appends ()
  "Chinese lang: turn.system-prompt contains language directive."
  (let ((superchat-lang "Chinese"))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (string-match-p "Chinese" (superchat-turn-system-prompt result)))))))

(ert-deftest language-instruction/empty-lang-noop ()
  "Empty lang string: no system-prompt added."
  (let ((superchat-lang ""))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (equal (superchat-turn-system-prompt result) ""))))))

;; ═══════════════════════════════════════════════════════════
;; memory-context
;; ═══════════════════════════════════════════════════════════

(ert-deftest memory-context/empty-list-noop ()
  "Empty memory list: turn.prompt unchanged."
  (test-prompt-hooks--with-turn "hello"
    (setf (superchat-turn-retrieved-memories turn) nil)
    (let ((result (superchat-prompt-hook--memory-context turn)))
      (should (equal (superchat-turn-prompt result) "")))))

(ert-deftest memory-context/populated-prepends ()
  "Non-empty memories: turn.prompt starts with formatted memories."
  (test-prompt-hooks--with-turn "hello"
    ;; DB row format: (id title content keywords mood status created_at)
    (setf (superchat-turn-retrieved-memories turn)
          (list (list 1 "Test Memory" "This is a test memory." "" "" "" "")))
    (let ((result (superchat-prompt-hook--memory-context turn)))
      (should (string-match-p "Retrieved Memories" (superchat-turn-prompt result)))
      (should (string-match-p "Test Memory" (superchat-turn-prompt result))))))

;; ═══════════════════════════════════════════════════════════
;; experience-context
;; ═══════════════════════════════════════════════════════════

(ert-deftest experience-context/explicit-selection-enters-continuity-envelope ()
  "An explicit Experience request prepends only its satisfied View."
  (let* ((selection
          '(:session-ids ("candidate" "context" "promotion")
            :scope "scope/v0"
            :context (:session-id "context" :event-id "evt-context")))
         (experience
          '(:contract "experience-view-v0" :status satisfied
            :prompt "Reviewed candidate guidance"
            :receipt (:candidate (:session-id "candidate"
                                  :event-id "evt-candidate")
                      :promotion (:session-id "promotion"
                                  :event-id "evt-promotion")))))
    (test-prompt-hooks--with-turn "hello"
      (setf (superchat-turn-prompt turn) "Current question"
            (superchat-turn-state turn) (list :experience-selection selection))
      (cl-letf (((symbol-function 'superchat-experience-assemble-view)
                 (lambda (sessions scope context)
                   (should (equal '("candidate" "context" "promotion")
                                  sessions))
                   (should (equal "scope/v0" scope))
                   (should (equal '(:session-id "context"
                                    :event-id "evt-context")
                                  context))
                   experience))
                ((symbol-function 'superchat-view-assemble)
                 (lambda (&rest _args)
                   '(:contract "context-continuity-v0" :status empty
                     :prompt nil :receipt (:tape-head 0)))))
        (setq turn (superchat-prompt-hook--experience-context turn))
        (should (equal "Reviewed candidate guidance\n\nCurrent question"
                       (superchat-turn-prompt turn)))
        (setq turn (superchat-prompt-hook--conversation-history turn))
        (should
         (equal experience
                (plist-get
                 (plist-get (superchat-turn-state turn) :context-envelope)
                 :experience-view)))))))

(ert-deftest experience-context/ordinary-turn-does-not-query-or-change-prompt ()
  "Ordinary chat remains outside the opt-in Experience View path."
  (test-prompt-hooks--with-turn "hello"
    (setf (superchat-turn-prompt turn) "Current question")
    (cl-letf (((symbol-function 'superchat-experience-assemble-view)
               (lambda (&rest _args)
                 (ert-fail "ordinary Turn queried Experience"))))
      (let ((result (superchat-prompt-hook--experience-context turn)))
        (should (equal "Current question" (superchat-turn-prompt result)))
        (should-not (plist-get (superchat-turn-state result)
                               :experience-view))))))

;; ═══════════════════════════════════════════════════════════
;; file-inline
;; ═══════════════════════════════════════════════════════════

(ert-deftest file-inline/no-fileref-noop ()
  "No file ref in input: turn unchanged."
  (test-prompt-hooks--with-turn "hello world"
    (let ((result (superchat-prompt-hook--file-inline turn)))
      (should (equal (superchat-turn-clean-input result) "hello world"))
      (should (equal (superchat-turn-prompt result) "")))))

(ert-deftest file-inline/missing-file-warns-keeps-going ()
  "Missing file: turn still returned, warning logged."
  (test-prompt-hooks--with-turn "#/nonexistent/file.txt"
    (let ((result (superchat-prompt-hook--file-inline turn)))
      (should (superchat-turn-p result))
      ;; clean-input should be stripped of the file ref
      (should (equal (superchat-turn-clean-input result) "")))))

(ert-deftest file-inline/binary-file-skipped ()
  "Binary file: content NOT inlined."
  (test-prompt-hooks--with-turn "question #/tmp/test.bin"
    (let ((result (superchat-prompt-hook--file-inline turn)))
      (should (string= (superchat-turn-clean-input result) "question")))))

;; ═══════════════════════════════════════════════════════════
;; template-substitution
;; ═══════════════════════════════════════════════════════════

(ert-deftest template-substitution/dollar-input ()
  "$input in template is replaced with clean-input."
  (let ((superchat-general-answer-prompt "Answer: $input"))
    (test-prompt-hooks--with-turn "what is lisp?"
      (let ((result (superchat-prompt-hook--template-substitution turn)))
        (should (string-match-p "what is lisp" (superchat-turn-prompt result)))
        (should-not (string-match-p "\\$input" (superchat-turn-prompt result)))))))

(ert-deftest template-substitution/dollar-lang ()
  "$lang in template is replaced."
  (let ((superchat-lang "French")
        (superchat-general-answer-prompt "Respond in $lang: $input"))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--template-substitution turn)))
        (should (string-match-p "French" (superchat-turn-prompt result)))
        (should-not (string-match-p "\\$lang" (superchat-turn-prompt result)))))))

(ert-deftest template-substitution/no-vars ()
  "Template without $input: user question appended."
  (let ((superchat-general-answer-prompt "Be helpful."))
    (test-prompt-hooks--with-turn "help me"
      (let ((result (superchat-prompt-hook--template-substitution turn)))
        (should (string-match-p "User question: help me"
                                (superchat-turn-prompt result)))))))

;; ═══════════════════════════════════════════════════════════
;; conversation-history
;; ═══════════════════════════════════════════════════════════

(ert-deftest conversation-history/hook-runs ()
  "Hook runs without error and returns a turn."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (test-prompt-hooks--with-turn "hello"
       (let ((result (superchat-prompt-hook--conversation-history turn)))
         (should (superchat-turn-p result)))))))

(ert-deftest conversation-history/tape-view-preserves-disclosure-header ()
  "The prompt Adapter should prepend the View and retain its audit envelope."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (superchat-db-tape-append "adapter-session" "user" "before")
     (superchat-db-tape-append "adapter-session" "assistant" "answer")
     (let ((turn (superchat-turn-new "now" "adapter-session")))
       (setf (superchat-turn-prompt turn) "Current question")
       (let* ((result (superchat-prompt-hook--conversation-history turn))
              (envelope (plist-get (superchat-turn-state result)
                                   :context-envelope)))
         (should
          (equal
           "[Context v0 | raw adapter-session#1-2 | represented none | omitted none | recall none]\n\nUser: before\nAssistant: answer\n\nCurrent question"
           (superchat-turn-prompt result)))
         (should (equal "context-continuity-v0"
                        (plist-get envelope :contract)))
         (should (eq 'satisfied (plist-get envelope :status))))))))

(ert-deftest conversation-history/read-failure-preserves-current-prompt ()
  "An unknown View should remain auditable without blocking the current Turn."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (let ((db-file (expand-file-name "superchat.db"
                                      superchat-data-directory)))
       (superchat-db-close)
       (delete-file db-file)
       (with-temp-file db-file
         (insert "not a sqlite database"))
       (let ((turn (superchat-turn-new "now" "adapter-broken")))
         (setf (superchat-turn-prompt turn) "Current question")
         (let* ((result (superchat-prompt-hook--conversation-history turn))
                (envelope (plist-get (superchat-turn-state result)
                                     :context-envelope)))
           (should (equal "Current question" (superchat-turn-prompt result)))
           (should (eq 'unknown (plist-get envelope :status)))
           (should (eq 'failed
                       (plist-get (plist-get envelope :receipt)
                                  :read-status)))))))))

(ert-deftest conversation-history/maps-existing-context-budgets-to-view ()
  "Existing prompt budget controls should remain effective after the cutover."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (superchat-db-tape-append "adapter-budget" "user" "older")
     (superchat-db-tape-append "adapter-budget" "assistant" "newer")
     (let ((superchat-context-message-count 1)
           (superchat-context-max-chars 4000)
           (turn (superchat-turn-new "now" "adapter-budget")))
       (setf (superchat-turn-prompt turn) "Current question")
       (let ((result (superchat-prompt-hook--conversation-history turn)))
         (should-not (string-match-p "User: older"
                                     (superchat-turn-prompt result)))
         (should (string-match-p "Assistant: newer"
                                 (superchat-turn-prompt result)))
         (should
          (string-prefix-p
           "[Context v0 | raw adapter-budget#2 | represented none | omitted 1 | recall available]"
           (superchat-turn-prompt result))))))))

;; ═══════════════════════════════════════════════════════════
;; Hook composition
;; ═══════════════════════════════════════════════════════════

(ert-deftest hooks/all-build-hooks-return-turn ()
  "All build hooks return the turn (never nil)."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (let ((superchat-lang "English"))
       (test-prompt-hooks--with-turn "hello"
         (setf (superchat-turn-retrieved-memories turn) nil)
         (let ((result turn))
           (setq result (superchat-prompt-hook--language-instruction result))
           (should (superchat-turn-p result))
           (setq result (superchat-prompt-hook--file-inline result))
           (should (superchat-turn-p result))
           (setq result (superchat-prompt-hook--template-substitution result))
           (should (superchat-turn-p result))
           (setq result (superchat-prompt-hook--memory-context result))
           (should (superchat-turn-p result))
           (setq result (superchat-prompt-hook--experience-context result))
           (should (superchat-turn-p result))
           (setq result (superchat-prompt-hook--conversation-history result))
           (should (superchat-turn-p result))))))))

;; ═══════════════════════════════════════════════════════════
;; generative-ui — preset restriction consistency
;; ═══════════════════════════════════════════════════════════

(ert-deftest generative-ui/injected-when-globally-enabled ()
  "No active preset: the global allowlist alone decides."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset nil))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (let ((prompt (superchat-turn-system-prompt result)))
          (should (string-match-p "render_ui" prompt))
          (should (string-match-p "call render_ui directly" prompt))
          (should (string-match-p "Do not define_tool" prompt)))))))

(ert-deftest generative-ui/not-injected-when-globally-disabled ()
  "render_ui absent from the global allowlist: no injection."
  (let ((superchat-llm-tool-names '("read-file"))
        (superchat--active-preset nil))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-restriction-suppresses-injection ()
  "An active preset whose :tools excludes render_ui suppresses the
surface description — advertising an unreachable tool sends the model
hunting for workarounds."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "tool-demo"
                                   :tools '("define_tool"))))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-none-suppresses-injection ()
  "A preset with an explicitly empty tool set suppresses injection."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "bare" :tools 'none)))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-including-render-ui-injects ()
  "A preset whose :tools includes render_ui keeps the description."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "ui" :tools '("render_ui"))))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (string-match-p "render_ui"
                                (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-inheriting-tools-injects ()
  "A preset with :tools nil inherits the global set, so injection stays."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "plain" :tools nil)))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (string-match-p "render_ui"
                                (superchat-turn-system-prompt result)))))))

;; ═══════════════════════════════════════════════════════════
;; file-inline — RLM-style metadata for large files
;; ═══════════════════════════════════════════════════════════

(ert-deftest file-inline/large-file-injects-metadata-not-content ()
  "Above the threshold, the prompt gets shape + access instructions,
never the body."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "big" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dotimes (i 2000)
              (insert (format "line %d with some padding text here\n" i))))
          (let ((superchat-inline-metadata-threshold 1024)
                (superchat-inline-file-content t))
            (test-prompt-hooks--with-turn (format "summarize #%s" path)
              (let* ((result (superchat-prompt-hook--file-inline turn))
                     (prompt (superchat-turn-prompt result)))
                (should (string-match-p "NOT inlined" prompt))
                (should (string-match-p "2000 lines" prompt))
                (should (string-match-p ":START-END" prompt))
                (should-not (string-match-p "read-file" prompt))
                ;; The body must not be there: a line from the middle.
                (should-not (string-match-p "line 1500" prompt))))))
      (delete-file path))))

(ert-deftest file-inline/small-file-still-inlines ()
  "Below the threshold, behaviour is unchanged: content is inlined."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "small" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "tiny content marker-xyz\n"))
          (let ((superchat-inline-metadata-threshold 32768)
                (superchat-inline-file-content t))
            (test-prompt-hooks--with-turn (format "summarize #%s" path)
              (let* ((result (superchat-prompt-hook--file-inline turn))
                     (prompt (superchat-turn-prompt result)))
                (should (string-match-p "marker-xyz" prompt))
                (should-not (string-match-p "NOT inlined" prompt))))))
      (delete-file path))))

(provide 'test-prompt-hooks)
;;; test-prompt-hooks.el ends here
