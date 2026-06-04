;;; test-prompt-hooks.el — ERT tests for superchat-prompt-hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests each prompt hook function in isolation.  No buffer, no LLM, no async.

;;; Code:

(require 'ert)
(require 'superchat-core)
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
  (test-prompt-hooks--with-turn "hello"
    (let ((result (superchat-prompt-hook--conversation-history turn)))
      (should (superchat-turn-p result)))))

;; ═══════════════════════════════════════════════════════════
;; Hook composition
;; ═══════════════════════════════════════════════════════════

(ert-deftest hooks/all-five-return-turn ()
  "All five hooks return the turn (never nil)."
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
        (setq result (superchat-prompt-hook--conversation-history result))
        (should (superchat-turn-p result))))))

(provide 'test-prompt-hooks)
;;; test-prompt-hooks.el ends here
