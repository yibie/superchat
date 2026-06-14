;;; test-ecosystem.el --- ERT tests for ecosystem hooks (Magit / eglot / org-babel) -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-magit, eglot tools, and ob-superchat.
;; Pure logic and buffer manipulation only — no LLM calls, no Magit/eglot servers.

;;; Code:

(require 'ert)
(require 'superchat-magit)
(require 'ob-superchat)
(require 'superchat-tools)
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; Magit: prompt building
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/magit-build-prompt-conventional ()
  "Build a conventional-commit prompt from a diff."
  (let ((superchat-magit-commit-style 'conventional))
    (let ((result (superchat-magit--build-prompt "diff --git a/foo.el b/foo.el")))
      (should (string-match-p "conventional-commit" result))
      (should (string-match-p "diff --git" result)))))

(ert-deftest ecosystem/magit-build-prompt-freeform ()
  "Build a free-form commit prompt from a diff."
  (let ((superchat-magit-commit-style 'freeform))
    (let ((result (superchat-magit--build-prompt "diff --git a/bar.el b/bar.el")))
      (should (string-match-p "concise" result))
      (should (string-match-p "imperative" result))
      (should (string-match-p "diff --git" result)))))

(ert-deftest ecosystem/magit-build-prompt-includes-diff ()
  "Prompt always includes the full diff text."
  (let ((diff "diff --git a/x.el b/x.el\n+42"))
    (let ((result (superchat-magit--build-prompt diff)))
      (should result)
      (should (string-match-p "\\+42" result)))))

;; ═══════════════════════════════════════════════════════════
;; eglot tools: soft-dep error when eglot absent
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/lsp-references-errors-without-eglot ()
  "lsp-references signals error when eglot is not active."
  ;; In a test environment without eglot, the function should error early.
  (should-error
   (condition-case nil
       (superchat-tool-lsp-references)
     (error (signal 'error "As expected")))))

(ert-deftest ecosystem/lsp-hover-errors-without-eglot ()
  "lsp-hover signals error when eglot is not active."
  (should-error
   (condition-case nil
       (superchat-tool-lsp-hover)
     (error (signal 'error "As expected")))))

(ert-deftest ecosystem/lsp-code-actions-errors-without-eglot ()
  "lsp-code-actions signals error when eglot is not active."
  (should-error
   (condition-case nil
       (superchat-tool-lsp-code-actions)
     (error (signal 'error "As expected")))))

;; ═══════════════════════════════════════════════════════════
;; ob-superchat: query building
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/ob-query-no-skill-returns-body ()
  "When no skill is set, the query is the body unchanged."
  (let ((result (ob-superchat--build-query "What is Emacs?" nil)))
    (should (equal result "What is Emacs?"))))

(ert-deftest ecosystem/ob-query-with-nil-skill-returns-body ()
  "When skill arg is nil, body is returned unchanged."
  (let ((result (ob-superchat--build-query "Some query" nil)))
    (should (equal result "Some query"))))

(ert-deftest ecosystem/ob-query-with-non-existent-skill ()
  "When skill does not exist, body is returned unchanged."
  (let ((result (ob-superchat--build-query "body text" 'nonexistent-skill-xyz)))
    (should (equal result "body text"))))

(ert-deftest ecosystem/ob-defcustoms-exist ()
  "ob-superchat defcustoms are defined with sensible defaults."
  (should ob-superchat-system-prompt)
  (should (stringp ob-superchat-system-prompt))
  (should (integerp ob-superchat-timeout))
  (should (> ob-superchat-timeout 0)))

;; ═══════════════════════════════════════════════════════════
;; superchat-magit: defcustoms exist
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/magit-defcustoms-exist ()
  "superchat-magit defcustoms are defined."
  (should superchat-magit-commit-style)
  (should (memq superchat-magit-commit-style '(conventional freeform)))
  (should (stringp superchat-magit-commit-system-prompt))
  (should (> (length superchat-magit-commit-system-prompt) 10)))

;; ═══════════════════════════════════════════════════════════
;; eglot tools are in the registry (integrated tool list)
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/eglot-tool-functions-defined ()
  "All three eglot tool functions are defined."
  (should (fboundp 'superchat-tool-lsp-references))
  (should (fboundp 'superchat-tool-lsp-code-actions))
  (should (fboundp 'superchat-tool-lsp-hover)))

;;; test-ecosystem.el ends here
