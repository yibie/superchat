;;; test-ecosystem.el --- ERT tests for ecosystem integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-magit, eglot tools, ob-superchat, and Telega.
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

(ert-deftest ecosystem/magit-call-llm-uses-blocking-runtime ()
  "Magit commit generation submits a tool-free blocking Runtime request."
  (let ((superchat-llm-backend 'backend)
        captured)
    (cl-letf (((symbol-function 'superchat-runtime-submit-blocking)
               (lambda (request &optional _timeout)
                 (setq captured request)
                 " fix: trim message \n")))
      (should (equal (superchat-magit--call-llm "diff prompt")
                     "fix: trim message")))
    (should (equal (plist-get captured :prompt) "diff prompt"))
    (should (equal (plist-get captured :system-prompt)
                   superchat-magit-commit-system-prompt))
    (should (eq (plist-get captured :tools) 'none))
    (should-not (plist-get captured :record-conversation))
    (should (eq (plist-get (plist-get captured :origin) :surface)
                'magit))))

;; ═══════════════════════════════════════════════════════════
;; eglot tools: soft-dep error when eglot absent
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/lsp-references-errors-without-eglot ()
  "lsp-references signals error when eglot is not active."
  ;; In a test environment without eglot, the function should error early.
  (should-error
   (condition-case nil
       (superchat-tool-lsp-references)
     (error (error "As expected")))))

(ert-deftest ecosystem/lsp-hover-errors-without-eglot ()
  "lsp-hover signals error when eglot is not active."
  (should-error
   (condition-case nil
       (superchat-tool-lsp-hover)
     (error (error "As expected")))))

(ert-deftest ecosystem/lsp-code-actions-errors-without-eglot ()
  "lsp-code-actions signals error when eglot is not active."
  (should-error
   (condition-case nil
       (superchat-tool-lsp-code-actions)
     (error (error "As expected")))))

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

(ert-deftest ecosystem/ob-call-llm-uses-blocking-runtime ()
  "Org Babel submits a tool-free blocking Runtime request with model override."
  (let ((superchat-llm-backend 'backend)
        (ob-superchat-timeout 7)
        captured)
    (cl-letf (((symbol-function 'superchat-runtime-submit-blocking)
               (lambda (request &optional _timeout)
                 (setq captured request)
                 "answer")))
      (should (equal (ob-superchat--call-llm "question" "system" "model-x")
                     "answer")))
    (should (equal (plist-get captured :input) "question"))
    (should (equal (plist-get captured :prompt) "question"))
    (should (equal (plist-get captured :system-prompt) "system"))
    (should (equal (plist-get captured :target-model) "model-x"))
    (should (= (plist-get captured :timeout) 7))
    (should (eq (plist-get captured :tools) 'none))
    (should-not (plist-get captured :record-conversation))
    (should (eq (plist-get (plist-get captured :origin) :surface)
                'org-babel))))

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
;; Telega: Cowork bundle entrypoint
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/cowork-telega-bundle-is-the-only-entrypoint ()
  "Core autoloads the Cowork bundle and no longer exposes the tracer."
  (should (autoloadp (symbol-function 'superchat-cowork-telega)))
  (should-not (fboundp 'superchat-telega-cowork))
  (let* ((root (file-name-directory (locate-library "superchat.el")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (output (generate-new-buffer " *cowork-telega-autoload-test*")))
    (unwind-protect
        (let ((status
               (call-process
                emacs nil output nil
                "-Q" "--batch" "-L" root "-l" "superchat"
                "--eval"
                "(let ((definition
                         (symbol-function 'superchat-cowork-telega)))
                   (unless (autoloadp definition) (kill-emacs 2))
                   (autoload-do-load definition 'superchat-cowork-telega)
                   (unless
                       (and (commandp 'superchat-cowork-telega)
                            (featurep 'superchat-cowork-telega)
                            (not (fboundp 'superchat-telega-cowork)))
                     (kill-emacs 3)))")))
          (should (= 0 status)))
      (kill-buffer output))))

;; ═══════════════════════════════════════════════════════════
;; eglot tools are in the registry (integrated tool list)
;; ═══════════════════════════════════════════════════════════

(ert-deftest ecosystem/eglot-tool-functions-defined ()
  "All three eglot tool functions are defined."
  (should (fboundp 'superchat-tool-lsp-references))
  (should (fboundp 'superchat-tool-lsp-code-actions))
  (should (fboundp 'superchat-tool-lsp-hover)))

;;; test-ecosystem.el ends here
