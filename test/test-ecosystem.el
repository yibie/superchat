;;; test-ecosystem.el --- ERT tests for ecosystem integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-magit, eglot tools, ob-superchat, and Telega.
;; Pure logic and buffer manipulation only — no LLM calls, no Magit/eglot servers.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
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
    (should (and (stringp (plist-get captured :session-id))
                 (not (string-empty-p (plist-get captured :session-id)))))
    (should (eq (plist-get captured :buffer) (current-buffer)))
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
    (should (and (stringp (plist-get captured :session-id))
                 (not (string-empty-p (plist-get captured :session-id)))))
    (should (eq (plist-get captured :buffer) (current-buffer)))
    (should (eq (plist-get (plist-get captured :origin) :surface)
                'org-babel))))

(ert-deftest ecosystem/blocking-adapters-use-public-identity-matrix ()
  "Org and Magit classify real source buffers through public authority."
  (dolist (adapter
           (list (lambda () (ob-superchat--call-llm "q" "s" nil))
                 (lambda () (superchat-magit--call-llm "p"))))
    (dolist (case '(ordinary inherited owned malformed))
      (let* ((session (format "adapter-owned-%s" (gensym)))
             (source (if (eq case 'owned)
                         (plist-get
                          (superchat-editor-workspace-acquire-buffer
                           session 'adapter-test)
                          :buffer)
                       (generate-new-buffer " *adapter-real-source*")))
             submitted before-markers)
        (unwind-protect
            (with-current-buffer source
              (when (eq case 'malformed)
                (setq-local superchat--editor-workspace-owned-p t))
              (setq before-markers
                    (list (local-variable-p
                           'superchat--editor-workspace-session-id)
                          (local-variable-p
                           'superchat--editor-workspace-owned-p)
                          (local-variable-p
                           'superchat--editor-workspace-role)))
              (cl-labels ((invoke () (funcall adapter)))
                (cl-letf (((symbol-function
                            'superchat-runtime-submit-blocking)
                           (lambda (request &optional _timeout)
                             (setq submitted request)
                             " result ")))
                  (if (eq case 'malformed)
                      (progn (should-error (invoke))
                             (should-not submitted))
                    (if (eq case 'inherited)
                        (let ((superchat--editor-workspace-session-id
                               "inherited")
                              (superchat--editor-workspace-owned-p t)
                              (superchat--editor-workspace-role
                               'inherited-role))
                          (invoke))
                      (invoke))
                    (should (eq source (plist-get submitted :buffer)))
                    (if (eq case 'owned)
                        (should (equal session
                                       (plist-get submitted :session-id)))
                      (should-not
                       (superchat-editor-workspace-owner-session source))))))
              (should (equal before-markers
                             (list (local-variable-p
                                    'superchat--editor-workspace-session-id)
                                   (local-variable-p
                                    'superchat--editor-workspace-owned-p)
                                   (local-variable-p
                                    'superchat--editor-workspace-role)))))
          (when (buffer-live-p source) (kill-buffer source))
          (ignore-errors (superchat-session-workspace-close session))))))
  ;; An adapter owner mismatch is unreachable without replacing the public
  ;; query: its candidate Session is either the exact queried owner or a mint
  ;; on an ordinary buffer.  The malformed case above exercises the reachable
  ;; pre-submit authority rejection; the public mismatch seam has its own
  ;; exact-buffer test in test-editor-workspace.el.
  )

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
         (helper (symbol-file 'superchat-test-helper 'provide))
         (emacs (expand-file-name invocation-name invocation-directory))
         (output (generate-new-buffer " *cowork-telega-autoload-test*")))
    (unwind-protect
        (let ((status
               (call-process
                emacs nil output nil
                "-Q" "--batch" "-L" root
                "-l" helper "-l" "superchat"
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

(defun ecosystem--call-with-ob-skill (function)
  "Call FUNCTION with a real temporary Skill directory, body and Org buffer."
  (require 'superchat-skills)
  (require 'org)
  (let* ((directory (make-temp-file "superchat-ob-skill-" t))
         (superchat-skills-directory directory)
         (body "Skill context\n第二行\n"))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "babel-fixture.md" directory)
            (insert "---\nname: display-alias\nmodel: must-not-override\n---\n" body))
          (with-temp-file (expand-file-name "babel-plain.md" directory)
            (insert body))
          (with-temp-buffer
            (org-mode)
            (funcall function directory body)))
      (delete-directory directory t))))

(ert-deftest ecosystem/ob-real-skill-symbol-content ()
  "A symbol names the real Skill file, whose Preset body precedes the query."
  (ecosystem--call-with-ob-skill
   (lambda (directory body)
     (let ((preset (superchat-skills-load "babel-fixture")))
       (should (superchat-preset-p preset))
       (should (equal "display-alias" (superchat-preset-name preset)))
       (should (equal (expand-file-name "babel-fixture.md" directory)
                      (superchat-preset-source-file preset)))
       (should (equal body (superchat-preset-skill-body preset)))
       (should (equal (concat body "\n\n" "Question\n原文\n")
                      (ob-superchat--build-query "Question\n原文\n" 'babel-fixture)))))))

(ert-deftest ecosystem/ob-real-skill-string-content ()
  "String basenames load real frontmatter and plain-body Skills."
  (ecosystem--call-with-ob-skill
   (lambda (_directory body)
     (dolist (name '("babel-fixture" "babel-plain"))
       (should (superchat-preset-p (superchat-skills-load name)))
       (should (equal (concat body "\n\n" "  query\n")
                      (ob-superchat--build-query "  query\n" name)))))))

(ert-deftest ecosystem/ob-real-skill-missing-is-silent ()
  "Missing names and display-only aliases leave exact body bytes unchanged."
  (ecosystem--call-with-ob-skill
   (lambda (_directory _body)
     (let ((query "  unchanged\n问题\n") diagnostics)
       (cl-letf (((symbol-function 'message)
                  (lambda (&rest args) (push args diagnostics)))
                 ((symbol-function 'display-warning)
                  (lambda (&rest args) (push args diagnostics))))
         (dolist (name '(nil absent-babel "absent-babel" display-alias "display-alias"))
           (should (equal query (ob-superchat--build-query query name)))))
       (should-not diagnostics)
       (cl-letf (((symbol-function 'superchat-skills-load) nil))
         (should (equal query (ob-superchat--build-query query 'babel-fixture))))))))

(ert-deftest ecosystem/ob-real-executor-preserves-expansion-options-results ()
  "Actual Babel expansion and adapter preserve request identity and results."
  (ecosystem--call-with-ob-skill
   (lambda (_directory body)
     (let ((superchat-llm-backend 'backend)
           (ob-superchat-system-prompt "default system")
           (ob-superchat-timeout 9)
           (source (current-buffer)))
       (dolist (model '(nil "model-x"))
         (dolist (system '(nil "" "block system"))
           (dolist (response '("  answer\n " nil boundary-error))
             (let* ((params (append '((:skill . "babel-fixture")
                                      (:prologue . "PROLOGUE") (:epilogue . "EPILOGUE"))
                                    (when model (list (cons :model model)))
                                    (when system (list (cons :system system)))))
                    (expanded (org-babel-expand-body:generic "query" params))
                    captured)
               (should (equal "PROLOGUE\nquery\nEPILOGUE" expanded))
               (should-not (equal "query" expanded))
               (cl-letf (((symbol-function 'superchat-runtime-submit-blocking)
                          (lambda (request &optional _timeout)
                            (setq captured request)
                            (if (eq response 'boundary-error)
                                (error "fixture boundary failure")
                              response))))
                 (should (equal (pcase response
                                  ('boundary-error "[Superchat error: fixture boundary failure]")
                                  ('nil "[Superchat: no response]")
                                  (_ "answer"))
                                (org-babel-execute:superchat "query" params))))
               (should (equal (concat body "\n\n" expanded) (plist-get captured :prompt)))
               (should (equal (plist-get captured :input) (plist-get captured :prompt)))
               (should (equal model (plist-get captured :target-model)))
               (should (equal (if (equal system "") nil (or system "default system"))
                              (plist-get captured :system-prompt)))
               (should (= 9 (plist-get captured :timeout)))
               (should (eq 'none (plist-get captured :tools)))
               (should-not (plist-get captured :record-conversation))
               (should (eq source (plist-get captured :buffer)))
               (should (stringp (plist-get captured :session-id)))
               (should (eq 'org-babel (plist-get (plist-get captured :origin) :surface)))
               (should (eq 'org-mode (plist-get (plist-get captured :origin) :major-mode)))
               (should-not (superchat-editor-workspace-owner-session source))
               (should-not (local-variable-p 'superchat--editor-workspace-owned-p))))))
       (let ((superchat-llm-backend nil) called)
         (cl-letf (((symbol-function 'superchat-runtime-submit-blocking)
                    (lambda (&rest _) (setq called t))))
           (should-error (org-babel-execute:superchat "q" nil) :type 'user-error))
         (should-not called))))))

(ert-deftest ecosystem/ob-real-source-block-inserts-results ()
  "A real Org block expands and writes RESULTS through the unchanged adapter."
  (ecosystem--call-with-ob-skill
   (lambda (_directory body)
     (let ((superchat-llm-backend 'backend)
           (org-confirm-babel-evaluate nil)
           (source (current-buffer)) captured)
       (insert "#+begin_src superchat :skill babel-fixture :model model-block :system \"block system\" :prologue BEFORE :epilogue AFTER :results scalar replace\nQuestion\n#+end_src\n")
       (goto-char (point-min))
       (cl-letf (((symbol-function 'superchat-runtime-submit-blocking)
                  (lambda (request &optional _timeout)
                    (setq captured request)
                    "  RESULT-中文\n ")))
         (should (equal "RESULT-中文" (org-babel-execute-src-block))))
       (should (equal (concat body "\n\nBEFORE\nQuestion\nAFTER")
                      (plist-get captured :prompt)))
       (should (eq source (plist-get captured :buffer)))
       (should (equal "model-block" (plist-get captured :target-model)))
       (should (equal "block system" (plist-get captured :system-prompt)))
       (should (string-match-p "#\\+RESULTS:\n: RESULT-中文" (buffer-string)))))))

(ert-deftest ecosystem/ob-skill-fixture-cleans-up-on-assertion ()
  "An assertion failure releases the actual Skill files, directory and buffer."
  (let (directory files buffer)
    (should-error
     (ecosystem--call-with-ob-skill
      (lambda (dir _body)
        (setq directory dir files (directory-files dir t "\\.md\\'")
              buffer (current-buffer))
        (ert-fail "ob fixture deliberate failure")))
     :type 'ert-test-failed)
    (should (= 2 (length files)))
    (should-not (buffer-live-p buffer))
    (should-not (file-exists-p directory))
    (dolist (file files) (should-not (file-exists-p file)))))

;;; test-ecosystem.el ends here
