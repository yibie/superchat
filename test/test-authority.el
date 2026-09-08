;;; test-authority.el --- Tests for superchat-authority -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-authority)
(defvar superchat-authority--session-allowlist)
(defvar superchat-authority-mode)
(require 'superchat-preset)
(require 'superchat-tools)
(require 'superchat-workspace)

(defvar superchat-agent-pre-tool-functions)
(defvar superchat-agent-post-tool-functions)
(defvar superchat-agent-post-tool-failure-functions)
(defvar superchat-agent-permission-functions)

(ert-deftest authority/trusted-ask-disposition-is-hashed ()
  (let ((effects '((:effect human-interaction :resource human-input
                   :scope (:session-id "S" :run-id "R")))))
    (should-not (equal (superchat-authority-approval-hash
                        "ask_user" '("City?") effects "authority-v1/default/human-input-v1" "E"
                        "human-input-v1:allow")
                       (superchat-authority-approval-hash
                        "ask_user" '("City?") effects "authority-v1/default/human-input-v1" "E"
                        "human-input-v1:deny")))
    (should-not (equal (superchat-authority-approval-hash "ask_user" '("City?") effects)
                       (superchat-authority-approval-hash
                        "ask_user" '("City?") '((:effect local-read :resource buffer)))))))

(ert-deftest authority/trusted-ask-effect-alone-never-permission ()
  (dolist (attendance '(attended unattended))
    (should (eq 'deny (plist-get (superchat-authority-decide
                                 "spoof" '("City?") attendance nil
                                 '((:effect human-interaction :resource human-input)))
                                :decision)))))

(ert-deftest authority/confirmation-uses-compact-minibuffer-with-details ()
  "Approval stays in the minibuffer; full request details remain available."
  (let ((use-dialog-box t)
        (read-char-choice-use-read-key t)
        (superchat-authority-approval-previews (make-hash-table :test 'equal)))
    (dolist (answer '(?y ?n))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (prompt choices &optional help &rest _)
                   (should-not use-dialog-box)
                   (should-not read-char-choice-use-read-key)
                   (should-not (string-match-p "\n" prompt))
                   (should (string-match-p "test-tool" prompt))
                   (should-not (string-match-p "secret-argument" prompt))
                   (should (string-match-p "secret-argument" help))
                   (should (string-match-p "request-identity" help))
                   (should (assq answer choices))
                   (list answer "answer"))))
        (should (eq (not (null (superchat-authority-confirm
                               '(:tool-name "test-tool"
                                 :args ("secret-argument")
                                 :effects ((:effect local-read))
                                 :request-hash "request-identity"))))
                    (eq answer ?y)))))
    (should use-dialog-box)
    (should read-char-choice-use-read-key)))

(ert-deftest authority/requires-explicit-attendance ()
  (should-error (superchat-authority-decide "read_buffer" '("README.md") nil))
  (should-error (superchat-authority-decide "read_buffer" '("README.md") t)))

(ert-deftest authority/normalizes-known-policies ()
  (should (eq 'default (superchat-authority-normalize-policy nil)))
  (should (eq 'observer (superchat-authority-normalize-policy "observer")))
  (should (eq 'developer-project
              (superchat-authority-normalize-policy :developer-project)))
  (should (superchat-authority-policy-known-p "cowork"))
  (should-not (superchat-authority-policy-known-p 'admin))
  (should-error (superchat-authority-normalize-policy 'admin)))

(ert-deftest authority/composes-decisions-by-strict-precedence ()
  (should (eq 'deny (superchat-authority-compose-decisions 'allow 'deny 'ask)))
  (should (eq 'ask (superchat-authority-compose-decisions '(defer allow ask))))
  (should (eq 'allow (superchat-authority-compose-decisions nil 'allow)))
  (should (eq 'defer (superchat-authority-compose-decisions nil 'defer))))

(ert-deftest authority/accepts-legacy-permission-results ()
  (should (eq 'defer (superchat-authority-decision-from-legacy nil)))
  (should (eq 'allow (superchat-authority-decision-from-legacy t)))
  (should (eq 'allow (superchat-authority-decision-from-legacy 'allow)))
  (should (eq 'ask (superchat-authority-decision-from-legacy :ask)))
  (should (eq 'deny (superchat-authority-decision-from-legacy "DENY")))
  (should (eq 'ask (superchat-authority-decision-from-legacy
                    '(:permissionDecision "ask"))))
  (should (eq 'defer (superchat-authority-decision-from-legacy 'bogus))))

(ert-deftest authority/normalizes-args-deterministically ()
  (let ((a '(:b 2 :a (:z 3 :y 4)))
        (b '(:a (:y 4 :z 3) :b 2))
        (table (make-hash-table :test 'equal)))
    (puthash "b" 2 table)
    (puthash "a" '(:z 3 :y 4) table)
    (should (equal (superchat-authority-normalize-args a)
                   (superchat-authority-normalize-args b)))
    (should (equal (superchat-authority-normalize-args a)
                   (superchat-authority-normalize-args table)))))

(ert-deftest authority/normalizes-nil-as-no-args ()
  (should (null (superchat-authority-normalize-args nil))))

(ert-deftest authority/keeps-positional-string-args-as-a-list ()
  (should (equal '("/tmp/x" "body")
                 (superchat-authority-normalize-args '("/tmp/x" "body")))))

(ert-deftest authority/resolves-representative-built-in-effects ()
  (should (equal (superchat-authority-resolve-effects "list_buffers" nil)
                 '((:effect local-read :resource buffer))))
  (should (equal (superchat-authority-resolve-effects "read_buffer" '("README.md"))
                 '((:effect local-read :resource buffer :scope "README.md"))))
  ;; Explicit `:scope nil' locked down: a missing/malformed buffer argument
  ;; must produce a present `:scope' key with value nil, not an absent key
  ;; (which would be mistaken for `list_buffers''s deliberate no-scope
  ;; "every buffer" effect).  No `:unprompted' key: S07-fix-2 removed it.
  (should (equal (superchat-authority-resolve-effects "read_buffer" '(:buffer nil))
                 '((:effect local-read :resource buffer :scope nil))))
  (should (equal (superchat-authority-resolve-effects
                  "edit_buffer"
                  '(:buffer_name "out" :old_string "before" :new_string "after"))
                 '((:effect local-write :resource buffer :scope "out"))))
  (should (equal (superchat-authority-resolve-effects
                  "edit_buffer" '("notes" "before" "after"))
                 '((:effect local-write :resource buffer :scope "notes"))))
  (should (equal (superchat-authority-resolve-effects
                  "read_buffer" '(:buffer "*scratch*"))
                 '((:effect local-read :resource buffer :scope "*scratch*"))))
  (should (equal (superchat-authority-resolve-effects
                  "replace_buffer" '(:buffer_name "*scratch*" :content "x"))
                 '((:effect local-write :resource buffer :scope "*scratch*"))))
  (should (equal (superchat-authority-resolve-effects
                  "shell-command" '("git status"))
                 '((:effect process-exec :resource process :scope "git status"))))
  (should (equal (superchat-authority-resolve-effects
                  "eval_elisp" '((expression . "(+ 1 1)")))
                 '((:effect process-exec :resource emacs :scope "(+ 1 1)"))))
  (should (equal (superchat-authority-resolve-effects
                  "apropos" '(:pattern "buffer" :type "command"))
                 '((:effect local-read :resource emacs :scope introspection))))
  (should (equal (superchat-authority-resolve-effects "describe-function" nil)
                 '((:effect local-read :resource emacs :scope introspection))))
  (should (equal (superchat-authority-resolve-effects "describe-variable" nil)
                 '((:effect local-read :resource emacs :scope introspection))))
  (should (equal (superchat-authority-resolve-effects "workspace_read" nil)
                 '((:effect local-read :resource workspace :scope current))))
  (should (equal (superchat-authority-resolve-effects
                  "workspace_read" '(:artifact_id "artifact-notes"))
                 '((:effect local-read :resource workspace
                    :scope "artifact-notes"))))
  (should (equal (superchat-authority-resolve-effects
                  "workspace_write"
                  '(:content "x" :artifact_id "artifact-notes"))
                 '((:effect local-write :resource workspace
                    :scope "artifact-notes"))))
  (should (equal (superchat-authority-resolve-effects
                  "workspace_publish" '(:content "report" :role "collector"))
                 '((:effect local-write :resource workspace :scope current))))
  (should (equal (superchat-authority-resolve-effects
                  "sql" '("SELECT * FROM tape"))
                 '((:effect local-read :resource database :scope superchat))))
  (should (equal (superchat-authority-resolve-effects
                  "team_message" '(:action "send" :to "lead"))
                 '((:effect local-write :resource team :scope message))))
  (should (equal (superchat-authority-resolve-effects "mcp.dynamic/tool" nil)
                 '((:effect unknown :resource dynamic :scope "mcp.dynamic/tool")))))

(ert-deftest authority/eval-elisp-read-only-whitelist-is-conservative ()
  "Only the small, explicit read-only Elisp whitelist gets local-read."
  (dolist (expression '("(buffer-list)"
                        "(buffer-name)"
                        "(point)"
                        "(buffer-substring-no-properties 1 2)"
                        "(symbol-value 'foo)"
                        "(boundp 'foo)"
                        "(fboundp 'foo)"
                        "(length '(a b))"
                        "(car '(a b))"
                        "(cdr '(a b))"
                        "(nth 1 '(a b))"
                        "(format \"%s\" \"ok\")"
                        "(concat \"a\" \"b\")"
                        "(let ((x 1)) (when t x))"
                        "(if t (buffer-list) (point))"
                        "(quote (delete-file \"x\"))"))
    (should
     (equal (superchat-authority-resolve-effects
             "eval_elisp" `((expression . ,expression)))
            `((:effect local-read :resource emacs :scope ,expression)))))
  (dolist (expression '("(delete-file \"x\")"
                        "(shell-command \"rm x\")"
                        "(write-region \"x\" nil \"file\")"
                        "(funcall (intern \"delete-file\") \"x\")"
                        "(function delete-file)"
                        "(apply #'delete-file '(\"x\"))"
                        "(eval '(delete-file \"x\"))"
                        "(let ((f #'delete-file)) (funcall f \"x\"))"
                        "(fset 'foo #'delete-file)"
                        "(unknown-function)"
                        "(let ((x 1)) (when t (delete-file \"y\")))"
                        "(set 'x 1)"
                        "(setq x 1)"
                        "(setf x 1)"))
    (should
     (equal (superchat-authority-resolve-effects
             "eval_elisp" `((expression . ,expression)))
            `((:effect process-exec :resource emacs :scope ,expression)))))
  (should
   (equal (superchat-authority-resolve-effects
           "eval_elisp" '((expression . "(")))
          '((:effect process-exec :resource emacs :scope "(")))))

(ert-deftest authority/observer-allows-classified-eval-read-only ()
  "Observer policy does not deny a classified read-only eval expression."
  (let* ((args '((expression . "(buffer-list)")))
         (effects (superchat-authority-resolve-effects "eval_elisp" args))
         (authority (superchat-authority-decide
                     "eval_elisp" args 'attended 'observer effects)))
    (should (eq 'local-read (plist-get (car effects) :effect)))
    (should-not (eq 'deny
                    (superchat-authority--effective-decision
                     "eval_elisp" args authority nil)))))

(ert-deftest authority/eval-elisp-rejects-multiple-top-level-forms ()
  "A read-only first form cannot hide a second top-level form."
  (should
   (equal (superchat-authority-resolve-effects
           "eval_elisp"
           '((expression . "(buffer-list) (delete-file \"x\")")))
          '((:effect process-exec :resource emacs
             :scope "(buffer-list) (delete-file \"x\")"))))
  (should
   (equal (superchat-authority-resolve-effects
           "eval_elisp"
           '((expression . "(buffer-list)\n")))
          '((:effect local-read :resource emacs
             :scope "(buffer-list)\n")))))

(ert-deftest authority/eval-elisp-read-classifier-works-before-cl-extra-load ()
  "The read classifier must not depend on an incidental `cl-extra' preload."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (locate-library "superchat-tools")))
        (output (generate-new-buffer " *superchat-eval-classifier-test*")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (should
           (= 0
              (call-process
               emacs nil output nil
               "-Q" "--batch" "-L" root
               "--eval"
               "(progn (setq load-prefer-newer t) (require 'superchat-tools) (unless (superchat-tools--eval-elisp-read-only-p \"(buffer-list)\") (kill-emacs 2)))"))))
      (kill-buffer output))))

(ert-deftest authority/retired-path-tools-have-no-effect-resolvers ()
  (dolist (name '("read-file" "write-file" "append-file" "quick-write"
                  "list-files" "find-files" "search-text" "make_directory"))
    (should (equal (superchat-authority-resolve-effects name nil)
                   `((:effect unknown :resource dynamic :scope ,name))))))

(ert-deftest authority/apropos-finds-functions-and-summarizes-docs ()
  (let ((result (superchat-tool-apropos "^superchat-tool-apropos$" "function")))
    (should (string-match-p
             (regexp-quote
              "superchat-tool-apropos — List Emacs symbols matching PATTERN")
             result))))

(ert-deftest authority/apropos-finds-variables ()
  (should
   (string-match-p
    (regexp-quote "superchat-llm-tool-names — Built-in llm.el tool names")
    (superchat-tool-apropos "^superchat-llm-tool-names$" "variable"))))

(ert-deftest authority/apropos-bounds-wide-results ()
  (let ((result (superchat-tool-apropos "" "command")))
    (should (<= (length (split-string result "\n"))
                (1+ superchat-tool-apropos-max-results)))
    (should (string-match-p
             "more matches; narrow pattern to see the rest\."
             result))))

(ert-deftest authority/apropos-reports-undocumented-functions ()
  (let ((name "superchat-test-apropos-undocumented"))
    (unwind-protect
        (progn
          (fset (intern name) (lambda () nil))
          (should (string-match-p
                   (regexp-quote "(no documentation)")
                   (superchat-tool-apropos
                    (concat "^" name "$") "function"))))
      (fmakunbound (intern name))
      (unintern (intern name) obarray))))

(ert-deftest authority/apropos-invalid-pattern-is-readable ()
  (should (string-match-p
           "Error: invalid apropos pattern"
           (superchat-tool-apropos "["))))

(ert-deftest authority/registered-effect-resolver-overrides-fallback ()
  (let ((superchat-authority-effect-resolvers
         (make-hash-table :test 'equal)))
    (superchat-authority-register-effect-resolver
     "custom"
     (lambda (args)
       `((:effect external-write
          :resource message
          :scope ,(car args)))))
    (should
     (equal
      '((:effect external-write :resource message :scope "thread-1"))
      (superchat-authority-resolve-effects "custom" '("thread-1"))))))

(ert-deftest authority/unregisters-policy-resolver-and-preview-registrations ()
  "Registered Authority seams can be removed using their normalized names."
  (let ((superchat-authority--policy-registry (make-hash-table :test 'eq))
        (superchat-authority-effect-resolvers (make-hash-table :test 'equal))
        (superchat-authority-approval-previews (make-hash-table :test 'equal))
        (resolver (lambda (_args) '((:effect local-read :resource test))))
        (preview (lambda (_request) nil)))
    (superchat-authority-register-policy :TestPolicy #'ignore)
    (superchat-authority-register-effect-resolver 'test-tool resolver)
    (superchat-authority-register-approval-preview 'test-tool preview)
    (should (superchat-authority-unregister-policy "TESTPOLICY"))
    (should-not (superchat-authority-policy-known-p 'testpolicy))
    (should (superchat-authority-unregister-effect-resolver 'test-tool))
    (should (superchat-authority-unregister-approval-preview "test-tool"))
    (should-not (gethash "test-tool" superchat-authority-effect-resolvers))
    (should-not (gethash "test-tool" superchat-authority-approval-previews))
    (should-not (superchat-authority-unregister-policy 'default))
    (should-not (superchat-authority-unregister-effect-resolver 'missing))
    (should-not (superchat-authority-unregister-approval-preview 'missing))))

(ert-deftest authority/executor-previews-before-approval-and-execution ()
  (authority-test--capture-events
    (let ((superchat-authority-approval-previews
           (make-hash-table :test 'equal))
          (superchat-authority-approval-function
           #'superchat-authority-confirm)
          order)
      (superchat-authority-register-approval-preview
       "edit_buffer"
       (lambda (_request) (push 'preview order)))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt _choices &optional _help)
                   (push 'approval order)
                   (cons ?y "yes, allow once"))))
        (should
         (equal "written"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args)
                   (push 'execute order)
                   "written")
                 '("notes" "before" "after")
                 :context
                 '(:session-id "session-1"
                   :attendance attended))))
        (should (equal (nreverse order)
                       '(preview approval execute)))))))

(ert-deftest authority/buffer-edit-preview-renders-a-diff ()
  "The edit preview names its target buffer and presents a readable diff."
  (let ((name "*superchat-preview-test*")
        (preview "*Superchat edit_buffer Preview: *superchat-preview-test**"))
    (unwind-protect
        (progn
          (superchat-tool--preview-buffer-edit
           `(:tool-name "edit_buffer"
             :args (:buffer_name ,name :old_string "before\n" :new_string "after\n")))
          (with-current-buffer preview
            (should (string-match-p (regexp-quote name) (buffer-string)))
            (should (string-match-p "^-before" (buffer-string)))
            (should (string-match-p "^+after" (buffer-string)))))
      (when (get-buffer preview)
        (kill-buffer preview)))))

(ert-deftest authority/replace-buffer-preview-compares-or-discloses-target ()
  "Replacement previews diff a live buffer and disclose a missing target."
  (let* ((name "*superchat-replace-preview-test*")
         (preview (format "*Superchat replace_buffer Preview: %s*" name))
         (target (get-buffer-create name)))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "before\n"))
          (superchat-tool--preview-buffer-edit
           `(:tool-name "replace_buffer" :args (:buffer_name ,name :content "after\n")))
          (with-current-buffer preview
            (should (string-match-p "^-before" (buffer-string)))
            (should (string-match-p "^+after" (buffer-string))))
          (kill-buffer target)
          (superchat-tool--preview-buffer-edit
           `(:tool-name "replace_buffer" :args (:buffer_name ,name :content "new body")))
          (with-current-buffer preview
            (should (string-match-p "does not exist" (buffer-string)))
            (should (string-match-p "new body" (buffer-string)))))
      (dolist (buffer (list target (get-buffer preview)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest authority/malformed-buffer-preview-keeps-refusal-reachable ()
  "A malformed buffer preview falls back without blocking the y/n approval."
  (let ((request
         '(:tool-name "edit_buffer"
           :args (:buffer_name "*missing*" :old_string 42 :new_string "after")
           :effects ((:effect local-write :resource buffer))
           :request-hash "malformed-preview"))
        fallback-called approval-called)
    (should (eq (gethash "edit_buffer" superchat-authority-approval-previews)
                #'superchat-tool--preview-buffer-edit))
    (dolist (name '("shell-command" "eshell-command" "eval_elisp"))
      (should (eq (gethash name superchat-authority-approval-previews)
                  #'superchat-tool--preview-arguments)))
    (cl-letf (((symbol-function 'superchat-tool--preview-arguments)
               (lambda (_request) (setq fallback-called t)))
              ((symbol-function 'read-multiple-choice)
               (lambda (_prompt _choices &optional _help)
                 (setq approval-called t)
                 (cons ?n "no"))))
      (should-not (superchat-authority-confirm request))
      (should fallback-called)
      (should approval-called))))

(ert-deftest authority/permission-mode-explore-denies-side-effects ()
  "Explore is read-only: side effects are denied even under legacy allow."
  (let ((superchat-authority-mode 'explore))
    (should
     (eq 'deny
         (superchat-authority--effective-decision
          "edit_buffer" '("buf" "x")
          (superchat-authority-decide
           "edit_buffer" '("buf" "x") 'attended)
          nil)))
    (should-not
     (eq 'deny
         (superchat-authority--effective-decision
          "read_buffer" '("README.md")
          (superchat-authority-decide
           "read_buffer" '("README.md") 'attended)
          nil)))))

(ert-deftest authority/permission-mode-execute-allows-classified-effects ()
  "Execute trusts classified effects when policy does not tighten them."
  (let ((superchat-authority-mode 'execute))
    (cl-letf (((symbol-function 'superchat-authority--policy-decision)
               (lambda (&rest _args) 'allow)))
      (should
       (eq 'allow
           (superchat-authority--effective-decision
            "edit_buffer" '("buf" "before" "after")
            (superchat-authority-decide
             "edit_buffer" '("buf" "before" "after") 'attended)
            '(:guardrails (:confirm-destructive t))))))))

(ert-deftest authority/confirm-session-allowlist-skips-future-asks ()
  "The y/n/a answer grants the session; the next call allows silently."
  (authority-test--capture-events
    (let ((superchat-authority--session-allowlist nil)
          asks)
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt choices &optional _help)
                   (push choices asks)
                   (cons ?a "always allow this session"))))
        ;; `a' approves this call AND grants the session
        (should (superchat-authority-confirm
                 (list :tool-name "write-file"
                       :args '("a.txt" "x")
                       :effects '((:effect local-write))
                       :request-hash "h1")))
        (should (equal '("write-file")
                       superchat-authority--session-allowlist)))
      ;; the next decision for the same tool never asks
      (cl-letf (((symbol-function 'superchat--agent-confirm-p)
                 (lambda (&rest _args) t)))
        (should
         (eq 'allow
             (superchat-authority--effective-decision
              "write-file" '("a.txt" "y")
              (superchat-authority-decide
               "write-file" '("a.txt" "y") 'attended)
              '(:guardrails (:confirm-destructive t)))))
        ;; the grant also holds without guardrails (the direct-trigger
        ;; path): it honors the individual approval, not the rail state
        (should
         (eq 'allow
             (superchat-authority--effective-decision
              "write-file" '("a.txt" "z")
              (superchat-authority-decide
               "write-file" '("a.txt" "z") 'attended)
              nil)))))))

(ert-deftest authority/confirm-dangerous-tools-never-offer-always ()
  "Process-executing tools get y/n only, never the session grant."
  (authority-test--capture-events
    (let ((superchat-authority--session-allowlist nil)
          choices)
      (cl-letf (((symbol-function 'read-multiple-choice)
                 (lambda (_prompt cs &optional _help)
                   (setq choices cs)
                   (cons ?y "yes, allow once"))))
        (superchat-authority-confirm
         (list :tool-name "shell-command"
               :args '("rm -rf /tmp/x")
               :effects '((:effect process-exec))
               :request-hash "h2"))
        (should (= 2 (length choices)))
        (should-not (member ?a (mapcar #'car choices)))
        (should (null superchat-authority--session-allowlist))))))

(ert-deftest authority/effects-derived-confirmation-catches-list-drift ()
  "A local-write tool missing from the legacy name list still confirms."
  ;; ReplaceBuffer is absent from superchat-agent-destructive-tools but
  ;; carries a local-write effect: the effects-derived check must ask.
  (should-not (and (boundp (quote superchat-agent-destructive-tools))
                 (member "ReplaceBuffer" superchat-agent-destructive-tools)))
  (cl-letf (((symbol-function 'superchat--agent-confirm-p)
             (lambda (&rest _args) nil)))
    (should
     (eq 'ask
         (superchat-authority--effective-decision
          "ReplaceBuffer" '("buf" "x")
          (superchat-authority-decide
           "ReplaceBuffer" '("buf" "x") 'attended)
          '(:guardrails (:confirm-destructive t)))))))

(ert-deftest authority/default-policy-gates-reads-and-side-effects ()
  (should (eq 'ask
              (plist-get (superchat-authority-decide
                          "read_buffer" '("README.md") 'attended)
                         :decision)))
  (should (eq 'ask
              (plist-get (superchat-authority-decide
                          "edit_buffer" '("buf" "x") 'attended)
                         :decision)))
  (should (eq 'deny
              (plist-get (superchat-authority-decide
                          "edit_buffer" '("buf" "x") 'unattended)
                         :decision)))
  (should (eq 'deny
              (plist-get (superchat-authority-decide
                          "read_buffer" '("README.md") 'unattended)
                         :decision))))

(ert-deftest authority/unknown-attended-asks-and-unattended-denies ()
  (should (eq 'ask
              (plist-get (superchat-authority-decide
                          "mcp.unknown/do" nil 'attended)
                         :decision)))
  (should (eq 'deny
              (plist-get (superchat-authority-decide
                          "mcp.unknown/do" nil 'unattended)
                         :decision))))

(ert-deftest authority/approval-hash-is-canonical-and-policy-bound ()
  (let* ((args-a '(:b 2 :a 1))
         (args-b '(:a 1 :b 2))
         (effects-a '((:resource file :effect local-write :scope "x")))
         (effects-b '((:scope "x" :effect local-write :resource file)))
         (hash-a (superchat-authority-approval-hash "write-file" args-a effects-a))
         (hash-b (superchat-authority-approval-hash "write-file" args-b effects-b)))
    (should (equal hash-a hash-b))
    (should-not (equal hash-a
                       (superchat-authority-approval-hash
                        "write-file" '(:a 1 :b 3) effects-a)))
    (should-not (equal hash-a
                       (superchat-authority-approval-hash
                        "write-file" args-a
                        '((:effect local-write :resource file :scope "y")))))
    (should-not (equal hash-a
                       (superchat-authority-approval-hash
                        "write-file" args-a effects-a
                        (superchat-authority-policy-version-for 'observer))))
    (should-not (equal hash-a
                       (superchat-authority-approval-hash
                        "write-file" args-a effects-a "authority-v2")))))

(defmacro authority-test--capture-events (&rest body)
  "Run BODY while collecting appended events in `events'."
  (declare (indent 0))
  `(let ((superchat-authority-approval-function (lambda (_request) t))
         events)
     (cl-letf (((symbol-function 'superchat-tape-append-event)
                (lambda (event)
                  (setq events (append events (list event)))
                  (length events)))
               ((symbol-function 'superchat--agent-intercept-tool-call)
                (lambda (_tool args) (list :args args)))
               ((symbol-function 'superchat--agent-check-permission)
                (lambda (_tool _args) 'defer))
               ((symbol-function 'superchat--agent-confirm-p)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--agent-run-tool-hooks)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--agent-filter-tool-result)
                (lambda (_tool _args result) result)))
       ,@body)))

(ert-deftest authority/approval-binds-the-run-transport-timer-owner ()
  "An approval callback can extend only the asking Run's transport wait."
  (authority-test--capture-events
    (let* ((captured-owner nil)
           (superchat-authority-approval-function
            (lambda (_request)
              (setq captured-owner
                    (superchat-authority-transport-timer-owner))
              t))
           (superchat-authority--session-allowlist nil)
           (superchat-authority-mode 'ask))
      (cl-letf (((symbol-function
                  'superchat-runtime-transport-timer-owner)
                 (lambda (run-id)
                   (and (equal run-id "run-one") 'owner-one))))
        (should
         (equal "written"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args) "written")
                 '("notes" "before" "after")
                 :context
                 '(:session-id "session-1"
                   :run-id "run-one"
                   :attendance attended))))
        (should (eq captured-owner 'owner-one))
        (should
         (eq (superchat-authority-transport-timer-owner
              '(:run-id "released-run"))
             superchat-authority-transport-timer-owner-unavailable))
        (should-not
         (superchat-authority-transport-timer-owner
          '(:session-id "non-runtime")))))))

(ert-deftest authority/unknown-policy-errors-before-execution ()
  (authority-test--capture-events
    (let ((called nil))
      (should-error
       (superchat-tool-execute
        "read-file"
        (lambda (&rest _args) (setq called t))
        '("README.md")
        :context '(:session-id "session-1"
                   :attendance attended
                   :authority-policy admin)))
      (should-not called)
      (should-not events))))

(ert-deftest authority/observer-denies-non-read-effects ()
  (authority-test--capture-events
    (let ((called nil))
      (cl-letf (((symbol-function 'superchat--agent-check-permission)
                 (lambda (_tool _args) 'allow)))
        (should
         (equal "[Tool call denied by authority policy]"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args) (setq called t))
                 '("out.txt" "body")
                 :context
                 '(:session-id "session-1"
                   :attendance attended
                   :authority-policy observer)))))
      (should-not called))))

(ert-deftest authority/profile-toolset-does-not-grant-authority ()
  "Exposing a Tool through a Profile never authorizes its Effects."
  (authority-test--capture-events
    (let ((profile
           (superchat-preset-from-plist
            '(:name coder :profile-id profile.coder :type agent
              :capability coding :tools ("write-file"))))
          called)
      (should (member "write-file" (superchat-preset-tools profile)))
      (cl-letf (((symbol-function 'superchat--agent-check-permission)
                 (lambda (_tool _args) 'allow)))
        (should
         (equal "[Tool call denied by authority policy]"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args) (setq called t))
                 '("out.txt" "body")
                 :context
                 '(:session-id "session-1"
                   :attendance attended
                   :authority-policy observer)))))
      (should-not called))))

(ert-deftest authority/developer-project-keeps-file-effects-inside-root ()
  (authority-test--capture-events
    (let* ((root (file-name-as-directory
                  (make-temp-file "superchat-authority-root" t)))
           (inside (expand-file-name "inside.txt" root))
           (outside (expand-file-name "outside.txt" temporary-file-directory))
           (superchat-tools-default-directory root)
           (superchat-authority-effect-resolvers
            (make-hash-table :test 'equal))
           (called nil))
      (unwind-protect
          (progn
            (superchat-authority-register-effect-resolver
             "project-file-write"
             (lambda (args)
               `((:effect local-write :resource file
                  :scope ,(superchat-authority-effect-argument
                           args "path" 0)))))
            (should
             (equal "ok"
                    (superchat-tool-execute
                     "project-file-write"
                     (lambda (&rest _args) (setq called t) "ok")
                     (list inside "body")
                     :context
                     `(:session-id "session-1"
                       :attendance attended
                       :authority-policy developer-project
                       :workspace-root ,root))))
            (should called)
            (setq called nil)
            (should
             (equal "ok"
                    (superchat-tool-execute
                     "project-file-write"
                     (lambda (&rest _args) (setq called t) "ok")
                     '("relative.txt" "body")
                     :context
                     `(:session-id "session-1"
                       :attendance attended
                       :authority-policy developer-project
                       :workspace-root ,root))))
            (should called)
            (setq called nil)
            (should
             (equal "[Tool call denied by authority policy]"
                    (superchat-tool-execute
                     "project-file-write"
                     (lambda (&rest _args) (setq called t) "ok")
                     (list outside "body")
                     :context
                     `(:session-id "session-1"
                       :attendance attended
                       :authority-policy developer-project
                       :workspace-root ,root))))
            (should-not called)
            (should
             (equal "[Tool call denied by authority policy]"
                    (superchat-tool-execute
                     "project-file-write"
                     (lambda (&rest _args) (setq called t) "ok")
                     (list inside "body")
                     :context
                     '(:session-id "session-1"
                       :attendance attended
                       :authority-policy developer-project))))
            (should-not called))
        (delete-directory root t)))))

(ert-deftest authority/executor-permission-hook-cannot-change-execution-request ()
  (authority-test--capture-events
    (let* ((root (file-name-as-directory
                  (make-temp-file "superchat-authority-root" t)))
           (inside (expand-file-name "inside.txt" root))
           called-args)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat--agent-check-permission)
                     (lambda (_tool args)
                       (setcar args "/tmp/outside.txt")
                       'allow)))
            (should
             (equal "written"
                    (superchat-tool-execute
                     "write-file"
                     (lambda (&rest args) (setq called-args args) "written")
                     (list inside "body")
                     :context
                     `(:session-id "session-1"
                       :attendance attended
                       :authority-policy developer-project
                       :workspace-root ,root))))
            (should (equal called-args (list inside "body"))))
        (delete-directory root t)))))

(ert-deftest authority/executor-approves-read-once ()
  (authority-test--capture-events
    (let* ((called 0)
           (approvals 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (should
       (equal "ok"
              (superchat-tool-execute
               "read-file"
               (lambda (_path) (cl-incf called) "ok")
               '("README.md")
               :context
               '(:session-id "session-1"
                 :attendance attended
                 :guardrails (:confirm-destructive nil)))))
      (should (= called 1))
      (should (= approvals 1))
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested"
                "approval.requested"
                "approval.decided"
                "tool.execution.started"
                "tool.completed"))))))

(ert-deftest authority/executor-deny-never-invokes-tool ()
  (authority-test--capture-events
    (let ((called nil))
      (cl-letf (((symbol-function 'superchat--agent-check-permission)
                 (lambda (_tool _args) 'deny)))
        (should
         (equal "[Tool call denied by authority policy]"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args) (setq called t))
                 '("out.txt" "body")
                 :context '(:session-id "session-1" :attendance attended)))))
      (should-not called)
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested" "tool.failed"))))))

(ert-deftest authority/blocked-experience-view-denies-declared-dependent-tool ()
  "A blocked View denies only tools explicitly dependent on it."
  (authority-test--capture-events
    (let ((called nil)
          (context
           '(:session-id "session-1"
             :attendance attended
             :context-envelope
             (:experience-view (:status blocked)
              :experience-action-policy
              (:dependent-tools ("minimal_refresh"))))))
      (should
       (equal
        "[Tool call blocked: Experience View is blocked; dependent action minimal_refresh is not authorized]"
        (superchat-tool-execute
         "minimal_refresh"
         (lambda (&rest _args) (setq called t) "unsafe")
         '("E-R17")
         :context context)))
      (should-not called)
      (let* ((requested (car events))
             (failed (car (last events)))
             (requested-payload (plist-get requested :payload))
             (failed-payload (plist-get failed :payload)))
        (should (equal "deny" (plist-get requested-payload :decision)))
        (should
         (equal
          "Experience View is blocked; dependent action minimal_refresh is not authorized"
          (plist-get requested-payload :block-reason)))
        (should (equal "blocked" (plist-get failed-payload :status)))
        (should
         (equal
          "Experience View is blocked; dependent action minimal_refresh is not authorized"
          (plist-get failed-payload :block-reason)))))))

(ert-deftest authority/blocked-experience-view-leaves-unrelated-tool-alone ()
  "A blocked View does not become a blanket tool ban."
  (authority-test--capture-events
    (should
     (equal "ok"
            (superchat-tool-execute
             "minimal_uncertain"
             (lambda (&rest _args) "ok")
             '("E-R17")
             :context
             '(:session-id "session-1"
               :attendance attended
               :context-envelope
               (:experience-view (:status blocked)
                :experience-action-policy
                (:dependent-tools ("minimal_refresh")))))))))

(ert-deftest authority/revision-recovery-policy-blocks-only-dependent-read ()
  "Awaiting workspace_info blocks repeat reads but not information tools."
  (authority-test--capture-events
    (let ((read-called nil)
          (info-called nil)
          (policy '(:kind "workspace-revision-recovery"
                    :phase "awaiting-info"
                    :required-tool "workspace_info"
                    :dependent-tools ("workspace_read")))
          completed)
      (let ((context (list :session-id "session-1"
                           :attendance 'attended
                           :context-envelope
                           (list :experience-view '(:status satisfied)
                                 :experience-action-policy policy))))
        (should
         (equal
          "[Tool call blocked: Experience recovery requires workspace_info; dependent action workspace_read is not authorized]"
          (superchat-tool-execute
           "workspace_read"
           (lambda (&rest _args) (setq read-called t) "should-not-run")
           '("artifact-1")
           :context context))))
      (should-not read-called)
      (let ((context (list :session-id "session-1"
                           :attendance 'attended
                           :context-envelope
                           (list :experience-view '(:status satisfied)
                                 :experience-action-policy policy)
                           :tool-completed-function
                           (lambda (value) (setq completed value)))))
        (should
         (equal "workspace-info"
                (superchat-tool-execute
                 "workspace_info"
                 (lambda () (setq info-called t) "workspace-info")
                 nil :context context))))
      (should info-called)
      (should (equal "workspace_info" (plist-get completed :tool-name)))
      (should (equal "tool.completed"
                     (plist-get (plist-get completed :event) :type))))))

(ert-deftest authority/executor-binds-approval-to-normalized-request ()
  (authority-test--capture-events
    (let (approved-request called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest args) (setq called-args args) "written")
               '("notes" "before" "after")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :execution-id "execution-1"
                 :approval-function
                 ,(lambda (request)
                    (setq approved-request request)
                    t)))))
      (should (equal called-args '("notes" "before" "after")))
      (should
       (equal (plist-get approved-request :request-hash)
              (superchat-authority-approval-hash
               "edit_buffer" '("notes" "before" "after")
               '((:effect local-write :resource buffer :scope "notes"))
               (superchat-authority-policy-version-for 'default)
               "execution-1")))
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested"
                "approval.requested"
                "approval.decided"
                "tool.execution.started"
                "tool.completed"))))))

(ert-deftest authority/tool-events-form-logical-causation-chain ()
  (authority-test--capture-events
    (should
     (equal "done"
            (superchat-tool-execute
             "write-file" (lambda (&rest _args) "done")
             '("out.txt" "body")
             :context
             '(:session-id "session-1"
               :attendance attended
               :causation-id "evt_run"
               :approval-function always))))
    (should
     (equal '("tool.requested" "approval.requested" "approval.decided"
              "tool.execution.started" "tool.completed")
            (mapcar (lambda (event) (plist-get event :type)) events)))
    (let ((previous "evt_run"))
      (dolist (event events)
        (let ((event-id (plist-get event :event-id)))
          (should (string-prefix-p "evt_" event-id))
          (should (equal previous (plist-get event :causation-id)))
          (setq previous event-id))))))

(ert-deftest authority/evidence-tool-chain-starts-at-origin-snapshot ()
  "Exact args/results retain attempt identity through the Authority chain."
  (authority-test--capture-events
    (should
     (equal "contents"
            (superchat-tool-execute
             "read-file" (lambda (_path) "contents") '("README.md")
             :context
             '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
               :attendance attended
               :evidence-contract "epistemic-learning-v0"
               :evidence-attempt 1
               :decision-snapshot-event-id "evt_snapshot_1"
               :tool-call-id "call-1"
               :tool-call-name "read-file"
               :tool-call-ordinal 1
               :tool-call-arguments ((path . "README.md"))
               :evidence-origin-validator always))))
    (let* ((requested (car events))
           (completed (car (last events)))
           (request-payload (plist-get requested :payload))
           (result-payload (plist-get completed :payload)))
      (should (equal "tool.requested" (plist-get requested :type)))
      (should (equal "evt_snapshot_1"
                     (plist-get requested :causation-id)))
      (should (= 1 (plist-get request-payload :evidence-attempt)))
      (should (equal "call-1" (plist-get request-payload :tool-call-id)))
      (should (string-match-p "README.md"
                              (plist-get request-payload :arguments-json)))
      (should (equal "tool.completed" (plist-get completed :type)))
      (should (eq 'restricted (plist-get requested :sensitivity)))
      (should (eq 'restricted (plist-get completed :sensitivity)))
      (should (= 1 (plist-get result-payload :evidence-attempt)))
      (should (string-match-p "contents"
                              (plist-get result-payload :result-json))))))

(ert-deftest authority/evidence-sensitive-arguments-fail-before-action ()
  "Sensitive named arguments never reach Tape policy, approval, or the tool."
  (authority-test--capture-events
    (let (called approved audit-error)
      (should
       (string-match-p
        "audit failed"
        (superchat-tool-execute
         "lookup" (lambda (_secret) (setq called t)) '("secret")
         :context
         `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
           :attendance attended
           :evidence-contract "epistemic-learning-v0"
           :evidence-attempt 1
           :decision-snapshot-event-id "evt_snapshot_1"
           :tool-call-id "call-1"
           :tool-call-name "lookup"
           :tool-call-ordinal 1
           :tool-call-arguments ((api_key . "secret"))
           :evidence-origin-validator always
           :approval-function ,(lambda (_request) (setq approved t) t)
           :audit-failure-function
           ,(lambda (err _result) (setq audit-error err))))))
      (should audit-error)
      (should-not called)
      (should-not approved)
      (should-not events))))

(ert-deftest authority/evidence-rewrite-cannot-change-executed-arguments ()
  "Eligible tools reject argument rewrites before policy or effects."
  (authority-test--capture-events
    (let (called approved resolved audit-error)
      (cl-letf (((symbol-function 'superchat--agent-intercept-tool-call)
                 (lambda (_tool _args)
                   '(:args ((:api_key "secret"))))))
        (should
         (string-match-p
          "audit failed"
          (superchat-tool-execute
           "lookup" (lambda (&rest _args) (setq called t)) '("alpha")
           :context
           `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
             :attendance attended
             :evidence-contract "epistemic-learning-v0"
             :evidence-attempt 1
             :decision-snapshot-event-id "evt_snapshot_1"
             :tool-call-id "call-1"
             :tool-call-name "lookup"
             :tool-call-ordinal 1
             :tool-call-arguments ((key . "alpha"))
             :evidence-origin-validator always
             :resolve-effects
             ,(lambda (_args) (setq resolved t) nil)
             :approval-function
             ,(lambda (_request) (setq approved t) t)
             :audit-failure-function
             ,(lambda (err _result) (setq audit-error err))))))
      (should audit-error)
      (should-not called)
      (should-not resolved)
      (should-not approved)
      (should-not events)))))

(ert-deftest authority/evidence-origin-requires-call-identity ()
  "Eligible Action origin needs a nonempty call ID and positive ordinal."
  (let ((base '(:evidence-contract "epistemic-learning-v0"
                :evidence-attempt 1
                :decision-snapshot-event-id "evt_snapshot_1"
                :tool-call-id "call-1"
                :tool-call-name "lookup"
                :tool-call-ordinal 1
                :tool-call-arguments ((key . "alpha"))
                :evidence-origin-validator always)))
    (dolist (context
             (list (plist-put (copy-sequence base) :tool-call-id "")
                   (plist-put (copy-sequence base) :tool-call-ordinal 0)))
      (should-error
       (superchat-authority--validated-evidence-action
        "lookup" '("alpha") context)))))

(ert-deftest authority/evidence-error-retains-signal-and-data ()
  "Exact failure evidence records the original signal before formatting it."
  (authority-test--capture-events
    (should
     (string-match-p
      "disk unavailable"
      (superchat-tool-execute
       "read-file"
       (lambda (_path) (signal 'file-error '("disk unavailable" "EIO")))
       '("README.md")
       :context
       '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
         :attendance attended
         :evidence-contract "epistemic-learning-v0"
         :evidence-attempt 1
         :decision-snapshot-event-id "evt_snapshot_1"
         :tool-call-id "call-1"
         :tool-call-name "read-file"
         :tool-call-ordinal 1
         :tool-call-arguments ((path . "README.md"))
         :evidence-origin-validator always))))
    (let* ((failed (car (last events)))
           (payload (plist-get failed :payload))
           (json (plist-get payload :error-json)))
      (should (equal "tool.failed" (plist-get failed :type)))
      (should (eq 'restricted (plist-get failed :sensitivity)))
      (should (string-match-p "file-error" json))
      (should (string-match-p "disk unavailable" json))
      (should (string-match-p "EIO" json)))))

(ert-deftest authority/ordinary-error-records-only-its-type ()
  "Ordinary failure history exposes a classifier without leaking details."
  (authority-test--capture-events
    (superchat-tool-execute
     "workspace_read"
     (lambda ()
       (signal 'superchat-workspace-artifact-revision-error
               '("secret revision detail")))
     nil
     :context
     '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
       :attendance attended))
    (let* ((failed (car (last events)))
           (payload (plist-get failed :payload)))
      (should (equal "tool.failed" (plist-get failed :type)))
      (should (equal "workspace_read" (plist-get payload :tool-name)))
      (should (equal "superchat-workspace-artifact-revision-error"
                     (plist-get payload :error-type)))
      (should-not (plist-member payload :error-json))
      (should-not (string-match-p "secret revision detail"
                                  (format "%S" failed))))))

(ert-deftest authority/evidence-sensitive-error-data-is-not-flattened ()
  "A secret-valued plist in signal data fails exact error audit."
  (authority-test--capture-events
    (let (delivered audit-error audit-result result)
      (setq
       result
       (superchat-tool-execute
        "lookup" (lambda (_key) (signal 'error '(:api_key "secret")))
        '("alpha")
        :context
        `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
          :attendance attended
          :evidence-contract "epistemic-learning-v0"
          :evidence-attempt 1
          :decision-snapshot-event-id "evt_snapshot_1"
          :tool-call-id "call-1"
          :tool-call-name "lookup"
          :tool-call-ordinal 1
          :tool-call-arguments ((key . "alpha"))
          :evidence-origin-validator always
          :audit-failure-function
          ,(lambda (err value)
             (setq audit-error err audit-result value))
          :delivery-function
          ,(lambda (value) (setq delivered value)))))
      (should audit-error)
      (should-not (string-match-p "secret" result))
      (should-not (string-match-p "secret" (format "%S" audit-result)))
      (should-not delivered)
      (should-not
       (cl-find "tool.failed" events
                :key (lambda (event) (plist-get event :type))
                :test #'equal)))))

(ert-deftest authority/executor-approval-cannot-change-execution-request ()
  (authority-test--capture-events
    (let ((path (copy-sequence "safe.txt"))
          called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest args) (setq called-args args) "written")
               (list path "body")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :approval-function
                 ,(lambda (request)
                    (aset (car (plist-get request :args)) 0 ?X)
                    t)))))
      (should (equal called-args '("safe.txt" "body"))))))

(ert-deftest authority/executor-detaches-request-from-caller-owned-args ()
  (authority-test--capture-events
    (let ((path (copy-sequence "safe.txt"))
          called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest args) (setq called-args args) "written")
               (list path "body")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :approval-function
                 ,(lambda (_request)
                    (aset path 0 ?X)
                    t)))))
      (should (equal called-args '("safe.txt" "body"))))))

(ert-deftest authority/executor-validator-cannot-change-execution-request ()
  (authority-test--capture-events
    (let (called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest args) (setq called-args args) "written")
               '("safe.txt" "body")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :validate
                 ,(lambda (args)
                    (setcar args "mutated.txt"))))))
      (should (equal called-args '("safe.txt" "body"))))))

(ert-deftest authority/executor-effect-resolver-cannot-change-execution-request ()
  (authority-test--capture-events
    (let (called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest args) (setq called-args args) "written")
               '("safe.txt" "body")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :resolve-effects
                 ,(lambda (args)
                    (setcar args "mutated.txt")
                    '((:effect local-write
                       :resource file
                       :scope "safe.txt")))))))
      (should (equal called-args '("safe.txt" "body"))))))

(ert-deftest authority/executor-resolves-effects-once ()
  (authority-test--capture-events
    (let ((superchat-authority-effect-resolvers
           (make-hash-table :test 'equal))
          (calls 0))
      (superchat-authority-register-effect-resolver
       "custom"
       (lambda (_args)
         (cl-incf calls)
         '((:effect local-read :resource custom))))
      (should
       (equal "done"
              (superchat-tool-execute
               "custom" (lambda () "done") nil
               :context
               '(:session-id "session-1"
                 :attendance attended
                 :approval-function always))))
      (should (= 1 calls)))))

(ert-deftest authority/executor-guardrail-cannot-change-execution-request ()
  (authority-test--capture-events
    (let (called-args)
      (cl-letf (((symbol-function 'superchat--agent-confirm-p)
                 (lambda (_tool args _guardrails)
                   (setcar args "mutated.txt")
                   nil)))
        (should
         (equal "written"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest args) (setq called-args args) "written")
                 '("safe.txt" "body")
                 :context
                 '(:session-id "session-1"
                   :attendance attended
                   :guardrails (:confirm-destructive t)))))
        (should (equal called-args '("safe.txt" "body")))))))

(ert-deftest authority/executor-pre-tool-hook-cannot-change-execution-request ()
  (authority-test--capture-events
    (let ((superchat-agent-pre-tool-functions
           (list (lambda (_tool args)
                   (setcar args "mutated.txt"))))
          called-args)
      (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (fn hooks)
                     (apply fn args)))))
        (should
         (equal "written"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest args) (setq called-args args) "written")
                 '("safe.txt" "body")
                 :context
                 '(:session-id "session-1"
                   :attendance attended))))
        (should (equal called-args '("safe.txt" "body")))))))

(ert-deftest authority/executor-declined-approval-never-invokes-tool ()
  (authority-test--capture-events
    (let ((called nil))
      (should
       (equal "[Tool call cancelled by user]"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest _args) (setq called t))
               '("out.txt" "body")
               :context
               '(:session-id "session-1"
                 :attendance attended
                 :approval-function ignore))))
      (should-not called)
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested"
                "approval.requested"
                "approval.decided"
                "tool.failed"))))))

(ert-deftest authority/approval-quit-has-the-same-durable-accounting-as-denial ()
  "Allow, deny, and C-g at approval leave one complete Authority outcome."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-approval-quit-" t))
         (superchat-data-directory directory)
         (superchat-authority-mode 'ask)
         (superchat-authority--session-allowlist nil))
    (unwind-protect
        (progn
          (superchat-db-close)
          (superchat-db-open)
          (dolist (mode '(allow deny quit))
            (let* ((session-id (format "approval-quit-%s" mode))
                   (called nil)
                   (interrupted nil)
                   (approval
                    (lambda (_request)
                      (pcase mode
                        ('allow t)
                        ('deny nil)
                        ('quit (signal 'quit '(approval-stop))))))
                   result caught)
              (condition-case condition
                  (setq result
                        (superchat-tool-execute
                         "approval-quit-test"
                         (lambda (&rest _args)
                           (setq called t)
                           "implemented")
                         '("target")
                         :context
                         (list :session-id session-id :attendance 'attended
                               ;; S03b: `superchat-tape-append-event' now
                               ;; requires non-empty `:run-id'/`:turn-id' for
                               ;; run-scoped types such as `tool.requested'.
                               :run-id session-id :turn-id session-id
                               :resolve-effects
                               (lambda (_args)
                                 '((:effect external-write
                                    :resource test :scope "approval-quit")))
                               :approval-function approval
                               :interrupt-function
                               (lambda () (setq interrupted t)))))
                (quit (setq caught condition)))
              (let* ((events (superchat-tape-replay-events session-id))
                     (types (mapcar (lambda (event) (plist-get event :type))
                                    events))
                     (decision
                      (cl-find "approval.decided" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal))
                     (failure
                      (cl-find "tool.failed" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal)))
                (pcase mode
                  ('allow
                   (should (equal "implemented" result))
                   (should called)
                   (should-not caught)
                   (should-not interrupted)
                   (should (equal '("tool.requested" "approval.requested"
                                    "approval.decided" "tool.execution.started"
                                    "tool.completed")
                                  types)))
                  ('deny
                   (should (equal "[Tool call cancelled by user]" result))
                   (should-not called)
                   (should-not caught)
                   (should-not interrupted))
                  ('quit
                   (should (equal '(quit approval-stop) caught))
                   (should-not called)
                   (should interrupted)))
                (unless (eq mode 'allow)
                  (should (equal '("tool.requested" "approval.requested"
                                   "approval.decided" "tool.failed")
                                 types))
                  (should (equal "cancelled"
                                 (plist-get (plist-get failure :payload)
                                            :status))))
                (should (equal (if (eq mode 'allow) "allow" "deny")
                               (plist-get (plist-get decision :payload)
                                          :decision)))
                (when (eq mode 'quit)
                  (should (equal "quit"
                                 (plist-get (plist-get decision :payload)
                                            :reason)))
                  (should (equal "user-quit"
                                 (plist-get (plist-get failure :payload)
                                            :interruption)))
                  (should (equal "approval"
                                 (plist-get (plist-get failure :payload)
                                            :phase))))))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest authority/executor-does-not-run-after-runtime-termination ()
  "A terminal-first call is refused before execution-audit admission."
  (authority-test--capture-events
    (let ((called nil)
          (context '(:session-id "session-1"
                     :run-id "run-1"
                     :attendance attended
                     :run-active-p (lambda () nil))))
      (should
       (equal "[Tool call blocked: Run is no longer active]"
              (superchat-tool-execute
               "edit_buffer"
               (lambda (&rest _args) (setq called t) "written")
               '("notes" "body")
               :context context)))
      (should-not called)
      (should-not events))))

(ert-deftest authority/late-admission-keeps-structural-and-predicate-boundaries ()
  "Early refusal does not waive argument shape or legacy predicate rules."
  (let ((context '(:session-id "s" :attendance attended :run-active-p nil)))
    (should-error (superchat-tool-execute "x" nil nil :context context))
    (should-error (superchat-tool-execute "x" #'ignore 3 :context context))
    (should-error (superchat-tool-execute "x" #'ignore nil :async t :context context))
    (should-error (superchat-tool-execute "x" #'ignore nil :context '(:attendance attended)))
    (should-error (superchat-tool-execute "x" #'ignore nil :context '(:session-id "s"))))
  (should (superchat-authority--run-active-p '(:session-id "legacy")))
  (dolist (predicate (list nil 1 (lambda () (error "predicate failure"))))
    (authority-test--capture-events
      (let ((calls 0) (effects 0))
        (should-not
         (superchat-tool-execute
          "x" (lambda (&rest _) (cl-incf effects)) nil :async t
          :callback (lambda (result)
                      (cl-incf calls) (should (string-match-p "Run is no longer active" result)))
          :context (list :session-id "s" :attendance 'attended :run-active-p predicate)))
        (should (= 1 calls)) (should (= 0 effects)) (should-not events)))))

(ert-deftest authority/late-admission-first-audit-failure-still-blocks ()
  "An active call cannot execute when its first audit append fails."
  (let ((effects 0) (attempts 0))
    (cl-letf (((symbol-function 'superchat-tape-append-event-record)
               (lambda (_event) (cl-incf attempts) (error "first audit unavailable"))))
      (should (string-match-p
               "audit failed"
               (superchat-tool-execute
                "read_buffer" (lambda (&rest _) (cl-incf effects)) '("fixture")
                :context '(:session-id "s" :attendance attended :run-active-p (lambda () t)))))
      (should (= 1 attempts)) (should (= 0 effects)))))

(ert-deftest authority/executor-prewrite-failure-prevents-side-effect ()
  (let ((called nil)
        (pre-hook-called nil))
    (let ((superchat-agent-pre-tool-functions
           (list (lambda (&rest _args) (setq pre-hook-called t)))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (when (equal (plist-get event :type)
                                "tool.execution.started")
                     (error "database unavailable"))
                   1))
                ((symbol-function 'superchat--agent-intercept-tool-call)
                 (lambda (_tool args) (list :args args)))
                ((symbol-function 'superchat--agent-check-permission)
                 (lambda (_tool _args) 'allow))
                ((symbol-function 'superchat--agent-confirm-p)
                 (lambda (&rest _args) nil)))
        (should
         (string-match-p
          "audit failed"
          (superchat-tool-execute
           "write-file"
           (lambda (&rest _args) (setq called t))
           '("out.txt" "body")
           :context '(:session-id "session-1" :attendance attended))))
        (should-not called)
        (should-not pre-hook-called)))))

(ert-deftest authority/executor-async-uses-the-same-gates ()
  (authority-test--capture-events
    (let (delivered called-args)
      (should-not
       (superchat-tool-execute
        "read-file"
        (lambda (callback &rest args)
          (setq called-args args)
          (funcall callback "async-result"))
        '("README.md")
        :async t
        :callback (lambda (result) (setq delivered result))
        :context '(:session-id "session-1" :attendance attended)))
      (should (equal called-args '("README.md")))
      (should (equal delivered "async-result"))
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested"
                "approval.requested"
                "approval.decided"
                "tool.execution.started"
                "tool.completed"))))))

(ert-deftest authority/pre-hook-quit-is-audited-before-original-propagates ()
  "A pre-hook quit records phase and never reaches the implementation."
  (authority-test--capture-events
    (let ((superchat-agent-pre-tool-functions '(test-authority--quit-pre))
          called interrupted caught)
      (cl-letf (((symbol-function 'test-authority--quit-pre)
                 (lambda (&rest _args) (signal 'quit '(pre-stop))))
                ((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (hook hooks) (apply hook args)))))
        (condition-case condition
            (superchat-tool-execute
             "read-file" (lambda (&rest _) (setq called t) "bad")
             '("README.md")
             :context
             (list :session-id "session-1" :attendance 'attended
                   :interrupt-function
                   (lambda () (setq interrupted t))))
          (quit (setq caught condition))))
      (should (equal '(quit pre-stop) caught))
      (should-not called)
      (should interrupted)
      (let* ((failed (car (last events)))
             (payload (plist-get failed :payload)))
        (should (equal "tool.failed" (plist-get failed :type)))
        (should (equal "user-quit" (plist-get payload :interruption)))
        (should (equal "pre" (plist-get payload :phase)))))))

(ert-deftest authority/filter-quit-records-receipt-not-final-fingerprint ()
  "A filter quit audits result-processing without claiming final delivery."
  (authority-test--capture-events
    (let (caught interrupted)
      (cl-letf (((symbol-function 'superchat--agent-filter-tool-result)
                 (lambda (&rest _args) (signal 'quit '(filter-stop)))))
        (condition-case condition
            (superchat-tool-execute
             "read-file" (lambda (&rest _) '(:value "returned"))
             '("README.md")
             :context
             (list :session-id "session-1" :attendance 'attended
                   :interrupt-function
                   (lambda () (setq interrupted t))))
          (quit (setq caught condition))))
      (should (equal '(quit filter-stop) caught))
      (should interrupted)
      (let* ((failed (car (last events)))
             (payload (plist-get failed :payload)))
        (should (equal "tool.failed" (plist-get failed :type)))
        (should (equal "result-processing" (plist-get payload :phase)))
        (should (plist-get payload :implementation-result-hash))
        (should-not (plist-member payload :result-hash))))))

(ert-deftest authority/filter-quit-retains-a-nil-implementation-receipt ()
  "A nil return is still a present implementation receipt."
  (authority-test--capture-events
    (cl-letf (((symbol-function 'superchat--agent-filter-tool-result)
               (lambda (&rest _) (signal 'quit '(nil-filter-stop)))))
      (condition-case nil
          (superchat-tool-execute
           "read-file" (lambda (&rest _) nil) '("README.md")
           :context '(:session-id "session-1" :attendance attended))
        (quit nil)))
    (let ((payload (plist-get (car (last events)) :payload)))
      (should (plist-member payload :implementation-result-hash))
      (should (plist-member payload :implementation-result-size)))))

(ert-deftest authority/implementation-quit-is-partial-and-preserves-condition ()
  "Implementation quit records uncertainty and cleanup cannot replace it."
  (authority-test--capture-events
    (let (caught recovery-called)
      (condition-case condition
          (superchat-tool-execute
           "read-file" (lambda (&rest _) (signal 'quit '(implementation-stop)))
           '("README.md")
           :context
           (list :session-id "session-1" :attendance 'attended
                 :recovery-failure-function
                 (lambda (&rest _) (setq recovery-called t))
                 :interrupt-function (lambda () (signal 'quit '(cleanup-stop)))))
        (quit (setq caught condition)))
      (should (equal '(quit implementation-stop) caught))
      (should-not recovery-called)
      (let ((payload (plist-get (car (last events)) :payload)))
        (should (equal "implementation" (plist-get payload :phase)))
        (should (eq t (plist-get payload :effects-may-be-partial)))))))

(ert-deftest authority/ordinary-success-freezes-before-post-notification ()
  "Ordinary post hooks cannot mutate the durable or delivered result."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-functions '(test-authority--mutate-post)))
      (cl-letf (((symbol-function 'test-authority--mutate-post)
                 (lambda (_tool _args result)
                   (plist-put result :value "mutated")
                   'ignored))
                ((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (hook hooks) (apply hook args)))))
        (let* ((result
                (superchat-tool-execute
                 "read-file" (lambda (&rest _) '(:value "frozen"))
                 '("README.md")
                 :context '(:session-id "session-1" :attendance attended)))
               (completed (car (last events)))
               (payload (plist-get completed :payload)))
          (should (equal '(:value "frozen") result))
          (should
           (equal (plist-get payload :result-hash)
                  (plist-get
                   (superchat-authority--result-fingerprint result)
                   :result-hash))))))))

(ert-deftest authority/post-notification-quit-does-not-add-second-outcome ()
  "A quit after durable success interrupts but cannot contradict success."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-functions '(test-authority--quit-post))
          caught interrupted)
      (cl-letf (((symbol-function 'test-authority--quit-post)
                 (lambda (&rest _) (signal 'quit '(post-stop))))
                ((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (hook hooks) (apply hook args)))))
        (condition-case condition
            (superchat-tool-execute
             "read-file" (lambda (&rest _) "done") '("README.md")
             :context
             (list :session-id "session-1" :attendance 'attended
                   :interrupt-function (lambda () (setq interrupted t))))
          (quit (setq caught condition))))
      (should (equal '(quit post-stop) caught))
      (should interrupted)
      (should (equal 1 (cl-count "tool.completed" events
                                 :key (lambda (e) (plist-get e :type))
                                 :test #'equal)))
      (should-not (cl-find "tool.failed" events
                           :key (lambda (e) (plist-get e :type))
                           :test #'equal)))))

(ert-deftest authority/delayed-async-filter-quit-preserves-original-condition ()
  "A callback invoked after execute returns still audits and re-signals quit."
  (authority-test--capture-events
    (let (tool-callback caught interrupted)
      (cl-letf (((symbol-function 'superchat--agent-filter-tool-result)
                 (lambda (&rest _) (signal 'quit '(late-filter-stop)))))
        (superchat-tool-execute
         "read-file" (lambda (callback &rest _) (setq tool-callback callback))
         '("README.md") :async t :callback #'ignore
         :context
         (list :session-id "session-1" :attendance 'attended
               :interrupt-function (lambda () (setq interrupted t))))
        (condition-case condition
            (funcall tool-callback "late-result")
          (quit (setq caught condition))))
      (should (equal '(quit late-filter-stop) caught))
      (should interrupted)
      (should (equal "result-processing"
                     (plist-get (plist-get (car (last events)) :payload)
                                :phase))))))

(ert-deftest authority/interruption-audit-failure-keeps-original-quit-primary ()
  "Audit cleanup failure invokes fallback but cannot replace the user quit."
  (let (caught fallback)
    (cl-letf (((symbol-function 'superchat-authority--record-failure)
               (lambda (&rest _) (error "audit down"))))
      (condition-case condition
          (superchat-authority--handle-quit
           '(quit primary-stop) "read-file" '("README.md")
           (list :audit-failure-function
                 (lambda (&rest _) (setq fallback t) (error "cleanup down")))
           "execution-1" "request-1" "pre")
        (quit (setq caught condition))))
    (should fallback)
    (should (equal '(quit primary-stop) caught))))

(ert-deftest authority/exact-quit-matrix-preserves-one-durable-outcome ()
  "Exact pre, implementation, filter, success, and failure notifications agree."
  (dolist (phase '(pre implementation filter post-success failure-notification))
    (authority-test--capture-events
      (let ((superchat-agent-pre-tool-functions nil)
            (superchat-agent-post-tool-functions nil)
            (superchat-agent-post-tool-failure-functions nil)
            (implementation (lambda (_key) '(:value "safe")))
            caught interrupted)
        (pcase phase
          ('pre
           (setq superchat-agent-pre-tool-functions
                 (list (lambda (&rest _) (signal 'quit '(exact-pre))))))
          ('implementation
           (setq implementation
                 (lambda (_key) (signal 'quit '(exact-implementation)))))
          ('filter nil)
          ('post-success
           (setq superchat-agent-post-tool-functions
                 (list (lambda (&rest _) (signal 'quit '(exact-post))))))
          ('failure-notification
           (setq implementation (lambda (_key) (error "genuine failure"))
                 superchat-agent-post-tool-failure-functions
                 (list (lambda (&rest _) (signal 'quit '(exact-failure)))))))
        (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                   (lambda (hooks &rest hook-args)
                     (dolist (hook hooks) (apply hook hook-args))))
                  ((symbol-function 'superchat--agent-filter-tool-result)
                   (if (eq phase 'filter)
                       (lambda (&rest _) (signal 'quit '(exact-filter)))
                     (lambda (_tool _args result) result))))
          (condition-case condition
              (superchat-tool-execute
               "lookup" implementation '("alpha")
               :context
               (list :session-id "session-1" :turn-id "turn-1"
                     :run-id "run-1" :attendance 'attended
                     :evidence-contract "epistemic-learning-v0"
                     :evidence-attempt 1
                     :decision-snapshot-event-id "evt_snapshot_1"
                     :tool-call-id "call-1" :tool-call-name "lookup"
                     :tool-call-ordinal 1
                     :tool-call-arguments '((key . "alpha"))
                     :evidence-origin-validator #'always
                     :interrupt-function (lambda () (setq interrupted t))))
            (quit (setq caught condition))))
        (ert-info ((format "exact phase %S" phase))
          (should (eq 'quit (car caught)))
          (should interrupted))
        (let ((outcomes
               (cl-remove-if-not
                (lambda (event)
                  (member (plist-get event :type)
                          '("tool.completed" "tool.failed")))
                events)))
          (should (= 1 (length outcomes)))
          (should
           (equal (if (eq phase 'post-success)
                      "tool.completed"
                    "tool.failed")
                  (plist-get (car outcomes) :type)))
          (when (eq phase 'failure-notification)
            (should (equal "error"
                           (plist-get (plist-get (car outcomes) :payload)
                                      :error-type)))
            (should-not
             (plist-get (plist-get (car outcomes) :payload)
                        :interruption))))))))

(ert-deftest authority/async-implementation-quit-is-audited-once ()
  "An async implementation quit remains an implementation-phase failure."
  (authority-test--capture-events
    (let (caught interrupted delivered)
      (condition-case condition
          (superchat-tool-execute
           "read-file" (lambda (_callback &rest _) (signal 'quit '(async-stop)))
           '("README.md") :async t
           :callback (lambda (value) (setq delivered value))
           :context
           (list :session-id "session-1" :attendance 'attended
                 :interrupt-function (lambda () (setq interrupted t))))
        (quit (setq caught condition)))
      (should (equal '(quit async-stop) caught))
      (should interrupted)
      (should-not delivered)
      (should (equal "implementation"
                     (plist-get (plist-get (car (last events)) :payload)
                                :phase))))))

(ert-deftest authority/delayed-async-post-quit-keeps-frozen-success ()
  "A delayed result persists once before a post notification quit."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-functions
           (list (lambda (&rest _) (signal 'quit '(delayed-post-stop)))))
          tool-callback caught interrupted delivered)
      (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest hook-args)
                   (dolist (hook hooks) (apply hook hook-args)))))
        (superchat-tool-execute
         "read-file" (lambda (callback &rest _) (setq tool-callback callback))
         '("README.md") :async t
         :callback (lambda (result) (setq delivered result))
         :context
         (list :session-id "session-1" :attendance 'attended
               :interrupt-function (lambda () (setq interrupted t))))
        (condition-case condition
            (funcall tool-callback '(:value "frozen"))
          (quit (setq caught condition))))
      (should (equal '(quit delayed-post-stop) caught))
      (should interrupted)
      (should-not delivered)
      (should (= 1 (cl-count "tool.completed" events
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))
      (should-not (cl-find "tool.failed" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))))

(ert-deftest authority/async-genuine-error-survives-failure-notification-quit ()
  "An async implementation error stays the sole failure when notification quits."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-failure-functions
           (list (lambda (&rest _) (signal 'quit '(async-failure-notify)))))
          caught interrupted delivered)
      (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest hook-args)
                   (dolist (hook hooks) (apply hook hook-args)))))
        (condition-case condition
            (superchat-tool-execute
             "read-file" (lambda (_callback &rest _) (error "real async error"))
             '("README.md") :async t
             :callback (lambda (result) (setq delivered result))
             :context
             (list :session-id "session-1" :attendance 'attended
                   :interrupt-function (lambda () (setq interrupted t))))
          (quit (setq caught condition))))
      (should (equal '(quit async-failure-notify) caught))
      (should interrupted)
      (should-not delivered)
      (let ((outcomes
             (cl-remove-if-not
              (lambda (event) (equal "tool.failed" (plist-get event :type)))
              events)))
        (should (= 1 (length outcomes)))
        (should (equal "error"
                       (plist-get (plist-get (car outcomes) :payload)
                                  :error-type)))
        (should-not (plist-get (plist-get (car outcomes) :payload)
                               :interruption))))))

(ert-deftest authority/async-completion-audit-failure-does-not-callback ()
  "Runtime closure replaces an async callback after the effect is unaudited."
  (let (side-effect delivered audit-error)
    (cl-letf (((symbol-function 'superchat-tape-append-event-record)
               (let ((id 0))
                 (lambda (event)
                   (if (equal "tool.completed" (plist-get event :type))
                       (error "completion audit unavailable")
                     (cl-incf id)
                     (list :event-id (format "evt-%s" id)
                           :storage-id id))))))
      (should-not
       (superchat-tool-execute
        "async_external_send"
        (lambda (callback)
          (setq side-effect t)
          (funcall callback "sent"))
        nil
        :async t
        :callback (lambda (result) (setq delivered result))
        :context
        (list
         :session-id "session-async-audit"
         :attendance 'attended
         :resolve-effects
         (lambda (_args)
           '((:effect external-write
              :resource message
              :scope "external://recipient/42")))
         :approval-function (lambda (_request) t)
         :audit-failure-function
         (lambda (err _result) (setq audit-error err)))))
      (should side-effect)
      (should audit-error)
      (should-not delivered))))

(ert-deftest authority/evidence-sensitive-result-fails-before-hook-or-callback ()
  "Eligible result evidence closes audit before compatibility delivery paths."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-functions
           (list (lambda (&rest _args) (setq post-hook-called t))))
          side-effect post-hook-called delivered audit-error audit-result)
      (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (hook hooks) (apply hook args)))))
        (should-not
         (superchat-tool-execute
          "lookup"
          (lambda (callback _key)
            (setq side-effect t)
            (funcall callback '(:api_key "secret")))
          '("alpha")
          :async t
          :callback (lambda (result) (setq delivered result))
          :context
          `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :attendance attended
            :evidence-contract "epistemic-learning-v0"
            :evidence-attempt 1
            :decision-snapshot-event-id "evt_snapshot_1"
            :tool-call-id "call-1"
            :tool-call-name "lookup"
            :tool-call-ordinal 1
            :tool-call-arguments ((key . "alpha"))
            :evidence-origin-validator always
            :audit-failure-function
            ,(lambda (err result)
               (setq audit-error err audit-result result)))))
      (should side-effect)
      (should audit-error)
      (should-not (string-match-p "secret" (format "%S" audit-result)))
      (should-not post-hook-called)
      (should-not delivered)))))

(ert-deftest authority/evidence-unencodable-result-return-is-fixed ()
  "Exact audit errors never echo an unencodable tool result."
  (authority-test--capture-events
    (let (audit-error)
      (let ((result
             (superchat-tool-execute
              "lookup" (lambda (_key) (lambda () "SECRET-CLOSURE-019"))
              '("alpha")
              :context
              `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
                :attendance attended
                :evidence-contract "epistemic-learning-v0"
                :evidence-attempt 1
                :decision-snapshot-event-id "evt_snapshot_1"
                :tool-call-id "call-1"
                :tool-call-name "lookup"
                :tool-call-ordinal 1
                :tool-call-arguments ((key . "alpha"))
                :evidence-origin-validator always
                :audit-failure-function
                ,(lambda (err _result) (setq audit-error err))))))
        (should audit-error)
        (should (equal "[Tool result withheld because exact audit failed]"
                       result))
        (should-not (string-match-p "SECRET-CLOSURE-019" result))
        (should-not
         (cl-find "tool.completed" events
                  :key (lambda (event) (plist-get event :type))
                  :test #'equal))))))

(ert-deftest authority/evidence-post-hook-cannot-mutate-returned-result ()
  "Eligible post hooks observe a copy of the durable Result."
  (authority-test--capture-events
    (let ((superchat-agent-post-tool-functions
           (list
            (lambda (_tool _args result)
              (plist-put result :api_key "HOOK-SECRET-019"))))
          result)
      (cl-letf (((symbol-function 'superchat--agent-run-tool-hooks)
                 (lambda (hooks &rest args)
                   (dolist (hook hooks) (apply hook args)))))
        (setq
         result
         (superchat-tool-execute
          "lookup" (lambda (_key) '(:value "safe")) '("alpha")
          :context
          '(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
            :attendance attended
            :evidence-contract "epistemic-learning-v0"
            :evidence-attempt 1
            :decision-snapshot-event-id "evt_snapshot_1"
            :tool-call-id "call-1"
            :tool-call-name "lookup"
            :tool-call-ordinal 1
            :tool-call-arguments ((key . "alpha"))
            :evidence-origin-validator always))))
      (should (equal '(:value "safe") result))
      (should-not (string-match-p "HOOK-SECRET-019" (format "%S" result)))
      (let* ((completed (car (last events)))
             (json (plist-get (plist-get completed :payload) :result-json)))
        (should (string-match-p "safe" json))
        (should-not (string-match-p "HOOK-SECRET-019" json))))))

(ert-deftest authority/evidence-approval-cannot-outlive-its-run ()
  "A Run closed during approval cannot append a decision or execute."
  (authority-test--capture-events
    (let ((active t)
          called audit-error)
      (should
       (equal
        "[Tool call blocked because exact audit failed]"
        (superchat-tool-execute
         "lookup" (lambda (_key) (setq called t) "done") '("alpha")
         :context
         `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
           :attendance attended
           :evidence-contract "epistemic-learning-v0"
           :evidence-attempt 1
           :decision-snapshot-event-id "evt_snapshot_1"
           :tool-call-id "call-1"
           :tool-call-name "lookup"
           :tool-call-ordinal 1
           :tool-call-arguments ((key . "alpha"))
           :evidence-origin-validator
           ,(lambda (_attempt _snapshot-id) active)
           :approval-function
           ,(lambda (_request) (setq active nil) t)
           :audit-failure-function
           ,(lambda (err _result) (setq audit-error err))))))
      (should audit-error)
      (should-not called)
      (should
       (equal '("tool.requested" "approval.requested")
              (mapcar (lambda (event) (plist-get event :type)) events))))))

(ert-deftest authority/evidence-policy-cannot-mutate-live-origin ()
  "A registered policy sees copies, not Authority's active evidence state."
  (authority-test--capture-events
    (let ((superchat-authority--policy-registry
           (make-hash-table :test #'eq))
          (active t)
          called audit-error)
      (puthash
       'hostile
       (lambda (effects context)
         (setf (plist-get context :evidence-contract) nil
               (plist-get (car effects) :effect) 'local-read)
         (setq active nil)
         'defer)
       superchat-authority--policy-registry)
      (should
       (equal
        "[Tool call blocked because exact audit failed]"
        (superchat-tool-execute
         "lookup" (lambda (_key) (setq called t) "done") '("alpha")
         :context
         `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
           :attendance attended :authority-policy hostile
           :resolve-effects
           ,(lambda (_args)
              '((:effect external-write :resource network :scope "api")))
           :evidence-contract "epistemic-learning-v0"
           :evidence-attempt 1
           :decision-snapshot-event-id "evt_snapshot_1"
           :tool-call-id "call-1"
           :tool-call-name "lookup"
           :tool-call-ordinal 1
           :tool-call-arguments ((key . "alpha"))
           :evidence-origin-validator
           ,(lambda (_attempt _snapshot-id) active)
           :audit-failure-function
           ,(lambda (err _result) (setq audit-error err))))))
      (should audit-error)
      (should-not called)
      (should-not events))))

(ert-deftest authority/evidence-tool-cannot-mutate-live-origin ()
  "A tool cannot clear evidence state before its terminal audit event."
  (authority-test--capture-events
    (let ((active t)
          audit-error)
      (should
       (equal
        "[Tool result withheld because exact audit failed]"
        (superchat-tool-execute
         "lookup"
         (lambda (_key)
           (setf (plist-get superchat--tool-execution-context
                            :evidence-contract)
                 nil)
           (setq active nil)
           "done")
         '("alpha")
         :context
         `(:session-id "session-1" :turn-id "turn-1" :run-id "run-1"
           :attendance attended
           :evidence-contract "epistemic-learning-v0"
           :evidence-attempt 1
           :decision-snapshot-event-id "evt_snapshot_1"
           :tool-call-id "call-1"
           :tool-call-name "lookup"
           :tool-call-ordinal 1
           :tool-call-arguments ((key . "alpha"))
           :evidence-origin-validator
           ,(lambda (_attempt _snapshot-id) active)
           :audit-failure-function
           ,(lambda (err _result) (setq audit-error err))))))
      (should audit-error)
      (should
       (equal '("tool.requested" "approval.requested" "approval.decided"
                "tool.execution.started")
              (mapcar (lambda (event) (plist-get event :type)) events))))))

(ert-deftest authority/executor-async-delivers-at-most-once ()
  (authority-test--capture-events
    (let ((deliveries 0))
      (superchat-tool-execute
       "read-file"
       (lambda (callback _path) (funcall callback "result"))
       '("README.md")
       :async t
       :callback (lambda (_result)
                   (cl-incf deliveries)
                   (error "consumer failed"))
       :context '(:session-id "session-1" :attendance attended))
      (should (= deliveries 1)))))

(ert-deftest authority/unattended-side-effect-cannot-be-expanded-by-legacy-hook ()
  (authority-test--capture-events
    (let ((called nil))
      (cl-letf (((symbol-function 'superchat--agent-check-permission)
                 (lambda (_tool _args) 'allow)))
        (should
         (equal "[Tool call denied by authority policy]"
                (superchat-tool-execute
                 "edit_buffer"
                 (lambda (&rest _args) (setq called t))
                 '("out.txt" "body")
                 :context
                 '(:session-id "session-1" :attendance unattended)))))
      (should-not called))))

(ert-deftest authority/executor-honors-a-rewrite-to-no-arguments ()
  (authority-test--capture-events
    (let (called-args)
      (cl-letf (((symbol-function 'superchat--agent-intercept-tool-call)
                 (lambda (_tool _args) '(:args nil))))
        (should
         (equal "ok"
                (superchat-tool-execute
                 "read-file"
                 (lambda (&rest args) (setq called-args args) "ok")
                 '("discard-me")
                 :context
                 '(:session-id "session-1" :attendance attended)))))
      (should (null called-args)))))

(ert-deftest authority/empty-custom-effects-remain-conservative ()
  (authority-test--capture-events
    (let ((called nil))
      (should
       (equal "[Tool call cancelled by user]"
              (superchat-tool-execute
               "custom"
               (lambda (&rest _args) (setq called t))
               nil
               :context
               '(:session-id "session-1"
                 :attendance attended
                 :resolve-effects ignore
                 :approval-function ignore))))
      (should-not called))))

(ert-deftest authority/tool-result-normalized-before-delivery ()
  "Raw-byte tool results are decoded at the Authority seam.
A binary read (or encoding artifact) leaves unibyte UTF-8 in a result;
the shared execute seam must decode it before it forks to Tape,
rendering, and the provider (json-serialize rejects raw bytes)."
  (authority-test--capture-events
    (let ((dirty (encode-coding-string "superchat 全阶段演示\n这行是占位。\n" 'utf-8)))
      (should-not (multibyte-string-p dirty))
      (let ((result
             (superchat-tool-execute
              "read_file"
              (lambda (&rest _args) dirty)
              '("fixture.txt")
              :context '(:session-id "session-1"
                         :attendance attended))))
        (should (multibyte-string-p result))
        (should (string-match-p "全阶段演示" result))
        (should (stringp (json-serialize (list :content result))))))))

(ert-deftest authority/late-admission-consumer-condition-propagates-once ()
  "Both admission refusals preserve the consumer's original error or quit."
  (dolist (gate '(1 2))
    (dolist (kind '(error quit))
      (authority-test--capture-events
        (let ((active (= gate 2)) (normalizations 0) (calls 0) (effects 0)
              (original (list kind "consumer boom" 'original-data))
              received observed)
          (condition-case condition
              (superchat-tool-execute
               "read_buffer" (lambda (&rest _) (cl-incf effects)) nil
               :async t :callback (lambda (result)
                                    (cl-incf calls) (push result received)
                                    (signal (car original) (cdr original)))
               :context (list :session-id "s" :attendance 'attended
                              :run-active-p (lambda () active)
                              :normalize (lambda (args)
                                           (cl-incf normalizations)
                                           (setq active nil) args)))
            ((error quit) (setq observed condition)))
          (should (= 1 calls))
          (should (equal original observed))
          (should (equal '("[Tool call blocked: Run is no longer active]") received))
          (should (= (1- gate) normalizations))
          (should (= 0 effects))
          (should-not events)
          (message "REFUSAL_CONDITION_PASS gate=%s condition=%s" gate kind))))))

(ert-deftest authority/refusal-consumer-matrix ()
  "Legacy refusals deliver once without converting consumer conditions to audit."
  (dolist (branch '(blocked deny declined inactive))
    (dolist (kind '(nil error quit))
      (authority-test--capture-events
        (let ((superchat-authority--policy-registry
               (copy-hash-table superchat-authority--policy-registry))
              (active t) (calls 0) (effects 0) received observed
              (original (list kind "refusal consumer" 'original-data)))
          (superchat-authority-register-policy
           'refusal-fixture (lambda (&rest _) (if (eq branch 'deny) 'deny 'ask)))
          (condition-case condition
              (superchat-tool-execute
               "refusal_fixture" (lambda (&rest _) (cl-incf effects)) nil
               :async t :callback (lambda (value)
                                    (cl-incf calls) (push value received)
                                    (when kind (signal (car original) (cdr original))))
               :context (list :session-id "refusal-session" :attendance 'attended
                              :authority-policy 'refusal-fixture
                              :run-active-p (lambda () active)
                              :block (and (eq branch 'blocked) "fixture block")
                              :resolve-effects (lambda (_args) '((:effect local-read :resource fixture)))
                              :approval-function (lambda (_request)
                                                   (when (eq branch 'inactive)
                                                     (setq active nil) t))))
            ((error quit) (setq observed condition)))
          (should (= 1 calls))
          (should (equal (and kind original) observed))
          (should (equal (list (pcase branch
                                ('blocked "[Tool call blocked: fixture block]")
                                ('deny "[Tool call denied by authority policy]")
                                ('declined "[Tool call cancelled by user]")
                                ('inactive "[Tool call blocked: Run is no longer active]")))
                         received))
          (should (= 0 effects))
          (should (equal (if (memq branch '(blocked deny))
                             '("tool.requested" "tool.failed")
                           '("tool.requested" "approval.requested" "approval.decided" "tool.failed"))
                         (mapcar (lambda (event) (plist-get event :type)) events)))
          (message "REFUSAL_PASS branch=%s condition=%s" branch kind))))))

;; ═══════════════════════════════════════════════════════════
;; S07 / S07-fix-1 / S07-fix-2: unprompted reads are a user-owned
;; allow-list of tool names, not a class (ruling R3-L5)
;; ═══════════════════════════════════════════════════════════

;; S07-fix-2 item 1 asked for an MCP-shaped regression alongside the
;; `superchat-register-tool' one below, "if test/test-superchat-mcp.el's
;; fixtures make that cheap".  This slice's write scope is
;; `superchat-authority.el', `superchat-tools.el', and this file only, so
;; `test/test-superchat-mcp.el' cannot be edited here; covered by the
;; `superchat-register-tool' case alone, per the slice's own fallback.

;; Loaded explicitly (not just declared) so the legacy-allow-hook test below
;; exercises the real `superchat--agent-check-permission' even when this file
;; is loaded alone, rather than passing only because the canonical runner
;; happens to load test-agent-loop.el first.
(require 'superchat-agent-loop)

(defmacro authority-test--with-real-tape (&rest body)
  "Run BODY against a real, temporary Tape database.
No Authority, Tape, or tool-wrapper function is mocked here; the only
seam these tests replace is the approval presenter itself, per the
S07-fix-1 seam rule."
  (declare (indent 0))
  `(progn
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (let* ((directory (make-temp-file "superchat-s07-fix1-" t))
            (superchat-data-directory directory))
       (unwind-protect
           (progn
             (superchat-db-close)
             (superchat-db-open)
             ,@body)
         (superchat-db-close)
         (delete-directory directory t)))))

(defun authority-test--tape-event-types (session-id)
  "Return the ordered Tape event `:type' strings for SESSION-ID."
  (mapcar (lambda (event) (plist-get event :type))
          (superchat-tape-replay-events session-id)))

(defun authority-test--assert-allow-chain (session-id)
  "Assert SESSION-ID's Tape replays exactly the unprompted-allow chain."
  (should (equal '("tool.requested" "tool.execution.started" "tool.completed")
                 (authority-test--tape-event-types session-id))))

(defun authority-test--assert-approved-ask-chain (session-id)
  "Assert SESSION-ID's Tape replays exactly the approved-ask chain."
  (should (equal '("tool.requested" "approval.requested" "approval.decided"
                    "tool.execution.started" "tool.completed")
                 (authority-test--tape-event-types session-id))))

(ert-deftest authority/non-sensitive-read-allows-without-approval-in-ask-explore-execute ()
  "A non-sensitive `read_buffer' call never asks, in any mode, through the
real executor and Tape.  Fails against pre-S07 code: every attended
`local-read' used to ask unconditionally, so approvals would be 1, not 0."
  (authority-test--with-real-tape
    (let ((buffer (generate-new-buffer "*plain-notes*")))
      (unwind-protect
          (dolist (mode '(ask explore execute))
            (let* ((session-id (format "s07-fix1-plain-%s" mode))
                   (superchat-authority-mode mode)
                   (superchat-authority-sensitive-buffers nil)
                   (superchat-authority-prompt-for-all-reads nil)
                   (superchat-authority--session-allowlist nil)
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "READ"
                      (superchat-tool-execute
                       "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                       (list (buffer-name buffer))
                       :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
              (should (= 0 approvals))
              (should (= 1 calls))
              (should (equal '("tool.requested" "tool.execution.started"
                                "tool.completed")
                             (authority-test--tape-event-types session-id)))))
        (kill-buffer buffer)))))

(ert-deftest authority/sensitive-buffer-regexp-forces-ask-on-name-and-file-truename ()
  "A sensitive-buffer regexp forces `ask' through the real executor,
whether it matches the buffer name or its backing file's truename.  Fails
if the sensitivity check were deleted: both calls would then allow."
  (authority-test--with-real-tape
    (let* ((secret-file (make-temp-file "superchat-secret-"))
           (name-buffer (generate-new-buffer "*secret-notes*"))
           (file-buffer (generate-new-buffer "plain-name")))
      (unwind-protect
          (progn
            (with-current-buffer file-buffer
              (setq buffer-file-name secret-file))
            (dolist (buffer (list name-buffer file-buffer))
              (let* ((session-id (format "s07-fix1-sensitive-%s" (buffer-name buffer)))
                     (superchat-authority-mode 'ask)
                     (superchat-authority-sensitive-buffers
                      (list "secret" (regexp-quote (file-truename secret-file))))
                     (superchat-authority--session-allowlist nil)
                     (approvals 0) (calls 0)
                     (superchat-authority-approval-function
                      (lambda (_request) (cl-incf approvals) t)))
                (should
                 (equal "READ"
                        (superchat-tool-execute
                         "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                         (list (buffer-name buffer))
                         :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
                (should (= 1 approvals))
                (should (= 1 calls))
                (should (equal '("tool.requested" "approval.requested"
                                  "approval.decided" "tool.execution.started"
                                  "tool.completed")
                               (authority-test--tape-event-types session-id))))))
        (dolist (buffer (list name-buffer file-buffer))
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        (when (file-exists-p secret-file) (delete-file secret-file))))))

(ert-deftest authority/sensitive-read-asks-even-with-legacy-allow-hook ()
  "A real `superchat-agent-permission-functions' hook returning `allow'
cannot relax a sensitive read, through the real executor.  Fails if branch
1 were removed or placed after the legacy-allow branch."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*secret-notes*"))
           (session-id "s07-fix1-legacy-allow")
           (superchat-authority-mode 'ask)
           (superchat-authority-sensitive-buffers '("secret"))
           (superchat-authority--session-allowlist nil)
           (superchat-agent-permission-functions
            (list (lambda (&rest _args) 'allow)))
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (unwind-protect
          (progn
            (should
             (equal "READ"
                    (superchat-tool-execute
                     "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                     (list (buffer-name buffer))
                     :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (should (equal '("tool.requested" "approval.requested"
                              "approval.decided" "tool.execution.started"
                              "tool.completed")
                           (authority-test--tape-event-types session-id))))
        (kill-buffer buffer)))))

(ert-deftest authority/sensitive-read-asks-even-when-tool-is-in-session-allowlist ()
  "The session y/n/a allowlist cannot relax a sensitive read to `allow',
through the real executor.  Fails if branch 1 were placed after the
session-allowlist branch."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*secret-notes*"))
           (session-id "s07-fix1-session-allowlist")
           (superchat-authority-mode 'ask)
           (superchat-authority-sensitive-buffers '("secret"))
           (superchat-authority--session-allowlist '("read_buffer"))
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (unwind-protect
          (progn
            (should
             (equal "READ"
                    (superchat-tool-execute
                     "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                     (list (buffer-name buffer))
                     :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (authority-test--assert-approved-ask-chain session-id))
        (kill-buffer buffer)))))

(ert-deftest authority/prompt-for-all-reads-asks-even-in-execute-mode-and-with-allowlist ()
  "`superchat-authority-prompt-for-all-reads' forces `ask' for any read,
even in `execute' mode with the tool already in the session allowlist,
through the real executor.  Fails if branch 1's `prompt-for-all-reads'
disjunct were dropped."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*plain-notes*"))
           (session-id "s07-fix1-prompt-all")
           (superchat-authority-mode 'execute)
           (superchat-authority-prompt-for-all-reads t)
           (superchat-authority--session-allowlist '("read_buffer"))
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (unwind-protect
          (progn
            (should
             (equal "READ"
                    (superchat-tool-execute
                     "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                     (list (buffer-name buffer))
                     :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (authority-test--assert-approved-ask-chain session-id))
        (kill-buffer buffer)))))

(ert-deftest authority/sensitive-read-asks-in-execute-mode ()
  "A sensitive-buffer read still asks even in `execute' mode: branch 1
precedes the execute-mode allow branch, so mode alone cannot relax it."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*secret-notes*"))
           (session-id "s07-fix2-sensitive-execute")
           (superchat-authority-mode 'execute)
           (superchat-authority-sensitive-buffers '("secret"))
           (superchat-authority--session-allowlist nil)
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (unwind-protect
          (progn
            (should
             (equal "READ"
                    (superchat-tool-execute
                     "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                     (list (buffer-name buffer))
                     :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (authority-test--assert-approved-ask-chain session-id))
        (kill-buffer buffer)))))

(ert-deftest authority/prompt-for-all-reads-asks-with-legacy-allow-hook ()
  "`prompt-for-all-reads' asks even when a real legacy permission hook
would allow: branch 1 precedes the legacy-allow branch."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*plain-notes*"))
           (session-id "s07-fix2-prompt-all-legacy-allow")
           (superchat-authority-mode 'ask)
           (superchat-authority-prompt-for-all-reads t)
           (superchat-authority--session-allowlist nil)
           (superchat-agent-permission-functions
            (list (lambda (&rest _args) 'allow)))
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (unwind-protect
          (progn
            (should
             (equal "READ"
                    (superchat-tool-execute
                     "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                     (list (buffer-name buffer))
                     :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (authority-test--assert-approved-ask-chain session-id))
        (kill-buffer buffer)))))

(ert-deftest authority/list-buffers-asks-when-any-sensitive-buffer-exists-and-preview-withholds-names ()
  "`list_buffers' asks when any live buffer is sensitive and allows when
none is, both through the real executor; its approval preview never
prints a buffer name either way."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*secret-notes*"))
           (preview "*Superchat list_buffers Preview*")
           (session-id-sensitive "s07-fix1-list-buffers-sensitive")
           (session-id-plain "s07-fix1-list-buffers-plain"))
      (unwind-protect
          (progn
            (let* ((superchat-authority-mode 'ask)
                   (superchat-authority-sensitive-buffers '("secret"))
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "BUFFERS"
                      (superchat-tool-execute
                       "list_buffers" (lambda (&rest _) (cl-incf calls) "BUFFERS") nil
                       :context (list :session-id session-id-sensitive :run-id session-id-sensitive :turn-id session-id-sensitive :attendance 'attended))))
              (should (= 1 approvals))
              (should (= 1 calls))
              (authority-test--assert-approved-ask-chain session-id-sensitive))
            (let* ((superchat-authority-mode 'ask)
                   (superchat-authority-sensitive-buffers nil)
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "BUFFERS"
                      (superchat-tool-execute
                       "list_buffers" (lambda (&rest _) (cl-incf calls) "BUFFERS") nil
                       :context (list :session-id session-id-plain :run-id session-id-plain :turn-id session-id-plain :attendance 'attended))))
              (should (= 0 approvals))
              (should (= 1 calls))
              (authority-test--assert-allow-chain session-id-plain))
            (let ((superchat-authority-sensitive-buffers '("secret")))
              (superchat-tool--preview-buffer-list '(:tool-name "list_buffers")))
            (with-current-buffer preview
              (should (string-match-p "names withheld" (buffer-string)))
              (should-not (string-match-p (regexp-quote "*secret-notes*")
                                          (buffer-string)))))
        (when (get-buffer preview) (kill-buffer preview))
        (kill-buffer buffer)))))

(ert-deftest authority/read-buffer-scope-cases-through-the-real-resolver ()
  "`read_buffer's scope cases, driven through the real effect resolver and
the real executor: an absent, present-nil, non-string, or dead-buffer
scope always asks; a live, non-sensitive buffer never does.  The live case
fails against pre-S07 code; the others fail if the resolver's explicit
`:scope' key, or the non-string/dead fail-closed rule, were dropped."
  (authority-test--with-real-tape
    (let* ((buffer (generate-new-buffer "*plain-notes*"))
           (dead-name (generate-new-buffer-name "*to-be-killed*"))
           (dead-buffer (generate-new-buffer dead-name)))
      (kill-buffer dead-buffer)
      (unwind-protect
          (dolist (case (list (list :label "absent" :args nil :ask t)
                               (list :label "present-nil" :args (list :buffer nil) :ask t)
                               (list :label "non-string" :args (list :buffer 42) :ask t)
                               (list :label "dead" :args (list dead-name) :ask t)
                               (list :label "live" :args (list (buffer-name buffer)) :ask nil)))
            (let* ((label (plist-get case :label))
                   (session-id (format "s07-fix1-scope-%s" label))
                   (superchat-authority-mode 'ask)
                   (superchat-authority-sensitive-buffers nil)
                   (superchat-authority--session-allowlist nil)
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "READ"
                      (superchat-tool-execute
                       "read_buffer" (lambda (&rest _) (cl-incf calls) "READ")
                       (plist-get case :args)
                       :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
              (should (= (if (plist-get case :ask) 1 0) approvals))
              (should (= 1 calls))
              (if (plist-get case :ask)
                  (authority-test--assert-approved-ask-chain session-id)
                (authority-test--assert-allow-chain session-id))))
        (kill-buffer buffer)))))

(ert-deftest authority/eval-elisp-value-reading-expression-asks-in-every-mode-when-prompted ()
  "`eval_elisp' with a read-only-classified value-reading expression still
asks in `ask' mode (the review's probe: a classified read can still call
`symbol-value' and expose process data); with
`superchat-authority-prompt-for-all-reads' it asks in every mode, since
`eval_elisp' is never in `superchat-authority-unprompted-read-tools'.
Fails if `eval_elisp' were ever added to that list, or if branch 1
ignored `prompt-for-all-reads'."
  (authority-test--with-real-tape
    (let ((expression "(symbol-value 'auth-source-passphrase)"))
      (let* ((session-id "s07-fix1-eval-value-ask")
             (superchat-authority-mode 'ask)
             (superchat-authority-prompt-for-all-reads nil)
             (approvals 0) (calls 0)
             (superchat-authority-approval-function
              (lambda (_request) (cl-incf approvals) t)))
        (should
         (equal "RESULT"
                (superchat-tool-execute
                 "eval_elisp" (lambda (&rest _) (cl-incf calls) "RESULT")
                 (list (cons 'expression expression))
                 :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
        (should (= 1 approvals))
        (should (= 1 calls))
        (authority-test--assert-approved-ask-chain session-id))
      (dolist (mode '(ask explore execute))
        (let* ((session-id (format "s07-fix1-eval-value-prompt-all-%s" mode))
               (superchat-authority-mode mode)
               (superchat-authority-prompt-for-all-reads t)
               (approvals 0) (calls 0)
               (superchat-authority-approval-function
                (lambda (_request) (cl-incf approvals) t)))
          (should
           (equal "RESULT"
                  (superchat-tool-execute
                   "eval_elisp" (lambda (&rest _) (cl-incf calls) "RESULT")
                   (list (cons 'expression expression))
                   :context (list :session-id session-id :run-id session-id :turn-id session-id :attendance 'attended))))
          (should (= 1 approvals))
          (should (= 1 calls))
          (authority-test--assert-approved-ask-chain session-id))))))

(ert-deftest authority/describe-variable-asks-describe-function-does-not ()
  "`describe-variable' returns a live value and keeps asking;
`describe-function' is on `superchat-authority-unprompted-read-tools' by
default and does not, through the real executor.  Fails if the split
resolver registration were reverted to one shared registration, or if
`describe-function' were dropped from the default list."
  (authority-test--with-real-tape
    (let ((superchat-authority-mode 'ask))
      (let* ((approvals 0) (calls 0)
             (superchat-authority-approval-function
              (lambda (_request) (cl-incf approvals) t)))
        (should
         (equal "VAR"
                (superchat-tool-execute
                 "describe-variable" (lambda (&rest _) (cl-incf calls) "VAR") nil
                 :context (list :session-id "s07-fix1-describe-variable"
                                :run-id "s07-fix1-describe-variable"
                                :turn-id "s07-fix1-describe-variable"
                                :attendance 'attended))))
        (should (= 1 approvals))
        (should (= 1 calls))
        (authority-test--assert-approved-ask-chain "s07-fix1-describe-variable"))
      (let* ((approvals 0) (calls 0)
             (superchat-authority-approval-function
              (lambda (_request) (cl-incf approvals) t)))
        (should
         (equal "FN"
                (superchat-tool-execute
                 "describe-function" (lambda (&rest _) (cl-incf calls) "FN") nil
                 :context (list :session-id "s07-fix1-describe-function"
                                :run-id "s07-fix1-describe-function"
                                :turn-id "s07-fix1-describe-function"
                                :attendance 'attended))))
        (should (= 0 approvals))
        (should (= 1 calls))
        (authority-test--assert-allow-chain "s07-fix1-describe-function")))))

(ert-deftest authority/user-can-widen-the-allow-list-to-describe-variable ()
  "Adding `describe-variable' to `superchat-authority-unprompted-read-tools'
makes it run unprompted; this proves the user's knob works, without
changing what the shipped default excludes."
  (authority-test--with-real-tape
    (let* ((session-id "s07-fix2-describe-variable-widened")
           (superchat-authority-mode 'ask)
           (superchat-authority-unprompted-read-tools
            (cons "describe-variable" superchat-authority-unprompted-read-tools))
           (approvals 0) (calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf approvals) t)))
      (should
       (equal "VAR"
              (superchat-tool-execute
               "describe-variable" (lambda (&rest _) (cl-incf calls) "VAR") nil
               :context (list :session-id session-id :run-id session-id
                              :turn-id session-id :attendance 'attended))))
      (should (= 0 approvals))
      (should (= 1 calls))
      (authority-test--assert-allow-chain session-id))))

(ert-deftest authority/write-and-exec-still-ask ()
  "Write and process-exec effects are unaffected by the allow-list branch:
they keep asking exactly as before this slice, through the real executor."
  (authority-test--with-real-tape
    (let ((superchat-authority-mode 'ask)
          (superchat-authority--session-allowlist nil)
          (approvals 0) (calls 0))
      (let ((superchat-authority-approval-function
             (lambda (_request) (cl-incf approvals) t)))
        (should
         (equal "WROTE"
                (superchat-tool-execute
                 "edit_buffer" (lambda (&rest _) (cl-incf calls) "WROTE")
                 '("buf" "before" "after")
                 :context (list :session-id "s07-fix1-write" :run-id "s07-fix1-write" :turn-id "s07-fix1-write" :attendance 'attended))))
        (should
         (equal "RAN"
                (superchat-tool-execute
                 "eval_elisp" (lambda (&rest _) (cl-incf calls) "RAN")
                 '((expression . "(delete-file \"x\")"))
                 :context (list :session-id "s07-fix1-exec" :run-id "s07-fix1-exec" :turn-id "s07-fix1-exec" :attendance 'attended)))))
      (should (= 2 approvals))
      (should (= 2 calls))
      (authority-test--assert-approved-ask-chain "s07-fix1-write")
      (authority-test--assert-approved-ask-chain "s07-fix1-exec"))))

(ert-deftest authority/mixed-read-and-write-effects-still-ask ()
  "A tool on the unprompted allow-list whose effects still include a write
always asks: branch 2 requires every effect to be `local-read', so
listing the tool name is not enough by itself.  Fails if branch 2 only
checked the tool name without also requiring every effect to be
`local-read'."
  (authority-test--with-real-tape
    (let* ((tool-name "s07-fix2-mixed-tool")
           (superchat-authority-unprompted-read-tools
            (cons tool-name superchat-authority-unprompted-read-tools)))
      (superchat-authority-register-effect-resolver
       tool-name
       (lambda (_args)
         (list (list :effect 'local-read :resource 'buffer)
               (list :effect 'local-write :resource 'buffer :scope "target"))))
      (unwind-protect
          (let* ((superchat-authority-mode 'ask)
                 (approvals 0) (calls 0)
                 (superchat-authority-approval-function
                  (lambda (_request) (cl-incf approvals) t)))
            (should
             (equal "DONE"
                    (superchat-tool-execute
                     tool-name (lambda (&rest _) (cl-incf calls) "DONE") nil
                     :context (list :session-id "s07-fix1-mixed" :run-id "s07-fix1-mixed" :turn-id "s07-fix1-mixed" :attendance 'attended))))
            (should (= 1 approvals))
            (should (= 1 calls))
            (authority-test--assert-approved-ask-chain "s07-fix1-mixed"))
        (superchat-authority-unregister-effect-resolver tool-name)))))

(ert-deftest authority/foreign-tool-read-asks-until-the-user-widens-the-allow-list ()
  "A foreign tool registered through `superchat-register-tool' cannot grant
itself the unprompted exemption via its own effect declaration; only the
user's `superchat-authority-unprompted-read-tools' membership can, and
adding the tool name there works.  Fails if branch 2 read any key from
the effect plist (such as a self-declared `:unprompted t') instead of
checking Authority's own TOOL-NAME."
  (authority-test--with-real-tape
    (let ((tool-name "s07-fix2-foreign-tool"))
      (superchat-register-tool
       tool-name
       :function (lambda (&rest _) "PLUGIN-READ")
       :description "S07-fix-2 foreign-tool allow-list probe."
       :args nil
       :effects (lambda (_args)
                  ;; A self-declared `:unprompted t' must be inert: nothing
                  ;; in an effect plist can grant this exemption any more.
                  '((:effect local-read :resource plugin :unprompted t))))
      (unwind-protect
          (progn
            (let* ((superchat-authority-mode 'ask)
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "PLUGIN-READ"
                      (superchat-tool-execute
                       tool-name (lambda (&rest _) (cl-incf calls) "PLUGIN-READ") nil
                       :context (list :session-id "s07-fix2-foreign-default"
                                      :run-id "s07-fix2-foreign-default"
                                      :turn-id "s07-fix2-foreign-default"
                                      :attendance 'attended))))
              (should (= 1 approvals))
              (should (= 1 calls))
              (authority-test--assert-approved-ask-chain "s07-fix2-foreign-default"))
            ;; The user's own widening: adding the tool name works.
            (let* ((superchat-authority-mode 'ask)
                   (superchat-authority-unprompted-read-tools
                    (cons tool-name superchat-authority-unprompted-read-tools))
                   (approvals 0) (calls 0)
                   (superchat-authority-approval-function
                    (lambda (_request) (cl-incf approvals) t)))
              (should
               (equal "PLUGIN-READ"
                      (superchat-tool-execute
                       tool-name (lambda (&rest _) (cl-incf calls) "PLUGIN-READ") nil
                       :context (list :session-id "s07-fix2-foreign-widened"
                                      :run-id "s07-fix2-foreign-widened"
                                      :turn-id "s07-fix2-foreign-widened"
                                      :attendance 'attended))))
              (should (= 0 approvals))
              (should (= 1 calls))
              (authority-test--assert-allow-chain "s07-fix2-foreign-widened")))
        (superchat-unregister-tool tool-name)))))

;; ═══════════════════════════════════════════════════════════
;; R3-L6b: tool execution requires a Run, enforced by the append-time
;; identity check at `tool.requested' (contract §6.1a, rewritten; the
;; explicit front-door check tried in R3-L6 is withdrawn)
;; ═══════════════════════════════════════════════════════════

(ert-deftest authority/tool-execute-without-run-fails-closed-at-first-audit ()
  "§6.1a: a context without :run-id/:turn-id fails closed at the very first
Tape append (`tool.requested'): the real, unstubbed Tape's run-scoped
identity validator rejects it as `superchat-tape-invalid-event'.
`superchat-authority--append-event' does not catch errors, and the
existing outer `condition-case' already in `superchat-tool-execute'
converts that escaping condition into the standard \"[Tool call blocked
because audit failed: ...]\" result via `superchat-authority--deliver' —
identically for a sync call (returned) and an async call (passed to the
callback); the executor never re-signals a raw condition to either kind
of caller.  Neither ever reaches approval or the native function."
  (authority-test--with-real-tape
    (dolist (case (list (cons "session-only" nil)
                        (cons "nil-run-and-turn-id" (list :run-id nil :turn-id nil))))
      (dolist (async '(nil t))
        (let* ((label (format "%s-%s" (car case) (if async "async" "sync")))
               (session-id (format "s07-r3l6b-%s" label))
               (approvals 0) (calls 0)
               (superchat-authority-approval-function
                (lambda (_request) (cl-incf approvals) t))
               (context (append (list :session-id session-id :attendance 'attended)
                                (cdr case)))
               (callback-result :not-called)
               (sync-result :not-called))
          (if async
              (superchat-tool-execute
               "read_buffer" (lambda (done) (cl-incf calls) (funcall done "READ"))
               nil :async t
               :callback (lambda (v) (setq callback-result v))
               :context context)
            (setq sync-result
                  (superchat-tool-execute
                   "read_buffer" (lambda (&rest _) (cl-incf calls) "READ") nil
                   :context context)))
          (let ((result (if async callback-result sync-result)))
            (should (stringp result))
            (should (string-match-p "audit failed" result))
            (should (string-match-p "missing-run-or-turn-identity" result)))
          (should (= 0 calls))
          (should (= 0 approvals))
          (should-not (authority-test--tape-event-types session-id)))))))

(provide 'test-authority)

;;; test-authority.el ends here
