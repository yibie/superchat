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
                 (lambda (_prompt _choices)
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
               (lambda (_prompt _choices)
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
                 (lambda (_prompt choices)
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
                 (lambda (_prompt cs)
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

(ert-deftest authority/executor-does-not-run-after-runtime-termination ()
  "A late approved call is blocked before ORIGINAL-FN can produce effects."
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
      (should
       (equal '("tool.requested"
                "approval.requested"
                "approval.decided"
                "tool.failed")
              (mapcar (lambda (event) (plist-get event :type)) events)))
      (should (equal "run-inactive"
                     (plist-get (plist-get (car (last events)) :payload)
                                :status))))))

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

(provide 'test-authority)

;;; test-authority.el ends here
