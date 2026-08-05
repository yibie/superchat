;;; test-authority.el --- Tests for superchat-authority -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-authority)
(require 'superchat-preset)
(require 'superchat-tools)

(defvar superchat-agent-pre-tool-functions)

(ert-deftest authority/requires-explicit-attendance ()
  (should-error (superchat-authority-decide "read-file" '("README.md") nil))
  (should-error (superchat-authority-decide "read-file" '("README.md") t)))

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
  (should (equal (superchat-authority-resolve-effects "read-file" '("README.md"))
                 '((:effect local-read :resource file :scope "README.md"))))
  (should (equal (superchat-authority-resolve-effects
                  "write-file" '(:content "x" :path "out.txt"))
                 '((:effect local-write :resource file :scope "out.txt"))))
  (should (equal (superchat-authority-resolve-effects
                  "write-file" '("/tmp/x" "body"))
                 '((:effect local-write :resource file :scope "/tmp/x"))))
  (should (equal (superchat-authority-resolve-effects
                  "read_buffer" '(:buffer "*scratch*"))
                 '((:effect local-read :resource buffer :scope "*scratch*"))))
  (should (equal (superchat-authority-resolve-effects
                  "ReplaceBuffer" '(:buffer_name "*scratch*" :content "x"))
                 '((:effect local-write :resource buffer :scope "*scratch*"))))
  (should (equal (superchat-authority-resolve-effects
                  "shell-command" '("git status"))
                 '((:effect process-exec :resource process :scope "git status"))))
  (should (equal (superchat-authority-resolve-effects
                  "search-text" '("needle" "/project"))
                 '((:effect local-read :resource file :scope "/project"))))
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

(ert-deftest authority/executor-previews-before-approval-and-execution ()
  (authority-test--capture-events
    (let ((superchat-authority-approval-previews
           (make-hash-table :test 'equal))
          (superchat-authority-approval-function
           #'superchat-authority-confirm)
          order)
      (superchat-authority-register-approval-preview
       "write-file"
       (lambda (_request) (push 'preview order)))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt)
                   (push 'approval order)
                   t)))
        (should
         (equal "written"
                (superchat-tool-execute
                 "write-file"
                 (lambda (&rest _args)
                   (push 'execute order)
                   "written")
                 '("safe.txt" "body")
                 :context
                 '(:session-id "session-1"
                   :attendance attended))))
        (should (equal (nreverse order)
                       '(preview approval execute)))))))

(ert-deftest authority/default-policy-gates-reads-and-side-effects ()
  (should (eq 'ask
              (plist-get (superchat-authority-decide
                          "read-file" '("README.md") 'attended)
                         :decision)))
  (should (eq 'ask
              (plist-get (superchat-authority-decide
                          "write-file" '("out.txt" "x") 'attended)
                         :decision)))
  (should (eq 'deny
              (plist-get (superchat-authority-decide
                          "write-file" '("out.txt" "x") 'unattended)
                         :decision)))
  (should (eq 'deny
              (plist-get (superchat-authority-decide
                          "read-file" '("README.md") 'unattended)
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
                 "write-file"
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
                 "write-file"
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
           (called nil))
      (unwind-protect
          (progn
            (should
             (equal "ok"
                    (superchat-tool-execute
                     "write-file"
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
                     "write-file"
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
                     "write-file"
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
                     "write-file"
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
                 "write-file"
                 (lambda (&rest _args) (setq called t))
                 '("out.txt" "body")
                 :context '(:session-id "session-1" :attendance attended)))))
      (should-not called)
      (should
       (equal (mapcar (lambda (event) (plist-get event :type)) events)
              '("tool.requested" "tool.failed"))))))

(ert-deftest authority/executor-binds-approval-to-normalized-request ()
  (authority-test--capture-events
    (let (approved-request called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "write-file"
               (lambda (&rest args) (setq called-args args) "written")
               '("out.txt" "body")
               :context
               `(:session-id "session-1"
                 :attendance attended
                 :execution-id "execution-1"
                 :approval-function
                 ,(lambda (request)
                    (setq approved-request request)
                    t)))))
      (should (equal called-args '("out.txt" "body")))
      (should
       (equal (plist-get approved-request :request-hash)
              (superchat-authority-approval-hash
               "write-file" '("out.txt" "body")
               '((:effect local-write :resource file :scope "out.txt"))
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

(ert-deftest authority/executor-approval-cannot-change-execution-request ()
  (authority-test--capture-events
    (let ((path (copy-sequence "safe.txt"))
          called-args)
      (should
       (equal "written"
              (superchat-tool-execute
               "write-file"
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
               "write-file"
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
               "write-file"
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
               "write-file"
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
                 "write-file"
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
                 "write-file"
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
               "write-file"
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
                 "write-file"
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

(provide 'test-authority)

;;; test-authority.el ends here
