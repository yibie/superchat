;;; test-skills.el --- Tests for superchat-skills -*- lexical-binding: t; -*-

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat)
(require 'superchat-skills)

(defun skills-test--deliver (request text &optional status tape-id)
  "Deliver TEXT through REQUEST's callback Sink (S13a legacy-shape helper)."
  (let ((sink (plist-get request :sink)))
    (should (superchat-sink-p sink))
    (if (memq status '(nil completed))
        (superchat-sink-emit sink 'final nil (list :text text :tape-id tape-id))
      (superchat-sink-emit sink 'error nil (list :text text :status status :tape-id tape-id)))))

(ert-deftest skills/requiring-superchat-does-not-initialize-the-skills-directory ()
  "A fresh data directory remains untouched until the Skills feature is used."
  (let* ((emacs (or (executable-find invocation-name)
                    (executable-find "emacs")))
         (root (file-name-directory (locate-library "superchat")))
         (home (make-temp-file "superchat-skills-load-home-" t))
         (data (expand-file-name "superchat-data" home))
         (output (generate-new-buffer " *superchat-skills-load*")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (let ((process-environment
                 (cons (concat "HOME=" home) process-environment)))
            (let ((status
                   (call-process
                    emacs nil output nil "-Q" "--batch" "-L" root "--eval"
                    (prin1-to-string
                     `(progn
                        (setq native-comp-enable-subr-trampolines nil
                              superchat-data-directory ,data)
                        (require 'superchat)
                        (when (file-exists-p superchat-data-directory)
                          (error "require created %s"
                                 superchat-data-directory)))))))
              (unless (= 0 status)
                (ert-fail (format "Fresh require child exited %s:\n%s"
                                  status
                                  (with-current-buffer output
                                    (buffer-string))))))))
      (when (buffer-live-p output) (kill-buffer output))
      (delete-directory home t))))

(ert-deftest test-skills-parse-input-explicit ()
  "Test explicit skill syntax parsing."
  ;; Basic case
  (let ((result (superchat-skills-parse-input ">code-review #file.el test")))
    (should (equal (car result) "code-review"))
    (should (equal (cdr result) "#file.el test")))
  ;; No args
  (let ((result (superchat-skills-parse-input ">planning")))
    (should (equal (car result) "planning"))
    (should (equal (cdr result) "")))
  ;; Not a skill prefix
  (let ((result (superchat-skills-parse-input "code-review test")))
    (should (null result))))

(ert-deftest test-skills-parse-registry ()
  "Test AGENTS.md registry parsing."
  (let* ((test-content "## Skill Registry\n\n### code-review\n- **name**: code-review\n- **context**: Review code quality\n- **triggers**: [\"review\", \"check\"]\n- **file**: skills/code-review.md\n\n### refactor\n- **name**: refactor\n- **context**: Refactor code\n- **triggers**: [\"refactor\", \"优化\"]\n- **file**: skills/refactor.md\n- **note**: Important\n")
         (skills (superchat-skills--parse-registry test-content)))
    (should (= (length skills) 2))
    ;; Check first skill
    (let ((first (car skills)))
      (should (equal (plist-get first :name) "code-review"))
      (should (equal (plist-get first :context) "Review code quality"))
      (should (equal (plist-get first :file) "skills/code-review.md"))
      (should (equal (plist-get first :triggers) '("review" "check"))))
    ;; Check second skill
    (let ((second (cadr skills)))
      (should (equal (plist-get second :name) "refactor"))
      (should (equal (plist-get second :note) "Important")))))

(ert-deftest test-skills-keyword-matching ()
  "Test keyword-based skill matching."
  ;; Temporarily lower threshold for testing
  (let ((superchat-skills-match-confidence-threshold 0.3)
        (test-skills (list (list :name "code-review"
                                  :context "Review code"
                                  :triggers '("review" "check" "看看")
                                  :file "skills/code-review.md")
                            (list :name "refactor"
                                  :context "Refactor code"
                                  :triggers '("refactor" "优化" "重构")
                                  :file "skills/refactor.md"))))
    ;; Test match
    (let ((match1 (superchat-skills--match-by-keywords "帮我 review 这段代码" test-skills)))
      (should match1)
      (should (equal (car match1) "code-review")))
    ;; Test match refactor
    (let ((match2 (superchat-skills--match-by-keywords "需要重构这个函数" test-skills)))
      (should match2)
      (should (equal (car match2) "refactor")))
    ;; Test no match
    (let ((match3 (superchat-skills--match-by-keywords "今天天气怎么样" test-skills)))
      (should (null match3)))))

(ert-deftest test-skills-async-match-routes-through-runtime ()
  "Async semantic matching must use the shared Runtime lifecycle."
  (let (submitted callback-result session-id submitted-buffer-valid
                 (default-before (get-buffer "*superchat*")))
    (cl-letf (((symbol-function 'superchat-skills--get-registry)
               (lambda () '((:name "code-review"))))
              ((symbol-function 'superchat-skills--build-match-prompt)
               (lambda (_input _registry) "match prompt"))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq submitted request)
                 (setq submitted-buffer-valid
                       (eq (plist-get request :buffer)
                           (superchat-editor-workspace-validate-buffer
                            (plist-get request :buffer)
                            (plist-get request :session-id))))
                 (skills-test--deliver
                  request
                  "MATCH: code-review\nCONFIDENCE: 0.9\nREASONING: fit")
                 '(:status accepted))))
      (superchat-skills-match
       "review this"
       (lambda (skill confidence reasoning)
         (setq callback-result (list skill confidence reasoning))))
      (should (equal '("code-review" 0.9 "fit") callback-result))
      (should (equal "match prompt" (plist-get submitted :prompt)))
      (setq session-id (plist-get submitted :session-id))
      (should (and (stringp session-id) (not (string-empty-p session-id))))
      (should submitted-buffer-valid)
      (should (eq default-before (get-buffer "*superchat*")))
      (should-not (plist-get submitted :record-conversation))
      (let ((host (plist-get submitted :buffer)))
        (should-not (buffer-live-p host))))))

(defmacro skills-test--with-real-runtime (&rest body)
  "Run BODY with isolated Runtime state and only external boundaries stubbed."
  (declare (indent 0))
  `(let ((superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat--active-timeout-timer nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-closure-shadow-functions nil)
         (superchat-runtime--closure-observations nil))
     ,@body))

(defun skills-test--public-owned-buffers ()
  "Return all live hosts visible through the public owner query."
  (cl-remove-if-not
   (lambda (buffer)
     (condition-case nil
         (superchat-editor-workspace-owner-session buffer)
       (error nil)))
   (buffer-list)))

(ert-deftest skills/real-runtime-terminal-matrix-closes-once ()
  "Real Runtime terminal and callback conditions close the operation host."
  (dolist (case '(success cancel provider-error provider-quit
                  callback-error callback-quit))
    (skills-test--with-real-runtime
      (let ((before (skills-test--public-owned-buffers))
            (callbacks 0)
            caught)
        (cl-letf (((symbol-function 'superchat-skills--get-registry)
                   (lambda () '((:name "code-review"))))
                  ((symbol-function 'superchat-skills--build-match-prompt)
                   (lambda (&rest _args) "match prompt"))
                  ((symbol-function 'superchat-tape-append-event)
                   (let ((id 0)) (lambda (_event) (cl-incf id))))
                  ((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'superchat--llm-generate-answer)
                   (lambda (&rest _args)
                     (pcase case
                       ('cancel
                        (funcall superchat--llm-runtime-observer
                                 'terminal
                                 '(:status cancelled :response "cancelled")))
                       ('provider-error (error "provider error"))
                       ('provider-quit (signal 'quit '(provider-quit)))
                       (_
                        (funcall
                         superchat--llm-runtime-observer 'terminal
                         '(:status completed
                           :response "MATCH: code-review\nCONFIDENCE: 0.9\nREASONING: fit")))))))
          (setq caught
                (condition-case condition
                    (superchat-skills-match
                     "review"
                     (lambda (&rest _args)
                       (cl-incf callbacks)
                       (pcase case
                         ('callback-error (error "callback error"))
                         ('callback-quit (signal 'quit '(callback-quit))))))
                  ((error quit) condition)))
          (if (eq case 'provider-error)
              (should (equal "provider error"
                             (error-message-string caught)))
            (when (eq case 'provider-quit)
              (should (equal '(quit provider-quit) caught))))
          (should (= 1 callbacks))
          (should (equal before (skills-test--public-owned-buffers))))))))

(ert-deftest skills/real-runtime-preaccept-rejections-do-not-accumulate ()
  "Five public submission rejections leave no hidden operation hosts."
  (skills-test--with-real-runtime
    (let ((before (skills-test--public-owned-buffers)))
      (cl-letf (((symbol-function 'superchat-skills--get-registry)
                 (lambda () '((:name "code-review"))))
                ((symbol-function 'superchat-skills--build-match-prompt)
                 (lambda (&rest _args) "match prompt"))
                ;; Valid Skills requests have no naturally rejectable Runtime
                ;; validation branch; replace only the public submission seam.
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (_request) (error "Runtime rejected request"))))
        (dotimes (_ 5)
          (should-error
           (superchat-skills-match "review" #'ignore)
           :type 'error))
        (should (equal before (skills-test--public-owned-buffers)))))))

(ert-deftest skills/rejection-refusal-is-retained-retryable-and-secondary ()
  "Close refusal and diagnostic failure cannot replace the original rejection."
  (skills-test--with-real-runtime
    (let ((before (skills-test--public-owned-buffers)) caught survivor)
      (cl-letf (((symbol-function 'superchat-skills--get-registry)
                 (lambda () '((:name "code-review"))))
                ((symbol-function 'superchat-skills--build-match-prompt)
                 (lambda (&rest _args) "match prompt"))
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (_request)
                   (setq survivor
                         (car (cl-set-difference
                               (skills-test--public-owned-buffers)
                               before)))
                   (with-current-buffer survivor
                     (setq-local kill-buffer-query-functions
                                 (list (lambda () nil))))
                   (error "primary Runtime rejection")))
                ((symbol-function 'display-warning)
                 (lambda (&rest _args) (error "diagnostic failed"))))
        (setq caught
              (condition-case condition
                  (superchat-skills-match "review" #'ignore)
                (error condition))))
      (should (equal "primary Runtime rejection"
                     (error-message-string caught)))
      (should (buffer-live-p survivor))
      (let ((session
             (superchat-editor-workspace-owner-session survivor)))
        (should (eq 'retained
                    (superchat-editor-workspace-state session)))
        (with-current-buffer survivor
          (setq-local kill-buffer-query-functions nil))
        (should (eq 'closed
                    (superchat-session-workspace-close session))))
      (should (equal before (skills-test--public-owned-buffers))))))

(ert-deftest skills/reentrant-delivery-callback-and-close-are-once ()
  "A nested terminal delivery cannot reenter callback or public close."
  (let (delivery (callbacks 0) (closes 0))
    (cl-letf (((symbol-function 'superchat-skills--get-registry)
               (lambda () '((:name "code-review"))))
              ((symbol-function 'superchat-skills--build-match-prompt)
               (lambda (&rest _args) "match prompt"))
              ((symbol-function 'superchat-session-workspace-close)
               (lambda (_session) (cl-incf closes) 'closed))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq delivery (lambda (text status tape-id)
                                  (skills-test--deliver request text status tape-id)))
                 (funcall delivery
                          "MATCH: code-review\nCONFIDENCE: 0.9\nREASONING: fit"
                          'completed nil)
                 '(:run-id "run-1" :turn-id "turn-1"))))
      (superchat-skills-match
       "review"
       (lambda (&rest _args)
         (cl-incf callbacks)
         (funcall delivery
                  "MATCH: code-review\nCONFIDENCE: 0.9\nREASONING: fit"
                  'completed nil)))
      (should (= 1 callbacks))
      (should (= 1 closes)))))

(ert-deftest skills/match-envelope-is-ephemeral-and-toolless ()
  "S13a: the skills match submits an envelope on the `skills' surface with
`:tools none', a gensym Session, the raw input as `:content', the prepared
match prompt as `:context :prepared-prompt', and a callback Sink whose
`:final' delivers once and closes the child."
  (let (request (closes 0) result)
    (cl-letf (((symbol-function 'superchat-skills--get-registry)
               (lambda () '((:name "code-review"))))
              ((symbol-function 'superchat-skills--build-match-prompt)
               (lambda (&rest _args) "match prompt"))
              ((symbol-function 'superchat-session-workspace-close)
               (lambda (_session) (cl-incf closes) 'closed))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (req) (setq request req) '(:run-id "run-1" :turn-id "turn-1"))))
      (superchat-skills-match "review this"
                              (lambda (skill confidence reasoning)
                                (setq result (list skill confidence reasoning))))
      (should request)
      (should (eq 'skills (plist-get request :surface)))
      (should (eq 'none (plist-get request :tools)))
      (should (string-prefix-p "skills-match-" (plist-get request :session-id)))
      (should (equal "review this" (plist-get request :input)))
      (should (equal "match prompt" (plist-get request :prompt)))
      (should (plist-member request :record-conversation))
      (should-not (plist-get request :record-conversation))
      (should (superchat-sink-p (plist-get request :sink)))
      (skills-test--deliver request "MATCH: code-review\nCONFIDENCE: 0.9\nREASONING: fit")
      (should (equal '("code-review" 0.9 "fit") result))
      (should (= 1 closes)))))

(ert-deftest test-skills-exists-p ()
  "Test skill existence check."
  ;; This will depend on actual files present
  ;; Just test the function structure
  (should (or (booleanp (superchat-skills-exists-p "code-review"))
              (null (superchat-skills-exists-p "code-review")))))

(ert-deftest test-preset-from-frontmatter ()
  "Test parsing a SKILL.md frontmatter into a preset."
  (let ((content "---\nname: coder\ndescription: Autonomous coding agent\nversion: \"1.1\"\ntype: agent\ntools: [read-file, search-text, shell-command]\nmodel: claude-sonnet-4\n---\n\nYou are a coding assistant.\n")
        (preset (superchat-preset-from-frontmatter "---\nname: coder\ndescription: Autonomous coding agent\nversion: \"1.1\"\ntype: agent\ntools: [read-file, search-text, shell-command]\nmodel: claude-sonnet-4\n---\n\nYou are a coding assistant.\n")))
    (should preset)
    (should (string= (superchat-preset-name preset) "coder"))
    (should (eq (superchat-preset-type preset) 'agent))
    (should (equal (superchat-preset-tools preset)
                   '("read-file" "search-text" "shell-command")))
    (should (string= (superchat-preset-model preset) "claude-sonnet-4"))
    (should (string= (superchat-preset-version preset) "1.1"))
    (should (string-match-p "coding assistant" (superchat-preset-skill-body preset)))))

(ert-deftest test-preset-apply-to-turn ()
  "Test applying a preset to a superchat turn."
  (let ((preset (superchat-preset-from-plist
                 (list :name "coder"
                       :type 'agent
                       :tools '("read-file" "write-file")
                       :model "claude-sonnet-4")))
        (turn (superchat-turn-new ">coder write a function")))
    (superchat-preset-apply preset turn)
    (should (eq (superchat-preset-type (superchat-turn-preset turn)) 'agent))
    (should (string= (superchat-turn-target-model turn) "claude-sonnet-4"))
    (should (equal (superchat-turn-tools turn) '("read-file" "write-file")))))

(ert-deftest test-skill-load-returns-preset ()
  "Test that `superchat-skills-load' returns a preset struct."
  (let ((tmp-dir (make-temp-file "superchat-skills-test" t))
        (superchat-skills-directory nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "test-agent.md" tmp-dir)
            (insert "---\nname: test-agent\ndescription: Test agent\ntype: agent\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((preset (superchat-skills-load "test-agent")))
            (should (superchat-preset-p preset))
            (should (eq (superchat-preset-type preset) 'agent))
            (should (equal (superchat-preset-tools preset) '("read-file")))
            (should (string= (superchat-preset-skill-body preset) "Body.\n"))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-agent-activates-preset ()
  "Test that /agent activates a preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "coder.md" tmp-dir)
            (insert "---\nname: coder\ndescription: Coder agent\ntype: agent\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-agent "agent" "coder" nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'agent))
            (should (string= (superchat-preset-name superchat--active-preset) "coder"))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-plan-activates-preset ()
  "Test that /plan activates a planning preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "planning.md" tmp-dir)
            (insert "---\nname: planning\ndescription: Planning skill\ntype: plan\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-plan "plan" nil nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'plan))
            (should (member "read-file" (superchat-preset-tools superchat--active-preset)))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-plan-frontmatter-less-skill-falls-back-to-builtin ()
  "A same-named planning skill without a plan frontmatter must not
shadow the builtin: it defaults to prompt type and would activate
with an empty tool surface, so /plan falls back."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "planning.md" tmp-dir)
            (insert "# Planning Skill\n\nNo frontmatter here.\n"))
          (let ((result (superchat--cmd-plan "plan" nil nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'plan))
            (should (member "emit_plan"
                            (superchat-preset-tools
                             superchat--active-preset)))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-team-overlays-current-capability ()
  "`/team' changes Topology without replacing the active Capability."
  (let ((superchat--session-id nil)
        (superchat-llm-tool-names '("list_buffers")))
    (with-temp-buffer
      (superchat--cmd-code "code" nil nil nil nil)
      (let ((capability superchat--active-preset))
        (should (eq (plist-get
                     (superchat--cmd-team "team" nil nil nil nil)
                     :type)
                    :echo))
        (should (eq capability superchat--active-preset))
        (should (equal "code"
                       (superchat-preset-name superchat--active-preset)))
        (should (eq 'team superchat--active-topology))
        (let* ((turn (superchat-preset-apply
                      capability (superchat-turn-new "fix it")))
               (plan (superchat--request-plan-from-turn turn)))
          (should (equal "code" (plist-get plan :capability)))
          (should (eq 'team (plist-get plan :topology)))
          (should-not (plist-member plan :agent-mode))
          (should (member "list_buffers" (plist-get plan :tools)))
          (should (member "team_task" (plist-get plan :tools)))
          (should (member "workspace_read" (plist-get plan :tools)))
          (should (member "workspace_write" (plist-get plan :tools)))
          (should (member "workspace_info" (plist-get plan :tools)))
          (should (string-match-p
                   "coding assistant" (plist-get plan :system-prompt)))
          (should (string-match-p
                   "\\[Topology: team\\]"
                   (plist-get plan :system-prompt))))
        (superchat--cmd-single "single" nil nil nil nil)
        (should (eq 'single superchat--active-topology))
        (should (eq capability superchat--active-preset))
        (should (member "/team" (superchat--get-all-command-names)))
        (should (member "/workspace" (superchat--get-all-command-names)))))))

(ert-deftest test-agent-profile-capability-is-an-independent-session-axis ()
  "Session configuration keeps Profile identity separate from Capability."
  (let ((superchat--session-id "profile-session")
        (profile
         (superchat-preset-from-plist
          '(:name "coder-safe" :type agent :capability "coding")))
        event)
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (value) (setq event value) 1)))
      (with-temp-buffer
        (superchat--activate-capability profile)
        (should (eq profile superchat--active-preset))
        (should (equal "coding" superchat--active-capability))
        (should
         (equal "coding"
                (plist-get
                 (superchat--session-runtime-configuration)
                 :capability)))
        (should
         (equal "coder-safe"
                (plist-get (plist-get event :payload) :profile-id)))))))

(ert-deftest test-builtin-capabilities-select-workspace-tools-independently ()
  "Capability Toolsets expose only the Workspace effects they need."
  (let ((code (superchat--builtin-capability-preset "code"))
        (research (superchat--builtin-capability-preset "research"))
        (plan (superchat--builtin-capability-preset "plan")))
    (dolist (preset (list code research plan))
      (should (superchat-preset-capability preset))
      (should (member "workspace_read" (superchat-preset-tools preset)))
      (should (member "workspace_info" (superchat-preset-tools preset))))
    (should (member "workspace_write" (superchat-preset-tools code)))
    (should-not (member "workspace_write"
                        (superchat-preset-tools research)))
    (should-not (member "workspace_write"
                        (superchat-preset-tools plan)))))

(ert-deftest test-workspace-command-registers-session-workspace ()
  "`/workspace' connects the Session binding to the Workspace module."
  (let ((directory (make-temp-file "superchat-workspace-root" t))
        (superchat--session-id "workspace-session")
        registered)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (_event) 1))
                  ((symbol-function 'superchat-workspace-bind)
                   (lambda (workspace-id root session-id)
                     (setq registered
                           (list workspace-id root session-id)))))
          (with-temp-buffer
            (superchat--cmd-workspace
             "workspace" directory nil nil nil)
            (should
             (equal registered
                    (list
                     (plist-get superchat--workspace-binding :id)
                     (plist-get superchat--workspace-binding :root)
                     "workspace-session")))))
      (delete-directory directory t))))

(ert-deftest test-cmd-team-accepts-a-project-directory ()
  "`/team DIR' should also bind a Session Workspace."
  (let ((directory (make-temp-file "superchat-team-root" t))
        (superchat--session-id nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat-tools-default-directory nil)
          (let ((result (superchat--cmd-team "team" directory nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (eq 'team superchat--active-topology))
            (should (string-prefix-p
                     "workspace-"
                     (plist-get superchat--workspace-binding :id)))
            (should (equal superchat-tools-default-directory
                           (file-name-as-directory
                            (file-truename directory))))))
      (delete-directory directory t))))

(ert-deftest test-runtime-configuration-axes-reset-independently ()
  "Capability, Topology, Authority, and Workspace remain independent."
  (let ((directory (make-temp-file "superchat-config-root" t))
        (superchat--session-id "configuration-session")
        events)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (setq events (append events (list event)))
                     (length events))))
          (with-temp-buffer
            (superchat--cmd-research "research" nil nil nil nil)
            (superchat--cmd-team "team" nil nil nil nil)
            (superchat--cmd-authority
             "authority" "observer" nil nil nil)
            (superchat--cmd-workspace
             "workspace" directory nil nil nil)
            (let ((config (superchat--session-runtime-configuration)))
              (should (equal "research" (plist-get config :capability)))
              (should (eq 'team (plist-get config :topology)))
              (should (equal "observer"
                             (plist-get config :authority-policy)))
              (should (plist-get config :workspace-id)))
            (superchat--cmd-reset "reset" nil nil nil nil)
            (should (equal '(:capability "chat"
                             :topology single
                             :authority-policy "default")
                           (superchat--session-runtime-configuration)))
            (should
             (equal
              '("capability.activated"
                "topology.changed"
                "authority.policy.selected"
                "workspace.bound"
                "capability.activated"
                "topology.changed"
                "authority.policy.selected"
                "workspace.unbound")
              (mapcar (lambda (event) (plist-get event :type))
                      events)))))
      (delete-directory directory t))))

(ert-deftest test-session-restores-renamed-profile-by-stable-id ()
  "Session history resolves a Profile after its user-facing name changes."
  (let ((renamed
         (superchat-preset-from-plist
          '(:name "Coder Renamed" :profile-id "profile.coder"
            :type agent :capability coding :source skill))))
    (cl-letf (((symbol-function 'superchat-view-session-configuration)
               (lambda (_session-id)
                 '(:capability "coding"
                   :profile-id "profile.coder"
                   :capability-source "skill"
                   :topology "single"
                   :authority-policy "default")))
              ((symbol-function 'superchat-skills-exists-p)
               (lambda (_name) nil))
              ((symbol-function 'superchat-skills-get-available)
               (lambda () '("coder-renamed")))
              ((symbol-function 'superchat-skills-load)
               (lambda (name)
                 (and (equal name "coder-renamed") renamed))))
      (with-temp-buffer
        (superchat--restore-session-runtime-configuration "session-1")
        (should (eq renamed superchat--active-preset))
        (should
         (equal "profile.coder"
                (superchat-preset-profile-id
                 superchat--active-preset)))))))

(ert-deftest test-cmd-skill-activates-preset ()
  "Test that /skill activates any preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "review.md" tmp-dir)
            (insert "---\nname: review\ndescription: Review skill\ntype: prompt\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-skill "skill" "review" nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'prompt))
            (should (string= (superchat-preset-name superchat--active-preset) "review"))))
      (delete-directory tmp-dir t))))

(provide 'test-skills)

;;; test-skills.el ends here
