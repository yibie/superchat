;;; test-llm-backend.el --- Tests for the v0.5 llm.el backend integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for superchat's llm.el backend, dispatchers, and parser.
;; No live API calls are made — `llm-chat', `llm-chat-async',
;; `llm-chat-streaming',
;; `llm-name', `llm-make-tool', and `llm-make-chat-prompt' are mocked
;; via `cl-letf' to the real v0.7+ signatures so the dispatcher's actual
;; call patterns are exercised:
;;
;;   (llm-chat          provider prompt &optional multi-output)
;;   (llm-chat-streaming provider prompt partial-cb response-cb error-cb
;;                       &optional multi-output)
;;   (llm-make-chat-prompt text :tools TOOLS)
;;   (llm-name          provider)
;;   (llm-make-tool     &rest args)
;;
;; Tests do not require llm.el to be installed in the host Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-parser)
(require 'superchat-tools)

;;;---------------------------------------------
;;; Helpers
;;;---------------------------------------------

(defvar test-llm--native-make-tool
  (and (featurep 'llm) (symbol-function 'llm-make-tool)))
(defvar test-llm--native-raw-make-tool
  (and (featurep 'llm) (symbol-function 'make-llm-tool)))
(defvar test-llm--native-tool-name
  (and (featurep 'llm) (symbol-function 'llm-tool-name)))
(defvar test-llm--native-tool-description
  (and (featurep 'llm) (symbol-function 'llm-tool-description)))
(defvar test-llm--native-tool-args
  (and (featurep 'llm) (symbol-function 'llm-tool-args)))
(defvar test-llm--native-tool-function
  (and (featurep 'llm) (symbol-function 'llm-tool-function)))
(defvar test-llm--native-tool-async
  (and (featurep 'llm) (symbol-function 'llm-tool-async)))

(defun test-llm--make-tool (&rest args)
  "Return a native llm tool when available, otherwise a test double."
  (if test-llm--native-make-tool
      (apply test-llm--native-make-tool args)
    (cons 'mock-tool args)))

(defun test-llm--make-mock-backend (provider-sym)
  "Return a fake llm provider plist tagged with PROVIDER-SYM.
The result behaves like a `make-llm-openai'/`-claude'/etc. struct
well enough for `superchat-backend-show' and the cl-defgeneric
helpers to introspect it (via the mocked `llm-name')."
  (list 'mock-backend
        :chat-model "gpt-4o-mini"
        :provider provider-sym))

(defun test-llm--with-mock-llm (body)
  "Run BODY with stub llm.el bindings for the v0.7+ real API.
Mocks `llm-make-tool', `llm-name', `llm-make-chat-prompt', `llm-chat',
`llm-chat-streaming', `superchat--provider-chat-model', and
`superchat-mcp-get-tools'.  The dispatchers in superchat.el then
exercise their actual call patterns against these mocks, so signature
mismatches are caught at test time."
  (cl-letf* (((symbol-function 'llm-make-tool)
              (or test-llm--native-make-tool
                  #'test-llm--make-tool))
             ((symbol-function 'make-llm-tool)
              (or test-llm--native-raw-make-tool
                  #'test-llm--make-tool))
             ((symbol-function 'llm-tool-name)
              (or test-llm--native-tool-name
                  (lambda (tool) (plist-get (cdr tool) :name))))
             ((symbol-function 'llm-tool-description)
              (or test-llm--native-tool-description
                  (lambda (tool) (plist-get (cdr tool) :description))))
             ((symbol-function 'llm-tool-args)
              (or test-llm--native-tool-args
                  (lambda (tool) (plist-get (cdr tool) :args))))
             ((symbol-function 'llm-tool-function)
              (or test-llm--native-tool-function
                  (lambda (tool) (plist-get (cdr tool) :function))))
             ((symbol-function 'llm-tool-async)
              (or test-llm--native-tool-async
                  (lambda (tool) (plist-get (cdr tool) :async))))
             ((symbol-function 'llm-name)
              (lambda (backend)
                (or (plist-get (cdr backend) :provider)
                    (car backend))))
             ;; In production this is dispatched via cl-defgeneric to
             ;; `(llm-openai-chat-model provider)' once `llm-openai' is
             ;; loaded.  Tests don't load llm-openai, so we install a
             ;; plist-aware fallback that the mock backends (which are
             ;; plain plists) can answer.
             ((symbol-function 'superchat--provider-chat-model)
              (lambda (provider)
                (and (consp provider)
                     (plist-get (cdr provider) :chat-model))))
             ((symbol-function 'llm-make-chat-prompt)
              (lambda (text &rest args)
                (list 'mock-prompt
                      :text text
                      :tools (plist-get args :tools))))
             ((symbol-function 'llm-chat)
              (lambda (_backend _prompt &optional _multi-output)
                "[mocked llm-chat response]"))
             ((symbol-function 'llm-capabilities)
              (lambda (_provider)
                '(streaming tool-use streaming-tool-use)))
             ((symbol-function 'llm-chat-streaming)
              (lambda (_provider _prompt _partial-cb _response-cb _error-cb
                       &optional _multi-output)
                nil))
             ((symbol-function 'superchat-mcp-get-tools)
              (lambda () nil)))
    (funcall body)))

;;;---------------------------------------------
;;; Defcustoms
;;;---------------------------------------------

(ert-deftest test-llm-backend-defaults ()
  "All three new defcustoms exist with their documented defaults."
  (should (boundp 'superchat-llm-backend))
  (should (boundp 'superchat-llm-model))
  (should (boundp 'superchat-llm-streaming))
  (should (boundp 'superchat-llm-tool-names))
  (should (null superchat-llm-backend))
  (should (null superchat-llm-model))
  (should (eq superchat-llm-streaming t))
  ;; The default surface stays small: read verbs, memory, and the Eshell
  ;; bridge.  Everything else attaches by scope — Capability presets,
  ;; the team Topology overlay, or extension activation.
  (should (equal superchat-llm-tool-names
                 '("read-file" "list-files" "search-text" "read_buffer"
                   "memory_search" "eshell-command")))
  (should-not (member "render_ui" superchat-llm-tool-names)))

;;;---------------------------------------------
;;; superchat-backend-show
;;;---------------------------------------------

(ert-deftest test-backend-show-unconfigured ()
  "With `superchat-llm-backend' nil, /backend reports UNCONFIGURED."
  (let ((superchat-llm-backend nil))
    (let ((content (superchat-backend-show)))
      (should (stringp content))
      (should (string-match-p "UNCONFIGURED" content))
      (should (string-match-p "llm\\.el" content))
      (should (string-match-p "make-llm-openai" content)))))

(ert-deftest test-backend-show-with-mock-provider ()
  "With a mock backend, /backend reports the provider and model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai)))
       (let ((content (superchat-backend-show)))
         (should (string-match-p "openai" content))
         (should (string-match-p "gpt-4o-mini" content))
         (should (string-match-p "Streaming: yes" content)))))))

;;;---------------------------------------------
;;; superchat-model-list
;;;---------------------------------------------

(ert-deftest test-model-list-no-backend ()
  "Without a backend, /models still renders a useful help message."
  (let ((superchat-llm-backend nil)
        (superchat-manual-models nil))
    (let ((content (superchat-model-list)))
      (should (stringp content))
      (should (string-match-p "Available Models" content))
      (should (string-match-p "superchat-manual-models" content)))))

(ert-deftest test-model-list-with-manual-models ()
  "Manual model list is shown in /models output."
  (let ((superchat-llm-backend nil)
        (superchat-manual-models '("gpt-4o-mini" "claude-3-5-sonnet")))
    (let ((content (superchat-model-list)))
      (should (string-match-p "Manual configuration" content))
      (should (string-match-p "gpt-4o-mini" content))
      (should (string-match-p "claude-3-5-sonnet" content)))))

(ert-deftest test-model-list-with-backend ()
  "Configured backend's :chat-model is shown as the current model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-manual-models nil))
       (let ((content (superchat-model-list)))
         (should (string-match-p "Configured llm backend" content))
         (should (string-match-p "gpt-4o-mini" content)))))))

(ert-deftest test-model-list-override-model ()
  "`superchat-llm-model' overrides the backend's :chat-model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-llm-model "gpt-4o")
           (superchat-manual-models nil))
       (let ((content (superchat-model-list)))
         (should (string-match-p "Current model: gpt-4o" content)))))))

(ert-deftest test-effective-backend-copies-openai-compatible-subclass ()
  "A model override preserves an OpenAI-compatible provider's subtype and key."
  (unless (require 'llm-deepseek nil t)
    (ert-skip "llm-deepseek is unavailable"))
  (let* ((key-function (lambda () "not-a-live-key"))
         (superchat-llm-backend
          (make-llm-deepseek
           :key key-function
           :chat-model "deepseek-v4-pro"))
         (superchat-llm-model nil)
         (effective
          (superchat--effective-llm-backend "deepseek-v4-flash")))
    (should (llm-deepseek-p effective))
    (should-not (eq effective superchat-llm-backend))
    (should (equal "deepseek-v4-flash"
                   (llm-deepseek-chat-model effective)))
    (should (equal "deepseek-v4-pro"
                   (llm-deepseek-chat-model superchat-llm-backend)))
    (should (eq key-function (llm-deepseek-key effective)))
    (should (equal "https://api.deepseek.com"
                   (llm-deepseek-url effective)))))

;;;---------------------------------------------
;;; superchat--is-ollama-backend-p
;;;---------------------------------------------

(ert-deftest test-ollama-detection-ollama-provider ()
  "An ollama provider is correctly identified as Ollama."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'ollama)))
       (should (superchat--is-ollama-backend-p))))))

(ert-deftest test-ollama-detection-non-ollama-provider ()
  "A non-ollama provider is correctly identified as NOT Ollama."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai)))
       (should-not (superchat--is-ollama-backend-p))))))

(ert-deftest test-ollama-detection-nil-backend ()
  "Nil backend is not an Ollama backend."
  (let ((superchat-llm-backend nil))
    (should-not (superchat--is-ollama-backend-p))))

;;;---------------------------------------------
;;; superchat-llm-tools-list
;;;---------------------------------------------

(ert-deftest test-tools-list-empty-when-llm-missing ()
  "When `llm-make-tool' is not fbound, the registry stays nil."
  (let ((superchat-llm-tools-list nil))
    (cl-letf (((symbol-function 'llm-make-tool) nil))
      (let ((result (superchat-get-llm-tools)))
        (should (null result))))))

(ert-deftest test-tools-list-populates-default-small-tool-surface ()
  "With a mock `llm-make-tool', reload registers the default small tool surface."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tool-names
            '("read-file" "list-files" "search-text" "read_buffer")))
       (let ((tools (superchat-llm-tools-reload)))
         (should (listp tools))
         (should (= 4 (length tools)))
         ;; After reload, the cache var is populated (not nil).
         (should superchat-llm-tools-list)
         (should (= 4 (length superchat-llm-tools-list)))
         (should (equal '("read-file" "list-files" "search-text" "read_buffer")
                        (mapcar #'llm-tool-name
                                superchat-llm-tools-list))))))))

(ert-deftest test-workspace-tool-exposes-artifact-revision-contract ()
  "Workspace writes expose and forward Artifact conflict arguments."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("workspace_write")))
       (let* ((tool (car (superchat-llm-tools-reload)))
              (args (llm-tool-args tool)))
         (should (equal '("content" "append" "artifact_id"
                          "expected_revision")
                        (mapcar (lambda (arg) (plist-get arg :name)) args)))))))
  (cl-letf (((symbol-function 'superchat-workspace-write)
             (lambda (&rest args) args)))
    (should (equal '("new" t "artifact-a" "revision-a")
                   (superchat-tool-workspace-write
                    "new" t "artifact-a" "revision-a")))))

(ert-deftest test-on-demand-attaches-tools-for-render-ui-intent ()
  "A direct `render_ui' request gets tools without enabling them globally."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (should (superchat--should-attach-tools-p
             "Use render_ui to draw a comparison table."))
    (should-not (superchat--should-attach-tools-p
                 "Explain the difference between Redux and Zustand."))))

(ert-deftest test-on-demand-attaches-tools-for-eshell-intent ()
  "An explicit `eshell-command' request gets the command bridge on demand."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (should (superchat--should-attach-tools-p
             "Use eshell-command to run pwd."))
    (should (superchat--should-attach-tools-p
             "Use eshell command to list the current directory."))
    (should-not (superchat--should-attach-tools-p
                 "Explain what Eshell is."))))

(ert-deftest test-on-demand-attaches-tools-for-team-intent ()
  "Explicit team tool names attach the collaboration surface on demand."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (dolist (input '("Use team_task to create the work."
                     "Call team_message after the review."
                     "Use delegate_to_subagent for research."
                     "Use delegate_to_subagent_parallel for both tasks."))
      (should (superchat--should-attach-tools-p input)))
    (should-not (superchat--should-attach-tools-p
                 "Explain how agent teamwork could work."))))

(ert-deftest test-render-ui-intent-hides-synthesis-tools ()
  "An explicit render request keeps agent-written UI tools out of the turn."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil)
        (superchat-llm-synthesized-tools
         (list (test-llm--make-tool
                :name "render-state-comparison-vui")))
        (superchat--llm-tool-names-signature nil))
    (cl-letf (((symbol-function 'llm-tool-name)
               (or test-llm--native-tool-name
                   (lambda (tool) (plist-get (cdr tool) :name))))
             ((symbol-function 'superchat-get-llm-tools)
               (lambda (&optional _tool-names)
                 (list (test-llm--make-tool :name "render_ui")
                       (test-llm--make-tool :name "define_tool"))))
             ((symbol-function 'superchat-mcp-get-tools)
               (lambda () nil)))
      (should (equal '("render_ui")
                     (mapcar #'llm-tool-name
                             (superchat--collect-llm-tools
                              "Use render_ui with vui-table.")))))))

(ert-deftest test-tools-list-reloads-when-allowlist-changes ()
  "Changing `superchat-llm-tool-names' must invalidate the cached tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names '("read-file")))
       (superchat-get-llm-tools)
       (setq superchat-llm-tool-names
             '("read-file" "define_tool"))
       (should (member "define_tool"
                       (mapcar #'llm-tool-name
                               (superchat-get-llm-tools))))))))

(ert-deftest test-request-tool-profile-expands-global-allowlist ()
  "An explicit preset tool list should not require global configuration."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names '("read-file"))
           (superchat-llm-synthesized-tools
            (list (test-llm--make-tool :name "grep-subagent"))))
       (should
        (equal
         '("team_task" "delegate_to_subagent")
         (mapcar #'llm-tool-name
                 (superchat--collect-llm-tools
                  "team request"
                  '("team_task" "delegate_to_subagent")))))
       (should (equal superchat-llm-tool-names '("read-file")))))))

(ert-deftest test-tools-list-can-expose-all-builtins ()
  "The larger legacy tool surface remains available behind an explicit opt-in."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tools-list nil)
            (superchat-llm-tool-names 'all)
            (expected-count 35))
       (let ((tools (superchat-llm-tools-reload)))
         ;; 34 core tools + define_tool (opt-in, see superchat-synthesis.el).
         ;; `render_ui' is included here because `all' explicitly opts into
         ;; every built-in surface.
         (should (= expected-count (length tools)))
         (should (= expected-count (length superchat-llm-tools-list))))))))

(ert-deftest test-eshell-command-executor-refusal-never-invokes-tool ()
  "The shared Executor never runs Eshell after user refusal."
  (cl-letf (((symbol-function 'superchat-tape-append-event)
             (lambda (_event) 1))
            ((symbol-function 'eshell-command-result)
             (lambda (&rest _args)
               (ert-fail "refused Eshell command was executed"))))
    (should
     (equal
      "[Tool call cancelled by user]"
      (superchat-tool-execute
       "eshell-command"
       #'superchat-tool-eshell-command
       '("pwd")
       :context
       '(:session-id "session-1"
         :attendance attended
         :approval-function ignore))))))

(ert-deftest test-eshell-command-uses-tool-directory-and-strips-properties ()
  "The Eshell bridge delegates in the effective tool directory."
  (let ((captured-directory nil)
        (superchat-tools-default-directory "/tmp"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'superchat--extend-timeout) #'ignore)
              ((symbol-function 'eshell-command-result)
               (lambda (_command status-var)
                 (set status-var 0)
                 (setq captured-directory default-directory)
                 (propertize "ok" 'face 'bold))))
      (should (equal "ok" (superchat-tool-eshell-command "pwd")))
      (should (equal "/tmp/" captured-directory)))))

(ert-deftest test-eshell-command-reports-nonzero-status ()
  "A failed Eshell command becomes model-correctable tool feedback."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
            ((symbol-function 'superchat--extend-timeout) #'ignore)
            ((symbol-function 'eshell-command-result)
             (lambda (_command status-var)
               (set status-var 127)
               "missing-command: command not found\n")))
    (let ((result (superchat-tool-eshell-command "missing-command")))
      (should (string-prefix-p "Error:" result))
      (should (string-match-p "status 127" result))
      (should (string-match-p "command not found" result)))))

(ert-deftest test-delegate-tool-discovers-custom-agent-presets ()
  "Delegate tool advertises custom agent skills, but not prompt skills."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (with-temp-file (expand-file-name "reviewer.md" tmp-dir)
               (insert "---\nname: reviewer\ndescription: Reviews patches\ntype: agent\n---\nReview carefully.\n"))
             (with-temp-file (expand-file-name "summarize.md" tmp-dir)
               (insert "---\nname: summarize\ndescription: Summarizes text\ntype: prompt\n---\nSummarize.\n"))
             (let* ((tools (superchat-llm-tools-reload))
                    (delegate (car tools))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "reviewer — Reviews patches" description))
               (should-not (string-match-p "summarize" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-delegate-tool-advertises-callable-skill-filename ()
  "Registry advertises the filename accepted by the sub-agent resolver."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (with-temp-file (expand-file-name "reviewer.md" tmp-dir)
               (insert "---\nname: fancy-name\ndescription: Reviews patches\ntype: agent\n---\nReview.\n"))
             (let* ((delegate (car (superchat-llm-tools-reload)))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "reviewer — Reviews patches" description))
               (should-not (string-match-p "fancy-name" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-delegate-tool-normalizes-mixed-case-and-builtin-collisions ()
  "Registry names remain callable on case-sensitive filesystems."
  (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t)))
    (unwind-protect
        (let ((superchat-skills-directory tmp-dir))
          (with-temp-file (expand-file-name "MyAgent.md" tmp-dir)
            (insert "---\nname: mixed\ndescription: Mixed case\ntype: agent\n---\nBody.\n"))
          (with-temp-file (expand-file-name "Researcher.md" tmp-dir)
            (insert "---\nname: collision\ndescription: Collision\ntype: agent\n---\nBody.\n"))
          (let ((registry (superchat--subagent-registry)))
            (should (assoc "myagent" registry))
            (should (= 1 (length (seq-filter
                                  (lambda (entry)
                                    (string= "researcher" (car entry)))
                                  registry))))
            (should (equal "mixed"
                           (superchat-preset-name
                            (superchat--subagent-preset "myagent"))))))
      (delete-directory tmp-dir t))))

(ert-deftest test-delegate-tool-cache-refreshes-after-agent-file-change ()
  "Cached delegate descriptions refresh when agent skills change."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat--subagent-registry-signature nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (superchat-get-llm-tools)
             (with-temp-file (expand-file-name "new-agent.md" tmp-dir)
               (insert "---\nname: new-agent\ndescription: Newly added\ntype: agent\n---\nBody.\n"))
             (let* ((delegate (car (superchat-get-llm-tools)))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "new-agent — Newly added" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-tools-list-cached-after-first-call ()
  "`superchat-get-llm-tools' returns the cached list on subsequent calls."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil))
       (superchat-get-llm-tools)
       (let ((first-call superchat-llm-tools-list))
         (should first-call)
         (should (eq first-call (superchat-get-llm-tools))))))))

(ert-deftest test-build-prompt-uses-native-multipart-for-images ()
  "Image context is sent as unibyte `llm-media' in a multipart prompt."
  (let ((image (make-temp-file "superchat-image-" nil ".png"))
        captured-content captured-data)
    (unwind-protect
        (progn
          (with-temp-file image
            (set-buffer-multibyte nil)
            (insert (unibyte-string #x89 #x50 #x4e #x47)))
          (cl-letf (((symbol-function 'make-llm-media)
                     (lambda (&rest args)
                       (setq captured-data (plist-get args :data))
                       (cons 'media args)))
                    ((symbol-function 'llm-make-multipart)
                     (lambda (&rest parts) (cons 'multipart parts)))
                    ((symbol-function 'llm-make-chat-prompt)
                     (lambda (content &rest _args)
                       (setq captured-content content))))
            (superchat--build-llm-prompt
             "Describe this image" nil nil nil (list image)))
          (should (eq (car captured-content) 'multipart))
          (should (equal (cadr captured-content) "Describe this image"))
          (should (equal (plist-get (cdr (caddr captured-content)) :mime-type)
                         "image/png"))
          (should-not (multibyte-string-p captured-data)))
      (delete-file image))))

(ert-deftest test-tool-error-feedback-recognizes-tool-failure-markers ()
  "Common tool failure strings become a corrective model message."
  (let* ((result '(:tool-results
                   (("write-file" . "❌ Content too large for quick-write"))))
         (feedback (superchat--llm-tool-error-feedback result)))
    (should (stringp feedback))
    (should (string-match-p "write-file" feedback))
    (should (string-match-p "Content too large" feedback))
    (should (string-match-p "retry" feedback))))

;;;---------------------------------------------
;;; superchat--llm-generate-answer (async streaming)
;;;---------------------------------------------

(ert-deftest test-final-render-keeps-tool-transcript ()
  "Formatting the final answer must not erase tool records above it."
  (let ((superchat-buffer-name " *superchat-tool-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (insert "* User: inspect this\n")
          (setq superchat--response-start-marker (point-marker)
                superchat--assistant-response-start-marker nil)
          (insert "Assistant is thinking...\n")
          (insert "** Tool result: read-file\n#+begin_example\ncontents\n#+end_example\n")
          (superchat--prepare-assistant-response-area)
          (goto-char (point-max))
          (insert "raw final answer")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore)
                    ((symbol-function 'superchat--insert-prompt) #'ignore))
            (superchat--process-llm-result "formatted final answer"))
          (let ((rendered (buffer-string)))
            (should (string-match-p "Tool result: read-file" rendered))
            (should (string-match-p "contents" rendered))
            (should (string-match-p "formatted final answer" rendered))
            (should (< (string-match "Tool result: read-file" rendered)
                       (string-match "formatted final answer" rendered)))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-tool-round-p-continues-when-model-narrates ()
  "Prose alongside a tool call must NOT be mistaken for the final answer.
Models routinely narrate the call they are about to make (\"Let me read
that file first.\") in the same round as the tool use.  Treating that
narration as the final answer strands the tool result unseen and hands
the user the narration instead of an answer."
  ;; Tool call with narration -> still a tool round.
  (should (superchat--llm-tool-round-p
           '(:text "Let me read that file first."
             :tool-results (("read-file" . "contents")))))
  ;; Tool call with no prose -> tool round.
  (should (superchat--llm-tool-round-p
           '(:text "" :tool-results (("read-file" . "contents")))))
  ;; Bare tool-result alist (non-multi-output form) -> tool round.
  (should (superchat--llm-tool-round-p '((read-file . "contents"))))
  ;; Text and NO tool call -> this is the final answer; stop.
  (should-not (superchat--llm-tool-round-p '(:text "The answer is 42.")))
  (should-not (superchat--llm-tool-round-p "The answer is 42.")))

(ert-deftest test-async-dispatcher-continues-when-model-narrates ()
  "The loop keeps going when round 1 carries prose *and* a tool call."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if (= (length requests) 1)
                        (funcall response-cb
                                 '(:text "Let me read that file first."
                                   :tool-results (("fake-tool" . "tool result"))))
                      (funcall response-cb
                               '(:text "final answer after tool"))))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 (length requests)))
         (should (string= final-result "final answer after tool")))))))

(ert-deftest test-async-dispatcher-feeds-tool-error-back-to-llm ()
  "A failed tool result gets an explicit correction message on the next round."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")))
           (requests 0)
           (feedback nil)
           final-result)
       (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (if (= requests 1)
                        (funcall response-cb
                                 '(:text "I tried it"
                                   :tool-results
                                   (("render_ui" .
                                     "genui: constructor vui-table is not allowed"))))
                      (funcall response-cb "corrected answer")))))
         (superchat--llm-generate-answer
          "Use render_ui with vui-table"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 requests))
         (should (= 1 (length feedback)))
         (should (eq 'user (cadar feedback)))
         (should (string-match-p "render_ui" (caar feedback)))
         (should (string-match-p "vui-table" (caar feedback)))
         (should (string-match-p "retry\\|重试" (caar feedback)))
         (should (string= "corrected answer" final-result)))))))

(ert-deftest test-async-dispatcher-retries-after-tool-protocol-error ()
  "Malformed tool arguments are fed back through the same bounded retry path."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")))
           (requests 0)
           (feedback nil)
           final-result)
       (cl-letf (((symbol-function 'superchat--llm-drop-dangling-tool-use)
                  #'ignore)
                 ((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           error-cb &optional _multi-output)
                    (cl-incf requests)
                    (if (= requests 1)
                        (funcall error-cb
                                 'llm-tool-missing-argument
                                 "Missing required argument 'components' for tool 'render_ui'")
                      (funcall response-cb "corrected answer")))))
         (superchat--llm-generate-answer
          "Use render_ui"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 requests))
         (should (= 1 (length feedback)))
         (should (eq 'user (cadar feedback)))
         (should (string-match-p "components" (caar feedback)))
         (should (string-match-p "retry" (caar feedback)))
         (should (string= "corrected answer" final-result)))))))

(ert-deftest test-plz-success-callback-tool-errors-reach-error-callback ()
  "Tool parsing errors raised after HTTP success must not strand the Run."
  (let (observed)
    (superchat--llm-request-plz-propagate-callback-errors
     (lambda (_url &rest args)
       (funcall (plist-get args :on-success) 'response))
     "https://example.invalid"
     :on-success
     (lambda (_response)
       (signal 'llm-tool-call-error '("truncated JSON")))
     :on-error
     (lambda (type message)
       (setq observed (list type message))))
    (should (eq 'llm-tool-call-error (car observed)))
    (should (string-match-p "truncated JSON" (cadr observed)))))

(ert-deftest test-async-dispatcher-does-not-retry-blocked-tool ()
  "A user/policy refusal gets one tool-free explanation, not another call."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")))
           (requests 0)
           (multi-output-values nil)
           (feedback nil)
           tools-disabled
           final-result)
       (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'superchat--llm-disable-prompt-tools)
                  (lambda (_prompt) (setq tools-disabled t)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional multi-output)
                    (cl-incf requests)
                    (push multi-output multi-output-values)
                    (if (= requests 1)
                        (funcall response-cb
                                 '(:tool-results
                                   (("render_ui" .
                                     "[Tool call denied by permission hook]"))))
                      (funcall response-cb "acknowledged")))))
         (superchat--llm-generate-answer
          "Use render_ui"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 requests))
         (should (equal '(nil t) multi-output-values))
         (should tools-disabled)
         (should (= 1 (length feedback)))
         (should (string-match-p "do not retry" (caar feedback)))
         (should (string= "acknowledged" final-result)))))))

(ert-deftest test-render-keeps-tool-transcript-when-model-narrates ()
  "Narration before a tool call must not drag the tool records into the
final rewrite.  The assistant marker is anchored at the run's first text
chunk, so a tool record appended after that narration sits inside the
region `superchat--process-llm-result' deletes — unless the round's text
is closed out when the tool record lands."
  (let ((superchat-buffer-name " *superchat-narrate-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (insert "* User: inspect this\n")
          (setq superchat--response-start-marker (point-marker)
                superchat--assistant-response-start-marker nil
                superchat--current-response-parts nil)
          (insert "Assistant is thinking...\n")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore)
                    ((symbol-function 'superchat--insert-prompt) #'ignore))
            ;; Round 1: the model narrates, THEN calls a tool.
            (superchat--stream-llm-result "Let me read that file first.")
            (superchat--agent-render-tool-call "read-file" '("README.md"))
            (superchat--agent-render-tool-result "read-file" "contents")
            ;; Round 2: the model answers.
            (superchat--stream-llm-result "raw final answer")
            (superchat--process-llm-result "The answer is 42."))
          (let ((rendered (buffer-string)))
            (should (string-match-p "Let me read that file first." rendered))
            (should (string-match-p "Tool call: read-file" rendered))
            (should (string-match-p "contents" rendered))
            (should (string-match-p "The answer is 42." rendered))
            ;; Order: narration -> tool record -> final answer.
            (should (< (string-match "Let me read that file first." rendered)
                       (string-match "Tool call: read-file" rendered)))
            (should (< (string-match "Tool call: read-file" rendered)
                       (string-match "The answer is 42." rendered)))
            ;; The raw streamed text of the last round is rewritten, not kept.
            (should-not (string-match-p "raw final answer" rendered))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-async-dispatcher-streams-and-finalizes ()
  "Async dispatcher streams chunks via partial-cb and finalizes with
the authoritative text from response-cb (not the joined chunks)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil)
           (streamed-chunks '()))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda () nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial-cb response-cb
                           _error-cb &optional _multi-output)
                    ;; Partial: push chunks in real streaming order.
                    (funcall partial-cb "Hello, ")
                    (funcall partial-cb "world")
                    ;; Response: hand the dispatcher the authoritative
                    ;; final text.  This is what llm.el does for a
                    ;; non-tool call.
                    (funcall response-cb "streamed result"))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (chunk) (push chunk streamed-chunks))
          nil
          nil)
         ;; Authoritative final text wins over the chunk join.
         (should (string= final-result "streamed result"))
         ;; Streamed chunks are still delivered to the user for UX.
         (should (= 2 (length streamed-chunks)))
         (should (member "Hello, " streamed-chunks))
         (should (member "world" streamed-chunks)))))))

(ert-deftest test-async-dispatcher-falls-back-for-nonstreaming-tool-use ()
  "Tool requests use `llm-chat-async' unless streaming tool use is advertised."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'deepseek))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (async-calls 0)
           (streaming-calls 0)
           final-result)
       (cl-letf (((symbol-function 'llm-capabilities)
                  (lambda (_provider) '(streaming tool-use)))
                 ((symbol-function 'llm-chat-async)
                  (lambda (_provider _prompt response-cb _error-cb
                           &optional _multi-output)
                    (cl-incf async-calls)
                    (if (= async-calls 1)
                        (funcall response-cb
                                 '(:tool-results
                                   (("fake-tool" . "tool result"))))
                      (funcall response-cb "final answer"))))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (&rest _args)
                    (cl-incf streaming-calls))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          #'ignore)
         (should (= 2 async-calls))
         (should (= 0 streaming-calls))
         (should (equal "final answer" final-result)))))))

(ert-deftest test-async-dispatcher-restores-originating-view ()
  "Delayed callbacks should run in the View that started the request."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((target-name (generate-new-buffer-name " *superchat-origin*"))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            partial-cb response-cb seen)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-get-llm-tools)
                      (lambda () nil))
                     ((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt partial response _error
                               &optional _multi-output)
                        (setq partial-cb partial
                              response-cb response))))
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (superchat--llm-generate-answer
                  "hello"
                  (lambda (_answer)
                    (push (list :final (buffer-name)
                                superchat-buffer-name)
                          seen))
                  (lambda (_chunk)
                    (push (list :stream (buffer-name)
                                superchat-buffer-name)
                          seen)))))
             ;; Simulate llm.el invoking callbacks after the initiating
             ;; dynamic binding has unwound and another buffer is current.
             (with-temp-buffer
               (funcall partial-cb "chunk")
               (funcall response-cb "done"))
             (should (equal (nreverse seen)
                            `((:stream ,target-name ,target-name)
                              (:final ,target-name ,target-name)))))
         (when (get-buffer target-name)
           (kill-buffer target-name)))))))

(ert-deftest test-async-dispatcher-reports-latest-runtime-request ()
  "Runtime observes each live request handle and one terminal outcome."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (calls 0)
           responses errors events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (push (list event value) events))))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial-cb response-cb
                             error-cb &optional _multi-output)
                      (cl-incf calls)
                      (push response-cb responses)
                      (push error-cb errors)
                      (intern (format "request-%d" calls)))))
           (superchat--llm-generate-answer
            "use the tool" #'ignore #'ignore)
           (funcall (car responses)
                    '(:tool-results (("fake-tool" . "result"))))
           (funcall (car responses) "final")
           ;; A provider callback arriving after completion is ignored.
           (funcall (car errors) 'error "late")
           (should
            (equal
             (nreverse events)
             '((request request-1)
               (request request-2)
               (terminal (:status completed :response "final")))))))))))

(ert-deftest test-agent-tool-restores-originating-view ()
  "Delayed sync and async agent tools should use their originating View."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((target-name
            (generate-new-buffer-name " *superchat-agent-origin*"))
           (superchat-agent-confirm-destructive nil)
           (superchat-authority-approval-function (lambda (_request) t))
           (original (lambda () "ok"))
           asyncp done-cb seen wrapped)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-tape-append-event)
                      (lambda (_event) 1))
                     ((symbol-function 'superchat--agent-render-tool-call)
                      (lambda (&rest _)
                        (push (list :call (buffer-name) superchat-buffer-name)
                              seen)))
                     ((symbol-function 'superchat--agent-render-tool-result)
                      (lambda (&rest _)
                        (push (list :result (buffer-name) superchat-buffer-name)
                              seen))))
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :name "mock"
                         :description "mock"
                         :args nil
                         :function original
                         :async asyncp)))))
             (with-temp-buffer
               (should (equal "ok"
                              (funcall (llm-tool-function wrapped)))))
             (should (equal (nreverse seen)
                            `((:call ,target-name ,target-name)
                              (:result ,target-name ,target-name))))
             (setq asyncp t
                   original (lambda (done) (setq done-cb done))
                   seen nil)
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :name "mock"
                         :description "mock"
                         :args nil
                         :function original
                         :async asyncp)))))
             (with-temp-buffer
               (funcall (llm-tool-function wrapped)
                        (lambda (_result)
                          (push (list :callback (buffer-name)
                                      superchat-buffer-name)
                                seen))))
             (with-temp-buffer
               (funcall done-cb "async-ok"))
             (should (equal (nreverse seen)
                            `((:call ,target-name ,target-name)
                              (:result ,target-name ,target-name)
                              (:callback ,target-name ,target-name)))))
         (when (get-buffer target-name)
           (kill-buffer target-name)))))))

(ert-deftest test-async-dispatcher-diffs-cumulative-plist-chunks ()
  "Multi-output plist chunks carry the FULL text so far (see
`llm-provider-utils-streaming-accumulate'); the dispatcher must forward
deltas, not re-insert the whole accumulated text on every chunk."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil)
           (streamed-chunks '()))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda () nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial-cb response-cb
                           _error-cb &optional _multi-output)
                    ;; llm.el accumulates: each partial carries all text
                    ;; received so far.
                    (funcall partial-cb '(:text "Hello, "))
                    (funcall partial-cb '(:text "Hello, world"))
                    (funcall partial-cb '(:text "Hello, world!"))
                    (funcall response-cb '(:text "Hello, world!")))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (chunk) (push chunk streamed-chunks))
          nil
          nil)
         (should (string= final-result "Hello, world!"))
         (should (equal (nreverse streamed-chunks)
                        '("Hello, " "world" "!"))))))))

(ert-deftest test-async-dispatcher-retries-after-tool-round ()
  "A tool round reuses its prompt and requests the model's final answer."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if (= (length requests) 1)
                        ;; llm.el has already appended this result to PROMPT.
                        (funcall response-cb
                                 '((fake-tool . "tool result")))
                      (funcall response-cb "final answer after tool")))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 (length requests)))
         (should (eq (car requests) (cadr requests)))
         (should (string= final-result "final answer after tool")))))))

(ert-deftest test-async-dispatcher-does-not-finalize-before-followup-response ()
  "A tool round must not finalize while its asynchronous follow-up runs.

Synchronous mocks hide this ordering error because the follow-up callback
can run re-entrantly before the first callback returns.  Real providers
return from the first callback after scheduling their next request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           first-response second-response final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if first-response
                        (setq second-response response-cb)
                      (setq first-response response-cb)))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 1 (length requests)))
         (should-not final-result)
         (funcall first-response '((fake-tool . "tool result")))
         (should (= 2 (length requests)))
         (should-not final-result)
         (funcall second-response "final answer after asynchronous tool")
         (should (string= final-result
                          "final answer after asynchronous tool")))))))

(ert-deftest test-async-dispatcher-recovers-missed-plz-sentinel ()
  "A dead plz request with an unprocessed response is finalized once."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'ollama))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (transport-buffer (generate-new-buffer " *test-plz-response*"))
           response-cb scheduled timer-cancelled sentinel-called final-result)
       (unwind-protect
           (cl-letf (((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt _partial-cb callback
                               _error-cb &optional _multi-output)
                        (setq response-cb callback)
                        'fake-plz-request))
                     ((symbol-function 'run-with-timer)
                      (lambda (_seconds _repeat function &rest args)
                        (setq scheduled
                              (lambda () (apply function args)))
                        'fake-watchdog-timer))
                     ((symbol-function 'cancel-timer)
                      (lambda (timer)
                        (setq timer-cancelled timer)))
                     ((symbol-function 'processp)
                      (lambda (value) (eq value 'fake-plz-request)))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) nil))
                     ((symbol-function 'process-buffer)
                      (lambda (_process) transport-buffer))
                     ((symbol-function 'process-get)
                      (lambda (_process property)
                        (pcase property
                          (:plz-result :plz-result)
                          (:plz-as nil))))
                     ((symbol-function 'process-sentinel)
                      (lambda (_process) 'plz--sentinel))
                     ((symbol-function 'plz--sentinel)
                      (lambda (_process status)
                        (setq sentinel-called status)
                        (funcall response-cb "final answer"))))
             (superchat--llm-generate-answer
              "finish the request"
              (lambda (result) (setq final-result result))
              #'ignore)
             (should scheduled)
             (should-not final-result)
             (funcall scheduled)
             (should (equal sentinel-called "workaround"))
             (should (eq timer-cancelled 'fake-watchdog-timer))
             (should (equal final-result "final answer")))
         (when (buffer-live-p transport-buffer)
           (kill-buffer transport-buffer)))))))

(ert-deftest test-async-dispatcher-stops-at-tool-round-limit ()
  "Tool-round cap applies before Superchat sends another model request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (superchat-llm-max-tool-rounds 1)
           (superchat-llm-round-limit-action 'stop)
           (requests 0)
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (funcall response-cb '((fake-tool . "tool result"))))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= requests 1))
         (should (string= final-result "[Stopped after 1 tool rounds]")))))))

(ert-deftest test-async-dispatcher-finalizes-after-tool-round-limit ()
  "The finalization action reuses the prompt with multi-output disabled."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (superchat-llm-max-tool-rounds 1)
           (superchat-llm-round-limit-action 'finalize)
           (requests '())
           tools-disabled
           final-result)
       (cl-letf (((symbol-function 'superchat--llm-disable-prompt-tools)
                  (lambda (_prompt) (setq tools-disabled t)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional multi-output)
                    (push (cons prompt multi-output) requests)
                    (if (= (length requests) 1)
                        (funcall response-cb '((fake-tool . "tool result")))
                      (funcall response-cb "final answer without tools")))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 (length requests)))
         (should (eq (caar requests) (caar (cdr requests))))
         (should (null (cdr (car requests))))
         (should tools-disabled)
         (should (string= final-result "final answer without tools")))))))

(ert-deftest test-async-dispatcher-error-cb-finalizes ()
  "When the error-cb fires, the final callback receives an error string.

llm.el invokes the error callback with TWO arguments (type message) at
every call site (`error-callback 'error <msg>').  A one-argument error-cb
crashes inside plz's timer with `wrong-number-of-arguments', which masks
the real API error, so this test drives the real two-argument contract."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil))
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb _response-cb
                           error-cb &optional _multi-output)
                    ;; The two-argument form llm.el actually uses.
                    (funcall error-cb 'error "rate limit exceeded"))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil)
          nil
          nil)
         (should (string-match-p "rate limit exceeded" final-result)))))))

(ert-deftest test-prompt-repair-survives-late-llm-load ()
  "Prompt repair must work if llm.el loads after Superchat.

`superchat-llm.el' is allowed to load before the optional llm dependency.
Define the minimal prompt structs after the module is loaded to reproduce
that order; a setter baked into the source at load time must not be needed.
Run the reproduction in a child Emacs so its fake llm structs cannot pollute
the other tests in this file."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (or (locate-library "superchat-llm")
                                       (expand-file-name "../superchat-llm.el"
                                                         (file-name-directory
                                                          (or load-file-name
                                                              buffer-file-name))))))
        (script (make-temp-file "superchat-llm-late-load-" nil ".el")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file script
            (insert
             "(progn\n"
             "  (require 'cl-lib)\n"
             "  (require 'superchat-llm)\n"
             "  (cl-defstruct llm-chat-prompt interactions tools)\n"
             "  (cl-defstruct llm-chat-prompt-interaction role content)\n"
             "  (let* ((user (make-llm-chat-prompt-interaction\n"
             "                 :role 'user :content \"question\"))\n"
             "         (tool-use (make-llm-chat-prompt-interaction\n"
             "                     :role 'assistant\n"
             "                     :content '((call:search . nil))))\n"
             "         (prompt (make-llm-chat-prompt\n"
             "                  :interactions (list user tool-use)\n"
             "                  :tools '(render-ui))))\n"
             "    (unless (superchat--llm-drop-dangling-tool-use prompt)\n"
             "      (kill-emacs 2))\n"
             "    (unless (equal (list user)\n"
             "                   (llm-chat-prompt-interactions prompt))\n"
             "      (kill-emacs 3))\n"
             "    (superchat--llm-disable-prompt-tools prompt)\n"
             "    (when (llm-chat-prompt-tools prompt)\n"
             "      (kill-emacs 4))))\n"))
          (should (= 0 (call-process emacs nil nil nil
                                     "-Q" "--batch" "-L" root "-l" script))))
      (when (file-exists-p script)
        (delete-file script)))))

(ert-deftest test-async-dispatcher-plain-chat-does-not-enumerate-tools ()
  "Plain on-demand chat should not synchronously walk built-in or MCP tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'on-demand)
           (superchat-show-response-mode t)
           (superchat--current-command nil)
           (tool-calls 0)
           (mcp-calls 0)
           captured-prompt
           captured-multi-output)
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda ()
                    (setq tool-calls (1+ tool-calls))
                    (list 'unexpected-tool)))
                 ((symbol-function 'superchat-mcp-get-tools)
                  (lambda ()
                    (setq mcp-calls (1+ mcp-calls))
                    (list 'unexpected-mcp-tool)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt partial-cb response-cb
                           _error-cb &optional multi-output)
                    (setq captured-prompt prompt
                          captured-multi-output multi-output)
                    (funcall partial-cb "ok")
                    (funcall response-cb "ok"))))
         (superchat--llm-generate-answer
          "plain question"
          (lambda (_result) nil)
          (lambda (_chunk) nil)
          nil
          nil)
         (should (= 0 tool-calls))
         (should (= 0 mcp-calls))
         (should (eq (car captured-prompt) 'mock-prompt))
         (should (string= "plain question"
                          (plist-get (cdr captured-prompt) :text)))
         (should (null (plist-get (cdr captured-prompt) :tools)))
         (should (null captured-multi-output)))))))

(ert-deftest test-core-run-turn-does-not-collect-tools ()
  "The core parse hook runner should not do request-time tool collection."
  (let ((superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions nil)
        (superchat-post-turn-functions nil)
        (superchat-llm-backend (test-llm--make-mock-backend 'openai))
        (collect-calls 0))
    (cl-letf (((symbol-function 'superchat--collect-llm-tools)
               (lambda (&optional _input)
                 (setq collect-calls (1+ collect-calls))
                 (list 'unexpected-tool))))
      (let ((prepared (superchat-core-run-turn
                       (superchat-turn-new "plain question" "test-session"))))
        (should (= 0 collect-calls))
        (should (null (superchat-turn-tools prepared)))))))

(ert-deftest test-send-input-plain-chat-builds-prompt-from-user-input ()
  "The default send path must pass the user's plain text into the LLM prompt."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         captured-prompt)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (insert "Why is the first token slow?")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt _callback _stream-callback
                              &optional _target-model _context-files _tools _system _preset)
                       (setq captured-prompt prompt))))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-prompt)
    (should (string-match-p "Why is the first token slow\\?"
                            captured-prompt))))

(ert-deftest test-send-input-preserves-leading-ascii-subject ()
  "A leading ASCII subject must not be stripped from Chinese plain input."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         captured-prompt)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (insert "RaBitQ 是什么")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt _callback _stream-callback
                              &optional _target-model _context-files _tools _system _preset)
                       (setq captured-prompt prompt))))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-prompt)
    (should (string-match-p "RaBitQ 是什么" captured-prompt))))

(ert-deftest test-send-input-inherits-active-preset-tools ()
  "When a buffer has an active preset, plain input should inherit its tools."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         (active-preset (superchat-preset-from-plist
                         (list :name "coder"
                               :type 'agent
                               :tools '("read-file" "search-text"))))
         captured-tools
         captured-preset)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (setq-local superchat--active-preset active-preset)
          (insert "read the readme")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _callback _stream-callback
                              &optional _target-model _context-files tools _system preset)
                       (setq captured-tools tools
                             captured-preset preset)
                       ;; Don't actually stream; finalize immediately.
                       nil)))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-tools)
    (should (eq active-preset captured-preset))
    (should (member "read-file" captured-tools))
    (should (member "search-text" captured-tools))))

(ert-deftest test-completion-at-point-prefixes-model-candidates ()
  "@ completion candidates should include the @ prefix because bounds include it."
  (let ((superchat--prompt-start nil))
    (with-temp-buffer
      (setq-local superchat--prompt-start (point-marker))
      (insert "@g")
      (cl-letf (((symbol-function 'superchat--get-available-models)
                 (lambda () '("gpt-4o" "claude-sonnet"))))
        (let ((capf (superchat--completion-at-point)))
          (should capf)
          (should (equal '("@gpt-4o" "@claude-sonnet")
                         (nth 2 capf))))))))

(ert-deftest test-slash-key-triggers-completion-in-prompt ()
  "Typing / in the prompt should invoke completion immediately."
  (let ((completion-calls 0))
    (with-temp-buffer
      (setq-local superchat--prompt-start (point-marker))
      (cl-letf (((symbol-function 'completion-at-point)
                 (lambda ()
                   (setq completion-calls (1+ completion-calls))
                   nil)))
        (superchat--insert-slash-and-complete)
        (should (string= "/" (buffer-string)))
        (should (= 1 completion-calls))))))

;;;---------------------------------------------
;;; superchat-parser (pure, no llm deps)
;;;---------------------------------------------

(ert-deftest test-parser-model-switch-with-model ()
  "`@gpt-4o Hello' -> ('Hello' . 'gpt-4o')."
  (let ((parsed (superchat-parser-model-switch "@gpt-4o Hello, world!")))
    (should parsed)
    (should (string= "Hello, world!" (car parsed)))
    (should (string= "gpt-4o" (cdr parsed)))))

(ert-deftest test-parser-model-switch-no-model ()
  "Plain text without @ returns nil."
  (should-not (superchat-parser-model-switch "just a regular message")))

(ert-deftest test-parser-command-simple ()
  "`/help some args' -> ('help' . 'some args')."
  (let ((parsed (superchat-parser-command "/help some args")))
    (should parsed)
    (should (string= "help" (car parsed)))
    (should (string= "some args" (cdr parsed)))))

(ert-deftest test-parser-command-multiline ()
  "Multi-line args are preserved after the command name."
  (let ((parsed (superchat-parser-command "/explain\nline1\nline2")))
    (should parsed)
    (should (string= "explain" (car parsed)))
    (should (string-match-p "line1" (cdr parsed)))
    (should (string-match-p "line2" (cdr parsed)))))

(ert-deftest test-parser-command-no-match ()
  "A non-command input returns nil."
  (should-not (superchat-parser-command "no slash here")))

(ert-deftest test-parser-define-with-prompt ()
  "`/define name \"prompt\"' -> ('name' . 'prompt')."
  (let ((parsed (superchat-parser-define "/define explain-code \"Please explain $input\"")))
    (should parsed)
    (should (string= "explain-code" (car parsed)))
    (should (string= "Please explain $input" (cdr parsed)))))

(ert-deftest test-parser-define-name-only ()
  "`/define name' -> ('name' . '')."
  (let ((parsed (superchat-parser-define "/define greet")))
    (should parsed)
    (should (string= "greet" (car parsed)))
    (should (string= "" (cdr parsed)))))

(ert-deftest test-parser-extract-file-path-quoted ()
  "`#\"/path/with space/file.txt\"' normalizes and unescapes."
  (let ((path (superchat-parser-extract-file-path "#\"/tmp/foo bar.txt\"")))
    (should path)
    (should (string-match-p "foo bar.txt" path))))

(ert-deftest test-parser-extract-file-path-unquoted ()
  "`#/path/to/file' returns the normalized path."
  (let ((path (superchat-parser-extract-file-path "#/etc/hosts")))
    (should path)
    (should (string-match-p "hosts" path))))

(ert-deftest test-parser-extract-file-path-none ()
  "Input without a #file ref returns nil."
  (should-not (superchat-parser-extract-file-path "no file ref here")))

;;; superchat-test-llm-backend.el ends here
