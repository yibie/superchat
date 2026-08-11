;;; preflight.el --- No-chat Experience View v2 preflight -*- lexical-binding: t; -*-

;; This file only inspects and serializes launch state.  It never invokes a
;; chat API or runs an Acceptance cell.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'subr-x)

(defconst superchat-acceptance-v26--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst superchat-acceptance-v26--repo-source-files
  '("superchat.el" "superchat-runtime.el" "superchat-llm.el"
    "superchat-authority.el" "superchat-workspace.el"
    "superchat-experience.el" "superchat-closure.el" "superchat-tape.el"
    "superchat-projection.el" "superchat-preset.el" "superchat-tools.el"
    "superchat-dispatcher.el" "superchat-prompt-hooks.el"
    "superchat-agent-loop.el" "superchat-db.el"))

(defconst superchat-acceptance-v26--dependency-features
  '(compat plz plz-event-source plz-media-type))

(defconst superchat-acceptance-v26--tool-order
  '("read-file" "list-files" "search-text" "read_buffer" "EditBuffer"
    "ReplaceBuffer" "eshell-command" "workspace_read" "workspace_write"
    "workspace_info"))

(defconst superchat-acceptance-v26--profile-accessors
  '(:name "code"
    :profile-id "code"
    :description "Coding in the current Emacs context"
    :type "agent"
    :capability "code"
    :skill-body
    "Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools."
    :tools ["read-file" "list-files" "search-text" "read_buffer" "EditBuffer"
            "ReplaceBuffer" "eshell-command" "workspace_read" "workspace_write"
            "workspace_info"]
    :model nil :temperature nil :max-tokens nil :reasoning "inherit"
    :max-tool-calls nil :confirm-destructive "inherit" :timeout nil :pre nil
    :version "1.0" :triggers [] :source "builtin" :source-file nil))

(defconst superchat-acceptance-v26--llm-commit
  "f6a83bb05e45377d5166821b84aa178c9164b55b")

(defconst superchat-acceptance-v26--llm-version "0.31.1")

(defconst superchat-acceptance-v26--adapter-sha256
  "74dcdad296f920ea7ee43e9411398f05e0079d044a34c6e2efa4d7d3aa107505")

(defconst superchat-acceptance-v26--model "qwen3.5:9b")

(defconst superchat-acceptance-v26--model-manifest-sha256
  "6488c96fa5faab64bb65cbd30d4289e20e6130ef535a93ef9a49f42eda893ea7")

(defconst superchat-acceptance-v26--model-layer-digest
  "sha256:dec52a44569a2a25341c4e4d3fee25846eed4f6f0b936278e3a3c900bb99d37c")

(defun superchat-acceptance-v26--repo-root ()
  "Return the checkout root containing this preflight."
  (expand-file-name "../.." superchat-acceptance-v26--directory))

(defun superchat-acceptance-v26--file-sha256 (file)
  "Return FILE's SHA-256, or signal when FILE is unreadable."
  (unless (file-readable-p file)
    (error "Cannot read required preflight file role: %s"
           (file-name-nondirectory file)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun superchat-acceptance-v26--string-sha256 (string)
  "Return SHA-256 of STRING's exact UTF-8 bytes."
  (secure-hash 'sha256 (encode-coding-string string 'utf-8)))

(defun superchat-acceptance-v26--package-source-version (file)
  "Return FILE's package descriptor version, or signal."
  (let ((description
         (with-temp-buffer
           (insert-file-contents file)
           (package-buffer-info))))
    (unless (package-desc-p description)
      (error "Package source version is unrecordable"))
    (package-version-join (package-desc-version description))))

(defun superchat-acceptance-v26--source-file (library)
  "Return readable source corresponding to located LIBRARY."
  (let ((source (and library
                     (if (string-suffix-p ".elc" library)
                         (concat (file-name-sans-extension library) ".el")
                       library))))
    (unless (and source (file-readable-p source))
      (error "Located library has no readable source"))
    (file-truename source)))

(defun superchat-acceptance-v26--command (directory program &rest args)
  "Run PROGRAM ARGS in DIRECTORY and return trimmed stdout, or signal."
  (with-temp-buffer
    (let ((default-directory directory))
      (let ((status
             (apply #'process-file program nil (current-buffer) nil args)))
        (unless (and (integerp status) (zerop status))
          (error "Preflight command failed: %s %s"
                 program (string-join args " ")))
        (string-trim-right (buffer-string))))))

(defun superchat-acceptance-v26--dependency-record (feature elpa-root)
  "Return installed FEATURE metadata, requiring ownership by ELPA-ROOT."
  (let* ((entry (assq feature package-alist))
         (description (cadr entry)))
    (unless (package-desc-p description)
      (error "Private ELPA dependency is absent from package-alist: %s" feature))
    (let* ((version-list (package-desc-version description))
           (version (and version-list (package-version-join version-list)))
           (package-root (file-truename (package-desc-dir description)))
           (_ (unless (file-in-directory-p package-root elpa-root)
                (error "Dependency is not owned by the private ELPA: %s" feature)))
           (_ (unless (require feature nil t)
                (error "Cannot require private ELPA dependency: %s" feature)))
           (source (superchat-acceptance-v26--source-file
                    (locate-library (symbol-name feature)))))
      (unless (and (stringp version) (not (string-empty-p version)))
        (error "Dependency version is unrecordable: %s" feature))
      (unless (file-in-directory-p source package-root)
        (error "Loaded dependency source differs from package-alist: %s" feature))
      (list :feature (symbol-name feature)
            :package (symbol-name (package-desc-name description))
            :version version
            :path (concat "<private-elpa>/"
                          (file-name-nondirectory
                           (directory-file-name package-root)) "/"
                          (file-relative-name source package-root))
            :sha256 (superchat-acceptance-v26--file-sha256 source)))))

(defun superchat-acceptance-v26--activate-private-dependencies (elpa-root)
  "Put each package-alist dependency owned by ELPA-ROOT on load-path."
  (dolist (feature superchat-acceptance-v26--dependency-features)
    (let* ((description (cadr (assq feature package-alist)))
           (package-root
            (and (package-desc-p description)
                 (file-truename (package-desc-dir description)))))
      (unless package-root
        (error "Private ELPA dependency is absent from package-alist: %s"
               feature))
      (unless (file-in-directory-p package-root elpa-root)
        (error "Dependency package-alist entry is outside private ELPA: %s"
               feature))
      ;; Emacs 31 treats its unrelated built-in compat.el as satisfying the
      ;; compat package, so package-initialize does not activate this directory.
      (add-to-list 'load-path package-root))))

(defun superchat-acceptance-v26--private-library (feature llm-root)
  "Return FEATURE source and require it to be inside private LLM-ROOT."
  (unless (require feature nil t)
    (error "Cannot require private llm feature: %s" feature))
  (let ((source (superchat-acceptance-v26--source-file
                 (locate-library (symbol-name feature)))))
    (unless (file-in-directory-p source llm-root)
      (error "Loaded llm feature is outside the private source role: %s" feature))
    source))

(defun superchat-acceptance-v26--jj-identity (repo-root)
  "Return the stored jj working-copy identity for REPO-ROOT."
  (let* ((template
          "change_id ++ \"\\n\" ++ commit_id ++ \"\\n\" ++ parents.map(|p| p.commit_id()).join(\" \") ++ \"\\n\"")
         (lines (split-string
                 (superchat-acceptance-v26--command
                  repo-root "jj" "log" "--ignore-working-copy" "-r" "@"
                  "--no-graph" "-T" template)
                 "\n" t)))
    (unless (= (length lines) 3)
      (error "jj working-copy identity is unrecordable"))
    (pcase-let ((`(,change ,commit ,parents) lines))
      (unless (and (not (string-match-p "[^a-z0-9]" change))
                   (= (length commit) 40)
                   (not (string-match-p "[^0-9a-f]" commit))
                   (cl-every
                    (lambda (parent)
                      (and (= (length parent) 40)
                           (not (string-match-p "[^0-9a-f]" parent))))
                    (split-string parents " " t)))
        (error "jj working-copy identity has an invalid shape"))
      (list :change-id change :commit-id commit
            :parent-commit-ids (vconcat (split-string parents " " t))
            :snapshot-policy "ignore-working-copy"))))

(defun superchat-acceptance-v26--symbol-vector (values role)
  "Return VALUES as a vector of symbol names, or signal for ROLE."
  (vconcat
   (mapcar (lambda (value)
             (unless (symbolp value)
               (error "%s contains an unrecordable function" role))
             (symbol-name value))
           values)))

(defun superchat-acceptance-v26--profile-accessors (profile)
  "Return every accessor value for PROFILE in JSON-safe form."
  (list :name (superchat-preset-name profile)
        :profile-id (superchat-preset-profile-id profile)
        :description (superchat-preset-description profile)
        :type (symbol-name (superchat-preset-type profile))
        :capability (superchat-preset-capability profile)
        :skill-body (superchat-preset-skill-body profile)
        :tools (vconcat (superchat-preset-tools profile))
        :model (superchat-preset-model profile)
        :temperature (superchat-preset-temperature profile)
        :max-tokens (superchat-preset-max-tokens profile)
        :reasoning (symbol-name (superchat-preset-reasoning profile))
        :max-tool-calls (superchat-preset-max-tool-calls profile)
        :confirm-destructive
        (symbol-name (superchat-preset-confirm-destructive profile))
        :timeout (superchat-preset-timeout profile)
        :pre (and (superchat-preset-pre profile)
                  (format "%S" (superchat-preset-pre profile)))
        :version (superchat-preset-version profile)
        :triggers (vconcat (or (superchat-preset-triggers profile) '()))
        :source (symbol-name (superchat-preset-source profile))
        :source-file (superchat-preset-source-file profile)))

(defun superchat-acceptance-v26--tool-schema-records (tools)
  "Return exact provider-visible JSON and hashes for ordered TOOLS."
  (let ((seen (make-hash-table :test #'equal)))
    (vconcat
     (mapcar
      (lambda (tool)
        (let ((name (llm-tool-name tool)))
          (unless (and (stringp name) (not (gethash name seen))
                       (functionp (llm-tool-function tool))
                       (not (llm-tool-async tool)))
            (error "Tool is duplicate, uncallable, or async: %s" name))
          (puthash name t seen)
          (let* ((schema (llm-provider-utils-openai-tool-spec tool))
                 (json (llm-provider-utils-json-serialize schema))
                 (bytes (encode-coding-string json 'utf-8)))
            (list :name name :schema schema :json json
                  :size (string-bytes bytes)
                  :sha256
                  (concat "sha256:"
                          (superchat-acceptance-v26--string-sha256 json))))))
      tools))))

(defun superchat-acceptance-v26--without-request-payload (request)
  "Return all provider REQUEST fields except messages and tool schemas."
  (let (result)
    (while request
      (let ((key (pop request)) (value (pop request)))
        (unless (memq key '(:messages :tools))
          (setq result (append result (list key (copy-tree value t)))))))
    result))

(defun superchat-acceptance-v26--guard-provider-work (function)
  "Call FUNCTION while hard-failing any chat or HTTP transport entry."
  (let ((calls 0)
        (guarded '(llm-chat llm-chat-async llm-chat-streaming
                   llm-request-plz-sync llm-request-plz-async plz))
        advice)
    (setq advice
          (lambda (&rest _)
            (cl-incf calls)
            (error "Provider construction attempted transport")))
    (dolist (entry guarded)
      (when (fboundp entry) (advice-add entry :before advice)))
    (unwind-protect
        (let ((value (funcall function)))
          (unless (zerop calls)
            (error "No-chat preflight observed %d provider requests" calls))
          (list :value value :request-count calls
                :guarded-functions
                (superchat-acceptance-v26--symbol-vector guarded "transport guard")))
      (dolist (entry guarded)
        (when (fboundp entry) (advice-remove entry advice))))))

(defun superchat-acceptance-v26--model-record ()
  "Return verified local Ollama model identity without a chat request."
  (let* ((models-root (or (getenv "OLLAMA_MODELS")
                          (expand-file-name ".ollama/models"
                                            (or (getenv "HOME") ""))))
         (manifest (expand-file-name
                    "manifests/registry.ollama.ai/library/qwen3.5/9b"
                    models-root))
         (digest (superchat-acceptance-v26--file-sha256 manifest))
         (object (with-temp-buffer
                   (insert-file-contents-literally manifest)
                   (json-parse-buffer :object-type 'plist :array-type 'array
                                      :null-object nil :false-object :false)))
         (layers (plist-get object :layers))
         (layer-digests
          (and (vectorp layers)
               (mapcar (lambda (layer) (plist-get layer :digest)) layers)))
         (ollama-version
          (superchat-acceptance-v26--command default-directory
                                            "ollama" "--version"))
         (model-list
          (superchat-acceptance-v26--command default-directory "ollama" "list")))
    (unless (and (equal digest superchat-acceptance-v26--model-manifest-sha256)
                 (member superchat-acceptance-v26--model-layer-digest layer-digests)
                 (string-match-p "\\`ollama version is [0-9]+\\.[0-9]+\\.[0-9]+\\'"
                                 ollama-version)
                 (string-match-p
                  "qwen3\\.5:9b[[:space:]]+6488c96fa5fa" model-list))
      (error "Local Ollama model identity differs from the frozen cell"))
    (list :provider-runtime-version ollama-version
          :model superchat-acceptance-v26--model
          :manifest-role "local-ollama-model-manifest"
          :manifest-sha256 digest
          :config-digest (plist-get (plist-get object :config) :digest)
          :layers (copy-tree layers t)
          :required-model-layer superchat-acceptance-v26--model-layer-digest)))

(defun superchat-acceptance-v26--write (manifest)
  "Write MANIFEST to the configured output, or print it to stdout."
  (if-let* ((output (getenv "SUPERCHAT_ACCEPTANCE_V26_OUTPUT")))
      (with-temp-file output
        (let ((print-length nil) (print-level nil))
          (prin1 manifest (current-buffer))
          (insert "\n")))
    (let ((print-length nil) (print-level nil))
      (prin1 manifest)
      (terpri))))

(defun superchat-acceptance-v26-preflight ()
  "Return and optionally persist the complete no-chat launch manifest."
  (let* ((repo-root (file-truename (superchat-acceptance-v26--repo-root)))
         (elpa-env (or (getenv "SUPERCHAT_ACCEPTANCE_V26_ELPA")
                       (error "Private ELPA role was not provided")))
         (llm-env (or (getenv "SUPERCHAT_ACCEPTANCE_V26_LLM_DIR")
                      (error "Private llm source role was not provided")))
         (elpa-root (file-name-as-directory (file-truename elpa-env)))
         (llm-root (file-name-as-directory (file-truename llm-env))))
    (unless (and (bound-and-true-p package--initialized)
                 (file-equal-p (file-truename package-user-dir) elpa-root))
      (error "Private package-user-dir was not initialized before preflight"))
    (add-to-list 'load-path repo-root)
    (superchat-acceptance-v26--activate-private-dependencies elpa-root)
    (let* ((dependencies
            (mapcar (lambda (feature)
                      (superchat-acceptance-v26--dependency-record
                       feature elpa-root))
                    superchat-acceptance-v26--dependency-features))
           (llm-source
            (superchat-acceptance-v26--private-library 'llm llm-root))
           (adapter-source
            (superchat-acceptance-v26--private-library 'llm-ollama llm-root))
           (commit (superchat-acceptance-v26--command
                    llm-root "git" "rev-parse" "HEAD"))
           (llm-version
            (superchat-acceptance-v26--package-source-version llm-source))
           (adapter-sha256
            (superchat-acceptance-v26--file-sha256 adapter-source)))
      (unless (and (equal commit superchat-acceptance-v26--llm-commit)
                   (equal llm-version superchat-acceptance-v26--llm-version)
                   (equal adapter-sha256
                          superchat-acceptance-v26--adapter-sha256))
        (error "Private llm checkout identity differs from the frozen cell"))
      (let ((inhibit-message t))
        (require 'superchat))
      (let* ((superchat-lang "English")
             (superchat-llm-tools-enabled t)
             (superchat-llm-reasoning 'none)
             (superchat-llm-max-tool-rounds 2)
             (superchat-llm-round-limit-action 'stop)
             (superchat-llm-unknown-tool-retries 0)
             (superchat-response-timeout 60)
             (superchat-tool-timeout-multiplier 1.0)
             (superchat-agent-max-tool-calls 2)
             (superchat-agent-confirm-destructive t)
             (profile (superchat--builtin-capability-preset "code"))
             (superchat--active-preset profile)
             (profile-accessors
              (superchat-acceptance-v26--profile-accessors profile))
             (_ (unless (equal profile-accessors
                               superchat-acceptance-v26--profile-accessors)
                  (error "Built-in code Profile accessors differ")))
             (profile-json
              (llm-provider-utils-json-serialize profile-accessors))
             (tools (superchat-get-llm-tools
                     (superchat-preset-tools profile)))
             (tool-names (mapcar #'llm-tool-name tools))
             (_ (unless (equal tool-names superchat-acceptance-v26--tool-order)
                  (error "Built-in code Profile tool order differs")))
             (_ (superchat--llm-validate-evidence-tools tools))
             (tool-schemas
              (superchat-acceptance-v26--tool-schema-records tools))
             (tool-schema-json
              (llm-provider-utils-json-serialize
               (vconcat (mapcar #'llm-provider-utils-openai-tool-spec tools))))
             (turn (superchat-turn-new "NO_CHAT_PREFLIGHT" "preflight-v2"))
             (prepared
              (superchat-core--run-hook-chain
               turn 'superchat-system-prompt-functions))
             (_ (superchat-preset-apply profile prepared))
             (system-prompt (superchat-turn-system-prompt prepared))
             (construction
              (superchat-acceptance-v26--guard-provider-work
               (lambda ()
                 (let* ((provider
                         (make-llm-ollama
                          :chat-model superchat-acceptance-v26--model
                          :host "localhost" :port 11434))
                        (_ (unless (llm-ollama-p provider)
                             (error "Pinned Ollama provider did not construct")))
                        (capabilities (llm-capabilities provider))
                        (streaming
                         (and (memq 'streaming-tool-use capabilities) t))
                        (prompt
                         (superchat--build-llm-prompt
                          "NO_CHAT_PREFLIGHT" tools system-prompt profile nil))
                        (request
                         (llm-provider-chat-request provider prompt streaming)))
                   (list :provider provider :capabilities capabilities
                         :streaming streaming :prompt prompt :request request)))))
             (constructed (plist-get construction :value))
             (provider (plist-get constructed :provider))
             (prompt (plist-get constructed :prompt))
             (request (plist-get constructed :request))
             (manifest
              (list
               :format "superchat-experience-view-preflight-v2.6"
               :mode "no-chat"
               :emacs emacs-version
               :product-source
               (vconcat
                (mapcar
                 (lambda (file)
                   (list :path file
                         :sha256
                         (superchat-acceptance-v26--file-sha256
                          (expand-file-name file repo-root))))
                 superchat-acceptance-v26--repo-source-files))
               :jj-working-copy
               (superchat-acceptance-v26--jj-identity repo-root)
               :private-package-role "<private-elpa>"
               :dependencies (vconcat dependencies)
               :private-source-role "<private-llm>"
               :llm
               (list :commit commit :version llm-version
                     :features (list :llm (featurep 'llm)
                                     :llm-ollama (featurep 'llm-ollama)
                                     :llm-ollama-p
                                     (and (fboundp 'llm-ollama-p)
                                          (llm-ollama-p provider)))
                     :source-role "<private-llm>/llm.el"
                     :sha256
                     (superchat-acceptance-v26--file-sha256 llm-source)
                     :adapter-role
                     "<private-llm>/llm-ollama.el"
                     :adapter-sha256 adapter-sha256)
               :provider
               (list :class "llm-ollama"
                     :capabilities
                     (vconcat (mapcar #'symbol-name
                                      (plist-get constructed :capabilities)))
                     :endpoint "http://localhost:11434"
                     :construction-request-count
                     (plist-get construction :request-count)
                     :guarded-functions
                     (plist-get construction :guarded-functions)
                     :constructed (and (llm-ollama-p provider) t))
               :model (superchat-acceptance-v26--model-record)
               :profile
               (list :accessors profile-accessors
                     :json profile-json
                     :size (string-bytes
                            (encode-coding-string profile-json 'utf-8))
                     :sha256
                     (concat
                      "sha256:"
                      (superchat-acceptance-v26--string-sha256 profile-json)))
               :tool-schemas
               (list :order (vconcat tool-names)
                     :records tool-schemas
                     :ordered-json tool-schema-json
                     :ordered-size
                     (string-bytes
                      (encode-coding-string tool-schema-json 'utf-8))
                     :ordered-sha256
                     (concat
                      "sha256:"
                      (superchat-acceptance-v26--string-sha256
                       tool-schema-json))
                     :validation
                     (list :profile-order t :unique t :callable t
                           :synchronous t :static t
                           :exact-evidence-validator t))
               :prompt
               (list :system-prompt system-prompt
                     :system-prompt-sha256
                     (superchat-acceptance-v26--string-sha256 system-prompt)
                     :system-prompt-functions
                     (superchat-acceptance-v26--symbol-vector
                      superchat-system-prompt-functions "system prompt hook")
                     :build-prompt-functions
                     (superchat-acceptance-v26--symbol-vector
                      superchat-build-prompt-functions "build prompt hook")
                     :prepared-context (llm-chat-prompt-context prompt)
                     :prepared-messages
                     (superchat--llm-evidence-messages prompt))
               :options
               (list :profile-guardrails
                     (superchat--agent-effective-guardrails profile)
                     :prompt-accessors
                     (superchat--llm-evidence-options
                      prompt (plist-get constructed :streaming) t)
                     :provider-request
                     (superchat-acceptance-v26--without-request-payload request)
                     :target-model superchat-acceptance-v26--model
                     :max-tool-rounds superchat-llm-max-tool-rounds
                     :unknown-tool-retries superchat-llm-unknown-tool-retries
                     :round-limit (symbol-name superchat-llm-round-limit-action)
                     :response-timeout superchat-response-timeout
                     :timeout-multiplier superchat-tool-timeout-multiplier
                     :authority-policy "authority-v1/default"
                     :attendance "attended"
                     :approval "read-only-only"
                     :topology "single" :language "English"
                     :active-extensions []))))
        (superchat-acceptance-v26--write manifest)
        manifest))))

(when (getenv "SUPERCHAT_ACCEPTANCE_V26_PREFLIGHT")
  (condition-case err
      (progn
        (superchat-acceptance-v26-preflight)
        (message "Experience acceptance v2.6 no-chat preflight: passed")
        (kill-emacs 0))
    (error
     (message "Experience acceptance v2.6 no-chat preflight failed: %s"
              (error-message-string err))
     (kill-emacs 1))))

(provide 'superchat-experience-acceptance-v26-preflight)

;;; preflight.el ends here
