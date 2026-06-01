;;; test-llm-backend.el --- Tests for the v0.5 llm.el backend integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for superchat's llm.el backend, dispatchers, and parser.
;; No live API calls are made — `llm-chat', `llm-chat-streaming',
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
              (lambda (&rest _args)
                (list 'mock-tool :name (plist-get _args :name))))
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
  (should (null superchat-llm-backend))
  (should (null superchat-llm-model))
  (should (eq superchat-llm-streaming t)))

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

(ert-deftest test-tools-list-populates-with-mock-llm ()
  "With a mock `llm-make-tool', reload registers all 13 tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil))
       (let ((tools (superchat-llm-tools-reload)))
         (should (listp tools))
         ;; We register 13 tools: shell-command, write-file, append-file,
         ;; quick-write, read-file, list-files, search-text,
         ;; make_directory, find-files, read_buffer, append_to_buffer,
         ;; EditBuffer, ReplaceBuffer.
         (should (= 13 (length tools)))
         ;; After reload, the cache var is populated (not nil).
         (should superchat-llm-tools-list)
         (should (= 13 (length superchat-llm-tools-list))))))))

(ert-deftest test-tools-list-cached-after-first-call ()
  "`superchat-get-llm-tools' returns the cached list on subsequent calls."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil))
       (superchat-get-llm-tools)
       (let ((first-call superchat-llm-tools-list))
         (should first-call)
         (should (eq first-call (superchat-get-llm-tools))))))))

;;;---------------------------------------------
;;; superchat--llm-generate-answer-sync
;;;---------------------------------------------

(ert-deftest test-sync-dispatcher-uses-llm-chat ()
  "Sync dispatcher calls `llm-chat' (no kwargs) and returns its result."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           llm-chat-called-with)
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  ;; The real function would repopulate the cache
                  ;; via `superchat-llm-tools-reload' since the
                  ;; mock `llm-make-tool' is fbound.  We want a
                  ;; clean "no tools" path here.
                  (lambda () nil))
                 ((symbol-function 'llm-chat)
                  (lambda (backend prompt &optional multi-output)
                    (setq llm-chat-called-with
                          (list :backend backend
                                :prompt prompt
                                :multi-output multi-output))
                    "sync response text")))
         (let ((result (superchat--llm-generate-answer-sync "hello")))
           (should (string= result "sync response text"))
           (should llm-chat-called-with)
           (should (string= "hello"
                            (plist-get llm-chat-called-with :prompt)))
           (should (null (plist-get llm-chat-called-with :multi-output)))))))))

(ert-deftest test-sync-dispatcher-passes-target-model ()
  "Sync dispatcher applies target-model to the backend via the
`superchat--provider-with-chat-model' cl-defgeneric copy."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           captured-backend)
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda () nil))
                 ((symbol-function 'superchat--provider-with-chat-model)
                  (lambda (provider new-model)
                    (append (list 'mock-backend-copy)
                            (plist-put (copy-sequence (cdr provider))
                                       :chat-model new-model)))))
         (cl-letf (((symbol-function 'llm-chat)
                    (lambda (backend _prompt &optional _multi-output)
                      (setq captured-backend backend)
                      "ok")))
           (superchat--llm-generate-answer-sync "hi" "gpt-4o")
           (should (string= "gpt-4o"
                            (plist-get (cdr captured-backend) :chat-model)))
           ;; And the new backend is a distinct object from the original.
           (should (not (eq superchat-llm-backend captured-backend)))))))))

(ert-deftest test-sync-dispatcher-errors-without-backend ()
  "Sync dispatcher errors with a clear message when backend is nil."
  (let ((superchat-llm-backend nil)
        (superchat-response-timeout nil))
    (should-error (superchat--llm-generate-answer-sync "hi")
                  :type 'error)))

(ert-deftest test-sync-dispatcher-passes-tools-in-prompt ()
  "Sync dispatcher wraps the prompt via `llm-make-chat-prompt' when
tools are registered, and signals `multi-output' so llm.el returns a
plist instead of a bare string."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-llm-tools-list
             (list (list 'mock-tool :name "fake-tool-1")
                   (list 'mock-tool :name "fake-tool-2"))))
       (let (captured-prompt captured-multi-output)
         (cl-letf (((symbol-function 'llm-chat)
                    (lambda (_backend prompt &optional multi-output)
                      (setq captured-prompt prompt
                            captured-multi-output multi-output)
                      "ok")))
           (superchat--llm-generate-answer-sync "hi")
           ;; The prompt is no longer the raw string — it's an
           ;; `llm-make-chat-prompt' struct (mocked here as a plist).
           (should (eq (car captured-prompt) 'mock-prompt))
           (let ((tools (plist-get (cdr captured-prompt) :tools)))
             (should tools)
             (should (= 2 (length tools))))
           ;; And llm.el must be told to return multi-output so it
           ;; includes :text instead of returning the string directly.
           (should captured-multi-output)))))))

;;;---------------------------------------------
;;; superchat--llm-generate-answer (async streaming)
;;;---------------------------------------------

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

(ert-deftest test-async-dispatcher-error-cb-finalizes ()
  "When the error-cb fires, the final callback receives an error string."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil))
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb _response-cb
                           error-cb &optional _multi-output)
                    (funcall error-cb "rate limit exceeded"))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil)
          nil
          nil)
         (should (string-match-p "rate limit exceeded" final-result)))))))

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
