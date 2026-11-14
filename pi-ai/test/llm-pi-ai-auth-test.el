;;; llm-pi-ai-auth-test.el --- Tests for llm-pi-ai-auth  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-auth.el'.  Translated from upstream:
;; `test/env-api-keys.test.ts' (the env-api-keys tests) and from
;; `test/models-runtime.test.ts' the cases of auth resolution order
;; ("resolves auth: stored credential owns the provider...", "a stored
;; credential without a matching handler blocks ambient fallback",
;; "wraps api-key auth failures in ModelsError", "uses explicit request
;; api key and env during provider auth resolution", "passes caller
;; signals to provider auth callbacks", "wraps credential store
;; failures").  OAuth cases are not ported.  The rest is from the
;; source's behaviour.  No test reads the real environment: each binds
;; `process-environment'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'llm-pi-ai-auth)

(defmacro llm-pi-ai-auth-test--with-env (vars &rest body)
  "Run BODY with a process environment of exactly VARS, a list of \"N=V\"."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence ,vars))) ,@body))

;;; env-api-keys.test.ts

(ert-deftest llm-pi-ai-auth-test-generic-github-tokens-are-not-copilot ()
  (llm-pi-ai-auth-test--with-env '("GH_TOKEN=gh-token" "GITHUB_TOKEN=github-token")
    (should-not (llm-pi-ai-find-env-keys "github-copilot"))
    (should-not (llm-pi-ai-get-env-api-key "github-copilot"))))

(ert-deftest llm-pi-ai-auth-test-copilot-token ()
  (llm-pi-ai-auth-test--with-env '("COPILOT_GITHUB_TOKEN=copilot-token"
                                   "GH_TOKEN=gh-token" "GITHUB_TOKEN=github-token")
    (should (equal (llm-pi-ai-find-env-keys "github-copilot")
                   '("COPILOT_GITHUB_TOKEN")))
    (should (equal (llm-pi-ai-get-env-api-key "github-copilot") "copilot-token"))))

(ert-deftest llm-pi-ai-auth-test-zai-coding-cn ()
  (llm-pi-ai-auth-test--with-env '("ZAI_CODING_CN_API_KEY=zai-coding-cn-token")
    (should (equal (llm-pi-ai-find-env-keys "zai-coding-cn")
                   '("ZAI_CODING_CN_API_KEY")))
    (should (equal (llm-pi-ai-get-env-api-key "zai-coding-cn")
                   "zai-coding-cn-token"))))

(ert-deftest llm-pi-ai-auth-test-anthropic-auth-token-reported-not-returned ()
  "ANTHROPIC_AUTH_TOKEN is reported but OAuth token and API key win as a key."
  (llm-pi-ai-auth-test--with-env '("ANTHROPIC_AUTH_TOKEN=auth-token"
                                   "ANTHROPIC_OAUTH_TOKEN=oauth-token"
                                   "ANTHROPIC_API_KEY=api-key")
    (should (equal (llm-pi-ai-find-env-keys "anthropic")
                   '("ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_OAUTH_TOKEN"
                     "ANTHROPIC_API_KEY")))
    (should (equal (llm-pi-ai-get-env-api-key "anthropic") "oauth-token"))))

(ert-deftest llm-pi-ai-auth-test-anthropic-auth-token-is-not-an-api-key ()
  (llm-pi-ai-auth-test--with-env '("ANTHROPIC_AUTH_TOKEN=auth-token")
    (should (equal (llm-pi-ai-find-env-keys "anthropic")
                   '("ANTHROPIC_AUTH_TOKEN")))
    (should-not (llm-pi-ai-get-env-api-key "anthropic"))))

(ert-deftest llm-pi-ai-auth-test-anthropic-oauth-token-is-an-api-key ()
  (llm-pi-ai-auth-test--with-env '("ANTHROPIC_OAUTH_TOKEN=oauth-token")
    (should (equal (llm-pi-ai-find-env-keys "anthropic")
                   '("ANTHROPIC_OAUTH_TOKEN")))
    (should (equal (llm-pi-ai-get-env-api-key "anthropic") "oauth-token"))))

(ert-deftest llm-pi-ai-auth-test-anthropic-api-key-fallback ()
  (llm-pi-ai-auth-test--with-env '("ANTHROPIC_API_KEY=api-key")
    (should (equal (llm-pi-ai-get-env-api-key "anthropic") "api-key"))))

;;; env-api-keys.ts, behaviour

(ert-deftest llm-pi-ai-auth-test-deepseek-env-key ()
  (llm-pi-ai-auth-test--with-env '("DEEPSEEK_API_KEY=sk-1")
    (should (equal (llm-pi-ai-get-env-api-key "deepseek") "sk-1"))
    (should (equal (llm-pi-ai-find-env-keys "deepseek") '("DEEPSEEK_API_KEY"))))
  (llm-pi-ai-auth-test--with-env nil
    (should-not (llm-pi-ai-get-env-api-key "deepseek"))
    (should-not (llm-pi-ai-find-env-keys "deepseek")))
  ;; an empty value is not a key
  (llm-pi-ai-auth-test--with-env '("DEEPSEEK_API_KEY=")
    (should-not (llm-pi-ai-get-env-api-key "deepseek"))))

(ert-deftest llm-pi-ai-auth-test-scoped-env-beats-process-env ()
  (llm-pi-ai-auth-test--with-env '("DEEPSEEK_API_KEY=process")
    (should (equal (llm-pi-ai-get-env-api-key
                    "deepseek" '(("DEEPSEEK_API_KEY" . "scoped")))
                   "scoped"))))

(ert-deftest llm-pi-ai-auth-test-unknown-provider ()
  (llm-pi-ai-auth-test--with-env '("OPENAI_API_KEY=x")
    (should-not (llm-pi-ai-find-env-keys "no-such-provider"))
    (should-not (llm-pi-ai-get-env-api-key "no-such-provider"))))

(ert-deftest llm-pi-ai-auth-test-bedrock-ambient ()
  (llm-pi-ai-auth-test--with-env '("AWS_PROFILE=dev")
    (should (equal (llm-pi-ai-get-env-api-key "amazon-bedrock") "<authenticated>")))
  (llm-pi-ai-auth-test--with-env '("AWS_ACCESS_KEY_ID=a")
    (should-not (llm-pi-ai-get-env-api-key "amazon-bedrock")))
  (llm-pi-ai-auth-test--with-env '("AWS_ACCESS_KEY_ID=a" "AWS_SECRET_ACCESS_KEY=b")
    (should (equal (llm-pi-ai-get-env-api-key "amazon-bedrock") "<authenticated>"))))

(ert-deftest llm-pi-ai-auth-test-vertex-adc ()
  "Vertex needs credentials file, project and location together."
  (let* ((file (make-temp-file "pi-ai-adc"))
         (vars (list (concat "GOOGLE_APPLICATION_CREDENTIALS=" file)
                     "GOOGLE_CLOUD_PROJECT=p" "GOOGLE_CLOUD_LOCATION=l")))
    (unwind-protect
        (progn
          (llm-pi-ai-auth-test--with-env vars
            (should (equal (llm-pi-ai-get-env-api-key "google-vertex")
                           "<authenticated>")))
          (llm-pi-ai-auth-test--with-env (butlast vars)
            (should-not (llm-pi-ai-get-env-api-key "google-vertex")))
          (llm-pi-ai-auth-test--with-env (cons "GOOGLE_CLOUD_API_KEY=k" vars)
            (should (equal (llm-pi-ai-get-env-api-key "google-vertex") "k"))))
      (delete-file file))))

;;; Auth context

(ert-deftest llm-pi-ai-auth-test-default-context-env ()
  (llm-pi-ai-auth-test--with-env '("PI_T_SET=v" "PI_T_BLANK=  ")
    (let ((ctx (llm-pi-ai-default-provider-auth-context)))
      (should (equal (llm-pi-ai-auth-context-lookup-env ctx "PI_T_SET") "v"))
      (should-not (llm-pi-ai-auth-context-lookup-env ctx "PI_T_BLANK"))
      (should-not (llm-pi-ai-auth-context-lookup-env ctx "PI_T_UNSET")))))

(ert-deftest llm-pi-ai-auth-test-default-context-file-exists ()
  (let* ((dir (make-temp-file "pi-ai-ctx" t))
         (file (expand-file-name "f" dir))
         (process-environment (cons (concat "HOME=" dir) process-environment)))
    (unwind-protect
        (let ((ctx (llm-pi-ai-default-provider-auth-context)))
          (write-region "x" nil file)
          (should (funcall (llm-pi-ai-auth-context-file-exists ctx) file))
          (should (funcall (llm-pi-ai-auth-context-file-exists ctx) "~/f"))
          (should-not (funcall (llm-pi-ai-auth-context-file-exists ctx) "~/g"))
          (should-not (funcall (llm-pi-ai-auth-context-file-exists ctx)
                               (expand-file-name "g" dir))))
      (delete-directory dir t))))

;;; envApiKeyAuth.resolve

(defun llm-pi-ai-auth-test--resolve (auth &optional credential ctx signal)
  "Call the resolve function of API-KEY-AUTH AUTH."
  (funcall (llm-pi-ai-api-key-auth-resolve auth)
           (or ctx (llm-pi-ai-default-provider-auth-context))
           credential (or signal (llm-pi-ai-make-abort-signal))))

(ert-deftest llm-pi-ai-auth-test-env-api-key-auth-stored-key-wins ()
  (llm-pi-ai-auth-test--with-env '("T_KEY=from-env")
    (let ((auth (llm-pi-ai-env-api-key-auth "Test" '("T_KEY"))))
      (should (equal (llm-pi-ai-api-key-auth-name auth) "Test"))
      (should (equal (llm-pi-ai-auth-test--resolve
                      auth '(:type api_key :key "stored" :env (("A" . "b"))))
                     '(:auth (:api-key "stored") :env (("A" . "b"))
                             :source "stored credential"))))))

(ert-deftest llm-pi-ai-auth-test-env-api-key-auth-first-set-variable ()
  (llm-pi-ai-auth-test--with-env '("T_B=bee" "T_C=sea")
    (let ((auth (llm-pi-ai-env-api-key-auth "Test" '("T_A" "T_B" "T_C"))))
      (let ((result (llm-pi-ai-auth-test--resolve auth)))
        (should (equal (plist-get result :auth) '(:api-key "bee")))
        (should (equal (plist-get result :source) "T_B"))))))

(ert-deftest llm-pi-ai-auth-test-env-api-key-auth-nothing-set ()
  (llm-pi-ai-auth-test--with-env nil
    (should-not (llm-pi-ai-auth-test--resolve
                 (llm-pi-ai-env-api-key-auth "Test" '("T_A"))))
    ;; a stored credential without a key falls through to the environment
    (should-not (llm-pi-ai-auth-test--resolve
                 (llm-pi-ai-env-api-key-auth "Test" '("T_A"))
                 '(:type api_key :env (("X" . "y")))))))

(ert-deftest llm-pi-ai-auth-test-env-api-key-auth-aborted ()
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (llm-pi-ai-abort-signal-abort signal)
    (should-error (llm-pi-ai-auth-test--resolve
                   (llm-pi-ai-env-api-key-auth "Test" '("T_A")) nil nil signal)
                  :type 'llm-pi-ai-abort-error)))

;;; resolveProviderAuth

(defun llm-pi-ai-auth-test--env-key-auth (key)
  "API key auth like the upstream test's `envKeyAuth': stored key, else KEY."
  (llm-pi-ai-make-api-key-auth
   :name "Test API key"
   :resolve (lambda (_ctx credential _signal)
              (let ((resolved (or (plist-get credential :key) key)))
                (when resolved
                  (list :auth (list :api-key resolved)
                        :source (if credential "stored" "env")))))))

(defun llm-pi-ai-auth-test--resolve-provider (auth credentials &optional overrides)
  "Resolve provider \"p1\" with API-KEY-AUTH AUTH."
  (llm-pi-ai-resolve-provider-auth
   "p1" (llm-pi-ai-make-provider-auth :api-key auth) credentials
   (llm-pi-ai-default-provider-auth-context) overrides))

(ert-deftest llm-pi-ai-auth-test-resolve-ambient-then-stored ()
  "A stored credential beats the environment; the environment only when
nothing is stored."
  (let ((credentials (llm-pi-ai-make-in-memory-credential-store))
        (auth (llm-pi-ai-auth-test--env-key-auth "env-key")))
    (let ((result (llm-pi-ai-auth-test--resolve-provider auth credentials)))
      (should (equal (plist-get (plist-get result :auth) :api-key) "env-key"))
      (should (equal (plist-get result :source) "env")))
    (funcall (llm-pi-ai-credential-store-modify credentials)
             "p1" (lambda (_current) '(:type api_key :key "stored-key")) nil)
    (let ((result (llm-pi-ai-auth-test--resolve-provider auth credentials)))
      (should (equal (plist-get (plist-get result :auth) :api-key) "stored-key"))
      (should (equal (plist-get result :source) "stored")))))

(ert-deftest llm-pi-ai-auth-test-resolve-explicit-api-key-wins ()
  (let ((credentials (llm-pi-ai-make-in-memory-credential-store))
        (auth (llm-pi-ai-auth-test--env-key-auth "env-key")))
    (funcall (llm-pi-ai-credential-store-modify credentials)
             "p1" (lambda (_current) '(:type api_key :key "stored-key")) nil)
    (should (equal (plist-get
                    (plist-get (llm-pi-ai-auth-test--resolve-provider
                                auth credentials '(:api-key "explicit-key"))
                               :auth)
                    :api-key)
                   "explicit-key"))))

(ert-deftest llm-pi-ai-auth-test-resolve-stored-oauth-blocks-ambient ()
  "A stored credential this provider has no handler for blocks the env."
  (let ((credentials (llm-pi-ai-make-in-memory-credential-store))
        (auth (llm-pi-ai-auth-test--env-key-auth "env-key")))
    (funcall (llm-pi-ai-credential-store-modify credentials)
             "p1" (lambda (_current)
                    '(:type oauth :access "a" :refresh "r" :expires 0))
             nil)
    (should-not (llm-pi-ai-auth-test--resolve-provider auth credentials))))

(ert-deftest llm-pi-ai-auth-test-resolve-no-api-key-auth ()
  "A provider with no API key auth resolves to nothing."
  (should-not (llm-pi-ai-resolve-provider-auth
               "p1" (llm-pi-ai-make-provider-auth)
               (llm-pi-ai-make-in-memory-credential-store)
               (llm-pi-ai-default-provider-auth-context)
               '(:api-key "k"))))

(ert-deftest llm-pi-ai-auth-test-resolve-wraps-api-key-failures ()
  (let* ((failing (llm-pi-ai-make-api-key-auth
                   :name "Failing"
                   :resolve (lambda (_ctx _credential _signal) (error "nope"))))
         (err (should-error (llm-pi-ai-auth-test--resolve-provider
                             failing (llm-pi-ai-make-in-memory-credential-store))
                            :type 'llm-pi-ai-models-error)))
    (should (eq (llm-pi-ai-models-error-code err) 'auth))
    (should (equal (llm-pi-ai-error-message-text err)
                   "API key auth failed for provider p1: nope"))))

(ert-deftest llm-pi-ai-auth-test-resolve-wraps-store-failures ()
  (let* ((store (llm-pi-ai-make-credential-store
                 :read (lambda (_id _options) (error "disk gone"))))
         (err (should-error (llm-pi-ai-auth-test--resolve-provider
                             (llm-pi-ai-auth-test--env-key-auth "k") store)
                            :type 'llm-pi-ai-models-error)))
    (should (eq (llm-pi-ai-models-error-code err) 'auth))
    (should (equal (llm-pi-ai-error-message-text err)
                   "Credential store read failed for p1: disk gone"))))

(ert-deftest llm-pi-ai-auth-test-resolve-env-override ()
  "An `:env' override is read before the auth context, and is merged
over the env of a stored credential; the explicit key is stored with it."
  (let* ((seen nil)
         (auth (llm-pi-ai-make-api-key-auth
                :name "Scoped"
                :resolve (lambda (ctx credential _signal)
                           (let ((account (or (cdr (assoc "ACCOUNT_ID"
                                                          (plist-get credential :env)))
                                              (llm-pi-ai-auth-context-lookup-env
                                               ctx "ACCOUNT_ID"))))
                             (setq seen (list credential account))
                             (when (and (plist-get credential :key) account)
                               (list :auth (list :api-key (plist-get credential :key)
                                                 :base-url (concat "https://example.test/"
                                                                   account))
                                     :env (list (cons "ACCOUNT_ID" account))))))))
         (result (llm-pi-ai-auth-test--with-env '("ACCOUNT_ID=from-process")
                   (llm-pi-ai-auth-test--resolve-provider
                    auth (llm-pi-ai-make-in-memory-credential-store)
                    '(:api-key "explicit-key" :env (("ACCOUNT_ID" . "acct")))))))
    (should (equal (plist-get (plist-get result :auth) :base-url)
                   "https://example.test/acct"))
    (should (equal (plist-get result :env) '(("ACCOUNT_ID" . "acct"))))
    (should (equal (car seen)
                   '(:type api_key :key "explicit-key"
                           :env (("ACCOUNT_ID" . "acct")))))))

(ert-deftest llm-pi-ai-auth-test-resolve-env-override-merges-stored-env ()
  (let* ((credentials (llm-pi-ai-make-in-memory-credential-store))
         (seen nil)
         (auth (llm-pi-ai-make-api-key-auth
                :name "Scoped"
                :resolve (lambda (_ctx credential _signal)
                           (setq seen credential)
                           (list :auth (list :api-key "k")))))
         (_ (funcall (llm-pi-ai-credential-store-modify credentials)
                     "p1" (lambda (_c) '(:type api_key :key "stored"
                                               :env (("A" . "1") ("B" . "2"))))
                     nil)))
    (llm-pi-ai-auth-test--resolve-provider auth credentials
                                           '(:env (("B" . "override") ("C" . "3"))))
    (should (equal (plist-get seen :key) "stored"))
    (should (equal (plist-get seen :env)
                   '(("A" . "1") ("B" . "override") ("C" . "3"))))))

(ert-deftest llm-pi-ai-auth-test-resolve-passes-signal ()
  "The caller's signal reaches the auth callback."
  (let* ((signal (llm-pi-ai-make-abort-signal))
         (received nil)
         (auth (llm-pi-ai-make-api-key-auth
                :name "Signal auth"
                :resolve (lambda (_ctx _credential sig)
                           (push sig received)
                           (list :auth (list :api-key "resolved"))))))
    (llm-pi-ai-auth-test--resolve-provider
     auth (llm-pi-ai-make-in-memory-credential-store) (list :signal signal))
    (should (equal received (list signal)))))

(ert-deftest llm-pi-ai-auth-test-resolve-aborted-first ()
  "An aborted signal stops resolution before the store or the auth runs."
  (let ((signal (llm-pi-ai-make-abort-signal))
        (touched nil))
    (llm-pi-ai-abort-signal-abort signal)
    (should-error
     (llm-pi-ai-auth-test--resolve-provider
      (llm-pi-ai-make-api-key-auth
       :name "x" :resolve (lambda (&rest _) (setq touched t) nil))
      (llm-pi-ai-make-credential-store
       :read (lambda (&rest _) (setq touched t) nil))
      (list :signal signal))
     :type 'llm-pi-ai-abort-error)
    (should-not touched)))

(ert-deftest llm-pi-ai-auth-test-resolve-abort-during-resolution-wins ()
  "A signal aborted inside the auth callback gives the abort, not the
ModelsError the callback's own failure would be wrapped in."
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (should-error
     (llm-pi-ai-auth-test--resolve-provider
      (llm-pi-ai-make-api-key-auth
       :name "x" :resolve (lambda (_ctx _credential sig)
                            (llm-pi-ai-abort-signal-abort sig)
                            (llm-pi-ai-abort-signal-throw-if-aborted sig)))
      (llm-pi-ai-make-in-memory-credential-store)
      (list :signal signal))
     :type 'llm-pi-ai-abort-error)))

;;; In-memory credential store

(ert-deftest llm-pi-ai-auth-test-in-memory-store ()
  (let ((store (llm-pi-ai-make-in-memory-credential-store)))
    (should-not (funcall (llm-pi-ai-credential-store-read store) "a" nil))
    ;; modify returns the stored credential; nil from fn leaves it unchanged
    (should (equal (funcall (llm-pi-ai-credential-store-modify store)
                            "a" (lambda (current) (should-not current)
                                  '(:type api_key :key "k1"))
                            nil)
                   '(:type api_key :key "k1")))
    (should (equal (funcall (llm-pi-ai-credential-store-modify store)
                            "a" (lambda (current) (should current) nil) nil)
                   '(:type api_key :key "k1")))
    (funcall (llm-pi-ai-credential-store-modify store)
             "b" (lambda (_c) '(:type oauth :access "x")) nil)
    (funcall (llm-pi-ai-credential-store-modify store)
             "a" (lambda (_c) '(:type api_key :key "k2")) nil)
    ;; list: no secrets, registration order, replaced entry keeps its place
    (should (equal (funcall (llm-pi-ai-credential-store-list store) nil)
                   '((:provider-id "a" :type api_key)
                     (:provider-id "b" :type oauth))))
    (should (equal (plist-get (funcall (llm-pi-ai-credential-store-read store)
                                       "a" nil)
                              :key)
                   "k2"))
    (funcall (llm-pi-ai-credential-store-delete store) "a" nil)
    (should-not (funcall (llm-pi-ai-credential-store-read store) "a" nil))))

(ert-deftest llm-pi-ai-auth-test-in-memory-store-aborted ()
  (let ((store (llm-pi-ai-make-in-memory-credential-store))
        (signal (llm-pi-ai-make-abort-signal))
        (ran nil))
    (llm-pi-ai-abort-signal-abort signal)
    (should-error (funcall (llm-pi-ai-credential-store-read store) "a"
                           (list :signal signal))
                  :type 'llm-pi-ai-abort-error)
    (should-error (funcall (llm-pi-ai-credential-store-modify store) "a"
                           (lambda (_c) (setq ran t) '(:type api_key :key "k"))
                           (list :signal signal))
                  :type 'llm-pi-ai-abort-error)
    (should-not ran)
    (should-not (funcall (llm-pi-ai-credential-store-read store) "a" nil))))

(provide 'llm-pi-ai-auth-test)

;;; llm-pi-ai-auth-test.el ends here
