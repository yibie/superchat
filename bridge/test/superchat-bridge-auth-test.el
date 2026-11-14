;;; superchat-bridge-auth-test.el --- Tests for superchat-bridge-auth  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-auth.el'.  Translated from upstream:
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
(require 'superchat-bridge-auth)

(defmacro superchat-bridge-auth-test--with-env (vars &rest body)
  "Run BODY with a process environment of exactly VARS, a list of \"N=V\"."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence ,vars))) ,@body))

;;; env-api-keys.test.ts

(ert-deftest superchat-bridge-auth-test-generic-github-tokens-are-not-copilot ()
  (superchat-bridge-auth-test--with-env '("GH_TOKEN=gh-token" "GITHUB_TOKEN=github-token")
    (should-not (superchat-bridge-find-env-keys "github-copilot"))
    (should-not (superchat-bridge-get-env-api-key "github-copilot"))))

(ert-deftest superchat-bridge-auth-test-copilot-token ()
  (superchat-bridge-auth-test--with-env '("COPILOT_GITHUB_TOKEN=copilot-token"
                                   "GH_TOKEN=gh-token" "GITHUB_TOKEN=github-token")
    (should (equal (superchat-bridge-find-env-keys "github-copilot")
                   '("COPILOT_GITHUB_TOKEN")))
    (should (equal (superchat-bridge-get-env-api-key "github-copilot") "copilot-token"))))

(ert-deftest superchat-bridge-auth-test-zai-coding-cn ()
  (superchat-bridge-auth-test--with-env '("ZAI_CODING_CN_API_KEY=zai-coding-cn-token")
    (should (equal (superchat-bridge-find-env-keys "zai-coding-cn")
                   '("ZAI_CODING_CN_API_KEY")))
    (should (equal (superchat-bridge-get-env-api-key "zai-coding-cn")
                   "zai-coding-cn-token"))))

(ert-deftest superchat-bridge-auth-test-anthropic-auth-token-reported-not-returned ()
  "ANTHROPIC_AUTH_TOKEN is reported but OAuth token and API key win as a key."
  (superchat-bridge-auth-test--with-env '("ANTHROPIC_AUTH_TOKEN=auth-token"
                                   "ANTHROPIC_OAUTH_TOKEN=oauth-token"
                                   "ANTHROPIC_API_KEY=api-key")
    (should (equal (superchat-bridge-find-env-keys "anthropic")
                   '("ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_OAUTH_TOKEN"
                     "ANTHROPIC_API_KEY")))
    (should (equal (superchat-bridge-get-env-api-key "anthropic") "oauth-token"))))

(ert-deftest superchat-bridge-auth-test-anthropic-auth-token-is-not-an-api-key ()
  (superchat-bridge-auth-test--with-env '("ANTHROPIC_AUTH_TOKEN=auth-token")
    (should (equal (superchat-bridge-find-env-keys "anthropic")
                   '("ANTHROPIC_AUTH_TOKEN")))
    (should-not (superchat-bridge-get-env-api-key "anthropic"))))

(ert-deftest superchat-bridge-auth-test-anthropic-oauth-token-is-an-api-key ()
  (superchat-bridge-auth-test--with-env '("ANTHROPIC_OAUTH_TOKEN=oauth-token")
    (should (equal (superchat-bridge-find-env-keys "anthropic")
                   '("ANTHROPIC_OAUTH_TOKEN")))
    (should (equal (superchat-bridge-get-env-api-key "anthropic") "oauth-token"))))

(ert-deftest superchat-bridge-auth-test-anthropic-api-key-fallback ()
  (superchat-bridge-auth-test--with-env '("ANTHROPIC_API_KEY=api-key")
    (should (equal (superchat-bridge-get-env-api-key "anthropic") "api-key"))))

;;; env-api-keys.ts, behaviour

(ert-deftest superchat-bridge-auth-test-deepseek-env-key ()
  (superchat-bridge-auth-test--with-env '("DEEPSEEK_API_KEY=sk-1")
    (should (equal (superchat-bridge-get-env-api-key "deepseek") "sk-1"))
    (should (equal (superchat-bridge-find-env-keys "deepseek") '("DEEPSEEK_API_KEY"))))
  (superchat-bridge-auth-test--with-env nil
    (should-not (superchat-bridge-get-env-api-key "deepseek"))
    (should-not (superchat-bridge-find-env-keys "deepseek")))
  ;; an empty value is not a key
  (superchat-bridge-auth-test--with-env '("DEEPSEEK_API_KEY=")
    (should-not (superchat-bridge-get-env-api-key "deepseek"))))

(ert-deftest superchat-bridge-auth-test-scoped-env-beats-process-env ()
  (superchat-bridge-auth-test--with-env '("DEEPSEEK_API_KEY=process")
    (should (equal (superchat-bridge-get-env-api-key
                    "deepseek" '(("DEEPSEEK_API_KEY" . "scoped")))
                   "scoped"))))

(ert-deftest superchat-bridge-auth-test-unknown-provider ()
  (superchat-bridge-auth-test--with-env '("OPENAI_API_KEY=x")
    (should-not (superchat-bridge-find-env-keys "no-such-provider"))
    (should-not (superchat-bridge-get-env-api-key "no-such-provider"))))

(ert-deftest superchat-bridge-auth-test-bedrock-ambient ()
  (superchat-bridge-auth-test--with-env '("AWS_PROFILE=dev")
    (should (equal (superchat-bridge-get-env-api-key "amazon-bedrock") "<authenticated>")))
  (superchat-bridge-auth-test--with-env '("AWS_ACCESS_KEY_ID=a")
    (should-not (superchat-bridge-get-env-api-key "amazon-bedrock")))
  (superchat-bridge-auth-test--with-env '("AWS_ACCESS_KEY_ID=a" "AWS_SECRET_ACCESS_KEY=b")
    (should (equal (superchat-bridge-get-env-api-key "amazon-bedrock") "<authenticated>"))))

(ert-deftest superchat-bridge-auth-test-vertex-adc ()
  "Vertex needs credentials file, project and location together."
  (let* ((file (make-temp-file "pi-ai-adc"))
         (vars (list (concat "GOOGLE_APPLICATION_CREDENTIALS=" file)
                     "GOOGLE_CLOUD_PROJECT=p" "GOOGLE_CLOUD_LOCATION=l")))
    (unwind-protect
        (progn
          (superchat-bridge-auth-test--with-env vars
            (should (equal (superchat-bridge-get-env-api-key "google-vertex")
                           "<authenticated>")))
          (superchat-bridge-auth-test--with-env (butlast vars)
            (should-not (superchat-bridge-get-env-api-key "google-vertex")))
          (superchat-bridge-auth-test--with-env (cons "GOOGLE_CLOUD_API_KEY=k" vars)
            (should (equal (superchat-bridge-get-env-api-key "google-vertex") "k"))))
      (delete-file file))))

;;; Auth context

(ert-deftest superchat-bridge-auth-test-default-context-env ()
  (superchat-bridge-auth-test--with-env '("PI_T_SET=v" "PI_T_BLANK=  ")
    (let ((ctx (superchat-bridge-default-provider-auth-context)))
      (should (equal (superchat-bridge-auth-context-lookup-env ctx "PI_T_SET") "v"))
      (should-not (superchat-bridge-auth-context-lookup-env ctx "PI_T_BLANK"))
      (should-not (superchat-bridge-auth-context-lookup-env ctx "PI_T_UNSET")))))

(ert-deftest superchat-bridge-auth-test-default-context-file-exists ()
  (let* ((dir (make-temp-file "pi-ai-ctx" t))
         (file (expand-file-name "f" dir))
         (process-environment (cons (concat "HOME=" dir) process-environment)))
    (unwind-protect
        (let ((ctx (superchat-bridge-default-provider-auth-context)))
          (write-region "x" nil file)
          (should (funcall (superchat-bridge-auth-context-file-exists ctx) file))
          (should (funcall (superchat-bridge-auth-context-file-exists ctx) "~/f"))
          (should-not (funcall (superchat-bridge-auth-context-file-exists ctx) "~/g"))
          (should-not (funcall (superchat-bridge-auth-context-file-exists ctx)
                               (expand-file-name "g" dir))))
      (delete-directory dir t))))

;;; envApiKeyAuth.resolve

(defun superchat-bridge-auth-test--resolve (auth &optional credential ctx signal)
  "Call the resolve function of API-KEY-AUTH AUTH."
  (funcall (superchat-bridge-api-key-auth-resolve auth)
           (or ctx (superchat-bridge-default-provider-auth-context))
           credential (or signal (superchat-bridge-make-abort-signal))))

(ert-deftest superchat-bridge-auth-test-env-api-key-auth-stored-key-wins ()
  (superchat-bridge-auth-test--with-env '("T_KEY=from-env")
    (let ((auth (superchat-bridge-env-api-key-auth "Test" '("T_KEY"))))
      (should (equal (superchat-bridge-api-key-auth-name auth) "Test"))
      (should (equal (superchat-bridge-auth-test--resolve
                      auth '(:type api_key :key "stored" :env (("A" . "b"))))
                     '(:auth (:api-key "stored") :env (("A" . "b"))
                             :source "stored credential"))))))

(ert-deftest superchat-bridge-auth-test-env-api-key-auth-first-set-variable ()
  (superchat-bridge-auth-test--with-env '("T_B=bee" "T_C=sea")
    (let ((auth (superchat-bridge-env-api-key-auth "Test" '("T_A" "T_B" "T_C"))))
      (let ((result (superchat-bridge-auth-test--resolve auth)))
        (should (equal (plist-get result :auth) '(:api-key "bee")))
        (should (equal (plist-get result :source) "T_B"))))))

(ert-deftest superchat-bridge-auth-test-env-api-key-auth-nothing-set ()
  (superchat-bridge-auth-test--with-env nil
    (should-not (superchat-bridge-auth-test--resolve
                 (superchat-bridge-env-api-key-auth "Test" '("T_A"))))
    ;; a stored credential without a key falls through to the environment
    (should-not (superchat-bridge-auth-test--resolve
                 (superchat-bridge-env-api-key-auth "Test" '("T_A"))
                 '(:type api_key :env (("X" . "y")))))))

(ert-deftest superchat-bridge-auth-test-env-api-key-auth-aborted ()
  (let ((signal (superchat-bridge-make-abort-signal)))
    (superchat-bridge-abort-signal-abort signal)
    (should-error (superchat-bridge-auth-test--resolve
                   (superchat-bridge-env-api-key-auth "Test" '("T_A")) nil nil signal)
                  :type 'superchat-bridge-abort-error)))

;;; resolveProviderAuth

(defun superchat-bridge-auth-test--env-key-auth (key)
  "API key auth like the upstream test's `envKeyAuth': stored key, else KEY."
  (superchat-bridge-make-api-key-auth
   :name "Test API key"
   :resolve (lambda (_ctx credential _signal)
              (let ((resolved (or (plist-get credential :key) key)))
                (when resolved
                  (list :auth (list :api-key resolved)
                        :source (if credential "stored" "env")))))))

(defun superchat-bridge-auth-test--resolve-provider (auth credentials &optional overrides)
  "Resolve provider \"p1\" with API-KEY-AUTH AUTH."
  (superchat-bridge-resolve-provider-auth
   "p1" (superchat-bridge-make-provider-auth :api-key auth) credentials
   (superchat-bridge-default-provider-auth-context) overrides))

(ert-deftest superchat-bridge-auth-test-resolve-ambient-then-stored ()
  "A stored credential beats the environment; the environment only when
nothing is stored."
  (let ((credentials (superchat-bridge-make-in-memory-credential-store))
        (auth (superchat-bridge-auth-test--env-key-auth "env-key")))
    (let ((result (superchat-bridge-auth-test--resolve-provider auth credentials)))
      (should (equal (plist-get (plist-get result :auth) :api-key) "env-key"))
      (should (equal (plist-get result :source) "env")))
    (funcall (superchat-bridge-credential-store-modify credentials)
             "p1" (lambda (_current) '(:type api_key :key "stored-key")) nil)
    (let ((result (superchat-bridge-auth-test--resolve-provider auth credentials)))
      (should (equal (plist-get (plist-get result :auth) :api-key) "stored-key"))
      (should (equal (plist-get result :source) "stored")))))

(ert-deftest superchat-bridge-auth-test-resolve-explicit-api-key-wins ()
  (let ((credentials (superchat-bridge-make-in-memory-credential-store))
        (auth (superchat-bridge-auth-test--env-key-auth "env-key")))
    (funcall (superchat-bridge-credential-store-modify credentials)
             "p1" (lambda (_current) '(:type api_key :key "stored-key")) nil)
    (should (equal (plist-get
                    (plist-get (superchat-bridge-auth-test--resolve-provider
                                auth credentials '(:api-key "explicit-key"))
                               :auth)
                    :api-key)
                   "explicit-key"))))

(ert-deftest superchat-bridge-auth-test-resolve-stored-oauth-blocks-ambient ()
  "A stored credential this provider has no handler for blocks the env."
  (let ((credentials (superchat-bridge-make-in-memory-credential-store))
        (auth (superchat-bridge-auth-test--env-key-auth "env-key")))
    (funcall (superchat-bridge-credential-store-modify credentials)
             "p1" (lambda (_current)
                    '(:type oauth :access "a" :refresh "r" :expires 0))
             nil)
    (should-not (superchat-bridge-auth-test--resolve-provider auth credentials))))

(ert-deftest superchat-bridge-auth-test-resolve-no-api-key-auth ()
  "A provider with no API key auth resolves to nothing."
  (should-not (superchat-bridge-resolve-provider-auth
               "p1" (superchat-bridge-make-provider-auth)
               (superchat-bridge-make-in-memory-credential-store)
               (superchat-bridge-default-provider-auth-context)
               '(:api-key "k"))))

(ert-deftest superchat-bridge-auth-test-resolve-wraps-api-key-failures ()
  (let* ((failing (superchat-bridge-make-api-key-auth
                   :name "Failing"
                   :resolve (lambda (_ctx _credential _signal) (error "nope"))))
         (err (should-error (superchat-bridge-auth-test--resolve-provider
                             failing (superchat-bridge-make-in-memory-credential-store))
                            :type 'superchat-bridge-models-error)))
    (should (eq (superchat-bridge-models-error-code err) 'auth))
    (should (equal (superchat-bridge-error-message-text err)
                   "API key auth failed for provider p1: nope"))))

(ert-deftest superchat-bridge-auth-test-resolve-wraps-store-failures ()
  (let* ((store (superchat-bridge-make-credential-store
                 :read (lambda (_id _options) (error "disk gone"))))
         (err (should-error (superchat-bridge-auth-test--resolve-provider
                             (superchat-bridge-auth-test--env-key-auth "k") store)
                            :type 'superchat-bridge-models-error)))
    (should (eq (superchat-bridge-models-error-code err) 'auth))
    (should (equal (superchat-bridge-error-message-text err)
                   "Credential store read failed for p1: disk gone"))))

(ert-deftest superchat-bridge-auth-test-resolve-env-override ()
  "An `:env' override is read before the auth context, and is merged
over the env of a stored credential; the explicit key is stored with it."
  (let* ((seen nil)
         (auth (superchat-bridge-make-api-key-auth
                :name "Scoped"
                :resolve (lambda (ctx credential _signal)
                           (let ((account (or (cdr (assoc "ACCOUNT_ID"
                                                          (plist-get credential :env)))
                                              (superchat-bridge-auth-context-lookup-env
                                               ctx "ACCOUNT_ID"))))
                             (setq seen (list credential account))
                             (when (and (plist-get credential :key) account)
                               (list :auth (list :api-key (plist-get credential :key)
                                                 :base-url (concat "https://example.test/"
                                                                   account))
                                     :env (list (cons "ACCOUNT_ID" account))))))))
         (result (superchat-bridge-auth-test--with-env '("ACCOUNT_ID=from-process")
                   (superchat-bridge-auth-test--resolve-provider
                    auth (superchat-bridge-make-in-memory-credential-store)
                    '(:api-key "explicit-key" :env (("ACCOUNT_ID" . "acct")))))))
    (should (equal (plist-get (plist-get result :auth) :base-url)
                   "https://example.test/acct"))
    (should (equal (plist-get result :env) '(("ACCOUNT_ID" . "acct"))))
    (should (equal (car seen)
                   '(:type api_key :key "explicit-key"
                           :env (("ACCOUNT_ID" . "acct")))))))

(ert-deftest superchat-bridge-auth-test-resolve-env-override-merges-stored-env ()
  (let* ((credentials (superchat-bridge-make-in-memory-credential-store))
         (seen nil)
         (auth (superchat-bridge-make-api-key-auth
                :name "Scoped"
                :resolve (lambda (_ctx credential _signal)
                           (setq seen credential)
                           (list :auth (list :api-key "k")))))
         (_ (funcall (superchat-bridge-credential-store-modify credentials)
                     "p1" (lambda (_c) '(:type api_key :key "stored"
                                               :env (("A" . "1") ("B" . "2"))))
                     nil)))
    (superchat-bridge-auth-test--resolve-provider auth credentials
                                           '(:env (("B" . "override") ("C" . "3"))))
    (should (equal (plist-get seen :key) "stored"))
    (should (equal (plist-get seen :env)
                   '(("A" . "1") ("B" . "override") ("C" . "3"))))))

(ert-deftest superchat-bridge-auth-test-resolve-passes-signal ()
  "The caller's signal reaches the auth callback."
  (let* ((signal (superchat-bridge-make-abort-signal))
         (received nil)
         (auth (superchat-bridge-make-api-key-auth
                :name "Signal auth"
                :resolve (lambda (_ctx _credential sig)
                           (push sig received)
                           (list :auth (list :api-key "resolved"))))))
    (superchat-bridge-auth-test--resolve-provider
     auth (superchat-bridge-make-in-memory-credential-store) (list :signal signal))
    (should (equal received (list signal)))))

(ert-deftest superchat-bridge-auth-test-resolve-aborted-first ()
  "An aborted signal stops resolution before the store or the auth runs."
  (let ((signal (superchat-bridge-make-abort-signal))
        (touched nil))
    (superchat-bridge-abort-signal-abort signal)
    (should-error
     (superchat-bridge-auth-test--resolve-provider
      (superchat-bridge-make-api-key-auth
       :name "x" :resolve (lambda (&rest _) (setq touched t) nil))
      (superchat-bridge-make-credential-store
       :read (lambda (&rest _) (setq touched t) nil))
      (list :signal signal))
     :type 'superchat-bridge-abort-error)
    (should-not touched)))

(ert-deftest superchat-bridge-auth-test-resolve-abort-during-resolution-wins ()
  "A signal aborted inside the auth callback gives the abort, not the
ModelsError the callback's own failure would be wrapped in."
  (let ((signal (superchat-bridge-make-abort-signal)))
    (should-error
     (superchat-bridge-auth-test--resolve-provider
      (superchat-bridge-make-api-key-auth
       :name "x" :resolve (lambda (_ctx _credential sig)
                            (superchat-bridge-abort-signal-abort sig)
                            (superchat-bridge-abort-signal-throw-if-aborted sig)))
      (superchat-bridge-make-in-memory-credential-store)
      (list :signal signal))
     :type 'superchat-bridge-abort-error)))

;;; In-memory credential store

(ert-deftest superchat-bridge-auth-test-in-memory-store ()
  (let ((store (superchat-bridge-make-in-memory-credential-store)))
    (should-not (funcall (superchat-bridge-credential-store-read store) "a" nil))
    ;; modify returns the stored credential; nil from fn leaves it unchanged
    (should (equal (funcall (superchat-bridge-credential-store-modify store)
                            "a" (lambda (current) (should-not current)
                                  '(:type api_key :key "k1"))
                            nil)
                   '(:type api_key :key "k1")))
    (should (equal (funcall (superchat-bridge-credential-store-modify store)
                            "a" (lambda (current) (should current) nil) nil)
                   '(:type api_key :key "k1")))
    (funcall (superchat-bridge-credential-store-modify store)
             "b" (lambda (_c) '(:type oauth :access "x")) nil)
    (funcall (superchat-bridge-credential-store-modify store)
             "a" (lambda (_c) '(:type api_key :key "k2")) nil)
    ;; list: no secrets, registration order, replaced entry keeps its place
    (should (equal (funcall (superchat-bridge-credential-store-list store) nil)
                   '((:provider-id "a" :type api_key)
                     (:provider-id "b" :type oauth))))
    (should (equal (plist-get (funcall (superchat-bridge-credential-store-read store)
                                       "a" nil)
                              :key)
                   "k2"))
    (funcall (superchat-bridge-credential-store-delete store) "a" nil)
    (should-not (funcall (superchat-bridge-credential-store-read store) "a" nil))))

(ert-deftest superchat-bridge-auth-test-in-memory-store-aborted ()
  (let ((store (superchat-bridge-make-in-memory-credential-store))
        (signal (superchat-bridge-make-abort-signal))
        (ran nil))
    (superchat-bridge-abort-signal-abort signal)
    (should-error (funcall (superchat-bridge-credential-store-read store) "a"
                           (list :signal signal))
                  :type 'superchat-bridge-abort-error)
    (should-error (funcall (superchat-bridge-credential-store-modify store) "a"
                           (lambda (_c) (setq ran t) '(:type api_key :key "k"))
                           (list :signal signal))
                  :type 'superchat-bridge-abort-error)
    (should-not ran)
    (should-not (funcall (superchat-bridge-credential-store-read store) "a" nil))))

(provide 'superchat-bridge-auth-test)

;;; superchat-bridge-auth-test.el ends here
