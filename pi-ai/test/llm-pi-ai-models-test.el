;;; llm-pi-ai-models-test.el --- Tests for llm-pi-ai-models  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-models.el', its port of `src/models.ts'.
;;
;; Translated from upstream:
;;   test/models-runtime.test.ts  "applies request-wide pricing tiers...",
;;     "registers, replaces, and deletes providers", "lists and finds
;;     models per provider", "swallows provider source failures...",
;;     "merges resolved auth into stream options; explicit options win
;;     per field", "adds model headers only for model auth and transforms
;;     assembled headers once", "uses explicit request api key and env",
;;     "produces an error stream for unknown providers", "streams through
;;     the provider", "resolves auth: stored credential owns the provider"
;;   test/max-thinking.test.ts  "is opt-in for ordinary reasoning models",
;;     "supports a hole between high and max".
;; The rest are from the source's behaviour: `calculateCost' cache
;; writes, `clampThinkingLevel' search order, `createProvider' dispatch
;; on `model.api', `hasApi', `modelsAreEqual', lazy streams.  Cases that
;; need stores, refresh, OAuth or telemetry are not ported.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'llm-pi-ai-models)

;;; Helpers

(defun llm-pi-ai-models-test--model (provider id &rest args)
  "Return a chat model ID of PROVIDER with ARGS as extra slots."
  (apply #'llm-pi-ai-make-model
         (append args
                 (list :id id :name id :api "test-api" :provider provider
                       :base-url "https://example.test/v1" :reasoning nil
                       :input '("text")
                       :cost (llm-pi-ai-make-model-cost)
                       :context-window 10000 :max-tokens 1000))))

(defun llm-pi-ai-models-test--done-message (model text)
  "Return a stop message of MODEL with TEXT."
  (llm-pi-ai-make-assistant-message
   :content (list (llm-pi-ai-make-text-content :text text))
   :api (llm-pi-ai-model-api model)
   :provider (llm-pi-ai-model-provider model)
   :model (llm-pi-ai-model-id model)
   :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason "stop"
   :timestamp (round (* 1000 (float-time)))))

(defun llm-pi-ai-models-test--canned-stream (model)
  "Return a complete stream of MODEL: start, done."
  (let ((stream (llm-pi-ai-create-assistant-message-event-stream))
        (message (llm-pi-ai-models-test--done-message model "ok")))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "start" :partial message))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "done" :reason "stop" :message message))
    (llm-pi-ai-event-stream-end stream message)
    stream))

(defun llm-pi-ai-models-test--api (calls)
  "Return streams that push a call record (MODEL CONTEXT OPTIONS) onto CALLS.
CALLS is a symbol whose value is the list."
  (let ((respond (lambda (model context options)
                   (set calls (append (symbol-value calls)
                                      (list (list model context options))))
                   (llm-pi-ai-models-test--canned-stream model))))
    (llm-pi-ai-make-provider-streams :stream respond :stream-simple respond)))

(defvar llm-pi-ai-models-test--calls nil "Calls the test API received.")

(defun llm-pi-ai-models-test--ambient-auth ()
  "Auth that reports configured with no auth values, like `ambientAuth'."
  (llm-pi-ai-make-provider-auth
   :api-key (llm-pi-ai-make-api-key-auth
             :name "Ambient"
             :resolve (lambda (_ctx _credential _signal) (list :auth nil)))))

(defun llm-pi-ai-models-test--provider (id &rest args)
  "Return a provider ID with the test API; ARGS are `create-provider' keys."
  (apply #'llm-pi-ai-create-provider
         (append args
                 (list :id id
                       :auth (llm-pi-ai-models-test--ambient-auth)
                       :models (list (llm-pi-ai-models-test--model id "model-a"))
                       :api (llm-pi-ai-models-test--api
                             'llm-pi-ai-models-test--calls)))))

(defun llm-pi-ai-models-test--context ()
  "Return a context with one user message."
  (llm-pi-ai-make-context
   :messages (list (llm-pi-ai-make-user-message :content "hi" :timestamp 1))))

(defun llm-pi-ai-models-test--events (stream)
  "Return the event types of the complete STREAM."
  (mapcar #'llm-pi-ai-assistant-message-event-type
          (llm-pi-ai-event-stream-to-list stream)))

(defun llm-pi-ai-models-test--usage (cache-write &optional cache-write1h)
  "Return the usage of the upstream tier test with CACHE-WRITE."
  (llm-pi-ai-make-usage :input 200000 :output 100000 :cache-read 72000
                        :cache-write cache-write :cache-write1h cache-write1h
                        :total-tokens (+ 372000 cache-write)
                        :cost (llm-pi-ai-make-cost)))

(defmacro llm-pi-ai-models-test--with-calls (&rest body)
  "Run BODY with an empty call list."
  `(let ((llm-pi-ai-models-test--calls nil)) ,@body))

;;; calculateCost

(defun llm-pi-ai-models-test--tiered-model ()
  "The model of upstream's tier test."
  (llm-pi-ai-models-test--model
   "openai" "gpt-5.6-sol"
   :cost (llm-pi-ai-make-model-cost
          :input 5 :output 30 :cache-read 0.5 :cache-write 6.25
          :tiers (list (llm-pi-ai-make-model-cost-tier
                        :input-tokens-above 272000
                        :input 10 :output 45 :cache-read 1 :cache-write 12.5)))))

(ert-deftest llm-pi-ai-models-test-cost-below-tier ()
  "372000 input tokens in all is above the tier; 372000 is checked with
cache writes at 0: input+read = 272000, not above 272000."
  (let* ((model (llm-pi-ai-models-test--tiered-model))
         (cost (llm-pi-ai-calculate-cost
                model (llm-pi-ai-models-test--usage 0))))
    (should (= (llm-pi-ai-cost-input cost) 1))
    (should (= (llm-pi-ai-cost-output cost) 3))
    (should (< (abs (- (llm-pi-ai-cost-cache-read cost) 0.036)) 1e-12))
    (should (= (llm-pi-ai-cost-cache-write cost) 0))))

(ert-deftest llm-pi-ai-models-test-cost-request-wide-tier ()
  "One more token above the threshold moves the whole request to the tier."
  (let* ((model (llm-pi-ai-models-test--tiered-model))
         (cost (llm-pi-ai-calculate-cost
                model (llm-pi-ai-models-test--usage 1))))
    (should (= (llm-pi-ai-cost-input cost) 2))
    (should (= (llm-pi-ai-cost-output cost) 4.5))
    (should (= (llm-pi-ai-cost-cache-read cost) 0.072))
    (should (= (llm-pi-ai-cost-cache-write cost) 0.0000125))
    (should (= (llm-pi-ai-cost-total cost)
               (+ (llm-pi-ai-cost-input cost) (llm-pi-ai-cost-output cost)
                  (llm-pi-ai-cost-cache-read cost)
                  (llm-pi-ai-cost-cache-write cost))))))

(ert-deftest llm-pi-ai-models-test-cost-highest-matching-tier ()
  "Of several tiers the highest threshold below the input applies, whatever
their order."
  (let* ((model (llm-pi-ai-models-test--model
                 "p" "m"
                 :cost (llm-pi-ai-make-model-cost
                        :input 1 :output 1
                        :tiers (list (llm-pi-ai-make-model-cost-tier
                                      :input-tokens-above 2000000 :input 30 :output 30)
                                     (llm-pi-ai-make-model-cost-tier
                                      :input-tokens-above 1000 :input 20 :output 20)
                                     (llm-pi-ai-make-model-cost-tier
                                      :input-tokens-above 100 :input 10 :output 10)))))
         (usage (llm-pi-ai-make-usage :input 5000 :output 1000000
                                      :cost (llm-pi-ai-make-cost))))
    (llm-pi-ai-calculate-cost model usage)
    (should (= (llm-pi-ai-cost-input (llm-pi-ai-usage-cost usage)) 0.1))
    (should (= (llm-pi-ai-cost-output (llm-pi-ai-usage-cost usage)) 20))))

(ert-deftest llm-pi-ai-models-test-cost-one-hour-cache-write ()
  "Cache writes of one hour cost twice the input rate; the rest the write rate."
  (let* ((model (llm-pi-ai-models-test--model
                 "p" "m" :cost (llm-pi-ai-make-model-cost
                                :input 3 :output 15 :cache-read 0.3 :cache-write 3.75)))
         (usage (llm-pi-ai-make-usage :input 0 :output 0 :cache-write 1000000
                                      :cache-write1h 400000
                                      :cost (llm-pi-ai-make-cost)))
         (cost (llm-pi-ai-calculate-cost model usage)))
    ;; (3.75 * 600000 + 3 * 2 * 400000) / 1e6
    (should (< (abs (- (llm-pi-ai-cost-cache-write cost) 4.65)) 1e-9))
    (should (eq cost (llm-pi-ai-usage-cost usage)))))

(ert-deftest llm-pi-ai-models-test-cost-integer-rates-are-not-truncated ()
  "A rate of 1 per million tokens times 1 token is a millionth, not 0."
  (let* ((model (llm-pi-ai-models-test--model
                 "p" "m" :cost (llm-pi-ai-make-model-cost :input 1)))
         (usage (llm-pi-ai-make-usage :input 1 :cost (llm-pi-ai-make-cost))))
    (llm-pi-ai-calculate-cost model usage)
    (should (= (llm-pi-ai-cost-input (llm-pi-ai-usage-cost usage)) 1e-6))))

;;; Thinking levels

(ert-deftest llm-pi-ai-models-test-thinking-levels-no-reasoning ()
  (should (equal (llm-pi-ai-get-supported-thinking-levels
                  (llm-pi-ai-models-test--model "p" "m"))
                 '("off")))
  (should (equal (llm-pi-ai-clamp-thinking-level
                  (llm-pi-ai-models-test--model "p" "m") "high")
                 "off")))

(ert-deftest llm-pi-ai-models-test-thinking-levels-ordinary-reasoning ()
  "xhigh and max are opt-in (max-thinking.test.ts, ordinary reasoning)."
  (let ((model (llm-pi-ai-models-test--model "p" "m" :reasoning t)))
    (should (equal (llm-pi-ai-get-supported-thinking-levels model)
                   '("off" "minimal" "low" "medium" "high")))
    (should (equal (llm-pi-ai-clamp-thinking-level model "max") "high"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "xhigh") "high"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "medium") "medium"))))

(ert-deftest llm-pi-ai-models-test-thinking-levels-hole-between-high-and-max ()
  "xhigh null, max mapped (max-thinking.test.ts, hole): xhigh clamps up to max."
  (let ((model (llm-pi-ai-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("xhigh" . :null) ("max" . "max")))))
    (should (equal (llm-pi-ai-get-supported-thinking-levels model)
                   '("off" "minimal" "low" "medium" "high" "max")))
    (should (equal (llm-pi-ai-clamp-thinking-level model "xhigh") "max"))))

(ert-deftest llm-pi-ai-models-test-thinking-levels-null-removes-level ()
  "A level mapped to null is removed; xhigh and max appear when mapped."
  (let ((model (llm-pi-ai-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("off" . :null) ("minimal" . :null)
                                      ("xhigh" . "xhigh") ("max" . "max")))))
    (should (equal (llm-pi-ai-get-supported-thinking-levels model)
                   '("low" "medium" "high" "xhigh" "max")))
    ;; off is not supported: the nearest above it is low
    (should (equal (llm-pi-ai-clamp-thinking-level model "off") "low"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "minimal") "low"))))

(ert-deftest llm-pi-ai-models-test-clamp-searches-up-then-down ()
  "A level not supported goes to the next supported one above, else below."
  (let ((model (llm-pi-ai-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("minimal" . :null) ("medium" . :null)
                                      ("high" . :null)))))
    (should (equal (llm-pi-ai-get-supported-thinking-levels model)
                   '("off" "low")))
    (should (equal (llm-pi-ai-clamp-thinking-level model "medium") "low"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "high") "low"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "max") "low"))
    (should (equal (llm-pi-ai-clamp-thinking-level model "minimal") "low"))
    ;; a level that is no level at all gives the first supported one
    (should (equal (llm-pi-ai-clamp-thinking-level model "nonsense") "off"))))

;;; hasApi, modelsAreEqual

(ert-deftest llm-pi-ai-models-test-has-api ()
  (let ((chat (llm-pi-ai-models-test--model "p" "m"))
        (image (llm-pi-ai-make-image-model :id "i" :provider "p" :api "test-api")))
    (should (llm-pi-ai-has-api chat "test-api"))
    (should-not (llm-pi-ai-has-api chat "openai-completions"))
    ;; non-chat models never match, even when their api id equals
    (should-not (llm-pi-ai-has-api image "test-api"))))

(ert-deftest llm-pi-ai-models-test-models-are-equal ()
  (let ((a (llm-pi-ai-models-test--model "p" "m"))
        (same (llm-pi-ai-models-test--model "p" "m" :name "other"))
        (other-id (llm-pi-ai-models-test--model "p" "n"))
        (other-provider (llm-pi-ai-models-test--model "q" "m"))
        (image (llm-pi-ai-make-image-model :id "m" :provider "p")))
    (should (llm-pi-ai-models-are-equal a same))
    (should-not (llm-pi-ai-models-are-equal a other-id))
    (should-not (llm-pi-ai-models-are-equal a other-provider))
    ;; same id and provider, different type
    (should-not (llm-pi-ai-models-are-equal a image))
    (should-not (llm-pi-ai-models-are-equal a nil))
    (should-not (llm-pi-ai-models-are-equal nil a))
    (should-not (llm-pi-ai-models-are-equal nil nil))))

;;; createProvider

(ert-deftest llm-pi-ai-models-test-create-provider-fields ()
  (let ((provider (llm-pi-ai-models-test--provider "p1")))
    (should (equal (llm-pi-ai-provider-id provider) "p1"))
    ;; name defaults to the id
    (should (equal (llm-pi-ai-provider-name provider) "p1"))
    (should (equal (llm-pi-ai-provider-name
                    (llm-pi-ai-models-test--provider "p1" :name "Pee"))
                   "Pee"))
    (should (equal (mapcar #'llm-pi-ai-model-id
                           (funcall (llm-pi-ai-provider-get-models provider)))
                   '("model-a")))))

(ert-deftest llm-pi-ai-models-test-create-provider-requires-api ()
  (should-error (llm-pi-ai-create-provider
                 :id "p" :auth (llm-pi-ai-models-test--ambient-auth) :models nil))
  (should-error (llm-pi-ai-create-provider
                 :id "p" :auth (llm-pi-ai-models-test--ambient-auth)
                 :models nil :api nil))
  ;; an empty api map is rejected too
  (should-error (llm-pi-ai-create-provider
                 :id "p" :auth (llm-pi-ai-models-test--ambient-auth)
                 :models nil :api '())))

(ert-deftest llm-pi-ai-models-test-create-provider-rejects-left-out-keys ()
  "No silent drop of what is not ported."
  (should-error (llm-pi-ai-create-provider
                 :id "p" :auth (llm-pi-ai-models-test--ambient-auth)
                 :api (llm-pi-ai-models-test--api 'llm-pi-ai-models-test--calls)
                 :fetch-models #'ignore)))

(ert-deftest llm-pi-ai-models-test-get-models-types ()
  "`get-models' is the chat models; `get-all-models' every type."
  (let* ((chat (llm-pi-ai-models-test--model "p" "c"))
         (image (llm-pi-ai-make-image-model :id "i" :provider "p"))
         (provider (llm-pi-ai-models-test--provider
                    "p" :models (list chat image))))
    (should (equal (funcall (llm-pi-ai-provider-get-models provider)) (list chat)))
    (should (equal (funcall (llm-pi-ai-provider-get-all-models provider))
                   (list chat image)))))

(ert-deftest llm-pi-ai-models-test-create-provider-dispatch-on-api ()
  "With an api map, a model goes to the implementation of its `api'; a model
whose api has none gets an error stream."
  (let* ((seen nil)
         (make (lambda (label)
                 (let ((respond (lambda (model _context _options)
                                  (push label seen)
                                  (llm-pi-ai-models-test--canned-stream model))))
                   (llm-pi-ai-make-provider-streams :stream respond
                                                    :stream-simple respond))))
         (provider (llm-pi-ai-create-provider
                    :id "p" :auth (llm-pi-ai-models-test--ambient-auth)
                    :models nil
                    :api (list (cons "api-a" (funcall make 'a))
                               (cons "api-b" (funcall make 'b)))))
         (context (llm-pi-ai-normalize-context (llm-pi-ai-models-test--context))))
    (funcall (llm-pi-ai-provider-stream provider)
             (llm-pi-ai-models-test--model "p" "m" :api "api-b") context nil)
    (funcall (llm-pi-ai-provider-stream-simple provider)
             (llm-pi-ai-models-test--model "p" "m" :api "api-a") context nil)
    (should (equal (nreverse seen) '(b a)))
    (let* ((stream (funcall (llm-pi-ai-provider-stream provider)
                            (llm-pi-ai-models-test--model "p" "m" :api "api-c")
                            context nil))
           (events (llm-pi-ai-event-stream-to-list stream))
           (message (llm-pi-ai-event-stream-result stream)))
      (should (equal (mapcar #'llm-pi-ai-assistant-message-event-type events)
                     '("error")))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Provider p has no API implementation for \"api-c\"")))))

;;; Models collection

(ert-deftest llm-pi-ai-models-test-registers-replaces-and-deletes-providers ()
  (let ((models (llm-pi-ai-create-models)))
    (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
    (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p2"))
    (should (equal (mapcar #'llm-pi-ai-provider-id
                           (llm-pi-ai-models-get-providers models))
                   '("p1" "p2")))
    ;; replacing keeps the place, and there is still one p1
    (let ((replacement (llm-pi-ai-models-test--provider "p1")))
      (llm-pi-ai-models-set-provider models replacement)
      (should (eq (llm-pi-ai-models-get-provider models "p1") replacement))
      (should (equal (mapcar #'llm-pi-ai-provider-id
                             (llm-pi-ai-models-get-providers models))
                     '("p1" "p2"))))
    (llm-pi-ai-models-delete-provider models "p1")
    (should-not (llm-pi-ai-models-get-provider models "p1"))
    (llm-pi-ai-models-clear-providers models)
    (should-not (llm-pi-ai-models-get-providers models))))

(ert-deftest llm-pi-ai-models-test-lists-and-finds-models ()
  (let ((models (llm-pi-ai-create-models)))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "p1" :models (list (llm-pi-ai-models-test--model "p1" "m1")
                                (llm-pi-ai-models-test--model "p1" "m2"))))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "p2" :models (list (llm-pi-ai-models-test--model "p2" "m3"))))
    (let ((ids (lambda (list) (mapcar #'llm-pi-ai-model-id list))))
      (should (equal (funcall ids (llm-pi-ai-models-get-models models))
                     '("m1" "m2" "m3")))
      (should (equal (funcall ids (llm-pi-ai-models-get-models models "p1"))
                     '("m1" "m2")))
      (should-not (llm-pi-ai-models-get-models models "nope")))
    (should (equal (llm-pi-ai-model-id (llm-pi-ai-models-get-model models "p2" "m3"))
                   "m3"))
    (should-not (llm-pi-ai-models-get-model models "p2" "missing"))
    (should-not (llm-pi-ai-models-get-model models "nope" "m3"))
    (let ((found (llm-pi-ai-models-get-model models "p2" "m3")))
      (should-not (llm-pi-ai-has-api found "openai-completions"))
      (should (llm-pi-ai-has-api found "test-api")))))

(ert-deftest llm-pi-ai-models-test-swallows-provider-source-failures ()
  "A provider whose `get-models' fails yields no models, in either listing."
  (let ((models (llm-pi-ai-create-models))
        (broken (llm-pi-ai-models-test--provider "broken")))
    (setf (llm-pi-ai-provider-get-models broken) (lambda () (error "boom")))
    (llm-pi-ai-models-set-provider models broken)
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "ok" :models (list (llm-pi-ai-models-test--model "ok" "m1"))))
    (should (equal (mapcar #'llm-pi-ai-model-id (llm-pi-ai-models-get-models models))
                   '("m1")))
    (should-not (llm-pi-ai-models-get-models models "broken"))
    ;; precise failures come from the provider directly
    (should-error (funcall (llm-pi-ai-provider-get-models broken)))))

;;; getAuth

(defun llm-pi-ai-models-test--env-key-auth (key)
  "Provider auth like upstream's `envKeyAuth': stored key, else KEY."
  (llm-pi-ai-make-provider-auth
   :api-key (llm-pi-ai-make-api-key-auth
             :name "Test API key"
             :resolve (lambda (_ctx credential _signal)
                        (let ((resolved (or (plist-get credential :key) key)))
                          (when resolved
                            (list :auth (list :api-key resolved)
                                  :source (if credential "stored" "env"))))))))

(ert-deftest llm-pi-ai-models-test-get-auth-stored-credential-owns-provider ()
  (let* ((credentials (llm-pi-ai-make-in-memory-credential-store))
         (models (llm-pi-ai-create-models :credentials credentials))
         (model (llm-pi-ai-models-test--model "p1" "model-a")))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "p1" :auth (llm-pi-ai-models-test--env-key-auth "env-key")))
    (let ((api-key (lambda (result) (plist-get (plist-get result :auth) :api-key))))
      ;; model and provider-id forms resolve the same provider-scoped auth
      (should (equal (funcall api-key (llm-pi-ai-models-get-auth models model))
                     "env-key"))
      (should (equal (funcall api-key (llm-pi-ai-models-get-auth models "p1"))
                     "env-key"))
      (should (equal (funcall api-key (llm-pi-ai-models-get-auth
                                       models model '(:api-key "explicit-key")))
                     "explicit-key"))
      ;; stored api-key credential beats the environment
      (funcall (llm-pi-ai-credential-store-modify credentials)
               "p1" (lambda (_c) '(:type api_key :key "stored-key")) nil)
      (let ((result (llm-pi-ai-models-get-auth models "p1")))
        (should (equal (funcall api-key result) "stored-key"))
        (should (equal (plist-get result :source) "stored"))))))

(ert-deftest llm-pi-ai-models-test-get-auth-unknown-provider ()
  (should-not (llm-pi-ai-models-get-auth (llm-pi-ai-create-models) "ghost")))

(ert-deftest llm-pi-ai-models-test-get-auth-model-headers ()
  "Model headers join the auth only when asked with a model."
  (let ((models (llm-pi-ai-create-models))
        (model (llm-pi-ai-models-test--model
                "p1" "model-a" :headers '(("x-model" . "model") ("x-shared" . "model")))))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "p1" :auth (llm-pi-ai-models-test--env-key-auth "key")))
    (should-not (plist-get (plist-get (llm-pi-ai-models-get-auth models "p1") :auth)
                           :headers))
    (should (equal (plist-get (plist-get (llm-pi-ai-models-get-auth models model)
                                         :auth)
                              :headers)
                   '(("x-model" . "model") ("x-shared" . "model"))))))

;;; stream

(ert-deftest llm-pi-ai-models-test-streams-through-the-provider ()
  (llm-pi-ai-models-test--with-calls
   (let ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a")))
     (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
     (let ((stream (llm-pi-ai-models-stream-simple
                    models model (llm-pi-ai-models-test--context))))
       (should (equal (llm-pi-ai-models-test--events stream) '("start" "done")))
       (should (equal (llm-pi-ai-assistant-message-stop-reason
                       (llm-pi-ai-event-stream-result stream))
                      "stop"))))))

(ert-deftest llm-pi-ai-models-test-stream-and-stream-simple-pick-the-function ()
  (let* ((which nil)
         (api (llm-pi-ai-make-provider-streams
               :stream (lambda (model _c _o)
                         (push 'stream which)
                         (llm-pi-ai-models-test--canned-stream model))
               :stream-simple (lambda (model _c _o)
                                (push 'stream-simple which)
                                (llm-pi-ai-models-test--canned-stream model))))
         (models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a")))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider "p1" :api api))
    (llm-pi-ai-models-stream models model (llm-pi-ai-models-test--context))
    (llm-pi-ai-models-stream-simple models model (llm-pi-ai-models-test--context))
    (should (equal (nreverse which) '(stream stream-simple)))))

(ert-deftest llm-pi-ai-models-test-stream-normalizes-the-context ()
  "The provider gets a transcript: the system prompt is a leading message."
  (llm-pi-ai-models-test--with-calls
   (let ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a")))
     (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
     (llm-pi-ai-models-stream-simple
      models model (llm-pi-ai-make-context
                    :system-prompt "sys"
                    :messages (list (llm-pi-ai-make-user-message
                                     :content "hi" :timestamp 1))))
     (let ((context (nth 1 (car llm-pi-ai-models-test--calls))))
       (should-not (llm-pi-ai-context-system-prompt context))
       (should (equal (mapcar #'llm-pi-ai-message-role
                              (llm-pi-ai-context-messages context))
                      '("system" "user")))))))

(ert-deftest llm-pi-ai-models-test-unknown-provider-is-an-error-stream ()
  "No signal to the caller: the stream's error event says what is wrong."
  (let* ((models (llm-pi-ai-create-models))
         (stream (llm-pi-ai-models-stream-simple
                  models (llm-pi-ai-models-test--model "ghost" "model-a")
                  (llm-pi-ai-models-test--context)))
         (events (llm-pi-ai-event-stream-to-list stream))
         (message (llm-pi-ai-event-stream-result stream)))
    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-type events)
                   '("error")))
    (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
    (should (string-search "Unknown provider: ghost"
                           (llm-pi-ai-assistant-message-error-message message)))
    (should (equal (llm-pi-ai-assistant-message-provider message) "ghost"))
    (should (equal (llm-pi-ai-assistant-message-model message) "model-a"))))

(ert-deftest llm-pi-ai-models-test-provider-not-configured ()
  (let* ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a")))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider
             "p1" :auth (llm-pi-ai-models-test--env-key-auth nil)))
    (let ((message (llm-pi-ai-event-stream-result
                    (llm-pi-ai-models-stream-simple
                     models model (llm-pi-ai-models-test--context)))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Provider is not configured: p1")))))

(ert-deftest llm-pi-ai-models-test-non-chat-model-is-an-error-stream ()
  (let* ((models (llm-pi-ai-create-models))
         (image (llm-pi-ai-make-image-model :id "i" :provider "p1" :api "test-api"
                                            :cost (llm-pi-ai-make-model-cost))))
    (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
    (let ((message (llm-pi-ai-event-stream-result
                    (llm-pi-ai-models-stream
                     models image (llm-pi-ai-models-test--context)))))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Model p1/i is not a chat model")))))

(ert-deftest llm-pi-ai-models-test-api-failure-is-an-error-stream ()
  "An API function that signals ends the stream with an error event."
  (let* ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a"))
         (api (llm-pi-ai-make-provider-streams
               :stream (lambda (&rest _) (error "api exploded"))
               :stream-simple (lambda (&rest _) (error "api exploded")))))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-models-test--provider "p1" :api api))
    (let ((message (llm-pi-ai-event-stream-result
                    (llm-pi-ai-models-stream-simple
                     models model (llm-pi-ai-models-test--context)))))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "api exploded")))))

(ert-deftest llm-pi-ai-models-test-merges-resolved-auth-into-options ()
  "Explicit options win per field; resolved auth fills the rest."
  (llm-pi-ai-models-test--with-calls
   (let* ((auth (llm-pi-ai-make-provider-auth
                 :api-key (llm-pi-ai-make-api-key-auth
                           :name "Test"
                           :resolve (lambda (&rest _)
                                      (list :auth
                                            (list :api-key "resolved-key"
                                                  :headers '(("Authorization" . "Bearer resolved-key")
                                                             ("x-a" . "auth") ("x-b" . "auth"))
                                                  :base-url "https://auth.test/v1"))))))
          (models (llm-pi-ai-create-models))
          (model (llm-pi-ai-models-test--model "p1" "model-a")))
     (llm-pi-ai-models-set-provider
      models (llm-pi-ai-models-test--provider "p1" :auth auth))
     (llm-pi-ai-models-stream-simple
      models model (llm-pi-ai-models-test--context)
      '(:api-key "explicit-key"
        :headers (("authorization" . "Explicit token") ("x-b" . "explicit"))))
     (let ((call (nth 0 llm-pi-ai-models-test--calls)))
       (should (equal (plist-get (nth 2 call) :api-key) "explicit-key"))
       (should (equal (plist-get (nth 2 call) :headers)
                      '(("x-a" . "auth") ("authorization" . "Explicit token")
                        ("x-b" . "explicit"))))
       (should (equal (llm-pi-ai-model-base-url (nth 0 call)) "https://auth.test/v1"))
       ;; the caller's model is not changed
       (should (equal (llm-pi-ai-model-base-url model) "https://example.test/v1")))
     ;; without explicit options, resolved auth applies
     (llm-pi-ai-models-stream-simple models model (llm-pi-ai-models-test--context))
     (should (equal (plist-get (nth 2 (nth 1 llm-pi-ai-models-test--calls)) :api-key)
                    "resolved-key")))))

(ert-deftest llm-pi-ai-models-test-explicit-api-key-and-env ()
  "The request's api key and env take part in auth resolution and are passed on."
  (llm-pi-ai-models-test--with-calls
   (let* ((auth (llm-pi-ai-make-provider-auth
                 :api-key (llm-pi-ai-make-api-key-auth
                           :name "Scoped"
                           :resolve
                           (lambda (ctx credential _signal)
                             (let ((account (or (cdr (assoc "ACCOUNT_ID"
                                                            (plist-get credential :env)))
                                                (llm-pi-ai-auth-context-lookup-env
                                                 ctx "ACCOUNT_ID"))))
                               (when (and (plist-get credential :key) account)
                                 (list :auth (list :api-key (plist-get credential :key)
                                                   :base-url (concat "https://example.test/"
                                                                     account))
                                       :env (list (cons "ACCOUNT_ID" account)))))))))
          (models (llm-pi-ai-create-models))
          (model (llm-pi-ai-models-test--model "p1" "model-a")))
     (llm-pi-ai-models-set-provider
      models (llm-pi-ai-models-test--provider "p1" :auth auth))
     (llm-pi-ai-models-stream-simple
      models model (llm-pi-ai-models-test--context)
      '(:api-key "explicit-key" :env (("ACCOUNT_ID" . "acct"))))
     (let ((call (car llm-pi-ai-models-test--calls)))
       (should (equal (llm-pi-ai-model-base-url (nth 0 call))
                      "https://example.test/acct"))
       (should (equal (plist-get (nth 2 call) :api-key) "explicit-key"))
       (should (equal (plist-get (nth 2 call) :env) '(("ACCOUNT_ID" . "acct"))))))))

(ert-deftest llm-pi-ai-models-test-model-headers-and-transform-headers ()
  "Model headers join the request headers; `:transform-headers' runs once,
last, and is not passed on."
  (llm-pi-ai-models-test--with-calls
   (let* ((models (llm-pi-ai-create-models))
          (model (llm-pi-ai-models-test--model
                  "p1" "model-a"
                  :headers '(("x-model" . "model") ("x-shared" . "model"))))
          (transforms 0))
     (llm-pi-ai-models-set-provider
      models (llm-pi-ai-models-test--provider
              "p1" :auth (llm-pi-ai-models-test--env-key-auth "key")))
     (llm-pi-ai-models-stream-simple
      models model (llm-pi-ai-models-test--context)
      (list :headers '(("x-explicit" . "explicit") ("X-Shared" . "explicit"))
            :transform-headers
            (lambda (headers)
              (cl-incf transforms)
              (should (equal headers '(("x-model" . "model")
                                       ("x-explicit" . "explicit")
                                       ("X-Shared" . "explicit"))))
              (append headers '(("x-transformed" . "yes"))))))
     (should (= transforms 1))
     (let ((options (nth 2 (car llm-pi-ai-models-test--calls))))
       (should (equal (plist-get options :headers)
                      '(("x-model" . "model") ("x-explicit" . "explicit")
                        ("X-Shared" . "explicit") ("x-transformed" . "yes"))))
       (should-not (plist-member options :transform-headers))))))

(ert-deftest llm-pi-ai-models-test-other-options-pass-through ()
  (llm-pi-ai-models-test--with-calls
   (let ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a"))
         (signal (llm-pi-ai-make-abort-signal)))
     (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
     (llm-pi-ai-models-stream-simple
      models model (llm-pi-ai-models-test--context)
      (list :reasoning "high" :signal signal :temperature 0.5))
     (let ((options (nth 2 (car llm-pi-ai-models-test--calls))))
       (should (equal (plist-get options :reasoning) "high"))
       (should (eq (plist-get options :signal) signal))
       (should (= (plist-get options :temperature) 0.5))))))

(ert-deftest llm-pi-ai-models-test-aborted-signal-ends-in-an-error-stream ()
  "A signal aborted before the call stops the request before the API runs."
  (llm-pi-ai-models-test--with-calls
   (let ((models (llm-pi-ai-create-models))
         (model (llm-pi-ai-models-test--model "p1" "model-a"))
         (signal (llm-pi-ai-make-abort-signal)))
     (llm-pi-ai-models-set-provider models (llm-pi-ai-models-test--provider "p1"))
     (llm-pi-ai-abort-signal-abort signal)
     (let ((message (llm-pi-ai-event-stream-result
                     (llm-pi-ai-models-stream-simple
                      models model (llm-pi-ai-models-test--context)
                      (list :signal signal)))))
       (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
       (should (equal (llm-pi-ai-assistant-message-error-message message)
                      "The operation was aborted"))
       (should-not llm-pi-ai-models-test--calls)))))

;;; Lazy streams

(ert-deftest llm-pi-ai-models-test-lazy-stream-forwards-a-filling-stream ()
  "Events pushed after the lazy stream is returned are forwarded as pushed."
  (let* ((inner (llm-pi-ai-create-assistant-message-event-stream))
         (model (llm-pi-ai-models-test--model "p" "m"))
         (outer (llm-pi-ai-lazy-stream model (lambda () inner)))
         (message (llm-pi-ai-models-test--done-message model "late")))
    (should (equal (llm-pi-ai-event-stream-state outer) 'active))
    (llm-pi-ai-event-stream-push
     inner (llm-pi-ai-make-event :type "start" :partial message))
    (llm-pi-ai-event-stream-push
     inner (llm-pi-ai-make-event :type "done" :reason "stop" :message message))
    (llm-pi-ai-event-stream-end inner message)
    (should (equal (llm-pi-ai-models-test--events outer) '("start" "done")))
    (should (eq (llm-pi-ai-event-stream-result outer) message))))

(ert-deftest llm-pi-ai-models-test-lazy-stream-setup-error ()
  (let* ((model (llm-pi-ai-models-test--model "p" "m"))
         (outer (llm-pi-ai-lazy-stream model (lambda () (error "setup failed"))))
         (events (llm-pi-ai-event-stream-to-list outer))
         (message (llm-pi-ai-event-stream-result outer)))
    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-type events)
                   '("error")))
    (should (equal (llm-pi-ai-assistant-message-event-reason (car events)) "error"))
    (should (eq (llm-pi-ai-assistant-message-event-error (car events)) message))
    (should (equal (llm-pi-ai-assistant-message-error-message message)
                   "setup failed"))
    (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
    (should (equal (llm-pi-ai-assistant-message-api message) "test-api"))
    (should (equal (llm-pi-ai-assistant-message-provider message) "p"))
    (should (equal (llm-pi-ai-assistant-message-model message) "m"))
    (should-not (llm-pi-ai-assistant-message-content message))
    (should (= (llm-pi-ai-usage-total-tokens (llm-pi-ai-assistant-message-usage message))
               0))
    (should (= (llm-pi-ai-cost-total
                (llm-pi-ai-usage-cost (llm-pi-ai-assistant-message-usage message)))
               0))))

(ert-deftest llm-pi-ai-models-test-lazy-api-loads-on-use ()
  "`lazy-api' calls LOAD inside each stream; a failing load is an error event."
  (let* ((loads 0)
         (model (llm-pi-ai-models-test--model "p" "m"))
         (context (llm-pi-ai-normalize-context (llm-pi-ai-models-test--context)))
         (api (llm-pi-ai-lazy-api
               (lambda ()
                 (cl-incf loads)
                 (llm-pi-ai-make-provider-streams
                  :stream (lambda (m _c _o)
                            (llm-pi-ai-models-test--canned-stream m))
                  :stream-simple (lambda (m _c _o)
                                   (llm-pi-ai-models-test--canned-stream m)))))))
    (should (= loads 0))
    (should (equal (llm-pi-ai-models-test--events
                    (funcall (llm-pi-ai-provider-streams-stream api)
                             model context nil))
                   '("start" "done")))
    (should (= loads 1)))
  (let* ((model (llm-pi-ai-models-test--model "p" "m"))
         (context (llm-pi-ai-normalize-context (llm-pi-ai-models-test--context)))
         (api (llm-pi-ai-lazy-api (lambda () (error "cannot load"))))
         (stream (funcall (llm-pi-ai-provider-streams-stream-simple api)
                          model context nil)))
    (should (equal (llm-pi-ai-assistant-message-error-message
                    (llm-pi-ai-event-stream-result stream))
                   "cannot load"))))

(provide 'llm-pi-ai-models-test)

;;; llm-pi-ai-models-test.el ends here
