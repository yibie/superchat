;;; superchat-bridge-models-test.el --- Tests for superchat-bridge-models  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-models.el', its port of `src/models.ts'.
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
(require 'superchat-bridge-models)

;;; Helpers

(defun superchat-bridge-models-test--model (provider id &rest args)
  "Return a chat model ID of PROVIDER with ARGS as extra slots."
  (apply #'superchat-bridge-make-model
         (append args
                 (list :id id :name id :api "test-api" :provider provider
                       :base-url "https://example.test/v1" :reasoning nil
                       :input '("text")
                       :cost (superchat-bridge-make-model-cost)
                       :context-window 10000 :max-tokens 1000))))

(defun superchat-bridge-models-test--done-message (model text)
  "Return a stop message of MODEL with TEXT."
  (superchat-bridge-make-assistant-message
   :content (list (superchat-bridge-make-text-content :text text))
   :api (superchat-bridge-model-api model)
   :provider (superchat-bridge-model-provider model)
   :model (superchat-bridge-model-id model)
   :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
   :stop-reason "stop"
   :timestamp (round (* 1000 (float-time)))))

(defun superchat-bridge-models-test--canned-stream (model)
  "Return a complete stream of MODEL: start, done."
  (let ((stream (superchat-bridge-create-assistant-message-event-stream))
        (message (superchat-bridge-models-test--done-message model "ok")))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "start" :partial message))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "done" :reason "stop" :message message))
    (superchat-bridge-event-stream-end stream message)
    stream))

(defun superchat-bridge-models-test--api (calls)
  "Return streams that push a call record (MODEL CONTEXT OPTIONS) onto CALLS.
CALLS is a symbol whose value is the list."
  (let ((respond (lambda (model context options)
                   (set calls (append (symbol-value calls)
                                      (list (list model context options))))
                   (superchat-bridge-models-test--canned-stream model))))
    (superchat-bridge-make-provider-streams :stream respond :stream-simple respond)))

(defvar superchat-bridge-models-test--calls nil "Calls the test API received.")

(defun superchat-bridge-models-test--ambient-auth ()
  "Auth that reports configured with no auth values, like `ambientAuth'."
  (superchat-bridge-make-provider-auth
   :api-key (superchat-bridge-make-api-key-auth
             :name "Ambient"
             :resolve (lambda (_ctx _credential _signal) (list :auth nil)))))

(defun superchat-bridge-models-test--provider (id &rest args)
  "Return a provider ID with the test API; ARGS are `create-provider' keys."
  (apply #'superchat-bridge-create-provider
         (append args
                 (list :id id
                       :auth (superchat-bridge-models-test--ambient-auth)
                       :models (list (superchat-bridge-models-test--model id "model-a"))
                       :api (superchat-bridge-models-test--api
                             'superchat-bridge-models-test--calls)))))

(defun superchat-bridge-models-test--context ()
  "Return a context with one user message."
  (superchat-bridge-make-context
   :messages (list (superchat-bridge-make-user-message :content "hi" :timestamp 1))))

(defun superchat-bridge-models-test--events (stream)
  "Return the event types of the complete STREAM."
  (mapcar #'superchat-bridge-assistant-message-event-type
          (superchat-bridge-event-stream-to-list stream)))

(defun superchat-bridge-models-test--usage (cache-write &optional cache-write1h)
  "Return the usage of the upstream tier test with CACHE-WRITE."
  (superchat-bridge-make-usage :input 200000 :output 100000 :cache-read 72000
                        :cache-write cache-write :cache-write1h cache-write1h
                        :total-tokens (+ 372000 cache-write)
                        :cost (superchat-bridge-make-cost)))

(defmacro superchat-bridge-models-test--with-calls (&rest body)
  "Run BODY with an empty call list."
  `(let ((superchat-bridge-models-test--calls nil)) ,@body))

;;; calculateCost

(defun superchat-bridge-models-test--tiered-model ()
  "The model of upstream's tier test."
  (superchat-bridge-models-test--model
   "openai" "gpt-5.6-sol"
   :cost (superchat-bridge-make-model-cost
          :input 5 :output 30 :cache-read 0.5 :cache-write 6.25
          :tiers (list (superchat-bridge-make-model-cost-tier
                        :input-tokens-above 272000
                        :input 10 :output 45 :cache-read 1 :cache-write 12.5)))))

(ert-deftest superchat-bridge-models-test-cost-below-tier ()
  "372000 input tokens in all is above the tier; 372000 is checked with
cache writes at 0: input+read = 272000, not above 272000."
  (let* ((model (superchat-bridge-models-test--tiered-model))
         (cost (superchat-bridge-calculate-cost
                model (superchat-bridge-models-test--usage 0))))
    (should (= (superchat-bridge-cost-input cost) 1))
    (should (= (superchat-bridge-cost-output cost) 3))
    (should (< (abs (- (superchat-bridge-cost-cache-read cost) 0.036)) 1e-12))
    (should (= (superchat-bridge-cost-cache-write cost) 0))))

(ert-deftest superchat-bridge-models-test-cost-request-wide-tier ()
  "One more token above the threshold moves the whole request to the tier."
  (let* ((model (superchat-bridge-models-test--tiered-model))
         (cost (superchat-bridge-calculate-cost
                model (superchat-bridge-models-test--usage 1))))
    (should (= (superchat-bridge-cost-input cost) 2))
    (should (= (superchat-bridge-cost-output cost) 4.5))
    (should (= (superchat-bridge-cost-cache-read cost) 0.072))
    (should (= (superchat-bridge-cost-cache-write cost) 0.0000125))
    (should (= (superchat-bridge-cost-total cost)
               (+ (superchat-bridge-cost-input cost) (superchat-bridge-cost-output cost)
                  (superchat-bridge-cost-cache-read cost)
                  (superchat-bridge-cost-cache-write cost))))))

(ert-deftest superchat-bridge-models-test-cost-highest-matching-tier ()
  "Of several tiers the highest threshold below the input applies, whatever
their order."
  (let* ((model (superchat-bridge-models-test--model
                 "p" "m"
                 :cost (superchat-bridge-make-model-cost
                        :input 1 :output 1
                        :tiers (list (superchat-bridge-make-model-cost-tier
                                      :input-tokens-above 2000000 :input 30 :output 30)
                                     (superchat-bridge-make-model-cost-tier
                                      :input-tokens-above 1000 :input 20 :output 20)
                                     (superchat-bridge-make-model-cost-tier
                                      :input-tokens-above 100 :input 10 :output 10)))))
         (usage (superchat-bridge-make-usage :input 5000 :output 1000000
                                      :cost (superchat-bridge-make-cost))))
    (superchat-bridge-calculate-cost model usage)
    (should (= (superchat-bridge-cost-input (superchat-bridge-usage-cost usage)) 0.1))
    (should (= (superchat-bridge-cost-output (superchat-bridge-usage-cost usage)) 20))))

(ert-deftest superchat-bridge-models-test-cost-one-hour-cache-write ()
  "Cache writes of one hour cost twice the input rate; the rest the write rate."
  (let* ((model (superchat-bridge-models-test--model
                 "p" "m" :cost (superchat-bridge-make-model-cost
                                :input 3 :output 15 :cache-read 0.3 :cache-write 3.75)))
         (usage (superchat-bridge-make-usage :input 0 :output 0 :cache-write 1000000
                                      :cache-write1h 400000
                                      :cost (superchat-bridge-make-cost)))
         (cost (superchat-bridge-calculate-cost model usage)))
    ;; (3.75 * 600000 + 3 * 2 * 400000) / 1e6
    (should (< (abs (- (superchat-bridge-cost-cache-write cost) 4.65)) 1e-9))
    (should (eq cost (superchat-bridge-usage-cost usage)))))

(ert-deftest superchat-bridge-models-test-cost-integer-rates-are-not-truncated ()
  "A rate of 1 per million tokens times 1 token is a millionth, not 0."
  (let* ((model (superchat-bridge-models-test--model
                 "p" "m" :cost (superchat-bridge-make-model-cost :input 1)))
         (usage (superchat-bridge-make-usage :input 1 :cost (superchat-bridge-make-cost))))
    (superchat-bridge-calculate-cost model usage)
    (should (= (superchat-bridge-cost-input (superchat-bridge-usage-cost usage)) 1e-6))))

;;; Thinking levels

(ert-deftest superchat-bridge-models-test-thinking-levels-no-reasoning ()
  (should (equal (superchat-bridge-get-supported-thinking-levels
                  (superchat-bridge-models-test--model "p" "m"))
                 '("off")))
  (should (equal (superchat-bridge-clamp-thinking-level
                  (superchat-bridge-models-test--model "p" "m") "high")
                 "off")))

(ert-deftest superchat-bridge-models-test-thinking-levels-ordinary-reasoning ()
  "xhigh and max are opt-in (max-thinking.test.ts, ordinary reasoning)."
  (let ((model (superchat-bridge-models-test--model "p" "m" :reasoning t)))
    (should (equal (superchat-bridge-get-supported-thinking-levels model)
                   '("off" "minimal" "low" "medium" "high")))
    (should (equal (superchat-bridge-clamp-thinking-level model "max") "high"))
    (should (equal (superchat-bridge-clamp-thinking-level model "xhigh") "high"))
    (should (equal (superchat-bridge-clamp-thinking-level model "medium") "medium"))))

(ert-deftest superchat-bridge-models-test-thinking-levels-hole-between-high-and-max ()
  "xhigh null, max mapped (max-thinking.test.ts, hole): xhigh clamps up to max."
  (let ((model (superchat-bridge-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("xhigh" . :null) ("max" . "max")))))
    (should (equal (superchat-bridge-get-supported-thinking-levels model)
                   '("off" "minimal" "low" "medium" "high" "max")))
    (should (equal (superchat-bridge-clamp-thinking-level model "xhigh") "max"))))

(ert-deftest superchat-bridge-models-test-thinking-levels-null-removes-level ()
  "A level mapped to null is removed; xhigh and max appear when mapped."
  (let ((model (superchat-bridge-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("off" . :null) ("minimal" . :null)
                                      ("xhigh" . "xhigh") ("max" . "max")))))
    (should (equal (superchat-bridge-get-supported-thinking-levels model)
                   '("low" "medium" "high" "xhigh" "max")))
    ;; off is not supported: the nearest above it is low
    (should (equal (superchat-bridge-clamp-thinking-level model "off") "low"))
    (should (equal (superchat-bridge-clamp-thinking-level model "minimal") "low"))))

(ert-deftest superchat-bridge-models-test-clamp-searches-up-then-down ()
  "A level not supported goes to the next supported one above, else below."
  (let ((model (superchat-bridge-models-test--model
                "p" "m" :reasoning t
                :thinking-level-map '(("minimal" . :null) ("medium" . :null)
                                      ("high" . :null)))))
    (should (equal (superchat-bridge-get-supported-thinking-levels model)
                   '("off" "low")))
    (should (equal (superchat-bridge-clamp-thinking-level model "medium") "low"))
    (should (equal (superchat-bridge-clamp-thinking-level model "high") "low"))
    (should (equal (superchat-bridge-clamp-thinking-level model "max") "low"))
    (should (equal (superchat-bridge-clamp-thinking-level model "minimal") "low"))
    ;; a level that is no level at all gives the first supported one
    (should (equal (superchat-bridge-clamp-thinking-level model "nonsense") "off"))))

;;; hasApi, modelsAreEqual

(ert-deftest superchat-bridge-models-test-has-api ()
  (let ((chat (superchat-bridge-models-test--model "p" "m"))
        (image (superchat-bridge-make-image-model :id "i" :provider "p" :api "test-api")))
    (should (superchat-bridge-has-api chat "test-api"))
    (should-not (superchat-bridge-has-api chat "openai-completions"))
    ;; non-chat models never match, even when their api id equals
    (should-not (superchat-bridge-has-api image "test-api"))))

(ert-deftest superchat-bridge-models-test-models-are-equal ()
  (let ((a (superchat-bridge-models-test--model "p" "m"))
        (same (superchat-bridge-models-test--model "p" "m" :name "other"))
        (other-id (superchat-bridge-models-test--model "p" "n"))
        (other-provider (superchat-bridge-models-test--model "q" "m"))
        (image (superchat-bridge-make-image-model :id "m" :provider "p")))
    (should (superchat-bridge-models-are-equal a same))
    (should-not (superchat-bridge-models-are-equal a other-id))
    (should-not (superchat-bridge-models-are-equal a other-provider))
    ;; same id and provider, different type
    (should-not (superchat-bridge-models-are-equal a image))
    (should-not (superchat-bridge-models-are-equal a nil))
    (should-not (superchat-bridge-models-are-equal nil a))
    (should-not (superchat-bridge-models-are-equal nil nil))))

;;; createProvider

(ert-deftest superchat-bridge-models-test-create-provider-fields ()
  (let ((provider (superchat-bridge-models-test--provider "p1")))
    (should (equal (superchat-bridge-provider-id provider) "p1"))
    ;; name defaults to the id
    (should (equal (superchat-bridge-provider-name provider) "p1"))
    (should (equal (superchat-bridge-provider-name
                    (superchat-bridge-models-test--provider "p1" :name "Pee"))
                   "Pee"))
    (should (equal (mapcar #'superchat-bridge-model-id
                           (funcall (superchat-bridge-provider-get-models provider)))
                   '("model-a")))))

(ert-deftest superchat-bridge-models-test-create-provider-requires-api ()
  (should-error (superchat-bridge-create-provider
                 :id "p" :auth (superchat-bridge-models-test--ambient-auth) :models nil))
  (should-error (superchat-bridge-create-provider
                 :id "p" :auth (superchat-bridge-models-test--ambient-auth)
                 :models nil :api nil))
  ;; an empty api map is rejected too
  (should-error (superchat-bridge-create-provider
                 :id "p" :auth (superchat-bridge-models-test--ambient-auth)
                 :models nil :api '())))

(ert-deftest superchat-bridge-models-test-create-provider-rejects-left-out-keys ()
  "No silent drop of what is not ported."
  (should-error (superchat-bridge-create-provider
                 :id "p" :auth (superchat-bridge-models-test--ambient-auth)
                 :api (superchat-bridge-models-test--api 'superchat-bridge-models-test--calls)
                 :fetch-models #'ignore)))

(ert-deftest superchat-bridge-models-test-get-models-types ()
  "`get-models' is the chat models; `get-all-models' every type."
  (let* ((chat (superchat-bridge-models-test--model "p" "c"))
         (image (superchat-bridge-make-image-model :id "i" :provider "p"))
         (provider (superchat-bridge-models-test--provider
                    "p" :models (list chat image))))
    (should (equal (funcall (superchat-bridge-provider-get-models provider)) (list chat)))
    (should (equal (funcall (superchat-bridge-provider-get-all-models provider))
                   (list chat image)))))

(ert-deftest superchat-bridge-models-test-create-provider-dispatch-on-api ()
  "With an api map, a model goes to the implementation of its `api'; a model
whose api has none gets an error stream."
  (let* ((seen nil)
         (make (lambda (label)
                 (let ((respond (lambda (model _context _options)
                                  (push label seen)
                                  (superchat-bridge-models-test--canned-stream model))))
                   (superchat-bridge-make-provider-streams :stream respond
                                                    :stream-simple respond))))
         (provider (superchat-bridge-create-provider
                    :id "p" :auth (superchat-bridge-models-test--ambient-auth)
                    :models nil
                    :api (list (cons "api-a" (funcall make 'a))
                               (cons "api-b" (funcall make 'b)))))
         (context (superchat-bridge-normalize-context (superchat-bridge-models-test--context))))
    (funcall (superchat-bridge-provider-stream provider)
             (superchat-bridge-models-test--model "p" "m" :api "api-b") context nil)
    (funcall (superchat-bridge-provider-stream-simple provider)
             (superchat-bridge-models-test--model "p" "m" :api "api-a") context nil)
    (should (equal (nreverse seen) '(b a)))
    (let* ((stream (funcall (superchat-bridge-provider-stream provider)
                            (superchat-bridge-models-test--model "p" "m" :api "api-c")
                            context nil))
           (events (superchat-bridge-event-stream-to-list stream))
           (message (superchat-bridge-event-stream-result stream)))
      (should (equal (mapcar #'superchat-bridge-assistant-message-event-type events)
                     '("error")))
      (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     "Provider p has no API implementation for \"api-c\"")))))

;;; Models collection

(ert-deftest superchat-bridge-models-test-registers-replaces-and-deletes-providers ()
  (let ((models (superchat-bridge-create-models)))
    (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
    (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p2"))
    (should (equal (mapcar #'superchat-bridge-provider-id
                           (superchat-bridge-models-get-providers models))
                   '("p1" "p2")))
    ;; replacing keeps the place, and there is still one p1
    (let ((replacement (superchat-bridge-models-test--provider "p1")))
      (superchat-bridge-models-set-provider models replacement)
      (should (eq (superchat-bridge-models-get-provider models "p1") replacement))
      (should (equal (mapcar #'superchat-bridge-provider-id
                             (superchat-bridge-models-get-providers models))
                     '("p1" "p2"))))
    (superchat-bridge-models-delete-provider models "p1")
    (should-not (superchat-bridge-models-get-provider models "p1"))
    (superchat-bridge-models-clear-providers models)
    (should-not (superchat-bridge-models-get-providers models))))

(ert-deftest superchat-bridge-models-test-lists-and-finds-models ()
  (let ((models (superchat-bridge-create-models)))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "p1" :models (list (superchat-bridge-models-test--model "p1" "m1")
                                (superchat-bridge-models-test--model "p1" "m2"))))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "p2" :models (list (superchat-bridge-models-test--model "p2" "m3"))))
    (let ((ids (lambda (list) (mapcar #'superchat-bridge-model-id list))))
      (should (equal (funcall ids (superchat-bridge-models-get-models models))
                     '("m1" "m2" "m3")))
      (should (equal (funcall ids (superchat-bridge-models-get-models models "p1"))
                     '("m1" "m2")))
      (should-not (superchat-bridge-models-get-models models "nope")))
    (should (equal (superchat-bridge-model-id (superchat-bridge-models-get-model models "p2" "m3"))
                   "m3"))
    (should-not (superchat-bridge-models-get-model models "p2" "missing"))
    (should-not (superchat-bridge-models-get-model models "nope" "m3"))
    (let ((found (superchat-bridge-models-get-model models "p2" "m3")))
      (should-not (superchat-bridge-has-api found "openai-completions"))
      (should (superchat-bridge-has-api found "test-api")))))

(ert-deftest superchat-bridge-models-test-swallows-provider-source-failures ()
  "A provider whose `get-models' fails yields no models, in either listing."
  (let ((models (superchat-bridge-create-models))
        (broken (superchat-bridge-models-test--provider "broken")))
    (setf (superchat-bridge-provider-get-models broken) (lambda () (error "boom")))
    (superchat-bridge-models-set-provider models broken)
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "ok" :models (list (superchat-bridge-models-test--model "ok" "m1"))))
    (should (equal (mapcar #'superchat-bridge-model-id (superchat-bridge-models-get-models models))
                   '("m1")))
    (should-not (superchat-bridge-models-get-models models "broken"))
    ;; precise failures come from the provider directly
    (should-error (funcall (superchat-bridge-provider-get-models broken)))))

;;; getAuth

(defun superchat-bridge-models-test--env-key-auth (key)
  "Provider auth like upstream's `envKeyAuth': stored key, else KEY."
  (superchat-bridge-make-provider-auth
   :api-key (superchat-bridge-make-api-key-auth
             :name "Test API key"
             :resolve (lambda (_ctx credential _signal)
                        (let ((resolved (or (plist-get credential :key) key)))
                          (when resolved
                            (list :auth (list :api-key resolved)
                                  :source (if credential "stored" "env"))))))))

(ert-deftest superchat-bridge-models-test-get-auth-stored-credential-owns-provider ()
  (let* ((credentials (superchat-bridge-make-in-memory-credential-store))
         (models (superchat-bridge-create-models :credentials credentials))
         (model (superchat-bridge-models-test--model "p1" "model-a")))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "p1" :auth (superchat-bridge-models-test--env-key-auth "env-key")))
    (let ((api-key (lambda (result) (plist-get (plist-get result :auth) :api-key))))
      ;; model and provider-id forms resolve the same provider-scoped auth
      (should (equal (funcall api-key (superchat-bridge-models-get-auth models model))
                     "env-key"))
      (should (equal (funcall api-key (superchat-bridge-models-get-auth models "p1"))
                     "env-key"))
      (should (equal (funcall api-key (superchat-bridge-models-get-auth
                                       models model '(:api-key "explicit-key")))
                     "explicit-key"))
      ;; stored api-key credential beats the environment
      (funcall (superchat-bridge-credential-store-modify credentials)
               "p1" (lambda (_c) '(:type api_key :key "stored-key")) nil)
      (let ((result (superchat-bridge-models-get-auth models "p1")))
        (should (equal (funcall api-key result) "stored-key"))
        (should (equal (plist-get result :source) "stored"))))))

(ert-deftest superchat-bridge-models-test-get-auth-unknown-provider ()
  (should-not (superchat-bridge-models-get-auth (superchat-bridge-create-models) "ghost")))

(ert-deftest superchat-bridge-models-test-get-auth-model-headers ()
  "Model headers join the auth only when asked with a model."
  (let ((models (superchat-bridge-create-models))
        (model (superchat-bridge-models-test--model
                "p1" "model-a" :headers '(("x-model" . "model") ("x-shared" . "model")))))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "p1" :auth (superchat-bridge-models-test--env-key-auth "key")))
    (should-not (plist-get (plist-get (superchat-bridge-models-get-auth models "p1") :auth)
                           :headers))
    (should (equal (plist-get (plist-get (superchat-bridge-models-get-auth models model)
                                         :auth)
                              :headers)
                   '(("x-model" . "model") ("x-shared" . "model"))))))

;;; stream

(ert-deftest superchat-bridge-models-test-streams-through-the-provider ()
  (superchat-bridge-models-test--with-calls
   (let ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a")))
     (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
     (let ((stream (superchat-bridge-models-stream-simple
                    models model (superchat-bridge-models-test--context))))
       (should (equal (superchat-bridge-models-test--events stream) '("start" "done")))
       (should (equal (superchat-bridge-assistant-message-stop-reason
                       (superchat-bridge-event-stream-result stream))
                      "stop"))))))

(ert-deftest superchat-bridge-models-test-stream-and-stream-simple-pick-the-function ()
  (let* ((which nil)
         (api (superchat-bridge-make-provider-streams
               :stream (lambda (model _c _o)
                         (push 'stream which)
                         (superchat-bridge-models-test--canned-stream model))
               :stream-simple (lambda (model _c _o)
                                (push 'stream-simple which)
                                (superchat-bridge-models-test--canned-stream model))))
         (models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a")))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider "p1" :api api))
    (superchat-bridge-models-stream models model (superchat-bridge-models-test--context))
    (superchat-bridge-models-stream-simple models model (superchat-bridge-models-test--context))
    (should (equal (nreverse which) '(stream stream-simple)))))

(ert-deftest superchat-bridge-models-test-stream-normalizes-the-context ()
  "The provider gets a transcript: the system prompt is a leading message."
  (superchat-bridge-models-test--with-calls
   (let ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a")))
     (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
     (superchat-bridge-models-stream-simple
      models model (superchat-bridge-make-context
                    :system-prompt "sys"
                    :messages (list (superchat-bridge-make-user-message
                                     :content "hi" :timestamp 1))))
     (let ((context (nth 1 (car superchat-bridge-models-test--calls))))
       (should-not (superchat-bridge-context-system-prompt context))
       (should (equal (mapcar #'superchat-bridge-message-role
                              (superchat-bridge-context-messages context))
                      '("system" "user")))))))

(ert-deftest superchat-bridge-models-test-unknown-provider-is-an-error-stream ()
  "No signal to the caller: the stream's error event says what is wrong."
  (let* ((models (superchat-bridge-create-models))
         (stream (superchat-bridge-models-stream-simple
                  models (superchat-bridge-models-test--model "ghost" "model-a")
                  (superchat-bridge-models-test--context)))
         (events (superchat-bridge-event-stream-to-list stream))
         (message (superchat-bridge-event-stream-result stream)))
    (should (equal (mapcar #'superchat-bridge-assistant-message-event-type events)
                   '("error")))
    (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
    (should (string-search "Unknown provider: ghost"
                           (superchat-bridge-assistant-message-error-message message)))
    (should (equal (superchat-bridge-assistant-message-provider message) "ghost"))
    (should (equal (superchat-bridge-assistant-message-model message) "model-a"))))

(ert-deftest superchat-bridge-models-test-provider-not-configured ()
  (let* ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a")))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider
             "p1" :auth (superchat-bridge-models-test--env-key-auth nil)))
    (let ((message (superchat-bridge-event-stream-result
                    (superchat-bridge-models-stream-simple
                     models model (superchat-bridge-models-test--context)))))
      (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     "Provider is not configured: p1")))))

(ert-deftest superchat-bridge-models-test-non-chat-model-is-an-error-stream ()
  (let* ((models (superchat-bridge-create-models))
         (image (superchat-bridge-make-image-model :id "i" :provider "p1" :api "test-api"
                                            :cost (superchat-bridge-make-model-cost))))
    (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
    (let ((message (superchat-bridge-event-stream-result
                    (superchat-bridge-models-stream
                     models image (superchat-bridge-models-test--context)))))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     "Model p1/i is not a chat model")))))

(ert-deftest superchat-bridge-models-test-api-failure-is-an-error-stream ()
  "An API function that signals ends the stream with an error event."
  (let* ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a"))
         (api (superchat-bridge-make-provider-streams
               :stream (lambda (&rest _) (error "api exploded"))
               :stream-simple (lambda (&rest _) (error "api exploded")))))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-models-test--provider "p1" :api api))
    (let ((message (superchat-bridge-event-stream-result
                    (superchat-bridge-models-stream-simple
                     models model (superchat-bridge-models-test--context)))))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     "api exploded")))))

(ert-deftest superchat-bridge-models-test-merges-resolved-auth-into-options ()
  "Explicit options win per field; resolved auth fills the rest."
  (superchat-bridge-models-test--with-calls
   (let* ((auth (superchat-bridge-make-provider-auth
                 :api-key (superchat-bridge-make-api-key-auth
                           :name "Test"
                           :resolve (lambda (&rest _)
                                      (list :auth
                                            (list :api-key "resolved-key"
                                                  :headers '(("Authorization" . "Bearer resolved-key")
                                                             ("x-a" . "auth") ("x-b" . "auth"))
                                                  :base-url "https://auth.test/v1"))))))
          (models (superchat-bridge-create-models))
          (model (superchat-bridge-models-test--model "p1" "model-a")))
     (superchat-bridge-models-set-provider
      models (superchat-bridge-models-test--provider "p1" :auth auth))
     (superchat-bridge-models-stream-simple
      models model (superchat-bridge-models-test--context)
      '(:api-key "explicit-key"
        :headers (("authorization" . "Explicit token") ("x-b" . "explicit"))))
     (let ((call (nth 0 superchat-bridge-models-test--calls)))
       (should (equal (plist-get (nth 2 call) :api-key) "explicit-key"))
       (should (equal (plist-get (nth 2 call) :headers)
                      '(("x-a" . "auth") ("authorization" . "Explicit token")
                        ("x-b" . "explicit"))))
       (should (equal (superchat-bridge-model-base-url (nth 0 call)) "https://auth.test/v1"))
       ;; the caller's model is not changed
       (should (equal (superchat-bridge-model-base-url model) "https://example.test/v1")))
     ;; without explicit options, resolved auth applies
     (superchat-bridge-models-stream-simple models model (superchat-bridge-models-test--context))
     (should (equal (plist-get (nth 2 (nth 1 superchat-bridge-models-test--calls)) :api-key)
                    "resolved-key")))))

(ert-deftest superchat-bridge-models-test-explicit-api-key-and-env ()
  "The request's api key and env take part in auth resolution and are passed on."
  (superchat-bridge-models-test--with-calls
   (let* ((auth (superchat-bridge-make-provider-auth
                 :api-key (superchat-bridge-make-api-key-auth
                           :name "Scoped"
                           :resolve
                           (lambda (ctx credential _signal)
                             (let ((account (or (cdr (assoc "ACCOUNT_ID"
                                                            (plist-get credential :env)))
                                                (superchat-bridge-auth-context-lookup-env
                                                 ctx "ACCOUNT_ID"))))
                               (when (and (plist-get credential :key) account)
                                 (list :auth (list :api-key (plist-get credential :key)
                                                   :base-url (concat "https://example.test/"
                                                                     account))
                                       :env (list (cons "ACCOUNT_ID" account)))))))))
          (models (superchat-bridge-create-models))
          (model (superchat-bridge-models-test--model "p1" "model-a")))
     (superchat-bridge-models-set-provider
      models (superchat-bridge-models-test--provider "p1" :auth auth))
     (superchat-bridge-models-stream-simple
      models model (superchat-bridge-models-test--context)
      '(:api-key "explicit-key" :env (("ACCOUNT_ID" . "acct"))))
     (let ((call (car superchat-bridge-models-test--calls)))
       (should (equal (superchat-bridge-model-base-url (nth 0 call))
                      "https://example.test/acct"))
       (should (equal (plist-get (nth 2 call) :api-key) "explicit-key"))
       (should (equal (plist-get (nth 2 call) :env) '(("ACCOUNT_ID" . "acct"))))))))

(ert-deftest superchat-bridge-models-test-model-headers-and-transform-headers ()
  "Model headers join the request headers; `:transform-headers' runs once,
last, and is not passed on."
  (superchat-bridge-models-test--with-calls
   (let* ((models (superchat-bridge-create-models))
          (model (superchat-bridge-models-test--model
                  "p1" "model-a"
                  :headers '(("x-model" . "model") ("x-shared" . "model"))))
          (transforms 0))
     (superchat-bridge-models-set-provider
      models (superchat-bridge-models-test--provider
              "p1" :auth (superchat-bridge-models-test--env-key-auth "key")))
     (superchat-bridge-models-stream-simple
      models model (superchat-bridge-models-test--context)
      (list :headers '(("x-explicit" . "explicit") ("X-Shared" . "explicit"))
            :transform-headers
            (lambda (headers)
              (cl-incf transforms)
              (should (equal headers '(("x-model" . "model")
                                       ("x-explicit" . "explicit")
                                       ("X-Shared" . "explicit"))))
              (append headers '(("x-transformed" . "yes"))))))
     (should (= transforms 1))
     (let ((options (nth 2 (car superchat-bridge-models-test--calls))))
       (should (equal (plist-get options :headers)
                      '(("x-model" . "model") ("x-explicit" . "explicit")
                        ("X-Shared" . "explicit") ("x-transformed" . "yes"))))
       (should-not (plist-member options :transform-headers))))))

(ert-deftest superchat-bridge-models-test-other-options-pass-through ()
  (superchat-bridge-models-test--with-calls
   (let ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a"))
         (signal (superchat-bridge-make-abort-signal)))
     (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
     (superchat-bridge-models-stream-simple
      models model (superchat-bridge-models-test--context)
      (list :reasoning "high" :signal signal :temperature 0.5))
     (let ((options (nth 2 (car superchat-bridge-models-test--calls))))
       (should (equal (plist-get options :reasoning) "high"))
       (should (eq (plist-get options :signal) signal))
       (should (= (plist-get options :temperature) 0.5))))))

(ert-deftest superchat-bridge-models-test-aborted-signal-ends-in-an-error-stream ()
  "A signal aborted before the call stops the request before the API runs."
  (superchat-bridge-models-test--with-calls
   (let ((models (superchat-bridge-create-models))
         (model (superchat-bridge-models-test--model "p1" "model-a"))
         (signal (superchat-bridge-make-abort-signal)))
     (superchat-bridge-models-set-provider models (superchat-bridge-models-test--provider "p1"))
     (superchat-bridge-abort-signal-abort signal)
     (let ((message (superchat-bridge-event-stream-result
                     (superchat-bridge-models-stream-simple
                      models model (superchat-bridge-models-test--context)
                      (list :signal signal)))))
       (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
       (should (equal (superchat-bridge-assistant-message-error-message message)
                      "The operation was aborted"))
       (should-not superchat-bridge-models-test--calls)))))

;;; Lazy streams

(ert-deftest superchat-bridge-models-test-lazy-stream-forwards-a-filling-stream ()
  "Events pushed after the lazy stream is returned are forwarded as pushed."
  (let* ((inner (superchat-bridge-create-assistant-message-event-stream))
         (model (superchat-bridge-models-test--model "p" "m"))
         (outer (superchat-bridge-lazy-stream model (lambda () inner)))
         (message (superchat-bridge-models-test--done-message model "late")))
    (should (equal (superchat-bridge-event-stream-state outer) 'active))
    (superchat-bridge-event-stream-push
     inner (superchat-bridge-make-event :type "start" :partial message))
    (superchat-bridge-event-stream-push
     inner (superchat-bridge-make-event :type "done" :reason "stop" :message message))
    (superchat-bridge-event-stream-end inner message)
    (should (equal (superchat-bridge-models-test--events outer) '("start" "done")))
    (should (eq (superchat-bridge-event-stream-result outer) message))))

(ert-deftest superchat-bridge-models-test-lazy-stream-setup-error ()
  (let* ((model (superchat-bridge-models-test--model "p" "m"))
         (outer (superchat-bridge-lazy-stream model (lambda () (error "setup failed"))))
         (events (superchat-bridge-event-stream-to-list outer))
         (message (superchat-bridge-event-stream-result outer)))
    (should (equal (mapcar #'superchat-bridge-assistant-message-event-type events)
                   '("error")))
    (should (equal (superchat-bridge-assistant-message-event-reason (car events)) "error"))
    (should (eq (superchat-bridge-assistant-message-event-error (car events)) message))
    (should (equal (superchat-bridge-assistant-message-error-message message)
                   "setup failed"))
    (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
    (should (equal (superchat-bridge-assistant-message-api message) "test-api"))
    (should (equal (superchat-bridge-assistant-message-provider message) "p"))
    (should (equal (superchat-bridge-assistant-message-model message) "m"))
    (should-not (superchat-bridge-assistant-message-content message))
    (should (= (superchat-bridge-usage-total-tokens (superchat-bridge-assistant-message-usage message))
               0))
    (should (= (superchat-bridge-cost-total
                (superchat-bridge-usage-cost (superchat-bridge-assistant-message-usage message)))
               0))))

(ert-deftest superchat-bridge-models-test-lazy-api-loads-on-use ()
  "`lazy-api' calls LOAD inside each stream; a failing load is an error event."
  (let* ((loads 0)
         (model (superchat-bridge-models-test--model "p" "m"))
         (context (superchat-bridge-normalize-context (superchat-bridge-models-test--context)))
         (api (superchat-bridge-lazy-api
               (lambda ()
                 (cl-incf loads)
                 (superchat-bridge-make-provider-streams
                  :stream (lambda (m _c _o)
                            (superchat-bridge-models-test--canned-stream m))
                  :stream-simple (lambda (m _c _o)
                                   (superchat-bridge-models-test--canned-stream m)))))))
    (should (= loads 0))
    (should (equal (superchat-bridge-models-test--events
                    (funcall (superchat-bridge-provider-streams-stream api)
                             model context nil))
                   '("start" "done")))
    (should (= loads 1)))
  (let* ((model (superchat-bridge-models-test--model "p" "m"))
         (context (superchat-bridge-normalize-context (superchat-bridge-models-test--context)))
         (api (superchat-bridge-lazy-api (lambda () (error "cannot load"))))
         (stream (funcall (superchat-bridge-provider-streams-stream-simple api)
                          model context nil)))
    (should (equal (superchat-bridge-assistant-message-error-message
                    (superchat-bridge-event-stream-result stream))
                   "cannot load"))))

(provide 'superchat-bridge-models-test)

;;; superchat-bridge-models-test.el ends here
