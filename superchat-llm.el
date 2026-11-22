;;; superchat-llm.el --- The only file that knows the bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Neutral messages in, one model response out.  This file never runs a tool:
;; a tool call the model asks for goes back to the caller in ON-DONE.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-bridge)
(require 'superchat-bridge-provider-anthropic)
(require 'superchat-bridge-provider-deepseek)
(require 'superchat-bridge-providers)
(require 'superchat-bridge-utils-abort)
(require 'superchat-bridge-utils-retry)

(defvar superchat-llm-providers
  (append '(("deepseek" . superchat-bridge-deepseek-provider)
            ("anthropic" . superchat-bridge-anthropic-provider))
          (mapcar (lambda (id) (cons id (apply-partially #'superchat-bridge-provider id)))
                  (superchat-bridge-provider-ids)))
  "Alist from a provider name to a function of no arguments that returns a
bridge provider.  The name is the provider's id.  A plugin may add to it.")

(defvar superchat-llm-request-function #'superchat-llm--request
  "What `superchat-llm-request' calls.  Tests bind it to a scripted function.")

(defvar superchat-llm--models nil
  "(PROVIDERS . MODELS): the collection made from that value of
`superchat-llm-providers'.")

(defvar superchat-llm--kept nil
  "((CALLS . MESSAGE) ...), newest first: the bridge assistant message of a
response that made calls, by the (ID . NAME) of each call.  Memory only: the
Tape keeps no reasoning, but some providers want it sent back with the calls
it led to until the turn is answered.  At most 64 are kept.")

(defvar superchat-llm--failed nil
  "((NAME . MESSAGE) ...): entries of `superchat-llm-providers' whose function
signalled when the models collection was last built.")

(defun superchat-llm--call-key (calls)
  "What CALLS, a list of (:id :name ...), are kept under."
  (mapcar (lambda (call) (cons (plist-get call :id) (plist-get call :name))) calls))

(defun superchat-llm--models (&optional retry)
  "The bridge models collection, rebuilt when `superchat-llm-providers' changed,
or with RETRY when an entry signalled when it was built, so that a fixed cause
is seen.  Each public function gives RETRY once, at its start: the model data
is there whenever it is asked for, and a broken entry does not rebuild the
collection at every look into it within one call."
  (let ((old (car superchat-llm--models)))
    (unless (and (not (and retry superchat-llm--failed))
                 (= (length old) (length superchat-llm-providers))
                 (cl-every (lambda (a b) (and (equal (car a) (car b))
                                              (eq (cdr a) (cdr b))))
                           old superchat-llm-providers))
      (setq superchat-llm--failed nil)
      (let ((models (superchat-bridge-create-models)) (seen nil))
        (dolist (entry superchat-llm-providers)
          ;; The first entry of a name counts.
          (unless (member (car entry) seen)
            (push (car entry) seen)
            ;; One broken entry must not take the others down.
            (condition-case err
                (superchat-bridge-models-set-provider models (funcall (cdr entry)))
              (error (push (cons (car entry) (error-message-string err))
                           superchat-llm--failed)))))
        ;; Copy the entries, not only the list: `setcdr' on one is seen.
        (setq superchat-llm--models
              (cons (mapcar (lambda (e) (cons (car e) (cdr e)))
                            superchat-llm-providers)
                    models)))))
  (cdr superchat-llm--models))

(defun superchat-llm-model-ids ()
  "The model ids of `superchat-llm-provider'.
A `user-error' says what to set when it is unset or unknown."
  (superchat-llm--models t)
  (superchat-llm--ids))

(defun superchat-llm--ids ()
  "The model ids of `superchat-llm-provider', from the collection as it is."
  (let ((names (mapcar #'car superchat-llm-providers)))
    (unless (member superchat-llm-provider names)
      (user-error "%s; set `superchat-llm-provider' to one of: %s, for example (setq superchat-llm-provider %S)"
                  (if superchat-llm-provider
                      (format "Unknown LLM provider %S" superchat-llm-provider)
                    "No LLM provider")
                  (string-join names ", ") (or (car names) "name"))))
  (let ((models (superchat-llm--models)))
    (when-let* ((bad (assoc superchat-llm-provider superchat-llm--failed)))
      (user-error "The entry %S of `superchat-llm-providers' failed: %s" (car bad) (cdr bad)))
    (mapcar #'superchat-bridge-model-id
            (superchat-bridge-models-get-models models superchat-llm-provider))))

(defun superchat-llm--find-model (id)
  "The bridge model ID of `superchat-llm-provider'.
A `user-error' says what to set when either is unset or unknown."
  (let ((known (superchat-llm--ids)))
    (or (superchat-bridge-models-get-model (superchat-llm--models)
                                           superchat-llm-provider id)
        (user-error "%s; set `superchat-llm-model' to one of: %s"
                    (if id (format "Unknown model %S of %s" id superchat-llm-provider)
                      (format "No model for %s" superchat-llm-provider))
                    (string-join known ", ")))))

(defun superchat-llm-context-window (&optional model)
  "The context window in tokens of MODEL of `superchat-llm-provider'.
MODEL defaults to `superchat-llm-model'.  Nil when the data has none, or
the provider or model is unset or unknown: the request reports those."
  (superchat-llm--models t)
  (let ((id (or model superchat-llm-model)))
    (when (and (stringp superchat-llm-provider) (stringp id))
      (let ((window (when-let* ((m (superchat-bridge-models-get-model
                                    (superchat-llm--models) superchat-llm-provider id)))
                      (superchat-bridge-model-context-window m))))
        (and (numberp window) (> window 0) window)))))

(defun superchat-llm-model-info (model)
  "What MODEL of `superchat-llm-provider' offers, a plist, or nil when unknown.
:context is its window in tokens, :input-cost US dollars per million input
tokens, :images non-nil when it takes images; each is nil when unknown."
  (superchat-llm--models t)
  (when-let* (((stringp superchat-llm-provider))
              (m (superchat-bridge-models-get-model
                  (superchat-llm--models) superchat-llm-provider model)))
    (let ((cost (superchat-bridge-model-cost m)))
      (list :context (superchat-bridge-model-context-window m)
            :input-cost (and cost (superchat-bridge-model-cost-rates-input cost))
            :images (and (member "image" (superchat-bridge-model-input m)) t)))))

(defun superchat-llm--now ()
  "Milliseconds since the epoch."
  (floor (* 1000 (float-time))))

(defun superchat-llm--tool (spec)
  "The bridge tool for SPEC, a plist (:name :description :args).
The parameters are a JSON-schema object; nil is `{}' to the bridge."
  (let ((args (plist-get spec :args)))
    (superchat-bridge-make-tool
     :name (plist-get spec :name)
     :description (or (plist-get spec :description) "")
     :parameters
     (append
      `((type . "object")
        (properties
         . ,(mapcar (lambda (arg)
                      `(,(intern (plist-get arg :name))
                        (type . ,(format "%s" (plist-get arg :type)))
                        (description . ,(or (plist-get arg :description) ""))))
                    args)))
      (when-let* ((required (seq-filter (lambda (arg) (plist-get arg :required)) args)))
        `((required . ,(vconcat (mapcar (lambda (arg) (plist-get arg :name))
                                        required)))))))))

(defun superchat-llm--args (arguments)
  "ARGUMENTS as the bridge holds them as a plist with keyword keys.
The bridge gives a hash table (or an alist) with arrays as vectors.  Objects
nest as plists, arrays as lists; JSON false and null are nil."
  (cond ((hash-table-p arguments)
         (cl-loop for key being the hash-keys of arguments using (hash-values value)
                  nconc (list (intern (concat ":" key)) (superchat-llm--args value))))
        ((consp arguments)
         (cl-loop for (key . value) in arguments
                  nconc (list (intern (format ":%s" key)) (superchat-llm--args value))))
        ((vectorp arguments) (mapcar #'superchat-llm--args arguments))
        ((memq arguments '(:false :null)) nil)
        (t arguments)))

(defun superchat-llm--json (value &optional top)
  "VALUE, call arguments as the Tape holds them, as the bridge wants JSON data.
A plist is an object, any other list an array.  The Tape has nil for false,
null and anything empty alike: nil is an empty object at TOP, else false."
  (cond ((and (consp value) (keywordp (car value)))
         (let ((table (make-hash-table :test #'equal)))
           (cl-loop for (key item) on value by #'cddr
                    do (puthash (substring (symbol-name key) 1)
                                (superchat-llm--json item) table))
           table))
        ((consp value) (vconcat (mapcar #'superchat-llm--json value)))
        ((null value) (if top (make-hash-table :test #'equal) :false))
        (t value)))

(defun superchat-llm--assistant (message model)
  "The bridge assistant message that replays assistant MESSAGE for MODEL.
The message kept for exactly its calls is that message itself.  Otherwise
one is built with the fields `superchat-bridge-transform-messages' and the
completions API read: api, provider and model of MODEL (so it counts as
MODEL's own), zero usage, stop reason \"toolUse\" or \"stop\", a timestamp."
  (let ((calls (plist-get message :calls)))
    (or (and calls (cdr (assoc (superchat-llm--call-key calls) superchat-llm--kept)))
        (superchat-bridge-make-assistant-message
         :content
         (append
          (unless (string-empty-p (or (plist-get message :text) ""))
            (list (superchat-bridge-make-text-content :text (plist-get message :text))))
          (mapcar (lambda (call)
                    (superchat-bridge-make-tool-call :id (plist-get call :id)
                                              :name (plist-get call :name)
                                              :arguments (superchat-llm--json
                                                          (plist-get call :args) t)))
                  calls))
         :api (superchat-bridge-model-api model)
         :provider (superchat-bridge-model-provider model)
         :model (superchat-bridge-model-id model)
         :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
         :stop-reason (if calls "toolUse" "stop")
         :timestamp (superchat-llm--now)))))

(defconst superchat-llm--unsendable
  (rx (any (#xd800 . #xdfff) (#x3fff80 . #x3fffff)))
  "A character a request cannot carry as JSON: a lone surrogate, or a raw
byte that was never decoded (text from a buffer of broken bytes).")

(defun superchat-llm--clean (text)
  "TEXT with each character a request cannot carry replaced by U+FFFD."
  (if (and text (string-match-p superchat-llm--unsendable text))
      (replace-regexp-in-string superchat-llm--unsendable "\ufffd" text t t)
    text))

(defun superchat-llm--context (messages system tools model)
  "The bridge context that replays MESSAGES for MODEL, with SYSTEM and TOOLS."
  ;; A new user turn: no provider wants the reasoning of this conversation's
  ;; earlier turns.  Another conversation's are not in MESSAGES and stay.
  (when (eq (plist-get (car (last messages)) :role) 'user)
    (dolist (message messages)
      (when-let* ((calls (plist-get message :calls)))
        (setq superchat-llm--kept
              (assoc-delete-all (superchat-llm--call-key calls)
                                superchat-llm--kept)))))
  (superchat-bridge-make-context
   :system-prompt (superchat-llm--clean system)
   :tools (mapcar #'superchat-llm--tool tools)
   :messages
   (cl-loop for message in messages
            for text = (superchat-llm--clean (plist-get message :text))
            for role = (plist-get message :role)
            for built = (pcase role
                          ('user (superchat-bridge-make-user-message
                                  :content text :timestamp (superchat-llm--now)))
                          ('assistant
                           (unless (and (string-empty-p (or text ""))
                                        (null (plist-get message :calls)))
                             (superchat-llm--assistant message model)))
                          ('tool (superchat-bridge-make-tool-result-message
                                  :tool-call-id (plist-get message :call-id)
                                  :tool-name (plist-get message :name)
                                  :content (list (superchat-bridge-make-text-content :text text))
                                  :timestamp (superchat-llm--now)))
                          (_ (error "Unknown message role %S" role)))
            when built collect built)))

(defconst superchat-llm--markup "<｜｜"
  "Where a provider's own call markup written as text starts: DeepSeek, with
no tool to call, writes `<｜｜DSML｜｜ invoke name=\"read\">'.  No answer
carries it.")

(defun superchat-llm--markup-in (message)
  "The place in MESSAGE where the provider wrote its call markup as text."
  (string-match-p (regexp-quote superchat-llm--markup)
                  (mapconcat #'superchat-bridge-text-content-text
                             (cl-remove-if-not #'superchat-bridge-text-content-p
                                               (superchat-bridge-message-content message))
                             "")))

(defun superchat-llm--result (message messages)
  "The ON-DONE plist for the final assistant MESSAGE, the answer to MESSAGES;
keeps it if it made calls."
  (let* ((blocks (superchat-bridge-message-content message))
         (text (mapconcat #'superchat-bridge-text-content-text
                          (cl-remove-if-not #'superchat-bridge-text-content-p blocks) ""))
         ;; A provider's call markup is not a sentence: the answer ends there.
         (markup (string-match (regexp-quote superchat-llm--markup) text))
         ;; A provider may leave the id out, or count from call_0 again in
         ;; each response or even within one; a result needs an id, and a
         ;; kept message one that no message it could be replayed for has.
         (taken (append (cl-loop for m in messages
                                 append (mapcar (lambda (c) (plist-get c :id))
                                                (plist-get m :calls)))
                        (cl-loop for (key) in superchat-llm--kept
                                 append (mapcar #'car key))))
         (calls (cl-loop for block in blocks
                         when (superchat-bridge-tool-call-p block)
                         do (let ((id (or (superchat-bridge-tool-call-id block) "")))
                              (when (or (string-empty-p id) (member id taken))
                                (setf (superchat-bridge-tool-call-id block)
                                      (make-temp-name "call_")))
                              (push (superchat-bridge-tool-call-id block) taken))
                         and collect (list :id (superchat-bridge-tool-call-id block)
                                       :name (superchat-bridge-tool-call-name block)
                                       :args (unless (superchat-bridge-tool-call-arguments-error block)
                                               (superchat-llm--args
                                                (superchat-bridge-tool-call-arguments block)))))))
    (when calls
      (setq superchat-llm--kept
            (ntake 64 (cons (cons (superchat-llm--call-key calls) message)
                            superchat-llm--kept))))
    (list :text (if markup (string-trim-right (substring text 0 markup)) text)
          :calls calls
          :model (superchat-bridge-assistant-message-model message))))

(defun superchat-llm--failure (message)
  "The failure of MESSAGE, a bridge message that ended with an error, as
ON-ERROR gets it.  Its text is all the bridge gives: the status leads it, and
the bridge's own classifier tells a passing failure from a quota or a refusal."
  (let* ((text (or (superchat-bridge-assistant-message-error-message message) ""))
         (status (and (string-match "\\`\\([1-5][0-9][0-9]\\)[ :]" text)
                      (string-to-number (match-string 1 text))))
         (case-fold-search t))
    (append
     (list :message (if (string-empty-p text) "The model failed with no message" text)
           :kind (cond ((not (superchat-bridge-retryable-assistant-error-p message)) 'other)
                       ((or (eql status 429) (string-match-p "rate.?limit\\|too many requests" text))
                        'rate-limit)
                       ((string-match-p "timed? out\\|timeout" text) 'timeout)
                       ;; Before `server': curl says "Could not connect to server".
                       ((and (not status)
                             (string-match-p "connect\\|network\\|socket\\|fetch\\|reset\\|closed\\|getaddrinfo\\|ENOTFOUND\\|EAI_AGAIN" text))
                        'network)
                       ((or status (string-match-p "overload\\|capacity\\|busy\\|unavailable\\|server"
                                                   text))
                        'server)
                       (t 'network)))
     (and status (list :status status)))))

(cl-defun superchat-llm--request (messages &key system tools model
                                           on-text on-done on-error)
  "Ask the provider once.  Arguments as `superchat-llm-request'."
  (unless (memq superchat-llm-reasoning '(nil minimal low medium high xhigh max))
    (user-error "`superchat-llm-reasoning' is %S; it is nil or one of minimal, low, medium, high, xhigh, max"
                superchat-llm-reasoning))
  (superchat-llm--models t)
  (let* ((pi-model (superchat-llm--find-model (or model superchat-llm-model)))
         (abort (superchat-bridge-make-abort-signal))
         (handle (list :cancelled nil :finished nil :abort abort))
         (stream nil) (keyless nil))
    ;; Resolving the key may signal too (a credential store that fails): that
    ;; is an error of this request, not of the caller's setup.
    (condition-case err
        (if (not (superchat-bridge-models-get-auth (superchat-llm--models) pi-model))
            (setq keyless t)
          (setq stream
                (superchat-bridge-models-stream-simple
                 (superchat-llm--models) pi-model
                 (superchat-llm--context messages system tools pi-model)
                 ;; Ten minutes for the response to start, one try: a retry
                 ;; is the run's, which shows it, and two owners multiply.
                 (append (list :signal abort :timeout-ms 600000 :max-retries 0)
                         (when superchat-llm-reasoning
                           (list :reasoning (symbol-name superchat-llm-reasoning)))))))
      (error (setf (plist-get handle :finished) t)
             (funcall on-error (list :message (error-message-string err) :kind 'other))))
    (when keyless
      (let* ((provider (superchat-bridge-models-get-provider
                        (superchat-llm--models) superchat-llm-provider))
             (key (superchat-bridge-provider-auth-api-key
                   (superchat-bridge-provider-auth provider)))
             (vars (and key (superchat-bridge-api-key-auth-env-vars key))))
        (if vars
            (user-error "No API key for %s: set %s in the environment and restart Emacs, or `setenv' it"
                        superchat-llm-provider (string-join vars " or "))
          (user-error "Provider %s is not configured: its credentials cannot be found"
                      superchat-llm-provider))))
    (when stream
      (superchat-bridge-event-stream-iterate
       stream
       (lambda (event)
         (when (and on-text
                    (not (or (plist-get handle :cancelled) (plist-get handle :finished)))
                    (equal (superchat-bridge-assistant-message-event-type event) "text_delta"))
           ;; the bridge would drop the stream on a signal and call nobody.
           (condition-case err
               (funcall on-text (superchat-bridge-assistant-message-event-delta event))
             (error (setf (plist-get handle :finished) t)
                    (superchat-bridge-abort-signal-abort abort)
                    (funcall on-error (list :message (error-message-string err)
                                            :kind 'other))))))
       (lambda ()
         (unless (or (plist-get handle :cancelled) (plist-get handle :finished))
           (setf (plist-get handle :finished) t)
           (let* ((message (superchat-bridge-event-stream-result stream))
                  (reason (and message (superchat-bridge-assistant-message-stop-reason message))))
             (cond ((equal reason "aborted"))
                   ((null message)
                    (funcall on-error (list :message "The model gave no response" :kind 'network)))
                   ((equal reason "error") (funcall on-error (superchat-llm--failure message)))
                   (t (let ((result (superchat-llm--result message messages)))
                        (cond
                         ((or (plist-get result :calls)
                              (not (string-empty-p (plist-get result :text))))
                          (funcall on-done result))
                         ((superchat-llm--markup-in message)
                          (funcall on-error (list :message "The model wrote a tool call as text instead of calling one"
                                                  :kind 'other)))
                         (t (funcall on-error (list :message "The model gave an empty response"
                                                    :kind 'empty))))))))))))
    handle))

(cl-defun superchat-llm-request (messages &key system tools model
                                          on-text on-done on-error)
  "Ask the model once for a response to MESSAGES; return a handle.
SYSTEM is the system prompt, TOOLS a list of tool specs, MODEL overrides
`superchat-llm-model'.  ON-TEXT gets each new piece of streamed text,
ON-DONE gets (:text S :calls CALLS :model S), ON-ERROR a plist
(:message S :kind KIND :status N), :status only when known.  Tool
calls come back in ON-DONE and are never run here."
  (funcall superchat-llm-request-function messages
           :system system :tools tools :model model
           :on-text on-text :on-done on-done :on-error on-error))

(defun superchat-llm-cancel (handle)
  "Cancel the request HANDLE: neither ON-DONE nor ON-ERROR will be called."
  (setf (plist-get handle :cancelled) t)
  (when-let* ((abort (plist-get handle :abort))
              ((not (plist-get handle :finished))))
    (superchat-bridge-abort-signal-abort abort)))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
