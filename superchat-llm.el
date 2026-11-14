;;; superchat-llm.el --- The only file that knows pi-ai -*- lexical-binding: t; -*-

;;; Commentary:
;; Neutral messages in, one model response out.  This file never runs a tool:
;; a tool call the model asks for goes back to the caller in ON-DONE.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'llm-pi-ai)
(require 'llm-pi-ai-provider-deepseek)
(require 'llm-pi-ai-utils-abort)

(defvar superchat-llm-providers '(("deepseek" . llm-pi-ai-deepseek-provider))
  "Alist from a provider name to a function of no arguments that returns a
pi-ai provider.  The name is the provider's id.  A plugin may add to it.")

(defvar superchat-llm-request-function #'superchat-llm--request
  "What `superchat-llm-request' calls.  Tests bind it to a scripted function.")

(defvar superchat-llm--models nil
  "(PROVIDERS . MODELS): the collection made from that value of
`superchat-llm-providers'.")

(defvar superchat-llm--kept (make-hash-table :test #'equal)
  "The pi-ai assistant message of a response that made calls, by the id of its
first call.  Memory only: the Tape keeps no reasoning, but some providers want
it sent back with the calls it led to until the turn is answered.")

(defun superchat-llm--models ()
  "The pi-ai models collection, rebuilt when `superchat-llm-providers' changed."
  (let ((old (car superchat-llm--models)))
    (unless (and (= (length old) (length superchat-llm-providers))
                 (cl-every (lambda (a b) (and (equal (car a) (car b))
                                              (eq (cdr a) (cdr b))))
                           old superchat-llm-providers))
      (let ((models (llm-pi-ai-create-models)))
        (dolist (entry superchat-llm-providers)
          (llm-pi-ai-models-set-provider models (funcall (cdr entry))))
        (setq superchat-llm--models
              (cons (copy-sequence superchat-llm-providers) models)))))
  (cdr superchat-llm--models))

(defun superchat-llm--find-model (id)
  "The pi-ai model ID of `superchat-llm-provider'.
A `user-error' says what to set when either is unset or unknown."
  (let ((names (mapcar #'car superchat-llm-providers)))
    (unless (member superchat-llm-provider names)
      (user-error "%s; set `superchat-llm-provider' to one of: %s, for example (setq superchat-llm-provider %S)"
                  (if superchat-llm-provider
                      (format "Unknown LLM provider %S" superchat-llm-provider)
                    "No LLM provider")
                  (string-join names ", ") (or (car names) "name"))))
  (let* ((models (superchat-llm--models))
         (known (mapcar #'llm-pi-ai-model-id
                        (llm-pi-ai-models-get-models models superchat-llm-provider))))
    (or (llm-pi-ai-models-get-model models superchat-llm-provider id)
        (user-error "%s; set `superchat-llm-model' to one of: %s"
                    (if id (format "Unknown model %S of %s" id superchat-llm-provider)
                      (format "No model for %s" superchat-llm-provider))
                    (string-join known ", ")))))

(defun superchat-llm--now ()
  "Milliseconds since the epoch."
  (floor (* 1000 (float-time))))

(defun superchat-llm--tool (spec)
  "The pi-ai tool for SPEC, a plist (:name :description :args).
The parameters are a JSON-schema object; nil is `{}' to pi-ai."
  (let ((args (plist-get spec :args)))
    (llm-pi-ai-make-tool
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
  "ARGUMENTS as pi-ai holds them as a plist with keyword keys.
pi-ai gives a hash table (or an alist) with arrays as vectors.  Objects nest as
plists, arrays as lists; JSON false and null are nil."
  (cond ((hash-table-p arguments)
         (cl-loop for key being the hash-keys of arguments using (hash-values value)
                  nconc (list (intern (concat ":" key)) (superchat-llm--args value))))
        ((consp arguments)
         (cl-loop for (key . value) in arguments
                  nconc (list (intern (format ":%s" key)) (superchat-llm--args value))))
        ((vectorp arguments) (mapcar #'superchat-llm--args arguments))
        ((memq arguments '(:false :null)) nil)
        (t arguments)))

(defun superchat-llm--assistant (message model)
  "The pi-ai assistant message that replays assistant MESSAGE for MODEL.
The kept message of its first call is that message itself.  Otherwise one is
built with the fields `llm-pi-ai-transform-messages' and the completions API
read: api, provider and model of MODEL (so it counts as MODEL's own), zero
usage, stop reason \"toolUse\" or \"stop\", and a timestamp."
  (let ((calls (plist-get message :calls)))
    (or (and calls (gethash (plist-get (car calls) :id) superchat-llm--kept))
        (llm-pi-ai-make-assistant-message
         :content
         (append
          (unless (string-empty-p (or (plist-get message :text) ""))
            (list (llm-pi-ai-make-text-content :text (plist-get message :text))))
          (mapcar (lambda (call)
                    (llm-pi-ai-make-tool-call :id (plist-get call :id)
                                              :name (plist-get call :name)
                                              :arguments (plist-get call :args)))
                  calls))
         :api (llm-pi-ai-model-api model)
         :provider (llm-pi-ai-model-provider model)
         :model (llm-pi-ai-model-id model)
         :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
         :stop-reason (if calls "toolUse" "stop")
         :timestamp (superchat-llm--now)))))

(defun superchat-llm--context (messages system tools model)
  "The pi-ai context that replays MESSAGES for MODEL, with SYSTEM and TOOLS."
  ;; A new user turn: no provider wants the reasoning of earlier turns.
  (when (eq (plist-get (car (last messages)) :role) 'user)
    (clrhash superchat-llm--kept))
  (llm-pi-ai-make-context
   :system-prompt system
   :tools (mapcar #'superchat-llm--tool tools)
   :messages
   (cl-loop for message in messages
            for text = (plist-get message :text)
            for role = (plist-get message :role)
            for built = (pcase role
                          ('user (llm-pi-ai-make-user-message
                                  :content text :timestamp (superchat-llm--now)))
                          ('assistant
                           (unless (and (string-empty-p (or text ""))
                                        (null (plist-get message :calls)))
                             (superchat-llm--assistant message model)))
                          ('tool (llm-pi-ai-make-tool-result-message
                                  :tool-call-id (plist-get message :call-id)
                                  :tool-name (plist-get message :name)
                                  :content (list (llm-pi-ai-make-text-content :text text))
                                  :timestamp (superchat-llm--now)))
                          (_ (error "Unknown message role %S" role)))
            when built collect built)))

(defun superchat-llm--result (message)
  "The ON-DONE plist for the final assistant MESSAGE; keeps it if it made calls."
  (let* ((blocks (llm-pi-ai-message-content message))
         (calls (cl-loop for block in blocks
                         when (llm-pi-ai-tool-call-p block)
                         collect (list :id (llm-pi-ai-tool-call-id block)
                                       :name (llm-pi-ai-tool-call-name block)
                                       :args (unless (llm-pi-ai-tool-call-arguments-error block)
                                               (superchat-llm--args
                                                (llm-pi-ai-tool-call-arguments block)))))))
    (when calls
      (puthash (plist-get (car calls) :id) message superchat-llm--kept))
    (list :text (mapconcat #'llm-pi-ai-text-content-text
                           (cl-remove-if-not #'llm-pi-ai-text-content-p blocks) "")
          :calls calls
          :model (llm-pi-ai-assistant-message-model message))))

(cl-defun superchat-llm--request (messages &key system tools model
                                           on-text on-done on-error)
  "Ask the provider once.  Arguments as `superchat-llm-request'."
  (let* ((pi-model (superchat-llm--find-model (or model superchat-llm-model)))
         (abort (llm-pi-ai-make-abort-signal))
         (handle (list :cancelled nil :finished nil :abort abort))
         (stream nil))
    (condition-case err
        (setq stream
              (llm-pi-ai-models-stream-simple
               (superchat-llm--models) pi-model
               (superchat-llm--context messages system tools pi-model)
               (append (list :signal abort)
                       (when superchat-llm-reasoning
                         (list :reasoning (symbol-name superchat-llm-reasoning))))))
      (error (setf (plist-get handle :finished) t)
             (funcall on-error (error-message-string err))))
    (when stream
      (llm-pi-ai-event-stream-iterate
       stream
       (lambda (event)
         (when (and on-text (not (plist-get handle :cancelled))
                    (equal (llm-pi-ai-assistant-message-event-type event) "text_delta"))
           (funcall on-text (llm-pi-ai-assistant-message-event-delta event))))
       (lambda ()
         (unless (or (plist-get handle :cancelled) (plist-get handle :finished))
           (setf (plist-get handle :finished) t)
           (let* ((message (llm-pi-ai-event-stream-result stream))
                  (reason (and message (llm-pi-ai-assistant-message-stop-reason message))))
             (cond ((equal reason "aborted"))
                   ((or (null message) (equal reason "error"))
                    (funcall on-error
                             (or (and message (llm-pi-ai-assistant-message-error-message message))
                                 "The model gave no response")))
                   (t (funcall on-done (superchat-llm--result message)))))))))
    handle))

(cl-defun superchat-llm-request (messages &key system tools model
                                          on-text on-done on-error)
  "Ask the model once for a response to MESSAGES; return a handle.
SYSTEM is the system prompt, TOOLS a list of tool specs, MODEL overrides
`superchat-llm-model'.  ON-TEXT gets each new piece of streamed text,
ON-DONE gets (:text S :calls CALLS :model S), ON-ERROR a string.  Tool
calls come back in ON-DONE and are never run here."
  (funcall superchat-llm-request-function messages
           :system system :tools tools :model model
           :on-text on-text :on-done on-done :on-error on-error))

(defun superchat-llm-cancel (handle)
  "Cancel the request HANDLE: neither ON-DONE nor ON-ERROR will be called."
  (setf (plist-get handle :cancelled) t)
  (when-let* ((abort (plist-get handle :abort))
              ((not (plist-get handle :finished))))
    (llm-pi-ai-abort-signal-abort abort)))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
