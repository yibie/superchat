;;; superchat-llm.el --- The only file that knows llm.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Neutral messages in, one model response out.  This file never runs a tool:
;; a tool call the model asks for goes back to the caller in ON-DONE.

;;; Code:

(require 'cl-lib)
(require 'superchat)
(require 'llm)
;; Only for `make-llm-provider-utils-tool-use': every provider reads the
;; assistant's tool-use turn as a list of these structs.
(require 'llm-provider-utils)

(defvar superchat-llm-request-function #'superchat-llm--request
  "What `superchat-llm-request' calls.  Tests bind it to a scripted function.")

(defun superchat-llm--tool (spec)
  "The `llm-tool' for SPEC, a plist (:name :description :args).
llm.el \"runs\" it, which does nothing: the calls are read from the response.
Every argument is optional for llm.el, which would refuse the whole response
over a missing one; the caller judges the calls."
  (make-llm-tool
   :name (plist-get spec :name)
   :description (plist-get spec :description)
   :function (lambda (&rest _) "recorded")
   :args (mapcar (lambda (arg)
                   (list :name (plist-get arg :name)
                         :type (plist-get arg :type)
                         :description (plist-get arg :description)
                         :optional t))
                 (plist-get spec :args))))

(defun superchat-llm--alist (plist)
  "PLIST (:path \"a\") as the alist ((path . \"a\")) that providers send."
  (cl-loop for (key value) on plist by #'cddr
           collect (cons (intern (substring (symbol-name key) 1)) value)))

(defun superchat-llm--plist (alist)
  "ALIST ((path . \"a\")) as the plist (:path \"a\"); a nil value is kept."
  (cl-loop for (key . value) in alist
           nconc (list (intern (concat ":" (symbol-name key))) value)))

(defun superchat-llm--explain (type message)
  "A sentence for a person about llm.el refusing a response: TYPE, MESSAGE."
  (pcase type
    ((guard (stringp message)) message)
    ('llm-tool-unknown-tool
     (format "The model asked for a tool that is not available: %s"
             (plist-get message :tool)))
    ('llm-tool-unknown-argument
     (format "The model gave tool %s an argument it does not have: %s"
             (plist-get message :tool) (plist-get message :arg)))
    (_ (format "The model's response was refused (%s)" type))))

(defun superchat-llm--prompt (messages system tools)
  "An `llm-chat-prompt' that replays MESSAGES, with TOOLS offered.
Replay uses llm.el's interaction structs: an assistant turn whose content is
a list of tool uses, then a `tool-results' turn, as llm.el writes them itself."
  (let ((prompt (llm-make-chat-prompt
                 "" :context system
                 :tools (mapcar #'superchat-llm--tool tools)))
        (results nil))
    (setf (llm-chat-prompt-interactions prompt) nil)
    (cl-flet ((add (role content)
                (setf (llm-chat-prompt-interactions prompt)
                      (append (llm-chat-prompt-interactions prompt)
                              (list (make-llm-chat-prompt-interaction
                                     :role role :content content)))))
              (flush ()
                (when results
                  (setf (llm-chat-prompt-interactions prompt)
                        (append (llm-chat-prompt-interactions prompt)
                                (list (make-llm-chat-prompt-interaction
                                       :role 'tool-results
                                       :tool-results (nreverse results)))))
                  (setq results nil))))
      (dolist (message messages)
        (let ((text (plist-get message :text)))
          (if (eq (plist-get message :role) 'tool)
              (push (make-llm-chat-prompt-tool-result
                     :call-id (plist-get message :call-id)
                     :tool-name (plist-get message :name) :result text)
                    results)
            (flush)
            (unless (and (eq (plist-get message :role) 'assistant)
                         (string-empty-p (or text "")))
              (add (plist-get message :role) text))
            (when-let* ((calls (plist-get message :calls)))
              (add 'assistant
                   (mapcar (lambda (call)
                             (make-llm-provider-utils-tool-use
                              :id (plist-get call :id) :name (plist-get call :name)
                              :args (superchat-llm--alist (plist-get call :args))))
                           calls))))))
      (flush))
    prompt))

(defun superchat-llm--provider (model)
  "The provider, asked for MODEL when one is given."
  (if model
      (let* ((provider (copy-sequence superchat-llm-provider))
             (type (type-of provider)))
        (unless (assq 'chat-model (cl-struct-slot-info type))
          (user-error "Provider %s has no chat-model slot, so a model cannot be set"
                      type))
        (setf (cl-struct-slot-value type 'chat-model provider) model)
        provider)
    superchat-llm-provider))

(cl-defun superchat-llm--request (messages &key system tools model
                                           on-text on-done on-error)
  "Ask the provider once.  Arguments as `superchat-llm-request'."
  (unless superchat-llm-provider
    (user-error "No LLM provider: set `superchat-llm-provider', for example \
(setq superchat-llm-provider (make-llm-openai :key \"...\"))"))
  (let* ((model (or model superchat-llm-model))
         (handle (list :cancelled nil :finished nil :request nil))
         (seen 0))
    (cl-flet ((live (callback)
                ;; A provider's callback may fire after a cancel, or twice.
                (lambda (&rest args)
                  (unless (or (plist-get handle :cancelled) (plist-get handle :finished))
                    (setf (plist-get handle :finished) t)
                    (apply callback args))))
              (fail (message)
                (setf (plist-get handle :finished) t)
                (funcall on-error message)))
      (condition-case err
          (setf (plist-get handle :request)
                (llm-chat-streaming
                 (superchat-llm--provider model)
                 (superchat-llm--prompt messages system tools)
                 ;; Partial results are the accumulated text; the caller gets deltas.
                 (lambda (partial)
                   (let ((text (plist-get partial :text)))
                     (when (and on-text text (> (length text) seen)
                                (not (plist-get handle :cancelled)))
                       (funcall on-text (substring text seen))
                       (setq seen (length text)))))
                 (live (lambda (result)
                         (funcall on-done
                                  (list :text (or (plist-get result :text) "")
                                        ;; Documented: `tool-uses' lists every use.
                                        :calls (mapcar
                                                (lambda (use)
                                                  (list :id (make-temp-name "call_")
                                                        :name (plist-get use :name)
                                                        :args (superchat-llm--plist
                                                               (plist-get use :args))))
                                                (plist-get result :tool-uses))
                                        :model (or model "default")))))
                 (live (lambda (type message)
                         (funcall on-error (superchat-llm--explain type message))))
                 t))
        ;; Raised while starting.  After a callback ran, the error is the
        ;; caller's own (inside ON-DONE, say) and is not turned into ON-ERROR.
        (error (if (or (plist-get handle :cancelled) (plist-get handle :finished))
                   (signal (car err) (cdr err))
                 (fail (error-message-string err)))))
      handle)))

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
  (when-let* ((request (plist-get handle :request))
              ((not (plist-get handle :finished))))
    (llm-cancel-request request)))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
