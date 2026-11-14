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

(defun superchat-llm--tool (spec record)
  "The `llm-tool' for SPEC, a plist (:name :description :args).
llm.el \"runs\" it by calling RECORD with the tool name and a plist of the
arguments; nothing else happens, and the result is a constant."
  (let ((names (mapcar (lambda (arg) (plist-get arg :name)) (plist-get spec :args))))
    (make-llm-tool
     :name (plist-get spec :name)
     :description (plist-get spec :description)
     :function (lambda (&rest values)
                 (funcall record (plist-get spec :name)
                          (cl-loop for name in names for value in values
                                   when value
                                   nconc (list (intern (concat ":" name)) value)))
                 "recorded")
     :args (mapcar (lambda (arg)
                     (list :name (plist-get arg :name)
                           :type (plist-get arg :type)
                           :description (plist-get arg :description)
                           :optional (not (plist-get arg :required))))
                   (plist-get spec :args)))))

(defun superchat-llm--alist (plist)
  "PLIST (:path \"a\") as the alist ((path . \"a\")) that providers send."
  (cl-loop for (key value) on plist by #'cddr
           collect (cons (intern (substring (symbol-name key) 1)) value)))

(defun superchat-llm--prompt (messages system tools record)
  "An `llm-chat-prompt' that replays MESSAGES, with TOOLS recording into RECORD.
Replay uses llm.el's interaction structs: an assistant turn whose content is
a list of tool uses, then a `tool-results' turn, as llm.el writes them itself."
  (let ((prompt (llm-make-chat-prompt
                 "" :context system
                 :tools (mapcar (lambda (spec) (superchat-llm--tool spec record))
                                tools)))
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
  (unless superchat-llm-provider
    (user-error "No LLM provider: set `superchat-llm-provider', for example \
(setq superchat-llm-provider (make-llm-openai :key \"...\"))"))
  (if model
      (let ((provider (copy-sequence superchat-llm-provider)))
        (setf (cl-struct-slot-value (type-of provider) 'chat-model provider)
              model)
        provider)
    superchat-llm-provider))

(cl-defun superchat-llm--request (messages &key system tools model
                                           on-text on-done on-error)
  "Ask the provider once.  Arguments as `superchat-llm-request'."
  (let* ((model (or model superchat-llm-model))
         (provider (superchat-llm--provider model))
         (recorded nil)
         (prompt (superchat-llm--prompt
                  messages system tools
                  (lambda (name args) (push (cons name args) recorded))))
         (handle (list :cancelled nil :finished nil :request nil))
         (seen 0))
    (cl-flet ((live (callback)
                ;; A provider's callback may still fire after a cancel.
                (lambda (&rest args)
                  (unless (plist-get handle :cancelled)
                    (setf (plist-get handle :finished) t)
                    (apply callback args)))))
      (setf (plist-get handle :request)
            (llm-chat-streaming
             provider prompt
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
                                    :calls (mapcar
                                            (lambda (call)
                                              (list :id (make-temp-name "call_")
                                                    :name (car call) :args (cdr call)))
                                            (nreverse recorded))
                                    :model (or model (llm-name provider))))))
             (live (lambda (_type message)
                     (funcall on-error (if (stringp message)
                                           message
                                         (format "%S" message)))))
             t)))
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
  (when-let* ((request (plist-get handle :request))
              ((not (plist-get handle :finished))))
    (llm-cancel-request request)))

(defun superchat-llm-models ()
  "Model names for completion: the provider's, else the configured one."
  (if (and superchat-llm-provider
           (memq 'model-list (llm-capabilities superchat-llm-provider)))
      (llm-models superchat-llm-provider)
    (and superchat-llm-model (list superchat-llm-model))))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
