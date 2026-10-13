;;; superchat-transport-llm.el --- default model-transport plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides the `model-transport' Service with today's llm.el loop
;; (`superchat--llm-generate-answer').  Migration step 0: a wrapper, no
;; behaviour change; step 2 moves the loop itself here.

;;; Code:

(require 'superchat-plugin)
(require 'superchat-service)

(declare-function superchat--llm-generate-answer "superchat-llm")
(declare-function superchat--llm-context-budget-chars "superchat-llm")
(declare-function superchat--effective-llm-backend "superchat-llm")
(declare-function superchat--provider-chat-model "superchat-llm")
(declare-function superchat-llm--resolve-streaming "superchat-llm")

(defun superchat-transport-llm--capabilities (model)
  "What MODEL can do, as (:streaming BOOL :tools BOOL).
Streaming is what `superchat-llm--resolve-streaming' says of the backend
and model a request for MODEL would use -- the resolver the request loop
itself asks.  An error from it reaches the caller: \"could not tell\" is
not \"does not stream\"."
  (require 'superchat-llm)
  (let* ((backend (superchat--effective-llm-backend model))
         (name (or (superchat--provider-chat-model backend) model "unknown")))
    (list :streaming (and (plist-get (superchat-llm--resolve-streaming backend name nil)
                                     :streaming)
                          t)
          :tools t)))

(superchat-define-plugin transport-llm
  :doc "The llm.el model loop as the default model-transport."
  :apply
  (lambda (scope _config)
    (superchat-service-provide
     scope 'model-transport
     (list (cons 'generate
                 (lambda (&rest args)
                   (require 'superchat-llm)
                   (apply #'superchat--llm-generate-answer args)))
           (cons 'context-budget
                 (lambda (&rest args)
                   (require 'superchat-llm)
                   (apply #'superchat--llm-context-budget-chars args)))
           (cons 'capabilities #'superchat-transport-llm--capabilities)))))

(provide 'superchat-transport-llm)
;;; superchat-transport-llm.el ends here
