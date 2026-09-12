;;; superchat-transport-llm.el --- default model-transport plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides the `model-transport' Service with today's llm.el loop
;; (`superchat--llm-generate-answer').  Migration step 0: a wrapper, no
;; behaviour change; step 2 moves the loop itself here.

;;; Code:

(require 'superchat-plugin)
(require 'superchat-service)

(declare-function superchat--llm-generate-answer "superchat-llm")
(declare-function superchat--llm-resolve-streaming "superchat-llm")

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
           (cons 'capabilities
                 (lambda (_model)
                   (require 'superchat-llm)
                   (list :streaming (and (fboundp 'superchat--llm-resolve-streaming)
                                         (ignore-errors (superchat--llm-resolve-streaming))
                                         t)
                         :tools t)))))))

(provide 'superchat-transport-llm)
;;; superchat-transport-llm.el ends here
