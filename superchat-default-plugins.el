;;; superchat-default-plugins.el --- default Service providers -*- lexical-binding: t; -*-

;;; Commentary:
;; Default plugins whose code loads lazily (experience, plan, agents-panel,
;; compact): their definitions must exist when `superchat-plugins' is
;; mounted, so they live here with autoloaded listener functions.  Every
;; other default plugin is defined in the file that owns its code.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-stage)

(declare-function superchat-experience-assemble-recovery-view "superchat-experience")
(declare-function superchat-experience--run-completed-hook "superchat-experience")


(superchat-define-plugin experience
  :inject '(tape)
  :doc "Experience self-learning: recovery views for the driver and the run-completed observer."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'recovery
     (superchat-service-table
      assemble-view #'superchat-experience-assemble-recovery-view))
    (superchat-on scope 'run-completed 'experience-observer
                  #'superchat-experience--run-completed-hook)))

(declare-function superchat-plan--nudge-on-completion "superchat-plan")
(declare-function superchat-agents-panel--refresh-after-turn "superchat-agents-panel")
(declare-function superchat-handoff--context-budget-exceeded "superchat-compact")

(superchat-define-plugin plan
  :doc "Plan objects: nudges the model toward the approved plan after each Turn."
  :apply (lambda (scope _)
           (superchat-on scope 'turn-completed 'plan-nudge
                         #'superchat-plan--nudge-on-completion)))

(superchat-define-plugin agents-panel
  :doc "Live sub-agent panel refresh."
  :apply (lambda (scope _)
           (superchat-on scope 'turn-completed 'agents-panel-refresh
                         #'superchat-agents-panel--refresh-after-turn)))

(superchat-define-plugin compact
  :doc "Context overflow recovery through compaction."
  :apply (lambda (scope _)
           (superchat-on scope 'context-budget-exceeded 'compact-recovery
                         #'superchat-handoff--context-budget-exceeded)))

(provide 'superchat-default-plugins)
;;; superchat-default-plugins.el ends here
