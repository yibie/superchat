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
(require 'superchat-event)

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
    (superchat-projection-register
     'experience-applicability
     :version 1
     :initial '(:nodes nil :relations nil)
     :apply #'superchat-experience--projection-apply)
    (superchat-on scope 'run-completed 'experience-observer
                  #'superchat-experience--run-completed-hook)))

(declare-function superchat-plan--nudge-on-completion "superchat-plan")
(declare-function superchat-agents-panel--refresh-after-turn "superchat-agents-panel")
(declare-function superchat-handoff--context-budget-exceeded "superchat-compact")

(declare-function superchat-inline--collect-reference "superchat-inline")
(declare-function superchat-register-surface "superchat-pipeline" (symbol))

;; These surface rows declare no `:inject': their `:apply' only loads code and
;; registers a surface/collector.  The Services below are call-time needs of the
;; entry points, checked by `superchat-plugin-require-active-row' so a missing
;; transport is reported as transport-unavailable, not as an unknown Surface.
(superchat-define-plugin inline-ask
  :doc "Inline ask: surface `inline-ask`, reference collector, command loading.
Call time needs Services `driver' and `view'."
  :apply (lambda (_scope _config)
           (require 'superchat-inline)
           (superchat-register-surface 'inline-ask)
           (superchat-context-register 'inline-reference
                                       #'superchat-inline--collect-reference
                                       :owner 'inline)))

(superchat-define-plugin inline-rewrite
  :doc "Inline rewrite: surface `inline-rewrite` and command loading.
Call time needs Services `driver' and `change-set'."
  :apply (lambda (_scope _config)
           (require 'superchat-rewrite)
           (superchat-register-surface 'inline-rewrite)))

(superchat-define-plugin org-babel
  :doc "Org Babel integration: surface `org-babel` and handler loading.
Call time needs Services `editor-workspace' and `driver' through submit-blocking."
  :apply (lambda (_scope _config)
           (require 'ob-superchat)
           (superchat-register-surface 'org-babel)))

(superchat-define-plugin magit
  :doc "Magit integration: surface `magit` and command loading.
Call time needs Services `editor-workspace' and `driver', plus Magit itself."
  :apply (lambda (_scope _config)
           (require 'superchat-magit)
           (superchat-register-surface 'magit)))

(superchat-define-plugin emacsclient
  :doc "JSON control adapter for emacsclient; it registers no Surface.
Call time needs Services `driver' and `session'."
  :apply (lambda (_scope _config)
           (require 'superchat-emacsclient)))

(declare-function superchat--team-projection-initial "superchat-subagent")
(declare-function superchat--team-projection-apply "superchat-subagent")
(declare-function superchat-workflow--instance-projection-apply "superchat-workflow")
(declare-function superchat-workspace--projection-apply "superchat-workspace")
(declare-function superchat-experience--projection-apply "superchat-experience")

(superchat-define-plugin subagent
  :inject '(driver tools tape)
  :doc "Sub-agent delegation: the team projection (tools and Surface autoload)."
  :apply (lambda (_scope _config)
    (superchat-projection-register
     'team
     :version 3
     :initial #'superchat--team-projection-initial
     :apply #'superchat--team-projection-apply)))

(superchat-define-plugin workflow
  :inject '(driver tape)
  :doc "Workflow recipes: the instance projection (the >> coordinator autoloads)."
  :apply (lambda (_scope _config)
    (superchat-projection-register
     'workflow-instance
     :version 1
     :initial nil
     :apply #'superchat-workflow--instance-projection-apply)))

(superchat-define-plugin workspace
  :inject '(tape)
  :doc "Artifact workspace: the artifacts projection (its tools autoload)."
  :apply (lambda (_scope _config)
    (superchat-projection-register
     'superchat-workspace-artifacts
     :version 2
     :initial nil
     :apply #'superchat-workspace--projection-apply)))

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
