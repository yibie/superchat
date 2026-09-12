;;; superchat-default-plugins.el --- default Service providers -*- lexical-binding: t; -*-

;;; Commentary:
;; Migration step 2 (docs/design/plugin-architecture.md §5): every Service of
;; spec §1.3 gets a default provider plugin whose method table delegates to
;; today's functions.  Behaviour is unchanged; the gain is that consumers can
;; now reach these capabilities by name (`superchat-service-call' /
;; `superchat-service-get') and a provider can be replaced by unmounting its
;; plugin and mounting another.  Step 5 moves each block into the plugin's
;; own file next to the code it wraps.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)

(declare-function superchat-pipeline-submit "superchat-pipeline" (envelope))
(declare-function superchat-runtime-cancel "superchat-runtime")
(declare-function superchat-runtime-steer "superchat-runtime")
(declare-function superchat-runtime-status "superchat-runtime")
(declare-function superchat-runtime-active-runs "superchat-runtime")
(declare-function superchat-runtime-pending-input "superchat-runtime")
(declare-function superchat-runtime-answer-input "superchat-runtime")
(declare-function superchat-runtime-enqueue-follow-up "superchat-runtime")
(declare-function superchat-runtime-pending-follow-ups "superchat-runtime")
(declare-function superchat-runtime-clear-follow-ups "superchat-runtime")
(declare-function superchat-tape-append-event "superchat-tape")
(declare-function superchat-tape-append-material "superchat-tape")
(declare-function superchat-tape-append-compaction-anchor "superchat-tape")
(declare-function superchat-tape-replay-events "superchat-tape")
(declare-function superchat-projection-register "superchat-projection")
(declare-function superchat-projection-replay "superchat-projection")
(declare-function superchat-projection-catch-up "superchat-projection")
(declare-function superchat-view-entries "superchat-tape-view")
(declare-function superchat-view-search "superchat-tape-view")
(declare-function superchat-view-assemble "superchat-tape-view")
(declare-function superchat-view-superseded-storage-ids "superchat-tape-view")
(declare-function superchat-view-session-history "superchat-tape-view")
(declare-function superchat-view-session-configuration "superchat-tape-view")
(declare-function superchat-view-session-ids "superchat-tape-view")
(declare-function superchat-authority-decide "superchat-authority")
(declare-function superchat-authority-resolve-effects "superchat-authority")
(declare-function superchat-authority-register-policy "superchat-authority")
(declare-function superchat-authority-register-effect-resolver "superchat-authority")
(declare-function superchat-authority-register-approval-preview "superchat-authority")
(declare-function superchat-tool-execute "superchat-authority")
(declare-function superchat-register-tool "superchat-tools")
(declare-function superchat-get-llm-tools "superchat-tools")
(declare-function superchat-render-view-sink "superchat-render")
(declare-function superchat-render-markdown-to-org "superchat-render")
(declare-function superchat-register-command "superchat")
(declare-function superchat-closure-judge "superchat-closure")
(declare-function superchat-editor-workspace-validate-buffer "superchat-editor-workspace")
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace")
(declare-function superchat-editor-workspace-owner-session "superchat-editor-workspace")
(declare-function superchat-editor-workspace-acquire-buffer "superchat-editor-workspace")
(declare-function superchat-editor-workspace-rollback-acquisition "superchat-editor-workspace")
(declare-function superchat-session-workspace-close "superchat-editor-workspace")
(declare-function superchat-change-set-apply "superchat-change-set")
(declare-function superchat-db-memory-insert "superchat-db")
(declare-function superchat-db-memory-search "superchat-db")
(declare-function superchat-db-tape-search-prioritized "superchat-db")
(declare-function superchat-db-memory-get-by-id "superchat-db")
(declare-function superchat-db-memory-list-review "superchat-db")
(declare-function superchat-db-memory-set-review "superchat-db")
(declare-function superchat-db-memory-count "superchat-db")
(declare-function superchat-db-memory-reset-projection "superchat-db")
(declare-function superchat-experience-assemble-recovery-view "superchat-experience")
(declare-function superchat-experience--run-completed-hook "superchat-experience")

(defvar superchat--command-alist)
(defvar superchat-authority-effect-resolvers)

(defmacro superchat-default-plugins--table (&rest pairs)
  "Build a method table from PAIRS of METHOD FUNCTION."
  `(list ,@(cl-loop for (method fn) on pairs by #'cddr
                    collect `(cons ',method ,fn))))

(superchat-define-plugin driver
  :inject '(tape)
  :doc "Envelope validation, the fixed Stage main line and the Run lifecycle."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'driver
     (superchat-default-plugins--table
      submit #'superchat-pipeline-submit
      cancel #'superchat-runtime-cancel
      steer #'superchat-runtime-steer
      status #'superchat-runtime-status
      active-runs #'superchat-runtime-active-runs
      pending-input #'superchat-runtime-pending-input
      answer-input #'superchat-runtime-answer-input
      enqueue-follow-up #'superchat-runtime-enqueue-follow-up
      pending-follow-ups #'superchat-runtime-pending-follow-ups
      clear-follow-ups #'superchat-runtime-clear-follow-ups))))

(superchat-define-plugin tape
  :doc "The append-only Tape and its projections."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'tape
     (superchat-default-plugins--table
      append-event #'superchat-tape-append-event
      append-material (lambda (&rest args) (apply #'superchat-tape-append-material args))
      append-anchor (lambda (&rest args) (apply #'superchat-tape-append-compaction-anchor args))
      replay #'superchat-tape-replay-events
      register-projection (lambda (&rest args) (apply #'superchat-projection-register args))
      replay-projection #'superchat-projection-replay
      catch-up-projection #'superchat-projection-catch-up))))

(superchat-define-plugin session-view
  :inject '(tape)
  :doc "Bounded Context Views, search and Session history over the Tape."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'session
     (superchat-default-plugins--table
      entries #'superchat-view-entries
      search #'superchat-view-search
      assemble-view (lambda (&rest args) (apply #'superchat-view-assemble args))
      superseded-ids #'superchat-view-superseded-storage-ids
      history (lambda (&rest args) (apply #'superchat-view-session-history args))
      configuration #'superchat-view-session-configuration
      session-ids #'superchat-view-session-ids))))

(superchat-define-plugin authority
  :inject '(tape)
  :doc "Effect resolution, policies and approvals."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'authority
     (superchat-default-plugins--table
      decide #'superchat-authority-decide
      resolve-effects #'superchat-authority-resolve-effects
      register-policy #'superchat-authority-register-policy
      register-effect-resolver #'superchat-authority-register-effect-resolver
      register-approval-preview #'superchat-authority-register-approval-preview))))

(superchat-define-plugin tools
  :inject '(authority tape)
  :doc "The tool registry, materialization for the model, and the one execution seam."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'tools
     (superchat-default-plugins--table
      register (lambda (&rest args) (apply #'superchat-register-tool args))
      descriptors (lambda ()
                    (let (names)
                      (when (hash-table-p superchat-authority-effect-resolvers)
                        (maphash (lambda (k _) (push k names))
                                 superchat-authority-effect-resolvers))
                      (nreverse names)))
      materialize #'superchat-get-llm-tools
      execute (lambda (&rest args) (apply #'superchat-tool-execute args))))))

(superchat-define-plugin chat-view
  :doc "The chat window's Sink and rendering."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'view
     (superchat-default-plugins--table
      sink-for (lambda (_surface context)
                 (superchat-render-view-sink (plist-get context :buffer)
                                             (plist-get context :session-id)
                                             (plist-get context :owner)))
      markdown-to-org #'superchat-render-markdown-to-org))))

(superchat-define-plugin chat-commands
  :doc "Slash command registry."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'commands
     (superchat-default-plugins--table
      register #'superchat-register-command
      lookup (lambda (name) (cdr (assoc name superchat--command-alist)))
      list (lambda () (mapcar #'car superchat--command-alist))))))

(superchat-define-plugin closure
  :inject '(tape)
  :doc "Closure judgments over Tape traces."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'closure
     (superchat-default-plugins--table judge #'superchat-closure-judge))))

(superchat-define-plugin editor-workspace
  :doc "Session-owned editor buffers."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'editor-workspace
     (superchat-default-plugins--table
      validate-buffer #'superchat-editor-workspace-validate-buffer
      buffer #'superchat-editor-workspace-buffer
      owner-session #'superchat-editor-workspace-owner-session
      acquire-buffer #'superchat-editor-workspace-acquire-buffer
      rollback-acquisition (lambda (&rest args)
                             (apply #'superchat-editor-workspace-rollback-acquisition args))
      close #'superchat-session-workspace-close))))

(superchat-define-plugin change-set
  :inject '(tape)
  :doc "The only writer into a host buffer (Invariant 1)."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'change-set
     (superchat-default-plugins--table
      apply (lambda (&rest args) (apply #'superchat-change-set-apply args))))))

(superchat-define-plugin memory
  :inject '(tape)
  :doc "Memory rows: insert, search, review."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'memory
     (superchat-default-plugins--table
      insert (lambda (&rest args) (apply #'superchat-db-memory-insert args))
      search #'superchat-db-memory-search
      search-prioritized #'superchat-db-tape-search-prioritized
      get #'superchat-db-memory-get-by-id
      list-review #'superchat-db-memory-list-review
      set-review #'superchat-db-memory-set-review
      count #'superchat-db-memory-count
      reset-projection #'superchat-db-memory-reset-projection))))

(superchat-define-plugin experience
  :inject '(tape)
  :doc "Experience self-learning: recovery views for the driver and the run-completed observer."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'recovery
     (superchat-default-plugins--table
      assemble-view #'superchat-experience-assemble-recovery-view))
    (superchat-plugin-add-hook 'superchat-run-completed-functions
                               #'superchat-experience--run-completed-hook)))

(provide 'superchat-default-plugins)
;;; superchat-default-plugins.el ends here
