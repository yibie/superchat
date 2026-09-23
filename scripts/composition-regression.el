;;; composition-regression.el --- M9 composition-layer regression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for M9 slice 1 (accepted record
;; docs/decisions/accepted/2026-09-22-spine-loads-without-the-product.md,
;; "Decision" item 1 and "Acceptance"): for each default row that is not a
;; Surface, unmounting it withdraws its registrations, a chat Turn still
;; completes, and remounting restores it.  Rows covered: `memory', `changes',
;; `subagent', `workflow', `workspace', `plan', `compact', plus an in-flight
;; Run that outlives unmounting `memory'.
;;
;; Finding recorded by this file: none of the seven modules has an entry point
;; that calls `superchat-plugin-require-active-row' (grep 2026-09-22), so there
;; is no row-refusal assertion to make for them; the absence is reported.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets.  All model calls
;; are scripted; no user configuration is evaluated.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/composition-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^composition/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Composition regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro composition--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

;;;; Composition restoration
;; Copied from scripts/m3-regression.el:2994-3033 under the `composition--'
;; prefix, so this suite does not load the leader-owned m3-regression.el.

(defun composition--default-row-names ()
  "The row names of `superchat-plugins' standard value."
  (mapcar (lambda (row) (if (consp row) (car row) row))
          (eval (car (get 'superchat-plugins 'standard-value)) t)))

(defun composition--mounted-row-names ()
  "The row names of every mounted plugin fiber."
  (mapcar #'car (superchat-plugin-report)))

(defmacro composition--with-restored-composition (&rest body)
  "Run BODY, then put the mounted composition back as it was.
Tests here mount and unmount rows in the global Scope, which no fixture
resets; restoration goes through `superchat-profile-mount', the product's own
exact composition operation."
  (declare (indent 0) (debug t))
  (let ((before (make-symbol "before"))
        (profile (make-symbol "profile")))
    `(let ((,before (composition--mounted-row-names))
           (,profile superchat-profile))
       (unwind-protect (progn ,@body)
         (ignore-errors (superchat-profile-mount ,profile))
         (dolist (row ,before)
           (unless (superchat-plugin-state row)
             (ignore-errors (superchat-plugin-mount row))))))))

;;;; Registration probes

(defun composition--service-present-p (name)
  "Non-nil when Service NAME has a provider in the global Scope."
  (superchat-service-get superchat-scope-global name))

(defun composition--projection-present-p (name)
  "Non-nil when projection NAME is registered."
  (alist-get name superchat-projection--registry))

(defun composition--listener-present-p (event id)
  "Non-nil when listener ID is visible for EVENT from the global Scope."
  (assq id (superchat-event-listeners nil event)))

(defun composition--complete-a-turn ()
  "Open the chat, complete one scripted Turn, return its run id."
  (let* ((view (smoke--open-chat))
         (run (smoke--send view "hello")))
    (should (stringp run))
    ;; The Run must have entered the registry before `smoke--run-done-p' can
    ;; tell "not started yet" apart from "already finished".
    (should (smoke--wait
             (lambda () (member run (superchat-runtime-active-runs)))
             10))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    run))

;;;; memory

(ert-deftest composition/memory/unmount-withdraws-and-remount-restores ()
  "Unmounting `memory' withdraws its Service and projection; a chat Turn
still completes and remounting restores both.
No entry point in `superchat-memory.el' calls
`superchat-plugin-require-active-row', so there is no refusal message to
assert here (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'memory))
      (should (composition--service-present-p 'memory))
      (should (composition--projection-present-p 'memory))
      (superchat-plugin-unmount 'memory)
      (should-not (superchat-plugin-row-active-p 'memory))
      (should-not (composition--service-present-p 'memory))
      (should-not (composition--projection-present-p 'memory))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'memory)
      (should (superchat-plugin-row-active-p 'memory))
      (should (composition--service-present-p 'memory))
      (should (composition--projection-present-p 'memory)))))

;;;; changes

(ert-deftest composition/changes/unmount-withdraws-and-remount-restores ()
  "Unmounting `changes' withdraws its `turn-completed' listener; a chat Turn
still completes and remounting restores the listener.
No entry point in `superchat-changes.el' calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'changes))
      (should (composition--listener-present-p 'turn-completed 'changes-notice))
      (superchat-plugin-unmount 'changes)
      (should-not (superchat-plugin-row-active-p 'changes))
      (should-not (composition--listener-present-p 'turn-completed 'changes-notice))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'changes)
      (should (superchat-plugin-row-active-p 'changes))
      (should (composition--listener-present-p 'turn-completed 'changes-notice)))))

;;;; subagent

(ert-deftest composition/subagent/unmount-withdraws-and-remount-restores ()
  "Unmounting `subagent' withdraws the `team' projection; a chat Turn still
completes and remounting restores the projection.
No entry point in `superchat-subagent.el' calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'subagent))
      (should (composition--projection-present-p 'team))
      (superchat-plugin-unmount 'subagent)
      (should-not (superchat-plugin-row-active-p 'subagent))
      (should-not (composition--projection-present-p 'team))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'subagent)
      (should (superchat-plugin-row-active-p 'subagent))
      (should (composition--projection-present-p 'team)))))

;;;; workflow

(ert-deftest composition/workflow/unmount-withdraws-and-remount-restores ()
  "Unmounting `workflow' withdraws the `workflow-instance' projection; a chat
Turn still completes and remounting restores the projection.
No entry point in `superchat-workflow.el' calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'workflow))
      (should (composition--projection-present-p 'workflow-instance))
      (superchat-plugin-unmount 'workflow)
      (should-not (superchat-plugin-row-active-p 'workflow))
      (should-not (composition--projection-present-p 'workflow-instance))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'workflow)
      (should (superchat-plugin-row-active-p 'workflow))
      (should (composition--projection-present-p 'workflow-instance)))))

;;;; workspace

(ert-deftest composition/workspace/unmount-withdraws-and-remount-restores ()
  "Unmounting `workspace' withdraws the `superchat-workspace-artifacts'
projection; a chat Turn still completes and remounting restores it.
No entry point in `superchat-workspace.el' calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'workspace))
      (should (composition--projection-present-p 'superchat-workspace-artifacts))
      (superchat-plugin-unmount 'workspace)
      (should-not (superchat-plugin-row-active-p 'workspace))
      (should-not (composition--projection-present-p 'superchat-workspace-artifacts))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'workspace)
      (should (superchat-plugin-row-active-p 'workspace))
      (should (composition--projection-present-p 'superchat-workspace-artifacts)))))

;;;; plan

(ert-deftest composition/plan/unmount-withdraws-and-remount-restores ()
  "Unmounting `plan' withdraws its `turn-completed' listener; a chat Turn
still completes and remounting restores the listener.
No entry point in `superchat-plan.el' calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'plan))
      (should (composition--listener-present-p 'turn-completed 'plan-nudge))
      (superchat-plugin-unmount 'plan)
      (should-not (superchat-plugin-row-active-p 'plan))
      (should-not (composition--listener-present-p 'turn-completed 'plan-nudge))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'plan)
      (should (superchat-plugin-row-active-p 'plan))
      (should (composition--listener-present-p 'turn-completed 'plan-nudge)))))

;;;; compact

(ert-deftest composition/compact/unmount-withdraws-and-remount-restores ()
  "Unmounting `compact' withdraws its `context-budget-exceeded' listener; a
chat Turn still completes and remounting restores the listener.
No entry point in the module that defines the listener calls
`superchat-plugin-require-active-row' (report finding)."
  (composition--fixture '((:text "ok"))
    (composition--with-restored-composition
      (should (superchat-plugin-row-active-p 'compact))
      (should (composition--listener-present-p
               'context-budget-exceeded 'compact-recovery))
      (superchat-plugin-unmount 'compact)
      (should-not (superchat-plugin-row-active-p 'compact))
      (should-not (composition--listener-present-p
                   'context-budget-exceeded 'compact-recovery))
      (composition--complete-a-turn)
      (superchat-plugin-mount 'compact)
      (should (superchat-plugin-row-active-p 'compact))
      (should (composition--listener-present-p
               'context-budget-exceeded 'compact-recovery)))))

;;;; An in-flight Run

(ert-deftest composition/in-flight-run-survives-unmounting-memory ()
  "Unmounting `memory' while a Turn is running does not harm the Run."
  (composition--fixture '((:text "slow" :delay 0.5))
    (composition--with-restored-composition
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "hello")))
        (should (stringp run))
        (should (smoke--wait
                 (lambda () (member run (superchat-runtime-active-runs)))
                 10))
        (superchat-plugin-unmount 'memory)
        (should-not (superchat-plugin-row-active-p 'memory))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (superchat-plugin-mount 'memory)
        (should (superchat-plugin-row-active-p 'memory))))))

;;;; Row-owned tools

(defun composition--offered-p (name)
  "Non-nil when tool NAME is in the chat surface's current tool decision."
  (member name (plist-get (superchat-select-tools-decision
                           (list :surface 'chat :input-core (list :operation 'chat)) nil)
                          :names)))

(defmacro composition--tool-withdrawn-test (row tool)
  `(ert-deftest ,(intern (format "composition/%s/unmount-withdraws-its-tool-%s" row tool)) ()
     ,(format "Unmounting `%s' stops offering `%s' to the model." row tool)
     (composition--with-restored-composition
       (let ((superchat-llm-tool-names
              (append superchat-llm-default-tool-names (list ,tool))))
         (should (composition--offered-p ,tool))
         (superchat-plugin-unmount ',row)
         (should-not (composition--offered-p ,tool))))))

(composition--tool-withdrawn-test subagent "delegate_to_subagent")
(composition--tool-withdrawn-test workspace "workspace_write")
(composition--tool-withdrawn-test memory "memory_search")

(ert-deftest composition/row-tools/explicit-list-cannot-select-a-withdrawn-tool ()
  "An explicit tool list cannot select a tool whose row is unmounted.
`superchat--tool-descriptor-p' consults the row's own registration, so the
withdrawn name is dropped while an ordinary built-in stays."
  (composition--with-restored-composition
    (superchat-plugin-unmount 'subagent)
    (let* ((decision (superchat-select-tools-decision
                      (list :surface 'chat
                            :input-core (list :operation 'chat)
                            :tools '("delegate_to_subagent" "read_buffer"))
                      nil))
           (names (plist-get decision :names)))
      (should-not (member "delegate_to_subagent" names))
      (should (member "read_buffer" names)))))

(ert-deftest composition/row-tools/default-surface-unchanged ()
  "The default tool surface is unchanged by the row-owned move.
Captured on the unmodified tree before this slice (superchat.el mounted
profile, default `superchat-llm-tool-names')."
  (composition--fixture nil
    (should (equal '("list_buffers" "read_buffer" "read_file" "read_reference"
                     "open_file" "edit_buffer" "describe-function" "apropos"
                     "eval_elisp" "ask_user")
                   (plist-get (superchat-select-tools-decision
                               (list :surface 'chat
                                     :input-core (list :operation 'chat))
                               nil)
                              :names)))))

(ert-deftest composition/row-tools/remount-restores-the-tool ()
  "Unmounting then remounting `workspace' withdraws and restores its tool."
  (composition--with-restored-composition
    (let ((superchat-llm-tool-names
           (append superchat-llm-default-tool-names (list "workspace_write"))))
      (should (composition--offered-p "workspace_write"))
      (superchat-plugin-unmount 'workspace)
      (should-not (composition--offered-p "workspace_write"))
      (superchat-plugin-mount 'workspace)
      (should (composition--offered-p "workspace_write")))))

(ert-deftest composition/row-tools/materialized-tool-refuses-after-unmount ()
  "A tool materialized while its row was mounted refuses after the unmount.
The model's first step arrives after a delay; `memory' is unmounted during
it, so the call reaches Authority with the row gone.  The refusal is a
`tool.failed' fact and reaches the model in the follow-up request; no body
and no `tool.execution.started' happen."
  (composition--fixture
      '((:delay 0.3 :tool-uses (("memory_search" (query . "x"))))
        (:text "done"))
    (composition--with-restored-composition
      (let* ((superchat-llm-tool-names
              (append superchat-llm-default-tool-names (list "memory_search")))
             (view (smoke--open-chat))
             (run (smoke--send view "search memory")))
        (should (stringp run))
        (should (smoke--wait
                 (lambda () (member run (superchat-runtime-active-runs)))
                 10))
        ;; Mounted when the request went out; the call arrives after this.
        (should (superchat-tools-builtin-descriptor-p "memory_search"))
        (superchat-plugin-unmount 'memory)
        (should-not (superchat-tools-builtin-descriptor-p "memory_search"))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (let* ((session (buffer-local-value 'superchat--session-id view))
               (events (superchat-tape-replay-events session))
               (failures (cl-remove-if-not
                          (lambda (event)
                            (and (equal "tool.failed" (plist-get event :type))
                                 (equal "memory_search"
                                        (plist-get (plist-get event :payload)
                                                   :tool-name))))
                          events)))
          (should (= 1 (length failures)))
          ;; The body never started, and the refusal reached the model.
          (should-not (cl-find-if
                       (lambda (event)
                         (equal "tool.execution.started" (plist-get event :type)))
                       events))
          (should (string-match-p
                   "memory row is not mounted"
                   (plist-get (car smoke--requests) :prompt-text))))))))

(provide 'composition-regression)
;;; composition-regression.el ends here
