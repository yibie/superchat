;;; test-pipeline-gates.el --- ERT tests for turn-pipeline contract §10 static gates -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract §10 gates as ERT tests, in the style of the layering gates in
;; test-runtime.el (`runtime/kernel-requires-stay-inside-kernel',
;; `runtime/blocking-submit-is-limited-to-sync-adapters',
;; `runtime/closure-knows-literal-production-tape-event-types').  Gates 3,
;; 5, 6, 7, 8 already have behavioural coverage from earlier slices and gate
;; 2 already has a stronger existing gate (S04-fix-1's
;; `runtime/every-llm-observer-event-has-a-branch'); see
;; .scratch/core-simplification/reports/S15-report.md for the mapping
;; table.  This file implements gates 1, 4, and 9.
;;
;; Reuses `test-runtime--file-forms'/`test-runtime--walk' (test-runtime.el)
;; and `superchat-stage-test--isolated' (test-stage.el); both must already
;; be loaded -- see the load order in run-tests.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-stage)
(require 'superchat-sink)

(defconst test-pipeline-gates--core-files
  '("superchat-core.el" "superchat-preset.el"
    "superchat-db.el" "superchat-tape.el"
    "superchat-projection.el" "superchat-closure.el"
    "superchat-authority.el"
    "superchat-runtime.el" "superchat-llm.el"
    "superchat-change-set.el" "superchat-sink.el"
    "superchat-prompt.el" "superchat-stage.el"
    "superchat-pipeline.el")
  "Core, per contract §10 gate 9: the kernel file list of
`runtime/kernel-requires-stay-inside-kernel' (test-runtime.el).
`superchat-sink.el', `superchat-stage.el', `superchat-pipeline.el' and
`superchat-prompt.el' are already members of that list.")

;; The form walker (`test-runtime--file-forms') lives in test-runtime.el;
;; load it explicitly so this file also works outside test/run-tests.el.
(require 'test-runtime
         (expand-file-name "test-runtime" (file-name-directory load-file-name)))
;; `superchat-stage-test--isolated' (registry save/restore) lives in test-stage.el.
(require 'test-stage
         (expand-file-name "test-stage" (file-name-directory load-file-name)))

(defun test-pipeline-gates--core-file-path (name)
  "Expand core file NAME against the production source directory."
  (expand-file-name name (file-name-directory (locate-library "superchat-runtime"))))

;; ═══════════════════════════════════════════════════════════
;; Gate 1: thirteen stages; no production hook list stands in for a stage
;; ═══════════════════════════════════════════════════════════

(ert-deftest gates/thirteen-stages-and-no-hook-list-stage ()
  "Contract §10 gate 1.  `superchat-stage-names' is exactly the fixed
thirteen names in contract order.  `superchat-core.el' keeps exactly one
`(dolist (V (symbol-value HOOK-VAR)) ...)' loop -- `superchat-core--run-hook-chain'
itself; after S06b unregistered the built-in prompt hooks, its only
production caller for the two hook lists that used to drive prompt
building (`superchat-system-prompt-functions', `superchat-build-prompt-functions')
is `superchat-prompt-collector--legacy-bridge' (contract §4.3's Legacy
bridge algorithm, itself registered as an ordinary `collect-context'
contributor -- not a hook list standing in for a stage).
`superchat-pre-topic-functions'/`superchat-post-topic-functions' are a
separate, pre-stage Turn-lifecycle hook pair the contract never names as
one of the thirteen stages, so callers of `superchat-core--run-hook-chain'
for those two lists are out of this gate's scope."
  (should (equal superchat-stage-names
                 '(resolve-session admit load-state select-tools
                   collect-context assemble-prompt run-model save-state
                   render dispatch on-error register-command
                   register-surface)))
  (let ((loops 0))
    (dolist (top (test-runtime--file-forms
                  (test-pipeline-gates--core-file-path "superchat-core.el")))
      (test-runtime--walk
       top
       (lambda (list)
         (when (and (eq (car list) 'dolist)
                    (consp (cadr list))
                    (consp (cadr (cadr list)))
                    (eq (car (cadr (cadr list))) 'symbol-value))
           (cl-incf loops)))
       #'ignore))
    (should (= loops 1)))
  ;; S15-fix-1: the `run-hooks' family is the other way a hook list can be
  ;; executed as an ordered stage.  Core has none; the only sanctioned hook
  ;; execution is the legacy bridge in superchat-prompt-hooks.el (checked
  ;; below through `superchat-core--run-hook-chain').
  (let (hook-runner-calls)
    (dolist (top (test-runtime--file-forms
                  (test-pipeline-gates--core-file-path "superchat-core.el")))
      (test-runtime--walk
       top
       (lambda (list)
         (when (memq (car list) '(run-hooks run-hook-with-args
                                  run-hook-with-args-until-success
                                  run-hook-with-args-until-failure
                                  run-hook-wrapped))
           (push (cadr list) hook-runner-calls)))
       #'ignore))
    (should (null hook-runner-calls)))
  (dolist (hook-list '(superchat-system-prompt-functions
                       superchat-build-prompt-functions))
    (let (callers
          (root (file-name-directory (locate-library "superchat-runtime"))))
      (dolist (file (directory-files root t "\\.el\\'"))
        (dolist (top (test-runtime--file-forms file))
          (let ((enclosing (and (consp top) (memq (car top) '(defun cl-defun))
                                (nth 1 top))))
            (test-runtime--walk
             top
             (lambda (list)
               (when (and (eq (car list) 'superchat-core--run-hook-chain)
                          (consp (cdr list)) (consp (cddr list))
                          (equal (cadr (cddr list)) nil) ;; no extra args
                          (consp (car (cddr list)))
                          (eq (car (car (cddr list))) 'quote)
                          (eq (cadr (car (cddr list))) hook-list))
                 (push (cons (file-name-nondirectory file) enclosing) callers)))
             #'ignore))))
      (should (equal callers
                     '(("superchat-prompt-hooks.el" . superchat-prompt-collector--legacy-bridge)))))))

;; ═══════════════════════════════════════════════════════════
;; Gate 4: renderer output cannot name a Sink
;; ═══════════════════════════════════════════════════════════

(defconst test-pipeline-gates--sample-sealed-result
  (list :run-id "gate-render-probe-run" :turn-id "gate-render-probe-turn"
        :session-id "gate-render-probe-session" :surface 'chat
        :status 'completed :started-at 0 :completed-at 0
        :error nil :pending-steer-count 0 :parent-run-id nil
        :text "hello from the gate probe")
  "A representative sealed Run result, shaped like the callback-safe
projection `superchat-runtime-sink-run' documents -- no buffer, marker,
function, timer, process or live Sink object -- which is what contract §10
row 9 (`render' stage) says a render implementation receives.")

(defun test-pipeline-gates--plist-names-a-sink-p (value)
  "Return non-nil if VALUE -- a render-implementation result -- names a
Sink anywhere: a `:sink' key at any depth (top level, `:payload', nested
lists) or a `superchat-sink' struct value at any depth, vectors included
(S15-fix-1: the renderer shape is `(:renderer-id SYM :payload PLIST)', so a
Sink under `:payload' names a Sink just as much as one at the top)."
  (cond
   ((superchat-sink-p value) t)
   ((eq value :sink) t)
   ((consp value)
    (or (test-pipeline-gates--plist-names-a-sink-p (car value))
        (test-pipeline-gates--plist-names-a-sink-p (cdr value))))
   ((vectorp value)
    (cl-some #'test-pipeline-gates--plist-names-a-sink-p value))
   (t nil)))

(defun test-pipeline-gates--render-results-naming-a-sink ()
  "Return the ids of registered `render' implementations whose result names
a Sink -- the one check gate 4 and its companion proof both run."
  (cl-loop for impl in (plist-get (superchat-stage--entry 'render) :implementations)
           when (test-pipeline-gates--plist-names-a-sink-p
                 (funcall (plist-get impl :function)
                          test-pipeline-gates--sample-sealed-result nil))
           collect (plist-get impl :id)))

(ert-deftest gates/render-stage-results-carry-no-sink ()
  "Contract §10 gate 4.  Every currently-registered `render' stage
implementation's result names no Sink, called with a representative sealed
result.  Production registers zero `render' implementations as of this
slice -- Chat View rendering still runs directly through
`superchat-runtime-view-sink-function' (S01/S02), not the stage registry,
matching `save-state'/`dispatch' (superchat-stage.el's `core-seal'/`core-sink'
placeholders) -- so this loop is currently vacuous; it exists so a future
slice that wires rendering through the `render' stage is caught immediately
if it violates the no-Sink-naming rule.  See
`gates/render-gate-catches-a-result-naming-a-sink' below for proof the
check itself works."
  (should (null (test-pipeline-gates--render-results-naming-a-sink))))

(ert-deftest gates/render-gate-catches-a-result-naming-a-sink ()
  "The gate's own check catches a violation at the top level and one hidden
under `:payload'.  Registers temporary `render' implementations through
`superchat-stage-test--isolated' (registry restored afterward -- never a
production file mutation) and runs the exact function the gate runs."
  (superchat-stage-test--isolated
    (superchat-stage-add
     'render 'gate-probe--top-level-sink
     (lambda (&rest _)
       (list :renderer-id 'probe :payload nil :sink (superchat-sink-create)))
     :owner 'test)
    (superchat-stage-add
     'render 'gate-probe--nested-sink
     (lambda (&rest _)
       (list :renderer-id 'probe :payload (list :sink (superchat-sink-create))))
     :owner 'test)
    (superchat-stage-add
     'render 'gate-probe--deep-struct
     (lambda (&rest _)
       (list :renderer-id 'probe
             :payload (list :items (vector (list :view (superchat-sink-create))))))
     :owner 'test)
    (superchat-stage-add
     'render 'gate-probe--clean
     (lambda (&rest _) (list :renderer-id 'probe :payload (list :text "ok")))
     :owner 'test)
    (should (equal (sort (copy-sequence (test-pipeline-gates--render-results-naming-a-sink))
                         #'string-lessp)
                   '(gate-probe--deep-struct gate-probe--nested-sink
                     gate-probe--top-level-sink)))))

;; ═══════════════════════════════════════════════════════════
;; Gate 9: core files carry no UI probe outside a reviewed allow-list
;; ═══════════════════════════════════════════════════════════

(defconst test-pipeline-gates--ui-probe-allow-list
  '((superchat-tools--external-registry "superchat-llm.el" "Plugin tool registry (`defvar' in superchat-tools.el, an optional feature the LLM core must not require -- see the eager-distribution gate); the baseline keeps plugin-registered names by user decision (contract §4.2 rule 3) and reads the registry softly so a headless caller simply sees no plugin tools." S25)
    (superchat--active-run-id "superchat-runtime.el" "Per-View buffer-local active Run id (`defvar-local' in superchat.el); Runtime's timeout-cancellation path reads it defensively for callers that still run without a View." S25)
    (superchat--agent-check-permission "superchat-authority.el" "Optional Agent-loop plugin permission predicate (superchat-agent-loop.el); Authority's legacy decision path runs without it and only defers to it when the plugin is loaded." S25)
    (superchat--agent-confirm-p "superchat-authority.el" "Optional Agent-loop plugin confirmation predicate (superchat-agent-loop.el); Authority's effective-decision path falls back to its own default when the plugin is absent." S25)
    (superchat--agent-filter-tool-result "superchat-authority.el" "Optional Agent-loop plugin result filter (superchat-agent-loop.el); Authority passes the raw result through unfiltered when the plugin is absent." S25)
    (superchat--agent-intercept-tool-call "superchat-authority.el" "Optional Agent-loop plugin call interceptor (superchat-agent-loop.el); Authority skips interception when the plugin is absent." S25)
    (superchat--agent-run-tool-hooks "superchat-authority.el" "Optional Agent-loop plugin pre/post-tool hook runner (superchat-agent-loop.el); Authority runs no hooks when the plugin is absent." S25)
    (superchat--agent-make-wrapper "superchat-llm.el" "Optional Agent-loop plugin tool-call wrapper (superchat-agent-loop.el); the LLM loop calls tools unwrapped when the plugin is absent." S25)
    (superchat-agent-post-tool-failure-functions "superchat-authority.el" "Optional Agent-loop plugin hook list (superchat-agent-loop.el); Authority notifies no post-tool-failure hooks when the plugin is absent." S25)
    (superchat-agent-post-tool-functions "superchat-authority.el" "Optional Agent-loop plugin hook list (superchat-agent-loop.el); Authority notifies no post-tool hooks when the plugin is absent." S25)
    (superchat-agent-pre-tool-functions "superchat-authority.el" "Optional Agent-loop plugin hook list (superchat-agent-loop.el); Authority invokes no pre-tool hooks when the plugin is absent." S25)
    (superchat--extend-timeout "superchat-authority.el" "Optional View timeout-extension entry point (superchat.el); Authority's confirm path skips extending the transport timeout when no View is loaded." S25)
    (superchat--extract-file-path "superchat-core.el" "Optional #file reference plugin entry point (superchat-dispatcher.el); Core's input parse step leaves context-files unset when the plugin is absent." S25)
    (superchat--invalidate-model-cache "superchat-llm.el" "Optional Models plugin cache-invalidation hook (superchat-models.el), called from a defcustom's :set function only when that plugin is loaded." S25)
    (superchat--last-surface "superchat-llm.el" "Per-View buffer-local surface tag (`defvar-local' in superchat.el); the LLM port reads it defensively to fill the audit `:surface' field when no View is attached." S25)
    (superchat--pause-boundary "superchat-llm.el" "Optional View pause-boundary entry point (superchat.el); the LLM loop's pause check is a no-op when no View is loaded." S25)
    (superchat--pause-requested "superchat-llm.el" "Per-View buffer-local pause flag (`defvar-local' in superchat.el); the LLM loop checks it defensively for callers that still run without a View." S25)
    (superchat--pause-run-id "superchat-llm.el" "Per-View buffer-local pause target (`defvar-local' in superchat.el); the LLM loop checks it defensively for callers that still run without a View." S25)
    (superchat--report-plan-progress-spec "superchat-llm.el" "Optional Tools plugin tool spec (superchat-tools.el); the LLM port's tool collection omits it when the plugin is absent." S25)
    (superchat--response-region-close "superchat-llm.el" "Optional render/View plugin entry point (superchat-render.el); the LLM loop closes the response region only when a View owns the buffer." S25)
    (superchat--syn-bind "superchat-llm.el" "Optional Synthesis plugin binder (superchat-synthesis.el); the LLM loop skips synthesis binding when the plugin is absent." S25)
    (superchat--syn-tool-p "superchat-llm.el" "Optional Synthesis plugin predicate (superchat-synthesis.el); evidence-tool validation treats no tool as a synthesis tool when the plugin is absent." S25)
    (superchat--active-plan "superchat-llm.el" "Per-View buffer-local active-plan state (`defvar-local' in superchat-plan.el); the LLM port's tool-name adjustment reads it defensively when the Plan plugin is absent." S25)
    (superchat--active-plan "superchat-preset.el" "Per-View buffer-local active-plan state (`defvar-local' in superchat-plan.el); Preset's active-plan check reads it defensively when the Plan plugin is absent." S25)
    (superchat-authority--session-allowlist "superchat-authority.el" "Per-View buffer-local session allow-list (`defvar-local' in superchat.el); Authority reads it defensively for callers that still run without a View." S25)
    (superchat-authority-mode "superchat-authority.el" "Per-View buffer-local authority mode (`defvar-local' in superchat.el); Authority's effective-decision path falls back to its own default when no View is attached." S25)
    (superchat-experience-assemble-recovery-view "superchat-runtime.el" "Optional Experience plugin recovery-View builder (superchat-experience.el); Runtime's context-recovery path only calls it when the plugin is loaded." S25)
    (superchat-get-llm-tools "superchat-llm.el" "Optional Tools plugin entry point (superchat-tools.el); the LLM port collects no plugin tools when it is absent." S25)
    (superchat-llm-synthesized-tools "superchat-llm.el" "Optional Synthesis plugin tool list (superchat-synthesis.el); the LLM port injects no synthesized tools when the plugin is absent." S25)
    (superchat-llm-tool-names "superchat-llm.el" "Optional Extension/Tools plugin custom baseline-name list (superchat-extension.el/superchat-tools.el); the LLM port falls back to its own default names when absent." S25)
    (superchat-parser-command "superchat-core.el" "Optional input-parsing plugin (superchat-parser.el); Core's parse step leaves the /command field unset when the plugin is absent." S25)
    (superchat-parser-model-switch "superchat-core.el" "Optional input-parsing plugin (superchat-parser.el); Core's parse step leaves the @model field unset when the plugin is absent." S25)
    (superchat-skills-parse-input "superchat-core.el" "Optional Skills plugin (superchat-skills.el); Core's parse step leaves the >skill field unset when the plugin is absent." S25)
    (superchat-tools--make-ask-user "superchat-llm.el" "Optional Tools plugin ask-user tool builder (superchat-tools.el); the LLM port omits the ask-user tool when the plugin is absent." S25)
    (superchat-tools--trusted-ask-descriptor-p "superchat-llm.el" "Optional Tools plugin descriptor predicate (superchat-tools.el); the LLM loop treats no descriptor as trusted when the plugin is absent." S25)
    (superchat--llm-generate-answer "superchat-runtime.el" "Kernel-internal lazy-load guard (superchat-llm.el is required, but Runtime checks readiness before its two direct calls rather than assuming load order); leader note after S01." S25)
    (superchat--clear-pause-state "superchat-runtime.el" "Optional View pause-clear entry point (superchat.el); Runtime's quiesce path clears no pause state when no View is loaded; leader note after S01." S25)
    (superchat-buffer-name "superchat-llm.el" "Core-defined (`defcustom' in this same file) but conceptually a View concern: read to locate a live chat buffer for a legacy tool descriptor, and dynamically rebound around a dead-callback invocation so the callback's buffer-name matches the caller's buffer, not the View's." S25)
    (superchat-buffer-name "superchat-preset.el" "Read to find the live chat buffer whose active-plan state to check; a View/buffer-naming concern reached from Preset." S25)
    (superchat-buffer-name "superchat-runtime.el" "Dynamically rebound around running post-response Turn hooks and around submitting an LLM request, so legacy hook/tool code that reads `superchat-buffer-name' sees the concrete View buffer's name; the Sink-based path does not need this once legacy hooks are gone." S25))
  "Contract §10 gate 9's reviewed exceptions: every remaining `superchat-'
`fboundp'/`boundp'/`bound-and-true-p' probe in a core file, and every
`superchat-buffer-name' occurrence in a core file outside its own
`defcustom' declaration, that is not itself defined by a core file (see
`test-pipeline-gates--core-defined-symbols').  Each row is
`(SYMBOL FILE REASON REMOVAL-SLICE)'.  Two rows
\(`superchat--llm-generate-answer', `superchat--clear-pause-state') name a
symbol a core file DOES define -- included anyway per the leader note
after S01, which asked for both to be allow-listed by name.")

(defun test-pipeline-gates--allow-listed-p (symbol file)
  "Return non-nil when SYMBOL/FILE has a reviewed exception."
  (cl-some (lambda (entry) (and (eq (nth 0 entry) symbol) (equal (nth 1 entry) file)))
           test-pipeline-gates--ui-probe-allow-list))

(defun test-pipeline-gates--own-defined-symbols (forms)
  "Symbols FORMS (one file's top-level forms) really define, discovered by
shape rather than a hand-copied list -- the same spirit as
`test-runtime--discover-file-writers'.  `defun'/`cl-defun'/`defmacro'/
`cl-defgeneric'/`cl-defmethod' always count.  `defvar'/`defvar-local'/
`defcustom'/`defconst' count only when they carry a value, so a bare
forward declaration such as `(defvar SYM)' -- a compiler hint, not a
definition -- is not mistaken for a real definition."
  (let (out)
    (dolist (form forms)
      (when (consp form)
        (pcase (car form)
          ((or 'defun 'cl-defun 'defmacro 'cl-defgeneric 'cl-defmethod)
           (push (nth 1 form) out))
          ((or 'defvar 'defvar-local 'defcustom 'defconst)
           (when (> (length form) 2) (push (nth 1 form) out))))))
    out))

(defconst test-pipeline-gates--core-defined-symbols
  (let (all)
    (dolist (file test-pipeline-gates--core-files)
      (let ((path (test-pipeline-gates--core-file-path file)))
        (when (file-exists-p path)
          (setq all (append (test-pipeline-gates--own-defined-symbols
                              (test-runtime--file-forms path))
                             all)))))
    all)
  "Every symbol any core file really defines (see
`test-pipeline-gates--own-defined-symbols').  A core file's `fboundp'/
`boundp'/`bound-and-true-p' probe of a symbol in this set is a
Kernel-internal load-order guard, not a probe of \"a non-core symbol\"
\(contract §10 gate 9) -- for example `superchat-runtime.el' checking
`superchat-stage-record-fault' is `fboundp' before calling into
`superchat-stage.el', both core.  It needs no allow-list entry.")

(defun test-pipeline-gates--superchat-probe-symbol (list)
  "Return the probed symbol if LIST is `(fboundp (quote SYM))',
`(boundp (quote SYM))', or `(bound-and-true-p SYM)' for a `superchat-'
SYM; nil otherwise."
  (cond
   ((and (memq (car list) '(fboundp boundp))
         (consp (cdr list)) (consp (cadr list)) (eq (car (cadr list)) 'quote)
         (symbolp (cadr (cadr list)))
         (string-prefix-p "superchat-" (symbol-name (cadr (cadr list)))))
    (cadr (cadr list)))
   ((and (eq (car list) 'bound-and-true-p) (symbolp (cadr list))
         (string-prefix-p "superchat-" (symbol-name (cadr list))))
    (cadr list))))

(ert-deftest gates/core-files-have-no-ui-probes-outside-allow-list ()
  "Contract §10 gate 9 (the `fboundp'/`boundp'/`bound-and-true-p' half, and
the `superchat-buffer-name' half; direct buffer insertion is out of this
gate's scope).  Every core file's `superchat-' probe either names a
symbol a core file itself defines
\(`test-pipeline-gates--core-defined-symbols') or has a reviewed exception
in `test-pipeline-gates--ui-probe-allow-list'; every core file's
`superchat-buffer-name' occurrence, outside its own `defcustom'
declaration, has a reviewed exception too.  See
`gates/ui-probe-gate-catches-an-unlisted-probe' for proof an unlisted
probe is actually caught."
  (let (violations)
    (dolist (file test-pipeline-gates--core-files)
      (let* ((path (test-pipeline-gates--core-file-path file))
             (forms (test-runtime--file-forms path)))
        (dolist (top forms)
          (unless (and (consp top) (memq (car top) '(defcustom defvar))
                       (eq (nth 1 top) 'superchat-buffer-name))
            (test-runtime--walk
             top
             (lambda (list)
               (let ((sym (test-pipeline-gates--superchat-probe-symbol list)))
                 (when (and sym
                            (not (memq sym test-pipeline-gates--core-defined-symbols))
                            (not (test-pipeline-gates--allow-listed-p sym file)))
                   (push (list sym file) violations))))
             (lambda (atom)
               (when (and (eq atom 'superchat-buffer-name)
                          (not (test-pipeline-gates--allow-listed-p
                                'superchat-buffer-name file)))
                 (push (list 'superchat-buffer-name file) violations))))))))
    (should-not violations)))

(ert-deftest gates/ui-probe-gate-catches-an-unlisted-probe ()
  "The check above actually flags a `superchat-' probe that names neither
a core-defined symbol nor an allow-list entry, proven against a scratch
probe file's symbol -- never a production file mutation."
  (let ((probe (make-temp-file "sc-gate-ui-probe-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--checks-something ()\n"
                    "  (fboundp 'superchat-totally-unlisted-symbol))\n"))
          (let* ((forms (test-runtime--file-forms probe))
                 (found nil))
            (dolist (top forms)
              (test-runtime--walk
               top
               (lambda (list)
                 (let ((sym (test-pipeline-gates--superchat-probe-symbol list)))
                   (when (eq sym 'superchat-totally-unlisted-symbol)
                     (setq found t))))
               #'ignore))
            (should found))
          (should-not
           (memq 'superchat-totally-unlisted-symbol
                 test-pipeline-gates--core-defined-symbols))
          (should-not
           (test-pipeline-gates--allow-listed-p
            'superchat-totally-unlisted-symbol (file-name-nondirectory probe))))
      (delete-file probe))))

(provide 'test-pipeline-gates)

;;; test-pipeline-gates.el ends here
