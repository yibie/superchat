;;; superchat-select-tools.el --- Default tools and request candidate validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Core builds explicit or surface-baseline candidates.  The Runtime request
;; waterfall replaces the old tool-selection plugin competition;
;; final validation retains explicit ceilings and prepared-tool trust.
;; The model transport materializes only the validated selection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-event)

(declare-function superchat-get-llm-tools "superchat-tools" (&optional tool-names input-operation))
(declare-function superchat-tools--visible-installation
                  "superchat-tools" (name &optional scope))
(declare-function superchat-authority-effect-resolver-p
                  "superchat-authority" (tool-name &optional scope))
(declare-function llm-tool-name "llm" (tool))
(defvar superchat-authority-unprompted-read-tools)
(defvar superchat-buffer-name)
(defvar superchat-llm-tool-names)
(defvar superchat-llm-synthesized-tools)
(defvar superchat--active-plan)

(defcustom superchat-llm-tools-enabled 'on-demand
  "Policy gating the surface baseline in `superchat-select-tools' (contract
§4.2 rule 4).

Attaching tools forces llm.el into multi-output / tool-calling mode,
which noticeably increases time-to-first-token because the model must
inspect every tool schema before responding.

Allowed values:
  nil                  -- the surface baseline is always `none' (chat-only,
                           no tools attached to an unrestricted request).
  `always' / t          -- the surface baseline applies normally.
  `on-demand' (default) -- kept only for compatibility with saved custom
                           values; behaves exactly like `always'.  Superchat
                           used to gate baseline attachment on a keyword
                           heuristic scanning the input for tool intent
                           (`#'-style file references, `render_ui',
                           `eshell-command', an active slash command); D14
                           showed that heuristic dead weight (it hid a
                           policy decision instead of skipping real work),
                           so it was deleted -- see approved decision 3.

This policy never overrides an explicit `:tools' request restriction or a
request waterfall override;
it only decides whether the surface baseline (rule 3) is `none' or its
normal tool set."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "On demand (compatibility alias for Always)" on-demand)
                 (const :tag "Always" always))
  :group 'superchat)

(declare-function superchat-llm-default-tool-names "superchat-tools")

(defun superchat--select-tools-baseline-names ()
  "Return the fixed surface baseline for rule 3 of contract §4.2.
The list is `superchat-llm-default-tool-names', read rather than copied: the
baseline and the variable's default are the same five names, and a second
hand-written copy is what let them drift out of order before.  Falls back to
the literal five when `superchat-tools' is not loaded yet, because this module
loads first."
  (if (boundp 'superchat-llm-default-tool-names)
      superchat-llm-default-tool-names
    '("list_buffers" "apropos" "read_buffer" "edit_buffer" "eval_elisp")))

(defconst superchat--select-tools-none-surfaces
  '(inline-rewrite org-babel magit skills compaction)
  "Surfaces whose contract §4.2 rule-3 baseline is always `none'.")

(defun superchat--tool-descriptor-p (name)
  "Return non-nil when NAME is a tool descriptor (contract §4.2).
It is a name string with a registered Authority effect resolver, or the
built-in `\"ask_user\"', whose effects Authority resolves specially as trusted
human input rather than through an ordinary resolver."
  (and (stringp name)
       (or (equal name "ask_user")
           (and (fboundp 'superchat-authority-effect-resolver-p)
                (superchat-authority-effect-resolver-p name)))))

(defun superchat--select-tools-custom-baseline-names (operation)
  "Return (NAMES . STRUCTS) for the chat/workflow/subagent/external/plugin
baseline (contract §4.2 rule 3).  NAMES includes `\"ask_user\"' when
OPERATION is non-nil; STRUCTS never does (see below) and is nil when
`superchat-get-llm-tools' is unavailable.

STRUCTS is the one `(superchat-get-llm-tools)' query this decision makes,
returned so `superchat--collect-llm-tools' can materialize this baseline
from it directly instead of querying the registry a second time (S09-fix-2
items 1 and 2's own \"querying the registry once\" requirement -- S09's
whole motivation was exactly the opposite: avoiding a repeated tool-set
walk, which a connected MCP server turns into a repeated network
round-trip, per this file's `superchat--llm-generate-answer' commentary).

NAMES is **not** every name that query returns (S09-fix-3 item 1, the
re-review's Critical finding: a registry holding an extra descriptor --
a test double, a connected MCP server's tool the user never configured --
must not silently widen the baseline).  It is `superchat-llm-tool-names'
configured value, filtered to that one query's own descriptor names, so
the baseline is exactly what the user configured and nothing the registry
merely happens to also hold -- **except a plugin-registered tool, which
NAMES always keeps regardless of that configured value** (S09-fix-5 item
3, contract §4.2 rule 3 as amended by the user: \"tools a plugin
registered through `superchat-register-tool' are offered by default ...
loading the plugin is the user's opt-in; Authority still gates every
call\"):
- `all' still means every registered descriptor -- NAMES is the query's
  names verbatim, unfiltered (rule 3's own \"`all' still means every
  registered descriptor\").
- A list value (the common case, including the five-name default) is
  filtered to the query's names, a configured built-in name the registry
  does not actually hold silently dropping -- but a name the query
  returns is kept even when the configured list omits it, as long as
  the visible Scope tool layer (soft: this file does not require
  `superchat-tools') has an installation for it, i.e. it is plugin-registered,
  not merely a built-in the registry happens to also hold.  The query can
  never *add* an unconfigured *built-in* name.
- Any other, non-list value (an unknown policy symbol) falls back to the
  fixed `superchat--select-tools-baseline-names' five, likewise filtered
  to the query's names with the same plugin-tool exception, and signals
  nothing (S09-fix-3 item 3).
Filtering the materializer's result by NAMES therefore stays a safe no-op
whenever the registry matches the configuration, and a real, correct
restriction the moment the registry and the configuration diverge in
either direction (fewer or more built-in descriptors) -- unlike either
prior attempt, which either could not see an addition to a customized
registry at all (S09-fix-1), let one silently widen the baseline
(S09-fix-2), or silently dropped an unclaimed plugin tool the registry
query itself already exposed (S09-fix-3/4, the `human_input_read'
regression S09-fix-5 item 3 corrects).

OPERATION only gates whether `\"ask_user\"' is *named* here, exactly as
rule 3 states (\"plus ask_user when the envelope carries an input
operation\") -- it is never passed to `superchat-get-llm-tools', which
would build (and strictly validate against the live Run) a real
trusted-input `ask_user' descriptor from it; a decision only names that
addition, and `superchat--collect-llm-tools' builds the real descriptor
separately (via `superchat-tools--make-ask-user', not a second registry
query) at materialization time, same validation timing as before this
fix."
  ;; Trusts the query's own names without a `superchat--tool-descriptor-p'
  ;; re-check: `superchat-register-tool' already requires EFFECTS (an
  ;; Authority resolver) for every registered tool, so a genuine registry
  ;; query cannot return a name lacking one -- the same trust
  ;; materialization has always placed in this exact query.  Re-filtering
  ;; here would instead reject a test double's own synthetic tool names,
  ;; which legitimately have no entry in the real
  ;; Authority Scope resolver layer.
  (let* (;; `superchat-llm-tool-names' is a `superchat-tools.el' defcustom;
         ;; this file does not require that file (soft dependency, like
         ;; `superchat--last-surface' elsewhere here), so a headless
         ;; caller with only `superchat-llm' loaded leaves it unbound --
         ;; captured once via `bound-and-true-p' instead of read directly,
         ;; so this function never signals `void-variable' there (`nil'
         ;; is also the correct "no configured list" value: QUERY-NAMES
         ;; below is nil too in that case, since `superchat-get-llm-tools'
         ;; is equally unavailable).
         (policy (bound-and-true-p superchat-llm-tool-names))
         ;; `superchat--llm-tool-enabled-p' (superchat-tools.el) does
         ;; `(member name superchat-llm-tool-names)' with no `listp' guard
         ;; of its own, so an unknown non-list policy value (a stray
         ;; symbol) would crash the registry query itself with
         ;; `(wrong-type-argument listp ...)' before this function ever
         ;; got a chance to fall back (S09-fix-3 item 3, the re-review's
         ;; `unknown-policy' reproduction).  Query under `all' instead in
         ;; that one case -- a strict superset of every real baseline --
         ;; so QUERY-NAMES below is always complete; CONFIGURED then
         ;; still narrows it to the fixed five, exactly as the honest
         ;; list/`all' cases narrow it to their own configuration.
         (query-policy (if (or (eq policy 'all) (listp policy)) policy 'all))
         (structs (if (fboundp 'superchat-get-llm-tools)
                      (let ((superchat-llm-tool-names query-policy))
                        (superchat-get-llm-tools))
                    nil))
         (query-names (mapcar #'llm-tool-name structs))
         ;; A plugin-registered name always survives the configured-names
         ;; filter (S09-fix-5 item 3).  The soft Scope-view helper keeps this
         ;; module independently loadable while avoiding a global mirror.
         (plugin-registered-p
          (lambda (name)
            (and (fboundp 'superchat-tools--visible-installation)
                 (superchat-tools--visible-installation name))))
         (configured
          (cond
           ((eq policy 'all) query-names)
           ((listp policy)
            (cl-remove-if-not
             (lambda (name)
               (or (member name policy) (funcall plugin-registered-p name)))
             query-names))
           (t (cl-remove-if-not
               (lambda (name)
                 (or (member name (superchat--select-tools-baseline-names))
                     (funcall plugin-registered-p name)))
               query-names))))
         (names (if (and operation (not (member "ask_user" configured)))
                    (append configured (list "ask_user"))
                  configured)))
    (cons names structs)))

(defun superchat--select-tools-baseline (request)
  "Return the contract §4.2 rules 3-4 surface baseline for REQUEST: `none',
or a cons `(NAMES . QUERY-INFO)'.  QUERY-INFO is nil for `inline-ask' (no
registry query is ever made for that bucket), or a plist `(:queried t
:structs STRUCTS-OR-NIL)' for the general bucket -- STRUCTS-OR-NIL is the
one registry query `superchat--select-tools-custom-baseline-names' already
made, and the explicit `:queried' key is what lets
`superchat-select-tools-decision' tell \"the query ran and returned an
empty STRUCTS list\" apart from \"no query was made\" (S09-fix-5 item 1: a
bare STRUCTS-OR-NIL cons cannot make that distinction, since both cases
look like nil).
`superchat-llm-tools-enabled' nil disables every baseline regardless of
surface (rule 4); any other value, including the legacy `on-demand' (kept
only for compatibility), behaves like `always'."
  (if (null superchat-llm-tools-enabled)
      'none
    (let ((surface (plist-get request :surface))
          (operation (plist-get (plist-get request :input-core) :operation)))
      (cond
       ((memq surface superchat--select-tools-none-surfaces) 'none)
       ((eq surface 'inline-ask)
        (cons (cl-remove-if-not #'superchat--tool-descriptor-p
                                 (bound-and-true-p superchat-authority-unprompted-read-tools))
              nil))
       (t (let* ((result (superchat--select-tools-custom-baseline-names operation))
                 (names (superchat--select-tools-surface-filter (car result) surface)))
            (cons names (list :queried t :structs (cdr result)))))))))

(defun superchat--select-tools-surface-filter (names surface)
  "Drop from NAMES every plugin tool registered with `:surfaces' that
excludes SURFACE (see `superchat-register-tool')."
  (cl-remove-if
   (lambda (name)
     (let* ((installation
             (and (fboundp 'superchat-tools--visible-installation)
                  (superchat-tools--visible-installation name)))
            (surfaces (plist-get installation :surfaces)))
       (and surfaces (not (memq surface surfaces)))))
   names))

(defun superchat--select-tools-adjust-names (names reason input)
  "Apply the three decision-time adjustments contract §4.2 rule 3 keeps,
moved here from the materializer (S09-fix-3 item 2): the decision itself
must already contain the final names, since nothing may add or remove a
name afterward.  NAMES is the base `explicit'/`stage'/`baseline' decision
\(never `none' -- the caller only applies this when NAMES is a real list);
REASON is that decision's own reason; INPUT is the raw prompt text, read
only for the `render_ui' keyword check.  Each rule keeps its exact
pre-S09-fix-3 condition:

1. A direct `render_ui' request has a single, audited UI path: `define_tool'
   is dropped so an older synthesized tool, or the self-extension
   primitive itself, cannot compete with it.  Applies regardless of
   REASON.
2. `report_plan_progress' is added while the session's approved plan
   governs this request, unless REASON is `explicit' (an exact
   request-local profile -- approval clears the planning preset, so
   execution requests reach this branch) or `render_ui' intent is active
   -- and only when the materializer can actually build the tool
   (`llm-make-tool'/`superchat--report-plan-progress-spec' both fbound,
   S09-fix-5 item 2: the decision must never name a tool it cannot
   materialize, the same guard `superchat--collect-llm-tools' already
   applies at construction time, checked here too rather than only
   there).
3. Every session-synthesized tool's name (`superchat-llm-synthesized-tools')
   is added, under the same two exclusions as rule 2."
  (let* ((render-ui-intent (superchat--render-ui-intent-p input))
         (names (if render-ui-intent (remove "define_tool" names) names)))
    (if (or (eq reason 'explicit) render-ui-intent)
        names
      (when (and (not (member "report_plan_progress" names))
                 (fboundp 'llm-make-tool)
                 (fboundp 'superchat--report-plan-progress-spec)
                 (boundp 'superchat-buffer-name)
                 superchat-buffer-name
                 (boundp 'superchat--active-plan)
                 (buffer-live-p (get-buffer superchat-buffer-name))
                 (buffer-local-value
                  'superchat--active-plan
                  (get-buffer superchat-buffer-name)))
        (setq names (append names (list "report_plan_progress"))))
      (dolist (tool (bound-and-true-p superchat-llm-synthesized-tools))
        (let ((name (llm-tool-name tool)))
          (unless (member name names)
            (setq names (append names (list name))))))
      names)))

(defun superchat-select-tools-decision (request _state)
  "Build REQUEST's default tool candidate without invoking plugin code.
Explicit restrictions win; otherwise use the surface baseline.  Request
waterfall middleware may subsequently replace the candidate, subject to the
original explicit restriction and the ordinary Authority admission boundary."
  (let* ((explicit (plist-get request :tools))
         (decision
          (cond
           ((eq explicit 'none) (list :names 'none :reason 'explicit))
           ((and explicit (listp explicit))
            (let (kept)
              (dolist (name explicit)
                (if (superchat--tool-descriptor-p name)
                    (push name kept)
                  (superchat-event-record-fault
                   'select-tools 'select-tools-invalid-name nil
                   (list :dropped-name name))))
              (list :names (nreverse kept) :reason 'explicit)))
           (t
            (let ((baseline (superchat--select-tools-baseline request)))
              (if (eq baseline 'none)
                  (list :names 'none :reason 'baseline)
                (append (list :names (car baseline) :reason 'baseline)
                        (when (plist-get (cdr baseline) :queried)
                          (list :materialized
                                (plist-get (cdr baseline) :structs))))))))))
    (unless (eq (plist-get decision :names) 'none)
      (plist-put decision :names
                 (superchat--select-tools-adjust-names
                  (plist-get decision :names)
                  (plist-get decision :reason)
                  (plist-get request :input))))
    decision))

(defun superchat--request-waterfall (scope candidate restriction prepared-p)
  "Rewrite CANDIDATE once in SCOPE and validate the complete result.
Only prompt, system text and tool names are writable.  RESTRICTION is nil
for a default candidate, or (NAMES) for an explicit ceiling (including an
empty list).  PREPARED-P freezes the trusted prepared tool surface.
Neither the candidate nor middleware owns these constraints."
  (let* ((original (copy-tree candidate))
         (result (superchat-emit scope 'before-provider-request
                                 (copy-tree candidate))))
    (if (superchat-waterfall-refused-p result)
        result
      (unless (and (proper-list-p result) result
                   (zerop (% (length result) 2)))
        (error "Request middleware must return a request candidate"))
      (let ((seen nil))
        (cl-loop for (key value) on result by #'cddr do
                 (unless (and (memq key '(:prompt :system :tools))
                              (not (memq key seen)))
                   (error "Request middleware changed a protected or unknown field: %S" key))
                 (push key seen)
                 (setq original (plist-put original key value))))
      (unless (and (stringp (plist-get original :prompt))
                   (or (null (plist-get original :system))
                       (stringp (plist-get original :system))))
        (error "Request middleware returned invalid prompt text"))
      (let* ((raw (plist-get original :tools))
             (names (unless (eq raw 'none) raw))
             (before (plist-get candidate :tools))
             (before (unless (eq before 'none) before)))
        (unless (and (proper-list-p names) (cl-every #'stringp names)
                     (= (length names) (length (delete-dups (copy-sequence names)))))
          (error "Request middleware tools must be unique registered names"))
        (when (and prepared-p (not (equal names before)))
          (error "Request middleware cannot replace prepared tools"))
        (when (and restriction
                   (cl-some (lambda (name) (not (member name (car restriction))))
                            names))
          (error "Request middleware widened an explicit tool restriction"))
        (dolist (name names)
          (unless (or (member name before) (superchat--tool-descriptor-p name))
            (error "Request middleware selected an unknown tool: %s" name))))
      original)))

(defun superchat--request-render-tool-guidance (candidate guidance)
  "Return (CANDIDATE . GUIDANCE) with pure system blocks rerendered.
GUIDANCE pairs each currently rendered block with a pure function of final
tool names.  It is process-local preparation data, never a collector or an
additional plugin registry.  Missing blocks stay removed by middleware."
  (let ((system (or (plist-get candidate :system) ""))
        (names (plist-get candidate :tools))
        updated)
    (dolist (item guidance)
      (let* ((old (car item))
             (new (funcall (cdr item) (unless (eq names 'none) names))))
        (unless (stringp new)
          (error "Tool guidance renderer must return text"))
        (cond
         ((string-empty-p old)
          (unless (string-empty-p new)
            (setq system (concat system "\n\n" new))))
         ((string-match-p (regexp-quote old) system)
          (setq system (string-replace old new system))))
        (push (cons new (cdr item)) updated)))
    (cons (plist-put candidate :system system) (nreverse updated))))

(defun superchat--request-tool-restriction (request)
  "Return REQUEST's immutable explicit tool ceiling, or nil for a baseline."
  (let ((decision (plist-get request :tool-decision)))
    (when (if decision
              (eq (plist-get decision :reason) 'explicit)
            (plist-get request :tools))
      (let ((names (if decision (plist-get decision :names)
                     (plist-get request :tools))))
        (list (unless (eq names 'none) (copy-sequence names)))))))

(defun superchat-select-tools (request state)
  "Return REQUEST/STATE's default candidate tool names.
Returns `none' or a list of tool name strings -- the
`:names' half of `superchat-select-tools-decision', which also reports
the reason; see that function for the full decision order."
  (plist-get (superchat-select-tools-decision request state) :names))

(defun superchat--render-ui-intent-p (input)
  "Return non-nil when INPUT explicitly names the built-in `render_ui'."
  (and (stringp input)
       (string-match-p "\\_<render_ui\\_>" input)))

(superchat-define-plugin tools-baseline
  :apply #'ignore
  :inject '(tools)
  :doc "Default surface tool policy.  Core builds the default candidate;
plugins customize it through the request waterfall before final admission.")

(provide 'superchat-select-tools)
;;; superchat-select-tools.el ends here
