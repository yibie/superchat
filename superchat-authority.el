;;; superchat-authority.el --- Pure authority decisions for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Policy helpers plus the single model-invoked tool execution seam.  Callers
;; provide a small context plist; validation, effects, authority, approval,
;; execution, and Tape ordering stay local to this module.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-stage)
(require 'superchat-invariant)
(require 'subr-x)
(require 'superchat-tape)

(declare-function superchat--agent-intercept-tool-call
                  "superchat-agent-loop" (tool-name args))
(declare-function superchat--agent-check-permission
                  "superchat-agent-loop" (tool-name args))
(declare-function superchat--agent-confirm-p
                  "superchat-agent-loop" (tool-name args &optional guardrails))
(declare-function superchat--agent-run-tool-hooks
                  "superchat-agent-loop" (hooks &rest args))
(declare-function superchat--agent-filter-tool-result
                  "superchat-agent-loop" (tool-name args result))
(declare-function superchat--extend-timeout "superchat"
                  (&optional transport-timer-owner))
(declare-function superchat-runtime-transport-timer-owner
                  "superchat-runtime" (run-id))
(declare-function superchat-runtime--request-input "superchat-runtime" (capability question))
(declare-function superchat-runtime--trusted-input-operation-p
                  "superchat-runtime" (context function async))

(defvar superchat-tools-default-directory nil)
(defvar superchat--syn-call-confirmed)
(defvar superchat--tool-execution-context nil
  "Effective executor context visible to synchronously-created child runs.")

(defconst superchat-authority-transport-timer-owner-unavailable
  :superchat-transport-timer-owner-unavailable
  "Marker for a Runtime Run that no longer owns transport timers.")

(defun superchat-authority-transport-timer-owner (&optional context)
  "Resolve CONTEXT's opaque transport timer owner.
Return `superchat-authority-transport-timer-owner-unavailable' when CONTEXT
identifies a Runtime Run whose owner is absent.  Return nil only when there is
no Runtime Run identity, preserving the legacy non-Runtime hook behavior.
When CONTEXT is nil, use `superchat--tool-execution-context'."
  (let* ((context (or context superchat--tool-execution-context))
         (owner (and (listp context)
                     (plist-get context :transport-timer-owner)))
         (run-id (and (listp context) (plist-get context :run-id))))
    (cond
     (owner owner)
     (run-id
      (or (and (fboundp 'superchat-runtime-transport-timer-owner)
               (ignore-errors
                 (superchat-runtime-transport-timer-owner run-id)))
          superchat-authority-transport-timer-owner-unavailable))
     (t nil))))

(defgroup superchat-authority nil
  "Tool effects, policy, approval, and execution."
  :group 'superchat)

(defcustom superchat-authority-approval-function
  #'superchat-authority-confirm
  "Function called with an approval REQUEST plist.
It must return non-nil to approve this exact normalized request."
  :type 'function
  :group 'superchat-authority)

(defcustom superchat-authority-sensitive-buffers nil
  "Regexps matched against a read buffer's name and backing file truename.
When a `local-read' effect touches a buffer whose name or file truename
matches any of these regexps, Authority always asks for approval before
the read, even in a mode or under a legacy hook that would otherwise let
attended reads through without prompting."
  :type '(repeat regexp)
  :group 'superchat-authority)

(defcustom superchat-authority-prompt-for-all-reads nil
  "When non-nil, every read-only tool call asks for approval.
Reads are still audited exactly as when this variable is nil; it only
removes the exemption from the approval prompt."
  :type 'boolean
  :group 'superchat-authority)

(defcustom superchat-authority-unprompted-read-tools
  '("read_buffer" "list_buffers" "apropos" "describe-function")
  "User-owned allow-list of tool names exempt from the read approval prompt.
A tool call skips the approval prompt only when its name is a member of
this list *and* every effect the resolver returned is `local-read'; the
membership check is Authority's own, keyed on the tool name it already
receives, so nothing a resolver, a `superchat-register-tool' caller, a
plugin, or an MCP declaration puts into an effect plist can grant this
exemption on its own.

Deliberately not in the default list, so they keep asking in `ask' mode:
`eval_elisp' (a conservatively read-only expression can still call
`symbol-value' or `buffer-substring-no-properties' and expose process
data), `describe-variable' (returns the variable's live value), `sql'
\(the whole database, every Session), history/memory tools, Workspace and
team reads, LSP reads, and every MCP or plugin tool.  Widening this list
is the user's decision, one tool name at a time."
  :type '(repeat string)
  :group 'superchat-authority)

(defconst superchat-authority-policy-version "authority-v1"
  "Base policy version included in approval hashes.")

(defconst superchat-authority-known-policies
  '(default observer developer-project operator cowork)
  "Named Authority policies accepted by the Runtime and UI.
Policies are tighten-only: they may deny or defer, but never allow a tool.")

(defvar superchat-authority--tool-installation nil
  "Private Scope installation captured from this materialized tool function.

It is process-local and dynamically bound only while Authority executes or
settles that exact tool invocation.  It is never copied into an Envelope,
Tape event, or public Run status.")

(defun superchat-authority--installation-from-function (function)
  "Return FUNCTION's private materialized tool installation, if any.

Plugin tools use an uninterned callable symbol whose property carries the
installation selected when that descriptor was materialized.  A raw legacy
callable deliberately returns nil.  Reading the identity from FUNCTION (not
from TOOL-NAME and the current Scope) prevents an old descriptor from pairing
its implementation with a later same-name resolver or approval preview."
  (and (symbolp function)
       (get function 'superchat-tools--installation)))

(defun superchat-authority--policy-name (name)
  "Normalize Authority policy NAME to its symbol, or signal."
  (let ((raw (cond ((keywordp name) (substring (symbol-name name) 1))
                   ((symbolp name) (symbol-name name))
                   ((stringp name) name)
                   (t (error "Invalid authority policy name: %S" name)))))
    (intern (downcase raw))))

(defun superchat-authority--scoped-value (registry key &optional scope)
  "Return REGISTRY KEY's nearest visible Authority installation value."
  (plist-get
   (superchat-scope-lookup
    (superchat-execution-scope-resolve scope) registry key)
   :value))

(defun superchat-authority--scoped-values (registry key &optional scope)
  "Return REGISTRY KEY values from global through the effective Scope.

Unlike nearest-wins lookup, policies need every ancestor's constraint so they
can combine monotonically at Authority entry."
  (let (values)
    (dolist (layer-scope
             (superchat-scope-chain
              (superchat-execution-scope-resolve scope)))
      (when-let* ((layer (assoc registry
                                (superchat-scope-layers layer-scope)))
                  (installation (cdr (assoc key (cdr layer))))
                  (value (plist-get installation :value)))
        (setq values (append values (list value)))))
    values))

(defun superchat-authority--scoped-register (registry key value &optional scope)
  "Install VALUE for REGISTRY KEY and return its exact self-removing undo."
  (let* ((target (superchat-registration-scope-resolve scope))
         (installation (list :value value :undo nil))
         ;; Duplicate rejection happens before the layer or cleanup changes.
         (withdraw (superchat-scope-register target registry key installation))
         (undo (superchat-scope-effect
                target
                (lambda ()
                  (plist-put installation :undo nil)
                  (funcall withdraw)))))
    (plist-put installation :undo undo)
    undo))

(defun superchat-authority--scoped-unregister (registry key &optional scope)
  "Withdraw REGISTRY KEY from its local target layer only."
  (when-let* ((installation (superchat-scope-local-lookup scope registry key))
              (undo (plist-get installation :undo)))
    (funcall undo)
    t))

(defun superchat-authority--registered-policy (name &optional scope)
  "Return NAME's nearest visible custom policy decider, or nil."
  (superchat-authority--scoped-value 'authority-policy name scope))

;;;###autoload
(cl-defun superchat-authority-register-policy (name decide &key scope)
  "Register tighten-only Authority policy NAME with decider DECIDE.

NAME is a symbol or string; built-in policy names cannot be replaced.
DECIDE is called with (EFFECTS CONTEXT) — the resolved effect
descriptors and the execution context plist — and returns `deny',
`ask', or `defer'.  Named policies may only tighten the base decision,
so an `allow' result is treated as `defer'.  A duplicate NAME in one Scope
is rejected; replace it by explicitly unregistering that local installation.
Returns the normalized policy symbol."
  (unless (functionp decide)
    (error "Policy decider for %s must be callable" name))
  (let ((normalized (superchat-authority--policy-name name)))
    (when (memq normalized superchat-authority-known-policies)
      (error "Authority policy %s is built-in and cannot be replaced"
             normalized))
    (superchat-authority--scoped-register
     'authority-policy normalized decide scope)
    normalized))

;;;###autoload
(cl-defun superchat-authority-unregister-policy (name &key scope)
  "Unregister the plugin-contributed Authority policy NAME.

NAME is normalized exactly as by `superchat-authority-register-policy'.
Returns non-nil when a registry entry was removed, and nil when NAME was not
registered.  Built-in policies are not stored in this registry, so they need
no special handling and return nil here."
  (superchat-authority--scoped-unregister
   'authority-policy (superchat-authority--policy-name name) scope))

(defconst superchat-authority--decision-rank
  '((deny . 3) (ask . 2) (allow . 1) (defer . 0))
  "Decision precedence.  Higher rank wins.")

(defun superchat-authority--valid-attendance-p (attendance)
  "Return non-nil when ATTENDANCE is explicit and known."
  (memq attendance '(attended unattended)))

(defun superchat-authority--check-attendance (attendance)
  "Signal unless ATTENDANCE is `attended' or `unattended'."
  (unless (superchat-authority--valid-attendance-p attendance)
    (error "attendance must be `attended' or `unattended', got %S" attendance)))

;;;###autoload
(defun superchat-authority-normalize-policy (&optional policy)
  "Return normalized Authority POLICY, or signal for unknown policies.
Nil means `default'.  Accepted policy names are symbols, keywords, or strings
from `superchat-authority-known-policies'."
  (let* ((raw (or policy 'default))
         (name (cond
                ((keywordp raw) (substring (symbol-name raw) 1))
                ((symbolp raw) (symbol-name raw))
                ((stringp raw) raw)
                (t (error "unknown authority policy: %S" policy))))
         (normalized (intern (downcase name))))
    (unless (or (memq normalized superchat-authority-known-policies)
                (superchat-authority--registered-policy normalized))
      (error "unknown authority policy: %S" policy))
    normalized))

;;;###autoload
(defun superchat-authority-policy-known-p (policy)
  "Return non-nil when POLICY names a supported Authority policy."
  (condition-case nil
      (progn (superchat-authority-normalize-policy policy) t)
    (error nil)))

;;;###autoload
(defun superchat-authority-policy-version-for (&optional policy)
  "Return the approval-hash policy version string for POLICY."
  (format "%s/%s"
          superchat-authority-policy-version
          (superchat-authority-normalize-policy policy)))

(defun superchat-authority--key-string (key)
  "Return deterministic string form of KEY."
  (cond ((keywordp key) (substring (symbol-name key) 1))
        ((symbolp key) (symbol-name key))
        ((stringp key) key)
        (t (prin1-to-string key))))

(defun superchat-authority--plist-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (consp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun superchat-authority--alist-p (value)
  "Return non-nil when VALUE looks like an alist."
  (and (consp value)
       (cl-every (lambda (item) (and (consp item) (not (listp (car item)))))
                 value)))

(defun superchat-authority--normalize-map (pairs)
  "Normalize PAIRS, sorting by deterministic key string."
  (cons
   :object
   (sort
    (mapcar (lambda (pair)
              (cons (superchat-authority--key-string (car pair))
                    (superchat-authority-normalize-args (cdr pair))))
            pairs)
    (lambda (a b) (string< (car a) (car b))))))

;;;###autoload
(defun superchat-authority-normalize-args (args)
  "Return deterministic, printable normalized ARGS.
Plists, alists, and hash tables become sorted object forms; ordinary lists
preserve order because positional tool arguments are order-sensitive."
  (cond
   ((null args) nil)
   ((hash-table-p args)
    (let (pairs)
      (maphash (lambda (key value) (push (cons key value) pairs)) args)
      (superchat-authority--normalize-map pairs)))
   ((vectorp args)
    (cons :vector
          (mapcar #'superchat-authority-normalize-args (append args nil))))
   ((superchat-authority--plist-p args)
    (let (pairs)
      (while args
        (push (cons (pop args) (pop args)) pairs))
      (superchat-authority--normalize-map pairs)))
   ((superchat-authority--alist-p args)
    (superchat-authority--normalize-map args))
   ((consp args)
    (mapcar #'superchat-authority-normalize-args args))
   ((symbolp args)
    (list :symbol (symbol-name args)))
   (t args)))

(defun superchat-authority--arg (args key index)
  "Return argument KEY or positional INDEX from ARGS."
  (cond
   ((hash-table-p args) (gethash key args))
   ((superchat-authority--plist-p args)
    (or (plist-get args (intern (concat ":" key)))
        (plist-get args (intern key))
        (plist-get args key)))
   ((superchat-authority--alist-p args)
    (cdr (or (assoc key args)
             (assoc (intern key) args)
             (assoc (intern (concat ":" key)) args))))
   ((listp args) (nth index args))
   (t nil)))

;;;###autoload
(defun superchat-authority-effect-argument (args key index)
  "Return named KEY or positional INDEX from tool ARGS."
  (superchat-authority--arg args key index))

(defun superchat-authority--snapshot-value (value)
  "Return a detached copy of mutable JSON-like VALUE."
  (cond
   ((stringp value) (copy-sequence value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash (superchat-authority--snapshot-value key)
                  (superchat-authority--snapshot-value item)
                  copy))
       value)
      copy))
   ((vectorp value)
    (apply #'vector
           (mapcar #'superchat-authority--snapshot-value (append value nil))))
   ((consp value)
    (cons (superchat-authority--snapshot-value (car value))
          (superchat-authority--snapshot-value (cdr value))))
   (t value)))

(defun superchat-authority--effect (effect resource &optional scope)
  "Return one effect descriptor."
  (append (list :effect effect :resource resource)
          (when scope (list :scope scope))))

;;;###autoload
(cl-defun superchat-authority-register-effect-resolver
    (tool-name resolver &key scope)
  "Register RESOLVER for TOOL-NAME and return RESOLVER."
  (unless (functionp resolver)
    (error "Effect resolver for %s must be callable" tool-name))
  (superchat-authority--scoped-register
   'authority-effect-resolver
   (if (symbolp tool-name) (symbol-name tool-name) tool-name)
   resolver scope)
  resolver)

;;;###autoload
(cl-defun superchat-authority-unregister-effect-resolver (tool-name &key scope)
  "Unregister the effect resolver for TOOL-NAME.

TOOL-NAME is normalized exactly as by
`superchat-authority-register-effect-resolver'.  Returns non-nil when an
entry was removed, and nil when no resolver was registered for TOOL-NAME."
  (superchat-authority--scoped-unregister
   'authority-effect-resolver
   (if (symbolp tool-name) (symbol-name tool-name) tool-name) scope))

;;;###autoload
(cl-defun superchat-authority-register-approval-preview
    (tool-name preview &key scope)
  "Register approval PREVIEW for TOOL-NAME and return PREVIEW."
  (unless (functionp preview)
    (error "Approval preview for %s must be callable" tool-name))
  (superchat-authority--scoped-register
   'authority-approval-preview
   (if (symbolp tool-name) (symbol-name tool-name) tool-name)
   preview scope)
  preview)

;;;###autoload
(cl-defun superchat-authority-unregister-approval-preview (tool-name &key scope)
  "Unregister the approval preview for TOOL-NAME.

TOOL-NAME is normalized exactly as by
`superchat-authority-register-approval-preview'.  Returns non-nil when an
entry was removed, and nil when no preview was registered for TOOL-NAME."
  (superchat-authority--scoped-unregister
   'authority-approval-preview
   (if (symbolp tool-name) (symbol-name tool-name) tool-name) scope))

(defun superchat-authority--visible-tool-installation (tool-name &optional scope)
  "Return live Scope tool installation TOOL-NAME visible from SCOPE."
  (let ((installation
         (superchat-scope-lookup
          (superchat-execution-scope-resolve scope) 'tool tool-name)))
    (and (plist-get installation :live) installation)))

(defun superchat-authority--installation-active-p (tool-name)
  "Whether the captured materialized TOOL-NAME installation is still current.

Built-ins and direct legacy Authority callers carry no installation and retain
their established behavior.  A plugin descriptor must still be the exact
nearest Scope installation when it reaches Authority; an old descriptor after
withdrawal or replacement therefore cannot start a new tool body."
  (let ((installation superchat-authority--tool-installation))
    (or (null installation)
        (and (equal tool-name (plist-get installation :name))
             (plist-get installation :live)
             (eq installation
                 (superchat-authority--visible-tool-installation tool-name))))))

(defun superchat-authority--effect-resolver (tool-name &optional scope)
  "Return TOOL-NAME's paired or standalone effect resolver from SCOPE."
  (let* ((captured superchat-authority--tool-installation)
         (installation
          (cond
           ((and captured (equal tool-name (plist-get captured :name))) captured)
           (t (superchat-authority--visible-tool-installation tool-name scope)))))
    (if installation
        ;; A plugin tool's resolver belongs to that installation.  Do not fall
        ;; through to a same-name standalone resolver and mix definitions.
        (plist-get installation :resolver)
      (superchat-authority--scoped-value
       'authority-effect-resolver tool-name scope))))

(defun superchat-authority-effect-resolver-p (tool-name &optional scope)
  "Return non-nil when TOOL-NAME has a visible Authority resolver in SCOPE."
  (functionp (superchat-authority--effect-resolver tool-name scope)))

(defun superchat-authority--approval-preview (tool-name &optional scope)
  "Return TOOL-NAME's paired or standalone approval preview from SCOPE."
  (let* ((captured superchat-authority--tool-installation)
         (installation
          (cond
           ((and captured (equal tool-name (plist-get captured :name))) captured)
           (t (superchat-authority--visible-tool-installation tool-name scope)))))
    (if installation
        (plist-get installation :preview)
      (superchat-authority--scoped-value
       'authority-approval-preview tool-name scope))))

(defun superchat-authority-visible-effect-resolver-names (&optional scope)
  "Return names with a visible resolver, including paired plugin tools."
  (delete-dups
   (append
    (mapcar #'car
            (superchat-scope-merge
             (superchat-execution-scope-resolve scope)
             'authority-effect-resolver))
    (mapcar #'car
            (superchat-scope-merge
             (superchat-execution-scope-resolve scope) 'tool)))))

;;;###autoload
(defun superchat-authority-resolve-effects (tool-name args)
  "Resolve TOOL-NAME and ARGS into pure effect descriptors."
  (let* ((name (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (resolver (superchat-authority--effect-resolver name)))
    (if (functionp resolver)
        (funcall resolver (superchat-authority--snapshot-value args))
      (list (superchat-authority--effect 'unknown 'dynamic name)))))

;;;###autoload
(defun superchat-authority-decision-from-legacy (result)
  "Translate legacy permission hook RESULT to an authority decision.
Accepted legacy values are nil, t, symbols, keyword symbols, strings, or a
plist with :decision, :permission-decision, or :permissionDecision."
  (let ((value (cond
                ((and (listp result)
                      (or (plist-member result :decision)
                          (plist-member result :permission-decision)
                          (plist-member result :permissionDecision)))
                 (or (plist-get result :decision)
                     (plist-get result :permission-decision)
                     (plist-get result :permissionDecision)))
                (t result))))
    (cond
     ((null value) 'defer)
     ((eq value t) 'allow)
     ((keywordp value)
      (superchat-authority-decision-from-legacy
       (intern (substring (symbol-name value) 1))))
     ((symbolp value)
      (if (assq value superchat-authority--decision-rank) value 'defer))
     ((stringp value)
      (let ((sym (intern (downcase value))))
        (if (assq sym superchat-authority--decision-rank) sym 'defer)))
     (t 'defer))))

;;;###autoload
(defun superchat-authority-compose-decisions (&rest decisions)
  "Compose DECISIONS with precedence deny > ask > allow > defer."
  (let ((items (if (and (= (length decisions) 1)
                        (listp (car decisions)))
                   (car decisions)
                 decisions))
        (winner 'defer))
    (dolist (decision items winner)
      (let ((normalized (superchat-authority-decision-from-legacy decision)))
        (when (> (alist-get normalized superchat-authority--decision-rank)
                 (alist-get winner superchat-authority--decision-rank))
          (setq winner normalized))))))

(defun superchat-authority--decision-for-effect (effect attendance)
  "Return default decision for EFFECT under ATTENDANCE."
  (let ((kind (plist-get effect :effect)))
    (pcase kind
      ;; The effect enum is not permission.  Only the authenticated built-in
      ;; execution path may select the nonprompting informational disposition.
      ('human-interaction 'deny)
      ((or 'local-read 'local-write 'process-exec 'runtime-exec
           'external-read 'external-write 'unknown)
       (if (eq attendance 'attended) 'ask 'deny))
      (_ 'defer))))

;;;###autoload
(defun superchat-authority-decide
    (tool-name args attendance &optional policy effects)
  "Return the authority decision plist for TOOL-NAME ARGS and ATTENDANCE.
ATTENDANCE must be explicitly `attended' or `unattended'.
When supplied, EFFECTS is the already-resolved effect snapshot."
  (superchat-authority--check-attendance attendance)
  (let* ((policy (superchat-authority-normalize-policy policy))
         (effects (or effects
                      (superchat-authority-resolve-effects tool-name args)))
         (decision (superchat-authority-compose-decisions
                    (mapcar (lambda (effect)
                              (superchat-authority--decision-for-effect
                               effect attendance))
                            effects))))
    (list :tool (if (symbolp tool-name) (symbol-name tool-name) tool-name)
          :attendance attendance
          :policy policy
          :policy-version (superchat-authority-policy-version-for policy)
          :decision decision
          :args (superchat-authority-normalize-args args)
          :effects effects)))

;;;###autoload
(defun superchat-authority-approval-hash
    (tool-name args effects &optional policy-version execution-id disposition)
  "Return canonical SHA-256 for TOOL-NAME, ARGS, EFFECTS, and POLICY-VERSION.
EXECUTION-ID is optional for callers that already allocate stable execution
identifiers before approval."
  (let* ((normalized-effects
          (sort (mapcar #'superchat-authority-normalize-args effects)
                (lambda (a b)
                  (string< (prin1-to-string a) (prin1-to-string b)))))
         (payload (list :tool (if (symbolp tool-name)
                                  (symbol-name tool-name)
                                tool-name)
                        :args (superchat-authority-normalize-args args)
                        :effects normalized-effects
                        :policy-version (or policy-version
                                            (superchat-authority-policy-version-for
                                             'default))
                        :execution-id execution-id))
         (payload (if disposition (append payload (list :disposition disposition)) payload)))
    (secure-hash 'sha256 (prin1-to-string payload))))

(defun superchat-authority--execution-id ()
  "Return a request-local tool execution identifier."
  (format "tool-%s-%06x"
          (format-time-string "%Y%m%dT%H%M%S%3N" nil t)
          (random #x1000000)))

(defun superchat-authority--intercept (tool-name args)
  "Return the compatibility rewrite decision for TOOL-NAME and ARGS."
  (if (fboundp 'superchat--agent-intercept-tool-call)
      (superchat--agent-intercept-tool-call tool-name args)
    (list :args args)))

(defun superchat-authority--legacy-decision (tool-name args)
  "Return the composed legacy permission decision for TOOL-NAME and ARGS."
  (if (fboundp 'superchat--agent-check-permission)
      (superchat-authority-decision-from-legacy
       (superchat--agent-check-permission
        tool-name (superchat-authority--snapshot-value args)))
    'defer))

(defun superchat-authority--unknown-effect-p (effects)
  "Return non-nil when EFFECTS contains an unclassified effect."
  (cl-some (lambda (effect)
             (eq (plist-get effect :effect) 'unknown))
           effects))

(defun superchat-authority--side-effect-p (effects)
  "Return non-nil when EFFECTS can change state or execute code."
  (cl-some
   (lambda (effect)
     (memq (plist-get effect :effect)
           '(local-write process-exec runtime-exec
             external-read external-write unknown)))
   effects))

(defun superchat-authority--read-only-effects-p (effects)
  "Return non-nil when EFFECTS is non-empty and every effect is `local-read'."
  (and effects
       (cl-every (lambda (effect) (eq (plist-get effect :effect) 'local-read))
                 effects)))

(defun superchat-authority--buffer-file-truename (buffer)
  "Return BUFFER's backing file truename, or nil when unavailable or remote."
  (with-current-buffer buffer
    (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
      (ignore-errors (file-truename buffer-file-name)))))

(defun superchat-authority--sensitive-buffer-name-p (name file-truename)
  "Return non-nil when NAME or FILE-TRUENAME matches a sensitive regexp."
  (and superchat-authority-sensitive-buffers
       (cl-some (lambda (regexp)
                  (or (and name (string-match-p regexp name))
                      (and file-truename (string-match-p regexp file-truename))))
                superchat-authority-sensitive-buffers)))

(defun superchat-authority--sensitive-buffer-scope-p (effect)
  "Return non-nil when buffer `local-read' EFFECT's scope is sensitive.
An EFFECT with no `:scope' member at all (`list_buffers', which reads
every buffer) is sensitive iff any live buffer matches
`superchat-authority-sensitive-buffers'.  A `:scope' naming a live
buffer is sensitive iff that buffer's name or file truename matches.  A
`:scope' naming no live buffer, or holding any non-string value
(including an explicit nil from a missing buffer argument), is always
treated as sensitive so an unresolvable single-buffer read always asks."
  (if (not (plist-member effect :scope))
      (cl-some (lambda (buffer)
                 (superchat-authority--sensitive-buffer-name-p
                  (buffer-name buffer)
                  (superchat-authority--buffer-file-truename buffer)))
               (buffer-list))
    (let ((scope (plist-get effect :scope)))
      (if (stringp scope)
          (let ((buffer (get-buffer scope)))
            (if (buffer-live-p buffer)
                (superchat-authority--sensitive-buffer-name-p
                 scope (superchat-authority--buffer-file-truename buffer))
              t))
        t))))

(defun superchat-authority--sensitive-read-p (effects)
  "Return non-nil when EFFECTS contains a sensitive buffer `local-read'.
Only resource `buffer' reads can be sensitive; every other resource is
never sensitive, regardless of scope."
  (cl-some
   (lambda (effect)
     (and (eq (plist-get effect :effect) 'local-read)
          (eq (plist-get effect :resource) 'buffer)
          (superchat-authority--sensitive-buffer-scope-p effect)))
   effects))

(defun superchat-authority--file-effect-p (effect)
  "Return non-nil when EFFECT is a local file read/write."
  (and (eq (plist-get effect :resource) 'file)
       (memq (plist-get effect :effect) '(local-read local-write))))

(defun superchat-authority--external-effect-p (effect)
  "Return non-nil when EFFECT reaches outside the local runtime."
  (memq (plist-get effect :effect) '(external-read external-write)))

(defun superchat-authority--workspace-path-p (scope workspace-root)
  "Return non-nil when file SCOPE resolves inside WORKSPACE-ROOT."
  (when (and workspace-root (stringp workspace-root))
    (let* ((base (or (and (boundp 'superchat-tools-default-directory)
                          superchat-tools-default-directory)
                     default-directory))
           (root (file-name-as-directory (file-truename workspace-root)))
           (raw-path (cond
                      ((eq scope 'default-directory) base)
                      ((stringp scope) (expand-file-name scope base))
                      (t nil)))
           (path (and raw-path
                      (ignore-errors (file-truename raw-path)))))
      (and path
           (or (string= (directory-file-name path)
                        (directory-file-name root))
               (file-in-directory-p path root))))))

(defun superchat-authority--policy-decision (policy effects context)
  "Return tighten-only decision for POLICY, EFFECTS, and CONTEXT."
  (let ((decisions
         (mapcar
          (lambda (registered)
            (let ((decision (superchat-authority-decision-from-legacy
                             (funcall
                              registered
                              (superchat-authority--snapshot-value effects)
                              (superchat-authority--snapshot-value context)))))
              ;; A named layer may only tighten; allow is equivalent to defer.
              (if (eq decision 'allow) 'defer decision)))
          (superchat-authority--scoped-values 'authority-policy policy))))
    (superchat-authority-compose-decisions
     (cons (superchat-authority--builtin-policy-decision policy effects context)
           decisions))))

(defun superchat-authority--builtin-policy-decision (policy effects context)
  "Return the built-in tighten-only decision for POLICY."
  (let ((workspace-root (plist-get context :workspace-root)))
    (catch 'decision
      (dolist (effect effects 'defer)
        (let ((kind (plist-get effect :effect))
              (resource (plist-get effect :resource))
              (scope (plist-get effect :scope)))
          (pcase policy
            ('default nil)
            ('observer
             (unless (memq kind '(local-read external-read human-interaction))
               (throw 'decision 'deny)))
            ('developer-project
             (when (or (superchat-authority--external-effect-p effect)
                       (and workspace-root
                            (superchat-authority--file-effect-p effect)
                            (not (superchat-authority--workspace-path-p
                                  scope workspace-root)))
                       (and (eq kind 'local-write)
                            (eq resource 'file)
                            (not workspace-root)))
               (throw 'decision 'deny)))
            ('operator
             (when (superchat-authority--external-effect-p effect)
               (throw 'decision 'deny)))
            ('cowork
             ;; Cowork may use external resources and update workspace/team
             ;; state through normal attended approval, but cannot write local
             ;; buffers/files/tools or execute processes.
             (when (or (eq kind 'process-exec)
                       (and (eq kind 'local-write)
                            (not (memq resource '(workspace team)))))
               (throw 'decision 'deny)))))))))

(defun superchat-authority--effective-decision
    (tool-name args authority context)
  "Combine AUTHORITY with compatibility policy for TOOL-NAME and ARGS."
  (let* ((attendance (plist-get authority :attendance))
         (policy (plist-get authority :policy))
         (effects (plist-get authority :effects))
         (read-only-effects (superchat-authority--read-only-effects-p effects))
         (unprompted-effects
          (and read-only-effects
               (member tool-name superchat-authority-unprompted-read-tools)))
         (default (plist-get authority :decision))
         (policy-decision
          (superchat-authority--policy-decision policy effects context))
         (legacy (superchat-authority--legacy-decision tool-name args))
         (guardrails (plist-get context :guardrails))
         (mode (and (boundp 'superchat-authority-mode)
                    superchat-authority-mode))
         (requires-confirm
          (and guardrails
               (fboundp 'superchat--agent-confirm-p)
               (or (superchat--agent-confirm-p
                    tool-name
                    (superchat-authority--snapshot-value args)
                    guardrails)
                   ;; Effects-derived confirmation: the legacy name list has
                   ;; drifted before (ReplaceBuffer), so classified effects
                   ;; always confirm while the guardrail is on.
                   (and (plist-get guardrails :confirm-destructive)
                        (cl-some
                         (lambda (effect)
                           (memq (plist-get effect :effect)
                                 '(local-write process-exec runtime-exec)))
                         effects)))))
         (session-allowed
          (and (boundp 'superchat-authority--session-allowlist)
               superchat-authority--session-allowlist
               (member tool-name
                       superchat-authority--session-allowlist))))
    (cond
     ;; Attendance is a tighten-only policy: no compatibility hook may make an
     ;; unattended side effect executable.
     ((and (eq attendance 'unattended)
           (superchat-authority--side-effect-p effects))
      'deny)
     ((eq policy-decision 'deny) 'deny)
     ((eq legacy 'deny) 'deny)
     ;; A registered policy's ask is a tightening constraint no
     ;; compatibility hook may relax back to allow.
     ((eq policy-decision 'ask) 'ask)
     ((eq legacy 'ask) 'ask)
     ;; A read-only tool call that touches a sensitive buffer, or any read
     ;; while every read must be confirmed, always asks: no later branch
     ;; (legacy allow, execute mode, session allowlist) may relax it.
     ((and read-only-effects
           (or (superchat-authority--sensitive-read-p effects)
               superchat-authority-prompt-for-all-reads))
      'ask)
     ;; Unprompted reads are a user-owned allow-list of tool names, not a
     ;; class: only TOOL-NAME's own membership in
     ;; `superchat-authority-unprompted-read-tools' skips the prompt, never
     ;; anything a resolver, `superchat-register-tool' caller, plugin, or
     ;; MCP declaration puts into an effect plist.  Every other all-
     ;; `local-read' set (e.g. `eval_elisp', `sql', LSP, Workspace,
     ;; `describe-variable', or any tool not on the list) falls through to
     ;; its pre-S07 decision below, in every mode, fully audited.
     (unprompted-effects 'allow)
     ;; Session permission modes (tighten-only).  Explore is read-only:
     ;; side effects are denied even where a legacy hook would allow them.
     ((and (eq mode 'explore)
           (superchat-authority--side-effect-p effects))
      'deny)
     ((eq legacy 'allow) 'allow)
     ;; Execute trusts the classified effect set; unknown effects stay
     ;; conservative (fall through to the default).
     ((and (eq mode 'execute)
           (cl-every
            (lambda (effect)
              (memq (plist-get effect :effect)
                    '(local-write process-exec runtime-exec)))
            effects))
      'allow)
     ;; The session allowlist (y/n/a) covers tools that would otherwise
     ;; ask; it never relaxes a policy or legacy ask above.
     (session-allowed 'allow)
     (requires-confirm 'ask)
     ;; The old global/preset switch is retained as a policy adapter for
     ;; classified local effects.  Unknown tools remain conservative.
     ((and (eq default 'ask)
           guardrails
           (null (plist-get guardrails :confirm-destructive))
           (cl-every
            (lambda (effect)
              (memq (plist-get effect :effect)
                    '(local-write process-exec runtime-exec)))
            effects))
      'allow)
     (t default))))

(defun superchat-authority--experience-block-reason (tool-name context)
  "Return a denial reason when a blocked Experience View owns TOOL-NAME.

The dependency list is explicit request policy.  A blocked View never blocks
unrelated tools, and Authority does not infer dependencies from Candidate text
or from the model prompt."
  (let* ((envelope (plist-get context :context-envelope))
         (view (and (superchat-authority--plist-p envelope)
                    (plist-get envelope :experience-view)))
         (policy (and (superchat-authority--plist-p envelope)
                      (plist-get envelope :experience-action-policy)))
         (status (and (superchat-authority--plist-p view)
                      (plist-get view :status)))
         (kind (and (superchat-authority--plist-p policy)
                    (plist-get policy :kind)))
         (phase (and (superchat-authority--plist-p policy)
                     (plist-get policy :phase)))
         (dependent-tools
          (and (superchat-authority--plist-p policy)
               (plist-get policy :dependent-tools)))
         (name (if (symbolp tool-name)
                   (symbol-name tool-name)
                 tool-name)))
    (when (and (or (member status '(blocked "blocked"))
                   (and (equal "workspace-revision-recovery" kind)
                        (equal "awaiting-info" phase)))
               (listp dependent-tools)
               (cl-some (lambda (candidate)
                         (equal name
                                (if (symbolp candidate)
                                    (symbol-name candidate)
                                  candidate)))
                       dependent-tools))
      (if (and (equal "workspace-revision-recovery" kind)
               (equal "awaiting-info" phase))
          (format "Experience recovery requires workspace_info; dependent action %s is not authorized"
                  name)
        (format "Experience View is blocked; dependent action %s is not authorized"
                name)))))

(defun superchat-authority--scope-for-tape (effect)
  "Return EFFECT with command-like scope values hashed for Tape."
  (let ((kind (plist-get effect :effect))
        (scope (plist-get effect :scope)))
    (if (and scope (memq kind '(process-exec external-write)))
        (let ((copy (copy-sequence effect)))
          (setq copy (plist-put copy :scope
                                (concat "sha256:"
                                        (secure-hash
                                         'sha256
                                         (encode-coding-string
                                          (prin1-to-string scope) 'utf-8)))))
          copy)
      effect)))

(defun superchat-authority--event-base (type context payload &optional summary)
  "Build a Runtime event of TYPE from CONTEXT and PAYLOAD."
  (let ((event
         (list :type type
               :session-id (plist-get context :session-id)
               :actor-id (or (plist-get context :actor-id) "lead")
               :summary summary
               :payload payload)))
    (dolist (key '(:turn-id :run-id :profile-id :workspace-id
                   :correlation-id :causation-id :parent-run-id :team-id
                   :audience :sensitivity))
      (when (plist-get context key)
        (setq event (plist-put event key (plist-get context key)))))
    event))

(defun superchat-authority--append-event
    (type tool-name context payload &optional summary requires-payload
          return-record)
  "Append a tool event and return its logical event identity."
  (superchat-authority--require-current-evidence-origin context)
  (let* ((event
          (superchat-authority--event-base
           type context
           (append (list :tool-name tool-name) payload)
           (or summary (format "%s: %s" type tool-name))))
         (event
          (if requires-payload
              (progn
                (setq event (plist-put event :requires-payload t))
                (plist-put event :sensitivity 'restricted))
            event))
         (record (superchat-tape-append-event-record event)))
    (if return-record
        (append event record)
      (plist-get record :event-id))))

(defun superchat-authority--evidence-fields (context)
  "Return attempt provenance fields from eligible tool CONTEXT."
  (when (plist-get context :evidence-contract)
    (list :evidence-contract (plist-get context :evidence-contract)
          :evidence-attempt (plist-get context :evidence-attempt)
          :decision-snapshot-event-id
          (plist-get context :decision-snapshot-event-id)
          :tool-call-id (plist-get context :tool-call-id)
          :tool-call-ordinal (plist-get context :tool-call-ordinal))))

(defun superchat-authority--exact-fields (label value)
  "Return exact JSON receipt fields for LABEL and VALUE."
  (let ((receipt (superchat-tape-exact-json value)))
    (when (> (plist-get receipt :size) superchat-tape-max-inline-bytes)
      (error "Exact %s evidence exceeds the Tape inline limit" label))
    (list (intern (format ":%s-json" label)) (plist-get receipt :json)
          (intern (format ":%s-size" label)) (plist-get receipt :size)
          (intern (format ":%s-hash" label)) (plist-get receipt :hash))))

(defun superchat-authority--require-current-evidence-origin (context)
  "Signal when eligible CONTEXT no longer names the active provider attempt."
  (when (plist-get context :evidence-contract)
    (let ((attempt (plist-get context :evidence-attempt))
          (snapshot-id (plist-get context :decision-snapshot-event-id))
          (validator (plist-get context :evidence-origin-validator)))
      (unless (and (integerp attempt) (> attempt 0)
                   (stringp snapshot-id) (not (string-empty-p snapshot-id))
                   (functionp validator)
                   (funcall validator attempt snapshot-id))
        (error "Tool evidence origin is missing, stale, or mismatched")))))

(defun superchat-authority--validated-evidence-action
    (tool-name args context)
  "Return exact action receipt for TOOL-NAME ARGS and eligible CONTEXT."
  (when-let* ((contract (plist-get context :evidence-contract)))
    (unless (equal contract "epistemic-learning-v0")
      (error "Unknown tool evidence contract: %s" contract))
    (let ((attempt (plist-get context :evidence-attempt))
          (snapshot-id (plist-get context :decision-snapshot-event-id))
          (call-id (plist-get context :tool-call-id))
          (ordinal (plist-get context :tool-call-ordinal))
          (call-name (plist-get context :tool-call-name))
          (call-args (plist-get context :tool-call-arguments)))
      (unless (and (integerp attempt) (> attempt 0)
                   (stringp snapshot-id) (not (string-empty-p snapshot-id))
                   (stringp call-id) (not (string-empty-p call-id))
                   (integerp ordinal) (> ordinal 0)
                   (equal tool-name call-name)
                   (listp call-args))
        (error "Tool evidence origin is missing, stale, or mismatched"))
      (superchat-authority--require-current-evidence-origin context)
      (superchat-authority--exact-fields
       "arguments"
       (list :tool-name tool-name
             :call-id call-id
             :named-arguments (copy-tree call-args t)
             :positional-arguments (vconcat (copy-tree args t)))))))

(defun superchat-authority--result-fingerprint (result)
  "Return bounded identity metadata for RESULT."
  (let* ((printed (prin1-to-string result))
         (encoded (encode-coding-string printed 'utf-8)))
    (list :result-size (string-bytes encoded)
          :result-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-authority--run-hooks (event &rest args)
  "Emit tool lifecycle EVENT with ARGS."
  (apply #'superchat-emit (superchat-execution-scope-resolve nil) event args))

(defun superchat-authority--filter-result (tool-name args result)
  "Apply the compatibility result filter to RESULT."
  (if (fboundp 'superchat--agent-filter-tool-result)
      (superchat--agent-filter-tool-result tool-name args result)
    result))

(defun superchat-authority--implementation-receipt (result)
  "Return bounded receipt metadata distinct from a delivered Result."
  (let* ((printed (prin1-to-string result))
         (encoded (encode-coding-string printed 'utf-8)))
    (list :implementation-result-size (string-bytes encoded)
          :implementation-result-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-authority--notify-failure (tool-name args err)
  "Notify failure hooks with detached ARGS and ERR."
  (superchat-authority--run-hooks
   'tool-failed
   tool-name
   (superchat-authority--snapshot-value args)
   (superchat-authority--snapshot-value err)))

(defun superchat-authority-confirm (request)
  "Ask the user to approve normalized tool REQUEST once.
`y' allows once, `a' allows for the rest of the session (offered only
for non-dangerous tools), `n' denies.  Dangerous process-executing
tools never enter the session allowlist.  Prompt in the minibuffer;
`?' shows the full effects, arguments and request identity."
  (let* ((tool-name (plist-get request :tool-name))
         (args (plist-get request :args))
         (effects (plist-get request :effects))
         (hash (plist-get request :request-hash))
         (dangerous-p
          (cl-some
           (lambda (effect)
             (memq (plist-get effect :effect)
                   '(process-exec runtime-exec)))
           effects)))
    (when-let* ((preview (superchat-authority--approval-preview tool-name)))
      (funcall preview (superchat-authority--snapshot-value request)))
    (let* ((use-dialog-box nil)
           (read-char-choice-use-read-key nil)
           (answer-cons
            (read-multiple-choice
             (format
              "Allow `%s'?%s (? for details) "
              tool-name (if dangerous-p " (dangerous — once only)" ""))
             (if dangerous-p
                 '((?y "yes, allow once") (?n "no"))
               '((?y "yes, allow once")
                 (?a "always allow this session")
                 (?n "no")))
             (format "Tool: %s\nEffects: %S\nArgs: %S\nRequest: %s\n\ny: allow once\nn: deny this call\n%s"
                     tool-name effects args hash
                     (if dangerous-p ""
                       "a: allow this tool for the rest of this session\n"))))
           ;; read-multiple-choice returns the chosen (char . help) cell.
           (answer (car answer-cons)))
      (prog1
          ;; `a' is an approval too: the tool executes now AND is
          ;; remembered for the session.
          (memq answer '(?y ?a))
        (when (eq answer ?a)
          (when (boundp 'superchat-authority--session-allowlist)
            (cl-pushnew tool-name superchat-authority--session-allowlist
                        :test #'equal)
            (message "Approved for this session: %s (mode change clears it)"
                     tool-name)))
        (when (fboundp 'superchat--extend-timeout)
          (superchat--extend-timeout
           (superchat-authority-transport-timer-owner)))))))

(defun superchat-authority--deliver (async callback result)
  "Return or asynchronously deliver RESULT."
  (if async
      (progn (funcall callback result) nil)
    result))

(defun superchat-authority--run-active-p (context)
  "Return whether the Runtime owning CONTEXT still accepts tool work.

Contexts from non-Runtime callers omit `:run-active-p' and retain the
historical behavior.  A Runtime context must provide a callable predicate;
missing or failing predicates fail closed so a late approval cannot reach the
tool implementation."
  (if (not (plist-member context :run-active-p))
      t
    (let ((predicate (plist-get context :run-active-p)))
      (and (functionp predicate)
           (condition-case nil
             (funcall predicate)
             (error nil))))))

(defun superchat-authority--execution-active-p (tool-name context)
  "Return whether TOOL-NAME may still begin native execution for CONTEXT."
  (and (superchat-authority--run-active-p context)
       (superchat-authority--installation-active-p tool-name)))

(defun superchat-authority--inactive-result (tool-name)
  "Return the truthful refusal text for inactive TOOL-NAME."
  (if (superchat-authority--installation-active-p tool-name)
      "[Tool call blocked: Run is no longer active]"
    "[Tool call blocked: Tool is no longer available]"))

(defun superchat-authority--record-inactive-fault
    (tool-name context execution-id)
  "Diagnose an execution gate without fabricating an Effect disposition."
  (superchat-stage-record-fault
   'tool-liveness execution-id 'authority
   (list :tool tool-name :run-id (plist-get context :run-id)
         :reason (if (superchat-authority--installation-active-p tool-name)
                     'run-inactive
                   'installation-inactive))))

(defun superchat-authority--record-failure
    (tool-name context execution-id request-hash status
               &optional result exact-error metadata)
  "Record a failed or refused tool execution."
  (let ((record
         (superchat-authority--append-event
          "tool.failed" tool-name context
          (append
           (list :execution-id execution-id
                 :request-hash request-hash
                 :status status)
           metadata
           (when-let* ((block-reason (plist-get context :block-reason)))
             (list :block-reason block-reason))
           (when-let* ((error-type (plist-get exact-error :error-type)))
             (list :error-type error-type))
           (superchat-authority--evidence-fields context)
           (when (plist-get context :evidence-contract)
             (superchat-authority--exact-fields
              "error" (or exact-error (list :status status :result result)))))
          nil (and (plist-get context :evidence-contract) t) t)))
    (unless (equal "user-quit" (plist-get metadata :interruption))
      (when-let* ((callback (plist-get context :failure-recorded-function)))
        (when (functionp callback)
          (funcall callback record)))
      (when-let* ((callback (plist-get context :recovery-failure-function)))
        (when (functionp callback)
          (condition-case err
              (funcall callback
                       (list :event record
                             :tool-name tool-name
                             :args (copy-tree
                                    (plist-get context
                                               :recovery-failure-args)
                                    t)
                             :status status
                             :result result
                             :error exact-error))
            (error
             (display-warning
              'superchat-authority
              (format "Tool failure observer failed: %s"
                      (error-message-string err))
              :warning))))))
    record))

(defun superchat-authority--handle-quit
    (condition tool-name args context execution-id request-hash phase
               &optional implementation-returned-p implementation-result
               outcome-durable failure-status)
  "Account for CONDITION at PHASE, interrupt its Run, then re-signal it."
  (let ((audit-ok outcome-durable)
        (failure-record nil))
    (unless outcome-durable
      (condition-case audit-condition
          (progn
            (setq failure-record
                  (superchat-authority--record-failure
                   tool-name context execution-id request-hash
                   (or failure-status "failed")
                   "[Tool call interrupted by user]"
                   (list :error-type "quit"
                         :error-data (copy-tree (cdr condition) t)
                         :message "Quit")
                   (append
                    (list :interruption "user-quit" :phase phase)
                    (when (equal phase "implementation")
                      (list :effects-may-be-partial t))
                    (when implementation-returned-p
                      (superchat-authority--implementation-receipt
                       implementation-result)))))
            (setq audit-ok t))
        ((error quit)
         (when-let* ((audit-failure
                      (plist-get context :audit-failure-function)))
           (when (functionp audit-failure)
             (condition-case nil
                 (funcall audit-failure audit-condition
                          "[Tool interruption audit failed]")
               ((error quit) nil)))))))
    (when failure-record
      (condition-case nil
          (superchat-authority--notify-failure tool-name args condition)
        ((error quit) nil)))
    (when audit-ok
      (when-let* ((interrupt (plist-get context :interrupt-function)))
        (when (functionp interrupt)
          (condition-case nil
              (funcall interrupt)
            ((error quit) nil)))))
    (signal (car condition) (cdr condition))))

(defun superchat-authority--finish-success
    (tool-name args result context execution-id request-hash async callback)
  "Filter, audit, and deliver successful RESULT."
  (let* ((evidence (plist-get context :evidence-contract))
         (completed-record nil)
         (result (superchat-authority--snapshot-value result))
         (audit
         (condition-case err
             (progn
               (superchat-authority--require-current-evidence-origin context)
               ;; Reject sensitive raw results before compatibility hooks can
               ;; render or copy them outside the exact-evidence seam.
               (when evidence
                 (superchat-authority--exact-fields
                  "result" (list :result result)))
               (setq result
                     (superchat-authority--filter-result
                      tool-name args result))
               ;; Detach from filter-owned data before persistence and
               ;; provider delivery on both ordinary and exact paths.
               (setq result (superchat-authority--snapshot-value result))
               (setq completed-record
                     (superchat-authority--append-event
                      "tool.completed" tool-name context
                      (append
                       (list :execution-id execution-id
                             :request-hash request-hash
                             :status "succeeded")
                       (superchat-authority--result-fingerprint result)
                       (superchat-authority--evidence-fields context)
                       (when evidence
                         (superchat-authority--exact-fields
                          "result" (list :result result))))
                      nil (and evidence t) t))
               ;; Runtime owns recovery state.  Notify it only after the
               ;; durable success event exists; a bookkeeping failure must not
               ;; turn a successfully executed tool into an audit failure.
               (when-let* ((observer
                           (plist-get context :tool-completed-function)))
                 (when (functionp observer)
                   (condition-case err
                       (funcall
                        observer
                        (list :tool-name tool-name
                              :args (superchat-authority--snapshot-value args)
                              :result (superchat-authority--snapshot-value
                                       result)
                              :event completed-record
                              :execution-id execution-id))
                     (error
                      (display-warning
                       'superchat-authority
                       (format "Tool completion observer failed: %s"
                               (error-message-string err))
                       :warning)))))
               (superchat-authority--run-hooks
                 'tool-after
                  tool-name args
                  (superchat-authority--snapshot-value result)
                  (and (listp context) (plist-get context :run-id)))
               (superchat-authority--require-current-evidence-origin context)
               'complete)
           (error (cons 'failed err))
           (quit
            (superchat-authority--handle-quit
             err tool-name args context execution-id request-hash
             "result-processing" t result (and completed-record t))))))
    (if (eq audit 'complete)
        (superchat-authority--deliver async callback result)
      (let* ((evidence (plist-get context :evidence-contract))
             (err (cdr audit))
             (failure
              (if evidence
                  "[Tool result withheld because exact audit failed]"
                (format "[Tool completed but audit failed: %s]"
                        (error-message-string err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
        (if (functionp audit-failure)
            (progn
              (funcall audit-failure err failure)
              ;; Runtime has closed the Run and cancelled its provider request.
              ;; A delayed async callback would only reopen the transport path.
              (unless async failure))
          (superchat-authority--deliver async callback failure))))))

(defun superchat-authority--finish-error
    (tool-name args err context execution-id request-hash async callback)
  "Audit ERR from TOOL-NAME and deliver a model-readable failure."
  (let* ((message (error-message-string err))
         (result (format "[Tool call failed: %s]" message))
         (exact-error
          (list :error-type (format "%s" (car err))
                :error-data (copy-tree (cdr err) t)
                :message message))
         (audit
          (condition-case audit-err
              (progn
                (superchat-authority--require-current-evidence-origin context)
                (superchat-authority--record-failure
                 tool-name context execution-id request-hash
                 "failed" result exact-error)
                (superchat-authority--notify-failure tool-name args err)
                (superchat-authority--require-current-evidence-origin context)
                'complete)
            (quit
             (superchat-authority--handle-quit
              audit-err tool-name args context execution-id request-hash
              "result-processing" nil nil t))
            (error (cons 'failed audit-err)))))
    (if (eq audit 'complete)
        (superchat-authority--deliver async callback result)
      (let* ((audit-err (cdr audit))
             (failure
              (if (plist-get context :evidence-contract)
                  "[Tool error withheld because exact audit failed]"
                (format "%s [Audit also failed: %s]"
                        result (error-message-string audit-err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
        (if (functionp audit-failure)
            (progn
              (funcall
               audit-failure audit-err
               "[Tool error withheld because exact audit failed]")
              (unless async failure))
          (superchat-authority--deliver async callback failure))))))

(defun superchat-authority--normalize-tool-result (value)
  "Normalize textual tool results at the shared execution seam.
A byte-level read (e.g. coding-system-for-read bound to binary) or an
encoding artifact leaves raw bytes inside a tool result string, and
Emacs 31's json-serialize rejects that string — killing the next
streaming request with `wrong-type-argument json-value-p'.  Decode
those back to Unicode here, before the result forks to the Tape, the
renderer, and the provider.  Non-string values pass through."
  (if (not (stringp value))
      value
    (if (or (and (not (multibyte-string-p value))
                 (string-match-p "[^\000-\177]" value))
            (and (multibyte-string-p value)
                 (catch 'raw-byte
                   (mapc (lambda (c)
                           (when (> c #x3FFF7F) (throw 'raw-byte t)))
                         value))))
        ;; Decode the underlying bytes as UTF-8; a failed decode keeps
        ;; the original value so behavior degrades, never hard-fails.
        (condition-case nil
            (decode-coding-string (string-as-unibyte value) 'utf-8)
          (error value))
      value)))

(defun superchat-authority--input-read-receipt (expected)
  "Read the exact stored EXPECTED input outcome, rejecting any conflict.
Ordinary results have a fingerprint, not durable raw result text."
  (let* ((events (superchat-tape-replay-events (plist-get expected :session-id)))
         (execution (plist-get (plist-get expected :payload) :execution-id))
         (matches (cl-remove-if-not
                   (lambda (event)
                     (or (equal (plist-get event :event-id) (plist-get expected :event-id))
                         (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
                              (equal execution (plist-get (plist-get event :payload) :execution-id)))))
                   events)))
    (when matches
      (unless (= (length matches) 1) (error "Conflicting input outcome receipts"))
      (let ((actual (car matches)))
        (dolist (key (append '(:event-id :type :schema :session-id :payload)
                             (mapcar #'car superchat-tape--envelope-keys)))
          (unless (and (eq (not (null (plist-member actual key)))
                           (not (null (plist-member expected key))))
                       (equal (plist-get actual key) (plist-get expected key)))
            (error "Input receipt mismatch at %s" key)))
        (unless (and (integerp (plist-get actual :storage-id))
                     (> (plist-get actual :storage-id) 0)
                     (stringp (plist-get actual :recorded-at)))
          (error "Input receipt has no stored identity"))
        actual))))

(defun superchat-authority--make-human-input-operation (run-id token)
  "Create the built-in operation for one accepted Run; no reverse require.
The native callback is intentionally not used to acknowledge waiting."
  (lambda (_done question)
    (let* ((context superchat--tool-execution-context)
           (capability (plist-get context :input-settlement))
           (identity (and (functionp capability) (funcall capability 'snapshot))))
      (unless (and (equal run-id (plist-get identity :run-id))
                   (eq token (plist-get identity :token))
                   (stringp question) (not (string-empty-p (string-trim question))))
        (error "Invalid trusted human input invocation"))
      (superchat-runtime--request-input capability (list :question question)))))

(defun superchat-authority--human-input-decision (tool-name args authority context)
  "Nonprompting disposition for an already authenticated informational operation.
Tightening asks become restrictions, never permission prompts or implicit allows."
  (if (or (not (eq (plist-get authority :attendance) 'attended))
          (memq (superchat-authority--policy-decision
                 (plist-get authority :policy) (plist-get authority :effects) context)
                '(deny ask))
          (memq (superchat-authority--legacy-decision tool-name args) '(deny ask)))
      'deny
    'allow))

(defun superchat-authority--make-input-settlement
    (tool-name args context execution-id request-hash callback)
  "Create the NONSHIPPING invocation's single settlement capability."
  (let (claim expected receipt (receipt-state 'not-attempted)
              (delivery 'not-started) result fault)
    (lambda (operation &rest values)
      (pcase operation
        ('snapshot
         (list :claim claim :receipt-state receipt-state
               :receipt (superchat-authority--snapshot-value receipt)
               :delivery delivery :fault fault
               :run-id (plist-get context :run-id)
               :session-id (plist-get context :session-id)
               :token (plist-get context :input-core-token)))
        ('settle
         (unless claim
           ;; Claim is the only winner, and precedes every callback/append.
           (setq claim (car values) fault 'prepare)
           (unless (memq claim '(answer failed)) (error "Invalid input outcome"))
           (setq result (superchat-authority--snapshot-value
                         (superchat-authority--normalize-tool-result (cadr values))))
           (when (eq claim 'answer)
             (setq result (superchat-authority--snapshot-value
                           (superchat-authority--filter-result tool-name args result))))
           (let* ((event (superchat-authority--event-base
                          (if (eq claim 'answer) "tool.completed" "tool.failed") context
                          (append
                           (list :tool-name tool-name :execution-id execution-id
                                 :request-hash request-hash
                                 :status (if (eq claim 'answer) "succeeded" "failed"))
                           (if (eq claim 'answer)
                               (superchat-authority--result-fingerprint result)
                             (list :error-type "input-stopped" :reason result)))))
                  (payload (superchat-tape--encoded-payload (plist-get event :payload))))
             (when (plist-get payload :omitted) (error "Input receipt payload omitted"))
             (setq event (plist-put event :schema 1)
                   event (plist-put event :event-id (superchat-tape--new-event-id)))
             ;; Compare the exact representation returned by Tape replay.
             (setq expected
                   (json-parse-string
                    (json-encode (plist-put (copy-tree event t) :payload payload))
                    :object-type 'plist :array-type 'list :null-object nil :false-object nil)
                   fault 'audit receipt-state 'attempting)
             (unwind-protect
                 (superchat-tape-append-event-record event)
               ;; A real append can commit and then signal.  Never infer
               ;; absence from its nonlocal exit or attempt another outcome.
               (condition-case nil
                   (setq receipt (superchat-authority--input-read-receipt expected)
                         receipt-state (if receipt 'committed 'absent))
                 ((error quit) (setq receipt-state 'unknown))))
             (unless (eq receipt-state 'committed) (error "Input receipt not verifiable"))
             (setq fault 'observer)
             (if (eq claim 'answer)
                 (progn
                   (when-let* ((observer (plist-get context :tool-completed-function)))
                     (funcall observer
                              (list :tool-name tool-name :args args :result result
                                    :event receipt :execution-id execution-id)))
                   (superchat-authority--run-hooks
                    'tool-after
                    tool-name args result
                    (and (listp context) (plist-get context :run-id))))
               (when-let* ((observer (plist-get context :failure-recorded-function)))
                 (funcall observer receipt)))
             (setq fault nil)
             t)))
        ('deliver
         (when (and (eq claim 'answer) (eq receipt-state 'committed)
                    (eq delivery 'not-started)
                    (superchat-authority--run-active-p context))
           (setq delivery 'entered)
           (condition-case condition
               (prog1 (funcall callback result) (setq delivery 'returned))
             ((error quit)
              (setq delivery 'raised)
              (signal (car condition) (cdr condition))))))
        (_ (error "Unknown input settlement operation"))))))

(defun superchat-authority--invoke-async
    (tool-name original-fn args context execution-id request-hash callback)
  "Invoke asynchronous ORIGINAL-FN once and account for its outcome."
  (if (and (plist-get context :input-core-token)
           (equal tool-name (plist-get context :input-core-tool-name)))
      (let* ((execution-scope (superchat-execution-scope-resolve nil))
             (installation superchat-authority--tool-installation)
             (capability (superchat-authority--make-input-settlement
                          tool-name args context execution-id request-hash callback))
             (superchat--syn-call-confirmed t)
             (superchat--tool-execution-context
              (plist-put (superchat-authority--snapshot-value context)
                         :input-settlement capability)))
        (let ((superchat-execution-scope execution-scope)
              (superchat-authority--tool-installation installation))
          (apply original-fn
                 (lambda (value)
                   (let ((superchat-execution-scope execution-scope)
                         (superchat-authority--tool-installation installation))
                     (when (funcall capability 'settle 'answer value)
                       (funcall capability 'deliver))))
                 args)))
    (superchat-authority--invoke-async-default
     tool-name original-fn args context execution-id request-hash callback)))

(defun superchat-authority--invoke-async-default
    (tool-name original-fn args context execution-id request-hash callback)
  "Preserve the ordinary async execution and consumer condition contract."
  (let ((completed nil)
        ;; Runtime releases its private Run reference at quiesce.  A native
        ;; callback must therefore carry the invocation's borrowed Scope now,
        ;; rather than attempt to recover it from a terminal Run later.
        (execution-scope (superchat-execution-scope-resolve nil))
        (installation superchat-authority--tool-installation))
    (condition-case condition
        (let ((superchat-execution-scope execution-scope)
              (superchat-authority--tool-installation installation)
              (superchat--syn-call-confirmed t)
              (superchat--tool-execution-context
               (superchat-authority--snapshot-value context)))
          (apply
           original-fn
           (cons
            (lambda (result)
              (unless completed
                (setq completed t)
                (let ((superchat-execution-scope execution-scope)
                      (superchat-authority--tool-installation installation))
                  ;; A genuine late result is a fact about an already-entered
                  ;; Effect.  Do not recheck installation liveness here: the
                  ;; invocation gate already prevented new work, while this
                  ;; completion must still settle truthful residual evidence.
                  (superchat-authority--finish-success
                   tool-name args
                   (superchat-authority--normalize-tool-result result)
                   context execution-id request-hash t callback))))
            args)))
      (error
       (unless completed
         (setq completed t)
         (superchat-authority--finish-error
          tool-name args condition context execution-id request-hash
          t callback)))
      (quit
       (unless completed
         (setq completed t)
         (superchat-authority--handle-quit
          condition tool-name args context execution-id request-hash
          "implementation"))
       (signal (car condition) (cdr condition))))))

(defun superchat-authority--invoke-sync
    (tool-name original-fn args context execution-id request-hash)
  "Invoke synchronous ORIGINAL-FN once and account for its outcome."
  (let ((outcome
         (condition-case condition
             (let ((superchat-execution-scope
                    (superchat-execution-scope-resolve nil))
                   (superchat-authority--tool-installation
                    superchat-authority--tool-installation)
                   (superchat--syn-call-confirmed t)
                   (superchat--tool-execution-context
                    (superchat-authority--snapshot-value context)))
               (cons 'ok
                     (superchat-authority--normalize-tool-result
                      (apply original-fn args))))
           (error (cons 'error condition))
           (quit
            (superchat-authority--handle-quit
             condition tool-name args context execution-id request-hash
             "implementation")))))
    (if (eq (car outcome) 'ok)
        (superchat-authority--finish-success
         tool-name args (cdr outcome) context execution-id request-hash nil nil)
      (superchat-authority--finish-error
       tool-name args (cdr outcome) context execution-id request-hash nil nil))))

(defun superchat-authority--invoke
    (tool-name original-fn args context execution-id request-hash
               async callback &optional refuse)
  "Invoke ORIGINAL-FN after the authority and audit gates.
REFUSE, when supplied, delivers a preexecution liveness refusal."
  (if (not (superchat-authority--execution-active-p tool-name context))
      (let ((result (superchat-authority--inactive-result tool-name)))
        ;; Losing execution authority is not an Effect disposition.  Preserve
        ;; unresolved Tape evidence and diagnose without fabricating a failure.
        (superchat-authority--record-inactive-fault
         tool-name context execution-id)
        (if refuse
            (funcall refuse result)
          (superchat-authority--deliver async callback result)))
    (progn
      ;; Persist before any compatibility hook or tool code can produce
      ;; effects.  An inactive Run returned above no longer needs an evidence
      ;; origin check because it cannot reach ORIGINAL-FN.
      (superchat-authority--require-current-evidence-origin context)
      (let* ((started-id
              (superchat-authority--append-event
               "tool.execution.started" tool-name context
               (list :execution-id execution-id
                     :request-hash request-hash
                     :status "executing")))
             (context
              (plist-put (copy-sequence context) :causation-id started-id)))
      (condition-case condition
          (superchat-authority--run-hooks
           'tool-before
           tool-name (superchat-authority--snapshot-value args)
           (and (listp context) (plist-get context :run-id)))
        (quit
         (superchat-authority--handle-quit
          condition tool-name args context execution-id request-hash "pre")))
      (superchat-authority--require-current-evidence-origin context)
      (if (or (not (superchat-authority--execution-active-p tool-name context))
              (and (plist-get context :input-core-token)
                   (equal tool-name "ask_user")
                   (not (superchat-runtime--trusted-input-operation-p
                         context original-fn async))))
          (let* ((human-input (and (equal tool-name "ask_user") (plist-get context :input-operation)))
                 (result (if human-input "[Human input adapter is no longer available]"
                           (superchat-authority--inactive-result tool-name))))
            ;; The trusted input operation has not run.  Its admitted failure
            ;; is truthful, but a failed receipt must never reach native done.
            ;; Ordinary late-work accounting keeps its existing behavior.
            (let (audit-condition)
              (if (and human-input
                       (superchat-authority--execution-active-p tool-name context))
                  (condition-case condition
                      (superchat-authority--record-failure
                       tool-name context execution-id request-hash "failed" result
                       '(:error-type "input-adapter-unavailable"))
                    ((error quit) (setq audit-condition condition)))
                (superchat-authority--record-inactive-fault
                 tool-name context execution-id))
              (if audit-condition
                  (unwind-protect
                      (when-let* ((failure (plist-get context :audit-failure-function)))
                        (funcall failure audit-condition "[Human input failure receipt is incomplete]"))
                    (when (eq (car audit-condition) 'quit)
                      (signal (car audit-condition) (cdr audit-condition))))
                (if refuse (funcall refuse result)
                  (superchat-authority--deliver async callback result)))))
        (if async
            (superchat-authority--invoke-async
             tool-name original-fn args context execution-id request-hash
             callback)
          (superchat-authority--invoke-sync
           tool-name original-fn args context execution-id request-hash)))))))

;;;###autoload
(cl-defun superchat-tool-execute
    (tool-name original-fn args &key async callback context)
  "Execute TOOL-NAME through the shared Authority and Tape seam.

ORIGINAL-FN receives normalized ARGS.  When ASYNC is non-nil it receives
CALLBACK as its first argument; otherwise the filtered result is returned.
CONTEXT must include `:session-id' and explicit `:attendance'."
  ;; Bind this invocation's identity even for a raw legacy callable: a nested
  ;; `superchat-tool-execute' must not inherit its caller's installation.
  ;; A withdrawn proxy is intentionally no longer `functionp', but still
  ;; carries its revoked identity so the initial liveness gate can return the
  ;; ordinary refusal rather than accidentally treating it as legacy.
  (let* ((superchat-authority--tool-installation
          (superchat-authority--installation-from-function original-fn))
         (session-id (plist-get context :session-id))
         (attendance (plist-get context :attendance))
         (policy (superchat-authority-normalize-policy
                  (or (plist-get context :authority-policy)
                      (plist-get context :policy))))
         refusal-delivery-started
         (refuse (lambda (result)
                   (setq refusal-delivery-started t)
                   (superchat-authority--deliver async callback result))))
    (unless (or (functionp original-fn)
                superchat-authority--tool-installation)
      (error "Tool %s has no callable implementation" tool-name))
    (unless (listp args)
      (error "Tool %s arguments must be a list" tool-name))
    (when (and async (not (functionp callback)))
      (error "Async tool %s requires a callback" tool-name))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Tool execution requires context :session-id"))
    (superchat-authority--check-attendance attendance)
    ;; A first arrival on a terminal Run, or through a revoked materialized
    ;; installation, was never admitted to execution audit.
    (unless (superchat-authority--execution-active-p tool-name context)
      (cl-return-from superchat-tool-execute
        (superchat-authority--deliver
         async callback (superchat-authority--inactive-result tool-name))))
    (setq context (plist-put context :authority-policy policy))
    ;; ── pre-policy: normalize only, never decide ─────────────────────
    ;; Liveness, identity, argument normalization, interception, effect
    ;; resolution and the block check all live here; none returns a verdict.
    (condition-case outer-err
        (let* ((provider-args (superchat-authority--snapshot-value args))
               (evidence-action
                (superchat-authority--validated-evidence-action
                 tool-name provider-args context))
               (context
                (if evidence-action
                    (plist-put
                     (copy-sequence context) :causation-id
                     (plist-get context :decision-snapshot-event-id))
                  context))
               (intercept (superchat-authority--intercept tool-name args))
               (args (if (plist-member intercept :args)
                         (plist-get intercept :args)
                       args))
               (normalizer (plist-get context :normalize))
               (args (if (functionp normalizer)
                         (funcall normalizer args)
                       args))
               (args (superchat-authority--snapshot-value args))
               (context
                (plist-put (copy-sequence context)
                           :recovery-failure-args
                           (copy-tree args t)))
               (_evidence-arguments
                (when (and evidence-action
                           (not (equal provider-args args)))
                  (error "Eligible tool arguments changed before execution")))
               (validator (plist-get context :validate))
               (_ (when (functionp validator)
                    (funcall validator
                             (superchat-authority--snapshot-value args))))
               (resolver (plist-get context :resolve-effects))
               (trusted-input
                (and (equal tool-name "ask_user")
                     (fboundp 'superchat-runtime--trusted-input-operation-p)
                     (superchat-runtime--trusted-input-operation-p context original-fn async)))
               (effects
                (superchat-authority--snapshot-value
                 (if trusted-input
                     (list (superchat-authority--effect
                            'human-interaction 'human-input
                            (list :session-id (plist-get context :session-id)
                                  :run-id (plist-get context :run-id))))
                   (or (if (functionp resolver)
                         (funcall
                          resolver
                          (superchat-authority--snapshot-value args))
                       (superchat-authority-resolve-effects tool-name args))
                     (list (superchat-authority--effect
                            'unknown 'dynamic tool-name))))))
               (input-candidate
                (or (equal tool-name "ask_user")
                    (cl-some (lambda (effect)
                               (eq (plist-get effect :effect) 'human-interaction)) effects)))
               (experience-block
                (superchat-authority--experience-block-reason
                 tool-name context))
               ;; ── monotonic guards (deny-only) ─────────────────────
               ;; The block reason, the named-policy decision composed by
               ;; `superchat-authority--policy-decision', and the effective
               ;; decision.  A guard returns deny/ask/nil and an `allow' is
               ;; read as no tightening, so nothing here relaxes an
               ;; inherited deny.
               (block-reason
                (or (and input-candidate (not trusted-input) "Untrusted human input operation")
                    (and trusted-input
                         (or resolver
                             (superchat-authority--effect-resolver "ask_user"))
                         "Human input effects cannot be overridden")
                    (and trusted-input
                         (not (and (= (length args) 1) (stringp (car args))
                                   (not (string-empty-p (string-trim (car args))))))
                         "Human input requires one nonempty question string")
                    experience-block
                    (plist-get context :block)
                    (plist-get intercept :block)))
               (blocked (and block-reason t))
               (context
                (if block-reason
                    (plist-put (copy-sequence context)
                               :block-reason block-reason)
                  context))
               ;; ── approval: the one place answering who may approve ──
               ;; The decision, the active-execution recheck and the
               ;; `approval.requested' branch below are the seam.
               (authority (superchat-authority-decide
                           tool-name args attendance policy effects))
               (authority
                (if input-candidate
                    (plist-put authority :policy-version
                               (concat (plist-get authority :policy-version) "/human-input-v1"))
                  authority))
               (decision
                (if blocked
                    'deny
                  (if trusted-input
                      (superchat-authority--human-input-decision tool-name args authority context)
                    (superchat-authority--effective-decision
                     tool-name args authority context))))
               ;; Invariant 3: the Authority decision is known and well-formed
               ;; before any tool body runs.
               (_authority-invariant
                (superchat-invariant-check
                 'tool-execution-passes-authority
                 (and (listp context) (plist-get context :session-id))
                 (and (listp context) (plist-get context :run-id))
                 tool-name authority decision))
               (decision
                (if (eq decision 'defer)
                    (if (eq attendance 'attended) 'ask 'deny)
                  decision))
               (disposition
                (when input-candidate (format "human-input-v1:%s" decision)))
               (execution-id (or (plist-get context :execution-id)
                                 (superchat-authority--execution-id)))
               (request-hash
                (superchat-authority-approval-hash
                 tool-name args effects
                 (plist-get authority :policy-version)
                 execution-id disposition))
               (tape-effects
                (mapcar #'superchat-authority--scope-for-tape effects))
               (request
                (list :tool-name tool-name
                      :args (superchat-authority-normalize-args args)
                      :effects effects
                      :decision decision
                      :attendance attendance
                      :policy (plist-get authority :policy)
                      :policy-version
                      (plist-get authority :policy-version)
                      :execution-id execution-id
                      :request-hash request-hash))
               (_request-active
                (superchat-authority--require-current-evidence-origin context))
               (_admission-active
                ;; Interceptors, resolvers and policy callbacks may cancel a
                ;; Run or withdraw this materialized installation.  Recheck
                ;; before persisting/starting any new native work.
                (unless (superchat-authority--execution-active-p tool-name context)
                  (cl-return-from superchat-tool-execute
                    (funcall refuse
                             (superchat-authority--inactive-result
                              tool-name)))))
               ;; ── body: entered only after the decision is well-formed ─
               ;; `tool.requested' is already on the Tape (invariant 3).
               (requested-id
                (superchat-authority--append-event
                 "tool.requested" tool-name context
                 (append
                  (list :execution-id execution-id
                        :request-hash request-hash
                        :effects tape-effects
                        :attendance attendance
                        :policy (plist-get authority :policy)
                        :policy-version
                        (plist-get authority :policy-version)
                        :argument-count (length args)
                        :decision (symbol-name decision))
                  (when disposition (list :disposition disposition))
                  (when block-reason
                    (list :block-reason block-reason))
                  (superchat-authority--evidence-fields context)
                  evidence-action)
                 nil (and evidence-action t)))
               (_
                (when-let* ((recorded
                             (plist-get context
                                        :evidence-action-recorded-function)))
                  (funcall recorded)))
               (_render-active
                (superchat-authority--require-current-evidence-origin context))
               (context
                (plist-put
                 (copy-sequence context) :causation-id requested-id)))
          ;; ── finalize: every branch delivers through `refuse' or the ──
          ;; exact Sink, so a refusal reaches the consumer as a result does.
          (cond
           (blocked
            (let ((result (format "[Tool call blocked: %s]" block-reason)))
              (superchat-authority--record-failure
               tool-name context execution-id request-hash "blocked" result)
              (funcall refuse result)))
           ((eq decision 'deny)
            (superchat-authority--record-failure
             tool-name context execution-id request-hash "denied"
             "[Tool call denied by authority policy]")
            (funcall refuse "[Tool call denied by authority policy]"))
           ((eq decision 'ask)
            (let* ((approval-requested-id
                    (superchat-authority--append-event
                     "approval.requested" tool-name context
                     (list :execution-id execution-id
                           :request-hash request-hash
                           :effects tape-effects
                           :policy-version
                           (plist-get authority :policy-version))))
                   (approval-context
                    (plist-put
                     (copy-sequence context)
                     :causation-id approval-requested-id))
                   (approval-function
                    (or (plist-get context :approval-function)
                        superchat-authority-approval-function))
                   (approved
                    (and (functionp approval-function)
                         (let ((superchat--tool-execution-context
                                approval-context))
                           (condition-case condition
                               (funcall
                                approval-function
                                (superchat-authority--snapshot-value
                                 request))
                             (quit
                              (let* ((decision-id
                                      (superchat-authority--append-event
                                       "approval.decided" tool-name approval-context
                                       (list :execution-id execution-id
                                             :request-hash request-hash
                                             :decision "deny"
                                             :reason "quit"
                                             :interruption "user-quit"
                                             :policy-version
                                             (plist-get authority :policy-version))))
                                     (decision-context
                                      (plist-put
                                       (copy-sequence approval-context)
                                       :causation-id decision-id)))
                                (superchat-authority--handle-quit
                                 condition tool-name args decision-context
                                 execution-id request-hash "approval"
                                 nil nil nil "cancelled")))))))
                   (_approval-active
                    (superchat-authority--require-current-evidence-origin
                     approval-context))
                   (decision-id
                    (superchat-authority--append-event
                     "approval.decided" tool-name approval-context
                     (list :execution-id execution-id
                           :request-hash request-hash
                           :decision (if approved "allow" "deny")
                           :policy-version
                           (plist-get authority :policy-version))))
                   (decision-context
                    (plist-put
                     (copy-sequence approval-context)
                     :causation-id decision-id)))
              (if approved
                  (superchat-authority--invoke
                   tool-name original-fn args decision-context
                   execution-id request-hash async callback refuse)
                (superchat-authority--record-failure
                 tool-name decision-context execution-id request-hash
                 "cancelled" "[Tool call cancelled by user]")
                (funcall refuse "[Tool call cancelled by user]"))))
           (t
            (superchat-authority--invoke
             tool-name original-fn args context execution-id request-hash
             async callback refuse))))
      (error
       (when refusal-delivery-started
         (signal (car outer-err) (cdr outer-err)))
       ;; Most importantly, a failed pre-execution Tape write lands here before
       ;; ORIGINAL-FN is reached.
       (let* ((evidence (plist-get context :evidence-contract))
              (failure
               (if evidence
                   "[Tool call blocked because exact audit failed]"
                 (format "[Tool call blocked because audit failed: %s]"
                         (error-message-string outer-err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
         (if (and evidence (functionp audit-failure))
             (progn
               (funcall audit-failure outer-err failure)
               (unless async failure))
           (superchat-authority--deliver async callback failure)))))))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(declare-function superchat-tool-execute "superchat-authority")
(superchat-define-plugin authority
  :inject '(tape)
  :doc "Effect resolution, policies and approvals."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'authority
     (superchat-service-table
      decide #'superchat-authority-decide
      resolve-effects #'superchat-authority-resolve-effects
      register-policy #'superchat-authority-register-policy
      register-effect-resolver #'superchat-authority-register-effect-resolver
      register-approval-preview #'superchat-authority-register-approval-preview
      transport-timer-owner #'superchat-authority-transport-timer-owner))))

(provide 'superchat-authority)

;;; superchat-authority.el ends here
