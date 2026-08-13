;;; superchat-authority.el --- Pure authority decisions for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Policy helpers plus the single model-invoked tool execution seam.  Callers
;; provide a small context plist; validation, effects, authority, approval,
;; execution, and Tape ordering stay local to this module.

;;; Code:

(require 'cl-lib)
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
(declare-function superchat--extend-timeout "superchat" ())

(defvar superchat-agent-pre-tool-functions)
(defvar superchat-agent-post-tool-functions)
(defvar superchat-agent-post-tool-failure-functions)
(defvar superchat-tools-default-directory nil)
(defvar superchat--syn-call-confirmed)
(defvar superchat--tool-execution-context nil
  "Effective executor context visible to synchronously-created child runs.")

(defgroup superchat-authority nil
  "Tool effects, policy, approval, and execution."
  :group 'superchat)

(defcustom superchat-authority-approval-function
  #'superchat-authority-confirm
  "Function called with an approval REQUEST plist.
It must return non-nil to approve this exact normalized request."
  :type 'function
  :group 'superchat-authority)

(defconst superchat-authority-policy-version "authority-v1"
  "Base policy version included in approval hashes.")

(defconst superchat-authority-known-policies
  '(default observer developer-project operator cowork)
  "Named Authority policies accepted by the Runtime and UI.
Policies are tighten-only: they may deny or defer, but never allow a tool.")

(defvar superchat-authority--policy-registry (make-hash-table :test 'eq)
  "Plugin-contributed policy deciders keyed by normalized policy symbol.")

(defun superchat-authority--registered-policy (name)
  "Return the registered policy decider for symbol NAME, or nil."
  (and (hash-table-p superchat-authority--policy-registry)
       (gethash name superchat-authority--policy-registry)))

;;;###autoload
(defun superchat-authority-register-policy (name decide)
  "Register tighten-only Authority policy NAME with decider DECIDE.

NAME is a symbol or string; built-in policy names cannot be replaced.
DECIDE is called with (EFFECTS CONTEXT) — the resolved effect
descriptors and the execution context plist — and returns `deny',
`ask', or `defer'.  Named policies may only tighten the base decision,
so an `allow' result is treated as `defer'.  Registering NAME again
replaces the previous decider.  Returns the normalized policy symbol."
  (unless (functionp decide)
    (error "Policy decider for %s must be callable" name))
  (let* ((raw (cond ((keywordp name) (substring (symbol-name name) 1))
                    ((symbolp name) (symbol-name name))
                    ((stringp name) name)
                    (t (error "Invalid authority policy name: %S" name))))
         (normalized (intern (downcase raw))))
    (when (memq normalized superchat-authority-known-policies)
      (error "Authority policy %s is built-in and cannot be replaced"
             normalized))
    (puthash normalized decide superchat-authority--policy-registry)
    normalized))

(defvar superchat-authority-effect-resolvers (make-hash-table :test 'equal)
  "Effect resolver functions keyed by normalized tool name.")

(defvar superchat-authority-approval-previews (make-hash-table :test 'equal)
  "Approval preview functions keyed by normalized tool name.")

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
(defun superchat-authority-register-effect-resolver (tool-name resolver)
  "Register RESOLVER for TOOL-NAME and return RESOLVER."
  (unless (functionp resolver)
    (error "Effect resolver for %s must be callable" tool-name))
  (unless (hash-table-p superchat-authority-effect-resolvers)
    (setq superchat-authority-effect-resolvers
          (make-hash-table :test 'equal)))
  (puthash (if (symbolp tool-name) (symbol-name tool-name) tool-name)
           resolver
           superchat-authority-effect-resolvers)
  resolver)

;;;###autoload
(defun superchat-authority-register-approval-preview (tool-name preview)
  "Register approval PREVIEW for TOOL-NAME and return PREVIEW."
  (unless (functionp preview)
    (error "Approval preview for %s must be callable" tool-name))
  (unless (hash-table-p superchat-authority-approval-previews)
    (setq superchat-authority-approval-previews
          (make-hash-table :test 'equal)))
  (puthash (if (symbolp tool-name) (symbol-name tool-name) tool-name)
           preview
           superchat-authority-approval-previews)
  preview)

;;;###autoload
(defun superchat-authority-resolve-effects (tool-name args)
  "Resolve TOOL-NAME and ARGS into pure effect descriptors."
  (let* ((name (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (resolver
          (and (hash-table-p superchat-authority-effect-resolvers)
               (gethash name superchat-authority-effect-resolvers))))
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
    (tool-name args effects &optional policy-version execution-id)
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
                        :execution-id execution-id)))
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
  (if-let* ((registered (superchat-authority--registered-policy policy)))
      (let ((decision (superchat-authority-decision-from-legacy
                       (funcall
                        registered
                        (superchat-authority--snapshot-value effects)
                        (superchat-authority--snapshot-value context)))))
        ;; Named policies may only tighten: an allow cannot widen the base.
        (if (eq decision 'allow) 'defer decision))
    (superchat-authority--builtin-policy-decision policy effects context)))

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
             (unless (memq kind '(local-read external-read))
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
          (and requires-confirm
               (boundp 'superchat-authority--session-allowlist)
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

(defun superchat-authority--run-hooks (hooks &rest args)
  "Run compatibility HOOKS with ARGS when the old hook runner exists."
  (when (and hooks (fboundp 'superchat--agent-run-tool-hooks))
    (apply #'superchat--agent-run-tool-hooks hooks args)))

(defun superchat-authority--filter-result (tool-name args result)
  "Apply the compatibility result filter to RESULT."
  (if (fboundp 'superchat--agent-filter-tool-result)
      (superchat--agent-filter-tool-result tool-name args result)
    result))

(defun superchat-authority-confirm (request)
  "Ask the user to approve normalized tool REQUEST once.
`y' allows once, `a' allows for the rest of the session (offered only
for non-dangerous tools), `n' denies.  Dangerous process-executing
tools never enter the session allowlist."
  (let* ((tool-name (plist-get request :tool-name))
         (args (plist-get request :args))
         (effects (plist-get request :effects))
         (hash (plist-get request :request-hash))
         (args-text (format "%S" args))
         (display-args
          (if (> (length args-text) 500)
              (concat (substring args-text 0 500) "…")
            args-text))
         (dangerous-p
          (cl-some
           (lambda (effect)
             (memq (plist-get effect :effect)
                   '(process-exec runtime-exec)))
           effects)))
    (when-let* ((preview
                 (and (hash-table-p superchat-authority-approval-previews)
                      (gethash tool-name
                               superchat-authority-approval-previews))))
      (funcall preview (superchat-authority--snapshot-value request)))
    (let ((answer
           (read-multiple-choice
            (format
             "Allow `%s' once?%s\nEffects: %S\nArgs: %s\nRequest: %s "
             tool-name (if dangerous-p "  (dangerous — no always)" "")
             effects display-args hash)
            (if dangerous-p
                '((?y "yes, allow once") (?n "no"))
              '((?y "yes, allow once")
                (?a "always allow this session")
                (?n "no"))))))
      (prog1
          (eq answer ?y)
        (when (eq answer ?a)
          (when (boundp 'superchat-authority--session-allowlist)
            (cl-pushnew tool-name superchat-authority--session-allowlist
                        :test #'equal)))
        (when (fboundp 'superchat--extend-timeout)
          (superchat--extend-timeout))))))

(defun superchat-authority--deliver (async callback result)
  "Return or asynchronously deliver RESULT."
  (if async
      (progn (funcall callback result) nil)
    result))

(defun superchat-authority--record-failure
    (tool-name context execution-id request-hash status
               &optional result exact-error)
  "Record a failed or refused tool execution."
  (let ((record
         (superchat-authority--append-event
          "tool.failed" tool-name context
          (append
           (list :execution-id execution-id
                 :request-hash request-hash
                 :status status)
           (when-let* ((block-reason (plist-get context :block-reason)))
             (list :block-reason block-reason))
           (when-let* ((error-type (plist-get exact-error :error-type)))
             (list :error-type error-type))
           (superchat-authority--evidence-fields context)
           (when (plist-get context :evidence-contract)
             (superchat-authority--exact-fields
              "error" (or exact-error (list :status status :result result)))))
          nil (and (plist-get context :evidence-contract) t) t)))
    (when-let ((callback (plist-get context :failure-recorded-function)))
      (when (functionp callback)
        (funcall callback record)))
    (when-let ((callback (plist-get context :recovery-failure-function)))
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
            :warning)))))
    record))

(defun superchat-authority--finish-success
    (tool-name args result context execution-id request-hash async callback)
  "Filter, audit, and deliver successful RESULT."
  (let* ((evidence (plist-get context :evidence-contract))
         (completed-record nil)
         (result (if evidence
                     (superchat-authority--snapshot-value result)
                   result))
         (audit
         (condition-case err
             (progn
               (superchat-authority--require-current-evidence-origin context)
               ;; Reject sensitive raw results before compatibility hooks can
               ;; render or copy them outside the exact-evidence seam.
               (when evidence
                 (superchat-authority--exact-fields
                  "result" (list :result result)))
               (unless evidence
                 (superchat-authority--run-hooks
                  (and (boundp 'superchat-agent-post-tool-functions)
                       superchat-agent-post-tool-functions)
                  tool-name args result))
               (setq result
                     (superchat-authority--filter-result
                      tool-name args result))
               (when evidence
                 ;; Detach from filter-owned data before persistence and
                 ;; provider delivery.
                 (setq result
                       (superchat-authority--snapshot-value result)))
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
               (when-let ((observer
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
               (when evidence
                 (superchat-authority--run-hooks
                 (and (boundp 'superchat-agent-post-tool-functions)
                       superchat-agent-post-tool-functions)
                  tool-name args
                  (superchat-authority--snapshot-value result)))
               (superchat-authority--require-current-evidence-origin context)
               'complete)
           (error (cons 'failed err)))))
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
                (unless (plist-get context :evidence-contract)
                  (superchat-authority--run-hooks
                   (and (boundp 'superchat-agent-post-tool-failure-functions)
                        superchat-agent-post-tool-failure-functions)
                   tool-name args err))
                (superchat-authority--record-failure
                 tool-name context execution-id request-hash
                 "failed" result exact-error)
                (when (plist-get context :evidence-contract)
                  (superchat-authority--run-hooks
                   (and (boundp 'superchat-agent-post-tool-failure-functions)
                        superchat-agent-post-tool-failure-functions)
                   tool-name args err))
                (superchat-authority--require-current-evidence-origin context)
                'complete)
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

(defun superchat-authority--invoke
    (tool-name original-fn args context execution-id request-hash
               async callback)
  "Invoke ORIGINAL-FN after the authority and audit gates."
  ;; Persist before any compatibility hook or tool code can produce effects.
  (superchat-authority--require-current-evidence-origin context)
  (let* ((started-id
          (superchat-authority--append-event
           "tool.execution.started" tool-name context
           (list :execution-id execution-id
                 :request-hash request-hash
                 :status "executing")))
         (context
          (plist-put (copy-sequence context) :causation-id started-id)))
    (superchat-authority--run-hooks
     (and (boundp 'superchat-agent-pre-tool-functions)
          superchat-agent-pre-tool-functions)
     tool-name (superchat-authority--snapshot-value args))
    (superchat-authority--require-current-evidence-origin context)
    (if async
        (let ((completed nil))
          (condition-case err
              (let ((superchat--syn-call-confirmed t)
                    (superchat--tool-execution-context
                     (superchat-authority--snapshot-value context)))
                (apply
                 original-fn
                 (lambda (result)
                   (unless completed
                     (setq completed t)
                     (superchat-authority--finish-success
                      tool-name args result context execution-id request-hash
                      t callback)))
                 args))
            (error
             (unless completed
               (setq completed t)
               (superchat-authority--finish-error
                tool-name args err context execution-id request-hash
                t callback)))))
      (let ((outcome
             (condition-case err
                 (let ((superchat--syn-call-confirmed t)
                       (superchat--tool-execution-context
                        (superchat-authority--snapshot-value context)))
                   (cons 'ok (apply original-fn args)))
               (error (cons 'error err)))))
        (if (eq (car outcome) 'ok)
            (superchat-authority--finish-success
             tool-name args (cdr outcome) context execution-id request-hash
             nil nil)
          (superchat-authority--finish-error
           tool-name args (cdr outcome) context execution-id request-hash
           nil nil))))))

;;;###autoload
(cl-defun superchat-tool-execute
    (tool-name original-fn args &key async callback context)
  "Execute TOOL-NAME through the shared Authority and Tape seam.

ORIGINAL-FN receives normalized ARGS.  When ASYNC is non-nil it receives
CALLBACK as its first argument; otherwise the filtered result is returned.
CONTEXT must include `:session-id' and explicit `:attendance'."
  (unless (functionp original-fn)
    (error "Tool %s has no callable implementation" tool-name))
  (unless (listp args)
    (error "Tool %s arguments must be a list" tool-name))
  (when (and async (not (functionp callback)))
    (error "Async tool %s requires a callback" tool-name))
  (let ((session-id (plist-get context :session-id))
        (attendance (plist-get context :attendance))
        (policy (superchat-authority-normalize-policy
                 (or (plist-get context :authority-policy)
                     (plist-get context :policy)))))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Tool execution requires context :session-id"))
    (superchat-authority--check-attendance attendance)
    (setq context (plist-put context :authority-policy policy))
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
               (effects
                (superchat-authority--snapshot-value
                 (or (if (functionp resolver)
                         (funcall
                          resolver
                          (superchat-authority--snapshot-value args))
                       (superchat-authority-resolve-effects tool-name args))
                     (list (superchat-authority--effect
                            'unknown 'dynamic tool-name)))))
               (experience-block
                (superchat-authority--experience-block-reason
                 tool-name context))
               (block-reason
                (or experience-block
                    (plist-get context :block)
                    (plist-get intercept :block)))
               (blocked (and block-reason t))
               (context
                (if block-reason
                    (plist-put (copy-sequence context)
                               :block-reason block-reason)
                  context))
               (authority (superchat-authority-decide
                           tool-name args attendance policy effects))
               (decision
                (if blocked
                    'deny
                  (superchat-authority--effective-decision
                   tool-name args authority context)))
               (decision
                (if (eq decision 'defer)
                    (if (eq attendance 'attended) 'ask 'deny)
                  decision))
               (execution-id (or (plist-get context :execution-id)
                                 (superchat-authority--execution-id)))
               (request-hash
                (superchat-authority-approval-hash
                 tool-name args effects
                 (plist-get authority :policy-version)
                 execution-id))
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
          (cond
           (blocked
            (let ((result (format "[Tool call blocked: %s]" block-reason)))
              (superchat-authority--record-failure
               tool-name context execution-id request-hash "blocked" result)
              (superchat-authority--deliver async callback result)))
           ((eq decision 'deny)
            (superchat-authority--record-failure
             tool-name context execution-id request-hash "denied"
             "[Tool call denied by authority policy]")
           (superchat-authority--deliver
             async callback "[Tool call denied by authority policy]"))
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
                   (approved (and (functionp approval-function)
                                  (funcall
                                   approval-function
                                   (superchat-authority--snapshot-value
                                    request))))
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
                   execution-id request-hash async callback)
                (superchat-authority--record-failure
                 tool-name decision-context execution-id request-hash
                 "cancelled" "[Tool call cancelled by user]")
                (superchat-authority--deliver
                 async callback "[Tool call cancelled by user]"))))
           (t
            (superchat-authority--invoke
             tool-name original-fn args context execution-id request-hash
             async callback))))
      (error
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

(provide 'superchat-authority)

;;; superchat-authority.el ends here
