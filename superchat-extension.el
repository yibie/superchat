;;; superchat-extension.el --- Declarative, activation-scoped extensions -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; An extension declares an activation predicate and a set of
;; contributions across the Runtime's orthogonal dimensions: tools for
;; the model surface, deterministic context injection, knowledge for the
;; prompt, plus unconditional commands and Authority policies.
;;
;; The point is activation scoping: an inactive extension costs nothing —
;; its tools stay off the model surface and its knowledge stays out of
;; the prompt.  The low-level registration seams
;; (`superchat-register-tool', `superchat-register-command',
;; `superchat-authority-register-policy') remain the primitives; this
;; module only decides *when* their contributions apply to a Request.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-authority)
(require 'superchat-tape)

(declare-function superchat-register-command "superchat" (name handler))
(declare-function superchat-unregister-command "superchat" (name))

(defvar superchat--command-alist)
(defvar superchat-llm-tool-names)

(defvar superchat-extension--registry nil
  "Alist of (NAME . SPEC) for declared extensions, oldest first.")

(defvar superchat-extension--command-shadows
  (make-hash-table :test 'equal)
  "Command shadow stacks keyed by command name.
Each value is (BASE . STACK), where BASE is (PRESENT-P . VALUE) and STACK
contains (OWNER . HANDLER) entries with the newest owner first.")

(defvar superchat-extension--policy-shadows
  (make-hash-table :test 'eq)
  "Policy shadow stacks keyed by normalized policy symbol.
Each value is (BASE . STACK), where BASE is (PRESENT-P . VALUE) and STACK
contains (OWNER . DECIDER) entries with the newest owner first.")

(defun superchat-extension--validate-commands (commands)
  "Validate COMMANDS and return their actual name/handler bindings."
  (mapcar
   (lambda (command)
     (let ((name (car command))
           (handler (cdr command)))
       (unless (and (stringp name) (not (string-empty-p name)))
         (error "Extension command name must be a non-empty string"))
       (unless (or (functionp handler)
                   (and (symbolp handler) handler))
         (error "Extension command %s requires a callable handler" name))
       (cons name handler)))
   commands))

(defun superchat-extension--validate-policies (policies)
  "Validate POLICIES and return normalized name/decider bindings.

Validation uses a temporary policy registry, so it cannot mutate the live
Authority state before the whole declaration has passed validation."
  (let ((superchat-authority--policy-registry
         (make-hash-table :test 'eq)))
    (mapcar
     (lambda (policy)
       (let ((decider (cdr policy)))
         (unless (functionp decider)
           (error "Extension policy %s requires a callable decider"
                  (car policy)))
         (cons (superchat-authority-register-policy (car policy) decider)
               decider)))
     policies)))

(defun superchat-extension--command-base-binding (name)
  "Return the live BASE binding record for command NAME."
  (let ((binding (and (boundp 'superchat--command-alist)
                      (assoc name superchat--command-alist))))
    (cons (and binding t)
          (and binding (cdr binding)))))

(defun superchat-extension--policy-base-binding (name)
  "Return the live BASE binding record for normalized policy NAME."
  (let* ((missing (make-symbol "missing"))
         (value (if (hash-table-p superchat-authority--policy-registry)
                    (gethash name superchat-authority--policy-registry
                             missing)
                  missing)))
    (cons (not (eq value missing))
          (unless (eq value missing) value))))

(defun superchat-extension--command-shadow (name)
  "Return or initialize the shadow record for command NAME."
  (or (gethash name superchat-extension--command-shadows)
      (let ((shadow (cons (superchat-extension--command-base-binding name)
                          nil)))
        (puthash name shadow superchat-extension--command-shadows)
        shadow)))

(defun superchat-extension--policy-shadow (name)
  "Return or initialize the shadow record for normalized policy NAME."
  (or (gethash name superchat-extension--policy-shadows)
      (let ((shadow (cons (superchat-extension--policy-base-binding name)
                          nil)))
        (puthash name shadow superchat-extension--policy-shadows)
        shadow)))

(defun superchat-extension--shadow-without-owner (stack owner)
  "Return STACK without entries owned by OWNER."
  (cl-remove-if (lambda (entry) (eq owner (car entry))) stack))

(defun superchat-extension--register-command-shadow (owner name handler)
  "Record OWNER's command NAME/HANDLER and register the live binding."
  (let* ((shadow (superchat-extension--command-shadow name))
         (stack (superchat-extension--shadow-without-owner
                 (cdr shadow) owner)))
    (setcdr shadow (cons (cons owner handler) stack))
    (superchat-register-command name handler)))

(defun superchat-extension--register-policy-shadow (owner name decider)
  "Record OWNER's policy NAME/DECIDER and register the live binding."
  (let* ((shadow (superchat-extension--policy-shadow name))
         (stack (superchat-extension--shadow-without-owner
                 (cdr shadow) owner)))
    (setcdr shadow (cons (cons owner decider) stack))
    (superchat-authority-register-policy name decider)))

(defun superchat-extension--remove-command-shadow (owner name)
  "Remove OWNER from command NAME's shadow and restore its live binding.
When the stack becomes empty, discard the shadow record so a later extension
snapshots the then-current live binding as its new BASE."
  (when-let* ((shadow (gethash name superchat-extension--command-shadows)))
    (let* ((stack (cdr shadow))
           (top (car stack))
           (owned (assq owner stack)))
      (when owned
        (setcdr shadow (superchat-extension--shadow-without-owner
                        stack owner))
        (when (and (eq owner (car top))
                   (boundp 'superchat--command-alist)
                   (let ((current (assoc name superchat--command-alist)))
                     (and current (eq (cdr current) (cdr top)))))
          (let ((next (car (cdr shadow)))
                (base (car shadow)))
            (cond
             (next
              (superchat-register-command name (cdr next)))
             ((car base)
              (superchat-register-command name (cdr base)))
             (t
              (superchat-unregister-command name)))))
        (unless (cdr shadow)
          (remhash name superchat-extension--command-shadows))))))

(defun superchat-extension--remove-policy-shadow (owner name)
  "Remove OWNER from policy NAME's shadow and restore its live binding.
When the stack becomes empty, discard the shadow record so a later extension
snapshots the then-current live binding as its new BASE."
  (when-let* ((shadow (gethash name superchat-extension--policy-shadows)))
    (let* ((stack (cdr shadow))
           (top (car stack))
           (owned (assq owner stack)))
      (when owned
        (setcdr shadow (superchat-extension--shadow-without-owner
                        stack owner))
        (when (and (eq owner (car top))
                   (hash-table-p superchat-authority--policy-registry))
          (let* ((missing (make-symbol "missing"))
                 (current (gethash name superchat-authority--policy-registry
                                   missing)))
            (when (and (not (eq current missing))
                       (eq current (cdr top)))
              (let ((next (car (cdr shadow)))
                    (base (car shadow)))
                (cond
                 (next
                  (superchat-authority-register-policy name (cdr next)))
                 ((car base)
                  (superchat-authority-register-policy name (cdr base)))
                 (t
                  (superchat-authority-unregister-policy name)))))))
        (unless (cdr shadow)
          (remhash name superchat-extension--policy-shadows))))))

(defun superchat-extension--unregister-seam-contributions
    (owner spec &optional retained-commands retained-policies)
  "Remove OWNER's old seam contributions from SPEC.
Only names absent from RETAINED-COMMANDS and RETAINED-POLICIES are removed;
same-name replacements already updated their shadow-stack entries."
  (dolist (command (plist-get spec :commands))
    (unless (assoc (car command) retained-commands)
      (superchat-extension--remove-command-shadow owner (car command))))
  (dolist (policy (plist-get spec :policies))
    (unless (assq (car policy) retained-policies)
      (superchat-extension--remove-policy-shadow owner (car policy)))))

;;;###autoload
(cl-defun superchat-define-extension
    (name &key description active-when tools context knowledge
          commands policies)
  "Declare extension NAME with activation-scoped contributions.

DESCRIPTION is a mandatory one-line string; it is shown to users in
`/extensions' and, while the extension is active, to the model.

ACTIVE-WHEN is a predicate called with the Runtime Request plist; nil
means the extension is active for every Request.  Prefer narrow
predicates — activation scoping is what keeps the tool surface small.

TOOLS is a list of tool-name strings (registered separately through
`superchat-register-tool' or built-in).  Tools claimed here leave the
global model surface and are composed into a Request only while this
extension is active, and only when the Request does not carry an
explicit `:tools' list of its own.  TOOLS is only an ownership claim here;
this function neither registers nor unregisters those tools.

CONTEXT is a function called with the Request; a non-empty string
result is appended to the prompt.  This is the home for deterministic
relevance decisions that should not burn a model round trip.

KNOWLEDGE is a string, or a function of the Request returning one,
appended to the system prompt while active.  Keep it short: it is the
`how and when' briefing, not documentation.

COMMANDS is an alist of (NAME . HANDLER) slash commands and POLICIES an
alist of (NAME . DECIDER) Authority policies; both register immediately
and unconditionally through the public seams.

Declaring NAME again registers the new commands and policies first, then
removes names absent from the new declaration.  Contributions are tracked
by NAME in shadow stacks owned by the extension symbol, so a shared handler
object cannot transfer ownership.  Returns NAME."
  (unless (symbolp name)
    (error "Extension name must be a symbol"))
  (unless (and (stringp description) (not (string-empty-p description)))
    (error "Extension %s requires a one-line :description" name))
  (when (and active-when (not (functionp active-when)))
    (error "Extension %s :active-when must be a function" name))
  (when (and context (not (functionp context)))
    (error "Extension %s :context must be a function" name))
  (when (and knowledge
             (not (stringp knowledge))
             (not (functionp knowledge)))
    (error "Extension %s :knowledge must be a string or function" name))
  (dolist (tool tools)
    (unless (and (stringp tool) (not (string-empty-p tool)))
      (error "Extension %s :tools must be tool-name strings" name)))
  (let* ((validated-commands
          (superchat-extension--validate-commands commands))
         (validated-policies
          (superchat-extension--validate-policies policies))
         (old-spec (cdr (assq name superchat-extension--registry)))
         (new-spec (list :description description
                         :active-when active-when
                         :tools tools
                         :context context
                         :knowledge knowledge
                         :commands validated-commands
                         :policies validated-policies)))
    (when (and validated-commands
               (not (fboundp 'superchat-register-command)))
      (error "Extension %s cannot register commands without superchat" name))
    (dolist (command validated-commands)
      (superchat-extension--register-command-shadow
       name (car command) (cdr command)))
    (dolist (policy validated-policies)
      (superchat-extension--register-policy-shadow
       name (car policy) (cdr policy)))
    (setf (alist-get name superchat-extension--registry) new-spec)
    (when old-spec
      (superchat-extension--unregister-seam-contributions
       name old-spec validated-commands validated-policies))
    name))

(defun superchat-extension-unregister (name)
  "Remove extension NAME and its owned seam registrations.

The command and policy shadow stacks remove NAME as an owner.  A live
binding changes only when NAME was the stack top and the current value is
still that top entry; the next owner or the original BASE binding is then
restored.  `:tools' entries are only claims on separately registered tools,
so this function does not register or unregister any tool.  Returns non-nil
when NAME was declared."
  (when-let* ((entry (assq name superchat-extension--registry)))
    (superchat-extension--unregister-seam-contributions name (cdr entry))
    (setf (alist-get name superchat-extension--registry nil 'remove) nil)
    t))

(defun superchat-extension-claimed-tool-p (tool-name)
  "Return non-nil when TOOL-NAME is claimed by any declared extension.
Claimed tools stay off the global model surface and appear only through
activation composition or an explicit request-local tool list."
  (cl-some (lambda (entry)
             (member tool-name (plist-get (cdr entry) :tools)))
           superchat-extension--registry))

(defun superchat-extension--active (request)
  "Return the (NAME . SPEC) entries active for REQUEST."
  (cl-remove-if-not
   (lambda (entry)
     (let ((predicate (plist-get (cdr entry) :active-when)))
       (or (null predicate)
           (ignore-errors (funcall predicate request)))))
   superchat-extension--registry))

(defun superchat-extension--knowledge-text (spec request)
  "Return SPEC's knowledge string for REQUEST, or nil."
  (let* ((knowledge (plist-get spec :knowledge))
         (text (cond ((functionp knowledge)
                      (ignore-errors (funcall knowledge request)))
                     ((stringp knowledge) knowledge))))
    (when (and (stringp text) (not (string-empty-p (string-trim text))))
      (string-trim text))))

(defun superchat-extension--briefing (active request)
  "Return the system-prompt briefing for ACTIVE extensions, or nil."
  (when active
    (string-join
     (cons "[Active Superchat extensions]"
           (mapcan
            (lambda (entry)
              (let* ((spec (cdr entry))
                     (lines (list (format "- %s: %s"
                                          (car entry)
                                          (plist-get spec :description))))
                     (knowledge (superchat-extension--knowledge-text
                                 spec request)))
                (if knowledge
                    (append lines (list knowledge))
                  lines)))
            active))
     "\n")))

(defun superchat-extension--compose-tools (request active)
  "Return REQUEST with ACTIVE extensions' tools merged in.
An explicit `:tools' value — including `none' — is a narrowing decision
the composition never widens.  With the allowlist set to `all' every
tool is already exposed, so nothing needs merging."
  (let ((extension-tools
         (delete-dups
          (mapcan (lambda (entry)
                    (copy-sequence (plist-get (cdr entry) :tools)))
                  active))))
    (if (or (null extension-tools)
            (plist-get request :tools)
            (eq superchat-llm-tool-names 'all))
        request
      (plist-put request :tools
                 (delete-dups
                  (append (and (listp superchat-llm-tool-names)
                               (copy-sequence superchat-llm-tool-names))
                          extension-tools))))))

(defun superchat-extension--record (request active)
  "Append the activation audit event for REQUEST when it has a Session."
  (when-let* ((session-id (plist-get request :session-id))
              ((stringp session-id))
              ((not (string-empty-p session-id))))
    (ignore-errors
      (superchat-tape-append-event
       (list :type "extension.activated"
             :session-id session-id
             :actor-id "runtime"
             :summary (format "extension.activated: %s"
                              (mapconcat (lambda (entry)
                                           (symbol-name (car entry)))
                                         active ", "))
             :payload
             (list :extensions
                   (mapcar (lambda (entry) (symbol-name (car entry)))
                           active)
                   :tools (plist-get request :tools)))))))

;;;###autoload
(defun superchat-extension-compose-request (request &optional acceptance-function)
  "Return REQUEST with active extensions' contributions composed in.
Idempotent: a Request that already carries composition metadata is
returned unchanged.  Surfaces submitting directly to
`superchat-runtime-submit' call this themselves; the chat dispatcher
calls it for every plan it executes.

When ACCEPTANCE-FUNCTION is non-nil, call it with the final Request before
publishing `extension.activated'.  The function may reject composition by
signalling; it does not grant Extensions routing authority."
  (if (or (null superchat-extension--registry)
          (plist-member request :superchat-extensions))
      (progn
        (when acceptance-function
          (funcall acceptance-function request))
        request)
    (let* ((request (copy-sequence request))
           (active (superchat-extension--active request)))
      (if (null active)
          (progn
            (setq request
                  (plist-put request :superchat-extensions nil))
            (when acceptance-function
              (funcall acceptance-function request))
            request)
        (setq request
              (plist-put request :superchat-extensions
                         (mapcar #'car active)))
        (setq request (superchat-extension--compose-tools request active))
        (dolist (entry active)
          (when-let* ((provider (plist-get (cdr entry) :context))
                      (text (ignore-errors (funcall provider request)))
                      ((stringp text))
                      ((not (string-empty-p (string-trim text)))))
            (setq request
                  (plist-put request :prompt
                             (format "%s\n\n[%s context]\n%s"
                                     (or (plist-get request :prompt) "")
                                     (car entry)
                                     (string-trim text))))))
        (when-let* ((briefing (superchat-extension--briefing
                               active request)))
          (setq request
                (plist-put request :system-prompt
                           (let ((system (plist-get request :system-prompt)))
                             (if (and (stringp system)
                                      (not (string-empty-p system)))
                                 (concat system "\n\n" briefing)
                               briefing)))))
        (when acceptance-function
          (funcall acceptance-function request))
        (superchat-extension--record request active)
        request))))

(defun superchat--cmd-extensions (_cmd _args _input _lang _target-model)
  "List declared extensions with their activation and contributions."
  (if (null superchat-extension--registry)
      '(:type :echo :content "No Superchat extensions are declared.")
    (list
     :type :echo
     :content
     (mapconcat
      (lambda (entry)
        (let ((spec (cdr entry)))
          (format "- %s%s — %s%s"
                  (car entry)
                  (if (plist-get spec :active-when) " (conditional)" "")
                  (plist-get spec :description)
                  (if-let* ((tools (plist-get spec :tools)))
                      (format "  [tools: %s]" (string-join tools ", "))
                    ""))))
      superchat-extension--registry "\n"))))

(provide 'superchat-extension)

;;; superchat-extension.el ends here
