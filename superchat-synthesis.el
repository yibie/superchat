;;; superchat-synthesis.el --- Agent-synthesized tools -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The agent writes a tool and starts using it in the same run.
;;
;; `define_tool' takes a name, a description, an argument schema and an
;; elisp body.  Superchat wraps the new tool like any other agent tool and
;; injects it into the *in-flight* llm prompt — so the model sees it on its
;; very next round.  This is only possible because v1.3.2 made the tool loop
;; actually loop, and because every llm.el provider rebuilds its request
;; from `llm-chat-prompt-tools' on each round.
;;
;; What is kept (docs/decisions/accepted/
;; 2026-10-13-a-synthesized-tool-is-a-definition.md):
;;
;;   - A DEFINITION, never a callable.  Every execution builds the tool from
;;     its definition and wraps it with its own wrapper, exactly as it does
;;     a built-in tool.  A wrapped tool carries the execution it was wrapped
;;     in; kept past that execution it refuses every call.
;;   - It belongs to the Workspace it was written in.  One written in a
;;     temporary conversation belongs to that conversation, lives in memory
;;     and is never written to disk.
;;   - The ledger says what happened.  `approved': a person was asked about
;;     this definition and allowed it.  `admitted': the permission level let
;;     it through and nobody looked.  Only an approved definition is loaded
;;     again in a later Emacs; the others wait in `superchat-pending' for
;;     `superchat-tools-review-synthesized'.
;;
;; Safety (docs/goals/homoiconic-agent.md, decisions 1-9):
;;
;;   - A synthesized tool is ALWAYS full Emacs control.  You cannot tell
;;     from a name what arbitrary code does, and "confirm once at definition"
;;     is defeated by argument-dependent behavior: (delete-directory path t)
;;     looks harmless in review and is not harmless when later called with
;;     path="/".  So the permission level treats `define_tool' and every
;;     call of an agent-written tool as full control: asked about per call
;;     under `edit-buffers', refused under `read-only', and let through
;;     unasked under `danger-full-access' (the level `auto' included).  The
;;     question shows the whole arguments, which for `define_tool' is the
;;     code.  Nothing in this file asks a second time.
;;   - Built-in and MCP names cannot be shadowed (an agent that redefines
;;     `read-file' to exfiltrate is a catastrophe you would never notice).
;;     Redefining your OWN earlier synthesis is allowed — iteration is the
;;     payoff of a working loop.
;;   - Sub-agents may synthesize, but only into their own in-flight prompt:
;;     the definition is kept nowhere, the name is forgotten when the
;;     request ends, and a name some conversation holds is refused — so a
;;     shareable sub-agent skill can neither push a tool into the main
;;     agent's toolbox nor change one that is there.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)

(declare-function llm-make-tool "llm")
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-chat-prompt-tools "llm" (prompt))
(declare-function superchat-get-llm-tools "superchat-tools" ())
(declare-function superchat-mcp-registered-tool-names "superchat-mcp"
                  (&optional server-name))
(declare-function superchat-tape-append-event "superchat-tape" (event))

;; ═══════════════════════════════════════════════════════════
;; Configuration and state
;; ═══════════════════════════════════════════════════════════

(defvar superchat-synthesis--table (make-hash-table :test #'equal)
  "The definitions the agent wrote, by whose they are.
A key is a Workspace id, or \"session:ID\" for a conversation that belongs
to no Workspace; a value is a list of definitions, newest first.  A
definition is the plist (:name :description :args :body :key) and nothing
that can be called: `superchat-synthesis-tools' builds the tools anew for
every request, and the request wraps them.

Kept out of `superchat-llm-tools-list' on purpose:
`superchat-llm-tools-reload' rebuilds that list from scratch, and it runs
whenever the agent registry changes — i.e. the next time the user saves a
skill file, every synthesized tool would silently vanish.")

(defvar superchat--syn-names nil
  "Names of the tools some conversation holds a definition of, newest first.
One list for this Emacs, whatever the Workspace: that a name is
agent-written is true of it everywhere, and the permission level reads it.")

(defvar superchat-synthesis--request-names
  (make-hash-table :test #'eq :weakness 'key)
  "In-flight prompt -> names of the tools written into that request only.
A sub-agent's synthesis: `superchat-synthesis-forget-request' drops the
entry when the request ends.")

(defvar superchat--syn-rehydrated nil
  "Non-nil once this Emacs attempted rehydration.")

(defun superchat-synthesis-reset ()
  "Forget every tool the agent wrote, in every Workspace of this Emacs.
Nothing on disk is touched."
  (interactive)
  (clrhash superchat-synthesis--table)
  (clrhash superchat-synthesis--request-names)
  (setq superchat--syn-names nil)
  (message "Synthesized tools cleared."))

(defun superchat--syn-tool-p (name)
  "Return non-nil when NAME is a tool the agent synthesized."
  (and (stringp name)
       (or (member name superchat--syn-names)
           (catch 'found
             (maphash (lambda (_prompt names)
                        (when (member name names) (throw 'found t)))
                      superchat-synthesis--request-names)
             nil))
       t))

;; ═══════════════════════════════════════════════════════════
;; Whose a definition is
;; ═══════════════════════════════════════════════════════════

(defvar superchat--tool-execution-context)
(defvar superchat--session-id)
(declare-function superchat-view-session-configuration "superchat-tape-view"
                  (session-id))

(defun superchat-synthesis-key (session-id workspace-id)
  "The key of the toolbox of SESSION-ID in the Workspace WORKSPACE-ID.
The Workspace's id when the conversation belongs to one; else
\"session:SESSION-ID\"; nil when neither is known."
  (cond ((and (stringp workspace-id) (not (string-empty-p workspace-id))
              (not (string-prefix-p "workspace-tmp-" workspace-id)))
         workspace-id)
        ((and (stringp session-id) (not (string-empty-p session-id)))
         (concat "session:" session-id))))

(defun superchat-synthesis--kept-on-disk-p (key)
  "Non-nil when KEY is a Workspace: its definitions are written to disk."
  (and (stringp key) (not (string-prefix-p "session:" key))))

(defun superchat-synthesis--session-workspace (session-id)
  "The id of the Workspace the conversation SESSION-ID belongs to, or nil."
  (and (stringp session-id)
       (fboundp 'superchat-view-session-configuration)
       (ignore-errors
         (plist-get (superchat-view-session-configuration session-id)
                    :workspace-id))))

(defun superchat-synthesis-current-key (&optional request)
  "The toolbox key of the conversation at hand, or nil.
REQUEST, a plist with `:session-id' and `:workspace-id', is asked first;
then the execution a tool call is part of; then the current buffer's
conversation.  A conversation's Workspace is read from its own record."
  (let* ((context (and (boundp 'superchat--tool-execution-context)
                       (listp superchat--tool-execution-context)
                       superchat--tool-execution-context))
         (session (or (plist-get request :session-id)
                      (plist-get context :session-id)
                      (and (boundp 'superchat--session-id) superchat--session-id)))
         (workspace (or (plist-get request :workspace-id)
                        (plist-get context :workspace-id)
                        (superchat-synthesis--session-workspace session))))
    (superchat-synthesis-key session workspace)))

(defun superchat-synthesis-definitions (&optional key)
  "The definitions of the toolbox KEY, newest first.
KEY defaults to `superchat-synthesis-current-key'; the symbol `all' gives
every toolbox's."
  (if (eq key 'all)
      (let (all)
        (maphash (lambda (_key definitions) (setq all (append definitions all)))
                 superchat-synthesis--table)
        all)
    (when-let* ((key (or key (superchat-synthesis-current-key))))
      (copy-sequence (gethash key superchat-synthesis--table)))))

(defun superchat-synthesis-tools (&optional key)
  "Fresh, unwrapped `llm-tool' structs for the definitions of the toolbox KEY.
Built anew at every call: the request that asks wraps them itself."
  (delq nil
        (mapcar (lambda (definition)
                  (ignore-errors (superchat--syn-make-tool definition)))
                (superchat-synthesis-definitions key))))

(defun superchat-synthesis-names (&optional key)
  "The names of the tools of the toolbox KEY, oldest first."
  (nreverse (mapcar (lambda (definition) (plist-get definition :name))
                    (superchat-synthesis-definitions key))))

(defun superchat-synthesis--keep (key name description arg-plists body)
  "Keep the definition of NAME in the toolbox KEY, replacing an older one."
  (puthash key
           (cons (list :name name :description description
                       :args arg-plists :body body :key key)
                 (cl-remove-if (lambda (definition)
                                 (equal name (plist-get definition :name)))
                               (gethash key superchat-synthesis--table)))
           superchat-synthesis--table)
  (unless (member name superchat--syn-names)
    (push name superchat--syn-names)))

(defun superchat-synthesis-forget-request (prompt)
  "Forget the tools written only into the request of PROMPT; it has ended."
  (when prompt
    (remhash prompt superchat-synthesis--request-names)))

;; ═══════════════════════════════════════════════════════════
;; Validation
;; ═══════════════════════════════════════════════════════════

(defun superchat--syn-existing-names ()
  "Return the names of every tool Superchat did not synthesize.

The MCP half asks the registry what is exposed rather than asking the
hub what it could expose.  It used to call `superchat-mcp-get-tools' and
map `llm-tool-name' over the result, but that function returns the
gptel-style plists `mcp.el' builds, and `llm-tool-name' on a plist
signals `wrong-type-argument' -- outside the `ignore-errors', which
covered only the collection call, so a single connected MCP server broke
`define_tool' outright.

Reading the registry is also the truer question.  What this guard needs
to know is which names are already taken on the tool surface, and since
MCP tools are registered when their server connects, that is exactly
what the registry holds.  It costs no hub round-trip, and it keeps
answering after the hub goes away mid-session."
  (append (mapcar #'llm-tool-name
                  (ignore-errors (superchat-get-llm-tools)))
          (ignore-errors (superchat-mcp-registered-tool-names))))

(defun superchat--syn-valid-name-p (name)
  "Return non-nil when NAME is a usable tool name."
  (and (stringp name)
       (string-match-p "\\`[a-zA-Z][a-zA-Z0-9_-]\\{0,63\\}\\'" name)))

(defun superchat--syn-check-name (name)
  "Return nil when NAME may be defined, or a refusal string.
Shadowing a built-in or MCP tool is refused outright; redefining an
earlier synthesis is allowed."
  (cond
   ((not (superchat--syn-valid-name-p name))
    (format "Error: invalid tool name %S (letters, digits, _ and - only)."
            name))
   ((and (member name (superchat--syn-existing-names))
         (not (superchat--syn-tool-p name)))
    (format "Error: `%s' is already a built-in tool and cannot be redefined. \
Choose a different name." name))
   (t nil)))

(defun superchat--syn-function-symbol (name)
  "Return the elisp symbol backing synthesized tool NAME.
Synthesized functions live in their own namespace so they cannot collide
with anything Superchat or the user defined."
  (intern (concat "superchat-syn--" name)))

(defun superchat--syn-parse-args (args)
  "Parse ARGS, a JSON array of argument objects, into llm.el arg plists.
Signals on malformed input; the caller turns that into a tool-result
string the model can learn from."
  (let ((items (cond
                ((null args) nil)
                ((and (stringp args) (string-empty-p (string-trim args))) nil)
                ((stringp args)
                 (if (fboundp 'json-parse-string)
                     (json-parse-string args :object-type 'plist
                                        :array-type 'list)
                   (json-read-from-string args)))
                ((listp args) args)
                (t (error "args must be a JSON array")))))
    (mapcar
     (lambda (item)
       (let ((name (plist-get item :name))
             (type (or (plist-get item :type) "string"))
             (desc (or (plist-get item :description) ""))
             (opt (plist-get item :optional)))
         (unless (and (stringp name) (not (string-empty-p name)))
           (error "each argument needs a name"))
         (unless (member type '("string" "number" "integer" "boolean"))
           (error "argument `%s' has unsupported type `%s' (use string, number, integer or boolean)"
                  name type))
         (append (list :name name
                       :type (intern type)
                       :description desc)
                 (when (and opt (not (eq opt :false)))
                   (list :optional t)))))
     items)))

;; ═══════════════════════════════════════════════════════════
;; Definition
;; ═══════════════════════════════════════════════════════════

(defun superchat--syn-build-defun (name arg-plists body)
  "Return the `defun' form for synthesized tool NAME.
BODY is the agent's elisp source; ARG-PLISTS name the parameters.
Every argument after a required one is optional to llm.el, so the elisp
lambda list mirrors that with `&optional'."
  (let* ((sym (superchat--syn-function-symbol name))
         (required (cl-remove-if (lambda (a) (plist-get a :optional)) arg-plists))
         (optional (cl-remove-if-not (lambda (a) (plist-get a :optional)) arg-plists))
         (arglist (append (mapcar (lambda (a) (intern (plist-get a :name))) required)
                          (when optional
                            (cons '&optional
                                  (mapcar (lambda (a) (intern (plist-get a :name)))
                                          optional)))))
         ;; Read the whole body as one form.  Reading form-by-form would
         ;; turn unbalanced source into a silent "empty body"; wrapping it
         ;; makes a parse failure say so, and the model can fix it and
         ;; retry — which is what the tool loop is for.
         (forms (cdr (condition-case nil
                         (read (concat "(progn\n" (or body "") "\n)"))
                       (error
                        (error "the body is not valid Emacs Lisp (unbalanced parentheses?)"))))))
    (when (or (null body) (string-empty-p (string-trim body)))
      (error "the tool body is empty"))
    (unless forms
      (error "the tool body has no forms"))
    `(defun ,sym ,arglist
       ,(format "Agent-synthesized tool `%s'." name)
       ,@forms)))

(defun superchat--syn-body-function (name arg-plists body)
  "Return a function that runs BODY, the source of synthesized tool NAME.
Made from the definition each time it is asked for.  It is not bound to
any symbol: two Workspaces can hold a tool of one name, and nothing
written later can change what an earlier definition runs."
  (let ((form (superchat--syn-build-defun name arg-plists body)))
    ;; (defun SYM ARGLIST DOC . FORMS)
    (eval `(lambda ,(nth 2 form) ,@(nthcdr 4 form)) t)))

(defun superchat--syn-make-tool (definition)
  "Return an unwrapped `llm-tool' for DEFINITION."
  (let ((name (plist-get definition :name))
        (arg-plists (plist-get definition :args)))
    (llm-make-tool
     :name name
     :description (plist-get definition :description)
     :args arg-plists
     :function (superchat--syn-guarded-function
                name
                (superchat--syn-body-function
                 name arg-plists (plist-get definition :body))))))

(defun superchat--syn-admission ()
  "How the `define_tool' call under way was let through: `asked' or `level'.
`asked' when the approval question was put for exactly this call and
answered yes -- the question shows the source.  `level' when the
permission level let it through and nobody was asked."
  (if (eq 'asked (plist-get (and (boundp 'superchat--tool-execution-context)
                                 (listp superchat--tool-execution-context)
                                 superchat--tool-execution-context)
                            :admitted-by))
      'asked
    'level))

(defun superchat--syn-define (prompt wrap-fn persist name description args body)
  "Define, register and hot-inject a synthesized tool.

PROMPT is the in-flight `llm-chat-prompt': the new tool is pushed into
its tools slot, so the model can call it on the very next round of this
same run — every provider rebuilds its request from that slot each round.
WRAP-FN wraps the tool exactly like the run's other tools (counter,
guardrails, confirmation, tape); the wrapped tool goes into PROMPT and
nowhere else.

When PERSIST is non-nil the DEFINITION is kept in the conversation's
toolbox (`superchat-synthesis-current-key'): later executions build and
wrap the tool themselves.  A Workspace's definition is also written to
disk, with what the ledger can truthfully say of it.  Sub-agents pass
nil: their tool lives in PROMPT only, and they cannot take a name a
conversation holds.

Returns a string for the model — including the refusal or the error,
which the model can read and act on."
  (condition-case err
      (let ((refusal
             (or (superchat--syn-check-name name)
                 (and (not persist)
                      (member name superchat--syn-names)
                      (format "Error: `%s' is a tool a conversation already holds, \
and a sub-agent cannot redefine it. Choose a different name." name))))
            (key (and persist (superchat-synthesis-current-key))))
        (cond
         (refusal refusal)
         ((and persist (null key))
          (format "Error defining tool `%s': no conversation to keep it in." name))
         (t
          (let* ((arg-plists (superchat--syn-parse-args args))
                 (definition (list :name name :description description
                                   :args arg-plists :body body :key key))
                 ;; Building the tool reads the body: a body that is not
                 ;; Lisp is refused here, before anything is kept.
                 (raw (superchat--syn-make-tool definition)))
            (if (not persist)
                (when prompt
                  (cl-pushnew name (gethash prompt superchat-synthesis--request-names)
                              :test #'equal))
              (superchat-synthesis--keep key name description arg-plists body)
              (condition-case persist-err
                  (let ((hash (superchat--syn-hash name description arg-plists body))
                        (how (superchat--syn-admission)))
                    (when (superchat-synthesis--kept-on-disk-p key)
                      (superchat--syn-persist-entry
                       name description arg-plists body key))
                    (superchat--syn-record-approval
                     hash name (if (eq how 'asked) 'approved 'admitted)
                     how (superchat-synthesis--kept-on-disk-p key)))
                (error
                 (message "superchat: could not persist tool `%s': %s"
                          name (error-message-string persist-err)))))
            ;; Wrapped last: the wrapper consults `superchat--syn-tool-p'
            ;; to decide that this tool is full control.
            (superchat--syn-inject
             prompt (if (functionp wrap-fn) (funcall wrap-fn raw) raw))
            (format "Tool `%s' is defined and available now — you can call it \
on your next step. Each call is admitted under this conversation's permission level." name)))))
    (error
     (format "Error defining tool `%s': %s. Fix the body and try again."
             name (error-message-string err)))))

(defun superchat--syn-set-prompt-tools (prompt tools)
  "Set PROMPT's tool slot to TOOLS after `llm' is available.

Superchat may load before the optional llm package, so expand llm's
cl-defstruct setter only when a real prompt is available."
  (eval `(setf (llm-chat-prompt-tools ',prompt) ',tools) t))

(defun superchat--syn-inject (prompt tool)
  "Add TOOL to PROMPT's tool slot, replacing any tool of the same name."
  (when (and prompt (fboundp 'llm-chat-prompt-tools))
    (let ((name (llm-tool-name tool)))
      (superchat--syn-set-prompt-tools
       prompt
       (cons tool
             (cl-remove-if
              (lambda (existing) (equal name (llm-tool-name existing)))
              (llm-chat-prompt-tools prompt)))))))

;; ═══════════════════════════════════════════════════════════
;; Binding the tool to a request
;; ═══════════════════════════════════════════════════════════

(defconst superchat--syn-define-tool-name "define_tool"
  "Name of the tool the agent uses to write tools.")

(defun superchat--syn-bind (prompt wrap-fn persist)
  "Bind `define_tool' inside PROMPT to this request.

The tool function needs the prompt in order to hot-inject what it
creates, and a tool function only ever receives its declared arguments —
so the binding happens by closure, once the prompt exists.  A dynamic
variable would not survive into llm.el's async callbacks, and a global
would be wrong the moment two sub-agents run at once."
  (when (and prompt
             (fboundp 'llm-chat-prompt-tools)
             (fboundp 'llm-tool-p))
    (let ((tools (ignore-errors (llm-chat-prompt-tools prompt))))
      ;; Guard on `llm-tool-p': this runs on every request, including ones
      ;; whose tool list never held `define_tool' at all, and a stray
      ;; non-struct entry must not take the request down with it.
      (when (cl-some (lambda (tool)
                       (and (llm-tool-p tool)
                            (equal (llm-tool-name tool)
                                   superchat--syn-define-tool-name)))
                     tools)
        (superchat--syn-set-prompt-tools
         prompt
         (mapcar
          (lambda (tool)
            (if (not (and (llm-tool-p tool)
                          (equal (llm-tool-name tool)
                                 superchat--syn-define-tool-name)))
                tool
              (let ((bound
                     (llm-make-tool
                      :name (llm-tool-name tool)
                      :description (llm-tool-description tool)
                      :args (llm-tool-args tool)
                      :function
                      (lambda (name description args body)
                        (superchat--syn-define
                         prompt wrap-fn persist name description args body)))))
                (if (functionp wrap-fn)
                    (funcall wrap-fn bound)
                  bound))))
          tools))))))

(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-p "llm" (obj))
(declare-function llm-tool-args "llm" (tool))

;; ═══════════════════════════════════════════════════════════
;; Persistence and rehydration
;;
;; Two files in the data directory.  The STORE holds the definitions of
;; the Workspaces' tools, readable on purpose.  The LEDGER holds, per
;; SHA-256 of a COMPLETE definition (name + description + args + body —
;; the description is a prompt-injection surface, so it is anchored too),
;; what happened to it:
;;
;;   approved  a person was asked about exactly this definition and said
;;             yes (`:how asked'), or approved it in the review (`:how
;;             review')
;;   admitted  the permission level let it through; nobody looked
;;   rejected  refused in the review
;;   deleted   removed from the store in the review
;;
;; On startup only an approved definition is loaded, into the Workspace
;; its store entry names.  Everything else waits: `superchat-pending'
;; lists it and `superchat-tools-review-synthesized' decides it.  The
;; files of versions before 2026-10-13 name no Workspace and their
;; "approved" does not say whether anybody looked (it was written under
;; every permission level alike), so none of them is loaded unseen.
;;
;; The ledger is a separate file so that ordinary writes to the tape
;; database cannot touch it.  That is not a protection against the agent:
;; a tool with full Emacs control -- `eval_elisp', a shell, any tool the
;; agent wrote -- can write this file as it can write any other.  Whether
;; such a tool may run is the permission level's decision.
;; ═══════════════════════════════════════════════════════════

(defvar superchat-data-directory)

(defun superchat--syn-store-file ()
  "Return the human-readable synthesized-tools store path."
  (expand-file-name "synthesized-tools.el"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      user-emacs-directory)))

(defun superchat--syn-approvals-file ()
  "Return the ledger path.
A file of its own, apart from the store and from the tape database.  A
tool with full Emacs control can still write it."
  (expand-file-name "synthesized-approvals.eld"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      user-emacs-directory)))

(defun superchat--syn-hash (name description arg-plists body)
  "Return the SHA-256 anchoring the COMPLETE tool definition."
  (secure-hash 'sha256
               (prin1-to-string (list name description arg-plists body))))

(defun superchat--syn-read-forms (file)
  "Read every form in FILE with a restricted reader.  Nil when absent."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((read-eval nil)
            (read-circle nil)
            (forms nil))
        (goto-char (point-min))
        (condition-case nil
            (while t (push (read (current-buffer)) forms))
          (end-of-file nil))
        (nreverse forms)))))

(defun superchat--syn-load-approvals ()
  "Return the ledger as an alist of (HASH . PLIST)."
  (car (superchat--syn-read-forms (superchat--syn-approvals-file))))

(defun superchat--syn-save-approvals (approvals)
  "Write APPROVALS to the ledger file."
  (with-temp-buffer
    (insert ";; Superchat synthesized-tool ledger — do not edit by hand.\n"
            ";; Each entry says what happened to the definition with this\n"
            ";; SHA-256: approved (a person looked), admitted (the permission\n"
            ";; level let it through, nobody looked), rejected or deleted.\n")
    (pp approvals (current-buffer))
    (write-region (point-min) (point-max)
                  (superchat--syn-approvals-file) nil 'nomessage)))

(defun superchat--syn-record-approval (hash name state &optional how ledger)
  "Record what happened to the definition HASH of the tool NAME.
STATE is `approved', `admitted', `rejected' or `deleted'.  HOW says by what:
`asked' (the approval question of the `define_tool' call), `review' (the
review screen) or `level' (the permission level, nobody asked).  The fact
goes on the Tape; the ledger file is written unless LEDGER is nil, as it is
for a definition that is never written to disk.  The actor is the user
only when a person decided."
  (unless (fboundp 'superchat-tape-append-event)
    (require 'superchat-tape))
  (let ((how (or how 'review))
        (context (and (boundp 'superchat--tool-execution-context)
                      (listp superchat--tool-execution-context)
                      superchat--tool-execution-context)))
    (superchat-tape-append-event
     (list :type "synthesis.approval.decided"
           :session-id "synthesis"
           :actor-id (if (eq how 'level)
                         (let ((actor (plist-get context :actor-id)))
                           (if (and (stringp actor) (not (equal actor "user")))
                               actor
                             "permission-level"))
                       "user")
           :topic "synthesis/approval"
           :summary (format "%s tool `%s' (%s)" state name hash)
           :requires-payload t
           :payload
           (list :definition-hash hash
                 :tool-name name
                 :decision state
                 :how (symbol-name how))))
    (when ledger
      (let ((approvals (assoc-delete-all hash (superchat--syn-load-approvals))))
        (superchat--syn-save-approvals
         (cons (cons hash (list :name name :state state :how how
                                :at (format-time-string "%FT%T%z")))
               approvals))))))

(defun superchat--syn-approval (hash)
  "Return the ledger's plist for HASH, or nil."
  (cdr (assoc hash (superchat--syn-load-approvals))))

(defun superchat--syn-store-entries ()
  "Return the persisted tool entries, newest last."
  (superchat--syn-read-forms (superchat--syn-store-file)))

(defun superchat--syn-write-store (entries)
  "Write ENTRIES as the store file.  Human-readable by design."
  (with-temp-buffer
    (insert ";; Superchat synthesized tools — written by `define_tool'.\n"
            ";; Readable on purpose: review with\n"
            ";; M-x superchat-tools-review-synthesized.  Editing an entry\n"
            ";; invalidates its approval hash; it will not load until\n"
            ";; re-approved.\n\n")
    (dolist (form entries)
      (pp form (current-buffer))
      (insert "\n"))
    (write-region (point-min) (point-max)
                  (superchat--syn-store-file) nil 'nomessage)))

(defun superchat--syn-same-entry-p (entry name workspace-id)
  "Non-nil when ENTRY is the store entry of NAME in WORKSPACE-ID."
  (and (equal name (plist-get entry :name))
       (equal workspace-id (plist-get entry :workspace-id))))

(defun superchat--syn-persist-entry (name description arg-plists body workspace-id)
  "Write or update the store entry of NAME in the Workspace WORKSPACE-ID."
  (superchat--syn-write-store
   (append (cl-remove-if
            (lambda (entry) (superchat--syn-same-entry-p entry name workspace-id))
            (superchat--syn-store-entries))
           (list (list :name name
                       :description description
                       :args arg-plists
                       :body body
                       :workspace-id workspace-id
                       :updated (format-time-string "%FT%T%z"))))))

(defun superchat--syn-delete-entry (name &optional workspace-id)
  "Remove the store entry of NAME in WORKSPACE-ID; nothing else is touched.
A nil WORKSPACE-ID is the entry of an earlier version, which names none."
  (superchat--syn-write-store
   (cl-remove-if
    (lambda (entry) (superchat--syn-same-entry-p entry name workspace-id))
    (superchat--syn-store-entries))))

(defun superchat--syn-guarded-function (name &optional function)
  "Return a callable for synthesized tool NAME that contains its errors.
FUNCTION is what runs; it defaults to the symbol
`superchat--syn-function-symbol' gives.  Whether the call may run at all
was decided before this body is reached, by the permission level in the
tool pipeline: nothing here asks."
  (let ((sym (or function (superchat--syn-function-symbol name))))
    (lambda (&rest args)
      ;; A synthesized body is model-written and may signal (an unbound var,
      ;; a wrong call).  Catch it here, so it comes back as a result the
      ;; model can read and fix instead of escaping into llm.el's async
      ;; response timer.
      (condition-case err
          (apply sym args)
        (error
         (format "[Synthesized tool `%s' errored: %s. Fix the body and try again.]"
                 name (error-message-string err)))))))

(defun superchat--syn-register (key name description arg-plists body)
  "Keep the definition of NAME, from persisted parts, in the toolbox KEY.
Signals when the body is not Lisp, or when NAME would shadow a tool that
now exists but did not when the definition was written — a package update
or a freshly enabled MCP server can introduce such a collision between
sessions."
  (let ((refusal (superchat--syn-check-name name)))
    (when refusal (error "%s" refusal)))
  (superchat--syn-body-function name arg-plists body)
  (superchat-synthesis--keep key name description arg-plists body))

(defun superchat--syn-entry-status (entry)
  "Return (STATUS . REASON) for the store ENTRY.
STATUS is `load' (approved by a person, into a named Workspace), `quiet'
\(rejected or deleted) or `review' with the REASON it waits for one."
  (let* ((name (plist-get entry :name))
         (approval (superchat--syn-approval
                    (superchat--syn-hash name
                                         (plist-get entry :description)
                                         (plist-get entry :args)
                                         (plist-get entry :body))))
         (state (plist-get approval :state)))
    (cond
     ((not (superchat--syn-valid-name-p name))
      '(review . "its name is not a tool name"))
     ((memq state '(rejected deleted)) '(quiet))
     ((eq state 'admitted)
      '(review . "let through by the permission level; nobody looked at it"))
     ((not (eq state 'approved))
      '(review . "changed or never reviewed"))
     ((not (and (plist-get approval :how)
                (superchat-synthesis--kept-on-disk-p (plist-get entry :workspace-id))))
      '(review . "from an earlier version: it names no workspace, and its approval does not say whether anybody looked"))
     (t '(load)))))

(defun superchat-synthesis-awaiting-review ()
  "The stored tools that are not loaded and wait to be looked at.
Each is (:name NAME :workspace-id ID :reason TEXT).  Read from the two
files; nothing is written."
  (delq nil
        (mapcar (lambda (entry)
                  (pcase (superchat--syn-entry-status entry)
                    (`(review . ,reason)
                     (list :name (plist-get entry :name)
                           :workspace-id (plist-get entry :workspace-id)
                           :reason reason))))
                (superchat--syn-store-entries))))

(defun superchat-synthesis-rehydrate (&optional force)
  "Load the stored tools a person approved, each into its Workspace.

Hash-anchored and silent: an entry loads without interaction only when
the SHA-256 of its complete definition matches a ledger line that says a
person approved it, and the entry names its Workspace.  Everything else —
admitted without review, changed, never reviewed, or written by a version
that recorded neither — is left for `superchat-tools-review-synthesized'
and listed by `superchat-pending'; rejected and deleted hashes stay
quiet.  Nothing on disk is written.  Idempotent per Emacs unless FORCE."
  (interactive "P")
  (when (or force (not superchat--syn-rehydrated))
    (setq superchat--syn-rehydrated t)
    (let ((loaded 0) (pending nil))
      (dolist (entry (superchat--syn-store-entries))
        (let ((name (plist-get entry :name)))
          (pcase (superchat--syn-entry-status entry)
            ('(load)
             (condition-case err
                 (progn (superchat--syn-register
                         (plist-get entry :workspace-id)
                         name
                         (plist-get entry :description)
                         (plist-get entry :args)
                         (plist-get entry :body))
                        (cl-incf loaded))
               (error (push (cons name (error-message-string err)) pending))))
            (`(review . ,reason) (push (cons name reason) pending)))))
      (when (> loaded 0)
        (message "superchat: %d synthesized tool%s rehydrated"
                 loaded (if (= loaded 1) "" "s")))
      (when pending
        (message "superchat: %d synthesized tool%s awaiting review — M-x superchat-tools-review-synthesized"
                 (length pending) (if (= (length pending) 1) "" "s")))
      (list :loaded loaded :pending pending))))

(declare-function superchat-workspace-of-directory "superchat-workspace" (dir))

(defun superchat--syn-review-workspace (name)
  "The id of the Workspace to approve the tool NAME into, asking if need be.
The Workspace of the conversation at hand; without one, the Workspace of a
directory the user names.  Signals when that directory is in none."
  (let ((key (superchat-synthesis-current-key)))
    (if (superchat-synthesis--kept-on-disk-p key)
        key
      (let* ((dir (read-directory-name
                   (format "Approve `%s' into the workspace at directory: " name)))
             (workspace (and (fboundp 'superchat-workspace-of-directory)
                             (superchat-workspace-of-directory dir))))
        (or (plist-get workspace :id)
            (user-error "%s is in no workspace" (abbreviate-file-name dir)))))))

(defun superchat-tools-review-synthesized (&optional only)
  "Look at the stored tools that are not loaded, and decide each.
With ONLY, a tool name, just that tool (`superchat-pending' passes the
row's).

Entries appear here when nobody looked at them (the permission level let
the definition through), when their content changed since approval, or
when an earlier version wrote them.  For each, the full definition is
shown and the choice is (a)pprove — into its Workspace, or, when it names
none, into the Workspace of this conversation or of a directory you name;
the ledger records your approval and the tool is loaded now — (r)eject —
the ledger remembers the hash as refused — (d)elete — the entry leaves
the store file and the ledger marks the hash deleted; nothing else on
disk is touched — or (s)kip."
  (interactive)
  (let ((reviewed 0))
    (dolist (entry (superchat--syn-store-entries))
      (let* ((name (plist-get entry :name))
             (description (plist-get entry :description))
             (arg-plists (plist-get entry :args))
             (body (plist-get entry :body))
             (workspace-id (plist-get entry :workspace-id))
             (hash (superchat--syn-hash name description arg-plists body))
             (status (superchat--syn-entry-status entry)))
        (when (and (eq 'review (car status))
                   (or (null only) (equal only name)))
          (cl-incf reviewed)
          (with-help-window "*Superchat: synthesized tool review*"
            (with-current-buffer standard-output
              (insert (format "Stored tool `%s' — %s\n\n" name (cdr status)))
              (insert (format "Description: %s\n\n" description))
              (insert "This code will run inside your Emacs when called\n"
                      "(each call is admitted under the conversation's permission level):\n\n")
              (insert (condition-case err
                          (pp-to-string
                           (superchat--syn-build-defun name arg-plists body))
                        (error (format "%s\n\n[%s]" body (error-message-string err)))))))
          (unwind-protect
              (pcase (read-char-choice
                      (format "`%s': (a)pprove and load, (r)eject, (d)elete, (s)kip? " name)
                      '(?a ?r ?d ?s))
                (?a (condition-case err
                        (let ((target (if (superchat-synthesis--kept-on-disk-p workspace-id)
                                          workspace-id
                                        (superchat--syn-review-workspace name))))
                          ;; Load first: only record approval if it
                          ;; actually registers (a name that now shadows a
                          ;; built-in must not be stamped approved).
                          (superchat--syn-register
                           target name description arg-plists body)
                          (unless (equal target workspace-id)
                            (superchat--syn-delete-entry name workspace-id)
                            (superchat--syn-persist-entry
                             name description arg-plists body target))
                          (superchat--syn-record-approval hash name 'approved 'review t)
                          (message "Approved and loaded `%s'" name))
                      (error
                       (message "Cannot load `%s': %s"
                                name (error-message-string err)))))
                (?r (superchat--syn-record-approval hash name 'rejected 'review t)
                    (message "Rejected `%s'" name))
                (?d (superchat--syn-delete-entry name workspace-id)
                    (superchat--syn-record-approval hash name 'deleted 'review t)
                    (message "Deleted `%s' from the store" name))
                (?s nil))
            (when (get-buffer "*Superchat: synthesized tool review*")
              (quit-windows-on "*Superchat: synthesized tool review*"))))))
    (when (zerop reviewed)
      (message "No stored synthesized tool waits to be looked at."))
    reviewed))

(provide 'superchat-synthesis)

;;; superchat-synthesis.el ends here
