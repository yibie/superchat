;;; superchat-synthesis.el --- Agent-synthesized tools -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The agent writes a tool and starts using it in the same run.
;;
;; `define_tool' takes a name, a description, an argument schema and an
;; elisp body.  Superchat confirms the code with the user, defuns it into
;; the `superchat-syn--' namespace, wraps it like any other agent tool,
;; and injects it into the *in-flight* llm prompt — so the model sees it
;; on its very next round.  This is only possible because v1.3.2 made the
;; tool loop actually loop, and because every llm.el provider rebuilds its
;; request from `llm-chat-prompt-tools' on each round.
;;
;; Safety (docs/goals/homoiconic-agent.md, decisions 1-9):
;;
;;   - A synthesized tool is ALWAYS destructive.  You cannot tell from a
;;     name what arbitrary code does, and "confirm once at definition" is
;;     defeated by argument-dependent behavior: (delete-directory path t)
;;     looks harmless in review and is not harmless when later called with
;;     path="/".  So every call confirms.
;;   - Confirmation is governed by `superchat-agent-confirm-synthesized',
;;     NOT by the global destructive switch.  Turning off confirmation
;;     globally says "I trust the tools superchat ships", which is not the
;;     same as "I trust code an LLM wrote three seconds ago".
;;   - The confirmation shows the pretty-printed code.  A confirmation on
;;     code you cannot read is not a gate.
;;   - Built-in and MCP names cannot be shadowed (an agent that redefines
;;     `read-file' to exfiltrate is a catastrophe you would never notice).
;;     Redefining your OWN earlier synthesis is allowed — iteration is the
;;     payoff of a working loop.
;;   - Sub-agents may synthesize, but only into their own in-flight prompt:
;;     nothing reaches the session registry, so a shareable sub-agent skill
;;     cannot push a tool into the main agent's toolbox.

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

(defcustom superchat-agent-confirm-synthesized t
  "If non-nil, confirm every definition and every call of a synthesized tool.

Deliberately independent of `superchat-agent-confirm-destructive':
disabling confirmation globally is a statement about the tools Superchat
ships and audits, not about code an LLM wrote seconds ago.  To run
synthesized tools unattended you must say so here, explicitly."
  :type 'boolean
  :group 'superchat-agent)

(defvar superchat-llm-synthesized-tools nil
  "Session-scoped `llm-tool' structs the agent wrote for itself.

Kept out of `superchat-llm-tools-list' on purpose:
`superchat-llm-tools-reload' rebuilds that list from scratch, and it runs
whenever the agent registry changes — i.e. the next time the user saves a
skill file, every synthesized tool would silently vanish.")

(defvar superchat--syn-names nil
  "Names of tools synthesized this session, newest first.")

(defun superchat-synthesis-reset ()
  "Forget every tool synthesized this session."
  (interactive)
  (setq superchat-llm-synthesized-tools nil
        superchat--syn-names nil)
  (message "Synthesized tools cleared."))

(defun superchat--syn-tool-p (name)
  "Return non-nil when NAME is a tool the agent synthesized."
  (and (stringp name) (member name superchat--syn-names) t))

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
;; Confirmation — the code, pretty-printed, before it enters the image
;; ═══════════════════════════════════════════════════════════

(defun superchat--syn-confirm (name description defun-form redefine &optional persist)
  "Show DEFUN-FORM for NAME and ask the user to approve it.
Returns non-nil when approved.  The code is pretty-printed in a preview
window: a confirmation on code you cannot read is not a gate.  When
PERSIST is non-nil, the window discloses that approval is recorded to
disk and the tool rehydrates silently in future sessions."
  (if (not superchat-agent-confirm-synthesized)
      t
    (with-help-window "*Superchat: agent-written tool*"
      (with-current-buffer standard-output
        (insert (if redefine
                    "The agent wants to REDEFINE a tool it wrote earlier.\n\n"
                  "The agent wants to define a new tool and call it.\n\n"))
        (insert (format "Name:        %s\n" name))
        (insert (format "Description: %s\n\n" description))
        (insert "This code will run inside your Emacs, with your permissions,\n")
        (insert "every time the agent calls the tool:\n\n")
        (insert (pp-to-string defun-form))
        (insert "\nEvery later call asks for confirmation as well.\n")
        (when persist
          (insert "\nApproving also SAVES this definition to\n")
          (insert (format "  %s\n" (superchat--syn-store-file)))
          (insert "and records its hash as approved, so it LOADS AUTOMATICALLY\n")
          (insert "in future sessions (silently, as long as the code is\n")
          (insert "unchanged).  Editing the file later revokes that approval\n")
          (insert "until you re-review it.\n"))))
    (unwind-protect
        (y-or-n-p
         (format "Define%s agent-written tool `%s'? "
                 (if persist " and persist" "") name))
      (when (get-buffer "*Superchat: agent-written tool*")
        (quit-windows-on "*Superchat: agent-written tool*")))))

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

(defun superchat--syn-define (prompt wrap-fn persist name description args body)
  "Define, confirm, register and hot-inject a synthesized tool.

PROMPT is the in-flight `llm-chat-prompt': the new tool is pushed into
its tools slot, so the model can call it on the very next round of this
same run — every provider rebuilds its request from that slot each round.
WRAP-FN wraps the tool exactly like the run's other tools (counter,
guardrails, confirmation, tape).  When PERSIST is non-nil the tool also
joins the session registry and survives to later turns; sub-agents pass
nil, so their synthesis dies with their prompt.

Returns a string for the model — including the refusal or the error,
which the model can read and act on."
  (condition-case err
      (let ((refusal (superchat--syn-check-name name)))
        (if refusal
            refusal
          (let* ((redefine (superchat--syn-tool-p name))
                 (arg-plists (superchat--syn-parse-args args))
                 (defun-form (superchat--syn-build-defun name arg-plists body)))
            (if (not (superchat--syn-confirm name description defun-form
                                             redefine persist))
                "[Tool definition declined by user]"
              (eval defun-form t)
              (let* ((raw (llm-make-tool
                           :name name
                           :description description
                           :args arg-plists
                           :function (superchat--syn-guarded-function name)))
                     (wrapped (if (functionp wrap-fn) (funcall wrap-fn raw) raw)))
                ;; Name first: the wrapper consults `superchat--syn-tool-p'
                ;; to decide that this tool is destructive.
                (unless (superchat--syn-tool-p name)
                  (push name superchat--syn-names))
                (when persist
                  (setq superchat-llm-synthesized-tools
                        (cons wrapped
                              (cl-remove-if
                               (lambda (tool) (equal name (llm-tool-name tool)))
                               superchat-llm-synthesized-tools)))
                  ;; Phase 2: the confirmation the user just gave IS the
                  ;; approval — persist the definition and anchor it to
                  ;; its hash so future sessions rehydrate silently.
                  (condition-case persist-err
                      (progn
                        (superchat--syn-persist-entry
                         name description arg-plists body)
                        (superchat--syn-record-approval
                         (superchat--syn-hash name description arg-plists body)
                         name 'approved))
                    (error
                     (message "superchat: could not persist tool `%s': %s"
                              name (error-message-string persist-err)))))
                (superchat--syn-inject prompt wrapped)
                (format "Tool `%s' is defined and available now — you can call it \
on your next step. Every call will ask the user for confirmation." name))))))
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
;; Phase 2 — persistence and rehydration
;;
;; Design (docs/research/harness-guardrails.md): approval happens ONCE,
;; at definition time, in front of the pretty-printed source.  What is
;; remembered is not "the user said yes" but the SHA-256 of the COMPLETE
;; definition (name + description + args + body — the description is a
;; prompt-injection surface, so it is anchored too).  On startup,
;; entries whose hash matches an approval load silently; anything else
;; waits for `superchat-tools-review-synthesized'.  This is the
;; `custom-safe-themes' model applied to synthesized tools.
;;
;; The approval ledger lives in its own file, NOT in the tape database:
;; the `sql' tool reaches that database, and an agent must not be able
;; to stamp its own approvals.  `superchat-tool-write-file' refuses the
;; ledger path as well (defense in depth).
;; ═══════════════════════════════════════════════════════════

(defvar superchat-data-directory)

(defun superchat--syn-store-file ()
  "Return the human-readable synthesized-tools store path."
  (expand-file-name "synthesized-tools.el"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      user-emacs-directory)))

(defun superchat--syn-approvals-file ()
  "Return the approval-ledger path.
Kept separate from the store and from the tape database so no
agent-reachable tool can write it."
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
  "Return the approval ledger as an alist of (HASH . PLIST)."
  (car (superchat--syn-read-forms (superchat--syn-approvals-file))))

(defun superchat--syn-save-approvals (approvals)
  "Write APPROVALS to the ledger file."
  (with-temp-buffer
    (insert ";; Superchat synthesized-tool approvals — do not edit by hand.\n"
            ";; Each entry anchors a definition the user reviewed and\n"
            ";; approved (or rejected) to its SHA-256.\n")
    (pp approvals (current-buffer))
    (write-region (point-min) (point-max)
                  (superchat--syn-approvals-file) nil 'nomessage)))

(defun superchat--syn-record-approval (hash name state)
  "Record STATE (`approved' or `rejected') for HASH of tool NAME."
  (unless (fboundp 'superchat-tape-append-event)
    (require 'superchat-tape))
  (superchat-tape-append-event
   (list :type "synthesis.approval.decided"
         :session-id "synthesis"
         :actor-id "user"
         :topic "synthesis/approval"
         :summary (format "%s tool `%s' (%s)" state name hash)
         :requires-payload t
         :payload
         (list :definition-hash hash
               :tool-name name
               :decision state)))
  (let ((approvals (assoc-delete-all hash (superchat--syn-load-approvals))))
    (superchat--syn-save-approvals
     (cons (cons hash (list :name name :state state
                            :at (format-time-string "%FT%T%z")))
           approvals))))

(defun superchat--syn-approval-state (hash)
  "Return `approved', `rejected', or nil for HASH."
  (plist-get (cdr (assoc hash (superchat--syn-load-approvals))) :state))

(defun superchat--syn-store-entries ()
  "Return the persisted tool entries, newest last."
  (superchat--syn-read-forms (superchat--syn-store-file)))

(defun superchat--syn-persist-entry (name description arg-plists body)
  "Write or update the store entry for NAME.  Human-readable by design."
  (let* ((entries (cl-remove-if
                   (lambda (entry) (equal name (plist-get entry :name)))
                   (superchat--syn-store-entries)))
         (entry (list :name name
                      :description description
                      :args arg-plists
                      :body body
                      :updated (format-time-string "%FT%T%z"))))
    (with-temp-buffer
      (insert ";; Superchat synthesized tools — written by `define_tool'.\n"
              ";; Readable on purpose: review with\n"
              ";; M-x superchat-tools-review-synthesized.  Editing an entry\n"
              ";; invalidates its approval hash; it will not load until\n"
              ";; re-approved.\n\n")
      (dolist (form (append entries (list entry)))
        (pp form (current-buffer))
        (insert "\n"))
      (write-region (point-min) (point-max)
                    (superchat--syn-store-file) nil 'nomessage))))

(defvar superchat--syn-call-confirmed nil
  "Non-nil while an outer layer has already gated a synthesized call.
The agent/sub-agent wrappers bind this around the tool invocation they
have themselves confirmed, so the inner guard does not ask twice.")

(defun superchat--syn-guarded-function (name)
  "Return a callable for synthesized tool NAME that self-confirms.

The agent wrapper gates calls in agent mode, but a synthesized tool can
also be attached to a plain non-agent request (e.g. when
`superchat-llm-tools-enabled' is `always'), where no wrapper runs and
nothing else consults `superchat-agent-confirm-synthesized'.  The gate
therefore lives on the tool itself; `superchat--syn-call-confirmed'
suppresses it when the outer layer already asked, so agent mode does not
double-confirm."
  (let ((sym (superchat--syn-function-symbol name)))
    (lambda (&rest args)
      (if (or superchat--syn-call-confirmed
              (not (bound-and-true-p superchat-agent-confirm-synthesized))
              (y-or-n-p
               (format "Run agent-written tool `%s' with args %s? "
                       name (if (> (length (format "%S" args)) 200)
                                "(large — see definition)"
                              (format "%S" args)))))
          ;; A synthesized body is model-written and may signal (an
          ;; unbound var, a wrong call).  Catch it here — on the
          ;; non-agent path there is no wrapper to contain it, and an
          ;; escaping error crashes llm.el's async response timer instead
          ;; of coming back as a result the model can read and fix.
          (condition-case err
              (apply sym args)
            (error
             (format "[Synthesized tool `%s' errored: %s. Fix the body and try again.]"
                     name (error-message-string err))))
        (format "[Call to synthesized tool `%s' declined by user]" name)))))

(defun superchat--syn-register-raw (name description arg-plists body)
  "Define and register NAME from persisted parts.  Returns the tool.
The struct is stored UNWRAPPED, but its function self-confirms via
`superchat--syn-guarded-function' so the per-call gate holds on every
Runtime path.

Signals when NAME would shadow a tool that now exists but did not when
the definition was approved — a package update or a freshly enabled MCP
server can introduce such a collision between sessions."
  (let ((refusal (superchat--syn-check-name name)))
    (when refusal (error "%s" refusal)))
  (eval (superchat--syn-build-defun name arg-plists body) t)
  (let ((tool (llm-make-tool
               :name name
               :description description
               :args arg-plists
               :function (superchat--syn-guarded-function name))))
    (unless (superchat--syn-tool-p name)
      (push name superchat--syn-names))
    (setq superchat-llm-synthesized-tools
          (cons tool
                (cl-remove-if
                 (lambda (existing) (equal name (llm-tool-name existing)))
                 superchat-llm-synthesized-tools)))
    tool))

(defvar superchat--syn-rehydrated nil
  "Non-nil once this session attempted rehydration.")

(defun superchat-synthesis-rehydrate (&optional force)
  "Load persisted synthesized tools whose definitions are approved.

Hash-anchored and silent: an entry loads without interaction only when
the SHA-256 of its complete definition matches a recorded approval —
i.e. it is byte-for-byte what the user once reviewed.  Changed or new
entries are left for `superchat-tools-review-synthesized'; rejected
hashes stay quiet.  Idempotent per session unless FORCE."
  (interactive "P")
  (when (or force (not superchat--syn-rehydrated))
    (setq superchat--syn-rehydrated t)
    (let ((loaded 0) (pending nil))
      (dolist (entry (superchat--syn-store-entries))
        (let* ((name (plist-get entry :name))
               (hash (superchat--syn-hash name
                                          (plist-get entry :description)
                                          (plist-get entry :args)
                                          (plist-get entry :body))))
          (pcase (and (superchat--syn-valid-name-p name)
                      (superchat--syn-approval-state hash))
            ('approved
             (condition-case err
                 (progn (superchat--syn-register-raw
                         name
                         (plist-get entry :description)
                         (plist-get entry :args)
                         (plist-get entry :body))
                        (cl-incf loaded))
               (error (push (cons name (error-message-string err)) pending))))
            ('rejected nil)
            (_ (push (cons name "changed or never reviewed") pending)))))
      (when (> loaded 0)
        (message "superchat: %d synthesized tool%s rehydrated"
                 loaded (if (= loaded 1) "" "s")))
      (when pending
        (message "superchat: %d synthesized tool%s awaiting review — M-x superchat-tools-review-synthesized"
                 (length pending) (if (= (length pending) 1) "" "s")))
      (list :loaded loaded :pending pending))))

(defun superchat-tools-review-synthesized ()
  "Review persisted synthesized tools that are not currently approved.

The exception path, not a routine: entries appear here only when their
content changed since approval, was never reviewed on this machine, or
was previously rejected.  For each, the full definition is shown and
the choice is (a)pprove — record the hash and load now, (r)eject —
remember the hash as refused, (d)elete — drop the entry from the store,
or (s)kip."
  (interactive)
  (let ((entries (superchat--syn-store-entries))
        (reviewed 0))
    (dolist (entry entries)
      (let* ((name (plist-get entry :name))
             (description (plist-get entry :description))
             (arg-plists (plist-get entry :args))
             (body (plist-get entry :body))
             (hash (superchat--syn-hash name description arg-plists body))
             (state (superchat--syn-approval-state hash)))
        (unless (eq state 'approved)
          (cl-incf reviewed)
          (with-help-window "*Superchat: synthesized tool review*"
            (with-current-buffer standard-output
              (insert (format "Persisted tool `%s' — %s\n\n" name
                              (if (eq state 'rejected)
                                  "previously REJECTED"
                                "changed or never reviewed here")))
              (insert (format "Description: %s\n\n" description))
              (insert "This code will run inside your Emacs when called\n"
                      "(every call still asks for confirmation):\n\n")
              (insert (pp-to-string
                       (superchat--syn-build-defun name arg-plists body)))))
          (unwind-protect
              (pcase (read-char-choice
                      (format "`%s': (a)pprove and load, (r)eject, (d)elete, (s)kip? " name)
                      '(?a ?r ?d ?s))
                (?a (condition-case err
                        (progn
                          ;; Load first: only record approval if it
                          ;; actually registers (a name that now shadows a
                          ;; built-in must not be stamped approved).
                          (superchat--syn-register-raw
                           name description arg-plists body)
                          (superchat--syn-record-approval hash name 'approved)
                          (message "Approved and loaded `%s'" name))
                      (error
                       (message "Cannot load `%s': %s"
                                name (error-message-string err)))))
                (?r (superchat--syn-record-approval hash name 'rejected)
                    (message "Rejected `%s'" name))
                (?d (superchat--syn-delete-entry name)
                    (message "Deleted `%s' from the store" name))
                (?s nil))
            (when (get-buffer "*Superchat: synthesized tool review*")
              (quit-windows-on "*Superchat: synthesized tool review*"))))))
    (when (zerop reviewed)
      (message "All persisted synthesized tools are approved — nothing to review."))))

(defun superchat--syn-delete-entry (name)
  "Remove NAME's entry from the store file."
  (let ((entries (cl-remove-if
                  (lambda (entry) (equal name (plist-get entry :name)))
                  (superchat--syn-store-entries))))
    (with-temp-buffer
      (insert ";; Superchat synthesized tools — written by `define_tool'.\n\n")
      (dolist (form entries)
        (pp form (current-buffer))
        (insert "\n"))
      (write-region (point-min) (point-max)
                    (superchat--syn-store-file) nil 'nomessage))))

(provide 'superchat-synthesis)

;;; superchat-synthesis.el ends here
