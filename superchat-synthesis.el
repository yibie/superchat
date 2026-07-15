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
(declare-function superchat-mcp-get-tools "superchat-mcp" ())

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
  "Return the names of every tool Superchat did not synthesize."
  (append (mapcar #'llm-tool-name
                  (ignore-errors (superchat-get-llm-tools)))
          (mapcar #'llm-tool-name
                  (ignore-errors (superchat-mcp-get-tools)))))

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

(defun superchat--syn-confirm (name description defun-form redefine)
  "Show DEFUN-FORM for NAME and ask the user to approve it.
Returns non-nil when approved.  The code is pretty-printed in a preview
window: a confirmation on code you cannot read is not a gate."
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
        (insert "\nEvery later call asks for confirmation as well.\n")))
    (unwind-protect
        (y-or-n-p (format "Define and register agent-written tool `%s'? " name))
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
            (if (not (superchat--syn-confirm name description defun-form redefine))
                "[Tool definition declined by user]"
              (eval defun-form t)
              (let* ((raw (llm-make-tool
                           :name name
                           :description description
                           :args arg-plists
                           :function (superchat--syn-function-symbol name)))
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
                               superchat-llm-synthesized-tools))))
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
              (llm-make-tool
               :name (llm-tool-name tool)
               :description (llm-tool-description tool)
               :args (llm-tool-args tool)
               :function
               (lambda (name description args body)
                 (superchat--syn-define prompt wrap-fn persist
                                        name description args body)))))
          tools))))))

(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-p "llm" (obj))
(declare-function llm-tool-args "llm" (tool))

(provide 'superchat-synthesis)

;;; superchat-synthesis.el ends here
