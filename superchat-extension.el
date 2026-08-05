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

(defvar superchat-llm-tool-names)

(defvar superchat-extension--registry nil
  "Alist of (NAME . SPEC) for declared extensions, oldest first.")

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
explicit `:tools' list of its own.

CONTEXT is a function called with the Request; a non-empty string
result is appended to the prompt.  This is the home for deterministic
relevance decisions that should not burn a model round trip.

KNOWLEDGE is a string, or a function of the Request returning one,
appended to the system prompt while active.  Keep it short: it is the
`how and when' briefing, not documentation.

COMMANDS is an alist of (NAME . HANDLER) slash commands and POLICIES an
alist of (NAME . DECIDER) Authority policies; both register immediately
and unconditionally through the public seams.

Declaring NAME again replaces the previous declaration.  Returns NAME."
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
  (dolist (command commands)
    (if (fboundp 'superchat-register-command)
        (superchat-register-command (car command) (cdr command))
      (error "Extension %s cannot register commands without superchat" name)))
  (dolist (policy policies)
    (superchat-authority-register-policy (car policy) (cdr policy)))
  (setf (alist-get name superchat-extension--registry)
        (list :description description
              :active-when active-when
              :tools tools
              :context context
              :knowledge knowledge
              :commands (mapcar #'car commands)
              :policies (mapcar #'car policies)))
  name)

(defun superchat-extension-unregister (name)
  "Remove extension NAME from the registry.
Commands, tools, and policies it registered through the low-level seams
remain registered; only the activation-scoped contributions stop.
Returns non-nil when NAME was declared."
  (when (assq name superchat-extension--registry)
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
(defun superchat-extension-compose-request (request)
  "Return REQUEST with active extensions' contributions composed in.
Idempotent: a Request that already carries composition metadata is
returned unchanged.  Surfaces submitting directly to
`superchat-runtime-submit' call this themselves; the chat dispatcher
calls it for every plan it executes."
  (if (or (null superchat-extension--registry)
          (plist-member request :superchat-extensions))
      request
    (let* ((request (copy-sequence request))
           (active (superchat-extension--active request)))
      (if (null active)
          (plist-put request :superchat-extensions nil)
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
