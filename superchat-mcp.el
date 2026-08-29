;;; superchat-mcp.el --- MCP v2 multi-server orchestration for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; MCP (Model Context Protocol) v2 — per-server lifecycle, health checks,
;; tool namespace disambiguation, graceful degradation.
;;
;; Upgraded from v0.8 (single-server, no namespace) to v1.1:
;;   - mcp:<server>:<tool> namespace
;;   - Explicit start/stop per server
;;   - Health check (lightweight ping)
;;   - Per-session server selection
;;   - Graceful degradation on server failure
;;   - Per-server status table

;;; Code:

(require 'cl-lib)

;; ── Model tool surface ──
;; MCP tools reach the model the way a plugin's tools do: registered into
;; `superchat-tools--external-registry', built into `llm-tool' structs by
;; `superchat-tools--external-llm-tools', wrapped by
;; `superchat--agent-wrap-function' and executed through
;; `superchat-tool-execute'.  This is a hard `require' rather than an
;; `fboundp' probe because a Superchat without the tool surface has
;; nowhere to put an MCP tool at all -- registration silently doing
;; nothing would be a worse failure than not loading.
(require 'superchat-tools)

;; ── Declared externals (mcp-hub) ──
(defvar mcp-hub-servers)
(defvar mcp-server-connections)
(declare-function mcp-hub-start-all-server "mcp-hub" (&optional callback servers syncp))
;; `mcp-hub-get-all-tool' is `(cl-defun mcp-hub-get-all-tool (&key asyncp categoryp))'
;; (mcp-hub.el).  A `&key' lambda list expands to `(&rest ARGS)', which is also
;; what `func-arity' reports for the live function, so that is what we declare.
;; Spelling `(&key asyncp categoryp)' out here instead makes the byte compiler
;; read `&key' as a third *required positional* argument and warn on every
;; keyword call site.  The signature declared until now --
;; `(asyncp categoryp errorHandle)' -- has never existed in any release of
;; mcp.el, and it is what let the positional call below rot unnoticed.
(declare-function mcp-hub-get-all-tool "mcp-hub" (&rest keys))

;; ═══════════════════════════════════════════════════════════
;; Defcustoms
;; ═══════════════════════════════════════════════════════════

(defcustom superchat-mcp-servers nil
  "Subset of `mcp-hub-servers' to use in this chat session.
When nil (default), all configured MCP servers are used.
Set to a list of server name strings to restrict to those servers only.

Examples:
  \\='(\"filesystem\" \"sqlite\")     ; only these two servers
  nil                               ; all configured servers"
  :type '(repeat string)
  :group 'superchat)

(defcustom superchat-mcp-health-timeout 5
  "Timeout in seconds for MCP server health checks.
Applied when running `/mcp` to check if each server is reachable."
  :type 'number
  :group 'superchat)

;; ═══════════════════════════════════════════════════════════
;; Effect declaration
;; ═══════════════════════════════════════════════════════════

(defconst superchat-mcp--effect-kinds
  '(local-read local-write process-exec runtime-exec
    external-read external-write unknown)
  "Closed set of effect kinds an MCP declaration may use.
Authority classifies exactly these seven kinds; a kind outside the set
is not a weaker claim, it is an unreadable one.  This file only
*declares* effects — `superchat-tools.el' owns what a declaration means
and `superchat-authority.el' owns the decision taken from it.")

(defconst superchat-mcp--effect-descriptor-type
  `(list :tag "Effect descriptor"
         (const :format "" :effect)
         (choice :tag "Kind"
                 ,@(mapcar (lambda (kind) (list 'const kind))
                           superchat-mcp--effect-kinds))
         (const :format "" :resource)
         (symbol :tag "Resource")
         (checklist :inline t
                    (list :inline t
                          (const :format "" :scope)
                          (sexp :tag "Scope"))))
  "Customize type of a single effect descriptor.
The shape is \(:effect KIND :resource RESOURCE [:scope SCOPE]) — the
same descriptor `superchat-register-tool' accepts as `:effects'.")

(defcustom superchat-mcp-server-effects nil
  "Effects declared for the tools of each MCP server.

Authority never learns tool names, so a tool reaches it only through
what it declares it does.  MCP tools come from a foreign process and
carry no effect information Superchat can read — `mcp.el' does not parse
the protocol's annotations at all — so the declaration is the user's,
made here, per server.

The value is an alist of \(SERVER-NAME . DECLARATION).  SERVER-NAME is a
string naming a key of `mcp-hub-servers'.  DECLARATION is a plist:

  :effects LIST   effect descriptors every tool of that server carries
  :tools   ALIST  reserved for per-tool overrides; nothing reads it yet

Each effect descriptor is \(:effect KIND :resource RESOURCE [:scope SCOPE]),
where KIND is one of `superchat-mcp--effect-kinds'.

A server absent from this alist — or present without a non-nil :effects
— falls back to a single `unknown' descriptor, which is exactly how such
a tool is treated today: Authority asks when a human is attending and
denies when nobody is.  Declaring effects is therefore an opt-in to
fewer prompts, never a way to loosen a decision by omission.

The :tools slot exists so per-tool overrides can be added later without
changing the shape of a declaration written today.  Per-tool override is
NOT implemented; nothing reads :tools yet.

Example:

  ((\"filesystem\" :effects ((:effect local-read :resource file)))
   (\"github\" :effects ((:effect external-read :resource network))))"
  :type `(alist
          :tag "MCP server effect declarations"
          :key-type (string :tag "Server name")
          :value-type
          (list :tag "Declaration"
                (const :format "" :effects)
                (repeat :tag "Effects carried by every tool of this server"
                        ,superchat-mcp--effect-descriptor-type)
                (checklist
                 :inline t
                 (list :inline t
                       (const :format "" :tools)
                       (alist :tag "Per-tool overrides (reserved, unread)"
                              :key-type (string :tag "Tool name")
                              :value-type
                              (repeat ,superchat-mcp--effect-descriptor-type))))))
  :group 'superchat)

(defconst superchat-mcp--unknown-effect
  '(:effect unknown :resource dynamic)
  "The descriptor an MCP server with no usable declaration falls back to.
Kind and resource are what `superchat-authority-resolve-effects' already
invents for a tool that has no effect resolver, so an undeclared server
keeps precisely today's treatment: ask when attended, deny when not.
It carries no :scope — Authority reads :scope only for file effects and
when hashing a `process-exec' or `external-write' scope into the Tape,
and `unknown' over `dynamic' is neither.")

(defun superchat-mcp--effect-descriptor-valid-p (descriptor)
  "Return non-nil if DESCRIPTOR is a shape `superchat-register-tool' can use.
That means a plist carrying an `:effect' drawn from
`superchat-mcp--effect-kinds' and a symbol `:resource'.  Anything else
is not a weaker declaration, it is an unreadable one."
  (condition-case nil
      (and (consp descriptor)
           (keywordp (car descriptor))
           (memq (plist-get descriptor :effect) superchat-mcp--effect-kinds)
           (let ((resource (plist-get descriptor :resource)))
             (and resource (symbolp resource) t)))
    (error nil)))

(defun superchat-mcp--valid-declaration-p (declared)
  "Return non-nil if DECLARED is a usable list of effect descriptors."
  (and (listp declared)
       (not (null declared))
       (condition-case nil
           (cl-every #'superchat-mcp--effect-descriptor-valid-p declared)
         (error nil))))

(defun superchat-mcp-get-server-effects (server-name)
  "Return the effect descriptors declared for MCP server SERVER-NAME.

A pure lookup in `superchat-mcp-server-effects': it reads no server
state, contacts no hub, and registers nothing.  Handing the result to
`superchat-register-tool' as `:effects' is separate work; this function
only answers what the user declared.

A declared server yields exactly its declared descriptors.  A server
with no declaration — or a declaration whose :effects is empty — yields
a list holding one `unknown' descriptor, because a declaration that
declares nothing is not a claim that the server is harmless.

Per-tool overrides are not consulted: the :tools slot of a declaration
is reserved, and this function is deliberately blind to it.

A declaration Superchat cannot read — a descriptor whose `:effect' is
outside `superchat-mcp--effect-kinds', or that carries no `:resource' —
falls back to the same `unknown' descriptor and says so in a warning.
This is a usability repair, not a security one: an unreadable kind
already resolved to `defer', which Authority collapses to `deny'
unattended and `ask' attended — exactly what `unknown' gets.  What the
warning fixes is the user believing they declared something when they
effectively declared nothing.

The returned structure is fresh, so a caller that rewrites a descriptor
in place cannot corrupt the user's setting."
  (let* ((declaration (and (stringp server-name)
                           (cdr (assoc server-name
                                       superchat-mcp-server-effects))))
         (declared (plist-get declaration :effects)))
    (cond
     ((null declared)
      (list (copy-sequence superchat-mcp--unknown-effect)))
     ((superchat-mcp--valid-declaration-p declared)
      (copy-tree declared))
     (t
      (display-warning
       'superchat
       (format "MCP server `%s' declares effects Superchat cannot read (%S); \
treating the server as undeclared.  Each descriptor must be \
(:effect KIND :resource RESOURCE [:scope SCOPE]) with KIND one of %S."
               server-name declared superchat-mcp--effect-kinds)
       :warning)
      (list (copy-sequence superchat-mcp--unknown-effect))))))

;; ═══════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp--active-servers ()
  "Return the list of server names active for this session.
Filters `mcp-hub-servers' through `superchat-mcp-servers' when set.
Returns a list of strings."
  (let ((all-names (if (superchat-mcp-available-p)
                       (mapcar #'car mcp-hub-servers)
                     nil)))
    (if superchat-mcp-servers
        (cl-intersection all-names superchat-mcp-servers :test #'string=)
      all-names)))

(defun superchat-mcp--server-running-p (server-name)
  "Return non-nil if MCP server SERVER-NAME is currently running.
SERVER-NAME is a string."
  (and (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (gethash server-name mcp-server-connections)))

;; ═══════════════════════════════════════════════════════════
;; Availability
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp-available-p ()
  "Return non-nil if MCP (Model Context Protocol) is available.
MCP is available when `mcp-hub' is loaded and at least one server is
configured in `mcp-hub-servers'."
  (and (featurep 'mcp-hub)
       (boundp 'mcp-hub-servers)
       mcp-hub-servers))

(defun superchat-mcp-servers-running-p ()
  "Return non-nil if any MCP servers are currently running."
  (and (superchat-mcp-available-p)
       (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (> (hash-table-count mcp-server-connections) 0)))

(defun superchat-mcp-get-server-count ()
  "Return the number of configured MCP servers active for this session.
Respects `superchat-mcp-servers' filtering."
  (length (superchat-mcp--active-servers)))

(defun superchat-mcp-get-running-server-count ()
  "Return the number of currently running MCP servers active for this session."
  (let ((count 0))
    (dolist (name (superchat-mcp--active-servers) count)
      (when (superchat-mcp--server-running-p name)
        (setq count (1+ count))))))

;; ═══════════════════════════════════════════════════════════
;; Server lifecycle
;; ═══════════════════════════════════════════════════════════

;;;###autoload
(defun superchat-mcp-start-servers (&optional callback)
  "Start all active MCP servers for this session.
Respects `superchat-mcp-servers' filtering.
When CALLBACK is provided, it is called after servers start."
  (interactive)
  (if (not (superchat-mcp-available-p))
      (message "MCP not available. Please install and configure mcp.el package")
    (let ((servers (superchat-mcp--active-servers)))
      (if (null servers)
          (message "No MCP servers configured. Please set `mcp-hub-servers'")
        (message "Starting %d MCP server(s)..." (length servers))
        ;; Construct the list of server configs for the filtered subset
        (let ((server-configs
               (cl-remove-if-not
                (lambda (entry) (member (car entry) servers))
                mcp-hub-servers)))
          (condition-case err
              (mcp-hub-start-all-server
               ;; The tools of the servers that just connected belong on
               ;; the model surface before CALLBACK runs: a caller that
               ;; issues a request from its callback must find them there.
               (lambda ()
                 (superchat-mcp-register-all-tools)
                 (when callback (funcall callback)))
               server-configs t)
            (error
             (message "Failed to start MCP servers: %s"
                      (error-message-string err)))))))))

;;;###autoload
(defun superchat-mcp-start-server (server-name)
  "Start a single MCP server named SERVER-NAME.
SERVER-NAME is a string matching a key in `mcp-hub-servers'.
When called interactively, prompts for the server name with completion."
  (interactive
   (list (completing-read "Start MCP server: "
                          (mapcar #'car mcp-hub-servers)
                          nil t)))
  (if (not (superchat-mcp-available-p))
      (message "MCP not available")
    (let ((entry (assoc server-name mcp-hub-servers)))
      (if (not entry)
          (message "MCP server '%s' not found in mcp-hub-servers" server-name)
        (if (superchat-mcp--server-running-p server-name)
            ;; Already connected still registers: a server started
            ;; outside Superchat -- or before this file was loaded -- is
            ;; connected with nothing of its own on the surface, and this
            ;; command is how a user says "make this server usable".
            (progn
              (superchat-mcp-register-server-tools server-name)
              (message "MCP server '%s' is already running" server-name))
          (message "Starting MCP server '%s'..." server-name)
          (condition-case err
              (mcp-hub-start-all-server
               (lambda ()
                 (superchat-mcp-register-server-tools server-name)
                 (message "MCP server '%s' started" server-name))
               (list entry)
               t)
            (error
             (message "Failed to start MCP server '%s': %s"
                      server-name (error-message-string err)))))))))

;;;###autoload
(defun superchat-mcp-stop-server (server-name)
  "Stop a single MCP server named SERVER-NAME.
Its tools leave the model tool surface first, together with the
Authority effect resolver and approval preview each of them installed.
When called interactively, prompts for running server names with completion."
  (interactive
   (let ((running (cl-remove-if-not
                   #'superchat-mcp--server-running-p
                   (mapcar #'car mcp-hub-servers))))
     (list (completing-read "Stop MCP server: " running nil t))))
  ;; Unregister before the connection is touched, and whether or not it
  ;; is still up.  A tool whose server is going away can only fail, and
  ;; a server that already died without Superchat noticing must not keep
  ;; its tools on the surface just because stopping it is now a no-op.
  (superchat-mcp-unregister-server-tools server-name)
  (if (not (superchat-mcp--server-running-p server-name))
      (message "MCP server '%s' is not running" server-name)
    (let ((conn (gethash server-name mcp-server-connections)))
      (condition-case err
          (progn
            ;; mcp-server-connections values are connection structs;
            ;; call the server's stop/shutdown if available
            (if (and conn (fboundp 'mcp-server-stop))
                (mcp-server-stop conn)
              ;; Fallback: remove from connections table
              (remhash server-name mcp-server-connections))
            (message "MCP server '%s' stopped" server-name))
        (error
         (message "Error stopping MCP server '%s': %s"
                  server-name (error-message-string err)))))))

;; ═══════════════════════════════════════════════════════════
;; Health check
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp-server-health (server-name)
  "Check if MCP server SERVER-NAME is healthy (reachable and responding).
Returns t if healthy, nil otherwise.
Health is determined by: server is running AND its connection hasn't errored."
  (and (superchat-mcp--server-running-p server-name)
       ;; If we have a connection, check it hasn't errored
       (let ((conn (gethash server-name mcp-server-connections)))
         (if conn
             ;; Most MCP connections track errors internally;
             ;; a nil conn-status or non-error status means healthy
             (not (and (fboundp 'mcp-server-connection-status)
                       (eq 'error (mcp-server-connection-status conn))))
           t))))

(defun superchat-mcp-check-all-health ()
  "Run health checks on all active servers.
Returns an alist of (server-name . healthy-p), where healthy-p is t or nil."
  (mapcar (lambda (name)
            (cons name (superchat-mcp-server-health name)))
          (superchat-mcp--active-servers)))

;; ═══════════════════════════════════════════════════════════
;; Command surface
;; ═══════════════════════════════════════════════════════════

;; `superchat--handle-default-command' calls EVERY built-in command with
;; no arguments at all, and renders the result only when it is a string;
;; anything else is dropped as `:noop'.  That contract is shared by all
;; built-ins, and its authority module is not in the Kernel graph yet, so
;; the three MCP commands adapt to it rather than the other way round.
;;
;; Adapting means two things.  A command that needs a server picks one
;; here, with `completing-read', instead of taking it as an argument --
;; the argument-taking functions above are left untouched, so starting
;; and stopping still register and unregister the server's tools exactly
;; as they did.  And a command that has an answer returns it as a string,
;; so the answer reaches the buffer instead of vanishing.

(defconst superchat-mcp--unavailable-message
  "MCP not available.  Install `mcp.el' and configure `mcp-hub-servers'."
  "What a `/mcp-*' command answers when MCP is not available.
A string rather than nil, because a built-in command that returns a
non-string renders nothing at all, and \"nothing happened\" is the one
answer a user cannot act on.")

(defun superchat-mcp--command-report (content interactively)
  "Return CONTENT, showing it too when INTERACTIVELY is non-nil.
`/mcp-*' commands are reached both from the chat command surface, which
renders the returned string, and from \\[execute-extended-command],
which renders nothing -- so an interactive caller is told by `message'.

INTERACTIVELY has to be passed in.  `called-interactively-p' answers
about the frame it is evaluated in, and the frame here is this helper's
own -- which every command reaches by an ordinary function call, never
by a key sequence.  Asking it here therefore answered nil for `M-x' too,
and the three commands printed nothing on the very door this helper
exists to serve.  Each command evaluates it in its own frame instead,
where the question is the one being asked.

The kind asked for there is `any' rather than `interactive'.  The two
differ only for a caller that reached the command through
`call-interactively' from Lisp, or in a keyboard macro, or under
`noninteractive' -- and every one of those wants the command's answer
shown exactly as \\[execute-extended-command] does.  `interactive' would
also make the repair unobservable in a batch test, since it is nil there
by definition."
  (when interactively
    (message "%s" content))
  content)

;;;###autoload
(defun superchat-mcp-start-server-command ()
  "Start one MCP server, chosen interactively, and report in a string.

The zero-argument door to `superchat-mcp-start-server', which requires a
server name.  Candidates are the servers active for this session, so
`superchat-mcp-servers' filtering decides what can be started here, and
the tool registration that starting performs is unchanged."
  (interactive)
  (let ((interactively (called-interactively-p 'any)))
    (superchat-mcp--command-report
     (if (not (superchat-mcp-available-p))
         superchat-mcp--unavailable-message
       (let ((candidates (superchat-mcp--active-servers)))
         (if (null candidates)
             "No MCP servers are configured for this session."
           (let ((name (completing-read "Start MCP server: " candidates nil t)))
             (if (or (null name) (string-empty-p name))
                 "No MCP server selected."
               (superchat-mcp-start-server name)
               ;; Starting is asynchronous unless the server was already
               ;; up, so the report says which of the two happened rather
               ;; than claiming a connection that may still be pending.
               (if (superchat-mcp--server-running-p name)
                   (format "MCP server `%s' is running; %d tool(s) on the tool surface."
                           name (length (superchat-mcp-registered-tool-names name)))
                 (format "Starting MCP server `%s'; its tools reach the tool surface once it connects."
                         name)))))))
     interactively)))

;;;###autoload
(defun superchat-mcp-stop-server-command ()
  "Stop one running MCP server, chosen interactively, and report in a string.

The zero-argument door to `superchat-mcp-stop-server'.  Only running
servers are offered, because stopping anything else is a no-op the user
should not have to discover by trying it.  The unregistration that
stopping performs is unchanged; the count reported is read before the
call, since afterwards those names are gone by design."
  (interactive)
  (let ((interactively (called-interactively-p 'any)))
    (superchat-mcp--command-report
     (if (not (superchat-mcp-available-p))
         superchat-mcp--unavailable-message
       (let ((running (cl-remove-if-not #'superchat-mcp--server-running-p
                                        (superchat-mcp--active-servers))))
         (if (null running)
             "No MCP server is running."
           (let ((name (completing-read "Stop MCP server: " running nil t)))
             (if (or (null name) (string-empty-p name))
                 "No MCP server selected."
               (let ((exposed (length (superchat-mcp-registered-tool-names name))))
                 (superchat-mcp-stop-server name)
                 (format "Stopped MCP server `%s'; %d tool(s) left the tool surface."
                         name exposed)))))))
     interactively)))

;;;###autoload
(defun superchat-mcp-health-report ()
  "Return a readable health report for the MCP servers active this session.

The zero-argument door to `superchat-mcp-check-all-health', which answers
in an alist.  An alist is not a string, so the command surface used to
drop the whole report as `:noop' and `/mcp-health' printed nothing.  This
formats that same alist and decides nothing about health itself."
  (interactive)
  (let ((interactively (called-interactively-p 'any)))
    (superchat-mcp--command-report
     (if (not (superchat-mcp-available-p))
         superchat-mcp--unavailable-message
       (let ((health (superchat-mcp-check-all-health)))
         (if (null health)
             "No MCP servers are configured for this session."
           (concat
            (format "MCP health — %d of %d server(s) healthy\n\n"
                    (cl-count-if #'cdr health) (length health))
            (mapconcat
             (lambda (entry)
               (let* ((name (car entry))
                      (healthy (cdr entry))
                      (running (superchat-mcp--server-running-p name)))
                 (format "%s %s — %s"
                         (cond ((not running) "—")
                               (healthy "✓")
                               (t "✗"))
                         name
                         (cond ((not running) "stopped")
                               (healthy "healthy")
                               (t "unhealthy")))))
             health "\n")
            "\n"))))
     interactively)))

;; ═══════════════════════════════════════════════════════════
;; Tool namespace
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp--prefix-tool (tool server-name)
  "Prefix TOOL's name with the MCP server namespace.
Returns a new tool plist with `:name' set to \"mcp:SERVER-NAME:ORIGINAL-NAME\".
TOOL is a plist with at least :name.  SERVER-NAME is a string."
  (let ((original-name (plist-get tool :name)))
    (plist-put (copy-sequence tool)
               :name
               (format "mcp:%s:%s" server-name original-name))))

(defconst superchat-mcp--category-prefix "mcp-"
  "Prefix `mcp.el' puts before a server name in a tool's `:category'.")

(defun superchat-mcp--tool-server (tool)
  "Return the name of the MCP server TOOL came from, or nil.
`mcp-hub-get-all-tool' tags every tool it returns with
`:category' \"mcp-<server>\", and only when it is called with
`:categoryp' non-nil.  It never sets a `:server' key, so `:category'
is the only attribution mcp.el offers.  Returns nil for a tool that
carries no usable category, because such a tool cannot be attributed
to a server at all."
  (let ((category (plist-get tool :category)))
    (when (and (stringp category)
               (string-prefix-p superchat-mcp--category-prefix category))
      (substring category (length superchat-mcp--category-prefix)))))

;; ═══════════════════════════════════════════════════════════
;; Tool collection (with graceful degradation)
;; ═══════════════════════════════════════════════════════════

(defconst superchat-mcp--collection-tag 'superchat-mcp--tool-collection
  "Head of the value `superchat-mcp--collect-tools' answers with.
A tool list cannot begin with this symbol -- every element of a tool
list is a plist, so its `car' is a keyword -- which is what lets a
collection and a bare tool list be told apart by sight.")

(defun superchat-mcp--make-collection (ok tools)
  "Build a tool collection: TOOLS, plus whether the hub actually answered.
OK is nil only when the hub was asked and signalled.  It is t when the
hub answered -- including when it answered with nothing -- and t when
there was no hub call to make at all."
  (list superchat-mcp--collection-tag ok tools))

(defun superchat-mcp--collection-p (value)
  "Return non-nil if VALUE was built by `superchat-mcp--make-collection'."
  (and (consp value) (eq (car value) superchat-mcp--collection-tag)))

(defun superchat-mcp--collection-ok-p (collection)
  "Return non-nil if COLLECTION came from a hub that answered.
A value that is not a collection at all -- a bare tool list handed in by
a caller -- counts as an answer, because such a caller has no failure to
report."
  (if (superchat-mcp--collection-p collection)
      (and (nth 1 collection) t)
    t))

(defun superchat-mcp--collection-tools (collection)
  "Return the tools of COLLECTION, or COLLECTION itself if it is a bare list."
  (if (superchat-mcp--collection-p collection)
      (nth 2 collection)
    collection))

(defun superchat-mcp--collect-tools ()
  "Collect this session's MCP tools and say whether the hub answered.

Returns a collection — see `superchat-mcp--make-collection'.  The
distinction it carries is the whole point of this function existing
beside `superchat-mcp-get-tools': an empty tool list and a hub that blew
up are the same nil, and a caller that reconciles the tool surface
against the result has to treat them as opposites.  Reconciling against
a failure would take every MCP tool off the surface — together with its
Authority effect resolver and its approval preview — because a hub that
could not be reached said nothing about them.

`mcp-hub-get-all-tool' already walks every connected server itself, so
it is called exactly ONCE for the whole hub; calling it once per server
returns the full tool set once per server instead.

Attribution comes from each tool's `:category' -- hence the
`:categoryp' argument -- never from `:server', a key mcp.el does not
set.  Every returned tool's name is prefixed with `mcp:<server>:', so
two servers exposing the same tool name yield two distinct entries
rather than one collision.

Tools belonging to servers outside `superchat-mcp-servers' are dropped,
as are tools mcp.el could not attribute to any server."
  (if (not (and (superchat-mcp-servers-running-p)
                (fboundp 'mcp-hub-get-all-tool)))
      ;; No hub call was made, so no hub call failed.  "Nothing is
      ;; connected" is an answer, and a truthful one.
      (superchat-mcp--make-collection t nil)
    (let ((active (superchat-mcp--active-servers))
          (collected nil)
          (ok t))
      (condition-case err
          (dolist (tool (mcp-hub-get-all-tool :categoryp t))
            (let ((server-name (superchat-mcp--tool-server tool)))
              (when (and server-name (member server-name active))
                (push (superchat-mcp--prefix-tool tool server-name)
                      collected))))
        (error
         (message "MCP tool collection failed: %s" (error-message-string err))
         (setq collected nil)
         (setq ok nil)))
      ;; Return in the order the hub reported them.
      (superchat-mcp--make-collection ok (nreverse collected)))))

(defun superchat-mcp-get-tools ()
  "Return MCP tools from the servers active in this session.

The plain list answer, for callers that only want to look: status
displays and counts.  A hub call that fails is reported and yields nil,
because a broken hub costs the tool surface, it does not break the
request — but nil here says nothing about which of the two happened, so
anything that changes the tool surface must go through
`superchat-mcp--collect-tools' instead and read the failure flag."
  (superchat-mcp--collection-tools (superchat-mcp--collect-tools)))

;; ═══════════════════════════════════════════════════════════
;; Model surface registration
;; ═══════════════════════════════════════════════════════════

(defconst superchat-mcp--tool-name-prefix "mcp:"
  "Prefix every MCP tool name Superchat registers begins with.
Built-in tool names are plain identifiers, so nothing carrying this
prefix can collide with one — `superchat-register-tool' rejects a
built-in name outright, and this prefix is what keeps that rejection
from ever being reached by accident.")

(defun superchat-mcp--name-prefix (&optional server-name)
  "Return the registered-name prefix for SERVER-NAME.
Without SERVER-NAME, the prefix shared by every MCP tool.  The trailing
colon matters: without it \"mcp:alpha:\" would also match a tool of a
server called \"alphabet\"."
  (if (and (stringp server-name) (not (string-empty-p server-name)))
      (format "%s%s:" superchat-mcp--tool-name-prefix server-name)
    superchat-mcp--tool-name-prefix))

(defun superchat-mcp-registered-tool-names (&optional server-name)
  "Return the MCP tool names currently on the model tool surface.

With SERVER-NAME, only the names belonging to that server; without it,
every name Superchat registered for MCP.  Names come back in
registration order.

The answer is read from `superchat-tools--external-registry', the table
that actually decides what is exposed, rather than from a ledger kept in
this file.  A ledger would be a second copy of the truth, and the moment
the two disagreed this function would report a tool as exposed after
something else had removed it — or, worse, fail to report one that is
still live and therefore leave it behind at unregistration."
  (let ((prefix (superchat-mcp--name-prefix server-name)))
    (delq nil
          (mapcar (lambda (entry)
                    (let ((name (car entry)))
                      (and (stringp name)
                           (string-prefix-p prefix name)
                           name)))
                  (reverse superchat-tools--external-registry)))))

(defun superchat-mcp--approval-preview (request)
  "Show what MCP call REQUEST would make before it is approved.

An MCP tool comes from a foreign process, so unless the user declared
effects for its server it resolves to `unknown' and Authority asks.  The
approval prompt itself shows the namespaced name, the effects and the
arguments — but not the description the server published, which is the
only statement of what the call actually does.  This preview supplies
it.  Registered for every MCP tool, it is also what makes the removal of
a server's approval previews something a test can observe."
  (let* ((tool-name (plist-get request :tool-name))
         (args (plist-get request :args))
         (spec (cdr (assoc tool-name superchat-tools--external-registry)))
         (description (or (plist-get spec :description)
                          "(no description published)")))
    (with-help-window (format "*Superchat MCP Preview: %s*" tool-name)
      (with-current-buffer standard-output
        (insert (format "=== %s ===\n\n%s\n\nArguments:\n%S\n"
                        tool-name description args))))))

(defun superchat-mcp--tool-description (tool)
  "Return a non-empty description for prefixed MCP TOOL.
`superchat-register-tool' requires one and an MCP server is free to
publish a tool without any; losing the tool over a missing sentence
would be a worse answer than naming it plainly."
  (let ((description (plist-get tool :description)))
    (if (and (stringp description) (not (string-empty-p description)))
        description
      (format "MCP tool %s (the server published no description)."
              (plist-get tool :name)))))

(defun superchat-mcp--register-tool (tool server-name)
  "Register one prefixed MCP TOOL of SERVER-NAME; return its name.

The tool's `:function' is handed to `superchat-register-tool' untouched.
That is the whole point: the wrapping that carries Authority, approval
and the Tape is `superchat--agent-wrap-function', applied later to every
tool on the surface alike, so an MCP tool that arrived here needs — and
must have — no wrapper of its own.

`:effects' is a resolver that looks the declaration up when Authority
asks, not the list the declaration held at this moment.  The declaration
does not vary with the arguments of a call, so a list would seem to do —
but `superchat-register-tool' turns a static list into
\(lambda (_args) LIST), which captures it.  That copy then outlives every
later edit of `superchat-mcp-server-effects': a user who tightens a
declaration, or retracts one so the server should fall back to `unknown',
would change nothing until the server was registered again, and Authority
would keep deciding from a claim the user has already replaced.  The
divergence runs both ways: a frozen `unknown' equally outlives a
declaration that would have relaxed it -- under the `observer' policy a
stale `unknown' denies where the live `local-read' only defers.  Either
direction is wrong, which is why the copy is not allowed to exist.
Looking the declaration up on each resolve keeps the defcustom the single
source of truth."
  (superchat-register-tool
   (plist-get tool :name)
   :function (plist-get tool :function)
   :description (superchat-mcp--tool-description tool)
   :args (plist-get tool :args)
   :async (plist-get tool :async)
   :effects (lambda (_args) (superchat-mcp-get-server-effects server-name))
   :approval-preview #'superchat-mcp--approval-preview))

(defun superchat-mcp-register-server-tools (server-name &optional collection)
  "Put the tools of MCP server SERVER-NAME on the model tool surface.

COLLECTION is the already-collected result of `superchat-mcp--collect-tools'
and defaults to a fresh one.  Registering several servers should pass one
collection in: the hub reports every connected server on each call, so
collecting per server would query it N times for the same answer.  A bare
tool list is accepted too and read as a collection that succeeded.  Only
tools attributed to SERVER-NAME are registered.

Registering a name that is already registered replaces it, so this is
safe to call again after a server's tool list changed — and names the
server no longer exposes are unregistered here rather than left behind
pointing at a tool that has gone.

That sweep runs ONLY when the collection succeeded.  A hub that signalled
reported nothing about this server, and treating \"the hub is down\" as
\"the server exposes nothing any more\" would take the tool, its Authority
effect resolver and its approval preview off the surface over a transient
failure — a removal this function is not permitted to make at all.  So a
failed collection registers nothing and removes nothing: the surface is
left exactly as the last successful collection left it.

A tool `superchat-register-tool' refuses costs only itself: the rest of
the server still reaches the surface, which is the same graceful
degradation `superchat-mcp--collect-tools' applies to a failing hub.

Returns the list of registered names."
  (unless (and (stringp server-name) (not (string-empty-p server-name)))
    (error "MCP tool registration requires a server name"))
  (let* ((collection (or collection (superchat-mcp--collect-tools)))
         (collected-p (superchat-mcp--collection-ok-p collection))
         (tools (superchat-mcp--collection-tools collection))
         (registered nil))
    (dolist (tool tools)
      (when (equal server-name (superchat-mcp--tool-server tool))
        (condition-case err
            (push (superchat-mcp--register-tool tool server-name) registered)
          (error
           (message "MCP tool %s was not registered: %s"
                    (plist-get tool :name) (error-message-string err))))))
    (setq registered (nreverse registered))
    (when collected-p
      (dolist (stale (cl-set-difference
                      (superchat-mcp-registered-tool-names server-name)
                      registered :test #'equal))
        (superchat-unregister-tool stale)))
    registered))

(defun superchat-mcp--registered-tool-server (name)
  "Return the server NAME was registered for, or nil if it is not an MCP name.
NAME is a registered tool name, so its shape is `mcp:<server>:<tool>' —
the shape `superchat-mcp--prefix-tool' builds.  A name that carries the
`mcp:' prefix but no second colon names no server and is answered for
with nil."
  (when (and (stringp name)
             (string-prefix-p superchat-mcp--tool-name-prefix name))
    (let* ((rest (substring name (length superchat-mcp--tool-name-prefix)))
           (colon (cl-position ?: rest)))
      (when (and colon (> colon 0))
        (substring rest 0 colon)))))

(defun superchat-mcp-reconcile-registered-tools ()
  "Take off the surface every MCP tool this session no longer allows.

`superchat-mcp-servers' is a session filter, and before exposure was
decided by registration it was consulted on every single request, so
dropping a server from it stopped that server's tools reaching the model
immediately.  Registration moved that decision to one moment, which is
what makes this function necessary: without it, a server removed from
the filter keeps its tools, its Authority effect resolvers and its
approval previews on the surface for the rest of the session.

The whole `mcp:' namespace is reconciled at once rather than one server
at a time, because the server that has to be cleaned up is precisely the
one no per-server loop visits any more — it is gone from
`superchat-mcp--active-servers'.

Removal goes through `superchat-unregister-tool', so the resolver and
the preview leave with the tool.  Returns the names removed."
  (let ((active (superchat-mcp--active-servers))
        (removed nil))
    (dolist (name (superchat-mcp-registered-tool-names))
      (let ((server (superchat-mcp--registered-tool-server name)))
        (unless (and server (member server active))
          (superchat-unregister-tool name)
          (push name removed))))
    (nreverse removed)))

(defun superchat-mcp-register-all-tools (&optional collection)
  "Register the tools of every running server active in this session.

The session filter is reconciled first, so a server dropped from
`superchat-mcp-servers' loses its tools here even though no later step
visits it.  Then the hub is collected ONCE for all remaining servers and
each is handed its own share of that one collection — including when the
collection is empty, which is why the collection travels as a value that
says whether it succeeded rather than as a list that might be nil for
either reason.  Returns every registered name.

COLLECTION, when given, is a collection a caller has already made — see
`superchat-mcp--collect-tools' — and is used instead of making another.
It exists for a caller that needs the same collection for something else
of its own, `superchat-mcp-status' being the one in the tree: without it
that caller would ask the hub a second time for an answer it is holding.
A collection that records a hub failure is honoured as such, so passing
one cannot talk this function into a sweep it would otherwise refuse.

This function is idempotent: registering a name that is registered
replaces it in place, reconciliation removes only what the session
filter already excludes, and the stale-name sweep finds nothing to
remove when the same collection is registered twice."
  (when (superchat-mcp-available-p)
    (superchat-mcp-reconcile-registered-tools)
    (let ((collection (or collection (superchat-mcp--collect-tools)))
          (registered nil))
      (dolist (server (superchat-mcp--active-servers))
        (when (superchat-mcp--server-running-p server)
          (setq registered
                (append registered
                        (superchat-mcp-register-server-tools
                         server collection)))))
      registered)))

(defun superchat-mcp-unregister-server-tools (server-name)
  "Take every tool of MCP server SERVER-NAME off the model tool surface.

Removal goes through `superchat-unregister-tool', which drops the tool's
Authority effect resolver and approval preview along with the tool
itself.  Removing the tool by hand would leave a resolver behind for a
name nothing can call, and a dangling resolver is silent — it surfaces
only much later, as effects claimed by a tool that never declared them.

Only names carrying this server's prefix are touched, so stopping one
server leaves every other server's tools exactly where they were.
Returns the names removed."
  (when (and (stringp server-name) (not (string-empty-p server-name)))
    (let ((names (superchat-mcp-registered-tool-names server-name)))
      (dolist (name names)
        (superchat-unregister-tool name))
      names)))

;; ═══════════════════════════════════════════════════════════
;; Status display
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp-status ()
  "Display per-server MCP status with health checks and tool counts.
When called interactively, opens a help buffer with the full status table.
Always returns the status content string for embedding in chat.

This is also where the model tool surface is reconciled against the hub.
Registration happens when a server is started through
`superchat-mcp-start-server' or `superchat-mcp-start-servers', and those
are not the only ways a server gets connected: `mcp.el' documents
`mcp-hub-start-all-server' in an init file and a `mcp-hub' UI, and a
server connected that way used to expose nothing at all — silently, while
this very report listed its tools as if the model could call them.  One
idempotent `superchat-mcp-register-all-tools' here makes the report true
of the surface instead of merely true of the hub.

Here rather than at load time because the ordering that actually happens
is the other one: a user starts servers from the `mcp-hub' UI long after
Superchat is up, and a reconcile that ran once at startup would have
nothing to find.  `/mcp' is the door that gets used when MCP looks wrong,
so it is the door that repairs it — and it repairs nothing the hub did
not confirm, because the collection carries whether the hub answered and
`superchat-mcp-register-server-tools' still refuses to sweep without it.

The collection is made once, here, and handed to registration rather than
collected again inside it: the counts below and the tools registered are
then the same single answer from the hub."
  (interactive)
  (let* ((available (superchat-mcp-available-p))
         (collection (and available (superchat-mcp--collect-tools)))
         (_registered (and available
                           (superchat-mcp-register-all-tools collection)))
         (active-servers (superchat-mcp--active-servers))
         (health (when available (superchat-mcp-check-all-health)))
         (total-configured (length active-servers))
         (total-running (superchat-mcp-get-running-server-count))
         (healthy-count (if health
                            (cl-count-if #'cdr health)
                          0))
         (all-tools (and available
                         (superchat-mcp--collection-tools collection)))
         (content
          (concat
           (format "MCP Status — %s\n\n" (if available "available" "not available"))
           (if (not available)
               "Install and configure mcp.el for MCP support.\n"
             (concat
              (format "Active servers: %d configured, %d running, %d healthy\n\n"
                      total-configured total-running healthy-count)
              ;; Per-server table
              "┌──────────────────────┬─────────┬────────┬───────┐\n"
              "│ Server               │ Status  │ Health │ Tools │\n"
              "├──────────────────────┼─────────┼────────┼───────┤\n"
              (mapconcat
               (lambda (entry)
                 (let* ((name (car entry))
                        (healthy (cdr entry))
                        (running (superchat-mcp--server-running-p name))
                        (status (cond
                                 (running "running")
                                 (t "stopped")))
                        (health-str (if running
                                        (if healthy "✓" "✗")
                                      "—"))
                        (tool-count
                         (if all-tools
                             (cl-count-if
                              (lambda (tool)
                                (string-prefix-p
                                 (format "mcp:%s:" name)
                                 (plist-get tool :name)))
                              all-tools)
                           0)))
                   (format "│ %-20s │ %-7s │ %-6s │ %5d │"
                           (truncate-string-to-width name 20)
                           status
                           health-str
                           tool-count)))
               health "\n")
              "\n"
              "└──────────────────────┴─────────┴────────┴───────┘\n\n"
              ;; Active server namespaces info
              "Tool namespace: use `mcp:<server>:<tool>`\n"
              (when (and (> total-running 0) all-tools)
                (concat
                 (format "Total tools available: %d\n" (length all-tools))))
              (when (> (length superchat-mcp-servers) 0)
                (format "Session filter: %s\n"
                        (mapconcat #'identity superchat-mcp-servers ", "))))))))
    ;; Interactive: show help buffer
    (when (called-interactively-p 'interactive)
      (with-help-window "*SuperChat MCP Status*"
        (with-current-buffer standard-output
          (insert content))))
    content))

(provide 'superchat-mcp)
;;; superchat-mcp.el ends here
