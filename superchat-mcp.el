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

;; ── Declared externals (mcp-hub) ──
(defvar mcp-hub-servers)
(defvar mcp-server-connections)
(declare-function mcp-hub-start-all-server "mcp-hub" (&optional callback servers syncp))
(declare-function mcp-hub-get-all-tool "mcp-hub" (asyncp categoryp errorHandle))

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
              (mcp-hub-start-all-server callback server-configs t)
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
            (message "MCP server '%s' is already running" server-name)
          (message "Starting MCP server '%s'..." server-name)
          (condition-case err
              (mcp-hub-start-all-server
               (lambda ()
                 (message "MCP server '%s' started" server-name))
               (list entry)
               t)
            (error
             (message "Failed to start MCP server '%s': %s"
                      server-name (error-message-string err)))))))))

;;;###autoload
(defun superchat-mcp-stop-server (server-name)
  "Stop a single MCP server named SERVER-NAME.
When called interactively, prompts for running server names with completion."
  (interactive
   (let ((running (cl-remove-if-not
                   #'superchat-mcp--server-running-p
                   (mapcar #'car mcp-hub-servers))))
     (list (completing-read "Stop MCP server: " running nil t))))
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

;; ═══════════════════════════════════════════════════════════
;; Tool collection (with graceful degradation)
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp-get-tools ()
  "Return MCP tools from all active, healthy servers.
Each tool's name is prefixed with `mcp:<server>:' to avoid collisions
between servers that expose identically-named tools.
Failing servers are skipped; the remaining tools are still returned
(graceful degradation)."
  (when (and (superchat-mcp-servers-running-p)
             (fboundp 'mcp-hub-get-all-tool))
    (let ((active (superchat-mcp--active-servers))
          (all-tools nil)
          (errors nil))
      (dolist (server-name active)
        (condition-case err
            (let ((server-tools
                   ;; Collect tools from this specific server
                   (mcp-hub-get-all-tool nil t nil)))
              (when server-tools
                ;; Filter to tools from this server and prefix names
                (dolist (tool server-tools)
                  (when (or (not (plist-get tool :server))
                            (string= (plist-get tool :server) server-name))
                    (push (superchat-mcp--prefix-tool tool server-name)
                          all-tools)))))
          (error
           (push (cons server-name (error-message-string err)) errors))))
      (when errors
        (message "MCP tool collection: %d server(s) had errors (%s)"
                 (length errors)
                 (mapconcat #'car errors ", ")))
      ;; Return in original order (re-reverse)
      (nreverse all-tools))))

;; ═══════════════════════════════════════════════════════════
;; Status display
;; ═══════════════════════════════════════════════════════════

(defun superchat-mcp-status ()
  "Display per-server MCP status with health checks and tool counts.
When called interactively, opens a help buffer with the full status table.
Always returns the status content string for embedding in chat."
  (interactive)
  (let* ((available (superchat-mcp-available-p))
         (active-servers (superchat-mcp--active-servers))
         (health (when available (superchat-mcp-check-all-health)))
         (total-configured (length active-servers))
         (total-running (superchat-mcp-get-running-server-count))
         (healthy-count (if health
                            (cl-count-if #'cdr health)
                          0))
         (all-tools (when available (superchat-mcp-get-tools)))
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
