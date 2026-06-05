;;; superchat-mcp.el --- MCP integration for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; MCP (Model Context Protocol) server management and tool discovery.
;; Extracted from superchat.el monolith (v0.9 split step 3).

;;; Code:

(require 'cl-lib)

(defvar mcp-hub-servers)

(defvar mcp-server-connections)

(defun superchat-mcp-available-p ()
  "Check if MCP (Model Context Protocol) is available."
  (and (featurep 'mcp-hub)
       (boundp 'mcp-hub-servers)
       mcp-hub-servers))

(defun superchat-mcp-servers-running-p ()
  "Check if any MCP servers are running."
  (and (superchat-mcp-available-p)
       (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (> (hash-table-count mcp-server-connections) 0)))

(defun superchat-mcp-get-server-count ()
  "Get number of configured MCP servers."
  (if (superchat-mcp-available-p)
      (length mcp-hub-servers)
    0))

(defun superchat-mcp-get-running-server-count ()
  "Get number of running MCP servers."
  (if (superchat-mcp-servers-running-p)
      (hash-table-count mcp-server-connections)
    0))

;;;###autoload
(defun superchat-mcp-start-servers (&optional callback)
  "Start MCP servers if available.
CALLBACK is called when servers are started."
  (interactive)
  (if (not (superchat-mcp-available-p))
      (message "MCP not available. Please install and configure mcp.el package")
    (if (zerop (superchat-mcp-get-server-count))
        (message "No MCP servers configured. Please set `mcp-hub-servers'")
      (message "Starting %d MCP server(s)..." (superchat-mcp-get-server-count))
      (condition-case err
          (mcp-hub-start-all-server callback nil t)
        (error
         (message "Failed to start MCP servers: %s" (error-message-string err)))))))

(defun superchat-mcp-get-tools ()
  "Get MCP tools if available."
  (when (and (superchat-mcp-servers-running-p)
             (fboundp 'mcp-hub-get-all-tool))
    ;; Call with 3 positional args: asyncp categoryp errorHandle
    (mcp-hub-get-all-tool nil t nil)))

(defun superchat-mcp-status ()
  "Display MCP status and available tools."
  (interactive)
  (let ((mcp-available (superchat-mcp-available-p))
        (servers-configured (superchat-mcp-get-server-count))
        (servers-running (superchat-mcp-get-running-server-count))
        (mcp-tools (superchat-mcp-get-tools)))
    (let ((content
           (concat
            (format "SuperChat MCP (Model Context Protocol) Status\n\n")
            (format "Available: %s\n" (if mcp-available "Yes" "No"))
            (format "Servers configured: %d\n" servers-configured)
            (format "Servers running: %d\n\n" servers-running)
            
            (when mcp-available
              (concat
               (if (zerop servers-configured)
                   "No MCP servers configured.\n\n"
                 (concat
                  "Configured servers:\n"
                  (mapconcat (lambda (server)
                               (format "  • %s" (car server)))
                             mcp-hub-servers "\n")
                  "\n"))
               
               (when (and (> servers-running 0) mcp-tools)
                 (concat
                  (format "MCP Tools available: %d\n" (length mcp-tools))
                  (mapconcat (lambda (tool)
                               (format "  • %s: %s" 
                                       (plist-get tool :name)
                                       (or (plist-get tool :description) "No description")))
                             mcp-tools "\n")
                  "\n\n"
                  "Usage: Tools are automatically integrated with gptel.\n"
                  "MCP tools appear with 'mcp-' prefix in gptel's tool system.\n")))))))
      
      ;; For interactive use, show help window
      (when (called-interactively-p 'interactive)
        (with-help-window "*SuperChat MCP Status*"
          (with-current-buffer standard-output
            (insert content))))
      ;; Return content for display in chat
      content)))


(provide 'superchat-mcp)
;;; superchat-mcp.el ends here
