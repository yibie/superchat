;;; test-superchat-mcp.el --- Test SuperChat MCP integration

;;; Commentary:
;; Test MCP integration functionality in SuperChat

;;; Code:

(require 'cl-lib)

;; Add mcp to load path
(add-to-list 'load-path "/Users/chenyibin/.emacs.d/straight/repos/mcp.el")

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

(defun test-superchat-mcp-basic ()
  "Test basic MCP functionality."
  (interactive)
  (message "=== Testing SuperChat MCP Basic Functions ===")
  
  ;; Test 1: Check MCP availability
  (let ((mcp-available (superchat-mcp-available-p)))
    (message "✅ MCP available: %s" (if mcp-available "Yes" "No"))
    
    (when mcp-available
      ;; Test 2: Check server counts
      (let ((configured (superchat-mcp-get-server-count))
            (running (superchat-mcp-get-running-server-count)))
        (message "✅ Servers configured: %d" configured)
        (message "✅ Servers running: %d" running))
      
      ;; Test 3: Get MCP tools
      (let ((mcp-tools (superchat-mcp-get-tools)))
        (message "✅ MCP tools count: %d" (if mcp-tools (length mcp-tools) 0))
        (when mcp-tools
          (message "First 3 MCP tools:")
          (dotimes (i (min 3 (length mcp-tools)))
            (let ((tool (nth i mcp-tools)))
              (message "  %d. %s: %s" (1+ i)
                       (plist-get tool :name)
                       (or (plist-get tool :description) "No description"))))))))
  
  (message "=== MCP Basic Test Complete ==="))

(defun test-superchat-mcp-commands ()
  "Test MCP command integration."
  (interactive)
  (message "=== Testing SuperChat MCP Commands ===")
  
  ;; Test 1: Check if MCP commands are in built-in commands
  (let ((builtin-commands superchat--builtin-commands))
    (message "✅ Built-in commands count: %d" (length builtin-commands))
    
    (let ((mcp-cmd (assoc "/mcp" builtin-commands))
          (mcp-start-cmd (assoc "/mcp-start" builtin-commands)))
      (if mcp-cmd
          (message "✅ /mcp command found: %s" (cdr mcp-cmd))
        (message "❌ /mcp command not found"))
      
      (if mcp-start-cmd
          (message "✅ /mcp-start command found: %s" (cdr mcp-start-cmd))
        (message "❌ /mcp-start command not found"))))
  
  ;; Test 2: Test command completion
  (let ((all-commands (superchat--get-all-command-names)))
    (message "✅ Total commands available: %d" (length all-commands))
    
    (let ((mcp-commands (cl-remove-if-not 
                         (lambda (cmd) (string-prefix-p "/mcp" cmd)) 
                         all-commands)))
      (if mcp-commands
          (progn
            (message "✅ MCP commands found: %d" (length mcp-commands))
            (dolist (cmd mcp-commands)
              (message "  • %s" cmd)))
        (message "❌ No MCP commands found in completion"))))
  
  (message "=== MCP Commands Test Complete ==="))

(defun test-superchat-mcp-integration ()
  "Test MCP integration with gptel tools."
  (interactive)
  (message "=== Testing SuperChat MCP Integration ===")
  
  ;; Test gptel tools integration
  (let ((gptel-tools (superchat-get-gptel-tools)))
    (message "✅ gptel tools count: %d" (length gptel-tools))
    
    ;; Check if MCP tools are included in gptel tools
    (let ((mcp-tools (cl-remove-if-not 
                      (lambda (tool) 
                        (and (plist-get tool :category)
                             (string-prefix-p "mcp-" (plist-get tool :category))))
                      gptel-tools)))
      (if mcp-tools
          (progn
            (message "✅ Found %d MCP tools integrated with gptel:" (length mcp-tools))
            (dolist (tool (cl-subseq mcp-tools 0 (min 3 (length mcp-tools))))
              (message "  • %s (category: %s)" 
                       (plist-get tool :name)
                       (plist-get tool :category))))
        (message "⚠️  No MCP tools currently integrated with gptel"))))
  
  ;; Test if MCP tools are automatically available
  (when (superchat-mcp-servers-running-p)
    (message "✅ MCP servers are running - tools should be available"))
  
  (message "=== MCP Integration Test Complete ==="))

(defun test-superchat-mcp-comprehensive ()
  "Run comprehensive MCP tests."
  (interactive)
  (message "=== Comprehensive SuperChat MCP Test ===")
  (test-superchat-mcp-basic)
  (test-superchat-mcp-commands)
  (test-superchat-mcp-integration)
  (message "=== Comprehensive MCP Testing Complete ==="))

;; Run the comprehensive test
(test-superchat-mcp-comprehensive)

(provide 'test-superchat-mcp)
;;; test-superchat-mcp.el ends here