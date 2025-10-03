;;; example-tools-usage.el --- Example of using SuperChat with gptel tools

;;; Commentary:
;; This example demonstrates how to use SuperChat with gptel tools enabled.

;;; Code:

(require 'superchat)

;; Example 1: Setting up gptel tools
(defun example-setup-gptel-tools ()
  "Example of setting up gptel tools for SuperChat."
  (interactive)
  
  ;; Configure gptel to use tools
  (setq gptel-use-tools t)
  
  ;; Define some example tools (you would replace these with actual tools)
  (setq gptel-tools
        (list
         ;; Example web search tool
         (gptel-make-tool
          :name "web_search"
          :function #'example-web-search
          :description "Search the web for information"
          :args '((:name "query" :type string :description "Search query"))
          :category "search")
         
         ;; Example file analysis tool  
         (gptel-make-tool
          :name "analyze_file"
          :function #'example-analyze-file
          :description "Analyze a file's content"
          :args '((:name "file_path" :type string :description "Path to file"))
          :category "analysis")))
  
  (message "✅ gptel tools configured with %d tools" (length gptel-tools)))

;; Example tool functions (simplified implementations)
(defun example-web-search (query &rest args)
  "Example web search tool function."
  (message "🔍 Searching web for: %s" query)
  (format "Search results for '%s': [simulated results]" query))

(defun example-analyze-file (file-path &rest args)
  "Example file analysis tool function."
  (message "📁 Analyzing file: %s" file-path)
  (if (file-exists-p file-path)
      (format "File analysis for '%s': %d bytes, readable" 
              file-path (file-attribute-size (file-attributes file-path)))
    (format "File '%s' does not exist" file-path)))

;; Example 2: Using SuperChat with tools
(defun example-start-superchat-with-tools ()
  "Start SuperChat with tools enabled."
  (interactive)
  
  ;; First set up tools
  (example-setup-gptel-tools)
  
  ;; Then start SuperChat
  (superchat)
  
  ;; Show instructions
  (message "🚀 SuperChat started with tools!")
  (message "💡 Try using the '/tools' command to see available tools")
  (message "💡 You can now ask questions that will trigger tool usage"))

;; Example 3: Check tools status
(defun example-check-tools-status ()
  "Example of checking tools status in SuperChat."
  (interactive)
  
  ;; Show gptel configuration
  (message "=== gptel Configuration ===")
  (message "gptel-use-tools: %s" (if (boundp 'gptel-use-tools) gptel-use-tools "Not set"))
  (message "gptel-tools count: %d" (if (boundp 'gptel-tools) (length gptel-tools) 0))
  
  ;; Show SuperChat integration status
  (message "\n=== SuperChat Integration ===")
  (message "Tools enabled in SuperChat: %s" (superchat-gptel-tools-enabled-p))
  (message "Tools available to SuperChat: %d" (length (superchat-get-gptel-tools)))
  
  ;; Show /tools command usage
  (message "\n=== Usage ===")
  (message "1. Start SuperChat: M-x superchat")
  (message "2. Check tools: /tools")
  (message "3. Ask questions that may trigger tools")
  (message "Example: 'Search the web for Emacs tips'"))

(provide 'example-tools-usage)
;;; example-tools-usage.el ends here