2;;; superchat-workflow.el --- Workflow functionality for superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; This file implements workflow functionality for superchat based on HAIP concepts,
;; adapted to use superchat's existing symbol semantics (@model, /command, #file).

;;; Code:

(require 'cl-lib)
(require 'superchat-parser)

;;;--------------------------------------------------
;;; Dependency Injection
;;;-----------------------------------------------

(defvar superchat-workflow--llm-executor nil
  "A function passed from superchat.el to execute LLM prompts.
This function should accept a prompt string and optionally a target-model parameter.")

(defvar superchat-workflow--current-lang "English"
  "Current language setting for workflow prompts.")

(defvar superchat-workflow--current-model nil
  "Current model for workflow execution.")

(cl-defun superchat-workflow-initialize (&key llm-executor lang)
  "Initialize the workflow system with dependencies from superchat.el.
LLM-EXECUTOR is the function to execute LLM prompts.
LANG is the current language setting (defaults to English)."
  (setq superchat-workflow--llm-executor llm-executor)
  (when lang
    (setq superchat-workflow--current-lang lang)))

;;;-----------------------------------------------
;;; Template Variable Support
;;;-----------------------------------------------

(defun superchat-workflow-replace-variables (prompt &optional input lang context)
  "Replace template variables in PROMPT.
Supports:
  $input - replaced with INPUT parameter (or user-input from CONTEXT)
  $lang  - replaced with LANG parameter or current workflow language

If CONTEXT is provided, can also extract user-input from the workflow context."
  (let* ((current-lang (or lang superchat-workflow--current-lang "English"))
         ;; Get input: priority is input parameter > context user-input > empty
         (actual-input (or input
                           (when (superchat-workflow-context-p context)
                             (superchat-workflow-context-user-input context))
                           ""))
         (result prompt))
    ;; Replace $input
    (setq result (replace-regexp-in-string "\\$input" actual-input result))
    ;; Replace $lang
    (setq result (replace-regexp-in-string "\\$lang" current-lang result))
    result))

;;;-----------------------------------------------
;;; Phase 1: Core Framework - Parser 
;;;-----------------------------------------------

(cl-defstruct superchat-workflow-step
  "Data structure representing a single workflow step."
  model
  command
  contexts
  prompt)

(cl-defstruct superchat-workflow
  "Data structure representing a parsed workflow.

It contains the original workflow content and a list of parsed steps."
  content
  steps
  metadata)

;;;-----------------------------------------------
;;; Workflow Parser Functions 
;;;-----------------------------------------------

(defun superchat-workflow-parse-workflow (content)
  "Parse workflow content, based on existing superchat symbol semantics.
CONTENT is the text content of the workflow file, return the parsed step list."
  (let ((steps '())
        (lines (split-string content "\n")))
    (dolist (line lines)
      (let ((step (superchat-workflow-parse-line line)))
        (when step
          (push step steps))))
    (nreverse steps)))

(defun superchat-workflow-parse-line (line)
  "Parse a single line, recognize @/#/ symbols.
LINE is a single line of text, return the step structure or nil."
  (when (not (string-empty-p (string-trim line)))
    (let* ((model-switch-info (superchat-parser-model-switch line))
           (model (when model-switch-info (cdr model-switch-info)))
           (command nil)
           (contexts (superchat-workflow-extract-contexts line))
           (inferred-command nil))
      
      ;; Extract command: must be at word boundary, not part of a file path
      ;; Match /command only if preceded by whitespace or start of line
      (when (string-match "\\(?:^\\|[[:space:]]\\)/\\([a-zA-Z0-9_-]+\\)" line)
        (let ((potential-command (match-string 1 line))
              (match-start (match-beginning 0)))
          ;; Verify it's not part of a file path by checking:
          ;; 1. No # before the match (within 5 chars)
          ;; 2. Not preceded by ~ or another /
          (when (not (or (and (>= match-start 1)
                              (string-match-p "#" (substring line (max 0 (- match-start 5)) match-start)))
                         (and (> match-start 0)
                              (memq (aref line (1- match-start)) '(?~ ?/)))))
            (setq command potential-command))))
      
      ;; Smart inference: if no explicit command but there's a file context
      ;; and the line mentions saving/writing, infer a save-file command
      (when (and (not command)
                 contexts
                 (or (string-match-p "保存\\|写入\\|存储\\|输出" line)
                     (string-match-p "save\\|write\\|store\\|output" line)))
        (setq inferred-command "save-file")
        (message "📝 Inferred command: save-file from line: %s" (substring line 0 (min 50 (length line)))))
      
      (when (or model command inferred-command contexts)
        (make-superchat-workflow-step
         :model model
         :command (or command inferred-command)
         :contexts contexts
         :prompt line)))))

(defun superchat-workflow-extract-contexts (line)
  "Extract all # context references from the line.
LINE is a single line of text, return the context path list.
Only extract references that look like file paths (contain path separators or file extensions)."
  (let ((contexts '())
        (start 0))
    ;; Improve the regular expression to ensure it matches the complete path
    (while (string-match "#\\s-*\\(?:\"\\([^\"]+\\)\"\\|\\([^[:space:]#]+\\(?:/[^[:space:]#]*\\)?\\(?:\\.[^[:space:]#]+\\)?\\)\\)" line start)
      (let* ((quoted-path (match-string 1 line))
             (unquoted-path (match-string 2 line))
             (candidate (or quoted-path unquoted-path)))
        ;; Only add to the context if the candidate path looks like a file
        (when (and candidate
                   (or (string-match-p "/" candidate)        ; Contains path separator
                       (string-match-p "\\." candidate)      ; Contains extension
                       (string-match-p "^~" candidate)       ; Starts with ~ (user directory)
                       (file-exists-p candidate)))          ; Or the file actually exists
          (push candidate contexts))
        (setq start (match-end 0))))
    (nreverse contexts)))

;;;-----------------------------------------------
;;; Phase 1: Core API Integration
;;;-----------------------------------------------

;;;-----------------------------------------------
;;; Phase 1: Context Management System 
;;;-----------------------------------------------

(cl-defstruct superchat-workflow-context
  "HAIP workflow context object"
  (steps '())                    ; Executed steps list
  (variables (make-hash-table :test 'equal))  ; Variables storage
  (results (make-hash-table :test 'equal))   ; Each step result storage
  (metadata '())                 ; Metadata
  (current-step 0)               ; Current step index
  (state 'running)               ; 执行状态
  (user-input ""))               ; User input passed to workflow (for $input variable)

(defun superchat-workflow-create-context (workflow-id &optional user-input)
  "Create a new workflow context.
USER-INPUT is optional input provided by the user when invoking the workflow."
  (make-superchat-workflow-context
   :metadata `((workflow-id . ,workflow-id)
               (start-time . ,(current-time)))
   :user-input (or user-input "")))

(defun superchat-workflow-update-context (context _step result)
  "Update the context object, add the step result."
  (let ((step-num (superchat-workflow-context-current-step context))
        (clean-content (superchat-workflow-extract-result-content result)))
    
    ;; 🐛 DEBUG: 显示原始结果和提取的内容
    (message "🐛 DEBUG: Step %d Raw Result Type: %s" step-num (type-of result))
    (message "🐛 DEBUG: Step %d Raw Result (first 200 chars): %s" 
             step-num 
             (let ((raw-str (prin1-to-string result)))
               (if (> (length raw-str) 200)
                   (concat (substring raw-str 0 200) "...")
                 raw-str)))
    (message "🐛 DEBUG: Step %d Extracted Content: %s" step-num 
             (if (> (length clean-content) 200)
                 (concat (substring clean-content 0 200) "...")
               clean-content))
    
    ;; Store ONLY clean string content - eliminate complexity at the source
    (puthash step-num clean-content (superchat-workflow-context-results context))

    ;; 🐛 DEBUG: 显示context中存储的内容
    (message "🐛 DEBUG: Step %d Stored in Context: %s" step-num 
             (let ((stored (gethash step-num (superchat-workflow-context-results context))))
               (if (> (length stored) 200)
                   (concat (substring stored 0 200) "...")
                 stored)))

    ;; Update the current step
    (cl-incf (superchat-workflow-context-current-step context))

    ;; Auto-extract variables from clean content
    (when (stringp clean-content)
      (superchat-workflow-auto-extract-variables context clean-content))))

(defun superchat-workflow-set-variable (context name value)
  "Set a variable in the context."
  (puthash name value (superchat-workflow-context-variables context)))

(defun superchat-workflow-get-variable (context name)
  "Get a variable from the context."
  (gethash name (superchat-workflow-context-variables context)))

(defun superchat-workflow-auto-extract-variables (context step-result)
  "Automatically extract variables from the step result."
  (when (stringp step-result)
    ;; Simple variable extraction logic, can be extended to use LLM later
    (let ((variables (superchat-workflow-simple-extract-variables step-result)))
      (dolist (var variables)
        (let ((name (car var))
              (value (cdr var)))
          (superchat-workflow-set-variable context name value))))))

(defun superchat-workflow-simple-extract-variables (text)
  "Simple variable extraction, based on common patterns."
  (let ((variables '()))
    ;; Extract file path
    (when (string-match "文件路径[：:]*\\s-*\\([^[:space:]\n]+\\)" text)
      (push (cons "file_path" (match-string 1 text)) variables))
    ;; Extract project name
    (when (string-match "项目[名称]*[：:]*\\s-*\\([^[:space:]\n]+\\)" text)
      (push (cons "project_name" (match-string 1 text)) variables))
    ;; Extract numeric information
    (when (string-match "\\([0-9]+\\)\\s-*个" text)
      (push (cons "count" (match-string 1 text)) variables))
    variables))

(defun superchat-workflow-format-variables (context)
  "Format variables from CONTEXT into readable text.
Returns formatted string of variables or empty string if no variables."
  (let* ((variables-table (superchat-workflow-context-variables context))
         (variable-list '()))
    ;; Convert hash table to list of key-value pairs
    (maphash (lambda (key value)
               (push (cons key value) variable-list))
             variables-table)
    
    (if variable-list
        (let ((formatted-vars
               (mapcar (lambda (var)
                         (format "  %s: %s" (car var) (cdr var)))
                       (sort variable-list (lambda (a b) (string< (car a) (car b)))))))
          (format "变量信息:\n%s" (string-join formatted-vars "\n")))
      "")))

;;;-----------------------------------------------
;;; Context Injection - HAIP Core Mechanism
;;;-----------------------------------------------

(defun superchat-workflow-inject-context (prompt context &optional max-context-length)
  "Intelligently inject context information into PROMPT.
This is the core HAIP mechanism for context management.

PROMPT: The original step prompt
CONTEXT: The workflow context object  
MAX-CONTEXT-LENGTH: Maximum length of context to inject (default: 2000)

Returns enhanced prompt with context information injected."
  (if (not (superchat-workflow-context-p context))
      ;; If no context, return original prompt
      (progn
        (message "🐛 DEBUG: No valid context object, returning original prompt")
        prompt)
    (let* ((max-len (or max-context-length 2000))
           ;; Get smart context summary with length limit
           (context-summary (superchat-workflow-build-smart-context context :max-length max-len))
           ;; Only include context if it's not empty and context has meaningful content
           (current-step (superchat-workflow-context-current-step context))
           (has-context-p (and context-summary 
                               (not (string-empty-p (string-trim context-summary)))
                               (> current-step 0))))
      
      ;; 🐛 DEBUG: 显示上下文注入的详细信息
      (message "🐛 DEBUG: Context injection - Current step: %d" current-step)
      (message "🐛 DEBUG: Context summary length: %d" (if context-summary (length context-summary) 0))
      (message "🐛 DEBUG: Has meaningful context: %s" has-context-p)
      (message "🐛 DEBUG: Context summary: %s" 
               (if context-summary
                   (if (> (length context-summary) 300)
                       (concat (substring context-summary 0 300) "...")
                     context-summary)
                 "NIL"))
      
      (if has-context-p
          ;; Enhanced prompt with context
          (let ((enhanced-prompt (format "%s\n\n--- 工作流上下文 ---\n%s\n\n请基于以上上下文执行当前任务。"
                                         prompt
                                         context-summary)))
            (message "🐛 DEBUG: Injecting context into prompt (total length: %d)" (length enhanced-prompt))
            enhanced-prompt)
        ;; No meaningful context, return original prompt
        (progn
          (message "🐛 DEBUG: No meaningful context, returning original prompt")
          prompt)))))

(defun superchat-workflow-summarize-context (context)
  "Generate a concise summary of the workflow context.
This function provides a brief overview of the workflow state."
  (let* ((current-step (superchat-workflow-context-current-step context))
         (total-steps (length (superchat-workflow-context-steps context)))
         (recent-results (superchat-workflow-get-recent-results context 2))
         (variables-text (superchat-workflow-format-variables context)))
    
    (string-join
     (delq nil
           (list (format "工作流进度: %d/%d 步" current-step total-steps)
                 (when (not (string-empty-p recent-results))
                   (format "最近结果:\n%s" recent-results))
                 (when (not (string-empty-p variables-text))
                   variables-text)))
     "\n\n")))

;;;-----------------------------------------------
;;; Model Switching Utility 
;;;-----------------------------------------------

(defun superchat-workflow-switch-model (model-name)
  "Switch to the specified model for workflow execution."
  (interactive "sModel name: ")
  (message "🔍 Attempting to switch to model: %s" model-name)
  (cond
   ;; Check if gptel is available and model variable exists
   ((and (boundp 'gptel-model) (stringp model-name))
    ;; For both gptel and ollama models, just set gptel-model directly
    ;; gptel will handle the backend switching automatically
    (let ((old-model gptel-model))
      (setq gptel-model model-name)
      (setq superchat-workflow--current-model model-name)
      (message "🔄 Switched from %s to model: %s" old-model model-name)
      (message "🔍 Current gptel-model is now: %s" gptel-model)
      (message "🔍 Workflow current model is now: %s" superchat-workflow--current-model)
      t)) ; Return t to indicate success

   ;; gptel not available, but we can still track the model for workflow
   ((stringp model-name)
    (setq superchat-workflow--current-model model-name)
    (message "🔄 Set workflow model to: %s (gptel not available)" model-name)
    t) ; Return t to indicate success

   ;; Invalid model name
   (t
    (message "⚠️ Invalid model name: %s" model-name)
    nil))) ; Return nil to indicate failure

(defun superchat-workflow-call-llm (prompt &optional target-model context)
 "Call the LLM executor with the given prompt, model, and context.
TARGET-MODEL overrides the current workflow model if provided.
CONTEXT is the workflow context - if provided, context will be intelligently injected."
 (when superchat-workflow--llm-executor
   (let* ((model-to-use (or target-model superchat-workflow--current-model))
          ;; 🔑 Core fix: Inject context into prompt if context is provided
          (enhanced-prompt (if context
                               (superchat-workflow-inject-context prompt context)
                             prompt))
          (gptel-tools (when (fboundp 'superchat-get-gptel-tools)
                         (superchat-get-gptel-tools)))
          (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                       (superchat-mcp-get-tools)))
          (all-tools (append gptel-tools mcp-tools))
          ;; Ensure gptel-use-tools is t if there are any tools
          (gptel-use-tools (and all-tools t)))
     
     ;; 🐛 DEBUG: LLM调用详情
     (message "🐛 DEBUG: LLM Call - Model: %s, Tools available: %d" 
              model-to-use (length all-tools))
     (message "🐛 DEBUG: LLM Call - gptel-use-tools: %s" gptel-use-tools)
     (message "🐛 DEBUG: LLM Call - Prompt length: %d" (length enhanced-prompt))
     (message "🐛 DEBUG: LLM Call - Prompt preview: %s" 
              (if (> (length enhanced-prompt) 200)
                  (concat (substring enhanced-prompt 0 200) "...")
                enhanced-prompt))
     
     ;; Debug logging for context injection
     (when (and context (superchat-workflow-context-p context))
       (let ((current-step (superchat-workflow-context-current-step context)))
         (message "🧠 Context injected for step %d (context length: %d chars)" 
                  current-step (length enhanced-prompt))))
     
     (let ((llm-result 
            (if model-to-use
                ;; Call with model parameter if the executor supports it
                (condition-case nil
                    (funcall superchat-workflow--llm-executor enhanced-prompt model-to-use)
                  ;; Fallback if executor doesn't support model parameter
                  (wrong-number-of-arguments
                   (funcall superchat-workflow--llm-executor enhanced-prompt)))
              ;; No model specified, call without model parameter
              (funcall superchat-workflow--llm-executor enhanced-prompt))))
       
       ;; 🐛 DEBUG: LLM返回结果
       (message "🐛 DEBUG: LLM Result - Type: %s" (type-of llm-result))
       (message "🐛 DEBUG: LLM Result - Content: %s" 
                (let ((result-str (prin1-to-string llm-result)))
                  (if (> (length result-str) 300)
                      (concat (substring result-str 0 300) "...")
                    result-str)))
       
       llm-result))))

;;;-----------------------------------------------
;;; Universal Workflow Executors (已删除 - 无实际使用价值)
;;;-----------------------------------------------

;;;-----------------------------------------------
;;; 文件保存辅助函数
;;;-----------------------------------------------

(cl-defun superchat-workflow--save-to-file (file-path content &key create-dirs-p)
  "保存内容到文件，提供统一的错误处理。

FILE-PATH: 保存的文件路径
CONTENT: 要保存的内容
CREATE-DIRS-P: 是否自动创建目录（默认为t）

返回保存结果消息或nil（失败时）"
  (condition-case err
      (when (and file-path content)
        ;; 确保目录存在
        (when create-dirs-p
          (let ((dir (file-name-directory file-path)))
            (when (and dir (not (file-directory-p dir)))
              (make-directory dir t))))

        ;; 保存文件
        (with-temp-file file-path
          (insert content))
        (format "内容已保存到文件：%s" file-path))
      (error
       (message "文件保存失败：%s" (error-message-string err))
       nil)))

(defun superchat-workflow--tool-confirmation-p (text)
  "Return non-nil when TEXT looks like a tool确认/状态提示，而不是正文内容."
  (when (stringp text)
    (let ((normalized (string-trim text)))
      (and (not (string-empty-p normalized))
           (string-match-p
            (rx string-start
                (or "✅"             ; tool 成功提示
                    "⚡"             ; quick replace / diff 提示
                    "❌"             ; tool 失败提示
                    "Error:"        ; 通用错误
                    "Warning:"
                    "Web search failed"
                    "Brave Search API error"
                    "内容已保存到文件"
                    "Content appended to"
                    "Quick replace successful"))
            normalized)))))

(defun superchat-workflow--find-last-meaningful-result (context &optional before-index)
  "在 CONTEXT 中由近及远寻找最近的有效正文结果。
可选参数 BEFORE-INDEX 指定从哪个步骤索引开始（包含）向前查找。"
  (let* ((results-table (superchat-workflow-context-results context))
         (idx (if before-index
                  before-index
                (1- (superchat-workflow-context-current-step context))))
         (found nil))
    (while (and (>= idx 0) (not found))
      (let ((candidate (gethash idx results-table)))
        (if (and (stringp candidate)
                 (not (string-empty-p (string-trim candidate)))
                 (not (superchat-workflow--tool-confirmation-p candidate)))
            (setq found candidate)
          (setq idx (1- idx)))))
    found))

(cl-defun superchat-workflow--generate-file-content (prompt context &key fallback-to-llm-p)
  "生成要保存到文件的内容。

PROMPT: 原始提示文本
CONTEXT: 工作流上下文
FALLBACK-TO-LLM-P: 当没有前一步结果时是否使用LLM生成（默认为t）

返回生成的内容字符串或nil"
  (let ((meaningful (superchat-workflow--find-last-meaningful-result context)))
    (or meaningful
        (when (and fallback-to-llm-p superchat-workflow--llm-executor)
          (message "⚠️ Workflow: 未找到可保存的正文，改为让模型重新生成内容。")
          (let ((save-prompt (format "请生成要保存到文件的摘要内容。根据以下指令：\n%s\n\n请生成简洁的摘要内容，不要包含其他说明。" prompt)))
            (superchat-workflow-call-llm save-prompt nil context))))))

;; 通用执行器注册（已删除 - 无实际使用价值）

;;;-----------------------------------------------
;;; Phase 2: Error Handling 
;;;-----------------------------------------------

(defun superchat-workflow-execute-step (step context)
  "Execute a single step, stop immediately on failure."
  (condition-case err
      (let ((model (superchat-workflow-step-model step))
            (command (superchat-workflow-step-command step))
            (contexts (superchat-workflow-step-contexts step))
            (prompt (superchat-workflow-step-prompt step)))

        ;; Validate the step
        (unless (or model command contexts)
          (error "步骤缺少可执行内容 (@, /, #)"))

        ;; Execute the step logic
        (superchat-workflow-execute-step-internal model command contexts prompt context))
    (error
     (message "⚠️ Workflow execution failed: %s" (error-message-string err))
     (setf (superchat-workflow-context-state context) 'failed)
     (signal 'superchat-workflow-execution-error err))))

(cl-defun superchat-workflow--execute-single-step (model command contexts prompt context &key collect-results-p error-handler)
  "执行单一步骤的核心逻辑，支持不同的行为模式。

MODEL: 模型名称
COMMAND: 命令名称
CONTEXTS: 文件上下文列表
PROMPT: 提示文本
CONTEXT: 工作流上下文
COLLECT-RESULTS-P: 是否收集执行结果
ERROR-HANDLER: 错误处理函数

返回执行结果字符串或nil。"
  (let ((results '())
        (step-result nil))

    ;; 1. Switch model (if supported by core system)
    (when model
      (condition-case err
          (progn
            (superchat-workflow-switch-model model)
            (let ((msg (format "Successfully switched to model %s" model)))
              (message "✅ %s" msg)
              (when collect-results-p
                (push msg results))))
        (error
         (when error-handler
           (funcall error-handler err "model-switch" model))
         nil)))

    ;; 2. Execute command using core superchat command system
    (when command
      (condition-case err
          (progn
            (let ((result (superchat-workflow-execute-command-via-core command prompt context contexts)))
              (message "✅ Command '%s' executed" command)
              (when collect-results-p
                (push result results))
              (setq step-result result)))
        (error
         (when error-handler
           (funcall error-handler err "command-execution" command))
         nil)))

    ;; 3. Process file contexts via core system
    (when contexts
      (dolist (ctx contexts)
        (condition-case err
            (let ((normalized-path (superchat-workflow-normalize-file-path ctx)))
              (when normalized-path
                (superchat-workflow-add-file-context-to-core normalized-path)
                (message "📁 Process file: %s" normalized-path)
                (when collect-results-p
                  (push (format "Processed file: %s" normalized-path) results))))
          (error
           (when error-handler
             (funcall error-handler err "file-context" ctx))
           nil))))

    ;; Return results based on collection mode
    (if collect-results-p
        (string-join (nreverse results) "\n\n")
      step-result)))

(defun superchat-workflow-execute-step-internal (model command contexts prompt context)
  "Execute the internal logic of the step using core superchat API."
  (superchat-workflow--execute-single-step model command contexts prompt context
                                           :collect-results-p t))

;;;-----------------------------------------------
;;; Core API Integration Functions
;;;-----------------------------------------------

(defun superchat-workflow-execute-command-via-core (command prompt context contexts)
  "智能执行策略：区分 workflow 特殊需求和普通命令"
  (cond
   ;; 1. 处理 save-file 推断命令
   ((string= command "save-file")
    (when (and contexts superchat-workflow--llm-executor)
      (let* ((file-path (superchat-workflow-normalize-file-path (car contexts)))
             (content (superchat-workflow--generate-file-content prompt context :fallback-to-llm-p t)))
        (or (superchat-workflow--save-to-file file-path content :create-dirs-p t)
            "文件保存失败"))))

   ;; 2. 处理 workflow 特殊命令映射
   ((string= command "search-news")
    ;; search-news 映射到 web-search 工具调用
    (when superchat-workflow--llm-executor
      (let ((search-prompt (format "请使用 web-search 工具搜索关于 AI 和技术新闻的最新信息。%s" prompt)))
        (superchat-workflow-call-llm search-prompt nil context))))
   
   ((string= command "analyze-news") 
    ;; analyze-news 使用前面步骤的结果进行分析
    (when superchat-workflow--llm-executor
      (let* ((recent-results (superchat-workflow-get-recent-results context 2))
             (analysis-prompt (format "请分析以下搜索到的新闻信息，生成一份简洁的中文摘要：\n\n%s\n\n%s" 
                                      recent-results prompt)))
        (superchat-workflow-call-llm analysis-prompt nil context))))

   ;; 3. 普通命令直接调用核心系统
   ((fboundp 'superchat--handle-command)
    (let* ((args (or prompt ""))
           (result (superchat--handle-command command args prompt)))
      (pcase (plist-get result :type)
        (:buffer (plist-get result :content))
        (:echo (plist-get result :content))
        (:llm-query
         (when superchat-workflow--llm-executor
           (superchat-workflow-call-llm (plist-get result :prompt) nil context)))
        (_
         ;; 如果命令执行成功但没有返回内容，尝试获取上下文信息
         (or (plist-get result :content)
             (when (and superchat-workflow--llm-executor prompt)
               (superchat-workflow-call-llm prompt nil context))
             "Command executed")))))

   ;; 4. 兜底：LLM 执行
   (t
    (when superchat-workflow--llm-executor
      (let ((enhanced-prompt (format "Execute command '/%s': %s" command prompt)))
        (superchat-workflow-call-llm enhanced-prompt nil context))))))

(defun superchat-workflow-normalize-file-path (file-path)
  "Normalize file path for workflow processing."
  (when (and file-path (not (string-empty-p file-path)))
    (let ((normalized-path (expand-file-name file-path)))
      ;; If relative path, resolve relative to workflow directory
      (when (and (not (file-name-absolute-p file-path))
                 (boundp 'superchat-data-directory))
        (let ((workflow-dir (superchat-workflow--workflow-dir)))
          (setq normalized-path (expand-file-name file-path workflow-dir))))
      normalized-path)))

(defun superchat-workflow-add-file-context-to-core (file-path)
  "Add file to core superchat context system."
  (when (and (file-exists-p file-path)
             (fboundp 'superchat--add-file-to-context))
    (superchat--add-file-to-context file-path)))

(defun superchat-workflow-execute-user-command (command prompt context contexts)
  "Legacy fallback - redirects to core API."
  (superchat-workflow-execute-command-via-core command prompt context contexts))

(defun superchat-workflow-get-recent-results (context n)
  "Get the most recent N step results from CONTEXT.
Returns a concatenated string of the results."
  (let* ((results-table (superchat-workflow-context-results context))
         (current-step (superchat-workflow-context-current-step context))
         (results '()))
    
    ;; 🐛 DEBUG: 显示当前步骤和context状态
    (message "🐛 DEBUG: Getting recent results - Current step: %d, Requesting: %d results" current-step n)
    
    (dotimes (i (min n current-step))
      (let* ((step-idx (- current-step i 1))
             (result (gethash step-idx results-table)))
        
        ;; 🐛 DEBUG: 显示每个步骤的结果
        (message "🐛 DEBUG: Step %d result: %s" step-idx 
                 (if result
                     (let ((content (superchat-workflow-extract-result-content result)))
                       (if (> (length content) 100)
                           (concat (substring content 0 100) "...")
                         content))
                   "NIL"))
        
        (when (and result (not (string-empty-p (string-trim (superchat-workflow-extract-result-content result)))))
          (push (string-trim (superchat-workflow-extract-result-content result)) results))))
    
    (let ((final-results (if results
                             (string-join (nreverse results) "\n\n---\n\n")
                           "")))
      ;; 🐛 DEBUG: 显示最终组合的结果
      (message "🐛 DEBUG: Final combined recent results (%d chars): %s" 
               (length final-results)
               (if (> (length final-results) 200)
                   (concat (substring final-results 0 200) "...")
                 final-results))
      final-results)))

(defun superchat-workflow--sanitize-string (string)
  "Normalize STRING so it survives JSON serialization.
Converts unibyte payloads to UTF-8 and strips control characters
that Emacs's `json-encode` rejects."
  (when (stringp string)
    (let* ((decoded (if (multibyte-string-p string)
                        string
                      (condition-case err
                          (decode-coding-string string 'utf-8 t)
                        (error
                         (message "🐛 DEBUG: Failed to decode string as UTF-8: %s"
                                  (error-message-string err))
                         string))))
           (control-chars-regexp "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]")
           (count 0)
           (clean (replace-regexp-in-string
                   control-chars-regexp
                   (lambda (_)
                     (cl-incf count)
                     " ")
                   decoded)))
      (when (> count 0)
        (message "🐛 DEBUG: Sanitized %d control chars from tool output" count))
      clean)))

(defun superchat-workflow-extract-result-content (result)
  "Extract string content from any result type.
Handles strings, lists, and tool-result structures from gptel."
  (let ((extracted-content
         (cond
          ;; If it's already a string, return as-is
          ((stringp result) 
           (message "🐛 DEBUG: Result is already a string")
           result)
          
          ;; Handle gptel tool-result structures
          ((and (listp result) 
                (eq (car result) 'tool-result)
                (> (length result) 1))
           (message "🐛 DEBUG: Processing tool-result structure")
           ;; Extract the actual content from tool-result
           (let ((tool-obj (cadr result)))
             (message "🐛 DEBUG: Tool object type: %s" (type-of tool-obj))
             (cond 
              ;; If it's a gptel-tool structure (vector), try to extract meaningful content
              ((and (vectorp tool-obj) (> (length tool-obj) 0))
               (message "🐛 DEBUG: Tool object is a vector with %d elements" (length tool-obj))
               (let ((content (aref tool-obj 0)))
                 (message "🐛 DEBUG: First element type: %s, content: %s" 
                          (type-of content) 
                          (if (stringp content) content "Not a string"))
                 (if (stringp content) content (prin1-to-string content))))
              ;; If it's a cons/list (actual tool result), extract the result
              ((listp tool-obj)
               (message "🐛 DEBUG: Tool object is a list with %d elements" (length tool-obj))
               ;; Check if this is the actual tool execution result pattern
               (cond
                ;; Look for string content in the tool result
                ((and (> (length tool-obj) 0)
                      (vectorp (car tool-obj))
                      (> (length (car tool-obj)) 0))
                 (message "🐛 DEBUG: Found vector in first position, extracting...")
                 (let ((result-content (aref (car tool-obj) 0)))
                   (if (functionp result-content)
                       (progn
                         (message "🐛 DEBUG: First element is function, this is tool definition, not result")
                         "ERROR: Got tool definition instead of execution result")
                     result-content)))
                ;; Direct string search in the list
                ((let ((string-content (seq-find #'stringp tool-obj)))
                   (when string-content
                     (message "🐛 DEBUG: Found string content in tool result")
                     string-content)))
                ;; Last resort for tool results
                (t 
                 (message "🐛 DEBUG: Tool result list doesn't contain expected string content")
                 (prin1-to-string tool-obj))))
              ;; Otherwise convert to string
              (t 
               (message "🐛 DEBUG: Tool object is not a vector or list, converting to string")
               (prin1-to-string tool-obj)))))
          
          ;; Handle lists that might contain content
          ((listp result)
           (message "🐛 DEBUG: Processing list result with %d elements" (length result))
           (let ((meaningful-content 
                  (seq-find #'stringp result)))  ; Find first string in the list
             (if meaningful-content
                 (progn
                   (message "🐛 DEBUG: Found string content in list")
                   meaningful-content)
               ;; If no string found, look for nested content
               (let ((nested (seq-find #'listp result)))
                 (if nested
                     (progn
                       (message "🐛 DEBUG: Found nested list, recursing")
                       (superchat-workflow-extract-result-content nested))
                   ;; Last resort: serialize the whole thing but limit length
                   (progn
                     (message "🐛 DEBUG: No string or nested content, serializing list")
                     (let ((serialized (prin1-to-string result)))
                       (if (> (length serialized) 500)
                           (concat (substring serialized 0 500) "...")
                         serialized))))))))
          
          ;; For other non-string objects, convert but with length limit
          (t 
           (message "🐛 DEBUG: Converting non-string object of type: %s" (type-of result))
           (let ((serialized (prin1-to-string result)))
             (if (> (length serialized) 500)
                 (concat (substring serialized 0 500) "...")
               serialized))))))

    (let ((sanitized (superchat-workflow--sanitize-string extracted-content)))
      (message "🐛 DEBUG: Final extracted content (%d chars): %s" 
               (length sanitized)
               (if (> (length sanitized) 150)
                   (concat (substring sanitized 0 150) "...")
                 sanitized))
      sanitized)))

(defun superchat-workflow-get-last-step-result (context)
  "Get the most recent step result from CONTEXT.
Returns the result string or nil if no results."
  (let* ((results-table (superchat-workflow-context-results context))
         (current-step (superchat-workflow-context-current-step context)))
    (when (> current-step 0)
      (gethash (1- current-step) results-table))))

;;;-----------------------------------------------
;;; Phase 2: Stream Execution System 
;;;-----------------------------------------------

(defun superchat-workflow-execute-workflow-stream (workflow-id workflow-content &optional user-input)
  "Stream execution of the workflow, provide real-time progress feedback.
USER-INPUT is optional input provided when invoking the workflow (for $input variable)."
  (interactive "sWorkflow ID: \nsWorkflow content: ")
  (let ((context (superchat-workflow-create-context workflow-id user-input))
        (steps (superchat-workflow-parse-workflow workflow-content))
        (execution-results '()))  ; New: collect all execution results

    ;; Store steps in context for executor access
    (setf (superchat-workflow-context-steps context) steps)

    (message "🚀 Start executing workflow: %s (%d steps)" workflow-id (length steps))

    (catch 'superchat-workflow-execution-error
      (dotimes (i (length steps))
        (let* ((step (nth i steps))
               (step-number (1+ i))
               (command (superchat-workflow-step-command step))
               (model (superchat-workflow-step-model step))
               (contexts (superchat-workflow-step-contexts step))
               (prompt (superchat-workflow-step-prompt step)))

          ;; Display execution progress
          (message "📋 Step %d/%d: %s"
                   step-number
                   (length steps)
                   (superchat-workflow--format-step-info step))

          ;; Execute step with proper error handling and result collection
          (let ((result nil)
                ;; Replace variables in prompt before execution
                (processed-prompt (when prompt
                                    (superchat-workflow-replace-variables 
                                     prompt 
                                     (superchat-workflow-context-user-input context)
                                     superchat-workflow--current-lang
                                     context))))
            (condition-case err
                (progn
                  ;; 使用公共函数执行步骤，带错误处理
                  (let ((step-error-handler
                         (lambda (error step-type info)
                           (message "⚠️ Step %d %s failed: %s"
                                    step-number step-type (error-message-string error)))))
                    (superchat-workflow--execute-single-step
                     model command contexts processed-prompt context
                     :collect-results-p nil
                     :error-handler step-error-handler)

                    ;; If no result from command, try to get LLM response for the step
                    (unless result
                      (when (and superchat-workflow--llm-executor processed-prompt)
                        (setq result (superchat-workflow-call-llm processed-prompt nil context)))))

                  ;; Collect execution results
                  (when result
                    (push (list step-number step result) execution-results)
                    (superchat-workflow-update-context context step result)))

              (error
               (message "⚠️ Step %d failed: %s" step-number (error-message-string err))
               (setf (superchat-workflow-context-state context) 'failed)
               (throw 'superchat-workflow-execution-error err))))

          ;; Display progress
          (superchat-workflow-update-progress step-number (length steps)))))

    ;; Check the final status and display the result summary
    (if (eq (superchat-workflow-context-state context) 'failed)
        (message "❌ Workflow '%s' execution failed" workflow-id)
      (progn
        (message "✅ Workflow '%s' execution completed" workflow-id)
        ;; Display the result summary
        (superchat-workflow-display-results-summary workflow-id execution-results)))

    context))

(defun superchat-workflow-update-progress (current total)
  "Update the execution progress."
  (let ((percentage (/ (* current 100) total)))
    (message "📊 Execution progress: %d%% (%d/%d)" percentage current total)))

(defun superchat-workflow-display-results-summary (workflow-id execution-results)
  "Display the workflow execution result summary."
  (when execution-results
    (let ((sorted-results (sort execution-results (lambda (a b) (< (car a) (car b))))))
      (message "")
      (message "📋 Workflow execution result summary (%s):" workflow-id)
      (message "═══════════════════════════════════════")

      (dolist (result sorted-results)
        (let* ((step-number (car result))
               (step (cadr result))
               (step-result (caddr result))
               (step-info (superchat-workflow--format-step-info step)))
          (message "")
          (message "🔸 Step %d: %s" step-number step-info)

          ;; Display the execution result (limit the length to avoid long output)
          (when step-result
            (let* ((clean-content (superchat-workflow-extract-result-content step-result))
                    (trimmed (string-trim clean-content)))
              (when (not (string-empty-p trimmed))
                (let ((truncated-result
                       (if (> (length trimmed) 200)
                           (concat (substring trimmed 0 200) "...")
                         trimmed)))
                  (message "   Result: %s" truncated-result))))))

      (message "")
      (message "🎉 Workflow execution completed! Total %d steps" (length sorted-results))
      (message "═══════════════════════════════════════")
      (message "")))))

;;;-----------------------------------------------
;;; Phase 3: Integration with SuperChat 
;;;-----------------------------------------------

;; Workflow file directory management
(defun superchat-workflow--workflow-dir ()
  "Return the workflow file directory."
  (let ((data-dir (if (boundp 'superchat-data-directory)
                       superchat-data-directory
                     (expand-file-name "superchat/" user-emacs-directory))))
    (expand-file-name "workflow/" data-dir)))

(defun superchat-workflow--ensure-workflow-dir ()
  "Ensure the workflow directory exists."
  (let ((workflow-dir (superchat-workflow--workflow-dir)))
    (unless (file-directory-p workflow-dir)
      (make-directory workflow-dir t))
    workflow-dir))

(defun superchat-workflow-get-available-workflows ()
  "Get all available workflow lists."
  (let ((workflow-dir (superchat-workflow--workflow-dir)))
    (when (file-directory-p workflow-dir)
      (mapcar #'file-name-base
              (directory-files workflow-dir nil "^.*\\.workflow$")))))

(defun superchat-workflow-workflow-exists-p (workflow-name)
  "Check if the workflow exists."
  (let ((workflow-file (expand-file-name (concat workflow-name ".workflow")
                                         (superchat-workflow--workflow-dir))))
    (file-exists-p workflow-file)))

(defun superchat-workflow-load-workflow (workflow-name)
  "Load the workflow file content."
  (let ((workflow-file (expand-file-name (concat workflow-name ".workflow")
                                         (superchat-workflow--workflow-dir))))
    (when (file-exists-p workflow-file)
      (with-temp-buffer
        (insert-file-contents workflow-file)
        (buffer-string)))))

;;;-----------------------------------------------
;;; Phase 3: Workflow Management Commands 
;;;-----------------------------------------------

(defun superchat-workflow-list-workflows ()
  "List all available workflows."
  (interactive)
  (let ((workflow-files (superchat-workflow-get-available-workflows)))
    (if workflow-files
        (message "Available workflows:\n%s"
                 (mapconcat (lambda (file) (format "  • %s" file))
                            workflow-files "\n"))
      (message "No workflow files found. Workflow directory: %s" (superchat-workflow--workflow-dir)))))

(defun superchat-workflow-execute-workflow (workflow-name &optional workflow-args)
  "Execute the specified workflow.
WORKFLOW-ARGS is optional user input that will be available as $input variable."
  (interactive "sWorkflow name: ")
  (if (superchat-workflow-workflow-exists-p workflow-name)
      (let* ((workflow-content (superchat-workflow-load-workflow workflow-name))
             (workflow-id (format "%s-%d" workflow-name (random 10000))))
        (when workflow-content
          (message "🚀 Start executing workflow: %s%s" 
                   workflow-name
                   (if (and workflow-args (not (string-empty-p workflow-args)))
                       (format " (input: %s)" workflow-args)
                     ""))
          (superchat-workflow-execute-workflow-stream workflow-id workflow-content workflow-args)))
    (message "❌ Workflow '%s' does not exist" workflow-name)))

(defun superchat-workflow-parse-workflow-input (input)
  "Parse the workflow input.
Syntax: >workflow-name optional-user-input
Returns: (workflow-name . user-input) cons or nil."
  (when (string-prefix-p ">" input)
    (let ((clean-input (string-trim (substring input 1))))
      (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?$" clean-input)
          (cons (match-string 1 clean-input)
                (string-trim (or (match-string 2 clean-input) "")))
        (cons clean-input "")))))

;;;-----------------------------------------------
;;; Phase 3: Command Integration Functions 
;;;-----------------------------------------------

(defun superchat-workflow-handle-workflow-command (workflow-name workflow-args _input)
  "Handle the workflow command.
WORKFLOW-ARGS is user input that will be available as $input in the workflow."
  (if (superchat-workflow-workflow-exists-p workflow-name)
      (progn
        (message "🔄 Execute workflow: %s%s"
                 workflow-name
                 (if (and workflow-args (not (string-empty-p workflow-args)))
                     (format " (input: %s)" workflow-args)
                   ""))
        (let* ((workflow-content (superchat-workflow-load-workflow workflow-name))
               (workflow-id (format "%s-%d" workflow-name (random 10000))))
          (superchat-workflow-execute-workflow-stream workflow-id workflow-content workflow-args)
          `(:type :echo :content ,(format "Workflow '%s' executed" workflow-name))))
    `(:type :echo :content ,(format "Workflow '%s' does not exist" workflow-name))))

(defun superchat-workflow-completion-workflows ()
  "Provide workflow name completion."
  (let ((available-workflows (superchat-workflow-get-available-workflows)))
    (mapcar (lambda (workflow) (concat ">" workflow)) available-workflows)))

;; Ensure the workflow directory exists
(superchat-workflow--ensure-workflow-dir)

;;;-----------------------------------------------
;;; Context Intelligence - HAIP核心机制
;;;-----------------------------------------------

(cl-defun superchat-workflow-build-smart-context (context &key max-length)
  "智能构建上下文摘要，避免LLM过载。

CONTEXT: 工作流上下文对象
MAX-LENGTH: 最大上下文长度（默认为2000字符）

返回优化后的上下文摘要字符串。"
  (let* ((max-len (or max-length 2000))
         (recent-results (superchat-workflow-get-recent-results context 2))
         (variables-text (superchat-workflow-format-variables context))
         (current-step (superchat-workflow-context-current-step context))
         (total-steps (length (superchat-workflow-context-steps context)))
         (context-parts '()))

    ;; 1. 添加执行进度信息
    (push (format "工作流进度: 步骤 %d/%d" current-step total-steps) context-parts)

    ;; 2. 添加最近的执行结果（限制长度）
    (when recent-results
      (let* ((results-summary (if (> (length recent-results) 500)
                                  (concat (substring recent-results 0 500) "...")
                                recent-results)))
        (push (format "最近执行结果:\n%s" results-summary) context-parts)))

    ;; 3. 添加变量信息（如果有）
    (when (and variables-text (not (string-empty-p (string-trim variables-text))))
      (push (format "变量信息:\n%s" variables-text) context-parts))

    ;; 4. 组合上下文并限制总长度
    (let* ((full-context (string-join (nreverse context-parts) "\n\n---\n\n"))
           (trimmed-context (if (> (length full-context) max-len)
                                (concat (substring full-context 0 max-len) "...")
                              full-context)))
      (format "【智能上下文摘要】\n%s" trimmed-context))))

;;;-----------------------------------------------
;;; Helper Functions
;;;-----------------------------------------------

(defun superchat-workflow--format-step-info (step)
  "Format the step information for logging output."
  (let ((model (superchat-workflow-step-model step))
        (command (superchat-workflow-step-command step))
        (contexts (superchat-workflow-step-contexts step)))
    (string-join
     (delq nil
           (list (when model (format "@%s" model))
                 (when command (format "/%s" command))
                 (when contexts (format "#%d-files" (length contexts)))))
     " ")))

(provide 'superchat-workflow)
