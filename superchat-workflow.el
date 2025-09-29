;; 全局哈希表，用于存储所有已注册的执行器
(defvar haip--executor-registry (make-hash-table :test 'equal))

;; 一个帮助函数，用于向注册表里添加新的执行器
(defun haip-register-executor (name function)
  "Register an EXECUTOR by mapping its NAME (a string like \"@SearchTool\")
   to a FUNCTION object."
  (if (not (fboundp function))
      (error "Cannot register '%s': '%s' is not a valid function" name function)
    (puthash name function haip--executor-registry)
    (message "HAIP executor %s registered." name)))

(defun haip-parse-prompt (prompt-string)
  "Parse a HAIP PROMPT-STRING into a list of structured steps."
  (let ((results '()))
    (let ((blocks (split-string prompt-string "\n\\s-*\n" t "[ \t\n]")))
      (dolist (block blocks)
        (let ((step-plist '())
              (executors '())
              (contexts '())
              (actions '()))

          (with-temp-buffer
            (insert block)
            (goto-char (point-min))
            ;; 使用明确的 [^ \t\n]+ 来保证可靠性
            (while (re-search-forward "@\\([^ \t\n]+\\)" nil t)
              (push (match-string 1) executors))

            (goto-char (point-min))
            (while (re-search-forward "#\\([^ \t\n]+\\)" nil t)
              (push (match-string 1) contexts))

            (goto-char (point-min))
            (while (re-search-forward "/\\([^ \t\n]+\\)" nil t)
              (push (match-string 1) actions)))

          (when executors
            (setq step-plist
                  (list :executor (car (nreverse executors))
                        :actions (nreverse actions)
                        :contexts (nreverse contexts)
                        :prompt block)))

          (when step-plist
            (push step-plist results)))))
    (nreverse results)))

(defun haip-resolve-context (context-item last-step-result)
  "Resolve a single CONTEXT-ITEM string into its actual value.
   This version fetches real data from files and URLs."
  (cond
   ;; 如果上下文是 :from-previous-step，就返回上一步的结果
   ((equal context-item ":from-previous-step")
    last-step-result)

   ;; 如果是 URL (以 #http 开头)
   ((string-match-p "^#https?://" context-item)
    (let ((url (substring context-item 1)))
      (condition-case err
          (with-current-buffer (url-retrieve-synchronously url)
            (goto-char (point-min))
            ;; 假设是网页，通常需要处理 HTML，这里简化为 buffer 内容
            (buffer-string))
        (error (format "Error fetching URL '%s': %s" url (error-message-string err))))))

   ;; 如果是本地文件
   ((string-prefix-p "#" context-item)
    (let ((file (expand-file-name (substring context-item 1))))
      (if (not (file-exists-p file))
          (format "Error: File not found at '%s'" file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string)))))

   ;; 其他情况原样返回
   (t context-item)))


(defun haip-parse-prompt (prompt-string)
  "Parse a HAIP PROMPT-STRING into a list of structured steps."
  (let ((results '()))
    (let ((blocks (split-string prompt-string "\n\\s-*\n" t "[ \t\n]")))
      (dolist (block blocks)
        (let ((step-plist '())
              (executors '())
              (contexts '())
              (actions '()))

          (with-temp-buffer
            (insert block)
            (goto-char (point-min))

            ;; 捕获完整的 @Executor 符号
            (while (re-search-forward "\\(@[a-zA-Z0-9_-]+\\)" nil t)
              (push (match-string 1) executors))

            (goto-char (point-min))
            ;; 捕获完整的 /Action 符号
            (while (re-search-forward "\\(/[a-zA-Z0-9_-]+\\)" nil t)
              (push (match-string 1) actions))

            (goto-char (point-min))
            ;; 捕获完整的 #Context 符号
            (while (re-search-forward "\\(#[^ \t\n]+\\)" nil t)
              (push (match-string 1) contexts))

            ;; 新增：明确捕获 :from-previous-step 关键词
            (goto-char (point-min))
            (while (re-search-forward "\\(:from-previous-step\\)" nil t)
              (push (match-string 1) contexts)))

          (when executors
            (setq step-plist
                  (list :executor (car (nreverse executors))
                        :actions (nreverse actions)
                        :contexts (nreverse contexts) ; 现在 contexts 列表包含了所有类型的上下文
                        :prompt block)))

          (when step-plist
            (push step-plist results)))))
    (nreverse results)))

(defun haip-run-prompt (prompt-string)
  "Parse and execute a HAIP prompt string."
  (let* ((steps (haip-parse-prompt prompt-string)))
    (if (not steps)
        (message "Invalid HAIP prompt: No valid steps found.")
      (haip-execute-workflow steps))))

(defun my-fake-search-tool (prompt contexts)
  "This is a fake search tool for testing.
   It ignores PROMPT and CONTEXTS, and just returns a fixed string."
  (message "Fake @SearchTool called with prompt: %s" prompt)
  (message "Fake @SearchTool called with contexts: %s" contexts)
  "这是伪造的搜索结果：AI is taking over the world.")

;; 现在，注册这个伪造的工具
(haip-register-executor "@SearchTool" #'my-fake-search-tool)

(defun my-fake-analyst-agent (prompt contexts)
  (format "AnalystAgent received contexts: %s. Returning final summary." contexts))

(haip-register-executor "@AnalystAgent" #'my-fake-analyst-agent)



(provide 'superchat-workflow)
