;; test-llm-keywords.el - 测试LLM关键词提取功能
;; usage: emacs -Q --batch -L . -l test-llm-keywords.el

;; 1. 加载必要的库
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'json)

;; 2. 设置测试环境
(setq superchat-data-directory "./test")

;; 3. 加载文件
(load-file "superchat-memory.el")

;; 4. 测试LLM关键词提取
(message "Testing LLM keyword extraction...")

;; 测试中文查询
(let ((query "之前关于 javascript 讨论了什么"))
  (message "Testing query: '%s'" query)
  
  ;; 测试本地关键词生成（改进后的版本）
  (let ((local-keywords (superchat-memory--generate-local-keywords query nil)))
    (message "Local keywords: %s" local-keywords))
  
  ;; 测试LLM关键词提取（如果可用）
  (if (and (featurep 'gptel) (fboundp 'gptel-request))
      (progn
        (message "Testing LLM keyword extraction...")
        (superchat-memory--extract-keywords-with-llm-async 
         query
         (lambda (keywords)
           (message "LLM extracted keywords: %s" keywords)
           (message "LLM keyword extraction test completed!")))
        (sit-for 5))
    (message "LLM not available, skipping LLM keyword extraction test")))

;; 测试异步记忆检索
(message "Testing async memory retrieval...")
(superchat-memory-retrieve-async 
 "javascript"
 (lambda (memories)
   (message "Async retrieval found %d memories" (length memories))
   (message "Async memory retrieval test completed!")))

(sit-for 2)
(message "All tests completed!")