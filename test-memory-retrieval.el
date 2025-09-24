;; test-memory-retrieval.el - 测试记忆检索功能
;; usage: emacs -Q --batch -L . -l test-memory-retrieval.el

;; 1. 加载必要的库
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'org-id)

;; 2. 设置测试环境
(setq superchat-data-directory "./test")

;; 3. 加载文件
(load-file "superchat-memory.el")

;; 4. 测试记忆检索功能
(message "Testing memory retrieval...")

;; 检查内存文件是否存在
(let ((memory-file (superchat-memory--get-file)))
  (message "Memory file path: %s" memory-file)
  (message "Memory file exists: %s" (file-exists-p memory-file)))

;; 测试阈值检查函数
(message "Testing threshold check...")
(message "Auto-recall min length: %d" superchat-memory-auto-recall-min-length)

;; 测试记忆检索
(message "Testing memory retrieval with query 'test'...")
(let ((memories (superchat-memory-retrieve "test")))
  (message "Retrieved %d memories" (length memories))
  (when memories
    (dolist (mem memories)
      (message "- Memory: %s (ID: %s)" (plist-get mem :title) (plist-get mem :id)))))

;; 测试记忆检索（更具体的查询）
(message "Testing memory retrieval with query 'body1'...")
(let ((memories (superchat-memory-retrieve "body1")))
  (message "Retrieved %d memories for 'body1'" (length memories))
  (when memories
    (dolist (mem memories)
      (message "- Memory: %s (ID: %s)" (plist-get mem :title) (plist-get mem :id)))))

;; 测试记忆检索（关键词查询）
(message "Testing memory retrieval with query 'entry'...")
(let ((memories (superchat-memory-retrieve "entry")))
  (message "Retrieved %d memories for 'entry'" (length memories))
  (when memories
    (dolist (mem memories)
      (message "- Memory: %s (ID: %s)" (plist-get mem :title) (plist-get mem :id)))))

(message "Memory retrieval test completed!")