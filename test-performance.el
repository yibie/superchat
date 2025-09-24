;; test-performance.el - 测试性能优化后的 superchat-memory
;; usage: emacs -Q --batch -L . -l test-performance.el

;; 1. 加载必要的库
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'org-id)

;; 2. 设置测试环境
(setq superchat-data-directory "./test")

;; 3. 加载被测文件（跳过 org-ql 和 gptel）
(defvar org-ql-cache nil)
(declare-function org-ql-select "org-ql" (files query &rest options))
(declare-function gptel-request "gptel" (prompt &rest args))
(defvar superchat-memory-keyword-extractor-llm-prompt nil)

(load-file "superchat-memory.el")

;; 4. 测试性能优化
(message "Testing performance optimizations...")

;; 检查配置变量
(message "Max entries to process: %d" superchat-memory-max-entries-to-process)
(message "Max concurrent requests: %d" superchat-memory-max-concurrent-llm-requests)

;; 测试条目限制功能
(let* ((file (superchat-memory--get-file))
       (all-entries (when (file-exists-p file)
                      (with-current-buffer (find-file-noselect file)
                        (org-with-wide-buffer
                         (let (collected)
                           (org-map-entries (lambda ()
                                              (push (superchat-memory--org-ql-entry) collected))
                                            "-ARCHIVED" 'file)
                           (nreverse collected)))))))
  (message "Total entries in memory file: %d" (length all-entries))
  (when (> (length all-entries) superchat-memory-max-entries-to-process)
    (message "✓ Entry limiting will be applied (found %d > %d limit)" 
             (length all-entries) superchat-memory-max-entries-to-process)))

;; 测试异步函数（不会实际调用 LLM）
(message "Testing async function with limited entries...")
(superchat-memory--find-merge-candidates-async
 (lambda (groups)
   (message "✓ Async function completed successfully with %d groups" (length groups))
   (message "Performance test completed!")))

;; 等待异步操作完成
(sit-for 1)
(message "Test finished.")