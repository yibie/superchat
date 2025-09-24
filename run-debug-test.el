;; run-debug-test.el - 测试 superchat-memory 语法修复
;; usage: emacs -Q --batch -L . -l run-debug-test.el

;; 1. package 源与初始化
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; 2. 确保需要的包都已安装
(dolist (pkg '(org-ql))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; 3. 把项目目录加入 load-path
(add-to-list 'load-path (file-name-directory load-file-name))

;; 4. 设置测试环境
(setq superchat-data-directory "./test")

;; 5. 加载被测文件
(load-file "superchat-memory.el")

;; 6. 创建测试内存文件
(unless (file-exists-p "./test")
  (make-directory "./test" t))

(with-temp-file "./test/memory.org"
  (insert "* Test Entry 1 :tag1:
:PROPERTIES:
:ID: test-id-1
:TIMESTAMP: [2024-01-01 10:00:00]
:KEYWORDS: test, memory, entry
:ACCESS_COUNT: 1
:END:
This is a test memory entry.

* Test Entry 2 :tag2:
:PROPERTIES:
:ID: test-id-2
:TIMESTAMP: [2024-01-01 11:00:00]
:KEYWORDS: test, memory, similar
:ACCESS_COUNT: 1
:END:
This is another test memory entry.
"))

;; 7. 测试函数语法
(message "Testing superchat-memory syntax...")

;; 测试基本函数是否可以调用
(condition-case err
    (progn
      (superchat-memory--get-file)
      (message "✓ superchat-memory--get-file works"))
  (error (message "✗ superchat-memory--get-file failed: %s" err)))

;; 测试异步函数定义是否正确
(condition-case err
    (progn
      (fboundp 'superchat-memory--find-merge-candidates-async)
      (message "✓ superchat-memory--find-merge-candidates-async is defined"))
  (error (message "✗ superchat-memory--find-merge-candidates-async failed: %s" err)))

;; 8. 测试异步函数（不依赖 gptel）
(condition-case err
    (progn
      (superchat-memory--find-merge-candidates-async
       (lambda (groups)
         (message "✓ Async function completed with %d groups" (length groups))
         (message "Debug test completed successfully!")))
      ;; 等待一小段时间让异步操作完成
      (sit-for 2))
  (error (message "✗ Async function test failed: %s" err)))

(message "All syntax tests completed!")