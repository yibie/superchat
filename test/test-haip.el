;; test/test-workflow-parser.el --- Tests for HAIP Parser

       (require 'ert)
       (require 'superchat-workflow) ;; 假设你的解析器函数在这个文件里

       ;;;; Test Cases

       (ert-deftest test-haip-parser-happy-path ()
         "Test parsing a typical, multi-step HAIP prompt."
         (let ((prompt "
       /AI技术新闻摘要
       description: 每周技术新闻摘要

       帮我完成一份本周的AI技术新闻摘要。

       首先，请使用 @SearchTool 这个工具来查找关于 #www.AI 和 #www.tech 的最新新闻，时间范围限定在过去7天内。

       然后，将搜索到的结果，交给 @AnalystAgent 这个专业的分析Agent，让他执行 /summarize 任务，最后把结果保存到本地文件#local.md。
       "))
               (expected-output
                '((:executor "@SearchTool"
                   :actions nil
                   :contexts '("#www.AI" "#www.tech")
                   :prompt "首先，请使用 @SearchTool 这个工具来查找关于 #www.AI 和 #www.tech 的最新新闻，时间范围限定在过去7天内。")
                  (:executor "@AnalystAgent"
                   :actions '("/summarize")
                   :contexts '("#local.md")
                   :prompt "然后，将搜索到的结果，交给 @AnalystAgent 这个专业的分析Agent，让他执行 /summarize 任务，最后把结果保存到本地文件#local.md。")))
               (actual-output (haip-parse-prompt prompt)))
           (should (equal actual-output expected-output))) 

       (ert-deftest test-haip-parser-no-executor ()
         "Test that text without an @executor is ignored."
         (let ((prompt "This is just a regular sentence without any commands."))
           (should (null (haip-parse-prompt prompt)))))

       (ert-deftest test-haip-parser-empty-string ()
         "Test that an empty string produces no steps."
         (should (null (haip-parse-prompt ""))))

       (ert-deftest test-haip-parser-single-simple-step ()
         "Test a single line prompt with all components."
         (let ((prompt "Please @GPT4 summarize #doc.txt using /summary-template.")
               (expected-output
                '((:executor "@GPT4"
                   :actions '("/summary-template")
                   :contexts '("#doc.txt")
                   :prompt "Please @GPT4 summarize #doc.txt using /summary-template."))))
           (should (equal (haip-parse-prompt prompt) expected-output))))

       (ert-deftest test-haip-parser-multiple-contexts-and-actions ()
         "Test a step with multiple contexts and actions."
         (let ((prompt "@Tool process #file1 and #file2 with /action1 and /action2.")
               (expected-output
                '((:executor "@Tool"
                   :actions '("/action1" "/action2")
                   :contexts '("#file1" "#file2")
                   :prompt "@Tool process #file1 and #file2 with /action1 and /action2."))))
           (should (equal (haip-parse-prompt prompt) expected-output))))

       (ert-deftest test-haip-parser-multiple-executors-in-one-block ()
         "Test that only the first @executor in a block is used."
         (let ((prompt "Use @ToolA to do this, then ask @ToolB to do that.")
               (expected-output
                '((:executor "@ToolA"
                   :actions nil
                   :contexts nil
                   :prompt "Use @ToolA to do this, then ask @ToolB to do that."))))
           (should (equal (haip-parse-prompt prompt) expected-output))))
