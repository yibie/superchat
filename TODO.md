# TODO.md

## 功能开发计划

### 1. ✅ 集成 gptel tools 支持 - **已完成**

**目标**: 让 superchat 可以直接调取 gptel tools

**✅ 已实现功能**:
- 直接读取用户在 gptel 中配置的 tools，无需重复配置
- 在聊天界面中无缝使用 gptel 的工具调用功能
- 支持 function calling 能力，可以调用外部工具和 API
- 添加 `/tools` 命令查看当前 tools 状态

**✅ 实现方式**:
- 修改 `superchat--llm-generate-answer` 函数，自动使用 gptel 的 tools 配置
- 添加 `superchat-get-gptel-tools()` 和 `superchat-gptel-tools-enabled-p()` 函数
- 实现工具状态查看功能
- 保持向后兼容，不影响现有功能

**✅ 技术实现**:
```elisp
;; 核心集成：自动使用 gptel 的 tools 配置
(let ((gptel-use-tools (superchat-gptel-tools-enabled-p))
      (gptel-tools (superchat-get-gptel-tools)))
  (gptel-request prompt :stream t ...))
```

**✅ 使用方法**:
1. 用户在 gptel 中配置 tools（设置 `gptel-use-tools` 和 `gptel-tools`）
2. 启动 SuperChat，tools 功能自动启用
3. 使用 `/tools` 命令查看当前 tools 状态
4. 在对话中直接使用 tools 功能

---

### 2. 集成 gptel MCP 支持
**目标**: 让 superchat 可以直接调取 gptel MCP (Model Context Protocol)

**需求描述**:
- 支持 MCP 协议的各种服务器
- 可以连接和使用不同的 MCP 服务
- 在聊天中直接使用 MCP 提供的功能

**实现思路**:
- 了解 gptel 中 MCP 的实现方式
- 在 superchat 中添加 MCP 连接管理
- 实现 MCP 功能的命令系统集成
- 添加 MCP 服务状态监控

**技术挑战**:
- MCP 服务的连接管理和故障恢复
- 不同 MCP 服务的兼容性处理
- 性能优化和资源管理

---

### 3. ✅ 已完成: @ 模型切换功能
**目标**: 让 superchat 可以直接通过 @ 更换后端的模型

**✅ 已实现功能**:
- 在聊天输入中使用 `@模型名` 语法快速切换模型
- 添加 `/models` 命令查看可用模型列表
- 实现临时模型切换，单次请求后自动恢复原模型
- 完整的测试套件验证功能正常工作

**✅ 实现方式**:
- 添加 `superchat--parse-model-switch()` 函数检测 @model_name 语法
- 修改 `superchat--send-input()` 函数处理模型切换逻辑
- 更新 `superchat--llm-generate-answer()` 支持临时模型切换
- 实现模型的临时设置和自动恢复机制

**✅ 技术实现**:
```elisp
;; @ 语法解析
(defun superchat--parse-model-switch (input)
  "Parse input for @model syntax and return (clean-input . model) cons."
  (when (and input (string-match "@\\([a-zA-Z0-9_-]+\\)" input))
    (let* ((model-name (match-string 1 input))
           (clean-input (replace-regexp-in-string "@[a-zA-Z0-9_-]+" "" input)))
      (cons (string-trim clean-input) model-name))))

;; 临时模型切换
(let ((original-model (when (boundp 'gptel-model) gptel-model)))
  (when target-model
    (setq gptel-model target-model))
  ;; ... 执行请求 ...
  ;; 恢复原模型
  (when original-model
    (setq gptel-model original-model)))
```

**✅ 使用示例**:
```
用户: @gpt-4o 帮我分析这段代码
用户: @claude-3-haiku 翻译这句话  
用户: @gemini-pro 总结这篇文章
用户: /models  # 查看可用模型
```

**✅ 测试验证**:
- 创建了完整的测试套件 `test/test-model-switching.el`
- 验证了 @ 语法解析功能正常工作
- 确认了模型切换逻辑和自动恢复机制
- 测试了可用模型列表功能

---

## 开发进度总结

### ✅ 已完成功能
1. **gptel tools 集成** - 零配置集成 gptel 的工具调用功能
2. **@ 模型切换功能** - 临时模型切换，单次请求后自动恢复

### 🔄 进行中/待完成功能
1. **gptel MCP 集成** - 支持模型上下文协议服务器

## 当前状态
- **已完成**: 2/3 主要功能
- **用户体验**: 显著提升，支持工具调用和模型切换
- **代码质量**: 保持零配置理念，完整的测试覆盖
- **向后兼容**: 所有现有功能保持不变

## 备注

- 所有新功能都需要保持向后兼容性
- 需要更新相应的文档和测试用例
- 考虑性能影响，特别是在大型对话历史中的表现