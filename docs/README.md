# Documentation

## 使用与开发入口

- [项目 README](../README.md) / [中文 README](../README_cn.md)：安装、配置与交互。
- [Skills 快速开始](SKILLS_QUICKSTART.md)：本地技能、Workflow 区别与导入导出。
- [架构说明](architecture.md)：当前模块分工与扩展入口。
- [插件开发指南](plugin-guide.md)：插件注册与 Runtime/Authority 接口。
- [领域词汇](../CONTEXT.md)：Session、Run、Tape、View 等概念。
- [架构图](architecture-map/README.md)：可编辑图及历史源码快照；需用当前代码核实。
- [Session EditorWorkspace 关闭](protocols/session-editor-workspace-close.md)：关闭行为边界。

## 设计与研究资料

- `adr/`：产品设计决策，注意每篇的 accepted/proposed 状态。
- `design/`：仍有用途的设计解释和实验合同，不等于全部已实现。
- `research/`：注明日期的调研与证据边界，不是当前依赖版本或功能承诺。
- `goals/`、`decision-maps/`：保留的方向讨论与实验决策历史，不是自动执行队列。

旧版 Org Memory 实施方案、过时 Agent/Runtime 快照、已替代的开发交接和
重复转换教程已清理。早期 Memory 探索文章仍作为研究保留；当前 Memory
实现是 SQLite/Tape 路径，不以那些文章中的 Org/org-ql 方案为准。

部分 `design/experience-view-internal-acceptance-contract-*.md` 被实验或测试
按字节读取和校验，不能仅因版本号旧而删除或改写。它们不授权重新执行模型实验。
当前行为以代码与测试为准；普通开发不要求更新历史实验记录。
