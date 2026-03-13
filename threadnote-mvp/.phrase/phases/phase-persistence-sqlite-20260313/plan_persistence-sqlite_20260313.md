# Plan — phase-persistence-sqlite-20260313

## Milestones

### M1 — Workspace 选择与目录结构
- `WorkspaceManager`：封装目录创建、bookmark 持久化、路径解析
- App 启动流程：无 Workspace → 选择界面；有 bookmark → 恢复
- 目录结构：`workspace.threadnote/db.sqlite` + `workspace.threadnote/attachments/`

### M2 — SQLite 层（GRDB 或原生 SQLite3）
- 设计 schema：threads、entries、snapshots（版本/迁移控制）
- `DatabaseManager`：替换 `PersistenceManager`，提供相同接口
- Store 层改为调用 DatabaseManager，对上层透明

### M3 — 附件管理
- `AttachmentManager`：复制文件到 attachments/，按内容 hash 去重
- CaptureTextView 拖放改为调用 AttachmentManager，插入相对路径
- RichBodyView 渲染时解析相对路径 → 绝对路径

### M4 — 迁移路径
- 检测旧 snapshot.json，提示迁移
- 导入逻辑：JSON → SQLite
- 旧文件备份

## Scope

**In scope**: WorkspaceManager、DatabaseManager、AttachmentManager、迁移工具、启动流程

**Out of scope**: iCloud、多 Workspace、加密、搜索索引（FTS）

## Priorities

1. M1（Workspace）— 基础，阻塞其他
2. M2（SQLite）— 核心，替换 snapshot.json
3. M3（附件）— 重要，但可在 M2 之后独立迭代
4. M4（迁移）— 完整性，上线前必须有

## Technology Decision

**SQLite 库选型**：
- **GRDB.swift**（推荐）：Swift-native，类型安全，支持 Codable，migration 系统完善
- 原生 SQLite3（备选）：无依赖，但 boilerplate 多

**Workspace 格式**：
- `.threadnote` 目录（macOS package，Finder 中显示为单一文件）
- 用 `LSItemContentTypes` 在 Info.plist 注册为 document type

## Risks & Dependencies

- GRDB 需要通过 SPM 引入（Package.swift 已有基础）
- NSFileBookmark 在沙盒外需要 `com.apple.security.files.user-selected.read-write` entitlement（已有）
- Schema 变更需要 migration 系统，设计时留好版本号

## Rollback

- M2 合并前，snapshot.json 路径保留为可切换选项（feature flag in Store）
- 迁移失败时保留 snapshot.json，不删除原始数据
