# Spec — phase-persistence-sqlite-20260313

## Summary

将当前临时的 `snapshot.json` 存储层替换为正式的 SQLite 数据库，并引入 Workspace 概念（用户选择数据库位置），附件文件与数据库同目录存储。

## Goals

- 用 SQLite 替代 snapshot.json 作为唯一持久化层
- 支持用户选择/创建 Workspace 目录（`.threadnote` 包格式）
- 附件文件复制到 `workspace/attachments/`，entry 存相对路径
- 重启后通过 NSFileBookmark 恢复 Workspace 访问权限
- 迁移路径：首次启动新版本时从 snapshot.json 自动导入

## Non-goals

- 不实现 iCloud Sync（可作为后续 phase）
- 不实现多 Workspace 并行打开
- 不实现数据库加密
- 不改变 Entry/Thread/AppSnapshot 的数据模型语义

## User Flows

### 首次启动
1. App 启动，检测无 Workspace → 弹出欢迎界面
2. 用户点击"新建工作区" → NSSavePanel 选择目录
3. App 在选定位置创建 `MyWorkspace.threadnote/` 目录结构
4. 加载种子数据，进入主界面

### 已有 snapshot.json 的用户（迁移）
1. App 启动，检测到旧 snapshot.json 但无 Workspace → 提示迁移
2. 用户选择 Workspace 位置
3. App 自动将 snapshot.json 数据导入 SQLite，旧文件备份为 snapshot.json.bak

### 正常使用
1. App 启动 → 从 UserDefaults 读取 Workspace bookmark → 恢复访问
2. 所有读写走 SQLite，对 Store 层透明

### 拖入文件
1. 用户拖文件到 CaptureEditor
2. 文件复制到 `workspace/attachments/<uuid>.<ext>`
3. Entry body 存相对路径 `attachments/<uuid>.<ext>`
4. 渲染时拼接 Workspace 路径还原绝对路径

## Edge Cases

- Workspace 目录被用户手动删除 → 启动时检测失败 → 重新选择
- 附件文件丢失 → RichBodyView 显示占位符，不崩溃
- SQLite 写入失败（磁盘满）→ 捕获错误，显示提示，不丢失内存中的数据
- 同一文件拖入两次 → 按内容 hash 去重，复用同一个 attachments 文件

## Acceptance Criteria

- [ ] App 首次启动弹出 Workspace 选择界面
- [ ] 选定 Workspace 后，目录内有 `db.sqlite` 和 `attachments/`
- [ ] 所有 Entry/Thread 数据从 SQLite 读写，snapshot.json 不再生成
- [ ] 拖入文件后，文件出现在 `attachments/`，entry 可正常渲染
- [ ] 重启 App 后数据完整保留
- [ ] 旧 snapshot.json 用户可一键迁移
