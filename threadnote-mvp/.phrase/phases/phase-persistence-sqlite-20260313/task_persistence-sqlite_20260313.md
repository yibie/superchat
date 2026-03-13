# Task List — phase-persistence-sqlite-20260313

task001 [ ] 场景: 创建 WorkspaceManager | Given: App 无已知 Workspace | When: 用户选择目录 | Then: 创建 .threadnote 包结构，bookmark 写入 UserDefaults | 验证: 目录含 db.sqlite 占位 + attachments/

task002 [ ] 场景: 启动恢复 Workspace | Given: UserDefaults 有 bookmark | When: App 启动 | Then: 通过 bookmark 解析路径，验证目录有效 | 验证: 主界面正常加载

task003 [ ] 场景: 引入 GRDB 依赖 | Given: Package.swift | When: 添加 GRDB SPM 依赖 | Then: 项目编译通过 | 验证: xcodebuild 无 error

task004 [ ] 场景: 设计并创建 SQLite schema | Given: 现有 Models.swift | When: 创建 DatabaseManager | Then: threads/entries 表结构与现有数据模型对应 | 验证: 可插入/查询 ThreadRecord 和 Entry

task005 [ ] 场景: Store 切换到 DatabaseManager | Given: Store 当前调用 PersistenceManager | When: 替换为 DatabaseManager | Then: 所有读写走 SQLite，snapshot.json 停止生成 | 验证: 数据重启后保留

task006 [ ] 场景: 创建 AttachmentManager | Given: Workspace 已初始化 | When: 拖入文件 | Then: 文件复制到 attachments/<hash>.<ext>，返回相对路径 | 验证: attachments/ 目录有对应文件

task007 [ ] 场景: CaptureEditor 使用 AttachmentManager | Given: 用户拖文件到 CaptureEditor | When: performDragOperation | Then: 调用 AttachmentManager，插入相对路径而非 file:// 绝对路径 | 验证: entry 存相对路径，跨目录移动后仍可渲染

task008 [ ] 场景: RichBodyView 解析相对路径 | Given: entry 含 attachments/ 相对路径 | When: 渲染 | Then: 拼接 Workspace 路径，显示图片/文件 | 验证: 图片正常显示

task009 [ ] 场景: 迁移旧 snapshot.json | Given: 存在旧 snapshot.json | When: App 首次以新版本启动 | Then: 提示迁移，数据导入 SQLite，旧文件备份为 .bak | 验证: 迁移后所有数据可见

task010 [ ] 场景: 注册 .threadnote document type | Given: Info.plist / project.yml | When: Finder 中双击 .threadnote | Then: App 打开对应 Workspace | 验证: Finder 显示为单一文件图标
