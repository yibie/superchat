# Task List — phase-electron-ai-cockpit-productization-20260315

task001 [ ] 场景: 建立 cockpit productization 阶段文档门禁 | Given: parity 阶段已完成，接下来进入产品化阶段 | When: 新建 spec/plan/task/change 并更新 PHASES/CHANGE 索引 | Then: 后续工作不再混在 parity 阶段里 | 验证: 文档检查
task002 [ ] 场景: 定义 planner block 到线程实体的映射表 | Given: 当前 planner blocks 主要还是只读 | When: 明确 judgment/gap/evidence/sources/questions 等 block 各自能触发的动作 | Then: cockpit 行动化有稳定边界 | 验证: 文档检查
task003 [ ] 场景: 为 Restart Note 增加最小可操作 block | Given: 右侧 cockpit 已能稳定渲染 planner blocks | When: 先让一种高价值 block 支持真实动作 | Then: cockpit 从摘要面板升级为工作入口 | 验证: 手动验证
task004 [ ] 场景: 区分 writing/review/summary 三种 Prepare 结构 | Given: 当前 Prepare 仍偏同构 | When: 为三种类型定义不同布局、字段和目标 | Then: Prepare 不再只是换标题的同一面板 | 验证: 手动验证
task005 [ ] 场景: 让 Prepare 结果支持回写 thread | Given: 当前 Prepare 是临时视图 | When: 增加将 Prepare 结果转为 note/task/anchor 的路径 | Then: AI 工作流形成闭环 | 验证: 单元测试+手动验证
task006 [ ] 场景: 让回写动作触发 thread state 重新综合 | Given: 写回后当前 AI 结果可能过时 | When: 回写动作统一触发 invalidate + re-synthesize | Then: cockpit 总是反映最新 thread 状态 | 验证: 单元测试
task007 [ ] 场景: 用真实使用流复审 AI cockpit 是否值得进入下一开发阶段 | Given: 产品化完成后需要判断是否形成稳定工作台 | When: 以一条真实 thread 流程做端到端复审 | Then: 新阶段是否继续扩展会有依据 | 验证: 文档检查
task008 [x] 场景: 将 thread stage 从标题旁迁移到 Restart Note 解释区 | Given: 当前 stage 标签直接贴在标题旁但缺少解释 | When: 移除标题旁 badge 并在 Restart Note 中展示阶段名与含义 | Then: 用户能在恢复上下文时理解 stage 而不是把它误认为标题的一部分 | 验证: 渲染测试
