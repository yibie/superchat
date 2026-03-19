# Qwen3.5:9b 本地模型优化实操记录

## 1. 背景与目标

本轮优化只针对本地模型，不做云端模型对比。

目标不是一次性把所有性能问题都解决，而是先把本地 AI 首响应时间做成可重复、可比较、可回归的真实基线，再根据真实数据做最小、可验证的优化。

本轮明确聚焦：

- Provider: `ollama`
- 主模型: `qwen3.5:9b`
- 任务类型:
  - `ping`
  - `prepareDraft`
  - `synthesizeResume`

不做的事：

- 不做云端模型对比
- 不做大规模 UI 重构
- 不做多模型路由
- 不做激进并发优化
- 不引入新的远端 serving 基础设施

## 2. 初始观察

最开始的直觉是“本地模型只是慢”，但真实 benchmark 很快证明这个判断不完整。

先得到的初始事实：

- `qwen3.5:4b` 的本地 `ping` 最快，可用性最好
- `qwen3.5:9b` 明显更慢，但质量更有可能满足 Threadnote 的结构化任务
- `27b/35b` 在当前交互目标下过慢
- `0.8b` 在当前环境下甚至会超时

对应基线文件：

- [ai-local-ping-compare-latest.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-local-ping-compare-latest.json)

### 2.1 初始 ping 耗时记录

这轮 session 早期，本地模型 `ping` 的代表性数据是：

| 模型 | 初始 ping 耗时 |
|---|---:|
| `qwen3.5:4b` | `8.96s` |
| `qwen3.5:9b` | `16.27s` |
| `qwen3.5:35b` | `25.42s` |
| `qwen3.5:27b` | `26.59s` |
| `qwen3.5:0.8b` | `60s` 内超时 |

这组数据的意义不是为了说明 `qwen3.5:9b` 最快，而是说明：

- `9b` 比 `27b/35b` 更接近可交互区间
- 但它仍然不够快，尤其是在真实结构化任务里
- 也因此，后续优化才聚焦在 `qwen3.5:9b`

## 3. 第一阶段：补真实基线与观测

### 3.1 新增真实 provider benchmark

新增脚本：

- [benchmark-ai-real.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/benchmark-ai-real.mjs)
- [benchmark-ai-compare.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/benchmark-ai-compare.mjs)

新增命令：

- `npm run benchmark:ai-real`
- `npm run benchmark:ai-real:record`
- `npm run benchmark:ai-compare`
- `npm run benchmark:ai-compare:record`

目标是让真实 provider 的端到端 AI 调用，和现有 large-thread benchmark 分离。

### 3.2 新增分段计时

在 AI 调用链中增加了这些分段数据：

- `queueWaitMS`
- `clientCreateMS`
- `providerCallMS`
- `totalMS`
- `promptBytes`
- `responseBytes`
- `responseLength`
- `modelID`
- `backendLabel`
- `coldStartClient`

相关实现主要在：

- [threadnoteAIService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/services/threadnoteAIService.js)
- [aiProviderRuntime.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/aiProviderRuntime.js)

### 3.3 新增失败样本保留

benchmark 不再因为单次失败就丢失结构，而是保留：

- 失败 sample
- 错误信息
- 最近一次测量
- 原始响应预览
- 原始响应 body 摘要

这一步非常关键，因为后面定位 Ollama 空响应问题时，靠的就是这些失败诊断信息。

## 4. 第二阶段：定位真正瓶颈

### 4.1 真实问题不是“纯慢”

一开始跑 `qwen3.5:9b` 的 `prepare` / `resume` 时，出现了一个非常反常的现象：

- `done_reason = "length"`
- `eval_count > 0`
- `prompt_eval_count > 0`
- 但 `response` 是空字符串

这说明模型并不是没跑，而是“跑了，但可用输出没落进 `response`”。

### 4.2 补低层诊断脚本

为避免继续在上层 prompt 上盲调，新增了低层探针：

- [diagnose-ollama-generate.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/diagnose-ollama-generate.mjs)

对应命令：

- `npm run benchmark:ollama-probe -- --model qwen3.5:9b --scenario prepare`

后续又扩展成可多模型对比：

- `npm run benchmark:ollama-probe -- --models qwen3.5:4b,qwen3.5:9b --scenario prepare --cases default,think-false`

### 4.3 低层 probe 的关键发现

probe 明确证明：

- 默认 Ollama 行为下，thinking-capable model 会把大量 token 消耗在 `thinking`
- 同样的 `num_predict` 预算下，`response` 可能为空，或者 JSON 被截断
- 显式发送 `think: false` 后，可用 JSON 会恢复

这是本轮最关键的定位结论。

换句话说，问题并不只是“模型慢”，而是“默认 reasoning 模式把输出预算吃光了”。

## 4.5 各阶段大模型响应耗时记录

下面把本轮过程中测到的 `qwen3.5:9b` 关键耗时按阶段列出来，便于直接看优化前后变化。

### 4.5.1 阶段一：问题尚未定位前

现象：

- `prepare` / `resume` 经常超时，或者返回空 `response`
- 即使 Ollama 显示 `eval_count > 0`，上层仍然拿不到可用 JSON

代表性观测：

- `prepare`
  - `providerCallMS` 约 `3168ms`
  - `responseChars = 0`
  - `done_reason = "length"`
- `resume`
  - `providerCallMS` 约 `5469ms`
  - `responseChars = 0`
  - `done_reason = "length"`

这一阶段的耗时不能算“成功基线”，因为结果本身不可用。

### 4.5.2 阶段二：定位到 thinking 问题后的低层 probe

通过低层 probe 比较 `default` 与 `think:false`：

- `prepare` probe
  - `default`: 约 `3202ms`，`responseChars = 0`，`thinkingChars = 485`
  - `think:false`: 约 `2941ms`，`responseChars = 565`
- `resume` probe
  - `default`: 约 `5679ms`，`responseChars = 0`，`thinkingChars = 979`
  - `think:false`: 约 `6526ms`，`doneReason = "stop"`，`responseChars = 582`

这一阶段最重要的不是绝对时间，而是证明了：

- 同样模型
- 同样 prompt 类别
- 只改 `think:false`

就能从“无可用响应”变成“可用 JSON 响应”。

## 5. 第三阶段：最小代码修正

### 5.1 对所有 Ollama 模型默认 `think:false`

最开始只对 `qwen3*` 做了特判，后来改成平台级策略：

- 所有走 Ollama native adapter 的本地模型，默认都发 `think: false`

实现位置：

- [vercelAiClientFactory.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/adapters/vercelAiClientFactory.js)

这样做的原因很务实：

- 当前 App 的主要本地任务是结构化 JSON 输出
- 这些任务预算本来就紧
- 保守策略应该优先保证“拿到完整可用结果”

### 5.2 对 Ollama 默认设置 `keep_alive`

之后又补了：

- `keep_alive: "15m"`

目的不是提高单次推理速度，而是尽量减少短时间内的模型再次装载成本。

### 5.3 本地 `resume` prompt 压缩

`resume` 比 `prepare` 更复杂，因此也更容易因为输出 schema 太长而被截断。

所以在本地模式下，专门加了 compact prompt 路径：

- 更短的 system prompt
- 更短的 JSON schema
- 限制字段规模
- 本地模式下允许 `presentation: null`

实现位置：

- [threadnoteAIService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/services/threadnoteAIService.js)

这一步的原则是：

- 先保结构化结果完整
- 再考虑表达丰富度

### 5.4 本地上下文裁剪

在应用层也做了本地上下文限制：

- 减少 recall query 数量
- 减少 evidence / notes 数量
- 对本地模型走更小的 evidence package

实现位置：

- [threadnoteApplicationService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/application/services/threadnoteApplicationService.js)

## 6. 第四阶段：把热态与冷态拆开

### 6.1 为什么一定要拆

如果不拆开，benchmark 很容易把两类成本混在一起：

- 模型第一次装载的冷启动成本
- 模型已驻留时的 steady-state 推理成本

这两者在优化策略上完全不同。

### 6.2 冷态基线

产物：

- [ai-real-qwen3.5-9b-cold-prepare.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-real-qwen3.5-9b-cold-prepare.json)
- [ai-real-qwen3.5-9b-cold-resume.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-real-qwen3.5-9b-cold-resume.json)

关键结果：

- `prepare` 冷态约 `7.15s`
- `resume` 冷态约 `7.22s`

更具体地说：

- `prepare cold`
  - `providerCallMS = 7148.17ms`
  - `totalMS = 7148.82ms`
- `resume cold`
  - `providerCallMS = 7223.75ms`
  - `totalMS = 7224.44ms`

### 6.3 热态 5 样本基线

产物：

- [ai-real-qwen3.5-9b-warm5.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-real-qwen3.5-9b-warm5.json)

关键结果：

- `prepare`:
  - `p50` 约 `2.97s`
  - `p95` 约 `3.23s`
- `resume`:
  - `p50` 约 `3.68s`
  - `p95` 约 `4.63s`

更具体地说：

- `prepare warm5`
  - `avg = 3046.71ms`
  - `p50 = 2967.09ms`
  - `p95 = 3229.05ms`
- `resume warm5`
  - `avg = 3774.25ms`
  - `p50 = 3679.24ms`
  - `p95 = 4625.50ms`

这说明冷启动带来的额外成本大约是：

- `prepare`: 多出约 `4.18s`
- `resume`: 多出约 `3.55s`

### 6.4 结论

到这一步，已经可以明确判断：

- 当前最值得优先打掉的是冷启动/模型驻留问题
- 不是先做流式 UI

## 7. 第五阶段：应用级预热与保活

### 7.1 新增运行时预热能力

在 runtime 中加入：

- `prewarmIfLocal()`
- `startLocalKeepWarm()`
- `stopLocalKeepWarm()`

实现位置：

- [aiProviderRuntime.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/aiProviderRuntime.js)

策略：

- 仅对本地 provider 生效
- 预热内部做去重，避免并发 warmup
- keep-warm 定时器低频运行

### 7.2 接入应用启动与 provider 配置变更

接入点：

- 桌面应用启动后后台预热
- provider 保存/切换后后台预热
- 退出前清理 keep-warm timer

相关文件：

- [main.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/desktop/main.js)
- [threadnoteApplicationService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/application/services/threadnoteApplicationService.js)

### 7.3 这一步的实际意义

它解决的是：

- 用户第一次触发 AI 时还没热起来
- 短时间空闲后模型被卸载

它不保证每次都完美消除波动，但能明显降低“第一次一定慢很多”的概率。

## 8. 第六阶段：控制环境污染

### 8.1 发现问题

后面实测发现，即使做了预热，`qwen3.5:9b` 的结果仍然会被其他常驻模型污染。

最典型的污染源是：

- `qwen3.5:35b` 仍然驻留在 Ollama

这会直接影响：

- GPU 内存占用
- 模型切换成本
- benchmark 稳定性

### 8.2 新增单模型隔离 benchmark

在真实 benchmark 中增加可选参数：

- `--isolate-local-model`

实现位置：

- [benchmark-ai-real.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/benchmark-ai-real.mjs)

行为：

- 仅对 `ollama` 生效
- 跑 benchmark 前执行 `ollama ps`
- 自动停掉目标模型之外的其他常驻模型
- 报告中记录：
  - `targetModel`
  - `stoppedModels`

### 8.3 隔离后的干净热态基线

产物：

- [ai-real-qwen3.5-9b-isolated-warm5.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-real-qwen3.5-9b-isolated-warm5.json)

关键结果：

- `prepare`
  - `p50`: `2960.55ms`
  - `p95`: `3166.19ms`
- `resume`
  - `p50`: `3401.05ms`
  - `p95`: `3913.01ms`

更具体地说：

- `prepare isolated warm5`
  - `avg = 2960.72ms`
  - `min = 2763.15ms`
  - `p50 = 2960.55ms`
  - `p95 = 3166.19ms`
- `resume isolated warm5`
  - `avg = 3569.77ms`
  - `min = 3352.43ms`
  - `p50 = 3401.05ms`
  - `p95 = 3913.01ms`

这是当前最可信的一轮本地热态基线。

### 8.4 阶段耗时变化总结

如果只看“最终可用结果”的响应耗时，可以把这一轮优化粗略理解成：

- `prepare`
  - 早期：经常空响应，虽然底层约 `3.1s` 已有 token 产生，但结果不可用
  - 冷态成功基线：约 `7.15s`
  - 热态成功基线：约 `2.96s`
- `resume`
  - 早期：经常空响应，底层约 `5.5s` 已有 token 产生，但结果不可用
  - 冷态成功基线：约 `7.22s`
  - 热态成功基线：约 `3.40s`

这能说明两件事：

- 第一，`think:false` 解决的是“能不能稳定拿到可用结果”
- 第二，预热、保活、单模型隔离解决的是“能不能稳定落在热态耗时区间”

### 8.5 阶段耗时对比表

| 阶段 | 操作 | 结果状态 | 代表耗时 | 备注 |
|---|---|---|---:|---|
| 初始模型选择阶段 | ping `qwen3.5:9b` | 可用 | `16.27s` | 本轮优化起点参考值 |
| 问题尚未定位前 | prepare | 不可用 | `3168ms` | `eval_count > 0`，但 `responseChars = 0` |
| 问题尚未定位前 | resume | 不可用 | `5469ms` | `eval_count > 0`，但 `responseChars = 0` |
| 低层 probe `default` | prepare | 不可用 | `3202ms` | `thinkingChars = 485`，`responseChars = 0` |
| 低层 probe `think:false` | prepare | 可用 | `2941ms` | 可返回结构化 JSON |
| 低层 probe `default` | resume | 不可用 | `5679ms` | `thinkingChars = 979`，`responseChars = 0` |
| 低层 probe `think:false` | resume | 可用 | `6526ms` | 可返回完整 JSON，`doneReason = stop` |
| 冷态成功基线 | prepare | 可用 | `7148.82ms` | 首次冷启动成功结果 |
| 冷态成功基线 | resume | 可用 | `7224.44ms` | 首次冷启动成功结果 |
| 热态 `warm5` | prepare `p50` | 可用 | `2967.09ms` | 未做单模型隔离 |
| 热态 `warm5` | prepare `p95` | 可用 | `3229.05ms` | 未做单模型隔离 |
| 热态 `warm5` | resume `p50` | 可用 | `3679.24ms` | 未做单模型隔离 |
| 热态 `warm5` | resume `p95` | 可用 | `4625.50ms` | 未做单模型隔离 |
| 隔离后热态 `isolated warm5` | prepare `p50` | 可用 | `2960.55ms` | 干净热态，当前最可信 |
| 隔离后热态 `isolated warm5` | prepare `p95` | 可用 | `3166.19ms` | 干净热态，当前最可信 |
| 隔离后热态 `isolated warm5` | resume `p50` | 可用 | `3401.05ms` | 干净热态，当前最可信 |
| 隔离后热态 `isolated warm5` | resume `p95` | 可用 | `3913.01ms` | 干净热态，当前最可信 |

### 8.6 本次改动的时间收益

如果只看本轮最重要的收益，可以直接对比这些数字：

| 对比项 | 优化前 | 优化后 | 收益 |
|---|---:|---:|---:|
| `qwen3.5:9b` 初始 `ping` | `16.27s` | 热态 `prepare p50 = 2.96s` | 约快 `13.31s` |
| `qwen3.5:9b` 初始 `ping` | `16.27s` | 热态 `resume p50 = 3.40s` | 约快 `12.87s` |
| `prepare` 冷态成功基线 | `7.15s` | 隔离后热态 `p50 = 2.96s` | 约快 `4.19s` |
| `resume` 冷态成功基线 | `7.22s` | 隔离后热态 `p50 = 3.40s` | 约快 `3.82s` |

这些收益不应该被理解成“模型本身突然变快了这么多”，而应该诚实地理解成：

- 一部分收益来自禁用默认 thinking，避免输出预算被浪费
- 一部分收益来自 prompt / schema 压缩，让结构化结果更容易在预算内完成
- 一部分收益来自预热、保活和单模型隔离，让 benchmark 落在更接近真实可用的热态区间

因此，本轮最大的实际收益不是某个单点微调，而是把 `qwen3.5:9b` 从“慢且经常不给可用结果”推进到了“热态下可稳定返回结果，且大约在 3 秒级别完成”。

## 9. 为什么暂时没有做流式首包显示

讨论过要不要上“流式首包显示”，但最后没有优先做，原因很明确：

- 在没有隔离环境之前，主要问题不是用户看不到 token，而是模型本身被冷启动和其他驻留模型拖慢
- 热态下 `prepare` 已经稳定在约 `3s`
- 热态下 `resume` 已经稳定在约 `3.4s-3.9s`

所以本轮最划算的优化路径不是：

- 先改 UI 成流式

而是：

- 先让模型稳定处于干净热态
- 再根据新的热态基线，判断流式是否仍有必要

## 10. 引入 `omlx` 的借鉴

本轮没有接入 `omlx`，但把它的思路记下来了，并下载到了：

- [vendor/omlx](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/vendor/omlx)

当前最值得借鉴的不是某个具体 API，而是这几个思想：

- 本地 serving 应该有明确的 benchmark 心智
- prefix reuse / KV reuse 值得长期关注
- 首响应问题要拆成 prefill 与 generation 来看
- 长 prefill 时需要可见反馈，而不是纯黑盒等待

## 11. 新增与修改的主要文件

### 11.1 核心代码

- [vercelAiClientFactory.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/adapters/vercelAiClientFactory.js)
- [aiProviderRuntime.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/aiProviderRuntime.js)
- [threadnoteAIService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/infrastructure/ai/runtime/services/threadnoteAIService.js)
- [threadnoteApplicationService.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/application/services/threadnoteApplicationService.js)
- [main.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/src/desktop/main.js)

### 11.2 benchmark / probe

- [benchmark-ai-real.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/benchmark-ai-real.mjs)
- [benchmark-ai-compare.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/benchmark-ai-compare.mjs)
- [diagnose-ollama-generate.mjs](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/scripts/diagnose-ollama-generate.mjs)

### 11.3 测试

- [aiProviderRuntime.test.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/tests/clean-room/aiProviderRuntime.test.js)
- [threadnoteAIService.test.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/tests/clean-room/threadnoteAIService.test.js)
- [aiRealBenchmark.test.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/tests/clean-room/aiRealBenchmark.test.js)
- [aiCompareBenchmark.test.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/tests/clean-room/aiCompareBenchmark.test.js)
- [ollamaGenerateProbe.test.js](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/tests/clean-room/ollamaGenerateProbe.test.js)

## 12. 实操中的关键经验

### 12.1 不要把“慢”当成唯一问题

本轮最大的偏差是最初把问题想成纯 latency 问题。

真实情况是：

- 一部分问题来自速度
- 更致命的一部分问题来自输出路径和预算分配

### 12.2 Benchmark 一定要能保留失败现场

如果失败样本不带：

- `rawResponsePreview`
- `rawResponseBodySummary`
- `thinkingChars`

这次根本不可能定位到 Ollama thinking 的问题。

### 12.3 本地模型优化不能脱离运行环境

只改 prompt，不控制：

- 模型驻留
- GPU 污染
- 其他常驻模型

得到的结论会很不稳定。

### 12.4 先保“完整可用结果”，再追求表达丰富度

对本地 JSON 任务来说，最重要的不是让返回更花哨，而是：

- 先确保结构完整
- 先确保 parse 稳定
- 先确保可重复

### 12.5 流式不是第一杠杆

当冷启动差值仍有数秒时，先做流式只是改善体感，不会改变根因。

## 13. 当前状态总结

截至本轮结束，`qwen3.5:9b` 在 Threadnote Electron 中已经达到以下状态：

- 本地 JSON 输出稳定，不再被默认 thinking 吃空
- `prepare` 与 `resume` 已有真实 provider 基线
- 热态基线可信，且已可重复
- 运行时具备预热与保活机制
- benchmark 支持隔离其他 Ollama 常驻模型

当前最可信的稳态参考值是：

- `prepare p50 ≈ 2.96s`
- `resume p50 ≈ 3.40s`

这意味着本轮优化已经从“系统经常不返回可用结果”推进到了“本地模型可用，并且可以继续做下一轮精细优化”。

## 14. 下一轮最合理的方向

如果继续做下一轮，本轮经验指向的优先级应该是：

1. 继续在隔离环境下复测应用级自动预热效果，而不是只看脚本热态
2. 若热态仍稳定在 `3s+`，再评估是否需要流式首包显示
3. 如果要再降稳态 latency，优先继续压固定 prompt scaffold 与重复前缀
4. 如果长期做本地模型体验，再评估是否引入更适合 prefix reuse 的本地 serving 方案
