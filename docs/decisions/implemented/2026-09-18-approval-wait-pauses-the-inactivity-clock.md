# Decision: 批准提问开着的时候，"模型无进展"计时不走

Status: implemented（2026-09-18；同日 accepted——用户在 `m7-attended-chat-01` 作废后直接给出语义和回归要求）

## 一句话

Superchat 主动停下来等人批准的那段时间，不算"模型没有进展"。提问打开时，这一次 provider attempt 的无进展计时停表；人回答之后，如果 Run 还有效，从剩余的窗口接着走。

## 为什么

`superchat-response-timeout`（默认 120 秒，工具 attempt 再乘 1.5 = 180 秒）是"模型卡住检测"：一次 attempt 在这么久里没有产生任何新内容，就判超时。它从 attempt 发出起一直走到 attempt 结束，**中间的工具执行也算在内**，所以批准提问期间它照常走。

M6 契约写的是"人在决定期间必须续期传输等待"，实现却是在**回答之后**才加 30 秒（`superchat-approval--decide` 的收尾）。只要人读批准内容超过 180 秒，Run 就在提问还开着的时候被自己杀掉；之后的回答被记下，却没有 Run 可以执行。

证据：真实运行 `m7-attended-chat-01`（`eval_elisp` 的提问开了 467 秒，第 181 秒 `[Response timeout: no progress for 181s …]`，Tape 顺序 `approval.requested → run.failed → approval.decided`）；零模型调用的复现 `m7-measure-idle-during-approval-01`（超时 8 秒、人等 25 秒，顺序相同）。这不是实验配置问题，是普通用户认真读一段代码就会遇到的交互缺陷。

## 契约

1. **一个事实，不新增机制**：Run 处于"等人"（批准提问）时，该 attempt 的无进展计时不运行。`ask_user` 的等待早已如此（可信输入协议会停掉同一个计时器）；批准等待接入同一个 transport timer owner，不另造第四套逻辑。
2. **停表**：批准链真正要去问回答者之前（`never` 策略不问人，也就不停表），取出该 Run 的 timer owner，停掉它当前的无进展计时器，记下剩余时间。
3. **续表**：回答者返回（允许、拒绝、quit、抛错，任何一种）之后，只有当 owner 的计时器槽里**仍是被停掉的那一个**时才重新启动它，时间为"现在 + 剩余"；随后照旧做 M6 已有的那次续期（+30 秒）。Run 在等待期间被取消、attempt 已经结束或换代时，槽已被清空或换人，不会被重新启动——晚到的回答不能让任何计时器或 Run 复活。
4. **不改的**：
   - M6 的取消规则：批准等待期间 Run 被 cancel，晚到的 yes 不执行工具、不发下一次请求，Run 仍以 `audit-incomplete` 封口。
   - 没有人在等的时候，provider 沉默照旧按原规则超时——看门狗没有被关掉。
   - 累计传输上限 `superchat-response-timeout-hard-limit`（600 秒）不在本决定内：它的语义是"一次工具会话总共能占多久"，不是"模型无进展"。人的等待是否计入它，是已登记的另一个问题（`.scratch/m5-config-acceptance/issues/01-…`），其 docstring 目前如实写着"ordinary tool-boundary pauses continue to consume it"。
   - 回答者不是人（插件 listener 自动回答）时同样停表、续表；瞬间完成，没有可观察的差别。
5. **统计**：人的等待时间可以单独统计（验收脚本已在记），但不消耗任何以"模型没有产生进展"为语义的计时。

## 验收

新回归 `scripts/approval-wait-regression.el`（真实执行管线 + 脚本模型，无进展超时设得很短、批准等待是它的数倍）：

1. 等待后**允许**：工具正常执行，Run `completed`，没有超时文字。
2. 等待后**拒绝**：模型收到正常的拒绝结果（不是超时），Run `completed`。
3. 等待期间 Run 被 **cancel**，随后回答 yes：工具不执行，没有下一次 provider 请求，Run `audit-incomplete`（M6 规则不变）。
4. **防过修**：没有任何批准等待、provider 沉默：照旧超时，答复里有 `no progress`。
5. **停表与续表可观察**：提问打开期间该 Run 的无进展计时器不在 Emacs 的计时器队列里；决定之后、同一 attempt 内它重新在队列里，且剩余时间大于零。

1、2、5 在修复前必须是红的，且红在这个缺口上；3、4 是守护测试，修复前后都绿，并各有一个变异体证明它咬得住。M5 execution、M6 permissions 及其余套件、canonical smoke、Kernel 检查不变。

## 交付记录（2026-09-18）

- 实现：`superchat-llm.el` 新增一对函数 `superchat-llm--transport-timer-owner-suspend-wait`／`-resume-wait`（把 owner 的无进展计时器移出队列并记下剩余；只在槽里仍是那一个计时器时按剩余时间放回）。`superchat-approval--decide` 在真正去问回答者之前停表，收尾时先续表、再做 M6 原有的那次 +30 秒续期。没有新增状态、事件或用户选项。
- 回归 `scripts/approval-wait-regression.el`，6 个测试：验收 1–5，外加一个函数级测试（第 3 条的"槽已清空／已换代则不放回"；经由被取消的 Run 走不到这一支，因为被取消的 Run 的 owner 已不可得——那是第一道防线，槽检查是第二道）。
- 修复前：1、2、5 红，且红成与真实运行相同的样子（Run `audit-incomplete`；提问期间计时器仍在队列里、还剩 0.97 秒）；3、4 绿。测试自身的两处错误在看到红之前已排除（产品对"超时 × 1.5"取整，0.4 秒会变成 0；最终答复文字要从 View 读）。
- 变异体（各自只让对应测试变红）：不停表（原缺陷）→ 1、2、5；停表后不续表 → 5；续表不看槽 → 函数级测试；把看门狗整体关掉 → 4。一个等价变异体如实记录：只去掉续表里的 `timer-activate` 不改变行为，因为紧随其后的 +30 秒续期本来就会重新入队。
- 端到端（交互式 Emacs，伪终端扮演人，零模型调用）：修复前 `m7-measure-idle-during-approval-01`（超时 8 秒、人等 25 秒）Run 在提问期间失败；修复后同样的设置 `…-02-fixed` 两个 Run 都 `completed`，Tape 顺序 `approval.requested → approval.decided → tool.completed → run.completed`。
- 其余套件、canonical smoke、Kernel 检查、加载全部通过，编译警告与基线一致。
- M5 验收脚本不再挪开这个计时器。

修复后，M5 验收脚本不再在测试 Emacs 里挪开这个计时器；`CA_MEASURE_IDLE` 只保留为复现手段。
