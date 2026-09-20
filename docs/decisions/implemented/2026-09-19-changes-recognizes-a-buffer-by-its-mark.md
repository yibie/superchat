# Decision: `/changes` 靠 Buffer 身上的标记认"工具改过的那一个"

Status: implemented（2026-09-19；同日 accepted——用户在三个选项里选定"给 Buffer 贴标签"，并决定保留 `/changes` 并把它修好）

## 一句话

结构化工具改一个 Buffer 时，在这个 Buffer 自己身上留一个看不见的标记（哪个 Session 的哪个 Run 改过它）。`/changes` 和 Run 结束提醒认的是这个标记，不再靠"当时的名字、当时的文件"回头去找。

## 为什么

[Buffer-first mutation](2026-09-18-buffer-first-mutation.md) 第五节规定的定位顺序是"先按记录的名字找 live buffer，找不到再按记录的文件找；不为此另存 Buffer 身份"。2026-09-19 的开发回顾用真实执行管线复现了它的缺口：工具改过的 Buffer 被改名之后，另一个无关的 Buffer 占用了旧名字，`/changes` 列出了那个无关的 Buffer，真正的目标反而漏掉（`.scratch/development-audit-20260919/issues/01-…`）。

这不是实现偏离契约，是契约没有覆盖"名字和文件指向不同 Buffer"。把顺序倒成文件优先也不行：`set-visited-file-name`、关闭后重开同样会让文件指向别的 Buffer。名字和文件都只是某一时刻的描述，不是身份；Emacs 里唯一的身份是 Buffer 对象本身。

影响限于候选列表和 Run 结束提醒——可能把用户引到无关 Buffer 的 diff／保存／还原上；没有自动保存或数据丢失。

## 契约

1. **标记**：结构化修改工具（`edit_buffer`、`append_to_buffer`、`replace_buffer`、`workspace_write` 的 region 目标）在真正改了 Buffer 之后，给被改的 **base buffer** 记上 `(SESSION-ID . RUN-ID)`。被打回的修改（见"坏的 Lisp 修改由工具打回"）没有改 Buffer，也不留标记。标记与写入 `tool.completed` 的 `:buffer`／`:buffer-file` 在同一处产生，二者不会不一致。
2. **标记属于那个 Buffer 对象**：改名、`set-visited-file-name`、切换 major mode 之后仍在；Buffer 被关闭，标记随之消失；另一个 Buffer 用了同样的名字或访问同一个文件，没有标记；克隆出来的 Buffer 不继承（标记里记着它属于哪个 Buffer 对象，对不上就无效）。
3. **候选的推导**改为：

   ```text
   Tape 上该 Run（或该 Session）及其委派出去的 Run 里，带 :buffer 的 tool.completed 事件 → 一组 (Session, Run)
       ∩
   此刻 live、buffer-modified-p 为真、且身上的标记含其中某一对的 Buffer
   ```

   Tape 仍然是"哪些 Run 做过结构化修改"的依据（委派的子 Session 照旧算在发起它的 Run 名下）；标记只回答"是哪一个 Buffer"。`:buffer`、`:buffer-file` 继续写进 Tape，作为当时的证据，不再用于定位。列出来的仍是 Buffer 此刻的名字、此刻的文件、此刻是否有未保存修改。
4. **不是账本**：标记不写 Tape、不落盘、不记录改了什么；不监视 Emacs；没有全局登记表——它只是 Buffer 上的一个局部变量。原契约"不为此另存 Buffer 身份"一句由本决定取代；"不创造新的领域对象（mutation ledger、待定变更、写入证据、文件批准状态）"不变。
5. **随之改变的行为**（用户已知悉）：Buffer 关掉再重开——即使同名、同文件、又被用户改过——不再列出，因为工具的修改已经随旧 Buffer 消失。Emacs 重启后没有标记，也没有未保存的 Buffer，二者一致。
6. **不变的局限**：`eval_elisp` 改的内容不归属；列表里可能包含用户自己在同一个 Buffer 上的后续修改（用户看的是 diff）；Buffer 保存或还原之后标记还在，用户之后再改它，它会重新出现在列表里。

## 验收

在 `scripts/m7-buffer-first-regression.el` 里，全部经真实执行管线：

1. 回顾里的场景：目标改名、无关 Buffer 占用旧名字并有未保存修改 → 只列出真正的目标。
2. Buffer 关掉后重开（同名同文件、用户又改过）→ 不列出。
3. 没有文件的 Buffer：工具改过的被关掉，用户新建同名 Buffer → 不列出。
4. 切换 major mode 之后仍然列出（标记不被 `kill-all-local-variables` 清掉）。
5. 克隆出来的 Buffer 不列出。
6. 被打回的修改不留标记。
7. 已有的 `/changes` 测试（indirect buffer、改名加 `set-visited-file-name`、保存后离开列表、还原）不变并通过。
8. 委派：子 Session 里的 Run 改的 Buffer，算在发起它的 Run 和 Session 名下；各 Run 只列自己标记过的，别的 Session 的不列。（写本契约时我以为已有测试覆盖委派，核对后发现没有——这一支自第一片起从未被测过——所以补成新测试。）

1、2、3 在实现前必须是红的；4、5、6 是守护，各有变异体。其余套件、canonical smoke、Kernel 检查不变。

## 交付记录（2026-09-19）

- 实现：`superchat-changes.el` 增加 buffer-local、`permanent-local` 的标记 `(BUFFER . ((SESSION . RUN) …))`，以及 `superchat-changes-mark-buffer`；候选改为"Tape 上做过结构化修改的 (Session, Run)，含委派出去的 ∩ 此刻 live、未保存、带其中某一对标记的 Buffer"，不再按名字或文件查找。`superchat-tools.el` 把"记录修改目标"和"打标记"收进同一个函数 `superchat-tools--note-mutation`，两处调用点（三个编辑工具的证据行、`workspace_write` 的 region 目标）共用，二者不可能不一致；被打回的修改两样都不做。
- 回归（`scripts/m7-buffer-first-regression.el`，21 个，新增 5 个）：实现前红的两个——旧名字被无关 Buffer 占用时列错并漏掉真目标（回顾的场景）；关掉重开的同名同文件 Buffer、以及重建的同名无文件 Buffer 仍被列出。守护三个——major mode 切换后仍列出、克隆不继承、被打回的修改不留标记；各 Run 只列自己的、别的 Session 的不列；委派的子 Run 的修改算在发起它的 Run 名下。
- 变异体（各自只让对应测试变红）：标记不是 permanent-local；不核对标记属于哪个 Buffer 对象；被打回的修改仍打标记；忘掉委派的 Run；不管哪个 Run 打的标记一律列出。最后一个第一轮没被咬住——当时没有任何测试区分"这个 Run 的 Buffer"和"别的 Run 的 Buffer"——补了测试后变红。
- 回顾自带的复现（`.scratch/development-audit-20260919/changes-boundary-repro.el`）修复后通过。验收脚本用同一个函数找"该给用户审阅哪个 Buffer"：校准 3 个场景 × 2 个入口 6/6 符合预期。
- 其余全部套件、canonical smoke（无基线变化）、Kernel 检查、加载通过；编译警告与基线一致。
