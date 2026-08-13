# 面向 Emacs：一次第一性原理的讨论

> 语境：superchat 定位为「Emacs 的 AI OS 层」（docs/design/architecture-positioning.md）。
> 本轮实机验证发现：模型对路径式文件工具（read_file / write_file）表现为拒用与误用，
> 对 Emacs 对象工具（buffer 全家、workspace_info）用得好。已决策逐步废弃路径式文件 I/O，
> 收敛到 Emacs 对象面。本文讨论这个决策背后的两个概念问题。

---

## 0. 结论先行

**一、何谓「面向 Emacs」**

面向 Emacs 不是「在 Emacs 里跑 agent」，而是让 agent 的**操作对象**从"按位置寻址的惰性字节流"换成"**有身份、有类型、可自省的活运行时对象**"。这个替换改变了四件
事，每一件都是架构性的，不是措辞性的：

1. **寻址由位置变为身份**——buffer 有名字与生命周期，路径只有位置；
2. **权威副本从磁盘搬到进程内**——agent 改的是活对象，落盘由人决定；
3. **工具从"syscall 包装的集合"变成"同一对象模型上的方法"**——语义自足，参数短，不需要外部上下文补全；
4. **政策层获得了 resource 维度**——`file / buffer / workspace / process / emacs / tape / language-server`（superchat-tools.el:87-221）。纯路径世界里，你只能分「
读」和「写」。

实机现象（模型拒用路径工具、用好 buffer 工具）的根因不是模型偏好，是**路径工具的语义在模型的世界模型里是欠定的**：相对哪个 cwd、会不会覆盖用户未保存的改动、
offset 该取多少——这些都要模型自己猜。buffer 工具是自足的：buffer 名就是全部上下文。

**二、Emacs 为何能担当**

因为 Emacs 是唯一存活至今、被大众使用的 **Lisp Machine 式单一可编程地址空间**：反射（自文档化）、统一对象模型（buffer 即世界）、一等 process 对象、可后加的行为
改造机制（advice/hook/动态绑定）。四十年里 Emacs 社区一直在做的事——把 git、LSP、邮件、大纲、远程文件翻译成 Emacs 对象——恰好就是"为 agent 准备适配层"，只是当时是
为人做的。

**诚实的反面**：这个选择放弃了宿主 OS 的整个能力面（浏览器、桌面、容器、真并行、进程隔离），也离开了当前模型训练分布最熟的工具形状。换回来的是可编程性、可审计性
、工具一致性、人机同屏。而 `eval-elisp` 让整件事必须被精确表述：

> **边界不是能力边界，是宣告与可见性的边界。superchat 提供的是可归因性（attributability），不是不可能性（impossibility）。**

由此得到一条对设计有约束力的推论：**收敛工具面（废弃路径式 I/O）的目的是消歧，不是限权**。误读成安全动作，会带来错误的安全信心。

---

# 一、何谓「面向 Emacs」

## 1.1 三种寻址模型的本质差异

|          | 面向文件系统        | 面向操作系统      | 面向 Emacs                                                |
|----------|---------------------|-------------------|-----------------------------------------------------------|
| 地址     | 路径                | pid / fd / 端口   | 对象名（buffer、symbol、marker、window、process、org id） |
| 对象性质 | 惰性字节序列        | 黑箱进程          | 有类型、有行为、可自省的活对象                            |
| 操作形式 | read/write 全量替换 | exec + stdout     | 对象上的方法                                              |
| 状态所在 | 磁盘快照            | 进程内，不可见    | 进程内，可见且可查询                                      |
| 权威副本 | 磁盘                | 无（一次性）      | buffer                                                    |
| 失败方式 | 静默覆盖            | 退出码 + 一坨文本 | Lisp error + 可回滚的 undo 记录                           |

差异不在 API 风格，在四条本质：

### (a) 身份 vs 位置

buffer 有身份。文件被移动、被重命名、还没落盘、甚至**根本不对应任何文件**（`*scratch*`、eshell、magit status、dired、compilation、superchat 自己的会话
buffer），buffer 依然是那个 buffer。路径只有位置，而位置随时失效，同一位置在不同时刻是不同内容。

对 agent 的意义：世界模型建在**位置**上，每次读都必须重新怀疑（"我上一步读的还作数吗"）；建在**身份**上，它可以持有一个稳定引用，多轮对话里指的是同一个东西。这
直接影响多步任务的可靠性。

（诚实补充：Emacs 的 buffer 身份不是无条件稳固的——同名文件会变成 `foo.el<2>`，临时 buffer 会被 kill。身份优势需要 superchat 层面加固，见 2.3。）

### (b) 活状态 vs 快照

磁盘上的 `.el` 文件不知道自己有没有被 load。Emacs 里的 symbol 知道自己现在的值、现在的定义、来自哪个文件、有没有被 advice 包过。

这就是为什么 `describe-function` / `describe-variable` 在 superchat 里是一等工具，并被单独归类为 `local-read emacs
introspection`（superchat-tools.el:202-205）：它回答的是「**现在**的实际行为」，路径工具只能回答「磁盘上写着什么」。

对 agent 而言这是**事实**与**文本证据**的差别。文本证据需要再验证，事实可以据以行动。

### (c) 权威副本的位置

这条最容易被低估，但它决定了人机协作的形态。

- **面向文件**：磁盘是权威。agent 写盘即生效。用户在编辑器里看到的是过期内容，还会撞上 "file changed on disk, revert?"。用户想撤销 agent 的改动，得靠 git，不能
靠 `C-/`——因为改动从来没进过 undo 历史。
- **面向 Emacs**：buffer 是权威。代码证据：`superchat-tool-edit-buffer` / `superchat-tool-replace-buffer`（superchat-tools.el:606-629）**只改 buffer，从不
save**。改动进入 `buffer-undo-list`，字面出现在用户正在看的屏幕上，major-mode hook 与 font-lock 全部生效，最后由**人按 `C-x C-s`** 落盘。

这一条把「审批」从弹窗搬进了编辑器的原生工作流：审批对象变成"你本来就在看的东西"。同时它复用了 Emacs 四十年积累的**防人类手滑的安全网**（undo、modified flag、
revert 检测、read-only、narrowing）来防模型手滑——而且用户已经会用这些。

顺带说明 `EditBuffer` 的一个设计细节：它要求 `old_string` 在 buffer 内**唯一匹配**，命中 0 次或 >1 次都报错（superchat-tools.el:612-621）。这是把"歧义"变成"显式
失败"，而不是让模型在多个候选里瞎猜。路径式 `write-file` 没有这层保护——它的语义就是全量覆盖，歧义被静默吞掉。

### (d) 操作语义的一致性

看一眼当前的路径工具族：`read-file` / `write-file` / `append-file` / `quick-write` / `list-files` / `find-files` / `search-text` / `make_directory`——八个工具、
三种写语义（覆盖 / 追加 / "小文件专用"）、两种搜索语义（glob / 正则）、路径相对谁不确定（`superchat-tools--base-dir` 依赖一个可被 `superchat-send-region` 改写的
变量，superchat-tools.el:60-69）。

Emacs 对象面只有一套流程：**定位 buffer → 在 buffer 里做编辑 → 由 Emacs 落盘**。

这解释了实机现象。模型不是"讨厌路径工具"，是这八个工具里每一个都要求它补全环境里没有的信息。而 `read_buffer("superchat-tools.el")` 是自足的。

### (e) 一个被忽视的重叠：`#` 引用与 read-file

superchat 里，「把文件内容变成上下文」有两条路：

1. 用户用 `#` 选文件（`superchat--smart-hash`，superchat.el:1639-1657），范围由**人**控制；
2. 模型调 `read-file` 带 offset/limit，范围由**模型盲猜**。

两条路语义重叠，是模型行为不稳定的直接来源——模型必须先决定"该我读还是等用户给"。**收敛不是减法，是消歧**：把这件事的唯一入口定死在人这一侧（`#` 注入）与对象这一
侧（buffer / workspace），矛盾就消失了。

## 1.2 三个任务，两种设计下的 agent 行为

### 任务 A：改一个函数

*给 `superchat-tool-edit-buffer` 加一个大小写敏感选项。*

**面向文件的 agent：**
```
search-text "superchat-tool-edit-buffer"     → 一堆文本命中（含注释、测试、docstring）
read-file superchat-tools.el offset:? limit:? → 猜范围，猜错就再读一次，或整文件塞进上下文
（在上下文里重建整个函数文本）
write-file superchat-tools.el <整个文件>      → 风险：覆盖用户未保存改动 / 改坏文件其他部分
```
后果链：用户的 Emacs 弹出 "file changed on disk"；用户 `C-/` 撤销不了（undo 历史里没有）；如果用户此刻 buffer 里有未保存修改，二者必居其一被丢弃。

**面向 Emacs 的 agent：**
```
（用户 # 注入 或 workspace/buffer 定位活 buffer）
read_buffer "superchat-tools.el"
EditBuffer buffer_name:"superchat-tools.el" old_string:<唯一片段> new_string:<新片段>
   → 唯一匹配约束保证不改错地方
   → 改动出现在用户屏幕上，C-/ 可撤销，diff 可看
eval-elisp (defun superchat-tool-edit-buffer ...) 或 M-x eval-defun  → 立刻生效，可当场验证
（用户 C-x C-s）
```

**实质差别**：面向文件的 agent 在**离线重建**一个文件；面向 Emacs 的 agent 在**就地修改一个活对象**，且每一步都被 Emacs 现有的安全网接住。前者的每一次写都是一次
不可见的赌博，后者的每一次写都是一次可见的提案。

### 任务 B：查一个引用

*谁调用了 `superchat-authority-resolve-effects`？*

**面向文件**：`search-text` 正则 → 得到字符串命中，包括注释、docstring、同名不同义、以及**宏展开后才出现的调用看不见**。agent 拿到的是一堆需要它自己再判断的线索
。

**面向 Emacs**：
- `lsp-references`（eglot 后端，superchat-tools.el:947-975）在语言服务器的**语义索引**上回答；
- 对 elisp 还可以 `describe-function` 看它**现在**的定义与被 advice 的情况；
- 极端情况 `eval-elisp` 直接问运行时。

**实质差别**：grep 回答的是「这个字符串出现在哪」，Emacs 回答的是「程序现在是什么样」。对 agent，前者是**待验证线索**，后者是**可行动事实**。多步任务里，线索会
累积不确定性，事实不会。

### 任务 C：跑一个测试

**面向 OS**：`shell-command "make test"` → 一坨 stdout → agent 用正则从文本里挖失败信息 → 失败位置是文本里的 `"file:line"` 字符串 → 还要转回路径去读文件。

**面向 Emacs**：`eshell-command` 或 `compile` → 输出进 compilation buffer，`compilation-mode` **已经**把 `file:line` 解析成可跳转的 marker；ERT 的结果是 Lisp 对
象不是文本。agent 可以 `read_buffer` 拿已解析的输出，也可以 `eval-elisp` 直接问 ERT 的结果对象；用户可以直接在同一个 buffer 里 `RET` 跳过去。

**实质差别**：Unix 的接口是 byte stream + exit code——这是它的伟大之处，也是它的天花板（"一切皆文本流"）。Emacs 的接口是**对象**，而且是**已经为人类阅读做过一次
解析**的对象。agent 恰好是"像人一样阅读"的消费者，所以它免费继承了这层解析工作。四十年的 `compilation-error-regexp-alist` 白送给 agent。

## 1.3 为什么这是架构实质，不是修辞

因为它决定了三件后续不可改的事：

1. **工具面的形状**：工具是「对象上的方法」还是「syscall 的一组包装」。这决定了工具数量的增长曲线——对象面上加能力是加方法（模型上手成本≈0），syscall 面上加能力
是加一个语义独立的新工具（模型要重新学）。
2. **政策面的粒度**：Authority 的 effect 描述符带 `:resource`（superchat-tools.el:71-74），取值遍布 `file / buffer / workspace / process / emacs / tape / team /
database / language-server / tool-definition`。**这套分类只有在有对象模型时才存在**。纯路径世界里，你只有「读」和「写」两档，审批记录写下来是 `write
/Users/.../x.el 12kB`；对象世界里是 `local-write buffer superchat-tools.el`——后者的信息量高一个量级，因为它说明了"改的是什么东西"，而不只是"往哪儿倒了多少字节"
。
3. **人机共享性**：人和 agent 操作同一个对象、共享同一个 undo 历史、看同一块屏幕。这是零成本的协作协议。

**反证法**：如果这只是修辞，你应该能把 buffer 工具原样换成路径工具而不损失什么。实际换掉之后损失的是：审批失去可视对象、undo 断链、未保存状态不可见、
major-mode/LSP 的语义层拿不到、Authority 的 resource 维度塌缩成 `file`。这些损失是结构性的，加多少 prompt 都补不回来。

---

# 二、Emacs 为何能担当这个职责

## 2.1 历史与设计的论据

### (a) 自文档化：为人设计的机制，恰好是 agent 需要的机制

Stallman 的 AI Memo 519（1981）标题里三个形容词都不是装饰：*Extensible, Customizable, **Self-Documenting** Display Editor*。（更早的源头是 1976 年 Steele 起草、
Stallman 完成的那套统一 TECO 宏集；而 Bernard Greenberg 的 *Multics Emacs: The History, Design and Implementation*（1979）第一次系统论证了"用 Lisp 作扩展语言"不
是实现细节而是设计本质。）

自文档化的技术含义：**运行时自带一个关于自己的、可查询的知识库**。`C-h f`、`C-h v`、`apropos`、`describe-mode` 不是文档系统，是**反射接口**。

对 agent 这是决定性的。agent 不需要外挂文档 RAG，`describe-function` 就能拿到"这个函数**现在**是什么"。而且注意这里的历史反讽：自文档化在 1970s 是为了解决"如何
让用户学会一个巨大的、可被自己修改的系统"；2020s 的 agent 面对的是**同一个问题**——面对一个不熟悉的、超出上下文窗口的巨大系统，通过提问逐步建立局部模型。四十年前
的答案直接可用。

### (b) Lisp Machine 传统：单一可编程地址空间

Lisp Machine 的核心特征——单一地址空间、一切对象可达、系统本身用同一门语言写、用户代码与系统代码没有二等公民之分——Emacs 是这个传统在通用 OS 上唯一大众化存活下来
的实现。

对 agent runtime，「单一可编程地址空间」的含义是：**没有 IPC，没有序列化边界，没有 API 表面**。任何一个能力，只要 Emacs 生态里有人实现过，agent 通过一次 funcall
就能用——**不需要有人先把它包装成 MCP 工具**。

这是工具面**长尾免费**。代价见 2.4。

### (c) buffer 即世界：工具一致性的物理基础

Emacs 的核心抽象基本只有一个：buffer（携带 point / mark / marker / text property / overlay / buffer-local variables / major-mode）。

- 文件是 buffer 的一种**来源**，不是 buffer 的定义；
- 进程输出是 buffer；目录是 buffer（dired）；版本控制状态是 buffer（magit）；调试器是 buffer；邮件是 buffer；UI 也是 buffer（superchat 自己的 render / genui /
tape-view 就建在这上面）。

对 agent 的价值：**只学一种对象，就能操作全部子系统**。

这不是设计纪律的产物，是宿主的物理事实。对比面向 OS 的 agent：要分别学文件 API、进程 API、浏览器 DOM、剪贴板、窗口管理器、通知中心——每套语义都不同，每套都是新的
失败模式。

### (d) process 模型：子进程是一等对象

Emacs 的子进程不是 fork/exec 之后的黑箱，是一个 **process 对象**，带 filter 与 sentinel，输出流进 buffer。长任务的中间状态是可观察、可中断、可归因的。

对 agent：「跑一个东西然后看结果」天然是**事件流**而不是阻塞等 stdout。superchat 的 subagent async placeholder（superchat-tools.el:673-689）能做到"跑着不卡
Emacs、结果回填到原位"，靠的就是这个模型。

### (e) 动态绑定 + advice + hook：政策层可以后加

Emacs 允许在**不改源码**的前提下改变系统行为，而且这种改变是可枚举（`advice-mapc`）、可撤销（`advice-remove`）的。宿主本身就是**为被改造而设计**的。

这是 superchat 的横切缝能存在的原因：Authority 不需要每个工具配合，它包在外面；`superchat-register-tool` 强制声明 `:effects`，并在注释里写死了原则——**"Authority
never learns tool names"**（superchat-tools.el:1642）。政策层认识的是效果词汇表，不是工具清单。这种解耦在一个 API 边界固定的宿主里做不出来。

### (f) org / magit / eglot：社区四十年做的其实是 agent 适配层

这些包的共同结构特征是：**把外部系统翻译成 Emacs 对象**。

- git → magit 的 section 对象与状态 buffer
- LSP → eglot 的 Lisp 数据结构
- 大纲 / 任务 / 文献 / 表格 → org 的树、property、babel
- 远程文件系统 → Tramp（远程与本地**同形**——这条对 agent 尤其划算：一套工具面自动获得远程能力）

也就是说：superchat 接手的是**已经完成的翻译工作**。这解释了为什么"新能力优先建在 Emacs 原生域"（定位文档 §3）不是保守，是套利——这些域的适配层已经有人写完并用了
二十年。

### (g) "Emacs 是操作系统，只是缺个好编辑器"——这句话的技术含义

不是玩笑。Emacs 具备 OS 的要素：

- **调度**：timer、process、异步回调
- **内存中的对象空间**：buffer、symbol、obarray
- **设备抽象**：Tramp 让远程与本地同形；buffer 让文件、进程、UI 同形
- **shell**：`M-x` 与 `eval` 就是它的命令行

它缺的是：**保护边界**（无进程隔离，一个死循环卡死全部）与**抢占式调度**。

对 agent runtime，这句话可以精确改写为：

> **Emacs 的能力面完备，隔离面缺席。**

这一条既是它能担当这个职责的原因（能力面完备 = agent 什么都够得着），也是 2.4 那部分诚实交代的全部来源（隔离面缺席 = 任何沙箱承诺都是假的）。

## 2.2 作为 agent runtime 的关键特质（不是编辑器特质）

按重要性排：

1. **反射**：runtime 能回答关于自己的问题（自文档 + eval）。这是"agent 能自己搞清楚状况"的前提。
2. **同址共享**：人和 agent 在同一进程、同一对象上工作，无需同步协议。屏幕就是共享状态的显示器。这是 Emacs 相对任何 sidecar 架构的结构性优势。
3. **一切可命名**：命名对象让工具参数短、稳定、语义自足。这是 1.1(d) 那个实机现象的机理。
4. **人类安全网可复用**：undo / modified flag / revert 检测 / read-only / narrowing——防人类手滑的，现在防模型手滑，且用户已经会用。
5. **文本带 property**：Emacs 的文本是"带元数据的文本"。agent 的输出能被 render 成可交互对象，不需要另立 GUI 技术栈。superchat 的 `render_ui` 被归类为
`local-write buffer`（superchat-tools.el:134-142）——**UI 就是 buffer 写入**，这在别的宿主里是不可想象的简化。
6. **审计层与操作层同语言**：Tape 是 Lisp 数据，Closure 是纯函数判定器。不存在"日志是另一个系统"的鸿沟——审计可以用与被审计者相同的手段被检查、被重放。

## 2.3 历史包袱（诚实清单）

- **单线程 + 无抢占**：agent 的长操作卡死 UI。这是最硬的包袱，也是 superchat 大量异步设计与 subagent async 的直接成因。
- **无隔离、无能力安全**：任何 Lisp 代码都能改任何东西，**包括 Authority 自己**。政策是**约定**，不是**机制**。这一点在 2.4 展开。
- **全局可变状态与动态作用域**：advice / hook 让行为难以静态推理。agent 读源码得到的答案未必是运行时答案——这既是包袱，也正好是自文档化 + `describe-function` 存
在的理由。
- **API 的历史沉积**：同一件事有五种做法，且模型的先验知识可能停留在十年前的写法（`cl-lib` 之前、`seq` 之前、`lexical-binding` 之前）。这是一个真实的、会持续存
在的模型误用来源。
- **交互式设计的错误处理**：许多命令面向人设计，失败方式是 `message` 而非可捕获的 error。工具化时必须包一层——这就是为什么 superchat 的工具函数普遍返回字符串错误
而不是抛出（如 superchat-tools.el:616-619 返回 `"Error: ..."`）。
- **buffer 身份并非绝对稳固**：同名文件 `<2>`、临时 buffer 被 kill、`revert-buffer` 后 marker 失效。1.1(a) 的身份优势需要在 superchat 层加固，不能当成免费假设。

值得区分的是：前两条是**Emacs 的本质约束**（改不了，只能设计绕行），后四条是**历史沉积**（可以在 superchat 层抹平）。

## 2.4 得失，与 eval-elisp 标出的那条线

### 损失了什么

**宿主 OS 的整个能力面**，且是设计上排除（定位文档 §3）：浏览器/桌面自动化、GUI 应用控制、容器与虚拟机、系统级调度、跨机编排、真正的并行执行。

含义要说透：**superchat 永远不会是"替你操作电脑"的 agent**。这不是能力缺口，是身份定义。一个想要"帮我订机票"的用户，来错了地方。

还损失了一样更微妙的东西：**与主流 agent 工具生态同形的便利**。市面上的标准工具面（Read / Write / Bash / Glob / Grep）是路径式的，模型的训练分布偏向它。收敛到
Emacs 对象面等于**主动离开模型的舒适区**。

——但本轮实机验证给出了相反的经验事实：模型在路径工具上表现**更差**。这说明**"贴近训练分布"抵不过"语义自足"**。这是本轮最有信息量的一条经验，值得单独记住，因为它
反直觉：模型在陌生但自洽的工具面上，比在熟悉但欠定的工具面上表现更好。

### 得到了什么

- **可编程性**：新能力 = 一段 elisp。不是一个新进程、新协议、新权限模型。扩展成本低一个数量级。
- **可审计性**：所有效果过 Authority 的 effect 分类落到 Tape。关键在于——**因为对象有类型，审计条目才有意义**。`local-write buffer superchat-tools.el` 与 `write
/Users/.../superchat-tools.el 12kB` 的差别，就是"可审查"与"可存档"的差别。
- **工具一致性**：同一对象模型下，工具是方法而非 API 集合。模型只需一套心智模型，新工具的边际学习成本趋近于零。
- **人机同屏**：审批与观察不需要额外 UI。改动发生在用户正在看的地方——这是本质上最便宜的透明度。

### eval-elisp 意味着边界在哪里

这是全文最需要精确的一段。

`superchat-tool-eval-elisp`（superchat-tools.el:664-671）的实现就是 `(eval (read expression) t)`，docstring 自己写着 "The expression has full access to the
Emacs process"。结论直白：

**1）工具面在能力意义上是完备的。**
任何 buffer 工具能做的，eval-elisp 都能做；而且远不止——它能 `call-process` 起外部进程、能改 `superchat-authority-*` 的变量、能 `kill-emacs`。

**2）所以边界不是能力边界。**
把 eval-elisp 放进工具面，就等于承认 agent 在 Emacs 内部拥有全权。任何形式的"我们限制了 agent 能做什么"，在 eval-elisp 面前都是假陈述。这一点必须对用户明说。

**3）边界是"宣告"与"可见性"的边界。**
Authority 把 `shell-command` / `eshell-command` / `eval-elisp` 三者一起解析为 `process-exec`（superchat-tools.el:143-150），策略层对 `process-exec` 走审批
（superchat-authority.el:446-480 的 builtin policy，以及 527-540 的 session 权限模式收紧）。其余对象工具是 `local-read` / `local-write`，门槛更低。

也就是说：**线画在「这次操作是否宣告自己要越界」上，不画在「它是否有能力越界」上**。

**4）用一句更准的话替换"界面承诺，不是沙箱承诺"：**

> superchat 保证的是**可归因性（attributability）**，不是**不可能性（impossibility）**。
> 它保证越界动作会以 `process-exec` 的身份出现在 Tape 上、经过审批点；它不保证越界不可能。

**5）这个选择是理性的，不是妥协。**
Emacs 本来就没有隔离机制（2.1g）。要做真沙箱，必须放弃"单一地址空间"——而那正是 Emacs 作为 agent runtime 的最大优势本身。与其造一个会给人错误安全感的假沙箱，不如
把边界做成**诚实的、可审计的、有摩擦的**。

这里要区分两个词：**摩擦**（审批门槛、eshell 优先于 shell-command）是**政策工具**，用来把"越界"变成一个需要人参与的显式事件；它不是**安全机制**，一个刻意绕行的
模型或一段被注入的指令可以从 eval-elisp 走过去。这两者不能混说。

**6）由此得到一条对设计有约束力的推论：**

> 既然 eval-elisp 使能力完备，那么废弃路径式文件 I/O 的目的就**不是限权，是消歧**。

这条必须写进文档，否则会被误读成一次安全加固，进而对系统的安全性产生错误信心。收敛工具面提升的是**模型行为的可预测性**与**审计条目的信息量**，不是**攻击面**。攻
击面在 eval-elisp 存在的前提下基本不变。

（同一逻辑的另一面：如果哪天真的想收窄攻击面，唯一有效的动作是**给 eval-elisp 与 shell-command 分级或下线**，而不是继续削减对象工具。这是判据，不是建议。）

## 2.5 「AI OS 层」这个词为什么成立

OS 的定义性职责有三：**抽象资源、仲裁访问、提供统一接口**。superchat 在 Emacs 内部恰好做这三件：

- 抽象资源：把 Emacs 对象抽象成工具面
- 仲裁访问：Authority 的 validate → effects → policy → approval → execute → Tape
- 统一接口：同一套 effect 词汇表（`local-read` / `local-write` / `process-exec` / `runtime-exec` / `external-*`）

它不是"Emacs 上的一个应用"，因为**应用不仲裁别人的访问**。

而它之所以能是「层」而不是「壳」，是因为 Emacs 允许在语言层面插入这一层（advice / hook / 单地址空间）。换个宿主就不成立：VSCode 扩展、JetBrains 插件只能拿到宿主
开放的 API 子集，第三方能力可以绕过你的缝直接调宿主。在那里，AI 层只能是 **sidecar**；只有在 Emacs 这种"系统即库"的宿主里，AI 层才可能成为**内核缝（seam）**。

这也是"Emacs 的 AI OS 层"与"支持 Emacs 的 AI 插件"之间的真实差别——不是野心大小，是**能否让所有能力都过同一道缝**。

---

# 三、可检验的推论

不谈路线，只给能证伪的判据。这些是用来检查上面的论证是否只是好听：

1. **语义自足假说**：若成立，则给 buffer 工具增加需要外部上下文的参数（相对路径、行号偏移）会让模型表现变差；反之，多做 `workspace_info` 这类"自陈式"工具会变好
。
2. **对象面一致性假说**：若成立，则新增一个 Emacs 域工具（如 org 子树操作）的模型上手成本，应显著低于新增一个同等复杂度的外部 API 工具。
3. **eval-elisp 是缺口探针**：若"能力完备的逃生舱"这个描述准确，则当对象工具出现缺口时，Tape 上应观察到模型**转向 eval-elisp 而不是失败**。这个转向率是工具面缺
口的直接度量。
4. **buffer 身份稳固性**（2.3 最后一条）：若确实是问题，Tape 上应有 `EditBuffer` 因 buffer 不存在或名称歧义而失败的记录，且集中在长会话后段。

---

# 四、一句话收束

面向 Emacs，是把 agent 的世界从"按位置寻址的死字节"换成"按身份寻址的活对象"；Emacs 能担这个职责，是因为它本来就是一台自带反射能力的单地址空间 Lisp 机器，而它的
社区四十年一直在无意中为这一天准备适配层；代价是永久放弃宿主 OS 的能力面，收益是可编程性、可审计性与人机同屏；而 eval-elisp 提醒我们把话说准——这条边界给出的是**
可归因性**，不是**不可能性**，因此收敛工具面的正当理由是**消歧**，不是**限权**。
