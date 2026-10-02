# Decision: M11 第 3 片——个人 workspace

Status: accepted — 用户，2026-10-02："好，下一片"。条目来自 [workspace 契约](2026-10-01-workspace-work-record-session-rules.md)的 W11；"leader 的选择"一节是 leader 在这句话之下定的机制，随本片向用户报告。

## Problem

日程、RSS、Telegram 这类界面不对应任何文件。它们的 buffer 有一个 `default-directory`，但那只是打开它时碰巧所在的目录：从某个项目的文件里打开日程，日程 buffer 的目录就是那个项目。第 1 片按目录认定 workspace，所以从日程里叫出的对话会落进那个碰巧的项目，带上它的规矩和历史；不在项目里时则是临时对话，说过的话留不下来。

## Decision

用户指定一个真实的目录作为个人 workspace。属于"不对应目录的界面"的 buffer，从它叫出的对话、就地请求都进个人 workspace 的主对话，带个人 workspace 的 `AGENTS.md`。没有指定时，这些界面叫出的是临时对话，不落进碰巧所在的项目。个人 workspace 是一个普通的 workspace：别的 workspace 的请求里没有它的规矩。

验收：`scripts/workspace-personal-regression.el`；一个真实任务——从日程里问一个问题，请求里带着个人 workspace 的规矩，回答照着做，另一个项目里的请求没有这条规矩。

## leader 的选择

1. **哪些 buffer 算"界面"由 major mode 决定。** 用户选项 `superchat-personal-workspace-modes` 列出这些 mode（默认含日程、日历、elfeed、telega、gnus、mu4e、notmuch、erc、rcirc），派生的 mode 也算。不按"没有访问文件"来判断：shell、dired、magit 的 buffer 也不访问文件，但它们属于所在的目录。
2. **指定用一个用户选项加一条命令。** 选项 `superchat-personal-workspace-directory`（默认无）；命令 `superchat-set-personal-workspace` 把目录声明为 workspace（写 `.superchat/id`）并把选项存进用户的定制文件。只在配置里设选项也生效，这时不写标记文件，身份按路径算。
3. **目录在某个 project 里时，个人 workspace 就是那个 project。** W6 规定一个 workspace 只有一个根，根由 Emacs 的 project 机制定。
4. **"个别界面另指给别的 workspace"用一个 buffer 局部变量。** `superchat-buffer-workspace-directory`，在该界面的 mode hook 里设。它优先于 mode 和目录，对任何 buffer 都有效。
5. **认定顺序集中在一个函数 `superchat-workspace-of-source`**：buffer 自己指定的目录；界面 → 个人 workspace（没指定则无）；否则按目录找 project。`superchat` 里两处原来直接按目录找的地方改为调用它。

## Alternatives considered

**没有访问文件的 buffer 一律归个人 workspace。** 输在 shell、dired、magit、编译输出都没有文件，它们明显属于所在的项目。

**没指定个人 workspace 时，界面按 `default-directory` 落进项目。** 输在契约 W11 写明"指定之前，从这些界面叫出的是临时 workspace"，而且那个目录是碰巧的。

**用一张"mode → 目录"的表来另指。** 输在 Emacs 里按 mode 设 buffer 局部变量已有 mode hook 这个现成的办法，不必再发明一张表。

**第一次从日程叫出对话时问用户要不要指定。** 输在打断：W5 的"当场问"是在用户说"记住"的时候，那时临时对话的说明已经告诉模型该怎么回答。

## Consequences

- 从日程、RSS、聊天界面叫出的对话，即使 buffer 的目录在某个项目里，也不再进那个项目的对话：指定了个人 workspace 就进它，否则是临时对话。
- 用户的定制文件在执行 `superchat-set-personal-workspace` 时被写入一项；个人目录里多一个 `.superchat/id`。
- 默认列表里没有的界面按目录认定，用户可以把它的 mode 加进列表。
