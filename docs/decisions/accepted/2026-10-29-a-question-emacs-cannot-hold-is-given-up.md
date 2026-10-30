# Decision: Emacs 撑不住递归编辑时，审批问题不再空转，而是限时后放弃

Status: accepted — 用户在 2026-10-29 的卡死诊断之后说"需要加保护和交叉测试"。

## Problem

`superchat-approval-publish` 把审批问题发布出去之后，在一个递归编辑（`recursive-edit`）里等答案：用户可以继续编辑，答了问题，计时器把这层编辑退出。等待的代码是 `superchat-approval--wait` 里的一个循环：

```elisp
(while (null (plist-get record :decision))
  (recursive-edit))
```

这个循环没有任何保护。Emacs 的 `recursive-edit` 在输入被阻塞时会直接返回 nil，不进入命令循环（`keyboard.c:809`，`if (input_blocked_p ()) return Qnil;`）。那种情况下循环会立刻再调一次，再立刻返回，无限空转：不重绘，不跑计时器，C-g 也进不来。整个 Emacs 看起来就是死了。

2026-10-29 15:52 用户问了一个普通问题，模型第二步要调 `eval_elisp`，权限级别 `edit-buffers` 要先问人。Tape 里记下 `approval.requested: eval_elisp`，之后再无任何事件；截图里状态行停在 5 秒，没有 `⏳ eval_elisp` 行，也没有审批面板，说明那之后没有重绘过。用户的 Emacs 只能强杀。重启后没再出现；用同一个二进制、同一套配置做了四种复现都没卡住，所以是什么让 `recursive-edit` 立刻返回，没有查出来。

查出来的是另一件事：这个交互分支没有任何测试覆盖。所有审批测试在 `--batch` 下跑，那里 `noninteractive` 为 t，走的是 `accept-process-output` 轮询分支。

## Decision

- `superchat-approval--wait` 要知道递归编辑有没有真的被撑住。退出编辑的那个计时器只会在那一层的命令循环里跑，所以它跑过就是证据：计时器在那一层跑到时设一个标记，每次进入前清掉。`recursive-edit` 回来时有决定，结束；没决定但标记设了，是用户自己 `C-M-c` 离开了这层，照旧再进一次；没决定、标记也没设，Emacs 在这里撑不住编辑，不再进了。
- 撑不住编辑时，改成轮询等待（和 batch 一样跑计时器和进程输出），最多等 `superchat-approval-unheld-wait` 秒，默认 30 秒。这段时间里 emacsclient 发来的 `superchat-approval-allow` / `superchat-approval-deny` 仍然能决定它；Run 结束的监视也仍然起作用。到点还没人答，问题就放弃，当作不允许，用户收到提示。
- 时限必须在循环里用时钟检查，不能靠计时器：阻塞输入的那一层在这个等待之上，等待不返回它就不能退；而 Run 被提问时期限是停着的（`superchat-runtime--deadline-passed-p` 在 `:deadline-stopped-at` 期间返回 nil）。所以没有时限的话，空转只是变成了一个安静的死等。
- 放弃要留痕。`approval.decided` 事件带 `:reason "no-edit" :interruption "no-edit"`，和 Run 结束的 `run-ended`、用户 C-g 的 `user-quit` 区分开。这次就是靠 Tape 定位到卡在哪一步的。
- 标记的代价说清楚：用户在计时器第一次跳（0.1 秒）之前就 `C-M-c`，看起来和撑不住编辑一样，会走轮询路径。轮询路径仍然能决定问题，只是那段时间键盘不响应。

batch 分支和 `inhibit-quit` 的绑定不动：出事的只有交互分支。

## 测试

`scripts/approval-edit-regression.el` 为每个场景起一个伪终端下的交互式子 Emacs（`scripts/approval-edit-child.el`），从计时器里发布问题，和产品里一样。六个场景：键盘输入 `M-x superchat-approval-allow` 允许，输入 deny 拒绝，`C-M-c` 离开后再允许（守卫不误判），`recursive-edit` 被替换成立刻返回 nil 后由句柄回答（守卫起作用），同样替换但没人回答（限时后放弃，留痕），同样替换但 Run 在 1 秒后结束（Run 监视仍然有效）。把 `superchat-approval.el` 换回修改前的版本跑"由句柄回答"这个场景，子 Emacs 空转，测试在 30 秒期限到时失败。

## Alternatives considered

轮询不限时。输在上面说的那条：阻塞输入的那一层退不了，Run 期限又停着，等待永远不结束，只是从吃满 CPU 变成安静地卡住。

撑不住编辑就立刻拒绝。输在 emacsclient：有 30 秒窗口，还能从外面回答；立刻拒绝就把这条路也关了。而且立刻拒绝的时候用户还没看到问题。

把等待换成 minibuffer 提问（`superchat-approval-prompt 'minibuffer`）。那是另一条代码路径，不经过 `recursive-edit`，但它在提问期间整个 Emacs 都停着，正是 2026-10-11 决定"发布后继续"要避免的。
