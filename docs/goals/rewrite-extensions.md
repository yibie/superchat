# Goal: 写回编辑器扩展(代码块当 patch + 多 region)

> 状态:**v1 落地完成 (2026-06-15)。** 里程碑候选:v1.2。
> 创建于 2026-06-15；实现在 2026-06-15。
> 隶属:`emacs-native-integration.md` 方向一的扩展(P1 `superchat-rewrite-region` 已落地)。
>
> **已落地**:
> - Part A: `superchat-rewrite-apply-code-block-at-point`(回复代码块→ediff 应用) + 源 buffer/region 记录
> - Part B: 多 region/矩形选区支持 (`region-noncontiguous-p` → 逐段收集→块数校验→逐段 ediff)
> - 测试: `test/test-rewrite.el` 扩展至 21 项 ERT(全部通过)

## 问题陈述

方向一 v1 已交付 `superchat-rewrite-region`(`superchat-rewrite.el`):选区 →
指令 → 无工具 blocking LLM → 解析首个 fenced 代码块(`superchat-rewrite--extract-code-block`,
`superchat-rewrite.el:111`)→ ediff 预览 → `atomic-change-group` 原子替换
(`--apply`:129 / `--ediff-and-replace`:147)。两处明确的 v1 边界还没补:

1. **聊天回复里的代码块只能复制粘贴**。当用户在普通对话中得到一段代码,
   没有「一键应用为 diff」的路径——rewrite 的能力只覆盖「选区改写」这一入口,
   不覆盖「任意回复的代码块」。
2. **rewrite 只支持单连续 region**(v1 决策),多处选区/矩形选区会失败。

## 方案

### Part A — 回复代码块当 patch(一键应用)

- 在渲染层(`superchat-render.el`)给每个 fenced 代码块附一个「应用」动作:
  - 复用 `superchat-rewrite--extract-code-block` 提取块内容与 lang-tag。
  - 目标定位:若代码块前文/skill 上下文记录了源 buffer+region(复用 rewrite/send-* 的源记录),
    则对该 region 走 `superchat-rewrite--ediff-and-replace`;否则提示用户选目标 buffer/位置。
  - 入口:代码块上的 `C-c C-c`(org-babel 风格)或按钮 overlay。
- 不新开 LLM 调用——纯本地 diff 应用,复用既有 ediff/atomic-change-group 路径。

### Part B — 多 region / 矩形选区

- `superchat-rewrite-region` 支持 `region-noncontiguous-p`(矩形/多段):
  - 收集各段,拼成带分隔标记的 prompt,要求 LLM 按同序返回等量代码块。
  - 解析多个 fenced 块,逐段 ediff/替换;数量不匹配则中止并提示。
- 退化路径:单段时行为与 v1 完全一致。

## 范围控制

复用 `superchat-rewrite.el` 既有提取/应用/ediff 函数,不新增 LLM 调用模式。
Part A 主要落在 `superchat-render.el`;Part B 落在 `superchat-rewrite.el`。
估算 ~+100 行 + 测试。

## 验收标准(草案)

- [x] 聊天回复中的 fenced 代码块提供「应用」入口;有源 region 记录时经 ediff 写回,无记录时引导选目标;应用结果可 `undo`。
  - 新增 `superchat-rewrite-apply-code-block-at-point` (`;;;###autoload`, `C-c C-a` in `superchat-mode-map`)
  - `superchat-rewrite--code-block-at-point` 解析 org `#+begin_src ... #+end_src` 块
  - `superchat-rewrite--resolve-target-buffer` / `--resolve-target-region` 目标定位
  - send-region/defun/rewrite-region 调用 `superchat-rewrite--record-source` 记录源 buffer+region
  - 无记录时 `read-buffer` 提示选择;有记录时走 ediff 预览 + `atomic-change-group` 应用
- [x] `superchat-rewrite-region` 在矩形/多段选区下按段返回并逐段替换;块数不匹配时中止并报错,不留半成品。
  - `superchat-rewrite--collect-region-segments` 收集每段 (TEXT BEG END)
  - `superchat-rewrite--multi-region` 组装多段 prompt,调用 LLM,解析多块
  - `superchat-rewrite--extract-all-code-blocks` 提取所有 fenced 代码块
  - 块数不匹配 `user-error`("expected %d, got %d")
- [x] 单连续 region 行为与 v1 字节级一致(回归保护)。
  - 路径分离:`region-noncontiguous-p` → multi path; else → `superchat-rewrite--single-region`(v1 原逻辑)
  - 既有 12 项测试全部通过
- [x] 扩展 `test/test-rewrite.el`:多块提取、多段映射、数量不匹配中止;全绿。
  - 新增 9 项测试: extract-all (4) + code-block-at-point (3) + record-source (1) + keymap (1)
  - 全量 54/54 通过

## 开放问题

- Part A 的目标定位:无源 region 记录时,默认选「最近一次访问的非聊天 buffer」还是强制用户挑?**v1 决策**: 强制 `read-buffer` 挑,避免误写。
- 多段返回的分隔约定:**v1 决策**: 依赖块顺序 + 数量校验,分隔符用 `superchat-rewrite-multi-region-separator`(`// === BLOCK %d ===`)。
- 矩形选区替换后列对齐如何保证?**v1 未解决**: 矩形选区退化为逐行独立区域,不做列对齐保证。

## 来源

- 2026-06-15 与维护者的方向讨论(本对话)。
- 现状审计:`superchat-rewrite.el:111`(`--extract-code-block`)、`:129`(`--apply`)、
  `:147`(`--ediff-and-replace`)、`superchat-render.el`(代码块渲染层)。
