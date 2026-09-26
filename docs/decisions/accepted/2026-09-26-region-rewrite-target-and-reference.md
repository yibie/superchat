# Decision: a region rewrite replaces only its target; the text around it is a read-only reference

Status: accepted — user, 2026-09-26, after a rewrite in their diary replaced one selected line with a polished copy of the whole previous heading: "整个 rewirte 的流程设计很有问题", then "按照建议" to round 1 (Q1–Q5) and "都按照建议处理" to round 2 (Q6–Q9) of a grilling session.

## 一句话

改写（region rewrite）以后只改你选中的那段，旁边的内容只给模型当参考，不许它改、也不许它交回；模型直接交回替换用的文字，不再包代码块；结果一看就不对（带着我们的标记，或长得离谱）时，产品会拦下或提醒，而不是照单全收。

打个比方：以前是把整页稿子递给编辑，用铅笔圈出一行，说"润色这一段"，编辑把整页都改了交回来，我们又把整页贴进了那一行的位置。以后是：圈出的那行单独给编辑，旁边几段复印件注明"仅供参考，别动"，编辑只交回那一行的新写法；交回来的东西如果夹着圈线的记号，或者比原来长出一百倍，就先拦下来。

## Problem

The observed run (user's Tape, session `20260926-082655-2767`, 2026-09-26 08:26, DeepSeek `deepseek-chat`):

- The Rewrite target was one line of 14 characters under a new Org heading.
- The request was `Rewrite the following org-mode code according to this instruction: 润色这一段`, then the 20 lines before the target, the target between `<<<REGION>>>` and `<<<END>>>`, and the 20 lines after (`superchat-rewrite--content`, `superchat-rewrite--context-text`, `superchat-rewrite.el:127-143`). The 20 lines before crossed into the previous heading and carried a whole film review.
- The system text was the code-editor prompt (`superchat-rewrite-system-prompt`): "You are a precise code editor. Rewrite the provided code … Return ONLY the rewritten code inside a fenced code block". Nothing said that only the marked text is to be rewritten and returned.
- The model polished everything it received and returned it all — the review, the heading, the markers, the quote — inside ```` ```org-mode ````. The instruction was followed, on the wrong text.
- The product took the first fenced block as the replacement without any check (`superchat-rewrite--sink`'s `:final`); the user accepted it: 14 characters replaced by 1,625 (`edit.applied`). The file on disk was saved clean afterwards.
- While typing, the preview showed the raw stream, fence included.

gptel's `gptel-rewrite` (the user's installed copy, `gptel-rewrite.el:141-175`, `:770-810`) sends only the selected text, and for a non-programming mode tells the model it is an editor of that language that returns only the replacement text, without explanation or code fences.

## Decision

The user took the recommendation on every question.

1. **Two kinds of rewrite, chosen by mode** (Q1 A). In a buffer derived from `prog-mode` the model is told it rewrites code of that language; in any other buffer it is told it is an editor of that kind of text (Org, Markdown, plain text). Both say: rewrite only the Rewrite target, follow the instruction, return only the replacement, no explanation, no code fence.
2. **Target plus a read-only reference** (Q2 B). The request carries the Rewrite reference and the Rewrite target in separately labelled parts, and the instruction last. The reference is labelled as text the model may read but must not change or return.
3. **Plain replacement text** (Q3 A). The model returns the replacement alone. When the answer is nevertheless one fenced block, the fence is removed. A target that ends with a newline keeps ending with one, and one that does not keeps not.
4. **A wrong-looking answer is caught** (Q4 A). An answer that contains any of the request's labels or markers is refused: the rewrite ends in an error naming the reason and cannot be accepted. An answer much longer than the target (more than three times its length and more than 200 characters longer) is shown, and the status line says how much longer it is and to check it before accepting.
5. **The stream is shown clean** (Q5 A). While typing, the preview shows the answer so far without an opening or closing code fence.
6. **The reference stays inside the target's section** (Q6 A). In Org and Markdown the reference is the rest of the heading section the target is in — from its heading line to the next heading of any level — and never crosses into another section; before the first heading, from the start of the buffer. In a `prog-mode` buffer it is the top-level definition(s) the target is in, widened by `superchat-rewrite-context-lines` lines on each side. In any other mode, `superchat-rewrite-context-lines` lines on each side. Each side is capped at 8,000 characters, keeping the part nearest the target.
7. **No selection, no rewrite** (Q7 A). Without an active region the command says so, as today.
8. **The prompt names the target** (Q8 A). The instruction prompt shows how much is selected and its beginning, for example `Rewrite 14 characters «看到一篇关于刘欢讲课的结语：» — instruction: `, so a wrong selection is visible before anything is sent.
9. **Review stays in place; the diff is announced** (Q9 A). The preview still shows the new text over the target; the ready status line lists `C-c C-d diff` beside accept and reject.

Unchanged and not asked again: a rewrite joins the Session the chat View is showing (M10, [editor mode, agent mode and review screen](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)); the keys; a refinement (`C-c C-r`) rewrites the previous answer, with no reference; a non-contiguous region gets one rewrite per segment; `superchat-rewrite-confirmation`.

A user-set `superchat-rewrite-system-prompt` string still replaces the built-in system text; its default becomes nil, meaning the built-in text for the buffer's kind.

## Amendment 2026-09-26: the keys reach the preview from right after it

The user's first real rewrite on this design worked (only the selected line changed), and showed that `C-c C-a` ran `org-attach`: after a forward selection point sits at the preview's end, just outside the overlay, where its keymap does not apply, so Org's own `C-c C-a`/`C-c C-k`/`C-c C-d`/`C-c C-r` ran. gptel's rewrite uses the same keys on an overlay keymap and has the same limit. The user chose, as recommended ("按建议"):

- **Q1 B**: the preview's `C-c` keys work inside the preview and right after it (point at its end); anywhere else the keys are the major mode's. A buffer-local minor mode, on while the buffer has a preview, binds them through a filter that yields nothing away from a preview.
- **Q2 A**: the menu on RET and a click, which already existed inside the preview (`superchat-rewrite-menu`; the question wrongly offered it as new), is named on the ready line (`RET menu`). RET right after a preview stays the mode's own, a new line.

Rejected: the keys buffer-wide while a preview is pending (hides Org's attach, deadline and folding keys meanwhile); moving point into the preview (it jumps, and moving away loses the keys again); other keys (Org uses nearly every `C-c C-letter`).

## Alternatives considered

**Send only the target (gptel's way).** The simplest and the least likely to go wrong. Lost to the reference because a code rewrite often needs the names around it and a text rewrite its tone; the check in 4 is what keeps the reference from leaking into the answer.

**Keep the fenced answer.** The fence gives a clear boundary to parse. Lost because it invites the "code editor" framing for prose, shows up in the stream, and the boundary it gives is not the one that matters (what is inside can still be the wrong text).

**Only warn on a wrong-looking answer.** Lost for the marker case: an answer carrying our own markers is certainly not a replacement for the target, and accepting it corrupts the buffer.

**The whole file as reference (Cursor-like).** Most context; lost on cost per request and because a long reference raises the chance of the model rewriting it.

**An inline red/green diff as the preview.** Clear for code; lost because a polished Chinese paragraph changes almost every sentence and the diff would be noise. The unified diff stays one key away.

## Acceptance

A new regression file (not the M3 set), each test red at the gap first: the request has labelled target, reference and instruction and no text outside the section; prog-mode and text-mode system texts; the observed request shape (a target under a new heading after a long previous section) no longer carries the previous section; a fenced answer is unwrapped, a plain one kept; an answer containing the markers ends in an error and cannot be accepted; a long answer is ready with the warning; the typing preview has no fence; the prompt names the target; the ready line lists the diff key; trailing-newline rule. Then mutants, the full sweep, and one real rewrite by the user on their diary.

## Consequences

- The request text of every region rewrite changes; smoke E and the existing rewrite tests keep passing because a fenced answer is still unwrapped.
- A user who customized `superchat-rewrite-system-prompt` keeps their text, which may still ask for a fence; the unwrap covers it.
