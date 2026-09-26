# Decision: `read_buffer` returns a bounded window with a header, like `read_file`

Status: accepted — user, 2026-09-26: "follow" to the three questions (window and caps like `read_file`: A; no line numbers: A; a narrowed buffer is read in its narrowed part and says so: A). Triage of `.scratch/issues/2026-09-24-read-buffer-has-no-window-or-cap.md`, chosen as the next item the same day ("按照你的建议来").

## 一句话

模型用 `read_buffer` 看一个打开着的 buffer 时，拿到的是整个 buffer，没有上限、没有行号、也没有任何说明；一个 1.5 MB 的 buffer 就原样塞进对话，之后这次任务的每一次请求都要再带上它。`read_file`（看磁盘上的文件）早就有"每次最多 2000 行 / 50 KB，用 offset 续读，头部写明是不是完整内容"的规矩。这份决定让 `read_buffer` 守同一套规矩。

打个比方：图书馆的"借纸质书"窗口（`read_file`）一次最多借 2000 页，并在借条上写"全书 N 页，这是第 A 到 B 页"；而"看正在修订的稿子"窗口（`read_buffer`）现在是把整摞稿子一次推过来，也不说有多少页。决定就是给第二个窗口也配上同样的借条和上限。

## Problem

Facts (issue file, re-checked 2026-09-26 against the tree):

- `superchat-tool-read-buffer` returns `(buffer-substring-no-properties (point-min) (point-max))` of the named buffer (`superchat-tools.el:386-391`). One argument, `buffer`. No header.
- `(point-min)`/`(point-max)` are the **accessible** bounds: in a narrowed buffer the tool returns only the narrowed part and says nothing, so a partial text passes for the whole buffer. `edit_buffer` also works on the accessible text (`superchat-tools.el:566-569`); `replace_buffer` widens.
- `read_file` on the same text has `offset`/`limit`, 2000 lines, 2000 characters per line, 50 KiB, and a header that never lets a partial result pass for the whole file ([read_file record](../implemented/2026-09-18-read-file.md)).
- Nothing between the tool and the provider shortens a result (`superchat-tool-execution--normalize-tool-result` only decodes bytes). `open_file` accepts files up to `large-file-warning-threshold` (10 MB), so a multi-megabyte buffer can be opened and then read whole.
- Repro `.scratch/cursor-like-integration/probes/repro-read-buffer.el`: a 1,580,000-byte, 20,000-line buffer comes back as 1,580,000 characters; `read_file` on the same file returns 51,389.

## What the references do

- **DeepSeek Harness** `read` (`packages/fs/tool-fs/src/read.ts`): `file_path`, `offset`, `limit`; 2000 lines, 2000 chars/line, 50 KiB; **line-numbered** output.
- **Pi** `read` (`packages/coding-agent/src/core/tools/read.ts`): `path`, `offset`, `limit`; 2000 lines or 50 KB; **no line numbers**; a trailing note "Showing lines A-B of N. Use offset=… to continue."
- Neither has a buffer concept: each has one read tool, and it is always windowed. No reference returns a text of unbounded size from a read.

## Decision

The user took the recommendation on each of the three questions ("follow", 2026-09-26).


1. **Same window and caps as `read_file`** (question 1, A). `read_buffer` takes `buffer` plus optional `offset` (1-based first line, default 1) and `limit` (default and maximum 2000). Same three limits, same constants (`superchat-tools-read-file-*`, not renamed), same argument validation, same truncation wording.
2. **Same result shape.** Header, a line `Content:`, the text:
   - `Buffer: NAME`
   - `Lines: A-B of N (complete buffer)` or `Lines: A-B of N (NOT the complete buffer: …reasons…)`, with the continue-with-offset hint; the empty-buffer and past-the-end cases as in `read_file`.
3. **No line numbers** (question 2, A), as `read_file` recorded (`edit_buffer` needs the exact text; a numbered text would have to be un-numbered before quoting). Pi agrees; dsh differs.
4. **Narrowing is said, not hidden** (question 3, A). The tool keeps reading the accessible part (what `edit_buffer` can edit). When the buffer is narrowed, the header adds `Note: the buffer is narrowed to lines X-Y of Z; line numbers here count from the start of the narrowed part.`, and the `Lines:` line says `NOT the complete buffer`. Reading does not widen: narrowing is the user's view.
5. **Settled without a question** (follow from 1-4 or from the current tool):
   - A buffer visiting a file with unsaved changes gets `Note: this is the unsaved text of FILE; the version on disk differs.` — the mirror of `read_file`'s note, so the model knows which one it has.
   - A name that is not a live buffer stays an error, as today; so is a bad `offset` or `limit`.
   - The header names the buffer as `list_buffers` does, as a quoted Emacs Lisp string, so a name with spaces or a newline is unambiguous.
   - One windowing helper, extracted from `superchat-tool-read-file`, serves both tools; `read_file`'s output stays byte-identical.
   - The tool description says what `read_file`'s says about the header, the caps and `offset`.

## Alternatives considered

**Leave `read_buffer` whole and rely on `read_file` for large files.** Lost because `read_file` reads the disk, not the buffer: after the model's own `edit_buffer` changes (which stay unsaved by the buffer-first rule) the disk is behind, and the only way to see the real text is `read_buffer`. The case that most needs a bounded read is the one this leaves uncovered.

**Line-numbered output (dsh's form), for both tools.** Line numbers let the model say "lines 120-140" and let a future edit tool address lines. Lost for now because every current edit tool takes exact text, and `read_file`'s recorded choice was no numbers; changing both is a separate decision, not part of bounding the size.

**Widen before reading.** Would always show the whole buffer. Lost because `edit_buffer` cannot touch text outside the narrowing, so the model would see text it cannot edit and the user's narrowing would be silently ignored.

**A cap without a window (cut at 50 KiB and stop).** Lost because the model then has no way to see the rest of an unsaved buffer at all.

## Acceptance

1. A new regression file `scripts/read-buffer-regression.el` (not the M3 set), each test red at the gap on the current tree before any code: header and `Content:`; window by `offset`/`limit`; each of the three caps with its header wording and continue hint; no line numbers; narrowed buffer (accessible text, note, not complete); unsaved visiting buffer note; bad arguments; dead buffer; schema has `offset`/`limit`; `read_file`'s output unchanged. Then mutants.
2. Full sweep green (smoke, every regression script, kernel check, byte-compile). Smoke records only that the `read_buffer` descriptor exists, not its schema or its results, so no baseline is expected to change; if one does, it goes to the user.
3. The repro above returns at most 50 KiB with a header saying where to continue.

## Consequences

- Inline ask's fixed tool baseline includes `read_buffer`: a source buffer over 2000 lines or 50 KiB now takes more than one read. Fewer tokens per request, more requests for a very large buffer.
- Tests and fixtures that read `*scratch*` through `read_buffer` get a header in the result. The known ones (smoke B and L, the M3 fixtures) do not assert on the result text; the sweep checks the rest.
