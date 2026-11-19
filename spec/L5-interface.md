# L5 interface: the chat buffer, inline commands, plugins

Part of the [specification](../SPEC.md). Modules: `superchat-chat.el`,
`superchat-turn.el`, `superchat-inline.el`, `superchat-plugin.el`,
`superchat-messages.el`. This
layer is what the person uses; every effect it has goes through
`superchat-run` and the public functions of the layers below.

## superchat-chat.el

One buffer per timeline, named `*superchat: TITLE*`, TITLE being the first
40 characters of the timeline's own first `user` entry (a fork's refs do
not count), or `new` until it has one. Major mode `superchat-chat-mode`,
derived from `org-mode`.

- The buffer is rendered from the `conversation` view, one turn at a time: a
  turn is a `user` entry and the entries after it up to the next one
  (the entries before the first `user` entry are a turn too). As an entry
  comes, its turn is drawn again from that turn's entries, so rendering the
  whole view again gives the same text and the same folding. Text above the
  input area is read-only. What a turn says is `superchat-turn-text` of its
  entries.
- The answer's text streams in as it comes, raw, in the `shadow` face,
  where the final answer goes: under `** Answer`, after Progress; the
  `assistant` entry replaces it when it comes.
- `TAB` on a heading of the entries toggles it by Org's own cycling, the
  read-only text notwithstanding; the subtree of the last heading ends where
  the input area's lines begin, which never fold. A heading the person
  opened or closed stays so when its turn is drawn again. Folds are put back
  by each heading's identity only, never by a position or a length: each
  hides that heading's body as Org reckons it in the text now, ending before
  the first line that belongs to no heading as deep (a permission line ends
  the body of the call before it, a line of the turn's other entries ends
  every body); a heading no longer there has no fold.
- Below the entries are a separator line and the prompt `❯ `, both
  read-only; the input area is everything after the prompt. Typing a
  self-inserting character with point above the input area moves point to
  the end of the input and inserts it there.
- `header-line-format` shows `MODEL · LEVEL · SCOPE`: the buffer's model or
  `superchat-llm-model` (`not set` when neither is), the permission level of
  the timeline, the scope through `abbreviate-file-name`; and ` · running`
  while the timeline is busy.
- The last character of what is shown passes on no text property: what is
  typed after it has no face and no entry id.
- The input area is the editable text after the prompt. In it, `RET`
  sends when point is in its last line, otherwise inserts a newline; above
  it, `RET` is an edit of read-only text and refused as any edit there is.
  `C-c C-c` always sends, `C-u C-c C-c` forces (below). `C-c C-k`
  cancels the run. What is sent or queued is
  deleted from the input of the buffer it was typed in, also when the
  command it was (`/new`, `/resume`, `/fork`) shows another buffer.
- When an entry or streamed text is shown, point, and the point of every
  window showing the buffer, stays where it was when above the input area
  and moves with the input when in it. A window whose point is in the input
  area then shows the end of the buffer on its last line, and as much of
  the entries above it as fits; any other window keeps the position it
  starts at.
- Sending from the input area while the timeline is busy (a run or a
  handoff) queues the input instead, when it is not empty and not a
  command: the input is cleared, and a dimmed line `⏸ queued: FIRST-LINE`
  (`⏸ N queued: FIRST-LINE` when N > 1), FIRST-LINE the first line of the
  first queued text cut to 60 characters and `…` when anything is left
  out, shows just above the separator, after the waiting line, as part of
  the same overlay. The queue is the buffer's only, not on the Tape:
  killing the buffer drops it. When the run ends `done`, or the handoff
  makes its anchor, the queued texts, joined by a blank line, are sent at
  once as one request, as a send from the input area would send them. When
  it ends otherwise (cancelled, error, a failed or cancelled handoff), the
  queue is not sent: its text goes back to the start of the input area,
  before what is typed there and apart from it by a blank line, and the
  message says so.
- `C-u C-c C-c` while the timeline is busy cancels the run or the handoff and,
  once it has ended (from the run's ON-DONE), sends the queue and then the
  input, joined by a blank line, as one request. When it is not busy it
  sends as `C-c C-c` does.
- What is refused while the timeline is busy (a command that needs it free
  such as `/handoff`, and `superchat-chat-send` called by a program, which
  never queues and returns nil) is refused with a message, not an error.
  A command is never queued. The message is `A request
  is still running; C-c C-k cancels it` during a run and `A handoff is
  still running; C-c C-k cancels it` while a handoff is pending.
- While the timeline is busy (a run or a handoff) the mode line shows
  `[running Ns]`, N the whole seconds since it began. While a run is active
  a waiting line just above the separator shows the strip of the current
  turn on a line of its own, when the turn has a call, then a spinner and
  the seconds since the latest entry: `⠋ NAME running… Ns` while a call of
  the turn has no result (the first such), else `⠋ thinking… Ns`; when the
  latest entry is a `retry` event, `⟳ trying again in Ns (I/N)`, N the whole
  seconds left of its wait, and the text streamed for the failed request is
  removed. While
  the person is asked about a call of this timeline
  (`superchat-permission-asking`), the cell of that call, the first with no
  result, is `⏸` in `warning`, and the spinner's line is, in `warning`,
  `⏸ waiting for your answer (minibuffer): NAME SUMMARY — y/n/a`, SUMMARY
  cut to 60 characters and `…`. It is an overlay, not
  text of the buffer and not an entry; a timer of the buffer updates it and
  the mode line, and is cancelled when the run ends or the buffer is
  killed; the line is gone when the run ends.
- While no run is active and a call of the timeline waits for an answer
  (`superchat-question`), the line above the separator is, in `warning`,
  `⏸ waiting for your answer: NAME SUMMARY — send y, n or a`, the keys
  in bold (`(superchat-chat-answer-keys &optional FACE)` → `y, n or a` in
  FACE, its keys also bold). Sending `y`,
  `n` or `a` then answers it with `superchat-answer` (`allow`, `deny`,
  `always`), shown as a run of this buffer; other text is refused with a
  message and stays in the input; a command is not refused.
- While a run is active the input stays editable. Streamed text that never became an entry is removed when
  the run ends (cancelled or error). A run that ends with `error` says so
  in the echo area with the error's text. An entry is shown once, however
  it reached the buffer. The callbacks of a run find the chat buffer of
  their timeline when they fire, so a buffer killed and opened again during
  a run shows the rest of it.
- After a handoff the buffer shows the conversation from its anchor: the
  anchor line, its summary and what comes next. The earlier entries stay on
  the Tape; `/topic` and `/recall` find them.
- `superchat` remembers the buffer it was called from as the origin of the
  next request sent from the chat buffer. Called from a chat buffer it
  records no origin.
- `superchat` (an autoloaded command): open the chat buffer of the current
  buffer's own conversation, or of a new timeline when it has none. With a
  prefix argument, always a new one. A buffer's own conversation is: in a
  chat buffer, its own; else a chat buffer opened from this buffer that has
  sent nothing yet; else the most recently active timeline of the scope, not
  ephemeral, that has a `user` entry whose origin `:file` is this buffer's
  file (or, for a buffer visiting no file, whose origin has no `:file` and
  this buffer's name as `:buffer`). It enables the plugins of `superchat-plugins`
  that are not enabled yet.
- `(superchat-chat-open TAPE &optional SCOPE)` → the chat buffer of timeline
  TAPE, made and rendered when there is none. It does not display it. SCOPE
  is for a timeline with no entry yet: the buffer keeps it and gives it to
  `superchat-run` with the first request; with no SCOPE, it keeps the scope
  of `default-directory` when the buffer is made. A chat buffer's scope is its
  timeline's scope, never whatever `default-directory` happens to be; every
  command that needs a scope (`/new`, `/resume`, `/topic`, `/recall`,
  `/notes`, and `superchat`, `superchat-note`, `superchat-recall`,
  `superchat-ask` called from that buffer) uses that.
- `(superchat-chat-send TEXT &optional ORIGIN)`: in a chat buffer, send TEXT
  as the user's request. Sending from the input area calls this.
- `(superchat-chat-recent-tape SCOPE)` → the name of the most recently active
  timeline of SCOPE (the one with the largest entry id) that is not
  ephemeral, or nil. A timeline is ephemeral when one of its `user` entries
  has `:ephemeral t` in its data.
- `superchat-note` (an autoloaded command, any buffer): read a line in the
  minibuffer and append it as a `note` entry, with no sources and its first
  60 characters as title, to the most recent timeline of the current
  buffer's scope, or to a new timeline of that scope when there is none.
  The person writes memory the same way the model does.
- `superchat-recall` (an autoloaded command, any buffer): read a query and
  open the `search` view of the current buffer's scope in a read-only
  buffer. With an empty query, the `notes` view.
- `superchat-chat-send` runs a line that starts with `/` as a command.
- A line starting with `/` is a command. `(superchat-command-define NAME
  FUNCTION DOC)` and `(superchat-command-remove NAME)`: NAME is written
  without the slash; FUNCTION is called with the rest of the line, in the
  chat buffer. DOC begins with the synopsis of the argument when the command
  takes one (`[name]`, `query.`); `/help` shows each command as `/NAME DOC`.
  A name that is no command is not sent: the message says `Unknown command
  /NAME; /help lists them` and the input stays.

Built-in commands, and no others:

| command | does |
|---|---|
| `/new` | a new timeline in the same scope as this buffer's timeline, in a new buffer |
| `/resume` | choose a timeline from the `timelines` view with `completing-read` and open it |
| `/fork` | fork at the entry at point, or at the latest entry when point is in the input area; open the new timeline |
| `/handoff [name]` | `superchat-handoff`; refused while the timeline is busy; the mode line shows `[running Ns]` until it is over, and `C-c C-k` cancels it |
| `/topic name` | open the `topic` view of that name in a read-only buffer; no name is a `user-error` |
| `/recall [query]` | `superchat-recall` with QUERY: the `search` view, or the `notes` view when it is empty |
| `/notes` | open the `notes` view in a read-only buffer |
| `/dream` | `superchat-inbox-dream` for this buffer's scope, and says so; `/dream off` turns it off |
| `/timers` | list `superchat-inbox-timers`, one line each: id, timeline, data, text, or `No timers.`; `/timers cancel ID` cancels one |
| `/note text` | append TEXT as a `note` entry to this timeline, as `superchat-note` does |
| `/level [read-only\|ask\|auto]` | show or set the permission level |
| `/model [name]` | show or set the model for this buffer; a name that is not one of `(superchat-llm-model-ids)`, the model ids of the provider, is a `user-error` that lists them |
| `/help` | list commands |

`/new`, `/resume` and `/fork` show the conversation they open in the window
they were typed in, in place of this one, whatever `display-buffer-alist`
says of chat buffers; no other window or tab is made. The window keeps its
history of buffers, so `previous-buffer` and a window tab line go back to
the conversations it showed.

## superchat-turn.el

The text of a turn, from its entries, and the answers' Markdown as Org.
`superchat-turn-answers` is a buffer's conversions.  The chat buffer calls
`superchat-turn-seal`, `superchat-turn-dim`, `superchat-turn-cut`,
`superchat-turn-strip` and the font-lock matcher `superchat-turn-cells`.

- `(superchat-turn-text ENTRIES STREAMING OVER)` → `(HEAD . TAIL)`: the
  text of one turn.  HEAD is the `user` entry and the headings down to
  Answer, with the streamed text between them; TAIL the final answer and
  the other entries.  OVER says the timeline is free, which folds
  Progress.  A turn shows, in this order:
  - its `user` entry: `* You`, then its text, a line of it that starts with
    `*` shown with one space before it, so that it is never a heading;
  - once the turn has an `assistant` entry or streamed text, one `* Assistant`
    heading;
  - under it, Progress, when the turn has a call or an intermediate text
    (the text of an `assistant` entry other than the final answer): one
    heading `** Progress  STRIP  N calls, K failed` (`1 call`; `, K failed`
    only when K > 0; `** Progress` when it holds no call), N the calls in
    it, K those whose `tool` entry has `:error`, `⏳ ` before `Progress`
    while a call of it has no result. Under it, in order, the intermediate texts, each followed
    by its calls, each call a level 3 heading. A `permission` event of the
    turn belongs to the call it was asked for, the first call of the turn
    without a `tool` entry when the event came: it is a dimmed line just
    before that call's heading. An intermediate text is a
    level 3 heading `*** 💬 FIRST-LINE`, the first line of the converted
    text cut to 60 characters and `…`, with the whole converted text as its
    body, open unless the person folds it; a text of one line has no body.
    Progress is
    open while the turn runs and folded once the turn has its final answer
    or the timeline is no longer busy with it, unless a call of it has an
    error: then it stays open, so that the error is seen;
  - then, when the turn has a final answer or streamed text, `** Answer`,
    then the final answer: the text of the turn's last `assistant` entry
    when that entry has no calls and some text, converted from Markdown to
    Org (the Tape keeps the Markdown). Answer is never folded but by the
    person;
  - the turn's other entries (permission events but those tied to no
    call, cancelled, error, level, retry and interrupted events, notes,
    anchors), each as its
    own lines, in order.
  The strip of a turn is one cell for each of its calls and intermediate
  texts, in order: `•` in `shadow` for an intermediate text, `▮` for a
  call, in `shadow` once it has a `tool` entry, in `error` when that entry
  has `:error`, in `default` while it has none. Over 60 cells it is `…`
  and the last 59. Its colours show in the heading too, over Org's face for
  the heading.
  A call's heading is `MARK NAME  ARGS  (DURATION)` after its stars, its
  body folded: MARK is `⏳` while the call has no `tool` entry in its turn,
  `✗` when that entry has `:error`, else `✓`; ARGS is the arguments plist
  printed with `%S` without its outer parentheses, newlines escaped, cut to
  60 characters and `…` (left out with its spaces when empty); DURATION is
  `N.Ns`, the `:at` of the `tool` entry less that of the entry before it, left
  out while there is none or when that entry's text begins `Interrupted:`.
  The body is the arguments, one key and
  value per line, then the result, each in a `#+begin_example` block escaped
  with `org-escape-code-in-string`; an empty part has no block. The other
  entries look like:
  - `anchor`: `── summary: NAME ──`, then its text and, when `:next` is not
    empty, `What comes next: NEXT`; an anchor with empty text (a fork)
    shows only its line;
  - `note`: a dimmed heading `** note: TITLE`, its text the body, folded
    by default (a note often repeats the answer);
  - `event`: its text as one dimmed line, an `error` event in the `error`
    face.
  Faces are `font-lock-face`, so Org's fontification keeps them.
- The Markdown of an answer becomes Org by these rules, and no others. A
  fenced block (```` ``` ```` or `~~~` at the start of a line, closed by a
  line of the same delimiter, or by the end of the text) and a native
  `#+begin_src` block (closed by `#+end_src`) become a `#+begin_src LANG`
  block, LANG the first word of the info string; the body is kept as it is,
  escaped with `org-escape-code-in-string`, without the newline before the
  closing line; nothing else applies inside it. Inline code `` `x` `` becomes
  `=x=`, or `~x~` when x has a `=`, and nothing else applies inside it. An
  image `![alt](https://u)` becomes `[[https://u]]`; a link
  `[text](https://u)` becomes `[[https://u][text]]`, the text may hold one
  level of brackets and the url one level of parentheses, the url escaped
  with `org-link-escape`. A line of
  three or more `-`, `*` or `_` (spaces between allowed) becomes `-----`.
  `# ` to `###### ` at the start of a line become headings, demoted so that
  the shallowest heading of the final answer is level 3, under Answer, the
  others keeping their depth below it, and a heading of an intermediate
  text becomes a line of bold text, `*TITLE*`, never a heading; any other
  line starting with `#` stays text. A list item `*`, `+` or
  `-` becomes `-`. Consecutive lines starting with `>` become a
  `#+begin_quote` block without the `>`. A table (consecutive lines starting
  with `|`) loses its alignment row (cells of `-` and `:` only) for an Org
  rule `|-` and is aligned with `org-table-align`. `**x**` and `__x__`
  become `*x*`, `*x*` and `_x_` become `/x/`, `~~x~~` becomes `+x+`; x is
  one or more characters that do not start or end with a space, and `_` does
  not open or close inside a word.

## superchat-inline.el

- `superchat-rewrite` (a command): with a region, read an instruction, run on
  a new ephemeral timeline with tools `none`, and show the model's replacement in an overlay as
  it streams: the region stays in sight, dimmed and struck through, the
  replacement follows it highlighted, and a last part names the keys
  (`C-c C-k` while it streams; `C-c C-c` and `C-c C-k` once it is complete,
  said in the echo area too), and both the last part and the echo area say
  that the keys act with point on the struck-through text. Point goes to the
  region, where the keys act: the overlay's keymap `superchat-inline-map`
  binds `C-c C-c` to `superchat-inline-accept` and `C-c C-k` to
  `superchat-inline-discard`. The overlay has no other help text. Each rewrite has its own timeline, so one does not resend the
  others. The replacement is the text of the run's `assistant` entry; the
  streamed text is only shown while it comes. `C-c C-c` on the overlay
  replaces the region (one undo step) once the run is over, `C-c C-k`
  cancels the run when it is active and discards the overlay. The
  request tells the model to answer with the replacement text only; a
  fenced code block around the whole answer is unwrapped.
- `superchat-ask` (a command): read a question; the origin is the current
  buffer, and the region when active is quoted in the request. It opens the
  chat buffer of the current buffer's own conversation, as `superchat`
  finds it (a new timeline when there is none), beside the current window and sends the question
  there with `superchat-chat-send`. When that timeline is busy, the question
  joins the chat buffer's queue instead, and the message names the chat
  buffer and says the question is queued.

## superchat-plugin.el

- `(superchat-plugin-define NAME &key doc enable disable)`: NAME is a
  symbol; ENABLE and
  DISABLE are functions of no arguments. Another keyword is an error.
  Defining a name that is enabled disables the old definition and then
  enables the new one, so loading a plugin's file again keeps it on. A plugin adds tools, views and
  commands in ENABLE and removes them in DISABLE.
- `(superchat-plugin-enable NAME)`, `(superchat-plugin-disable NAME)`,
  `(superchat-plugin-enabled-p NAME)`.
- `(superchat-plugin-enable-configured)`: enable each name in the user
  option `superchat-plugins` that is not enabled. A name with no definition
  is an error that names it.

Plugins live in another repository. This one ships none.

## superchat-messages.el

- `superchat-messages`, a command: the buffer `*superchat: messages*`, in
  `superchat-messages-mode` (from `special-mode`), one line for each
  `event` of type `sent` on the Tape and each `question` that waits
  (`superchat-question`), oldest first: `YYYY-MM-DD HH:MM  TIMELINE
  FIRST-LINE`, a question's ending `  ? waits for y, n or a`, the keys in bold; `Nothing was sent.` when there is none. Point goes to the
  newest. `g` draws it again; `RET` (`superchat-messages-open`) opens the
  chat buffer of the line's timeline. On a question's line, `y`, `n` or `a`
  (`superchat-messages-answer`) sends that key from its chat buffer, draws
  the buffer again and shows the chat buffer.
