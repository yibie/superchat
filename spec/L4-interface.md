# L4 interface: the chat buffer, inline commands, plugins

Part of the [specification](../SPEC.md). Modules: `superchat-chat.el`,
`superchat-inline.el`, `superchat-plugin.el`. This layer is what the person
uses; every effect it has goes through `superchat-run` and the public
functions of the layers below.

## superchat-chat.el

One buffer per timeline, named `*superchat: TITLE*`, TITLE being the first
40 characters of the timeline's own first `user` entry (a fork's refs do
not count), or `new` until it has one. Major mode `superchat-chat-mode`,
derived from `text-mode`.

- The buffer is rendered from the `conversation` view. Text above the input
  area is read-only. Entries look like:
  - `user`: `▸ You`, then its text;
  - `assistant`: `▸ Assistant`, then its text; with no text, nothing, not
    even the header;
  - a tool call: one dimmed line `⚙ NAME(ARGS) → ` and the first line of the
    result, ARGS being the arguments plist printed with `%S` without its
    outer parentheses, newlines escaped, then cut to 60 characters and `…`
    (empty when there are none);
  - `anchor`: `── summary: NAME ──`, then its text and, when `:next` is not
    empty, `What comes next: NEXT`; an anchor with empty text (a fork)
    shows only its line;
  - `note`: a dimmed line `note: TITLE`, then its text;
  - `event`: its text as one dimmed line, an `error` event in the `error`
    face.
- The last character of what is shown passes on no text property: what is
  typed after it has no face and no entry id.
- The input area is the editable text after the last entry. In it, `RET`
  sends when point is in its last line, otherwise inserts a newline; above
  it, `RET` is an edit of read-only text and refused as any edit there is.
  `C-c C-c` always sends. `C-c C-k` cancels the run. What is sent is
  deleted from the input of the buffer it was typed in, also when the
  command it was (`/new`, `/resume`, `/fork`) shows another buffer.
- When an entry or streamed text is shown, point, and the point of every
  window showing the buffer, stays where it was when above the input area
  and moves with the input when in it.
- What is refused while the timeline is busy (sending, `/handoff`) is
  refused with a message, not an error, and the input stays. The message is
  `A request is still running; C-c C-k cancels it` during a run and `A
  handoff is still running; C-c C-k cancels it` while a handoff is pending.
- While a run is active the mode line shows `[running]`, streamed text
  appears as it comes, and the input stays editable but sending is refused
  with a message. Streamed text that never became an entry is removed when
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
- `superchat` (an autoloaded command): open the chat buffer of the most
  recent timeline of the current buffer's scope, or a new one. With a prefix
  argument, always a new one. It enables the plugins of `superchat-plugins`
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
| `/handoff [name]` | `superchat-handoff`; refused while the timeline is busy; the mode line shows `[running]` until it is over, and `C-c C-k` cancels it |
| `/topic name` | open the `topic` view of that name in a read-only buffer; no name is a `user-error` |
| `/recall [query]` | `superchat-recall` with QUERY: the `search` view, or the `notes` view when it is empty |
| `/notes` | open the `notes` view in a read-only buffer |
| `/note text` | append TEXT as a `note` entry to this timeline, as `superchat-note` does |
| `/level [read-only\|ask\|auto]` | show or set the permission level |
| `/model [name]` | show or set the model for this buffer; a name that is not one of `(superchat-llm-model-ids)`, the model ids of the provider, is a `user-error` that lists them |
| `/help` | list commands |

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
  chat buffer of `superchat-chat-recent-tape` for the scope (a new timeline
  when there is none) beside the current window and sends the question
  there with `superchat-chat-send`. When that timeline is busy, the question
  is put into the chat buffer's input area instead, and the message names
  the chat buffer and says the question was not sent.

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
