# Changelog

All notable changes to superchat.

## Unreleased — skills, models and commands anywhere in a message

- `>skill`, `@model` and `/command` no longer have to open your message:
  `please >code-review look at this` invokes the skill. In the middle of a
  message a name counts only when it exists (an existing skill, a known
  model, an existing command) and stands alone between spaces; everything
  else — `a > b`, a quoted line, an e-mail address, a path — stays text.
  What opens the message still wins, and `#file` still works at the start
  only. Messages Superchat sends by itself are read at their start only.
- An agent skill named in the middle of a message applies to that message
  only: that request carries its persona, tools and model, and the
  conversation stays in the mode it was in. Named at the start it still
  becomes the conversation's mode. A plan skill switches the conversation
  wherever it is named.

## Unreleased — working beside you

- Work that Superchat starts by itself (a scheduled task, a work record that
  wakes) no longer opens the review screen or takes the cursor; the echo area
  still says what is unsaved. Executions you start are unchanged.
- Closing a conversation's window no longer fails the execution running in
  it. It goes on, and only a question it must put to you fails.
- When a file changed on disk behind its buffer, Superchat notices before the
  agent edits: a buffer without unsaved changes is reloaded first, one with
  unsaved changes is edited and you are told that saving would overwrite the
  file. Emacs's own "changed on disk; really edit the buffer?" no longer
  appears in the middle of an edit.
- A work record opened from the list shows which executions worked on it.
- A server error in an unusual shape ends the execution with that error
  instead of leaving it waiting forever.

## Unreleased — rules reach rewrite and workflows

- Rewriting a selected region and running a workflow now follow the
  workspace's rules (`AGENTS.md`, else `CLAUDE.md`), as the chat window
  already did. A rewrite still answers with the replacement text only.
  A rule about how to answer (explain first, a line of reasoning) is said
  not to apply to a rewrite, so it no longer ends up in your file; a rule
  about the text itself (language, date format) still applies.
- When a workspace's rules file exists but cannot be read, Superchat says so
  instead of sending the request as if there were no rules.

## Unreleased — work that came due and did not start

- A scheduled task whose message could not be sent is started a few minutes
  later instead of being lost; one that came due just before Emacs quit is
  started the next time Superchat starts. It still fires once.
- When Superchat cannot tell whether a scheduled task started (Emacs quit
  while it was being sent), it says so in the conversation and does not start
  it a second time.
- A scheduled task no longer queues behind an execution that is running in
  its conversation; it waits and starts when the conversation is free.
- A scheduled task that came due and has not started yet can be cancelled.
- A work record whose wake failed is tried again after five minutes, up to
  three times, without restarting Emacs.
- A work record whose execution was cut off by Emacs quitting is marked
  `interrupted`, and the workspace's main conversation is told once. It is
  not started again.

## Unreleased — a wake that goes wrong is not lost

- When a work record's time comes and the wake cannot be sent, the record
  keeps waiting for that time instead of silently becoming an ordinary open
  record, and Superchat takes it up the next time it starts. The same holds
  when Emacs quit in the middle of a wake.
- Several records due at once: one failing no longer stops the others.
- The line in the workspace's main conversation saying a record was taken up
  is now written even when Emacs was restarted while that work ran; it is
  written once.

## Unreleased — history stays in its workspace

- The `sql` tool, which reads every conversation of every workspace, is no
  longer offered to the model by anything Superchat ships: it is out of the
  `research` preset and out of `superchat-llm-tool-names` `all`. Name `"sql"`
  in your own tool list if you want the model to have it.
- `memory_search`, `tool_history`, `file_history` and `recent_errors` read
  for the conversation whose run called them, whatever buffer you are looking
  at by then, and return an error instead of searching every conversation
  when there is none.

## Unreleased — a question you can answer later

- When the model needs an answer that can wait, it registers the wait on the
  work record and ends its turn instead of holding the conversation with a
  question panel; `ask_user` stays for a question you can answer right now.
- Your next plain message in that conversation ends the wait by itself: the
  record is continuable again before the model sees your message. Commands,
  and messages a timer or a woken record sends, do not count.

## Unreleased — work that picks itself up at its time

- A work record can wait for a time ("do the rest in two hours"): at that
  time Superchat starts a new conversation in the record's workspace, on that
  record, and the model carries on; a line in the workspace's main
  conversation says how it went. A wait that came due while Emacs was closed
  wakes once after Superchat loads.
- A record that is merely continuable is not picked up by itself: it waits
  for you.
- A sub-agent delegated while a record is current gets a snapshot of the
  record; its task and result are kept on the record.
- In a jj repository, a record's facts include the jj change each execution
  left its edits in.
- While a conversation has a current record, `timer_create` will not set a
  task timer to continue it; the model registers a wait on the record
  instead.

## Unreleased — fewer questions, fewer stops

- When `eval_elisp` is offered, the model is told that each tool call costs a
  round trip and to ask Emacs for several facts in one call; that on macOS
  and Windows a case-only rename is not an overwrite; and that a change on
  disk you asked for in so many words (rename, move, copy, delete) is your
  decision already, so it does it and says what it did. Saving a buffer it
  edited stays yours.
- The `elisp_script` tool exists but is not offered by default: on real
  tasks it made the model slower and less reliable.

## Unreleased — work records

- A piece of work that takes more than one conversation now has a work
  record: what is to be delivered, how far it got, why the next step has to
  wait, and where to pick it up. The model keeps it with the new
  `work_record` tool when you ask it to track something or the work clearly
  outlives one go; ordinary questions get none.
- At the end of every execution Superchat attaches the files that execution
  changed and decides how the record stands — completed, waiting, failed or
  continuable — from what was recorded, never from the wording of the answer.
- A request carries the conversation's current record in full and the other
  records of its workspace by status, so a new conversation, even after a
  restart, can pick the work up. Records of other workspaces never appear.
- `M-x superchat-work-list` lists the records of every workspace; `c`
  completes the one at point.
- In a temporary conversation no record is kept.

## Unreleased — the model knows how your windows are arranged

- Every chat, in-place ask and in-place do request now says how the
  selected frame is arranged: its tabs, and for each window the buffer,
  file, width and position, which is selected, which is a side window, and
  which one is this conversation. "Put the agenda on the right" no longer
  starts with the model querying your windows.
- A file the model edited outside the conversation's workspace is marked on
  `/changes` and on the review screen before you save it. It is not
  stopped.
- `timer_list` now starts with the current time and also lists the timers
  that fired in the last 24 hours, so "did I miss a reminder?" gets a right
  answer after Emacs was closed.

## Unreleased — reminders and scheduled tasks that survive a restart

- Ask "remind me in 20 minutes to call mom" or "every morning at 8, give me
  today's agenda": the model sets a Superchat timer with the new
  `timer_create` tool (`timer_list` and `timer_cancel` list and cancel them).
- A timer belongs to the conversation's workspace and is kept on the Tape,
  so it survives a restart. When it comes due you are told in the echo area
  (with the bell; see `superchat-timer-notify-function`) and a line is left
  in the workspace's main conversation; a scheduled task also starts the
  model there. One that came due while Emacs was closed fires once after
  Superchat loads, marked with the time it was due; a repeating one fires
  only its latest missed occurrence.
- The three tools are on the default tool surface and need no approval.

## Unreleased — a Personal Workspace for the agenda, feeds and chats

- `M-x superchat-set-personal-workspace` designates a directory as your
  Personal Workspace (`superchat-personal-workspace-directory`). Superchat
  called from the agenda, the calendar, elfeed, telega, mail and IRC buffers
  (`superchat-personal-workspace-modes`) then joins that workspace's
  conversation and carries its `AGENTS.md`, whatever directory the buffer
  happens to sit in. Until you designate one, a conversation called from
  those buffers is a temporary one; it no longer lands in the project you
  opened the agenda from.
- It is a workspace like any other: no other workspace sees its rules.
- To give one interface to a different workspace, set
  `superchat-buffer-workspace-directory` in its mode hook.

## Unreleased — a workspace keeps its rules, and recall stays inside it

- A conversation that belongs to a workspace (a project directory) now sends
  that directory's `AGENTS.md` — `CLAUDE.md` when there is none — with every
  request, up to 6000 characters, read from the saved file. Sub-agents it
  delegates to carry the same rules.
- Say "remember …" or "from now on …" and the model adds the rule to that
  file's buffer and tells you what it wrote; it becomes a rule when you save.
  In a temporary conversation (outside any project) the model is told nothing
  is kept, and says so.
- Automatic recall and the model's `memory_search` look only in the
  conversations of the same workspace; a temporary conversation sees only
  itself. `superchat-memory-auto-recall-cross-session` now defaults to `all`,
  meaning all of this workspace (it was `remembered`, across everything).
- A recall query in Chinese, Japanese or Korean now matches: it is cut into
  overlapping three-character pieces, which is what the index holds. Before,
  the text between two punctuation marks was searched as one word and found
  nothing.

## Unreleased — the Artifact Board gets its own name

- `superchat-workspace.el` is now `superchat-artifact-board.el`, and its
  functions and options begin with `superchat-artifact-board-`. "Workspace"
  is being given to the place work happens in (a project with its rules and
  conversations), so the shared artifact registry a lead agent and its
  sub-agents use takes the name the glossary gives it. The four commands
  (`…-add-region`, `…-set-region`, `…-clear`, `…-show`) and the two options
  (`…-fallback-buffer`, `…-auto-create`) keep their old names as obsolete
  aliases. The fallback buffer's default name is `*superchat-artifact-board*`.
  The tool names the model sees (`workspace_read` and the others) and the
  recorded event names do not change.

## Unreleased — the model is told the whole Emacs is in reach

- The Emacs operating note gains one line: the buffer a request comes from
  is where you are, not the limit of the task; your agenda, feeds, notes,
  files and repositories are in the same Emacs and are asked for before the
  model says it cannot reach them. In task runs on DeepSeek the model had
  answered "I cannot reach your agenda" without looking in 4 of 8 runs; with
  the line, in none of 11. The note is now at most 1280 characters (about
  310 tokens on each request that carries it).

## Unreleased — in-place act gets the chat window's system text

- `superchat-inline-act` requests now carry the same system instructions as
  the chat window's (tool guidance, the Emacs operating note, the reference
  pages index), as its documentation always said. Since it was added they
  had carried none.

## Unreleased — a switch for llm's "not free software" warning

- `superchat-llm-warning-message`: set it to nil and superchat's requests no
  longer trigger llm's "… API is not free software" warning, which llm
  issues on every request to such a provider and which kept bringing up the
  *Warnings* window. The default, t, leaves it to llm; llm's own
  `llm-warn-on-nonfree` and other packages' use of llm are unchanged.

## Unreleased — in-place commands join the chat's conversation

- `superchat-inline-ask`, `superchat-inline-act` and
  `superchat-rewrite-region` now go into the conversation the chat window
  shows (the one beside your file, else the main one), wherever the file is.
  The separate `inline:<project>` and `inline:global` conversations are no
  longer used (old ones still open); `superchat-inline-session-scope` is
  obsolete.
- Their requests carry only what they point at and your words, not the
  conversation's earlier turns, as Cursor's inline edit does; a follow-up in
  the same overlay carries that overlay's own earlier exchange, and the note
  that you saved or discarded a buffer it edited. (Since
  "earlier turns reach the model as real messages", such a follow-up had
  been sent without its first question; this fixes it.)
- The chat window shows each in-place turn when it ends, with a button back
  to the file and line, and later messages in the chat carry it. While the
  chat is working, an in-place request waits its turn. What an in-place act
  edited is listed by `/changes`.

## Unreleased — the review screen from a file; one reading of lookup-key

- `M-x superchat-changes`, `superchat-changes-diff`, `superchat-changes-save`
  and `superchat-changes-revert` work from a file's window: they use the
  conversation an in-place command from there would join (the chat beside
  the file, else the main one), instead of failing with a type error.
- The `emacs-keybindings` reference page now reads a number from `lookup-key`
  the way the operating note does: that map has no binding for the key.  It
  used to say the first keys were bound to a command, which is wrong when
  they are simply unbound; in an attended run a model that had read the page
  drew that wrong conclusion.

## Unreleased — the model learns what you saved

- When you save a buffer the model edited and left unsaved (with `C-x C-s`,
  the review screen's `s`, or any other way), or throw its unsaved edits
  away, the next turn starts with one line telling the model so. Before, it
  was never told, found the file changed on disk, and could make up a reason.
  A save the model itself makes with `save_buffer` is not reported as yours.

## Unreleased — one key between the two layouts; the review screen takes the cursor

- `C-c C-w` in the chat moves the conversation into its own tab, filling it,
  and back beside the file you came from; the text you had not sent stays.
  For the rest of the Emacs session the chat opens in the layout you last
  switched to; `superchat-display-style` now only sets the first one. Also
  `M-x superchat-toggle-display-style`, which from a file's window acts on
  the chat beside it.
- The review screen for unsaved changes now takes the cursor when it
  appears after a Run, so `s`/`n`/`q` work without switching to it first;
  when the review ends the cursor is back where it was. Keys typed before it
  appeared are still dropped, so they cannot save anything; if you are typing
  in the minibuffer at that moment, it waits until you are done. A key typed
  after it appears acts on it.

## Unreleased — earlier turns reach the model as real messages

- A later turn now sends the earlier ones to the model as real messages:
  each question, each tool call the model made with the arguments it sent,
  what each call returned, and each answer, in the same form as the calls
  it is making now. Before, they arrived as a transcript inside your new
  message (`User:`, `Tool:` and `Assistant:` lines under a `[Context v0 …]`
  header), and in one test run the model answered by writing tool records of
  its own instead of calling the tools.
- Your message is sent as you typed it: `superchat-general-answer-prompt`
  now defaults to `$input`. Before, every message was wrapped in "You are a
  helpful assistant. Please provide a clear and comprehensive answer to the
  user's question: …".
- The request inspector (`superchat-act` with a prefix argument) shows the
  earlier turns read-only above the text you can edit, with the line saying
  how much history was included or left out; Copy includes them. Editing the
  text no longer sends the request without its earlier turns. To leave them
  out, start a new conversation (`M-x superchat-new-session`) or branch after
  an earlier turn (`M-x superchat-branch-session`).
- New tool calls are stored with their arguments named
  (`{"buffer":"init.el"}` rather than `["init.el"]`); calls stored before
  are named by the tool's current parameters when they are sent again.
- Earlier turns take about as much room as before: on one recorded
  conversation the messages were about 5% shorter than the old transcript
  (ids and message framing added; header, labels and escaped newlines gone).

## Unreleased — the chat opens beside the file you are editing

- `M-x superchat` now shows the chat in a side window on the right of the
  tab you are in, beside the file you are editing, instead of in a new tab
  that it fills. Your windows stay open, only narrower. Starting, opening or
  branching a conversation shows it in that same window;
  `superchat-act` shows it there without taking focus, and
  `superchat-send-region` shows it there and moves you into it. The plan
  panel, when open, keeps its own place below the chat.
- Set `superchat-display-style` to `agent` for the old layout: each
  conversation in its own tab, filling it. `superchat-display-single-window`
  now applies only there.
- `M-x superchat-close-session-workspace` run in the chat window closes that
  conversation even when it has no tab of its own; if a Run is still working
  it asks first, as closing the tab does.
- Not yet: a key to switch between the two layouts. `superchat-act` and
  `superchat-send-region` still go to the main chat buffer, which is not the
  one beside the file if you opened another conversation there.

## Unreleased — the review screen for unsaved changes, in color

- When a Run leaves buffers it edited unsaved, a review screen now appears in
  another window, without taking focus: one buffer at a time, the diff of
  its file against the buffer, `s` to save it, `n` to leave it unsaved and
  see the next, `q` to stop. It is the screen the attended acceptance runs
  used, now in the product and colored (`diff-mode`; diffs.el's renderer
  with line numbers and word-level highlighting when diffs.el is installed).
  Keys only work once you switch to it, and keys typed before are dropped,
  so typing elsewhere never saves anything.
- `/changes`, `M-x superchat-changes` and `M-x superchat-changes-diff` (and
  inline act's `d`) open the same screen. Buffers that visit no file are
  listed but not reviewed. `superchat-changes-review-after-run` turns the
  automatic screen off; `superchat-changes-review-use-diffs` turns diffs.el
  off.

## Unreleased — `eval_elisp` never asks you anything

- Code the model runs through `eval_elisp` can no longer pop up a question
  for you. In an acceptance run the model "checked" a command by running it,
  which opened a "Describe function:" prompt you had to answer. Now such
  code stops at the prompt, and the model is told to look the answer up
  itself (`commandp`, `key-binding`, reading the buffer or file), or to ask
  you with `ask_user` when only you know. The approval question for an
  `eval_elisp` call is asked as before. The key-binding how-to page now also
  says to check a command with `commandp` instead of running it.
- Limit: a bare key read (`read-key`, as `describe-key` uses) is not stopped
  this way and still reaches you.

## Unreleased — a reopened conversation reads like the live one

- `superchat-open-session` rebuilds a past conversation from the database.
  It showed internal bookkeeping (Step starts and ends, request records) as
  "System" entries between your question and the answer, tool calls with
  empty "Tool call" / "Tool result" blocks, and a message you sent while the
  model was working as "Injected model material". Now the bookkeeping is left
  out, each tool block shows the arguments and the result that were stored
  (up to 2000 characters each, saying how much was cut), and a message sent
  mid-task shows its text. An answer that follows a folded entry gets its own
  Assistant heading, so it can no longer end up folded out of sight.

## Unreleased — inline act gets the finish check; the next reply replaces the last

- `superchat-inline-act` now gets the same one finish check as the chat
  window: when the model has used tools and stops on a reply that announces a
  step it did not take ("Let me also confirm…"), Superchat asks it once to do
  the step or give its final report.
- In the inline act and inline ask overlays, a reply that follows another in
  the same task (after a tool call, or after the finish check) now replaces
  the previous one on screen instead of being glued to its end ("…holds
  beta.Final report: …"). The previous reply stays visible until the next one
  starts. The chat window is unchanged.

## Unreleased — the request record includes what was added mid-task

- Superchat's record of each model request (M1 request evidence) now names
  the text a task picked up while running: a message you sent while it was
  working, an injection, and the finish-check note. Rebuilding a request
  from the database now shows that text as the model received it, and a
  later turn's rebuilt history shows it too, instead of the placeholder
  "Injected model material". Nothing the model receives has changed.

## Unreleased — the inline act overlay shows when a tool is running

- While the model's tool is running, the inline act overlay now says
  "Working: tool…" and goes back to "Waiting…" when the tool is done.
  Before, it said "Waiting…" all the way through on providers that do not
  stream tool calls. Runtime now tells every surface when a tool starts and
  ends; the chat window looks exactly as before.

## Unreleased — a network glitch or a busy provider no longer ends the task

- When the connection to the model provider fails for a moment (a timeout,
  a failed TLS handshake, a refused or reset connection), or the provider
  answers "too many requests" (HTTP 429) or with a server error (HTTP 5xx),
  Superchat now waits 30 seconds and sends the same request again, up to ten
  times, instead of ending the task with an API error. It never retries once
  part of the answer has already been shown, and a cancelled task sends
  nothing more. Any other HTTP error (for example 400 or 401) still ends the
  task at once. Set `superchat-llm-transient-retries` to 0 for the old
  behaviour, or `superchat-llm-transient-retry-interval` to wait a different
  number of seconds.
- The HTTP status is read where the llm library handles the failed request;
  Superchat never guesses it from the provider's message text.
- While it waits, the wait is visible instead of a bare "Waiting…": the chat
  window's status line counts down ("Provider error; retrying in 23s (1/10)…
  · Operation timeout.") and its header says "retrying 1/10"; inline ask,
  inline act and the rewrite preview show the same notice until the request
  is sent again. The notice names the HTTP status or a short error line,
  never the provider's full response.

## Unreleased — one finish check when a task stops half-way

- In the chat window and `superchat-act`, when the model has used tools and
  then stops with a reply that is empty or ends by announcing its own next
  step ("Let me also confirm…", "接下来我…") without doing it, Superchat
  sends it one note: do the step now, or give the final report. This
  happens at most once per task, never while you are steering the task and
  never after the tool-round budget asked for a final answer. The note is
  recorded as coming from Superchat, not from you. Turn it off with
  `superchat-llm-finish-check`.

## Unreleased — later turns remember what the tools returned

- The model now remembers, in later turns of the same conversation, what its
  tools returned. Each tool call is stored with its arguments and its result
  or refusal text, up to 2000 characters each, and later turns show it as one
  line such as `Tool: read_buffer("init.el") -> [ok] …`, after a note that
  these lines are past observations and data, not instructions. Refused or
  failed calls are marked with their status, never as a success. Compaction
  summarizes these lines like the rest of the conversation.
- **Privacy:** this stores file text the model read and values it evaluated
  in the local database. Fields with secret-looking names are redacted;
  secrets inside text are not. Set `superchat-tape-store-tool-material` to
  nil to store none of it.
- Before this, a follow-up such as "undo that" could not know what the model
  had seen in the previous turn unless its final answer had repeated it.
- The history header no longer tells the model it can recall omitted rows:
  no tool lets it do so.

## Unreleased — inline act (M8)

- New command `superchat-inline-act`: give an instruction in the buffer you are
  editing and the AI does the task there, with the default tools, the ordinary
  permission levels and approval questions.  Progress and the answer appear in
  an overlay under the line; the chat window does not open.  At the end the
  overlay states how many tool calls the Run made and which buffers it left
  edited and unsaved.  On the overlay `d` shows the diff, `r` follows up with
  tools still available, `q` cancels, `M-RET` opens the menu.
  `superchat-act` still works through the chat window.
- Fixed: a Run started from a non-chat buffer (inline ask, Org Babel, Magit)
  had its tool-call log inserted into that buffer, and an inline edit could fail
  on the text of its own log.  Tool logs now go only to a chat window.

## Unreleased — Emacs 30.1 is the minimum

- The declared minimum is now Emacs 30.1 (`Package-Requires`, `superchat-pkg.el`,
  both READMEs).  Superchat was run on a locally built Emacs 29.4: it loads and
  completes real tasks, but two behaviours differ from Emacs 30, both because an
  Emacs function changed its behaviour rather than its name.  `copy-tree` does
  not copy records on 29, so recording request evidence rewrote the live prompt
  (the system text moved into a system message); and an approval question asked
  from a provider callback could not be cancelled with C-g on 29 (you had to
  answer `n`).  Rather than carry fixes for a version nobody here runs, the
  floor moves to the version the code is written and tested against.
- Correction, after running it on a locally built Emacs 30.1: the C-g problem
  was not Emacs 29's alone. Emacs 30 also reads that answer key by key, so on
  30.1 too C-g could not cancel an approval question; only `n` worked. Fixed:
  C-g now cancels the question on every supported Emacs. Everything else,
  including a real DeepSeek chat session, passed on 30.1 unchanged.

## Unreleased — reference pages read on demand

- New default tool `read_reference` and a short index of Superchat's bundled
  reference pages in the system text (present only when that tool is on the
  request's final tool surface).  The model can fetch a how-to page in the
  middle of a task without changing its role, tools or permissions; the first
  page is `emacs-keybindings`.  A page is advice: it stands below your
  instructions and the permission decisions, never tells the model to save,
  and nothing in it is run.  The tool takes an id, not a path, and does not
  touch the file system; no permission level asks about it.  When
  `eval_elisp` fails with an error a page is the known answer to (a keymap
  variable that does not exist, a key sequence that starts with a non-prefix
  key), the product's failure feedback names the page once; the error text
  itself is unchanged.  If you set `superchat-llm-tool-names` yourself, add
  `"read_reference"` to get it.  **Only the mechanism is delivered; whether
  real models read a page when they should and do better for it has not been
  verified.**
- Fixed: a request from the current-buffer entry (`superchat-act`) carried
  the Emacs operating note even when your own `superchat-llm-tool-names` held
  neither `eval_elisp` nor `edit_buffer`.

## Unreleased — Emacs operating note

- Requests that can change Emacs now carry a short operating note in the system
  text (at most 1024 characters, about 250 tokens): ask the running Emacs
  instead of guessing, write keys with `kbd`, what a number from `lookup-key`
  means, that an edit stays in the buffer so `load FILE` still reads the old
  text, that a change is made live by evaluating only the changed forms and not
  the whole buffer, that `eval_elisp` takes one expression, how to undo, and to
  report only what was checked -- the buffer edited, whether it is unsaved,
  saving is the user's.  It was written from mistakes models
  repeated in the attended runs of 2026-09-18 to 20.  It appears only when
  `eval_elisp` or `edit_buffer` is on the request's final tool surface.
  `superchat-emacs-operating-note`: `t` (default), `nil` to turn it off, or a
  string to send your own text instead.  **Its effect on real models has not
  been verified**; the reference carrier and `read_reference` (M5 slice 3)
  remain a proposal.

## Unreleased — renamed module

- `superchat-authority.el` is now `superchat-tool-execution.el`, and every
  symbol it defines changed prefix with it (`superchat-authority-…` →
  `superchat-tool-execution-…`; the feature and the customization group too).
  The file has held only the tool execution pipeline since M6.  No
  compatibility aliases: the module had no user options and nothing outside the
  repository used its symbols.  `superchat-tool-execute` keeps its name.

## Unreleased — fixes

- Installing with `package.el` no longer reports a byte-compile error in the
  bundled Telega plugin (`Cannot open load file:
  superchat-cowork-telega-legacy`): the plugin's session file now finds its
  sibling when it is compiled on its own.  `superchat-pkg.el` declares the same
  requirements as the `superchat.el` header (Emacs 29.1, llm 0.31.1).
- Removed the user option `superchat-completion-check-delay`.  Nothing read it;
  a configuration that sets it is unaffected.

- `superchat-response-timeout-hard-limit` now defaults to nil: there is no
  cumulative 600-second limit on a tool conversation any more.  A Run is still
  bounded by the per-attempt inactivity timeout, the tool round budget and the
  Run deadline (1800 s).  Under
  the default permission level the time spent reading approval questions could
  use the old limit up and end the Run with a question still open.  Set a
  positive integer to get the old behaviour back.  The option and its clock
  machinery remain in the code; removing them is a separate item.
- The Run deadline (`superchat-runtime-default-deadline`, 1800 s) does not run
  while an approval question is open, or while an `ask_user` question from the
  model is waiting in the chat window: the question waits for you, your answer
  always counts, and afterwards the deadline continues with what was left.  A
  Run without a question, and an unattended Run, are bounded as before.
- An approval question no longer outlives its Run.  When the Run ends while a
  question is open -- it was cancelled elsewhere, it failed -- the
  question is dismissed without a key press, the user is told that the call was
  not run, and the decision is recorded as cancelled because the Run ended
  (distinct from the user's own `C-g`).  Before, the question stayed on screen
  and answering it did nothing.

- `/changes` and the Run-end reminder recognize the buffer a tool edited by a
  mark on the buffer itself, not by the name and file recorded at the time.
  Before, after the edited buffer was renamed, an unrelated buffer that took
  its old name was listed and the real one was missed.  The mark is a
  buffer-local value set by the editing tool: it follows the buffer through a
  rename, `set-visited-file-name` and a major-mode change, is not inherited by
  a clone, and goes away with the buffer -- so a buffer that was killed and
  visited again is no longer listed.  Nothing is added to the Tape or to disk.

- `eval_elisp`'s description no longer tells the model to persist.  It still
  carried an M5-era sentence -- "For persistence, save the actual loaded
  configuration and verify in a fresh Emacs that loads it" -- the opposite of
  what `edit_buffer` and the runtime context say under buffer-first mutation,
  and three attended runs saved through `eval_elisp` and spawned a fresh Emacs
  exactly as it asked.  It now says the tool is not for saving files, that
  saving is the user's decision, and that a lasting configuration change is
  made in the buffer of the file Emacs actually loads.

- An approval question can only be answered by input that arrives after it is
  asked.  The default `y`/`n`/`?` question did not clear the input queue, so a
  key typed earlier -- a second `y` meant for the previous question, or text
  being typed in another buffer -- approved a full-control call unseen (measured:
  0.004 s).  Pending input is now discarded before each question, the way Emacs
  does before its own dangerous questions; `C-g` still quits; and while a
  keyboard macro is running the default answerer counts as nobody (the call is
  refused as having no approval provider, and the macro's keys are not read).
  Discarding input also ends a keyboard macro that is being defined.  This keeps
  old input from approving; it does not prove the arguments were read.

- `edit_buffer`, `append_to_buffer` and `replace_buffer` rehearse their change
  on a private copy when the target is a Lisp buffer (a mode derived from
  `lisp-data-mode`).  If the buffer reads as Lisp before the edit and would
  not after it -- an unbalanced parenthesis or an unterminated string, the
  test `check-parens` applies -- the call fails with an `Error:` result that
  names the kind of mismatch, the line and column and the offending line, and
  the buffer is left exactly as it was (text, modified mark, undo list, change
  hooks and lock file untouched; nothing is attributed to the call).  The
  model sees the error in the same Run and submits the edit again.  A buffer
  that was already unbalanced is edited as before and told whether it reads
  now; other modes are not checked; a correct edit answers word for word as
  before.  An edit that only balances after a second call is refused.

- An open approval question no longer counts as the model making no progress.
  The provider attempt's inactivity timer (`superchat-response-timeout`, 180 s
  for a tool attempt by default) kept running while Superchat waited for the
  user's `y`/`n`, and was renewed only after the answer: reading the code of an
  `eval_elisp` call for more than three minutes failed the Run under the open
  question.  The timer now stops while somebody is being asked and continues
  from what remained after the decision.  A Run cancelled during the wait is
  still not revived by a late yes, and provider silence with nobody waiting
  times out as before.  The cumulative limit
  (`superchat-response-timeout-hard-limit`) is unchanged.

## Unreleased — buffer-first mutation (M7)

- New default tool `open_file`: brings a file into Emacs as a buffer (a real
  visit -- major mode, hooks, file-local variables; Emacs's own questions are
  not Superchat approvals), takes no focus, changes nothing on disk, and gives
  an empty buffer for a path that does not exist.
- Unsaved-changes outlet: a message at the end of a Run that left edited
  buffers unsaved, the `/changes` command, and `M-x superchat-changes-diff` /
  `-save` / `-revert` (user operations).  Derived from the Tape and the
  buffers' modified marks; `tool.completed` of a structured buffer edit now
  names its target buffer.  Edits made through `eval_elisp` are not tracked.
- **Breaking:** the model's ordinary edits now end in the buffer, and saving
  is the user's.  `write_file` is removed.  `save_buffer` is no longer a
  default tool; a Preset or Request that lists it gets a tool that asks the
  user on every call and is refused where nobody can be asked (`read-only`,
  or the approval policy `never`, which `danger-full-access` uses).
- **Breaking:** the write sandbox is removed: no writable roots, no target
  containment, no `sandbox_permissions` / `justification` arguments, no
  one-shot escalation.  An ordinary buffer is editable wherever its file
  lives; Superchat's own buffers never are; `read-only` refuses every buffer
  edit, with nothing to retry.  A user's `super-save`, `auto-save-visited-mode`
  or format-on-save may still persist an edited buffer -- Superchat's default
  modification path performs no persistence and does not fight the
  environment.
- The default permission level is renamed `edit-buffers`.  `workspace-write`
  is still accepted in `superchat-sandbox-default-mode`, in `/permissions` and
  in old Tape rows, and is never written again.
- Default built-in tools are now eight: `list_buffers`, `apropos`,
  `describe-function`, `read_buffer`, `read_file`, `open_file`, `edit_buffer`,
  `eval_elisp`.

## Unreleased — `read_file`

- New default tool `read_file`: reads a local text file from disk without
  opening a buffer (no major mode, file-local variables or `find-file-hook`),
  at most 2000 lines / 2000 characters per line / 50 KiB per call, with a
  header that names the resolved path, the lines returned and every
  truncation, and text without line numbers.  It always returns the disk
  version and names a buffer that holds unsaved changes.  Directories,
  devices, FIFOs, binary files, remote paths and files over 20 MiB are errors.
- Reading is the same under all three permission levels and asks nobody:
  what can be read is decided by the operating-system permissions of the
  Emacs process, there is no sensitive-path list, and what is read is sent to
  the model provider.  An explicit tool list or Preset is not widened.

## Unreleased — permission levels (M6)

(Superseded in part by M7 above before any release: `write_file`, the write
target checks and the one-shot escalation listed here no longer exist, and
`workspace-write` is now called `edit-buffers`.)

- **The default experience changed.** Tool calls are no longer decided by
  Authority's effect policies but by the Session's permission level
  (`/permissions`): `workspace-write` (default), `danger-full-access`,
  `read-only`.  By default reads, and edits, saves and new files inside the
  Session's workspace and the temporary directory, ask nothing; `eval_elisp`,
  shell commands, `define_tool`, agent-written tools and MCP / third-party
  tools ask on every call.  Reads of "sensitive" buffers no longer ask.
- A write the level does not allow is answered with a denial the model can
  retry once for that one call, stating the wider level it wants and why; you
  are asked (`y`/`n`/`?` for the full arguments) and the answer covers exactly
  those arguments.  "Look, don't touch" is `/permissions read-only`; an
  absolute ban also sets the Session's approval policy to `never`.
- New default tools `save_buffer` and `write_file`; `superchat-mcp-trusted-servers`
  lists MCP servers whose tools run unasked.
- Approvals are answered through `superchat-approval-function`
  (`approval/request` Event); plugins decide with a `tools/pre-execute`
  listener and can wrap or filter a call with `tools/execute` /
  `tools/post-execute`.  The Telega plugin uses these per request.
- Going back is not a setting: restart Emacs on the commit before the
  cutover (`58d54615`).
- **Removed with Authority's decision code:** the `/authority` command and the
  `authority.policy.selected` Session setting (old Tape rows stay as inert
  history); `superchat-authority-approval-function`,
  `superchat-authority-unprompted-read-tools`,
  `superchat-authority-sensitive-buffers`,
  `superchat-authority-prompt-for-all-reads`, `superchat-authority-mode` /
  `superchat-authority-set-mode` and the `SChat[ask]` mode-line segment;
  `superchat-agent-confirm-destructive`, `superchat-agent-destructive-tools`,
  `superchat-agent-confirm-synthesized`; `superchat-mcp-server-effects` (use
  `superchat-mcp-trusted-servers`); the `:effects` and `:approval-preview`
  arguments of `superchat-register-tool`; the `superchat-authority-register-*`
  functions and the `authority` Service / plugin; the Request keys
  `:authority-policy`, `:attendance` and `:approval-function`; the Events
  `tool-permission`, `tool-args-rewrite` and `tool-result-filter` (use
  `tools/pre-execute` and `tools/post-execute`).  A Preset's
  `confirm_destructive` field is still parsed and no longer consulted.
  `/reset` keeps the sandbox mode and approval policy (it still unbinds the
  Workspace, so the workspace root of the next request may change).  A Session
  whose approval policy is `never` -- `danger-full-access` by default, and
  every delegated child -- is not offered `ask_user`.
- The approval question's `?` shows the tool, the reason and the whole
  arguments through `read-multiple-choice`'s own help, in both of its input
  paths; a `?` choice of our own was shadowed by that function's help key, so
  the code of an `eval_elisp` call could not be inspected before answering.

## v1.4.0 — core simplification

- One turn pipeline for every surface: chat, inline ask, region rewrite,
  sub-agents, skills, workflows, compaction and the Telega plugin submit
  Request envelopes through `superchat-pipeline-submit`; output reaches the
  user only through the envelope's Sink (`docs/design/turn-pipeline-contract.md`).
- Inline ask (`superchat-inline-ask`): answer in an overlay, reference
  cycling (`C-c SPC`), reply and visit; one Session per project.
- Region rewrite previews asynchronously in an overlay: accept as one undo
  step, reject, refine, diff, merge as conflict markers, per-segment
  rectangle previews.  The source buffer is untouched until accepted.
- Messages sent during a running answer are queued and sent automatically;
  the manual `C-c C-q` draft command was removed.
- Steering, follow-up queue, cascade cancellation of sub-agents, regenerate /
  edit-and-resend, reopen with full history, cross-session memory recall,
  automatic compaction on context overflow (with a real fix for very long
  conversations), one collapsible heading per tool call.
- Tool selection is one decision: the configured built-ins plus every
  plugin-registered tool, Authority still gating every call; only the
  user-owned `superchat-authority-unprompted-read-tools` read without asking.
- The `experiments/` snapshots and the ERT test suite were removed from the
  repository.
- **Plugin architecture v2** (`docs/design/plugin-architecture.md`): a
  Kernel of plugins/Scopes, Services, Stages and Events; every built-in is a
  default plugin in `superchat-plugins`; one prompt path through the driver
  for every surface; the hook-list variables, `superchat-define-extension`
  and the legacy Runtime request bridge are gone; Telega is the reference
  plugin; `scripts/smoke.el` + `scripts/kernel-check.el` replace the test
  suite as the migration's verification. Every driver Run records a
  `prompt.assembled` receipt; Kernel Invariants record `invariant.violated`
  when a seam rule breaks.
- Removed dead code: unregistered path-based file tools, implicit skill
  matching, the standard-SKILL export module, the explicit Recall API.

## Unreleased (v1.3.2)

### Post-review hardening

- Classified `claim.recorded` as explicitly irrelevant to Closure Contract v0
  Run/Turn readiness, preventing an explicit Claim from making later Runs
  `audit-incomplete`.
- Restricted Claim basis and supersession references to the Claim's Session;
  replacements must also target an existing Claim in the same scope.
- Added the Tape material-content seam and migrated messages, anchors,
  compaction, and Memory migration away from direct DB writes.
- Kept generated experiment databases and archives, plus local tool settings,
  outside Git while retaining their expected paths and SHA-256 digests.

### Closure terminal guarding

- Froze Closure Contract v0 as the Runtime lifecycle safety kernel.  Future
  product work moves to long-running Agent capability rather than expanding
  Closure into a general world or knowledge model.
- Added a minimal semantic environment boundary for event schema and Authority
  policy identity.  Missing, unsupported, or inconsistent policy versions can
  no longer produce a proved lifecycle judgment.
- Tightened Effect failure transitions so pre-execution `blocked`/`denied`,
  approval cancellation, and post-start execution failure cannot be confused.
- Enabled the Tape-derived Closure Guard by default before Runtime writes a
  Run terminal seal.  Only an exactly matching `proved` readiness judgment may
  proceed; open, unknown, invalid, or replay-failure outcomes leave the live
  Run `audit-incomplete` without inventing a durable failure seal.
- Kept Closure Shadow comparison opt-in.  Guard decisions remain available in
  the bounded process-local observation ledger without paying for a second
  replay on every normal Run.
- Added a fresh-load default regression test and a rerunnable live canary that
  covers normal completion, Tool approval and failure paths, cancellation with
  an active Effect, timeout identity, and cross-Run isolation.
- Set `superchat-closure-terminal-guard-enabled` to nil to opt out.  This affects
  only later Runs and never rewrites Tape or backfills a missing terminal seal.

### Phase 3 generative UI

- Added an optional `vui.el` sub-agent panel for `/agents`, with lifecycle-driven
  refreshes and guarded Cancel buttons; the existing text summary remains the
  fallback when VUI is unavailable.
- Added the pure generated-UI whitelist gate and red-team coverage. Agent
  forms may use only approved VUI constructors, literals, and registered
  action forms; callbacks are expanded only after validation.
- Added the opt-in `render_ui` tool, which mounts validated trees in a separate
  buffer and advertises its constructor/action vocabulary only when enabled.

### v1.3.2 tool-loop repair

- Fixed the P0 tool-loop gap: llm.el runs a requested tool and appends the
  result to the mutable chat prompt, but the caller must send that prompt
  again. The asynchronous chat, synchronous executor, and asynchronous
  sub-agent paths now do so until the model returns text.
- Added `superchat-llm-max-tool-rounds` (default 12) alongside the existing
  total tool-call guard. At the limit, `superchat-llm-round-limit-action`
  can ask for another budget, send one final request with tools removed, or
  stop. Batch use automatically takes the tool-free final-answer path.
- Tool call/result transcript blocks remain in the chat buffer while the
  final streamed answer is formatted. Sub-agent cancellation follows the
  newest llm.el request, and its timeout pauses while a round-limit prompt is
  awaiting the user's answer.
- Raised the declared runtime floor to Emacs 29.1 (SQLite already required
  it) and llm.el 0.31.1.

### v1.3.1 control plane

- `/agents` lists active sub-agents with id, preset, depth, elapsed time,
  and progress-placeholder position. `/cancel-agent <id>` cancels one run.
- Sub-agent requests retain the object returned by `llm-chat-async` and cancel
  through llm.el's supported `llm-cancel-request` API. Completion, failure,
  manual cancellation, and timeout share one finish-once path.
- `superchat-subagent-timeout` bounds all runs (default 300 seconds); agent
  frontmatter may set a shorter per-agent `timeout`, preserved by import/export.
- Workflow steps now opt into the existing agent-loop tool wrappers, gaining
  the same call counting, confirmation, lifecycle hooks, rendering, and tape
  behavior as normal agent turns.
- `eval-elisp` is now destructive by default. Introspector and custom agent
  skills must pass the same confirmation/permission gate before evaluating
  arbitrary Elisp, preventing a shared skill from disabling global guardrails.

### Agent registry and profiles

- Delegation tool descriptions are generated from the live agent registry:
  the three built-ins remain first, and custom `type: agent` SKILL.md files
  are discoverable by name and description without editing Lisp. Prompt and
  workflow skills are excluded from delegation.
- Agent presets now carry typed `temperature`, `max_tokens`, and `reasoning`
  settings through main-agent, synchronous sub-agent, and asynchronous
  sub-agent calls to `llm-make-chat-prompt`. Invalid values warn and inherit.
- Per-agent `max_tool_calls` and `confirm_destructive` guardrails apply to
  both main and delegated agents. They are intentionally tighten-only:
  effective limits use `min(global, profile)` and confirmation uses
  `global OR profile`, so a shared SKILL.md cannot weaken global policy.
- Legacy synchronous sub-agent runs now receive an isolated buffer-local
  tool-call counter instead of sharing state with the caller.

### Preset runtime contract (agent-profiles Phase 1)

- **System prompts now actually reach the LLM.** `turn.system-prompt`
  was a dead-end slot: hooks populated it (language instruction, tool
  guidance) but nothing sent it to the provider. The pipeline is now
  wired end to end — `superchat--build-llm-prompt` gained a `:context`
  parameter, both `superchat--llm-generate-answer` variants accept a
  system prompt, and the dispatcher, agent runs, and both sub-agent
  paths pass it through.
- **Agent/plan preset bodies are personas.** `superchat-preset-apply`
  prepends the skill body to the turn's system prompt for `type: agent`
  and `type: plan` presets, so the persona reaches every turn —
  including `/agent`-mode follow-ups and delegated sub-agents, which
  previously never saw it. Explicit `>skill` invocation no longer also
  embeds the body in the user prompt (it would have been sent twice).
- **Preset models reach the effective backend.**
  `superchat--execute-llm-query` falls back to the turn's target-model,
  fixing both the dispatcher ordering bug (model was captured before
  preset application) and the sub-agent path (which calls with no
  explicit model). Precedence: an explicit per-turn `@model` wins;
  the preset fills the slot only when empty.
- **`tools: []` means zero tools.** An explicitly empty tools list in
  SKILL.md frontmatter now parses to a `none` sentinel (absent key
  still inherits the global tool set) and propagates through preset →
  turn → tool collection.
- **`:backend` preset field removed.** It parsed and stored but had no
  runtime consumer; a `backend:` key in frontmatter is now ignored.
- Tests: 211 → 228; new `test-preset-contract.el` locks all of the
  above end to end.

## 1.2.1 (2026-07-12, untagged)

### Async sub-agent engine

- `superchat--subagent-run-async`: sub-agents now run through async
  llm.el callbacks — Emacs stays interactive during delegation. The
  sync runner is kept as a legacy path.
- `superchat--subagent-run-parallel-async` replaces the `make-thread`
  implementation: requests run as concurrent llm.el calls (separate
  curl processes), so parallel delegation is genuinely parallel;
  `superchat-subagent-parallel-max` is now a launch window with
  queueing. Results aggregate in spec order regardless of completion
  order.
- Explicit execution context replaces dynamic bindings (which don't
  survive async callbacks): session id, depth, and the tool-call
  counter travel through closures. Sub-agent tool calls are gated by
  the same permission hooks / confirmation as the main agent loop and
  logged to the sub-agent's own tape; tool errors return as strings
  instead of re-signaling.
- Delegation depth guard: `superchat-subagent-max-depth` (default 1) —
  at the limit, delegate tools degrade to a denial stub; below it,
  nested delegation re-enters at depth + 1.
- Progress placeholders: `/subagent` and the delegate tools insert a
  "⏳ running…" line immediately and replace it in place with the
  report on completion; concurrent placeholders coexist via markers.
- `delegate_to_subagent` / `delegate_to_subagent_parallel` are now
  registered as async llm.el tools; `/subagent` delegates without
  blocking.
- Tests: 202 → 211 (async runner, spec-order aggregation, launch
  window, error isolation, depth guard, placeholder lifecycle).

## 1.2.0 (2026-07-12)

### Workflow engine (rewritten)

- Standalone `.workflow` files restored under `<data-dir>/workflow/`, with a
  fully **asynchronous** linear executor — steps chain through LLM completion
  callbacks, Emacs never blocks. Replaces the v0.7 SKILL.md `type: workflow`
  placeholder that never actually called the LLM.
- New `>>name [args]` prefix invokes workflows (`>` stays reserved for
  skills); `/workflow <name> [args]` is an equivalent entry point.
- Cross-step variables: `$result` (previous step), `$stepN` (any step),
  plus `$input` / `$lang` / `$date`.
- Per-step annotations: `@model` one-shot override (line start),
  `/command` expands the command's prompt template with args bound to
  `$input` (line start), `#path` context files (token must contain `.`
  or `/`; resolved once at invocation time).
- LLM error responses stop the chain with a rendered failure notice
  instead of leaking into the next step's `$result`.
- Sample recipe: `examples/research.workflow`.

### Shared workspace (new)

- `superchat-workspace.el`: designate a highlighted region as a shared
  state area (`superchat-workspace-set-region`); markers track buffer
  edits. Falls back to an auto-created `*superchat-workspace*` buffer.
- Three new LLM tools: `workspace_read`, `workspace_write`,
  `workspace_info` — main agent and delegated sub-agents coordinate
  through them.

### Agent mode

- Per-tool lifecycle hooks: `superchat-agent-pre-tool-functions`,
  `superchat-agent-permission-functions` (programmatic allow/deny gate,
  deny wins), `superchat-agent-post-tool-functions`,
  `superchat-agent-post-tool-failure-functions`.

### tape.systems unification + sub-agents (untagged v1.1 block, 2026-06/07)

- Tape schema v3: FTS5 trigram index, `topic` column, View layer.
- `/remember`, `/recall`, `/compact`, `/expand` rebuilt on tape anchors.
- SQL + structured tape retrieval tools; topic lifecycle hooks.
- Agent loop with tool observability and guardrails; session compaction.
- Sub-agent presets (researcher / executor / introspector) and
  `delegate_to_subagent` / `delegate_to_subagent_parallel` tools.

### Fixes

- DB schema: migrations now run before creating indexes/FTS tables that
  depend on migrated columns — v2 databases previously aborted the upgrade
  with "no such column: topic" and were left without `tape_fts`.
  `_schema_version` uses DELETE+INSERT to keep a single authoritative row.
- `ob-superchat` / `superchat-magit` / `superchat-rewrite`: system prompts
  go through llm.el's `:context` instead of overwriting `interactions`
  with a plain plist (which dropped the user message).
- Workflow parser: inline prose (`@john.doe`, `#42`, `foo/bar`) is no
  longer eaten as annotations; `$step10` is no longer corrupted by the
  `$step1` substitution.
- Version metadata: `;; Version:` header and the stale
  `superchat-version` defconst (was "0.5") both bumped to 1.2.0.

### Tests

- Suite: 179 → 198. `test-workspace.el` wired into `run-tests.el`
  (previously never ran); the always-true async error test replaced with
  real sync-error and error-string chain tests; new regressions for
  `$step10`, inline annotation parsing, command template expansion, and
  workspace replace semantics.

## 1.0.1 (2026-06)

### Documentation

- **README rewrite**: 569→296 lines. Replaced gptel/org-ql era content with
  hook-pipeline positioning, comparison table, 5-minute tour, Core Concepts,
  and honest limitations section.
- **README_cn.md rewrite**: 557→296 lines. Same structure, native Chinese.
- **`docs/architecture.md`**: new 326-line technical companion covering
  pipeline, turn struct, hook signatures, turn lifecycle, dispatcher,
  command/skill extension, and non-pluggable gaps.
- **`docs/SKILLS_QUICKSTART.md`**: updated to canonical SKILL.md format
  with `type: workflow` section.
- **3 new example skills**: `explain-region` (type: prompt),
  `git-commit-message` (type: workflow), `weekly-tech-digest` (type: workflow).
- **AGENTS.md**: fixed stale facts (file count 17, llm 0.24, SQLite memory,
  gptel migration complete).

## 1.0.0 (2026-06)

Tagged. `superchat-pkg.el` created.

## 0.9 (2026-06) — Stabilization pass

### Monolith split

- `superchat.el` split from 2589→~1548 lines into 6 focused modules:
  `superchat-models.el`, `superchat-save.el`, `superchat-mcp.el`,
  `superchat-render.el`, `superchat-llm.el`, `superchat-dispatcher.el`.

### Hook pipeline alignment

- `superchat--build-final-prompt` deleted. Replaced by 5 focused hook functions
  in `superchat-prompt-hooks.el`.
- Prompt building now runs exclusively through `superchat-core-run-turn` hooks
  (system-prompt → build-prompt → post-turn).

### Infrastructure

- **Autoload coverage**: 13 `;;;###autoload` across 10 files.
- **CI**: `.github/workflows/test.yml` added.
- **llm minimum**: bumped to `0.24` (was `0.7`).
- **Working-tree hygiene**: removed `superchat.el.bak` and `:memory:`.
- **`.gitignore`**: reconciled bogus entries; tracked 19 hidden docs/test files.

### Known gaps (deferred to v1.1)

- `superchat-ollama-timeout-multiplier`: `defvaralias` false positive in `:type` check.
- `checkdoc` and `package-lint` never run.

## 0.8 (2026-06-04) — SQLite memory facade

- **The pivot**: replaced the Org-mode memory store with a thin compatibility
  facade over `superchat-db.el` (SQLite + FTS5).
- Rewrote `superchat-memory.el` as a facade over `superchat-db` (2516 → 337 lines).
- Rewrote test suite: deleted org-ql tests (16), audited soul tests (33 → 8),
  expanded facade tests (2 → 13).
- One-shot migration `M-x superchat-memory-import-from-org`.
- Soul.org kept as dual-track historical archive (not actively written).
- LLM keyword enrichment deleted (FTS5 handles most recall).
- `superchat-memory-synthesize-soul` retired.

### MCP v2 — deferred

The planned MCP v2 multi-server orchestration was deferred to v1.1.
Current `superchat-mcp.el` (109 lines) only handles basic start/stop/tool collection.

## 0.7 (2026-06) — Skills v2: standard format + workflow restoration

### SKILL.md format unification

- In-repo `skills/*.md` files migrated to canonical SKILL.md frontmatter
  (`name`/`description`/`version`/`type`/`triggers`).
- `superchat-skills-standard--parse-frontmatter` with graceful missing-field
  handling.

### Workflow restoration (`type: workflow`)

- `superchat-workflow.el` revived as step-by-step executor.
- SKILL.md `type: workflow` dispatches to step executor rather than single
  LLM prompt.
- Legacy `.workflow` file import shim (auto-synthesize SKILL.md on load).

## 0.6 (2026-06-01) — Memory-Soul dual-track

- **Memory-Soul separation**: two distinct tracks — synthesized `memory.org`
  (knowledge, scored, decayed) and raw-event `soul.org` (verbatim, mood,
  context, never auto-merged).
- **`superchat-memory-add-raw`**: with `:mood`, `:context`, `:verbatim`
  keyword args. No LLM required.
- **Contradiction coexistence**: `:CONTRADICTION:` tag, `:VALIDITY: expired`,
  `:REPLACED_BY:` link. Contradictions kept, not merged.
- **Manual review UI**: `superchat-memory-review-mode` (y/n/e/s/q keybindings).
- **Mood tag taxonomy**: `defcustom superchat-memory-mood-taxonomy`.
- **New defcustoms**: `superchat-memory-soul-file`,
  `superchat-memory-soul-synthesis-mode`,
  `superchat-memory-contradiction-context-window`,
  `superchat-memory-mood-taxonomy`.
- **23 new tests** in `test/test-memory-soul.el`.

## 0.5 (2026-05) — Backend hard-swap: gptel → llm.el

- **BREAKING**: gptel replaced by [llm.el](https://github.com/ahyatt/llm) (GNU ELPA, 0.24).
- **Emacs 28.1 required** (was 27.1).
- **Removed** `superchat-agent.el`.
- **Removed** gptel-specific `:around` advice on `gptel-curl--stream-cleanup`.
- **New defcustoms**: `superchat-llm-backend`, `superchat-llm-model`,
  `superchat-llm-streaming`, `superchat-manual-models`.
- **New `/backend` command** (replaces `/tools`; alias kept).
- **Built-in tool registry**: 13 tools via `llm-make-tool`.
- Test suite rewritten: 9 gptel test files deleted; new `test/test-llm-backend.el`.

## 0.4 (2025-10-13)

- **Workflow Integration**: workflows run inside superchat, sharing the tool stack.
- **Tool Output Hardening**: sanitized workflow results for rich Markdown/HTML.
- **Utility Additions**: `superchat-version` constant, expanded tool tests.

## 0.3 (2025-10-03)

- **MCP Integration**: zero-config MCP server detection, real-time status,
  tool integration via `/mcp` and `/mcp-start` commands.
- **gptel Tools Integration**: zero-config tool calling via gptel's registry.
- **@ Model Switching**: switch models mid-conversation with `@model` syntax.
  `/models` command for available model list.

## 0.2 (2025-09-23)

- **Memory System**: persistent AI memory with tiered capture, LLM-powered
  keyword extraction, scoring/decay, and auto-merge of similar memories.
- **Bug Fixes**: stability and compatibility improvements.
