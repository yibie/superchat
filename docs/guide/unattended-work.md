# Work that runs by itself

Superchat can do work nobody started at that moment: a fixed command at its time, a look into a failed compile, a read of new feed entries, a scheduled task, a work record it keeps going with. You set each one up by saying it in the chat; the model sets a timer or rule with its `timer_create` tool, and asks you before anything runs unattended. This page says what each one does, what it may touch, and where you see what came of it.

The captures below are buffer text from a batch run with a scripted model and a fixed clock.

## A command at its time

Say it in the chat:

```
every morning tell me when the star count changes
```

The model sets a timer with one fixed shell command (`timer_create`, kind `command`). You are asked once, with the command in full. It then runs once right away so you see what it prints, and after that Superchat runs it by itself at its time, with no model and no question, in the Workspace's root, and compares the output with the last one. Nothing changed: nothing is said. It changed: a line in `M-x superchat-butler`, a line in the conversation you are using, a count on the mode line and a row in `M-x superchat-pending`. Failing once or twice is quiet; three times in a row is one line, gone when it works again.

Say "give me a brief" and that line is written by a model with no tools from what the command printed, one model call each time a line is due; the program still decides whether a line is due, and `RET` shows the raw output.

**It runs unattended with everything a shell can do, so read the command when you are asked.** `k` on its row in `M-x superchat-pending` cancels it. Its environment is Emacs's own: an Emacs started from Finder may not find `gh`, which the first run tells you.

## A failed compile, looked into

```
when a compile fails here, find out why, and tell me only when it is my code
```

The model sets a rule of this Workspace (`timer_create` with `on: "compile_failed"`). It is off until you say so, and you are asked once: the question names the directory and your words, and says that what the compile printed and the files read go to the model provider. After that, each time `M-x compile` fails in that directory, a model looks into it by itself.

It can read the project and change nothing: it has `read_file` and `read_buffer` within the project, and files named like `.env` or `*.pem` are refused (`superchat-watch-unreadable`; this goes by the name, so a secret in a file with another name is read). When it has something to tell, you get the same line as a command's: in `M-x superchat-butler`, in the conversation you are using, on the mode line and in `M-x superchat-pending`; `RET` shows all of it and what it read, `t` brings it into your message.

When your words leave some failures out, as these do, and by them there is nothing to tell, it says so with a tool and nothing appears; the reason is on record. When they leave nothing out, it has no such tool, and you hear of every failure. The program decides first and calls no model for what is no compilation of this project, for the output it looked at last, or past 20 looks in 24 hours (`superchat-watch-looks-per-day`). A compile that passes takes the line away. The rule's row in `M-x superchat-pending` says how many looks, what came of them and the tokens; `k` cancels it.

**What a compile prints may hold a stranger's words: the model treats them as data, and it may still repeat them to you.**

## A failed compile, fixed in a copy (macOS)

```
when a compile fails here, find out why, and fix it if you can
```

The same rule with `fix: true`, and the question also says what a fix is granted. Say "only when it is my code" as well and it can also keep quiet; without that it tells you each time, with a fix or why there is none.

It tries in a copy of the project: no history, nothing named like a secret (`superchat-fix-copy-excludes`, `superchat-fix-copy-megabytes`). When the same failure comes back there, a model reads, changes files with `edit_file` and runs the command that failed with `run_build`, which takes no argument: there is no shell. The operating system confines that build: it writes only in the copy and reads only the copy, the system and the usual toolchains (`superchat-fix-sandbox-read` adds more), with no network, not even this machine. When the failure does not come back in the copy (a missing tool, or a build that needs the network), it only looks, and says why.

The program builds once more after the model, and only what passes is offered: `RET` shows the difference, `a` puts it in your buffers **unsaved** and opens the review, `s` writes. What no longer matches what you have is refused, all of it. At most 3 builds, 120 seconds each (`superchat-fix-builds`, `superchat-fix-build-seconds`), and 5 fixes in 24 hours (`superchat-fix-per-day`).

**Read the difference: a model can make a build pass by changing a test or the build script, and the program cannot tell.** A rule with `fix` cannot be set where there is no sandbox for it: Linux and Windows.

## New feed entries

```
when new posts come into elfeed, tell me about the ones on Emacs
```

A rule on `feed_new`, with elfeed loaded. Once elfeed has put new entries in and stopped fetching for 30 seconds (`superchat-elfeed-quiet-seconds`), a model with no tools reads their titles, feeds and the first 600 characters of their text: at most the newest 30 (`superchat-elfeed-entries`), from the last 3 days (`superchat-elfeed-days`), at most 12 times in 24 hours (`superchat-elfeed-looks-per-day`). What it reads was written by others and goes to the model provider.

The line names the entries it picked with elfeed's own titles; `RET` lists them with elfeed's links, and a title opens the entry in elfeed. The model names entries by number and writes no link of its own: a link in its words is its words. Your words decide each batch: with nothing they ask for, it says nothing, and it cannot say nothing about a batch once it has named an entry that is about what they ask for. There is one such rule in all.

To keep what comes in while Superchat is not loaded, put this in your init file after `superchat-data-directory` is set:

```elisp
(require 'superchat-inbox)
```

What comes into elfeed is then kept on the Tape as it comes in (its feed and id, not its text), whether Superchat is loaded or not, and read from there. With a rule on `feed_new` set and Superchat not loaded, the inbox loads Superchat once Emacs has been idle for 2 seconds (`superchat-inbox-wake`, `superchat-inbox-wake-seconds`), without a word; an Emacs that exits while a batch is read reads it again when it starts. A line said while no conversation is on screen rings its doorbell when one is shown. The inbox loads only the Tape: 4 files.

Asking about your feeds without a rule ("anything about security in my feeds lately?") uses `feed_search`; see [integrations](integrations.md#elfeed).

## Where you see what came of it

### The butler

`M-x superchat-butler` lists what work nobody asked for concluded, one line each: a scheduled task or a work record taken up at its time, a timer's fixed command, how it ended, the first line of its answer.

```text
11-11 23:32  Command "tell me when the star count changes" first run: 41
11-11 23:32  Command "tell me when the star count changes" changed: 42 (was 41)
```

The same line appears in the conversation you are using, with a `[bring in]` button; the model does not see that line, and it is gone when the conversation is reopened.

```text
* System: Butler: [bring in] 
Command "tell me when the star 
count changes" changed: 42 (was 
41)
* User: 
```

`RET` goes to its conversation, or for a command shows what it printed. `t` (or `[bring in]`) attaches what it concluded to the message you are writing in the conversation you are using, and takes you there: one line above the input area stands for it, and the input area stays yours. `C-c C-b` shows it whole, as it will be sent, and `C-u C-c C-b` takes it off; the same conclusion twice is attached once. Nothing is sent: when you send, it goes at the start of your message as a quote, and only then does the model know it. `l` opens the pending list and `g` reads the lines again. If you set `superchat-plugins` yourself, add the `butler` row.

### The pending list

`M-x superchat-pending` is one list of what Superchat is doing and what it waits for: executions under way and who started them, work records waiting for you or for a time, what came due and did not start, scheduled tasks it cannot tell were started, timers to come, work that ended unseen, buffers left unsaved, and stored tools the agent wrote that are not loaded because nobody looked at them. A task that ran again and again is one row that says how many times and its latest conclusion; cancelling the task takes the row away.

```text
 Ended, not looked at   Command "tell me when the star count change… changed: 42 (was 41)                                 butler
 Scheduled              tell me when the star count changes          ran 2 times, last: changed (2027-01-16 00:00), 1 ch… 20261111-233228-b10f
 Scheduled              renew the domain                             reminder at 2027-01-16 02:00                         20261111-233228-b10f
 Scheduled              when a compile fails here, tell me only whe… not looked yet; when a compilation fails in /privat… 20261111-233228-b10f
```

`RET` goes to the conversation (or the buffer; for a tool the agent wrote, to its review, where you approve it into a workspace, reject it or delete it; for a timer's command, what it printed), `v` shows a work record, `k` stops a running execution or cancels a timer, `d` forgets an ended one, `t` puts what an ended one concluded in the message you are writing (as `t` in the butler), `y` / `n` / `a` answer an open approval (allow, refuse, allow all), `b` opens the butler, `g` reads everything again.

The mark on the mode line (` [Superchat: 1 asking]` while a question is open, ` [Superchat: 2]` for work that ended) opens this list. Looking at the list writes nothing and queues nothing. Its keys do write: `g` has the notices read again, which keeps a checkpoint in the database; `d` records that you looked at an ended execution; `y`, `n` and `a` record your answer to an approval; a review records your decision on a tool.

### The schedule

`M-x superchat-schedule-task` lists what you set to run by itself, one row each: timers, commands and rules. The header says how many; each row says what kind it is, when it runs, when next, how many times it ran, how it went last time and how it stands (`set`, `running`, `due, not started`, `3 not looked at`). A task that runs every hour stays one row.

```text
command         daily                          2027-01-16 01:00      2 changed                        set                tell me when the star count changes
reminder        once                           2027-01-16 02:00      0                                set                renew the domain
rule (compile)  when a compilation in /privat…                       0                                set                when a compile fails here, tell me only when it is my code
```

`RET` shows the last result, `t` brings it into the message you are writing, `k` cancels after asking you, `g` reads it all again.

## Scheduled tasks

A timer of kind `task` starts the model on its text at its time, in a conversation of its own. That conversation runs at `superchat-timer-task-level` (default `read-only`): it only reads, and cannot change a buffer, run code or ask you. `/permissions` in that conversation, or the option, lets it do more. A timer of kind `reminder` only tells you.

## A work record Superchat keeps going with

```
/work move every test helper into tests/helpers.el and make the suite pass
```

`/work <goal>` starts a work record Superchat keeps working on: when an execution ends and the record is not finished, Superchat goes on with it by itself in the same conversation, until it is done or needs you. It also stops when you stop it, when an execution fails, when nothing was changed and nothing was written on the record for a while (`superchat-work-round-idle-limit`, 2 tries in a row), or after too many tries (`superchat-work-round-limit`, 256); the record then says why and waits for you.

`/work resume` goes on after a stop, `/work pause` stops Superchat going on by itself, `/work` alone says how the record stands. How many times Superchat went on with a record is shown in one place, the record's detail (`M-x superchat-work-list`, `RET`; or `v` in `M-x superchat-pending`), as `Tries: N/256`. The model needs its `work_record` tool for all this; the `work-records` row offers it whatever `superchat-llm-tool-names` says, and `/work` starts nothing where a Preset's own tool list leaves it out. Superchat goes on only while the conversation's buffer is alive and not after a restart; approvals are still asked one by one.

The rationale for these features is in the decision records: [a command that runs by itself](../decisions/accepted/2026-10-31-a-command-that-runs-by-itself.md), [a failed compile is looked into](../decisions/accepted/2026-10-31-a-failed-compile-is-looked-into.md), [a fix tried in a copy](../decisions/accepted/2026-10-31-a-fix-tried-in-a-copy.md), [what others wrote in your feeds](../decisions/accepted/2026-11-02-what-others-wrote-in-your-feeds.md), [an inbox that keeps what came in](../decisions/accepted/2026-11-03-an-inbox-that-keeps-what-came-in.md), [work nobody asked for has its own place](../decisions/accepted/2026-10-31-work-nobody-asked-for-has-its-own-place.md), [one list of what is pending](../decisions/accepted/2026-10-11-one-list-of-what-is-pending.md), [a list of what you set to run](../decisions/accepted/2026-11-06-a-list-of-what-you-set-to-run.md), [goal rounds](../decisions/accepted/2026-10-11-goal-rounds.md).
