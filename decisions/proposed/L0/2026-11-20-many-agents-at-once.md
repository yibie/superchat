# Decision: Many agents at once

Status: proposed

## Problem

Superchat runs many conversations at once (one run per timeline, all
asynchronous), but it is not a multi-agent system: an agent has no identity
of its own, cannot hand work to another agent and wait for the result, has
no channel to other agents, and nothing shows the person which of several
running conversations needs them. The person has not decided whether to
take this on: it may bring much complexity. This note keeps what was found,
so that the decision, either way, starts from it.

What three systems do (researched 2026-11-20):

- **An Emacs agent multiplexer** (gist `ojhughes/58d4b71a…`, `agents.el`):
  runs external terminal agents in ghostel buffers. Each gets an id in its
  environment; the agent's own hooks report one of five states (`working`,
  `blocked`, `done`, `idle`, `exited`) back through `emacsclient`, so Emacs
  never reads terminal output. "Needs me" is `blocked` or an unseen `done`,
  ranked, oldest first, with a key that cycles through them; seen means the
  buffer appeared in a window. Fan-out runs one prompt in N worktrees;
  `agent-when-done` forwards one agent's final answer to another.
- **oh-my-pi** (`@oh-my-pi/pi-coding-agent` 16.4.4): delegation inside one
  process. A `task` tool starts children, asynchronous by default, each
  delivering its result into the parent's conversation when it calls
  `yield` (validated against a schema; three reminders, then a warning).
  Agents are Markdown with YAML frontmatter (`tools`, `spawns`, `model`,
  `output`); concurrency 32, depth 2; children run with every tool allowed,
  "the parent task approval is the authorization boundary"; worktree
  isolation is optional, merged by patch or cherry-pick; a global registry
  (`running`, `idle`, `parked`, `aborted`) feeds an Agent Hub; peers talk
  through an IRC tool; finished children park and revive from their JSONL.
- **Orca** (stablyai/orca): a desktop app over external CLIs, one agent per
  terminal per worktree, merged by pull request. States `working`, `idle`,
  `blocked` come from hooks and terminal titles; one inbox across
  worktrees, unread marks, notifications. Experimental orchestration: a
  coordinator agent creates tasks and starts workers through a CLI; workers
  send `worker_done` exactly once, ask the coordinator instead of a local
  prompt, send heartbeats; messages are durable, read oldest first, replayed
  until acknowledged; depth 4; stopping a worker stops its helpers.

Common to all three: an explicit state per agent from events, one place
that says who needs the person, results delivered asynchronously as a
message, an explicit completion signal, a depth limit with cancelling down
the tree.

## Proposal

Two steps, each a decision of its own:

1. **See many agents (orchestration by the person).**
   - L1: a view of each timeline's state, derived from the Tape: `working`
     (a run goes), `blocked` (`superchat-question` waits), `done` (ended,
     not yet seen), `idle`.
   - L5: "seen" from `window-buffer-change-functions` (not on the Tape); a
     "needs me" order (`blocked`, then unseen `done`, oldest first) with a
     command that cycles through it; the messages buffer grouped by state;
     a mode line count only when nonzero.
2. **Delegation (an agent starts agents).**
   - The identity of a child is a profile
     (`2026-11-20-a-profile-is-who-answers.md`).
   - L0: a child timeline's first entry refers to its parent's run.
   - L2/L3: a tool that starts a run on a new child timeline with a
     profile and an assignment, asynchronous; its result comes back to the
     parent through the inbox (`superchat-inbox-put`), which already reads
     oldest first and counts an item answered only when a `user` entry
     refers to it. Completion is explicit (a final answer, or a `yield`
     tool). A depth limit; cancelling a parent cancels its children.
   - A child's question goes to the parent as a `question`, the mechanism
     Superchat already has, rather than every tool allowed as in omp.
   - Each child may get its own `:directory` (a worktree).

What Superchat already has for this: asynchronous runs per timeline, the
inbox (durable, ordered, acknowledged by reference), `superchat-steer`
(text a busy run takes at its next step), `question`/`answer` and the
`waiting` state, `:on-done`, a working directory per timeline.

## Alternatives considered

- Do neither. Open: the person may judge the complexity too high; step 1
  still helps with the conversations that already run at once.
- Orchestrate external terminal agents, as the gist and Orca do. Not taken:
  Superchat is its own agent; its state is on its own Tape, needing no hooks.
- Children with every tool allowed, as omp. Not taken: Superchat's
  `question` keeps the person's level for children too.
- A coordinator speaking a CLI, as Orca. Not taken: in one Emacs process a
  tool and the inbox do the same without a protocol.
- Peer messaging between agents (omp's IRC). Not taken first: delegation
  and results cover the common case; peers can come later on the inbox.

## Acceptance criteria

- Step 1: with three timelines, one running, one waiting at a question, one
  finished and unseen, the view gives `working`, `blocked`, `done`; the
  cycle command visits the `blocked` one first, then the `done`; showing
  the `done` one's buffer makes it `idle`.
- Step 2: a parent run that delegates two children continues; each child's
  answer reaches the parent as one inbox item; cancelling the parent
  cancels both; a third level beyond the depth limit is refused; a child's
  question waits as a `question` the person can answer.

## Risks

- Complexity: concurrency, cancellation down a tree, costs that multiply,
  permissions crossing runs; the reason this is undecided.
- Cost: N children are N runs of requests; budgets per child are needed
  (omp: a soft request budget, then a graceful stop).
- Every layer changes for step 2; step 1 touches only L1 and L5.
- `superchat-agent.el` is at 773 of 800 lines.
