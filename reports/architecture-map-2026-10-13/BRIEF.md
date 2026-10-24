# Architecture walk-through of Superchat — working brief

Two agents walk the Superchat codebase together as architects: pi (model gpt-6-astra)
and a Claude session that steers. Goal: understand the code through a map we draw
together, and find where the code, its documents and its own stated rules contradict
each other.

## Ground rules (both agents)

- READ-ONLY on product code, tests and existing docs. The only writable place is
  this directory, `reports/architecture-map-2026-10-13/`. No commits.
- Evidence: every claim about behaviour cites `file:line` on HEAD. Distinguish
  code fact / inference / untested guess.
- The map is `MAP.md` in this directory. One section per layer. Each node is one
  line: name, file(s), one-sentence responsibility, what it owns (state), who calls
  it, who it calls.
- Contradictions go to `CONTRADICTIONS.md`, each with: the two things that disagree
  (quoted, with file:line), who is right in practice (traced in code), and the cost
  a user would notice.
- Do not run Emacs against the live database in `~/.config/nova-emacs/superchat/`.
  Batch Emacs runs, if needed, use `-Q -L . -L plugins/superchat-cowork-telega`
  with `package-user-dir` at `~/.cache/superchat-test-elpa` and
  `temporary-file-directory` under `~/.cache/sc-map/`.

## Reading order (project's own)

1. `AGENTS.md`, `CONTEXT.md` (vocabulary), `docs/architecture.md`, `docs/roadmap.md`
2. `docs/decisions/accepted/` — the rulings, newest first
3. The kernel (`superchat-kernel.el`, `superchat-emit`, `superchat-scope`),
   `superchat-tape.el`, `superchat-runtime.el`, `superchat-tool-execution.el`,
   `superchat-agent-loop.el`, `superchat-llm.el`, `superchat-approval.el`,
   `superchat-work.el`, `superchat-timer.el`, `superchat-pending.el`,
   `superchat-changes.el`, `superchat-plugin.el`, `superchat-default-plugins.el`
4. Entry surfaces: `superchat.el` (chat View), inline (`superchat-inline*.el`),
   emacsclient, Telega plugin, subagent, workflow.

## Layers the map should end up with (adjust if the code says otherwise)

Kernel (events, scopes) · Tape (facts) · Runtime (Runs, queue, Sinks) ·
Tool pipeline (policy, approval, execution, recording) · LLM transport ·
Rows/plugins (what a plugin may own) · Read models (pending, changes, work) ·
Surfaces (chat, inline, headless, Telega) · Actors (lead, timers, work-records,
goal-rounds, subagents).

## Questions the walk-through must answer

1. What is the one object a user action turns into, and where does its identity
   live? (Envelope → Run → Steps → facts). Where does identity leak across
   boundaries (e.g. capabilities captured with a Run id and stored longer)?
2. Who is allowed to write state outside the Tape, and is each such place
   declared somewhere?
3. For each surface: what does it depend on being alive (a buffer, a window,
   a View), and what happens when that dies mid-Run?
4. For each "ending" (Run completed/failed/cancelled, approval abandoned, timer
   fired without View, round brake, wake out of retries): what fact is written,
   who is told, and what is reconstructible after restart?
5. Where do three or more documents/docstrings describe the same behaviour,
   and do they agree?
