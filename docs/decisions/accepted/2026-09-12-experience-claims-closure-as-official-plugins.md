# Decision: Experience, Claims and Closure are official plugins, not spine

Status: accepted — superseded in part by [the M3a removal decision](2026-09-15-m3a-remove-closure-and-claims.md): Claims and Closure are deleted and Experience loses its context collector and its review entry; the ruling that Experience stays an official plugin in the default set stands

## Problem

The driver plugin's exact dependencies are `:inject '(tape closure model-transport)` (`superchat-pipeline.el`), so Closure cannot be omitted from a composition today; Experience reaches the spine only through the optional `recovery` service, which the driver probes rather than requires. The user asked whether Experience, Claims and Closure belong in the spine.

## Decision

The user decided on 2026-09-12, answering "Experience、Claims、Closure 进不进新骨架": "不进骨架，它们更适合作为官方插件". They become official plugins that attach to durable session events, and the spine must load without them.

This supersedes the 2026-09-11 ruling that the Experience subsystem belongs in the core. It does not remove them from the default set: Experience, Claims and Closure stay mounted by default, as do GenUI and emacsclient, unless a separate user decision trims the default set.

The work item is [the roadmap's M3a](../../roadmap.md#milestones), a bounded sub-item immediately after M3: the spine loads without them, proof-dependent consumers fail closed when Closure is absent, and the default set is unchanged.

## Alternatives considered

**Keep them in the spine and wire the Experience collector.** Rejected: it would add a model-visible path that no journey asks for, before the Tape-derived history is settled.

**Delete them.** Rejected: the user kept them as official plugins; the evidence-recording ideas are meant to prove themselves on session events.

## Consequences

How a Run seal is proved when Closure is not mounted is not designed; until a replacement is designed and accepted, the current Closure handoff, tool and terminal-evidence contracts stay in force and the driver keeps injecting `closure`. Experience's recovery view stays reachable through the optional `recovery` service.
