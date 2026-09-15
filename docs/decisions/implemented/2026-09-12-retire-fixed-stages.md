# Decision: Retire the fixed Stage pipeline

Status: implemented — delivered under M3, accepted by the user on 2026-09-15

## Problem

The eight Stage names have dispersed owners: `parse` is invoked only by the chat dispatcher over a preparation Turn, the six preparation Stages by the driver, and `save-state` by Runtime; the earlier pipeline specification described a different count. A plugin that needs to intercept a turn must learn which owner runs the Stage it cares about, and the Stage registry is one of several extension mechanisms (Stages, Events, the FIFO, steering) rather than one main line.

## Decision

The user decided on 2026-09-12, answering the question "固定阶段流水线要不要废掉": "同意废掉阶段". The fixed Stage registry and its fixed order are retired. One execution main line with a small number of interception points replaces them, in the manner of dsh (durable session events, live agent events, capability-seam events). Its mechanism is the [M3 execution main line decision](2026-09-14-m3-execution-main-line-resolution.md), completed by the [M3 closing rulings](2026-09-15-m3-closing-rulings.md); acceptance is under M3 in [the roadmap](../../roadmap.md#milestones).

The replacement API (inbox, waterfalls, combination rules) is the one those two records decide; [the broader restructuring proposal](../proposed/2026-09-12-restructure-on-deepseek-harness.md) stays a proposal. The Stage-era FIFO's queueing semantics live on in Runtime's inbox.

## Alternatives considered

**Keep the Stages and only align the documents with the eight names.** Rejected: the dispersed ownership stays, and the Stage registry remains one of several mechanisms a plugin must choose between.

**Keep the Stages as the only extension point and move `parse` into the driver.** Rejected: the chat surface needs parsing before an Envelope exists and needs queue-while-running, which the Stage model does not express; the replacement main line must carry those needs rather than force them into stages.

## Consequences

No Stage registry, Stage name or `superchat-stage` feature exists. Plugins extend the main line through Event listeners (`superchat-on`) and context contributors (`superchat-context-register`), both in `superchat-event.el`, as [architecture.md](../../architecture.md) maps.
