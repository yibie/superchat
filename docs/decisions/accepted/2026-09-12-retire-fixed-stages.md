# Decision: Retire the fixed Stage pipeline

Status: accepted

## Problem

The eight Stage names have dispersed owners: `parse` is invoked only by the chat dispatcher over a preparation Turn, the six preparation Stages by the driver, and `save-state` by Runtime; the earlier pipeline specification described a different count. A plugin that needs to intercept a turn must learn which owner runs the Stage it cares about, and the Stage registry is one of several extension mechanisms (Stages, Events, the FIFO, steering) rather than one main line.

## Decision

The user decided on 2026-09-12, answering the question "固定阶段流水线要不要废掉": "同意废掉阶段". The fixed Stage registry and its fixed order are retired. They are replaced by one execution main line with a small number of interception points, in the manner of dsh (durable session events, live agent events, capability-seam events). The order of that work and its acceptance are in [the roadmap, M3](../../roadmap.md#milestones).

What is not decided: the exact replacement API (inbox, waterfall names, combination rules). That is [a proposal](../proposed/2026-09-12-restructure-on-deepseek-harness.md). The queueing semantics the Stage-era FIFO provides migrate into the replacement; they are not dropped.

## Alternatives considered

**Keep the Stages and only align the documents with the eight names.** Rejected: the dispersed ownership stays, and the Stage registry remains one of several mechanisms a plugin must choose between.

**Keep the Stages as the only extension point and move `parse` into the driver.** Rejected: the chat surface needs parsing before an Envelope exists and needs queue-while-running, which the Stage model does not express; the replacement main line must carry those needs rather than force them into stages.

## Consequences

Until M3 is done, the current Stage code is `current` and stays documented in [architecture.md](../../architecture.md). Plugin authors should expect the `superchat-stage-add` API to be replaced; new plugins should prefer Event listeners where one exists.
