# Change Log: Resume Recovery

## 2026-03-11

- Initialized the Resume Recovery phase:
  - narrowed the product focus to three-line recovery by default
  - established `Resolved So Far` as the only default secondary layer
  - separated this product correction from the ongoing plastic-resume implementation phase
- Implemented the first pass of three-line Resume recovery:
  - replaced the default Resume component wall with three goal-shaped recovery lines
  - added a collapsed `Resolved So Far` layer under the default Resume
  - kept goal metadata in inspector so the main Resume stays small
- Updated heuristic resume synthesis for `Build / Study / Research`:
  - each goal type now produces a different 3-line recovery vocabulary
  - fallback synthesis continues to work through the same recovery shape
- Verified the first pass with both `swift build` and `xcodebuild`
- Refreshed the sample database so demo data better matches product intent:
  - replaced the single-thread seed with clearer `Build / Study / Research` examples
  - removed the most misleading unresolved demo note
  - kept this change scoped to seed data rather than product logic
- Reshaped the thread page into a restart workbench:
  - moved `Resolved / Since You Left / Key Anchors` into a dedicated memory rail
  - replaced the old resume-and-stream stack with `Restart Note + Current Working Segment + Continue`
  - reduced the right inspector to tools so thread context is no longer duplicated
- Hardened the shared Xcode scheme against IDE log initialization timeouts:
  - set `IDEPreferLogStreaming=YES` for Run/Test/Profile in `project.yml`
  - regenerated `Threadnote.xcodeproj` so the shared scheme carries the setting
  - verified the project still builds with both `swift build` and `xcodebuild`
- Rolled back the thread-layout overreach after screenshot review:
  - restored the single-column `Restart Note / Continue / Working Stream / Settled So Far` reading path
  - moved `Resources / Timeline / Thread Memory` back to the bottom secondary-action strip
  - kept `Thread Memory` as a popover instead of a rail tab
- Replaced the temporary browse-only list fallback with a real global `Resources` surface:
  - removed the revived `ListDocument` path and the remaining list maintenance APIs
  - added a global `Resources` document that aggregates attachment-bearing notes and `@object` notes across all threads
  - aligned thread-local `Resources` with the same attachment + `@object` resource definition
  - preserved `@` as object mention syntax and did not change parser behavior

## 2026-03-12

- Typed the derived resource layer instead of keeping the old attachment/object-note split:
  - introduced a single derived resource classifier with `Links / Media / Mentions`
  - kept resource typing fully derived from existing entries and metadata, with no snapshot migration
  - rebuilt the global `Resources` document around typed per-thread sections and typed summary counts
- Restored a right-side secondary tool layer without breaking the single-column thread flow:
  - replaced the thread-local `Resources` and `Timeline` sheets with an on-demand overlay inspector
  - kept the inspector limited to fixed `Resources / Timeline` tabs and left room for future extra tabs without shipping them now
  - preserved `Thread Memory` as a separate popover so the main thread page still reads top-to-bottom
- Tightened the thread chrome again after UI review:
  - moved `Resources / Timeline / Thread Memory` above the thread title instead of below the working stream
  - removed the standalone `Settled So Far` section from the thread page
  - kept settled memory only in the `Thread Memory` secondary path
- Pulled the thread workbench closer to the distill-style reference layout:
  - turned `Resources / Timeline / Thread Memory` into a chrome-level tool strip instead of plain section buttons
  - kept the right inspector discoverable by leaving its tabs exposed even while the panel is collapsed
  - restyled `Resources / Timeline` inside the inspector as distill-oriented cards rather than flat lists
- Corrected the first dock attempt after screenshot review:
  - removed duplicated `Resources / Timeline` buttons from the main workbench so those tools live only on the right-side dock
  - re-anchored the exposed inspector tabs to the right edge instead of letting them drift to the left side of the detail view
  - kept `Thread Memory` as the only chrome-level secondary action above the thread title
