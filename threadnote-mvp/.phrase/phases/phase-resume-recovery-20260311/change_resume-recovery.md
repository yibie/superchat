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
- Tightened the workbench layout after screenshot review:
  - moved the memory rail into the right inspector to reduce center-column spread
  - renamed `Current Working Segment` to `Where You Left Off`
  - rendered `Restart Note` as bullet points instead of a paragraph so the next move scans faster
