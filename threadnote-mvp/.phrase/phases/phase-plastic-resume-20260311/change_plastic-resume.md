# Change Log: Plastic Resume

## 2026-03-11

- Initialized the plastic-resume phase with explicit scope:
  - embed `goal layer` inside `Thread`
  - synthesize `Resume` from a stable component library
  - keep AI limited to goal-type suggestion and component selection
- Implemented the thread `goal layer` data model:
  - added `goalStatement / goalType / successCondition / currentStage` to `ThreadRecord`
  - kept snapshot decoding backward compatible by deriving a legacy default goal layer for older threads
- Expanded the AI boundary for plastic Resume:
  - added `goalTypeSuggestion`
  - upgraded `resumeSynthesis` to return ordered `ResumeComponent` blocks instead of only summary text
  - kept AI limited to stable component selection, ordering, and content filling
- Updated thread creation and editing flows:
  - new threads now require goal statement, type, success condition, and stage
  - stream-originated thread creation now opens a goal-confirmation sheet instead of silently creating a generic thread
  - thread inspector exposes goal context and an `Edit Goal` path
- Reworked the workbench Resume card into a component-driven surface:
  - always keeps `Goal Focus / Current State / Next Best Move`
  - varies middle blocks across `Build / Study / Research`
  - adjusts component ordering based on `currentStage`
- Verified implementation through both `swift build` and `xcodebuild`
