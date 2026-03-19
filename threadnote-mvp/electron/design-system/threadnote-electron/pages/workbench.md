# Workbench Page Overrides

> PROJECT: Threadnote Electron
> Page: workbench (desktop app main workspace)
> This file overrides Master rules for the actual app UI.

---

## Intent

- Direction: comfortable + natural, calm but efficient.
- Keep current 3-column workflow and improve visual hierarchy + feedback clarity.
- Prioritize keyboard flow, focus visibility, and low cognitive load.
- Product principle: `Capture low, synthesize high.`
- Product principle: entry 负责捕获，thread 负责成义。
- Interpretation: treat entry as a low-semantic capture unit; treat thread as the high-semantic container that accumulates question, judgment, synthesis, and next-step meaning over time.

## Semantic Model

- Entry modes are limited to `note`, `question`, `source`.
- Treat these as reading/capture modes, not ontology-level content types.
- `note`: default captured content, including ideas, claims, plans, comparisons, summaries, and patterns.
- `question`: content that explicitly asks for clarification, answer, or decision.
- `source`: links, attachments, citations, excerpts, and evidence carriers.
- `@object` handles object identity (`@Figma`, `@OpenAI`, `@Atlas`); do not introduce object-class tags such as `tool`.

Legacy kind mapping for presentation and migration:
- `idea`, `claim`, `comparison`, `pattern`, `plan`, `decided`, `solved`, `verified`, `dropped`, `handoff`, `anchorWritten` -> `note`
- `evidence` -> `source`
- `question` -> `question`
- `source` -> `source`

---

## Layout Overrides

- App shell: 3 columns with breathing space.
- Desktop (>=1360): `272px minmax(640px,1fr) 336px`
- Laptop (1180-1359): `248px minmax(560px,1fr) 312px`
- Tablet (<1180): collapse to single column, but keep sticky top context bar for current thread.
- Internal spacing rhythm: use 8/12/16/24 scale only.

---

## Typography Overrides

- Heading font: `"Avenir Next", "SF Pro Display", "PingFang SC", sans-serif`
- Body font: `"Avenir Next", "SF Pro Text", "PingFang SC", sans-serif`
- Reason: works with current CSP (`style-src 'self'`) and avoids remote font dependency.
- Size rhythm:
  - Page title: 26/32
  - Section title: 18/24
  - Body: 14/22
  - Meta text: 12/18

---

## Color Overrides

Use this app palette instead of Master:

- `--bg: #f4f6f3`
- `--surface: #fbfcfa`
- `--surface-soft: #f2f6f2`
- `--ink: #1f2a24`
- `--muted: #617267`
- `--line: #d8e2d9`
- `--line-strong: #becdc0`
- `--accent: #2f7d67`
- `--accent-soft: #e6f2ec`
- `--success: #2f8b5e`
- `--warning: #a97835`
- `--danger: #b35349`

Accent usage rule:
- Primary action + active nav only.
- Keep accent density low (<10% of visible area).

---

## Component Overrides

### Shell Panels

- Sidebar / main canvas / inspector use slightly different elevation:
  - Sidebar, inspector: flat + 1px border.
  - Main canvas: subtle lift (`shadow-sm`) for reading focus.

### Toolbar (Thread Document)

- Split into 3 groups:
  - Primary: `Add Note`, `Timeline`
  - Secondary: `Prepare`, `Resources`, `Thread Memory`
  - Destructive/rare: `Archive` (move into overflow menu)
- Keep max 4 visible buttons before overflow.

### Entry Card

- Entry card should reflect capture mode, not full semantic structure.
- Entry carries low-semantic content only; do not force claim/pattern/plan/evidence semantics into the card shell.
- High-semantic meaning should surface at thread level through thread header, inspector, summaries, anchors, and grouped views.
- Increase card padding from 10px to 12-14px.
- Replace dense action row with priority order:
  - Always visible: Reply, Edit
  - Secondary via "More": View Source, Delete
- Hover only changes border + background tint, no scale.
- Reference / relation metadata should not stay as raw `[[...]]` syntax in body copy.
- Default treatment: render references as a secondary meta row under the body using calm pill chips.
- Keep relation pill visually lighter than the reference pill so label remains primary.

Reference / relation style references:
- Style A `soft segmented` (default): muted reference pill + accent-soft relation pill, best match for current calm workbench.
- Style B `outline`: transparent pills with hairline borders, works when cards already have a stronger fill.
- Style C `tinted`: filled neutral reference pill + slightly stronger accent relation pill, good for dense thread views.
- Style D `minimal`: text-only chips with icons and almost no container, use only if card density becomes the top priority.

### Inputs

- Focus ring required on all interactive controls:
  - `outline: 2px solid #2f7d67`
  - `outline-offset: 2px`
- Textareas default min height:
  - Stream composer: 132px
  - Inline note: 112px

---

## Interaction Overrides

- Motion:
  - Hover/focus transition: 140-180ms ease
  - Async state transitions: 180-220ms
- All async actions need visible pending state:
  - Button loading text or inline spinner
  - Success feedback toast for create/update/delete
- Keyboard:
  - Keep existing `Cmd+Enter` submit.
  - Add visible shortcut hints only when input is focused.
- Accessibility:
  - No focus outline removal.
  - Preserve logical tab order: sidebar -> main -> inspector.

---

## Mapping to Current Code

- Global visual tokens: `renderer/src/styles.css`
- 3-column shell and responsive behavior: `renderer/src/styles.css`
- Thread toolbar structure: `renderer/src/ui/App.jsx`
- Entry action hierarchy: `renderer/src/ui/App.jsx`
- Sidebar hierarchy and row density: `renderer/src/ui/App.jsx`

---

## Acceptance Criteria (UI/UX)

- Users can identify current active context in <=1 second.
- Keyboard navigation has visible focus on all interactive elements.
- Primary writing action is always discoverable without scrolling.
- No layout jump during hover/press/reply toggle.
- Visual tone is calm/natural with readable contrast in light mode.
