# Workbench Page Overrides

> PROJECT: Threadnote Electron
> Page: workbench (desktop app main workspace)
> This file overrides Master rules for the actual app UI.

---

## Intent

- Direction: comfortable + natural, calm but efficient.
- Keep current 3-column workflow and improve visual hierarchy + feedback clarity.
- Prioritize keyboard flow, focus visibility, and low cognitive load.

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

- Increase card padding from 10px to 12-14px.
- Replace dense action row with priority order:
  - Always visible: Reply, Edit
  - Secondary via "More": View Source, Delete
- Hover only changes border + background tint, no scale.

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
