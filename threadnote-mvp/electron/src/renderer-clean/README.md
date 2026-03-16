# Renderer Clean

This directory is no longer the active renderer workbench.

Current status:

- Active runtime entry moved to `electron/renderer/` with React + Vite
- This folder now keeps only reusable renderer-side logic still imported by the new renderer

Remaining responsibilities here:

- `editor/` token-aware editor runtime and completion helpers
- `components/richBodyRenderer.js`

Rules:

- Do not add HTML entrypoints or DOM-shell runtime files back into this folder
- Move the remaining shared modules into the new renderer tree after the React replacement stabilizes

Do not recreate the removed legacy `src/renderer/` tree.
