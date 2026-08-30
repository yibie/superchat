# Session EditorWorkspace Close

status: active
scope: process-local buffers owned by one Session EditorWorkspace and its tagged Session tab
applies_when: code or a native tab close requests closure of a stable Session ID
owner: `superchat-editor-workspace.el`
source Evidence ID: `E-20260903-003`; `E-20260905-001`
verification: EditorWorkspace 34/34; Slice09 native UI 51/51; canonical ERT 1220 total, 1203 expected, 0 unexpected, 17 skipped; independent Slice09 final check PASS

## Action contract

1. Resolve the stable Session ID directly or from the current tab's Session
   parameter.  Do not infer it from a tab name, selected window, buffer name,
   directory, project, or correlation ID.
2. Read one public Runtime status projection and select active Runs whose
   Session ID equals the target.  When the snapshot is non-empty, prompt once.
3. Rejection changes no Run, EditorWorkspace state, buffer, or tab.  It vetoes
   native closure and points the user to
   `M-x superchat-close-session-workspace` for a later retry.
4. Acceptance publishes `closing` before invoking any cancellation callback.
   Cancel the captured Run IDs through `superchat-runtime-cancel`.  A
   cancellation error or quit leaves the workspace frozen in `closing` and
   performs no buffer or tab effect; the next non-reentrant invocation takes a
   fresh snapshot.
5. After cancellation succeeds, use ordinary `kill-buffer` behavior on the
   captured buffers carrying the target's owned marker.  Do not bypass their
   query hooks.  Rescan live markers after the attempts: survivors produce
   `retained`; no survivors produce `closed`.
6. A retained close leaves the tagged tab and owned survivors visible.  Retry
   uses the same action and a fresh live-marker scan.  A closed result removes
   tagged tabs; if the tagged tab is the frame's sole tab, retain the native
   tab after removing its Session parameter.

## Native hook ordering

Third-party prevent-close hooks ordered before Superchat may veto without
starting the action.  Once the event reaches Superchat's hook for a tagged
Session tab, EditorWorkspace consumes the close transaction.  Its internal
tab deletion suppresses prevent-close hooks to avoid recursively splitting
the transaction, while ordinary tab pre-close hooks still run.  Hooks ordered
after Superchat therefore do not run for that tagged close.

## Boundaries

- Live buffer-local ownership markers are the close authority.  Tab, project,
  ibuffer, name, directory, and file location are projections or attributes.
- Runtime owns Run cancellation and execution quiescence; EditorWorkspace does
  not implement a second Run state machine.
- Closing an EditorWorkspace does not delete or rewrite durable Session,
  Artifact, Anchor, or Tape data.
- Runtime owns Run lifecycle and execution quiescence but no editor ownership,
  close, retry, or abandonment path. EditorWorkspace is the sole buffer-close
  authority under this protocol.
