# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L2: `save_buffer` leaves a backup file (`calc.el~`) in the project; the model then sees it in `ls`. The specification does not say whether saving makes backups.
- L4: a timeline cut off by Emacs exiting shows `⏳ Progress` when opened, as if running, until the next run marks it; the chat could read the Tape the way `superchat-run` does when it shows a timeline.
- L4: the duration of a call marked `Interrupted:` is the time from the cut-off to the mark (seen as `(97.6s)`), not how long the call ran; such a call may need no duration.
- L4: a chat buffer open on a timeline that the inbox worker runs is not told of the entries as they come; it could watch the timeline while it has a run it did not start.
