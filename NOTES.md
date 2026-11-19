# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L2: `save_buffer` leaves a backup file (`calc.el~`) in the project; the model then sees it in `ls`. The specification does not say whether saving makes backups.
- L5: a chat buffer open on a timeline that the inbox worker runs is not told of the entries as they come; it could watch the timeline while it has a run it did not start.
- L5: the `question` and `answer` events of a turn show after its `** Answer`, among the other entries, not where the call waited.
- L3: the inbox joins pending items into one `user` entry, so the model cannot tell how many messages came (DeepSeek answered "one message" for two); each item could be marked.
- L5: no command opens a timeline by its name; `superchat` opens only the current buffer's own conversation, and `superchat-messages` only timelines that sent or ask.
