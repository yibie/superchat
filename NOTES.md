# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L5: the `question` and `answer` events of a turn show after its `** Answer`, among the other entries, not where the call waited.
- L5: no command opens a timeline by its name; `superchat` opens only the current buffer's own conversation, and `superchat-messages` only timelines that sent or ask.
