# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- superchat-llm: an assistant message rebuilt from the Tape (no kept message) sends a call argument that is JSON false as `{}`, because the bridge reads nil as an empty object and has no false in a plist; SPEC says false is nil on the Tape. Only the unkept replay is affected.
- superchat-agent: when ON-ENTRY signals inside the continuation of an asynchronous tool's result, the run ends with `error` as specified, but the signal itself leaves through the timer or process sentinel that delivered the result (SPEC names no caller to give it to); Emacs reports it there.
- superchat-tools: SPEC says DONE takes "`(error MESSAGE)` data as a second argument" but shows `(funcall DONE nil "what went wrong")`; implemented the latter, the message string.
