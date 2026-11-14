# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- superchat-llm: the Tape has nil for JSON false, null, `[]` and `{}` alike, so a call argument rebuilt from the Tape (no kept message) is sent as false whichever it was; only the unkept replay is affected.
- superchat-agent: when ON-ENTRY signals inside the continuation of an asynchronous tool's result, the run ends with `error` as specified, but the signal itself leaves through the timer or process sentinel that delivered the result (SPEC names no caller to give it to); Emacs reports it there.
