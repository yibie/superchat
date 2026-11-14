# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- superchat-llm: the Tape has nil for JSON false, null, `[]` and `{}` alike, so a call argument rebuilt from the Tape (no kept message) is sent as false whichever it was; only the unkept replay is affected.
- superchat-agent: when ON-ENTRY signals inside the continuation of an asynchronous tool's result, the run ends with `error` as specified, but the signal itself leaves through the timer or process sentinel that delivered the result (SPEC names no caller to give it to); Emacs reports it there.
- superchat-agent: the user entry of a run whose handoff failed is appended while the run is ending with `error`, so an ON-ENTRY that cancels on it finds the timeline already free; the status stays `error` (SPEC says ON-ENTRY may cancel, not what a late cancel does).
- superchat-agent: when ON-ENTRY signals on an entry the ending of a run wrote itself, the signal leaves through whoever ended the run, after ON-DONE, and ON-ENTRY does not get the entries after that one; SPEC names no other place for it.
- superchat-permission: `superchat-tape-select` opens the Tape a second time on every call, so each permission lookup costs about 0.7 ms (2000 sync calls at `auto`: 1.6 s, was 1.2 s; at `ask` with "always": 3.4 s, was 11.2 s).
