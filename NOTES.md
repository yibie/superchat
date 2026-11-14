# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- superchat-llm: an assistant message rebuilt from the Tape (no kept message) sends a call argument that is JSON false as `{}`, because the bridge reads nil as an empty object and has no false in a plist; SPEC says false is nil on the Tape. Only the unkept replay is affected.
