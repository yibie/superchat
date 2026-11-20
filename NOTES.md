# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L5: text sent while a run the inbox or a timer started goes is queued, not steered: that run's ON-DONE (the worker's) does not give back :steers, so it would be lost. Steering into it needs the worker to hand :steers back, e.g. as new inbox items.
