# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L3: SPEC does not say that a handoff whose answer has no summary fails ("The model gave no summary."), as the code does; L1 now reads an anchor with refs and an empty summary as a fork wherever it is the latest, so L3 must never write one otherwise.
