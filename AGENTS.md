# Superchat

`SPEC.md` is the whole contract: read it first, all of it. Build only what it
names. When something seems missing, write one line in `NOTES.md` and go on.

- Run `scripts/check.sh` before handing work back. It must print `CHECK OK`.
  It runs in a copy under the temporary directory; never run the tests
  directly in this directory.
- Stay in this directory. Do not read or search any other Superchat checkout
  on this machine: this is a clean implementation and must not borrow from an
  earlier one.
- Each module has a line cap in `SPEC.md`. Under the cap is good; far under is
  better. Do not pad, and do not compress code to fit: if it does not fit,
  the design is wrong, say so in `NOTES.md`.
- A test deletes only what it made with `make-temp-file`.
