# Superchat

`SPEC.md` and the layer specifications it links (`spec/L*.md`) are the whole
contract. Before you write, read `SPEC.md`, all of it, and the specification
of the layer you work in, all of it. Of a lower layer read only the public
functions you call. Build only what they name. When something seems missing,
write one line in `NOTES.md`, starting with the layer it concerns, and go on.

- Work in one layer at a time: edit only its files, their tests, its
  specification and its decision notes. A need in another layer goes to
  `NOTES.md`.
- A commit that changes a rule of a specification adds or updates a note
  under `decisions/` in the same commit, or says `Decision: none — WHY` in
  its message; `SPEC.md`, "Decisions", gives the shape. Run
  `git config core.hooksPath scripts/hooks` once so the commit is checked.
- Run `scripts/check.sh` before handing work back; `scripts/check.sh L1`
  runs the tests of one layer while you work. It must print `CHECK OK`. It
  runs in a copy under the temporary directory; never run the tests directly
  in this directory.
- A use of another layer's private name is an error of the gates. Do not add
  a line to `scripts/gate-exceptions` to get past one: remove the use, or
  write the missing public function into the lower layer's specification.
- Stay in this directory. Do not read or search any other Superchat checkout
  on this machine: this is a clean implementation and must not borrow from an
  earlier one. One exception: for the Org display of the L5 chat buffer
  (Markdown shown as Org, the look of the chat), the earlier Superchat in
  `~/Documents/emacs/package/superchat` may be read and its code borrowed.
  What is borrowed is reshaped toward the display of agent-shell, which is
  closer to what a person needs; it follows SPEC.md's rules against overflow,
  the layer's line cap and this repository's style. For `docs/migrating.md`
  only, its README, its `defcustom`s and its line counts may be read, to
  compare; no code is borrowed. Nothing else is read there.
- Each module has a line cap in `SPEC.md`. Under the cap is good; far under is
  better. Do not pad or compress code to fit. Split a module by what it is,
  never by its line count: when code does not fit, stop and tell the
  person, who raises the cap or changes the design.
- A test deletes only what it made with `make-temp-file`.
- The shell is macOS's: BSD tools, so GNU-only options such as `cat -A` or
  `grep -P` fail; use `sed -n l` and `grep -E`.
- `scripts/check.sh` exits 1 when a check fails. Read its last line, `CHECK
  OK` or `CHECK FAILED`: piped (`| tail`), the exit status is the pipe's, 0.
- A probe outside `check.sh` runs in a copy made as `check.sh` makes one,
  where `bridge/lisp` is `bridge-lisp`: `-L . -L bridge-lisp -L test`.
