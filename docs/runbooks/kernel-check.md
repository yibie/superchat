# Kernel Check

status: active
scope: the code citations in `KERNEL.md` (`file.el:NNN@construct` or `file.el:NNN@-`) and the checker that judges them, `test/kernel-check.el`
applies_when: `check` verifies a Kernel revision; `learn` is about to write `KERNEL.md`; `do` closes a change that moved lines in a cited `.el` file; anyone is about to edit a Kernel entry
owner: project maintainer
source: `E-20260829-001` (K5 marker and pairs behavior); `E-20260828-001` (K4 re-anchor, drift probes, incident); `test/kernel-check.el`; `test/test-kernel-check.el` (24 ERT tests, run by `test/run-tests.el`)
verification: from the checkout root, `emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md` prints `errors 0, warnings 0`; `bash docs/spec-agents/check-kernel.sh .` prints `ok`; the ERT suite runs the 24 `kernel-check/` tests with 0 unexpected results

## Preconditions

- Run from the checkout root. Cited files resolve against the current directory
  (`--root DIR` overrides this), so the same command works on a copy of the
  Kernel kept outside the tree.
- Emacs 31.0.91 in batch mode is what this was verified on; the checker needs only
  `cl-lib` and `subr-x`.
- Any scratch root you point `--root` at must be a real copy made with `cp`,
  never a symlink to the checkout. Writing "into the copy" through a symlink
  writes into the real file; that is how two source files were truncated on
  2026-08-28 (see the incident in `E-20260828-001`). Check with `test -L`
  before every write into a scratch root.

## The three modes

```
emacs -Q --batch -L . -l test/kernel-check.el -- check [--strict] [--root DIR] KERNEL.md
emacs -Q --batch -L . -l test/kernel-check.el -- convert [--root DIR] KERNEL.md OUT.md
emacs -Q --batch -L . -l test/kernel-check.el -- pairs [--root DIR] KERNEL.md OUT.md
```

`check` reads every code span of the form `` `file:NNN` ``, `` `file:NNN@construct` ``, `` `file:NNN@-` ``,
`` `:NNN` `` (short: inherits the file of the previous complete citation in the
same `###` entry), or a range such as `` `file:10-12` `` / `` `file:1,5-6` ``. For each:

| citation | error when | warning when |
| --- | --- | --- |
| any | the file is missing, the line is out of range, or the line is blank | — |
| `.el` with `@construct` | the line is no longer inside the named top-level form (the message names the expected and the actual form) | — |
| `.el` with `@-` | the line is inside a top-level form (the message names `declared bare` and the actual form) | — |
| `.el` without annotation (`@construct` or `@-`) | — | always (`--strict` turns this into an error) |
| `.md`, `.sh` | as "any" | — |
| `.md`, `.sh` with `@marker` | a construct marker is used on a non-`.el` citation | — |
| malformed annotated citation (for example `x.el:7@`) | the citation is reported as malformed | — |

The construct is the name of the top-level form whose extent covers the line,
found with the Lisp reader (`kernel-check--construct-at`): `defun`, `defmacro`,
`defvar`, `defvar-local`, `defcustom`, `defconst`, `defsubst`, `cl-defun`,
`cl-defmacro`, `cl-defstruct`, `cl-defgeneric`, `cl-defmethod`, `ert-deftest`,
and `define-*` forms (their quoted first argument resolved to the symbol) give
their name; an unnamed form gives `HEAD/FIRST-QUOTED-SYMBOL` or bare `HEAD`; a
top-level object with no symbol head — a literal string, a vector, a list-headed form — yields its Lisp type name (`string`, `vector`, `cons`); a comment, header or blank line gives nothing. It prints `refs N, errors E, warnings W` and exits 1
on any error (with `--strict`, also on any warning), else 0.

`convert` writes OUT.md as a copy of the input with `@construct` appended to
every `.el` citation that lacks one and whose line sits inside a form. It changes
no other byte, is idempotent (converting its own output inserts 0), refuses to
overwrite its input (exit 2), and on any error writes nothing and reports
`inserted 0`. A citation whose line has no construct is left bare with a
`no construct at FILE:NNN; left unchanged` warning.
It never writes `@-`: deciding that a bare annotation is intentional is a
`learn` judgment. A real top-level form whose head is `-` is left bare and is not
converted.

`pairs` writes a report listing, per entry, the other entries that cite the same
construct, plus every sentence of the entry containing a universal-quantifier
term (`only`, `every`, `never`, `exactly`, `always`, `single`, `sole`, `nothing`,
`nowhere`, `all`, `no other`). Each universal sentence is followed immediately
by the narrowing sentences from the same entry as `- narrowed by:` lines; the
attachment is by sentence position, so duplicate text is preserved and a
sentence is not attached to itself. The summary adds `narrowed N in M entries`.
The narrowing vocabulary is `not every`, `not all`, `but not`, `except`, `unless`,
`only when`, `only if`, `rather than`, `outside`, `narrow`, `narrows`, `narrowed`,
`narrowing`, `narrower`, `narrowest`, and `no longer`. On K5: 76 entries, 314
shared groups, 167 universal sentences, 95 narrowed in 30 entries.

## When each mode must run

| action | run | gate |
| --- | --- | --- |
| `check` (verifying any change that touched `KERNEL.md`) | `check --strict KERNEL.md` | `errors 0`; `warnings 0` |
| `learn`, before writing `KERNEL.md` | `convert` on the new text, then `check --strict` on the output | `errors 0`; `warnings 0`; every new `.el` citation annotated, including intentional `@-` markers |
| `do`, closing a product change that inserted or removed lines in a cited `.el` file | `check KERNEL.md` | not a gate for `do`: report every error as a `learn` to-do in the hand-off. `do` does not edit `KERNEL.md` |
| anyone, before editing a Kernel entry | `pairs KERNEL.md OUT.md` | read the paired entries first (see `docs/protocols/kernel-review.md`) |

## The gate, stated plainly

`--strict` is the gate for K5. The two intentional bare citations are declared
explicitly with `@-`: `superchat.el:9@-` (the `Package-Requires` header) and
`superchat-compact.el:215@-` (the excluded-section comment). An `@-` marker
asserts only that the cited line is outside a top-level form; it does not anchor
the line's text, so a human checks those two lines once on each Kernel revision.
K5's strict gate is:

```
$ emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md
refs 806, errors 0, warnings 0
```

Any error, or any warning, fails the gate. If either `@-` line becomes a real
form, `check` reports a declared-bare construct error; `convert` does not add
the marker automatically.

## What a failure means, and recovery

- `expected construct X, actual Y` (or `actual nil`): the cited file changed and
  the line crossed a form boundary. Recovery is a re-anchor only: move the line
  number to the line the sentence describes, keep or recompute the construct
  with `convert`, and leave the entry's prose and `since:` untouched. A
  re-anchor is not a semantic revision.
- `line is out of range` / `referenced line is blank` on a short citation: the
  short citation is inheriting the wrong file. Replace it with a complete
  `file.el:NNN` citation of the file the sentence and the entry's `source:` line
  name, then `convert`. Do not "fix" it by changing the line number in the
  inherited file.
- `reader failed at position …`: the cited file does not read as Lisp; the
  checker reports it once per file and stops judging constructs in that file.
  Fix the source first.
- `short citation has no previous complete file in this entry`: the first
  citation of an entry must be complete.

## Limits to know about

- An `@-` marker is a structural assertion, not a text anchor: it verifies only
  that the cited line is outside a top-level form. Swapping the two comment lines
  while retaining the markers leaves `check --strict` green (T1 in
  `E-20260829-001`); the compensation is a human check of both lines at each
  Kernel revision.
- The anchor is the form, not the line. A line inserted above a form makes the
  citation of that form's first line fail (`actual nil`); a line inserted
  inside a form leaves every later citation green with a line number that is
  now one off. Measured on K4 (`E-20260828-001`): one line above
  `superchat-tool-execute` → 6 errors, all on `superchat-authority.el:1132`; one
  line inside its body → 0 errors while all 26 `superchat-authority.el`
  citations beyond it went stale. A green `check` therefore proves the
  construct, not the line.
- A short citation that inherits the wrong file but lands on a non-blank line
  of that file passes `check` and is annotated with the wrong construct by
  `convert`. 32 of the 40 corrections in K4 were of this kind. The only
  detection today is reading the sentence against the entry's `source:` line;
  the fix is always to write the file name out.
- Ranges are checked at both ends, in every form (`` `x.el:2-3` ``, `` `:2-3` ``,
  `` (`:2`-`:3`) ``); a misspelled mode name prints the usage line and exits 2.
  Both were gaps in an earlier draft of the checker and are closed; the ERT
  tests cover the range forms.
