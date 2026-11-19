# Decision: A save leaves no backup file

Status: implemented

## Problem

`save_buffer` is the one tool that writes a file, and it called `save-buffer`
with the person's own settings around it. On an Emacs with the default
`make-backup-files` on, saving `calc.el` left `calc.el~` beside it, and the
model then saw that file in `ls` and `find` and had to work out what it was.
It is not a file the model made, it is not a file the person asked for, and
nothing in the specification said whether saving makes backups. `NOTES.md`
recorded it.

## Decision

`save_buffer` binds `make-backup-files` to nil around `save-buffer`.
`spec/L2-capabilities.md` says so: it writes the file and leaves no backup
file, whatever the person's Emacs says.

The tool's contract is one file written. A backup is an extra file in the
project the model cannot see the reason for, in a directory the model reads
with `ls`, `find` and `grep`; and a person who wants backups wants them for
their own editing, which goes through their own `save-buffer`, not through a
tool a model calls.

## Alternatives considered

- Say in the specification that saving follows the person's settings, and
  have the model told of the backup file. The file list of a project then
  depends on a variable of the person's Emacs, and every `ls` has to be read
  with that in mind, in every turn. The tool exists to make the model's
  view of the project plain.
- Turn backups off with `backup-inhibited` instead. Same effect, but
  `backup-inhibited` is also set buffer-locally when a file is visited, and
  it is a variable that says the opposite of what is meant; `make-backup-files`
  is the one that says whether a backup is made.

## Consequences

- A save through the tool writes the file and nothing else, on any Emacs.
- The person's own `save-buffer` is untouched: backups still happen where
  they set them to.
- `test/test-tools.el` holds it: with `make-backup-files` t and a
  `backup-enable-predicate` that allows the temporary directory, a save
  leaves the directory holding only its one file. Both bindings are needed:
  the predicate refuses `/tmp` by default, so the case would pass without the
  fix.
