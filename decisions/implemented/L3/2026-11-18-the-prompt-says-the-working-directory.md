# Decision: The prompt says where a relative path of a tool resolves

Status: implemented

## Problem

A path of a tool call is relative to the scope (`SPEC.md`, L2): the project
root, or the user's home directory when the buffer a timeline was started
from is in no project. Nothing told the model which of the two. In a
directory that is not a project, an instruction to change `main.py` was
read against the home directory: the file was not found, or a new one was
written in the wrong place. The benchmark met it in its first run, and it
is quiet — no error, only a wrong result.

## Decision

`superchat-context-system` gives the working directory as the second part
of the prompt, before the rules file: the scope, or the home directory for
`"home"`, with the line that a tool's relative path is against it and that
`shell` runs there. A timeline with no entry has no scope, so it has no
such line. The scope keeps its meaning: a directory that is no project is
still `"home"`.

## Alternatives considered

- Make `superchat-run` in a directory that is not a project an error, or
  ask the person first. Not taken: use 3, the personal agent, is at home,
  and a run from an ordinary directory is part of it.
- Resolve a relative path against the directory the run was started from.
  Not taken: the scope is `"home"` and carries no path, so this would put a
  second notion of working directory beside the scope.
- Say it in the base instructions. Not taken: the directory differs by
  timeline, and the base is one constant for every request.
- Warn the person when a chat opens outside a project. Not taken: at home
  it is the normal case, and a message that is always shown is not read;
  the model, which chooses the paths, is the one that needs to know.

## Consequences

Every request of a timeline with a scope is longer by one line. A chat
started outside a project says in its prompt that paths are against the
home directory, so the model writes an absolute path instead of guessing.
