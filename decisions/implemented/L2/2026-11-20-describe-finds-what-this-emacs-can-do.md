# Decision: describe finds what this Emacs can do

Status: implemented

## Problem

Step 1 of
[../../proposed/L0/2026-11-20-what-this-emacs-can-do.md](../../proposed/L0/2026-11-20-what-this-emacs-can-do.md):
a model working in Emacs guesses function names, and a package can have
hundreds of them.

## Decision

- A deferred `read` tool `describe` answers from Emacs's own
  documentation, in layers: a prefix's public functions are counted by
  group when there are more than 30, listed with first doc lines when
  fewer or when a query matches; a function gives its arglist and
  documentation; a buffer its mode and the commands its local keymap
  binds. No list passes 30 lines without saying how many more there are.
- `superchat-tools.el`'s cap goes from 900 to 1000: tool definitions stay
  in that one file, as the person asked.

## Alternatives considered

- List every function of a package at once. Not taken: telega alone has
  hundreds, past any useful length.
- Register chosen functions by hand. Not taken as the base: the
  documentation is already there for every package.

## Consequences

- Keys shown are global ones; a mode's keys are found through `buffer`.
- Steps 2 and 3 of the proposal (recipes, a skill's interface) remain
  proposed.
