# Decision: Facts of the project learned in dreaming

Status: proposed

## Problem

Runs relearn the same things about a project. In the person's own use
(timeline `20261118-165024-f2af`) the model checked a word ceiling with
`wc -w` again and again, had edits fail because a formatter ran on save,
and guessed the names of private functions with `eval_elisp`, failing
fifteen times. What one run learned the hard way, the next run did not know.
Microsoft's TokenEconomizer calls the cure "micro-skills": short facts of a
repository, learned from finished sessions and stated at the start of the
next. Its rule is to trust a fact only after three sessions confirm it, to
retire it when a session contradicts it, and never to state advice about
finishing or checking work. Notes are not that: a note is shown as soon as
it is written, once, with nothing to confirm or retire it.

## Proposal

- **L0.** A new entry kind, `fact`: `:text` the fact in one line, `:data`
  `(:key KEY :source SOURCE)` with `:refs` the entries that show it, or
  `(:key KEY :retired t)` to retire it. KEY names what the fact is about,
  so that the same fact seen again has the same key; SOURCE is `detector`
  or `model`. The Tape stays append-only: a fact is confirmed or retired by
  more entries.
- **L1.** A view `facts` for a scope: one row per KEY, its newest text, the
  **runs** its evidence comes from (the `user` entries before the entries in
  its `:refs`), the **reviews** that recorded it (the dreaming runs whose
  `fact` entries carry the KEY), and whether a retiring entry came after the
  newest of them. A fact is **active** when its evidence comes from three
  runs and two reviews recorded it, and it is not retired. Runs, not
  timelines: the person works mostly in one long timeline (42 of the 53
  runs of this project's scope are in one). Two reviews: one dreaming run
  citing three runs at once cannot make a fact active by itself.
- **L3.** The system prompt gains "Facts of this project", the active facts
  of the scope, newest confirmed first, at most 1000 characters, after the
  notes. A candidate, seen in fewer than three timelines, is never shown.
- **L4, a new module `superchat-facts.el`.**
  - A detector, no model, of **form repair**: in a run, a `shell` command
    that failed with an environmental error (command not found, an unknown
    or invalid option, a missing environment variable, the command's own
    script or program file not found), and a later command of the same
    program, in a **different form**, that succeeded. The program is the
    first word after leading `cd DIR &&` and `VAR=VALUE` parts are taken off.
    The fact is "`FAILED` fails here; `WORKED` works", keyed by the failing
    form with numbers and paths taken out. A test or a check that fails and
    then passes after a code change is no such fact: its error is no
    environmental class and its form does not change. A save that changed
    the text (the notice of L2) makes "saving `*.EXT` reformats it; read
    before editing again", keyed by the extension.
  - A tool `fact` (`key`, `text`, `sources`; class `read`, it writes only
    the Tape), and `retire` in it to retire a key, offered only to the
    dreaming run, as a chat run is offered no `send`.
  - Dreaming, once a day: runs the detector over the runs since its last
    review, writing a `fact` entry per finding in that run's timeline, then
    asks the model to read the runs that had a `redirect` or `paused` event
    and record, with `fact`, what about the project would have saved those
    rounds, citing the entries, reusing the key of a fact it saw before.
  - A fact's text is refused when it is advice about finishing or checking
    work, the words "make sure", "verify", "double-check" or "done" as whole
    words: a fact says how the project is, not how to work.
- **L5.** `/facts` lists the facts of the scope with their state and count;
  `/facts forget KEY` retires one.

## Alternatives considered

- Facts as notes. Not taken: a note is in every system prompt at once, so an
  unconfirmed fact would steer runs; a correcting note replaces it but cannot
  wait for three sightings.
- TokenEconomizer's firing gate: state a fact only while the failure it
  prevents keeps happening in 40% of the sessions that touch it. Not taken
  first: it needs to know which sessions touch a fact; it is the refinement
  once facts exist.
- A fact learned at once, in the run that found it. Not taken: a fact seen
  once is often wrong; three runs and two reviews are the evidence.
- Count timelines, as the first draft of this note did. Not taken: in one
  long timeline nothing would ever be confirmed, and every model fact sits
  in the dreaming timeline alone.
- Its five detector families. Only "form repair" is generic enough here, and
  the save notice is Superchat's own; boundary, empty probe and env value
  are for later, when the Tape shows them.
- Leave learning to the person's `AGENTS.md`. Not taken alone: it is what
  the person writes; facts are what the runs found.

## Acceptance criteria

- Before any code: the detector, replayed offline over this project's Tape
  (53 runs), lists its candidate facts and how many would become active; the
  person reads the list and decides whether to build.
- A form repair seen in three runs and recorded by two reviews makes one
  active fact; seen in two runs, or recorded by one review, it stays a
  candidate and is not in the system prompt.
- A `scripts/check.sh` that fails and later passes makes no fact.
- A retiring entry, by `/facts forget` or the tool, takes a fact out of the
  system prompt, and seeing it again later does not bring it back unless
  three timelines see it after the retirement.
- The facts block never passes 1000 characters.
- `fact` refuses "Make sure the tests pass" and accepts "Tests run with
  `scripts/check.sh`, not `make test`" and "The abandoned branch is gone";
  a chat run is not offered `fact`.
- A scripted dreaming run over a Tape with a `redirect` run records a fact
  that cites entries of that run.

## Risks

- The detector's keys decide what counts as "the same fact"; masked too
  little, nothing repeats; masked too much, unlike commands merge.
- A model proposal can be wrong three times; retiring by hand is the guard.
- The system prompt grows by up to 1000 characters in every request of the
  scope.
- Five layers change, in order L0 to L5; L3's specification is near its
  ceiling and gains a paragraph.
