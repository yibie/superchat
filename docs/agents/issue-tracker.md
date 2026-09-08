# Issue tracker: Local Markdown

Issues and specs live locally. Do not publish them to a remote tracker.

## Conventions

- One feature per directory: `.scratch/<feature-slug>/`.
- Spec: `.scratch/<feature-slug>/spec.md`.
- Implementation issues: `.scratch/<feature-slug>/issues/<NN>-<slug>.md`,
  numbered from 01, one file per issue.
- Record triage in a `Status:` line near the top of each spec or issue.
  Use the vocabulary in `triage-labels.md`.
- Append discussion under `## Comments`; preserve earlier decisions.
- Existing scratch reports remain historical records; do not automatically
  migrate or relabel them.

## Skill operations

“Publish to the issue tracker” means create the corresponding local file.
“Fetch the relevant ticket” means read the referenced local file.
Publishing does not imply a commit, push, or remote issue creation.
