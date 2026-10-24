# Slice 10 — stop point 2: the four parts done, consistency, the 63 old runs (not committed)

Implementer session 39daf101, 2026-10-20. All changes are in `.scratch/task-sim/harness-tools/`; `harness-goals/` is untouched (its tasks-goals.el, tasks.el, tasks-step4.el, batch.sh compare equal to the baseline copy, which they matched at the start). Nothing committed, .gitignore not touched, per ruling 2.

Evidence in this folder: `slice-10.diff` (diff -ru against `~/.cache/sc-slice10/harness-tools.before/`, copied before any change), `slice-10-files.txt` (10 files, sha256 before/after), `slice-10-consistency.tsv` (30 runs), `slice-10-leak-scan-63.tsv` (the old runs' scan).

## 10a — the checks are not in the run directory while the model runs

- The 22 checks (21 of tasks-goals.el + T1) are in `checks-goals.el`, ported to read only the disk, evidence/http, the steps in result.json and evidence/gather/. They were removed from the task files after the consistency check; prompts and `:setup` unchanged (the diff shows prompt lines only because the closing parenthesis moved onto them). 28 tasks keep their check in the sandbox (SLICE said 30; the three files load 50 tasks, 50 − 22 = 28): their result.json says `"check_in_sandbox": true`, and the reports list them apart.
- Gathering is generic (ruling 4), in run-task.el after the save and the answer: every visited fixture buffer's text, all Superchat timers, all Emacs timers (printed, bounded), every Workspace under the home and the Personal one with its sessions, Work Records (data, status, rendering) and its `work.*`/`timer.*` facts, `custom-enabled-themes`. Task entries add values only: `:gather-setup` (A1, T1 originals; P3, P3T start time — written right after :setup) and `:gather` (E2's probe). T1R needed no start time: its check does not use one.
- **When the judge comes in** (your condition). `launch.sh`:
  - lines 29–31: the harness is copied without `checks-goals.el`;
  - line 83: after the last model Emacs exits, `model emacs ended at T`;
  - lines 88–99: proxy killed; any process with its cwd or an open file under the run directory is ended (`ps` here does not show other processes' environment, so `lsof` is used);
  - line 104: `checks in the run before judging: N` (a `find` over the run, code snapshot excluded);
  - lines 105–106: copied to `RUN/judge/`, `judge copied in at T`;
  - line 107: run under `run-sandboxed.sh` (same profile);
  - lines 110–111: removed, `judge removed at T; left: N`.
  One run as an example, `T4W-real-t1-20261020-221118-60248/evidence/emacs.log`: model emacs ended at 1792559630.565, checks in the run before judging: 0, judge copied in at 1792559636.021, judge removed at 1792559636.282; left: 0. All 10 real runs of today show 0 before and 0 left, the copy 5.3–5.5 s after the model's Emacs ended (the `lsof` pass takes most of it). No run left a process to end.
- Still readable by the model: the gather code and the value entries (it can see *that* themes, timers, records are kept, not what is expected), and helper functions in tasks-goals.el that only checks use (`ts-p2-inbox-entries`, `ts-k2c-statuses`, `ts-p1w-rule-re` …) — left in place because the judge loads the task files for them and the fixture constants. Moving them is a follow-up if you want it.

## Consistency (ruling 5) — 30 runs, 0 disagreements

Before the old checks were removed, every run produced both verdicts (`slice-10-consistency.tsv`):

| set | runs | tasks | sandbox = judge | per-check disagreements |
|---|---|---|---|---|
| scripted, correct/wrong | 23 | A1 A2 C1 E2 T1 (both), C2 A3 E1 P1 P2 P3 (both), W2P (correct) | 23/23 | 0 |
| real model, D13, once each | 7 | A2 C1 DLG K2C P3T T1 T4W | 7/7 (6 pass, K2C fails in both) | 0 |

Every correct script passes and every wrong one fails under both. Not compared by any run: **P1m, P1W, R1X, W11A, T1R, W9U** (6 of 22; no scripted model and not in the surface batch). Their judges are ports of the same code and read the same gathered data as K2C/T4W/W2P, but that is not evidence.

## Pipeline after the switch — one task on each surface

`surface-batch.sh slice10-pipe 1 A2`: D13 fail, L4 pass, L4find pass; all three `judged: after the run, by checks-goals.el`, no `check_in_sandbox`, leak scan 0 hits, `summary.md` written. The D13 failure is the model's: it concluded the disk ignores case and renamed nothing (photos/ still holds `IMG_0001.JPG`); the judge read the disk correctly. Also through `batch.sh` (the 7 real runs): summary.tsv gained `tainted` and `judged` columns.

## 10b — the 63 runs of 2026-10-13

`leak-scan.py` over `runs/surface-main` (635 distinct tool calls from the wire, 520 `tool.requested` facts): **0 hits, 0 tainted**. The first pass had 2 hits, both false: a model's own plist key `:check-via-count-matches-3` (DLG L4 t1) and a sub-agent's shortened path `/Users/.../home/notes/ideas.org` (DLG L4 t2). Two rules were narrowed for them: `:check` matches only as a whole word, and a path containing `/.../` is skipped. A spot test of 8 cases (harness path, a path outside, bare `:check`, `:check-via`, /tmp, /usr/bin, an elided path, "gather") behaves as intended. Keywords: harness/, tasks-goals, checks-goals (kept, your condition), tasks.el, run-task.el, :check, gather, evidence/, profile.sb; plus absolute paths outside the run and the profile's readable directories. Note: "gather" will match ordinary English if a model writes it; such a hit needs reading.

## 10c — fingerprints

`fingerprint.py`, run by launch.sh after the judge: `fp_tools`, `fp_system`, `fp_rules` (sha256) and `fingerprint` (12 hex) in result.json. The run's own directory is replaced by `<RUN>` before hashing: without it every Workspace task had a fingerprint of its own (the system text names `RUN/home/notes/AGENTS.md`). `fp_rules` hashes the "Workspace rules, from FILE." block as sent in request 1, or is `none`. `code-commit.txt` kept. `surface-report.py` computes the fingerprint of an older run from its request file without writing into the run.

## 10d — baseline divergence

`surface-report.py --tables [--mix-fingerprints]`: clean runs (judged after the run, no leak hit) by task × surface with one count per fingerprint (different fingerprints are shown side by side, not merged, unless `--mix-fingerprints`); runs judged in the sandbox in their own table; tainted runs; and the D13 baseline per task across trials, 0 < passes < trials marked 分叉. On the 63 old runs (all judged in the sandbox): **T1 2/3 and K2C 1/3 are 分叉**; A2, C1, P3T, T4W, DLG 3/3. (Added at slice 11d: K2C's 1/3 was obtained under a check that rewarded the wrong behaviour — it passed records left continuable with a round already queued, against the user's "the rest next time"; see `slice-11d-k2c.md`.) `surface-aggregate.py` is unchanged and still averages across fingerprints; surface-batch.sh now writes `summary.md` from surface-report.py instead.

## Spending

10 real runs today (7 consistency + 3 pipeline), DeepSeek through the proxy. 23 scripted runs, no model.

## Not done / open

- 6 moved checks never compared by a run (above).
- Helper functions only checks use are still in the run directory.
- A process the model starts that survives with neither cwd nor an open file under the run would not be found before judging.
- No commit, by ruling 2.
