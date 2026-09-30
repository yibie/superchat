# Decision: a reference page on running ERT tests in the user's Emacs

Status: accepted — user, 2026-09-29: "可以提供一个 ert 参考资料".

## 一句话

模拟测试里“跑一下这个文件的测试”这个任务（T5），模型常在 ERT 的内部函数上反复猜错：调用只能在批处理里用、会退出 Emacs 的函数，调用不存在的函数，把庞大的统计对象直接打印出来，最后把工具次数用光或停在半句话上。给它一页像按键参考那样的说明：怎么加载测试文件、怎么只跑这个文件的测试、怎么把结果读成简单的数据、哪些做法不要用。模型遇到相关任务时自己去读。

## Evidence

`.scratch/task-sim/FOURTH-BATCH-REPORT.md`, T5 runs of 2026-09-29: calls
to `ert-run-tests-batch-and-exit` ("only for use in batch mode") in nearly
every run; void functions guessed (`ert-stats-completed-unexpected-tests`,
`ert--stats-test-printable-result`, `ert-test-result-type`); "Apparently
circular structure being printed" from returning the stats object; 9 to 15
tool errors in a run; runs ending on the tool budget or a half answer.  The
denote-journal skill experiment (2026-09-27) went from 12, 11 and 31 tool
calls to 7, 9 and 5 with a page to read.

## Decision

1. A page `emacs-ert-tests` joins `superchat-reference--pages`: read the test
   file before loading it; load it with its directory on `load-path`; run
   only its tests with `ert-run-tests-batch` and a name regexp; read the
   stats into plain data (`ert-stats-total`, `ert-stats-completed-unexpected`,
   `ert-stats-skipped`, each test's name, result type and condition); list
   what a selector picks with `ert-select-tests`; do not call
   `ert-run-tests-batch-and-exit`, select `t`, or open the `*ert*` window;
   a clean run in a separate `emacs --batch` through `call-process`, whose
   exit status is 0 only when every test passed; report per test.
   Every call on the page was run in Emacs 32, batch and interactive.
2. Its pointers: an `eval_elisp` error "only for use in batch mode" or
   "function definition is void: ert-" points the model at the page.
3. The page is advice, as every page is.  The index line it adds is part of
   the system text wherever `read_reference` is offered; smoke baseline A
   changes by that line only, after the user sees the diff.

## Alternatives considered

**A skill instead of a reference page.**  A skill is the user's to install;
running tests is general Emacs work every user meets, the same class as key
bindings, which already has a page.

**Put the recipe in the operating note.**  Lost: every request that can
change Emacs would carry it; the page is read when the task is about tests.

## Acceptance

- `scripts/ert-reference-regression.el`: the page is within the limits, the
  index stays within 600 characters, the pointers find it, and the page's
  own load and run forms, read from its text and evaluated against a fixture,
  report the right passes, failure and skip.
- The full sweep passes; smoke baseline A changes by the index line only.
- A real batch of T5 shows whether the page changes behaviour.

## Measured (2026-09-29)

T5 with a real model, six runs each, side by side: with the page 6/6 passed,
median 4 tool calls, no tool error in any run, median 16 s, the page read in
every run; without it (6a0565ea) 5/6, median 18.5 calls, 5 to 15 tool errors
a run, median 63 s, `ert-run-tests-batch-and-exit` tried in every run
(`.scratch/task-sim/runs/batch-seventh-page`, `batch-seventh-nopage`).
Offline: `scripts/ert-reference-regression.el` 3 tests, red before the page
at the gap; 4 mutants caught.  Full sweep green with smoke baseline A's new
index line (the user: "基准可以改", 2026-09-30).
