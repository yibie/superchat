#!/bin/sh
# Serial chain, one Emacs at a time, each recorded in manifest.json by run-checks.py.
# SNAP = read-only export of 981719f4 (the tree the implementer had before 87db657c), made with `git archive`.
cd /Users/chenyibin/Documents/emacs/package/superchat
R="python3 -I reports/guidance-2026-10-11/evidence/run-checks.py"
SNAP=$HOME/.cache/sc-guide/src-981719f4
LIVE=/Users/chenyibin/Documents/emacs/package/superchat
# 1. Red-first check: HEAD's pending-regression.el (its fixtures are identical in both trees) against the pre-fix product code.
GUIDE_ROOT=$SNAP $R pending-suite-on-981719f4 emacs -l $LIVE/scripts/pending-regression.el --eval '(ert-run-tests-batch-and-exit "^pending/")'
# 2. My review probe on the pre-fix snapshot: what the implementer would have seen.
GUIDE_ROOT=$SNAP $R fr-pending-probes-on-981719f4 emacs -l $LIVE/reports/fixes-review-2026-10-11/evidence/fr-pending-probes.el
# 3. The same probe on HEAD: which assertions flip after the fixes.
$R fr-pending-probes-on-head emacs -l reports/fixes-review-2026-10-11/evidence/fr-pending-probes.el
# 4. The suites the three commits name, on HEAD.
$R pending-suite emacs -l scripts/pending-regression.el --eval '(ert-run-tests-batch-and-exit "^pending/")'
$R work-outcome-suite emacs -l scripts/work-outcome-regression.el --eval '(ert-run-tests-batch-and-exit "^work-outcome/")'
$R work-wake-fault-suite emacs -l scripts/work-wake-fault-regression.el --eval '(ert-run-tests-batch-and-exit "^work-wake-fault/")'
$R command-load-suite emacs -l scripts/command-load-regression.el -f ert-run-tests-batch-and-exit
$R long-history-suite emacs -l scripts/long-history-regression.el -f ert-run-tests-batch-and-exit
# 5. My FR-02 / FR-08 probes on HEAD: the remount wake should now be refused, the acquire fault should still reproduce.
$R fr-scope-probes-on-head emacs -l reports/fixes-review-2026-10-11/evidence/fr-scope-probes.el
echo CHAIN-1-DONE
