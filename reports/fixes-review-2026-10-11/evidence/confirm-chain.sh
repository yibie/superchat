#!/bin/sh
# Serial confirmation chain: one Emacs at a time, each recorded by run-checks.py.
cd /Users/chenyibin/Documents/emacs/package/superchat || exit 1
R="python3 -I reports/fixes-review-2026-10-11/evidence/run-checks.py"
$R kernel emacs -l scripts/kernel-check.el
$R smoke emacs -l scripts/smoke.el
$R object-scope emacs -l scripts/object-scope-regression.el --eval '(ert-run-tests-batch-and-exit "^object-scope/")'
$R shared-resource emacs -l scripts/shared-resource-regression.el --eval '(ert-run-tests-batch-and-exit "^shared-resource/")'
$R work-outcome emacs -l scripts/work-outcome-regression.el --eval '(ert-run-tests-batch-and-exit "^work-outcome/")'
$R pending emacs -l scripts/pending-regression.el --eval '(ert-run-tests-batch-and-exit "^pending/")'
$R annotate emacs -l scripts/annotate-regression.el --eval '(ert-run-tests-batch-and-exit "^annotate/")'
$R annotate-chat emacs -l scripts/annotate-chat-regression.el --eval '(ert-run-tests-batch-and-exit "^annotate-chat/")'
$R m7-buffer-first emacs -l scripts/m7-buffer-first-regression.el --eval '(ert-run-tests-batch-and-exit "^m7/")'
$R timer emacs -l scripts/timer-regression.el --eval '(ert-run-tests-batch-and-exit "^timer/")'
$R timer-start-fault emacs -l scripts/timer-start-fault-regression.el --eval '(ert-run-tests-batch-and-exit "^timer-fault/")'
$R unattended-notice emacs -l scripts/unattended-notice-regression.el --eval '(ert-run-tests-batch-and-exit "^unattended-notice/")'
$R work-wake-fault emacs -l scripts/work-wake-fault-regression.el --eval '(ert-run-tests-batch-and-exit "^work-wake-fault/")'
$R work-record emacs -l scripts/work-record-regression.el --eval '(ert-run-tests-batch-and-exit "^work-record/")'
$R m3 emacs -l scripts/m3-regression.el -f ert-run-tests-batch-and-exit
$R codex-domain-probes emacs -l reports/architecture-review-main-2026-10-11/evidence/domain-probes.el
$R codex-composition-probes emacs -l reports/architecture-review-main-2026-10-11/evidence/composition-probes.el
$R codex-annotation-probes emacs -l reports/architecture-review-main-2026-10-11/evidence/annotation-probes.el
$R codex-boundary-probes emacs -l reports/architecture-review-main-2026-10-11/evidence/boundary-probes.el
echo CHAIN-DONE
