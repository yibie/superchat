#!/bin/sh
# Confirmation layer on HEAD (18811e04): kernel, smoke, M3 run-only (the M3 set is leader-only to modify; this only runs it).
cd /Users/chenyibin/Documents/emacs/package/superchat
R="python3 -I reports/guidance-2026-10-11/evidence/run-checks.py"
$R kernel emacs -l scripts/kernel-check.el
$R smoke emacs -l scripts/smoke.el
$R m3 emacs -l scripts/m3-regression.el -f ert-run-tests-batch-and-exit
echo CHAIN-3-DONE
