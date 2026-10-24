#!/bin/sh
cd /Users/chenyibin/Documents/emacs/package/superchat
R="python3 -I reports/guidance-2026-10-11/evidence/run-checks.py"
$R actor-probe emacs -l reports/guidance-2026-10-11/evidence/actor-probe.el
FR_TIMEOUT=900 $R fr-settle-probes-on-head emacs -l reports/fixes-review-2026-10-11/evidence/fr-settle-probes.el
echo CHAIN-2-DONE
