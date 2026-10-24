#!/bin/sh
# Third serial chain: the closed-loop probes, then the fold-cost re-measurement with a longer turn wait.
cd /Users/chenyibin/Documents/emacs/package/superchat || exit 1
R="python3 -I reports/fixes-review-2026-10-11/evidence/run-checks.py"
$R fr-loop-probes emacs -l evidence/fr-loop-probes.el
FR_TIMEOUT=3000 $R p3-fold-cost-3 emacs -l evidence/p3-fold-cost.el
echo CHAIN3-DONE
