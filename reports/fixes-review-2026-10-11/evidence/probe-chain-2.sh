#!/bin/sh
# Second serial chain: the three probe files after their fixture fixes, then the fold-cost re-measurement.
cd /Users/chenyibin/Documents/emacs/package/superchat || exit 1
R="python3 -I reports/fixes-review-2026-10-11/evidence/run-checks.py"
$R fr-scope-probes-2 emacs -l evidence/fr-scope-probes.el
$R fr-settle-probes-2 emacs -l evidence/fr-settle-probes.el
$R fr-pending-probes-2 emacs -l evidence/fr-pending-probes.el
$R p3-fold-cost-2 emacs -l evidence/p3-fold-cost.el
echo CHAIN2-DONE
