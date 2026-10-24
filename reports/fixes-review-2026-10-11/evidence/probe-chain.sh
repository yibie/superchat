#!/bin/sh
# Serial probe chain: one Emacs at a time, each recorded by run-checks.py.
cd /Users/chenyibin/Documents/emacs/package/superchat || exit 1
R="python3 -I reports/fixes-review-2026-10-11/evidence/run-checks.py"
$R fr-pending-load emacs -l evidence/fr-pending-load.el
$R fr-scope-probes emacs -l evidence/fr-scope-probes.el
$R fr-settle-probes emacs -l evidence/fr-settle-probes.el
$R fr-pending-probes emacs -l evidence/fr-pending-probes.el
$R fr-annotate-probes emacs -l evidence/fr-annotate-probes.el
echo PROBES-DONE
