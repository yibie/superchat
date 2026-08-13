#!/bin/bash
# Verify the auto annotation path against real experiment databases
# (copies only; originals are never touched).
set -euo pipefail
repo=$(cd "$(dirname "$0")/../.." && pwd)
exp_dir="$repo/experiments/experience-learning-minimal"
run_dir="$exp_dir/auto-layer-verify-1"
workdir="$(mktemp -d /tmp/superchat-auto-verify.XXXXXX)"
mkdir -p "$run_dir"
output="$run_dir/verdict.el"
if [ -f "$output" ]; then
  echo "Refusing to overwrite $output" >&2
  exit 1
fi

SUPERCHAT_AUTO_VERIFY_EXECUTE=1 \
SUPERCHAT_AUTO_VERIFY_OUTPUT="$output" \
SUPERCHAT_AUTO_VERIFY_WORKDIR="$workdir" \
  emacs -Q --batch \
  -L "$repo" \
  -L "$exp_dir" \
  -L "$repo/test" \
  -l "$exp_dir/auto-layer-verify.el" \
  2>&1 | grep -vE "Warning|terms|free software|Switching|Loading|Agentic Skills"
