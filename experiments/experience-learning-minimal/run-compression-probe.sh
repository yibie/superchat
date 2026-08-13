#!/bin/bash
# Run the gzip-lm style compression probe over real recovery episodes.
set -euo pipefail
repo=$(cd "$(dirname "$0")/../.." && pwd)
exp_dir="$repo/experiments/experience-learning-minimal"
run_dir="$exp_dir/compression-probe-2"
mkdir -p "$run_dir"
output="$run_dir/verdict.el"
if [ -f "$output" ]; then
  echo "Refusing to overwrite $output" >&2
  exit 1
fi

SUPERCHAT_COMPRESSION_PROBE_EXECUTE=1 \
SUPERCHAT_COMPRESSION_PROBE_OUTPUT="$output" \
  emacs -Q --batch \
  -L "$exp_dir" \
  -L "$repo/experiments/experience-view-recovery-trace" \
  -l "$exp_dir/compression-probe.el" \
  2>&1 | grep -vE "Warning|terms|free software|Switching|Loading|Agentic Skills"
