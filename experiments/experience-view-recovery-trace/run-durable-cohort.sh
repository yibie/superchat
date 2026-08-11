#!/bin/sh
# Run the six-cell durable cohort with a real Ollama model and retain full traces.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
exp_dir="$repo_root/experiments/experience-view-recovery-trace"
scratch_root=$(mktemp -d -t superchat-durable-cohort)
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"
output="$exp_dir/durable-cohort-5/trace.el"
timestamp=$(date +%Y%m%d-%H%M%S)

mkdir -p "$elpa_root"
git clone --quiet https://github.com/yibie/llm.git "$llm_root"
git -C "$llm_root" fetch --quiet origin f6a83bb05e45377d5166821b84aa178c9164b55b
git -C "$llm_root" checkout --quiet f6a83bb05e45377d5166821b84aa178c9164b55b

SUPERCHAT_RECOVERY_TRACE_ELPA="$elpa_root" emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_RECOVERY_TRACE_ELPA")
          package-install-upgrade-built-in t
          package-archives '\''(("gnu" . "https://elpa.gnu.org/packages/")
                                ("melpa" . "https://melpa.org/packages/")))
    (package-initialize)
    (package-refresh-contents)
    (dolist (name '\''(compat plz plz-event-source plz-media-type))
      (let ((description (cadr (assq name package-archive-contents))))
        (unless (package-desc-p description)
          (error "No package descriptor for %s" name))
        (package-install description nil t))))'

set +e
SUPERCHAT_DURABLE_COHORT_EXECUTE=1 \
SUPERCHAT_DURABLE_COHORT_OUTPUT="$output" \
SUPERCHAT_RECOVERY_TRACE_ELPA="$elpa_root" \
emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_RECOVERY_TRACE_ELPA"))
    (package-initialize))' \
  -L "$llm_root" \
  -L "$repo_root" \
  -l "$exp_dir/durable-cohort.el"
status=$?
set -e

printf 'durable cohort trace written: %s (status=%s)\n' "$output" "$status"
exit "$status"
