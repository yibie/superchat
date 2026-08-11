#!/bin/sh
# Run the five real-environment methods for the workspace recovery domain
# with a real Ollama model and retain full traces.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
exp_dir="$repo_root/experiments/experience-learning-minimal"
scratch_root=$(mktemp -d -t superchat-recovery-five)
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"
output="${SUPERCHAT_SELF_OUTPUT:-$exp_dir/self-learning-qwen30b-1/trace.el}"

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
SUPERCHAT_RECOVERY_SELF_EXECUTE=1 \
SUPERCHAT_RECOVERY_SELF_OUTPUT="$output" \
SUPERCHAT_EXPERIMENT_MODEL="${SUPERCHAT_EXPERIMENT_MODEL:-qwen3-30b-a3b}" \
SUPERCHAT_EXPERIMENT_URL="${SUPERCHAT_EXPERIMENT_URL:-http://localhost:1234/v1/}" \
SUPERCHAT_RECOVERY_TRACE_ELPA="$elpa_root" \
emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_RECOVERY_TRACE_ELPA"))
    (package-initialize))' \
  -L "$llm_root" \
  -L "$repo_root" \
  -L "$repo_root/experiments/experience-view-internal-acceptance-v2.6" \
  -L "$repo_root/experiments/experience-view-recovery-trace" \
  -l "$exp_dir/recovery-self-learning.el"
status=$?
set -e

printf 'recovery five-method trace written: %s (status=%s)\n' "$output" "$status"
exit "$status"
