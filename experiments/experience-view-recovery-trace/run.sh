#!/bin/sh
# Run one diagnostic P-missing cell and retain a full local trace.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
scratch_root=$(mktemp -d -t superchat-recovery-trace)
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"
output="$repo_root/experiments/experience-view-recovery-trace/trace-p-missing.el"

if [ -e "$output" ]; then
  output="$repo_root/experiments/experience-view-recovery-trace/trace-p-missing-$(date +%Y%m%d-%H%M%S).el"
fi

mkdir -p "$elpa_root"
git clone --quiet https://github.com/yibie/llm.git "$llm_root"
git -C "$llm_root" fetch --quiet origin \
  f6a83bb05e45377d5166821b84aa178c9164b55b
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
SUPERCHAT_RECOVERY_TRACE_EXECUTE=1 \
SUPERCHAT_RECOVERY_TRACE_ELPA="$elpa_root" \
SUPERCHAT_RECOVERY_TRACE_OUTPUT="$output" \
emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_RECOVERY_TRACE_ELPA"))
    (package-initialize))' \
  -L "$llm_root" \
  -l "$repo_root/experiments/experience-view-recovery-trace/trace.el"
status=$?
set -e

printf 'recovery trace written: %s\n' "$output"
exit "$status"
