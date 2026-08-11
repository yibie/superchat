#!/bin/sh
# Execute the frozen Experience View v1 cohort exactly once.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
scratch_root=$(mktemp -d -t superchat-acceptance-v1)
evidence_root="$repo_root/experiments/experience-view-internal-acceptance-v1/evidence-$(date -u +%Y%m%dT%H%M%SZ)"
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"

cleanup() {
  rm -rf "$scratch_root"
}
trap cleanup EXIT HUP INT TERM

mkdir -p "$evidence_root"
git clone --quiet https://github.com/ahyatt/llm.git "$llm_root"
git -C "$llm_root" checkout --quiet 34ac61091994c10bda45eed54433a1a87ca26af1

SUPERCHAT_ACCEPTANCE_ELPA="$elpa_root" emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_ELPA"))
    (setq package-archives '\''(("gnu" . "https://elpa.gnu.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")))
    (package-initialize)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (package '\''(compat plz plz-event-source plz-media-type))
      (unless (package-installed-p package) (package-install package))))'

SUPERCHAT_ACCEPTANCE_V1_RUN=1 \
SUPERCHAT_ACCEPTANCE_V1_EVIDENCE_DIR="$evidence_root" \
SUPERCHAT_ACCEPTANCE_V1_LLM_DIR="$llm_root" \
SUPERCHAT_ACCEPTANCE_ELPA="$elpa_root" \
emacs -Q --batch \
  --eval '(progn (require '\''package) (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_ELPA")) (package-initialize))' \
  -L "$llm_root" \
  -l "$repo_root/experiments/experience-view-internal-acceptance-v1/run.el"
