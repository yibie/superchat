#!/bin/sh
# Provision the frozen private boundary, then run the no-chat v2 preflight.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

# Process gate: compile/pure-load/hash-check before provisioning dependencies.
emacs -Q --batch -L "$repo_root" \
  -l "$repo_root/experiments/experience-view-internal-acceptance-v2.6/verify-runner.el" \
  --eval '(superchat-acceptance-v26-verify-runner)'

scratch_root=$(mktemp -d -t superchat-acceptance-v2.6)
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"
manifest="$scratch_root/launch-preflight.el"

mkdir -p "$elpa_root"
git clone --quiet https://github.com/yibie/llm.git "$llm_root"
git -C "$llm_root" fetch --quiet origin \
  f6a83bb05e45377d5166821b84aa178c9164b55b
git -C "$llm_root" checkout --quiet f6a83bb05e45377d5166821b84aa178c9164b55b

# `compat' is built into Emacs 31, so package-installed-p would silently
# accept the wrong copy.  Passing INTERACTIVE=t to package-install permits the
# archive package to replace that built-in copy inside this private ELPA.
SUPERCHAT_ACCEPTANCE_V26_ELPA="$elpa_root" emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_V26_ELPA")
          package-install-upgrade-built-in t
          package-archives '\''(("gnu" . "https://elpa.gnu.org/packages/")
                                ("melpa" . "https://melpa.org/packages/")))
    (package-initialize)
    (package-refresh-contents)
    (dolist (name '\''(compat plz plz-event-source plz-media-type))
      (let ((description (cadr (assq name package-archive-contents))))
        (unless (package-desc-p description)
          (error "No GNU ELPA descriptor for %s" name))
        (package-install description nil t))))'

SUPERCHAT_ACCEPTANCE_V26_PREFLIGHT=1 \
SUPERCHAT_ACCEPTANCE_V26_ELPA="$elpa_root" \
SUPERCHAT_ACCEPTANCE_V26_LLM_DIR="$llm_root" \
SUPERCHAT_ACCEPTANCE_V26_OUTPUT="$manifest" \
emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_V26_ELPA"))
    (package-initialize))' \
  -L "$llm_root" \
  -l "$repo_root/experiments/experience-view-internal-acceptance-v2.6/preflight.el"

printf 'v2.6 no-chat preflight passed; manifest=%s\n' "$manifest"
