#!/bin/sh
# Execute the frozen v2 cohort exactly once.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
scratch_root=$(mktemp -d -t superchat-acceptance-v2)
llm_root="$scratch_root/llm"
elpa_root="$scratch_root/elpa"

mkdir -p "$elpa_root"
git clone --quiet https://github.com/ahyatt/llm.git "$llm_root"
git -C "$llm_root" checkout --quiet 34ac61091994c10bda45eed54433a1a87ca26af1

# Emacs 31's built-in compat must not satisfy the private package lock.
SUPERCHAT_ACCEPTANCE_V2_ELPA="$elpa_root" emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_V2_ELPA")
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

SUPERCHAT_ACCEPTANCE_V2_EXECUTE=1 \
SUPERCHAT_ACCEPTANCE_V2_ELPA="$elpa_root" \
SUPERCHAT_ACCEPTANCE_V2_LLM_DIR="$llm_root" \
emacs -Q --batch \
  --eval '(progn
    (require '\''package)
    (setq package-user-dir (getenv "SUPERCHAT_ACCEPTANCE_V2_ELPA"))
    (package-initialize))' \
  -L "$llm_root" \
  -l "$repo_root/experiments/experience-view-internal-acceptance-v2/run.el"

printf 'v2 cohort completed; evidence root is under %s/experiments/experience-view-internal-acceptance-v2/evidence-<contract-sha256>\n' "$repo_root"
