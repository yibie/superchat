#!/bin/sh
# Compile with warnings as errors, run every test, check the line caps.
# usage: scripts/check.sh [module ...]   (no argument: everything present)
# Everything runs in a copy under the temporary directory: a test that
# deletes or writes where it should not cannot reach the repository.
src="$(cd "$(dirname "$0")/.." && pwd)" || exit 1
work=$(mktemp -d "${TMPDIR:-/tmp}/superchat-check.XXXXXX") || exit 1
trap 'rm -rf "$work"' EXIT
cp "$src"/*.el "$work"/ 2>/dev/null
mkdir "$work/test" && cp "$src"/test/*.el "$work/test"/ 2>/dev/null
cp -R "$src/bridge/lisp" "$work/bridge-lisp" && cp -R "$src/bridge/data" "$work/data" || exit 1
cd "$work" || exit 1
ELPA="${SUPERCHAT_TEST_ELPA:-$HOME/.cache/superchat-test-elpa}"
EMACS="${EMACS:-emacs}"
command -v "$EMACS" >/dev/null 2>&1 || EMACS=Emacs
INIT="(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))"
fail=0
caps="superchat-tape:550 superchat-view:500 superchat-llm:450 superchat-tools:800 superchat-permission:250 superchat-agent:700 superchat-context:250 superchat-recall:200 superchat-chat:1100 superchat-inline:600 superchat-plugin:150 superchat:120"
total=0
for pair in $caps; do
  name=${pair%%:*}; cap=${pair##*:}
  [ -f "$name.el" ] || continue
  n=$(wc -l < "$name.el" | tr -d ' ')
  total=$((total + n))
  if [ "$n" -gt "$cap" ]; then echo "CAP  $name.el: $n lines, cap $cap"; fail=1; else echo "ok   $name.el: $n/$cap"; fi
done
for f in *.el; do
  case " $caps " in *" ${f%.el}:"*) ;; *) echo "CAP  $f is not a module of SPEC.md"; fail=1 ;; esac
done
echo "total $total/6000"
[ "$total" -gt 6000 ] && { echo "CAP  total over 6000"; fail=1; }
banned=$(grep -HnE '\((ignore-errors|bound-and-true-p|featurep)\b|\((fboundp|boundp) +.superchat|^\(cl-defstruct|^\(cl-defgeneric|^\(defcustom' *.el | grep -v '^superchat\.el:.*defcustom')
[ -n "$banned" ] && { echo "RULE banned forms:"; echo "$banned"; fail=1; }
mkdir "$work/elc"
for f in *.el; do
  "$EMACS" -Q --batch --eval "$INIT" -L . -L bridge-lisp \
    --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$work/elc\")))" \
    -f batch-byte-compile "$f" 2>&1 | grep -v '^$' && fail=1
  [ -f "$work/elc/${f}c" ] || { echo "COMPILE $f failed"; fail=1; }
done
tests=""
if [ $# -gt 0 ]; then for m in "$@"; do tests="$tests -l test/test-$m.el"; done
else for t in test/test-*.el; do [ -f "$t" ] && tests="$tests -l $t"; done; fi
if [ -n "$tests" ]; then
  HOME="$work/home" "$EMACS" -Q --batch --eval "$INIT" -L . -L bridge-lisp -L test $tests -f ert-run-tests-batch-and-exit > "$work/test.log" 2>&1 || fail=1
  grep -E '^Ran |^   FAILED|^Test .* condition:|^    \(' "$work/test.log"
fi
[ -d "$src" ] || { echo "DANGER the repository directory is gone"; fail=1; }
[ "$fail" = 0 ] && echo "CHECK OK" || echo "CHECK FAILED"
exit $fail
