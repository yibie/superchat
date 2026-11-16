#!/bin/sh
# Run the gates, compile with warnings as errors, run the tests.
# usage: scripts/check.sh [L1 | module ...]   (no argument: every test;
# a layer such as L1: the tests of its modules, as SPEC.md's table says)
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
# The gates read the repository itself: what they check is the files, not a run.
"$EMACS" -Q --batch -l "$src/scripts/gates.el" "$src" 2>&1 || fail=1
sh "$src/scripts/hooks/commit-msg" --self-test || fail=1
mkdir "$work/elc"
for f in *.el; do
  "$EMACS" -Q --batch --eval "$INIT" -L . -L bridge-lisp \
    --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$work/elc\")))" \
    -f batch-byte-compile "$f" 2>&1 | grep -v '^$' && fail=1
  [ -f "$work/elc/${f}c" ] || { echo "COMPILE $f failed"; fail=1; }
done
tests=""
if [ $# -gt 0 ]; then
  for m in "$@"; do
    case "$m" in
      L[0-9]) for f in $(sed -n "s/^| $m | \`superchat-\([a-z]*\)\.el\` .*/\1/p" "$src/SPEC.md"); do
                [ -f "test/test-$f.el" ] && tests="$tests -l test/test-$f.el"; done ;;
      *) tests="$tests -l test/test-$m.el" ;;
    esac
  done
else for t in test/test-*.el; do [ -f "$t" ] && tests="$tests -l $t"; done; fi
if [ -n "$tests" ]; then
  HOME="$work/home" "$EMACS" -Q --batch --eval "$INIT" -L . -L bridge-lisp -L test $tests -f ert-run-tests-batch-and-exit > "$work/test.log" 2>&1 || fail=1
  grep -E '^Ran |^   FAILED|^Test .* condition:|^    \(' "$work/test.log"
fi
[ -d "$src" ] || { echo "DANGER the repository directory is gone"; fail=1; }
[ "$fail" = 0 ] && echo "CHECK OK" || echo "CHECK FAILED"
exit $fail
