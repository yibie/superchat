#!/bin/sh
# Compile pi-ai/lisp with warnings as errors and run pi-ai/test, in a copy
# under the temporary directory.  usage: scripts/check-pi-ai.sh [test-file-stem ...]
src="$(cd "$(dirname "$0")/.." && pwd)" || exit 1
work=$(mktemp -d "${TMPDIR:-/tmp}/pi-ai-check.XXXXXX") || exit 1
trap 'rm -rf "$work"' EXIT
cp -R "$src/pi-ai/lisp" "$src/pi-ai/test" "$work"/ || exit 1
[ -d "$src/pi-ai/data" ] && cp -R "$src/pi-ai/data" "$work"/
cd "$work" || exit 1
EMACS="${EMACS:-emacs}"
fail=0
mkdir "$work/elc"
for f in lisp/*.el; do
  "$EMACS" -Q --batch -L lisp \
    --eval "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$work/elc\")))" \
    -f batch-byte-compile "$f" 2>&1 | grep -v '^$' && fail=1
  [ -f "$work/elc/$(basename "$f")c" ] || { echo "COMPILE $f failed"; fail=1; }
done
tests=""
if [ $# -gt 0 ]; then for m in "$@"; do tests="$tests -l test/$m.el"; done
else for t in test/*-test.el; do [ -f "$t" ] && tests="$tests -l $t"; done; fi
if [ -n "$tests" ]; then
  HOME="$work/home" "$EMACS" -Q --batch -L lisp -L test $tests -f ert-run-tests-batch-and-exit > "$work/test.log" 2>&1 || fail=1
  grep -E '^Ran |^   FAILED|^Test .* condition:|^    \(' "$work/test.log"
fi
echo "pi-ai lines: $(cat lisp/*.el | wc -l | tr -d ' ')"
[ -d "$src" ] || { echo "DANGER the repository directory is gone"; fail=1; }
[ "$fail" = 0 ] && echo "CHECK OK" || echo "CHECK FAILED"
exit $fail
