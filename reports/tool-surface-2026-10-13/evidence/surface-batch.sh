#!/opt/homebrew/bin/bash
# surface-batch.sh LABEL TRIALS TASK... -- three surfaces per task, interleaved, serial.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd); root=$(cd "$here/.." && pwd)
label=$1; trials=$2; shift 2
out="$root/runs/surface-$label"; mkdir -p "$out"; : > "$out/runs.txt"
L4="read_buffer,edit_buffer,eval_elisp,open_file"
one() { # one SURFACE TASK TRIAL
  local s=$1 t=$2 n=$3 line
  for attempt in 1 2 3; do
    case $s in
      D13)    line=$("$here/launch.sh" "$t" real "$s-t$n" 2>&1 | grep '^run:' | sed 's/^run: //');;
      L4)     line=$(TS_TOOLS="$L4" "$here/launch.sh" "$t" real "$s-t$n" 2>&1 | grep '^run:' | sed 's/^run: //');;
      L4find) line=$(TS_TOOLS="$L4" TS_EXTRA_LISP=ts-discovery.el "$here/launch.sh" "$t" real "$s-t$n" 2>&1 | grep '^run:' | sed 's/^run: //');;
    esac
    grep -q 'startup stall' "$line/evidence/result.json" 2>/dev/null || break
    echo "$t $s t$n startup stall, attempt $attempt: $line" >> "$out/stalls.txt"
  done
  echo "$line" >> "$out/runs.txt"
  echo "$(date +%H:%M:%S) $t $s t$n -> $(python3 -I -c "import json,sys; d=json.load(open('$line/evidence/result.json')); print(d.get('pass'), d.get('status'), round(d.get('seconds',0) or 0))" 2>/dev/null) $line"
}
for t in "$@"; do
  for n in $(seq 1 "$trials"); do
    for s in D13 L4 L4find; do one "$s" "$t" "$n"; done
  done
done
python3 "$here/surface-report.py" $(cat "$out/runs.txt") > "$out/summary.tsv"
echo "done: $out/summary.tsv"
