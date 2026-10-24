#!/usr/bin/env python3
"""Read-only review runner. Each invocation records the exact command and status."""
import hashlib, json, os, subprocess, sys, time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
OUT = Path(__file__).resolve().parent
MANIFEST = OUT / 'review-manifest.json'
CACHE = Path.home() / '.cache/sc-codex-review/round3'
INIT = '(progn (require (quote package)) (setq package-user-dir (expand-file-name "~/.cache/superchat-test-elpa") native-comp-enable-subr-trampolines nil) (package-initialize))'
BASE = ['emacs', '-Q', '--batch', '--eval', INIT, '--eval',
        f'(setq temporary-file-directory "{CACHE}/" superchat-workflow-directory "{CACHE}/workflow/")',
        '-L', '.', '-L', 'plugins/superchat-cowork-telega']

def hashes():
    result = {}
    for top in ['scripts', 'docs', 'reports/architecture-review-2026-10-10',
                'reports/architecture-review-main-2026-10-10', '.scratch/m3-verification']:
        for p in (ROOT / top).rglob('*'):
            if p.is_file(): result[str(p.relative_to(ROOT))] = hashlib.sha256(p.read_bytes()).hexdigest()
    for p in list(ROOT.glob('*.el')) + list(ROOT.glob('*.md')) + [ROOT / '.claude/settings.local.json']:
        if p.is_file(): result[str(p.relative_to(ROOT))] = hashlib.sha256(p.read_bytes()).hexdigest()
    return result

if not MANIFEST.exists():
    data = dict(reviewed_commit='75e8337004d8f13971b70712d8e288a8d7cb123c',
                reviewed_range='fcccf3aa..75e83370', root=str(ROOT),
                model='scripted only; SMOKE_REAL and SMOKE_BASELINE removed', commands=[])
    (OUT / 'initial-hashes.json').write_text(json.dumps(hashes(), indent=2))
else:
    data = json.loads(MANIFEST.read_text())
name, *args = sys.argv[1:]
if name == 'verify-unchanged':
    start = time.time()
    old = json.loads((OUT / 'initial-hashes.json').read_text()); new = hashes()
    changes = [p for p in sorted(set(old) | set(new)) if old.get(p) != new.get(p)]
    (OUT / 'unchanged.json').write_text(json.dumps(dict(changed=changes, files=len(old)), indent=2))
    (OUT / 'verify-unchanged.log').write_text(json.dumps(dict(changed=changes, files=len(old)), indent=2) + '\n')
    data = json.loads(MANIFEST.read_text())
    data['commands'].append(dict(name=name, command=[sys.executable, str(Path(__file__).resolve()), name],
                                exit_status=int(bool(changes)), seconds=round(time.time()-start, 2),
                                log='verify-unchanged.log'))
    MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
    print(json.dumps(changes)); sys.exit(bool(changes))
if args and args[0] == 'emacs':
    args = BASE + args[1:]
env = dict(os.environ)
for key in ['SMOKE_REAL', 'SMOKE_BASELINE']: env.pop(key, None)
CACHE.mkdir(parents=True, exist_ok=True)
(CACHE / 'workflow').mkdir(exist_ok=True)
env['TMPDIR'] = str(CACHE)
start = time.time()
with (OUT / (name + '.log')).open('w') as f:
    try: rc = subprocess.run(args, cwd=ROOT, env=env, stdout=f, stderr=subprocess.STDOUT, timeout=360).returncode
    except subprocess.TimeoutExpired: rc = 124
if MANIFEST.exists(): data = json.loads(MANIFEST.read_text())
data['commands'].append(dict(name=name, command=args, exit_status=rc,
                             seconds=round(time.time()-start, 2), log=name+'.log'))
MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
print(f'{name}: exit={rc}; {time.time()-start:.2f}s; {OUT / (name + ".log")}')
if args[0] != 'emacs': print((OUT / (name+'.log')).read_text())
