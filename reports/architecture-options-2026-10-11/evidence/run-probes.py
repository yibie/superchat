#!/usr/bin/env python3
"""Read-only probe runner for reports/architecture-options-2026-10-11.

Every Emacs probe runs against the exported snapshot of 75e83370 in
~/.cache/sc-arch-research/src, never against the live tree.  One Emacs at a
time.  Each invocation appends the exact command, exit status, seconds and
log name to probe-manifest.json.  `init` hashes the live repository's product,
test and protected files; `verify-unchanged` compares them again.

Usage:
  run-probes.py init
  run-probes.py <name> emacs -l evidence/<probe>.el [more emacs args]
  run-probes.py <name> <any other command...>
  run-probes.py verify-unchanged
"""
import hashlib, json, os, subprocess, sys, time
from pathlib import Path

LIVE = Path('/Users/chenyibin/Documents/emacs/package/superchat')
SNAPSHOT = Path.home() / '.cache/sc-arch-research/src'
CACHE = Path.home() / '.cache/sc-arch-research/tmp'
OUT = Path(__file__).resolve().parent
MANIFEST = OUT / 'probe-manifest.json'
INIT = ('(progn (require (quote package)) '
        '(setq package-user-dir (expand-file-name "~/.cache/superchat-test-elpa") '
        'native-comp-enable-subr-trampolines nil) (package-initialize))')
BASE = ['emacs', '-Q', '--batch', '--eval', INIT, '--eval',
        f'(setq temporary-file-directory "{CACHE}/" superchat-workflow-directory "{CACHE}/workflow/")',
        '-L', '.', '-L', 'plugins/superchat-cowork-telega']


def hashes():
    result = {}
    for top in ['scripts', 'docs', 'plugins', 'skills', '.scratch/m3-verification',
                'reports/architecture-review-2026-10-10',
                'reports/architecture-review-main-2026-10-10',
                'reports/architecture-review-main-2026-10-11']:
        base = LIVE / top
        if not base.exists():
            continue
        for p in base.rglob('*'):
            if p.is_file():
                result[str(p.relative_to(LIVE))] = hashlib.sha256(p.read_bytes()).hexdigest()
    for p in list(LIVE.glob('*.el')) + list(LIVE.glob('*.md')):
        if p.is_file():
            result[str(p.relative_to(LIVE))] = hashlib.sha256(p.read_bytes()).hexdigest()
    return result


def load_manifest():
    if MANIFEST.exists():
        return json.loads(MANIFEST.read_text())
    return dict(snapshot_commit='75e8337004d8f13971b70712d8e288a8d7cb123c',
                snapshot_dir=str(SNAPSHOT), live_root=str(LIVE),
                model='scripted transport only; no real provider', commands=[])


def main():
    name, *args = sys.argv[1:]
    data = load_manifest()
    if name == 'init':
        (OUT / 'initial-hashes.json').write_text(json.dumps(hashes(), indent=2, sort_keys=True))
        MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
        print('hashed', len(json.loads((OUT / 'initial-hashes.json').read_text())), 'live files')
        return 0
    if name == 'verify-unchanged':
        start = time.time()
        old = json.loads((OUT / 'initial-hashes.json').read_text())
        new = hashes()
        changed = [p for p in sorted(set(old) | set(new)) if old.get(p) != new.get(p)]
        report = dict(changed=changed, files=len(old))
        (OUT / 'unchanged.json').write_text(json.dumps(report, indent=2))
        data['commands'].append(dict(name=name, command=[sys.executable, str(Path(__file__).resolve()), name],
                                     exit_status=int(bool(changed)), seconds=round(time.time() - start, 2),
                                     log='unchanged.json'))
        MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
        print(json.dumps(report))
        return int(bool(changed))
    if args and args[0] == 'emacs':
        args = BASE + [a if not a.startswith('evidence/') else str(OUT / a[len('evidence/'):]) for a in args[1:]]
    env = dict(os.environ)
    for key in ['SMOKE_REAL', 'SMOKE_BASELINE']:
        env.pop(key, None)
    CACHE.mkdir(parents=True, exist_ok=True)
    (CACHE / 'workflow').mkdir(exist_ok=True)
    env['TMPDIR'] = str(CACHE)
    start = time.time()
    log = OUT / (name + '.log')
    with log.open('w') as f:
        try:
            rc = subprocess.run(args, cwd=SNAPSHOT, env=env, stdout=f, stderr=subprocess.STDOUT,
                                timeout=900).returncode
        except subprocess.TimeoutExpired:
            rc = 124
    data = load_manifest()
    data['commands'].append(dict(name=name, command=args, cwd=str(SNAPSHOT), exit_status=rc,
                                 seconds=round(time.time() - start, 2), log=name + '.log'))
    MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
    print(f'{name}: exit={rc}; {time.time() - start:.1f}s; {log}')
    return rc


if __name__ == '__main__':
    sys.exit(main())
