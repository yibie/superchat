#!/usr/bin/env python3
"""Read-only runner for reports/guidance-2026-10-11 (copied from the fixes review runner).
GUIDE_ROOT in the environment runs the command in a snapshot directory instead of the live tree.

Runs every Emacs check against the live tree at the repository root (the
other session is not running anything here meanwhile), one at a time.  Each
invocation appends the exact command, exit status, seconds and log name to
manifest.json.  `init` hashes the product, test, doc, plugin, protected and
earlier-report files; `verify-unchanged` compares them again.

Usage:
  run-checks.py init
  run-checks.py <name> emacs -l evidence/<probe>.el [more emacs args]
  run-checks.py <name> <any other command...>
  run-checks.py verify-unchanged
"""
import hashlib, json, os, subprocess, sys, time
from pathlib import Path

ROOT = Path('/Users/chenyibin/Documents/emacs/package/superchat')
CACHE = Path.home() / '.cache/sc-guide'
OUT = Path(__file__).resolve().parent
MANIFEST = OUT / 'manifest.json'
INIT = ('(progn (require (quote package)) '
        '(setq package-user-dir (expand-file-name "~/.cache/superchat-test-elpa") '
        'native-comp-enable-subr-trampolines nil) (package-initialize))')
BASE = ['emacs', '-Q', '--batch', '--eval', INIT, '--eval',
        f'(setq temporary-file-directory "{CACHE}/" superchat-workflow-directory "{CACHE}/workflow/")',
        '-L', '.', '-L', 'plugins/superchat-cowork-telega']


def hashes():
    result = {}
    tops = ['scripts', 'docs', 'plugins', 'skills', '.scratch/m3-verification',
            'reports/architecture-review-2026-10-10',
            'reports/architecture-review-main-2026-10-10',
            'reports/architecture-review-main-2026-10-11',
            'reports/architecture-options-2026-10-11', 'reports/fixes-review-2026-10-11']
    for top in tops:
        base = ROOT / top
        if not base.exists():
            continue
        for p in base.rglob('*'):
            if p.is_file():
                result[str(p.relative_to(ROOT))] = hashlib.sha256(p.read_bytes()).hexdigest()
    extra = list(ROOT.glob('*.el')) + list(ROOT.glob('*.md')) + [ROOT / '.claude/settings.local.json']
    for p in extra:
        if p.is_file():
            result[str(p.relative_to(ROOT))] = hashlib.sha256(p.read_bytes()).hexdigest()
    return result


def load_manifest():
    if MANIFEST.exists():
        return json.loads(MANIFEST.read_text())
    return dict(reviewed_commits=['981719f4', '87db657c', '18811e04'],
                baseline='437ca4e8', root=str(ROOT),
                model='scripted transport only; SMOKE_REAL and SMOKE_BASELINE removed',
                commands=[])


def main():
    name, *args = sys.argv[1:]
    data = load_manifest()
    if name == 'init':
        (OUT / 'initial-hashes.json').write_text(json.dumps(hashes(), indent=2, sort_keys=True))
        MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
        print('hashed', len(json.loads((OUT / 'initial-hashes.json').read_text())), 'files')
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
        args = BASE + [str(OUT / a[len('evidence/'):]) if a.startswith('evidence/') else a
                       for a in args[1:]]
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
            rc = subprocess.run(args, cwd=os.environ.get('GUIDE_ROOT', str(ROOT)), env=env, stdout=f, stderr=subprocess.STDOUT,
                                timeout=int(os.environ.get('FR_TIMEOUT', '1500'))).returncode
        except subprocess.TimeoutExpired:
            rc = 124
    data = load_manifest()
    data['commands'].append(dict(name=name, command=args, cwd=os.environ.get('GUIDE_ROOT', str(ROOT)), exit_status=rc,
                                 seconds=round(time.time() - start, 2), log=name + '.log'))
    MANIFEST.write_text(json.dumps(data, ensure_ascii=False, indent=2))
    print(f'{name}: exit={rc}; {time.time() - start:.1f}s; {log}')
    return rc


if __name__ == '__main__':
    sys.exit(main())
