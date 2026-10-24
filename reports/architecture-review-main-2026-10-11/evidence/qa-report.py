#!/usr/bin/env python3
"""Check this review's links, evidence inventory and recorded test outcomes."""
import hashlib
import json
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
REPORT = HERE.parent / 'REPORT.md'
manifest_path = HERE / 'review-manifest.json'
manifest = json.loads(manifest_path.read_text())
report = REPORT.read_text()
links = re.findall(r'\]\(([^)]+)\)', report)
missing = [link for link in links
           if not link.startswith(('http:', 'https:', '#'))
           and not (REPORT.parent / link.split('#')[0]).exists()]
assert not missing, missing
missing_logs = [c['log'] for c in manifest['commands']
                if c.get('log') and not (HERE / c['log']).exists()]
assert not missing_logs, missing_logs
final_probes = {
    'domain-probes.el': ('domain-probes-final.log', 4),
    'annotation-probes.el': ('annotation-probes-02.log', 4),
    'open-items-probes.el': ('open-items-probes.log', 1),
    'boundary-probes.el': ('boundary-probes.log', 2),
    'composition-probes.el': ('composition-probes.log', 3),
}
for filename, (logname, count) in final_probes.items():
    source = (HERE / filename).read_text()
    assert 'lexical-binding: t' in source.splitlines()[0], filename
    assert len(re.findall(r'^\(ert-deftest r3-', source, re.M)) == count, filename
    assert f'Ran {count} tests, {count} results as expected, 0 unexpected' in (HERE / logname).read_text(), logname
commands = {c['name']: c for c in manifest['commands']}
for name in ('baseline-probes', 'delta-probes', 'notice-probe'):
    assert commands[name]['exit_status'] == 1, name
for name in ('domain-probes-final', 'annotation-probes-02', 'open-items-probes',
             'boundary-probes', 'composition-probes', 'm3', 'smoke', 'kernel'):
    assert commands[name]['exit_status'] == 0, name
assert 'SMOKE OK (0 mismatches)' in (HERE / 'smoke.log').read_text()
assert '137 results as expected, 0 unexpected, 2 skipped' in (HERE / 'm3.log').read_text()
assert [f'R3-{i:02}' for i in range(1, 9)] == re.findall(r'^### (R3-\d\d)：', report, re.M)
required = ['CLAUDE.md', 'AGENTS.md', 'CONTEXT.md', 'docs/architecture.md',
            'docs/roadmap.md', 'docs/agents/domain.md', 'docs/agents/development.md',
            'reports/architecture-review-main-2026-10-10/index.html']
required += [str(p.relative_to(ROOT)) for p in sorted((ROOT / 'docs/decisions/accepted').glob('2026-10-10-*.md'))]
manifest['required_input_sha256'] = {
    name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in required
}
manifest['final_probe_artifacts'] = {
    name: {'sha256': hashlib.sha256((HERE / name).read_bytes()).hexdigest(),
           'final_log': logname, 'observation_tests': count}
    for name, (logname, count) in final_probes.items()
}
manifest['result_interpretation'] = {
    'baseline-probes/delta-probes/notice-probe': 'Exit 1 is expected: six old gap assertions no longer hold; F3 positive guard still passes. Files unchanged.',
    'domain-probes/domain-probes-02/domain-probes-03': 'Own-probe construction errors (duplicate fixture ERT definitions, then parentheses), not product defects; retained logs.',
    'annotation-probes': 'Own fixture default-directory was deleted before View recreation; own probe fixed and rerun as annotation-probes-02.',
    'own_final_probes': '14 observation assertions include reproduced defects, accepted limitations and positive guards; green is not product acceptance.',
    'm3': '139 total: 137 expected, 2 real-provider skips, 0 unexpected. Protected sources not edited.',
}
manifest['execution_boundaries'] = {
    'emacs_checks': 'Serial, emacs -Q --batch; same package initialization as round 2; no real model APIs.',
    'scratch': str(Path.home() / '.cache/sc-codex-review/round3'),
    'not_run': ['.scratch/task-sim/', 'real provider tests', 'GUI acceptance', 'whole-package byte compilation', 'hard process crash / power-loss'],
    'read_only': 'Only this output directory written in the worktree; initial-hashes.json / unchanged.json verify protected inputs.',
}
manifest_path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2))
result = dict(local_links_checked=len(links), missing_links=missing, missing_logs=missing_logs,
              final_observation_tests=sum(n for _, n in final_probes.values()),
              confirmed_findings=8, required_input_hashes=len(required),
              report_sha256=hashlib.sha256(REPORT.read_bytes()).hexdigest())
(HERE / 'report-qa.json').write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n')
print(json.dumps(result, ensure_ascii=False, indent=2))
