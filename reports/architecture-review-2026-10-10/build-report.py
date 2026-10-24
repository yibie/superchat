"""Build the self-contained architectural review from a local source snapshot."""
from pathlib import Path
from html import escape
import hashlib
import json
import re

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
SOURCES = [
    ('composition', '组合不是进程隔离', 'docs/design/plugin-architecture.md', 17, 49),
    ('driver', '统一入口与请求准备', 'superchat-pipeline.el', 509, 585),
    ('services', '全局 Service，调用者 Scope', 'superchat-service.el', 32, 80),
    ('runtime', '重启修复不是执行恢复', 'superchat-runtime.el', 537, 568),
    ('permissions', '固定权限阶段与只可收紧的监听器', 'superchat-sandbox.el', 255, 363),
    ('eval', '完整 Elisp 在宿主进程执行', 'superchat-tools.el', 918, 956),
    ('ownership', '确切 buffer 对象的 ownership', 'superchat-editor-workspace.el', 119, 156),
    ('projection', 'Projection 与 checkpoint 同事务', 'superchat-projection.el', 72, 110),
    ('evidence', '请求证据实际重建', 'superchat-invariant.el', 98, 123),
    ('recall', '按 Workspace 筛选回忆 Session', 'superchat-tape-view.el', 115, 131),
    ('rules', '规则 collector 的入口限制', 'superchat-prompt-hooks.el', 517, 542),
    ('child', '子代理单独携带规则与工作快照', 'superchat-subagent.el', 1069, 1109),
    ('work-api', '按 id 读写记录，没有 Session 所属检查', 'superchat-work.el', 191, 283),
    ('work-tool', 'action 分支与静默丢弃字段', 'superchat-work.el', 461, 513),
    ('timer-api', '面向模型的全局定时器查询与取消', 'superchat-timer.el', 451, 491),
    ('wake', '持久标记先于请求提交', 'superchat-work.el', 624, 687),
    ('work-plugin', 'Work Record 的观察者与调度生命周期', 'superchat-work.el', 765, 820),
    ('tool-owner', '内置工具按 row 撤销的覆盖范围', 'superchat-tools.el', 2366, 2404),
    ('collector', 'Work Record collector 属于 chat-collectors', 'superchat-prompt-hooks.el', 988, 1050),
    ('changes', '待保存候选依赖活 buffer；终态触发审阅', 'superchat-changes.el', 218, 283),
    ('work-read', 'Work Record 全量 fold 与当前记录选择', 'superchat-work.el', 55, 70),
    ('sql', 'SQL 工具没有 Workspace 参数', 'superchat-tools.el', 1263, 1283),
    ('routing', '当前 inline Session 按来源 Workspace 解析', 'superchat.el', 3602, 3632),
    ('model-surface', '默认组合的实际行', 'superchat-plugin.el', 236, 246),
]

evidence = []
for key, title, filename, start, end in SOURCES:
    lines = (ROOT / filename).read_text().splitlines()
    snippet = '\n'.join(f'{i:4d}  {lines[i-1]}' for i in range(start, min(end, len(lines)) + 1))
    evidence.append(f'''<details id="source-{key}" class="source-detail">
      <summary><span>{escape(title)}</span><code>{escape(filename)}:{start}–{end}</code></summary>
      <pre><code>{escape(snippet)}</code></pre>
      <a class="source-file" href="../../{escape(filename)}">打开仓库源文件 ↗</a>
    </details>''')

files = sorted(ROOT.glob('superchat*.el'))
counts = sorted(((p.name, len(p.read_text().splitlines())) for p in files), key=lambda v: -v[1])
manifest = {
    'review_date': '2026-10-10',
    'worktree': str(ROOT),
    'jj_working_commit': 'de7a2011',
    'product_parent_commit': '92a03554',
    'sources': {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in files},
    'top_level_elisp_lines': sum(n for _, n in counts),
    'top_level_elisp_files': len(files),
    'largest_modules': counts[:8],
    'note': 'Line counts include comments and blank lines. Not a complexity or quality score.',
}
(HERE / 'source-manifest.json').write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + '\n')
template = (HERE / 'report.template.html').read_text()
html = template.replace('<!-- SOURCE_EXCERPTS -->', '\n'.join(evidence))
html = html.replace('{{MODULE_COUNT}}', str(len(files))).replace('{{LINE_COUNT}}', f'{sum(n for _, n in counts):,}')
(HERE / 'superchat-architecture-review.html').write_text(html)
print(json.dumps({'report': str(HERE / 'superchat-architecture-review.html'), 'bytes': len(html.encode()), 'sources': len(SOURCES)}, ensure_ascii=False))
