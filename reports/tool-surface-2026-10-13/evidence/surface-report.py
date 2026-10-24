#!/usr/bin/env python3
"""surface-report.py RUNDIR... -> one TSV row per run, from evidence files only.
Columns: run, task, trial, surface, pass, status, seconds, requests, tool_calls,
failed_calls, not_provided, enable_calls, enabled, req1_tools, req1_tools_bytes, tools_per_request
"""
import json, os, sys, re
def names(req):
    return [t.get("function", {}).get("name") or t.get("name") for t in req.get("tools") or []]
rows = []
for run in sys.argv[1:]:
    ev = os.path.join(run, "evidence")
    try: d = json.load(open(os.path.join(ev, "result.json")))
    except Exception as e: d = {"harness_error": str(e)}
    http = os.path.join(ev, "http")
    reqs = sorted(f for f in os.listdir(http)) if os.path.isdir(http) else []
    reqs = [f for f in reqs if f.endswith("-request.json")]
    per = []; req1 = []; req1_bytes = 0; provided = set()
    for i, f in enumerate(reqs):
        try: r = json.load(open(os.path.join(http, f)))
        except Exception: continue
        n = names(r); per.append(n); provided |= set(n)
        if i == 0:
            req1 = n; req1_bytes = len(json.dumps(r.get("tools") or [], ensure_ascii=False).encode())
    calls = d.get("tool_calls", [])
    failed = sum(1 for c in calls if c.get("status") not in ("succeeded", None))
    notprov = sum(1 for c in calls if c.get("tool") and c["tool"] not in provided and c["tool"] != "ask_user")
    enable = [c for c in calls if c.get("tool") == "enable_tools"]
    enabled = ";".join(re.sub(r'[^A-Za-z0-9_,]', '', str(c.get("args", ""))) for c in enable)
    base = os.path.basename(run); m = re.match(r"([A-Za-z0-9]+)-real-(.+?)-(\d{8}-\d{6})", base)
    task = d.get("task") or (m.group(1) if m else "?"); label = m.group(2) if m else "?"
    surface, _, trial = label.rpartition("-t") if "-t" in label else (label, "", "")
    rows.append([base, task, trial or "", surface, d.get("pass"), d.get("status"), round(d.get("seconds", 0) or 0, 1),
                 len(per), len(calls), failed, notprov, len(enable), enabled,
                 ",".join(x for x in req1 if x), req1_bytes,
                 " | ".join(",".join(x for x in p if x) for p in per), d.get("harness_error", "")])
print("\t".join("run task trial surface pass status seconds requests tool_calls failed_calls not_provided enable_calls enabled req1_tools req1_tools_bytes tools_per_request harness_error".split()))
for r in rows: print("\t".join(str(x) for x in r))
