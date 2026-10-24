#!/usr/bin/env python3
"""surface-aggregate.py summary.tsv -> markdown tables: per task x surface, and per surface."""
import csv, sys, statistics as st
from collections import defaultdict
rows = list(csv.DictReader(open(sys.argv[1]), delimiter="\t"))
S = ["D13", "L4", "L4find"]
cells = defaultdict(list)
for r in rows: cells[(r["task"], r["surface"])].append(r)
def med(xs): return st.median(xs) if xs else 0
def cell(rs):
    if not rs: return "—"
    p = sum(1 for r in rs if r["pass"] == "True"); n = len(rs)
    req = med([int(r["requests"]) for r in rs]); calls = med([int(r["tool_calls"]) for r in rs])
    sec = med([float(r["seconds"]) for r in rs]); failed = sum(int(r["failed_calls"]) for r in rs)
    en = sum(int(r["enable_calls"]) for r in rs)
    return f"{p}/{n} · {req:g} req · {calls:g} calls · {sec:.0f} s" + (f" · {failed} failed" if failed else "") + (f" · enable×{en}" if en else "")
tasks = []
for r in rows:
    if r["task"] not in tasks: tasks.append(r["task"])
print("| 任务 | " + " | ".join(S) + " |"); print("|---|" + "---|" * len(S))
for t in tasks: print(f"| {t} | " + " | ".join(cell(cells[(t, s)]) for s in S) + " |")
print(); print("| 面 | 通过 | 请求中位 | 调用中位 | 秒中位 | 失败调用 | 调了没提供的 | enable 调用 | 第1请求工具字节 |"); print("|---|---|---|---|---|---|---|---|---|")
for s in S:
    rs = [r for r in rows if r["surface"] == s]
    if not rs: continue
    p = sum(1 for r in rs if r["pass"] == "True")
    print(f"| {s} | {p}/{len(rs)} | {med([int(r['requests']) for r in rs]):g} | {med([int(r['tool_calls']) for r in rs]):g} | {med([float(r['seconds']) for r in rs]):.0f} | {sum(int(r['failed_calls']) for r in rs)} | {sum(int(r['not_provided']) for r in rs)} | {sum(int(r['enable_calls']) for r in rs)} | {med([int(r['req1_tools_bytes']) for r in rs]):g} |")
print(); print("L4find: enable_tools 点名 →")
for r in rows:
    if r["surface"] == "L4find" and r["enable_calls"] != "0": print(f"- {r['task']} t{r['trial']}: {r['enabled']} → pass={r['pass']}")
print(); print("错误与异常：")
for r in rows:
    if r["harness_error"] or r["status"] not in ("completed",): print(f"- {r['task']} {r['surface']} t{r['trial']}: status={r['status']} {r['harness_error']}")
