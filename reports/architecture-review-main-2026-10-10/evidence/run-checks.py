import os,subprocess,json,time
from pathlib import Path
root=Path('/Users/chenyibin/Documents/emacs/package/superchat')
out=Path('/private/tmp/superchat-main-review-2026-10-10')
init="(progn (require (quote package)) (setq package-user-dir (expand-file-name \"~/.cache/superchat-test-elpa\") native-comp-enable-subr-trampolines nil) (package-initialize))"
base=['emacs','-Q','--batch','--eval',init,'-L','.','-L','plugins/superchat-cowork-telega']
jobs=[('kernel',['emacs','-Q','--batch','-L','.','-l','scripts/kernel-check.el']),('smoke',base+['-l','scripts/smoke.el']),('m3',base+['-l','scripts/m3-regression.el','-f','ert-run-tests-batch-and-exit'])]
for name,prefix in [('workspace-scope','workspace-scope/'),('work-wake-fault','work-wake-fault/'),('timer-start-fault','timer-fault/'),('entry-rules','entry-rules/'),('beside-user','beside-user/'),('input-syntax','input-syntax/'),('skill-once','skill-once/'),('skill-context','skill-context/'),('unattended-notice','unattended-notice/')]:
 jobs.append((name,base+['-l','scripts/'+name+'-regression.el','--eval','(ert-run-tests-batch-and-exit "^'+prefix+'")']))
results=[]
env=dict(os.environ);env.pop('SMOKE_REAL',None)
for name,cmd in jobs:
 t=time.time()
 with (out/(name+'.log')).open('w') as f:
  try: rc=subprocess.run(cmd,cwd=root,env=env,stdout=f,stderr=subprocess.STDOUT,timeout=240).returncode
  except subprocess.TimeoutExpired: rc=124
 results.append(dict(name=name,returncode=rc,seconds=round(time.time()-t,2),command=cmd))
 (out/'checks.json').write_text(json.dumps(results,indent=2))
 print(name,rc,results[-1]['seconds'],flush=True)
