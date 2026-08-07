# Experience View Internal Acceptance Contract v1

Status: frozen; not executed  
Frozen: 2026-08-11  
Supersedes: rejected v0 launch contract only  
Decision controlled: whether
`superchat-runtime-recovery-intervention-enabled` may change from `nil` to `t`

## Inherited boundary

This contract incorporates every product-source digest, Provider/Model,
Profile, prompt, tool, Authority, cohort, verdict, evidence, request-ceiling,
once-only, and ship/keep-off clause in
[`experience-view-internal-acceptance-contract-v0.md`](experience-view-internal-acceptance-contract-v0.md),
whose SHA-256 is
`85179efbb728fbda148cf8b95dc1b12d9263055fa9888a8c42d23bccfddfe2ab`.

The sole replacement is the launch bootstrap below.  The old v0 result remains
rejected and must not be rerun.  A later `internal-acceptance-v1` ticket may
run this new contract once, only after this bootstrap seals its manifest.

## Replaced launch bootstrap

`emacs -Q` and `--batch` intentionally skip automatic package activation.  A
bare `locate-library` result is therefore not a transport preflight.  Before
any provider request, the runner must create a fresh private scratch root and:

1. clone `https://github.com/ahyatt/llm.git` into that root and check out
   `34ac61091994c10bda45eed54433a1a87ca26af1`;
2. create a private `elpa/` directory containing the llm dependencies
   `compat`, `plz`, `plz-event-source`, and `plz-media-type`; and
3. start the runner through this exact command shape, with the two absolute
   scratch paths substituted:

   ```sh
   emacs -Q --batch \
     --eval '(progn (require '\''package) (setq package-user-dir "<scratch>/elpa") (package-initialize))' \
     -L <scratch>/llm \
     -l <acceptance-runner.el>
   ```

The runner must then require `llm` and `llm-ollama`, construct
`(make-llm-ollama :chat-model "qwen3.5:9b" :host "localhost" :port 11434)`,
and reject preflight unless both features and the provider predicate are true.
This is setup only: construction must not send a chat request.

The launch manifest must retain the private source path only as a redacted
role, plus the checked-out llm commit, llm version, `llm-ollama.el` SHA-256,
and the resolved dependency names, versions, paths, and source digests.  The
expected adapter SHA-256 for the pinned checkout is
`96141fa9c58ada05684cc1f071e865986087cc01db640f41253f17aba581f17d`.
It must retain every v0 preflight field as well.  A missing, mismatched, or
unrecordable dependency rejects this v1 contract before a provider call.

## Verified bootstrap observation

On Emacs 31.0.91, a fresh temporary package directory with the listed
dependencies and an llm checkout at the pinned commit loaded both features and
constructed the `qwen3.5:9b` Ollama provider under `emacs -Q --batch`.
No `POST /api/chat` was made.  This verifies the bootstrap shape; it does not
execute any Acceptance cell or establish a First Recovery Action.
