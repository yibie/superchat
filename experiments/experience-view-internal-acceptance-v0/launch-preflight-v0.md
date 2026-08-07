# Experience View Internal Acceptance — Launch Preflight v0

Status: rejected before any provider request  
Date: 2026-08-11  
Contract: [`experience-view-internal-acceptance-contract-v0.md`](../../docs/design/experience-view-internal-acceptance-contract-v0.md), SHA-256 `85179efbb728fbda148cf8b95dc1b12d9263055fa9888a8c42d23bccfddfe2ab`

## Recorded environment

- Mutable jj working copy: change `oyyuxnvpztqw`, commit `55d2b11c3c00`.
- Emacs: GNU Emacs `31.0.91`.
- Ollama binary and local `GET /api/version`: `0.32.6`.
- Endpoint inspected: `http://localhost:11434`.
- Actual `POST /api/chat` transport invocations: `0`.
- Cohort cells begun: `0/7`; retries, replacement runs, and manual
  completions: `0`.

## Product source seal

All 15 source digests required by the frozen contract matched exactly:

```text
20807812e04e795946b9ef872885fa3b446a473597a401a74f532c7c04b9ead0  superchat.el
4f48e7fe43a469ec1aee2739524fba2b4d1daff14f6e4cb06ce3ca44d8dd556a  superchat-runtime.el
71614821b7cf74ef6b047d841403b546fd29f20f28ca783f8e4433d57a79b462  superchat-llm.el
428a645ec3986c6fe0ac64dc4b2634ce0fa6f7c00a05378cc672efbdbab52937  superchat-authority.el
c7fd05abc3d38d657daf3b94f3ba1ef54c6be6744df293601d436de5d0459634  superchat-workspace.el
335305878437903fcfa36cd4e6cd64d03a3ff3a1d11d1751214a82ac56128531  superchat-experience.el
8e6e4bad3ce76b759794da84ea8c03d2f43d7bf4c165350fd92c4b99f086d198  superchat-closure.el
68b0d592c9c797f10e94b81e09155f515a9c2c64bd40038bc566ba9ec68fcf86  superchat-tape.el
4597d6623de642e12d7dc210a4a0c3fbbe650e96e9a7063b24d009073430867f  superchat-projection.el
09458c6356e30199e11ac0cd779e7be9d14ea59db73cf80de5b5a3bd032e878f  superchat-preset.el
45e6d459d8bcd168e1173f0e0092ba5e214e23bd2ca68b41fa582490cd8ff779  superchat-tools.el
c8b6a8e91842f1d00131cf68f3d1238f194f80404298a7f111634319c922db48  superchat-dispatcher.el
362fec26a1dc776a095f687395f76e26c53b6e0d39f6fe3c18cdf1dfef8d4f0b  superchat-prompt-hooks.el
f2aae4b32cee04ca049e43286dd2cfeca316b952b2b8ce734b58418ea70aaab8  superchat-agent-loop.el
f4a76b0239f0041f7e895e3622df820ac1aaf85bec5aedfca7b59077fe0b1536  superchat-db.el
```

## Rejection

The frozen reference cell requires `llm-ollama` from llm.el `0.31.1`, package
commit `34ac61091994c10bda45eed54433a1a87ca26af1`, and the adapter file digest.
Both an ordinary batch Emacs and `emacs -Q --batch`, after
`(require 'package)` and `(package-initialize)`, returned `nil` from
`locate-library` for `llm` and `llm-ollama`.  Consequently the preflight could
not load the frozen adapter or record its version, commit, capabilities, or
SHA-256.  This is a contract-defined infrastructure mismatch, so no provider
request was permitted.

The frozen contract therefore did not reach any Acceptance cell and cannot
authorize the default-on change.  The canonical ERT suite was not run: it
cannot repair this failed launch preflight or substitute for the seven required
provider cells.

Any future attempt needs a newly frozen contract after installing and pinning
the required adapter; it must not reuse this rejected contract revision.
