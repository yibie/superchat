# Decision: The bridge speaks Anthropic's Messages API

Status: proposed

## Problem

`bridge/` has ported one API of pi's package, `openai-completions`. The
model data it carries names about 1,700 models; 709 of them use that API
and can be asked. The others cannot: `anthropic-messages` (407 models),
`bedrock-converse-stream` (193), `openai-responses` (125),
`google-generative-ai` and `google-vertex` (47), `mistral-conversations`
(37) and more. Superchat's first use is the AI layer of Emacs for any
package, and today it reaches only OpenAI-compatible endpoints.

`bridge/README.md` already puts `api/anthropic-messages` with
`providers/anthropic` in Tier 1, next to the DeepSeek pair that was
ported; `bridge/docs/PORTING.md` still marks both `todo`.

## Proposal

- Port `src/api/anthropic-messages.ts` to
  `lisp/superchat-bridge-api-anthropic-messages.el` and
  `src/providers/anthropic.ts` to
  `lisp/superchat-bridge-provider-anthropic.el`, by the rules of
  `bridge/TIER1.md`: file by file from the upstream source, semantics as in
  the source, tests translated from its tests, `scripts/check-bridge.sh`
  green, the rows of `PORTING.md` updated.
- `superchat-llm-providers` gets the entry `"anthropic"`; nothing else in
  `superchat-llm.el` changes, since it reaches every API through
  `superchat-bridge-models-stream-simple`.
- The other APIs stay `todo`; each would be a decision of its own, in the
  order of the models it opens.

## Alternatives considered

- Port `openai-responses` first. Not taken now: the bridge's own roadmap
  names Anthropic for Tier 1, and its Messages API opens more models (407
  against 125).
- Reach Anthropic through an OpenAI-compatible gateway. Not taken as the
  answer: it opens the models but not their own features (thinking blocks,
  prompt caching) that the source's adapter carries.

## Acceptance criteria

- `scripts/check-bridge.sh` prints `CHECK OK` with the new files and their
  translated tests; no test touches the network.
- With `superchat-llm-provider` `"anthropic"` and a stub HTTP seam, a
  request streams text, a tool call and its result, and a 429 reaches
  ON-ERROR with kind `rate-limit`.
- A run against the real API, with a key, answers and calls a tool.

## Risks

- The port is long: the source's adapter carries thinking, caching and
  many content kinds. `TIER1.md` says to leave out a branch whole and list
  it, never half of one.
- The upstream source is read from a temporary directory on this machine
  (`bridge/TIER1.md`); if it is gone, it must be fetched again at the same
  commit before the port starts.
- The kinds of failure in `superchat-llm.el` are read from the bridge's
  error messages; Anthropic's must be pinned by tests as the DeepSeek ones
  are.
