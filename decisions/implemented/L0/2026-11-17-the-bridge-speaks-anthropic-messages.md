# Decision: The bridge speaks Anthropic's Messages API

Status: implemented

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

## Decision

- `src/api/anthropic-messages.ts` is ported to
  `bridge/lisp/superchat-bridge-api-anthropic-messages.el` and
  `src/providers/anthropic.ts` to
  `bridge/lisp/superchat-bridge-provider-anthropic.el`, by the rules of
  `bridge/TIER1.md`, with their tests translated; `bridge/docs/PORTING.md`
  marks both done and lists each branch left out whole.
- `superchat-llm-providers` has the entry `"anthropic"`, second after
  `"deepseek"`; nothing else in `superchat-llm.el` changes.
- An HTTP error reads as the SDK's: the status, a space, the body's JSON, so
  `superchat-llm.el` reads its kind as it reads DeepSeek's.
- Left out whole: OAuth keys (`sk-ant-oat`) and the Claude Code identity
  they carry, GitHub Copilot, federation, the SDK's client options and
  default headers, and grammar sampling. A request that needs one ends with
  an error event.
- The other APIs stay `todo`.

## Alternatives considered

- Port `openai-responses` first. Not taken now: the bridge's own roadmap
  names Anthropic for Tier 1, and its Messages API opens more models (407
  against 125).
- Reach Anthropic through an OpenAI-compatible gateway. Not taken as the
  answer: it opens the models but not their own features (thinking blocks,
  prompt caching) that the source's adapter carries.

## Consequences

- With a key in `ANTHROPIC_API_KEY` (or `ANTHROPIC_AUTH_TOKEN`), the 17
  Claude models of `bridge/data/anthropic.json` can be asked, with thinking,
  tool calls and prompt caching.
- Tests in `test/test-llm.el` pin it through `superchat-llm-request` with a
  stub HTTP seam: a tool call streamed in pieces comes back as a call, the
  next request carries the `tool_use` and its `tool_result`, and a 429 is
  one failure of kind `rate-limit`.
- No run has been made against the real API: there was no key. The README
  says so.
- Other providers whose data names `anthropic-messages` models
  (openrouter, vercel-ai-gateway and others) are not wired to this API.

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
