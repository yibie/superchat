# Local Model Optimization Notes

## Current baseline

- Local `ping` benchmark shows `qwen3.5:4b` is the fastest usable local model, around 8.96s.
- `qwen3.5:9b` is slower, around 16.27s, but still much more realistic than `27b/35b` for interactive local use.
- `qwen3.5:0.8b` timed out even with a 60s `ping` budget in the current environment.
- Source benchmark: [docs/benchmarks/ai-local-ping-compare-latest.json](/Users/chenyibin/Documents/emacs/package/superchat/threadnote-mvp/electron/docs/benchmarks/ai-local-ping-compare-latest.json)

## Newly confirmed local bottleneck

- For `qwen3.5:9b` on Ollama, the main failure mode was not pure latency. It was hidden reasoning output.
- With default Ollama behavior, the model can spend the entire `num_predict` budget in the `thinking` channel and leave `response` empty or truncated.
- This showed up as:
  - `done_reason = "length"`
  - `eval_count > 0`
  - `responseChars = 0` or incomplete JSON
  - large `thinking` payloads
- A direct low-level probe confirmed that `think: false` restores usable JSON output for the same prompt class.
- Threadnote now applies `think: false` by default for all Ollama local models, not just `qwen`.
- Diagnostic entrypoint: `npm run benchmark:ollama-probe -- --model qwen3.5:9b --scenario prepare`

## Why target `qwen3.5:9b`

- `4b` is faster, but may trade away too much reasoning quality for Threadnote's structured `prepare/resume` tasks.
- `9b` is the smallest local model in this environment that still looks like a plausible quality target for meaningful synthesis.
- `27b/35b` are too slow for the current interactive target.

## `omlx` ideas worth borrowing

Reference repo: [jundot/omlx](https://github.com/jundot/omlx)

- OpenAI-compatible local serving.
  `omlx` serves on an OpenAI-compatible API, which matches Threadnote's provider abstraction well. This means future backend swaps do not need product-layer rewrites.

- Tiered KV cache and prefix reuse.
  `omlx` emphasizes hot RAM cache plus SSD cache, block-based KV reuse, and prefix sharing. This is directly relevant because Threadnote repeatedly sends stable system prompts and similar task scaffolding for `prepare` and `resume`.

- Long-prefill survivability.
  `omlx` explicitly mentions SSE keep-alive during long prefill. Threadnote currently waits for one full response, so long local prefill is invisible to the user until completion.

- Per-model controls.
  `omlx` exposes per-model memory, TTL, pinning, and batch settings. The general lesson is that local serving should be treated as an actively tuned runtime, not a fixed black box.

- Built-in benchmark mindset.
  `omlx` measures prefill and generation separately. This aligns with the direction Threadnote should move toward for local-model optimization.

## `omlx` ideas that are not first priority here

- Continuous batching is not the first lever.
  Threadnote's current problem is single-user first response, not multi-user throughput. Batching may help server efficiency, but it does not guarantee better cold single-request latency.

- Multi-model pool management is useful later.
  Right now the higher-value path is to make one local target model feel responsive enough before adding runtime complexity.

## Concrete implications for Threadnote

- Keep optimizing around local OpenAI-compatible backends, not Ollama-specific behavior.
- Prioritize reducing prompt prefill cost for `qwen3.5:9b`.
- Disable default reasoning mode for Ollama local models when the app is asking for structured JSON output under tight completion budgets.
- Add more explicit local-model metrics next:
  - prompt bytes
  - prompt tokens if available
  - prefill-like latency proxy
  - full completion latency
- Move toward streamed local responses for visible progress instead of waiting for `stream: false` completion.

## Immediate next steps

- Benchmark `qwen3.5:9b` `prepare` and `resume` with wider budgets to learn the true latency range.
- Trim fixed prompt scaffolding and repeated context first, before changing product behavior.
- Evaluate whether a future local backend swap to an OpenAI-compatible server with stronger KV reuse is justified.
