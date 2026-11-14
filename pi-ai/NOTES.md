# NOTES

One line per unclear point or conflict, newest last.

- `utils-retry`/`utils-provider-retry` (transform/retry port): the source's callbacks may be async; the Lisp ones are plain functions and their results are ignored. `FINISH` gets `(:ok V)` or `(:error E)`.
- `utils-provider-retry`: an error is retried only when it is a plist with `:message`, `:status` and `:headers` all present (even nil); the caller of the OpenAI-completions port must turn HTTP status >= 400 and the HTTP seam's `(:error KIND ...)` into such a plist.
- `utils-error-body`: the source reads SDK error objects; the Lisp form is a plist (keys in the file header) or a Lisp error condition. The openai-completions port must build the plist from the HTTP response (`:status`, `:error` as a hash table, `:body` as a string).
- `utils-retry` first defined its own `llm-pi-ai-abort-signal`; replaced by `llm-pi-ai-utils-abort.el`, whose `llm-pi-ai-abort-signal-aborted-p` rejects nil (so retry code uses its own `llm-pi-ai--signal-aborted-p` for a nil signal).
- `utils-sanitize-unicode` adds `llm-pi-ai-string-trim-js` and `llm-pi-ai-utf16-length`, which are not in the source; `transform-messages` and `error-body` need them.
- models/auth: `data/deepseek.json` holds `deepseek-flash` and `deepseek-v4-pro`; there is no `deepseek-v4-flash` in the 1.4.0 data.
- models: `src/api/lazy.ts` (`lazyStream`, `lazyApi`, `ProviderStreams`) has no owner; ported inside `llm-pi-ai-models.el` under the final names, move to `llm-pi-ai-api-lazy.el` when someone owns it. `ProviderStreams` also belongs in `llm-pi-ai-types.el`.
- models: `src/utils/transcript.ts` is only half ported (`createInitialSystemMessage`, `normalizeContext`) in `llm-pi-ai-utils-transcript.el`; whoever ports the rest extends that file.
- models: `src/utils/identity.ts` is not ported; `applyAuth`'s OpenCode session header and the `identity` option of `createModels` are left out, a request's `:identity` passes through untouched.
- representations chosen (not in PORTING.md §6): options are plists with keyword names of the source's fields; headers and env are alists; auth result and credential are plists; credential `:type` is the symbol `api_key`; JSON `null` in a thinking-level map is `:null`; model `compat` keys are kebab-case symbols (acronyms are words: `supportsOpenAIGrammarTools` -> `supports-open-ai-grammar-tools`), false is nil so test presence with `assq`.
- abort: `AbortController` and `AbortSignal` are one struct `llm-pi-ai-abort-signal`; HTTP's `llm-pi-ai-http-abort` takes a handle, so a streaming API must add a listener to the signal that calls it.
- event-stream: `llm-pi-ai-event-stream--time` compares a message timestamp in milliseconds with `started-at` in seconds, so its "stamped before the stream" guard never fires; `lazy-stream` stamps setup errors in milliseconds as `Date.now()` does.
- headers: a header value of nil and `:null` both mean the source's `null` (remove); `llm-pi-ai--merge-headers` keeps either as it is.
