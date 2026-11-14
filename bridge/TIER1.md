# Tier 1: one streamed chat request to DeepSeek

`bridge/` is the port of `@earendil-works/pi-ai@1.4.0` (commit
`58c0b02ce552adab76014e7ed712ab7981359aa8`), now kept inside Superchat. Read
`README.md` and `docs/PORTING.md` (conventions in its §6) and the four finished
files in `lisp/` before writing: new files follow their style, naming and
header.

The upstream TypeScript is at
`/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/d271af03-d802-485a-ac45-8f8fd170cd99/scratchpad/pi-upstream/packages/ai`
(read-only). Port from it file by file. Semantics follow the source; do not
improve it and do not add what it does not have.

## Goal of this tier

    (superchat-bridge-stream-simple MODEL CONTEXT OPTIONS)  ; -> assistant-message event stream

works for a DeepSeek model: text, thinking, tool calls, usage, stop reason,
errors, abort. Everything not needed for that stays `todo` in PORTING.md.

## Rules

- Stay inside `bridge/`. Edit only the files you were given. Do not read any
  other Superchat checkout, nor `~/Documents/emacs/package/llm`.
- Emacs 30.1, lexical binding, no dependency outside Emacs itself and `curl`.
- Run `scripts/check-bridge.sh` (from the repository root) before handing back;
  it must print `CHECK OK`. It compiles with warnings as errors and runs the
  tests in a temporary copy. Never run tests inside the repository.
- A test deletes only what it made with `make-temp-file`. No test touches the
  network; no test needs an API key.
- Every ported function names its source function in its docstring, as the
  finished files do. Tests are translated from the upstream `test/*.test.ts`
  where one exists; otherwise write tests from the source's behaviour.
- What the source does that you leave out (Copilot headers, Cloudflare,
  telemetry, OAuth, images, classifiers, websockets): leave it out whole, and
  list it in your hand-back so PORTING.md can say `skip` or `todo` with the
  reason. Do not leave half of a branch.
- Update your files' rows in `docs/PORTING.md`.
- No silent error handling: an error reaches the caller or becomes the
  stream's `error` event, as in the source.
- Something unclear or a conflict with another file: one line in
  `bridge/NOTES.md`, and go on.

## The HTTP seam (no upstream file: upstream uses the `openai` npm client)

`lisp/superchat-bridge-http.el`, as `docs/HTTP.md` recommends: `make-process` + `curl`.

    (superchat-bridge-http-request &key url method headers body timeout
                            on-headers on-chunk on-done)  ; -> HANDLE
    (superchat-bridge-http-abort HANDLE)

- HEADERS is an alist of strings. BODY is a unibyte or multibyte string, sent
  as UTF-8 bytes on curl's stdin (`--data-binary @-`); it never appears in the
  argument list. TIMEOUT is seconds for the whole request, nil for none.
- ON-HEADERS, optional: called once with (STATUS HEADERS), STATUS an integer,
  HEADERS an alist with lower-case names, before any chunk.
- ON-CHUNK: called with each piece of the response body as a decoded string,
  as it arrives. A multibyte character split across two reads is delivered
  whole, in the later call.
- ON-DONE: called exactly once, with a plist:
  `(:status N :headers ALIST)` when the response ended, whatever N is;
  `(:error KIND :message S)` otherwise, KIND one of `curl-missing`, `network`,
  `timeout`, `aborted`. After `superchat-bridge-http-abort`, ON-CHUNK is not called
  again and ON-DONE is called once with `aborted`.
- The curl program and the function that starts the process are variables, so
  a test can feed bytes in pieces without a network.

    (superchat-bridge-sse-parser ON-EVENT)  ; -> function taking one chunk string

- Returns a function to pass body chunks to. ON-EVENT is called with
  (EVENT DATA) for each complete server-sent event: EVENT the `event:` field
  or nil, DATA the `data:` lines joined with newlines. Handles CRLF, comments,
  events split across chunks.

## Files and owners

| File | Source |
|---|---|
| `superchat-bridge-http.el` | none (above) |
| `superchat-bridge-models.el` | `src/models.ts`: `createProvider`, `calculateCost`, `getSupportedThinkingLevels`, `clampThinkingLevel`, `hasApi`, `modelsAreEqual`, and of `createModels` what a `streamSimple`/`stream` call through a provider needs (auth resolution for an API key, request transforms). Stores, refresh, telemetry, images, classifiers, deferred: `todo`. |
| `superchat-bridge-model-catalog.el` | `src/model-catalog.ts`: `flattenChatModelCatalog` and what it needs, reading `data/<provider>.json`. |
| `superchat-bridge-auth.el` | `src/auth/helpers.ts` `envApiKeyAuth` (resolve only), `src/auth/types.ts`, `src/auth/context.ts`, `src/auth/resolve.ts` as far as an API key from the environment needs. `src/env-api-keys.ts`. |
| `superchat-bridge-provider-deepseek.el` | `src/providers/deepseek.ts` |
| `superchat-bridge-api-transform-messages.el` | `src/api/transform-messages.ts` |
| `superchat-bridge-utils-*.el` | `sanitize-unicode`, `hash`, `headers`, `error-body`, `provider-env`, `provider-retry`, `retry`, `overflow`: each as its user needs it |
| `superchat-bridge-api-openai-completions.el` | `src/api/openai-completions.ts`, over `superchat-bridge-http.el` in place of the `openai` client |
| `superchat-bridge.el` | entry: requires, and `superchat-bridge-stream-simple` / `superchat-bridge-stream` as `src/index.ts` exposes them |
