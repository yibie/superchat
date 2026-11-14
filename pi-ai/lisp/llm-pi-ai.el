;;; llm-pi-ai.el --- Entry file of the pi-ai port  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/index.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; The source's index is "core only, side-effect free": it exports types,
;; the event stream, `models', the auth modules, and a few utils, and it
;; exports no `stream' or `streamSimple' function.  Streaming goes through
;; a Models collection: `llm-pi-ai-create-models', then
;; `llm-pi-ai-models-set-provider', then `llm-pi-ai-models-stream' or
;; `llm-pi-ai-models-stream-simple' (`Models.stream' and
;; `Models.streamSimple').  So this file defines no function: it requires
;; what the index exports and that exists in Lisp, and provides
;; `llm-pi-ai'.
;;
;; Provider factories and API implementations are not loaded here, as in
;; the source (`providers/*' and `api/*' are separate entries):
;; `(require \='llm-pi-ai-provider-deepseek)' gives
;; `llm-pi-ai-deepseek-provider', which loads
;; `llm-pi-ai-api-openai-completions' on its first request.
;;
;; Exports of `src/index.ts' that have no Lisp file yet: `typebox'
;; (`Type', `Static', `TSchema'), `api/*' option types (they are types),
;; `models-store', `providers/faux', `session-resources',
;; `utils/diagnostics', `utils/identity', `utils/typebox-helpers',
;; `utils/uuid', `utils/validation', `utils/version',
;; `compat/extension-oauth-types'.  See `docs/PORTING.md'.

;;; Code:

(require 'llm-pi-ai-types)
(require 'llm-pi-ai-event-stream)
(require 'llm-pi-ai-models)
(require 'llm-pi-ai-auth)
(require 'llm-pi-ai-utils-json-parse)
(require 'llm-pi-ai-utils-overflow)
(require 'llm-pi-ai-utils-retry)
(require 'llm-pi-ai-utils-text)
(require 'llm-pi-ai-utils-transcript)

(provide 'llm-pi-ai)

;;; llm-pi-ai.el ends here
