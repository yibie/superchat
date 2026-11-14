;;; superchat-bridge.el --- Entry file of the pi-ai port  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/index.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; The source's index is "core only, side-effect free": it exports types,
;; the event stream, `models', the auth modules, and a few utils, and it
;; exports no `stream' or `streamSimple' function.  Streaming goes through
;; a Models collection: `superchat-bridge-create-models', then
;; `superchat-bridge-models-set-provider', then `superchat-bridge-models-stream' or
;; `superchat-bridge-models-stream-simple' (`Models.stream' and
;; `Models.streamSimple').  So this file defines no function: it requires
;; what the index exports and that exists in Lisp, and provides
;; `superchat-bridge'.
;;
;; Provider factories and API implementations are not loaded here, as in
;; the source (`providers/*' and `api/*' are separate entries):
;; `(require \='superchat-bridge-provider-deepseek)' gives
;; `superchat-bridge-deepseek-provider', which loads
;; `superchat-bridge-api-openai-completions' on its first request.
;;
;; Exports of `src/index.ts' that have no Lisp file yet: `typebox'
;; (`Type', `Static', `TSchema'), `api/*' option types (they are types),
;; `models-store', `providers/faux', `session-resources',
;; `utils/diagnostics', `utils/identity', `utils/typebox-helpers',
;; `utils/uuid', `utils/validation', `utils/version',
;; `compat/extension-oauth-types'.  See `docs/PORTING.md'.

;;; Code:

(require 'superchat-bridge-types)
(require 'superchat-bridge-event-stream)
(require 'superchat-bridge-models)
(require 'superchat-bridge-auth)
(require 'superchat-bridge-utils-json-parse)
(require 'superchat-bridge-utils-overflow)
(require 'superchat-bridge-utils-retry)
(require 'superchat-bridge-utils-text)
(require 'superchat-bridge-utils-transcript)

(provide 'superchat-bridge)

;;; superchat-bridge.el ends here
