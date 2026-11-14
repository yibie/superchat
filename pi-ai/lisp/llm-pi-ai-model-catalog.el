;;; llm-pi-ai-model-catalog.el --- Model catalogs read from data/*.json  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/model-catalog.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/', and of
;; the one thing the generated `src/providers/<id>.models.ts' files do:
;; import `data/<id>.json'.
;;
;; The source's type machinery (`ChatModelCatalog' and friends) exists
;; for TypeScript's checker only and is not ported.  What is ported:
;; `flattenModelCatalog' and its three public forms.
;;
;; Representation.  A catalog's JSON file is an object of apis, each an
;; object of "TYPE:ID" keys to model objects.  `read-file' returns
;; GROUPS, an alist ((API . ((KEY . MODEL) ...)) ...) of strings and
;; `llm-pi-ai-model' structs (`llm-pi-ai-image-model' and
;; `llm-pi-ai-classifier-model' for those types).  `flatten-*' return an
;; alist ((ID . MODEL) ...) of the models of one type: the source's
;; `Object.fromEntries', so a later model with the same id replaces the
;; value of the earlier one, which keeps its place.
;;
;; The fields of a model object become struct slots:
;;
;;   baseUrl contextWindow maxTokens ...  kebab-case slots
;;   type                    the symbol `chat', `image' or `classifier'
;;   reasoning               t or nil
;;   input, output           lists of strings
;;   cost                    `llm-pi-ai-model-cost', its `tiers' being
;;                           `llm-pi-ai-model-cost-tier's
;;   inputLimits             `llm-pi-ai-model-input-limits' and its parts
;;   headers                 alist (NAME . VALUE), JSON null -> nil
;;   thinkingLevelMap        alist ((LEVEL . VALUE)): VALUE a string, or
;;                           `:null' for JSON null; an absent level is
;;                           absent from the alist
;;   promptCache             alist ((RETENTION . SECONDS))
;;   samplingParams, samplingParamsByThinkingLevel
;;                           alists with string keys as in the file
;;   compat                  alist whose keys are kebab-case SYMBOLS
;;                           (`supportsStrictMode' -> `supports-strict-mode',
;;                           `supportsOpenAIGrammarTools' ->
;;                           `supports-open-ai-grammar-tools'); JSON true
;;                           is t and false is nil, so test presence with
;;                           `assq', not the value, where the source tells
;;                           `false' from `undefined'; objects inside it
;;                           are alists with string keys, arrays lists.
;;
;; Fields the slots do not have (`enabled', `free', `lab', `providers'
;; of the files' model-picker entries) are ignored.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'llm-pi-ai-types)

(defvar llm-pi-ai-data-directory
  (expand-file-name
   "../data/"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory of the model catalog files `<provider>.json'.
It is the `data/' directory next to `lisp/'.")

;;; JSON to structs

(defun llm-pi-ai-model-catalog--kebab (name)
  "Return NAME, a camelCase string, as kebab-case: acronyms are words."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1-\\2"
      (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2" name)))))

(defun llm-pi-ai-model-catalog--field (object key)
  "Return the value of KEY, a string, in OBJECT, a parsed JSON alist."
  (alist-get (intern key) object))

(defun llm-pi-ai-model-catalog--bool (value)
  "Return t for JSON true, else nil."
  (eq value t))

(defun llm-pi-ai-model-catalog--plain (value)
  "Return the parsed JSON VALUE with objects as string-keyed alists.
`:false' becomes nil; `t', `:null', numbers and strings stay."
  (cond ((eq value :false) nil)
        ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (mapcar (lambda (entry)
                   (cons (symbol-name (car entry))
                         (llm-pi-ai-model-catalog--plain (cdr entry))))
                 value))
        ((listp value) (mapcar #'llm-pi-ai-model-catalog--plain value))
        (t value)))

(defun llm-pi-ai-model-catalog--compat (object)
  "Return the `compat' OBJECT as an alist with kebab-case symbol keys."
  (mapcar (lambda (entry)
            (cons (intern (llm-pi-ai-model-catalog--kebab (symbol-name (car entry))))
                  (llm-pi-ai-model-catalog--plain (cdr entry))))
          object))

(defun llm-pi-ai-model-catalog--thinking-level-map (object)
  "Return the `thinkingLevelMap' OBJECT as an alist.
The entries are (LEVEL . STRING), or (LEVEL . :null) for JSON null."
  (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry))) object))

(defun llm-pi-ai-model-catalog--cost (object)
  "Return the `cost' OBJECT as an `llm-pi-ai-model-cost'."
  (let ((field (lambda (obj key) (or (llm-pi-ai-model-catalog--field obj key) 0))))
    (llm-pi-ai-make-model-cost
     :input (funcall field object "input")
     :output (funcall field object "output")
     :cache-read (funcall field object "cacheRead")
     :cache-write (funcall field object "cacheWrite")
     :tiers (mapcar
             (lambda (tier)
               (llm-pi-ai-make-model-cost-tier
                :input (funcall field tier "input")
                :output (funcall field tier "output")
                :cache-read (funcall field tier "cacheRead")
                :cache-write (funcall field tier "cacheWrite")
                :input-tokens-above (funcall field tier "inputTokensAbove")))
             (llm-pi-ai-model-catalog--field object "tiers")))))

(defun llm-pi-ai-model-catalog--input-limits (object)
  "Return the `inputLimits' OBJECT as an `llm-pi-ai-model-input-limits'."
  (let* ((images (llm-pi-ai-model-catalog--field object "images"))
         (resize (llm-pi-ai-model-catalog--field images "resize")))
    (llm-pi-ai-make-model-input-limits
     :max-request-bytes (llm-pi-ai-model-catalog--field object "maxRequestBytes")
     :images
     (and images
          (llm-pi-ai-make-model-image-input-limits
           :max-per-message (llm-pi-ai-model-catalog--field images "maxPerMessage")
           :max-per-request (llm-pi-ai-model-catalog--field images "maxPerRequest")
           :resize
           (and resize
                (llm-pi-ai-make-model-image-resize-options
                 :max-width (llm-pi-ai-model-catalog--field resize "maxWidth")
                 :max-height (llm-pi-ai-model-catalog--field resize "maxHeight")
                 :max-bytes (llm-pi-ai-model-catalog--field resize "maxBytes")
                 :jpeg-quality (llm-pi-ai-model-catalog--field resize "jpegQuality"))))))))

(defun llm-pi-ai-model-catalog-model-from-json (object)
  "Return the model struct of OBJECT, one parsed JSON model object.
The struct is an `llm-pi-ai-image-model' or `llm-pi-ai-classifier-model'
when `type' says so, else an `llm-pi-ai-model'; see the Commentary."
  (let* ((type-name (llm-pi-ai-model-catalog--field object "type"))
         (type (and type-name (intern type-name)))
         (field (lambda (key) (llm-pi-ai-model-catalog--field object key)))
         (args (list :type type
                     :id (funcall field "id")
                     :name (funcall field "name")
                     :api (funcall field "api")
                     :provider (funcall field "provider")
                     :base-url (funcall field "baseUrl")
                     :input (funcall field "input")
                     :input-limits (let ((limits (funcall field "inputLimits")))
                                     (and limits
                                          (llm-pi-ai-model-catalog--input-limits limits)))
                     :cost (llm-pi-ai-model-catalog--cost (funcall field "cost"))
                     :headers (mapcar (lambda (entry)
                                        (cons (car entry)
                                              (unless (eq (cdr entry) :null)
                                                (cdr entry))))
                                      (llm-pi-ai-model-catalog--plain
                                       (funcall field "headers")))
                     :reasoning (llm-pi-ai-model-catalog--bool (funcall field "reasoning"))
                     :thinking-level-map (llm-pi-ai-model-catalog--thinking-level-map
                                          (funcall field "thinkingLevelMap"))
                     :prompt-cache (llm-pi-ai-model-catalog--plain (funcall field "promptCache"))
                     :context-window (funcall field "contextWindow")
                     :max-tokens (funcall field "maxTokens")
                     :sampling-params (llm-pi-ai-model-catalog--plain
                                       (funcall field "samplingParams"))
                     :sampling-params-by-thinking-level
                     (llm-pi-ai-model-catalog--plain
                      (funcall field "samplingParamsByThinkingLevel"))
                     :compat (llm-pi-ai-model-catalog--compat (funcall field "compat")))))
    (cond ((eq type 'image)
           (apply #'llm-pi-ai-make-image-model
                  :output (funcall field "output") args))
          ((eq type 'classifier) (apply #'llm-pi-ai-make-classifier-model args))
          (t (apply #'llm-pi-ai-make-model args)))))

(defun llm-pi-ai-model-catalog-from-json (object)
  "Return the groups of OBJECT, a parsed JSON catalog; see the Commentary."
  (mapcar (lambda (api-entry)
            (cons (symbol-name (car api-entry))
                  (mapcar (lambda (model-entry)
                            (cons (symbol-name (car model-entry))
                                  (llm-pi-ai-model-catalog-model-from-json
                                   (cdr model-entry))))
                          (cdr api-entry))))
          object))

(defun llm-pi-ai-model-catalog-read-file (file)
  "Return the groups of the catalog in FILE, a JSON file.
Signal an error when FILE cannot be read or parsed."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (goto-char (point-min))
    (llm-pi-ai-model-catalog-from-json
     (json-parse-buffer :object-type 'alist :array-type 'list
                        :null-object :null :false-object :false))))

(defun llm-pi-ai-model-catalog-read-provider (provider)
  "Return the groups of catalog `PROVIDER.json' in the data directory.
The directory is `llm-pi-ai-data-directory'."
  (llm-pi-ai-model-catalog-read-file
   (expand-file-name (concat provider ".json") llm-pi-ai-data-directory)))

;;; Flattening

(defun llm-pi-ai-model-catalog--flatten (groups type)
  "Return the models of TYPE in GROUPS as an alist ((ID . MODEL) ...).
This is the source's `flattenModelCatalog'; the models are compared on
their `type' slot as it is, so a model without a type is in no catalog."
  (let ((result nil))
    (dolist (group groups)
      (dolist (entry (cdr group))
        (let ((model (cdr entry)))
          (when (eq (llm-pi-ai-model-type model) type)
            (let ((existing (assoc (llm-pi-ai-model-id model) result)))
              (if existing
                  (setcdr existing model)
                (setq result (append result
                                     (list (cons (llm-pi-ai-model-id model) model))))))))))
    result))

(defun llm-pi-ai-flatten-chat-model-catalog (_provider groups)
  "Return the chat models of GROUPS as an alist ((ID . MODEL) ...).
_PROVIDER is the provider id, unused at run time as in the source's
`flattenChatModelCatalog'."
  (llm-pi-ai-model-catalog--flatten groups 'chat))

(defun llm-pi-ai-flatten-image-model-catalog (_provider groups)
  "Return the image models of GROUPS, the source's `flattenImageModelCatalog'."
  (llm-pi-ai-model-catalog--flatten groups 'image))

(defun llm-pi-ai-flatten-classifier-model-catalog (_provider groups)
  "Return the classifier models of GROUPS.
This is the source's `flattenClassifierModelCatalog'."
  (llm-pi-ai-model-catalog--flatten groups 'classifier))

(provide 'llm-pi-ai-model-catalog)

;;; llm-pi-ai-model-catalog.el ends here
