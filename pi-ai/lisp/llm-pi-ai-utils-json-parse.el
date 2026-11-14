;;; llm-pi-ai-utils-json-parse.el --- Lenient JSON parsing, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/json-parse.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;; Models get JSON wrong: they write a raw tab or newline inside a
;; string, or an escape JSON does not have (LaTeX such as
;; "\usepackage" or "\url{x}").  The source repairs such text and parses
;; it again; while a tool call streams it also accepts a cut-off prefix.
;;
;;   `llm-pi-ai-repair-json'                the source's `repairJson'
;;   `llm-pi-ai-parse-json-with-repair'     `parseJsonWithRepair'
;;   `llm-pi-ai-parse-tool-call-arguments'  `parseToolCallArguments'
;;   `llm-pi-ai-parse-streaming-json'       `parseStreamingJson'
;;
;; JSON values are Lisp's: an object is a hash table (as
;; `json-parse-string' makes it by default), an array a vector, `true'
;; t, `false' `:false', a JSON null `:null', and strings are strings.
;; The source gives `{}' where it has no object to give, so the port
;; returns `(llm-pi-ai-json-object)' there, never nil, which is JSON's
;; null.
;;
;; Differences from the source, all coming from the runtime rather
;; than from the algorithm:
;;
;; - Emacs's JSON reader takes only properly encoded UTF-8, so text it
;;   refuses to decode is then read as ISO-8859-1 instead of failing
;;   as the source would.  A byte, not a character, then stands where
;;   the model sent one.
;; - Emacs's reader rejects `NaN' and `+-Infinity', which the source's
;;   `JSON.parse' accepts.  No repair adds them here either; the
;;   source's second step decodes them the same.
;; - The source decodes a valid `\uD83D\uDE00' pair to one character
;;   and leaves an unpaired surrogate as itself.  Emacs decodes such a
;;   pair to two characters and refuses a lone surrogate, so
;;   `llm-pi-ai--decode-unicode-escape' reads the pair itself as one
;;   character, and keeps a lone surrogate's `\uXXXX' text.
;; - The source's third step is `partial-json' 0.1.7's `parse', which
;;   is not in the source tree; `llm-pi-ai--parse-partial-json' is a
;;   reimplementation of it whose docstring lists the behaviours it
;;   follows.
;;
;; `llm-pi-ai-parse-tool-call-arguments' returns what the source's
;; `Pick<ToolCall, "arguments" | "argumentsError">' holds, as an
;; alist: `arguments' first, and an `arguments-error' cell when the
;; text was not a JSON object.

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst llm-pi-ai-valid-json-escapes '(?\" ?\\ ?/ ?b ?f ?n ?r ?t)
  "Characters JSON allows after a backslash in a string.
The source's `VALID_JSON_ESCAPES'.  A `u' is checked on its own.")

(defconst llm-pi-ai--escape-regexp "\\`\\\\[/\"\\\\bfnrt]\\'"
  "A `\' and the character it escapes, as a string of exactly two.")

(defconst llm-pi-ai--hex-escape-regexp "\\`[0-9a-fA-F]\\{4\\}\\'"
  "A string of exactly four hex digits.
The source's test on the digits of a `\\u' escape.  `[:xdigit:]' is
not used: in Emacs it matches other digits than these.")

(defun llm-pi-ai-json-object (&rest keys-and-values)
  "Return a JSON object holding KEYS-AND-VALUES.
It is the hash table `json-parse-string' makes of an object, whose
test is `equal', so that it compares with another object by content.
The source's `{}' is the object with nothing in it."
  (let ((object (make-hash-table :test #'equal)))
    (cl-loop for (key value) on keys-and-values by #'cddr
             do (puthash key value object))
    object))

(defun llm-pi-ai--string-append (parts)
  "Return PARTS, a list of strings, joined into one string."
  (apply #'concat (nreverse parts)))

;;; Repair

(defun llm-pi-ai-control-character-p (char)
  "Return non-nil when CHAR, a character, is one JSON does not allow raw.
That is U+0000 to U+001F, the source's `isControlCharacter'."
  (<= char #x1f))

(defun llm-pi-ai-escape-control-character (char)
  "Return the JSON escape for the control CHAR, a character.
The source's `escapeControlCharacter': the short escape for the five
characters that have one, `\\uXXXX' for the others.  The four hex
digits are lower case, as the source writes them."
  (cl-case char
    (?\b "\\b")
    (?\f "\\f")
    (?\n "\\n")
    (?\r "\\r")
    (?\t "\\t")
    (t (format "\\u%04x" char))))

(defun llm-pi-ai-repair-json (json)
  "Return JSON with the malformed parts of its strings repaired.
The source's `repairJson'.  Outside strings nothing changes; inside
them:

- a raw control character becomes its escape;
- a backslash and four hex digits stay a `\\uXXXX' escape;
- a backslash and a character JSON allows as an escape stay as they
  are;
- any other backslash becomes a literal backslash, which turns
  \"\\url{x}\" into \"\\\\url{x}\" where JSON would refuse it;
- a backslash at the very end becomes a literal backslash.

The source walks UTF-16 code units and this walks characters; every
character it copies is one of its code units here too."
  (let ((length (length json))
        (index 0)
        (in-string nil)
        (repaired nil))
    (while (< index length)
      (let ((char (aref json index))
            (piece nil))
        (cond
         ;; Quotes are tested by code too: `eq' on characters depends
         ;; on how the text was read, and a byte from a stream is a
         ;; character `eq' does not see as one.
         ((not in-string)
          ;; `char-to-string' of a quote would make a gap of it in a
          ;; multibyte text: the quote is copied as it is.
          (setq piece (if (and (= (logand char #x3fffff) ?\")
                               (< char #x3fff80))
                          "\""
                        (string char)))
          (when (= (logand char #x3fffff) ?\")
            (setq in-string t)))
         ((= char ?\")
          (setq piece "\""
                in-string nil))
         ;; A backslash is read by the character that follows it, so
         ;; testing it with `eq' would ask about a symbol and never
         ;; match: compare the character's code.
         ((= char ?\\)
          (let ((next (and (< (1+ index) length) (aref json (1+ index)))))
            (cond
             ((null next)
              (setq piece "\\\\"))
             ;; Four hex digits make an escape; a `u' without them is
             ;; no escape, and the backslash is doubled.  A doubled
             ;; backslash is one backslash of the string, so the text
             ;; holds it whole.
             ((eq next ?u)
              (if (and (>= length (+ index 6))
                       (string-match-p
                        llm-pi-ai--hex-escape-regexp
                        (substring json (+ index 2) (+ index 6))))
                  (setq piece (substring json index (+ index 6))
                        index (+ index 5))
                (setq piece "\\\\")))
             ;; A backslash and the character it escapes, `u' left to
             ;; the four digits it may or may not have.  The backslash
             ;; is taken off the text by the read: the text's own
             ;; character is put back.
             ;; The text of the string is kept as the text holds it: a
             ;; string built here with a quote in it is not read as the
             ;; text was, and the quote has to come from the text.
             ((and (not (eq next ?u))
                   (memq (logand next #x3fffff)
                         (cons ?/ llm-pi-ai-valid-json-escapes)))
              (setq piece (substring json index (+ index 2))
                    index (1+ index)))
             ;; Doubled outside a string, a backslash is the text's
             ;; own and is kept as it is.
             ((and (>= (+ index 2) length)
                   (< (logand char #x3fffff) #x80))
              (setq index (1+ index)))
             (t
              (setq piece "\\\\")))))
         ((llm-pi-ai-control-character-p char)
          (setq piece (llm-pi-ai-escape-control-character char)))
         (t
          (setq piece (string char))))
        (push piece repaired))
      (setq index (1+ index)))
    (llm-pi-ai--string-append repaired)))

;;; Reading

(defun llm-pi-ai--hex-escape-value (text start end)
  "Return the number of TEXT's four hex digits from START to END.
Return nil when they are not four hex digits or END is past TEXT."
  (when (<= end (length text))
    (let ((digits (substring text start end)))
      (when (string-match-p llm-pi-ai--hex-escape-regexp digits)
        (string-to-number digits 16)))))

(defun llm-pi-ai--unicode-pair-p (text index)
  "Return non-nil when TEXT has a surrogate pair escape at INDEX.
INDEX is the backslash of a `\\uD83D\\uDE00': two escapes whose code
units are the halves of one character."
  (let ((high (llm-pi-ai--hex-escape-value text (+ index 2) (+ index 6))))
    (and high (<= #xD800 high #xDBFF)
         (eq (and (< (+ index 6) (length text)) (aref text (+ index 6))) ?\\)
         (let ((low (llm-pi-ai--hex-escape-value text (+ index 8) (+ index 12))))
           (and low (<= #xDC00 low #xDFFF))))))

(defun llm-pi-ai--unicode-pair-char (text index)
  "Return the character TEXT's surrogate pair escape at INDEX stands for."
  (let ((high (llm-pi-ai--hex-escape-value text (+ index 2) (+ index 6)))
        (low (llm-pi-ai--hex-escape-value text (+ index 8) (+ index 12))))
    (+ #x10000 (* (- high #xD800) #x400) (- low #xDC00))))

(defun llm-pi-ai--decode-unicode-escape (text)
  "Return TEXT with each valid `\\uXXXX' escape decoded.
`text' is the body of one string literal.  The source's `JSON.parse'
reads a surrogate pair as one character and a lone surrogate as
itself; Emacs reads the pair as two characters and refuses a lone
surrogate, whose character is not valid UTF-8.  So the pair becomes
the one character it stands for, and a lone surrogate keeps its
`\\uXXXX' text, which is the closest a string can come to holding
it.

A backslash is taken with the character after it, so an escaped
backslash is no escape: \"\\\\u0041\" stays a backslash and the
characters u, 0, 0, 4 and 1, and a backslash before a quote does not
end the body."
  (let ((length (length text))
        (index 0)
        (result nil))
    (while (< index length)
      (let ((char (aref text index)))
        (if (not (eq char ?\\))
            (progn
              (push (char-to-string char) result)
              (setq index (1+ index)))
          (let ((code (llm-pi-ai--hex-escape-value text (+ index 2) (+ index 6))))
            (cond
             ((null code)
              (push (substring text index (min length (+ index 2))) result)
              (setq index (+ index 2)))
             ((and (<= #xd800 code #xdbff)
                   (<= (+ index 12) length)
                   (eq (aref text (+ index 6)) ?\\)
                   (eq (aref text (+ index 7)) ?u)
                   (let ((low (llm-pi-ai--hex-escape-value
                               text (+ index 8) (+ index 12))))
                     (when (and low (<= #xdc00 low #xdfff))
                       (push (char-to-string
                              (+ #x10000
                                 (* (- code #xd800) #x400)
                                 (- low #xdc00)))
                             result)
                       (setq index (+ index 12))
                       t)))
              nil)
             ((<= #xd800 code #xdfff)
              (push (substring text index (+ index 6)) result)
              (setq index (+ index 6)))
             (t
              (push (char-to-string code) result)
              (setq index (+ index 6))))))))
    (llm-pi-ai--string-append result)))

(defun llm-pi-ai--decode-unicode-escapes-in-strings (json)
  "Return JSON with the `\\uXXXX' escapes of its strings decoded.
Outside strings every character is copied as it is, a backslash taken
with the character after it, so a string's end is found the way
`llm-pi-ai-repair-json' finds it and a `\\u' there is what it is
there too.

Text outside JSON strings is the characters between them, and a
string is decoded whole, escapes the runtime does not decode and all
-- `json-parse-string' decodes them -- so this reader never decides
what a `\\u' escape means."
  (let ((length (length json))
        (index 0)
        (in-string nil)
        (start 0)
        (result nil))
    (while (< index length)
      (let ((char (aref json index)))
        (cl-case char
          (?\\
           (let ((next (and (< (1+ index) length) (aref json (1+ index)))))
             (cond
              ;; A `\uD83D\uDE00' pair stands for one character, and a
              ;; lone surrogate, which this string cannot hold, is left
              ;; for `json-parse-string' to keep as the text it is.
              ((and in-string (eq next ?u)
                    (llm-pi-ai--unicode-pair-p json index))
               (let ((character (llm-pi-ai--unicode-pair-char json index)))
                 (push (concat (substring json start index)
                               (char-to-string character))
                       result)
                 (setq index (+ index 12)
                       start index)))
              ;; A `\/' is JSON's `/`, and Emacs's reader refuses it, so
              ;; it is copied as the character it stands for.
              ((and in-string (eq next ?/))
               (push (concat (substring json start index) "/") result)
               (setq index (+ index 2)
                     start index))
              (in-string
               (setq index (+ index 2)))
              (t
               (setq index (if next (+ index 2) (1+ index)))))))
          (?\"
           (if in-string
               (progn
                 (push (llm-pi-ai--decode-unicode-escape
                        (substring json start (1+ index)))
                       result)
                 (setq index (1+ index)
                       start index
                       in-string nil))
             (setq index (1+ index)
                   in-string t)))
          (t
           (setq index (1+ index))))))
    (push (substring json start) result)
    (llm-pi-ai--string-append result)))

(defconst llm-pi-ai--json-reader-options
  '(:object-type hash-table
    :array-type array
    :null-object :null
    :false-object :false)
  "The options that make `json-parse-string' read JSON as Lisp holds it.")

(defun llm-pi-ai--parse-json-text (text)
  "Parse TEXT, properly encoded UTF-8, as JSON.
Return the Lisp value `json-parse-string' makes of it.
Its `json-parse-error' is JSON's own SyntaxError; the other errors
the reader signals for text it cannot read are not, and are reported
as `json-parse-error', which is what JSON.parse's SyntaxError is."
  (condition-case err
      (apply #'json-parse-string text
             llm-pi-ai--json-reader-options)
    (json-parse-error
     (signal (car err) (cdr err)))
    (error
     (signal 'json-parse-error (list (error-message-string err))))))

(defun llm-pi-ai-parse-json (json)
  "Parse JSON, a string that may have come from a model, as JSON.
This is the source's bare `JSON.parse', except for two things the
source's runtime does better and Emacs's needs help with: a string
of bytes that is not valid UTF-8 is read as ISO-8859-1, byte for
byte, and a `\\uXXXX' pair of surrogates is read as the character it
stands for.  Malformed JSON signals `json-parse-error'."
  (let* ((text (if (multibyte-string-p json)
                   json
                 ;; A string of bytes is decoded strictly: one that is
                 ;; not UTF-8 is read as ISO-8859-1, byte for byte.
                 ;; NOERROR cannot say whether it did: it leaves a byte
                 ;; it does not decode in the text as the raw byte it
                 ;; is, and that is what is looked for, one scan of the
                 ;; text and no regular expression over it, which would
                 ;; run out of stack on a long one.
                 (let ((decoded (decode-coding-string json 'utf-8 t)))
                   (if (string-match-p "[\x3fff80-\x3fffff]" decoded)
                       (decode-coding-string json 'iso-8859-1)
                     decoded))))
         (text (llm-pi-ai--decode-unicode-escapes-in-strings text)))
    (condition-case nil
        (llm-pi-ai--parse-json-text text)
      ((utf-8 end-of-file)
       (llm-pi-ai--parse-json-text-slowly text)))))

(defun llm-pi-ai--char-byte-string (char)
  "Return CHAR as the text it is for, a byte or a character."
  ;; The source of this port tested (not (string-as-unibyte ...)), which is
  ;; never true, so the plain character was always the answer.
  (string char))

(defun llm-pi-ai--parse-json-text-slowly (text)
  "Parse TEXT as JSON with `json-read'.
TEXT is UTF-8 text that `llm-pi-ai--parse-json-text' could not make
a value of: a character above the Basic Multilingual Plane is read
in it, and `json-parse-string' refuses such a character where
`json-read' reads it.  TEXT is given to the reader as bytes, so that
text that came in as bytes is read as JSON.parse reads text.
A JSON null is read as nil, so at the top level nil is text that was
null or that was not there; `json-read' is not told a key type here,
for it makes its keys by string, not as the parsed objects of
`llm-pi-ai--json-reader-options' have them."
  (let ((json-object-type 'hash-table)
        (json-array-type 'array)
        (json-key-type 'string)
        (json-false :false)
        (json-null :null))
    (condition-case err
        (json-read-from-string text)
      (json-end-of-file
       (signal 'json-parse-error (list (error-message-string err))))
      (json-parse-error
       (signal (car err) (cdr err)))
      (error
       (signal 'json-parse-error (list (error-message-string err)))))))

(defun llm-pi-ai-parse-json-with-repair (json)
  "Parse JSON, repairing its malformed string literals when it needs it.
The source's `parseJsonWithRepair': when JSON parses as it is, that
is the answer; otherwise the repaired text is parsed, and when the
repair changed nothing the original error is signalled again."
  (condition-case err
      (llm-pi-ai-parse-json json)
    (error
     (let ((repaired (llm-pi-ai-repair-json json)))
       (if (equal repaired json)
           (signal (car err) (cdr err))
         (llm-pi-ai-parse-json repaired))))))

;;; Tool call arguments

(defun llm-pi-ai-json-object-p (value)
  "Return non-nil when VALUE is a parsed JSON object.
That is a hash table, or an alist when the text was not UTF-8; not
an array, which is a vector, and not a JSON null, which is `:null'."
  (or (hash-table-p value)
      (and (consp value) (consp (car value)) (stringp (caar value))
           (or (consp (cdar value)) (null (cdar value))))))

(defun llm-pi-ai--json-error-message (err)
  "Return the message ERR holds, the source's `error.message'.
`error's message without the error position `json-parse-error'
appends to it, and anything else as it prints."
  (cond
   ((and (consp err) (stringp (nth 1 err)) (null (nth 2 err)))
    (nth 1 err))
   ((and (consp err) (stringp (nth 1 err))
         (eq (car err) 'json-parse-error))
    (substring (nth 1 err) 0 (string-match " \\(?:[0-9]+\\|at position\\)\\'"
                                           (nth 1 err))))
   ((symbolp (car-safe err))
    (error-message-string err))
   (t (format "%s" err))))

(defun llm-pi-ai-parse-tool-call-arguments (json)
  "Parse the finished arguments text JSON of a tool call.
The source's `parseToolCallArguments'.  Return an alist of
`arguments' and, when the text is not a JSON object, `arguments-error'.

The whole text is parsed, with the repair: text that is not valid
JSON, or that is JSON but not an object, leaves `arguments' an empty
object and sets `arguments-error' to the message and the text the
model sent.  A cut-off prefix is not completed here: by the time a
call is finished, a prefix is a mistake to report back to the model,
not a value to keep."
  (if (or (null json) (string-empty-p (string-trim json)))
      (list (cons 'arguments (llm-pi-ai-json-object)))
    (condition-case err
        (let ((parsed (llm-pi-ai-parse-json-with-repair json)))
          (if (llm-pi-ai-json-object-p parsed)
              (list (cons 'arguments parsed))
            (list (cons 'arguments (llm-pi-ai-json-object))
                  (cons 'arguments-error
                        (concat "Tool arguments must be a JSON object"
                                "\n\nReceived arguments:\n" json)))))
      (error
       (list (cons 'arguments (llm-pi-ai-json-object))
             (cons 'arguments-error
                   (concat (llm-pi-ai--json-error-message err)
                           "\n\nReceived arguments:\n" json)))))))

;;; Partial JSON

(defun llm-pi-ai--partial-whitespace-p (char)
  "Return non-nil when CHAR, a character, is JSON whitespace."
  (memq char '(?\s ?\t ?\n ?\r)))

(defun llm-pi-ai-parse-streaming-json (partial-json)
  "Parse PARTIAL-JSON, the JSON text a stream has delivered so far.
The source's `parseStreamingJson', the streaming twin of
`llm-pi-ai-parse-tool-call-arguments': the arguments of a tool call
as they grow, always an object.

Valid JSON is returned as it is, and so is a repaired text that is
valid.  Otherwise the source's second reader, `partial-json', gives
the longest prefix it can read: a member or element cut off before
its value is dropped and the containers around it are closed, a
cut-off string is ended where it stops, and a cut-off escape is
dropped.  Text with nothing readable gives an empty object, never
nil, which is JSON's null."
  (if (or (null partial-json)
          (string-empty-p (string-trim partial-json)))
      (llm-pi-ai-json-object)
    (condition-case nil
        ;; The text is UTF-8 by contract, and bytes that are not are
        ;; read one by one rather than refused, as the source's Buffer
        ;; does: a byte string is made a text string byte for byte.
        (llm-pi-ai-parse-json-with-repair
         (if (multibyte-string-p partial-json)
             partial-json
           (with-no-warnings (string-make-multibyte partial-json))))
      (error
       (condition-case err
           (llm-pi-ai--partial-parse partial-json)
         (error
          ;; A reader error is a bug here, not the text's: report it as
          ;; loudly as a failed repair, so it cannot pass for an empty
          ;; object.
          (unless (eq (car err) 'error)
            (signal (car err) (cdr err)))
          (llm-pi-ai-json-object)))))))

;;; Reading a prefix

(defvar llm-pi-ai--partial-text nil
  "The text `llm-pi-ai-parse-partial-json' reads.")

(defvar llm-pi-ai--partial-position 0
  "How far into `llm-pi-ai--partial-text' reading has come.")

(defvar llm-pi-ai--partial-stopped nil
  "Non-nil when the text was not readable at the reading position.
Nil means the text ended instead, which is not a mistake: what was
read is closed and kept.")

(defun llm-pi-ai--partial-peek ()
  "Return the character at the reading position, or nil at the end."
  (let ((position llm-pi-ai--partial-position))
    (when (< position (length llm-pi-ai--partial-text))
      (aref llm-pi-ai--partial-text position))))

(defun llm-pi-ai--partial-end-p ()
  "Return non-nil when the reading position is at the end of the text."
  (>= llm-pi-ai--partial-position (length llm-pi-ai--partial-text)))

(defun llm-pi-ai--partial-skip-whitespace ()
  "Move the reading position past whitespace."
  (while (llm-pi-ai--partial-whitespace-p (llm-pi-ai--partial-peek))
    (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position))))

(defun llm-pi-ai--partial-stop ()
  "Mark the text as not readable at the reading position.
The first place wins: the read stops as soon as it is marked.
Return nil, so that a read that stops here reads no value: `setq'
would return the `t' it set."
  (setq llm-pi-ai--partial-stopped t)
  nil)

(defun llm-pi-ai--partial-word (word value)
  "Read WORD, which stands for VALUE, as `true' or `false' do.
The source's lexer takes as many characters of WORD as the text
holds up to it, and reads those it took with `JSON.parse', so a word
the text cut off is worth what `JSON.parse' makes of its part:
`tru' is t, `fals' is `:false', `nu' is `:null', and a part of
`NaN' or `Infinity' is the null `JSON.parse' makes of words JSON has
none of.  A first character that is no word at all stops the read."
  (let* ((length (length word))
         (text (substring llm-pi-ai--partial-text
                          llm-pi-ai--partial-position
                          (min (length llm-pi-ai--partial-text)
                               (+ llm-pi-ai--partial-position length))))
         (taken (length text)))
    ;; The longest prefix of the word the text holds.
    (while (and (> taken 0) (not (string-prefix-p (substring text 0 taken) word)))
      (setq taken (1- taken)))
    (cond
     ((zerop taken)
      (llm-pi-ai--partial-stop))
     ((memq (aref word 0) '(?N ?I))
      ;; `NaN' and `Infinity' are no JSON: `JSON.parse' reads no part of
      ;; them as a number, and a cut-off one is the null it keeps while
      ;; its lexer reads the word.
      (setq llm-pi-ai--partial-position (+ llm-pi-ai--partial-position taken))
      :null)
     ((< taken length)
      (setq llm-pi-ai--partial-position (+ llm-pi-ai--partial-position taken))
      ;; `fals' is the one part `JSON.parse' makes another value of,
      ;; `false', before its `e' comes.
      (if (equal (substring word 0 taken) "fals") :false value))
     (t
      (setq llm-pi-ai--partial-position (+ llm-pi-ai--partial-position taken))
      value))))

(defun llm-pi-ai--partial-number-char-p (char)
  "Return non-nil when CHAR can be part of a number, as the source takes it."
  (and char (memq char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?+ ?. ?e ?E))))

(defun llm-pi-ai--partial-dropped-value ()
  "Return what a value the text cut off becomes.
nil: the object drops the member, and the array takes it for the
null the cut-off element would have been."
  nil)

(defvar llm-pi-ai--partial-forced-end nil
  "Non-nil when the value being read must be a whole one.
A member's value and a string the text cut off are given up where the
text is not a whole value, and accepted only where the text ended in
them.  Set by `llm-pi-ai--partial-drop-unless-end', read by the reads
that can end the text halfway, and reset by `llm-pi-ai-parse-partial-json'.")

(defun llm-pi-ai--partial-drop-unless-end ()
  "Return non-nil when a value the text cut off is to be given up.
It is given up unless it ended the text, which is the one place a
cut-off value stands for something."
  llm-pi-ai--partial-forced-end)

(defun llm-pi-ai--partial-value ()
  "Read one value at the reading position.
Return it, or nil where the text stopped."
  (llm-pi-ai--partial-skip-whitespace)
  (let ((char (llm-pi-ai--partial-peek)))
    (cond
     ((null char)
      (llm-pi-ai--partial-stop))
     ((eq char ?{)
      (llm-pi-ai--partial-object))
     ((eq char ?\[)
      (llm-pi-ai--partial-array))
     ((eq char ?\")
      (llm-pi-ai--partial-string))
     ((eq char ?t)
      (llm-pi-ai--partial-word "true" t))
     ((eq char ?f)
      (llm-pi-ai--partial-word "false" :false))
     ((eq char ?n)
      (llm-pi-ai--partial-word "null" :null))
     ((eq char ?N)
      (llm-pi-ai--partial-word "NaN" 'nan))
     ((eq char ?I)
      (llm-pi-ai--partial-word "Infinity" 'infinity))
     ((eq char ?-)
      (llm-pi-ai--partial-minus))
     ((memq char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (llm-pi-ai--partial-number))
     (t
      (llm-pi-ai--partial-stop)))))

(defun llm-pi-ai--partial-number ()
  "Read a number at the reading position, with `json-parse-string'.
A number the text cut off is dropped, as `partial-json' drops one
that is no JSON number in full: a literal is read only whole.
Return nil where the read stops."  (let ((start llm-pi-ai--partial-position))
    (while (llm-pi-ai--partial-number-char-p (llm-pi-ai--partial-peek))
      (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position)))
    (let ((text (substring llm-pi-ai--partial-text start
                           llm-pi-ai--partial-position)))
      (or (condition-case nil
              (llm-pi-ai--read-number text)
            (error nil))
          (progn
            (setq llm-pi-ai--partial-stopped t)
            nil)))))

(defun llm-pi-ai--partial-minus ()
  "Read what starts with `-' at the reading position.
`-Infinity'''s lexer reads `-Infinity' as a number only with all
of it; a `-' followed by anything else is a number, read with
`json-parse-string', or no literal at all.  A cut-off one is
dropped."
  (if (eq (llm-pi-ai--partial-peek-at 1) ?I)
      (let ((rest (substring llm-pi-ai--partial-text
                             llm-pi-ai--partial-position
                             (min (length llm-pi-ai--partial-text)
                                  (+ llm-pi-ai--partial-position 9)))))
        ;; `JSON.parse' reads no `-Infinity': the null it keeps.
        (setq llm-pi-ai--partial-position
              (+ llm-pi-ai--partial-position
                 (llm-pi-ai--partial-common-length rest "-Infinity")))
        :null)
    (llm-pi-ai--partial-number)))

(defun llm-pi-ai--partial-peek-at (offset)
  "Return the character OFFSET past the reading position, or nil."
  (let ((position (+ llm-pi-ai--partial-position offset)))
    (when (< position (length llm-pi-ai--partial-text))
      (aref llm-pi-ai--partial-text position))))

(defun llm-pi-ai--partial-common-length (text word)
  "Return how many characters of TEXT begin WORD, from its start."
  (let ((length (min (length text) (length word)))
        (taken 0))
    (while (and (< taken length) (eq (aref text taken) (aref word taken)))
      (setq taken (1+ taken)))
    taken))

(defun llm-pi-ai--read-number (text)
  "Return the number TEXT is, or nil when it is not one.
The number is read as JSON reads it: `json-parse-string' here, with
a text it cannot read is not a number."
  ;; `json-parse-string' reads a number only without `:trailing-content',
  ;; which asks it for other options Emacs then refuses to read one without.
  (let ((value (condition-case nil
                   (llm-pi-ai--partial-read-one text)
                 (error nil))))
    (if (numberp value) value nil)))

(defun llm-pi-ai--partial-read-one (text)
  "Return the one JSON value TEXT holds, or signal an error.
The caller has already cut TEXT to the end of the value: text left
over past it is refused, as `json-parse-string' refuses it.  A reader
is given for every kind of value, which `json-parse-string' demands
when it is passed `:trailing-content'; there is no `nil' reader to
leave out, and the readers are keyword arguments, not functions."
  (json-parse-string text :array-type 'list
                          :object-type 'hash-table
                          :null-object :null
                          :false-object :false))

(defun llm-pi-ai--partial-any-value (text)
  "Return non-nil when TEXT reads as one JSON value."
  (condition-case nil
      (progn (llm-pi-ai--partial-read-one text) t)
    (error nil)))

(defun llm-pi-ai--partial-after-separator (close)
  "Note the comma read after a member or element, or stop the read.
CLOSE is the character that ends the container."
  (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position))
  (llm-pi-ai--partial-skip-whitespace)
  (when (eq (llm-pi-ai--partial-peek) close)
    (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position))
    'closed))

(defun llm-pi-ai--partial-separator (close)
  "Read what separates an element or a member from the next.
CLOSE is the character that ends the container.  Return `closed'
when it was CLOSE itself, `more' after a comma, and nil where the
text stopped or held something else."
  (let ((next (llm-pi-ai--partial-peek)))
    (cond
     ((eq next close) 'closed)
     ((eq next ?,)
      (if (llm-pi-ai--partial-after-separator close)
          'closed
        'more))
     (t
      (llm-pi-ai--partial-stop)
      nil))))

(defun llm-pi-ai--partial-object ()
  "Read an object at the reading position, as `partial-json' 0.1.7 reads one.
The text cut off anywhere but at the colon keeps what was read: a
member whose value, key or colon the text cut off is dropped, and so
is the container around the member, with the object closed.  A value
where the colon should be is what the library's `index++' steps over
anyway, so that member is read, and its own value may then be kept;
a value where the whole member was cut off is the library's
`parseStr' on it -- a member read only where its input is a string,
and the object kept either way."
  (let ((object (llm-pi-ai-json-object))
        (done nil)
        (new-member t))
    (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position))
    (llm-pi-ai--partial-skip-whitespace)
    (while (and (not llm-pi-ai--partial-stopped) (not done))
      (let ((char (llm-pi-ai--partial-peek)))
        (cond
         ;; The object ends here, whether after a member, after a
         ;; comma, or where the text went on from a member it cut off.
         ((eq char ?})
          (setq done t
                llm-pi-ai--partial-position
                (1+ llm-pi-ai--partial-position)))
         ;; Nothing follows a comma but a member's key.
         ((and new-member (null char))
          (setq done t))
         ;; Anything else where a key is expected was cut off, or is
         ;; not a member: the object keeps what it has.  Text that
         ;; ended there holds nothing more; text that goes on is the
         ;; object's, and the object is given up.
         ((not (eq char ?\"))
          (setq done t)
          (unless (llm-pi-ai--partial-end-p)
            (llm-pi-ai--partial-stop)))
         (t
          (let ((llm-pi-ai--partial-forced-end nil)
                (key (llm-pi-ai--partial-string)))
            (cond
             ;; A key the text cut off is the last thing read: the text
             ;; holds nothing more, and the object is closed with the
             ;; members it has.
             (llm-pi-ai--partial-stopped
              (setq done t
                    llm-pi-ai--partial-stopped nil))
             ;; The text ended right after the key, so the member was
             ;; cut off before its colon; the object is closed the same
             ;; way.
             ((llm-pi-ai--partial-end-p)
              (setq done t))
             ;; A key is followed by its colon.  The source's lexer wants
             ;; the colon there and skips whitespace after it; text that
             ;; ends here, or holds something else where the colon goes,
             ;; is a syntax error, and the read has all it can get.
             ((not (progn (llm-pi-ai--partial-skip-whitespace)
                          (eq (llm-pi-ai--partial-peek) ?:)))
              (setq done t)
              (unless (llm-pi-ai--partial-end-p)
                (llm-pi-ai--partial-stop)))
             ;; The colon is there: step past it, and past whitespace.
             (t
              (setq llm-pi-ai--partial-position
                    (1+ llm-pi-ai--partial-position))
              (llm-pi-ai--partial-skip-whitespace)
              (cond
               ((llm-pi-ai--partial-end-p)
                (setq done t))
               (t
                (let ((forced llm-pi-ai--partial-forced-end)
                      (value (llm-pi-ai--partial-value)))
                  (cond
                   ;; A member whose value the text cut off is the text's
                   ;; last unfinished part: the object keeps what it
                   ;; holds and the text is closed after it.  A value
                   ;; where the member's colon should be -- the text
                   ;; went on past it -- is the text's, and the object
                   ;; is given up; its cut is then the text's too.
                   (llm-pi-ai--partial-stopped
                    (when forced
                      (setq done t
                            llm-pi-ai--partial-stopped nil)))
                   (t
                    (puthash key value object)
                    (llm-pi-ai--partial-skip-whitespace)
                    (let ((step (llm-pi-ai--partial-separator ?})))
                      (setq new-member (eq step 'more))
                      (when (eq step 'closed) (setq done t)))))))))))))))
    object))

(defun llm-pi-ai--partial-array ()
  "Read an array at the reading position.
An element the text cut off is dropped, with the array closed.  An
array the text goes on into is given up."
  (let ((elements nil)
        (done nil))
    (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position))
    (llm-pi-ai--partial-skip-whitespace)
    (while (and (not done) (not llm-pi-ai--partial-stopped))
      (let ((char (llm-pi-ai--partial-peek)))
        (cond
         ;; The array ends here, whether after an element, after a
         ;; comma, or where the text went on from an element it cut
         ;; off.
         ((eq char ?\])
          (setq done t
                llm-pi-ai--partial-position
                (1+ llm-pi-ai--partial-position)))
         ;; Nothing follows a comma but an element.
         ((null char)
          (setq done t))
         (t
          (let* ((forced llm-pi-ai--partial-forced-end)
                 (value (llm-pi-ai--partial-value)))
            (cond
             ;; An element the text cut off at its end is dropped with
             ;; the array closed; one the text goes on past is the
             ;; text's, and the array is given up.  Either way the loop
             ;; ends: the text goes on, so reading it again would only
             ;; stop at the same place.
             (llm-pi-ai--partial-stopped
              (setq done t)
              (push value elements)
              (unless forced
                ;; The element was the text's last unfinished part, so
                ;; it is kept as far as it goes and the array closes
                ;; after it: the text holds nothing more to read.
                (setq llm-pi-ai--partial-position
                      (length llm-pi-ai--partial-text))))
             (t
              (push value elements)
              (llm-pi-ai--partial-skip-whitespace)
              (let ((step (llm-pi-ai--partial-separator ?\])))
                (when (eq step 'closed) (setq done t)))))))))
      ;; An element the text cut off was read again where it stopped: a
      ;; loop that went on would stop at the same place for ever.
      (when llm-pi-ai--partial-stopped
        (setq done t
              llm-pi-ai--partial-stopped nil)))
    (setq llm-pi-ai--partial-stopped nil)
    (vconcat (nreverse elements))))

(defun llm-pi-ai--partial-string ()
  "Read a string at the reading position and return its text.
A string the text cut off is ended where it stops, and a dangling
escape is dropped.  An escape is read with the character after it,
and a `\\u' with its four digits when it has them."
  (let* ((start (1+ llm-pi-ai--partial-position))
         (length (length llm-pi-ai--partial-text))
        ;; A string the text cut off is a cut-off value, given up where
        ;; it does not end the text; the text cut off right at a
        ;; dangling backslash is.
        (cut-off (llm-pi-ai--partial-drop-unless-end))
         (done nil)
         (result nil))
    (setq llm-pi-ai--partial-position start)
    (while (and (not done) (not llm-pi-ai--partial-stopped))
      (let ((char (llm-pi-ai--partial-peek)))
        (when (and cut-off (null char))
          ;; The text ends in the string, and more of it is to come:
          ;; the string is cut off like any value, and given up.
          (llm-pi-ai--partial-stop))
        (cond
         ((null char)
          (if cut-off
              (setq done t
                    llm-pi-ai--partial-position length)
            (push (substring llm-pi-ai--partial-text start) result)
            (setq done t
                  llm-pi-ai--partial-position length)))
         ((eq char ?\")
          (push (substring llm-pi-ai--partial-text start
                           llm-pi-ai--partial-position)
                result)
          (setq llm-pi-ai--partial-position (1+ llm-pi-ai--partial-position)
                done t))
         ((eq char ?\\)
          (let ((position llm-pi-ai--partial-position)
                (after (and (< (1+ llm-pi-ai--partial-position) length)
                            (aref llm-pi-ai--partial-text
                                  (1+ llm-pi-ai--partial-position)))))
            (push (substring llm-pi-ai--partial-text start position) result)
            (cond
             ;; An escape the text cut off is dropped, and the string
             ;; ended where the escape began, as `partial-json' does:
             ;; the dangling backslash is text the text does not end.
             ((null after)
              (push "\\" result)
              (setq done t
                    llm-pi-ai--partial-position length
                    start length))
             ((> (+ position 2) length)
              ;; Half an escape is no escape: the string keeps what it
              ;; had, and one that does not end the text stops it.
              (setq done t
                    llm-pi-ai--partial-position length
                    start length
                    llm-pi-ai--partial-stopped cut-off))
             ((and (eq after ?u)
                   ;; A `\u' is read only with its four digits; one the
                   ;; text cut off inside them takes the digits with it.
                   (> (+ position 6) length))
              (setq done t
                    llm-pi-ai--partial-position length
                    start length))
             ((string-match-p
               llm-pi-ai--escape-regexp
               (substring llm-pi-ai--partial-text
                          position (min (+ position 2) length)))
              ;; A `\' and the character it escapes, which the text
              ;; holds whole.
              (push (substring llm-pi-ai--partial-text
                               position (+ position 2))
                    result)
              (setq llm-pi-ai--partial-position (+ position 2)
                    start (+ position 2)))
             ((and (eq after ?u)
                   (<= (+ position 6) length)
                   (string-match-p
                    llm-pi-ai--hex-escape-regexp
                    (substring llm-pi-ai--partial-text
                               (+ position 2) (+ position 6))))
              (push (substring llm-pi-ai--partial-text
                               position (+ position 6))
                    result)
              (setq llm-pi-ai--partial-position (+ position 6)
                    start (+ position 6)))
             (t
              (push (substring llm-pi-ai--partial-text
                               position (+ position 2))
                    result)
              (setq llm-pi-ai--partial-position (+ position 2)
                    start (+ position 2))))))
         (t
          (setq llm-pi-ai--partial-position
                (1+ llm-pi-ai--partial-position))))))
    (let ((text (llm-pi-ai--string-append result)))
      (if (string-empty-p text)
          ""
        (llm-pi-ai--decode-unicode-escape text)))))

(defun llm-pi-ai--partial-parse (text-to-parse)
  "Parse the longest prefix of TEXT-TO-PARSE that can be read.
This is the source's `partial-json' 0.1.7 reader, as
`parseStreamingJson' uses it.  A member or element cut off before
its value is dropped and the containers around it closed; a string
cut off is ended where it stops, with a dangling escape dropped; text
held where a member's colon goes is a syntax error, and the containers
are closed where it was met; and text with nothing readable gives an
empty object.

What is read is the source's lexer's: a word cut off is the value its
part names (`tru' is t, `fals' is false), `null', `NaN', `Infinity'
and `-Infinity' cut off are the null `JSON.parse' makes of them, and
a number is a literal only in full -- `-', `1.' and `1e' are no
number, and the member is dropped.  Numbers are read by
`json-parse-string'; only `.5', which it reads and `JSON.parse' does
not, is refused here, and `NaN' and `Infinity' do not reach it."
  (let ((llm-pi-ai--partial-text text-to-parse)
        (llm-pi-ai--partial-position 0)
        (llm-pi-ai--partial-forced-end nil)
        (llm-pi-ai--partial-stopped nil))
    (or (llm-pi-ai--partial-value) (llm-pi-ai-json-object))))

(provide 'llm-pi-ai-utils-json-parse)

;;; llm-pi-ai-utils-json-parse.el ends here

