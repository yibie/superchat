;;; llm-pi-ai-utils-json-parse-test.el --- Tests for llm-pi-ai-utils-json-parse  -*- lexical-binding: t; -*-

;; Tests for Tier 0: the port of src/utils/json-parse.ts
;; (@earendil-works/pi-ai@1.4.0, commit 58c0b02c).
;;
;; The source's own file, packages/ai/test/json-parse.test.ts, is not
;; in the checkout's working tree; it was taken from commit 58c0b02c
;; and its cases are the ones marked "source" below, one per
;; `describe' block in the order the source has them.  The cases
;; marked "port" cover what the source's runtime did for it and
;; Emacs's needs here: surrogate pairs, bytes, numbers and the repair
;; itself.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'llm-pi-ai-utils-json-parse)

;; The source's stream reader is `partial-json' 0.1.7, which
;; `parseStreamingJson' calls with `ALL' (0xff: every kind of value
;; may be cut off, the readers of the kinds JSON cannot hold included,
;; none of which the source's calls leave out), and the cases marked
;; "upstream" below check the port's answer against the library's own
;; where it is installed: they run node, with the package's
;; `index.js', and are skipped where there is no node or no library.
;; It is looked for under the checkout's packages/ai directory, given
;; by `LLM_PI_AI_UPSTREAM' or taken as `/tmp/pi-ai-probe/packages/ai',
;; then in the checkout's own `node_modules' (found by the
;; `package.json' above `default-directory') and in this package's.
(defvar llm-pi-ai--upstream-ai-dir
  (or (getenv "LLM_PI_AI_UPSTREAM")
      "/tmp/pi-ai-probe/packages/ai")
  "The source's packages/ai directory, for checking against `partial-json'.")

(defun llm-pi-ai-utils-json-parse-test--read (text)
  "Return TEXT's JSON as the source's `JSON.parse' makes it, or the symbol `error'.
Objects read as hash tables, arrays as vectors, `false' as `:false',
null as `:null', and a number as a number."
  (condition-case nil
      (json-parse-string text :object-type 'hash-table
                              :array-type 'array
                              :null-object :null
                              :false-object :false)
    (error 'error)))

(defun llm-pi-ai-utils-json-parse-test--node ()
  "Return how to run the source's `partial-json' with node, or nil when it cannot be.
It is a list of the program and its arguments before the script."
  (when (and (executable-find "node") (llm-pi-ai-utils-json-parse-test--module))
    ;; The script is one line: the package's `index.js' is loaded by
    ;; `import' with its path, the text read from standard input, and
    ;; its answer `JSON.stringify'ed as a JSON string.
    (list (executable-find "node")
          "-e"
          (concat
           "(async () => {"
           " const {parse} = await import(" (json-serialize (vector (llm-pi-ai-utils-json-parse-test--module))) ");"
           " const t = require('fs').readFileSync(0, 'utf8');"
           " let r;"
           " try { r = JSON.stringify(parse(t, 255) ?? {}); }"
           " catch (e) { r = 'error'; }"
           " process.stdout.write(r);"
           " })()"))))

(defun llm-pi-ai-utils-json-parse-test--module ()
  "Return the path of `partial-json''s `index.js', or nil when it is not installed.
It is the copy installed under `llm-pi-ai--upstream-ai-dir' (the
collector the checkout installed), or in the `node_modules' of the
directory `default-directory' belongs to, found by its
`package.json'."
  (let ((relative "node_modules/partial-json/dist/index.js"))
    (cl-find-if #'file-readable-p
                (list (expand-file-name relative llm-pi-ai--upstream-ai-dir)
                      (expand-file-name relative
                                        (or (locate-dominating-file
                                             default-directory "package.json")
                                            default-directory))))))

(defun llm-pi-ai-utils-json-parse-test--partial-json (text)
  "Return `partial-json' 0.1.7's parse of TEXT, the source's stream reader.
The library's answer comes back as JSON, which this reads as
`llm-pi-ai-utils-json-parse-test--read' reads it; the library
throwing comes back as the symbol `error'."
  (let ((command (llm-pi-ai-utils-json-parse-test--node)))
    (unless command
      (ert-skip (format "no node, or no partial-json under %s"
                        llm-pi-ai--upstream-ai-dir)))
    (with-temp-buffer
      (let ((status (apply #'call-process-region
                           text nil (car command)
                           (list :coding 'utf-8-unix) (list (current-buffer) nil)
                           nil (cdr command))))
        (unless (zerop status)
          (ert-fail (format "node exited with %s on %S" status text)))
        (let ((answer (buffer-string)))
          (if (equal answer "error")
              'error
            (llm-pi-ai-utils-json-parse-test--read answer)))))))

(defun llm-pi-ai-utils-json-parse-test--same-as-upstream (text)
  "Return non-nil when the port parses TEXT as the source's stream reader does.
Check the port's `llm-pi-ai-parse-streaming-json' against
`partial-json' 0.1.7, whose answer may be a value JSON has none of:
`NaN', `Infinity' and `-Infinity' it reads as itself, and the port
keeps `partial-json''s own stand-ins for them.  Where the source's
answer is a JSON object, the port's object is compared with it by
its members, reading the source's members as `partial-json''s JSON
has them."
  (let* ((upstream (llm-pi-ai-utils-json-parse-test--partial-json text))
         (port (llm-pi-ai-parse-streaming-json text)))
    (cond
     ;; The library threw: the source answers with an empty object.
     ((eq upstream 'error)
      (llm-pi-ai-utils-json-parse-test--same port (llm-pi-ai-json-object)))
     (t
      (cl-labels ((source (value)
                    (cond ((hash-table-p value)
                           (let ((object (llm-pi-ai-json-object)))
                             (maphash (lambda (key member)
                                        (puthash key (source member) object))
                                      value)
                             object))
                          ((vectorp value)
                           (vconcat (mapcar #'source value)))
                          (t value))))
        (llm-pi-ai-utils-json-parse-test--same port (source upstream)))))))

;;; Helpers

(defun llm-pi-ai-utils-json-parse-test--object (&rest keys-and-values)
  "Return a JSON object holding KEYS-AND-VALUES."
  (apply #'llm-pi-ai-json-object keys-and-values))

(defun llm-pi-ai-utils-json-parse-test--get (object key)
  "Return the value of KEY in the JSON OBJECT."
  (gethash key object))

(defun llm-pi-ai-utils-json-parse-test--same (actual expected)
  "Return non-nil when ACTUAL and EXPECTED hold the same JSON.
A hash table is compared by its members, not by identity."
  ;; A member's value can be an object of its own, and `equal' compares
  ;; hash tables by identity, so the whole is compared as text.
  (equal (format "%S" (llm-pi-ai-utils-json-parse-test--hash actual))
         (format "%S" (llm-pi-ai-utils-json-parse-test--hash expected))))

(defun llm-pi-ai-utils-json-parse-test--hash (object)
  "Turn OBJECT, parsed JSON, into a hash table `equal' can compare.
Hash tables are compared by identity, so parsed JSON is compared
after this: alist for an object, vector for an array, the value
itself for anything else."
  (cond ((null object) :null)
        ((hash-table-p object)
         (let (pairs)
           (maphash (lambda (key value)
                      (push (cons key (llm-pi-ai-utils-json-parse-test--hash value))
                            pairs))
                    object)
           (cons 'object (sort pairs (lambda (a b) (string< (car a) (car b)))))))
        ((vectorp object) (cons 'array (mapcar #'llm-pi-ai-utils-json-parse-test--hash object)))
        (t object)))

;;; parseStreamingJson (source)

(ert-deftest llm-pi-ai-utils-json-parse-test-keeps-invalid-u-escapes-as-text ()
  "Keep invalid `\\u' escapes as literal text without dropping fields."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"text\":\"\\url{x}\"}")
                 (llm-pi-ai-utils-json-parse-test--object "text" "\\url{x}")))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json
                  "{\"path\":\"a.tex\",\"content\":\"\\usepackage{x}\",\"mode\":\"w\"}")
                 (llm-pi-ai-utils-json-parse-test--object
                  "path" "a.tex"
                  "content" "\\usepackage{x}"
                  "mode" "w"))))

(ert-deftest llm-pi-ai-utils-json-parse-test-decodes-valid-u-escapes ()
  "Decode valid `\\u' escapes."
  ;; The source writes "{caf\u00e9 \url}", read as the five characters
  ;; `caf', `e' with an acute accent, a space, and the LaTeX command with
  ;; one backslash -- which then yields two.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json
                  (concat "{\"text\":\"caf" "\u00e9" " \\url\"}"))
                 (llm-pi-ai-utils-json-parse-test--object
                  "text" (concat "caf" "\u00e9" " \\url")))))

(ert-deftest llm-pi-ai-utils-json-parse-test-omits-cut-off-u-escape ()
  "Omit a `\\u' escape cut off mid-stream."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"text\":\"ab\\u00}")
                 (llm-pi-ai-utils-json-parse-test--object "text" "ab"))))

(ert-deftest llm-pi-ai-utils-json-parse-test-streaming-keeps-invalid-text ()
  "A stream cut off in text keeps the text it has so far."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"kind\":\"seek\",\"clip")
                 (llm-pi-ai-utils-json-parse-test--object "kind" "seek"))))

;;; parseToolCallArguments (source)

(ert-deftest llm-pi-ai-utils-json-parse-test-tool-arguments-parse-and-repair ()
  "Parse complete JSON and repair invalid escapes."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-tool-call-arguments "{\"kind\":\"seek\",\"note\":\"\\url\"}")
                 (list (cons 'arguments
                             (llm-pi-ai-utils-json-parse-test--object
                              "kind" "seek" "note" "\\url"))))))

(ert-deftest llm-pi-ai-utils-json-parse-test-tool-arguments-empty ()
  "Use empty arguments for empty text."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-tool-call-arguments "")
                 (list (cons 'arguments (llm-pi-ai-json-object))))))

(ert-deftest llm-pi-ai-utils-json-parse-test-tool-arguments-no-prefix ()
  "Do not keep the valid prefix of malformed JSON."
  (let* ((raw "{\"action\":\"apply\",\"actions\":[{\"kind\":\"seek\",\"time\":.5}]}")
         (result (llm-pi-ai-parse-tool-call-arguments raw))
         (error (cdr (assq 'arguments-error result))))
    (should (llm-pi-ai-utils-json-parse-test--same (cdr (assq 'arguments result)) (llm-pi-ai-json-object)))
    ;; No `^': in Emacs it anchors the start of any line, not only the
    ;; start of the search.  It would let the pattern find "\n\n" at a
    ;; line start that the source's error does not put it at, so the
    ;; pattern asks for the text the source's message has.
    (should (string-match-p "\n\nReceived arguments:\n" error))
    (should (string-suffix-p raw error))))

(ert-deftest llm-pi-ai-utils-json-parse-test-tool-arguments-not-completed ()
  "Do not complete cut-off JSON."
  (let* ((raw "{\"kind\":\"seek\",\"clipId\":\"clip-fl")
         (result (llm-pi-ai-parse-tool-call-arguments raw))
         (error (cdr (assq 'arguments-error result))))
    (should (llm-pi-ai-utils-json-parse-test--same (cdr (assq 'arguments result)) (llm-pi-ai-json-object)))
    (should (string-suffix-p raw error))))

(ert-deftest llm-pi-ai-utils-json-parse-test-tool-arguments-not-an-object ()
  "Reject JSON that is not an object."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-tool-call-arguments "[1,2]")
                 (list (cons 'arguments (llm-pi-ai-json-object))
                       (cons 'arguments-error
                             "Tool arguments must be a JSON object\n\nReceived arguments:\n[1,2]")))))

;;; repairJson (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-leaves-good-json ()
  "Valid JSON comes out of the repair unchanged."
  (let ((json "{\"a\":\"b\",\"c\":[1,2.5]\t}\n"))
    (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json json) json))))

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-escapes-control-characters ()
  "A raw control character inside a string becomes its escape."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"x\ty\"}")
                 "{\"a\":\"x\\ty\"}"))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"n\nl\"}")
                 "{\"a\":\"n\\nl\"}"))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"\u0001\"}")
                 "{\"a\":\"\\u0001\"}")))

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-doubles-invalid-escapes ()
  "A backslash before a character JSON does not escape is doubled."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"\\url\"}")
                 "{\"a\":\"\\\\url\"}"))
  ;; `\"' is an escape JSON allows (`VALID_JSON_ESCAPES'), so the source
  ;; keeps it as it is and the `}' after it is still inside the string:
  ;; what the source returns is the text unchanged.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"\\\"}")
                 "{\"a\":\"\\\"}"))
  ;; `\\' is an escape too, so the source keeps it as it is: no quote
  ;; follows it, the `}' is still inside the string, and the text comes
  ;; back unchanged, quote at the end and all.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"some-text}{\\\\\"}")
                 "{\"a\":\"some-text}{\\\\\"}")))

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-keeps-u-escapes ()
  "A `\\u' with four hex digits is left as it is; with fewer it is doubled."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"\\u00e9\"}")
                 "{\"a\":\"\\u00e9\"}"))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"\\u00e\"}")
                 "{\"a\":\"\\\\u00e\"}")))

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-trailing-backslash ()
  "A backslash at the very end of the text is doubled."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-repair-json "{\"a\":\"x\\") "{\"a\":\"x\\\\")))

;;; Reading (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-parse-json-signals ()
  "Malformed JSON signals `json-parse-error'."
  (should-error (llm-pi-ai-parse-json "{") :type 'json-parse-error)
  (should-error (llm-pi-ai-parse-json-with-repair "{1 2}")
                :type 'json-parse-error))

(ert-deftest llm-pi-ai-utils-json-parse-test-parse-json-values ()
  "Parsed JSON is Lisp's: objects hash tables, arrays vectors, scalars atoms."
  (let ((value (llm-pi-ai-parse-json
                "{\"s\":\"x\",\"n\":1.5,\"i\":2,\"t\":true,\"f\":false,\"z\":null,\"a\":[1,\"b\"]}")))
    (should (hash-table-p value))
    (should (llm-pi-ai-utils-json-parse-test--same (gethash "s" value) "x"))
    (should (llm-pi-ai-utils-json-parse-test--same (gethash "n" value) 1.5))
    (should (llm-pi-ai-utils-json-parse-test--same (gethash "i" value) 2))
    (should (eq (gethash "t" value) t))
    (should (eq (gethash "f" value) :false))
    (should (eq (gethash "z" value) :null))
    (should (llm-pi-ai-utils-json-parse-test--same (gethash "a" value) [1 "b"])))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-json "[1,2]") [1 2])))

(ert-deftest llm-pi-ai-utils-json-parse-test-surrogate-pair ()
  "A surrogate pair is read as the one character it stands for."
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "e" (llm-pi-ai-parse-json "{\"e\":\"\\ud83d\\ude00\"}"))
                 "\U0001F600"))
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "e" (llm-pi-ai-parse-streaming-json "{\"e\":\"\\ud83d\\ude00\"}"))
                 "\U0001F600")))

(ert-deftest llm-pi-ai-utils-json-parse-test-lone-surrogate ()
  "A lone surrogate keeps its escape text, which Emacs cannot read otherwise."
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "s" (llm-pi-ai-parse-streaming-json "{\"s\":\"a\\ud83db\"}"))
                 "a\\ud83db")))

(ert-deftest llm-pi-ai-utils-json-parse-test-escaped-backslash ()
  "An escaped backslash in front of a u is not an escape."
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "s" (llm-pi-ai-parse-json "{\"s\":\"a\\\\u0041b\"}"))
                 "a\\u0041b")))

(ert-deftest llm-pi-ai-utils-json-parse-test-bytes ()
  "Bytes that are not UTF-8 are read byte for byte, not refused."
  (let ((json (encode-coding-string "{\"s\":\"caf\u00e9\"}" 'iso-8859-1)))
    (should (llm-pi-ai-utils-json-parse-test--same (gethash "s" (llm-pi-ai-parse-json json)) "caf\u00e9"))))

(ert-deftest llm-pi-ai-utils-json-parse-test-bytes-long ()
  "A long text of bytes is read whole, at no cost to the reader."
  (let ((text (make-string 200000 ?x)))
    (should (equal (gethash "a" (llm-pi-ai-parse-json
                                 (concat "{\"a\":\"" text "\"}")))
                   text))
    (should (equal (gethash "a" (llm-pi-ai-parse-json
                                 (encode-coding-string
                                  (concat "{\"a\":\"" (make-string 100000 ?\u00e9) "\"}")
                                  'iso-8859-1)))
                   (make-string 100000 ?\u00e9)))))

(ert-deftest llm-pi-ai-utils-json-parse-test-repair-parses ()
  "The repaired text parses where the source refused it."
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "a" (llm-pi-ai-parse-json-with-repair "{\"a\":\"\\q\\u0041\"}"))
                 "\\qA")))

;;; Partial JSON, from partial-json 0.1.7 (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-end-of-input ()
  "Input that stops where a value was expected drops that member."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1,\"b\":")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1,AND[")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1,")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":\"1\"")
                 (llm-pi-ai-utils-json-parse-test--object "a" "1"))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-escape-ends-string ()
  "Input that stops at an escape ends the string as it is."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":\"1\\")
                 (llm-pi-ai-utils-json-parse-test--object "a" "1\\")))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":\"1\\u")
                 (llm-pi-ai-utils-json-parse-test--object "a" "1"))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-half-a-member ()
  "Input that stops in the middle of what starts a member drops it."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\"")
                 (llm-pi-ai-json-object)))
  ;; `partial-json' 0.1.7 reads the quote after a key as the key's own
  ;; end, not as the value's start: the member has no colon, the
  ;; string `foo' is no value, and the object is given up -- `{}' --
  ;; so nothing of `a' is kept.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\"\"foo\"")
                 (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":-")
                 (llm-pi-ai-json-object)))
  ;; A word cut off that `partial-json' knows the whole of is the value
  ;; it begins: its lexer takes as many characters of `true' as it has.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":t")
                 (llm-pi-ai-utils-json-parse-test--object "a" t)))
  ;; `n' is read as the `null' the source's lexer makes of it: the part
  ;; of `null' the text holds is read with `JSON.parse', which gives
  ;; `null' for `n'.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":n")
                 (llm-pi-ai-utils-json-parse-test--object "a" :null)))
  ;; `f' and `fa' are read as the `false' the source's lexer makes of
  ;; them, as `n' is the `null'.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":f")
                 (llm-pi-ai-utils-json-parse-test--object "a" :false)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"b\":")
                 (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":fa")
                 (llm-pi-ai-utils-json-parse-test--object "a" :false))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-unexpected-characters ()
  "A character where the input allowed none is a syntax error, and
the source read what could still be read before it."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\"]}")
                 (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":\"\"\"}")
                 (llm-pi-ai-utils-json-parse-test--object "a" "")))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "\n]")
                 (llm-pi-ai-json-object))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-numbers-and-loops ()
  "Partial numbers, number constants and nested containers are read
as far as they go; a loop element still waiting is dropped."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":-inf")
                 (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1.5,")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1.5)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1e2}")
                 (llm-pi-ai-utils-json-parse-test--object "a" 100.0)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":[1,2,3,")
                 (llm-pi-ai-utils-json-parse-test--object "a" (vector 1 2 3)))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-nested-loops ()
  "Two loops open at the end are both left to be closed."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":[1,[1,")
                 (llm-pi-ai-utils-json-parse-test--object
                  "a" (vector 1 (vector 1))))))

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-not-an-object ()
  "Text that starts as an array keeps its prefix as an array."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "[1,") [1]))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "[\"a\"")
                 (vector "a"))))

;;; Numbers (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-infinity-is-refused ()
  "`Infinity' is JSON everywhere but in JavaScript; Emacs refuses it."
  (should-error (llm-pi-ai-parse-json "{\"a\":Infinity}") :type 'json-parse-error)
  (should-error (llm-pi-ai-parse-json "{\"a\":NaN}") :type 'json-parse-error))

;;; Entry points (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-streaming-empty ()
  "Empty and blank text gives an empty object, as `{}' does."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json nil) (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "") (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "  \n ") (llm-pi-ai-json-object))))

(ert-deftest llm-pi-ai-utils-json-parse-test-streaming-keeps-text ()
  "A stream cut off in a string keeps the text so far, ended with a quote."
  (should (llm-pi-ai-utils-json-parse-test--same (gethash "text" (llm-pi-ai-parse-streaming-json "{\"text\":\"hel"))
                 "hel")))

(ert-deftest llm-pi-ai-utils-json-parse-test-streaming-drops-and-closes ()
  "A member cut off before its value is dropped and the object closed."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":1,\"b\":")
                 (llm-pi-ai-utils-json-parse-test--object "a" 1)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "[1,2,") [1 2]))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":[1,{\"b\":2")
                 (llm-pi-ai-utils-json-parse-test--object
                  "a" (vector 1 (llm-pi-ai-utils-json-parse-test--object "b" 2))))))

(ert-deftest llm-pi-ai-utils-json-parse-test-streaming-unreadable ()
  "Text with nothing to read gives an empty object.
What counts as nothing to read is the source's `partial-json' 0.1.7's:
blank text it refuses its own way, and a comma it reads as no value.
`{\"a\":]}' is `a' with the empty array the `[' the library steps
over expected -- its answer is `{\"a\":[]}' -- and the source's
`parseStreamingJson' gives an empty object wherever the library
throws.  The port answers an empty object in all three cases: what
the source answers, which is what is expected here."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json ",") (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":]}")
                 (llm-pi-ai-json-object))))

;;; Words cut off (port)

(ert-deftest llm-pi-ai-utils-json-parse-test-partial-word-prefix ()
  "A word the text cut off stands for the value it has got to.
Checked against `partial-json' 0.1.7, whose lexer reads as many
characters of `true', `false', `null', `NaN' and `Infinity' as the
text holds and then reads them with `JSON.parse'.  So `NaN' cut off
is the null `JSON.parse' makes of it (`JSON.stringify' drops it
from the object, and the value is null), and `Infinity' or
`-Infinity' cut off is a null too: `NaN' and `Infinity' are no JSON
number to the lexer's `JSON.parse'."
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":tr")
                 (llm-pi-ai-utils-json-parse-test--object "a" t)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":fals")
                 (llm-pi-ai-utils-json-parse-test--object "a" :false)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":nu")
                 (llm-pi-ai-utils-json-parse-test--object "a" :null)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":Na")
                 (llm-pi-ai-utils-json-parse-test--object "a" :null)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":Inf")
                 (llm-pi-ai-utils-json-parse-test--object "a" :null)))
  ;; A word the text cut off before it was one is no value, and the
  ;; member is dropped.
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":-inf")
                 (llm-pi-ai-json-object)))
  (should (llm-pi-ai-utils-json-parse-test--same (llm-pi-ai-parse-streaming-json "{\"a\":x")
                 (llm-pi-ai-json-object))))
