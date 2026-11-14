;;; llm-pi-ai-utils-hash.el --- Short deterministic hash, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/hash.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `llm-pi-ai-short-hash'   the source's `shortHash'
;;
;; The result ends up in request fields (ids, cache keys), so it must
;; be the very string the source gives.  Two things make that more than
;; a transcription:
;;
;;  - The source hashes UTF-16 code units (`charCodeAt').  An Emacs
;;    string holds characters, so a character above U+FFFF is hashed as
;;    its two surrogate code units, high one first.
;;  - The source uses 32-bit integer arithmetic (`Math.imul', `>>>').
;;    All values here are kept as unsigned 32-bit integers; `imul' only
;;    needs the low 32 bits of the product, and an XOR or a logical
;;    shift acts on the same bits whether or not they are read as
;;    signed, so the unsigned form gives the same final bits.

;;; Code:

(defconst llm-pi-ai--u32-mask #xFFFFFFFF)

(defun llm-pi-ai--imul (a b)
  "Return the low 32 bits of A times B, as an unsigned number.
`Math.imul' of the source, with its result read as unsigned."
  (logand (* a b) llm-pi-ai--u32-mask))

(defun llm-pi-ai--utf16-units (string)
  "Return the UTF-16 code units of STRING, a list of integers.
A character above U+FFFF gives a surrogate pair; any other character,
a lone surrogate included, gives itself."
  (let ((units nil))
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (if (and (> char #xFFFF) (<= char #x10FFFF))
            (let ((offset (- char #x10000)))
              (push (+ #xD800 (ash offset -10)) units)
              (push (+ #xDC00 (logand offset #x3FF)) units))
          (push (logand char #xFFFF) units))))
    (nreverse units)))

(defun llm-pi-ai--to-string-36 (number)
  "Return NUMBER, a non-negative integer, as lower-case base 36 text.
`Number.prototype.toString(36)' of the source."
  (if (= number 0)
      "0"
    (let ((digits nil))
      (while (> number 0)
        (push (aref "0123456789abcdefghijklmnopqrstuvwxyz" (% number 36))
              digits)
        (setq number (/ number 36)))
      (concat digits))))

(defun llm-pi-ai-short-hash (string)
  "Return a short deterministic hash of STRING, as the source does.
The source's `shortHash': two 32-bit mixes of the UTF-16 code units
of STRING, each written in base 36, the second one first.  Not
cryptographic."
  (let ((h1 #xdeadbeef)
        (h2 #x41c6ce57))
    (dolist (ch (llm-pi-ai--utf16-units string))
      (setq h1 (llm-pi-ai--imul (logxor h1 ch) 2654435761))
      (setq h2 (llm-pi-ai--imul (logxor h2 ch) 1597334677)))
    (setq h1 (logxor (llm-pi-ai--imul (logxor h1 (ash h1 -16)) 2246822507)
                     (llm-pi-ai--imul (logxor h2 (ash h2 -13)) 3266489909)))
    (setq h2 (logxor (llm-pi-ai--imul (logxor h2 (ash h2 -16)) 2246822507)
                     (llm-pi-ai--imul (logxor h1 (ash h1 -13)) 3266489909)))
    (concat (llm-pi-ai--to-string-36 h2)
            (llm-pi-ai--to-string-36 h1))))

(provide 'llm-pi-ai-utils-hash)

;;; llm-pi-ai-utils-hash.el ends here
