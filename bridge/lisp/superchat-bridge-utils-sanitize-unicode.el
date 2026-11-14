;;; superchat-bridge-utils-sanitize-unicode.el --- Unpaired surrogates, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/sanitize-unicode.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `superchat-bridge-sanitize-surrogates'   the source's `sanitizeSurrogates'
;;
;; A JavaScript string is a sequence of UTF-16 code units, so it can
;; hold half of a surrogate pair: a high surrogate (U+D800..U+DBFF)
;; with no low one after it, or a low surrogate (U+DC00..U+DFFF) with
;; no high one before it.  Such a string cannot be written as JSON,
;; and the source removes those halves.  An Emacs string is a
;; sequence of characters, and an astral character such as U+1F648 is
;; one element, not two.  The equivalent of "an unpaired surrogate"
;; is therefore a character in U+D800..U+DFFF in the string, which
;; is what `decode-coding-string' and friends leave behind for broken
;; input.  This port applies the source's rule to those characters,
;; element by element: a high surrogate stays only when the next
;; character is a low surrogate, a low surrogate only when the
;; previous one is a high surrogate, and every other one goes.  Both
;; tests look at the original string, as the source's regexp (with
;; its lookahead and lookbehind) does.  Every character outside
;; U+D800..U+DFFF, astral ones included, is kept.
;;
;; The file also holds two small string helpers which are not in the
;; source but which the other utils files need, because JavaScript's
;; `trim' and `length' differ from Emacs's: `superchat-bridge-string-trim-js'
;; and `superchat-bridge-utf16-length'.

;;; Code:

(require 'cl-lib)

(defconst superchat-bridge--surrogate-regexp
  (concat "[" (string #xD800) "-" (string #xDFFF) "]")
  "Matches a character in U+D800..U+DFFF, built at run time.")

(defun superchat-bridge--high-surrogate-p (char)
  "Return non-nil when CHAR is in U+D800..U+DBFF."
  (and (characterp char) (>= char #xD800) (<= char #xDBFF)))

(defun superchat-bridge--low-surrogate-p (char)
  "Return non-nil when CHAR is in U+DC00..U+DFFF."
  (and (characterp char) (>= char #xDC00) (<= char #xDFFF)))

(defun superchat-bridge-sanitize-surrogates (text)
  "Return TEXT without its unpaired surrogate characters.
The source's `sanitizeSurrogates'.  See the file commentary for what
an unpaired surrogate is in an Emacs string.  A string without any
character in U+D800..U+DFFF is returned as it is."
  (if (not (string-match-p superchat-bridge--surrogate-regexp text))
      text
    (let ((last (1- (length text)))
          (out nil))
      (dotimes (i (length text))
        (let ((char (aref text i)))
          (cond
           ((superchat-bridge--high-surrogate-p char)
            (when (and (< i last)
                       (superchat-bridge--low-surrogate-p (aref text (1+ i))))
              (push char out)))
           ((superchat-bridge--low-surrogate-p char)
            (when (and (> i 0)
                       (superchat-bridge--high-surrogate-p (aref text (1- i))))
              (push char out)))
           (t (push char out)))))
      (concat (nreverse out)))))

;;; JavaScript string helpers (not in the source)

(defconst superchat-bridge--js-whitespace
  "[ \t\n\v\f\r   -     　﻿]+"
  "Regexp for a run of what JavaScript's `String.prototype.trim' removes.")

(defun superchat-bridge-string-trim-js (string)
  "Return STRING without leading and trailing JavaScript whitespace.
`String.prototype.trim' removes all Unicode white space, the no-break
space and the byte order mark among it, where `string-trim' removes
only ASCII white space."
  (string-trim string superchat-bridge--js-whitespace superchat-bridge--js-whitespace))

(defun superchat-bridge-utf16-length (string)
  "Return the length of STRING in UTF-16 code units.
That is JavaScript's `string.length': a character above U+FFFF counts
as two."
  (let ((n (length string)))
    (dotimes (i (length string))
      (when (> (aref string i) #xFFFF)
        (setq n (1+ n))))
    n))

(provide 'superchat-bridge-utils-sanitize-unicode)

;;; superchat-bridge-utils-sanitize-unicode.el ends here
