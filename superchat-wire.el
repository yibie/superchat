;;; superchat-wire.el --- Text a provider request can carry -*- lexical-binding: t; -*-

;;; Commentary:
;; A provider request is JSON, and `json-serialize' takes Unicode scalar
;; values only.  An Emacs string holds more than that: one half of a
;; surrogate pair as a character of its own (telega keeps an emoji of a chat
;; title as two of them), a raw byte (a multibyte character cut in half, a
;; file read as bytes), a character above U+10FFFF, or bytes in a unibyte
;; string.  One such character anywhere in a request and the request cannot be
;; built at all.
;;
;; `superchat-wire-safe-text' is the one statement of "this text can be
;; sent": what it returns is taken by `json-serialize'.  The tool execution
;; seam applies it to a result before the result goes to the Tape, the View
;; and the provider, and the model transport applies it to every text of a
;; request before each attempt, so the place a text came in by does not
;; matter (docs/decisions/accepted/2026-11-09-a-request-that-cannot-be-encoded.md,
;; docs/postmortems/0002-text-a-request-could-not-carry.md).

;;; Code:

(defconst superchat-wire-replacement-character #xFFFD
  "What stands for a character or byte a request cannot carry.")

(defun superchat-wire-text-p (text)
  "Return non-nil when `json-serialize' takes the string TEXT as it is."
  (condition-case nil
      (and (json-serialize (vector text)) t)
    (error nil)))

(defsubst superchat-wire--raw-byte-p (character)
  "Return non-nil when CHARACTER is a raw byte in a multibyte string."
  (>= character #x3FFF80))

(defsubst superchat-wire--high-surrogate-p (character)
  (<= #xD800 character #xDBFF))

(defsubst superchat-wire--low-surrogate-p (character)
  (<= #xDC00 character #xDFFF))

(defun superchat-wire--decode-bytes (bytes)
  "Return the characters the list BYTES spells as UTF-8, oldest first.
A byte that is part of no character stays a raw byte.  Three bytes that spell
one half of a surrogate pair, which is how SQLite hands back a half the Tape
was given, become that half."
  (let ((characters
         (append (decode-coding-string (apply #'unibyte-string bytes) 'utf-8)
                 nil))
        (out nil))
    (while characters
      (let ((first (nth 0 characters))
            (second (nth 1 characters))
            (third (nth 2 characters)))
        (if (and (eql first #x3FFFED)
                 second (<= #x3FFFA0 second #x3FFFBF)
                 third (<= #x3FFF80 third #x3FFFBF))
            (progn
              (push (logior #xD000
                            (ash (logand second #x3F) 6)
                            (logand third #x3F))
                    out)
              (setq characters (nthcdr 3 characters)))
          (push first out)
          (setq characters (cdr characters)))))
    (nreverse out)))

(defun superchat-wire--repair (text)
  "Return the string TEXT with every character a request can carry.
Bytes that spell UTF-8 are read as the text they spell, the two halves of a
surrogate pair become the character they stand for, and what is still no
Unicode character becomes `superchat-wire-replacement-character'."
  (let ((characters (append (if (multibyte-string-p text)
                                text
                              (string-to-multibyte text))
                            nil))
        (decoded nil)
        (out nil))
    ;; Bytes first: a run of them may spell text, or a surrogate half.
    (while characters
      (if (superchat-wire--raw-byte-p (car characters))
          (let ((bytes nil))
            (while (and characters
                        (superchat-wire--raw-byte-p (car characters)))
              (push (logand (pop characters) #xFF) bytes))
            (setq decoded
                  (nconc (nreverse (superchat-wire--decode-bytes
                                    (nreverse bytes)))
                         decoded)))
        (push (pop characters) decoded)))
    (setq decoded (nreverse decoded))
    (while decoded
      (let ((character (pop decoded)))
        (push (cond
               ((and (superchat-wire--high-surrogate-p character)
                     decoded
                     (superchat-wire--low-surrogate-p (car decoded)))
                (+ #x10000
                   (ash (- character #xD800) 10)
                   (- (pop decoded) #xDC00)))
               ((or (<= #xD800 character #xDFFF)
                    (> character #x10FFFF))
                superchat-wire-replacement-character)
               (t character))
              out)))
    (concat (nreverse out))))

(defun superchat-wire-safe-text (text)
  "Return TEXT as a string `json-serialize' takes.
A string it already takes is returned itself, so the common case costs one
encoding and changes nothing.  Otherwise the result is a new string: see
`superchat-wire--repair'.  A value that is no string is returned as it is."
  (cond
   ((not (stringp text)) text)
   ((superchat-wire-text-p text) text)
   (t (superchat-wire--repair text))))

(provide 'superchat-wire)
;;; superchat-wire.el ends here
