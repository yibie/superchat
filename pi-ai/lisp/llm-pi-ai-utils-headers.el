;;; llm-pi-ai-utils-headers.el --- Header records, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/headers.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `llm-pi-ai-headers-to-record'           the source's `headersToRecord'
;;   `llm-pi-ai-provider-headers-to-record'  `providerHeadersToRecord'
;;
;; A JavaScript record of strings is an alist of (NAME . VALUE) here,
;; in the order the record's keys come in.  A `Headers' object has no
;; Emacs twin; the HTTP layer (`llm-pi-ai-http') hands over an alist of
;; response headers with lower-case names, and that is what
;; `llm-pi-ai-headers-to-record' takes in its place, doing what the
;; `Headers' iterator does on the way: names lower-cased, the values of
;; one name joined with ", " (`set-cookie' excepted, which the iterator
;; yields one by one, so the last one is what a record keeps), values
;; trimmed of HTTP white space, and entries sorted by name.
;;
;; `ProviderHeaders' is a record of string or null; the null (a
;; header to remove) is nil here, or `:null' as `json-parse-string'
;; spells it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun llm-pi-ai-headers-to-record (headers)
  "Return HEADERS, an alist of (NAME . VALUE) strings, as a record.
The source's `headersToRecord' for a `Headers' object, see the file
commentary.  The result is an alist with one entry per lower-case
name, sorted by name."
  (let ((table (make-hash-table :test #'equal))
        (names nil))
    (dolist (header headers)
      (let ((name (downcase (car header)))
            (value (string-trim (cdr header) "[ \t\n\r]+" "[ \t\n\r]+")))
        (unless (gethash name table)
          (push name names))
        (puthash name
                 (if (or (equal name "set-cookie")
                         (null (gethash name table)))
                     (list value)
                   (append (gethash name table) (list value)))
                 table)))
    (mapcar (lambda (name)
              (let ((values (gethash name table)))
                (cons name (if (equal name "set-cookie")
                               (car (last values))
                             (string-join values ", ")))))
            (sort names #'string<))))

(defun llm-pi-ai-provider-headers-to-record (&rest header-sources)
  "Merge HEADER-SOURCES, alists of `ProviderHeaders', into one record.
The source's `providerHeadersToRecord'.  A source is an alist of
\(NAME . VALUE), or nil.  Names are compared without regard to case:
a later source's entry replaces an earlier one's, the name with the
spelling the later one gave it, and a nil (or `:null') value removes
the header.  The result is an alist in the order the merged entries
were last set in, or nil when no header is left, where the source
returns undefined."
  (let ((merged nil))
    (dolist (source header-sources)
      (dolist (entry source)
        (let ((normalized (downcase (car entry)))
              (value (cdr entry)))
          (setq merged (cl-remove normalized merged
                                  :key (lambda (item) (downcase (car item)))
                                  :test #'equal))
          (unless (or (null value) (eq value :null))
            (setq merged (append merged (list (cons (car entry) value))))))))
    merged))

(provide 'llm-pi-ai-utils-headers)

;;; llm-pi-ai-utils-headers.el ends here
