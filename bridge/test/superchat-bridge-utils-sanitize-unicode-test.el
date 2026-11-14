;;; superchat-bridge-utils-sanitize-unicode-test.el --- Tests for sanitize-unicode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-sanitize-unicode.el', the port of
;; `src/utils/sanitize-unicode.ts'.  The upstream checkout keeps no
;; test for it.  Every expectation below was taken by running the
;; source's `sanitizeSurrogates' under node 22 on the same sequence of
;; UTF-16 units (a high surrogate H = U+D83D, a low one L = U+DE48) and
;; reading the units it left; `(string #xD83D)' is the Emacs
;; character that stands for such a unit, see the file's commentary.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-sanitize-unicode)

(defconst superchat-bridge-sanitize-test--h (string #xD83D))
(defconst superchat-bridge-sanitize-test--l (string #xDE48))

(defun superchat-bridge-sanitize-test--run (&rest parts)
  "Sanitize the concatenation of PARTS."
  (superchat-bridge-sanitize-surrogates (apply #'concat parts)))

(ert-deftest superchat-bridge-utils-sanitize-unicode-test-plain-text ()
  "Text without surrogates, astral characters included, is kept."
  (should (equal (superchat-bridge-sanitize-surrogates "") ""))
  (should (equal (superchat-bridge-sanitize-surrogates "x") "x"))
  (should (equal (superchat-bridge-sanitize-surrogates "Hello \U0001F648 World")
                 "Hello \U0001F648 World")))

(ert-deftest superchat-bridge-utils-sanitize-unicode-test-unpaired-removed ()
  "A lone high or low surrogate goes."
  (let ((h superchat-bridge-sanitize-test--h) (l superchat-bridge-sanitize-test--l))
    ;; sanitizeSurrogates("a\ud83db") => "ab"
    (should (equal (superchat-bridge-sanitize-test--run "a" h "b") "ab"))
    ;; sanitizeSurrogates("a\ude48b") => "ab"
    (should (equal (superchat-bridge-sanitize-test--run "a" l "b") "ab"))
    (should (equal (superchat-bridge-sanitize-test--run h) ""))
    (should (equal (superchat-bridge-sanitize-test--run l) ""))
    ;; "\ude48\ud83d" (low then high: both unpaired) => ""
    (should (equal (superchat-bridge-sanitize-test--run l h) ""))
    ;; "\ude48\ude48" => ""
    (should (equal (superchat-bridge-sanitize-test--run l l) ""))))

(ert-deftest superchat-bridge-utils-sanitize-unicode-test-pairs-kept ()
  "A high surrogate followed by a low one is a pair and is kept."
  (let ((h superchat-bridge-sanitize-test--h) (l superchat-bridge-sanitize-test--l))
    ;; "a🙈b" => unchanged
    (should (equal (superchat-bridge-sanitize-test--run "a" h l "b")
                   (concat "a" h l "b")))
    ;; "\ud83d🙈" => "🙈"
    (should (equal (superchat-bridge-sanitize-test--run h h l) (concat h l)))
    ;; "🙈\ude48" => "🙈"
    (should (equal (superchat-bridge-sanitize-test--run h l l) (concat h l)))))

(provide 'superchat-bridge-utils-sanitize-unicode-test)
;;; superchat-bridge-utils-sanitize-unicode-test.el ends here
