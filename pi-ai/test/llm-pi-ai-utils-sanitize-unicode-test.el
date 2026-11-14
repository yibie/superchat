;;; llm-pi-ai-utils-sanitize-unicode-test.el --- Tests for sanitize-unicode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-sanitize-unicode.el', the port of
;; `src/utils/sanitize-unicode.ts'.  The upstream checkout keeps no
;; test for it.  Every expectation below was taken by running the
;; source's `sanitizeSurrogates' under node 22 on the same sequence of
;; UTF-16 units (a high surrogate H = U+D83D, a low one L = U+DE48) and
;; reading the units it left; `(string #xD83D)' is the Emacs
;; character that stands for such a unit, see the file's commentary.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-sanitize-unicode)

(defconst llm-pi-ai-sanitize-test--h (string #xD83D))
(defconst llm-pi-ai-sanitize-test--l (string #xDE48))

(defun llm-pi-ai-sanitize-test--run (&rest parts)
  "Sanitize the concatenation of PARTS."
  (llm-pi-ai-sanitize-surrogates (apply #'concat parts)))

(ert-deftest llm-pi-ai-utils-sanitize-unicode-test-plain-text ()
  "Text without surrogates, astral characters included, is kept."
  (should (equal (llm-pi-ai-sanitize-surrogates "") ""))
  (should (equal (llm-pi-ai-sanitize-surrogates "x") "x"))
  (should (equal (llm-pi-ai-sanitize-surrogates "Hello \U0001F648 World")
                 "Hello \U0001F648 World")))

(ert-deftest llm-pi-ai-utils-sanitize-unicode-test-unpaired-removed ()
  "A lone high or low surrogate goes."
  (let ((h llm-pi-ai-sanitize-test--h) (l llm-pi-ai-sanitize-test--l))
    ;; sanitizeSurrogates("a\ud83db") => "ab"
    (should (equal (llm-pi-ai-sanitize-test--run "a" h "b") "ab"))
    ;; sanitizeSurrogates("a\ude48b") => "ab"
    (should (equal (llm-pi-ai-sanitize-test--run "a" l "b") "ab"))
    (should (equal (llm-pi-ai-sanitize-test--run h) ""))
    (should (equal (llm-pi-ai-sanitize-test--run l) ""))
    ;; "\ude48\ud83d" (low then high: both unpaired) => ""
    (should (equal (llm-pi-ai-sanitize-test--run l h) ""))
    ;; "\ude48\ude48" => ""
    (should (equal (llm-pi-ai-sanitize-test--run l l) ""))))

(ert-deftest llm-pi-ai-utils-sanitize-unicode-test-pairs-kept ()
  "A high surrogate followed by a low one is a pair and is kept."
  (let ((h llm-pi-ai-sanitize-test--h) (l llm-pi-ai-sanitize-test--l))
    ;; "a🙈b" => unchanged
    (should (equal (llm-pi-ai-sanitize-test--run "a" h l "b")
                   (concat "a" h l "b")))
    ;; "\ud83d🙈" => "🙈"
    (should (equal (llm-pi-ai-sanitize-test--run h h l) (concat h l)))
    ;; "🙈\ude48" => "🙈"
    (should (equal (llm-pi-ai-sanitize-test--run h l l) (concat h l)))))

(provide 'llm-pi-ai-utils-sanitize-unicode-test)
;;; llm-pi-ai-utils-sanitize-unicode-test.el ends here
