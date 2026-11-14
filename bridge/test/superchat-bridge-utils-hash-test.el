;;; superchat-bridge-utils-hash-test.el --- Tests for superchat-bridge-utils-hash  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-hash.el', the port of `src/utils/hash.ts'.
;; The upstream checkout keeps no test for it.  Every expected value
;; is the output of the source's `shortHash', run under node 22
;; (`node --experimental-strip-types') on the same string.  The hash
;; goes into request fields, so these must never differ.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-hash)

(ert-deftest superchat-bridge-utils-hash-test-values-from-node ()
  "Short hashes equal what the source gives."
  ;; shortHash("")
  (should (equal (superchat-bridge-short-hash "") "k4n83c7h0j2b"))
  ;; shortHash("a")
  (should (equal (superchat-bridge-short-hash "a") "m8735310ae7sx"))
  ;; shortHash("abc")
  (should (equal (superchat-bridge-short-hash "abc") "y0biex7f9bbh"))
  ;; shortHash("hello world")
  (should (equal (superchat-bridge-short-hash "hello world") "n7rb4n1m39uz8"))
  ;; shortHash("call_abc123|fc_def456")
  (should (equal (superchat-bridge-short-hash "call_abc123|fc_def456") "cckprgfsnj4x"))
  ;; shortHash("x".repeat(500))
  (should (equal (superchat-bridge-short-hash (make-string 500 ?x)) "130aesn1aldl4j"))
  ;; shortHash("call_" + "y".repeat(450) + "|fc_" + "z".repeat(100))
  (should (equal (superchat-bridge-short-hash
                  (concat "call_" (make-string 450 ?y) "|fc_" (make-string 100 ?z)))
                 "1hi92rx1k5f1pl")))

(ert-deftest superchat-bridge-utils-hash-test-non-ascii ()
  "Characters outside ASCII are hashed as UTF-16 units."
  ;; shortHash("你好")
  (should (equal (superchat-bridge-short-hash "你好") "603p56zx32gv"))
  ;; shortHash("é")
  (should (equal (superchat-bridge-short-hash "é") "1rr184m1efd1ir"))
  ;; shortHash("ÿĀ￿")
  (should (equal (superchat-bridge-short-hash "ÿĀ￿") "hma6zdzdru6i")))

(ert-deftest superchat-bridge-utils-hash-test-astral-is-two-units ()
  "A character above U+FFFF is hashed as its surrogate pair."
  ;; shortHash("emoji \u{1F648} x"), JavaScript counting two units
  (should (equal (superchat-bridge-short-hash "emoji \U0001F648 x") "p0eigw1autlio"))
  ;; The same string written with the two surrogate characters
  ;; hashes alike.
  (should (equal (superchat-bridge-short-hash
                  (concat "emoji " (string #xD83D #xDE48) " x"))
                 "p0eigw1autlio"))
  ;; shortHash("\ud83d lone"): a lone surrogate is one unit
  (should (equal (superchat-bridge-short-hash (concat (string #xD83D) " lone"))
                 "vd93coy8x17n")))

(provide 'superchat-bridge-utils-hash-test)
;;; superchat-bridge-utils-hash-test.el ends here
