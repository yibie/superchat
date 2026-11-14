;;; superchat-bridge-utils-headers-test.el --- Tests for superchat-bridge-utils-headers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-headers.el', the port of
;; `src/utils/headers.ts'.  The upstream checkout keeps no test for it;
;; the expected values come from running the source's two functions
;; under node 22, with a `Headers' object built from the same pairs.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-headers)

(ert-deftest superchat-bridge-utils-headers-test-to-record ()
  "Names lower-cased and sorted, duplicates joined, values trimmed."
  ;; headersToRecord(new Headers([["Content-Type","a"],["X-B","1"],
  ;;   ["x-b","2"],["Set-Cookie","a=1"],["Set-Cookie","b=2"],["A-Z","  pad  "]]))
  ;;   => {"a-z":"pad","content-type":"a","set-cookie":"b=2","x-b":"1, 2"}
  (should (equal (superchat-bridge-headers-to-record
                  '(("Content-Type" . "a") ("X-B" . "1") ("x-b" . "2")
                    ("Set-Cookie" . "a=1") ("Set-Cookie" . "b=2")
                    ("A-Z" . "  pad  ")))
                 '(("a-z" . "pad") ("content-type" . "a")
                   ("set-cookie" . "b=2") ("x-b" . "1, 2"))))
  (should (equal (superchat-bridge-headers-to-record nil) nil)))

(ert-deftest superchat-bridge-utils-headers-test-provider-headers ()
  "Later sources win, by name without case; nil removes."
  ;; providerHeadersToRecord({A:"1",B:"2"},{a:null,C:"3"},undefined,{b:"22"})
  ;;   => {"C":"3","b":"22"}
  (should (equal (superchat-bridge-provider-headers-to-record
                  '(("A" . "1") ("B" . "2"))
                  '(("a") ("C" . "3"))
                  nil
                  '(("b" . "22")))
                 '(("C" . "3") ("b" . "22"))))
  ;; the JSON null spelling too
  (should (equal (superchat-bridge-provider-headers-to-record
                  '(("A" . "1")) '(("A" . :null)))
                 nil))
  ;; providerHeadersToRecord({A:null}) => undefined
  (should (null (superchat-bridge-provider-headers-to-record '(("A")))))
  ;; providerHeadersToRecord(undefined) => undefined
  (should (null (superchat-bridge-provider-headers-to-record nil)))
  (should (null (superchat-bridge-provider-headers-to-record))))

(provide 'superchat-bridge-utils-headers-test)
;;; superchat-bridge-utils-headers-test.el ends here
