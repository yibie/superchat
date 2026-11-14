;;; llm-pi-ai-utils-text-test.el --- Tests for llm-pi-ai-utils-text  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-text.el', its port of `src/utils/text.ts'.
;;
;; The upstream checkout keeps no `text.test.ts': `packages/ai' has no
;; test for this file, so each expectation below is the source
;; function's own output, taken by running `src/utils/text.ts' itself
;; (types erased, the `import type' dropped) under node 22 and writing
;; down what it printed.  The expressions are listed next to each
;; test, so that they can be run again against a later checkout.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-text)

;;; Content text

(ert-deftest llm-pi-ai-utils-text-test-content-text-string ()
  "A string content is itself, whatever the separator."
  ;; contentText('hi')
  (should (equal (llm-pi-ai-content-text "hi") "hi"))
  ;; contentText('hi', '-')
  (should (equal (llm-pi-ai-content-text "hi" "-") "hi"))
  (should (equal (llm-pi-ai-content-text "") "")))

(ert-deftest llm-pi-ai-utils-text-test-content-text-joins-text-blocks ()
  "Only the text blocks' text is joined, with a newline by default."
  ;; contentText([{type:'text',text:'a'},{type:'toolCall',id:'x',name:'n',arguments:{}},{type:'text',text:'b'}])
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-text-content :text "a")
                        (llm-pi-ai-make-tool-call :id "x" :name "n" :arguments '())
                        (llm-pi-ai-make-text-content :text "b")))
                 "a\nb"))
  ;; contentText([{type:'thinking',thinking:'t'},{type:'text',text:'a'},{type:'text',text:'b'}], '-')
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-thinking-content :thinking "t")
                        (llm-pi-ai-make-text-content :text "a")
                        (llm-pi-ai-make-text-content :text "b"))
                  "-")
                 "a-b")))

(ert-deftest llm-pi-ai-utils-text-test-content-text-without-text ()
  "No text block gives the empty string: no separator is added."
  ;; contentText([])
  (should (equal (llm-pi-ai-content-text '()) ""))
  ;; contentText([{type:'thinking',thinking:'t'},{type:'image',data:'AA',mimeType:'image/png'}])
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-thinking-content :thinking "t")
                        (llm-pi-ai-make-image-content :data "AA" :mime-type "image/png")))
                 "")))

(ert-deftest llm-pi-ai-utils-text-test-content-text-empty-block ()
  "An empty text block is text: it is joined in."
  ;; contentText([{type:'text',text:''},{type:'text',text:'b'}])
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-text-content :text "")
                        (llm-pi-ai-make-text-content :text "b")))
                 "\nb"))
  ;; contentText([{type:'text',text:'a'},{type:'text',text:''}])
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-text-content :text "a")
                        (llm-pi-ai-make-text-content :text "")))
                 "a\n")))

(ert-deftest llm-pi-ai-utils-text-test-content-text-single-block ()
  "One text block takes no separator."
  ;; contentText([{type:'text',text:'only'}], ' -- ')
  (should (equal (llm-pi-ai-content-text
                  (list (llm-pi-ai-make-text-content :text "only"))
                  " -- ")
                 "only")))

;;; System message text

(ert-deftest llm-pi-ai-utils-text-test-system-message-text-plain ()
  "A system message without sections is its content."
  ;; getSystemMessageText({role:'system',content:'sys',timestamp:0})
  (should (equal (llm-pi-ai-system-message-text
                  (llm-pi-ai-make-system-message :content "sys"))
                 "sys")))

(ert-deftest llm-pi-ai-utils-text-test-system-message-text-sections ()
  "Sections follow the content, a blank line between, and a nil section
is left out."
  ;; getSystemMessageText({role:'system',
  ;;   content:[{type:'text',text:'p'},{type:'text',text:'q'}],
  ;;   sections:{a:'A',b:null,c:'C'},timestamp:0})
  (should (equal (llm-pi-ai-system-message-text
                  (llm-pi-ai-make-system-message
                   :content (list (llm-pi-ai-make-text-content :text "p")
                                  (llm-pi-ai-make-text-content :text "q"))
                   :sections '(("a" . "A") ("b") ("c" . "C"))))
                 "p\nq\n\nA\n\nC")))

(ert-deftest llm-pi-ai-utils-text-test-system-message-text-drops-empties ()
  "An empty content or section is no part at all; all empty is `'."
  ;; getSystemMessageText({role:'system',content:'',sections:{a:'',b:''},timestamp:0})
  (should (equal (llm-pi-ai-system-message-text
                  (llm-pi-ai-make-system-message
                   :content ""
                   :sections '(("a" . "") ("b" . ""))))
                 ""))
  ;; getSystemMessageText({role:'system',content:'',sections:{a:'',b:'B'},timestamp:0})
  (should (equal (llm-pi-ai-system-message-text
                  (llm-pi-ai-make-system-message
                   :content ""
                   :sections '(("a" . "") ("b" . "B"))))
                 "B")))

(ert-deftest llm-pi-ai-utils-text-test-system-message-text-no-sections ()
  "Absent sections read as nil, as `sections ?? {}' does."
  ;; getSystemMessageText({role:'system',content:'',sections:null,timestamp:0})
  (should (equal (llm-pi-ai-system-message-text
                  (llm-pi-ai-make-system-message :content ""))
                 "")))

;;; System message update

(ert-deftest llm-pi-ai-utils-text-test-system-message-update ()
  "Content first, then each section by name: a removed one says so, a
changed one carries its text after a blank line."
  ;; renderSystemMessageUpdate({role:'system',content:'add',
  ;;   sections:{tools:'T',removed:null},timestamp:0})
  (should (equal (llm-pi-ai-render-system-message-update
                  (llm-pi-ai-make-system-message
                   :content "add"
                   :sections '(("tools" . "T") ("removed"))))
                 (concat "add\n\n"
                         "Updated system prompt section \"tools\":\n\nT\n\n"
                         "Removed system prompt section \"removed\"."))))

(ert-deftest llm-pi-ai-utils-text-test-system-message-update-content-only ()
  "Without sections only the content is rendered, and empty content is
no part."
  ;; renderSystemMessageUpdate({role:'system',content:'',sections:null,timestamp:0})
  (should (equal (llm-pi-ai-render-system-message-update
                  (llm-pi-ai-make-system-message :content ""))
                 ""))
  ;; renderSystemMessageUpdate({role:'system',content:'only',sections:null,timestamp:0})
  (should (equal (llm-pi-ai-render-system-message-update
                  (llm-pi-ai-make-system-message :content "only"))
                 "only")))

(ert-deftest llm-pi-ai-utils-text-test-system-message-update-all-removed ()
  "Every section is reported, in the order it was added in."
  ;; renderSystemMessageUpdate({role:'system',
  ;;   content:[{type:'text',text:'p'},{type:'text',text:'q'}],
  ;;   sections:{x:null,y:null},timestamp:0})
  (should (equal (llm-pi-ai-render-system-message-update
                  (llm-pi-ai-make-system-message
                   :content (list (llm-pi-ai-make-text-content :text "p")
                                  (llm-pi-ai-make-text-content :text "q"))
                   :sections '(("x") ("y"))))
                 (concat "p\nq\n\n"
                         "Removed system prompt section \"x\".\n\n"
                         "Removed system prompt section \"y\"."))))

(provide 'llm-pi-ai-utils-text-test)

;;; llm-pi-ai-utils-text-test.el ends here
