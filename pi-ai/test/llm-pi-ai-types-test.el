;;; llm-pi-ai-types-test.el --- Tests for llm-pi-ai-types  -*- lexical-binding: t; -*-

;; Tests for Tier 0: the type structs mirroring src/types.ts and
;; src/providers/model-schema.ts.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-types)
(require 'cl-lib)

;;; Content blocks

(ert-deftest llm-pi-ai-types-test-text-content ()
  "A text content block carries its type and text."
  (let ((block (llm-pi-ai-make-text-content :text "hi")))
    (should (llm-pi-ai-text-content-p block))
    (should (llm-pi-ai-content-p block))
    (should (equal (llm-pi-ai-text-content-type block) "text"))
    (should (equal (llm-pi-ai-text-content-text block) "hi"))))

(ert-deftest llm-pi-ai-types-test-thinking-content ()
  "A thinking block defaults `redacted' to nil and types as thinking."
  (let ((block (llm-pi-ai-make-thinking-content :thinking "why")))
    (should (equal (llm-pi-ai-thinking-content-type block) "thinking"))
    (should (equal (llm-pi-ai-thinking-content-thinking block) "why"))
    (should-not (llm-pi-ai-thinking-content-redacted block))))

(ert-deftest llm-pi-ai-types-test-image-content ()
  "An image block keeps its data and media type."
  (let ((block (llm-pi-ai-make-image-content :data "AAA" :mime-type "image/png")))
    (should (equal (llm-pi-ai-image-content-type block) "image"))
    (should (equal (llm-pi-ai-image-content-mime-type block) "image/png"))))

(ert-deftest llm-pi-ai-types-test-tool-call ()
  "A tool call has type \"toolCall\" and its arguments error unset."
  (let ((call (llm-pi-ai-make-tool-call :id "c1" :name "read"
                                        :arguments '((:path . "a.txt")))))
    (should (llm-pi-ai-tool-call-p call))
    (should (llm-pi-ai-content-p call))
    (should (equal (llm-pi-ai-tool-call-type call) "toolCall"))
    (should (equal (llm-pi-ai-tool-call-name call) "read"))
    (should-not (llm-pi-ai-tool-call-arguments-error call))))

(ert-deftest llm-pi-ai-types-test-content-p-negative ()
  "A message is not a content block."
  (should-not (llm-pi-ai-content-p (llm-pi-ai-make-user-message)))
  (should-not (llm-pi-ai-content-p nil)))

;;; Messages

(ert-deftest llm-pi-ai-types-test-message-roles ()
  "Each constructor fixes the role the union type declares."
  (should (equal (llm-pi-ai-message-role (llm-pi-ai-make-system-message)) "system"))
  (should (equal (llm-pi-ai-message-role (llm-pi-ai-make-user-message)) "user"))
  (should (equal (llm-pi-ai-message-role (llm-pi-ai-make-assistant-message)) "assistant"))
  (should (equal (llm-pi-ai-message-role (llm-pi-ai-make-tool-result-message))
                 "toolResult")))

(ert-deftest llm-pi-ai-types-test-message-predicates ()
  "`llm-pi-ai-message-p' accepts all four message types and nothing else."
  (dolist (message (list (llm-pi-ai-make-system-message)
                         (llm-pi-ai-make-user-message)
                         (llm-pi-ai-make-assistant-message)
                         (llm-pi-ai-make-tool-result-message)))
    (should (llm-pi-ai-message-p message)))
  (should (llm-pi-ai-system-message-p (llm-pi-ai-make-system-message)))
  (should (llm-pi-ai-user-message-p (llm-pi-ai-make-user-message)))
  (should (llm-pi-ai-assistant-message-p (llm-pi-ai-make-assistant-message)))
  (should (llm-pi-ai-tool-result-message-p (llm-pi-ai-make-tool-result-message)))
  (should-not (llm-pi-ai-message-p (llm-pi-ai-make-text-content)))
  (should-not (llm-pi-ai-message-p nil)))

(ert-deftest llm-pi-ai-types-test-message-fields ()
  "Constructor keyword arguments land in the right slots."
  (let ((user (llm-pi-ai-make-user-message
               :content (list (llm-pi-ai-make-text-content :text "hello"))
               :timestamp 42)))
    (should (equal (llm-pi-ai-message-role user) "user"))
    (should (equal (llm-pi-ai-message-timestamp user) 42))
    (should (llm-pi-ai-text-content-p (car (llm-pi-ai-message-content user)))))
  (let ((assistant (llm-pi-ai-make-assistant-message
                    :api "openai-completions" :model "gpt-4o"
                    :stop-reason "stop")))
    (should (equal (llm-pi-ai-message-role assistant) "assistant"))
    (should (equal (llm-pi-ai-assistant-message-api assistant) "openai-completions"))
    (should (equal (llm-pi-ai-assistant-message-stop-reason assistant) "stop"))))

(ert-deftest llm-pi-ai-types-test-system-message-sections ()
  "A later system message can add tools and name prompt sections."
  (let ((message (llm-pi-ai-make-system-message
                  :sections '(("style" . "terse"))
                  :tools-added '("read") :tools-removed '("write"))))
    (should (equal (llm-pi-ai-system-message-sections message)
                   '(("style" . "terse"))))
    (should (equal (llm-pi-ai-system-message-tools-added message) '("read")))
    (should (equal (llm-pi-ai-system-message-tools-removed message) '("write")))))

(ert-deftest llm-pi-ai-types-test-tool-result-message ()
  "A tool result message keeps its call id and error flag."
  (let ((message (llm-pi-ai-make-tool-result-message
                  :tool-call-id "c1" :tool-name "read"
                  :content (list (llm-pi-ai-make-text-content :text "ok"))
                  :is-error nil)))
    (should (equal (llm-pi-ai-tool-result-message-tool-call-id message) "c1"))
    (should (equal (llm-pi-ai-tool-result-message-role message) "toolResult"))
    (should-not (llm-pi-ai-tool-result-message-is-error message))))

(ert-deftest llm-pi-ai-types-test-message-copier ()
  "Copying a message copies the struct together with the role."
  (let* ((user (llm-pi-ai-make-user-message :timestamp 1))
         (copy (llm-pi-ai-copy-user-message user)))
    (should-not (eq copy user))
    (should (llm-pi-ai-user-message-p copy))
    (should (equal (llm-pi-ai-message-role copy) "user"))
    (setf (llm-pi-ai-message-timestamp copy) 2)
    (should (equal (llm-pi-ai-message-timestamp user) 1))))

;;; Struct definition and re-evaluation

(ert-deftest llm-pi-ai-types-test-structs-reevaluate ()
  "Re-evaluating the library keeps every constructor working.
`cl-defstruct' writes its constructor as a lambda that only accepts
the keywords it saw; defining a struct must therefore take the old
function away first, or the new one keeps the old arity."
  (llm-pi-ai-make-user-message :timestamp 1)
  (llm-pi-ai-make-assistant-message :api "x")
  (llm-pi-ai-make-tool-result-message :tool-call-id "c")
  (llm-pi-ai-make-image-model :id "i")
  (llm-pi-ai-make-classifier-model :id "k")
  (load (locate-library "llm-pi-ai-types") nil t)
  (should (equal (llm-pi-ai-message-role
                  (llm-pi-ai-make-user-message :timestamp 2))
                 "user"))
  (should (equal (llm-pi-ai-assistant-message-api
                  (llm-pi-ai-make-assistant-message :api "openai-completions"))
                 "openai-completions"))
  (should (equal (llm-pi-ai-tool-result-message-tool-call-id
                  (llm-pi-ai-make-tool-result-message :tool-call-id "c2"))
                 "c2"))
  (should (eq (llm-pi-ai-get-model-type (llm-pi-ai-make-image-model))
              'image))
  (should (eq (llm-pi-ai-get-model-type (llm-pi-ai-make-classifier-model))
              'classifier)))

;;; Usage and cost

(ert-deftest llm-pi-ai-types-test-cost-defaults ()
  "Cost slots are numeric and default to zero."
  (let ((cost (llm-pi-ai-make-cost)))
    (should (equal (llm-pi-ai-cost-input cost) 0))
    (should (equal (llm-pi-ai-cost-cache-write cost) 0))))

;;; Models

(ert-deftest llm-pi-ai-types-test-model-type ()
  "A plain model is a chat model; sub-models report their own type."
  (let ((chat (llm-pi-ai-make-model :id "m" :name "M" :api "openai-completions")))
    (should (eq (llm-pi-ai-get-model-type chat) 'chat))
    (should (llm-pi-ai-is-model-type chat 'chat)))
  (let ((image (llm-pi-ai-make-image-model :id "img")))
    (should (eq (llm-pi-ai-get-model-type image) 'image))
    (should (llm-pi-ai-model-p image))
    (should (equal (llm-pi-ai-model-id image) "img")))
  (let ((classifier (llm-pi-ai-make-classifier-model :id "cls")))
    (should (eq (llm-pi-ai-get-model-type classifier) 'classifier))
    (should-not (llm-pi-ai-is-model-type classifier 'chat))))

(ert-deftest llm-pi-ai-types-test-image-model-output ()
  "An image model keeps the media it can produce."
  (let ((image (llm-pi-ai-make-image-model :output '("image" "text"))))
    (should (equal (llm-pi-ai-image-model-output image) '("image" "text")))))

(ert-deftest llm-pi-ai-types-test-model-input-limits ()
  "Input limits hold request size and image limits."
  (let ((limits (llm-pi-ai-make-model-input-limits :max-request-bytes 1024)))
    (should (equal (llm-pi-ai-model-input-limits-max-request-bytes limits) 1024))))

;;; Events

(ert-deftest llm-pi-ai-types-test-event ()
  "An event records its type and the fields that type carries."
  (let ((event (llm-pi-ai-make-event :type "text_delta" :content-index 0 :delta "he")))
    (should (equal (llm-pi-ai-assistant-message-event-type event) "text_delta"))
    (should (equal (llm-pi-ai-assistant-message-event-delta event) "he"))
    (should-not (llm-pi-ai-assistant-message-event-reason event))))

;;; Constants

(ert-deftest llm-pi-ai-types-test-constants ()
  "The schema constants keep the order and membership of the source."
  (should (equal llm-pi-ai-thinking-levels
                 '("minimal" "low" "medium" "high" "xhigh" "max")))
  (should (equal (car llm-pi-ai-model-thinking-levels) "off"))
  (should (equal llm-pi-ai-model-input-modalities '("text" "image")))
  (should (equal llm-pi-ai-cache-retentions '("none" "short" "long")))
  (should (equal llm-pi-ai-model-prompt-cache-retentions '("short" "long"))))

(provide 'llm-pi-ai-types-test)
;;; llm-pi-ai-types-test.el ends here
