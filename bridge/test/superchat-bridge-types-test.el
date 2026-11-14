;;; superchat-bridge-types-test.el --- Tests for superchat-bridge-types  -*- lexical-binding: t; -*-

;; Tests for Tier 0: the type structs mirroring src/types.ts and
;; src/providers/model-schema.ts.

;;; Code:

(require 'ert)
(require 'superchat-bridge-types)
(require 'cl-lib)

;;; Content blocks

(ert-deftest superchat-bridge-types-test-text-content ()
  "A text content block carries its type and text."
  (let ((block (superchat-bridge-make-text-content :text "hi")))
    (should (superchat-bridge-text-content-p block))
    (should (superchat-bridge-content-p block))
    (should (equal (superchat-bridge-text-content-type block) "text"))
    (should (equal (superchat-bridge-text-content-text block) "hi"))))

(ert-deftest superchat-bridge-types-test-thinking-content ()
  "A thinking block defaults `redacted' to nil and types as thinking."
  (let ((block (superchat-bridge-make-thinking-content :thinking "why")))
    (should (equal (superchat-bridge-thinking-content-type block) "thinking"))
    (should (equal (superchat-bridge-thinking-content-thinking block) "why"))
    (should-not (superchat-bridge-thinking-content-redacted block))))

(ert-deftest superchat-bridge-types-test-image-content ()
  "An image block keeps its data and media type."
  (let ((block (superchat-bridge-make-image-content :data "AAA" :mime-type "image/png")))
    (should (equal (superchat-bridge-image-content-type block) "image"))
    (should (equal (superchat-bridge-image-content-mime-type block) "image/png"))))

(ert-deftest superchat-bridge-types-test-tool-call ()
  "A tool call has type \"toolCall\" and its arguments error unset."
  (let ((call (superchat-bridge-make-tool-call :id "c1" :name "read"
                                        :arguments '((:path . "a.txt")))))
    (should (superchat-bridge-tool-call-p call))
    (should (superchat-bridge-content-p call))
    (should (equal (superchat-bridge-tool-call-type call) "toolCall"))
    (should (equal (superchat-bridge-tool-call-name call) "read"))
    (should-not (superchat-bridge-tool-call-arguments-error call))))

(ert-deftest superchat-bridge-types-test-content-p-negative ()
  "A message is not a content block."
  (should-not (superchat-bridge-content-p (superchat-bridge-make-user-message)))
  (should-not (superchat-bridge-content-p nil)))

;;; Messages

(ert-deftest superchat-bridge-types-test-message-roles ()
  "Each constructor fixes the role the union type declares."
  (should (equal (superchat-bridge-message-role (superchat-bridge-make-system-message)) "system"))
  (should (equal (superchat-bridge-message-role (superchat-bridge-make-user-message)) "user"))
  (should (equal (superchat-bridge-message-role (superchat-bridge-make-assistant-message)) "assistant"))
  (should (equal (superchat-bridge-message-role (superchat-bridge-make-tool-result-message))
                 "toolResult")))

(ert-deftest superchat-bridge-types-test-message-predicates ()
  "`superchat-bridge-message-p' accepts all four message types and nothing else."
  (dolist (message (list (superchat-bridge-make-system-message)
                         (superchat-bridge-make-user-message)
                         (superchat-bridge-make-assistant-message)
                         (superchat-bridge-make-tool-result-message)))
    (should (superchat-bridge-message-p message)))
  (should (superchat-bridge-system-message-p (superchat-bridge-make-system-message)))
  (should (superchat-bridge-user-message-p (superchat-bridge-make-user-message)))
  (should (superchat-bridge-assistant-message-p (superchat-bridge-make-assistant-message)))
  (should (superchat-bridge-tool-result-message-p (superchat-bridge-make-tool-result-message)))
  (should-not (superchat-bridge-message-p (superchat-bridge-make-text-content)))
  (should-not (superchat-bridge-message-p nil)))

(ert-deftest superchat-bridge-types-test-message-fields ()
  "Constructor keyword arguments land in the right slots."
  (let ((user (superchat-bridge-make-user-message
               :content (list (superchat-bridge-make-text-content :text "hello"))
               :timestamp 42)))
    (should (equal (superchat-bridge-message-role user) "user"))
    (should (equal (superchat-bridge-message-timestamp user) 42))
    (should (superchat-bridge-text-content-p (car (superchat-bridge-message-content user)))))
  (let ((assistant (superchat-bridge-make-assistant-message
                    :api "openai-completions" :model "gpt-4o"
                    :stop-reason "stop")))
    (should (equal (superchat-bridge-message-role assistant) "assistant"))
    (should (equal (superchat-bridge-assistant-message-api assistant) "openai-completions"))
    (should (equal (superchat-bridge-assistant-message-stop-reason assistant) "stop"))))

(ert-deftest superchat-bridge-types-test-system-message-sections ()
  "A later system message can add tools and name prompt sections."
  (let ((message (superchat-bridge-make-system-message
                  :sections '(("style" . "terse"))
                  :tools-added '("read") :tools-removed '("write"))))
    (should (equal (superchat-bridge-system-message-sections message)
                   '(("style" . "terse"))))
    (should (equal (superchat-bridge-system-message-tools-added message) '("read")))
    (should (equal (superchat-bridge-system-message-tools-removed message) '("write")))))

(ert-deftest superchat-bridge-types-test-tool-result-message ()
  "A tool result message keeps its call id and error flag."
  (let ((message (superchat-bridge-make-tool-result-message
                  :tool-call-id "c1" :tool-name "read"
                  :content (list (superchat-bridge-make-text-content :text "ok"))
                  :is-error nil)))
    (should (equal (superchat-bridge-tool-result-message-tool-call-id message) "c1"))
    (should (equal (superchat-bridge-tool-result-message-role message) "toolResult"))
    (should-not (superchat-bridge-tool-result-message-is-error message))))

(ert-deftest superchat-bridge-types-test-message-copier ()
  "Copying a message copies the struct together with the role."
  (let* ((user (superchat-bridge-make-user-message :timestamp 1))
         (copy (superchat-bridge-copy-user-message user)))
    (should-not (eq copy user))
    (should (superchat-bridge-user-message-p copy))
    (should (equal (superchat-bridge-message-role copy) "user"))
    (setf (superchat-bridge-message-timestamp copy) 2)
    (should (equal (superchat-bridge-message-timestamp user) 1))))

;;; Struct definition and re-evaluation

(ert-deftest superchat-bridge-types-test-structs-reevaluate ()
  "Re-evaluating the library keeps every constructor working.
`cl-defstruct' writes its constructor as a lambda that only accepts
the keywords it saw; defining a struct must therefore take the old
function away first, or the new one keeps the old arity."
  (superchat-bridge-make-user-message :timestamp 1)
  (superchat-bridge-make-assistant-message :api "x")
  (superchat-bridge-make-tool-result-message :tool-call-id "c")
  (superchat-bridge-make-image-model :id "i")
  (superchat-bridge-make-classifier-model :id "k")
  (load (locate-library "superchat-bridge-types") nil t)
  (should (equal (superchat-bridge-message-role
                  (superchat-bridge-make-user-message :timestamp 2))
                 "user"))
  (should (equal (superchat-bridge-assistant-message-api
                  (superchat-bridge-make-assistant-message :api "openai-completions"))
                 "openai-completions"))
  (should (equal (superchat-bridge-tool-result-message-tool-call-id
                  (superchat-bridge-make-tool-result-message :tool-call-id "c2"))
                 "c2"))
  (should (eq (superchat-bridge-get-model-type (superchat-bridge-make-image-model))
              'image))
  (should (eq (superchat-bridge-get-model-type (superchat-bridge-make-classifier-model))
              'classifier)))

;;; Usage and cost

(ert-deftest superchat-bridge-types-test-cost-defaults ()
  "Cost slots are numeric and default to zero."
  (let ((cost (superchat-bridge-make-cost)))
    (should (equal (superchat-bridge-cost-input cost) 0))
    (should (equal (superchat-bridge-cost-cache-write cost) 0))))

;;; Models

(ert-deftest superchat-bridge-types-test-model-type ()
  "A plain model is a chat model; sub-models report their own type."
  (let ((chat (superchat-bridge-make-model :id "m" :name "M" :api "openai-completions")))
    (should (eq (superchat-bridge-get-model-type chat) 'chat))
    (should (superchat-bridge-is-model-type chat 'chat)))
  (let ((image (superchat-bridge-make-image-model :id "img")))
    (should (eq (superchat-bridge-get-model-type image) 'image))
    (should (superchat-bridge-model-p image))
    (should (equal (superchat-bridge-model-id image) "img")))
  (let ((classifier (superchat-bridge-make-classifier-model :id "cls")))
    (should (eq (superchat-bridge-get-model-type classifier) 'classifier))
    (should-not (superchat-bridge-is-model-type classifier 'chat))))

(ert-deftest superchat-bridge-types-test-image-model-output ()
  "An image model keeps the media it can produce."
  (let ((image (superchat-bridge-make-image-model :output '("image" "text"))))
    (should (equal (superchat-bridge-image-model-output image) '("image" "text")))))

(ert-deftest superchat-bridge-types-test-model-input-limits ()
  "Input limits hold request size and image limits."
  (let ((limits (superchat-bridge-make-model-input-limits :max-request-bytes 1024)))
    (should (equal (superchat-bridge-model-input-limits-max-request-bytes limits) 1024))))

;;; Events

(ert-deftest superchat-bridge-types-test-event ()
  "An event records its type and the fields that type carries."
  (let ((event (superchat-bridge-make-event :type "text_delta" :content-index 0 :delta "he")))
    (should (equal (superchat-bridge-assistant-message-event-type event) "text_delta"))
    (should (equal (superchat-bridge-assistant-message-event-delta event) "he"))
    (should-not (superchat-bridge-assistant-message-event-reason event))))

;;; Constants

(ert-deftest superchat-bridge-types-test-constants ()
  "The schema constants keep the order and membership of the source."
  (should (equal superchat-bridge-thinking-levels
                 '("minimal" "low" "medium" "high" "xhigh" "max")))
  (should (equal (car superchat-bridge-model-thinking-levels) "off"))
  (should (equal superchat-bridge-model-input-modalities '("text" "image")))
  (should (equal superchat-bridge-cache-retentions '("none" "short" "long")))
  (should (equal superchat-bridge-model-prompt-cache-retentions '("short" "long"))))

(provide 'superchat-bridge-types-test)
;;; superchat-bridge-types-test.el ends here
