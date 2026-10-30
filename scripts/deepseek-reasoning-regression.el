;;; deepseek-reasoning-regression.el --- DeepSeek reasoning is sent back -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; DeepSeek's thinking mode, on by default for its V4 models, answers 400
;; ("The `reasoning_content' in the thinking mode must be passed back to the
;; API") when an assistant message after the last user message lacks
;; `reasoning_content'.  llm.el 0.32.1 never sends it, so every DeepSeek
;; execution that called a tool failed at its second request.  superchat-llm.el
;; keeps each reply's reasoning on the interactions llm.el appends for it and
;; sends it back (decision:
;; docs/decisions/accepted/2026-10-28-deepseek-reasoning-is-sent-back.md).
;;
;; Every case records a reply through llm.el's own
;; `llm-provider-utils-process-result' and serializes the next request through
;; `llm-provider-chat-request', as the real adapter does; nothing reaches a
;; network.
;;
;; Run from the repository root, with an existing prepared dependency directory:
;;
;;   SUPERCHAT_TEST_ELPA=/path/to/prepared/elpa
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$SUPERCHAT_TEST_ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/deepseek-reasoning-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^deepseek-reasoning/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'llm-openai)
(require 'llm-deepseek)

(when (getenv "SMOKE_REAL")
  (error "DeepSeek reasoning regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst dsr--reasoning "The board needs a look before any move."
  "The reasoning of the scripted reply.")

(defun dsr--provider ()
  (make-llm-deepseek :key "unused" :chat-model "deepseek-v4-flash"))

(defun dsr--compatible ()
  (make-llm-openai-compatible :url "http://localhost:1234/v1/"
                              :chat-model "deepseek-v4-flash"))

(defun dsr--prompt (reasoning)
  "A prompt with one tool, asking at REASONING."
  (llm-make-chat-prompt
   "What is on the board?"
   :reasoning reasoning
   :tools (list (llm-make-tool :name "look" :description "Look at the board."
                               :function (lambda () "an empty board")
                               :args nil))))

(defun dsr--record-reply (provider prompt &optional text)
  "Record a reply to PROMPT from PROVIDER: TEXT (default \"Let me look.\"),
the scripted reasoning and one call of `look', run through llm.el."
  (let (done)
    (llm-provider-utils-process-result
     provider prompt
     (append (unless (eq text 'none) (list :text (or text "Let me look.")))
             (list :reasoning dsr--reasoning
                   :tool-uses (list (make-llm-provider-utils-tool-use
                                     :id "call_1" :name "look"
                                     :args (make-hash-table :test #'equal)))))
     t
     (lambda (_) (setq done t))
     (lambda (type message) (error "Tool use failed: %s %s" type message)))
    (should done)
    prompt))

(defun dsr--wire (provider prompt)
  "The messages of the request PROVIDER would send for PROMPT, as JSON alists."
  (superchat--llm-prepare-reasoning provider prompt)
  (alist-get 'messages
             (json-parse-string
              (llm-provider-utils-json-serialize
               (llm-provider-chat-request provider prompt nil))
              :object-type 'alist :array-type 'list
              :null-object nil :false-object :false)))

(defun dsr--assistants (messages)
  (cl-remove-if-not (lambda (m) (equal (alist-get 'role m) "assistant")) messages))

(defun dsr--kept (prompt)
  "The reasoning each assistant interaction of PROMPT keeps."
  (mapcar (lambda (i) (plist-get (llm-chat-prompt-interaction-multi-turn-plist i)
                                 :deepseek-reasoning-content))
          (cl-remove-if-not (lambda (i) (eq (llm-chat-prompt-interaction-role i) 'assistant))
                            (llm-chat-prompt-interactions prompt))))

(ert-deftest deepseek-reasoning/a-reply-keeps-its-reasoning-on-its-interactions ()
  "llm.el appends the reply's text and its call as two assistant
interactions: the first keeps the reasoning, the second \"\"."
  (let ((prompt (dsr--record-reply (dsr--provider) (dsr--prompt 'maximum))))
    (should (equal (dsr--kept prompt) (list dsr--reasoning "")))))

(ert-deftest deepseek-reasoning/the-next-request-sends-the-reasoning-back ()
  (let* ((provider (dsr--provider))
         (messages (dsr--wire provider (dsr--record-reply provider (dsr--prompt 'maximum))))
         (assistants (dsr--assistants messages)))
    (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                   '("user" "assistant" "assistant" "tool")))
    (should (equal (mapcar (lambda (m) (alist-get 'reasoning_content m)) assistants)
                   (list dsr--reasoning "")))
    (should (equal (alist-get 'content (car assistants)) "Let me look."))
    (should (alist-get 'tool_calls (cadr assistants)))))

(ert-deftest deepseek-reasoning/the-provider-default-sends-it-too ()
  "No reasoning setting sends no `thinking' field, and DeepSeek then thinks."
  (let* ((provider (dsr--provider))
         (messages (dsr--wire provider (dsr--record-reply provider (dsr--prompt nil)))))
    (should (equal (mapcar (lambda (m) (alist-get 'reasoning_content m))
                           (dsr--assistants messages))
                   (list dsr--reasoning "")))))

(ert-deftest deepseek-reasoning/a-reply-without-text-puts-it-on-its-calls ()
  (let* ((provider (dsr--provider))
         (prompt (dsr--record-reply provider (dsr--prompt 'maximum) 'none)))
    (should (equal (dsr--kept prompt) (list dsr--reasoning)))
    (should (equal (mapcar (lambda (m) (alist-get 'reasoning_content m))
                           (dsr--assistants (dsr--wire provider prompt)))
                   (list dsr--reasoning)))))

(ert-deftest deepseek-reasoning/thinking-turned-off-sends-no-field ()
  (let* ((provider (dsr--provider))
         (messages (dsr--wire provider (dsr--record-reply provider (dsr--prompt 'none)))))
    (should (= 2 (length (dsr--assistants messages))))
    (dolist (message messages)
      (should-not (assq 'reasoning_content message)))))

(ert-deftest deepseek-reasoning/another-provider-is-left-as-it-was ()
  (let* ((provider (dsr--compatible))
         (prompt (dsr--record-reply provider (dsr--prompt nil))))
    (should (equal (dsr--kept prompt) '(nil nil)))
    (dolist (message (dsr--wire provider prompt))
      (should-not (assq 'reasoning_content message)))))

(ert-deftest deepseek-reasoning/an-earlier-turn-is-left-alone ()
  "An assistant message before the last user message keeps no reasoning and
gets none: DeepSeek accepts that, and only the current turn needs the field."
  (let* ((provider (dsr--provider))
         (prompt (dsr--prompt 'maximum)))
    (superchat--llm-set-history
     prompt (list (list :kind "user" :content "Hello")
                  (list :kind "assistant" :content "Hello to you.")))
    (dsr--record-reply provider prompt)
    (let ((messages (dsr--wire provider prompt)))
      (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                     '("user" "assistant" "user" "assistant" "assistant" "tool")))
      (should (equal (mapcar (lambda (m) (alist-get 'reasoning_content m))
                             (dsr--assistants messages))
                     (list nil dsr--reasoning "")))
      (should-not (assq 'reasoning_content (nth 1 messages))))))

(ert-deftest deepseek-reasoning/history-replaced-mid-run-keeps-the-current-reasoning ()
  "Compaction replaces the earlier turns by identity and keeps the Run's own."
  (let* ((provider (dsr--provider))
         (prompt (dsr--record-reply provider (dsr--prompt 'maximum))))
    (superchat--llm-set-history
     prompt (list (list :kind "user" :content "Summary of before")
                  (list :kind "assistant" :content "Noted.")))
    (should (equal (mapcar (lambda (m) (alist-get 'reasoning_content m))
                           (dsr--assistants (dsr--wire provider prompt)))
                   (list nil dsr--reasoning "")))))

(ert-deftest deepseek-reasoning/a-dropped-dangling-call-keeps-the-messages-aligned ()
  "After llm.el's unanswered call is dropped, the reasoning still lands on the
message of the interaction that kept it."
  (let* ((provider (dsr--provider))
         (prompt (dsr--prompt 'maximum)))
    (let ((superchat--llm-deepseek-reply-reasoning (cons prompt dsr--reasoning)))
      (llm-provider-utils-append-to-prompt prompt "Let me look.")
      (llm-provider-utils-append-to-prompt
       prompt (list (make-llm-provider-utils-tool-use
                     :id "call_9" :name "unknown" :args (make-hash-table)))
       nil nil 'assistant))
    (should (superchat--llm-drop-dangling-tool-use prompt))
    (let ((messages (dsr--wire provider prompt)))
      (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                     '("user" "assistant")))
      (should (equal (alist-get 'reasoning_content (nth 1 messages)) dsr--reasoning)))))

(provide 'deepseek-reasoning-regression)

;;; deepseek-reasoning-regression.el ends here
