;;; tool-error-type-regression.el --- a broken tool call is repaired without streaming -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Without streaming, llm.el parses a reply's tool-call arguments inside the
;; request's success callback.  Arguments that are not valid JSON raise
;; `llm-tool-call-error' there.  `superchat--llm-request-plz-propagate-callback-errors'
;; routes the condition to the request's `:on-error', and llm.el's `:on-error'
;; reports every error as `error' with its message, so the type is gone by the
;; time Superchat's error callback runs.  The repair for a broken tool call
;; (`superchat-llm-unknown-tool-retries') looks for an `llm-tool-*' type, so
;; it never ran without streaming: in the ARC run
;; superchat-vc33-think3-20261029-010010 (request 51) one broken call ended
;; the execution.  Postmortem: docs/postmortems/0001-a-broken-tool-call-lost-its-type.md.
;;
;; The first case runs llm.el's own non-streaming path for a DeepSeek
;; provider on that reply, with only plz's request replaced; the others run a
;; whole chat through the scripted network.
;;
;; Run from the repository root, with an existing prepared dependency directory:
;;
;;   SUPERCHAT_TEST_ELPA=/path/to/prepared/elpa
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$SUPERCHAT_TEST_ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega -L scripts \
;;     -l scripts/tool-error-type-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^tool-error-type/")'

;;; Code:

(load (expand-file-name "tool-hooks-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(require 'llm-deepseek)
(require 'plz-media-type)

(defvar superchat-llm-streaming)
(defvar superchat--llm-callback-error)
(defvar superchat--llm-unparsed-stop-reason)
(defvar llm-warn-on-nonfree)
(defvar hm--wires)

(defconst tet--broken-arguments
  "{\"progn\n  (defun arcg (cmd)\n    (let* ((out (shell-command-to-string (concat arc-bin \""
  "The tool-call arguments of request 51 in superchat-vc33-think3-20261029-010010.")

(defconst tet--message
  "An error occurred when calling an LLM tool: \"Invalid JSON in tool call arguments: could not parse JSON stream: 2, nil, 8\""
  "What Superchat's error callback received for that reply.")

(defun tet--reply-body ()
  "DeepSeek's reply to that request, shortened, as plz decodes it for llm.el.
`plz-media-type:application/json' keeps its default slots in llm.el 0.32.1:
objects as alists, arrays as vectors, false as `:json-false', null as nil."
  (json-parse-string
   (json-serialize
    `((id . "tet") (object . "chat.completion") (created . 1)
      (model . "deepseek-v4-flash")
      (choices . [((index . 0)
                   (message . ((role . "assistant") (content . "")
                               (reasoning_content . "r")
                               (tool_calls
                                . [((index . 0) (id . "call_00_tet") (type . "function")
                                    (function . ((name . "eval_elisp")
                                                 (arguments . ,tet--broken-arguments))))])))
                   (finish_reason . "tool_calls"))])
      (usage . ((prompt_tokens . 1) (completion_tokens . 1) (total_tokens . 2)))))
   :object-type 'alist :array-type 'array :false-object :json-false :null-object nil))

(defun tet--broken-step (&optional raised)
  "A scripted reply whose arguments did not parse, as Superchat receives it.
RAISED, when non-nil, is the condition the success callback raised."
  (append (list :unparsed-stop-reason 'tool-calls
                :error (cons 'error tet--message))
          (when raised (list :raised raised))))

(defconst tet--raised
  '(llm-tool-call-error
    "Invalid JSON in tool call arguments: could not parse JSON stream: 2, nil, 8"))

(defconst tet--repair "not complete valid JSON"
  "Part of the correction Superchat sends for `llm-tool-call-error'.")

(ert-deftest tool-error-type/1-llm-el-reports-the-parse-error-as-error-and-superchat-keeps-the-type ()
  "llm.el 0.32.1's non-streaming path, with only plz's request replaced."
  (let ((provider (make-llm-deepseek :key "tet" :chat-model "deepseek-v4-flash"))
        (prompt (llm-make-chat-prompt "go"))
        (llm-warn-on-nonfree nil)
        (superchat--llm-unparsed-stop-reason nil)
        got)
    (cl-letf (((symbol-function 'plz-media-type-request)
               (lambda (_method _url &rest args)
                 (funcall (plist-get args :then)
                          (make-plz-response :status 200 :body (tet--reply-body)))
                 nil)))
      (llm-chat-async provider prompt
                      (lambda (_response) (setq got '(success)))
                      (lambda (type message)
                        (setq got (list type message
                                        (bound-and-true-p superchat--llm-callback-error))))))
    (setq superchat--llm-unparsed-stop-reason nil)
    ;; llm.el's `:on-error' drops the type ...
    (should (eq 'error (nth 0 got)))
    (should (string-match-p "Invalid JSON in tool call arguments" (nth 1 got)))
    ;; ... and Superchat holds the condition while the callback runs.
    (should (eq 'llm-tool-call-error (car-safe (nth 2 got))))
    (should-not superchat--llm-callback-error)))

(ert-deftest tool-error-type/2-without-streaming-a-broken-call-is-repaired ()
  (let ((superchat-llm-streaming nil))
    (th--run (list (tet--broken-step tet--raised) '(:text "TET-DONE")) nil
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 2 (length smoke--requests)))
      (should (string-match-p tet--repair (nth 1 (th--prompts))))
      ;; The correction follows the user's message: nothing dangles before it.
      (let* ((messages (car hm--wires))
             (last (car (last messages)))
             (before (car (last messages 2))))
        (should (equal "user" (alist-get 'role last)))
        (should (string-match-p tet--repair (alist-get 'content last)))
        (should-not (alist-get 'tool_calls before)))
      (should-not superchat--llm-unparsed-stop-reason))))

(ert-deftest tool-error-type/3-the-repair-stops-after-its-limit ()
  (let ((superchat-llm-streaming nil))
    (th--run (list (tet--broken-step tet--raised) (tet--broken-step tet--raised)
                   (tet--broken-step tet--raised) '(:text "TET-NEVER"))
        nil
      (should (= 3 (length smoke--requests)))
      (should-not (eq 'completed (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest tool-error-type/4-an-error-that-did-not-come-from-a-tool-call-is-not-repaired ()
  (let ((superchat-llm-streaming nil))
    ;; A plain error, and a raised condition that is not a tool-call mistake.
    (dolist (raised (list nil '(llm-error "Something else")))
      (th--run (list (append (list :error (cons 'error "Something else"))
                             (when raised (list :raised raised)))
                     '(:text "TET-NEVER"))
          nil
        (should (= 1 (length smoke--requests)))
        (should-not (eq 'completed (plist-get (superchat-runtime-status run) :status)))))))

(ert-deftest tool-error-type/5-a-cut-off-reply-is-still-said-to-be-cut-off ()
  "The type comes back before the cut-off check, which takes either."
  (let ((superchat-llm-streaming nil))
    (th--run (list (list :unparsed-stop-reason 'length
                         :raised tet--raised
                         :error (cons 'error tet--message))
                   '(:text "TET-DONE"))
        nil
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (let ((second (nth 1 (th--prompts))))
        (should (string-match-p "Your reply was cut off at the output limit" second))
        (should-not (string-match-p tet--repair second))))))

(provide 'tool-error-type-regression)

;;; tool-error-type-regression.el ends here
