;;; history-messages-regression.el --- earlier turns as real messages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "earlier turns reach the model as real
;; messages" (docs/decisions/implemented/2026-09-25-earlier-turns-as-real-tool-messages.md;
;; context contract, amendment 2026-09-25).  An earlier turn reaches the model
;; as its user message, one assistant message per Step with that Step's tool
;; calls, one tool message per call, and its final assistant text -- not as a
;; `User:'/`Tool:'/`Assistant:' transcript inside the new user message -- and
;; the input goes as typed.
;;
;; Every provider request is captured at the llm boundary and serialized with
;; llm.el's own OpenAI-compatible request builder, the form DeepSeek receives,
;; so the assertions are about the wire messages, not superchat's internals.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets, and needs no scratch
;; files.  All model calls are scripted.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/history-messages-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^history-messages/")'

;;; Code:

;; 620, not 400: read_buffer's result carries a header since
;; 2026-09-26, so its Tool line grew from about 70 to about 140 characters;
;; 620 keeps the same recent entries raw that 400 kept before.

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)

(when (getenv "SMOKE_REAL")
  (error "History-messages regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-compact)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar hm--wires nil
  "Provider requests of the current test, newest first, as wire messages.")

(defun hm--wire-of (provider prompt)
  "Return PROMPT as the message list llm.el sends to an OpenAI-style PROVIDER.
Serialized and parsed back, so it is plain data taken at request time."
  (let* ((request (llm-provider-chat-request provider prompt nil))
         (messages (or (plist-get request :messages)
                       (alist-get 'messages request))))
    (json-parse-string (json-encode messages)
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defvar hm--extra-tool-names nil
  "Tools a fixture offers besides the default surface.
`open_file' left the default surface on 2026-10-27; a test whose scripted
model opens a file names it here.")

(defmacro hm--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical fixture, capturing wire messages."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names (append superchat-llm-default-tool-names
                                           hm--extra-tool-names))
         (superchat-llm-tools-list nil)
         (hm--wires nil)
         (target (get-buffer-create "hm-target")))
     (with-current-buffer target
       (erase-buffer)
       (insert "HM-TARGET-TEXT\n"))
     (unwind-protect
         (smoke--with-fixture ,script
           (let ((network (symbol-function 'smoke--fake-network)))
             (cl-letf (((symbol-function 'smoke--fake-network)
                        (lambda (mode provider prompt partial response error multi)
                          (push (hm--wire-of provider prompt) hm--wires)
                          (funcall network mode provider prompt
                                   partial response error multi))))
               ,@body)))
       (setq superchat-llm-tools-list nil)
       (when (buffer-live-p target) (kill-buffer target)))))

(defun hm--turn (view text)
  "Send TEXT from VIEW and wait until its Run completed; return the Run id."
  (let ((run (smoke--send view text)))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    run))

(defun hm--wire (n)
  "The Nth provider request of this test, counting from 1."
  (let ((wires (reverse hm--wires)))
    (should (>= (length wires) n))
    (nth (1- n) wires)))

(defun hm--body (wire)
  "WIRE without its leading system message."
  (if (equal "system" (alist-get 'role (car wire))) (cdr wire) wire))

(defun hm--roles (wire)
  (mapcar (lambda (message) (alist-get 'role message)) (hm--body wire)))

(defun hm--text (message)
  "MESSAGE's content as a string (empty when it has none)."
  (let ((content (alist-get 'content message)))
    (if (stringp content) content "")))

(defun hm--calls (message)
  (alist-get 'tool_calls message))

(defun hm--call-arguments (call)
  (alist-get 'arguments (alist-get 'function call)))

(defun hm--session (view)
  (buffer-local-value 'superchat--session-id view))

(defun hm--events (session type)
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events session)))

(defun hm--read-round (buffer)
  `(:tool-uses (("read_buffer" (buffer . ,buffer)))))

;;;; A first turn has nothing earlier

(ert-deftest history-messages/a-first-turn-has-no-earlier-messages ()
  "Guard (green before and after): the first request of a Session carries
the system text and one user message."
  (hm--fixture (list '(:text "ONLY-ANSWER"))
    (hm--turn (smoke--open-chat) "HM-ONLY question")
    (should (equal '("user") (hm--roles (hm--wire 1))))))

;;;; An earlier turn with a tool call

(defun hm--two-turns (&optional second-script)
  "Run a turn that reads hm-target and answers, then the question HM-SECOND.
SECOND-SCRIPT is the second turn's model script (default: one answer).
Return the chat View."
  (setq smoke--script
        (append (list (hm--read-round "hm-target") '(:text "TURN-ONE-ANSWER"))
                (or second-script (list '(:text "TURN-TWO-ANSWER")))))
  (let ((view (smoke--open-chat)))
    (hm--turn view "HM-FIRST look at hm-target")
    (hm--turn view "HM-SECOND question")
    view))

(ert-deftest history-messages/an-earlier-turn-arrives-as-messages ()
  "The second turn's request carries the first turn as messages: its user
message, an assistant message with the read_buffer call, the tool message with
what it returned, and the final answer -- then the new question."
  (hm--fixture nil
    (hm--two-turns)
    (let ((body (hm--body (hm--wire 3))))
      (should (equal '("user" "assistant" "tool" "assistant" "user")
                     (hm--roles (hm--wire 3))))
      (should (string-match-p "HM-FIRST" (hm--text (nth 0 body))))
      (should (equal "read_buffer"
                     (alist-get 'name (alist-get 'function (car (hm--calls (nth 1 body)))))))
      (should (string-match-p "HM-TARGET-TEXT" (hm--text (nth 2 body))))
      (should (string-match-p "TURN-ONE-ANSWER" (hm--text (nth 3 body))))
      (should (string-match-p "HM-SECOND" (hm--text (nth 4 body)))))))

(ert-deftest history-messages/calls-of-one-step-share-one-assistant-message ()
  "Two calls the model made in one reply are one assistant message with both
calls, then one tool message each; the next reply's call is its own message."
  (hm--fixture (list '(:tool-uses (("read_buffer" (buffer . "hm-target"))
                                   ("read_buffer" (buffer . "hm-target"))))
                     (hm--read-round "hm-target")
                     '(:text "TURN-ONE-ANSWER")
                     '(:text "TURN-TWO-ANSWER"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "HM-FIRST read twice")
      (hm--turn view "HM-SECOND question"))
    (let* ((wire (car hm--wires))
           (body (hm--body wire)))
      (should (equal '("user" "assistant" "tool" "tool" "assistant" "tool"
                       "assistant" "user")
                     (hm--roles wire)))
      (should (= 2 (length (hm--calls (nth 1 body)))))
      (should (= 1 (length (hm--calls (nth 4 body)))))
      (should (equal (mapcar (lambda (call) (alist-get 'id call))
                             (hm--calls (nth 1 body)))
                     (list (alist-get 'tool_call_id (nth 2 body))
                           (alist-get 'tool_call_id (nth 3 body))))))))

(ert-deftest history-messages/no-transcript-text-and-no-header ()
  "No message of the second turn's request holds the old transcript: no
Context header, no role-labelled lines, no tool line, no tool note."
  (hm--fixture nil
    (hm--two-turns)
    (let ((texts (mapcar #'hm--text (hm--wire 3))))
      (should texts)
      (dolist (needle '("[Context v0" "User: HM-FIRST" "Assistant: TURN-ONE-ANSWER"
                        "Tool: read_buffer(" "(Tool lines are past observations"))
        (should-not (cl-some (lambda (text) (string-match-p (regexp-quote needle) text))
                             texts))))))

(ert-deftest history-messages/call-ids-are-the-execution-ids ()
  "The earlier call's wire id is its Tape execution id, on the call and on the
tool message that answers it."
  (hm--fixture nil
    (let* ((view (hm--two-turns))
           (requested (car (hm--events (hm--session view) "tool.requested")))
           (execution-id (plist-get (plist-get requested :payload) :execution-id))
           (body (hm--body (hm--wire 3))))
      (should (stringp execution-id))
      (should (equal "assistant" (alist-get 'role (nth 1 body))))
      (should (equal execution-id (alist-get 'id (car (hm--calls (nth 1 body))))))
      (should (equal execution-id (alist-get 'tool_call_id (nth 2 body)))))))

(ert-deftest history-messages/call-arguments-are-named ()
  "The earlier call carries its arguments as an object keyed by parameter."
  (hm--fixture nil
    (hm--two-turns)
    (let* ((body (hm--body (hm--wire 3)))
           (call (car (hm--calls (nth 1 body)))))
      (should call)
      (should (equal '((buffer . "hm-target"))
                     (json-parse-string (hm--call-arguments call) :object-type 'alist))))))

(ert-deftest history-messages/new-calls-store-named-arguments ()
  "A tool.requested row keeps its arguments as a JSON object keyed by the
tool's parameter names."
  (hm--fixture (list (hm--read-round "hm-target") '(:text "DONE"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "HM-FIRST look at hm-target")
      (let* ((requested (car (hm--events (hm--session view) "tool.requested")))
             (text (plist-get (plist-get requested :payload) :arguments-text)))
        (should (stringp text))
        (should (equal '((buffer . "hm-target"))
                       (json-parse-string text :object-type 'alist :array-type 'list)))))))

(ert-deftest history-messages/the-input-goes-as-typed ()
  "The user message carries the input, not the old general-answer wrapper."
  (hm--fixture (list '(:text "ONLY-ANSWER"))
    (hm--turn (smoke--open-chat) "HM-ONLY question")
    (let ((user (car (last (hm--body (hm--wire 1))))))
      (should (string-match-p "HM-ONLY question" (hm--text user)))
      (should-not (string-match-p "You are a helpful assistant" (hm--text user))))))

(ert-deftest history-messages/cut-arguments-go-empty-with-a-note ()
  "An earlier call whose stored arguments were cut is sent with empty
arguments, and its tool message says what was kept."
  (let ((long (make-string 2500 ?Q)))
    (hm--fixture (list (hm--read-round long) '(:text "TURN-ONE-ANSWER")
                       '(:text "TURN-TWO-ANSWER"))
      (let ((view (smoke--open-chat)))
        (hm--turn view "HM-FIRST read a long name")
        (hm--turn view "HM-SECOND question")
        (let* ((body (hm--body (hm--wire 3)))
               (assistant (cl-find-if #'hm--calls body))
               (tool (cl-find "tool" body :key (lambda (m) (alist-get 'role m))
                              :test #'equal)))
          (should assistant)
          (should (equal "{}" (hm--call-arguments (car (hm--calls assistant)))))
          (should tool)
          (should (string-match-p (regexp-quote "(Arguments not kept in full:")
                                  (hm--text tool)))
          (should (string-match-p (regexp-quote "[cut: ") (hm--text tool))))))))

(ert-deftest history-messages/a-small-budget-keeps-calls-with-their-results ()
  "With a budget that leaves older items out, every earlier call in the
request is answered by its tool message, and every tool message by its call.
Two reads, so the budget can leave one out after the turn's question takes
its place (2026-10-22: kept calls keep their question)."
  (hm--fixture (list (hm--read-round "hm-target") (hm--read-round "hm-target")
                     '(:text "TURN-ONE-ANSWER")
                     '(:text "TURN-TWO-ANSWER"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "HM-FIRST look at hm-target")
      (let ((superchat-context-max-chars 260))
        (hm--turn view "HM-SECOND question")))
    (let* ((body (hm--body (car hm--wires)))
           (call-ids (cl-loop for message in body
                              append (mapcar (lambda (call) (alist-get 'id call))
                                             (hm--calls message))))
           (answered (cl-loop for message in body
                              when (equal "tool" (alist-get 'role message))
                              collect (alist-get 'tool_call_id message))))
      (should (= 1 (length call-ids)))
      (should (equal call-ids answered)))))

(ert-deftest history-messages/kept-calls-keep-the-question-they-served ()
  "A budget that keeps only a turn's last calls still keeps that turn's
question, ahead of them; the calls in between go instead.  In the user's
Session of 2026-10-22 the history kept 22 probes and dropped the question
they answered, so after \"go on\" the model probed on and never answered."
  (hm--fixture (list (hm--read-round "hm-target") (hm--read-round "hm-target")
                     (hm--read-round "hm-target") (hm--read-round "hm-target")
                     (hm--read-round "hm-target")
                     '(:text "TURN-ONE-ANSWER")
                     '(:text "TURN-TWO-ANSWER"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "HM-QUESTION compare the two libraries")
      (let ((superchat-context-max-chars 600))
        (hm--turn view "HM-GO-ON")))
    (let* ((wire (car hm--wires))
           (body (hm--body wire))
           (texts (mapcar #'hm--text body))
           (calls (cl-loop for message in body
                           append (mapcar (lambda (call) (alist-get 'id call))
                                          (hm--calls message))))
           (answered (cl-loop for message in body
                              when (equal "tool" (alist-get 'role message))
                              collect (alist-get 'tool_call_id message))))
      ;; The budget did leave calls out, and kept some.
      (should (< 0 (length calls) 5))
      (should (equal calls answered))
      ;; The question is there, first, and the new input is last.
      (should (equal "user" (alist-get 'role (car body))))
      (should (string-match-p "HM-QUESTION" (car texts)))
      (should (string-match-p "TURN-ONE-ANSWER" (mapconcat #'identity texts "\n")))
      (should (string-match-p "HM-GO-ON" (car (last texts)))))))

(ert-deftest history-messages/a-question-already-kept-is-not-repeated ()
  "Guard: a budget that holds the whole turn sends its question once."
  (hm--fixture (list (hm--read-round "hm-target") (hm--read-round "hm-target")
                     '(:text "TURN-ONE-ANSWER")
                     '(:text "TURN-TWO-ANSWER"))
    (let ((view (smoke--open-chat)))
      (hm--turn view "HM-QUESTION compare the two libraries")
      (hm--turn view "HM-GO-ON"))
    (let ((texts (mapcar #'hm--text (hm--body (car hm--wires)))))
      (should (= 1 (cl-count-if (lambda (text) (string-match-p "HM-QUESTION" text))
                                texts)))
      (should (= 2 (cl-count-if #'hm--calls (hm--body (car hm--wires))))))))

(ert-deftest history-messages/adjacent-requests-keep-their-prefix ()
  "Within the second turn's Run, the second request begins with every message
of the first, byte for byte, history included."
  (hm--fixture nil
    (hm--two-turns (list (hm--read-round "hm-target") '(:text "TURN-TWO-ANSWER")))
    (let ((first (hm--wire 3)) (second (hm--wire 4)))
      (should (cl-some #'hm--calls (hm--body first)))
      (should (> (length second) (length first)))
      (should (equal first (cl-subseq second 0 (length first)))))))

;;;; Material a Step inserted, and a compaction summary

(ert-deftest history-messages/injected-material-is-a-user-message ()
  "Material the first turn admitted reaches the second turn as its own user
message, framed as injected material."
  (hm--fixture (list '(:text "TURN-ONE-ANSWER") '(:text "TURN-TWO-ANSWER"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view)))
      (superchat-runtime-inject session '(:source (:kind test) :text "HM-INJECTED"))
      (hm--turn view "HM-FIRST question")
      (hm--turn view "HM-SECOND question")
      (let ((body (hm--body (hm--wire 2))))
        (should (cl-find "[Injected material] HM-INJECTED" body
                         :key #'hm--text :test #'equal))
        (should-not (cl-some (lambda (message)
                               (string-match-p "User: \\[Injected material\\]"
                                               (hm--text message)))
                             body))))))

(defun hm--compaction-script ()
  "Four turns (tool pairs in the first and fourth), a summary, one more turn."
  (list (hm--read-round "hm-target")
        '(:text "answer one") '(:text "answer two") '(:text "answer three")
        (hm--read-round "hm-target")
        '(:text "answer four")
        (list :text (concat "HM-SUMMARY\n" superchat-compact--completion-marker))
        '(:text "after compaction")))

(ert-deftest history-messages/a-summary-is-the-first-message ()
  "After a compaction, the next turn's request starts with the summary as a
user message that says it is a summary."
  (hm--fixture (hm--compaction-script)
    (let ((view (smoke--open-chat)))
      (let ((superchat-context-max-chars 620))
        (dolist (text '("ask one" "ask two" "ask three" "ask four"))
          (hm--turn view text))
        (let (done)
          (with-current-buffer view
            (superchat-compact-session (lambda (_ok _value) (setq done t))))
          (should (smoke--wait (lambda () done) 30))))
      (hm--turn view "HM-AFTER question")
      (let* ((wire (car hm--wires))
             (first (car (hm--body wire))))
        (should (equal "user" (alist-get 'role first)))
        (should (string-match-p "HM-SUMMARY" (hm--text first)))
        ;; Its own message: not the current question, not the other turns.
        (should-not (string-match-p "HM-AFTER" (hm--text first)))
        (should (string-match-p "summary" (downcase (hm--text first))))
        (should-not (cl-some (lambda (message)
                               (string-match-p (regexp-quote "[Context v0")
                                               (hm--text message)))
                             wire))))))

(ert-deftest history-messages/the-summarizer-gets-one-text-message ()
  "Compaction's own request keeps the text rendering: one user message holding
the span to summarize, tool lines included, and no earlier-turn messages."
  (hm--fixture (hm--compaction-script)
    (let ((view (smoke--open-chat))
          (superchat-context-max-chars 620))
      (dolist (text '("ask one" "ask two" "ask three" "ask four"))
        (hm--turn view text))
      (let (done)
        (with-current-buffer view
          (superchat-compact-session (lambda (_ok _value) (setq done t))))
        (should (smoke--wait (lambda () done) 30))))
    (let ((wire (car hm--wires)))
      (should (= 7 (length hm--wires)))
      (should (equal '("user") (hm--roles wire)))
      (should (string-match-p "ask one" (hm--text (car (hm--body wire)))))
      (should (string-match-p (regexp-quote "Tool: read_buffer(")
                              (hm--text (car (hm--body wire))))))))

;;;; Recovery after a request that is too long

(ert-deftest history-messages/a-recovered-request-carries-the-summary ()
  "When the provider refuses a request as too long, compaction resumes the Run:
the resumed request starts with the summary instead of the earlier turns it
replaced, still ends with the current question, and the Run completes.
\(That the question is sent once, and the system text kept, is pinned in
scripts/context-recovery-regression.el.)"
  (hm--fixture (append (butlast (hm--compaction-script) 2)
                       (list '(:error (llm-context-length-exceeded
                                       . "context length exceeded"))
                             (list :text (concat "HM-SUMMARY\n"
                                                 superchat-compact--completion-marker))
                             '(:text "after recovery")))
    (let ((view (smoke--open-chat))
          (superchat-context-max-chars 620))
      (dolist (text '("ask one" "ask two" "ask three" "ask four"))
        (hm--turn view text))
      (let ((run (smoke--send view "HM-FIVE question")))
        (should (stringp run))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))
      (let* ((refused (hm--body (nth 2 hm--wires)))
             (resumed (hm--body (car hm--wires)))
             (call-ids (cl-loop for message in resumed
                                append (mapcar (lambda (call) (alist-get 'id call))
                                               (hm--calls message)))))
        (should-not (string-match-p "HM-SUMMARY" (hm--text (car refused))))
        ;; The summary replaces the turns it covers; they are not kept beside it.
        (should (cl-find "ask one" refused :key #'hm--text :test #'equal))
        (should-not (cl-find "ask one" resumed :key #'hm--text :test #'equal))
        (should (equal "user" (alist-get 'role (car resumed))))
        (should (string-match-p "HM-SUMMARY" (hm--text (car resumed))))
        (should (equal "user" (alist-get 'role (car (last resumed)))))
        (should (string-match-p "HM-FIVE" (hm--text (car (last resumed)))))
        (should (equal call-ids
                       (cl-loop for message in resumed
                                when (equal "tool" (alist-get 'role message))
                                collect (alist-get 'tool_call_id message))))))))

(ert-deftest history-messages/a-retry-rewrite-leaves-earlier-messages-alone ()
  "A rewrite of the request before a later attempt replaces the current user
message, never an earlier user message with the same text."
  (let ((prompt (llm-make-chat-prompt "HM-SAME")))
    (superchat--llm-set-history
     prompt (list (list :kind "user" :content "HM-SAME")
                  (list :kind "assistant" :content "HM-EARLIER-ANSWER")))
    (should (superchat--llm-replace-prompt-input prompt "HM-SAME" "HM-NEW"))
    (should (equal '("HM-SAME" "HM-EARLIER-ANSWER" "HM-NEW")
                   (mapcar #'llm-chat-prompt-interaction-content
                           (llm-chat-prompt-interactions prompt))))))

(ert-deftest history-messages/a-later-rewrite-still-skips-earlier-turns ()
  "After a first request has put the system text at the front of the list, a
rewrite before a later attempt still replaces the current user message, not
an earlier one with the same text."
  (let ((prompt (llm-make-chat-prompt "HM-SAME" :context "HM-SYSTEM"))
        (provider (make-llm-openai-compatible :url "http://127.0.0.1:9/v1"
                                              :chat-model "hm-model")))
    (superchat--llm-set-history prompt (list (list :kind "user" :content "HM-SAME")))
    ;; What building the first request does to the prompt.
    (llm-provider-chat-request provider prompt nil)
    (should (equal 'system (llm-chat-prompt-interaction-role
                            (car (llm-chat-prompt-interactions prompt)))))
    (should (superchat--llm-replace-prompt-input prompt "HM-SAME" "HM-NEW"))
    (should (equal '("HM-SAME" "HM-NEW")
                   (mapcar #'llm-chat-prompt-interaction-content
                           (cdr (llm-chat-prompt-interactions prompt)))))))

;;;; The request inspector

(defun hm--inspect (input)
  "Open the request inspector for INPUT from the editing source."
  (with-current-buffer (get-buffer smoke--origin-buffer-name)
    (superchat-act input t))
  (let ((inspector (cl-find-if (lambda (buffer)
                                 (with-current-buffer buffer
                                   (derived-mode-p 'superchat-request-mode)))
                               (buffer-list))))
    (should (buffer-live-p inspector))
    inspector))

(defun hm--inspector-body (inspector)
  "The editable prompt of INSPECTOR."
  (with-current-buffer inspector
    (buffer-substring-no-properties superchat--request-prompt-start
                                    superchat--request-prompt-end)))

(ert-deftest history-messages/the-inspector-shows-earlier-turns-read-only ()
  "An inspected request shows the earlier turn -- its question, what its call
returned and its answer -- and the selection's disclosure, read-only, before
the editable prompt, and Copy includes them."
  (hm--fixture (list (hm--read-round "hm-target") '(:text "HM-ANSWER-ONE"))
    (hm--turn (smoke--open-chat) "HM-ASK-ONE")
    (let* ((inspector (hm--inspect "HM-ASK-TWO"))
           (body (hm--inspector-body inspector)))
      (with-current-buffer inspector
        (dolist (marker '("[Context v0 |" "HM-ASK-ONE" "HM-TARGET-TEXT" "HM-ANSWER-ONE"))
          (should-not (string-match-p (regexp-quote marker) body))
          (goto-char (point-min))
          (should (search-forward marker nil t))
          (should (get-text-property (match-beginning 0) 'read-only))
          (should (< (point) superchat--request-prompt-start)))
        (let ((kill-ring nil) (kill-ring-yank-pointer nil)
              (interprogram-cut-function nil))
          (superchat-request-copy)
          (should (string-match-p "HM-ANSWER-ONE" (car kill-ring))))))))

(ert-deftest history-messages/an-edited-inspector-send-keeps-earlier-turns ()
  "Editing the prompt in the inspector does not drop the earlier turn: it goes
as messages before the edited prompt, which is sent exactly as edited."
  (hm--fixture (list '(:text "HM-ANSWER-ONE") '(:text "HM-ANSWER-TWO"))
    (hm--turn (smoke--open-chat) "HM-ASK-ONE")
    (let ((inspector (hm--inspect "HM-ASK-TWO")))
      (with-current-buffer inspector
        (let ((inhibit-read-only t))
          (delete-region superchat--request-prompt-start superchat--request-prompt-end)
          (goto-char superchat--request-prompt-start)
          (insert "HM-EDITED-ONLY"))
        (superchat-request-send))
      (should (smoke--wait (lambda () (and (= 2 (length hm--wires))
                                           (null (superchat-runtime-active-runs))))
                           15))
      (let ((body (hm--body (hm--wire 2))))
        (should (equal '("user" "assistant" "user") (hm--roles (hm--wire 2))))
        (should (equal "HM-ASK-ONE" (hm--text (nth 0 body))))
        (should (equal "HM-ANSWER-ONE" (hm--text (nth 1 body))))
        (should (equal "HM-EDITED-ONLY" (hm--text (nth 2 body))))))))

;;;; The request evidence

(ert-deftest history-messages/the-evidence-rebuilds-the-messages ()
  "The second turn's request evidence rebuilds the same earlier messages the
provider received: kinds, contents, call ids and names."
  (hm--fixture nil
    (let* ((view (hm--two-turns))
           (session (hm--session view))
           (lists (hm--events session "request.message-list"))
           (second (car (last lists)))
           (evidence (superchat-tape-request-evidence
                      session (plist-get (plist-get second :payload) :request-id)))
           (messages (plist-get evidence :messages))
           (body (hm--body (hm--wire 3))))
      (should (eq 'reproduced (plist-get evidence :status)))
      (should (equal '("user" "assistant" "tool" "assistant")
                     (mapcar (lambda (m) (plist-get m :kind)) messages)))
      (should (equal (alist-get 'id (car (hm--calls (nth 1 body))))
                     (plist-get (car (plist-get (nth 1 messages) :tool-calls)) :id)))
      (should (equal (alist-get 'tool_call_id (nth 2 body))
                     (plist-get (nth 2 messages) :tool-call-id)))
      ;; llm.el's OpenAI serializer frames every tool result, live or
      ;; earlier, as "Result of tool call is TEXT".
      (should (string-suffix-p (plist-get (nth 2 messages) :content)
                               (hm--text (nth 2 body)))))))

;;;; Rows stored before arguments were named

(ert-deftest history-messages/an-old-positional-row-is-named-by-the-tool ()
  "Arguments stored as a positional array are named by the tool's parameter
list; a text that does not read, or a tool no longer defined, gives empty
arguments and a note."
  (should (fboundp 'superchat-view-call-arguments))
  (let ((named (superchat-view-call-arguments "read_buffer" "[\"init.el\"]" nil)))
    (should (equal '((buffer . "init.el"))
                   (json-parse-string (plist-get named :arguments) :object-type 'alist)))
    (should-not (plist-get named :note)))
  (let ((object (superchat-view-call-arguments "read_buffer" "{\"buffer\":\"init.el\"}" nil)))
    (should (equal '((buffer . "init.el"))
                   (json-parse-string (plist-get object :arguments) :object-type 'alist))))
  (let ((cut (superchat-view-call-arguments "read_buffer" "[\"init.e" 2400)))
    (should (equal "{}" (plist-get cut :arguments)))
    (should (string-match-p (regexp-quote "(Arguments not kept in full: [\"init.e")
                            (plist-get cut :note)))
    (should (string-match-p (regexp-quote "[cut: ") (plist-get cut :note))))
  (let ((gone (superchat-view-call-arguments "no_such_tool_hm" "[\"x\"]" nil)))
    (should (equal "{}" (plist-get gone :arguments)))
    (should (string-match-p (regexp-quote "(Arguments not kept in full:")
                            (plist-get gone :note)))))

(provide 'history-messages-regression)

;;; history-messages-regression.el ends here
