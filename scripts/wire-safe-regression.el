;;; wire-safe-regression.el --- text a request cannot carry does not end a Run -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "a request that cannot be encoded"
;; (docs/decisions/accepted/2026-11-09-a-request-that-cannot-be-encoded.md;
;; postmortem docs/postmortems/0002-text-a-request-could-not-carry.md).
;;
;; A provider request is JSON and `json-serialize' takes Unicode scalar values
;; only.  An Emacs string can hold one half of a surrogate pair (telega keeps
;; an emoji of a chat title as two), a raw byte, a character above U+10FFFF.
;; On 2026-11-09 seven of twelve executions of one conversation ended on
;; `[llm-chat-streaming error: Apparently circular structure being printed]'
;; because a tool result held such a pair: the request could not be
;; serialized, and the size estimate fell back to printing the prompt, whose
;; tools are closures over the Run.
;;
;; The cases cover the one function that states "this text can be sent", the
;; three ways text comes into a request (a tool result, the user's message, a
;; context fragment), an earlier turn read back from SQLite, the estimate
;; when no serializer works, llm.el's own request body with only plz
;; replaced, and what a Run that could not start a request records.
;;
;; This standalone suite reuses the canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets and needs no scratch
;; files.  All model calls are scripted.
;;
;; Run from the repository root, with an existing prepared dependency directory:
;;
;;   SUPERCHAT_TEST_ELPA=/path/to/prepared/elpa
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$SUPERCHAT_TEST_ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/wire-safe-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^wire-safe/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)

(when (getenv "SMOKE_REAL")
  (error "Wire-safe regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-wire)
(require 'llm-deepseek)
(require 'plz-media-type)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; What a string can hold --------------------------------------------------------

(defconst ws--flag #x1F6A9
  "The emoji telega keeps as the two halves #xD83D and #xDEA9.")

(defun ws--pair ()
  "The flag as telega holds it: two characters, each one half."
  (string #xD83D #xDEA9))

(defun ws--raw (&rest bytes)
  "BYTES as raw bytes in a multibyte string."
  (string-to-multibyte (apply #'unibyte-string bytes)))

(defun ws--sent-p (text)
  "Whether `json-serialize', which builds every request, takes TEXT."
  (condition-case nil
      (and (json-serialize (vector text)) t)
    (error nil)))

;;;; The function --------------------------------------------------------------------

(ert-deftest wire-safe/text-a-request-takes-is-returned-itself ()
  (dolist (text (list "" "plain" "读书笔记" (string ws--flag) (string 0 7 27 ?a)
                      (string #xFFFF #x10FFFF)))
    (should (ws--sent-p text))
    (should (eq text (superchat-wire-safe-text text)))))

(ert-deftest wire-safe/a-value-that-is-no-string-passes ()
  (should-not (superchat-wire-safe-text nil))
  (should (equal '(1 "a") (superchat-wire-safe-text '(1 "a"))))
  (should (eq 'done (superchat-wire-safe-text 'done))))

(ert-deftest wire-safe/the-two-halves-of-a-pair-become-the-character ()
  (let ((title (concat "读书" (ws--pair) "群")))
    (should-not (ws--sent-p title))
    (should (equal (concat "读书" (string ws--flag) "群")
                   (superchat-wire-safe-text title)))))

(ert-deftest wire-safe/a-half-read-back-from-sqlite-is-still-paired ()
  "SQLite hands a half back as the three bytes that spell it."
  (let ((stored (concat "a" (ws--raw #xED #xA0 #xBD #xED #xBA #xA9) "b")))
    (should-not (ws--sent-p stored))
    (should (equal (string ?a ws--flag ?b) (superchat-wire-safe-text stored)))))

(ert-deftest wire-safe/what-is-no-character-is-replaced ()
  (dolist (case (list (cons (string ?a #xD83D ?b) (string ?a #xFFFD ?b))
                      (cons (string #xDEA9) (string #xFFFD))
                      ;; The halves the wrong way round are two halves alone.
                      (cons (string #xDEA9 #xD83D) (string #xFFFD #xFFFD))
                      (cons (ws--raw #xFF) (string #xFFFD))
                      (cons (string #x110000 ?z) (string #xFFFD ?z))
                      ;; A character cut in half, as a shell's output can be.
                      (cons (concat "发" (ws--raw #xE6 #x98))
                            (string ?发 #xFFFD #xFFFD))
                      ;; Bytes of another encoding are not guessed at.
                      (cons (unibyte-string ?c ?a ?f #xE9) (string ?c ?a ?f #xFFFD))))
    (should-not (ws--sent-p (car case)))
    (should (equal (cdr case) (superchat-wire-safe-text (car case))))))

(ert-deftest wire-safe/bytes-that-spell-text-are-read-as-that-text ()
  (let ((bytes (encode-coding-string "读书" 'utf-8)))
    (should (equal "读书" (superchat-wire-safe-text bytes)))
    (should (equal "读书" (superchat-wire-safe-text (string-to-multibyte bytes))))
    (should (equal "a读书b"
                   (superchat-wire-safe-text
                    (concat "a" (string-to-multibyte bytes) "b"))))))

(ert-deftest wire-safe/whatever-goes-in-a-request-takes-what-comes-out ()
  "The property, on strings made of everything a string can hold."
  (let ((alphabet (vector ?a ?\n 0 ?币 ws--flag #xD83D #xDEA9 #xDBFF #xDC00
                          #xFFFD #xFFFF #x10FFFF #x110000 #x3FFF7F
                          #x3FFFED #x3FFFA0 #x3FFFBD #x3FFFBA #x3FFFA9
                          #x3FFFE5 #x3FFFB8 #x3FFF81 #x3FFFFF #x3FFF80))
        (repaired 0))
    (random "wire-safe")
    (dotimes (_ 3000)
      (let* ((text (apply #'string
                          (cl-loop repeat (random 30)
                                   collect (aref alphabet (random (length alphabet))))))
             (safe (superchat-wire-safe-text text)))
        (should (ws--sent-p safe))
        ;; Done once is done: the second pass changes nothing.
        (should (eq safe (superchat-wire-safe-text safe)))
        (if (ws--sent-p text)
            (should (eq text safe))
          (cl-incf repaired))))
    (should (> repaired 2000))))

(ert-deftest wire-safe/the-tool-seam-states-the-same-thing ()
  "The seam every tool result crosses before the Tape, the View and the provider."
  (dolist (result (list "plain" (ws--pair) (ws--raw #xFF) (string #x110000)
                        (concat "发" (ws--raw #xE6 #x98))
                        (encode-coding-string "读书" 'utf-8)
                        (unibyte-string ?c ?a ?f #xE9)))
    (should (ws--sent-p (superchat-tool-execution--normalize-tool-result result))))
  (should (equal (string ws--flag)
                 (superchat-tool-execution--normalize-tool-result (ws--pair))))
  (should (equal '(:not "text")
                 (superchat-tool-execution--normalize-tool-result '(:not "text")))))

;;;; Whole executions ----------------------------------------------------------------

(defvar ws--wires nil
  "Provider requests of the current test, newest first.
Each is (ok . REQUEST), the request as `json-serialize' wrote it and read
back, or (error . CONDITION) when it could not be written.")

(defun ws--wire-of (provider prompt)
  "PROMPT as PROVIDER's own builder and `json-serialize' send it.
`json-serialize' is what llm.el's request layer calls on the body; json.el's
`json-encode' would write a half it cannot carry and hide the failure."
  (condition-case err
      (cons 'ok
            (json-parse-string
             (json-serialize
              (llm-provider-chat-request provider (copy-tree prompt t) t))
             :object-type 'alist :array-type 'list
             :null-object nil :false-object nil))
    (error (cons 'error err))))

(defmacro ws--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical fixture, keeping each request as sent.
`print-circle' is nil inside, as in an Emacs that has not changed it: this
file turns it on for ERT's own output, and with it on, printing a prompt's
tools succeeds and the failure of 2026-11-09 cannot happen."
  (declare (indent 1) (debug t))
  `(let ((print-circle nil)
         (superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (ws--wires nil))
     (smoke--with-fixture ,script
       (let ((network (symbol-function 'smoke--fake-network)))
         (cl-letf (((symbol-function 'smoke--fake-network)
                    (lambda (mode provider prompt partial response error multi)
                      (push (ws--wire-of provider prompt) ws--wires)
                      (funcall network mode provider prompt
                               partial response error multi))))
           ,@body)))))

(defun ws--run (view text)
  "Send TEXT from VIEW, wait for its Run to end, and return the Run id."
  (let ((run (smoke--send view text)))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun ws--status (run)
  (plist-get (superchat-runtime-status run) :status))

(defun ws--wire (n)
  "The Nth provider request of this test, counting from 1."
  (let ((wires (reverse ws--wires)))
    (should (>= (length wires) n))
    (nth (1- n) wires)))

(defun ws--messages (wire)
  (should (eq 'ok (car wire)))
  (alist-get 'messages (cdr wire)))

(defun ws--contents (wire role)
  "The text of every ROLE message of WIRE."
  (cl-loop for message in (ws--messages wire)
           when (equal role (alist-get 'role message))
           collect (let ((content (alist-get 'content message)))
                     (if (stringp content) content ""))))

(defun ws--events (view type)
  (cl-remove-if-not
   (lambda (event) (equal type (plist-get event :type)))
   (superchat-tape-replay-events (buffer-local-value 'superchat--session-id view))))

(defun ws--view-text (view)
  (with-current-buffer view
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ws--has-flag (text)
  (and (stringp text) (string-search (string ws--flag) text) t))

(ert-deftest wire-safe/a-tool-result-with-a-pair-goes-on-and-is-recorded-as-sent ()
  "The execution of 2026-11-09: `eval_elisp' returns a telega chat title."
  (ws--fixture (list '(:tool-uses (("eval_elisp" (expression . "(string #xD83D #xDEA9)"))))
                     '(:text "WS-ANSWER"))
    (let* ((view (smoke--open-chat))
           (run (ws--run view "title of the chat")))
      (should (eq 'completed (ws--status run)))
      (should (= 2 (length ws--wires)))
      (should (string-match-p "WS-ANSWER" (ws--view-text view)))
      ;; The follow-up request carries the flag itself.
      (should (cl-some #'ws--has-flag (ws--contents (ws--wire 2) "tool")))
      ;; And the Tape holds the text the provider was sent.
      (let ((recorded (plist-get (plist-get (car (ws--events view "tool.completed"))
                                            :payload)
                                 :result-text)))
        (should (ws--sent-p recorded))
        (should (ws--has-flag recorded))))))

(ert-deftest wire-safe/a-message-typed-with-a-pair-is-sent-now-and-from-the-tape ()
  (ws--fixture (list '(:text "WS-FIRST") '(:text "WS-SECOND"))
    (let* ((view (smoke--open-chat))
           (first (ws--run view (concat "summarise " (ws--pair)))))
      (should (eq 'completed (ws--status first)))
      (should (cl-some #'ws--has-flag (ws--contents (ws--wire 1) "user")))
      ;; The next turn reads the first one back from SQLite, where a half is
      ;; three bytes: it is the same flag in the history.
      (let ((second (ws--run view "and again")))
        (should (eq 'completed (ws--status second)))
        (should (= 2 (length ws--wires)))
        (should (cl-some #'ws--has-flag (ws--contents (ws--wire 2) "user")))))))

(ert-deftest wire-safe/a-buffer-named-with-a-pair-can-be-asked-from ()
  "The origin buffer's name is a context fragment of every request from it."
  (let ((smoke--origin-buffer-name (concat " *ws chat " (ws--pair) "*")))
    (ws--fixture (list '(:text "WS-ANSWER"))
      (let* ((view (smoke--open-chat))
             (run (ws--run view "hello")))
        (should (eq 'completed (ws--status run)))
        (should (= 1 (length ws--wires)))
        (should (cl-some #'ws--has-flag (ws--contents (ws--wire 1) "user")))))))

;;;; An estimate measures; it does not decide -----------------------------------------

(ert-deftest wire-safe/an-estimate-comes-back-when-nothing-can-serialize ()
  "A prompt's tools are closures over the Run: it cannot be printed."
  (let* ((cell (list nil))
         (tool (llm-make-tool :function (lambda (&rest _) cell)
                              :name "ws_tool" :description "A tool."
                              :args '((:name "x" :type string :description "An x."))))
         (prompt (llm-make-chat-prompt "hello there" :context "SYSTEM"
                                       :tools (list tool)))
         (provider (make-llm-openai-compatible :url "http://127.0.0.1:9/v1"
                                               :chat-model "ws-model")))
    ;; The closure holds a structure that holds the closure.
    (setcar cell (llm-tool-function tool))
    (cl-letf (((symbol-function 'llm-provider-chat-request)
               (lambda (&rest _) (error "No builder")))
              ((symbol-function 'superchat--llm-evidence-messages)
               (lambda (&rest _) (error "No evidence"))))
      (should-not (superchat--llm-serialized-attempt provider prompt t))
      (let ((estimate (superchat--llm-estimate-context-tokens provider prompt)))
        (should (integerp estimate))
        ;; "hello there" + "SYSTEM" + the tool's words, at two characters a
        ;; token: more than a handful, less than the page a print would be.
        (should (< 10 estimate 200))))))

(ert-deftest wire-safe/a-run-goes-on-when-its-estimate-has-no-serializer ()
  (ws--fixture (list '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                     '(:text "WS-ANSWER"))
    (let ((builder (symbol-function 'llm-provider-chat-request))
          (estimating nil))
      ;; Only the estimate loses its serializers: the request itself is built
      ;; as ever.
      (cl-letf (((symbol-function 'superchat--llm-serialized-attempt)
                 (let ((real (symbol-function 'superchat--llm-serialized-attempt)))
                   (lambda (&rest arguments)
                     (let ((estimating t))
                       (cl-letf (((symbol-function 'llm-provider-chat-request)
                                  (lambda (&rest more)
                                    (if estimating
                                        (error "No builder")
                                      (apply builder more))))
                                 ((symbol-function 'superchat--llm-evidence-messages)
                                  (lambda (&rest _) (error "No evidence"))))
                         (apply real arguments)))))))
        (ignore estimating)
        (let* ((view (smoke--open-chat))
               (run (ws--run view "read it")))
          (should (eq 'completed (ws--status run)))
          (should (= 2 (length smoke--requests)))
          (should (string-match-p "WS-ANSWER" (ws--view-text view))))))))

;;;; llm.el's own request body ----------------------------------------------------------

(defun ws--body-llm-el-sends (prompt)
  "The request body llm.el hands plz for PROMPT, or the condition it signals.
llm.el's own non-streaming path for a DeepSeek provider, with only plz's
request replaced."
  (let ((provider (make-llm-deepseek :key "ws" :chat-model "deepseek-v4-flash"))
        (llm-warn-on-nonfree nil)
        (body nil))
    (condition-case err
        (cl-letf (((symbol-function 'plz-media-type-request)
                   (lambda (_method _url &rest arguments)
                     (setq body (plist-get arguments :body))
                     nil)))
          (llm-chat-async provider prompt #'ignore #'ignore)
          body)
      (error (cons 'signalled err)))))

(defun ws--prompt-with-result (result)
  "A prompt whose one tool call returned RESULT."
  (let ((prompt (llm-make-chat-prompt "title of the chat" :context "SYSTEM")))
    (llm-provider-utils-append-to-prompt
     prompt nil
     (list (make-llm-chat-prompt-tool-result
            :call-id "call_ws" :tool-name "eval_elisp" :result result)))
    prompt))

(ert-deftest wire-safe/llm-el-rejects-the-pair-and-takes-what-the-gate-leaves ()
  ;; As it is, llm.el cannot build the body.  When this stops holding, llm.el
  ;; or Emacs has changed what a request can carry.
  (let ((sent (ws--body-llm-el-sends (ws--prompt-with-result (ws--pair)))))
    (should (eq 'signalled (car-safe sent)))
    (should (superchat--llm-request-not-encodable-p (cdr sent))))
  ;; Through the gate, the bytes plz is given are JSON holding the flag.
  (let* ((prompt (superchat--llm-prompt-wire-safe
                  (ws--prompt-with-result (ws--pair))))
         (sent (ws--body-llm-el-sends prompt)))
    (should (stringp sent))
    (should-not (multibyte-string-p sent))
    (let* ((request (json-parse-string (decode-coding-string sent 'utf-8)
                                       :object-type 'alist :array-type 'list))
           (tool (cl-find "tool" (alist-get 'messages request)
                          :key (lambda (message) (alist-get 'role message))
                          :test #'equal)))
      (should (ws--has-flag (alist-get 'content tool))))))

(ert-deftest wire-safe/the-gate-reaches-every-text-of-a-prompt ()
  (let* ((prompt (llm-make-chat-prompt
                  (llm-make-multipart (concat "part " (ws--pair)) "plain")
                  :context (concat "SYSTEM " (ws--pair))))
         (kept "left as it is"))
    (llm-provider-utils-append-to-prompt prompt (concat "answer " (ws--pair)))
    (llm-provider-utils-append-to-prompt
     prompt nil
     (list (make-llm-chat-prompt-tool-result
            :call-id "call_ws" :tool-name "eval_elisp" :result (ws--pair))))
    (llm-provider-utils-append-to-prompt prompt kept nil nil 'user)
    (should (eq prompt (superchat--llm-prompt-wire-safe prompt)))
    (should (ws--has-flag (llm-chat-prompt-context prompt)))
    (pcase-let ((`(,first ,answer ,results ,last)
                 (llm-chat-prompt-interactions prompt)))
      (should (ws--has-flag
               (car (llm-multipart-parts
                     (llm-chat-prompt-interaction-content first)))))
      (should (ws--has-flag (llm-chat-prompt-interaction-content answer)))
      (should (ws--has-flag
               (llm-chat-prompt-tool-result-result
                (car (llm-chat-prompt-interaction-tool-results results)))))
      ;; A text that can be sent stays the object it was.
      (should (eq kept (llm-chat-prompt-interaction-content last))))))

;;;; A request that still cannot be started ---------------------------------------------

(defun ws--failing-network (condition)
  "A network whose every request signals CONDITION before it is sent."
  (lambda (&rest _)
    (signal (car condition) (cdr condition))))

(ert-deftest wire-safe/a-request-that-cannot-be-encoded-ends-the-run-saying-so ()
  "What the gate does not reach (a tool's schema, a provider's option) still fails."
  (let ((rejected (concat (make-string 300 ?x) (ws--pair))))
    (ws--fixture (list '(:text "WS-NEVER"))
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (ws--failing-network
                  (list 'wrong-type-argument 'json-value-p rejected))))
        (let* ((view (smoke--open-chat))
               (run (ws--run view "hello"))
               (text (ws--view-text view))
               (failed (car (ws--events view "run.failed")))
               (failure (plist-get (plist-get failed :payload) :failure)))
          (should (eq 'failed (ws--status run)))
          (should (string-match-p
                   "\\[Superchat could not start this request: a text in it cannot be encoded as JSON (302 characters, starting \"x+\")\\]"
                   text))
          ;; Not the words of 2026-11-09, and not the text that was rejected.
          (should-not (string-match-p "circular\\|llm-chat-streaming" text))
          (should-not (string-search (ws--pair) text))
          (should (equal "request-not-encodable" (plist-get failure :class)))
          (should (equal "initial" (plist-get failure :phase)))
          (should (equal "wrong-type-argument" (plist-get failure :error-type)))
          (should (ws--sent-p (plist-get failure :reason))))))))

(ert-deftest wire-safe/any-other-request-that-does-not-start-is-named-with-its-reason ()
  (ws--fixture (list '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                     '(:text "WS-NEVER"))
    (let ((network (symbol-function 'smoke--fake-network))
          (calls 0))
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (lambda (&rest arguments)
                   (if (= 1 (cl-incf calls))
                       (apply network arguments)
                     (error "WS-NO-CURL")))))
        (let* ((view (smoke--open-chat))
               (run (ws--run view "read it"))
               (failure (plist-get (plist-get (car (ws--events view "run.failed"))
                                              :payload)
                                   :failure)))
          (should (eq 'failed (ws--status run)))
          (should (string-match-p
                   "\\[Superchat could not start this request: WS-NO-CURL\\]"
                   (ws--view-text view)))
          (should (equal "request-not-started" (plist-get failure :class)))
          ;; The request after a tool round is the one that did not start.
          (should (equal "tool-followup" (plist-get failure :phase)))
          (should (equal "WS-NO-CURL" (plist-get failure :reason))))))))

(ert-deftest wire-safe/a-run-that-fails-any-other-way-carries-no-such-reason ()
  (ws--fixture (list '(:error (error . "WS-PROVIDER-SAID-NO")))
    (let* ((view (smoke--open-chat))
           (run (ws--run view "hello"))
           (failed (car (ws--events view "run.failed"))))
      (should (eq 'failed (ws--status run)))
      (should failed)
      (should-not (plist-member (plist-get failed :payload) :failure)))))

(ert-deftest wire-safe/a-propertied-text-is-quoted-as-its-characters ()
  "telega's texts carry properties; the reason quotes the characters only."
  (let* ((title (concat (propertize (make-string 45 ?x) 'telega-emoji-p t)
                        (ws--pair)))
         (failure (superchat--llm-local-failure
                   (list 'wrong-type-argument 'json-value-p title)
                   'tool-followup)))
    (should (equal "a text in it cannot be encoded as JSON (47 characters, starting \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\")"
                   (plist-get failure :reason)))
    (should (equal "tool-followup" (plist-get failure :phase)))))

;;;; Two more places a string meets a printer or an encoder ---------------------------

(ert-deftest wire-safe/a-prompt-size-diagnostic-does-not-end-a-run ()
  "`superchat-show-ttft-breakdown' counts the request's tools, which are closures."
  (let ((superchat-show-ttft-breakdown t))
    (ws--fixture (list '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                       '(:text "WS-ANSWER"))
      (let* ((view (smoke--open-chat))
             (run (ws--run view "read it")))
        (should (alist-get 'tools (cdr (ws--wire 1))))
        (should (eq 'completed (ws--status run)))
        (should (= 2 (length smoke--requests)))
        (should (string-match-p "WS-ANSWER" (ws--view-text view)))))))

(ert-deftest wire-safe/a-chat-holding-a-half-is-cached-without-a-question ()
  "Closing the chat writes its text to the session cache; it must not ask.
On Emacs 32 the halves of a pair are written as the bytes ED A0 BD ED BA A9
without a question, and read back as six raw bytes; a raw byte (a character
cut in half) makes the write ask \"Select coding system\", which in batch
reads standard input and signals."
  (ws--fixture (list)
    (let ((view (smoke--open-chat))
          (cache (expand-file-name "session-cache.org" superchat-data-directory)))
      (with-current-buffer view
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n" (make-string 120 ?x) " 群 " (ws--pair)
                  " 发" (ws--raw #xE6 #x98) "\n")))
      (with-current-buffer view (superchat--summarize-session-on-exit))
      (should (file-exists-p cache))
      ;; Read back as the next startup reads it
      ;; (`superchat--process-cached-session-on-startup').
      (let ((text (with-temp-buffer
                    (insert-file-contents cache)
                    (buffer-string))))
        (should (ws--has-flag text))
        (should (string-search (string ?发 #xFFFD #xFFFD) text))
        (should (ws--sent-p text))))))

(provide 'wire-safe-regression)
;;; wire-safe-regression.el ends here
