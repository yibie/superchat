;;; test-llm.el --- tests for superchat-llm.el -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'json)
(require 'superchat-llm)
;; Loaded here so that the `let' of their variables binds them dynamically.
(require 'superchat-bridge-http)
(require 'superchat-bridge-utils-retry)

;; The provider is a real the bridge provider whose API is a stub: it records what
;; it was given and returns the event stream the script says, at once or from
;; a timer.  Script entry: (:text S :thinking S :calls ((ID NAME ARGS)...)
;; :deltas LIST :stop S :error S :later BOOL :raise BOOL).

(defvar test-llm--calls nil "What the stub API received, newest first.")
(defvar test-llm--script nil "Entries of the stub's answers, in order.")
(defvar test-llm--last nil "The final assistant message the stub made last.")

(defun test-llm--message (model spec)
  "The final assistant message of MODEL that script entry SPEC describes."
  (superchat-bridge-make-assistant-message
   :content (append
             (when-let* ((thinking (plist-get spec :thinking)))
               (list (superchat-bridge-make-thinking-content
                      :thinking thinking :thinking-signature "reasoning_content")))
             (when-let* ((text (plist-get spec :text)))
               (list (superchat-bridge-make-text-content :text text)))
             (mapcar (pcase-lambda (`(,id ,name ,args))
                       (superchat-bridge-make-tool-call :id id :name name :arguments args))
                     (plist-get spec :calls)))
   :api (superchat-bridge-model-api model) :provider (superchat-bridge-model-provider model)
   :model (superchat-bridge-model-id model)
   :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
   :stop-reason (or (plist-get spec :stop) (if (plist-get spec :calls) "toolUse" "stop"))
   :error-message (plist-get spec :error)
   :timestamp 1))

(defun test-llm--emit (stream message deltas)
  "Push the events of MESSAGE, with text DELTAS, to STREAM and end it."
  (superchat-bridge-event-stream-push stream (superchat-bridge-make-event :type "start" :partial message))
  (dolist (delta deltas)
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "text_delta" :content-index 0
                                  :delta delta :partial message)))
  (let ((reason (superchat-bridge-assistant-message-stop-reason message)))
    (superchat-bridge-event-stream-push
     stream (if (member reason '("error" "aborted"))
                (superchat-bridge-make-event :type "error" :reason reason :error message)
              (superchat-bridge-make-event :type "done" :reason reason :message message))))
  (superchat-bridge-event-stream-end stream message))

(defun test-llm--respond (model context options)
  "The stub API: record the request, answer from `test-llm--script'."
  (push (list :model model :context context :options options) test-llm--calls)
  (let* ((spec (pop test-llm--script))
         (stream (superchat-bridge-create-assistant-message-event-stream))
         (message (test-llm--message model spec))
         (deltas (or (plist-get spec :deltas)
                     (and (plist-get spec :text) (list (plist-get spec :text))))))
    (when (plist-get spec :raise) (error "Boom at start"))
    (setq test-llm--last message)
    (if (plist-get spec :later)
        (run-at-time 0 nil #'test-llm--emit stream message deltas)
      (test-llm--emit stream message deltas))
    stream))

(defun test-llm--provider (&optional id resolve)
  "A bridge provider ID (default \"stub\") with two models and the stub API.
RESOLVE, when given, is its API key resolver."
  (superchat-bridge-create-provider
   :id (or id "stub") :name "Stub" :base-url "http://stub.test"
   :auth (superchat-bridge-make-provider-auth
          :api-key (superchat-bridge-make-api-key-auth
                    :name "none" :resolve (or resolve (lambda (_c _k _s) (list :auth nil)))))
   :models (mapcar (lambda (model)
                     (superchat-bridge-make-model
                      :id model :name model :api "test-api" :provider (or id "stub")
                      :base-url "http://stub.test" :input '("text")
                      :reasoning t :cost (superchat-bridge-make-model-cost)
                      :context-window 10000 :max-tokens 1000))
                   '("model-a" "model-b"))
   :api (superchat-bridge-make-provider-streams :stream #'test-llm--respond
                                         :stream-simple #'test-llm--respond)))

(defmacro test-llm-with (script &rest body)
  "Run BODY with the stub provider answering SCRIPT, and no state kept."
  (declare (indent 1))
  `(let ((test-llm--calls nil) (test-llm--script (copy-sequence ,script))
         (superchat-llm-providers (list (cons "stub" #'test-llm--provider)))
         (superchat-llm-provider "stub") (superchat-llm-model "model-a")
         (superchat-llm-reasoning nil) (superchat-llm--models nil)
         (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request))
     ,@body))

(defun test-llm--ask (messages &rest keys)
  "Ask with MESSAGES and KEYS; return (:pieces :done :errors :handle), newest first."
  (let ((r (list :pieces nil :done nil :errors nil)))
    (setf (plist-get r :handle)
          (apply #'superchat-llm-request messages
                 :on-text (lambda (s) (push s (plist-get r :pieces)))
                 :on-done (lambda (x) (push x (plist-get r :done)))
                 :on-error (lambda (x) (push x (plist-get r :errors)))
                 keys))
    r))

(defun test-llm--settle ()
  "Let timers run for a moment."
  (let ((end (+ (float-time) 0.06)))
    (while (< (float-time) end) (accept-process-output nil 0.01))))

(defun test-llm--user (text) (list :role 'user :text text))

(defun test-llm--sent (&optional n)
  "The messages of the Nth newest request of the stub (default the newest)."
  (superchat-bridge-context-messages (plist-get (nth (or n 0) test-llm--calls) :context)))

(defun test-llm--args-hash ()
  "Arguments as the bridge parses them: a hash table with nested object, array, false."
  (superchat-bridge-json-object "path" "a.el" "count" 5
                         "opts" (superchat-bridge-json-object "deep" t "off" :false)
                         "list" ["x" "y"]))

(ert-deftest superchat-llm-text-deltas-and-done ()
  (test-llm-with (list '(:text "Hello" :deltas ("Hel" "lo")))
    (let ((r (test-llm--ask (list (test-llm--user "hi")) :system "Be brief.")))
      (should (equal (reverse (plist-get r :pieces)) '("Hel" "lo")))
      (should (equal (plist-get r :done)
                     '((:text "Hello" :calls nil :model "model-a"))))
      (should-not (plist-get r :errors))
      (should (plist-member (plist-get (car test-llm--calls) :options) :signal)))))

(ert-deftest superchat-llm-async-answer-comes-once ()
  (test-llm-with (list '(:text "late" :later t))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should-not (plist-get r :done))
      (test-llm--settle)
      (should (equal (plist-get r :pieces) '("late")))
      (should (= (length (plist-get r :done)) 1))
      (should-not (plist-get r :errors)))))

(ert-deftest superchat-llm-calls-have-provider-ids-and-plist-args ()
  (test-llm-with (list (list :text "ok" :thinking "hmm"
                             :calls `(("call_9" "read_file" ,(test-llm--args-hash))
                                      ("call_10" "list_buffers" nil))))
    (let* ((r (test-llm--ask (list (test-llm--user "hi"))))
           (done (car (plist-get r :done))))
      (should (equal (plist-get done :calls)
                     '((:id "call_9" :name "read_file"
                        :args (:path "a.el" :count 5 :opts (:deep t :off nil)
                               :list ("x" "y")))
                       (:id "call_10" :name "list_buffers" :args nil))))
      ;; the reasoning is in neither the text nor the result's keys
      (should (equal (plist-get done :text) "ok"))
      (should (equal (cl-loop for (k) on done by #'cddr collect k)
                     '(:text :calls :model))))))

(ert-deftest superchat-llm-unparsable-arguments-come-back-as-nil-args ()
  (let ((bad (superchat-bridge-make-tool-call :id "c1" :name "t" :arguments (superchat-bridge-json-object)
                                       :arguments-error "not an object")))
    (test-llm-with (list '(:text "x"))
      (cl-letf (((symbol-function 'test-llm--message)
                 (let ((orig (symbol-function 'test-llm--message)))
                   (lambda (model spec)
                     (let ((m (funcall orig model spec)))
                       (setf (superchat-bridge-message-content m) (list bad)
                             (superchat-bridge-assistant-message-stop-reason m) "toolUse")
                       m)))))
        (let ((r (test-llm--ask (list (test-llm--user "hi")))))
          (should (equal (plist-get (car (plist-get r :done)) :calls)
                         '((:id "c1" :name "t" :args nil)))))))))

(ert-deftest superchat-llm-error-stop-goes-to-on-error ()
  (test-llm-with (list '(:stop "error" :error "429 slow down"))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should (equal (plist-get r :errors) '("429 slow down")))
      (should-not (plist-get r :done)))))

(ert-deftest superchat-llm-aborted-stop-calls-neither ()
  (test-llm-with (list '(:stop "aborted"))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should-not (plist-get r :done))
      (should-not (plist-get r :errors)))))

(ert-deftest superchat-llm-cancel-before-the-answer ()
  (test-llm-with (list '(:text "late" :later t))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (superchat-llm-cancel (plist-get r :handle))
      (test-llm--settle)
      (should (superchat-bridge-abort-signal-aborted-p
               (plist-get (plist-get (car test-llm--calls) :options) :signal)))
      (should-not (plist-get r :pieces))
      (should-not (plist-get r :done))
      (should-not (plist-get r :errors)))))

(ert-deftest superchat-llm-cancel-after-the-answer-changes-nothing ()
  (test-llm-with (list '(:text "done"))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (superchat-llm-cancel (plist-get r :handle))
      (should (= (length (plist-get r :done)) 1))
      (should-not (plist-get r :errors))
      (should-not (superchat-bridge-abort-signal-aborted-p
                   (plist-get (plist-get (car test-llm--calls) :options) :signal))))))

(ert-deftest superchat-llm-start-time-signal-goes-to-on-error ()
  (test-llm-with (list '(:raise t))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should (= (length (plist-get r :errors)) 1))
      (should (string-match-p "Boom at start" (car (plist-get r :errors))))
      (should-not (plist-get r :done))))
  ;; a message the bridge cannot be given fails inside the module, same route
  (test-llm-with nil
    (let ((r (test-llm--ask (list '(:role system :text "x")))))
      (should (= (length (plist-get r :errors)) 1))
      (should (string-match-p "role" (car (plist-get r :errors))))
      (should-not test-llm--calls))))

(ert-deftest superchat-llm-an-error-in-on-done-is-the-callers ()
  (test-llm-with (list '(:text "x"))
    (let (errors)
      (should-error (superchat-llm-request
                     (list (test-llm--user "hi"))
                     :on-done (lambda (_) (error "mine"))
                     :on-error (lambda (e) (push e errors))))
      (should-not errors))))

(ert-deftest superchat-llm-unset-or-unknown-provider-is-a-user-error ()
  (test-llm-with nil
    (let ((superchat-llm-provider nil))
      (let ((err (should-error (test-llm--ask (list (test-llm--user "hi")))
                               :type 'user-error)))
        (should (string-match-p "superchat-llm-provider" (cadr err)))
        (should (string-match-p "stub" (cadr err)))))
    (let ((superchat-llm-provider "nope"))
      (let ((err (should-error (test-llm--ask (list (test-llm--user "hi")))
                               :type 'user-error)))
        (should (string-match-p "nope" (cadr err)))
        (should (string-match-p "stub" (cadr err)))))
    (should-not test-llm--calls)))

(ert-deftest superchat-llm-unset-or-unknown-model-is-a-user-error ()
  (test-llm-with nil
    (let ((superchat-llm-model nil))
      (let ((err (should-error (test-llm--ask (list (test-llm--user "hi")))
                               :type 'user-error)))
        (should (string-match-p "superchat-llm-model" (cadr err)))
        (should (string-match-p "model-a, model-b" (cadr err)))))
    (let ((superchat-llm-model "model-z"))
      (let ((err (should-error (test-llm--ask (list (test-llm--user "hi")))
                               :type 'user-error)))
        (should (string-match-p "model-z" (cadr err)))
        (should (string-match-p "model-a, model-b" (cadr err)))))
    (should-not test-llm--calls)))

(ert-deftest superchat-llm-model-override ()
  (test-llm-with (list '(:text "x") '(:text "y"))
    (let ((r (test-llm--ask (list (test-llm--user "hi")) :model "model-b")))
      (should (equal (plist-get (car (plist-get r :done)) :model) "model-b"))
      (should (equal (superchat-bridge-model-id (plist-get (car test-llm--calls) :model))
                     "model-b")))
    (test-llm--ask (list (test-llm--user "hi")))
    (should (equal (superchat-bridge-model-id (plist-get (car test-llm--calls) :model))
                   "model-a"))
    (should-error (test-llm--ask (list (test-llm--user "hi")) :model "model-z")
                  :type 'user-error)))

(ert-deftest superchat-llm-provider-added-later-is-found ()
  (test-llm-with (list '(:text "x") '(:text "y"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((superchat-llm-provider "other"))
      (should-error (test-llm--ask (list (test-llm--user "hi"))) :type 'user-error))
    (let ((superchat-llm-providers
           (append superchat-llm-providers (list (cons "other" (lambda () (test-llm--provider "other"))))))
          (superchat-llm-provider "other"))
      (should (= (length (plist-get (test-llm--ask (list (test-llm--user "hi"))) :done))
                 1)))))

(ert-deftest superchat-llm-reasoning-reaches-the-options ()
  (test-llm-with (list '(:text "a") '(:text "b"))
    (test-llm--ask (list (test-llm--user "hi")))
    (should-not (plist-member (plist-get (car test-llm--calls) :options) :reasoning))
    (let ((superchat-llm-reasoning 'high))
      (test-llm--ask (list (test-llm--user "hi"))))
    (should (equal (plist-get (plist-get (car test-llm--calls) :options) :reasoning)
                   "high"))))

(ert-deftest superchat-llm-tool-specs-become-json-schema ()
  (test-llm-with (list '(:text "x"))
    (test-llm--ask (list (test-llm--user "hi"))
                   :tools '((:name "read_file" :description "Read a file"
                             :args ((:name "path" :type string :description "File" :required t)
                                    (:name "count" :type number :description "Lines")
                                    (:name "all" :type boolean :required t)))
                            (:name "list_buffers" :description "List" :args nil)))
    (let* ((system (car (test-llm--sent)))
           (tools (superchat-bridge-system-message-tools-added system)))
      (should (equal (mapcar #'superchat-bridge-tool-name tools) '("read_file" "list_buffers")))
      (should (equal (superchat-bridge-tool-description (car tools)) "Read a file"))
      (should (equal (superchat-bridge-tool-parameters (car tools))
                     '((type . "object")
                       (properties (path (type . "string") (description . "File"))
                                   (count (type . "number") (description . "Lines"))
                                   (all (type . "boolean") (description . "")))
                       (required . ["path" "all"]))))
      (should (equal (superchat-bridge-tool-parameters (cadr tools))
                     '((type . "object") (properties)))))))

(defun test-llm--replay (call-id)
  "Messages that answer CALL-ID's turn: user, assistant with the call, result."
  (list (test-llm--user "hi")
        (list :role 'assistant :text "ok"
              :calls (list (list :id call-id :name "read_file" :args '(:path "a.el"))))
        (list :role 'tool :call-id call-id :name "read_file" :text "contents")))

(ert-deftest superchat-llm-kept-message-is-replayed-within-a-turn ()
  (test-llm-with (list (list :text "ok" :thinking "I should read"
                             :calls '(("call_1" "read_file" nil)))
                       '(:text "all done"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((first test-llm--last))
      (test-llm--ask (test-llm--replay "call_1"))
      (let* ((sent (test-llm--sent))
             (assistant (cl-find-if #'superchat-bridge-assistant-message-p sent)))
        (should (eq assistant first))
        (should (cl-some #'superchat-bridge-thinking-content-p
                         (superchat-bridge-message-content assistant)))
        (should (cl-some #'superchat-bridge-tool-result-message-p sent))))))

(ert-deftest superchat-llm-kept-message-is-dropped-after-a-user-message ()
  (test-llm-with (list '(:text "ok" :thinking "I should read"
                         :calls (("call_1" "read_file" nil)))
                       '(:text "next"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((first test-llm--last))
      (test-llm--ask (append (test-llm--replay "call_1") (list (test-llm--user "again"))))
      (let ((assistant (cl-find-if #'superchat-bridge-assistant-message-p (test-llm--sent))))
        (should-not (eq assistant first))
        (should-not (cl-some #'superchat-bridge-thinking-content-p
                             (superchat-bridge-message-content assistant)))
        (should (equal (superchat-bridge-tool-call-id
                        (cl-find-if #'superchat-bridge-tool-call-p
                                    (superchat-bridge-message-content assistant)))
                       "call_1"))))))

(ert-deftest superchat-llm-unkept-assistant-is-built-as-the-models-own ()
  (test-llm-with (list '(:text "x"))
    (test-llm--ask (test-llm--replay "call_7"))
    (let* ((model (plist-get (car test-llm--calls) :model))
           (assistant (cl-find-if #'superchat-bridge-assistant-message-p (test-llm--sent)))
           (call (cl-find-if #'superchat-bridge-tool-call-p (superchat-bridge-message-content assistant))))
      (should (equal (list (superchat-bridge-assistant-message-api assistant)
                           (superchat-bridge-assistant-message-provider assistant)
                           (superchat-bridge-assistant-message-model assistant)
                           (superchat-bridge-assistant-message-stop-reason assistant))
                     (list (superchat-bridge-model-api model) "stub" "model-a" "toolUse")))
      (should (equal (superchat-bridge-tool-call-name call) "read_file"))
      (should (equal (superchat-bridge-text-content-text
                      (car (superchat-bridge-message-content (cl-find-if #'superchat-bridge-tool-result-message-p
                                                                  (test-llm--sent)))))
                     "contents")))))

;; The path a real failure came through: DeepSeek asks for its reasoning back
;; together with the tool call it led to.

(ert-deftest superchat-llm-deepseek-gets-its-reasoning-back ()
  (let* ((requests nil)
         (process-environment (list "DEEPSEEK_API_KEY=dummy" "HOME=/nonexistent"))
         (superchat-bridge-retry-run-at-time-function (lambda (&rest _) nil))
         (superchat-bridge-retry-cancel-timer-function #'ignore)
         (superchat-bridge-http-start-function
          (lambda (_args _config stdin on-output on-exit)
            (setq requests (append requests (list (list stdin on-output on-exit))))
            #'ignore))
         (superchat-llm-providers '(("deepseek" . superchat-bridge-deepseek-provider)))
         (superchat-llm-provider "deepseek") (superchat-llm-model "deepseek-flash")
         (superchat-llm-reasoning 'high) (superchat-llm--models nil)
         (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request)
         (chunks
          (list
           "{\"id\":\"c1\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"reasoning_content\":\"Need the list\"}}]}"
           "{\"id\":\"c1\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_a\",\"type\":\"function\",\"function\":{\"name\":\"list_buffers\",\"arguments\":\"{\\\"all\\\":true}\"}}]}}]}"
           "{\"id\":\"c1\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"))
         (tool '(:name "list_buffers" :description "List the open buffers"
                 :args ((:name "all" :type boolean :description "Include hidden"))))
         (r (test-llm--ask (list (test-llm--user "hi")) :system "Be brief." :tools (list tool))))
    (pcase-let ((`(,_ ,on-output ,on-exit) (car requests)))
      (funcall on-output
               (encode-coding-string
                (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
                        (mapconcat (lambda (c) (concat "data: " c "\n\n")) chunks "")
                        "data: [DONE]\n\n")
                'utf-8))
      (funcall on-exit 0 ""))
    (should-not (plist-get r :errors))
    (let ((first (json-parse-string (car (nth 0 requests)) :object-type 'alist)))
      (should (equal (alist-get 'name (alist-get 'function (aref (alist-get 'tools first) 0)))
                     "list_buffers"))
      (should (equal (alist-get 'reasoning_effort first) "high")))
    (let ((done (car (plist-get r :done))))
      (should (equal (plist-get done :calls)
                     '((:id "call_a" :name "list_buffers" :args (:all t)))))
      (test-llm--ask (list (test-llm--user "hi")
                           (list :role 'assistant :text "" :calls (plist-get done :calls))
                           (list :role 'tool :call-id "call_a" :name "list_buffers"
                                 :text "*scratch*"))
                     :system "Be brief." :tools (list tool))
      (let* ((second (json-parse-string (car (nth 1 requests)) :object-type 'alist))
             (messages (append (alist-get 'messages second) nil))
             (assistant (cl-find "assistant" messages
                                 :key (lambda (m) (alist-get 'role m)) :test #'equal))
             (result (cl-find "tool" messages
                              :key (lambda (m) (alist-get 'role m)) :test #'equal)))
        (should (equal (alist-get 'reasoning_content assistant) "Need the list"))
        (should (equal (alist-get 'id (aref (alist-get 'tool_calls assistant) 0)) "call_a"))
        (should (equal (alist-get 'tool_call_id result) "call_a"))
        (should (equal (alist-get 'content result) "*scratch*"))))))

;; Defects a reviewer found: each test below fails without its fix.

(defun test-llm--tool-call-of (message)
  (cl-find-if #'superchat-bridge-tool-call-p (superchat-bridge-message-content message)))

(defun test-llm--assistants ()
  "The assistant messages of the newest request of the stub."
  (cl-remove-if-not #'superchat-bridge-assistant-message-p (test-llm--sent)))

(defun test-llm--thinking-of (message)
  (when-let* ((block (cl-find-if #'superchat-bridge-thinking-content-p
                                 (superchat-bridge-message-content message))))
    (superchat-bridge-thinking-content-thinking block)))

(defun test-llm--on-text-raises (later)
  "Ask with an ON-TEXT that signals on the first delta; LATER pushes from a timer."
  (test-llm-with (list (list :text "abc" :deltas '("a" "b" "c") :later later))
    (let* ((texts nil) (done nil) (errors nil)
           (handle (superchat-llm-request
                    (list (test-llm--user "hi"))
                    :on-text (lambda (s) (push s texts) (error "bad %s" s))
                    :on-done (lambda (x) (push x done))
                    :on-error (lambda (x) (push x errors)))))
      (test-llm--settle)
      (should (equal errors '("bad a")))
      (should-not done)
      (should (equal texts '("a")))
      (should (superchat-bridge-abort-signal-aborted-p
               (plist-get (plist-get (car test-llm--calls) :options) :signal)))
      (should (plist-get handle :finished)))))

(ert-deftest superchat-llm-on-text-error-aborts-and-reaches-on-error-at-once ()
  (test-llm--on-text-raises nil))

(ert-deftest superchat-llm-on-text-error-aborts-and-reaches-on-error-from-a-timer ()
  (test-llm--on-text-raises t))

(ert-deftest superchat-llm-rebuilt-arguments-are-json-objects-and-arrays ()
  (test-llm-with (list '(:text "x"))
    (test-llm--ask
     (list (test-llm--user "hi")
           (list :role 'assistant :text ""
                 :calls (list (list :id "c1" :name "t"
                                    :args '(:arr ((:k 1) (:k 2)) :nested (("a" "b"))
                                            :f nil :n 3 :s ":kw" :obj (:x "y") :zh "中文"))
                              (list :id "c2" :name "u" :args nil)))
           (list :role 'tool :call-id "c1" :name "t" :text "r1")
           (list :role 'tool :call-id "c2" :name "u" :text "r2")))
    (let* ((assistant (car (test-llm--assistants)))
           (calls (cl-remove-if-not #'superchat-bridge-tool-call-p
                                    (superchat-bridge-message-content assistant)))
           (args (superchat-bridge-tool-call-arguments (car calls)))
           (parsed (json-parse-string
                    (json-serialize args :false-object :false :null-object :null)
                    :object-type 'alist :array-type 'list
                    :false-object :false :null-object :null)))
      (should (hash-table-p args))
      (should (equal (alist-get 'arr parsed) '(((k . 1)) ((k . 2)))))
      (should (equal (alist-get 'nested parsed) '(("a" "b"))))
      (should (eq (alist-get 'f parsed) :false))
      (should (equal (alist-get 'n parsed) 3))
      (should (equal (alist-get 's parsed) ":kw"))
      (should (equal (alist-get 'obj parsed) '((x . "y"))))
      (should (equal (alist-get 'zh parsed) "中文"))
      (let ((empty (superchat-bridge-tool-call-arguments (cadr calls))))
        (should (hash-table-p empty))
        (should (equal (json-serialize empty) "{}"))))))

(ert-deftest superchat-llm-rebuilt-empty-arguments-reach-the-provider-as-an-object ()
  ;; the same through the real DeepSeek request body
  (let* ((requests nil)
         (process-environment (list "DEEPSEEK_API_KEY=dummy" "HOME=/nonexistent"))
         (superchat-bridge-http-start-function
          (lambda (_args _config stdin _out _exit)
            (push stdin requests) #'ignore))
         (superchat-llm-providers '(("deepseek" . superchat-bridge-deepseek-provider)))
         (superchat-llm-provider "deepseek") (superchat-llm-model "deepseek-flash")
         (superchat-llm-reasoning nil) (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request))
    (test-llm--ask (test-llm--replay "c1"))
    (test-llm--ask (list (test-llm--user "hi")
                         (list :role 'assistant :text ""
                               :calls (list (list :id "c1" :name "t" :args nil)))
                         (list :role 'tool :call-id "c1" :name "t" :text "r")))
    (let* ((body (json-parse-string (car requests) :object-type 'alist))
           (assistant (cl-find "assistant" (append (alist-get 'messages body) nil)
                               :key (lambda (m) (alist-get 'role m)) :test #'equal)))
      (should (equal (alist-get 'arguments
                                (alist-get 'function (aref (alist-get 'tool_calls assistant) 0)))
                     "{}")))))

(defun test-llm--two-responses-one-turn (id)
  "Two responses of one turn whose first call is ID each; return both
finished messages and the messages the third request sent."
  (test-llm-with (list (list :thinking "T1" :calls `((,id "read_file" nil)))
                       (list :thinking "T2" :calls `((,id "read_file" nil)))
                       '(:text "fin"))
    (let (first second ids)
      (let ((r (test-llm--ask (list (test-llm--user "hi")))))
        (setq first test-llm--last)
        (push (plist-get (car (plist-get (car (plist-get r :done)) :calls)) :id) ids))
      (let ((r (test-llm--ask (test-llm--replay (car ids)))))
        (setq second test-llm--last)
        (push (plist-get (car (plist-get (car (plist-get r :done)) :calls)) :id) ids))
      (test-llm--ask
       (list (test-llm--user "hi")
             (list :role 'assistant :text ""
                   :calls (list (list :id (nth 1 ids) :name "read_file" :args nil)))
             (list :role 'tool :call-id (nth 1 ids) :name "read_file" :text "r1")
             (list :role 'assistant :text ""
                   :calls (list (list :id (car ids) :name "read_file" :args nil)))
             (list :role 'tool :call-id (car ids) :name "read_file" :text "r2")))
      (list first second (nreverse ids) (test-llm--assistants)))))

(ert-deftest superchat-llm-empty-provider-ids-get-generated-ids-each-replay-its-own ()
  (pcase-let ((`(,first ,second ,ids ,sent) (test-llm--two-responses-one-turn "")))
    (should (cl-every (lambda (id) (and (stringp id) (not (string-empty-p id)))) ids))
    (should-not (equal (car ids) (cadr ids)))
    ;; Tape-side id, the kept struct's id: one and the same
    (should (equal (superchat-bridge-tool-call-id (test-llm--tool-call-of first)) (car ids)))
    (should (equal (superchat-bridge-tool-call-id (test-llm--tool-call-of second)) (cadr ids)))
    (should (equal (mapcar #'test-llm--thinking-of sent) '("T1" "T2")))
    (should (eq (car sent) first))
    (should (eq (cadr sent) second))))

(ert-deftest superchat-llm-same-provider-id-twice-each-replay-its-own ()
  ;; "call_0" in two responses of one turn: the second gets an id of its own.
  (pcase-let ((`(,first ,second ,ids ,sent) (test-llm--two-responses-one-turn "call_0")))
    (should (equal (car ids) "call_0"))
    (should-not (equal (cadr ids) "call_0"))
    (should (equal (mapcar #'test-llm--thinking-of sent) '("T1" "T2")))
    (should (eq (car sent) first))
    (should (eq (cadr sent) second))))

(ert-deftest superchat-llm-kept-message-needs-the-same-names-and-count ()
  (dolist (calls '(((:id "c1" :name "other" :args nil))
                   ((:id "c1" :name "read_file" :args nil) (:id "c2" :name "x" :args nil))))
    (test-llm-with (list '(:thinking "T" :calls (("c1" "read_file" nil)))
                         '(:text "x"))
      (test-llm--ask (list (test-llm--user "hi")))
      (let ((first test-llm--last))
        (test-llm--ask (list (test-llm--user "hi")
                             (list :role 'assistant :text "" :calls calls)))
        (let ((assistant (car (test-llm--assistants))))
          (should-not (eq assistant first))
          (should-not (test-llm--thinking-of assistant))))))
  ;; the kept one has two calls, the Tape one
  (test-llm-with (list '(:thinking "T" :calls (("c1" "read_file" nil) ("c2" "x" nil)))
                       '(:text "x"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((first test-llm--last))
      (test-llm--ask (list (test-llm--user "hi")
                           (list :role 'assistant :text ""
                                 :calls '((:id "c1" :name "read_file" :args nil)))))
      (should-not (eq (car (test-llm--assistants)) first)))))

(ert-deftest superchat-llm-another-conversations-user-turn-keeps-this-ones-message ()
  (test-llm-with (list '(:thinking "TA" :calls (("call_a" "read_file" nil)))
                       '(:text "b")
                       '(:text "a2")
                       '(:text "a3"))
    (test-llm--ask (list (test-llm--user "hi A")))
    (let ((a test-llm--last))
      ;; conversation B starts a turn: its MESSAGES have none of A's calls
      (test-llm--ask (list (test-llm--user "hi B")))
      (should (= (length superchat-llm--kept) 1))
      ;; A's next round still sends the kept struct itself
      (test-llm--ask (test-llm--replay "call_a"))
      (should (eq (car (test-llm--assistants)) a))
      ;; A's own next user turn drops it
      (test-llm--ask (append (test-llm--replay "call_a") (list (test-llm--user "again"))))
      (should-not (eq (car (test-llm--assistants)) a))
      (should-not superchat-llm--kept))))

(ert-deftest superchat-llm-kept-table-holds-at-most-64 ()
  (test-llm-with (cl-loop for i below 70
                          collect (list :calls (list (list (format "id_%d" i) "t" nil))))
    (dotimes (_ 70) (test-llm--ask (list (test-llm--user "hi"))))
    (should (= (length superchat-llm--kept) 64))
    ;; newest first: the oldest six are gone
    (should (equal (caar (car superchat-llm--kept)) '("id_69" . "t")))
    (should-not (assoc '(("id_5" . "t")) superchat-llm--kept))
    (should (assoc '(("id_6" . "t")) superchat-llm--kept))))

(ert-deftest superchat-llm-unknown-reasoning-value-is-a-user-error ()
  (test-llm-with nil
    (let* ((superchat-llm-reasoning 'bogus)
           (err (should-error (test-llm--ask (list (test-llm--user "hi")))
                              :type 'user-error)))
      (dolist (word '("bogus" "minimal" "low" "medium" "high" "xhigh" "max"))
        (should (string-match-p word (cadr err))))
      (should-not test-llm--calls))))

(ert-deftest superchat-llm-missing-api-key-is-a-user-error-before-any-curl ()
  (let* ((started nil)
         (process-environment (list "HOME=/nonexistent"))
         (superchat-bridge-http-start-function
          (lambda (&rest _) (setq started t) #'ignore))
         (superchat-llm-providers '(("deepseek" . superchat-bridge-deepseek-provider)))
         (superchat-llm-provider "deepseek") (superchat-llm-model "deepseek-flash")
         (superchat-llm-reasoning nil) (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request)
         (err (should-error (test-llm--ask (list (test-llm--user "hi"))) :type 'user-error)))
    (should (string-match-p "DEEPSEEK_API_KEY" (cadr err)))
    (should-not started)))

(ert-deftest superchat-llm-default-providers-are-the-bridges ()
  (let ((names (mapcar #'car (default-value 'superchat-llm-providers))))
    (should (member "deepseek" names))
    (should (member "groq" names))
    (should (equal (cdr names) (superchat-bridge-provider-ids)))
    (should (equal (superchat-bridge-provider-id
                    (funcall (cdr (assoc "groq" (default-value 'superchat-llm-providers)))))
                   "groq"))))

(ert-deftest superchat-llm-missing-key-names-the-providers-own-variable ()
  (let* ((started nil)
         (process-environment (list "HOME=/nonexistent"))
         (superchat-bridge-http-start-function
          (lambda (&rest _) (setq started t) #'ignore))
         (superchat-llm-providers (default-value 'superchat-llm-providers))
         (superchat-llm-provider "moonshotai-cn") (superchat-llm-model "kimi-k3")
         (superchat-llm-reasoning nil) (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request)
         (err (should-error (test-llm--ask (list (test-llm--user "hi"))) :type 'user-error)))
    (should (string-match-p "MOONSHOT_API_KEY" (cadr err)))
    (should-not started)))

(ert-deftest superchat-llm-keyless-provider-passes-the-key-check ()
  (let* ((config nil)
         (process-environment (list "HOME=/nonexistent"))
         (superchat-bridge-retry-run-at-time-function (lambda (&rest _) nil))
         (superchat-bridge-retry-cancel-timer-function #'ignore)
         (superchat-bridge-http-start-function
          (lambda (_args cfg _stdin _on-output _on-exit) (setq config cfg) #'ignore))
         (superchat-llm-providers
          (list (cons "local" (lambda ()
                                (superchat-bridge-openai-compatible-provider
                                 :id "local" :base-url "http://localhost:1234/v1"
                                 :models '("m"))))))
         (superchat-llm-provider "local") (superchat-llm-model "m")
         (superchat-llm-reasoning nil) (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request))
    (test-llm--ask (list (test-llm--user "hi")))
    (should (string-match-p "http://localhost:1234/v1/chat/completions" config))))

(ert-deftest superchat-llm-options-carry-the-timeout-and-retries ()
  (test-llm-with (list '(:text "x"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((options (plist-get (car test-llm--calls) :options)))
      (should (equal (plist-get options :timeout-ms) 600000))
      (should (equal (plist-get options :max-retries) 2)))))

(ert-deftest superchat-llm-deepseek-429-retry-after-0-then-200 ()
  (let* ((starts 0) (requests nil) (timers nil)
         (process-environment (list "DEEPSEEK_API_KEY=dummy" "HOME=/nonexistent"))
         (superchat-bridge-retry-run-at-time-function
          ;; the 600 s response timeout is never run; only the short backoff
          (lambda (secs _repeat fn) (when (< secs 100) (push fn timers)) fn))
         (superchat-bridge-retry-cancel-timer-function #'ignore)
         (superchat-bridge-http-start-function
          (lambda (_args _config _stdin on-output on-exit)
            (cl-incf starts)
            (push (list on-output on-exit) requests)
            #'ignore))
         (superchat-llm-providers '(("deepseek" . superchat-bridge-deepseek-provider)))
         (superchat-llm-provider "deepseek") (superchat-llm-model "deepseek-flash")
         (superchat-llm-reasoning nil) (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-request-function #'superchat-llm--request)
         (r (test-llm--ask (list (test-llm--user "hi")))))
    (should (= starts 1))
    (pcase-let ((`(,on-output ,on-exit) (car requests)))
      (funcall on-output
               (encode-coding-string
                (concat "HTTP/1.1 429 Too Many Requests\r\nretry-after: 0\r\n"
                        "Content-Type: application/json\r\n\r\n"
                        "{\"error\":{\"message\":\"slow down\"}}")
                'utf-8))
      (funcall on-exit 0 ""))
    (should-not (plist-get r :done))
    (should-not (plist-get r :errors))
    (let ((end (+ (float-time) 3)))
      (while (and (< starts 2) (< (float-time) end))
        (while timers (funcall (pop timers)))
        (accept-process-output nil 0.01)))
    (should (= starts 2))
    (pcase-let ((`(,on-output ,on-exit) (car requests)))
      (funcall on-output
               (encode-coding-string
                (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
                        "data: {\"id\":\"c\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"hi\"}}]}\n\n"
                        "data: {\"id\":\"c\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n"
                        "data: [DONE]\n\n")
                'utf-8))
      (funcall on-exit 0 ""))
    (test-llm--settle)
    (should-not (plist-get r :errors))
    (should (= (length (plist-get r :done)) 1))
    (should (equal (plist-get (car (plist-get r :done)) :text) "hi"))
    (should (= starts 2))))

(ert-deftest superchat-llm-a-provider-entry-that-signals-is-named-and-isolated ()
  (test-llm-with (list '(:text "ok"))
    (setq superchat-llm-providers
          (cons (cons "broken" (lambda () (error "plugin broke"))) superchat-llm-providers))
    (should (equal (superchat-llm-model-ids) '("model-a" "model-b")))
    (should (equal (plist-get (test-llm--ask (list (test-llm--user "hi"))) :errors) nil))
    (let* ((superchat-llm-provider "broken")
           (err (should-error (superchat-llm-model-ids) :type 'user-error)))
      (should (string-match-p "broken" (cadr err)))
      (should (string-match-p "plugin broke" (cadr err))))))

(defmacro test-llm--with-window-provider (&rest body)
  "BODY with a provider \"cw\" whose models have windows of 8000 and 2000."
  (declare (indent 0))
  `(let ((superchat-llm--models nil)
         (superchat-llm-providers
          (list (cons "cw" (lambda ()
                             (superchat-bridge-openai-compatible-provider
                              :id "cw" :base-url "http://localhost:1/v1"
                              :models '((:id "m8" :context-window 8000)
                                        (:id "m2" :context-window 2000)
                                        (:id "m0" :context-window 0))))))))
     ,@body))

(ert-deftest superchat-llm-context-window ()
  (let ((superchat-llm-provider "deepseek") (superchat-llm-model "deepseek-flash"))
    (should (= (superchat-llm-context-window) 1000000))
    (should-not (superchat-llm-context-window "no-such-model"))
    (let ((superchat-llm-model nil))
      (should-not (superchat-llm-context-window))
      (should (= (superchat-llm-context-window "deepseek-flash") 1000000))))
  (test-llm--with-window-provider
    (let ((superchat-llm-provider "cw") (superchat-llm-model "m2"))
      (should (= (superchat-llm-context-window "m8") 8000))
      (should (= (superchat-llm-context-window) 2000))
      (should-not (superchat-llm-context-window "m0"))
      (should-not (superchat-llm-context-window "nope"))))
  (let ((superchat-llm-provider nil) (superchat-llm-model "deepseek-flash"))
    (should-not (superchat-llm-context-window))
    (should-not (superchat-llm-context-window "deepseek-flash")))
  (let ((superchat-llm-provider "no-such-provider"))
    (should-not (superchat-llm-context-window "deepseek-flash"))))

(ert-deftest superchat-llm-an-entry-that-signalled-is-tried-again ()
  (test-llm-with nil
    (let ((ready nil) (superchat-llm-provider "flaky"))
      (setq superchat-llm-providers
            (list (cons "flaky" (lambda ()
                                  (unless ready (error "credentials not ready"))
                                  (test-llm--provider "flaky")))))
      (should-error (superchat-llm-model-ids) :type 'user-error)
      (setq ready t)
      ;; Fixed: the model ids see it at once, with no request first.
      (should (equal (superchat-llm-model-ids) '("model-a" "model-b"))))))

(ert-deftest superchat-llm-a-failed-entry-is-tried-once-a-call ()
  (test-llm-with (list '(:text "a") '(:text "b"))
    (let ((tries 0))
      (setq superchat-llm-providers
            (cons (cons "broken" (lambda () (cl-incf tries) (error "plugin broke")))
                  superchat-llm-providers))
      (test-llm--ask (list (test-llm--user "hi")))
      (should (= tries 1))
      (superchat-llm-context-window)
      (should (= tries 2))
      (superchat-llm-model-ids)
      (should (= tries 3))
      (test-llm--ask (list (test-llm--user "hi")))
      (should (= tries 4)))))

(ert-deftest superchat-llm-a-key-resolver-that-signals-goes-to-on-error ()
  (test-llm-with nil
    (setq superchat-llm-providers
          (list (cons "stub" (lambda ()
                               (test-llm--provider
                                nil (lambda (&rest _) (error "Vault locked")))))))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should (= (length (plist-get r :errors)) 1))
      (should (string-match-p "Vault locked" (car (plist-get r :errors))))
      (should-not (plist-get r :done))
      (should-not test-llm--calls))))

(ert-deftest superchat-llm-an-id-an-earlier-message-has-is-made-anew ()
  ;; A's call_0 is no longer kept after the next user turn; B counts from
  ;; call_0 again and must not be replayed in A's place.
  (test-llm-with (list '(:thinking "TA" :calls (("call_0" "read_file" nil)))
                       '(:thinking "TB" :calls (("call_0" "read_file" nil)))
                       '(:text "fin"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let* ((a-turn (append (test-llm--replay "call_0") (list (test-llm--user "again"))))
           (r (test-llm--ask a-turn))
           (b test-llm--last)
           (id (plist-get (car (plist-get (car (plist-get r :done)) :calls)) :id)))
      (should-not (equal id "call_0"))
      (test-llm--ask (append a-turn
                             (list (list :role 'assistant :text ""
                                         :calls (list (list :id id :name "read_file" :args nil)))
                                   (list :role 'tool :call-id id :name "read_file" :text "r"))))
      (let ((sent (test-llm--assistants)))
        (should (= (length sent) 2))
        (should-not (eq (car sent) b))
        (should (equal (superchat-bridge-tool-call-id (test-llm--tool-call-of (car sent)))
                       "call_0"))
        (should (eq (cadr sent) b))))))

(ert-deftest superchat-llm-one-id-twice-in-a-response-is-made-anew ()
  (test-llm-with (list '(:calls (("call_0" "read_file" nil) ("call_0" "read_file" nil))))
    (let* ((r (test-llm--ask (list (test-llm--user "hi"))))
           (ids (mapcar (lambda (c) (plist-get c :id))
                        (plist-get (car (plist-get r :done)) :calls))))
      (should (equal (car ids) "call_0"))
      (should-not (equal (cadr ids) "call_0")))))

(provide 'test-llm)
;;; test-llm.el ends here

(ert-deftest superchat-llm-first-entry-of-a-name-counts ()
  (test-llm-with nil
    (let ((mk (lambda (m)
                (lambda ()
                  (superchat-bridge-openai-compatible-provider
                   :id "dup" :base-url "http://h/v1" :models (list m)))))
          (superchat-llm-provider "dup"))
      (setq superchat-llm-providers
            (list (cons "dup" (funcall mk "first")) (cons "dup" (funcall mk "second"))))
      (should (equal (superchat-llm-model-ids) '("first"))))))

(ert-deftest superchat-llm-changed-entry-is-seen-at-the-next-request ()
  (test-llm-with nil
    (let ((entry (cons "dup" (lambda () (test-llm--provider "dup"))))
          (superchat-llm-provider "dup"))
      (setq superchat-llm-providers (list entry))
      (should (equal (superchat-llm-model-ids) '("model-a" "model-b")))
      (setcdr entry (lambda ()
                      (superchat-bridge-openai-compatible-provider
                       :id "dup" :base-url "http://h/v1" :models '("z"))))
      (should (equal (superchat-llm-model-ids) '("z"))))))

(ert-deftest superchat-llm-missing-key-without-variable-names-is-a-user-error ()
  (let* ((process-environment (list "HOME=/nonexistent"))
         (mk (lambda (id auth)
               (cons id (lambda ()
                          (superchat-bridge-create-provider
                           :id id :base-url "http://h/v1" :auth auth
                           :models (list (superchat-bridge-make-model
                                          :type 'chat :id "m" :name "m"
                                          :api "openai-completions" :provider id
                                          :base-url "http://h/v1" :input '("text")
                                          :cost (superchat-bridge-make-model-cost)
                                          :context-window 128000 :max-tokens 16384))
                           :api (superchat-bridge-openai-completions-api))))))
         (superchat-llm-providers
          (list (funcall mk "p1" (superchat-bridge-make-provider-auth
                                  :api-key (superchat-bridge-make-api-key-auth
                                            :name "k" :resolve (lambda (&rest _) nil))))
                (funcall mk "p2" (superchat-bridge-make-provider-auth))))
         (superchat-llm-model "m") (superchat-llm--models nil) (superchat-llm--kept nil)
         (superchat-llm-reasoning nil))
    (dolist (id '("p1" "p2"))
      (let* ((superchat-llm-provider id)
             (err (should-error (superchat-llm--request
                                 (list (test-llm--user "hi"))
                                 :on-done #'ignore :on-error #'ignore)
                                :type 'user-error)))
        (should (string-match-p id (cadr err)))
        (should-not (string-match-p "set  in\\|set  or" (cadr err)))))))
