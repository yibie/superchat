;;; test-llm.el --- tests for superchat-llm.el -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'json)
(require 'superchat-llm)
;; Loaded here so that the `let' of their variables binds them dynamically.
(require 'llm-pi-ai-http)
(require 'llm-pi-ai-utils-retry)

;; The provider is a real pi-ai provider whose API is a stub: it records what
;; it was given and returns the event stream the script says, at once or from
;; a timer.  Script entry: (:text S :thinking S :calls ((ID NAME ARGS)...)
;; :deltas LIST :stop S :error S :later BOOL :raise BOOL).

(defvar test-llm--calls nil "What the stub API received, newest first.")
(defvar test-llm--script nil "Entries of the stub's answers, in order.")
(defvar test-llm--last nil "The final assistant message the stub made last.")

(defun test-llm--message (model spec)
  "The final assistant message of MODEL that script entry SPEC describes."
  (llm-pi-ai-make-assistant-message
   :content (append
             (when-let* ((thinking (plist-get spec :thinking)))
               (list (llm-pi-ai-make-thinking-content
                      :thinking thinking :thinking-signature "reasoning_content")))
             (when-let* ((text (plist-get spec :text)))
               (list (llm-pi-ai-make-text-content :text text)))
             (mapcar (pcase-lambda (`(,id ,name ,args))
                       (llm-pi-ai-make-tool-call :id id :name name :arguments args))
                     (plist-get spec :calls)))
   :api (llm-pi-ai-model-api model) :provider (llm-pi-ai-model-provider model)
   :model (llm-pi-ai-model-id model)
   :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason (or (plist-get spec :stop) (if (plist-get spec :calls) "toolUse" "stop"))
   :error-message (plist-get spec :error)
   :timestamp 1))

(defun test-llm--emit (stream message deltas)
  "Push the events of MESSAGE, with text DELTAS, to STREAM and end it."
  (llm-pi-ai-event-stream-push stream (llm-pi-ai-make-event :type "start" :partial message))
  (dolist (delta deltas)
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "text_delta" :content-index 0
                                  :delta delta :partial message)))
  (let ((reason (llm-pi-ai-assistant-message-stop-reason message)))
    (llm-pi-ai-event-stream-push
     stream (if (member reason '("error" "aborted"))
                (llm-pi-ai-make-event :type "error" :reason reason :error message)
              (llm-pi-ai-make-event :type "done" :reason reason :message message))))
  (llm-pi-ai-event-stream-end stream message))

(defun test-llm--respond (model context options)
  "The stub API: record the request, answer from `test-llm--script'."
  (push (list :model model :context context :options options) test-llm--calls)
  (let* ((spec (pop test-llm--script))
         (stream (llm-pi-ai-create-assistant-message-event-stream))
         (message (test-llm--message model spec))
         (deltas (or (plist-get spec :deltas)
                     (and (plist-get spec :text) (list (plist-get spec :text))))))
    (when (plist-get spec :raise) (error "Boom at start"))
    (setq test-llm--last message)
    (if (plist-get spec :later)
        (run-at-time 0 nil #'test-llm--emit stream message deltas)
      (test-llm--emit stream message deltas))
    stream))

(defun test-llm--provider (&optional id)
  "A pi-ai provider ID (default \"stub\") with two models and the stub API."
  (llm-pi-ai-create-provider
   :id (or id "stub") :name "Stub" :base-url "http://stub.test"
   :auth (llm-pi-ai-make-provider-auth
          :api-key (llm-pi-ai-make-api-key-auth
                    :name "none" :resolve (lambda (_c _k _s) (list :auth nil))))
   :models (mapcar (lambda (model)
                     (llm-pi-ai-make-model
                      :id model :name model :api "test-api" :provider (or id "stub")
                      :base-url "http://stub.test" :input '("text")
                      :reasoning t :cost (llm-pi-ai-make-model-cost)
                      :context-window 10000 :max-tokens 1000))
                   '("model-a" "model-b"))
   :api (llm-pi-ai-make-provider-streams :stream #'test-llm--respond
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
  (llm-pi-ai-context-messages (plist-get (nth (or n 0) test-llm--calls) :context)))

(defun test-llm--args-hash ()
  "Arguments as pi-ai parses them: a hash table with nested object, array, false."
  (llm-pi-ai-json-object "path" "a.el" "count" 5
                         "opts" (llm-pi-ai-json-object "deep" t "off" :false)
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
  (let ((bad (llm-pi-ai-make-tool-call :id "c1" :name "t" :arguments (llm-pi-ai-json-object)
                                       :arguments-error "not an object")))
    (test-llm-with (list '(:text "x"))
      (cl-letf (((symbol-function 'test-llm--message)
                 (let ((orig (symbol-function 'test-llm--message)))
                   (lambda (model spec)
                     (let ((m (funcall orig model spec)))
                       (setf (llm-pi-ai-message-content m) (list bad)
                             (llm-pi-ai-assistant-message-stop-reason m) "toolUse")
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
      (should (llm-pi-ai-abort-signal-aborted-p
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
      (should-not (llm-pi-ai-abort-signal-aborted-p
                   (plist-get (plist-get (car test-llm--calls) :options) :signal))))))

(ert-deftest superchat-llm-start-time-signal-goes-to-on-error ()
  (test-llm-with (list '(:raise t))
    (let ((r (test-llm--ask (list (test-llm--user "hi")))))
      (should (= (length (plist-get r :errors)) 1))
      (should (string-match-p "Boom at start" (car (plist-get r :errors))))
      (should-not (plist-get r :done))))
  ;; a message pi-ai cannot be given fails inside the module, same route
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
      (should (equal (llm-pi-ai-model-id (plist-get (car test-llm--calls) :model))
                     "model-b")))
    (test-llm--ask (list (test-llm--user "hi")))
    (should (equal (llm-pi-ai-model-id (plist-get (car test-llm--calls) :model))
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
           (tools (llm-pi-ai-system-message-tools-added system)))
      (should (equal (mapcar #'llm-pi-ai-tool-name tools) '("read_file" "list_buffers")))
      (should (equal (llm-pi-ai-tool-description (car tools)) "Read a file"))
      (should (equal (llm-pi-ai-tool-parameters (car tools))
                     '((type . "object")
                       (properties (path (type . "string") (description . "File"))
                                   (count (type . "number") (description . "Lines"))
                                   (all (type . "boolean") (description . "")))
                       (required . ["path" "all"]))))
      (should (equal (llm-pi-ai-tool-parameters (cadr tools))
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
             (assistant (cl-find-if #'llm-pi-ai-assistant-message-p sent)))
        (should (eq assistant first))
        (should (cl-some #'llm-pi-ai-thinking-content-p
                         (llm-pi-ai-message-content assistant)))
        (should (cl-some #'llm-pi-ai-tool-result-message-p sent))))))

(ert-deftest superchat-llm-kept-message-is-dropped-after-a-user-message ()
  (test-llm-with (list '(:text "ok" :thinking "I should read"
                         :calls (("call_1" "read_file" nil)))
                       '(:text "next"))
    (test-llm--ask (list (test-llm--user "hi")))
    (let ((first test-llm--last))
      (test-llm--ask (append (test-llm--replay "call_1") (list (test-llm--user "again"))))
      (let ((assistant (cl-find-if #'llm-pi-ai-assistant-message-p (test-llm--sent))))
        (should-not (eq assistant first))
        (should-not (cl-some #'llm-pi-ai-thinking-content-p
                             (llm-pi-ai-message-content assistant)))
        (should (equal (llm-pi-ai-tool-call-id
                        (cl-find-if #'llm-pi-ai-tool-call-p
                                    (llm-pi-ai-message-content assistant)))
                       "call_1"))))))

(ert-deftest superchat-llm-unkept-assistant-is-built-as-the-models-own ()
  (test-llm-with (list '(:text "x"))
    (test-llm--ask (test-llm--replay "call_7"))
    (let* ((model (plist-get (car test-llm--calls) :model))
           (assistant (cl-find-if #'llm-pi-ai-assistant-message-p (test-llm--sent)))
           (call (cl-find-if #'llm-pi-ai-tool-call-p (llm-pi-ai-message-content assistant))))
      (should (equal (list (llm-pi-ai-assistant-message-api assistant)
                           (llm-pi-ai-assistant-message-provider assistant)
                           (llm-pi-ai-assistant-message-model assistant)
                           (llm-pi-ai-assistant-message-stop-reason assistant))
                     (list (llm-pi-ai-model-api model) "stub" "model-a" "toolUse")))
      (should (equal (llm-pi-ai-tool-call-name call) "read_file"))
      (should (equal (llm-pi-ai-text-content-text
                      (car (llm-pi-ai-message-content (cl-find-if #'llm-pi-ai-tool-result-message-p
                                                                  (test-llm--sent)))))
                     "contents")))))

;; The path a real failure came through: DeepSeek asks for its reasoning back
;; together with the tool call it led to.

(ert-deftest superchat-llm-deepseek-gets-its-reasoning-back ()
  (let* ((requests nil)
         (process-environment (list "DEEPSEEK_API_KEY=dummy" "HOME=/nonexistent"))
         (llm-pi-ai-retry-run-at-time-function (lambda (&rest _) nil))
         (llm-pi-ai-retry-cancel-timer-function #'ignore)
         (llm-pi-ai-http-start-function
          (lambda (_args _config stdin on-output on-exit)
            (setq requests (append requests (list (list stdin on-output on-exit))))
            #'ignore))
         (superchat-llm-providers '(("deepseek" . llm-pi-ai-deepseek-provider)))
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

(provide 'test-llm)
;;; test-llm.el ends here
