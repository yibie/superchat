;;; request-evidence-regression.el --- a message list names its messages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "a message list names its messages"
;; (docs/decisions/accepted/2026-11-09-a-message-list-names-its-messages.md).
;;
;; A `request.message-list' row copied the whole text of every earlier user
;; and assistant message into the list of every request that carried it.  In
;; a long conversation the row grew past `superchat-tape-max-inline-bytes',
;; the Tape kept its size and hash only, and `prompt-reconstructable'
;; recorded "no message-list row for request" for every later request (50
;; such rows in one conversation of 2026-11-05).  A raw fact and a compaction
;; Anchor of the Session are now recorded by the row they name; their text is
;; read from that row when the request is rebuilt.
;;
;; The conversation runs through the canonical fixture (scripts/smoke.el)
;; with a scripted model and the real SQLite Tape.  Every provider request is
;; captured at the llm boundary and serialized with llm.el's own
;; OpenAI-compatible request builder, so "the request that was sent" is the
;; wire message list, not superchat's internals.
;;
;; Run from the repository root with the prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/request-evidence-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^request-evidence/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)

(when (getenv "SMOKE_REAL")
  (error "Request-evidence regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar re--wires nil
  "Provider requests of the current test, newest first, as wire messages.")

(defun re--wire-of (provider prompt)
  "Return PROMPT as the message list llm.el sends to an OpenAI-style PROVIDER."
  (let* ((request (llm-provider-chat-request provider prompt nil))
         (messages (or (plist-get request :messages)
                       (alist-get 'messages request))))
    (json-parse-string (json-encode messages)
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defmacro re--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical fixture, capturing wire messages."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil)
         (re--wires nil)
         (target (get-buffer-create "re-target")))
     (with-current-buffer target
       (erase-buffer)
       (insert "RE-TARGET-TEXT\n"))
     (unwind-protect
         (smoke--with-fixture ,script
           (let ((network (symbol-function 'smoke--fake-network)))
             (cl-letf (((symbol-function 'smoke--fake-network)
                        (lambda (mode provider prompt partial response error multi)
                          (push (re--wire-of provider prompt) re--wires)
                          (funcall network mode provider prompt
                                   partial response error multi))))
               ,@body)))
       (setq superchat-llm-tools-list nil)
       (when (buffer-live-p target) (kill-buffer target)))))

(defun re--turn (view text)
  "Send TEXT from VIEW and wait until its Run completed; return the Run id."
  (let ((run (smoke--send view text)))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    run))

(defun re--session (view)
  (buffer-local-value 'superchat--session-id view))

(defun re--events (session type)
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events session)))

(defun re--body (wire)
  "WIRE without its leading system message."
  (if (equal "system" (alist-get 'role (car wire))) (cdr wire) wire))

(defun re--text (message)
  "MESSAGE's content as a string (empty when it has none)."
  (let ((content (alist-get 'content message)))
    (if (stringp content) content "")))

(defun re--long (marker)
  "Return MARKER followed by 4500 characters, a long message."
  (concat marker " " (make-string 4500 ?x)))

(defun re--sent-equals-rebuilt (body rebuilt)
  "Assert REBUILT, the history a rebuild returns, is BODY's leading messages.
Each message is compared by role and text; an assistant message with tool
calls by its call ids and names, a tool message by its call id and text."
  (should (> (length body) (length rebuilt)))
  (cl-loop for message in rebuilt
           for wire in body
           do (should (equal (plist-get message :kind) (alist-get 'role wire)))
           do (cond
               ((plist-get message :tool-calls)
                (should (equal (mapcar (lambda (call) (plist-get call :id))
                                       (plist-get message :tool-calls))
                               (mapcar (lambda (call) (alist-get 'id call))
                                       (alist-get 'tool_calls wire))))
                (should (equal (mapcar (lambda (call) (plist-get call :name))
                                       (plist-get message :tool-calls))
                               (mapcar (lambda (call)
                                         (alist-get 'name (alist-get 'function call)))
                                       (alist-get 'tool_calls wire)))))
               ((equal "tool" (plist-get message :kind))
                (should (equal (plist-get message :tool-call-id)
                               (alist-get 'tool_call_id wire)))
                ;; llm.el's OpenAI serializer frames every tool result as
                ;; "Result of tool call is TEXT".
                (should (string-suffix-p (plist-get message :content)
                                         (re--text wire))))
               (t (should (equal (plist-get message :content) (re--text wire))))))
  ;; What follows the history is the turn's own question.
  (should (equal "user" (alist-get 'role (nth (length rebuilt) body)))))

;;;; A long conversation

(defun re--long-conversation ()
  "Run four turns whose earlier messages total more than the inline limit.
The first turn reads re-target once.  Return the chat View."
  (setq smoke--script
        (list `(:tool-uses (("read_buffer" (buffer . "re-target"))))
              (list :text (re--long "RE-ANSWER-1"))
              (list :text (re--long "RE-ANSWER-2"))
              (list :text (re--long "RE-ANSWER-3"))
              (list :text "RE-ANSWER-4")))
  (let ((view (smoke--open-chat)))
    (re--turn view (re--long "RE-ASK-1 read re-target"))
    (re--turn view (re--long "RE-ASK-2"))
    (re--turn view (re--long "RE-ASK-3"))
    (re--turn view "RE-ASK-4")
    view))

(ert-deftest request-evidence/a-long-conversation-keeps-every-message-list ()
  "Every request's message list is stored whole, although the user and
assistant messages the last request carries total more than the limit, and
`prompt-reconstructable' records no violation."
  (re--fixture nil
    (let* ((session (re--session (re--long-conversation)))
           (lists (re--events session "request.message-list"))
           (last-body (re--body (car re--wires))))
      ;; The premise: the last request's user and assistant texts alone are
      ;; over the limit, so a list that copied them could not be kept.
      (should (> (apply #'+ (mapcar (lambda (message)
                                      (string-bytes (re--text message)))
                                    last-body))
                 superchat-tape-max-inline-bytes))
      (should (= (length re--wires) (length lists)))
      (dolist (event lists)
        (let ((payload (plist-get event :payload)))
          (should-not (plist-get payload :omitted))
          (should (stringp (plist-get payload :request-id)))))
      (should-not (re--events session "invariant.violated"))
      (should-not (re--events session "invariant.check-failed")))))

(ert-deftest request-evidence/every-request-rebuilds-as-it-was-sent ()
  "Each request rebuilt from the Tape equals the request that was sent,
message by message: the earlier user and assistant messages, the earlier tool
call and its result, then the turn's own question."
  (re--fixture nil
    (let* ((session (re--session (re--long-conversation)))
           (lists (re--events session "request.message-list"))
           (wires (reverse re--wires)))
      (should (= (length wires) (length lists)))
      (cl-loop for event in lists
               for wire in wires
               for payload = (plist-get event :payload)
               for evidence = (superchat-tape-request-evidence
                               session (plist-get payload :request-id)
                               (plist-get (plist-get payload :execution) :step))
               do (should (eq 'reproduced (plist-get evidence :status)))
               do (re--sent-equals-rebuilt (re--body wire)
                                           (plist-get evidence :messages)))
      ;; The last request carries all three long turns.
      (let ((last (plist-get (superchat-tape-request-evidence
                              session (plist-get (plist-get (car (last lists)) :payload)
                                                 :request-id))
                             :messages)))
        (should (equal '("user" "assistant" "tool" "assistant" "user" "assistant"
                         "user" "assistant")
                       (mapcar (lambda (message) (plist-get message :kind)) last)))))))

;;;; Each item kind, recorded by identity and in the old shape

(defun re--old-items (session)
  "Append one row of each kind a history item names to SESSION.
Return the items as the Context View hands them to a request: with their
text, the shape a message list recorded before 2026-11-09."
  (let* ((user (superchat-tape-append-material session "user" "RE-OLD-USER"))
         (answer (superchat-tape-append-material session "assistant" "RE-OLD-ANSWER"))
         (anchor (superchat-tape-append-compaction-anchor
                  session "RE-SUMMARY" (vector user answer)))
         (note (superchat-tape-append-material session "system" "RE-SYSTEM-FACT"))
         (material (superchat-runtime-admit-between-turns
                    session '(:kind "user-save" :buffer "re-target") "RE-SAVED"))
         (user-2 (superchat-tape-append-material session "user" "RE-USER-2"))
         (answer-2 (superchat-tape-append-material session "assistant" "RE-ANSWER-2")))
    (list (list :class 'model-interpretation :storage-id anchor
                :source-ids (vector user answer) :lossy t :content "RE-SUMMARY")
          (list :class 'raw-fact :session-id session :storage-id note
                :kind "system" :content "RE-SYSTEM-FACT")
          (list :class 'raw-fact :session-id session :storage-id material
                :kind "user"
                :content (superchat-tape-injected-material-line "RE-SAVED")
                :event-id "evt-re" :source '(:kind "user-save" :buffer "re-target")
                :input-ids nil)
          (list :class 'raw-fact :session-id session :storage-id user-2
                :kind "user" :content "RE-USER-2")
          (list :class 'raw-fact :session-id session :storage-id answer-2
                :kind "assistant" :content "RE-ANSWER-2"))))

(ert-deftest request-evidence/each-item-kind-is-named-and-rebuilds-the-same ()
  "A compaction Anchor, a system fact, admitted material and a user and an
assistant message are recorded by the row they name, without their text, and
the request rebuilds to the messages the provider received.  A list written
before this change, whose items carry their text, rebuilds to the same."
  (re--fixture nil
    (let* ((session (re--session (smoke--open-chat)))
           (items (re--old-items session))
           (sent (superchat-tape-history-messages
                  items (superchat-tape-step-key-function session)))
           (header (superchat-tape-request-record
                    session "RE-NEW" "system" [] "model" items nil))
           (new (car (last (re--events session "request.message-list")))))
      (should (integerp header))
      ;; The old shape, written as a schema 1 row did: every item whole.
      (superchat-tape-append-event
       (list :type "request.message-list" :session-id session :actor-id "kernel"
             :summary "request.message-list: old"
             :payload (list :request-id "RE-OLD" :series-id (format "series-%s" session)
                            :header-storage-id header :schema 1 :template-version 2
                            :items (vconcat items) :receipt nil :execution nil)))
      (dolist (item (append (plist-get (plist-get new :payload) :items) nil))
        (should (integerp (plist-get item :storage-id)))
        (should-not (plist-member item :content)))
      (should (equal '(("model-interpretation" nil) ("raw-fact" "system")
                       ("raw-fact" "user") ("raw-fact" "user") ("raw-fact" "assistant"))
                     (mapcar (lambda (item)
                               (list (format "%s" (plist-get item :class))
                                     (plist-get item :kind)))
                             (append (plist-get (plist-get new :payload) :items) nil))))
      (let ((rebuilt-new (superchat-tape-request-evidence session "RE-NEW"))
            (rebuilt-old (superchat-tape-request-evidence session "RE-OLD")))
        (should (eq 'reproduced (plist-get rebuilt-new :status)))
        (should (eq 'reproduced (plist-get rebuilt-old :status)))
        (should (equal sent (plist-get rebuilt-new :messages)))
        (should (equal sent (plist-get rebuilt-old :messages)))
        (should (equal "[Summary of the earlier conversation, written by a model] RE-SUMMARY"
                       (plist-get (car sent) :content)))))))

(ert-deftest request-evidence/an-item-of-another-session-keeps-its-text ()
  "Guard (green before and after): the rebuild reads rows of the request's own
Session only, so an item naming another Session's row, or naming no row, is
recorded with its text."
  (re--fixture nil
    (let ((session (re--session (smoke--open-chat))))
      (superchat-tape-request-record
       session "RE-OTHER" "system" [] "model"
       (list (list :class 'raw-fact :session-id "elsewhere" :storage-id 7
                   :kind "user" :content "RE-ELSEWHERE")
             (list :kind "user" :content "RE-NO-ROW"))
       nil)
      (should (equal '("RE-ELSEWHERE" "RE-NO-ROW")
                     (mapcar (lambda (item) (plist-get item :content))
                             (append (plist-get (plist-get (car (last (re--events session "request.message-list")))
                                                           :payload)
                                                :items)
                                     nil)))))))

;;;; A list still over the limit

(ert-deftest request-evidence/a-list-over-the-limit-is-reported-with-its-run ()
  "A message list still over the limit cannot be rebuilt, and the check says
so; the violation names the Run whose request it is."
  (re--fixture (list '(:text "RE-FIRST") '(:text "RE-SECOND"))
    (let* ((view (smoke--open-chat))
           (session (re--session view)))
      (re--turn view "RE-ASK-FIRST")
      ;; A limit just above an empty list, so the second request's two
      ;; named items are already too many.
      (let* ((superchat-tape-max-inline-bytes 450)
             (run (re--turn view "RE-ASK-SECOND"))
             (violations (re--events session "invariant.violated")))
        (should (plist-get (plist-get (car (last (re--events session "request.message-list")))
                                      :payload)
                           :omitted))
        (should (= 1 (length violations)))
        (should (equal run (plist-get (car violations) :run-id)))
        (should (string-match-p "no message-list row for request"
                                (plist-get (plist-get (car violations) :payload)
                                           :detail)))))))

;;;; The check rebuilds the attempt it follows

(ert-deftest request-evidence/the-check-rebuilds-the-attempt-it-follows ()
  "A Run's second attempt whose list is over the limit is reported, although
the Run's first attempt, under the same request id, was stored whole: the
check rebuilds the attempt of its own Step, not the newest whole list."
  (re--fixture (list '(:tool-uses (("read_buffer" (buffer . "re-target"))))
                     '(:text "RE-AFTER-READ"))
    (let* ((view (smoke--open-chat))
           (session (re--session view))
           (append-event (symbol-function 'superchat-tape-append-event))
           (lists-seen 0))
      ;; Only the second message list meets a limit it is over.
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (and (equal "request.message-list" (plist-get event :type))
                            (> (cl-incf lists-seen) 1))
                       (let ((superchat-tape-max-inline-bytes 1))
                         (funcall append-event event))
                     (funcall append-event event)))))
        (let* ((run (re--turn view "RE-ASK read re-target"))
               (lists (re--events session "request.message-list"))
               (violations (re--events session "invariant.violated")))
          ;; The premise: two attempts, the first whole, the second a hash.
          (should (= 2 (length lists)))
          (should-not (plist-get (plist-get (car lists) :payload) :omitted))
          (should (plist-get (plist-get (cadr lists) :payload) :omitted))
          (should (= 1 (length violations)))
          (should (equal run (plist-get (car violations) :run-id)))
          (should (string-match-p "no message-list row for request"
                                  (plist-get (plist-get (car violations) :payload)
                                             :detail))))))))

;;;; A rebuilt request carries the texts that were sent

(defun re--pair ()
  "An emoji as telega holds it: the two halves of a surrogate pair."
  (string #xD83D #xDEA9))

(ert-deftest request-evidence/a-rebuilt-request-carries-the-text-that-was-sent ()
  "The user typed \"ab\" and the two halves of a pair.  The next request sent
the earlier turn as \"ab\" and the one character, because the transport
repairs every text before an attempt; that request rebuilt from the Tape
carries the same text, which `json-serialize' takes.  The Tape row keeps what
the user typed."
  (re--fixture (list '(:text "RE-FIRST") '(:text "RE-SECOND"))
    (let* ((view (smoke--open-chat))
           (session (re--session view)))
      (re--turn view (concat "ab" (re--pair)))
      (re--turn view "cd")
      (let* ((payload (plist-get (car (last (re--events session "request.message-list")))
                                 :payload))
             (evidence (superchat-tape-request-evidence
                        session (plist-get payload :request-id)))
             (rebuilt (car (plist-get evidence :messages)))
             (sent (car (re--body (car re--wires))))
             (row (car (superchat-db-tape-select
                        "SELECT content FROM tape WHERE id = ? AND session_id = ?"
                        (list (plist-get (car (append (plist-get payload :items) nil))
                                         :storage-id)
                              session)))))
        (should (eq 'reproduced (plist-get evidence :status)))
        ;; The premise: the wire carried the earlier turn repaired.
        (should (equal "user" (alist-get 'role sent)))
        (should (equal (string ?a ?b #x1F6A9) (re--text sent)))
        (should (equal "user" (plist-get rebuilt :kind)))
        (should (equal (re--text sent) (plist-get rebuilt :content)))
        (should (json-serialize (vector (plist-get rebuilt :content))))
        ;; The row is as typed, and that is not what was sent.
        (should (stringp (car row)))
        (should-not (equal (re--text sent) (car row)))))))

(ert-deftest request-evidence/a-rebuilt-request-of-sendable-text-is-unchanged ()
  "Guard (green before and after): a request whose texts could all be sent
rebuilds to what the Tape holds, character for character."
  (re--fixture (list '(:text "RE-FIRST") '(:text "RE-SECOND"))
    (let* ((view (smoke--open-chat))
           (session (re--session view)))
      (re--turn view "ab 读书")
      (re--turn view "cd")
      (let* ((request-id (plist-get (plist-get (car (last (re--events session "request.message-list")))
                                               :payload)
                                    :request-id))
             (evidence (superchat-tape-request-evidence session request-id))
             (as-stored (cl-letf (((symbol-function 'superchat-wire-safe-text)
                                   #'identity))
                          (superchat-tape-request-evidence session request-id))))
        (should (eq 'reproduced (plist-get evidence :status)))
        (should (equal "ab 读书" (plist-get (car (plist-get evidence :messages)) :content)))
        (should (equal as-stored evidence))))))

;;;; A Run submitted without a request id

(defun re--submit (request)
  "Submit REQUEST straight to the Runtime and wait for its Run; return the Run id."
  (let* ((handle (superchat-runtime-submit request))
         (run (plist-get handle :run-id)))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun re--request-ids (session run)
  "The request ids RUN's Step rows and message lists in SESSION carry."
  (cl-loop for event in (superchat-tape-replay-events session)
           for payload = (plist-get event :payload)
           when (or (and (member (plist-get event :type) '("step.started" "step.ended"))
                         (equal run (plist-get payload :run-id)))
                    (and (equal "request.message-list" (plist-get event :type))
                         (equal run (plist-get (plist-get payload :execution) :run-id))))
           collect (cons (plist-get event :type) (plist-get payload :request-id))))

(ert-deftest request-evidence/a-run-submitted-without-a-request-id-is-given-one ()
  "A caller outside the pipeline submits a Request without `:request-id'.
The Run is given one: its Step rows and its message list carry the same id,
the request rebuilds under it, and the check records no violation."
  (re--fixture (list '(:text "RE-TAGGED"))
    (let* ((host (get-buffer-create " *re-host*"))
           (run (unwind-protect
                    (re--submit (list :prompt "RE-TAG this" :buffer host
                                      :surface 'supertag
                                      :sink (superchat-sink-discard)))
                  (kill-buffer host)))
           (session (plist-get (superchat-runtime-status run) :session-id))
           (rows (re--request-ids session run))
           (ids (delete-dups (mapcar #'cdr rows))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should-not (re--events session "invariant.violated"))
      (should (equal '("step.started" "request.message-list" "step.ended")
                     (mapcar #'car rows)))
      (should (= 1 (length ids)))
      (should (stringp (car ids)))
      (should-not (equal "request" (car ids)))
      (should (eq 'reproduced
                  (plist-get (superchat-tape-request-evidence session (car ids))
                             :status))))))

(ert-deftest request-evidence/a-request-that-brought-its-id-keeps-it ()
  "Guard (green before and after): a Request with its own `:request-id' keeps it."
  (re--fixture (list '(:text "RE-TAGGED"))
    (let* ((host (get-buffer-create " *re-host*"))
           (run (unwind-protect
                    (re--submit (list :prompt "RE-TAG this" :buffer host
                                      :request-id "RE-OWN-ID"
                                      :surface 'supertag
                                      :sink (superchat-sink-discard)))
                  (kill-buffer host)))
           (session (plist-get (superchat-runtime-status run) :session-id))
           (rows (re--request-ids session run)))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 3 (length rows)))
      (should (equal '("RE-OWN-ID") (delete-dups (mapcar #'cdr rows))))
      (should (eq 'reproduced
                  (plist-get (superchat-tape-request-evidence session "RE-OWN-ID")
                             :status))))))

(provide 'request-evidence-regression)

;;; request-evidence-regression.el ends here
