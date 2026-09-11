;;; test-cowork-telega.el --- Tests for superchat-cowork-telega -*- lexical-binding: t; -*-

;;; Commentary:

;; Plugin-local tests, runnable standalone:
;;
;;   emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
;;     -l plugins/superchat-cowork-telega/test-cowork-telega.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Telega and the model transport are mocked; the Superchat seams the
;; plugin registers into are real.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "../../test/test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-cowork-telega)

(defun superchat-cowork-telega-test--compose-identity
    (request &optional acceptance-function)
  "Return REQUEST after honoring optional ACCEPTANCE-FUNCTION."
  (when acceptance-function
    (funcall acceptance-function request))
  request)

(defun superchat-cowork-telega-test--bundle-el-files ()
  "Return every production Lisp file in the cowork Telega bundle."
  (directory-files
   (file-name-directory (locate-library "superchat-cowork-telega.el"))
   t "\\`superchat-cowork-telega.*\\.el\\'"))

(defmacro superchat-cowork-telega-test--with-telega (sent &rest body)
  "Run BODY with a mocked Telega; sends are pushed onto SENT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'telega-chat-get)
              (lambda (id &optional _offline)
                (when (memq id '(123 999))
                  (list :id id :title (format "Chat %s" id)))))
             ((symbol-function 'telega-chat-title)
              (lambda (chat &optional _fmt _badges)
                (plist-get chat :title)))
             ((symbol-function 'telega-string-fmt-text)
              (lambda (text &optional _markup) text))
             ((symbol-function 'telega--sendMessage)
              (lambda (chat imc &optional _reply _options &rest _keys)
                (push (list :chat-id (plist-get chat :id)
                            :text (plist-get imc :text))
                      ,sent)
                t)))
     ,@body))

(defun superchat-cowork-telega-test--deliver (request text status &optional tape-id)
  "Deliver TEXT with STATUS through REQUEST's callback Sink (S14).
REQUEST is the legacy Request the pipeline built; `completed' goes to the
Sink's `:final', any other status to its `:error'."
  (let ((sink (plist-get request :sink)))
    (should (superchat-sink-p sink))
    (if (eq status 'completed)
        (superchat-sink-emit sink 'final nil (list :text text :tape-id tape-id))
      (superchat-sink-emit sink 'error nil
                           (list :text text :status status :tape-id tape-id)))))

(ert-deftest cowork-telega/uses-only-public-superchat-api ()
  "The bundle must not reference any `superchat--' internal symbol."
  (dolist (file (superchat-cowork-telega-test--bundle-el-files))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "\\_<superchat--" nil t)))))

(ert-deftest cowork-telega/does-not-register-desktop-notifications ()
  "The macOS bundle must not depend on D-Bus notification seams."
  (dolist (file (superchat-cowork-telega-test--bundle-el-files))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (pattern '("\\_<dbus\\_>"
                         "\\_<notifications-notify\\_>"
                         "\\_<telega-notifications-msg-temex\\_>"))
        (goto-char (point-min))
        (should-not (re-search-forward pattern nil t))))))

(ert-deftest cowork-telega/bundle-ships-session-skill ()
  "The bundle carries workflow guidance as a skill resource."
  (let ((skill
         (expand-file-name
          "skills/cowork-telegram/SKILL.md"
          (file-name-directory
           (locate-library "superchat-cowork-telega.el")))))
    (should (file-readable-p skill))
    (with-temp-buffer
      (insert-file-contents skill)
      (should (re-search-forward "understand" nil t))
      (should (re-search-forward "approval" nil t)))))

(ert-deftest cowork-telega/bundle-loads-by-absolute-path ()
  "The bundle resolves sibling modules without a plugin load-path entry."
  (let* ((bundle (locate-library "superchat-cowork-telega.el"))
         (root (file-name-directory (locate-library "superchat.el")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (output (generate-new-buffer " *cowork-telega-load-test*")))
    (unwind-protect
        (let ((status
               (call-process
                emacs nil output nil
                "-Q" "--batch" "-L" root "-l" bundle
                "--eval"
                "(princ (commandp 'superchat-cowork-telega))")))
          (should (= 0 status))
          (with-current-buffer output
            (should (string-suffix-p "t" (string-trim (buffer-string))))))
      (kill-buffer output))))

(ert-deftest cowork-telega/registers-tool-with-declared-effects ()
  "Registration wires the send tool into the Authority effect model."
  (let ((effects (superchat-authority-resolve-effects
                  superchat-cowork-telega-tool-name
                  '("123" "hello"))))
    (should (= 1 (length effects)))
    (should (eq 'external-write (plist-get (car effects) :effect)))
    (should (eq 'message (plist-get (car effects) :resource)))
    (should (equal "telega://chat/123" (plist-get (car effects) :scope)))))

(ert-deftest cowork-telega/tool-is-exposed-on-the-model-surface ()
  "The registered tool appears in a request-local tool profile."
  (skip-unless (fboundp 'llm-make-tool))
  (should (cl-find superchat-cowork-telega-tool-name
                   (superchat-get-llm-tools
                    (list superchat-cowork-telega-tool-name))
                   :key #'llm-tool-name :test #'equal)))

(ert-deftest cowork-telega/send-flows-through-authority-and-tape ()
  "An approved send executes with the full audit chain."
  (let ((sent '())
        (events '())
        (superchat-authority-approval-function (lambda (_request) t)))
    (superchat-cowork-telega-test--with-telega sent
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   1)))
        (let ((result (superchat-tool-execute
                       superchat-cowork-telega-tool-name
                       #'superchat-cowork-telega--send
                       '("123" "hello there")
                       :context '(:session-id "telega-chat-123"
                                  :attendance attended
                                  :authority-policy "cowork"))))
          (should (equal "Sent Telega message to Chat 123" result))
          (should (equal '((:chat-id 123 :text "hello there")) sent))
          (dolist (type '("tool.requested" "approval.requested"
                          "approval.decided" "tool.execution.started"
                          "tool.completed"))
            (should (member type events))))))))

(ert-deftest cowork-telega/refused-approval-blocks-the-send ()
  "A declined approval must record cancellation and send nothing."
  (let ((sent '())
        (events '())
        (superchat-authority-approval-function (lambda (_request) nil)))
    (superchat-cowork-telega-test--with-telega sent
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   1)))
        (let ((result (superchat-tool-execute
                       superchat-cowork-telega-tool-name
                       #'superchat-cowork-telega--send
                       '("123" "hello there")
                       :context '(:session-id "telega-chat-123"
                                  :attendance attended
                                  :authority-policy "cowork"))))
          (should (equal "[Tool call cancelled by user]" result))
          (should (null sent))
          (should (member "tool.failed" events)))))))

(ert-deftest cowork-telega/validator-pins-sends-to-the-request-chat ()
  "The request validator rejects sends targeting another chat."
  (let* ((sent '())
         (request
          (superchat-cowork-telega-test--with-telega sent
            (superchat-cowork-telega--request
             '(:id 123 :title "Chat 123")
             '(:text "context") "reply" #'ignore)))
         (validator (plist-get request :tool-validator))
         (superchat-authority-approval-function (lambda (_request) t)))
    (superchat-cowork-telega-test--with-telega sent
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (let ((result (superchat-tool-execute
                       superchat-cowork-telega-tool-name
                       #'superchat-cowork-telega--send
                       '("999" "wrong chat")
                       :context (list :session-id "telega-chat-123"
                                      :attendance 'attended
                                      :authority-policy "cowork"
                                      :validate validator))))
          (should (stringp result))
          (should (string-match-p "differs from current chat" result))
          (should (null sent)))))))

(ert-deftest cowork-telega/request-shape-and-async-submission ()
  "The composed Request pins cowork semantics and completes asynchronously."
  (let* ((sent '())
         (finish nil)
         (request
          (superchat-cowork-telega-test--with-telega sent
            (superchat-cowork-telega--request
             '(:id 123 :title "Chat 123")
             '(:text "Sender: original message" :message-id 7)
             "reply politely"
             (lambda (response status)
               (setq finish (list response status)))))))
    (should (equal "cowork" (plist-get request :capability)))
    (should (equal "cowork" (plist-get request :authority-policy)))
    (should (eq 'attended (plist-get request :attendance)))
    (should-not (plist-get request :tools))
    (should (equal "telega-chat-123" (plist-get request :session-id)))
    (should (equal "telega://chat/123/message/7"
                   (plist-get (plist-get request :origin) :uri)))
    (should (equal "cowork.telega"
                   (superchat-preset-profile-id
                    (plist-get request :preset))))
    (cl-letf (((symbol-function 'superchat--llm-generate-answer)
               (lambda (_prompt _cb _stream &rest _)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          (list :status 'completed :response "Sent."))))
              ((symbol-function 'superchat-tape-append-event)
               (lambda (_event) 1)))
      (let ((superchat-closure-terminal-guard-enabled nil)
            (composed (superchat-extension-compose-request request)))
        (should (memq 'telega-cowork
                      (plist-get composed :superchat-extensions)))
        (should (member superchat-cowork-telega-tool-name
                        (plist-get composed :tools)))
        (should (member "memory_search" (plist-get composed :tools)))
        (should (string-match-p "Active Superchat extensions"
                                (plist-get composed :system-prompt)))
        (should (string-match-p "telega_send_message exactly once"
                                (plist-get composed :system-prompt)))
        (let ((handle (superchat-pipeline-submit
                       (superchat-pipeline-envelope-from-request composed))))
          (should (eq 'accepted (plist-get handle :status)))
          (should (equal '("Sent." completed) finish))
          (should (eq 'completed
                      (plist-get (superchat-runtime-status
                                  (plist-get handle :run-id))
                                 :status))))))))

(ert-deftest cowork-telega/extension-activates-only-on-telega-surface ()
  "The declared extension composes nothing outside Telega Requests."
  (cl-letf (((symbol-function 'superchat-tape-append-event)
             (lambda (_event) 1)))
    (let ((elsewhere (superchat-extension-compose-request
                      (list :prompt "hello"
                            :session-id "not-telega"
                            :origin '(:surface superchat)))))
      (should-not (memq 'telega-cowork
                        (plist-get elsewhere :superchat-extensions)))
      (should-not (member superchat-cowork-telega-tool-name
                          (or (plist-get elsewhere :tools) '()))))))

(ert-deftest cowork-telega/schedule-request-injects-agenda-context ()
  "A scheduling-flavored request gets the agenda injected deterministically."
  (let ((superchat-cowork-telega-agenda-function
         (lambda () "2026-07-29 14:00 Team sync")))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (_event) 1)))
      (let ((scheduling
             (superchat-extension-compose-request
              (list :prompt "User request:\n周三有空吗"
                    :input "周三有空吗"
                    :session-id "telega-chat-123"
                    :origin '(:surface telega))))
            (ordinary
             (superchat-extension-compose-request
              (list :prompt "User request:\nthanks!"
                    :input "thanks!"
                    :session-id "telega-chat-123"
                    :origin '(:surface telega))))
            (follow-up
             (superchat-extension-compose-request
              (list :prompt
                    "Previous Cowork Session:\nUser: 周三有空吗\n\nCurrent: thanks"
                    :input "thanks!"
                    :session-id "telega-chat-123"
                    :origin '(:surface telega)))))
        (should (string-match-p "\\[telega-cowork context\\]"
                                (plist-get scheduling :prompt)))
        (should (string-match-p "Team sync"
                                (plist-get scheduling :prompt)))
        (should-not (string-match-p "Team sync"
                                    (plist-get ordinary :prompt)))
        (should-not (string-match-p "Team sync"
                                    (plist-get follow-up :prompt)))))))

;; ── S1: autonomy ladder ───────────────────────────────────────

(ert-deftest cowork-telega/draft-autonomy-composes-text-only ()
  "Draft autonomy carries an empty tool surface that stays empty."
  (let ((sent '()))
    (superchat-cowork-telega-test--with-telega sent
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (let* ((request (superchat-cowork-telega--request
                         '(:id 123 :title "Chat 123")
                         '(:text "context") "reply" #'ignore 'draft))
               (composed (superchat-extension-compose-request request)))
          (should (eq 'none (plist-get request :tools)))
          ;; Activation composition must not widen the explicit surface.
          (should (eq 'none (plist-get composed :tools)))
          (should (string-match-p
                   "Return ONLY the complete reply text"
                   (superchat-preset-skill-body
                    (plist-get composed :preset)))))))))

(ert-deftest cowork-telega/draft-lands-in-the-input-area ()
  "A finished draft is inserted into the originating chatbuf."
  (let ((inserted '()))
    (with-temp-buffer
      (cl-letf (((symbol-function 'telega-chatbuf-input-insert)
                 (lambda (imc) (push imc inserted))))
        (superchat-cowork-telega--insert-draft (current-buffer) "hello!")))
    (should (equal '("hello!") inserted))))

;; ── S1: reading history ───────────────────────────────────────

(ert-deftest cowork-telega/read-tool-declares-scoped-read ()
  "The read tool declares an external read scoped to the chat."
  (let ((effects (superchat-authority-resolve-effects
                  superchat-cowork-telega-read-tool-name '("123" 10))))
    (should (eq 'external-read (plist-get (car effects) :effect)))
    (should (equal "telega://chat/123" (plist-get (car effects) :scope)))))

(ert-deftest cowork-telega/read-tool-returns-oldest-first ()
  "History arrives reverse-chronological and is delivered oldest-first."
  (let (result)
    (cl-letf (((symbol-function 'telega-chat-get)
               (lambda (_id &optional _offline) '(:id 123 :title "C")))
              ((symbol-function 'telega--getChatHistory)
               (lambda (_chat _from _offset &optional _limit _local callback)
                 (funcall callback
                          (list :messages
                                (vector '(:id 2 :date 1753600000
                                          :content (:@type "messageText"))
                                        '(:id 1 :date 1753500000
                                          :content (:@type "messageText")))))))
              ((symbol-function 'telega-msg-sender) (lambda (_m) '(:id 7)))
              ((symbol-function 'telega-msg-sender-title)
               (lambda (_s &rest _) "Ada"))
              ((symbol-function 'telega-msg-content-text)
               (lambda (msg &optional _speech)
                 (format "msg-%s" (plist-get msg :id))))
              ((symbol-function 'telega-tme-internal-link-to)
               (lambda (msg &rest _)
                 (format "telega:link%s" (plist-get msg :id)))))
      (superchat-cowork-telega--read-messages
       (lambda (text) (setq result text)) "123" 2))
    (should (stringp result))
    ;; Oldest (id 1) precedes newest (id 2); links survive.
    (should (string-match-p "msg-1.*\n.*msg-2" result))
    (should (string-match-p "⟨telega:link1⟩" result))
    (should (string-match-p "Ada" result))))

;; ── S1: scope consent and digest ──────────────────────────────

(ert-deftest cowork-telega/scope-approval-allows-only-granted-reads ()
  "The consented scope auto-approves; everything else falls back."
  (let* ((fallbacks '())
         (superchat-authority-approval-function
          (lambda (request) (push request fallbacks) 'fallback))
         (approve (superchat-cowork-telega--scope-approval
                   '("telega://chat/123"))))
    (should (eq t (funcall approve
                           '(:effects ((:effect external-read
                                        :resource message
                                        :scope "telega://chat/123"))))))
    (should (eq 'fallback
                (funcall approve
                         '(:effects ((:effect external-read
                                      :resource message
                                      :scope "telega://chat/999"))))))
    (should (eq 'fallback
                (funcall approve
                         '(:effects ((:effect external-write
                                      :resource message
                                      :scope "telega://chat/123"))))))
    (should (= 2 (length fallbacks)))))

(ert-deftest cowork-telega/digest-run-cannot-send ()
  "The digest Request is structurally unable to write outward."
  (cl-letf (((symbol-function 'superchat-tape-append-event)
             (lambda (_event) 1)))
    (let* ((request (superchat-cowork-telega--digest-request
                     '((:id 123 :title "Chat 123"))
                     "summarize" #'ignore))
           (composed (superchat-extension-compose-request request)))
      (should (equal "telega-digest"
                     (plist-get request :authority-policy)))
      (should (superchat-authority-policy-known-p 'telega-digest))
      ;; Injection drill: even a hostile message cannot reach a send
      ;; tool — the explicit surface has none and stays unwidened...
      (should-not (member superchat-cowork-telega-tool-name
                          (plist-get composed :tools)))
      (should (member superchat-cowork-telega-read-tool-name
                      (plist-get composed :tools)))
      ;; ...and the policy denies external writes outright.
      (should (eq 'deny
                  (superchat-authority--policy-decision
                   'telega-digest
                   '((:effect external-write :resource message
                      :scope "telega://chat/123"))
                   nil)))
      (should (functionp (plist-get request :approval-function))))))

;; ── S2: triage rules ──────────────────────────────────────────

(ert-deftest cowork-telega/temex-lint-rejects-code-execution ()
  "Declarative temexes pass; eval/call are rejected anywhere."
  (should (superchat-cowork-telega--parse-temex
           "(and (chat (ids 123)) (contains \"lottery\"))"))
  (should (superchat-cowork-telega--parse-temex "unread"))
  (should-error (superchat-cowork-telega--parse-temex
                 "(eval (delete-file \"/tmp/x\"))"))
  (should-error (superchat-cowork-telega--parse-temex
                 "(or (contains \"a\") (call ignore))"))
  (should-error (superchat-cowork-telega--parse-temex "")))

(defmacro superchat-cowork-telega-test--with-rules (&rest body)
  "Run BODY with an isolated, temporary rule store."
  (declare (indent 0))
  `(let ((superchat-cowork-telega-rules-file
          (make-temp-file "telega-rules" nil ".eld"))
         (superchat-cowork-telega--rules nil))
     (unwind-protect
         (progn (delete-file superchat-cowork-telega-rules-file)
                ,@body)
       (when (file-exists-p superchat-cowork-telega-rules-file)
         (delete-file superchat-cowork-telega-rules-file)))))

(ert-deftest cowork-telega/rule-roundtrip-persists-and-applies ()
  "An approved rule persists, reloads identically, and mutes matches."
  (superchat-cowork-telega-test--with-rules
    (let ((superchat-authority-approval-function (lambda (_request) t)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (let ((result (superchat-tool-execute
                       "telega_propose_rules"
                       #'superchat-cowork-telega--propose-rule
                       '("(contains \"lottery\")" "17 ads this week")
                       :context '(:session-id "telega-triage"
                                  :attendance attended
                                  :authority-policy "telega-triage"))))
          (should (string-match-p "added (mute)" result)))))
    (should (= 1 (length superchat-cowork-telega--rules)))
    (should (file-exists-p superchat-cowork-telega-rules-file))
    ;; Reload from disk and get the identical temex back.
    (let ((persisted superchat-cowork-telega--rules))
      (setq superchat-cowork-telega--rules nil)
      (superchat-cowork-telega--load-rules)
      (should (equal (plist-get (car persisted) :temex)
                     (plist-get (car superchat-cowork-telega--rules)
                                :temex))))
    ;; The deterministic predicate consults the rule.
    (cl-letf (((symbol-function 'telega-msg-match-p)
               (lambda (_msg temex)
                 (equal temex '(contains "lottery")))))
      (should (superchat-cowork-telega--mute-p '(:id 1))))))

(ert-deftest cowork-telega/refused-rule-writes-nothing ()
  "A declined proposal leaves the store and the disk untouched."
  (superchat-cowork-telega-test--with-rules
    (let ((superchat-authority-approval-function (lambda (_request) nil)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (should (equal "[Tool call cancelled by user]"
                       (superchat-tool-execute
                        "telega_propose_rules"
                        #'superchat-cowork-telega--propose-rule
                        '("(contains \"x\")" "")
                        :context '(:session-id "telega-triage"
                                   :attendance attended
                                   :authority-policy "telega-triage"))))))
    (should (null superchat-cowork-telega--rules))
    (should-not (file-exists-p superchat-cowork-telega-rules-file))))

(ert-deftest cowork-telega/malicious-temex-blocked-before-approval ()
  "The triage validator rejects code-execution temexes pre-approval."
  (superchat-cowork-telega-test--with-rules
    (let* ((approval-asked nil)
           (superchat-authority-approval-function
            (lambda (_request) (setq approval-asked t) t))
           (validator (plist-get (superchat-cowork-telega--triage-request
                                  nil "review" #'ignore)
                                 :tool-validator)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (let ((result (superchat-tool-execute
                       "telega_propose_rules"
                       #'superchat-cowork-telega--propose-rule
                       '("(eval (kill-emacs))" "evil")
                       :context (list :session-id "telega-triage"
                                      :attendance 'attended
                                      :authority-policy "telega-triage"
                                      :validate validator))))
          (should (string-match-p "not allowed" result))
          (should-not approval-asked)
          (should (null superchat-cowork-telega--rules)))))))

(ert-deftest cowork-telega/triage-policy-is-tighten-only ()
  "The triage policy denies everything except the rule-store write."
  (dolist (case '(((:effect external-write :resource message) . deny)
                  ((:effect process-exec :resource process) . deny)
                  ((:effect local-write :resource file) . deny)
                  ((:effect local-write :resource telega-rules) . defer)
                  ((:effect external-read :resource message) . defer)))
    (should (eq (cdr case)
                (superchat-authority--policy-decision
                 'telega-triage (list (car case)) nil)))))

(ert-deftest cowork-telega/replay-counts-matching-samples ()
  "The approval replay counts hits over the sampled messages."
  (let ((superchat-cowork-telega--triage-samples
         (list (list :chat-title "A" :text "win the lottery"
                     :msg '(:text "win the lottery"))
               (list :chat-title "A" :text "meeting at 3"
                     :msg '(:text "meeting at 3"))
               (list :chat-title "B" :text "lottery again"
                     :msg '(:text "lottery again")))))
    (cl-letf (((symbol-function 'telega-msg-match-p)
               (lambda (msg _temex)
                 (string-match-p "lottery" (plist-get msg :text)))))
      (let ((replay (superchat-cowork-telega--replay '(contains "lottery"))))
        (should (= 2 (length (car replay))))
        (should (= 3 (cdr replay)))))))

(ert-deftest cowork-telega/triage-request-cannot-reach-other-tools ()
  "The triage Request exposes only the proposal tool."
  (cl-letf (((symbol-function 'superchat-tape-append-event)
             (lambda (_event) 1)))
    (let* ((request (superchat-cowork-telega--triage-request
                     nil "review" #'ignore))
           (composed (superchat-extension-compose-request request)))
      (should (equal '("telega_propose_rules")
                     (plist-get composed :tools)))
      (should (equal "telega-triage"
                     (plist-get request :authority-policy)))
      (should (equal "triage.telega"
                     (superchat-preset-profile-id
                      (plist-get request :preset)))))))

(ert-deftest cowork-telega/sample-collection-keeps-chat-identity ()
  "Async sample collection attributes messages to the right chat."
  (let (result)
    (cl-letf (((symbol-function 'telega--getChatHistory)
               (lambda (chat _from _offset &optional _limit _local callback)
                 (funcall callback
                          (list :messages
                                (vector (list :id (plist-get chat :id)))))))
              ((symbol-function 'telega-chat-title)
               (lambda (chat &optional _fmt _badges)
                 (plist-get chat :title)))
              ((symbol-function 'telega-msg-content-text)
               (lambda (msg &optional _speech)
                 (format "from-%s" (plist-get msg :id)))))
      (superchat-cowork-telega--collect-samples
       '((:id 1 :title "One") (:id 2 :title "Two"))
       5 (lambda (samples) (setq result samples))))
    (should (= 2 (length result)))
    (should (equal '("One" "Two")
                   (mapcar (lambda (sample) (plist-get sample :chat-title))
                           result)))
    (should (equal '("from-1" "from-2")
                   (mapcar (lambda (sample) (plist-get sample :text))
                           result)))))

;; ── Cowork Session ────────────────────────────────────────────

(cl-defmacro superchat-cowork-telega-test--with-session
    ((session-var origin-var &optional context) &rest body)
  "Open a mocked Session bound to SESSION-VAR and ORIGIN-VAR.
CONTEXT defaults to one selected message.  Session and origin buffers
are always killed after BODY."
  (declare (indent 1))
  `(let ((,origin-var (generate-new-buffer " *cowork-telega-origin*"))
         (,session-var nil))
     (unwind-protect
         (cl-letf (((symbol-function 'display-buffer) #'ignore))
           (setq ,session-var
                 (superchat-cowork-telega-session-open
                  '(:id 123 :title "Chat 123")
                  ,origin-var
                  ,(or context
                       ''(:text "Ada: original question" :message-id 7))))
           ,@body)
       (when (buffer-live-p ,session-var) (kill-buffer ,session-var))
       (when (buffer-live-p ,origin-var) (kill-buffer ,origin-var)))))

(ert-deftest cowork-telega/session-reuses-chat-and-separates-chats ()
  "One chat reuses its Session; a different chat gets another one."
  (let ((origin (generate-new-buffer " *cowork-telega-origin*"))
        first second other)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (setq first
                (superchat-cowork-telega-session-open
                 '(:id 123 :title "Chat 123") origin nil)
                second
                (superchat-cowork-telega-session-open
                 '(:id 123 :title "Chat 123") origin nil)
                other
                (superchat-cowork-telega-session-open
                 '(:id 999 :title "Chat 999") origin nil))
          (should (eq first second))
          (should-not (eq first other)))
      (dolist (buffer (list first other origin))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest cowork-telega/session-buffer-is-a-read-only-org-view ()
  "The Cowork sidecar is an Org view without losing its actions."
  (superchat-cowork-telega-test--with-session (session origin)
    (with-current-buffer session
      (should (derived-mode-p 'org-mode))
      (should buffer-read-only)
      (should visual-line-mode)
      (should (eq (key-binding (kbd "C-c C-c"))
                  #'superchat-cowork-telega-session-follow-up))
      (should (eq (key-binding (kbd "C-c C-a"))
                  #'superchat-cowork-telega-session-add-scope))
      (should (eq (key-binding (kbd "C-c C-d"))
                  #'superchat-cowork-telega-session-insert-last))
      (should (string-match-p "^#\\+title: Cowork Telegram — Chat 123$"
                              (buffer-string)))
      (should (string-match-p "^\\* Read scope$" (buffer-string)))
      (should (string-match-p "^\\* Conversation$" (buffer-string))))))

(ert-deftest cowork-telega/session-renders-model-markdown-as-org ()
  "Model Markdown and Telega links render through the shared Org path."
  (superchat-cowork-telega-test--with-session (session origin)
    (let ((state (superchat-cowork-telega--session-at session)))
      (superchat-cowork-telega--append-history
       state 'assistant
       "**Status**\n[post 71858](telega:@lobste_rs#71858)")
      (superchat-cowork-telega--render-session state))
    (with-current-buffer session
      (should (string-match-p "^\\*\\* Cowork$" (buffer-string)))
      (should (string-match-p
               "\\*Status\\*\n\\[\\[telega:@lobste_rs#71858\\]\\[post 71858\\]\\]"
               (buffer-string)))
      (should-not (string-match-p "\\*\\*Status\\*\\*" (buffer-string)))
      (let ((links (org-element-map (org-element-parse-buffer) 'link
                     #'identity)))
        (should (= 1 (length links)))
        (should (equal "telega:@lobste_rs#71858"
                       (org-element-property :raw-link (car links))))))))

(ert-deftest cowork-telega/command-names ()
  "The Session owns the main command and the one-shot action is explicit."
  (should (commandp 'superchat-cowork-telega))
  (should (commandp 'superchat-cowork-telega-reply))
  (should-not (fboundp 'superchat-telega)))

(ert-deftest cowork-telega/session-command-needs-no-selected-message ()
  "The canonical entry can start from the current chat alone."
  (let (request session-buffer)
    (with-temp-buffer
      (setq-local telega-chatbuf--chat '(:id 123 :title "Chat 123"))
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &rest _)
                       (setq session-buffer buffer)))
                    ((symbol-function 'superchat-cowork-telega--context)
                     (lambda ()
                       (error
                        "Select a Telega message or text region first")))
                    ((symbol-function 'superchat-extension-compose-request)
                     #'superchat-cowork-telega-test--compose-identity)
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (value)
                       (setq request value)
                       '(:status accepted :run-id "run-test"))))
            (superchat-cowork-telega "What needs my attention?")
            (should (string-match-p
                     "No message selected"
                     (plist-get request :prompt)))
            (should (equal (list superchat-cowork-telega-read-tool-name)
                           (plist-get request :tools))))
        (when (buffer-live-p session-buffer)
          (kill-buffer session-buffer))))))

(ert-deftest cowork-telega/session-request-is-read-only-and-keeps-history ()
  "Follow-up prompts retain prior turns without gaining write tools."
  (superchat-cowork-telega-test--with-session (session origin)
    (let (requests)
      (cl-letf (((symbol-function 'superchat-extension-compose-request)
                 #'superchat-cowork-telega-test--compose-identity)
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (push request requests)
                   '(:status accepted :run-id "run-test"))))
        (superchat-cowork-telega-session-submit
         session "What happened here?")
        (let ((first (car requests)))
          (should (equal (list superchat-cowork-telega-read-tool-name)
                         (plist-get first :tools)))
          (should-not
           (member superchat-cowork-telega-tool-name
                   (plist-get first :tools)))
          (should-not
           (member "telega_propose_rules" (plist-get first :tools)))
          (superchat-cowork-telega-test--deliver first
                   "There is one open question." 'completed 1))
        (superchat-cowork-telega-session-submit
         session "What should I do next?")
        (let ((second (car requests)))
          (should (string-match-p
                   "What happened here\\?"
                   (plist-get second :prompt)))
          (should (string-match-p
                   "There is one open question\\."
                   (plist-get second :prompt)))
          (should (string-match-p
                   "What should I do next\\?"
                   (plist-get second :prompt))))))))

(ert-deftest cowork-telega/session-composes-bundled-skill-without-send-briefing ()
  "The Session gets product guidance without legacy send instructions."
  (superchat-cowork-telega-test--with-session (session origin)
    (let (request)
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (value)
                   (setq request value)
                   '(:status accepted :run-id "run-test")))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (superchat-cowork-telega-session-submit session "Help me understand")
        (should (string-match-p
                 "understand what happened"
                 (plist-get request :system-prompt)))
        (should-not (string-match-p
                     "call telega_send_message exactly once"
                     (plist-get request :system-prompt)))))))

(ert-deftest cowork-telega/session-scope-grant-is-reused-and-audited ()
  "An explicit chat grant is listed, audited, and auto-approved."
  (superchat-cowork-telega-test--with-session (session origin)
    (let (request events fallback-asked)
      (cl-letf (((symbol-function 'superchat-extension-compose-request)
                 #'superchat-cowork-telega-test--compose-identity)
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (value)
                   (setq request value)
                   '(:status accepted :run-id "run-test")))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push event events)
                   1)))
        (superchat-cowork-telega-session-grant-chats
         session '((:id 999 :title "Chat 999")))
        (let ((superchat-authority-approval-function
               (lambda (_request)
                 (setq fallback-asked t)
                 nil)))
          (superchat-cowork-telega-session-submit session "Compare both chats"))
        (should (string-match-p "Chat 999.*999" (plist-get request :prompt)))
        (should
         (eq t
             (funcall
              (plist-get request :approval-function)
              '(:effects ((:effect external-read
                           :resource message
                           :scope "telega://chat/999"))))))
        (should-not fallback-asked)
        (should
         (cl-find "cowork.scope-granted" events
                  :key (lambda (event) (plist-get event :type))
                  :test #'equal))))))

(ert-deftest cowork-telega/session-validator-rejects-unknown-read-scope ()
  "A Session read cannot name a chat outside its known scope set."
  (superchat-cowork-telega-test--with-session (session origin)
    (let (request)
      (cl-letf (((symbol-function 'superchat-extension-compose-request)
                 #'superchat-cowork-telega-test--compose-identity)
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (value)
                   (setq request value)
                   '(:status accepted :run-id "run-test"))))
        (superchat-cowork-telega-session-submit session "Read recent context")
        (let ((validator (plist-get request :tool-validator)))
          (should (functionp validator))
          (should (funcall validator '("123" 20)))
          (should-error (funcall validator '("999" 20))))))))

(ert-deftest cowork-telega/session-blocks-overlapping-turns ()
  "A Session accepts one in-flight turn at a time."
  (superchat-cowork-telega-test--with-session (session origin)
    (cl-letf (((symbol-function 'superchat-extension-compose-request)
               #'superchat-cowork-telega-test--compose-identity)
              ((symbol-function 'superchat-runtime-submit)
               (lambda (_request)
                 '(:status accepted :run-id "run-test"))))
      (superchat-cowork-telega-session-submit session "First")
      (should-error
       (superchat-cowork-telega-session-submit session "Second")
       :type 'user-error))))

(ert-deftest cowork-telega/session-inserts-last-answer-into-origin ()
  "The latest answer returns to the originating Telega input area."
  (superchat-cowork-telega-test--with-session (session origin)
    (let (request inserted)
      (cl-letf (((symbol-function 'superchat-extension-compose-request)
                 #'superchat-cowork-telega-test--compose-identity)
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (value)
                   (setq request value)
                   '(:status accepted :run-id "run-test")))
                ((symbol-function 'telega-chatbuf-input-insert)
                 (lambda (text) (setq inserted text))))
        (superchat-cowork-telega-session-submit session "Draft a reply")
        (superchat-cowork-telega-test--deliver request
                 "Friday works for me." 'completed 1)
        (superchat-cowork-telega-session-insert-last session)
        (should (equal "Friday works for me." inserted))))))

;; -- Session EditorWorkspace migration ---------------------------------

(ert-deftest cowork-telega/four-request-shapes-use-exact-owned-hosts ()
  "Every shipped Request shape carries its stable Session's exact host."
  (let ((cases
         (list
          (list "telega-chat-123" 'telega-reply-protocol
                (lambda ()
                  (superchat-cowork-telega--request
                   '(:id 123 :title "Chat 123") '(:text "hello")
                   "reply" #'ignore)))
          (list "telega-digest" 'telega-digest-protocol
                (lambda ()
                  (superchat-cowork-telega--digest-request
                   '((:id 123 :title "Chat 123")) "digest" #'ignore)))
          (list "telega-triage" 'telega-triage-protocol
                (lambda ()
                  (superchat-cowork-telega--triage-request nil "triage" #'ignore))))))
    (unwind-protect
        (dolist (case cases)
          (pcase-let* ((`(,session-id ,role ,builder) case)
                       (request (funcall builder))
                       (buffer (plist-get request :buffer))
                       (reused (plist-get (funcall builder) :buffer)))
            (should (equal session-id (plist-get request :session-id)))
            (should (buffer-live-p buffer))
            (should (eq buffer reused))
            (should (eq buffer
                        (superchat-editor-workspace-buffer session-id role)))
            (should (eq buffer
                        (superchat-editor-workspace-validate-buffer
                         buffer session-id)))
            (dolist (legacy '(:buffer-name :ephemeral-buffer
                              :host-authority-mode))
              (should-not (plist-member request legacy)))))
      (dolist (session-id '("telega-chat-123" "telega-digest"
                            "telega-triage"))
        (superchat-session-workspace-close session-id)))))

(ert-deftest cowork-telega/contextual-request-uses-session-view-exactly ()
  "The visible contextual Session stores and submits its exact owned View."
  (superchat-cowork-telega-test--with-session (buffer origin)
    (let* ((session (superchat-cowork-telega--session-at buffer))
           (session-id (superchat-cowork-telega--session-id session))
           (request (superchat-cowork-telega--session-request
                     session "inspect" #'ignore)))
      (should (equal "telega-cowork-123" session-id))
      (should (eq buffer
                  (superchat-editor-workspace-buffer
                   session-id 'telega-session-view)))
      (should (eq buffer (plist-get request :buffer)))
      (dolist (legacy '(:buffer-name :ephemeral-buffer :host-authority-mode))
        (should-not (plist-member request legacy))))))

(ert-deftest cowork-telega/four-request-shapes-enforce-public-host-authority ()
  "All Request shapes enforce owned, borrowed, and exact-object boundaries."
  (superchat-cowork-telega-test--with-session (view origin)
    (let* ((contextual (superchat-cowork-telega--session-at view))
           (cases
            (list
             (list "telega-chat-123" 'telega-reply-protocol
                   (lambda ()
                     (superchat-cowork-telega--request
                      '(:id 123 :title "Chat 123") '(:text "x")
                      "reply" #'ignore)))
             (list "telega-digest" 'telega-digest-protocol
                   (lambda ()
                     (superchat-cowork-telega--digest-request
                      '((:id 123 :title "Chat 123")) "digest" #'ignore)))
             (list "telega-triage" 'telega-triage-protocol
                   (lambda ()
                     (superchat-cowork-telega--triage-request
                      nil "triage" #'ignore)))
             (list "telega-cowork-123" 'telega-session-view
                   (lambda ()
                     (superchat-cowork-telega--session-request
                      contextual "inspect" #'ignore))))))
      (dolist (case cases)
        (pcase-let* ((`(,session-id ,role ,builder) case)
                     (request (funcall builder))
                     (owned (plist-get request :buffer))
                     (other-session (concat session-id "-foreign"))
                     (mismatch
                      (superchat-editor-workspace-buffer
                       other-session role t))
                     (malformed (generate-new-buffer " *telega-malformed*"))
                     (borrowed (generate-new-buffer " *telega-inherited*"))
                     (same-name (generate-new-buffer " *telega-same-name*"))
                     (original-name (buffer-name owned))
                     (runtime-count 0)
                     (provider-count 0)
                     (events nil)
                     (superchat-closure-terminal-guard-enabled nil))
          (unwind-protect
              (progn
                ;; These local markers are intentionally malformed test input;
                ;; classification and rejection still use only the public API.
                (with-current-buffer malformed
                  (setq-local superchat--editor-workspace-owned-p t))
                (cl-letf (((symbol-function 'superchat-tape-append-event)
                           (lambda (event) (push event events) 1))
                          ((symbol-function 'superchat-runtime-submit)
                           (lambda (_candidate)
                             (cl-incf runtime-count)
                             '(:status accepted :run-id "unreachable")))
                          ((symbol-function 'superchat--llm-generate-answer)
                           (lambda (&rest _args) (cl-incf provider-count))))
                  (dolist (bad (list mismatch malformed))
                    (let ((candidate (copy-sequence request)))
                      (setq candidate (plist-put candidate :buffer bad))
                      (should-error
                       (superchat-runtime-test-submit
                        (superchat-cowork-telega--compose-request candidate)))))
                  (should (= 0 runtime-count))
                  (should (= 0 provider-count))
                  (should-not
                   (cl-find "extension.activated" events
                            :key (lambda (event) (plist-get event :type))
                            :test #'equal)))
                ;; Dynamically inherited/default-looking values are unowned.
                (let ((superchat--editor-workspace-owned-p t)
                      (superchat--editor-workspace-session-id other-session)
                      (superchat--editor-workspace-role role)
                      (superchat-closure-terminal-guard-enabled nil))
                  (should-not
                   (superchat-editor-workspace-owner-session borrowed))
                  (let ((candidate (copy-sequence request)))
                    (setq candidate (plist-put candidate :buffer borrowed))
                    (cl-letf
                        (((symbol-function 'superchat-tape-append-event)
                          (lambda (_event) 1))
                         ((symbol-function 'display-buffer) #'ignore)
                         ((symbol-function 'superchat--llm-generate-answer)
                          (lambda (_prompt _callback _stream &rest _args)
                            (funcall superchat--llm-runtime-observer
                                     'terminal
                                     '(:status completed :response "done"))
                            #'ignore)))
                      (let* ((composed
                              (superchat-cowork-telega--compose-request
                               candidate))
                             (handle (superchat-runtime-test-submit composed)))
                        (should (eq borrowed (plist-get composed :buffer)))
                        (should
                         (eq 'completed
                             (plist-get
                              (superchat-runtime-status
                               (plist-get handle :run-id))
                              :status))))))
                  (should-not
                   (local-variable-p 'superchat--editor-workspace-owned-p
                                     borrowed)))
                ;; A foreign object with the old name cannot replace the exact
                ;; owned object retained by the Request.
                (with-current-buffer owned
                  (rename-buffer (generate-new-buffer-name
                                  (concat original-name "-owned")) t))
                (with-current-buffer same-name
                  (rename-buffer original-name t))
                (let ((superchat-closure-terminal-guard-enabled nil))
                  (cl-letf
                      (((symbol-function 'superchat-tape-append-event)
                        (lambda (_event) 1))
                       ((symbol-function 'display-buffer) #'ignore)
                       ((symbol-function 'superchat--llm-generate-answer)
                        (lambda (_prompt _callback _stream &rest _args)
                          (funcall superchat--llm-runtime-observer
                                   'terminal
                                   '(:status completed :response "done"))
                          #'ignore)))
                    (let* ((composed
                            (superchat-cowork-telega--compose-request request))
                           (handle (superchat-runtime-test-submit composed)))
                      (should (eq owned (plist-get composed :buffer)))
                      (should-not (eq same-name (plist-get composed :buffer)))
                      (should
                       (eq 'completed
                           (plist-get
                            (superchat-runtime-status
                             (plist-get handle :run-id))
                            :status))))))
                (should (eq owned
                            (superchat-editor-workspace-buffer session-id role)))
                (should (eq 'closed
                            (superchat-session-workspace-close other-session)))
                (should-not (buffer-live-p mismatch))
                (should (buffer-live-p borrowed))
                (should (buffer-live-p same-name)))
            (when (buffer-live-p mismatch) (kill-buffer mismatch))
            (when (buffer-live-p malformed) (kill-buffer malformed))
            (when (buffer-live-p borrowed) (kill-buffer borrowed))
            (when (buffer-live-p same-name) (kill-buffer same-name))))))))

(ert-deftest cowork-telega/ordinary-hosts-survive-runtime-terminals-until-close ()
  "Reply, digest, and triage hosts survive Runtime terminals until close."
  (let ((builders
         (list
          (cons "telega-chat-123"
                (lambda () (superchat-cowork-telega--request
                            '(:id 123 :title "Chat 123") '(:text "x")
                            "reply" #'ignore)))
          (cons "telega-digest"
                (lambda () (superchat-cowork-telega--digest-request
                            '((:id 123 :title "Chat 123")) "digest" #'ignore)))
          (cons "telega-triage"
                (lambda () (superchat-cowork-telega--triage-request
                            nil "triage" #'ignore))))))
    (dolist (entry builders)
      (dolist (terminal '(completed failed cancel))
        (let* ((request (funcall (cdr entry)))
               (buffer (plist-get request :buffer))
               (superchat-closure-terminal-guard-enabled nil))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (_event) 1))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _callback _stream &rest _args)
                       (unless (eq terminal 'cancel)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  (list :status terminal
                                        :response (symbol-name terminal))))
                       #'ignore)))
            (let ((handle (superchat-runtime-test-submit request)))
              (when (eq terminal 'cancel)
                (superchat-runtime-cancel
                 (plist-get handle :run-id) "test cancellation"))
              (should (buffer-live-p buffer)))))))
    (dolist (entry builders)
      (let* ((session-id (car entry))
             (buffer (plist-get (funcall (cdr entry)) :buffer)))
        (should (buffer-live-p buffer))
        (should (eq 'closed
                    (superchat-session-workspace-close session-id)))
        (should-not (buffer-live-p buffer))))))

(ert-deftest cowork-telega/contextual-host-survives-runtime-terminals-until-close ()
  "The contextual View survives success, failure, and cancellation."
  (superchat-cowork-telega-test--with-session (buffer origin)
    (let* ((session (superchat-cowork-telega--session-at buffer))
           (session-id (superchat-cowork-telega--session-id session)))
      (dolist (terminal '(completed failed cancel))
        (let ((request (superchat-cowork-telega--session-request
                        session "inspect" #'ignore))
              (superchat-closure-terminal-guard-enabled nil))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (_event) 1))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _callback _stream &rest _args)
                       (unless (eq terminal 'cancel)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  (list :status terminal
                                        :response (symbol-name terminal))))
                       #'ignore)))
            (let ((handle (superchat-runtime-test-submit request)))
              (when (eq terminal 'cancel)
                (superchat-runtime-cancel
                 (plist-get handle :run-id) "test cancellation"))
              (should (buffer-live-p buffer))))))
      (should (eq 'closed (superchat-session-workspace-close session-id)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest cowork-telega/callback-nonlocal-exits-cross-real-runtime ()
  "Runtime isolates callback error/quit and leaves all four hosts alive."
  (superchat-cowork-telega-test--with-session (view origin)
    (let* ((contextual (superchat-cowork-telega--session-at view))
           (cases
            (list
             (list "telega-cowork-123"
                   (lambda (callback)
                     (let* ((request
                             (superchat-cowork-telega--session-request
                              contextual "inspect" #'ignore))
                            (delivery (plist-get request :delivery-function)))
                       (setf (superchat-cowork-telega--session-running-p
                              contextual) t)
                       (plist-put
                        request :delivery-function
                        (lambda (response status tape-id)
                          (funcall delivery response status tape-id)
                          (funcall callback response status))))))
             (list "telega-chat-123"
                   (lambda (callback)
                     (superchat-cowork-telega--request
                      '(:id 123 :title "Chat 123") '(:text "x")
                      "reply" callback)))
             (list "telega-digest"
                   (lambda (callback)
                     (superchat-cowork-telega--digest-request
                      '((:id 123 :title "Chat 123")) "digest" callback)))
             (list "telega-triage"
                   (lambda (callback)
                     (superchat-cowork-telega--triage-request
                      nil "triage" callback))))))
      (dolist (case cases)
        (pcase-let ((`(,session-id ,builder) case))
          (dolist (signal-type '(error quit))
            (let* ((calls 0)
                   (terminal-observer nil)
                   (request
                    (funcall builder
                             (lambda (&rest _args)
                               (cl-incf calls)
                               (signal signal-type '("foreign callback")))))
                   (buffer (plist-get request :buffer))
                   (superchat-closure-terminal-guard-enabled nil))
              (cl-letf (((symbol-function 'superchat-tape-append-event)
                         (lambda (_event) 1))
                        ((symbol-function 'display-warning) #'ignore)
                        ((symbol-function 'display-buffer) #'ignore)
                        ((symbol-function 'superchat--llm-generate-answer)
                         (lambda (_prompt _callback _stream &rest _args)
                           (setq terminal-observer
                                 superchat--llm-runtime-observer)
                           (funcall superchat--llm-runtime-observer
                                    'terminal
                                    '(:status completed :response "done"))
                           #'ignore)))
                (let* ((handle (superchat-runtime-test-submit request))
                       (run-id (plist-get handle :run-id))
                       (status (superchat-runtime-status run-id)))
                  (should (= 1 calls))
                  (should (eq 'completed (plist-get status :status)))
                  (should-not (member run-id (superchat-runtime-active-runs)))
                  (funcall terminal-observer
                           'terminal '(:status completed :response "late"))
                  (should (= 1 calls))
                  (should (eq buffer (plist-get request :buffer)))
                  (should (buffer-live-p buffer))
                  (when (equal session-id "telega-cowork-123")
                    (should-not
                     (superchat-cowork-telega--session-running-p contextual))))))))
      (dolist (session-id '("telega-chat-123" "telega-digest"
                            "telega-triage" "telega-cowork-123"))
        (let ((buffers
               (superchat-editor-workspace-owned-buffers session-id)))
          (should (eq 'closed (superchat-session-workspace-close session-id)))
          (dolist (buffer buffers)
            (should-not (buffer-live-p buffer)))))
      (should-not (gethash 123 superchat-cowork-telega--sessions))))))

(ert-deftest cowork-telega/extension-cannot-retarget-before-activation-tape ()
  "Composition rejects Session/buffer retargeting before activation audit."
  (let* ((request (superchat-cowork-telega--request
                   '(:id 123 :title "Chat 123") '(:text "hello")
                   "reply" #'ignore))
         (buffer (plist-get request :buffer))
         (alternate-session "telega-chat-999")
         (alternate
          (superchat-editor-workspace-buffer
           alternate-session 'telega-reply-protocol t))
         (nil-value-candidate (copy-sequence request))
         (_nil-value
          (setq nil-value-candidate
                (plist-put nil-value-candidate :buffer nil)))
         (missing-key-candidate (copy-sequence request))
         (_key-removed (cl-remf missing-key-candidate :buffer))
         (tamperers
          (list
           (cons 'in-place-buffer
                 (lambda (candidate)
                   (plist-put candidate :buffer alternate)))
           (cons 'copied-buffer
                 (lambda (candidate)
                   (plist-put (copy-sequence candidate) :buffer alternate)))
           (cons 'nil-buffer-value
                 (lambda (_candidate) nil-value-candidate))
           (cons 'missing-buffer-key
                 (lambda (_candidate) missing-key-candidate))
           (cons 'alternate-valid-pair
                 (lambda (candidate)
                   (let ((copy (copy-sequence candidate)))
                     (setq copy
                           (plist-put copy :session-id alternate-session))
                     (plist-put copy :buffer alternate))))))
         events runtime-count provider-count)
    ;; Keep fixture validity outside the broad rejection assertion below.
    (should (plist-member nil-value-candidate :buffer))
    (should-not (plist-member missing-key-candidate :buffer))
    (unwind-protect
        (dolist (entry tamperers)
          (let ((name (car entry))
                (tamper (cdr entry)))
            (setq events nil runtime-count 0 provider-count 0)
            (when (eq name 'missing-buffer-key)
              (should-not (plist-member missing-key-candidate :buffer)))
            (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (lambda (event) (push event events) 1))
                      ((symbol-function 'superchat-runtime-submit)
                       (lambda (_candidate) (cl-incf runtime-count)))
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) (cl-incf provider-count)))
                      ((symbol-function 'superchat-extension--compose-tools)
                       (lambda (candidate _active)
                         (funcall tamper candidate))))
              (should-error
               (superchat-runtime-test-submit
                (superchat-cowork-telega--compose-request request))))
            (should (= 0 runtime-count))
            (should (= 0 provider-count))
            (should-not (cl-find "extension.activated" events
                                 :key (lambda (event) (plist-get event :type))
                                 :test #'equal))
            (should (buffer-live-p buffer))))
      (superchat-session-workspace-close "telega-chat-123")
      (superchat-session-workspace-close alternate-session))))

(ert-deftest cowork-telega/plugin-guide-publishes-modern-host-contract ()
  "The public guide example and field table describe the exact-buffer seam."
  (let ((guide (expand-file-name
                "../../docs/plugin-guide.md"
                (file-name-directory
                 (locate-library "superchat-cowork-telega.el")))))
    (with-temp-buffer
      (insert-file-contents guide)
      ;; S14: the example submits an envelope whose host rides in
      ;; `:context :host-buffer'; the driver validates it.
      (dolist (text '("(require 'superchat-editor-workspace)"
                      "superchat-editor-workspace-buffer"
                      "superchat-editor-workspace-validate-buffer"
                      ":host-buffer buffer" "superchat-pipeline-submit"
                      "ordinary borrowed"
                      "owned host" "breaking boundary"
                      "已删除的 legacy 输入"
                      "数字版本号尚未指定" "external consumers"))
        (goto-char (point-min))
        (should (search-forward text nil t))))))

(ert-deftest cowork-telega/active-experiments-have-no-legacy-host-producers ()
  "Active experiment submit sites use public exact EditorWorkspace hosts."
  (let ((root (file-name-directory (locate-library "superchat.el"))))
    (dolist (relative '("experiments/experience-learning-minimal/paired-use.el"
                        "experiments/experience-learning-minimal/recovery-self-learning.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name relative root))
        (should (search-forward "(require 'superchat-editor-workspace)" nil t))
        (goto-char (point-min))
        (should (search-forward "superchat-editor-workspace-validate-buffer" nil t))
        (goto-char (point-min))
        (should-not (re-search-forward
                     ":ephemeral-buffer\\|:host-authority-mode\\|:buffer-name"
                     nil t))))))

(provide 'test-cowork-telega)

;;; test-cowork-telega.el ends here

(ert-deftest cowork-telega/session-submits-envelope-with-callback-sink ()
  "S14: the Cowork Session submits a Request envelope on the `telega'
surface with a callback Sink; the request-local approval function and the
tool validator travel as `:runtime-extras'; the chat id and Session buffer
are in the envelope's context."
  (superchat-cowork-telega-test--with-session (session-buffer origin)
    (let* ((session (superchat-cowork-telega--session-at session-buffer))
           (request (superchat-cowork-telega--session-request session "hello" #'ignore))
           (envelope (superchat-pipeline-envelope-from-request
                      request
                      (list :telega-chat-id "123" :telega-buffer session-buffer)))
           (extras (plist-get envelope :runtime-extras))
           (context (plist-get envelope :context)))
      (ignore origin)
      (should (eq 'telega (plist-get envelope :surface)))
      (should (memq 'telega superchat-pipeline--surfaces))
      (should (equal "hello" (plist-get envelope :content)))
      (should (superchat-sink-p (plist-get envelope :sink)))
      (should (functionp (plist-get extras :approval-function)))
      (should (functionp (plist-get extras :tool-validator)))
      (should (plist-member extras :record-conversation))
      (should (equal "123" (plist-get context :telega-chat-id)))
      (should (eq session-buffer (plist-get context :host-buffer)))
      (should (stringp (plist-get context :prepared-prompt)))
      (dolist (key '(:buffer :buffer-name :prompt :input :stream-function :delivery-function))
        (should-not (plist-member envelope key)))
      ;; The envelope passes the pipeline's own validation.
      (should (plist-get (superchat-pipeline-validate-envelope envelope) :request-id)))))
