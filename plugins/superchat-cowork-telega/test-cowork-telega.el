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

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-cowork-telega)

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
      (let ((composed (superchat-extension-compose-request request)))
        (should (memq 'telega-cowork
                      (plist-get composed :superchat-extensions)))
        (should (member superchat-cowork-telega-tool-name
                        (plist-get composed :tools)))
        (should (member "memory_search" (plist-get composed :tools)))
        (should (string-match-p "Active Superchat extensions"
                                (plist-get composed :system-prompt)))
        (should (string-match-p "telega_send_message exactly once"
                                (plist-get composed :system-prompt)))
        (let ((handle (superchat-runtime-submit composed)))
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
                     #'identity)
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
                 #'identity)
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
          (funcall (plist-get first :delivery-function)
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
                 #'identity)
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
                 #'identity)
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
               #'identity)
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
                 #'identity)
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (value)
                   (setq request value)
                   '(:status accepted :run-id "run-test")))
                ((symbol-function 'telega-chatbuf-input-insert)
                 (lambda (text) (setq inserted text))))
        (superchat-cowork-telega-session-submit session "Draft a reply")
        (funcall (plist-get request :delivery-function)
                 "Friday works for me." 'completed 1)
        (superchat-cowork-telega-session-insert-last session)
        (should (equal "Friday works for me." inserted))))))

(provide 'test-cowork-telega)

;;; test-cowork-telega.el ends here
