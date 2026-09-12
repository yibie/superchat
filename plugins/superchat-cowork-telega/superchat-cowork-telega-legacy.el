;;; superchat-cowork-telega-legacy.el --- Compatibility Telega workflows -*- lexical-binding: t; -*-

;;; Commentary:

;; Proven Telega tools and the pre-Session reply/digest/triage entrypoints.
;; The bundle loader keeps these commands available while
;; `superchat-cowork-telega-session' owns the primary user experience.
;;
;; Unlike the in-tree `superchat-telega' tracer bullet, the model here
;; drives the send itself: `telega_send_message' is registered on the
;; model tool surface, the Request pins a `cowork' Capability and the
;; tighten-only `cowork' Authority policy, a request-local validator
;; binds every send to the chat the Request came from, and Authority
;; asks the user with a full-draft preview before anything leaves Emacs.
;;
;; There are still no triggers, background runs, or automatic replies.
;; Loading this plugin alongside `superchat-telega' is safe — both
;; declare the same effects for `telega_send_message' — but only one of
;; them should be your entry point.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'superchat)
(require 'superchat-authority)
(require 'superchat-editor-workspace)
(require 'superchat-extension)
(require 'superchat-preset)
(require 'superchat-runtime)
(require 'superchat-pipeline)
(require 'superchat-tape)
(require 'superchat-tools)

(defun superchat-cowork-telega--request-buffer (session-id role)
  "Return SESSION-ID's exact validated EditorWorkspace buffer for ROLE."
  (let ((buffer (superchat-editor-workspace-buffer session-id role t)))
    (superchat-editor-workspace-validate-buffer buffer session-id)))

(defun superchat-cowork-telega--compose-request (request)
  "Compose REQUEST without allowing Extensions to retarget its identity."
  (let ((session-id (plist-get request :session-id))
        (buffer (plist-get request :buffer)))
    (superchat-editor-workspace-validate-buffer buffer session-id)
    (superchat-extension-compose-request
     request
     (lambda (candidate)
       (unless (and (plist-member candidate :session-id)
                    (equal session-id (plist-get candidate :session-id)))
         (error "Extension cannot retarget or remove the Telega Request Session"))
       (unless (and (plist-member candidate :buffer)
                    (eq buffer (plist-get candidate :buffer)))
         (error "Extension cannot retarget or remove the Telega Request buffer"))
       (superchat-editor-workspace-validate-buffer buffer session-id)))))

(defvar telega-chatbuf--chat)

(declare-function telega--sendMessage "telega-tdlib"
                  (chat imc &optional input-reply-to options &rest keys))
(declare-function telega--getChatHistory "telega-tdlib"
                  (chat from-msg-id offset &optional limit only-local callback))
(declare-function telega-chat-get "telega-chat" (chat-id &optional offline-p))
(declare-function telega-chat-title "telega-chat"
                  (chat &optional fmt-type no-badges))
(declare-function telega-chatbuf-input-insert "telega-chat" (imc))
(declare-function telega-completing-read-chat-list "telega-util"
                  (prompt &optional chats-list sort-criteria))
(declare-function telega-tme-internal-link-to "telega-tme"
                  (chat-or-msg &rest params))
(declare-function telega-msg-at "telega-msg" (&optional pos msg-predicate))
(declare-function telega-msg-content-text "telega-msg"
                  (msg &optional with-speech-recognition-p))
(declare-function telega-msg-sender "telega-msg" (tl-obj))
(declare-function telega-msg-sender-title "telega-msg"
                  (msg-sender &rest args))
(declare-function telega-string-fmt-text "telega-util"
                  (text &optional default-markup-func))

(defconst superchat-cowork-telega-tool-name "telega_send_message"
  "Model tool this plugin registers for sending Telega messages.")

(defconst superchat-cowork-telega-read-tool-name "telega_read_messages"
  "Model tool this plugin registers for reading Telega chat history.")

(defcustom superchat-cowork-telega-autonomy 'approve
  "How far the Telega coworker may go with a reply.
`draft'   — compose only; the draft lands in the Telega input area and
            the user edits and sends it themselves.
`approve' — compose and send through the audited tool after the user
            approves the full draft once."
  :type '(choice (const :tag "Draft into the input area" draft)
                 (const :tag "Send after one approval" approve))
  :group 'superchat)

;; ── Telega helpers ────────────────────────────────────────────

(defun superchat-cowork-telega--chat-id (value)
  "Return VALUE as an integer Telega chat ID, or signal."
  (cond
   ((integerp value) value)
   ((and (stringp value)
         (string-match-p "\\`-?[0-9]+\\'" value))
    (string-to-number value))
   (t (error "Invalid Telega chat ID: %S" value))))

(defun superchat-cowork-telega--chat-title (chat)
  "Return a plain display title for CHAT."
  (substring-no-properties
   (or (and (fboundp 'telega-chat-title)
            (telega-chat-title chat nil t))
       (plist-get chat :title)
       (format "chat %s" (plist-get chat :id)))))

(defun superchat-cowork-telega--send (chat-id message)
  "Send MESSAGE to the explicitly named Telega CHAT-ID.
Authority approval and audit are owned by the shared Tool Executor;
this function only performs the already-approved send."
  (unless (and (stringp message)
               (not (string-empty-p (string-trim message))))
    (error "Telega message must be non-empty"))
  (dolist (function '(telega-chat-get telega-string-fmt-text
                      telega--sendMessage))
    (unless (fboundp function)
      (error "Telega is not loaded")))
  (let* ((id (superchat-cowork-telega--chat-id chat-id))
         (chat (telega-chat-get id t)))
    (unless chat
      (error "Telega chat is not loaded: %s" chat-id))
    (telega--sendMessage
     chat
     (list :@type "inputMessageText"
           :text (telega-string-fmt-text message))
     nil nil :sync-p t)
    (format "Sent Telega message to %s"
            (superchat-cowork-telega--chat-title chat))))

;; ── Tool registration (public seam) ───────────────────────────

(defun superchat-cowork-telega--send-effects (args)
  "Declare the external write performed by send ARGS."
  (let ((chat-id (superchat-authority-effect-argument args "chat_id" 0)))
    (list (list :effect 'external-write
                :resource 'message
                :scope (format "telega://chat/%s" chat-id)))))

(defun superchat-cowork-telega--preview (request)
  "Show the full outgoing draft described by approval REQUEST."
  (let* ((args (plist-get request :args))
         (chat-id (superchat-authority-effect-argument args "chat_id" 0))
         (message (superchat-authority-effect-argument args "message" 1))
         (chat (and (fboundp 'telega-chat-get)
                    (ignore-errors
                      (telega-chat-get
                       (superchat-cowork-telega--chat-id chat-id) t)))))
    (with-help-window "*Superchat Telega Draft*"
      (with-current-buffer standard-output
        (insert (format "Recipient: %s\nChat ID: %s\n\n%s"
                        (if chat
                            (superchat-cowork-telega--chat-title chat)
                          chat-id)
                        chat-id
                        message))))))

(superchat-register-tool
 superchat-cowork-telega-tool-name
 :function #'superchat-cowork-telega--send
 :description "Send a message to the named Telega chat.  Use only the \
chat_id supplied in the request context; the user approves every send."
 :args (list (list :name "chat_id" :type 'string
                   :description "Target Telega chat ID from the request context.")
             (list :name "message" :type 'string
                   :description "Complete message text to send."))
 :effects #'superchat-cowork-telega--send-effects
 :approval-preview #'superchat-cowork-telega--preview)

;; ── Reading history (analyst role) ────────────────────────────

(defun superchat-cowork-telega--format-message (msg)
  "Return one digest-ready line for TDLib message MSG."
  (let* ((date (plist-get msg :date))
         (stamp (if (and (numberp date) (> date 0))
                    (format-time-string "%Y-%m-%d %H:%M" date)
                  "????-??-?? ??:??"))
         (sender (and (fboundp 'telega-msg-sender)
                      (ignore-errors (telega-msg-sender msg))))
         (sender-title
          (or (and sender
                   (fboundp 'telega-msg-sender-title)
                   (ignore-errors
                     (substring-no-properties
                      (telega-msg-sender-title sender))))
              "?"))
         (text (or (and (fboundp 'telega-msg-content-text)
                        (ignore-errors
                          (substring-no-properties
                           (telega-msg-content-text msg))))
                   "[non-text content]"))
         (link (and (fboundp 'telega-tme-internal-link-to)
                    (ignore-errors (telega-tme-internal-link-to msg)))))
    (format "[%s] %s: %s%s"
            stamp sender-title (string-trim text)
            (if link (format " ⟨%s⟩" link) ""))))

(defun superchat-cowork-telega--read-messages (callback chat-id &optional limit)
  "Asynchronously read up to LIMIT recent messages from CHAT-ID.
CALLBACK receives one formatted string, oldest message first.
Authority approval is owned by `superchat-tool-execute'."
  (dolist (function '(telega-chat-get telega--getChatHistory))
    (unless (fboundp function)
      (error "Telega is not loaded")))
  (let* ((id (superchat-cowork-telega--chat-id chat-id))
         (chat (telega-chat-get id t))
         (want (min (max (if (numberp limit) limit 50) 1) 200)))
    (unless chat
      (error "Telega chat is not loaded: %s" chat-id))
    (telega--getChatHistory chat 0 0 want nil
      (lambda (history)
        (funcall
         callback
         (condition-case err
             (let ((messages (append (plist-get history :messages) nil)))
               (if (null messages)
                   "No messages found."
                 (mapconcat #'superchat-cowork-telega--format-message
                            (nreverse messages) "\n")))
           (error (format "[Error formatting history: %s]"
                          (error-message-string err)))))))))

(defun superchat-cowork-telega--read-effects (args)
  "Declare the external read performed by history ARGS."
  (let ((chat-id (superchat-authority-effect-argument args "chat_id" 0)))
    (list (list :effect 'external-read
                :resource 'message
                :scope (format "telega://chat/%s" chat-id)))))

(superchat-register-tool
 superchat-cowork-telega-read-tool-name
 :function #'superchat-cowork-telega--read-messages
 :async t
 :description "Read up to limit (default 50) recent messages from the \
named Telega chat.  Returns them oldest-first with timestamps, senders, \
and per-message ⟨telega:...⟩ links you must preserve in summaries."
 :args (list (list :name "chat_id" :type 'string
                   :description "Telega chat ID to read.")
             (list :name "limit" :type 'number
                   :description "Maximum messages (default 50, cap 200)."
                   :optional t))
 :effects #'superchat-cowork-telega--read-effects)

;; ── Triage rules (gatekeeper role) ────────────────────────────
;;
;; The LLM never sits in the hot path.  It proposes rules as telega's
;; native temex s-expressions; the user approves each rule with a
;; retroactive replay preview; approved rules are plain data applied by
;; a deterministic predicate hooked into
;; `telega-msg-ignore-predicates'.

(defvar telega-msg-ignore-predicates)
(declare-function telega-msg-match-p "telega-match" (msg temex))

(defcustom superchat-cowork-telega-rules-file
  (if (boundp 'superchat-data-directory)
      (expand-file-name "telega-rules.eld" superchat-data-directory)
    (locate-user-emacs-file "superchat-telega-rules.eld"))
  "File persisting the approved triage rules as readable Lisp data."
  :type 'file
  :group 'superchat)

(defvar superchat-cowork-telega--rules nil
  "Approved triage rules, newest first.
Each rule is a plist with :id, :kind (`mute'), :temex, :rationale,
and :approved-at.")

(defvar superchat-cowork-telega--triage-samples nil
  "Messages sampled for the current triage Run.
Each entry is (:chat-title TITLE :text TEXT :msg RAW-TDLIB-MSG); the
approval preview replays proposed rules against them.")

(defun superchat-cowork-telega--lint-temex (temex)
  "Signal unless TEMEX is a purely declarative match expression.
Telega's temex language includes `eval' and `call' matchers that
execute arbitrary code; a proposed rule is data, never a program, so
those are rejected anywhere in the tree.  A version-locked whitelist
of all matchers would break across telega releases; excluding the only
two code-execution forms expresses the same intent durably."
  (cond
   ((or (symbolp temex) (stringp temex) (numberp temex)) temex)
   ((consp temex)
    (let ((head (car temex)))
      (when (memq head '(eval call))
        (error "Rule temex must be declarative; `%s' is not allowed" head))
      (dolist (element (cdr temex))
        (superchat-cowork-telega--lint-temex element))
      temex))
   (t (error "Invalid temex element: %S" temex))))

(defun superchat-cowork-telega--parse-temex (temex-string)
  "Read TEMEX-STRING into a linted temex, or signal."
  (unless (and (stringp temex-string)
               (not (string-empty-p (string-trim temex-string))))
    (error "Rule temex must be a non-empty s-expression string"))
  (let* ((parsed (car (read-from-string temex-string))))
    (superchat-cowork-telega--lint-temex parsed)
    parsed))

(defun superchat-cowork-telega--load-rules ()
  "Load rules from `superchat-cowork-telega-rules-file'."
  (setq superchat-cowork-telega--rules
        (when (file-readable-p superchat-cowork-telega-rules-file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents superchat-cowork-telega-rules-file)
                (car (read-from-string (buffer-string))))
            (error nil)))))

(defun superchat-cowork-telega--save-rules ()
  "Persist the current rules to `superchat-cowork-telega-rules-file'."
  (let ((directory (file-name-directory superchat-cowork-telega-rules-file)))
    (when directory
      (make-directory directory t)))
  (with-temp-file superchat-cowork-telega-rules-file
    (insert ";; Superchat Telega triage rules — edited via "
            "M-x superchat-telega-rules\n")
    (prin1 superchat-cowork-telega--rules (current-buffer))
    (insert "\n")))

(defun superchat-cowork-telega--mute-p (msg)
  "Ignore-predicate consulting the approved mute rules."
  (and (fboundp 'telega-msg-match-p)
       (cl-some
        (lambda (rule)
          (and (eq (plist-get rule :kind) 'mute)
               (ignore-errors
                 (telega-msg-match-p msg (plist-get rule :temex)))))
        superchat-cowork-telega--rules)))

(defvar superchat-cowork-telega--hooks-installed nil)

(defun superchat-cowork-telega--install-hooks ()
  "Wire the rule predicates into telega's native seams, once."
  (when (and (not superchat-cowork-telega--hooks-installed)
             (boundp 'telega-msg-ignore-predicates))
    (add-hook 'telega-msg-ignore-predicates
              #'superchat-cowork-telega--mute-p)
    (setq superchat-cowork-telega--hooks-installed t)))

(superchat-cowork-telega--load-rules)
(if (featurep 'telega)
    (superchat-cowork-telega--install-hooks)
  (with-eval-after-load 'telega
    (superchat-cowork-telega--install-hooks)))

;; ── Rule proposal tool ────────────────────────────────────────

(defun superchat-cowork-telega--replay (temex)
  "Return (MATCHES . TOTAL) replaying TEMEX over the triage samples."
  (let ((matches '()))
    (when (fboundp 'telega-msg-match-p)
      (dolist (sample superchat-cowork-telega--triage-samples)
        (when (ignore-errors
                (telega-msg-match-p (plist-get sample :msg) temex))
          (push sample matches))))
    (cons (nreverse matches)
          (length superchat-cowork-telega--triage-samples))))

(defun superchat-cowork-telega--preview-rule (request)
  "Show the proposed rule with its retroactive replay."
  (let* ((args (plist-get request :args))
         (temex-string (superchat-authority-effect-argument args "temex" 0))
         (rationale (superchat-authority-effect-argument args "rationale" 1))
         (temex (ignore-errors
                  (car (read-from-string temex-string))))
         (replay (and temex (superchat-cowork-telega--replay temex))))
    (with-help-window "*Superchat Telega Rule*"
      (with-current-buffer standard-output
        (insert (format "Proposed mute rule:\n\n  %s\n\nRationale: %s\n"
                        temex-string (or rationale "-")))
        (when replay
          (insert (format "\nReplay: would have matched %d of %d sampled messages.\n"
                          (length (car replay)) (cdr replay)))
          (dolist (sample (seq-take (car replay) 5))
            (insert (format "  - [%s] %s\n"
                            (plist-get sample :chat-title)
                            (truncate-string-to-width
                             (or (plist-get sample :text) "") 70
                             nil nil "…")))))))))

(defun superchat-cowork-telega--propose-rule (temex-string rationale)
  "Persist one approved triage rule and apply it immediately.
Authority approval (with replay preview) is owned by
`superchat-tool-execute'; this body runs only after the user said yes."
  (let* ((temex (superchat-cowork-telega--parse-temex temex-string))
         (rule (list :id (format "rule-%s-%04x"
                                 (format-time-string "%Y%m%d%H%M%S")
                                 (random #x10000))
                     :kind 'mute
                     :temex temex
                     :rationale (or rationale "")
                     :approved-at (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (push rule superchat-cowork-telega--rules)
    (superchat-cowork-telega--save-rules)
    (superchat-cowork-telega--install-hooks)
    (superchat-tape-append-event
     (list :type "rule.approved"
           :session-id "telega-triage"
           :actor-id "user"
           :summary (format "rule.approved: %s %s"
                            "mute" (plist-get rule :id))
           :payload (list :id (plist-get rule :id)
                          :kind "mute"
                          :temex (prin1-to-string temex)
                          :rationale (or rationale ""))))
    (format "Rule %s added (mute): %s"
            (plist-get rule :id) temex-string)))

(defun superchat-cowork-telega--rule-effects (_args)
  "Declare the local rule-store write performed by a proposal."
  '((:effect local-write :resource telega-rules
             :scope "telega-rules/mute")))

(superchat-register-tool
 "telega_propose_rules"
 :function #'superchat-cowork-telega--propose-rule
 :description "Propose ONE mute rule as a declarative telega temex \
s-expression.  The user reviews a replay of the rule against sampled \
messages and must approve it.  Never use eval or call in the temex."
 :args (list (list :name "temex" :type 'string
                   :description "Declarative match expression, e.g. (and (chat (ids 123)) (contains \"lottery\")).")
             (list :name "rationale" :type 'string
                   :description "One line explaining the evidence."
                   :optional t))
 :effects #'superchat-cowork-telega--rule-effects
 :approval-preview #'superchat-cowork-telega--preview-rule)

;; ── Extension declaration (activation-scoped) ─────────────────

(defcustom superchat-cowork-telega-agenda-function
  #'superchat-cowork-telega--org-agenda-snippet
  "Function returning a short upcoming-agenda string, or nil.
Called with no arguments when the user's request looks like it is
about scheduling; the result is injected as deterministic context so
the model can answer availability questions without a tool round."
  :type 'function
  :group 'superchat)

(defconst superchat-cowork-telega--schedule-regexp
  (concat "\\(?:有空\\|几点\\|时间\\|明天\\|今天\\|后天\\|周[一二三四五六日末]\\|"
          "available\\|schedule\\|meet\\|appointment\\|tomorrow\\|"
          "\\_<when\\_>\\|\\_<time\\_>\\)")
  "Heuristic for requests that are probably about scheduling.")

(defun superchat-cowork-telega--org-agenda-snippet ()
  "Return the next three days of Org agenda headlines, or nil."
  (when (and (require 'org nil t)
             (fboundp 'org-agenda-get-day-entries)
             (bound-and-true-p org-agenda-files))
    (ignore-errors
      (let* ((today (calendar-current-date))
             (lines
              (cl-loop
               for offset from 0 to 2
               for date = (calendar-gregorian-from-absolute
                           (+ (calendar-absolute-from-gregorian today)
                              offset))
               append
               (cl-loop
                for file in (org-agenda-files)
                append
                (mapcar
                 (lambda (entry)
                   (format "%04d-%02d-%02d %s"
                           (nth 2 date) (nth 0 date) (nth 1 date)
                           (substring-no-properties
                            (or (get-text-property 0 'txt entry) entry))))
                 (org-agenda-get-day-entries file date))))))
        (when lines
          (string-join lines "\n"))))))

(defun superchat-cowork-telega--schedule-context (request)
  "Inject the user's agenda when REQUEST looks like a scheduling reply."
  (let ((intent (or (plist-get request :input)
                    (plist-get request :prompt)
                    "")))
    (when (string-match-p superchat-cowork-telega--schedule-regexp intent)
      (when-let* ((agenda (and (functionp
                                superchat-cowork-telega-agenda-function)
                               (funcall
                                superchat-cowork-telega-agenda-function))))
        (concat "The user's upcoming agenda (for availability answers):\n"
                agenda)))))

(defun superchat-cowork-telega--digest-policy (effects _context)
  "Tighten-only policy: a digest Run must never write outward."
  (if (cl-some (lambda (effect)
                 (eq (plist-get effect :effect) 'external-write))
               effects)
      'deny
    'defer))

(defun superchat-cowork-telega--triage-policy (effects _context)
  "Tighten-only policy for triage Runs.
No external writes, no code execution; the only permitted local write
is the triage rule store itself (which still asks with a preview)."
  (catch 'decision
    (dolist (effect effects 'defer)
      (let ((kind (plist-get effect :effect)))
        (cond
         ((eq kind 'external-write) (throw 'decision 'deny))
         ((memq kind '(process-exec runtime-exec)) (throw 'decision 'deny))
         ((and (eq kind 'local-write)
               (not (eq (plist-get effect :resource) 'telega-rules)))
          (throw 'decision 'deny)))))))

(defun superchat-cowork-telega--rules-summary ()
  "Return a human-readable summary of the approved rules."
  (if (null superchat-cowork-telega--rules)
      "No Telega triage rules are approved."
    (mapconcat
     (lambda (rule)
       (format "- %s [%s] %s — %s"
               (plist-get rule :id)
               (plist-get rule :kind)
               (prin1-to-string (plist-get rule :temex))
               (let ((rationale (plist-get rule :rationale)))
                 (if (string-empty-p (or rationale "")) "-" rationale))))
     superchat-cowork-telega--rules "\n")))

;;;###autoload
(defun superchat-cowork-telega--cmd-rules (_cmd _args _input _lang _model)
  "List the approved Telega triage rules."
  (list :type :echo :content (superchat-cowork-telega--rules-summary)))

(defun superchat-cowork-telega--extension-knowledge (request)
  "Return tool-shaped workflow guidance for REQUEST."
  (let ((tools (plist-get request :tools)))
    (cond
     ((and (fboundp 'superchat-cowork-telega-skill-body)
           (equal
            "cowork-session.telega"
            (and (plist-get request :preset)
                 (superchat-preset-profile-id
                  (plist-get request :preset)))))
      (superchat-cowork-telega-skill-body))
     ((and (listp tools)
           (member superchat-cowork-telega-tool-name tools))
      (concat
       "Compose one complete reply, then call telega_send_message exactly "
       "once with the Chat ID from the request context.  The user approves "
       "the complete draft before it is sent."))
     ((and (listp tools)
           (member "telega_propose_rules" tools))
      (concat
       "Ground every proposed rule in sampled evidence.  Propose one "
       "declarative temex rule per telega_propose_rules call."))
     ((and (listp tools)
           (member superchat-cowork-telega-read-tool-name tools))
      (concat
       "Read only the granted chats with telega_read_messages.  Preserve "
       "the ⟨telega:...⟩ links when referring to specific messages."))
     (t
      "Use only the explicitly available Telega capabilities."))))

;; The digest and triage flows submit envelopes on their own Surfaces
;; (`superchat-cowork-telega-session.el' registers `telega').
(dolist (surface '(telega-digest telega-triage))
  (unless (memq surface superchat-pipeline--surfaces)
    (superchat-register-surface surface)))

(superchat-define-extension 'telega-cowork
  :description
  "Telegram coworker: drafts or sends replies with approval, digests granted chats read-only, and proposes triage rules the user approves."
  :active-when
  (lambda (request)
    (memq (plist-get (plist-get request :origin) :surface)
          '(telega telega-digest telega-triage)))
  :tools (list superchat-cowork-telega-tool-name
               superchat-cowork-telega-read-tool-name
               "telega_propose_rules"
               "memory_search")
  :context #'superchat-cowork-telega--schedule-context
  :knowledge #'superchat-cowork-telega--extension-knowledge
  :policies (list (cons 'telega-digest
                        #'superchat-cowork-telega--digest-policy)
                  (cons 'telega-triage
                        #'superchat-cowork-telega--triage-policy))
  :commands (list (cons "telega-rules"
                        #'superchat-cowork-telega--cmd-rules)))

;; ── Cowork profile and request ────────────────────────────────

(defun superchat-cowork-telega--profile (autonomy)
  "Return the participant Agent Profile for AUTONOMY level."
  (superchat-preset-from-plist
   (if (eq autonomy 'draft)
       (list :name "Telega Cowork Draft"
             :profile-id "cowork.telega"
             :type 'agent
             :capability "cowork"
             :tools 'none
             :body
             (concat
              "You cowork inside the user's current Telega chat. Return "
              "ONLY the complete reply text, nothing else: Superchat "
              "places it into the input area for the user to edit and "
              "send themselves.")
             :source 'plugin)
     (list :name "Telega Cowork"
           :profile-id "cowork.telega"
           :type 'agent
           :capability "cowork"
           :tools (list superchat-cowork-telega-tool-name "memory_search")
           :body
           (concat
            "You cowork inside the user's current Telega chat. Follow the "
            "active extension briefing; after the send tool returns, state "
            "the outcome in one short sentence.")
           :source 'plugin))))

(defun superchat-cowork-telega--context ()
  "Return attended context from the current Telega buffer."
  (cond
   ((use-region-p)
    (let ((text (string-trim
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))))
      (when (string-empty-p text)
        (user-error "Select a Telega message or non-empty text region first"))
      (list :text text)))
   ((fboundp 'telega-msg-at)
    (let* ((message (telega-msg-at (point)))
           (text (and message
                      (telega-msg-content-text message 'with-speech)))
           (sender (and message
                        (fboundp 'telega-msg-sender)
                        (telega-msg-sender message)))
           (sender-title (and sender
                              (fboundp 'telega-msg-sender-title)
                              (telega-msg-sender-title sender))))
      (unless (and (stringp text) (not (string-empty-p text)))
        (user-error "Select a Telega message or text region first"))
      (list :text (if sender-title
                      (format "%s: %s"
                              (substring-no-properties sender-title) text)
                    text)
            :message-id (plist-get message :id))))
   (t (user-error "Select a Telega message or text region first"))))

(defun superchat-cowork-telega--request (chat context instruction on-finish
                                              &optional autonomy)
  "Build the cowork Runtime Request for CHAT, CONTEXT, and INSTRUCTION.
ON-FINISH receives (RESPONSE STATUS) at any terminal Run state.
AUTONOMY defaults to `superchat-cowork-telega-autonomy'; at `draft' the
Request carries an explicit empty tool surface that activation
composition never widens."
  (let* ((autonomy (or autonomy superchat-cowork-telega-autonomy))
         (expected-chat-id
          (superchat-cowork-telega--chat-id (plist-get chat :id)))
         (chat-id (number-to-string expected-chat-id))
         (title (superchat-cowork-telega--chat-title chat))
         (context-text (plist-get context :text))
         (session-id (format "telega-chat-%s" chat-id))
         (buffer (superchat-cowork-telega--request-buffer
                  session-id 'telega-reply-protocol))
         (message-id (and (plist-get context :message-id)
                          (format "%s" (plist-get context :message-id))))
         (validator
          (lambda (args)
            (let ((requested
                   (superchat-cowork-telega--chat-id
                    (superchat-authority-effect-argument
                     args "chat_id" 0))))
              (unless (= requested expected-chat-id)
                (error "Telega send target %s differs from current chat %s"
                       requested expected-chat-id))))))
    (append
     (when (eq autonomy 'draft)
       ;; Draft mode composes text only; the explicit empty surface is a
       ;; narrowing decision extension composition never widens.
       (list :tools 'none))
     (list
     :input instruction
     :session-id session-id
     :prompt (format
              (concat "User request:\n%s\n\nTelega chat: %s\nChat ID: %s\n\n"
                      "Selected context:\n%s")
              instruction title chat-id context-text)
     :preset (superchat-cowork-telega--profile autonomy)
     :actor-id "cowork"
     :capability "cowork"
     :topology 'single
     :authority-policy "cowork"
     :attendance 'attended
     ;; In `approve' mode there is no explicit :tools — activation
     ;; composition supplies the surface (core verbs + this extension's
     ;; send and memory tools).
     :tool-validator validator
     :origin (append
              (list :surface 'telega
                    :chat-id chat-id
                    :chat-title title
                    :uri (if message-id
                             (format "telega://chat/%s/message/%s"
                                     chat-id message-id)
                           (format "telega://chat/%s" chat-id)))
              (when message-id (list :message-id message-id)))
     :buffer buffer
     :stream-function #'ignore
     :delivery-function
     (lambda (response status _tape-id)
       (funcall on-finish response status))))))

(defun superchat-cowork-telega--insert-draft (buffer draft)
  "Place DRAFT into the Telega input area of BUFFER."
  (if (and (buffer-live-p buffer)
           (fboundp 'telega-chatbuf-input-insert))
      (with-current-buffer buffer
        (telega-chatbuf-input-insert draft)
        (message "Superchat Telega: draft ready — edit and send with RET"))
    (message "Superchat Telega draft:\n%s" draft)))

;;;###autoload
(defun superchat-cowork-telega-reply (instruction)
  "Run the compatibility reply action under INSTRUCTION.
For the canonical contextual experience use `superchat-cowork-telega'.
At autonomy `approve' the model sends through the audited
`telega_send_message' tool after you approve the full draft; at
`draft' the reply lands in the input area for you to edit and send.
Runs asynchronously either way."
  (interactive
   (list (read-string
          "Cowork request: "
          "Reply to the selected message appropriately and send it")))
  (unless (and (boundp 'telega-chatbuf--chat) telega-chatbuf--chat)
    (user-error "Run this command from a Telega chat buffer"))
  (let* ((chatbuf (current-buffer))
         (autonomy superchat-cowork-telega-autonomy)
         (request
          (superchat-cowork-telega--request
           telega-chatbuf--chat
           (superchat-cowork-telega--context)
           instruction
           (if (eq autonomy 'draft)
               (lambda (response status)
                 (if (eq status 'completed)
                     (superchat-cowork-telega--insert-draft
                      chatbuf (string-trim (or response "")))
                   (message "Superchat Telega cowork %s: %s"
                            status response)))
             (lambda (response status)
               (if (eq status 'completed)
                   (message "Superchat Telega cowork: %s"
                            (string-trim (or response "")))
                 (message "Superchat Telega cowork %s: %s"
                          status response))))
           autonomy)))
    (prog1 (superchat-pipeline-submit
            (superchat-pipeline-envelope-from-request
             (superchat-cowork-telega--compose-request request)))
      (message "Superchat Telega cowork: working asynchronously..."))))

;; ── Digest (analyst role) ─────────────────────────────────────

(defun superchat-cowork-telega--scope-approval (granted-scopes)
  "Return an approval function auto-allowing reads within GRANTED-SCOPES.
Any other effect — a different chat, a write, an unknown — falls back
to the interactive default approval."
  (lambda (request)
    (let ((effects (plist-get request :effects)))
      (if (and effects
               (cl-every
                (lambda (effect)
                  (and (eq (plist-get effect :effect) 'external-read)
                       (member (plist-get effect :scope) granted-scopes)))
                effects))
          t
        (funcall superchat-authority-approval-function request)))))

(defun superchat-cowork-telega--digest-profile ()
  "Return the read-only analyst Agent Profile."
  (superchat-preset-from-plist
   (list :name "Telega Digest"
         :profile-id "digest.telega"
         :type 'agent
         :capability "digest"
         :tools (list superchat-cowork-telega-read-tool-name
                      "memory_search")
         :body
         (concat
          "You digest Telegram chats the user granted for this Run. "
          "Read each granted chat with telega_read_messages, then produce "
          "a concise Org-mode summary grouped by chat.  Preserve the "
          "⟨telega:...⟩ link of every message you cite.  Report facts "
          "from the messages only; never invent content, never draft or "
          "send replies.")
         :source 'plugin)))

(defun superchat-cowork-telega--digest-request (chats instruction on-finish)
  "Build the read-only digest Request over CHATS for INSTRUCTION.
ON-FINISH receives (RESPONSE STATUS) at any terminal Run state."
  (let* ((session-id "telega-digest")
         (buffer (superchat-cowork-telega--request-buffer
                  session-id 'telega-digest-protocol))
         (ids (mapcar (lambda (chat)
                        (superchat-cowork-telega--chat-id
                         (plist-get chat :id)))
                      chats))
         (granted-scopes
          (mapcar (lambda (id) (format "telega://chat/%s" id)) ids))
         (roster
          (mapconcat
           (lambda (chat)
             (format "- %s (chat_id %s)"
                     (superchat-cowork-telega--chat-title chat)
                     (superchat-cowork-telega--chat-id
                      (plist-get chat :id))))
           chats "\n")))
    (list
     :input instruction
     :session-id session-id
     :prompt (format
              (concat "User request:\n%s\n\nGranted chats:\n%s\n\n"
                      "Read each granted chat, then summarize.")
              instruction roster)
     :preset (superchat-cowork-telega--digest-profile)
     :actor-id "digest"
     :capability "digest"
     :topology 'single
     :authority-policy "telega-digest"
     :attendance 'attended
     :tools (list superchat-cowork-telega-read-tool-name "memory_search")
     :approval-function (superchat-cowork-telega--scope-approval
                         granted-scopes)
     :origin (list :surface 'telega-digest :chat-ids ids)
     :buffer buffer
     :stream-function #'ignore
     :delivery-function
     (lambda (response status _tape-id)
       (funcall on-finish response status)))))

(defun superchat-cowork-telega--show-digest (response)
  "Show digest RESPONSE in the dedicated Org buffer."
  (let ((buffer (get-buffer-create "*telega-digest*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (insert (format "#+title: Telega Digest — %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M")))
        (insert response)
        (unless (bolp) (insert "\n"))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun superchat-telega-digest (instruction)
  "Run the specialized read-only digest action under INSTRUCTION.
For the canonical contextual experience use `superchat-cowork-telega'.
You pick the chats and grant read access once; the Run reads only
those chats (any other effect falls back to interactive approval),
cannot write outward by policy, and delivers an Org summary with
per-message links.  The granted scope is recorded on the Tape."
  (interactive
   (list (read-string
          "Digest instruction: "
          "Summarize what happened, grouped by chat, with links")))
  (unless (fboundp 'telega-completing-read-chat-list)
    (user-error "Telega is not loaded"))
  (let* ((chats (telega-completing-read-chat-list "Digest chats"))
         (titles (mapcar #'superchat-cowork-telega--chat-title chats))
         (ids (mapcar (lambda (chat)
                        (superchat-cowork-telega--chat-id
                         (plist-get chat :id)))
                      chats)))
    (unless chats
      (user-error "No chats selected"))
    (unless (y-or-n-p
             (format "Grant this digest read access to %d chat(s) — %s? "
                     (length titles)
                     (string-join (seq-take titles 6) ", ")))
      (user-error "Digest cancelled"))
    (superchat-tape-append-event
     (list :type "digest.scope-granted"
           :session-id "telega-digest"
           :actor-id "user"
           :summary (format "digest.scope-granted: %d chats" (length ids))
           :payload (list :chat-ids ids)))
    (prog1 (superchat-pipeline-submit
            (superchat-pipeline-envelope-from-request
            (superchat-cowork-telega--compose-request
             (superchat-cowork-telega--digest-request
              chats instruction
              (lambda (response status)
                (if (eq status 'completed)
                    (superchat-cowork-telega--show-digest response)
                  (message "Telega digest %s: %s" status response)))))))
      (message "Telega digest: reading %d chat(s) asynchronously..."
               (length chats)))))

;; ── Triage flow (gatekeeper role) ─────────────────────────────

(defun superchat-cowork-telega--collect-samples (chats per-chat callback)
  "Asynchronously sample PER-CHAT recent messages from CHATS.
CALLBACK receives the sample list once every chat answered."
  (if (null chats)
      (funcall callback nil)
    (let ((remaining (length chats))
          (samples '()))
      (dolist (chat chats)
        (let ((chat chat))
          (telega--getChatHistory chat 0 0 per-chat nil
            (lambda (history)
              (condition-case nil
                  (dolist (msg (append (plist-get history :messages) nil))
                    (push (list :chat-title
                                (superchat-cowork-telega--chat-title chat)
                                :text
                                (or (and (fboundp 'telega-msg-content-text)
                                         (ignore-errors
                                           (substring-no-properties
                                            (telega-msg-content-text msg))))
                                    "[non-text content]")
                                :msg msg)
                          samples))
                (error nil))
              (when (zerop (cl-decf remaining))
                (funcall callback (nreverse samples))))))))))

(defun superchat-cowork-telega--sample-lines (samples)
  "Format SAMPLES for the triage prompt, bounded in size."
  (string-join
   (mapcar (lambda (sample)
             (format "- [%s] %s"
                     (plist-get sample :chat-title)
                     (truncate-string-to-width
                      (or (plist-get sample :text) "") 160 nil nil "…")))
           (seq-take samples 120))
   "\n"))

(defun superchat-cowork-telega--triage-profile ()
  "Return the rule-proposing gatekeeper Agent Profile."
  (superchat-preset-from-plist
   (list :name "Telega Triage"
         :profile-id "triage.telega"
         :type 'agent
         :capability "triage"
         :tools '("telega_propose_rules")
         :body
         (concat
          "You review sampled Telegram messages and the user's existing "
          "triage rules, then propose new declarative temex mute rules "
          "for recurring noise.  One telega_propose_rules call per rule, "
          "each grounded in the sampled evidence.  Never use eval or call "
          "in a temex.  If nothing warrants a rule, say so and stop.")
         :source 'plugin)))

(defun superchat-cowork-telega--triage-request (samples instruction on-finish)
  "Build the triage Request over SAMPLES for INSTRUCTION."
  (let* ((session-id "telega-triage")
         (buffer (superchat-cowork-telega--request-buffer
                  session-id 'telega-triage-protocol))
         (validator
         (lambda (args)
           (superchat-cowork-telega--parse-temex
            (superchat-authority-effect-argument args "temex" 0)))))
    (list
     :input instruction
     :session-id session-id
     :prompt (format
              (concat "User request:\n%s\n\nCurrent rules:\n%s\n\n"
                      "Sampled messages:\n%s")
              instruction
              (superchat-cowork-telega--rules-summary)
              (superchat-cowork-telega--sample-lines samples))
     :preset (superchat-cowork-telega--triage-profile)
     :actor-id "triage"
     :capability "triage"
     :topology 'single
     :authority-policy "telega-triage"
     :attendance 'attended
     :tools '("telega_propose_rules")
     :tool-validator validator
     :origin '(:surface telega-triage)
     :buffer buffer
     :stream-function #'ignore
     :delivery-function
     (lambda (response status _tape-id)
       (funcall on-finish response status)))))

;;;###autoload
(defun superchat-telega-triage (instruction)
  "Run the specialized triage action under INSTRUCTION.
For the canonical contextual experience use `superchat-cowork-telega'.
You pick the chats and consent to the sampling once; the Run can only
call `telega_propose_rules', every proposal is approved individually
with a retroactive replay preview, and outward writes are denied by
policy.  Approved rules apply immediately and persist across sessions."
  (interactive
   (list (read-string
          "Triage instruction: "
          "Propose mute rules for recurring noise")))
  (unless (fboundp 'telega-completing-read-chat-list)
    (user-error "Telega is not loaded"))
  (let* ((chats (telega-completing-read-chat-list "Triage chats"))
         (titles (mapcar #'superchat-cowork-telega--chat-title chats))
         (ids (mapcar (lambda (chat)
                        (superchat-cowork-telega--chat-id
                         (plist-get chat :id)))
                      chats)))
    (unless chats
      (user-error "No chats selected"))
    (unless (y-or-n-p
             (format "Sample recent messages from %d chat(s) — %s — for rule proposals? "
                     (length titles)
                     (string-join (seq-take titles 6) ", ")))
      (user-error "Triage cancelled"))
    (superchat-tape-append-event
     (list :type "triage.scope-granted"
           :session-id "telega-triage"
           :actor-id "user"
           :summary (format "triage.scope-granted: %d chats" (length ids))
           :payload (list :chat-ids ids)))
    (superchat-cowork-telega--collect-samples
     chats 30
     (lambda (samples)
       (setq superchat-cowork-telega--triage-samples samples)
       (superchat-pipeline-submit
        (superchat-pipeline-envelope-from-request
        (superchat-cowork-telega--compose-request
         (superchat-cowork-telega--triage-request
          samples instruction
          (lambda (response status)
            (if (eq status 'completed)
                (message "Telega triage: %s"
                         (string-trim (or response "")))
              (message "Telega triage %s: %s" status response)))))))))
    (message "Telega triage: sampling %d chat(s) asynchronously..."
             (length chats))))

;;;###autoload
(defun superchat-telega-rules ()
  "Show the approved Telega triage rules."
  (interactive)
  (message "%s" (superchat-cowork-telega--rules-summary)))

;;;###autoload
(defun superchat-telega-rule-remove (id)
  "Remove the approved triage rule named ID."
  (interactive
   (list (completing-read
          "Remove rule: "
          (mapcar (lambda (rule) (plist-get rule :id))
                  superchat-cowork-telega--rules)
          nil t)))
  (let ((rule (cl-find id superchat-cowork-telega--rules
                       :key (lambda (rule) (plist-get rule :id))
                       :test #'equal)))
    (unless rule
      (user-error "No such rule: %s" id))
    (setq superchat-cowork-telega--rules
          (cl-remove rule superchat-cowork-telega--rules))
    (superchat-cowork-telega--save-rules)
    (superchat-tape-append-event
     (list :type "rule.removed"
           :session-id "telega-triage"
           :actor-id "user"
           :summary (format "rule.removed: %s" id)
           :payload (list :id id)))
    (message "Removed rule %s." id)))

(provide 'superchat-cowork-telega-legacy)

;;; superchat-cowork-telega-legacy.el ends here
