;;; superchat-core.el — Turn struct + pipeline runner for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; The Turn struct (the chat surface's parsed input) and its parser.  Prompt
;; building belongs to the driver, which collects the chat collectors'
;; fragments through the `collect-context' Event and assembles them with the
;; core assembler (superchat-prompt-hooks.el registers the chat collectors);
;; the old hook lists are gone (v2, spec §1.5).

;;; Code:

(require 'cl-lib)
(require 'superchat-prompt)

(declare-function superchat--extract-file-path "superchat-dispatcher" (input))
(defvar superchat--file-ref-regexp)

;; ═══════════════════════════════════════════════════════════
;; Turn struct
;; ═══════════════════════════════════════════════════════════

(cl-defstruct (superchat-turn
               (:constructor superchat-turn--create)
               (:copier superchat-turn-copy))
  (id nil :read-only t)
  (session-id nil :read-only t)
  (origin nil)
  (inbound "")
  (clean-input "")
  (target-model nil)
  (skill nil)
  (preset nil)
  (command nil)
  (command-args "")
  (context-files nil)
  (system-prompt "")
  (prompt "")
  (tools nil)
  (retrieved-memories nil)
  (streaming-parts nil)
  (llm-result "")
  (error nil)
  (state nil))

(defun superchat-turn-new (inbound &optional session-id)
  "Create a fresh turn for INBOUND text, with optional SESSION-ID."
  (superchat-turn--create
   :id (format "%d-%x" (time-convert nil 'integer) (random #xffff))
   :session-id (or session-id "default")
   :inbound (string-trim (or inbound ""))
   :clean-input (string-trim (or inbound ""))
   :state nil))

(defun superchat-core--body-token (text sigil name-regexp known-p)
  "Return (NAME . REST) for the first SIGIL token in TEXT that KNOWN-P accepts.
The token stands alone: the start of TEXT or whitespace before it, whitespace
or the end of TEXT after it.  NAME-REGEXP matches a name; KNOWN-P is called
with one.  A name KNOWN-P refuses stays ordinary text.  REST is TEXT without
the token."
  (let ((regexp (concat "\\(?:\\`\\|[[:space:]]\\)\\(" (regexp-quote sigil)
                        "\\(" name-regexp "\\)\\)\\(?:[[:space:]]\\|\\'\\)"))
        (case-fold-search nil)
        (start 0)
        found)
    (while (and (not found) (string-match regexp text start))
      (let ((name (match-string 2 text))
            (token-start (match-beginning 1))
            (token-end (match-end 1)))
        (if (funcall known-p name)
            (let ((before (substring text 0 token-start))
                  (after (substring text token-end)))
              (setq found
                    (cons name
                          (string-trim
                           (concat before
                                   (if (string-match-p "[[:space:]]\\'" before)
                                       (replace-regexp-in-string
                                        "\\`[ \t]+" "" after)
                                     after))))))
          (setq start token-end))))
    found))

(defun superchat-core--known-skill-p (name)
  "Return non-nil when NAME is an existing skill."
  (and (fboundp 'superchat-skills-exists-p)
       (ignore-errors (superchat-skills-exists-p name))))

(defun superchat-core--known-model-p (name)
  "Return non-nil when NAME is a model known without asking the provider."
  (and (fboundp 'superchat--models-known-without-fetch)
       (member name (ignore-errors (superchat--models-known-without-fetch)))
       t))

(defun superchat-core--known-command-p (name)
  "Return non-nil when NAME is an existing command."
  (and (fboundp 'superchat--get-all-command-names)
       (member (concat "/" name)
               (ignore-errors (superchat--get-all-command-names)))
       t))

(defun superchat-core--parse-input (turn)
  "Parse model switch, skill, command, and file refs from TURN's inbound.
`@model', `>skill' and `/command' are read at the start of the input as
before.  When the user sent the message, each is also read once from the
middle of the text, there only when its name exists; see
docs/decisions/proposed/2026-10-10-triggers-anywhere-in-the-message.md."
  (let ((input (superchat-turn-inbound turn))
        ;; A timer's task and a woken Work Record send text the model wrote.
        (body-p (not (bound-and-true-p superchat-automated-input))))
    ;; @model
    (when-let* ((parsed
                 (or (and (fboundp 'superchat-parser-model-switch)
                          (superchat-parser-model-switch input))
                     (and body-p
                          (when-let* ((found (superchat-core--body-token
                                              input "@" "[a-zA-Z0-9_./:-]+"
                                              #'superchat-core--known-model-p)))
                            (cons (cdr found) (car found)))))))
      (setf (superchat-turn-clean-input turn) (car parsed))
      (setf (superchat-turn-target-model turn) (cdr parsed)))
    ;; >>workflow — dedicated workflow prefix: `>' invokes skills,
    ;; `>>' invokes .workflow recipes.  Rewrites to the reserved
    ;; `workflow' skill token so the dispatcher's workflow branch
    ;; handles it: ">>research foo" ≡ ">workflow research foo".
    (let ((ci (superchat-turn-clean-input turn)))
      (when (and (stringp ci)
                 (string-match "\\`>>\\s-*" ci))
        (setf (superchat-turn-skill turn) "workflow")
        (setf (superchat-turn-clean-input turn)
              (string-trim (substring ci (match-end 0))))))
    ;; >skill
    (unless (superchat-turn-skill turn)
      (let ((ci (superchat-turn-clean-input turn)))
        (if-let* ((parsed (and (fboundp 'superchat-skills-parse-input)
                               (superchat-skills-parse-input ci))))
            (progn
              (setf (superchat-turn-skill turn) (car parsed))
              (setf (superchat-turn-clean-input turn)
                    (replace-regexp-in-string
                     (concat ">" (regexp-quote (car parsed)) "\\s-*")
                     "" ci)))
          ;; What stands at the start wins: a message that opens with a
          ;; command keeps a later `>name' as its text.
          (when-let* ((found (and body-p (stringp ci)
                                  (not (and (fboundp 'superchat-parser-command)
                                            (superchat-parser-command ci)))
                                  (superchat-core--body-token
                                   ci ">" "[a-zA-Z0-9_-]+"
                                   #'superchat-core--known-skill-p))))
            (setf (superchat-turn-skill turn) (car found))
            (setf (superchat-turn-clean-input turn) (cdr found))
            ;; Named in the middle, an agent skill applies to this message
            ;; only; the dispatcher reads this to leave the Session as it is.
            (setf (superchat-turn-state turn)
                  (plist-put (superchat-turn-state turn)
                             :skill-from-body t))))))
    ;; /command
    (unless (superchat-turn-skill turn)
      (let ((ci (superchat-turn-clean-input turn)))
        (if-let* ((parsed (and (fboundp 'superchat-parser-command)
                               (superchat-parser-command ci))))
            (progn
              (setf (superchat-turn-command turn) (car parsed))
              (setf (superchat-turn-command-args turn) (or (cdr parsed) "")))
          ;; A command found in the middle is the same message with the
          ;; command written first.
          (when-let* ((found (and body-p (stringp ci)
                                  (superchat-core--body-token
                                   ci "/" "[^[:space:]/]+"
                                   #'superchat-core--known-command-p))))
            (setf (superchat-turn-command turn) (car found))
            (setf (superchat-turn-command-args turn) (cdr found))
            (setf (superchat-turn-clean-input turn)
                  (string-trim (concat "/" (car found) " " (cdr found))))))))
    ;; #file
    (when (fboundp 'superchat--extract-file-path)
      (when-let* ((file (ignore-errors
                          (superchat--extract-file-path
                           (superchat-turn-clean-input turn)))))
        (setf (superchat-turn-context-files turn) (list file))))
  turn))

(defun superchat-core--strip-file-ref (turn)
  "Strip a `#file' reference from TURN's clean-input once and record the
path as `:file-ref' in TURN's state.  `superchat-core--parse-input' already
parsed the same path into `turn.context-files'; only the text edit and the
state note happen here (contract §4.3)."
  (when (and (boundp 'superchat--file-ref-regexp)
             (fboundp 'superchat--extract-file-path))
    (let* ((initial-query (string-trim (or (superchat-turn-clean-input turn) "")))
           (match-p (string-match superchat--file-ref-regexp initial-query))
           (path-start (and match-p (match-beginning 0)))
           (path-end (and match-p (match-end 0)))
           (file-path (and match-p (superchat--extract-file-path initial-query))))
      (when file-path
        (let* ((text-before (substring initial-query 0 path-start))
               (text-after (substring initial-query path-end))
               (stripped (string-trim (concat (string-trim text-before) " "
                                               (string-trim text-after)))))
          (setf (superchat-turn-clean-input turn) stripped)
          (setf (superchat-turn-state turn)
                (plist-put (superchat-turn-state turn) :file-ref file-path))))
      file-path)))

(defun superchat-core-prepare-turn (turn)
  "Return a parsed copy of TURN: @model, >skill, /command and #file are
read from `inbound', the `#file' token is stripped from `clean-input' and
recorded as `:file-ref' state.  Prompt collection and assembly are NOT
done here (migration step 3, one prompt path): the driver collects context
through the `collect-context' Event and assembles with the core assembler
on the chat Envelope, whose `:context (:chat-turn TURN …)' carries this
parsed Turn.  The chat surface calls this function directly; it is no
longer reached through a parsing registry."
  (let ((current (superchat-turn-copy turn)))
    (superchat-core--parse-input current)
    (superchat-core--strip-file-ref current)
    current))

(provide 'superchat-core)

;;; superchat-core.el ends here
