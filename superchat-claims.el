;;; superchat-claims.el --- Explicit Claim capture for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Claims are contestable statements backed by durable Tape references.  This
;; module records Claims and human Feedback and derives a replay-only Current
;; View; it does not promote them into Memory.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-db)
(require 'superchat-tape)

(defvar superchat--session-id nil)

(defun superchat-claims--required-string (value label)
  "Return trimmed VALUE, or signal that LABEL is required."
  (unless (stringp value)
    (error "%s must be a non-empty string" label))
  (let ((value (string-trim value)))
    (when (string-empty-p value)
      (error "%s must be a non-empty string" label))
    value))

(defconst superchat-claims--kinds
  '("generic" "attribution" "pattern" "context" "candidate")
  "Closed Claim kinds understood by the experience model.")

(defun superchat-claims--typed-event (reference)
  "Return the unique Tape event named by typed REFERENCE."
  (let* ((session-id
          (superchat-claims--required-string
           (plist-get reference :session-id) "Claim basis session"))
         (event-id
          (superchat-claims--required-string
           (plist-get reference :event-id) "Claim basis event"))
         (matches
          (cl-remove-if-not
           (lambda (event)
             (equal event-id (plist-get event :event-id)))
           (superchat-tape-replay-events session-id))))
    (unless (= 1 (length matches))
      (error "Claim basis event must resolve exactly once: %S" reference))
    (car matches)))

(defun superchat-claims--normalize-basis (basis)
  "Return validated legacy or typed Tape references from BASIS.
An empty BASIS is allowed and records an unsupported Claim."
  (unless (listp basis)
    (error "Claim basis must be a list of Tape references"))
  (mapcar
   (lambda (reference)
     (cond
      ((stringp reference)
       (let ((reference
              (superchat-claims--required-string
               reference "Claim basis reference")))
         (unless (string-match "\\`tape:\\([1-9][0-9]*\\)\\'" reference)
           (error "Claim basis reference must use tape:N: %s" reference))
         (unless (superchat-db-tape-select
                  "SELECT 1 FROM tape WHERE id = ? LIMIT 1"
                  (list (string-to-number (match-string 1 reference))))
           (error "Unknown Tape fact: %s" reference))
         reference))
      ((and (consp reference)
            (cl-evenp (length reference))
            (cl-loop for (key _value) on reference by #'cddr
                     always (keywordp key)))
       (let* ((session-id
               (superchat-claims--required-string
                (plist-get reference :session-id) "Claim basis session"))
              (event-id
               (when-let* ((value (plist-get reference :event-id)))
                 (superchat-claims--required-string
                  value "Claim basis event")))
              (storage-id (plist-get reference :storage-id)))
         (unless (and (= 4 (length reference))
                      (xor event-id storage-id))
           (error "Typed Claim basis must contain one event or storage ID: %S"
                  reference))
         (if event-id
             (progn
               (superchat-claims--typed-event reference)
               (list :session-id session-id :event-id event-id))
           (unless (superchat-tape-storage-reference-p session-id storage-id)
             (error "Unknown typed Tape fact: %S" reference))
           (list :session-id session-id :storage-id storage-id))))
      (t (error "Invalid Claim basis reference: %S" reference))))
   basis))

(defun superchat-claims--normalize-kind (kind)
  "Return canonical Claim KIND or signal an error."
  (let ((kind (if (symbolp kind) (symbol-name kind)
                (superchat-claims--required-string kind "Claim kind"))))
    (unless (member kind superchat-claims--kinds)
      (error "Unknown Claim kind: %s" kind))
    kind))

(defun superchat-claims--claim-event (claim-id events)
  "Return CLAIM-ID's Claim event from EVENTS, or signal an error."
  (let ((event
         (cl-find claim-id events
                  :key (lambda (candidate)
                         (plist-get candidate :event-id))
                  :test #'equal)))
    (unless (and event (equal "claim.recorded" (plist-get event :type)))
      (error "Unknown Claim: %s" claim-id))
    event))

(defun superchat-claims--normalize-supersedes (supersedes scope events)
  "Validate SUPERSEDES against a Claim in SCOPE among EVENTS."
  (when supersedes
    (let* ((supersedes
            (superchat-claims--required-string supersedes "Superseded Claim"))
           (event (superchat-claims--claim-event supersedes events)))
      (unless (equal scope
                     (plist-get (plist-get event :payload) :scope))
        (error "Superseded Claim belongs to another scope: %s" supersedes))
      supersedes)))

(defun superchat-claim-record
    (statement scope basis &optional session-id actor-id supersedes claim-kind)
  "Record a Claim and return its Tape identity.
STATEMENT is contestable within SCOPE and BASIS is a list of existing legacy
`tape:N' or typed Tape references.  Empty BASIS is valid but unsupported.
SESSION-ID defaults to the current Superchat session, ACTOR-ID defaults to
`user', and SUPERSEDES optionally names an earlier Claim event ID in the same
Session and scope.  CLAIM-KIND defaults to `generic'."
  (let* ((statement
          (superchat-claims--required-string statement "Claim statement"))
         (scope (superchat-claims--required-string scope "Claim scope"))
         (session-id
          (superchat-claims--required-string
           (or session-id superchat--session-id) "Claim session"))
         (actor-id
          (superchat-claims--required-string (or actor-id "user") "Claim actor"))
         ;; ponytail: supersession scans one Session; add an indexed logical-ID
         ;; lookup only after measured capture latency.
         (events (when supersedes
                   (superchat-tape-replay-events session-id)))
         (basis (superchat-claims--normalize-basis basis))
         (supersedes (superchat-claims--normalize-supersedes
                      supersedes scope events))
         (claim-kind
          (superchat-claims--normalize-kind
           (or claim-kind
               (when supersedes
                 (plist-get
                  (plist-get
                   (superchat-claims--claim-event supersedes events)
                   :payload)
                  :kind))
               "generic")))
         (payload (list :statement statement :scope scope :kind claim-kind
                        :basis (vconcat basis))))
    (when supersedes
      (let ((prior-kind
             (or (plist-get
                  (plist-get
                   (superchat-claims--claim-event supersedes events) :payload)
                  :kind)
                 "generic")))
        (unless (equal claim-kind prior-kind)
          (error "Replacement Claim kind differs from its predecessor"))))
    (when supersedes
      (setq payload (plist-put payload :supersedes supersedes)))
    (superchat-tape-append-event-record
     (list :type "claim.recorded"
           :schema 1
           :session-id session-id
           :actor-id actor-id
           :topic "experience/claim"
           :summary "claim.recorded"
           :sensitivity "confidential"
           :requires-payload t
           :payload payload))))

(defun superchat-feedback-record
    (target kind basis note &optional session-id actor-id replacement)
  "Record durable Feedback about TARGET and return its Tape identity.
KIND is `support', `challenge', or `correct'.  BASIS may cite existing legacy
or typed Tape references.  TARGET and optional REPLACEMENT must be Claims in
SESSION-ID; REPLACEMENT must explicitly supersede TARGET."
  (let* ((target (superchat-claims--required-string target "Feedback target"))
         (kind (if (symbolp kind) (symbol-name kind)
                 (superchat-claims--required-string kind "Feedback kind")))
         (note (superchat-claims--required-string note "Feedback note"))
         (session-id
          (superchat-claims--required-string
           (or session-id superchat--session-id) "Feedback session"))
         (actor-id
          (superchat-claims--required-string
           (or actor-id "user") "Feedback actor"))
         (events (superchat-tape-replay-events session-id))
         (_target-event (superchat-claims--claim-event target events))
         (basis (superchat-claims--normalize-basis basis))
         (replacement
          (when replacement
            (superchat-claims--required-string replacement
                                               "Feedback replacement"))))
    (unless (member kind '("support" "challenge" "correct"))
      (error "Unknown Feedback kind: %s" kind))
    (when replacement
      (let ((replacement-event
             (superchat-claims--claim-event replacement events)))
        (unless (equal target
                       (plist-get (plist-get replacement-event :payload)
                                  :supersedes))
          (error "Feedback replacement does not supersede target: %s"
                 replacement))))
    (let ((payload (list :target target :kind kind
                         :basis (vconcat basis) :note note)))
      (when replacement
        (setq payload (plist-put payload :replacement replacement)))
      (superchat-tape-append-event-record
       (list :type "feedback.recorded"
             :schema 1
             :session-id session-id
             :actor-id actor-id
             :topic "experience/feedback"
             :summary "feedback.recorded"
             :sensitivity "confidential"
             :requires-payload t
             :payload payload)))))

(defun superchat-claims--normalize-event (event)
  "Return normalized Claim or Feedback data for Tape EVENT, or nil."
  (let ((payload (plist-get event :payload))
        (event-id (plist-get event :event-id)))
    (pcase (plist-get event :type)
      ("claim.recorded"
       (list :type "claim.recorded"
             :claim-id event-id
             :event-id event-id
             :session-id (plist-get event :session-id)
             :actor-id (plist-get event :actor-id)
             :recorded-at (plist-get event :recorded-at)
             :statement (plist-get payload :statement)
             :scope (plist-get payload :scope)
             :kind (or (plist-get payload :kind) "generic")
             :basis (or (plist-get payload :basis) '())
             :supersedes (plist-get payload :supersedes)))
      ("feedback.recorded"
       (list :type "feedback.recorded"
             :feedback-id event-id
             :event-id event-id
             :session-id (plist-get event :session-id)
             :actor-id (plist-get event :actor-id)
             :recorded-at (plist-get event :recorded-at)
             :target (plist-get payload :target)
             :kind (plist-get payload :kind)
             :basis (or (plist-get payload :basis) '())
             :note (plist-get payload :note)
             :replacement (plist-get payload :replacement))))))

(defun superchat-claims-replay (session-id &optional scope)
  "Replay Claims recorded for SESSION-ID, optionally restricted to SCOPE."
  (let ((session-id
         (superchat-claims--required-string session-id "Claim session"))
        (scope (when scope
                 (superchat-claims--required-string scope "Claim scope"))))
    (delq
     nil
     (mapcar
      (lambda (event)
        (let ((claim (superchat-claims--normalize-event event)))
          (when (and claim
                     (equal "claim.recorded" (plist-get claim :type))
                     (or (null scope)
                         (equal scope (plist-get claim :scope))))
            claim)))
      (superchat-tape-replay-events session-id)))))

(defun superchat-claims-current-view (session-id scope)
  "Replay the Claim Current View for SESSION-ID and SCOPE."
  (let ((session-id
         (superchat-claims--required-string session-id "Claim session"))
        (scope (superchat-claims--required-string scope "Claim scope"))
        claims feedback current)
    (dolist (event (superchat-tape-replay-events session-id))
      (let ((record (superchat-claims--normalize-event event)))
        (pcase (plist-get record :type)
          ("claim.recorded"
           (when (equal scope (plist-get record :scope))
             (push record claims)
             (when (or (null current)
                       (equal current (plist-get record :supersedes)))
               (setq current (plist-get record :claim-id)))))
          ("feedback.recorded"
           (when (cl-find (plist-get record :target) claims
                          :key (lambda (claim)
                                 (plist-get claim :claim-id))
                          :test #'equal)
             (push record feedback))))))
    (list :claims (nreverse claims)
          :feedback (nreverse feedback)
          :current current)))

;;;###autoload
(defun superchat-claim-capture (statement scope basis)
  "Explicitly capture a Claim from STATEMENT, SCOPE, and BASIS.
Interactively, STATEMENT comes from the active region or the minibuffer.
BASIS accepts comma- or whitespace-separated `tape:N' references."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Claim: "))
    (read-string "Claim scope: " superchat--session-id)
    (split-string (read-string "Fact references (tape:N, optional): ")
                  "[[:space:],]+" t)))
  (let ((record (superchat-claim-record statement scope basis)))
    (message "Claim recorded: %s" (plist-get record :event-id))
    record))

;;;###autoload
(defun superchat-claim-correct
    (target statement scope basis note &optional session-id actor-id)
  "Correct TARGET with a replacement STATEMENT and durable Feedback.
The replacement Claim and Feedback commit together.  Interactively,
STATEMENT comes from the active region or the minibuffer."
  (interactive
   (list
    (read-string "Claim ID to correct: ")
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Replacement Claim: "))
    (read-string "Claim scope: " superchat--session-id)
    (split-string (read-string "Fact references (tape:N, optional): ")
                  "[[:space:],]+" t)
    (read-string "Correction note: ")))
  (let ((session-id (or session-id superchat--session-id)))
    (superchat-db-call-with-transaction
     (lambda ()
       (let* ((replacement
               (superchat-claim-record statement scope basis session-id
                                       actor-id target))
              (feedback
               (superchat-feedback-record
                target 'correct basis note session-id actor-id
                (plist-get replacement :event-id))))
         (list :replacement replacement :feedback feedback))))))

(provide 'superchat-claims)

;;; superchat-claims.el ends here
