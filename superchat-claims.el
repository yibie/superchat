;;; superchat-claims.el --- Explicit Claim capture for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Claims are contestable statements backed by durable Tape references.  This
;; module records and replays them; it does not promote them into Memory or
;; derive a Current View.

;;; Code:

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

(defun superchat-claims--normalize-basis (basis)
  "Return validated Tape references from BASIS.
An empty BASIS is allowed and records an unsupported Claim."
  (unless (listp basis)
    (error "Claim basis must be a list of tape:N references"))
  (mapcar
   (lambda (reference)
     (let ((reference
            (superchat-claims--required-string reference "Claim basis reference")))
       (unless (string-match "\\`tape:\\([1-9][0-9]*\\)\\'" reference)
         (error "Claim basis reference must use tape:N: %s" reference))
       (unless
           (superchat-db-tape-select
            "SELECT 1 FROM tape WHERE id = ? LIMIT 1"
            (list (string-to-number (match-string 1 reference))))
         (error "Unknown Tape fact: %s" reference))
       reference))
   basis))

(defun superchat-claim-record
    (statement scope basis &optional session-id actor-id supersedes)
  "Record a Claim and return its Tape identity.
STATEMENT is contestable within SCOPE and BASIS is a list of existing
`tape:N' Fact references.  Empty BASIS is valid but unsupported.  SESSION-ID
defaults to the current Superchat session, ACTOR-ID defaults to `user', and
SUPERSEDES optionally names an earlier Claim event ID."
  (let* ((statement
          (superchat-claims--required-string statement "Claim statement"))
         (scope (superchat-claims--required-string scope "Claim scope"))
         (session-id
          (superchat-claims--required-string
           (or session-id superchat--session-id) "Claim session"))
         (actor-id
          (superchat-claims--required-string (or actor-id "user") "Claim actor"))
         (basis (superchat-claims--normalize-basis basis))
         (supersedes
          (when supersedes
            (superchat-claims--required-string supersedes "Superseded Claim")))
         (payload (list :statement statement :scope scope :basis basis)))
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

(defun superchat-claims--normalize-event (event)
  "Return normalized Claim data for Tape EVENT, or nil."
  (when (equal "claim.recorded" (plist-get event :type))
    (let ((payload (plist-get event :payload))
          (event-id (plist-get event :event-id)))
      (list :type "claim.recorded"
            :claim-id event-id
            :event-id event-id
            :session-id (plist-get event :session-id)
            :actor-id (plist-get event :actor-id)
            :recorded-at (plist-get event :recorded-at)
            :statement (plist-get payload :statement)
            :scope (plist-get payload :scope)
            :basis (or (plist-get payload :basis) '())
            :supersedes (plist-get payload :supersedes)))))

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
                     (or (null scope)
                         (equal scope (plist-get claim :scope))))
            claim)))
      (superchat-tape-replay-events session-id)))))

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

(provide 'superchat-claims)

;;; superchat-claims.el ends here
