;;; superchat-approval.el --- One-shot approval seam -*- lexical-binding: t; -*-

;;; Commentary:
;; Answers one question: may this specific action proceed once?  The shape is
;; DeepSeek Harness's user-approval seam: a per-Session policy (`ask' or
;; `never'), an `approval/request' waterfall of answerers, four closed
;; outcomes of which only `allowed-once' grants, and a paired audit on the
;; Tape.  See docs/decisions/implemented/2026-09-17-permissions-the-dsh-way.md.
;;
;; The tool pipeline (`superchat-tool-execute') asks here when the Session's
;; permission level answers `ask'; the explicit-target write tools ask here
;; for a one-shot escalation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-event)
(require 'superchat-tape)

(declare-function superchat-view-session-configuration "superchat-tape-view" (session-id))
(declare-function superchat-runtime-effect-capable-p "superchat-runtime" (run-id))
(declare-function superchat-runtime--note-open-work "superchat-runtime"
                  (run-id execution-id action))
(declare-function superchat-authority-transport-timer-owner "superchat-authority"
                  (&optional context))
(declare-function superchat--extend-timeout "superchat" (&optional transport-timer-owner))

(defconst superchat-approval-policies '(ask never)
  "The closed per-Session approval policies.
`ask' delegates to the composed answerers.  `never' rejects every request
before any answerer sees it: the unattended stance.")

(defconst superchat-approval-outcomes
  '(allowed-once rejected cancelled unavailable)
  "The closed approval outcomes.  Only `allowed-once' grants.")

(defcustom superchat-approval-default-policy 'ask
  "Approval policy of a Session that has not selected one."
  :type '(choice (const ask) (const never))
  :group 'superchat)

(defcustom superchat-approval-function #'superchat-approval-confirm
  "Human answerer the approval chain ends in, called with one request plist.
It returns non-nil to allow exactly that request once.  The request carries
`:tool-name', `:args', `:reason', `:call-id', `:session-id' and `:run-id'; an
answerer that wants a preview renders it from those.  The default asks in the
minibuffer and is not available in a batch Emacs, where nobody can answer.
When nil, nobody can answer and every request is `unavailable'."
  :type '(choice (const nil) function)
  :group 'superchat)

(define-error 'superchat-approval-outside-run
	      "Approval was requested outside a live Run")

;;;; Per-Session policy

(defun superchat-approval--policy-symbol (value)
  "Return VALUE as a member of `superchat-approval-policies', or nil."
  (let ((symbol (cond ((symbolp value) value)
                      ((stringp value) (intern value)))))
    (car (memq symbol superchat-approval-policies))))

(defun superchat-approval-override (session-id)
  "Return SESSION-ID's recorded approval policy, or nil without one."
  (and (stringp session-id) (not (string-empty-p session-id))
       (fboundp 'superchat-view-session-configuration)
       (superchat-approval--policy-symbol
        (plist-get (superchat-view-session-configuration session-id)
                   :approval-policy))))

(defun superchat-approval-effective-policy (session-id)
  "Return the approval policy in force for SESSION-ID."
  (or (superchat-approval-override session-id)
      (superchat-approval--policy-symbol superchat-approval-default-policy)
      'ask))

(defun superchat-approval-set-policy (session-id policy &optional pin)
  "Record POLICY for SESSION-ID; return non-nil when an event was appended.
The single write path.  Selecting the effective policy again appends nothing.
With PIN non-nil the policy is recorded unless the Session already recorded
it, even when it equals the configured default."
  (let ((policy (or (superchat-approval--policy-symbol policy)
                    (error "Approval policy must be one of `ask' or `never': %S"
                           policy))))
    (unless (eq policy (if pin
                           (superchat-approval-override session-id)
                         (superchat-approval-effective-policy session-id)))
      (superchat-tape-append-event
       (list :type "approval.policy" :session-id session-id :actor-id "user"
             :summary "approval.policy"
             :payload (list :policy (symbol-name policy))))
      t)))

;;;; The human answerer

(defun superchat-approval--details (request)
  "Return REQUEST's tool, reason and whole arguments as help text."
  (concat (format "Tool: %s\n" (plist-get request :tool-name))
          (when (plist-get request :reason)
            (format "Reason: %s\n" (plist-get request :reason)))
          "\nArguments:\n"
          (pp-to-string (plist-get request :args))
          "\ny  allow this one call, with exactly these arguments\n"
          "n  do not allow it\n"))

(defun superchat-approval-confirm (request)
  "Ask the user to allow REQUEST once.  Return non-nil to allow.
The choices are allow once and deny; there is no standing grant.  `?' (or
the help character) shows the tool, the reason and the whole arguments and
leaves the question open: the details are `read-multiple-choice''s own
HELP-STRING, because that function owns the `?' key in both of its input
paths and a choice of ours on that key would never be reached."
  (let ((use-dialog-box nil)
        (reason (plist-get request :reason)))
    (eq ?y
        (car (read-multiple-choice
              (format "Allow `%s' once?%s "
                      (plist-get request :tool-name)
                      (if (and (stringp reason) (not (string-empty-p reason)))
                          (format " (%s)" reason)
                        ""))
              '((?y "yes, allow once") (?n "no"))
              (superchat-approval--details request))))))

(defun superchat-approval--fallback-answerer ()
  "Return the human answerer the chain ends in, or nil when nobody can answer.
The default minibuffer answerer counts as nobody in a batch Emacs; a function
a script or a plugin put in its place still answers there."
  (cond ((and (eq superchat-approval-function #'superchat-approval-confirm)
              noninteractive)
         nil)
        ((functionp superchat-approval-function) superchat-approval-function)))

(defun superchat-approval--extend-wait (run-id)
  "Keep RUN-ID's transport wait alive while a human decides."
  (when (and (fboundp 'superchat--extend-timeout)
             (fboundp 'superchat-authority-transport-timer-owner))
    (ignore-errors
      (superchat--extend-timeout
       (superchat-authority-transport-timer-owner (list :run-id run-id))))))

;;;; Request

(defun superchat-approval-snapshot-value (value)
  "Return a copy of JSON-like VALUE sharing no mutable structure with it.
Strings, vectors, hash tables and conses are copied at every depth, so a
holder of the copy cannot change what the holder of VALUE sees."
  (cond
   ((stringp value) (copy-sequence value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key item)
                 (puthash (superchat-approval-snapshot-value key)
                          (superchat-approval-snapshot-value item)
                          copy))
               value)
      copy))
   ((vectorp value)
    (apply #'vector (mapcar #'superchat-approval-snapshot-value (append value nil))))
   ((consp value)
    (cons (superchat-approval-snapshot-value (car value))
          (superchat-approval-snapshot-value (cdr value))))
   (t value)))

(defun superchat-approval--append (type request payload)
  "Append approval event TYPE for REQUEST with PAYLOAD."
  (let ((event (list :type type
                     :session-id (plist-get request :session-id)
                     :run-id (plist-get request :run-id)
                     :turn-id (plist-get request :turn-id)
                     :actor-id (or (plist-get request :actor-id) "lead")
                     :summary (format "%s: %s" type (plist-get request :tool-name))
                     :payload (append (list :tool-name (plist-get request :tool-name)
                                            :execution-id (plist-get request :execution-id))
                                      payload))))
    (superchat-tape-append-event event)))

(defun superchat-approval--note (request action)
  "Account ACTION for REQUEST's execution with Runtime's open-work marks."
  (when (fboundp 'superchat-runtime--note-open-work)
    (superchat-runtime--note-open-work
     (plist-get request :run-id) (plist-get request :execution-id) action)))

(defun superchat-approval--decide (request scope)
  "Return the outcome for REQUEST, asking answerers in SCOPE under `ask'.
Every answerer entry ends the same way.  An answerer that signals an error,
whether a waterfall listener or the answerer of last resort, is a closed
`unavailable'.  A `quit' from either entry propagates to the caller as a
quit.  Whenever somebody was asked, the transport wait is renewed."
  (if (eq 'never (superchat-approval-effective-policy
                  (plist-get request :session-id)))
      'rejected
    ;; Answerers get their own copy: nothing they do to it reaches the
    ;; snapshot the caller executes from.
    (let ((public (list :tool-name (plist-get request :tool-name)
                        :args (superchat-approval-snapshot-value
                               (plist-get request :args))
                        :reason (plist-get request :reason)
                        :call-id (plist-get request :call-id)
                        :session-id (plist-get request :session-id)
                        :run-id (plist-get request :run-id))))
      (unwind-protect
          (condition-case nil
              (let ((claimed
                     ;; The chain ends in the human answerer, so a listener
                     ;; that delegates gets the person's decision back from
                     ;; its continuation and can act on it -- record a grant,
                     ;; say.  A listener that neither answers nor delegates
                     ;; leaves the request unanswered.
                     (let ((superchat-event-waterfall-propagate-quit t)
                           (superchat-event-waterfall-terminal
                            (lambda (asked)
                              (let ((answerer (superchat-approval--fallback-answerer)))
                                (cond ((not answerer) 'unavailable)
                                      ((funcall answerer asked) 'allowed-once)
                                      (t 'rejected))))))
                       (superchat-emit scope 'approval/request public))))
                (if (memq claimed superchat-approval-outcomes)
                    claimed
                  ;; A refusal, a fault or a rogue value opens nothing.
                  'unavailable))
            (error 'unavailable))
        (superchat-approval--extend-wait (plist-get request :run-id))))))

(defun superchat-approval-request (request &optional scope)
  "Ask whether REQUEST may proceed once; return one approval outcome.
See `superchat-approval-request-snapshot', which also returns the approved
arguments a caller must execute from."
  (plist-get (superchat-approval-request-snapshot request scope) :outcome))

(defun superchat-approval-request-snapshot (request &optional scope)
  "Ask whether REQUEST may proceed once; return (:outcome OUTCOME :args ARGS).
ARGS is a snapshot of REQUEST's `:args' taken on entry, before any answerer
or other reentrant code runs.  An approval is for exactly that snapshot: a
caller that executes after `allowed-once' executes ARGS, never the list it
passed in, so nothing that changed the original while the question was open
is run under this approval.  Answerers receive a separate copy.

REQUEST is a plist with string `:session-id', `:run-id', `:turn-id',
`:execution-id' and `:tool-name', plus `:args', `:reason' and `:call-id'.
The request must belong to a live Run, because its audit pair
`approval.requested' / `approval.decided' is Run-scoped; outside one this
signals `superchat-approval-outside-run' before appending anything.

The `never' policy resolves `rejected' before any answerer runs.  Under `ask'
the `approval/request' waterfall in SCOPE may claim the request by returning
an outcome, or delegate by calling its continuation; an unclaimed request
goes to the answerer of last resort, and with none it is `unavailable'.  A
quit while deciding records a denial marked as a user quit, resolves the
open-approval mark, and is re-signalled.  Callers fail closed unless the
outcome is `allowed-once'."
  (dolist (key '(:session-id :run-id :turn-id :execution-id :tool-name))
    (unless (and (stringp (plist-get request key))
                 (not (string-empty-p (plist-get request key))))
      (error "Approval request requires string %s" key)))
  (unless (and (fboundp 'superchat-runtime-effect-capable-p)
               (superchat-runtime-effect-capable-p (plist-get request :run-id)))
    (signal 'superchat-approval-outside-run (list (plist-get request :run-id))))
  (let* ((approved-args (superchat-approval-snapshot-value
                         (plist-get request :args)))
         (request (plist-put (copy-sequence request) :args approved-args)))
    (superchat-approval--note request 'approval-entered)
    (superchat-approval--append
     "approval.requested" request
     (when (plist-get request :reason) (list :reason (plist-get request :reason))))
    (let ((outcome
           (condition-case condition
               (superchat-approval--decide request scope)
             (quit
              (superchat-approval--append
               "approval.decided" request
               (list :decision "deny" :outcome "cancelled"
                     :reason "quit" :interruption "user-quit"))
              (superchat-approval--note request 'approval-settled)
              (signal (car condition) (cdr condition))))))
      (superchat-approval--append
       "approval.decided" request
       (list :decision (if (eq outcome 'allowed-once) "allow" "deny")
             :outcome (symbol-name outcome)))
      ;; The mark is cleared only after the decision is durable: a failed
      ;; append leaves it open, and the Run then seals `audit-incomplete'.
      (superchat-approval--note request 'approval-settled)
      (list :outcome outcome :args approved-args))))

(provide 'superchat-approval)
;;; superchat-approval.el ends here
