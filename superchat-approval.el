;;; superchat-approval.el --- One-shot approval seam -*- lexical-binding: t; -*-

;;; Commentary:
;; Answers one question: may this specific action proceed once?  The shape is
;; DeepSeek Harness's user-approval seam: a per-Session policy (`ask' or
;; `never'), an `approval/request' waterfall of answerers, four closed
;; outcomes of which only `allowed-once' grants, and a paired audit on the
;; Tape.  See docs/decisions/implemented/2026-09-17-permissions-the-dsh-way.md.
;;
;; The tool pipeline (`superchat-tool-execute') asks here when the Session's
;; permission level answers `ask'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-event)
(require 'superchat-tape)

(declare-function superchat-view-session-configuration "superchat-tape-view" (session-id))
(declare-function superchat-runtime-effect-capable-p "superchat-runtime" (run-id))
(declare-function superchat-runtime--note-open-work "superchat-runtime"
                  (run-id execution-id action))
(declare-function superchat-tool-execution-transport-timer-owner "superchat-tool-execution"
                  (&optional context))
(declare-function superchat--extend-timeout "superchat" (&optional transport-timer-owner))
(declare-function superchat-llm--transport-timer-owner-suspend-wait "superchat-llm" (owner))
(declare-function superchat-llm--transport-timer-owner-resume-wait "superchat-llm" (owner token))

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

(defcustom superchat-approval-function #'superchat-approval-ask
  "Human answerer the approval chain ends in, called with one request plist.
It returns non-nil to allow exactly that request once.  The request carries
`:tool-name', `:args', `:reason', `:call-id', `:session-id' and `:run-id'; an
answerer that wants a preview renders it from those.  The default publishes
the question and lets Emacs go on (`superchat-approval-prompt'); it is not
available in a batch Emacs, where nobody can answer.
When nil, nobody can answer and every request is `unavailable'."
  :type '(choice (const nil) function)
  :group 'superchat)

;; decision of 2026-10-11, "an approval is published, then answered".
(defcustom superchat-approval-prompt 'publish
  "How `superchat-approval-ask' puts its question to the user.
`publish' shows the question in its conversation and in `superchat-pending',
lets Emacs go on, and waits for `superchat-approval-allow',
`superchat-approval-deny' or an answer from the list.  `minibuffer' asks in
the minibuffer and holds Emacs until it is answered."
  :type '(choice (const :tag "Publish it and go on" publish)
                 (const :tag "Ask in the minibuffer" minibuffer))
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
paths and a choice of ours on that key would never be reached.

This is the minibuffer answerer: Emacs is held until the question is answered.
The default answerer is `superchat-approval-ask', which publishes the question
instead and comes here only when `superchat-approval-prompt' says so.

Only input that arrives after the question is asked can answer it: pending
input is discarded first, and inside a running keyboard macro nothing is read
and the answer is no.  This keeps old input from approving; it does not prove
that anybody read the arguments (decision of 2026-09-19, \"an approval is
answered by input that came after it\")."
  (let ((use-dialog-box nil)
        (reason (plist-get request :reason)))
    (unless executing-kbd-macro
      ;; The answer must be input that came after this question.  Whatever was
      ;; typed before it -- a second `y' meant for the previous question, text
      ;; being typed elsewhere -- is dropped, the way Emacs itself does before
      ;; its own dangerous questions (userlock.el).  C-g is not queued input
      ;; and is untouched.  This also ends a keyboard macro being DEFINED: an
      ;; approval is not something to record into one.
      (discard-input))
    (and
     ;; A running keyboard macro is not a person: its keys are not read.
     (not executing-kbd-macro)
     (eq ?y
        (car
         ;; C-g must quit the question wherever it is asked.  Tool calls, and
         ;; so this question, usually run from a timer or a process filter,
         ;; where `inhibit-quit' is t.  Emacs 31 and later read the answer in
         ;; the minibuffer, which quits anyway; Emacs 30 reads it with
         ;; `read-event', which under `inhibit-quit' returns C-g as a
         ;; character that `read-multiple-choice' rejects as an invalid choice
         ;; and asks again, so the question could only be answered with `n'.
         (let ((inhibit-quit nil))
           (read-multiple-choice
            (format "Allow `%s' once?%s "
                    (plist-get request :tool-name)
                    (if (and (stringp reason) (not (string-empty-p reason)))
                        (format " (%s)" reason)
                      ""))
            '((?y "yes, allow once") (?n "no"))
            (superchat-approval--details request))))))))

;;;; A published question -----------------------------------------------------
;; decision of 2026-10-11, "an approval is published, then answered".  The
;; tool pipeline calls the approval seam and waits for its value, so the call
;; stack still waits here; what changes is that Emacs runs underneath it.

(defvar superchat-approval--open nil
  "The published questions nobody has answered yet, oldest first.
Each is a plist (:handle :request :asked-at :decision :depth :overlay :panel),
changed
in place.  In memory only: a question does not outlive its Run, and a Run does
not outlive its Emacs.")

(defun superchat-approval--record (handle)
  "The open question HANDLE names, or nil."
  (cl-find handle superchat-approval--open
           :key (lambda (record) (plist-get record :handle))))

(defun superchat-approval-open-questions ()
  "The published approval questions still open, oldest first.
Each is a plist (:handle :tool-name :reason :args :session-id :run-id
:asked-at); `:handle' is what `superchat-approval-answer' takes."
  (mapcar (lambda (record)
            (let ((request (plist-get record :request)))
              (list :handle (plist-get record :handle)
                    :tool-name (plist-get request :tool-name)
                    :reason (plist-get request :reason)
                    :args (plist-get request :args)
                    :session-id (plist-get request :session-id)
                    :run-id (plist-get request :run-id)
                    :asked-at (plist-get record :asked-at))))
          (cl-remove-if (lambda (record) (plist-get record :decision))
                        superchat-approval--open)))

(defun superchat-approval-answer (handle allow)
  "Answer the open question HANDLE: allow it once when ALLOW, else refuse it.
Return non-nil when this call decided it.  A question already answered, given
up with its Run, or never asked is not decided and nil is returned."
  (when-let* ((record (superchat-approval--record handle)))
    (unless (plist-get record :decision)
      (plist-put record :decision (if allow 'allow 'deny))
      t)))

(declare-function superchat-permission-set "superchat-sandbox" (session-id name))

(defconst superchat-approval-all-level 'auto
  "The permission level the answer \"allow all\" selects for its conversation.")

(defun superchat-approval-answer-all (handle)
  "Allow the open question HANDLE once and stop asking in its conversation.
The conversation is switched to the permission level `auto' first: when that
cannot be recorded the error is signaled and the question stays open.  Return
non-nil when this call decided the question; one already answered, given up
with its Run, or never asked is not decided, selects nothing, and nil is
returned.  Only that conversation is switched; /permissions edit-buffers
brings the questions back."
  (when-let* ((record (superchat-approval--record handle)))
    (unless (plist-get record :decision)
      (superchat-permission-set
       (plist-get (plist-get record :request) :session-id)
       superchat-approval-all-level)
      (plist-put record :all t)
      (superchat-approval-answer handle t))))

(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role))
(declare-function superchat--approval-panel-show "superchat" (handle request))
(declare-function superchat--approval-panel-settle "superchat" (panel label))

(defun superchat-approval--view (session)
  "The live conversation buffer of SESSION, or nil."
  (when (stringp session)
    (let ((owned (and (fboundp 'superchat-editor-workspace-buffer)
                      (ignore-errors
                        (superchat-editor-workspace-buffer session 'session-view)))))
      (if (buffer-live-p owned)
          owned
        (cl-find-if (lambda (buffer)
                      (and (buffer-live-p buffer)
                           (local-variable-p 'superchat--session-id buffer)
                           (equal session
                                  (buffer-local-value 'superchat--session-id buffer))))
                    (buffer-list))))))

(defun superchat-approval--line (request)
  "What the conversation shows of the open question REQUEST."
  (let ((reason (plist-get request :reason)))
    (concat
     "\n"
     (propertize
      (format "Allow `%s' once?%s\n  M-x superchat-approval-allow, superchat-approval-allow-all, superchat-approval-deny, superchat-approval-details; or answer it in M-x superchat-pending\n"
              (plist-get request :tool-name)
              (if (and (stringp reason) (not (string-empty-p reason)))
                  (format " (%s)" reason)
                ""))
      'face 'warning))))

(defun superchat-approval--show (record)
  "Show RECORD's question in its conversation and in the echo area."
  (let* ((request (plist-get record :request))
         (view (superchat-approval--view (plist-get request :session-id))))
    (when view
      ;; A panel of buttons where the conversation can hold one; the plain
      ;; line otherwise, and whenever drawing the panel fails.
      (let ((panel (and (fboundp 'superchat--approval-panel-show)
                        (condition-case nil
                            (superchat--approval-panel-show
                             (plist-get record :handle) request)
                          (error nil)))))
        (if panel
            (plist-put record :panel panel)
          (with-current-buffer view
            (let ((overlay (make-overlay (point-max) (point-max) view t t)))
              (overlay-put overlay 'superchat-approval (plist-get record :handle))
              (overlay-put overlay 'after-string (superchat-approval--line request))
              (plist-put record :overlay overlay))))))
    (superchat-approval--announce
     (format "`%s' asks to be allowed once; answer with M-x superchat-approval-allow or superchat-approval-deny, or in M-x superchat-pending"
             (plist-get request :tool-name)))))

(defun superchat-approval--unshow (record)
  "Take RECORD's question out of its conversation.
A panel stays in the transcript and says how the question ended."
  (when-let* ((panel (plist-get record :panel)))
    (when (fboundp 'superchat--approval-panel-settle)
      (ignore-errors
        (superchat--approval-panel-settle
         panel (pcase (plist-get record :decision)
                 ((and 'allow (guard (plist-get record :all)))
                  "Allowed; this conversation no longer asks")
                 ('allow "Allowed once")
                 ('deny "Denied")
                 (_ "No longer open; nothing was allowed"))))))
  (when-let* ((overlay (plist-get record :overlay)))
    (when (overlayp overlay) (delete-overlay overlay))))

(defun superchat-approval--wait (record)
  "Return once RECORD is decided, with Emacs running meanwhile.
At a keyboard that is a recursive edit: the user goes on editing, and the
edit is left when the question is decided.  A level the user entered on top of
it is theirs and is not left for them; the decision then takes effect when
they leave it.  Without a keyboard, timers and processes run."
  ;; A tool call, and so this wait, usually runs from a timer or a process
  ;; filter, where `inhibit-quit' is t: C-g must work in the Emacs that goes on.
  (let ((inhibit-quit nil))
    (if noninteractive
        (while (null (plist-get record :decision))
          (accept-process-output nil 0.05))
      (let* ((depth (1+ (recursion-depth)))
             (leave (run-with-timer
                     0.1 0.1
                     (lambda ()
                       (when (and (plist-get record :decision)
                                  (= depth (recursion-depth)))
                         (throw 'exit nil))))))
        (plist-put record :depth depth)
        (unwind-protect
            (while (null (plist-get record :decision))
              (recursive-edit))
          (cancel-timer leave))))))

(defun superchat-approval-publish (request)
  "Publish REQUEST as an open question and return non-nil once it is allowed.
An answerer for `superchat-approval-function'.  The question is listed by
`superchat-approval-open-questions' until `superchat-approval-answer' decides
it or the wait is abandoned -- by a quit, or because its Run ended."
  (let ((record (list :handle (make-symbol "approval") :request request
                      :asked-at (float-time) :decision nil :depth nil
                      :overlay nil :panel nil)))
    (setq superchat-approval--open (append superchat-approval--open (list record)))
    (unwind-protect
        (progn
          (superchat-approval--show record)
          (superchat-approval--wait record)
          (eq 'allow (plist-get record :decision)))
      (setq superchat-approval--open (delq record superchat-approval--open))
      (superchat-approval--unshow record))))

(defun superchat-approval-ask (request)
  "Ask the user to allow REQUEST once.  Return non-nil to allow.
The default answerer.  It publishes the question and lets Emacs go on
\(`superchat-approval-publish'), or asks in the minibuffer
\(`superchat-approval-confirm') when `superchat-approval-prompt' says so.  A
running keyboard macro is not a person and is refused either way."
  (cond (executing-kbd-macro nil)
        ((eq superchat-approval-prompt 'minibuffer)
         (superchat-approval-confirm request))
        (t (superchat-approval-publish request))))

(defun superchat-approval--question-here ()
  "The open question the user means: this conversation's, or the only one."
  (let* ((open (superchat-approval-open-questions))
         (session (and (boundp 'superchat--session-id)
                       (symbol-value 'superchat--session-id)))
         (here (and (stringp session)
                    (cl-find session open
                             :key (lambda (q) (plist-get q :session-id))
                             :test #'equal))))
    (cond (here here)
          ((null open) (user-error "No approval question is open"))
          ((null (cdr open)) (car open))
          (t (user-error "%d approval questions are open; answer them in M-x superchat-pending or from their conversations"
                         (length open))))))

(defun superchat-approval-allow ()
  "Allow, once, what this conversation's open approval question asks for."
  (interactive)
  (let ((question (superchat-approval--question-here)))
    (when (superchat-approval-answer (plist-get question :handle) t)
      (superchat-approval--announce
       (format "allowed `%s' once" (plist-get question :tool-name))))))

(defun superchat-approval-allow-all ()
  "Allow this conversation's open approval question and stop asking here.
The conversation is switched to the permission level `auto': full Emacs
control is no longer asked about, saving a buffer still is.  Other
conversations are not changed; /permissions edit-buffers brings the
questions back."
  (interactive)
  (let ((question (superchat-approval--question-here)))
    (when (superchat-approval-answer-all (plist-get question :handle))
      (superchat-approval--announce
       (format "allowed `%s'; this conversation is now at the level auto and no longer asks (/permissions edit-buffers to be asked again)"
               (plist-get question :tool-name))))))

(defun superchat-approval-deny ()
  "Refuse what this conversation's open approval question asks for."
  (interactive)
  (let ((question (superchat-approval--question-here)))
    (when (superchat-approval-answer (plist-get question :handle) nil)
      (superchat-approval--announce
       (format "did not allow `%s'" (plist-get question :tool-name))))))

(defun superchat-approval-details ()
  "Show the tool, the reason and the whole arguments of the open question."
  (interactive)
  (let ((question (superchat-approval--question-here)))
    (with-help-window "*superchat approval*"
      (princ (superchat-approval--details question)))))

;;;; A question does not outlive its Run ---------------------------------------
;; decision of 2026-09-19, "a question is dismissed when its Run ends".

(declare-function superchat-runtime-effect-capable-p "superchat-runtime" (run-id))
(declare-function superchat-runtime-status "superchat-runtime" (run-id))

(defvar superchat-approval--interruption nil
  "Why the question being decided was given up, or nil.
Bound around one decision; `run-ended' when its Run ended under it.")

(defun superchat-approval--announce (text)
  "Tell the user TEXT."
  (message "superchat: %s" text))

(defun superchat-approval--ask-while-run-lives (run-id tool-name ask)
  "Call ASK, the answerer of last resort, and return what it returns.
While it blocks, watch RUN-ID: once that Run can no longer act -- its deadline
passed, it was cancelled elsewhere, it failed -- the question can decide
nothing, so ASK is abandoned with a non-local exit (the way C-g abandons it),
the user is told, and `:run-ended' is returned.  A question without a live
Run to begin with is asked as it always was."
  (if (not (and (stringp run-id)
                (fboundp 'superchat-runtime-effect-capable-p)
                (superchat-runtime-effect-capable-p run-id)))
      (funcall ask)
    (let* ((tag (make-symbol "run-ended"))
           (watch nil)
           (result
            (catch tag
              (unwind-protect
                  (progn
                    (setq watch
                          (run-with-timer
                           0.5 0.5
                           (lambda ()
                             (unless (superchat-runtime-effect-capable-p run-id)
                               (throw tag tag)))))
                    (funcall ask))
                (when (timerp watch) (cancel-timer watch))))))
      (if (not (eq result tag))
          result
        (when-let* ((help (get-buffer "*Multiple Choice Help*")))
          (quit-windows-on help))
        (superchat-approval--announce
         (format "the Run ended (%s) while `%s' was waiting for your answer; the call was not run"
                 (or (ignore-errors
                       (plist-get (superchat-runtime-status run-id) :status))
                     "gone")
                 tool-name))
        :run-ended))))

(defun superchat-approval--fallback-answerer ()
  "Return the human answerer the chain ends in, or nil when nobody can answer.
The default answerer, and the minibuffer one, count as nobody in a batch
Emacs and while a keyboard macro is running; a function a script or a plugin
put in its place still answers there."
  (cond ((and (memq superchat-approval-function
                    '(superchat-approval-ask superchat-approval-confirm))
              ;; Nobody is at the keyboard in batch, and a running keyboard
              ;; macro is not somebody: its `y' is not a person's consent.
              (or noninteractive executing-kbd-macro))
         nil)
        ((functionp superchat-approval-function) superchat-approval-function)))

(defun superchat-approval--transport-owner (run-id)
  "Return RUN-ID's transport timer owner, or nil."
  (when (and run-id (fboundp 'superchat-tool-execution-transport-timer-owner))
    (ignore-errors
      (superchat-tool-execution-transport-timer-owner (list :run-id run-id)))))

(defun superchat-approval--suspend-wait (run-id)
  "Stop RUN-ID's inactivity clock while somebody decides; return a token.
A person reading the question is not a model making no progress."
  (when (fboundp 'superchat-llm--transport-timer-owner-suspend-wait)
    (ignore-errors
      (superchat-llm--transport-timer-owner-suspend-wait
       (superchat-approval--transport-owner run-id)))))

(defun superchat-approval--resume-wait (run-id token)
  "Let RUN-ID's inactivity clock continue from TOKEN, if its Run still waits."
  (when (and token (fboundp 'superchat-llm--transport-timer-owner-resume-wait))
    (ignore-errors
      (superchat-llm--transport-timer-owner-resume-wait
       (superchat-approval--transport-owner run-id) token))))

(defun superchat-approval--extend-wait (run-id)
  "Keep RUN-ID's transport wait alive while a human decides."
  (when (and (fboundp 'superchat--extend-timeout)
             (fboundp 'superchat-tool-execution-transport-timer-owner))
    (ignore-errors
      (superchat--extend-timeout
       (superchat-tool-execution-transport-timer-owner (list :run-id run-id))))))

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
quit.  While somebody is being asked the Run's inactivity clock does not run;
afterwards it continues from what remained and is renewed once."
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
                        :run-id (plist-get request :run-id)))
          (clock (superchat-approval--suspend-wait (plist-get request :run-id))))
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
                              (let* ((answerer (superchat-approval--fallback-answerer))
                                     (answer
                                      (and answerer
                                           (superchat-approval--ask-while-run-lives
                                            (plist-get asked :run-id)
                                            (plist-get asked :tool-name)
                                            (lambda () (funcall answerer asked))))))
                                (cond ((not answerer) 'unavailable)
                                      ((eq answer :run-ended)
                                       (setq superchat-approval--interruption 'run-ended)
                                       'cancelled)
                                      (answer 'allowed-once)
                                      (t 'rejected))))))
                       (superchat-emit scope 'approval/request public))))
                (if (memq claimed superchat-approval-outcomes)
                    claimed
                  ;; A refusal, a fault or a rogue value opens nothing.
                  'unavailable))
            (error 'unavailable))
        (superchat-approval--resume-wait (plist-get request :run-id) clock)
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
    (let* ((superchat-approval--interruption nil)
           (outcome
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
       (append
        (list :decision (if (eq outcome 'allowed-once) "allow" "deny")
              :outcome (symbol-name outcome))
        ;; Given up because its Run ended: not the user's C-g (`user-quit').
        (when (eq superchat-approval--interruption 'run-ended)
          (list :reason "run-ended" :interruption "run-ended"))))
      ;; The mark is cleared only after the decision is durable: a failed
      ;; append leaves it open, and the Run then seals `audit-incomplete'.
      (superchat-approval--note request 'approval-settled)
      (list :outcome outcome :args approved-args))))

(provide 'superchat-approval)
;;; superchat-approval.el ends here
