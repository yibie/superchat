;;; superchat-plan.el --- Structured plans: propose, review, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A plan is a structured object, not prose.  The model proposes steps via
;; the `emit_plan' tool; the steps land on the Tape as `plan.proposed' and
;; render for review as stateful widget.el controls (a checkbox per step,
;; whose value is the step's checked state) in a TextUI side window — the
;; same window the Codex-style plan panel occupies.  The review never
;; touches the chat buffer: the widget holds the state, the plan data is
;; the source of truth, and the rendered frame is a pure projection of
;; it.  Approving reads the data (never rendered text), lands
;; `plan.approved' on the Tape, and the same side window switches to the
;; execution progress view: status markers the model drives through the
;; `report_plan_progress' tool (each report records `plan.progress' on
;; the Tape).
;;
;; TextUI is used here because its side window keeps plan state visible
;; next to the chat; widget.el controls inside it activate through
;; textui-mode's composed widget-keymap.
;;
;; The included steps become the session's active plan, injected into
;; every following execution Run until cleared.  Acceptance criteria are
;; commitments for the human and future Closure checks — they never
;; drive automatic retries.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'superchat-tape)

(declare-function superchat-send-input "superchat")
(declare-function textui-open "textui" (name render-function &optional initial-state))
(declare-function textui-update "textui" (buffer updater &rest keys))
(declare-function textui-request-refresh "textui" (buffer))

(defgroup superchat-plan nil
  "Structured plans: propose, review, approve."
  :group 'superchat)

(defcustom superchat-plan-auto-execute t
  "When non-nil, approving a plan with included steps immediately
sends a follow-up request telling the model to execute them."
  :type 'boolean
  :group 'superchat-plan)

(defvar-local superchat--active-plan nil
  "The approved plan for this session.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S
:included P :status S) ...))")

(defvar-local superchat-plan--proposed nil
  "The plan currently under review in this buffer.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S
:checked P) ...))")

(defvar superchat-plan--view-buffer "*superchat-plan-review*"
  "TextUI buffer that renders the plan review / progress side window.")

(defvar superchat-plan--view-chat-buffer nil
  "Name of the chat buffer whose plan the side window shows.
The view is a pure projection: the plan data lives in the chat buffer
and the widgets' state is synced back into it on every change.")

(defun superchat-plan--validate-steps (steps)
  "Return a rejection string for STEPS, or nil when they are valid.
Error messages carry a concrete example: the model that just failed
retries with the same shape unless the correction is pedagogical."
  (cond
   ((not (listp steps))
    "steps must be a list of (text accept) pairs, e.g. [[\"Edit line 2\" \"line 2 equals the target text\"]]")
   ((null steps)
    "steps must not be empty")
   ((> (length steps) 20)
    "at most 20 steps")
   (t
    (cl-loop for step in steps
             for id from 1
             do (unless (listp step)
                  (cl-return
                   (format
                    (concat "step %d is not a list. Each step must be a "
                            "(text accept) PAIR: [\"what to do\" "
                            "\"how to verify it\"], "
                            "e.g. [\"Edit line 2\" \"line 2 equals the target text\"]")
                    id)))
             do (unless (= 2 (length step))
                  (cl-return
                   (format
                    (concat "step %d has %d elements. Each step must be "
                            "a (text accept) PAIR of exactly two strings: "
                            "[\"what to do\" \"how to verify it\"], "
                            "e.g. [\"Edit line 2\" \"line 2 equals the target text\"]")
                    id (length step))))
             do (unless (and (stringp (car step))
                             (not (string-empty-p (string-trim (car step)))))
                  (cl-return (format "step %d text must be a non-empty string"
                                    id)))
             do (unless (and (stringp (cadr step))
                             (not (string-empty-p (string-trim (cadr step)))))
                  (cl-return (format "step %d accept must be a non-empty string"
                                    id)))
             do (unless (<= (length (car step)) 500)
                  (cl-return (format "step %d text too long" id)))
             finally return nil))))

(defun superchat-plan--proposed-event (session-id steps summary)
  "Append the `plan.proposed' event for SESSION-ID and return its id."
  (plist-get
   (superchat-tape-append-event-record
    (list :type "plan.proposed" :schema 1
          :session-id session-id :actor-id "lead"
          :payload
          (list :summary summary
                ;; A list of plists would be misread by json-encode as an
                ;; alist ({"id": [1, "text", ...]}); a vector encodes as
                ;; an array of objects and replays as a clean list of
                ;; plists.
                :steps
                (vconcat
                 (cl-loop for step in steps
                          for id from 1
                          collect (list :id id :text (car step)
                                        :accept (cadr step)))))))
   :event-id))

;; ── Side window: review controls and execution progress ──

(define-widget 'superchat-plan-step-checkbox 'checkbox
  "Plan review step: a checkbox whose value is the step's checked state.
The control itself carries the state; toggling it syncs the value into
the plan data and refreshes the view.  :step-id links the control to
its step in the proposed plan."
  :format "%[%v%] %t"
  :value nil
  ;; Explicit keymap: Emacs 31 widget.el installs none by default, and a
  ;; nil overlay keymap means silent no-op clicks (textui-mode composes
  ;; widget-keymap into its own mode map, but this keeps the widget safe
  ;; in any buffer).
  :keymap widget-keymap
  ;; All plan view controls route through the one interaction seam;
  ;; each control only declares its intent.
  :notify #'superchat-plan--view-action
  :plan-action :toggle)

(defun superchat-plan--view-action (widget &rest _)
  "Single interaction seam for every plan view control.
Controls declare their intent with :plan-action; all behavior lives
here instead of in each widget's own notify callback."
  (pcase (widget-get widget :plan-action)
    (:toggle
     (let* ((id (widget-get widget :step-id))
            (chat (and superchat-plan--view-chat-buffer
                       (get-buffer superchat-plan--view-chat-buffer))))
       (when (and id chat)
         (with-current-buffer chat
           (when superchat-plan--proposed
             (let ((step
                    (cl-find id (plist-get superchat-plan--proposed :steps)
                             :key (lambda (s) (plist-get s :id)))))
               ;; :checked pre-seeded at proposal: setf on an existing
               ;; key mutates in place; a missing key would silently
               ;; rebind the local instead.
               (setf (plist-get step :checked) (widget-value widget))))))
       (when (and (fboundp 'textui-request-refresh)
                  (derived-mode-p 'textui-mode))
         (textui-request-refresh (current-buffer)))))
    (:approve (superchat-plan--view-approve))))

(defun superchat-plan--accept-line (step)
  "Return STEP's acceptance criterion as a single trimmed line."
  (let ((accept (plist-get step :accept)))
    (format "     » %s"
            (if (> (string-width accept) 30)
                (truncate-string-to-width accept 30 nil nil "…")
              accept))))

(defun superchat-plan--progress-bar (done total)
  "Return a character progress bar for DONE of TOTAL steps."
  (let ((filled (if (zerop total)
                    0
                  (round (* 8 (/ (float done) total))))))
    (concat "[" (make-string filled ?█)
            (make-string (- 8 filled) ?░) "]")))

(defun superchat-plan--review-frame (proposed)
  "Return the review frame for the PROPOSED plan."
  (let* ((steps (plist-get proposed :steps))
         (checked (cl-count t steps
                            :key (lambda (step)
                                   (plist-get step :checked)))))
    ;; A bare element list renders as an inline row in TextUI; the
    ;; top-level frame is a one-element list whose column flex stacks
    ;; each child on its own line.  Layering follows the hybrid card
    ;; scheme: only identity (header) and the action point get boxes.
    (list
     (list :type :flex :direction :column :gap 1
           :children
           (append
            (list
             (list :type :flex :direction :column :border t :padding 1
                   :children
                   (list
                    (list :type :text
                          :value
                          (format "Plan: %s"
                                  (or (plist-get proposed :summary)
                                      "plan")))))
             (list :type :text :value (make-string 24 ?─)))
            (cl-loop for step in steps
                     append
                     (let ((id (plist-get step :id)))
                       (list
                        (list :type 'superchat-plan-step-checkbox
                              :tag (format "%d. %s" id
                                           (plist-get step :text))
                              :value (plist-get step :checked)
                              :step-id id
                              :layout
                              (list :focus-id
                                    (intern
                                     (format "plan-step-%d" id))))
                        (list :type :text
                              :value
                              (superchat-plan--accept-line step)))))
            (list
             (list :type :flex :direction :row :gap 2
                   :children
                   (list
                    (list :type :text
                          :value
                          (format "%d/%d selected" checked
                                  (length steps)))
                    (list :type 'push-button
                          :tag "Approve selected steps"
                          :keymap widget-keymap
                          :notify #'superchat-plan--view-action
                          :plan-action :approve)))))))))

(defun superchat-plan--progress-frame (plan)
  "Return the execution progress frame for the approved PLAN."
  (let* ((steps (cl-remove-if-not (lambda (s) (plist-get s :included))
                                  (plist-get plan :steps)))
         (total (length steps))
         (done (cl-count "done" steps :test #'equal
                         :key (lambda (s) (plist-get s :status)))))
    (list
     (list :type :flex :direction :column :gap 1
           :children
           (append
            (list
             (list :type :flex :direction :column :border t :padding 1
                   :children
                   (list
                    (list :type :text
                          :value
                          (format "Plan: %s — in progress"
                                  (or (plist-get plan :summary) "plan")))))
             (list :type :text :value (make-string 24 ?─)))
            (cl-loop for step in steps
                     append
                     (let* ((status (plist-get step :status))
                            (status-line
                             (list
                              :type :text
                              :value
                              (format "%s %d. %s"
                                      (pcase status
                                        ("done" "[✓]")
                                        ("in-progress" "[→]")
                                        (_ "[ ]"))
                                      (plist-get step :id)
                                      (plist-get step :text))))
                            (accept-line
                             (list :type :text
                                   :value
                                   (superchat-plan--accept-line step))))
                       (if (equal status "in-progress")
                           ;; spotlight: the only boxed step
                           (list
                            (list :type :flex :direction :column
                                  :border t :padding 1
                                  :children
                                  (list status-line accept-line)))
                         (list status-line accept-line))))
            (list
             (list :type :flex :direction :row :gap 2
                   :children
                   (list
                    (list :type :text
                          :value (format "%d/%d done" done total))
                    (list :type :text
                          :value
                          (superchat-plan--progress-bar done total))))))))))

(defun superchat-plan--view-frame (&optional _width)
  "Return the side window frame for the chat buffer's plan."
  (let* ((chat (and superchat-plan--view-chat-buffer
                    (get-buffer superchat-plan--view-chat-buffer)))
         (proposed (and chat
                        (buffer-local-value 'superchat-plan--proposed chat)))
         (active (and chat
                      (buffer-local-value 'superchat--active-plan chat))))
    (cond
     (proposed (superchat-plan--review-frame proposed))
     (active (superchat-plan--progress-frame active))
     (t (list
         ;; keep the card language for the empty state so the panel
         ;; outline does not vanish between plan transitions
         (list :type :flex :direction :column :border t :padding 1
               :children
               (list (list :type :text
                           :value "No plan in this session."))))))))

(defun superchat-plan--view-available-p ()
  "Return non-nil when TextUI can render the plan side window."
  (and (or (featurep 'textui) (require 'textui nil t))
       (fboundp 'textui-open)
       (fboundp 'textui-update)
       (fboundp 'textui-request-refresh)))

(defun superchat-plan--view-show ()
  "Show the plan view in a right side window.
TextUI's own display picks an arbitrary window; move the buffer into a
right side window if it is shown anywhere else.  Window layout is a GUI
concern: batch tests skip it (the sole window cannot be rearranged)."
  (when (not noninteractive)
    (let ((buffer (get-buffer superchat-plan--view-buffer)))
      (when buffer
        (dolist (win (get-buffer-window-list buffer nil t))
          (unless (eq (window-parameter win 'window-side) 'right)
            (delete-window win)))
        (display-buffer
         buffer
         '((display-buffer-in-side-window)
           (side . right)
           (window-width . 0.42)))))))

(defun superchat-plan--view-update ()
  "Open or update the plan side window."
  (let ((buffer (get-buffer superchat-plan--view-buffer)))
    (cond
     ((and buffer
           (with-current-buffer buffer (derived-mode-p 'textui-mode)))
      (textui-update buffer
                     (lambda (_old) (superchat-plan--view-frame)))
      (superchat-plan--view-show)
      buffer)
     (t
      (textui-open superchat-plan--view-buffer
                   (lambda (_width) (superchat-plan--view-frame)))
      (superchat-plan--view-show)))))

(defun superchat-plan--view-close ()
  "Close the plan side window and clear its frame when it exists."
  (let ((buffer (get-buffer superchat-plan--view-buffer)))
    (when buffer
      (when-let ((win (get-buffer-window buffer)))
        (quit-window nil win))
      (when (and (fboundp 'textui-request-refresh)
                 (with-current-buffer buffer
                   (derived-mode-p 'textui-mode)))
        (textui-request-refresh buffer)))))

(defun superchat-plan--view-approve (&rest _)
  "Approve the plan under review from the side window."
  (if (and superchat-plan--view-chat-buffer
           (get-buffer superchat-plan--view-chat-buffer))
      (with-current-buffer (get-buffer superchat-plan--view-chat-buffer)
        (superchat-plan-approve))
    (message "No plan review to approve.")))

;; ── Progress reporting (report_plan_progress tool) ──

(defun superchat-plan--validate-updates (updates)
  "Return a rejection string for progress UPDATES, or nil when valid."
  (cond
   ((not (listp updates))
    "updates must be a list of (step-id status) pairs, e.g. [[1 \"done\"]]")
   ((null updates)
    "updates must not be empty")
   (t
    (cl-loop for update in updates
             do (unless (listp update)
                  (cl-return
                   (format
                    "update %S is not a list. Each update must be a (step-id status) PAIR, e.g. [1 \"done\"]"
                    update)))
             do (unless (= 2 (length update))
                  (cl-return
                   (format
                    "update %S has %d elements. Each update must be a PAIR of exactly two elements: [step-id \"done\"]"
                    update (length update))))
             do (unless (integerp (car update))
                  (cl-return
                   (format "update %S: step id must be an integer" update)))
             do (unless (member (cadr update)
                                '("done" "in-progress" "pending"))
                  (cl-return
                   (format
                    "update %S: status must be one of \"done\", \"in-progress\", \"pending\""
                    update)))
             finally return nil))))

(defun superchat-plan-report-progress (updates)
  "Apply UPDATES (a list of (step-id status) pairs) to the active plan.
Records `plan.progress' on the Tape and refreshes the side window.
Returns a user-facing result string."
  (let ((rejection (superchat-plan--validate-updates updates)))
    (if rejection
        (format "Error: %s" rejection)
      (with-current-buffer (get-buffer-create superchat-buffer-name)
        (if (not superchat--active-plan)
            "Error: no active plan in this session"
          (let* ((active superchat--active-plan)
                 (bad
                  (cl-find-if
                   (lambda (update)
                     (not
                      (cl-find (car update) (plist-get active :steps)
                               :key (lambda (s) (plist-get s :id)))))
                   updates)))
            (if bad
                (format "Error: step %d is not part of the approved plan"
                        (car bad))
              (dolist (update updates)
                (let ((id (car update))
                      (status (cadr update))
                      (step
                       (cl-find (car update) (plist-get active :steps)
                                :key (lambda (s) (plist-get s :id)))))
                  ;; :status pre-seeded at approval; setf on an existing
                  ;; key mutates in place.
                  (setf (plist-get step :status) status)
                  (superchat-tape-append-event-record
                   (list :type "plan.progress" :schema 1
                         :session-id superchat--session-id
                         :actor-id "lead"
                         :payload
                         (list :proposed
                               (list :session-id superchat--session-id
                                     :event-id (plist-get active :event-id))
                               :step-id id :status status)))))
              (setq superchat-plan--view-chat-buffer
                    (buffer-name (current-buffer)))
              (when (superchat-plan--view-available-p)
                (superchat-plan--view-update))
              (format "Progress recorded: %s"
                      (mapconcat
                       (lambda (update)
                         (format "%d=%s" (car update) (cadr update)))
                       updates ", ")))))))))

(defun superchat-plan--restore-on-reopen (session-id)
  "Restore an unresolved plan review into the current session View.
Called after a session View is rebuilt from Tape: a plan that was
proposed but never approved or cleared re-opens its review side
window."
  (unless (or superchat-plan--proposed superchat--active-plan)
    (let ((pending nil))
      (dolist (event (superchat-tape-replay-events session-id))
        (pcase (plist-get event :type)
          ("plan.proposed"
           (setq pending (list (plist-get event :event-id)
                               (plist-get event :payload))))
          ((or "plan.approved" "plan.cleared")
           (when (and pending
                      (equal (plist-get
                              (plist-get (plist-get event :payload)
                                         :proposed)
                              :event-id)
                             (car pending)))
             (setq pending nil)))))
      (when pending
        (let ((payload (cadr pending)))
          (setq superchat-plan--proposed
                (list :summary (plist-get payload :summary)
                      :event-id (car pending)
                      :steps (mapcar
                              (lambda (step)
                                (append step (list :checked t)))
                              (plist-get payload :steps))))
          (setq superchat-plan--view-chat-buffer
                (buffer-name (current-buffer)))
          (when (superchat-plan--view-available-p)
            (superchat-plan--view-update)))))))

(defvar-local superchat-plan--nudge-pending nil
  "Non-nil while a dispatched execution has unreported pending steps.
Set when approval dispatches the execution follow-up; consumed by the
turn-completion nudge that pushes the model to report and finish.")

(defvar-local superchat-plan--nudges 0
  "Count of completion nudges sent for the current active plan.")

(defun superchat-plan--nudge-on-completion (status)
  "Push the model to finish and report after an execution turn.
Fires when the execution follow-up was dispatched and the active plan
still has pending steps; stops after two nudges or when every included
step reports done."
  (when (and (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status))
             superchat-plan--nudge-pending
             superchat--active-plan)
    (let* ((steps
            (cl-remove-if-not (lambda (s) (plist-get s :included))
                              (plist-get superchat--active-plan :steps)))
           (pending
            (cl-remove-if (lambda (s)
                            (equal (plist-get s :status) "done"))
                          steps)))
      (cond
       ((null pending)
        (setq superchat-plan--nudge-pending nil))
       ((< superchat-plan--nudges 2)
        (setq superchat-plan--nudges (1+ superchat-plan--nudges))
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert
           (format
            (concat "%d plan step%s still pending: %s. Keep executing, "
                    "and report each step via report_plan_progress "
                    "(status \"done\" when its criterion holds).")
            (length pending)
            (if (= (length pending) 1) "" "s")
            (mapconcat
             (lambda (s)
               (format "%d. %s" (plist-get s :id) (plist-get s :text)))
             pending "; "))))
        (superchat-send-input))
       (t
        ;; Cap reached: stop pushing, keep the panel honest.
        (setq superchat-plan--nudge-pending nil))))))

(add-hook 'superchat-turn-completed-functions
          #'superchat-plan--nudge-on-completion)

;; ── Propose / approve / clear ──

(defun superchat-plan-propose (steps &optional summary)
  "Propose STEPS (a list of (text accept) pairs) as a reviewable plan.
Appends `plan.proposed' to the current session's Tape, stores the plan
as review data, and renders the review side window when TextUI is
available.  Returns a user-facing result string."
  (let ((rejection (superchat-plan--validate-steps steps)))
    (if rejection
        (format "Error: %s" rejection)
      (if (not (and (boundp 'superchat--session-id) superchat--session-id))
          "Error: no active Superchat session"
        (if-let ((existing
                  (with-current-buffer
                      (get-buffer-create superchat-buffer-name)
                    (cond (superchat-plan--proposed 'under-review)
                          (superchat--active-plan 'approved)))))
            ;; One session, one plan: never open a second review.
            ;; "declined" classifies the message as non-retryable in the
            ;; tool error feedback, so the model stops proposing.
            (concat "Error: plan proposal declined — a plan is already "
                    (if (eq existing 'approved) "approved" "under review")
                    " in this session. Do not propose another plan.")
          (let* ((summary (string-trim (or summary "")))
                 (event-id
                  (superchat-plan--proposed-event
                   superchat--session-id steps summary))
                 (structured
                  (cl-loop for step in steps
                           for id from 1
                           collect (list :id id :text (car step)
                                         :accept (cadr step)
                                         ;; Default: keep all steps;
                                         ;; the user unchecks to drop.
                                         :checked t)))
                 (view-error nil))
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (setq superchat-plan--proposed
                    (list :summary summary :event-id event-id
                          :steps structured)))
            (setq superchat-plan--view-chat-buffer superchat-buffer-name)
            (when (superchat-plan--view-available-p)
              ;; The view is presentation only; a failure must not fail
              ;; the tool — the plan data and Tape event are the contract.
              (condition-case err
                  (superchat-plan--view-update)
                (error (setq view-error (error-message-string err)))))
            (cond
             (view-error
              (format "Plan proposed: %d steps (review view failed: %s)"
                      (length steps) view-error))
             ((superchat-plan--view-available-p)
              (format
               (concat "Plan proposed: %d steps. Review in the side "
                       "window: uncheck steps to drop, then Approve.")
               (length steps)))
             (t
              (format
               (concat "Plan proposed: %d steps (textui unavailable; "
                       "M-x superchat-plan-approve approves all steps).")
               (length steps))))))))))

;;;###autoload
(defun superchat-plan-approve ()
  "Approve the plan under review in the current session.
Checked steps become the active plan and are injected into every
following execution Run until `superchat-plan-clear'.  The checked
state is read from the plan data, never from rendered text."
  (interactive)
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (if (not superchat-plan--proposed)
        (message "No plan under review.")
      (let* ((all-steps (plist-get superchat-plan--proposed :steps))
             (included
              (mapcar (lambda (step) (plist-get step :id))
                      (cl-remove-if-not
                       (lambda (step) (plist-get step :checked))
                       all-steps)))
             (steps
              (mapcar
               (lambda (step)
                 ;; :status pre-seeded: setf on (plist-get step :status)
                 ;; mutates in place only when the key already exists.
                 (append step (list :included
                                    (and (plist-get step :checked) t)
                                    :status nil)))
               all-steps))
             (event-id
              (plist-get
               (superchat-tape-append-event-record
                (list :type "plan.approved" :schema 1
                      :session-id superchat--session-id
                      :actor-id "user"
                      :payload
                      (list :proposed
                            (list :session-id superchat--session-id
                                  :event-id
                                  (plist-get superchat-plan--proposed
                                             :event-id))
                            ;; vector: see the plan.proposed event builder
                            :steps (vconcat steps))))
               :event-id)))
        (setq superchat--active-plan
              (list :summary (plist-get superchat-plan--proposed :summary)
                    :event-id event-id :steps steps))
        (setq superchat-plan--proposed nil)
        ;; Approval is the planning→execution phase transition: exit the
        ;; planning identity so the execution Run is not governed by a
        ;; persona that forbids executing before approval.  The active
        ;; plan still travels with the Run via the prompt hook.
        (when (and (boundp 'superchat--active-preset)
                   superchat--active-preset
                   (fboundp 'superchat-preset-plan-p)
                   (superchat-preset-plan-p superchat--active-preset))
          (setq superchat--active-preset nil))
        (let ((auto (and included
                         superchat-plan-auto-execute
                         (boundp 'superchat--prompt-start)
                         superchat--prompt-start
                         (marker-position superchat--prompt-start))))
          (when auto
            ;; Approving authorizes execution; queue the follow-up into
            ;; the input region and send it so the Run starts now.  The
            ;; prompt hook injects the active plan into this request.
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert "Execute the approved plan now."))
            (setq superchat-plan--nudge-pending t
                  superchat-plan--nudges 0)
            (superchat-send-input))
          (message "Plan approved: %d/%d steps active%s"
                   (length included) (length steps)
                   (if auto " — executing" "")))
        ;; Switch the review window to the progress view.  Last: window
        ;; operations may change the selected window, so nothing after
        ;; this may rely on the chat buffer being current.
        (when (superchat-plan--view-available-p)
          (superchat-plan--view-update))))))

;;;###autoload
(defun superchat-plan-clear ()
  "Clear the session's active plan and any plan under review.
Appends `plan.cleared' when a plan was under review, so a rebuilt
session View does not resurrect a review the user discarded."
  (interactive)
  (when (and (boundp 'superchat--session-id)
             superchat--session-id
             superchat-plan--proposed)
    (superchat-tape-append-event-record
     (list :type "plan.cleared" :schema 1
           :session-id superchat--session-id
           :actor-id "user"
           :payload
           (list :proposed
                 (list :session-id superchat--session-id
                       :event-id
                       (plist-get superchat-plan--proposed
                                  :event-id))))))
  (setq superchat--active-plan nil
        superchat-plan--proposed nil
        superchat-plan--nudge-pending nil
        superchat-plan--nudges 0)
  (superchat-plan--view-close)
  (message "Active plan cleared."))

(defun superchat-plan--context-string (plan)
  "Return the prompt context block for the approved PLAN."
  (let ((included
         (cl-remove-if-not (lambda (step) (plist-get step :included))
                           (plist-get plan :steps))))
    (concat
     (format "Approved plan: %s\n" (or (plist-get plan :summary) "plan"))
     (mapconcat
      (lambda (step)
        (format "%d. %s\n   (done when: %s)"
                (plist-get step :id)
                (plist-get step :text)
                (plist-get step :accept)))
      included
      "\n")
     "\n\nThis plan is approved. Execute the included steps now. "
     "Report progress as you go: call report_plan_progress with an "
     "updates list of (step-id status) pairs — mark the step "
     "in-progress when you start it and done only when its completion "
     "criterion holds. Never mark a step done before its criterion "
     "holds. The plan panel in the side window shows these statuses: "
     "keep them current or the session will ask you to finish the "
     "plan. "
     "Do not propose a new plan.")))

(provide 'superchat-plan)

;;; superchat-plan.el ends here
