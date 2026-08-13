;;; superchat-plan.el --- Structured plans: propose, review, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A plan is a structured object, not prose.  The model proposes steps via
;; the `emit_plan' tool; the steps land on the Tape as `plan.proposed' and
;; render as a reviewable org subtree with checkboxes and :accept:
;; criteria.  The user checks the steps to keep, then approves;
;; `plan.approved' lands on the Tape and the included steps become the
;; session's active plan, injected into every following execution Run
;; until cleared.  Acceptance criteria are commitments for the human and
;; future Closure checks — they never drive automatic retries.

;;; Code:

(require 'cl-lib)
(require 'superchat-tape)

(defvar-local superchat--active-plan nil
  "The approved plan for this session.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S
:included P) ...))")

(defvar-local superchat-plan--proposed nil
  "The plan currently under review in this buffer.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S) ...))")

(defun superchat-plan--validate-steps (steps)
  "Return a rejection string for STEPS, or nil when they are valid."
  (cond
   ((not (listp steps))
    "steps must be a list of (text accept) pairs")
   ((null steps)
    "steps must not be empty")
   ((> (length steps) 20)
    "at most 20 steps")
   (t
    (cl-loop for step in steps
             for id from 1
             do (unless (and (listp step) (= 2 (length step)))
                  (cl-return (format "step %d must be (text accept)" id)))
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
                :steps
                (cl-loop for step in steps
                         for id from 1
                         collect (list :id id :text (car step)
                                       :accept (cadr step))))))
   :event-id))

(defun superchat-plan--insert-tree (summary event-id steps)
  "Render the plan under review as an org subtree with checkboxes."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "\n** Plan: %s  [0/%d]\n"
                      (or summary "execution plan") (length steps)))
      (insert (format ":PROPERTIES:\n:SUPERCHAT_PLAN: %s\n:END:\n" event-id))
      (dolist (step steps)
        (insert (format "- [ ] %s\n" (plist-get step :text)))
        (insert (format "  :accept: %s\n" (plist-get step :accept)))))))

(defun superchat-plan-propose (steps &optional summary)
  "Propose STEPS (a list of (text accept) pairs) as a reviewable plan.
Appends `plan.proposed' to the current session's Tape and renders the
org review subtree.  Returns a user-facing result string."
  (let ((rejection (superchat-plan--validate-steps steps)))
    (if rejection
        (format "Error: %s" rejection)
      (if (not (and (boundp 'superchat--session-id) superchat--session-id))
          "Error: no active Superchat session"
        (let* ((summary (string-trim (or summary "")))
               (event-id
                (superchat-plan--proposed-event
                 superchat--session-id steps summary))
               (structured
                (cl-loop for step in steps
                         for id from 1
                         collect (list :id id :text (car step)
                                       :accept (cadr step)))))
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (setq superchat-plan--proposed
                  (list :summary summary :event-id event-id
                        :steps structured)))
          (superchat-plan--insert-tree summary event-id structured)
          (format
           (concat "Plan proposed: %d steps. Review the checkboxes, "
                   "uncheck the steps to drop, then "
                   "M-x superchat-plan-approve.")
           (length steps)))))))

(defun superchat-plan--collect-checkboxes ()
  "Return (checked-ids . unchecked-ids) from the rendered review tree."
  (let ((checked nil)
        (unchecked nil))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^- \\[\\([ X]*\\)\\] \\(.*\\)$" nil t)
          (let ((text (match-string 2))
                (on-p (equal "X" (match-string 1))))
            (let ((match
                   (cl-find text
                            (plist-get superchat-plan--proposed :steps)
                            :test (lambda (needle step)
                                    (equal needle (plist-get step :text))))))
              (when match
                (if on-p
                    (push (plist-get match :id) checked)
                  (push (plist-get match :id) unchecked))))))))
    (cons (nreverse checked) (nreverse unchecked))))

;;;###autoload
(defun superchat-plan-approve ()
  "Approve the plan under review in the current session.
Checked steps become the active plan and are injected into every
following execution Run until `superchat-plan-clear'."
  (interactive)
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (if (not superchat-plan--proposed)
        (message "No plan under review.")
      (let* ((checked (superchat-plan--collect-checkboxes))
             (included (car checked))
             (steps
              (mapcar
               (lambda (step)
                 (append step (list :included
                                    (and (member (plist-get step :id) included)
                                         t))))
               (plist-get superchat-plan--proposed :steps)))
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
                            :steps steps)))
               :event-id)))
        (setq superchat--active-plan
              (list :summary (plist-get superchat-plan--proposed :summary)
                    :event-id event-id :steps steps))
        (setq superchat-plan--proposed nil)
        (message "Plan approved: %d/%d steps active"
                 (length included) (length steps))))))

;;;###autoload
(defun superchat-plan-clear ()
  "Clear the session's active plan."
  (interactive)
  (setq superchat--active-plan nil)
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
      "\n"))))

(provide 'superchat-plan)

;;; superchat-plan.el ends here
