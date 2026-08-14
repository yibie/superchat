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

(defvar superchat-plan--pending-tree nil
  "Tree text waiting to render after the current Run's response.
Set by `superchat-plan-propose' and consumed by
`superchat-plan--render-pending' on turn completion, so the review
subtree lands after the assistant's final text, not in the middle of
the tool round.")

(defvar superchat-plan--pending-session nil
  "Session id that owns `superchat-plan--pending-tree'.")

(defun superchat-plan--validate-steps (steps)
  "Return a rejection string for STEPS, or nil when they are valid.
Error messages carry a concrete example: the model that just failed
retries with the same shape unless the correction is pedagogical."
  (cond
   ((not (listp steps))
    "steps must be a list of (text accept) pairs, e.g. [[\"修改第二行\" \"第二行内容等于目标文本\"]]")
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
                            "e.g. [\"修改第二行\" \"第二行内容等于目标文本\"]")
                    id)))
             do (unless (= 2 (length step))
                  (cl-return
                   (format
                    (concat "step %d has %d elements. Each step must be "
                            "a (text accept) PAIR of exactly two strings: "
                            "[\"what to do\" \"how to verify it\"], "
                            "e.g. [\"修改第二行\" \"第二行内容等于目标文本\"]")
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
                :steps
                (cl-loop for step in steps
                         for id from 1
                         collect (list :id id :text (car step)
                                       :accept (cadr step))))))
   :event-id))

(defun superchat-plan--insert-tree (summary event-id steps)
  "Render the plan under review as an org subtree with checkboxes.
Each checkbox line carries a local C-c C-c binding that toggles the
step, so the user can check/uncheck without leaving the session's
read-only rendering discipline."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert (format "\n** Plan: %s  [0/%d]\n"
                        (or summary "execution plan") (length steps)))
        (add-text-properties
         start (1- (point))
         (list 'keymap
               (let ((map (make-sparse-keymap)))
                 (define-key map (kbd "C-c C-c")
                             #'superchat-plan-approve)
                 map)
               'help-echo
               "C-c C-c approves the plan")))
      (insert (format ":PROPERTIES:\n:SUPERCHAT_PLAN: %s\n:END:\n" event-id))
      (dolist (step steps)
        (let ((start (point)))
          (insert (format "- [ ] %s\n" (plist-get step :text)))
          (add-text-properties
           start (1- (point))
           (list 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "C-c C-c")
                               #'superchat-plan-toggle-step)
                   map)
                 'help-echo
                 "C-c C-c toggles this step")))
        (insert (format "  :accept: %s\n" (plist-get step :accept)))))))

;;;###autoload
(defun superchat-plan-toggle-step ()
  "Toggle the checkbox of the plan step at point.
Bound to C-c C-c on rendered plan steps via a text-property keymap."
  (interactive)
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "- \\[\\([ X]*\\)\\] ")
        (let ((inhibit-read-only t))
          (replace-match
           (concat "- " (if (equal "X" (match-string 1)) "[ ]" "[X]") " ")
           nil nil nil 0)
          ;; replace-match drops text properties; restore the local
          ;; binding so the step stays toggleable.
          (add-text-properties
           (line-beginning-position) (line-end-position)
           (list 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "C-c C-c")
                               #'superchat-plan-toggle-step)
                   map)
                 'help-echo "C-c C-c toggles this step"))
          t)))))

(defun superchat-plan--at-step-line-p ()
  "Return non-nil when point is on a rendered plan step line."
  (save-excursion
    (beginning-of-line)
    (looking-at "- \\[\\([ X]*\\)\\] ")))

(defun superchat-plan--at-headline-p ()
  "Return non-nil when point is on the plan review headline."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\*\\* Plan: ")))

(defun superchat-plan--review-cc ()
  "Dispatch C-c C-c during plan review: toggle a step, approve the
plan, or fall back to sending the current input."
  (interactive)
  (cond ((superchat-plan--at-step-line-p) (superchat-plan-toggle-step))
        ((superchat-plan--at-headline-p) (superchat-plan-approve))
        (t (call-interactively #'superchat-send-input))))

(defvar superchat-plan-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-plan--review-cc)
    map))

;;;###autoload
(define-minor-mode superchat-plan-review-mode
  "Review-mode keybindings while a plan is under review.
C-c C-c toggles a step, approves on the headline, and falls back to
sending the current input elsewhere.  Buffer-local bindings survive
re-renders that strip text properties from the conversation text."
  :lighter " Plan-Review")

(defun superchat-plan--render-pending ()
  "Insert the pending plan tree after the assistant response.
Called from `superchat-turn-completed-functions' when the Run that
proposed the plan reaches a terminal state; inserts before the next
input prompt so the review subtree follows the assistant's text."
  (when (and superchat-plan--pending-tree
             (boundp 'superchat--session-id)
             superchat--session-id
             (equal superchat-plan--pending-session
                    superchat--session-id))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when (and (boundp 'superchat--prompt-start)
                   superchat--prompt-start
                   (marker-position superchat--prompt-start))
          ;; Insert BEFORE the "* User" headline line.  Inserting at the
          ;; marker itself would land inside the input region (the marker
          ;; is where typed input begins) and the tree text would be
          ;; re-sent as the next request.
          (goto-char superchat--prompt-start)
          (beginning-of-line))
        (insert superchat-plan--pending-tree)
        (superchat-plan-review-mode 1)))
    (setq superchat-plan--pending-tree nil
          superchat-plan--pending-session nil)))

(defun superchat-plan--flush-on-terminal (status)
  "Render a pending plan tree when STATUS is a terminal Run state."
  (when (fboundp 'superchat-runtime--terminal-p)
    (when (superchat-runtime--terminal-p (plist-get status :status))
      (superchat-plan--render-pending))))

(add-hook 'superchat-turn-completed-functions
          #'superchat-plan--flush-on-terminal)

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
          ;; Defer the subtree until the Run finishes, so it lands after
          ;; the assistant's final text (turn-completed hook renders it).
          (setq superchat-plan--pending-tree
                (with-temp-buffer
                  (let ((superchat-buffer-name (current-buffer)))
                    (superchat-plan--insert-tree summary event-id structured))
                  ;; buffer-substring keeps text properties (buffer-string
                  ;; strips them, which would drop the local keymaps).
                  (buffer-substring (point-min) (point-max)))
                superchat-plan--pending-session superchat--session-id)
          (format
           (concat "Plan proposed: %d steps. Review the checkboxes "
                   "(C-c C-c on a step toggles it), then "
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
  (superchat-plan--render-pending)
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
