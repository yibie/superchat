;;; answer-regression.el --- a question can be found and answered -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for two things measured with real input on 2026-11-01
;; (docs/research/super-agent.md, "审批在哪里"):
;;
;; - After a button of the card was pressed by key, what the user typed next went
;;   into the card.  The card is read-only and a cursor on it goes back to the
;;   input area
;;   (docs/decisions/accepted/2026-10-31-what-you-type-lands-where-you-type.md).
;; - A question nobody is looking at waited until its deadline without the mode
;;   line saying so.  The mode line counts open questions, and it opens the list
;;   (docs/decisions/accepted/2026-10-31-work-nobody-asked-for-has-its-own-place.md).
;;
;; A batch Emacs has one frame and nobody at the keyboard: "the cursor" here is
;; point in the selected window, and a key on a button is `widget-button-press'
;; at point.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/answer-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^answer/")'

;;; Code:

(load (expand-file-name "own-place-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'wid-edit)

(defconst answer--asking '(:tool-uses (("eval_elisp" (expression . "1"))))
  "A round of the model that asks the user: `eval_elisp' at the default level.")

(defvar answer--give-up nil
  "The timer of `answer--give-up-later'; one is enough, and it belongs to one test.")

(defun answer--give-up-later ()
  "Answer whatever is still open a while from now.
A question left open would make its turn wait for ever, and an error in a timer
goes unnoticed: a test that went wrong should fail, not hang."
  (when (timerp answer--give-up)
    (cancel-timer answer--give-up))
  (setq answer--give-up
        (run-at-time 4 nil (lambda ()
                             (dolist (question (superchat-approval-open-questions))
                               (superchat-approval-answer (plist-get question :handle) nil))))))

(defun answer--view (root)
  "A chat View under ROOT, with nothing left to count from an earlier test."
  (setq superchat-changes--notices nil)
  (superchat-changes--notices-refresh)
  (answer--give-up-later)
  (with-current-buffer (wtimer--file root) (superchat)))

(defun answer--button (view tag)
  "Where VIEW's newest button TAG is."
  (with-current-buffer view
    (save-excursion
      (goto-char (point-max))
      (search-backward tag))))

(defun answer--on-button (view tag)
  "Select VIEW's window and put the cursor on its newest button TAG."
  (select-window (get-buffer-window view))
  (with-current-buffer view
    (goto-char (answer--button view tag))))

(defun answer--open-handle ()
  "The handle of the open question."
  (plist-get (car (superchat-approval-open-questions)) :handle))

;;;; The card ------------------------------------------------------------------------

(ert-deftest answer/a-button-pressed-by-key-leaves-the-cursor-where-you-type ()
  ;; Measured 2026-11-01: the next words read "next wordsAllowed once" (2 runs of 2).
  (dolist (tag '("Allow once" "Allow all" "Deny"))
    (wtimer--with (list answer--asking '(:text "DONE")) (p)
      (let* ((view (answer--view p))
             (superchat-approval-function #'superchat-approval-publish))
        (run-at-time 0.2 nil (lambda ()
                               (answer--on-button view tag)
                               (with-current-buffer view (widget-button-press (point)))))
        (hm--turn view "go")
        (should (eq view (window-buffer (selected-window))))
        (with-current-buffer view
          (should (= (point) (point-max)))
          (let ((last-command-event ?x))
            (self-insert-command 1)))
        (should (equal "x" (presence--input view)))))))

(ert-deftest answer/a-draft-stays-and-the-cursor-goes-to-its-end ()
  (wtimer--with (list answer--asking '(:text "DONE")) (p)
    (let* ((view (answer--view p))
           (superchat-approval-function #'superchat-approval-publish))
      (run-at-time 0.2 nil (lambda ()
                             (presence--type view "half a message")
                             (answer--on-button view "Allow once")
                             (with-current-buffer view (widget-button-press (point)))))
      (hm--turn view "go")
      (should (equal "half a message" (presence--input view)))
      (with-current-buffer view
        (should (= (point) (point-max)))))))

(ert-deftest answer/a-cursor-outside-the-card-stays-where-it-is ()
  ;; A click presses the button without moving the cursor, and so does the list.
  (wtimer--with (list answer--asking '(:text "DONE")) (p)
    (let* ((view (answer--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (place nil))
      (run-at-time 0.2 nil (lambda ()
                             (select-window (get-buffer-window view))
                             (goto-char (point-min))
                             (setq place (point-marker))
                             (with-current-buffer view
                               (widget-button-press (answer--button view "Allow once")))))
      (hm--turn view "go")
      (should place)
      (with-current-buffer view
        (should (= (point) place))))))

(ert-deftest answer/a-cursor-in-another-window-stays-where-it-is ()
  (wtimer--with (list answer--asking '(:text "DONE")) (p)
    (let* ((view (answer--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (code (presence--code-window))
           (place nil))
      (run-at-time 0.2 nil (lambda ()
                             (with-current-buffer view
                               (goto-char (answer--button view "Allow once")))
                             (select-window code)
                             (setq place (point-marker))
                             (with-current-buffer view
                               (widget-button-press (answer--button view "Allow once")))))
      (hm--turn view "go")
      (should place)
      (should (eq code (selected-window)))
      (should (= (window-point code) place)))))

(ert-deftest answer/the-card-cannot-be-typed-on ()
  (wtimer--with (list answer--asking '(:text "DONE")) (p)
    (let* ((view (answer--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (open nil))
      (cl-flet ((protected (text)
                  (with-current-buffer view
                    (save-excursion
                      (goto-char (point-max))
                      (search-backward text)
                      (let ((inhibit-read-only nil))
                        (condition-case nil
                            (progn (insert "x") nil)
                          (text-read-only t)))))))
        (run-at-time 0.2 nil
                     (lambda ()
                       (setq open (list (protected "Approval needed")
                                        (protected "Waiting for your answer")
                                        (protected "Allow once")
                                        (protected "Details")))
                       (superchat-approval-answer (answer--open-handle) t)))
        (hm--turn view "go")
        (should (equal '(t t t t) open))
        ;; Saying how it ended rewrites a line of it.
        (should (protected "Allowed once"))
        (should (protected "Deny"))))))

;;;; The mode line --------------------------------------------------------------------

(ert-deftest answer/the-mode-line-says-a-question-waits ()
  (wtimer--with (list answer--asking '(:text "DONE")) (p)
    (let* ((view (answer--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (during nil))
      (should-not superchat-changes-notice-mode-line)
      (run-at-time 0.2 nil (lambda ()
                             (setq during superchat-changes-notice-mode-line)
                             (superchat-approval-answer (answer--open-handle) t)))
      (hm--turn view "go")
      (should (string-match-p "\\[Superchat: 1 asking\\]" (or during "")))
      (should (string-match-p "question" (get-text-property 0 'help-echo during)))
      (should (eq 'superchat-pending
                  (lookup-key (get-text-property 0 'local-map during) [mode-line mouse-1])))
      ;; Answered: the user's own execution leaves nothing to count.
      (should-not superchat-changes-notice-mode-line))))

(ert-deftest answer/the-mode-line-counts-a-question-and-what-ended ()
  (wtimer--with (list '(:text "TASK-ANSWER") answer--asking '(:text "DONE")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (setq superchat-changes--notices nil)
      (answer--give-up-later)
      (own-place--fire-and-settle timer)
      (should (string-match-p "\\[Superchat: 1\\]" superchat-changes-notice-mode-line))
      (let ((superchat-approval-function #'superchat-approval-publish)
            (during nil))
        (run-at-time 0.2 nil (lambda ()
                               (setq during superchat-changes-notice-mode-line)
                               (superchat-approval-answer (answer--open-handle) t)))
        (hm--turn view "go")
        (should (string-match-p "\\[Superchat: 1 asking, 1\\]" (or during "")))
        (should (string-match-p "\\[Superchat: 1\\]" superchat-changes-notice-mode-line))))))

(ert-deftest answer/a-question-of-a-task-is-found-from-the-mode-line ()
  ;; Seeing the mark, opening the list, answering in its first row.
  (wtimer--with (list answer--asking '(:text "TASK-ANSWER")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "TASK-MARKER")
      (setq superchat-changes--notices nil)
      (answer--give-up-later)
      (let ((superchat-timer-task-level 'edit-buffers)
            (superchat-approval-function #'superchat-approval-publish)
            (during nil) (row nil))
        (run-at-time 0.2 nil (lambda ()
                               (setq during superchat-changes-notice-mode-line
                                     row (car (superchat-pending-items)))
                               (superchat-pending-allow row)))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (should (string-match-p "\\[Superchat: 1 asking\\]" (or during "")))
        (should (eq 'asking (plist-get row :kind)))
        ;; It ended: what is left to look at is that it did.
        (should (string-match-p "\\[Superchat: 1\\]" superchat-changes-notice-mode-line))))))

(ert-deftest answer/two-questions-count-two ()
  (wtimer--with (list answer--asking answer--asking '(:text "ONE") '(:text "TWO")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (setq superchat-changes--notices nil)
      (answer--give-up-later)
      (let ((superchat-timer-task-level 'edit-buffers)
            (superchat-approval-function #'superchat-approval-publish)
            (during nil))
        (run-at-time 0.2 nil (lambda () (smoke--send view "go")))
        (run-at-time 0.8 nil (lambda ()
                               (setq during superchat-changes-notice-mode-line)
                               (dolist (question (superchat-approval-open-questions))
                                 (superchat-approval-answer (plist-get question :handle) t))))
        (superchat-timer-fire timer (plist-get timer :due))
        (wtimer--settled)
        (should (string-match-p "\\[Superchat: 2 asking" (or during "")))))))

(provide 'answer-regression)
;;; answer-regression.el ends here
