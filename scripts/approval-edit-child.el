;;; approval-edit-child.el --- Interactive child of approval-edit-regression -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs ONE scenario in an interactive Emacs (started under a pseudo-terminal
;; by approval-edit-regression.el), because the recursive edit a published
;; question waits in does not exist in batch: there the wait polls.  The
;; question is published from a timer, the way a tool call reaches it in the
;; product (from a process filter or a timer, not from a command).
;;
;; Protocol: before the question a marker file LABEL.open appears in
;; $AEC_DIR; the parent then types real keys into the terminal, or nothing.
;; The result is written to $AEC_DIR/result.el and Emacs exits.

;;; Code:

(require 'cl-lib)
(require 'superchat-approval)
(require 'superchat-runtime)

(defconst aec-dir (file-name-as-directory (getenv "AEC_DIR")))
(defconst aec-request
  (list :tool-name "eval_elisp"
        :reason "eval_elisp has full control of Emacs"
        :args '("(progn (setq counter 0) (setq counter (+ 2 counter)) counter)")))

(defvar aec-announced nil "What the product told the user, newest first.")
(defvar aec-depths nil "The recursion depths a 0.1 s timer saw, newest first.")
(defvar aec-ticks 0 "How often that timer ran.")

(defun aec-mark (name)
  (write-region "" nil (expand-file-name name aec-dir) nil 'silent))

(defun aec-answer-later (seconds allow)
  "After SECONDS, answer the open question by its handle, as emacsclient would."
  (run-at-time seconds nil
               (lambda ()
                 (when-let* ((question (car (superchat-approval-open-questions))))
                   (superchat-approval-answer (plist-get question :handle) allow)))))

(defun aec-publish (label)
  "Publish the product's question from here; return what happened."
  (aec-mark (concat label ".open"))
  (let ((start (float-time)))
    (list :label label
          :answer (condition-case nil
                      (if (superchat-approval-publish aec-request) 'allowed 'rejected)
                    (quit 'quit))
          :seconds (- (float-time) start)
          :depth-after (recursion-depth)
          :max-depth (apply #'max 0 aec-depths)
          :ticks aec-ticks
          :interruption superchat-approval--interruption
          :announced (reverse aec-announced))))

(defmacro aec-without-an-edit (&rest body)
  "Run BODY where `recursive-edit' comes back at once, as it does while input
is blocked (keyboard.c returns nil then)."
  `(cl-letf (((symbol-function 'recursive-edit) (lambda () nil)))
     ,@body))

(defun aec-run ()
  (advice-add 'superchat-approval--announce :before
              (lambda (text) (push text aec-announced)))
  (run-with-timer 0 0.1 (lambda ()
                          (cl-incf aec-ticks)
                          (push (recursion-depth) aec-depths)))
  (let ((result
         (pcase (getenv "AEC_SCENARIO")
           ;; The parent types the answer.
           ("typed" (aec-publish "q1"))
           ;; Emacs cannot hold the edit; an answer comes by handle after 1 s.
           ("no-edit"
            (aec-answer-later 1.0 t)
            (aec-without-an-edit (aec-publish "q1")))
           ;; Emacs cannot hold the edit; nobody answers; the wait is bounded.
           ("no-edit-unanswered"
            (setq superchat-approval-unheld-wait 1.5)
            (aec-without-an-edit (aec-publish "q1")))
           ;; Emacs cannot hold the edit; the Run dies at 1 s, long before
           ;; the bound.
           ("no-edit-run-ends"
            (setq superchat-approval-unheld-wait 20)
            (let ((alive-until (+ (float-time) 1.0)))
              (advice-add 'superchat-runtime-effect-capable-p :override
                          (lambda (_run) (< (float-time) alive-until)))
              (aec-mark "q1.open")
              (let ((start (float-time)))
                (list :label "q1"
                      :answer (aec-without-an-edit
                               (superchat-approval--ask-while-run-lives
                                "aec-run" "eval_elisp"
                                (lambda ()
                                  (if (superchat-approval-publish
                                       (append aec-request '(:run-id "aec-run")))
                                      'allowed 'rejected))))
                      :seconds (- (float-time) start)
                      :depth-after (recursion-depth)
                      :announced (reverse aec-announced)))))
           (other (list :answer 'unknown-scenario :label other)))))
    (let ((print-length nil) (print-level nil))
      (write-region (prin1-to-string result) nil
                    (expand-file-name "result.el" aec-dir) nil 'silent))
    (kill-emacs 0)))

(run-at-time 0.3 nil #'aec-run)

;;; approval-edit-child.el ends here
