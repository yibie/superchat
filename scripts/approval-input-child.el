;;; approval-input-child.el --- Interactive child of approval-input-regression -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs ONE scenario in an interactive Emacs (started under a pseudo-terminal
;; by approval-input-regression.el), because the input queue the contract is
;; about does not exist in batch: there the question reads stdin.  The scenario
;; runs from a timer, the way a tool call reaches the approval question in the
;; product (from a process filter or a timer, not from a command).
;;
;; Protocol: before each question a marker file LABEL.open appears in
;; $AIC_DIR; the parent then types real keys into the terminal.  Results are
;; written to $AIC_DIR/result.el and Emacs exits.

;;; Code:

(require 'cl-lib)
(require 'superchat-approval)
(require 'superchat-runtime)

(defconst aic-dir (file-name-as-directory (getenv "AIC_DIR")))
(defconst aic-request
  (list :tool-name "eval_elisp"
        :reason "eval_elisp has full control of Emacs"
        :args '("(with-current-buffer \"init.el\" (save-buffer))")))

(defun aic-mark (name)
  (write-region "" nil (expand-file-name name aic-dir) nil 'silent))

(defun aic-ask (label)
  "Ask the product's default question; return (LABEL ANSWER SECONDS)."
  (aic-mark (concat label ".open"))
  (let ((start (float-time)))
    (list label
          (condition-case nil
              (if (superchat-approval-confirm aic-request) 'allowed 'rejected)
            (quit 'quit))
          (- (float-time) start))))

(defvar aic-macro-seen nil)
(defun aic-macro-command ()
  "What a tool call inside a keyboard macro meets."
  (interactive)
  (setq aic-macro-seen
        (list :chain-has-a-human (and (superchat-approval--fallback-answerer) t)
              :direct-answer (if (superchat-approval-confirm aic-request) 'allowed 'rejected))))

(defun aic-run ()
  (let ((results
         (pcase (getenv "AIC_SCENARIO")
           ("stale-y"
            ;; Typed before the question exists; still in the queue.
            (setq unread-command-events (list ?y))
            (list (aic-ask "q1")))
           ("double-y" (list (aic-ask "q1") (aic-ask "q2")))
           ("fresh" (list (aic-ask "q1")))
           ("run-ends"
            ;; A Run that stops being able to act one second after the question
            ;; opened.  Nobody types anything.
            (let ((alive-until (+ (float-time) 1.0)))
              (advice-add 'superchat-runtime-effect-capable-p :override
                          (lambda (_run) (< (float-time) alive-until)))
              (aic-mark "q1.open")
              (let ((start (float-time)))
                (list (list "q1"
                            (superchat-approval--ask-while-run-lives
                             "aic-run" "eval_elisp"
                             (lambda () (if (superchat-approval-confirm
                                             (append aic-request '(:run-id "aic-run")))
                                            'allowed 'rejected)))
                            (- (float-time) start))))))
           ("macro"
            (global-set-key [f9] #'aic-macro-command)
            (let ((buffer (get-buffer-create "aic-typing")))
              (switch-to-buffer buffer)
              (execute-kbd-macro (vector 'f9 ?y))
              (list (append aic-macro-seen
                            (list :typed-into-buffer
                                  (with-current-buffer buffer (buffer-string)))))))
           (other (list (list 'unknown-scenario other))))))
    (let ((print-length nil) (print-level nil))
      (write-region (prin1-to-string results) nil
                    (expand-file-name "result.el" aic-dir) nil 'silent))
    (kill-emacs 0)))

(run-at-time 0.3 nil #'aic-run)

;;; approval-input-child.el ends here
