;;; stall-retry-regression.el --- an attempt that stays silent is sent again -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; When `superchat-response-timeout' expires before a provider attempt has
;; produced any answer text, reasoning or tool-call data, the provider has
;; stalled.  The attempt is given up -- its transport cancelled, anything it
;; still delivers ignored -- and the request is sent again after
;; `superchat-llm-transient-retry-interval', up to `superchat-llm-stall-retries'
;; times in a row.  An attempt that produced something still ends the
;; execution as a timed-out response.  Decision:
;; docs/decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md.
;;
;; Run from the repository root, with an existing prepared dependency directory:
;;
;;   SUPERCHAT_TEST_ELPA=/path/to/prepared/elpa
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$SUPERCHAT_TEST_ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega -L scripts \
;;     -l scripts/stall-retry-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^stall-retry/")'

;;; Code:

(load (expand-file-name "tool-hooks-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-llm-streaming)
(defvar superchat-llm-stall-retries)
(defvar superchat-llm-transient-retry-interval)
(defvar superchat-response-timeout)

(defmacro sr--run (script settings &rest body)
  "Send one message with SCRIPT as the model and SETTINGS bound in the fixture.
Every attempt gets a one-second inactivity window, whatever its prompt size.
BODY sees VIEW, SESSION and RUN."
  (declare (indent 2) (debug t))
  `(pend--fresh
     (hm--fixture ,script
       (cl-letf (((symbol-function 'superchat-llm--adaptive-response-timeout)
                  (lambda (base _tokens) (and base 1))))
         (let* ((superchat-response-timeout 1)
                (superchat-llm-transient-retry-interval 0.2)
                ,@settings
                (view (smoke--open-chat))
                (session (buffer-local-value 'superchat--session-id view))
                (run (smoke--send view "go")))
           (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
           ,@body)))))

(defun sr--status (run)
  (plist-get (superchat-runtime-status run) :status))

(defun sr--chat (view)
  (with-current-buffer view (buffer-string)))

(ert-deftest stall-retry/1-a-silent-attempt-is-sent-again ()
  (sr--run (list '(:hang t) '(:text "SR-DONE")) ()
    (should (eq 'completed (sr--status run)))
    (should (= 2 (length smoke--requests)))
    (should (string-match-p "SR-DONE" (sr--chat view)))
    (should-not (string-match-p "Response timeout" (sr--chat view)))))

(ert-deftest stall-retry/2-what-the-silent-attempt-sends-later-is-ignored ()
  "The given-up attempt answers between the timeout and the retry."
  (dolist (late (list '(:delay 2.0 :error (error . "curl process killed"))
                      '(:delay 2.0 :text "SR-LATE")))
    (sr--run (list late '(:text "SR-DONE"))
        ((superchat-llm-transient-retry-interval 2.5))
      (should (eq 'completed (sr--status run)))
      (should (= 2 (length smoke--requests)))
      (should (string-match-p "SR-DONE" (sr--chat view)))
      (should-not (string-match-p "SR-LATE" (sr--chat view))))))

(ert-deftest stall-retry/3-an-attempt-that-said-something-is-not-sent-again ()
  (dolist (said (list '(:chunks ("SR-PARTIAL ") :hang t)
                      '(:chunks ((:reasoning "SR-THINKING")) :hang t)))
    (sr--run (list said '(:text "SR-NEVER"))
        ((superchat-llm-streaming t))
      (should-not (eq 'completed (sr--status run)))
      (should (= 1 (length smoke--requests)))
      (should (string-match-p "Response timeout" (sr--chat view)))
      (should-not (string-match-p "SR-NEVER" (sr--chat view))))))

(ert-deftest stall-retry/4-after-its-limit-the-execution-ends-timed-out ()
  (sr--run (list '(:hang t) '(:hang t) '(:text "SR-NEVER"))
      ((superchat-llm-stall-retries 1))
    (should-not (eq 'completed (sr--status run)))
    (should (= 2 (length smoke--requests)))
    (should (string-match-p "Response timeout" (sr--chat view)))))

(ert-deftest stall-retry/5-zero-ends-the-execution-at-the-first-stall ()
  (sr--run (list '(:hang t) '(:text "SR-NEVER"))
      ((superchat-llm-stall-retries 0))
    (should-not (eq 'completed (sr--status run)))
    (should (= 1 (length smoke--requests)))
    (should (string-match-p "Response timeout" (sr--chat view)))))

(ert-deftest stall-retry/6-the-count-starts-over-after-a-reply-goes-through ()
  (sr--run (list '(:hang t) th--read '(:hang t) '(:text "SR-DONE"))
      ((superchat-llm-stall-retries 1))
    (should (eq 'completed (sr--status run)))
    (should (= 4 (length smoke--requests)))
    (should (= 1 (length (th--facts session "tool.completed"))))
    (should (string-match-p "SR-DONE" (sr--chat view)))))

(provide 'stall-retry-regression)

;;; stall-retry-regression.el ends here
