;;; task-sim-fixes-regression.el --- fixes from the first task simulation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-09-27-fixes-from-the-first-task-simulation.md:
;; a bounded eval_elisp value, ask_user sharing a response as a correctable
;; mistake, and the context preflight naming the request.  The canonical
;; smoke fixture supplies the Session and a scripted model; no real model and
;; no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/task-sim-fixes-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^task-sim-fixes/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; P1: a successful eval_elisp value is bounded ---------------------------------

(ert-deftest task-sim-fixes/a-small-value-is-unchanged ()
  (skip-when (getenv "SMOKE_REAL"))
  (should (equal "Result: 3" (superchat-tool-eval-elisp "(+ 1 2)")))
  (should (equal "Result: \"abc\"" (superchat-tool-eval-elisp "\"abc\""))))

(ert-deftest task-sim-fixes/a-huge-value-keeps-head-and-tail-and-says-so ()
  "The observed case was a 108,699-byte keymap dump."
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((result (superchat-tool-eval-elisp
                  "(concat \"HEAD\" (make-string 108000 ?x) \"TAIL\")"))
         (whole (+ 108008 2)))          ; printed with its two quotes
    (should (< (length result) 21000))
    (should (string-prefix-p "Result: \"HEAD" result))
    (should (string-suffix-p "TAIL\"" result))
    (should (string-match-p (format "%d characters" whole) result))
    (should (string-match-p (format "%d characters were cut" (- whole 20000)) result))
    (should (string-match-p "smaller" result))))

(ert-deftest task-sim-fixes/a-value-at-the-limit-is-unchanged ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((result (superchat-tool-eval-elisp "(make-string 19998 ?y)")))
    ;; 19,998 characters and two quotes: exactly the limit.
    (should (equal (concat "Result: \"" (make-string 19998 ?y) "\"") result)))
  (should (string-match-p "cut" (superchat-tool-eval-elisp "(make-string 19999 ?y)"))))

(ert-deftest task-sim-fixes/errors-keep-their-own-bound ()
  "Guard: an error result is bounded as before, not by the new value limit."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((result (superchat-tool-eval-elisp "(error \"%s\" (make-string 5000 ?e))")))
    (should (string-prefix-p "Error: evaluating expression: " result))
    (should (string-match-p "\\[Error details truncated\\]" result))))

;;;; P2: ask_user sharing a response is corrected, not fatal ------------------------

(defun tsf--deliver-like-llm (original provider prompt result multi response error)
  "Call ORIGINAL; a signal from it reaches ERROR as llm.el delivers it.
Through the real transport, a condition signalled while llm.el processes a
response arrives at the request's error callback as the type `error' and
the condition's message (observed in the task simulation's run
TB-fake-batch, 2026-09-27); the smoke fixture calls the processing from a
timer, where it would escape instead."
  (condition-case err
      (funcall original provider prompt result multi response error)
    (error (funcall error 'error (error-message-string err)))))

(defmacro tsf--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tools-list nil))
       (advice-add 'llm-provider-utils-process-result :around #'tsf--deliver-like-llm)
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (advice-remove 'llm-provider-utils-process-result #'tsf--deliver-like-llm)
         (setq superchat-llm-tools-list nil)))))

(ert-deftest task-sim-fixes/ask-user-with-another-call-is-corrected ()
  (let ((probe (get-buffer-create "tsf-probe")))
    (with-current-buffer probe (erase-buffer) (insert "TSF-PROBE-TEXT\n"))
    (unwind-protect
        (tsf--fixture (list '(:tool-uses (("ask_user" (question . "Which file?"))
                                          ("read_buffer" (buffer . "tsf-probe"))))
                            '(:text "TSF-DONE"))
          (progn
            (let* ((view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view))
                   (run (smoke--send view "which file is the diary?")))
              (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
              (let ((status (superchat-runtime-status run)))
                ;; The Run goes on: it is not failed with an API error.
                (should (eq 'completed (plist-get status :status))))
              (should (= 2 (length smoke--requests)))
              ;; The second request tells the model what to do differently.
              (should (string-match-p "ask_user must be the only"
                                      (plist-get (car smoke--requests) :prompt-text)))
              ;; Neither call of the refused response ran: no question was
              ;; asked and nothing was read.
              (should-not (cl-find-if (lambda (event)
                                        (and (equal "tool.requested" (plist-get event :type))
                                             (equal run (plist-get event :run-id))))
                                      (superchat-tape-replay-events session)))
              (should (string-match-p "TSF-DONE" (with-current-buffer view (buffer-string))))
              (should-not (string-match-p "API error" (with-current-buffer view (buffer-string)))))))
      (kill-buffer probe))))

;;;; P3: the preflight names the request ---------------------------------------------

(ert-deftest task-sim-fixes/the-preflight-says-the-request-is-too-long ()
  "A small model limit makes an ordinary long question exceed it."
  (tsf--fixture (list '(:text "never sent"))
    (cl-letf (((symbol-function 'llm-chat-token-limit) (lambda (&rest _) 6000)))
      (let* ((view (smoke--open-chat))
             (run (smoke--send view (concat "TSF-LONG " (make-string 30000 ?z)))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
        (should (= 0 (length smoke--requests)))
        (let ((text (with-current-buffer view (buffer-string))))
          (should (string-match-p "请求过长" text))
          (should-not (string-match-p "文件过大" text)))))))

(provide 'task-sim-fixes-regression)

;;; task-sim-fixes-regression.el ends here
