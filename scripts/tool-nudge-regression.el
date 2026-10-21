;;; tool-nudge-regression.el --- the model is told when its calls keep failing or run short -*- lexical-binding: t; -*-

;;; Commentary:
;; Slices 9c and 9d (reports/guidance-2026-10-11/SLICE-9.md).
;;
;; 9c, made hard in slice 11c: after `superchat-tool-failure-brake' tool
;; calls in a row failed (default 3) the execution stops, through the
;; tool-loop hook `superchat-tool-failure-brake-hook'.  A call that succeeds
;; starts the count again; a call the permission level or the user refused is
;; not a failure.
;;
;; 9d: when 15 calls of `superchat-agent-max-tool-calls' are left (slice
;; 11a; it was 70%), the next request says so.  Once per execution.
;; While it runs, the status line shows "tools N/LIMIT", read from the
;; Runtime entry.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/tool-nudge-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^tool-nudge/")'

;;; Code:

(load (expand-file-name "eval-failure-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-tool-loop-hooks)

(defun tn--call (expression)
  (list :tool-uses (list (list "eval_elisp" (cons 'expression expression)))))

(defconst tn--fail (tn--call "(car 1)"))
(defconst tn--ok (tn--call "(+ 1 2)"))
(defconst tn--read '(:tool-uses (("read_buffer" (buffer . "*scratch*")))))

(defun tn--prompts ()
  "The prompt text of every request the scripted model received, in order."
  (mapcar (lambda (request) (plist-get request :prompt-text))
          (reverse smoke--requests)))

(defun tn--count (needle text)
  (let ((start 0) (n 0))
    (while (string-match (regexp-quote needle) text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun tn--first-with (needle)
  "The 1-based number of the first request whose prompt holds NEEDLE, or nil."
  (cl-position-if (lambda (text) (string-match-p (regexp-quote needle) text))
                  (tn--prompts)))

(defmacro tn--run (script &rest body)
  (declare (indent 1) (debug t))
  `(ef--fixture ,script
     (let ((view (smoke--open-chat)))
       (ef--run view "go")
       ,@body)))

;;;; 11c: three failed calls in a row stop the execution ---------------------
;; Rewritten in slice 11c: these five tests asserted 9c's soft sentence ("The
;; last 3 tool calls failed. Change approach ...") in the next request.  The
;; sentence is gone; the execution now stops after the third failure.

(defconst tn--stopped "Stopped after 3 failed calls in a row")

(defun tn--calls (view)
  "How many tool calls VIEW's conversation asked for."
  (cl-count "tool.requested"
            (superchat-tape-replay-events (buffer-local-value 'superchat--session-id view))
            :key (lambda (event) (plist-get event :type)) :test #'equal))

(defmacro tn--run-1 (script &rest body)
  "Like `tn--run', with VIEW and RUN bound."
  (declare (indent 1) (debug t))
  `(ef--fixture ,script
     (let* ((view (smoke--open-chat))
            (run (ef--run view "go")))
       ,@body)))

(ert-deftest tool-nudge/three-failures-in-a-row-stop-the-execution ()
  (tn--run-1 (list tn--fail tn--fail tn--fail tn--fail '(:text "TN-NEVER"))
    ;; No fourth call, and no request after the third failure.
    (should (= 3 (tn--calls view)))
    (should (= 3 (length smoke--requests)))
    (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
    (should (string-match-p tn--stopped (with-current-buffer view (buffer-string))))
    (should (equal tn--stopped
                   (plist-get (plist-get (superchat-runtime-status run) :tool-budget) :reason)))))

(ert-deftest tool-nudge/the-soft-sentence-is-gone ()
  (tn--run (list tn--fail tn--fail tn--fail '(:text "TN-NEVER"))
    (should-not (tn--first-with "Change approach"))))

(ert-deftest tool-nudge/a-success-starts-the-count-again ()
  (tn--run-1 (list tn--fail tn--fail tn--ok tn--fail tn--fail '(:text "TN-DONE"))
    (should (= 5 (tn--calls view)))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))
  (tn--run-1 (list tn--fail tn--fail tn--ok tn--fail tn--fail tn--fail tn--fail '(:text "TN-NEVER"))
    (should (= 6 (tn--calls view)))
    (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))))

(ert-deftest tool-nudge/refused-calls-are-not-failures ()
  "Guard: a call the permission level refuses is a decision, not a failure."
  (let ((superchat-sandbox-default-mode 'read-only))
    (ef--fixture (list tn--ok tn--ok tn--ok tn--ok '(:text "TN-DONE"))
      (let* ((superchat-sandbox-default-mode 'read-only)
             (view (smoke--open-chat))
             (run (ef--run view "go")))
        (should (string-match-p "permission level" (car (last (tn--prompts)))))
        (should (= 4 (tn--calls view)))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))))

(ert-deftest tool-nudge/the-brake-can-be-turned-off ()
  (let ((superchat-tool-failure-brake nil))
    (tn--run-1 (list tn--fail tn--fail tn--fail tn--fail '(:text "TN-DONE"))
      (should (= 4 (tn--calls view)))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest tool-nudge/no-hooks-no-brake ()
  "The brake is a tool-loop hook: with none, nothing stops."
  (let ((superchat-tool-loop-hooks nil))
    (tn--run-1 (list tn--fail tn--fail tn--fail tn--fail '(:text "TN-DONE"))
      (should (= 4 (tn--calls view))))))

;;;; 9d, as slice 11a set it: 15 calls before the limit --------------------
;; Rewritten in slice 11a: the reminder came at 70% of the limit (7 of 10);
;; it now comes when 15 calls are left (`superchat-tool-budget-reminder-left').

(defconst tn--left "15 tool calls left in this execution. Finish, or stop and report what is done and what is not.")

(ert-deftest tool-nudge/fifteen-before-the-limit-the-calls-left-are-named ()
  (let ((superchat-agent-max-tool-calls 20))
    (tn--run (append (make-list 5 tn--read) '((:text "TN-DONE")))
      ;; Five calls of twenty: fifteen are left, the sixth request says so.
      (should (equal 5 (tn--first-with tn--left))))))

(ert-deftest tool-nudge/the-calls-left-are-named-once ()
  (let ((superchat-agent-max-tool-calls 20))
    (tn--run (append (make-list 9 tn--read) '((:text "TN-DONE")))
      (should (= 1 (tn--count "tool calls left in this execution"
                             (car (last (tn--prompts)))))))))

(ert-deftest tool-nudge/more-than-fifteen-left-nothing-is-said ()
  "Guard."
  (let ((superchat-agent-max-tool-calls 20))
    (tn--run (append (make-list 4 tn--read) '((:text "TN-DONE")))
      (should-not (tn--first-with "tool calls left")))))

;;;; 11a: the limit is 120 ------------------------------------------------------

(ert-deftest tool-nudge/the-limit-is-120 ()
  (should (= 120 (eval (car (get 'superchat-agent-max-tool-calls 'standard-value)) t))))

(ert-deftest tool-nudge/with-120-the-reminder-is-at-105 ()
  (let ((superchat-runtime--runs (make-hash-table :test #'equal)))
    (puthash "run-tn" (list :run-id "run-tn" :status 'running-model
                            :tool-calls '(:used 104 :limit 120))
             superchat-runtime--runs)
    (should-not (superchat-runtime--tool-nudges "run-tn"))
    (puthash "run-tn" (list :run-id "run-tn" :status 'running-model
                            :tool-calls '(:used 105 :limit 120))
             superchat-runtime--runs)
    (should (equal (list tn--left) (superchat-runtime--tool-nudges "run-tn")))))

(ert-deftest tool-nudge/the-reminder-can-be-turned-off ()
  (should (boundp 'superchat-tool-budget-reminder-left))
  (let ((superchat-tool-budget-reminder-left nil)
        (superchat-agent-max-tool-calls 20))
    (tn--run (append (make-list 6 tn--read) '((:text "TN-DONE")))
      (should-not (tn--first-with "tool calls left")))))

(ert-deftest tool-nudge/the-runtime-counts-the-calls ()
  (let ((superchat-agent-max-tool-calls 10)
        seen)
    (cl-letf* ((update (symbol-function 'superchat-runtime--update))
               ((symbol-function 'superchat-runtime--update)
                (lambda (run-id &rest pairs)
                  (when (plist-member pairs :tool-calls)
                    (push (plist-get (plist-get pairs :tool-calls) :used) seen))
                  (apply update run-id pairs))))
      (ef--fixture (append (make-list 3 tn--read) '((:text "TN-DONE")))
        (let* ((view (smoke--open-chat))
               (run (ef--run view "go")))
          (should (equal '(1 2 3) (reverse seen)))
          (should (equal '(:used 3 :limit 10)
                         (plist-get (superchat-runtime-status run) :tool-calls))))))))


(defun tn--async-calls (n limit)
  "Call an async tool wrapper N times under LIMIT; return what Runtime was told.
The executor is replaced: only the wrapper's own counting is looked at.
Returns (CUT-OFFS . COUNTS), each a list of argument lists, oldest first."
  (let (cut-offs counts)
    (cl-letf (((symbol-function 'superchat-runtime-note-tool-cut-off)
               (lambda (&rest args) (push args cut-offs)))
              ((symbol-function 'superchat-runtime-note-tool-call)
               (lambda (&rest args) (push args counts)))
              ((symbol-function 'superchat-tool-execute) #'ignore))
      (let* ((superchat-agent-max-tool-calls limit)
             (wrapped (superchat--agent-wrap-function
                       "async_probe" (lambda (callback) (funcall callback "r")) t
                       nil (cons 0 nil) nil
                       (list :run-id "run-async" :session-id ""))))
        (dotimes (_ n) (funcall wrapped #'ignore))))
    (cons (nreverse cut-offs) (nreverse counts))))

(ert-deftest tool-nudge/an-async-tool-is-counted-too ()
  "The asynchronous branch of the wrapper counts each call, up to the limit."
  (should (equal '(("run-async" 1 2) ("run-async" 2 2) ("run-async" 2 2))
                 (cdr (tn--async-calls 3 2)))))

(ert-deftest tool-nudge/the-status-line-shows-the-count ()
  (should (string-match-p
           "tools 37/50"
           (superchat--thinking-status-message
            '(:model-phase waiting-first-token :tool-calls (:used 37 :limit 50))
            nil)))
  ;; Guard: before the first call there is no count.
  (should-not (string-match-p
               "tools"
               (superchat--thinking-status-message
                '(:model-phase waiting-first-token) nil))))

(provide 'tool-nudge-regression)
;;; tool-nudge-regression.el ends here
