;;; tool-hooks-regression.el --- two hard hooks in the tool loop -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 11b (reports/guidance-2026-10-11/SLICE-11.md).  Each function in
;; `superchat-tool-loop-hooks' is asked twice about every tool call: before
;; its body runs (phase `before') and after its result is known (phase
;; `after', with :result and :failed).  It answers nil (let it be), a string
;; (the result the model is given instead), or (:stop REASON) (end this
;; execution).  A rewrite keeps the original in `tool.completed' and records
;; `tool.rewritten'; a stop ends the Run with a visible ending, the way a
;; cut-off does (9b): the reason on the ending facts, under the answer and on
;; the pending row.  A hook that signals is a fault and counts as nil.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/tool-hooks-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^tool-hooks/")'

;;; Code:

(load (expand-file-name "tool-limit-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-tool-loop-hooks)

(defconst th--read '(:tool-uses (("read_buffer" (buffer . "hm-target")))))
(defconst th--edit '(:tool-uses (("edit_buffer" (buffer_name . "hm-target")
                                  (old_string . "HM-TARGET") (new_string . "TH-EDITED")))))

(defun th--facts (session type)
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events session)))

(defun th--payloads (session type)
  (mapcar (lambda (event) (plist-get event :payload)) (th--facts session type)))

(defun th--prompts ()
  (mapcar (lambda (request) (plist-get request :prompt-text)) (reverse smoke--requests)))

(defmacro th--run (script hooks &rest body)
  "Send one message with SCRIPT as the model and HOOKS as the tool-loop hooks.
BODY sees VIEW, SESSION and RUN."
  (declare (indent 2) (debug t))
  `(pend--fresh
     (hm--fixture ,script
       (let* ((superchat-tool-loop-hooks ,hooks)
              (view (smoke--open-chat))
              (session (buffer-local-value 'superchat--session-id view))
              (run (smoke--send view "go")))
         (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
         ,@body))))

(ert-deftest tool-hooks/a-hook-that-lets-it-be-changes-nothing ()
  (let (asked plain)
    (th--run (list th--read '(:text "TH-DONE")) nil
      (setq plain (plist-get (car (th--payloads session "tool.completed")) :result-text)))
    (th--run (list th--read '(:text "TH-DONE"))
        (list (lambda (phase call) (push (list phase (plist-get call :tool-name)) asked) nil))
      (should (equal '((before "read_buffer") (after "read_buffer")) (reverse asked)))
      (should (stringp plain))
      (should (equal plain (plist-get (car (th--payloads session "tool.completed")) :result-text)))
      (should (string-match-p (regexp-quote "HM-TARGET-TEXT") (nth 1 (th--prompts))))
      (should-not (th--facts session "tool.rewritten")))))

(ert-deftest tool-hooks/a-rewrite-after-is-what-the-model-sees-and-both-are-kept ()
  (th--run (list th--read '(:text "TH-DONE"))
      (list (lambda (phase _call) (and (eq phase 'after) "TH-REWRITTEN")))
    (let ((second (nth 1 (th--prompts))))
      (should (string-match-p "TH-REWRITTEN" second))
      (should-not (string-match-p "HM-TARGET-TEXT" second)))
    (should (string-match-p "HM-TARGET-TEXT"
                            (plist-get (car (th--payloads session "tool.completed")) :result-text)))
    (let ((rewrite (car (th--payloads session "tool.rewritten"))))
      (should rewrite)
      (should (equal "after" (plist-get rewrite :phase)))
      (should (equal "TH-REWRITTEN" (plist-get rewrite :result-text)))
      (should (equal (plist-get (car (th--payloads session "tool.completed")) :execution-id)
                     (plist-get rewrite :execution-id))))))

(ert-deftest tool-hooks/a-rewrite-before-means-the-body-does-not-run ()
  (th--run (list th--edit '(:text "TH-DONE"))
      (list (lambda (phase _call) (and (eq phase 'before) "TH-NOT-RUN")))
    (should (equal "HM-TARGET-TEXT\n" (with-current-buffer "hm-target" (buffer-string))))
    (should (string-match-p "TH-NOT-RUN" (nth 1 (th--prompts))))
    (should (equal "before" (plist-get (car (th--payloads session "tool.rewritten")) :phase)))))

(ert-deftest tool-hooks/a-stop-after-ends-the-execution-visibly ()
  (pend--fresh
    (hm--fixture (list th--read th--read '(:text "TH-NEVER"))
      (let* ((superchat-tool-loop-hooks
              (list (lambda (phase _call) (and (eq phase 'after) '(:stop "TH hook says stop")))))
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (let ((superchat-automated-input 'timers)) (smoke--send view "a task"))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        ;; No second call, no further request.
        (should (= 1 (length (th--facts session "tool.requested"))))
        (should (= 1 (length smoke--requests)))
        ;; The ending says why: the facts, under the answer, the pending row.
        (let ((budget (plist-get (superchat-runtime-status run) :tool-budget)))
          (should (equal "TH hook says stop" (plist-get budget :reason))))
        (should (cl-some (lambda (event)
                           (and (equal run (plist-get event :run-id))
                                (equal "TH hook says stop"
                                       (plist-get (plist-get (plist-get event :payload) :tool-budget)
                                                  :reason))))
                         (superchat-tape-replay-events session)))
        ;; The notice under the answer, not only the final text "[...]".
        (should (string-match-p (regexp-quote "TH hook says stop; unfinished or unverified work may remain.")
                                (with-current-buffer view (buffer-string))))
        (superchat-changes-notices-reload)
        (let ((item (car (pend--of 'unseen))))
          (should (equal run (plist-get item :key)))
          (should (string-match-p "\\`TH hook says stop" (plist-get item :detail))))))))

(ert-deftest tool-hooks/after-a-stop-the-rest-of-the-round-is-refused ()
  (th--run (list '(:tool-uses (("read_buffer" (buffer . "hm-target"))
                               ("read_buffer" (buffer . "hm-target"))))
                 '(:text "TH-NEVER"))
      (list (lambda (phase _call) (and (eq phase 'after) '(:stop "TH stop mid-round"))))
    (should (= 1 (length (th--facts session "tool.completed"))))
    (let ((refused (car (th--payloads session "tool.failed"))))
      (should (equal "blocked" (plist-get refused :status)))
      (should (string-match-p "Tool calls stopped: TH stop mid-round"
                              (or (plist-get refused :block-reason) ""))))))

(ert-deftest tool-hooks/an-async-tool-is-refused-after-a-stop-too ()
  "The asynchronous branch of the wrapper (sub-agent delegation, ask_user)."
  (let (blocks)
    (cl-letf (((symbol-function 'superchat-runtime-tools-stopped) (lambda (_run) "TH async stop"))
              ((symbol-function 'superchat-runtime-note-tool-call) #'ignore)
              ((symbol-function 'superchat-tool-execute)
               (lambda (&rest call) (push (plist-get (plist-get (nthcdr 3 call) :context) :block) blocks))))
      (funcall (superchat--agent-wrap-function
                "async_probe" (lambda (callback) (funcall callback "r")) t
                nil (cons 0 nil) nil (list :run-id "run-async" :session-id ""))
               #'ignore))
    (should (equal '("Tool calls stopped: TH async stop") blocks))))

(ert-deftest tool-hooks/a-stop-keeps-an-earlier-budget-decision ()
  (let ((superchat-runtime--runs (make-hash-table :test #'equal)))
    (puthash "run-th" (list :run-id "run-th" :status 'running-model
                            :tool-budget '(:action cut-off :limit 3 :used 3))
             superchat-runtime--runs)
    (superchat-runtime-stop-tools "run-th" "TH late stop")
    (should (eq 'cut-off (plist-get (plist-get (gethash "run-th" superchat-runtime--runs)
                                               :tool-budget)
                                    :action)))
    ;; The rest of the execution is still refused.
    (should (equal "TH late stop" (superchat-runtime-tools-stopped "run-th")))))

(ert-deftest tool-hooks/a-stop-before-means-the-body-does-not-run ()
  (th--run (list th--edit th--edit '(:text "TH-NEVER"))
      (list (lambda (phase _call) (and (eq phase 'before) '(:stop "TH stop before"))))
    (should (equal "HM-TARGET-TEXT\n" (with-current-buffer "hm-target" (buffer-string))))
    (should (= 1 (length (th--facts session "tool.requested"))))
    (should (equal '("stopped") (mapcar (lambda (p) (plist-get p :status))
                                        (th--payloads session "tool.failed"))))
    (should (equal "TH stop before"
                   (plist-get (plist-get (superchat-runtime-status run) :tool-budget) :reason)))))

(ert-deftest tool-hooks/a-hook-that-fails-is-a-fault-and-lets-it-be ()
  (th--run (list th--read '(:text "TH-DONE"))
      (list (lambda (_phase _call) (error "TH hook broke")))
    (should (cl-some (lambda (fault) (eq 'tool-loop-hook (plist-get fault :site)))
                     superchat-event-faults))
    (should (string-match-p "HM-TARGET-TEXT"
                            (plist-get (car (th--payloads session "tool.completed")) :result-text)))
    (should (string-match-p (regexp-quote "HM-TARGET-TEXT") (nth 1 (th--prompts))))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))

(ert-deftest tool-hooks/the-default-is-the-failure-brake-alone ()
  (should (equal '(superchat-tool-failure-brake-hook)
                 (eval (car (get 'superchat-tool-loop-hooks 'standard-value)) t))))

(ert-deftest tool-hooks/the-failure-brake-on-the-pending-list ()
  "11c: three failed calls in a row end the execution, and the list says so."
  (pend--fresh
    (hm--fixture (list '(:tool-uses (("read_buffer" (buffer . "th-no-such-buffer"))))
                       '(:tool-uses (("read_buffer" (buffer . "th-no-such-buffer"))))
                       '(:tool-uses (("read_buffer" (buffer . "th-no-such-buffer"))))
                       '(:tool-uses (("read_buffer" (buffer . "th-no-such-buffer"))))
                       '(:text "TH-NEVER"))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (let ((superchat-automated-input 'timers)) (smoke--send view "a task"))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (should (equal '("failed" "failed" "failed")
                       (mapcar (lambda (p) (plist-get p :status)) (th--payloads session "tool.failed"))))
        (should (= 3 (length (th--facts session "tool.requested"))))
        (should (string-match-p "Stopped after 3 failed calls in a row"
                                (with-current-buffer view (buffer-string))))
        (superchat-changes-notices-reload)
        (should (string-match-p "\\`Stopped after 3 failed calls in a row"
                                (plist-get (car (pend--of 'unseen)) :detail)))))))

(provide 'tool-hooks-regression)
;;; tool-hooks-regression.el ends here
