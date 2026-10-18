;;; tool-limit-regression.el --- an execution cut off by the tool-call limit says so -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 9b (reports/guidance-2026-10-11/SLICE-9.md).  The agent loop refuses
;; every call past `superchat-agent-max-tool-calls' and tells the model, but
;; nothing else knew: in the user's two Runs of 2026-10-18 `run.completed'
;; said only status completed.  The cut-off is now on the Runtime entry as
;; `:tool-budget' (:action cut-off :limit N :used N), so the ending facts
;; carry it, and the chat, the pending list and the notices list say "Cut
;; off after N tool calls" instead of an ordinary ending.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/tool-limit-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^tool-limit/")'

;;; Code:

(load (expand-file-name "seen-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun tl--reads (n)
  "A script of N rounds, each one read_buffer call, then a text answer."
  (append (make-list n '(:tool-uses (("read_buffer" (buffer . "hm-target")))))
          (list '(:text "TL-DONE"))))

(defun tl--ending (session run)
  "The payload of RUN's `run.completed' fact in SESSION."
  (plist-get (cl-find-if (lambda (event)
                           (and (equal "run.completed" (plist-get event :type))
                                (equal run (plist-get event :run-id))))
                         (superchat-tape-replay-events session))
             :payload))

(ert-deftest tool-limit/the-cut-off-is-in-the-ending-facts ()
  (pend--fresh
    (hm--fixture (tl--reads 4)
      (let* ((superchat-agent-max-tool-calls 3)
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (smoke--send view "read it")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (let ((budget (plist-get (superchat-runtime-status run) :tool-budget)))
          (should (eq 'cut-off (plist-get budget :action)))
          (should (equal 3 (plist-get budget :limit)))
          (should (equal 3 (plist-get budget :used))))
        (let ((budget (plist-get (tl--ending session run) :tool-budget)))
          (should (member (plist-get budget :action) '(cut-off "cut-off")))
          (should (equal 3 (plist-get budget :limit))))
        (should (string-match-p
                 "Cut off after 3 tool calls"
                 (with-current-buffer view (buffer-string))))))))

(ert-deftest tool-limit/under-the-limit-nothing-is-said ()
  "Guard: an execution that stays within the limit carries no cut-off."
  (pend--fresh
    (hm--fixture (tl--reads 3)
      (let* ((superchat-agent-max-tool-calls 3)
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (smoke--send view "read it")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (should-not (plist-get (superchat-runtime-status run) :tool-budget))
        (should-not (plist-get (tl--ending session run) :tool-budget))
        (should-not (string-match-p "Cut off"
                                    (with-current-buffer view (buffer-string))))))))

(ert-deftest tool-limit/the-pending-list-says-cut-off ()
  (pend--fresh
    (hm--fixture (tl--reads 4)
      (let* ((superchat-agent-max-tool-calls 3)
             (view (smoke--open-chat))
             (run (let ((superchat-automated-input 'timers))
                    (smoke--send view "a task"))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (superchat-changes-notices-reload)
        (let ((item (car (pend--of 'unseen))))
          (should (equal run (plist-get item :key)))
          (should (string-match-p "\\`Cut off after 3 tool calls" (plist-get item :detail)))
          (should-not (string-match-p "completed" (plist-get item :detail))))
        (let ((row (cadr (car (superchat-changes--notices-entries)))))
          (should (equal "Cut off after 3 tool calls" (aref row 2))))))))

(ert-deftest tool-limit/my-execution-cut-off-with-its-conversation-closed-says-so ()
  "The fact `run.ended-unwatched' carries the cut-off, which is all the list
has of an execution the user started."
  (pend--fresh
    (wrec--with (cons '(:tool-uses (("read_buffer" (buffer . "hm-target"))) :delay 0.4)
                      (tl--reads 3))
        (p)
      (let* ((superchat-agent-max-tool-calls 3)
             (view (wrec--view p))
             (session (hm--session view))
             (run (seen--closed-before-the-end view "read it")))
        (let ((fact (car (seen--facts session "run.ended-unwatched"))))
          (should (equal 3 (plist-get (plist-get (plist-get fact :payload) :tool-budget)
                                      :limit))))
        (let ((item (car (pend--of 'unseen))))
          (should (equal run (plist-get item :key)))
          (should (string-match-p "\\`Cut off after 3 tool calls"
                                  (plist-get item :detail))))))))

(ert-deftest tool-limit/an-ordinary-ending-still-reads-completed ()
  "Guard: the pending row of an execution within the limit is as before."
  (pend--fresh
    (hm--fixture (tl--reads 1)
      (let* ((superchat-agent-max-tool-calls 3)
             (view (smoke--open-chat))
             (run (let ((superchat-automated-input 'timers))
                    (smoke--send view "a task"))))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (superchat-changes-notices-reload)
        (should (string-match-p "\\`completed"
                                (plist-get (car (pend--of 'unseen)) :detail)))))))


(defun tl--async-calls (n limit)
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

(ert-deftest tool-limit/an-async-tool-is-cut-off-too ()
  "The asynchronous branch of the wrapper (sub-agent delegation, ask_user)."
  (should (equal '(("run-async" 2 2)) (car (tl--async-calls 3 2)))))

(ert-deftest tool-limit/a-round-limit-is-not-overwritten ()
  "Guard: the first budget decision of an execution is the one kept."
  (should (functionp 'superchat-runtime-note-tool-cut-off))
  (let ((superchat-runtime--runs (make-hash-table :test #'equal)))
    (puthash "run-tl" (list :run-id "run-tl" :status 'running-model
                            :tool-budget '(:rounds 5 :limit 5 :action finalize))
             superchat-runtime--runs)
    (superchat-runtime-note-tool-cut-off "run-tl" 3 3)
    (should (eq 'finalize (plist-get (plist-get (gethash "run-tl" superchat-runtime--runs)
                                                :tool-budget)
                                     :action)))))

(provide 'tool-limit-regression)
;;; tool-limit-regression.el ends here
