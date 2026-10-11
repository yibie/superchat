;;; work-outcome-regression.el --- what an execution left reaches its record -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-11-what-an-execution-left-reaches-its-record.md
;; (review round 3, R3-02).  An execution ending and its Work Record being
;; told how it stands are two writes.  When the second fails it is said and
;; recorded as a fault; and the outcome a record lacks for the last execution
;; that ended under it is written later, once, from the Tape alone, without
;; starting anything again.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/work-outcome-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^work-outcome/")'

;;; Code:

(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-work)
(require 'superchat-event)

(defun wout--outcomes (id)
  "The outcomes record ID has, oldest first."
  (plist-get (superchat-work-get id) :outcomes))

(defun wout--outcome-rows (session id)
  "The `work.outcome' facts SESSION's Tape holds for record ID, as (RUN . OUTCOME)."
  (delq nil
        (mapcar (lambda (event)
                  (let ((payload (plist-get event :payload)))
                    (and (equal id (or (plist-get payload :work-id)
                                       (plist-get payload :work_id)))
                         (cons (or (plist-get payload :run-id)
                                   (plist-get payload :run_id))
                               (plist-get payload :outcome)))))
                (hm--events session "work.outcome"))))

(defmacro wout--outcome-write-failing (hits &rest body)
  "Run BODY while writing a `work.outcome' fact fails, counting in HITS."
  (declare (indent 1) (debug t))
  `(let ((append-event (symbol-function 'superchat-tape-append-event)))
     (cl-letf (((symbol-function 'superchat-tape-append-event)
                (lambda (event &rest args)
                  (if (equal "work.outcome" (plist-get event :type))
                      (progn (cl-incf ,hits)
                             (error "WOUT injected: outcome storage unavailable"))
                    (apply append-event event args)))))
       ,@body)))

(defmacro wout--as-if-emacs-quit (&rest body)
  "Run BODY with the end-of-execution observer doing nothing.
That is what the Tape holds when Emacs quits between a Run's end and the
record being told."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'superchat-work--on-turn-completed) #'ignore))
     ,@body))

;;;; The failure is said --------------------------------------------------------

(ert-deftest work-outcome/a-write-that-fails-is-recorded-and-said ()
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (hits 0) (said nil) run)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (push (apply #'format format-string args) said)))))
        (wout--outcome-write-failing hits
          (setq run (hm--turn view "Do this step"))))
      (should (= hits 1))
      (should-not (wout--outcomes id))
      ;; The Run itself ended as it ended.
      (should (= 1 (length (hm--events session "run.completed"))))
      (let ((fault (cl-find 'work-outcome superchat-event-faults
                            :key (lambda (f) (plist-get f :site)))))
        (should fault)
        (should (equal id (plist-get fault :id))))
      (should (cl-some (lambda (line)
                         (and (string-match-p (regexp-quote id) line)
                              (string-match-p (regexp-quote run) line)
                              (string-match-p "no outcome" line)))
                       said)))))

;;;; And written later, once ----------------------------------------------------

(ert-deftest work-outcome/the-next-scan-writes-what-is-missing-once ()
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (hits 0) run)
      (wout--outcome-write-failing hits
        (setq run (hm--turn view "Do this step")))
      (should-not (wout--outcomes id))
      (should (= 1 (superchat-work-settle-missing)))
      (should (equal (list (cons run "continuable")) (wout--outcome-rows session id)))
      ;; Asked again, and through the scan a mounting row makes: nothing more.
      (should (= 0 (superchat-work-settle-missing)))
      (unwind-protect (superchat-work-arm-all) (superchat-work-disarm-all))
      (should (equal '("continuable") (wout--outcomes id)))
      ;; Nothing was started again.
      (should (= 1 (length (hm--events session "run.started")))))))

(ert-deftest work-outcome/the-next-execution-of-the-conversation-writes-it ()
  "No restart is needed: the conversation's next Run end repairs the last one."
  (wrec--with '((:text "FIRST") (:text "SECOND")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (hits 0) first second)
      (wout--outcome-write-failing hits
        (setq first (hm--turn view "Do the first step")))
      (should-not (wout--outcomes id))
      (setq second (hm--turn view "Do the second step"))
      (let ((rows (wout--outcome-rows session id)))
        (should (= 2 (length rows)))
        (should (equal "continuable" (cdr (assoc first rows))))
        (should (equal "continuable" (cdr (assoc second rows))))))))

(ert-deftest work-outcome/an-emacs-that-quit-in-between-is-caught-up ()
  "The Run ended on the Tape and the record was never told."
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           run)
      (wout--as-if-emacs-quit
        (setq run (hm--turn view "Do this step")))
      (should-not (wout--outcomes id))
      (should-not superchat-event-faults)
      (unwind-protect (superchat-work-arm-all) (superchat-work-disarm-all))
      (should (equal (list (cons run "continuable")) (wout--outcome-rows session id))))))

(ert-deftest work-outcome/a-failed-execution-is-caught-up-as-failed ()
  (wrec--with '((:error (error . "WOUT_PROVIDER_DOWN"))) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           run)
      (wout--as-if-emacs-quit
        (setq run (smoke--send view "Do this step"))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 (superchat-work-settle-missing)))
      (should (equal (list (cons run "failed")) (wout--outcome-rows session id))))))

;;;; Guards ---------------------------------------------------------------------

(ert-deftest work-outcome/an-execution-that-was-settled-leaves-nothing-to-write ()
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (run (hm--turn view "Do this step")))
      (should (equal (list (cons run "continuable")) (wout--outcome-rows session id)))
      (should (= 0 (superchat-work-settle-missing)))
      (should (= 0 (superchat-work-settle-missing session)))
      (should (equal '("continuable") (wout--outcomes id)))
      (should-not superchat-event-faults))))

(ert-deftest work-outcome/an-execution-belongs-to-the-record-it-ended-under ()
  "A record the conversation moved on from is not given a later scan's guess."
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (old (superchat-work-start session "WOUT-OLD-GOAL"))
           new)
      (wout--as-if-emacs-quit
        (hm--turn view "Do this step"))
      (setq new (superchat-work-start session "WOUT-NEW-GOAL"))
      (should (equal new (superchat-work-current session)))
      ;; No Run ended since the conversation turned to the new record.
      (should (= 0 (superchat-work-settle-missing)))
      (should-not (wout--outcomes new))
      (should-not (wout--outcomes old)))))

;; Two installations of the row each observe the end of a Run.
(ert-deftest work-outcome/an-end-observed-twice-is-written-once ()
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (run (hm--turn view "Do this step"))
           (status (append (list :session-id session :run-id run)
                           (superchat-runtime-status run))))
      (superchat-work--on-turn-completed nil status)
      (superchat-work--on-turn-completed nil status)
      (should (equal (list (cons run "continuable")) (wout--outcome-rows session id))))))

;; Review round 3, R3-01 again: a focus on a record out of reach is no claim.
(ert-deftest work-outcome/a-record-out-of-reach-is-given-nothing ()
  (wrec--with '((:text "DONE")) (p q)
    (let* ((sa (hm--session (wrec--view p)))
           (id (superchat-work-start sa "WOUT-PRIVATE-GOAL"))
           (b (wrec--view q)) (sb (hm--session b)))
      (superchat-work--append sb "work.focused" "legacy focus" (list :work-id id))
      (wout--as-if-emacs-quit
        (hm--turn b "An unrelated question"))
      (should (= 0 (superchat-work-settle-missing)))
      (should (= 0 (superchat-work-settle-missing sb)))
      (should-not (wout--outcomes id)))))

(ert-deftest work-outcome/a-report-that-fails-is-recorded ()
  "The note to the main conversation is not swallowed either."
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL")))
      (cl-letf (((symbol-function 'superchat-work--report-woken)
                 (lambda (&rest _) (error "WOUT injected: report unavailable")))
                ((symbol-function 'message) #'ignore))
        (hm--turn view "Do this step"))
      (should (cl-find 'work-report superchat-event-faults
                       :key (lambda (f) (plist-get f :site))))
      ;; The outcome itself was written.
      (should (equal '("continuable") (wout--outcomes id))))))

;;;; The scan is not paid at every end (FR-01) ---------------------------------

(defmacro wout--counting-selects (var &rest body)
  "Run BODY counting the Tape SELECTs it makes in VAR."
  (declare (indent 1) (debug t))
  `(let ((select (symbol-function 'superchat-db-tape-select)))
     (cl-letf (((symbol-function 'superchat-db-tape-select)
                (lambda (&rest args) (cl-incf ,var) (apply select args))))
       ,@body)))

(ert-deftest work-outcome/an-end-with-nothing-owed-does-not-scan ()
  "An execution that ends when no write failed here looks for nothing missing."
  (wrec--with '((:text "FIRST") (:text "SECOND")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (scans 0)
           (scan (symbol-function 'superchat-work-settle-missing)))
      (cl-letf (((symbol-function 'superchat-work-settle-missing)
                 (lambda (&rest args) (cl-incf scans) (apply scan args))))
        (hm--turn view "Do the first step")
        (hm--turn view "Do the second step"))
      (should (= 0 scans))
      (should (equal '("continuable" "continuable") (wout--outcomes id))))))

(ert-deftest work-outcome/a-failed-write-is-owed-until-the-next-end ()
  "The conversation is remembered as owed, and the debt is paid once."
  (wrec--with '((:text "FIRST") (:text "SECOND") (:text "THIRD")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "WOUT-GOAL"))
           (hits 0) (scans 0)
           (scan (symbol-function 'superchat-work-settle-missing)))
      (wout--outcome-write-failing hits
        (hm--turn view "Do the first step"))
      (should (gethash session superchat-work--owed))
      (cl-letf (((symbol-function 'superchat-work-settle-missing)
                 (lambda (&rest args) (cl-incf scans) (apply scan args))))
        (hm--turn view "Do the second step")
        (should (= 1 scans))
        (should-not (gethash session superchat-work--owed))
        (hm--turn view "Do the third step")
        (should (= 1 scans)))
      (should (= 3 (length (wout--outcome-rows session id)))))))

(ert-deftest work-outcome/the-scan-reads-the-facts-once-however-many-runs-ended ()
  "Looking for what is missing costs the same reads after two Runs as after six."
  (wrec--with '((:text "A") (:text "B") (:text "C") (:text "D") (:text "E") (:text "F")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (two 0) (six 0))
      (superchat-work-start session "WOUT-GOAL")
      (hm--turn view "one") (hm--turn view "two")
      (wout--counting-selects two (should (= 0 (superchat-work-settle-missing session))))
      (dotimes (_ 4) (hm--turn view "more"))
      (wout--counting-selects six (should (= 0 (superchat-work-settle-missing session))))
      (should (= two six)))))

(provide 'work-outcome-regression)
;;; work-outcome-regression.el ends here
