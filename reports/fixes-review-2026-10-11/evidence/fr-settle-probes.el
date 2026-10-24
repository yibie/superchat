;;; fr-settle-probes.el --- what superchat-work-settle-missing costs and writes -*- lexical-binding: t; -*-
;; Review of 4bea4054 (R3-02).  Observations and measurements; "guard-"
;; tests assert the commit's promise.  Scripted provider, isolated DB.
(load (expand-file-name "scripts/work-outcome-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t)
    (advice-remove 'load skip-loaded)))

(defun fr-settle--append-ended-run (session id i with-outcome)
  "Append to SESSION a Run that started and completed under record ID.
With WITH-OUTCOME, also the `work.outcome' the observer would have written."
  (let ((run (format "fr-run-%05d" i)) (turn (format "fr-turn-%05d" i)))
    (superchat-tape-append-event
     (list :type "run.started" :session-id session :run-id run :turn-id turn
           :actor-id "runtime" :summary "run started" :payload (list :capability "chat")))
    (superchat-tape-append-event
     (list :type "run.completed" :session-id session :run-id run :turn-id turn
           :actor-id "runtime" :summary "run completed" :payload nil))
    (when with-outcome
      (superchat-work--append session "work.outcome"
                              (format "Work record %s: continuable" id)
                              (list :work-id id :run-id run :outcome "continuable")))
    run))

(defmacro fr-settle--counting-selects (var &rest body)
  "Run BODY counting the Tape SELECTs it makes in VAR."
  (declare (indent 1) (debug t))
  `(let ((select (symbol-function 'superchat-db-tape-select)))
     (cl-letf (((symbol-function 'superchat-db-tape-select)
                (lambda (&rest args) (cl-incf ,var) (apply select args))))
       ,@body)))

(ert-deftest fr-settle/run-end-cost-grows-with-the-runs-ended-under-the-record ()
  "superchat-work--on-turn-completed (superchat-work.el:1133) calls
superchat-work-settle-missing for the conversation first, and that calls
superchat-work--facts-in-run (one full `work.%' scan of the Tape) for every
Run that ever ended under the current record.  Measured here: the scan at a
Run's end, and a whole scripted turn, as those Runs grow."
  (wrec--with '((:text "A") (:text "B") (:text "C") (:text "D")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (id (superchat-work-start session "FR-COST"))
           (have 0))
      (princ "FR-SETTLE COST (live tree; isolated SQLite; one sample each)\nruns ended under the record | settle-missing(session) ms | Tape SELECTs in it | hm--turn wall ms | Tape SELECTs in the turn | work rows on the Tape\n")
      (dolist (target '(0 50 200 1000))
        (dotimes (i (- target have))
          (fr-settle--append-ended-run session id (+ have i) t))
        (setq have target)
        (let* ((settle-selects 0) (turn-selects 0)
               (settle-ms (fr-settle--counting-selects settle-selects
                            (* 1000 (car (benchmark-run 1 (superchat-work-settle-missing session))))))
               (t0 (float-time)))
          (fr-settle--counting-selects turn-selects
            (hm--turn view (format "turn at %d" target)))
          (princ (format "%d | %.0f | %d | %.0f | %d | %d\n"
                         target settle-ms settle-selects
                         (* 1000 (- (float-time) t0)) turn-selects
                         (length (superchat-work--rows)))))))))

(ert-deftest fr-settle/mount-writes-every-outcome-the-focused-record-lacks ()
  "Runs that ended under a focus with no outcome -- as Runs ended while the
work-records row was unmounted leave them -- are all written at the next
mount, each as an execution of the record."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "FR-BACKFILL"))
           (render-before (length (superchat-work-render (superchat-work-get id)))))
      (dotimes (i 300) (fr-settle--append-ended-run session id i nil))
      (let* ((t0 (float-time))
             (written (superchat-work-settle-missing))
             (ms (* 1000 (- (float-time) t0)))
             (record (superchat-work-get id)))
        (princ (format "FR-SETTLE BACKFILL: 300 ended Runs under the focus without an outcome => settle-missing wrote %d outcomes in %.0f ms; the record now has %d outcomes and %d execution lines for the user; the record text a request carries went from %d to %d characters\n"
                       written ms (length (plist-get record :outcomes))
                       (length (split-string (or (superchat-work-executions-text record) "") "\n"))
                       render-before (length (superchat-work-render record))))
        (should (= 300 written))
        (should (= 0 (superchat-work-settle-missing)))))))

(ert-deftest fr-settle/guard-a-woken-run-ended-untold-is-settled-once-and-reported-once ()
  "arm-all runs settle-missing before --report-cut-off: one outcome, one report."
  (wrec--with '((:text "WOKEN-OK")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "FR-UNTOLD")) (main (car pair)) (id (cdr pair))
               woken)
          (wout--as-if-emacs-quit
            (setq woken (car (superchat-work-wake-due)))
            (wrec--settled))
          (let ((ws (hm--session woken)))
            (should (= 1 (length (hm--events ws "run.completed"))))
            (should-not (plist-get (superchat-work-get id) :outcomes))
            (should-not (wrec--facts main "work.reported"))
            ;; A restart: this Emacs knows no Run.
            (clrhash superchat-runtime--runs)
            (superchat-work-arm-all)
            (wrec--settled)
            (princ (format "FR-SETTLE GUARD: outcomes=%S reported=%S\n"
                           (plist-get (superchat-work-get id) :outcomes)
                           (wrec--facts main "work.reported")))
            (should (equal '("continuable") (plist-get (superchat-work-get id) :outcomes)))
            (should (= 1 (length (wrec--facts main "work.reported"))))))))))

(ert-deftest fr-settle/a-record-the-conversation-moved-on-from-stays-without-outcome-and-without-fault ()
  "By the decision; recorded here as a gap that is silent."
  (wrec--with '((:text "DONE")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (old (superchat-work-start session "FR-OLD")) said)
      (wout--as-if-emacs-quit (hm--turn view "Do this step"))
      (superchat-work-start session "FR-NEW")
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string (push (apply #'format format-string args) said)) nil)))
        (should (= 0 (superchat-work-settle-missing)))
        (unwind-protect (superchat-work-arm-all) (superchat-work-disarm-all)))
      (princ (format "FR-SETTLE MOVED-ON: old record outcomes=%S faults=%S said=%S\n"
                     (plist-get (superchat-work-get old) :outcomes) superchat-event-faults
                     (cl-remove-if-not (lambda (s) (string-match-p "superchat" s)) said)))
      (should-not (plist-get (superchat-work-get old) :outcomes))
      (should-not superchat-event-faults))))

(ert-deftest fr-settle/note-the-main-conversations-later-turns-are-written-to-the-woken-record ()
  "Not new: the main conversation's focus stays on the record after the wake,
so its later Runs are its executions too."
  (wrec--with '((:text "WOKEN-OK") (:text "MAIN-LATER")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "FR-MAIN-LATER"))
               (main (car pair)) (id (cdr pair))
               (view (superchat-timer--live-view main)))
          (should (buffer-live-p view))
          (superchat-work-wake-due)
          (wrec--settled)
          (hm--turn view "an unrelated question in the main conversation")
          (princ (format "FR-SETTLE NOTE: executions of the record after a wake and one main turn: %S\n"
                         (mapcar (lambda (e) (list (plist-get e :session) (plist-get e :outcome)))
                                 (plist-get (superchat-work-get id) :executions))))
          (should (= 2 (length (plist-get (superchat-work-get id) :executions)))))))))

(ert-run-tests-batch-and-exit "^fr-settle/")
;;; fr-settle-probes.el ends here
