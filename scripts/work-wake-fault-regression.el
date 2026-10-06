;;; work-wake-fault-regression.el --- A wake that goes wrong -*- lexical-binding: t; -*-

;;; Commentary:
;; Fault injection for waking a Work Record at its time (M11 slice 8.1,
;; `superchat-work-wake').  Each test breaks one step of a wake and asks what
;; the user is left with: a wait that came due is either taken up, or still
;; pending, never silently gone; one wait starts one execution; an execution
;; that was started is reported in the main conversation even when the
;; in-memory table that linked them is gone (an Emacs restart).
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/work-wake-fault-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-wake-fault/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun wwf--due-record (root goal)
  "Start a record GOAL in ROOT's main conversation, waiting for a time now due.
Return (MAIN-SESSION . RECORD-ID)."
  (let* ((main (hm--session (wrec--view root)))
         (id (superchat-work-start main goal)))
    (superchat-work-wait main id "time" nil (+ wrec--now 60))
    (superchat-work-disarm-all)
    (setq wrec--now (+ wrec--now 120))
    (cons main id)))

(defun wwf--wake-prompts (id)
  "How many provider requests so far carry the wake message of record ID."
  (cl-count-if
   (lambda (wire)
     (string-match-p (regexp-quote (format superchat-work-wake-prompt id))
                     (mapconcat #'hm--text wire "\n")))
   hm--wires))

(defmacro wwf--with-broken-send (&rest body)
  "Run BODY while sending the wake message signals an error."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'superchat-send-input)
              (lambda (&rest _) (error "WWF injected: nothing accepted the wake"))))
     ,@body))

(ert-deftest work-wake-fault/a-wake-nothing-accepted-is-taken-up-on-the-next-start ()
  "The wake fact was written but no execution started (send failed, or Emacs
quit in between).  The next start of Emacs must take the record up."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let ((id (cdr (wwf--due-record p "Lost wake"))))
          (wwf--with-broken-send
            (ignore-errors (superchat-work-wake-due)))
          (should (= 0 (wwf--wake-prompts id)))
          ;; Emacs starts again; sending works.
          (superchat-work-arm-all)
          (wrec--settled)
          (should (= 1 (wwf--wake-prompts id))))))))

(ert-deftest work-wake-fault/a-wake-nothing-accepted-is-not-shown-as-settled ()
  "After a wake nothing accepted, the record must not read as an ordinary
continuable record with no wait: the user would never learn its time came."
  (wrec--with nil (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let ((id (cdr (wwf--due-record p "Lost wake shown"))))
          (wwf--with-broken-send
            (ignore-errors (superchat-work-wake-due)))
          ;; It still shows as waiting for its time, which has passed.
          (should (eq 'waiting-other
                      (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest work-wake-fault/a-wake-cut-off-by-emacs-quitting-is-taken-up-on-the-next-start ()
  "Emacs quit after the wake facts were written and before anything was
sent: no error handler ran.  Sending that does nothing leaves the same Tape."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let ((id (cdr (wwf--due-record p "Cut off"))))
          (cl-letf (((symbol-function 'superchat-send-input) #'ignore))
            (superchat-work-wake-due))
          (should (= 0 (wwf--wake-prompts id)))
          (superchat-work-arm-all)
          (wrec--settled)
          (should (= 1 (wwf--wake-prompts id)))
          ;; Taken up once: a further start begins nothing more.
          (superchat-work-arm-all)
          (wrec--settled)
          (should (= 1 (wwf--wake-prompts id))))))))

(ert-deftest work-wake-fault/the-woken-execution-is-not-told-the-record-still-waits ()
  "The request of the woken execution shows the record without its old wait."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (wwf--due-record p "No stale wait")
        (superchat-work-wake-due)
        (wrec--settled)
        (should-not (string-match-p "(waiting for "
                                    (mapconcat #'hm--text (hm--wire 1) "\n")))))))

(ert-deftest work-wake-fault/one-failed-wake-does-not-stop-the-others ()
  "Two records are due.  Sending fails for the first one only; the second
must still be taken up in the same scan."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((main (hm--session (wrec--view p)))
               (a (superchat-work-start main "First due"))
               (b (superchat-work-start main "Second due"))
               (send (symbol-function 'superchat-send-input))
               (calls 0))
          (superchat-work-wait main a "time" nil (+ wrec--now 60))
          (superchat-work-wait main b "time" nil (+ wrec--now 60))
          (superchat-work-disarm-all)
          (setq wrec--now (+ wrec--now 120))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (if (= calls 1)
                           (error "WWF injected: first wake fails")
                         (apply send args)))))
            (ignore-errors (superchat-work-wake-due))
            (wrec--settled))
          (should (= 2 calls))
          (should (= 1 (+ (wwf--wake-prompts a) (wwf--wake-prompts b)))))))))

(ert-deftest work-wake-fault/a-scan-during-the-execution-starts-nothing-more ()
  "The same wait is scanned again while its execution runs, and after."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let ((id (cdr (wwf--due-record p "Scanned twice"))))
          (should (= 1 (length (superchat-work-wake-due))))
          (should-not (superchat-work-wake-due))
          (superchat-work-arm-all)
          (wrec--settled)
          (should-not (superchat-work-wake-due))
          (should (= 1 (wwf--wake-prompts id)))
          (should (= 1 (length (plist-get (superchat-work-get id) :woken)))))))))

(ert-deftest work-wake-fault/the-report-survives-losing-the-in-memory-link ()
  "The execution was accepted, then the table linking it to its record was
lost (Emacs restarted while it ran).  The main conversation is still told."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Link lost"))
               (main (car pair)))
          (should (= 1 (length (superchat-work-wake-due))))
          ;; Whatever links the execution to its record in memory is lost.
          (when (boundp 'superchat-work--woken-runs)
            (clrhash (symbol-value 'superchat-work--woken-runs)))
          (wrec--settled)
          (should (= 1 (length (wrec--facts main "work.reported")))))))))

(ert-deftest work-wake-fault/a-failed-execution-is-recorded-as-failed ()
  "The execution starts and the provider fails: the record says so, and the
main conversation is told; nothing is started again by itself."
  (wrec--with '((:error (error . "WWF provider down"))) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Provider down"))
               (main (car pair))
               (id (cdr pair)))
          (should (= 1 (length (superchat-work-wake-due))))
          (wrec--settled)
          (should (equal "failed" (car (last (wrec--outcomes id)))))
          (should (= 1 (length (wrec--facts main "work.reported"))))
          (should-not (superchat-work-wake-due)))))))

(ert-deftest work-wake-fault/the-main-conversation-is-told-once ()
  "A later turn in the woken conversation does not report the wake again."
  (wrec--with '((:text "ok") (:text "ok again")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Told once"))
               (main (car pair))
               (view (car (superchat-work-wake-due))))
          (wrec--settled)
          (should (= 1 (length (wrec--facts main "work.reported"))))
          (hm--turn view "and one more thing")
          (should (= 1 (length (wrec--facts main "work.reported")))))))))

(ert-deftest work-wake-fault/a-cut-off-wake-is-left-alone-once-the-record-moved-on ()
  "After a wake was cut off, the record was written to from elsewhere.  The
product does not put the old wait back over that."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Moved on"))
               (main (car pair))
               (id (cdr pair)))
          (cl-letf (((symbol-function 'superchat-send-input) #'ignore))
            (superchat-work-wake-due))
          (superchat-work-update main id "done by hand" nil nil)
          (superchat-work-arm-all)
          (wrec--settled)
          (should (= 0 (wwf--wake-prompts id)))
          (should-not (plist-get (superchat-work-get id) :waiting)))))))

;;;; Without a restart; an execution cut off mid-way ----------------------------------

(ert-deftest work-wake-fault/a-failed-wake-is-tried-again-without-a-restart ()
  "Sending fails once.  Emacs keeps running; the wake is tried again by itself."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-seconds 0))
      (wrec--with-clock
        (let* ((id (cdr (wwf--due-record p "Tried again")))
               (send (symbol-function 'superchat-send-input))
               (calls 0))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (if (= calls 1)
                           (error "WWF injected: first send fails")
                         (apply send args)))))
            (should-not (superchat-work-wake-due))
            (should (smoke--wait (lambda () (= 1 (wwf--wake-prompts id))) 10))
            (wrec--settled))
          (should (= 2 calls))
          (should-not (plist-get (superchat-work-get id) :waiting)))))))

(ert-deftest work-wake-fault/a-wake-that-keeps-failing-stops-being-retried ()
  "Sending always fails: one try and `superchat-work-wake-retry-limit' more."
  (wrec--with nil (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-seconds 0)
          (superchat-work-wake-retry-limit 2)
          (calls 0))
      (wrec--with-clock
        (let ((id (cdr (wwf--due-record p "Keeps failing"))))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _)
                       (setq calls (1+ calls))
                       (error "WWF injected: send always fails"))))
            (superchat-work-wake-due)
            (should (smoke--wait (lambda () (= 3 calls)) 10))
            ;; Nothing more comes, however long Emacs keeps running.
            (should-not (smoke--wait (lambda () (> calls 3)) 1)))
          (should (= 0 (hash-table-count superchat-work--armed)))
          ;; Still shown as waiting for its time.
          (should (eq 'waiting-other
                      (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest work-wake-fault/an-execution-cut-off-by-emacs-quitting-is-reported-as-interrupted ()
  "The woken execution started and Emacs quit before it ended: its Run is on
the Tape and this Emacs does not know it.  The main conversation is told, the
record says its last execution was cut off, and nothing is started again."
  (wrec--with nil (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Cut off mid-way"))
               (main (car pair))
               (id (cdr pair)))
          ;; Sending leaves a started Run on the Tape and nothing else.
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _)
                       (superchat-tape-append-event
                        (list :type "run.started" :session-id superchat--session-id
                              :run-id "run-wwf-cut-off" :turn-id "turn-wwf-cut-off"
                              :actor-id "runtime"
                              :summary "run.started: run-wwf-cut-off")))))
            (should (= 1 (length (superchat-work-wake-due)))))
          (should (= 0 (length (wrec--facts main "work.reported"))))
          ;; Emacs starts again.
          (superchat-work-arm-all)
          (let ((reported (wrec--facts main "work.reported")))
            (should (= 1 (length reported)))
            (should (equal "run-wwf-cut-off" (plist-get (car reported) :run-id))))
          (should (equal "interrupted" (car (last (wrec--outcomes id)))))
          (should (string-match-p "was cut off when Emacs exited"
                                  (superchat-work-render (superchat-work-get id))))
          (should (= 0 (wwf--wake-prompts id)))
          ;; Told once.
          (superchat-work-arm-all)
          (should (= 1 (length (wrec--facts main "work.reported")))))))))

(ert-deftest work-wake-fault/a-running-execution-is-not-taken-for-a-cut-off-one ()
  "A scan while the woken execution runs in this Emacs reports nothing."
  (wrec--with '((:text "ok")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "Still running"))
               (main (car pair))
               (id (cdr pair)))
          (should (= 1 (length (superchat-work-wake-due))))
          (superchat-work-arm-all)
          (should (= 0 (length (wrec--facts main "work.reported"))))
          (wrec--settled)
          (should (= 1 (length (wrec--facts main "work.reported"))))
          (should (equal "continuable" (car (last (wrec--outcomes id))))))))))

(provide 'work-wake-fault-regression)
;;; work-wake-fault-regression.el ends here
