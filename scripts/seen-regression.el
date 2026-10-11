;;; seen-regression.el --- giving up and looking are facts; the notices read them -*- lexical-binding: t; -*-

;;; Commentary:
;; A start or a wake this Emacs tries no more is a fact on the Tape, written
;; once.  That the user looked at an ended execution is a fact too, so a new
;; Emacs remembers it.  The executions Superchat started that ended and were
;; not looked at are read from the Tape: they outlive a restart, and one cut
;; off when Emacs quit is among them.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   --eval '(ert-run-tests-batch-and-exit "^seen/")'

;;; Code:

(load (expand-file-name "pending-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "work-wake-fault-regression.el"
                              (file-name-directory (or load-file-name buffer-file-name)))
            nil t)
    (advice-remove 'load skip-loaded)))

(defun seen--new-emacs ()
  "What a new Emacs knows: no Run, no notice, nothing counted or worked out.
Then the plugin's mount reads the Tape."
  (clrhash superchat-runtime--runs)
  (setq superchat-changes--notices nil)
  (clrhash superchat-changes--unsaved-at-end)
  (clrhash superchat-changes--started-by)
  (superchat-changes-notices-reload))

(defun seen--facts (session type)
  (hm--events session type))

(defmacro seen--counting-reads (var &rest body)
  "Run BODY, counting in VAR the reads of the Tape database."
  (declare (indent 1) (debug t))
  `(let ((read (symbol-function 'superchat-db-tape-select)))
     (cl-letf (((symbol-function 'superchat-db-tape-select)
                (lambda (&rest args) (cl-incf ,var) (apply read args))))
       ,@body)))

;;;; Giving up is a fact, written once --------------------------------------------

(ert-deftest seen/a-task-out-of-tries-leaves-one-fact ()
  (pend--fresh
    (wtimer--with nil (p)
      (let ((session (cdr (tsf--task p "SEEN gave up")))
            (superchat-timer-retry-limit 1))
        (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)
        (unwind-protect
            (tsf--send-failing
              (superchat-timer-catch-up)
              ;; One failure: it is tried again, nothing is given up yet.
              (should (= 0 (length (seen--facts session "timer.gave-up"))))
              (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds 1))
              (superchat-timer-retry-tasks)
              (let ((facts (seen--facts session "timer.gave-up")))
                (should (= 1 (length facts)))
                (should (= 2 (plist-get (plist-get (car facts) :payload) :tries)))
                (should (equal "timers" (plist-get (car facts) :actor-id))))
              ;; Passed over again and again, it says so no second time.
              (superchat-timer-retry-tasks)
              (setq wtimer--now (+ wtimer--now 100000))
              (superchat-timer-retry-tasks)
              (should (= 1 (length (seen--facts session "timer.gave-up")))))
          (clrhash superchat-timer--retries)
          (clrhash superchat-timer--not-before))))))

(ert-deftest seen/a-record-out-of-tries-leaves-one-fact ()
  (wrec--with nil (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-limit 0)
          (tries 0))
      (wrec--with-clock
        (let* ((pair (wwf--due-record p "SEEN out of tries"))
               (main (car pair)) (id (cdr pair)))
          (wwf--counting-broken-sends tries
            (superchat-work-wake-due)
            (let ((facts (seen--facts main "work.wake-gave-up")))
              (should (= 1 (length facts)))
              (should (equal id (plist-get (plist-get (car facts) :payload) :work-id)))
              (should (= 1 (plist-get (plist-get (car facts) :payload) :tries)))
              (should (equal "work-records" (plist-get (car facts) :actor-id))))
            (superchat-work-wake-due)
            (superchat-work-arm-all)
            (should (= 1 (length (seen--facts main "work.wake-gave-up"))))
            ;; The record itself is what it was: the fact is not one of its own.
            (should (plist-get (superchat-work-get id) :id))))))))

(ert-deftest seen/a-wake-that-is-tried-again-has-not-given-up ()
  "Sending always fails with one more try allowed: the fact comes with the
last try, and counts both."
  (wrec--with nil (p)
    (let ((wrec--now wrec--now)
          (superchat-work-wake-retry-seconds 0)
          (superchat-work-wake-retry-limit 1)
          (calls 0))
      (wrec--with-clock
        (let ((main (car (wwf--due-record p "SEEN tried again"))))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda (&rest _)
                       (setq calls (1+ calls))
                       (error "SEEN injected: send always fails"))))
            (superchat-work-wake-due)
            ;; One try failed and another is still to come: nothing given up.
            (should (= 1 calls))
            (should (= 0 (length (seen--facts main "work.wake-gave-up"))))
            (should (smoke--wait (lambda () (= 2 calls)) 10)))
          (let ((facts (seen--facts main "work.wake-gave-up")))
            (should (= 1 (length facts)))
            (should (= 2 (plist-get (plist-get (car facts) :payload) :tries)))))))))

;;;; Not looked at outlives a restart --------------------------------------------------

(ert-deftest seen/a-notice-is-still-there-in-a-new-emacs ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (should (= 1 (length superchat-changes--notices)))
      (seen--new-emacs)
      (should (= 1 (length superchat-changes--notices)))
      (let ((notice (car superchat-changes--notices))
            (item (car (pend--of 'unseen))))
        (should (equal (plist-get fired :run-id) (plist-get notice :run)))
        (should (equal session (plist-get notice :session)))
        (should (eq 'completed (plist-get notice :status)))
        (should (string-match-p "\\`scheduled task timer-" (plist-get notice :what)))
        (should (equal (plist-get fired :run-id) (plist-get item :key))))
      (should (string-match-p "1" (un--shown))))))

(ert-deftest seen/looking-is-a-fact-a-new-emacs-remembers ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (let ((run (plist-get fired :run-id)))
        (should (superchat-changes-notice-dismiss run))
        (should-not superchat-changes--notices)
        (let ((facts (seen--facts session "run.seen")))
          (should (= 1 (length facts)))
          (should (equal run (plist-get (car facts) :run-id)))
          (should (equal "user" (plist-get (car facts) :actor-id))))
        (seen--new-emacs)
        (should-not superchat-changes--notices)
        (should-not (pend--of 'unseen))
        (should-not (un--shown))
        ;; Asked again, nothing is written again.
        (should-not (superchat-changes-notice-dismiss run))
        (should (= 1 (length (seen--facts session "run.seen"))))))))

(ert-deftest seen/a-task-cut-off-when-emacs-quit-is-listed ()
  "It started (`timer.started' on record) and Emacs quit while it ran."
  (pend--fresh
    (wrec--with '((:text "NEVER-DELIVERED" :delay 30)) (p)
      (let* ((now 1800000000.0)
             (superchat-timer-now-function (lambda () now))
             (superchat-timer-notify-function #'ignore)
             (session (hm--session (wrec--view p))))
        (unwind-protect
            (progn
              (superchat-timer-create session 'task "SEEN cut off" (+ now 60))
              (setq now (+ now 60))
              (let* ((fired (car (superchat-timer-catch-up)))
                     (run (plist-get fired :run-id)))
                (should (stringp run))
                ;; Still running: under way, not something to look at.
                (superchat-changes-notices-reload)
                (should-not superchat-changes--notices)
                (seen--new-emacs)
                (let ((notice (car superchat-changes--notices))
                      (item (car (pend--of 'unseen))))
                  (should (= 1 (length superchat-changes--notices)))
                  (should (equal run (plist-get notice :run)))
                  (should (eq 'cut-off (plist-get notice :status)))
                  (should (string-match-p "cut off when Emacs quit"
                                          (plist-get item :detail)))
                  (should (string-match-p "1" (un--shown))))
                ;; Reopening the conversation seals the Run; it is the same row.
                (when-let* ((view (superchat-timer--live-view session)))
                  (kill-buffer view))
                (let ((default-directory superchat-data-directory))
                  (superchat--render-session-view session))
                (superchat-changes-notices-reload)
                (should (= 1 (length superchat-changes--notices)))
                (should (eq 'cut-off
                            (plist-get (car superchat-changes--notices) :status)))
                (should (superchat-changes-notice-dismiss run))
                (seen--new-emacs)
                (should-not superchat-changes--notices)))
          (superchat-timer-disarm-all))))))

(ert-deftest seen/mounting-the-plugin-reads-what-an-earlier-emacs-left ()
  (pend--fresh
    (un--with-task '((:text "ANSWER")) (session fired)
      (should (= 1 (length superchat-changes--notices)))
      ;; A new Emacs, before anything asks.
      (clrhash superchat-runtime--runs)
      (setq superchat-changes--notices nil)
      (let ((scope (superchat-scope-create "seen-mount" superchat-scope-global)))
        (unwind-protect
            (progn
              (superchat-plugin-mount 'changes nil scope)
              (should (equal (list (plist-get fired :run-id))
                             (mapcar (lambda (n) (plist-get n :run))
                                     superchat-changes--notices)))
              (should (string-match-p "1" (un--shown))))
          (ignore-errors (superchat-scope-dispose scope)))))))

;;;; Each execution is looked at by its own fact ----------------------------------------

(defun seen--two-ended (view)
  "Two executions Superchat started in VIEW's conversation, both ended."
  (list (let ((superchat-automated-input 'timers)) (hm--turn view "one"))
        (let ((superchat-automated-input 'work-records)) (hm--turn view "two"))))

(ert-deftest seen/dismissing-all-records-each-one ()
  (pend--fresh
    (hm--fixture (list '(:text "ONE") '(:text "TWO"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (runs (seen--two-ended view)))
        (superchat-changes-notices-reload)
        (should (= 2 (length superchat-changes--notices)))
        ;; Newest first.
        (should (equal (reverse runs)
                       (mapcar (lambda (n) (plist-get n :run))
                               superchat-changes--notices)))
        (superchat-changes-notices-dismiss-all)
        (should-not superchat-changes--notices)
        (should (equal (sort (copy-sequence runs) #'string<)
                       (sort (mapcar (lambda (e) (plist-get e :run-id))
                                     (seen--facts session "run.seen"))
                             #'string<)))
        (seen--new-emacs)
        (should-not superchat-changes--notices)))))

(ert-deftest seen/a-look-that-cannot-be-recorded-keeps-the-notice ()
  (pend--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (run (let ((superchat-automated-input 'timers)) (hm--turn view "one")))
             said)
        (superchat-changes-notices-reload)
        (should (= 1 (length superchat-changes--notices)))
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (&rest _) (error "SEEN injected: the Tape is closed")))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (when format-string
                       (push (apply #'format format-string args) said))
                     nil)))
          (should-not (superchat-changes-notice-dismiss run)))
        (should (= 1 (length superchat-changes--notices)))
        (should (cl-some (lambda (line)
                           (string-match-p "could not be recorded.*SEEN injected" line))
                         said))
        (should-not (seen--facts session "run.seen"))
        (seen--new-emacs)
        (should (= 1 (length superchat-changes--notices)))))))

;;;; Whose executions ---------------------------------------------------------------------

(ert-deftest seen/an-execution-the-user-started-is-not-listed ()
  (pend--fresh
    (hm--fixture (list '(:text "ONE") '(:text "TWO"))
      (let* ((view (smoke--open-chat))
             (mine (hm--turn view "one"))
             (its (let ((superchat-automated-input 'timers)) (hm--turn view "two"))))
        (seen--new-emacs)
        (should (equal (list its)
                       (mapcar (lambda (n) (plist-get n :run))
                               superchat-changes--notices)))
        (should-not (member mine (mapcar (lambda (n) (plist-get n :run))
                                         superchat-changes--notices)))))))

;;;; The read is bounded --------------------------------------------------------------------

(ert-deftest seen/the-list-reads-nothing-when-it-is-drawn ()
  (pend--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (reads 0))
        (let ((superchat-automated-input 'timers)) (hm--turn view "one"))
        (superchat-changes-notices-reload)
        (seen--counting-reads reads
          (dotimes (_ 5)
            (superchat-pending--unseen)
            (superchat-changes--notices-entries)
            (superchat-changes--notices-refresh)))
        (should (= 0 reads))))))

(ert-deftest seen/a-reload-reads-once-however-many-conversations ()
  (pend--fresh
    (hm--fixture (list '(:text "ONE"))
      (let* ((view (smoke--open-chat))
             (few 0) (many 0))
        ;; Looked at: only the reload's own read is left to count.
        (hm--turn view "one")
        (seen--counting-reads few (superchat-changes-notices-reload))
        (dotimes (n 40)
          (superchat-tape-append-event
           (list :type "timer.created" :session-id (format "seen-other-%d" n)
                 :actor-id "timers" :summary "filler"
                 :payload (list :timer-id (format "filler-%d" n)))))
        (seen--counting-reads many (superchat-changes-notices-reload))
        (should (= 1 few))
        (should (= 1 many))))))

(ert-deftest seen/what-is-settled-is-not-read-again ()
  "The bound moves past executions that were looked at, and stays below one
that is not."
  (pend--fresh
    (hm--fixture (list '(:text "ONE") '(:text "TWO") '(:text "THREE"))
      (let* ((view (smoke--open-chat))
             (session (hm--session view))
             (bound (lambda ()
                      (or (cadr (superchat-db-projection-checkpoint
                                 superchat-changes--unseen-checkpoint))
                          0)))
             (began (lambda (run)
                      (caar (superchat-db-tape-select
                             "SELECT id FROM tape WHERE session_id = ?
                              AND json_extract(meta, '$.event_type') = 'run.started'
                              AND json_extract(meta, '$.run_id') = ?"
                             (list session run)))))
             (first (let ((superchat-automated-input 'timers)) (hm--turn view "one")))
             (second (let ((superchat-automated-input 'timers)) (hm--turn view "two"))))
        (superchat-changes-notices-reload)
        ;; Nothing looked at: the bound is below the older one.
        (should (< (funcall bound) (funcall began first)))
        ;; The newer one looked at: the older one still holds the bound.
        (should (superchat-changes-notice-dismiss second))
        (should (< (funcall bound) (funcall began first)))
        (should (= 1 (length superchat-changes--notices)))
        ;; Both looked at: everything so far is settled.
        (should (superchat-changes-notice-dismiss first))
        (should (> (funcall bound) (funcall began second)))
        (let ((settled (funcall bound))
              (third (let ((superchat-automated-input 'timers))
                       (hm--turn view "three"))))
          (superchat-changes-notices-reload)
          (should (equal (list third)
                         (mapcar (lambda (n) (plist-get n :run))
                                 superchat-changes--notices)))
          (should (>= (funcall bound) settled))
          (should (< (funcall bound) (funcall began third))))))))

(provide 'seen-regression)

;;; seen-regression.el ends here
