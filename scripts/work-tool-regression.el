;;; work-tool-regression.el --- what the work_record tool is given, it keeps or refuses -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-the-work-record-tool-keeps-or-refuses.md.
;; Two findings of the architecture review of 2026-10-10.  F5: a start that
;; came with progress, a place to pick up and a wait answered "Started" and
;; kept none of them.  F4: with the `work-records' row unmounted the tool
;; still registered a wait for a time that nothing would ever act on.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/work-tool-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^work-tool/")'

;;; Code:

(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defvar superchat--tool-execution-context)

(defmacro wtool--as (session &rest body)
  "Run BODY as a tool call of a Run in SESSION."
  (declare (indent 1) (debug t))
  `(let ((superchat--tool-execution-context (list :session-id ,session)))
     ,@body))

(defun wtool--rows ()
  "How many Work Record facts every Tape holds."
  (length (superchat-work--rows)))

(defmacro wtool--row-off (&rest body)
  "Run BODY with the `work-records' row unmounted, mounting it again after."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (superchat-plugin-unmount 'work-records)
              (should-not (superchat-plugin-row-active-p 'work-records))
              ,@body)
     (superchat-plugin-mount 'work-records)))

(defconst wtool--off "turned off in this Emacs"
  "What a wait for a time is told while nothing would act on it.")

;;;; F5: what comes with a start --------------------------------------------------

(ert-deftest work-tool/a-start-keeps-the-progress-the-place-and-the-wait-it-came-with ()
  "The review's F5."
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (answer (wtool--as session
                       (superchat-work-tool "start" nil "WTOOL-GOAL" "WTOOL-DONE-SO-FAR"
                                            nil "WTOOL-PICK-UP" "time" "WTOOL-LATER"
                                            nil nil 60 nil)))
             (id (superchat-work-current session))
             (record (superchat-work-get id)))
        (should id)
        (should (string-prefix-p (format "Started %s; " id) answer))
        ;; The answer shows what was kept.
        (should (string-match-p "WTOOL-DONE-SO-FAR" answer))
        (should (equal "WTOOL-DONE-SO-FAR" (plist-get record :progress)))
        (should (equal "WTOOL-PICK-UP" (plist-get record :resume)))
        (let ((waiting (plist-get record :waiting)))
          (should (equal "time" (plist-get waiting :for)))
          (should (equal "WTOOL-LATER" (plist-get waiting :detail)))
          (should (= (+ wrec--now 3600) (plist-get waiting :until))))
        ;; And the wait is one that will be acted on.
        (should (gethash id superchat-work--armed))))))

(ert-deftest work-tool/a-start-keeps-a-wait-for-the-user ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p))))
      (wtool--as session
        (superchat-work-tool "start" nil "WTOOL-GOAL" nil "WTOOL-BLOCKED" nil
                             "user" "WTOOL-WHICH-ONE"))
      (let ((record (superchat-work-get (superchat-work-current session))))
        (should (equal "WTOOL-BLOCKED" (plist-get record :blocked)))
        (should (equal "user" (plist-get (plist-get record :waiting) :for)))
        (should (equal "WTOOL-WHICH-ONE" (plist-get (plist-get record :waiting) :detail)))))))

(ert-deftest work-tool/a-plain-start-answers-as-before ()
  "Guard."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (answer (wtool--as session (superchat-work-tool "start" nil "WTOOL-GOAL")))
           (id (superchat-work-current session)))
      (should (equal (format "Started %s; it is this conversation's current work record." id)
                     answer))
      (let ((record (superchat-work-get id)))
        (should-not (plist-get record :progress))
        (should-not (plist-get record :waiting))))))

(ert-deftest work-tool/a-start-that-cannot-keep-what-it-came-with-starts-nothing ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (rows (wtool--rows)))
      (wtool--as session
        ;; A conclusion: starting and completing are two calls.
        (should (string-match-p "\\`Error: .*not started and completed in one call"
                                (superchat-work-tool "start" nil "WTOOL-GOAL" nil nil nil
                                                     nil nil "WTOOL-CONCLUSION")))
        ;; A wait for a time without the time.
        (should (string-match-p "\\`Error: .*needs the time"
                                (superchat-work-tool "start" nil "WTOOL-GOAL" "P" nil nil
                                                     "time" "later")))
        ;; A wait for something a record cannot wait for.
        (should (string-match-p "\\`Error: .*waits for one of"
                                (superchat-work-tool "start" nil "WTOOL-GOAL" "P" nil nil
                                                     "tomorrow"))))
      (should (= rows (wtool--rows)))
      (should-not (superchat-work-current session)))))

(ert-deftest work-tool/an-update-whose-wait-is-refused-writes-nothing ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "WTOOL-GOAL"))
           (rows (wtool--rows)))
      (wtool--as session
        (should (string-match-p "\\`Error: .*needs the time"
                                (superchat-work-tool "update" id nil "WTOOL-HALF" nil nil
                                                     "time" "later"))))
      (should (= rows (wtool--rows)))
      (should-not (plist-get (superchat-work-get id) :progress)))))

(ert-deftest work-tool/an-update-still-keeps-its-progress-and-its-wait ()
  "Guard: the case found in the real task T4W."
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "WTOOL-GOAL")))
        (wtool--as session
          (should-not (string-prefix-p
                       "Error" (superchat-work-tool "update" id nil "WTOOL-HALF" nil nil
                                                    "time" "later" nil nil 30))))
        (let ((record (superchat-work-get id)))
          (should (equal "WTOOL-HALF" (plist-get record :progress)))
          (should (= (+ wrec--now 1800) (plist-get (plist-get record :waiting) :until))))))))

;;;; F4: the row is not mounted ------------------------------------------------------

(ert-deftest work-tool/with-the-row-off-the-model-has-no-work-record ()
  "The review's F4, through the real tool loop, as it stands since 2026-10-12.
`work_record' is the `work-records' row's tool (slice 7b): with the row off
the model is not offered it, and a call to it all the same is answered as not
available before any body runs, so no record is started and no wait is
registered.  Until then the
tool stayed and only its wait for a time was refused; the function still
answers that way when it is called directly (the tests below)."
  (wrec--with nil (p)
    (let ((view (wrec--view p)))
      (wtool--row-off
        (setq superchat-llm-tools-list nil)
        (setq smoke--script
              (list (wrec--call '(action . "start") '(goal . "WTOOL-ROW-OFF"))
                    (wrec--call '(action . "wait") '(wait_for . "time") '(in_minutes . 60))
                    '(:text "finished")))
        (hm--turn view "register a future wait")
        (let ((session (hm--session view)))
          ;; The model was not offered the tool ...
          (should-not (member "work_record"
                              (plist-get (car smoke--requests) :tools)))
          ;; ... and was told why its calls did nothing.
          (should (string-match-p "work_record.*not available"
                                  (prin1-to-string hm--wires)))
          (should-not (superchat-work-current session))
          (should-not (superchat-work-records))
          (should-not (hm--events session "work.created"))
          (should-not (hm--events session "work.waiting"))
          (should (= 0 (hash-table-count superchat-work--armed))))))))

(ert-deftest work-tool/with-the-row-off-nothing-that-comes-with-a-time-is-half-kept ()
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "WTOOL-GOAL")))
      (wtool--row-off
        (let ((rows (wtool--rows)))
          (wtool--as session
            ;; A start with a wait for a time starts nothing.
            (should (string-match-p wtool--off
                                    (superchat-work-tool "start" nil "WTOOL-SECOND" "P" nil nil
                                                         "time" "later" nil nil 60)))
            ;; An update with one writes nothing.
            (should (string-match-p wtool--off
                                    (superchat-work-tool "update" id nil "WTOOL-HALF" nil nil
                                                         "time" "later" nil nil 60))))
          (should (= rows (wtool--rows)))
          (should (equal id (superchat-work-current session)))
          (should-not (plist-get (superchat-work-get id) :progress)))))))

(ert-deftest work-tool/with-the-row-off-what-needs-no-timer-still-works ()
  "Guard: the history stays readable and the record stays writable."
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "WTOOL-GOAL")))
      (wtool--row-off
        (wtool--as session
          (should (string-match-p "WTOOL-GOAL" (superchat-work-tool "read" id)))
          (should-not (string-prefix-p "Error" (superchat-work-tool "update" id nil "WTOOL-HALF")))
          (should-not (string-prefix-p
                       "Error" (superchat-work-tool "wait" id nil nil nil nil "user" "WTOOL-ASK")))
          (should (equal "user" (plist-get (plist-get (superchat-work-get id) :waiting) :for)))
          (should (string-match-p "Completed" (superchat-work-tool "done" id nil nil nil nil
                                                                   nil nil "WTOOL-DELIVERED"))))
        (should (equal "WTOOL-DELIVERED" (plist-get (superchat-work-get id) :conclusion)))))))

(ert-deftest work-tool/with-the-row-on-again-a-wait-for-a-time-is-kept-and-armed ()
  "Guard."
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (id (superchat-work-start session "WTOOL-GOAL")))
        (wtool--row-off
          (wtool--as session
            (should (string-match-p wtool--off
                                    (superchat-work-tool "wait" id nil nil nil nil
                                                         "time" "later" nil nil 60)))))
        (should (superchat-plugin-row-active-p 'work-records))
        (wtool--as session
          (should-not (string-prefix-p
                       "Error" (superchat-work-tool "wait" id nil nil nil nil
                                                    "time" "later" nil nil 60))))
        (should (equal "time" (plist-get (plist-get (superchat-work-get id) :waiting) :for)))
        (should (gethash id superchat-work--armed))))))

(provide 'work-tool-regression)

;;; work-tool-regression.el ends here
