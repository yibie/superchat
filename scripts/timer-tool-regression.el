;;; timer-tool-regression.el --- a timer the tool sets will fire, or it is refused -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-the-timer-tool-sets-or-refuses.md.
;; The same kind of gap as the review's F4, in the timer tools: with the
;; `timers' row unmounted `timer_create' answered "the user will be
;; reminded" and wrote a timer that nothing would fire.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/timer-tool-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^timer-tool/")'

;;; Code:

(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defvar superchat--tool-execution-context)

(defmacro ttool--as (session &rest body)
  "Run BODY as a tool call of a Run in SESSION."
  (declare (indent 1) (debug t))
  `(let ((superchat--tool-execution-context (list :session-id ,session)))
     ,@body))

(defmacro ttool--row-off (&rest body)
  "Run BODY with the `timers' row unmounted, mounting it again after."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (superchat-plugin-unmount 'timers)
              (should-not (superchat-plugin-row-active-p 'timers))
              ,@body)
     (superchat-plugin-mount 'timers)))

(defconst ttool--off "turned off in this Emacs"
  "What a timer tool says while nothing would fire a timer.")

(defun ttool--created (session)
  "How many timers SESSION's Tape says were created."
  (length (hm--events session "timer.created")))

(ert-deftest timer-tool/with-the-row-off-a-timer-is-refused-in-the-tool-loop ()
  "Through the real tool loop: the model is told, and nothing is written."
  (wrec--with nil (p)
    (let ((view (wrec--view p)))
      (ttool--row-off
        (setq smoke--script
              (list '(:tool-uses (("timer_create" (text . "TTOOL-LOOP")
                                   (kind . "reminder") (in_minutes . 60))))
                    '(:text "finished")))
        (hm--turn view "remind me in an hour")
        (let ((session (hm--session view)))
          (should (string-match-p ttool--off (prin1-to-string hm--wires)))
          (should (= 0 (ttool--created session)))
          (should-not (superchat-timer-list))
          (should (= 0 (hash-table-count superchat-timer--armed))))))))

(ert-deftest timer-tool/with-the-row-off-neither-kind-is-set ()
  (wrec--with nil (p)
    (wrec--with-clock
      (let ((session (hm--session (wrec--view p))))
        (ttool--row-off
          (ttool--as session
            (dolist (kind '("reminder" "task"))
              (let ((answer (superchat-timer-tool-create "TTOOL-OFF" kind 60)))
                (should (string-prefix-p "Error:" answer))
                (should (string-match-p ttool--off answer))
                (should (string-match-p "no timer was set" answer))))
            ;; A repeating one, and one given a clock time, are refused too.
            (should (string-match-p
                     ttool--off
                     (superchat-timer-tool-create "TTOOL-OFF" "task" 60 nil "daily"))))
          (should (= 0 (ttool--created session)))
          (should-not (superchat-timer-list)))))))

(ert-deftest timer-tool/with-the-row-off-list-says-so-and-cancel-works ()
  "What was set before can still be seen and cancelled."
  (wrec--with nil (p)
    (wrec--with-clock
      (let* ((session (hm--session (wrec--view p)))
             (id (plist-get (superchat-timer-create
                             session 'reminder "TTOOL-EARLIER"
                             (+ (superchat-timer--now) 3600))
                            :id)))
        (ttool--row-off
          (ttool--as session
            (let ((listing (superchat-timer-tool-list)))
              (should (string-match-p ttool--off listing))
              (should (string-match-p "TTOOL-EARLIER" listing)))
            (should (string-match-p "cancelled" (superchat-timer-tool-cancel id)))
            (should-not (superchat-timer-list))))))))

(ert-deftest timer-tool/with-the-row-on-nothing-changed ()
  "Guard: the answers of a mounted row do not mention being off."
  (wrec--with nil (p)
    (wrec--with-clock
      (let ((session (hm--session (wrec--view p))))
        (should (superchat-plugin-row-active-p 'timers))
        (ttool--as session
          (let ((answer (superchat-timer-tool-create "TTOOL-ON" "reminder" 60)))
            (should (string-prefix-p "Timer timer-" answer))
            (should (string-match-p "the user will be reminded" answer))
            (should-not (string-match-p ttool--off answer)))
          (let ((listing (superchat-timer-tool-list)))
            (should (string-match-p "TTOOL-ON" listing))
            (should-not (string-match-p ttool--off listing))))
        (should (= 1 (ttool--created session)))
        (should (= 1 (hash-table-count superchat-timer--armed)))))))

(ert-deftest timer-tool/mounted-again-a-timer-is-set-and-armed ()
  (wrec--with nil (p)
    (wrec--with-clock
      (let ((session (hm--session (wrec--view p))))
        (ttool--row-off
          (ttool--as session
            (should (string-match-p
                     ttool--off (superchat-timer-tool-create "TTOOL-BACK" "reminder" 60)))))
        (should (superchat-plugin-row-active-p 'timers))
        (ttool--as session
          (should (string-prefix-p
                   "Timer timer-" (superchat-timer-tool-create "TTOOL-BACK" "reminder" 60))))
        (let ((timer (car (superchat-timer-list))))
          (should (equal "TTOOL-BACK" (plist-get timer :text)))
          (should (timerp (gethash (plist-get timer :id) superchat-timer--armed))))))))

(provide 'timer-tool-regression)
;;; timer-tool-regression.el ends here
