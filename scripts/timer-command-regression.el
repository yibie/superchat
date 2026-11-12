;;; timer-command-regression.el --- a command the program runs by itself -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-a-command-that-runs-by-itself.md: a timer
;; can carry one fixed command, asked about once when it is set.  At its time the
;; program runs it and compares; no model, no Run, no question.  Only a change, or
;; a failure that keeps happening, is said.
;;
;; The commands here are real processes run by `sh'.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-jit-compilation nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/timer-command-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^timer-command/")'

;;; Code:

(load (expand-file-name "own-place-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun tcmd--view (root)
  ;; Each test is a new Emacs: nothing is left to look at from the one before.
  (setq superchat-changes--notices nil)
  (clrhash superchat-butler--conclusions)
  (with-current-buffer (wtimer--file root) (superchat)))

(defun tcmd--settled ()
  "Wait until no command of a timer is running."
  (should (smoke--wait (lambda () (zerop (hash-table-count superchat-timer--running))) 10)))

(defun tcmd--set (session text command &optional repeat report-unchanged)
  "Set a command timer in SESSION, due in an hour, and wait for its first run."
  (prog1 (superchat-timer-create session 'command text (+ (superchat-timer--now) 3600)
                                 repeat command report-unchanged)
    (tcmd--settled)))

(defun tcmd--counter (root value)
  "Write VALUE where a command made by `tcmd--reading' reads it, in ROOT."
  (wtimer--write root "value.txt" value))

(defun tcmd--reading (root value)
  "The command printing a file under ROOT that starts with VALUE."
  (tcmd--counter root value)
  "cat value.txt")

(defun tcmd--fire (timer &optional occurrence)
  "Fire TIMER and wait for its command."
  (prog1 (superchat-timer-fire timer (or occurrence (superchat-timer--now)))
    (tcmd--settled)))

(defun tcmd--results (&optional timer)
  "The `command.result' facts, oldest first; of TIMER when given."
  (cl-remove-if-not
   (lambda (event)
     (or (null timer)
         (equal (plist-get timer :id)
                (plist-get (plist-get event :payload) :timer-id))))
   (hm--events superchat-butler-session-id "command.result")))

(defun tcmd--status (event)
  (plist-get (plist-get event :payload) :status))

(defun tcmd--lines (&optional timer)
  "The butler's lines; of TIMER when given."
  (cl-remove-if-not
   (lambda (line)
     (or (null timer)
         (string-prefix-p (concat (plist-get timer :id) "@")
                          (or (plist-get line :run) ""))))
   (superchat-butler-lines)))

(defun tcmd--last-line-text ()
  (superchat-butler-line-text (car (last (superchat-butler-lines)))))

(defun tcmd--count ()
  (length (superchat-changes--own-notices)))

(defmacro tcmd--asking (answer &rest body)
  "Run BODY with questions answered ANSWER; bind `tcmd--asked' to what was asked."
  (declare (indent 1) (debug t))
  `(let* ((tcmd--asked nil)
          (superchat-approval-function
           (lambda (request) (push request tcmd--asked) ,answer)))
     ,@body))

(defvar tcmd--asked nil)

(defun tcmd--call (&rest fields)
  "A tool call of `timer_create' with FIELDS, an alist."
  `(:tool-uses (("timer_create" ,@fields))))

(defun tcmd--events-anywhere (type)
  "How many facts of TYPE are on the whole Tape."
  (caar (superchat-db-tape-select
         "SELECT COUNT(*) FROM tape WHERE json_extract(meta, '$.event_type') = ?"
         (list type))))

(defun tcmd--as-shown (view)
  "VIEW's text as a window shows it once Org has fontified it.
Batch Emacs does not fontify by itself, so this does what redisplay would.
What Org hides is left out, a `display' string stands for the text under it,
and what Org composes into something else says so."
  (with-current-buffer view
    (org-set-font-lock-defaults)
    (setq font-lock-set-defaults nil)
    (font-lock-set-defaults)
    (with-silent-modifications
      (font-lock-unfontify-region (point-min) (point-max))
      (font-lock-default-fontify-region (point-min) (point-max) nil))
    (let ((pos (point-min)) (out nil))
      (while (< pos (point-max))
        (let ((display (get-char-property pos 'display)))
          (cond ((invisible-p pos) (setq pos (1+ pos)))
                ((stringp display)
                 (push display out)
                 (setq pos (next-single-char-property-change pos 'display)))
                ((get-text-property pos 'composition)
                 (push "<composed>" out)
                 (setq pos (1+ pos)))
                (t (push (char-to-string (char-after pos)) out)
                   (setq pos (1+ pos))))))
      (apply #'concat (nreverse out)))))

(defun tcmd--panel-shown (root)
  "Have the script set a timer in ROOT; what its panel shows, without line breaks.
Refuses it.  Refusing happens whatever goes wrong, because a question left open
would make the turn wait for ever."
  (let* ((view (tcmd--view root))
         (superchat-approval-function #'superchat-approval-publish)
         (shown nil))
    (run-at-time 0.2 nil
                 (lambda ()
                   (unwind-protect (setq shown (tcmd--as-shown view))
                     (superchat-approval-answer
                      (plist-get (car (superchat-approval-open-questions)) :handle) nil))))
    (hm--turn view "tell me")
    (should (string-match "Approval needed\n\\(\\(?:.\\|\n\\)*?\\)Waiting for your answer"
                          (or shown "")))
    (replace-regexp-in-string "\n" "" (match-string 1 shown))))

;;;; Setting one -----------------------------------------------------------------

(ert-deftest timer-command/setting-one-asks-once-with-the-whole-command ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(repeat . "daily")
                                  '(command . "echo COMMAND-TEXT"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((view (tcmd--view p)))
        (hm--turn view "tell me every day")
        (tcmd--settled)
        (should (= 1 (length tcmd--asked)))
        (let ((reason (plist-get (car tcmd--asked) :reason)))
          (should (string-match-p "echo COMMAND-TEXT" reason))
          (should (string-match-p (regexp-quote (directory-file-name p)) reason))
          (should (string-match-p "daily" reason)))
        (let ((timer (car (superchat-timer-list))))
          (should (eq 'command (plist-get timer :kind)))
          (should (equal "echo COMMAND-TEXT" (plist-get timer :command)))
          (should (equal (file-name-as-directory p)
                         (file-name-as-directory (plist-get timer :directory))))
          (should (= 86400 (plist-get timer :repeat))))
        ;; The model is told what happens, and to tell the user.
        (should (string-match-p "only when" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/a-first-run-saying-denied-can-be-corrected ()
  ;; Measured 2026-11-01: "Permission denied" from a script without the execute
  ;; bit read as a refusal, and the model was told not to retry (1 run of 2 gave
  ;; up).  What a command prints is not what the user decided.
  (dolist (word '("Permission denied" "blocked" "refused" "cancelled"))
    (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                    '(in_minutes . 60) '(repeat . "daily")
                                    (cons 'command (format "echo %s >&2; exit 126" word)))
                        '(:text "finished"))
        (p)
      (tcmd--asking t
        (let ((view (tcmd--view p)))
          (hm--turn view "tell me")
          (tcmd--settled)
          (should-not (superchat-timer-list))
          (let ((wire (prin1-to-string hm--wires)))
            (should (string-match-p (regexp-quote word) wire))
            (should (string-match-p "Correct the arguments of the failed call" wire))
            (should-not (string-match-p "do not retry it" wire))))))))

(ert-deftest timer-command/refusing-it-still-says-do-not-retry ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(repeat . "daily")
                                  '(command . "echo THE-COMMAND"))
                      '(:text "finished"))
      (p)
    (tcmd--asking nil
      (let ((view (tcmd--view p)))
        (hm--turn view "tell me")
        (tcmd--settled)
        (should (string-match-p "do not retry it" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/refused-means-no-timer ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo NOT-RUN"))
                      '(:text "finished"))
      (p)
    (tcmd--asking nil
      (hm--turn (tcmd--view p) "tell me")
      (tcmd--settled)
      (should (= 1 (length tcmd--asked)))
      (should-not (superchat-timer-list))
      (should-not (tcmd--results))
      (should (string-match-p "denied" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/any-call-with-a-command-is-asked-about ()
  ;; Whatever `kind' says, what would run is what is asked about.
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "task")
                                  '(in_minutes . 60) '(command . "echo AS-A-TASK"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me")
      (tcmd--settled)
      (should (= 1 (length tcmd--asked)))
      (should (eq 'command (plist-get (car (superchat-timer-list)) :kind))))))

(ert-deftest timer-command/other-timers-are-not-asked-about ()
  (wtimer--with (list (tcmd--call '(text . "A REMINDER") '(kind . "reminder")
                                  '(in_minutes . 60))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "remind me")
      (should-not tcmd--asked)
      (should (eq 'reminder (plist-get (car (superchat-timer-list)) :kind))))))

(ert-deftest timer-command/read-only-refuses-without-asking ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo NOT-RUN"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((view (tcmd--view p)))
        (superchat-permission-set (hm--session view) 'read-only t)
        (hm--turn view "tell me")
        (should-not tcmd--asked)
        (should-not (superchat-timer-list))
        (should (string-match-p "read-only" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/auto-still-asks ()
  ;; It goes on after this conversation is closed: as saving, `auto' does not cover it.
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((view (tcmd--view p)))
        (superchat-permission-set (hm--session view) 'auto t)
        (hm--turn view "tell me")
        (should (= 1 (length tcmd--asked)))
        (should (superchat-timer-list))))))

(ert-deftest timer-command/with-approvals-off-it-is-refused ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo NOT-RUN"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((view (tcmd--view p)))
        (superchat-permission-set (hm--session view) 'danger-full-access t)
        (hm--turn view "tell me")
        (should-not tcmd--asked)
        (should-not (superchat-timer-list))
        (should (string-match-p "denied" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/nobody-looking-at-a-background-conversation-cannot-set-one ()
  ;; The question would wait for nobody, and what it read may have told it to.
  (wtimer--with (list (tcmd--call '(text . "PLANTED") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo PLANTED"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let* ((view (tcmd--view p))
             (other (own-place--background view)))
        (hm--turn other "a task")
        (should-not tcmd--asked)
        (should-not (superchat-timer-list))
        (should (string-match-p "nobody" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/someone-looking-at-it-can-be-asked ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let* ((view (tcmd--view p))
             (other (own-place--background view)))
        (set-window-buffer (selected-window) other)
        (hm--turn other "set it")
        (should (= 1 (length tcmd--asked)))
        (should (superchat-timer-list))))))

(ert-deftest timer-command/another-listener-s-refusal-stands ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo NOT-RUN"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((off (superchat-on nil 'tools/pre-execute 'tcmd-refuse
                               (lambda (call next)
                                 (if (equal "timer_create" (plist-get call :tool-name))
                                     (plist-put call :kind 'deny)
                                   (funcall next call))))))
        (unwind-protect (hm--turn (tcmd--view p) "tell me")
          (funcall off))
        (should-not tcmd--asked)
        (should-not (superchat-timer-list))))))

(ert-deftest timer-command/a-call-that-cannot-work-is-an-error-and-asks-nothing ()
  ;; Nobody approves something only to hear that it could not be set, and the model
  ;; is told what to correct, not that a policy stopped it.
  (dolist (fields `(((text . "NO TIME") (kind . "command") (command . "echo HI"))
                    ((text . "BAD REPEAT") (kind . "command") (in_minutes . 60)
                     (repeat . "fortnightly") (command . "echo HI"))
                    ((text . "TWO LINES") (kind . "command") (in_minutes . 60)
                     (command . "echo ONE\necho TWO"))
                    ((text . "AN ESCAPE") (kind . "command") (in_minutes . 60)
                     (command . ,(concat "echo HI" (string 27) "[2K")))
                    ((text . "TOO LONG") (kind . "command") (in_minutes . 60)
                     (command . ,(make-string 4001 ?x)))))
    (wtimer--with (list (apply #'tcmd--call fields) '(:text "finished")) (p)
      (tcmd--asking t
        (hm--turn (tcmd--view p) "tell me")
        (should-not tcmd--asked)
        (should-not (superchat-timer-list))
        (let ((wire (prin1-to-string hm--wires)))
          (should (string-match-p "Error: " wire))
          (should-not (string-match-p "denied" wire)))))))

(ert-deftest timer-command/a-time-in-the-past-can-be-corrected-before-the-question ()
  ;; Measured 2026-11-01: the model does not know the date.  Set first for a day in
  ;; May: two questions and two timers.  Refused as a policy: it gave up.
  (wtimer--with (list (tcmd--call '(text . "PAST") '(kind . "command")
                                  '(at . "2026-05-01 09:00") '(repeat . "daily")
                                  '(command . "echo HI"))
                      (tcmd--call '(text . "PAST") '(kind . "command")
                                  '(at . "2030-05-01 09:00") '(repeat . "daily")
                                  '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me")
      (tcmd--settled)
      (should (= 1 (length tcmd--asked)))
      (should (= 1 (length (superchat-timer-list))))
      (let ((wire (prin1-to-string hm--wires)))
        (should (string-match-p "passed.*it is now" wire))
        (should-not (string-match-p "do not retry" wire))))))

(ert-deftest timer-command/no-kind-of-timer-takes-a-time-in-the-past ()
  ;; Measured 2026-11-01: a daily task set for a day in July ran at once, and the
  ;; model went on to set two more.
  (dolist (kind '("task" "reminder"))
    (wtimer--with (list (tcmd--call '(text . "OLD") (cons 'kind kind)
                                    '(at . "2026-05-01 09:00") '(repeat . "daily"))
                        '(:text "finished"))
        (p)
      (hm--turn (tcmd--view p) "every day")
      (should-not (superchat-timer-list))
      (should (string-match-p "passed.*it is now" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/a-time-of-day-is-the-next-time-it-is-that-time ()
  (wtimer--with nil (_p)
    (let* ((now (encode-time (list 0 30 10 15 6 2030 nil -1 nil)))
           (wtimer--now (float-time now))
           (at (lambda (hour minute day)
                 (float-time (encode-time (list 0 minute hour day 6 2030 nil -1 nil))))))
      ;; Still to come today, or tomorrow's.
      (should (= (funcall at 11 0 15) (superchat-timer--next-time-of-day "11:00")))
      (should (= (funcall at 9 5 16) (superchat-timer--next-time-of-day " 9:05 ")))
      ;; This very minute has come: the next one is tomorrow's.
      (should (= (funcall at 10 30 16) (superchat-timer--next-time-of-day "10:30")))
      (should (= (funcall at 23 59 15) (superchat-timer--next-time-of-day "23:59")))
      (should-error (superchat-timer--next-time-of-day "24:00"))
      (should-error (superchat-timer--next-time-of-day "09:60"))
      ;; A month's end.
      (let ((wtimer--now (float-time (encode-time (list 0 0 12 30 6 2030 nil -1 nil)))))
        (should (equal "2030-07-01 09:00"
                       (superchat-timer-format-time
                        (superchat-timer--next-time-of-day "09:00"))))))))

(ert-deftest timer-command/every-morning-needs-no-date ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me every morning")
      (tcmd--settled)
      (let ((timer (car (superchat-timer-list))))
        (should timer)
        (should (string-suffix-p " 09:00" (superchat-timer-format-time (plist-get timer :due))))
        (should (> (plist-get timer :due) wtimer--now))))))

(ert-deftest timer-command/tomorrow-at-nine-needs-the-date ()
  ;; Measured 2026-11-01: "tomorrow at 9" came as at 09:00, which is today, 2 runs of 2.
  (dolist (kind '("reminder" "task"))
    (wtimer--with (list (tcmd--call '(text . "MEETING") (cons 'kind kind) '(at . "09:00"))
                        '(:text "finished"))
        (p)
      (hm--turn (tcmd--view p) "tomorrow at 9")
      (should-not (superchat-timer-list))
      (should (string-match-p "give the date too.*it is now" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/a-work-record-s-time-is-as-it-was ()
  (should-error (superchat-timer-parse-time "09:00"))
  (should-error (superchat-timer-parse-time "tomorrow"))
  (should (numberp (superchat-timer-parse-time "2030-07-01 09:00"))))

(ert-deftest timer-command/a-first-run-that-fails-sets-nothing-and-says-why ()
  ;; Measured 2026-11-01: the model said all was set over a script without the
  ;; execute bit, 2 runs of 5, and the check would have failed every morning.
  (wtimer--with (list (tcmd--call '(text . "WRONG") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "echo NOPE >&2; exit 3"))
                      (tcmd--call '(text . "RIGHT") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "echo FINE"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me")
      (tcmd--settled)
      (should (= 2 (length tcmd--asked)))
      (let ((timers (superchat-timer-list))
            (wire (prin1-to-string hm--wires)))
        (should (= 1 (length timers)))
        (should (equal "RIGHT" (plist-get (car timers) :text)))
        (should (string-match-p "first run failed: exit 3: NOPE.*Nothing was set" wire))
        (should (string-match-p "ran once just now and worked" wire))
        (should-not (= 0 (hash-table-count superchat-timer--armed)))
        (should (= 1 (hash-table-count superchat-timer--armed)))))))

(ert-deftest timer-command/what-it-would-have-replaced-stays-when-it-fails ()
  (wtimer--with (list (tcmd--call '(text . "AGAIN") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "test -f broken && exit 3; echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let* ((view (tcmd--view p))
             (old (tcmd--set (hm--session view) "OLD"
                             "test -f broken && exit 3; echo HI" 86400)))
        (wtimer--write p "broken" "")
        (hm--turn view "tell me")
        (tcmd--settled)
        (let ((timers (superchat-timer-list)))
          (should (= 1 (length timers)))
          (should (equal (plist-get old :id) (plist-get (car timers) :id))))))))

(ert-deftest timer-command/one-slow-to-finish-is-kept-and-said-later ()
  (wtimer--with (list (tcmd--call '(text . "SLOW") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "sleep 0.4; exit 3"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (let ((superchat-timer-command-first-wait 0.05))
        (hm--turn (tcmd--view p) "tell me")
        (should (string-match-p "running once now" (prin1-to-string hm--wires)))
        (tcmd--settled)
        (should (= 1 (length (superchat-timer-list))))
        (should (string-match-p "SLOW.*failed.*exit 3" (tcmd--last-line-text)))))))

(ert-deftest timer-command/the-model-is-not-told-what-it-printed ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(at . "09:00") '(repeat . "daily")
                                  '(command . "cat value.txt"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (tcmd--counter p "SECRET-VALUE\n")
      (hm--turn (tcmd--view p) "tell me")
      (tcmd--settled)
      (should (string-match-p "SECRET-VALUE" (tcmd--last-line-text)))
      (should-not (string-match-p "SECRET-VALUE" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/one-that-runs-once-does-not-wait-or-run ()
  (wtimer--with (list (tcmd--call '(text . "LATER") '(kind . "command")
                                  '(in_minutes . 60) '(command . "exit 3"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "later")
      (should (= 1 (length (superchat-timer-list))))
      (should-not (tcmd--results)))))

(ert-deftest timer-command/the-answer-says-how-to-cancel-any-timer ()
  ;; Measured 2026-11-01: a model set the wrong timer, saw it, set the right one, and
  ;; the wrong one went on.  It has no tool for that by default: asked to cancel one,
  ;; it searched through Lisp, 3 questions, and 1 run of 3 did not find the way.
  (dolist (fields `(((text . "STARS") (kind . "command") (at . "09:00") (repeat . "daily")
                     (command . "echo HI"))
                    ((text . "MEETING") (kind . "reminder") (in_minutes . 60))
                    ((text . "A TASK") (kind . "task") (in_minutes . 60))))
    (wtimer--with (list (apply #'tcmd--call fields) '(:text "finished")) (p)
      (tcmd--asking t
        (hm--turn (tcmd--view p) "tell me")
        (tcmd--settled)
        (let ((timer (car (superchat-timer-list))))
          (should timer)
          (should (string-match-p (format "superchat-timer-cancel .*%s" (plist-get timer :id))
                                  (prin1-to-string hm--wires))))))))

(ert-deftest timer-command/setting-the-same-command-again-replaces-it ()
  (wtimer--with (list (tcmd--call '(text . "FIRST") '(kind . "command")
                                  '(in_minutes . 60) '(repeat . "daily")
                                  '(command . "echo SAME"))
                      (tcmd--call '(text . "SECOND") '(kind . "command")
                                  '(in_minutes . 120) '(repeat . "daily")
                                  '(command . "echo SAME"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me, later")
      (tcmd--settled)
      (should (= 2 (length tcmd--asked)))
      (should (string-match-p "replaces" (plist-get (car tcmd--asked) :reason)))
      (should-not (string-match-p "replaces" (plist-get (cadr tcmd--asked) :reason)))
      (let ((timers (superchat-timer-list)))
        (should (= 1 (length timers)))
        (should (equal "SECOND" (plist-get (car timers) :text))))
      (should (string-match-p "replacing" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/another-command-or-workspace-is-not-replaced ()
  (wtimer--with (list (tcmd--call '(text . "THIS") '(kind . "command")
                                  '(in_minutes . 60) '(repeat . "daily")
                                  '(command . "echo THIS"))
                      '(:text "finished"))
      (p q)
    (let* ((view (tcmd--view p))
           (other (tcmd--view q)))
      (tcmd--set (hm--session view) "ANOTHER" "echo ANOTHER" 86400)
      (tcmd--set (hm--session other) "ELSEWHERE" "echo THIS" 86400)
      (tcmd--asking t
        (hm--turn view "tell me")
        (tcmd--settled))
      (should (= 3 (length (superchat-timer-list)))))))

(ert-deftest timer-command/the-question-is-all-there-in-a-narrow-window ()
  ;; Measured 2026-11-01: a View truncates its lines, and the command was cut off
  ;; at the window's edge, past where anyone reads it.
  (let ((command (concat "gh api repos/yibie/superchat --jq .stargazers_count; "
                         "echo TAIL-" (make-string 120 ?x) "-END")))
    (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                    '(in_minutes . 60) (cons 'command command))
                        '(:text "finished"))
        (p)
      (let* ((view (tcmd--view p))
             (superchat-approval-function #'superchat-approval-publish)
             (shown nil))
        (run-at-time 0.2 nil
                     (lambda ()
                       (setq shown (tcmd--as-shown view))
                       (superchat-approval-answer
                        (plist-get (car (superchat-approval-open-questions)) :handle) t)))
        (hm--turn view "tell me")
        ;; Only the panel: the transcript's tool call says the arguments as well.
        (should (string-match "Approval needed\n\\(\\(?:.\\|\n\\)*?\\)Waiting for your answer"
                              shown))
        (let ((panel (match-string 1 shown))
              (width (window-body-width (or (get-buffer-window view t) (selected-window)))))
          (should (string-match-p "\\`Allow `timer_create' once\\?\n" panel))
          ;; Put back together, nothing is missing.
          (should (string-match-p (regexp-quote command)
                                  (replace-regexp-in-string "\n" "" panel)))
          (dolist (line (split-string panel "\n"))
            (should (<= (string-width line) width))))))))

(ert-deftest timer-command/the-question-does-not-let-org-hide-part-of-a-command ()
  ;; A View is Org, and Org shows [[a][b]] as "b".  This question is the one time
  ;; anyone reads the command, and all of the first part would run.
  (let ((command "echo stars [[x; echo HIDDEN-PART; echo ][ok]]"))
    (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                    '(in_minutes . 60) (cons 'command command))
                        '(:text "finished"))
        (p)
      (let ((panel (tcmd--panel-shown p)))
        (should (string-search "HIDDEN-PART" panel))
        (should (string-search command panel))))))

(ert-deftest timer-command/the-question-shows-a-command-whatever-org-is-set-to ()
  ;; Settings people do use: markers hidden, entities and scripts pretty.
  (let ((command "echo *a* /tmp/b/ =c= ~d~ +e+ _f_ \\vert g_h i^2")
        (org-hide-emphasis-markers t)
        (org-pretty-entities t))
    (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                    '(in_minutes . 60) (cons 'command command))
                        '(:text "finished"))
        (p)
      (should (string-search command (tcmd--panel-shown p))))))

(ert-deftest timer-command/a-question-holds-nothing-org-can-read ()
  (let ((text (superchat--literal-lines "[[a][b]]\n*c*")))
    (should (equal " \n " (substring-no-properties text)))
    (should (equal "[[a][b]]" (get-text-property 0 'display text)))
    (should (equal "*c*" (get-text-property 2 'display text)))))

(ert-deftest timer-command/breaking-a-line-leaves-nothing-out ()
  (dolist (line (list "one two three four five six seven eight nine ten"
                      (make-string 100 ?x)
                      (make-string 40 ?中)
                      "short"
                      "a  b   c    d     e      f       g        h"))
    (let ((pieces (superchat--break-line line 20)))
      (should (equal line (apply #'concat pieces)))
      (dolist (piece pieces) (should (<= (string-width piece) 20))))))

(ert-deftest timer-command/the-echo-area-says-all-of-it-too ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo THE-COMMAND"))
                      '(:text "finished"))
      (p)
    (let* ((view (tcmd--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (said nil))
      (cl-letf* ((announce (symbol-function 'superchat-approval--announce))
                 ((symbol-function 'superchat-approval--announce)
                  (lambda (text) (push text said) (funcall announce text))))
        (run-at-time 0.2 nil
                     (lambda ()
                       (superchat-approval-answer
                        (plist-get (car (superchat-approval-open-questions)) :handle) t)))
        (hm--turn view "tell me"))
      (should (cl-some (lambda (text) (string-match-p "echo THE-COMMAND" text)) said)))))

(ert-deftest timer-command/answering-from-the-list-shows-the-command-first ()
  ;; A row cannot hold it, and `y' there would be the whole of the user's say.
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo THE-COMMAND"))
                      '(:text "finished"))
      (p)
    (let* ((view (tcmd--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (prompts nil) (refused nil) (allowed nil))
      (run-at-time
       0.2 nil
       (lambda ()
         (let ((row (cl-find 'asking (superchat-pending-items)
                             :key (lambda (item) (plist-get item :kind)))))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (prompt) (push prompt prompts) nil)))
             (setq refused (condition-case nil
                               (progn (superchat-pending-allow row) nil)
                             (user-error t))))
           (should (superchat-approval-open-questions))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (prompt) (push prompt prompts) t)))
             (setq allowed (superchat-pending-allow row))))))
      (hm--turn view "tell me")
      (should refused)
      (should allowed)
      (should (= 2 (length prompts)))
      (should (string-match-p "echo THE-COMMAND" (car prompts)))
      (should (superchat-timer-list)))))

(ert-deftest timer-command/refusing-from-the-list-needs-no-reading ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo THE-COMMAND"))
                      '(:text "finished"))
      (p)
    (let* ((view (tcmd--view p))
           (superchat-approval-function #'superchat-approval-publish)
           (prompts nil))
      (run-at-time
       0.2 nil
       (lambda ()
         ;; It answers yes, so that being asked fails the test below: an error
         ;; here is swallowed by the timer and the turn would wait for ever.
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (prompt) (push prompt prompts) t)))
           (superchat-pending-deny
            (cl-find 'asking (superchat-pending-items)
                     :key (lambda (item) (plist-get item :kind)))))))
      (hm--turn view "tell me")
      (should-not prompts)
      (should-not (superchat-timer-list)))))

(ert-deftest timer-command/kind-command-needs-a-command ()
  (wtimer--with (list (tcmd--call '(text . "NOTHING") '(kind . "command")
                                  '(in_minutes . 60))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me")
      (should-not tcmd--asked)
      (should-not (superchat-timer-list)))))

(ert-deftest timer-command/the-tool-body-writes-nothing-unless-it-was-asked ()
  ;; Past the pipeline's question, as a call that bypassed it would be.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (superchat--tool-execution-context (list :session-id (hm--session view)))
           (answer (superchat-timer-tool-create
                    "STARS" "command" 60 nil nil "echo HI")))
      (should (string-match-p "\\`Error" answer))
      (should-not (superchat-timer-list)))))

(ert-deftest timer-command/the-question-only-exists-while-the-row-is-mounted ()
  (wtimer--with (list (tcmd--call '(text . "STARS") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (let ((view (tcmd--view p)))
      (unwind-protect
          (progn
            (superchat-plugin-unmount 'timers)
            (tcmd--asking t
              (hm--turn view "tell me")
              (should-not tcmd--asked)
              (should-not (superchat-timer-list))
              (should (string-match-p "turned off\\|not mounted"
                                      (prin1-to-string hm--wires)))))
        (superchat-plugin-mount 'timers)))))

;;;; The first run ---------------------------------------------------------------

(ert-deftest timer-command/a-repeating-one-runs-at-once-and-says-what-it-printed ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400))
           (lines (tcmd--lines timer)))
      (should (= 1 (length lines)))
      (should (string-match-p "STARS" (plist-get (car lines) :what)))
      (should (equal "71" (plist-get (car lines) :text)))
      ;; It says so where the user is, and the user is here: nothing to count.
      (should (string-match-p "STARS.*71" (own-place--text view)))
      (should (= 0 (tcmd--count)))
      (should (equal "first" (tcmd--status (car (tcmd--results timer))))))))

(ert-deftest timer-command/one-that-runs-once-waits-for-its-time ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "LATER" "echo HI")))
      (should-not (tcmd--results timer))
      (should-not (tcmd--lines timer)))))

(ert-deftest timer-command/a-first-run-that-fails-says-why-at-once ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "BROKEN"
                             "echo NOT-FOUND >&2; exit 127" 86400)))
      (should (= 1 (length (tcmd--lines timer))))
      (should (string-match-p "failed.*127.*NOT-FOUND"
                              (superchat-butler-line-text (car (tcmd--lines timer)))))
      (should (string-match-p "BROKEN" (own-place--text view)))
      (should (= 0 (tcmd--count))))))

;;;; Running at its time -----------------------------------------------------------

(ert-deftest timer-command/it-runs-where-it-was-set ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "WHERE" "pwd > where.txt"))
           (default-directory temporary-file-directory))
      (tcmd--fire timer)
      (should (equal (file-truename (file-name-as-directory p))
                     (file-truename
                      (file-name-as-directory
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name "where.txt" p))
                         (string-trim (buffer-string))))))))))

(ert-deftest timer-command/no-model-no-execution-no-question ()
  (wtimer--with nil (p)
    (tcmd--asking t
      (let* ((view (tcmd--view p))
             (timer (tcmd--set (hm--session view) "STARS"
                               (tcmd--reading p "1\n") 86400)))
        (tcmd--counter p "2\n")
        (tcmd--fire timer)
        (should-not hm--wires)
        (should-not tcmd--asked)
        (should-not (superchat-runtime-active-runs))
        (should (= 0 (tcmd--events-anywhere "run.started")))
        (should (= 0 (tcmd--events-anywhere "approval.requested")))
        (should (= 0 (tcmd--events-anywhere "tool.requested")))))))

(ert-deftest timer-command/it-does-not-hold-emacs-up ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "SLOW" "sleep 3"))
           (before (float-time)))
      (superchat-timer-fire timer (superchat-timer--now))
      (should (< (- (float-time) before) 1))
      (should (= 1 (hash-table-count superchat-timer--running))))))

(ert-deftest timer-command/the-same-output-says-nothing ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400))
           (text (own-place--text view))
           (shown (length (superchat-butler-lines))))
      (tcmd--fire timer)
      (should (= shown (length (superchat-butler-lines))))
      (should (= 0 (tcmd--count)))
      (should (equal text (own-place--text view)))
      (should-not wtimer--told)
      ;; Quiet, not unrecorded.
      (should (= 2 (length (tcmd--results timer))))
      (should (equal "unchanged" (tcmd--status (cadr (tcmd--results timer))))))))

(ert-deftest timer-command/a-change-is-one-line-everywhere ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400)))
      (tcmd--counter p "74\n")
      (tcmd--fire timer)
      (let ((line (car (last (tcmd--lines timer)))))
        (should (= 2 (length (tcmd--lines timer))))
        (should (string-match-p "STARS.*changed.*74.*71" (superchat-butler-line-text line)))
        (should (= 1 (tcmd--count)))
        (should (string-match-p "STARS.*changed.*74.*71" (own-place--joined view)))
        (let ((row (cl-find (plist-get line :run) (superchat-pending-items)
                            :key (lambda (item) (plist-get item :key))
                            :test #'equal)))
          (should row)
          (should (eq 'unseen (plist-get row :kind)))
          (should (string-match-p "STARS" (plist-get row :what)))
          (should (string-match-p "74" (plist-get row :detail))))))))

(ert-deftest timer-command/the-comparison-is-with-the-last-output ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "1\n") 86400)))
      (dolist (value '("2\n" "2\n" "3\n" "1\n"))
        (tcmd--counter p value)
        (tcmd--fire timer))
      (should (equal '("first" "changed" "unchanged" "changed" "changed")
                     (mapcar #'tcmd--status (tcmd--results timer))))
      (should (= 4 (length (tcmd--lines timer)))))))

(ert-deftest timer-command/trailing-blanks-are-not-a-change ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400)))
      (tcmd--counter p "71\n\n  \n")
      (tcmd--fire timer)
      (should (equal "unchanged" (tcmd--status (car (last (tcmd--results timer)))))))))

(ert-deftest timer-command/what-is-only-on-stderr-is-not-a-change ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             "date +%N >&2; echo STEADY" 86400)))
      (tcmd--fire timer)
      (should (equal "unchanged" (tcmd--status (car (last (tcmd--results timer)))))))))

(ert-deftest timer-command/report-unchanged-says-it-every-time ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "BRIEF"
                             (tcmd--reading p "71\n") 86400 t)))
      (tcmd--fire timer)
      (should (= 2 (length (tcmd--lines timer))))
      (should (string-match-p "no change.*71"
                              (superchat-butler-line-text (car (last (tcmd--lines timer))))))
      (should (= 1 (tcmd--count))))))

(ert-deftest timer-command/one-that-runs-once-always-says ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "ONCE" "echo RESULT")))
      (tcmd--fire timer)
      (should (= 1 (length (tcmd--lines timer))))
      (should (string-match-p "RESULT" (tcmd--last-line-text)))
      (should (= 1 (tcmd--count)))
      (should-not (superchat-timer-list)))))

(ert-deftest timer-command/one-that-runs-once-says-when-it-fails ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "ONCE" "exit 2")))
      (tcmd--fire timer)
      (should (= 1 (length (tcmd--lines timer))))
      (should (string-match-p "failed" (tcmd--last-line-text)))
      (should (= 1 (tcmd--count))))))

(ert-deftest timer-command/a-line-says-what-is-new ()
  (should (equal "74 (was 71)" (superchat-timer--change-words "71" "74")))
  (should (equal "M c.el (+1 more new line)"
                 (superchat-timer--change-words "M a.el\nM b.el"
                                                "M a.el\nM c.el\nM b.el\nM d.el")))
  (should (equal "M c.el"
                 (superchat-timer--change-words "M a.el\nM b.el"
                                                "M a.el\nM b.el\nM c.el")))
  (should (equal "gone: M b.el"
                 (superchat-timer--change-words "M a.el\nM b.el" "M a.el")))
  (should (equal "gone: M b.el (+1 more line)"
                 (superchat-timer--change-words "M a.el\nM b.el\nM c.el" "M a.el")))
  (should (equal "gone: 71" (superchat-timer--change-words "71" "")))
  (should (equal "the same lines in another order"
                 (superchat-timer--change-words "a\nb" "b\na"))))

(ert-deftest timer-command/a-first-line-says-how-much-more-there-is ()
  (should (equal "(no output)" (superchat-timer--first-words "")))
  (should (equal "71" (superchat-timer--first-words "71")))
  (should (equal "M a.el (+2 more lines)"
                 (superchat-timer--first-words "M a.el\nM b.el\nM c.el"))))

;;;; When it fails ---------------------------------------------------------------

(ert-deftest timer-command/one-failure-is-quiet-three-are-one-line ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && { echo LOGGED-OUT >&2; exit 4; }; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400)))
           (shown (length (tcmd--lines timer))))
      (wtimer--write p "broken" "")
      (tcmd--fire timer)
      (tcmd--fire timer)
      (should (= shown (length (tcmd--lines timer))))
      (should (= 0 (tcmd--count)))
      (tcmd--fire timer)
      (should (= (1+ shown) (length (tcmd--lines timer))))
      (should (= 1 (tcmd--count)))
      (should (string-match-p "failed 3 times.*4.*LOGGED-OUT" (tcmd--last-line-text)))
      ;; Said once: a fourth is not said again.
      (tcmd--fire timer)
      (should (= (1+ shown) (length (tcmd--lines timer))))
      (should (= 1 (tcmd--count))))))

(ert-deftest timer-command/when-it-works-again-the-failure-goes-away ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && exit 4; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400))))
      (wtimer--write p "broken" "")
      (dotimes (_ 3) (tcmd--fire timer))
      (should (= 1 (tcmd--count)))
      (delete-file (expand-file-name "broken" p))
      (tcmd--fire timer)
      (should (= 0 (tcmd--count)))
      ;; What failing did not change is still the value it compares with.
      (should (equal "unchanged" (tcmd--status (car (last (tcmd--results timer))))))
      (superchat-changes-notices-reload)
      (should (= 0 (tcmd--count))))))

(ert-deftest timer-command/works-again-with-a-new-value-says-the-change ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && exit 4; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400))))
      (wtimer--write p "broken" "")
      (dotimes (_ 3) (tcmd--fire timer))
      (delete-file (expand-file-name "broken" p))
      (tcmd--counter p "72\n")
      (tcmd--fire timer)
      (should (= 1 (tcmd--count)))
      (should (string-match-p "changed.*72.*71" (tcmd--last-line-text))))))

(ert-deftest timer-command/working-again-forgets-only-the-failure ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && exit 4; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400))))
      (tcmd--counter p "72\n")
      (tcmd--fire timer)
      (should (= 1 (tcmd--count)))
      (wtimer--write p "broken" "")
      (dotimes (_ 3) (tcmd--fire timer))
      (should (= 2 (tcmd--count)))
      (delete-file (expand-file-name "broken" p))
      (tcmd--fire timer)
      (should (= 1 (tcmd--count)))
      (should (eq 'changed (plist-get (car (superchat-changes--own-notices)) :status))))))

(ert-deftest timer-command/one-killed-from-outside-failed ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "KILLED" "kill -9 $$")))
      (tcmd--fire timer)
      (should (equal "failed" (tcmd--status (car (last (tcmd--results timer))))))
      (should (string-match-p "killed" (tcmd--last-line-text))))))

(ert-deftest timer-command/a-failure-in-another-emacs-still-counts ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && exit 4; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400))))
      (wtimer--write p "broken" "")
      (tcmd--fire timer)
      (tcmd--fire timer)
      ;; A restart: what is in memory is gone, the facts are not.
      (clrhash superchat-butler--conclusions)
      (superchat-changes-notices-reload)
      (tcmd--fire timer)
      (should (= 1 (tcmd--count))))))

(ert-deftest timer-command/one-that-runs-too-long-is-stopped ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (superchat-timer-command-seconds 0.2)
           (timer (tcmd--set (hm--session view) "SLOW" "sleep 30")))
      (tcmd--fire timer)
      (let ((result (car (last (tcmd--results timer)))))
        (should (equal "failed" (tcmd--status result)))
        (should (string-match-p "longer than" (plist-get (plist-get result :payload) :why))))
      (should (string-match-p "longer than" (tcmd--last-line-text))))))

(ert-deftest timer-command/one-that-prints-too-much-is-stopped ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (superchat-timer-command-max-output 10000)
           (timer (tcmd--set (hm--session view) "NOISY" "yes")))
      (tcmd--fire timer)
      (let ((result (car (last (tcmd--results timer)))))
        (should (equal "failed" (tcmd--status result)))
        (should (string-match-p "printed more" (plist-get (plist-get result :payload) :why)))))))

(ert-deftest timer-command/a-directory-that-is-gone-is-a-failure ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "ONCE" "echo HI"))
           (gone (make-temp-name (expand-file-name "gone" temporary-file-directory))))
      (setq timer (plist-put timer :directory gone))
      (tcmd--fire timer)
      (should (equal "failed" (tcmd--status (car (last (tcmd--results timer))))))
      (should (= 1 (tcmd--count))))))

(ert-deftest timer-command/one-still-running-is-not-started-again ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (progn (tcmd--counter p "1\n")
                         (tcmd--set (hm--session view) "SLOW"
                                    "sleep 0.5; cat value.txt; echo x >> ran.txt" 86400)))
           (before (length (tcmd--results timer))))
      (superchat-timer-fire timer (+ 100 (superchat-timer--now)))
      (superchat-timer-fire timer (+ 200 (superchat-timer--now)))
      (should (equal "skipped" (tcmd--status (car (last (tcmd--results timer))))))
      (tcmd--settled)
      (should (= (+ 2 before) (length (tcmd--results timer))))
      (should (equal "x\nx\n" (with-temp-buffer
                                (insert-file-contents (expand-file-name "ran.txt" p))
                                (buffer-string)))))))

(ert-deftest timer-command/a-skipped-one-is-no-value-to-compare-with ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (progn (tcmd--counter p "1\n")
                         (tcmd--set (hm--session view) "STEADY" "sleep 0.3; cat value.txt" 86400))))
      (superchat-timer-fire timer (+ 100 (superchat-timer--now)))
      (superchat-timer-fire timer (+ 200 (superchat-timer--now)))
      (tcmd--settled)
      (tcmd--fire timer (+ 300 (superchat-timer--now)))
      (should (= 1 (length (tcmd--lines timer)))))))

(ert-deftest timer-command/a-skipped-one-does-not-end-a-run-of-failures ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "test -f broken && { sleep 0.2; exit 4; }; cat value.txt")
           (timer (progn (tcmd--counter p "71\n")
                         (tcmd--set (hm--session view) "STARS" command 86400)))
           (shown (length (tcmd--lines timer))))
      (wtimer--write p "broken" "")
      (tcmd--fire timer)
      (superchat-timer-fire timer (+ 100 (superchat-timer--now)))
      (superchat-timer-fire timer (+ 200 (superchat-timer--now)))
      (should (equal "skipped" (tcmd--status (car (last (tcmd--results timer))))))
      (tcmd--settled)
      (should (= (1+ shown) (1+ (length (tcmd--lines timer)))))
      (tcmd--fire timer)
      (should (= (1+ shown) (length (tcmd--lines timer)))))))

;;;; Stopping it -----------------------------------------------------------------

(ert-deftest timer-command/cancelling-stops-it-and-records-nothing ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "SLOW"
                             "test -f slow && sleep 30; echo HI" 86400))
           (process nil))
      (should (= 1 (length (tcmd--results timer))))
      (wtimer--write p "slow" "")
      (superchat-timer-fire timer (+ 100 (superchat-timer--now)))
      (setq process (gethash (plist-get timer :id) superchat-timer--running))
      (should (process-live-p process))
      (should (superchat-timer-cancel (plist-get timer :id)))
      (should-not (process-live-p process))
      (tcmd--settled)
      (should (= 1 (length (tcmd--results timer)))))))

(ert-deftest timer-command/turning-the-row-off-stops-it ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "SLOW" "sleep 30"))
           (process nil))
      (superchat-timer-fire timer (superchat-timer--now))
      (setq process (gethash (plist-get timer :id) superchat-timer--running))
      (superchat-timer-disarm-all)
      (should-not (process-live-p process))
      (should (= 0 (hash-table-count superchat-timer--running)))
      (should-not (tcmd--results timer)))))

(ert-deftest timer-command/the-process-leaves-nothing-behind ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (before (length (process-list)))
           (buffers (length (buffer-list)))
           (timer (tcmd--set (hm--session view) "STARS" "echo HI; echo ERR >&2" 86400)))
      (tcmd--fire timer)
      (should (= before (length (process-list))))
      ;; The butler's own buffer and such may have been made; no process buffer.
      (should-not (cl-find-if (lambda (buffer)
                                (string-prefix-p " *superchat command" (buffer-name buffer)))
                              (buffer-list)))
      (should (<= (- (length (buffer-list)) buffers) 2)))))

;; Found by the first test of a run: arming, which runs right after the row is
;; mounted, killed a command set a moment before.
(ert-deftest timer-command/arming-again-leaves-a-running-command-alone ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "SLOW" "sleep 0.3; echo DONE"))
           (process nil))
      (superchat-timer-fire timer (superchat-timer--now))
      (setq process (gethash (plist-get timer :id) superchat-timer--running))
      (superchat-timer-arm-all)
      (should (process-live-p process))
      (tcmd--settled)
      (should (string-match-p "DONE" (tcmd--last-line-text))))))

(ert-deftest timer-command/the-list-can-cancel-it ()
  ;; The user can take back what they allowed without asking the model.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS" "echo HI" 86400))
           (row (cl-find (plist-get timer :id) (superchat-pending-items)
                         :key (lambda (item) (plist-get item :key))
                         :test #'equal)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
        (superchat-pending-stop row)
        (should (superchat-timer-list)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
        (superchat-pending-stop row)
        (should-not (superchat-timer-list))
        (should-not (gethash (plist-get timer :id) superchat-timer--armed))))))

(ert-deftest timer-command/k-on-a-row-that-is-neither-says-so ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p)))
      (ignore view)
      (should-error (superchat-pending-stop (list :kind 'unsaved :key "x"))
                    :type 'user-error))))

;;;; After a restart ---------------------------------------------------------------

(ert-deftest timer-command/one-missed-while-emacs-was-closed-runs-once-and-compares ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400)))
      (tcmd--counter p "72\n")
      ;; Three days later, in a new Emacs.
      (clrhash superchat-butler--conclusions)
      (setq wtimer--now (+ (plist-get timer :due) (* 3 86400)))
      (superchat-timer-catch-up)
      (tcmd--settled)
      (should (= 2 (length (tcmd--results timer))))
      (should (= 1 (length (hm--events superchat-butler-session-id "timer.fired"))))
      (should (string-match-p "changed.*72.*71" (tcmd--last-line-text))))))

(ert-deftest timer-command/a-new-emacs-still-lists-what-changed ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS"
                             (tcmd--reading p "71\n") 86400)))
      (tcmd--counter p "74\n")
      (tcmd--fire timer)
      (let ((shown (length (superchat-butler-lines))))
        (clrhash superchat-butler--conclusions)
        (setq superchat-changes--notices nil)
        (superchat-changes-notices-reload)
        (superchat-butler-catch-up)
        (should (= 1 (tcmd--count)))
        (should (= shown (length (superchat-butler-lines))))
        (should (string-match-p
                 "74" (plist-get (cl-find 'unseen (superchat-pending-items)
                                          :key (lambda (item) (plist-get item :kind)))
                                 :detail)))))))

;;;; It touches nothing -------------------------------------------------------------

(ert-deftest timer-command/the-main-conversation-is-not-touched ()
  (wtimer--with '((:text "AN ANSWER")) (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (timer (tcmd--set session "STARS" (tcmd--reading p "SECRET-71\n") 86400))
           (rows (own-place--rows session)))
      (presence--type view "half a sentence")
      (let ((from-the-end (with-current-buffer view (- (point-max) (point)))))
        (tcmd--counter p "SECRET-74\n")
        (tcmd--fire timer)
        (should (= rows (own-place--rows session)))
        (with-current-buffer view
          (should (string-match-p "half a sentence\\'" (buffer-string)))
          (should (= from-the-end (- (point-max) (point))))
          (delete-region (- (point-max) (length "half a sentence")) (point-max)))
        (hm--turn view "A QUESTION")
        ;; The model is told nothing of it.
        (should-not (string-match-p "SECRET" (prin1-to-string hm--wires)))))))

(ert-deftest timer-command/it-opens-no-conversation-and-stays-out-of-main ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (id (plist-get (own-place--workspace view) :id))
           (timer (tcmd--set session "STARS" (tcmd--reading p "1\n") 86400))
           (sessions (superchat-view-session-ids))
           (views (cl-count-if (lambda (buffer)
                                 (buffer-local-value 'superchat-mode buffer))
                               (buffer-list))))
      (tcmd--counter p "2\n")
      (tcmd--fire timer)
      (should (equal sessions (superchat-view-session-ids)))
      (should (= views (cl-count-if (lambda (buffer)
                                      (buffer-local-value 'superchat-mode buffer))
                                    (buffer-list))))
      (should (equal (list session) (superchat-view-workspace-session-ids id))))))

(ert-deftest timer-command/set-in-an-older-conversation-it-does-not-make-that-one-main ()
  (wtimer--with nil (p)
    (let* ((old (tcmd--view p))
           (timer (tcmd--set (hm--session old) "STARS" (tcmd--reading p "1\n") 86400))
           (new (superchat-workspace-new-conversation (own-place--workspace old)))
           (id (plist-get (own-place--workspace old) :id)))
      (superchat-tape-append-event
       (list :type "run.seen" :session-id (hm--session new) :actor-id "test"
             :summary "newest" :payload (list :run-id "none")))
      (should (equal (hm--session new)
                     (car (superchat-view-workspace-session-ids id))))
      (tcmd--counter p "2\n")
      (tcmd--fire timer)
      (should (equal (hm--session new)
                     (car (superchat-view-workspace-session-ids id)))))))

;;;; Seeing it ---------------------------------------------------------------------

(ert-deftest timer-command/the-list-shows-the-command ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS" "echo THE-COMMAND" 86400))
           (row (cl-find (plist-get timer :id) (superchat-pending-items)
                         :key (lambda (item) (plist-get item :key))
                         :test #'equal)))
      (should row)
      ;; How it has been going comes first: the column cuts what is long.
      (should (string-match-p "next .*echo THE-COMMAND" (plist-get row :detail))))))

(ert-deftest timer-command/the-timer-tools-see-it ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (timer (tcmd--set session "STARS" "echo THE-COMMAND" 86400))
           (superchat--tool-execution-context (list :session-id session)))
      (should (string-match-p "echo THE-COMMAND" (superchat-timer-tool-list)))
      (tcmd--fire timer)
      (should (string-match-p "Fired in the last 24 hours:\n.*command.*STARS"
                              (superchat-timer-tool-list)))
      (should (string-match-p "cancelled"
                              (superchat-timer-tool-cancel (plist-get timer :id)))))))

(ert-deftest timer-command/another-workspace-does-not-see-it ()
  (wtimer--with nil (p q)
    (let* ((view (tcmd--view p))
           (other (tcmd--view q))
           (timer (tcmd--set (hm--session view) "STARS" "echo THE-COMMAND" 86400))
           (superchat--tool-execution-context (list :session-id (hm--session other))))
      (tcmd--fire timer)
      (should-not (string-match-p "STARS" (superchat-timer-tool-list)))
      (should (string-match-p "another workspace"
                              (superchat-timer-tool-cancel (plist-get timer :id)))))))

(ert-deftest timer-command/set-in-no-workspace-it-is-yours ()
  ;; Measured 2026-11-05: a rule set in a temporary conversation could be seen
  ;; only there.  A timer of no project is the user's: every conversation's
  ;; model sees it and can cancel it.
  (wtimer--with nil (p)
    (let* ((in-p (hm--session (tcmd--view p)))
           (default-directory (file-name-as-directory p))
           (temporary (hm--session (superchat--temporary-view)))
           (timer (tcmd--set temporary "STARS" "echo THE-COMMAND" 86400)))
      (should (eq 'you (plist-get (car (superchat-timer-list)) :owner)))
      (dolist (session (list temporary "another-conversation" in-p))
        (should (superchat-timer--within-p session timer)))
      (let ((superchat--tool-execution-context (list :session-id in-p)))
        (should (string-match-p "STARS" (superchat-timer-tool-list)))
        (should (string-match-p "cancelled"
                                (superchat-timer-tool-cancel (plist-get timer :id))))))))

(ert-deftest timer-command/set-in-a-workspace-it-is-the-workspace-s ()
  (wtimer--with nil (p)
    (let* ((timer (tcmd--set (hm--session (tcmd--view p)) "STARS" "echo THE-COMMAND" 86400)))
      (should (eq 'project (plist-get (car (superchat-timer-list)) :owner)))
      (should-not (superchat-timer--within-p "another-conversation" timer)))))

(ert-deftest timer-command/an-old-timer-with-no-owner-keeps-to-its-conversation ()
  ;; Set before timers said whose they are: the guess from where it was set.
  (wtimer--with nil (_p)
    (superchat-tape-append-event
     (list :type "timer.created" :session-id "old-conversation" :actor-id "timers"
           :summary "Reminder set" :payload (list :timer-id "timer-old" :kind "reminder"
                                                  :text "OLD" :due (+ wtimer--now 600.0))))
    (let ((timer (car (superchat-timer-list))))
      (should (equal "timer-old" (plist-get timer :id)))
      (should-not (plist-get timer :owner))
      (should (superchat-timer--within-p "old-conversation" timer))
      (should-not (superchat-timer--within-p "another-conversation" timer)))))

(ert-deftest timer-command/a-temporary-conversation-is-told-a-timer-is-kept ()
  (should (string-match-p "will not be kept" superchat-workspace-temporary-note))
  (should (string-match-p "timer_create is kept" superchat-workspace-temporary-note)))

(defun tcmd--cancel-through-the-model (view id answer)
  "VIEW's model cancels ID, the user answering ANSWER.  Return what was asked.
VIEW must have been made with `timer_cancel' in `hm--extra-tool-names'."
  (setq smoke--script (list `(:tool-uses (("timer_cancel" (id . ,id))))
                            '(:text "finished")))
  (tcmd--asking answer
    (hm--turn view "cancel that timer")
    tcmd--asked))

(ert-deftest timer-command/a-model-cancels-a-timer-only-when-the-user-says-yes ()
  ;; 2026-11-06, the user: a timer a person set is cancelled by a person.
  (let ((hm--extra-tool-names '("timer_list" "timer_cancel")))
    (wtimer--with nil (p)
      (let* ((view (tcmd--view p))
             (timer (superchat-timer-create (hm--session view) 'reminder "CALL-MUM"
                                            (+ wtimer--now 3600)))
             (asked (tcmd--cancel-through-the-model view (plist-get timer :id) t)))
        (should (= 1 (length asked)))
        (should (string-match-p "CALL-MUM" (plist-get (car asked) :reason)))
        (should-not (superchat-timer-list))))))

(ert-deftest timer-command/a-timer-the-user-keeps-is-not-cancelled ()
  (let ((hm--extra-tool-names '("timer_list" "timer_cancel")))
    (wtimer--with nil (p)
      (let* ((view (tcmd--view p))
             (timer (superchat-timer-create (hm--session view) 'reminder "CALL-MUM"
                                            (+ wtimer--now 3600)))
             (asked (tcmd--cancel-through-the-model view (plist-get timer :id) nil)))
        (should (= 1 (length asked)))
        (should (equal (list (plist-get timer :id))
                       (mapcar (lambda (one) (plist-get one :id)) (superchat-timer-list))))))))

(ert-deftest timer-command/a-timer-it-may-not-touch-asks-nobody ()
  ;; Another Workspace's: the tool refuses it, as before; nobody is asked.
  (let ((hm--extra-tool-names '("timer_list" "timer_cancel")))
    (wtimer--with nil (p q)
      (let* ((timer (superchat-timer-create (hm--session (tcmd--view p)) 'reminder "CALL-MUM"
                                            (+ wtimer--now 3600)))
             (asked (tcmd--cancel-through-the-model (tcmd--view q) (plist-get timer :id) t)))
        (should-not asked)
        (should (= 1 (length (superchat-timer-list))))))))

(ert-deftest timer-command/a-task-nobody-sees-cannot-cancel-a-timer ()
  ;; Nobody could say yes: it is refused, not left waiting.
  (let ((hm--extra-tool-names '("timer_list" "timer_cancel")))
    (wtimer--with nil (p)
      (cl-destructuring-bind (view _session task) (presence--task p "TASK-MARKER")
        (let ((keep (superchat-timer-create (hm--session view) 'reminder "CALL-MUM"
                                            (+ wtimer--now 7200))))
          (setq smoke--script (list `(:tool-uses (("timer_cancel" (id . ,(plist-get keep :id)))))
                                    '(:text "TASK-ANSWER")))
          (tcmd--asking t
            (own-place--fire-and-settle task)
            (should-not tcmd--asked))
          (should (cl-find (plist-get keep :id) (superchat-timer-list)
                           :key (lambda (one) (plist-get one :id)) :test #'equal))
          (should (cl-some (lambda (text) (string-match-p "not cancelled" text))
                           (own-place--tool-results 2))))))))

(ert-deftest timer-command/no-such-timer-asks-nobody ()
  (let ((hm--extra-tool-names '("timer_list" "timer_cancel")))
    (wtimer--with nil (p)
      (should-not (tcmd--cancel-through-the-model (tcmd--view p) "timer-no-such" t)))))

(defun tcmd--changed (root)
  "A timer in a new conversation of ROOT whose output changed: (VIEW TIMER LINE)."
  (let* ((view (tcmd--view root))
         (timer (tcmd--set (hm--session view) "STARS"
                           (tcmd--reading root "71\nMORE-LINES\n") 86400)))
    (tcmd--counter root "74\nMORE-LINES\n")
    (tcmd--fire timer)
    (list view timer (car (last (tcmd--lines timer))))))

(defun tcmd--output-shown-p ()
  (let ((buffer (get-buffer superchat-butler-output-buffer-name)))
    (and (buffer-live-p buffer) (get-buffer-window buffer)
         (with-current-buffer buffer
           (and (string-match-p "74\nMORE-LINES" (buffer-string))
                (string-match-p "cat value.txt" (buffer-string))
                t)))))

(ert-deftest timer-command/the-butler-shows-the-output-and-it-counts-as-looked-at ()
  (wtimer--with nil (p)
    (cl-destructuring-bind (_view _timer line) (tcmd--changed p)
      (with-current-buffer (superchat-butler)
        (goto-char (point-min))
        (search-forward "changed")
        (superchat-butler-open))
      (should (tcmd--output-shown-p))
      (should (= 0 (tcmd--count)))
      (superchat-changes-notices-reload)
      (should (= 0 (tcmd--count)))
      (should (plist-get line :run)))))

(ert-deftest timer-command/the-list-shows-the-output-and-it-counts-as-looked-at ()
  (wtimer--with nil (p)
    (cl-destructuring-bind (_view _timer line) (tcmd--changed p)
      (superchat-pending-open
       (cl-find (plist-get line :run) (superchat-pending-items)
                :key (lambda (item) (plist-get item :key))
                :test #'equal))
      (should (tcmd--output-shown-p))
      (should (= 0 (tcmd--count))))))

(ert-deftest timer-command/the-notices-list-shows-the-output-too ()
  (wtimer--with nil (p)
    (cl-destructuring-bind (_view _timer line) (tcmd--changed p)
      (superchat-changes-notices-open (plist-get line :run))
      (should (tcmd--output-shown-p))
      (should (= 0 (tcmd--count))))))

(ert-deftest timer-command/the-list-can-forget-it ()
  (wtimer--with nil (p)
    (cl-destructuring-bind (_view _timer line) (tcmd--changed p)
      (superchat-pending-dismiss
       (cl-find (plist-get line :run) (superchat-pending-items)
                :key (lambda (item) (plist-get item :key))
                :test #'equal))
      (should (= 0 (tcmd--count))))))

(ert-deftest timer-command/a-line-of-the-first-run-shows-its-output ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (_timer (tcmd--set (hm--session view) "STARS" "echo FIRST-OUTPUT" 86400)))
      (with-current-buffer (superchat-butler)
        (goto-char (point-min))
        (search-forward "STARS")
        (superchat-butler-open))
      (with-current-buffer superchat-butler-output-buffer-name
        (should (string-match-p "FIRST-OUTPUT" (buffer-string)))))))

(ert-deftest timer-command/a-long-output-is-cut-and-still-compared-whole ()
  ;; The Tape drops a payload over its limit whole: the value compared with must
  ;; survive that, and what differs past the part kept is still a change.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (command "cat value.txt")
           (long (make-string 30000 ?中))
           (timer (progn (tcmd--counter p (concat long "A\n"))
                         (tcmd--set (hm--session view) "LONG" command 86400)))
           (payload (plist-get (car (tcmd--results timer)) :payload)))
      (should-not (plist-get payload :omitted))
      (should (<= (string-bytes (plist-get payload :output))
                  superchat-timer-command-output-bytes))
      (tcmd--fire timer)
      (should (equal "unchanged" (tcmd--status (car (last (tcmd--results timer))))))
      (tcmd--counter p (concat long "B\n"))
      (tcmd--fire timer)
      (should (equal "changed" (tcmd--status (car (last (tcmd--results timer)))))))))

(ert-deftest timer-command/output-the-tape-would-swell-is-still-kept ()
  ;; Control characters take six bytes each in the JSON the Tape stores.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "CONTROL"
                             "head -c 5000 /dev/zero | tr '\\0' '\\001'" 86400))
           (payload (plist-get (car (tcmd--results timer)) :payload)))
      (should-not (plist-get payload :omitted))
      (should (plist-get payload :hash)))))

(ert-deftest timer-command/with-the-butler-row-off-there-is-no-doorbell ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p)))
      (unwind-protect
          (progn
            (superchat-plugin-unmount 'butler)
            (let ((timer (tcmd--set (hm--session view) "STARS" "echo HI" 86400)))
              (should (tcmd--lines timer))
              (should-not (string-match-p "STARS" (own-place--text view)))))
        (superchat-plugin-mount 'butler)))))

(ert-deftest timer-command/report-unchanged-comes-from-the-tool ()
  (dolist (case '((t . t) (:json-false . nil) (nil . nil) ("false" . nil)))
    (wtimer--with (list (tcmd--call '(text . "BRIEF") '(kind . "command")
                                    '(in_minutes . 60) '(repeat . "daily")
                                    '(command . "echo HI")
                                    (cons 'report_unchanged (car case)))
                        '(:text "finished"))
        (p)
      (tcmd--asking t
        (hm--turn (tcmd--view p) "brief me")
        (tcmd--settled)
        (let ((timer (car (superchat-timer-list))))
          (should (equal (and (cdr case) t) (plist-get timer :report-unchanged)))
          (should (string-match-p (if (cdr case) "each time" "only when")
                                  (prin1-to-string hm--wires))))))))

(ert-deftest timer-command/the-question-says-when-one-runs-once ()
  (wtimer--with (list (tcmd--call '(text . "ONCE") '(kind . "command")
                                  '(in_minutes . 60) '(command . "echo HI"))
                      '(:text "finished"))
      (p)
    (tcmd--asking t
      (hm--turn (tcmd--view p) "tell me later")
      (should (string-match-p "once at" (plist-get (car tcmd--asked) :reason)))
      (should (string-match-p "only\\|result then" (prin1-to-string hm--wires))))))

(ert-deftest timer-command/set-in-no-workspace-it-runs-where-the-conversation-is ()
  (wtimer--with nil (p)
    (let* ((default-directory (file-name-as-directory p))
           (view (superchat--temporary-view))
           (session (hm--session view))
           (default-directory temporary-file-directory)
           (timer (tcmd--set session "HERE" "pwd > where.txt")))
      (should (null (plist-get timer :workspace-root)))
      (should (equal (expand-file-name (file-name-as-directory p))
                     (expand-file-name (plist-get timer :directory))))
      (tcmd--fire timer)
      (should (file-exists-p (expand-file-name "where.txt" p))))))

(ert-deftest timer-command/quiet-runs-do-not-push-lines-out-of-the-butler ()
  ;; The facts of a quiet run lie on the same Session as the lines.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS" "echo STEADY" 86400))
           (superchat-butler-lines-shown 3))
      (dotimes (_ 5) (tcmd--fire timer))
      (should (= 1 (length (superchat-butler-lines)))))))

(ert-deftest timer-command/a-long-line-stays-one-line-in-the-butler ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "LONG"
                             (tcmd--reading p (concat (make-string 500 ?x) "\n")) 86400))
           (line (car (tcmd--lines timer))))
      (should (<= (length (plist-get line :text)) superchat-butler-text-width)))))

(ert-deftest timer-command/colours-do-not-reach-a-line ()
  (should (equal "green" (superchat-timer--first-words
                          (concat (string 27) "[32mgreen" (string 27) "[0m")))))

(ert-deftest timer-command/a-stored-command-is-the-approved-one ()
  ;; It would be lost whole if the Tape dropped the payload: refuse, do not forget.
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (superchat-tape-max-inline-bytes 100))
      (should-error (superchat-timer-create (hm--session view) 'command
                                            (make-string 200 ?x)
                                            (+ (superchat-timer--now) 3600) nil
                                            "echo HI"))
      (should-not (superchat-timer-list)))))

(provide 'timer-command-regression)
;;; timer-command-regression.el ends here
