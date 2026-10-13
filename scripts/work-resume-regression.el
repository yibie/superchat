;;; work-resume-regression.el --- /work resume after Superchat stopped -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 8a (architecture map of 2026-10-13, contradiction C11).  When the
;; work on a record gets nowhere, Superchat stops, has the record wait for
;; the user, and says "/work resume goes on".  `/work resume' then refused
;; the record, because a record that waits is not one to go on with -- and a
;; slash command does not run the hook by which an ordinary message clears a
;; wait for the user.  The instruction the user was given did not work.
;;
;; `/work resume' is the user's answer.  On a record that waits for the user
;; it clears the wait, with the same fact an ordinary message writes, and
;; goes on.  A record that is done, or that waits for a time or for another
;; agent, is refused as before.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/work-resume-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-resume/")'

;;; Code:

(load (expand-file-name "round-brake-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(declare-function superchat-work--cmd "superchat-work")
(declare-function superchat-work--on-user-input "superchat-work")

(defun wres--resume (view)
  "Run `/work resume' in VIEW as the command does; return what it echoes."
  (plist-get (with-current-buffer view (superchat-work--cmd nil "resume" nil nil nil))
             :content))

(defun wres--clearing (session)
  "The newest `work.waiting' fact of SESSION, without what differs by moment."
  (let ((event (car (last (hm--events session "work.waiting")))))
    (list :text (plist-get event :text)
          :payload (plist-get event :payload))))

(defun wres--stopped (view session)
  "Bring the record of VIEW to where the brake stopped it; return its id."
  (gr--first-turn view)
  (gr--quiet)
  (let ((id (gr--record)))
    (should (= 2 (gr--count session "work.round")))
    (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
    id))

(ert-deftest work-resume/after-the-brake-the-command-goes-on ()
  "What the brake's own message tells the user to type works."
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 6))
        (p)
      (let* ((superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view))
             (id (wres--stopped view session)))
        ;; The conversation told the user to do exactly this.
        (should (string-match-p "/work resume goes on"
                                (with-current-buffer view (buffer-string))))
        (let ((echo (wres--resume view)))
          (should (string-match-p "\\`Superchat goes on with " echo)))
        (gr--quiet)
        ;; It went on: two more tries, and the brake stopped it again.
        (should (= 4 (gr--count session "work.round")))
        (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))))))

(ert-deftest work-resume/the-command-typed-in-the-conversation-goes-on ()
  "The same through the prompt, where a command skips the user-input hook."
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 6))
        (p)
      (let* ((superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view)))
        (wres--stopped view session)
        (smoke--send view "/work resume")
        (gr--quiet)
        (should (= 4 (gr--count session "work.round")))))))

(ert-deftest work-resume/the-wait-is-cleared-with-the-fact-a-message-writes ()
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 2))
        (p)
      (let* ((superchat-work-round-limit 5)
             ;; Nothing is queued after the command: the fact it wrote stays
             ;; the newest one.
             (view (wrec--view p)) (session (hm--session view))
             (id (wres--stopped view session))
             by-command by-message)
        (cl-letf (((symbol-function 'superchat-work--queue-round) (lambda (&rest _) t)))
          (wres--resume view))
        (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
        (setq by-command (wres--clearing session))
        ;; The same record waits again, and an ordinary message answers it.
        (superchat-work-pause id)
        (superchat-work-wait session id "user" "stopped: again")
        (superchat-work--on-user-input session "go on")
        (setq by-message (wres--clearing session))
        (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
        (should (plist-get by-message :payload))
        (should (equal by-message by-command))))))

(ert-deftest work-resume/a-record-waiting-for-a-time-is-still-refused ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "The record is started.")) (p)
      (let* ((superchat-work-rounds-when-started nil)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (let ((id (gr--record))
              (waits (gr--count session "work.waiting")))
          (cl-letf (((symbol-function 'superchat-work--wake-active-p) #'ignore))
            (superchat-work--append session "work.waiting" "waits until later"
                                    (list :work-id id :for "time"
                                          :until (+ (float-time) 3600))))
          (should (eq 'waiting-other (superchat-work-status (superchat-work-get id))))
          (should (string-match-p
                   "nothing to go on with"
                   (cadr (should-error (wres--resume view) :type 'user-error))))
          ;; Nothing was cleared and nothing goes on.
          (should (= (1+ waits) (gr--count session "work.waiting")))
          (should (eq 'waiting-other (superchat-work-status (superchat-work-get id))))
          (should-not (superchat-work-continuing-p id)))))))

(ert-deftest work-resume/a-record-waiting-for-another-agent-is-still-refused ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "The record is started.")) (p)
      (let* ((superchat-work-rounds-when-started nil)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (let ((id (gr--record)))
          (superchat-work-wait session id "agent" "the review")
          (should-error (wres--resume view) :type 'user-error)
          (should (eq 'waiting-other (superchat-work-status (superchat-work-get id))))
          (should-not (superchat-work-continuing-p id)))))))

(ert-deftest work-resume/a-completed-record-is-still-refused ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "The record is started.")) (p)
      (let* ((superchat-work-rounds-when-started nil)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (let ((id (gr--record))
              (queued (gr--count session "queue.queued")))
          (superchat-work-wait session id "user" "is it right?")
          (superchat-work-complete session id "WRES-DONE" 'user)
          (should-error (wres--resume view) :type 'user-error)
          (should (eq 'completed (superchat-work-status (superchat-work-get id))))
          (should (= queued (gr--count session "queue.queued"))))))))

(ert-deftest work-resume/at-the-limit-the-wait-stays-and-the-limit-is-named ()
  "Stopped after too many tries: the command cannot go on, so it says what
would let it, and the record keeps saying why it stopped."
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 4))
        (p)
      (let* ((superchat-work-round-idle-limit nil)
             (superchat-work-round-limit 2)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let* ((id (gr--record)) (waits (gr--count session "work.waiting")))
          (should (= 2 (gr--count session "work.round")))
          (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
          (should (string-match-p "superchat-work-round-limit" (wres--resume view)))
          (should (= waits (gr--count session "work.waiting")))
          (should (string-match-p "too many tries"
                                  (or (cdr (rb--waiting id)) "")))
          (should-not (superchat-work-continuing-p id))
          (should (= 2 (gr--count session "work.round"))))))))

(provide 'work-resume-regression)
;;; work-resume-regression.el ends here
