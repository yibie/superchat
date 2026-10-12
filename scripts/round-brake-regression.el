;;; round-brake-regression.el --- rounds that get nowhere stop -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 7b, item 2.  In the user's own Emacs on 2026-10-12 a record's work
;; was finished in its first round and rounds 2 to 5 did nothing; only the
;; user's stop ended them, and the limit was 256.
;;
;; After `superchat-work-round-idle-limit' rounds in a row (2 by default) in
;; which no structured tool changed a buffer and the model wrote nothing on
;; the record, no further round is queued: the record waits for the user,
;; "no progress in N rounds".  DeepSeek Harness has no such brake.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/round-brake-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^round-brake/")'

;;; Code:

(load (expand-file-name "goal-rounds-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-work-round-idle-limit)

(defun rb--waiting (id)
  "The standing wait of the record ID, as (FOR . DETAIL)."
  (let ((waiting (plist-get (superchat-work-get id) :waiting)))
    (and waiting (cons (plist-get waiting :for) (plist-get waiting :detail)))))

(defun rb--idle (n)
  "N model answers that do nothing but talk, announcing nothing."
  (cl-loop for i from 1 to n
           collect (list :text (format "RB-IDLE-%d: everything was already done." i))))

(ert-deftest round-brake/two-rounds-that-change-nothing-end-the-rounds ()
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 6))
        (p)
      ;; A small limit: without the brake the rounds end there, and this
      ;; test fails instead of going on for minutes.
      (let* ((superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let* ((id (gr--record)) (waiting (rb--waiting id)))
          ;; Two rounds ran and the third was not queued.
          (should (= 2 (gr--count session "work.round")))
          (should (equal '("lead" "goal-rounds" "goal-rounds") (gr--actors session)))
          (should-not (superchat-work-continuing-p id))
          ;; The record says why it stopped, and waits for the user.
          (should (equal "user" (format "%s" (car waiting))))
          (should (string-match-p "no progress in 2 rounds" (or (cdr waiting) "")))
          (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
          ;; The conversation says so too.
          (should (string-match-p "no progress in 2 rounds"
                                  (with-current-buffer view (buffer-string)))))))))

(ert-deftest round-brake/a-round-that-edits-a-buffer-is-progress ()
  "Idle, then an edit, then idle: never two idle rounds in a row before the
record is completed."
  (pend--fresh
    (wrec--with nil (p)
      ;; `wrec--view' visits a.txt under P; that buffer is the one edited.
      (let* ((view (wrec--view p)) (session (hm--session view))
             (file (get-file-buffer (expand-file-name "a.txt" p))))
        (should (buffer-live-p file))
        (setq smoke--script
              (list (gr--start) '(:text "The record is started.")
                    '(:text "RB-IDLE-1: nothing to do yet.")
                    (wrec--edit-round (buffer-name file)) '(:text "One item is ticked.")
                    '(:text "RB-IDLE-2: nothing more this round.")
                    (wrec--call '(action . "done") '(conclusion . "RB-DONE"))
                    '(:text "All ticked.")))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 4 (gr--count session "work.round")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id))))
          (should-not (hm--events session "work.waiting")))))))

(ert-deftest round-brake/a-round-that-writes-on-the-record-is-progress ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "The record is started.")
                      '(:text "RB-IDLE-1: nothing to do yet.")
                      (wrec--call '(action . "update") '(progress . "RB-HALF"))
                      '(:text "Recorded how far it got.")
                      '(:text "RB-IDLE-2: nothing more this round.")
                      (wrec--call '(action . "done") '(conclusion . "RB-DONE"))
                      '(:text "All ticked."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 4 (gr--count session "work.round")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id))))
          (should-not (hm--events session "work.waiting")))))))

(ert-deftest round-brake/the-number-of-rounds-is-the-users ()
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 6))
        (p)
      (let* ((superchat-work-round-idle-limit 3)
             (superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 3 (gr--count session "work.round")))
          (should (string-match-p "no progress in 3 rounds"
                                  (or (cdr (rb--waiting id)) ""))))))))

(ert-deftest round-brake/nil-turns-the-brake-off ()
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 4)
                        (list (wrec--call '(action . "done") '(conclusion . "RB-DONE"))
                              '(:text "All ticked.")))
        (p)
      (let* ((superchat-work-round-idle-limit nil)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 5 (gr--count session "work.round")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest round-brake/resume-after-the-brake-gives-the-same-number-again ()
  "Saying to go on is the user's answer: the idle rounds before it no longer count."
  (pend--fresh
    (wrec--with (append (list (gr--start) '(:text "The record is started."))
                        (rb--idle 6))
        (p)
      (let* ((superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 2 (gr--count session "work.round")))
          ;; The user clears the wait and says to go on.
          (superchat-work-wait session id nil nil)
          (with-current-buffer view (superchat-work--cmd nil "resume" nil nil nil))
          (gr--quiet)
          (should (= 4 (gr--count session "work.round")))
          (should (eq 'waiting-user (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest round-brake/an-edit-saved-before-the-round-ended-is-progress ()
  "What the user's Emacs did on 2026-10-12: the edited buffer was saved by the
user's own configuration before the round ended, so the round left no unsaved
change for the record -- and still it had changed a buffer."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((superchat-work-round-limit 5)
             (view (wrec--view p)) (session (hm--session view))
             (file (get-file-buffer (expand-file-name "a.txt" p))))
        (should (buffer-live-p file))
        ;; Stand in for a save-on-change configuration.
        (with-current-buffer file
          (add-hook 'after-change-functions
                    (lambda (&rest _)
                      (run-at-time 0 nil
                                   (lambda ()
                                     (when (buffer-live-p file)
                                       (with-current-buffer file
                                         (let ((inhibit-message t)) (basic-save-buffer)))))))
                    nil t))
        (setq smoke--script
              (list (gr--start) '(:text "The record is started.")
                    (wrec--edit-round (buffer-name file)) '(:text "One item is ticked.")
                    '(:text "RB-IDLE-1: nothing more this round.")
                    (wrec--call '(action . "done") '(conclusion . "RB-DONE"))
                    '(:text "All ticked.")))
        (gr--first-turn view)
        (gr--quiet)
        (let ((id (gr--record)))
          ;; The edit left nothing unsaved for the record to note ...
          (should-not (buffer-modified-p file))
          ;; ... and the round after it was the first idle one, not the second.
          (should (= 3 (gr--count session "work.round")))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

(ert-deftest round-brake/a-round-that-never-ran-is-not-an-idle-one ()
  "A round queued and then dropped has no execution to judge."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session "RB-GOAL")))
        (superchat-work--append session "work.round" "a round that was dropped"
                                (list :work-id id :round 1 :limit 256))
        (should (= 0 (superchat-work-idle-rounds session id)))))))

(provide 'round-brake-regression)
;;; round-brake-regression.el ends here
