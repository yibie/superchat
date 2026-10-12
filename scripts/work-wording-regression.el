;;; work-wording-regression.el --- the user is not shown rounds -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 7c (the user, 2026-10-12: "我还是不理解为何我们这里有一个
;; work_round 的概念").  A round is how the work is driven, not something
;; the user asked about.  What the user reads says the work goes on, waits,
;; is done or stopped.  How often Superchat went on with a record is shown in
;; one place, the record's own detail, as "Tries: N/LIMIT".
;;
;; The model's side is unchanged: its prompt still says "round N/LIMIT".
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/work-wording-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-wording/")'

;;; Code:

(load (expand-file-name "work-surface-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(declare-function superchat-work-list-show "superchat-work" (id))
(defvar superchat--commands-with-descriptions)

(defvar superchat-work-round-idle-limit)

(defun wword--idle (n)
  "N model answers that do nothing but talk, announcing nothing."
  (cl-loop for i from 1 to n
           collect (list :text (format "WW-IDLE-%d: everything was already done." i))))

(defun wword--no-round (text where)
  "Fail when TEXT, read by the user at WHERE, speaks of a round.
The name of the option that sets the limit is a name, not prose."
  (let ((case-fold-search t)
        (prose (replace-regexp-in-string "superchat-work-round-[a-z-]+" "" (or text ""))))
    (when (string-match "round" prose)
      (ert-fail (list 'the-user-is-shown-a-round where
                      (substring prose (max 0 (- (match-beginning 0) 40))
                                 (min (length prose) (+ (match-end 0) 40))))))))

(defun wword--rows ()
  "Everything the pending list would print, one string."
  (mapconcat (lambda (item)
               (format "%s | %s" (plist-get item :what) (plist-get item :detail)))
             (superchat-pending-items) "\n"))

(defun wword--shown (view)
  (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max))))

(defun wword--detail (id)
  "What `superchat-work-list-show' shows of the record ID."
  (save-window-excursion
    (superchat-work-list-show id)
    (with-current-buffer "*superchat work record*" (buffer-string))))

(ert-deftest work-wording/nothing-the-user-reads-speaks-of-a-round ()
  "The conversation, the list while it runs and after, every answer of /work,
and what the two ends say: at the limit, and after rounds that got nowhere."
  (pend--fresh
    ;; The first time Superchat goes on is slow, so the list can be read
    ;; while it runs.
    (wrec--with (append (list '(:text "The first items are ticked.")
                              '(:text "WW-SLOW: everything was already done." :delay 2))
                        (wword--idle 8))
        (p)
      (let* ((superchat-work-round-limit 3)
             (superchat-work-round-idle-limit nil)
             (view (wrec--view p)) (session (hm--session view))
             echoes)
        ;; /work GOAL, then the list while Superchat goes on by itself.
        (push (wsurf--echo view (concat "/work " gr--goal)) echoes)
        (gr--round-run session)
        (wword--no-round (wword--rows) 'list-while-going-on)
        (should (string-match-p "going on with work record" (wword--rows)))
        (push (wsurf--echo view "/work") echoes)
        (gr--quiet)
        ;; The limit ended it: the record waits, and says why without rounds.
        (let ((id (gr--record)))
          (should (= 3 (gr--count session "work.round")))
          (wword--no-round (wword--rows) 'list-at-the-limit)
          (should (string-match-p "stopped after too many tries" (wword--rows)))
          (push (wsurf--echo view "/work") echoes)
          (push (wsurf--echo view "/work pause") echoes)
          ;; Cleared and resumed, with the brake on: it ends for no progress.
          (superchat-work-wait session id nil nil)
          (let ((superchat-work-round-limit 9)
                (superchat-work-round-idle-limit 2))
            (push (wsurf--echo view "/work resume") echoes)
            (gr--quiet)
            (wword--no-round (wword--rows) 'list-after-the-brake)
            (should (string-match-p "stopped: no progress" (wword--rows)))
            (push (wsurf--echo view "/work") echoes)))
        (wword--no-round (wword--shown view) 'conversation)
        (should (string-match-p "Superchat goes on with work record" (wword--shown view)))
        (should (string-match-p "Superchat stopped working on work record"
                                (wword--shown view)))
        (dolist (echo echoes) (wword--no-round echo 'work-command))
        ;; What /work GOAL itself said: the oldest of the echoes.
        (should (string-match-p
                 "keeps working on it until it is done or needs you; /work pause"
                 (car (last echoes))))))))

(ert-deftest work-wording/a-paused-record-and-a-refusal-do-not-speak-of-rounds ()
  (pend--fresh
    (wrec--with (list '(:text "The first items are ticked.")
                      '(:text "NEVER-DELIVERED" :delay 5))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)) echoes)
        (wsurf--echo view (concat "/work " gr--goal))
        (let ((run (gr--round-run session)))
          (superchat-runtime-cancel run)
          (gr--quiet))
        (wword--no-round (wword--rows) 'list-when-paused)
        (should (string-match-p "\\(^\\| \\)paused" (wword--rows)))
        (push (wsurf--echo view "/work") echoes)
        (wsurf--row-off
          (push (wsurf--echo view "/work resume") echoes)
          (push (wsurf--echo view "/work another goal") echoes))
        (should (cl-some (lambda (echo) (string-match-p "Not resumed" echo)) echoes))
        (dolist (echo echoes) (wword--no-round echo 'work-command))
        (wword--no-round (wword--shown view) 'conversation)))))

(ert-deftest work-wording/the-commands-description-does-not-speak-of-rounds ()
  (let ((described (with-temp-buffer
                     (insert-file-contents (locate-library "superchat.el"))
                     (goto-char (point-min))
                     (and (re-search-forward "(\"work\" \\. \"\\([^\"]*\\)\")" nil t)
                          (match-string 1)))))
    (should (stringp described))
    (wword--no-round described 'command-table)
    (should (string-match-p "keeps working on" described))))

(ert-deftest work-wording/the-records-detail-says-how-many-tries ()
  "The one place the count is shown; the model's prompt keeps its round."
  (pend--fresh
    (wrec--with (list '(:text "The first items are ticked.")
                      '(:text "Ten more are ticked.")
                      (wrec--call '(action . "done") '(conclusion . "WW-DONE"))
                      '(:text "All ticked."))
        (p)
      (let* ((superchat-work-round-idle-limit nil)
             (view (wrec--view p)) (session (hm--session view)))
        (wsurf--echo view (concat "/work " gr--goal))
        (gr--quiet)
        (let ((id (gr--record)))
          (should (= 2 (gr--count session "work.round")))
          (should (string-match-p "^Tries: 2/256$" (wword--detail id)))
          (wword--no-round (wword--detail id) 'record-detail)
          ;; The model is still told which round it is in, and the limit.
          (should (string-match-p "round 2/256" (wrec--user-text 3))))))))

(ert-deftest work-wording/a-record-superchat-never-went-on-with-shows-no-tries ()
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session "WW-GOAL")))
        (should-not (string-match-p "Tries" (wword--detail id)))))))

(ert-deftest work-wording/a-record-superchat-goes-on-with-is-listed-as-going-on ()
  "Between two executions the record's own row says so, without a count."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session "WW-GOAL")))
        (superchat-work--append session "work.round" "it went on once"
                                (list :work-id id :round 1 :limit 256))
        (superchat-work-continue id)
        (wword--no-round (wword--rows) 'list-between-executions)
        (should (string-match-p "| going on\\(;\\|$\\)" (wword--rows)))))))

(provide 'work-wording-regression)
;;; work-wording-regression.el ends here
