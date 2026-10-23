;;; braked-round-regression.el --- a braked round ends only itself -*- lexical-binding: t; -*-

;;; Commentary:
;; Found in the user's GUI field test of 2026-10-22 (G17, G14).  A record was
;; worked on in rounds for four hours; five times the failure brake ("Stopped
;; after 3 failed calls in a row", slice 11c) ended a round, and each time the
;; round's execution failed, so the rounds stopped until someone typed
;; `/work resume'.  Nothing in the conversation said they had stopped: the
;; last line was the brake's own.
;;
;; The user's ruling (2026-10-22, option 2): the brake stops the round it is
;; in, not the work.  A round the brake ended is followed by the next one;
;; rounds that get nowhere still end at `superchat-work-round-idle-limit'.
;; Whenever the rounds do stop without the record being done, the
;; conversation says so and names `/work resume'.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/braked-round-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^braked-round/")'

;;; Code:

(load (expand-file-name "round-brake-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-tool-failure-brake)

(defun br--fail (buffer-name)
  "A model round whose one call fails: the text to replace is not there."
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . "BR-ABSENT") (new_string . "x")))))

(defun br--braked (buffer-name)
  "The three failing rounds after which the brake ends the execution."
  (make-list superchat-tool-failure-brake (br--fail buffer-name)))

(defun br--text (view)
  (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max))))

(defmacro br--with (script-fn &rest body)
  "BODY with VIEW, SESSION and FILE; SCRIPT-FN, given FILE's buffer name,
returns the model's script."
  (declare (indent 1) (debug t))
  `(pend--fresh
     (wrec--with nil (p)
       (let* ((superchat-work-round-limit 6)
              (view (wrec--view p)) (session (hm--session view))
              (file (get-file-buffer (expand-file-name "a.txt" p))))
         (should (buffer-live-p file))
         (setq smoke--script (funcall ,script-fn (buffer-name file)))
         (gr--first-turn view)
         (gr--quiet)
         ,@body))))

(ert-deftest braked-round/the-brake-ends-the-round-not-the-work ()
  (br--with (lambda (b)
              (append (list (gr--start) '(:text "The record is started."))
                      (br--braked b)
                      (list (wrec--call '(action . "done") '(conclusion . "BR-DONE"))
                            '(:text "All ticked."))))
    (let ((id (gr--record)))
      ;; The brake ended round 1, and round 2 followed by itself.
      (should (= 1 (gr--count session "run.failed")))
      (should (string-match-p "Stopped after 3 failed calls in a row" (br--text view)))
      (should (= 2 (gr--count session "work.round")))
      (should (equal '("lead" "goal-rounds" "goal-rounds") (gr--actors session)))
      (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))

(ert-deftest braked-round/braked-rounds-that-change-nothing-still-end ()
  "Guard: a braked round changed nothing, so two in a row are rounds that get
nowhere, and the rounds end there, not at the limit."
  (br--with (lambda (b)
              (append (list (gr--start) '(:text "The record is started."))
                      (br--braked b) (br--braked b) (br--braked b)
                      (list '(:text "BR-NEVER"))))
    (let ((id (gr--record)))
      (should (= 2 (gr--count session "work.round")))
      (should-not (superchat-work-continuing-p id))
      (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
      (should (string-match-p "Superchat stopped working on work record .*no progress"
                              (br--text view))))))

(ert-deftest braked-round/a-failed-round-says-the-work-paused ()
  "A round that fails for another reason still pauses the rounds, and the
conversation says so and how to go on."
  (br--with (lambda (_b)
              (list (gr--start) '(:text "The record is started.")
                    '(:error (error . "BR_PROVIDER_DOWN"))))
    (let ((id (gr--record)))
      (should (= 1 (gr--count session "work.round")))
      (should-not (superchat-work-continuing-p id))
      (should (string-match-p
               (format "Superchat paused work record %s: .*failed.*/work resume goes on"
                       (regexp-quote id))
               (br--text view))))))

(ert-deftest braked-round/a-round-that-waits-says-how-to-go-on ()
  "Answering the wait clears it and does not open the rounds again, so the
line says that `/work resume' does."
  (br--with (lambda (_b)
              (list (gr--start) '(:text "The record is started.")
                    (wrec--call '(action . "wait") '(wait_for . "user")
                                '(detail . "BR-WHICH-RULE"))
                    '(:text "I need the rule.")))
    (let ((id (gr--record)))
      (should (= 1 (gr--count session "work.round")))
      (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
      (should (string-match-p
               (format "Superchat paused work record %s: it waits for you: BR-WHICH-RULE\\. Once answered, /work resume goes on"
                       (regexp-quote id))
               (br--text view))))))

(ert-deftest braked-round/a-completed-record-says-nothing-more ()
  "Guard: the record is done, so no line about going on."
  (br--with (lambda (_b)
              (list (gr--start) '(:text "The record is started.")
                    (wrec--call '(action . "done") '(conclusion . "BR-DONE"))
                    '(:text "All ticked.")))
    (should-not (string-match-p "Superchat paused work record" (br--text view)))))

(provide 'braked-round-regression)

;;; braked-round-regression.el ends here
