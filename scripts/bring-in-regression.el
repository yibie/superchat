;;; bring-in-regression.el --- a conclusion brought into your message -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-31-bringing-a-conclusion-into-your-message.md:
;; the one switch there is between work nobody asked for and the work at hand.  A
;; key puts what such work concluded at the start of the message being written in
;; the conversation in use.  Nothing is sent, and what the user then sends is an
;; ordinary message.  What stands in a quoted line triggers nothing.
;;
;; A batch Emacs has one frame and nobody at the keyboard: "the cursor" here is
;; point in the selected window.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/bring-in-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^bring-in/")'

;;; Code:

(load (expand-file-name "timer-command-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(load (expand-file-name "input-syntax-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst bring--heading "Butler's report (not my words):"
  "How a message with a conclusion in it begins.")

(defconst bring--asking '(:tool-uses (("eval_elisp" (expression . "1"))))
  "A round of the model that asks the user: `eval_elisp' at the default level.")

(defmacro bring--with (script vars &rest body)
  "BODY in the timer fixture, SCRIPT and VARS as there, with nothing to look at yet."
  (declare (indent 2) (debug t))
  `(wtimer--with ,script ,vars
     ;; Each test is a new Emacs: nothing is left from the one before.
     (setq superchat-changes--notices nil)
     (clrhash superchat-butler--conclusions)
     ,@body))

(defvar bring--give-up nil
  "The timer of `bring--give-up-later'; one is enough, and it belongs to one test.")

(defun bring--give-up-later ()
  "Answer whatever is still open a while from now.
A question left open would make its turn wait for ever: a test that went wrong
should fail, not hang."
  (when (timerp bring--give-up)
    (cancel-timer bring--give-up))
  (setq bring--give-up
        (run-at-time 4 nil (lambda ()
                             (dolist (question (superchat-approval-open-questions))
                               (superchat-approval-answer (plist-get question :handle) nil))))))

(defun bring--task (root)
  "A task under ROOT came due and ended: (VIEW RUN HOME-SESSION).
VIEW wraps its lines, so that what is in it can be compared line by line; what a
window that cuts them does is tested by itself."
  (cl-destructuring-bind (view _session timer) (presence--task root "TASK-MARKER")
    (with-current-buffer view (setq truncate-lines nil))
    (let ((fired (own-place--fire-and-settle timer)))
      (list view (plist-get fired :run-id) (plist-get fired :session-id)))))

(defun bring--press-t ()
  "Press `t' where the cursor is."
  (call-interactively (key-binding (kbd "t"))))

(defun bring--from-butler ()
  "In the butler, on its newest line, press `t'."
  (pop-to-buffer (superchat-butler))
  (goto-char (point-max))
  (forward-line -1)
  (bring--press-t))

(defun bring--on-row (key)
  "In the list, go to the row of KEY; that the row is there is part of the test."
  (pop-to-buffer (superchat-pending))
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal key (plist-get (tabulated-list-get-id) :key))))
    (forward-line 1))
  (should (equal key (plist-get (tabulated-list-get-id) :key))))

(defun bring--notice-p (run)
  (cl-find run superchat-changes--notices
           :key (lambda (notice) (plist-get notice :run)) :test #'equal))

(defun bring--user-rows (session)
  "What the user said in SESSION, as the Tape has it."
  (mapcar #'car (superchat-db-tape-select
                 "SELECT content FROM tape WHERE session_id = ? AND kind = 'user'"
                 (list session))))

(defun bring--send (view)
  "Send what VIEW's input area holds and wait for its execution to end."
  (let ((run (with-current-buffer view
               (superchat-send-input)
               superchat--active-run-id)))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
    run))

(defun bring--last-user-text (n)
  "The last message of the Nth request."
  (let ((message (car (last (hm--body (hm--wire n))))))
    (should (equal "user" (alist-get 'role message)))
    (hm--text message)))

(defun bring--button (view)
  "Where the newest `[bring in]' of VIEW is."
  (with-current-buffer view
    (save-excursion
      (goto-char (point-max))
      (search-backward "[bring in]"))))

;;;; The butler -----------------------------------------------------------------------

(ert-deftest bring-in/t-in-the-butler-puts-the-whole-answer-in-your-message ()
  (bring--with '((:text "FIRST-LINE\n\nSECOND-PART")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (let ((input (presence--input view)))
        (should (string-prefix-p bring--heading input))
        (should (string-match-p "^> .*TASK-MARKER.* completed$" input))
        (should (string-match-p "^> FIRST-LINE$" input))
        (should (string-match-p "^>$" input))
        (should (string-match-p "^> SECOND-PART$" input))))))

(ert-deftest bring-in/the-cursor-is-where-you-go-on-writing ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should (eq view (window-buffer (selected-window))))
      (with-current-buffer view
        (should (= (point) (point-max)))
        (let ((last-command-event ?x))
          (self-insert-command 1)))
      (should (string-suffix-p "\n\nx" (presence--input view))))))

(ert-deftest bring-in/nothing-is-sent ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (let ((rows (own-place--rows (hm--session view)))
            (wires (length hm--wires)))
        (bring--from-butler)
        (should (= wires (length hm--wires)))
        (should (= rows (own-place--rows (hm--session view))))
        (should-not (superchat-runtime-active-runs))
        (should-not (bring--user-rows (hm--session view)))))))

(ert-deftest bring-in/a-half-written-message-stays-after-the-quote ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (presence--type view "HALF-A-MESSAGE")
      (bring--from-butler)
      (let ((input (presence--input view)))
        (should (string-prefix-p bring--heading input))
        (should (string-suffix-p "\n\nHALF-A-MESSAGE" input))
        (should (= 1 (how-many "HALF-A-MESSAGE" (point-min) (point-max))))
        (with-current-buffer view
          (should (= (point) (point-max))))))))

(ert-deftest bring-in/it-counts-as-looked-at ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view run home) (bring--task p)
      (should (bring--notice-p run))
      (should superchat-changes-notice-mode-line)
      (bring--from-butler)
      (should-not (bring--notice-p run))
      (should-not superchat-changes-notice-mode-line)
      (should (hm--events home "run.seen"))
      (let ((payload (plist-get (car (hm--events superchat-butler-session-id "butler.brought"))
                                :payload)))
        (should (equal run (plist-get payload :run-id)))
        (should (equal (hm--session view) (plist-get payload :into)))))))

(ert-deftest bring-in/the-line-stays-in-the-butler-and-can-be-brought-again ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (with-current-buffer view
        (let ((inhibit-read-only t))
          (delete-region superchat--prompt-start (point-max))))
      (bring--from-butler)
      (should (string-match-p "^> FIRST-LINE$" (presence--input view))))))

(ert-deftest bring-in/the-butler-says-which-keys-it-has ()
  (bring--with nil (_p)
    (with-current-buffer (superchat-butler)
      (should (string-match-p "\\bt\\b" (format "%s" header-line-format)))
      (should (string-match-p "RET" (format "%s" header-line-format))))))

(ert-deftest bring-in/the-butler-opens-on-its-newest-line ()
  ;; Seen writing the real run, 2026-11-01: the cursor was on the empty line after the
  ;; last one, so `t' and `RET' said there was no line.
  (bring--with '((:text "ONE") (:text "TWO")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (own-place--fire-and-settle timer)
      (superchat-timer-fire timer (+ (plist-get timer :due) 1))
      (wtimer--settled)
      (superchat-butler)
      (bring--press-t)
      (should (string-match-p "^> TWO$" (presence--input view)))
      (should-not (string-match-p "^> ONE$" (presence--input view))))))

(ert-deftest bring-in/t-on-no-line-does-nothing ()
  (bring--with nil (p)
    (let ((view (tcmd--view p)))
      (pop-to-buffer (superchat-butler))
      (should-error (bring--press-t) :type 'user-error)
      (should (equal "" (presence--input view))))))

;;;; What you send --------------------------------------------------------------------

(ert-deftest bring-in/what-you-send-is-one-ordinary-message ()
  (bring--with '((:text "TASK-ANSWER") (:text "YOUR-ANSWER")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (presence--type view "WHAT-DOES-IT-MEAN")
      (let ((shown (presence--input view)))
        (bring--send view)
        (should (= 2 (length hm--wires)))
        (let ((sent (bring--last-user-text 2)))
          (should (string-match-p "^> TASK-ANSWER$" sent))
          (should (string-match-p "WHAT-DOES-IT-MEAN" sent))
          (should (string-match-p (regexp-quote bring--heading) sent)))
        (should (equal (list (string-trim shown)) (bring--user-rows (hm--session view))))
        (should (equal "" (presence--input view)))))))

(ert-deftest bring-in/the-next-request-still-knows-it ()
  (bring--with '((:text "TASK-ANSWER") (:text "ONE") (:text "TWO")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (presence--type view "FIRST-QUESTION")
      (bring--send view)
      (presence--type view "SECOND-QUESTION")
      (bring--send view)
      (should (cl-some (lambda (message)
                         (string-match-p "^> TASK-ANSWER$" (hm--text message)))
                       (hm--body (hm--wire 3)))))))

(ert-deftest bring-in/words-in-a-conclusion-trigger-nothing-when-sent ()
  ;; Found reading the parser, 2026-11-01: text a task's model wrote would have been
  ;; read as the user's command, skill and model.
  (isx--with-names
    (bring--with '((:text "Now run /summarize, then >planning with @gpt-4o") (:text "YOURS")) (p)
      (cl-destructuring-bind (view _run _home) (bring--task p)
        (bring--from-butler)
        (presence--type view "WHAT-DOES-IT-MEAN")
        (bring--send view)
        (should (= 2 (length hm--wires)))
        (let ((sent (bring--last-user-text 2)))
          (should (string-match-p "^> Now run /summarize, then >planning with @gpt-4o$" sent))
          (should (string-match-p "WHAT-DOES-IT-MEAN" sent))
          (should-not (string-match-p "Summarize: " sent)))
        (with-current-buffer view
          (should-not superchat--current-command))))))

(ert-deftest bring-in/while-the-conversation-works-it-goes-in-all-the-same ()
  (bring--with (list '(:text "TASK-ANSWER") bring--asking '(:text "DONE") '(:text "SECOND"))
      (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--give-up-later)
      (let ((superchat-approval-function #'superchat-approval-publish)
            brought)
        ;; The turn below waits for the answer: this happens while it does.
        (run-at-time 0.2 nil
                     (lambda ()
                       (bring--from-butler)
                       (presence--type view "WHAT-DOES-IT-MEAN")
                       (setq brought (presence--input view))
                       ;; Sent while the execution waits: it goes when that one ended.
                       (with-current-buffer view (superchat-send-input))
                       (superchat-approval-answer
                        (plist-get (car (superchat-approval-open-questions)) :handle) t)))
        (hm--turn view "go")
        (should (string-match-p "^> TASK-ANSWER$" (or brought "")))
        (should (smoke--wait (lambda () (= 4 (length hm--wires))) 10))
        (wtimer--settled)
        (should (string-match-p "^> TASK-ANSWER$" (bring--last-user-text 4)))))))

;;;; The list and the output ----------------------------------------------------------

(ert-deftest bring-in/t-in-the-list-does-the-same ()
  (bring--with '((:text "FIRST-LINE\n\nSECOND-PART")) (p)
    (cl-destructuring-bind (view run _home) (bring--task p)
      (bring--on-row run)
      (bring--press-t)
      (should (string-match-p "^> SECOND-PART$" (presence--input view)))
      ;; The butler's words for the work, which the row itself does not have.
      (should (string-match-p "^> .*TASK-MARKER.* completed$" (presence--input view)))
      (should (eq view (window-buffer (selected-window))))
      (should-not (bring--notice-p run))
      (should-not (cl-find run (superchat-pending-items)
                           :key (lambda (item) (plist-get item :key)) :test #'equal)))))

(ert-deftest bring-in/a-row-with-no-conclusion-says-so ()
  (bring--with nil (p)
    (let ((view (tcmd--view p)))
      (dolist (kind '(running asking ask-user due timer unsaved continuable))
        (should-error (superchat-pending-bring (list :kind kind :key "some-key" :what "x"
                                                     :session (hm--session view)))
                      :type 'user-error))
      (should-error (superchat-pending-bring nil) :type 'user-error)
      (should (equal "" (presence--input view))))))

(ert-deftest bring-in/a-command-brings-what-it-printed ()
  (bring--with nil (p)
    (cl-destructuring-bind (view _timer line) (tcmd--changed p)
      (bring--on-row (plist-get line :run))
      (bring--press-t)
      (let ((input (presence--input view)))
        (should (string-prefix-p bring--heading input))
        (should (string-match-p (concat "^> " (regexp-quote (superchat-butler-line-text line)) "$")
                                input))
        (should (string-match-p "^> \\$ cat value\\.txt$" input))
        (should (string-match-p "^> 74$" input))
        (should (string-match-p "^> MORE-LINES$" input))))))

(ert-deftest bring-in/t-where-a-command-s-output-is-shown ()
  (bring--with nil (p)
    (cl-destructuring-bind (view _timer line) (tcmd--changed p)
      (superchat-butler-show (plist-get line :run))
      (should (tcmd--output-shown-p))
      (select-window (get-buffer-window superchat-butler-output-buffer-name))
      (bring--press-t)
      (should (string-match-p "^> MORE-LINES$" (presence--input view)))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/a-failed-command-brings-what-it-said-on-standard-error ()
  (bring--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "CHECK" (tcmd--reading p "1\n") 86400)))
      ;; Failing three times is one line.
      (let ((line (cl-loop for occurrence from 1 to 3
                           do (superchat-timer--note-result
                               timer (+ (superchat-timer--now) occurrence) nil
                               (list :why "exit 3" :exit 3 :output "" :stderr "ERROR-WORDS\n"))
                           finally return (car (last (tcmd--lines timer))))))
        (should (string-match-p "failed" (superchat-butler-line-text line)))
        (superchat-butler-bring line view)
        (let ((input (presence--input view)))
          (should (string-match-p "^> On standard error:$" input))
          (should (string-match-p "^> ERROR-WORDS$" input)))))))

(ert-deftest bring-in/a-long-output-says-how-long-it-was ()
  (bring--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "LONG" (tcmd--reading p "short\n") 86400)))
      (with-current-buffer view (setq truncate-lines nil))
      (tcmd--counter p (make-string 9000 ?x))
      (tcmd--fire timer)
      (superchat-butler-bring (car (last (tcmd--lines timer))) view)
      (let ((input (presence--input view)))
        (should (string-match-p "^> (the first 3000 of 6000 characters)$" input))
        (should (string-match-p "^> (9000 bytes in all)$" input))))))

(ert-deftest bring-in/an-output-kept-whole-does-not-say-how-long-it-was ()
  (bring--with nil (p)
    (cl-destructuring-bind (view _timer line) (tcmd--changed p)
      (superchat-butler-bring line view)
      (should-not (string-match-p "bytes in all" (presence--input view))))))

;;;; The window you pressed it in -----------------------------------------------------

(ert-deftest bring-in/the-butler-goes-away-when-you-go-on-to-write ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should-not (get-buffer-window superchat-butler-buffer-name))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/the-list-goes-away-when-you-go-on-to-write ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view run _home) (bring--task p)
      (bring--on-row run)
      (bring--press-t)
      (should-not (get-buffer-window superchat-pending-buffer-name))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/the-output-goes-away-when-you-go-on-to-write ()
  (bring--with nil (p)
    (cl-destructuring-bind (view _timer line) (tcmd--changed p)
      (superchat-butler-show (plist-get line :run))
      (select-window (get-buffer-window superchat-butler-output-buffer-name))
      (bring--press-t)
      (should-not (get-buffer-window superchat-butler-output-buffer-name))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/t-where-no-line-of-it-is-on-record-does-nothing ()
  (bring--with nil (p)
    (let ((view (tcmd--view p)))
      (with-temp-buffer
        (setq superchat-butler--output-of "no-such-line")
        (should-error (superchat-butler-bring-output) :type 'user-error))
      (should (equal "" (presence--input view))))))

;;;; Which conversation ---------------------------------------------------------------

(ert-deftest bring-in/the-conversation-you-are-using-wins ()
  (bring--with '((:text "FIRST-LINE")) (p q)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      ;; Another project's conversation shown beside the code.
      (let ((other (with-current-buffer (wtimer--file q) (superchat))))
        (should (eq other (window-buffer (get-buffer-window other))))
        (should-not (get-buffer-window view))
        (bring--from-butler)
        (should (string-match-p "^> FIRST-LINE$" (presence--input other)))
        (should (equal "" (presence--input view)))
        (should (eq other (window-buffer (selected-window))))))))

(ert-deftest bring-in/with-none-shown-the-main-conversation-opens ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (delete-window (get-buffer-window view))
      (should-not (get-buffer-window view))
      (bring--from-butler)
      (should (string-match-p "^> FIRST-LINE$" (presence--input view)))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/a-command-with-none-shown-goes-to-the-main-conversation-too ()
  (bring--with nil (p)
    (cl-destructuring-bind (view _timer line) (tcmd--changed p)
      (delete-window (get-buffer-window view))
      (bring--on-row (plist-get line :run))
      (bring--press-t)
      (should (string-match-p "^> MORE-LINES$" (presence--input view)))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/a-background-conversation-is-not-where-it-goes ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run home) (bring--task p)
      (let ((background (superchat-timer--live-view home)))
        (set-window-buffer (get-buffer-window view) background)
        (should-not (get-buffer-window view))
        (bring--from-butler)
        (should (equal "" (presence--input background)))
        (should (string-match-p "^> FIRST-LINE$" (presence--input view)))
        (should (eq view (window-buffer (selected-window))))))))

(ert-deftest bring-in/with-no-workspace-it-goes-to-the-temporary-conversation ()
  (bring--with '((:text "FIRST-LINE")) (_p)
    (let* ((view (with-current-buffer (get-buffer-create "*no project*")
                   (superchat--temporary-view)))
           (timer (superchat-timer-create (hm--session view) 'task "TASK-MARKER"
                                          (+ wtimer--now 60))))
      (setq wtimer--now (+ wtimer--now 60))
      (own-place--fire-and-settle timer)
      (should-not (get-buffer-window view))
      (bring--from-butler)
      (should (string-match-p "^> FIRST-LINE$" (presence--input view)))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/into-its-own-conversation-nothing-is-quoted ()
  ;; An execution of your own that ended with its conversation closed: the model there
  ;; knows already.
  (bring--with '((:text "YOUR-ANSWER")) (p)
    (let* ((view (tcmd--view p))
           (run (hm--turn view "a question"))
           (session (hm--session view)))
      (delete-window (get-buffer-window view))
      (superchat-pending-bring (list :kind 'unseen :key run :what "x" :detail "completed"
                                     :session session))
      (should (equal "" (presence--input view)))
      (should (eq view (window-buffer (selected-window))))
      (should-not (hm--events superchat-butler-session-id "butler.brought")))))

(ert-deftest bring-in/an-execution-of-your-own-is-quoted-into-another-conversation ()
  (bring--with '((:text "YOUR-ANSWER\n\nMORE-OF-IT")) (p q)
    (let* ((view (tcmd--view p))
           (run (hm--turn view "a question"))
           (other (with-current-buffer (wtimer--file q) (superchat))))
      (superchat-pending-bring (list :kind 'unseen :key run :what "YOUR-WORK"
                                     :detail "completed" :session (hm--session view)))
      (let ((input (presence--input other)))
        (should (string-match-p "^> YOUR-WORK completed$" input))
        (should (string-match-p "^> MORE-OF-IT$" input))))))

(ert-deftest bring-in/in-agent-mode-it-goes-to-the-conversation-s-own-tab ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (let ((superchat--display-style-current 'agent)
            (tabs (length (tab-bar-tabs))))
        (unwind-protect
            (progn
              (delete-window (get-buffer-window view))
              (bring--from-butler)
              (should (string-match-p "^> FIRST-LINE$" (presence--input view)))
              (should (eq view (window-buffer (selected-window))))
              ;; A tab of its own, as agent mode opens any conversation.
              (should (= (1+ tabs) (length (tab-bar-tabs)))))
          (while (> (length (tab-bar-tabs)) tabs)
            (tab-bar-close-tab)))))))

;;;; The doorbell ---------------------------------------------------------------------

(ert-deftest bring-in/the-doorbell-has-a-button-that-brings-it-in ()
  (bring--with '((:text "FIRST-LINE\n\nSECOND-PART")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      ;; The user is in the code.
      (presence--go-to (get-buffer-create "*the code*"))
      (with-current-buffer view
        (let ((button (button-at (bring--button view))))
          (should button)
          (button-activate button)))
      (should (string-match-p "^> SECOND-PART$" (presence--input view)))
      (should (eq view (window-buffer (selected-window))))
      (with-current-buffer view
        (should (= (point) (point-max)))))))

(ert-deftest bring-in/the-button-answers-to-a-key-and-survives-org ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      ;; What redisplay would do; Org takes `keymap' off the text it fontifies.
      (tcmd--as-shown view)
      (with-current-buffer view
        (goto-char (bring--button view))
        (should (button-at (point)))
        (should (eq 'push-button (key-binding (kbd "RET"))))
        (goto-char (point-max))))))

(ert-deftest bring-in/the-button-brings-it-into-the-conversation-it-is-in ()
  (bring--with '((:text "FIRST-LINE")) (p q)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      ;; Another conversation is the one shown; the button belongs to this one.
      (with-current-buffer (wtimer--file q) (superchat))
      (with-current-buffer view
        (button-activate (button-at (bring--button view))))
      (should (string-match-p "^> FIRST-LINE$" (presence--input view)))
      (should (eq view (window-buffer (selected-window)))))))

(ert-deftest bring-in/in-a-very-narrow-window-nothing-of-the-doorbell-is-lost ()
  (bring--with '((:text "FIRST-LINE")) (p)
    (cl-destructuring-bind (view _session timer) (presence--task p "TASK-MARKER")
      (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 20)))
        (own-place--fire-and-settle timer))
      (should (string-match-p "Butler: \\[bring in\\] Scheduled task \"TASK-MARKER\" completed: FIRST-LINE"
                              (own-place--text view))))))

(ert-deftest bring-in/the-doorbell-with-its-button-is-still-on-no-tape ()
  (bring--with '((:text "FIRST-LINE") (:text "YOURS")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (should (string-match-p "\\[bring in\\]" (own-place--text view)))
      (should (zerop (caar (superchat-db-tape-select
                            "SELECT COUNT(*) FROM tape WHERE session_id = ? AND content LIKE '%bring in%'"
                            (list (hm--session view))))))
      (hm--turn view "a question")
      (should-not (cl-some (lambda (message) (string-match-p "bring in" (hm--text message)))
                           (hm--wire 2))))))

;;;; What you read is what goes ---------------------------------------------------------

(defun bring--fits-p (text window)
  "Non-nil when every line of TEXT fits WINDOW."
  (cl-every (lambda (line) (<= (string-width line) (window-body-width window)))
            (split-string text "\n")))

(defconst bring--long-words
  (mapconcat #'identity (make-list 60 "WORD") " ")
  "One long line with places to break it.")

(ert-deftest bring-in/where-the-window-cuts-lines-they-are-broken ()
  ;; Measured 2026-11-01: `* User: Butler's report, quoted (work that ran by itself, not my w$'.
  (bring--with `((:text ,bring--long-words)) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (with-current-buffer view (setq truncate-lines t))
      (bring--from-butler)
      (let ((input (presence--input view)))
        (should (bring--fits-p input (get-buffer-window view)))
        (should (cl-every (lambda (line) (or (string-prefix-p bring--heading line)
                                             (string-empty-p line)
                                             (string-prefix-p ">" line)))
                          (split-string input "\n")))
        ;; Nothing is left out.
        (should (= 60 (with-temp-buffer (insert input) (how-many "WORD" (point-min) (point-max)))))))))

(ert-deftest bring-in/a-long-word-is-broken-to-fit-the-window-too ()
  (bring--with `((:text ,(make-string 500 ?x))) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (with-current-buffer view (setq truncate-lines t))
      (bring--from-butler)
      (let ((input (presence--input view)))
        (should (bring--fits-p input (get-buffer-window view)))
        (should (= 500 (cl-count ?x input)))))))

(ert-deftest bring-in/where-the-window-wraps-lines-they-stay-whole ()
  ;; Breaking can split a long word, an address for one.
  (bring--with `((:text ,bring--long-words)) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (with-current-buffer view (setq truncate-lines nil))
      (bring--from-butler)
      (should (string-match-p (concat "^> " (regexp-quote bring--long-words) "$")
                              (presence--input view))))))

(ert-deftest bring-in/a-conversation-opened-for-it-fits-too ()
  (bring--with `((:text ,bring--long-words)) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (with-current-buffer view (setq truncate-lines t))
      (delete-window (get-buffer-window view))
      (bring--from-butler)
      (should (bring--fits-p (presence--input view) (get-buffer-window view))))))

(ert-deftest bring-in/all-the-doorbell-says-can-be-read-and-its-button-is-in-sight ()
  ;; Measured 2026-11-01: the line was cut at the edge of the window, its button with it.
  (bring--with `((:text ,bring--long-words)) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (with-current-buffer view
        (setq truncate-lines t)
        (let* ((window (get-buffer-window view))
               (button (bring--button view))
               (text (buffer-substring-no-properties
                      (save-excursion (goto-char button) (line-beginning-position))
                      (superchat--response-region--prompt-line-position
                       (superchat--response-region--state-for-owner :pending t)))))
          (should (< (save-excursion (goto-char button) (current-column)) 20))
          (should (bring--fits-p text window))
          (should (= 1 (with-temp-buffer
                         (insert text)
                         (how-many "TASK-MARKER" (point-min) (point-max)))))
          ;; The first line of the answer is 200 wide in a line: the words at its end show.
          (should (string-match-p "WORD…\\|WORD$" text)))))))

(ert-deftest bring-in/org-cannot-hide-what-you-will-send ()
  (bring--with '((:text "before [[HIDDEN-PART][ok]] after")) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should (string-match-p "HIDDEN-PART" (presence--input view)))
      (should (string-match-p "HIDDEN-PART" (tcmd--as-shown view))))))

(ert-deftest bring-in/brackets-that-pile-up-do-not-make-a-link-again ()
  (should-not (string-match-p "\\[\\[" (superchat-butler--quotable "[[[x]] [[[[y")))
  (should (equal "plain" (superchat-butler--quotable "plain"))))

(ert-deftest bring-in/invisible-characters-do-not-come-along ()
  (bring--with `((:text ,(concat "A" (string #x200b) "B" (string #x202e) "C\e[31mD\e[0mE"
                                  (string #x2028) "G\tH"))) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should (string-match-p "^> ABCDEG\tH$" (presence--input view)))))
  (should (equal "ab\ncd" (superchat-butler--quotable "a\rb\ncd"))))

(ert-deftest bring-in/a-long-answer-is-cut-and-says-so ()
  (bring--with `((:text ,(make-string 9000 ?x))) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (let ((input (presence--input view)))
        (should (< (length input) (+ superchat-butler-brought-chars 400)))
        (should (string-match-p "^> .*first 3000 of 9000 characters" input))))))

(ert-deftest bring-in/an-answer-that-fits-is-not-called-cut ()
  (bring--with `((:text ,(make-string 3000 ?x))) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should-not (string-match-p "characters" (presence--input view))))))

(ert-deftest bring-in/a-failed-task-brings-how-it-ended ()
  (bring--with '((:error (error . "BOOM-FROM-THE-SERVER"))) (p)
    (cl-destructuring-bind (view _run _home) (bring--task p)
      (bring--from-butler)
      (should (string-match-p "^> .*TASK-MARKER.* failed" (presence--input view))))))

;;;; Quoted lines -----------------------------------------------------------------------

(defun bring--inert-p (text)
  "Non-nil when TEXT reads as nothing but text."
  (let ((turn (isx--parse text)))
    (and (null (superchat-turn-skill turn))
         (null (superchat-turn-command turn))
         (null (superchat-turn-target-model turn))
         (null (plist-get (superchat-turn-state turn) :destination))
         (equal (string-trim text) (string-trim (superchat-turn-clean-input turn))))))

(ert-deftest bring-in/what-stands-in-a-quoted-line-triggers-nothing ()
  (isx--with-names
    (dolist (line '("run /summarize now" "/summarize" "try >planning first" ">planning"
                    "ask @gpt-4o about it" "@gpt-4o" "then >annotate \"foo\" more" ">annotate"
                    ">annotate a.txt"))
      (dolist (text (list (format "Report:\n> %s\n\nwhat is this" line)
                          (format "Report:\n> %s" line)
                          (format "> %s\n\nwhat is this" line)
                          (format "first\n> quoted\n> %s\nlast" line)))
        (should (bring--inert-p text))))))

(ert-deftest bring-in/a-message-that-opens-with-a-quote-is-not-a-skill ()
  ;; Measured 2026-11-01: this read as the skill `what'.
  (isx--with-names
    (should (bring--inert-p "> Butler: x completed\n\nwhat is this"))
    (should (bring--inert-p "> planning"))))

(ert-deftest bring-in/the-same-words-outside-a-quote-still-count ()
  (isx--with-names
    (should (equal "summarize"
                   (superchat-turn-command (isx--parse "Report:\n> quoted\n\nplease /summarize this"))))
    (should (equal "planning"
                   (superchat-turn-skill (isx--parse "Report:\n> quoted\n\nuse >planning here"))))
    (should (equal "gpt-4o"
                   (superchat-turn-target-model (isx--parse "Report:\n> quoted\n\nask @gpt-4o here"))))
    (should (eq 'annotate
                (plist-get (superchat-turn-state (isx--parse "Report:\n> quoted\n\nthis >annotate"))
                           :destination)))
    ;; At the start, as ever.
    (should (equal "planning" (superchat-turn-skill (isx--parse ">planning do it"))))
    (should (equal "summarize" (superchat-turn-command (isx--parse "/summarize do it"))))))

(ert-deftest bring-in/a-word-after-the-same-word-in-a-quote-still-counts ()
  ;; The search goes on past a quoted line, and takes out the right one.
  (isx--with-names
    (let ((turn (isx--parse "Report:\n> quoted >planning\n\nuse >planning here")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should (string-match-p "^> quoted >planning$" (superchat-turn-clean-input turn)))
      (should (string-match-p "^use here$" (superchat-turn-clean-input turn))))
    (let ((turn (isx--parse "Report:\n> quoted >annotate\n\nthis >annotate")))
      (should (eq 'annotate (plist-get (superchat-turn-state turn) :destination)))
      (should (string-match-p "^> quoted >annotate$" (superchat-turn-clean-input turn))))))

(ert-deftest bring-in/the-line-after-a-quote-is-not-one ()
  (isx--with-names
    (should (equal "summarize" (superchat-turn-command (isx--parse "> quoted\n/summarize this"))))
    (should (equal "planning" (superchat-turn-skill (isx--parse "> quoted\n>planning"))))))

(ert-deftest bring-in/a-message-too-short-to-be-a-quote-is-read-as-ever ()
  (isx--with-names
    (should (bring--inert-p "a"))
    (should (bring--inert-p ""))))

(ert-deftest bring-in/a-greater-than-sign-elsewhere-is-not-a-quote ()
  (isx--with-names
    (should (equal "summarize" (superchat-turn-command (isx--parse "x > 1\nsee > /summarize"))))
    ;; No space after it: that is the sigil.
    (should (equal "planning" (superchat-turn-skill (isx--parse "first\n>planning"))))))

(provide 'bring-in-regression)
;;; bring-in-regression.el ends here
