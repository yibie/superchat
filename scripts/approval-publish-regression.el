;;; approval-publish-regression.el --- an approval is published, then answered -*- lexical-binding: t; -*-

;;; Commentary:
;; Guidance 2026-10-11, slice 5 (ruling 4 of the five on closing the loop): an
;; approval question is published, Emacs goes on, and a later call answers it
;; -- from the conversation or from `superchat-pending'.  The minibuffer is
;; the fallback.  The audit pair, the `never' policy and "a question does not
;; outlive its Run" are as they were.
;;
;; A batch Emacs cannot show that a person may go on editing: what is tested
;; is that the question is listed while it is open, that timers run meanwhile,
;; and that one later call decides it once.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/approval-publish-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^approval-publish/")'

;;; Code:

(load (expand-file-name "approval-wait-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-pending)

(defvar superchat-approval-prompt)
(defvar superchat-approval--open)
(defvar superchat--session-id)
(declare-function superchat-approval-publish "superchat-approval" (request))
(declare-function superchat-approval-ask "superchat-approval" (request))
(declare-function superchat-approval-open-questions "superchat-approval" ())
(declare-function superchat-approval-answer "superchat-approval" (handle allow))
(declare-function superchat-approval-allow "superchat-approval" ())
(declare-function superchat-approval-deny "superchat-approval" ())
(declare-function superchat-pending-answer "superchat-pending" (item allow))

(defvar apub--seen nil "What the timer saw while the question was open.")
(defvar apub--ticks 0 "How often an unrelated timer ran while the question was open.")

(defmacro apub--pipeline (script &rest body)
  "Run BODY in the approval fixture with the publishing answerer."
  (declare (indent 1) (debug t))
  `(aw--pipeline ,script
     (let ((superchat-approval-function #'superchat-approval-publish)
           (superchat-approval--open nil)
           (apub--seen nil) (apub--ticks 0))
       ,@body)))

(defun apub--later (function)
  "Call FUNCTION once a question is open; return the timer."
  (let (timer)
    (setq timer
          (run-with-timer
           0.05 0.05
           (lambda ()
             (when (and (fboundp 'superchat-approval-open-questions)
                        (superchat-approval-open-questions))
               (cancel-timer timer)
               (funcall function (car (superchat-approval-open-questions)))))))
    timer))

(defmacro apub--with-timers (timers &rest body)
  (declare (indent 1) (debug t))
  `(let ((apub--timers (list ,@timers)))
     (unwind-protect (progn ,@body)
       (dolist (timer apub--timers) (when (timerp timer) (cancel-timer timer))))))

(defun apub--decisions (session)
  (mapcar (lambda (event) (plist-get (plist-get event :payload) :outcome))
          (cl-remove-if-not (lambda (event) (equal "approval.decided" (plist-get event :type)))
                            (superchat-tape-replay-events session))))

;;;; Published, then answered ----------------------------------------------------

(ert-deftest approval-publish/an-open-question-is-listed-and-answered-later ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           run)
      (apub--with-timers
          ((apub--later
            (lambda (question)
              (setq apub--seen
                    (list :questions (length (superchat-approval-open-questions))
                          :tool (plist-get question :tool-name)
                          :session (plist-get question :session-id)
                          :requested (aw--count session "approval.requested")
                          :decided (aw--count session "approval.decided")))
              ;; Emacs goes on for a while before anybody answers.
              (run-with-timer 0.3 nil #'superchat-approval-answer
                              (plist-get question :handle) t)))
           (run-with-timer 0.01 0.02 (lambda () (cl-incf apub--ticks))))
        (setq run (aw--settle (smoke--send view "go"))))
      (should (equal (list :questions 1 :tool "eval_elisp" :session session
                           :requested 1 :decided 0)
                     apub--seen))
      (should (> apub--ticks 5))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 aw--effect))
      (should (equal '("allowed-once") (apub--decisions session)))
      (should-not (superchat-approval-open-questions)))))

(ert-deftest approval-publish/a-later-no-refuses-the-call ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           run)
      (apub--with-timers
          ((apub--later (lambda (question)
                          (superchat-approval-answer (plist-get question :handle) nil))))
        (setq run (aw--settle (smoke--send view "go"))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session)))
      ;; A no is a refusal, and the model is told so; it was not cancelled.
      (let ((told (or (plist-get (car smoke--requests) :prompt-text) "")))
        (should (string-match-p "Tool call denied by user" told))
        (should-not (string-match-p "cancelled" told))))))

(ert-deftest approval-publish/a-question-is-answered-once-and-only-by-its-handle ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           answers)
      (apub--with-timers
          ((apub--later
            (lambda (question)
              (let ((handle (plist-get question :handle)))
                (push (superchat-approval-answer (make-symbol "not-a-question") t) answers)
                (push (aw--count session "approval.decided") answers)
                (push (and (superchat-approval-answer handle nil) t) answers)
                ;; Decided, it is no longer listed as open, though the call
                ;; that waits for it has not returned yet.
                (push (length (superchat-approval-open-questions)) answers)
                ;; The second answer comes too late to change the first.
                (push (superchat-approval-answer handle t) answers)))))
        (aw--settle (smoke--send view "go")))
      (should (equal '(nil 0 t 0 nil) (nreverse answers)))
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session))))))

(ert-deftest approval-publish/a-question-does-not-outlive-its-run ()
  (apub--pipeline (list aw--eval-round '(:text "must not be requested"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           handle late run)
      (apub--with-timers
          ((apub--later
            (lambda (question)
              (setq handle (plist-get question :handle))
              (superchat-runtime-cancel (plist-get question :run-id) "[apub stop]"))))
        (setq run (aw--settle (smoke--send view "go"))))
      (should handle)
      ;; As with the minibuffer: the Run ended with its question still open.
      (should (memq (plist-get (superchat-runtime-status run) :status)
                    '(cancelled audit-incomplete)))
      (should-not (superchat-approval-open-questions))
      (setq late (superchat-approval-answer handle t))
      (should-not late)
      (should (= 0 aw--effect))
      (should (= 1 (aw--count session "approval.decided"))))))

;;;; Where it is answered --------------------------------------------------------

(ert-deftest approval-publish/the-conversation-shows-the-question-and-answers-it ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           shown)
      (apub--with-timers
          ((apub--later
            (lambda (_question)
              (setq shown (aw--shown view))
              ;; The command answers the question of the conversation it is
              ;; called in.
              (with-current-buffer view (superchat-approval-allow)))))
        (aw--settle (smoke--send view "go")))
      (should (string-match-p "Allow `eval_elisp' once\\?" (or shown "")))
      (should (string-match-p "Waiting for your answer" (or shown "")))
      (should (= 1 aw--effect))
      (should (equal '("allowed-once") (apub--decisions session)))
      ;; The panel stays in the transcript and says how the question ended.
      (should (string-match-p "Allowed once" (aw--shown view)))
      (should-not (string-match-p "Waiting for your answer" (aw--shown view))))))

(ert-deftest approval-publish/a-conversation-that-cannot-hold-a-panel-shows-the-line ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           shown)
      (cl-letf (((symbol-function 'superchat--approval-panel-show)
                 (lambda (&rest _) (error "no panel here"))))
        (apub--with-timers
            ((apub--later
              (lambda (_question)
                (setq shown
                      (with-current-buffer view
                        (mapconcat
                         (lambda (overlay)
                           (or (and (overlay-get overlay 'superchat-approval)
                                    (overlay-get overlay 'after-string))
                               ""))
                         (overlays-in (point-min) (point-max)) "")))
                (with-current-buffer view (superchat-approval-allow)))))
          (aw--settle (smoke--send view "go"))))
      (should (string-match-p "eval_elisp" (or shown "")))
      (should (string-match-p "superchat-approval-allow" (or shown "")))
      (should (= 1 aw--effect))
      (should (equal '("allowed-once") (apub--decisions session)))
      ;; The line is gone with the question.
      (should-not (cl-some (lambda (overlay) (overlay-get overlay 'superchat-approval))
                           (with-current-buffer view
                             (overlays-in (point-min) (point-max))))))))

(ert-deftest approval-publish/the-list-answers-the-row-that-asks ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           rows answered)
      (apub--with-timers
          ((apub--later
            (lambda (_question)
              (setq rows (cl-remove-if-not
                          (lambda (item) (eq 'asking (plist-get item :kind)))
                          (superchat-pending-items)))
              (setq answered (superchat-pending-answer (car rows) nil)))))
        (aw--settle (smoke--send view "go")))
      (should (= 1 (length rows)))
      (should answered)
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session))))))

;;;; The minibuffer is the fallback ------------------------------------------------

(ert-deftest approval-publish/the-default-answerer-publishes-unless-told-otherwise ()
  (let ((request (list :tool-name "eval_elisp" :args '((expression . "1"))
                       :session-id "apub-session" :run-id nil))
        (superchat-approval--open nil)
        (asked 0))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (cl-incf asked) '(?y "yes, allow once"))))
      ;; Told to use the minibuffer, it reads there.
      (let ((superchat-approval-prompt 'minibuffer))
        (should (with-timeout (2 (ert-fail "published instead of asking in the minibuffer"))
                  (superchat-approval-ask request)))
        (should (= 1 asked)))
      ;; By default it publishes and reads nothing.
      (apub--with-timers
          ((apub--later (lambda (question)
                          (superchat-approval-answer (plist-get question :handle) t))))
        (let ((superchat-approval-prompt 'publish))
          (should (superchat-approval-ask request))))
      (should (= 1 asked)))))

;;;; Which question is meant ---------------------------------------------------

(defun apub--two-open (body)
  "Call BODY with two questions open, of conversations apub-a and apub-b.
BODY's errors are caught: it runs in a timer.
BODY gets the two results as a cons once both are decided; it is called from
a timer while both wait, and must answer both."
  (let ((superchat-approval--open nil)
        (body (let ((inner body)) (lambda () (ignore-errors (funcall inner)))))
        first second timers)
    (unwind-protect
        (progn
          (push (run-with-timer
                 0.05 nil
                 (lambda ()
                   (setq second
                         (list (superchat-approval-publish
                                (list :tool-name "tool-b" :args nil
                                      :session-id "apub-b" :run-id "run-b"))))))
                timers)
          (push (run-with-timer 0.2 nil body) timers)
          ;; Whatever BODY left open is refused, so a body that answered
          ;; wrongly shows as a wrong result and not as a wait without end.
          (push (run-with-timer
                 1.5 nil
                 (lambda ()
                   (dolist (question (superchat-approval-open-questions))
                     (superchat-approval-answer (plist-get question :handle) nil))))
                timers)
          (setq first
                (list (superchat-approval-publish
                       (list :tool-name "tool-a" :args nil
                             :session-id "apub-a" :run-id "run-a"))))
          (cons (car first) (car second)))
      (dolist (timer timers) (cancel-timer timer)))))

(ert-deftest approval-publish/the-command-answers-the-question-of-its-conversation ()
  (let (refused)
    (should
     (equal '(nil . t)
            (apub--two-open
             (lambda ()
               (should (= 2 (length (superchat-approval-open-questions))))
               ;; Two are open and this buffer belongs to neither: not guessed.
               (with-temp-buffer
                 (setq refused (condition-case nil
                                   (progn (superchat-approval-allow) nil)
                                 (user-error t))))
               (with-temp-buffer
                 (setq-local superchat--session-id "apub-b")
                 (superchat-approval-allow))
               (with-temp-buffer
                 (setq-local superchat--session-id "apub-a")
                 (superchat-approval-deny))))))
    (should refused)
    (should-not (superchat-approval-open-questions))))

(ert-deftest approval-publish/the-list-answers-the-question-of-its-row ()
  (cl-letf (((symbol-function 'superchat-pending-refresh) #'ignore))
    (should
     (equal '(nil . t)
            (apub--two-open
             (lambda ()
               (superchat-pending-answer (list :kind 'asking :key "run-b") t)
               (superchat-pending-answer (list :kind 'asking :key "run-a") nil)))))))

(ert-deftest approval-publish/the-list-answers-only-a-row-that-asks ()
  (let ((superchat-approval--open nil) refused)
    (cl-letf (((symbol-function 'superchat-pending-refresh) #'ignore))
      (should
       (equal '(nil . nil)
              (apub--two-open
               (lambda ()
                 ;; A row that is only running asks for nothing, though an
                 ;; approval of that execution is open.
                 (setq refused
                       (condition-case nil
                           (progn (superchat-pending-answer
                                   (list :kind 'running :key "run-b") t)
                                  nil)
                         (user-error t))))))))
    (should refused)))

(ert-deftest approval-publish/a-keyboard-macro-is-not-a-person ()
  (apub--with-timers
      ((apub--later (lambda (question)
                      (superchat-approval-answer (plist-get question :handle) t))))
    (let ((executing-kbd-macro t)
          (superchat-approval--open nil)
          (superchat-approval-prompt 'publish))
      (should-not (superchat-approval-ask
                   (list :tool-name "eval_elisp" :args nil
                         :session-id "apub-session" :run-id nil)))))
  (should-not (superchat-approval-open-questions)))

;;;; The question is a panel in its conversation (slice 5b) ---------------------

(defvar widget-keymap)
(declare-function widget-get "wid-edit" (widget property))
(declare-function superchat--thinking-status-message "superchat" (status latest))

(defun apub--buttons (view)
  "The approval buttons VIEW shows, as an alist of (TAG . WIDGET)."
  (with-current-buffer view
    (let (found)
      (dolist (overlay (append (car (overlay-lists)) (cdr (overlay-lists))))
        (let ((widget (overlay-get overlay 'button)))
          (when (and widget
                     (member (widget-get widget :tag)
                             '("Allow once" "Deny" "Details")))
            (cl-pushnew (cons (widget-get widget :tag) widget) found
                        :key #'car :test #'equal))))
      found)))

(defun apub--press (view tag)
  "Do what pressing the button TAG in VIEW does."
  (funcall (widget-get (cdr (assoc tag (apub--buttons view))) :superchat-action)))

(ert-deftest approval-publish/the-conversation-shows-a-panel-of-three-buttons ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat)) run)
      (apub--with-timers
          ((apub--later
            (lambda (question)
              (setq apub--seen
                    (list :tags (sort (mapcar #'car (apub--buttons view)) #'string<)
                          :text (and (string-match-p "eval_elisp" (aw--shown view)) t)))
              (superchat-approval-answer (plist-get question :handle) t))))
        (setq run (aw--settle (smoke--send view "go"))))
      (should (equal '(:tags ("Allow once" "Deny" "Details") :text t) apub--seen))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 aw--effect)))))

(ert-deftest approval-publish/pressing-deny-answers-no ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (answer (symbol-function 'superchat-approval-answer))
           calls run)
      (cl-letf (((symbol-function 'superchat-approval-answer)
                 (lambda (handle allow)
                   (push allow calls)
                   (funcall answer handle allow))))
        (apub--with-timers
            ((apub--later (lambda (_question)
                            (if (assoc "Deny" (apub--buttons view))
                                (apub--press view "Deny")
                              ;; No button: fail instead of waiting forever.
                              (push :no-button calls)
                              (funcall answer
                                       (plist-get (car (superchat-approval-open-questions))
                                                  :handle)
                                       t)))))
          (setq run (aw--settle (smoke--send view "go")))))
      (should (equal '(nil) calls))
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session)))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest approval-publish/pressing-allow-once-answers-yes ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           pressed)
      (apub--with-timers
          ((apub--later (lambda (question)
                          (if (assoc "Allow once" (apub--buttons view))
                              (progn (setq pressed t) (apub--press view "Allow once"))
                            (superchat-approval-answer (plist-get question :handle) nil)))))
        (aw--settle (smoke--send view "go")))
      (should pressed)
      (should (= 1 aw--effect))
      (should (equal '("allowed-once") (apub--decisions session))))))

(ert-deftest approval-publish/an-answered-panel-says-so-and-its-buttons-do-nothing ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers
          ((apub--later (lambda (question)
                          (superchat-approval-answer (plist-get question :handle) nil))))
        (aw--settle (smoke--send view "go")))
      ;; The label, not the refusal text the tool record may show.
      (let ((case-fold-search nil))
        (should (string-match-p "^Denied$" (aw--shown view)))
        (should-not (string-match-p "^Allowed once$" (aw--shown view))))
      (should-not (string-match-p "Waiting for your answer" (aw--shown view)))
      ;; A stale button decides nothing.
      (when (assoc "Allow once" (apub--buttons view))
        (apub--press view "Allow once"))
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session))))))

(ert-deftest approval-publish/the-status-line-says-it-waits-for-you ()
  (apub--pipeline (list aw--eval-round '(:text "APUB_DONE"))
    (let* ((view (smoke--open-chat)))
      (apub--with-timers
          ((apub--later
            (lambda (question)
              (setq apub--seen
                    (superchat--thinking-status-message
                     (superchat-runtime-status (plist-get question :run-id)) nil))
              (superchat-approval-answer (plist-get question :handle) t))))
        (aw--settle (smoke--send view "go")))
      (should (stringp apub--seen))
      (should (string-match-p "Waiting for your answer" apub--seen))
      (should-not (string-match-p "first token" apub--seen)))))

;;; approval-publish-regression.el ends here
