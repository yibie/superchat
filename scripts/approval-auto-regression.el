;;; approval-auto-regression.el --- "allow all" selects the level auto -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 5c (the user, 2026-10-12: "审批这里，应当添加一个选项 auto，或者
;; allow all time").  A fourth permission level, `auto': the sandbox mode
;; `danger-full-access' with the approval policy `ask'.  Full Emacs control is
;; not asked about, saving still is, and the model keeps `ask_user'.  An
;; approval question gets one more answer, "Allow all": this call is allowed
;; once and the conversation is switched to `auto'.  Only that conversation.
;;
;; This is a level of the Session, not a grant per tool: the approval hashes
;; and unprompted lists the decision of 2026-09-17 removed are not back.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/approval-auto-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^approval-auto/")'

;;; Code:

(load (expand-file-name "approval-publish-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(declare-function superchat-approval-answer-all "superchat-approval" (handle))
(declare-function superchat-approval-allow-all "superchat-approval" ())
(declare-function superchat-pending-allow-all "superchat-pending" (item))
(declare-function superchat-permission-level-decision "superchat-sandbox"
                  (tool-name args context &optional scope))
(declare-function superchat--cmd-permissions "superchat"
                  (cmd args input lang target-model))
(declare-function superchat--with-supported-input "superchat"
                  (request scope &optional current-p))

(defvar aauto--asked 0 "How many approval questions were opened.")

(defun aauto--answering (function)
  "Call FUNCTION with each approval question as it opens; return the timer.
Every question is counted.  FUNCTION must decide it, or the Run waits."
  (let (seen)
    (run-with-timer
     0.05 0.05
     (lambda ()
       (dolist (question (and (fboundp 'superchat-approval-open-questions)
                              (superchat-approval-open-questions)))
         (unless (member (plist-get question :handle) seen)
           (push (plist-get question :handle) seen)
           (cl-incf aauto--asked)
           (funcall function question)))))))

(defun aauto--all (question)
  "Answer QUESTION with \"allow all\"; refuse it where that does not exist."
  (unless (and (fboundp 'superchat-approval-answer-all)
               ;; An error would leave the question open and the Run waiting.
               (ignore-errors
                 (superchat-approval-answer-all (plist-get question :handle))))
    (superchat-approval-answer (plist-get question :handle) nil)))

(defun aauto--levels (session)
  "The levels SESSION selected, oldest first, read from the Tape."
  (delq nil
        (mapcar (lambda (event)
                  (and (equal "permission.level" (plist-get event :type))
                       (plist-get (plist-get event :payload) :level)))
                (superchat-tape-replay-events session))))

(defmacro aauto--pipeline (script &rest body)
  (declare (indent 1) (debug t))
  `(apub--pipeline ,script
     (let ((aauto--asked 0))
       ,@body)))

;;;; The answer -----------------------------------------------------------------

(ert-deftest approval-auto/allow-all-runs-this-call-and-selects-auto ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers ((aauto--answering #'aauto--all))
        (aw--settle (smoke--send view "go")))
      (should (= 1 aw--effect))
      (should (equal '("allowed-once") (apub--decisions session)))
      (should (equal '("auto") (aauto--levels session)))
      (should (eq 'auto (superchat-permission-current session)))
      (should (eq 'danger-full-access (superchat-sandbox-effective-mode session)))
      (should (eq 'ask (superchat-approval-effective-policy session))))))

(ert-deftest approval-auto/the-next-full-control-call-is-not-asked ()
  (aauto--pipeline (list aw--eval-round aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers ((aauto--answering #'aauto--all))
        (aw--settle (smoke--send view "go")))
      (should (= 1 aauto--asked))
      (should (= 1 (aw--count session "approval.requested")))
      (should (= 2 aw--effect)))))

(ert-deftest approval-auto/allow-once-still-asks-the-next-time ()
  "The guard: the old answer changes nothing about the level."
  (aauto--pipeline (list aw--eval-round aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (superchat-approval-answer (plist-get question :handle) t))))
        (aw--settle (smoke--send view "go")))
      (should (= 2 aauto--asked))
      (should (= 2 aw--effect))
      (should-not (aauto--levels session))
      (should (eq 'edit-buffers (superchat-permission-current session))))))

(ert-deftest approval-auto/an-answered-question-is-not-answered-again-with-all ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           late)
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (superchat-approval-answer (plist-get question :handle) nil)
              (setq late (if (fboundp 'superchat-approval-answer-all)
                             (superchat-approval-answer-all
                              (plist-get question :handle))
                           :missing)))))
        (aw--settle (smoke--send view "go")))
      (should (null late))
      (should (= 0 aw--effect))
      (should-not (aauto--levels session))
      (should (eq 'edit-buffers (superchat-permission-current session))))))

;;;; What the level does ---------------------------------------------------------

(ert-deftest approval-auto/saving-is-still-asked-and-can-be-asked ()
  (aauto--pipeline '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (context (list :session-id session)))
      (superchat-permission-set session 'auto)
      (should (eq 'ask (plist-get (superchat-permission-level-decision
                                   "save_buffer" nil context)
                                  :decision)))
      ;; `ask' with the policy `never' is a refusal; here it is a question.
      (should (eq 'ask (superchat-approval-effective-policy session)))
      (should (eq 'allow (plist-get (superchat-permission-level-decision
                                     "eval_elisp" nil context)
                                    :decision))))))

(ert-deftest approval-auto/the-model-keeps-ask-user ()
  "The adapter that makes `ask_user' usable is attached under `auto'."
  (aauto--pipeline '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (request (list :buffer view :session-id session :tools nil)))
      (superchat-permission-set session 'auto)
      (should (plist-get (superchat--with-supported-input request 'response)
                         :input-core))
      ;; The contrast: the level that asks nobody takes it away.
      (superchat-permission-set session 'danger-full-access)
      (should-not (plist-member (superchat--with-supported-input request 'response)
                                :input-core)))))

(ert-deftest approval-auto/another-conversation-is-not-switched ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (other "aauto-another-conversation"))
      (apub--with-timers ((aauto--answering #'aauto--all))
        (aw--settle (smoke--send view "go")))
      (should (eq 'auto (superchat-permission-current session)))
      (should (eq 'edit-buffers (superchat-permission-current other)))
      (should (eq 'ask (plist-get (superchat-permission-level-decision
                                   "eval_elisp" nil (list :session-id other))
                                  :decision))))))

(ert-deftest approval-auto/the-level-is-read-back-from-the-tape ()
  "Nothing but the Tape holds the level, so a new Emacs reads the same."
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers ((aauto--answering #'aauto--all))
        (aw--settle (smoke--send view "go")))
      ;; What a new Emacs has: the Tape, and none of this one's questions.
      (setq superchat-approval--open nil)
      (let ((configuration (superchat-view-session-configuration session)))
        (should (equal "auto" (format "%s" (plist-get configuration :permission-level))))
        (should (eq 'auto (superchat-permission-current session)))))))

;;;; Where it is selected and answered -------------------------------------------

(ert-deftest approval-auto/permissions-names-and-selects-it ()
  (aauto--pipeline '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (with-current-buffer view
        (should (string-match-p
                 "available: .*\\bauto\\b"
                 (plist-get (superchat--cmd-permissions nil "" nil nil nil) :content)))
        (let ((said (plist-get (superchat--cmd-permissions nil "auto" nil nil nil)
                               :content)))
          (should (string-match-p "\\`Permissions: auto " said))
          (should (string-match-p "[Ss]aving" said)))
        (should (string-match-p
                 "\\`Permissions: auto "
                 (plist-get (superchat--cmd-permissions nil "" nil nil nil) :content))))
      (should (eq 'auto (superchat-permission-current session))))))

(defun aauto--button (view tag)
  "The approval button TAG that VIEW shows, or nil."
  (with-current-buffer view
    (cl-some (lambda (overlay)
               (let ((widget (overlay-get overlay 'button)))
                 (and widget (equal tag (widget-get widget :tag)) widget)))
             (append (car (overlay-lists)) (cdr (overlay-lists))))))

(ert-deftest approval-auto/the-panel-has-allow-all-and-pressing-it-answers ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           pressed)
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (let ((button (aauto--button view "Allow all")))
                (if button
                    (progn (setq pressed t)
                           (funcall (widget-get button :superchat-action)))
                  ;; No button: fail instead of waiting forever.
                  (superchat-approval-answer (plist-get question :handle) nil))))))
        (aw--settle (smoke--send view "go")))
      (should pressed)
      (should (= 1 aw--effect))
      (should (eq 'auto (superchat-permission-current session)))
      (let ((case-fold-search nil))
        (should (string-match-p "^Allowed; this conversation no longer asks for tools; saving still asks$"
                                (aw--shown view)))))))

(ert-deftest approval-auto/the-command-answers-with-all ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (if (fboundp 'superchat-approval-allow-all)
                  (with-current-buffer view (superchat-approval-allow-all))
                (superchat-approval-answer (plist-get question :handle) nil)))))
        (aw--settle (smoke--send view "go")))
      (should (= 1 aw--effect))
      (should (eq 'auto (superchat-permission-current session))))))

(ert-deftest approval-auto/the-list-answers-with-all ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DONE"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (let ((row (cl-find-if
                          (lambda (item) (eq 'asking (plist-get item :kind)))
                          (superchat-pending-items))))
                (if (and row (fboundp 'superchat-pending-allow-all))
                    (superchat-pending-allow-all row)
                  (superchat-approval-answer (plist-get question :handle) nil))))))
        (aw--settle (smoke--send view "go")))
      (should (= 1 aw--effect))
      (should (eq 'auto (superchat-permission-current session)))
      (should (eq #'superchat-pending-allow-all
                  (lookup-key superchat-pending-mode-map (kbd "a")))))))

(ert-deftest approval-auto/a-level-that-cannot-be-recorded-leaves-the-question-open ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           seen)
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (let ((handle (plist-get question :handle)))
                (setq seen
                      (list :signaled
                            (condition-case nil
                                (cl-letf (((symbol-function 'superchat-permission-set)
                                           (lambda (&rest _) (error "aauto: no write"))))
                                  (superchat-approval-answer-all handle)
                                  nil)
                              (error t))
                            :still-open
                            (and (cl-find handle (superchat-approval-open-questions)
                                          :key (lambda (q) (plist-get q :handle))
                                          :test #'equal)
                                 t)))
                (superchat-approval-answer handle nil)))))
        (aw--settle (smoke--send view "go")))
      (should (equal '(:signaled t :still-open t) seen))
      (should (= 0 aw--effect))
      (should (equal '("rejected") (apub--decisions session))))))

;;;; A stale button ---------------------------------------------------------------

(ert-deftest approval-auto/a-stale-allow-all-button-selects-nothing ()
  (aauto--pipeline (list aw--eval-round '(:text "AAUTO_DECLINED"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (apub--with-timers
          ((aauto--answering
            (lambda (question)
              (superchat-approval-answer (plist-get question :handle) nil))))
        (aw--settle (smoke--send view "go")))
      (let ((button (aauto--button view "Allow all")))
        (should button)
        (funcall (widget-get button :superchat-action)))
      (should (= 0 aw--effect))
      (should-not (aauto--levels session))
      (should (eq 'edit-buffers (superchat-permission-current session))))))

(provide 'approval-auto-regression)
;;; approval-auto-regression.el ends here
