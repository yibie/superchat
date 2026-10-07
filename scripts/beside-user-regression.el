;;; beside-user-regression.el --- working beside the user -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-10-07-working-beside-the-user.md:
;; an execution goes on when its conversation's buffer is killed and fails only
;; where it asks the user; an execution Superchat started at a time does not
;; take the cursor; a file that changed on disk is found by Superchat before
;; an edit and said to the model; a Work Record's display names its
;; executions; a server error llm.el cannot read ends the request instead of
;; leaving it waiting.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/beside-user-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^beside-user/")'

;;; Code:

(load (expand-file-name "work-wake-fault-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-timer)
(require 'superchat-changes)
(require 'superchat-work)
(require 'llm-openai)
(defvar superchat-sandbox-default-mode)
(defvar superchat-changes-review-after-run)

(defmacro bu--with-dir (dir &rest body)
  "Bind DIR to a fresh directory; kill the buffers visiting it afterwards."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "bu-" t)))))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,dir (file-truename file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory ,dir t))))

(defun bu--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent))
  file)

(defun bu--other-program-writes (file text)
  "Write TEXT to FILE as another program would, with a later time on it."
  (bu--write file text)
  (set-file-times file (time-add (current-time) 60)))

(defun bu--tool-results (n)
  "The texts of the tool results in the Nth provider request."
  (mapcar #'hm--text
          (cl-remove-if-not (lambda (m) (equal "tool" (alist-get 'role m)))
                            (hm--body (hm--wire n)))))

(defun bu--settle (run)
  (should (stringp run))
  (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
  (plist-get (superchat-runtime-status run) :status))

(defun bu--edit-round (buffer old new)
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer)
                 (old_string . ,old) (new_string . ,new)))))

;;;; The conversation's buffer is killed while its execution works -----------------

(ert-deftest beside-user/an-execution-goes-on-when-its-view-is-killed ()
  ;; Measured 2026-10-07 on real models: 4 of 4 such executions failed with
  ;; "Human input adapter is no longer available" and the work was not done.
  (hm--fixture (list (append (hm--read-round "hm-target") '(:delay 0.5))
                     '(:text "BU-ANSWER"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (smoke--send view "read it")))
      (should (smoke--wait (lambda () (= 1 (length hm--wires))) 10))
      (let ((kill-buffer-query-functions nil))
        (should (kill-buffer view)))
      (should (eq 'completed (bu--settle run)))
      ;; The second request was made and its answer is on the Tape.
      (should (= 2 (length hm--wires)))
      (should (hm--events session "run.completed"))
      (should-not (hm--events session "run.failed")))))

(ert-deftest beside-user/asking-the-user-without-a-view-fails-the-question-only ()
  (hm--fixture (list (append (hm--read-round "hm-target") '(:delay 0.5))
                     '(:tool-uses (("ask_user" (question . "Which one?"))))
                     '(:text "BU-COULD-NOT-ASK"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (smoke--send view "read it, then ask me")))
      (should (smoke--wait (lambda () (= 1 (length hm--wires))) 10))
      (let ((kill-buffer-query-functions nil))
        (should (kill-buffer view)))
      (should (eq 'completed (bu--settle run)))
      ;; The model was told its question could not be put, and went on.
      (should (= 3 (length hm--wires)))
      (should (cl-some (lambda (text)
                         (string-match-p "The user cannot be asked: this conversation's window is closed" text))
                       (bu--tool-results 3)))
      (should-not (hm--events session "run.failed")))))

;;;; Work nobody is watching does not take the cursor -------------------------------

(defun bu--fact (session type payload &optional run)
  (superchat-tape-append-event
   (append (list :type type :session-id session :actor-id "test"
                 :summary type :payload payload)
           (and run (list :run-id run)))))

(ert-deftest beside-user/which-executions-nobody-started ()
  (hm--fixture (list '(:text "ONE") '(:text "TWO"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (first (hm--turn view "one"))
           (second (hm--turn view "two")))
      ;; An ordinary execution is the user's.
      (should-not (superchat-changes--unattended-run-p session first))
      (should-not (superchat-changes--unattended-run-p session second))
      ;; A scheduled task names the execution it started.
      (bu--fact session "timer.started"
                (list :timer-id "t1" :occurrence 1 :run-id second))
      (should (superchat-changes--unattended-run-p session second))
      (should-not (superchat-changes--unattended-run-p session first))
      ;; A woken record's execution is the first of its conversation; what
      ;; the user sends there afterwards is the user's.
      (bu--fact session "work.woken" (list :work-id "w1" :until 1 :main session))
      (should (superchat-changes--unattended-run-p session first)))))

(ert-deftest beside-user/a-task-that-came-due-is-nobodys-execution ()
  ;; The real path: the task fires, Superchat starts the execution.
  (wrec--with '((:text "ANSWER")) (p)
    (let* ((now 1800000000.0)
           (superchat-timer-now-function (lambda () now))
           (superchat-timer-notify-function #'ignore)
           (session (hm--session (wrec--view p))))
      (unwind-protect
          (progn
            (superchat-timer-create session 'task "BU-TASK do the thing" (+ now 60))
            (setq now (+ now 60))
            (let ((fired (car (superchat-timer-catch-up))))
              (wrec--settled)
              (should (stringp (plist-get fired :run-id)))
              (should (superchat-changes--unattended-run-p
                       session (plist-get fired :run-id)))))
        (superchat-timer-disarm-all)))))

(ert-deftest beside-user/a-woken-records-execution-is-nobodys ()
  ;; The real path: the record's time comes, Superchat wakes it in a new
  ;; conversation.  What the user sends there afterwards is the user's.
  (wrec--with '((:text "WOKEN-ANSWER") (:text "USER-ANSWER")) (p)
    (let ((wrec--now wrec--now))
      (wrec--with-clock
        (let* ((main (car (wwf--due-record p "BU woken")))
               (view (car (superchat-work-wake-due)))
               (session (hm--session view)))
          (wrec--settled)
          (should-not (equal session main))
          (let ((runs (mapcar (lambda (e) (plist-get e :run-id))
                              (hm--events session "run.started"))))
            (should (= 1 (length runs)))
            (should (superchat-changes--unattended-run-p session (car runs)))
            (let ((mine (hm--turn view "and now me")))
              (should-not (superchat-changes--unattended-run-p session mine))
              (should (superchat-changes--unattended-run-p session (car runs))))))))))

(defun bu--review-shown-p ()
  (and (get-buffer superchat-changes-review-buffer-name)
       (get-buffer-window superchat-changes-review-buffer-name t)
       t))

(defmacro bu--with-candidate (file-var &rest body)
  "Run BODY with one unsaved file buffer as every execution's candidate."
  (declare (indent 1) (debug t))
  `(bu--with-dir dir
     (let* ((,file-var (bu--write (expand-file-name "a.txt" dir) "hello\n"))
            (buffer (find-file-noselect ,file-var)))
       (with-current-buffer buffer (goto-char (point-max)) (insert "more\n"))
       (cl-letf (((symbol-function 'superchat-changes-candidates)
                  (lambda (&rest _)
                    (list (list :buffer buffer :name (buffer-name buffer)
                                :file ,file-var)))))
         (unwind-protect (progn ,@body)
           (when-let* ((review (get-buffer superchat-changes-review-buffer-name)))
             (kill-buffer review)))))))

(ert-deftest beside-user/a-scheduled-execution-shows-no-review-screen ()
  ;; Measured 2026-10-07: a woken record's review screen took the cursor out
  ;; of the draft the user was typing in.
  (hm--fixture (list '(:text "ONE"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (hm--turn view "one"))
           (before (selected-window)))
      (bu--with-candidate file
        (bu--fact session "timer.started"
                  (list :timer-id "t1" :occurrence 1 :run-id run))
        (superchat-changes--review-after-run session run)
        (should-not (bu--review-shown-p))
        (should (eq before (selected-window)))))))

(ert-deftest beside-user/the-users-own-execution-still-shows-the-review-screen ()
  ;; Guard: green before and after.
  (hm--fixture (list '(:text "ONE"))
    (let* ((view (smoke--open-chat))
           (session (hm--session view))
           (run (hm--turn view "one")))
      (bu--with-candidate file
        (superchat-changes--review-after-run session run)
        (should (bu--review-shown-p))))))

;;;; A file that changed on disk ----------------------------------------------------

(defmacro bu--no-emacs-question (&rest body)
  "Run BODY; fail when Emacs asks about a file changed on disk."
  (declare (indent 0) (debug t))
  `(let (asked)
     (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                (lambda (&rest _) (setq asked t))))
       ,@body)
     (should-not asked)))

(ert-deftest beside-user/an-unedited-buffer-is-reloaded-and-the-edit-sent-back ()
  ;; Measured 2026-10-07: Emacs asked "changed on disk; really edit the
  ;; buffer?" in the middle of the agent's edit, 4 times of 4.
  (bu--with-dir dir
    (let* ((file (bu--write (expand-file-name "list.txt" dir) "milk\neggs\n"))
           (buffer (find-file-noselect file))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-changes-review-after-run nil))
      (hm--fixture (list (bu--edit-round "list.txt" "eggs" "eggs 12")
                         '(:text "BU-DONE"))
        (bu--other-program-writes file "milk\neggs\nbread\n")
        (bu--no-emacs-question
          (should (eq 'completed (bu--settle (smoke--send (smoke--open-chat) "eggs are twelve")))))
        (let ((result (car (bu--tool-results 2))))
          (should (string-match-p "edit not applied" result))
          (should (string-match-p "changed on disk" result))
          (should (string-match-p "reloaded from disk" result)))
        ;; The buffer holds what the other program wrote, and no edit.
        (with-current-buffer buffer
          (should (equal "milk\neggs\nbread\n" (buffer-string)))
          (should-not (buffer-modified-p)))))))

(ert-deftest beside-user/a-buffer-with-unsaved-changes-is-edited-and-the-model-told ()
  (bu--with-dir dir
    (let* ((file (bu--write (expand-file-name "list.txt" dir) "milk\neggs\n"))
           (buffer (find-file-noselect file))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-changes-review-after-run nil))
      (with-current-buffer buffer (goto-char (point-max)) (insert "mine\n"))
      (hm--fixture (list (bu--edit-round "list.txt" "eggs" "eggs 12")
                         '(:text "BU-DONE"))
        (bu--other-program-writes file "milk\neggs\nbread\n")
        (bu--no-emacs-question
          (should (eq 'completed (bu--settle (smoke--send (smoke--open-chat) "eggs are twelve")))))
        (let ((result (car (bu--tool-results 2))))
          (should (string-match-p "Replaced 1 occurrence" result))
          (should (string-match-p "changed on disk" result))
          (should (string-match-p "would overwrite" result)))
        ;; The user's line and the edit are in the buffer; the disk is untouched.
        (with-current-buffer buffer
          (should (equal "milk\neggs 12\nmine\n" (buffer-string))))
        (should (equal "milk\neggs\nbread\n"
                       (with-temp-buffer (insert-file-contents file) (buffer-string))))))))

(ert-deftest beside-user/an-edit-of-an-unchanged-file-says-nothing-of-the-disk ()
  ;; Guard: green before and after.
  (bu--with-dir dir
    (let* ((file (bu--write (expand-file-name "list.txt" dir) "milk\neggs\n"))
           (buffer (find-file-noselect file))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-changes-review-after-run nil))
      (hm--fixture (list (bu--edit-round "list.txt" "eggs" "eggs 12")
                         '(:text "BU-DONE"))
        (should (eq 'completed (bu--settle (smoke--send (smoke--open-chat) "eggs are twelve"))))
        (let ((result (car (bu--tool-results 2))))
          (should (string-match-p "Replaced 1 occurrence" result))
          (should-not (string-match-p "changed on disk" result)))
        (with-current-buffer buffer
          (should (equal "milk\neggs 12\n" (buffer-string))))))))

(ert-deftest beside-user/reading-a-buffer-says-its-file-changed-on-disk ()
  (bu--with-dir dir
    (let* ((file (bu--write (expand-file-name "list.txt" dir) "milk\n"))
           (buffer (find-file-noselect file)))
      (should-not (string-match-p "changed on disk"
                                  (superchat-tool-read-buffer (buffer-name buffer))))
      (bu--other-program-writes file "milk\nbread\n")
      (let ((text (superchat-tool-read-buffer (buffer-name buffer))))
        (should (string-match-p "changed on disk after this buffer read it" text))
        ;; Reading changes nothing.
        (should (string-match-p "Content:\nmilk\n\\'" text))))))

;;;; A Work Record names its executions ----------------------------------------------

(ert-deftest beside-user/a-record-shown-to-the-user-names-its-executions ()
  ;; Measured 2026-10-07: 0 of 3 records shown named the execution that did
  ;; the work before a restart.
  (wrec--with nil (p)
    (let* ((session (hm--session (wrec--view p)))
           (id (superchat-work-start session "deliver the thing")))
      (should (stringp id))
      (should-not (superchat-work-executions-text (superchat-work-get id)))
      (bu--fact session "work.outcome"
                (list :work-id id :run-id "run-BU-1" :outcome "continuable"))
      (bu--fact session "work.outcome"
                (list :work-id id :run-id "run-BU-2" :outcome "completed"))
      (let ((text (superchat-work-executions-text (superchat-work-get id))))
        (should (string-match-p "run-BU-1 in conversation .*: continuable" text))
        (should (string-match-p "run-BU-2 in conversation .*: completed" text))
        (should (< (string-match "run-BU-1" text) (string-match "run-BU-2" text))))
      ;; What a request carries is as it was.
      (should-not (string-match-p "run-BU-1"
                                  (superchat-work-render (superchat-work-get id))))
      ;; The user's display has both.
      (save-window-excursion
        (superchat-work-list-show id)
        (with-current-buffer "*superchat work record*"
          (should (string-match-p "deliver the thing" (buffer-string)))
          (should (string-match-p "run-BU-2" (buffer-string))))))))

;;;; A server error llm.el cannot read ------------------------------------------------

(ert-deftest beside-user/a-server-error-that-is-plain-text-is-an-error-not-a-signal ()
  ;; Measured 2026-10-07: LM Studio answered two requests at once with
  ;; {"error": "text"}; llm.el signalled reading it, the error callback was
  ;; never called and the execution did not end.
  (let ((provider (make-llm-openai :key "k")))
    (let ((text (llm-provider-chat-extract-error
                 provider '((error . "Engine protocol predict stream returned an error")))))
      (should (stringp text))
      (should (string-match-p "Engine protocol predict stream" text)))
    ;; An error object is still read by llm.el itself.
    (should (string-match-p
             "boom"
             (llm-provider-chat-extract-error
              provider '((error . ((type . "server_error") (message . "boom")))))))
    ;; No error is still no error.
    (should-not (llm-provider-chat-extract-error provider '((choices . []))))))

(provide 'beside-user-regression)
;;; beside-user-regression.el ends here
