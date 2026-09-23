;;; finish-check-regression.el --- one finish check after a text-only reply -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the accepted decision
;; docs/decisions/accepted/2026-09-23-one-finish-check-after-a-text-only-reply.md:
;; after a tool-using Run ends on an empty reply or a reply whose last sentence
;; announces the model's own next action, one system-originated note is queued
;; through the existing steering path and the Run takes one more Step; a second
;; such reply is final, and no other surface or condition fires.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets.  All model calls
;; are scripted; no user configuration is evaluated.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-subr-trampolines nil) (package-initialize))' \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/finish-check-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^finish-check/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Finish-check regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro finish-check--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defconst finish-check--half-answer
  "Verification passed. Let me also confirm the file on disk."
  "A reply whose last sentence announces the model's own next action.")

(defconst finish-check--final-report
  (concat "Final report: the buffer was read. Nothing was changed; "
          "saving is yours.")
  "A long final report that does not announce an action.")

(defun finish-check--read-round (buffer-name)
  "A script step calling `read_buffer' on BUFFER-NAME."
  `(:tool-uses (("read_buffer" (buffer . ,buffer-name)))))

(defun finish-check--settle (run)
  "Wait until exact RUN is terminal; return RUN."
  (should (stringp run))
  (should (smoke--wait
           (lambda () (smoke--run-done-p run))
           20))
  run)

(defun finish-check--session (view)
  "Return VIEW's Session id."
  (buffer-local-value 'superchat--session-id view))

(defun finish-check--run (view text)
  "Send TEXT from VIEW and wait for the scripted Turn."
  (finish-check--settle (smoke--send view text)))

(defun finish-check--prompt (index)
  "Return the INDEXth (0-based, chronological) provider prompt."
  (plist-get (nth index (reverse (copy-sequence smoke--requests))) :prompt-text))

(defun finish-check--count (needle text)
  "Count non-overlapping occurrences of NEEDLE in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq start (match-end 0) count (1+ count)))
    count))

(defun finish-check--steers (session)
  "Return SESSION's `run.steered' events."
  (cl-remove-if-not
   (lambda (event) (equal "run.steered" (plist-get event :type)))
   (superchat-tape-replay-events session)))

(defun finish-check--note-events (session)
  "Return SESSION's `run.steered' events carrying the finish-check note."
  (cl-remove-if-not
   (lambda (event)
     (let ((payload (plist-get event :payload)))
       (and (member (plist-get payload :origin) '("finish-check" finish-check))
            (string-match-p (regexp-quote superchat-llm--finish-check-note)
                            (or (plist-get payload :instruction) "")))))
   (superchat-tape-replay-events session)))

(defun finish-check--buffer-text (view)
  "Return VIEW's full buffer text."
  (with-current-buffer view (buffer-string)))

;;;; The core behaviour

(ert-deftest finish-check/half-answer-gets-one-note ()
  "A tool-using Run whose reply announces a step takes one more Step.
The note reaches the model, the next reply is the answer, and the Tape
records one system-originated steer."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-A\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    (list :text finish-check--final-report))
            (let* ((view (smoke--open-chat))
                   (run (finish-check--run view "read it"))
                   (session (finish-check--session view)))
              (should (eq 'completed
                          (plist-get (superchat-runtime-status run) :status)))
              ;; Two requests for the tool round, one more for the check.
              (should (= 3 (length smoke--requests)))
              (let ((third (finish-check--prompt 2)))
                (should (= 1 (finish-check--count
                              superchat-llm--finish-check-note third))))
              ;; The third reply is the Run's answer.
              (should (string-match-p "Final report"
                                      (finish-check--buffer-text view)))
              ;; One system-originated steer on the Tape.
              (let ((notes (finish-check--note-events session)))
                (should (= 1 (length notes)))
                (should (equal "finish-check"
                               (format "%s" (plist-get (plist-get (car notes) :payload)
                                                        :origin)))))))
      (kill-buffer target)))))

(ert-deftest finish-check/second-half-answer-is-final ()
  "A second announcing reply ends the Run; exactly one note was sent."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-B\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    '(:text "Let me check one more thing."))
            (let* ((view (smoke--open-chat))
                   (run (finish-check--run view "read it"))
                   (session (finish-check--session view)))
              (should (eq 'completed
                          (plist-get (superchat-runtime-status run) :status)))
              (should (= 3 (length smoke--requests)))
              (should (= 1 (length (finish-check--note-events session))))
              (should (string-match-p "Let me check one more thing"
                                      (finish-check--buffer-text view))))))
      (kill-buffer target))))

(ert-deftest finish-check/long-report-gets-none ()
  "A final report that does not announce an action gets no check."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-C\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--final-report))
            (let ((view (smoke--open-chat)))
              (finish-check--run view "read it")
              (should (= 2 (length smoke--requests)))
              (should-not (finish-check--note-events (finish-check--session view))))))
      (kill-buffer target))))

(ert-deftest finish-check/no-tool-call-gets-none ()
  "A Run with no tool call never fires the check."
  (finish-check--fixture (list '(:text "Let me think about that."))
    (let ((view (smoke--open-chat)))
      (finish-check--run view "hello")
      (should (= 1 (length smoke--requests)))
      (should-not (finish-check--note-events (finish-check--session view))))))

(ert-deftest finish-check/empty-reply-gets-one-note ()
  "An explicit empty reply fires one check; a missing response still fails."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-D\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    '(:text "")
                    '(:text "Done after the note."))
            (let ((view (smoke--open-chat)))
              (finish-check--run view "read it")
              (should (= 3 (length smoke--requests)))
              (should (= 1 (length (finish-check--note-events
                                    (finish-check--session view)))))))
          ;; A missing response keeps its existing failure path.
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    '(:text nil))
            (let* ((view (smoke--open-chat))
                   (run (finish-check--run view "read it"))
                   (session (finish-check--session view)))
              (should (eq 'failed
                          (plist-get (superchat-runtime-status run) :status)))
              (should (= 2 (length smoke--requests)))
              (should-not (finish-check--note-events session)))))
      (kill-buffer target))))

(ert-deftest finish-check/pending-steer-wins ()
  "A user steer pending at the text-only reply wins; the note does not fire."
  (finish-check--fixture (list '(:text "first answer") '(:text "second answer"))
    (let* ((view (smoke--open-chat))
           (steered nil))
      (superchat-on
       nil 'turn-stopping 'finish-check-user-steer
       (lambda (run _turn)
         (unless steered
           (setq steered t)
           (superchat-runtime-steer (plist-get run :run-id) "USER-STEER-TEXT"))))
      (finish-check--run view "hello")
      (let ((session (finish-check--session view)))
        (should (= 2 (length smoke--requests)))
        (should (string-match-p "USER-STEER-TEXT" (finish-check--prompt 1)))
        (should-not (string-match-p (regexp-quote superchat-llm--finish-check-note)
                                    (finish-check--prompt 1)))
        (should (= 1 (length (finish-check--steers session))))
        (should-not (finish-check--note-events session))))))

(ert-deftest finish-check/budget-finalization-gets-none ()
  "When the budget asked for a final answer, the check does not fire."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-E\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    '(:text "Let me finish the report."))
            (let ((superchat-llm-max-tool-rounds 1)
                  (superchat-llm-round-limit-action 'finalize)
                  (view (smoke--open-chat)))
              (finish-check--run view "read it")
              (should (= 2 (length smoke--requests)))
              (should-not (finish-check--note-events
                           (finish-check--session view))))))
      (kill-buffer target))))

(ert-deftest finish-check/option-off-gets-none ()
  "With the option nil, an announcing reply gets no check."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-F\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    '(:text "unused"))
            (let ((superchat-llm-finish-check nil)
                  (view (smoke--open-chat)))
              (finish-check--run view "read it")
              (should (= 2 (length smoke--requests)))
              (should-not (finish-check--note-events
                           (finish-check--session view))))))
      (kill-buffer target))))

(ert-deftest finish-check/inline-act-gets-none ()
  "The same announcing reply through inline act never fires the check."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-G\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer))
            (let* ((overlay (superchat-inline-act "read it")))
              (should (smoke--wait
                       (lambda ()
                         (eq 'ready
                             (plist-get (overlay-get overlay 'superchat-inline)
                                        :status)))
                       20))
              (should (= 2 (length smoke--requests)))
              (should-not (finish-check--note-events
                           (plist-get (overlay-get overlay 'superchat-inline)
                                      :session-id))))))
      (kill-buffer target))))

;;;; Evidence and the chat View

(ert-deftest finish-check/the-note-is-on-the-tape ()
  "The Tape carries the note as a finish-check steer and as its material,
and the third request's prompt contains it once."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-H\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    (list :text finish-check--final-report))
            (let* ((view (smoke--open-chat))
                   (session (finish-check--session view)))
              (finish-check--run view "read it")
              (let* ((events (superchat-tape-replay-events session))
                     (steers (cl-remove-if-not
                              (lambda (event)
                                (equal "run.steered" (plist-get event :type)))
                              events))
                     (materials (cl-remove-if-not
                                 (lambda (event)
                                   (and (equal "attachment.added"
                                               (plist-get event :type))
                                        (equal "finish-check"
                                               (format
                                                "%s"
                                                (plist-get
                                                 (plist-get (plist-get event :payload)
                                                            :source)
                                                 :origin)))))
                                 events)))
                (should (= 1 (length steers)))
                (should (equal "finish-check"
                               (format "%s" (plist-get (plist-get (car steers)
                                                                  :payload)
                                                        :origin))))
                (should (= 1 (length materials)))
                (should (equal superchat-llm--finish-check-note
                               (plist-get (plist-get (car materials) :payload)
                                          :text)))
                (should (= 1 (finish-check--count
                              superchat-llm--finish-check-note
                              (finish-check--prompt 2))))))))
      (kill-buffer target))))

(ert-deftest finish-check/request-evidence-omits-step-material ()
  "The third request's rebuilt evidence includes its Step-claimed finish note."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-H\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    (list :text finish-check--final-report))
            (let* ((view (smoke--open-chat))
                   (session (finish-check--session view)))
              (finish-check--run view "read it")
              (let* ((lists (cl-remove-if-not
                             (lambda (event)
                               (equal "request.message-list" (plist-get event :type)))
                             (superchat-tape-replay-events session)))
                     (request-id (plist-get (plist-get (car (last lists)) :payload)
                                            :request-id))
                     (evidence (superchat-tape-request-evidence session request-id))
                     (messages (mapcar (lambda (message)
                                         (or (plist-get message :content) ""))
                                       (plist-get evidence :messages))))
                (should (eq 'reproduced (plist-get evidence :status)))
                (should (cl-some (lambda (content)
                                   (string-match-p
                                    (regexp-quote superchat-llm--finish-check-note)
                                    content))
                                 messages))))))
      (kill-buffer target))))

(ert-deftest finish-check/chat-view-does-not-show-it-as-the-user ()
  "The chat buffer never presents the note under a `* User:' heading."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-I\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    (list :text finish-check--final-report))
            (let ((view (smoke--open-chat)))
              (finish-check--run view "read it")
              (let ((text (finish-check--buffer-text view)))
                (should-not
                 (string-match-p
                  (concat "\\* User:[^\n]*"
                          (regexp-quote "last message has no tool call"))
                  text))))))
      (kill-buffer target))))

;;;; The pure trigger

(ert-deftest finish-check/trigger ()
  "The trigger reports empty, announced, or nothing, with exclusions."
  (should (eq 'empty (superchat-llm--finish-check-trigger "")))
  ;; "让我知道" asks the user to tell; it announces no action of the model's.
  (should-not (superchat-llm--finish-check-trigger "改好了。让我知道你还想怎么调整。"))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger
               "Verification passed. Let me also confirm the file on disk.")))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger "Done. I'll check the tail.")))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger
               "Read it. Now I will restore the old value.")))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger
               "Checked. Next, I will save the buffer.")))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger "改好了。让我再确认一下磁盘上的内容。")))
  (should (eq 'announced
              (superchat-llm--finish-check-trigger "确认完毕。接下来我会检查配置。")))
  ;; Exclusions.
  (should-not (superchat-llm--finish-check-trigger "Let me know if you need more."))
  (should-not (superchat-llm--finish-check-trigger "Please save it when you can."))
  (should-not (superchat-llm--finish-check-trigger "你可以保存这个文件。"))
  (should-not (superchat-llm--finish-check-trigger
               "Verified.\n```\n(let ((x 1)) (let me-check x))\n```"))
  (should-not (superchat-llm--finish-check-trigger
               "Done. \"Let me check the disk\""))
  (should-not (superchat-llm--finish-check-trigger "Report: nothing was changed.")))

(ert-deftest finish-check/unqueued-note-sends-nothing ()
  "When the note cannot be queued, the half answer is final: no request
carries nothing new."
  (let ((target (generate-new-buffer "finish-check-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "FC-TOKEN-G\n"))
          (finish-check--fixture
              (list (finish-check--read-round (buffer-name target))
                    (list :text finish-check--half-answer)
                    (list :text finish-check--final-report))
            (cl-letf (((symbol-function 'superchat-runtime-steer)
                       (lambda (&rest _) nil)))
              (let* ((view (smoke--open-chat))
                     (run (finish-check--run view "read it")))
                (should (eq 'completed
                            (plist-get (superchat-runtime-status run) :status)))
                (should (= 2 (length smoke--requests)))
                (should (string-match-p "Let me also confirm"
                                        (finish-check--buffer-text view)))))))
      (kill-buffer target))))

(provide 'finish-check-regression)
;;; finish-check-regression.el ends here
