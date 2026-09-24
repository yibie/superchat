;;; session-view-regression.el --- what a reopened session shows -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of the chat View `superchat-open-session' rebuilds from the
;; Tape.  Load with the prepared ELPA and the pinned llm source, then run
;; `^session-view/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Session-view regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defmacro session-view--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical isolated fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun session-view--reopen (script &optional target-text prompt)
  "Run one chat turn with SCRIPT, TARGET-TEXT in buffer `session-view-target'.
PROMPT is the user's message.  Return the reopened View's text."
  (let ((target (get-buffer-create "session-view-target")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (erase-buffer)
            (insert (or target-text "TARGET-TEXT\n")))
          (session-view--fixture script
            (let* ((view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view))
                   (run (smoke--send view (or prompt "what does the target say?"))))
              (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
              (with-current-buffer (superchat--render-session-view session)
                (buffer-substring-no-properties (point-min) (point-max))))))
      (kill-buffer target))))

(defconst session-view--read-round
  '(:tool-uses (("read_buffer" (buffer . "session-view-target"))))
  "A scripted step reading the target buffer.")

(defun session-view--heading-above (text needle)
  "Return the nearest Org heading line above the first NEEDLE in TEXT."
  (let ((at (let ((case-fold-search nil))
              (string-match (regexp-quote needle) text))))
    (should at)
    (let ((heading nil) (start 0))
      (while (string-match "^\\*+ .*$" text start)
        (when (< (match-beginning 0) at)
          (setq heading (match-string 0 text)))
        (setq start (match-end 0)))
      heading)))

(ert-deftest session-view/bookkeeping-rows-are-not-shown ()
  "Step brackets and request evidence are not shown between a question and
its answer, neither expanded nor folded."
  (let ((text (session-view--reopen
               (list session-view--read-round '(:text "It says what it says.")))))
    (should (string-match-p "\\* User: what does the target say\\?" text))
    (should (string-match-p "It says what it says\\." text))
    (should-not (string-match-p
                 "step\\.started\\|step\\.ended\\|request\\.header\\|request\\.message-list"
                 text))))

(ert-deftest session-view/an-answer-after-a-folded-row-stays-visible ()
  "After the finish check's steer (a folded audit row) the final report
starts its own Assistant heading instead of sitting in the folded row."
  (let ((text (session-view--reopen
               (list session-view--read-round
                     '(:text "Verification passed. Let me also confirm the file on disk.")
                     '(:text "Final report: the buffer was read. Nothing was changed."))
               nil "read it")))
    (should (string-match-p "Runtime audit: run\\.steered" text))
    (should (equal "** Assistant:"
                   (session-view--heading-above text "Final report: the buffer was read")))))

(ert-deftest session-view/a-tool-block-shows-what-was-stored ()
  "A reopened tool block shows the arguments and the result the Tape stored."
  (let ((text (session-view--reopen
               (list session-view--read-round '(:text "It says what it says.")))))
    (should (string-match-p "Tool call: read_buffer\n.*session-view-target" text))
    (should (string-match-p "Tool result: read_buffer\nTARGET-TEXT" text))))

(ert-deftest session-view/a-cut-result-says-so ()
  "A result the Tape kept only in part says how much was cut."
  (let* ((superchat-tape-tool-material-max-chars 12)
         (text (session-view--reopen
                (list session-view--read-round '(:text "It is long."))
                (concat (make-string 40 ?x) "\n"))))
    (should (string-match-p "Tool result: read_buffer\nxxxxxxxxxxxx" text))
    (should (string-match-p "\\[cut: [0-9]+ more characters\\]" text))))

(ert-deftest session-view/injected-material-shows-its-text ()
  "A steer admitted mid-Run shows the text the model received, not the row's
placeholder summary, and never under a User heading."
  (let ((target (get-buffer-create "session-view-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (erase-buffer) (insert "TARGET-TEXT\n"))
          (session-view--fixture
              (list (append session-view--read-round '(:delay 0.1))
                    '(:text "Read it."))
            (let* ((view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view))
                   (run (smoke--send view "read the target")))
              (should (superchat-runtime-steer run "SV-STEER-TEXT"))
              (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
              (let ((text (with-current-buffer (superchat--render-session-view session)
                            (buffer-substring-no-properties (point-min) (point-max)))))
                (should (string-match-p "\\*\\* Injected material\nSV-STEER-TEXT" text))
                (should-not (string-match-p "Injected model material" text))
                (should-not (string-match-p "\\* User: SV-STEER-TEXT" text))))))
      (kill-buffer target))))

(provide 'session-view-regression)
;;; session-view-regression.el ends here
