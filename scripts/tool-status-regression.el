;;; tool-status-regression.el --- tool-body status delivery -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for the accepted tool-status decision.  Load with the
;; prepared ELPA, pinned llm source, and run `^tool-status/' ERT tests.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Tool-status regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defmacro tool-status--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical isolated fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun tool-status--settle (run)
  "Wait for RUN to become terminal."
  (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
  (superchat-runtime-status run))

(defun tool-status--read-round (&optional count)
  "Return a scripted step with COUNT read_buffer calls."
  (list :tool-uses
        (cl-loop repeat (or count 1)
                 collect '("read_buffer" (buffer . "*scratch*")))))

(defmacro tool-status--record-statuses (&rest body)
  "Record actual Sink statuses emitted during BODY in `statuses'."
  (declare (indent 0) (debug t))
  `(let ((statuses nil)
         (original (symbol-function 'superchat-sink-emit)))
     (cl-letf (((symbol-function 'superchat-sink-emit)
                (lambda (sink slot run payload &rest options)
                  (when (eq slot 'status) (push payload statuses))
                  (apply original sink slot run payload options))))
       ,@body)))

(ert-deftest tool-status/a-tool-body-sends-calling-tool-then-waiting ()
  (tool-status--fixture (list (tool-status--read-round) '(:text "done"))
    (tool-status--record-statuses
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "read")))
        (should (eq 'completed (plist-get (tool-status--settle run) :status)))
        (should (equal '(waiting calling-tool waiting ready)
                       (nreverse statuses)))))))

(ert-deftest tool-status/model-phase-follows-the-body ()
  (tool-status--fixture (list (tool-status--read-round) '(:text "done"))
    (let ((during nil)
          (original (symbol-function 'superchat-tool-read-buffer)))
      (cl-letf (((symbol-function 'superchat-tool-read-buffer)
                 (lambda (&rest args)
                   (let ((run (car (superchat-runtime-active-runs))))
                     (push (plist-get (superchat-runtime-status run) :model-phase) during))
                   (apply original args))))
        (let* ((view (smoke--open-chat))
               (run (smoke--send view "read"))
               (status (tool-status--settle run)))
          (should (equal '(calling-tool) during))
          (should-not (eq 'calling-tool (plist-get status :model-phase))))))))

(ert-deftest tool-status/two-tools-send-waiting-once-after-the-last ()
  (tool-status--fixture (list (tool-status--read-round 2) '(:text "done"))
    (tool-status--record-statuses
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "read twice")))
        (should (eq 'completed (plist-get (tool-status--settle run) :status)))
        ;; The real executor runs this scripted pair sequentially.
        (should (equal '(waiting calling-tool waiting calling-tool waiting ready)
                       (nreverse statuses)))))))

(ert-deftest tool-status/terminal-run-gets-nothing-more ()
  (tool-status--fixture (list (tool-status--read-round) '(:text "done"))
    (tool-status--record-statuses
      (let* ((view (smoke--open-chat))
             (run (smoke--send view "read")))
        (tool-status--settle run)
        (let ((before (length statuses)))
          (superchat-runtime--note-open-work run "late-effect" 'effect-settled)
          (should (= before (length statuses))))))))

(ert-deftest tool-status/chat-window-unchanged ()
  (tool-status--fixture (list (tool-status--read-round) '(:text "done"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "read")))
      (tool-status--settle run)
      ;; Canonical smoke compares the full transcript byte-for-byte.  This
      ;; local check catches an extra waiting line left by the mid-Run status.
      (with-current-buffer view
        (should-not (string-match-p "Prefill / waiting first token"
                                    (buffer-string)))))))

(ert-deftest tool-status/chat-second-run-still-shows-waiting ()
  (tool-status--fixture
      (list (tool-status--read-round) '(:text "first done")
            (tool-status--read-round) '(:text "second done"))
    (let ((prefill-count 0)
          (original (symbol-function 'superchat--update-status)))
      (cl-letf (((symbol-function 'superchat--update-status)
                 (lambda (message &rest args)
                   (when (string-prefix-p "Prefill / waiting first token" message)
                     (cl-incf prefill-count))
                   (apply original message args))))
        (let ((view (smoke--open-chat)))
          (tool-status--settle (smoke--send view "first read"))
          (tool-status--settle (smoke--send view "second read"))
          (should (= 2 prefill-count))
          (with-current-buffer view
            (should-not (string-match-p "Prefill / waiting first token"
                                        (buffer-string)))))))))

(provide 'tool-status-regression)
;;; tool-status-regression.el ends here
