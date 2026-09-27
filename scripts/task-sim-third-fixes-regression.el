;;; task-sim-third-fixes-regression.el --- fixes from the third task simulation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for two defects the third task-simulation batch found
;; (.scratch/issues/2026-09-27-stream-order and
;; .scratch/issues/2026-09-27-finish-check-dots):
;;
;; - Streamed server-sent events reach llm.el in the order they arrived.
;;   plz-event-source hands each event to its listener on a timer set to
;;   `current-time', and Emacs runs timers of equal time newest first, so
;;   two events parsed within one clock tick arrived swapped and a streamed
;;   answer showed two neighbouring pieces in the wrong order.
;; - The finish check reads the whole last sentence: a dot inside it (a file
;;   name, "e.g.", a version number) does not end it.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/task-sim-third-fixes-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^task-sim-third/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-llm)
(require 'plz-event-source)

;;;; Streamed events in arrival order

(defun ts3--deliver (n &optional still-clock)
  "Insert N events in one chunk; return the numbers the listener saw, in order.
With STILL-CLOCK every `current-time' call returns the same reading."
  (let* ((seen nil)
         (buffer (generate-new-buffer " *ts3-events*"))
         (source (plz-event-source-buffer :buffer (buffer-name buffer)))
         (data (mapconcat (lambda (i) (format "data: %d\n\n" i))
                          (number-sequence 1 n) "")))
    (unwind-protect
        (progn
          (plz-event-source-add-listener
           source 'message
           (lambda (event)
             (push (string-to-number (plz-event-source-event-data event)) seen)))
          (plz-event-source-open source)
          (if still-clock
              (let ((now (current-time)))
                (cl-letf (((symbol-function 'current-time) (lambda (&rest _) now)))
                  (plz-event-source--insert source data)))
            (plz-event-source--insert source data))
          (with-timeout (10 (error "Events not delivered"))
            (while (< (length seen) n) (accept-process-output nil 0.01)))
          (nreverse seen))
      (kill-buffer buffer))))

(ert-deftest task-sim-third/events-of-one-clock-reading-keep-their-order ()
  (should (equal '(1 2 3 4 5) (ts3--deliver 5 t))))

(ert-deftest task-sim-third/bursts-keep-their-order ()
  "At natural timing: 100 bursts of 20 events, every one in order."
  (dotimes (_ 100)
    (should (equal (number-sequence 1 20) (ts3--deliver 20)))))

(ert-deftest task-sim-third/a-clock-step-back-does-not-delay-events ()
  "Guard: a clock set back by an hour does not hold events for that hour."
  (let ((superchat--llm-last-event-time (time-add (current-time) 3600)))
    (should (equal '(1 2 3) (ts3--deliver 3)))))

;;;; The finish check reads the whole last sentence

(ert-deftest task-sim-third/a-dot-inside-the-last-sentence-does-not-cut-it ()
  (dolist (text '("Only one diary file exists. Let me check whether there's a diary mechanism (e.g. a Superchat diary function or org-agenda diary file)."
                  "The binding is wrong. Let me open init.el"
                  "The binding is wrong. Let me open init.el."
                  "I found it. Let me read version 2.0 of the file."
                  "查到了。让我再看一下 init.el"))
    (should (eq 'announced (superchat-llm--finish-check-trigger text)))))

(ert-deftest task-sim-third/a-report-with-dots-stays-final ()
  "Guard: a finished report whose last sentence names a file does not fire."
  (should-not (superchat-llm--finish-check-trigger
               "Let me summarise. I changed init.el and left it unsaved."))
  (should-not (superchat-llm--finish-check-trigger
               "让我总结一下。我改了 init.el，没有保存。")))

(provide 'task-sim-third-fixes-regression)

;;; task-sim-third-fixes-regression.el ends here
