;;; test-tape-browser.el --- Tests for the human face of the Tape -*- lexical-binding: t; -*-

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-tape-browser)

(defun test-tape-browser--seed (session-id n)
  "Append N material entries to SESSION-ID and return their ids."
  (let ((ids nil))
    (dotimes (i n)
      (push (plist-get
             (superchat-tape-append-event-record
              (list :type "intent.received" :schema 1 :session-id session-id
                    :payload (list :text (format "entry %d" i))))
             :event-id)
            ids))
    ids))

(ert-deftest tape-browser/session-rows-aggregate-own-entries ()
  "Sessions rows count the session's own physical entries."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-a" 3)
    (test-tape-browser--seed "session-b" 1)
    (let* ((rows (superchat-tape-browser--sessions-rows))
           (row-a (cl-find "session-a" rows :key #'car :test #'equal))
           (row-b (cl-find "session-b" rows :key #'car :test #'equal)))
      (should row-a)
      (should row-b)
      (should (equal "3" (aref (cadr row-a) 1)))
      (should (equal "1" (aref (cadr row-b) 1))))))

(ert-deftest tape-browser/entry-rows-summarize-content ()
  "Entry rows render kind and a one-line content summary."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-a" 2)
    (let ((rows (superchat-tape-browser--entry-rows "session-a")))
      (should (= 2 (length rows)))
      (should (equal "user" (aref (cadar rows) 1)))
      (should-not (string-empty-p (aref (cadar rows) 2))))))

(ert-deftest tape-browser/summarize-single-line ()
  "Long multi-line content collapses to one truncated line."
  (let ((summary (superchat-tape-browser--summarize
                  (concat "line one\nline two\n"
                          (make-string 80 ?x)))))
    (should-not (string-match-p "\n" summary))
    (should (<= (length summary) 70))))

(ert-deftest tape-browser/active-count-matches-runtime ()
  "The active column counts non-terminal Runs owned by the session."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (should (= 0 (superchat-tape-browser--active-count "session-a")))
    (cl-letf (((symbol-function 'superchat-runtime-active-runs)
               (lambda () '("run-1" "run-2")))
              ((symbol-function 'superchat-runtime-status)
               (lambda (run-id)
                 (list :run-id run-id :session-id "session-a"
                       :status 'running-model))))
      (should (= 2 (superchat-tape-browser--active-count "session-a"))))))

(ert-deftest tape-browser/session-command-renders-list ()
  "M-x superchat-sessions opens a tabulated list of known sessions."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-a" 1)
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (_buffer &rest _) nil)))
      (should (superchat-sessions))
      (with-current-buffer "*superchat-sessions*"
        (should (eq major-mode 'tabulated-list-mode))
        (should (string-match-p "session-a" (buffer-string)))))))

(ert-deftest tape-browser/session-history-renders-selected-session ()
  "A direct history entry point renders only its selected session."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-a" 1)
    (test-tape-browser--seed "session-b" 2)
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (_buffer &rest _) nil)))
      (should (superchat-session-history "session-a"))
      (with-current-buffer "*superchat-session-entries*"
        (should (eq major-mode 'tabulated-list-mode))
        (should (equal superchat-tape-browser--session "session-a"))
        (should (= 1 (length (funcall tabulated-list-entries))))
        (should (string-match-p "intent.received" (buffer-string)))))))

;;; test-tape-browser.el ends here
