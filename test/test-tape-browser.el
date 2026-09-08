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
                    :run-id (format "%s-run" session-id)
                    :turn-id (format "%s-turn" session-id)
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

(ert-deftest tape-browser/display-time-converts-utc-to-local ()
  "`superchat-tape-browser--display-time' converts a UTC SQLite timestamp to
local time; it must not merely echo the raw string when the local zone is
not UTC."
  (let ((old-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/Los_Angeles")
          (let* ((timestamp "2026-09-11 05:00:00")
                 ;; Computed independently of the function under test: build
                 ;; the UTC instant from its numeric fields (zone 0) rather
                 ;; than parsing the same string the same way.
                 (expected
                  (format-time-string
                   "%Y-%m-%d %H:%M:%S"
                   (encode-time (list 0 0 5 11 9 2026 nil nil 0)))))
            ;; Sanity: the local zone actually differs from UTC here, so an
            ;; unconverted echo of the raw string would not match `expected'.
            (should-not (equal expected timestamp))
            (should (equal expected
                          (superchat-tape-browser--display-time timestamp)))))
      (setenv "TZ" old-tz))))

(ert-deftest tape-browser/rows-display-sqlite-utc-times-in-local-time ()
  "Both list Views render SQLite's UTC timestamps through the local-time
display helper, not the raw stored string."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-time" 1)
    (let* ((raw-row (car (superchat-view-entries "session-time")))
           (timestamp (nth 5 raw-row))
           (expected (superchat-tape-browser--display-time timestamp))
           (entry-row (car (superchat-tape-browser--entry-rows "session-time")))
           (session-row
            (cl-find "session-time" (superchat-tape-browser--sessions-rows)
                     :key #'car :test #'equal)))
      (should (equal expected (aref (cadr entry-row) 3)))
      (should (equal expected (aref (cadr session-row) 2))))))

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

(ert-deftest tape-browser/entry-list-keeps-its-session-after-opening-sessions ()
  "RET in an existing entry list still opens its own session's entry detail."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-tape-browser--seed "session-a" 1)
    (test-tape-browser--seed "session-b" 1)
    (let ((buffers '("*superchat-sessions*" "*superchat-session-entries*"
                     "*superchat-entry*"))
          entries entry-id)
      (unwind-protect
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (_buffer &rest _args) nil)))
            (setq entries (superchat-session-history "session-a"))
            (setq entry-id (caar (superchat-view-entries "session-a")))
            (superchat-sessions)
            (with-current-buffer entries
              (goto-char (point-min))
              (while (and (not (equal (tabulated-list-get-id) entry-id))
                          (not (eobp)))
                (forward-line 1))
              (should (equal entry-id (tabulated-list-get-id)))
              (superchat-tape-browser--open))
            (with-current-buffer "*superchat-entry*"
              (should (string-match-p "Entry [0-9]+ in session session-a"
                                      (buffer-string))))
        (dolist (name buffers)
          (when-let* ((buffer (get-buffer name)))
            (kill-buffer buffer))))))))

;;; test-tape-browser.el ends here
