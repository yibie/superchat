;;; superchat-guard.el --- A run going round, and its one redirect -*- lexical-binding: t; -*-

;;; Commentary:
;; A run that goes round spends rounds without getting closer: the same file
;; changed again and again, the same failure coming back.  This file counts
;; those signals over a run's entries and, when they hold, writes the one
;; redirect the run gets: the run's own counts and a line to change the
;; approach.  It never stops a run.  The signals follow Microsoft's
;; TokenEconomizer loop guard (tokeneconomizer/loopguard), checked against
;; the Tape of real use.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)

(defvar superchat-guard-min-calls 18
  "Calls a run makes before the guard looks: a loop needs time to form.")

(defvar superchat-guard-churn 4
  "Changes to one file that, past `superchat-guard-min-calls', mean going round.")

(defvar superchat-guard-errors 8
  "Failed calls that, past `superchat-guard-min-calls', mean going round.")

(defun superchat-guard--failed-p (entry)
  "Non-nil when the `tool' ENTRY failed: the tool said so, or a command
exited with a status other than 0 or timed out."
  (or (plist-get (plist-get entry :data) :error)
      (string-match-p "^\\[\\(exit status [1-9][0-9]*\\|timed out\\)" (or (plist-get entry :text) ""))))

(defun superchat-guard--signature (text)
  "The first line of TEXT that names a failure, else its first line, with
numbers and paths taken out, so that two runs failing the same way match."
  (let* ((lines (split-string (or text "") "\n" t "[ \t]+"))
         (line (or (seq-find (lambda (l) (string-match-p "[Ee]rror\\|FAIL\\|[Ff]ailed\\|exit status" l))
                             lines)
                   (car lines) "")))
    (string-trim (replace-regexp-in-string "0x[0-9a-f]+\\|[0-9]+\\|[^ \t]*/[^ \t]*" "" line))))

(defun superchat-guard-signals (entries)
  "The signals of a run over its ENTRIES: a plist of :calls, :errors (failed
calls), :repeats (failures that repeat an earlier one), :churn (the most
changes to one file by `edit' or `write') and :file (that file)."
  (let ((calls 0) (errors 0) (repeats 0) (seen nil) (files nil))
    (dolist (e entries)
      (pcase (plist-get e :kind)
        ('assistant
         (dolist (c (plist-get (plist-get e :data) :calls))
           (when-let* (((member (plist-get c :name) '("edit" "write")))
                       (path (plist-get (plist-get c :args) :path)))
             (cl-incf (alist-get path files 0 nil #'equal)))))
        ('tool
         (cl-incf calls)
         (when (superchat-guard--failed-p e)
           (cl-incf errors)
           (let ((sig (superchat-guard--signature (plist-get e :text))))
             (if (member sig seen) (cl-incf repeats) (push sig seen)))))))
    (let ((top (car (sort files (lambda (a b) (> (cdr a) (cdr b)))))))
      (list :calls calls :errors errors :repeats repeats
            :churn (or (cdr top) 0) :file (car top)))))

(defun superchat-guard-going-round-p (signals)
  "Non-nil when SIGNALS, of `superchat-guard-signals', show a run going round."
  (and (>= (plist-get signals :calls) superchat-guard-min-calls)
       (or (>= (plist-get signals :repeats) 1)
           (>= (plist-get signals :errors) superchat-guard-errors)
           (>= (plist-get signals :churn) superchat-guard-churn))))

(defun superchat-guard-counts (signals)
  "SIGNALS as one line a person and a model can read."
  (string-join
   (delq nil (list (format "%d calls" (plist-get signals :calls))
                   (and (> (plist-get signals :churn) 1)
                        (format "%s changed %d times" (plist-get signals :file)
                                (plist-get signals :churn)))
                   (and (> (plist-get signals :errors) 0)
                        (format "%d failed, %d of them the same failure again"
                                (plist-get signals :errors) (plist-get signals :repeats)))))
   "; "))

(defun superchat-guard-redirect (signals)
  "The system line a run going round gets once, from its SIGNALS."
  (format "Process check, counted from the Tape: this run made %s. That looks like going round. Do not stop: say in one line why the approach is not working, then try a materially different one: read again what the failing check expects, consider another cause or another file, and test the new idea before changing files again."
          (superchat-guard-counts signals)))

(provide 'superchat-guard)
;;; superchat-guard.el ends here
