;;; superchat-lint.el --- The checks of a file edit or write just saved -*- lexical-binding: t; -*-

;;; Commentary:
;; A mistake the model makes in a file is cheapest to fix in the same round
;; it was made.  After `edit' or `write' saves a file, the checks here run
;; on it and what they find is added to the tool's result, so the model sees
;; it at once.  Two checks are built in; a person or a project adds more to
;; `superchat-lint-functions', `superchat-lint-command' making one of a
;; shell command.  A lint never refuses a change: it only reports.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)

(defvar superchat-lint-functions nil
  "Functions called in the buffer of a file `edit' or `write' just saved,
with the file name.  Each returns nil, or one or more lines that say what is
wrong, which the tool's result shows.  `superchat-lint-command' makes one.")

(defvar superchat-lint-project-command nil
  "A shell command a project runs after `edit' or `write' saves one of its
files, or nil.  Set in the project's `.dir-locals.el', run in that file's
directory; see `superchat-lint-check'.  Not safe as a local variable: it
runs the project's code, so the person accepts its value once.")

(defvar superchat-lint-seconds 20
  "Seconds a command of `superchat-lint-command' may take before it is stopped.")

(defun superchat-lint--parens ()
  "In an Emacs Lisp buffer, where its brackets or strings do not balance."
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-data-mode)
    (save-excursion
      (condition-case nil
          (progn (check-parens) nil)
        (error (format "Unbalanced parentheses or an unclosed string near line %d"
                       (line-number-at-pos)))))))

(defun superchat-lint--characters ()
  "Lines holding a character rarely meant in a file: a control character, a
private-use one, or the replacement character."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (< (length found) 3)
                  (re-search-forward
                   (rx (any (0 . 8) 11 (14 . 31) 127 (#xE000 . #xF8FF) #xFFFD)) nil t))
        (push (format "Line %d holds the character U+%04X, rarely meant (a control, private-use or replacement character)"
                      (line-number-at-pos) (char-before))
              found))
      (and found (string-join (nreverse found) "\n")))))

(defun superchat-lint-check (file)
  "What the checks find in the current buffer, just saved to FILE: a list of
lines, each starting with \"Lint: \"; nil when nothing is wrong.  A check that
signals is reported, not raised."
  (let (found)
    (dolist (check (append (list (lambda (_) (superchat-lint--parens))
                                 (lambda (_) (superchat-lint--characters))
                                 #'superchat-lint--project)
                           superchat-lint-functions))
      (let ((text (condition-case err (funcall check file)
                    (error (format "the check %s failed: %s"
                                   (if (symbolp check) check "(a function)")
                                   (error-message-string err))))))
        (when (and (stringp text) (not (string-empty-p (string-trim text))))
          (dolist (line (split-string (string-trim text) "\n" t))
            (push (concat "Lint: " line) found)))))
    (nreverse found)))

(defun superchat-lint--project (file)
  "Run the current buffer's `superchat-lint-project-command' on FILE, in the
directory of the `.dir-locals.el' that set it."
  (when superchat-lint-project-command
    (let ((default-directory (or (locate-dominating-file file ".dir-locals.el")
                                 default-directory)))
      (funcall (superchat-lint-command "" superchat-lint-project-command) file))))

(defun superchat-lint-command (pattern command)
  "A check for `superchat-lint-functions': for a saved file whose name
matches the regexp PATTERN, run the shell COMMAND in `default-directory',
\"%s\" in it replaced by the file's name, quoted.  When it exits with a
status other than 0, the check says so with the last 10 lines of its output;
it is stopped after `superchat-lint-seconds'."
  (lambda (file)
    (when (string-match-p pattern file)
      (let* ((line (string-replace "%s" (shell-quote-argument file) command))
             (out (generate-new-buffer " *superchat-lint*"))
             (proc (make-process :name "superchat-lint" :buffer out :noquery t
                                 :command (list shell-file-name shell-command-switch line)
                                 :connection-type 'pipe :sentinel #'ignore))
             (deadline (+ (float-time) superchat-lint-seconds)))
        (unwind-protect
            (progn
              (while (and (process-live-p proc) (< (float-time) deadline))
                (accept-process-output proc 0.1))
              (cond
               ((process-live-p proc)
                (delete-process proc)
                (format "%s: stopped after %d seconds" line superchat-lint-seconds))
               ((/= (process-exit-status proc) 0)
                (with-current-buffer out
                  (let ((lines (split-string (string-trim (buffer-string)) "\n" t)))
                    (format "%s exited %d:\n%s" line (process-exit-status proc)
                            (string-join (last lines 10) "\n")))))))
          (when (process-live-p proc) (delete-process proc))
          (kill-buffer out))))))

(provide 'superchat-lint)
;;; superchat-lint.el ends here
