;;; superchat-tools.el --- The tool registry and the built-in tools -*- lexical-binding: t; -*-

;;; Commentary:
;; A tool's function takes one plist and returns a string or signals; an
;; asynchronous tool takes a DONE function too.  `superchat-tool-call' is the
;; only place a tool's error is caught.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-permission)

(defvar superchat-tools--registry (make-hash-table :test #'equal)
  "Tool name to its plist (:description :args :class :function :async).")

(defvar superchat-tools--always (make-hash-table :test #'equal)
  "(TIMELINE . TOOL-NAME) to t once the user answered `always'.
Not on the Tape: it lasts until Emacs exits.")

(defvar superchat-tool-tape nil
  "The timeline of the tool call being run, for a tool that needs it.")

(cl-defun superchat-tool-define (name &key description args class function async)
  "Define tool NAME, replacing any tool of that name.
DESCRIPTION is a string or a function of no arguments returning one.
ARGS is a list of (:name S :type T :description S :required BOOL), T being
string, number or boolean.
CLASS is `read', `write' or `exec'.  FUNCTION takes one plist, returns a string.
With ASYNC, FUNCTION takes the plist and DONE and returns at once, nothing or a
function that stops the work; DONE is called once with the result string, or
as (funcall DONE nil MESSAGE) for an error."
  (unless (memq class '(read write exec))
    (error "Tool %s: class must be read, write or exec, not %S" name class))
  (puthash name (list :description description :args args :class class :function function
                :async async)
           superchat-tools--registry))

(defun superchat-tool-remove (name)
  "Remove tool NAME."
  (remhash name superchat-tools--registry))

(defun superchat-tools--get (name)
  "The definition of tool NAME, or an error."
  (or (gethash name superchat-tools--registry) (error "Unknown tool: %s" name)))

(defun superchat-tool-specs (&optional names)
  "Specs for `superchat-llm-request' of the tools in NAMES, default all."
  (mapcar (lambda (name)
            (let* ((tool (superchat-tools--get name))
                   (description (plist-get tool :description)))
              (list :name name
                    :description (if (functionp description) (funcall description) description)
                    :args (plist-get tool :args))))
          (or names (hash-table-keys superchat-tools--registry))))

(defun superchat-tools--cut (text)
  "TEXT, cut to `superchat-tool-max-chars' with a note of what was left out."
  (if (<= (length text) superchat-tool-max-chars)
      text
    (format "%s\n[%d characters left out]" (substring text 0 superchat-tool-max-chars)
            (- (length text) superchat-tool-max-chars))))

(defun superchat-tools--summary (name args)
  "One short line for the user: tool NAME with ARGS."
  (let ((text (format "%s %S" name args)))
    (if (> (length text) 200) (concat (substring text 0 200) "...") text)))

(defun superchat-tools--permit (tape name tool args)
  "Decide whether the call may run; append the permission event when it was
asked about or denied.  Return nil to run, or the text that says why not."
  (let* ((decision (superchat-permission-decide tape (plist-get tool :class)))
         (key (cons tape name))
         (answer (cond ((not (eq decision 'ask)) decision)
                       ((gethash key superchat-tools--always) 'always)
                       (t (superchat-permission-ask name (superchat-tools--summary name args)))))
         (allowed (memq answer '(allow always))))
    (when (eq answer 'always) (puthash key t superchat-tools--always))
    (unless (eq decision 'allow)
      (superchat-tape-append
       tape 'event (format "%s (%s): %s -> %s" name (plist-get tool :class) decision
                           (if allowed "allowed" "denied"))
       :data (list :type "permission" :tool name :decision (symbol-name decision)
                   :allowed (and allowed t))))
    (unless allowed
      (format "Permission denied: %s is a %s tool and the level is %s."
              name (plist-get tool :class) (superchat-permission-level tape)))))

(defun superchat-tools--start (tape call finish cancelled)
  "Start CALL for timeline TAPE.  FINISH is called with (TEXT ERRORP) once.
CANCELLED is nil or a function that says whether the caller gave up while the
user was asked.  Return the tool's stop function or nil.  May signal."
  (let* ((name (plist-get call :name))
         (args (plist-get call :args))
         (tool (superchat-tools--get name))
         (missing (cl-remove-if (lambda (a) (plist-member args (intern (concat ":" (plist-get a :name)))))
                                (cl-remove-if-not (lambda (a) (plist-get a :required))
                                                  (plist-get tool :args))))
         (scope (plist-get (car (superchat-tape-entries :tape tape :limit 1 :newest t)) :scope))
         ;; A process made by the tool takes its directory from here.
         (default-directory (if (equal scope "home") (expand-file-name "~/") scope))
         (superchat-tool-tape tape)
         (why (unless missing (superchat-tools--permit tape name tool args))))
    (cond
     (missing (funcall finish (format "Missing required argument: %s"
                                      (mapconcat (lambda (a) (plist-get a :name)) missing ", "))
                       t)
              nil)
     (why (funcall finish why t) nil)
     ((and cancelled (funcall cancelled)) (funcall finish "Cancelled." t) nil)
     (t
      (let ((done (lambda (value &optional message)
                    (cond (message (funcall finish (format "Error: %s" message) t))
                          ((stringp value) (funcall finish value nil))
                          (t (funcall finish (format "Error: tool %s returned %S, not a string"
                                                     name value)
                                      t))))))
        (if (plist-get tool :async)
            (let ((stop (funcall (plist-get tool :function) args done)))
              (and (functionp stop) stop))
          (funcall done (funcall (plist-get tool :function) args))
          nil))))))

(defun superchat-tool-call (tape call on-result &optional cancelled)
  "Run CALL, (:id S :name S :args PLIST), for timeline TAPE.
CANCELLED, a function of no arguments, is asked once the user has answered:
when it says yes the tool does not run and its result is `Cancelled.'.
Append the result as a tool entry and call ON-RESULT with it once: before this
returns for a synchronous tool, later for an asynchronous one.  Return nil, or
a function that stops the tool: then nothing is appended and ON-RESULT is
not called.  Whatever the tool or the user does, the result is an entry."
  (let ((starting t) finished pending stop)
    (cl-labels ((deliver (text errorp)
                  (funcall on-result
                           ;; A quit must not leave a call without its result.
                           (let ((inhibit-quit t))
                             (superchat-tape-append
                              tape 'tool (superchat-tools--cut text)
                              :data (list :call-id (plist-get call :id) :name (plist-get call :name)
                                          :error (and errorp t))))))
                (finish (text errorp)
                  (unless finished
                    (setq finished t)
                    ;; Delivered after FUNCTION returned, so that what ON-RESULT
                    ;; signals is not taken for the tool's error.
                    (if starting (setq pending (cons text errorp)) (deliver text errorp))))
                (fail (text)
                  (setq finished t pending (cons text t))))
      (condition-case err (setq stop (let ((inhibit-quit nil))
                                       (superchat-tools--start tape call #'finish cancelled)))
        (error (fail (format "Error: %s" (error-message-string err))))
        (quit (fail "Interrupted by the user.")))
      (setq starting nil)
      (if pending
          (progn (deliver (car pending) (cdr pending)) nil)
        (lambda ()
          (unless finished
            (setq finished t)
            (when stop (funcall stop))))))))

;;; Reading

(defun superchat-tools--path (path)
  "Absolute name of PATH, relative to `default-directory'."
  (expand-file-name path default-directory))

(defun superchat-tools--buffer (name)
  "The live buffer called NAME, or an error."
  (or (get-buffer name) (error "No buffer named %s" name)))

(defun superchat-tools--numbered (args)
  "Numbered lines of the current buffer from ARGS' :start, at most 400."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((total (count-lines (point-min) (point-max)))
             ;; JSON has one number type: a line may come as 2.0.
             (start (max 1 (truncate (or (plist-get args :start) 1))))
             (count (min 400 (max 1 (truncate (or (plist-get args :count) 400)))))
             (end (min total (+ start count -1))))
        (goto-char (point-min))
        (forward-line (1- start))
        (if (> start total)
            (format "Nothing at line %d: %d lines in all." start total)
          (concat (format "Lines %d-%d of %d:\n" start end total)
                  (mapconcat (lambda (n)
                               (prog1 (format "%d\t%s" n (buffer-substring-no-properties
                                                          (pos-bol) (pos-eol)))
                                 (forward-line 1)))
                             (number-sequence start end) "\n")))))))

(superchat-tool-define
 "read_file" :class 'read
 :description "Read a file with line numbers, at most 400 lines a call. Paths are relative to the project."
 :args '((:name "path" :type string :description "File to read" :required t)
         (:name "start" :type number :description "First line, default 1")
         (:name "count" :type number :description "Lines to read, default and most 400"))
 :function (lambda (args)
             (let ((file (superchat-tools--path (plist-get args :path))))
               (if-let* ((buffer (find-buffer-visiting file)))
                   (with-current-buffer buffer (superchat-tools--numbered args))
                 (with-temp-buffer (insert-file-contents file)
                                   (superchat-tools--numbered args))))))

(superchat-tool-define
 "read_buffer" :class 'read
 :description "Read an Emacs buffer with line numbers, at most 400 lines a call."
 :args '((:name "buffer" :type string :description "Buffer name" :required t)
         (:name "start" :type number :description "First line, default 1")
         (:name "count" :type number :description "Lines to read, default and most 400"))
 :function (lambda (args)
             (with-current-buffer (superchat-tools--buffer (plist-get args :buffer))
               (superchat-tools--numbered args))))

(superchat-tool-define
 "list_buffers" :class 'read
 :description "List the user's buffers: name, mode, file, size in characters."
 :function (lambda (_)
             (mapconcat (lambda (b)
                          (with-current-buffer b
                            (format "%s\t%s\t%s\t%d" (buffer-name) major-mode
                                    (or buffer-file-name "-") (buffer-size))))
                        (cl-remove-if (lambda (b) (string-prefix-p " " (buffer-name b)))
                                      (buffer-list))
                        "\n")))

(defun superchat-tools--spawn (command seconds finish &optional enough)
  "Run COMMAND, a list, with no input.  Call FINISH once with the output and
why it ended: the exit status, `timeout' after SECONDS, `cut' when the output
passed a few times `superchat-tool-max-chars', or `enough' when ENOUGH, called
with the output so far, says so.  Return a function that kills the process and
cancels the timer; FINISH is then not called.  The buffer is killed always."
  (let* ((buffer (generate-new-buffer " *superchat-process*" t))
         (limit (* 4 superchat-tool-max-chars))
         (ended nil) timer process)
    (cl-flet ((end (why)
                (unless ended
                  (setq ended t)
                  (cancel-timer timer)
                  (delete-process process)
                  (let ((output (with-current-buffer buffer (buffer-string))))
                    (kill-buffer buffer)
                    ;; WHY nil: the caller stopped it and wants no result.
                    (when why (funcall finish output why))))))
      (condition-case err
          (setq process
                (make-process
                 :name "superchat-process" :buffer buffer :command command
                 :connection-type 'pipe :noquery t :file-handler t
                 :filter (lambda (process text)
                           (when (buffer-live-p (process-buffer process))
                             (with-current-buffer (process-buffer process)
                               (goto-char (point-max))
                               (insert text)
                               (cond ((> (buffer-size) limit)
                                      (delete-region (1+ limit) (point-max))
                                      (end 'cut))
                                     ((and enough (funcall enough)) (end 'enough))))))
                 :sentinel (lambda (process _)
                             (unless (process-live-p process)
                               (end (process-exit-status process))))))
        (error (kill-buffer buffer) (signal (car err) (cdr err))))
      (setq timer (run-at-time seconds nil (lambda () (end 'timeout))))
      (process-send-eof process)
      (lambda () (end nil)))))

(superchat-tool-define
 "search_files" :class 'read :async t
 :description "grep -rn for a pattern under the project, or under PATH. At most 100 lines."
 :args '((:name "pattern" :type string :description "grep pattern" :required t)
         (:name "path" :type string :description "File or directory to search, default the project"))
 :function (lambda (args done)
             ;; Stops at 101 matching lines or after ten seconds, so a search
             ;; of a huge tree does not run on.
             (superchat-tools--spawn
              (list "grep" "-rnIs" "--exclude-dir=.git" "-e" (plist-get args :pattern)
                    (or (plist-get args :path) "."))
              10
              (lambda (output why)
                (let ((lines (split-string output "\n" t)))
                  (cond ((eq why 'cut)
                         (funcall done (format "[output cut at %d characters]\n%s"
                                               (length output)
                                               (string-join (take 100 lines) "\n"))))
                        ((> (length lines) 100)
                         (funcall done (format "%s\n[more matching lines not shown]"
                                               (string-join (take 100 lines) "\n"))))
                        ((eq why 'timeout)
                         (funcall done (format "%s\n[stopped after 10 seconds: give a narrower path]"
                                               (string-join lines "\n"))))
                        (lines (funcall done (string-join lines "\n")))
                        ((eq why 1) (funcall done "No matches."))
                        (t (funcall done nil (format "grep exited with status %s" why))))))
              (lambda () (> (count-lines (point-min) (point-max)) 100)))))

;;; Writing

(superchat-tool-define
 "edit_buffer" :class 'write
 :description "Replace OLD by NEW in a buffer. OLD must occur exactly once. The buffer is not saved."
 :args '((:name "buffer" :type string :description "Buffer name" :required t)
         (:name "old" :type string :description "Text to replace, exactly once" :required t)
         (:name "new" :type string :description "Replacement" :required t))
 :function (lambda (args)
             (let ((old (plist-get args :old)) (new (plist-get args :new)) (n 0))
               (when (string-empty-p old) (error "OLD is empty"))
               (with-current-buffer (superchat-tools--buffer (plist-get args :buffer))
                 (let ((case-fold-search nil))
                  (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (while (search-forward old nil t)
                       (cl-incf n)
                       (goto-char (1+ (match-beginning 0))))
                     (unless (= n 1)
                       (error "OLD occurs %d times in %s; it must occur exactly once" n (buffer-name)))
                     (goto-char (point-min))
                     (search-forward old)
                     (undo-boundary)
                     (replace-match new t t)
                     (undo-boundary))))
                 (format "Replaced 1 occurrence in %s; not saved." (buffer-name))))))

(superchat-tool-define
 "write_file" :class 'write
 :description "Create or replace a file, through a buffer, and save it."
 :args '((:name "path" :type string :description "File to write" :required t)
         (:name "content" :type string :description "The whole new content" :required t))
 :function (lambda (args)
             (let ((file (superchat-tools--path (plist-get args :path))))
               (make-directory (file-name-directory file) t)
               (with-current-buffer (find-file-noselect file)
                 (undo-boundary)
                 (erase-buffer)
                 (insert (plist-get args :content))
                 (undo-boundary)
                 (let ((require-final-newline nil)) (save-buffer))
                 (format "Wrote %d characters to %s." (buffer-size) file)))))

(superchat-tool-define
 "save_buffer" :class 'write
 :description "Save a buffer to its file."
 :args '((:name "buffer" :type string :description "Buffer name" :required t))
 :function (lambda (args)
             (with-current-buffer (superchat-tools--buffer (plist-get args :buffer))
               (unless buffer-file-name (error "Buffer %s visits no file" (buffer-name)))
               (save-buffer)
               (format "Saved %s." buffer-file-name))))

;;; Executing

(superchat-tool-define
 "eval_elisp" :class 'exec
 :description "Evaluate Emacs Lisp forms in order (lexical binding). Returns what was messaged, then the last value."
 :args '((:name "code" :type string :description "One or more forms" :required t))
 :function (lambda (args)
             (let ((forms nil) (value nil)
                   (log (get-buffer-create "*Messages*")))
               (with-temp-buffer
                 (insert (plist-get args :code))
                 (goto-char (point-min))
                 (with-syntax-table emacs-lisp-mode-syntax-table
                   (while (progn (forward-comment (buffer-size)) (not (eobp)))
                     (push (condition-case err (read (current-buffer))
                             (error (error "Cannot read the code after character %d: %s"
                                           (point) (error-message-string err))))
                           forms))))
               (let ((from (with-current-buffer log (copy-marker (point-max)))))
                 (dolist (form (nreverse forms)) (setq value (eval form t)))
                 (concat (with-current-buffer log (buffer-substring-no-properties from (point-max)))
                         (prin1-to-string value))))))

(superchat-tool-define
 "shell" :class 'exec :async t
 :description "Run a shell command in the project directory; returns its output and exit status."
 :args '((:name "command" :type string :description "Command line" :required t))
 :function (lambda (args done)
             (superchat-tools--spawn
              (list shell-file-name "-c" (plist-get args :command))
              superchat-shell-seconds
              (lambda (output why)
                (let ((note (pcase why
                              ('cut (format "[output cut at %d characters; the command was stopped]"
                                            (length output)))
                              ('timeout (format "[timed out after %d seconds and killed]"
                                                superchat-shell-seconds))
                              (_ (format "[exit status %d]" why)))))
                  ;; Up front when the output is long: the cut of the whole result
                  ;; keeps the head, and must not take the note.
                  (funcall done (if (> (+ (length output) (length note)) superchat-tool-max-chars)
                                    (concat note "\n" output)
                                  (concat output "\n" note))))))))

(provide 'superchat-tools)
;;; superchat-tools.el ends here
