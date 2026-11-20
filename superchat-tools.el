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
(require 'superchat-lint)

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

(defun superchat-tools--summary (args)
  "One short line for the user: the ARGS of a call.  The question names the
tool itself."
  (let ((text (format "%S" args)))
    (if (> (length text) 200) (concat (substring text 0 200) "...") text)))

(defun superchat-tools--reads (tool args)
  "The files and directories a call of TOOL with ARGS reads: its path, the
project when the path is left out, and the file of a buffer it names.
A path is expanded as the tool expands it, so what is checked is what is read."
  (delq nil
        (list (when (and (cl-some (lambda (a) (equal (plist-get a :name) "path"))
                                  (plist-get tool :args))
                         ;; A call that names a buffer reads no default path.
                         (not (and (plist-get args :buffer) (not (plist-get args :path)))))
                (superchat-tools--path (let ((path (plist-get args :path)))
                                         (if (stringp path) path "."))))
              (when-let* ((name (plist-get args :buffer)) ((stringp name))
                          (buffer (get-buffer name)))
                (buffer-file-name buffer)))))

(defvar superchat-tools--loose nil
  "Non-nil while a call of a timeline with no working directory runs.")

(defun superchat-tools--outside-p (scope tool args)
  "Non-nil when a call of TOOL with ARGS reads outside SCOPE and outside
`superchat-directory', where the skills the system prompt offers are.  Links
are resolved first; \"home\" is no project, so only `superchat-directory' is
inside it."
  (cl-some (lambda (file)
             (not (or (file-in-directory-p file superchat-directory)
                      (and (not (equal scope "home")) (file-in-directory-p file scope)))))
           (superchat-tools--reads tool args)))

(defun superchat-tools--permit (tape call tool args scope)
  "Decide whether CALL may run; append the permission event when the user
was asked now or the call is denied.  Return nil to run, `later' when the
question waits, or the text that says why not.  A read outside SCOPE is
decided as a write: the user agrees to it first."
  (let* ((name (plist-get call :name))
         (class (if (and (eq (plist-get tool :class) 'read)
                         (superchat-tools--outside-p scope tool args))
                    'write
                  (plist-get tool :class)))
         (decision (superchat-permission-decide tape class))
         (key (cons tape name))
         (asked nil)
         (answer (cond ((not (eq decision 'ask)) decision)
                       ((gethash key superchat-tools--always) 'always)
                       ((superchat-permission-answered tape (plist-get call :id)))
                       (t (setq asked t)
                          (superchat-permission-ask name (superchat-tools--summary args) tape))))
         (allowed (memq answer '(allow always))))
    (when (eq answer 'always) (puthash key t superchat-tools--always))
    ;; An "always" given earlier or an answer already on the Tape asks nobody:
    ;; an event for each such call would only crowd the Tape.
    (when (and (not (eq answer 'later)) (or asked (not allowed)))
      (superchat-tape-append
       tape 'event (format "%s (%s%s): %s -> %s" name (plist-get tool :class)
                           (if (eq class (plist-get tool :class)) "" ", outside the project")
                           decision (if allowed "allowed" "denied"))
       :data (list :type "permission" :tool name :decision (symbol-name decision)
                   :allowed (and allowed t))))
    (cond
     ((eq answer 'later) 'later)
     ((not allowed)
      (format "Permission denied: %s is a %s tool%s and the level is %s."
              name (plist-get tool :class)
              (if (eq class (plist-get tool :class)) "" ", used outside the project")
              (superchat-permission-level tape))))))

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
         (directory (superchat-tape-directory tape))
         (superchat-tools--loose (not directory))
         ;; A process made by the tool takes its directory from here.
         (default-directory (or directory (superchat-tools-scratch tape)))
         (superchat-tool-tape tape)
         (why (unless missing (superchat-tools--permit tape call tool args scope))))
    (cond
     ((eq why 'later) (funcall finish 'later nil) nil)
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
returns for a synchronous tool, later for an asynchronous one.  When the
question about it must wait (the ask function said `later'), nothing runs and
ON-RESULT gets the `question' event instead.  Return nil, or
a function that stops the tool: then nothing is appended and ON-RESULT is
not called.  Whatever the tool or the user does, the result is an entry."
  (let ((starting t) finished pending stop)
    (cl-labels ((deliver (text errorp)
                  (funcall on-result
                           ;; A quit must not leave a call without its result.
                           (let ((inhibit-quit t)
                                 (summary (superchat-tools--summary (plist-get call :args))))
                             (if (eq text 'later)
                                 (superchat-tape-append
                                  tape 'event (format "%s %s" (plist-get call :name) summary)
                                  :data (list :type "question" :call-id (plist-get call :id)
                                              :tool (plist-get call :name) :summary summary))
                               (superchat-tape-append
                                tape 'tool (superchat-tools--cut text)
                                :data (list :call-id (plist-get call :id)
                                            :name (plist-get call :name)
                                            :error (and errorp t)))))))
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

(defconst superchat-tools--path-note
  "PATH is relative to the project, or absolute (~ works). A file outside the project is read only after the user agrees."
  "What a reading tool says of its path: the model must not think it is bound
to the project and turn to `shell', which the user cannot judge as well.")

(defconst superchat-tools--division
  "Files go through read, grep, find, edit and write; edit and write save the file. Outside programs go through shell, the running Emacs through eval_elisp."
  "What each tool that touches files says of the division of the tools.")

(defun superchat-tools--describe (&rest parts)
  "A file tool's description: PARTS, then the division of the tools."
  (string-join (append parts (list superchat-tools--division)) " "))

(defun superchat-tools--path (path)
  "Absolute name of PATH, relative to `default-directory'; a relative PATH is
an error in a timeline with no working directory."
  (when (and superchat-tools--loose (not (file-name-absolute-p path)))
    (error "This conversation has no working directory: give an absolute path"))
  (expand-file-name path default-directory))

(defun superchat-tools-scratch (tape)
  "The directory, made when missing, where a call of timeline TAPE runs when
it has no working directory: a process it starts needs one that exists."
  (let ((dir (expand-file-name (concat "superchat-" (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" tape) "/")
                               temporary-file-directory)))
    (make-directory dir t)
    dir))

(defun superchat-tools--buffer (name)
  "The live buffer called NAME, or an error."
  (or (get-buffer name) (error "No buffer named %s" name)))

(defun superchat-tools--number (value default)
  "VALUE as a number, DEFAULT when it is nil.
A model may send a number as a string."
  (cond ((null value) default)
        ((stringp value) (string-to-number value))
        (t value)))

(defun superchat-tools--count (value default)
  "VALUE as a whole number of at least 1, DEFAULT when it is nil.
JSON has one number type: a count may come as 2.0."
  (max 1 (truncate (superchat-tools--number value default))))

(defun superchat-tools--room ()
  "The characters a tool's own text may take.  The rest of
`superchat-tool-max-chars' is for its notices, so that the cut of the whole
result never takes one."
  (max 1 (- superchat-tool-max-chars (min 1000 (/ superchat-tool-max-chars 4)))))

(defun superchat-tools--noted (text notices)
  "TEXT, then NOTICES that are not nil, in brackets, after a blank line."
  (let ((notes (mapconcat (lambda (n) (concat "[" n "]")) (delq nil notices) "\n")))
    (cond ((string-empty-p notes) text)
          ((string-empty-p text) notes)
          (t (concat text "\n\n" notes)))))

(defun superchat-tools--join (lines)
  "As many whole LINES as fit the room, joined by newlines, with the notice
that some were left out, or nil: (TEXT . NOTICE)."
  (let ((room (superchat-tools--room)) (size -1) kept)
    (while (and lines (<= (+ size 1 (length (car lines))) room))
      (cl-incf size (1+ (length (car lines))))
      (push (pop lines) kept))
    (cons (string-join (nreverse kept) "\n")
          (and lines (format "%d characters limit reached" room)))))

(defun superchat-tools--lines (args what)
  "The lines of the current buffer from ARGS' :offset, whole, at most its
:limit, 2000 and the room, with the notice that names the next offset.
WHAT, \"file\" or \"buffer\", is what the notices call the text."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((total (count-lines (point-min) (point-max)))
             (from (superchat-tools--count (plist-get args :offset) 1))
             (limit (and (plist-get args :limit) (superchat-tools--count (plist-get args :limit) 1)))
             (most (min 2000 (or limit 2000)))
             (room (superchat-tools--room))
             (to (1- from))
             start end cut)
        (when (> from (max total 1))
          (error "Offset %d is beyond end of %s (%d lines total)" from what total))
        (goto-char (point-min))
        (forward-line (1- from))
        (setq start (point) end (point))
        (while (and (not cut) (< (- to from -1) most) (< to total))
          (if (> (- (pos-eol) start) room)
              (setq cut t)
            (setq to (1+ to) end (pos-eol))
            (forward-line 1)))
        (if (and cut (< to from))
            (let ((next (if (< from total) (format " Use offset=%d to continue." (1+ from)) "")))
              (superchat-tools--noted
               (buffer-substring-no-properties start (+ start room))
               (list (format "Line %d is %d characters; showing the first %d.%s"
                             from (- (pos-eol) start) room next))))
          (let ((next (format "Use offset=%d to continue." (1+ to))))
            (superchat-tools--noted
             (buffer-substring-no-properties start end)
             (list (cond ((= to total) nil)
                         (cut (format "Showing lines %d-%d of %d (%d characters limit). %s"
                                      from to total room next))
                         ((and limit (<= limit 2000))
                          (format "%d more lines in %s. %s" (- total to) what next))
                         (t (format "Showing lines %d-%d of %d. %s" from to total next)))))))))))

(defconst superchat-tools--read-args
  '((:name "offset" :type number :description "First line, 1-indexed, default 1")
    (:name "limit" :type number :description "Most lines to read"))
  "The arguments that choose which lines a read returns.")

(defun superchat-tools--listing (dir args)
  "The entries of directory DIR, sorted ignoring case, a / after a directory,
at most ARGS' :limit (default 500)."
  (let* ((limit (superchat-tools--count (plist-get args :limit) 500))
         (names (sort (directory-files dir nil directory-files-no-dot-files-regexp t)
                      (lambda (a b) (string< (downcase a) (downcase b)))))
         (joined (superchat-tools--join
                  (mapcar (lambda (n) (if (file-directory-p (expand-file-name n dir)) (concat n "/") n))
                          (take limit names)))))
    (superchat-tools--noted
     (if names (car joined) "(empty directory)")
     (list (and (> (length names) limit)
                (format "%d entries limit reached. Use limit=%d for more" limit (* 2 limit)))
           (cdr joined)))))

(superchat-tool-define
 "read" :class 'read
 :description (superchat-tools--describe
               "Read a file or an Emacs buffer as text, without line numbers, at most 2000 lines or the"
               "character limit a call; a notice at the end names the offset to continue from."
               "Unsaved edits in a file's buffer are seen. A directory is listed: its entries, a / after"
               "directories, dotfiles included, LIMIT entries at most. To learn a file's line count,"
               "read it with limit 1." superchat-tools--path-note)
 :args (append '((:name "path" :type string :description "File or directory to read; or give buffer")
                 (:name "buffer" :type string :description "Buffer to read; or give path"))
               superchat-tools--read-args)
 :function (lambda (args)
             (let ((path (plist-get args :path)) (name (plist-get args :buffer)))
               (superchat-tools--either path name)
               (if name
                   (with-current-buffer (superchat-tools--buffer name)
                     (superchat-tools--lines args "buffer"))
                 (let ((file (superchat-tools--path path)))
                   (cond ((file-directory-p file) (superchat-tools--listing file args))
                         ((find-buffer-visiting file)
                          (with-current-buffer (find-buffer-visiting file)
                            (superchat-tools--lines args "file")))
                         (t (with-temp-buffer (insert-file-contents file)
                                              (superchat-tools--lines args "file")))))))))

;;; Processes

(defconst superchat-tools--late
  "Stopped after 10 seconds. Give a narrower path or glob"
  "The notice of a listing or search that ran out of time.")

(defun superchat-tools--spawn (command seconds finish &optional enough spill)
  "Run COMMAND, a list, with no input.  Call FINISH once with the output and
why it ended: the exit status, `timeout' after SECONDS, `cut' when the output
passed four times `superchat-tool-max-chars', or `enough' when ENOUGH, called
in the process buffer, says so.  With SPILL, a function called in the process
buffer once it holds more than that, the process goes on: SPILL makes room,
and returns non-nil when the output is too large to go on (`cut').
Return a function that kills the process and cancels the timer; FINISH is then
not called.  The buffer is killed always."
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
                               (cond ((<= (buffer-size) limit)
                                      (when (and enough (funcall enough)) (end 'enough)))
                                     (spill (when (funcall spill) (end 'cut)))
                                     (t (delete-region (1+ limit) (point-max))
                                        (end 'cut))))))
                 :sentinel (lambda (process _)
                             (unless (process-live-p process)
                               (end (process-exit-status process))))))
        (error (kill-buffer buffer) (signal (car err) (cdr err))))
      (setq timer (run-at-time seconds nil (lambda () (end 'timeout))))
      (process-send-eof process)
      (lambda () (end nil)))))

;;; Changing

(defun superchat-tools--either (path buffer)
  "An error unless exactly one of PATH and BUFFER is given."
  (unless (xor path buffer) (error "Give exactly one of path and buffer")))

(defun superchat-tools--visit (file function)
  "Call FUNCTION in the buffer visiting FILE, visiting it when none does.
A buffer visited here is killed again when FUNCTION signals: nothing in it
is worth keeping."
  (when (file-directory-p file) (error "%s is a directory" file))
  (let* ((old (find-buffer-visiting file))
         ;; Only local variables already trusted: a question here would
         ;; hold Emacs, and nobody may be watching.
         (buffer (or old (let ((enable-local-variables :safe)) (find-file-noselect file))))
         done)
    (unwind-protect
        (prog1 (with-current-buffer buffer (funcall function)) (setq done t))
      (unless (or done old) (kill-buffer buffer)))))

(defun superchat-tools--replace (old new)
  "Replace the one occurrence of OLD in the current buffer by NEW, one undo step.
Case and overlapping occurrences count."
  (when (string-empty-p old) (error "OLD is empty"))
  (let ((case-fold-search nil) (n 0))
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
        (undo-boundary)))))

(defun superchat-tools--save (unsaved)
  "Save the current buffer to its file, unless it visits none or UNSAVED, the
person's own changes it held before the tool changed it, which are theirs to
save.  Return the notices to add to the result: of the save, then of the
lint of what was saved."
  (cond
   ((not buffer-file-name) nil)
   (unsaved (list "Not saved: the buffer held unsaved changes of the person's; they save it"))
   (t (let ((before (buffer-substring-no-properties (point-min) (point-max))))
        (make-directory (file-name-directory buffer-file-name) t)
        (unless (file-exists-p buffer-file-name) (set-buffer-modified-p t))
        ;; Quiet: a save message in the echo area for each change is noise.
        (let ((require-final-newline nil) (make-backup-files nil) (save-silently t))
          (save-buffer))
        (cons (unless (string= before (buffer-substring-no-properties (point-min) (point-max)))
                "Saving changed the text (a hook of the person's, such as a formatter); read it before the next edit")
              (superchat-lint-check buffer-file-name))))))

(defun superchat-tools--change (path name function)
  "Call FUNCTION in buffer NAME, or in the buffer visiting PATH (visited when
none does), then save it; the notices of the save and its lint."
  (cl-flet ((change () (let ((unsaved (and buffer-file-name (buffer-modified-p))))
                         (funcall function)
                         (superchat-tools--save unsaved))))
    (if name
        (with-current-buffer (superchat-tools--buffer name) (change))
      (superchat-tools--visit (superchat-tools--path path) #'change))))

(superchat-tool-define
 "edit" :class 'write
 :description (superchat-tools--describe
               "Replace OLD by NEW in a file, or in an Emacs buffer, and save it."
               "OLD must occur exactly once. The person can undo it in the file's buffer.")
 :args '((:name "path" :type string :description "File to edit; or give buffer")
         (:name "buffer" :type string :description "Buffer to edit; or give path")
         (:name "old" :type string :description "Text to replace, exactly once" :required t)
         (:name "new" :type string :description "Replacement" :required t))
 :function (lambda (args)
             (let ((path (plist-get args :path)) (name (plist-get args :buffer)))
               (superchat-tools--either path name)
               (when (and path (not (find-buffer-visiting (superchat-tools--path path)))
                          (not (file-exists-p (superchat-tools--path path))))
                 (error "No such file: %s" path))
               (superchat-tools--noted
                (format "Replaced 1 block in %s." (or name path))
                (superchat-tools--change
                 path name (lambda () (superchat-tools--replace (plist-get args :old)
                                                                (plist-get args :new))))))))

(superchat-tool-define
 "write" :class 'write
 :description (superchat-tools--describe
               "Make CONTENT the whole text of a file, created with its directory if it does"
               "not exist, and save it.")
 :args '((:name "path" :type string :description "File to write" :required t)
         (:name "content" :type string :description "The whole new content" :required t))
 :function (lambda (args)
             (let ((content (plist-get args :content)))
               (superchat-tools--noted
                (format "Wrote %d characters to %s." (length content) (plist-get args :path))
                (superchat-tools--change
                 (plist-get args :path) nil
                 (lambda ()
                   ;; A new file in a new directory is visited read-only.
                   (unless (file-exists-p buffer-file-name) (setq buffer-read-only nil))
                   (save-restriction
                     (widen)
                     (undo-boundary)
                     (erase-buffer)
                     (insert content)
                     (undo-boundary))))))))

;;; Executing

(superchat-tool-define
 "eval_elisp" :class 'exec
 :description "Evaluate Emacs Lisp forms in order (lexical binding) in the running Emacs. Returns what was messaged, then the value of each form, one line each. Files go through read, edit and the other file tools; outside programs through shell."
 :args '((:name "code" :type string :description "One or more forms" :required t))
 :function (lambda (args)
             (let ((forms nil) (values nil)
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
                 ;; The debugger would hold the run, and Emacs with it, until
                 ;; the person leaves it: an error is the result instead.
                 (let ((debug-on-error nil) (debug-on-signal nil) (debug-on-quit nil))
                   (dolist (form (nreverse forms)) (push (eval form t) values)))
                 (concat (with-current-buffer log (buffer-substring-no-properties from (point-max)))
                         (let ((print-escape-newlines t))
                           (mapconcat #'prin1-to-string (nreverse values) "\n")))))))

(defun superchat-tools--tail (output file spilled)
  "The end of a command's OUTPUT that fits, whole lines, with the notice of
the cut.  FILE holds what came before OUTPUT, SPILLED newlines, or is nil.
When something is cut the whole output is in FILE, made here if needed."
  (let* ((text (string-remove-suffix "\n" output))
         (rest (and (not (string-empty-p text)) (nreverse (split-string text "\n"))))
         (total (+ spilled (length rest)))
         (room (superchat-tools--room))
         (size -1) (n 0) kept)
    (while (and rest (< n 2000) (<= (+ size 1 (length (car rest))) room))
      (cl-incf size (1+ (length (car rest))))
      (cl-incf n)
      (push (pop rest) kept))
    ;; A single line longer than the room: its end, where an error would be.
    (when (and rest (null kept))
      (push (substring (pop rest) (- room)) kept)
      (setq n 1))
    (when (or rest file)
      (let ((coding-system-for-write 'utf-8))
        (write-region output nil (or file (setq file (make-temp-file "superchat-shell-")))
                      t 'silent)))
    (cons (string-join kept "\n")
          (and file (format "Showing lines %d-%d of %d. Full output: %s"
                            (- total n -1) total total file)))))

(superchat-tool-define
 "shell" :class 'exec :async t
 :description "Run a shell command in the project directory, with no input: for outside programs (build, tests, git). Returns the end of its output, at most 2000 lines or the character limit (the whole output is then saved to a file the notice names), and its exit status. Never use it to read, list or search files (cat, head, tail, sed -n, ls, find, grep): use read (offset and limit for a range of lines; a directory is listed), find and grep (glob, ignore_case, context), which the user can see and judge. Act on the running Emacs with eval_elisp."
 :args '((:name "command" :type string :description "Command line" :required t)
         (:name "timeout" :type number :description "Seconds, default superchat-shell-seconds"))
 :function (lambda (args done)
             (let ((seconds (superchat-tools--number (plist-get args :timeout) superchat-shell-seconds))
                   (file nil) (spilled 0) (written 0))
               (superchat-tools--spawn
                (list shell-file-name "-c" (plist-get args :command))
                seconds
                (lambda (output why)
                  (let ((tail (superchat-tools--tail output file spilled)))
                    (funcall done (superchat-tools--noted
                                   (car tail)
                                   (list (cdr tail)
                                         (pcase why
                                           ('timeout (format "timed out after %s seconds and killed"
                                                             seconds))
                                           ('cut (format "output passed %d characters; the command was stopped"
                                                         written))
                                           (_ (format "exit status %s" why))))))))
                nil
                ;; Called in the process buffer: all but its tail goes to the file,
                ;; which may not grow without end (a command like `yes').
                (lambda ()
                  (let ((end (- (point-max) superchat-tool-max-chars))
                        (coding-system-for-write 'utf-8))
                    (unless file (setq file (make-temp-file "superchat-shell-")))
                    (cl-incf spilled (cl-count ?\n (buffer-substring-no-properties (point-min) end)))
                    (write-region (point-min) end file t 'silent)
                    (cl-incf written (- end (point-min)))
                    (delete-region (point-min) end)
                    (> written (* 200 superchat-tool-max-chars))))))))

(provide 'superchat-tools)
;;; superchat-tools.el ends here
