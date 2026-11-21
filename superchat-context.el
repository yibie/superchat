;;; superchat-context.el --- The system prompt of a request -*- lexical-binding: t; -*-

;;; Commentary:
;; Built fresh for every request from the Tape and the files of the user:
;; nothing here is remembered between requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-view)
(require 'superchat-tools)

(defconst superchat-context--base
  "You are Superchat, an assistant inside the user's Emacs.
You act on Emacs, its buffers and files through tools.  Use a tool when you
need a fact; do not guess file contents, buffer contents or the state of Emacs.

Read and change files with `read`, `grep`, `find`, `edit` and `write`;
`edit` and `write` save the file.  Use `shell` for outside programs and
`eval_elisp` for the running Emacs.
Calls that do not wait on each other go in one response: read the files you
need together, and look at another file while a check runs.
Read before you change: look at a file or buffer first, then edit it.
Make the smallest change that does what was asked, and say what you changed.
Each tool result starts with its number, #ID.  When your answer rests on
results, end it with one line naming those and no others, the probes and
failed tries left out: Sources: #ID, #ID

The user sets a permission level.  A tool call may be denied or ask the user
first.  When one is denied, do not look for another way round it: say what
you could not do and why, and let the user decide.

Everything said in earlier conversations is on the Tape.  The `recall` tool
searches it and `note` keeps a conclusion worth keeping.  Search the Tape before saying that you do
not know something the user may have told you.

Answer briefly and plainly.  When a request is unclear, ask one question."
  "What every request is told first.")

(defun superchat-context--directory (tape)
  "The line that says where a tool's relative path of TAPE is against, or
that it has none and works in Emacs, from the buffer it came from."
  (if-let* ((directory (superchat-tape-directory tape)))
      (format "The working directory is %s: a tool's relative path is against it\
 and `shell' runs there.  Give an absolute path for any other file." directory)
    (let ((buffer (plist-get (plist-get (plist-get (car (superchat-tape-entries
                                                          :tape tape :kinds '(user) :newest t :limit 1))
                                                    :data)
                                         :origin)
                             :buffer)))
      (concat "This conversation has no working directory: it works in Emacs."
              (and buffer (format " It came from the buffer %s, which `read' and `edit' take as `buffer'." buffer))
              " `eval_elisp' reaches the rest of Emacs.  A file tool needs an absolute path;\
 `shell' runs in a scratch directory of its own.  When the person names a\
 file without its path, ask where it is: do not search the disk for it."))))

(defun superchat-context--read (file)
  "The text of FILE, or nil when it does not exist."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun superchat-context--rules (scope)
  "The AGENTS.md of SCOPE, a project root or \"home\"."
  (when-let* ((text (superchat-context--read
                     (expand-file-name "AGENTS.md" (if (equal scope "home")
                                                       superchat-directory
                                                     scope)))))
    (concat "Rules of this " (if (equal scope "home") "user" "project") ":\n" text)))

(defun superchat-context--description (file)
  "The description: line of the frontmatter of FILE, or nil.
Frontmatter is only at the very start and ends at the first line that is `---'."
  (when-let* ((text (superchat-context--read file))
              (lines (split-string text "\n")))
    ;; With no closing line there is no frontmatter, only a body.
    (when (and (equal (car lines) "---") (member "---" (cdr lines)))
      (cl-loop for line in (cdr lines)
               until (equal line "---")
               when (string-match "\\`description:[ \t]*\\(.*?\\)[ \t]*\\'" line)
               return (match-string 1 line)))))

(defvar superchat-context-skills-listed 20
  "Above this many skills the catalog is one line, which says where to find them.")

(defun superchat-context--skill-files (scope)
  "The SKILL.md files of SCOPE and the user, a project's hiding the user's of
the same name."
  (let ((dirs (append (unless (or (null scope) (equal scope "home"))
                        (list (expand-file-name ".superchat/skills/" scope)))
                      (list (expand-file-name "skills/" superchat-directory)))))
    (cons dirs
          (cl-loop with seen for dir in dirs
                   append (cl-loop for file in (file-expand-wildcards (expand-file-name "*/SKILL.md" dir))
                                   for name = (superchat-context--skill-name file)
                                   unless (member name seen)
                                   do (push name seen) and collect file)))))

(defun superchat-context--skill-name (file)
  "The name of the skill whose SKILL.md is FILE."
  (file-name-nondirectory (directory-file-name (file-name-directory file))))

(defun superchat-context--interface (file)
  "The entries of the interface.eld beside FILE, read and never evaluated;
an entry not of the shape is left out."
  (let ((eld (expand-file-name "interface.eld" (file-name-directory file))))
    (when-let* ((text (superchat-context--read eld)))
      (seq-filter
       (lambda (e)
         (and (proper-list-p e)
              (stringp (plist-get e :name)) (string-match-p "\\`[a-z0-9_]+\\'" (plist-get e :name))
              (memq (plist-get e :class) '(read write exec))
              (stringp (plist-get e :code))
              (cl-every (lambda (a) (and (stringp (plist-get a :name))
                                         (memq (plist-get a :type) '(string number boolean))))
                        (plist-get e :args))))
       (condition-case nil (car (read-from-string text)) (error nil))))))

(defun superchat-context--fill (code args values)
  "The form CODE with each symbol named as one of ARGS replaced by that
argument's value from VALUES, checked against its type: a value, never code."
  (let ((given (mapcar (lambda (a)
                         (let ((v (plist-get values (intern (concat ":" (plist-get a :name))))))
                           (unless (pcase (plist-get a :type)
                                     ('string (stringp v)) ('number (numberp v))
                                     ('boolean (memq v '(t nil :false))))
                             (error "Argument %s must be a %s" (plist-get a :name) (plist-get a :type)))
                           (cons (intern (plist-get a :name)) (if (eq v :false) nil v))))
                       args)))
    (cl-labels ((walk (x) (cond ((and (symbolp x) (assq x given)) (list 'quote (cdr (assq x given))))
                                ((consp x) (cons (walk (car x)) (walk (cdr x))))
                                (t x))))
      (walk (car (read-from-string code))))))

(defun superchat-context-interfaces (tape)
  "Make the interfaces of the skills of TAPE's scope its deferred tools, named
SKILL__NAME; those of other skills go."
  (dolist (name (superchat-tool-skill-tools)) (superchat-tool-remove name))
  (let ((scope (plist-get (car (superchat-tape-entries :tape tape :newest t :limit 1)) :scope)))
    (dolist (file (cdr (superchat-context--skill-files scope)))
      (let ((skill (superchat-context--skill-name file)))
        (dolist (e (superchat-context--interface file))
          (let ((args (plist-get e :args)) (code (plist-get e :code)))
            (superchat-tool-define
             (format "%s__%s" (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" skill) (plist-get e :name))
             :class (plist-get e :class) :deferred t :skill skill
             :description (or (plist-get e :doc) code)
             :args (mapcar (lambda (a) (append a (list :required t))) args)
             :function (lambda (values)
                         (let ((debug-on-error nil) (print-escape-newlines t))
                           (prin1-to-string
                            (superchat-tool-plain (eval (superchat-context--fill code args values) t))))))))))))

(defun superchat-context--skills (scope)
  "The catalog of skills of SCOPE and the user: name, description and path,
never the body; a project's skill hides the user's of the same name."
  (pcase-let ((`(,dirs . ,files) (superchat-context--skill-files scope)))
    (cond
     ((null files) nil)
     ((> (length files) superchat-context-skills-listed)
      (format "There are %d skills, each a SKILL.md under %s.  Find one that fits the task\
 with find and grep over their description: lines, then read it."
              (length files) (mapconcat #'abbreviate-file-name dirs " and ")))
     (t (concat "Skills you can use.  Read a skill's file with read when its\
 description fits the task:\n"
                (mapconcat
                 (lambda (file)
                   (let ((n (length (superchat-context--interface file))))
                     (format "- %s: %s (%s)%s" (superchat-context--skill-name file)
                             (or (superchat-context--description file) "no description") file
                             (if (> n 0)
                                 (format "; %d tools: load_tools %s" n (superchat-context--skill-name file))
                               ""))))
                 files "\n"))))))

(defun superchat-context--notes (scope)
  "The notes of SCOPE, newest first; the render's cut drops the oldest."
  (when-let* ((notes (superchat-view 'notes :scope scope)))
    (concat "Notes kept from earlier conversations:\n"
            (superchat-view-render notes 2000))))

(defun superchat-context--origin (tape)
  "Where the latest user entry of TAPE came from, when it says."
  (when-let* ((entry (car (superchat-tape-entries :tape tape :kinds '(user)
                                                  :newest t :limit 1)))
              (origin (plist-get (plist-get entry :data) :origin)))
    (format "The user wrote this from buffer %s%s%s%s."
            (plist-get origin :buffer)
            (if-let* ((file (plist-get origin :file))) (format ", file %s" file) "")
            (if-let* ((mode (plist-get origin :mode))) (format ", mode %s" mode) "")
            (if-let* ((line (plist-get origin :line))) (format ", line %s" line) ""))))

(defun superchat-context-system (tape &optional extra)
  "The system prompt for timeline TAPE, with the string EXTRA last.
A timeline with no entry has no scope, so no rules file and no notes."
  (let ((scope (plist-get (car (superchat-tape-entries :tape tape :newest t :limit 1))
                          :scope)))
    (string-join
     (delq nil (list superchat-context--base
                     (and scope (superchat-context--directory tape))
                     (and scope (superchat-context--rules scope))
                     (superchat-context--skills scope)
                     (and scope (superchat-context--notes scope))
                     (superchat-context--origin tape)
                     extra))
     "\n\n")))

(provide 'superchat-context)
;;; superchat-context.el ends here
