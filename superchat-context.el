;;; superchat-context.el --- The system prompt of a request -*- lexical-binding: t; -*-

;;; Commentary:
;; Built fresh for every request from the Tape and the files of the user:
;; nothing here is remembered between requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-view)

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

The user sets a permission level.  A tool call may be denied or ask the user
first.  When one is denied, do not look for another way round it: say what
you could not do and why, and let the user decide.

Everything said in earlier conversations is on the Tape.  The `recall` tool
searches it and `note` keeps a conclusion worth keeping; load them with
`load_tools` when you need them.  Search the Tape before saying that you do
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

(defun superchat-context--skills (scope)
  "The catalog of skills of SCOPE and the user: name, description and path,
never the body; a project's skill hides the user's of the same name."
  (let* ((dirs (append (unless (or (null scope) (equal scope "home"))
                         (list (expand-file-name ".superchat/skills/" scope)))
                       (list (expand-file-name "skills/" superchat-directory))))
         (files (cl-loop with seen for dir in dirs
                         append (cl-loop for file in (file-expand-wildcards (expand-file-name "*/SKILL.md" dir))
                                         for name = (file-name-nondirectory
                                                     (directory-file-name (file-name-directory file)))
                                         unless (member name seen)
                                         do (push name seen) and collect file))))
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
                   (format "- %s: %s (%s)"
                           (file-name-nondirectory (directory-file-name (file-name-directory file)))
                           (or (superchat-context--description file) "no description")
                           file))
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
