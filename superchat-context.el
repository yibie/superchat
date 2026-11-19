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

Read and change files with `read`, `grep`, `find`, `ls`, `edit` and `write`;
edits stay in buffers until `save_buffer`.  Save before running a command on
a changed file.  Use `shell` for outside programs and `eval_elisp` for the
running Emacs.
Read before you change: look at a file or buffer first, then edit it.
Make the smallest change that does what was asked, and say what you changed.

The user sets a permission level.  A tool call may be denied or ask the user
first.  When one is denied, do not look for another way round it: say what
you could not do and why, and let the user decide.

Everything said in earlier conversations is on the Tape.  The `recall` tool
searches it and `note` keeps a conclusion worth keeping.  Search the Tape
before saying that you do not know something the user may have told you.

Answer briefly and plainly.  When a request is unclear, ask one question."
  "What every request is told first.")

(defun superchat-context--directory (scope)
  "The line that says where a tool's relative path is against."
  (format "The working directory is %s: a tool's relative path is against it\
 and `shell' runs there.  Give an absolute path for any other file."
          (if (equal scope "home") (expand-file-name "~/") scope)))

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

(defun superchat-context--skills ()
  "The catalog of skills: name, description and path, never the body."
  (let ((files (file-expand-wildcards
                (expand-file-name "skills/*/SKILL.md" superchat-directory))))
    (when files
      (concat "Skills you can use.  Read a skill's file with read when its\
 description fits the task:\n"
              (mapconcat
               (lambda (file)
                 (format "- %s: %s (%s)"
                         (file-name-nondirectory (directory-file-name (file-name-directory file)))
                         (or (superchat-context--description file) "no description")
                         file))
               files "\n")))))

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
                     (and scope (superchat-context--directory scope))
                     (and scope (superchat-context--rules scope))
                     (superchat-context--skills)
                     (and scope (superchat-context--notes scope))
                     (superchat-context--origin tape)
                     extra))
     "\n\n")))

(provide 'superchat-context)
;;; superchat-context.el ends here
