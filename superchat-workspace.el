;;; superchat-workspace.el --- Workspace resolution from a directory -*- lexical-binding: t; -*-

;;; Commentary:
;; This file decides which Workspace a directory or buffer belongs to.
;; A Workspace is a place where work happens: a directory Emacs
;; recognises as a project (contract W1), not necessarily a version
;; repository (W3), with exactly one root (W6).  Its identity is the
;; id kept in `.superchat/id' under the root, so it survives renames
;; (W4); without that file the id is a hash of the real path.  Only
;; `superchat-workspace-declare' writes the file.
;;
;; The Artifact Board lives in `superchat-artifact-board.el'; this file
;; only answers "which Workspace" and "what are its rules" (the
;; `AGENTS.md' under the root, contract R2).  Loading it has no side effects:
;; the project finder is installed by
;; `superchat-workspace-install-project-finder', which the default row
;; `workspace-resolution' calls when it is mounted and undoes when it is
;; unmounted.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'superchat-plugin)

(defconst superchat-workspace--id-max-length 128
  "Longest id, in characters, accepted from a `.superchat/id' file.")

(defun superchat-workspace--canonical-root (root)
  "Return ROOT as an absolute, symlink-free directory name."
  (file-name-as-directory (file-truename (expand-file-name root))))

(defun superchat-workspace-id-from-root (root)
  "Return the path-derived Workspace id for the directory ROOT.
The id is \"workspace-\" plus the first 12 characters of the SHA-256 of
ROOT's real path, the same value `superchat--workspace-binding-for-directory'
computes."
  (concat "workspace-"
          (substring (secure-hash 'sha256 (superchat-workspace--canonical-root root))
                     0 12)))

(defun superchat-workspace-id-file (root)
  "Return the absolute path of the id file under ROOT."
  (expand-file-name ".superchat/id" root))

(defun superchat-workspace-read-id (root)
  "Return the id stored in ROOT's id file, or nil.
Nil means the file is missing, unreadable, empty, longer than 128
characters, or contains anything outside [A-Za-z0-9._-].  Never signals."
  (condition-case nil
      (let ((file (superchat-workspace-id-file root)))
        (when (and (file-regular-p file) (file-readable-p file))
          (let ((text (string-trim
                       (with-temp-buffer
                         (insert-file-contents file nil 0 1024)
                         (buffer-string)))))
            (when (and (> (length text) 0)
                       (<= (length text) superchat-workspace--id-max-length)
                       (string-match-p "\\`[A-Za-z0-9._-]+\\'" text))
              text))))
    (error nil)))

(defun superchat-workspace--from-root (root name)
  "Return the Workspace plist for ROOT, called NAME."
  (list :id (or (superchat-workspace-read-id root)
                (superchat-workspace-id-from-root root))
        :root root
        :name name
        :kind 'project))

(defun superchat-workspace-of-directory (dir)
  "Return the Workspace containing DIR, or nil.
The result is (:id ID :root ROOT :name NAME :kind project).  Emacs'
project machinery decides membership; the id is the id file's content
when valid, else the path-derived id.  Never prompts and never creates
files; file errors give nil."
  (condition-case nil
      (when (and (stringp dir) (file-directory-p dir))
        (let ((pr (project-current nil dir)))
          (when pr
            (superchat-workspace--from-root
             (superchat-workspace--canonical-root (project-root pr))
             (project-name pr)))))
    (file-error nil)))

(defun superchat-workspace-of-buffer (&optional buffer)
  "Return the Workspace of BUFFER's `default-directory', or nil.
BUFFER defaults to the current buffer; a dead buffer gives nil."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (superchat-workspace-of-directory
       (buffer-local-value 'default-directory buffer)))))

;;;###autoload
(defun superchat-workspace-declare (dir)
  "Make DIR part of a Workspace by ensuring an id file exists, and return it.
If DIR is already inside a project, the id file goes under that
project's root; otherwise DIR becomes the root.  An existing valid id
file is left untouched; otherwise the id currently in use is written,
followed by a newline.  Signals a `user-error' unless DIR is an existing
directory."
  (interactive (list (read-directory-name "Declare workspace directory: "
                                          default-directory nil t)))
  (unless (and (stringp dir) (file-directory-p dir))
    (user-error "Not a directory: %s" dir))
  (let* ((existing (superchat-workspace-of-directory dir))
         (root (or (plist-get existing :root)
                   (superchat-workspace--canonical-root dir)))
         (name (or (plist-get existing :name)
                   (file-name-nondirectory (directory-file-name root)))))
    (unless (superchat-workspace-read-id root)
      (let ((file (superchat-workspace-id-file root)))
        (make-directory (file-name-directory file) t)
        (write-region (concat (superchat-workspace-id-from-root root) "\n")
                      nil file nil 'silent)))
    (when (called-interactively-p 'interactive)
      (message "Declared workspace root: %s" root))
    (superchat-workspace--from-root root name)))

;;;; Rules ------------------------------------------------------------------------

(defconst superchat-workspace-remember-note-with-file
  "When the user explicitly asks you to remember something, or says \"from now on\" or \"always\", add it to %s: open that file as a buffer, edit the buffer, and tell the user what you wrote.  It takes effect once the user saves the file.  Do not write down preferences you only inferred."
  "What a request says about remembering when the Workspace has a rules file.
The one %s is the file's path (contract R4).")

(defconst superchat-workspace-remember-note-without-file
  "This workspace has no rules file yet.  When the user explicitly asks you to remember something, or says \"from now on\" or \"always\", write it to %s: open that path as a buffer (a new file starts as an empty buffer), edit the buffer, and tell the user what you wrote.  It takes effect once the user saves the file.  Do not write down preferences you only inferred."
  "What a request says about remembering when the Workspace has no rules file.
The one %s is the path `AGENTS.md' would have under the root.")

(defconst superchat-workspace-temporary-note
  "This is a temporary conversation: it belongs to no workspace, and nothing said here is carried into later conversations.  If the user asks you to remember something for later, say that it will not be kept here, and that `/workspace DIRECTORY' in this window makes a directory a workspace whose rules are kept in its AGENTS.md."
  "What a request says about remembering in a conversation bound to no Workspace
\(contract W5, W10).")

(defconst superchat-workspace-rules-max-chars 6000
  "Most characters of a rules file that go into one request.")

(defconst superchat-workspace-rules-file-names '("AGENTS.md" "CLAUDE.md")
  "File names looked for under a Workspace root, first one found wins.")

(defun superchat-workspace-rules-file (root)
  "Return the rules file of the Workspace rooted at ROOT, or nil.
That is `AGENTS.md' under ROOT, else `CLAUDE.md', when it is a readable
regular file.  Nil when ROOT is not an existing directory."
  (when (and (stringp root) (file-directory-p root))
    (cl-some (lambda (name)
               (let ((file (expand-file-name name root)))
                 (and (file-regular-p file) (file-readable-p file) file)))
             superchat-workspace-rules-file-names)))

(defun superchat-workspace-rules-text (root)
  "Return the rules of the Workspace rooted at ROOT as request text, or nil.
The text is read from the file on disk, never from a buffer, so an unsaved
edit is not in it (contract R4: saving is the user's consent).  It names the
file, and is cut at `superchat-workspace-rules-max-chars' characters with a
closing line that says so.  Nil when there is no rules file or it is blank.
Never signals."
  (condition-case nil
      (let ((file (superchat-workspace-rules-file root)))
        (when file
          (let* ((body (string-trim
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
                 (cut (> (length body) superchat-workspace-rules-max-chars)))
            (unless (string-empty-p body)
              (concat "Workspace rules, from " file
                      ".  The user keeps these for all work in this workspace; follow them.\n\n"
                      (if cut
                          (substring body 0 superchat-workspace-rules-max-chars)
                        body)
                      (if cut
                          (format "\n\n[Cut at %d characters; read %s for the rest.]"
                                  superchat-workspace-rules-max-chars file)
                        ""))))))
    (error nil)))

(defun superchat-workspace-remember-note (root)
  "Return what a request says about remembering, for the Workspace at ROOT.
With a rules file, the model is told to edit that file's buffer; with a
root but no rules file, to start `AGENTS.md' there; with a nil ROOT (a
temporary conversation), that nothing is kept.  The model writes only on
the user's explicit word, and the user's save is what makes it a rule
\(contract R4)."
  (if (and (stringp root) (file-directory-p root))
      (let ((file (superchat-workspace-rules-file root)))
        (if file
            (format superchat-workspace-remember-note-with-file file)
          (format superchat-workspace-remember-note-without-file
                  (expand-file-name "AGENTS.md" root))))
    superchat-workspace-temporary-note))

(defun superchat-workspace-project-finder (dir)
  "Return a project for the nearest `.superchat/' directory above DIR, or nil.
Suitable for `project-find-functions'.  The user's home directory is
never taken as a root.  Nil when DIR does not exist."
  (when (and (stringp dir) (file-directory-p dir))
    (let ((root (locate-dominating-file
                 dir (lambda (d) (file-directory-p (expand-file-name ".superchat" d))))))
      (when (and root
                 (not (file-equal-p root (expand-file-name "~"))))
        (cons 'superchat-workspace (file-name-as-directory (expand-file-name root)))))))

(cl-defmethod project-root ((project (head superchat-workspace)))
  "Return the root directory of the declared Workspace PROJECT."
  (cdr project))

(defun superchat-workspace-install-project-finder ()
  "Add `superchat-workspace-project-finder' to `project-find-functions'.
It is added at depth 90, after Emacs' own finders, so it only matters
in directories Emacs does not already recognise."
  (add-hook 'project-find-functions #'superchat-workspace-project-finder 90))

(defun superchat-workspace-uninstall-project-finder ()
  "Remove `superchat-workspace-project-finder' from `project-find-functions'."
  (remove-hook 'project-find-functions #'superchat-workspace-project-finder))

(defun superchat-workspace-temporary (session-id)
  "Return the temporary Workspace for SESSION-ID, which has no root.
Signals an error unless SESSION-ID is a non-empty string."
  (unless (and (stringp session-id) (> (length session-id) 0))
    (error "Temporary workspace needs a non-empty session id: %S" session-id))
  (list :id (concat "workspace-tmp-" session-id)
        :root nil
        :name "temporary"
        :kind 'temporary))

(defun superchat-workspace-temporary-p (workspace)
  "Return non-nil when WORKSPACE is a temporary Workspace."
  (eq (plist-get workspace :kind) 'temporary))

(superchat-define-plugin workspace-resolution
  :doc "Workspace resolution: a directory holding `.superchat/' is a project
for all of Emacs, through a finder on `project-find-functions' that runs
after Emacs' own."
  :apply
  (lambda (scope _config)
    (superchat-workspace-install-project-finder)
    (superchat-scope-effect scope
                            #'superchat-workspace-uninstall-project-finder)))

(provide 'superchat-workspace)
;;; superchat-workspace.el ends here
