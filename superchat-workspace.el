;;; superchat-workspace.el --- Workspace resolution from a directory -*- lexical-binding: t; -*-

;;; Commentary:
;; This file decides which Workspace a directory or buffer belongs to.
;; A Workspace is a record Superchat keeps -- an id, a name, a kind and a
;; membership rule -- and a directory is only one of its attributes
;; (docs/decisions/accepted/2026-11-10-a-workspace-is-a-record-not-a-directory.md).
;; There are two kinds:
;;
;; - A project: a directory Emacs recognises as a project (contract W1),
;;   not necessarily a version repository (W3), with exactly one root (W6).
;;   Its identity is the id kept in `.superchat/id' under the root, so it
;;   survives renames (W4); without that file the id is a hash of the real
;;   path.  Only `superchat-workspace-declare' writes the file.
;; - Home: the one Workspace with no root.  It always exists, belongs to the
;;   user, and receives every buffer no project claims.  Its id and its rules
;;   are kept under `superchat-data-directory' (`home/id', `home/AGENTS.md'),
;;   never in a `.superchat/' directory, so no project finder takes it for a
;;   project.  Nothing runs in that directory and it bounds nothing.
;;
;; Interfaces that belong to no directory -- the agenda, feeds, chats --
;; belong to Home, or to the Personal Workspace when the user designated a
;; directory for them (W11); `superchat-workspace-of-source' is the one place
;; that decides which Workspace a request made from a buffer belongs to.
;; A Home conversation is also its source's: `superchat-source-key' names
;; the buffer a request comes from, so the same chat, feed entry, file or
;; buffer continues the same conversation.
;;
;; The Artifact Board lives in `superchat-artifact-board.el'; this file
;; only answers "which Workspace" and "what are its rules" (the
;; `AGENTS.md' under the root, contract R2, or Home's own).  Loading it has
;; no side effects: the project finder is installed by
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
      (superchat-workspace--read-id-file (superchat-workspace-id-file root))
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

;;;; The Personal Workspace and where a source buffer belongs ---------------------

(defgroup superchat-workspace nil
  "Which Workspace a buffer belongs to."
  :group 'superchat)

(defcustom superchat-personal-workspace-directory nil
  "The directory of the Personal Workspace, or nil for none.
The Personal Workspace is the home of the interfaces that belong to no
directory -- the agenda, feeds, chats: buffers in one of
`superchat-personal-workspace-modes'.  It is a Workspace like any other,
with its own `AGENTS.md' and conversations, and no other Workspace sees
its rules.  While this is nil, a conversation called from those
interfaces belongs to Home (`superchat-workspace-home'), one conversation
per source.  Set it with `superchat-set-personal-workspace', which also
declares the directory."
  :type '(choice (const :tag "None" nil) directory)
  :group 'superchat-workspace)

(defcustom superchat-personal-workspace-modes
  '(org-agenda-mode calendar-mode diary-mode
    elfeed-search-mode elfeed-show-mode newsticker-treeview-mode
    telega-root-mode telega-chat-mode
    gnus-group-mode gnus-summary-mode gnus-article-mode
    mu4e-main-mode mu4e-headers-mode mu4e-view-mode
    notmuch-hello-mode notmuch-search-mode notmuch-show-mode
    erc-mode rcirc-mode)
  "Major modes whose buffers belong to the Personal Workspace.
A buffer in one of these modes, or a mode derived from one, belongs to
the Personal Workspace whatever its `default-directory' happens to be;
while `superchat-personal-workspace-directory' is nil, it belongs to
Home, never to the project its `default-directory' happens to lie in."
  :type '(repeat symbol)
  :group 'superchat-workspace)

(defvar-local superchat-buffer-workspace-directory nil
  "When non-nil, the directory whose Workspace this buffer belongs to.
It wins over the buffer's mode and `default-directory'.  Set it in a mode
hook to give one interface to a Workspace other than the personal one:

  (add-hook \='elfeed-search-mode-hook
            (lambda ()
              (setq-local superchat-buffer-workspace-directory \"~/research/\")))")

(defun superchat-workspace-at-directory (dir)
  "Return the Workspace DIR belongs to, DIR itself being the root if need be.
Inside a project that is the project's Workspace; elsewhere DIR is the
root, named after it.  Nil unless DIR is an existing directory.  Never
creates files."
  (when (and (stringp dir) (file-directory-p dir))
    (or (superchat-workspace-of-directory dir)
        (let ((root (superchat-workspace--canonical-root dir)))
          (superchat-workspace--from-root
           root (file-name-nondirectory (directory-file-name root)))))))

(defun superchat-workspace-personal ()
  "Return the Personal Workspace, or nil when none is designated.
Nil too when `superchat-personal-workspace-directory' is not an existing
directory."
  (superchat-workspace-at-directory superchat-personal-workspace-directory))

(defun superchat-workspace-personal-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an interface of the Personal Workspace.
That is a live buffer whose major mode is, or derives from, one of
`superchat-personal-workspace-modes'.  BUFFER defaults to the current one."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (apply #'derived-mode-p superchat-personal-workspace-modes))
         t)))

;;;; Home: the Workspace with no root ------------------------------------------

(defconst superchat-workspace-home-name "Home"
  "The stored name of Home, the Workspace of everything in no project.")

(defvar superchat-data-directory)

(defun superchat-workspace-home-directory ()
  "Return the directory where Home keeps its id and rules, or nil.
That is `home/' under `superchat-data-directory'.  It is a store, not a
root: no command runs there, no file is inside or outside of it, and no
project is looked for there."
  (and (boundp 'superchat-data-directory)
       (stringp superchat-data-directory)
       (not (string-empty-p superchat-data-directory))
       (file-name-as-directory
        (expand-file-name "home" superchat-data-directory))))

(defun superchat-workspace-home-rules-path ()
  "Return the path of Home's one rules file, `AGENTS.md' in its store, or nil."
  (when-let* ((dir (superchat-workspace-home-directory)))
    (expand-file-name "AGENTS.md" dir)))

(defun superchat-workspace--read-id-file (file)
  "Return the id stored in FILE, or nil.
Nil means the file is missing, unreadable, empty, longer than
`superchat-workspace--id-max-length' characters, or contains anything
outside [A-Za-z0-9._-].  Never signals."
  (condition-case nil
      (when (and (file-regular-p file) (file-readable-p file))
        (let ((text (string-trim
                     (with-temp-buffer
                       (insert-file-contents file nil 0 1024)
                       (buffer-string)))))
          (when (and (> (length text) 0)
                     (<= (length text) superchat-workspace--id-max-length)
                     (string-match-p "\\`[A-Za-z0-9._-]+\\'" text))
            text)))
    (error nil)))

(defun superchat-workspace-home-id ()
  "Return Home's id as kept in its store, or nil when it has none yet.
Never creates files."
  (when-let* ((dir (superchat-workspace-home-directory)))
    (superchat-workspace--read-id-file (expand-file-name "id" dir))))

(defun superchat-workspace-home ()
  "Return Home: (:id ID :root nil :name \"Home\" :kind home).
Its id is read from `id' in `superchat-workspace-home-directory', and
written there the first time it is needed.  Nil only when the data
directory is unset."
  (when-let* ((dir (superchat-workspace-home-directory)))
    (list :id (or (superchat-workspace-home-id)
                  (let ((id (format "home-%012x" (random (expt 16 12)))))
                    (make-directory dir t)
                    (write-region (concat id "\n") nil (expand-file-name "id" dir)
                                  nil 'silent)
                    id))
          :root nil
          :name superchat-workspace-home-name
          :kind 'home)))

(defun superchat-workspace-home-id-p (id)
  "Return non-nil when ID is Home's id.  Never creates files."
  (and (stringp id) (not (string-empty-p id))
       (equal id (superchat-workspace-home-id))))

(defun superchat-workspace-home-p (workspace)
  "Return non-nil when the Workspace or binding plist WORKSPACE is Home.
A plist that names no kind is Home when it has no root and Home's id, as a
binding rebuilt from an old fact or a record may be."
  (and (consp workspace)
       (let ((kind (plist-get workspace :kind)))
         (if kind
             (member (format "%s" kind) '("home"))
           (and (null (plist-get workspace :root))
                (superchat-workspace-home-id-p (plist-get workspace :id)))))
       t))

(defun superchat-workspace-home-config-p (config)
  "Return non-nil when the Session configuration CONFIG is bound to Home.
CONFIG is what `superchat-view-session-configuration' returns: its
`workspace.bound' fact says `:workspace-kind' home, or, for a fact that
names no kind, has no root and Home's id."
  (let ((id (plist-get config :workspace-id))
        (root (plist-get config :workspace-root))
        (kind (plist-get config :workspace-kind)))
    (and (stringp id) (not (string-empty-p id))
         (not (and (stringp root) (not (string-empty-p root))))
         (if kind
             (equal (format "%s" kind) "home")
           (superchat-workspace-home-id-p id))
         t)))

(defun superchat-workspace-rules-root (id root &optional kind)
  "Return the directory holding the rules of the Workspace ID at ROOT, or nil.
A project's root; for Home (KIND `home', or no root and Home's id), Home's
store; nil for a conversation of no Workspace."
  (cond ((and (stringp root) (not (string-empty-p root))) root)
        ((and (stringp id) (not (string-empty-p id))
              (if kind
                  (equal (format "%s" kind) "home")
                (superchat-workspace-home-id-p id)))
         (superchat-workspace-home-directory))))

(defun superchat-workspace--home-store-p (dir)
  "Return non-nil when DIR names Home's store directory."
  (when-let* ((home (superchat-workspace-home-directory)))
    (and (stringp dir)
         (equal (file-name-as-directory (expand-file-name dir)) home))))

(defun superchat-workspace-home-rules-file-p (file)
  "Return non-nil when FILE is Home's one rules file, `home/AGENTS.md'.
FILE is a canonical path (`superchat-sandbox-canonical-path'); it is
compared with `AGENTS.md' in the store's real directory.  A rules file that
is itself a link to another file under the data directory is therefore not
this file.  This is the one file under `superchat-data-directory' the model
may open (D8)."
  (when-let* ((dir (superchat-workspace-home-directory)))
    (and (stringp file)
         (equal (expand-file-name file)
                (expand-file-name "AGENTS.md" (file-truename dir))))))

;;;; Which source a request is made from (D12, D13) ------------------------------

(defcustom superchat-source-key-functions
  '((telega-chat-mode . superchat-source-key-telega-chat)
    (elfeed-show-mode . superchat-source-key-elfeed-entry)
    (eww-mode . superchat-source-key-eww)
    (dired-mode . superchat-source-key-dired)
    (Info-mode . superchat-source-key-info)
    (gnus-summary-mode . superchat-source-key-gnus-summary)
    (notmuch-search-mode . superchat-source-key-notmuch-search)
    (notmuch-show-mode . superchat-source-key-notmuch-show)
    (rcirc-mode . superchat-source-key-rcirc))
  "How a buffer of a major mode names the source it shows, for Home.
Each element is (MODE . FUNCTION).  FUNCTION is called with no argument in
a buffer whose major mode is, or derives from, MODE, and returns the
source's key -- a string, coarse to fine, compared whole -- or nil when it
cannot tell.  The first function that returns a key wins.  A buffer no
function names is keyed by the file it visits (`file:PATH'), else by its
mode and name (`buffer:MODE:NAME').  Add an element for a mode whose
buffers all share one name, or every one of them continues the same
conversation."
  :type '(alist :key-type symbol :value-type function)
  :group 'superchat-workspace)

(defun superchat-source-key--join (&rest parts)
  "Join PARTS with colons, each printed with `format' %s."
  (mapconcat (lambda (part) (format "%s" part)) parts ":"))

(defvar telega-chatbuf--chat)
(defvar telega-chatbuf--topic)
(declare-function telega-topic-id "ext:telega-topic" (topic))

(defun superchat-source-key-telega-chat ()
  "The key of a telega chat buffer: `telega:chat:ID', plus `:topic:ID'.
ID is the chat's Telegram id from `telega-chatbuf--chat'; the topic is the
forum topic the buffer shows now, when telega gives its id.  The account
is not part of the key."
  (let ((chat (bound-and-true-p telega-chatbuf--chat)))
    (when-let* ((id (and (listp chat) (plist-get chat :id))))
      (let* ((topic (bound-and-true-p telega-chatbuf--topic))
             (topic-id (and topic (fboundp 'telega-topic-id)
                            (ignore-errors (telega-topic-id topic)))))
        (if topic-id
            (superchat-source-key--join "telega" "chat" id "topic" topic-id)
          (superchat-source-key--join "telega" "chat" id))))))

(defvar elfeed-show-entry)
(declare-function elfeed-entry-id "ext:elfeed-db" (entry))

(defun superchat-source-key-elfeed-entry ()
  "The key of an elfeed entry buffer: `elfeed:entry:NAMESPACE:ID'.
From `elfeed-entry-id' of `elfeed-show-entry'; every entry buffer has the
same name, so this function is what keeps two entries apart."
  (when-let* ((entry (bound-and-true-p elfeed-show-entry))
              ((fboundp 'elfeed-entry-id))
              (id (ignore-errors (elfeed-entry-id entry))))
    (if (consp id)
        (superchat-source-key--join "elfeed" "entry" (car id) (cdr id))
      (superchat-source-key--join "elfeed" "entry" id))))

(defvar eww-data)

(defun superchat-source-key-eww ()
  "The key of an eww buffer: `eww:URL', the page it shows now."
  (let ((url (and (boundp 'eww-data) (listp eww-data) (plist-get eww-data :url))))
    (and (stringp url) (not (string-empty-p url)) (concat "eww:" url))))

(defvar dired-directory)

(defun superchat-source-key-dired ()
  "The key of a dired buffer: `dir:DIRECTORY', its full directory name."
  (let* ((dir (bound-and-true-p dired-directory))
         (dir (if (consp dir) (car dir) dir)))
    (and (stringp dir) (not (string-empty-p dir))
         (concat "dir:" (file-name-as-directory (expand-file-name dir))))))

(defvar Info-current-file)
(defvar Info-current-node)

(defun superchat-source-key-info ()
  "The key of an Info buffer: `info:MANUAL:NODE'.
MANUAL is the manual file's name without its directory, which changes when
Emacs is upgraded."
  (let ((file (bound-and-true-p Info-current-file))
        (node (bound-and-true-p Info-current-node)))
    (when (and file (stringp node))
      (superchat-source-key--join
       "info"
       (if (stringp file) (file-name-nondirectory file) file)
       node))))

(defvar gnus-newsgroup-name)

(defun superchat-source-key-gnus-summary ()
  "The key of a Gnus summary buffer: `gnus:group:GROUP'."
  (let ((group (bound-and-true-p gnus-newsgroup-name)))
    (and (stringp group) (not (string-empty-p group))
         (concat "gnus:group:" group))))

(defvar notmuch-search-query-string)

(defun superchat-source-key-notmuch-search ()
  "The key of a notmuch search buffer: `notmuch:query:QUERY'."
  (let ((query (bound-and-true-p notmuch-search-query-string)))
    (and (stringp query) (not (string-empty-p query))
         (concat "notmuch:query:" query))))

(defvar notmuch-show-thread-id)

(defun superchat-source-key-notmuch-show ()
  "The key of a notmuch thread buffer: `notmuch:THREAD-ID'.
The thread id already reads `thread:...'."
  (let ((thread (bound-and-true-p notmuch-show-thread-id)))
    (and (stringp thread) (not (string-empty-p thread))
         (concat "notmuch:" thread))))

(defvar rcirc-server)
(defvar rcirc-target)

(defun superchat-source-key-rcirc ()
  "The key of an rcirc buffer: `rcirc:SERVER:TARGET', or `rcirc:SERVER'."
  (let ((server (bound-and-true-p rcirc-server))
        (target (bound-and-true-p rcirc-target)))
    (when (and (stringp server) (not (string-empty-p server)))
      (if (and (stringp target) (not (string-empty-p target)))
          (superchat-source-key--join "rcirc" server target)
        (concat "rcirc:" server)))))

(defun superchat-source-key (&optional buffer)
  "Return the key of the source BUFFER stands for, computed now.
In order (D13): the first function of `superchat-source-key-functions'
whose mode BUFFER's major mode is or derives from and that returns a
non-empty string; else, for a buffer visiting a file (an indirect buffer
by its base's), `file:' and that file's full name; else `buffer:', the
major mode, `:' and the buffer's name as it is now -- a name Emacs made
unique with a suffix such as `<2>' is used as it is, though which buffer
gets the suffix depends on the order buffers were made in.  Nil only for
a dead BUFFER.  BUFFER defaults to the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (or (cl-some (lambda (pair)
                       (and (symbolp (car pair))
                            (derived-mode-p (car pair))
                            (functionp (cdr pair))
                            (let ((key (condition-case nil (funcall (cdr pair))
                                         (error nil))))
                              (and (stringp key) (not (string-empty-p key)) key))))
                     superchat-source-key-functions)
            (when-let* ((file (or buffer-file-name
                                  (and (buffer-base-buffer)
                                       (buffer-file-name (buffer-base-buffer))))))
              (concat "file:" (expand-file-name file)))
            (format "buffer:%s:%s" major-mode (buffer-name)))))))

(defun superchat-workspace-of-source (&optional buffer)
  "Return the Workspace a request made from BUFFER belongs to.
In order: the Workspace of `superchat-buffer-workspace-directory' when
the buffer sets it; for an interface of the Personal Workspace
\(`superchat-workspace-personal-buffer-p'), the Personal Workspace when one
is designated -- never the project its `default-directory' happens to lie
in; otherwise the project Workspace of the buffer's directory; and when
none of these applies, Home (`superchat-workspace-home').  BUFFER defaults
to the current buffer; a dead buffer, or an unset data directory, gives
nil."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (let ((own (buffer-local-value 'superchat-buffer-workspace-directory
                                     buffer)))
        (cond
         ((and (stringp own) (file-directory-p own))
          (superchat-workspace-at-directory own))
         ((superchat-workspace-personal-buffer-p buffer)
          (or (superchat-workspace-personal)
              (superchat-workspace-home)))
         (t (or (superchat-workspace-of-buffer buffer)
                (superchat-workspace-home))))))))

;;;###autoload
(defun superchat-set-personal-workspace (dir)
  "Make DIR the Personal Workspace, and return that Workspace.
DIR is declared a Workspace (`superchat-workspace-declare') and saved as
`superchat-personal-workspace-directory' for future sessions.  Signals a
`user-error' unless DIR is an existing directory."
  (interactive (list (read-directory-name "Personal workspace directory: "
                                          nil nil t)))
  (let ((workspace (superchat-workspace-declare dir)))
    (customize-save-variable 'superchat-personal-workspace-directory
                             (plist-get workspace :root))
    (when (called-interactively-p 'interactive)
      (message "Personal workspace: %s" (plist-get workspace :root)))
    workspace))

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
  "This is a temporary conversation: it belongs to no workspace, and nothing said here is carried into later conversations.  If the user asks you to remember something for later, say that it will not be kept here, and that `/workspace DIRECTORY' in this window makes a directory a workspace whose rules are kept in its AGENTS.md.  A timer or a rule set with timer_create is kept, and other conversations see it."
  "What a request says about remembering in a conversation bound to no Workspace
\(contract W5, W10).  Since Home (2026-11-10) only a conversation the user
unbound with `/workspace clear' or `/reset', or one from before, is such a
conversation.")

(defconst superchat-workspace-rules-max-chars 6000
  "Most characters of a rules file that go into one request.")

(defconst superchat-workspace-rules-file-names '("AGENTS.md" "CLAUDE.md")
  "File names looked for under a Workspace root, first one found wins.")

(defun superchat-workspace--rules-file-names (root)
  "The rules file names looked for under ROOT.
Home's store has one rules file, `AGENTS.md'; a project root may also use
`CLAUDE.md'."
  (if (superchat-workspace--home-store-p root)
      '("AGENTS.md")
    superchat-workspace-rules-file-names))

(defun superchat-workspace-rules-file (root)
  "Return the rules file of the Workspace rooted at ROOT, or nil.
That is `AGENTS.md' under ROOT, else `CLAUDE.md', when it is a readable
regular file; in Home's store (`superchat-workspace-rules-root') only
`AGENTS.md'.  Nil when ROOT is not an existing directory."
  (when (and (stringp root) (file-directory-p root))
    (cl-some (lambda (name)
               (let ((file (expand-file-name name root)))
                 (and (file-regular-p file) (file-readable-p file) file)))
             (superchat-workspace--rules-file-names root))))

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
              (concat (if (superchat-workspace--home-store-p root)
                          "Home rules, from "
                        "Workspace rules, from ")
                      file
                      (if (superchat-workspace--home-store-p root)
                          ".  The user keeps these for all work outside a project (Home); follow them.\n\n"
                        ".  The user keeps these for all work in this workspace; follow them.\n\n")
                      (if cut
                          (substring body 0 superchat-workspace-rules-max-chars)
                        body)
                      (if cut
                          (format "\n\n[Cut at %d characters; read %s for the rest.]"
                                  superchat-workspace-rules-max-chars file)
                        ""))))))
    (error nil)))

(defun superchat-workspace-rules-problem (root)
  "Return why the Workspace at ROOT has a rules file that cannot be used, or nil.
That is a file named as rules which exists and is not readable, or whose
read signals, while no other rules file is readable.  The string names the
file.  A Workspace with no rules file, or with a readable one, gives nil."
  (when (and (stringp root) (file-directory-p root))
    (let ((file (superchat-workspace-rules-file root)))
      (if file
          (condition-case err
              (with-temp-buffer (insert-file-contents file) nil)
            (error (format "%s could not be read (%s)"
                           file (error-message-string err))))
        (when-let* ((present
                     (cl-some (lambda (name)
                                (let ((f (expand-file-name name root)))
                                  (and (file-regular-p f) f)))
                              (superchat-workspace--rules-file-names root))))
          (format "%s is not readable" present))))))

(defconst superchat-workspace-rules-problem-note
  "This workspace has a rules file that Superchat could not read: %s.  The user's rules are therefore NOT in this request.  Tell the user this in your answer before relying on any convention of this workspace."
  "What a chat request says when the Workspace's rules file cannot be read.")

(defconst superchat-workspace-home-note
  "This conversation is in Home, the workspace of everything outside a project.  Home's rules and work records are shared by the Home conversations and never reach a project; each Home conversation belongs to the buffer it was called from, so what was said in another one is not here."
  "What a request of a Home conversation says before its remembering note.")

(defun superchat-workspace-remember-note (root)
  "Return what a request says about remembering, for the Workspace at ROOT.
With a rules file, the model is told to edit that file's buffer; with a
root but no rules file, to start `AGENTS.md' there; with a nil ROOT (a
temporary conversation), that nothing is kept.  ROOT may be Home's store
\(`superchat-workspace-rules-root'), which need not exist yet: the note
then says so first, and names `AGENTS.md' there.  The model writes only on
the user's explicit word, and the user's save is what makes it a rule
\(contract R4)."
  (cond
   ((superchat-workspace--home-store-p root)
    (let ((file (superchat-workspace-rules-file root)))
      (concat superchat-workspace-home-note "  "
              (if file
                  (format superchat-workspace-remember-note-with-file file)
                (format superchat-workspace-remember-note-without-file
                        (expand-file-name "AGENTS.md" root))))))
   ((and (stringp root) (file-directory-p root))
    (let ((file (superchat-workspace-rules-file root)))
      (if file
          (format superchat-workspace-remember-note-with-file file)
        (format superchat-workspace-remember-note-without-file
                (expand-file-name "AGENTS.md" root)))))
   (t superchat-workspace-temporary-note)))

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
