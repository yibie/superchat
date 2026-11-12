;;; superchat-fix.el --- A copy of a project, and building in it in a sandbox -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; docs/decisions/accepted/2026-10-31-a-fix-tried-in-a-copy.md.
;;
;; What a rule that may fix (`superchat-watch.el') stands on: a copy of a project
;; that holds no history and nothing named like a secret, a build in it that the
;; operating system confines, the difference between what was changed and what
;; was there, and that difference applied to what the user has now.  It knows
;; nothing of models or of rules.
;;
;; A model that can edit files and run the build can run anything: it changes the
;; build script and runs the build.  So the confinement is the operating system's,
;; and where there is none the rule cannot be set.  The policy is given on the
;; command line, since a file could be changed by the build it is meant to hold.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ansi-color)
(require 'superchat-sandbox)

(declare-function superchat-watch--unreadable-p "superchat-watch" (name))

(defcustom superchat-fix-copy-megabytes 10240
  "How large a project is copied to try a fix in, at most.
On a file system that clones (APFS) a copy costs next to nothing; on another
it is a real copy.  A larger project is only looked into."
  :type 'natnum
  :group 'superchat-timer)

(defcustom superchat-fix-copy-excludes '(".git" ".jj" ".hg" ".svn")
  "Names that are not copied, at any depth.
History stays out: the model's attempts do not reach the user's repository, and
nothing in the copy can push.  Add what is large and no build needs."
  :type '(repeat string)
  :group 'superchat-timer)

(defcustom superchat-fix-sandbox-read nil
  "More directories a build in a copy may read.
By default it reads the copy and the system's and the usual toolchains' places,
and nothing under the home directory.  A toolchain or a library there needs
naming here, or the failure does not come back in the copy and it is only looked
into."
  :type '(repeat directory)
  :group 'superchat-timer)

(defcustom superchat-fix-builds 3
  "How many times a model may run the build in one try."
  :type 'natnum
  :group 'superchat-timer)

(defcustom superchat-fix-build-seconds 120
  "How many seconds one build in a copy may take."
  :type 'number
  :group 'superchat-timer)

(defcustom superchat-fix-seconds 600
  "How many seconds a model may take to try one fix."
  :type 'number
  :group 'superchat-timer)

(defcustom superchat-fix-per-day 5
  "How many fixes one rule tries in 24 hours, at most; after that it only looks."
  :type 'natnum
  :group 'superchat-timer)

(defvar superchat-fix-edits 20
  "How many times a model may edit in one try.")

(defvar superchat-fix-diff-bytes 65536
  "How large a fix may be; a larger one is no simple fix, and is not offered.")

(defconst superchat-fix--sandbox-exec "/usr/bin/sandbox-exec"
  "The confinement, by its absolute path: it is not to be found through PATH.")

(defconst superchat-fix--read-paths
  '("/usr" "/bin" "/sbin" "/System" "/Library/Apple" "/Library/Preferences/Logging"
    "/private/etc" "/private/var/db/timezone" "/private/var/db/dyld"
    "/private/var/select" "/private/var/db/xcode_select_link" "/dev"
    "/opt/homebrew" "/opt/local" "/Library/Developer"
    "/Applications/Xcode.app/Contents")
  "What a build may read besides its copy: the system and the usual toolchains.")

(defconst superchat-fix--read-files
  '("/Library/Preferences/com.apple.dt.Xcode.plist")
  "Single files a build may read: `make' asks this one about Xcode's licence.")

(defconst superchat-fix--noise
  "^[0-9-]+ [0-9:.]+ xcodebuild\\[[0-9]+:[0-9]+\\] .*\n"
  "What Xcode prints by itself when a tool is looked for inside the sandbox.
`make', `cc' and `git' in /usr/bin ask `xcrun', which asks `xcodebuild' the
first time in each temporary directory, and that cannot reach two services from
here.  Measured 2026-11-02: it made every C project differ from the failure it
copied, and none was ever tried.")

(defconst superchat-fix--output-bytes 200000
  "How much of what a build prints is kept, from its end.")

(defvar superchat-fix--live nil "The copies that exist now.")
(defvar superchat-fix--made nil "Every copy this Emacs made.")

(defun superchat-fix-available-p ()
  "Non-nil when a build can be confined here: macOS, for now."
  (and (eq system-type 'darwin)
       (file-executable-p superchat-fix--sandbox-exec)))

;;;; The copy -------------------------------------------------------------------------

(defun superchat-fix--tmp-of (dir)
  "The directory a build in the copy DIR has for itself."
  (concat (directory-file-name dir) ".tmp"))

(defun superchat-fix-delete (dir)
  "Delete the copy DIR, without waiting for it.
It runs rm -rf, so it goes only for what this Emacs made.  A build can leave a
directory read-only, and that stops rm: chmod -R goes first, and it does not
follow links."
  (cond
   ((member dir superchat-fix--live)
    (setq superchat-fix--live (delete dir superchat-fix--live))
    (let ((default-directory "/"))
      (call-process "/bin/sh" nil 0 nil "-c"
                    "chmod -R u+rwx \"$1\" \"$2\" 2>/dev/null; rm -rf \"$1\" \"$2\""
                    "delete" (directory-file-name dir) (superchat-fix--tmp-of dir))))
   ((member dir superchat-fix--made) nil)
   (t (error "%s is no copy made here" dir))))

(defun superchat-fix--delete-all ()
  "Delete the copies that are left: Emacs is exiting."
  (mapc #'superchat-fix-delete (copy-sequence superchat-fix--live)))

(defun superchat-fix--copy-script ()
  "The script copying $1 to $2 unless it is larger than $3 kilobytes.
It prints what is in the copy, separated by NUL."
  (let ((names (mapcar #'shell-quote-argument superchat-fix-copy-excludes)))
    (concat
     "set -e\n"
     "size=$(du -sk \"$1\" | cut -f1)\n"
     "if [ \"$size\" -gt \"$3\" ]; then echo \"$size\"; exit 64; fi\n"
     "cd \"$1\"\n"
     "find . -mindepth 1 -maxdepth 1 "
     (mapconcat (lambda (name) (concat "! -name " name)) names " ")
     " -exec cp -cRPp {} \"$2\"/ \\;\n"
     (when names
       (concat "find \"$2\" -mindepth 1 \\( "
               (mapconcat (lambda (name) (concat "-name " name)) names " -o ")
               " \\) -prune -exec rm -rf {} +\n"))
     "find \"$2\" -mindepth 1 -print0\n")))

(defun superchat-fix--leave-out-secrets (dir listing)
  "Delete from the copy DIR what is named like a secret.  LISTING is its contents.
It goes through every name of what can be a large project, while Emacs waits,
so the name is judged first: that asks the disk nothing.  Measured 2026-11-02 on
20000 files: 0.28 seconds asking the disk first, 0.02 judging the name first."
  (let ((prefix (length dir)))
    (dolist (file (split-string listing "\0" t))
      (when (superchat-watch--unreadable-p (substring file prefix))
        (if (and (file-directory-p file) (not (file-symlink-p file)))
            (delete-directory file t)
          (delete-file file))))))

(defun superchat-fix-copy (root done)
  "Copy the project ROOT, without waiting; return a function that stops it.
DONE is called once with (:dir DIR), DIR being the copy, or (:why WORDS).
Stopped, it is not called and nothing is left."
  (add-hook 'kill-emacs-hook #'superchat-fix--delete-all)
  (let* ((dir (file-name-as-directory
               (file-truename (make-temp-file "superchat-copy-" t))))
         (tmp (superchat-fix--tmp-of dir))
         (output (generate-new-buffer " *superchat-copy*" t))
         (stopped nil)
         (process
          (let ((coding-system-for-read 'utf-8-unix)
                (default-directory temporary-file-directory))
            (make-directory tmp)
            (push dir superchat-fix--live)
            (push dir superchat-fix--made)
            (make-process
             :name "superchat-copy" :buffer output :noquery t :connection-type 'pipe
             :command (list "/bin/sh" "-c" (superchat-fix--copy-script) "copy"
                            (directory-file-name (expand-file-name root))
                            (directory-file-name dir)
                            (number-to-string (* 1024 superchat-fix-copy-megabytes)))
             :sentinel
             (lambda (process _event)
               (when (and (memq (process-status process) '(exit signal)) (not stopped))
                 (let ((code (process-exit-status process))
                       (text (with-current-buffer output (buffer-string))))
                   (kill-buffer output)
                   (cond
                    ((eql code 0)
                     (if-let* ((why (condition-case err
                                        (progn (superchat-fix--leave-out-secrets dir text) nil)
                                      (file-error (error-message-string err)))))
                         (progn
                           (superchat-fix-delete dir)
                           (funcall done (list :why (format "copying it failed: %s" why))))
                       (funcall done (list :dir dir))))
                    ((eql code 64)
                     (superchat-fix-delete dir)
                     (funcall done
                              (list :why
                                    (format "the project is %d MB, more than superchat-fix-copy-megabytes (%d)"
                                            (/ (string-to-number text) 1024)
                                            superchat-fix-copy-megabytes))))
                    (t
                     (superchat-fix-delete dir)
                     (funcall done (list :why (format "copying it failed: %s"
                                                      (string-trim text)))))))))))))
    (lambda ()
      (setq stopped t)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p output)
        (kill-buffer output))
      (superchat-fix-delete dir))))

;;;; The sandbox ----------------------------------------------------------------------

(defun superchat-fix--app-of (directory)
  "The application bundle DIRECTORY lies in, or DIRECTORY."
  (if (string-match "\\`.*?\\.app/" directory)
      (match-string 0 directory)
    directory))

(defun superchat-fix--policy (dir)
  "What a build in the copy DIR may do: nothing but what is listed.
So no network, not even this machine, and neither reading nor writing elsewhere."
  (let ((tmp (superchat-fix--tmp-of dir))
        (subpaths (lambda (paths)
                    (mapconcat (lambda (path)
                                 (format "(subpath %S)"
                                         (directory-file-name (file-truename path))))
                               paths " "))))
    (concat
     "(version 1)\n(deny default)\n"
     "(allow process-fork)\n(allow process-exec)\n"
     "(allow signal (target same-sandbox))\n"
     "(allow sysctl-read)\n(allow pseudo-tty)\n"
     ;; Stat and resolving a path anywhere; content only below.
     "(allow file-read-metadata)\n"
     "(allow file-read* (literal \"/\"))\n"
     "(allow file-read* "
     (funcall subpaths
              (append (list dir tmp)
                      superchat-fix--read-paths
                      superchat-fix-sandbox-read
                      (list (superchat-fix--app-of (file-truename invocation-directory)))))
     " "
     (mapconcat (lambda (file) (format "(literal %S)" file)) superchat-fix--read-files " ")
     ")\n"
     "(allow file-write* " (funcall subpaths (list dir tmp))
     " (literal \"/dev/null\") (literal \"/dev/tty\") (literal \"/dev/ptmx\")"
     " (regex #\"^/dev/ttys[0-9]+$\"))\n"
     "(allow file-ioctl (literal \"/dev/tty\") (literal \"/dev/ptmx\")"
     " (regex #\"^/dev/ttys[0-9]+$\"))\n")))

(defun superchat-fix--command (dir command)
  "What runs COMMAND confined to the copy DIR, as a list."
  (list superchat-fix--sandbox-exec "-p" (superchat-fix--policy dir)
        (or shell-file-name "/bin/sh") "-c" command))

(defun superchat-fix--environment (dir)
  "All a build in the copy DIR gets of the environment: no key of the user's."
  (let ((tmp (superchat-fix--tmp-of dir)))
    (append
     (list (concat "PATH=" (or (getenv "PATH") "/usr/bin:/bin"))
           (concat "HOME=" tmp)
           (concat "TMPDIR=" tmp "/")
           "TERM=dumb"
           (format "INSIDE_EMACS=%s,compile" emacs-version))
     (when-let* ((lang (getenv "LANG"))) (list (concat "LANG=" lang)))
     (when-let* ((all (getenv "LC_ALL"))) (list (concat "LC_ALL=" all))))))

(defun superchat-fix--as-project (dir root text)
  "TEXT, speaking of the project ROOT where it speaks of its copy DIR."
  (let ((project (directory-file-name (file-truename root)))
        (case-fold-search nil))
    (replace-regexp-in-string
     (regexp-quote (directory-file-name dir)) project
     (replace-regexp-in-string
      (regexp-quote (superchat-fix--tmp-of dir)) "/tmp/build" text t t)
     t t)))

(defun superchat-fix-build (dir root directory command done &optional window)
  "Run COMMAND in the copy DIR of the project ROOT, confined, without waiting.
It runs in the place of the copy that DIRECTORY is in the project.  WINDOW is
\(LINES . COLUMNS) of the terminal it sees: a compiler breaks its lines, and
cuts what it quotes, at the width of the window it runs in.  Return a
function that stops it.  DONE is called once with (:exit N :text TEXT), TEXT
being what it printed, speaking of the project; with :timed-out t when it took
too long.  Stopped, it is not called.  One that cannot start says so that way: a
build had its hands on the copy, and may have removed where the next one runs.
DIRECTORY outside ROOT has no place in the copy, and that goes the same way."
  (let* ((output (generate-new-buffer " *superchat-build*" t))
         (here (expand-file-name
                (file-relative-name (file-truename directory) (file-truename root))
                dir))
         (stopped nil)
         (timed-out nil)
         (timer nil)
         (refused nil)
         (kill-group
          (lambda (process)
            ;; What it left in the background goes with it: its group outlives it.
            (when-let* ((pid (process-id process)))
              (ignore-errors (signal-process (- pid) 'SIGKILL)))))
         (process
          (condition-case err
              (let ((process-environment (superchat-fix--environment dir))
                    (coding-system-for-read 'utf-8-unix)
                    (coding-system-for-write 'utf-8-unix)
                    (default-directory (file-name-as-directory here)))
                (unless (superchat-sandbox--under-p here (directory-file-name dir))
                  (signal 'file-error (list "It started outside the project" directory)))
                (make-process
                 :name "superchat-build" :buffer output :noquery t :connection-type 'pty
                 :command (superchat-fix--command dir command)
                 :filter
                 (lambda (_process text)
                   (when (buffer-live-p output)
                     (with-current-buffer output
                       (goto-char (point-max))
                       (insert text)
                       (when (> (buffer-size) (* 2 superchat-fix--output-bytes))
                         (delete-region (point-min) (- (point-max) superchat-fix--output-bytes))))))
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (funcall kill-group process)
                     (when (timerp timer) (cancel-timer timer))
                     (unless stopped
                       (let ((code (process-exit-status process))
                             (text (with-current-buffer output
                                     (ansi-color-filter-apply (buffer-string)))))
                         (kill-buffer output)
                         (funcall done
                                  (append
                                   (list :exit code
                                         :text (superchat-fix--as-project
                                                dir root
                                                (replace-regexp-in-string
                                                 superchat-fix--noise ""
                                                 (replace-regexp-in-string "\r$" "" text t t)
                                                 t t)))
                                   (when timed-out (list :timed-out t))))))))))
            (file-error (setq refused (error-message-string err)) nil))))
    (if refused
        (progn
          (kill-buffer output)
          (let ((late (run-at-time 0 nil done (list :exit 127 :text refused))))
            (lambda () (cancel-timer late))))
      (when window
        (set-process-window-size process (car window) (cdr window)))
      (process-send-eof process)
      (setq timer (run-at-time superchat-fix-build-seconds nil
                               (lambda ()
                                 (when (process-live-p process)
                                   (setq timed-out t)
                                   (funcall kill-group process)))))
      (lambda ()
        (setq stopped t)
        ;; This kills its group too, and runs the sentinel, which cancels the timer.
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p output)
          (kill-buffer output))))))

;;;; Editing in the copy ---------------------------------------------------------------

(defun superchat-fix--bytes (file)
  "What FILE holds, as bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun superchat-fix--write-bytes (file bytes)
  "Make BYTES what FILE holds."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun superchat-fix-edit (file old new)
  "In FILE of a copy, put NEW for OLD, which must be there exactly once.
With OLD empty it makes FILE, which must not exist.  Signal when it cannot be
done."
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        (case-fold-search nil))
    (if (string-empty-p old)
        (progn
          (when (file-exists-p file)
            (error "%s exists: give the text to replace" (file-name-nondirectory file)))
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (insert new)
            (write-region (point-min) (point-max) file nil 'silent)))
      (unless (file-regular-p file)
        (error "%s does not exist: give no old text to make it" (file-name-nondirectory file)))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((count (how-many (regexp-quote old) (point-min) (point-max))))
          (cond
           ((zerop count)
            (error "The old text was not found in %s: nothing was changed"
                   (file-name-nondirectory file)))
           ((> count 1)
            (error "The old text is there %d times in %s: give more of it"
                   count (file-name-nondirectory file)))))
        (goto-char (point-min))
        (search-forward old)
        (replace-match new t t)
        (write-region (point-min) (point-max) file nil 'silent)))))

;;;; The difference --------------------------------------------------------------------

(defun superchat-fix-name-forges-p (name)
  "Non-nil when the file NAME has a control character in it.
A name goes in the header of a difference as it is, so one with a newline could
write lines there that `patch' takes for a difference of another file."
  (string-match-p "[[:cntrl:]]" name))

(defun superchat-fix--diff-of (name base file)
  "The unified difference for NAME between the bytes BASE and FILE, or nil.
BASE nil means it did not exist."
  (let* ((old (make-temp-file "superchat-diff-"))
         (out (generate-new-buffer " *superchat-diff*" t)))
    (unwind-protect
        (progn
          (when base (superchat-fix--write-bytes old base))
          (let* ((coding-system-for-read 'utf-8-unix)
                 (default-directory temporary-file-directory)
                 (code (call-process
                        "diff" nil out nil "-u"
                        "--label" (if base (concat "a/" name) "/dev/null")
                        "--label" (concat "b/" name)
                        (if base old "/dev/null") file)))
            (pcase code
              (0 nil)
              (1 (with-current-buffer out (buffer-string)))
              (_ (error "Comparing %s failed" name)))))
      (delete-file old)
      (kill-buffer out))))

(defun superchat-fix-diff (dir edited)
  "What was changed in the copy DIR.
EDITED is an alist of name and what was there.  Return (:patch TEXT :paths
NAMES), nil when nothing is different, or (:why WORDS) when it is not to be
offered.  What is read here is read outside the sandbox, and a build had its
hands on it: a name that became a link goes no further.  A fix is kept on the
record as text, so a difference that is not text does not go either."
  (condition-case err
      (catch 'refused
        (let (patches paths)
          (dolist (entry (reverse edited))
            (let* ((name (car entry))
                   (file (expand-file-name name dir)))
              (unless (and (file-regular-p file)
                           (not (file-symlink-p file))
                           (superchat-sandbox--under-p
                            (file-truename file) (directory-file-name dir)))
                (throw 'refused (list :why (format "%s is no file of the copy any more" name))))
              (when-let* ((patch (superchat-fix--diff-of name (cdr entry) file)))
                (push patch patches)
                (push name paths))))
          (when patches
            (let ((patch (mapconcat #'identity (reverse patches) "")))
              (cond
               ((> (string-bytes patch) superchat-fix-diff-bytes)
                (list :why (format "it is too large: %d bytes" (string-bytes patch))))
               ((string-match-p "[\x3fff80-\x3fffff]" patch)
                (list :why "it touches lines that are not UTF-8"))
               (t (list :patch patch :paths (reverse paths))))))))
    (error (list :why (error-message-string err)))))

;;;; Applying it -----------------------------------------------------------------------

(defun superchat-fix--now-holds (file)
  "What FILE holds for the user now, as bytes, unsaved changes included; or nil."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (encode-coding-string (save-restriction (widen) (buffer-string))
                              (or buffer-file-coding-system 'utf-8-unix)))
    (and (file-regular-p file) (superchat-fix--bytes file))))

(defun superchat-fix-apply-texts (root paths patch)
  "What the files PATHS of the project ROOT hold with PATCH applied, as bytes.
Return an alist of name and bytes.  This changes nothing.  Signal `user-error'
when a path leads out of the project, is named like a secret or has a control
character in it, or when what is there now no longer matches: nothing may be
applied by a guess, and none of it when one does not."
  (let* ((root (file-name-as-directory (file-truename root)))
         (place (make-temp-file "superchat-apply-" t))
         (patch-file (expand-file-name "the.patch" place))
         (tree (expand-file-name "tree" place))
         (out (generate-new-buffer " *superchat-patch*" t)))
    (unwind-protect
        (progn
          (make-directory tree)
          (dolist (name paths)
            (let ((file (expand-file-name name root)))
              (unless (and (not (file-name-absolute-p name))
                           (not (superchat-fix-name-forges-p name))
                           (superchat-sandbox--under-p
                            (superchat-sandbox-canonical-path file)
                            (directory-file-name root))
                           (not (superchat-watch--unreadable-p name)))
                (user-error "%s is not a place a fix may go" name))
              (when-let* ((holds (superchat-fix--now-holds file)))
                (let ((copy (expand-file-name name tree)))
                  (make-directory (file-name-directory copy) t)
                  (superchat-fix--write-bytes copy holds)))))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file patch-file (insert patch)))
          (let ((code (let ((default-directory tree))
                        (call-process "patch" nil out nil
                                      "-f" "-s" "-F0" "-p1" "-d" tree "-i" patch-file))))
            (unless (eql code 0)
              (user-error "It no longer matches what is there (%s); nothing was changed"
                          (mapconcat #'identity paths ", "))))
          (mapcar (lambda (name)
                    (cons name (superchat-fix--bytes (expand-file-name name tree))))
                  paths))
      (kill-buffer out)
      (delete-directory place t))))

(provide 'superchat-fix)

;;; superchat-fix.el ends here
