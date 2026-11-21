;;; superchat-learn.el --- A skill drafted from code that ran -*- lexical-binding: t; -*-

;;; Commentary:

;; `superchat-learn' drafts skills/PREFIX/SKILL.md from the `recipes' view:
;; `eval_elisp' code that ran without error and that an answer cited.  A run
;; with no tools titles each form and writes the description; code keeps
;; only the forms that ran, byte for byte, and adds the package's version.
;; A skill that exists grows: only forms not in it, and not dropped from an
;; earlier draft, are drafted, and C-c C-c adds them at its end.  The draft
;; waits in a buffer: C-c C-c saves it, C-c C-k drops it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lisp-mnt)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-agent)

(defvar-local superchat-learn--prefix nil "The package prefix of this draft.")
(defvar-local superchat-learn--adding nil "Non-nil when this draft adds to a skill.")
(defvar-local superchat-learn--offered nil "The code of the blocks this draft showed.")
(defvar-local superchat-learn--new-version nil "The package's version for this draft.")

(defconst superchat-learn--block "^```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t]*$"
  "An elisp block of a skill; group 1 is its code.")

(defun superchat-learn--blocks (text)
  "The trimmed code of each elisp block of TEXT."
  (let (codes)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward superchat-learn--block nil t)
        (push (string-trim (match-string 1)) codes)))
    (nreverse codes)))

(defun superchat-learn--version (prefix)
  "The version in the header of the library PREFIX, Emacs's for one of its\nown, or nil."
  (when-let* ((file (locate-library (concat (superchat-learn--name prefix) ".el"))))
    (or (with-temp-buffer
          (insert-file-contents file nil 0 4000)
          (lm-version))
        ;; A library that comes with Emacs has the Emacs's version.
        (and (file-in-directory-p file lisp-directory) (concat "Emacs " emacs-version)))))

(defun superchat-learn--prompt (prefix recipes adding)
  "The request that turns RECIPES of package PREFIX into a SKILL.md, or,
when ADDING, into sections added to its SKILL.md."
  (concat
   (if adding
       (format "These forms add to the SKILL.md of the Emacs package %s.  Each ran in\
 this Emacs without error, and an answer rested on its result.  You have no tools\
 and need none.  Reply with the new sections and nothing else: no frontmatter.  " prefix)
     (concat
   (format "Write a SKILL.md for the Emacs package %s from the forms below.  Each ran in\
 this Emacs without error, and an answer rested on its result.  You have no tools\
 and need none: all you need is here.  Reply with the file and nothing else.\n" prefix)
   "Start with a frontmatter between --- lines holding only `name: "
   (superchat-learn--name prefix)
   "` and `description:` one line on when to use this skill.  Then, "))
   "For each form\
 worth keeping, a `## ` title, one line on what it does and what it returns, and\
 the form in an ```elisp block copied exactly.  Leave out forms that only probe.\
 Write nothing else.\n\n"
   (mapconcat (lambda (e)
                (format "Form:\n%s\nIt returned: %s\n"
                        (plist-get (plist-get e :data) :code)
                        (car (split-string (plist-get e :text) "\n"))))
              recipes "\n")))

(defun superchat-learn--check (draft codes version)
  "DRAFT with each elisp block not among CODES left out, and VERSION added.
Return (TEXT . LEFT-OUT)."
  (let ((left 0))
    (with-temp-buffer
      (insert draft)
      (goto-char (point-min))
      (while (re-search-forward superchat-learn--block nil t)
        (unless (member (string-trim (match-string 1)) (mapcar #'string-trim codes))
          (cl-incf left)
          (replace-match "(A form left out here: it is not code that ran.)" t t)))
      (goto-char (point-min))
      (when (and version (re-search-forward "^name:.*$" nil t))
        (insert "\nversion: " version))
      (cons (buffer-string) left))))

(defun superchat-learn--name (prefix)
  "The skill's name for package PREFIX: PREFIX without a trailing dash."
  (string-remove-suffix "-" prefix))

(defun superchat-learn--file (prefix)
  "Where the skill of PREFIX is saved."
  (expand-file-name (format "skills/%s/SKILL.md" (superchat-learn--name prefix))
                    superchat-directory))

(defun superchat-learn--rejected-file (prefix)
  "The list of forms dropped from drafts of PREFIX's skill."
  (expand-file-name "rejected.eld" (file-name-directory (superchat-learn--file prefix))))

(defun superchat-learn--rejected (prefix)
  "The code of the forms dropped from drafts of PREFIX's skill."
  (let ((file (superchat-learn--rejected-file prefix)))
    (when (file-exists-p file)
      (with-temp-buffer (insert-file-contents file) (read (current-buffer))))))

(defun superchat-learn--old-version (text)
  "The `version:' of the skill TEXT, or nil."
  (and (string-match "^version: \\(.*\\)$" text) (match-string 1 text)))

(defun superchat-learn-save ()
  "Save this draft as the skill of its package, or add it to that skill.
A form the draft showed and the person deleted is not drafted again."
  (interactive)
  (let* ((prefix superchat-learn--prefix)
         (file (superchat-learn--file prefix))
         (text (buffer-string))
         (version superchat-learn--new-version)
         (kept (superchat-learn--blocks text))
         (dropped (seq-remove (lambda (c) (member c kept)) superchat-learn--offered)))
    (when (or superchat-learn--adding (not (file-exists-p file))
              (y-or-n-p (format "%s exists; replace it? " file)))
      (make-directory (file-name-directory file) t)
      (if (not superchat-learn--adding)
          (write-region text nil file nil 'silent)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (cond ((not version))
                ((re-search-forward "^version: .*$" nil t) (replace-match (concat "version: " version) t t))
                ((re-search-forward "^name:.*$" nil t) (insert "\nversion: " version)))
          (goto-char (point-max))
          (insert (if (bolp) "\n" "\n\n") (string-trim text) "\n")
          (write-region nil nil file nil 'silent)))
      (when dropped
        (with-temp-file (superchat-learn--rejected-file prefix)
          (prin1 (append (superchat-learn--rejected prefix) dropped) (current-buffer))))
      (message "Saved %s%s" file
               (if dropped (format "; %d dropped form(s) will not be drafted again" (length dropped)) ""))
      (quit-window t))))

(defun superchat-learn-drop ()
  "Drop this draft."
  (interactive)
  (quit-window t))

(defvar-keymap superchat-learn-mode-map
  "C-c C-c" #'superchat-learn-save
  "C-c C-k" #'superchat-learn-drop)

(define-derived-mode superchat-learn-mode text-mode "Superchat-Learn"
  "A skill drafted from code that ran, waiting to be saved or dropped.")

(defun superchat-learn--show (prefix text left adding version old-version known)
  "Show TEXT as the draft skill of PREFIX; LEFT forms were left out.
ADDING: TEXT adds to the skill; KNOWN forms were skipped as in it or
dropped; it had
OLD-VERSION; VERSION is the package's."
  (with-current-buffer (get-buffer-create (format "*superchat: learn %s*" prefix))
    (let ((inhibit-read-only t)) (erase-buffer) (insert text))
    (superchat-learn-mode)
    (setq superchat-learn--prefix prefix
          superchat-learn--adding adding
          superchat-learn--new-version version
          superchat-learn--offered (superchat-learn--blocks text)
          header-line-format
          (concat (format " C-c C-c %s %s   C-c C-k drop"
                          (if adding "add to" "save to")
                          (abbreviate-file-name (superchat-learn--file prefix)))
                  (if adding (format "   %d already there or dropped" known) "")
                  (if (> left 0) (format "   %d form(s) left out: not code that ran" left) "")
                  (if (and adding version old-version (not (equal version old-version)))
                      (format "   version %s → %s: recheck older forms" old-version version)
                    "")))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun superchat-learn (prefix)
  "Draft the skill of package PREFIX from cited code that ran."
  (interactive "sPackage prefix: ")
  (let* ((all (or (superchat-view 'recipes :name prefix)
                  (user-error "No answer has cited code that names %s and ran" prefix)))
         (file (superchat-learn--file prefix))
         (old (and (file-exists-p file)
                   (with-temp-buffer (insert-file-contents file) (buffer-string))))
         (known (append (and old (superchat-learn--blocks old)) (superchat-learn--rejected prefix)))
         (code (lambda (e) (string-trim (plist-get (plist-get e :data) :code))))
         (recipes (seq-remove (lambda (e) (member (funcall code e) known)) all))
         (version (superchat-learn--version prefix)))
    (if (not recipes)
        (message "No new form for the skill of %s: all %d are in it or were dropped" prefix (length all))
    (message "Drafting the skill of %s from %d form(s)..." prefix (length recipes))
    (superchat-run (superchat-tape-new-name) (superchat-learn--prompt prefix recipes old)
                   :ephemeral t :tools 'none
                   :on-done
                   (lambda (run)
                     (cond
                      ((not (eq (plist-get run :status) 'done))
                       (message "The draft of %s failed: %s" prefix (plist-get run :error)))
                      ((not (string-match-p (if old "^## " "\\`[ \t\n]*---\n")
                                            (or (plist-get run :text) "")))
                       (message "The model wrote no draft of %s; try /learn again" prefix))
                      (t
                       (let ((checked (superchat-learn--check (plist-get run :text)
                                                              (mapcar code recipes)
                                                              (and (not old) version))))
                         (superchat-learn--show prefix (car checked) (cdr checked) (and old t)
                                                version (and old (superchat-learn--old-version old))
                                                (- (length all) (length recipes)))))))))))

(provide 'superchat-learn)
;;; superchat-learn.el ends here
