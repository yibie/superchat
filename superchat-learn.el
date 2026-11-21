;;; superchat-learn.el --- A skill drafted from code that ran -*- lexical-binding: t; -*-

;;; Commentary:

;; `superchat-learn' drafts skills/PREFIX/SKILL.md from the `recipes' view:
;; `eval_elisp' code that ran without error and that an answer cited.  A run
;; with no tools titles each form and writes the description; code keeps
;; only the forms that ran, byte for byte, and adds the package's version.
;; The draft waits in a buffer: C-c C-c saves it, C-c C-k drops it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lisp-mnt)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-agent)

(defvar-local superchat-learn--prefix nil "The package prefix of this draft.")

(defun superchat-learn--version (prefix)
  "The version in the header of the library PREFIX, Emacs's for one of its\nown, or nil."
  (when-let* ((file (locate-library (concat (superchat-learn--name prefix) ".el"))))
    (or (with-temp-buffer
          (insert-file-contents file nil 0 4000)
          (lm-version))
        ;; A library that comes with Emacs has the Emacs's version.
        (and (file-in-directory-p file lisp-directory) (concat "Emacs " emacs-version)))))

(defun superchat-learn--prompt (prefix recipes)
  "The request that turns RECIPES of package PREFIX into a SKILL.md."
  (concat
   (format "Write a SKILL.md for the Emacs package %s from the forms below.  Each ran in\
 this Emacs without error, and an answer rested on its result.  You have no tools\
 and need none: all you need is here.  Reply with the file and nothing else.\n" prefix)
   "Start with a frontmatter between --- lines holding only `name: "
   (superchat-learn--name prefix)
   "` and `description:` one line on when to use this skill.  Then, for each form\
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
      (while (re-search-forward "^```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t]*$" nil t)
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

(defun superchat-learn-save ()
  "Save this draft as the skill of its package."
  (interactive)
  (let ((file (superchat-learn--file superchat-learn--prefix)))
    (when (or (not (file-exists-p file))
              (y-or-n-p (format "%s exists; replace it? " file)))
      (make-directory (file-name-directory file) t)
      (write-region (point-min) (point-max) file nil 'silent)
      (message "Saved %s" file)
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

(defun superchat-learn--show (prefix text left)
  "Show TEXT as the draft skill of PREFIX; LEFT forms were left out."
  (with-current-buffer (get-buffer-create (format "*superchat: learn %s*" prefix))
    (let ((inhibit-read-only t)) (erase-buffer) (insert text))
    (superchat-learn-mode)
    (setq superchat-learn--prefix prefix
          header-line-format
          (format " C-c C-c save to %s   C-c C-k drop%s"
                  (abbreviate-file-name (superchat-learn--file prefix))
                  (if (> left 0) (format "   %d form(s) left out: not code that ran" left) "")))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun superchat-learn (prefix)
  "Draft the skill of package PREFIX from cited code that ran."
  (interactive "sPackage prefix: ")
  (let ((recipes (superchat-view 'recipes :name prefix)))
    (unless recipes
      (user-error "No answer has cited code that names %s and ran" prefix))
    (message "Drafting the skill of %s from %d form(s)..." prefix (length recipes))
    (superchat-run (superchat-tape-new-name) (superchat-learn--prompt prefix recipes)
                   :ephemeral t :tools 'none
                   :on-done
                   (lambda (run)
                     (cond
                      ((not (eq (plist-get run :status) 'done))
                       (message "The draft of %s failed: %s" prefix (plist-get run :error)))
                      ((not (string-match-p "\\`[ \t\n]*---\n" (or (plist-get run :text) "")))
                       (message "The model wrote no draft of %s; try /learn again" prefix))
                      (t
                       (let ((checked (superchat-learn--check
                                       (plist-get run :text)
                                       (mapcar (lambda (e) (plist-get (plist-get e :data) :code))
                                               recipes)
                                       (superchat-learn--version prefix))))
                         (superchat-learn--show prefix (car checked) (cdr checked)))))))))

(provide 'superchat-learn)
;;; superchat-learn.el ends here
