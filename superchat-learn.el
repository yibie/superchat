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
(defvar-local superchat-learn--offered nil "The templates this draft showed.")
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

(defun superchat-learn--producer (id)
  "The call whose result is the entry ID, as code, or nil."
  (let* ((tool (superchat-tape-get id)) (call (plist-get (plist-get tool :data) :call-id)))
    (cl-loop for a in (reverse (superchat-tape-entries :tape (plist-get tool :tape) :kinds '(assistant)
                                                       :before id :newest t :limit 5))
             thereis (cl-loop for c in (plist-get (plist-get a :data) :calls)
                              when (equal (plist-get c :id) call)
                              return (or (plist-get (plist-get c :args) :code)
                                         (format "%s %S" (plist-get c :name) (plist-get c :args)))))))

(defun superchat-learn--from (hole)
  "Where the value of HOLE came from, in words a later run can use."
  (pcase (plist-get hole :class)
    ('input "from the request")
    ('decided "a value given each time")
    ('suspected "in the request or in more than one result: check it")
    ('varies "differs between runs: check it")
    (`(copied . ,id) (format "read from the result of %s"
                             (truncate-string-to-width
                              (replace-regexp-in-string "[ \t\n]+" " " (or (superchat-learn--producer id) "an earlier call"))
                              70 nil nil "…")))))

(defun superchat-learn--prompt (prefix groups adding)
  "The request that turns the templates GROUPS of package PREFIX into a
SKILL.md, or, when ADDING, into sections added to its SKILL.md."
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
   "For each form worth keeping, a `## ` title, one line on what it does and what it\
 returns, and the form in an ```elisp block copied exactly, its UPPERCASE holes as\
 they are: a hole stands for a value given each time.  Leave out forms that only\
 probe.  Write nothing else.\n\n"
   (mapconcat (lambda (g)
                (concat (format "Form:\n%s\n" (plist-get g :text))
                        (if (plist-get g :holes)
                            (format "Holes: %s\n"
                                    (mapconcat (lambda (h) (format "%s, here %S, %s" (plist-get h :name)
                                                                   (plist-get h :value) (superchat-learn--from h)))
                                               (plist-get g :holes) "; "))
                          "")
                        (format "It returned: %s\n" (plist-get g :returned))))
              groups "\n")))

(defun superchat-learn--evidence (group)
  "The line under a recipe: where each of its holes came from, or nil."
  (and (plist-get group :holes)
       (mapconcat (lambda (h) (format "%s: %s." (plist-get h :name) (superchat-learn--from h)))
                  (plist-get group :holes) " ")))

(defun superchat-learn--check (draft groups version)
  "DRAFT with each elisp block that is not one of the templates GROUPS left
out, the evidence of each kept one under it, and VERSION added.
Return (TEXT LEFT-OUT SHOWN), SHOWN the groups whose blocks it keeps."
  (let ((left 0) shown)
    (with-temp-buffer
      (insert draft)
      (goto-char (point-min))
      (while (re-search-forward superchat-learn--block nil t)
        (let* ((beg (match-beginning 0)) (end (match-end 0))
               (key (superchat-learn--key (match-string 1)))
               (g (seq-find (lambda (g) (equal (plist-get g :key) key)) groups)))
          (if (not g)
              (progn (cl-incf left)
                     (delete-region beg end)
                     (goto-char beg)
                     (insert "(A form left out here: it is not code that ran.)"))
            (push g shown)
            (goto-char end)
            (when-let* ((line (superchat-learn--evidence g))) (insert "\n" line)))))
      (goto-char (point-min))
      (when (and version (re-search-forward "^name:.*$" nil t))
        (insert "\nversion: " version))
      (list (buffer-string) left (delete-dups (nreverse shown))))))

(defun superchat-learn--name (prefix)
  "The skill's name for package PREFIX: PREFIX without a trailing dash."
  (string-remove-suffix "-" prefix))

(defun superchat-learn--file (prefix)
  "Where the skill of PREFIX is saved."
  (expand-file-name (format "skills/%s/SKILL.md" (superchat-learn--name prefix))
                    superchat-directory))

(defun superchat-learn--beside (prefix name)
  "The file NAME beside PREFIX's skill."
  (expand-file-name name (file-name-directory (superchat-learn--file prefix))))

(defun superchat-learn--read-data (file)
  "The Lisp data of FILE, or nil when there is none."
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (read (current-buffer)))))

(defun superchat-learn--write-data (file data)
  "Write DATA to FILE as Lisp data."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (prin1 data (current-buffer))))

(defun superchat-learn--rejected (prefix)
  "The code of the forms dropped from drafts of PREFIX's skill."
  (superchat-learn--read-data (superchat-learn--beside prefix "rejected.eld")))

(defun superchat-learn--add-evidence (prefix groups)
  "Add the runs of GROUPS to evidence.eld beside PREFIX's skill: an alist
from a template's key to the ids of the results it ran as."
  (let* ((file (superchat-learn--beside prefix "evidence.eld"))
         (evidence (superchat-learn--read-data file)))
    (dolist (g groups)
      (let ((cell (assoc (plist-get g :key) evidence)))
        (if cell
            (setcdr cell (seq-uniq (append (cdr cell) (plist-get g :ids))))
          (push (cons (plist-get g :key) (plist-get g :ids)) evidence))))
    (superchat-learn--write-data file evidence)))

(defun superchat-learn--decide (saved template holes)
  "Match the forms SAVED against TEMPLATE: :fail, or the person's decision on
each hole of HOLES, ((HOLE . hole) or (HOLE . keep) ...), `keep' when the
hole's value was put back."
  (catch 'fail
    (let (out)
      (cl-labels ((match (s x)
                    (let ((h (superchat-learn--named x holes)))
                      (cond (h (cond ((and (symbolp s) (equal (symbol-name s) (plist-get h :name)))
                                      (push (cons h 'hole) out))
                                     ((equal s (plist-get h :value)) (push (cons h 'keep) out))
                                     (t (throw 'fail :fail))))
                            ((and (consp x) (consp s)) (match (car s) (car x)) (match (cdr s) (cdr x)))
                            ((not (equal s x)) (throw 'fail :fail))))))
        (match saved template)
        out))))

(defun superchat-learn--old-version (text)
  "The `version:' of the skill TEXT, or nil."
  (and (string-match "^version: \\(.*\\)$" text) (match-string 1 text)))

(defun superchat-learn-save ()
  "Save this draft as the skill of its package, or add it to that skill.
A template the person deleted is not drafted again; a hole whose value the
person put back is kept as a value in later drafts."
  (interactive)
  (let* ((prefix superchat-learn--prefix)
         (file (superchat-learn--file prefix))
         (text (buffer-string))
         (version superchat-learn--new-version)
         (saved (mapcar #'superchat-learn--forms (superchat-learn--blocks text)))
         kept dropped decided)
    (dolist (g superchat-learn--offered)
      (let ((match (cl-loop for s in saved
                            for r = (superchat-learn--decide s (plist-get g :forms) (plist-get g :holes))
                            unless (eq r :fail) return (list r))))
        (if match
            (progn (push g kept) (setq decided (append (car match) decided)))
          (push (plist-get g :text) dropped))))
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
        (superchat-learn--write-data (superchat-learn--beside prefix "rejected.eld")
                                     (append (superchat-learn--rejected prefix) dropped)))
      (when kept (superchat-learn--add-evidence prefix kept))
      (let ((roles (superchat-learn--read-data (superchat-learn--beside prefix "roles.eld"))))
        (dolist (d decided)
          (when-let* ((fn (plist-get (car d) :fn))
                      ((string-prefix-p prefix (symbol-name fn))))
            (setf (alist-get (cons fn (plist-get (car d) :pos)) roles nil nil #'equal) (cdr d))))
        (when decided (superchat-learn--write-data (superchat-learn--beside prefix "roles.eld") roles)))
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

(defun superchat-learn--show (prefix checked adding version old-version known)
  "Show the draft skill of PREFIX: CHECKED is (TEXT LEFT-OUT SHOWN).
ADDING: it adds to the skill, which had OLD-VERSION; KNOWN forms were
skipped as in it or dropped; VERSION is the package's."
  (pcase-let ((`(,text ,left ,shown) checked))
    (with-current-buffer (get-buffer-create (format "*superchat: learn %s*" prefix))
      (let ((inhibit-read-only t)) (erase-buffer) (insert text))
      (superchat-learn-mode)
      (let* ((holes (apply #'append (mapcar (lambda (g) (plist-get g :holes)) shown)))
             (check (seq-count (lambda (h) (memq (plist-get h :class) '(suspected varies))) holes))
             (written (apply #'+ (mapcar (lambda (g) (length (plist-get g :unknown))) shown))))
        (setq superchat-learn--prefix prefix
              superchat-learn--adding adding
              superchat-learn--new-version version
              superchat-learn--offered shown
              header-line-format
              (concat (format " C-c C-c %s %s   C-c C-k drop"
                              (if adding "add to" "save to")
                              (abbreviate-file-name (superchat-learn--file prefix)))
                      (if adding (format "   %d already there or dropped" known) "")
                      (if holes (format "   %d hole(s)%s" (length holes)
                                        (if (> check 0) (format ", %d to check" check) ""))
                        "")
                      (if (> written 0) (format "   %d value(s) left as written" written) "")
                      (if (> left 0) (format "   %d form(s) left out: not code that ran" left) "")
                      (if (and adding version old-version (not (equal version old-version)))
                          (format "   version %s → %s: recheck older forms" old-version version)
                        ""))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; A form's literals, classed by where they came from.

(defun superchat-learn--forms (code)
  "The forms of CODE in order; nil when it does not read."
  (let ((pos 0) out)
    (condition-case nil
        (while t
          (let ((r (read-from-string code pos))) (push (car r) out) (setq pos (cdr r))))
      (end-of-file nil)
      (invalid-read-syntax (setq out nil)))
    (nreverse out)))

(defun superchat-learn--context (tool)
  "The run of the result TOOL: (USER-TEXT . EARLIER-RESULTS)."
  (let* ((tape (plist-get tool :tape))
         (user (car (superchat-tape-entries :tape tape :kinds '(user) :before (plist-get tool :id)
                                            :newest t :limit 1))))
    (cons (or (plist-get user :text) "")
          (and user (superchat-tape-entries :tape tape :kinds '(tool) :after (plist-get user :id)
                                            :before (plist-get tool :id))))))

(defun superchat-learn--telling-p (value)
  "Non-nil when VALUE is rare enough for where it occurs to mean something."
  (or (and (integerp value) (>= (abs value) 100)) (floatp value)
      (and (stringp value) (>= (length value) 3))))

(defun superchat-learn--occurs-p (value text)
  "Non-nil when VALUE occurs in TEXT: a string as written, a number as a token."
  (if (stringp value)
      (string-search value text)
    (string-match-p (format "\\(?:\\`\\|[^0-9.]\\)%s\\(?:\\'\\|[^0-9.]\\)"
                            (regexp-quote (number-to-string value)))
                    text)))

(defun superchat-learn--origin (value context)
  "Where the literal VALUE came from in its run CONTEXT: `input', (copied . ID),
`suspected', `unknown' when it occurs nowhere, `short' when it is too common."
  (if (not (superchat-learn--telling-p value))
      'short
    (let ((asked (superchat-learn--occurs-p value (car context)))
          (from (seq-filter (lambda (r) (superchat-learn--occurs-p value (plist-get r :text)))
                            (cdr context))))
      (cond ((and asked (not from)) 'input)
            ((and (not asked) (not (cdr from)) from) (cons 'copied (plist-get (car from) :id)))
            ((or asked from) 'suspected)
            (t 'unknown)))))

(defun superchat-learn--argname (fn i)
  "The name of argument I of FN, upcased, or nil."
  (let* ((arglist (and (symbolp fn) (fboundp fn) (help-function-arglist fn t)))
         (rest (cadr (memq '&rest (and (listp arglist) arglist))))
         (fixed (and (listp arglist)
                     (seq-remove (lambda (a) (memq a '(&optional &rest)))
                                 (seq-take-while (lambda (a) (not (eq a '&rest))) arglist))))
         (name (or (nth i fixed) rest)))
    (and name (symbolp name) (upcase (symbol-name name)))))

(defun superchat-learn--sink (var body)
  "The name of the argument VAR is first passed as in BODY, or nil."
  (catch 'found
    (let ((walk nil))
      (setq walk (lambda (x)
                   (when (and (consp x) (proper-list-p x) (not (eq (car x) 'quote)))
                     (seq-do-indexed (lambda (a i) (when (eq a var)
                                                     (throw 'found (superchat-learn--argname (car x) i))))
                                     (cdr x))
                     (mapc walk x))))
      (mapc walk body)
      nil)))

(defvar superchat-learn--found nil "The holes and marks of the form being made a template.")
(defvar superchat-learn--roles nil "The person's decisions: ((FN . POS) . hole or keep).")

(defun superchat-learn--class (value context fn pos)
  "The class of literal VALUE at argument POS of FN: the person's decision
first, then its origin in CONTEXT."
  (pcase (cdr (assoc (cons fn pos) superchat-learn--roles))
    ('hole 'decided)
    ('keep 'kept)
    (_ (superchat-learn--origin value context))))

(defun superchat-learn--hole (value name class fn pos)
  "A hole for VALUE named NAME, or the hole already made for that value."
  (let ((same (seq-find (lambda (h) (equal (plist-get h :value) value))
                        (plist-get superchat-learn--found :holes))))
    (if same
        (plist-get same :symbol)
      (let* ((base (or name "VALUE")) (n 1) (taken (mapcar (lambda (h) (plist-get h :name))
                                                            (plist-get superchat-learn--found :holes)))
             (name base))
        (while (member name taken) (setq name (format "%s%d" base (cl-incf n))))
        (let ((symbol (make-symbol name)))
          (setq superchat-learn--found
                (plist-put superchat-learn--found :holes
                           (append (plist-get superchat-learn--found :holes)
                                   (list (list :name name :symbol symbol :value value
                                               :class class :fn fn :pos pos)))))
          symbol)))))

(defun superchat-learn--literal (x context name fn pos)
  "X, a literal or a quoted list of literals, as a hole or as it is."
  (let* ((values (if (consp x) (cadr x) (list x)))
         (classes (mapcar (lambda (v) (superchat-learn--class v context fn pos)) values))
         (copies (delete-dups (seq-filter #'consp classes)))
         (class (cond ((memq 'kept classes) nil)
                      ((or (memq 'suspected classes) (cdr copies)
                           (and copies (or (memq 'input classes) (memq 'decided classes))))
                       'suspected)
                      (copies (car copies))
                      ((memq 'input classes) 'input)
                      ((memq 'decided classes) 'decided))))
    (cond (class (superchat-learn--hole x name class fn pos))
          (t (when (memq 'unknown classes)
               (push x (plist-get superchat-learn--found :unknown)))
             x))))

(defun superchat-learn--walk (x context &optional name fn pos)
  "X with its input and copied literals made holes; holes go to
`superchat-learn--found'.  NAME, FN, POS: the argument X is."
  (cond
   ((or (numberp x) (stringp x)) (superchat-learn--literal x context name fn pos))
   ((not (consp x)) x)
   ((eq (car x) 'quote)
    (let ((d (cadr x)))
      (if (and (proper-list-p d) d (seq-every-p (lambda (e) (or (numberp e) (stringp e))) d))
          (superchat-learn--literal x context name fn pos)
        x)))
   ((and (memq (car x) '(let let*)) (listp (cadr x)))
    (cons (car x)
          (cons (mapcar (lambda (b)
                          (if (and (consp b) (cdr b))
                              (list (car b) (superchat-learn--walk
                                             (cadr b) context
                                             (or (superchat-learn--sink (car b) (cddr x))
                                                 (upcase (symbol-name (car b))))))
                            b))
                        (cadr x))
                (mapcar (lambda (y) (superchat-learn--walk y context)) (cddr x)))))
   ((and (eq (car x) 'setq) (proper-list-p x))
    (cons 'setq (seq-map-indexed (lambda (y i)
                                   (if (cl-oddp i)
                                       (superchat-learn--walk y context
                                                              (upcase (symbol-name (nth (1- i) (cdr x)))))
                                     y))
                                 (cdr x))))
   ((and (symbolp (car x)) (proper-list-p x))
    (cons (car x) (seq-map-indexed (lambda (y i)
                                     (superchat-learn--walk y context (superchat-learn--argname (car x) i)
                                                            (car x) i))
                                   (cdr x))))
   ((proper-list-p x) (mapcar (lambda (y) (superchat-learn--walk y context)) x))
   (t x)))

(defun superchat-learn--named (x holes)
  "The hole of HOLES that the symbol X names, or nil."
  (and (symbolp x) x (seq-find (lambda (h) (equal (plist-get h :name) (symbol-name x))) holes)))

(defun superchat-learn--fill (template holes)
  "TEMPLATE with each symbol that names a hole of HOLES given its value."
  (cond ((superchat-learn--named template holes)
         (plist-get (superchat-learn--named template holes) :value))
        ((consp template) (cons (superchat-learn--fill (car template) holes)
                                (superchat-learn--fill (cdr template) holes)))
        (t template)))

(defun superchat-learn--template (code context)
  "CODE as a template: (:text TEXT :forms FORMS :holes HOLES :unknown VALUES).
The template is kept only when reading TEXT and putting each hole's value
back gives CODE's forms exactly; otherwise CODE itself, without holes."
  (let* ((forms (superchat-learn--forms code))
         (superchat-learn--found nil)
         (template (mapcar (lambda (f) (superchat-learn--walk f context)) forms))
         (holes (plist-get superchat-learn--found :holes))
         (text (string-trim (mapconcat #'pp-to-string template "\n")))
         (back (and holes (superchat-learn--forms text))))
    (if (and holes (equal (superchat-learn--fill back holes) forms))
        (list :text text :forms back :holes holes :unknown (plist-get superchat-learn--found :unknown))
      (list :text (string-trim code) :forms forms
            :unknown (plist-get superchat-learn--found :unknown)))))

(defun superchat-learn--canon (forms)
  "FORMS with each local variable of a `let', `let*' or `lambda' renamed in
the order it is bound, for comparing two forms."
  (let ((n 0))
    (cl-labels ((walk (x names)
                  (cond ((symbolp x) (or (cdr (assq x names)) x))
                        ((not (consp x)) x)
                        ((eq (car x) 'quote) x)
                        ((and (memq (car x) '(let let*)) (listp (cadr x)) (proper-list-p x))
                         (let* ((inner names)
                                (binds (mapcar (lambda (b)
                                                 (let* ((var (if (consp b) (car b) b))
                                                        (init (and (consp b) (walk (cadr b) (if (eq (car x) 'let*) inner names))))
                                                        (new (intern (format "v%d" (cl-incf n)))))
                                                   (push (cons var new) inner)
                                                   (list new init)))
                                               (cadr x))))
                           (cons (car x) (cons binds (mapcar (lambda (y) (walk y inner)) (cddr x))))))
                        ((and (eq (car x) 'lambda) (listp (cadr x)) (proper-list-p x))
                         (let* ((inner names)
                                (args (mapcar (lambda (a)
                                                (if (memq a '(&optional &rest)) a
                                                  (let ((new (intern (format "v%d" (cl-incf n)))))
                                                    (push (cons a new) inner) new)))
                                              (cadr x))))
                           (cons 'lambda (cons args (mapcar (lambda (y) (walk y inner)) (cddr x))))))
                        ((proper-list-p x) (mapcar (lambda (y) (walk y names)) x))
                        (t (cons (walk (car x) names) (walk (cdr x) names))))))
      (prin1-to-string (mapcar (lambda (f) (walk f nil)) forms)))))

(defun superchat-learn--key (code)
  "The comparing key of CODE: its forms with locals renamed."
  (superchat-learn--canon (superchat-learn--forms code)))

;;;###autoload
(defun superchat-learn (prefix)
  "Draft the skill of package PREFIX from cited code that ran: each form a
template whose literals that came from the request or from an earlier result
are holes."
  (interactive "sPackage prefix: ")
  (let* ((all (or (superchat-view 'recipes :name prefix)
                  (user-error "No answer has cited code that names %s and ran" prefix)))
         (file (superchat-learn--file prefix))
         (old (and (file-exists-p file)
                   (with-temp-buffer (insert-file-contents file) (buffer-string))))
         (shape (lambda (code) (superchat-learn--shape (superchat-learn--forms code))))
         (in-skill (mapcar shape (and old (superchat-learn--blocks old))))
         (known (append in-skill (mapcar shape (superchat-learn--rejected prefix))))
         (groups (let ((superchat-learn--roles
                        (superchat-learn--read-data (superchat-learn--beside prefix "roles.eld"))))
                   (superchat-learn--groups all)))
         (fresh (seq-remove (lambda (g) (member (plist-get g :shape) known)) groups))
         (again (seq-filter (lambda (g) (member (plist-get g :shape) in-skill)) groups))
         (version (superchat-learn--version prefix)))
    (when again (superchat-learn--add-evidence prefix again))
    (if (not fresh)
        (message "No new form for the skill of %s: all %d are in it or were dropped" prefix (length groups))
      (message "Drafting the skill of %s from %d form(s)..." prefix (length fresh))
      (superchat-run (superchat-tape-new-name) (superchat-learn--prompt prefix fresh old)
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
                         (superchat-learn--show prefix
                                                (superchat-learn--check (plist-get run :text) fresh
                                                                        (and (not old) version))
                                                (and old t) version
                                                (and old (superchat-learn--old-version old))
                                                (- (length groups) (length fresh))))))))))

(defun superchat-learn--blank (x)
  "X with each literal, quoted list of literals and UPPER-CASE symbol as `_'."
  (let ((case-fold-search nil))
    (cond ((or (numberp x) (stringp x)) '_)
          ((and (symbolp x) x (not (eq x t)) (string-match-p "\\`[A-Z][A-Z0-9-]*\\'" (symbol-name x))) '_)
          ((and (eq (car-safe x) 'quote) (proper-list-p (cadr x)) (cadr x)
                (seq-every-p (lambda (e) (or (numberp e) (stringp e))) (cadr x)))
           '_)
          ((consp x) (cons (superchat-learn--blank (car x)) (superchat-learn--blank (cdr x))))
          (t x))))

(defun superchat-learn--shape (forms)
  "The shape of FORMS: equal for runs of one procedure with other values."
  (superchat-learn--canon (superchat-learn--blank forms)))

(defun superchat-learn--holds-p (x holes)
  "Non-nil when X is, or holds, a symbol naming a hole of HOLES."
  (if (consp x)
      (or (superchat-learn--holds-p (car x) holes) (superchat-learn--holds-p (cdr x) holes))
    (superchat-learn--named x holes)))

(defun superchat-learn--generalize (instances)
  "One template for INSTANCES of one shape, each (:forms TEMPLATE :holes HOLES
:code FORMS): (:forms TEMPLATE :holes HOLES), or nil when a run's code does not
come back from it.  A position whose values differ is a hole; one that had no
hole yet is of class `varies'."
  (let (holes (all (mapcar (lambda (_) nil) instances)))
    (cl-labels
        ((hole (xs name fn pos)
           (let* ((found (cl-mapcar (lambda (x i) (superchat-learn--named x (plist-get i :holes)))
                                    xs instances))
                  (old (seq-find #'identity found))
                  (base (or (plist-get old :name) name "VALUE"))
                  (hname base) (n 1))
             ;; A hole the runs already had, met again: its values are recorded.
             (when (and old (seq-find (lambda (h) (equal (plist-get h :name) base)) holes))
               (cl-return-from hole (intern base)))
             (while (seq-find (lambda (h) (equal (plist-get h :name) hname)) holes)
               (setq hname (format "%s%d" base (cl-incf n))))
             (setq all (cl-mapcar (lambda (x h vals) (cons (cons hname (if h (plist-get h :value) x)) vals))
                                  xs found all))
             (push (list :name hname :class (or (plist-get old :class) 'varies)
                         :value (if (car found) (plist-get (car found) :value) (car xs))
                         :fn (or (plist-get old :fn) fn) :pos (or (plist-get old :pos) pos))
                   holes)
             (intern hname)))
         (walk (xs name fn pos)
           (let ((x (car xs)))
             (cond ((and (seq-every-p (lambda (y) (equal y x)) xs)
                         (not (cl-some (lambda (y i) (superchat-learn--holds-p y (plist-get i :holes)))
                                       xs instances)))
                    x)
                   ((or (seq-some #'atom xs) (seq-some (lambda (y) (eq (car-safe y) 'quote)) xs))
                    (hole xs name fn pos))
                   ((and (symbolp (car x)) (proper-list-p x))
                    (cons (car x) (cl-loop for i from 0 below (length (cdr x))
                                           collect (walk (mapcar (lambda (y) (nth (1+ i) y)) xs)
                                                         (superchat-learn--argname (car x) i) (car x) i))))
                   (t (cons (walk (mapcar #'car xs) name fn pos) (walk (mapcar #'cdr xs) name fn pos)))))))
      (let ((template (walk (mapcar (lambda (i) (plist-get i :forms)) instances) nil nil nil)))
        (when (cl-every (lambda (i vals)
                          (equal (superchat-learn--fill
                                  template (mapcar (lambda (v) (list :name (car v) :value (cdr v))) vals))
                                 (plist-get i :code)))
                        instances all)
          (list :forms template :holes (nreverse holes)))))))

(defun superchat-learn--groups (recipes)
  "RECIPES as templates, newest first; each a plist :key :shape :text :forms
:holes :unknown :ids :returned.  Runs of one shape share one template."
  (let (buckets)
    (dolist (e recipes)
      (let* ((code (plist-get (plist-get e :data) :code))
             (tpl (superchat-learn--template code (superchat-learn--context e)))
             (shape (superchat-learn--shape (plist-get tpl :forms)))
             (one (append (list :key (superchat-learn--key (plist-get tpl :text)) :shape shape
                                :code (superchat-learn--forms code) :ids (list (plist-get e :id))
                                :returned (car (split-string (plist-get e :text) "\n")))
                          tpl))
             (bucket (assoc shape buckets)))
        (if bucket (setcdr bucket (append (cdr bucket) (list one))) (push (list shape one) buckets))))
    (apply #'append
           (mapcar
            (lambda (bucket)
              (let* ((runs (cdr bucket))
                     (ids (apply #'append (mapcar (lambda (r) (plist-get r :ids)) runs)))
                     (texts (delete-dups (mapcar (lambda (r) (plist-get r :key)) runs)))
                     (general (and (cdr texts) (superchat-learn--generalize runs))))
                (cond ((not (cdr texts)) (list (plist-put (car runs) :ids ids)))
                      (general
                       (let ((text (string-trim (mapconcat #'pp-to-string (plist-get general :forms) "\n"))))
                         (list (append (list :key (superchat-learn--key text) :text text :ids ids
                                             :forms (superchat-learn--forms text)
                                             :unknown (apply #'append (mapcar (lambda (r) (plist-get r :unknown)) runs)))
                                       general (car runs)))))
                      (t runs))))
            (nreverse buckets)))))

(provide 'superchat-learn)
;;; superchat-learn.el ends here
