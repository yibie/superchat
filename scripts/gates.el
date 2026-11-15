;;; gates.el --- Checks of the repository that need no test run -*- lexical-binding: t; -*-

;; usage: emacs -Q --batch -l scripts/gates.el ROOT
;; Every gate is a function from data to a list of problems, so that it can be
;; shown an invalid case first: a gate that accepts one checks nothing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar gates-root nil "The repository being checked.")

(defun gates--read (file)
  "The text of FILE under the root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file gates-root))
    (buffer-string)))

(defun gates--short (file)
  "The name of module FILE in the dependency arrows: `superchat-chat.el' is chat."
  (if (equal file "superchat.el") "superchat.el"
    (string-remove-suffix ".el" (string-remove-prefix "superchat-" file))))

(defun gates-modules (spec)
  "The module table of SPEC as a list of (FILE LAYER CAP)."
  (let (rows (start 0))
    (while (string-match "^| \\(L[0-9]\\) | `\\([^`]+\\.el\\)` | \\([0-9]+\\) |" spec start)
      (push (list (match-string 2 spec) (match-string 1 spec)
                  (string-to-number (match-string 3 spec)))
            rows)
      (setq start (match-end 0)))
    (nreverse rows)))

(defun gates-arrows (spec)
  "The dependency arrows of SPEC: an alist from a module to what it points to."
  (when (string-match "Dependencies go one way only:\n+```\n\\(\\(?:.*\n\\)*?\\)```" spec)
    (let (arrows)
      (dolist (line (split-string (match-string 1 spec) "\n" t))
        (pcase-let ((`(,from ,to) (split-string line " → ")))
          (dolist (f (split-string from ", *"))
            (unless (equal f "every file")
              (setf (alist-get f arrows nil nil #'equal)
                    (append (alist-get f arrows nil nil #'equal)
                            (split-string to ", *")))))))
      arrows)))

(defun gates--reach (module arrows)
  "Every module MODULE may require: what the arrows reach from it, carried on."
  (let ((seen (list "superchat.el")) (todo (list module)))
    (while todo
      (dolist (next (alist-get (pop todo) arrows nil nil #'equal))
        (unless (member next seen) (push next seen) (push next todo))))
    seen))

(defun gates-caps (modules sizes)
  "Problems of SIZES, an alist from file to lines, against MODULES."
  (let ((total (apply #'+ (mapcar #'cdr sizes))) problems)
    (dolist (m modules)
      (when-let* ((n (cdr (assoc (car m) sizes))))
        (when (> n (nth 2 m))
          (push (format "CAP  %s: %d lines, cap %d" (car m) n (nth 2 m)) problems))))
    (when (> total 6000) (push (format "CAP  total %d over 6000" total) problems))
    (dolist (s sizes)
      (unless (assoc (car s) modules)
        (push (format "CAP  %s is not a module of SPEC.md" (car s)) problems)))
    (nreverse problems)))

(defun gates--layer (file modules)
  "The layer of FILE, a module or `test/test-NAME.el', or nil."
  (let ((module (if (string-match "\\`test/test-\\(.*\\)\\.el\\'" file)
                    (concat "superchat-" (match-string 1 file) ".el")
                  file)))
    (nth 1 (assoc module modules))))

(defun gates-requires (file text modules arrows)
  "Problems of the requires in TEXT of module FILE."
  (let ((mine (gates--layer file modules))
        (allowed (gates--reach (gates--short file) arrows))
        (start 0) problems)
    (while (string-match "^(require '\\(superchat[a-z-]*\\))" text start)
      (let* ((feature (match-string 1 text))
             (target (concat feature ".el")))
        (setq start (match-end 0))
        (cond ((string-prefix-p "superchat-bridge" feature)
               (unless (equal file "superchat-llm.el")
                 (push (format "DEPS %s requires %s; only superchat-llm.el knows the bridge"
                               file feature)
                       problems)))
              ((not (assoc target modules))
               (push (format "DEPS %s requires %s, no module" file feature) problems))
              ((string< mine (gates--layer target modules))
               (push (format "DEPS %s (%s) requires %s of a higher layer"
                             file mine feature)
                     problems))
              ((not (member (gates--short target) allowed))
               (push (format "DEPS %s requires %s: no arrow leads there" file feature)
                     problems)))))
    (nreverse problems)))

(defun gates-privates (file text modules)
  "The private names TEXT of FILE uses that belong to another layer, as
\"FILE NAME\" strings."
  (let ((mine (gates--layer file modules)) (start 0) found)
    (while (string-match "\\_<\\(superchat\\(?:-[a-z]+\\)*\\)--[a-z-]+" text start)
      (let* ((name (match-string 0 text))
             (owner (concat (match-string 1 text) ".el"))
             (layer (if (string-prefix-p "superchat-bridge" owner)
                        "bridge"
                      (nth 1 (assoc owner modules)))))
        (setq start (match-end 0))
        (when (and layer (not (equal layer mine)))
          (cl-pushnew (format "%s %s" file name) found :test #'equal))))
    (nreverse found)))

(defun gates-exceptions (found listed)
  "Problems of FOUND, the uses across layers, against LISTED, the lines of
the exceptions file: a use not listed, and a listed line that matches none."
  (let ((keys (mapcar (lambda (l) (string-join (seq-take (split-string l) 2) " ")) listed)))
    (append (mapcar (lambda (f) (format "LAYER %s: a private name of another layer" f))
                    (cl-set-difference found keys :test #'equal))
            (mapcar (lambda (k) (format "LAYER stale exception: %s" k))
                    (cl-set-difference keys found :test #'equal)))))

(defconst gates--banned
  (concat "(\\(?:ignore-errors\\|bound-and-true-p\\|featurep\\)\\_>"
          "\\|(\\(?:fboundp\\|boundp\\) +.superchat"
          "\\|^(\\(?:cl-defstruct\\|cl-defgeneric\\)\\_>")
  "The forms the rules against overflow ban everywhere.")

(defun gates-banned (file text)
  "Problems of the banned forms in TEXT of FILE; `defcustom' only in superchat.el."
  (let (problems)
    (dolist (line (split-string text "\n"))
      (when (or (string-match-p gates--banned line)
                (and (string-match-p "^(defcustom\\_>" line) (not (equal file "superchat.el"))))
        (push (format "RULE %s: %s" file (string-trim line)) problems)))
    (nreverse problems)))

(defun gates-links (file text exists)
  "Problems of the local links in TEXT of FILE, a path under the root;
EXISTS says if an absolute path is there."
  (let ((start 0) problems)
    (while (string-match "\\](\\([^)#]+\\)[^)]*)" text start)
      (let ((target (match-string 1 text)))
        (setq start (match-end 0))
        (unless (or (string-match-p "\\`[a-z]+:" target)
                    (funcall exists (expand-file-name
                                     target (expand-file-name (or (file-name-directory file) "")
                                                              gates-root))))
          (push (format "LINK %s: %s leads to no file" file target) problems))))
    (nreverse problems)))

(defun gates--self-test ()
  "Show every gate an invalid case; the names of those that accept it."
  (let* ((modules '(("superchat.el" "L0" 120) ("superchat-tape.el" "L1" 10)
                    ("superchat-chat.el" "L4" 10)))
         (arrows '(("chat" "tape"))))
    (cl-loop for (name . problems)
             in `(("caps" . ,(gates-caps modules '(("superchat-tape.el" . 11))))
                  ("unknown file" . ,(gates-caps modules '(("superchat-x.el" . 1))))
                  ("layer order" . ,(gates-requires "superchat-tape.el" "(require 'superchat-chat)"
                                                    modules arrows))
                  ("arrows" . ,(gates-requires "superchat-chat.el" "(require 'superchat-chat)"
                                               (cons '("superchat-chat.el" "L4" 1) modules) nil))
                  ("bridge" . ,(gates-requires "superchat-chat.el" "(require 'superchat-bridge)"
                                               modules arrows))
                  ("privates" . ,(gates-exceptions
                                  (gates-privates "superchat-chat.el" "(superchat-tape--db)" modules)
                                  nil))
                  ("stale exception" . ,(gates-exceptions nil '("a.el superchat-x--y why")))
                  ("banned" . ,(gates-banned "superchat-tape.el" "(defcustom x 1 \"\")"))
                  ("links" . ,(gates-links "SPEC.md" "[a](spec/none.md)" #'ignore)))
             unless problems collect name)))

(defun gates-run (root)
  "Check ROOT; print each problem; exit 1 when there is one."
  (setq gates-root (file-name-as-directory root))
  (let* ((spec (gates--read "SPEC.md"))
         (modules (gates-modules spec))
         (arrows (gates-arrows spec))
         (files (sort (directory-files gates-root nil "\\`superchat.*\\.el\\'") #'string<))
         (tests (mapcar (lambda (f) (concat "test/" f))
                        (directory-files (expand-file-name "test" gates-root) nil "\\.el\\'")))
         (docs (cons "SPEC.md" (mapcar (lambda (f) (concat "spec/" f))
                                       (directory-files (expand-file-name "spec" gates-root)
                                                        nil "\\.md\\'"))))
         (listed (cl-remove-if (lambda (l) (or (string-empty-p l) (string-prefix-p "#" l)))
                               (split-string (gates--read "scripts/gate-exceptions") "\n")))
         (problems
          (append
           (mapcar (lambda (g) (format "GATE %s accepts an invalid case" g)) (gates--self-test))
           (unless (and modules arrows) (list "GATE SPEC.md has no module table or no arrows"))
           (gates-caps modules (mapcar (lambda (f) (cons f (with-temp-buffer
                                                             (insert-file-contents
                                                              (expand-file-name f gates-root))
                                                             (count-lines (point-min) (point-max)))))
                                       files))
           (cl-loop for f in files when (assoc f modules)
                    append (gates-requires f (gates--read f) modules arrows))
           (cl-loop for f in files append (gates-banned f (gates--read f)))
           (gates-exceptions (cl-loop for f in (append files tests)
                                      when (gates--layer f modules)
                                      append (gates-privates f (gates--read f) modules))
                             listed)
           (cl-loop for d in docs
                    append (gates-links d (gates--read d)
                                        #'file-exists-p)))))
    (let ((total (cl-loop for f in files sum (with-temp-buffer
                                               (insert-file-contents (expand-file-name f gates-root))
                                               (count-lines (point-min) (point-max))))))
      (dolist (m modules)
        (when (member (car m) files)
          (message "ok   %s %s: %d/%d" (nth 1 m) (car m)
                   (with-temp-buffer (insert-file-contents (expand-file-name (car m) gates-root))
                                     (count-lines (point-min) (point-max)))
                   (nth 2 m))))
      (message "total %d/6000" total))
    (dolist (p problems) (message "%s" p))
    (kill-emacs (if problems 1 0))))

(when noninteractive (gates-run (car command-line-args-left)))

;;; gates.el ends here
