;;; kernel-check.el --- Check and annotate KERNEL citations -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loadable as a library and can also be run directly:
;;
;;   emacs -Q --batch -L . -l test/kernel-check.el -- check KERNEL.md
;;   emacs -Q --batch -L . -l test/kernel-check.el -- convert KERNEL.md OUT.md
;;   emacs -Q --batch -L . -l test/kernel-check.el -- pairs KERNEL.md OUT.md
;;
;; `--strict' may be added to `check'.  `--root DIRECTORY' changes the
;; directory used to resolve cited source files; by default it is the
;; current working directory, which also lets a converted /tmp/K4.md refer
;; back to the checkout in which the command is run.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst kernel-check--citation-regexp
  "\\`\\(?:\\([[:alnum:]_./+~?-]+\\)\\)?\\(:\\([0-9]+\\)\\)\\(?:@\\([^`@[:space:]]+\\)\\)?\\'"
  "Regexp for one citation inside a Markdown code span.

Group 1 is an optional complete file name, group 3 is the line number,
and group 4 is an optional construct name.")

(defconst kernel-check--named-form-heads
  '(defun defmacro defvar defvar-local defcustom defconst defsubst
    cl-defun cl-defmacro cl-defstruct cl-defgeneric cl-defmethod
    ert-deftest)
  "Top-level form heads whose next argument names the construct.")

(defconst kernel-check--universal-term-regexp
  "\\_<\\(?:only\\|every\\|never\\|exactly\\|always\\|single\\|sole\\|nothing\\|nowhere\\|all\\)\\_>\\|\\_<no[[:space:]]+other\\_>"
  "Regexp for words that make a sentence a universal-quantifier sentence.")

(defun kernel-check--el-file-p (file)
  "Return non-nil when FILE names an Emacs Lisp source file."
  (and (stringp file)
       (string-equal (downcase (or (file-name-extension file) "")) "el")))

(defun kernel-check--entry-name (entry)
  "Return ENTRY's display name, or a stable placeholder for no entry."
  (or (and entry (plist-get entry :name)) "<outside entries>"))

(defun kernel-check--document-entries ()
  "Return the `###' entries in the current Markdown buffer.

An entry runs from its `### ' heading through the next `### ' or `## '
heading.  The current buffer is not modified."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^### \\(.*\\)$" nil t)
        (let ((start (line-beginning-position))
              (name (string-trim (match-string-no-properties 1)))
              (body-start (save-excursion
                            (forward-line 1)
                            (point))))
          (push (list :name name :start start :body-start body-start)
                headings))))
    (setq headings (nreverse headings))
    (mapcar
     (lambda (heading)
       (let ((next (save-excursion
                     (goto-char (plist-get heading :body-start))
                     (when (re-search-forward "^###? " nil t)
                       (match-beginning 0)))))
         (list :name (plist-get heading :name)
               :start (plist-get heading :start)
               :body-start (plist-get heading :body-start)
               :end (or next (point-max)))))
     headings)))

(defun kernel-check--entry-at-position (position entries)
  "Return the entry in ENTRIES that contains POSITION."
  (cl-find-if
   (lambda (entry)
     (and (<= (plist-get entry :start) position)
          (< position (plist-get entry :end))))
   entries))

(defun kernel-check--citation-specs (text)
  "Return citation specs parsed from one code-span TEXT.

The citation grammar also permits a single code span to contain a range such
as `file.el:10-12` or `file.el:10,20-22`.  Every endpoint is returned as a
  separate spec so checking and conversion inspect both ends of every span."
  (cond
   ;; Try the single-citation grammar first so a construct whose name ends
   ;; in `-<digits>' is not mistaken for an unannotated range.
   ((string-match kernel-check--citation-regexp text)
    (list (list :file (match-string-no-properties 1 text)
                :line (string-to-number (match-string-no-properties 3 text))
                :construct (match-string-no-properties 4 text)
                :relative-token-start 0
                :relative-token-end (length text)
                :relative-annotation-position (match-end 2))))
   ((string-match
     "\\`\\([^:]+\\):\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?,\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?-\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?\\'"
     text)
    (let ((file (match-string-no-properties 1 text)))
      (list
       (list :file file
             :line (string-to-number (match-string-no-properties 2 text))
             :construct (match-string-no-properties 3 text)
             :relative-token-start (match-beginning 2)
             :relative-token-end (or (match-end 3) (match-end 2))
             :relative-annotation-position (match-end 2))
       (list :file file
             :line (string-to-number (match-string-no-properties 4 text))
             :construct (match-string-no-properties 5 text)
             :relative-token-start (match-beginning 4)
             :relative-token-end (or (match-end 5) (match-end 4))
             :relative-annotation-position (match-end 4))
       (list :file file
             :line (string-to-number (match-string-no-properties 6 text))
             :construct (match-string-no-properties 7 text)
             :relative-token-start (match-beginning 6)
             :relative-token-end (or (match-end 7) (match-end 6))
             :relative-annotation-position (match-end 6)))))
   ((string-match
     "\\`\\([^:]+\\):\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?-\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?\\'"
     text)
    (let ((file (match-string-no-properties 1 text)))
      (list
       (list :file file
             :line (string-to-number (match-string-no-properties 2 text))
             :construct (match-string-no-properties 3 text)
             :relative-token-start (match-beginning 2)
             :relative-token-end (or (match-end 3) (match-end 2))
             :relative-annotation-position (match-end 2))
       (list :file file
             :line (string-to-number (match-string-no-properties 4 text))
             :construct (match-string-no-properties 5 text)
             :relative-token-start (match-beginning 4)
             :relative-token-end (or (match-end 5) (match-end 4))
             :relative-annotation-position (match-end 4)))))
   ((string-match
     "\\`:\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?-\\([0-9]+\\)\\(?:@\\([^`@,[:space:]]+\\)\\)?\\'"
     text)
    (list
     (list :file nil
           :line (string-to-number (match-string-no-properties 1 text))
           :construct (match-string-no-properties 2 text)
           :relative-token-start (match-beginning 1)
           :relative-token-end (or (match-end 2) (match-end 1))
           :relative-annotation-position (match-end 1))
     (list :file nil
           :line (string-to-number (match-string-no-properties 3 text))
           :construct (match-string-no-properties 4 text)
           :relative-token-start (match-beginning 3)
           :relative-token-end (or (match-end 4) (match-end 3))
           :relative-annotation-position (match-end 3))))
   ;; Unannotated range forms need endpoint positions for checking and
   ;; byte-local conversion.
   ((string-match
     "\\`\\([^:]+\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)\\'"
     text)
    (let ((file (match-string-no-properties 1 text)))
      (list
       (list :file file
             :line (string-to-number (match-string-no-properties 2 text))
             :construct nil
             :relative-token-start (match-beginning 2)
             :relative-token-end (match-end 2)
             :relative-annotation-position (match-end 2))
       (list :file file
             :line (string-to-number (match-string-no-properties 3 text))
             :construct nil
             :relative-token-start (match-beginning 3)
             :relative-token-end (match-end 3)
             :relative-annotation-position (match-end 3))
       (list :file file
             :line (string-to-number (match-string-no-properties 4 text))
             :construct nil
             :relative-token-start (match-beginning 4)
             :relative-token-end (match-end 4)
             :relative-annotation-position (match-end 4)))))
   ((string-match
     "\\`\\([^:]+\\):\\([0-9]+\\)-\\([0-9]+\\)\\'"
     text)
    (let ((file (match-string-no-properties 1 text)))
      (list
       (list :file file
             :line (string-to-number (match-string-no-properties 2 text))
             :construct nil
             :relative-token-start (match-beginning 2)
             :relative-token-end (match-end 2)
             :relative-annotation-position (match-end 2))
       (list :file file
             :line (string-to-number (match-string-no-properties 3 text))
             :construct nil
             :relative-token-start (match-beginning 3)
             :relative-token-end (match-end 3)
             :relative-annotation-position (match-end 3)))))
   ))

(defun kernel-check--parse-references-in-buffer (&optional on-error)
  "Return citation plists found in the current Markdown buffer.

Short citations inherit the file from the previous complete citation in the
same `###' entry.  Returned references retain buffer positions so the
converter can insert annotations without rewriting any other text."
  (let ((entries (kernel-check--document-entries))
        (references nil)
        (last-entry :unset)
        last-file)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\r\n]*\\)`" nil t)
        (let* ((span-start (match-beginning 0))
               (span-end (match-end 0))
               (content (match-string-no-properties 1))
               (entry (kernel-check--entry-at-position span-start entries)))
          (unless (eq entry last-entry)
            (setq last-entry entry
                  last-file nil))
          (dolist (spec (kernel-check--citation-specs-or-report content entry on-error))
            (let* ((complete-file (plist-get spec :file))
                   (file (or complete-file last-file))
                   (content-start (1+ span-start)))
              (when complete-file
                (setq last-file complete-file))
              (push
               (list :file file
                     :file-token complete-file
                     :short-p (null complete-file)
                     :line (plist-get spec :line)
                     :construct (plist-get spec :construct)
                     :entry-name (kernel-check--entry-name entry)
                     :raw content
                     :span-start span-start
                     :span-end span-end
                     :token-start (+ content-start
                                     (plist-get spec :relative-token-start))
                     :token-end (+ content-start
                                   (plist-get spec :relative-token-end))
                     :annotation-position (+ content-start
                                             (plist-get spec
                                                        :relative-annotation-position)))
               references))))))
    (nreverse references)))

(defun kernel-check--parse-references (file &optional on-error)
  "Return citation plists parsed from Markdown FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (kernel-check--parse-references-in-buffer on-error)))

(defun kernel-check--skip-space-and-comments ()
  "Move over top-level whitespace and comments in the current buffer."
  (let ((moved t))
    (while moved
      (skip-chars-forward " \t\n\r")
      (let ((before (point)))
        (condition-case nil
            (progn
              (forward-comment 1)
              (setq moved (/= before (point))))
          (error
           (setq moved nil))))
      (when (>= (point) (point-max))
        (setq moved nil)))
    (skip-chars-forward " \t\n\r")))

(defun kernel-check--read-top-level-forms ()
  "Return a plist of parsed forms and at most one reader diagnostic.

The Lisp reader, rather than indentation or a regular expression, determines
the form boundaries.  A malformed source file stops the scan and records the
position of that first reader failure so callers can report it once."
  (let (forms reader-error)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (kernel-check--skip-space-and-comments)
        (when (< (point) (point-max))
          (let ((start (point)))
            (condition-case err
                (let ((form (read (current-buffer))))
                  (push (list :start start
                              :end (point)
                              :form form)
                        forms))
              (end-of-file
               (setq reader-error
                     (list :position start
                           :line (line-number-at-pos start)
                           :message "unexpected end of file"))
               (goto-char (point-max)))
              (error
               (setq reader-error
                     (list :position start
                           :line (line-number-at-pos start)
                           :message (error-message-string err)))
               (goto-char (point-max))))))))
    (list :forms (nreverse forms)
          :reader-error reader-error)))

(defun kernel-check--name-token (value)
  "Return the symbol name represented by VALUE, if any."
  (cond
   ((symbolp value) (symbol-name value))
   ((and (consp value)
         (eq (car value) 'quote)
         (symbolp (cadr value)))
    (symbol-name (cadr value)))
   ((and (consp value) (symbolp (car value)))
    (symbol-name (car value)))
   (t nil)))

(defun kernel-check--first-quoted-symbol (object)
  "Return the first symbol directly wrapped in `quote' under OBJECT."
  (cond
   ((and (consp object)
         (eq (car object) 'quote)
         (symbolp (cadr object)))
    (symbol-name (cadr object)))
   ((consp object)
    (or (kernel-check--first-quoted-symbol (car object))
        (kernel-check--first-quoted-symbol (cdr object))))
   (t nil)))

(defun kernel-check--form-construct-name (form)
  "Return the construct name represented by parsed top-level FORM.

Named definition heads use their name argument.  For an unnamed top-level
form, the result is HEAD/FIRST-QUOTED-SYMBOL when a quoted symbol exists, or
HEAD otherwise; an object without a symbol head returns its Lisp type name."
  (let* ((head (car-safe form))
         (head-name (and (symbolp head) (symbol-name head)))
         (name-argument (and (consp form) (cadr form))))
    (if (or (not (consp form)) (not (symbolp head))) (symbol-name (type-of form))
      (if (or (memq head kernel-check--named-form-heads)
              (string-prefix-p "define-" head-name))
          (or (kernel-check--name-token name-argument) head-name)
        (let ((quoted (kernel-check--first-quoted-symbol (cdr form))))
          (if quoted
              (concat head-name "/" quoted)
            head-name))))))

(defun kernel-check--line-interval (line)
  "Return (START . END) for LINE in the current buffer, or nil."
  (when (and (integerp line) (> line 0))
    (save-excursion
      (goto-char (point-min))
      (when (= 0 (forward-line (1- line)))
        (cons (line-beginning-position) (line-end-position))))))

(defun kernel-check--form-intersects-line-p (form line-start line-end)
  "Return non-nil when FORM overlaps the line interval."
  (and (< (plist-get form :start)
          (min (point-max) (1+ line-end)))
       (< line-start (plist-get form :end))))

(defun kernel-check--reader-diagnostic (file diagnostic diagnostics)
  "Record DIAGNOSTIC for FILE in DIAGNOSTICS and return its message.

DIAGNOSTICS is nil for callers that only need the construct result."
  (let ((message
         (format "%s:%d: reader failed at position %d: %s"
                 file
                 (plist-get diagnostic :line)
                 (plist-get diagnostic :position)
                 (plist-get diagnostic :message))))
    (when (and diagnostics (not (gethash file diagnostics)))
      (puthash file message diagnostics))
    message))

(defun kernel-check--construct-info (file line &optional diagnostics)
  "Return construct and reader-diagnostic information for FILE and LINE.

The optional DIAGNOSTICS hash table receives at most one diagnostic per FILE."
  (when (and (kernel-check--el-file-p file)
             (integerp line)
             (> line 0))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)
          (let ((interval (kernel-check--line-interval line)))
            (when interval
              (let* ((parsed (kernel-check--read-top-level-forms))
                     (reader-error (plist-get parsed :reader-error))
                     (diagnostic
                      (and reader-error
                           (kernel-check--reader-diagnostic
                            file reader-error diagnostics)))
                     (form
                      (cl-find-if
                       (lambda (candidate)
                         (kernel-check--form-intersects-line-p
                          candidate (car interval) (cdr interval)))
                       (plist-get parsed :forms))))
                (list :construct
                      (and form
                           (kernel-check--form-construct-name
                            (plist-get form :form)))
                      :reader-error diagnostic)))))
      (error nil))))

(defun kernel-check--construct-at (file line &optional diagnostics)
  "Return the construct containing LINE in Emacs Lisp FILE, or nil.

Constructs are parsed top-level forms, not indentation guesses.  A named
`defun', `defmacro', `defvar', `defvar-local', `defcustom', `defconst',
`defsubst', `cl-defun', `cl-defmacro', `cl-defstruct', `cl-defgeneric',
  `cl-defmethod', `define-*', or `ert-deftest' returns its NAME.  An unnamed
top-level form returns HEAD/FIRST-QUOTED-SYMBOL, or HEAD when no quoted
symbol exists.  A top-level object with no symbol head returns its Lisp type
name.  Comments, file headers, autoload cookies, blank lines, and any line
outside a top-level form returns nil; DIAGNOSTICS records reader failures."
  (plist-get (kernel-check--construct-info file line diagnostics)
             :construct))

(defun kernel-check--source-line (path line)
  "Return status and text for LINE in PATH."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (if (/= 0 (forward-line (1- line)))
            (list :status 'out-of-range)
          (let ((content (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
            (list :status (if (string-match-p "\\`[ \t\r]*\\'"
                                               content)
                              'blank
                            'ok)
                  :content content))))
    (error (list :status 'unreadable :error err))))

(defun kernel-check--reference-label (reference)
  "Return a compact display label for REFERENCE."
  (format "%s:%d%s"
          (or (plist-get reference :file) "<unresolved-file>")
          (plist-get reference :line)
          (if (plist-get reference :construct)
              (concat "@" (plist-get reference :construct))
            "")))

(defun kernel-check--reference-location (reference root)
  "Resolve REFERENCE against ROOT and return its source-line status."
  (let ((file (plist-get reference :file))
        (line (plist-get reference :line)))
    (cond
     ((not (stringp file))
      (list :error
            (format "%s: short citation has no previous complete file in this entry"
                    (kernel-check--reference-label reference))))
     ((not (and (integerp line) (> line 0)))
      (list :error
            (format "%s: line number is not positive"
                    (kernel-check--reference-label reference))))
    (t
      (let* ((path (expand-file-name file (or root default-directory)))
             (line-info
              (if (file-regular-p path)
                  (kernel-check--source-line path line)
                (list :status 'missing))))
        (if (eq (plist-get line-info :status) 'ok)
            (list :file file
                  :path path
                  :line-content (plist-get line-info :content)
                  :status 'ok)
          (list :file file
                :path path
                :line-content (plist-get line-info :content)
                :status (plist-get line-info :status)
                :error
                (format
                 "%s: %s%s"
                 (kernel-check--reference-label reference)
                 (pcase (plist-get line-info :status)
                   ('missing "file does not exist")
                   ('out-of-range "line is out of range")
                   ('blank "referenced line is blank")
                   (_ "source file cannot be read"))
                 (if (plist-get line-info :content)
                     (format ": %s" (plist-get line-info :content))
                   "")))))))))

(defun kernel-check--validate-reference (reference root &optional diagnostics)
  "Validate REFERENCE and return a plist with `:error' when invalid."
  (let ((location (kernel-check--reference-location reference root)))
    (if (plist-get location :error)
        location
      (let* ((file (plist-get reference :file))
             (path (plist-get location :path))
             (expected (plist-get reference :construct))
             (info (and expected
                        (kernel-check--el-file-p file)
                        (kernel-check--construct-info
                         path (plist-get reference :line) diagnostics)))
             (actual (plist-get info :construct))
             (reader-error (plist-get info :reader-error)))
        (if reader-error
            (append location (list :actual actual
                                   :reader-error reader-error))
          (if (and expected
                   (or (not (kernel-check--el-file-p file))
                       (if (equal expected "-") actual (not (equal expected actual)))))
              (list :path path
                    :line-content (plist-get location :line-content)
                    :status 'ok
                    :actual actual
                    :error
                    (format
                     (kernel-check--mismatch-format expected (not (kernel-check--el-file-p file)))
                     (kernel-check--reference-label reference)
                     (if (kernel-check--el-file-p file) (if (equal expected "-") (or actual "nil") expected) expected)
                     (if (equal expected "-") "" (or actual "nil"))
                     (plist-get reference :line)
                     (plist-get location :line-content)))
            (append location (list :actual actual))))))))

(defun kernel-check--bare-el-reference-p (reference)
  "Return non-nil when REFERENCE is an unannotated `.el' citation."
  (and (null (plist-get reference :construct))
       (kernel-check--el-file-p (plist-get reference :file))))

(defun kernel-check--check-buffer (root strict)
  "Check citations in the current buffer against ROOT."
  (let* ((errors nil)
         (warnings nil)
         (diagnostics (make-hash-table :test #'equal))
         (references (kernel-check--parse-references-in-buffer (lambda (message) (push message errors)))))
    (dolist (reference references)
      (when (kernel-check--bare-el-reference-p reference)
        (push (format "[%s] bare .el citation %s"
                      (plist-get reference :entry-name)
                      (kernel-check--reference-label reference))
              warnings))
      (let ((validation (kernel-check--validate-reference
                         reference root diagnostics)))
        (when (plist-get validation :error)
          (push (plist-get validation :error) errors))))
    (maphash (lambda (_file message)
               (push message errors))
             diagnostics)
    (setq errors (nreverse errors)
          warnings (nreverse warnings))
    (list :references references
          :errors errors
          :warnings warnings
          :strict strict
          :error-count (+ (length errors)
                          (if strict (length warnings) 0))
          :exit-code (if (or errors (and strict warnings)) 1 0))))

(defun kernel-check-check (kernel-file &optional strict root)
  "Check citations in KERNEL-FILE and return a result plist.

ROOT defaults to `default-directory', not KERNEL-FILE's directory, so an
output file in `/tmp' can still cite source files in the checkout."
  (with-temp-buffer
    (insert-file-contents kernel-file)
    (kernel-check--check-buffer (or root default-directory) strict)))

(defun kernel-check--write-literal-buffer (output)
  "Write the current unibyte buffer to OUTPUT without recoding it."
  (let ((coding-system-for-write 'no-conversion)
        (write-region-inhibit-fsync t))
    (write-region (point-min) (point-max) output nil 'silent)))

(defun kernel-check--same-file-p (first second)
  "Return non-nil when FIRST and SECOND name the same output path."
  (equal (expand-file-name first) (expand-file-name second)))

(defun kernel-check-convert (input output &optional root)
  "Annotate bare `.el' citations from INPUT into OUTPUT.

Only `@CONSTRUCT' text is inserted.  The input is read and the output is
written literally so all existing bytes, including whitespace and `since:'
lines, remain unchanged.  Return a result plist."
  (when (kernel-check--same-file-p input output)
    (error "converter refuses to overwrite its input"))
  (let (references errors warnings insertions
        (diagnostics (make-hash-table :test #'equal)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally input)
      (setq references
            (kernel-check--parse-references-in-buffer
             (lambda (message)
               (push message errors))))
      (dolist (reference references)
        (let ((validation
               (kernel-check--validate-reference reference
                                                  (or root default-directory)
                                                  diagnostics)))
          (if (plist-get validation :error)
              (push (plist-get validation :error) errors)
            (when (kernel-check--bare-el-reference-p reference)
              (let ((construct
                     (kernel-check--construct-at
                      (plist-get validation :path)
                      (plist-get reference :line)
                      diagnostics)))
                (if (and construct (not (equal construct "-")))
                    (push (cons (plist-get reference :annotation-position)
                                (concat "@" construct))
                          insertions)
                  (if (equal construct "-")
                      (push (format "[%s] reserved construct '-' at %s; left unchanged"
                                    (plist-get reference :entry-name)
                                    (kernel-check--reference-label reference))
                            warnings)
                    (unless (gethash (plist-get validation :path) diagnostics)
                      (push (format "[%s] no construct at %s; left unchanged"
                                    (plist-get reference :entry-name)
                                    (kernel-check--reference-label reference))
                            warnings)))))))))
      (maphash (lambda (_file message)
                 (push message errors))
               diagnostics)
      (setq errors (nreverse errors)
            warnings (nreverse warnings))
      (unless errors
        (dolist (insertion
                 (sort insertions (lambda (left right)
                                    (> (car left) (car right)))))
          (goto-char (car insertion))
          (insert (cdr insertion)))
        (kernel-check--write-literal-buffer output))
      (list :input input
            :output output
            :references references
            :inserted (if errors 0 (length insertions))
            :errors errors
            :warnings warnings
            :error-count (length errors)
            :exit-code (if errors 1 0)))))

(defun kernel-check--mismatch-format (expected &optional non-el)
  "Return the validation message format for EXPECTED."
  (cond
   (non-el "%s: construct marker @%s is only valid for .el citations")
   ((equal expected "-")
    "%s: declared bare, actual construct %s%s, line %d: %s")
   (t "%s: expected construct %s, actual %s, line %d: %s")))

(defun kernel-check--pair-key (reference &optional root)
  "Return the grouping key for REFERENCE."
  (let* ((location (kernel-check--reference-location reference
                                                     (or root default-directory)))
         (file (or (plist-get location :file)
                   (plist-get reference :file)))
         (path (plist-get location :path))
         (construct (or (plist-get reference :construct)
                        (and (kernel-check--el-file-p file)
                             path
                             (kernel-check--construct-at
                              path (plist-get reference :line))))))
    (format "%s%s"
            (or file "<unresolved-file>")
            (if (and construct (not (equal construct "-")))
                (concat "@" construct)
              (format ":%d" (plist-get reference :line))))))

(defun kernel-check--pair-data (references &optional root)
  "Return (GROUP-ORDER . GROUPS) for REFERENCES.

GROUPS is a hash table mapping each key to the ordered entry names that cite
it."
  (let ((groups (make-hash-table :test #'equal))
        group-order)
    (dolist (reference references)
      (let* ((key (kernel-check--pair-key reference root))
             (entry (plist-get reference :entry-name))
             (members (gethash key groups)))
        (unless members
          (push key group-order))
        (unless (member entry members)
          (puthash key (append members (list entry)) groups))))
    (cons (nreverse group-order) groups)))

(defun kernel-check--split-sentences (text)
  "Split TEXT at periods and semicolons outside Markdown code spans."
  (let ((start 0)
        (cursor 0)
        (in-code nil)
        sentences)
    (while (< cursor (length text))
      (let ((char (aref text cursor)))
        (cond
         ((= char ?`)
          (setq in-code (not in-code)))
         ((and (not in-code)
               (or (= char ?.) (= char ?\;)))
          (push (substring text start cursor) sentences)
          (setq start (1+ cursor)))))
      (setq cursor (1+ cursor)))
    (when (< start (length text))
      (push (substring text start) sentences))
    (nreverse sentences)))

(defconst kernel-check--narrowing-term-regexp
  (concat "\\_<not every\\_>\\|\\_<not all\\_>\\|\\_<but not\\_>\\|\\_<except\\_>"
          "\\|\\_<unless\\_>\\|\\_<only when\\_>\\|\\_<only if\\_>"
          "\\|\\_<rather than\\_>\\|\\_<outside\\_>\\|\\_<narrow\\(?:s\\|ed\\|ing\\|er\\|est\\)?\\_>"
          "\\|\\_<no longer\\_>")
  "Regexp for sentences that narrow a universal-quantifier sentence.")

(defun kernel-check--entry-sentences (entry)
  "Return non-empty, trimmed sentences from ENTRY's body."
  (let ((text (buffer-substring-no-properties
               (plist-get entry :body-start)
               (plist-get entry :end))))
    (delq nil
          (mapcar
           (lambda (sentence)
             (let ((trimmed (string-trim sentence)))
               (unless (string-empty-p trimmed)
                 trimmed)))
           (kernel-check--split-sentences text)))))

(defun kernel-check--indexed-sentence-matches (entry regexp)
  "Return `(INDEX . SENTENCE)' matches for REGEXP in ENTRY's body."
  (cl-loop for sentence in (kernel-check--entry-sentences entry)
           for index from 0
           when (string-match-p regexp sentence)
           collect (cons index sentence)))

(defun kernel-check--universal-sentences (entry)
  "Return universal-quantifier sentences from ENTRY's body."
  (let ((case-fold-search t))
    (mapcar #'cdr
            (kernel-check--indexed-sentence-matches
             entry kernel-check--universal-term-regexp))))

(defun kernel-check--narrowing-sentences (entry)
  "Return narrowing sentences from ENTRY's body."
  (let ((case-fold-search t))
    (mapcar #'cdr
            (kernel-check--indexed-sentence-matches
             entry kernel-check--narrowing-term-regexp))))

(defun kernel-check--render-pairs (entries references &optional root)
  "Render a pairing report for ENTRIES and REFERENCES."
  (let* ((pair-data (kernel-check--pair-data references
                                             (or root default-directory)))
         (group-order (car pair-data))
         (groups (cdr pair-data))
         (entry-data nil)
         (universal nil)
         (universal-items nil)
         (narrowed 0)
         (narrowed-entries 0)
         (output "# Kernel citation pairs\n\n"))
    (dolist (entry entries)
      (let ((name (plist-get entry :name))
            (shared nil))
        (dolist (key group-order)
          (let* ((members (gethash key groups))
                 (others (cl-remove name members :test #'equal)))
            (when (and (member name members) others)
              (push (format "- `%s` — %s"
                            key (string-join others ", "))
                    shared))))
        (setq shared (nreverse shared))
        (setq output
              (concat output
                      (format "## %s\n\n### Shared constructs\n\n"
                              name)
                      (if shared
                          (concat (string-join shared "\n") "\n")
                        "None.\n")
                      "\n"))
        (let* ((entry-universal-indexed
                (let ((case-fold-search t))
                  (kernel-check--indexed-sentence-matches
                   entry kernel-check--universal-term-regexp)))
               (entry-narrowing-indexed
                (let ((case-fold-search t))
                  (kernel-check--indexed-sentence-matches
                   entry kernel-check--narrowing-term-regexp)))
               (entry-universal (mapcar #'cdr entry-universal-indexed))
               (entry-narrowing (mapcar #'cdr entry-narrowing-indexed))
               (has-narrowing
                (cl-some
                 (lambda (universal)
                   (cl-some
                    (lambda (narrowing)
                      (/= (car universal) (car narrowing)))
                    entry-narrowing-indexed))
                 entry-universal-indexed)))
          (push (list :name name
                      :universal entry-universal
                      :narrowing entry-narrowing
                      :universal-indexed entry-universal-indexed
                      :narrowing-indexed entry-narrowing-indexed
                      :has-narrowing has-narrowing)
                entry-data))))
    (setq entry-data
          (nreverse entry-data)
          entry-data
          (append
           (cl-remove-if-not (lambda (item)
                              (plist-get item :has-narrowing))
                            entry-data)
           (cl-remove-if (lambda (item)
                           (plist-get item :has-narrowing))
                         entry-data)))
    (dolist (data entry-data)
      (let ((name (plist-get data :name))
            (narrowing-indexed (plist-get data :narrowing-indexed)))
        (when (plist-get data :has-narrowing)
          (setq narrowed-entries (1+ narrowed-entries)))
        (dolist (universal-item (plist-get data :universal-indexed))
          (let* ((sentence (cdr universal-item))
                 (sentence-narrowing
                  (mapcar
                   #'cdr
                   (cl-remove-if
                    (lambda (narrowing)
                      (= (car universal-item) (car narrowing)))
                    narrowing-indexed))))
            (when sentence-narrowing
              (setq narrowed (1+ narrowed)))
            (push (cons name sentence) universal)
            (push (list :name name
                        :sentence sentence
                        :narrowing sentence-narrowing)
                  universal-items)))))
    (setq universal (nreverse universal)
          universal-items (nreverse universal-items))
    (setq output
          (concat output
                  (format "## Universal-quantifier sentences (%d)\n\n"
                          (length universal))
                  (if universal
                      (concat
                       (mapconcat
                        (lambda (item)
                          (concat
                           (format "- **%s**: %s"
                                   (plist-get item :name)
                                   (plist-get item :sentence))
                           (let ((narrowing (plist-get item :narrowing)))
                             (if narrowing
                                 (concat
                                  "\n"
                                  (mapconcat
                                   (lambda (sentence)
                                     (format "  - narrowed by: %s"
                                             sentence))
                                   narrowing
                                   "\n"))
                               ""))))
                        universal-items
                        "\n")
                       "\n")
                    "None.\n")))
    (list :text output
          :group-order group-order
          :groups groups
          :universal universal
          :narrowed narrowed
          :narrowed-entries narrowed-entries)))

(defun kernel-check-pairs (input output &optional root)
  "Write a pairing report for INPUT to OUTPUT and return a result plist.

ROOT is used to resolve `.el' citations when an unannotated citation needs its
construct name; pairing otherwise operates on the Markdown citation text."
  (with-temp-buffer
    (insert-file-contents input)
    (let* ((entries (kernel-check--document-entries))
           (references (kernel-check--parse-references-in-buffer))
           (report (kernel-check--render-pairs entries references
                                               (or root default-directory))))
      (with-temp-file output
        (insert (plist-get report :text)))
      (list :input input
            :output output
            :references references
            :entries entries
            :groups (plist-get report :groups)
            :group-order (plist-get report :group-order)
            :universal (plist-get report :universal)
            :narrowed (plist-get report :narrowed)
            :narrowed-entries (plist-get report :narrowed-entries)
            :exit-code 0))))

(defun kernel-check--summary (result)
  "Return the check summary line for RESULT."
  (format "refs %d, errors %d, warnings %d"
          (length (plist-get result :references))
          (plist-get result :error-count)
          (length (plist-get result :warnings))))

(defun kernel-check--print-check-result (result)
  "Print RESULT in check-mode format."
  (dolist (error-message (plist-get result :errors))
    (princ (format "error: %s\n" error-message)))
  (dolist (warning-message (plist-get result :warnings))
    (princ (format "warning: %s\n" warning-message)))
  (princ (concat (kernel-check--summary result) "\n")))

(defun kernel-check--print-convert-result (result)
  "Print RESULT in convert-mode format."
  (dolist (error-message (plist-get result :errors))
    (princ (format "error: %s\n" error-message)))
  (dolist (warning-message (plist-get result :warnings))
    (princ (format "warning: %s\n" warning-message)))
  (princ
   (format "refs %d, inserted %d, errors %d, warnings %d\n"
           (length (plist-get result :references))
           (plist-get result :inserted)
           (plist-get result :error-count)
           (length (plist-get result :warnings)))))

(defun kernel-check--run-command-line ()
  "Run the command described by `command-line-args-left'."
  (condition-case err
      (when (equal (car command-line-args-left) "--")
        (pop command-line-args-left))
    (error
     (princ (format "error: %s\n" (error-message-string err)))
     (kill-emacs 2)))
  (condition-case err
      (let* ((command (pop command-line-args-left))
             (args command-line-args-left)
             (strict nil)
             (root default-directory)
             positional)
        (while args
          (let ((arg (pop args)))
            (cond
             ((equal arg "--strict")
              (setq strict t))
             ((equal arg "--root")
              (unless args
                (error "--root needs a directory"))
              (setq root (expand-file-name (pop args))))
             ((equal arg "--") nil)
             (t (push arg positional)))))
        (setq positional (nreverse positional))
        (let ((result
               (cond
                ((equal command "check")
                 (unless (= (length positional) 1)
                   (error "usage: check [--strict] FILE"))
                 (kernel-check-check (car positional) strict root))
                ((equal command "convert")
                 (unless (= (length positional) 2)
                   (error "usage: convert INPUT OUTPUT"))
                 (kernel-check-convert (nth 0 positional)
                                       (nth 1 positional)
                                       root))
                ((equal command "pairs")
                 (unless (= (length positional) 2)
                   (error "usage: pairs INPUT OUTPUT"))
                 (kernel-check-pairs (nth 0 positional)
                                     (nth 1 positional)
                                     root))
                (t
                 (error "usage: check|convert|pairs ...")))))
          (cond
           ((equal command "check")
            (kernel-check--print-check-result result))
           ((equal command "convert")
            (kernel-check--print-convert-result result))
           (t
            (princ
             (format "wrote %s: entries %d, shared groups %d, universal sentences %d, narrowed %d in %d entries\n"
                     (plist-get result :output)
                     (length (plist-get result :entries))
                     (length (plist-get result :group-order))
                     (length (plist-get result :universal))
                     (plist-get result :narrowed)
                     (plist-get result :narrowed-entries)))))
          (kill-emacs (plist-get result :exit-code))))
    (error
     (princ (format "error: %s\n" (error-message-string err)))
     (kill-emacs 2))))

(defconst kernel-check--malformed-citation-regexp
  "\\(?:[[:alnum:]_./+~?-]+\\)?:[0-9]+@"
  "Regexp for citation-looking code spans that failed to parse.")

(defun kernel-check--citation-specs-or-report (text entry on-error)
  "Return citation specs for TEXT and report malformed ones when requested."
  (let ((specs (kernel-check--citation-specs text)))
    (when (and on-error (null specs)
               (string-match-p kernel-check--malformed-citation-regexp text))
      (funcall on-error
               (format "[%s] malformed citation `%s`"
                       (kernel-check--entry-name entry)
                       text)))
    specs))

(when (and noninteractive
           command-line-args-left
           (or (equal (car command-line-args-left) "--")
               (member (car command-line-args-left)
                       '("check" "convert" "pairs"))
               (not (string-prefix-p "-" (car command-line-args-left)))))
  (kernel-check--run-command-line))

(provide 'kernel-check)

;;; kernel-check.el ends here
