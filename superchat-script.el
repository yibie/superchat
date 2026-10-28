;;; superchat-script.el --- A restricted Emacs Lisp script that calls tools -*- lexical-binding: t; -*-

;;; Commentary:
;; The `elisp_script' tool (M11 slice 6): the model writes Emacs Lisp that
;; calls other tools with (tool "NAME" :ARG VALUE ...) and reports with
;; (out VALUE).  Only the script's output reaches the model, so several
;; calls cost one round trip instead of one each.
;;
;; The language is what `unsafep' accepts -- no global state changes, only
;; functions marked free of side effects -- plus the entry points below, and
;; without `while' and the few functions that allocate without bound, so a
;; script always ends: Emacs is single-threaded, and a script that loops
;; would freeze it.  Nested calls go through `superchat-tool-execute', the
;; one tool pipeline, so the permission level, approval, the Tape facts and
;; the record of edited buffers apply to each exactly as to a direct call.
;; See docs/decisions/accepted/2026-10-02-m11-slice-6-elisp-script.md.
;;
;; The same entry, `superchat-tool', is open to an `eval_elisp' expression
;; (decision of 2026-10-26, eval_elisp as code mode): there the language is
;; all of Emacs Lisp, and the tools give it what plain Lisp does not -- the
;; checks of `edit_buffer', the layer lines, the record of changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'unsafep)
(require 'superchat-tool-execution)

(declare-function superchat-get-llm-tools "superchat-tools"
                  (&optional tool-names input-operation))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(defvar superchat--tool-execution-context)

(defconst superchat-script-tool-name "elisp_script"
  "The name of the script tool.")

(defconst superchat-script-forbidden
  '(while number-sequence make-list make-string make-vector make-bool-vector)
  "Symbols a script may not mention, though `unsafep' would accept them.
`while' can loop forever; the others allocate as much as asked at once.")

(defconst superchat-script-entries '(tool out)
  "Functions a script may call besides those `unsafep' accepts.")

(defconst superchat-script-read-functions
  '(;; Files and directories: questions, never changes.
    expand-file-name abbreviate-file-name file-truename file-relative-name
    file-exists-p file-directory-p file-regular-p file-readable-p
    file-symlink-p file-newer-than-file-p file-attributes
    file-attribute-size file-attribute-modification-time file-attribute-type
    directory-files directory-files-recursively file-expand-wildcards
    locate-dominating-file file-name-directory file-name-nondirectory
    file-name-extension file-name-sans-extension file-name-base
    file-name-as-directory directory-file-name
    ;; Buffers.
    buffer-list get-buffer get-file-buffer find-buffer-visiting buffer-name
    buffer-file-name buffer-live-p buffer-modified-p buffer-size
    buffer-local-value buffer-substring-no-properties
    save-current-buffer set-buffer
    ;; Windows, frames and tabs.
    selected-window selected-frame frame-list window-list window-buffer
    window-live-p window-parameter window-dedicated-p window-total-width
    window-total-height window-body-width window-body-height
    window-left-column window-top-line window-edges window-point window-start
    get-buffer-window get-buffer-window-list frame-parameter
    frame-selected-window frame-root-window tab-bar-tabs
    ;; Time.
    current-time float-time format-time-string decode-time encode-time
    time-add time-subtract time-less-p time-equal-p days-between
    ;; Lists, sequences and strings.
    seq-filter seq-remove seq-map seq-mapn seq-find seq-some seq-every-p
    seq-take seq-drop seq-uniq seq-sort seq-sort-by seq-reverse seq-count
    seq-position seq-contains-p seq-elt seq-length seq-into seq-group-by
    cl-remove-if cl-remove-if-not cl-find cl-find-if cl-some cl-every
    cl-position cl-count cl-subseq cl-sort-by
    string-join string-trim string-trim-left string-trim-right
    string-prefix-p string-suffix-p string-empty-p string-blank-p
    split-string replace-regexp-in-string string-replace
    format-message number-to-string string-to-number
    ;; The environment.
    getenv user-login-name user-full-name system-name)
  "Functions a script may call though `unsafep' does not know them as safe.
They ask about files, buffers, windows, time and data and change nothing a
script could leave behind: the current buffer `set-buffer' changes is put back
when the script ends.  Found from the real runs of 2026-10-03, where 14 of 25
scripts were refused, most for `expand-file-name' (M11 slice 6.4).")

(defconst superchat-script-macros
  '(with-current-buffer when-let when-let* if-let if-let* and-let*)
  "Macros a script may use; they are expanded before the check.
Other macros are refused: one could expand into a loop with no end.")

(defun superchat-script--expand (form)
  "FORM with the macros of `superchat-script-macros' expanded, others kept."
  (cond ((not (consp form)) form)
        ((memq (car form) '(quote function)) form)
        ((memq (car form) superchat-script-macros)
         (superchat-script--expand (macroexpand-1 form)))
        (t (let ((rest form) out)
             (while (consp rest)
               (push (superchat-script--expand (car rest)) out)
               (setq rest (cdr rest)))
             (nconc (nreverse out) rest)))))

(defconst superchat-script-not-callable
  '("elisp_script" "ask_user" "delegate_to_subagent" "delegate_to_subagent_parallel")
  "Tools a script cannot call: itself, and tools that wait for an answer.")

(defconst superchat-script-not-callable-from-eval
  (cons "eval_elisp" superchat-script-not-callable)
  "Tools an `eval_elisp' expression cannot call through `superchat-tool'.
`eval_elisp' itself among them: the expression already is Emacs Lisp.")

(defvar superchat-script--calls nil
  "While a script or an `eval_elisp' expression runs: its nested calls so far.
A list (COUNT PARENT-CONTEXT PARENT-ID CALLER): COUNT the calls made,
PARENT-CONTEXT the calling tool's execution context, PARENT-ID its
execution id, CALLER \"a script\" or \"eval_elisp\".  Nil elsewhere, where
`superchat-tool' cannot be used.")

(defconst superchat-script-max-calls 100
  "Most tool calls one script may make.")

(defconst superchat-script-max-output 10000
  "Most characters of output one script returns; the rest is cut from the middle.")

(defun superchat-script--forbidden-in (form)
  "Return the first symbol of `superchat-script-forbidden' in FORM, or nil."
  (cond ((and (symbolp form) (memq form superchat-script-forbidden)) form)
        ((consp form) (or (superchat-script--forbidden-in (car form))
                          (superchat-script--forbidden-in (cdr form))))))

(defun superchat-script-read (code)
  "Return CODE's forms as one `progn' form.  Signals on a reading error."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (buffer-size)) (not (eobp)))
        (push (condition-case nil
                  (read (current-buffer))
                (end-of-file
                 (error "a parenthesis or a string is not closed")))
              forms))
      (cons 'progn (nreverse forms)))))

(defun superchat-script-check (form)
  "Return why FORM may not run, as a string, or nil when it may.
FORM is checked with the allowed macros expanded (`superchat-script--expand')."
  (let ((forbidden (superchat-script--forbidden-in form))
        (unsafe (let ((safe-functions (append superchat-script-entries
                                              superchat-script-read-functions)))
                  (unsafep (superchat-script--expand form)))))
    (cond (forbidden (format "`%s' is not allowed in a script" forbidden))
          ((and (consp unsafe) (eq (car unsafe) 'global-variable))
           (format "setting the variable `%s' is not allowed in a script"
                   (cadr unsafe)))
          ((and (consp unsafe) (eq (car unsafe) 'function))
           (format "`%s' is not allowed in a script; a script may only read \
and compute -- make changes through a tool, or use eval_elisp" (cadr unsafe)))
          (unsafe (format "%S is not allowed in a script" unsafe)))))

(defvar superchat-llm-tool-names)
(defvar superchat-llm-script-only-tool-names)

(defun superchat-script--callable-tools ()
  "The tools a script may call: the offered ones and the script-only ones."
  (if (eq superchat-llm-tool-names 'all)
      (superchat-get-llm-tools)
    (superchat-get-llm-tools
     (cl-union (copy-sequence superchat-llm-tool-names)
               (bound-and-true-p superchat-llm-script-only-tool-names)
               :test #'equal))))

(defun superchat-script--callable-tool (name caller)
  "The llm.el tool NAME a script or eval_elisp may call, or signal why not.
CALLER is the fourth element of `superchat-script--calls'."
  (when (member name (if (equal caller "eval_elisp")
                         superchat-script-not-callable-from-eval
                       superchat-script-not-callable))
    (error "%s cannot be called from %s" name caller))
  (let ((tool (cl-find name (superchat-script--callable-tools)
                       :key #'llm-tool-name :test #'equal)))
    (cond ((null tool) (error "No tool named %s" name))
          ((llm-tool-async tool) (error "%s cannot be called from %s" name caller))
          (t tool))))

(defun superchat-script--arguments (tool plist)
  "TOOL's positional arguments from the keyword PLIST a script gave."
  (let ((names (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool))))
    (cl-loop for (key _value) on plist by #'cddr
             unless (and (keywordp key)
                         (member (substring (symbol-name key) 1) names))
             do (if names
                    (error "%s has no argument %s; its arguments are %s"
                           (llm-tool-name tool) key
                           (mapconcat (lambda (name) (concat ":" name)) names " "))
                  (error "%s takes no arguments" (llm-tool-name tool))))
    (mapcar (lambda (arg)
              (let ((key (intern (concat ":" (plist-get arg :name)))))
                (if (or (plist-member plist key) (plist-get arg :optional))
                    (plist-get plist key)
                  (error "%s needs %s" (llm-tool-name tool) key))))
            (llm-tool-args tool))))

(defun superchat-script--clip (text)
  "TEXT cut to `superchat-script-max-output' characters, keeping both ends."
  (let ((max superchat-script-max-output))
    (if (<= (length text) max)
        text
      (let ((half (/ max 2)))
        (concat (substring text 0 half)
                (format "\n[... %d characters cut from the middle ...]\n"
                        (- (length text) max))
                (substring text (- (length text) half)))))))

(defun superchat-tool (name &rest arguments)
  "Call the tool NAME with keyword ARGUMENTS and return its result text.
For example (superchat-tool \"edit_buffer\" :buffer_name \"notes\"
:old_string \"a\" :new_string \"b\").  Only inside an `elisp_script' script
or an `eval_elisp' expression; the call goes through
`superchat-tool-execute' like one the model makes, and a failed call
returns its failure text."
  (let ((calls superchat-script--calls))
    (unless calls
      (error "superchat-tool works only inside eval_elisp or elisp_script"))
    (unless (stringp name) (error "A tool name must be a string"))
    (pcase-let ((`(,count ,parent ,parent-id ,caller) calls))
      (when (>= count superchat-script-max-calls)
        (error "More than %d tool calls in one %s"
               superchat-script-max-calls
               (if (equal caller "eval_elisp") "expression" "script")))
      (let* ((target (superchat-script--callable-tool name caller))
             (args (superchat-script--arguments target arguments))
             (context (plist-put (copy-sequence parent) :execution-id
                                 (format "%s/%d" parent-id
                                         (setcar calls (1+ count))))))
        ;; A time limit or a C-g leaves by `throw', past the call's own
        ;; accounting; the call must still end.
        (let ((settled nil))
          (unwind-protect
              (prog1 (superchat-tool-execute name (llm-tool-function target) args
                                             :context context)
                (setq settled t))
            (unless settled
              (superchat-tool-execution-settle-left name context))))))))

(defun superchat-script-run (code)
  "Run the script CODE and return what reaches the model.
The result starts with \"Script completed.\", \"Script failed.\" or
\"Script refused:\".  A failed script keeps the output it made before the
error; calls it made are not undone."
  (let* ((parent superchat--tool-execution-context)
         (calls (list 0 parent (or (plist-get parent :execution-id) "script")
                      "a script"))
         (output nil)
         (form (condition-case err
                   (superchat-script-read code)
                 (error (cons :unreadable (error-message-string err)))))
         (refusal (if (eq (car form) :unreadable)
                      (format "the script cannot be read: %s" (cdr form))
                    (superchat-script-check form))))
    (if refusal
        (format "Script refused: %s.  Nothing was run." refusal)
      (cl-flet ((tool (name &rest plist)
                  (let ((superchat-script--calls calls))
                    (apply #'superchat-tool name plist)))
                ;; Returns nil, so a script ending in (out ...) does not
                ;; report the same value twice.
                (out (value)
                  (push (if (stringp value) value (format "%S" value)) output)
                  nil))
        (let* ((error-text nil)
               (value (condition-case err
                          ;; The current buffer a script switches to is put back.
                          (save-current-buffer
                            (eval `(cl-flet ((tool ,#'tool) (out ,#'out))
                                     ,(superchat-script--expand form))
                                  t))
                        (error (setq error-text (error-message-string err)) nil))))
          (when (and value (not error-text)) (out value))
          (superchat-script--clip
           (concat (if error-text "Script failed." "Script completed.")
                   (format " %d tool call%s.\n" (car calls) (if (= (car calls) 1) "" "s"))
                   (mapconcat #'identity (nreverse output) "\n")
                   (if error-text (format "\nScript error: %s" error-text) ""))))))))

(provide 'superchat-script)
;;; superchat-script.el ends here
