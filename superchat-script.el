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

(defconst superchat-script-not-callable
  '("elisp_script" "ask_user" "delegate_to_subagent" "delegate_to_subagent_parallel")
  "Tools a script cannot call: itself, and tools that wait for an answer.")

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
        (push (read (current-buffer)) forms))
      (cons 'progn (nreverse forms)))))

(defun superchat-script-check (form)
  "Return why FORM may not run, as a string, or nil when it may."
  (let ((forbidden (superchat-script--forbidden-in form))
        (unsafe (let ((safe-functions superchat-script-entries))
                  (unsafep form))))
    (cond (forbidden (format "`%s' is not allowed in a script" forbidden))
          ((and (consp unsafe) (eq (car unsafe) 'global-variable))
           (format "setting the variable `%s' is not allowed in a script"
                   (cadr unsafe)))
          ((and (consp unsafe) (eq (car unsafe) 'function))
           (format "`%s' is not allowed in a script" (cadr unsafe)))
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

(defun superchat-script--callable-tool (name)
  "The llm.el tool NAME a script may call, or signal why not."
  (when (member name superchat-script-not-callable)
    (error "%s cannot be called from a script" name))
  (let ((tool (cl-find name (superchat-script--callable-tools)
                       :key #'llm-tool-name :test #'equal)))
    (cond ((null tool) (error "No tool named %s" name))
          ((llm-tool-async tool) (error "%s cannot be called from a script" name))
          (t tool))))

(defun superchat-script--arguments (tool plist)
  "TOOL's positional arguments from the keyword PLIST a script gave."
  (let ((names (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool))))
    (cl-loop for (key _value) on plist by #'cddr
             unless (and (keywordp key)
                         (member (substring (symbol-name key) 1) names))
             do (error "%s has no argument %s; its arguments are %s"
                       (llm-tool-name tool) key
                       (mapconcat (lambda (name) (concat ":" name)) names " ")))
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

(defun superchat-script-run (code)
  "Run the script CODE and return what reaches the model.
The result starts with \"Script completed.\", \"Script failed.\" or
\"Script refused:\".  A failed script keeps the output it made before the
error; calls it made are not undone."
  (let* ((parent superchat--tool-execution-context)
         (parent-id (or (plist-get parent :execution-id) "script"))
         (count 0)
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
                  (unless (stringp name) (error "A tool name must be a string"))
                  (when (>= count superchat-script-max-calls)
                    (error "More than %d tool calls in one script"
                           superchat-script-max-calls))
                  (let* ((target (superchat-script--callable-tool name))
                         (args (superchat-script--arguments target plist))
                         (context (plist-put (copy-sequence parent) :execution-id
                                             (format "%s/%d" parent-id (cl-incf count)))))
                    (superchat-tool-execute name (llm-tool-function target) args
                                            :context context)))
                ;; Returns nil, so a script ending in (out ...) does not
                ;; report the same value twice.
                (out (value)
                  (push (if (stringp value) value (format "%S" value)) output)
                  nil))
        (let* ((error-text nil)
               (value (condition-case err
                          (eval `(cl-flet ((tool ,#'tool) (out ,#'out)) ,form) t)
                        (error (setq error-text (error-message-string err)) nil))))
          (when (and value (not error-text)) (out value))
          (superchat-script--clip
           (concat (if error-text "Script failed." "Script completed.")
                   (format " %d tool call%s.\n" count (if (= count 1) "" "s"))
                   (mapconcat #'identity (nreverse output) "\n")
                   (if error-text (format "\nScript error: %s" error-text) ""))))))))

(provide 'superchat-script)
;;; superchat-script.el ends here
