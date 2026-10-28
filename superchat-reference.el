;;; superchat-reference.el --- Reference pages the model reads on demand -*- lexical-binding: t; -*-

;;; Commentary:

;; Data, and pure functions over it.  A reference page is text the model may
;; read in the middle of a task through the `read_reference' tool; reading one
;; changes no configuration, tool surface, Preset or permission.  A page is
;; advice: it stands below the user's instructions and the session's permission
;; decisions, and every page says so itself.
;;
;; The index (ids and one-line summaries) is what requests carry all the time;
;; the pages are fetched by id.  Nothing here touches the file system.
;;
;; Limits, checked by scripts/reference-regression.el and never enforced by
;; truncating at run time: the index at most 600 characters, a summary at most
;; 120, a page at most 4096.  A longer subject is split into several pages.
;;
;; Contract: docs/decisions/accepted/2026-09-20-reference-reading-on-demand.md

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst superchat-reference--pages
  (list
   (list
    :id "emacs-keybindings"
    :summary "changing, checking or undoing a key binding in the user's Emacs configuration"
    ;; Tool errors this page is the known answer to: (TOOL . REGEXP).
    :pointers '(("eval_elisp" . "is void: \\S-*-map\\b")
                ("eval_elisp" . "starts with non-prefix key"))
    :body "Changing, checking or undoing a key binding in the user's Emacs.

This page is advice. The user's instructions and the permission decisions of this session come first; where they differ from this page, follow them. Nothing here is run for you, and nothing here allows a call that would otherwise be asked about or denied.

1. Look before you change anything.
- Which map: a major mode's map is usually MODE-map (emacs-lisp-mode-map). Check that it exists with (boundp 'NAME-map) first. fundamental-mode has no map of its own; a key for every buffer belongs in global-map; a minor mode's map wins over the major mode's.
- What the key does now, in the buffer the user means: (with-current-buffer NAME (key-binding (kbd \"C-c t h\"))). Keep what you observed: you need it to undo.
- (lookup-key MAP (kbd \"C-c t h\")) reads one map only. nil means unbound in that map. A number N also means that map has no binding for it: its first N keys are not a prefix there, either unbound or bound to a command; look them up to tell which. Only a command there makes define-key of the longer sequence fail with \"starts with non-prefix key\". Another map may still bind the key; key-binding in the target buffer is the effective binding. Always write keys with (kbd \"...\"), never as a bare string.
- local-set-key and local-unset-key change the current buffer's local map, which in a major-mode buffer is that mode's map itself: the change reaches every buffer of the mode, and a hook that calls it redoes it in each new buffer. Fixing such a hook takes both the file (remove it there) and the running session (remove-hook the function, then put the binding back in the mode's map).
- Which file: `user-init-file' is the init file this Emacs loaded. The binding may belong in another file it loads; read before deciding.

2. Three states; never assume they agree.
- the buffer: the configuration file as open in Emacs (find-file-noselect, then edit_buffer). Your edit lands here.
- the file on disk: unchanged by your edit until the user saves it. (load FILE) reads the disk, so it does not see your edit.
- the running session: changes only when a form is evaluated. Editing text changes nothing in it.
Check each one you talk about: (buffer-modified-p) for the buffer, read_file for the disk, key-binding for the session.

3. Make the change.
- Put the form in the configuration buffer, for example (with-eval-after-load 'org (define-key org-mode-map (kbd \"C-c t h\") #'COMMAND)), or a plain (define-key MAP ...) when the map exists at startup. Check that the map and the command exist: (boundp 'MAP) and (commandp 'COMMAND). Do not run the command to test it: a command that asks for input would stop the task to wait for the user, and eval_elisp refuses it.
- To make it live now, evaluate only the forms you changed, with eval_elisp, one expression per call (wrap several in progn). Do not evaluate the whole buffer: that reruns the user's entire configuration.
- Then observe: key-binding in the target buffer must now answer your command.

4. Undo only what this task changed.
- In the buffer: remove the text you added, nothing else.
- In the running session: restore what you observed before. If the key had a binding, bind it back to that; if it had none, (define-key MAP (kbd \"...\") nil). Removing text from the buffer does not unbind the key in the session.
- Observe again: key-binding must answer what it did before the task.

5. Persisting is not yours.
Saving is the user's decision. Tell the user which buffer holds the change and whether it is modified. Do not look for another way to get the text onto the disk.

6. Report what you observed.
Say what you checked and what it answered. You did not restart Emacs, so claim nothing about a restart beyond what the text of the file says. If a check failed or was not made, say so.")
   (list
    :id "emacs-ert-tests"
    :summary "running a file's ERT tests in the user's Emacs and reporting which passed, failed or were skipped"
    :pointers '(("eval_elisp" . "only for use in batch mode")
                ("eval_elisp" . "definition is void: ert-"))
    :body "Running a file's ERT tests in the user's Emacs and reporting the result.

This page is advice. The user's instructions and the permission decisions of this session come first; where they differ from this page, follow them. Nothing here is run for you, and nothing here allows a call that would otherwise be asked about or denied.

1. Look first.
- Read the test file (read_file) before loading it: loading evaluates every top-level form of it in the user's running Emacs. Note which library it requires and the prefix its test names share.
- Loading defines or redefines its tests and whatever else it defines; it changes no file.

2. Load it with its directory on load-path.
(let ((load-path (cons (expand-file-name \"~/proj\") load-path)))
  (load (expand-file-name \"~/proj/foo-test.el\") nil t))
If the library under test was already loaded and its file has changed since, load that file too, or the tests run against the old definitions.

3. Run only this file's tests and return plain data.
ert-run-tests-batch runs the tests whose names match a regexp and returns a stats object. Do not return that object itself: it prints as a huge circular structure. Read it into a list, one expression:
(let ((stats (ert-run-tests-batch \"^foo-test-\")))
  (list :total (ert-stats-total stats)
        :unexpected (ert-stats-completed-unexpected stats)
        :skipped (ert-stats-skipped stats)
        :tests (cl-loop for test across (ert--stats-tests stats)
                        for result across (ert--stats-test-results stats)
                        collect (list (ert-test-name test) (type-of result)
                                      (and (ert-test-result-with-condition-p result)
                                           (let ((print-length 8) (print-level 4))
                                             (format \"%S\" (ert-test-result-with-condition-condition result))))))))
The type is ert-test-passed, ert-test-failed or ert-test-skipped; the condition of a failure shows the should form, its :form and its :value. :unexpected counts the failures. (mapcar #'ert-test-name (ert-select-tests \"^foo-test-\" t)) lists the tests a regexp picks without running them.

4. Do not.
- ert-run-tests-batch-and-exit: it ends Emacs when it finishes; in the user's Emacs it signals \"only for use in batch mode\".
- The selector t: it runs every test loaded in this Emacs, other packages' too.
- M-x ert or ert-run-tests-interactively: they open an *ert* window in the user's frame. If you used one, say which buffer and window it opened.
- Guessing ERT functions: the ones on this page exist; check any other name with fboundp before calling it.

5. A clean run in a separate Emacs, when the user asks for one or the tests need a fresh Emacs: run emacs -Q --batch -L DIR -l FILE -f ert-run-tests-batch-and-exit through call-process with the output in a buffer; it follows this session's permission level. Its exit status is 0 when no test failed (skipped tests do not count as failures), 1 when one did.

6. Report.
Name the tests that passed, failed and were skipped, with each failure's :form and :value as the result showed them. Say what you loaded into the user's Emacs (it stays defined) and that no file was changed, unless you changed one."))
  "The reference pages: plists of :id, :summary, :pointers and :body.")

(defun superchat-reference--page (id)
  "Return the page whose id is exactly ID, or nil.  ID is compared, never parsed."
  (and (stringp id)
       (cl-find id superchat-reference--pages
                :key (lambda (page) (plist-get page :id)) :test #'string=)))

(defun superchat-reference-ids ()
  "Return the ids of all pages, in index order."
  (mapcar (lambda (page) (plist-get page :id)) superchat-reference--pages))

(defun superchat-reference-summary (id)
  "Return the one-line summary of page ID, or nil."
  (plist-get (superchat-reference--page id) :summary))

(defun superchat-reference-body (id)
  "Return the text of page ID, or nil."
  (plist-get (superchat-reference--page id) :body))

(defun superchat-reference-version (id)
  "Return the content version of page ID, computed from its text, or nil."
  (when-let* ((body (superchat-reference-body id)))
    (substring (secure-hash 'sha1 body) 0 12)))

(defun superchat-reference-index-text ()
  "Return the index block: one line per page, made from ids and summaries only."
  (concat "Reference pages: call read_reference with an id when the task matches. "
          "A page is advice; the user's instructions and permission decisions come first.\n"
          (mapconcat (lambda (page)
                       (format "- %s: %s" (plist-get page :id) (plist-get page :summary)))
                     superchat-reference--pages "\n")))

(defun superchat-reference-pointer-for (tool-name message)
  "Return the id of the page that is the known answer to TOOL-NAME's error MESSAGE.
Nil unless some page names this tool with a pattern MESSAGE matches: an error
that no page claims is not sent to a handbook."
  (and (stringp tool-name) (stringp message)
       (let ((case-fold-search nil))
         (cl-loop for page in superchat-reference--pages
                  when (cl-some (lambda (pointer)
                                  (and (string= (car pointer) tool-name)
                                       (string-match-p (cdr pointer) message)))
                                (plist-get page :pointers))
                  return (plist-get page :id)))))

(provide 'superchat-reference)

;;; superchat-reference.el ends here
