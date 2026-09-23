;;; inline-act-regression.el --- M8 inline-act regression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for M8 slice 1: the `inline-act' row, its entry point,
;; the envelope it submits, and in-place progress in the overlay header.
;; A tool Turn started at point edits the source buffer and reports in the
;; overlay; the chat window is never opened.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets, and needs no scratch
;; files.  All model calls are scripted; no user configuration is evaluated.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/inline-act-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^inline-act/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Inline act regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro inline-act--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun inline-act--state (overlay)
  "Return OVERLAY's inline state plist."
  (overlay-get overlay 'superchat-inline))

(defun inline-act--settle (overlay)
  "Wait until OVERLAY's Run is terminal; return its run id."
  (should (smoke--wait
           (lambda () (plist-get (inline-act--state overlay) :run-id))
           10))
  (let ((run (plist-get (inline-act--state overlay) :run-id)))
    (should (smoke--wait
             (lambda ()
               (superchat-runtime--terminal-p
                (plist-get (superchat-runtime-status run) :status)))
             20))
    run))

(defun inline-act--edit-round (buffer-name old new)
  "A script step calling `edit_buffer' on BUFFER-NAME replacing OLD by NEW."
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . ,old) (new_string . ,new)))))

(defun inline-act--edit-script (buffer-name)
  "A script: edit BUFFER-NAME, then answer text."
  (list (inline-act--edit-round buffer-name "alpha" "beta")
        '(:text "Done.")))

(defun inline-act--request-tools ()
  "Tool names of the chronologically first provider request of the fixture."
  (plist-get (car (last smoke--requests)) :tools))

;;;; Row and surface

(ert-deftest inline-act/row/is-in-default-profile-and-registers-surface ()
  "The default profile resolves the `inline-act' row and the row owns its surface."
  (should (memq 'inline-act (superchat-profile-rows)))
  (should (memq 'inline-act superchat-pipeline--surfaces)))

(ert-deftest inline-act/row/entry-point-refuses-without-row ()
  "Without its row, the entry point refuses and submits nothing."
  (inline-act--fixture nil
    (let ((mounted (superchat-plugin-row-active-p 'inline-act)))
      (unwind-protect
          (progn
            (superchat-plugin-unmount 'inline-act)
            (with-temp-buffer
              (insert "some text")
              (let ((refusal (error-message-string
                              (should-error (superchat-inline-act "hello")))))
                (should (string-match-p "inline-act" refusal))
                (should (string-match-p "profile" refusal))))
            (should-not smoke--requests))
        (when mounted (superchat-plugin-mount 'inline-act))))))

;;;; A tool Turn from the source buffer

(ert-deftest inline-act/run/edit-lands-in-the-buffer-and-overlay-is-ready ()
  "A scripted tool Turn edits the source buffer and reports in the overlay.
No chat buffer is created: the whole Run stays in the source buffer."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture (inline-act--edit-script target)
        (let* ((overlay (superchat-inline-act "replace alpha with beta")))
          (inline-act--settle overlay)
          (should (smoke--wait
                   (lambda ()
                     (eq 'ready (plist-get (inline-act--state overlay) :status)))))
          (should (equal "beta\n" (buffer-string)))
          (should (eq 'ready (plist-get (inline-act--state overlay) :status)))
          ;; Slice 2 appends the Run footer after the answer; the exact footer
          ;; is asserted by `inline-act/footer/...' below.
          (should (string-prefix-p "Done." (plist-get (inline-act--state overlay) :text)))
          (should-not (get-buffer superchat-buffer-name)))))))

(ert-deftest inline-act/run/request-carries-the-default-tool-baseline ()
  "The Run's provider request offers the default baseline, not inline-ask's four."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture (inline-act--edit-script target)
        (let ((overlay (superchat-inline-act "replace alpha with beta")))
          (inline-act--settle overlay)
          (let ((tools (inline-act--request-tools)))
            (should (member "edit_buffer" tools))
            (should (member "read_buffer" tools))))))))

(ert-deftest inline-act/run/status-header-shows-working-during-a-tool-call ()
  "The live overlay says Working inside the actual edit tool body."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture (inline-act--edit-script target)
        (let ((overlay nil)
              (during nil)
              (original (symbol-function 'superchat-tool-edit-buffer)))
          (cl-letf (((symbol-function 'superchat-tool-edit-buffer)
                     (lambda (&rest args)
                       (push (overlay-get overlay 'after-string) during)
                       (apply original args))))
            (setq overlay (superchat-inline-act "replace alpha with beta"))
            (inline-act--settle overlay)
            (should (equal 1 (length during)))
            (should (string-match-p "Working: tool" (car during)))
            (should (string-match-p "Ready" (overlay-get overlay 'after-string)))
            (should (eq 'ready (plist-get (inline-act--state overlay) :status)))))))))

;;;; The Run footer

(ert-deftest inline-act/footer/counts-tool-calls-and-names-unsaved-buffers ()
  "The overlay footer counts the Run's tool calls and names its unsaved edits."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture (inline-act--edit-script target)
        (let* ((overlay (superchat-inline-act "replace alpha with beta")))
          (inline-act--settle overlay)
          (should (smoke--wait
                   (lambda ()
                     (eq 'ready (plist-get (inline-act--state overlay) :status)))))
          (let* ((state (inline-act--state overlay))
                 (text (plist-get state :text))
                 (footer (superchat-inline-act--footer
                          (plist-get state :session-id)
                          (plist-get state :run-id))))
            (should (string-prefix-p "Done." text))
            (should (string-match-p "1 tool call" text))
            (should (string-match-p "unsaved" text))
            (should (string-match-p (regexp-quote target) footer))))))))

(ert-deftest inline-act/footer/is-empty-without-tool-calls ()
  "A Run with no tool calls and no edits has nothing to append."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (inline-act--fixture '((:text "Nothing to do."))
      (let* ((overlay (superchat-inline-act "just answer"))
             (state (lambda () (overlay-get overlay 'superchat-inline))))
        (inline-act--settle overlay)
        (should (smoke--wait
                 (lambda ()
                   (eq 'ready (plist-get (funcall state) :status)))))
        (should (equal "Nothing to do." (plist-get (funcall state) :text)))))))

;;;; The `d' and `r' keys

(ert-deftest inline-act/keys/d-opens-the-diff-of-the-edited-buffer ()
  "`d' shows the diff of the buffer the Run edited."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture (inline-act--edit-script target)
        (let ((overlay (superchat-inline-act "replace alpha with beta"))
              (called nil))
          (inline-act--settle overlay)
          (cl-letf (((symbol-function 'superchat-changes-diff)
                     (lambda (buffer) (setq called buffer))))
            (superchat-inline-act-diff overlay))
          (should (eq called (get-buffer target))))))))

(ert-deftest inline-act/keys/reply-resubmits-with-the-inline-act-surface ()
  "The `r' reply stays on the inline-act surface in the same Session."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (inline-act--fixture
          (append (inline-act--edit-script target) '((:text "Second.")))
        (let ((overlay (superchat-inline-act "replace alpha with beta")))
          (inline-act--settle overlay)
          (should (smoke--wait
                   (lambda ()
                     (eq 'ready (plist-get (inline-act--state overlay) :status)))))
          (superchat-inline-act-reply overlay "and now?")
          (inline-act--settle overlay)
          (should (smoke--wait
                   (lambda ()
                     (and (eq 'ready
                              (plist-get (inline-act--state overlay) :status))
                          (string-prefix-p
                           "Second."
                           (plist-get (inline-act--state overlay) :text))))))
          (let* ((session (plist-get (inline-act--state overlay) :session-id))
                 (intents (cl-remove-if-not
                           (lambda (event)
                             (equal "intent.received" (plist-get event :type)))
                           (superchat-tape-replay-events session))))
            (should (= 2 (length intents)))
            (should (equal "inline-act"
                           (plist-get (plist-get (car (last intents)) :payload)
                                      :surface)))))))))

;;;; Approval denial and cancellation

(defvar inline-act--probe nil
  "Bound to nil by a test; set to a non-nil value only if its expression runs.")

(ert-deftest inline-act/approval/denied-ask-tool-reaches-the-seam-and-does-not-run ()
  "A denied ask-class tool records the approval pair and its body never runs."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((inline-act--probe nil))
      (inline-act--fixture
          (list `(:tool-uses (("eval_elisp"
                               (expression . "(setq inline-act--probe 'ran)"))))
                '(:text "Understood."))
        (let* ((superchat-approval-function (lambda (_request) nil))
               (overlay (superchat-inline-act "try to evaluate it")))
          (should (smoke--wait
                   (lambda () (plist-get (inline-act--state overlay) :run-id))
                   10))
          (let ((run (inline-act--settle overlay)))
            (should (smoke--wait
                     (lambda ()
                       (eq 'ready (plist-get (inline-act--state overlay) :status)))))
            ;; The denied body never ran.
            (should-not inline-act--probe)
            ;; The source buffer was not touched.
            (should (equal "alpha\n" (buffer-string)))
            (let* ((session (plist-get (inline-act--state overlay) :session-id))
                   (events (cl-remove-if-not
                            (lambda (event) (equal run (plist-get event :run-id)))
                            (superchat-tape-replay-events session)))
                   (types (mapcar (lambda (event) (plist-get event :type)) events))
                   (requested (cl-position "approval.requested" types :test #'equal))
                   (decided (cl-position "approval.decided" types :test #'equal)))
              (should requested)
              (should decided)
              (should (< requested decided))
              ;; The refusal is recorded as a failed call of the exact tool.
              (let ((failed (cl-find-if
                             (lambda (event)
                               (and (equal "tool.failed" (plist-get event :type))
                                    (equal "eval_elisp"
                                           (plist-get (plist-get event :payload)
                                                      :tool-name))))
                             events)))
                (should failed)
                ;; The ask path refuses as "cancelled" (`rejected' outcome).
                (should (equal "cancelled"
                               (plist-get (plist-get failed :payload) :status))))))
          ;; `eval_elisp' is full Emacs control; the level asks before it runs.
          (should (eq 'full-control (superchat-sandbox-tool-class "eval_elisp")))
          (should (string-prefix-p "Understood."
                                   (plist-get (inline-act--state overlay) :text))))))))

(ert-deftest inline-act/keys/q-cancels-the-run-and-removes-the-overlay ()
  "`q' (via `superchat-inline-clear') cancels the Run and removes the overlay."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (inline-act--fixture '((:hang t))
      (let* ((overlay (superchat-inline-act "never answers"))
             (state (lambda () (overlay-get overlay 'superchat-inline))))
        (should (smoke--wait (lambda () (plist-get (funcall state) :run-id)) 10))
        (let* ((run (plist-get (funcall state) :run-id))
               (session (plist-get (funcall state) :session-id)))
          (superchat-inline-clear overlay)
          (should-not (overlay-buffer overlay))
          (should (smoke--wait
                   (lambda ()
                     (let ((status (plist-get (superchat-runtime-status run) :status)))
                       (and status (superchat-runtime--terminal-p status))))
                   20))
          (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
          (should (smoke--wait
                   (lambda ()
                     (cl-find-if
                      (lambda (event)
                        (and (equal "run.cancelled" (plist-get event :type))
                             (equal run (plist-get event :run-id))))
                      (superchat-tape-replay-events session)))
                   5)))))))

;;;; The menu

(ert-deftest inline-act/keys/menu-offers-diff-and-replies-on-inline-act ()
  "`M-RET' and a click on an inline act overlay open the inline act menu.
Its reply stays on the `inline-act' surface, so tools remain available, and it
offers the diff.  The inline-ask menu it used to inherit replied read-only and
had no diff."
  (should (eq #'superchat-inline-act-menu
              (lookup-key superchat-inline-act--keymap (kbd "M-RET"))))
  (should (eq #'superchat-inline-act-menu
              (lookup-key superchat-inline-act--keymap [mouse-1])))
  (let (called)
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) '(?r "reply")))
              ((symbol-function 'superchat-inline-act-reply)
               (lambda (&rest _) (push 'act-reply called)))
              ((symbol-function 'superchat-inline-reply)
               (lambda (&rest _) (push 'ask-reply called))))
      (superchat-inline-act-menu 'an-overlay))
    (should (equal '(act-reply) called)))
  (let (called)
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) '(?d "diff")))
              ((symbol-function 'superchat-inline-act-diff)
               (lambda (&rest _) (push 'diff called))))
      (superchat-inline-act-menu 'an-overlay))
    (should (equal '(diff) called))))

(ert-deftest inline-act/keys/w-copies-on-both-overlays ()
  "`w' copies the answer on an inline ask overlay and, through the parent
keymap, on an inline act overlay, as both READMEs say."
  (should (eq #'superchat-inline-copy
              (lookup-key superchat-inline--keymap (kbd "w"))))
  (should (eq #'superchat-inline-copy
              (lookup-key superchat-inline-act--keymap (kbd "w")))))

(provide 'inline-act-regression)
;;; inline-act-regression.el ends here
