;;; borrowed-buffer-transcript-regression.el --- borrowed hosts get no tool transcript -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for M8 slice 1b: tool presentation renders only into a
;; chat View.  A Run hosted on a borrowed non-chat buffer (inline, Org Babel,
;; Magit) must not have the agent loop's `*** NAME' / `Tool result:' transcript
;; inserted into it; its tool calls are on the Tape and in the surface's own
;; presentation (the inline Sink).
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
;;     -l scripts/borrowed-buffer-transcript-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^borrowed-buffer/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Borrowed-buffer transcript tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar superchat-mode)
(defvar superchat--session-id)

(defmacro borrowed-buffer--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun borrowed-buffer--settle (run)
  "Wait until exact RUN is terminal."
  (should (stringp run))
  (should (smoke--wait
           (lambda ()
             (superchat-runtime--terminal-p
              (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

;;;; A borrowed host buffer

(ert-deftest borrowed-buffer/inline-ask-tool-call-leaves-the-source-buffer-untouched ()
  "An inline tool call is on the Tape and in the overlay, not in the source buffer."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (let ((target (buffer-name)))
      (borrowed-buffer--fixture
          (list `(:tool-uses (("read_buffer" (buffer . ,target))))
                '(:text "It says alpha."))
        (let* ((overlay (superchat-inline-ask "what is here"))
               (state (lambda () (overlay-get overlay 'superchat-inline))))
          (should (smoke--wait
                   (lambda () (plist-get (funcall state) :run-id))
                   10))
          (borrowed-buffer--settle (plist-get (funcall state) :run-id))
          (should (smoke--wait
                   (lambda ()
                     (eq 'ready (plist-get (funcall state) :status)))))
          (should (equal "alpha\n" (buffer-string)))
          (should (eq 'ready (plist-get (funcall state) :status))))))))

;;;; The chat View keeps its presentation

(ert-deftest borrowed-buffer/chat-view-still-renders-tool-calls ()
  "The chat View still gets the `Tool result:' transcript of its tool calls."
  (borrowed-buffer--fixture
      (list '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
            '(:text "second"))
    (let* ((view (smoke--open-chat))
           (run (borrowed-buffer--settle (smoke--send view "read it"))))
      (should (eq 'completed
                  (plist-get (superchat-runtime-status run) :status)))
      (should (smoke--wait
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "Tool result: read_buffer" (buffer-string))))
               10)))))

;;;; The predicate itself

(ert-deftest borrowed-buffer/render-buffer-returns-nil-for-a-plain-buffer ()
  "Only a chat View for the exact Session is a render target."
  (with-temp-buffer
    (should-not (superchat--agent-render-buffer (current-buffer)))
    (should-not (superchat--agent-render-buffer (current-buffer) "s1"))
    (setq-local superchat-mode t)
    (setq-local superchat--session-id "s1")
    (should (eq (current-buffer)
                (superchat--agent-render-buffer (current-buffer))))
    (should (eq (current-buffer)
                (superchat--agent-render-buffer (current-buffer) "s1")))
    (should-not (superchat--agent-render-buffer (current-buffer) "s2"))))

(provide 'borrowed-buffer-transcript-regression)
;;; borrowed-buffer-transcript-regression.el ends here
