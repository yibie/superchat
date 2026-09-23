;;; context-header-regression.el --- Context v0 recall disclosure -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the H1 amendment to
;; docs/design/context-continuity-contract-v0.md: the rendered `Context v0'
;; header says `recall available' if and only if `:disclosure :recallable' is
;; non-empty AND the request offers the model a tool that performs raw recall.
;; Contract v0 offers no such tool, so the header says `recall none' while
;; `:disclosure :recallable' keeps listing every recallable selector.
;;
;; This standalone suite reuses canonical smoke helpers as a library.  It
;; neither loads nor modifies the leader-owned M3/M5 sets.  All model calls
;; are scripted; no user configuration is evaluated.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/context-header-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^context-header/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Context header regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defconst context-header--answer
  (concat "\nCONTEXT_HEADER_FULL_START\n" (make-string 3 96)
          "elisp\n(setq-local fill-column 72)\n" (make-string 3 96) "\n"
          (make-string 900 ?甲)
          "\nCONTEXT_HEADER_TAIL: the answer is long enough to be omitted.\n\n")
  "Raw answer long enough to be omitted under a small `:max-chars'.")

(defmacro context-header--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-context-max-chars 6000)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil))
       (smoke--with-fixture ,script ,@body))))

(defun context-header--settle (run)
  "Wait for RUN's completion and its durable Turn seal."
  (should (stringp run))
  (should (smoke--wait
           (lambda ()
             (and (smoke--run-done-p run)
                  (eq 'completed (plist-get (superchat-runtime-status run) :status))))
           15))
  (let ((session (plist-get (superchat-runtime-status run) :session-id)))
    (should (smoke--wait
             (lambda ()
               (cl-find-if
                (lambda (event)
                  (and (equal "turn.completed" (plist-get event :type))
                       (equal run (plist-get event :run-id))))
                (superchat-tape-replay-events session)))
             3)))
  run)

(defun context-header--recallable (envelope)
  "Return ENVELOPE's recallable selectors as a list."
  (append (plist-get (plist-get envelope :disclosure) :recallable) nil))

;;;; Recall is advertised only when the model can perform it

(ert-deftest context-header/recall-is-not-advertised-to-the-model ()
  "With recallable omissions and no recall tool, the header says `recall none'.
`:disclosure :recallable' still lists the selectors for callers that can act."
  (context-header--fixture (list (list :text context-header--answer))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (context-header--settle (smoke--send view "CONTEXT_HEADER_Q"))
      (let* ((envelope
              (superchat-view-assemble session :max-entries 50 :max-chars 100))
             (prompt (plist-get envelope :prompt)))
        (should (eq 'partial (plist-get envelope :status)))
        (should (> (length (context-header--recallable envelope)) 0))
        (should (string-match-p "| recall none\\]" prompt))
        (should-not (string-match-p "recall available" prompt))))))

(ert-deftest context-header/recall-is-advertised-only-when-the-model-can-recall ()
  "The same omitted Session advertises recall when the request can recall."
  (context-header--fixture (list (list :text context-header--answer))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (context-header--settle (smoke--send view "CONTEXT_HEADER_Q"))
      (let* ((superchat-view-model-can-recall t)
             (envelope
              (superchat-view-assemble session :max-entries 50 :max-chars 100))
             (prompt (plist-get envelope :prompt)))
        (should (> (length (context-header--recallable envelope)) 0))
        (should (string-match-p "recall available" prompt))))))

(ert-deftest context-header/no-omission-still-says-none ()
  "Nothing omitted: `recall none', and the flag alone cannot advertise."
  (context-header--fixture nil
    (let* ((session "context-header-plain")
           (text "A short raw assistant row.")
           (meta (superchat-tape--encode-meta
                  (list :event_type "response.completed" :event_id "ch-plain-event"
                        :schema 1 :session_id session
                        :run_id "ch-plain-run" :turn_id "ch-plain-turn"
                        :payload (superchat-runtime--content-metadata text)))))
      (superchat-db-tape-append session "assistant" text :meta meta)
      (let ((envelope
             (superchat-view-assemble session :max-entries 50 :max-chars 6000)))
        (should (eq 'satisfied (plist-get envelope :status)))
        (should (= 0 (length (context-header--recallable envelope))))
        (should (string-match-p "| recall none\\]" (plist-get envelope :prompt)))
        (should-not (string-match-p "recall available" (plist-get envelope :prompt))))
      ;; A recall tool with nothing omitted is still not an advertisement.
      (let* ((superchat-view-model-can-recall t)
             (envelope
              (superchat-view-assemble session :max-entries 50 :max-chars 6000)))
        (should (string-match-p "| recall none\\]" (plist-get envelope :prompt)))
        (should-not (string-match-p "recall available"
                                    (plist-get envelope :prompt)))))))

(provide 'context-header-regression)
;;; context-header-regression.el ends here
