;;; tool-material-regression.el --- bounded tool argument/result text -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the accepted decision
;; docs/decisions/accepted/2026-09-23-the-next-turn-sees-previous-tool-results.md
;; (and its component contract, docs/components/superchat-tool-execution.md,
;; "Stored tool material"): ordinary tool events keep bounded argument and
;; result text in their payload; row content and full-text search stay free of
;; it; one option turns storage off.
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
;;     -l scripts/tool-material-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^tool-material/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Tool material regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-print-backtrace-right-margin 160
      ert-batch-backtrace-right-margin 160)

(defmacro tool-material--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun tool-material--settle (run)
  "Wait until exact RUN is terminal."
  (should (stringp run))
  (should (smoke--wait
           (lambda ()
             (and (smoke--run-done-p run)
                  (eq 'completed (plist-get (superchat-runtime-status run) :status))))
           20))
  run)

(defun tool-material--events (session type)
  "Return SESSION's replayed events of exactly TYPE."
  (cl-remove-if-not
   (lambda (event) (equal type (plist-get event :type)))
   (superchat-tape-replay-events session)))

(defun tool-material--payload (session type)
  "Return the payload of SESSION's first replayed TYPE event, or nil."
  (let ((event (car (tool-material--events session type))))
    (and event (plist-get event :payload))))

(defun tool-material--session (view)
  "Return VIEW's Session id."
  (buffer-local-value 'superchat--session-id view))

(defun tool-material--run (view text)
  "Send TEXT from VIEW and wait for the scripted Turn."
  (tool-material--settle (smoke--send view text)))

;;;; Success: arguments and result text are stored, the row summary is not

(ert-deftest tool-material/success-keeps-arguments-and-result ()
  "A successful call keeps its arguments and result text in the payload.
The row content and full-text search stay free of the result token."
  (let ((target (generate-new-buffer "tool-material-target")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TMTOKEN-7731\nrest of the buffer\n"))
          (let ((name (buffer-name target)))
            (tool-material--fixture
                (list `(:tool-uses (("read_buffer" (buffer . ,name))))
                      '(:text "done"))
              (let* ((view (smoke--open-chat))
                     (session (tool-material--session view)))
                (tool-material--run view "read the buffer")
                (let ((requested (tool-material--payload session "tool.requested"))
                      (completed (tool-material--payload session "tool.completed")))
                  (should (stringp (plist-get requested :arguments-text)))
                  (should (string-match-p
                           (regexp-quote name)
                           (plist-get requested :arguments-text)))
                  (should (stringp (plist-get completed :result-text)))
                  (should (string-match-p
                           "TMTOKEN-7731"
                           (plist-get completed :result-text)))
                  ;; The row `content' (and therefore full-text search) keeps
                  ;; only the summary.
                  (should-not
                   (cl-some (lambda (row)
                              (string-match-p "TMTOKEN" (or (nth 3 row) "")))
                            (superchat-view-entries session)))
                  (should-not (superchat-view-search "TMTOKEN" session)))))))
      (kill-buffer target))))

;;;; Failure: the refusal text the model got is stored

(ert-deftest tool-material/failure-keeps-the-refusal-text ()
  "A refused call keeps the delivered refusal text, status unchanged."
  (tool-material--fixture
      (list `(:tool-uses (("eval_elisp"
                           (expression . "(setq tool-material-probe 'ran)"))))
            '(:text "ok"))
    (let* ((superchat-approval-function (lambda (_request) nil))
           (view (smoke--open-chat))
           (session (tool-material--session view)))
      (tool-material--run view "try it")
      (let* ((payload (tool-material--payload session "tool.failed"))
             (text (plist-get payload :result-text)))
        (should (equal "cancelled" (plist-get payload :status)))
        (should (stringp text))
        (should (string-match-p "Tool call cancelled by user" text))
        ;; The model received exactly this refusal text in the follow-up.
        (should (string-match-p
                 (regexp-quote text)
                 (plist-get (car smoke--requests) :prompt-text)))))))

;;;; The bound and its marker

(ert-deftest tool-material/long-result-is-cut-with-its-length ()
  "A result longer than the bound is cut, flagged, and keeps its length."
  (let ((target (generate-new-buffer "tool-material-long")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert (make-string 300 ?x)))
          (let ((name (buffer-name target))
                (full (superchat-tool-read-buffer (buffer-name target))))
            (tool-material--fixture
                (list `(:tool-uses (("read_buffer" (buffer . ,name))))
                      '(:text "done"))
              (let ((superchat-tape-tool-material-max-chars 40))
                (let* ((view (smoke--open-chat))
                       (session (tool-material--session view)))
                  (tool-material--run view "read")
                  (let* ((payload (tool-material--payload session "tool.completed"))
                         (text (plist-get payload :result-text)))
                    (should (stringp text))
                    (should (= 40 (length text)))
                    (should (equal (substring full 0 40) text))
                    (should (eq t (plist-get payload :result-truncated)))
                    (should (= (length full) (plist-get payload :result-chars)))))))))
      (kill-buffer target))))

(ert-deftest tool-material/payload-never-omitted-for-the-new-text ()
  "Text that cannot fit is shortened until the payload is still stored.
The bound is larger than the payload without text, so an omission here would be
a defect of the fitter, not of the budget."
  (let ((target (generate-new-buffer "tool-material-bounded")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert (make-string 4000 ?y)))
          (let ((name (buffer-name target)))
            (tool-material--fixture
                (list `(:tool-uses (("read_buffer" (buffer . ,name))))
                      '(:text "done"))
              (let ((superchat-tape-tool-material-max-chars 100000)
                    (superchat-tape-max-inline-bytes 1024))
                (let* ((view (smoke--open-chat))
                       (session (tool-material--session view)))
                  (tool-material--run view "read")
                  (let ((payload (tool-material--payload session "tool.completed")))
                    (should-not (plist-get payload :omitted))
                    (should (equal "read_buffer" (plist-get payload :tool-name)))
                    (should (stringp (plist-get payload :execution-id)))
                    (should (eq t (plist-get payload :result-truncated)))
                    (should (<= (superchat-tape--encoded-payload-size payload)
                                1024))
                    ;; The bound really is enough without the text.
                    (let ((base (cl-remove-if
                                 (lambda (key)
                                   (memq key '(:result-text :result-truncated
                                               :result-chars)))
                                 payload)))
                      (should (< (superchat-tape--encoded-payload-size base)
                                 1024))))))))))
      (kill-buffer target)))

;;;; The off switch

(ert-deftest tool-material/off-switch-stores-nothing ()
  "With storage off, no tool event carries argument or result text."
  (let ((target (generate-new-buffer "tool-material-off")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "TMTOKEN-OFF\n"))
          (let ((name (buffer-name target)))
            (tool-material--fixture
                (list `(:tool-uses (("read_buffer" (buffer . ,name))))
                      '(:text "done"))
              (let ((superchat-tape-store-tool-material nil))
                (let* ((view (smoke--open-chat))
                       (session (tool-material--session view)))
                  (tool-material--run view "read")
                  (dolist (type '("tool.requested" "tool.completed" "tool.failed"))
                    (dolist (event (tool-material--events session type))
                      (let ((payload (plist-get event :payload)))
                        (should-not (plist-member payload :arguments-text))
                        (should-not (plist-member payload :result-text))
                        (should-not (plist-member payload :arguments-truncated))
                        (should-not (plist-member payload :result-truncated)))))))))))
      (kill-buffer target)))

;;;; The fitter itself

(ert-deftest tool-material/fit-is-pure ()
  "The fitter cuts, flags, halves and removes without side effects."
  ;; Cut to the configured bound, with the original length.
  (let ((superchat-tape-tool-material-max-chars 10))
    (let* ((original (list :tool-name "read_buffer" :execution-id "e1"
                           :arguments-text (make-string 100 ?a)))
           (fitted (superchat-tape-fit-tool-material original)))
      (should (= 10 (length (plist-get fitted :arguments-text))))
      (should (eq t (plist-get fitted :arguments-truncated)))
      (should (= 100 (plist-get fitted :arguments-chars)))
      ;; Purity: the input payload is untouched.
      (should (= 100 (length (plist-get original :arguments-text))))
      (should-not (plist-get original :arguments-truncated))))
  ;; A text within the bound is untouched and carries no flags.
  (let ((fitted (superchat-tape-fit-tool-material
                 (list :tool-name "t" :result-text "short"))))
    (should (equal "short" (plist-get fitted :result-text)))
    (should-not (plist-member fitted :result-truncated))
    (should-not (plist-member fitted :result-chars)))
  ;; Halving under the Tape's own byte bound.
  (let* ((superchat-tape-max-inline-bytes 200)
         (text (make-string 4000 ?b))
         (fitted (superchat-tape-fit-tool-material
                  (list :tool-name "t" :result-text text))))
    (should (< (length (plist-get fitted :result-text)) 4000))
    (should (eq t (plist-get fitted :result-truncated)))
    (should (= 4000 (plist-get fitted :result-chars)))
    (should (<= (superchat-tape--encoded-payload-size fitted) 200)))
  ;; Nothing fits: the text fields are removed and the rest stays.
  (let* ((superchat-tape-max-inline-bytes 1)
         (fitted (superchat-tape-fit-tool-material
                  (list :tool-name "t" :execution-id "e" :result-text "abc"))))
    (should-not (plist-member fitted :result-text))
    (should-not (plist-member fitted :result-truncated))
    (should-not (plist-member fitted :result-chars))
    (should (equal "t" (plist-get fitted :tool-name)))
    (should (equal "e" (plist-get fitted :execution-id)))))

(provide 'tool-material-regression)
;;; tool-material-regression.el ends here
