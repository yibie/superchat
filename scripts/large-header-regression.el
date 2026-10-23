;;; large-header-regression.el --- a large request header is kept whole -*- lexical-binding: t; -*-

;;; Commentary:
;; G16 of the llm-pi-ai field test (2026-10-22 19:27): once the system text
;; and tool schemas grew past `superchat-tape-max-inline-bytes', the Tape
;; replaced each `request.header' payload with its size and hash.  The
;; replacement has no `:header-digest', so the next request found no header
;; to reuse and wrote another, also replaced, and `prompt-reconstructable'
;; failed at every step.  A header is written once per series, so it is now
;; kept whole whatever its size.  The bound on `request.message-list'
;; payloads is the open roadmap observation of 2026-10-11 and stays.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/large-header-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^large-header/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro lh--fixture (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-tools-list nil))
       (smoke--with-fixture '((:text "LH-DONE")) ,@body))))

(defun lh--headers (session)
  (cl-remove-if-not (lambda (event) (equal "request.header" (plist-get event :type)))
                    (superchat-tape-replay-events session)))

(defconst lh--system (make-string (* 2 superchat-tape-max-inline-bytes) ?s)
  "A system text twice the inline bound.")

(defun lh--record (session request-id)
  (superchat-tape-request-record session request-id lh--system [] "model" nil nil))

(ert-deftest large-header/a-large-header-is-written-once ()
  (lh--fixture
    (let ((session (buffer-local-value 'superchat--session-id (smoke--open-chat))))
      (lh--record session "LH-1")
      (lh--record session "LH-2")
      (lh--record session "LH-3")
      (should (= 1 (length (lh--headers session)))))))

(ert-deftest large-header/a-large-header-keeps-its-text ()
  (lh--fixture
    (let ((session (buffer-local-value 'superchat--session-id (smoke--open-chat))))
      (lh--record session "LH-1")
      (let ((payload (plist-get (car (lh--headers session)) :payload)))
        (should-not (plist-get payload :omitted))
        (should (equal lh--system (plist-get payload :system)))
        (should (plist-get payload :header-digest))))))

(ert-deftest large-header/a-request-under-a-large-header-can-be-rebuilt ()
  (lh--fixture
    (let ((session (buffer-local-value 'superchat--session-id (smoke--open-chat))))
      (lh--record session "LH-1")
      (lh--record session "LH-2")
      (let ((evidence (superchat-tape-request-evidence session "LH-2")))
        (should (eq 'reproduced (plist-get evidence :status)))
        (should (equal lh--system (plist-get evidence :system)))))))

(ert-deftest large-header/other-large-payloads-are-still-bounded ()
  "Guard: the bound holds for every event that does not ask to keep its payload."
  (should (plist-get (superchat-tape--encoded-payload (list :text lh--system))
                     :omitted)))

(provide 'large-header-regression)

;;; large-header-regression.el ends here
