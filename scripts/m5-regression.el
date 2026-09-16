;;; m5-regression.el --- operating context and reference reading regression set -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; OWNERSHIP -- read before editing.  Like `scripts/m3-regression.el', this set
;; belongs to the leader agent alone.  Only the leader may change this file or
;; the breakage scripts in `.scratch/m5-verification/mutants/'.  Every other
;; agent or session, including an implementer fixing a defect a test found, may
;; run and read it but must not edit it: report the test you believe is wrong,
;; or the known-failure marker your work should flip, to the leader with
;; evidence.
;;
;; The user ruled on 2026-09-16 that new tests are not added to
;; `scripts/m3-regression.el' any more and get their own harness instead.  This
;; file therefore builds its own fixture directly on the canonical smoke
;; helpers rather than loading the M3 set -- loading that set would drag its
;; 139 tests in with it and the split would be cosmetic.  The M3 set keeps M3,
;; M3a and M4; this one starts at the Emacs operating-context contract.
;;
;; The authority is
;; `docs/decisions/proposed/2026-09-16-emacs-operating-context-and-reference-contract.md'
;; (slices 1 and 2 accepted 2026-09-16; slices 3 and 4 still proposed).  The
;; canonical smoke set stays the byte-for-byte event-sequence gate; this file
;; asserts what the model actually receives: which tools a request offers, and
;; which facts about the user's Emacs reach it.
;;
;;   m5/discovery/  the default tool surface and what narrows it
;;   m5/origin/     the live Emacs facts a request carries, and where they sit
;;
;; A test marked `:expected-result :failed' pins a gap between the tree and the
;; contract; each names the gap so a red run reports it rather than a bare
;; void-function error.  The leader clears the marker when the slice lands.
;;
;; Run:
;;   emacs -Q --batch <load-path args> -l scripts/m5-regression.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Scope objects are cyclic (parent <-> children).  An ERT failure report that
;; prints one without `print-circle' dies with "Apparently circular structure".
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; Harness -------------------------------------------------------------------

(defmacro m5r-fixture (script &rest body)
  "Run BODY in a fresh scripted smoke fixture that answers with SCRIPT.
Adds the process-local state `smoke--with-fixture' does not reset: the
follow-up FIFO, its dispatch timers and the fault ring."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil))
     (smoke--with-fixture ,script ,@body)))

(defun m5r-status (run-id)
  "Public status of RUN-ID."
  (plist-get (superchat-runtime-status run-id) :status))

(defun m5r-settle (run-id &optional seconds)
  "Wait until RUN-ID is terminal and its Turn seal is on the Tape."
  (should (smoke--wait (lambda ()
                         (and (smoke--run-done-p run-id)
                              (superchat-runtime--terminal-p (m5r-status run-id))))
                       (or seconds 30)))
  ;; Status turns terminal before the seal finishes appending.
  (smoke--wait (lambda ()
                 (let ((session (plist-get (superchat-runtime-status run-id)
                                           :session-id)))
                   (m5r-of-type "turn.completed" (m5r-events session))))
               3)
  run-id)

(defun m5r-events (session)
  "SESSION's Tape rows."
  (and session (superchat-tape-replay-events session)))

(defun m5r-of-type (type events)
  "The EVENTS whose type is TYPE."
  (cl-remove-if-not (lambda (e) (equal type (format "%s" (plist-get e :type))))
                    events))

(defun m5r-last-request ()
  "The newest request the scripted transport saw, or nil.
`smoke--fake-network' pushes, so the newest request is the head of the list;
`(car (last ...))' would return the oldest one."
  (car smoke--requests))

(defun m5r-request-tools (&optional request)
  "Tool names the model was actually offered in REQUEST."
  (plist-get (or request (m5r-last-request)) :tools))

(defun m5r-prompt-has (marker)
  "Whether MARKER appears in any prompt the transport saw."
  (cl-some (lambda (r)
             (string-match-p (regexp-quote marker) (or (plist-get r :prompt-text) "")))
           smoke--requests))

(defun m5r-receipt (session)
  "The newest `prompt.assembled' receipt payload for SESSION."
  (let ((event (car (last (m5r-of-type "prompt.assembled" (m5r-events session))))))
    (plist-get event :payload)))

(defun m5r-count-substring (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (let ((count 0) (start 0))
    (while (string-match (regexp-quote needle) (or haystack "") start)
      (setq count (1+ count) start (match-end 0)))
    count))

;;;; Slice 1: documentation discovery ------------------------------------------

(ert-deftest m5/discovery/the-default-request-offers-the-documentation-tool ()
  "An ordinary chat request offers `describe-function', so the model can read a
symbol's documentation without going through the approval-gated `eval_elisp'.
The name is already on `superchat-authority-unprompted-read-tools', so nothing
about approval changes; what is missing is its place on the default surface.
Known gap at 7da55807: `superchat-llm-default-tool-names' has the other four
verbs and not this one, so the discovery loop apropos -> describe -> eval is
broken in its middle step."
  :expected-result :failed
  (m5r-fixture '((:text "ok"))
    (let ((view (smoke--open-chat)))
      (m5r-settle (smoke--send view "what does save-buffer do?"))
      (let ((offered (m5r-request-tools)))
        (should offered)
        (should (member "apropos" offered))
        (should (member "describe-function" offered))
        ;; It reads documentation, not a live value, which is why it may be
        ;; unprompted where `describe-variable' is not.
        (should (member "describe-function" superchat-authority-unprompted-read-tools))
        (should-not (member "describe-variable" superchat-authority-unprompted-read-tools))))))

(ert-deftest m5/discovery/an-explicit-tool-list-is-not-widened ()
  "Adding a verb to the default surface must not widen a request that names its
own tools: an explicit list keeps exactly what it names.  This is the guarantee
that makes slice 1 safe to ship by default."
  (m5r-fixture '((:text "ok"))
    (let ((view (smoke--open-chat)))
      (with-current-buffer view
        (setq-local superchat--active-preset
                    (superchat-preset-from-plist
                     (list :name "m5r-narrow" :type 'agent :capability "m5r"
                           :tools '("read_buffer")
                           :body "narrow" :source 'builtin))))
      (m5r-settle (smoke--send view "hello"))
      ;; Name the precondition: without it a missing request reads as an empty
      ;; tool list, which looks like the narrowing working perfectly.
      (should (smoke--wait (lambda () smoke--requests) 10))
      (let ((offered (m5r-request-tools)))
        (should (member "read_buffer" offered))
        (should-not (member "describe-function" offered))
        (should-not (member "apropos" offered))))))

;;;; Slice 2: shared origin context --------------------------------------------

(ert-deftest m5/origin/both-entries-carry-the-same-origin-once ()
  "The chat window and `superchat-act' send the model the same facts about the
buffer the user came from, and each fact appears once.
Today only the act path carries them, and it carries them by appending an
\"[Emacs origin]\" block to the user's own input text
(`superchat--input-with-origin', which has one caller); the chat path carries
nothing at all.
Known gap at 7da55807: there is no origin context collector, so the two entries
cannot agree."
  :expected-result :failed
  (m5r-fixture '((:text "ok") (:text "ok"))
    (let ((source (get-buffer-create "m5r-origin-source.py")))
      (unwind-protect
          (let (act-prompt chat-prompt)
            (with-current-buffer source
              (fundamental-mode)
              (insert "def hello():\n    pass\n")
              (goto-char (point-min))
              (ignore-errors (superchat-act "explain this")))
            (should (smoke--wait (lambda () smoke--requests) 10))
            (setq act-prompt (plist-get (m5r-last-request) :prompt-text))
            ;; Let the act Run finish: a second send while it is live would be
            ;; queued, not started, and this test is about two separate turns.
            (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 10))
            ;; The same source buffer, reached through the chat window instead.
            ;; A fresh Session: sharing one would let the act turn's own origin
            ;; block reach this request as conversation history, which would
            ;; make the test pass without the chat path carrying anything.
            (let ((view (with-current-buffer source (superchat-new-session))))
              (m5r-settle (smoke--send view "explain this"))
              (setq chat-prompt (plist-get (m5r-last-request) :prompt-text)))
            (should act-prompt)
            (should chat-prompt)
            (dolist (fact (list (buffer-name source) "fundamental"))
              (ert-info ((format "origin fact: %s" fact))
                (should (string-match-p (regexp-quote fact) act-prompt))
                (should (string-match-p (regexp-quote fact) chat-prompt))))
            ;; Once, not twice: the inspector path assembles and backfills the
            ;; prompt, and the collector runs again when the request is sent.
            (should (= 1 (m5r-count-substring (buffer-name source) chat-prompt))))
        (kill-buffer source)))))

(ert-deftest m5/origin/the-selection-is-data-not-instruction ()
  "A region the user had selected reaches the model as budgeted data, not as a
system instruction: the user's own text must never become part of what the model
is told to be.  Checked structurally on the assembly receipt -- the same request
sent with and without a selection must leave `system-chars' unchanged while
`message-chars' grows -- so it holds whatever id or slot the implementation
chooses.
Known gap at 7da55807: the chat path carries no origin or selection at all, so
the selected text never reaches the model by either route."
  :expected-result :failed
  (m5r-fixture '((:text "ok") (:text "ok"))
    (let ((source (get-buffer-create "m5r-selection-source.py")))
      (unwind-protect
          (let (plain selected)
            (with-current-buffer source
              (fundamental-mode)
              (insert "M5R_SELECTED_MARKER = 1\n"))
            ;; First turn: no region active.
            (let* ((view (with-current-buffer source
                           (deactivate-mark)
                           (smoke--open-chat)))
                   (session (buffer-local-value 'superchat--session-id view)))
              (m5r-settle (smoke--send view "what is this"))
              (setq plain (m5r-receipt session))
              ;; Second turn: the whole buffer selected in the source.
              (with-current-buffer source
                (goto-char (point-min))
                (set-mark (point-min))
                (goto-char (point-max))
                (activate-mark))
              (m5r-settle (smoke--send view "and now"))
              (setq selected (m5r-receipt session)))
            (should plain)
            (should selected)
            (should (m5r-prompt-has "M5R_SELECTED_MARKER"))
            ;; Data grows; instructions do not.
            (should (> (or (plist-get selected :message-chars) 0)
                       (or (plist-get plain :message-chars) 0)))
            (should (= (or (plist-get selected :system-chars) 0)
                       (or (plist-get plain :system-chars) 0))))
        (kill-buffer source)))))

(provide 'm5-regression)
;;; m5-regression.el ends here
