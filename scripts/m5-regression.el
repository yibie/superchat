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
follow-up FIFO, its dispatch timers and the fault ring.

Every test in this set is scripted-only and skips under SMOKE_REAL=1.  They
assert on what one exact request carried and on tool calls the script makes the
model issue; a real provider supplies neither, so running them against one
fails on the fixture rather than on the product.  Real-model evidence for these
behaviours belongs to the slice's own end-to-end run, not to this set.  A future
test that genuinely needs a live provider should not use this fixture."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil))
       (smoke--with-fixture ,script ,@body))))

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

(defun m5r-open-chat-from (source)
  "Open the chat window from SOURCE and return the View.
`smoke--open-chat\' pins its own origin buffer so the canonical baseline cannot
depend on whichever buffer happened to be current; a test about the origin has
to name the buffer it means."
  (with-current-buffer source (superchat))
  (or (get-buffer superchat-buffer-name) (current-buffer)))

(defmacro m5r-capturing-provider (var &rest body)
  "Run BODY with VAR collecting each provider prompt as (:system S :text T).
The fixture already replaced the transport; this wraps whatever is installed.
S is the prompt\'s `context\' field alone; T is `llm-chat-prompt-to-text\', which
is the whole request -- context and every role together -- so the pair supports
\"this string is not in the system field\" but not yet \"it is in the current user
message\".  Slice 2\'s full acceptance should read
`llm-chat-prompt-interactions\' to locate the current user content and prove the
role as well.
Even so the pair is what closes a false green: comparing whole prompt strings,
or character counts between two turns, cannot tell the halves apart, and an
implementation that puts the same wrong thing into `system\' on every turn
leaves both counts unchanged and passes."
  (declare (indent 1) (debug t))
  (let ((orig (make-symbol "orig")))
    `(let ((,var nil))
       (let ((,orig (symbol-function 'llm-chat-streaming)))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (provider prompt &rest args)
                      (push (list :system (or (ignore-errors
                                                (llm-chat-prompt-context prompt))
                                              "")
                                  :text (or (ignore-errors
                                              (llm-chat-prompt-to-text prompt))
                                            ""))
                            ,var)
                      (apply ,orig provider prompt args))))
           ,@body)))))

;;;; Slice 1: documentation discovery ------------------------------------------

(ert-deftest m5/discovery/the-default-request-offers-the-documentation-tool ()
  "An ordinary chat request offers `describe-function', so the model can read a
symbol's documentation without going through the approval-gated `eval_elisp'.
It is a built-in read, which no permission level asks about, so nothing about
approval changes; what was missing is its place on the default surface.
It joined `superchat-llm-default-tool-names' with slice 1; before that the
discovery loop apropos -> describe -> eval was broken in its middle step."
  (m5r-fixture '((:text "ok"))
    (let ((view (smoke--open-chat)))
      (m5r-settle (smoke--send view "what does save-buffer do?"))
      (let ((offered (m5r-request-tools)))
        (should offered)
        (should (member "apropos" offered))
        (should (member "describe-function" offered))
        ;; Since M6 the permission level decides who is asked: a built-in
        ;; tool checks its own targets and is not asked about, while the
        ;; approval-gated route it spares the model is full Emacs control.
        (should (eq 'builtin (superchat-sandbox-tool-class "describe-function")))
        (should (eq 'full-control (superchat-sandbox-tool-class "eval_elisp")))))))

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

(ert-deftest m5/discovery/the-documentation-tool-reads-without-a-prompt ()
  "Offering the tool is not the same as being able to use it.  The model is
scripted to call `describe-function\', so the call goes the whole way -- tool
selection, the executor, the trusted wrapper -- and the answer must carry a known
function\'s real documentation with no approval asked, because a built-in read
is asked about under no permission level.  A stricter listener still wins.
This is the second half of slice 1: seen, callable, and decided correctly.
Before slice 1 the name was not on the default surface, so the call was refused
before Authority saw it: measured, the same script with `read_buffer\' reached
the approval function and this one did not."
  (m5r-fixture '((:tool-uses (("describe-function" (function_name . "save-buffer"))))
                 (:text "done"))
    (let* ((asked 0)
           (superchat-approval-function
            (lambda (_request) (setq asked (1+ asked)) t))
           (view (smoke--open-chat)))
      (m5r-settle (smoke--send view "what does save-buffer do?"))
      ;; The follow-up request carries the tool result back to the model.
      (should (m5r-prompt-has "save-buffer"))
      (should (m5r-prompt-has "Save current buffer"))
      (should (= 0 asked)))))

(ert-deftest m5/discovery/a-stricter-read-policy-still-prompts ()
  "Being unprompted is the default, not a promise: whoever wants to be asked
about documentation reads says so with a `tools/pre-execute\' listener that
answers `ask\', and is asked.  Slice 1 must not quietly weaken this.
Until M6 the same stance was the option `superchat-authority-prompt-for-all-reads\';
before slice 1 the call did not reach the executor at all, so nothing was asked."
  (m5r-fixture '((:tool-uses (("describe-function" (function_name . "save-buffer"))))
                 (:text "done"))
    (let* ((asked 0)
           (superchat-approval-function
            (lambda (_request) (setq asked (1+ asked)) t))
           (view (smoke--open-chat)))
      (superchat-on nil 'tools/pre-execute 'm5r-ask-for-reads
                    (lambda (call _next)
                      (plist-put call :kind 'ask)))
      (unwind-protect
          (progn
            (m5r-settle (smoke--send view "what does save-buffer do?"))
            (should (m5r-prompt-has "Save current buffer"))
            (should (= 1 asked)))
        (superchat-off 'tools/pre-execute 'm5r-ask-for-reads)))))

;;;; Slice 2: shared origin context --------------------------------------------

(ert-deftest m5/origin/both-entries-carry-the-same-origin-once ()
  "The chat window and `superchat-act' send the model the same facts about the
buffer the user came from, and each fact appears once.
Today only the act path carries them, and it carries them by appending an
\"[Emacs origin]\" block to the user's own input text
(`superchat--input-with-origin', which has one caller); the chat path carries
nothing at all.
Known gap at 7da55807: there is no origin context collector, so the two entries
cannot agree.  Slice 2 gave both entries one collector, so the act path no
longer splices the facts into the user's own input text to carry them."
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
  "A region the user had selected reaches the model as data in the message it
belongs to, and never as part of the system instructions: the user\'s own text
must not become part of what the model is told to be.
Checked on the two halves of the provider prompt for one request, not by
comparing counts between two turns.  An earlier version of this test compared
`system-chars\' and `message-chars\' across two turns in one Session; the user
showed it passing against a deliberately wrong collector that put the whole
source buffer into `system\' on every turn -- constant wrongness leaves both
deltas intact, and the growth it did see came from conversation history.
Known gap at 35639ef7: the chat path carries no origin or selection at all, so
the selected text never reached the model by either route; slice 2 put it in its
own `attachment' fragment, under the budget."
  (m5r-fixture '((:text "ok"))
    (let ((source (get-buffer-create "m5r-selection-source.py")))
      (unwind-protect
          (m5r-capturing-provider seen
            (with-current-buffer source
              (fundamental-mode)
              (insert "M5R_SELECTED_MARKER = 1\n")
              (goto-char (point-min))
              (set-mark (point-min))
              (goto-char (point-max))
              (activate-mark))
            (let ((view (m5r-open-chat-from source)))
              (m5r-settle (smoke--send view "what is this")))
            (should seen)
            (let* ((request (car seen))
                   (system (plist-get request :system))
                   (text (plist-get request :text)))
              ;; It reached the model...
              (should (string-match-p "M5R_SELECTED_MARKER" text))
              ;; ...as data, exactly once, and not as an instruction.
              (should-not (string-match-p "M5R_SELECTED_MARKER" system))
              (should (= 1 (m5r-count-substring "M5R_SELECTED_MARKER" text)))))
        (kill-buffer source)))))

(ert-deftest m5/origin/a-renamed-source-keeps-its-identity ()
  "The association holds the buffer, not its name: renaming the source keeps it,
and a new buffer that takes the old name is never read in its place.  This is
what makes \"the object you were talking about\" mean something across an
ordinary editing session."
  (m5r-fixture '((:text "ok"))
    (let ((source (get-buffer-create "m5r-life.py"))
          (decoy nil))
      (unwind-protect
          (progn
            (with-current-buffer source (fundamental-mode) (insert "ORIGINAL = 1\n"))
            (let ((view (m5r-open-chat-from source)))
              (with-current-buffer source (rename-buffer "m5r-life-renamed.py"))
              (setq decoy (get-buffer-create "m5r-life.py"))
              (with-current-buffer decoy (text-mode) (insert "DECOY = 2\n"))
              (m5r-settle (smoke--send view "what is this"))
              (let ((text (plist-get (m5r-last-request) :prompt-text)))
                ;; The same object, under its current name.
                (should (string-match-p "m5r-life-renamed\\.py" text))
                (should-not (string-match-p "Buffer: m5r-life\\.py" text))
                ;; The impostor's own facts never appear.
                (should-not (string-match-p "text-mode" text)))))
        (when (buffer-live-p decoy) (kill-buffer decoy))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest m5/origin/a-dead-source-is-reported-not-silently-dropped ()
  "When the buffer a chat was opened from is gone, the request says so and names
it.  Silence is the wrong answer: the model would otherwise answer about a
buffer it was never told had disappeared, and a new buffer that took the name
must not stand in either.
The facts are kept on the View while the source is alive, because a killed
buffer answers every accessor -- `buffer-name' included -- with nil, so what was
recorded is the only honest answer left.  Not being silent is the floor here,
not being fully described: a source that dies before any snapshot was taken is
reported by the name the association recorded."
  (m5r-fixture '((:text "ok"))
    (let ((source (get-buffer-create "m5r-dead.py"))
          (replacement nil))
      (unwind-protect
          (progn
            (with-current-buffer source (fundamental-mode) (insert "GONE = 1\n"))
            (let ((view (m5r-open-chat-from source)))
              (kill-buffer source)
              (setq replacement (get-buffer-create "m5r-dead.py"))
              (with-current-buffer replacement (text-mode) (insert "REPLACEMENT = 2\n"))
              (m5r-settle (smoke--send view "what is this"))
              (let ((text (plist-get (m5r-last-request) :prompt-text)))
                (should (string-match-p "m5r-dead\\.py" text))
                (should (string-match-p "no longer exists" text))
                ;; The replacement is a different object and is never read.
                (should-not (string-match-p "text-mode" text))
                (should-not (string-match-p "REPLACEMENT" text)))))
        (when (buffer-live-p replacement) (kill-buffer replacement))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest m5/origin/unmounting-the-collector-stops-the-context ()
  "The origin is contributed by a row: unmount `chat-collectors' and new requests
carry no origin, while the user's buffer and the association are untouched.
A composition change withdraws a contribution; it does not reach into the
user's Emacs."
  (m5r-fixture '((:text "ok") (:text "ok"))
    (let ((source (get-buffer-create "m5r-unmount.py")))
      (unwind-protect
          (let ((view nil))
            (with-current-buffer source (fundamental-mode) (insert "STILL_HERE = 1\n"))
            (setq view (m5r-open-chat-from source))
            (m5r-settle (smoke--send view "first"))
            (should (string-match-p "m5r-unmount\\.py"
                                    (plist-get (m5r-last-request) :prompt-text)))
            (unwind-protect
                (progn
                  (superchat-plugin-unmount 'chat-collectors)
                  (m5r-settle (smoke--send view "second"))
                  (should-not (string-match-p "m5r-unmount\\.py"
                                              (plist-get (m5r-last-request) :prompt-text))))
              (unless (superchat-plugin-state 'chat-collectors)
                (ignore-errors (superchat-plugin-mount 'chat-collectors))))
            ;; The row went; the user's buffer and the association stayed.
            (should (buffer-live-p source))
            (should (eq source (buffer-local-value 'superchat--origin-buffer view))))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest m5/origin/the-inspector-body-is-sent-exactly-as-reviewed ()
  "The request inspector shows the assembled body, and what the user confirms is
what goes out.  Deleting the selection there must delete it from the request:
the collector runs again at send, and it must not render the facts a second
time nor restore what the user took away.  This is the case the three capture
moments -- capture, preview, send -- make easy to get wrong.
The blocker this pinned was wider than first registered: the send transaction is
unbound for *every* caller of `superchat--execute-plan-in-chat', so plain
`superchat-act' signalled too -- after its request had already gone out, which
is why a probe that only checked for a request called the path healthy.  The
transaction is now armed at that shared entry.  Two more defects in what the user
sees being what the user sends surfaced with it: the inspector's button row was
inside the region the send reads, and the selection fragment carried a second
`[Emacs origin]' heading of its own."
  (m5r-fixture '((:text "ok"))
    (let ((source (get-buffer-create "m5r-inspect.py"))
          (inspector nil))
      (unwind-protect
          (progn
            (with-current-buffer source
              (fundamental-mode)
              (insert "M5R_INSPECT_SELECTION = 1\n")
              (goto-char (point-min))
              (set-mark (point-min))
              (goto-char (point-max))
              (activate-mark))
            (with-current-buffer source (superchat-act "explain" t))
            (setq inspector
                  (cl-find-if (lambda (b)
                                (with-current-buffer b
                                  (derived-mode-p 'superchat-request-mode)))
                              (buffer-list)))
            (should (buffer-live-p inspector))
            (with-current-buffer inspector
              (let ((body (buffer-substring-no-properties
                           superchat--request-prompt-start
                           superchat--request-prompt-end)))
                ;; The user is shown both halves before deciding.
                (should (string-match-p "Emacs origin" body))
                (should (string-match-p "M5R_INSPECT_SELECTION" body)))
              ;; The user deletes the selection from what will be sent.
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char superchat--request-prompt-start)
                  (while (re-search-forward "^.*M5R_INSPECT_SELECTION.*\n?"
                                            superchat--request-prompt-end t)
                    (replace-match ""))))
              (superchat-request-send))
            (should (smoke--wait (lambda () smoke--requests) 10))
            (let ((text (plist-get (m5r-last-request) :prompt-text)))
              ;; Gone, and not put back.
              (should-not (string-match-p "M5R_INSPECT_SELECTION" text))
              ;; The inspector's own chrome is not part of the message.  The
              ;; prompt's end marker used to advance over the button row as it
              ;; was inserted, so the labels joined the region the send reads.
              (dolist (label '("Send" "Toggle Log" "Open Log" "Cancel"))
                (ert-info ((format "inspector chrome: %s" label))
                  (should-not (string-match-p (concat "\\b" label "\\b") text))))
              ;; The facts the user did keep appear once, not twice.
              (should (= 1 (m5r-count-substring "[Emacs origin]" text)))
              (should (= 1 (m5r-count-substring "m5r-inspect.py" text)))))
        (when (buffer-live-p inspector) (kill-buffer inspector))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest m5/origin/a-queued-request-keeps-the-snapshot-it-was-prepared-with ()
  "A request the user sent while another Run was live is prepared when they sent
it, so it carries the facts as they were then.  Renaming the source while the
request waits in the queue must not rewrite what that request says: the user
asked about the buffer as it was, and the snapshot is frozen at preparation.
The facts are frozen once, where a request is prepared, so ordinary chat, queued
chat and the inspector's plan all say what was true when the user sent them;
only the next request re-reads the View."
  (m5r-fixture '((:text "first" :delay 1.0) (:text "second"))
    (let ((source (get-buffer-create "m5r-queued.py")))
      (unwind-protect
          (let* ((view (m5r-open-chat-from source))
                 (first nil))
            (with-current-buffer source (fundamental-mode) (insert "QUEUED = 1\n"))
            (setq first (smoke--send view "first"))
            (should (smoke--wait (lambda () (member first (superchat-runtime-active-runs))) 10))
            ;; Sent while the first Run is live: this one is queued, not started.
            (smoke--send view "second")
            (with-current-buffer source (rename-buffer "m5r-queued-renamed.py"))
            (m5r-settle first)
            (should (smoke--wait (lambda () (>= (length smoke--requests) 2)) 10))
            (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 10))
            (let ((text (plist-get (m5r-last-request) :prompt-text)))
              (should (string-match-p "m5r-queued\\.py" text))
              (should-not (string-match-p "m5r-queued-renamed" text))))
        (when (buffer-live-p source) (kill-buffer source))))))

(provide 'm5-regression)
;;; m5-regression.el ends here
