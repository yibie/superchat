;;; context-recovery-regression.el --- a Run resumed after compaction -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the Recovery paragraph of the context contract
;; (docs/design/context-continuity-contract-v0.md, amendment 2026-09-25, as
;; amended the same day) and the "Recovery after a request that is too long"
;; bullet of docs/decisions/implemented/2026-09-25-earlier-turns-as-real-tool-messages.md.
;;
;; When the provider refuses a request as too long, compaction summarizes the
;; Session and the same Run resumes with the View rebuilt after it.  Two rules:
;;
;; - The rebuilt View leaves out the rows the resumed Run wrote -- its question
;;   and the tool calls it already made -- because the resumed request carries
;;   them as the current Turn's own messages
;;   (.scratch/issues/2026-09-25-a-recovered-request-repeats-the-question.md).
;; - The Runtime adopts the rebuilt View, so the resumed attempt's request
;;   evidence names what the provider received, not the history from before
;;   compaction
;;   (.scratch/issues/2026-09-25-a-resumed-request-evidence-names-the-old-history.md).
;;
;; Every provider request is captured at the llm boundary and serialized with
;; llm.el's own OpenAI-compatible request builder, so the assertions are about
;; the wire messages.  The helpers follow history-messages-regression.el's;
;; that suite is not loaded, so its tests do not run again here.  All model
;; calls are scripted.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/context-recovery-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^context-recovery/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)

(when (getenv "SMOKE_REAL")
  (error "Context-recovery regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-compact)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar cr--wires nil
  "Provider requests of the current test, newest first, as wire messages.")

(defun cr--wire-of (provider prompt)
  "Return PROMPT as the message list llm.el sends to an OpenAI-style PROVIDER."
  (let* ((request (llm-provider-chat-request provider prompt nil))
         (messages (or (plist-get request :messages)
                       (alist-get 'messages request))))
    (json-parse-string (json-encode messages)
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defmacro cr--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical fixture, capturing wire messages."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil)
         (cr--wires nil)
         (target (get-buffer-create "cr-target")))
     (with-current-buffer target
       (erase-buffer)
       (insert "CR-TARGET-TEXT\n"))
     (unwind-protect
         (smoke--with-fixture ,script
           (let ((network (symbol-function 'smoke--fake-network)))
             (cl-letf (((symbol-function 'smoke--fake-network)
                        (lambda (mode provider prompt partial response error multi)
                          (push (cr--wire-of provider prompt) cr--wires)
                          (funcall network mode provider prompt
                                   partial response error multi))))
               ,@body)))
       (setq superchat-llm-tools-list nil)
       (when (buffer-live-p target) (kill-buffer target)))))

(defun cr--body (wire)
  "WIRE without its leading system message."
  (if (equal "system" (alist-get 'role (car wire))) (cdr wire) wire))

(defun cr--text (message)
  "MESSAGE's content as a string (empty when it has none)."
  (let ((content (alist-get 'content message)))
    (if (stringp content) content "")))

(defun cr--calls (message)
  (alist-get 'tool_calls message))

(defun cr--count (needle messages)
  "How many of MESSAGES contain NEEDLE."
  (cl-count-if (lambda (message)
                 (string-match-p (regexp-quote needle) (cr--text message)))
               messages))

(defun cr--read-round ()
  '(:tool-uses (("read_buffer" (buffer . "cr-target")))))

(defun cr--script (refused-turn)
  "Four turns, then REFUSED-TURN's model steps, a summary, the answer.
REFUSED-TURN ends with the provider refusing a request as too long."
  (append (list (cr--read-round)
                '(:text "answer one") '(:text "answer two") '(:text "answer three")
                (cr--read-round)
                '(:text "answer four"))
          refused-turn
          (list '(:error (llm-context-length-exceeded
                          . "context length exceeded"))
                (list :text (concat "CR-SUMMARY\n"
                                    superchat-compact--completion-marker))
                '(:text "after recovery"))))

(defun cr--resume (view)
  "Run four turns in VIEW, then CR-FIVE, whose request is refused as too long.
Return CR-FIVE's Run id once it completed."
  (let ((superchat-context-max-chars 400))
    (dolist (text '("ask one" "ask two" "ask three" "ask four"))
      (let ((run (smoke--send view text)))
        (should (stringp run))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))
    (let ((run (smoke--send view "CR-FIVE question")))
      (should (stringp run))
      (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      run)))

(defun cr--session (view)
  (buffer-local-value 'superchat--session-id view))

(defun cr--run-message-lists (session run)
  "RUN's `request.message-list' events in SESSION, oldest first."
  (cl-remove-if-not
   (lambda (event)
     (and (equal "request.message-list" (plist-get event :type))
          (equal run (plist-get (plist-get (plist-get event :payload) :execution)
                                :run-id))))
   (superchat-tape-replay-events session)))

;;;; The resumed request keeps its system text (found after acceptance)

(ert-deftest context-recovery/the-resumed-request-keeps-its-system-text ()
  "The resumed request begins with the same system text as the refused one,
and no earlier-turn message is sent twice."
  (cr--fixture (cr--script nil)
    (cr--resume (smoke--open-chat))
    (let ((refused (nth 2 cr--wires))
          (resumed (car cr--wires)))
      (should (equal "system" (alist-get 'role (car refused))))
      (should (equal (car refused) (car resumed)))
      (should (<= (cr--count "answer four" resumed) 1)))))

(ert-deftest context-recovery/the-evidence-places-the-summary-first ()
  "After a compaction, the request evidence rebuilds the summary where the
provider received it: the first earlier message, framed as a summary."
  (cr--fixture (list (cr--read-round)
                     '(:text "answer one") '(:text "answer two") '(:text "answer three")
                     (cr--read-round) '(:text "answer four")
                     (list :text (concat "CR-SUMMARY\n" superchat-compact--completion-marker))
                     '(:text "after compaction"))
    (let ((view (smoke--open-chat)))
      (let ((superchat-context-max-chars 400))
        (dolist (text '("ask one" "ask two" "ask three" "ask four"))
          (let ((run (smoke--send view text)))
            (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))
        (let (done)
          (with-current-buffer view
            (superchat-compact-session (lambda (_ok _value) (setq done t))))
          (should (smoke--wait (lambda () done) 30))))
      (let* ((run (smoke--send view "CR-AFTER question"))
             (_ (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
             (lists (cr--run-message-lists (cr--session view) run))
             (evidence (superchat-tape-request-evidence
                        (cr--session view)
                        (plist-get (plist-get (car (last lists)) :payload) :request-id)))
             (first (car (plist-get evidence :messages)))
             (wire-first (car (cr--body (car cr--wires)))))
        (should (eq 'reproduced (plist-get evidence :status)))
        (should (string-match-p "CR-SUMMARY" (cr--text wire-first)))
        (should (equal "user" (plist-get first :kind)))
        (should (equal (cr--text wire-first) (plist-get first :content)))))))

;;;; The rebuilt View leaves out the resumed Run's own rows

(ert-deftest context-recovery/the-question-is-sent-once ()
  "A request refused before any tool round resumes with the summary first and
the current question once, as the last user message -- not also as an earlier
turn."
  (cr--fixture (cr--script nil)
    (cr--resume (smoke--open-chat))
    (let ((refused (cr--body (nth 2 cr--wires)))
          (resumed (cr--body (car cr--wires))))
      ;; Guards: the refused request had the question once, and the resumed one
      ;; is the compacted request.
      (should (= 1 (cr--count "CR-FIVE" refused)))
      (should (string-match-p "CR-SUMMARY" (cr--text (car resumed))))
      (should (equal "user" (alist-get 'role (car (last resumed)))))
      (should (string-match-p "CR-FIVE" (cr--text (car (last resumed)))))
      (should (= 1 (cr--count "CR-FIVE" resumed))))))

(defun cr--run-execution-ids (session run)
  "The execution ids of the tool calls RUN requested in SESSION."
  (delq nil
        (mapcar (lambda (event)
                  (and (equal "tool.requested" (plist-get event :type))
                       (equal run (plist-get event :run-id))
                       (plist-get (plist-get event :payload) :execution-id)))
                (superchat-tape-replay-events session))))

(ert-deftest context-recovery/a-call-made-before-the-refusal-is-sent-once ()
  "A Run that made a tool round before its request was refused resumes with
that call once: as the live round after the question, not also as an earlier
turn's call before it."
  (cr--fixture (cr--script (list (cr--read-round)))
    (let* ((view (smoke--open-chat))
           (run (cr--resume view))
           (executions (cr--run-execution-ids (cr--session view) run))
           (resumed (cr--body (car cr--wires)))
           (question (cl-position "user" resumed
                                  :key (lambda (m) (alist-get 'role m))
                                  :test #'equal :from-end t))
           (live (nthcdr (1+ question) resumed))
           (wire-ids (cl-loop for message in resumed
                              append (mapcar (lambda (call) (alist-get 'id call))
                                             (cr--calls message))
                              when (alist-get 'tool_call_id message)
                              collect (alist-get 'tool_call_id message))))
      ;; Guards: the Run made one call before the refusal, and the resumed
      ;; request is the compacted one with the question last.
      (should (= 1 (length executions)))
      (should (string-match-p "CR-SUMMARY" (cr--text (car resumed))))
      (should (string-match-p "CR-FIVE" (cr--text (nth question resumed))))
      ;; The live round: one assistant message with the call, then its result.
      (should (equal '("assistant" "tool")
                     (mapcar (lambda (m) (alist-get 'role m)) live)))
      (should (= 1 (length (cr--calls (car live)))))
      ;; An earlier-turn call is sent under its execution id; this Run's call
      ;; must not be among them.
      (should-not (cl-intersection executions wire-ids :test #'equal))
      (should (= 1 (cr--count "CR-TARGET-TEXT" resumed)))
      (should (= 1 (cr--count "CR-FIVE" resumed))))))

(ert-deftest context-recovery/a-replaced-turn-stays-out ()
  "Guard (green before and after): a turn the user regenerated stays out of
the rebuilt View, as it stays out of the View built at a Turn's start -- the
rebuilt View's exclusions are the Session's replaced rows and the Run's own."
  (cr--fixture (append (list (cr--read-round)
                             '(:text "answer one") '(:text "answer two")
                             '(:text "answer three")
                             '(:text "CR-REPLACED answer")
                             '(:text "CR-KEPT answer")
                             '(:error (llm-context-length-exceeded
                                       . "context length exceeded"))
                             (list :text (concat "CR-SUMMARY\n"
                                                 superchat-compact--completion-marker))
                             '(:text "after recovery")))
    (let ((view (smoke--open-chat))
          (superchat-context-max-chars 400))
      (dolist (text '("ask one" "ask two" "ask three" "ask four"))
        (let ((run (smoke--send view text)))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))
      (let ((run (with-current-buffer view
                   (superchat-regenerate)
                   superchat--active-run-id)))
        (should (stringp run))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
      (let ((run (smoke--send view "CR-FIVE question")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 30))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))
      (let ((refused (cr--body (nth 2 cr--wires)))
            (resumed (cr--body (car cr--wires))))
        ;; Guards: the replaced answer was out of the refused request, the
        ;; answer that replaced it in, and the resumed request is compacted.
        (should (= 0 (cr--count "CR-REPLACED" refused)))
        (should (= 1 (cr--count "CR-KEPT" refused)))
        (should (string-match-p "CR-SUMMARY" (cr--text (car resumed))))
        (should (< 0 (cr--count "CR-KEPT" resumed)))
        (should (= 0 (cr--count "CR-REPLACED" resumed)))))))

;;;; The Runtime adopts the rebuilt View

(defun cr--ids (items)
  "The storage ids of ITEMS, a View's selection or a recorded one."
  (mapcar (lambda (item) (plist-get item :storage-id)) (append items nil)))

(ert-deftest context-recovery/the-resumed-evidence-names-the-rebuilt-view ()
  "The resumed attempt's request evidence names the View rebuilt after
compaction -- its items and its receipt -- not the View the Run was submitted
with, so it rebuilds the summary and no question from before compaction."
  (let (rebuilt)
    (cl-flet ((capture (envelope) (setq rebuilt envelope) envelope))
      (advice-add 'superchat-compact--context-envelope :filter-return #'capture)
      (unwind-protect
          (cr--fixture (cr--script nil)
            (let* ((view (smoke--open-chat))
                   (run (cr--resume view))
                   (session (cr--session view))
                   (lists (cr--run-message-lists session run))
                   (first (plist-get (car lists) :payload))
                   (last (plist-get (car (last lists)) :payload))
                   (evidence (superchat-tape-request-evidence
                              session (plist-get last :request-id)))
                   (contents (mapcar (lambda (m) (format "%s" (plist-get m :content)))
                                     (plist-get evidence :messages)))
                   (anchor (superchat-view-compaction-anchor-latest session))
                   (anchor-id (car anchor))
                   (sources (append (superchat-view-compaction-anchor-source-ids anchor)
                                    nil))
                   (own (delq nil (mapcar (lambda (event)
                                            (and (equal run (plist-get event :run-id))
                                                 (plist-get event :storage-id)))
                                          (superchat-tape-replay-events session)))))
              ;; Guards: two attempts (refused, resumed), a summary between
              ;; them, and the rebuilt View is not the submitted one.
              (should (= 2 (length lists)))
              (should (integerp anchor-id))
              (should sources)
              (should rebuilt)
              (should-not (equal (cr--ids (plist-get first :items))
                                 (cr--ids (plist-get rebuilt :items))))
              ;; The resumed attempt names the rebuilt View's selection: the
              ;; summary, none of the rows it replaced, none of the Run's own.
              (should (equal (cr--ids (plist-get rebuilt :items))
                             (cr--ids (plist-get last :items))))
              (should (memql anchor-id (cr--ids (plist-get last :items))))
              (should-not (cl-intersection sources (cr--ids (plist-get last :items))))
              (should-not (cl-intersection own (cr--ids (plist-get last :items))))
              ;; Its evidence rebuilds that selection.
              (should (eq 'reproduced (plist-get evidence :status)))
              (should (cl-some (lambda (text) (string-match-p "CR-SUMMARY" text))
                               contents))
              (should-not (member "ask one" contents))
              (should-not (cl-some (lambda (text) (string-match-p "CR-FIVE" text))
                                   contents))
              ;; The receipt: the refused attempt read the Tape before the
              ;; summary was written, the resumed one after it.
              (should (equal (plist-get (plist-get rebuilt :receipt) :tape-head)
                             (plist-get (plist-get last :receipt) :tape-head)))
              (should (< (plist-get (plist-get first :receipt) :tape-head)
                         anchor-id))))
        (advice-remove 'superchat-compact--context-envelope #'capture)))))

(provide 'context-recovery-regression)

;;; context-recovery-regression.el ends here
