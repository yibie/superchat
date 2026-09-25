;;; tool-compaction-regression.el --- compaction covers tool observations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the T3 slice of the accepted decision
;; docs/decisions/accepted/2026-09-23-the-next-turn-sees-previous-tool-results.md
;; and the contract amendment "Amendment 2026-09-23: tool observations"
;; (docs/design/context-continuity-contract-v0.md), paragraph "Compaction.":
;; the summarizer's input carries the same Tool: line (note first), the new
;; Anchor's sources include both rows of a summarized pair, expansion renders
;; one entry per pair, and a pair left in the recent window is untouched.
;; Since the amendment of 2026-09-25 (earlier turns as messages) the next
;; turn receives the retained pair as a call and its tool message, while the
;; summarizer's own request keeps the Tool: line.
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
;;     -l scripts/tool-compaction-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^tool-compaction/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'json)

(when (getenv "SMOKE_REAL")
  (error "Tool compaction regression tests are offline; unset SMOKE_REAL"))

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

(defvar tool-compaction--wires nil
  "Provider requests of the current test, newest first, as wire messages.")

(defun tool-compaction--wire-of (provider prompt)
  "Return PROMPT as the messages llm.el sends to an OpenAI-style PROVIDER."
  (let* ((request (llm-provider-chat-request provider prompt nil))
         (messages (or (plist-get request :messages)
                       (alist-get 'messages request))))
    (json-parse-string (json-encode messages)
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defmacro tool-compaction--fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil)
         (tool-compaction--wires nil))
     (smoke--with-fixture ,script
       (let ((network (symbol-function 'smoke--fake-network)))
         (cl-letf (((symbol-function 'smoke--fake-network)
                    (lambda (mode provider prompt partial response error multi)
                      (push (tool-compaction--wire-of provider prompt)
                            tool-compaction--wires)
                      (funcall network mode provider prompt
                               partial response error multi))))
           ,@body)))))

(defun tool-compaction--script (name-a name-b)
  "Script: a tool pair on NAME-A, three answers, a pair on NAME-B, summary."
  (list `(:tool-uses (("read_buffer" (buffer . ,name-a))))
        '(:text "answer one")
        '(:text "answer two")
        '(:text "answer three")
        `(:tool-uses (("read_buffer" (buffer . ,name-b))))
        '(:text "answer four")
        (list :text (concat "SUMMARY-T3\n"
                            superchat-compact--completion-marker))
        '(:text "after compaction")))

(defun tool-compaction--settle (run)
  "Wait until exact RUN is terminal."
  (should (stringp run))
  (should (smoke--wait
           (lambda ()
             (and (smoke--run-done-p run)
                  (eq 'completed (plist-get (superchat-runtime-status run) :status))))
           20))
  run)

(defun tool-compaction--session (view)
  "Return VIEW's Session id."
  (buffer-local-value 'superchat--session-id view))

(defun tool-compaction--run (view text)
  "Send TEXT from VIEW and wait for the scripted Turn."
  (tool-compaction--settle (smoke--send view text)))

(defun tool-compaction--replayed (session type)
  "Return SESSION's replayed events of exactly TYPE."
  (cl-remove-if-not
   (lambda (event) (equal type (plist-get event :type)))
   (superchat-tape-replay-events session)))

(defun tool-compaction--id (session type &optional nth)
  "Return SESSION's Nth (default first) replayed TYPE event's storage id."
  (plist-get (nth (or nth 0) (tool-compaction--replayed session type))
             :storage-id))

(defun tool-compaction--count (needle text)
  "Count non-overlapping occurrences of NEEDLE in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq start (match-end 0) count (1+ count)))
    count))

(defun tool-compaction--anchor (session)
  "Return SESSION's latest Anchor row."
  (superchat-service-call nil 'session 'anchor-latest session))

(defun tool-compaction--anchor-source-ids (session)
  "Return SESSION's latest Anchor source ids as a list."
  (let ((anchor (tool-compaction--anchor session)))
    (and anchor
         (append (superchat-service-call nil 'session 'anchor-source-ids anchor)
                 nil))))

(defun tool-compaction--compact (view)
  "Run one scripted compaction from VIEW; return (OK . VALUE)."
  (let (done result)
    (with-current-buffer view
      (superchat-compact-session
       (lambda (ok value) (setq done t result (cons ok value)))))
    (should (smoke--wait (lambda () done) 30))
    result))

(defun tool-compaction--prepare (view &optional store-material)
  "Four turns, then a compaction; return the Session id.
STORE-MATERIAL, when nil, disables tool material storage."
  (let ((superchat-context-max-chars 400)
        (superchat-tape-store-tool-material store-material))
    (tool-compaction--run view "ask one")
    (tool-compaction--run view "ask two")
    (tool-compaction--run view "ask three")
    (tool-compaction--run view "ask four")
    (tool-compaction--compact view)
    (tool-compaction--session view)))

(defun tool-compaction--prompt ()
  "Return the newest provider request's prompt text."
  (plist-get (car smoke--requests) :prompt-text))

(defun tool-compaction--tool-line (text token)
  "Return TEXT's Tool: line containing TOKEN, or nil."
  (when (string-match (format "^Tool: .*%s.*$" (regexp-quote token)) text)
    (match-string 0 text)))

;;;; The summarizer sees the tool line

(ert-deftest tool-compaction/summary-input-contains-the-tool-line ()
  "The compaction prompt carries the note once and the summarized tool line."
  (let ((a (generate-new-buffer "tool-compaction-a"))
        (b (generate-new-buffer "tool-compaction-b")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "T3-TOKEN-A\n"))
          (with-current-buffer b (insert "T3-TOKEN-B\n"))
          (tool-compaction--fixture
              (tool-compaction--script (buffer-name a) (buffer-name b))
            (let ((view (smoke--open-chat)))
              (tool-compaction--prepare view t)
              (let ((prompt (tool-compaction--prompt)))
                (should (= 1 (tool-compaction--count
                              superchat-tape-tool-observation-note prompt)))
                (should (tool-compaction--tool-line prompt "T3-TOKEN-A"))
                (should (string-match-p "-> \\[ok\\]" prompt))))))
      (kill-buffer a)
      (kill-buffer b))))

;;;; The Anchor covers both rows of the summarized pair, not the recent one

(ert-deftest tool-compaction/anchor-covers-both-rows ()
  "The new Anchor names both rows of the summarized pair; the recent pair
is left out."
  (let ((a (generate-new-buffer "tool-compaction-a"))
        (b (generate-new-buffer "tool-compaction-b")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "T3-TOKEN-A\n"))
          (with-current-buffer b (insert "T3-TOKEN-B\n"))
          (tool-compaction--fixture
              (tool-compaction--script (buffer-name a) (buffer-name b))
            (let* ((view (smoke--open-chat))
                   (session (tool-compaction--prepare view t))
                   (sources (tool-compaction--anchor-source-ids session)))
              (should sources)
              ;; Pair A is summarized; both of its rows are sources.
              (should (memql (tool-compaction--id session "tool.requested" 0)
                             sources))
              (should (memql (tool-compaction--id session "tool.completed" 0)
                             sources))
              ;; Pair B is retained raw and must not be covered.
              (should-not (memql (tool-compaction--id session "tool.requested" 1)
                                 sources))
              (should-not (memql (tool-compaction--id session "tool.completed" 1)
                                 sources)))))
      (kill-buffer a)
      (kill-buffer b))))

;;;; After compaction: summary, no double call, recent pair still visible

(ert-deftest tool-compaction/next-turn-has-no-double-call ()
  "The next turn's request carries the Anchor summary and nothing of the
summarized pair, while the retained recent pair still arrives, once, as a call
and the tool message answering it."
  (let ((a (generate-new-buffer "tool-compaction-a"))
        (b (generate-new-buffer "tool-compaction-b")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "T3-TOKEN-A\n"))
          (with-current-buffer b (insert "T3-TOKEN-B\n"))
          (tool-compaction--fixture
              (tool-compaction--script (buffer-name a) (buffer-name b))
            (let* ((view (smoke--open-chat))
                   (superchat-context-max-chars 400))
              (tool-compaction--prepare view t)
              (tool-compaction--run view "after compaction")
              (let* ((wire (car tool-compaction--wires))
                     (texts (mapcar (lambda (message)
                                      (let ((content (alist-get 'content message)))
                                        (if (stringp content) content "")))
                                    wire))
                     (calls (cl-loop for message in wire
                                     append (alist-get 'tool_calls message)))
                     (answers (cl-remove-if-not
                               (lambda (message)
                                 (equal "tool" (alist-get 'role message)))
                               wire)))
                (should (cl-some (lambda (text) (string-match-p "SUMMARY-T3" text))
                                 texts))
                (should-not (cl-some (lambda (text) (string-match-p "T3-TOKEN-A" text))
                                     texts))
                (should (= 1 (length calls)))
                (should (= 1 (length answers)))
                (should (equal (alist-get 'id (car calls))
                               (alist-get 'tool_call_id (car answers))))
                (should (string-match-p "T3-TOKEN-B"
                                        (alist-get 'content (car answers))))))))
      (kill-buffer a)
      (kill-buffer b))))

;;;; Expansion renders one entry per pair

(ert-deftest tool-compaction/expansion-renders-one-line-per-pair ()
  "Expanding the Anchor yields one tool-line entry for the pair and no
`tool.requested:'/`tool.completed:' summary text."
  (let ((a (generate-new-buffer "tool-compaction-a"))
        (b (generate-new-buffer "tool-compaction-b")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "T3-TOKEN-A\n"))
          (with-current-buffer b (insert "T3-TOKEN-B\n"))
          (tool-compaction--fixture
              (tool-compaction--script (buffer-name a) (buffer-name b))
            (let* ((view (smoke--open-chat))
                   (session (tool-compaction--prepare view t))
                   (anchor (tool-compaction--anchor session))
                   (rows (superchat-service-call
                          nil 'session 'expand-anchor session anchor))
                   (entries (superchat-compact--expanded-entries session rows))
                   (tool-entries
                    (cl-remove-if-not (lambda (entry)
                                        (string-prefix-p "Tool: "
                                                         (or (plist-get entry :content)
                                                             "")))
                                      entries)))
              (should (= 1 (length tool-entries)))
              (should (string-match-p "T3-TOKEN-A"
                                      (plist-get (car tool-entries) :content)))
              (should (memql (tool-compaction--id session "tool.completed" 0)
                             (append (plist-get (car tool-entries) :source-ids)
                                     nil)))
              (should-not
               (cl-some (lambda (entry)
                          (string-match-p "tool\\.\\(?:requested\\|completed\\|execution\\.started\\):"
                                          (or (plist-get entry :content) "")))
                        entries)))))
      (kill-buffer a)
      (kill-buffer b))))

;;;; Storage off: exactly the pre-slice behaviour

(ert-deftest tool-compaction/rows-without-text-are-unchanged ()
  "With no stored text, compaction ignores tool rows exactly as before."
  (let ((a (generate-new-buffer "tool-compaction-a"))
        (b (generate-new-buffer "tool-compaction-b")))
    (unwind-protect
        (progn
          (with-current-buffer a (insert "T3-TOKEN-A\n"))
          (with-current-buffer b (insert "T3-TOKEN-B\n"))
          (tool-compaction--fixture
              (tool-compaction--script (buffer-name a) (buffer-name b))
            (let* ((view (smoke--open-chat))
                   (session (tool-compaction--prepare view nil))
                   (prompt (tool-compaction--prompt))
                   (sources (tool-compaction--anchor-source-ids session))
                   (tool-ids (append
                              (mapcar (lambda (event) (plist-get event :storage-id))
                                      (tool-compaction--replayed session "tool.requested"))
                              (mapcar (lambda (event) (plist-get event :storage-id))
                                      (tool-compaction--replayed session "tool.completed")))))
              (should-not (string-match-p "Tool:" prompt))
              (should-not (string-match-p
                           (regexp-quote superchat-tape-tool-observation-note)
                           prompt))
              (should tool-ids)
              (dolist (id tool-ids)
                (should-not (memql id sources))))))
      (kill-buffer a)
      (kill-buffer b))))

(provide 'tool-compaction-regression)
;;; tool-compaction-regression.el ends here
