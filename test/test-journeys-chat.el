;;; test-journeys-chat.el --- Chat-window journey acceptance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests intentionally start at the visible Superchat entry points.  The
;; fixture replaces only llm.el's provider request functions; all Runtime,
;; llm tool processing, Authority, Tape, rendering, and tools are real.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'journey-fixture
         (expand-file-name "journey-fixture"
                           (file-name-directory load-file-name)))
(require 'llm-deepseek)

(defun journey-chat--types-in-order-p (types wanted)
  "Return non-nil when WANTED occurs in TYPES in that order."
  (let ((remaining types))
    (catch 'missing
      (dolist (type wanted t)
        (let ((tail (member type remaining)))
          (unless tail (throw 'missing nil))
          (setq remaining (cdr tail)))))))

(defun journey-chat--confirm-with-key (key recorder)
  "Return a public approval callback that selects KEY in the real prompt.
RECORDER is called with the normalized approval request.  Batch Emacs cannot
read an interactive key, so the shim supplies only `read-multiple-choice''s
answer; `superchat-authority-confirm' still builds the actual choices and owns
the approval / session-allow semantics."
  (lambda (request)
    (funcall recorder request)
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _arguments)
                 (or (assq key choices)
                     (error "Journey choice %c was not offered" key)))))
      (superchat-authority-confirm request))))

(defun journey-chat--confirm-dangerous-with-y (recorder)
  "Return a callback selecting y while proving dangerous prompts omit a.
RECORDER is called with the normalized approval request."
  (lambda (request)
    (funcall recorder request)
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _arguments)
                 (should-not (assq ?a choices))
                 (or (assq ?y choices)
                     (error "Dangerous approval did not offer y")))))
      (superchat-authority-confirm request))))

(defun journey-chat--complete-choice (text thunk)
  "Run THUNK's real completion path and select TEXT in batch Emacs.

`completing-read-function' is Emacs's documented input dispatch variable.
Batch Emacs has no live minibuffer for setup hooks, so this models choosing a
visible completion without replacing any Superchat function.  The shim fails
loudly unless TEXT is genuinely offered by the caller's own COLLECTION
(respecting PREDICATE) and the caller actually requires a match, so a broken
candidate-assembly path cannot stay green by returning an unoffered model."
  (let ((completing-read-function
         (lambda (_prompt collection &optional predicate require-match
                          &rest _arguments)
           (unless require-match
             (error "Journey completion %S did not require a match" text))
           (unless (member text (all-completions text collection predicate))
             (error "Journey completion %S was not offered by the collection"
                    text))
           text)))
    (funcall thunk)))

(defun journey-chat--read-string (text thunk)
  "Run THUNK while supplying TEXT to its real `read-string' interaction.

The narrow input shim is needed only because batch Emacs reads an unavailable
stdin instead of activating a minibuffer.  THUNK remains the actual Superchat
command and all of its Runtime and dispatch behavior remains real."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _arguments) text)))
    (funcall thunk)))

(ert-deftest journey/c1a-require-writes-nothing-under-home ()
  "Loading Superchat leaves a fresh HOME untouched."
  ;; `journey-fixture-load-does-not-write-home-p' snapshots the whole temp
  ;; HOME tree (00b-fix-1 item 6); a plain `require' of Superchat does not
  ;; currently write under HOME.  The runner has already loaded Superchat,
  ;; so this assertion uses an isolated batch Emacs with its own HOME and
  ;; waits for it to exit.  A temporary sabotage write inside the
  ;; predicate's child process (verified manually, not committed) reliably
  ;; turns this red.
  (should (journey-fixture-load-does-not-write-home-p)))

(ert-deftest journey/c1-new-conversation ()
  "J1: a fresh view has its own Session, offers recent Sessions, and
gives visible guidance when no backend is configured."
  (journey-fixture-with ()
    (let* ((prior-session-ids
            (cl-loop for index from 0 below 5
                     collect (format "journey-c1-prior-session-%02d-%06x"
                                     index (random #x1000000)))))
      (dolist (session-id prior-session-ids)
        (superchat-tape-append-material
         session-id "user"
         (format "JOURNEY_C1_PRIOR_SESSION_QUESTION_%s" session-id)))
      (let* ((first (journey-fixture-open-chat))
             (first-session (journey-fixture-session-id first))
             (second (superchat-new-session))
             (second-session (journey-fixture-session-id second)))
        (with-current-buffer first
          (should (derived-mode-p 'org-mode))
          (should superchat-mode)
          (should (string-match-p "^#\\+TITLE: superchat" (buffer-string)))
          (should (string-match-p "\\* User: " (buffer-string)))
          ;; J1 UI: the fresh view offers the recent Sessions so returning to
          ;; one does not require search or memory of its identifier.
          (should (string-match-p "\\* Recent sessions" (buffer-string)))
          (dolist (session-id prior-session-ids)
            (should (string-match-p (regexp-quote session-id)
                                    (buffer-string)))))
        (should (stringp first-session))
        (should (stringp second-session))
        (should-not (equal first-session second-session))
        (should-not (journey-fixture-events first-session))
        ;; J1 Fail case: an unconfigured backend must give visible guidance
        ;; in the fresh view itself, not only after a send is attempted.
        (let* ((superchat-llm-backend nil)
               (third (superchat-new-session)))
          (with-current-buffer third
            (should (string-match-p "is not configured" (buffer-string)))))))))

(ert-deftest journey/c2-send-answer-streaming ()
  "C2 sends visible text/history and streams both declared and local models."
  (journey-fixture-with
      (:model "qwen/qwen3.8-27b"
       :tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           ;; Fresh per-test so an override declared below never leaks into
           ;; a later test's default value.
           (superchat-llm-model-capability-overrides nil)
           partial-visible)
      (journey-fixture-set-script
       (list (list :chunks '("KNOWN" "KNOWN ANSWER")
                   :text "KNOWN ANSWER"
                   ;; Captured after the real partial callbacks but before the
                   ;; final result is processed, so this can only see streamed
                   ;; chunk text, not the eventual final-text delivery.
                   :after-chunks
                   (lambda (_state)
                     (setq partial-visible
                           (with-current-buffer view (buffer-string)))))))
      (journey-fixture-send "first visible user message" view)
      (should (journey-fixture-wait-for-run view))
      (should (string-match-p "KNOWN ANSWER" partial-visible))
      (let ((known (journey-fixture-last-request)))
        (should (eq (plist-get known :mode) 'streaming))
        (should (plist-get known :tools))
        (should (string-match-p "first visible user message"
                                (plist-get known :prompt-text)))
        (should (string-match-p "KNOWN ANSWER"
                                (with-current-buffer view (buffer-string)))))
      (journey-fixture-set-provider-model "unrecognised-local-model")
      (journey-fixture-set-script
       (list (list :chunks '("LOCAL" "LOCAL ANSWER") :text "LOCAL ANSWER")))
      (journey-fixture-send "second visible user message" view)
      (should (journey-fixture-wait-for-run view))
      (let ((local (journey-fixture-last-request)))
        (should (eq (plist-get local :mode) 'streaming))
        (should (string-match-p "second visible user message"
                                (plist-get local :prompt-text)))
        (should (string-match-p "KNOWN ANSWER"
                                (plist-get local :prompt-text))))
      ;; A user-declared model capability overrides both the provider's own
      ;; report and the adapter default.  `llm-deepseek' is a distinct
      ;; provider type from `llm-openai-compatible' (not `eq'), so an
      ;; unrecognised model on it gets no adapter entry and genuinely falls
      ;; back to non-streaming until the user declares otherwise.
      (setq superchat-llm-backend
            (make-llm-deepseek :key "x" :chat-model "unknown-local"))
      (journey-fixture-set-script
       (list (list :text "FALLBACK ANSWER")))
      (let ((run-id (journey-fixture-send "third visible user message" view)))
        (should (journey-fixture-wait-for-run view))
        (let ((fallback-request (journey-fixture-last-request)))
          (should (eq (plist-get fallback-request :mode) 'async))
          (should (eq (plist-get (superchat-runtime--entry run-id)
                                  :capability-source)
                      'fallback))))
      (superchat-llm-set-model-capabilities
       "unknown-local" '(:streaming t :streaming-tool-use t))
      (journey-fixture-set-script
       (list (list :chunks '("OVERRIDE" "OVERRIDE ANSWER")
                   :text "OVERRIDE ANSWER")))
      (let ((run-id (journey-fixture-send "fourth visible user message" view)))
        (should (journey-fixture-wait-for-run view))
        (let ((override-request (journey-fixture-last-request)))
          (should (eq (plist-get override-request :mode) 'streaming))
          (should (eq (plist-get (superchat-runtime--entry run-id)
                                  :capability-source)
                      'override))))
      (should (journey-chat--types-in-order-p
               (journey-fixture-event-types session)
               '("intent.received" "run.started" "response.completed"
                 "run.completed" "turn.completed"))))))

(ert-deftest journey/c3-stop-cleans-view-and-tape ()
  "C3 cancellation leaves partial text, a stopped marker, and a clean prompt."
  (journey-fixture-with
      (:script (list (list :chunks '("partial answer") :hang t)))
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (run-id (journey-fixture-send "tell a long story" view)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "partial answer" (buffer-string))))))
      ;; This is the real C-c C-k command path.  The batch-only input shim
      ;; supplies the empty redirect draft after cancellation.
      (journey-chat--read-string
       "" (lambda () (with-current-buffer view (superchat-cancel))))
      (should (journey-fixture-wait-for-run view))
      (let ((types (journey-fixture-event-types session))
            (status (plist-get (superchat-runtime-status run-id) :status))
            (text (with-current-buffer view (buffer-string))))
        (should (journey-fixture-cancellations))
        (should (eq status 'cancelled))
        (should (member "run.cancelled" types))
        (should (member "turn.completed" types))
        (should-not (member "response.completed" types))
        (should (string-match-p "partial answer" text))
        (should (string-match-p "\\(?:[Ss]topped\\|已停止\\)" text))
        (should-not (string-match-p "Prefill / waiting" text))))))

(ert-deftest journey/c4-regenerate-and-edit-resend-drop-replaced-turns ()
  "C4 regenerate and edit-and-resend replace the previous Turn.
Regenerate leaves exactly one `* User:' row for the question, sends the
question once in the new request without the replaced turn's material, and
never touches the prompt's draft; compaction's own Context View excludes it
too.  Edit-and-resend refuses over unsent text, then returns the old
question to the prompt; the next ordinary send carries the edited text and
neither replaced text, and a rebuilt View hides the replaced rows.
Boundaries: both commands refuse during an active Run, regenerate refuses a
`/command' target before any Tape or View change, and an unreadable
supersession index omits history rather than assembling it unexcluded."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (question "C4_OLD_QUESTION")
           (old-answer "C4_OLD_ANSWER")
           (new-answer "C4_NEW_ANSWER")
           (kept-question "C4_KEPT_QUESTION")
           (kept-answer "C4_KEPT_ANSWER")
           ;; The two long answers make the 30% raw tail of the compaction
           ;; budget deterministic: each alone exceeds it, so only the newest
           ;; small rows stay raw and the older material is compacted.
           (kept-answer-text (concat kept-answer (make-string 3000 ?k)))
           (old-answer-text (concat old-answer (make-string 3000 ?y)))
           (edited "C4_EDITED_QUESTION")
           (edited-answer "C4_EDITED_ANSWER")
           (draft "C4_KEPT_DRAFT")
           (plain-system "C4_PLAIN_SYSTEM_MATERIAL")
           plain-system-id config-id old-answer-id kept-answer-id tool-row-ids)
      ;; A completed earlier Turn must stay in context while the replaced one
      ;; leaves, so the exclusion is selective rather than an empty history.
      (journey-fixture-set-script (list (list :text kept-answer-text)))
      (journey-fixture-send kept-question view)
      (should (journey-fixture-wait-for-run view))
      (setq kept-answer-id
            (car (cl-find kept-answer
                          (superchat-view-entries session nil '("assistant"))
                          :key (lambda (row) (nth 3 row))
                          :test (lambda (needle content)
                                  (string-prefix-p needle content)))))
      ;; The replaced turn carries tool rows; a plain (non-runtime-event)
      ;; System material row and a Session configuration annotation follow it.
      (journey-fixture-set-script
       (list (list :tool-uses '(("list_buffers" . ())))
             (list :text old-answer-text)))
      (journey-fixture-send question view)
      (should (journey-fixture-wait-for-run view))
      (setq old-answer-id
            (car (cl-find old-answer
                          (superchat-view-entries session nil '("assistant"))
                          :key (lambda (row) (nth 3 row))
                          :test (lambda (needle content)
                                  (string-prefix-p needle content))))
            tool-row-ids
            (mapcar #'car (superchat-view-entries
                           session nil '("tool_call" "tool_result")))
            plain-system-id
            (superchat-tape-append-material session "system" plain-system)
            config-id
            (superchat-tape-append-event
             (list :type "capability.activated" :schema 1
                   :session-id session :actor-id "test"
                   :payload (list :name "chat" :source "builtin")
                   :summary "capability.activated: chat")))
      (should old-answer-id)
      (should tool-row-ids)
      ;; Regenerate never touches the prompt or its draft.
      (journey-fixture-set-script (list (list :text new-answer)))
      (with-current-buffer view
        (goto-char (point-max))
        (insert draft)
        (superchat-regenerate))
      (should (journey-fixture-wait-for-run view))
      (let ((prompt-text (plist-get (journey-fixture-last-request) :prompt-text)))
        (should (string-match-p (regexp-quote question) prompt-text))
        (should (string-match-p (regexp-quote kept-answer) prompt-text))
        (should-not (string-match-p (regexp-quote old-answer) prompt-text))
        (should-not (string-match-p (regexp-quote plain-system) prompt-text))
        (should (= 1 (journey-chat--count-substring question prompt-text))))
      ;; The whole replaced-turn range is superseded -- answer, tool rows and
      ;; the plain System material row -- while the configuration annotation
      ;; stays for Session configuration replay.
      (let ((superseded (superchat-view-superseded-storage-ids session)))
        (should (memql old-answer-id superseded))
        (should (memql plain-system-id superseded))
        (should (cl-every (lambda (id) (memql id superseded)) tool-row-ids))
        (should-not (memql config-id superseded)))
      (with-current-buffer view
        (should (string-match-p (regexp-quote new-answer) (buffer-string)))
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward
                  (concat "^\\* User[^\n]*" (regexp-quote question)) nil t)
            (setq count (1+ count)))
          (should (= 1 count)))
        (should (equal draft
                       (buffer-substring-no-properties superchat--prompt-start
                                                       (point-max))))
        ;; The replaced answer is dimmed, not deleted.
        (goto-char (point-min))
        (should (search-forward old-answer nil t))
        (should (eq 'superchat-superseded-face
                    (get-text-property (match-beginning 0) 'face))))
      ;; A non-empty prompt refuses edit-and-resend and changes nothing.
      (let ((before (with-current-buffer view (buffer-string)))
            (events (length (journey-fixture-events session))))
        (let ((err (should-error
                    (with-current-buffer view (superchat-edit-last-message))
                    :type 'user-error)))
          (should (string-match-p "unsent text" (error-message-string err))))
        (should (equal before (with-current-buffer view (buffer-string))))
        (should (= events (length (journey-fixture-events session)))))
      ;; Clear the draft, take the last question back, and send an edit.
      (with-current-buffer view
        (goto-char superchat--prompt-start)
        (delete-region (point) (point-max))
        (superchat-edit-last-message)
        (should (equal question
                       (buffer-substring-no-properties superchat--prompt-start
                                                       (point-max))))
        ;; The replaced question row is dimmed too.
        (goto-char (point-min))
        (should (search-forward question nil t))
        (should (eq 'superchat-superseded-face
                    (get-text-property (match-beginning 0) 'face)))
        (goto-char superchat--prompt-start)
        (delete-region (point) (point-max))
        (insert edited))
      (journey-fixture-set-script (list (list :text edited-answer)))
      (with-current-buffer view
        (superchat-send-input))
      (should (journey-fixture-wait-for-run view))
      (let ((prompt-text (plist-get (journey-fixture-last-request) :prompt-text)))
        (should (string-match-p (regexp-quote edited) prompt-text))
        (should (= 1 (journey-chat--count-substring edited prompt-text)))
        (should (string-match-p (regexp-quote kept-answer) prompt-text))
        (should-not (string-match-p (regexp-quote question) prompt-text))
        (should-not (string-match-p (regexp-quote old-answer) prompt-text)))
      ;; A rebuilt View hides every superseded row and keeps the rest.
      (let* ((superseded (superchat-view-superseded-storage-ids session))
             (rows (superchat-view-session-history session))
             (contents (mapcar (lambda (row) (or (nth 3 row) "")) rows)))
        (should superseded)
        (should (cl-some (lambda (content)
                           (string-prefix-p kept-answer content))
                         contents))
        (should-not (cl-some
                     (lambda (row) (memql (nth 0 row) superseded))
                     rows)))
      ;; An unreadable supersession index omits history for that Turn while
      ;; the current input still goes out -- never a stale context.
      (journey-fixture-set-script (list (list :text "C4_SCAN_ANSWER")))
      (let ((messages-before (with-current-buffer (messages-buffer) (point-max)))
            (real-entries (symbol-function 'superchat-view-entries)))
        (cl-letf (((symbol-function 'superchat-view-entries)
                   (lambda (session-id &optional since-id kinds limit)
                     (if (equal kinds '("system"))
                         (error "injected supersession-index read failure")
                       (funcall real-entries session-id since-id kinds limit)))))
          (journey-fixture-send "C4_SCAN_QUESTION" view)
          (should (journey-fixture-wait-for-run view)))
        (should (> (journey-chat--count-messages-matching
                    "history omitted" messages-before
                    (with-current-buffer (messages-buffer) (point-max)))
                   0)))
      (let ((prompt-text (plist-get (journey-fixture-last-request) :prompt-text)))
        (should (string-match-p "C4_SCAN_QUESTION" prompt-text))
        (should-not (string-match-p (regexp-quote kept-answer) prompt-text))
        (should-not (string-match-p (regexp-quote old-answer) prompt-text)))
      ;; Real compaction chain: an overflow after the regenerate must not send
      ;; or anchor the replaced rows.  A 500-char budget makes the 30% raw tail
      ;; deterministic for this real session (Tape caps a runtime response at
      ;; 500 chars): the newest turn stays raw and the older kept/replaced rows
      ;; are compacted.
      (require 'superchat-compact)
      (let ((original-max-chars superchat-context-max-chars)
            (summary-token (format "C4_COMPACT_SUMMARY_%06x" (random #x1000000))))
        ;; The fixture delivers rounds from timers, which run outside any
        ;; dynamic binding, so the compaction budget is set globally here and
        ;; restored below rather than let-bound.
        (unwind-protect
            (progn
              (setq superchat-context-max-chars 500)
              (journey-fixture-set-script
               (list (list :error '(error . "maximum context length exceeded"))
                     (list :text (format "%s\n[[SUPERCHAT-COMPACTION-COMPLETE]]"
                                         summary-token))
                     (list :text "C4_COMPACTED_RETRY")))
              (journey-fixture-send "C4_OVERFLOW_QUESTION" view)
              (should (journey-fixture-wait-for-run view 8))
              (let* ((requests (last (journey-fixture-requests) 3))
                     (overflow-request (nth 0 requests))
                     (compaction-prompt (plist-get (nth 1 requests) :prompt-text))
                     (retry-prompt (plist-get (nth 2 requests) :prompt-text))
                     (anchor-rows
                      (seq-filter
                       (lambda (row)
                         (and (equal (nth 2 row) "anchor")
                              (equal (nth 1 row) "compact")))
                       (superchat-view-entries session)))
                     (anchor (car (last anchor-rows)))
                     (anchor-meta
                      (and anchor
                           (ignore-errors
                             (let ((json-object-type 'alist)
                                   (json-array-type 'list)
                                   (json-key-type 'symbol))
                               (json-read-from-string (nth 4 anchor))))))
                     (source-ids
                      (and anchor-meta
                           (append (alist-get 'source_ids anchor-meta) nil))))
                (should (= 3 (length requests)))
                (should (string-match-p "C4_OVERFLOW_QUESTION"
                                        (plist-get overflow-request :prompt-text)))
                (should (string-match-p "C4_COMPACTED_RETRY"
                                        (with-current-buffer view (buffer-string))))
                ;; The compaction really read the older history it summarizes...
                (should (string-match-p (regexp-quote kept-answer)
                                        compaction-prompt))
                ;; ...but neither the replaced question nor the replaced answer.
                (should-not (string-match-p (regexp-quote question)
                                            compaction-prompt))
                (should-not (string-match-p (regexp-quote old-answer)
                                            compaction-prompt))
                (should (string-match-p (regexp-quote summary-token)
                                        retry-prompt))
                (should (string-match-p (regexp-quote summary-token)
                                        (or (nth 3 anchor) "")))
                ;; The Anchor's provenance holds the kept row, never a
                ;; superseded id.
                (should source-ids)
                (should (memql kept-answer-id source-ids))
                (should-not
                 (cl-some (lambda (id)
                            (memql id (superchat-view-superseded-storage-ids
                                       session)))
                          source-ids))))
          (setq superchat-context-max-chars original-max-chars)))
      ;; A supersession-index read failure fails the compaction instead of
      ;; summarizing unexcluded history: the overflowing Run ends `failed'.
      (journey-fixture-set-script
       (list (list :error '(error . "maximum context length exceeded"))))
      (let ((real-entries (symbol-function 'superchat-view-entries)))
        (cl-letf (((symbol-function 'superchat-view-entries)
                   (lambda (session-id &optional since-id kinds limit)
                     (if (equal kinds '("system"))
                         (error "injected supersession-index read failure")
                       (funcall real-entries session-id since-id kinds limit)))))
          (let ((run-id (journey-fixture-send "C4_OVERFLOW_FAIL_QUESTION" view)))
            (should (journey-fixture-wait-for-run view 8))
            (should (eq 'failed
                        (plist-get (superchat-runtime-status run-id)
                                   :status))))))
      ;; Both commands refuse while a Run is active, changing nothing.
      (journey-fixture-set-script
       (list (list :chunks '("C4_HANG_PARTIAL") :hang t)))
      (let ((run-id (journey-fixture-send "C4_HANG_QUESTION" view)))
        (should run-id)
        (should (journey-fixture-wait-until
                 (lambda ()
                   (with-current-buffer view
                     (string-match-p "C4_HANG_PARTIAL" (buffer-string))))))
        (let ((before (with-current-buffer view (buffer-string)))
              (faces (journey-chat--superseded-face-count view))
              (superseded-before
               (length (superchat-view-superseded-storage-ids session))))
          (should-error (with-current-buffer view (superchat-regenerate))
                        :type 'user-error)
          (should-error (with-current-buffer view (superchat-edit-last-message))
                        :type 'user-error)
          (should (equal before (with-current-buffer view (buffer-string))))
          (should (= faces (journey-chat--superseded-face-count view)))
          (should (= superseded-before
                     (length (superchat-view-superseded-storage-ids session)))))
        (journey-chat--read-string
         "" (lambda () (with-current-buffer view (superchat-cancel))))
        (should (journey-fixture-wait-for-run view)))
      ;; Regenerate refuses a `/command' target before any Tape or View
      ;; change; edit-and-resend is allowed and returns it to the prompt.
      (let ((command-text "/clear"))
        (superchat-tape-append-material session "user" command-text)
        (superchat-tape-append-material session "assistant" "C4_COMMAND_ANSWER")
        (let ((before (with-current-buffer view (buffer-string)))
              (faces (journey-chat--superseded-face-count view))
              (events (length (journey-fixture-events session))))
          (let ((err (should-error
                      (with-current-buffer view (superchat-regenerate))
                      :type 'user-error)))
            (should (string-match-p "command" (error-message-string err))))
          (should (equal before (with-current-buffer view (buffer-string))))
          (should (= faces (journey-chat--superseded-face-count view)))
          (should (= events (length (journey-fixture-events session)))))
        (with-current-buffer view
          (superchat-edit-last-message)
          (should (equal command-text
                         (buffer-substring-no-properties
                          superchat--prompt-start (point-max)))))))))

(defun journey-chat--count-substring (needle haystack)
  "Count NEEDLE's non-overlapping literal occurrences in HAYSTACK."
  (let ((count 0) (start 0) (regexp (regexp-quote needle)))
    (while (string-match regexp haystack start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun journey-chat--superseded-face-count (buffer)
  "Return how many positions in BUFFER carry `superchat-superseded-face'."
  (with-current-buffer buffer
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((face (get-text-property (point) 'face)))
            (when (or (eq face 'superchat-superseded-face)
                      (and (listp face)
                           (memq 'superchat-superseded-face face)))
              (setq count (1+ count))))
          (forward-char)))
      count)))

(defun journey-chat--count-messages-matching (regexp start end)
  "Count REGEXP matches recorded in *Messages* between positions START and END."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char start)
      (let ((count 0))
        (while (re-search-forward regexp end t)
          (setq count (1+ count)))
        count))))

(ert-deftest journey/c5-attach-file-inlines-and-removes-token ()
  "C5 handles bare and question-bearing #/absolute/file attachments; the
file body and its \"Inlined N characters\" notice each appear exactly
once per send, even on the second send where the path is both already
tracked from the first send and explicitly re-referenced (S06b-fix-3
finding 2: `context-files' must not re-emit a path `file-inline' already
owns for the current Turn)."
  (journey-fixture-with ()
    (let* ((file (expand-file-name "attachment.txt"
                                   (journey-fixture-state-data-directory
                                    journey-fixture--state)))
           (content "JOURNEY_FILE_CONTENT_42\nsecond line\n")
           (sentinel "JOURNEY_FILE_CONTENT_42")
           (notice-regexp "superchat: Inlined [0-9]+ characters from")
           (view (journey-fixture-open-chat))
           marker)
      (with-temp-file file (insert content))
      (journey-fixture-set-script
       (list (list :text "bare ok") (list :text "question ok")))
      (setq marker (with-current-buffer (messages-buffer) (point-max)))
      (journey-fixture-send (concat "#" file) view)
      (should (journey-fixture-wait-for-run view))
      (let ((next (with-current-buffer (messages-buffer) (point-max))))
        (should (= 1 (journey-chat--count-messages-matching
                      notice-regexp marker next)))
        (setq marker next))
      (journey-fixture-send (concat "#" file " summarize it") view)
      (should (journey-fixture-wait-for-run view))
      (should (= 1 (journey-chat--count-messages-matching
                    notice-regexp marker
                    (with-current-buffer (messages-buffer) (point-max)))))
      (let ((requests (journey-fixture-requests)))
        (should (= 2 (length requests)))
        (dolist (request requests)
          (let ((prompt-text (plist-get request :prompt-text)))
            (should (= 1 (journey-chat--count-substring sentinel prompt-text)))
            (should-not (string-match-p (regexp-quote (concat "#" file))
                                        prompt-text))))))))

(ert-deftest journey/c6-attach-region-keeps-source-and-origin ()
  "C6 adds project/origin context without changing a fileless source buffer."
  (journey-fixture-with ()
    (let* ((root (file-name-as-directory
                  (expand-file-name
                   "fileless-project"
                   (journey-fixture-state-data-directory journey-fixture--state))))
           (view (journey-fixture-open-chat))
           (source (generate-new-buffer " *journey-fileless-source*"))
           (token "JOURNEY_REGION_CODE_42")
           before prompt after)
      ;; A fileless source buffer must get a real project root even when the
      ;; checkout is not a VC working tree, so build a temporary project the
      ;; same way the file-visiting half below does.
      (make-directory (expand-file-name ".git" root) t)
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq default-directory root)
              (emacs-lisp-mode)
              (insert (format "(defun journey-region () %S)\n" token))
              (set-buffer-modified-p nil)
              (setq before (journey-fixture-buffer-snapshot source)))
            (journey-fixture-set-script (list (list :text "region answer")))
            (with-current-buffer source
              (superchat-send-region (point-min) (point-max) "explain this"))
            (should (journey-fixture-wait-for-run view))
            (setq prompt (plist-get (journey-fixture-last-request) :prompt-text)
                  after (journey-fixture-buffer-snapshot source))
            (should (equal before after))
            (should (string-match-p token prompt))
            (should (string-match-p
                     (regexp-quote (format ";; project: %s" root))
                     prompt))
            (should (equal superchat-tools-default-directory root))
            (should-not (string-match-p ";; file: \\." prompt))
            ;; A file-visiting buffer under its own temporary project must
            ;; emit the real relative `;; file:' header, not just omit the
            ;; fileless one.
            (let* ((project-dir
                    (file-name-as-directory
                     (expand-file-name
                      "file-project"
                      (journey-fixture-state-data-directory
                       journey-fixture--state))))
                   (file (expand-file-name "notes.txt" project-dir))
                   (file-token "JOURNEY_FILE_REGION_CODE_77")
                   file-buffer file-prompt)
              (make-directory (expand-file-name ".git" project-dir) t)
              (with-temp-file file (insert (format "%s\n" file-token)))
              (setq file-buffer (find-file-noselect file))
              (journey-fixture-set-script
               (list (list :text "file region answer")))
              (with-current-buffer file-buffer
                (superchat-send-region (point-min) (point-max)
                                       "explain this file"))
              (should (journey-fixture-wait-for-run view))
              (setq file-prompt
                    (plist-get (journey-fixture-last-request) :prompt-text))
              (should (string-match-p file-token file-prompt))
              (should (string-match-p (regexp-quote ";; file: notes.txt")
                                      file-prompt))
              (let ((kill-buffer-query-functions nil))
                (kill-buffer file-buffer))))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest journey/c6b-dired-attach-inlines-text-file ()
  "C6b carries a Dired-selected text file into the next model prompt."
  (journey-fixture-with ()
    (require 'dired)
    (let* ((directory (journey-fixture-state-data-directory journey-fixture--state))
           (file (expand-file-name "dired-attachment.txt" directory))
           (content "JOURNEY_DIRED_CONTENT_77\n")
           (view (journey-fixture-open-chat))
           dired-buffer)
      (with-temp-file file (insert content))
      (setq dired-buffer (dired-noselect directory))
      (with-current-buffer dired-buffer
        (dired-goto-file file)
        (superchat-dired-send))
      (journey-fixture-set-script (list (list :text "dired answer")))
      (journey-fixture-send "what is in the attached file?" view)
      (should (journey-fixture-wait-for-run view))
      (should (string-match-p content
                              (plist-get (journey-fixture-last-request)
                                         :prompt-text))))))

(ert-deftest journey/c7-switch-model-is-one-shot-and-global ()
  "C7 @model affects one request and the model picker changes later requests."
  (journey-fixture-with
      (:model "fixture-default-model"
       :script (list (list :text "alternate")
                     (list :text "default")
                     (list :text "global")))
    (let ((view (journey-fixture-open-chat)))
      (journey-fixture-send "@alternate-model use this once" view)
      (should (journey-fixture-wait-for-run view))
      (should (equal "alternate-model"
                     (plist-get (journey-fixture-last-request) :model)))
      (journey-fixture-send "back to provider default" view)
      (should (journey-fixture-wait-for-run view))
      (should (equal "fixture-default-model"
                     (plist-get (journey-fixture-last-request) :model)))
      (let ((superchat-manual-models '("global-model")))
        (journey-chat--complete-choice
         "global-model"
         (lambda ()
           (with-current-buffer view (superchat--transient-switch-model)))))
      (should (equal superchat-llm-model "global-model"))
      (journey-fixture-send "use the selected global model" view)
      (should (journey-fixture-wait-for-run view))
      (should (equal "global-model"
                     (plist-get (journey-fixture-last-request) :model))))))

(ert-deftest journey/c8-read-buffer-is-unprompted-but-audited ()
  "C8 read_buffer avoids approval while still producing audited tool events."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (target (generate-new-buffer " *journey-read-target*"))
           (approvals 0)
           (superchat-authority-approval-function
            (lambda (_request) (setq approvals (1+ approvals)) t)))
      (with-current-buffer target (insert "JOURNEY_READ_BUFFER_CONTENT"))
      (journey-fixture-set-script
       (list (list :tool-uses
                   `(("read_buffer" . ((buffer . ,(buffer-name target))))))
             (list :text "I read the requested buffer.")))
      (journey-fixture-send "read the target buffer" view)
      (should (journey-fixture-wait-for-run view))
      (let ((types (journey-fixture-event-types session))
            (second-request (nth 1 (journey-fixture-requests))))
        (should (= 0 approvals))
        (should (journey-chat--types-in-order-p
                 types '("tool.requested" "tool.execution.started" "tool.completed")))
        (should-not (member "approval.requested" types))
        (should-not (member "approval.decided" types))
        (should (string-match-p "JOURNEY_READ_BUFFER_CONTENT"
                                (plist-get second-request :prompt-text)))))))

(ert-deftest journey/c9-edit-buffer-allow-once ()
  "C9 shows a real approval, applies exactly one edit, and audits it."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (target (generate-new-buffer " *journey-edit-target*"))
           (approvals nil)
           (superchat-authority-approval-function
            (journey-chat--confirm-with-key ?y
                                             (lambda (request)
                                               (push request approvals)))))
      (with-current-buffer target (insert "alpha beta gamma"))
      (journey-fixture-set-script
       (list
        (list :tool-uses
              `(("edit_buffer" . ((buffer_name . ,(buffer-name target))
                                   (old_string . "beta")
                                   (new_string . "BETA")))))
        (list :text "Done.")))
      (journey-fixture-send "replace beta in the target" view)
      (should (journey-fixture-wait-for-run view))
      (should (= 1 (length approvals)))
      (should (equal "alpha BETA gamma"
                     (with-current-buffer target (buffer-string))))
      (let ((types (journey-fixture-event-types session))
            (second-request (nth 1 (journey-fixture-requests)))
            (text (with-current-buffer view (buffer-string))))
        (should (journey-chat--types-in-order-p
                 types '("tool.requested" "approval.requested" "approval.decided"
                         "tool.execution.started" "tool.completed")))
        (should (string-match-p "Successfully edited buffer"
                                (plist-get second-request :prompt-text)))
        (should (string-match-p "Tool call: edit_buffer" text))
        (should (string-match-p "Tool result:" text))))))

(ert-deftest journey/c10-approvals-a-n-and-quit ()
  "C10 exercises session allow, denial feedback, and C-g accounting."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((target (generate-new-buffer " *journey-approval-target*"))
           (view-a (journey-fixture-open-chat))
           (approvals-a nil)
           (superchat-authority-approval-function
            (journey-chat--confirm-with-key ?a
                                             (lambda (request)
                                               (push request approvals-a)))))
      (with-current-buffer target (insert "alpha beta gamma"))
      (journey-fixture-set-script
       (list
        (list :tool-uses
              `(("edit_buffer" . ((buffer_name . ,(buffer-name target))
                                   (old_string . "alpha") (new_string . "ALPHA")))))
        (list :tool-uses
              `(("edit_buffer" . ((buffer_name . ,(buffer-name target))
                                   (old_string . "gamma") (new_string . "GAMMA")))))
        (list :text "Both edits completed.")))
      (journey-fixture-send "uppercase alpha and gamma" view-a)
      (should (journey-fixture-wait-for-run view-a))
      (should (= 1 (length approvals-a)))
      (should (equal "ALPHA beta GAMMA"
                     (with-current-buffer target (buffer-string))))
      (with-current-buffer view-a
        (should (member "edit_buffer" superchat-authority--session-allowlist)))
      ;; A new Session proves n is independent of the earlier session allow.
      (let* ((view-n (superchat-new-session))
             (session-n (journey-fixture-session-id view-n))
             (approvals-n nil)
             (superchat-authority-approval-function
              (journey-chat--confirm-with-key ?n
                                               (lambda (request)
                                                 (push request approvals-n)))))
        (journey-fixture-set-script
         (list
          (list :tool-uses
                `(("edit_buffer" . ((buffer_name . ,(buffer-name target))
                                     (old_string . "beta") (new_string . "zzz")))))
          (list :text "I will not retry that edit.")))
        (journey-fixture-send "replace beta with zzz" view-n)
        (should (journey-fixture-wait-for-run view-n))
        (should (= 1 (length approvals-n)))
        (should (equal "ALPHA beta GAMMA"
                       (with-current-buffer target (buffer-string))))
        (should (member "tool.failed" (journey-fixture-event-types session-n)))
        (should (string-match-p "do not retry"
                                (plist-get (journey-fixture-last-request)
                                           :prompt-text))))
      ;; C-g is deliberately represented by the same public approval
      ;; customization signaling `quit' as `read-multiple-choice' does.
      (let* ((view-q (superchat-new-session))
             (session-q (journey-fixture-session-id view-q))
             (run-id nil)
             (superchat-authority-approval-function
              (lambda (_request) (signal 'quit nil))))
        (journey-fixture-set-script
         (list
          (list :tool-uses
                `(("edit_buffer" . ((buffer_name . ,(buffer-name target))
                                     (old_string . "beta") (new_string . "quitted")))))))
        (setq run-id (journey-fixture-send "try then quit approval" view-q))
        (condition-case nil
            (journey-fixture-wait-until (lambda () nil) 0.2)
          (quit nil))
        (condition-case nil
            (journey-fixture-wait-until (lambda () nil) 0.1)
          (quit nil))
        (let ((types (journey-fixture-event-types session-q))
              (status (plist-get (superchat-runtime-status run-id) :status)))
          (should (member "approval.decided" types))
          (should (member "tool.failed" types))
          (should-not (eq status 'running-model)))))))

(ert-deftest journey/c11-tool-progress-is-one-collapsible-line-per-call ()
  "C11: each tool call is one collapsible heading, running -> done with its
duration; a tool round never opens a second `** Assistant:' heading."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (target (generate-new-buffer " *journey-c11-target*")))
      (with-current-buffer target (insert "line one\nline two"))
      (journey-fixture-set-script
       (list
        (list :tool-uses `(("read_buffer" . ((buffer . ,(buffer-name target))))))
        (list :tool-uses `(("read_buffer" . ((buffer . ,(buffer-name target))))))
        (list :text "Read it twice.")))
      (journey-fixture-send "read the target buffer twice" view)
      (should (journey-fixture-wait-for-run view))
      (with-current-buffer view
        (let ((text (buffer-string)))
          (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
          ;; S21-fix-1: both calls completed, so both headings are ✓
          ;; specifically -- not merely one of the three possible states.
          (should (= 2 (how-many "^\\*\\*\\* ✓ read_buffer"
                                 (point-min) (point-max))))
          (should (= 2 (how-many "✓ read_buffer ([0-9.]+s)"
                                 (point-min) (point-max))))
          (should-not (string-match-p "^\\*\\*\\* Tool result:" text))
          (should (string-match-p "Tool call: read_buffer" text))
          (should (string-match-p "Tool result:" text))
          (should (string-match-p "Read it twice\\." text)))))))

(ert-deftest journey/c12-steer-reaches-next-model-call ()
  "C12 pauses at a tool boundary and carries resume text to the next round."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (target (generate-new-buffer " *journey-steer-target*")))
      (with-current-buffer target (insert "JOURNEY_STEER_BUFFER"))
      (journey-fixture-set-script
       (list
        (list :tool-uses `(("read_buffer" . ((buffer . ,(buffer-name target))))))
        (list :tool-uses `(("read_buffer" . ((buffer . ,(buffer-name target))))))
        (list :text "final response")))
      (journey-fixture-send "read it twice, then answer" view)
      (with-current-buffer view (superchat-pause))
      (should
       (journey-fixture-wait-until
        (lambda ()
          (with-current-buffer view superchat--pending-send-request))))
      (journey-chat--read-string
       "STEER_TEXT_REACHES_NEXT_CALL"
       (lambda () (with-current-buffer view (superchat-resume))))
      (should (journey-fixture-wait-for-run view 8))
      (let ((follow-ups (cdr (journey-fixture-requests))))
        (should follow-ups)
        (should (string-match-p "STEER_TEXT_REACHES_NEXT_CALL"
                                (plist-get (car follow-ups) :prompt-text))))
      ;; S21-fix-1 (Spec 3c): a steered, multi-round tool turn still keeps
      ;; exactly one `** Assistant:' heading for the whole turn.
      (with-current-buffer view
        (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))))))

(ert-deftest journey/c12b-message-typed-during-run-is-queued-then-sent ()
  "D7: a message sent while ordinary chat has an active Run is queued, not
dropped, shows in the mode line, and dispatches on its own once the active
Run ends."
  (journey-fixture-with
      (:script (list (list :chunks '("JOURNEY_C12B_PARTIAL") :hang t)
                     (list :text "second answer")))
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (first-run (journey-fixture-send "first" view)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "JOURNEY_C12B_PARTIAL" (buffer-string))))))
      (journey-fixture-send "second" view)
      (should (= 1 (superchat-runtime-pending-follow-ups session)))
      (with-current-buffer view
        (should (string-match-p "queued" (or (superchat--mode-line-run-state) ""))))
      (superchat-runtime-cancel first-run "journey C12b cancel")
      (should (journey-fixture-wait-until
               (lambda () (= 2 (length (journey-fixture-requests))))))
      (should (string-match-p
               "second"
               (plist-get (car (last (journey-fixture-requests))) :prompt-text)))
      (let ((types (journey-fixture-event-types session)))
        (should (member "queue.queued" types))
        (should (member "queue.dispatched" types))))))

(ert-deftest journey/c13-cancel-cascades-to-child-run ()
  "C13 cancels a publicly enabled delegate tool's child Run with its parent."
  (journey-fixture-with
      (:tools-policy 'always)
    ;; Keep the separate Closure terminal-proof gate out of this one D5
    ;; observation: an outstanding delegate effect otherwise becomes
    ;; `audit-incomplete' before parent-to-child cancellation can be judged.
    ;; D2 exercises the same gate enabled with a plan.progress event.
    (let ((superchat-closure-terminal-guard-enabled nil))
      (require 'superchat-subagent)
      (let* ((superchat-llm-tool-names '("delegate_to_subagent"))
             (superchat-llm-tools-list nil)
             (view (journey-fixture-open-chat))
             (parent nil)
             child)
        (journey-fixture-set-script
         (list
          (list :tool-uses
                '(("delegate_to_subagent"
                   . ((preset . "researcher") (task . "inspect the task")))))
          (list :hang t)))
        (setq parent (journey-fixture-send "delegate this task" view))
        (should
         (journey-fixture-wait-until
          (lambda ()
            (setq child
                  (cl-find-if
                   (lambda (run-id)
                     (equal parent
                            (plist-get (superchat-runtime-status run-id)
                                       :parent-run-id)))
                   (superchat-runtime-active-runs))))
          4))
        (should (superchat-runtime-cancel parent))
        (should
         (journey-fixture-wait-until
          (lambda ()
            (eq 'cancelled
                (plist-get (superchat-runtime-status child) :status)))
          1))))))

(ert-deftest journey/c14-overflow-compacts-and-retries-once ()
  "C14 retries a context-overflow response after automatic compaction."
  (journey-fixture-with ()
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (filler (make-string 3000 ?x))
           ;; A distinctive, run-unique token on the earliest raw entry: it
           ;; cannot already appear anywhere else (unlike the shared FILLER,
           ;; which every entry repeats), so its absence from the retried
           ;; prompt is real evidence the raw entry was dropped, not merely
           ;; that the two prompts differ textually.
           (early-token (format "JOURNEY_C14_EARLY_%06x_%s"
                                (random #x1000000) (float-time)))
           ;; A distinctive token inside the scripted compaction answer, so
           ;; the anchor and the retried prompt can be checked for the real
           ;; summary text rather than merely differing from the original.
           (summary-token (format "JOURNEY_C14_SUMMARY_%06x_%s"
                                  (random #x1000000) (float-time))))
      (superchat-tape-append-material
       session "user" (format "Q0 %s %s" early-token filler))
      (superchat-tape-append-material session "assistant"
                                      (format "A0 %s" filler))
      (cl-loop for index from 1 below 20 do
               (superchat-tape-append-material session "user"
                                               (format "Q%d %s" index filler))
               (superchat-tape-append-material session "assistant"
                                               (format "A%d %s" index filler)))
      ;; Three rounds: the main Run's oversized attempt (rejected), the
      ;; compaction sub-request's own real provider call (must end with the
      ;; completion marker `superchat-compact--parse-completed-summary'
      ;; requires), then the main Run's retry with the compacted View.
      (journey-fixture-set-script
       (list (list :error '(error . "maximum context length exceeded"))
             (list :text (format "%s\n[[SUPERCHAT-COMPACTION-COMPLETE]]"
                                 summary-token))
             (list :text "compacted retry answer")))
      (journey-fixture-send "summarize the prior conversation" view)
      (should (journey-fixture-wait-for-run view 5))
      (let* ((requests (journey-fixture-requests))
             (first-prompt (plist-get (nth 0 requests) :prompt-text))
             (compaction-prompt (plist-get (nth 1 requests) :prompt-text))
             (retried-prompt (plist-get (nth 2 requests) :prompt-text))
             (anchor-rows
              (seq-filter
               (lambda (row)
                 (and (equal (nth 2 row) "anchor")
                      (equal (nth 1 row) "compact")))
               (superchat-view-entries session)))
             (anchor (car (last anchor-rows)))
             (summary (and anchor (nth 3 anchor)))
             (meta (and anchor
                        (ignore-errors
                          (let ((json-object-type 'alist)
                                (json-array-type 'list)
                                (json-key-type 'symbol))
                            (json-read-from-string (nth 4 anchor))))))
             (source-ids (and meta (alist-get 'source_ids meta))))
        (should (= 3 (length requests)))
        (should (string-match-p "compacted retry answer"
                                (with-current-buffer view (buffer-string))))
        ;; A blind retry of the identical oversized prompt must not pass:
        ;; require durable compaction evidence (a compact Anchor with
        ;; non-empty source provenance) and prove the retried prompt is the
        ;; compacted View, not the first prompt verbatim.
        (should anchor)
        (should source-ids)
        (should-not (equal first-prompt retried-prompt))
        (should (string-match-p (regexp-quote summary) retried-prompt))
        ;; The compaction sub-request must have actually read the old raw
        ;; history it claims to summarize, not fabricated a summary blind.
        (should (string-match-p (regexp-quote early-token) compaction-prompt))
        ;; The durable Anchor and the retried prompt must carry the real,
        ;; scripted summary text, not just any non-empty string.
        (should (string-match-p (regexp-quote summary-token) summary))
        (should (string-match-p (regexp-quote summary-token) retried-prompt))
        ;; The reviewer's sabotage (prepending the summary to the identical
        ;; oversized prompt: "COMPACT_SUMMARY\nQ0 FULL_OLD_HISTORY") passed
        ;; every assertion above because "Q0" and the summary both matched
        ;; while the entire old prompt was still embedded verbatim.  A
        ;; distinctive raw token that only the dropped early entry carries
        ;; cannot survive real compaction into the retried prompt.
        (should-not (string-match-p (regexp-quote early-token) retried-prompt))))))

(ert-deftest journey/c14b-overflow-compaction-anchor-failure-fails-main-run ()
  "A compaction Anchor write failure fails the main Run instead of stalling it.

Regression for the Critical finding in the 2026-09-11 S19 review: an error
signalled while applying a successfully compacted summary (writing its
Anchor row to Tape) used to escape `superchat-compact--finish' uncaught.
Runtime recorded it only as the *compaction* Run's own `delivery.failed'
warning; the *main*, overflowing Run's own `on-finish' -- the only thing
that could ever resume or fail it -- was never called, leaving that Run
stuck in `running-model' forever with the View showing an active Run."
  (journey-fixture-with ()
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (filler (make-string 3000 ?x))
           run-id)
      (superchat-tape-append-material session "user" (format "Q0 %s" filler))
      (superchat-tape-append-material session "assistant" (format "A0 %s" filler))
      (cl-loop for index from 1 below 20 do
               (superchat-tape-append-material session "user"
                                               (format "Q%d %s" index filler))
               (superchat-tape-append-material session "assistant"
                                               (format "A%d %s" index filler)))
      ;; Two rounds: the main Run's oversized attempt (rejected), then the
      ;; compaction sub-request's own real provider call, which completes
      ;; successfully -- the injected failure below is purely in the
      ;; Anchor Tape append that follows a *successful* compaction
      ;; response, per rule 4(b) (a deliberately injected Tape-append
      ;; failure, not a Runtime/Authority/dispatch replacement).
      (journey-fixture-set-script
       (list (list :error '(error . "maximum context length exceeded"))
             (list :text "SUMMARY\n[[SUPERCHAT-COMPACTION-COMPLETE]]")))
      (cl-letf (((symbol-function 'superchat-tape-append-compaction-anchor)
                 (lambda (&rest _args)
                   (error "INJECTED ANCHOR APPEND FAILURE"))))
        (setq run-id (journey-fixture-send "summarize the prior conversation" view))
        (should
         (journey-fixture-wait-until
          (lambda ()
            (superchat-runtime--terminal-p
             (plist-get (superchat-runtime-status run-id) :status)))
          5)))
      ;; The main Run must reach a real terminal `failed' state within the
      ;; wait -- not still `running-model' -- and the View must show no
      ;; active Run.
      (should (eq 'failed (plist-get (superchat-runtime-status run-id) :status)))
      (should-not (journey-fixture-run-active-p view))
      (should (= 2 (length (journey-fixture-requests))))
      (should
       (string-match-p "Compaction anchor application failed"
                       (with-current-buffer view (buffer-string))))
      ;; No Anchor exists on Tape: the injected failure genuinely prevented
      ;; the write, it was not silently recorded despite the error.
      (should-not
       (seq-filter
        (lambda (row) (and (equal (nth 2 row) "anchor")
                          (equal (nth 1 row) "compact")))
        (superchat-view-entries session))))))

(ert-deftest journey/c15-reopen-rebuilds-history-and-localizes-time ()
  "C15 M-x superchat after closing a view rebuilds it with local timestamps."
  (require 'superchat-tape-browser)
  (journey-fixture-with
      (:script (list (list :text "JOURNEY_REOPEN_ANSWER")))
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view)))
      (journey-fixture-send "JOURNEY_REOPEN_QUESTION" view)
      (should (journey-fixture-wait-for-run view))
      (let ((kill-buffer-query-functions nil)) (kill-buffer view))
      (let* ((reopened (superchat))
             (history-visible
              (with-current-buffer reopened
                (and (equal session superchat--session-id)
                     (string-match-p "JOURNEY_REOPEN_ANSWER" (buffer-string)))))
             (last-row (car (last (superchat-view-entries session))))
             (utc (nth 5 last-row))
             ;; `(concat utc " UTC")' is silently parsed as *local* time on
             ;; Emacs 32 (N9); an explicit zero offset is unambiguous.
             (local (format-time-string
                     "%Y-%m-%d %H:%M" (date-to-time (concat utc " +0000"))))
             (utc-minute (substring utc 0 (min 16 (length utc))))
             (browser (superchat-sessions))
             (local-time-visible
              (with-current-buffer browser
                (string-match-p (regexp-quote local) (buffer-string)))))
        ;; Form both observations before asserting so one failure report
        ;; captures both user-visible contracts rather than short-circuiting.
        (should history-visible)
        (should local-time-visible)
        ;; When the local zone differs from UTC, the localized string must
        ;; not equal the raw SQLite UTC string; otherwise localization would
        ;; be silently a no-op and the assertion above would pass for the
        ;; wrong reason.
        (unless (string= (format-time-string "%z") "+0000")
          (should-not (equal local utc-minute)))))))

(ert-deftest journey/c16-remember-recalls-across-sessions ()
  "C16 explicit memory in session A reaches a question in session B."
  (journey-fixture-with ()
    (let ((view-a (journey-fixture-open-chat)))
      (journey-fixture-send "/remember the cat is named JOURNEY_TOM" view-a)
      (let ((view-b (superchat-new-session)))
        (journey-fixture-set-script (list (list :text "JOURNEY_TOM")))
        (journey-fixture-send "what is the cat name we discussed?" view-b)
        (should (journey-fixture-wait-for-run view-b))
        (should (string-match-p "JOURNEY_TOM"
                                (plist-get (journey-fixture-last-request)
                                           :prompt-text)))))))

(ert-deftest journey/c17-eval-elisp-is-dangerous-and-once-only ()
  "C17 invokes eval_elisp only after y/n confirmation, never session allow."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (evaluated nil)
           (approvals nil)
           (superchat-authority-approval-function
            (journey-chat--confirm-dangerous-with-y
             (lambda (request) (push request approvals)))))
      ;; A previous focused ERT run must not make this test falsely green.
      (when (boundp 'journey-chat--eval)
        (makunbound 'journey-chat--eval))
      (unwind-protect
          (progn
            (journey-fixture-set-script
             (list
              (list :tool-uses
                    '(("eval_elisp"
                       . ((expression . "(setq journey-chat--eval 42)")))))
              (list :text "evaluated")))
            (journey-fixture-send "evaluate a harmless expression" view)
            (should (journey-fixture-wait-for-run view))
            (setq evaluated (bound-and-true-p journey-chat--eval))
            (should (= 1 (length approvals)))
            (should (equal evaluated 42))
            (with-current-buffer view
              (should-not
               (member "eval_elisp" superchat-authority--session-allowlist))))
        (when (boundp 'journey-chat--eval)
          (makunbound 'journey-chat--eval))))))

(ert-deftest journey/d2-plan-progress-does-not-poison-later-runs ()
  "D2 later Runs remain completed after a real plan.progress Tape event."
  (journey-fixture-with ()
    (let ((superchat-closure-terminal-guard-enabled t))
      (let* ((view (journey-fixture-open-chat))
             (session (journey-fixture-session-id view)))
        (journey-fixture-set-script (list (list :text "baseline")))
        (journey-fixture-send "baseline question" view)
        (should (journey-fixture-wait-for-run view))
        (superchat-tape-append-event-record
         (list :type "plan.progress" :schema 1 :session-id session :actor-id "lead"
               :payload (list :proposed (list :session-id session :event-id "evt-plan")
                              :step-id 1 :status "done")))
        (journey-fixture-set-script (list (list :text "after plan progress")))
        (let ((run-id (journey-fixture-send "question after plan progress" view)))
          (should (journey-fixture-wait-for-run view))
          (should (eq 'completed
                      (plist-get (superchat-runtime-status run-id) :status))))))))

(ert-deftest journey/d14-policy-visible-small-tool-set ()
  "D14 attaches exactly the configured small tool surface to ordinary chat,
chosen in exactly one place (contract §4.2, `superchat-select-tools'):
the real Run status exposes the same names and the `baseline' reason
S09-fix-1 added, and a static gate (`runtime/only-select-tools-decides-
tool-names') proves no other production file decides them."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((superchat-llm-tool-names
            '("list_buffers" "apropos" "read_buffer" "edit_buffer" "eval_elisp"))
           (superchat-llm-tools-list nil)
           (view (journey-fixture-open-chat))
           run-id)
      (journey-fixture-set-script (list (list :text "plain answer")))
      (setq run-id (journey-fixture-send "a plain conversational question" view))
      (should (journey-fixture-wait-for-run view))
      (should
       (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos"
                "eval_elisp" "ask_user")
              (plist-get (journey-fixture-last-request) :tools)))
      (let ((status (superchat-runtime-status run-id)))
        (should (eq 'baseline (plist-get status :runtime-tool-decision)))
        (should (equal (plist-get (journey-fixture-last-request) :tools)
                       (plist-get status :runtime-selected-tools)))))))

(ert-deftest journey/attempt-details-record-capability-source ()
  "Attempt details carry :capability-source, and Runtime's entry copies it,
through the real journey fixture: a real temporary Tape database, only the
provider network call replaced.  Moved here from `test/test-llm-backend.el'
(S08-fix-3 item 3): that file documents \"tests do not require llm.el to be
installed,\" so a test built on the journey fixture -- which requires
`llm'/`llm-openai' unconditionally and fails loudly by design -- belongs in
this file instead of skipping there when the dependency is absent.

`superchat-runtime--public-status' (superchat-runtime.el) filters entries to
a fixed key whitelist that does not include :capability-source, so this
reads the internal Run entry via `superchat-runtime--entry' rather than
`superchat-runtime-status'.  `journey/c2-send-answer-streaming' above already
covers the `fallback' and `override' capability sources; this test is the
only coverage of the `provider' source, which needs a backend whose exact
struct type is `llm-openai' (not the fixture's default
`llm-openai-compatible', which would resolve via `adapter' instead)."
  (journey-fixture-with
      (:script (list (list :text "cap answer")))
    (setq superchat-llm-backend
          (make-llm-openai :key "unused" :chat-model "capability-source-model"))
    (let* ((view (journey-fixture-open-chat))
           (run-id (journey-fixture-send "cap" view)))
      (should (journey-fixture-wait-for-run view))
      (should (eq 'completed
                  (plist-get (superchat-runtime-status run-id) :status)))
      (should (eq 'provider
                  (plist-get (superchat-runtime--entry run-id)
                             :capability-source))))))

(provide 'test-journeys-chat)
;;; test-journeys-chat.el ends here
