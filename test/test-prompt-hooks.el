;;; test-prompt-hooks.el — ERT tests for superchat-prompt-hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests each prompt hook function in isolation.  No buffer, no LLM, no async.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-prompt-hooks)
;; Load superchat.el to get defcustom default values
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════

(defun test-prompt-hooks--fragment (fragments id)
  "Return the fragment in FRAGMENTS whose :id is ID, or nil."
  (cl-find id fragments :key (lambda (f) (plist-get f :id)) :test #'equal))

(defmacro test-prompt-hooks--with-turn (inbound &rest body)
  "Create a fresh turn from INBOUND and execute BODY."
  (declare (indent 1))
  `(let ((turn (superchat-turn-new ,inbound "test-session")))
     ,@body))

(defun test-prompt-hooks--with-temp-db (body)
  "Run BODY with a fresh temporary Superchat database."
  (let* ((tmp-dir (make-temp-file "superchat-prompt-hooks-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (funcall body))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

;; ═══════════════════════════════════════════════════════════
;; language-instruction
;; ═══════════════════════════════════════════════════════════

(ert-deftest language-instruction/english-noop ()
  "English default: turn.system-prompt stays empty."
  (let ((superchat-lang "English"))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (equal (superchat-turn-system-prompt result) ""))))))

(ert-deftest language-instruction/chinese-appends ()
  "Chinese lang: turn.system-prompt contains language directive."
  (let ((superchat-lang "Chinese"))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (string-match-p "Chinese" (superchat-turn-system-prompt result)))))))

(ert-deftest language-instruction/empty-lang-noop ()
  "Empty lang string: no system-prompt added."
  (let ((superchat-lang ""))
    (test-prompt-hooks--with-turn "hello"
      (let ((result (superchat-prompt-hook--language-instruction turn)))
        (should (equal (superchat-turn-system-prompt result) ""))))))

(ert-deftest parallel-tools/does-not-advertise-a-hard-coded-surface ()
  "Provider-facing schemas, not this hook, own the available tool names."
  (let ((superchat-llm-tools-enabled 'always))
    (test-prompt-hooks--with-turn "hello"
      (let ((prompt
             (superchat-turn-system-prompt
              (superchat-prompt-hook--parallel-tool-calls turn))))
        (should (string-match-p "available tools" prompt))
        (should-not (string-match-p "list_buffers\\|read_buffer" prompt))))))

;; ═══════════════════════════════════════════════════════════
;; memory-context
;; ═══════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════
;; experience-context
;; ═══════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════
;; file-inline
;; ═══════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════
;; template-substitution
;; ═══════════════════════════════════════════════════════════

(ert-deftest template-substitution/dollar-input ()
  "$input in template is replaced with clean-input."
  (let ((superchat-general-answer-prompt "Answer: $input"))
    (let ((result (superchat-prompt--substitute-template "what is lisp?")))
      (should (string-match-p "what is lisp" result))
      (should-not (string-match-p "\\$input" result)))))

(ert-deftest template-substitution/dollar-lang ()
  "$lang in template is replaced."
  (let ((superchat-lang "French")
        (superchat-general-answer-prompt "Respond in $lang: $input"))
    (let ((result (superchat-prompt--substitute-template "hello")))
      (should (string-match-p "French" result))
      (should-not (string-match-p "\\$lang" result)))))

(ert-deftest template-substitution/no-vars ()
  "Template without $input: user question appended."
  (let ((superchat-general-answer-prompt "Be helpful."))
    (should (string-match-p "User question: help me"
                            (superchat-prompt--substitute-template "help me")))))

;; ═══════════════════════════════════════════════════════════
;; conversation-history
;; ═══════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════
;; Hook composition
;; ═══════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════
;; generative-ui — preset restriction consistency
;; ═══════════════════════════════════════════════════════════

(ert-deftest generative-ui/injected-when-globally-enabled ()
  "No active preset: the global allowlist alone decides."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset nil))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (let ((prompt (superchat-turn-system-prompt result)))
          (should (string-match-p "render_ui" prompt))
          (should (string-match-p "call render_ui directly" prompt))
          (should (string-match-p "Do not define_tool" prompt)))))))

(ert-deftest generative-ui/not-injected-when-globally-disabled ()
  "render_ui absent from the global allowlist: no injection."
  (let ((superchat-llm-tool-names '("read-file"))
        (superchat--active-preset nil))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-restriction-suppresses-injection ()
  "An active preset whose :tools excludes render_ui suppresses the
surface description — advertising an unreachable tool sends the model
hunting for workarounds."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "tool-demo"
                                   :tools '("define_tool"))))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-none-suppresses-injection ()
  "A preset with an explicitly empty tool set suppresses injection."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "bare" :tools 'none)))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (equal "" (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-including-render-ui-injects ()
  "A preset whose :tools includes render_ui keeps the description."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "ui" :tools '("render_ui"))))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (string-match-p "render_ui"
                                (superchat-turn-system-prompt result)))))))

(ert-deftest generative-ui/preset-inheriting-tools-injects ()
  "A preset with :tools nil inherits the global set, so injection stays."
  (require 'superchat-preset)
  (let ((superchat-llm-tool-names '("render_ui"))
        (superchat--active-preset
         (superchat-preset--create :name "plain" :tools nil)))
    (test-prompt-hooks--with-turn "draw a panel"
      (let ((result (superchat-prompt-hook--generative-ui turn)))
        (should (string-match-p "render_ui"
                                (superchat-turn-system-prompt result)))))))

;; ═══════════════════════════════════════════════════════════
;; file-inline — RLM-style metadata for large files
;; ═══════════════════════════════════════════════════════════

(ert-deftest file-inline/large-file-injects-metadata-not-content ()
  "Above the threshold, the prompt gets shape + access instructions,
never the body."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "big" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dotimes (i 2000)
              (insert (format "line %d with some padding text here\n" i))))
          (let ((superchat-inline-metadata-threshold 1024)
                (superchat-inline-file-content t))
            (test-prompt-hooks--with-turn (format "#%s summarize" path)
              (let* ((fragments (superchat-prompt-collect turn))
                     (prompt (plist-get (test-prompt-hooks--fragment
                                         fragments (format "file:%s" path))
                                        :text)))
                (should (string-match-p "NOT inlined" prompt))
                (should (string-match-p "2000 lines" prompt))
                (should (string-match-p ":START-END" prompt))
                (should-not (string-match-p "read-file" prompt))
                ;; The body must not be there: a line from the middle.
                (should-not (string-match-p "line 1500" prompt))))))
      (delete-file path))))

(ert-deftest file-inline/small-file-still-inlines ()
  "Below the threshold, behaviour is unchanged: content is inlined."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "small" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "tiny content marker-xyz\n"))
          (let ((superchat-inline-metadata-threshold 32768)
                (superchat-inline-file-content t))
            (test-prompt-hooks--with-turn (format "#%s summarize" path)
              (let* ((fragments (superchat-prompt-collect turn))
                     (prompt (plist-get (test-prompt-hooks--fragment
                                         fragments (format "file:%s" path))
                                        :text)))
                (should (string-match-p "marker-xyz" prompt))
                (should-not (string-match-p "NOT inlined" prompt))))))
      (delete-file path))))

;; ═══════════════════════════════════════════════════════════
;; superchat-prompt-collect — collect-context / assemble-prompt cutover
;; ═══════════════════════════════════════════════════════════

(ert-deftest prompt-hooks/bare-file-ref-inlines-content-and-strips-token ()
  "A bare #file ref sends file content and never the #path token itself."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "superchat-collect-bare" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "BARE_COLLECT_TOKEN_1\n"))
          (let* ((superchat--current-context-files nil)
                 (superchat--context-file-ranges nil)
                 (superchat-inline-file-content t)
                 (superchat-inline-metadata-threshold nil)
                 (turn (superchat-turn-new (format "#%s" path) "collect-bare")))
            (superchat-core--parse-input turn)
            (let* ((fragments (superchat-prompt-collect turn))
                   (attachment (cl-find-if
                                (lambda (f) (string-prefix-p "file:" (plist-get f :id)))
                                fragments))
                   (user-input (test-prompt-hooks--fragment fragments "user-input")))
              (should attachment)
              (should (string-match-p "BARE_COLLECT_TOKEN_1"
                                      (plist-get attachment :text)))
              (should user-input)
              (should-not (string-match-p (regexp-quote (format "#%s" path))
                                          (plist-get user-input :text)))
              (should-not (string-match-p (regexp-quote path)
                                          (plist-get user-input :text))))))
      (delete-file path))))

(ert-deftest prompt-hooks/file-ref-with-question-keeps-question-and-strips-token ()
  "A #file ref plus a question keeps the question and drops the token."
  (require 'superchat-dispatcher)
  (let ((path (make-temp-file "superchat-collect-q" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "QUESTION_COLLECT_TOKEN_2\n"))
          (let* ((superchat--current-context-files nil)
                 (superchat--context-file-ranges nil)
                 (superchat-inline-file-content t)
                 (superchat-inline-metadata-threshold nil)
                 (turn (superchat-turn-new (format "#%s summarize it" path)
                                           "collect-question")))
            (superchat-core--parse-input turn)
            (let* ((fragments (superchat-prompt-collect turn))
                   (attachment (cl-find-if
                                (lambda (f) (string-prefix-p "file:" (plist-get f :id)))
                                fragments))
                   (user-input (test-prompt-hooks--fragment fragments "user-input")))
              (should attachment)
              (should (string-match-p "QUESTION_COLLECT_TOKEN_2"
                                      (plist-get attachment :text)))
              (should user-input)
              (should (string-match-p "summarize it" (plist-get user-input :text)))
              (should-not (string-match-p (regexp-quote (format "#%s" path))
                                          (plist-get user-input :text))))))
      (delete-file path))))

(ert-deftest prompt-hooks/context-max-chars-limits-history-only ()
  "A small `superchat-context-max-chars' caps only the history budget
(its documented meaning, `superchat-llm.el's docstring: \"Maximum
rendered characters of recent conversation\"): a turn with a large
system prompt and a real hard attachment still assembles — no
`prompt-over-budget' — and the History collector's `:refit' clamps its
own call to that configured limit, shrinking the stored Context View
relative to what the same session produces with no limit set at all.
(A disclosure header the collector does not control is always present
in `:prompt' on top of whatever entries fit, so the *content* budget
handed to `superchat-view-assemble' — recorded verbatim as
`:context-max-chars' — is what is actually capped, not the final string
length; see `superchat-prompt-collector--history'.)"
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (superchat-db-tape-append "collect-cap" "user" (make-string 3000 ?x))
     (superchat-db-tape-append "collect-cap" "assistant" (make-string 3000 ?y))
     (let ((path (make-temp-file "superchat-collect-cap" nil ".txt")))
       (unwind-protect
           (progn
             (with-temp-file path (insert (make-string 500 ?q)))
             (let* ((superchat-lang "Chinese")
                    (superchat-llm-tools-enabled 'always)
                    (superchat-llm-tool-names '("render_ui" "define_tool"))
                    (superchat-inline-file-content t)
                    (superchat-inline-metadata-threshold nil)
                    (superchat--current-context-files nil)
                    (superchat--context-file-ranges nil)
                    (uncapped
                     (let ((superchat-context-max-chars nil))
                       (superchat-core-prepare-turn
                        (superchat-turn-new (format "#%s summarize" path)
                                            "collect-cap"))))
                    (capped
                     (let ((superchat-context-max-chars 20))
                       (superchat-core-prepare-turn
                        (superchat-turn-new (format "#%s summarize" path)
                                            "collect-cap")))))
               (should-not (superchat-turn-error uncapped))
               (should-not (superchat-turn-error capped))
               ;; The system prompt (language directive, parallel-tool
               ;; guidance, define_tool and render_ui blurbs) and the
               ;; attachment are large; none of that is squeezed out by
               ;; the 20-character `superchat-context-max-chars'.
               (should (> (length (superchat-turn-system-prompt capped)) 100))
               (should (string-match-p (regexp-quote (make-string 500 ?q))
                                       (superchat-turn-prompt capped)))
               ;; The collector's own call was capped at exactly 20 …
               (should (equal 20 (plist-get (superchat-turn-state capped)
                                            :context-max-chars)))
               ;; … which measurably shrinks the resulting Context View
               ;; against the same session with no cap at all.
               (let ((history-uncapped
                      (plist-get (plist-get (superchat-turn-state uncapped)
                                            :context-envelope)
                                 :prompt))
                     (history-capped
                      (plist-get (plist-get (superchat-turn-state capped)
                                            :context-envelope)
                                 :prompt)))
                 (should (> (length (or history-uncapped "")) 20))
                 (should (< (length (or history-capped ""))
                            (length (or history-uncapped "")))))))
         (delete-file path))))))

(ert-deftest prompt-hooks/collect-flattens-a-multi-fragment-collector ()
  "`superchat-stage-call' returns one list PER collector (contract §4.1's
`collect' kind, not a flat fragment list); `context-files' legitimately
returns several fragments from ONE call (one per attached text file).
`superchat-prompt-collect' must flatten that list of lists before adding
the core `user-input' fragment, not leave nested lists in its result."
  (let ((one (make-temp-file "superchat-multi-ctx-1" nil ".txt"))
        (two (make-temp-file "superchat-multi-ctx-2" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file one (insert "MULTI_CTX_TOKEN_ONE\n"))
          (with-temp-file two (insert "MULTI_CTX_TOKEN_TWO\n"))
          (let* ((superchat--current-context-files (list one two))
                 (turn (superchat-turn-new "what is in the files?" "collect-multi")))
            (superchat-core--parse-input turn)
            (let* ((fragments (superchat-prompt-collect turn))
                   (ctx-fragments
                    (cl-remove-if-not
                     (lambda (f) (string-prefix-p "ctx:" (plist-get f :id)))
                     fragments)))
              ;; Every element is a fragment plist (has :text), never a
              ;; list of fragments nested one level too deep.
              (dolist (f fragments)
                (should (stringp (plist-get f :text))))
              (should (= 2 (length ctx-fragments)))
              (should (cl-some (lambda (f) (string-match-p "MULTI_CTX_TOKEN_ONE"
                                                            (plist-get f :text)))
                               ctx-fragments))
              (should (cl-some (lambda (f) (string-match-p "MULTI_CTX_TOKEN_TWO"
                                                            (plist-get f :text)))
                               ctx-fragments)))))
      (delete-file one)
      (delete-file two))))

(ert-deftest prompt-hooks/context-files-skips-the-turn-explicit-file-ref ()
  "S06b-fix-3 finding 2: a Turn that explicitly re-references an already
tracked `#file' path must not get its body twice.  `context-files' must
skip exactly the path the current Turn's `#file' owns (via `file-inline'),
while still emitting a fragment for an unrelated tracked path."
  (let ((referenced (make-temp-file "superchat-ctx-skip-ref" nil ".txt"))
        (other (make-temp-file "superchat-ctx-skip-other" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file referenced (insert "CTX_SKIP_REFERENCED_TOKEN\n"))
          (with-temp-file other (insert "CTX_SKIP_OTHER_TOKEN\n"))
          (let* ((superchat--current-context-files (list referenced other))
                 (superchat--context-file-ranges nil)
                 (superchat-inline-file-content t)
                 (superchat-inline-metadata-threshold nil)
                 (turn (superchat-turn-new (format "#%s summarize it" referenced)
                                           "collect-ctx-skip")))
            (superchat-core--parse-input turn)
            (let* ((fragments (superchat-prompt-collect turn))
                   (referenced-fragments
                    (cl-remove-if-not
                     (lambda (f) (string-match-p "CTX_SKIP_REFERENCED_TOKEN"
                                                 (or (plist-get f :text) "")))
                     fragments))
                   (other-fragment
                    (cl-find-if
                     (lambda (f) (string-prefix-p "ctx:" (plist-get f :id)))
                     fragments)))
              ;; The referenced path's body appears exactly once — from
              ;; `file-inline''s hard fragment, not also from `context-files'.
              (should (= 1 (length referenced-fragments)))
              (should (string-prefix-p "file:" (plist-get (car referenced-fragments) :id)))
              ;; An unrelated tracked path is unaffected: `context-files'
              ;; still emits it.
              (should other-fragment)
              (should (string-match-p "CTX_SKIP_OTHER_TOKEN"
                                      (plist-get other-fragment :text)))
              ;; The referenced path stays tracked for later Turns.
              (should (member referenced superchat--current-context-files)))))
      (delete-file referenced)
      (delete-file other))))

(ert-deftest prompt-hooks/history-refit-receives-remaining-budget ()
  "A large hard attachment leaves less budget for history; its `:refit'
shrinks the Context View to fit, instead of overflowing the budget."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (superchat-db-tape-append "collect-refit" "user" (make-string 3000 ?x))
     (superchat-db-tape-append "collect-refit" "assistant" (make-string 3000 ?y))
     (let* ((budget 8000)
            (turn-small (superchat-turn-new "what happened?" "collect-refit"))
            (turn-big (superchat-turn-new "what happened?" "collect-refit")))
       (superchat-core--parse-input turn-small)
       (superchat-core--parse-input turn-big)
       (let ((small-fragments (superchat-prompt-collect turn-small))
             (big-fragments
              (cons (superchat-prompt-fragment
                     :owner 'test :id "big-attachment" :slot 'attachment
                     :text (make-string 7000 ?z) :priority 0 :budget-class 'hard)
                    (superchat-prompt-collect turn-big))))
         (superchat-prompt-assemble small-fragments budget)
         (superchat-prompt-assemble big-fragments budget)
         (let ((history-small
                (plist-get (plist-get (superchat-turn-state turn-small)
                                      :context-envelope)
                           :prompt))
               (history-big
                (plist-get (plist-get (superchat-turn-state turn-big)
                                      :context-envelope)
                           :prompt)))
           (should (stringp history-small))
           (should (> (length history-small) 0))
           (should (< (length (or history-big "")) (length history-small)))))))))

(ert-deftest prompt-hooks/legacy-third-party-hook-still-contributes ()
  "A third-party function left on the retired hook lists still
contributes, through the legacy bridge collector, exactly once."
  (let* ((marker "THIRD_PARTY_LEGACY_MARKER")
         (superchat-build-prompt-functions
          (list (lambda (turn)
                  (setf (superchat-turn-prompt turn)
                        (concat (superchat-turn-prompt turn) marker))
                  turn))))
    (test-prompt-hooks--with-turn "hello"
      (let* ((fragments (superchat-prompt-collect turn))
             (legacy (test-prompt-hooks--fragment fragments "legacy-prompt"))
             (occurrences 0))
        (should legacy)
        (should (string-match-p marker (plist-get legacy :text)))
        (dolist (f fragments)
          (let ((start 0)
                (text (plist-get f :text)))
            (while (string-match (regexp-quote marker) text start)
              (setq occurrences (1+ occurrences))
              (setq start (match-end 0)))))
        (should (= 1 occurrences))))))

(ert-deftest file-inline/headless-file-turn-does-not-leak-into-global-default ()
  "A `#file' Turn run outside any View (the real headless
`superchat-prompt-collect'/`superchat-core-prepare-turn' path) must not
write `superchat--current-context-files''s global default -- doing so
would silently leak the file's content into a second, unrelated View's
assembled prompt (S06b-fix-4).  This test necessarily uses two real
buffers -- one headless, one a minimal `superchat-mode' View -- because
buffer-locality is exactly what is under test."
  (test-prompt-hooks--with-temp-db
   (lambda ()
     (let* ((baseline (default-value 'superchat--current-context-files))
            (path (make-temp-file "superchat-headless-leak" nil ".txt"))
            second-buffer)
       (unwind-protect
           (progn
             (with-temp-file path (insert "S06B_FIX4_SECRET_CONTENT"))
             ;; 1. The real headless path: no View, no `superchat-mode'.
             (with-temp-buffer
               (let ((result (superchat-core-prepare-turn
                              (superchat-turn-new
                               (format "#%s inspect" path) "headless-leak-a"))))
                 (should-not (superchat-turn-error result))
                 ;; The current Turn's own hard `file:' fragment still
                 ;; carries the file -- unaffected by where the tracking
                 ;; write happens.
                 (should (string-match-p "S06B_FIX4_SECRET_CONTENT"
                                         (superchat-turn-prompt result)))))
             ;; 2. The global default must be exactly as it was before.
             (should (equal baseline (default-value 'superchat--current-context-files)))
             ;; 3. A second, unrelated View never sees the file either.
             (setq second-buffer (generate-new-buffer " *s06b-fix-4-second-view*"))
             (with-current-buffer second-buffer
               (superchat-mode 1)
               (setq-local superchat--session-id "headless-leak-b")
               (let ((result (superchat-core-prepare-turn
                              (superchat-turn-new "unrelated question"
                                                   "headless-leak-b"))))
                 (should-not (superchat-turn-error result))
                 (should-not (string-match-p "S06B_FIX4_SECRET_CONTENT"
                                             (superchat-turn-prompt result))))))
         (when (buffer-live-p second-buffer) (kill-buffer second-buffer))
         (delete-file path)
         (setq-default superchat--current-context-files baseline))))))

(provide 'test-prompt-hooks)
;;; test-prompt-hooks.el ends here
