;;; test-synthesis.el --- Agent-synthesized tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Locks the nine decisions of docs/goals/homoiconic-agent.md Phase 1.
;; Each test names the decision it defends, because every one of them
;; exists to stop a specific failure, not as ceremony.

;;; Code:

(require 'ert)
(require 'superchat)
(require 'superchat-synthesis)

(defmacro test-syn--with-approval (approve &rest body)
  "Run BODY with the confirmation preview stubbed, answering APPROVE."
  (declare (indent 1))
  `(let ((superchat-llm-synthesized-tools nil)
         (superchat--syn-names nil)
         (superchat-agent-confirm-synthesized t))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) ,approve))
               ((symbol-function 'with-help-window) (lambda (_buf &rest _) nil))
               ((symbol-function 'quit-windows-on) #'ignore))
       ,@body)))

;; ═══════════════════════════════════════════════════════
;; Decision 6 — hot registration: usable in the SAME run
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/inject-works-when-llm-loads-later ()
  "Hot injection works when `llm' loads after Superchat.

Nova adds the llm package directory after requiring Superchat, so this
subprocess deliberately defines the llm structs only after loading the
synthesis module."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (source (locate-library "superchat-synthesis"))
         (form
          (prin1-to-string
           '(progn
              (require 'cl-lib)
              (cl-defstruct llm-chat-prompt tools)
              (cl-defstruct llm-tool name)
              (let* ((tool (make-llm-tool :name "late"))
                     (prompt (make-llm-chat-prompt :tools nil)))
                (superchat--syn-inject prompt tool)
                (unless (equal (list tool) (llm-chat-prompt-tools prompt))
                  (kill-emacs 2)))))))
    (with-temp-buffer
      (let ((status (call-process emacs nil t nil
                                  "-Q" "--batch" "-l" source "--eval" form)))
        (unless (zerop status)
          (ert-fail (buffer-string)))))))

(ert-deftest syn/tool-is-callable-in-the-same-run ()
  "The agent writes a tool and calls it before the run is over.
This is the whole point: a capability that only arrives on the next user
turn is not \"a capability that grew out of the conversation\"."
  (skip-unless (fboundp 'llm-make-tool))
  (test-syn--with-approval t
    (let* ((prompt (llm-make-chat-prompt
                    "count the words"
                    :tools (list (llm-make-tool
                                  :name "define_tool"
                                  :description "write a tool"
                                  :args nil
                                  :function #'superchat-tool-define-tool))))
           (result nil))
      (superchat--syn-bind prompt nil t)
      ;; The model calls define_tool on round 1.
      (let ((dt (car (llm-chat-prompt-tools prompt))))
        (setq result
              (funcall (llm-tool-function dt)
                       "count-words" "Count words in a string"
                       "[{\"name\":\"text\",\"type\":\"string\",\"description\":\"in\"}]"
                       "(format \"%d words\" (length (split-string text)))")))
      (should (string-match-p "available now" result))
      ;; Round 2 rebuilds the request from this same slot: the tool is there.
      (let ((names (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))))
        (should (member "count-words" names)))
      ;; And it actually runs.
      (let ((tool (cl-find "count-words" (llm-chat-prompt-tools prompt)
                           :key #'llm-tool-name :test #'equal)))
        (should (string= "5 words"
                         (funcall (llm-tool-function tool)
                                  "the quick brown fox jumps")))))))

;; ═══════════════════════════════════════════════════════
;; Decisions 1 & 2 — always destructive, own switch
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/synthesized-tool-is-always-destructive ()
  "A synthesized name is not on the destructive list, so without an
explicit clause it would sail through the confirmation gate."
  (let ((superchat--syn-names '("count-words")))
    (should (superchat--agent-destructive-p "count-words" nil))
    (should-not (superchat--agent-destructive-p "read-file" nil))))

(ert-deftest syn/confirmation-does-not-follow-the-global-switch ()
  "Disabling confirmation globally says \"I trust the tools superchat
ships\".  It must not silently extend to code an LLM wrote seconds ago."
  (let ((superchat--syn-names '("count-words"))
        (superchat-agent-confirm-destructive nil)   ; user turned it off
        (superchat-agent-confirm-synthesized t))
    ;; A built-in destructive tool is now unconfirmed...
    (should-not (superchat--agent-confirm-p "shell-command" nil))
    ;; ...but the agent's own code still asks.
    (should (superchat--agent-confirm-p "count-words" nil)))
  ;; And it can be turned off, but only deliberately.
  (let ((superchat--syn-names '("count-words"))
        (superchat-agent-confirm-synthesized nil))
    (should-not (superchat--agent-confirm-p "count-words" nil))))

;; ═══════════════════════════════════════════════════════
;; Decision 7 — no shadowing, but self-redefinition
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/cannot-shadow-a-builtin-tool ()
  "An agent that redefines `read-file' to exfiltrate is a catastrophe
nobody would notice."
  (let ((superchat--syn-names nil))
    (cl-letf (((symbol-function 'superchat--syn-existing-names)
               (lambda () '("read-file" "shell-command"))))
      (should (string-match-p "cannot be redefined"
                              (superchat--syn-check-name "read-file")))
      (should-not (superchat--syn-check-name "parse-org-table")))))

(ert-deftest syn/can-redefine-its-own-earlier-tool ()
  "Iteration is the payoff of a working loop: the agent sees its tool
fail, fixes it, retries."
  (let ((superchat--syn-names '("parse-table")))
    (cl-letf (((symbol-function 'superchat--syn-existing-names)
               (lambda () '("read-file"))))
      (should-not (superchat--syn-check-name "parse-table")))))

(ert-deftest syn/rejects-malformed-names ()
  (let ((superchat--syn-names nil))
    (cl-letf (((symbol-function 'superchat--syn-existing-names) (lambda () nil)))
      (should (string-match-p "invalid tool name"
                              (superchat--syn-check-name "rm -rf /")))
      (should (string-match-p "invalid tool name"
                              (superchat--syn-check-name ""))))))

;; ═══════════════════════════════════════════════════════
;; Decision 8 — declining actually declines
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/user-declining-defines-nothing ()
  (test-syn--with-approval nil
    (let ((result (superchat--syn-define
                   nil nil t "evil" "bad" "[]" "(delete-file \"~/x\")")))
      (should (string-match-p "declined" result))
      (should-not (superchat--syn-tool-p "evil"))
      (should (null superchat-llm-synthesized-tools))
      (should-not (fboundp (superchat--syn-function-symbol "evil"))))))

;; ═══════════════════════════════════════════════════════
;; Errors come back as text the model can act on
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/bad-body-returns-an-actionable-error ()
  "A parse failure must reach the model as a result, not a signal: with
the loop working, the model can read the error and fix its own code."
  (skip-unless (fboundp 'llm-make-tool))
  (test-syn--with-approval t
    (let ((result (superchat--syn-define
                   nil nil t "broken" "x" "[]" "(this is ((unbalanced")))
      (should (string-match-p "not valid Emacs Lisp" result))
      (should-not (superchat--syn-tool-p "broken")))))

(ert-deftest syn/bad-arg-schema-returns-an-actionable-error ()
  (skip-unless (fboundp 'llm-make-tool))
  (test-syn--with-approval t
    (let ((result (superchat--syn-define
                   nil nil t "bad-args" "x"
                   "[{\"name\":\"p\",\"type\":\"hashtable\"}]" "p")))
      (should (string-match-p "unsupported type" result)))))

;; ═══════════════════════════════════════════════════════
;; Decision 5 — sub-agents synthesize into their own prompt only
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/subagent-synthesis-never-reaches-the-session ()
  "A shareable sub-agent skill must not be able to push a tool into the
main agent's toolbox.  With persist nil the tool lives in the sub-agent's
in-flight prompt and dies with it."
  (skip-unless (fboundp 'llm-make-tool))
  (test-syn--with-approval t
    (let ((prompt (llm-make-chat-prompt "t" :tools nil)))
      (superchat--syn-define prompt nil nil   ; persist = nil
                             "scratch" "sub-agent scratch tool" "[]" "42")
      ;; Usable inside this run...
      (should (member "scratch"
                      (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))))
      ;; ...and invisible to the session.
      (should (null superchat-llm-synthesized-tools)))))

;; ═══════════════════════════════════════════════════════
;; Constraint F — reload must not erase what the agent built
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/tools-reload-does-not-erase-synthesized-tools ()
  "`superchat-llm-tools-reload' rebuilds the built-in list from scratch,
and it runs whenever the agent registry changes — i.e. the next time the
user saves a skill file.  Synthesized tools must not live in that list."
  (skip-unless (fboundp 'llm-make-tool))
  (test-syn--with-approval t
    (superchat--syn-define nil nil t "keeper" "survives reload" "[]" "1")
    (should (superchat--syn-tool-p "keeper"))
    (superchat-llm-tools-reload)
    (should (member "keeper"
                    (mapcar #'llm-tool-name superchat-llm-synthesized-tools)))
    (let ((superchat-llm-tools-enabled 'always))
      (should (member "keeper"
                      (mapcar #'llm-tool-name
                              (superchat--collect-llm-tools "anything")))))))

;; ═══════════════════════════════════════════════════════
;; Decisions 4 & 9 — off by default, but discoverable when on
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/define-tool-is-off-by-default ()
  "Self-extension is a jump in what the agent can do; it should be a
deliberate act, like shell-command, write-file and eval-elisp — none of
which are in the default allowlist either."
  (should-not (member "define_tool"
                      (default-value 'superchat-llm-tool-names))))

(ert-deftest syn/system-prompt-hint-only-when-enabled ()
  "Built and never triggered is the likely failure: faced with a task no
tool covers, a model answers from memory rather than wondering whether it
could build the tool it lacks."
  (let ((superchat-llm-tool-names '("read-file")))
    (let ((turn (superchat-turn-new "x")))
      (superchat-prompt-hook--self-extension turn)
      (should-not (string-match-p "define_tool"
                                  (or (superchat-turn-system-prompt turn) "")))))
  (let ((superchat-llm-tool-names '("read-file" "define_tool")))
    (let ((turn (superchat-turn-new "x")))
      (superchat-prompt-hook--self-extension turn)
      (should (string-match-p "define_tool"
                              (superchat-turn-system-prompt turn))))))

(ert-deftest syn/system-prompt-hint-respects-active-tool-profile ()
  "A slash-command profile must not advertise an excluded `define_tool'."
  (let ((superchat-llm-tool-names '("read-file" "define_tool"))
        (superchat--active-preset
         (superchat-preset--create :name "team" :tools '("team_task"))))
    (let ((turn (superchat-turn-new "x")))
      (superchat-prompt-hook--self-extension turn)
      (should-not (string-match-p "define_tool"
                                  (or (superchat-turn-system-prompt turn) ""))))))

;; ═══════════════════════════════════════════════════════
;; Phase 2 — persistence, hash anchoring, rehydration
;; ═══════════════════════════════════════════════════════

(defmacro test-syn--with-store (&rest body)
  "Run BODY with a throwaway data directory and fresh synthesis state.
Stubs the two llm.el constructors the registration path touches, so
the persistence tests run without the optional llm package."
  (declare (indent 0))
  `(let* ((superchat-data-directory (make-temp-file "syn-store" t))
          (superchat-llm-synthesized-tools nil)
          (superchat--syn-names nil)
          (superchat--syn-rehydrated nil))
     (cl-letf (((symbol-function 'llm-make-tool)
                (lambda (&rest kw) kw))
               ((symbol-function 'llm-tool-name)
                (lambda (tool) (plist-get tool :name))))
       (unwind-protect (progn ,@body)
         (delete-directory superchat-data-directory t)))))

(ert-deftest syn/approval-records-runtime-tape-event ()
  "Synthesized-tool approval is an admitted fact, not a raw DB write."
  (test-syn--with-store
    (let (event)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (value)
                   (setq event value)
                   1)))
        (superchat--syn-record-approval "hash-1" "greet" 'approved))
      (should (equal "synthesis.approval.decided"
                     (plist-get event :type)))
      (should (equal "synthesis/approval" (plist-get event :topic)))
      (should
       (equal '(:definition-hash "hash-1"
                :tool-name "greet"
                :decision approved)
              (plist-get event :payload))))))

(ert-deftest syn/persist-then-rehydrate-loads-silently ()
  "A persisted, hash-approved definition rehydrates with no interaction."
  (test-syn--with-store
    (superchat--syn-persist-entry
     "greet" "Say hello" '((:name "who" :type string :description "name"))
     "(concat \"hi \" who)")
    (superchat--syn-record-approval
     (superchat--syn-hash "greet" "Say hello"
                          '((:name "who" :type string :description "name"))
                          "(concat \"hi \" who)")
     "greet" 'approved)
    ;; A fresh session: nothing registered yet.
    (let ((result (superchat-synthesis-rehydrate)))
      (should (= 1 (plist-get result :loaded)))
      (should-not (plist-get result :pending)))
    (should (superchat--syn-tool-p "greet"))
    (should (fboundp (superchat--syn-function-symbol "greet")))
    (should (equal "hi bob"
                   (funcall (superchat--syn-function-symbol "greet") "bob")))))

(ert-deftest syn/tampered-definition-does-not-load ()
  "Any post-approval edit — including to the description — blocks
silent rehydration.  The hash anchors the COMPLETE definition."
  (test-syn--with-store
    (superchat--syn-persist-entry "greet" "Say hello" nil "\"hi\"")
    (superchat--syn-record-approval
     (superchat--syn-hash "greet" "Say hello" nil "\"hi\"")
     "greet" 'approved)
    ;; Tamper with the description only; body untouched.
    (superchat--syn-persist-entry "greet" "Say hello. Also run rm -rf" nil "\"hi\"")
    (let ((result (superchat-synthesis-rehydrate)))
      (should (= 0 (plist-get result :loaded)))
      (should (= 1 (length (plist-get result :pending)))))
    (should-not (superchat--syn-tool-p "greet"))))

(ert-deftest syn/rejected-hash-stays-quiet ()
  "A rejected definition neither loads nor nags."
  (test-syn--with-store
    (superchat--syn-persist-entry "bad" "Nope" nil "\"x\"")
    (superchat--syn-record-approval
     (superchat--syn-hash "bad" "Nope" nil "\"x\"")
     "bad" 'rejected)
    (let ((result (superchat-synthesis-rehydrate)))
      (should (= 0 (plist-get result :loaded)))
      (should-not (plist-get result :pending)))))

(ert-deftest syn/review-approve-records-hash-and-loads ()
  "The review command approves a pending entry: hash recorded, tool live."
  (test-syn--with-store
    (superchat--syn-persist-entry "pending-tool" "New here" nil "42")
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?a))
              ((symbol-function 'with-help-window) (lambda (_b &rest _) nil))
              ((symbol-function 'quit-windows-on) #'ignore))
      (superchat-tools-review-synthesized))
    (should (eq 'approved
                (superchat--syn-approval-state
                 (superchat--syn-hash "pending-tool" "New here" nil "42"))))
    (should (superchat--syn-tool-p "pending-tool"))))

(ert-deftest syn/define-with-persist-writes-store-and-approval ()
  "Confirming `define_tool' persists the entry and stamps its hash —
definition-time confirmation IS the approval; no second ceremony."
  (test-syn--with-store
    (test-syn--with-approval t
      (let ((superchat-data-directory superchat-data-directory))
        (superchat--syn-define nil nil t "adder" "Add"
                               "[{\"name\":\"n\",\"type\":\"integer\",\"description\":\"n\"}]"
                               "(+ n 1)")))
    (let ((entries (superchat--syn-store-entries)))
      (should (= 1 (length entries)))
      (should (equal "adder" (plist-get (car entries) :name))))
    (let* ((entry (car (superchat--syn-store-entries)))
           (hash (superchat--syn-hash (plist-get entry :name)
                                      (plist-get entry :description)
                                      (plist-get entry :args)
                                      (plist-get entry :body))))
      (should (eq 'approved (superchat--syn-approval-state hash))))))

(ert-deftest syn/approval-ledger-is-tool-unwritable ()
  "No agent file tool may write the approval ledger."
  (test-syn--with-store
    (should (string-match-p
             "cannot be written"
             (superchat-tool-write-file (superchat--syn-approvals-file) "evil")))
    (should (string-match-p
             "cannot be written"
             (superchat-tool-append-file (superchat--syn-approvals-file) "evil")))
    (should (string-match-p
             "cannot be written"
             (superchat-tool-quick-write (superchat--syn-approvals-file) "evil")))))

(ert-deftest syn/sql-tool-cannot-mutate-via-with ()
  "The read-only SQL gate rejects WITH...INSERT smuggling."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-db)
  (let ((superchat-data-directory (make-temp-file "syn-db" t)))
    (unwind-protect
        (progn
          (should-error (superchat-db-tape-select
                         "WITH x AS (SELECT 1) INSERT INTO tape VALUES (1)"))
          (should-error (superchat-db-tape-select
                         "SELECT 1; DROP TABLE tape"))
          (should (superchat-db-tape-select "SELECT 1")))
      (when (fboundp 'superchat-db-close) (ignore-errors (superchat-db-close)))
      (delete-directory superchat-data-directory t))))

(ert-deftest syn/rehydrated-tool-confirms-on-non-agent-call ()
  "A rehydrated tool gates itself: on a non-agent path (no wrapper) it
still asks, and declining blocks execution."
  (test-syn--with-store
    (superchat--syn-persist-entry "g" "d" nil "42")
    (superchat--syn-record-approval
     (superchat--syn-hash "g" "d" nil "42") "g" 'approved)
    (superchat-synthesis-rehydrate)
    (let* ((tool (car superchat-llm-synthesized-tools))
           (fn (plist-get tool :function))
           (superchat-agent-confirm-synthesized t))
      ;; Non-agent direct call, user declines → blocked.
      (let ((superchat--syn-call-confirmed nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
          (should (string-match-p "declined" (funcall fn)))))
      ;; User approves → runs.
      (let ((superchat--syn-call-confirmed nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (should (equal 42 (funcall fn)))))
      ;; Outer layer already gated (agent path) → no prompt, runs.
      (let ((superchat--syn-call-confirmed t))
        (should (equal 42 (funcall fn)))))))

(ert-deftest syn/guarded-call-contains-body-errors ()
  "A synthesized body that signals returns an actionable string, not a
crash — nothing escapes to the caller's async machinery."
  (test-syn--with-store
    ;; A body that references an unbound variable, like a model might write.
    (superchat--syn-persist-entry "boom" "d" nil "(+ 1 undefined-var)")
    (superchat--syn-record-approval
     (superchat--syn-hash "boom" "d" nil "(+ 1 undefined-var)") "boom" 'approved)
    (superchat-synthesis-rehydrate)
    (let* ((tool (car superchat-llm-synthesized-tools))
           (fn (plist-get tool :function))
           (superchat--syn-call-confirmed t)
           (result (funcall fn)))
      (should (stringp result))
      (should (string-match-p "errored" result))
      (should (string-match-p "undefined-var" result)))))

(ert-deftest syn/rehydrate-skips-name-that-now-shadows-a-builtin ()
  "A persisted name that has since become a built-in is not loaded."
  (test-syn--with-store
    (superchat--syn-persist-entry "read-file" "shadow" nil "1")
    (superchat--syn-record-approval
     (superchat--syn-hash "read-file" "shadow" nil "1") "read-file" 'approved)
    (cl-letf (((symbol-function 'superchat--syn-existing-names)
               (lambda () '("read-file"))))
      (let ((result (superchat-synthesis-rehydrate)))
        (should (= 0 (plist-get result :loaded)))
        (should (assoc "read-file" (plist-get result :pending)))
        (should-not (superchat--syn-tool-p "read-file"))))))

;; ═══════════════════════════════════════════════════════
;; read-file line slices (RLM-style on-demand access)
;; ═══════════════════════════════════════════════════════

(ert-deftest syn/read-file-slice-returns-requested-lines ()
  (let ((path (make-temp-file "slice" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dotimes (i 100) (insert (format "L%d\n" (1+ i)))))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
            (let ((result (superchat-tool-read-file path 40 3)))
              (should (string-match-p "\\[lines 40\\.\\.42 of 100\\]" result))
              (should (string-match-p "^L40$" result))
              (should (string-match-p "^L42$" result))
              (should-not (string-match-p "^L43$" result)))
            ;; offset past EOF is a readable error, not empty output
            (should (string-match-p "beyond the file's 100 lines"
                                    (superchat-tool-read-file path 200 5)))
            ;; no slice args → whole file, unchanged behaviour
            (should (string-match-p "^L100$" (superchat-tool-read-file path)))))
      (delete-file path))))

(provide 'test-synthesis)

;;; test-synthesis.el ends here
