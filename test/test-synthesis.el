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

(provide 'test-synthesis)

;;; test-synthesis.el ends here
