;;; test-preset-contract.el --- Preset runtime-contract regressions -*- lexical-binding: t; -*-

;;; Commentary:

;; Locks the end-to-end semantics of preset fields (v1.3 Phase 1,
;; docs/goals/agent-profiles.md).  These are contract tests: they
;; assert that body/model/tools actually reach their runtime
;; consumers, not merely that parsing stores them.

;;; Code:

(require 'ert)
(require 'superchat)

;; ═══════════════════════════════════════════════════════
;; 1. Body → system prompt
;; ═══════════════════════════════════════════════════════

(ert-deftest contract/agent-preset-body-enters-system-prompt ()
  "preset-apply puts an agent preset's body into turn.system-prompt."
  (let ((preset (superchat-preset-from-plist
                 (list :name "coder" :type 'agent :body "You are a coder.")))
        (turn (superchat-turn-new "do something")))
    (superchat-preset-apply preset turn)
    (should (string-match-p "You are a coder."
                            (superchat-turn-system-prompt turn)))))

(ert-deftest contract/plan-preset-body-enters-system-prompt ()
  "Plan presets get the same persona treatment as agent presets."
  (let ((preset (superchat-preset-from-plist
                 (list :name "planner" :type 'plan :body "Read-only planner.")))
        (turn (superchat-turn-new "plan it")))
    (superchat-preset-apply preset turn)
    (should (string-match-p "Read-only planner."
                            (superchat-turn-system-prompt turn)))))

(ert-deftest contract/prompt-preset-body-stays-out-of-system-prompt ()
  "Prompt-type presets keep their body out of the system prompt.
For them the body is a prompt template handled by the skills layer."
  (let ((preset (superchat-preset-from-plist
                 (list :name "tpl" :type 'prompt :body "Template body.")))
        (turn (superchat-turn-new "hi")))
    (superchat-preset-apply preset turn)
    (should-not (string-match-p "Template body."
                                (or (superchat-turn-system-prompt turn) "")))))

(ert-deftest contract/body-prepended-before-hook-instructions ()
  "Persona comes before mechanical instructions added by earlier hooks."
  (let ((preset (superchat-preset-from-plist
                 (list :name "coder" :type 'agent :body "PERSONA")))
        (turn (superchat-turn-new "x")))
    (setf (superchat-turn-system-prompt turn) "LANG-INSTRUCTION")
    (superchat-preset-apply preset turn)
    (let ((sys (superchat-turn-system-prompt turn)))
      (should (< (string-match "PERSONA" sys)
                 (string-match "LANG-INSTRUCTION" sys))))))

(ert-deftest contract/execute-llm-query-exports-system-prompt ()
  "The dispatcher result plist carries the turn's system prompt."
  (let ((turn (superchat-turn-new "question")))
    (setf (superchat-turn-system-prompt turn) "SYS")
    (setf (superchat-turn-prompt turn) "PROMPT")
    (let ((result (superchat--execute-llm-query turn)))
      (should (equal "SYS" (plist-get result :system-prompt))))))

(ert-deftest contract/execute-llm-query-omits-empty-system-prompt ()
  "No :system-prompt key when the turn has none."
  (let ((turn (superchat-turn-new "question")))
    (setf (superchat-turn-prompt turn) "PROMPT")
    (let ((result (superchat--execute-llm-query turn)))
      (should-not (plist-member result :system-prompt)))))

(ert-deftest contract/skills-invoke-agent-skill-no-double-body ()
  "For agent skills the invoke prompt is the bare user input;
the body travels via the turn's system prompt instead."
  (let ((tmp-dir (make-temp-file "superchat-contract" t))
        (superchat-skills-directory nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "coder.md" tmp-dir)
            (insert "---\nname: coder\ndescription: Coder\ntype: agent\n---\n\nAGENT-PERSONA\n"))
          (let* ((turn (superchat-turn-new ">coder fix the bug"))
                 (result (superchat-skills-invoke "coder" "fix the bug" turn)))
            (should (equal "fix the bug" (plist-get result :prompt)))
            (should (string-match-p "AGENT-PERSONA"
                                    (superchat-turn-system-prompt turn)))))
      (delete-directory tmp-dir t))))

;; ═══════════════════════════════════════════════════════
;; 2. Model → effective backend
;; ═══════════════════════════════════════════════════════

(ert-deftest contract/preset-model-reaches-execute-llm-query ()
  "execute-llm-query reads the turn's target-model when called
without an explicit model — the sub-agent path and the dispatcher's
pre-preset-apply binding both rely on this."
  (let ((preset (superchat-preset-from-plist
                 (list :name "cheap" :type 'agent :model "qwen3:8b")))
        (turn (superchat-turn-new "task")))
    (superchat-preset-apply preset turn)
    (setf (superchat-turn-prompt turn) "P")
    (let ((result (superchat--execute-llm-query turn)))
      (should (equal "qwen3:8b" (plist-get result :target-model))))))

(ert-deftest contract/explicit-model-arg-still-wins-in-execute ()
  "An explicit target-model argument overrides the turn's."
  (let ((turn (superchat-turn-new "task")))
    (setf (superchat-turn-target-model turn) "turn-model")
    (setf (superchat-turn-prompt turn) "P")
    (let ((result (superchat--execute-llm-query turn nil "arg-model")))
      (should (equal "arg-model" (plist-get result :target-model))))))

(ert-deftest contract/user-at-model-wins-over-preset-model ()
  "A per-turn @model (already on the turn) is not clobbered by the preset."
  (let ((preset (superchat-preset-from-plist
                 (list :name "cheap" :type 'agent :model "preset-model")))
        (turn (superchat-turn-new "task")))
    (setf (superchat-turn-target-model turn) "user-model")
    (superchat-preset-apply preset turn)
    (should (equal "user-model" (superchat-turn-target-model turn)))))

(ert-deftest contract/preset-model-fills-empty-turn ()
  "Without a user @model, the preset's model fills the turn."
  (let ((preset (superchat-preset-from-plist
                 (list :name "cheap" :type 'agent :model "preset-model")))
        (turn (superchat-turn-new "task")))
    (superchat-preset-apply preset turn)
    (should (equal "preset-model" (superchat-turn-target-model turn)))))

(ert-deftest contract/subagent-async-model-reaches-llm-call ()
  "The async sub-agent path hands the preset model to the LLM call."
  (let ((captured-model nil)
        (tmp-dir (make-temp-file "superchat-contract" t))
        (superchat-skills-directory nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "cheapie.md" tmp-dir)
            (insert "---\nname: cheapie\ndescription: Cheap agent\ntype: agent\nmodel: qwen3:8b\n---\n\nBody.\n"))
          (cl-letf (((symbol-function 'superchat--subagent-llm-async)
                     (lambda (_ctx _prompt _tools model _system callback)
                       (setq captured-model model)
                       (funcall callback "ok"))))
            (superchat--subagent-run-async 'cheapie "task" nil #'ignore))
          (should (equal "qwen3:8b" captured-model)))
      (delete-directory tmp-dir t))))

;; ═══════════════════════════════════════════════════════
;; 3. tools: [] = explicitly no tools
;; ═══════════════════════════════════════════════════════

(ert-deftest contract/empty-tools-frontmatter-parses-to-none ()
  "tools: [] parses to the `none' sentinel; absent key stays nil."
  (let ((with-empty (superchat-preset-from-frontmatter
                     "---\nname: x\ndescription: d\ntype: agent\ntools: []\n---\nBody"))
        (without (superchat-preset-from-frontmatter
                  "---\nname: y\ndescription: d\ntype: agent\n---\nBody")))
    (should (eq 'none (superchat-preset-tools with-empty)))
    (should (null (superchat-preset-tools without)))))

(ert-deftest contract/skills-load-empty-tools-parses-to-none ()
  "superchat-skills-load applies the same tools: [] semantics."
  (let ((tmp-dir (make-temp-file "superchat-contract" t))
        (superchat-skills-directory nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "toolless.md" tmp-dir)
            (insert "---\nname: toolless\ndescription: No tools\ntype: agent\ntools: []\n---\n\nBody.\n"))
          (should (eq 'none (superchat-preset-tools
                             (superchat-skills-load "toolless")))))
      (delete-directory tmp-dir t))))

(ert-deftest contract/collect-llm-tools-none-returns-nil ()
  "The `none' sentinel yields an empty tool set at collection time,
even when global tools would otherwise attach."
  (cl-letf (((symbol-function 'superchat-get-llm-tools)
             (lambda () '(dummy-tool)))
            ((symbol-function 'superchat--should-attach-tools-p)
             (lambda (_input) t)))
    (should (null (superchat--collect-llm-tools "input" 'none)))))

(ert-deftest contract/none-tools-propagate-through-turn ()
  "preset tools `none' lands on the turn and in the query plist."
  (let ((preset (superchat-preset-from-plist
                 (list :name "toolless" :type 'agent :tools 'none)))
        (turn (superchat-turn-new "task")))
    (superchat-preset-apply preset turn)
    (should (eq 'none (superchat-turn-tools turn)))
    (setf (superchat-turn-prompt turn) "P")
    (should (eq 'none (plist-get (superchat--execute-llm-query turn) :tools)))))

;; ═══════════════════════════════════════════════════════
;; 4. :backend removed
;; ═══════════════════════════════════════════════════════

(ert-deftest contract/backend-slot-removed ()
  "The never-consumed :backend slot is gone; a backend: key in
frontmatter is ignored without error."
  (should-not (fboundp 'superchat-preset-backend))
  (let ((preset (superchat-preset-from-frontmatter
                 "---\nname: x\ndescription: d\nbackend: ollama\n---\nBody")))
    (should (superchat-preset-p preset))))

;; ═══════════════════════════════════════════════════════
;; 5. Per-preset inference options
;; ═══════════════════════════════════════════════════════

(ert-deftest contract/profile-inference-frontmatter-is-typed ()
  "Inference profile fields are converted to their runtime types."
  (let ((preset (superchat-preset-from-frontmatter
                 "---\nname: precise\ndescription: Precise agent\ntype: agent\ntemperature: 0.2\nmax_tokens: 1200\nreasoning: medium\n---\nBody")))
    (should (= 0.2 (superchat-preset-temperature preset)))
    (should (= 1200 (superchat-preset-max-tokens preset)))
    (should (eq 'medium (superchat-preset-reasoning preset)))))

(ert-deftest contract/profile-inference-options-reach-llm-prompt ()
  "A preset's inference options reach llm-make-chat-prompt."
  (let ((preset (superchat-preset-from-plist
                 (list :name "precise" :temperature 0.2
                       :max-tokens 1200 :reasoning 'medium)))
        captured)
    (cl-letf (((symbol-function 'llm-make-chat-prompt)
               (lambda (_text &rest args) (setq captured args))))
      (superchat--build-llm-prompt "Prompt" nil nil preset))
    (should (= 0.2 (plist-get captured :temperature)))
    (should (= 1200 (plist-get captured :max-tokens)))
    (should (eq 'medium (plist-get captured :reasoning)))))

(ert-deftest contract/subagent-profile-inference-options-reach-llm-prompt ()
  "The async sub-agent uses the inference options in its captured preset."
  (let* ((preset (superchat-preset-from-plist
                  (list :name "precise" :temperature 0.3
                        :max-tokens 900 :reasoning 'light)))
         (ctx (superchat--subagent-make-context preset 1))
         (superchat-llm-backend 'backend)
         captured)
    (cl-letf (((symbol-function 'superchat--effective-llm-backend) #'identity)
              ((symbol-function 'llm-make-chat-prompt)
               (lambda (_text &rest args)
                 (setq captured args)
                 'prompt))
              ((symbol-function 'llm-chat-async)
               (lambda (_backend _prompt response-cb _error-cb _multi-output)
                 (funcall response-cb "ok"))))
      (superchat--subagent-llm-async ctx "Prompt" nil nil nil #'ignore))
    (should (= 0.3 (plist-get captured :temperature)))
    (should (= 900 (plist-get captured :max-tokens)))
    (should (eq 'light (plist-get captured :reasoning)))))

(ert-deftest contract/execute-llm-query-exports-preset ()
  "Dispatcher results retain the preset needed by async generation."
  (let* ((preset (superchat-preset-from-plist
                  (list :name "precise" :temperature 0.2)))
         (turn (superchat-turn-new "task")))
    (setf (superchat-turn-prompt turn) "Prompt")
    (superchat-preset-apply preset turn)
    (should (eq preset
                (plist-get (superchat--execute-llm-query turn) :preset)))))

(ert-deftest contract/native-skill-loader-preserves-inference-options ()
  "Loading a user skill preserves all typed inference options."
  (let ((tmp-dir (make-temp-file "superchat-profile-" t)))
    (unwind-protect
        (let ((superchat-skills-directory tmp-dir))
          (with-temp-file (expand-file-name "precise.md" tmp-dir)
            (insert "---\nname: precise\ndescription: Precise agent\ntype: agent\ntemperature: 0.4\nmax_tokens: 700\nreasoning: maximum\n---\nBody\n"))
          (let ((preset (superchat-skills-load "precise")))
            (should (= 0.4 (superchat-preset-temperature preset)))
            (should (= 700 (superchat-preset-max-tokens preset)))
            (should (eq 'maximum (superchat-preset-reasoning preset)))))
      (delete-directory tmp-dir t))))

(ert-deftest contract/invalid-profile-inference-options-warn-and-inherit ()
  "Invalid inference values are ignored with one warning per field."
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional _level _buffer)
                 (push message warnings))))
      (let ((preset (superchat-preset-from-plist
                     (list :name "bad" :temperature 3
                           :max-tokens 0 :reasoning 'impossible))))
        (should (null (superchat-preset-temperature preset)))
        (should (null (superchat-preset-max-tokens preset)))
        (should (eq 'inherit (superchat-preset-reasoning preset)))))
    (should (= 3 (length warnings)))))

(ert-deftest contract/profile-reasoning-overrides-or-inherits-global ()
  "Preset reasoning overrides the global value; absent reasoning inherits it."
  (let ((superchat-llm-reasoning 'maximum)
        inherited overridden)
    (cl-letf (((symbol-function 'llm-make-chat-prompt)
               (lambda (_text &rest args) args)))
      (setq inherited
            (superchat--build-llm-prompt
             "P" nil nil (superchat-preset-from-plist (list :name "a"))))
      (setq overridden
            (superchat--build-llm-prompt
             "P" nil nil (superchat-preset-from-plist
                            (list :name "b" :reasoning 'none)))))
    (should (eq 'maximum (plist-get inherited :reasoning)))
    (should (eq 'none (plist-get overridden :reasoning)))))

(ert-deftest contract/profile-reasoning-boolean-values-are-typed ()
  "Boolean-style reasoning values map to medium and none."
  (should
   (eq 'medium
       (superchat-preset-reasoning
        (superchat-preset-from-frontmatter
         "---\nname: on\ndescription: d\nreasoning: true\n---\nBody"))))
  (should
   (eq 'none
       (superchat-preset-reasoning
        (superchat-preset-from-frontmatter
         "---\nname: off\ndescription: d\nreasoning: false\n---\nBody")))))

(provide 'test-preset-contract)

;;; test-preset-contract.el ends here
