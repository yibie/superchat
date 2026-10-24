;;; ts-discovery.el --- harness-only prototype: dormant tools the model enables itself -*- lexical-binding: t; -*-
;; Loaded by run-task.el through TS_EXTRA_LISP after TS_TOOLS set the core
;; list.  Not product code.  Two parts, both copied from how `define_tool'
;; already works (superchat-synthesis.el):
;;  1. a <dormant_tools> block in the system text listing, by name and first
;;     sentence, every default-surface or row-owned built-in tool that is not
;;     in the core list;
;;  2. a plugin tool `enable_tools' that hot-injects the named dormant tools
;;     into the in-flight prompt, so the model can call them on its next step.
(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-synthesis)
(require 'superchat-tools)
(require 'superchat-prompt-hooks)

(defvar tsd-core (copy-sequence superchat-llm-tool-names)
  "The core surface this run started with (TS_TOOLS).")

(defvar tsd-enabled nil "Names enabled so far in this Emacs, in order.")

(defun tsd-dormant-names ()
  "Default-surface built-in names not in the core and not already offered by a row."
  ;; Default-surface names only: listing row-owned tools too would let the
  ;; lean surface reach more than D13 does (a product question, not tested).
  ;; Row-offered names (work_record) are on the wire already, so not dormant.
  (cl-remove-if (lambda (n) (or (member n tsd-core)
                                (member n (bound-and-true-p superchat-tools-row-offered-builtin-names))
                                (member n '("ask_user" "enable_tools" "define_tool"))))
                superchat-llm-default-tool-names))

(defun tsd-struct (name)
  "The built-in llm tool struct for NAME, or nil."
  (ignore-errors (car (superchat-get-llm-tools (list name)))))

(defun tsd-first-sentence (text)
  (let ((s (string-trim (or text ""))))
    (let ((end (and (string-match "[.!?][ \t\n]" s 20) (1+ (match-beginning 0)))))
      (cond (end (substring s 0 end))
            ((> (length s) 160) (concat (substring s 0 157) "..."))
            (t s)))))

(defun tsd-catalog-text ()
  (let ((entries
         (delq nil
               (mapcar (lambda (n)
                         (when-let* ((tool (tsd-struct n)))
                           (format "- %s: %s" n (tsd-first-sentence (llm-tool-description tool)))))
                       (tsd-dormant-names)))))
    (when entries
      (concat "Some tools are dormant: they exist but are not attached to this request. "
              "When the task needs one, call enable_tools with its name or names "
              "(comma-separated); the tool is then callable on your next step. "
              "Do not imitate a dormant tool with eval_elisp.\n\n<dormant_tools>\n"
              (mapconcat #'identity entries "\n")
              "\n</dormant_tools>"))))

(defun tsd-prompt-hook (turn)
  "Append the dormant-tools block to TURN's system prompt."
  (when-let* ((text (ignore-errors (tsd-catalog-text))))
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn) "\n" text)))
  turn)

;; The language-instruction hook is unconditional (no tool gate), so riding
;; on its return value puts the block into every chat request.
(advice-add 'superchat-prompt-hook--language-instruction :filter-return #'tsd-prompt-hook)

(defun tsd-enable (prompt wrap-fn names)
  "Inject the dormant tools NAMES into PROMPT, wrapped with WRAP-FN."
  (let* ((names (if (stringp names) (split-string names "[ ,]+" t) names))
         (dormant (tsd-dormant-names)) ok bad)
    (dolist (n names)
      (let ((tool (and (member n dormant) (tsd-struct n))))
        (if (null tool)
            (push n bad)
          (superchat--syn-inject prompt (if (functionp wrap-fn) (funcall wrap-fn tool) tool))
          (push n ok)
          (cl-pushnew n tsd-enabled :test #'equal))))
    (when (fboundp 'ts-mark)
      (ts-mark (format "enable_tools: ok=%S bad=%S" (reverse ok) (reverse bad))))
    (string-trim
     (concat
      (when ok (format "Enabled: %s. Call them on your next step. " (string-join (nreverse ok) ", ")))
      (when bad (format "Not dormant tools: %s. The dormant tools are: %s."
                        (string-join (nreverse bad) ", ") (string-join dormant ", ")))))))

(superchat-register-tool
 "enable_tools"
 :function (lambda (_names) "Error: enable_tools is not bound to a request.")
 :description "Attach dormant tools to this conversation so you can call them on your next step. Pass the names from <dormant_tools>, comma-separated."
 :surfaces '(chat)
 :args (list (list :name "names" :type 'string
                   :description "Comma-separated names of dormant tools to enable, e.g. \"timer_create\" or \"work_record,timer_create\"")))

(defun tsd-bind (prompt wrap-fn &rest _)
  "Bind `enable_tools' in PROMPT to this request, as `superchat--syn-bind' does for define_tool."
  (when (and prompt (fboundp 'llm-chat-prompt-tools))
    (let ((tools (ignore-errors (llm-chat-prompt-tools prompt))))
      (when (cl-some (lambda (tool) (and (llm-tool-p tool) (equal (llm-tool-name tool) "enable_tools"))) tools)
        (superchat--syn-set-prompt-tools
         prompt
         (mapcar (lambda (tool)
                   (if (not (and (llm-tool-p tool) (equal (llm-tool-name tool) "enable_tools")))
                       tool
                     (let ((bound (llm-make-tool
                                   :name "enable_tools"
                                   :description (llm-tool-description tool)
                                   :args (llm-tool-args tool)
                                   :function (lambda (names) (tsd-enable prompt wrap-fn names)))))
                       (if (functionp wrap-fn) (funcall wrap-fn bound) bound))))
                 tools))))))

(advice-add 'superchat--syn-bind :after #'tsd-bind)

(when (fboundp 'ts-mark)
  (ts-mark (format "discovery prototype: core=%S dormant=%S" tsd-core (tsd-dormant-names))))

(provide 'ts-discovery)
;;; ts-discovery.el ends here
