;;; superchat-prompt-hooks.el — Prompt-building hooks for superchat-core pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Five focused hook functions that replace the monolithic
;; `superchat--build-final-prompt'.  Each hook receives a
;; `superchat-turn' struct, reads/writes its slots, and returns
;; the modified turn.
;;
;; Hook signatures: (turn) → modified-turn or nil
;; nil signals "skip me" — the core runner discards it.

;;; Code:

(require 'cl-lib)
(require 'superchat-core)

;; ── Forward declarations (owned by other modules) ──
(defvar superchat-lang)
(defvar superchat-general-answer-prompt)
(defvar superchat-context-message-count)
(defvar superchat--file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-inline-context-template)
(defvar superchat-inline-max-bytes)

(declare-function superchat--normalize-file-path "superchat-dispatcher" (file-path))
(declare-function superchat--textual-file-p "superchat-dispatcher" (path))
(declare-function superchat--read-inline-file-content "superchat-dispatcher" (file-path))
(declare-function superchat--render-inline-context "superchat-dispatcher" (file-path content))
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--format-retrieved-memories "superchat-dispatcher" (memories))
(declare-function superchat--conversation-context-string "superchat" (limit))

;; ═══════════════════════════════════════════════════════════
;; System-prompt hooks
;; ═══════════════════════════════════════════════════════════

(defun superchat-prompt-hook--language-instruction (turn)
  "Append a language instruction to `turn.system-prompt' if needed.
When `superchat-lang' is not English and the template does not
already reference `$lang', set a language directive."
  (let* ((current-lang (or superchat-lang "English"))
         (template (or superchat-general-answer-prompt "")))
    (unless (or (string-empty-p current-lang)
                (string= current-lang "English")
                (string-match-p (regexp-quote "$lang") template))
      (setf (superchat-turn-system-prompt turn)
            (concat (superchat-turn-system-prompt turn)
                    (unless (string-empty-p (superchat-turn-system-prompt turn))
                      "\n")
                    (format "Your response must be in %s." current-lang)))))
  turn)

;; ═══════════════════════════════════════════════════════════
;; Build-prompt hooks (order matters — see add-hook calls below)
;; ═══════════════════════════════════════════════════════════

(defun superchat-prompt-hook--file-inline (turn)
  "Parse #file ref from `turn.clean-input', inline file content into `turn.prompt'.
Strips the file ref from `turn.clean-input' so downstream hooks
see the plain query.  When no file ref is present, returns turn unchanged."
  (let* ((initial-query (string-trim (or (superchat-turn-clean-input turn) "")))
         (file-path
          (when (string-match superchat--file-ref-regexp initial-query)
            (superchat--normalize-file-path
             (or (match-string 1 initial-query)
                 (match-string 2 initial-query))))))
    (if (null file-path)
        turn
      ;; Strip file ref from clean-input
      (let* ((path-start (match-beginning 0))
             (path-end (match-end 0))
             (text-before (substring initial-query 0 path-start))
             (text-after (substring initial-query path-end))
             (user-query (string-trim (concat (string-trim text-before) " "
                                              (string-trim text-after))))
             (file-content
              (when (and (file-exists-p file-path)
                         (superchat--textual-file-p file-path))
                (superchat--read-inline-file-content file-path)))
             (inline-context
              (when file-path
                (superchat--add-file-to-context file-path)
                (when (and superchat-inline-file-content
                           (not (and (string-empty-p user-query) file-content)))
                  (superchat--render-inline-context file-path file-content)))))
        ;; When user only supplied a file ref, inject file content as $input
        (when (and (string-empty-p user-query) file-content)
          (setq user-query (format "File: %s\n\n%s" file-path file-content)))
        ;; Warn on missing file
        (when (and file-path (not (file-exists-p file-path)))
          (message "Warning: Referenced file does not exist: %s" file-path))
        ;; Update turn slots
        (setf (superchat-turn-clean-input turn) user-query)
        (when inline-context
          (setf (superchat-turn-prompt turn)
                (concat inline-context
                        (unless (string-empty-p (superchat-turn-prompt turn))
                          "\n\n")
                        (superchat-turn-prompt turn)))))))
  turn)

(defun superchat-prompt-hook--template-substitution (turn)
  "Substitute `$input' and `$lang' in the template, append to `turn.prompt'.
Uses `turn.clean-input' as the effective user query."
  (let* ((current-lang (or superchat-lang "English"))
         (template (or superchat-general-answer-prompt ""))
         (user-query (string-trim (or (superchat-turn-clean-input turn) "")))
         (processed-template template))
    ;; $lang substitution
    (when (string-match-p (regexp-quote "$lang") processed-template)
      (setq processed-template
            (replace-regexp-in-string (regexp-quote "$lang")
                                      current-lang
                                      processed-template
                                      t t)))
    ;; $input substitution
    (let ((base-prompt
           (if (string-match-p (regexp-quote "$input") processed-template)
               (replace-regexp-in-string (regexp-quote "$input")
                                         user-query
                                         processed-template
                                         t t)
             (concat processed-template "\n\nUser question: " user-query))))
      (setf (superchat-turn-prompt turn)
            (concat (superchat-turn-prompt turn)
                    (unless (string-empty-p (superchat-turn-prompt turn))
                      "\n\n")
                    base-prompt))))
  turn)

(defun superchat-prompt-hook--memory-context (turn)
  "Prepend formatted memories from `turn.retrieved-memories' to `turn.prompt'."
  (when-let* ((mems (superchat-turn-retrieved-memories turn))
              (formatted (superchat--format-retrieved-memories mems))
              ((not (string-empty-p formatted))))
    (setf (superchat-turn-prompt turn)
          (concat formatted "\n\n" (superchat-turn-prompt turn))))
  turn)

(defun superchat-prompt-hook--conversation-history (turn)
  "Prepend conversation history context to `turn.prompt'."
  (let ((context (superchat--conversation-context-string
                  superchat-context-message-count)))
    (when (and context (not (string-empty-p context)))
      (setf (superchat-turn-prompt turn)
            (concat context "\n\n" (superchat-turn-prompt turn)))))
  turn)

;; ═══════════════════════════════════════════════════════════
;; Registration — runs at load time
;; ═══════════════════════════════════════════════════════════

(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--language-instruction)

;; build-prompt order matters — later hooks see earlier hooks' work
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--file-inline)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--template-substitution)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--memory-context)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--conversation-history)

(provide 'superchat-prompt-hooks)
;;; superchat-prompt-hooks.el ends here
