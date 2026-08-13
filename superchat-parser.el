;;; superchat-parser.el --- Parsers for superchat symbols -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a set of pure functions for parsing superchat's special
;; symbols: @ for models, / for commands, and # for contexts. It is a
;; low-level library with no dependencies on other superchat modules.

;;; Code:

(defconst superchat-parser--file-ref-regexp
  "#\\s-*\\(\"\\([^\"]+\\)\"\\|\\([~/][^[:space:]#:]+\\)\\)\\(:\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\)?"
  "Regexp to capture a file path after a leading '#'.
Groups 1/2 capture a quoted or unquoted absolute path; groups 3-5
capture an optional line range as `:START' or `:START-END' (1-based,
inclusive).")

(defconst superchat-parser--command-name-regexp
  "[^[:space:]/]+"
  "Regexp for command names used after a leading '/'.
Matches any non-whitespace, non-slash characters (supports Unicode).")

(defun superchat-parser--normalize-file-path (file-path)
  "Normalize FILE-PATH: unescape spaces, strip quotes, expand, trim newline."
  (when (and file-path (stringp file-path))
    (let ((fp (replace-regexp-in-string "\\\\ " " " file-path)))
      (when (and (>= (length fp) 2)
                 (string= "\"" (substring fp 0 1))
                 (string= "\"" (substring fp -1)))
        (setq fp (substring fp 1 -1)))
      (setq fp (expand-file-name fp))
      (setq fp (replace-regexp-in-string "\n$" "" fp))
      fp)))

(defun superchat-parser-model-switch (input)
  "Parse INPUT for @model syntax and return (clean-input . model) cons.
If no @model syntax is found, return nil."
  (when (and input (string-match "@\\([a-zA-Z0-9_.:-]+\\)" input))
    (let* ((model-name (match-string 1 input))
           (clean-input (replace-regexp-in-string "@[a-zA-Z0-9_.:-]+" "" input)))
      (cons (string-trim clean-input) model-name))))

(defun superchat-parser-define (input)
  "Parse /define command from INPUT."
  (when (and input (stringp input))
    (cond
     ((string-match (format "^/define\\s-+\\(%s\\)\\s-+\"\\(\\(?:.\\|\n\\)*?\\)\"\\s-*$"
                            superchat-parser--command-name-regexp)
                    input)
      (cons (match-string 1 input) (or (match-string 2 input) "")))
     ((string-match (format "^/define\\s-+\\(%s\\)\\s-*$"
                            superchat-parser--command-name-regexp)
                    input)
      (cons (match-string 1 input) ""))
     (t nil))))

(defun superchat-parser-command (input)
  "Parse INPUT, return (command . args) or nil."
  (when (and input (stringp input))
    ;; Support multi-line args: anything after the command (spaces or newline)
    ;; is treated as args and preserved.
    (if (string-match (format "^/\\(%s\\)\\(?:\\s-+\\(\\(?:.\\|\n\\)*\\)\\)?\\'"
                              superchat-parser--command-name-regexp)
                      input)
        (cons (or (match-string 1 input) "")
              (string-trim (or (match-string 2 input) "")))
      nil)))

(defun superchat-parser-extract-file-path (input)
  "Extract and normalize the first file path from INPUT string."
  (let (file-path)
    (when (and input (string-match superchat-parser--file-ref-regexp input))
      (setq file-path (or (match-string 1 input)
                          (match-string 2 input)))
      (setq file-path (superchat-parser--normalize-file-path file-path)))
    file-path))

(defun superchat-parser-extract-file-range (input)
  "Extract the optional line range from INPUT, or nil.
Returns (START . END) for `:START-END', (START . START) for `:START',
and nil when INPUT carries no range."
  (when (and input (string-match superchat-parser--file-ref-regexp input))
    (let ((start (match-string 5 input))
          (end (match-string 6 input)))
      (when start
        (cons (string-to-number start)
              (string-to-number (or end start)))))))

(provide 'superchat-parser)

;;; superchat-parser.el ends here
