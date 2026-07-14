;;; superchat-render.el --- Buffer rendering for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Chat buffer rendering: MD→Org conversion, prompt insertion,
;; streaming display, TTFT annotation, and response finalization.
;; Extracted from superchat.el monolith (v0.9 split step 4).

;;; Code:

(require 'cl-lib)
(require 'superchat-preset)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-buffer-name)
(defvar superchat--response-start-marker)
(defvar superchat--assistant-response-start-marker)
(defvar superchat--prompt-start)
(defvar superchat--active-preset)
(defvar superchat--current-turn)
(defvar superchat--current-response-parts)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat--ttft-start-time)
(defvar superchat-file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-inline-context-template)
(defvar superchat-inline-max-bytes)
(defvar superchat--context-files nil)
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--read-inline-file-content "superchat" (file-path))
(declare-function superchat--render-inline-context "superchat" (file-path content))
(declare-function superchat--textual-file-p "superchat" (file-path))

(defun superchat--md-to-org (md-string)
  "Convert MD-STRING from Markdown to Org-mode format.
This function is adapted from ollama-buddy's implementation,
using a robust two-pass approach to handle code blocks correctly.
This is a pure function that takes a string and returns a converted string."
  (if (string-empty-p md-string)
      ""
    (with-temp-buffer
      (insert md-string)
      (save-excursion
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (save-match-data
            ;; First, handle code blocks by temporarily protecting their content
            (goto-char (point-min))
            (let ((code-blocks nil)
                  (counter 0)
                  block-start block-end lang content placeholder)
              (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
                (setq lang (match-string 1)
                      content (match-string 2)
                      block-start (match-beginning 0)
                      block-end (match-end 0)
                      placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
                (push (list placeholder lang content) code-blocks)
                (delete-region block-start block-end)
                (goto-char block-start)
                (insert placeholder)
                (setq counter (1+ counter)))

              ;; Apply regular Markdown to Org transformations
              (goto-char (point-min))
              (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
                (replace-match (concat (match-string 1) "- \\2")))

              (goto-char (point-min))
              (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
                (replace-match "*\\1*"))

              (goto-char (point-min))
              (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
                (replace-match "\\1/\\2/\\3"))

              (goto-char (point-min))
              (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
                (replace-match "[[\\2][\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "`\\(.*?\\)`" nil t)
                (replace-match "=\\1="))

              (goto-char (point-min))
              (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
                (replace-match "-----"))

              (goto-char (point-min))
              (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
                (replace-match "[[\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "^\\(#+\\) " nil t)
                (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))

              ;; (goto-char (point-min))
              ;; (while (re-search-forward "—" nil t)
              ;;   (replace-match ", "))

              ;; Restore code blocks
              (dolist (block (nreverse code-blocks))
                (let ((placeholder (nth 0 block))
                      (lang (nth 1 block))
                      (content (nth 2 block)))
                  (goto-char (point-min))
                  (when (search-forward placeholder nil t)
                    (replace-match (format "#+begin_src %s\n%s\n#+end_src" (or lang "") content) t t))))))))
      (buffer-string))))

(defun superchat--insert-prompt ()
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n")  ; Always insert a blank line before the prompt
      (let* ((preset-label
              (when (and (boundp 'superchat--active-preset)
                         superchat--active-preset)
                (format "%s:%s"
                        (superchat-preset-type superchat--active-preset)
                        (superchat-preset-name superchat--active-preset))))
             (headline
              (cond
               ((and superchat--current-command preset-label)
                (format "* User [%s mode | %s]: " superchat--current-command preset-label))
               (superchat--current-command
                (format "* User [%s mode]: " superchat--current-command))
               (preset-label
                (format "* User [%s]: " preset-label))
               (t "* User: "))))
        (insert (propertize headline 'face 'superchat-prompt-face))
        (setq superchat--prompt-start (point-marker))))))

(defun superchat--prepare-for-response ()
  "Move to end of buffer, insert newline, and set marker for response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (setq superchat--response-start-marker (point-marker)))))

(defun superchat--update-status (message)
  "Update the status MESSAGE in the chat buffer (e.g., 'Assistant is thinking...')."
  (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert (propertize message 'face 'italic))))))

(defmacro superchat--ttft-log (stage)
  "Log STAGE with elapsed time from `superchat--ttft-start-time'.
Only active when `superchat-show-ttft-breakdown' is non-nil.
STAGE is a string label describing the pipeline step."
  `(when (and superchat-show-ttft-breakdown superchat--ttft-start-time)
     (message "⏱ TTFT [%s]: %.3fs" ,stage
              (- (float-time) superchat--ttft-start-time))))

(defun superchat--prepare-assistant-response-area ()
  "Prepare the buffer for the assistant's response.
This function should only be called once per response."
  (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        ;; Tools may have rendered their call/result transcript after
        ;; the status line while llm.el was executing them.  Put the
        ;; eventual final answer after that append-only transcript, so
        ;; `superchat--process-llm-result' rewrites only its own raw
        ;; streamed text rather than deleting the tool records.
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq superchat--assistant-response-start-marker (point-marker))
        (insert "\n** Assistant\n")
        (setq superchat--response-start-marker nil)))))

(defun superchat--annotate-ttft (elapsed)
  "Append a TTFT annotation to the current Assistant header.
ELAPSED is seconds since the request was dispatched.  The annotation
is inserted on the `** Assistant' header line so it stays visible in
the chat buffer (echo-area `message' is unreliable during streaming
because redisplay clobbers it)."
  (when (and superchat--assistant-response-start-marker
             (marker-position superchat--assistant-response-start-marker))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t)
            (annotation (propertize (format "  ⚡ TTFT: %.2fs" elapsed)
                                    'face 'shadow)))
        (save-excursion
          (goto-char superchat--assistant-response-start-marker)
          ;; Marker sits just before the inserted "\n** Assistant\n";
          ;; move to end of the "** Assistant" header line.
          (when (re-search-forward "^\\*\\* Assistant" nil t)
            (end-of-line)
            (insert annotation)))))))

(defun superchat--stream-llm-result (chunk)
  "Process a CHUNK from the LLM streaming response, update UI and accumulate.
CHUNK is rendered with the `superchat-streaming-pending' face so raw
markdown mid-stream doesn't get re-interpreted by font-lock (which
was causing visible \"residue\" / flicker before the final rewrite)."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (push chunk superchat--current-response-parts)
      (unless superchat--assistant-response-start-marker
        (superchat--prepare-assistant-response-area))
      (goto-char (point-max))
      (let ((start (point)))
        (insert chunk)
        (put-text-property start (point) 'face 'superchat-streaming-pending)))))

(defun superchat--process-llm-result (answer)
  "Finalize the LLM ANSWER processing.
This function is called ONCE after the entire response has been streamed."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content (if (and answer (not (string-empty-p answer)))
                                answer
                              "Assistant did not provide a response.")))
      ;; If streaming was active, replace the raw content with the formatted version.
      (if (and superchat--assistant-response-start-marker (marker-position superchat--assistant-response-start-marker))
          (progn
            (goto-char superchat--assistant-response-start-marker)
            (delete-region (point) (point-max))
            (insert "\n** Assistant\n")
            (insert (superchat--md-to-org response-content)))
        ;; Fallback for non-streaming or failed streaming.
        (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
          (superchat--prepare-assistant-response-area)
          (insert (superchat--md-to-org response-content))))

      ;; Apply the text property for the assistant's response
      (when (and superchat--assistant-response-start-marker (marker-position superchat--assistant-response-start-marker))
        (put-text-property superchat--assistant-response-start-marker (point-max) 'superchat-role 'assistant)
        (setq superchat--assistant-response-start-marker nil))

      ;; 1. Update conversation history with the full response
      (superchat--record-message "assistant" response-content)
      
      ;; 2. Insert the next prompt (it handles its own spacing)
      (superchat--insert-prompt))))

(defun superchat--insert-system-message (content)
  "Insert a system message CONTENT into the chat buffer and refresh the prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (propertize (format "* System: %s" content) 'face 'italic))
      (insert "\n")
      (superchat--refresh-prompt))))

(defun superchat--refresh-prompt ()
  "Clear any status message and insert a fresh prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (point-max)))
      (goto-char (point-max))
      (superchat--insert-prompt))))


(provide 'superchat-render)
;;; superchat-render.el ends here
