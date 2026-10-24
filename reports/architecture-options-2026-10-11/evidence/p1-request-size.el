;;; p1-request-size.el --- how big is one provider request, per surface -*- lexical-binding: t; -*-

;; Research probe, read-only.  Runs against the exported snapshot of 75e83370
;; with the scripted transport of the canonical fixtures; no real provider.
;; It serializes the WHOLE request llm.el would send (messages and tool
;; schemas, through `llm-provider-chat-request') and records its size, so the
;; numbers are what the provider receives, not what the chat window shows.
;;
;; Scenarios (each a label in the output):
;;   temp-chat-1        a plain question in a temporary conversation
;;   project-chat-1     the same in a project Workspace with a 6000-character
;;                      AGENTS.md, a current Work Record and three skills
;;   project-chat-2     the second request of that conversation's second turn,
;;                      after a turn whose model called read_buffer
;;   inline-ask         superchat-inline-ask from a project file
;;   inline-rewrite     superchat-rewrite-region on a project file
;;   inline-annotate    the superchat-annotate command on a project file
;;
;; Tokens are NOT measured; an estimate is printed with its heuristic
;; (ASCII characters / 4 + non-ASCII characters / 1.5) and is labelled so.

(load (expand-file-name "scripts/work-record-regression.el" default-directory) nil t)
(mapc (lambda (test) (ert-delete-test (ert-test-name test))) (ert-select-tests t t))
(require 'superchat-annotate)
(require 'superchat-inline)
(require 'superchat-rewrite)
(require 'superchat-skills)

(defconst p1--out (file-name-directory (or load-file-name buffer-file-name)))
(defvar p1--label "unlabelled")
(defvar p1--captures nil "Newest first.")

(defun p1--chars (s)
  "Return (ASCII . NON-ASCII) character counts of S."
  (let ((ascii 0) (non 0))
    (dotimes (i (length s))
      (if (< (aref s i) 128) (cl-incf ascii) (cl-incf non)))
    (cons ascii non)))

(defun p1--estimate (s)
  "A rough token estimate for S; see the file header for the heuristic."
  (let ((c (p1--chars s)))
    (round (+ (/ (car c) 4.0) (/ (cdr c) 1.5)))))

(defun p1--capture (provider prompt)
  "Serialize the request llm.el would send and record its sizes under `p1--label'."
  (let* ((request (llm-provider-chat-request provider prompt nil))
         (json (json-encode request))
         (parsed (json-parse-string json :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil))
         (messages (alist-get 'messages parsed))
         (tools (alist-get 'tools parsed))
         (system (cl-find-if (lambda (m) (equal "system" (alist-get 'role m))) messages))
         (system-text (let ((c (alist-get 'content system))) (if (stringp c) c "")))
         (tools-json (if tools (json-encode tools) ""))
         (user-texts (mapcar (lambda (m) (let ((c (alist-get 'content m))) (if (stringp c) c "")))
                             (cl-remove-if-not (lambda (m) (equal "user" (alist-get 'role m))) messages)))
         (label (format "%s-%d" p1--label
                        (1+ (cl-count-if (lambda (c) (string-prefix-p p1--label (plist-get c :label)))
                                         p1--captures)))))
    (push (list :label label
                :bytes (string-bytes json)
                :chars (length json)
                :est-tokens (p1--estimate json)
                :system-chars (length system-text)
                :system-est-tokens (p1--estimate system-text)
                :tools-count (length tools)
                :tools-json-bytes (string-bytes tools-json)
                :tools-est-tokens (p1--estimate tools-json)
                :messages (length messages)
                :user-chars (apply #'+ (mapcar #'length user-texts))
                :user-est-tokens (apply #'+ (mapcar #'p1--estimate user-texts)))
          p1--captures)
    (with-temp-file (expand-file-name (format "p1-%s-system.txt" label) p1--out)
      (insert system-text))
    (with-temp-file (expand-file-name (format "p1-%s-messages.txt" label) p1--out)
      (dolist (m messages)
        (insert (format "--- role=%s chars=%d\n" (alist-get 'role m)
                        (let ((c (alist-get 'content m))) (if (stringp c) (length c) 0))))
        (let ((c (alist-get 'content m))) (when (stringp c) (insert c "\n")))
        (when-let* ((calls (alist-get 'tool_calls m)))
          (insert (format "[tool_calls: %d]\n" (length calls))))))
    (with-temp-file (expand-file-name (format "p1-%s-tools.json" label) p1--out)
      (insert tools-json))))

(defmacro p1--capturing (&rest body)
  "Run BODY with every scripted provider request measured."
  `(let ((network (symbol-function 'smoke--fake-network)))
     (cl-letf (((symbol-function 'smoke--fake-network)
                (lambda (mode provider prompt partial response error multi)
                  (p1--capture provider prompt)
                  (funcall network mode provider prompt partial response error multi))))
       ,@body)))

(defun p1--settle ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 30)))

(defconst p1--rules-unit
  "回答用中文。日期写成 2026-10-09 这种格式。每次回答先写一行以“思路：”开头的话。Headings in English; keep org structure; do not touch TODO states.\n")

(defun p1--rules-text ()
  "A rules text of exactly 6000 characters, mixed Chinese and English."
  (let ((s ""))
    (while (< (length s) 6000) (setq s (concat s p1--rules-unit)))
    (substring s 0 6000)))

(defun p1--write-skills ()
  "Three catalogued skills under the fixture's skills directory."
  (dolist (name '("meeting-notes" "org-agenda-review" "elisp-debugging"))
    (let ((dir (expand-file-name name superchat-skills-directory)))
      (make-directory dir t)
      (with-temp-file (expand-file-name "SKILL.md" dir)
        (insert (format "---\nname: %s\ndescription: How to handle %s in this workspace, step by step, with the files to read first.\n---\n# %s\n\nRead the relevant file, then act.\n" name name name))))))

(ert-deftest p1/temporary-chat ()
  (hm--fixture '((:text "Done."))
    (p1--capturing
      (let ((view (smoke--open-chat)))
        (setq p1--label "temp-chat")
        (hm--turn view "在 log.org 末尾加一行“- 第一次检查”。")))))

(ert-deftest p1/project-chat-and-in-place-entries ()
  (wrec--with (list (hm--read-round "hm-target") '(:text "Read it.")
                    '(:text "Second turn answer.")
                    '(:text "An inline answer.")
                    '(:text "我们定于 2026-10-09 15:00 召开会议，请携带电脑。")
                    '(:text "1 | hello | REMARK ONE"))
      (p)
    (p1--capturing
      (wrec--write p "AGENTS.md" (p1--rules-text))
      (p1--write-skills)
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (id (superchat-work-start session "P1: finish the reading list")))
        (superchat-work-update session id "half the list read" nil "continue at item 6")
        (setq p1--label "project-chat")
        (hm--turn view "继续。")
        (hm--turn view "再读一遍。")
        (setq p1--label "inline-ask")
        (with-current-buffer (wrec--file p "notes.org")
          (superchat-inline-ask "这段是什么意思？"))
        (p1--settle)
        (setq p1--label "inline-rewrite")
        (with-current-buffer (wrec--file p "notice.org")
          (erase-buffer)
          (insert "* 通知\n大家好。\n我们定在2026年10月9日下午三点开会，记得带电脑哦。\n")
          (set-buffer-modified-p nil)
          (goto-char (point-min)) (forward-line 2)
          ;; `superchat-rewrite-region' reads the region's bounds through
          ;; `region-noncontiguous-p', so the region must be active as it is
          ;; for the interactive call.
          (let ((transient-mark-mode t)
                (beg (point)) (end (line-end-position)))
            (push-mark beg t t)
            (goto-char end)
            (superchat-rewrite-region beg end "改得正式一些")))
        (p1--settle)
        (setq p1--label "inline-annotate")
        (with-current-buffer (wrec--file p "draft.org")
          (superchat-annotate "指出值得注意的地方"))
        (p1--settle)
        (should (>= (length p1--captures) 6))))))

(defun p1--report ()
  (let ((rows (reverse p1--captures)))
    (princ "\nP1 REQUEST SIZE (scripted transport, snapshot 75e83370)\n")
    (princ "label | bytes | chars | est.tokens | system chars (est) | tools n / json bytes (est) | messages | user chars (est)\n")
    (dolist (c rows)
      (princ (format "%s | %d | %d | ~%d | %d (~%d) | %d / %d (~%d) | %d | %d (~%d)\n"
                     (plist-get c :label) (plist-get c :bytes) (plist-get c :chars)
                     (plist-get c :est-tokens) (plist-get c :system-chars)
                     (plist-get c :system-est-tokens) (plist-get c :tools-count)
                     (plist-get c :tools-json-bytes) (plist-get c :tools-est-tokens)
                     (plist-get c :messages) (plist-get c :user-chars)
                     (plist-get c :user-est-tokens))))
    (princ "est.tokens = ASCII chars/4 + non-ASCII chars/1.5; a heuristic, not a tokenizer.\n")
    (with-temp-file (expand-file-name "p1-request-size.json" p1--out)
      (insert (json-encode rows)))))

(let ((stats (ert-run-tests-batch "^p1/")))
  (p1--report)
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
