;;; superchat-chat.el --- The chat buffer and slash commands -*- lexical-binding: t; -*-

;;; Commentary:
;; The buffer is an Org rendering of the `conversation' view of one timeline,
;; then a prompt and an input area.  The view is shown turn by turn, and the
;; current turn is drawn again from its entries as each comes, so rendering
;; the whole view again gives the same text.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-permission)
(require 'superchat-agent)
(require 'superchat-recall)
(require 'superchat-plugin)

(defvar-local superchat-chat--tape nil "Name of this buffer's timeline.")
(defvar-local superchat-chat--model nil "Model for this buffer, or nil for the default.")
(defvar-local superchat-chat--pending-scope nil
  "Scope given to `superchat-chat-open', for a timeline with no entry yet.")
(defvar-local superchat-chat--origin nil "Origin for the next request sent, or nil.")
(defvar-local superchat-chat--turn nil "Marker: where the current turn's text starts.")
(defvar-local superchat-chat--entries nil "The entries of the current turn, in order.")
(defvar-local superchat-chat--stream nil "Marker: where streamed text starts, under the Assistant heading.")
(defvar-local superchat-chat--stream-end nil "Marker: where streamed text ends and the rest of the turn starts.")
(defvar-local superchat-chat--block nil "Marker: where the separator and the prompt start.")
(defvar-local superchat-chat--input nil "Marker: where the input area starts.")
(defvar-local superchat-chat--answers nil "Hash table: an assistant entry's id to its text as Org.")
(defvar-local superchat-chat--waiting nil "Overlay of the waiting line, above the separator.")
(defvar-local superchat-chat--timer nil "Timer that keeps the waiting line and the mode line current.")
(defvar-local superchat-chat--began nil "When the timeline became busy.")
(defvar-local superchat-chat--queue nil "Texts sent while the timeline was busy, the newest first.")
(defvar-local superchat-chat--force nil "Non-nil: the queue goes when the run ends, however it ends.")
(defvar-local superchat-chat--shown 0 "Id of the latest entry shown; nothing at or below it is shown again.")

(defvar superchat-chat--commands nil
  "Alist of (NAME FUNCTION . DOC), NAME without the slash.")

(defun superchat-command-define (name function doc)
  "Define slash command NAME (a string without the slash).
FUNCTION is called with the rest of the line, in the chat buffer."
  (setq superchat-chat--commands
        (append (assoc-delete-all name superchat-chat--commands)
                (list (cons name (cons function doc))))))

(defun superchat-command-remove (name)
  "Remove slash command NAME."
  (setq superchat-chat--commands (assoc-delete-all name superchat-chat--commands)))

(defun superchat-chat--scope ()
  "The scope of this buffer.  A chat buffer has its timeline's, else the one it
was opened with; any other buffer that of `default-directory'."
  (if superchat-chat--tape
      (or (plist-get (car (superchat-tape-entries :tape superchat-chat--tape :limit 1)) :scope)
          superchat-chat--pending-scope)
    (superchat-tape-scope-of default-directory)))

(defun superchat-chat--origin-here ()
  "The origin of a request made from the current buffer, at point."
  (append (list :buffer (buffer-name))
          (and buffer-file-name (list :file buffer-file-name))
          (list :mode (symbol-name major-mode) :line (line-number-at-pos))))

;;; Markdown as Org

(defun superchat-chat--org (markdown level)
  "MARKDOWN as Org, by the rules of the specification and no others.
Headings are demoted so that the shallowest is at LEVEL, under the heading
the text sits in; with no LEVEL each is a line of bold text.  What no rule
may touch (blocks, inline code, links) is kept out behind placeholders of
private-use characters, which no rule matches."
  (with-temp-buffer
    (insert markdown)
    (let ((kept nil) blocks (top 6))
      (cl-flet* ((keep (string)
                   (push string kept)
                   (format "\uE000%d\uE001" (1- (length kept))))
                 (each (regexp function)
                   (goto-char (point-min))
                   (while (re-search-forward regexp nil t) (funcall function)))
                 (emphasis (mark to inner bound)
                   ;; MARK x MARK becomes TO x TO; x holds none of INNER and
                   ;; neither starts nor ends with a space.  A BOUND mark does
                   ;; not open or close inside a word.
                   (let ((edge (concat "[^ \t\n" inner "]")) (mark (regexp-quote mark)))
                     (each (concat (and bound "\\(?:^\\|[^[:alnum:]]\\)")
                                   "\\(" mark "\\(" edge "\\(?:[^\n" inner "]*?" edge "\\)?\\)"
                                   mark "\\)" (and bound "\\(?:[^[:alnum:]]\\|$\\)"))
                           (lambda ()
                             (let ((text (concat to (match-string 2) to)) (at (match-beginning 1)))
                               (replace-match text t t nil 1)
                               (goto-char (+ at (length text)))))))))
        (let ((case-fold-search t))
          (each "^[ \t]*\\(```+\\|~~~+\\|#\\+begin_src\\_>\\)\\(.*\\)$"
                (lambda ()
                  (let* ((start (match-beginning 0))
                         (from (min (1+ (match-end 0)) (point-max)))
                         (delimiter (match-string 1))
                         (native (string-prefix-p "#" delimiter))
                         (open (if native (string-trim (match-string 0))
                                 (string-trim-right
                                  (concat "#+begin_src " (car (split-string (match-string 2)))))))
                         (found (progn (goto-char from)
                                       (re-search-forward
                                        (if native "^[ \t]*#\\+end_src\\_>.*$"
                                          (concat "^[ \t]*" (regexp-quote delimiter) "[ \t]*$"))
                                        nil t)))
                         (body (string-remove-suffix
                                "\n" (buffer-substring from (if found (match-beginning 0) (point-max))))))
                    (delete-region start (if found (match-end 0) (point-max)))
                    (goto-char start)
                    (insert (keep (concat open "\n"
                                          (if (string-empty-p body) ""
                                            (concat (org-escape-code-in-string body) "\n"))
                                          "#+end_src")))))))
        (setq blocks (length kept))
        (each "`\\([^`\n]+\\)`"
              (lambda ()
                (let ((code (string-trim (match-string 1))))
                  (replace-match (keep (if (string-search "=" code) (concat "~" code "~")
                                         (concat "=" code "=")))
                                 t t))))
        (let ((url "(\\(\\(?:[^ \t\n()]\\|([^ \t\n()]*)\\)+\\))"))
          (each (concat "!\\[[^]\n]*\\]" url)
                (lambda () (replace-match (keep (format "[[%s]]" (org-link-escape (match-string 1))))
                                          t t)))
          (each (concat "\\[\\(\\(?:[^][\n]\\|\\[[^][\n]*\\]\\)+\\)\\]" url)
                (lambda () (replace-match (keep (format "[[%s][%s]]" (org-link-escape (match-string 2))
                                                        (match-string 1)))
                                          t t))))
        (each "^[ \t]*\\([-*_]\\)\\(?:[ \t]*\\1\\)\\{2,\\}[ \t]*$" (lambda () (replace-match "-----" t t)))
        ;; Before the lists: a quoted list item is a list item.
        (each "^>.*\\(?:\n>.*\\)*"
              (lambda () (replace-match (concat "#+begin_quote\n"
                                                (replace-regexp-in-string "^> ?" "" (match-string 0))
                                                "\n#+end_quote")
                                        t t)))
        (each "^\\([ \t]*\\)[-*+] " (lambda () (replace-match "\\1- " t)))
        ;; Heading stars are private-use characters until the emphasis is done.
        (each "^\\(#\\{1,6\\}\\) " (lambda () (setq top (min top (length (match-string 1))))))
        (each "^\\(#\\{1,6\\}\\) \\(.*\\)$"
              (lambda () (replace-match
                          (if level
                              (concat (make-string (- (+ (length (match-string 1)) level) top) ?\uE003)
                                      " " (match-string 2))
                            (concat "\uE003" (string-trim (match-string 2)) "\uE003"))
                          t t)))
        (each "^[ \t]*|\\(?:[ \t]*:?-+:?[ \t]*|\\)+[ \t]*$" (lambda () (replace-match "|-" t t)))
        (emphasis "**" "\uE003" "" nil)
        (emphasis "__" "\uE003" "" t)
        (emphasis "~~" "+" "~" nil)
        (emphasis "*" "/" "*" nil)
        (emphasis "_" "/" "_" t)
        (each "\uE003" (lambda () (replace-match "*" t t)))
        ;; The newest first: a link may hold inline code kept before it.
        (cl-flet ((restore (from)
                    (cl-loop for i from (1- (length kept)) downto from
                             do (goto-char (point-min))
                             (search-forward (format "\uE000%d\uE001" i))
                             (replace-match (nth (- (length kept) 1 i) kept) t t))))
          (restore blocks)
          ;; Before the blocks come back: a line of code starting with | is no table.
          (goto-char (point-min))
          (when (re-search-forward "^[ \t]*|" nil t)
            (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
            (org-table-map-tables #'org-table-align t))
          (setq kept (last kept blocks))
          (restore 0))
        (buffer-string)))))

;;; Rendering

(defun superchat-chat--seal (string &optional id)
  "Make STRING read-only, carrying ID.  Text typed right after it is free and
plain: its last character passes on no property, neither a face nor the id."
  (unless (string-empty-p string)
    (add-text-properties 0 (length string)
                         (list 'read-only t 'front-sticky '(read-only) 'superchat-chat-id id)
                         string)
    (add-text-properties (1- (length string)) (length string) '(rear-nonsticky t) string))
  string)

(defun superchat-chat--dim (string &optional face)
  "STRING in FACE, `shadow' by default, as a face Org's fontification keeps."
  (propertize string 'font-lock-face (or face 'shadow)))

(defun superchat-chat--loose (string)
  "STRING with `superchat-chat-loose' 1, a line that stops every body, wherever
it has none."
  (let ((at 0))
    (while (< at (length string))
      (let ((next (next-single-property-change at 'superchat-chat-loose string (length string))))
        (unless (get-text-property at 'superchat-chat-loose string)
          (put-text-property at next 'superchat-chat-loose 1 string))
        (setq at next)))
    string))

(defun superchat-chat--cut (line width)
  "LINE cut to WIDTH characters and `…' when longer."
  (if (> (length line) width) (concat (substring line 0 width) "…") line))

(defun superchat-chat--args (args)
  "ARGS on one line, cut to 60 characters."
  (superchat-chat--cut (if args (let ((print-escape-newlines t))
                                  (string-trim (format "%S" args) "(" ")"))
                         "")
                       60))

(defun superchat-chat--heading (head body key id folded)
  "HEAD, a heading line, and BODY under it, read-only, with ID.  KEY names the
heading when its turn is drawn again; FOLDED is how it is shown by default."
  (let ((head (copy-sequence head)))
    (put-text-property 0 1 'superchat-chat-fold (list key folded) head)
    (superchat-chat--seal (concat head body "\n") id)))

(defun superchat-chat--call (call level)
  "The heading at LEVEL of CALL, (:call :result :seconds :id :asked), and its
body, after the lines of the permission events ASKED for it; it carries the id
of its tool entry, or of its assistant entry before."
  (let* ((result (plist-get call :result))
         (args (plist-get (plist-get call :call) :args))
         (short (superchat-chat--args args)))
    (concat
     ;; A line no call's body takes in: it ends the body of the call before.
     (propertize (mapconcat #'superchat-chat--format (plist-get call :asked) "")
                 'superchat-chat-loose level)
     (superchat-chat--heading
     (string-join (delq nil (list (concat (make-string level ?*) " "
                                          (cond ((not result) "⏳")
                                                ((plist-get (plist-get result :data) :error) "✗")
                                                (t "✓"))
                                          " " (plist-get (plist-get call :call) :name))
                                  (and (not (string-empty-p short)) short)
                                  (and result (format "(%.1fs)" (plist-get call :seconds)))))
                  "  ")
     (mapconcat (lambda (part) (format "\n#+begin_example\n%s\n#+end_example"
                                       (org-escape-code-in-string part)))
                (seq-remove #'string-empty-p
                            (list (string-trim-right
                                   (cl-loop for (key value) on args by #'cddr
                                            concat (format "%s %S\n" key value)))
                                  (string-trim-right (or (plist-get result :text) ""))))
                "")
     ;; The assistant entry's id too: a provider may give each response the
     ;; same call ids.
     (format "call %s %s" (plist-get call :id) (plist-get (plist-get call :call) :id))
     (plist-get (or result call) :id)
     t))))

(defun superchat-chat--answer (entry level)
  "The text of assistant ENTRY as Org, its headings from LEVEL or, with no
LEVEL, bold lines; converted once per buffer."
  (with-memoization (gethash (cons (plist-get entry :id) level) superchat-chat--answers)
    (superchat-chat--org (string-trim (plist-get entry :text)) level)))

(defun superchat-chat--format (entry)
  "The text of ENTRY, a `user', `anchor', `note' or `event' entry, read-only
and carrying its id."
  (let* ((text (string-trim-right (plist-get entry :text)))
         (data (plist-get entry :data))
         (string
          (pcase (plist-get entry :kind)
            ;; A line of the user's that starts with stars would be a heading of
            ;; the buffer: one space keeps it the user's text.
            ('user (concat "* You\n" (replace-regexp-in-string "^\\*" " *" text) "\n\n"))
            ('anchor (concat (superchat-chat--dim (format "── summary: %s ──" (plist-get data :name)))
                             (if (string-empty-p text) "\n\n" (concat "\n" text "\n\n"))
                             (let ((next (string-trim (or (plist-get data :next) ""))))
                               (unless (string-empty-p next)
                                 (concat "What comes next: " next "\n\n")))))
            ;; A heading folded by default: a note often repeats the answer.
            ;; Its text is no line that stops a body, so the heading folds it.
            ('note (superchat-chat--heading
                    (superchat-chat--dim (format "** note: %s" (plist-get data :title)))
                    (propertize (concat "\n" (replace-regexp-in-string "^\\*" " *" text) "\n")
                                'superchat-chat-loose most-positive-fixnum)
                    (format "note %s" (plist-get entry :id)) (plist-get entry :id) t))
            (_ (superchat-chat--dim (concat text "\n")
                                    (if (equal (plist-get data :type) "error") 'error 'shadow))))))
    (superchat-chat--seal string (plist-get entry :id))))

(defun superchat-chat--process (answers results asked done)
  "The Progress heading of the turn whose `assistant' entries other than the
final answer are ANSWERS, and what it holds: their texts and calls.  RESULTS
maps a call id to (TOOL ENTRY . SECONDS), ASKED to the permission events asked
for it.  It is folded once the turn is DONE,
unless a call failed."
  (let (inside names waiting failed)
    (dolist (entry answers)
      (unless (string-empty-p (string-trim (plist-get entry :text)))
        ;; A heading of its own: after a call, it is not taken into the call's subtree.
        (let ((text (superchat-chat--answer entry nil)))
          (push (superchat-chat--heading
                 (concat "*** 💬 " (superchat-chat--cut (car (split-string text "\n")) 60))
                 (if (string-search "\n" text) (concat "\n" text) "")
                 (format "said %s" (plist-get entry :id)) (plist-get entry :id) nil)
                inside)))
      (dolist (call (plist-get (plist-get entry :data) :calls))
        (let ((result (gethash (plist-get call :id) results)))
          (push (plist-get call :name) names)
          (unless result (setq waiting t))
          (when (plist-get (plist-get (car result) :data) :error) (setq failed t))
          (push (superchat-chat--call (list :call call :result (car result) :seconds (cdr result)
                                            :id (plist-get entry :id)
                                            :asked (gethash (plist-get call :id) asked))
                                      3)
                inside))))
    (when inside
      (superchat-chat--heading
       (concat "** " (and waiting "⏳ ") "Progress"
               (when names
                 (format ": %d call%s: %s" (length names) (if (cdr names) "s" "")
                         (mapconcat (lambda (name)
                                      (let ((k (seq-count (apply-partially #'equal name) names)))
                                        (if (> k 1) (format "%s ×%d" name k) name)))
                                    (seq-uniq (reverse names)) ", "))))
       (concat "\n" (string-remove-suffix "\n" (apply #'concat (nreverse inside))))
       (format "progress %s" (plist-get (car answers) :id))
       (plist-get (car answers) :id)
       (and done (not failed))))))

(defun superchat-chat--turn-text (entries streaming over)
  "The text of ENTRIES, one turn, as (HEAD . TAIL); streamed text goes between.
HEAD is the user's request and, once the model answers or is STREAMING, the
Assistant heading, Progress and, with a final answer or STREAMING, the Answer
heading; TAIL is the final answer and the other entries.  Progress is folded
once the final answer is there or the turn is OVER."
  (let* ((results (make-hash-table :test #'equal))
         (asked (make-hash-table :test #'equal))
         (waiting nil)
         (answers (seq-filter (lambda (e) (eq (plist-get e :kind) 'assistant)) entries))
         (final (car (last answers)))
         (final (and final (not (plist-get (plist-get final :data) :calls))
                     (not (string-empty-p (string-trim (plist-get final :text))))
                     final))
         (before nil))
    (dolist (entry entries)
      (pcase (plist-get entry :kind)
        ('assistant (setq waiting (append waiting (mapcar (lambda (c) (plist-get c :id))
                                                          (plist-get (plist-get entry :data) :calls)))))
        ('tool (let ((id (plist-get (plist-get entry :data) :call-id)))
                 (setq waiting (remove id waiting))
                 (puthash id (cons entry (if before (- (plist-get entry :at) (plist-get before :at)) 0))
                          results)))
        ;; Asked for the next call to run: the first that has no result yet.
        ('event (when (and waiting (equal (plist-get (plist-get entry :data) :type) "permission"))
                  (puthash (car waiting) (append (gethash (car waiting) asked) (list entry)) asked)
                  (setq entries (remq entry entries)))))
      (setq before entry))
    (cl-flet ((others (user) (mapconcat #'superchat-chat--format
                                        (seq-filter (lambda (e) (eq (eq (plist-get e :kind) 'user) user))
                                                    (seq-remove (lambda (e) (memq (plist-get e :kind)
                                                                                  '(assistant tool)))
                                                                entries))
                                        "")))
      (cons (concat (others t)
                    (when (or answers streaming)
                      (concat (superchat-chat--seal "* Assistant\n" (plist-get (car answers) :id))
                              (superchat-chat--process
                               (seq-remove (lambda (e) (eq e final)) answers) results asked
                               (or final over))
                              (when (or final streaming)
                                (superchat-chat--seal "** Answer\n" (plist-get final :id))))))
            (let ((tail (concat (when final
                                  (superchat-chat--seal (concat (superchat-chat--answer final 3) "\n\n")
                                                        (plist-get final :id)))
                                ;; A line no heading's body takes in.
                                (superchat-chat--loose (others nil)))))
              ;; Streamed text has no end of line of its own.
              (if (and streaming (not (string-empty-p tail)))
                  (concat (superchat-chat--seal "\n\n") tail)
                tail))))))

(defun superchat-chat--folds (from to)
  "The (POSITION KEY FOLDED) of each heading between FROM and TO, FOLDED being
how it is shown by default."
  (let ((at from) folds)
    (while (setq at (text-property-not-all at to 'superchat-chat-fold nil))
      (push (cons at (get-text-property at 'superchat-chat-fold)) folds)
      (setq at (1+ at)))
    folds))

(defun superchat-chat--body (at)
  "The (START . END) of the body of the heading at AT as Org reckons it in the
text now, ending before the first line that belongs to no heading as deep, or
nil when it has none."
  (save-excursion
    (goto-char at)
    (let* ((level (org-current-level))
           (start (line-end-position))
           (stop start))
      ;; The separator and the input are no heading's body either.
      (while (and (< stop (min (point-max) superchat-chat--block))
                  (not (let ((loose (get-text-property stop 'superchat-chat-loose)))
                         (and loose (<= loose level)))))
        (setq stop (next-single-property-change stop 'superchat-chat-loose nil
                                                (min (point-max) superchat-chat--block))))
      (save-restriction
        (narrow-to-region (point-min) stop)
        (org-end-of-subtree t t)
        (let ((end (if (and (bolp) (> (point) start)) (1- (point)) (point))))
          (and (> end start) (cons start end)))))))

(defun superchat-chat--choices (from to)
  "The (KEY . FOLDED) of each heading in sight between FROM and TO that the
person folded or opened, against the way it is shown by default."
  (cl-loop for (at key folded) in (superchat-chat--folds from to)
           for body = (superchat-chat--body at)
           for now = (and body (org-fold-folded-p (car body)) t)
           when (and body (not (invisible-p at)) (not (eq now folded)))
           collect (cons key now)))

(defun superchat-chat--fold (from to choices)
  "Fold, by its key, each heading between FROM and TO that folds by default or
by CHOICES: its body as Org reckons it now."
  (pcase-dolist (`(,at ,key ,folded) (superchat-chat--folds from to))
    (when-let* (((if-let* ((choice (assoc key choices))) (cdr choice) folded))
                (body (superchat-chat--body at)))
      (org-fold-region (car body) (cdr body) t 'outline))))

(defun superchat-chat--edit (function)
  "Call FUNCTION with the buffer writable.  The user's point and that of every
window showing the buffer keep their distance from the end in the input area,
their position above it."
  (let* ((inhibit-read-only t)
         ;; In the entries a position, for a turn drawn again; below them a
         ;; marker, which moves with what is inserted above it.
         (keep (lambda (at) (cond ((>= at superchat-chat--input) (- at (point-max)))
                                  ((< at superchat-chat--block) at)
                                  (t (copy-marker at)))))
         (here (funcall keep (point)))
         ;; A window not selected has a point of its own.
         (windows (mapcar (lambda (w) (cons w (funcall keep (window-point w))))
                          (get-buffer-window-list nil nil t))))
    (funcall function)
    (let ((back (lambda (at) (cond ((markerp at) (prog1 (marker-position at) (set-marker at nil)))
                                   ((<= at 0) (+ (point-max) at))
                                   (t (min at superchat-chat--input))))))
      (goto-char (funcall back here))
      (pcase-dolist (`(,w . ,at) windows) (set-window-point w (funcall back at))))))

(defun superchat-chat--put (at string &rest stay)
  "Insert STRING at marker AT, before every marker there but those of STAY."
  (let ((from (marker-position at)))
    (goto-char from)
    (insert-before-markers string)
    (dolist (marker stay) (when (= marker (point)) (set-marker marker from)))))

(defun superchat-chat--draw (&optional choices)
  "Draw the current turn again from its entries, keeping what streamed; a
heading the person folded or opened stays so, as do those of CHOICES."
  (let* ((choices (append choices (superchat-chat--choices superchat-chat--turn superchat-chat--block)))
         (streamed (buffer-substring superchat-chat--stream superchat-chat--stream-end))
         (text (superchat-chat--turn-text superchat-chat--entries (not (string-empty-p streamed))
                                          (not (superchat-run-active superchat-chat--tape)))))
    (delete-region superchat-chat--turn superchat-chat--block)
    (superchat-chat--put superchat-chat--turn (car text) superchat-chat--turn)
    (superchat-chat--put superchat-chat--stream-end streamed
                         superchat-chat--turn superchat-chat--stream)
    (superchat-chat--put superchat-chat--block (cdr text)
                         superchat-chat--turn superchat-chat--stream superchat-chat--stream-end)
    (superchat-chat--fold superchat-chat--turn superchat-chat--block choices)))

(defconst superchat-chat--prompt
  (superchat-chat--seal (concat (superchat-chat--dim (make-string 40 ?─)) "\n"
                                (superchat-chat--dim "❯ " 'bold)))
  "The separator line and the prompt, between the entries and the input.")

(defun superchat-chat--render ()
  "Show the whole `conversation' view again, keeping the unsent input."
  (superchat-chat--edit
   (lambda ()
     (let ((input (buffer-substring superchat-chat--input (point-max)))
           (choices (superchat-chat--choices (point-min) superchat-chat--block))
           (turns nil))
       (setq superchat-chat--shown 0)
       (dolist (entry (superchat-view 'conversation :tape superchat-chat--tape))
         (setq superchat-chat--shown (max superchat-chat--shown (plist-get entry :id)))
         (if (or (not turns) (eq (plist-get entry :kind) 'user))
             (push (list entry) turns)
           (push entry (car turns))))
       (setq turns (nreverse (mapcar #'reverse turns))
             superchat-chat--entries (car (last turns)))
       (erase-buffer)
       (dolist (turn (butlast turns))
         (let ((text (superchat-chat--turn-text turn nil t))) (insert (car text) (cdr text))))
       (dolist (marker (list superchat-chat--turn superchat-chat--stream
                             superchat-chat--stream-end superchat-chat--block))
         (set-marker marker (point)))
       (superchat-chat--fold (point-min) (point) choices)
       (insert superchat-chat--prompt)
       (set-marker superchat-chat--input (point))
       (insert input)
       (superchat-chat--draw choices)))))

(defun superchat-chat--line (text width)
  "TEXT on one line, cut to WIDTH characters."
  (let ((line (replace-regexp-in-string "[\n\t]+" " " text)))
    (substring line 0 (min width (length line)))))

(defun superchat-chat--name (tape)
  "The buffer name for timeline TAPE."
  (let ((first (car (superchat-tape-entries :tape tape :kinds '(user) :limit 1))))
    (format "*superchat: %s*" (if first (superchat-chat--line (plist-get first :text) 40) "new"))))

(defun superchat-chat--waiting-line (now)
  "The waiting line of the current turn at time NOW."
  (let* ((answered (delq nil (mapcar (lambda (e) (and (eq (plist-get e :kind) 'tool)
                                                      (plist-get (plist-get e :data) :call-id)))
                                     superchat-chat--entries)))
         (call (seq-find (lambda (c) (not (member (plist-get c :id) answered)))
                         (mapcan (lambda (e) (copy-sequence (plist-get (plist-get e :data) :calls)))
                                 superchat-chat--entries))))
    (propertize (format "%c %s %ds\n" (aref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod (floor (* now 10)) 10))
                        (if call (format "%s running…" (plist-get call :name)) "thinking…")
                        (- now (or (plist-get (car (last superchat-chat--entries)) :at) now)))
                'face 'shadow)))

(defun superchat-chat--queued-line ()
  "The line that shows the queue, or nil when it is empty."
  (when-let* ((first (car (last superchat-chat--queue))))
    (let ((line (car (split-string first "\n"))))
      (propertize (format "⏸ %squeued: %s\n"
                          (if (cdr superchat-chat--queue)
                              (format "%d " (length superchat-chat--queue))
                            "")
                          (if (string-search "\n" first)
                              (concat (substring line 0 (min 60 (length line))) "…")
                            (superchat-chat--cut line 60)))
                  'face 'shadow))))

(defun superchat-chat--stop ()
  "Cancel the timer of the buffer."
  (when superchat-chat--timer
    (cancel-timer superchat-chat--timer)
    (setq superchat-chat--timer nil)))

(defun superchat-chat--status ()
  "Show the state of the timeline in the header line, the mode line and the
waiting line; a timer keeps them current while the timeline is busy."
  (let ((run (superchat-run-active superchat-chat--tape))
        (now (float-time)))
    (if (not run)
        (superchat-chat--stop)
      (unless superchat-chat--timer
        (let ((buffer (current-buffer)))
          (setq superchat-chat--began now
                superchat-chat--timer
                (run-at-time 0.2 0.2 (lambda () (when (buffer-live-p buffer)
                                                  (with-current-buffer buffer
                                                    (superchat-chat--status)))))))))
    (setq header-line-format
          (string-replace "%" "%%"
                          (concat (or superchat-chat--model superchat-llm-model "not set")
                                  " · " (symbol-name (superchat-permission-level superchat-chat--tape))
                                  " · " (abbreviate-file-name (superchat-chat--scope))
                                  (and run " · running")))
          mode-line-process (and run (format " [running %ds]" (- now superchat-chat--began))))
    (move-overlay superchat-chat--waiting superchat-chat--block superchat-chat--block)
    (overlay-put superchat-chat--waiting 'before-string
                 (when-let* ((lines (delq nil (list (and run (not (eq (plist-get run :status) 'handoff))
                                                         (superchat-chat--waiting-line now))
                                                    (superchat-chat--queued-line)))))
                   (apply #'concat lines)))
    (force-mode-line-update)))

(defun superchat-chat--add (_entry)
  "Show the entries of the timeline after the latest one shown, the entry just
appended among them: one that reached the Tape another way is shown too."
  (let ((new (superchat-tape-entries :tape superchat-chat--tape :after superchat-chat--shown)))
    (cond
     ;; A render for an anchor shows the rest of the list itself.
     ((seq-find (lambda (e) (eq (plist-get e :kind) 'anchor)) new) (superchat-chat--render))
     (new
      (superchat-chat--edit
       (lambda ()
         (dolist (entry new)
           (pcase (plist-get entry :kind)
             ('user (set-marker superchat-chat--turn superchat-chat--block)
                    (setq superchat-chat--entries nil))
             ;; The entry replaces what streamed.
             ('assistant (delete-region superchat-chat--stream superchat-chat--stream-end)))
           (setq superchat-chat--entries (append superchat-chat--entries (list entry))
                 superchat-chat--shown (plist-get entry :id)))
         (superchat-chat--draw)))))
    (when (seq-find (lambda (e) (eq (plist-get e :kind) 'user)) new)
      (rename-buffer (superchat-chat--name superchat-chat--tape) t)))
  (superchat-chat--status))

(defun superchat-chat--on-text (text)
  "Show streamed TEXT as it comes, raw, where the final answer goes."
  (superchat-chat--edit
   (lambda ()
     (let ((first (= superchat-chat--stream superchat-chat--stream-end)))
       (superchat-chat--put superchat-chat--stream-end
                            (superchat-chat--seal (superchat-chat--dim (copy-sequence text)))
                            superchat-chat--turn superchat-chat--stream)
       ;; The first text may bring the turn its Assistant heading.
       (when first (superchat-chat--draw))))))

(defun superchat-chat--unstream ()
  "Remove the streamed text that never became an entry."
  (superchat-chat--edit
   (lambda ()
     (delete-region superchat-chat--stream superchat-chat--stream-end)
     (superchat-chat--draw))))

(defun superchat-chat--busy ()
  "When the timeline has a run or a handoff, say so and return non-nil."
  (when-let* ((run (superchat-run-active superchat-chat--tape)))
    (message "A %s is still running; C-c C-k cancels it"
             (if (eq (plist-get run :status) 'handoff) "handoff" "request"))))

(defun superchat-chat--buffer (tape)
  "The chat buffer of timeline TAPE, or nil."
  (seq-find (lambda (b) (equal (buffer-local-value 'superchat-chat--tape b) tape))
            (buffer-list)))

(defun superchat-chat--in (tape function)
  "A function that calls FUNCTION in the chat buffer of TAPE as it is when
the function is called: a buffer killed and opened again gets the rest of a run."
  (lambda (&rest args)
    (when-let* ((buffer (superchat-chat--buffer tape)))
      (with-current-buffer buffer (apply function args)))))

;;; Sending

(defun superchat-chat-send (text &optional origin)
  "In a chat buffer, send TEXT as the user's request; ORIGIN goes on its entry.
A line starting with / is a command.  Return nil when nothing was sent."
  (unless superchat-chat--tape (user-error "Not a Superchat chat buffer"))
  (cond
   ((string-prefix-p "/" text)
    (if-let* ((command (and (string-match "\\`/\\([^ \t\n]*\\)[ \t\n]*" text)
                            (assoc (match-string 1 text) superchat-chat--commands))))
        (progn (funcall (cadr command) (string-trim (substring text (match-end 0)))) t)
      (message "Unknown command %s; /help lists them" (car (split-string text)))
      nil))
   ((superchat-chat--busy) nil)
   (t
    (let ((tape superchat-chat--tape)
          (origin (or origin superchat-chat--origin)))
      (setq superchat-chat--origin nil)
      (unwind-protect
          (apply #'superchat-run superchat-chat--tape text
                         :origin origin :model superchat-chat--model
                         :on-text (superchat-chat--in tape #'superchat-chat--on-text)
                         :on-entry (superchat-chat--in tape #'superchat-chat--add)
                         :on-done (superchat-chat--in
                                   tape (lambda (run)
                                            (superchat-chat--unstream)
                                            (superchat-chat--status)
                                            (when (eq (plist-get run :status) 'error)
                                              (message "Request failed: %s"
                                                       (plist-get run :error)))
                                            (superchat-chat--release
                                             (eq (plist-get run :status) 'done))))
                         ;; The first request of a timeline fixes its scope.
                         (and superchat-chat--pending-scope
                              (not (superchat-tape-entries :tape tape :limit 1))
                              (list :scope superchat-chat--pending-scope)))
        (superchat-chat--status))
      t))))

(defun superchat-chat--enqueue (text)
  "Queue TEXT, sent when the timeline is free."
  (push text superchat-chat--queue)
  (superchat-chat--status))

(defun superchat-chat--release (sent)
  "The timeline is free: send the queue when SENT or forced, else put its
text back at the start of the input and say so."
  (when superchat-chat--queue
    (let ((text (string-join (reverse superchat-chat--queue) "\n\n"))
          (force superchat-chat--force))
      (setq superchat-chat--queue nil superchat-chat--force nil)
      (if (or sent force)
          (superchat-chat-send text)
        (superchat-chat--edit
         (lambda ()
           (goto-char superchat-chat--input)
           (insert text (if (= (point) (point-max)) "" "\n\n"))))
        (message "The queued request was not sent; it is back in the input"))
      (superchat-chat--status)))
  (setq superchat-chat--force nil))

(defun superchat-chat--send-input (&optional force)
  "Send the input area; keep it when nothing was sent.  While the timeline is
busy, queue it, or with FORCE cancel the run and send the queue and the input."
  (interactive "P")
  (let* ((buffer (current-buffer))
         (text (string-trim (buffer-substring-no-properties superchat-chat--input
                                                            (point-max))))
         (busy (superchat-run-active superchat-chat--tape))
         (clear (lambda () (with-current-buffer buffer
                             (delete-region superchat-chat--input (point-max))))))
    (cond
     ((and force busy)
      (unless (string-empty-p text) (superchat-chat--enqueue text))
      (funcall clear)
      (setq superchat-chat--force t)
      (superchat-chat--cancel))
     ((string-empty-p text) nil)
     ((and busy (not (string-prefix-p "/" text)))
      (superchat-chat--enqueue text)
      (funcall clear))
     ;; The input goes only once the request is on its way: an error or a
     ;; refusal leaves it where it was.  Entries are inserted above the marker.
     ;; A command may have gone to another chat buffer: this one's input goes.
     ((superchat-chat-send text) (funcall clear)))))

(defun superchat-chat--return ()
  "Send on the last line of the input, else insert a newline."
  (interactive)
  (if (and (>= (point) superchat-chat--input) (= (line-end-position) (point-max)))
      (superchat-chat--send-input)
    (newline)))

(defun superchat-chat--cancel ()
  "Cancel the run of this timeline."
  (interactive)
  (if-let* ((run (superchat-run-active superchat-chat--tape)))
      ;; A cancelled handoff has no callback to show its event, nor to free
      ;; the queue: render it and free it here.
      (progn (superchat-cancel superchat-chat--tape)
             (superchat-chat--render)
             (superchat-chat--status)
             (when (eq (plist-get run :status) 'handoff) (superchat-chat--release nil)))
    (message "Nothing is running in this conversation")))

(defun superchat-chat--cycle ()
  "Org's cycling at point; the subtree of the last heading ends at the
separator, so the prompt and the input never fold."
  (interactive)
  (save-restriction
    (when (< (point) superchat-chat--block)
      (narrow-to-region (point-min) superchat-chat--block))
    (call-interactively #'org-cycle)))

(defun superchat-chat--self-insert ()
  "Insert the character typed; above the input area, at the end of the input."
  (interactive)
  (when (< (point) superchat-chat--input) (goto-char (point-max)))
  (call-interactively #'org-self-insert-command))

(defvar-keymap superchat-chat-mode-map
  "RET" #'superchat-chat--return
  "TAB" #'superchat-chat--cycle
  "C-c C-c" #'superchat-chat--send-input
  "C-c C-k" #'superchat-chat--cancel
  "<remap> <self-insert-command>" #'superchat-chat--self-insert)

(define-derived-mode superchat-chat-mode org-mode "Superchat"
  "Major mode of a Superchat conversation: entries above, input below."
  (setq superchat-chat--turn (point-min-marker)
        superchat-chat--stream (point-min-marker)
        superchat-chat--stream-end (point-min-marker)
        superchat-chat--block (point-min-marker)
        superchat-chat--input (point-min-marker)
        superchat-chat--answers (make-hash-table :test #'equal)
        superchat-chat--waiting (make-overlay (point-min) (point-min)))
  (add-hook 'kill-buffer-hook #'superchat-chat--stop nil t))

;;; Opening

(defun superchat-chat-open (tape &optional scope)
  "The chat buffer of timeline TAPE, made and rendered when there is none.
SCOPE is the scope of a timeline that has no entry yet."
  (or (superchat-chat--buffer tape)
      (with-current-buffer (generate-new-buffer "*superchat: new*")
        (superchat-chat-mode)
        (setq superchat-chat--tape tape
              superchat-chat--pending-scope (or scope (superchat-tape-scope-of default-directory)))
        (superchat-chat--render)
        (superchat-chat--status)
        (rename-buffer (superchat-chat--name tape) t)
        (current-buffer))))

(defun superchat-chat-recent-tape (scope)
  "The most recently active timeline of SCOPE that is not ephemeral, or nil."
  ;; Not the `timelines' view: it answers with the first entry of each timeline
  ;; and has a limit, and this needs only the newest name.
  (caar (superchat-tape-select
         (concat "SELECT tape FROM entry WHERE scope = ? AND tape NOT IN"
                 " (SELECT tape FROM entry WHERE kind = 'user'"
                 " AND json_extract(data, '$.ephemeral') = 1)"
                 " GROUP BY tape ORDER BY MAX(id) DESC LIMIT 1")
         (list scope))))

(defun superchat-chat--tape-here ()
  "The timeline of this buffer's own conversation, or nil when it has none.
A conversation belongs to the buffer it was started from: the chat buffer
itself, a chat buffer opened from here with nothing sent yet, else the most
recently active timeline of this scope that has a `user' entry whose origin is
this file, or this buffer when it visits none."
  (let ((origin (superchat-chat--origin-here)))
    (cl-flet ((mine (o) (if (plist-get origin :file)
                            (equal (plist-get o :file) (plist-get origin :file))
                          (and (null (plist-get o :file))
                               (equal (plist-get o :buffer) (plist-get origin :buffer))))))
      (or superchat-chat--tape
          (cl-some (lambda (b)
                     (with-current-buffer b
                       (and superchat-chat--tape superchat-chat--origin
                            (mine superchat-chat--origin)
                            (not (superchat-tape-entries :tape superchat-chat--tape :limit 1))
                            superchat-chat--tape)))
                   (buffer-list))
          (caar (superchat-tape-select
                 (concat "SELECT tape FROM entry WHERE scope = ? AND tape IN"
                         " (SELECT tape FROM entry WHERE kind = 'user' AND "
                         (if (plist-get origin :file)
                             "json_extract(data, '$.origin.file') = ?"
                           "json_extract(data, '$.origin.file') IS NULL AND json_extract(data, '$.origin.buffer') = ?")
                         ") AND tape NOT IN (SELECT tape FROM entry WHERE kind = 'user'"
                         " AND json_extract(data, '$.ephemeral') = 1)"
                         " GROUP BY tape ORDER BY MAX(id) DESC LIMIT 1")
                 (list (superchat-chat--scope)
                       (or (plist-get origin :file) (plist-get origin :buffer)))))))))

;;;###autoload
(defun superchat (&optional new)
  "Open the chat of this buffer's own conversation, or a new one when it has
none.  With a prefix argument NEW, always a new one."
  (interactive "P")
  (superchat-plugin-enable-configured)
  (let* ((origin (and (not superchat-chat--tape) (superchat-chat--origin-here)))
         (scope (superchat-chat--scope))
         (tape (or (and (not new) (superchat-chat--tape-here))
                   (superchat-tape-new-name)))
         (buffer (superchat-chat-open tape scope)))
    (when origin
      (with-current-buffer buffer (setq superchat-chat--origin origin)))
    (pop-to-buffer buffer)))

;;; Commands

(defun superchat-chat--show (title text)
  "Show TEXT in a read-only buffer named after TITLE."
  (with-current-buffer (get-buffer-create (format "*superchat-view: %s*" title))
    (let ((inhibit-read-only t))
      (special-mode)
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(defun superchat-chat--show-entries (title entries)
  "Show ENTRIES, rendered, in a read-only buffer named after TITLE."
  (superchat-chat--show title (if entries (superchat-view-render entries) "No entries.")))

(defun superchat-chat--title (text)
  "The title of a note TEXT: its first 60 characters."
  (substring text 0 (min 60 (length text))))

;;;###autoload
(defun superchat-note (text)
  "Keep TEXT as a note of this buffer's scope, as the model's note tool does."
  (interactive (list (read-string "Note: ")))
  (when (string-empty-p (string-trim text)) (user-error "A note needs text"))
  (let* ((scope (superchat-chat--scope))
         (tape (or (superchat-chat-recent-tape scope) (superchat-tape-new-name))))
    (superchat-recall-note tape (superchat-chat--title text) text nil scope)
    (when-let* ((buffer (superchat-chat--buffer tape)))
      (with-current-buffer buffer (superchat-chat--add nil)))))

;;;###autoload
(defun superchat-recall (query)
  "Show what the Tape has on QUERY in this buffer's scope.
With no QUERY, show the notes."
  (interactive (list (read-string "Recall (empty for notes): ")))
  (let ((scope (superchat-chat--scope)))
    (if (string-empty-p query)
        (superchat-chat--show-entries "notes" (superchat-view 'notes :scope scope))
      (superchat-chat--show-entries
       (format "recall %s" query) (superchat-view 'search :query query :scope scope)))))

(defun superchat-chat--note (text)
  "Append TEXT as a note to this timeline and show it."
  (when (string-empty-p text) (user-error "/note needs text"))
  (superchat-recall-note superchat-chat--tape (superchat-chat--title text)
                         text nil (superchat-chat--scope))
  (superchat-chat--add nil))

(defun superchat-chat--switch (buffer)
  "Show chat BUFFER in the selected window, in place of this conversation.
`set-window-buffer', not `switch-to-buffer': a rule of the user's
`display-buffer-alist' for chat buffers must not send it to another window.
The window keeps its history, so a window tab line lists the conversations
it showed."
  (set-window-buffer nil buffer)
  (set-buffer buffer))

(defun superchat-chat--resume (_)
  "Choose a timeline of this scope and open it."
  (let ((choices (mapcar (lambda (e)
                           (cons (format "%s  [%s]"
                                         (superchat-chat--line (plist-get e :text) 60)
                                         (plist-get e :tape))
                                 (plist-get e :tape)))
                         (superchat-view 'timelines :scope (superchat-chat--scope)))))
    (unless choices (user-error "No timelines in this scope"))
    (superchat-chat--switch
     (superchat-chat-open
      (cdr (assoc (completing-read "Resume: " choices nil t) choices))))))

(defun superchat-chat--fork (_)
  "Fork at the entry at point and open the new timeline."
  (let ((id (if (>= (point) superchat-chat--input)
                (and (> superchat-chat--shown 0) superchat-chat--shown)
              (get-text-property (point) 'superchat-chat-id))))
    (unless id (user-error "No entry at point"))
    (superchat-chat--switch (superchat-chat-open (superchat-fork superchat-chat--tape id)))))

(defun superchat-chat--handoff (name)
  "Summarise the conversation into an anchor named NAME; refused when busy."
  (unless (superchat-chat--busy)
    (superchat-handoff superchat-chat--tape (and (not (string-empty-p name)) name)
                     (superchat-chat--in superchat-chat--tape
                                         (lambda (anchor)
                                           (superchat-chat--render)
                                           (superchat-chat--status)
                                           (message (if anchor "Handoff made" "Handoff failed"))
                                           (superchat-chat--release anchor)))))
  (superchat-chat--status))

(defun superchat-chat--level (name)
  "Show the permission level, or set it to NAME."
  (if (string-empty-p name)
      (message "Permission level: %s" (superchat-permission-level superchat-chat--tape))
    (superchat-permission-set superchat-chat--tape (intern name))
    (superchat-chat--add nil)))

(defun superchat-chat--model-command (name)
  "Show the model of this buffer, or set it to NAME."
  (if (string-empty-p name)
      (message "Model: %s" (or superchat-chat--model superchat-llm-model "not set"))
    (let ((known (superchat-llm-model-ids)))
      (unless (member name known)
        (user-error "Unknown model %S of %s; one of: %s"
                    name superchat-llm-provider (string-join known ", "))))
    (setq superchat-chat--model name)
    (superchat-chat--status)
    (message "Model of this buffer: %s" name)))

(defun superchat-chat--help (_)
  "List the commands."
  (superchat-chat--show "help"
                        (mapconcat (lambda (c) (format "/%s  %s" (car c) (cddr c)))
                                   superchat-chat--commands "\n")))

(superchat-command-define
 "new" (lambda (_)
         (superchat-chat--switch (superchat-chat-open (superchat-tape-new-name) (superchat-chat--scope))))
 "Start a new timeline in this scope, in this window.")
(superchat-command-define "resume" #'superchat-chat--resume "Open another timeline of this scope.")
(superchat-command-define "fork" #'superchat-chat--fork
                          "Fork at the entry at point and open the new timeline.")
(superchat-command-define "handoff" #'superchat-chat--handoff
                          "[name] Summarise this conversation into an anchor.")
(superchat-command-define
 "topic" (lambda (name)
           (when (string-empty-p name) (user-error "/topic needs a name"))
           (superchat-chat--show-entries
            (concat "topic " name)
            (superchat-view 'topic :name name :scope (superchat-chat--scope))))
 "name. Show the entries of the topic.")
(superchat-command-define "recall" #'superchat-recall
                          "[query] Search the Tape of this scope; with none, show the notes.")
(superchat-command-define
 "notes" (lambda (_) (superchat-chat--show-entries
                      "notes" (superchat-view 'notes :scope (superchat-chat--scope))))
 "Show the notes of this scope.")
(superchat-command-define "note" #'superchat-chat--note
                          "text. Keep TEXT as a note, as the note tool does.")
(superchat-command-define "level" #'superchat-chat--level
                          "[read-only|ask|auto] Show or set the permission level.")
(superchat-command-define "model" #'superchat-chat--model-command
                          "[name] Show or set the model of this buffer.")
(superchat-command-define "help" #'superchat-chat--help "List the commands.")

(provide 'superchat-chat)
;;; superchat-chat.el ends here
