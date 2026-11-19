;;; superchat-turn.el --- The text of one turn of a conversation -*- lexical-binding: t; -*-

;;; Commentary:
;; A turn of the chat buffer as text: the entries of one turn in, the Org text
;; out.  Nothing here reads a buffer but a temporary one, for the Markdown.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'superchat)

(defvar superchat-turn-answers (make-hash-table :test #'equal)
  "Hash table: an assistant entry's id and its LEVEL to its text as Org.")

;;; Markdown as Org

(defun superchat-turn--markdown (markdown level)
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
                ;; `string-trim' matches a regexp: the match data must outlive it.
                (let ((code (save-match-data (string-trim (match-string 1)))))
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
                            (concat "\uE003" (save-match-data (string-trim (match-string 2))) "\uE003"))
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

;;; The text of a turn

(defun superchat-turn-seal (string &optional id)
  "Make STRING read-only, carrying ID.  Text typed right after it is free and
plain: its last character passes on no property, neither a face nor the id."
  (unless (string-empty-p string)
    (add-text-properties 0 (length string)
                         (list 'read-only t 'front-sticky '(read-only) 'superchat-turn-id id)
                         string)
    (add-text-properties (1- (length string)) (length string) '(rear-nonsticky t) string))
  string)

(defun superchat-turn-dim (string &optional face)
  "STRING in FACE, `shadow' by default, as a face Org's fontification keeps."
  (propertize string 'font-lock-face (or face 'shadow)))

(defun superchat-turn--loose (string)
  "STRING with `superchat-turn-loose' 1, a line that stops every body, wherever
it has none."
  (let ((at 0))
    (while (< at (length string))
      (let ((next (next-single-property-change at 'superchat-turn-loose string (length string))))
        (unless (get-text-property at 'superchat-turn-loose string)
          (put-text-property at next 'superchat-turn-loose 1 string))
        (setq at next)))
    string))

(defun superchat-turn-cut (line width)
  "LINE cut to WIDTH characters and `…' when longer."
  (if (> (length line) width) (concat (substring line 0 width) "…") line))

(defun superchat-turn--args (args)
  "ARGS on one line, cut to 60 characters."
  (superchat-turn-cut (if args (let ((print-escape-newlines t))
                                 (string-trim (format "%S" args) "(" ")"))
                        "")
                      60))

(defun superchat-turn--heading (head body key id folded)
  "HEAD, a heading line, and BODY under it, read-only, with ID.  KEY names the
heading when its turn is drawn again; FOLDED is how it is shown by default."
  (let ((head (copy-sequence head)))
    (put-text-property 0 1 'superchat-turn-fold (list key folded) head)
    (superchat-turn-seal (concat head body "\n") id)))

(defun superchat-turn--call (call level)
  "The heading at LEVEL of CALL, (:call :result :seconds :id :asked), and its
body, after the lines of the permission events ASKED for it; it carries the id
of its tool entry, or of its assistant entry before."
  (let* ((result (plist-get call :result))
         (args (plist-get (plist-get call :call) :args))
         (short (superchat-turn--args args)))
    (concat
     ;; A line no call's body takes in: it ends the body of the call before.
     (propertize (mapconcat #'superchat-turn--format (plist-get call :asked) "")
                 'superchat-turn-loose level)
     (superchat-turn--heading
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

(defun superchat-turn--answer (entry level)
  "The text of assistant ENTRY as Org, its headings from LEVEL or, with no
LEVEL, bold lines; converted once per buffer."
  (with-memoization (gethash (cons (plist-get entry :id) level) superchat-turn-answers)
    (superchat-turn--markdown (string-trim (plist-get entry :text)) level)))

(defun superchat-turn--format (entry)
  "The text of ENTRY, a `user', `anchor', `note' or `event' entry, read-only
and carrying its id."
  (let* ((text (string-trim-right (plist-get entry :text)))
         (data (plist-get entry :data))
         (string
          (pcase (plist-get entry :kind)
            ;; A line of the user's that starts with stars would be a heading of
            ;; the buffer: one space keeps it the user's text.
            ('user (concat "* You\n" (replace-regexp-in-string "^\\*" " *" text) "\n\n"))
            ('anchor (concat (superchat-turn-dim (format "── summary: %s ──" (plist-get data :name)))
                             (if (string-empty-p text) "\n\n" (concat "\n" text "\n\n"))
                             (let ((next (string-trim (or (plist-get data :next) ""))))
                               (unless (string-empty-p next)
                                 (concat "What comes next: " next "\n\n")))))
            ;; A heading folded by default: a note often repeats the answer.
            ;; Its text is no line that stops a body, so the heading folds it.
            ('note (superchat-turn--heading
                    (superchat-turn-dim (format "** note: %s" (plist-get data :title)))
                    (propertize (concat "\n" (replace-regexp-in-string "^\\*" " *" text) "\n")
                                'superchat-turn-loose most-positive-fixnum)
                    (format "note %s" (plist-get entry :id)) (plist-get entry :id) t))
            (_ (superchat-turn-dim (concat text "\n")
                                   (if (equal (plist-get data :type) "error") 'error 'shadow))))))
    (superchat-turn-seal string (plist-get entry :id))))

(defun superchat-turn-strip (entries &optional asking)
  "One cell for each call and intermediate text of the turn ENTRIES, coloured
by how it went; with ASKING, the first call with no result waits for the
person.  Each cell carries its face twice: Org's heading face would hide it."
  (let ((results (make-hash-table :test #'equal)) cells open)
    (dolist (entry entries)
      (when (eq (plist-get entry :kind) 'tool)
        (puthash (plist-get (plist-get entry :data) :call-id) entry results)))
    (cl-flet ((cell (char face) (push (propertize char 'face face 'superchat-turn-cell face) cells)))
      (dolist (entry entries)
        ;; An entry with calls is never the final answer.
        (when-let* ((calls (and (eq (plist-get entry :kind) 'assistant)
                                (plist-get (plist-get entry :data) :calls))))
          (unless (string-empty-p (string-trim (plist-get entry :text))) (cell "•" 'shadow))
          (dolist (call calls)
            (let ((result (gethash (plist-get call :id) results)))
              (cond ((plist-get (plist-get result :data) :error) (cell "▮" 'error))
                    (result (cell "▮" 'shadow))
                    ((and asking (not open)) (cell "⏸" 'warning))
                    (t (cell "▮" 'default)))
              (unless result (setq open t)))))))
    (setq cells (nreverse cells))
    (apply #'concat (if (> (length cells) 60) (cons "…" (last cells 59)) cells))))

(defun superchat-turn-cells (limit)
  "Font-lock matcher: the next run of strip cells of one face before LIMIT."
  (when-let* ((at (text-property-not-all (point) limit 'superchat-turn-cell nil)))
    (goto-char (next-single-property-change at 'superchat-turn-cell nil limit))
    (set-match-data (list at (point)))
    t))

(defun superchat-turn--progress (answers results asked done strip)
  "The Progress heading of the turn whose `assistant' entries other than the
final answer are ANSWERS, and what it holds: their texts and calls.  RESULTS
maps a call id to (TOOL ENTRY . SECONDS), ASKED to the permission events asked
for it; STRIP is the turn's strip.  It is folded once the turn is DONE,
unless a call failed."
  (let ((calls 0) (failed 0) inside waiting)
    (dolist (entry answers)
      (unless (string-empty-p (string-trim (plist-get entry :text)))
        ;; A heading of its own: after a call, it is not taken into the call's subtree.
        (let ((text (superchat-turn--answer entry nil)))
          (push (superchat-turn--heading
                 (concat "*** 💬 " (superchat-turn-cut (car (split-string text "\n")) 60))
                 (if (string-search "\n" text) (concat "\n" text) "")
                 (format "said %s" (plist-get entry :id)) (plist-get entry :id) nil)
                inside)))
      (dolist (call (plist-get (plist-get entry :data) :calls))
        (let ((result (gethash (plist-get call :id) results)))
          (cl-incf calls)
          (unless result (setq waiting t))
          (when (plist-get (plist-get (car result) :data) :error) (cl-incf failed))
          (push (superchat-turn--call (list :call call :result (car result) :seconds (cdr result)
                                            :id (plist-get entry :id)
                                            :asked (gethash (plist-get call :id) asked))
                                      3)
                inside))))
    (when inside
      (superchat-turn--heading
       (concat "** " (and waiting "⏳ ") "Progress"
               (when (> calls 0)
                 (format "  %s  %d call%s%s" strip calls (if (= calls 1) "" "s")
                         (if (> failed 0) (format ", %d failed" failed) ""))))
       (concat "\n" (string-remove-suffix "\n" (apply #'concat (nreverse inside))))
       (format "progress %s" (plist-get (car answers) :id))
       (plist-get (car answers) :id)
       (and done (= failed 0))))))

(defun superchat-turn-text (entries streaming over)
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
    (cl-flet ((others (user) (mapconcat #'superchat-turn--format
                                        (seq-filter (lambda (e) (eq (eq (plist-get e :kind) 'user) user))
                                                    (seq-remove (lambda (e) (memq (plist-get e :kind)
                                                                                  '(assistant tool)))
                                                                entries))
                                        "")))
      (cons (concat (others t)
                    (when (or answers streaming)
                      (concat (superchat-turn-seal "* Assistant\n" (plist-get (car answers) :id))
                              (superchat-turn--progress
                               (seq-remove (lambda (e) (eq e final)) answers) results asked
                               (or final over) (superchat-turn-strip entries))
                              (when (or final streaming)
                                (superchat-turn-seal "** Answer\n" (plist-get final :id))))))
            (let ((tail (concat (when final
                                  (superchat-turn-seal (concat (superchat-turn--answer final 3) "\n\n")
                                                       (plist-get final :id)))
                                ;; A line no heading's body takes in.
                                (superchat-turn--loose (others nil)))))
              ;; Streamed text has no end of line of its own.
              (if (and streaming (not (string-empty-p tail)))
                  (concat (superchat-turn-seal "\n\n") tail)
                tail))))))

(provide 'superchat-turn)


(provide 'superchat-turn)
;;; superchat-turn.el ends here
