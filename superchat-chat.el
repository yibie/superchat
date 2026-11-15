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
(defvar-local superchat-chat--stream nil "Marker: where the turn's text ends and streamed text starts.")
(defvar-local superchat-chat--block nil "Marker: where the separator and the prompt start.")
(defvar-local superchat-chat--input nil "Marker: where the input area starts.")
(defvar-local superchat-chat--answers nil "Hash table: an assistant entry's id to its text as Org.")
(defvar-local superchat-chat--waiting nil "Overlay of the waiting line, above the separator.")
(defvar-local superchat-chat--timer nil "Timer that keeps the waiting line and the mode line current.")
(defvar-local superchat-chat--began nil "When the timeline became busy.")
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

(defun superchat-chat--org (markdown)
  "MARKDOWN as Org, by the rules of the specification and no others.
Headings are demoted so that the shallowest is level 2, under the Assistant
heading.  What no rule may touch (blocks, inline code, links) is kept out
behind placeholders of private-use characters, which no rule matches."
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
        (each "^\\([ \t]*\\)[-*+] " (lambda () (replace-match "\\1- " t)))
        ;; Heading stars are private-use characters until the emphasis is done.
        (each "^\\(#\\{1,6\\}\\) " (lambda () (setq top (min top (length (match-string 1))))))
        (each "^\\(#\\{1,6\\}\\) "
              (lambda () (replace-match (concat (make-string (- (length (match-string 1)) top -2) ?\uE003)
                                                " ")
                                        t t)))
        (each "^>.*\\(?:\n>.*\\)*"
              (lambda () (replace-match (concat "#+begin_quote\n"
                                                (replace-regexp-in-string "^> ?" "" (match-string 0))
                                                "\n#+end_quote")
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

(defun superchat-chat--args (args)
  "ARGS on one line, cut to 60 characters."
  (let ((line (if args (let ((print-escape-newlines t))
                         (string-trim (format "%S" args) "(" ")"))
                "")))
    (if (> (length line) 60) (concat (substring line 0 60) "…") line)))

(defun superchat-chat--heading (head body key id)
  "HEAD, a heading line, and BODY, folded under it by KEY; read-only, with ID."
  (let ((head (copy-sequence head)))
    (put-text-property 0 1 'superchat-chat-fold (cons key (length body)) head)
    (superchat-chat--seal (concat head body "\n") id)))

(defun superchat-chat--call (call level)
  "The heading at LEVEL of CALL, (:call :result :seconds :done :id), and its
body; it carries the id of its tool entry, or of its assistant entry before."
  (let* ((result (plist-get call :result))
         (args (plist-get (plist-get call :call) :args))
         (short (superchat-chat--args args)))
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
     (plist-get (plist-get call :call) :id)
     (plist-get (or result call) :id))))

(defun superchat-chat--calls (calls)
  "The text of CALLS, consecutive tool calls of one turn: those of the rounds
answered in full that have no error go under one heading when they are two."
  (let* ((good (seq-filter (lambda (c) (and (plist-get c :done)
                                             (not (plist-get (plist-get (plist-get c :result) :data)
                                                             :error))))
                           calls))
         (names (seq-uniq (mapcar (lambda (c) (plist-get (plist-get c :call) :name)) good))))
    (if (< (length good) 2)
        (mapconcat (lambda (c) (superchat-chat--call c 2)) calls "")
      (concat
       (superchat-chat--heading
        (format "** ▸ %d calls: %s" (length good)
                (mapconcat (lambda (name)
                             (let ((k (seq-count (lambda (c) (equal (plist-get (plist-get c :call) :name)
                                                                    name))
                                                 good)))
                               (if (> k 1) (format "%s ×%d" name k) name)))
                           names ", "))
        (concat "\n" (string-remove-suffix
                      "\n" (mapconcat (lambda (c) (superchat-chat--call c 3)) good "")))
        (concat "calls " (plist-get (plist-get (car good) :call) :id))
        (plist-get (plist-get (car good) :result) :id))
       (mapconcat (lambda (c) (superchat-chat--call c 2)) (seq-difference calls good #'eq) "")))))

(defun superchat-chat--answer (entry)
  "The text of assistant ENTRY as Org, converted once per buffer."
  (with-memoization (gethash (plist-get entry :id) superchat-chat--answers)
    (superchat-chat--org (string-trim (plist-get entry :text)))))

(defun superchat-chat--format (entry)
  "The text of ENTRY, other than a tool call, read-only and carrying its id."
  (let* ((text (string-trim-right (plist-get entry :text)))
         (data (plist-get entry :data))
         (string
          (pcase (plist-get entry :kind)
            ;; A line of the user's that starts with stars would be a heading of
            ;; the buffer: one space keeps it the user's text.
            ('user (concat "* You\n" (replace-regexp-in-string "^\\*" " *" text) "\n\n"))
            ('assistant (concat "* Assistant\n"
                                (unless (string-empty-p (string-trim text))
                                  (concat (superchat-chat--answer entry) "\n\n"))))
            ('anchor (concat (superchat-chat--dim (format "── summary: %s ──" (plist-get data :name)))
                             (if (string-empty-p text) "\n\n" (concat "\n" text "\n\n"))
                             (let ((next (string-trim (or (plist-get data :next) ""))))
                               (unless (string-empty-p next)
                                 (concat "What comes next: " next "\n\n")))))
            ('note (concat (superchat-chat--dim (format "note: %s" (plist-get data :title)))
                           "\n" text "\n\n"))
            (_ (superchat-chat--dim (concat text "\n")
                                    (if (equal (plist-get data :type) "error") 'error 'shadow))))))
    (superchat-chat--seal string (plist-get entry :id))))

(defun superchat-chat--turn-text (entries)
  "The text of ENTRIES, one turn.  A call is shown where its assistant entry
is, with the result its tool entry brought, which shows nothing of its own."
  (let ((results (make-hash-table :test #'equal)) (before nil) (out nil) (calls nil))
    (dolist (entry entries)
      (when (eq (plist-get entry :kind) 'tool)
        (puthash (plist-get (plist-get entry :data) :call-id)
                 (cons entry (if before (- (plist-get entry :at) (plist-get before :at)) 0))
                 results))
      (setq before entry))
    (cl-flet ((flush () (when calls
                          (push (superchat-chat--calls (nreverse calls)) out)
                          (setq calls nil))))
      (dolist (entry entries)
        (pcase (plist-get entry :kind)
          ('tool nil)
          ('assistant
           (let* ((own (plist-get (plist-get entry :data) :calls))
                  (said (not (string-empty-p (string-trim (plist-get entry :text)))))
                  (done (seq-every-p (lambda (c) (gethash (plist-get c :id) results)) own)))
             (when said (flush))
             (when (or said (and own (not calls)))
               (push (superchat-chat--format entry) out))
             (dolist (call own)
               (let ((result (gethash (plist-get call :id) results)))
                 (push (list :call call :result (car result) :seconds (cdr result) :done done
                             :id (plist-get entry :id))
                       calls)))))
          (_ (flush) (push (superchat-chat--format entry) out))))
      (flush))
    (apply #'concat (nreverse out))))

(defun superchat-chat--folds (from to)
  "The (POSITION KEY . LENGTH) of each heading between FROM and TO that folds."
  (let ((at from) folds)
    (while (setq at (text-property-not-all at to 'superchat-chat-fold nil))
      (push (cons at (get-text-property at 'superchat-chat-fold)) folds)
      (setq at (1+ at)))
    folds))

(defun superchat-chat--eol (position)
  "The end of the line of POSITION."
  (save-excursion (goto-char position) (line-end-position)))

(defun superchat-chat--opened (from to)
  "The keys of the headings between FROM and TO that are open."
  (cl-loop for (at key . length) in (superchat-chat--folds from to)
           unless (or (= length 0) (org-fold-folded-p (superchat-chat--eol at))) collect key))

(defun superchat-chat--fold (from to opened)
  "Fold the headings between FROM and TO, but those whose key is in OPENED."
  (pcase-dolist (`(,at ,key . ,length) (superchat-chat--folds from to))
    (unless (or (member key opened) (= length 0))
      (let ((eol (superchat-chat--eol at)))
        (org-fold-region eol (+ eol length) t 'outline)))))

(defun superchat-chat--edit (function)
  "Call FUNCTION with the buffer writable.  The user's point and that of every
window showing the buffer keep their distance from the end in the input area,
their position above it."
  (let* ((inhibit-read-only t)
         ;; In the entries a position, for a turn drawn again; below them a
         ;; marker, which moves with what is inserted above it.
         (keep (lambda (at) (cond ((>= at superchat-chat--input) (- at (point-max)))
                                  ((< at superchat-chat--stream) at)
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

(defun superchat-chat--draw ()
  "Draw the current turn again from its entries; a heading open stays open."
  (let ((opened (superchat-chat--opened superchat-chat--turn superchat-chat--stream)))
    (delete-region superchat-chat--turn superchat-chat--stream)
    (superchat-chat--put superchat-chat--turn (superchat-chat--turn-text superchat-chat--entries)
                         superchat-chat--turn)
    (superchat-chat--fold superchat-chat--turn superchat-chat--stream opened)))

(defconst superchat-chat--prompt
  (superchat-chat--seal (concat (superchat-chat--dim (make-string 40 ?─)) "\n"
                                (superchat-chat--dim "❯ " 'bold)))
  "The separator line and the prompt, between the entries and the input.")

(defun superchat-chat--render ()
  "Show the whole `conversation' view again, keeping the unsent input."
  (superchat-chat--edit
   (lambda ()
     (let ((input (buffer-substring superchat-chat--input (point-max)))
           (opened (superchat-chat--opened (point-min) superchat-chat--stream))
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
       (dolist (turn (butlast turns)) (insert (superchat-chat--turn-text turn)))
       (set-marker superchat-chat--turn (point))
       (insert (superchat-chat--turn-text superchat-chat--entries))
       (set-marker superchat-chat--stream (point))
       (set-marker superchat-chat--block (point))
       (insert superchat-chat--prompt)
       (set-marker superchat-chat--input (point))
       (insert input)
       (superchat-chat--fold (point-min) superchat-chat--stream opened)))))

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
                 (and run (not (eq (plist-get run :status) 'handoff))
                      (superchat-chat--waiting-line now)))
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
             ('user (set-marker superchat-chat--turn superchat-chat--stream)
                    (setq superchat-chat--entries nil))
             ;; The entry replaces what streamed.
             ('assistant (delete-region superchat-chat--stream superchat-chat--block)))
           (setq superchat-chat--entries (append superchat-chat--entries (list entry))
                 superchat-chat--shown (plist-get entry :id)))
         (superchat-chat--draw)))))
    (when (seq-find (lambda (e) (eq (plist-get e :kind) 'user)) new)
      (rename-buffer (superchat-chat--name superchat-chat--tape) t)))
  (superchat-chat--status))

(defun superchat-chat--on-text (text)
  "Show streamed TEXT as it comes, raw, below the entries."
  (superchat-chat--edit
   (lambda ()
     (when (= superchat-chat--stream superchat-chat--block)
       (superchat-chat--put superchat-chat--block (superchat-chat--seal "* Assistant\n")
                            superchat-chat--turn superchat-chat--stream))
     (superchat-chat--put superchat-chat--block
                          (superchat-chat--seal (superchat-chat--dim (copy-sequence text)))
                          superchat-chat--turn superchat-chat--stream))))

(defun superchat-chat--unstream ()
  "Remove the streamed text that never became an entry."
  (superchat-chat--edit
   (lambda () (delete-region superchat-chat--stream superchat-chat--block))))

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
                                                       (plist-get run :error)))))
                         ;; The first request of a timeline fixes its scope.
                         (and superchat-chat--pending-scope
                              (not (superchat-tape-entries :tape tape :limit 1))
                              (list :scope superchat-chat--pending-scope)))
        (superchat-chat--status))
      t))))

(defun superchat-chat--send-input ()
  "Send the input area; keep it when nothing was sent."
  (interactive)
  (let ((buffer (current-buffer))
        (text (string-trim (buffer-substring-no-properties superchat-chat--input
                                                           (point-max)))))
    ;; The input goes only once the request is on its way: an error or a
    ;; refusal leaves it where it was.  Entries are inserted above the marker.
    ;; A command may have gone to another chat buffer: this one's input goes.
    (when (and (not (string-empty-p text)) (superchat-chat-send text))
      (with-current-buffer buffer
        (delete-region superchat-chat--input (point-max))))))

(defun superchat-chat--return ()
  "Send on the last line of the input, else insert a newline."
  (interactive)
  (if (and (>= (point) superchat-chat--input) (= (line-end-position) (point-max)))
      (superchat-chat--send-input)
    (newline)))

(defun superchat-chat--cancel ()
  "Cancel the run of this timeline."
  (interactive)
  (if (superchat-run-active superchat-chat--tape)
      ;; A cancelled handoff has no callback to show its event: render it.
      (progn (superchat-cancel superchat-chat--tape)
             (superchat-chat--render)
             (superchat-chat--status))
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
        superchat-chat--block (point-min-marker)
        superchat-chat--input (point-min-marker)
        superchat-chat--answers (make-hash-table)
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

;;;###autoload
(defun superchat (&optional new)
  "Open the chat of the most recent timeline of this buffer's project.
With a prefix argument NEW, start a new timeline."
  (interactive "P")
  (superchat-plugin-enable-configured)
  (let* ((origin (and (not superchat-chat--tape) (superchat-chat--origin-here)))
         (scope (superchat-chat--scope))
         (tape (or (and (not new) (superchat-chat-recent-tape scope))
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

(defun superchat-chat--resume (_)
  "Choose a timeline of this scope and open it."
  (let ((choices (mapcar (lambda (e)
                           (cons (format "%s  [%s]"
                                         (superchat-chat--line (plist-get e :text) 60)
                                         (plist-get e :tape))
                                 (plist-get e :tape)))
                         (superchat-view 'timelines :scope (superchat-chat--scope)))))
    (unless choices (user-error "No timelines in this scope"))
    (pop-to-buffer
     (superchat-chat-open
      (cdr (assoc (completing-read "Resume: " choices nil t) choices))))))

(defun superchat-chat--fork (_)
  "Fork at the entry at point and open the new timeline."
  (let ((id (if (>= (point) superchat-chat--input)
                (and (> superchat-chat--shown 0) superchat-chat--shown)
              (get-text-property (point) 'superchat-chat-id))))
    (unless id (user-error "No entry at point"))
    (pop-to-buffer (superchat-chat-open (superchat-fork superchat-chat--tape id)))))

(defun superchat-chat--handoff (name)
  "Summarise the conversation into an anchor named NAME; refused when busy."
  (unless (superchat-chat--busy)
    (superchat-handoff superchat-chat--tape (and (not (string-empty-p name)) name)
                     (superchat-chat--in superchat-chat--tape
                                         (lambda (anchor)
                                           (superchat-chat--render)
                                           (superchat-chat--status)
                                           (message (if anchor "Handoff made" "Handoff failed"))))))
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
         (pop-to-buffer (superchat-chat-open (superchat-tape-new-name) (superchat-chat--scope))))
 "Start a new timeline in this scope, in a new buffer.")
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
