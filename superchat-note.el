;;; superchat-note.el --- Your notes on the text, handed to the model together -*- lexical-binding: t; -*-

;;; Commentary:
;; You mark a piece of text in any buffer, a file or a diff, and write a note
;; on it.  The text is not changed.  Notes gather, in as many buffers as you
;; like; `superchat-note-attach' puts all of them on the message you are
;; writing in a conversation, as one attachment above where you write (see
;; `superchat-butler--attach').  What is attached follows the notes until it is
;; sent: a note changed or dropped meanwhile is sent as it then is.  Once sent,
;; the notes that went are gone.
;;
;; Notes on a file are kept in `superchat-note-file' and come back when the file
;; is visited again, found by the text they were on.  A note whose text is not
;; found any more keeps its words and where it was, and says so.  Notes on a
;; buffer visiting no file live as long as the buffer.
;;
;; The other way round, the model's remarks hung on the text, is
;; `superchat-annotate'.
;; docs/decisions/accepted/2026-11-06-your-notes-on-the-text.md

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

(defvar superchat-data-directory)
(defvar superchat-mode)
(defvar superchat--session-id)
(declare-function superchat "superchat" (&optional preset-name))
(declare-function superchat-butler--attach "superchat-butler"
                  (view run what words &optional sent))
(declare-function superchat-butler--conversation-in-use "superchat-butler"
                  (except &optional foreground))
(declare-function superchat-butler--quotable "superchat-butler" (text))
(declare-function superchat-butler--quoted "superchat-butler" (text width))
(declare-function superchat-butler--show-view "superchat-butler" (view))
(declare-function superchat-butler-attached-p "superchat-butler" (view run))
(declare-function superchat-butler-attached-refresh "superchat-butler" (run))
(declare-function superchat-view-session-configuration "superchat" (session-id))

(defgroup superchat-note nil
  "Your notes on the text, handed to the model together."
  :group 'superchat)

(defface superchat-note-face
  '((((background dark)) :background "#3a3420" :extend t)
    (t :background "#fff3c4" :extend t))
  "Face of a piece of text you wrote a note on."
  :group 'superchat-note)

(defcustom superchat-note-file nil
  "File the notes on files are kept in.
Nil means notes.eld in `superchat-data-directory'."
  :type '(choice (const :tag "In the data directory" nil) file)
  :group 'superchat-note)

(defcustom superchat-note-quoted-lines 30
  "At most this many lines of the text a note is on go with it to the model.
More are cut, and the message says how many lines it was."
  :type 'integer
  :group 'superchat-note)

(defconst superchat-note--attachment 'superchat-notes
  "What stands for the notes among what is attached to a message.")

;;;; What a note is ----------------------------------------------------------------

;; A note is a plist:
;;   :id     a string, never reused
;;   :text   what you wrote
;;   :time   when it was written, an ISO time string
;;   :file   the file it is on, or nil for a buffer visiting none
;;   :line   the line its text starts on, then
;;   :lines  how many lines its text is, then
;;   :quote  its text, then
;; A note placed in a buffer is also an overlay there, whose `superchat-note'
;; property is the plist; the overlay is where its text is now.

(defvar superchat-note--kept 'unread
  "Notes on files that are in no buffer now, by file: an alist.
`unread' until `superchat-note-file' is read.")

(defvar superchat-note--counter 0
  "Makes note ids unique within this Emacs.")

(defun superchat-note--file ()
  "The file the notes are kept in."
  (or superchat-note-file
      (expand-file-name "notes.eld"
                        (if (boundp 'superchat-data-directory)
                            superchat-data-directory
                          (expand-file-name "superchat/" user-emacs-directory)))))

(defun superchat-note--new-id ()
  "A new note id."
  (format "%s-%d-%04x" (format-time-string "%Y%m%dT%H%M%S")
          (cl-incf superchat-note--counter) (random #x10000)))

(defun superchat-note--overlays (&optional buffer beg end)
  "The note overlays in BUFFER (the current one), between BEG and END, in order."
  (with-current-buffer (or buffer (current-buffer))
    (sort (cl-remove-if-not (lambda (overlay) (overlay-get overlay 'superchat-note))
                            (overlays-in (or beg (point-min)) (or end (point-max))))
          (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun superchat-note--at (&optional position)
  "The note overlay at POSITION (point), the innermost when several are."
  (let* ((position (or position (point)))
         (here (cl-remove-if-not
                (lambda (overlay)
                  (and (overlay-get overlay 'superchat-note)
                       (<= (overlay-start overlay) position)
                       (or (< position (overlay-end overlay))
                           ;; A note whose text went: where it was.
                           (= (overlay-start overlay) (overlay-end overlay) position))))
                (overlays-in (max (point-min) (1- position))
                             (min (point-max) (1+ position))))))
    (car (sort here (lambda (a b)
                      (< (- (overlay-end a) (overlay-start a))
                         (- (overlay-end b) (overlay-start b))))))))

(defun superchat-note--refreshed (overlay)
  "The note of OVERLAY, with where its text is now."
  (let ((note (copy-sequence (overlay-get overlay 'superchat-note))))
    (with-current-buffer (overlay-buffer overlay)
      (let ((beg (overlay-start overlay))
            (end (overlay-end overlay)))
        (save-restriction
          (widen)
          (setq note (plist-put note :line (line-number-at-pos beg t)))
          (setq note (plist-put note :lines
                                (1+ (- (line-number-at-pos end t)
                                       (line-number-at-pos beg t)))))
          (setq note (plist-put note :quote
                                (buffer-substring-no-properties beg end)))
          (when (= beg end)
            (setq note (plist-put note :lost t)))
          (setq note (plist-put note :file
                                (and buffer-file-name
                                     (expand-file-name buffer-file-name)))))))
    note))

;;;; Keeping them -------------------------------------------------------------------

(defun superchat-note--read-kept ()
  "Read `superchat-note-file' once."
  (when (eq superchat-note--kept 'unread)
    (setq superchat-note--kept
          (let ((file (superchat-note--file)))
            (or (and (file-readable-p file)
                     (with-demoted-errors "superchat: notes not read: %S"
                       (with-temp-buffer
                         (insert-file-contents file)
                         (let ((data (read (current-buffer))))
                           (and (listp data) data)))))
                nil)))))

(defun superchat-note--live-files ()
  "Files some buffer has notes on now, with those notes: an alist."
  (let (alist)
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'buffer-file-name buffer)
        (let ((notes (mapcar #'superchat-note--refreshed
                             (superchat-note--overlays buffer))))
          (when notes
            (push (cons (expand-file-name (buffer-local-value 'buffer-file-name buffer))
                        notes)
                  alist)))))
    alist))

(defun superchat-note--save ()
  "Write every note on a file to `superchat-note-file'."
  (superchat-note--read-kept)
  (let* ((live (superchat-note--live-files))
         (all (append live
                      (cl-remove-if (lambda (entry) (assoc (car entry) live))
                                    superchat-note--kept)))
         (file (superchat-note--file)))
    ;; Nothing to keep, and nothing kept before: no file.
    (when (or all (file-exists-p file))
     (with-demoted-errors "superchat: notes not saved: %S"
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (let ((print-length nil) (print-level nil))
          (insert ";; Superchat: your notes on files.  -*- mode: lisp-data -*-\n")
          (prin1 all (current-buffer))
          (insert "\n")))))))

(defun superchat-note--keep-buffer ()
  "Before the buffer goes: its notes are kept, by file."
  (when (and buffer-file-name (superchat-note--overlays))
    (superchat-note--read-kept)
    (let ((file (expand-file-name buffer-file-name))
          (notes (mapcar #'superchat-note--refreshed (superchat-note--overlays))))
      (setq superchat-note--kept
            (cons (cons file notes)
                  (cl-remove-if (lambda (entry) (equal (car entry) file))
                                superchat-note--kept)))
      (mapc #'delete-overlay (superchat-note--overlays))
      (superchat-note--save))))

(defun superchat-note--find (quote line)
  "Where QUOTE is in this buffer, nearest LINE: (BEG . END), or nil."
  (unless (string-empty-p quote)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- (max 1 line)))
        (let* ((there (point))
               (after (save-excursion (and (search-forward quote nil t)
                                           (match-beginning 0))))
               (before (save-excursion (goto-char (min (point-max)
                                                       (+ there (length quote))))
                                       (and (search-backward quote nil t)
                                            (match-beginning 0))))
               (best (cond ((and after before)
                            (if (<= (- after there) (- there before)) after before))
                           (t (or after before)))))
          (and best (cons best (+ best (length quote)))))))))

(defun superchat-note--restore ()
  "Put back the notes kept on this buffer's file."
  (when buffer-file-name
    (superchat-note--read-kept)
    (let* ((file (expand-file-name buffer-file-name))
           (entry (assoc file superchat-note--kept)))
      (when entry
        (dolist (note (cdr entry))
          (let ((place (superchat-note--find (or (plist-get note :quote) "")
                                             (or (plist-get note :line) 1))))
            (if place
                (superchat-note--place note (car place) (cdr place))
              ;; Its text is not here any more: where it was, with no text.
              (let ((position (save-excursion
                                (save-restriction
                                  (widen)
                                  (goto-char (point-min))
                                  (forward-line (1- (or (plist-get note :line) 1)))
                                  (point)))))
                (superchat-note--place (copy-sequence note) position position)))))
        (setq superchat-note--kept (delq entry superchat-note--kept))))))

;;;; Placing and showing ----------------------------------------------------------

(defun superchat-note--place (note beg end)
  "Hang NOTE on BEG..END of this buffer; return the overlay."
  (let ((overlay (make-overlay beg end nil nil nil)))
    (overlay-put overlay 'superchat-note note)
    (overlay-put overlay 'face 'superchat-note-face)
    (overlay-put overlay 'help-echo (plist-get note :text))
    (when (= beg end)
      (overlay-put overlay 'before-string
                   (propertize "[note: its text is gone]" 'face 'warning)))
    (superchat-note--watch)
    overlay))

(defvar-local superchat-note--shown nil
  "The note overlay whose words the echo area shows now.")

(defun superchat-note--echo ()
  "With point on a note, the echo area says it."
  (let ((overlay (superchat-note--at)))
    (unless (eq overlay superchat-note--shown)
      (setq superchat-note--shown overlay)
      (when (and overlay (not (minibufferp)) (not (current-message)))
        (let ((note (overlay-get overlay 'superchat-note)))
          (message "Your note%s: %s"
                   (if (plist-get note :lost) " (its text is gone)" "")
                   (plist-get note :text)))))))

(defun superchat-note--watch ()
  "Notes in this buffer: the echo area says one point is on."
  (add-hook 'post-command-hook #'superchat-note--echo nil t))

(defun superchat-note--changed ()
  "The notes changed: keep them, and show again what is attached."
  (superchat-note--save)
  (when (fboundp 'superchat-butler-attached-refresh)
    (superchat-butler-attached-refresh superchat-note--attachment)))

;;;; Writing a note -------------------------------------------------------------------

(defconst superchat-note--edit-buffer "*superchat note*"
  "The buffer a note is written in.")

(defvar-local superchat-note--edit-target nil
  "What the note written here is for: an overlay, or (BUFFER BEG END).")

(defvar-local superchat-note--edit-windows nil
  "The window configuration to go back to.")

(defvar superchat-note-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-note-edit-finish)
    (define-key map (kbd "C-c C-k") #'superchat-note-edit-abort)
    map)
  "Keys of the buffer a note is written in.")

(define-derived-mode superchat-note-edit-mode text-mode "Note"
  "Write a note on the text you marked.
\\[superchat-note-edit-finish] keeps it; an empty note drops it.
\\[superchat-note-edit-abort] leaves it as it was."
  (setq header-line-format
        (substitute-command-keys
         "Your note.  \\[superchat-note-edit-finish] keeps it (empty drops it), \\[superchat-note-edit-abort] leaves it as it was")))

(defun superchat-note--edit (target words)
  "Write the note for TARGET, starting from WORDS."
  (let ((windows (current-window-configuration)))
    (pop-to-buffer (get-buffer-create superchat-note--edit-buffer)
                   '((display-buffer-reuse-window display-buffer-below-selected)
                     (window-height . 8)))
    (superchat-note-edit-mode)
    (erase-buffer)
    (when words (insert words))
    (setq superchat-note--edit-target target
          superchat-note--edit-windows windows)))

(defun superchat-note--edit-close ()
  "Close the note buffer and go back."
  (let ((windows superchat-note--edit-windows)
        (buffer (current-buffer)))
    (when windows (set-window-configuration windows))
    (kill-buffer buffer)))

(defun superchat-note-edit-finish ()
  "Keep this note; an empty one is dropped."
  (interactive)
  (let ((words (string-trim (buffer-string)))
        (target superchat-note--edit-target))
    (superchat-note--edit-close)
    (cond
     ((overlayp target)
      (when (overlay-buffer target)
        (if (string-empty-p words)
            (progn (delete-overlay target)
                   (message "superchat: note dropped"))
          (let ((note (copy-sequence (overlay-get target 'superchat-note))))
            (overlay-put target 'superchat-note (plist-put note :text words))
            (overlay-put target 'help-echo words)))
        (superchat-note--changed)))
     ((and (consp target) (buffer-live-p (car target)) (not (string-empty-p words)))
      (with-current-buffer (car target)
        (superchat-note--place (list :id (superchat-note--new-id)
                                     :text words
                                     :time (format-time-string "%FT%T%z"))
                               (nth 1 target) (nth 2 target))
        (deactivate-mark))
      (superchat-note--changed)
      (message "superchat: noted; %s puts your notes on your message"
               (substitute-command-keys "\\[superchat-note-attach]"))))))

(defun superchat-note-edit-abort ()
  "Leave the note as it was."
  (interactive)
  (superchat-note--edit-close))

;;;###autoload
(defun superchat-note ()
  "Write a note on the region, else the word or line at point.
On a note already, change its words; empty words drop it.  The text is not
changed.  `superchat-note-attach' puts your notes on the message you write."
  (interactive)
  (let ((overlay (and (not (use-region-p)) (superchat-note--at))))
    (if overlay
        (superchat-note--edit overlay (plist-get (overlay-get overlay 'superchat-note) :text))
      (let ((bounds (cond ((use-region-p) (cons (region-beginning) (region-end)))
                          ((bounds-of-thing-at-point 'symbol))
                          ((not (= (line-beginning-position) (line-end-position)))
                           (cons (line-beginning-position) (line-end-position))))))
        (unless bounds
          (user-error "Mark the text to write a note on"))
        (superchat-note--edit (list (current-buffer) (car bounds) (cdr bounds)) nil)))))

;;;###autoload
(defun superchat-note-delete ()
  "Drop the note at point; with a region, every note in it."
  (interactive)
  (let ((overlays (if (use-region-p)
                      (superchat-note--overlays nil (region-beginning) (region-end))
                    (let ((one (superchat-note--at))) (and one (list one))))))
    (unless overlays
      (user-error (if (use-region-p) "No note in the region" "No note here")))
    (mapc #'delete-overlay overlays)
    (when (use-region-p) (deactivate-mark))
    (superchat-note--changed)
    (message "superchat: %d note%s dropped" (length overlays)
             (if (cdr overlays) "s" ""))))

;;;###autoload
(defun superchat-note-clear ()
  "Drop every note in this buffer, after asking."
  (interactive)
  (let ((overlays (superchat-note--overlays)))
    (unless overlays
      (user-error "No notes in this buffer"))
    (when (yes-or-no-p (format "Drop the %d note%s in this buffer? "
                               (length overlays) (if (cdr overlays) "s" "")))
      (mapc #'delete-overlay overlays)
      (superchat-note--changed))))

(defun superchat-note--move (forward)
  "Go to the next note, or the previous one unless FORWARD."
  (let* ((here (point))
         (overlays (superchat-note--overlays))
         (target (if forward
                     (cl-find-if (lambda (o) (> (overlay-start o) here)) overlays)
                   (cl-find-if (lambda (o) (< (overlay-start o) here)) (reverse overlays)))))
    (unless target
      (user-error (if forward "No note after this" "No note before this")))
    (goto-char (overlay-start target))))

;;;###autoload
(defun superchat-note-next ()
  "Go to the next note in this buffer."
  (interactive)
  (superchat-note--move t))

;;;###autoload
(defun superchat-note-previous ()
  "Go to the previous note in this buffer."
  (interactive)
  (superchat-note--move nil))

;;;; All of them ----------------------------------------------------------------------

(defun superchat-note--all ()
  "Every note: (NOTE . OVERLAY-OR-NIL), by file or buffer, in order."
  (superchat-note--read-kept)
  (let (all)
    (dolist (buffer (buffer-list))
      (dolist (overlay (superchat-note--overlays buffer))
        (push (cons (superchat-note--refreshed overlay) overlay) all)))
    (dolist (entry superchat-note--kept)
      (dolist (note (cdr entry))
        (push (cons note nil) all)))
    (sort (nreverse all)
          (lambda (a b)
            (let ((fa (or (plist-get (car a) :file) "")) (fb (or (plist-get (car b) :file) "")))
              (if (equal fa fb)
                  (< (or (plist-get (car a) :line) 0) (or (plist-get (car b) :line) 0))
                (string< fa fb)))))))

(defun superchat-note--where (note overlay root)
  "Where NOTE is, for the model and for you: the file, under ROOT when it is."
  (let ((file (plist-get note :file)))
    (cond ((and file root (file-in-directory-p file root))
           (file-relative-name file root))
          (file (abbreviate-file-name file))
          (overlay (format "buffer %s" (buffer-name (overlay-buffer overlay))))
          (t "a buffer that is gone"))))

(defun superchat-note--lines (note)
  "\"line N\" or \"lines N-M\" for NOTE."
  (let ((line (or (plist-get note :line) 1))
        (lines (or (plist-get note :lines) 1)))
    (if (<= lines 1)
        (format "line %d" line)
      (format "lines %d-%d" line (+ line lines -1)))))

(defun superchat-note--quote (note)
  "NOTE's text, cut to `superchat-note-quoted-lines'."
  (let* ((lines (split-string (or (plist-get note :quote) "") "\n"))
         (kept (seq-take lines superchat-note-quoted-lines)))
    (concat (string-join kept "\n")
            (if (> (length lines) (length kept))
                (format "\n(and %d lines more)" (- (length lines) (length kept)))
              ""))))

(defun superchat-note--root (view)
  "The Workspace root of the conversation VIEW, or nil."
  (and view (buffer-live-p view)
       (let* ((session (buffer-local-value 'superchat--session-id view))
              (config (and session (fboundp 'superchat-view-session-configuration)
                           (superchat-view-session-configuration session))))
         (plist-get config :workspace-root))))

(defun superchat-note--words (&optional view)
  "Every note, as the words that go to the model from VIEW; \"\" with none.
The text a note is on is quoted (`superchat-butler--quoted'): what stands in it
triggers nothing.  Your words are yours."
  (let ((all (superchat-note--all))
        (root (superchat-note--root view))
        (number 0))
    (if (null all)
        ""
      (concat
       (format "My notes on the text (%d):" (length all))
       (mapconcat
        (lambda (pair)
          (cl-incf number)
          (let* ((note (car pair))
                 (overlay (cdr pair))
                 (unsaved (and overlay (buffer-modified-p (overlay-buffer overlay))
                               (plist-get note :file)))
                 (quote (superchat-butler--quotable (superchat-note--quote note))))
            (concat
             "\n\n"
             (format "%d. %s, %s%s%s:" number
                     (superchat-note--where note overlay root)
                     (superchat-note--lines note)
                     (if unsaved " (not saved; lines as in my buffer)" "")
                     (if (plist-get note :lost) " (that text is not there any more)" ""))
             (if (string-empty-p quote) ""
               (concat "\n" (superchat-butler--quoted quote nil)))
             "\nMy note: " (plist-get note :text))))
        all "")))))

(defun superchat-note--what ()
  "The one line that stands for the notes."
  (let* ((all (superchat-note--all))
         (places (delete-dups
                  (mapcar (lambda (pair)
                            (let ((file (plist-get (car pair) :file)))
                              (if file (file-name-nondirectory file)
                                (if (cdr pair) (buffer-name (overlay-buffer (cdr pair)))
                                  "?"))))
                          all))))
    (format "%d note%s of yours, on %s" (length all) (if (cdr all) "s" "")
            (string-join places ", "))))

(defun superchat-note--sent (ids)
  "The notes IDS went with a message: they are done."
  (dolist (buffer (buffer-list))
    (dolist (overlay (superchat-note--overlays buffer))
      (when (member (plist-get (overlay-get overlay 'superchat-note) :id) ids)
        (delete-overlay overlay))))
  (superchat-note--read-kept)
  (setq superchat-note--kept
        (cl-remove-if-not
         #'cdr
         (mapcar (lambda (entry)
                   (cons (car entry)
                         (cl-remove-if (lambda (note) (member (plist-get note :id) ids))
                                       (cdr entry))))
                 superchat-note--kept)))
  (superchat-note--save))

;;;###autoload
(defun superchat-note-attach ()
  "Put all your notes on the message you are writing in the conversation.
Nothing is sent.  That conversation is the one in sight, else the one
\\[superchat] opens from here.  What is attached follows your notes until
the message goes; then the notes that went are done."
  (interactive)
  (require 'superchat)
  (require 'superchat-butler)
  (unless (superchat-note--all)
    (user-error "You have no notes; %s writes one"
                (substitute-command-keys "\\[superchat-note]")))
  (let ((view (or (superchat-butler--conversation-in-use nil t)
                  (progn (call-interactively #'superchat)
                         (and (bound-and-true-p superchat-mode) (current-buffer))))))
    (unless view
      (user-error "No conversation to attach them to"))
    (superchat-butler--show-view view)
    (let (sending)
      (when (superchat-butler--attach
         view superchat-note--attachment
         #'superchat-note--what
         (lambda ()
           (setq sending (mapcar (lambda (pair) (plist-get (car pair) :id))
                                 (superchat-note--all)))
           (superchat-note--words view))
         (lambda () (superchat-note--sent sending)))
        (with-current-buffer view (goto-char (point-max)))
        (message "superchat: your notes are attached, sent with your message; %s shows them"
                 (substitute-command-keys "\\[superchat-butler-attached-toggle]"))))))

;;;; The list -----------------------------------------------------------------------

(defvar superchat-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'superchat-notes-visit)
    (define-key map (kbd "e") #'superchat-notes-edit)
    (define-key map (kbd "d") #'superchat-notes-drop)
    (define-key map (kbd "a") #'superchat-note-attach)
    (define-key map (kbd "g") #'superchat-notes)
    map)
  "Keys of the list of your notes.")

(define-derived-mode superchat-notes-mode tabulated-list-mode "Superchat-Notes"
  "Your notes, in every buffer and file.
RET goes to the text, e changes the words, d drops one, a puts them all on
the message you are writing, g reads them again."
  (setq tabulated-list-format [("Where" 28 t) ("Text" 32 nil) ("Note" 0 nil)])
  (tabulated-list-init-header))

(defun superchat-notes--one-line (text width)
  "TEXT on one line, at most WIDTH wide."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text ""))) width nil nil "…"))

;;;###autoload
(defun superchat-notes ()
  "List your notes, in every buffer and file."
  (interactive)
  (let ((buffer (get-buffer-create "*superchat notes*")))
    (with-current-buffer buffer
      (superchat-notes-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (pair)
                      (let ((note (car pair)))
                        (list pair
                              (vector (format "%s:%d"
                                              (superchat-note--where note (cdr pair) nil)
                                              (or (plist-get note :line) 1))
                                      (if (or (plist-get note :lost)
                                              (and (cdr pair)
                                                   (= (overlay-start (cdr pair))
                                                      (overlay-end (cdr pair)))))
                                          (propertize "(its text is gone)" 'face 'warning)
                                        (superchat-notes--one-line (plist-get note :quote) 30))
                                      (superchat-notes--one-line (plist-get note :text) 80)))))
                    (superchat-note--all)))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun superchat-notes--pair ()
  "The note on this line of the list."
  (or (tabulated-list-get-id) (user-error "No note on this line")))

(defun superchat-notes--go (pair)
  "Show the text of the note PAIR; return its overlay."
  (let* ((note (car pair))
         (overlay (or (and (cdr pair) (overlay-buffer (cdr pair)) (cdr pair))
                      (let ((file (plist-get note :file)))
                        (unless (and file (file-exists-p file))
                          (user-error "Its file is not there any more"))
                        (with-current-buffer (find-file-noselect file)
                          (cl-find (plist-get note :id) (superchat-note--overlays)
                                   :key (lambda (o) (plist-get (overlay-get o 'superchat-note) :id))
                                   :test #'equal))))))
    (unless overlay (user-error "That note is gone"))
    (pop-to-buffer (overlay-buffer overlay))
    (goto-char (overlay-start overlay))
    overlay))

(defun superchat-notes-visit ()
  "Go to the text of the note on this line."
  (interactive)
  (superchat-notes--go (superchat-notes--pair)))

(defun superchat-notes-edit ()
  "Change the words of the note on this line."
  (interactive)
  (let ((overlay (superchat-notes--go (superchat-notes--pair))))
    (superchat-note--edit overlay (plist-get (overlay-get overlay 'superchat-note) :text))))

(defun superchat-notes-drop ()
  "Drop the note on this line."
  (interactive)
  (let* ((pair (superchat-notes--pair))
         (id (plist-get (car pair) :id)))
    (when (yes-or-no-p (format "Drop the note \"%s\"? "
                               (superchat-notes--one-line (plist-get (car pair) :text) 40)))
      (superchat-note--sent (list id))
      (when (fboundp 'superchat-butler-attached-refresh)
        (superchat-butler-attached-refresh superchat-note--attachment))
      (superchat-notes))))

;;;; Keys -------------------------------------------------------------------------------

;;;###autoload
(defvar superchat-note-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'superchat-note)
    (define-key map (kbd "d") #'superchat-note-delete)
    (define-key map (kbd "D") #'superchat-note-clear)
    (define-key map (kbd "l") #'superchat-notes)
    (define-key map (kbd "a") #'superchat-note-attach)
    (define-key map (kbd "]") #'superchat-note-next)
    (define-key map (kbd "[") #'superchat-note-previous)
    map)
  "Your notes, under one prefix you bind, for example
\(keymap-global-set \"C-c n\" superchat-note-map).")

;;;###autoload
(fset 'superchat-note-map superchat-note-map)

(add-hook 'find-file-hook #'superchat-note--restore)
(add-hook 'kill-buffer-hook #'superchat-note--keep-buffer)
(add-hook 'kill-emacs-hook (lambda () (when (superchat-note--live-files) (superchat-note--save))))

(provide 'superchat-note)
;;; superchat-note.el ends here
