;;; superchat-annotate.el --- Remarks hung on the text they are about -*- lexical-binding: t; -*-

;;; Commentary:
;; Annotating: take the region (else the whole buffer) → ask the model →
;; each of its remarks is hung on the text it is about, as an overlay.  The
;; buffer's text is never changed and nothing is saved.  No tool-calling.
;;
;; The model sees the text with line numbers and answers one remark per row,
;; "LINE | QUOTE | REMARK".  A remark is placed where its quote is found on
;; that line, else within `superchat-annotate--near-lines' lines of it; a
;; remark whose quote is found in neither place is not placed anywhere, and
;; is listed as such (docs/decisions/accepted/2026-10-10-remarks-hung-on-the-text.md).

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-sink)
(require 'superchat-inline)

;; ── Forward declarations ──
(defvar superchat-llm-backend)
(defvar superchat-llm-empty-response-text)
(declare-function superchat-conversation-show-turn "superchat"
                  (session-id run-id source))

(defgroup superchat-annotate nil
  "Remarks hung on the text they are about."
  :group 'superchat)

(defcustom superchat-annotate-default-instruction
  "Point out what is worth noticing: likely mistakes, unclear places, risks."
  "What `superchat-annotate' asks for when the user gives no instruction."
  :type 'string
  :group 'superchat-annotate)

(defcustom superchat-annotate-warn-lines 300
  "Above this many lines `superchat-annotate' says placement is untested.
Placement by line number was measured on texts of up to 87 lines."
  :type 'integer
  :group 'superchat-annotate)

(defconst superchat-annotate--near-lines 2
  "How many lines from the stated line a remark's quote may be found.")

(defconst superchat-annotate--list-buffer "*superchat annotations*"
  "Name of the buffer `superchat-annotate-list' shows.")

(defface superchat-annotate-quote
  '((t :underline (:style wave) :inherit warning))
  "Face of the text a remark is about."
  :group 'superchat-annotate)

(defface superchat-annotate-remark
  '((t :inherit shadow :slant italic))
  "Face of a remark shown at the end of its line."
  :group 'superchat-annotate)

(defvar superchat-annotate-instruction-history nil
  "Minibuffer history of `superchat-annotate' instructions.")

(defvar-local superchat-annotate--report nil
  "What the last annotation of this buffer gave, a plist.
:placed COUNT, :unplaced ROWS, :unparsed LINES, :empty BOOLEAN; a ROW is
\(:line N :quote TEXT :remark TEXT).")

;;;; What the model is asked ------------------------------------------------------

(defun superchat-annotate--system-text ()
  "Return the system text of an annotation request."
  "You annotate a text the user gives you.  The text is shown with a line \
number and a bar at the start of every line; those are not part of the text.

Answer with one remark per row and nothing else, each row in exactly this form:
LINE | QUOTE | REMARK

LINE is the number of the line the remark is about.  QUOTE is a short piece \
copied exactly from that line (at most 40 characters, without the line \
number), showing which part you mean.  REMARK is what you have to say about \
it, in one line.  No heading, no introduction, no closing sentence, no blank \
rows, no code fence.")

(defun superchat-annotate--numbered (text)
  "Return TEXT with a line number and a bar before each of its lines."
  (let ((n 0))
    (mapconcat (lambda (line) (format "%d| %s" (cl-incf n) line))
               (split-string text "\n")
               "\n")))

(defun superchat-annotate--content (text instruction)
  "Return the model input for annotating TEXT under INSTRUCTION."
  (concat "Instruction: " instruction "\n\n"
          "The text, with line numbers:\n<<<TEXT>>>\n"
          (superchat-annotate--numbered text)
          "\n<<<END TEXT>>>\n\n"
          "Reply with rows of the form LINE | QUOTE | REMARK only."))

;;;; Reading the answer -----------------------------------------------------------

(defun superchat-annotate--empty-answer-p (answer)
  "Return non-nil when ANSWER is the model saying nothing at all.
The transport hands an empty answer on as `superchat-llm-empty-response-text'."
  (let ((text (string-trim (or answer ""))))
    (or (string-empty-p text)
        (equal text (bound-and-true-p superchat-llm-empty-response-text)))))

(defun superchat-annotate--parse (answer)
  "Return (ROWS . UNPARSED) read from ANSWER.
ROWS are (:line N :quote TEXT :remark TEXT) in the answer's order; UNPARSED
are the non-blank lines of ANSWER that are not such a row."
  (let (rows unparsed)
    (dolist (line (split-string (or answer "") "\n"))
      (let ((row (string-trim line)))
        (cond
         ((string-empty-p row))
         ((string-match "\\`\\([0-9]+\\)[ \t]*|[ \t]*\\(.*?\\)[ \t]*|[ \t]*\\(.+\\)\\'" row)
          ;; All three are read before trimming, which matches again.
          (let ((line (match-string 1 row))
                (quote (match-string 2 row))
                (remark (match-string 3 row)))
            (push (list :line (string-to-number line)
                        :quote (string-trim quote "[\"“”`]+" "[\"“”`]+")
                        :remark (string-trim remark))
                  rows)))
         (t (push row unparsed)))))
    (cons (nreverse rows) (nreverse unparsed))))

(defun superchat-annotate--line-count (beg end)
  "Return how many lines BEG..END holds, as `superchat-annotate--numbered' counts.
BEG is at the start of a line; END is on the last one, wherever in it."
  (1+ (- (line-number-at-pos end) (line-number-at-pos beg))))

(defun superchat-annotate--quote-on-line (quote)
  "Return (BEG . END) of QUOTE on the current line, or nil."
  (save-excursion
    (let ((eol (line-end-position)))
      (beginning-of-line)
      (when (search-forward quote eol t)
        (cons (match-beginning 0) (match-end 0))))))

(defun superchat-annotate--without-ellipsis (quote)
  "Return QUOTE without the dots a model puts where it cut the quote short.
\"some text...\" and \"…some text\" give \"some text\"."
  (string-trim quote "\\(?:\\.\\.\\.+\\|…\\|[ \t]\\)+" "\\(?:\\.\\.\\.+\\|…\\|[ \t]\\)+"))

(defun superchat-annotate--locate-exactly (beg end line quote)
  "Return (QBEG . QEND) of QUOTE near LINE of BEG..END, or nil.
The stated line is tried first, then the lines one away, then two away, up
to `superchat-annotate--near-lines'; a line outside BEG..END is never tried."
  (unless (string-empty-p quote)
    (save-excursion
      (let ((last-line (superchat-annotate--line-count beg end))
            found)
        (dolist (offset (cons 0 (cl-loop for d from 1 to superchat-annotate--near-lines
                                         append (list d (- d)))))
          (let ((n (+ line offset)))
            ;; BEG..END are whole lines, so a line 1..LAST-LINE is inside them.
            (when (and (not found) (>= n 1) (<= n last-line))
              (goto-char beg)
              (forward-line (1- n))
              (setq found (superchat-annotate--quote-on-line quote)))))
        found))))

(defun superchat-annotate--locate (beg end line quote)
  "Return (QBEG . QEND) of QUOTE for a remark about LINE of BEG..END, or nil.
LINE counts from 1 at BEG.  QUOTE is looked for as written; when it is not
found and it starts or ends with the dots of a quote cut short, what is
left of it is looked for.  Either way the text found is in the buffer."
  (or (superchat-annotate--locate-exactly beg end line quote)
      (let ((cut (superchat-annotate--without-ellipsis quote)))
        (unless (equal cut quote)
          (superchat-annotate--locate-exactly beg end line cut)))))

;;;; Overlays ---------------------------------------------------------------------

(defvar superchat-annotate--overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'superchat-annotate-next)
    (define-key map (kbd "M-p") #'superchat-annotate-previous)
    map)
  "Keys active with point on an annotated piece of text.")

(defun superchat-annotate--make (qbeg qend row run-id)
  "Hang ROW's remark on QBEG..QEND; RUN-ID is the Run that gave it."
  (let* ((remark (plist-get row :remark))
         (head (make-overlay qbeg qend nil t nil))
         (eol (save-excursion (goto-char qend) (line-end-position)))
         (tail (make-overlay eol eol nil t t)))
    (overlay-put head 'superchat-annotate (append row (list :run-id run-id)))
    (overlay-put head 'superchat-annotate-tail tail)
    (overlay-put head 'face 'superchat-annotate-quote)
    (overlay-put head 'help-echo remark)
    (overlay-put head 'keymap superchat-annotate--overlay-keymap)
    (overlay-put head 'evaporate t)
    (overlay-put tail 'superchat-annotate-head head)
    (overlay-put tail 'after-string
                 (propertize (concat "  ⟵ " remark) 'face 'superchat-annotate-remark))
    head))

(defun superchat-annotate-overlays (&optional buffer)
  "Return BUFFER's annotations ordered by position (default: current buffer).
A remark whose text was deleted is dropped here, with its end-of-line text."
  (with-current-buffer (or buffer (current-buffer))
    (let (heads)
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (cond
         ((overlay-get overlay 'superchat-annotate) (push overlay heads))
         ((overlay-get overlay 'superchat-annotate-head)
          (unless (overlay-buffer (overlay-get overlay 'superchat-annotate-head))
            (delete-overlay overlay)))))
      (sort heads (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun superchat-annotate--delete (overlay)
  "Remove the annotation OVERLAY and its end-of-line text."
  (when (overlayp overlay)
    (let ((tail (overlay-get overlay 'superchat-annotate-tail)))
      (when (overlayp tail) (delete-overlay tail)))
    (delete-overlay overlay)))

;;;###autoload
(defun superchat-annotate-clear (&optional buffer)
  "Remove every annotation from BUFFER (default: the current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'superchat-annotate--delete (superchat-annotate-overlays))
    (remove-overlays (point-min) (point-max) 'superchat-annotate-pending t)
    (setq superchat-annotate--report nil)))

;;;###autoload
(defun superchat-annotate-next ()
  "Move point to the next annotation in the buffer, wrapping around."
  (interactive)
  (let* ((overlays (superchat-annotate-overlays))
         (next (or (cl-find-if (lambda (o) (> (overlay-start o) (point))) overlays)
                   (car overlays))))
    (unless next (user-error "No annotations in this buffer"))
    (goto-char (overlay-start next))
    (message "%s" (plist-get (overlay-get next 'superchat-annotate) :remark))
    next))

;;;###autoload
(defun superchat-annotate-previous ()
  "Move point to the previous annotation in the buffer, wrapping around."
  (interactive)
  (let* ((overlays (superchat-annotate-overlays))
         (previous (or (cl-find-if (lambda (o) (< (overlay-start o) (point)))
                                   (reverse overlays))
                       (car (last overlays)))))
    (unless previous (user-error "No annotations in this buffer"))
    (goto-char (overlay-start previous))
    (message "%s" (plist-get (overlay-get previous 'superchat-annotate) :remark))
    previous))

;;;; Placing an answer ------------------------------------------------------------

(defun superchat-annotate--place (buffer beg end answer run-id)
  "Hang the remarks of ANSWER on BEG..END of BUFFER and return the report.
BEG and END are markers.  RUN-ID is the Run that gave ANSWER.  The report is
the plist `superchat-annotate--report' holds; BUFFER's text is not changed."
  (with-current-buffer buffer
    (let* ((empty (superchat-annotate--empty-answer-p answer))
           (parsed (superchat-annotate--parse (if empty "" answer)))
           (placed 0)
           unplaced)
      (dolist (row (car parsed))
        (let ((where (superchat-annotate--locate
                      (marker-position beg) (marker-position end)
                      (plist-get row :line) (plist-get row :quote))))
          (if where
              (progn (superchat-annotate--make (car where) (cdr where) row run-id)
                     (cl-incf placed))
            (push row unplaced))))
      (setq superchat-annotate--report
            (list :placed placed
                  :unplaced (nreverse unplaced)
                  :unparsed (cdr parsed)
                  :empty empty)))))

(defun superchat-annotate--report-text (report)
  "Return one line saying what REPORT, an annotation's outcome, holds."
  (let ((placed (plist-get report :placed))
        (unplaced (length (plist-get report :unplaced)))
        (unparsed (length (plist-get report :unparsed))))
    (cond
     ((plist-get report :empty)
      "superchat: the model gave no remarks: its answer was empty")
     ((and (zerop placed) (zerop unplaced))
      (format "superchat: the model gave no remarks in the asked form (%d rows not understood; M-x superchat-annotate-list)"
              unparsed))
     (t
      (concat (format "superchat: %d remark%s placed" placed (if (= placed 1) "" "s"))
              (unless (zerop unplaced) (format ", %d not placed" unplaced))
              (unless (zerop unparsed) (format ", %d rows not understood" unparsed))
              (unless (and (zerop unplaced) (zerop unparsed))
                " (M-x superchat-annotate-list)"))))))

;;;; The request ------------------------------------------------------------------

(defun superchat-annotate--pending (beg)
  "Return an overlay at BEG saying an annotation is being asked for."
  (let ((overlay (make-overlay beg beg nil t nil)))
    (overlay-put overlay 'superchat-annotate-pending t)
    (overlay-put overlay 'before-string
                 (propertize "[superchat: annotating…]\n"
                             'face 'superchat-annotate-remark))
    overlay))

(defun superchat-annotate--sink (buffer beg end pending session-id)
  "Return the Sink that hangs an answer's remarks on BEG..END of BUFFER.
PENDING is the overlay shown while waiting; SESSION-ID is the conversation."
  (let ((done (lambda ()
                (when (overlayp pending) (delete-overlay pending))
                (set-marker beg nil) (set-marker end nil))))
    (superchat-sink-create
     :id (format "annotate-%08x" (random #x100000000))
     :live-p (lambda () (buffer-live-p buffer))
     :status #'ignore
     :stream #'ignore
     :final (lambda (run result)
              (let* ((run-id (if (stringp run) run (plist-get run :run-id)))
                     (report (superchat-annotate--place
                              buffer beg end (plist-get result :text) run-id))
                     (line (with-current-buffer buffer
                             (line-number-at-pos (marker-position beg)))))
                (funcall done)
                (message "%s" (superchat-annotate--report-text report))
                (when (and (stringp run-id)
                           (fboundp 'superchat-conversation-show-turn))
                  (superchat-conversation-show-turn
                   session-id run-id
                   (list :kind "annotation" :buffer buffer
                         :file (buffer-file-name buffer) :line line)))))
     :error (lambda (_run failure)
              (funcall done)
              (message "superchat: no annotations: %s"
                       (if (eq (plist-get failure :status) 'cancelled)
                           "cancelled"
                         (or (plist-get failure :text) "the request failed"))))
     :orphaned (lambda (_run _reason) (funcall done)))))

(defun superchat-annotate--bounds ()
  "Return (BEG . END), the whole lines `superchat-annotate' is about.
The lines the active region touches, else the whole buffer."
  (if (use-region-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (cons (save-excursion (goto-char beg) (line-beginning-position))
              (save-excursion
                (goto-char end)
                ;; A region ending at the start of a line does not touch it.
                (when (and (bolp) (> end beg)) (backward-char))
                (line-end-position))))
    (cons (point-min)
          (save-excursion
            (goto-char (point-max))
            (when (and (bolp) (> (point) (point-min))) (backward-char))
            (point)))))

(defun superchat-annotate--start (beg end instruction)
  "Ask for annotations of BEG..END under INSTRUCTION; return the Run handle."
  (superchat-plugin-require-active-row 'inline-rewrite)
  (unless superchat-llm-backend
    (user-error "superchat-llm-backend is not configured"))
  (when (= beg end)
    (user-error "Nothing to annotate"))
  (let* ((buffer (current-buffer))
         (text (buffer-substring-no-properties beg end))
         (lines (superchat-annotate--line-count beg end))
         (session-id (superchat-inline-session-id buffer))
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end t))
         (pending (superchat-annotate--pending beg))
         (envelope
          (list :surface 'inline-annotate
                :session-id session-id
                :content (superchat-annotate--content text instruction)
                :tools 'none
                :system-prompt (superchat-annotate--system-text)
                :sink (superchat-annotate--sink buffer beg-marker end-marker
                                                pending session-id)
                :context (list :annotate-buffer buffer))))
    (when (> lines superchat-annotate-warn-lines)
      (message "superchat: annotating %d lines; placement was measured on texts of up to 87"
               lines))
    (condition-case err
        (superchat-service-call nil 'driver 'submit envelope)
      ((error quit)
       (delete-overlay pending)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun superchat-annotate (instruction)
  "Ask the model for remarks on the region, else on the whole buffer.
Each remark is hung on the text it is about; the text itself is not
changed.  INSTRUCTION says what to look for.  Earlier annotations of the
same lines are replaced.  `superchat-annotate-next' and
`superchat-annotate-previous' go from one to the other,
`superchat-annotate-list' lists them with the ones that could not be
placed, `superchat-annotate-clear' removes them."
  (interactive
   (list (let ((typed (read-string
                       (format "Annotate (default: %s): "
                               superchat-annotate-default-instruction)
                       nil 'superchat-annotate-instruction-history)))
           (if (string-empty-p (string-trim typed))
               superchat-annotate-default-instruction
             typed))))
  (let ((bounds (superchat-annotate--bounds)))
    (dolist (overlay (superchat-annotate-overlays))
      (when (and (>= (overlay-start overlay) (car bounds))
                 (<= (overlay-end overlay) (cdr bounds)))
        (superchat-annotate--delete overlay)))
    (superchat-annotate--start (car bounds) (cdr bounds) instruction)))

;;;; The list ---------------------------------------------------------------------

(defun superchat-annotate--go (buffer position)
  "Show BUFFER with point at POSITION."
  (when (buffer-live-p buffer)
    (pop-to-buffer buffer)
    (goto-char position)))

;;;###autoload
(defun superchat-annotate-list (&optional buffer)
  "List BUFFER's annotations, and the remarks that could not be placed."
  (interactive)
  (let* ((source (or buffer (current-buffer)))
         (overlays (superchat-annotate-overlays source))
         (report (buffer-local-value 'superchat-annotate--report source))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create superchat-annotate--list-buffer)
      (erase-buffer)
      (special-mode)
      (insert (format "Annotations of %s\n\n" (buffer-name source)))
      (if (null overlays)
          (insert (if (plist-get report :empty)
                      "The model gave no remarks: its answer was empty.\n"
                    "No remarks are placed.\n"))
        (dolist (overlay overlays)
          (let ((position (overlay-start overlay)))
            (insert-text-button
             (format "%4d" (with-current-buffer source (line-number-at-pos position)))
             'action (lambda (_) (superchat-annotate--go source position))
             'follow-link t)
            (insert "  "
                    (plist-get (overlay-get overlay 'superchat-annotate) :remark)
                    "\n"))))
      (when-let* ((unplaced (plist-get report :unplaced)))
        (insert (format "\nNot placed (%d): the quoted text was not found near the stated line.\n"
                        (length unplaced)))
        (dolist (row unplaced)
          (insert (format "  line %d, \"%s\": %s\n" (plist-get row :line)
                          (plist-get row :quote) (plist-get row :remark)))))
      (when-let* ((unparsed (plist-get report :unparsed)))
        (insert (format "\nNot understood (%d): rows of the answer not in the asked form.\n"
                        (length unparsed)))
        (dolist (row unparsed) (insert "  " row "\n")))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'superchat-annotate)
;;; superchat-annotate.el ends here
