;;; superchat-tape-browser.el --- The human face of the Tape -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; superchat-tape-view.el holds the queries; this module gives them a face.
;; Three levels, the first two as tabulated lists:
;;   M-x superchat-sessions       sessions (entries / last activity / active)
;;   M-x superchat-session-history  one session's physical entries
;;   RET on a session             its own physical entries
;;   RET on an entry              the full row, pretty-printed
;; `o' opens the session as a reconstructed chat View.  Branch lineage is
;; deliberately NOT merged here: the browser shows what each session owns,
;; so a branch anchor remains identifiable as such.

;;; Code:

(require 'cl-lib)
(require 'superchat-tape-view)
(require 'superchat-runtime)

(defvar superchat-tape-browser--session nil
  "Session whose timeline the current browser window shows.")

(defun superchat-tape-browser--active-count (session-id)
  "Number of active Runs owned by SESSION-ID."
  (let ((n 0))
    (dolist (run-id (superchat-runtime-active-runs))
      (let ((status (superchat-runtime-status run-id)))
        (when (and status
                   (equal session-id (plist-get status :session-id)))
          (setq n (1+ n)))))
    n))

(defun superchat-tape-browser--summarize (content)
  "Return a one-line CONTENT summary for list display."
  (let ((text (string-trim
               (replace-regexp-in-string
                "[\n\r]+" " "
                (format "%s" (or content ""))))))
    (if (> (length text) 70)
        (concat (substring text 0 67) "...")
      text)))

(defun superchat-tape-browser--sessions-rows ()
  "Return tabulated rows for every known Tape session."
  (mapcar
   (lambda (id)
     (let* ((entries (superchat-view-entries id))
            (last-row (car (last entries)))
            (last-at (and last-row (nth 5 last-row))))
       (list id
             (vector
              (list id 'face 'link)
              (number-to-string (length entries))
              (or last-at "")
              (number-to-string
               (superchat-tape-browser--active-count id))))))
   (superchat-view-session-ids)))

(defun superchat-tape-browser--entry-rows (session-id)
  "Return tabulated rows for SESSION-ID's own physical entries."
  (mapcar
   (lambda (row)
     (let ((id (nth 0 row))
           (kind (or (nth 2 row) ""))
           (content (nth 3 row))
           (created (or (nth 5 row) "")))
       (list id
             (vector
              (number-to-string id)
              (format "%s" kind)
              (superchat-tape-browser--summarize content)
              created))))
   (superchat-view-entries session-id)))

;;;###autoload
(defun superchat-sessions ()
  "List every known Tape session with entries, activity, and active Runs."
  (interactive)
  (let ((buffer (get-buffer-create "*superchat-sessions*")))
    (with-current-buffer buffer
      (setq superchat-tape-browser--session nil)
      (tabulated-list-mode)
      (setq tabulated-list-format
             [("Session" 36 nil)
             ("Entries" 8 nil :right-align t)
             ("Last activity" 24 t)
             ("Active" 7 nil :right-align t)])
      (setq tabulated-list-sort-key '("Last activity" . t))
      (setq tabulated-list-entries
            #'superchat-tape-browser--sessions-rows)
      (tabulated-list-init-header)
      (setq-local
       tabulated-list--current-sort-function nil)
      (tabulated-list-print t)
      (use-local-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map tabulated-list-mode-map)
         (define-key map (kbd "RET") #'superchat-tape-browser--open)
         (define-key map (kbd "o") #'superchat-tape-browser--open-chat)
         (define-key map (kbd "g") #'superchat-tape-browser--refresh)
         map)))
    (display-buffer buffer)
    buffer))

(defun superchat-tape-browser--session-at-point ()
  "Return the session id on the current row, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (if (buffer-local-value 'superchat-tape-browser--session
                            (current-buffer))
        (number-to-string id)
      (format "%s" id))))

(defun superchat-tape-browser--entry-at-point ()
  "Return the entry id on the current timeline row, or nil."
  (when (buffer-local-value 'superchat-tape-browser--session
                            (current-buffer))
    (let ((id (tabulated-list-get-id)))
      (and id (string-to-number (format "%s" id))))))

;;;###autoload
(defun superchat-session-history (session-id)
  "Display the physical Tape timeline for SESSION-ID."
  (interactive
   (list (completing-read "Session history: "
                          (superchat-view-session-ids) nil t)))
  (let ((buffer (get-buffer-create "*superchat-session-entries*")))
    (with-current-buffer buffer
      (setq superchat-tape-browser--session session-id)
      (tabulated-list-mode)
      (setq tabulated-list-format
            [("ID" 6 nil :right-align t)
             ("Kind" 28 nil)
             ("Summary" 70 nil)
             ("Created" 24 nil)])
      (setq tabulated-list-entries
            (lambda () (superchat-tape-browser--entry-rows session-id)))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (use-local-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map tabulated-list-mode-map)
         (define-key map (kbd "RET") #'superchat-tape-browser--open)
         (define-key map (kbd "g") #'superchat-tape-browser--refresh)
         map)))
    (display-buffer buffer)
    buffer))

(defun superchat-tape-browser--open ()
  "Drill into the row at point: session timeline or entry detail."
  (interactive)
  (if (buffer-local-value 'superchat-tape-browser--session
                          (current-buffer))
      (let* ((entry-id (superchat-tape-browser--entry-at-point))
             (session-id (buffer-local-value
                          'superchat-tape-browser--session
                          (current-buffer)))
             (row (cl-find-if
                   (lambda (r) (= entry-id (nth 0 r)))
                   (superchat-view-entries session-id))))
        (when row
          (let ((buffer (get-buffer-create "*superchat-entry*")))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (format "Entry %d in session %s\n\n"
                                entry-id session-id))
                (insert (format "kind: %S\ntopic: %S\ncreated: %S\n\n"
                                (nth 2 row) (nth 1 row) (nth 5 row)))
                (insert "content:\n")
                (insert (or (nth 3 row) ""))
                (insert "\n\nmeta:\n")
                (pp (nth 4 row) (current-buffer)))
              (special-mode))
            (display-buffer buffer))))
    (let ((session-id (superchat-tape-browser--session-at-point)))
      (when session-id
        (superchat-session-history session-id)))))

(defun superchat-tape-browser--open-chat ()
  "Open the session at point as a reconstructed chat View."
  (interactive)
  (let ((session-id (superchat-tape-browser--session-at-point)))
    (when session-id
      (if (fboundp 'superchat-open-session)
          (superchat-open-session session-id)
        (message "superchat-open-session is not available")))))

(defun superchat-tape-browser--refresh ()
  "Re-read the data behind the current browser window."
  (interactive)
  (tabulated-list-revert))

(provide 'superchat-tape-browser)

;;; superchat-tape-browser.el ends here
