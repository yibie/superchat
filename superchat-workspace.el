;;; superchat-workspace.el --- Region-based shared workspace for multi-agent state -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A workspace is a user-highlighted region in any Emacs buffer.  The
;; main agent and its sub-agents read and write the region content.
;; Markers auto-track buffer edits so the workspace stays in sync.
;;
;; Agents use workspace_read / workspace_write tools (registered in
;; `superchat-tools.el') to interact with the workspace.
;;
;; If no region is set, `superchat-workspace-write' auto-creates a
;; fallback buffer (`*superchat-workspace*').

;;; Code:

;; ═══════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════

(defcustom superchat-workspace-fallback-buffer "*superchat-workspace*"
  "Buffer name used as fallback when no workspace region is set."
  :type 'string
  :group 'superchat)

(defcustom superchat-workspace-auto-create t
  "When non-nil, create the fallback buffer on first write if no region is set."
  :type 'boolean
  :group 'superchat)

;; ═══════════════════════════════════════════════════════════
;; State — marker pair for the active workspace region
;; ═══════════════════════════════════════════════════════════

(defvar superchat-workspace--start nil
  "Marker for the start of the workspace region, or nil.
Insertion-type nil so content inserted at the start stays inside
the region (the marker does not advance past it).")

(defvar superchat-workspace--end nil
  "Marker for the end of the workspace region, or nil.
Insertion-type t so content appended at the end stays inside the
region (the marker advances past it).")

(defun superchat-workspace--active-p ()
  "Return non-nil when both workspace markers are live."
  (and (markerp superchat-workspace--start)
       (markerp superchat-workspace--end)
       (marker-buffer superchat-workspace--start)
       (marker-buffer superchat-workspace--end)
       (eq (marker-buffer superchat-workspace--start)
           (marker-buffer superchat-workspace--end))
       (<= (marker-position superchat-workspace--start)
           (marker-position superchat-workspace--end))))

(defun superchat-workspace--buffer ()
  "Return the buffer containing the workspace region, or nil."
  (when (and (markerp superchat-workspace--start)
             (marker-buffer superchat-workspace--start))
    (marker-buffer superchat-workspace--start)))

;; ═══════════════════════════════════════════════════════════
;; Fallback buffer (when no region is set)
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace--fallback-buffer ()
  "Return the fallback workspace buffer, creating it if needed."
  (let ((buf (get-buffer superchat-workspace-fallback-buffer)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create superchat-workspace-fallback-buffer)))
    buf))

;; ═══════════════════════════════════════════════════════════
;; Public API — set / clear / show
;; ═══════════════════════════════════════════════════════════

;;;###autoload
(defun superchat-workspace-set-region (beg end)
  "Designate the region BEG..END as the shared workspace.
The region is tracked via markers that adapt to buffer edits."
  (interactive "r")
  (when (called-interactively-p 'any)
    (unless (use-region-p)
      (user-error "No active region; highlight text first")))
  ;; Clean up old markers
  (when (markerp superchat-workspace--start)
    (set-marker superchat-workspace--start nil))
  (when (markerp superchat-workspace--end)
    (set-marker superchat-workspace--end nil))
  ;; Create new markers in current buffer
  (setq superchat-workspace--start
        (set-marker (make-marker) beg (current-buffer)))
  (setq superchat-workspace--end
        (set-marker (make-marker) end (current-buffer)))
  ;; start marker stays at insertion point so prepended content stays inside the region
  (set-marker-insertion-type superchat-workspace--start nil)
  ;; end marker advances so appended content extends the workspace
  (set-marker-insertion-type superchat-workspace--end t)
  (message "Workspace region set in %s (%d..%d)"
           (buffer-name) beg end))

;;;###autoload
(defun superchat-workspace-clear ()
  "Clear the active workspace region."
  (interactive)
  (when (markerp superchat-workspace--start)
    (set-marker superchat-workspace--start nil))
  (when (markerp superchat-workspace--end)
    (set-marker superchat-workspace--end nil))
  (setq superchat-workspace--start nil
        superchat-workspace--end nil)
  (message "Workspace region cleared"))

;;;###autoload
(defun superchat-workspace-show ()
  "Display the active workspace region or fallback buffer."
  (interactive)
  (if (superchat-workspace--active-p)
      (let ((buf (superchat-workspace--buffer)))
        (pop-to-buffer buf)
        (goto-char superchat-workspace--start)
        (unless (pos-visible-in-window-p superchat-workspace--end)
          (recenter 0))
        (pulse-momentary-highlight-region
         superchat-workspace--start superchat-workspace--end))
    (pop-to-buffer (superchat-workspace--fallback-buffer))))

;; ═══════════════════════════════════════════════════════════
;; Reading
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace-read ()
  "Return the current workspace region content as a string.
Returns nil if no workspace is active."
  (if (superchat-workspace--active-p)
      (with-current-buffer (superchat-workspace--buffer)
        (buffer-substring-no-properties
         superchat-workspace--start superchat-workspace--end))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (with-current-buffer (get-buffer superchat-workspace-fallback-buffer)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun superchat-workspace-info ()
  "Return a human-readable summary of the current workspace."
  (cond
   ((superchat-workspace--active-p)
    (let ((buf (superchat-workspace--buffer)))
      (format "active region in %s (%d..%d, %d chars)"
              (buffer-name buf)
              (marker-position superchat-workspace--start)
              (marker-position superchat-workspace--end)
              (- (marker-position superchat-workspace--end)
                 (marker-position superchat-workspace--start)))))
   ((buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
    (format "fallback buffer %s" superchat-workspace-fallback-buffer))
   (t "no workspace active")))

;; ═══════════════════════════════════════════════════════════
;; Writing
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace-write (content &optional append)
  "Write CONTENT into the workspace region.
When APPEND is non-nil, content is added at the end of the region.
Otherwise, the region content is replaced.  If no region is active,
content is written to the fallback buffer."
  (cond
   ((superchat-workspace--active-p)
    (let ((buf (superchat-workspace--buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (if append
              (save-excursion
                (goto-char superchat-workspace--end)
                (unless (bolp) (insert "\n"))
                (insert (string-trim content))
                (unless (bolp) (insert "\n")))
            (save-excursion
              (delete-region superchat-workspace--start superchat-workspace--end)
              (goto-char superchat-workspace--start)
              (insert (string-trim content))
              (unless (bolp) (insert "\n"))))))
      (format "%s workspace region in %s"
              (if append "Appended to" "Replaced") (buffer-name buf))))
   (t
    (unless (or superchat-workspace-auto-create
                (buffer-live-p (get-buffer superchat-workspace-fallback-buffer)))
      (error "Workspace fallback buffer does not exist and auto-create is disabled"))
    (let ((buf (superchat-workspace--fallback-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (if append
              (progn
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert (string-trim content) "\n"))
            (erase-buffer)
            (insert (string-trim content) "\n"))))
      (format "%s fallback buffer %s"
              (if append "Appended to" "Wrote")
              superchat-workspace-fallback-buffer)))))

(provide 'superchat-workspace)
;;; superchat-workspace.el ends here
