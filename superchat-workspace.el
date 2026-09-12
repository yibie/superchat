;;; superchat-workspace.el --- Region-based shared workspace for multi-agent state -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A workspace is a logical registry of buffer-region artifacts.  The
;; main agent and its sub-agents read and write those regions through
;; workspace_read / workspace_write tools (registered in
;; `superchat-tools.el').
;;
;; Markers auto-track buffer edits so live artifacts stay in sync.  If no
;; region is set, `superchat-workspace-write' auto-creates a fallback buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'superchat-closure nil t)
(require 'superchat-projection nil t)
(require 'superchat-tape nil t)

(declare-function superchat-db-tape-select "superchat-db" (sql &optional params))
(declare-function superchat-projection-register
                  "superchat-projection"
                  (name &rest keys))
(declare-function superchat-projection-replay
                  "superchat-projection"
                  (name events))
(declare-function superchat-closure-judge "superchat-closure" (query trace))
(declare-function superchat-tape-append-event "superchat-tape" (event))
(declare-function superchat-tape-replay-events
                  "superchat-tape" (session-id &optional since-id limit))

(defvar superchat--session-id nil)
(defvar superchat-data-directory nil)
(defvar superchat--tool-execution-context nil)
(defvar superchat--workspace-binding nil)
(defvar superchat-buffer-name)

(define-error 'superchat-workspace-artifact-revision-error
  "Workspace Artifact revision is missing or stale")

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
;; State
;; ═══════════════════════════════════════════════════════════

(defconst superchat-workspace--legacy-id "legacy"
  "Workspace ID used by direct legacy Lisp calls.")

(defvar superchat-workspace--registry (make-hash-table :test 'equal)
  "Hash table keyed by Workspace ID.
Each value is a plist with `:artifacts' and `:active-artifact-id'.")

(defvar superchat-workspace--artifact-counter 0
  "Counter used for process-local artifact IDs.")

(defun superchat-workspace--registry ()
  "Return the workspace registry, creating it if tests rebound it to nil."
  (unless (hash-table-p superchat-workspace--registry)
    (setq superchat-workspace--registry (make-hash-table :test 'equal)))
  superchat-workspace--registry)

(defun superchat-workspace--tool-context ()
  "Return the current Tool Executor context plist, or nil."
  (when (and (boundp 'superchat--tool-execution-context)
             (listp superchat--tool-execution-context))
    superchat--tool-execution-context))

(defun superchat-workspace--bound-workspace-id ()
  "Return the current buffer's Session Workspace ID, or nil."
  (when (and (boundp 'superchat--workspace-binding)
             (listp superchat--workspace-binding))
    (plist-get superchat--workspace-binding :id)))

(defun superchat-workspace--session-id (&optional workspace)
  "Return the best available Session ID for audit events.
WORKSPACE is the current workspace plist."
  (or (plist-get (superchat-workspace--tool-context) :session-id)
      (and (boundp 'superchat--session-id) superchat--session-id)
      (plist-get workspace :session-id)))

;;;###autoload
(defun superchat-workspace-current-id ()
  "Return the Workspace ID for the current execution context.
Resolution order is Tool Executor context, current Session binding, any
active chat binding, then the legacy direct-call fallback."
  (or (plist-get (superchat-workspace--tool-context) :workspace-id)
      (superchat-workspace--bound-workspace-id)
      (when-let* ((buffer (and (boundp 'superchat-buffer-name)
                               (get-buffer superchat-buffer-name))))
        (with-current-buffer buffer
          (superchat-workspace--bound-workspace-id)))
      superchat-workspace--legacy-id))

(defun superchat-workspace--workspace (workspace-id &optional create)
  "Return workspace WORKSPACE-ID, creating it when CREATE is non-nil."
  (let* ((id (or workspace-id (superchat-workspace-current-id)))
         (registry (superchat-workspace--registry))
         (workspace (gethash id registry)))
    (when (and create (not workspace))
      (setq workspace (list :id id :artifacts nil :active-artifact-id nil))
      (puthash id workspace registry))
    workspace))

(defun superchat-workspace--put-workspace (workspace)
  "Persist WORKSPACE in the registry and return it."
  (puthash (plist-get workspace :id) workspace (superchat-workspace--registry))
  workspace)

;;;###autoload
(defun superchat-workspace-bind (workspace-id &optional root session-id)
  "Ensure logical WORKSPACE-ID exists and bind it in the current buffer.
Optional ROOT and SESSION-ID are stored as metadata for later audit events."
  (unless (and (stringp workspace-id) (not (string-empty-p workspace-id)))
    (error "Workspace ID must be a non-empty string"))
  (let ((workspace (superchat-workspace--workspace workspace-id t)))
    (unless (superchat-workspace--workspace-artifacts workspace)
      (when-let* ((projected (ignore-errors
                               (superchat-workspace--projected-artifacts
                                workspace-id))))
        (let ((artifacts
               (mapcar (lambda (artifact)
                         (cons (plist-get artifact :artifact-id) artifact))
                       projected)))
          (setq workspace (plist-put workspace :artifacts artifacts))
          (setq workspace (plist-put workspace :active-artifact-id
                                     (caar artifacts))))))
    (setq workspace (plist-put workspace :root root))
    (setq workspace (plist-put workspace :session-id session-id))
    (superchat-workspace--put-workspace workspace)
    (setq-local superchat--workspace-binding
                (append (list :id workspace-id) (when root (list :root root))))
    workspace))

(defun superchat-workspace--artifact-id (workspace)
  "Return a new process-local artifact ID unique across workspaces."
  (let (artifact-id)
    (while
        (progn
          (setq artifact-id
                (format "artifact-%s-%d"
                        (substring
                         (secure-hash 'sha256 (plist-get workspace :id))
                         0 8)
                        (cl-incf superchat-workspace--artifact-counter)))
          (assoc artifact-id
                 (superchat-workspace--workspace-artifacts workspace))))
    artifact-id))

(defun superchat-workspace--file-adapter-p (artifact)
  "Return non-nil when ARTIFACT is backed by a durable file."
  (memq (plist-get artifact :adapter) '(generated-file durable-file)))

(defun superchat-workspace--artifact-live-p (artifact)
  "Return non-nil when ARTIFACT content can be resolved now."
  (if (superchat-workspace--file-adapter-p artifact)
      (file-readable-p (plist-get artifact :path))
    (let ((start (plist-get artifact :start-marker))
          (end (plist-get artifact :end-marker)))
      (and (markerp start)
           (markerp end)
           (marker-buffer start)
           (marker-buffer end)
           (eq (marker-buffer start) (marker-buffer end))
           (<= (marker-position start) (marker-position end))))))

(defun superchat-workspace--artifact-buffer (artifact)
  "Return ARTIFACT's live buffer, or nil."
  (when (superchat-workspace--artifact-live-p artifact)
    (marker-buffer (plist-get artifact :start-marker))))

(defun superchat-workspace--content-hash (content)
  "Return SHA-256 hash for CONTENT."
  (secure-hash 'sha256 content))

(defun superchat-workspace--artifact-content (artifact)
  "Return ARTIFACT content or signal when it is unresolved."
  (unless (superchat-workspace--artifact-live-p artifact)
    (error "Workspace artifact is unresolved: %s"
           (plist-get artifact :artifact-id)))
  (if (superchat-workspace--file-adapter-p artifact)
      (with-temp-buffer
        (insert-file-contents (plist-get artifact :path))
        (buffer-string))
    (with-current-buffer (superchat-workspace--artifact-buffer artifact)
      (save-restriction
        (widen)
        (buffer-substring-no-properties
         (plist-get artifact :start-marker)
         (plist-get artifact :end-marker))))))

(defun superchat-workspace--artifact-metadata (artifact)
  "Return public metadata for ARTIFACT.
The return value documents `superchat-workspace-add-region': callers get this
  plist, whose `:artifact-id' can be passed back to targeted read/write calls."
  (let* ((live (superchat-workspace--artifact-live-p artifact))
         (file-p (superchat-workspace--file-adapter-p artifact))
         (start-marker (plist-get artifact :start-marker))
         (end-marker (plist-get artifact :end-marker))
         (buffer (and live (not file-p) (marker-buffer start-marker)))
         (start (if (and live (not file-p)) (marker-position start-marker)
                  (plist-get artifact :start)))
         (end (if (and live (not file-p)) (marker-position end-marker)
                (plist-get artifact :end)))
         (buffer-name (or (and buffer (buffer-name buffer))
                          (plist-get artifact :buffer-name)))
         (revision (if live
                       (superchat-workspace--content-hash
                        (superchat-workspace--artifact-content artifact))
                     (plist-get artifact :revision))))
    (list :artifact-id (plist-get artifact :artifact-id)
          :uri (plist-get artifact :uri)
          :type (plist-get artifact :type)
          :adapter (plist-get artifact :adapter)
          :role (plist-get artifact :role)
          :revision revision
          :content-hash revision
          :buffer-name buffer-name
          :path (plist-get artifact :path)
          :start start
          :end end
          :live live
          :session-id (plist-get artifact :session-id)
          :run-id (plist-get artifact :run-id)
          :actor-id (plist-get artifact :actor-id))))

(defun superchat-workspace--artifact-with-provenance (artifact workspace)
  "Return ARTIFACT attributed to the current Runtime context."
  (let ((context (superchat-workspace--tool-context))
        (artifact (copy-sequence artifact)))
    (setq artifact
          (plist-put artifact :session-id
                     (superchat-workspace--session-id workspace)))
    (dolist (key '(:run-id :actor-id))
      (setq artifact (plist-put artifact key (plist-get context key))))
    artifact))

(defun superchat-workspace--workspace-artifacts (workspace)
  "Return WORKSPACE artifacts alist."
  (plist-get workspace :artifacts))

(defun superchat-workspace--set-workspace-artifacts (workspace artifacts)
  "Set WORKSPACE ARTIFACTS and persist the workspace."
  (setq workspace (plist-put workspace :artifacts artifacts))
  (superchat-workspace--put-workspace workspace))

(defun superchat-workspace--find-artifact (workspace artifact-id)
  "Return ARTIFACT-ID in WORKSPACE, or nil."
  (cdr (assoc artifact-id (superchat-workspace--workspace-artifacts workspace))))

(defun superchat-workspace--active-artifact (workspace)
  "Return WORKSPACE's active artifact, or nil."
  (superchat-workspace--find-artifact
   workspace (plist-get workspace :active-artifact-id)))

(defun superchat-workspace--target-artifact (workspace artifact-id)
  "Return selected artifact in WORKSPACE.
When ARTIFACT-ID is nil, return the active artifact."
  (if artifact-id
      (or (superchat-workspace--find-artifact workspace artifact-id)
          (error "Unknown workspace artifact: %s" artifact-id))
    (superchat-workspace--active-artifact workspace)))

(defun superchat-workspace--replace-artifact (workspace artifact)
  "Replace ARTIFACT in WORKSPACE and return updated workspace."
  (let ((artifact-id (plist-get artifact :artifact-id)))
    (superchat-workspace--set-workspace-artifacts
     workspace
     (cons (cons artifact-id artifact)
           (assoc-delete-all
            artifact-id (copy-sequence
                         (superchat-workspace--workspace-artifacts
                          workspace)))))))

(defun superchat-workspace--remove-artifact (workspace artifact-id)
  "Remove ARTIFACT-ID from WORKSPACE and return updated workspace."
  (let* ((artifacts (cl-remove artifact-id
                               (superchat-workspace--workspace-artifacts workspace)
                               :key #'car :test #'equal))
         (active (plist-get workspace :active-artifact-id)))
    (setq workspace (plist-put workspace :artifacts artifacts))
    (when (equal active artifact-id)
      (setq workspace (plist-put workspace :active-artifact-id
                                 (caar artifacts))))
    (superchat-workspace--put-workspace workspace)))

(defun superchat-workspace--event-payload (artifact &optional old-revision)
  "Return bounded event payload for ARTIFACT.
OLD-REVISION is included for change/remove facts."
  (let ((metadata (superchat-workspace--artifact-metadata artifact)))
    (append
     (list :artifact-id (plist-get metadata :artifact-id)
           :uri (plist-get metadata :uri)
           :type (plist-get metadata :type)
           :adapter (plist-get metadata :adapter)
           :role (plist-get metadata :role)
           :buffer-name (plist-get metadata :buffer-name)
           :path (plist-get metadata :path)
           :start (plist-get metadata :start)
           :end (plist-get metadata :end)
           :revision (plist-get metadata :revision)
           :content_hash (plist-get metadata :content-hash)
           :live (plist-get metadata :live))
     (when old-revision
       (list :old-revision old-revision)))))

(defun superchat-workspace--emit (workspace type artifact &optional old-revision)
  "Append a bounded workspace lifecycle event for ARTIFACT."
  (when-let* ((session-id (superchat-workspace--session-id workspace)))
    (let* ((context (superchat-workspace--tool-context))
           (event
            (list :type type
                  :session-id session-id
                  :workspace-id (plist-get workspace :id)
                  :topic "workspace"
                  :summary (format "%s: %s"
                                   type (plist-get artifact :uri))
                  :payload
                  (superchat-workspace--event-payload artifact old-revision))))
      (dolist (key '(:turn-id :run-id :actor-id :profile-id :parent-run-id
                     :correlation-id :causation-id :team-id))
        (when-let* ((value (plist-get context key)))
          (setq event (plist-put event key value))))
      (superchat-tape-append-event event))))

;; ═══════════════════════════════════════════════════════════
;; Fallback buffer
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace--fallback-buffer-name (workspace-id)
  "Return fallback buffer name for WORKSPACE-ID."
  (if (equal workspace-id superchat-workspace--legacy-id)
      superchat-workspace-fallback-buffer
    (format "%s<%s>" superchat-workspace-fallback-buffer workspace-id)))

(defun superchat-workspace--fallback-buffer (&optional workspace-id)
  "Return the fallback workspace buffer for WORKSPACE-ID, creating it if needed."
  (let ((buf (get-buffer (superchat-workspace--fallback-buffer-name
                          (or workspace-id
                              (superchat-workspace-current-id))))))
    (unless (buffer-live-p buf)
      (setq buf
            (get-buffer-create
             (superchat-workspace--fallback-buffer-name
              (or workspace-id (superchat-workspace-current-id))))))
    buf))

(defun superchat-workspace--fallback-artifact (workspace)
  "Create or return WORKSPACE's fallback buffer artifact."
  (let* ((workspace-id (plist-get workspace :id))
         (buf (superchat-workspace--fallback-buffer workspace-id))
         (artifact (superchat-workspace--active-artifact workspace)))
    (if (and artifact
             (equal (plist-get artifact :role) 'fallback)
             (superchat-workspace--artifact-live-p artifact))
        artifact
      (with-current-buffer buf
        (let ((start (set-marker (make-marker) (point-min) buf))
              (end (set-marker (make-marker) (point-max) buf)))
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (let* ((artifact-id (superchat-workspace--artifact-id workspace))
                 (content (buffer-substring-no-properties (point-min) (point-max)))
                 (revision (superchat-workspace--content-hash content))
                 (new-artifact
                  (superchat-workspace--artifact-with-provenance
                   (list :artifact-id artifact-id
                         :uri (format "buffer://%s" artifact-id)
                         :type 'buffer-region
                         :adapter 'buffer-region
                         :role 'fallback
                         :revision revision
                         :buffer-name (buffer-name buf)
                         :start (point-min)
                         :end (point-max)
                         :start-marker start
                         :end-marker end)
                   workspace)))
            (let ((old-workspace (copy-tree workspace)))
              (condition-case err
                  (progn
                    (setq workspace (superchat-workspace--replace-artifact
                                     workspace new-artifact))
                    (setq workspace (plist-put workspace :active-artifact-id
                                               artifact-id))
                    (superchat-workspace--put-workspace workspace)
                    (superchat-workspace--emit
                     workspace "workspace.artifact.added" new-artifact))
                (error
                 (superchat-workspace--put-workspace old-workspace)
                 (set-marker start nil)
                 (set-marker end nil)
                 (signal (car err) (cdr err)))))
            new-artifact))))))

;; ═══════════════════════════════════════════════════════════
;; Public API — set / add / clear / show
;; ═══════════════════════════════════════════════════════════

;;;###autoload
(defun superchat-workspace-add-region (beg end &optional role workspace-id)
  "Add BEG..END as a buffer-region artifact and return its metadata plist.
The returned `:artifact-id' is the stable target for
`superchat-workspace-read' and `superchat-workspace-write'."
  (interactive "r")
  (when (called-interactively-p 'any)
    (unless (use-region-p)
      (user-error "No active region; highlight text first")))
  (let* ((workspace (superchat-workspace--workspace workspace-id t))
         (old-workspace (copy-tree workspace))
         (artifact-id (superchat-workspace--artifact-id workspace))
         (start (set-marker (make-marker) beg (current-buffer)))
         (end-marker (set-marker (make-marker) end (current-buffer)))
         (content (save-restriction
                    (widen)
                    (buffer-substring-no-properties start end-marker)))
         (revision (superchat-workspace--content-hash content))
         (artifact
          (superchat-workspace--artifact-with-provenance
           (list :artifact-id artifact-id
                 :uri (format "buffer://%s" artifact-id)
                 :type 'buffer-region
                 :adapter 'buffer-region
                 :role (or role 'workspace)
                 :revision revision
                 :buffer-name (buffer-name)
                 :start beg
                 :end end
                 :start-marker start
                 :end-marker end-marker)
           workspace)))
    (set-marker-insertion-type start nil)
    (set-marker-insertion-type end-marker t)
    (condition-case err
        (progn
          (setq workspace (superchat-workspace--replace-artifact workspace artifact))
          (setq workspace (plist-put workspace :active-artifact-id artifact-id))
          (superchat-workspace--put-workspace workspace)
          (superchat-workspace--emit workspace "workspace.artifact.added" artifact)
          (let ((metadata (superchat-workspace--artifact-metadata artifact)))
            (when (called-interactively-p 'any)
              (message "Workspace artifact %s set in %s (%d..%d)"
                       artifact-id (buffer-name) beg end))
            metadata))
      (error
       (superchat-workspace--put-workspace old-workspace)
       (set-marker start nil)
       (set-marker end-marker nil)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun superchat-workspace-publish (content &optional role workspace-id)
  "Publish immutable CONTENT as a durable Artifact and return its metadata."
  (unless (and (stringp content) (not (string-empty-p content)))
    (error "Published Artifact content must be a non-empty string"))
  (let* ((workspace (superchat-workspace--workspace workspace-id t))
         (session-id (superchat-workspace--session-id workspace))
         (old-workspace (copy-tree workspace))
         (artifact-id (superchat-workspace--artifact-id workspace))
         (directory
          (expand-file-name
           (format "artifacts/%s/"
                   (substring
                    (secure-hash 'sha256 (plist-get workspace :id)) 0 12))
           (or superchat-data-directory
               (expand-file-name "superchat/" user-emacs-directory))))
         (path (expand-file-name (concat artifact-id ".txt") directory))
         (revision (superchat-workspace--content-hash content))
         (role (cond ((symbolp role) role)
                     ((and (stringp role)
                           (<= (length role) 64)
                           (string-match-p
                            "\\`[[:alnum:]_-]+\\'" role))
                      (intern (downcase role)))
                     ((null role) 'report)
                     (t (error "Invalid Artifact role: %S" role))))
         (artifact
          (superchat-workspace--artifact-with-provenance
           (list :artifact-id artifact-id
                 :uri (concat "file://" path)
                 :path path
                 :type 'generated-artifact
                 :adapter 'generated-file
                 :role role
                 :revision revision)
           workspace)))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Publishing an Artifact requires a Runtime Session"))
    (make-directory directory t)
    (condition-case err
        (progn
          (with-temp-file path (insert content))
          (setq workspace
                (superchat-workspace--replace-artifact workspace artifact))
          (setq workspace
                (plist-put workspace :active-artifact-id artifact-id))
          (superchat-workspace--put-workspace workspace)
          (superchat-workspace--emit
           workspace "workspace.artifact.added" artifact)
          (superchat-workspace--artifact-metadata artifact))
      (error
       (superchat-workspace--put-workspace old-workspace)
       (when (file-exists-p path)
         (delete-file path))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun superchat-workspace-set-region (beg end &optional role workspace-id)
  "Designate BEG..END as the active shared workspace region.
The region is tracked via markers that adapt to buffer edits."
  (interactive "r")
  (let* ((workspace (superchat-workspace--workspace workspace-id t))
         (active-id (plist-get workspace :active-artifact-id))
         (metadata (superchat-workspace-add-region
                    beg end role (plist-get workspace :id))))
    (when active-id
      (superchat-workspace-clear active-id (plist-get workspace :id)))
    (message "Workspace region set in %s (%d..%d)"
             (buffer-name) beg end)
    metadata))

;;;###autoload
(defun superchat-workspace-clear (&optional artifact-id workspace-id)
  "Clear ARTIFACT-ID from WORKSPACE-ID.
With no arguments, clear the active artifact in the current workspace."
  (interactive)
  (let* ((workspace (superchat-workspace--workspace workspace-id))
         (artifact (and workspace
                        (superchat-workspace--target-artifact
                         workspace artifact-id))))
    (when artifact
      (superchat-workspace--emit
       workspace "workspace.artifact.removed" artifact
       (plist-get (superchat-workspace--artifact-metadata artifact)
                  :revision))
      (when (markerp (plist-get artifact :start-marker))
        (set-marker (plist-get artifact :start-marker) nil))
      (when (markerp (plist-get artifact :end-marker))
        (set-marker (plist-get artifact :end-marker) nil))
      (setq workspace
            (superchat-workspace--remove-artifact
             workspace (plist-get artifact :artifact-id))))
    (message "Workspace region cleared")))

;;;###autoload
(defun superchat-workspace-show (&optional artifact-id workspace-id)
  "Display selected workspace artifact or fallback buffer."
  (interactive)
  (let* ((workspace (superchat-workspace--workspace workspace-id))
         (artifact (and workspace
                        (superchat-workspace--target-artifact
                         workspace artifact-id))))
    (cond
     ((and artifact
           (superchat-workspace--file-adapter-p artifact)
           (superchat-workspace--artifact-live-p artifact))
      (pop-to-buffer (find-file-noselect (plist-get artifact :path))))
     ((and artifact (superchat-workspace--artifact-live-p artifact))
      (let ((buf (superchat-workspace--artifact-buffer artifact)))
        (pop-to-buffer buf)
        (goto-char (plist-get artifact :start-marker))
        (unless (pos-visible-in-window-p (plist-get artifact :end-marker))
          (recenter 0))
        (pulse-momentary-highlight-region
         (plist-get artifact :start-marker)
         (plist-get artifact :end-marker))))
     (artifact
      (user-error "Workspace artifact is unresolved: %s"
                  (plist-get artifact :artifact-id)))
     (t
      (pop-to-buffer (superchat-workspace--fallback-buffer
                      (or workspace-id
                          (superchat-workspace-current-id))))))))

;; ═══════════════════════════════════════════════════════════
;; Reading
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace--active-p (&optional artifact-id workspace-id)
  "Return non-nil when the selected workspace artifact is live."
  (when-let* ((workspace (superchat-workspace--workspace workspace-id))
              (artifact (superchat-workspace--target-artifact
                         workspace artifact-id)))
    (superchat-workspace--artifact-live-p artifact)))

(defun superchat-workspace--require-artifact-readable
    (artifact workspace-id expected-revision)
  "Require readable content and a Closure-backed handoff for cross-Run ARTIFACT."
  (let* ((context (superchat-workspace--tool-context))
         (consumer-run-id (plist-get context :run-id))
         (producer-run-id (plist-get artifact :run-id)))
    (when (and (stringp consumer-run-id)
               (stringp producer-run-id)
               (not (equal consumer-run-id producer-run-id)))
      ;; ponytail: buffer-region markers are process-local; persist a
      ;; snapshot only if cross-process recovery becomes a product promise.
      (unless (superchat-workspace--artifact-live-p artifact)
        (error "Workspace artifact is not live in this Emacs process: %s"
               (plist-get artifact :artifact-id)))
      (superchat-workspace--check-revision
       artifact expected-revision
       'superchat-workspace-artifact-revision-error)
      (let* ((reference
              (list :workspace-id workspace-id
                    :artifact-id (plist-get artifact :artifact-id)
                    :revision expected-revision))
             (decision
              (superchat-workspace-artifact-delivery-decision
               (plist-get artifact :session-id)
               producer-run-id reference)))
        (unless (eq 'deliver (plist-get decision :action))
          (if (eq 'artifact-revision-stale (plist-get decision :reason))
              (signal 'superchat-workspace-artifact-revision-error
                      (list "Projected Artifact revision is stale"))
            (error "Artifact delivery %s: %s"
                   (plist-get decision :action)
                   (plist-get decision :reason))))
        ;; The live buffer may change while Closure and Projection are read.
        (superchat-workspace--check-revision
         artifact expected-revision
         'superchat-workspace-artifact-revision-error)))))

(defun superchat-workspace-read
    (&optional artifact-id workspace-id expected-revision)
  "Return selected workspace content as a string.
ARTIFACT-ID and WORKSPACE-ID target a specific artifact.  Returns nil when no
workspace or fallback buffer is active.  Cross-Run tool reads must pass the
projected EXPECTED-REVISION and satisfy the Artifact Delivery Contract."
  (let* ((workspace (superchat-workspace--workspace workspace-id))
         (artifact (and workspace
                        (superchat-workspace--target-artifact
                         workspace artifact-id))))
    (cond
     (artifact
      (superchat-workspace--require-artifact-readable
       artifact (plist-get workspace :id) expected-revision)
      (superchat-workspace--artifact-content artifact))
     (artifact-id
      (error "Unknown workspace artifact: %s" artifact-id))
     ((buffer-live-p
       (get-buffer (superchat-workspace--fallback-buffer-name
                    (or workspace-id (superchat-workspace-current-id)))))
      (with-current-buffer
          (get-buffer (superchat-workspace--fallback-buffer-name
                       (or workspace-id (superchat-workspace-current-id))))
        (buffer-substring-no-properties (point-min) (point-max))))
     (t nil))))

(defun superchat-workspace-info (&optional workspace-id)
  "Return a human-readable summary of WORKSPACE-ID."
  (let* ((workspace (superchat-workspace--workspace workspace-id))
         (artifact (and workspace (superchat-workspace--active-artifact workspace)))
         (artifacts (and workspace (superchat-workspace-artifacts
                                    (plist-get workspace :id)))))
    (cond
     (artifacts
      (let ((metadata (superchat-workspace--artifact-metadata artifact)))
        (concat
         (cond
          ((and artifact (plist-get metadata :live)
                (eq 'buffer-region (plist-get metadata :adapter)))
           (format "active region in %s (%d..%d, %d chars, artifact %s)"
                   (plist-get metadata :buffer-name)
                   (plist-get metadata :start)
                   (plist-get metadata :end)
                   (- (plist-get metadata :end) (plist-get metadata :start))
                   (plist-get metadata :artifact-id)))
          ((and artifact (plist-get metadata :live))
           (format "active Artifact %s uri=%s role=%s revision=%s"
                   (plist-get metadata :artifact-id)
                   (plist-get metadata :uri)
                   (plist-get metadata :role)
                   (plist-get metadata :revision)))
          (t
           (format "workspace %s has unresolved artifacts"
                   (plist-get workspace :id))))
         "\n"
         (mapconcat
          (lambda (item)
            (format "artifact %s uri=%s role=%s live=%s revision=%s buffer=%s"
                    (plist-get item :artifact-id)
                    (or (plist-get item :uri) "")
                    (or (plist-get item :role) "")
                    (if (plist-get item :live) "t" "nil")
                    (or (plist-get item :revision) "")
                    (or (plist-get item :buffer-name) "")))
          artifacts
          "\n"))))
     ((buffer-live-p (get-buffer
                      (superchat-workspace--fallback-buffer-name
                       (or workspace-id (superchat-workspace-current-id)))))
      (format "fallback buffer %s"
              (superchat-workspace--fallback-buffer-name
               (or workspace-id (superchat-workspace-current-id)))))
     (t "no workspace active"))))

;; ═══════════════════════════════════════════════════════════
;; Writing
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace--write-region (artifact content append)
  "Write CONTENT to ARTIFACT, appending when APPEND is non-nil."
  (let ((buf (superchat-workspace--artifact-buffer artifact)))
    (unless buf
      (error "Workspace artifact is unresolved: %s"
             (plist-get artifact :artifact-id)))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (if append
              (save-excursion
                (goto-char (plist-get artifact :end-marker))
                (unless (bolp) (insert "\n"))
                (insert (string-trim content))
                (unless (bolp) (insert "\n")))
            (save-excursion
              (delete-region (plist-get artifact :start-marker)
                             (plist-get artifact :end-marker))
              (goto-char (plist-get artifact :start-marker))
              (insert (string-trim content))
              (unless (bolp) (insert "\n")))))))))

(defun superchat-workspace--write-file-artifact (artifact content append)
  "Write CONTENT to file-backed ARTIFACT, appending when APPEND is non-nil."
  (let ((path (plist-get artifact :path)))
    (unless path
      (error "Durable Artifact is missing its file path: %s"
             (plist-get artifact :artifact-id)))
    (let* ((current (and (file-readable-p path)
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
           (next (if append
                     (concat (or current "") "\n" (string-trim content) "\n")
                   (concat (string-trim content) "\n"))))
      (with-temp-file path (insert next)))))

;;;###autoload
(defun superchat-workspace-publish-mutable (content &optional role workspace-id)
  "Publish mutable durable CONTENT and return its metadata.
Unlike `superchat-workspace-publish', the returned Artifact can later be
rewritten through `superchat-workspace-write' with revision tracking."
  (unless (and (stringp content) (not (string-empty-p content)))
    (error "Published Artifact content must be a non-empty string"))
  (let* ((workspace (superchat-workspace--workspace workspace-id t))
         (session-id (superchat-workspace--session-id workspace))
         (old-workspace (copy-tree workspace))
         (artifact-id (superchat-workspace--artifact-id workspace))
         (directory
          (expand-file-name
           (format "artifacts/%s/"
                   (substring
                    (secure-hash 'sha256 (plist-get workspace :id)) 0 12))
           (or superchat-data-directory
               (expand-file-name "superchat/" user-emacs-directory))))
         (path (expand-file-name (concat artifact-id ".txt") directory))
         (revision (superchat-workspace--content-hash content))
         (role (cond ((symbolp role) role)
                     ((and (stringp role)
                           (<= (length role) 64)
                           (string-match-p
                            "\\`[[:alnum:]_-]+\\'" role))
                      (intern (downcase role)))
                     ((null role) 'report)
                     (t (error "Invalid Artifact role: %S" role))))
         (artifact
          (superchat-workspace--artifact-with-provenance
           (list :artifact-id artifact-id
                 :uri (concat "file://" path)
                 :path path
                 :type 'generated-artifact
                 :adapter 'durable-file
                 :role role
                 :revision revision)
           workspace)))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Publishing an Artifact requires a Runtime Session"))
    (make-directory directory t)
    (condition-case err
        (progn
          (with-temp-file path (insert content))
          (setq workspace
                (superchat-workspace--replace-artifact workspace artifact))
          (setq workspace
                (plist-put workspace :active-artifact-id artifact-id))
          (superchat-workspace--put-workspace workspace)
          (superchat-workspace--emit
           workspace "workspace.artifact.added" artifact)
          (superchat-workspace--artifact-metadata artifact))
      (error
       (superchat-workspace--put-workspace old-workspace)
       (when (file-exists-p path)
         (delete-file path))
       (signal (car err) (cdr err))))))

(defun superchat-workspace--tool-write-p ()
  "Return non-nil when a write originates from Tool Executor context."
  (and (superchat-workspace--tool-context) t))

(defun superchat-workspace--check-revision
    (artifact expected-revision &optional condition)
  "Validate EXPECTED-REVISION against ARTIFACT.
Signal CONDITION instead of a generic error when it is non-nil."
  (let ((revision (plist-get (superchat-workspace--artifact-metadata artifact)
                             :revision)))
    (unless (and (stringp expected-revision)
                 (equal expected-revision revision))
      (let ((message
             (format "Stale workspace artifact revision: expected %s, current %s"
                     (or expected-revision "<missing>") revision)))
        (if condition
            (signal condition (list message))
          (error "%s" message))))))

(defun superchat-workspace-write
    (content &optional append artifact-id expected-revision workspace-id)
  "Write CONTENT into a workspace artifact.
When APPEND is non-nil, content is added at the end.  ARTIFACT-ID targets a
specific artifact.  Tool-context writes to existing artifacts must pass
EXPECTED-REVISION.  WORKSPACE-ID targets another logical workspace."
  (let* ((workspace (superchat-workspace--workspace workspace-id t))
         (workspace-id (plist-get workspace :id))
         (artifact (superchat-workspace--target-artifact workspace artifact-id))
         (existing-artifact-p artifact))
    (when (and artifact
               (eq 'generated-file (plist-get artifact :adapter)))
      (error "Published Artifacts are immutable; publish a new Artifact"))
    (unless artifact
      (unless (or superchat-workspace-auto-create
                  (buffer-live-p
                   (get-buffer
                    (superchat-workspace--fallback-buffer-name workspace-id))))
        (error "Workspace fallback buffer does not exist and auto-create is disabled"))
      (setq artifact (superchat-workspace--fallback-artifact workspace))
      (setq workspace (superchat-workspace--workspace workspace-id t)))
    (when (and existing-artifact-p (superchat-workspace--tool-write-p))
      (superchat-workspace--check-revision artifact expected-revision))
    (let* ((old-artifact (copy-tree artifact))
           (old-revision (plist-get
                          (superchat-workspace--artifact-metadata artifact)
                          :revision))
           (fallback-p (eq (plist-get artifact :role) 'fallback))
           (file-p (superchat-workspace--file-adapter-p artifact))
           result)
      (condition-case err
          (progn
            (if file-p
                (let ((path (plist-get artifact :path)))
                  ;; Re-check at the commit point; the first check protects
                  ;; the optimistic caller snapshot.
                  (when (and existing-artifact-p
                             (superchat-workspace--tool-write-p))
                    (superchat-workspace--check-revision artifact
                                                         expected-revision))
                  (superchat-workspace--write-file-artifact
                   artifact content append)
                  (let* ((metadata
                          (superchat-workspace--artifact-metadata artifact))
                         (revision (plist-get metadata :revision)))
                    (setq artifact (plist-put artifact :revision revision))
                    (setq artifact (plist-put artifact :content-hash revision))
                    (setq artifact
                          (superchat-workspace--artifact-with-provenance
                           artifact workspace))
                    (setq workspace (superchat-workspace--replace-artifact
                                     workspace artifact))
                    (superchat-workspace--emit
                     workspace "workspace.artifact.changed"
                     artifact old-revision))
                  (setq result
                        (format "%s durable Artifact %s"
                                (if append "Appended to" "Wrote")
                                (file-name-nondirectory path))))
              (with-current-buffer (superchat-workspace--artifact-buffer artifact)
                (atomic-change-group
                  ;; Re-check at the commit point.  The first check protects
                  ;; the optimistic caller snapshot; this one prevents a
                  ;; buffer edit between that check and the actual mutation.
                  (when (and existing-artifact-p
                             (superchat-workspace--tool-write-p))
                    (superchat-workspace--check-revision artifact
                                                         expected-revision))
                  (superchat-workspace--write-region artifact content append)
                  (let* ((metadata
                          (superchat-workspace--artifact-metadata artifact))
                         (revision (plist-get metadata :revision)))
                    (setq artifact (plist-put artifact :revision revision))
                    (setq artifact (plist-put artifact :start
                                              (plist-get metadata :start)))
                    (setq artifact (plist-put artifact :end
                                              (plist-get metadata :end)))
                    (setq artifact (plist-put artifact :buffer-name
                                              (plist-get metadata :buffer-name)))
                    (setq artifact
                          (superchat-workspace--artifact-with-provenance
                           artifact workspace))
                    (setq workspace (superchat-workspace--replace-artifact
                                     workspace artifact))
                    (superchat-workspace--emit
                     workspace "workspace.artifact.changed"
                     artifact old-revision))))
              (setq result
                    (if fallback-p
                        (format "%s fallback buffer %s"
                                (if append "Appended to" "Wrote")
                                (superchat-workspace--fallback-buffer-name
                                 workspace-id))
                      (format "%s workspace region in %s"
                              (if append "Appended to" "Replaced")
                              (buffer-name
                               (superchat-workspace--artifact-buffer
                                artifact)))))))
        (error
         (setq workspace (superchat-workspace--replace-artifact
                          workspace old-artifact))
         (signal (car err) (cdr err))))
      result)))

;; ═══════════════════════════════════════════════════════════
;; Projection
;; ═══════════════════════════════════════════════════════════

(defun superchat-workspace--plist-get-any (plist &rest keys)
  "Return the first non-nil value in PLIST for KEYS."
  (catch 'value
    (dolist (key keys)
      (let ((value (plist-get plist key)))
        (when value
          (throw 'value value))))))

(defun superchat-workspace--projected-artifact (payload event)
  "Return unresolved artifact metadata from event PAYLOAD."
  (let* ((artifact-id (superchat-workspace--plist-get-any
                       payload :artifact-id :artifact_id))
         (revision (superchat-workspace--plist-get-any
                    payload :revision :content-hash :content_hash))
         (type (superchat-workspace--plist-get-any payload :type))
         (adapter (superchat-workspace--plist-get-any payload :adapter))
         (role (superchat-workspace--plist-get-any payload :role)))
    (list :artifact-id artifact-id
          :uri (superchat-workspace--plist-get-any payload :uri)
          :type (if (stringp type) (intern type) type)
          :adapter (if (stringp adapter) (intern adapter) adapter)
          :role (if (stringp role) (intern role) role)
          :revision revision
          :content-hash revision
          :buffer-name (superchat-workspace--plist-get-any
                        payload :buffer-name :buffer_name)
          :path (superchat-workspace--plist-get-any payload :path)
          :start (superchat-workspace--plist-get-any payload :start)
          :end (superchat-workspace--plist-get-any payload :end)
          :live nil
          :workspace-id (plist-get event :workspace-id)
          :event-id (plist-get event :event-id)
          :session-id (plist-get event :session-id)
          :run-id (plist-get event :run-id)
          :actor-id (plist-get event :actor-id))))

(defun superchat-workspace--projection-event (row)
  "Return normalized workspace event from Tape ROW."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (meta (ignore-errors (json-read-from-string (or (nth 4 row) "{}"))))
         (payload (plist-get meta :payload)))
    (list :type (or (plist-get meta :event_type)
                    (plist-get meta :event-type))
          :event-id (or (plist-get meta :event_id)
                        (plist-get meta :event-id))
          :session-id (or (plist-get meta :session_id)
                          (plist-get meta :session-id))
          :run-id (or (plist-get meta :run_id)
                      (plist-get meta :run-id))
          :actor-id (or (plist-get meta :actor_id)
                        (plist-get meta :actor-id))
          :workspace-id (or (plist-get meta :workspace_id)
                            (plist-get meta :workspace-id))
          :payload payload)))

(defun superchat-workspace--projection-apply (state row)
  "Apply workspace Tape ROW to pure projection STATE."
  (let* ((event (superchat-workspace--projection-event row))
         (type (plist-get event :type))
         (workspace-id (plist-get event :workspace-id))
         (payload (plist-get event :payload))
         (artifact-id (superchat-workspace--plist-get-any
                       payload :artifact-id :artifact_id))
         (artifacts (cdr (assoc workspace-id state))))
    (pcase type
      ("workspace.artifact.added"
       (when artifact-id
         (setf (alist-get workspace-id state nil nil #'equal)
               (cons (cons artifact-id
                           (superchat-workspace--projected-artifact
                            payload event))
                     (cl-remove artifact-id artifacts
                                :key #'car :test #'equal)))))
      ("workspace.artifact.changed"
       (when-let* ((artifact (cdr (assoc artifact-id artifacts))))
         (let ((changed (superchat-workspace--projected-artifact
                         payload event)))
           (dolist (key '(:uri :type :adapter :role :revision :content-hash
                          :buffer-name :path :start :end))
             (when-let* ((value (plist-get changed key)))
               (setq artifact (plist-put artifact key value))))
           (dolist (key '(:event-id :session-id :run-id :actor-id))
             (setq artifact (plist-put artifact key
                                       (plist-get changed key))))
           (setcdr (assoc artifact-id artifacts) artifact))))
      ("workspace.artifact.removed"
       (setf (alist-get workspace-id state nil nil #'equal)
             (cl-remove artifact-id artifacts :key #'car :test #'equal))))
    state))

(when (fboundp 'superchat-projection-register)
  (superchat-projection-register
   'superchat-workspace-artifacts
   :version 2
   :initial nil
   :apply #'superchat-workspace--projection-apply))

(defun superchat-workspace--projected-artifacts (workspace-id)
  "Return projected artifacts for WORKSPACE-ID from Tape."
  (when (and (fboundp 'superchat-db-tape-select)
             (fboundp 'superchat-projection-replay))
    ;; ponytail: O(workspace events) replay keeps the schema unchanged; promote
    ;; envelope indexes only after measured workspace replay latency.
    (let* ((rows (superchat-db-tape-select
                  "SELECT id, topic, kind, content, meta, created_at
                   FROM tape
                   WHERE topic = 'workspace'
                   ORDER BY id ASC"))
           (state (superchat-projection-replay
                   'superchat-workspace-artifacts rows)))
      (mapcar #'cdr (cdr (assoc workspace-id state))))))

;;;###autoload
(defun superchat-workspace-artifacts (&optional workspace-id replay)
  "Return artifact metadata for WORKSPACE-ID.
When REPLAY is non-nil, rebuild unresolved metadata from Tape projection."
  (let ((id (or workspace-id (superchat-workspace-current-id))))
    (if replay
        (superchat-workspace--projected-artifacts id)
      (let ((workspace (superchat-workspace--workspace id)))
        (mapcar (lambda (entry)
                  (superchat-workspace--artifact-metadata (cdr entry)))
                (superchat-workspace--workspace-artifacts workspace))))))

(defun superchat-workspace-artifact-delivery-decision
    (session-id run-id reference &optional trace)
  "Return whether REFERENCE from RUN-ID is safe to consume.
REFERENCE must identify one Workspace Artifact and its expected revision.
The result's `:action' is `deliver', `wait', `manual-review', or `reject'."
  (let ((workspace-id (plist-get reference :workspace-id))
        (artifact-id (plist-get reference :artifact-id))
        (expected-revision (plist-get reference :revision))
        action reason artifact well-formed terminal)
    (if (not (cl-every
              (lambda (value)
                (and (stringp value) (not (string-empty-p value))))
              (list session-id run-id workspace-id artifact-id
                    expected-revision)))
        (setq action 'reject reason 'invalid-artifact-reference)
      (condition-case err
          (let* ((trace (or trace (superchat-tape-replay-events session-id)))
                 (well
                  (superchat-closure-judge
                   (list :kind 'trace-well-formed :run-id run-id) trace)))
            (setq well-formed well)
            (cond
             ((eq 'invalid (plist-get well :verdict))
              (setq action 'reject reason 'invalid-trace))
             ((not (eq 'valid (plist-get well :verdict)))
              (setq action 'manual-review reason 'unknown-trace))
             (t
              (setq terminal
                    (superchat-closure-judge
                     (list :kind 'durable-run-terminal :run-id run-id) trace))
              (pcase (plist-get terminal :verdict)
                ('open (setq action 'wait reason 'run-open))
                ('proved
                 (if (not (eq 'completed
                              (plist-get (plist-get terminal :judgment)
                                         :terminal-kind)))
                     (setq action 'reject reason 'run-not-successful)
                   (setq artifact
                         (cl-find artifact-id
                                  (superchat-workspace-artifacts workspace-id t)
                                  :key (lambda (item)
                                         (plist-get item :artifact-id))
                                  :test #'equal))
                   (let ((event-id (plist-get artifact :event-id))
                         (terminal-event-id
                          (car (last
                                (plist-get (plist-get terminal :witness)
                                           :event-ids)))))
                     (cond
                      ((not artifact)
                       (setq action 'reject reason 'artifact-not-present))
                      ((not (and (plist-get artifact :uri)
                                 (plist-get artifact :revision)
                                 event-id
                                 (plist-get artifact :session-id)
                                 (plist-get artifact :run-id)))
                       (setq action 'manual-review
                             reason 'artifact-provenance-unknown))
                      ((or (not (equal session-id
                                       (plist-get artifact :session-id)))
                           (not (equal run-id
                                       (plist-get artifact :run-id))))
                       (setq action 'reject reason 'artifact-producer-mismatch))
                      ((not (equal expected-revision
                                   (plist-get artifact :revision)))
                       (setq action 'reject reason 'artifact-revision-stale))
                      ((let ((artifact-position
                              (cl-position event-id trace
                                           :key (lambda (event)
                                                  (plist-get event :event-id))
                                           :test #'equal))
                             (terminal-position
                              (cl-position terminal-event-id trace
                                           :key (lambda (event)
                                                  (plist-get event :event-id))
                                           :test #'equal)))
                         (not (and artifact-position terminal-position
                                   (< artifact-position terminal-position))))
                       (setq action 'reject reason 'artifact-outside-run-proof))
                      (t (setq action 'deliver reason 'proved))))))
                (_ (setq action 'manual-review reason 'unknown-terminal))))))
        (error
         (setq action 'manual-review
               reason (list 'delivery-check-failed
                            (error-message-string err))))))
    (list :action action
          :reason reason
          :reference reference
          :artifact artifact
          :trace-judgment well-formed
          :terminal-judgment terminal)))

(provide 'superchat-workspace)
;;; superchat-workspace.el ends here
