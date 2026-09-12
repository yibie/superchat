;;; superchat-editor-workspace.el --- Session-scoped editor resources -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; This middle-tier module owns the live Emacs buffers created for one
;; Superchat Session.  Buffer membership is derived from exact buffer-local
;; markers on every query; the state registry deliberately stores no buffer
;; inventory.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'ibuf-ext)
(require 'project)
(require 'subr-x)
(require 'tab-bar)
(require 'superchat-runtime)

(defconst superchat-editor-workspace--tab-session-key 'superchat-session-id
  "Tab parameter containing the exact Session ID owned by a tab.")

(defvar superchat-editor-workspace--states (make-hash-table :test #'equal)
  "Live EditorWorkspace state keyed by stable Session ID.")

(defvar superchat-editor-workspace--allocation-stack nil
  "Session IDs whose buffer acquisition is synchronously in progress.")

(defvar superchat-editor-workspace--close-stack nil
  "Session IDs whose close transaction is synchronously executing.")

(defvar-local superchat--editor-workspace-session-id nil
  "Stable Session ID of this owned EditorWorkspace buffer.")

(defvar-local superchat--editor-workspace-owned-p nil
  "Non-nil when this buffer is owned by a Session EditorWorkspace.")

(defvar-local superchat--editor-workspace-role nil
  "Stable role of this owned EditorWorkspace buffer within its Session.")

;; A Session View chooses its major mode after allocation.  Ownership is an
;; editor-resource fact and must survive that ordinary mode initialization.
(put 'superchat--editor-workspace-session-id 'permanent-local t)
(put 'superchat--editor-workspace-owned-p 'permanent-local t)
(put 'superchat--editor-workspace-role 'permanent-local t)

(defun superchat-editor-workspace--check-session-id (session-id)
  "Return valid stable SESSION-ID, or signal an error."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "EditorWorkspace requires a non-empty Session ID"))
  session-id)

(defun superchat-editor-workspace--check-role (role)
  "Return valid buffer ROLE, or signal an error."
  (unless (or (and (symbolp role) role)
              (and (stringp role) (not (string-empty-p role))))
    (error "EditorWorkspace role must be a symbol or non-empty string"))
  role)

(defun superchat-editor-workspace-state (session-id)
  "Return the live EditorWorkspace state for SESSION-ID.

A Session with no materialized live record is `unmaterialized'."
  (superchat-editor-workspace--check-session-id session-id)
  (or (gethash session-id superchat-editor-workspace--states)
      'unmaterialized))

(defun superchat-editor-workspace--classify-buffer (buffer)
  "Classify exact live BUFFER's local EditorWorkspace metadata.

Return `(:state unowned)', `(:state malformed)', or an owned classification
carrying `:session-id' and, when valid and exact buffer-local, `:role'.
Inherited/default/dynamically bound values are deliberately invisible here."
  (unless (buffer-live-p buffer)
    (error "EditorWorkspace classification requires an exact live buffer"))
  (with-current-buffer buffer
    (let ((owned-local
           (local-variable-p 'superchat--editor-workspace-owned-p buffer))
          (session-local
           (local-variable-p
            'superchat--editor-workspace-session-id buffer)))
      (cond
       ((and (not owned-local) (not session-local))
        '(:state unowned))
       ((not (eq owned-local session-local))
        '(:state malformed))
       ((and (eq superchat--editor-workspace-owned-p t)
             (stringp superchat--editor-workspace-session-id)
             (not (string-empty-p
                   superchat--editor-workspace-session-id)))
        (append
         (list :state 'owned
               :session-id superchat--editor-workspace-session-id)
         (when (and
                (local-variable-p 'superchat--editor-workspace-role buffer)
                (or (and (symbolp superchat--editor-workspace-role)
                         superchat--editor-workspace-role)
                    (and (stringp superchat--editor-workspace-role)
                         (not (string-empty-p
                               superchat--editor-workspace-role)))))
           (list :role superchat--editor-workspace-role))))
       (t '(:state malformed))))))

(defun superchat-editor-workspace--owned-by-session-p (buffer session-id)
  "Return non-nil when BUFFER is classified as owned by SESSION-ID."
  (let ((classification
         (superchat-editor-workspace--classify-buffer buffer)))
    (and (eq 'owned (plist-get classification :state))
         (equal session-id (plist-get classification :session-id)))))

(defun superchat-editor-workspace-owned-buffers (session-id)
  "Return live buffers owned by SESSION-ID's EditorWorkspace.

Membership is recalculated from `buffer-list' and exact buffer-local markers;
no parallel buffer ledger is maintained."
  (superchat-editor-workspace--check-session-id session-id)
  (let (owned)
    (dolist (buffer (buffer-list) (nreverse owned))
      (when (superchat-editor-workspace--owned-by-session-p buffer session-id)
        (push buffer owned)))))

(defun superchat-editor-workspace-owner-session (buffer)
  "Return BUFFER's valid owning Session ID, or nil when it is unowned.

BUFFER must be an exact live buffer object.  Malformed ownership signals an
error rather than being mistaken for an ordinary borrowed buffer.  The
private marker representation does not escape this authority boundary."
  (let* ((classification
          (superchat-editor-workspace--classify-buffer buffer))
         (state (plist-get classification :state)))
    (pcase state
      ('owned (plist-get classification :session-id))
      ('unowned nil)
      ('malformed
       (error "EditorWorkspace buffer has malformed local ownership metadata")))))

(defun superchat-editor-workspace-validate-buffer (buffer session-id)
  "Validate BUFFER ownership for SESSION-ID and return the exact BUFFER.

An ordinary unowned live buffer is valid borrowed input.  When BUFFER is
owned, its owner returned by `superchat-editor-workspace-owner-session' must
exactly match SESSION-ID.  This function neither creates a buffer nor submits
a Runtime Request."
  (superchat-editor-workspace--check-session-id session-id)
  (let ((owner (superchat-editor-workspace-owner-session buffer)))
    (when (and owner (not (equal owner session-id)))
      (error "EditorWorkspace buffer belongs to Session %s, not %s"
             owner session-id)))
  buffer)

(defun superchat-editor-workspace--role-buffer (session-id role)
  "Return SESSION-ID's live owned buffer for ROLE, or nil."
  (catch 'found
    (dolist (buffer (superchat-editor-workspace-owned-buffers session-id))
      (when (equal role
                   (plist-get
                    (superchat-editor-workspace--classify-buffer buffer)
                    :role))
        (throw 'found buffer)))))

(defun superchat-editor-workspace--active-runs (session-id)
  "Return active public Runtime statuses for exact SESSION-ID."
  (cl-remove-if-not
   (lambda (status)
     (equal session-id (plist-get status :session-id)))
   (plist-get (superchat-runtime-status-projection) :active-runs)))

(defvar superchat--session-id)
(defvar superchat-mode)

(defun superchat-editor-workspace--validate-rollover
    (buffer name old-session new-session)
  "Validate the exact Session View BUFFER for OLD-SESSION rollover."
  (superchat-editor-workspace--check-session-id old-session)
  (superchat-editor-workspace--check-session-id new-session)
  (unless (and (bufferp buffer) (buffer-live-p buffer)
               (equal name (buffer-name buffer)))
    (error "Session View was killed or renamed during clear"))
  (let ((classification (superchat-editor-workspace--classify-buffer buffer)))
    (unless (and (eq (plist-get classification :state) 'owned)
                 (equal old-session (plist-get classification :session-id))
                 (eq (plist-get classification :role) 'session-view)
                 (local-variable-p 'superchat-mode buffer)
                 (buffer-local-value 'superchat-mode buffer)
                 (local-variable-p 'superchat--session-id buffer)
                 (equal old-session
                        (buffer-local-value 'superchat--session-id buffer)))
      (error "Clear requires the unchanged exact owned Session View")))
  (unless (eq (superchat-editor-workspace-state old-session) 'open)
    (error "Old Session workspace is not open"))
  (unless (and (not (equal old-session new-session))
               (eq (superchat-editor-workspace-state new-session) 'unmaterialized)
               (null (superchat-editor-workspace-owned-buffers new-session))
               (null (superchat-editor-workspace--active-runs new-session))
               ;; Freshness includes material/Anchor rows, not just decoded
               ;; Runtime events.  This is a read-only storage existence query.
               (null (superchat-db-tape-replay new-session nil 1)))
    (error "Clear target is not a fresh empty Session")))

(defun superchat-editor-workspace-rollover-session-view
    (buffer old-session new-session clear-view)
  "Roll exact Session View BUFFER from OLD-SESSION to NEW-SESSION.
CLEAR-VIEW receives a publication thunk; it resets the View and invokes that
thunk in its own View transaction, signalling on failure.  Cancellation happens
before that transaction and must never be undone by a View text rollback.  This
is exclusively a Session View operation, never a general resource transfer.
Return `rejected' without mutations when cancellation consent is declined."
  (let ((name (and (bufferp buffer) (buffer-name buffer))))
    (when (cl-intersection
           (list old-session new-session)
           (append superchat-editor-workspace--allocation-stack
                   superchat-editor-workspace--close-stack)
           :test #'equal)
      (user-error "Session clear is already in progress"))
    (superchat-editor-workspace--validate-rollover
     buffer name old-session new-session)
    (let ((active (superchat-editor-workspace--active-runs old-session)))
      (if (and active
               (not (yes-or-no-p
                     (format "Cancel %d active Run(s) and clear Session %s? "
                             (length active) old-session))))
          (progn
            (message "Clear rejected; manually cancel or use M-x superchat-close-session-workspace, then retry")
            'rejected)
        (let ((superchat-editor-workspace--allocation-stack
               (append (list old-session new-session)
                       superchat-editor-workspace--allocation-stack))
              (superchat-editor-workspace--close-stack
               (append (list old-session new-session)
                       superchat-editor-workspace--close-stack))
              published publication-complete old-tabs)
          (condition-case condition
              (progn
                (dolist (status active)
                  (superchat-runtime-cancel (plist-get status :run-id)
                                            "[Cancelled because the View is clearing]"))
                ;; Cancellation delivers synchronously and can run arbitrary
                ;; user code, including a second submit against the old host.
                (superchat-editor-workspace--validate-rollover
                 buffer name old-session new-session)
                (when (superchat-editor-workspace--active-runs old-session)
                  (error "Session acquired another active Run during cancellation"))
                (setq old-tabs
                      (cl-loop for frame in (frame-list)
                               for tabs = (frame-parameter frame 'tabs)
                               when (cl-some
                                     (lambda (tab)
                                       (equal old-session
                                              (superchat-editor-workspace--tab-session-id tab)))
                                     tabs)
                               collect (cons frame (copy-tree tabs))))
                (with-current-buffer buffer
                  (let ((inhibit-quit t))
                    (unwind-protect
			(progn
                          (funcall
                           clear-view
                           (lambda ()
                             (when publication-complete
                               (error "Session View publication was invoked twice"))
                             (superchat-editor-workspace--validate-rollover
                              buffer name old-session new-session)
                             (when (superchat-editor-workspace--active-runs old-session)
                               (error "Session acquired an active Run during View reset"))
                             (setq-local superchat--session-id new-session)
                             (setq-local superchat--editor-workspace-session-id new-session)
                             (unless (and (buffer-live-p buffer)
                                          (equal name (buffer-name buffer))
                                          (equal new-session
						 (buffer-local-value 'superchat--session-id buffer))
                                          (equal new-session
						 (superchat-editor-workspace-owner-session buffer)))
                               (error "Session View identity changed during publication"))
                             (puthash new-session 'open superchat-editor-workspace--states)
                             (unless (superchat-editor-workspace-owned-buffers old-session)
                               (puthash old-session 'closed superchat-editor-workspace--states)
                               ;; Retag only actual old Session associations.  If
                               ;; old protocol hosts remain, their tabs stay intact
                               ;; as public-close entry points.
                               (dolist (entry old-tabs)
				 (let ((tabs (tab-bar-tabs (car entry))))
                                   (dolist (tab tabs)
                                     (when (equal old-session
                                                  (superchat-editor-workspace--tab-session-id tab))
                                       (superchat-editor-workspace--set-tab-session-id tab new-session)))
                                   (tab-bar-tabs-set tabs (car entry)))))
                             (setq publication-complete t)))
                          (unless publication-complete
                            (error "Session View reset did not publish its Session"))
                          (setq published t))
                      (unless published
			;; Watchers may signal on either publication write.  The
			;; mandatory identity restoration must not run those same
			;; failing watchers again or be interrupted by quit.
			(let ((inhibit-quit t)
                              (variables '(superchat--session-id
                                           superchat--editor-workspace-session-id))
                              watchers)
                          (dolist (variable variables)
                            (let ((functions (get-variable-watchers variable)))
                              (push (cons variable functions) watchers)
                              (dolist (function functions)
				(remove-variable-watcher variable function))))
                          (unwind-protect
                              (progn
				(setq-local superchat--session-id old-session)
				(setq-local superchat--editor-workspace-session-id old-session)
				(puthash old-session 'open superchat-editor-workspace--states)
				(remhash new-session superchat-editor-workspace--states)
				(dolist (entry old-tabs)
                                  (when (frame-live-p (car entry))
                                    (set-frame-parameter (car entry) 'tabs (cdr entry)))))
                            (dolist (entry watchers)
                              (dolist (function (cdr entry))
				(add-variable-watcher (car entry) function)))))))))
                'cleared)
            ((error quit)
             (when active
               (message "Clear failed after cancellation was attempted; cancellation cannot be rolled back. Actual Run statuses: %S"
                        (mapcar (lambda (status)
                                  (let ((run-id (plist-get status :run-id)))
                                    (cons run-id
                                          (plist-get (superchat-runtime-status run-id) :status))))
                                active)))
             (signal (car condition) (cdr condition)))))))))

(defun superchat-editor-workspace-project (session-id root)
  "Return SESSION-ID's transient project for authoritative ROOT.

Nil ROOT means that the Session has no project projection.  This function does
not discover, remember, or manufacture a root."
  (superchat-editor-workspace--check-session-id session-id)
  (when root
    (unless (and (stringp root)
                 (not (string-empty-p root))
                 (file-directory-p root))
      (error "EditorWorkspace project root must be an existing directory"))
    (cons 'transient (file-name-as-directory (expand-file-name root)))))

(defun superchat-editor-workspace-project-buffers (session-id root)
  "Return buffers visible through SESSION-ID's transient project at ROOT.

The result may include ordinary borrowed buffers below ROOT.  It is a read-only
discovery projection and never grants EditorWorkspace close authority."
  (when-let* ((project
               (superchat-editor-workspace-project session-id root)))
    (project-buffers project)))

(defun superchat-editor-workspace-ibuffer-group (session-id)
  "Return one standard ibuffer filter group for exact SESSION-ID ownership.

The predicate is evaluated by ibuffer in each candidate buffer and reads the
live ownership markers; no buffer collection is captured or cached."
  (superchat-editor-workspace--check-session-id session-id)
  (list (format "Superchat Session %s" session-id)
        (list 'predicate
              'superchat-editor-workspace--owned-by-session-p
              '(current-buffer)
              session-id)))

(defun superchat-editor-workspace--tab-session-id (tab)
  "Return stable Session ID stored on TAB, or nil."
  (alist-get superchat-editor-workspace--tab-session-key (cdr tab)))

(defun superchat-editor-workspace--stored-session-tab-p (session-id)
  "Return non-nil when a frame already stores a tab for SESSION-ID.

This predicate deliberately reads frame parameters directly, so a headless
programmatic close with no Session tab never consults native tab machinery."
  (cl-some
   (lambda (frame)
     (cl-some
      (lambda (tab)
        (equal session-id (superchat-editor-workspace--tab-session-id tab)))
      (frame-parameter frame 'tabs)))
   (frame-list)))

(defun superchat-editor-workspace--current-tab ()
  "Return the current tab object."
  (cl-find-if (lambda (tab) (eq (car tab) 'current-tab)) (tab-bar-tabs)))

(defun superchat-editor-workspace--set-tab-session-id (tab session-id)
  "Set TAB's stable Session parameter to SESSION-ID."
  (setf (alist-get superchat-editor-workspace--tab-session-key (cdr tab))
        session-id)
  tab)

(defun superchat-editor-workspace--clear-tab-session-id (tab)
  "Remove the Session parameter from TAB."
  (setf (alist-get superchat-editor-workspace--tab-session-key
                   (cdr tab) nil 'remove)
        nil)
  tab)

(defun superchat-editor-workspace--neutralize-session-tabs (session-id frame)
  "Remove exact SESSION-ID parameters from every matching tab on FRAME."
  (let ((tabs (tab-bar-tabs frame)))
    (dolist (tab tabs)
      (when (equal session-id
                   (superchat-editor-workspace--tab-session-id tab))
        (superchat-editor-workspace--clear-tab-session-id tab)))
    (tab-bar-tabs-set tabs frame)))

(defun superchat-editor-workspace--select-session-tab (session-id)
  "Select or create the tab tagged with exact SESSION-ID."
  (superchat-editor-workspace--check-session-id session-id)
  (let (location)
    (catch 'found
      (dolist (frame (frame-list))
        (let ((position
               (cl-position-if
                (lambda (tab)
                  (equal session-id
                         (superchat-editor-workspace--tab-session-id tab)))
                (tab-bar-tabs frame))))
          (when position
            (setq location (cons frame (1+ position)))
            (throw 'found location)))))
    (if location
        (progn
          (select-frame (car location))
          (tab-bar-select-tab (cdr location)))
      (tab-bar-new-tab)
      (superchat-editor-workspace--set-tab-session-id
       (superchat-editor-workspace--current-tab) session-id)))
  (superchat-editor-workspace--current-tab))

(defun superchat-editor-workspace--close-session-tabs (session-id)
  "Close every tab tagged with exact SESSION-ID.

When the sole tab is tagged, remove its Session parameter and retain the tab as
a neutral Emacs tab.  Native prevent-close hooks are disabled for the internal
delete, while ordinary pre-close hooks still run exactly once."
  (let ((tab-bar-tab-prevent-close-functions nil))
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (let ((tabs (tab-bar-tabs frame)))
          (cl-loop for tab in (reverse tabs)
                   for reverse-index from 0
                   when (equal
                         session-id
                         (superchat-editor-workspace--tab-session-id tab))
                   do (if (= (length (tab-bar-tabs frame)) 1)
                          (superchat-editor-workspace--neutralize-session-tabs
                           session-id frame)
                        (let (deleted)
                          (unwind-protect
                              (progn
                                (tab-bar-close-tab
                                 (- (length tabs) reverse-index))
                                (setq deleted t))
                            (unless deleted
                              ;; Never leave a closed EditorWorkspace advertised
                              ;; by a stale Session tab after deletion aborts.
                              (superchat-editor-workspace--neutralize-session-tabs
                               session-id frame)))))))))))

(defun superchat-editor-workspace--create-buffer (session-id role directory)
  "Create and mark one buffer owned by SESSION-ID for ROLE in DIRECTORY."
  (let ((buffer
         (generate-new-buffer
          (format " *superchat-editor:%s:%s*" session-id role)))
        initialized)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory directory)
            ;; Publish the owned marker last so membership scans cannot observe
            ;; a partially initialized owned buffer from a variable watcher.
            (setq-local superchat--editor-workspace-session-id session-id
                        superchat--editor-workspace-role role
                        superchat--editor-workspace-owned-p t))
          (setq initialized t)
          buffer)
      (unless initialized
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun superchat-editor-workspace-buffer (session-id role &optional create)
  "Return SESSION-ID's owned buffer for ROLE.

When CREATE is non-nil, create and mark a missing buffer.  Creation lazily
opens an unmaterialized workspace and reopens a closed one.  Allocation is
refused while the workspace is `closing' or `retained'."
  (superchat-editor-workspace--check-session-id session-id)
  (superchat-editor-workspace--check-role role)
  (let ((state (superchat-editor-workspace-state session-id)))
    (when (and create
               (member session-id
                       superchat-editor-workspace--allocation-stack))
      (error "EditorWorkspace %s is already allocating a buffer" session-id))
    (when (and create (memq state '(closing retained)))
      (error "EditorWorkspace %s is %s; buffer allocation is frozen"
             session-id state))
    (or (superchat-editor-workspace--role-buffer session-id role)
        (when create
          (let ((directory default-directory))
            (unless (and (stringp directory) (file-directory-p directory))
              (error "EditorWorkspace requires a valid default directory"))
            ;; Publish `open' before ownership so a synchronous watcher can
            ;; only close an open workspace, never an unmaterialized one.
            (puthash session-id 'open superchat-editor-workspace--states)
            (let ((superchat-editor-workspace--allocation-stack
                   (cons session-id
                         superchat-editor-workspace--allocation-stack))
                  buffer
                  acquired)
              (unwind-protect
                  (progn
                    (setq buffer
                          (superchat-editor-workspace--create-buffer
                           session-id role directory))
                    (unless (and (buffer-live-p buffer)
                                 (eq 'open
                                     (superchat-editor-workspace-state
                                      session-id)))
                      (error "EditorWorkspace %s closed during allocation"
                             session-id))
                    (setq acquired t)
                    buffer)
                (unless acquired
                  (when (buffer-live-p buffer)
                    (condition-case nil
                        (kill-buffer buffer)
                      (error nil)))
                  (if (superchat-editor-workspace-owned-buffers session-id)
                      ;; Rollback uses ordinary kill semantics too.  A
                      ;; survivor is therefore retained, never hidden behind
                      ;; the state that preceded acquisition.
                      (puthash session-id 'retained
                               superchat-editor-workspace--states)
                    ;; Restore only if no reentrant close changed the state.
                    (when (eq 'open
                              (superchat-editor-workspace-state session-id))
                      (if (eq state 'unmaterialized)
                          (remhash session-id
                                   superchat-editor-workspace--states)
                        (puthash session-id state
                                 superchat-editor-workspace--states))))))))))))

(defun superchat-editor-workspace-acquire-buffer (session-id role)
  "Acquire SESSION-ID's ROLE buffer and report authority-known freshness.

Return `(:buffer BUFFER :newly-acquired BOOLEAN)'.  The result is an
ephemeral fact, not a lease or transferable ownership capability."
  (superchat-editor-workspace--check-session-id session-id)
  (superchat-editor-workspace--check-role role)
  ;; Emacs Lisp does not yield between this lookup and allocation.  The
  ;; existing allocation guard owns all synchronous reentry during creation.
  (if-let* ((buffer
             (superchat-editor-workspace--role-buffer session-id role)))
      (list :buffer buffer :newly-acquired nil)
    (list :buffer
          (superchat-editor-workspace-buffer session-id role t)
          :newly-acquired t)))

(defun superchat-editor-workspace-rollback-acquisition
    (session-id role buffer newly-acquired)
  "Roll back an exact fresh SESSION-ID ROLE BUFFER acquisition.

When NEWLY-ACQUIRED is nil this is an unconditional no-op.  A dead buffer,
repeated call, or ownership mismatch is harmless.  Ordinary kill queries and
hooks remain authoritative; a live survivor publishes `retained'."
  (when newly-acquired
    (superchat-editor-workspace--check-session-id session-id)
    (superchat-editor-workspace--check-role role)
    (when (buffer-live-p buffer)
      (let ((classification
             (superchat-editor-workspace--classify-buffer buffer)))
        (when (and (eq 'owned (plist-get classification :state))
                   (equal session-id
                          (plist-get classification :session-id))
                   (equal role (plist-get classification :role)))
          (condition-case nil
              (kill-buffer buffer)
            ((error quit) nil))
          (if (buffer-live-p buffer)
              (progn
                (puthash session-id 'retained
                         superchat-editor-workspace--states)
                'retained)
            'released))))))

(defun superchat-session-workspace-close (session-id)
  "Close SESSION-ID's live EditorWorkspace and return its resulting state.

Close uses ordinary `kill-buffer' behavior and never bypasses buffer queries.
If any exactly owned buffer survives, the workspace becomes `retained'.  A
later call retries by rescanning the live buffer list.  Closing an
unmaterialized or already closed workspace is an idempotent success."
  (superchat-editor-workspace--check-session-id session-id)
  (if (member session-id superchat-editor-workspace--close-stack)
      'closing
    (let ((state (superchat-editor-workspace-state session-id))
          (owned (superchat-editor-workspace-owned-buffers session-id))
          (active-runs (superchat-editor-workspace--active-runs session-id)))
      (cond
     ((and active-runs
           (not (yes-or-no-p
                 (format
                  "Cancel %d active Superchat Run%s and close Session %s? "
                  (length active-runs)
                  (if (= (length active-runs) 1) "" "s")
                  session-id))))
      (message
       "Session close rejected; use M-x superchat-close-session-workspace to retry")
      'rejected)
     ((and (null active-runs)
           (memq state '(unmaterialized closed))
           (null owned))
      (puthash session-id 'closed superchat-editor-workspace--states)
      (when (superchat-editor-workspace--stored-session-tab-p session-id)
        (superchat-editor-workspace--close-session-tabs session-id))
      'closed)
     (t
      (puthash session-id 'closing superchat-editor-workspace--states)
      (let ((superchat-editor-workspace--close-stack
             (cons session-id superchat-editor-workspace--close-stack))
            cancel-condition
            result)
        ;; Cancellation can synchronously run delivery callbacks.  The
        ;; allocation freeze above must therefore precede the first call.
        (dolist (status active-runs)
          (when-let* ((run-id (plist-get status :run-id)))
            (condition-case condition
                (superchat-runtime-cancel
                 run-id
                 "[Cancelled because the Session workspace is closing]")
              ((error quit)
               (unless cancel-condition
                 (setq cancel-condition condition))))))
        ;; A partial cancellation failure leaves the allocation freeze in place
        ;; and makes the next non-reentrant public close a genuine retry.
        (when cancel-condition
          (signal (car cancel-condition) (cdr cancel-condition)))
        (unwind-protect
            (dolist (buffer owned)
              (condition-case nil
                  (kill-buffer buffer)
                (error nil)))
          ;; Recompute membership even when a kill hook exits nonlocally, so
          ;; the workspace never remains stuck in `closing'.
          (setq result
                (if (superchat-editor-workspace-owned-buffers session-id)
                    'retained
                  'closed))
          (puthash session-id result superchat-editor-workspace--states))
        (if (eq result 'closed)
            (when (superchat-editor-workspace--stored-session-tab-p session-id)
              (superchat-editor-workspace--close-session-tabs session-id))
          (message
           "Session workspace retained live buffers; inspect and retry with M-x superchat-close-session-workspace"))
        result))))))

;;;###autoload
(defun superchat-close-session-workspace (&optional session-id)
  "Close SESSION-ID through the shared EditorWorkspace authority.

Interactively, use the stable Session ID stored on the current tab."
  (interactive)
  (let ((target
          (or session-id
              (superchat-editor-workspace--tab-session-id
               (superchat-editor-workspace--current-tab)))))
    (unless target
      (user-error "Current tab is not a Superchat Session tab"))
    (superchat-session-workspace-close target)))

(defun superchat-editor-workspace-prevent-tab-close (tab only-tab-p)
  "Apply Session close authority before native close of TAB.

ONLY-TAB-P is non-nil when Emacs cannot remove the final tab.  Return non-nil
to veto native closure.  Non-Session tabs are transparent."
  (ignore only-tab-p)
  (when-let* ((session-id
               (superchat-editor-workspace--tab-session-id tab)))
    ;; This hook exclusively consumes Session-tab close.  The authority performs
    ;; the internal native deletion, so later prevent hooks never get a chance
    ;; to split editor-resource closure from tab closure.
    (condition-case err
        (superchat-session-workspace-close session-id)
      (error
       (message "Session workspace close failed: %s"
                (error-message-string err))))
    t))

(add-hook 'tab-bar-tab-prevent-close-functions
          #'superchat-editor-workspace-prevent-tab-close t)

;; ── Plugin (spec §3): the default provider defined next to its code ──
(superchat-define-plugin editor-workspace
  :doc "Session-owned editor buffers."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'editor-workspace
     (superchat-service-table
      validate-buffer #'superchat-editor-workspace-validate-buffer
      buffer #'superchat-editor-workspace-buffer
      owner-session #'superchat-editor-workspace-owner-session
      acquire-buffer #'superchat-editor-workspace-acquire-buffer
      rollback-acquisition (lambda (&rest args)
                             (apply #'superchat-editor-workspace-rollback-acquisition args))
      close #'superchat-session-workspace-close))))

(provide 'superchat-editor-workspace)

;;; superchat-editor-workspace.el ends here
