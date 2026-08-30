;;; test-native-ui.el --- ERT tests for transient menu + dired/embark hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-dispatch, superchat-dired-send, and embark hooks.
;; Pure logic checks — no transient/dired/embark servers needed.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'superchat nil t)
(require 'superchat-editor-workspace)
(require 'superchat-tape-browser)
(require 'tab-bar)

(ert-deftest native-ui/clear-preserves-primary-and-old-host-entry ()
  "Rollover retags only its tabs and retains public close for old survivors."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (dolist (retain '(nil t))
    (let* ((directory (make-temp-file "superchat-clear-tabs-" t))
           (superchat-data-directory directory)
           (superchat-db--connection nil) (superchat-db--path nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (superchat-editor-workspace--states (make-hash-table :test #'equal))
           (superchat-manual-models t)
           (superchat-buffer-name (generate-new-buffer-name " *clear-primary*"))
           (old "clear-tab-old") view host new other)
      (unwind-protect
          (native-ui-test--with-isolated-tabs
            (superchat-db-open (expand-file-name "superchat.db" directory))
            (setq view (superchat--ensure-chat-view superchat-buffer-name old))
            (native-ui-test--tag-current-tab old)
            (when retain
              (setq host (superchat-editor-workspace-buffer old 'protocol-host t)))
            (tab-bar-new-tab)
            (native-ui-test--tag-current-tab "FOREIGN-TAB")
            (with-current-buffer view
              (superchat--clear-chat-and-context)
              (setq new superchat--session-id))
            (should (equal "FOREIGN-TAB"
                           (superchat-editor-workspace--tab-session-id
                            (superchat-editor-workspace--current-tab))))
            (if retain
                (progn
                  (should (eq 'open (superchat-editor-workspace-state old)))
                  (should (equal (list host) (superchat-editor-workspace-owned-buffers old)))
                  (should (= 1 (length (native-ui-test--session-tabs old)))))
              (should (eq 'closed (superchat-editor-workspace-state old)))
              (should-not (native-ui-test--session-tabs old))
              (should (= 1 (length (native-ui-test--session-tabs new)))))
            (superchat)
            (should (eq view (current-buffer)))
            (should (eq view (get-buffer superchat-buffer-name)))
            (should (equal new (superchat-editor-workspace-owner-session view)))
            (when retain
              (should (eq 'closed (superchat-close-session-workspace old)))
              (should-not (buffer-live-p host))
              (should (buffer-live-p view)))
            ;; Ordinary tabs that still have their own View keep M-x routing.
            (setq other (superchat--ensure-chat-view
                         (generate-new-buffer-name " *other-clear-view*") "other-live"))
            (native-ui-test--tag-current-tab "other-live")
            (superchat)
            (should (eq other (current-buffer))))
        (dolist (buffer (list view host other))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (superchat--cancel-status-refresh-timer)
              (setq-local kill-buffer-hook nil))
            (kill-buffer buffer)))
        (superchat-db-close)
        (delete-directory directory t)))))

(defmacro native-ui-test--with-isolated-tabs (&rest body)
  "Run BODY with one disposable tab and locally isolated tab hooks."
  (declare (indent 0) (debug t))
  `(let ((saved-tabs (copy-tree (frame-parameter nil 'tabs)))
         (tab-bar-tab-post-open-functions nil)
         (tab-bar-tab-pre-close-functions nil)
         (tab-bar-tab-prevent-close-functions nil)
         (tab-bar-close-last-tab-choice nil))
     (unwind-protect
         (save-window-excursion
           (set-frame-parameter nil 'tabs nil)
           (tab-bar-tabs)
           ,@body)
       (set-frame-parameter nil 'tabs saved-tabs))))

(defun native-ui-test--tab-session-id (tab)
  "Return TAB's exact Superchat Session parameter."
  (alist-get 'superchat-session-id tab))

(defun native-ui-test--tag-current-tab (session-id)
  "Attach SESSION-ID to the current disposable test tab and return it."
  (let ((tab (seq-find (lambda (candidate)
                         (eq (car candidate) 'current-tab))
                       (tab-bar-tabs))))
    (setf (alist-get 'superchat-session-id (cdr tab)) session-id)
    (tab-bar-tabs-set (tab-bar-tabs))
    tab))

(defun native-ui-test--session-tabs (session-id)
  "Return disposable test tabs tagged with exact SESSION-ID."
  (cl-remove-if-not
   (lambda (tab)
     (equal session-id (native-ui-test--tab-session-id tab)))
   (tab-bar-tabs)))

(defun native-ui-test--discard-workspace (session-id)
  "Remove test resources belonging to SESSION-ID without native tab actions."
  (dolist (buffer (superchat-editor-workspace-owned-buffers session-id))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local kill-buffer-query-functions nil)
        (setq-local kill-buffer-hook nil))
      (kill-buffer buffer)))
  (remhash session-id superchat-editor-workspace--states))

;; ═══════════════════════════════════════════════════════════
;; superchat-dispatch (wrapper exists always)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/dispatch-function-defined ()
  "superchat-dispatch is always defined."
  (should (fboundp 'superchat-dispatch)))

(ert-deftest native-ui/dispatch-fallback-function-defined ()
  "superchat--fallback-dispatch is always defined."
  (should (fboundp 'superchat--fallback-dispatch)))

(ert-deftest native-ui/dispatch-transient-stub-defined ()
  "superchat--dispatch-menu-transient stub is always defined."
  (should (fboundp 'superchat--dispatch-menu-transient)))

(ert-deftest native-ui/dispatch-transient-helpers-defined ()
  "All transient helper commands are defined."
  (should (fboundp 'superchat--transient-switch-model))
  (should (fboundp 'superchat--transient-set-reasoning))
  (should (fboundp 'superchat--transient-pick-skill))
  (should (fboundp 'superchat--transient-pick-command)))

(ert-deftest native-ui/reasoning-selector-updates-global-default ()
  "The reasoning selector updates the global default."
  (let ((superchat-llm-reasoning 'none)
        (superchat--active-preset nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Maximum")))
      (superchat--transient-set-reasoning))
    (should (eq superchat-llm-reasoning 'maximum))))

(ert-deftest native-ui/reasoning-selector-keeps-preset-priority-visible ()
  "An explicit preset reasoning value remains effective after selection."
  (let* ((superchat-llm-reasoning 'none)
         (preset (superchat-preset-from-plist
                  '(:name "coder" :reasoning medium)))
         (superchat--active-preset preset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Maximum")))
      (superchat--transient-set-reasoning))
    (should (eq superchat-llm-reasoning 'maximum))
    (should (eq (superchat-preset-reasoning preset) 'medium))))

;; ═══════════════════════════════════════════════════════════
;; superchat-mode-map has dispatch binding
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/keymap-has-dispatch-binding ()
  "superchat-mode-map binds C-c C-d to superchat-dispatch."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-d"))
              'superchat-dispatch)))

(ert-deftest native-ui/keymap-has-existing-bindings-intact ()
  "Existing keybindings are preserved."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-c"))
              'superchat-send-input))
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-s"))
              'superchat--save-conversation))
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-h"))
              'superchat--list-commands)))

(ert-deftest native-ui/session-tab-parameter-survives-roundtrip-and-rename ()
  "Session tab lookup uses its exact parameter, never its cosmetic name."
  (let ((session-id "native-ui-tab-roundtrip")
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (setq buffer
                (superchat-editor-workspace-buffer
                 session-id 'session-view t))
          (superchat--display-session-view buffer session-id)
          (should (= 1 (length (native-ui-test--session-tabs session-id))))
          (let* ((tagged (car (native-ui-test--session-tabs session-id)))
                 (index (seq-position (tab-bar-tabs) tagged #'eq)))
            (tab-bar-select-tab (1+ index))
            (tab-bar-rename-tab "cosmetic-name-with-no-session-id")
            (tab-bar-new-tab)
            (tab-bar-select-tab (1+ index))
            (should
             (equal session-id
                    (native-ui-test--tab-session-id
                     (seq-find (lambda (tab)
                                 (eq (car tab) 'current-tab))
                               (tab-bar-tabs)))))
            (superchat--display-session-view buffer session-id)
            (should (= 1
                       (length
                        (native-ui-test--session-tabs session-id))))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/native-tab-close-alone-does-not-kill-buffer ()
  "Built-in tab deletion carries no implicit buffer ownership authority."
  (let ((buffer (generate-new-buffer " *native-ui-tab-no-kill*")))
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab "native-ui-raw-tab")
          (tab-bar-new-tab)
          (tab-bar-close-tab 1)
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest native-ui/ensure-chat-view-keeps-render-on-exact-owned-buffer ()
  "A name collision cannot redirect synchronous View initialization."
  (let* ((session-id "native-ui-ensure-exact-view")
         (requested-name " *native-ui-ensure-collision*")
         (initial-name
          (format " *superchat-editor:%s:session-view*" session-id))
         (foreign (generate-new-buffer requested-name))
         owned final-name foreign-marker foreign-marker-position
         foreign-marker-insertion-type)
    (unwind-protect
        (progn
          (with-current-buffer foreign
            (fundamental-mode)
            (insert "FOREIGN-ENSURE")
            (setq foreign-marker (copy-marker 3 t)
                  foreign-marker-position (marker-position foreign-marker)
                  foreign-marker-insertion-type
                  (marker-insertion-type foreign-marker))
            (setq-local superchat--prompt-start foreign-marker)
            (set-buffer-modified-p t))
          (let ((foreign-content (with-current-buffer foreign (buffer-string)))
                (foreign-mode (buffer-local-value 'major-mode foreign))
                (foreign-modified (buffer-modified-p foreign))
                (foreign-owned-local
                 (local-variable-p 'superchat--editor-workspace-owned-p foreign))
                (foreign-session-local
                 (local-variable-p 'superchat--editor-workspace-session-id foreign))
                (foreign-superchat-mode
                 (and (local-variable-p 'superchat-mode foreign)
                      (buffer-local-value 'superchat-mode foreign))))
            (setq owned
                  (superchat--ensure-chat-view requested-name session-id)
                  final-name (buffer-name owned))
            (should (buffer-live-p owned))
            (should-not (eq owned foreign))
            (should-not (equal initial-name requested-name))
            (should-not (equal final-name requested-name))
            (should-not (equal final-name initial-name))
            (should-not (get-buffer initial-name))
            (should (eq owned
                        (superchat-editor-workspace-validate-buffer
                         owned session-id)))
            (with-current-buffer owned
              (should (derived-mode-p 'org-mode))
              (should superchat-mode)
              (should (markerp superchat--prompt-start))
              (should (eq owned (marker-buffer superchat--prompt-start)))
              (should (string-match-p "\\* User: " (buffer-string))))
            (should (equal foreign-content
                           (with-current-buffer foreign (buffer-string))))
            (should (eq foreign-mode (buffer-local-value 'major-mode foreign)))
            (should (eq foreign-modified (buffer-modified-p foreign)))
            (should (eq foreign-marker
                        (buffer-local-value 'superchat--prompt-start foreign)))
            (should (eq foreign
                        (marker-buffer foreign-marker)))
            (should (= foreign-marker-position
                       (marker-position foreign-marker)))
            (should (eq foreign-marker-insertion-type
                        (marker-insertion-type foreign-marker)))
            (should (eq foreign-owned-local
                        (local-variable-p
                         'superchat--editor-workspace-owned-p foreign)))
            (should (eq foreign-session-local
                        (local-variable-p
                         'superchat--editor-workspace-session-id foreign)))
            (should (eq foreign-superchat-mode
                        (and (local-variable-p 'superchat-mode foreign)
                             (buffer-local-value 'superchat-mode foreign))))))
      (when (buffer-live-p foreign) (kill-buffer foreign))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/render-session-view-keeps-rebuild-on-exact-owned-buffer ()
  "A name collision cannot redirect synchronous Session reconstruction."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-exact-rebuild-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (session-id "native-ui-render-exact-view")
         (requested-name (superchat--session-buffer-name session-id))
         (initial-name
          (format " *superchat-editor:%s:session-view*" session-id))
         (foreign (generate-new-buffer requested-name))
         owned final-name foreign-marker foreign-marker-position
         foreign-marker-insertion-type)
    (unwind-protect
        (progn
          (with-current-buffer foreign
            (text-mode)
            (insert "FOREIGN-REBUILD")
            (setq foreign-marker (copy-marker 4 t)
                  foreign-marker-position (marker-position foreign-marker)
                  foreign-marker-insertion-type
                  (marker-insertion-type foreign-marker))
            (setq-local superchat--prompt-start foreign-marker)
            (set-buffer-modified-p t))
          (let ((foreign-content (with-current-buffer foreign (buffer-string)))
                (foreign-mode (buffer-local-value 'major-mode foreign))
                (foreign-modified (buffer-modified-p foreign))
                (foreign-owned-local
                 (local-variable-p 'superchat--editor-workspace-owned-p foreign))
                (foreign-session-local
                 (local-variable-p 'superchat--editor-workspace-session-id foreign)))
            (superchat-db-open database)
            (superchat-tape-append-material session-id "user" "Persisted exact question")
            (superchat-tape-append-material session-id "assistant" "Persisted exact answer")
            (setq owned (superchat--render-session-view session-id)
                  final-name (buffer-name owned))
            (should-not (eq owned foreign))
            (should-not (equal initial-name requested-name))
            (should-not (equal final-name requested-name))
            (should-not (equal final-name initial-name))
            (should-not (get-buffer initial-name))
            (should (eq owned
                        (superchat-editor-workspace-validate-buffer
                         owned session-id)))
            (with-current-buffer owned
              (should (string-match-p "Persisted exact question" (buffer-string)))
              (should (string-match-p "Persisted exact answer" (buffer-string)))
              (should (markerp superchat--prompt-start))
              (should (eq owned (marker-buffer superchat--prompt-start))))
            (should (equal foreign-content
                           (with-current-buffer foreign (buffer-string))))
            (should (eq foreign-mode (buffer-local-value 'major-mode foreign)))
            (should (eq foreign-modified (buffer-modified-p foreign)))
            (should (eq foreign-marker
                        (buffer-local-value 'superchat--prompt-start foreign)))
            (should (eq foreign
                        (marker-buffer foreign-marker)))
            (should (= foreign-marker-position
                       (marker-position foreign-marker)))
            (should (eq foreign-marker-insertion-type
                        (marker-insertion-type foreign-marker)))
            (should (eq foreign-owned-local
                        (local-variable-p
                         'superchat--editor-workspace-owned-p foreign)))
            (should (eq foreign-session-local
                        (local-variable-p
                         'superchat--editor-workspace-session-id foreign)))))
      (superchat-db-close)
      (when (buffer-live-p foreign) (kill-buffer foreign))
      (native-ui-test--discard-workspace session-id)
      (delete-directory directory t))))

(ert-deftest native-ui/direct-authority-closes-associated-tagged-tab ()
  "The public authority closes associated tabs without a caller-side flag."
  (let ((session-id "native-ui-direct-authority-tab")
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                     (lambda (&optional _current-run-id)
                       '(:active-runs nil))))
            (should (eq 'closed
                        (superchat-session-workspace-close session-id))))
          (should-not (buffer-live-p buffer))
          (should-not (native-ui-test--session-tabs session-id)))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/earlier-third-party-veto-prevents-all-session-effects ()
  "A prevent hook before Superchat vetoes without invoking its authority."
  (let ((session-id "native-ui-earlier-veto")
        (authority-calls 0)
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (let ((tab-bar-tab-prevent-close-functions
                 (list (lambda (&rest _args) t)
                       #'superchat-editor-workspace-prevent-tab-close)))
            (cl-letf (((symbol-function 'superchat-session-workspace-close)
                       (lambda (_candidate)
                         (setq authority-calls (1+ authority-calls))
                         'closed)))
              (tab-bar-close-tab 1)))
          (should (= 0 authority-calls))
          (should (buffer-live-p buffer))
          (should (eq 'open
                      (superchat-editor-workspace-state session-id)))
          (should (= 1 (length (native-ui-test--session-tabs session-id))))
          (should (= 2 (length (tab-bar-tabs)))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/superchat-exclusively-commits-before-later-veto-hook ()
  "After Superchat accepts a tagged close, no later prevent hook participates."
  (let ((session-id "native-ui-exclusive-close")
        (later-hook-calls 0)
        cancelled
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (let ((tab-bar-tab-prevent-close-functions
                 (list
                  #'superchat-editor-workspace-prevent-tab-close
                  (lambda (&rest _args)
                    (setq later-hook-calls (1+ later-hook-calls))
                    t))))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         (list :active-runs
                               (list (list :run-id "exclusive-target"
                                           :session-id session-id
                                           :status 'running-model)))))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (run-id &optional _response)
                         (push run-id cancelled)))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (tab-bar-close-tab 1)))
          (should (= 0 later-hook-calls))
          (should (equal '("exclusive-target") cancelled))
          (should-not (buffer-live-p buffer))
          (should-not (native-ui-test--session-tabs session-id))
          (should (= 1 (length (tab-bar-tabs))))
          (should (eq 'closed
                      (superchat-editor-workspace-state session-id))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/internal-tab-delete-runs-pre-close-hook-once ()
  "The exclusive Adapter performs exactly one native pre-close notification."
  (let ((session-id "native-ui-one-pre-close")
        (pre-close-calls 0))
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (superchat-editor-workspace-buffer session-id 'protocol t)
          (let ((tab-bar-tab-prevent-close-functions
                 (list #'superchat-editor-workspace-prevent-tab-close))
                (tab-bar-tab-pre-close-functions
                 (list (lambda (&rest _args)
                         (setq pre-close-calls (1+ pre-close-calls))))))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         '(:active-runs nil))))
              (tab-bar-close-tab 1)))
          (should (= 1 pre-close-calls))
          (should-not (native-ui-test--session-tabs session-id)))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/tab-delete-error-never-leaves-closed-tagged-session ()
  "A native deletion error cannot commit closed while its exact tag survives."
  (let ((session-id "native-ui-tab-delete-error")
        (delete-attempts 0))
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (superchat-editor-workspace-buffer session-id 'protocol t)
          (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                     (lambda (&optional _current-run-id)
                       '(:active-runs nil)))
                    ((symbol-function 'tab-bar-close-tab)
                     (lambda (&rest _args)
                       (setq delete-attempts (1+ delete-attempts))
                       (error "simulated native tab deletion failure"))))
            (condition-case nil
                (superchat-session-workspace-close session-id)
              (error nil)))
          (should (= 1 delete-attempts))
          (should-not
           (and (eq 'closed
                    (superchat-editor-workspace-state session-id))
                (native-ui-test--session-tabs session-id))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/cancel-error-without-owned-buffer-is-retryable ()
  "Cancel failure tries the snapshot, preserves the tab, and retries later."
  (let ((session-id "native-ui-cancel-error-no-owned")
        (active-run-ids '("cancel-error-a" "cancel-error-b"))
        (fail-first t)
        (confirmation-calls 0)
        attempts)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                     (lambda (&optional _current-run-id)
                       (list
                        :active-runs
                        (mapcar
                         (lambda (run-id)
                           (list :run-id run-id
                                 :session-id session-id
                                 :status 'running-model))
                         active-run-ids))))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (run-id &optional _response)
                       (setq attempts (append attempts (list run-id)))
                       (if (and fail-first
                                (equal run-id "cancel-error-a"))
                           (error "simulated synchronous cancel failure")
                         (setq active-run-ids
                               (delete run-id active-run-ids)))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _args)
                       (setq confirmation-calls
                             (1+ confirmation-calls))
                       t)))
            (should-error
             (superchat-session-workspace-close session-id))
            (should (equal '("cancel-error-a" "cancel-error-b") attempts))
            (should (equal '("cancel-error-a") active-run-ids))
            (should (eq 'closing
                        (superchat-editor-workspace-state session-id)))
            (should (= 1 (length
                           (native-ui-test--session-tabs session-id))))
            (setq fail-first nil)
            (should (eq 'closed
                        (superchat-session-workspace-close session-id)))
            (should (= 2 confirmation-calls))
            (should-not active-run-ids)
            (should-not (native-ui-test--session-tabs session-id))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/cancel-error-with-owned-buffer-defers-resource-close ()
  "Cancel failure preserves owned resources until a successful confirmed retry."
  (let ((session-id "native-ui-cancel-error-owned")
        (active-run-ids '("owned-cancel-a" "owned-cancel-b"))
        (fail-first t)
        (confirmation-calls 0)
        attempts
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                     (lambda (&optional _current-run-id)
                       (list
                        :active-runs
                        (mapcar
                         (lambda (run-id)
                           (list :run-id run-id
                                 :session-id session-id
                                 :status 'running-model))
                         active-run-ids))))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (run-id &optional _response)
                       (setq attempts (append attempts (list run-id)))
                       (if (and fail-first
                                (equal run-id "owned-cancel-a"))
                           (error "simulated synchronous cancel failure")
                         (setq active-run-ids
                               (delete run-id active-run-ids)))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _args)
                       (setq confirmation-calls
                             (1+ confirmation-calls))
                       t)))
            (should-error
             (superchat-session-workspace-close session-id))
            (should (equal '("owned-cancel-a" "owned-cancel-b") attempts))
            (should (buffer-live-p buffer))
            (should (eq 'closing
                        (superchat-editor-workspace-state session-id)))
            (should (= 1 (length
                           (native-ui-test--session-tabs session-id))))
            (setq fail-first nil)
            (should (eq 'closed
                        (superchat-session-workspace-close session-id)))
            (should (= 2 confirmation-calls))
            (should-not active-run-ids)
            (should-not (buffer-live-p buffer))
            (should-not (native-ui-test--session-tabs session-id))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/pre-close-quit-neutralizes-tag-and-propagates ()
  "A nonlocal quit during native deletion cannot leave a closed Session tag."
  (let ((session-id "native-ui-pre-close-quit")
        quit-seen
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (let ((tab-bar-tab-pre-close-functions
                 (list (lambda (&rest _args) (signal 'quit nil)))))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         '(:active-runs nil))))
              (condition-case nil
                  (superchat-session-workspace-close session-id)
                (quit (setq quit-seen t)))))
          (should quit-seen)
          (should-not (buffer-live-p buffer))
          (should (eq 'closed
                      (superchat-editor-workspace-state session-id)))
          (should-not (native-ui-test--session-tabs session-id)))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/non-session-tab-is-transparent-to-close-hook ()
  "The native Adapter ignores tabs without an exact Session parameter."
  (let (closed)
    (cl-letf (((symbol-function 'superchat-session-workspace-close)
               (lambda (session-id)
                 (push session-id closed)
                 'closed)))
      (should-not
       (superchat-editor-workspace-prevent-tab-close
        '(tab (name . "ordinary")) nil)))
    (should-not closed)))

(ert-deftest native-ui/manual-and-native-close-share-one-authority ()
  "The command and native hook both call the programmatic close authority."
  (let ((session-id "native-ui-shared-close-authority")
        calls)
    (native-ui-test--with-isolated-tabs
      (native-ui-test--tag-current-tab session-id)
      (cl-letf (((symbol-function 'superchat-session-workspace-close)
                 (lambda (candidate)
                   (push candidate calls)
                   'rejected)))
        (call-interactively #'superchat-close-session-workspace)
        (should
         (superchat-editor-workspace-prevent-tab-close
          (car (native-ui-test--session-tabs session-id)) nil)))
      (should (equal (list session-id session-id) calls)))))

(ert-deftest native-ui/retained-close-keeps-tab-and-manual-retry-closes-it ()
  "A kill refusal vetoes native close; an allowed manual retry closes both."
  (let ((session-id "native-ui-retained-tab")
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions (list (lambda () nil))))
          (let ((tab-bar-tab-prevent-close-functions
                 (list #'superchat-editor-workspace-prevent-tab-close)))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         '(:active-runs nil))))
              (tab-bar-close-tab 1)
              (should (= 1
                         (length
                          (native-ui-test--session-tabs session-id))))
              (should (buffer-live-p buffer))
              (should (eq 'retained
                          (superchat-editor-workspace-state session-id)))
              (with-current-buffer buffer
                (setq-local kill-buffer-query-functions nil))
              (let* ((tagged
                      (car (native-ui-test--session-tabs session-id)))
                     (index (seq-position (tab-bar-tabs) tagged #'eq)))
                (tab-bar-select-tab (1+ index)))
              ;; Batch calls have no command-loop provenance, so model the
              ;; built-in interactive predicate without replacing the command.
              (cl-letf (((symbol-function 'called-interactively-p)
                         (lambda (&optional _kind) t)))
                (should (eq 'closed
                            (call-interactively
                             #'superchat-close-session-workspace))))
              (should-not (buffer-live-p buffer))
              (should-not
               (native-ui-test--session-tabs session-id)))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/accepted-native-close-orders-effects-before-tab ()
  "Native close freezes, cancels target Runs, kills owned buffers, then the tab."
  (let ((session-id "native-ui-accepted-tab")
        events
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (tab-bar-new-tab)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook
                      (lambda () (setq events (append events '(kill))))
                      nil t))
          (let ((tab-bar-tab-prevent-close-functions
                 (list #'superchat-editor-workspace-prevent-tab-close))
                (tab-bar-tab-pre-close-functions
                 (list (lambda (&rest _args)
                         (setq events (append events '(tab-close)))))))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         (list :active-runs
                               (list (list :run-id "native-target"
                                           :session-id session-id
                                           :status 'running-model)
                                     (list :run-id "native-other"
                                           :session-id "native-other-session"
                                           :status 'running-model)))))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (run-id &optional _response)
                         (setq events
                               (append events (list (intern run-id))))))
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (tab-bar-close-tab 1)))
          (should (equal '(native-target kill tab-close) events))
          (should-not (buffer-live-p buffer))
          (should-not (native-ui-test--session-tabs session-id)))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/sole-session-tab-closes-into-neutral-tab ()
  "Closing the sole tagged tab leaves one untagged native tab behind."
  (let ((session-id "native-ui-sole-session-tab")
        buffer)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (native-ui-test--tag-current-tab session-id)
          (setq buffer
                (superchat-editor-workspace-buffer session-id 'protocol t))
          (let ((tab-bar-tab-prevent-close-functions
                 (list #'superchat-editor-workspace-prevent-tab-close)))
            (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                       (lambda (&optional _current-run-id)
                         '(:active-runs nil))))
              (tab-bar-close-tab)))
          (should-not (buffer-live-p buffer))
          (should (= 1 (length (tab-bar-tabs))))
          (should-not
           (native-ui-test--tab-session-id (car (tab-bar-tabs)))))
      (native-ui-test--discard-workspace session-id))))

(ert-deftest native-ui/sessions-entry-point-prints-populated-list ()
  "The public sessions entry point prints a non-empty tabulated list."
  (let (buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-tape-browser--sessions-rows)
                   (lambda ()
                     '(("session-ui"
                        ["session-ui" "1" "2025-01-01 00:00:00" "0"]))))
                  ((symbol-function 'display-buffer)
                   (lambda (candidate &rest _args)
                     (setq buffer candidate)
                     candidate)))
          (setq buffer (superchat-sessions))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (eq major-mode 'tabulated-list-mode))
            (should (string-match-p "session-ui" (buffer-string)))
            (should (equal '("Last activity" . t)
                           tabulated-list-sort-key))
            (should (eq t (nth 2 (aref tabulated-list-format 2))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest native-ui/resume-timeout-is-owned-by-the-current-view-and-run ()
  "A resumed Run's safety timer belongs to its View and captured Run ID."
  (let ((superchat-buffer-name (generate-new-buffer-name " *superchat-resume-view*"))
        (superchat-response-timeout nil)
        cancelled)
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (let ((owner
                 (superchat-llm--make-transport-timer-owner
                  :buffer (current-buffer) :run-id "run-view")))
            (setq-local superchat--pause-requested t
                        superchat--pause-run-id "run-view"
                        superchat--active-run-id "run-view"
                        superchat--pending-send-request
                        (superchat-llm--make-pending-pause
                         :run-id "run-view"
                         :transport-owner owner
                         :reason 'tool-followup
                         :continuation #'ignore)))
          (cl-letf (((symbol-function 'superchat-runtime-effect-capable-p)
                     (lambda (run-id) (equal run-id "run-view")))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args) ""))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (run-id &optional _response)
                       (setq cancelled run-id))))
            (superchat-resume)
            (should (and (boundp 'superchat--resume-timeout-timer)
                         (timerp superchat--resume-timeout-timer)))
            (let ((timer superchat--resume-timeout-timer))
              (funcall (timer--function timer)))
            (should (equal "run-view" cancelled))
            (should-not superchat--resume-timeout-timer)))
      (when (get-buffer superchat-buffer-name)
        (with-current-buffer superchat-buffer-name
          (when (and (boundp 'superchat--resume-timeout-timer)
                     (timerp superchat--resume-timeout-timer))
            (cancel-timer superchat--resume-timeout-timer))
          (when (timerp superchat--active-timeout-timer)
            (cancel-timer superchat--active-timeout-timer)))
        (kill-buffer superchat-buffer-name)))))

(ert-deftest native-ui/cancel-clears-pause-continuation-in-current-view ()
  "Cancelling a paused Run clears its View-local continuation state."
  (let ((superchat-buffer-name (generate-new-buffer-name " *superchat-cancel-view*"))
        cancelled)
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (let ((owner
                 (superchat-llm--make-transport-timer-owner
                  :buffer (current-buffer) :run-id "run-cancel")))
            (setq-local superchat-mode t
                        superchat--pause-requested t
                        superchat--pause-run-id "run-cancel"
                        superchat--active-run-id "run-cancel"
                        superchat--pending-send-request
                        (superchat-llm--make-pending-pause
                         :run-id "run-cancel"
                         :transport-owner owner
                         :reason 'tool-followup
                         :continuation #'ignore)))
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("run-cancel")))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (run-id &optional _response)
                       (setq cancelled run-id)))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args) ""))
                    ((symbol-function 'message) #'ignore))
            (superchat-cancel))
          (should (equal "run-cancel" cancelled))
          (should-not superchat--pause-requested)
          (should-not superchat--pause-run-id)
          (should-not superchat--pending-send-request))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest native-ui/runtime-quiescence-clears-owned-pause-state ()
  "Runtime quiescence clears only the terminated Run's View state."
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (name (generate-new-buffer-name " *superchat-release-view*"))
        entry)
    (unwind-protect
        (with-current-buffer (get-buffer-create name)
          (let ((owner
                 (superchat-llm--make-transport-timer-owner
                  :buffer (current-buffer) :run-id "run-release")))
            (setq-local superchat--pause-requested t
                        superchat--pause-run-id "run-release"
                        superchat--active-run-id "run-release"
                        superchat--pending-send-request
                        (superchat-llm--make-pending-pause
                         :run-id "run-release"
                         :transport-owner owner
                         :reason 'tool-followup
                         :continuation #'ignore)
                        superchat--resume-timeout-run-id "run-release"
                        superchat--resume-timeout-timer
                        (run-with-timer 100 nil #'ignore)
                        superchat--active-timeout-timer
                        (run-with-timer 100 nil #'ignore)))
          (setq entry (list :run-id "run-release"
                            :buffer (current-buffer)
                            :buffer-name name
                            :status 'completed
                            :deadline-timer nil))
          (superchat-runtime--put entry)
          (superchat-runtime--quiesce entry)
          (should-not superchat--pause-requested)
          (should-not superchat--pause-run-id)
          (should-not superchat--pending-send-request)
          (should-not (timerp superchat--resume-timeout-timer))
          (should-not (timerp superchat--active-timeout-timer)))
      (when (get-buffer name)
        (with-current-buffer name
          (when (timerp superchat--resume-timeout-timer)
            (cancel-timer superchat--resume-timeout-timer))
          (when (timerp superchat--active-timeout-timer)
            (cancel-timer superchat--active-timeout-timer)))
        (kill-buffer name)))))

(ert-deftest native-ui/runtime-quiescence-without-owner-needs-exact-active-run ()
  "Quiescence never claims a timer without an exact active Run match."
  (let ((name (generate-new-buffer-name " *superchat-release-owner*")))
    (unwind-protect
        (with-current-buffer (get-buffer-create name)
          (dolist (active-run-id '(nil "run-other" "run-release"))
            (let* ((superchat-runtime--runs
                    (make-hash-table :test #'equal))
                   (timer (run-with-timer 100 nil #'ignore))
                   (entry (list :run-id "run-release"
                                :buffer (current-buffer)
                                :buffer-name name
                                :status 'completed
                                :deadline-timer nil)))
              (setq-local superchat--active-run-id active-run-id
                          superchat--active-timeout-timer timer
                          superchat--active-timeout-timer-owner nil)
              (superchat-runtime--put entry)
              (superchat-runtime--quiesce entry)
              (if (equal active-run-id "run-release")
                  (progn
                    (should-not (memq timer timer-list))
                    (should-not superchat--active-timeout-timer))
                (should (memq timer timer-list))
                (should (eq timer superchat--active-timeout-timer)))
              (when (timerp timer)
                (cancel-timer timer)))))
      (when (get-buffer name)
        (with-current-buffer name
          (when (timerp superchat--active-timeout-timer)
            (cancel-timer superchat--active-timeout-timer)))
        (kill-buffer name)))))

(ert-deftest native-ui/keymap-has-run-control-binding ()
  "C-c C-g opens the Run Control panel on demand."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-g"))
              'superchat-agents-panel-show)))

(ert-deftest native-ui/keymap-has-follow-up-queue-binding ()
  "C-c C-q queues a draft for the next user round."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-q"))
              'superchat-queue-follow-up)))

(ert-deftest native-ui/mode-line-reports-live-run-state ()
  "The mode line projects the active Run without owning its lifecycle."
  (with-temp-buffer
    (setq-local superchat--active-run-id "run-live")
    (setq-local superchat--pause-requested nil)
    (cl-letf (((symbol-function 'superchat-runtime-active-runs)
               (lambda () '("run-live")))
              ((symbol-function 'superchat-runtime-status)
               (lambda (_run-id)
                 '(:status running-model :capability code :model "model-x"))))
      (should (equal " · run running-model · code · model-x · C-c C-g"
                     (superchat--mode-line-run-state)))
      (setq-local superchat--pause-requested t)
      (should (equal " · run PAUSED · code · model-x · C-c C-g"
                     (superchat--mode-line-run-state))))))

(ert-deftest native-ui/header-line-reports-agent-context-state ()
  "The chat header projects live model, attempt, and context usage."
  (with-temp-buffer
    (setq-local superchat--active-run-id "run-live")
    (setq-local superchat--pause-requested nil)
    (let ((superchat-handoff-auto-enabled t)
          (status '(:status running-model :model "model-x" :attempt 2
                    :context-used 6800 :context-limit 8192
                    :context-limit-source lm-studio-runtime
                    :context-estimated-p t)))
      (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                 (lambda () '("run-live")))
                ((symbol-function 'superchat-runtime-status)
                 (lambda (_run-id) status)))
        (let ((text (substring-no-properties
                     (superchat--header-line-run-state))))
          (should (string-match-p "model-x · thinking · attempt 2" text))
          (should (string-match-p
                   "context ~6.8k/8.2k 83.0% \\[runtime\\]" text))
          (should (string-suffix-p "auto-compact" text)))
        (setq status (plist-put status :context-recovery 'compacting))
        (should (string-match-p
                 "model-x · compacting · attempt 2"
                 (substring-no-properties
                 (superchat--header-line-run-state))))))))

(ert-deftest native-ui/status-band-projects-runtime-background-and-mcp ()
  "The two status surfaces read one Runtime projection plus live MCP health."
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (now 120.0))
    (with-temp-buffer
      (setq-local superchat--active-run-id "run-foreground"
                  superchat--pause-requested nil)
      (superchat-runtime--put
       (list :run-id "run-foreground"
             :session-id "status-session"
             :transcript-owner "owner-foreground"
             :status 'running-model
             :capability "chat"
             :model "model-x"
             :attempt 2
             :started-at 100.0
             :context-used 6800
             :context-limit 8192
             :context-limit-source 'lm-studio-runtime
             :context-estimated-p t))
      (superchat-runtime--put
       (list :run-id "run-background"
             :session-id "status-session"
             :transcript-owner "owner-background"
             :status 'running-model
             :capability "workflow"
             :model "model-y"
             :started-at 110.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _args) now))
                ((symbol-function 'superchat-mcp-health-summary)
                 (lambda () '(:servers 3 :ok 2 :degraded 1))))
        (let ((header (substring-no-properties
                       (superchat--header-line-run-state)))
              (mode-line (substring-no-properties
                          (superchat--mode-line-run-state))))
          (should (string-match-p "model-x · thinking · attempt 2 · 20s"
                                  header))
          (should (string-match-p "context ~6.8k/8.2k 83.0% \\[runtime\\]"
                                  header))
          (should (string-match-p "run thinking · chat · model-x · 20s"
                                  mode-line))
          (should (string-match-p "background 1" mode-line))
          (should (string-match-p "MCP 2 ok/1 degraded" mode-line))
          (should-not (string-match-p "cost" (concat header mode-line))))))))

(ert-deftest native-ui/status-band-preserves-percent-in-real-mode-line-render ()
  "Status percentages survive the actual header and lighter formatters."
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (now 120.0))
    (with-temp-buffer
      (setq-local superchat--active-run-id "run-percent"
                  superchat--pause-requested nil)
      (superchat-runtime--put
       (list :run-id "run-percent"
             :session-id "percent-session"
             :transcript-owner "owner-percent"
             :status 'running-model
             :capability "chat"
             :model "model-50%"
             :attempt 1
             :started-at 100.0
             :context-used 6800
             :context-limit 8192
             :context-limit-source 'lm-studio-runtime
             :context-estimated-p t))
      (let ((superchat-manual-models t))
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _args) now)))
          (superchat-mode 1)
          (unwind-protect
              (let* ((header
                      (format-mode-line header-line-format nil nil
                                        (current-buffer)))
                     (lighter-spec
                      (cdr (assq 'superchat-mode minor-mode-alist)))
                     (lighter
                      (format-mode-line lighter-spec nil nil
                                        (current-buffer))))
                (if (and noninteractive
                         (string-empty-p header)
                         (string-empty-p lighter))
                    ;; Emacs 31 deliberately makes `format-mode-line' a
                    ;; no-op in a --batch process.  Still evaluate the exact
                    ;; :eval bodies passed to that real formatter and assert
                    ;; that its input preserves %% for the live formatter.
                    (let ((header-input
                           (eval (nth 1 (nth 1 header-line-format))))
                          (lighter-input
                           (eval (nth 1 (car lighter-spec)))))
                      (should (string-match-p "83.0%%" header-input))
                      (should (string-match-p "model-50%%" lighter-input)))
                  (should (string-match-p "83.0%" header))
                  (should-not (string-match-p "83.0%%" header))
                  (should (string-match-p "model-50%" lighter))
                  (should-not (string-match-p "model-50%%" lighter))))
            (superchat-mode -1)))))))

(ert-deftest native-ui/status-refresh-timer-is-released-by-terminal-mode-off-and-kill ()
  "A View status timer leaves `timer-list' on every terminal path."
  (let ((superchat-manual-models t)
        (superchat-runtime--runs (make-hash-table :test #'equal))
        terminal-buffer mode-buffer killed-buffer)
    (unwind-protect
        (progn
          (setq terminal-buffer
                (get-buffer-create " *superchat-status-terminal*"))
          (with-current-buffer terminal-buffer
            (setq-local superchat-buffer-name (buffer-name))
            (superchat-mode 1)
            (superchat-runtime--put
             '(:run-id "run-terminal-1"
               :session-id "status-timer-session"
               :status running-model
               :started-at 1.0))
            (superchat--track-active-run '(:run-id "run-terminal-1"))
            (let ((timer superchat--status-refresh-timer))
              (superchat-runtime--put
               '(:run-id "run-terminal-2"
                 :session-id "status-timer-session"
                 :status running-model
                 :started-at 2.0))
              (superchat--track-active-run '(:run-id "run-terminal-2"))
              (should (eq timer superchat--status-refresh-timer))
              (superchat-runtime--update "run-terminal-1" :status 'completed)
              (cl-letf (((symbol-function 'message) #'ignore))
                (superchat--notify-run-terminal
                 '(:run-id "run-terminal-1"
                   :status completed
                   :session-id "status-timer-session")))
              (should (eq timer superchat--status-refresh-timer))
              (should (memq timer timer-list))
              (superchat-runtime--update "run-terminal-2" :status 'completed)
              (cl-letf (((symbol-function 'message) #'ignore))
                (superchat--notify-run-terminal
                 '(:run-id "run-terminal-2"
                   :status completed
                   :session-id "status-timer-session")))
              (should-not superchat--status-refresh-timer)
              (should-not (memq timer timer-list))))

          (setq mode-buffer (get-buffer-create " *superchat-status-mode-off*"))
          (with-current-buffer mode-buffer
            (setq-local superchat-buffer-name (buffer-name))
            (superchat-mode 1)
            (superchat--track-active-run '(:run-id "run-mode-off-timer"))
            (let ((timer superchat--status-refresh-timer))
              (superchat-mode -1)
              (should-not superchat--status-refresh-timer)
              (should-not (memq timer timer-list))))

          (setq killed-buffer
                (get-buffer-create " *superchat-status-kill-buffer*"))
          (with-current-buffer killed-buffer
            (setq-local superchat-buffer-name (buffer-name))
            (superchat-mode 1)
            (superchat--track-active-run '(:run-id "run-kill-timer"))
            (let ((timer superchat--status-refresh-timer))
              (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                         #'ignore))
                (kill-buffer killed-buffer))
              (should-not (memq timer timer-list)))))
      (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                 #'ignore))
        (dolist (buffer (list terminal-buffer mode-buffer killed-buffer))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (timerp superchat--status-refresh-timer)
                (cancel-timer superchat--status-refresh-timer)))
            (kill-buffer buffer)))))))

(ert-deftest native-ui/welcome-recent-sessions-read-tape-and-open-button ()
  "The welcome list projects recent Tape Sessions and its button opens one."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-welcome-test-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-welcome-recent-session-count 2)
         opened)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (superchat-tape-append-material "welcome-older" "user"
                                          "Older session first line")
          (superchat-tape-append-material "welcome-older" "assistant"
                                          "Older answer")
          (superchat-tape-append-material "welcome-newer" "user"
                                          "Newest session first line\nsecond line")
          (let ((rows (superchat--welcome-recent-session-rows)))
            (should (equal '("welcome-newer" "welcome-older")
                           (mapcar (lambda (row) (plist-get row :session-id))
                                   rows)))
            (should (equal "Newest session first line"
                           (plist-get (car rows) :summary)))
            (should (stringp (plist-get (car rows) :last-activity))))
          (with-temp-buffer
            (org-mode)
            (cl-letf (((symbol-function 'superchat-open-session)
                       (lambda (session-id) (setq opened session-id))))
              (superchat--insert-welcome-recent-sessions)
              (should (string-match-p "\\* Recent sessions" (buffer-string)))
              (should (string-match-p "welcome-newer" (buffer-string)))
              (should (string-match-p "Newest session first line"
                                      (buffer-string)))
              (goto-char (point-min))
              (search-forward "welcome-newer")
              (backward-char 1)
              (let ((button (widget-at)))
                (should button)
                (should (eq (lookup-key (widget-get button :keymap)
                                        (kbd "RET"))
                            'widget-button-press))
                ;; Exercise the same key command a user invokes, including
                ;; widget.el's real notify argument shape.
                (should (eq (key-binding (kbd "RET") nil t)
                            'widget-button-press))
                (call-interactively (key-binding (kbd "RET") nil t))
                (should (equal "welcome-newer" opened))))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/reopened-session-folds-runtime-audit-rows ()
  "A rebuilt View folds persisted run/turn system rows under Org headings."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-audit-view-test-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (session-id "audit-view-session")
         buffer)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (superchat-tape-append-material session-id "user" "Persisted question")
          (superchat-tape-append-event
           (list :type "run.started"
                 :session-id session-id
                 :run-id "run-audit"
                 :turn-id "turn-audit"
                 :summary "run.started"))
          (superchat-tape-append-event
           (list :type "turn.started"
                 :session-id session-id
                 :run-id "run-audit"
                 :turn-id "turn-audit"
                 :summary "turn.started"))
          (setq buffer (superchat--render-session-view session-id))
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "** Runtime audit: run.started" nil t))
            (forward-line 1)
            (should (invisible-p (point)))
            (goto-char (point-min))
            (should (search-forward "** Runtime audit: turn.started" nil t))
            (forward-line 1)
            (should (invisible-p (point)))
            (goto-char (point-min))
            (should (search-forward "Persisted question" nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/title-insertions-use-a-valid-face ()
  "The chat title uses an installed face rather than a removed font-lock face."
  (should (facep 'org-document-title))
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "superchat.el"
                       (file-name-directory (locate-library "superchat"))))
    (should-not (search-forward "font-lock-title-face" nil t))))

(ert-deftest native-ui/superchat-mode-installs-and-restores-header-line ()
  "The chat status header does not destroy a pre-existing header line."
  (with-temp-buffer
    (setq-local header-line-format "original")
    (superchat-mode 1)
    (should (equal '(" " (:eval
                         (superchat--mode-line-safe-string
                          (superchat--header-line-run-state))))
                   header-line-format))
    (superchat-mode 1)
    (superchat-mode -1)
    (should (equal "original" header-line-format))))

;; ═══════════════════════════════════════════════════════════
;; superchat-dired-send
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/dired-send-function-defined ()
  "superchat-dired-send is defined."
  (should (fboundp 'superchat-dired-send)))

(ert-deftest native-ui/dired-send-errors-outside-dired ()
  "superchat-dired-send errors when not in dired-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (superchat-dired-send))))

;; ═══════════════════════════════════════════════════════════
;; Embark key naming (no collision with existing commands)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/embark-key-not-bound-by-default ()
  "Embark keys 's' and 'S' are not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map "s"))
  (should-not (lookup-key superchat-mode-map "S")))

(ert-deftest native-ui/dired-key-not-bound-by-default-in-chat ()
  "dired C-c s is not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map (kbd "C-c s"))))

;; ═══════════════════════════════════════════════════════════
;; Universal request entry + inspectable request plan
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/request-plan-build-defers-post-turn-effects ()
  "Building a request plan must not run post-turn effects."
  (let ((superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions nil)
        (superchat-post-turn-functions
         (list (lambda (_turn) (error "post-turn hook ran during dry-run"))))
        (superchat-post-topic-finalized-functions nil)
        (superchat-memory-auto-recall-min-length 999999)
        (origin '(:buffer "source" :major-mode text-mode
                  :point 4 :region "selected text")))
    (with-temp-buffer
      (setq-local superchat--active-preset nil)
      (setq-local superchat--active-topology 'single)
      (setq-local superchat--active-authority-policy "default")
      (setq-local superchat--workspace-binding nil)
      (setq-local superchat--current-command nil)
      (setq-local superchat--pending-recalled-memories nil)
      (let ((plan (superchat-build-request-plan
                   "Explain this" "session-1" origin)))
        (should (eq (plist-get plan :type) :llm-query))
        (should (equal (plist-get plan :origin) origin))
        (should (equal (plist-get plan :session-id) "session-1"))
        (should (equal (plist-get plan :capability) "chat"))
        (should (eq (plist-get plan :topology) 'single))
        (should (equal (plist-get plan :authority-policy) "default"))
        (should (equal (plist-get plan :input) "Explain this"))
        (should (equal (plist-get plan :clean-input) "Explain this"))
        (should (equal (plist-get plan :user-message) "Explain this"))
        (should-not (plist-member plan :turn))
        (should-not (plist-member plan :finalized))
        (should (string-match-p "selected text"
                                (plist-get plan :prompt)))))))

(ert-deftest native-ui/request-plan-executes-the-inspected-value ()
  "Execution submits the inspected request without exposing a Turn."
  (let* ((plan (list :type :llm-query
                     :input "original"
                     :prompt "edited exact prompt"
                     :session-id "session-1"
                     :lang "English"))
         captured)
    (cl-letf (((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq captured request)
                 '(:run-id "run-1" :status accepted))))
      (superchat-execute-request-plan plan))
    (should (eq captured plan))
    (should-not (plist-member plan :turn))
    (should (equal (plist-get plan :prompt) "edited exact prompt"))
    (should (equal (plist-get (plist-get plan :run-handle) :run-id)
                   "run-1"))))

(ert-deftest native-ui/request-plan-targets-its-originating-view ()
  "Dispatch submits the View captured by the request plan."
  (let* ((target-name (generate-new-buffer-name " *superchat-view-target*"))
         (superchat-buffer-name " *superchat-default-target*")
         captured)
    (unwind-protect
        (with-current-buffer (get-buffer-create target-name)
          (let ((plan (list :type :llm-query
                            :prompt "hello"
                            :session-id "session-1"
                            :buffer-name target-name)))
            (cl-letf (((symbol-function 'superchat-runtime-submit)
                       (lambda (request)
                         (setq captured request)
                         '(:run-id "run-1" :status accepted))))
              (superchat--dispatch-result plan "English" nil))))
      (when (get-buffer target-name)
        (kill-buffer target-name)))
    (should (equal (plist-get captured :buffer-name) target-name))))

(ert-deftest native-ui/act-captures-region-from-read-only-buffer ()
  "`superchat-act' captures a read-only region without modifying its buffer."
  (let ((transient-mark-mode t)
        captured-input captured-origin captured-inspect)
    (with-temp-buffer
      (insert "alpha selected omega")
      (goto-char 7)
      (push-mark 15 t t)
      (setq buffer-read-only t)
      (cl-letf (((symbol-function 'superchat-submit)
                 (lambda (input origin inspect)
                   (setq captured-input input
                         captured-origin origin
                         captured-inspect inspect))))
        (superchat-act "Explain" t)))
    (should (equal captured-input "Explain"))
    (should captured-inspect)
    (should (equal (plist-get captured-origin :region) "selected"))))

(ert-deftest native-ui/request-inspector-sends-edited-prompt ()
  "The inspector sends the edited prompt without rebuilding the plan."
  (let* ((plan (list :type :llm-query
                     :prompt "original prompt"
                     :input "original"
                     :session-id "session-1"
                     :lang "English"
                     :origin nil))
         (buffer (superchat--display-request-plan plan))
         captured)
    (unwind-protect
        (with-current-buffer buffer
          (delete-region superchat--request-prompt-start
                         superchat--request-prompt-end)
          (goto-char superchat--request-prompt-start)
          (insert "edited prompt")
          (cl-letf (((symbol-function 'superchat--execute-plan-in-chat)
                     (lambda (request-plan)
                       (setq captured request-plan))))
            (superchat-request-send)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (should (eq captured plan))
    (should (equal (plist-get captured :prompt) "edited prompt"))))

(ert-deftest native-ui/request-plan-captures-image-attachment ()
  "A #path image is frozen into the plan and shown by the inspector."
  (let ((image (make-temp-file "superchat-plan-image-" nil ".webp"))
        (superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions nil)
        (superchat-post-turn-functions nil)
        (superchat-memory-auto-recall-min-length 999999))
    (unwind-protect
        (progn
          (with-temp-file image (insert "RIFF"))
          (with-temp-buffer
            (setq-local superchat--active-preset nil)
            (setq-local superchat--current-command nil)
            (setq-local superchat--current-context-files nil)
            (setq-local superchat--pending-recalled-memories nil)
            (let* ((plan (superchat-build-request-plan
                          (format "#%s Describe" image) "image-session"))
                   (buffer (superchat--display-request-plan plan)))
              (unwind-protect
                  (with-current-buffer buffer
                    (should (equal (plist-get plan :context-files)
                                   (list image)))
                    (should (string-match-p (regexp-quote image)
                                            (buffer-string)))
                    (should (string-match-p "image/webp, 4 bytes"
                                            (buffer-string))))
                (kill-buffer buffer)))))
      (delete-file image))))

(ert-deftest native-ui/request-inspector-toggles-native-llm-log ()
  "The inspector enables `llm-log' without losing prompt edits."
  (let* ((llm-log nil)
         (plan (list :type :llm-query :prompt "original"))
         (buffer (superchat--display-request-plan plan)))
    (unwind-protect
        (with-current-buffer buffer
          (delete-region superchat--request-prompt-start
                         superchat--request-prompt-end)
          (goto-char superchat--request-prompt-start)
          (insert "edited")
          (superchat-request-toggle-log)
          (should llm-log)
          (should (equal (plist-get plan :prompt) "edited"))
          (should (string-match-p "llm.el log: enabled" (buffer-string)))
          (superchat-request-toggle-log)
          (should-not llm-log))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest native-ui/elapsed-duration-is-compact-and-shared ()
  "Header, lighter, and Thinking use one compact duration format."
  (dolist (case '((59 "59s")
                  (60 "1m00s")
                  (61 "1m01s")
                  (3599 "59m59s")
                  (3600 "1h00m")
                  (3661 "1h01m")
                  (90061 "1d01h")))
    (pcase-let ((`(,seconds ,expected) case))
      (should
       (equal expected
              (superchat--runtime-elapsed-label
               (list :started-at 0 :completed-at seconds))))))
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (now 90061.0))
    (with-temp-buffer
      (setq-local superchat--active-run-id "run-long-elapsed"
                  superchat--pause-requested nil)
      (superchat-runtime--put
       '(:run-id "run-long-elapsed"
         :session-id "long-elapsed-session"
         :transcript-owner "owner-long-elapsed"
         :status running-model
         :capability "chat"
         :model "model-long"
         :started-at 0))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&rest _args) now)))
        (should (string-match-p "model-long · thinking · 1d01h"
                                (substring-no-properties
                                 (superchat--header-line-run-state))))
        (should (string-match-p "run thinking · chat · model-long · 1d01h"
                                (substring-no-properties
                                 (superchat--mode-line-run-state))))
        (should (equal "Thinking… 1d01h · latest thought"
                       (superchat--thinking-status-message
                        (superchat-runtime-status "run-long-elapsed")
                        "latest thought")))))))

(ert-deftest output-p1/final-opens-at-retained-owner-end ()
  "A callback-only final stays after A tools and before B's mutable region."
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name))
          (superchat-show-ttft nil))
      (let ((superchat--runtime-transcript-owner "custom-A"))
        (superchat--response-region-stream "A-NARRATION")
        (superchat--response-region-close nil nil)
        (superchat--response-region-insert-transcript "A-TOOL" :current-run))
      (let ((superchat--runtime-transcript-owner "custom-B"))
        (superchat--response-region-stream "B-PARTIAL"))
      (let ((superchat--runtime-transcript-owner "custom-A"))
        (superchat--response-region-close "A-FINAL" t))
      (let ((superchat--runtime-transcript-owner "custom-B"))
        (superchat--response-region-close "B-FINAL" t))
      (let ((text (buffer-string)))
        (should (string-match-p "A-NARRATION" text))
        (should (string-match-p "A-FINAL" text))
        (should (< (string-match "A-TOOL" text) (string-match "A-FINAL" text)))
        (should (< (string-match "A-FINAL" text) (string-match "B-FINAL" text)))
        (should-not (string-match-p "B-PARTIAL" text))))))

(ert-deftest output-p1/nil-round-close-is-idempotent ()
  "No-content closes add neither an empty Assistant nor a duplicate round."
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name))
          (superchat--runtime-transcript-owner "p1-owner"))
      (superchat--response-region-close nil nil)
      (superchat--response-region-close nil nil)
      (should (string-empty-p (buffer-string)))
      (superchat--response-region-stream "NARRATION")
      (superchat--response-region-close nil nil)
      (superchat--response-region-insert-transcript "TOOL" :current-run)
      (let ((text (buffer-string)))
        (superchat--response-region-close nil nil)
        (superchat--response-region-close "  \n" t)
        (should (equal text (buffer-string)))))))

(ert-deftest output-p1/render-empty-final-substitution-after-tool ()
  "Direct Render nil/empty finals use its existing substitute in their own segment.
The real LLM loop has a separate earlier empty-response substitution."
  (unless (sqlite-available-p) (ert-skip "SQLite unavailable"))
  (let* ((directory (make-temp-file "superchat-p1-empty-render-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "tape.db" directory))
          (dolist (answer '(nil ""))
            (with-temp-buffer
              (let ((superchat-buffer-name (buffer-name))
                    (superchat--runtime-transcript-owner "p1-empty-owner")
                    (superchat-show-ttft nil)
                    (expected "Assistant did not provide a response."))
                (setq-local superchat--session-id
                            (format "p1-render-empty-%S" answer))
                (superchat--response-region-stream "EMPTY-NARRATION")
                (superchat--response-region-close nil nil)
                (superchat--response-region-insert-transcript "EMPTY-TOOL" :current-run)
                (let ((before (buffer-string)))
                  (superchat--response-region-close nil nil)
                  (superchat--response-region-close nil nil)
                  (should (equal before (buffer-string))))
                (superchat--process-llm-result answer)
                (let ((text (buffer-string)))
                  (should (< (string-match "EMPTY-NARRATION" text)
                             (string-match "EMPTY-TOOL" text)))
                  (should (< (string-match "EMPTY-TOOL" text)
                             (string-match (regexp-quote expected) text)))
                  (should (= 1 (cl-count expected superchat--conversation-history
                                         :key (lambda (message) (plist-get message :content))
                                         :test #'equal)))
                  (goto-char (point-min))
                  (should (= 2 (how-many "^\\*\\* Assistant" (point-min) (point-max))))
                  (should (= 1 (how-many (regexp-quote expected) (point-min) (point-max)))))))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest output-p2/noncurrent-close-error-quit-keeps-boundaries ()
  "A failed final rewrite preserves B projection and temporary marker types."
  (dolist (failure '(error quit))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--response-region-begin "A")
        (superchat--response-region-stream "A-PART" "A")
        (superchat--response-region-insert-persistent "A-NOTICE" nil "A")
        (superchat--response-region-begin "B")
        (superchat--response-region-stream "B-PART" "B")
        (let* ((boundary (plist-get (cdr (assoc "A" superchat--response-region-states))
                                    :persistent-marker))
               (before (output-p2-test--projection)) caught)
          (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                     (lambda (_text) (signal failure '("P2 final fault")))))
            (condition-case condition
                (superchat--response-region-close "A-FINAL" t "A")
              ((error quit) (setq caught (car condition)))))
          (should (eq failure caught))
          (should (equal before (output-p2-test--projection)))
          (should-not (marker-insertion-type boundary))
          (should (string-match-p "B-PART" (buffer-string))))))))

(ert-deftest output-p2/noncurrent-insert-error-quit-keeps-marker-types ()
  "A partial insertion cannot leave the closed A boundary advancing into B."
  (dolist (failure '(error quit))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--response-region-begin "A")
        (superchat--response-region-close "A-FINAL" t "A")
        (superchat--response-region-begin "B")
        (superchat--response-region-stream "B-PARTIAL" "B")
        (let* ((a-end (plist-get (cdr (assoc "A" superchat--response-region-states))
                                 :assistant-end))
               (before (output-p2-test--projection))
               (insert-text (symbol-function 'insert-before-markers)) caught)
          (cl-letf (((symbol-function 'insert-before-markers)
                     (lambda (&rest text)
                       (apply insert-text text)
                       (signal failure '("P2 insertion fault")))))
            (condition-case condition
                (superchat--response-region-insert-transcript "A-TOOL\n" :current-run "A")
              ((error quit) (setq caught (car condition)))))
          (should (eq failure caught))
          (should (equal before (output-p2-test--projection)))
          (should-not (marker-insertion-type a-end)))))))

(defun output-p2-test--projection ()
  "Return the seven compatibility projection values without copying markers."
  (mapcar #'symbol-value
          '(superchat--response-region-current-owner
            superchat--response-start-marker
            superchat--assistant-response-start-marker
            superchat--assistant-response-end-marker
            superchat--response-region-persistent-marker
            superchat--prompt-start superchat--current-response-parts)))

(ert-deftest output-p2/explicit-owner-keeps-current-projection ()
  (dolist (ambient '(nil "WRONG"))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (let ((superchat--runtime-transcript-owner "A"))
          (superchat--response-region-stream "A-TEXT")
          (superchat--response-region-close nil nil))
        (let ((superchat--runtime-transcript-owner "B"))
          (superchat--response-region-stream "B-TEXT"))
        (let ((before (output-p2-test--projection))
              (superchat--runtime-transcript-owner ambient))
          (superchat--response-region-insert-transcript "A-TOOL" :current-run "A")
          (superchat--response-region-stream "A-LATER" "A")
          (superchat--response-region-close "A-FINAL" t "A")
          (should (equal before (output-p2-test--projection)))
          (should (string-match-p "B-TEXT" (buffer-string)))
          (should-not (assoc "WRONG" superchat--response-region-states)))))))

(ert-deftest output-p2/absent-owner-does-not-adopt-pending ()
  (dolist (ambient '(nil "WRONG"))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--response-region-begin "B")
        (superchat--response-region-stream "B-TEXT" "B")
        (superchat--response-region-ensure-view-prompt t)
        (let ((pending (cdr (assoc :pending superchat--response-region-states)))
              (before (output-p2-test--projection))
              (superchat--runtime-transcript-owner ambient))
          (superchat--update-status "A-STATUS" "A")
          (should (eq pending (cdr (assoc :pending superchat--response-region-states))))
          (should (equal before (output-p2-test--projection)))
          (let ((a (cdr (assoc "A" superchat--response-region-states))))
            (should-not (plist-get a :prompt-marker))
            (should-not (plist-get a :assistant-start))
            (should-not (plist-get a :parts))))))))

(ert-deftest output-p2/begin-existing-owner-does-not-reselect ()
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name))
          (superchat--runtime-transcript-owner nil))
      (superchat--prepare-for-response)
      (let ((pending (cdr (assoc :pending superchat--response-region-states))))
        (superchat--response-region-begin "A")
        (should (eq pending (cdr (assoc "A" superchat--response-region-states)))))
      (superchat--response-region-begin "B")
      (let ((before (output-p2-test--projection)))
        (superchat--response-region-begin "A")
        (should (equal before (output-p2-test--projection))))
      (superchat--response-region-insert-transcript "C" :current-run "C")
      (let ((before (output-p2-test--projection)))
        (superchat--response-region-begin "C")
        (should (equal before (output-p2-test--projection)))))))

(ert-deftest output-prompt/pending-creation-reuse-and-consumption ()
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name))
          (superchat--queued-follow-up "QUEUED"))
      (superchat--response-region-begin "A")
      (superchat--insert-prompt)
      (let ((prompt superchat--prompt-start)
            (pending (cdr (assoc :pending superchat--response-region-states))))
        (should (eq prompt (plist-get pending :prompt-marker)))
        (should (equal "QUEUED" (buffer-substring prompt (point-max))))
        (superchat--response-region-ensure-view-prompt t)
        (should (eq prompt superchat--prompt-start))
        (superchat--response-region-begin "B")
        (should (eq pending (cdr (assoc :pending superchat--response-region-states))))
        (superchat--response-region-consume-prompt)
        (superchat--prepare-for-response)
        (superchat--response-region-begin "C")
        (should (eq pending (cdr (assoc "C" superchat--response-region-states))))
        (should-not (plist-get pending :prompt-marker))
        (should (string-match-p "QUEUED" (buffer-string)))))))

(ert-deftest output-prompt/stale-marker-is-not-live-or-consumed ()
  (dolist (detach '(nil t))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (insert "* User: HISTORICAL\n")
        (superchat--insert-prompt)
        (let* ((pending (cdr (assoc :pending superchat--response-region-states)))
               (stale superchat--prompt-start))
          (if detach (set-marker stale nil)
            (delete-region (line-beginning-position) (point-max)))
          (superchat--response-region-begin "A")
          (should (eq pending (cdr (assoc :pending superchat--response-region-states))))
          (should-not (superchat--response-region--view-prompt))
          (superchat--response-region-ensure-view-prompt)
          (should-not (eq stale superchat--prompt-start))
          (should (superchat--response-region--view-prompt))
          (should (eq superchat--prompt-start (plist-get pending :prompt-marker)))
          (should (string-match-p "HISTORICAL" (buffer-string))))))))

(ert-deftest output-prompt/header-validity-survives-fontification-not-replacement ()
  "The header tag is structural validation, never draft or owner identity."
  (dolist (text '("" "draft\nsecond line"))
    (with-temp-buffer
      (org-mode)
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--insert-prompt)
        (let* ((prompt superchat--prompt-start)
               (header-start (copy-marker (line-beginning-position) t)))
          (insert-and-inherit text)
          (should-not (text-property-not-all prompt (point-max)
                                            'superchat-prompt-header nil))
          (dotimes (_ 2)
            (font-lock-set-defaults)
            (font-lock-fontify-region (point-min) (point-max))
            (should (eq 'org-level-1 (get-text-property (1- prompt) 'face)))
            (should (eq prompt (superchat--response-region--view-prompt)))
            ;; Normal insertion at the input boundary must not extend the tag.
            (goto-char prompt)
            (insert-and-inherit "edit")
            (should-not (get-text-property prompt 'superchat-prompt-header)))
          (goto-char header-start)
          (insert-and-inherit "\n")
          (should-not (get-text-property (1- header-start) 'superchat-prompt-header))
          (goto-char header-start)
          (let ((inhibit-read-only t))
            (delete-region header-start prompt)
            ;; Visually identical replacement does not acquire structural validity.
            (insert-and-inherit "* User: "))
          (font-lock-fontify-region (point-min) (point-max))
          (should-not (superchat--response-region--view-prompt))
          (superchat--response-region-ensure-view-prompt)
          (should-not (eq prompt superchat--prompt-start))
          (should (superchat--response-region--view-prompt)))))))

(defun output-o3-test--snapshot ()
  "Capture values and identities without copying away mutable marker evidence."
  (list (buffer-string) (point)
        (mapcar (lambda (entry)
                  (list entry (cdr entry) (copy-sequence (cdr entry))
                        (cl-loop for value in (cdr entry)
                                 when (markerp value)
                                 collect (list value (marker-position value)
                                               (marker-buffer value)
                                               (marker-insertion-type value)))))
                superchat--response-region-states)
        (output-p2-test--projection)
        (mapcar (lambda (overlay)
                  (list overlay (overlay-start overlay) (overlay-end overlay)
                        (overlay-buffer overlay) (copy-sequence (overlay-properties overlay))))
                (append (car (overlay-lists)) (cdr (overlay-lists))))))

(ert-deftest output-o3/formatter-failure-precedes-all-state-mutation ()
  (dolist (fault '(error quit))
    (dolist (phase '(absent inactive active))
      (dolist (other '(nil t))
        (with-temp-buffer
          (let ((superchat-buffer-name (buffer-name)))
            (superchat--insert-prompt)
            (unless (eq phase 'absent)
              (superchat--response-region-begin "A")
              (when (eq phase 'active)
                (superchat--response-region-add-reasoning "THINKING" "A")
                (superchat--response-region-stream "PRIOR PART" "A"))
              (superchat--update-status "A STATUS" "A"))
            (when other
              (superchat--response-region-begin "B")
              (superchat--response-region-stream "B PART" "B"))
            (goto-char superchat--prompt-start)
            (insert (propertize "DRAFT" 'o3-draft t))
            (let ((before (output-o3-test--snapshot)) caught)
              (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                         (lambda (_text) (signal fault '("formatter fixture")))))
                (condition-case condition
                    (superchat--response-region-close "FINAL" t "A")
                  ((error quit) (setq caught condition))))
              (should (eq fault (car caught)))
              (should (equal-including-properties (car before) (buffer-string)))
              (should (equal before (output-o3-test--snapshot)))
              (cl-mapc
               (lambda (saved entry)
                 (should (eq (nth 0 saved) entry))
                 (should (eq (nth 1 saved) (cdr entry)))
                 (cl-loop for (key value) on (nth 2 saved) by #'cddr
                          when (or (consp value) (markerp value) (overlayp value))
                          do (should (eq value (plist-get (cdr entry) key)))))
               (nth 2 before) superchat--response-region-states))))))))

(ert-deftest output-o3/formats-meaningful-final-once-before-publication ()
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name)) (calls 0)
          (formatter (symbol-function 'superchat-render-markdown-to-org)))
      (superchat--insert-prompt)
      (let ((before (output-o3-test--snapshot)))
        (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                   (lambda (content)
                     (cl-incf calls)
                     (should (equal before (output-o3-test--snapshot)))
                     (funcall formatter content))))
          (superchat--response-region-close "ONCE" t "NEW")))
      (should (= 1 calls))
      (should (string-match-p "ONCE" (buffer-string))))))

(ert-deftest output-o3/closed-nil-round-does-not-format-or-open ()
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--response-region-begin "A")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                   (lambda (_) (ert-fail "nil closed round formatted"))))
          (dotimes (_ 2) (superchat--response-region-close nil nil "A")))
        (should (equal before (buffer-string)))
        (should-not (plist-get (cdr (assoc "A" superchat--response-region-states))
                               :assistant-start))))))

;;; test-native-ui.el ends here

(ert-deftest answer-heading/code-examples-and-relative-depth ()
  (let* ((input "# Title\n\n### Deep\n\n```text\n# literal markdown\n* literal org\n```\n\n#+begin_src text\n# source hash\n* source star\n#+end_src\n\n## Chapter\n")
         (generic (superchat-render-markdown-to-org input))
         (answer (superchat--format-assistant-answer input)))
    (should (string-prefix-p "* Title" generic))
    (should (string-prefix-p "*** Title" answer))
    (should (string-match-p "^\\*\\*\\*\\*\\* Deep$" answer))
    (should (string-match-p "^\\*\\*\\*\\* Chapter$" answer))
    (should (string-match-p (regexp-quote "# literal markdown\n* literal org")
                           (org-unescape-code-in-string answer)))
    (should (string-match-p (regexp-quote "#+begin_src text\n# source hash\n* source star\n#+end_src")
                           (org-unescape-code-in-string answer)))
    (should (equal generic (superchat-render-markdown-to-org input)))))

(ert-deftest answer-heading/late-custom-owner-preserves-next-owner ()
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--insert-prompt)
      (superchat--response-region-begin "custom-A")
      (superchat--response-region-stream "NARRATION" "custom-A")
      (superchat--response-region-close nil nil "custom-A")
      (superchat--response-region-begin "custom-B")
      (superchat--response-region-stream "B ANSWER" "custom-B")
      (let ((projection (output-p2-test--projection)))
        (superchat--response-region-close "# A TITLE\n## A CHILD\nA BODY" t "custom-A")
        (should (equal projection (output-p2-test--projection))))
      (let ((a (save-excursion (goto-char (point-min)) (search-forward "*** A TITLE") (line-beginning-position)))
            (b (save-excursion (goto-char (point-min)) (search-forward "B ANSWER") (match-beginning 0)))
            (prompt (superchat--response-region--view-prompt)))
        (goto-char a) (should (org-up-heading-safe))
        (should (looking-at "\\*\\* Assistant"))
        (org-fold-hide-subtree)
        (should (invisible-p a))
        (should-not (invisible-p b))
        (should-not (invisible-p prompt))))))

(ert-deftest answer-heading/source-example-stays-inside-assistant-fold ()
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--insert-prompt)
      (superchat--response-region-close "# Title\n```org\n* literal org\n```\n" t "custom")
      (goto-char (point-min)) (re-search-forward "^\\*\\* Assistant")
      (org-fold-hide-subtree)
      (search-forward "literal org")
      (should (invisible-p (match-beginning 0)))
      (should-not (invisible-p (1- (superchat--response-region--view-prompt))))
      (should-not (invisible-p (superchat--response-region--view-prompt))))))

(ert-deftest answer-heading/code-body-semantic-roundtrip ()
  (let ((body "* star\n,* comma-star\n,,* two-commas\n#+keyword\n,#+comma-keyword\n  * indented\n  ,* indented-comma\nordinary, comma\n\n"))
    (dolist (syntax '(markdown tilde org))
      (let* ((input (concat "# Title\n"
                            (pcase syntax ('markdown "```text\n") ('tilde "~~~text\n") (_ "#+begin_src text\n"))
                            body
                            (pcase syntax ('markdown "```\n") ('tilde "~~~\n") (_ "#+end_src\n"))))
             (answer (superchat--format-assistant-answer input)))
        (with-temp-buffer
          (org-mode) (insert answer)
          (goto-char (point-min)) (search-forward "#+begin_src text\n")
          (let ((start (point)))
            (re-search-forward "^#\\+end_src")
            (should (equal body (org-unescape-code-in-string
                                 (buffer-substring-no-properties start (match-beginning 0))))))
          (should (equal '("Title")
                         (org-element-map (org-element-parse-buffer) 'headline
                           (lambda (h) (org-element-property :raw-value h))))))))))

(ert-deftest answer-heading/raw-stream-is-formatted-at-round-close ()
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--insert-prompt)
      (superchat--response-region-begin "round")
      (superchat--response-region-stream "# Round title\n## Child\n" "round")
      (should (string-match-p "^# Round title$" (buffer-string)))
      (superchat--response-region-close nil nil "round")
      (goto-char (point-min)) (search-forward "*** Round title")
      (beginning-of-line) (should (org-up-heading-safe))
      (should (looking-at "\\*\\* Assistant")))))
