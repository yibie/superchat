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
(require 'journey-fixture
         (expand-file-name "journey-fixture" (file-name-directory load-file-name)))

(ert-deftest native-ui/tool-record-org-escape-roundtrip ()
  (require 'superchat-agent-loop)
  (dolist (kind '(call result))
    (dolist (text (list "ordinary" "* rogue\n#+end_example\n#+begin_src elisp\n,comma\n ** nested"
                       (concat "* rogue\n#+end_example\n" (make-string 2100 ?x))))
      (with-temp-buffer
        (org-mode)
        (let ((superchat-buffer-name (buffer-name)))
          (superchat--insert-prompt)
          (superchat--response-region-begin 'tools t)
          (let* ((args (list text))
                 ;; S21: both the call and the result example block open
                 ;; with a `Tool call: NAME'/`Tool result: NAME' line ahead
                 ;; of the payload -- see `superchat--agent-render-tool-
                 ;; call'/`-result' in superchat-agent-loop.el.
                 (formatted (if (eq kind 'call)
                                (concat "Tool call: fixture\n" (string-trim (format "%S" args)))
                              (let ((value (string-trim text)))
                                (concat "Tool result: fixture\n"
                                        (if (> (length value) 2000)
                                            (concat (substring value 0 2000) "\n...")
                                          value))))))
            (if (eq kind 'call)
                (superchat--agent-render-tool-call "fixture" args (current-buffer) nil 'tools)
              (superchat--response-region-close nil nil 'tools)
              (superchat--agent-render-tool-result "fixture" text (current-buffer) nil 'tools))
            (goto-char (point-min))
            (re-search-forward "^#\\+begin_example\n")
            (let ((start (point)))
              (re-search-forward "^#\\+end_example")
              (should (equal (concat formatted "\n")
                             (org-unescape-code-in-string
                              (buffer-substring-no-properties start (match-beginning 0))))))
            (goto-char (point-min))
            ;; S21: the collapsible heading is `*** [status] NAME', not a
            ;; literal `Tool call:'/`Tool result:' line.  S21-fix-1: assert
            ;; the exact state each checkpoint means -- `call' has just
            ;; rendered its own still-running heading; `result' (called here
            ;; with no preceding call, so no pending record to update) falls
            ;; back to a fresh, non-failed heading.
            (re-search-forward
             (format "^\\*\\*\\* %s fixture" (if (eq kind 'call) "⏳" "✓")))
            (beginning-of-line)
            (should (org-up-heading-safe))
            (should (looking-at "\\*\\* Assistant:"))
            (org-fold-hide-subtree)
            (should-not (invisible-p superchat--prompt-start))
            (org-fold-show-subtree)))))))

(ert-deftest native-ui/input-widget-insertion-keeps-following-owner ()
  (require 'wid-edit)
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--insert-prompt)
      (superchat--response-region-begin 'a t)
      (superchat--response-region-add-reasoning "Private reasoning" 'a)
      (superchat--response-region-close "A answer" t 'a)
      (superchat--response-region-begin 'b t)
      (let* ((a (cdr (assoc 'a superchat--response-region-states)))
             (b (cdr (assoc 'b superchat--response-region-states)))
             (start (plist-get b :assistant-start))
             (end (plist-get a :assistant-end))
             (old (marker-position start))
             (prompt superchat--prompt-start)
             widget)
        ;; Exercise the legal coincident closed-end/following-start layout.
        (set-marker end start)
        (should (= old end))
        (superchat--response-region-insert-transcript
         "*** Required input\nCity?\n" :current-run 'a
         (lambda () (setq widget (widget-create 'push-button :tag "Answer"
                                                :keymap widget-keymap :notify #'ignore))))
        (should (> start old))
        (should (<= (widget-get widget :to) start))
        (should-not (invisible-p (widget-get widget :from)))
        (should (< start prompt))
        (should (eq superchat--response-region-current-owner 'b))
        (superchat--response-region-close "B final" t 'b)
        (should (widget-at (widget-get widget :from)))
        (should (string-match-p "City?" (buffer-string)))
        (should (string-match-p "B final" (buffer-string)))))))

(ert-deftest native-ui/input-widget-insertion-error-quit-rolls-back ()
  (require 'wid-edit)
  (dolist (condition '(error quit))
    (with-temp-buffer
      (org-mode)
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--insert-prompt)
        (superchat--response-region-begin 'a t)
        (superchat--response-region-close "A" t 'a)
        (superchat--response-region-begin 'b t)
        (let* ((text (buffer-string))
               (states (copy-tree superchat--response-region-states))
               (markers (cl-loop for entry in states append
                                 (cl-loop for value in (cdr entry) when (markerp value)
                                          collect (list value (marker-position value)
                                                        (marker-insertion-type value)))))
               (overlays (overlays-in (point-min) (point-max)))
               (position (point)))
          (should (equal
                   (condition-case err
                       (superchat--response-region-insert-transcript
                        "*** Required input\n" :current-run 'a
                        (lambda ()
                          (widget-create 'push-button :tag "Half" :keymap widget-keymap)
                          (signal condition '("original insertion" detail))))
                     ((error quit) err))
                   (list condition "original insertion" 'detail)))
          (should (equal-including-properties text (buffer-string)))
          (should (equal states superchat--response-region-states))
          (dolist (entry markers)
            (should (= (nth 1 entry) (marker-position (car entry))))
            (should (eq (nth 2 entry) (marker-insertion-type (car entry)))))
          (should (equal overlays (overlays-in (point-min) (point-max))))
          (should (eq superchat--response-region-current-owner 'b))
          (should (= position (point))))))))

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
          (superchat-tape-import-event
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

(ert-deftest native-ui/send-region-pins-the-source-project-on-the-chat-view ()
  "A region from a project overrides an already-open chat View's tool-search root."
  (let* ((root (make-temp-file "superchat-region-project-" t))
         (source-path (expand-file-name "src/example.el" root))
         (other-root (file-name-as-directory (make-temp-file "superchat-region-other-" t)))
         (chat-name (generate-new-buffer-name " *superchat-region-chat*"))
         (superchat-buffer-name chat-name)
         (superchat-llm-backend nil)
         (old-default (default-value 'superchat-tools-default-directory))
         source chat)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory (file-name-directory source-path) t)
          (with-temp-file source-path
            (insert "(message \"region\")\n"))
          (setq-default superchat-tools-default-directory nil)
          (setq source (find-file-noselect source-path))
          (with-current-buffer source
            (setq-local superchat-tools-default-directory "/sentinel/source-root/"))
          ;; Simulate a chat View that is already open and Session-restored
          ;; with a different project root, the way a real View is: the
          ;; buffer exists, has a prompt, and its own buffer-local value.
          (superchat--insert-prompt)
          (setq chat (get-buffer chat-name))
          (with-current-buffer chat
            (setq-local superchat-tools-default-directory other-root))
          (save-window-excursion
            (with-current-buffer source
              (superchat-send-region (point-min) (point-max) "")))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (equal (file-name-as-directory root)
                           superchat-tools-default-directory)))
          ;; The source buffer's own value is never written -- only the
          ;; destination chat View is repinned.
          (with-current-buffer source
            (should (equal "/sentinel/source-root/"
                          superchat-tools-default-directory))))
      (setq-default superchat-tools-default-directory old-default)
      (dolist (buffer (list source chat))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'superchat--cancel-status-refresh-timer)
              (superchat--cancel-status-refresh-timer))
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (delete-directory root t)
      (when (file-directory-p other-root)
        (delete-directory other-root t)))))

(ert-deftest native-ui/send-defun-pins-the-source-project-on-the-chat-view ()
  "The enclosing defun from a project overrides an already-open chat View's
tool-search root."
  (let* ((root (make-temp-file "superchat-defun-project-" t))
         (source-path (expand-file-name "src/example.el" root))
         (other-root (file-name-as-directory (make-temp-file "superchat-defun-other-" t)))
         (chat-name (generate-new-buffer-name " *superchat-defun-chat*"))
         (superchat-buffer-name chat-name)
         (superchat-llm-backend nil)
         (old-default (default-value 'superchat-tools-default-directory))
         source chat)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory (file-name-directory source-path) t)
          (with-temp-file source-path
            (insert "(defun example-fn ()\n  (message \"defun\"))\n"))
          (setq-default superchat-tools-default-directory nil)
          (setq source (find-file-noselect source-path))
          (with-current-buffer source
            (setq-local superchat-tools-default-directory "/sentinel/source-root/"))
          ;; Simulate a chat View that is already open and Session-restored
          ;; with a different project root, the way a real View is: the
          ;; buffer exists, has a prompt, and its own buffer-local value.
          (superchat--insert-prompt)
          (setq chat (get-buffer chat-name))
          (with-current-buffer chat
            (setq-local superchat-tools-default-directory other-root))
          (save-window-excursion
            (with-current-buffer source
              (goto-char (point-min))
              (search-forward "message")
              (superchat-send-defun "")))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (equal (file-name-as-directory root)
                           superchat-tools-default-directory)))
          ;; The source buffer's own value is never written -- only the
          ;; destination chat View is repinned.
          (with-current-buffer source
            (should (equal "/sentinel/source-root/"
                          superchat-tools-default-directory))))
      (setq-default superchat-tools-default-directory old-default)
      (dolist (buffer (list source chat))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'superchat--cancel-status-refresh-timer)
              (superchat--cancel-status-refresh-timer))
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (delete-directory root t)
      (when (file-directory-p other-root)
        (delete-directory other-root t)))))

(ert-deftest native-ui/send-region-without-file-omits-dot-file-header ()
  "An unvisited source buffer never renders the misleading `;; file: .'."
  (let* ((root (make-temp-file "superchat-region-no-file-" t))
         (chat-name (generate-new-buffer-name " *superchat-no-file-chat*"))
         (superchat-buffer-name chat-name)
         (superchat-llm-backend nil)
         (old-default (default-value 'superchat-tools-default-directory))
         (source (generate-new-buffer " *superchat-no-file-source*"))
         chat)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (setq-default superchat-tools-default-directory nil)
          (save-window-excursion
            (with-current-buffer source
              (setq-local default-directory (file-name-as-directory root))
              (emacs-lisp-mode)
              (insert "(message \"unsaved\")\n")
              (superchat-send-region (point-min) (point-max) ""))
            (setq chat (get-buffer chat-name)))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should-not (string-match-p ";; file: \\." (buffer-string)))))
      (setq-default superchat-tools-default-directory old-default)
      (dolist (buffer (list source chat))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'superchat--cancel-status-refresh-timer)
              (superchat--cancel-status-refresh-timer))
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

;; ═══════════════════════════════════════════════════════════
;; Embark key naming (no collision with existing commands)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/embark-key-not-bound-by-default ()
  "Embark keys 's' and 'S' are not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map "s"))
  (should-not (lookup-key superchat-mode-map "S")))

(ert-deftest native-ui/embark-load-binds-superchat-actions-to-its-keymaps ()
  "The real Embark load hook populates its file and region maps."
  (let* ((emacs (or (executable-find invocation-name)
                    (executable-find "emacs")))
         (root (file-name-directory (locate-library "superchat")))
         (home (make-temp-file "superchat-embark-home-" t))
         (data (expand-file-name "superchat-data" home))
         (stub-directory (make-temp-file "superchat-embark-stub-" t))
         (stub (expand-file-name "embark.el" stub-directory))
         (output (generate-new-buffer " *superchat-embark-load*")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file stub
            (insert "(defvar embark-file-map (make-sparse-keymap))\n"
                    "(defvar embark-region-map (make-sparse-keymap))\n"
                    "(provide 'embark)\n"))
          (let ((process-environment
                 (cons (concat "HOME=" home) process-environment)))
            (let ((status
                   (call-process
                    emacs nil output nil
                    "-Q" "--batch" "-L" root "-L" stub-directory
                    "--eval"
                    (prin1-to-string
                     `(progn
                        (setq native-comp-enable-subr-trampolines nil
                              superchat-data-directory ,data)
                        (require 'superchat)
                        (require 'embark)
                        (unless (eq (lookup-key embark-file-map (kbd "s"))
                                    'superchat-embark-file-send)
                          (error "Embark file action was not installed"))
                        (unless (eq (lookup-key embark-region-map (kbd "S"))
                                    'superchat-send-region-or-defun)
                          (error "Embark region action was not installed")))))))
              (unless (= 0 status)
                (ert-fail (format "Embark child exited %s:\n%s"
                                  status
                                  (with-current-buffer output (buffer-string))))))))
      (when (buffer-live-p output) (kill-buffer output))
      (delete-directory home t)
      (delete-directory stub-directory t))))

(ert-deftest native-ui/dired-key-not-bound-by-default-in-chat ()
  "dired C-c s is not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map (kbd "C-c s"))))

(ert-deftest native-ui/embark-file-send-localizes-to-the-destination-view ()
  "S06b-fix-3 finding 1: Embark sends a file from a plain, non-Superchat
source buffer.  The fix opens the destination View first — the same
ordering `superchat-dired-send' already uses — so the attachment lands
on that View's own buffer-local list, never on
`superchat--current-context-files''s global default nor on an unrelated
second View that is already open."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-embark-leak-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (file (expand-file-name "embark-leak.txt" directory))
         (token "EMBARK_LEAK_TOKEN_9")
         (chat-name (generate-new-buffer-name " *superchat-embark-chat*"))
         (other-name (generate-new-buffer-name " *superchat-embark-other*"))
         (old-default (default-value 'superchat--current-context-files))
         (source (generate-new-buffer " *superchat-embark-source*"))
         chat other)
    (unwind-protect
        (native-ui-test--with-isolated-tabs
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (with-temp-file file (insert (format "%s\n" token)))
          (setq-default superchat--current-context-files nil)
          ;; A second, unrelated View, already open before Embark runs.
          ;; Built directly through `superchat--ensure-chat-view' (an
          ;; explicit distinct session id) rather than `(superchat)', which
          ;; would tag the one disposable test tab with this View's session
          ;; and route the Embark action's own `(superchat)' call right back
          ;; to this same buffer instead of opening a new one.
          (setq other (superchat--ensure-chat-view other-name "embark-other-session"))
          ;; The Embark action itself runs from a plain, non-Superchat buffer.
          (with-current-buffer source
            (fundamental-mode)
            (let ((superchat-buffer-name chat-name))
              (superchat-embark-file-send file)))
          (setq chat (get-buffer chat-name))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (local-variable-p 'superchat--current-context-files))
            (should (equal (list file) superchat--current-context-files)))
          ;; The global default is untouched...
          (should (equal old-default (default-value 'superchat--current-context-files)))
          ;; ...and so is the unrelated second View: it never sees the file,
          ;; so a prompt built there contains none of its text.
          (with-current-buffer other
            (should-not (member file superchat--current-context-files))
            (should-not
             (cl-some (lambda (f) (string-match-p token (or (plist-get f :text) "")))
                      (superchat-prompt-collector--context-files
                       (list :context nil) nil nil)))))
      (dolist (buffer (list source chat other))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (fboundp 'superchat--cancel-status-refresh-timer)
              (superchat--cancel-status-refresh-timer))
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer)))
      (setq-default superchat--current-context-files old-default)
      (ignore-errors (superchat-db-close))
      (delete-directory directory t))))

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
    ;; S10: chat submission now translates PLAN into an envelope and back
    ;; into a fresh legacy Request built by `superchat-pipeline-submit' --
    ;; CAPTURED is no longer PLAN itself, but its `:prompt' is still exactly
    ;; the inspected/edited value, unchanged by that translation.
    (should (equal (plist-get captured :prompt) "edited exact prompt"))
    (should-not (plist-member plan :turn))
    (should (equal (plist-get plan :prompt) "edited exact prompt"))
    (should (equal (plist-get (plist-get plan :run-handle) :run-id)
                   "run-1"))))

(ert-deftest native-ui/request-plan-targets-its-originating-view ()
  "Dispatch submits the View captured by the request plan."
  (let* ((target-name (generate-new-buffer-name " *superchat-view-target*"))
         (superchat-buffer-name " *superchat-default-target*")
         target-buffer captured)
    (unwind-protect
        (with-current-buffer (setq target-buffer (get-buffer-create target-name))
          (let ((plan (list :type :llm-query
                            :prompt "hello"
                            :session-id "session-1"
                            :buffer-name target-name)))
            (cl-letf (((symbol-function 'superchat-runtime-submit)
                       (lambda (request)
                         (setq captured request)
                         '(:run-id "run-1" :status accepted))))
              (superchat--dispatch-result plan "English" nil))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))
    ;; S10 (contract §2.3): `:buffer-name' "is never a routing key" for an
    ;; envelope producer and is no longer carried on the legacy Request the
    ;; pipeline builds; the exact buffer OBJECT is the real identity now.
    (should (eq (plist-get captured :buffer) target-buffer))))

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
                  ;; S21: the tool round and the final close share the one
                  ;; heading this whole multi-round turn has had all along.
                  (should (= 1 (how-many "^\\*\\* Assistant" (point-min) (point-max))))
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
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--response-region-begin "A")
      (superchat--insert-prompt)
      (let ((prompt superchat--prompt-start)
            (pending (cdr (assoc :pending superchat--response-region-states))))
        (should (eq prompt (plist-get pending :prompt-marker)))
        (superchat--response-region-ensure-view-prompt t)
        (should (eq prompt superchat--prompt-start))
        (superchat--response-region-begin "B")
        (should (eq pending (cdr (assoc :pending superchat--response-region-states))))
        (superchat--response-region-consume-prompt)
        (superchat--prepare-for-response)
        (superchat--response-region-begin "C")
        (should (eq pending (cdr (assoc "C" superchat--response-region-states))))
        (should-not (plist-get pending :prompt-marker))))))

(ert-deftest output-prompt/stale-marker-is-not-live-or-consumed ()
  (dolist (detach '(nil t))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (insert "* User: HISTORICAL\n")
        (superchat--insert-prompt)
        (let* ((pending (cdr (assoc :pending superchat--response-region-states)))
               (stale superchat--prompt-start))
          (if detach (set-marker stale nil)
            ;; Deliberate corruption bypasses normal header protection.
            (let ((inhibit-read-only t))
              (delete-region (line-beginning-position) (point-max))))
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
          ;; This structural insertion is an intentional corruption probe.
          (let ((inhibit-read-only t)) (insert-and-inherit "\n"))
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

(ert-deftest chat-header/user-boundary-protects-normal-editing ()
  (dolist (fontify '(nil t))
    (with-temp-buffer
      (org-mode)
      (let ((superchat-buffer-name (buffer-name))
            (kill-ring nil) (kill-read-only-ok nil))
        (superchat--insert-prompt)
        (let* ((prompt superchat--prompt-start)
               (start (- prompt (length "* User: "))))
          (should (equal "* User: " (buffer-substring-no-properties start prompt)))
          (when fontify
            (font-lock-set-defaults)
            (font-lock-fontify-region (point-min) (point-max))
            (should (eq 'org-level-1 (get-text-property start 'face))))
          (goto-char prompt)
          (should-error (backward-delete-char 1) :type 'text-read-only)
          (should-error (delete-region start prompt) :type 'text-read-only)
          (goto-char start)
          (should-error (kill-line) :type 'text-read-only)
          (goto-char start)
          (should-error (insert "X") :type 'text-read-only)
          (goto-char (+ start 3))
          (should-error (insert "X") :type 'text-read-only)
          (kill-ring-save start prompt)
          (should (equal "* User: " (substring-no-properties (car kill-ring))))
          (goto-char prompt)
          (insert-and-inherit "abc")
          (goto-char prompt) (insert-and-inherit "FIRST")
          (forward-char 1) (insert-and-inherit "MID")
          (goto-char (point-max)) (insert-and-inherit "END")
          (backward-delete-char 1)
          (should-not (text-property-not-all prompt (point-max) 'read-only nil))
          (should-not (text-property-not-all prompt (point-max) 'superchat-prompt-header nil))
          (should-error (delete-region (1- prompt) (point-max)) :type 'text-read-only))))))

(ert-deftest chat-header/assistant-all-render-phases ()
  (dolist (phase '(stream final reasoning nonpartial))
    (with-temp-buffer
      (org-mode)
      (let ((superchat-buffer-name (buffer-name)))
        (superchat--insert-prompt)
        (superchat--response-region-begin "header-owner")
        (pcase phase
          ('stream (superchat--response-region-stream "BODY" "header-owner"))
          ('reasoning
           (superchat--response-region-add-reasoning "THOUGHT" "header-owner")
           (superchat--response-region-close "BODY" t "header-owner"))
          ('nonpartial
           (let ((superchat--nonpartial-e2e-elapsed 2.0))
             (superchat--response-region-close "BODY" t "header-owner")))
          (_ (superchat--response-region-close "BODY" t "header-owner")))
        (goto-char (point-min))
        (should (re-search-forward "^\\*\\* Assistant:" nil t))
        (let ((start (match-beginning 0)) (end (match-end 0)))
          (font-lock-set-defaults)
          (font-lock-fontify-region (point-min) (point-max))
          (should (eq 'org-level-2 (get-text-property start 'face)))
          (should-error (delete-region start end) :type 'text-read-only)
          (goto-char start) (should-error (kill-line) :type 'text-read-only)
          (goto-char (point-min)) (search-forward "BODY")
          (should-not (get-text-property (1- (point)) 'read-only))
          (insert "EDIT")
          (goto-char start) (org-fold-hide-subtree)
          (goto-char (point-min)) (search-forward "BODY")
          (should (invisible-p (1- (point))))
          (goto-char superchat--prompt-start)
          (insert "DRAFT")
          (should-not (get-text-property (1- (point)) 'read-only)))))))

(ert-deftest chat-header/preset-mode-labels-remain-protected ()
  (dolist (case '((nil nil "* User: ")
                  ("review" nil "* User [review mode]: ")
                  (nil t "* User [chat:Profile]: ")
                  ("review" t "* User [review mode | chat:Profile]: ")))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name))
            (superchat--current-command (nth 0 case))
            (superchat--active-preset (and (nth 1 case)
                                           (superchat-preset--create :type 'chat :name "Profile"))))
        (superchat--insert-prompt)
        (let ((start (line-beginning-position)) (end (point)))
          (should (equal (nth 2 case) (buffer-substring-no-properties start end)))
          (should-not (text-property-not-all start end 'read-only t))
          (insert "editable")
          (should-not (get-text-property end 'read-only)))))))

(ert-deftest chat-header/ttft-and-model-lookalike-are-distinct ()
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name))
          (superchat--ttft-elapsed 0.5))
      (superchat--insert-prompt)
      (superchat--response-region-begin "timed")
      (superchat--response-region-close "Answer\n** Assistant: MODEL TEXT" t "timed")
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\* Assistant:.*TTFT" nil t))
      (let ((start (line-beginning-position)) (end (line-end-position)))
        (should-not (text-property-not-all start end 'read-only t))
        (should-error (delete-region start (1+ end)) :type 'text-read-only))
      (search-forward "MODEL TEXT")
      (should-not (get-text-property (1- (point)) 'read-only)))))

(ert-deftest chat-header/replay-preserves-tape-and-editable-input ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "header-replay-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (session "header-replay") buffer before)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "tape.db" directory))
          (superchat-tape-append-material session "user" "RAW QUESTION")
          (superchat-tape-append-material session "assistant" "RAW ANSWER")
          (setq before (superchat-db-tape-replay session))
          (setq buffer (superchat--render-session-view session))
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "* User: RAW QUESTION" nil t))
            (should (search-forward "** Assistant:\nRAW ANSWER" nil t))
            (font-lock-set-defaults)
            (font-lock-fontify-region (point-min) (point-max))
            (goto-char (point-min)) (search-forward "** Assistant:")
            (should-error (backward-delete-char 1) :type 'text-read-only)
            (goto-char superchat--prompt-start)
            (should-error (backward-delete-char 1) :type 'text-read-only)
            (insert-and-inherit "DRAFT")
            (backward-delete-char 1)
            (should (equal "DRAF" (buffer-substring-no-properties superchat--prompt-start (point-max))))
            (should-not (get-text-property (1- (point-max)) 'read-only))
            (should (equal "RAW ANSWER" (plist-get (car superchat--conversation-history) :content))))
          (should (equal before (superchat-db-tape-replay session))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (superchat-db-close)
      (delete-directory directory t))))

;; ═══════════════════════════════════════════════════════════
;; S24: fresh-View onboarding and recent-sessions (C1)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/fresh-view-lists-recent-sessions-and-keeps-prompt-last ()
  "A brand-new View on a Session with prior Tape history lists it, prompt last."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-fresh-view-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-llm-backend '(mock-backend))
         (fresh-session "s24-fresh-lists-prior")
         (prior-session "s24-fresh-lists-prior-old")
         buffer)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (superchat-tape-append-material prior-session "user"
                                          "S24_PRIOR_SESSION_QUESTION")
          (setq buffer
                (superchat--ensure-chat-view
                 (generate-new-buffer-name " *s24-fresh-view*") fresh-session))
          (with-current-buffer buffer
            (should (string-match-p "^#\\+TITLE: superchat" (buffer-string)))
            (should (string-match-p "\\* Recent sessions" (buffer-string)))
            (should (string-match-p (regexp-quote prior-session) (buffer-string)))
            ;; Never onboards when a backend is configured.
            (should-not (string-match-p "is not configured" (buffer-string)))
            (should (markerp superchat--prompt-start))
            (should (marker-position superchat--prompt-start))
            (should (string-match-p "\\* User: " (buffer-string)))
            ;; The prompt line is the last thing in the buffer: nothing but
            ;; the (empty) input area follows its marker.
            (should (string-match-p
                     "\\`\\'"
                     (buffer-substring-no-properties
                      superchat--prompt-start (point-max))))
            (goto-char (point-min))
            (should (< (search-forward "* Recent sessions")
                       (marker-position superchat--prompt-start)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (native-ui-test--discard-workspace fresh-session)
      (native-ui-test--discard-workspace prior-session)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/no-backend-shows-onboarding-block-once ()
  "The onboarding block appears only when unconfigured, and never duplicates."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-onboarding-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (unconfigured-session "s24-onboarding-nil-backend")
         (configured-session "s24-onboarding-with-backend")
         unconfigured-buffer configured-buffer)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (let ((superchat-llm-backend nil))
            (setq unconfigured-buffer
                  (superchat--ensure-chat-view
                   (generate-new-buffer-name " *s24-onboarding-nil*")
                   unconfigured-session))
            (with-current-buffer unconfigured-buffer
              (should (string-match-p "is not configured" (buffer-string)))
              (goto-char (point-min))
              (search-forward "is not configured")
              (should (get-text-property (1- (point)) 'read-only)))
            ;; Re-opening the same (now non-fresh) View does not insert a
            ;; second copy of the block.
            (superchat--ensure-chat-view
             (buffer-name unconfigured-buffer) unconfigured-session)
            (with-current-buffer unconfigured-buffer
              (goto-char (point-min))
              (should (= 1 (how-many "is not configured")))))
          (let ((superchat-llm-backend '(mock-backend)))
            (setq configured-buffer
                  (superchat--ensure-chat-view
                   (generate-new-buffer-name " *s24-onboarding-configured*")
                   configured-session))
            (with-current-buffer configured-buffer
              (should-not (string-match-p "is not configured" (buffer-string))))))
      (when (buffer-live-p unconfigured-buffer) (kill-buffer unconfigured-buffer))
      (when (buffer-live-p configured-buffer) (kill-buffer configured-buffer))
      (native-ui-test--discard-workspace unconfigured-session)
      (native-ui-test--discard-workspace configured-session)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/setup-guidance-matches-between-onboarding-and-backend-show ()
  "Onboarding and an unconfigured `/backend' show the exact same setup
guidance, and its LM Studio form agrees with README's key and URL (the
chat model is an intentional placeholder, not a claimed exact copy)."
  (let* ((superchat-llm-backend nil)
         (examples (superchat--llm-backend-setup-examples)))
    (should (string-match-p ":key \"lm-studio\"" examples))
    (should (string-match-p "http://localhost:1234/v1/" examples))
    (should (string-match-p (regexp-quote examples) (superchat-backend-show)))
    (with-temp-buffer
      (org-mode)
      (superchat--insert-onboarding)
      (should (string-match-p (regexp-quote examples) (buffer-string))))))

;; ═══════════════════════════════════════════════════════════
;; S24-fix-1: fresh-View presentation on /clear and on-demand region/defun;
;; recent-Sessions read-only
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/clear-presents-fresh-view-like-a-new-session ()
  "`/clear' inserts the same presentation as a brand-new View: title, then
onboarding (nil backend) or nothing, then recent Sessions, then the prompt
last."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (dolist (backend-configured '(nil t))
    (let* ((directory (make-temp-file "superchat-clear-fresh-" t))
           (superchat-data-directory directory)
           (superchat-db--connection nil) (superchat-db--path nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (superchat-editor-workspace--states (make-hash-table :test #'equal))
           (superchat-llm-backend (and backend-configured '(mock-backend)))
           (prior-session (format "s24-clear-fresh-prior-%s" backend-configured))
           (old-session (format "s24-clear-fresh-old-%s" backend-configured))
           (buffer-name (generate-new-buffer-name " *s24-clear-fresh*"))
           view)
      (unwind-protect
          (progn
            (superchat-db-open (expand-file-name "superchat.db" directory))
            (superchat-tape-append-material prior-session "user" "S24_CLEAR_PRIOR_QUESTION")
            (setq view (superchat--ensure-chat-view buffer-name old-session))
            (with-current-buffer view
              (superchat--clear-chat-and-context)
              (should (string-match-p "^#\\+TITLE: superchat" (buffer-string)))
              (if backend-configured
                  (should-not (string-match-p "is not configured" (buffer-string)))
                (should (string-match-p "is not configured" (buffer-string))))
              (should (string-match-p "\\* Recent sessions" (buffer-string)))
              (should (string-match-p (regexp-quote prior-session) (buffer-string)))
              (should (markerp superchat--prompt-start))
              (should (string-match-p
                       "\\`\\'"
                       (buffer-substring-no-properties superchat--prompt-start (point-max))))
              (goto-char (point-min))
              (let ((title-at (search-forward "#+TITLE")) onboarding-at recent-at)
                (goto-char (point-min))
                (unless backend-configured
                  (setq onboarding-at (search-forward "is not configured")))
                (goto-char (point-min))
                (setq recent-at (search-forward "* Recent sessions"))
                (should (< title-at recent-at))
                (when onboarding-at (should (< onboarding-at recent-at)))
                (should (< recent-at (marker-position superchat--prompt-start))))))
        (when (buffer-live-p view) (kill-buffer view))
        (native-ui-test--discard-workspace prior-session)
        (native-ui-test--discard-workspace old-session)
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest native-ui/clear-rollback-leaves-old-view-intact ()
  "A failed clear transaction leaves the old View's text, Session identity,
and EditorWorkspace ownership exactly as they were."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-clear-rollback-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-llm-backend '(mock-backend))
         (old-session "s24-clear-rollback-old")
         (buffer-name (generate-new-buffer-name " *s24-clear-rollback*"))
         view before)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (setq view (superchat--ensure-chat-view buffer-name old-session))
          (with-current-buffer view
            (setq before (buffer-string))
            (cl-letf (((symbol-function 'superchat--insert-prompt)
                       (lambda (&rest _) (error "S24-FIX-1 ROLLBACK FAULT"))))
              (should-error (superchat--clear-chat-and-context)))
            (should (equal before (buffer-string)))
            (should (equal old-session superchat--session-id))
            (should (equal old-session (superchat-editor-workspace-owner-session view)))
            (should (eq 'open (superchat-editor-workspace-state old-session)))))
      (when (buffer-live-p view) (kill-buffer view))
      (native-ui-test--discard-workspace old-session)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/clear-rollback-restores-exact-welcome-button-identity ()
  "A clear failure after presentation deletes every recent-Sessions button
the failed attempt created and leaves the pre-existing button -- the exact
same overlay object, at the exact same position -- untouched.
`atomic-change-group' undoes the buffer *text* a failed clear inserted, but
never touches overlay lifecycle on its own: without the rollback also
tracking welcome-button overlays, a failed retry that ran
`superchat--insert-welcome-recent-sessions' again would leave a second,
freshly created button overlay behind, or resurrect the original one with
stale bindings.  Fails on the pre-S24-fix-2 source (`jj file show -r @
--at-op 4d94a6766576 --ignore-working-copy superchat.el`), where
`superchat--clear-view-transaction' captures no welcome-widget resources at
all."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-clear-rollback-widgets-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-llm-backend '(mock-backend))
         (prior-session "s24-clear-rollback-widgets-prior")
         (old-session "s24-clear-rollback-widgets-old")
         (buffer-name (generate-new-buffer-name " *s24-clear-rollback-widgets*"))
         view before before-overlays before-button)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (superchat-tape-append-material prior-session "user" "S24_ROLLBACK_WIDGET_PRIOR")
          (setq view (superchat--ensure-chat-view buffer-name old-session))
          (with-current-buffer view
            (goto-char (point-min))
            (should (search-forward (regexp-quote prior-session) nil t))
            (setq before-button (widget-at (match-beginning 0)))
            (should before-button)
            (setq before (buffer-string)
                  before-overlays (overlays-in (point-min) (point-max)))
            ;; After the old View was rendered, give the *old* Session (the
            ;; one about to be cleared) a Tape row of its own -- it now
            ;; outranks `prior-session' by recency, so any welcome block a
            ;; failed retry rebuilds lists a different row set (old-session
            ;; first, two rows) rather than happening to recreate the same
            ;; single row, matching the reviewer's own adversarial probe.
            (superchat-tape-append-material old-session "user" "S24_ROLLBACK_WIDGET_OLD")
            (cl-letf (((symbol-function 'superchat--insert-prompt)
                       (lambda (&rest _) (error "S24-FIX-2 ROLLBACK WIDGET FAULT"))))
              (should-error (superchat--clear-chat-and-context)))
            (should (equal before (buffer-string)))
            (should (equal old-session superchat--session-id))
            (should (equal old-session (superchat-editor-workspace-owner-session view)))
            (should-not
             (cl-set-exclusive-or before-overlays (overlays-in (point-min) (point-max))
                                  :test #'eq))
            (goto-char (point-min))
            (search-forward (regexp-quote prior-session))
            (should (eq before-button (widget-at (match-beginning 0))))))
      (when (buffer-live-p view) (kill-buffer view))
      (native-ui-test--discard-workspace prior-session)
      (native-ui-test--discard-workspace old-session)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/on-demand-region-and-defun-route-through-fresh-view ()
  "`superchat-send-region'/`-defun' build a missing chat buffer through the
shared fresh-View initializer (title, then onboarding/recent Sessions,
then the prompt with the context); once sent, only that context and
instruction reach the model prompt and the durable Tape Entry -- none of
the fresh-View presentation text.  With no backend configured, nothing
reaches the provider transport at all.
This test is not about memory/recall, but the `prior-session' row it
seeds below (to prove Session-onboarding text and identifiers do not
leak) shares enough words with the sent instruction that, once
`superchat-memory-recall-scope' defaults to `all', S23's auto-recall
could attach it to the fresh Session as a coincidental \"memory\" and
fail this test for an unrelated reason; auto-recall is disabled here so
this test only exercises what it names."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (require 'llm nil t) (require 'llm-ollama nil t)))
  (dolist (sender '(region defun))
    (let* ((directory (make-temp-file "superchat-on-demand-" t))
           (superchat-data-directory directory)
           (superchat-db--connection nil) (superchat-db--path nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (superchat-editor-workspace--states (make-hash-table :test #'equal))
           (superchat-llm-backend (make-llm-ollama :chat-model "s24-on-demand"))
           (superchat-response-timeout nil)
           (superchat-memory-auto-recall-min-length 999999)
           (prior-session (format "s24-on-demand-prior-%s" sender))
           (superchat-buffer-name (generate-new-buffer-name " *s24-on-demand-target*"))
           (source (generate-new-buffer (format " *s24-on-demand-source-%s*" sender)))
           respond captured-prompt target)
      (unwind-protect
          (cl-letf (((symbol-function 'llm-chat-async)
                     (lambda (_provider prompt done _err &optional _multi)
                       (setq respond done captured-prompt prompt) nil))
                    ((symbol-function 'llm-chat-streaming)
                     (lambda (_provider prompt _partial done _err &optional _multi)
                       (setq respond done captured-prompt prompt) nil)))
            (superchat-db-open (expand-file-name "superchat.db" directory))
            (superchat-tape-append-material prior-session "user" "S24_ON_DEMAND_PRIOR")
            (with-current-buffer source
              (emacs-lisp-mode)
              (insert "(defun s24-on-demand-fn ()\n  \"S24_ON_DEMAND_TOKEN\")\n")
              (pcase sender
                ('region
                 (superchat-send-region (point-min) (point-max) "S24_ON_DEMAND_INSTRUCTION"))
                ('defun
                 (goto-char (point-min))
                 (search-forward "defun")
                 (superchat-send-defun "S24_ON_DEMAND_INSTRUCTION"))))
            (setq target (get-buffer superchat-buffer-name))
            (should (buffer-live-p target))
            (with-current-buffer target
              (should (string-match-p "^#\\+TITLE: superchat" (buffer-string)))
              (should-not (string-match-p "is not configured" (buffer-string)))
              (should (string-match-p "\\* Recent sessions" (buffer-string)))
              (should (string-match-p (regexp-quote prior-session) (buffer-string))))
            (should (functionp respond))
            (should (llm-chat-prompt-p captured-prompt))
            ;; The real, provider-visible content -- every user-role
            ;; interaction's text, read through llm.el's own accessors
            ;; rather than an ad-hoc scrape of a discarded argument.
            (let ((model-visible
                   (mapconcat
                    #'llm-chat-prompt-interaction-content
                    (seq-filter
                     (lambda (interaction)
                       (eq (llm-chat-prompt-interaction-role interaction) 'user))
                     (llm-chat-prompt-interactions captured-prompt))
                    "\n")))
              (should (string-match-p "S24_ON_DEMAND_TOKEN" model-visible))
              (should (string-match-p "S24_ON_DEMAND_INSTRUCTION" model-visible))
              (dolist (sentinel (list "#+TITLE: superchat" "* Recent sessions"
                                      "RET on a session" prior-session
                                      "S24_ON_DEMAND_PRIOR" "is not configured"))
                (should-not (string-match-p (regexp-quote sentinel) model-visible))))
            (funcall respond "S24 ON DEMAND ANSWER")
            (let* ((rows (superchat-db-tape-replay
                          (buffer-local-value 'superchat--session-id target)))
                   (sent-row (cl-find "user" rows :key (lambda (row) (nth 2 row))
                                      :test #'equal)))
              (should sent-row)
              (should (string-match-p "S24_ON_DEMAND_TOKEN" (nth 3 sent-row)))
              (should (string-match-p "S24_ON_DEMAND_INSTRUCTION" (nth 3 sent-row)))
              (dolist (sentinel (list "#+TITLE: superchat" "* Recent sessions"
                                      "RET on a session" prior-session
                                      "S24_ON_DEMAND_PRIOR" "is not configured"))
                (should-not (string-match-p (regexp-quote sentinel) (nth 3 sent-row))))))
        (when (buffer-live-p source) (kill-buffer source))
        (when (buffer-live-p target)
          (native-ui-test--discard-workspace (buffer-local-value 'superchat--session-id target)))
        (when (buffer-live-p target) (kill-buffer target))
        (native-ui-test--discard-workspace prior-session)
        (superchat-db-close)
        (delete-directory directory t))))
  ;; No backend configured: the fresh View is still built (onboarding
  ;; instead of a configured send) but the provider transport is never
  ;; reached -- the context/instruction stay unsent in the editable prompt.
  (let* ((directory (make-temp-file "superchat-on-demand-nil-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-llm-backend nil)
         (superchat-buffer-name (generate-new-buffer-name " *s24-on-demand-nil-target*"))
         (source (generate-new-buffer " *s24-on-demand-nil-source*"))
         called target)
    (unwind-protect
        (cl-letf (((symbol-function 'llm-chat-async)
                   (lambda (&rest _args) (setq called t) nil))
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (&rest _args) (setq called t) nil)))
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (with-current-buffer source
            (emacs-lisp-mode)
            (insert "(defun s24-on-demand-nil-fn ()\n  \"S24_ON_DEMAND_NIL_TOKEN\")\n")
            (superchat-send-region (point-min) (point-max) "S24_ON_DEMAND_NIL_INSTRUCTION"))
          (setq target (get-buffer superchat-buffer-name))
          (should (buffer-live-p target))
          (with-current-buffer target
            (should (string-match-p "is not configured" (buffer-string)))
            (should (string-match-p "S24_ON_DEMAND_NIL_TOKEN" (buffer-string)))
            (should (string-match-p "S24_ON_DEMAND_NIL_INSTRUCTION" (buffer-string))))
          (should-not called))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p target) (kill-buffer target))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/recent-sessions-list-is-read-only-but-buttons-work ()
  "The whole recent-Sessions block is read-only; its buttons still open a
Session, and the prompt after it stays editable."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-recent-readonly-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-llm-backend '(mock-backend))
         (prior-session "s24-recent-readonly-prior")
         (fresh-session "s24-recent-readonly-fresh")
         buffer opened)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (superchat-tape-append-material prior-session "user" "S24_RECENT_READONLY_QUESTION")
          (setq buffer (superchat--ensure-chat-view
                       (generate-new-buffer-name " *s24-recent-readonly*") fresh-session))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "* Recent sessions")
            (goto-char (match-beginning 0))
            (should-error (insert "X") :type 'text-read-only)
            (search-forward "RET on a session")
            (goto-char (match-beginning 0))
            (should-error (insert "X") :type 'text-read-only)
            (search-forward (regexp-quote prior-session))
            (goto-char (match-beginning 0))
            (should-error (insert "X") :type 'text-read-only)
            (end-of-line)
            (should-error (insert "X") :type 'text-read-only)
            (let* ((pos (save-excursion (goto-char (point-min))
                                        (search-forward prior-session)
                                        (1- (point))))
                   (button (widget-at pos)))
              (should button)
              (cl-letf (((symbol-function 'superchat-open-session)
                         (lambda (session-id) (setq opened session-id))))
                (widget-apply button :notify button))
              (should (equal prior-session opened)))
            (goto-char superchat--prompt-start)
            (should-not (get-text-property (point) 'read-only))
            (insert "still editable")
            (should (string-match-p "still editable" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (native-ui-test--discard-workspace prior-session)
      (native-ui-test--discard-workspace fresh-session)
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest native-ui/response-mode-indicator-only-in-chat-view ()
  "The response-mode indicator writes only into a live `superchat-mode' View.
`superchat-buffer-name' can name a foreign buffer while the Runtime request
it identifies runs (for example the rewrite command's source buffer); the
indicator must leave that buffer untouched and must not conjure one into
existence when none exists yet."
  (let ((superchat-show-response-mode t))
    ;; No buffer exists under that name: the indicator must not create one.
    (let ((absent-name (generate-new-buffer-name " *response-mode-indicator-absent*")))
      (let ((superchat-buffer-name absent-name))
        (superchat--show-response-mode-indicator 'streaming))
      (should-not (get-buffer absent-name)))
    ;; A plain buffer already exists under that name but is not a chat View:
    ;; it stays byte-identical, unmodified, with its undo list unchanged.
    (let ((foreign (generate-new-buffer " *response-mode-indicator-foreign*")))
      (unwind-protect
          (with-current-buffer foreign
            (text-mode)
            (buffer-enable-undo)
            (insert "ORIGINAL SOURCE TEXT")
            (set-buffer-modified-p nil)
            (setq buffer-undo-list nil)
            (let ((before-text (buffer-substring-no-properties (point-min) (point-max)))
                  (before-modified (buffer-modified-p))
                  (before-undo-length
                   (if (listp buffer-undo-list) (length buffer-undo-list) 0))
                  (superchat-buffer-name (buffer-name foreign)))
              (superchat--show-response-mode-indicator 'streaming)
              (should (equal before-text
                             (buffer-substring-no-properties (point-min) (point-max))))
              (should (eq before-modified (buffer-modified-p)))
              (should (= before-undo-length
                         (if (listp buffer-undo-list) (length buffer-undo-list) 0)))))
        (when (buffer-live-p foreign) (kill-buffer foreign))))
    ;; A live chat View still shows the indicator exactly as before.
    (let ((view (generate-new-buffer " *response-mode-indicator-view*")))
      (unwind-protect
          (with-current-buffer view
            (org-mode)
            (superchat-mode 1)
            (let ((superchat-buffer-name (buffer-name view)))
              (superchat--insert-prompt)
              (superchat--response-region-begin 'response-mode-owner t)
              (superchat--show-response-mode-indicator 'streaming))
            (should (string-match-p "🔄 Streaming Response Mode" (buffer-string))))
        (when (buffer-live-p view) (kill-buffer view))))))

(ert-deftest native-ui/cancel-keeps-partial-and-marks-stopped ()
  "Cancelling a streaming Run keeps the partial answer, appends a visible
`[Stopped]' marker, and clears the status line -- the chat View Sink's own
render (`superchat-render-view-sink'), not Runtime's deleted legacy probe."
  (journey-fixture-with
      (:script (list (list :chunks '("partial answer") :hang t)))
    (let* ((view (journey-fixture-open-chat))
           (run-id (journey-fixture-send "tell a long story" view)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "partial answer" (buffer-string))))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (with-current-buffer view (superchat-cancel)))
      (should (journey-fixture-wait-for-run view))
      (should (eq 'cancelled (plist-get (superchat-runtime-status run-id) :status)))
      (with-current-buffer view
        (let ((text (buffer-string)))
          (should (string-match-p "partial answer" text))
          (should (string-match-p "\\[Stopped\\]" text))
          (should-not (string-match-p "Prefill / waiting" text)))))))

(ert-deftest native-ui/controlled-stop-keeps-partial-and-marks-stopped ()
  "A controlled (`:input-core') Run's cancel also keeps the partial answer
and marks it `[Stopped]'.  S01's transitional \"controlled stop renders
nothing\" exception (`runtime/s01-transition-controlled-stop-renders-nothing')
is gone now that the legacy View Sink is deleted.

Rewritten onto `journey-fixture-with' (S02-fix-2): the ad-hoc `llm-ollama'
fixture this replaced resolved to the literal fallback model \"unknown\",
which needed a `superchat-llm-model-capability-overrides' override to force
streaming on — and that override was bound to the SAME list object as the
global default, so `superchat-llm-set-model-capabilities''s in-place
`setcdr' on an already-existing \"unknown\" entry could leak into later
alphabetically ordered tests.  `journey-fixture-with''s real
`llm-openai-compatible' provider already resolves streaming to true at the
adapter level (`superchat-llm--resolve-streaming'), so no override, no
shared mutable state, and no `skip-unless' are needed at all."
  (journey-fixture-with
      (:script (list (list :chunks '("partial answer") :hang t)))
    (let* ((view (journey-fixture-open-chat))
           (session-id (journey-fixture-session-id view))
           (handle
            (superchat-runtime-submit
             (list :prompt "hello" :buffer view
                   :session-id session-id :deadline nil
                   :input-core '(:enabled t))))
           (run-id (plist-get handle :run-id)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "partial answer" (buffer-string))))))
      (should (superchat-runtime-cancel run-id))
      (should (eq 'cancelled (plist-get (superchat-runtime-status run-id) :status)))
      (with-current-buffer view
        (let ((text (buffer-string)))
          (should (string-match-p "partial answer" text))
          (should (string-match-p "\\[Stopped\\]" text))
          (should-not (string-match-p "Prefill / waiting" text)))))))

(ert-deftest cmd-dispatch/rejected-enqueue-preserves-draft-and-recalled-memories ()
  "A rejected `queue.queued' Tape append must not consume the draft, mark
it queued, or drop the manually recalled-memory payload (S05b-fix-2 item
1: the caller used to lock/consume/mark-queued/open-a-new-prompt
unconditionally, regardless of `superchat-runtime-enqueue-follow-up''s
return value).  Injects only the `queue.queued' Tape append failure
(the seam rule's permitted \"Tape append failure you deliberately
inject\"), leaving every other Tape write real.

Moved onto `journey-fixture-with' (S05b-fix-3): the first send's scripted
round hangs so a real Run stays active for the second send's `admit'
check, and provider requests are counted with `journey-fixture-requests'
rather than an ad-hoc mock of `superchat--llm-generate-answer'.  Also
fixes what the S05b re-review rejected: `(buffer-string)' cannot see an
overlay's `after-string', so the queued indicator is checked by scanning
`overlays-in' instead; and the prompt marker's position and buffer are
captured before the second send and asserted unchanged after, rather than
only checking that the draft text is somewhere in the buffer."
  (let ((superchat-memory-auto-recall-min-length 999999))
    (journey-fixture-with
        (:script (list (list :hang t)))
      (let* ((view (journey-fixture-open-chat))
             (session (journey-fixture-session-id view)))
        (journey-fixture-send "first message" view)
        (should (= 1 (length (journey-fixture-requests))))
        (with-current-buffer view
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "second message MUST-STAY")))
        (let* ((recalled (list (list :content "RECALLED_MEMORY_MUST_STAY")))
               (marker-position (with-current-buffer view
                                  (marker-position superchat--prompt-start)))
               (marker-buffer (with-current-buffer view
                                (marker-buffer superchat--prompt-start)))
               messages)
          (with-current-buffer view
            (setq-local superchat--pending-recalled-memories recalled))
          (cl-letf* ((real-append (symbol-function 'superchat-db-tape-append))
                     ((symbol-function 'superchat-db-tape-append)
                      (cl-function
                       (lambda (session-id kind content &key meta topic)
                         (if (equal "queue.queued" (plist-get meta :event_type))
                             (error "tape write failed")
                           (funcall real-append session-id kind content
                                    :meta meta :topic topic)))))
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (push (apply #'format fmt args) messages))))
            (with-current-buffer view (superchat-send-input)))
          (should (= 0 (superchat-runtime-pending-follow-ups session)))
          (should (= 1 (length (journey-fixture-requests))))
          (with-current-buffer view
            (should (equal recalled superchat--pending-recalled-memories)))
          (should (cl-some (lambda (m) (string-match-p "Could not queue" m))
                           messages))
          (should-not (cl-some (lambda (m) (string-match-p "\\`Queued" m))
                               messages))
          (with-current-buffer view
            (should-not
             (cl-some (lambda (ov)
                        (let ((after (overlay-get ov 'after-string)))
                          (and after (string-match-p "queued" after))))
                      (overlays-in (point-min) (point-max))))
            (should (= marker-position (marker-position superchat--prompt-start)))
            (should (eq marker-buffer (marker-buffer superchat--prompt-start)))
            (should (equal "second message MUST-STAY"
                           (buffer-substring-no-properties
                            superchat--prompt-start (point-max))))
            (should-not (text-property-not-all
                         superchat--prompt-start (point-max) 'read-only nil))))))))

(ert-deftest cmd-dispatch/admit-fault-drops-and-leaves-draft-intact ()
  "An `admit' stage fault (a faulted direct implementation, no valid
direct result) must `drop', append `admission.dropped', and leave the
draft completely untouched -- never fall through to `process' or
`queue' (S05b-fix-2 item 2).  Temporarily removes the real
`chat-active-run' implementation so the only direct implementation is
the erroring test contribution, restoring it in `unwind-protect'.

Moved onto `journey-fixture-with' (S05b-fix-3): the first send's scripted
round hangs so a real Run stays active for the second send's `admit'
check, provider requests are counted with `journey-fixture-requests', and
Tape is read with `journey-fixture-events'.  Also fixes what the S05b
re-review rejected: the prompt marker's position and buffer are captured
before the second send and asserted unchanged after, rather than only
checking that the draft text is somewhere in the buffer.  Cancels the
still-hanging first Run at the end -- `journey-fixture-with' owns the
temporary Tape database and cancels any Run still active on exit, so
this only needs to assert the cancellation it triggers reaches a
terminal status.  Binds `superchat-stage-faults' locally around the
fault-producing part so this test's `admit' fault cannot leak into later
tests, and asserts the global ring is unchanged afterward."
  (let ((superchat-memory-auto-recall-min-length 999999))
    (journey-fixture-with
        (:script (list (list :hang t)))
      (let* ((view (journey-fixture-open-chat))
             (session (journey-fixture-session-id view))
             (run-id (journey-fixture-send "first message" view))
             (global-stage-faults superchat-stage-faults))
        (should (= 1 (length (journey-fixture-requests))))
        (with-current-buffer view
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "second message DRAFT-INTACT")))
        (let ((marker-position (with-current-buffer view
                                  (marker-position superchat--prompt-start)))
              (marker-buffer (with-current-buffer view
                               (marker-buffer superchat--prompt-start))))
          (superchat-stage-remove 'admit 'chat-active-run)
          (superchat-stage-add 'admit 'test-admit-boom
                                (lambda (&rest _) (error "admit boom"))
                                :owner 'test)
          ;; Bind the ring so this test's fault stays out of the global one;
          ;; the "fault recorded" assertions below read the local value.
          (let ((superchat-stage-faults nil))
            (let ((faults-before (length superchat-stage-faults))
                  messages)
              (unwind-protect
                  (cl-letf (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) messages))))
                    (with-current-buffer view (superchat-send-input)))
                (superchat-stage-remove 'admit 'test-admit-boom)
                (superchat-stage-add 'admit 'chat-active-run
                                      #'superchat-chat--admit :owner 'chat))
              (should (> (length superchat-stage-faults) faults-before))
              (should (cl-find 'test-admit-boom superchat-stage-faults
                                :key (lambda (f) (plist-get f :id))))
              (should (cl-find "admission.dropped"
                               (journey-fixture-events session)
                               :key (lambda (e) (plist-get e :type)) :test #'equal))
              (should (= 0 (superchat-runtime-pending-follow-ups session)))
              (should (= 1 (length (journey-fixture-requests))))
              (should (cl-some (lambda (m) (string-match-p "Message not sent" m))
                               messages))))
          (with-current-buffer view
            (should (= marker-position (marker-position superchat--prompt-start)))
            (should (eq marker-buffer (marker-buffer superchat--prompt-start)))
            (should (equal "second message DRAFT-INTACT"
                           (buffer-substring-no-properties
                            superchat--prompt-start (point-max))))
            (should-not (text-property-not-all
                         superchat--prompt-start (point-max) 'read-only nil))))
        ;; Quiescing the still-active first Run dispatches nothing -- nothing
        ;; was ever queued for it to hand off -- and it reaches a terminal
        ;; status.
        (should (superchat-runtime-cancel run-id "cleanup"))
        (should (eq 'cancelled (plist-get (superchat-runtime-status run-id) :status)))
        (should (= 0 (superchat-runtime-pending-follow-ups session)))
        (should (= 1 (length (journey-fixture-requests))))
        ;; The test's own `admit' fault must not have escaped its binding.
        (should (equal global-stage-faults superchat-stage-faults))))))

(ert-deftest native-ui/reopen-after-kill-renders-history-and-tool-headings ()
  "A killed View reopens with its history, S21 tool headings, and no
replaced turns (S22/C15 + S20 + S21)."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (target (generate-new-buffer " *native-ui-reopen-target*"))
           reopened)
      (with-current-buffer target (insert "NATIVE_UI_REOPEN_TOOL_CONTENT"))
      ;; Turn 1 keeps a real tool call; turn 2 is replaced by a regenerate, so
      ;; the reopened View must show the tool headings and only the new answer.
      (journey-fixture-set-script
       (list (list :tool-uses
                   `(("read_buffer" . ((buffer . ,(buffer-name target))))))
             (list :text "NATIVE_UI_TOOL_ANSWER")))
      (journey-fixture-send "read the target" view)
      (should (journey-fixture-wait-for-run view))
      (journey-fixture-set-script (list (list :text "NATIVE_UI_OLD_ANSWER")))
      (journey-fixture-send "NATIVE_UI_SECOND_QUESTION" view)
      (should (journey-fixture-wait-for-run view))
      (journey-fixture-set-script (list (list :text "NATIVE_UI_NEW_ANSWER")))
      (with-current-buffer view
        (superchat-regenerate))
      (should (journey-fixture-wait-for-run view))
      (let ((kill-buffer-query-functions nil)) (kill-buffer view))
      (setq reopened (superchat))
      (with-current-buffer reopened
        (should (equal session superchat--session-id))
        (should (string-match-p "NATIVE_UI_TOOL_ANSWER" (buffer-string)))
        (should (string-match-p "^\\*\\*\\* ✓ read_buffer (—)" (buffer-string)))
        (should (string-match-p "Tool call: read_buffer" (buffer-string)))
        (should (string-match-p "Tool result:" (buffer-string)))
        (should (= 2 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
        (should (string-match-p "NATIVE_UI_NEW_ANSWER" (buffer-string)))
        ;; S20: the replaced turn never comes back.
        (should-not (string-match-p "NATIVE_UI_OLD_ANSWER" (buffer-string)))
        (should (= 1 (how-many "^\\* User: NATIVE_UI_SECOND_QUESTION"
                               (point-min) (point-max))))
        ;; A Session with history keeps S24's onboarding presentation out.
        (should-not (string-match-p "is not configured" (buffer-string)))
        (should-not (string-match-p "Recent sessions" (buffer-string)))))))

(ert-deftest native-ui/reopen-during-live-run-does-not-seal-it ()
  "Reopening a killed View while its Run is still live must not seal it.
`superchat-runtime-reconcile-session' is restart reconciliation and would
otherwise write a false `run.failed :abandoned' row for the still-running,
process-local Run (S22-fix-1)."
  (journey-fixture-with
      (:script (list (list :chunks '("NATIVE_UI_HELD_PARTIAL") :hang t)))
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (run-id (journey-fixture-send "hold this run" view)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "NATIVE_UI_HELD_PARTIAL" (buffer-string))))))
      (should (eq 'running-model
                  (plist-get (superchat-runtime-status run-id) :status)))
      (let ((kill-buffer-query-functions nil)) (kill-buffer view))
      ;; Reopen the same Session in the SAME Emacs while the Run is current.
      (let ((reopened (superchat)))
        (with-current-buffer reopened
          (should (equal session superchat--session-id))
          (should (string-match-p "hold this run" (buffer-string))))
        (should (member run-id (superchat-runtime-active-runs)))
        (should (eq 'running-model
                    (plist-get (superchat-runtime-status run-id) :status)))
        (let ((events (journey-fixture-events session)))
          (should-not (cl-some (lambda (event)
                                 (and (equal "run.failed" (plist-get event :type))
                                      (plist-get (plist-get event :payload)
                                                 :abandoned)))
                               events)))
        ;; The Run still ends with exactly one real terminal fact.
        (superchat-runtime-cancel run-id "S22-fix-1 live-run reopen test")
        (should (journey-fixture-wait-for-run reopened))
        (let* ((events (journey-fixture-events session))
               (terminals (cl-remove-if-not
                           (lambda (event)
                             (member (plist-get event :type)
                                     '("run.completed" "run.failed"
                                                          "run.cancelled")))
                           events)))
          (should (= 1 (length terminals)))
          (should (equal "run.cancelled" (plist-get (car terminals) :type)))
          (should-not (cl-some (lambda (event)
                                 (and (equal "run.failed" (plist-get event :type))
                                      (plist-get (plist-get event :payload)
                                                 :abandoned)))
                               events)))))))

;; S15-fix-1: contract §10 gate 7 "clear on View kill", observed through the
;; real kill-buffer hook rather than by calling the clear function directly.
(ert-deftest native-ui/killing-the-view-drops-its-queued-follow-ups ()
  "Killing a chat View while it holds a queued follow-up drops that
follow-up durably (`queue.dropped', reason `view-closed') and never
dispatches it: the queued message's provider request is never made."
  (journey-fixture-with
      (:script (list (list :chunks '("VIEW_KILL_PARTIAL") :hang t)
                     (list :text "must never be requested")))
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (first-run (journey-fixture-send "first" view)))
      (should (journey-fixture-wait-until
               (lambda ()
                 (with-current-buffer view
                   (string-match-p "VIEW_KILL_PARTIAL" (buffer-string))))))
      (journey-fixture-send "second" view)
      (should (= 1 (superchat-runtime-pending-follow-ups session)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer view))
      (should (= 0 (superchat-runtime-pending-follow-ups session)))
      (let ((dropped (cl-find "queue.dropped" (journey-fixture-events session)
                              :key (lambda (event) (plist-get event :type))
                              :test #'equal)))
        (should dropped)
        (should (equal "view-closed"
                       (format "%s" (plist-get (plist-get dropped :payload) :reason)))))
      ;; Ending the first Run must not resurrect the dropped follow-up.
      (superchat-runtime-cancel first-run "view killed")
      (sit-for 0.2)
      (should (= 1 (length (journey-fixture-requests))))
      (should-not (member "queue.dispatched" (journey-fixture-event-types session))))))
