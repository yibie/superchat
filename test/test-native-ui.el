;;; test-native-ui.el --- ERT tests for transient menu + dired/embark hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-dispatch, superchat-dired-send, and embark hooks.
;; Pure logic checks — no transient/dired/embark servers needed.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat nil t)
(require 'superchat-tape-browser)

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
          (setq-local superchat--pause-requested t
                      superchat--active-run-id "run-view"
                      superchat--pending-round-reason 'tool-followup
                      superchat--pending-send-request #'ignore)
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("run-view")))
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
          (setq-local superchat-mode t
                      superchat--pause-requested t
                      superchat--pause-run-id "run-cancel"
                      superchat--active-run-id "run-cancel"
                      superchat--pending-round-reason 'tool-followup
                      superchat--pending-send-request #'ignore)
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

(ert-deftest native-ui/runtime-release-clears-owned-pause-state ()
  "Runtime terminal release clears only the terminated Run's View state."
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (name (generate-new-buffer-name " *superchat-release-view*"))
        entry)
    (unwind-protect
        (with-current-buffer (get-buffer-create name)
          (setq-local superchat--pause-requested t
                      superchat--pause-run-id "run-release"
                      superchat--active-run-id "run-release"
                      superchat--pending-round-reason 'tool-followup
                      superchat--pending-send-request #'ignore
                      superchat--resume-timeout-run-id "run-release"
                      superchat--resume-timeout-timer
                      (run-with-timer 100 nil #'ignore)
                      superchat--active-timeout-timer
                      (run-with-timer 100 nil #'ignore))
          (setq entry (list :run-id "run-release"
                            :buffer-name name
                            :status 'completed
                            :deadline-timer nil
                            :ephemeral-buffer nil))
          (superchat-runtime--put entry)
          (superchat-runtime--release entry)
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

(ert-deftest native-ui/superchat-mode-installs-and-restores-header-line ()
  "The chat status header does not destroy a pre-existing header line."
  (with-temp-buffer
    (setq-local header-line-format "original")
    (superchat-mode 1)
    (should (equal '(" " (:eval (superchat--header-line-run-state)))
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
                          (format "Describe #%s" image) "image-session"))
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

;;; test-native-ui.el ends here
