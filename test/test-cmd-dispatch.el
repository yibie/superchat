;;; test-cmd-dispatch.el — Test command dispatch through hooks -*- lexical-binding: t; -*-

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat)
(require 'superchat-workflow)

(eval-and-compile
  (unless (fboundp 'test-input-batch--fixture)
    (load (expand-file-name "test-llm-backend.el" (file-name-directory load-file-name)) nil t))
  (unless (fboundp 'superchat-stage-test--isolated)
    (load (expand-file-name "test-stage.el" (file-name-directory load-file-name)) nil t)))

(defmacro producer-test--chat (mode &rest body)
  "Real owned chat, transport mocks only; never decorate a Request in tests."
  (declare (indent 1) (debug t))
  `(test-input-batch--fixture ,mode
     (let ((superchat-editor-workspace--states (make-hash-table :test #'equal))
           (superchat-buffer-name (generate-new-buffer-name " *ask-producer*"))
           (superchat-turn-completed-functions '(superchat--notify-run-terminal))
           (superchat-run-completed-functions nil)
           (superchat-llm-tool-names '("input_batch_effect"))
           (superchat-runtime-default-deadline 1)
           view session)
       (unwind-protect
           (save-window-excursion
             (call-interactively #'superchat)
             (setq view (current-buffer) session superchat--session-id)
             ,@body)
         (let ((superchat-turn-completed-functions nil))
           (maphash (lambda (run _) (superchat-runtime-cancel run "cleanup")) superchat-runtime--runs))
         (when (buffer-live-p view)
           (with-current-buffer view (superchat--cancel-status-refresh-timer))
           (kill-buffer view))))))

(defun producer-test--press (group action)
  "Press the real inline RET binding and wait for its deferred action."
  (let ((button (nth (if (eq action 'answer) 2 3) (widget-get group :children)))
        (view (widget-get group :input-view))
        (snapshot (widget-get group :input-snapshot))
        (bound (+ (float-time) 3)))
    (goto-char (overlay-start (widget-get button :button-overlay)))
    (should (eq (key-binding (kbd "RET")) 'widget-button-press))
    (execute-kbd-macro (kbd "RET"))
    (while (and (< (float-time) bound) (superchat--input-current view snapshot))
      (sleep-for 0.01))
    (should-not (superchat--input-current view snapshot))))

(ert-deftest producer-ask/ordinary-real-answer-stop-expiry ()
  (dolist (mode '(async streaming))
    (dolist (action '(answer stop expiry))
      (producer-test--chat mode
        (let* ((approvals 0)
               (superchat-authority-approval-function (lambda (_) (cl-incf approvals) nil)))
          (human-input-test--send "Please ask which city")
          (let* ((run superchat--active-run-id)
                 (request (car test-input-batch--requests)))
            (should (member "ask_user" (mapcar #'llm-tool-name
                                               (llm-chat-prompt-tools (plist-get request :prompt)))))
            (test-trusted-ask--reply request)
            (let* ((q (superchat-runtime-pending-input run))
                   (group (superchat--input-panel-find view session run (plist-get q :handle))))
              (should group)
              (should (eq 'ask-user (plist-get (plist-get (superchat-runtime--entry run) :input-core) :operation)))
              (pcase action
                ('answer
                 (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
                   (producer-test--press group 'answer))
                 (should (= 2 (length test-input-batch--requests)))
                 (test-input-batch--reply (car test-input-batch--requests) nil)
                 (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))
                ('stop
                 (producer-test--press group 'stop)
                 (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status))))
                ('expiry
                 (input-terminal-test--wait-ended run)
                 (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
                 (should (equal "Expired" (widget-value (nth 1 (widget-get group :children)))))))
              (unless (eq action 'answer) (should (= 1 (length test-input-batch--requests)))))
            (should (= 0 approvals))))))))

(ert-deftest producer-ask/profile-and-default-precedence ()
  (dolist (mode '(async streaming))
    (dolist (row '((nil nil nil) (nil always t) (nil on-demand t)
                   (none always nil) (("input_batch_effect") always nil)
                   (("ask_user") nil t)))
      (producer-test--chat mode
        (let ((superchat-llm-tools-enabled (nth 1 row)))
          (when (car row)
            (setq-local superchat--active-preset
                        (superchat-preset--create :name "ask-profile" :type 'prompt
                                                 :tools (car row))))
          (human-input-test--send "Question")
          (let* ((run superchat--active-run-id)
                 (names (mapcar #'llm-tool-name
                                (llm-chat-prompt-tools
                                 (plist-get (car test-input-batch--requests) :prompt)))))
            (should (eq (not (null (member "ask_user" names))) (nth 2 row)))
            (should (eq (not (null (plist-get (superchat-runtime--entry run) :input-core))) (nth 2 row)))
            (when (eq (car row) 'none) (should-not names))))))))

(ert-deftest producer-ask/extension-default-selection-and-audit ()
  (dolist (mode '(async streaming))
    (producer-test--chat mode
      (let ((superchat-extension--registry nil))
        (superchat-define-extension 'ask-context :description "Ask context" :tools '("input_batch_effect")
                                     :context (lambda (_) "Extra context"))
        (human-input-test--send "Question")
        (let* ((request (car test-input-batch--requests))
               (names (mapcar #'llm-tool-name (llm-chat-prompt-tools (plist-get request :prompt))))
               (event (cl-find "extension.activated" (superchat-tape-replay-events session)
                               :test #'equal :key (lambda (e) (plist-get e :type)))))
          (should (member "ask_user" names))
          (should (member "input_batch_effect" names))
          (should (member "ask_user" (plist-get (plist-get event :payload) :tools)))
          (test-trusted-ask--reply request)
          (should (superchat-runtime-pending-input superchat--active-run-id)))))))

(ert-deftest producer-ask/extension-cannot-mutate-pair-before-or-after-record ()
  (dolist (phase '(compose recorded))
    (dolist (field '(:presenter :eligible-function :tool-name))
      (producer-test--chat 'async
        (let ((superchat-extension--registry nil)
              (append-event (symbol-function 'superchat-tape-append-event)) captured hits)
          (cl-labels ((mutate ()
                        (setq hits t)
                        (let ((config (plist-get captured :input-core)))
                          (if (eq field :tool-name)
                              (aset (plist-get config field) 0 ?X)
                            (plist-put config field #'ignore)))))
            (superchat-define-extension
             'ask-mutation :description "Ask mutation" :context
             (lambda (request)
               (setq captured request)
               (when (eq phase 'compose) (mutate)) "Context"))
            (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (lambda (event &rest args)
                         (prog1 (apply append-event event args)
                           (when (and (eq phase 'recorded)
                                      (equal "extension.activated" (plist-get event :type)))
                             (mutate))))))
              (condition-case nil (human-input-test--send "Question") (error nil)))
            (should hits)
            (should-not test-input-batch--requests)
            (let ((events (superchat-tape-replay-events session)))
              (should (eq (not (null (cl-find "extension.activated" events :test #'equal
                                              :key (lambda (e) (plist-get e :type)))))
                          (eq phase 'recorded))))))))))

(ert-deftest producer-ask/unsupported-direct-and-borrowed-stay-unexposed ()
  (producer-test--chat 'async
    (dolist (buffer (list view test-input-batch--host))
      (let* ((run (plist-get (superchat-runtime-test-submit
                             (list :prompt "Direct" :buffer buffer :session-id session :deadline nil)) :run-id))
             (names (mapcar #'llm-tool-name (llm-chat-prompt-tools
                                            (plist-get (car test-input-batch--requests) :prompt)))))
        (should-not (member "ask_user" names))
        (should-not (plist-get (superchat-runtime--entry run) :input-core))))
    (dolist (extra '((:parent-run-id "parent") (:topology team) (:topology subagent)
                     (:attendance unattended) (:prepared-tools nil)))
      (let ((request (append (list :buffer view :session-id session :prompt "Unsupported") extra)))
        (should-not (plist-get (superchat--with-supported-input request 'response) :input-core))))))

(ert-deftest producer-ask/public-q1-q2-and-downstream-approval ()
  (dolist (mode '(async streaming))
    (producer-test--chat mode
      (let* ((endpoint (symbol-function 'llm-request-plz-async))
             (superchat-authority--policy-registry (copy-hash-table superchat-authority--policy-registry))
             (approvals 0)
             (superchat-authority-approval-function (lambda (_) (cl-incf approvals) t)))
        (superchat-authority-register-policy
         'producer-write-policy
         (lambda (effects _context)
           (if (eq 'human-interaction (plist-get (car effects) :effect)) 'defer 'ask)))
        (superchat-register-tool "producer_write" :description "Fixture downstream write"
                                 :args '((:name "value" :type string :description "value"))
                                 :effects '((:effect local-write :resource buffer :scope "fixture"))
                                 :function (lambda (_) "wrote"))
        (setq-local superchat--active-preset
                    (superchat-preset--create :name "write-profile" :type 'prompt
                                             :tools '("ask_user" "producer_write")))
        (setq-local superchat-authority-mode 'auto
                    superchat-authority--session-allowlist nil
                    superchat--active-authority-policy 'producer-write-policy)
        (human-input-test--send "Question")
        (let ((run superchat--active-run-id))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let* ((q1 (superchat-runtime-pending-input run))
                 (p1 (superchat--input-panel-find view session run (plist-get q1 :handle))))
            (cl-letf (((symbol-function 'llm-request-plz-async)
                       (lambda (&rest args)
                         (prog1 (apply endpoint args)
                           (when (= 2 (length test-input-batch--requests))
                             (test-trusted-ask--reply (car test-input-batch--requests) "Which date?")))))
                      ((symbol-function 'read-string) (lambda (&rest _) "Paris")))
              (superchat--input-panel-action p1 'answer))
            (let* ((q2 (superchat-runtime-pending-input run))
                   (p2 (superchat--input-panel-find view session run (plist-get q2 :handle))))
              (should (equal "Which date?" (plist-get q2 :question)))
              (superchat--input-panel-action p1 'stop)
              (should (superchat-runtime-pending-input run))
              (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Tomorrow")))
                (superchat--input-panel-action p2 'answer))))
          (should (= 0 approvals))
          (test-input-batch--reply (car test-input-batch--requests) '("producer_write") "value")
          (should (= 1 approvals)))))))

(ert-deftest input-panel/native-entry-answer-and-draft ()
  "The fixture-only presenter uses real native scheduling in both modes.
S05b/D7: a same-view send while the native input round is active queues the
draft instead of retaining it inertly -- it still does not interfere with
the pending widget, and dispatches on its own once the Run completes."
  (unless (fboundp 'test-input-batch--reply)
    (ert-skip "Load test-llm-backend for the shared native transport fixture"))
  (dolist (mode '(async streaming))
    (input-panel-test--with-chat
     mode
     (lambda (view session requests reply)
       (human-input-test--send "Ask a question")
       (let ((run (car (superchat-runtime-active-runs))))
         (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
         (let* ((pending (superchat-runtime-pending-input run))
                (panel (superchat--input-panel-find view session run (plist-get pending :handle)))
                (draft superchat--prompt-start))
           (should (superchat--input-widget-live-p panel))
           (should (eq view (marker-buffer (widget-get panel :from))))
           (should (< (widget-get panel :to) draft))
           (insert (propertize "PRIVATE DRAFT" 'input-fixture t))
           (goto-char (+ draft 3))
           (execute-kbd-macro (kbd "C-c C-c"))
           (should (= 1 (length (funcall requests))))
           (should (equal (list run) (superchat-runtime-active-runs)))
           (should (= 1 (superchat-runtime-pending-follow-ups session)))
           (save-excursion
             (let ((button (nth 2 (widget-get panel :children))) deferred)
               (goto-char (widget-get button :from))
               (should (equal (widget-get button :tag) "Answer"))
               (should (eq (key-binding (kbd "RET")) 'widget-button-press))
               (should (eq (key-binding (kbd "<down-mouse-1>")) 'widget-button-click))
               (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris"))
                         ((symbol-function 'run-at-time)
                          (lambda (_time _repeat fn &rest args)
                            (setq deferred (lambda () (apply fn args))))))
                 (execute-kbd-macro (kbd "RET"))
                 (should (superchat-runtime-pending-input run))
                 (should (superchat--input-widget-live-p panel)))
               ;; Only the widget action was deferred; native clocks are real.
               (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
                 (funcall deferred))))
           (should (= 2 (length (funcall requests))))
           (dolist (request (funcall requests))
             (should-not (string-match-p "PRIVATE DRAFT"
                                         (prin1-to-string (plist-get request :data)))))
           (should-not (superchat-runtime-pending-input run))
           (funcall reply (car (funcall requests)) nil)
           (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
           ;; Runtime's follow-up queue dispatches on a zero-delay timer once
           ;; the Run ends; pump the event loop for it.
           (let ((deadline (+ (float-time) 2.0)))
             (while (and (< (length (funcall requests)) 3) (< (float-time) deadline))
               (sit-for 0.01)))
           (should (= 3 (length (funcall requests))))
           (should (= 0 (superchat-runtime-pending-follow-ups session)))
           (should (string-match-p "PRIVATE DRAFT"
                                   (prin1-to-string (plist-get (car (funcall requests)) :data))))))))))

(defvar input-panel-test--on-request nil)

(ert-deftest input-inline/untrusted-text-fold-final-and-empty-answer ()
  (dolist (mode '(async streaming))
    (input-panel-test--with-chat
     mode
     (lambda (view session requests reply)
       (human-input-test--send "Question")
       (let* ((run (car (superchat-runtime-active-runs)))
              (question "City?\n* rogue\n#+end_example\n,comma\n ** nested")
              (owner (plist-get (superchat-runtime-status run) :transcript-owner)))
         (should-not (equal owner run))
         (funcall reply (car (funcall requests)) '("input_batch_ask") question)
         (let* ((pending (superchat-runtime-pending-input run))
                (group (superchat--input-panel-find view session run (plist-get pending :handle)))
                (button (nth 2 (widget-get group :children)))
                (prompt superchat--prompt-start)
                (recall (list (list :content "RECALL untouched"))))
           (setq-local superchat--pending-recalled-memories recall)
           (save-excursion
             (goto-char (widget-get group :from))
             (should (looking-at "\\*\\*\\* Required input"))
             (should (org-up-heading-safe))
             (ert-info ((buffer-substring-no-properties (point-min) (point-max)))
               (should (looking-at "\\*\\* Assistant:")))
             (org-fold-hide-subtree)
             (should (invisible-p (widget-get button :from)))
             (should-not (invisible-p prompt))
             (org-fold-show-subtree)
             (should-not (invisible-p (widget-get button :from)))
             (font-lock-set-defaults)
             (font-lock-fontify-region (point-min) (point-max))
             (goto-char (widget-get button :from))
             (should (eq (key-binding (kbd "RET")) 'widget-button-press)))
           (let (feedback)
             (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " \n "))
                       ((symbol-function 'message) (lambda (fmt &rest args) (setq feedback (apply #'format fmt args)))))
               (superchat--input-panel-action group 'answer))
             (should (equal feedback "Answer cannot be empty. Enter a response or stop this work.")))
           (should (eq (plist-get pending :handle) (plist-get (superchat-runtime-pending-input run) :handle)))
           (should (= 1 (length (funcall requests))))
           (should (eq recall superchat--pending-recalled-memories))
           (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
             (superchat--input-panel-action group 'answer))
           (funcall reply (car (funcall requests)) nil)
           (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
           (should (superchat--input-widget-live-p group))
           (should (< (widget-get group :to) prompt))
           (should (eq recall superchat--pending-recalled-memories))
           (should (string-match-p ",\\* rogue" (buffer-string)))))))))

(ert-deftest input-inline/captured-stop-scope-labels ()
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (let ((show (symbol-function 'superchat--input-show)))
       (dolist (scope '(response workflow plan))
         (human-input-test--send "Scope")
         (let ((run (car (superchat-runtime-active-runs))))
           (cl-letf (((symbol-function 'superchat--input-show)
                      (lambda (v s snapshot)
                        (funcall show v s (plist-put (copy-sequence snapshot) :presentation-scope scope)))))
             (funcall reply (car (funcall requests)) '("input_batch_ask") "City?"))
           (let* ((q (superchat-runtime-pending-input run))
                  (group (superchat--input-panel-find view session run (plist-get q :handle)))
                  (expected (pcase scope ('response "Stop response") ('workflow "Stop workflow") (_ "Stop plan execution"))))
             (should (equal expected (widget-get (nth 3 (widget-get group :children)) :tag)))
             (superchat--input-panel-action group 'stop)
             (should (plist-get (superchat-runtime-status run) :stop-requested)))))))))

(ert-deftest input-inline/failed-clear-keeps-restored-widget-binding ()
  (dolist (condition '(error quit))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (human-input-test--send "Question")
     (let ((run (car (superchat-runtime-active-runs))))
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let* ((q (superchat-runtime-pending-input run))
              (group (superchat--input-panel-find view session run (plist-get q :handle)))
              (watcher (lambda (_ value operation where)
                         (when (and (eq operation 'set) (eq where view)
                                    (not (equal value session)))
                           (signal condition '("publication injected"))))))
         (unwind-protect
             (progn
               (add-variable-watcher 'superchat--session-id watcher)
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                 (should (equal (condition-case err (superchat--clear-chat-and-context)
                                  ((error quit) err))
                                (list condition "publication injected")))))
           (remove-variable-watcher 'superchat--session-id watcher))
         (should (equal session superchat--session-id))
         ;; Cancellation happened before failed publication; restored controls
         ;; remain truthful/inert rather than granting the old answer anew.
         (should (plist-get (superchat-runtime-status run) :stop-requested))
         (should (superchat--input-widget-live-p group))
         (should (eq group (superchat--input-panel-find view session run (plist-get q :handle))))
         (should (equal "Stopped" (widget-value (nth 1 (widget-get group :children)))))
         (let* ((hide (nth 4 (widget-get group :children)))
                (button-overlay (widget-get hide :button-overlay))
                (foreign (make-overlay (point-max) (point-max))))
           (unwind-protect
               (progn
                 (save-excursion
                   (goto-char (widget-get hide :from))
                   (should (eq (key-binding (kbd "RET")) 'widget-button-press))
                   (should (eq (key-binding (kbd "<down-mouse-1>")) 'widget-button-click)))
                 (superchat--clear-chat-and-context)
                 (should-not (superchat--input-widget-live-p group))
                 (should-not (overlay-buffer button-overlay))
                 (should (eq view (overlay-buffer foreign))))
             (delete-overlay foreign)))))))))
(ert-deftest input-panel/sole-foreign-view-candidate-not-owned ()
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (human-input-test--send "Ordinary A")
     (let* ((a (car (superchat-runtime-active-runs)))
            (a-request (car (funcall requests)))
            (owner (plist-get (superchat-runtime-status a) :transcript-owner))
            (foreign (generate-new-buffer " *independent foreign host*")))
       (unwind-protect
           (let ((b (plist-get
                     (superchat-runtime-test-submit
                      (list :prompt "Independent borrowed caller" :session-id session :buffer foreign
                            :transcript-owner owner :surface 'superchat :topology 'single
                            :tools nil :deadline nil :stream-function #'ignore :delivery-function #'ignore)) :run-id)))
             (funcall reply a-request nil)
             (should (eq 'completed (plist-get (superchat-runtime-status a) :status)))
             (should (member b (superchat-runtime-active-runs)))
             (message "SELECTOR before current-owner=%S borrowed=%S" superchat--response-region-current-owner b)
             (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
               (call-interactively #'superchat-cancel))
             (message "SELECTOR after borrowed-status=%S" (plist-get (superchat-runtime-status b) :status))
             (should (eq 'running-model (plist-get (superchat-runtime-status b) :status))))
         (kill-buffer foreign))))))
(ert-deftest input-panel/public-stop-before-submit-returns ()
  (dolist (cutpoint '("run.completed" "turn.completed"))
   (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (let* ((append-event (symbol-function 'superchat-tape-append-event-record))
            (cancel (symbol-function 'superchat-runtime-cancel))
            active tracked accepted feedback observed-run observed-status (cancel-calls 0)
            (input-panel-test--on-request
             (lambda (request)
               (if (= 1 (length (funcall requests)))
                   (progn
                     (funcall reply request '("input_batch_ask") "City?")
                     (let* ((run (car (superchat-runtime-active-runs)))
                            (q (superchat-runtime-pending-input run))
                            (panel (superchat--input-panel-find view session run (plist-get q :handle))))
                       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
                         (superchat--input-panel-action panel 'answer))))
                 (funcall reply request nil)))))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (prog1 (funcall append-event event)
                      (when (equal cutpoint (plist-get event :type))
                        (setq observed-run (plist-get event :run-id)
                              observed-status (superchat-runtime-status observed-run)
                              active (superchat-runtime-active-runs)
                              tracked (buffer-local-value 'superchat--active-run-id view))
                        (with-current-buffer view
                          (cl-letf (((symbol-function 'superchat-runtime-cancel)
                                     (lambda (&rest args)
                                       (cl-incf cancel-calls)
                                       (setq accepted (apply cancel args))))
                                    ((symbol-function 'read-string) (lambda (&rest _) ""))
                                    ((symbol-function 'message)
                                     (lambda (format-string &rest args)
                                       (setq feedback (apply #'format format-string args)))))
                            (call-interactively #'superchat-cancel))))))))
         (human-input-test--send "Question"))
       (message "SELECTOR-PROBE active=%S tracked=%S accepted=%S feedback=%S final=%S requests=%s"
                active tracked accepted feedback (superchat-runtime-status observed-run)
                (length (funcall requests)))
       (should (member observed-run active))
       (should-not tracked)
       (should (plist-get observed-status :control-active-p))
       (when (equal cutpoint "turn.completed")
         (should (eq 'completed (plist-get observed-status :status))))
       (should accepted)
       (should (= 1 cancel-calls))
       (should (= 2 (length (funcall requests))))
       (should (eq 'completed (plist-get (superchat-runtime-status observed-run) :status)))
       (should (plist-get (superchat-runtime-status observed-run) :stop-requested))
       (with-current-buffer view (should-not (string-match-p "FINAL" (buffer-string)))))))))

(ert-deftest input-panel/untracked-selector-exact-provenance-and-ambiguity ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (dolist (other-kind '(ordinary workflow subagent foreign ambiguous))
    (input-panel-test--with-chat
     'async
     (lambda (view session requests reply)
       (human-input-test--send "Question")
       (let* ((a (car (superchat-runtime-active-runs)))
              (owner (plist-get (superchat-runtime-status a) :transcript-owner))
              (foreign (generate-new-buffer " *selector foreign*")))
         (unwind-protect
             (progn
               (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
               (let* ((b (plist-get
                          (superchat-runtime-test-submit
                           (list :prompt "Other caller" :session-id (if (eq other-kind 'foreign) "foreign-selector" session)
                                 :buffer (if (eq other-kind 'ambiguous) view foreign)
                                 :transcript-owner (if (eq other-kind 'ordinary) "different-owner" owner)
                                 :surface (if (eq other-kind 'subagent) 'subagent 'superchat)
                                 :topology (if (eq other-kind 'workflow) 'workflow 'single)
                                 :tools nil :deadline nil
                                 :stream-function #'ignore :delivery-function #'ignore))
                          :run-id))
                      ;; Exercise fallback without fabricating a tracking id.
                      (superchat--active-run-id nil))
                 (let ((superchat--response-region-current-owner "not-the-owner"))
                   (call-interactively #'superchat-cancel)
                   (should-not (plist-get (superchat-runtime-status a) :stop-requested)))
                 (let ((superchat--session-id "not-the-session"))
                   (call-interactively #'superchat-cancel)
                   (should-not (plist-get (superchat-runtime-status a) :stop-requested)))
                 (if (eq other-kind 'ambiguous)
                     (progn
                       (should-error (call-interactively #'superchat-cancel) :type 'user-error)
                       (should-not (plist-get (superchat-runtime-status a) :stop-requested)))
                   (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
                     (call-interactively #'superchat-cancel))
                   (should (plist-get (superchat-runtime-status a) :stop-requested)))
                 (should (member b (superchat-runtime-active-runs)))
                 (should-not (plist-get (superchat-runtime-status b) :stop-requested))))
           (kill-buffer foreign)))))))
(defvar input-panel-test--stop-observer nil)

(ert-deftest input-panel/stop-notification-once-even-with-condition ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (dolist (fault '(nil error quit))
    (let* ((notifications 0)
          (input-panel-test--stop-observer
           (lambda (status)
             (cl-incf notifications)
             (should (plist-get status :stop-requested))
             (should-not (superchat-runtime-effect-capable-p (plist-get status :run-id)))
             (superchat-runtime-cancel (plist-get status :run-id))
             (when fault (signal fault '("stop observer"))))))
      (input-panel-test--with-chat
       'async
       (lambda (view session requests reply)
         (human-input-test--send "Question")
         (let ((run (car (superchat-runtime-active-runs))))
           (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
           (let* ((q (superchat-runtime-pending-input run))
                  (panel (superchat--input-panel-find view session run (plist-get q :handle))))
             (superchat--input-panel-action panel 'stop)
             (superchat--input-panel-action panel 'stop)
             (should (= 1 notifications))
             (should-not (plist-get (superchat-runtime-status run) :control-active-p))
             (when fault
               (should (equal (cons fault '("stop observer"))
                              (plist-get (superchat-runtime-status run) :stop-notification-error))))
             (should (= 1 (length (funcall requests)))))))))))

(ert-deftest input-panel/same-question-two-exact-views ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (human-input-test--send "Question")
     (let* ((a (car (superchat-runtime-active-runs)))
            (other (superchat--ensure-chat-view (generate-new-buffer-name " *input B*") "input-panel-B")))
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let* ((qa (superchat-runtime-pending-input a))
              (pa (superchat--input-panel-find view session a (plist-get qa :handle)))
              (b (with-current-buffer other
                   (setq-local superchat-authority-mode 'auto
                               superchat-authority--session-allowlist '("input_batch_ask"))
                   (plist-get
                    (superchat-runtime-test-submit
                     (list :prompt "Question" :session-id "input-panel-B" :buffer other
                           :tools '("input_batch_ask") :deadline nil
                           :input-core (superchat--input-presenter-config other "input-panel-B" "input_batch_ask")))
                    :run-id))))
         (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
         (let* ((qb (superchat-runtime-pending-input b))
                (pb (superchat--input-panel-find other "input-panel-B" b (plist-get qb :handle))))
           (should (superchat--input-widget-live-p pb))
           (should-not (eq pa pb))
           (should-not (superchat--input-current other qa))
           (superchat--input-panel-action pa 'stop)
           (should (eq (plist-get qb :handle) (plist-get (superchat-runtime-pending-input b) :handle)))
           (should-not (plist-get (superchat-runtime-status b) :stop-requested))
           (superchat--input-panel-action pb 'stop)
           (with-current-buffer other
             (let ((inhibit-read-only t)) (widget-delete pb)))))))))

(ert-deftest input-panel/public-task-stop-after-completed-before-delivery ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (human-input-test--send "Question")
     (let* ((run (car (superchat-runtime-active-runs)))
            (append-event (symbol-function 'superchat-tape-append-event-record))
            (cancel (symbol-function 'superchat-runtime-cancel))
            accepted feedback)
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let ((q (superchat-runtime-pending-input run)))
         (superchat-runtime-answer-input run (plist-get q :handle) "Paris"))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (prog1 (funcall append-event event)
                      (when (and (equal run (plist-get event :run-id))
                                 (equal "run.completed" (plist-get event :type)))
                        (should (cl-find "run.completed" (superchat-tape-replay-events session)
                                         :test #'equal :key (lambda (row) (plist-get row :type))))
                        (with-current-buffer view
                          (cl-letf (((symbol-function 'superchat-runtime-cancel)
                                     (lambda (&rest args) (setq accepted (apply cancel args))))
                                    ((symbol-function 'read-string) (lambda (&rest _) ""))
                                    ((symbol-function 'message)
                                     (lambda (format-string &rest args)
                                       (setq feedback (apply #'format format-string args)))))
                            (call-interactively #'superchat-cancel))))))))
         (funcall reply (car (funcall requests)) nil))
       (should accepted)
       (should (string-match-p "Stop accepted" feedback))
       (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
       (should-not (plist-get (superchat-runtime-status run) :control-active-p))
       (should-not (superchat-runtime-cancel run))
       (should (= 2 (length (funcall requests))))
       (with-current-buffer view (should-not (string-match-p "FINAL" (buffer-string))))
       (let ((events (superchat-tape-replay-events session)))
         (should (= 1 (cl-count "run.completed" events :test #'equal :key (lambda (e) (plist-get e :type)))))
         (should (= 0 (cl-count "run.cancelled" events :test #'equal :key (lambda (e) (plist-get e :type))))))))))

(ert-deftest input-panel/q1-q2-and-minibuffer-revalidation ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (dolist (mode '(async streaming))
    (input-panel-test--with-chat
     mode
     (lambda (view session requests reply)
       (human-input-test--send "Question")
       (let ((run (car (superchat-runtime-active-runs))))
         (funcall reply (car (funcall requests)) '("input_batch_ask") "Q1")
         (let* ((q1 (superchat-runtime-pending-input run))
                (panel (superchat--input-panel-find view session run (plist-get q1 :handle)))
                (input-panel-test--on-request
                 (lambda (request)
                   (let ((input-panel-test--on-request nil))
                     (funcall reply request '("input_batch_ask") "Q2")))))
           (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "First")))
             (superchat--input-panel-action panel 'answer))
           (let* ((q2 (superchat-runtime-pending-input run))
                  (panel2 (superchat--input-panel-find view session run (plist-get q2 :handle))))
             (should (equal "Q2" (plist-get q2 :question)))
             (should-not (eq (plist-get q1 :handle) (plist-get q2 :handle)))
             (superchat--input-panel-action panel 'stop)
             (should (eq (plist-get q2 :handle) (plist-get (superchat-runtime-pending-input run) :handle)))
             ;; A minibuffer callback settles Q2 and installs Q3 before its
             ;; stale text returns.  The returned text must not answer Q3.
             (let ((input-panel-test--on-request
                    (lambda (request)
                      (let ((input-panel-test--on-request nil))
                        (funcall reply request '("input_batch_ask") "Q3")))))
               (cl-letf (((symbol-function 'read-string)
                          (lambda (&rest _)
                            (superchat-runtime-answer-input run (plist-get q2 :handle) "Second")
                            "STALE ANSWER")))
                 (superchat--input-panel-action panel2 'answer)))
             (should (equal "Q3" (plist-get (superchat-runtime-pending-input run) :question)))
             (should (= 3 (length (funcall requests))))
             (should-not (plist-get (superchat-runtime-status run) :stop-requested)))))))))

(defun input-terminal-test--wait-ended (run)
  "Yield to native timers for at most three seconds; never finish RUN here."
  (let ((bound (+ (float-time) 3)))
    (while (and (not (memq (plist-get (superchat-runtime-status run) :status)
                           '(completed cancelled failed timed-out audit-incomplete)))
                (< (float-time) bound))
      (sleep-for 0.01)))
  (should (memq (plist-get (superchat-runtime-status run) :status)
                '(completed cancelled failed timed-out audit-incomplete))))

(ert-deftest input-terminal/elapsed-refreshes-existing-group ()
  (dolist (mode '(async streaming))
    (dolist (presentation '(visible hidden renamed history-point))
      (ert-info ((format "mode=%s presentation=%s" mode presentation))
        (input-panel-test--with-chat
         mode
         (lambda (view session requests reply)
           (let ((superchat-turn-completed-functions
                  (list #'superchat--notify-run-terminal))
                 (superchat-runtime-default-deadline 0.2))
             (human-input-test--send "Expires")
             (let ((run (car (superchat-runtime-active-runs))))
               (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
               (let* ((q (superchat-runtime-pending-input run))
                      (group (superchat--input-panel-find view session run (plist-get q :handle)))
                      (draft-marker superchat--prompt-start)
                      (recall (list 'next-draft-recall)))
                 (should (superchat--input-widget-live-p group))
                 (should (equal "Waiting for your answer"
                                (widget-value (nth 1 (widget-get group :children)))))
                 (goto-char (point-max))
                 (insert (propertize "next\ndraft" 'face 'bold))
                 (setq-local superchat--pending-recalled-memories recall)
                 (pcase presentation
                   ('hidden (superchat--input-panel-action group 'hide))
                   ('renamed (rename-buffer (concat (buffer-name) " renamed") t))
                   ('history-point (goto-char (point-min))))
                 (let ((draft (buffer-substring draft-marker (point-max)))
                       (saved-point (copy-marker (point)))
                       (hidden (widget-get group :input-hidden)))
                   (unwind-protect
                       (progn
                         (input-terminal-test--wait-ended run)
                         (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
                         (should (eq 'unanswered (plist-get (superchat-runtime-status run)
                                                           :input-continuation-blocked)))
                         (should-not (superchat-runtime-pending-input run))
                         ;; No refresh, hook or deadline function is invoked by
                         ;; this test: the actual product terminal observer runs.
                         (should (equal "Expired" (widget-value (nth 1 (widget-get group :children)))))
                         (dolist (button (cl-subseq (widget-get group :children) 2 4))
                           (should (widget-get button :inactive)))
                         (should (eq hidden (widget-get group :input-hidden)))
                         (when hidden (should (overlay-get hidden 'invisible)))
                         (should (eq draft-marker superchat--prompt-start))
                         (should (equal-including-properties draft (buffer-substring draft-marker (point-max))))
                         (should (= saved-point (point)))
                         (should (eq recall superchat--pending-recalled-memories))
                         (should (= 1 (length (funcall requests))))
                         (let ((events (superchat-tape-replay-events session)))
                           (should (= 1 (cl-count "tool.failed" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
                           (should-not (cl-find "tool.completed" events :key (lambda (e) (plist-get e :type)) :test #'equal))
                           (should (eq 'proved (plist-get (superchat-closure-judge
                                                          (list :kind 'durable-run-terminal :run-id run) events)
                                                         :verdict)))))
                     (set-marker saved-point nil))))))))))))

(ert-deftest input-terminal/stale-or-deleted-view-is-not-refreshed ()
  (dolist (mode '(async streaming))
    (dolist (change '(new-session dead replacement deleted))
      (input-panel-test--with-chat
       mode
       (lambda (view session requests reply)
         (let ((superchat-turn-completed-functions (list #'superchat--notify-run-terminal))
               (superchat-runtime-default-deadline 0.2))
           (human-input-test--send "Expires")
           (let ((run (car (superchat-runtime-active-runs))))
             (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
             (let* ((q (superchat-runtime-pending-input run))
                    (group (superchat--input-panel-find view session run (plist-get q :handle)))
                    (name (buffer-name view)) replacement)
               (should group)
               (unwind-protect
                   (progn
                     (pcase change
                       ('new-session (setq-local superchat--session-id "replacement-session"))
                       ('deleted (let ((inhibit-read-only t)) (widget-delete group)))
                       ((or 'dead 'replacement)
                        (kill-buffer view)
                        (when (eq change 'replacement)
                          (setq replacement (get-buffer-create name))
                          (with-current-buffer replacement
                            (org-mode)
                            (setq-local superchat--session-id session)
                            (insert "Same name, different View")))))
                     (let* ((target (if (buffer-live-p view) view replacement))
                            (before (and target (with-current-buffer target (buffer-string)))))
                       (input-terminal-test--wait-ended run)
                       (when (memq change '(new-session replacement))
                         (with-current-buffer target
                           (should (equal-including-properties before (buffer-string)))))
                       (when (eq change 'new-session)
                         (should (equal "Waiting for your answer"
                                        (widget-value (nth 1 (widget-get group :children))))))
                       (when (memq change '(dead replacement deleted))
                         (should-not (superchat--input-widget-live-p group))
                         (should-not (superchat--input-groups target))))
                     (should (= 1 (length (funcall requests)))))
                 (when (buffer-live-p replacement) (kill-buffer replacement)))))))))))

(ert-deftest input-terminal/q2-expiry-does-not-refresh-other-run ()
  (dolist (mode '(async streaming))
    (input-panel-test--with-chat
     mode
     (lambda (view session requests reply)
       (let ((superchat-turn-completed-functions (list #'superchat--notify-run-terminal))
             (superchat-runtime-default-deadline 0.4))
         (human-input-test--send "Q1")
         (let ((a (car (superchat-runtime-active-runs))))
           (funcall reply (car (funcall requests)) '("input_batch_ask") "Q1")
           (let* ((q1 (superchat-runtime-pending-input a))
                  (g1 (superchat--input-panel-find view session a (plist-get q1 :handle)))
                  (input-panel-test--on-request
                   (lambda (request)
                     (let ((input-panel-test--on-request nil))
                       (funcall reply request '("input_batch_ask") "Q2")))))
             (superchat-runtime-answer-input a (plist-get q1 :handle) "First")
             (let* ((q2 (superchat-runtime-pending-input a))
                    (g2 (superchat--input-panel-find view session a (plist-get q2 :handle)))
                    (input-panel-test--on-request nil)
                    (b-handle (superchat-runtime-test-submit
                               (list :prompt "Independent B" :session-id session :buffer view
                                     :tools '("input_batch_ask") :deadline nil
                                     :transcript-owner "independent-input-owner"
                                     :input-core (superchat--input-presenter-config view session "input_batch_ask"))))
                    (b (plist-get b-handle :run-id)))
               (should (equal "Q2" (plist-get q2 :question)))
               (should-not (eq (plist-get q1 :handle) (plist-get q2 :handle)))
               (funcall reply (car (funcall requests)) '("input_batch_ask") "B")
               (let* ((qb (superchat-runtime-pending-input b))
                      (gb (superchat--input-panel-find view session b (plist-get qb :handle)))
                      (owner superchat--response-region-current-owner)
                      (refresh (symbol-function 'superchat--input-panel-refresh))
                      refreshed)
                 (should gb)
                 (cl-letf (((symbol-function 'superchat--input-panel-refresh)
                            (lambda (group)
                              (push group refreshed)
                              (funcall refresh group))))
                   (input-terminal-test--wait-ended a))
                 (should (memq g2 refreshed))
                 (should-not (memq gb refreshed))
                 (should (eq 'timed-out (plist-get (superchat-runtime-status a) :status)))
                 (should (equal "Expired" (widget-value (nth 1 (widget-get g2 :children)))))
                 (should (eq (plist-get qb :handle) (plist-get (superchat-runtime-pending-input b) :handle)))
                 (should (equal "Waiting for your answer" (widget-value (nth 1 (widget-get gb :children)))))
                 (should (equal owner superchat--response-region-current-owner))
                 (superchat--input-panel-action g1 'stop)
                 (should-not (plist-get (superchat-runtime-status b) :stop-requested))
                 (should (= 3 (length (funcall requests)))))))))))))

(ert-deftest input-terminal/original-failure-condition-is-not-expiry ()
  (dolist (mode '(async streaming))
    (dolist (origin '(audit observer))
      (dolist (kind '(error quit))
        (input-panel-test--with-chat
         mode
         (lambda (view session requests reply)
           (let ((superchat-turn-completed-functions (list #'superchat--notify-run-terminal)))
             (human-input-test--send "Question")
             (let ((run (car (superchat-runtime-active-runs))))
               (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
               (let* ((q (superchat-runtime-pending-input run))
                      (group (superchat--input-panel-find view session run (plist-get q :handle)))
                      (append-event (symbol-function 'superchat-tape-append-event-record))
                      (completed (symbol-function 'superchat-runtime--recovery-tool-completed))
                      (refresh (symbol-function 'superchat--input-refresh-terminal-groups))
                      (notifications 0) caught)
                 (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                            (lambda (event &rest args)
                              (if (and (eq origin 'audit) (equal "tool.completed" (plist-get event :type)))
                                  (signal kind '("ORIGINAL INPUT CONDITION"))
                                (apply append-event event args))))
                           ((symbol-function 'superchat-runtime--recovery-tool-completed)
                            (lambda (id completion)
                              (prog1 (funcall completed id completion)
                                (when (and (eq origin 'observer) (equal id run))
                                  (signal kind '("ORIGINAL INPUT CONDITION"))))))
                           ((symbol-function 'superchat--input-refresh-terminal-groups)
                            (lambda (status)
                              (prog1 (funcall refresh status)
                                (cl-incf notifications)
                                (signal kind '("SECONDARY UI CONDITION"))))))
                   (condition-case err
                       (superchat-runtime-answer-input run (plist-get q :handle) "Paris")
                     ((error quit) (setq caught err))))
                 (should (equal caught (list kind "ORIGINAL INPUT CONDITION")))
                 (should (= 1 notifications))
                 (should (eq (if (eq origin 'audit) 'audit-incomplete 'failed)
                             (plist-get (superchat-runtime-status run) :status)))
                 (should (eq 'answer-not-delivered (plist-get (superchat-runtime-status run)
                                                            :input-continuation-blocked)))
                 (should (equal "Inactive" (widget-value (nth 1 (widget-get group :children)))))
                 (should (= 1 (length (funcall requests))))
                 (let ((events (superchat-tape-replay-events session)))
                   (should (= (if (eq origin 'audit) 0 1)
                              (cl-count "tool.completed" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
                   (should-not (cl-find "tool.failed" events :key (lambda (e) (plist-get e :type)) :test #'equal))))))))))))

(ert-deftest input-inline/deferred-hide-exact-view-session ()
  (dolist (mode '(async streaming))
    (dolist (change '(pending ended rename new-session dead replacement))
      (ert-info ((format "mode=%s change=%s" mode change))
        (input-panel-test--with-chat
         mode
         (lambda (view session requests reply)
           (human-input-test--send "Question")
           (let ((run (car (superchat-runtime-active-runs))))
             (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
             (let* ((q (superchat-runtime-pending-input run))
                    (group (superchat--input-panel-find view session run (plist-get q :handle)))
                    (hide (nth 4 (widget-get group :children)))
                    (name (buffer-name view)) deferred replacement)
               (unwind-protect
                   (progn
                     (save-excursion
                       (goto-char (widget-get hide :from))
                       (should (eq (key-binding (kbd "RET")) 'widget-button-press))
                       (cl-letf (((symbol-function 'run-at-time)
                                  (lambda (_when _repeat fn &rest args)
                                    (setq deferred (lambda () (apply fn args))))))
                         (execute-kbd-macro (kbd "RET"))))
                     (should deferred)
                     (should-not (widget-get group :input-hidden))
                     (pcase change
                       ('ended (superchat-runtime-cancel run "ended before Hide"))
                       ('rename (with-current-buffer view (rename-buffer (concat name " renamed") t)))
                       ('new-session (with-current-buffer view
                                       (setq-local superchat--session-id "new-session-before-hide")))
                       ((or 'dead 'replacement)
                        (kill-buffer view)
                        (when (eq change 'replacement)
                          (setq replacement (get-buffer-create name))
                          (with-current-buffer replacement
                            (org-mode)
                            (setq-local superchat--session-id session)
                            (insert "Replacement View")))))
                     (let* ((target (if (buffer-live-p view) view replacement))
                            (before (when target
                                      (with-current-buffer target
                                        (list (buffer-string)
                                              (copy-tree buffer-invisibility-spec)
                                              (append (car (overlay-lists)) (cdr (overlay-lists)))
                                              (point))))))
                       (funcall deferred)
                       (if (memq change '(pending ended rename))
                           (should (eq view (overlay-buffer (widget-get group :input-hidden))))
                         (should-not (widget-get group :input-hidden))
                         (when target
                           (with-current-buffer target
                             (should (equal before
                                            (list (buffer-string)
                                                  (copy-tree buffer-invisibility-spec)
                                                  (append (car (overlay-lists)) (cdr (overlay-lists)))
                                                  (point))))))))
                     (should (= 1 (length (funcall requests)))))
                 (when (buffer-live-p replacement) (kill-buffer replacement)))))))))))

(ert-deftest input-panel/hide-reopen-stop-and-original-conditions ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (let ((superchat-response-timeout 2))
       (human-input-test--send "Question"))
     (let ((run (car (superchat-runtime-active-runs))))
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let* ((q (superchat-runtime-pending-input run))
              (handle (plist-get q :handle))
              (panel (superchat--input-panel-find view session run handle))
              (owner (superchat-runtime-transport-timer-owner run))
              (clock-before (copy-superchat-llm--transport-timer-owner owner)))
         (dotimes (_ 3) (superchat--input-panel-refresh panel))
         (should (eq owner (superchat-runtime-transport-timer-owner run)))
         (should (numberp (superchat-llm--transport-timer-owner-hard-remaining owner)))
         (should (equal clock-before owner))
         (superchat--input-panel-action panel 'hide)
         (should (superchat-runtime-pending-input run))
         (let ((inhibit-read-only t)) (widget-delete panel))
         (with-current-buffer view (call-interactively #'superchat-input-reopen))
         (setq panel (superchat--input-panel-find view session run handle))
         (should (superchat--input-widget-live-p panel))
         (should (equal clock-before owner))
         (dolist (condition '((error "original" extra) (quit original)))
           (let (caught)
             (cl-letf (((symbol-function 'read-string)
                        (lambda (&rest _) (signal (car condition) (cdr condition)))))
               (condition-case value (superchat--input-panel-action panel 'answer)
                 ((error quit) (setq caught value))))
             (should (equal caught condition))
             (should (eq handle (plist-get (superchat-runtime-pending-input run) :handle)))))
         (superchat--input-panel-action panel 'stop)
         (should (plist-get (superchat-runtime-status run) :stop-requested))
         (should-not (superchat-runtime-pending-input run))
         (should (equal "Stopped" (widget-value (nth 1 (widget-get panel :children)))))
         (should (= 1 (length (funcall requests)))))))))

(ert-deftest input-panel/exact-view-after-minibuffer ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (dolist (mutation '(rename clear kill))
    (input-panel-test--with-chat
     'async
     (lambda (view session requests reply)
       (human-input-test--send "Question")
       (let ((run (car (superchat-runtime-active-runs))))
         (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
         (let* ((q (superchat-runtime-pending-input run))
                (panel (superchat--input-panel-find view session run (plist-get q :handle)))
                (answer (symbol-function 'superchat-runtime-answer-input))
                (answer-calls 0))
           (cl-letf (((symbol-function 'superchat-runtime-answer-input)
                      (lambda (&rest args) (cl-incf answer-calls) (apply answer args)))
                     ((symbol-function 'read-string)
                      (lambda (&rest _)
                        (with-current-buffer view
                          (pcase mutation
                            ('rename (rename-buffer (generate-new-buffer-name " *renamed-input*")))
                            ('clear (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                                      (superchat--clear-chat-and-context)))
                            ('kill (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                                     (superchat-session-workspace-close session)))))
                        "ANSWER")))
             (superchat--input-panel-action panel 'answer))
           (if (eq mutation 'rename)
               (progn (should (= 1 answer-calls))
                      (should (= 2 (length (funcall requests)))))
             (should (= 0 answer-calls))
             (should (= 1 (length (funcall requests)))))))))))

(ert-deftest input-panel/clear-declined-expired-and-foreign-controls ()
  (unless (fboundp 'test-input-batch--reply) (ert-skip "Requires native fixture"))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (human-input-test--send "Question")
     (let ((run (car (superchat-runtime-active-runs))))
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let* ((q (superchat-runtime-pending-input run))
              (panel (superchat--input-panel-find view session run (plist-get q :handle)))
              (text (buffer-string)) (draft superchat--prompt-start)
              (foreign (generate-new-buffer " *foreign input*")))
         (unwind-protect
             (progn
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                 (superchat--clear-chat-and-context))
               (should (equal-including-properties text (buffer-string)))
               (should (eq draft superchat--prompt-start))
               (should (eq (plist-get q :handle) (plist-get (superchat-runtime-pending-input run) :handle)))
               (should-error (superchat--input-presenter-config foreign session "input_batch_ask"))
               ;; A same-named replacement is not the original View object.
               (let ((name (buffer-name view)))
                 (with-current-buffer view (rename-buffer (generate-new-buffer-name " *original input*")))
                 (with-current-buffer foreign (rename-buffer name)
                                      (setq-local superchat--session-id session))
                 (should-not (superchat--input-current foreign q)))
               (superchat--input-panel-refresh panel)
               (should (= 1 (length (funcall requests)))))
           (kill-buffer foreign))))))
  (input-panel-test--with-chat
   'async
   (lambda (view session requests reply)
     (let ((superchat-runtime-default-deadline 0.1))
       (human-input-test--send "Expires"))
     (let ((run (car (superchat-runtime-active-runs))))
       (funcall reply (car (funcall requests)) '("input_batch_ask") "City?")
       (let* ((q (superchat-runtime-pending-input run))
              (panel (superchat--input-panel-find view session run (plist-get q :handle))))
         (should (superchat--input-widget-live-p panel))
         (sleep-for 0.15)
         (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
         (superchat--input-panel-refresh panel)
         (should (equal "Expired" (widget-value (nth 1 (widget-get panel :children)))))
         (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Stale panel opened minibuffer"))))
           (superchat--input-panel-action panel 'answer))
         (should (= 1 (length (funcall requests)))))))))

(defun input-panel-test--with-chat (mode body)
  "Run BODY with real M-x, native scheduler and fixture-only Request config.
The existing native fixture supplies ONLY transport replies.  Completion
hooks are isolated by the owned-View fixture: this is not Workflow/Plan
integration.  No model-visible tool or config is installed outside this test."
  (unless (and (featurep 'llm) (require 'llm-openai nil t)
               (fboundp 'test-input-batch--reply))
    (ert-skip "Requires installed llm and test-llm-backend native transport fixture"))
  (cmd-clear-test--with-owned-view
   (lambda (old _session)
     (let* ((superchat-llm-backend
             (make-llm-openai-compatible :url "http://invalid.test"
                                       :chat-model (if (eq mode 'streaming) "qwen/qwen3.8-27b" "input-ui-async")))
            ;; S08: the adapter rule grants both capabilities to any model
            ;; on the exact `llm-openai-compatible' type, so the async mode
            ;; needs its non-streaming behavior declared explicitly now.
            (superchat-llm-model-capability-overrides
             (unless (eq mode 'streaming)
               (list (list "input-ui-async"
                           :streaming nil :streaming-tool-use nil))))
            (superchat-llm-tools-enabled 'always)
            (superchat-response-timeout nil) (superchat-runtime-default-deadline nil)
            (superchat-show-ttft nil)
            (superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
            (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
            (superchat-llm-tools-list superchat-llm-tools-list)
            (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
            (test-input-batch--requests nil) (test-input-batch--authority-count 0)
            view session handoff)
       (save-window-excursion
         (switch-to-buffer old) (kill-buffer old)
         (call-interactively #'superchat)
         (setq view (current-buffer) session superchat--session-id)
         (setq-local superchat-authority-mode 'auto
                     superchat-authority--session-allowlist '("input_batch_ask"))
         (superchat-register-tool
          "input_batch_ask" :description "Fixture-only question"
          :args '((:name "value" :type string :description "question")) :async t
          :effects '((:effect local-read :resource "fixture-question"))
          :function (lambda (_done value)
                      (superchat-runtime--request-input
                       (plist-get superchat--tool-execution-context :input-settlement)
                       (list :question value))))
         (setq handoff
               (lambda (original request &optional submit-p)
                 (if (and (eq view (plist-get request :buffer))
                          (equal session (plist-get request :session-id)))
                     (let ((copy (copy-sequence request)))
                       (let* ((config (superchat--input-presenter-config view session "input_batch_ask"))
                              (stop (plist-get config :stop-consumer)))
                         (when input-panel-test--stop-observer
                           (setq config
                                 (plist-put config :stop-consumer
                                            (lambda (status)
                                              (funcall stop status)
                                              (funcall input-panel-test--stop-observer status)))))
                         (setq copy (plist-put copy :input-core config)))
                       (setq copy (plist-put copy :tools '("input_batch_ask")))
                       ;; An opaque published owner is not the Run id.
                       (setq copy (plist-put copy :transcript-owner (concat "ui-owner:" session)))
                       (funcall original copy submit-p))
                   (funcall original request submit-p))))
         (unwind-protect
             (progn
               (advice-add 'superchat--submit-chat-request :around handoff)
               (cl-letf (((symbol-function 'llm-request-plz-async)
                          (lambda (url &rest args)
                            (apply #'superchat--llm-request-plz-propagate-callback-errors
                                   (lambda (_url &rest actual)
                                     (push (append actual (list :source nil)) test-input-batch--requests)
                                     (when input-panel-test--on-request
                                       (funcall input-panel-test--on-request (car test-input-batch--requests)))
                                     nil)
                                   url args))))
                 (funcall body view session (lambda () test-input-batch--requests) #'test-input-batch--reply)))
           (advice-remove 'superchat--submit-chat-request handoff)
           (dolist (run (superchat-runtime-active-runs)) (superchat-runtime-cancel run "fixture cleanup"))
           (dolist (request test-input-batch--requests)
             (when-let* ((source (plist-get request :source)))
               (plz-event-source-close source)
               (when (buffer-live-p (oref source buffer)) (kill-buffer (oref source buffer)))))
           (dolist (buffer (buffer-list))
             (when (and (boundp 'superchat--input-view) (buffer-live-p buffer)
                        (eq (buffer-local-value 'superchat--input-view buffer) view))
               (kill-buffer buffer)))
           (maphash (lambda (id _state) (superchat-session-workspace-close id))
                    superchat-editor-workspace--states)))))))
(defvar superchat-workflow-directory)

(defmacro workflow-capf-test--with-recipes (&rest body)
  "Evaluate BODY with real isolated recipes and an editable prompt."
  (declare (indent 0) (debug t))
  `(let ((superchat-workflow-directory (make-temp-file "workflow-capf-" t)))
     (unwind-protect
         (progn
           (dolist (name '("review" "research" "中文"))
             (with-temp-file (expand-file-name (concat name ".workflow")
                                              superchat-workflow-directory)
               (insert "Review $input\n")))
           (with-temp-buffer
             (org-mode)
             (insert "* User: ")
             (setq-local superchat--prompt-start (point-marker))
             ,@body))
       (delete-directory superchat-workflow-directory t))))

(ert-deftest workflow-name/completion-quotes-name-not-arguments ()
  (workflow-capf-test--with-recipes
    (dolist (name '("weather report" "say \"hello\""))
      (with-temp-file (expand-file-name (concat name ".workflow") superchat-workflow-directory)
        (insert "Forecast $input\n")))
    (setq-local completion-at-point-functions '(superchat--completion-at-point))
    (dolist (pair '(("wea" . "\"weather report\"")
                    ("weather " . "\"weather report\"")
                    ("\"weather " . "\"weather report\"")
                    ("say" . "\"say \\\"hello\\\"\"")))
      (delete-region superchat--prompt-start (point-max))
      (insert "/workflow " (car pair))
      (completion-at-point)
      (should (equal (concat "/workflow " (cdr pair))
                     (buffer-substring-no-properties superchat--prompt-start (point-max))))
      (insert " 杭州")
      (should-not (superchat--completion-at-point)))))

(ert-deftest workflow-name/argument-file-completion-still-works ()
  (workflow-capf-test--with-recipes
    (insert "/workflow review #/tmp/")
    (let ((capf (superchat--completion-at-point)))
      (should capf)
      (should (eq #'completion-file-name-table (nth 2 capf)))
      (should (equal "/tmp/" (buffer-substring-no-properties (nth 0 capf) (nth 1 capf)))))))

(ert-deftest workflow-name/real-input-completion-and-both-prefixes ()
  (let ((superchat-workflow-directory (make-temp-file "workflow-name-send-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "天气  预报.workflow" superchat-workflow-directory)
            (insert "Forecast $input\n"))
          (dolist (prefix '("/workflow " ">>"))
            (human-input-test--with-chat
             (lambda (_view _session requests)
               (insert prefix)
               (if (equal prefix "/workflow ")
                   (progn (insert "天气") (completion-at-point))
                 (insert "\"天气  预报\""))
               (should-not (funcall requests))
               (insert " 杭州  tomorrow")
               (execute-kbd-macro (kbd "C-c C-c"))
               (should (= 1 (length (funcall requests))))
               (should (string-match-p "Forecast 杭州  tomorrow"
                                       (prin1-to-string (plist-get (car (funcall requests)) :prompt))))))))
      (delete-directory superchat-workflow-directory t))))

(ert-deftest workflow-capf/two-stages-and-name-only-bounds ()
  (workflow-capf-test--with-recipes
    (insert "/work")
    (let ((capf (superchat--completion-at-point)))
      (should (member "/workflow" (all-completions "/work" (nth 2 capf)))))
    (delete-region superchat--prompt-start (point-max))
    (insert "/workflow ")
    (let ((capf (superchat--completion-at-point)))
      (should capf)
      (should (= (nth 0 capf) (point)))
      (should (= (nth 1 capf) (point)))
      (should (eq t (plist-get (nthcdr 3 capf) :company-prefix-length)))
      (should (equal (sort (all-completions "" (nth 2 capf)) #'string<)
                     '("research" "review" "中文"))))
    (insert "rev")
    (let ((capf (superchat--completion-at-point)))
      (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                     "rev"))
      (should (equal (all-completions "rev" (nth 2 capf)) '("review"))))
    ;; Real completion inserts a name, never sends it or replaces the command.
    (setq-local completion-at-point-functions '(superchat--completion-at-point))
    (cl-letf (((symbol-function 'superchat-workflow-execute-async)
               (lambda (&rest _) (ert-fail "Completion executed a workflow")))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (&rest _) (ert-fail "Completion submitted a Run"))))
      (completion-at-point)
      (should (equal (buffer-substring-no-properties superchat--prompt-start (point-max))
                     "/workflow review")))
    (insert " explain this code")
    (should-not (superchat--completion-at-point))))

(ert-deftest workflow-capf/safe-live-discovery-and-unicode ()
  (workflow-capf-test--with-recipes
    (make-symbolic-link (expand-file-name "review.workflow" superchat-workflow-directory)
                       (expand-file-name "link.workflow" superchat-workflow-directory))
    (make-directory (expand-file-name "directory.workflow" superchat-workflow-directory))
    (insert "/workflow 中")
    (let* ((capf (superchat--completion-at-point))
           (names (all-completions "" (nth 2 capf))))
      (should (equal (all-completions "中" (nth 2 capf)) '("中文")))
      (should-not (member "link" names))
      (should-not (member "directory" names))
      (dolist (name names) (should (superchat-workflow--exists-p name))))
    (delete-file (expand-file-name "review.workflow" superchat-workflow-directory))
    (should-not (member "review" (all-completions "" (nth 2 (superchat--completion-at-point)))))))

(ert-deftest workflow-capf/does-not-complete-history-or-arguments ()
  (workflow-capf-test--with-recipes
    (dolist (input '("mention /workflow " "/workflows " "/workflow review "
                     "/workflow review explain" ">planning /workflow "))
      (delete-region superchat--prompt-start (point-max))
      (insert input)
      (should-not (superchat--completion-at-point)))
    (goto-char (point-min))
    (should-not (superchat--completion-at-point))))

(ert-deftest workflow-capf/empty-directory-gives-edit-guidance ()
  (workflow-capf-test--with-recipes
    (dolist (file (directory-files superchat-workflow-directory t "\\.workflow\\'"))
      (delete-file file))
    (insert "/workflow ")
    (let (feedback)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq feedback (apply #'format format-string args)))))
        (should-not (nth 2 (superchat--completion-at-point))))
      (should (string-match-p "superchat-workflow-edit" (or feedback ""))))
    (should (string-match-p "superchat-workflow-edit"
                           (plist-get (superchat--cmd-workflow "workflow" "" "" nil nil)
                                      :content)))))

(ert-deftest workflow-capf/completed-name-executes-only-on-send ()
  "Real chat completion and both command spellings reach the same recipe."
  (let ((superchat-workflow-directory (make-temp-file "workflow-capf-send-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "review.workflow" superchat-workflow-directory)
            (insert "Review $input\n"))
          (dolist (prefix '("/workflow " ">>"))
            (human-input-test--with-chat
             (lambda (_view session requests)
               (insert prefix "rev")
               (if (equal prefix "/workflow ")
                   (completion-at-point)
                 (insert "iew"))
               (should-not (funcall requests))
               (should-not (superchat-runtime-active-runs))
               (insert " TASK")
               (execute-kbd-macro (kbd "C-c C-c"))
               (should (= 1 (length (funcall requests))))
               (let* ((run-id (car (superchat-runtime-active-runs)))
                      (status (superchat-runtime-status run-id)))
                 (should (eq 'workflow (plist-get status :topology)))
                 (should (equal session (plist-get status :session-id))))
               (should (string-match-p
                        "Review TASK"
                        (prin1-to-string (plist-get (car (funcall requests)) :prompt))))))))
      (delete-directory superchat-workflow-directory t))))

(ert-deftest workflow-capf/first-use-lazily-loads-workflow ()
  "Fresh distribution stays lazy until Workflow name completion is requested."
  (let* ((root (file-name-directory (locate-library "superchat")))
         (directory (make-temp-file "workflow-capf-autoload-" t))
         (output (generate-new-buffer " *workflow-capf-autoload*")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "review.workflow" directory)
            (insert "Review $input\n"))
          (let ((status
                 (call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil output nil "-Q" "--batch" "-L" root
                  "--eval"
                  (prin1-to-string
                   `(progn
                      (setq superchat-data-directory ,directory)
                      (require 'superchat)
                      (when (featurep 'superchat-workflow) (error "Eager Workflow load"))
                      (setq superchat-workflow-directory ,directory)
                      (with-temp-buffer
                        (setq-local superchat--prompt-start (point-marker))
                        (insert "/workflow ")
                        (unless (equal '("review")
                                       (all-completions "" (nth 2 (superchat--completion-at-point))))
                          (error "Missing first-use completion")))
                      (unless (featurep 'superchat-workflow) (error "Workflow not loaded"))
                      (princ "WORKFLOW-CAPF-AUTOLOAD-OK"))))))
            (should (= status 0))
            (with-current-buffer output
              (should (string-match-p "WORKFLOW-CAPF-AUTOLOAD-OK" (buffer-string))))))
      (kill-buffer output)
      (delete-directory directory t))))

(defun human-input-test--with-chat (body &optional synchronous tools)
  "Run BODY through a fresh interactive View, with only native transport mocked."
  (unless (and (featurep 'llm) (require 'llm-openai nil t))
    (ert-skip "Requires isolated installed llm 0.31.1"))
  (cmd-clear-test--with-owned-view
   (lambda (view _session)
     (let ((superchat-llm-backend (make-llm-openai :key "unused" :chat-model "input-fixture"))
           ;; nil is the documented no-tools value.  The old `never' symbol
           ;; fell through to on-demand and now legitimately enables ask.
           (superchat-llm-tools-enabled (if tools 'always nil))
           (superchat-response-timeout nil)
           (superchat-runtime-default-deadline nil)
           (superchat-show-ttft nil)
           requests)
       (save-window-excursion
         (switch-to-buffer view)
         (kill-buffer view)
         (call-interactively #'superchat)
         (setq view (current-buffer))
         (cl-labels ((capture (prompt partial final failure multi)
                       (push (list :prompt prompt :partial partial :final final
                                   :failure failure :multi multi) requests)
                       (cond ((functionp synchronous) (funcall synchronous final))
                             (synchronous (funcall final "SYNC FINAL")))))
           (cl-letf (((symbol-function 'llm-chat-streaming)
                      (lambda (_provider prompt partial final failure &optional multi)
                        (capture prompt partial final failure multi)))
                     ((symbol-function 'llm-chat-async)
                      (lambda (_provider prompt final failure &optional multi)
                        (capture prompt nil final failure multi))))
             (unwind-protect
                 (funcall body view superchat--session-id (lambda () requests))
               (dolist (run-id (superchat-runtime-active-runs))
                 (superchat-runtime-cancel run-id "input fixture cleanup"))
               (when (buffer-live-p view)
                 (with-current-buffer view
                   (superchat--cancel-status-refresh-timer)
                   (setq-local kill-buffer-hook nil)))
               ;; M-x creates real Session tabs; close only this fixture's
               ;; isolated scopes through the public ownership authority.
               (maphash (lambda (session _state)
                          (superchat-session-workspace-close session))
                        superchat-editor-workspace--states)))))))))

(defun human-input-test--send (text)
  "Type TEXT and execute the actual chat key binding in the selected View."
  (should (eq (key-binding (kbd "C-c C-c")) 'superchat-send-input))
  (goto-char (point-max)) (insert text)
  (execute-kbd-macro (kbd "C-c C-c")))

(ert-deftest initial-assistant/visible-before-first-callback ()
  "The real send key leaves an Assistant between the question and next draft."
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (human-input-test--send "INITIAL QUESTION")
     (should (= 1 (length (funcall requests))))
     (let* ((text (buffer-string))
            (assistant (string-match "^\\*\\* Assistant:" text))
            (draft superchat--prompt-start))
       (message "INITIAL-ASSISTANT before-callback=%S" text)
       (should assistant)
       (should (< (string-match "INITIAL QUESTION" text) assistant))
       (should (< assistant draft))
       (should (< assistant (string-match "Streaming Response Mode" text)))
       (should-not (invisible-p (1+ assistant)))
       (should (get-text-property (1+ assistant) 'read-only))
       (should (= 2 (how-many "^\\* User:" (point-min) (point-max))))
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (insert (propertize "NEXT DRAFT" 'fixture-draft t))
       (goto-char (+ draft 2))
       (funcall (plist-get (car (funcall requests)) :final) "FINAL ANSWER")
       (output-prompt-test--assert-draft draft (propertize "NEXT DRAFT" 'fixture-draft t) 2)
       (should (get-text-property draft 'fixture-draft))
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (should (string-match-p "FINAL ANSWER" (buffer-string)))))))

(ert-deftest initial-assistant/native-tool-first-is-child ()
  "Native llm scheduling keeps tool records below the waiting Assistant."
  (human-input-test--with-chat
   (lambda (_view session requests)
     (setq-local superchat-authority--session-allowlist '("list_buffers"))
     (human-input-test--send "List the current buffers")
     (let* ((capture (car (funcall requests)))
            (prompt (plist-get capture :prompt))
            (draft superchat--prompt-start))
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (should-not (cl-find "tool.requested" (superchat-tape-replay-events session)
                            :key (lambda (event) (plist-get event :type)) :test #'equal))
       (insert (propertize "TOOL NEXT" 'fixture-draft t))
       (goto-char (+ draft 3))
       (llm-provider-utils-execute-tool-uses
        superchat-llm-backend prompt
        (list (make-llm-provider-utils-tool-use
               :id "initial-tool" :name "list_buffers" :args nil))
        (plist-get capture :multi) nil
       (plist-get capture :final) (plist-get capture :failure))
       (output-prompt-test--assert-draft draft (propertize "TOOL NEXT" 'fixture-draft t) 3)
       (should (= 2 (length (funcall requests))))
       (save-excursion
         (goto-char (point-min))
         ;; S21: the collapsible heading is `*** [status] NAME', not a
         ;; literal `Tool call:' line -- that text now lives inside the
         ;; example block under the same heading (still checked below).
         (re-search-forward "^\\*\\*\\* [✓✗⏳] list_buffers")
         (should (org-up-heading-safe))
         (should (looking-at "\\*\\* Assistant:"))
         (org-fold-hide-subtree)
         (save-excursion
           (re-search-forward "Tool result:")
           (should (invisible-p (1- (point)))))
         (should-not (invisible-p draft))
         (org-fold-show-subtree))
       ;; A second tool-only round keeps the existing segment close boundary;
       ;; it must not manufacture another empty Assistant between the tools.
       (setq capture (car (funcall requests)))
       (llm-provider-utils-execute-tool-uses
        superchat-llm-backend (plist-get capture :prompt)
        (list (make-llm-provider-utils-tool-use
               :id "initial-tool-2" :name "list_buffers" :args nil))
        (plist-get capture :multi) nil
        (plist-get capture :final) (plist-get capture :failure))
       (should (= 3 (length (funcall requests))))
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (save-excursion
         (goto-char (point-min))
         ;; S21: the collapsible heading is `*** [status] NAME', not a
         ;; literal `Tool call:'/`Tool result:' line.
         (while (re-search-forward "^\\*\\*\\* [✓✗⏳] list_buffers" nil t)
           (save-excursion
             (should (org-up-heading-safe))
             (should (looking-at "\\*\\* Assistant:")))))
       (funcall (plist-get (car (funcall requests)) :final) "TOOL FINAL")
       (output-prompt-test--assert-draft draft (propertize "TOOL NEXT" 'fixture-draft t) 3)
       (should (get-text-property draft 'fixture-draft))
       ;; S21: the final close still writes only the ONE heading this
       ;; whole multi-round turn has had all along -- never a second one.
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (let ((events (superchat-tape-replay-events session)))
         (should (= 2 (cl-count "tool.completed" events
                                :key (lambda (event) (plist-get event :type))
                                :test #'equal)))
         (should (= 1 (cl-count "run.completed" events
                                :key (lambda (event) (plist-get event :type)) :test #'equal)))
         (dolist (value (list prompt events superchat--conversation-history))
           (should-not (string-match-p "TOOL NEXT" (prin1-to-string value)))))))
   nil t))

(ert-deftest initial-assistant/first-output-and-terminal-matrix ()
  "Initial presentation survives partial, reasoning and pre-token terminals."
  (dolist (mode '(text reasoning callback cancel failure synchronous))
    (human-input-test--with-chat
     (lambda (_view session requests)
       (human-input-test--send "MATRIX QUESTION")
       (let* ((capture (car (funcall requests)))
              (run superchat--active-run-id)
              (marker superchat--prompt-start)
              (draft (propertize "MATRIX DRAFT\nSECOND" 'fixture-draft t)))
         (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
         (insert draft) (goto-char (+ marker 4))
         (output-prompt-test--fontify marker)
         (setq draft (buffer-substring marker (point-max)))
         (pcase mode
           ('text (funcall (plist-get capture :partial) "PARTIAL BODY"))
           ('reasoning (funcall (plist-get capture :partial) '(:reasoning "PRIVATE THINKING"))))
         (output-prompt-test--assert-draft marker draft 4)
         (pcase mode
           ('cancel (superchat-runtime-cancel run "BEFORE TOKEN CANCEL"))
           ('failure (funcall (plist-get capture :failure) 'error "BEFORE TOKEN FAILURE"))
           ('synchronous nil)
           (_ (funcall (plist-get capture :final) "MATRIX FINAL")))
         (output-prompt-test--assert-draft marker draft 4)
         (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
         (should (= 2 (how-many "^\\* User:" (point-min) (point-max))))
         (dolist (value (list (plist-get capture :prompt) superchat--conversation-history
                              (superchat-tape-replay-events session)))
           (should-not (string-match-p "MATRIX DRAFT" (prin1-to-string value))))
         (unless (memq mode '(cancel failure synchronous))
           (should (string-match-p "MATRIX FINAL" (buffer-string))))))
     (eq mode 'synchronous))))

(ert-deftest initial-assistant/custom-owner-and-repeated-begin ()
  "Default status addresses the published custom owner without reselecting it."
  (human-input-test--with-chat
   (lambda (view session requests)
     (let* ((handle (superchat-runtime-test-submit
                     (list :prompt "CUSTOM QUESTION" :buffer view :session-id session
                           :tools 'none :deadline nil :transcript-owner "INITIAL-CUSTOM")))
            (owner (plist-get (superchat-runtime-status (plist-get handle :run-id))
                             :transcript-owner)))
       (should (equal owner "INITIAL-CUSTOM"))
       (should-not (assoc (plist-get handle :run-id) superchat--response-region-states))
       (should (= 1 (how-many "^\\*\\* Assistant:" (point-min) (point-max))))
       (superchat--response-region-begin "OTHER-OWNER")
       (superchat--response-region-stream "OTHER BODY" "OTHER-OWNER")
       (let ((before (buffer-string))
             (projection superchat--response-region-current-owner)
             (marker superchat--assistant-response-start-marker))
         (let ((superchat--runtime-transcript-owner "WRONG-AMBIENT"))
           (superchat--response-region-begin owner t))
         (should (equal-including-properties before (buffer-string)))
         (should (equal projection superchat--response-region-current-owner))
         (should (eq marker superchat--assistant-response-start-marker)))
       (funcall (plist-get (car (funcall requests)) :final) "CUSTOM FINAL")
       (should (string-match-p "CUSTOM FINAL" (buffer-string)))
       (should (string-match-p "OTHER BODY" (buffer-string)))
       (should (equal "OTHER-OWNER" superchat--response-region-current-owner))))))

(ert-deftest human-input/fresh-and-reopen-one-prompt ()
  (human-input-test--with-chat
   (lambda (_view _session _requests)
     (should (= 1 (how-many "^\\* User" (point-min) (point-max))))
     (let ((marker superchat--prompt-start))
       (insert "draft") (goto-char (+ marker 2))
       (call-interactively #'superchat)
       (output-prompt-test--assert-draft marker "draft" 2)
       (should (= 1 (how-many "^\\* User" (point-min) (point-max))))))))

(ert-deftest cancel-draft/real-replacement-not-concatenation ()
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (human-input-test--send "QUESTION")
     (let ((marker (superchat--response-region--view-prompt)))
       (goto-char marker) (insert "DRAFT")
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "EDIT")))
         (execute-kbd-macro (kbd "C-c C-k")))
       (should (equal "EDIT" (buffer-substring-no-properties marker (point-max))))
       (should (= 1 (length (funcall requests))))))))

(ert-deftest cancel-draft/real-prefill-replace-matrix ()
  "Cancel first, then edit one exact visible draft in both transport modes."
  (dolist (streaming '(t nil))
    (dolist (answer '(unchanged "REPLACED" "" "  MULTI\nLINE  "))
      (let ((superchat-llm-streaming streaming))
        (human-input-test--with-chat
         (lambda (_view session requests)
           (human-input-test--send "CANCEL QUESTION")
           (let* ((request (car (funcall requests)))
                  (run superchat--active-run-id)
                  (marker (superchat--response-region--view-prompt))
                  (draft (propertize "PRIVATE-DRAFT\nTAIL" 'draft-property t))
                  initial reads)
             (goto-char marker) (insert draft) (goto-char (+ marker 3))
             (output-prompt-test--fontify marker)
             (setq draft (buffer-substring marker (point-max)))
             (cl-letf (((symbol-function 'read-string)
                        (lambda (_prompt &optional value &rest _)
                          (setq initial value reads (1+ (or reads 0)))
                          (should (eq 'cancelled
                                      (plist-get (superchat-runtime-status run) :status)))
                          (if (eq answer 'unchanged) value answer))))
               (should (eq (key-binding (kbd "C-c C-k")) 'superchat-cancel))
               (execute-kbd-macro (kbd "C-c C-k")))
             (should (= reads 1))
             (should (equal initial (substring-no-properties draft)))
             (should (eq marker (superchat--response-region--view-prompt)))
             (if (eq answer 'unchanged)
                 (output-prompt-test--assert-draft marker draft 3)
               (should (equal answer (buffer-substring-no-properties marker (point-max)))))
             (should (= 1 (length (funcall requests))))
             (should-not (superchat-runtime-active-runs))
             (should (= 1 (cl-count "run.cancelled" (superchat-tape-replay-events session)
                                    :key (lambda (e) (plist-get e :type)) :test #'equal)))
             (dolist (value (list (plist-get request :prompt) superchat--conversation-history
                                  (superchat-db-tape-replay session)
                                  (superchat-tape-replay-events session)))
               (should-not (string-match-p "PRIVATE-DRAFT\\|REPLACED\\|MULTI"
                                           (prin1-to-string value))))
             (let ((before (buffer-string)))
               (when (plist-get request :partial)
                 (funcall (plist-get request :partial) "LATE OLD"))
               (funcall (plist-get request :final) "LATE FINAL")
               (should (equal-including-properties before (buffer-string)))))))))))

(ert-deftest cancel-draft/real-edit-condition-preserves-draft ()
  (dolist (condition '(error quit))
    (human-input-test--with-chat
     (lambda (_view _session requests)
       (human-input-test--send "CANCEL QUESTION")
       (let ((run superchat--active-run-id)
             (marker (superchat--response-region--view-prompt))
             (draft (propertize "PRIVATE-DRAFT" 'draft-property t)) caught)
         (goto-char marker) (insert draft) (goto-char (+ marker 2))
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _) (signal condition '("original redirect" 17)))))
           (condition-case err
               (execute-kbd-macro (kbd "C-c C-k"))
             ((error quit) (setq caught err))))
         (should (equal caught (cons condition '("original redirect" 17))))
         (output-prompt-test--assert-draft marker draft 2)
         (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
         (should (= 1 (length (funcall requests)))))))))

(ert-deftest cancel-draft/no-active-run-does-not-open-editor ()
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (let ((marker (superchat--response-region--view-prompt)))
       (goto-char marker) (insert "UNSENT")
       (let ((before (buffer-string)))
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _) (ert-fail "No Run: no redirect editor"))))
           (execute-kbd-macro (kbd "C-c C-k")))
         (should (equal-including-properties before (buffer-string)))
         (should-not (funcall requests)))))))

(ert-deftest cancel-draft/real-reentry-never-retargets ()
  (dolist (stage '(cancel read))
    (dolist (mutation '(clear kill close session prompt rename))
      (human-input-test--with-chat
       (lambda (view _session requests)
         (human-input-test--send "CANCEL QUESTION")
         (let ((run superchat--active-run-id)
               (marker (superchat--response-region--view-prompt))
               (name (buffer-name view)) foreign mutated reads snapshot)
           (goto-char marker) (insert "ORIGINAL DRAFT")
           (cl-labels
               ((mutate ()
                  (setq mutated t)
                  (with-current-buffer view
                    (pcase mutation
                      ('clear (superchat--clear-chat-and-context))
                      ((or 'kill 'close)
                       (if (eq mutation 'close)
                           (superchat-session-workspace-close superchat--session-id)
                         (kill-buffer view))
                       (should-not (buffer-live-p view))
                       (setq foreign (get-buffer-create name))
                       (with-current-buffer foreign (insert "FOREIGN")))
                      ('session (setq-local superchat--session-id "replacement-session"))
                      ('prompt (superchat--response-region-consume-prompt)
                               (superchat--response-region-ensure-view-prompt))
                      ('rename (rename-buffer (generate-new-buffer-name "*renamed-cancel*")))))
                  (setq snapshot (with-current-buffer (or foreign view) (buffer-string)))))
             (unwind-protect
                 (let ((superchat-turn-completed-functions
                        (when (eq stage 'cancel)
                          (list (lambda (status)
                                  (when (equal run (plist-get status :run-id)) (mutate)))))))
                   (cl-letf (((symbol-function 'read-string)
                              (lambda (&rest _)
                                (setq reads (1+ (or reads 0)))
                                (when (eq stage 'read) (mutate))
                                "STALE REDIRECT")))
                     (execute-kbd-macro (kbd "C-c C-k")))
                   (should mutated)
                   (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
                   (should (= 1 (length (funcall requests))))
                   (with-current-buffer (or foreign view)
                     (if (eq mutation 'rename)
                         (should (equal "STALE REDIRECT"
                                        (buffer-substring-no-properties marker (point-max))))
                       (should (equal-including-properties snapshot (buffer-string)))))
                   (when (and (eq stage 'cancel) (not (eq mutation 'rename)))
                     (should-not reads)))
               (when (buffer-live-p foreign) (kill-buffer foreign))))))))))

(ert-deftest human-input/ordinary-draft-terminal-matrix ()
  "S05b/D7: sending while ordinary chat has an active Run now queues the
draft (Runtime records one pending follow-up, the prompt is consumed, a
fresh empty prompt replaces it) instead of retaining it inertly; the Run's
own terminal event -- final, cancel, or error -- dispatches the queued
draft on its own, with no second manual send."
  (dolist (terminal '(final cancel error))
    (human-input-test--with-chat
     (lambda (_view session requests)
       (human-input-test--send "FIRST QUESTION")
       (should (= 1 (length (funcall requests))))
       (let* ((first (car (funcall requests)))
              (run superchat--active-run-id)
              (marker (superchat--response-region--view-prompt))
              (draft (propertize "PRIVATE-DRAFT\nSECOND" 'human-draft t)))
         (should marker)
         (goto-char marker) (insert draft) (goto-char (+ marker 3))
         (output-prompt-test--fontify marker)
         (setq draft (buffer-substring marker (point-max)))
         (funcall (plist-get first :partial) '(:reasoning "THINKING"))
         (funcall (plist-get first :partial) "PARTIAL")
         (output-prompt-test--assert-draft marker draft 3)
         (let (messages)
           (cl-letf (((symbol-function 'message)
                      (lambda (format-string &rest args)
                        (push (apply #'format format-string args) messages))))
             (execute-kbd-macro (kbd "C-c C-c")))
           (should (= 1 (length (funcall requests))))
           (should (= 1 (superchat-runtime-pending-follow-ups session)))
           (should (cl-some (lambda (m) (string-prefix-p "Queued (1 pending)" m))
                             messages)))
         ;; The draft's own marker is detached by the consume; the prompt
         ;; the user now sees is a different, fresh one.
         (should-not (eq marker superchat--prompt-start))
         (should (equal (list run) (superchat-runtime-active-runs)))
         (goto-char (point-min))
         (let ((point (point)))
           (funcall (plist-get first :partial) "PARTIAL MORE")
           (should (= point (point))))
         (pcase terminal
           ('final (funcall (plist-get first :final) "FINAL"))
           ('cancel (superchat-runtime-cancel run "human-input test cancel"))
           ('error (funcall (plist-get first :failure) 'error "LOCAL ERROR")))
         ;; Runtime's follow-up queue dispatches on a zero-delay timer once
         ;; the Run ends; pump the event loop for it.
         (let ((deadline (+ (float-time) 2.0)))
           (while (and (< (length (funcall requests)) 2) (< (float-time) deadline))
             (sit-for 0.01)))
         (should (= 2 (length (funcall requests))))
         (should (= 0 (superchat-runtime-pending-follow-ups session)))
         (should (string-match-p
                  "PRIVATE-DRAFT"
                  (prin1-to-string (plist-get (car (funcall requests)) :prompt)))))))))

(ert-deftest human-input/clear-confirm-before-consume ()
  (dolist (answer '(nil t))
    (human-input-test--with-chat
     (lambda (view session requests)
       (human-input-test--send "FIRST")
       (let ((first (car (funcall requests)))
             (run superchat--active-run-id) confirms)
         (funcall (plist-get first :partial) "OLD PARTIAL")
         (should (superchat--response-region--view-prompt))
         (goto-char (point-max)) (insert "/clear")
         (let ((before (buffer-string)) (point (point))
               (marker superchat--prompt-start)
               (events (superchat-tape-replay-events session)))
           (cl-letf (((symbol-function 'yes-or-no-p)
                      (lambda (&rest _) (push t confirms) answer)))
             (execute-kbd-macro (kbd "C-c C-c")))
           (should (= 1 (length confirms)))
           (if (not answer)
               (progn
                 (should (equal-including-properties before (buffer-string)))
                 (should (= point (point))) (should (eq marker superchat--prompt-start))
                 (should (equal events (superchat-tape-replay-events session)))
                 (should (member run (superchat-runtime-active-runs))))
             (should (eq view (current-buffer)))
             (should-not (equal session superchat--session-id))
             (let ((fresh (buffer-string)))
               (funcall (plist-get first :partial) "LATE OLD")
               (funcall (plist-get first :final) "LATE FINAL")
               (should (equal-including-properties fresh (buffer-string))))
             (human-input-test--send "NEW SESSION")
             (should (= 2 (length (funcall requests)))))))))))

(ert-deftest human-input/synchronous-final-before-submit-return ()
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (human-input-test--send "FIRST")
     (should (superchat--response-region--view-prompt))
     (should-not (superchat-runtime-active-runs))
     (human-input-test--send "SECOND")
     (should (= 2 (length (funcall requests))))
     (should (= 3 (how-many "^\\* User" (point-min) (point-max))))
     (should (= 2 (how-many "^\\*\\* Assistant" (point-min) (point-max))))) t))

(ert-deftest human-input/reentry-before-submit-returns ()
  "S05b/D7: the reentrant send used to match the Runtime-public-but-untracked
Run and retain its draft inertly; it now queues it instead (Runtime still
finds the Run active by transcript-owner even though the outer UI has not
tracked its handle yet).  This fixture's synchronous completion recurses
into another real send on every delivery (including a later auto-dispatch
of this same queued item), so the assertions stop before letting Runtime's
zero-delay follow-up timer fire, and the queue is cleared through the
public API afterward rather than relying on this fixture's own cleanup."
  (human-input-test--with-chat
   (lambda (_view session requests)
     (human-input-test--send "FIRST")
     (should-not (superchat-runtime-active-runs))
     (should (equal "" (buffer-substring superchat--prompt-start (point-max))))
     (should (= 1 (length (funcall requests))))
     (should (= 1 (superchat-runtime-pending-follow-ups session)))
     (superchat-runtime-clear-follow-ups session 'test-cleanup))
   (lambda (final)
     ;; Runtime is already public but the outer UI has not tracked its handle.
     (call-interactively #'superchat)
     (human-input-test--send "REENTERED")
     (funcall final "SYNC FINAL"))))

(ert-deftest human-input/unrelated-same-session-run-does-not-block-view ()
  "An unrelated Run sharing this Session but owned by another buffer/transcript
does not block or queue a plain send here; once this View has its own active
Run, S05b/D7 queues a further send instead of retaining it inertly."
  (human-input-test--with-chat
   (lambda (view session requests)
     (with-temp-buffer
       (let ((other (current-buffer)))
         (superchat-runtime-test-submit
          (list :buffer other :session-id session :prompt "UNRELATED"
                :tools 'none :deadline nil :transcript-owner "OTHER-CUSTOM")))
       (with-current-buffer view
         (should (= 1 (length (funcall requests))))
         (human-input-test--send "VIEW QUESTION")
         (should (= 2 (length (funcall requests))))
         (let ((marker superchat--prompt-start))
           (human-input-test--send "VISIBLE DRAFT")
           (should (= 2 (length (funcall requests))))
           (should (= 1 (superchat-runtime-pending-follow-ups session)))
           (should-not (eq marker superchat--prompt-start))))))))

(ert-deftest human-input/native-tool-round-preserves-visible-draft ()
  (unless (and (featurep 'llm) (require 'llm-provider-utils nil t))
    (ert-skip "Requires isolated installed llm 0.31.1"))
  (dolist (async '(nil t))
    (let ((superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
          (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
          (superchat-llm-tools-list superchat-llm-tools-list)
          (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
          ;; Default policy, deliberately NOT bound to `all' (S09-fix-5 item
          ;; 3, restoring the S09-fix-4 workaround): the user decision
          ;; amending contract §4.2 rule 3 makes every plugin-registered
          ;; tool part of the default baseline regardless of
          ;; `superchat-llm-tool-names', so this plugin tool -- registered
          ;; below via the public `superchat-register-tool' seam, outside
          ;; the default five-name allowlist -- must reach the model under
          ;; the plain, unconfigured default.  `superchat-register-tool'
          ;; already forces a fresh `superchat-llm-tools-list' rebuild (it
          ;; resets the cache to nil), so this reaches the real registry
          ;; query without a separate signature reset.
          (name "human_input_read") pending (effects 0))
      (unwind-protect
          (progn
            (superchat-register-tool
             name :description "Input fixture local read" :args nil :async async
             :effects '((:effect local-read :resource "input-fixture"))
             :function (if async
                           (lambda (done) (cl-incf effects) (setq pending done))
                         (lambda () (cl-incf effects) "TOOL RESULT")))
            (human-input-test--with-chat
             (lambda (_view session requests)
               (setq-local superchat-authority--session-allowlist (list name))
               (human-input-test--send "USE TOOL")
               (let* ((first (car (funcall requests)))
                      (run superchat--active-run-id)
                      (marker superchat--prompt-start)
                      (draft (propertize "UNSENT TOOL DRAFT" 'human-draft t)))
                 (goto-char marker) (insert draft) (goto-char (+ marker 2))
                 (llm-provider-utils-execute-tool-uses
                  superchat-llm-backend (plist-get first :prompt)
                  (list (make-llm-provider-utils-tool-use :id "input-call" :name name :args nil))
                  (plist-get first :multi) nil (plist-get first :final) (plist-get first :failure))
                 (output-prompt-test--assert-draft marker draft 2)
                 (when async
                   (should (functionp pending)) (funcall pending "TOOL RESULT"))
                 (output-prompt-test--assert-draft marker draft 2)
                 (should (= 1 effects))
                 (should (= 2 (length (funcall requests))))
                 (funcall (plist-get (car (funcall requests)) :final) "TOOL FINAL")
                 (output-prompt-test--assert-draft marker draft 2)
                 (should (string-match-p "TOOL RESULT" (buffer-string)))
                 (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
                 (let ((events (superchat-tape-replay-events session)))
                   (should (= 1 (cl-count "tool.completed" events
                                         :key (lambda (event) (plist-get event :type)) :test #'equal)))
                   (should (eq 'proved (plist-get (superchat-closure-judge
                                                  (list :kind 'durable-run-terminal :run-id run)
                                                  events) :verdict)))
                   (should-not (string-match-p "UNSENT TOOL DRAFT" (prin1-to-string events))))
                 (should-not (string-match-p "UNSENT TOOL DRAFT"
                                             (prin1-to-string (mapcar (lambda (r) (plist-get r :prompt))
                                                                    (funcall requests)))))))
             nil t))
        (superchat-unregister-tool name)))))

(ert-deftest cmd/clear-rotates-session-and-clears-view-state ()
  "`/clear' starts a new Tape context in addition to clearing the View."
  (with-temp-buffer
    (setq-local superchat-mode t)
    (setq-local superchat-buffer-name (buffer-name))
    (setq-local superchat--session-id "clear-before-session")
    (setq-local superchat--conversation-history '("old turn"))
    (setq-local superchat--current-command "old-command")
    (setq-local superchat--pending-recalled-memories
                '((:content "stale-recall")))
    (let ((superchat--current-context-files '("/tmp/old-context.txt"))
          (superchat-data-directory (make-temp-file "superchat-clear-test" t))
          (before superchat--session-id))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                       (lambda () nil)))
              (superchat--clear-chat-and-context))
            (should (stringp superchat--session-id))
            (should-not (equal before superchat--session-id))
            (should-not superchat--conversation-history)
            (should-not superchat--current-command)
            (should-not superchat--pending-recalled-memories)
            (should-not superchat--current-context-files)
            (should (string-match-p "\\`#\\+TITLE: superchat"
                                    (buffer-string))))
        (delete-directory superchat-data-directory t)))))

(ert-deftest cmd/clear-skips-delivery-from-the-old-session-view ()
  "A Run from before `/clear' must not render into the new View.

Rewritten for S01: this used to poke the deleted `superchat-runtime--deliver'
directly with a synthetic, buffer-less entry.  It now submits a real Run
into the View while it is Session A, switches the View to Session B the way
`/clear' actually does, then delivers the old Run's terminal through the
real observer.  R3-L7 keeps the durable result: the Run still seals
`completed' on Session A's Tape even though nothing renders into the View,
which now belongs to Session B."
  (cmd-clear-test--with-owned-view
   (lambda (view session-a)
     (let (observer run-id)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _) (setq observer superchat--llm-runtime-observer))))
         (setq run-id
               (plist-get
                (superchat-runtime-test-submit
                 (list :prompt "OLD QUESTION" :buffer view :session-id session-a
                       :tools 'none :deadline nil))
                :run-id)))
       ;; This View is EditorWorkspace-owned, so the full interactive
       ;; `superchat--clear-chat-and-context' would ask to cancel the still-
       ;; active Run from Session A before rolling over — exactly the
       ;; opposite of what this test needs (a late terminal arriving after
       ;; the View has already moved on).  Use the lower-level primitive the
       ;; ruling names instead: the same View-reset transaction, publishing
       ;; the new Session ID directly, without EditorWorkspace's ownership
       ;; rollover or its cancellation prompt.
       (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                  (lambda () nil)))
         (superchat--clear-view-transaction
          (lambda () (setq superchat--session-id (superchat--new-session-id)))
          nil))
       (should-not (equal session-a superchat--session-id))
       (funcall observer 'terminal '(:status completed :response "OLD ASSISTANT ANSWER"))
       (should-not (string-match-p "OLD ASSISTANT ANSWER" (buffer-string)))
       (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status)))
       (let ((types (mapcar (lambda (e) (plist-get e :type))
                            (superchat-tape-replay-events session-a))))
         (should (member "response.completed" types))
         (should (member "run.completed" types)))))))

(ert-deftest cmd/clear-resets-response-region-before-next-run ()
  "`/clear' detaches the old region and leaves the next Run uncorrupted."
  (let ((directory (make-temp-file "superchat-clear-region-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((superchat-buffer-name (buffer-name))
                (superchat-data-directory directory))
            (setq-local superchat-mode t
                        superchat--session-id "clear-region-before"
                        superchat--conversation-history nil
                        superchat--current-response-parts nil)
            (insert "* User: old\n")
            (superchat--prepare-for-response)
            (superchat--update-status "OLD-THINKING")
            (superchat--stream-llm-result "OLD-RAW")
            (superchat--insert-persistent-system-message "OLD-NOTICE")
            (let ((old-status superchat--response-start-marker)
                  (old-start superchat--assistant-response-start-marker)
                  (old-end superchat--assistant-response-end-marker)
                  (old-notice superchat--response-region-persistent-marker)
                  (old-prompt superchat--prompt-start))
              (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                         #'ignore))
                (superchat--clear-chat-and-context))
              (dolist (marker (list old-status old-start old-end old-notice
                                     old-prompt))
                (should-not (and (markerp marker) (marker-position marker))))
              (should-not superchat--response-start-marker)
              (should-not superchat--assistant-response-start-marker)
              (should-not superchat--assistant-response-end-marker)
              (should-not superchat--response-region-persistent-marker)
              (should-not superchat--current-response-parts)
              (should (string-match-p "\\`#\\+TITLE: superchat" (buffer-string)))

              (goto-char (point-max))
              (insert "NEXT-INPUT")
              (setq superchat--prompt-start nil)
              (superchat--prepare-for-response)
              (superchat--update-status "NEXT-THINKING")
              (superchat--stream-llm-result "NEXT-RAW")
              (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
                (superchat--process-llm-result "NEXT-FINAL"))
              (let ((text (buffer-string)))
                (should-not (string-match-p "NEXT-RAW" text))
                (should (string-match-p "NEXT-FINAL" text))
                (should (< (string-match "#\\+TITLE" text)
                           (string-match "NEXT-FINAL" text)))
                (should-not superchat--assistant-response-start-marker)
                (should-not superchat--assistant-response-end-marker)))))
      (delete-directory directory t))))

(ert-deftest cmd/session-view-rebuild-resets-response-region ()
  "Rebuilding a Session View detaches its old response-region markers."
  (let* ((directory (make-temp-file "superchat-session-view-region-test-" t))
         (db-file (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (session-id "session-view-region")
         (buffer-name (superchat--session-buffer-name session-id))
         old-markers)
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (superchat-tape-append-material session-id "user" "persisted user")
          (superchat-tape-append-material
           session-id "assistant" "persisted assistant")
          (let ((buffer (superchat--ensure-chat-view buffer-name session-id)))
            (let ((superchat-buffer-name buffer-name))
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (setq-local superchat--response-start-marker nil
                              superchat--assistant-response-start-marker nil
                              superchat--assistant-response-end-marker nil
                              superchat--response-region-persistent-marker nil
                              superchat--current-response-parts nil
                              superchat--prompt-start nil)
                  (insert "* User: stale view\n")
                  (superchat--prepare-for-response)
                  (superchat--stream-llm-result "STALE-RAW")
                  (superchat--insert-persistent-system-message "STALE-NOTICE")
                  (setq old-markers
                        (list superchat--response-start-marker
                              superchat--assistant-response-start-marker
                              superchat--assistant-response-end-marker
                              superchat--response-region-persistent-marker
                              superchat--prompt-start)))
              (let ((rebuilt (superchat--render-session-view session-id)))
                (with-current-buffer rebuilt
                  (dolist (marker old-markers)
                    (should-not (and (markerp marker)
                                     (marker-position marker))))
                  (should-not superchat--response-start-marker)
                  (should-not superchat--assistant-response-start-marker)
                  (should-not superchat--assistant-response-end-marker)
                  (should-not superchat--response-region-persistent-marker)
                  (should-not superchat--current-response-parts)
                  (should (markerp superchat--prompt-start))
                  (should (string-match-p "persisted user" (buffer-string)))
                  (should (string-match-p "persisted assistant" (buffer-string)))
                  (should-not (string-match-p "STALE-RAW" (buffer-string)))
                  (should-not (string-match-p "STALE-NOTICE" (buffer-string)))))))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest cmd/completion-follows-live-command-sources ()
  "Completion and `/commands' derive from the sources used by dispatch."
  (let ((names (superchat--get-all-command-names))
        (display (superchat--list-commands-as-string)))
    ;; `/review' comes from the optional package (S26), not the kernel alist.
    (dolist (command '("workflow" "code" "research"
                       "cowork" "single" "authority" "extensions"))
      (should (member (concat "/" command) names)))
    (dolist (source (list superchat--command-alist
                          superchat--builtin-commands))
      (dolist (command source)
        (should (member (concat "/" (car command)) names))
        (should (string-match-p
                 (regexp-quote (concat "/" (car command)))
                 display))))))

(defun test-cmd-dispatch--workflow-error-message (input)
  "Return the workflow dispatch message produced for INPUT.
The full input pipeline runs inside a temporary buffer; only final result
rendering and memory recall are replaced so this fixture cannot touch the
shared chat view or database during file loading."
  (let ((workflow-directory
         (make-temp-file "superchat-workflow-negative-" t))
        captured-content)
    (unwind-protect
        (with-temp-buffer
          (setq-local superchat--session-id "workflow-negative-session"
                      superchat--prompt-start (copy-marker (point-min)))
          (insert input)
          (let ((superchat-workflow-directory workflow-directory))
            (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'superchat--dispatch-result)
                       (lambda (result _lang _target-model)
                         (setq captured-content
                               (plist-get result :content)))))
              (superchat-send-input)))
          captured-content)
      (delete-directory workflow-directory t))))

(ert-deftest workflow/missing-ascii-name-reports-no-such-workflow ()
  "An absent ASCII workflow name reports the name-specific error."
  (should (equal
           "No such workflow: batch2b-missing"
           (test-cmd-dispatch--workflow-error-message ">>batch2b-missing"))))

(ert-deftest workflow/non-ascii-name-reports-no-such-workflow ()
  "A missing non-ASCII workflow name reaches the normal missing-file path."
  (should (equal
           "No such workflow: 不存在的名字"
           (test-cmd-dispatch--workflow-error-message ">>不存在的名字"))))

(ert-deftest chat/request-submit-validates-and-borrows-exact-session-view ()
  "Chat submission validates the exact View before Runtime sees it."
  (let (validations submitted)
    (with-temp-buffer
      (setq-local superchat--session-id "chat-exact-session")
      (cl-letf (((symbol-function 'superchat-extension-compose-request)
                 (lambda (request &optional guard)
                   (when guard (funcall guard request))
                   request))
                ((symbol-function 'superchat-editor-workspace-validate-buffer)
                 (lambda (buffer session-id)
                   (push (list buffer session-id) validations)
                   buffer))
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (setq submitted request)
                   '(:run-id "chat-exact-run" :status accepted))))
        (superchat-execute-request-plan
         (list :type :llm-query
               :prompt "hello"
               :session-id superchat--session-id
               :buffer (current-buffer))))
      ;; S10: chat submission now also crosses `superchat-pipeline-submit',
      ;; whose own host-buffer resolution (`superchat-pipeline--host') is a
      ;; third validation of the exact same View/Session pair (contract §4,
      ;; plan §1.1) -- not a second router, the same authority checked once
      ;; more at the pipeline boundary.
      (should (equal (nreverse validations)
                     (list (list (current-buffer) "chat-exact-session")
                           (list (current-buffer) "chat-exact-session")
                           (list (current-buffer) "chat-exact-session"))))
      (should (eq (plist-get submitted :buffer) (current-buffer)))
      (should-not (plist-member submitted :ephemeral-buffer)))))

(ert-deftest chat/extension-cannot-retarget-producer-session-before-runtime ()
  "Extension composition cannot become a second Session router."
  (let ((producer (generate-new-buffer " *chat-producer-session*"))
        runtime-called)
    (unwind-protect
        (with-current-buffer producer
          (cl-letf (((symbol-function 'superchat-extension-compose-request)
                     (lambda (request &optional guard)
                       (let ((candidate
                              (plist-put (copy-sequence request)
                                         :session-id "extension-session")))
                         (when guard (funcall guard candidate))
                         candidate)))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (_request) (setq runtime-called t))))
            (should-error
             (superchat-execute-request-plan
              (list :type :llm-query :prompt "hello"
                    :session-id "producer-session" :buffer producer))
             :type 'error)
            (should-not runtime-called)))
      (when (buffer-live-p producer) (kill-buffer producer)))))

(defun plan-owner-test--callback-integrity (key remove)
  "Unit boundary: real composition, captured identity, no Runtime permitted."
  (let ((producer (generate-new-buffer " *plan-callback*"))
        (superchat-extension--registry nil)
        called original-request)
    (unwind-protect
        (with-current-buffer producer
          (setq original-request
                (list :prompt "generated" :buffer producer :session-id "producer-session"
                      :accepted-function (lambda (_) nil)
                      :owner-stop-function (lambda (_) nil)))
          (superchat-define-extension
           'mutate-owner :description "Fixture callback mutation" :context
           (lambda (candidate)
             ;; Change both the original and candidate: reading the original
             ;; after this callback cannot detect the replacement.
             (let ((replacement (lambda (_) 'wrong)))
               (plist-put original-request key replacement)
               (if remove
                   (let ((tail candidate))
                     (while (and (cddr tail) (not (eq (caddr tail) key)))
                       (setq tail (cddr tail)))
                     (when (cddr tail) (setcdr (cdr tail) (cddddr tail))))
                 (plist-put candidate key replacement)))
             "context"))
          (cl-letf (((symbol-function 'superchat-runtime-submit)
                     (lambda (_) (setq called t))))
            (should-error (superchat--submit-chat-request original-request))
            (should-not called)))
      (kill-buffer producer))))

(ert-deftest chat/plan-owner-accepted-capture-not-mutable-original ()
  (plan-owner-test--callback-integrity :accepted-function nil))
(ert-deftest chat/plan-owner-stop-capture-not-mutable-original ()
  (plan-owner-test--callback-integrity :owner-stop-function nil))
(ert-deftest chat/plan-owner-accepted-presence-protected ()
  (plan-owner-test--callback-integrity :accepted-function t))
(ert-deftest chat/plan-owner-stop-presence-protected ()
  (plan-owner-test--callback-integrity :owner-stop-function t))

(ert-deftest chat/plan-owner-final-callback-check-after-activation ()
  "A post-acceptance mutation cannot pass the final pre-Runtime boundary."
  (let ((producer (generate-new-buffer " *plan-callback-record*"))
        (superchat-extension--registry nil) candidate called recorded)
    (unwind-protect
        (with-current-buffer producer
          (superchat-define-extension 'capture-owner :description "Fixture capture" :context
                                     (lambda (request) (setq candidate request) "context"))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (_) (setq recorded t)
                       (plist-put candidate :accepted-function #'ignore)))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (_) (setq called t))))
            (should-error
             (superchat--submit-chat-request
              (list :prompt "generated" :buffer producer :session-id "producer-session"
                    :accepted-function (lambda (_) nil) :owner-stop-function nil)))
            (should recorded)
            (should-not called)))
      (kill-buffer producer))))

(ert-deftest chat/extension-cannot-retarget-producer-buffer-before-runtime ()
  "Extension composition cannot replace the exact producer View object."
  (let ((producer (generate-new-buffer " *chat-producer-buffer*"))
        (replacement (generate-new-buffer " *chat-extension-buffer*"))
        runtime-called)
    (unwind-protect
        (with-current-buffer producer
          (cl-letf (((symbol-function 'superchat-extension-compose-request)
                     (lambda (request &optional guard)
                       (let ((candidate
                              (plist-put (copy-sequence request)
                                         :buffer replacement)))
                         (when guard (funcall guard candidate))
                         candidate)))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (_request) (setq runtime-called t))))
            (should-error
             (superchat-execute-request-plan
              (list :type :llm-query :prompt "hello"
                    :session-id "producer-session" :buffer producer))
             :type 'error)
            (should-not runtime-called)))
      (when (buffer-live-p producer) (kill-buffer producer))
      (when (buffer-live-p replacement) (kill-buffer replacement)))))

(ert-deftest chat/extension-retarget-is-rejected-before-activation-publication ()
  "A composed identity change has zero Extension, Runtime, or provider effects."
  (let ((producer (generate-new-buffer " *chat-atomic-producer*"))
        (replacement (generate-new-buffer " *chat-atomic-replacement*"))
        (superchat-extension--registry nil)
        events runtime-called provider-called)
    (unwind-protect
        (with-current-buffer producer
          (superchat-define-extension
           'retarget
           :description "Mutates identity"
           :context
           (lambda (request)
             (plist-put request :buffer replacement)
             (plist-put request :session-id "wrong-session")
             "context"))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (event) (push event events)))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (_request) (setq runtime-called t)))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args) (setq provider-called t))))
            (should-error
             (superchat-execute-request-plan
              (list :type :llm-query :prompt "hello"
                    :session-id "producer-session" :buffer producer))
             :type 'error)
            (should-not events)
            (should-not runtime-called)
            (should-not provider-called)))
      (setq superchat-extension--registry nil)
      (when (buffer-live-p producer) (kill-buffer producer))
      (when (buffer-live-p replacement) (kill-buffer replacement)))))

(ert-deftest chat/extension-cannot-remove-either-producer-identity ()
  "Copied composition cannot erase a frozen buffer or Session identity."
  (dolist (key '(:buffer :session-id))
    (let ((producer (generate-new-buffer " *chat-identity-removal*"))
          runtime-called)
      (unwind-protect
          (with-current-buffer producer
            (cl-letf
                (((symbol-function 'superchat-extension-compose-request)
                  (lambda (request &optional guard)
                    (let ((candidate (copy-sequence request)))
                      (setq candidate (plist-put candidate key nil))
                      ;; A nil value is not presence; remove the key entirely.
                      (cl-remf candidate key)
                      (when guard (funcall guard candidate))
                      candidate)))
                 ((symbol-function 'superchat-runtime-submit)
                  (lambda (_request) (setq runtime-called t))))
              (should-error
               (superchat-execute-request-plan
                (list :type :llm-query :prompt "hello"
                      :session-id "producer-session" :buffer producer))
               :type 'error)
              (should-not runtime-called)))
        (when (buffer-live-p producer) (kill-buffer producer))))))

(ert-deftest chat/legal-extension-composition-publishes-and-submits-once ()
  "Accepted enrichment records one activation and reaches Runtime once."
  (let ((producer (generate-new-buffer " *chat-legal-extension*"))
        (superchat-extension--registry nil)
        events submissions)
    (unwind-protect
        (with-current-buffer producer
          (superchat-define-extension
           'legal :description "Adds context" :context (lambda (_request) "ok"))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (event) (push event events) 1))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (request)
                       (push request submissions)
                       '(:run-id "legal-run" :status accepted))))
            (superchat-execute-request-plan
             (list :type :llm-query :prompt "hello"
                   :session-id "producer-session" :buffer producer)))
          (should (= 1 (length events)))
          (should (equal "extension.activated"
                         (plist-get (car events) :type)))
          (should (= 1 (length submissions))))
      (setq superchat-extension--registry nil)
      (when (buffer-live-p producer) (kill-buffer producer)))))

(ert-deftest chat/same-name-view-rejects-malformed-workspace-ownership ()
  "View discovery delegates malformed marker interpretation to its authority."
  (let* ((name (generate-new-buffer-name "*superchat:malformed-view*"))
         (buffer (get-buffer-create name)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local superchat--editor-workspace-owned-p t)
            (setq-local superchat--editor-workspace-session-id nil))
          (should-error (superchat--ensure-chat-view name) :type 'error))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest workflow/dispatcher-passes-one-resolved-candidate-to-loader ()
  "Dispatcher passes its sole resolved workflow path through unchanged."
  (let* ((directory (make-temp-file "superchat-workflow-single-resolve-" t))
         (candidate (expand-file-name "one.workflow" directory))
         (resolve-count 0)
         load-args)
    (unwind-protect
        (progn
          (with-temp-file candidate
            (insert "one step\n"))
          (with-temp-buffer
            (setq-local superchat-mode t
                        superchat--session-id "workflow-resolve-session"
                        superchat-buffer-name (buffer-name)
                        superchat--prompt-start (copy-marker (point-min)))
            (insert ">>one argument")
            (let ((superchat-workflow-directory directory)
                  (superchat-lang "English")
                  (superchat-show-ttft nil)
                  (original-load (symbol-function 'superchat-workflow--load)))
              (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                         #'ignore)
                        ((symbol-function 'superchat-workflow--resolve)
                         (lambda (_name)
                           (setq resolve-count (1+ resolve-count))
                           candidate))
                        ((symbol-function 'superchat-workflow--load)
                         (lambda (&rest args)
                           (setq load-args args)
                           (apply original-load args)))
                        ((symbol-function
                          'superchat-workflow--render-workflow-header)
                         #'ignore)
                        ((symbol-function 'superchat-workflow--run-step)
                         #'ignore))
                (superchat-send-input))))
          (should (= 1 resolve-count))
          (should (equal (list "one" candidate) load-args)))
      (delete-directory directory t))))

(ert-deftest workflow/slash-handler-passes-one-resolved-candidate-to-loader ()
  "The real `/workflow' handler passes its candidate without re-resolving."
  (let* ((directory (make-temp-file "superchat-workflow-slash-resolve-" t))
         (candidate (expand-file-name "one.workflow" directory))
         (resolve-count 0)
         load-args
         execute-args
         (original-resolve (symbol-function 'superchat-workflow--resolve))
         (original-load (symbol-function 'superchat-workflow--load))
         (original-execute
          (symbol-function 'superchat-workflow-execute-async)))
    (unwind-protect
        (progn
          (with-temp-file candidate
            (insert "one step\n"))
          (let ((superchat-workflow-directory directory)
                (superchat-lang "English")
                (superchat--session-id "workflow-slash-session"))
            (cl-letf (((symbol-function 'superchat-workflow--resolve)
                       (lambda (name)
                         (setq resolve-count (1+ resolve-count))
                         (funcall original-resolve name)))
                      ((symbol-function 'superchat-workflow-execute-async)
                       (lambda (&rest args)
                         (setq execute-args args)
                         (apply original-execute args)))
                      ((symbol-function 'superchat-workflow--load)
                       (lambda (&rest args)
                         (setq load-args args)
                         (apply original-load args)))
                      ((symbol-function
                        'superchat-workflow--render-workflow-header)
                       #'ignore)
                      ((symbol-function 'superchat-workflow--run-step)
                       #'ignore))
              (should (equal '(:type :noop)
                             (superchat--cmd-workflow
                              "workflow" "one argument" nil "English" nil)))))
          (should (= 1 resolve-count))
          (should (equal (list "one" "argument" (file-truename candidate))
                         execute-args))
          (should (equal (list "one" (file-truename candidate)) load-args)))
      (delete-directory directory t))))

(defun test-cmd-dispatch--match-count (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (max (1+ (match-beginning 0)) (match-end 0))))
    count))

(defun test-cmd-dispatch--reset-buffer (session-id)
  "Reset the current buffer to a minimal real Superchat View."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local superchat--session-id session-id
                superchat--conversation-history nil
                superchat--current-command nil
                superchat--current-context-files nil
                superchat--pending-recalled-memories nil
                superchat--response-start-marker nil
                superchat--assistant-response-start-marker nil
                superchat--assistant-response-end-marker nil
                superchat--response-region-persistent-marker nil
                superchat--prompt-start nil
                superchat--current-response-parts nil
                superchat--ttft-elapsed nil)))

(defun test-cmd-dispatch--interleaved-system-output (input late-p)
  "Run INPUT through the real command/render path during an older Run.
When LATE-P is non-nil, stream a late older-Run chunk before INPUT.  Only
the durable assistant history callback is isolated; transcript placement and
the send/handler/dispatcher/renderer path remain real."
  (let ((name (generate-new-buffer-name " *superchat-command-interleave*"))
        (directory (make-temp-file "superchat-command-interleave-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "command" directory) t)
          (with-current-buffer (get-buffer-create name)
            (org-mode)
            (let ((superchat-buffer-name name)
                  (superchat-data-directory directory)
                  (superchat-lang "English")
                  (superchat-llm-backend nil)
                  (superchat-llm-model nil)
                  (superchat-memory-auto-recall-min-length 999999)
                  (superchat-context-message-count 0)
                  (superchat-context-max-chars 20)
                  (superchat-system-prompt-functions nil)
                  (superchat-build-prompt-functions nil)
                  (superchat-pre-topic-functions nil)
                  (superchat-post-topic-functions nil)
                  (superchat-show-ttft nil))
              (erase-buffer)
              (setq-local superchat--session-id "command-interleave"
                          superchat--conversation-history nil
                          superchat--current-command nil
                          superchat--current-context-files nil
                          superchat--pending-recalled-memories nil
                          superchat--response-start-marker nil
                          superchat--assistant-response-start-marker nil
                          superchat--assistant-response-end-marker nil
                          superchat--response-region-persistent-marker nil
                          superchat--prompt-start nil
                          superchat--current-response-parts nil)
              (insert "* User: older model run\n")
              (superchat--prepare-for-response)
              (superchat--update-status "OLDER-THINKING")
              (superchat--stream-llm-result "OLDER-EARLY")
              (superchat--insert-persistent-system-message "COMPACT-NOTICE")
              (when late-p
                (superchat--stream-llm-result "OLDER-LATE"))
              (goto-char (point-max))
              (insert input)
              (superchat-send-input)
              (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
                (superchat--process-llm-result "OLDER-FINAL"))
              (buffer-string))))
      (when (get-buffer name)
        (kill-buffer name))
      (delete-directory directory t))))

(require 'org)

(ert-deftest cmd/backend-output-follows-own-user-row-during-older-run ()
  "A backend System result stays after /backend and outside the older Run."
  (let* ((text (test-cmd-dispatch--interleaved-system-output "/backend" nil))
         (final (string-match "OLDER-FINAL" text))
         (notice (string-match "COMPACT-NOTICE" text))
         (input (string-match "\\* User: /backend" text))
         (output (string-match "Backend: llm\\.el" text))
         (prompt (string-match "\\* User: \\(?:\\n\\|\\'\\)" text)))
    (should (= 1 (test-cmd-dispatch--match-count "Backend: llm\\.el" text)))
    (should (= 1 (test-cmd-dispatch--match-count "Provider: UNCONFIGURED" text)))
    (should (< final notice))
    (should (< notice input))
    (should (< input output))
    (should (< output prompt))))

(ert-deftest cmd/commands-output-survives-late-older-run-finalize ()
  "A /commands System result survives an older Run's late final rewrite."
  (let* ((text (test-cmd-dispatch--interleaved-system-output "/commands" t))
         (final (string-match "OLDER-FINAL" text))
         (notice (string-match "COMPACT-NOTICE" text))
         (input (string-match "\\* User: /commands" text))
         (output (string-match "Show all available commands list" text))
         (prompt (string-match "\\* User: \\(?:\\n\\|\\'\\)" text)))
    (should (= 1 (test-cmd-dispatch--match-count
                  "Show all available commands list" text)))
    (should (< final notice))
    (should (< notice input))
    (should (< input output))
    (should (< output prompt))))

(ert-deftest cmd/commands-output-survives-unbounded-older-run ()
  "C-c C-h survives an older unbounded Run's later final rewrite."
  (let ((name (generate-new-buffer-name " *superchat-third-branch-command*")))
    (unwind-protect
        (with-current-buffer (get-buffer-create name)
          (org-mode)
          (let ((superchat-buffer-name name))
            (test-cmd-dispatch--reset-buffer "third-branch-command")
            (insert "* User: older model run\n")
            (superchat--prepare-for-response)
            (superchat--update-status "OLDER-THINKING")
            (superchat--stream-llm-result "OLDER-EARLY")
            ;; This is the third owner branch: an active unbounded assistant
            ;; region has neither a submitted Turn marker nor a prompt.
            (should superchat--assistant-response-start-marker)
            (should-not superchat--assistant-response-end-marker)
            (should-not superchat--response-region-persistent-marker)
            (should-not superchat--response-start-marker)
            (should-not superchat--prompt-start)
            ;; Exercise the real C-c C-h command, not the slash handler.
            (superchat--list-commands)
            (should (markerp superchat--assistant-response-end-marker))
            (should (marker-position superchat--assistant-response-end-marker))
            (should-not
             (marker-insertion-type superchat--assistant-response-end-marker))
            (let ((boundary (marker-position
                             superchat--assistant-response-end-marker)))
              (goto-char boundary)
              (should (looking-at-p "[\n ]*\\* System:")))
            (superchat--stream-llm-result "OLDER-LATE")
            (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
              (superchat--process-llm-result "OLDER-FINAL"))
            (let* ((text (buffer-string))
                   (final (string-match "OLDER-FINAL" text))
                   (output (string-match "Show all available commands list"
                                         text))
                   (prompt (string-match
                            "\\* User: \\(?:\\n\\|\\'\\)" text)))
              (should (= 1 (test-cmd-dispatch--match-count
                            "Show all available commands list" text)))
              (should (= 1 (test-cmd-dispatch--match-count
                            "OLDER-FINAL" text)))
              (should-not (string-match-p "OLDER-EARLY\\|OLDER-LATE" text))
              (should (< final output))
              (should (< output prompt))
              (should-not superchat--assistant-response-start-marker)
              (should-not superchat--assistant-response-end-marker))))
      (when (get-buffer name)
        (kill-buffer name)))))

(ert-deftest cmd/idle-commands-output-follows-own-user-row ()
  "A real idle /commands result follows its own user row."
  (let ((name (generate-new-buffer-name " *superchat-idle-commands*"))
        (directory (make-temp-file "superchat-idle-commands-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "command" directory) t)
          (with-current-buffer (get-buffer-create name)
            (org-mode)
            (let ((superchat-buffer-name name)
                  (superchat-data-directory directory)
                  (superchat-lang "English")
                  (superchat-llm-backend nil)
                  (superchat-llm-model nil)
                  (superchat-memory-auto-recall-min-length 999999)
                  (superchat-context-message-count 0)
                  (superchat-context-max-chars 20)
                  (superchat-system-prompt-functions nil)
                  (superchat-build-prompt-functions nil)
                  (superchat-pre-topic-functions nil)
                  (superchat-post-topic-functions nil)
                  (superchat-post-turn-functions nil)
                  (superchat-show-ttft nil))
              (test-cmd-dispatch--reset-buffer "idle-commands")
              (insert "#+TITLE: superchat\n")
              (superchat--insert-prompt)
              (goto-char (point-max))
              (insert "/commands")
              (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
                (superchat-send-input))
              (let* ((text (buffer-string))
                     (input (string-match "\\* User: /commands" text))
                     (output (string-match
                              "Show all available commands list" text))
                     (prompt (string-match
                              "\\* User: \\(?:\\n\\|\\'\\)" text)))
                (should (= 1 (test-cmd-dispatch--match-count
                              "Show all available commands list" text)))
                (should (= 1 (test-cmd-dispatch--match-count
                              "\\* User: /commands" text)))
                (should (= 1 (test-cmd-dispatch--match-count
                              "\\* User: \\(?:\\n\\|\\'\\)" text)))
                (should (< input output))
                (should (< output prompt))))))
      (when (get-buffer name)
        (kill-buffer name))
      (delete-directory directory t))))

(ert-deftest workflow/empty-name-reports-usage ()
  "An empty workflow name reports usage; other names have reserved boundaries."
  (should (equal
           "Usage: >>name [args]"
           (test-cmd-dispatch--workflow-error-message ">>"))))

(ert-deftest workflow/non-ascii-file-name-executes-through-real-loader ()
  "A Chinese .workflow filename is found and submitted by the real path."
  (let* ((workflow-directory
          (make-temp-file "superchat-workflow-unicode-" t))
         (workflow-file
          (expand-file-name "我的流程.workflow" workflow-directory))
         submitted transcript)
    (unwind-protect
        (progn
          (with-temp-file workflow-file
            (insert "处理 $input\n"))
          (let ((superchat-workflow-directory workflow-directory)
                (superchat-lang "English")
                (superchat-show-ttft nil)
                (superchat-show-ttft-breakdown nil))
            (with-temp-buffer
              (setq-local superchat-mode t
                          superchat--session-id "workflow-unicode-session"
                          superchat-buffer-name (buffer-name)
                          superchat--prompt-start (copy-marker (point-min)))
              (insert ">>我的流程 参数值")
              (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                         #'ignore)
                        ((symbol-function 'superchat-runtime-submit)
                         (lambda (request)
                           (setq submitted request)
                           '(:run-id "run-unicode-workflow"
                             :status accepted))))
                (superchat-send-input))
              (setq transcript (buffer-string))))
          (should (equal "workflow" (plist-get submitted :capability)))
          (should (equal "处理 参数值" (plist-get submitted :prompt)))
          (should (string-match-p "\\*\\* Workflow: 我的流程"
                                  transcript)))
      (delete-directory workflow-directory t))))

(ert-deftest workflow/original-view-input-is-independent-during-streaming ()
  "A workflow stream and a new prompt in its View keep separate Run regions."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-workflow-interleave-" t))
         (workflow-directory (expand-file-name "workflow" directory))
         (workflow-file (expand-file-name "interleave.workflow"
                                          workflow-directory))
         (name (generate-new-buffer-name " *superchat-workflow-interleave*"))
         (superchat-data-directory directory)
         (superchat-workflow-directory workflow-directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-runtime-default-deadline nil)
         (superchat-show-ttft nil)
         (superchat-show-ttft-breakdown nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-llm-backend nil)
         (superchat-llm-model nil)
         (superchat-lang "English")
         (superchat-pre-topic-functions nil)
         (superchat-post-topic-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         requests)
    (unwind-protect
        (progn
          (make-directory workflow-directory t)
          (with-temp-file workflow-file
            (insert "first workflow step\n"))
          (with-current-buffer (get-buffer-create name)
            (org-mode)
            (let ((superchat-buffer-name name))
              (setq-local superchat-mode t
                          superchat--session-id "workflow-interleave-session"
                          superchat--conversation-history nil
                          superchat--current-command nil
                          superchat--response-region-states nil
                          superchat--response-region-current-owner nil
                          superchat--prompt-start nil
                          superchat--active-run-id nil)
              (insert "#+TITLE: superchat\n")
              (superchat--insert-prompt)
              (goto-char (point-max))
              (insert ">>interleave")
              (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                         (lambda (prompt _callback stream-callback &rest _args)
                           (push (list :prompt prompt
                                       :stream stream-callback
                                       :observer superchat--llm-runtime-observer)
                                 requests)
                           nil))
                        ((symbol-function
                          'superchat-workflow--resume-decision-for-step)
                         (lambda (&rest _args) '(:action advance))))
                ;; Both submissions use the real dispatcher and Runtime;
                ;; only the provider transport is held open by the mock.
                (superchat-send-input)
                (should (= 1 (length requests)))
                (goto-char (point-max))
                (insert "plain chat while workflow streams")
                (superchat-send-input)
                (should (= 2 (length requests)))
                (let* ((workflow-request
                        (cl-find "first workflow step" requests
                                 :key (lambda (request) (plist-get request :prompt))
                                 :test #'equal))
                       (chat-request
                        (cl-find-if
                         (lambda (request)
                           (string-match-p
                            "plain chat while workflow streams"
                            (plist-get request :prompt)))
                         requests))
                       (projection (superchat-runtime-status-projection)))
                  (should workflow-request)
                  (should chat-request)
                  ;; The two Runs share the Session Context View by design;
                  ;; separate owner regions control rendering placement only.
                  (should (string-match-p
                           "first workflow step"
                           (plist-get chat-request :prompt)))
                  (should (= 2 (plist-get projection :active-count)))
                  ;; Deliver in an interleaved order through each Run's real
                  ;; stream callback and transport observer.
                  (funcall (plist-get workflow-request :stream) "WF-STREAM")
                  (funcall (plist-get chat-request :stream) "CHAT-STREAM")
                  (funcall (plist-get workflow-request :observer)
                           'terminal
                           '(:status completed :response "WF-FINAL"))
                  (funcall (plist-get chat-request :observer)
                           'terminal
                           '(:status completed :response "CHAT-FINAL"))
                  (let* ((text (buffer-string))
                         (workflow-user (string-match
                                         "\\* User: >>interleave" text))
                         (workflow-final (string-match "WF-FINAL" text))
                         (chat-user (string-match
                                     "\\* User: plain chat while workflow streams"
                                     text))
                         (chat-final (string-match "CHAT-FINAL" text))
                         (events (superchat-tape-replay-events
                                  "workflow-interleave-session"))
                         (terminal-count
                          (cl-count "run.completed" events
                                    :key (lambda (event) (plist-get event :type))
                                    :test #'equal)))
                    (should (= 1 (test-cmd-dispatch--match-count
                                  "WF-FINAL" text)))
                    (should (= 1 (test-cmd-dispatch--match-count
                                  "CHAT-FINAL" text)))
                    (should-not (string-match-p "WF-STREAM\\|CHAT-STREAM" text))
                    (should (< workflow-user workflow-final))
                    (should (< workflow-final chat-user))
                    (should (< chat-user chat-final))
                    (should (= 2 terminal-count))))))))
      (when (get-buffer name)
        (with-current-buffer name
          (when (timerp superchat--status-refresh-timer)
            (cancel-timer superchat--status-refresh-timer))
          (when (fboundp 'superchat--response-region-reset)
            (superchat--response-region-reset)))
        (kill-buffer name))
      (superchat-db-close)
      (delete-directory directory t))))

(defun test-cmd-dispatch--interleaved-terminal (terminal-status)
  "Run an in-place workflow/ordinary interleave ending with TERMINAL-STATUS.
The provider is held open by a real Runtime request while both Runs stream into
one View.  TERMINAL-STATUS is `cancelled', `failed', or `completed'.  Return a
plist containing the rendered TEXT, durable EVENTS, and remaining active Runs.
The helper deliberately uses the same Session for both requests: owner keys
separate transcript placement, not the Context View."
  (let* ((directory (make-temp-file "superchat-workflow-terminal-" t))
         (workflow-directory (expand-file-name "workflow" directory))
         (workflow-file (expand-file-name "interleave.workflow"
                                          workflow-directory))
         (name (generate-new-buffer-name " *superchat-workflow-terminal*"))
         (superchat-data-directory directory)
         (superchat-workflow-directory workflow-directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-runtime-default-deadline nil)
         (superchat-show-ttft nil)
         (superchat-show-ttft-breakdown nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-llm-backend nil)
         (superchat-llm-model nil)
         (superchat-lang "English")
         (superchat-pre-topic-functions nil)
         (superchat-post-topic-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (requests nil)
         (workflow-run-id nil)
         (chat-run-id nil)
         result)
    (unwind-protect
        (progn
          (make-directory workflow-directory t)
          (with-temp-file workflow-file
            (insert "first workflow step\n"))
          (with-current-buffer (get-buffer-create name)
            (org-mode)
            (let ((superchat-buffer-name name))
              (setq-local superchat-mode t
                          superchat--session-id "workflow-terminal-session"
                          superchat--conversation-history nil
                          superchat--current-command nil
                          superchat--response-region-states nil
                          superchat--response-region-current-owner nil
                          superchat--prompt-start nil
                          superchat--active-run-id nil)
              (insert "#+TITLE: superchat\n")
              (superchat--insert-prompt)
              (goto-char (point-max))
              (insert ">>interleave")
              (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                         (lambda (prompt _callback stream-callback &rest _args)
                           (push (list :prompt prompt
                                       :stream stream-callback
                                       :observer superchat--llm-runtime-observer)
                                 requests)
                           nil))
                        ((symbol-function
                          'superchat-workflow--resume-decision-for-step)
                         (lambda (&rest _args) '(:action advance))))
                (let ((before (superchat-runtime-active-runs)))
                  (superchat-send-input)
                  (setq workflow-run-id
                        (car (cl-set-difference
                              (superchat-runtime-active-runs)
                              before :test #'equal))))
                (goto-char (point-max))
                (insert "plain chat while workflow streams")
                (let ((before (superchat-runtime-active-runs)))
                  (superchat-send-input)
                  (setq chat-run-id
                        (car (cl-set-difference
                              (superchat-runtime-active-runs)
                              before :test #'equal))))
                (let* ((workflow-request
                        (cl-find "first workflow step" requests
                                 :key (lambda (request) (plist-get request :prompt))
                                 :test #'equal))
                       (chat-request
                        (cl-find-if
                         (lambda (request)
                           (string-match-p
                            "plain chat while workflow streams"
                            (plist-get request :prompt)))
                         requests)))
                  (should workflow-run-id)
                  (should chat-run-id)
                  (should workflow-request)
                  (should chat-request)
                  ;; Same Session means workflow material is intentionally in
                  ;; the ordinary Run's Context View.
                  (should (string-match-p
                           "first workflow step"
                           (plist-get chat-request :prompt)))
                  (funcall (plist-get workflow-request :stream) "WF-STREAM")
                  (funcall (plist-get chat-request :stream) "CHAT-STREAM")
                  (pcase terminal-status
                    ('cancelled
                     (should (superchat-runtime-cancel workflow-run-id)))
                    ('failed
                     (funcall (plist-get workflow-request :observer)
                              'terminal
                              '(:status failed
                                :response "WORKFLOW-PROVIDER-FAILED")))
                    ('completed
                     (funcall (plist-get workflow-request :observer)
                              'terminal
                              '(:status completed :response "WF-FINAL")))
                    (_
                     (error "Unknown terminal status: %S" terminal-status)))
                  (funcall (plist-get chat-request :observer)
                           'terminal
                           '(:status completed :response "CHAT-FINAL"))
                  (setq result
                        (list :text (buffer-string)
                              :events
                              (superchat-tape-replay-events
                               "workflow-terminal-session")
                              :active (superchat-runtime-active-runs))))))))
      (when (get-buffer name)
        (with-current-buffer name
          (when (timerp superchat--status-refresh-timer)
            (cancel-timer superchat--status-refresh-timer))
          (when (fboundp 'superchat--response-region-reset)
            (superchat--response-region-reset)))
        (kill-buffer name))
      (ignore-errors (superchat-db-close))
      (delete-directory directory t))
    result))

(ert-deftest workflow/original-view-input-cancellation-is-terminal-and-local ()
  "Cancelling an in-place workflow preserves the ordinary Run's delivery."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((result (test-cmd-dispatch--interleaved-terminal 'cancelled))
         (text (plist-get result :text))
         (events (plist-get result :events)))
    (should-not (plist-get result :active))
    (should (string-match-p "\\*\\* Workflow stopped at step 1/1" text))
    (should (string-match-p "\\[Stopped by user\\]" text))
    (should (= 1 (test-cmd-dispatch--match-count "CHAT-FINAL" text)))
    (should-not (string-match-p "WF-FINAL" text))
    (should (string-match-p "WF-STREAM" text))
    (should-not (string-match-p "CHAT-STREAM" text))
    (should (= 1 (cl-count "run.cancelled" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))
    (should (= 1 (cl-count "run.completed" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))))

(ert-deftest workflow/original-view-input-failure-is-terminal-and-local ()
  "A failed in-place workflow preserves the ordinary Run's delivery."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((result (test-cmd-dispatch--interleaved-terminal 'failed))
         (text (plist-get result :text))
         (events (plist-get result :events)))
    (should-not (plist-get result :active))
    (should (= 1 (test-cmd-dispatch--match-count
                  "\\*\\* Workflow FAILED at step 1/1" text)))
    (should (string-match-p "WORKFLOW-PROVIDER-FAILED" text))
    (should (= 1 (test-cmd-dispatch--match-count "CHAT-FINAL" text)))
    (should-not (string-match-p "WF-STREAM\\|CHAT-STREAM" text))
    (should (= 1 (cl-count "run.failed" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))
    (should (= 1 (cl-count "run.completed" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))))

(defun test-cmd-dispatch--skill-request (skill-type input &optional threshold)
  "Run INPUT through a real SKILL-TYPE file and return request diagnostics."
  (let* ((skills-directory
          (make-temp-file "superchat-skill-dispatch-" t))
         (skill-name (format "batch4-%s" skill-type))
         (skill-file (expand-file-name (concat skill-name ".md")
                                       skills-directory))
         submitted messages transcript)
    (unwind-protect
        (progn
          (with-temp-file skill-file
            (insert (format
                     "---\nname: %s\ndescription: Batch4 %s\ntype: %s\nprofile_id: profile.%s\ntools: [read-file]\n---\n\nSYSTEM-%s\n"
                     skill-name skill-type skill-type skill-type skill-type)))
          (let ((superchat-skills-directory skills-directory)
                (superchat-lang "English")
                (superchat-inline-file-content t)
                (superchat-inline-metadata-threshold threshold)
                (superchat-show-ttft nil)
                (superchat-show-ttft-breakdown nil)
                (superchat--active-preset nil))
            (with-temp-buffer
              (setq-local superchat-mode t
                          superchat--session-id nil
                          superchat-buffer-name (buffer-name)
                          superchat--prompt-start (copy-marker (point-min))
                          superchat--active-preset nil)
              (insert input)
              (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                         #'ignore)
                        ((symbol-function 'superchat-runtime-submit)
                         (lambda (request)
                           (setq submitted request)
                           '(:run-id "run-skill-dispatch"
                             :status accepted)))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args)
                                 messages))))
                (superchat-send-input)
                (setq transcript (buffer-string)))))
          (list :request submitted
                :messages (nreverse messages)
                :transcript transcript))
      (delete-directory skills-directory t))))

(ert-deftest skill/file-context-survives-final-runtime-request-for-all-presets ()
  "Prompt, Agent, and Plan skills preserve the prepared #file payload."
  ;; `superchat--add-file-to-context' (reached through the real #file prompt
  ;; hook this test exercises) mutates `superchat--current-context-files' in
  ;; whatever dynamic scope is active; without this binding it leaks into
  ;; the variable's global default and survives past this test.
  (let ((superchat--current-context-files nil)
        (file (make-temp-file "superchat-skill-file-" nil ".el"))
        (file-text
         "BATCH7_INLINE_FILE_TOKEN\n(defun batch7-inline-file () 'once)\n"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert file-text))
          (dolist (skill-type '(prompt agent plan))
            (let* ((result
                    (test-cmd-dispatch--skill-request
                     skill-type
                     (format ">batch4-%s #%s Inspect the attached file"
                             skill-type file)))
                   (request (plist-get result :request))
                   (prompt (or (plist-get request :prompt) ""))
                   (system (or (plist-get request :system-prompt) ""))
                   (skill-body (format "SYSTEM-%s" skill-type))
                   (wire-order (concat system "\n" prompt))
                   (body-position
                    (string-match (regexp-quote skill-body) wire-order))
                   (file-position
                    (string-match (regexp-quote file-text) wire-order)))
              ;; Assert the exact prompt handed to Runtime, not an
              ;; intermediate Turn or hook result.
              (should (= 1 (test-cmd-dispatch--match-count
                            (regexp-quote file-text) wire-order)))
              (should (= 1 (test-cmd-dispatch--match-count
                            (regexp-quote skill-body) wire-order)))
              (should (numberp body-position))
              (should (numberp file-position))
              (should (< body-position file-position))
              (if (eq skill-type 'prompt)
                (progn
                    (should (string-match-p "SYSTEM-prompt" prompt))
                    (should-not (string-match-p "SYSTEM-prompt" system)))
                (should (string-match-p skill-body system))))))
      (delete-file file))))

(ert-deftest skill/metadata-only-file-survives-request-and-is-visible-to-user ()
  "A large skill attachment keeps metadata and discloses no full injection."
  ;; See the sibling test above for why this binding is required: the real
  ;; #file prompt hook this test exercises otherwise leaks into the global
  ;; default of `superchat--current-context-files'.
  (let ((superchat--current-context-files nil)
        (file (make-temp-file "superchat-skill-metadata-" nil ".el"))
        (token "BATCH7_METADATA_BODY_TOKEN"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (make-string 700 ?x) token "\n"))
          (let* ((result
                  (test-cmd-dispatch--skill-request
                   'prompt
                   (format ">batch4-prompt #%s Inspect the attached file" file)
                   32))
                 (request (plist-get result :request))
                 (prompt (plist-get request :prompt))
                 (transcript (plist-get result :transcript)))
            (should (string-match-p "Referenced file.*NOT inlined" prompt))
            (should (string-match-p (regexp-quote file) prompt))
            (should-not (string-match-p (regexp-quote token) prompt))
            (should (string-match-p "未全文注入" transcript))))
      (delete-file file))))

(ert-deftest skill/mixed-input-uses-clean-prompt-and-echoes-skipped-command ()
  "Agent and plan skills preserve the prepared prompt and explain precedence."
  (dolist (skill-type '(agent plan))
    (let* ((result
            (test-cmd-dispatch--skill-request
             skill-type
             (format ">batch4-%s /reset hello" skill-type)))
           (request (plist-get result :request))
           (messages (plist-get result :messages)))
      (should (string-match-p
               (regexp-quote "/reset hello")
               (plist-get request :prompt)))
      (should-not (equal "/reset hello" (plist-get request :prompt)))
      (should (equal "/reset hello" (plist-get request :clean-input)))
      (should (string-match-p (format "SYSTEM-%s" skill-type)
                              (plist-get request :system-prompt)))
      (should
       (member "/reset 未作为命令执行，已当作普通文字" messages)))))

(ert-deftest skill/mixed-input-only-warns-for-a-leading-command-token ()
  "Only a command in the cleaned input's first token gets a reminder."
  (dolist (input '(" >batch4-agent explain /reset"
                   ">batch4-agent explain\n/reset"
                   ">batch4-agent ```text\n/xx\n```"
                   ">batch4-agent https://example.test/a/b"))
    (let* ((result (test-cmd-dispatch--skill-request 'agent input))
           (messages (plist-get result :messages)))
      (should-not
       (member "/reset 未作为命令执行，已当作普通文字" messages))
      (should-not
       (member "/xx 未作为命令执行，已当作普通文字" messages)))))

(ert-deftest skill/mixed-input-leading-command-still-warns ()
  "A command in the cleaned input's first token still gets a reminder."
  (let* ((result
          (test-cmd-dispatch--skill-request
           'agent ">batch4-agent /unknown args"))
         (messages (plist-get result :messages)))
    (should
     (member "/unknown 未作为命令执行，已当作普通文字" messages))))

(defun cmd-clear-test--with-owned-view (body)
  "Call BODY with an actual owned View and isolated Runtime/SQLite state."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (ert-skip "SQLite unavailable"))
  (let* ((directory (make-temp-file "superchat-clear-owned-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-buffer-name
          (generate-new-buffer-name " *superchat-clear-owned*"))
         view)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "superchat.db" directory))
          (setq view (superchat--ensure-chat-view
                      superchat-buffer-name "clear-owned-before"))
          (with-current-buffer view
            (funcall body view superchat--session-id)))
      ;; A cancellation delivery may itself submit work.  Cleanup still uses
      ;; real cancellation, but never lets that work reach a provider.
      (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
        (maphash
         (lambda (run-id _entry)
           (superchat-runtime-cancel run-id "test cleanup"))
         superchat-runtime--runs))
      (when (buffer-live-p view)
        (with-current-buffer view
          ;; These are this fixture's buffer and hooks, never a user buffer.
          (superchat--cancel-status-refresh-timer)
          (setq-local kill-buffer-hook nil))
        (kill-buffer view))
      (superchat-db-close)
      (delete-directory directory t))))

(defun output-prompt-test--assert-draft (marker draft offset)
  "Assert the exact pending input and point survive a presentation callback."
  (should (eq marker superchat--prompt-start))
  (should (eq marker (plist-get (cdr (assoc :pending superchat--response-region-states))
                               :prompt-marker)))
  (should (equal-including-properties draft (buffer-substring marker (point-max))))
  (should (= offset (- (point) marker)))
  (should (= 1 (cl-count-if
                (lambda (entry)
                  (superchat--response-region--state-marker-position
                   (cdr entry) :prompt-marker))
                superchat--response-region-states))))

(defun output-prompt-test--fontify (marker)
  "Exercise actual Org fontification, even when batch jit-lock is inactive."
  (should (derived-mode-p 'org-mode))
  (save-excursion
    (font-lock-set-defaults)
    (font-lock-fontify-region (point-min) (point-max)))
  (should (eq (get-text-property (1- marker) 'face) 'org-level-1)))

(defun output-prompt-test--live-draft-matrix (workflow &optional fontify)
  "Transport-only fixtures exercise live drafts through the public producers."
  (dolist (stream-first '(nil t))
    (dolist (status '(nil t))
      (dolist (text '("" "PRIVATE-DRAFT" "PRIVATE-DRAFT\nsecond line"))
        (dolist (terminal '(completed cancelled failed))
          (cmd-clear-test--with-owned-view
           (lambda (view session)
             (let* ((superchat-workflow-directory superchat-data-directory)
                    (draft (propertize text 'output-prompt-draft t))
                    (marker superchat--prompt-start)
                    (offset (min 3 (length draft)))
                    (status-function (symbol-function 'superchat--update-status))
                    captures handle)
               (goto-char marker) (insert draft) (goto-char (+ marker offset))
               (when fontify
                 (output-prompt-test--fontify marker)
                 ;; Preserve the actual fontified draft properties thereafter.
                 (setq draft (buffer-substring marker (point-max))))
               (when workflow
                 (with-temp-file (expand-file-name "draft.workflow" superchat-workflow-directory)
                   (insert "FIRST QUESTION\nSECOND QUESTION\n")))
               (cl-letf (((symbol-function 'superchat--update-status)
                          (and status status-function))
                         ((symbol-function 'superchat--llm-generate-answer)
                          (lambda (prompt _done partial &rest _)
                            (push (list :prompt prompt :partial partial
                                        :observer superchat--llm-runtime-observer
                                        :run-id (plist-get superchat--tool-execution-context :run-id))
                                  captures))))
                 (if workflow
                     (superchat-workflow-execute-async "draft")
                   (setq handle (superchat-runtime-test-submit
                                 (list :prompt "ONLY SUBMITTED QUESTION" :buffer view
                                       :session-id session :tools 'none :deadline nil
                                       :transcript-owner "PROMPT-CUSTOM"))))
                 (output-prompt-test--assert-draft marker draft offset)
                 (when stream-first
                   (funcall (plist-get (car captures) :partial) "PARTIAL"))
                 (output-prompt-test--assert-draft marker draft offset)
                 (superchat--insert-system-message "INDEPENDENT NOTICE")
                 (output-prompt-test--assert-draft marker draft offset)
                 (if (eq terminal 'cancelled)
                     (superchat-runtime-cancel (plist-get (car captures) :run-id))
                   (funcall (plist-get (car captures) :observer) 'terminal
                            (list :status terminal :response "FINAL" :error "LOCAL FAILURE")))
                 (output-prompt-test--assert-draft marker draft offset)
                 (when (and workflow (eq terminal 'completed))
                   (should (= 2 (length captures)))
                   (when stream-first
                     (funcall (plist-get (car captures) :partial) "SECOND PARTIAL"))
                   (output-prompt-test--assert-draft marker draft offset)
                   (funcall (plist-get (car captures) :observer) 'terminal
                            '(:status completed :response "SECOND FINAL"))
                   (output-prompt-test--assert-draft marker draft offset))
                 (superchat--insert-system-message "AFTER TERMINAL NOTICE")
                 (output-prompt-test--assert-draft marker draft offset))
               (should (= 1 (how-many "^\\* User" (point-min) (point-max))))
               (unless (string-empty-p text)
                 (dolist (value (list captures superchat--conversation-history
                                      (superchat-db-tape-replay session)
                                      (superchat-tape-replay-events session)))
                   (should-not (string-match-p "PRIVATE-DRAFT" (prin1-to-string value)))))
               (when handle
                 (should (eq terminal (plist-get
                                       (superchat-runtime-status (plist-get handle :run-id))
                                       :status))))))))))))

(ert-deftest output-prompt/real-runtime-retains-live-draft ()
  (output-prompt-test--live-draft-matrix nil))

(ert-deftest output-prompt/real-workflow-retains-live-draft ()
  (output-prompt-test--live-draft-matrix t))

(ert-deftest output-prompt/fontified-runtime-retains-live-draft ()
  (output-prompt-test--live-draft-matrix nil t))

(ert-deftest output-prompt/fontified-workflow-retains-live-draft ()
  (output-prompt-test--live-draft-matrix t t))

(ert-deftest output-prompt/real-llm-workflow-draft-and-ordinary-send ()
  "Native transport wrapper preserves live input, then consumes only actual send."
  (unless (and (featurep 'llm) (require 'llm-openai nil t))
    (ert-skip "Requires isolated installed llm 0.31.1"))
  (cmd-clear-test--with-owned-view
   (lambda (_view session)
     (let* ((superchat-workflow-directory superchat-data-directory)
            (superchat-llm-backend (make-llm-openai :key "unused" :chat-model "prompt"))
            (superchat-llm-tools-enabled nil)
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (draft (propertize "DRAFT\nMULTILINE" 'output-prompt-draft t))
            (marker superchat--prompt-start) callbacks wf ordinary)
       (with-temp-file (expand-file-name "live.workflow" superchat-workflow-directory)
         (insert "WF QUESTION\n"))
       (goto-char marker) (insert draft) (goto-char (+ marker 2))
       (output-prompt-test--fontify marker)
       (setq draft (buffer-substring marker (point-max)))
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt partial response _error &optional _multi)
                    (push (list prompt partial response) callbacks) nil)))
         (superchat-workflow-execute-async "live")
         (setq wf (car callbacks))
         (output-prompt-test--assert-draft marker draft 2)
         (should-not (string-match-p "MULTILINE" (prin1-to-string (car wf))))
         (funcall (nth 1 wf) "WF PART")
         (output-prompt-test--assert-draft marker draft 2)
         ;; The actual ordinary command consumes the draft at the approved boundary.
         (superchat-send-input)
         (setq ordinary (car callbacks))
         (should (= 2 (length callbacks)))
         (should (string-match-p "MULTILINE" (prin1-to-string (car ordinary))))
         (funcall (nth 1 ordinary) "B PART")
         (funcall (nth 2 wf) "WF FINAL")
         (funcall (nth 2 ordinary) "B FINAL")
         (should (string-match-p "WF FINAL" (buffer-string)))
         (should (string-match-p "B FINAL" (buffer-string)))
         (should (string-match-p "MULTILINE" (buffer-string)))
         (should (eq superchat--prompt-start
                     (plist-get (cdr (assoc :pending superchat--response-region-states))
                                :prompt-marker)))
         (should (= 1 (cl-count-if
                       (lambda (e) (superchat--response-region--state-marker-position
                                    (cdr e) :prompt-marker))
                       superchat--response-region-states)))
         (should (= 2 (cl-count "run.completed" (superchat-tape-replay-events session)
                                :key (lambda (e) (plist-get e :type)) :test #'equal))))))))

(ert-deftest output-prompt/real-cancel-new-prompt-keeps-pending-association ()
  "Former redirect-keeps-pending-association: a new prompt is not the old draft."
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (handle)
       (superchat--response-region-consume-prompt)
       (superchat--prepare-for-response)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
         (setq handle (superchat-runtime-test-submit
                       (list :prompt "CANCEL" :buffer view :session-id session
                             :tools 'none :deadline nil))))
       (setq superchat--active-run-id (plist-get handle :run-id))
       (cl-letf (((symbol-function 'read-string)
                  (lambda (&rest _) (ert-fail "No original draft to edit"))))
         (superchat-cancel))
       (let ((prompt superchat--prompt-start))
         (superchat--response-region-ensure-view-prompt t)
         (should (eq prompt superchat--prompt-start))
         (should (equal "" (buffer-substring prompt (point-max))))
         (should (eq prompt (plist-get (cdr (assoc :pending superchat--response-region-states))
                                      :prompt-marker))))))))

(ert-deftest output-prompt/real-delivery-failure-system-message-reuses-pending ()
  "O2's actual failure/notice route creates pending once after failed rendering."
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (observer)
       (superchat--response-region-consume-prompt)
       (superchat--prepare-for-response)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _) (setq observer superchat--llm-runtime-observer))))
         (superchat-runtime-test-submit (list :prompt "Q" :buffer view :session-id session
                                        :tools 'none :deadline nil)))
       (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                  (lambda (_) (error "O2 delivery fixture"))))
         (funcall observer 'terminal '(:status completed :response "DURABLE")))
       (superchat--insert-system-message "AFTER-FAILURE")
       (let ((prompt superchat--prompt-start))
         (superchat--response-region-ensure-view-prompt t)
         (should (eq prompt superchat--prompt-start))
         (should (eq prompt (plist-get (cdr (assoc :pending superchat--response-region-states))
                                      :prompt-marker))))))))

(ert-deftest output-context/claimed-failures-preserve-effects-and-b-view ()
  "Invalid presentation claims and diagnostic faults cannot interrupt Authority.
The `session-only' case is §6.1a's fail-closed-at-first-audit outcome,
asserted separately below: a context with no :run-id/:turn-id at all is
not a \"diagnostic fault\" the rest of this test's shared body models
(every other KIND mutates one field of a real, already-run-scoped
captured context), so it gets its own assertions instead of running
through the shared successful-execution body."
  (dolist (kind '(unknown mismatch missing-session wrong-run nil-owner status-error status-quit))
    (dolist (async '(nil t))
      (dolist (warning-fault '(nil error quit reentry))
        (cmd-clear-test--with-owned-view
         (lambda (view session)
           (let ((effects 0) (deliveries 0) (warnings 0) (counter (cons 0 nil))
                 context handle wrapped dones results)
             (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                        (lambda (&rest _)
                          (setq context (copy-sequence superchat--tool-execution-context)))))
               (setq handle (superchat-runtime-test-submit
                             (list :prompt "CONTEXT A" :buffer view :session-id session
                                   :tools 'none :deadline nil :transcript-owner "CTX-CUSTOM-A"
                                   :approval-function (lambda (_) t)))))
             (setq context (copy-sequence context))
             (pcase kind
               ('unknown (setq context (plist-put context :run-id "UNKNOWN")))
               ('mismatch (setq context (plist-put context :session-id "MISMATCH")))
               ('missing-session (setq context (plist-put context :session-id nil))))
             (superchat--response-region-begin "CTX-B")
             (superchat--response-region-stream "B PART" "CTX-B")
             (let* ((before (buffer-string))
                    (projection (mapcar #'symbol-value
                                        '(superchat--response-region-current-owner
                                          superchat--response-start-marker
                                          superchat--assistant-response-start-marker
                                          superchat--assistant-response-end-marker
                                          superchat--response-region-persistent-marker
                                          superchat--prompt-start superchat--current-response-parts)))
                    (status (symbol-function 'superchat-runtime-status))
                    (implementation (if async
                                        (lambda (done) (cl-incf effects) (push done dones))
                                      (lambda () (cl-incf effects) "RESULT"))))
               (cl-letf (((symbol-function 'superchat-runtime-status)
                          (lambda (run)
                            (pcase kind
                              ('status-error (error "eligibility diagnostic"))
                              ('status-quit (signal 'quit nil))
                              ('nil-owner (plist-put (funcall status run) :transcript-owner nil))
                              ('wrong-run (plist-put (funcall status run) :run-id "WRONG-PUBLISHED-RUN"))
                              (_ (funcall status run))))))
                 (setq wrapped (superchat--agent-wrap-function
                                "read_buffer" implementation async nil counter view context)))
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (&rest _)
                            (cl-incf warnings)
                            (pcase warning-fault
                              ('error (error "warning error"))
                              ('quit (signal 'quit nil))
                              ('reentry
                               (if async
                                   (funcall wrapped (lambda (v) (cl-incf deliveries) (push v results)))
                                 (should (equal "RESULT" (funcall wrapped))))
                               (signal 'quit nil))))))
                 (if async
                     (funcall wrapped (lambda (v) (cl-incf deliveries) (push v results)))
                   (should (equal "RESULT" (funcall wrapped))))
                 (dolist (done dones) (funcall done "RESULT") (funcall done "DUPLICATE")))
               (should (= 1 warnings))
               (should (= (if (eq warning-fault 'reentry) 2 1) effects (car counter)))
               (when async
                 (should (= effects deliveries))
                 (should (equal results (make-list effects "RESULT"))))
               (should (equal-including-properties before (buffer-string)))
               (should (equal projection
                              (mapcar #'symbol-value
                                      '(superchat--response-region-current-owner
                                        superchat--response-start-marker
                                        superchat--assistant-response-start-marker
                                        superchat--assistant-response-end-marker
                                        superchat--response-region-persistent-marker
                                        superchat--prompt-start superchat--current-response-parts))))
               (let ((events (superchat-tape-replay-events
                              (if (eq kind 'mismatch) "MISMATCH" session))))
                 (should (= effects (cl-count "tool.requested" events
                                              :key (lambda (e) (plist-get e :type)) :test #'equal)))
                 (should (= effects (cl-count "tool.completed" events
                                              :key (lambda (e) (plist-get e :type)) :test #'equal))))
               (should (superchat-runtime-status (plist-get handle :run-id))))))))))
  ;; §6.1a: `session-only' (a context naming a Session but no Run at all —
  ;; no :run-id/:turn-id) fails closed at the first Tape append
  ;; (`tool.requested'), before any approval or native call, for both the
  ;; sync and async wrapper shapes.
  (dolist (async '(nil t))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let* ((effects 0)
              (approvals 0)
              (before (buffer-string))
              (context (list :session-id session :attendance 'attended
                             :approval-function
                             (lambda (_) (cl-incf approvals) t)))
              (implementation (if async
                                  (lambda (done) (cl-incf effects) (funcall done "RESULT"))
                                (lambda () (cl-incf effects) "RESULT")))
              (wrapped (superchat--agent-wrap-function
                        "read_buffer" implementation async nil (cons 0 nil) view context))
              result)
         (if async
             (funcall wrapped (lambda (v) (setq result v)))
           (setq result (funcall wrapped)))
         (should (= 0 effects))
         (should (= 0 approvals))
         (should (stringp result))
         (should (string-match-p "audit failed" result))
         (should
          (= 0 (cl-count "tool.requested" (superchat-tape-replay-events session)
                         :key (lambda (e) (plist-get e :type)) :test #'equal)))
         (should (equal before (buffer-string))))))))

(ert-deftest output-context/approval-binder-keeps-real-custom-identity ()
  "An actual approval callback creates the native wrapper under full Runtime context."
  (unless (featurep 'llm) (ert-skip "Requires installed llm"))
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let ((effects 0) (approvals 0) context child)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _) (setq context (copy-sequence superchat--tool-execution-context)))))
         (superchat-runtime-test-submit
          (list :prompt "APPROVAL" :buffer view :session-id session :tools 'none :deadline nil
                :transcript-owner "APPROVAL-CUSTOM"
                :approval-function
                (lambda (_)
                  (cl-incf approvals)
                  (should (equal session (plist-get superchat--tool-execution-context :session-id)))
                  (should (equal (plist-get context :run-id)
                                 (plist-get superchat--tool-execution-context :run-id)))
                  (should (equal "APPROVAL-CUSTOM"
                                 (plist-get (superchat-runtime-status
                                             (plist-get superchat--tool-execution-context :run-id))
                                            :transcript-owner)))
                  (setq child (funcall (superchat--agent-make-wrapper)
                                       (make-llm-tool :name "read_buffer" :args nil
                                                      :description "approval child"
                                                      :function (lambda () (cl-incf effects) "CHILD"))))
                  t))))
       (let ((outer (superchat--agent-wrap-function
                     "context_approval_probe" (lambda () (cl-incf effects) "OUTER")
                     nil nil (cons 0 nil) view context)))
         (should (equal "OUTER" (funcall outer))))
       (should (= 1 approvals))
       (should (equal "CHILD" (funcall (llm-tool-function child))))
       (should (= 2 effects))
       (should (string-match-p "Tool call: read_buffer" (buffer-string)))
       (should (assoc "APPROVAL-CUSTOM" superchat--response-region-states))
       (should (= 2 approvals))
       (should (= 2 (cl-count "approval.decided" (superchat-tape-replay-events session)
                              :key (lambda (e) (plist-get e :type)) :test #'equal)))))))

(ert-deftest output-context/capture-once-and-nil-legacy-compatibility ()
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (context handle)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _) (setq context (copy-sequence superchat--tool-execution-context)))))
         (setq handle (superchat-runtime-test-submit
                       (list :prompt "CAPTURE" :buffer view :session-id session :tools 'none
                             :deadline nil :transcript-owner "ONCE-CUSTOM"
                             :approval-function (lambda (_) t)))))
       (let ((wrapped (superchat--agent-wrap-function
                       "read_buffer" (lambda () (ert-fail "Cancelled effect")) nil
                       nil (cons 0 nil) view context)))
         (superchat-runtime-cancel (plist-get handle :run-id))
         (superchat--response-region-begin "B")
         (superchat--response-region-stream "B-PART" "B")
         (rename-buffer (generate-new-buffer-name " *context-renamed*"))
         (cl-letf (((symbol-function 'superchat-runtime-status)
                    (lambda (_) (ert-fail "Presentation queried after capture"))))
           (should (string-match-p "no longer active" (funcall wrapped))))
         (should (equal '("B-PART") superchat--current-response-parts))
         (should (equal "B" superchat--response-region-current-owner))
         (should (string-match-p "Tool result" (buffer-string))))
       ;; §6.1a: a legacy nil context claims no Runtime Run, so it carries no
       ;; :run-id/:turn-id.  S03b's append-time identity check at the first
       ;; Tape row (`tool.requested') fails closed before approval or the
       ;; native call, rather than running with no Runtime isolation.  The
       ;; nil argument below is `superchat--agent-wrap-function''s own
       ;; RUNTIME-CONTEXT parameter (the wrapper) -- a literal nil context
       ;; handed straight to the public `superchat-tool-execute' would
       ;; instead fail its earlier, unrelated :session-id precondition
       ;; before ever reaching Tape, which is not the case this exercises.
       (dolist (async '(nil t))
         (let* ((effects 0) (warnings 0) (approvals 0)
                (superchat-authority-approval-function
                 (lambda (_) (cl-incf approvals) t))
                (before
                 (cl-count "tool.requested" (superchat-tape-replay-events session)
                          :key (lambda (e) (plist-get e :type)) :test #'equal))
                (implementation (if async
                                    (lambda (done) (cl-incf effects) (funcall done "LEGACY"))
                                  (lambda () (cl-incf effects) "LEGACY")))
                result)
           (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) (cl-incf warnings))))
             (let ((wrapped (superchat--agent-wrap-function
                            "read_buffer" implementation async nil (cons 0 nil) view nil)))
               (if async
                   (funcall wrapped (lambda (v) (setq result v)))
                 (setq result (funcall wrapped)))))
           (should (= 0 effects))
           (should (= 0 approvals))
           (should (= 0 warnings))
           (should (stringp result))
           (should (string-match-p "audit failed" result))
           (should
            (= before
               (cl-count "tool.requested" (superchat-tape-replay-events session)
                        :key (lambda (e) (plist-get e :type)) :test #'equal)))
           (should-not (string-match-p "LEGACY" (buffer-string)))))))))

(ert-deftest output-context/diagnostic-containment-does-not-swallow-consumer-or-tool-quit ()
  (dolist (consumer '(nil t))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let ((effects 0) done caught
             (context (list :session-id session :attendance 'attended
                            :run-id "run-1" :turn-id "turn-1"
                            :approval-function (lambda (_) t))))
         (let ((wrapped (superchat--agent-wrap-function
                         "read_buffer"
                         (if consumer
                             (lambda (callback) (cl-incf effects) (setq done callback))
                           (lambda () (cl-incf effects) (signal 'quit '(original-tool))))
                         consumer nil (cons 0 nil) view context)))
           (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) (signal 'quit '(diagnostic)))))
             (condition-case condition
                 (if consumer
                     (progn
                       (funcall wrapped (lambda (_) (signal 'quit '(original-consumer))))
                       (funcall done "RESULT"))
                   (funcall wrapped))
               (quit (setq caught condition))))
           (should (= 1 effects))
           (should (equal caught (list 'quit (if consumer 'original-consumer 'original-tool))))))))))

(ert-deftest output-o3/real-runtime-durable-final-and-explicit-rebuild ()
  "Formatter failure preserves the live View while Runtime completes durably once.

The `quit' half diverges from `error' by contract (S01 §3.3 \"Terminal
quit\", review finding F9): the terminal payload callback's `quit' is
captured, the aggregate `delivery.failed' receipt is appended, completion
hooks and quiesce still run, and only then is the original `quit'
re-signalled — a C-g during rendering must still reach the caller.  The
`error' half is unchanged: an `error' from that same callback is fully
absorbed into the one warning below, never re-signalled."
  (dolist (fault '(error quit))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let (observer partial handle warnings)
         (superchat--response-region-consume-prompt)
         (superchat--prepare-for-response)
         (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _done stream &rest _)
                      (setq observer superchat--llm-runtime-observer partial stream))))
           (setq handle (superchat-runtime-test-submit
                         (list :prompt "O3 QUESTION" :buffer view :session-id session
                               :tools 'none :deadline nil :transcript-owner "O3-CUSTOM"))))
         (funcall partial '(:reasoning "VISIBLE REASONING"))
         (funcall partial "VISIBLE PARTIAL")
         (let ((before-point (point-marker))
               (parts superchat--current-response-parts)
               (start superchat--assistant-response-start-marker))
           (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                      (lambda (_) (signal fault '("O3 FINAL FORMAT FAULT"))))
                     ((symbol-function 'display-warning)
                      (lambda (_type message &rest _) (push message warnings))))
             (if (eq fault 'quit)
                 ;; `should-error' with `:type 'quit' lands ERT's own QUIT
                 ;; result category instead of being caught here; catch it
                 ;; explicitly to keep asserting on it below.
                 (let (caught)
                   (condition-case err
                       (funcall observer 'terminal '(:status completed :response "EXACT DURABLE FINAL"))
                     (quit (setq caught err)))
                   (should (equal '(quit "O3 FINAL FORMAT FAULT") caught)))
               (funcall observer 'terminal '(:status completed :response "EXACT DURABLE FINAL"))))
           ;; N6: the terminal `:status' clears the status line before the
           ;; payload callback even attempts to render (contract §3.3), so a
           ;; formatter fault inside that payload leaves the partial answer
           ;; in place with its waiting status line already gone -- not a
           ;; byte-identical pre-fault snapshot.
           (let ((after (buffer-string)))
             (should (string-match-p "VISIBLE REASONING" after))
             (should (string-match-p "VISIBLE PARTIAL" after))
             (should-not (string-match-p "Prefill / waiting" after))
             (should-not (string-match-p "EXACT DURABLE FINAL" after)))
           ;; A marker, not a raw integer: the status-line clear above
           ;; legitimately deletes text before point, shifting any bare
           ;; position; a marker tracks the same logical place through that.
           (should (= (marker-position before-point) (point)))
           (should (eq parts superchat--current-response-parts))
           (should (eq start superchat--assistant-response-start-marker))
           (should (= 1 (length warnings)))
           (should (string-match-p "delivery failed" (car warnings)))
           (funcall observer 'terminal '(:status completed :response "DUPLICATE MUST NOT APPEAR"))
           (let ((after (buffer-string)))
             (should (string-match-p "VISIBLE REASONING" after))
             (should (string-match-p "VISIBLE PARTIAL" after))
             (should-not (string-match-p "Prefill / waiting" after))
             (should-not (string-match-p "EXACT DURABLE FINAL" after))
             (should-not (string-match-p "DUPLICATE MUST NOT APPEAR" after))))
         (let* ((run-id (plist-get handle :run-id))
                (events (superchat-tape-replay-events session)))
           (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status)))
           (when (eq fault 'quit)
             (should (member "delivery.failed"
                             (mapcar (lambda (e) (plist-get e :type)) events))))
           (should (= 1 (cl-count "response.completed" events
                                  :key (lambda (e) (plist-get e :type)) :test #'equal)))
           (should (eq 'proved (plist-get (superchat-closure-judge
                                          (list :kind 'durable-run-terminal :run-id run-id)
                                          events) :verdict)))
           (should (= 1 (cl-count "EXACT DURABLE FINAL"
                                  (superchat-db-tape-replay session)
                                  :key (lambda (row) (nth 3 row)) :test #'equal)))
           (should-not (string-match-p "DUPLICATE MUST NOT APPEAR" (prin1-to-string events))))
         ;; Explicit rebuild is healthy after the fault injection ends; no auto-retry.
         (let ((rebuilt (superchat--render-session-view session)))
           (unwind-protect
               (with-current-buffer rebuilt
                 (should (string-match-p "EXACT DURABLE FINAL" (buffer-string)))
                 (should (string-match-p "EXACT DURABLE FINAL"
                                         (prin1-to-string superchat--conversation-history))))
             (unless (eq rebuilt view) (kill-buffer rebuilt)))))))))

(ert-deftest output-prompt/real-borrowed-unbounded-notice-survives-final ()
  "O4 stays supported without a prompt or an implicit generic seal.

Rewritten for S01: a real production \"borrowed View\" is an ordinary chat
buffer (`superchat-mode') that EditorWorkspace does not own (`superchat.el'
~2593), not a plain non-chat buffer — the latter's default rendering is
exactly the D11 defect B-inv-1/2 forbid (contract §2.3: no callback and any
other buffer discards).  This models the real case: a second, unowned chat
View for the same Session, not the fixture's owned one."
  (cmd-clear-test--with-owned-view
   (lambda (_view session)
     (let ((borrowed (generate-new-buffer " *o4-borrowed-view*")))
       (unwind-protect
           (with-current-buffer borrowed
             (superchat-mode 1)
             (setq-local superchat--session-id session)
             (let ((superchat-buffer-name (buffer-name)) observer partial)
               (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                          (lambda (_prompt _done stream &rest _)
                            (setq observer superchat--llm-runtime-observer partial stream))))
                 (superchat-runtime-test-submit (list :prompt "BORROWED" :buffer (current-buffer)
                                                :session-id session :tools 'none :deadline nil)))
               (funcall partial "PART")
               (let ((state (cdr (assoc superchat--response-region-current-owner
                                        superchat--response-region-states))))
                 (should (plist-get state :assistant-start))
                 (should-not (plist-get state :assistant-end))
                 (superchat--response-region-insert-transcript "INDEPENDENT" :independent-turn)
                 (should (plist-get state :assistant-end)))
               (funcall observer 'terminal '(:status completed :response "BORROWED FINAL"))
               (should (string-match-p "INDEPENDENT" (buffer-string)))
               (should (string-match-p "BORROWED FINAL" (buffer-string)))))
         (when (buffer-live-p borrowed) (kill-buffer borrowed)))))))

(ert-deftest output-p2/real-workflow-headers-prompt-and-interleaved-owner ()
  "Real Workflow/LLM/Closure keeps its headers and draft around another owner."
  (unless (and (featurep 'llm) (require 'llm-openai nil t))
    (ert-skip "P2 Workflow requires installed llm 0.31.1"))
  (cmd-clear-test--with-owned-view
   (lambda (_view session)
     (let* ((superchat-workflow-directory superchat-data-directory)
            (superchat-llm-backend (make-llm-openai :key "unused" :chat-model "p2"))
            (superchat-llm-tools-enabled nil)
            (superchat-show-ttft nil)
            (superchat-response-timeout nil)
            (file (expand-file-name "p2.workflow" superchat-workflow-directory))
            callbacks owner)
       (with-temp-file file (insert "P2-FIRST\nP2-SECOND\n"))
       (superchat--response-region-consume-prompt)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial response _error &optional _multi)
                    (push (list partial response) callbacks) nil)))
         (superchat-workflow-execute-async "p2")
         (setq owner superchat--response-region-current-owner)
         (should (string-prefix-p "workflow-p2-" owner))
         (should (< (string-match "Workflow: p2" (buffer-string))
                    (save-excursion (goto-char superchat--prompt-start)
                                    (line-beginning-position))))
         (goto-char (point-max)) (insert "P2-DRAFT")
         (superchat--response-region-begin "P2-OTHER")
         (superchat--response-region-stream "OTHER-PART" "P2-OTHER")
         (let ((superchat--runtime-transcript-owner "WRONG"))
           (funcall (caar callbacks) "WF-PART")
           (funcall (cadar callbacks) "WF-FIRST-FINAL")
           (should (= 2 (length callbacks)))
           (funcall (cadar callbacks) "WF-SECOND-FINAL"))
         (should (equal "P2-OTHER" superchat--response-region-current-owner))
         (should (equal '("OTHER-PART") superchat--current-response-parts))
         (superchat--response-region-close "OTHER-FINAL" t "P2-OTHER")
         (let ((text (buffer-string)))
           (dolist (part '("Step 1/2" "Step 2/2" "WF-FIRST-FINAL"
                           "WF-SECOND-FINAL" "Workflow complete" "P2-DRAFT" "OTHER-FINAL"))
             (should (string-match-p (regexp-quote part) text)))
           (should-not (string-match-p "WF-PART\\|OTHER-PART" text)))
         (let ((events (superchat-tape-replay-events session)))
           (should (= 2 (cl-count "run.completed" events
                                  :key (lambda (e) (plist-get e :type)) :test #'equal)))))))))

(ert-deftest output-p2/default-handoffs-initialize-without-status ()
  "Default stream and delivery initialize their owner without ambient selection."
  (dolist (stream-first '(nil t))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let (observer stream handle)
         (cl-letf (((symbol-function 'superchat--update-status) nil)
                   ((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _done partial &rest _)
                      (setq observer superchat--llm-runtime-observer
                            stream partial))))
           (setq handle
                 (superchat-runtime-test-submit
                  (list :prompt "P2" :buffer view :session-id session
                        :tools 'none :deadline nil :transcript-owner "P2-CUSTOM"))))
         (should-not (assoc "P2-CUSTOM" superchat--response-region-states))
         (let ((superchat--runtime-transcript-owner "WRONG"))
           (when stream-first (funcall stream "P2-PARTIAL"))
           (funcall observer 'terminal '(:status completed :response "P2-FINAL")))
         (should (equal "P2-CUSTOM" superchat--response-region-current-owner))
         (should-not (assoc "WRONG" superchat--response-region-states))
         (should (string-match-p "P2-FINAL" (buffer-string)))
         (should-not (string-match-p "P2-PARTIAL" (buffer-string)))
         (should (= 1 (save-excursion
                        (goto-char (point-min))
                        (how-many "^\\* User:" (point-min) (point-max)))))
         (should (eq 'completed (plist-get
                                (superchat-runtime-status (plist-get handle :run-id))
                                :status)))
         (should (eq 'proved
                     (plist-get (superchat-closure-judge
                                 (list :kind 'durable-run-terminal
                                       :run-id (plist-get handle :run-id))
                                 (superchat-tape-replay-events session)) :verdict))))))))

(ert-deftest output-p2/custom-callbacks-retain-dynamic-owner-and-arity ()
  "Legacy custom callbacks receive the captured owner and their original arity."
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (observer stream seen)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (_prompt _done partial &rest _)
                    (setq observer superchat--llm-runtime-observer stream partial))))
         (superchat-runtime-test-submit
          (list :prompt "P2" :buffer view :session-id session :tools 'none :deadline nil
                :transcript-owner "P2-CALLBACK"
                :stream-function
                (lambda (chunk)
                  (push (list 'stream chunk superchat--runtime-transcript-owner) seen))
                :delivery-function
                (lambda (response status tape-id)
                  (should (integerp tape-id))
                  (push (list status response superchat--runtime-transcript-owner) seen)))))
       (let ((superchat--runtime-transcript-owner "OUTSIDE"))
         (funcall stream "CHUNK")
         (funcall observer 'terminal '(:status completed :response "FINAL"))
         (funcall observer 'terminal '(:status completed :response "DUPLICATE"))
         (should (equal "OUTSIDE" superchat--runtime-transcript-owner)))
       (should (equal '((completed "FINAL" "P2-CALLBACK")
                        (stream "CHUNK" "P2-CALLBACK")) seen))))))

(ert-deftest cmd/clear-owned-view-accepts-real-submit ()
  "Clear must preserve its exact owned host for a real new-Session Run."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (superchat-tape-append-material old-session "user" "OLD-TAPE-SECRET")
     (setq-local superchat--active-preset
                 (superchat-preset--create :name "kept-profile" :type 'prompt
                                          :capability "chat")
                 superchat--active-capability "chat"
                 superchat--active-topology 'single
                 superchat--active-authority-policy "default"
                 superchat-authority-mode 'auto
                 superchat-authority--session-allowlist '("read_buffer"))
     (let ((old-rows (superchat-db-tape-replay old-session))
           (preset superchat--active-preset)
           (configuration (superchat--session-runtime-configuration))
           (allowlist superchat-authority--session-allowlist)
           (guardrails (superchat--agent-effective-guardrails superchat--active-preset))
           (name (buffer-name view))
           prompts)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (prompt _complete _stream &rest _args)
                    (push prompt prompts)
                    (funcall superchat--llm-runtime-observer
                             'terminal '(:status completed :response "NEW-ANSWER")))))
         (superchat--clear-chat-and-context)
         (should (eq preset superchat--active-preset))
         (should (equal configuration (superchat--session-runtime-configuration)))
         (should (eq allowlist superchat-authority--session-allowlist))
         (should (eq 'auto superchat-authority-mode))
         (should (equal guardrails (superchat--agent-effective-guardrails superchat--active-preset)))
         (should (equal name (buffer-name view)))
         (let* ((new-session superchat--session-id)
                (request (superchat-build-request-plan "NEW-QUESTION" new-session))
                (handle (superchat--submit-chat-request
                         (append (list :buffer view :tools 'none :deadline nil)
                                 request))))
           (should-not (equal old-session new-session))
           (should (equal new-session
                          (superchat-editor-workspace-owner-session view)))
           (should (= 1 (hash-table-count superchat-runtime--runs)))
           (should (eq 'completed
                       (plist-get (superchat-runtime-status
                                   (plist-get handle :run-id)) :status)))
           (should (equal new-session (plist-get handle :session-id)))
           (should (equal old-rows (superchat-db-tape-replay old-session)))
           (should (= 1 (length prompts)))
           (should-not (string-match-p "OLD-TAPE-SECRET" (car prompts)))))))))

(ert-deftest cmd/clear-owned-view-refusal-is-zero-mutation ()
  "A refused real cancellation must leave the entire View and Tape alone."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (let ((inhibit-read-only t)) (insert (make-string 160 ?x)))
     (with-temp-file (expand-file-name "session-cache.org" superchat-data-directory)
       (insert "CACHE-SENTINEL"))
     (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore)
               ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
       (let* ((handle (superchat-runtime-test-submit
                       (list :prompt "RUN" :session-id old-session
                             :buffer view :tools 'none :deadline nil
                             :delivery-function #'ignore)))
              (text (buffer-string))
              (rows (superchat-db-tape-replay old-session))
              (state (superchat-editor-workspace-state old-session)))
         (superchat--clear-chat-and-context)
         (should (equal text (buffer-string)))
         (should (equal rows (superchat-db-tape-replay old-session)))
         (should (equal old-session superchat--session-id))
         (should (equal old-session
                        (superchat-editor-workspace-owner-session view)))
         (should (eq state (superchat-editor-workspace-state old-session)))
         (should (equal "CACHE-SENTINEL"
                        (with-temp-buffer
                          (insert-file-contents
                           (expand-file-name "session-cache.org" superchat-data-directory))
                          (buffer-string))))
         (should (superchat-runtime-effect-capable-p
                  (plist-get handle :run-id))))))))

(defun cmd-clear-test--late-callback-matrix (async)
  "Check real cancellation and late output for an ASYNC or sync tool."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (let (observer stream wrapped run-id
           (effects 0) (deliveries 0) (tool-deliveries 0)
           stream-clean terminal-clean tool-clean tool-result)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (_prompt _complete stream-callback &rest _args)
                    (setq observer superchat--llm-runtime-observer
                          stream stream-callback
                          wrapped
                          (superchat--agent-wrap-function
                           "read_buffer"
                           (if async
                               (lambda (done &rest _args)
                                 (cl-incf effects)
                                 (funcall done "LATE-EFFECT"))
                             (lambda (&rest _args)
                               (cl-incf effects) "LATE-EFFECT"))
                           async nil (cons 0 nil) view
                           superchat--tool-execution-context)))))
         (setq run-id
               (plist-get
                (superchat-runtime-test-submit
                 (list :prompt "OLD-QUESTION" :session-id old-session
                       :buffer view :tools 'none :deadline nil
                       :approval-function (lambda (_request) t)
                       :delivery-function
                       (lambda (_value _status _tape-id)
                         (cl-incf deliveries)
                         (with-current-buffer view
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert "CUSTOM-DELIVERY"))))))
                :run-id)))
       ;; Explicitly cancel first to isolate late-output admission from clear's
       ;; still-missing active-Run cancellation/rollover implementation.
       (should (superchat-runtime-cancel run-id))
       (should (eq 'cancelled
                   (plist-get (superchat-runtime-status run-id) :status)))
       (should (= 1 deliveries))
       (superchat--clear-chat-and-context)
       (let ((clean-text (buffer-string)))
         (funcall stream "LATE-STREAM")
         (setq stream-clean (equal clean-text (buffer-string)))
         (funcall observer 'terminal '(:status completed :response "LATE-TERMINAL"))
         (setq terminal-clean (and (= 1 deliveries)
                                    (equal clean-text (buffer-string))))
         (if async
             (funcall wrapped
                      (lambda (result)
                        (cl-incf tool-deliveries)
                        (setq tool-result result))
                      "late-buffer")
           (setq tool-result (funcall wrapped "late-buffer")))
         (setq tool-clean (equal clean-text (buffer-string)))
         (message
          "CLEAR-LATE async=%S stream-clean=%S custom-delivery-clean=%S tool-clean=%S effects=%d callback-count=%d result=%S view=%S"
          async stream-clean terminal-clean tool-clean effects tool-deliveries
          tool-result (buffer-string)))
       ;; The oracle deliberately does not require zero old-Run audit events:
       ;; that separate contract belongs to Slice 05, not this View isolation.
       (should (= 0 effects))
       (when async (should (= 1 tool-deliveries)))
       (should stream-clean)
       (should terminal-clean)
       (should tool-clean)
       ;; Repeat delivery after an actual fresh-Session Run starts streaming.
       (let (new-stream new-handle)
         (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _complete callback &rest _)
                      (setq new-stream callback))))
           (setq new-handle
                 (superchat-runtime-test-submit
                  (list :prompt "NEW-RUN" :session-id superchat--session-id
                        :buffer view :tools 'none :deadline nil))))
         (funcall new-stream "NEW-STREAM")
         (let ((text (buffer-string)))
           (funcall stream "OLD-AFTER-NEW-RUN")
           (funcall observer 'terminal '(:status completed :response "OLD-TERMINAL"))
           (if async
               (funcall wrapped (lambda (_) (cl-incf tool-deliveries)) "late-again")
             (funcall wrapped "late-again"))
           (should (equal text (buffer-string)))
           (should (= 1 deliveries))
           (should (= 0 effects))
           (when async (should (= 2 tool-deliveries)))
           (should (superchat-runtime-effect-capable-p
                    (plist-get new-handle :run-id)))))))))

(ert-deftest cmd/clear-owned-view-real-cancel-late-sync-tool ()
  "A cancelled sync wrapper cannot render into the fresh View."
  (cmd-clear-test--late-callback-matrix nil))

(ert-deftest cmd/clear-owned-view-publication-error-and-quit ()
  "Each identity publication boundary must restore both identities and View."
  (dolist (variable '(superchat--session-id superchat--editor-workspace-session-id))
    (dolist (failure '(error quit))
      (cmd-clear-test--with-owned-view
       (lambda (view old-session)
         (let* ((text (buffer-string))
                (prompt superchat--prompt-start)
                (position (marker-position prompt))
                (watcher (lambda (_symbol value operation where)
                           (when (and (eq where view) (eq operation 'set)
                                      (not (equal value old-session)))
                             (signal failure '("publication test")))))
                caught)
           (unwind-protect
               (progn
                 (add-variable-watcher variable watcher)
                 (condition-case condition
                     (superchat--clear-chat-and-context)
                   ((error quit) (setq caught (car condition))))
                 (should (eq failure caught))
                 (should (equal text (buffer-string)))
                 (should (eq prompt superchat--prompt-start))
                 (should (equal position (marker-position prompt)))
                 (should (eq view (marker-buffer prompt)))
                 (should (equal old-session superchat--session-id))
                 (should (equal old-session
                                (superchat-editor-workspace-owner-session view)))
                 (should (eq 'open (superchat-editor-workspace-state old-session))))
             (remove-variable-watcher variable watcher))))))))

(ert-deftest cmd/clear-owned-view-reset-error-and-quit ()
  "A failing real Render reset must restore detached marker resources."
  (dolist (failure '(error quit))
    (cmd-clear-test--with-owned-view
     (lambda (view old-session)
       (let ((original (symbol-function 'superchat--response-region-reset))
             (text (buffer-string))
             (prompt superchat--prompt-start)
             (position (marker-position superchat--prompt-start))
             caught)
         (cl-letf (((symbol-function 'superchat--response-region-reset)
                    (lambda () (funcall original)
                      (signal failure '("reset test")))))
           (condition-case condition
               (superchat--clear-chat-and-context)
             ((error quit) (setq caught (car condition)))))
         (should (eq failure caught))
         (should (equal text (buffer-string)))
         (should (eq prompt superchat--prompt-start))
         (should (equal position (marker-position prompt)))
         (should (eq view (marker-buffer prompt)))
         (should (equal old-session superchat--session-id))
         (should (equal old-session
                        (superchat-editor-workspace-owner-session view))))))))

(defun answer-heading-test--check (answer)
  (human-input-test--with-chat
   (lambda (_view session requests)
     (human-input-test--send "Summarize SEO")
     (funcall (plist-get (car (funcall requests)) :final) answer)
     (font-lock-set-defaults)
     (save-excursion (font-lock-fontify-region (point-min) (point-max)))
     (let* ((assistant (save-excursion (goto-char (point-min))
                                     (re-search-forward "^\\*\\* Assistant") (line-beginning-position)))
            (title (save-excursion (goto-char assistant)
                                  (search-forward "SEO 知识要点总结") (line-beginning-position)))
            (chapter (save-excursion (goto-char title)
                                    (search-forward "一、基本概念") (line-beginning-position)))
            (prompt (superchat--response-region--view-prompt))
            (parent (save-excursion (goto-char title) (and (org-up-heading-safe) (point)))))
       (goto-char assistant) (org-fold-hide-subtree)
       (message "ANSWER_HEADING title-parent=%S assistant=%S title-level=%S chapter-level=%S title-hidden=%S next-input-hidden=%S"
                parent assistant (save-excursion (goto-char title) (org-outline-level))
                (save-excursion (goto-char chapter) (org-outline-level))
                (and (invisible-p title) t) (and (invisible-p prompt) t))
       ;; History has always trimmed outer whitespace in --record-message;
       ;; durable Runtime Tape below keeps the exact transport response.
       (should (cl-some (lambda (entry) (equal (plist-get entry :content) (string-trim answer)))
                        superchat--conversation-history))
       (should (cl-some (lambda (row) (equal (nth 3 row) answer))
                        (superchat-db-tape-replay session)))
       (should-not (invisible-p prompt))
       (should-not (invisible-p (1- prompt)))
       (should (equal parent assistant))
       (should (invisible-p title))
       (should (invisible-p chapter))))))
(ert-deftest answer-heading/real-markdown-descends-from-assistant ()
  (answer-heading-test--check "# SEO 知识要点总结\n\n## 一、基本概念\nSEO BODY\n\n### 子节\nDETAIL\n\n```org\n* code star\n,* literal comma\n#+code\n```\n"))

(defun cmd-clear-test--same-session-late-tool (async custom-owner control)
  "Run real A/B/C transport scheduling; optionally deliver ASYNC late A tool."
  (let (answer)
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let (captures wrapped a b (effects 0) (callbacks 0))
         (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _complete stream &rest _)
                      (push (list :stream stream :observer superchat--llm-runtime-observer
                                  :context (copy-sequence superchat--tool-execution-context))
                            captures))))
           (setq a (superchat-runtime-test-submit
                    (append (list :prompt "A-QUESTION" :session-id session :buffer view
                                  :tools 'none :deadline nil
                                  :approval-function (lambda (_request) t))
                            (when custom-owner (list :transcript-owner custom-owner)))))
           (let ((original
                  (if async (lambda (done &rest _) (cl-incf effects) (funcall done "EFFECT"))
                    (lambda (&rest _) (cl-incf effects) "EFFECT")))
                 (superchat--tool-execution-context (plist-get (car captures) :context)))
             (setq wrapped
                   (if (featurep 'llm)
                       (llm-tool-function
                        (funcall (superchat--agent-make-wrapper)
                                 (make-llm-tool :name "read_buffer" :description "Local fixture"
                                                :args nil :async async :function original)))
                     (superchat--agent-wrap-function
                      "read_buffer" original async nil (cons 0 nil) view
                      superchat--tool-execution-context)))
             (message "SAME-SESSION real-llm-make-wrapper-chain=%S custom-owner=%S async=%S"
                      (featurep 'llm) custom-owner async))
           (when custom-owner
             (should (equal custom-owner
                            (plist-get (superchat-runtime-status (plist-get a :run-id))
                                       :transcript-owner))))
           (funcall (plist-get (car captures) :stream) "A-PART")
           (superchat-runtime-cancel (plist-get a :run-id) "A-CANCELLED")
           (setq b (superchat-runtime-test-submit
                    (list :prompt "B-QUESTION" :session-id session :buffer view
                          :tools 'none :deadline nil)))
           (let* ((capture-b (car captures))
                  (owner-b (plist-get (superchat-runtime-status (plist-get b :run-id))
                                     :transcript-owner)))
             (funcall (plist-get capture-b :stream) "B-PART1")
             (let ((start superchat--assistant-response-start-marker)
                   (end superchat--assistant-response-end-marker)
                   (parts superchat--current-response-parts))
               (unless control
                 (message "SAME-SESSION before-late states=%S text=%S"
                          superchat--response-region-states (buffer-string))
                 (if async
                     (funcall wrapped (lambda (_result) (cl-incf callbacks)) "LATE-A")
                   (should (string-match-p "no longer active" (funcall wrapped "LATE-A"))))
                 (should (equal owner-b superchat--response-region-current-owner))
                 (should (eq start superchat--assistant-response-start-marker))
                 (should (eq end superchat--assistant-response-end-marker))
                 (should (equal parts superchat--current-response-parts))
                 (should (marker-position start))
                 (let* ((text (buffer-string))
                        ;; N6: a cancelled Run's own render is its partial
                        ;; answer plus the `[Stopped]' marker, never the
                        ;; cancel reason text -- locate A's block by that,
                        ;; not by "A-CANCELLED".  Guard each `string-match'
                        ;; result so a missing marker fails as an assertion,
                        ;; not a `wrong-type-argument' inside `<'.
                        (a-part (string-match "A-PART" text))
                        (stopped (string-match "\\[Stopped\\]" text))
                        (tool-call (string-match "Tool call: read_buffer" text))
                        (tool-result (string-match "Tool result: read_buffer" text))
                        (b-part1 (string-match "B-PART1" text)))
                   (should a-part)
                   (should stopped)
                   (should tool-call)
                   (should tool-result)
                   (should b-part1)
                   (should (< a-part stopped))
                   (should (< stopped tool-call))
                   (should (< tool-result b-part1))))
             ;; This is intentionally outside any Runtime dynamic binding.
             (superchat--response-region-insert-transcript "INDEPENDENT-NOTE" :independent-turn)
             (should (equal owner-b superchat--response-region-current-owner))
             (funcall (plist-get capture-b :stream) "B-PART2")
             (funcall (plist-get capture-b :observer)
                      'terminal '(:status completed :response "B-FINAL"))
             (should (eq 'completed (plist-get (superchat-runtime-status (plist-get b :run-id)) :status)))))
           (superchat-runtime-test-submit
            (list :prompt "C-QUESTION" :session-id session :buffer view :tools 'none :deadline nil))
           (funcall (plist-get (car captures) :stream) "C-PART")
           (funcall (plist-get (car captures) :observer) 'terminal
                    '(:status completed :response "C-FINAL"))
           (should (= 0 effects))
           (should (= callbacks (if (and async (not control)) 1 0)))
           (setq answer (buffer-substring-no-properties (point-min) (point-max)))
           (unless control
             (should (string-match-p "Tool call: read_buffer" answer))
             (should (string-match-p "Tool result: read_buffer" answer)))
           (should-not (string-match-p "B-PART\\|C-PART" answer))))))
    answer))

(ert-deftest cmd/clear-owned-view-same-session-late-tool-matrix ()
  "Late A records preserve B rendering and C, with actual and custom owners."
  (let ((control (cmd-clear-test--same-session-late-tool nil nil t)))
    (dolist (async '(nil t))
      (dolist (owner '(nil "CUSTOM-A-OWNER"))
        (let ((actual (cmd-clear-test--same-session-late-tool async owner nil)))
          (should (= 3 (with-temp-buffer
                         (insert actual) (goto-char (point-min))
                         (let ((count 0))
                           (while (re-search-forward "^\\*\\* Assistant" nil t)
                             (cl-incf count)) count))))
          (should (equal (substring control (string-match "B-FINAL" control))
                         (substring actual (string-match "B-FINAL" actual)))))))))

(ert-deftest cmd/clear-owned-view-cancel-mutation-matrix ()
  "Real cancellation deliveries invalidate every changed rollover precondition."
  (dolist (mutation '(rename kill role owner ui target target-state reenter))
    (cmd-clear-test--with-owned-view
     (lambda (view old-session)
       (let ((target "clear-mutation-target") caught reentry-rejected replacement)
         (unwind-protect
             (cl-letf (((symbol-function 'superchat--new-session-id) (lambda () target))
                       ((symbol-function 'yes-or-no-p) (lambda (_) t))
                       ((symbol-function 'superchat--llm-generate-answer) #'ignore))
               (let* ((name (buffer-name view))
                      (handle
                       (superchat-runtime-test-submit
                        (list :prompt "OLD" :session-id old-session :buffer view
                              :tools 'none :deadline nil
                              :delivery-function
                              (lambda (&rest _)
                                (with-current-buffer view
                                  (pcase mutation
                                    ('rename (rename-buffer (generate-new-buffer-name " *renamed-clear*")))
                                    ('kill (setq-local kill-buffer-hook nil)
                                           (kill-buffer view)
                                           (setq replacement (get-buffer-create name)))
                                    ('role (setq-local superchat--editor-workspace-role 'protocol-host))
                                    ('owner (setq-local superchat--editor-workspace-session-id "changed-owner"))
                                    ('ui (setq-local superchat--session-id "changed-ui"))
                                    ('target (superchat-tape-append-material target "user" "OCCUPIED"))
                                    ('target-state (puthash target 'closed superchat-editor-workspace--states))
                                    ('reenter
                                     (condition-case nil (superchat--clear-chat-and-context)
                                       (error (setq reentry-rejected t)))))))))))
                 (condition-case condition (superchat--clear-chat-and-context)
                   ((error quit) (setq caught condition)))
                 (should (eq 'cancelled (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)))
                 (if (eq mutation 'reenter)
                     (progn (should reentry-rejected)
                            (should-not caught)
                            (should (equal target (superchat-editor-workspace-owner-session view))))
                   (should caught)
                   (when (buffer-live-p view)
                     (should (equal (if (eq mutation 'owner) "changed-owner" old-session)
                                    (superchat-editor-workspace-owner-session view)))
                     (should (equal (if (eq mutation 'ui) "changed-ui" old-session)
                                    (buffer-local-value 'superchat--session-id view)))))))
           (when (buffer-live-p replacement) (kill-buffer replacement))))))))

(ert-deftest cmd/clear-owned-view-target-material-is-not-fresh ()
  "An event-free Tape with real material still occupies a target Session."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (superchat-tape-append-material "material-target" "user" "EXISTING")
     (let ((text (buffer-string)) (called nil))
       (should-error
        (superchat-editor-workspace-rollover-session-view
         view old-session "material-target" (lambda (_publish) (setq called t))))
       (should-not called)
       (should (equal text (buffer-string)))
       (should (equal old-session superchat--session-id))))))

(ert-deftest cmd/clear-owned-view-rejects-invalid-exact-metadata ()
  "The dedicated action never adopts malformed, foreign or unowned buffers."
  (dolist (mutation '(unowned partial invalid role inherited-role ui old-state same-target))
    (cmd-clear-test--with-owned-view
     (lambda (view old-session)
       (pcase mutation
         ('unowned (kill-local-variable 'superchat--editor-workspace-owned-p)
                   (kill-local-variable 'superchat--editor-workspace-session-id))
         ('partial (kill-local-variable 'superchat--editor-workspace-session-id))
         ('invalid (setq-local superchat--editor-workspace-owned-p nil))
         ('role (setq-local superchat--editor-workspace-role 'protocol-host))
         ('inherited-role (kill-local-variable 'superchat--editor-workspace-role))
         ('ui (setq-local superchat--session-id "foreign-session"))
         ('old-state (puthash old-session 'retained superchat-editor-workspace--states)))
       (let ((text (buffer-string))
             (locals (buffer-local-variables))
             (state (superchat-editor-workspace-state old-session)) called)
         (should-error
          (superchat-editor-workspace-rollover-session-view
           view old-session (if (eq mutation 'same-target) old-session "fresh-target")
           (lambda (_) (setq called t))))
         (should-not called)
         (should (equal text (buffer-string)))
         (should (equal locals (buffer-local-variables)))
         (should (eq state (superchat-editor-workspace-state old-session))))))))

(ert-deftest cmd/clear-foreign-primary-is-unchanged ()
  "A name collision with a foreign buffer never grants clear authority."
  (with-temp-buffer
    (let ((superchat-buffer-name (buffer-name)))
      (insert "FOREIGN-CONTENT")
      (let ((text (buffer-string)) (locals (buffer-local-variables)))
        (should-error (superchat--clear-chat-and-context))
        (should (equal text (buffer-string)))
        (should (equal locals (buffer-local-variables)))))))

(ert-deftest cmd/clear-owned-view-partial-cancellation-is-not-rollback ()
  "A fault after a real cancel preserves its terminal fact and the other Run."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (let ((cancel (symbol-function 'superchat-runtime-cancel)) handles caught)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore)
                 ((symbol-function 'yes-or-no-p) (lambda (_) t)))
         (dotimes (_ 2)
           (push (superchat-runtime-test-submit
                  (list :prompt "PARTIAL" :session-id old-session :buffer view
                        :tools 'none :deadline nil :delivery-function #'ignore)) handles))
         (let ((text (buffer-string)))
           ;; Fault injection retains the real cancel, SQLite facts and status.
           (cl-letf (((symbol-function 'superchat-runtime-cancel)
                      (lambda (&rest args)
                        (apply cancel args)
                        (error "fault after actual cancellation"))))
             (condition-case condition (superchat--clear-chat-and-context)
               (error (setq caught condition))))
           (should caught)
           (should (equal old-session superchat--session-id))
           (should (equal old-session (superchat-editor-workspace-owner-session view)))
           (let ((statuses (mapcar
                            (lambda (handle)
                              (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status))
                            handles))
                 (waiting-line "Prefill / waiting first token… 0s"))
             (should (= 1 (cl-count 'cancelled statuses)))
             (should (= 1 (cl-count 'running-model statuses)))
             ;; Cancellation is not rollback: the still-running Run's own
             ;; waiting line survives untouched; only the cancelled Run's is
             ;; cleared (N6) -- not restored to the pre-fault byte-identical
             ;; text.
             (should (= 2 (cl-count waiting-line (split-string text "\n") :test #'equal)))
             (should (= 1 (cl-count waiting-line (split-string (buffer-string) "\n") :test #'equal))))))))))

(ert-deftest cmd/clear-owned-view-delayed-admitted-result-keeps-owner ()
  "An already admitted async result arrives in A's block during B streaming."
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (captures finish wrapped a b (effects 0) (deliveries 0))
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (_prompt _complete stream &rest _)
                    (push (list :stream stream :observer superchat--llm-runtime-observer
                                :context (copy-sequence superchat--tool-execution-context)) captures))))
         (setq a (superchat-runtime-test-submit
                  (list :prompt "A" :session-id session :buffer view :tools 'none
                        :deadline nil :transcript-owner "DELAYED-CUSTOM-A"
                        :approval-function (lambda (_) t))))
         (setq wrapped (superchat--agent-wrap-function
                        "read_buffer" (lambda (done &rest _) (cl-incf effects) (setq finish done))
                        t nil (cons 0 nil) view (plist-get (car captures) :context)))
         (funcall wrapped (lambda (_) (cl-incf deliveries)) "ADMITTED")
         (should (= 1 effects))
         (should (= 0 deliveries))
         (superchat-runtime-cancel (plist-get a :run-id))
         (should-not (superchat-runtime-effect-capable-p (plist-get a :run-id)))
         (setq b (superchat-runtime-test-submit
                  (list :prompt "B" :session-id session :buffer view :tools 'none :deadline nil)))
         (funcall (plist-get (car captures) :stream) "B-MID")
         (let ((owner superchat--response-region-current-owner)
               (start superchat--assistant-response-start-marker))
           (funcall finish "DELAYED-A-RESULT")
           (funcall finish "DUPLICATE")
           (should (= 1 deliveries))
           (should (= 1 effects))
           (should (equal owner superchat--response-region-current-owner))
           (should (eq start superchat--assistant-response-start-marker))
           (should (< (string-match "DELAYED-A-RESULT" (buffer-string))
                      (string-match "B-MID" (buffer-string)))))
         (funcall (plist-get (car captures) :observer) 'terminal
                  '(:status completed :response "B-FINAL"))
         (should (eq 'completed (plist-get (superchat-runtime-status (plist-get b :run-id)) :status)))
         (should (string-match-p "DELAYED-A-RESULT" (buffer-string)))
         (should-not (string-match-p "B-MID\\|DUPLICATE" (buffer-string))))))))

(ert-deftest cmd/clear-owned-view-real-cancel-late-async-tool ()
  "A cancelled async wrapper cannot render into the fresh View."
  (cmd-clear-test--late-callback-matrix t))

(ert-deftest cmd/clear-owned-view-cancel-reentry-new-run-blocks-rollover ()
  "A new Run submitted by real cancellation delivery invalidates rollover."
  (cmd-clear-test--with-owned-view
   (lambda (view old-session)
     (let ((confirmations 0) (deliveries 0) handle child)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args) nil))
                 ((symbol-function 'yes-or-no-p)
                  (lambda (_prompt) (cl-incf confirmations) t)))
         (setq handle
               (superchat-runtime-test-submit
                (list :prompt "OLD-QUESTION" :session-id old-session
                      :buffer view :tools 'none :deadline nil
                      :delivery-function
                      (lambda (_value _status _tape-id)
                        (cl-incf deliveries)
                        (unless child
                          (setq child
                                (superchat-runtime-test-submit
                                 (list :prompt "REENTRANT-QUESTION"
                                       :session-id old-session :buffer view
                                       :tools 'none :deadline nil
                                       :delivery-function #'ignore))))))))
         (condition-case nil
             (superchat--clear-chat-and-context)
           (error nil))
         (message "CLEAR-REENTRY confirmations=%d deliveries=%d old-status=%S child=%S ui-session=%S owner=%S"
                  confirmations deliveries
                  (plist-get (superchat-runtime-status
                              (plist-get handle :run-id)) :status)
                  child superchat--session-id
                  (superchat-editor-workspace-owner-session view))
         (should (= 1 confirmations))
         (should (= 1 deliveries))
         (should child)
         (should (eq 'cancelled (plist-get (superchat-runtime-status
                                           (plist-get handle :run-id)) :status)))
         (should (superchat-runtime-effect-capable-p (plist-get child :run-id)))
         (should (equal old-session superchat--session-id))
         (should (equal old-session
                        (superchat-editor-workspace-owner-session view))))))))

(defun test-cmd-dispatch--recall-scope-fixture (thunk)
  "Call THUNK with a fresh temporary Tape database for cross-session recall."
  (let* ((tmp-dir (make-temp-file "superchat-cmd-dispatch-recall-" t))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn (superchat-db-open) (funcall thunk))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest cmd-dispatch/recall-command-scope-all ()
  "`/recall' with the default `all' scope finds rows from other sessions."
  (require 'superchat)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (superchat-tape-append-material
      "recall-cmd-session-a" "user" "the cat is named RECALLCMDTOM")
     (let ((superchat--session-id "recall-cmd-session-b")
           (superchat--pending-recalled-memories nil))
       (superchat--cmd-recall "recall" "RECALLCMDTOM" nil nil nil)
       (should (cl-some
                (lambda (memory)
                  (string-match-p "RECALLCMDTOM" (plist-get memory :content)))
                superchat--pending-recalled-memories))))))

(ert-deftest cmd-dispatch/auto-recall-searches-all-sessions-and-prefers-current ()
  "Auto-recall searches every session, prefers the current session's own
matches, and does not re-inject rows still inside its own recent
`superchat-context-message-count' window (they are already history)."
  (require 'superchat)
  (require 'superchat-dispatcher)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (superchat-tape-append-material
      "auto-recall-session-a" "anchor" "ALPHATOMXYZ noted by session A"
      :topic "remember")
     (superchat-tape-append-material
      "auto-recall-session-b" "assistant" "ALPHATOMXYZ mentioned long ago")
     (dotimes (i 3)
       (superchat-tape-append-material
        "auto-recall-session-b" "user" (format "unrelated filler turn %d" i)))
     (superchat-tape-append-material
      "auto-recall-session-b" "user" "ALPHATOMXYZ mentioned just now")
     (let* ((superchat-context-message-count 2)
            (turn (superchat-turn-new "ALPHATOMXYZ" "auto-recall-session-b")))
       (superchat--attach-indexed-memories turn "ALPHATOMXYZ" "auto-recall-session-b")
       (let ((memories (superchat-turn-retrieved-memories turn)))
         (should memories)
         ;; The own-session row still inside the last 2 tape rows is already
         ;; part of ordinary conversation history; auto-recall must not
         ;; duplicate it as a "memory".
         (should-not
          (cl-some (lambda (memory)
                     (string-match-p "mentioned just now" (plist-get memory :content)))
                   memories))
         ;; A same-session match outside that window is preferred first.
         (should (string-match-p "mentioned long ago"
                                 (plist-get (car memories) :content)))
         ;; The cross-session anchor from session A is still found.
         (should (cl-some
                  (lambda (memory)
                    (string-match-p "noted by session A" (plist-get memory :content)))
                  memories)))))))

(ert-deftest cmd-dispatch/admit-stage-decides-queue-for-active-run ()
  "S05b's chat `admit' implementation (owner `chat', id `chat-active-run')
queues only a `chat' envelope whose SNAPSHOT shows an active Run; either
condition's absence returns nil so the stage's core default `process'
applies (contract §4 stage 2, §7.5 admit action table)."
  (require 'superchat-dispatcher)
  (should (equal
           (list :action 'queue :reason "run-active" :target-run-id "run-1")
           (superchat-stage-call
            'admit "session-1" (list :surface 'chat :content "hi")
            (list :active-run-id "run-1" :pending-count 0))))
  (should-not (superchat-stage-call
               'admit "session-1" (list :surface 'chat :content "hi")
               (list :active-run-id nil :pending-count 0)))
  (should-not (superchat-stage-call
               'admit "session-1" (list :surface 'inline-ask :content "hi")
               (list :active-run-id "run-1" :pending-count 0))))

(ert-deftest cmd-dispatch/auto-recall-cross-session-defaults-to-remembered-only ()
  "Auto-recall's cross-session inclusion is controlled by
`superchat-memory-auto-recall-cross-session': the default (`remembered')
admits only an explicit `/remember' anchor from another session, never an
ordinary chat row that merely shares a word; `all' restores S23's
original behaviour; `none' admits nothing cross-session."
  (require 'superchat)
  (require 'superchat-dispatcher)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((superchat--session-id "cross-session-a"))
       (superchat-tape-append-material
        "cross-session-a" "user" "GAMMATOMXYZ ordinary chat note")
       (superchat--cmd-remember "remember" "DELTATOMXYZ remembered fact" nil nil nil))
     (let ((query "GAMMATOMXYZ DELTATOMXYZ discussion"))
       ;; Default: `remembered'.
       (let* ((turn (superchat-turn-new query "cross-session-b")))
         (superchat--attach-indexed-memories turn query "cross-session-b")
         (let ((memories (superchat-turn-retrieved-memories turn)))
           (should-not
            (cl-some (lambda (m) (string-match-p "GAMMATOMXYZ" (plist-get m :content)))
                     memories))
           (should-not
            (cl-some (lambda (m) (string-match-p "\\[remember\\]" (plist-get m :content)))
                     memories))
           (should
            (cl-some (lambda (m) (string-match-p "DELTATOMXYZ remembered fact"
                                                  (plist-get m :content)))
                     memories))))
       ;; `all': ordinary cross-session rows are admitted too.
       (let* ((superchat-memory-auto-recall-cross-session 'all)
              (turn (superchat-turn-new query "cross-session-b")))
         (superchat--attach-indexed-memories turn query "cross-session-b")
         (let ((memories (superchat-turn-retrieved-memories turn)))
           (should
            (cl-some (lambda (m) (string-match-p "GAMMATOMXYZ" (plist-get m :content)))
                     memories))
           (should
            (cl-some (lambda (m) (string-match-p "DELTATOMXYZ" (plist-get m :content)))
                     memories))))
       ;; `none': nothing from the other session at all.
       (let* ((superchat-memory-auto-recall-cross-session 'none)
              (turn (superchat-turn-new query "cross-session-b")))
         (superchat--attach-indexed-memories turn query "cross-session-b")
         (should-not (superchat-turn-retrieved-memories turn)))))))

(ert-deftest cmd-dispatch/recall-command-unaffected-by-cross-session-defcustom ()
  "`/recall' keeps finding ordinary and remembered rows from other sessions
regardless of `superchat-memory-auto-recall-cross-session', since it only
governs automatic recall."
  (require 'superchat)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((superchat--session-id "recall-unaffected-a"))
       (superchat-tape-append-material
        "recall-unaffected-a" "user" "EPSILONTOMXYZ ordinary chat note")
       (superchat--cmd-remember "remember" "ZETATOMXYZ remembered fact" nil nil nil))
     (dolist (cross-session '(remembered all none))
       (let ((superchat-memory-auto-recall-cross-session cross-session)
             (superchat--session-id "recall-unaffected-b")
             (superchat--pending-recalled-memories nil))
         (superchat--cmd-recall "recall" "EPSILONTOMXYZ ZETATOMXYZ" nil nil nil)
         (should
          (cl-some (lambda (m) (string-match-p "EPSILONTOMXYZ" (plist-get m :content)))
                   superchat--pending-recalled-memories))
         (should
          (cl-some (lambda (m) (string-match-p "ZETATOMXYZ" (plist-get m :content)))
                   superchat--pending-recalled-memories)))))))

(ert-deftest cmd-dispatch/auto-recall-eligibility-survives-many-ineligible-foreign-rows ()
  "An eligible foreign `/remember' anchor and a current-Session row both
survive even when more than ten newer, ineligible foreign rows would
have occupied the whole pre-fix fetch window (S23-review Medium finding:
filtering after the SQL LIMIT could hide every eligible automatic
memory)."
  (require 'superchat)
  (require 'superchat-dispatcher)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((superchat--session-id "eligibility-foreign"))
       (superchat--cmd-remember "remember" "TAUXYZ remembered fact" nil nil nil))
     (superchat-tape-append-material
      "eligibility-current" "user" "TAUXYZ current session row")
     (dotimes (i 15)
       (superchat-tape-append-material
        "eligibility-foreign" "user" (format "TAUXYZ ordinary foreign row %d" i)))
     (let* ((query "please recall anything about TAUXYZ")
            (superchat-context-message-count 0)
            (turn (superchat-turn-new query "eligibility-current")))
       (superchat--attach-indexed-memories turn query "eligibility-current")
       (let ((memories (superchat-turn-retrieved-memories turn)))
         (should
          (cl-some (lambda (m) (string-match-p "current session row" (plist-get m :content)))
                   memories))
         (should
          (cl-some (lambda (m) (string-match-p "remembered fact" (plist-get m :content)))
                   memories)))))))

(ert-deftest cmd-dispatch/auto-recall-default-window-does-not-starve-foreign-anchor ()
  "With the default (nil) `superchat-context-message-count' -- the whole
current Session counts as already-visible history and is excluded from
recall -- a current Session with enough matching rows to fill the whole
pre-fix fetch window must not starve out a foreign `/remember' anchor.
The own-Session exclusion now happens in the search's SQL before LIMIT
(S23-fix-6), not in a post-fetch filter that only looked at rows that
already survived LIMIT."
  (require 'superchat)
  (require 'superchat-dispatcher)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((superchat--session-id "default-window-foreign"))
       (superchat--cmd-remember "remember" "TAUXYZ remembered fact" nil nil nil))
     (dotimes (i 15)
       (superchat-tape-append-material
        "default-window-current" "user" (format "TAUXYZ current session row %d" i)))
     (let* ((superchat-context-message-count nil)
            (query "please recall anything about TAUXYZ")
            (turn (superchat-turn-new query "default-window-current")))
       (superchat--attach-indexed-memories turn query "default-window-current")
       (let ((memories (superchat-turn-retrieved-memories turn)))
         (should
          (cl-some (lambda (m) (string-match-p "remembered fact" (plist-get m :content)))
                   memories))
         (should-not
          (cl-some (lambda (m) (string-match-p "current session row" (plist-get m :content)))
                   memories)))))))

(ert-deftest cmd-dispatch/auto-recall-none-excludes-recent-window-before-limit ()
  "`superchat-memory-auto-recall-cross-session' `none' is not merely
\"admit nothing cross-session\" -- it must also exclude the current
Session's own recent-window rows before `LIMIT', exactly like
`remembered'/`all' do.  S23-fix-6 accidentally dropped this exclusion
for `none' when it moved the other two branches' exclusion into SQL:
with the default nil window, `none' used to recall nothing at all; after
the regression it could recall up to `limit' rows that are already
visible as ordinary conversation history (S23-fix-7 finding 1)."
  (require 'superchat)
  (require 'superchat-dispatcher)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((superchat-memory-auto-recall-cross-session 'none))
       ;; Default (nil) window: the whole current Session counts as
       ;; already-visible history, so nothing should come back even
       ;; though every row matches the query.
       (let ((superchat--session-id "none-default-window"))
         (dotimes (i 6)
           (superchat-tape-append-material
            "none-default-window" "user" (format "OMICRONXYZ current row %d" i))))
       (let* ((query "OMICRONXYZ")
              (turn (superchat-turn-new query "none-default-window")))
         (superchat--attach-indexed-memories turn query "none-default-window")
         (should-not (superchat-turn-retrieved-memories turn)))
       ;; A bounded window (`superchat-context-message-count' 2): the two
       ;; rows inside the window are excluded, the two older rows outside
       ;; it are eligible, and a foreign row is never admitted at all
       ;; (`none's ordinary job, reconfirmed here).
       (superchat-tape-append-material
        "none-foreign" "user" "OMICRONXYZ foreign row")
       (superchat-tape-append-material
        "none-bounded-window" "user" "OMICRONXYZ older row 1")
       (superchat-tape-append-material
        "none-bounded-window" "user" "OMICRONXYZ older row 2")
       (superchat-tape-append-material
        "none-bounded-window" "user" "OMICRONXYZ windowed row 1")
       (superchat-tape-append-material
        "none-bounded-window" "user" "OMICRONXYZ windowed row 2")
       (let* ((superchat-context-message-count 2)
              (query "OMICRONXYZ")
              (turn (superchat-turn-new query "none-bounded-window")))
         (superchat--attach-indexed-memories turn query "none-bounded-window")
         (let ((memories (superchat-turn-retrieved-memories turn)))
           (should
            (cl-some (lambda (m) (string-match-p "older row 1" (plist-get m :content)))
                     memories))
           (should
            (cl-some (lambda (m) (string-match-p "older row 2" (plist-get m :content)))
                     memories))
           (should-not
            (cl-some (lambda (m) (string-match-p "windowed row" (plist-get m :content)))
                     memories))
           (should-not
            (cl-some (lambda (m) (string-match-p "foreign row" (plist-get m :content)))
                     memories))))))))

(ert-deftest cmd-dispatch/tape-search-prioritized-exclude-ids-past-sqlite-parameter-limit ()
  "EXCLUDE-IDS must keep working correctly well past SQLite's default
~999-variable per-statement bound-parameter limit, since a long-running
Session's `superchat-context-message-count' window can hold thousands of
ids.  S23-fix-6 chose one JSON array bound parameter plus `json_each'
for exactly this reason; this is that check committed as a real-DB
regression test instead of the scratch script S23-fix-6's report relied
on (S23-fix-7 finding 2)."
  (require 'superchat)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (let ((session-id "exclude-ids-scale-session")
           excluded-ids)
       (dotimes (i 1600)
         (superchat-tape-append-material
          session-id "user" (format "PIXYZ excluded row %d" i)))
       (setq excluded-ids (mapcar #'car (superchat-view-entries session-id)))
       (should (= (length excluded-ids) 1600))
       (dotimes (i 3)
         (superchat-tape-append-material
          session-id "user" (format "PIXYZ kept row %d" i)))
       (let ((rows
              (superchat-db-tape-search-prioritized
               "PIXYZ" session-id 20 nil excluded-ids)))
         (should (= (length rows) 3))
         (should (cl-every
                  (lambda (row) (string-match-p "kept row" (nth 4 row)))
                  rows))
         (should-not
          (cl-some (lambda (row) (memql (nth 0 row) excluded-ids)) rows)))))))

(ert-deftest cmd-dispatch/recall-command-current-session-survives-many-foreign-rows ()
  "`/recall' still returns a current-Session match even when more than
forty better-ranked rows from another session would have filled the
whole pre-fix fetch window (S23-review Medium finding: `/recall' could
not guarantee current-Session rows first)."
  (require 'superchat)
  (test-cmd-dispatch--recall-scope-fixture
   (lambda ()
     (superchat-tape-append-material
      "recall-priority-current" "user" "SIGMAXYZ current session match")
     (dotimes (i 45)
       (superchat-tape-append-material
        "recall-priority-foreign" "user" (format "SIGMAXYZ foreign row %d" i)))
     (let ((superchat--session-id "recall-priority-current")
           (superchat--pending-recalled-memories nil))
       (superchat--cmd-recall "recall" "SIGMAXYZ" nil nil nil)
       (should
        (cl-some (lambda (m) (string-match-p "current session match" (plist-get m :content)))
                 superchat--pending-recalled-memories))))))

;;; S10: the chat dispatcher submits an envelope through the driver.

(ert-deftest cmd-dispatch/chat-submit-goes-through-pipeline ()
  "S10: `superchat--submit-chat-request' builds a real envelope and calls
`superchat-pipeline-submit', which runs the driver's OWN `admit' stage call
a second time -- with a real envelope carrying `:context (:chat-buffer …)'.
This is distinct from `superchat-send-input''s own pre-existing admit
pre-check (S05b), which only fires when a Run is already active in this
View and passes a bare `(:surface chat :content INPUT)' plist with no
`:context' key at all -- so for this test's first, ordinary send, that
pre-check never runs at all.  A test-only `admit' contribution that only
sets its flag when `:context' is present therefore proves the driver ran
with a translated envelope, without the `superchat-stage.el' call-counter
the leader ruled out (S10-dispatch.md); restored via
`superchat-stage-test--isolated'."
  (dolist (mode '(async streaming))
    (superchat-stage-test--isolated
      (producer-test--chat mode
        (let (saw-envelope-admit)
          (superchat-stage-add
           'admit 'cmd-dispatch-test-pipeline-proof
           (lambda (_session-id envelope _snapshot)
             (when (plist-member envelope :context)
               (setq saw-envelope-admit t))
             nil)
           :owner 'cmd-dispatch-test)
          (human-input-test--send "Question through the driver")
          (should saw-envelope-admit)
          (should (= 1 (length test-input-batch--requests)))
          (should-not superchat-stage-faults))))))

(ert-deftest cmd-dispatch/admit-active-run-match-survives-envelope-submission ()
  "S10's synthesized Sink/`:transcript-owner' keeps `superchat-send-input''s
admit/active-Run match correct: typing a second message while the chat's
first pipeline-submitted Run is still running is queued, not sent as a
second, duplicate Run (C12/C12b) -- the concrete failure mode a
`:transcript-owner' the Sink and the Run entry disagreed on would produce
silently (S10-dispatch.md, leader decision on plan §2.2)."
  (dolist (mode '(async streaming))
    (producer-test--chat mode
      (human-input-test--send "first message")
      (should (= 1 (length test-input-batch--requests)))
      (let ((run superchat--active-run-id))
        (should run)
        (human-input-test--send "second message while first is running")
        (should (= 1 (length test-input-batch--requests)))
        (should (= 1 (superchat-runtime-pending-follow-ups session)))
        (test-input-batch--reply (car test-input-batch--requests) nil)
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))))

(ert-deftest cmd-dispatch/regenerate-submits-through-pipeline ()
  "`superchat-regenerate' (routed through
`superchat--submit-composed-chat-request' since S10, not Runtime's low-level
legacy entry point directly) still reaches the provider: if the envelope
translation were broken, this would error instead of producing a second
Request."
  (dolist (mode '(async streaming))
    (producer-test--chat mode
      (human-input-test--send "original question")
      (test-input-batch--reply (car test-input-batch--requests) nil)
      (should (eq 'completed
                  (plist-get (superchat-runtime-status superchat--active-run-id) :status)))
      (let ((before (length test-input-batch--requests)))
        (with-current-buffer view
          (superchat-regenerate))
        (should (= (1+ before) (length test-input-batch--requests)))))))
