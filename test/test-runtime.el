;;; test-runtime.el --- ERT tests for Superchat Runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat)

(defmacro superchat-runtime-test--isolated (&rest body)
  "Run BODY with isolated Runtime state and completion hooks."
  (declare (indent 0))
  `(let ((superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-buffer-name " *superchat-runtime-test*")
         (superchat--active-timeout-timer nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-closure-shadow-functions nil)
         (superchat-runtime--closure-observations nil))
     ,@body))

(defun superchat-runtime-test--request ()
  "Return a minimal prepared Runtime request."
  '(:type :llm-query
    :input "hello"
    :clean-input "hello"
    :prompt "User question: hello"
    :session-id "session-runtime"
    :buffer-name " *superchat-runtime-test*"))

(ert-deftest runtime/closure-terminal-guard-is-enabled-by-default ()
  "A fresh Superchat load must enforce Closure terminal readiness."
  (should superchat-closure-terminal-guard-enabled))

(ert-deftest runtime/legacy-executor-transport-shim-stays-deleted ()
  "The legacy blocking executor entry point must not come back."
  (require 'superchat-executor)
  (should-not (fboundp 'superchat-executor-call-llm)))

(ert-deftest runtime/is-the-only-turn-completion-owner ()
  "Only Runtime may expose or call a Turn completion seam."
  (should-not (fboundp 'superchat-core-finalize-turn))
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "superchat-runtime.el")
      (with-temp-buffer
          (insert-file-contents file)
        (should-not
           (re-search-forward
            "(\\s-*\\(?:superchat-core-finalize-turn\\|superchat-runtime--\\(?:finish\\|complete-turn\\)\\)\\_>"
            nil t)))))))

(ert-deftest runtime/product-entry-points-do-not-call-model-transport ()
  "Product entry points must submit through Runtime, never model transport.
The transport lives in `superchat-llm.el' (pinned to exactly one
streaming call); `superchat-runtime.el' is its only caller (pinned to
exactly one invocation).  Every other file — including the
`superchat.el' distribution — must stay transport-free."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (owners '("superchat-llm.el" "superchat-runtime.el")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (member (file-name-nondirectory file) owners)
        (with-temp-buffer
          (insert-file-contents file)
          (should-not
           (re-search-forward
            "(\\s-*\\(?:llm-chat\\|llm-chat-async\\|llm-chat-streaming\\|superchat--llm-generate-answer\\(?:-sync\\)?\\)\\_>"
            nil t)))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-llm.el" root))
      (should (= 1 (how-many "^[^;\n]*(\\s-*llm-chat-streaming\\_>"))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-runtime.el" root))
      (should (= 1 (how-many "^[^;\n]*(\\s-*superchat--llm-generate-answer\\_>"))))))

(ert-deftest runtime/blocking-submit-is-limited-to-sync-adapters ()
  "Only declared synchronous boundary adapters may block on Runtime.
Everything else — Agent family, Surfaces, automation, maintenance hooks
like compaction — must submit asynchronously and return a Run handle."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (sync-adapters '("superchat-runtime.el" ; defines the adapter seam
                         "ob-superchat.el"      ; #+RESULTS: needs a string
                         "superchat-magit.el"   ; inserts a commit message
                         "superchat-rewrite.el"))) ; replaces the region
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (member (file-name-nondirectory file) sync-adapters)
        (with-temp-buffer
          (insert-file-contents file)
          (should-not
           (re-search-forward
            "(\\s-*superchat-runtime-submit-blocking\\_>" nil t)))))))

(ert-deftest runtime/blocking-pumps-stay-behind-declared-seams ()
  "Busy-wait pumps must not spread beyond their two declared owners.
`superchat-runtime.el' pumps inside the blocking adapter seam and
`superchat-subagent.el' inside the documented map primitive."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (owners '("superchat-runtime.el" "superchat-subagent.el")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((pumps (how-many "(\\s-*accept-process-output\\_>")))
          (if (member (file-name-nondirectory file) owners)
              (should (= 1 pumps))
            (should (= 0 pumps))))))))

(ert-deftest runtime/kernel-requires-stay-inside-kernel ()
  "Kernel modules may only require other kernel modules.
The kernel must stay loadable headless: no UI, no chat distribution,
and no integration package may be pulled in from below."
  (let* ((root (file-name-directory (locate-library "superchat-runtime")))
         (kernel '("superchat-core.el" "superchat-preset.el"
                   "superchat-db.el" "superchat-tape.el"
                   "superchat-projection.el" "superchat-closure.el"
                   "superchat-authority.el"
                   "superchat-runtime.el" "superchat-llm.el"))
         (kernel-features (mapcar #'file-name-sans-extension kernel)))
    (dolist (name kernel)
      (with-temp-buffer
        (insert-file-contents (expand-file-name name root))
        (goto-char (point-min))
        (while (re-search-forward "^(require '\\(superchat[a-z-]*\\)" nil t)
          (should (member (match-string 1) kernel-features)))))))

(ert-deftest runtime/eager-distribution-does-not-require-integrations ()
  "Integration packages load on demand, never via a top-level require.
Only another integration package may require one (the agents panel
requires the subagent module it visualizes); everything else reaches
them through autoloads and the kernel registries."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (plugins '("superchat-rewrite" "superchat-compact" "superchat-workflow"
                   "superchat-subagent" "superchat-agents-panel"
                   "superchat-genui" "superchat-magit" "ob-superchat")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (let ((base (file-name-sans-extension
                   (file-name-nondirectory file))))
        (unless (member base plugins)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "^(require '\\([a-z-]+\\)" nil t)
              (should-not (member (match-string 1) plugins)))))))))

(ert-deftest runtime/request-approval-function-reaches-tool-context ()
  "A Request-scoped :approval-function must reach the tool context."
  (superchat-runtime-test--isolated
    (should-error
     (superchat-runtime--validate-request
      '(:prompt "hi" :approval-function "not-a-function")))
    (let* ((approval (lambda (_request) t))
           captured)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _)
                   (setq captured
                         (plist-get superchat--tool-execution-context
                                    :approval-function))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            (list :status 'completed :response "ok")))))
        (superchat-runtime-submit
         (append (superchat-runtime-test--request)
                 (list :approval-function approval)))
        (should (eq approval captured))))))

(ert-deftest runtime/session-capability-activation-rejects-conflict ()
  "Activating a Preset under a conflicting Capability name must fail."
  (with-temp-buffer
    (let ((preset (superchat-preset-from-plist
                   (list :name "coder"
                         :profile-id "coder"
                         :type 'agent
                         :capability "code"
                         :source 'builtin))))
      (should-error (superchat--activate-capability preset "research"))
      (should (eq preset (superchat--activate-capability preset "code"))))))

(ert-deftest runtime/production-sources-do-not-restore-agent-mode ()
  "Capability and Topology must not collapse back into `agent-mode'."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not
         (and (search-forward "agent-mode" nil t)
              (file-name-nondirectory file)))))))

(ert-deftest runtime/production-does-not-write-legacy-agent-identities ()
  "Production code may read legacy identity, but must not write it."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (search-forward "agent-id" nil t))
        (goto-char (point-min))
        (should-not (search-forward "subagent-id" nil t))
        (goto-char (point-min))
        (should-not (search-forward "subagent_id" nil t))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-subagent.el" root))
      (should (= 1 (how-many "\\_<agent_id\\_>")))
      (should (search-forward "(alist-get 'agent_id payload)" nil t)))))

(ert-deftest runtime/profile-storage-uses-domain-identity-name ()
  "Profile storage must not expose configuration-source terminology."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (search-forward "declared-profile-id" nil t))))))

(ert-deftest runtime/submit-owns-lifecycle-and-public-status ()
  "A successful submit emits one ordered lifecycle and hides private state."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          timeline)
      (setq superchat-post-turn-functions
            (list (lambda (_turn) (push 'post-turn timeline))))
      (setq superchat-run-completed-functions
            (list (lambda (_status) (push 'run-hook timeline))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (push 'turn-hook timeline))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (intern (plist-get event :type)) timeline)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer tape-id)
                   (push (list 'delivered answer tape-id) timeline)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'request 'provider-request)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done"))
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'accepted (plist-get handle :status)))
          (should (eq 'completed (plist-get status :status)))
          (should-not (plist-member status :turn))
          (should-not (plist-member status :transport-request))
          (should
           (equal
            (nreverse timeline)
            '(intent\.received run\.started response\.completed
              post-turn
              run\.completed turn\.completed
              (delivered "done" 3)
              run-hook turn-hook))))))))

(ert-deftest runtime/delivery-observes-durable-terminal-seal ()
  "Delivery adapters run only after the Run and Turn are durably closed."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events delivered-events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events
                         (append events (list (plist-get event :type))))
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-submit
         (append
          (superchat-runtime-test--request)
          (list :delivery-function
                (lambda (&rest _args)
                  (setq delivered-events (copy-sequence events))))))
        (should
         (equal '("intent.received" "run.started" "response.completed"
                  "run.completed" "turn.completed")
                delivered-events))))))

(ert-deftest runtime/delivery-failure-does-not-reopen-durable-run ()
  "A failed consumer handoff cannot rewrite an already sealed Run."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (&rest _args) (error "consumer failed"))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'completed (plist-get status :status)))
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.completed" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/closure-observations-are-bounded-copies ()
  "Runtime retains a bounded, caller-safe diagnostic history."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime--closure-observation-limit 2))
      (superchat-runtime--record-closure-observation
       '(:source shadow :agreement agreed :run-id "run-1"))
      (superchat-runtime--record-closure-observation
       '(:source shadow :agreement disagreed :run-id "run-2"))
      (superchat-runtime--record-closure-observation
       '(:source guard :decision allowed :run-id "run-3"))
      (let ((observations (superchat-runtime-closure-observations)))
        (should
         (equal '("run-3" "run-2")
                (mapcar (lambda (item) (plist-get item :run-id))
                        observations)))
        (should (floatp (plist-get (car observations) :observed-at)))
        (setf (plist-get (car observations) :run-id) "mutated")
        (should
         (equal "run-3"
                (plist-get
                 (car (superchat-runtime-closure-observations))
                 :run-id))))
      (should
       (equal '("run-2")
              (mapcar
               (lambda (item) (plist-get item :run-id))
               (superchat-runtime-closure-observations t))))
      (superchat-runtime-clear-closure-observations)
      (should-not (superchat-runtime-closure-observations)))))

(ert-deftest runtime/closure-shadow-observes-without-owning-terminal-state ()
  "Shadow Closure agrees from Tape without changing Runtime delivery or status."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-shadow-" t))
           (superchat-data-directory directory)
           (superchat-closure-shadow-enabled t)
           report
           delivered
           (superchat-closure-shadow-functions
            (list (lambda (value) (setq report value)))))
      (unwind-protect
          (progn
            (superchat-db-close)
            (setq superchat-post-turn-functions
                  (list
                   (lambda (turn)
                     (superchat-tape-append-event
                      (list :type "attachment.added"
                            :session-id (superchat-turn-session-id turn)
                            :turn-id (superchat-turn-id turn)
                            :actor-id "user"
                            :payload '(:mime_type "image/png"))))))
            (superchat-tape-append-event
             '(:type "anchor.created"
               :session-id "session-runtime"
               :actor-id "user"
               :payload (:parent_session_id "parent")))
            (superchat-tape-append-event
             '(:type "extension.activated"
               :session-id "session-runtime"
               :actor-id "runtime"
               :payload (:extensions ("example"))))
            (superchat-tape-append-event
             '(:type "capability.activated"
               :session-id "session-runtime"
               :actor-id "user"
               :payload (:name "chat")))
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((request
                      (append
                       (superchat-runtime-test--request)
                       (list :delivery-function
                             (lambda (response status tape-id)
                               (setq delivered
                                     (list response status tape-id))))))
                     (handle (superchat-runtime-submit request))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 6) delivered))
                (should (eq 'agreed (plist-get report :agreement)))
                (should
                 (eq 'proved
                     (plist-get (plist-get report :run-judgment)
                                :verdict)))
                (should
                 (eq 'proved
                     (plist-get (plist-get report :turn-judgment)
                                :verdict)))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'shadow (plist-get observation :source)))
                  (should (eq 'agreed (plist-get observation :agreement)))
                  (should
                   (equal "session-runtime"
                          (plist-get observation :session-id)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-shadow-reports-disagreement-without-rejecting-run ()
  "A missing durable seal is reported but cannot change Runtime completion."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-shadow-gap-" t))
           (superchat-data-directory directory)
           (superchat-closure-shadow-enabled t)
           (replay (symbol-function 'superchat-tape-replay-events))
           report
           delivered
           (superchat-closure-shadow-functions
            (list (lambda (value) (setq report value)))))
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat-tape-replay-events)
                       (lambda (session-id &optional since-id limit)
                         (butlast
                          (funcall replay session-id since-id limit)
                          2)))
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 3) delivered))
                (should (eq 'disagreed (plist-get report :agreement)))
                (should
                 (eq 'open
                     (plist-get (plist-get report :run-judgment)
                                :verdict)))))
        (superchat-db-close)
        (delete-directory directory t))))))

(ert-deftest runtime/closure-shadow-failure-does-not-fail-run ()
  "An unavailable shadow Projection remains a diagnostic, not a terminal cause."
  (superchat-runtime-test--isolated
    (let ((superchat-closure-shadow-enabled t)
          (next-id 0)
          report
          delivered)
      (setq superchat-closure-shadow-functions
            (list (lambda (value) (setq report value))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat-tape-replay-events)
                 (lambda (&rest _args) (error "shadow unavailable")))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (response status tape-id)
                          (setq delivered (list response status tape-id)))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'completed (plist-get status :status)))
          (should (equal '("done" completed 3) delivered))
          (should (eq 'unknown (plist-get report :agreement)))
          (should (equal "shadow unavailable" (plist-get report :error))))))))

(ert-deftest runtime/closure-terminal-guard-allows-proved-completion ()
  "A proved readiness judgment permits the matching terminal seals."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id)))
                     (types
                      (mapcar
                       (lambda (event) (plist-get event :type))
                       (superchat-tape-replay-events "session-runtime"))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 3) delivered))
                (should
                 (equal '("intent.received" "run.started"
                          "response.completed" "run.completed"
                          "turn.completed")
                        types))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'guard (plist-get observation :source)))
                  (should (eq 'allowed (plist-get observation :decision)))
                  (should
                   (eq 'proved
                       (plist-get (plist-get observation :judgment)
                                  :verdict)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-allows-clean-cancellation ()
  "Cancellation is guarded against its response-free Tape prefix."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-cancel-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'llm-cancel-request) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) 'provider-request)))
              (let* ((handle
                      (superchat-runtime-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (run-id (plist-get handle :run-id)))
                (should (superchat-runtime-cancel run-id))
                (should
                 (eq 'cancelled
                     (plist-get (superchat-runtime-status run-id) :status)))
                (should
                 (equal '("[Cancelled by user]" cancelled 3) delivered))
                (should
                 (equal
                  '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (mapcar
                   (lambda (event) (plist-get event :type))
                   (superchat-tape-replay-events "session-runtime")))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-rejects-open-readiness ()
  "An open judgment prevents a terminal seal and becomes audit-incomplete."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-open-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           (replay (symbol-function 'superchat-tape-replay-events))
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function 'superchat-tape-replay-events)
                  (lambda (session-id &optional since-id limit)
                    (butlast
                     (funcall replay session-id since-id limit))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should
                 (eq 'audit-incomplete (plist-get status :status)))
                (should (eq 'audit-incomplete (nth 1 delivered)))
                (should-not (nth 2 delivered))
                (should
                 (string-match-p "Closure terminal guard"
                                 (car delivered)))
                (should
                 (equal
                  '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (mapcar
                   (lambda (event) (plist-get event :type))
                   (funcall replay "session-runtime"))))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'guard (plist-get observation :source)))
                  (should (eq 'rejected (plist-get observation :decision)))
                  (should
                   (eq 'open
                       (plist-get (plist-get observation :judgment)
                                  :verdict)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-failure-is-audit-incomplete ()
  "A Guard interpreter failure cannot leak resources or claim completion."
  (superchat-runtime-test--isolated
    (let ((superchat-closure-terminal-guard-enabled t)
          (next-id 0)
          events delivered)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat-tape-replay-events)
                 (lambda (&rest _args) (error "guard unavailable")))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (response status tape-id)
                          (setq delivered
                                (list response status tape-id)))))))
               (run-id (plist-get handle :run-id))
               (entry (superchat-runtime--entry run-id)))
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should (eq 'audit-incomplete (nth 1 delivered)))
          (should (string-match-p "guard unavailable" (car delivered)))
          (should
           (equal '("intent.received" "run.started" "response.completed")
                  (nreverse events)))
          (let ((observation
                 (car (superchat-runtime-closure-observations))))
            (should (eq 'guard (plist-get observation :source)))
            (should (eq 'unknown (plist-get observation :decision)))
            (should
             (equal "guard unavailable" (plist-get observation :error))))
          (should-not (plist-get entry :delivery-function))
          (should-not (plist-get entry :transport-request)))))))

(ert-deftest runtime/events-use-logical-causation-and-storage-delivery ()
  "Runtime causation uses event identity while UI delivery keeps row identity."
  (superchat-runtime-test--isolated
    (let ((next-storage-id 0)
          events delivered)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events (append events (list event)))
                   (cl-incf next-storage-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (_answer storage-id)
                   (setq delivered storage-id)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-submit (superchat-runtime-test--request))
        (should (= 3 delivered))
        (should
         (equal '("intent.received" "run.started"
                  "response.completed" "run.completed" "turn.completed")
                (mapcar (lambda (event) (plist-get event :type)) events)))
        (let ((started-payload (plist-get (nth 1 events) :payload)))
          (should (equal "lead" (plist-get started-payload :actor-id)))
          (should (equal "chat" (plist-get started-payload :capability)))
          (should-not (plist-member started-payload :profile-id)))
        (let ((previous nil))
          (dolist (event events)
            (let ((event-id (plist-get event :event-id)))
              (should (string-prefix-p "evt_" event-id))
              (should (equal previous (plist-get event :causation-id)))
              (setq previous event-id))))))))

(ert-deftest runtime/propagates-orthogonal-configuration ()
  "Runtime exposes config in status, audit events, and tool context."
  (superchat-runtime-test--isolated
    (let ((root (make-temp-file "superchat-runtime-root" t))
          (validator #'ignore)
          intent-event tool-context)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (let ((id 0))
                       (lambda (event)
                         (when (equal "intent.received"
                                      (plist-get event :type))
                           (setq intent-event event))
                         (cl-incf id))))
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--process-llm-result) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (setq tool-context superchat--tool-execution-context)
                       (funcall superchat--llm-runtime-observer
                                'terminal
                                '(:status completed :response "done")))))
            (let* ((handle
                    (superchat-runtime-submit
                     `(:input "inspect"
                       :clean-input "inspect"
                       :prompt "inspect"
                       :capability "code"
                       :topology team
                       :authority-policy developer-project
                       :workspace-id "workspace-test"
                       :workspace-root ,root
                       :tool-validator ,validator
                       :origin (:major-mode text-mode
                                :uri "buffer://notes/region/10-20"
                                :region "do-not-persist"))))
                   (status
                    (superchat-runtime-status (plist-get handle :run-id)))
                   (payload (plist-get intent-event :payload)))
              (should (equal "code" (plist-get status :capability)))
              (should (eq 'team (plist-get status :topology)))
              (should (equal "developer-project"
                             (plist-get status :authority-policy)))
              (should (eq 'text-mode (plist-get status :surface)))
              (should
               (eq 'text-mode
                   (buffer-local-value
                    'superchat--last-surface
                    (get-buffer superchat-buffer-name))))
              (should (equal "workspace-test"
                             (plist-get status :workspace-id)))
              (should (eq 'team (plist-get tool-context :topology)))
              (should (equal "developer-project"
                             (plist-get tool-context :authority-policy)))
              (should (eq validator (plist-get tool-context :validate)))
              (should (equal "text-mode" (plist-get payload :surface)))
              (should
               (equal "buffer://notes/region/10-20"
                      (plist-get payload :origin-uri)))
              (should-not (string-match-p
                           "do-not-persist"
                           (prin1-to-string intent-event)))))
        (delete-directory root t)))))

(ert-deftest runtime/agent-preset-identifies-profile-and-instance ()
  "An Agent Run exposes stable Profile and direct parent identities."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "Coder Fast" :profile-id "profile.coder"
              :type agent :capability "coding"
              :body "Implement changes.")))
          (next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push event events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :actor-id "coder-1"
                        :parent-run-id "run-parent"
                        :capability "coding"
                        :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id)))
               (started
                (cl-find-if
                 (lambda (event)
                   (equal "run.started" (plist-get event :type)))
                 events)))
          (should (equal "coder-1" (plist-get status :actor-id)))
          (should (equal "profile.coder" (plist-get status :profile-id)))
          (should (equal "run-parent"
                         (plist-get status :parent-run-id)))
          (should (equal "coding" (plist-get status :capability)))
          (should (equal "profile.coder" (plist-get started :profile-id)))
          (should (equal "run-parent"
                         (plist-get started :parent-run-id)))
          (should
           (equal "profile.coder"
                  (plist-get (plist-get started :payload) :profile-id)))
          (should
           (equal "coder-1"
                  (plist-get (plist-get started :payload) :actor-id)))
          (should
           (equal "coding"
                  (plist-get (plist-get started :payload) :capability))))))))

(ert-deftest runtime/agent-profile-requires-profile-id ()
  "A legacy Agent Profile falls back to its name with a warning."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          profile
          warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _args)
                   (push message warnings)))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (setq profile
              (superchat-preset-from-plist
               '(:name "coder-safe" :type agent :capability "coding")))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "coder-safe" (plist-get status :profile-id)))
          (should
           (cl-some
            (lambda (message)
              (string-match-p
               "Deprecated agent preset without profile-id"
               message))
            warnings)))))))

(ert-deftest runtime/profile-name-is-not-an-implicit-capability ()
  "Runtime falls back to chat, never from Profile identity to Capability."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "coder-fast" :type agent :body "Implement changes.")))
          (next-id 0)
          warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _args)
                   (push message warnings)))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :actor-id "coder-1" :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "coder-fast" (plist-get status :profile-id)))
          (should (equal "chat" (plist-get status :capability)))
          (should
           (cl-some
            (lambda (message)
              (string-match-p "no explicit capability" message))
            warnings)))))))

(ert-deftest runtime/agent-profile-capability-conflict-rejected ()
  "A Request cannot reinterpret an Agent Profile as another Capability."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "coder-safe" :profile-id "profile.coder"
              :type agent :capability "coding")))
          (transport-called nil)
          (event-count 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf event-count)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq transport-called t))))
        (let ((condition
               (should-error
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :capability "review" :preset profile)))
                :type 'error)))
          (should
           (string-match-p
            "Capability review conflicts with Agent Profile profile.coder"
            (error-message-string condition))))
        (should-not transport-called)
        (should (= 0 event-count))
        (should (= 0 (hash-table-count superchat-runtime--runs)))))))

(ert-deftest runtime/profile-does-not-grant-authority ()
  "Profile metadata cannot widen the Runtime-selected Authority."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name coder :profile-id profile.coder
              :type agent :capability coding
              :authority-policy developer-project)))
          (next-id 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should
           (equal "default" (plist-get status :authority-policy))))))))

(ert-deftest runtime/non-agent-preset-does-not-create-profile ()
  "Prompt and Workflow Presets remain configuration, not Agent Profiles."
  (dolist (type '(prompt workflow))
    (let ((preset
           (superchat-preset-from-plist
            (list :name (symbol-name type) :type type))))
      (should-not (superchat-preset-profile-id preset)))))

(ert-deftest runtime/rejects-unknown-configuration ()
  "Runtime fails closed before transport for unknown policy or Topology."
  (should-error
   (superchat-runtime-submit
    '(:prompt "hello" :authority-policy "admin")))
  (should-error
   (superchat-runtime-submit
    '(:prompt "hello" :topology collective))))

(ert-deftest runtime/post-turn-waits-for-terminal-response ()
  "Post-turn hooks do not run while the provider request is active."
  (superchat-runtime-test--isolated
    (let (observer
          (post-calls 0)
          (next-id 0))
      (setq superchat-post-turn-functions
            (list (lambda (_turn) (cl-incf post-calls))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'provider-request)
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (= 0 post-calls))
          (should (eq 'running-model
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (funcall observer
                   'terminal '(:status completed :response "later"))
          (should (= 1 post-calls))
          (should (eq 'completed
                      (plist-get (superchat-runtime-status run-id)
                                 :status))))))))

(ert-deftest runtime/custom-delivery-and-stream-stay-private ()
  "Non-chat callers receive callbacks without exposing them through status."
  (superchat-runtime-test--isolated
    (let (delivered streamed)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest args)
                   (funcall (nth 2 args) "part")
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((request
                (append
                 (superchat-runtime-test--request)
                 (list :stream-function
                       (lambda (chunk) (setq streamed chunk))
                       :delivery-function
                       (lambda (response status tape-id)
                         (setq delivered
                               (list response status tape-id))))))
               (handle (superchat-runtime-submit request))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "part" streamed))
          (should (equal '("done" completed 3) delivered))
          (should-not (plist-member status :delivery-function))
          (should-not (plist-member status :stream-function)))))))

(ert-deftest runtime/ephemeral-buffer-is-released-after-delivery ()
  "A non-chat Run may ask Runtime to remove its hidden execution buffer."
  (superchat-runtime-test--isolated
    (let ((name " *superchat-runtime-ephemeral*"))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-submit
         (list :prompt "hello"
               :buffer-name name
               :ephemeral-buffer t
               :delivery-function (lambda (&rest _args) nil)))
        (should-not (get-buffer name))))))

(ert-deftest runtime/blocking-waits-on-submitted-run ()
  "The blocking adapter returns delivery from the ordinary submit path."
  (superchat-runtime-test--isolated
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (let ((id 0))
                 (lambda (_event) (cl-incf id))))
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _args)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          '(:status completed :response "blocking done")))))
      (should
       (equal "blocking done"
              (superchat-runtime-submit-blocking
               '(:prompt "hello" :record-conversation nil)))))))

(ert-deftest runtime/blocking-timeout-cancels-run ()
  "A blocking wait cancels its accepted Run before signaling timeout."
  (superchat-runtime-test--isolated
    (let (cancelled)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) 'provider-request))
                ((symbol-function 'superchat-runtime-cancel)
                 (lambda (run-id &optional reason)
                   (setq cancelled (list run-id reason))
                   t)))
        (should-error
         (superchat-runtime-submit-blocking '(:prompt "hello") 0))
        (should (string-prefix-p "run-" (car cancelled)))
        (should (string-match-p "Timed out" (cadr cancelled)))))))

(ert-deftest runtime/stateless-request-gets-session-id ()
  "A Surface may omit Session identity and receive an ephemeral one."
  (superchat-runtime-test--isolated
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (let ((id 0))
                 (lambda (_event) (cl-incf id))))
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _args)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          '(:status completed :response "done")))))
      (let ((handle (superchat-runtime-submit '(:prompt "hello"))))
        (should (string-prefix-p "session-"
                                 (plist-get handle :session-id)))))))

(ert-deftest runtime/forwards-request-local-transport-controls ()
  "Prepared tools and loop controls are scoped to one Runtime request."
  (superchat-runtime-test--isolated
    (let ((wrapper (lambda (tool) tool))
          (round-limit (lambda () 'stop))
          captured active-p)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq captured
                         (list superchat--llm-tools-prepared-p
                               superchat--llm-prepared-tools
                               superchat--llm-tool-wrapper
                               superchat--llm-round-limit-function
                               superchat--llm-unknown-tool-retry-limit
                               superchat--llm-persist-synthesized-tools)
                         active-p superchat--llm-run-active-p)
                   (should (funcall active-p))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-submit
         (list :prompt "hello"
               :prepared-tools '(tool-a)
               :tool-wrapper wrapper
               :round-limit-function round-limit
               :unknown-tool-retries 7
               :persist-synthesized-tools nil))
        (should (equal (list t '(tool-a) wrapper round-limit 7 nil)
                       captured))
        (should-not (funcall active-p))))))

(ert-deftest runtime/record-conversation-can-be-suppressed ()
  "Runtime events remain while legacy conversation hooks can be skipped."
  (superchat-runtime-test--isolated
    (let ((recorded 0))
      (setq superchat-post-turn-functions
            (list #'superchat--hook-record-user-message))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--record-message)
                 (lambda (&rest _args) (cl-incf recorded)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-submit
         '(:input "hello"
           :prompt "hello"
           :record-conversation nil))
        (should (= 0 recorded))))))

(ert-deftest runtime/cancel-uses-latest-request-and-ignores-late-result ()
  "Cancellation targets the newest tool-round handle and is terminal."
  (superchat-runtime-test--isolated
    (let (observer cancelled delivered
          (next-id 0)
          events turn-hook)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id) (setq delivered answer)))
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'request-1)
                   (funcall observer 'request 'request-2)
                   'request-2)))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'request-2 cancelled))
          (should (equal "[Cancelled by user]" delivered))
          (should (eq 'cancelled
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (funcall observer
                   'terminal '(:status completed :response "too late"))
          (should (eq 'cancelled
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (should-not (member "response.completed" events))
          (should
           (equal '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (nreverse events)))
          (should turn-hook))))))

(ert-deftest runtime/local-cancel-wins-synchronous-provider-callback ()
  "A callback raised by provider cancellation cannot complete the Run first."
  (superchat-runtime-test--isolated
    (let (observer cancelled delivered
          (next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id) (push answer delivered)))
                ((symbol-function 'llm-cancel-request)
                 (lambda (request)
                   (setq cancelled request)
                   (funcall observer
                            'terminal
                            '(:status completed :response "provider result"))))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'provider-request)
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'provider-request cancelled))
          (should
           (eq 'cancelled
               (plist-get (superchat-runtime-status run-id) :status)))
          (should (equal '("[Cancelled by user]") delivered))
          (should
           (equal '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/failed-run-still-completes-its-turn ()
  "A provider failure terminates both the Run and its owning Turn."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events
          completion-hook turn-hook)
      (setq superchat-run-completed-functions
            (list (lambda (_status) (setq completion-hook t))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status failed :response "provider failed")))))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'failed (plist-get status :status)))
          (should-not completion-hook)
          (should turn-hook)
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/synchronous-transport-error-closes-run ()
  "A synchronous transport error follows the ordinary failed terminal path."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events delivered turn-hook entry)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id) (setq delivered answer)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (error "network broken"))))
        (should-error
         (superchat-runtime-submit
          (append (superchat-runtime-test--request)
                  '(:ephemeral-buffer t))))
        (maphash (lambda (_run-id value) (setq entry value))
                 superchat-runtime--runs)
        (should (eq 'failed (plist-get entry :status)))
        (should (plist-get entry :turn-completed))
        (should turn-hook)
        (should (string-match-p "network broken" delivered))
        (should-not (get-buffer " *superchat-runtime-test*"))
        (should
         (equal '("intent.received" "run.started" "response.completed"
                  "run.failed" "turn.completed")
                (nreverse events)))))))

(ert-deftest runtime/prewrite-failure-prevents-provider-call ()
  "A failed run.started append stops before transport execution."
  (superchat-runtime-test--isolated
    (let ((provider-called nil)
          turn-hook)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "run.started" (plist-get event :type))
                       (error "db unavailable")
                     1)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (setq provider-called t))))
        (should-error
         (superchat-runtime-submit
          (superchat-runtime-test--request)))
        (should-not provider-called)
        (let (entry)
          (maphash
           (lambda (_run-id value) (setq entry value))
           superchat-runtime--runs)
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should (plist-get entry :turn-completed))
          (should turn-hook))))))

(ert-deftest runtime/prewrite-audit-failure-survives-restart ()
  "A pre-provider audit failure leaves a recoverable durable terminal seal."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-prewrite-recovery-" t))
           (superchat-data-directory directory)
           (append-record
            (symbol-function 'superchat-tape-append-event-record))
           provider-called run-id delivered-status)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal "run.started" (plist-get event :type))
                        (error "db unavailable")
                      (funcall append-record event))))
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args) (setq provider-called t))))
              (should-error
               (superchat-runtime-submit
                (append
                 (superchat-runtime-test--request)
                 (list :delivery-function
                       (lambda (_response status _tape-id)
                         (setq delivered-status status))))))
              (maphash (lambda (id _entry) (setq run-id id))
                       superchat-runtime--runs)
              (should-not provider-called)
              (should (eq 'audit-incomplete delivered-status))
              (should
               (eq 'audit-incomplete
                   (plist-get (superchat-runtime-status run-id) :status))))
            (superchat-db-close)
            (setq superchat-runtime--runs (make-hash-table :test #'equal))
            (let* ((trace
                    (superchat-tape-replay-events "session-runtime"))
                   (types (mapcar (lambda (event) (plist-get event :type))
                                  trace))
                   (turn-id (plist-get (car trace) :turn-id))
                   (run-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-run-terminal :run-id run-id)
                     trace))
                   (turn-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-turn-terminal
                           :run-id run-id :turn-id turn-id)
                     trace)))
              (should-not (member "run.started" types))
              (should (equal '("intent.received" "run.failed" "turn.completed")
                             types))
              (should (eq 'proved (plist-get run-judgment :verdict)))
              (should
               (eq 'audit-incomplete
                   (plist-get (plist-get run-judgment :judgment)
                              :terminal-kind)))
              (should (eq 'proved (plist-get turn-judgment :verdict)))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/timed-out-run-closes-despite-hook-errors ()
  "A timed-out Run persists closure and ignores extension hook failures."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events
          (hook-calls 0))
      (setq superchat-turn-completed-functions
            (list (lambda (_status)
                    (cl-incf hook-calls)
                    (error "broken extension hook"))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status timed-out :response "provider timeout")))))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'timed-out (plist-get status :status)))
          (should (= 1 hook-calls))
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/completion-audit-failure-is-visible ()
  "A post-response Tape failure becomes audit-incomplete, never success."
  (superchat-runtime-test--isolated
    (let ((complete-turn
           (symbol-function 'superchat-runtime--complete-turn))
          (next-id 0)
          delivered
          completed-hook turn-hook completion-call)
      (setq superchat-run-completed-functions
            (list (lambda (_status) (setq completed-hook t))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "response.completed" (plist-get event :type))
                     (error "disk full")
                     (cl-incf next-id))))
                ((symbol-function 'superchat-runtime--complete-turn)
                 (lambda (entry status causation-id &optional unaudited)
                   (setq completion-call (list status unaudited))
                   (funcall complete-turn
                            entry status causation-id unaudited)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id) (setq delivered answer)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'audit-incomplete (plist-get status :status)))
          (should (string-match-p "Audit incomplete" delivered))
          (should-not completed-hook)
          (should turn-hook)
          (should (equal '(audit-incomplete t) completion-call)))))))

(ert-deftest runtime/tool-completion-audit-failure-closes-run ()
  "A completed external effect without completion audit closes the Run."
  (superchat-runtime-test--isolated
    (let ((superchat-authority-approval-function (lambda (_request) t))
          (next-id 0)
          events side-effect delivered-status cancelled)
      (superchat-authority-register-effect-resolver
       "runtime_external_send"
       (lambda (_args)
         '((:effect external-write
            :resource message
            :scope "external://recipient/42"))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "tool.completed" (plist-get event :type))
                       (error "completion audit unavailable")
                     (push (plist-get event :type) events)
                     (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'request 'provider-request)
                   (superchat-tool-execute
                    "runtime_external_send"
                    (lambda ()
                      (setq side-effect t)
                      "sent")
                    nil
                    :context superchat--tool-execution-context)
                   ;; A late provider completion cannot recover the Run.
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-submit
                 (list
                  :input "send"
                  :prompt "send"
                  :attendance 'attended
                  :authority-policy 'cowork
                  :delivery-function
                  (lambda (_value status _tape-id)
                    (setq delivered-status status)))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should side-effect)
          (should (eq 'audit-incomplete (plist-get status :status)))
          (should (eq 'audit-incomplete delivered-status))
          (should (eq 'provider-request cancelled))
          (should-not (member "run.completed" events))
          (should-not (member "turn.completed" events))
          (should (plist-get status :completed-at)))))))

(ert-deftest runtime/audit-incomplete-survives-restart-without-faking-effect ()
  "Tape recovery seals the Run but leaves its unaudited Effect unresolved."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-audit-recovery-" t))
           (superchat-data-directory directory)
           (superchat-authority-approval-function (lambda (_request) t))
           (append-record
            (symbol-function 'superchat-tape-append-event-record))
           run-id)
      (unwind-protect
          (progn
            (superchat-db-close)
            (superchat-authority-register-effect-resolver
             "runtime_recovery_external_send"
             (lambda (_args)
               '((:effect external-write
                  :resource message
                  :scope "external://recipient/42"))))
            (cl-letf
                (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal "tool.completed" (plist-get event :type))
                        (error "completion audit unavailable")
                      (funcall append-record event))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'llm-cancel-request) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (funcall superchat--llm-runtime-observer
                             'request 'provider-request)
                    (superchat-tool-execute
                     "runtime_recovery_external_send"
                     (lambda () "sent") nil
                     :context superchat--tool-execution-context))))
              (let ((handle
                     (superchat-runtime-submit
                      (list :input "send"
                            :prompt "send"
                            :session-id "session-audit-recovery"
                            :attendance 'attended
                            :authority-policy 'cowork))))
                (setq run-id (plist-get handle :run-id))
                (should
                 (eq 'audit-incomplete
                     (plist-get (superchat-runtime-status run-id) :status)))))
            ;; Simulate a fresh process: only durable recovery + Tape remain.
            (superchat-db-close)
            (setq superchat-runtime--runs (make-hash-table :test #'equal))
            (let* ((trace
                    (superchat-tape-replay-events "session-audit-recovery"))
                   (types (mapcar (lambda (event) (plist-get event :type))
                                  trace))
                   (run-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-run-terminal :run-id run-id)
                     trace))
                   (turn-id (plist-get (car trace) :turn-id))
                   (turn-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-turn-terminal
                           :run-id run-id :turn-id turn-id)
                     trace))
                   (pending
                    (superchat-closure-judge
                     (list :kind 'pending-effects :run-id run-id)
                     trace)))
              (should-not (member "tool.completed" types))
              (should (member "run.failed" types))
              (should (member "turn.completed" types))
              (should (eq 'proved (plist-get run-judgment :verdict)))
              (should
               (eq 'audit-incomplete
                   (plist-get (plist-get run-judgment :judgment)
                              :terminal-kind)))
              (should (eq 'proved (plist-get turn-judgment :verdict)))
              (should (eq 'open (plist-get pending :verdict)))
              (should
               (equal (mapcar (lambda (event) (plist-get event :event-id))
                              trace)
                      (mapcar
                       (lambda (event) (plist-get event :event-id))
                       (superchat-tape-replay-events
                        "session-audit-recovery"))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/audit-incomplete-releases-after-delivery-error ()
  "Audit failure still releases private state when its delivery adapter fails."
  (superchat-runtime-test--isolated
    (let ((next-id 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "response.completed" (plist-get event :type))
                       (error "disk full")
                     (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((request
                (append
                 (superchat-runtime-test--request)
                 (list :ephemeral-buffer t
                       :delivery-function
                       (lambda (&rest _args)
                         (error "renderer broken")))))
               (handle (superchat-runtime-submit request))
               (run-id (plist-get handle :run-id))
               (entry (superchat-runtime--entry run-id)))
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should-not (plist-get entry :delivery-function))
          (should-not (get-buffer " *superchat-runtime-test*")))))))

(ert-deftest runtime/record-message-reuses-runtime-tape-events ()
  "Conversation history reuses Runtime event IDs without legacy duplicates."
  (let ((superchat--conversation-history nil)
        (db-appends 0))
    (cl-letf (((symbol-function 'superchat-db-tape-append)
               (lambda (&rest _args) (cl-incf db-appends))))
      (superchat--record-message "user" "hello" 11)
      (superchat--record-message "assistant" "done" 12)
      (should (= 0 db-appends))
      (should
       (equal '(12 11)
              (mapcar (lambda (message) (plist-get message :id))
                      superchat--conversation-history))))))

(provide 'test-runtime)

;;; test-runtime.el ends here
