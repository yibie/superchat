;;; test-workspace.el --- Tests for superchat-workspace -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'superchat-workspace)

(defmacro superchat-test--with-workspace (&rest body)
  "Execute BODY in a fresh temp buffer with workspace region set."
  `(let ((superchat-workspace-auto-create nil)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0))
     (with-temp-buffer
       (insert "before region\n"
               "line one\n"
               "line two\n"
               "after region\n")
       (superchat-workspace-set-region 15 33)  ;; "line one\nline two\n" = 18 chars
       ,@body)))

(defun superchat-test-workspace--row
    (id workspace-id type payload &optional envelope)
  "Return a Tape-like ROW for workspace projection tests."
  (list id "workspace" "system" type
        (json-encode
         (append
          (list :event_type type
                :workspace_id workspace-id
                :payload payload)
          envelope))))

(ert-deftest test-workspace-region-read ()
  "Workspace read returns region content."
  (superchat-test--with-workspace
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should (string-match-p "line two" content))
     (should-not (string-match-p "before region" content))
     (should-not (string-match-p "after region" content)))))

(ert-deftest test-workspace-region-write-replace ()
  "Workspace write replaces region content."
  (superchat-test--with-workspace
   (superchat-workspace-write "new content")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "new content" content))
     (should-not (string-match-p "line one" content))
     (should-not (string-match-p "line two" content)))))

(ert-deftest test-workspace-region-write-append ()
  "Workspace write with append adds content at end of region."
  (superchat-test--with-workspace
   (superchat-workspace-write "appended" t)
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should (string-match-p "line two" content))
     (should (string-match-p "appended" content)))))

(ert-deftest test-workspace-region-markers-track-edits ()
  "Markers auto-track buffer changes."
  (superchat-test--with-workspace
   ;; Insert before region — markers move
   (goto-char 1)
   (insert "PREFIX\n")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line one" content))
     (should-not (string-match-p "PREFIX" content)))
   ;; Insert after region — markers unaffected
   (goto-char (point-max))
   (insert "\nSUFFIX")
   (let ((content (superchat-workspace-read)))
     (should (string-match-p "line two" content))
     (should-not (string-match-p "SUFFIX" content)))))

(ert-deftest test-workspace-info-active-region ()
  "Workspace info reports active region details."
  (superchat-test--with-workspace
   (let ((info (superchat-workspace-info)))
     (should (string-match-p "active region" info)))))

(ert-deftest test-workspace-clear-deactivates ()
  "Clearing workspace makes it inactive."
  (superchat-test--with-workspace
   (should (superchat-workspace--active-p))
   (superchat-workspace-clear)
   (should-not (superchat-workspace--active-p))))

(ert-deftest test-workspace-read-nil-when-inactive ()
  "Workspace read returns nil when no region is active and no fallback buffer."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (should (null (superchat-workspace-read)))))

(ert-deftest test-workspace-fallback-write-replaces ()
  "Non-append writes to the fallback buffer replace its content."
  (let ((superchat-workspace-auto-create t)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (unwind-protect
        (progn
          (superchat-workspace-write "first")
          (superchat-workspace-write "second")
          (let ((content (superchat-workspace-read)))
            (should (string-match-p "second" content))
            (should-not (string-match-p "first" content)))
          (superchat-workspace-write "third" t)
          (let ((content (superchat-workspace-read)))
            (should (string-match-p "second" content))
            (should (string-match-p "third" content))))
      (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
        (kill-buffer superchat-workspace-fallback-buffer)))))

(ert-deftest test-workspace-no-region-no-fallback-errors ()
  "Write errors when no region, no auto-create, and no fallback buffer."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (when (buffer-live-p (get-buffer superchat-workspace-fallback-buffer))
      (kill-buffer superchat-workspace-fallback-buffer))
    (should-error (superchat-workspace-write "test"))))

(ert-deftest test-workspace-ids-are-isolated ()
  "Workspace IDs keep active regions isolated."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (with-temp-buffer
      (insert "alpha")
      (superchat-workspace-bind "workspace-a")
      (let ((first (superchat-workspace-set-region
                    (point-min) (point-max))))
        (with-temp-buffer
          (insert "beta")
          (superchat-workspace-bind "workspace-b")
          (let ((second (superchat-workspace-set-region
                         (point-min) (point-max))))
            (should-not (equal (plist-get first :artifact-id)
                               (plist-get second :artifact-id)))
            (should-not (equal (plist-get first :uri)
                               (plist-get second :uri))))
          (should (equal "alpha" (superchat-workspace-read nil "workspace-a")))
          (should (equal "beta" (superchat-workspace-read nil "workspace-b"))))))))

(ert-deftest test-workspace-targets-multiple-artifacts ()
  "A workspace can read and write a selected artifact."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (with-temp-buffer
      (insert "one\ntwo\n")
      (superchat-workspace-bind "workspace-a")
      (let* ((first (superchat-workspace-add-region 1 4 'draft))
             (second (superchat-workspace-add-region 5 8 'notes))
             (first-id (plist-get first :artifact-id))
             (second-id (plist-get second :artifact-id))
             (first-revision (plist-get first :revision)))
        (should (equal "one" (superchat-workspace-read first-id "workspace-a")))
        (should (equal "two" (superchat-workspace-read second-id "workspace-a")))
        (superchat-workspace-write "ONE" nil first-id first-revision "workspace-a")
        (should (equal "ONE\n" (superchat-workspace-read first-id "workspace-a")))
        (should (equal "two" (superchat-workspace-read second-id "workspace-a")))
        (let ((info (superchat-workspace-info "workspace-a")))
          (should (string-match-p first-id info))
          (should (string-match-p second-id info))
          (should (string-match-p "buffer://" info))
          (should (string-match-p "revision" info)))
        (should (eq (plist-get (car (superchat-workspace-artifacts "workspace-a"))
                               :type)
                    'buffer-region))))))

(ert-deftest test-workspace-invalid-artifact-id-errors ()
  "Targeting an unknown artifact fails."
  (superchat-test--with-workspace
   (should-error (superchat-workspace-read "missing"))
   (should-error (superchat-workspace-write "x" nil "missing"))))

(ert-deftest test-workspace-killed-buffer-is-unresolved ()
  "Killed buffers leave artifact metadata but cannot be read."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (let ((buf (generate-new-buffer "superchat-workspace-dead")))
      (with-current-buffer buf
        (insert "gone")
        (superchat-workspace-bind "workspace-a")
        (superchat-workspace-set-region (point-min) (point-max)))
      (kill-buffer buf)
      (let ((artifact (car (superchat-workspace-artifacts "workspace-a"))))
        (should-not (plist-get artifact :live))
        (should-error
         (superchat-workspace-read (plist-get artifact :artifact-id)
                                   "workspace-a"))
        (should-error
         (superchat-workspace-show (plist-get artifact :artifact-id)
                                   "workspace-a"))))))

(ert-deftest test-workspace-current-id-prefers-tool-context ()
  "Tool execution context wins over chat buffer binding."
  (let ((superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (superchat--tool-execution-context '(:workspace-id "tool-workspace"))
        (superchat--workspace-binding '(:id "chat-workspace" :root "/tmp")))
    (should (equal "tool-workspace" (superchat-workspace-current-id)))))

(ert-deftest test-workspace-tool-context-shares-artifacts-across-buffers ()
  "Runs with one Workspace ID resolve the same Artifact registry."
  (let ((superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (superchat--tool-execution-context
         '(:session-id "session-a" :workspace-id "workspace-a")))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (&rest _args) 1)))
      (with-temp-buffer
        (insert "shared")
        (superchat-workspace-add-region (point-min) (point-max))
        (with-temp-buffer
          (should (equal "shared" (superchat-workspace-read))))))))

(ert-deftest test-workspace-current-id-uses-chat-buffer-before-legacy ()
  "Context resolution checks only the configured chat buffer before legacy."
  (let ((superchat-buffer-name "*superchat-test-chat*")
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (chat (generate-new-buffer "*superchat-test-chat*"))
        (other (generate-new-buffer "*superchat-other-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (setq-local superchat--workspace-binding
                        '(:id "wrong-workspace" :root "/tmp/wrong")))
          (with-current-buffer chat
            (setq-local superchat--workspace-binding
                        '(:id "chat-workspace" :root "/tmp/chat")))
          (with-temp-buffer
            (should (equal "chat-workspace" (superchat-workspace-current-id)))))
      (kill-buffer chat)
      (kill-buffer other))))

(ert-deftest test-workspace-tool-write-requires-current-revision ()
  "Tool-context writes to an existing artifact reject stale revisions."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (superchat--tool-execution-context '(:session-id "s1"
                                             :turn-id "turn-1"
                                             :run-id "run-1"
                                             :actor-id "actor-1"
                                             :workspace-id "workspace-a")))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (&rest _args) 1)))
      (with-temp-buffer
        (insert "old")
        (let* ((artifact (superchat-workspace-add-region (point-min) (point-max)))
               (artifact-id (plist-get artifact :artifact-id))
               (revision (plist-get artifact :revision)))
          (should-error
           (superchat-workspace-write "missing" nil artifact-id nil "workspace-a"))
          (superchat-workspace-write "new" nil artifact-id revision "workspace-a")
          (should-error
           (superchat-workspace-write "stale" nil artifact-id revision "workspace-a"))
          (should (equal "new\n" (superchat-workspace-read artifact-id "workspace-a"))))))))

(ert-deftest test-workspace-cross-run-read-requires-delivery-proof ()
  "A tool cannot consume another Run's Artifact without proof and revision."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (superchat--tool-execution-context
         '(:session-id "child-session" :turn-id "child-turn"
           :run-id "child-run" :actor-id "child"
           :workspace-id "workspace-a"))
        decision-reference)
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (&rest _args) 1)))
      (with-temp-buffer
        (insert "deliverable")
        (let* ((artifact
                (superchat-workspace-add-region (point-min) (point-max)))
               (artifact-id (plist-get artifact :artifact-id))
               (revision (plist-get artifact :revision))
               (superchat--tool-execution-context
                '(:session-id "parent-session" :run-id "parent-run"
                  :actor-id "parent" :workspace-id "workspace-a")))
          (cl-letf (((symbol-function
                      'superchat-workspace-artifact-delivery-decision)
                     (lambda (_session-id _run-id reference &optional _trace)
                       (setq decision-reference reference)
                       '(:action deliver :reason proved))))
            (should-error
             (superchat-workspace-read artifact-id "workspace-a"))
            (should
             (equal "deliverable"
                    (superchat-workspace-read
                     artifact-id "workspace-a" revision)))
            (should (equal revision
                           (plist-get decision-reference :revision)))
            (should-error
             (superchat-workspace-read
              artifact-id "workspace-a" "stale"))))))))

(ert-deftest test-workspace-tape-events-omit-content ()
  "Artifact lifecycle events carry identifiers and revisions, not content."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (events nil)
        (superchat--tool-execution-context '(:session-id "s1"
                                             :turn-id "turn-1"
                                             :run-id "run-1"
                                             :actor-id "actor-1"
                                             :workspace-id "workspace-a")))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (event)
                 (push event events)
                 1)))
      (with-temp-buffer
        (insert "secret body")
        (let* ((artifact (superchat-workspace-add-region (point-min) (point-max)))
               (artifact-id (plist-get artifact :artifact-id))
               (revision (plist-get artifact :revision)))
          (superchat-workspace-write "changed secret" nil artifact-id revision)
          (superchat-workspace-clear artifact-id)
          (dolist (event events)
            (should (equal "workspace" (plist-get event :topic)))
            (should (equal "workspace-a" (plist-get event :workspace-id)))
            (should (equal "run-1" (plist-get event :run-id)))
            (should (equal "actor-1" (plist-get event :actor-id)))
            (should (plist-get (plist-get event :payload) :content_hash))
            (should-not (string-match-p "secret" (prin1-to-string event)))))))))

(ert-deftest test-workspace-projection-replays-lifecycle ()
  "Workspace projection rebuilds artifact references from Tape rows."
  (let ((events
         (list
          (superchat-test-workspace--row
           1 "workspace-a" "workspace.artifact.added"
           '(:artifact_id "a1" :uri "buffer://a1" :type "buffer-region"
             :adapter "buffer-region" :role "draft" :revision "r1"
             :content_hash "r1" :buffer_name "buf" :start 1 :end 4
             :live t)
           '(:event_id "evt-1" :session_id "child-session"
             :run_id "run-1" :actor_id "child-1"))
          (superchat-test-workspace--row
           2 "workspace-a" "workspace.artifact.changed"
           '(:artifact_id "a1" :uri "buffer://a1" :old_revision "r1"
             :revision "r2" :content_hash "r2" :buffer_name "new-buf"
             :start 2 :end 6)
           '(:event_id "evt-2" :session_id "child-session"
             :run_id "run-1" :actor_id "child-1"))
          (superchat-test-workspace--row
           3 "other" "workspace.artifact.removed"
           '(:artifact_id "x" :uri "buffer://x" :revision "rx"
             :content_hash "rx")))))
    (cl-letf (((symbol-function 'superchat-db-tape-select)
               (lambda (&rest _args) events)))
      (let ((artifacts (superchat-workspace-artifacts "workspace-a" t)))
        (should (= 1 (length artifacts)))
        (should (equal "a1" (plist-get (car artifacts) :artifact-id)))
        (should (equal "r2" (plist-get (car artifacts) :revision)))
        (should (equal "evt-2" (plist-get (car artifacts) :event-id)))
        (should (equal "child-session"
                       (plist-get (car artifacts) :session-id)))
        (should (equal "run-1" (plist-get (car artifacts) :run-id)))
        (should (equal "child-1" (plist-get (car artifacts) :actor-id)))
        (should (equal "new-buf" (plist-get (car artifacts) :buffer-name)))
        (should (= 6 (plist-get (car artifacts) :end)))
        (should-not (plist-get (car artifacts) :live))))))

(ert-deftest test-workspace-bind-rehydrates-projected-artifacts ()
  "Binding a workspace loads persisted unresolved references."
  (let ((superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (events
         (list
          (superchat-test-workspace--row
           1 "workspace-a" "workspace.artifact.added"
           '(:artifact_id "artifact-1" :uri "buffer://artifact-1"
             :type "buffer-region" :adapter "buffer-region" :role "draft"
             :revision "r1" :content_hash "r1"))
          (superchat-test-workspace--row
           2 "workspace-a" "workspace.artifact.added"
           '(:artifact_id "artifact-2" :uri "buffer://artifact-2"
             :type "buffer-region" :adapter "buffer-region" :role "notes"
             :revision "r2" :content_hash "r2")))))
    (cl-letf (((symbol-function 'superchat-db-tape-select)
               (lambda (&rest _args) events)))
      (let ((workspace (superchat-workspace-bind "workspace-a")))
        (should (equal "artifact-2" (plist-get workspace :active-artifact-id)))
        (should-not (plist-get (car (superchat-workspace-artifacts "workspace-a"))
                               :live)))
      (with-temp-buffer
        (insert "new")
        (let ((artifact (superchat-workspace-add-region (point-min) (point-max)
                                                        nil "workspace-a")))
          (should-not (equal "artifact-1"
                             (plist-get artifact :artifact-id))))))))

(ert-deftest test-workspace-fallback-lifecycle-projects ()
  "Fallback writes emit add/change events that replay as one unresolved ref."
  (let ((superchat-workspace-auto-create t)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (events nil)
        (superchat--tool-execution-context '(:session-id "s1"
                                             :workspace-id "workspace-a")))
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (push event events)
                     1)))
          (superchat-workspace-write "fallback secret" nil nil nil "workspace-a")
          (should (equal '("workspace.artifact.changed"
                           "workspace.artifact.added")
                         (mapcar (lambda (event) (plist-get event :type))
                                 events)))
          (should-not (string-match-p "secret" (prin1-to-string events)))
          (let ((rows (cl-loop for event in (reverse events)
                               for id from 1
                               collect
                               (list id "workspace" "system"
                                     (plist-get event :summary)
                                     (json-encode
                                      (list :event_type (plist-get event :type)
                                            :workspace_id
                                            (plist-get event :workspace-id)
                                            :payload
                                            (plist-get event :payload)))))))
            (cl-letf (((symbol-function 'superchat-db-tape-select)
                       (lambda (&rest _args) rows)))
              (let ((artifacts
                     (superchat-workspace-artifacts "workspace-a" t)))
                (should (= 1 (length artifacts)))
                (should (eq 'fallback (plist-get (car artifacts) :role)))
                (should-not (plist-get (car artifacts) :live))))))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p superchat-workspace-fallback-buffer
                               (buffer-name buffer))
          (kill-buffer buffer))))))

(ert-deftest test-workspace-audit-failure-rolls-back-buffer-edit ()
  "If Tape append fails, the buffer mutation is rolled back."
  (let ((superchat-workspace-auto-create nil)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (superchat--tool-execution-context '(:session-id "s1"
                                             :workspace-id "workspace-a")))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (&rest _args) 1)))
      (with-temp-buffer
        (insert "old")
        (let* ((artifact (superchat-workspace-add-region (point-min) (point-max)))
               (artifact-id (plist-get artifact :artifact-id))
               (revision (plist-get artifact :revision)))
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (lambda (&rest _args) (error "audit down"))))
            (should-error
             (superchat-workspace-write "new" nil artifact-id revision "workspace-a")))
          (should (equal "old" (superchat-workspace-read artifact-id "workspace-a"))))))))

(ert-deftest test-workspace-named-fallbacks-are-isolated ()
  "Named workspaces do not share fallback buffer content."
  (let ((superchat-workspace-auto-create t)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0))
    (unwind-protect
        (progn
          (superchat-workspace-bind "workspace-a")
          (superchat-workspace-write "alpha" nil nil nil "workspace-a")
          (superchat-workspace-bind "workspace-b")
          (superchat-workspace-write "beta" nil nil nil "workspace-b")
          (should (equal "alpha\n" (superchat-workspace-read nil "workspace-a")))
          (should (equal "beta\n" (superchat-workspace-read nil "workspace-b")))
          (should-not (get-buffer superchat-workspace-fallback-buffer)))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p superchat-workspace-fallback-buffer
                               (buffer-name buffer))
          (kill-buffer buffer))))))

(ert-deftest test-workspace-published-artifact-survives-registry-loss ()
  "Published Artifact content remains readable after process-local loss."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-workspace-publish-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat--tool-execution-context
          '(:session-id "producer-session"
            :run-id "producer-run"
            :actor-id "collector"
            :workspace-id "research-workspace"))
         metadata)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (setq metadata
                (superchat-workspace-publish
                 "durable research notes" 'collector
                 "research-workspace"))
          (setq superchat-workspace--registry nil)
          (superchat-db-close)
          (superchat-workspace-bind "research-workspace")
          (should (string-match-p "active Artifact"
                                  (superchat-workspace-info
                                   "research-workspace")))
          (should
           (equal "durable research notes"
                  (superchat-workspace-read
                   (plist-get metadata :artifact-id)
                   "research-workspace"
                   (plist-get metadata :revision)))))
      (superchat-db-close)
      (delete-directory directory t))))

(provide 'test-workspace)

;;; test-workspace.el ends here
