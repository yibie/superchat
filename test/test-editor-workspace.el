;;; test-editor-workspace.el --- Session EditorWorkspace tests -*- lexical-binding: t; -*-

;;; Code:

(setq load-prefer-newer t)

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'ibuf-ext)
(require 'superchat-editor-workspace)

(defun editor-workspace-test--kill-buffer (buffer)
  "Kill BUFFER without leaving this test's query or hook fixtures installed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions nil)
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer)))

(defmacro editor-workspace-test--with-sessions (sessions &rest body)
  "Run BODY and close editor workspaces named by SESSIONS afterward."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn ,@body)
     (dolist (session-id ,sessions)
       (dolist (buffer (superchat-editor-workspace-owned-buffers session-id))
         (editor-workspace-test--kill-buffer buffer))
       (ignore-errors (superchat-session-workspace-close session-id))
       (remhash session-id superchat-editor-workspace--states))))

(ert-deftest editor-workspace/lazy-lookup-allocates-no-buffer ()
  "Looking up a missing role leaves the logical workspace unmaterialized."
  (let ((session-id "editor-workspace-lazy"))
    (editor-workspace-test--with-sessions (list session-id)
      (should (eq 'unmaterialized
                  (superchat-editor-workspace-state session-id)))
      (should-not
       (superchat-editor-workspace-buffer session-id 'protocol nil))
      (should-error
       (superchat-editor-workspace-buffer session-id nil nil))
      (should-not
       (superchat-editor-workspace-owned-buffers session-id))
      (should (eq 'unmaterialized
                  (superchat-editor-workspace-state session-id))))))

(ert-deftest editor-workspace/invalid-default-directory-does-not-publish ()
  "Creation without a valid captured directory leaves no live projection."
  (let ((session-id "editor-workspace-invalid-directory")
        (invalid-directory
         (file-name-as-directory
          (make-temp-name
           (expand-file-name "superchat-invalid-directory-"
                             temporary-file-directory))))
        allocation-error)
    (editor-workspace-test--with-sessions (list session-id)
      (should-not (file-directory-p invalid-directory))
      (let ((default-directory invalid-directory))
        (condition-case err
            (superchat-editor-workspace-buffer session-id 'protocol t)
          (error (setq allocation-error err))))
      (should allocation-error)
      (should (eq 'unmaterialized
                  (superchat-editor-workspace-state session-id)))
      (should-not
       (superchat-editor-workspace-owned-buffers session-id)))))

(ert-deftest editor-workspace/ownership-publication-cannot-undo-close ()
  "A synchronous close during ownership publication wins over acquisition."
  (let ((session-id "editor-workspace-publication-close")
        close-result
        allocation-result
        allocation-error)
    (editor-workspace-test--with-sessions (list session-id)
      (let ((watcher
             (lambda (_symbol new-value operation _where)
               (when (and (eq operation 'set)
                          (eq new-value t)
                          (equal superchat--editor-workspace-session-id
                                 session-id))
                 (setq close-result
                       (superchat-session-workspace-close session-id))))))
        (unwind-protect
            (progn
              (add-variable-watcher
               'superchat--editor-workspace-owned-p watcher)
              (condition-case err
                  (setq allocation-result
                        (superchat-editor-workspace-buffer
                         session-id 'protocol t))
                (error (setq allocation-error err))))
          (remove-variable-watcher
           'superchat--editor-workspace-owned-p watcher)))
      (should (eq 'closed close-result))
      (should-not
       (and (bufferp allocation-result)
            (buffer-live-p allocation-result)))
      (should (or allocation-error
                  (not allocation-result)
                  (not (buffer-live-p allocation-result))))
      (should (eq 'closed
                  (superchat-editor-workspace-state session-id)))
      (should-not
       (superchat-editor-workspace-owned-buffers session-id)))))

(ert-deftest editor-workspace/publication-rollback-retains-kill-survivor ()
  "A refused rollback kill remains visible and recoverable as retained."
  (let ((session-id "editor-workspace-publication-retained")
        close-result
        allocation-error)
    (editor-workspace-test--with-sessions (list session-id)
      (let ((watcher
             (lambda (_symbol new-value operation _where)
               (when (and (eq operation 'set)
                          (eq new-value t)
                          (equal superchat--editor-workspace-session-id
                                 session-id))
                 (setq close-result
                       (superchat-session-workspace-close session-id))
                 (setq-local kill-buffer-query-functions
                             (list (lambda () nil)))))))
        (unwind-protect
            (progn
              (add-variable-watcher
               'superchat--editor-workspace-owned-p watcher)
              (condition-case err
                  (superchat-editor-workspace-buffer
                   session-id 'protocol t)
                (error (setq allocation-error err))))
          (remove-variable-watcher
           'superchat--editor-workspace-owned-p watcher)))
      (should close-result)
      (should allocation-error)
      (should (eq 'retained
                  (superchat-editor-workspace-state session-id)))
      (let ((survivors
             (superchat-editor-workspace-owned-buffers session-id)))
        (should (= 1 (length survivors)))
        (should (eq 'retained
                    (superchat-session-workspace-close session-id)))
        (with-current-buffer (car survivors)
          (setq-local kill-buffer-query-functions nil))
        (should (eq 'closed
                    (superchat-session-workspace-close session-id)))
        (should-not (buffer-live-p (car survivors)))))))

(ert-deftest editor-workspace/closed-state-rescans-exact-owned-survivor ()
  "A closed state cannot hide a live buffer carrying exact owned markers."
  (let ((session-id "editor-workspace-closed-survivor"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((buffer
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        ;; Model recovery from an interrupted publication whose state commit
        ;; and exact live ownership markers disagree.
        (puthash session-id 'closed superchat-editor-workspace--states)
        (should (eq 'closed
                    (superchat-session-workspace-close session-id)))
        (should-not (buffer-live-p buffer))
        (should-not
         (superchat-editor-workspace-owned-buffers session-id))))))

(ert-deftest editor-workspace/same-session-role-reuses-exact-live-buffer ()
  "A live role buffer is reused and carries exact ownership metadata."
  (let ((session-id "editor-workspace-role-reuse"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((first
             (superchat-editor-workspace-buffer session-id 'protocol t))
            second)
        (setq second
              (superchat-editor-workspace-buffer session-id 'protocol t))
        (should (buffer-live-p first))
        (should (eq first second))
        (should (eq 'open (superchat-editor-workspace-state session-id)))
        (with-current-buffer first
          (should (local-variable-p
                   'superchat--editor-workspace-session-id))
          (should (equal session-id
                         superchat--editor-workspace-session-id))
          (should (local-variable-p 'superchat--editor-workspace-owned-p))
          (should (eq t superchat--editor-workspace-owned-p))
          (should (local-variable-p 'superchat--editor-workspace-role))
          (should (eq 'protocol superchat--editor-workspace-role)))))))

(ert-deftest editor-workspace/dead-role-buffer-is-recreated ()
  "A dead role buffer is absence, so create returns a new exact object."
  (let ((session-id "editor-workspace-dead-recreate"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((first
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (editor-workspace-test--kill-buffer first)
        (should-not
         (superchat-editor-workspace-buffer session-id 'protocol nil))
        (let ((second
               (superchat-editor-workspace-buffer session-id 'protocol t)))
          (should (buffer-live-p second))
          (should-not (eq first second)))))))

(ert-deftest editor-workspace/acquisition-reports-freshness-atomically ()
  "The additive acquisition API reports fresh creation and exact reuse."
  (let ((session-id "editor-workspace-acquisition"))
    (editor-workspace-test--with-sessions (list session-id)
      (let* ((fresh (superchat-editor-workspace-acquire-buffer
                     session-id 'protocol))
             (reused (superchat-editor-workspace-acquire-buffer
                      session-id 'protocol)))
        (should (bufferp (plist-get fresh :buffer)))
        (should (eq t (plist-get fresh :newly-acquired)))
        (should (eq (plist-get fresh :buffer) (plist-get reused :buffer)))
        (should-not (plist-get reused :newly-acquired))
        (should (eq (plist-get fresh :buffer)
                    (superchat-editor-workspace-buffer
                     session-id 'protocol nil)))))))

(ert-deftest editor-workspace/rollback-is-fresh-exact-and-idempotent ()
  "Rollback closes only the exact fresh tuple and false is unconditional."
  (let ((session-id "editor-workspace-rollback")
        (other-session "editor-workspace-rollback-other")
        (dead (generate-new-buffer " *editor-workspace-dead-rollback*")))
    (editor-workspace-test--with-sessions (list session-id other-session)
      (let* ((fresh (superchat-editor-workspace-acquire-buffer
                     session-id 'protocol))
             (buffer (plist-get fresh :buffer))
             (other (superchat-editor-workspace-buffer
                     other-session 'protocol t)))
        (kill-buffer dead)
        (should-not (superchat-editor-workspace-rollback-acquisition
                     nil nil dead nil))
        (should-not (superchat-editor-workspace-rollback-acquisition
                     other-session 'protocol buffer t))
        (should-not (superchat-editor-workspace-rollback-acquisition
                     session-id 'other-role buffer t))
        (should-not (superchat-editor-workspace-rollback-acquisition
                     session-id 'protocol other t))
        (should (buffer-live-p buffer))
        (should (buffer-live-p other))
        (should (eq 'released
                    (superchat-editor-workspace-rollback-acquisition
                     session-id 'protocol buffer t)))
        (should-not (buffer-live-p buffer))
        (should-not (superchat-editor-workspace-rollback-acquisition
                     session-id 'protocol buffer t))))))

(ert-deftest editor-workspace/rollback-retains-query-and-hook-failures ()
  "Rollback retains an exact survivor and ordinary public close retries it."
  (dolist (failure '(query-error query-quit hook-error hook-quit))
    (let ((session-id (format "editor-workspace-rollback-%s" failure)))
      (editor-workspace-test--with-sessions (list session-id)
        (let* ((fresh (superchat-editor-workspace-acquire-buffer
                       session-id 'protocol))
               (buffer (plist-get fresh :buffer)))
          (with-current-buffer buffer
            (pcase failure
              ('query-error
               (setq-local kill-buffer-query-functions
                           (list (lambda () (error "query failure")))))
              ('query-quit
               (setq-local kill-buffer-query-functions
                           (list (lambda () (signal 'quit '(query))))))
              ('hook-error
               (add-hook 'kill-buffer-hook
                         (lambda () (error "hook failure")) nil t))
              ('hook-quit
               (add-hook 'kill-buffer-hook
                         (lambda () (signal 'quit '(hook))) nil t))))
          (should (eq 'retained
                      (superchat-editor-workspace-rollback-acquisition
                       session-id 'protocol buffer t)))
          (should (buffer-live-p buffer))
          (should (eq 'retained
                      (superchat-editor-workspace-state session-id)))
          (should (memq buffer
                        (superchat-editor-workspace-owned-buffers session-id)))
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil)
            (setq-local kill-buffer-hook nil))
          (should (eq 'closed
                      (superchat-session-workspace-close session-id)))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest editor-workspace/session-membership-is-exact-and-isolated ()
  "Equal roles in different Sessions never share or close each other's host."
  (let ((session-a "editor-workspace-isolation-a")
        (session-b "editor-workspace-isolation-b"))
    (editor-workspace-test--with-sessions (list session-a session-b)
      (let ((buffer-a
             (superchat-editor-workspace-buffer session-a 'protocol t))
            (buffer-b
             (superchat-editor-workspace-buffer session-b 'protocol t)))
        (should-not (eq buffer-a buffer-b))
        (should (equal (list buffer-a)
                       (superchat-editor-workspace-owned-buffers session-a)))
        (should (equal (list buffer-b)
                       (superchat-editor-workspace-owned-buffers session-b)))
        (should (superchat-session-workspace-close session-a))
        (should-not (buffer-live-p buffer-a))
        (should (buffer-live-p buffer-b))
        (should (eq 'closed
                    (superchat-editor-workspace-state session-a)))
        (should (eq 'open
                    (superchat-editor-workspace-state session-b)))))))

(ert-deftest editor-workspace/enumeration-scans-live-buffer-markers ()
  "A manually tagged live buffer is discovered without a parallel ledger."
  (let ((session-id "editor-workspace-live-scan")
        (buffer (generate-new-buffer " *editor-workspace-live-scan*")))
    (unwind-protect
        (editor-workspace-test--with-sessions (list session-id)
          ;; Materialize only the workspace state.  BUFFER is deliberately not
          ;; returned by the allocator: discovery must come from live markers.
          (superchat-editor-workspace-buffer session-id 'protocol t)
          (with-current-buffer buffer
            (setq-local superchat--editor-workspace-session-id session-id)
            (setq-local superchat--editor-workspace-owned-p t)
            (setq-local superchat--editor-workspace-role 'manually-tagged))
          (should (memq buffer
                        (superchat-editor-workspace-owned-buffers session-id)))
          (should (superchat-session-workspace-close session-id))
          (should-not (buffer-live-p buffer)))
      (editor-workspace-test--kill-buffer buffer))))

(ert-deftest editor-workspace/unmarked-and-borrowed-buffers-are-never-closed ()
  "Names and Session association without the owned marker grant no authority."
  (let ((session-id "editor-workspace-borrowed")
        (unmarked (generate-new-buffer " *editor-workspace-unmarked*"))
        (borrowed (generate-new-buffer " *editor-workspace-borrowed*")))
    (unwind-protect
        (editor-workspace-test--with-sessions (list session-id)
          (with-current-buffer borrowed
            (setq-local superchat--editor-workspace-session-id session-id)
            (setq-local superchat--editor-workspace-owned-p nil)
            (setq-local superchat--editor-workspace-role 'borrowed))
          (should-not
           (memq unmarked
                 (superchat-editor-workspace-owned-buffers session-id)))
          (should-not
           (memq borrowed
                 (superchat-editor-workspace-owned-buffers session-id)))
          (should (superchat-session-workspace-close session-id))
          (should (buffer-live-p unmarked))
          (should (buffer-live-p borrowed)))
      (editor-workspace-test--kill-buffer unmarked)
      (editor-workspace-test--kill-buffer borrowed))))

(ert-deftest editor-workspace/validate-buffer-returns-exact-owned-object ()
  "Validation accepts exact ownership for the requested Session."
  (let ((session-id "editor-workspace-validate-owned"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((buffer
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (should (eq buffer
                    (superchat-editor-workspace-validate-buffer
                     buffer session-id)))))))

(ert-deftest editor-workspace/validate-buffer-rejects-owner-session-mismatch ()
  "An owned buffer cannot be submitted on behalf of another Session."
  (let ((owner "editor-workspace-validate-owner")
        (other "editor-workspace-validate-other"))
    (editor-workspace-test--with-sessions (list owner other)
      (let ((buffer (superchat-editor-workspace-buffer owner 'protocol t)))
        (should-error
         (superchat-editor-workspace-validate-buffer buffer other)
         :type 'error)))))

(ert-deftest editor-workspace/validate-buffer-rejects-malformed-ownership ()
  "Owned truth without a valid Session marker is malformed ownership."
  (let ((buffer (generate-new-buffer " *editor-workspace-malformed*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local superchat--editor-workspace-owned-p t)
            (setq-local superchat--editor-workspace-session-id nil))
          (should-error
           (superchat-editor-workspace-validate-buffer
            buffer "editor-workspace-expected")
           :type 'error))
      (editor-workspace-test--kill-buffer buffer))))

(ert-deftest editor-workspace/validate-buffer-allows-ordinary-borrowed-object ()
  "An unowned live buffer is a valid exact borrowed Runtime View."
  (let ((buffer (generate-new-buffer " *editor-workspace-validate-borrowed*")))
    (unwind-protect
        (should (eq buffer
                    (superchat-editor-workspace-validate-buffer
                     buffer "editor-workspace-borrower")))
      (editor-workspace-test--kill-buffer buffer))))

(ert-deftest editor-workspace/validate-buffer-rejects-dead-exact-object ()
  "Ownership validation never locates or recreates a dead buffer by name."
  (let ((buffer (generate-new-buffer " *editor-workspace-validate-dead*")))
    (kill-buffer buffer)
    (should-error
     (superchat-editor-workspace-validate-buffer
      buffer "editor-workspace-dead")
     :type 'error)))

(ert-deftest editor-workspace/owner-session-is-the-only-public-marker-query ()
  "The public query returns owner identity without exposing marker shape."
  (let ((session-id "editor-workspace-owner-query")
        (borrowed (generate-new-buffer " *editor-workspace-owner-borrowed*")))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((owned (superchat-editor-workspace-buffer
                    session-id 'session-view t)))
        (should (equal session-id
                       (superchat-editor-workspace-owner-session owned)))
        (should-not (superchat-editor-workspace-owner-session borrowed))))
    (editor-workspace-test--kill-buffer borrowed)))

(ert-deftest editor-workspace/owner-session-rejects-dead-and-malformed-buffers ()
  "Dead objects and malformed ownership never look ordinary and unowned."
  (let ((dead (generate-new-buffer " *editor-workspace-owner-dead*"))
        (malformed (generate-new-buffer " *editor-workspace-owner-malformed*")))
    (kill-buffer dead)
    (with-current-buffer malformed
      (setq-local superchat--editor-workspace-owned-p t)
      (setq-local superchat--editor-workspace-session-id nil))
    (unwind-protect
        (progn
          (should-error (superchat-editor-workspace-owner-session dead)
                        :type 'error)
          (should-error (superchat-editor-workspace-owner-session malformed)
                        :type 'error))
      (editor-workspace-test--kill-buffer malformed))))

(ert-deftest editor-workspace/inherited-markers-never-grant-close-authority ()
  "Default or dynamically bound markers are not buffer ownership facts."
  (let ((session-id "editor-workspace-inherited-markers")
        (buffer (generate-new-buffer " *editor-workspace-inherited*"))
        (superchat--editor-workspace-owned-p t)
        (superchat--editor-workspace-session-id
         "editor-workspace-inherited-markers")
        (superchat--editor-workspace-role 'session-view))
    (unwind-protect
        (progn
          (should-not (superchat-editor-workspace-owner-session buffer))
          (should-not
           (memq buffer
                 (superchat-editor-workspace-owned-buffers session-id)))
          (should (eq 'closed
                      (superchat-session-workspace-close session-id)))
          (should (buffer-live-p buffer)))
      (editor-workspace-test--kill-buffer buffer))))

(ert-deftest editor-workspace/partial-local-markers-are-malformed-not-owned ()
  "One local ownership marker is rejected and never enters a close set."
  (dolist (marker '(owned session))
    (let ((session-id "editor-workspace-partial-markers")
          (buffer (generate-new-buffer " *editor-workspace-partial*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (if (eq marker 'owned)
                  (setq-local superchat--editor-workspace-owned-p t)
                (setq-local superchat--editor-workspace-session-id session-id)))
            (should-error
             (superchat-editor-workspace-owner-session buffer)
             :type 'error)
            (should-not
             (memq buffer
                   (superchat-editor-workspace-owned-buffers session-id)))
            (should (eq 'closed
                        (superchat-session-workspace-close session-id)))
            (should (buffer-live-p buffer)))
        (editor-workspace-test--kill-buffer buffer)))))

(ert-deftest editor-workspace/close-is-idempotent ()
  "Closing open, closed, and unmaterialized workspaces is harmless."
  (let ((open-session "editor-workspace-idempotent-open")
        (missing-session "editor-workspace-idempotent-missing"))
    (editor-workspace-test--with-sessions (list open-session missing-session)
      (let ((buffer
             (superchat-editor-workspace-buffer open-session 'protocol t)))
        (should (superchat-session-workspace-close open-session))
        (should-not (buffer-live-p buffer))
        (should (eq 'closed
                    (superchat-editor-workspace-state open-session)))
        (should (superchat-session-workspace-close open-session))
        (should (eq 'closed
                    (superchat-editor-workspace-state open-session))))
      (should (superchat-session-workspace-close missing-session))
      (should-not
       (superchat-editor-workspace-owned-buffers missing-session)))))

(ert-deftest editor-workspace/kill-query-retains-and-retry-rescans ()
  "A normal kill veto retains its survivor; retry closes it after permission."
  (let ((session-id "editor-workspace-retained"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((buffer
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions (list (lambda () nil))))
        (should (eq 'retained
                    (superchat-session-workspace-close session-id)))
        (should (buffer-live-p buffer))
        (should (eq 'retained
                    (superchat-editor-workspace-state session-id)))
        (should (memq buffer
                      (superchat-editor-workspace-owned-buffers session-id)))
        (should-error
         (superchat-editor-workspace-buffer session-id 'protocol t))
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions nil))
        (should (superchat-session-workspace-close session-id))
        (should-not (buffer-live-p buffer))
        (should (eq 'closed
                    (superchat-editor-workspace-state session-id)))))))

(ert-deftest editor-workspace/active-run-close-rejection-is-zero-mutation ()
  "Rejecting the single active-Run confirmation changes no Session state."
  (let ((session-id "editor-workspace-active-reject")
        (other-session-id "editor-workspace-active-reject-other")
        (projection-calls 0)
        (confirmation-calls 0)
        cancelled
        messages)
    (editor-workspace-test--with-sessions
        (list session-id other-session-id)
      (let ((owned
             (superchat-editor-workspace-buffer session-id 'protocol t))
            (other-owned
             (superchat-editor-workspace-buffer
              other-session-id 'protocol t)))
        (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                   (lambda (&optional _current-run-id)
                     (setq projection-calls (1+ projection-calls))
                     (list
                      :active-runs
                      (list
                       (list :run-id "reject-target-a"
                             :session-id session-id
                             :status 'running-model)
                       (list :run-id "reject-other"
                             :session-id other-session-id
                             :status 'running-model)
                       (list :run-id "reject-target-b"
                             :session-id session-id
                             :status 'delivering)))))
                  ((symbol-function 'superchat-runtime-cancel)
                   (lambda (run-id &optional _response)
                     (push run-id cancelled)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _args)
                     (setq confirmation-calls (1+ confirmation-calls))
                     nil))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _args)
                     (setq confirmation-calls (1+ confirmation-calls))
                     nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (superchat-session-workspace-close session-id))
        (should (= 1 projection-calls))
        (should (= 1 confirmation-calls))
        (should-not cancelled)
        (should (buffer-live-p owned))
        (should (buffer-live-p other-owned))
        (should (eq 'open
                    (superchat-editor-workspace-state session-id)))
        (should
         (cl-some
          (lambda (text)
            (string-match-p
             (regexp-quote "M-x superchat-close-session-workspace") text))
          messages))))))

(ert-deftest editor-workspace/active-run-close-freezes-cancels-then-kills ()
  "Accepted close freezes first and cancels only its active Run snapshot."
  (let ((session-id "editor-workspace-active-accept")
        (other-session-id "editor-workspace-active-accept-other")
        (projection-calls 0)
        (confirmation-calls 0)
        callback-errors
        reentrant-close-results
        cancelled
        events)
    (editor-workspace-test--with-sessions
        (list session-id other-session-id)
      (let ((owned
             (superchat-editor-workspace-buffer session-id 'protocol t))
            (other-owned
             (superchat-editor-workspace-buffer
              other-session-id 'protocol t)))
        (with-current-buffer owned
          (add-hook 'kill-buffer-hook
                    (lambda () (setq events (append events '(kill))))
                    nil t))
        (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                   (lambda (&optional _current-run-id)
                     (setq projection-calls (1+ projection-calls))
                     (list
                      :active-runs
                      (list
                       (list :run-id "accept-target-a"
                             :session-id session-id
                             :status 'running-model)
                       (list :run-id "accept-other"
                             :session-id other-session-id
                             :status 'running-model)
                       (list :run-id "accept-target-b"
                             :session-id session-id
                             :status 'delivering)))))
                  ((symbol-function 'superchat-runtime-cancel)
                   (lambda (run-id &optional _response)
                     (setq cancelled (append cancelled (list run-id))
                           events (append events '(cancel)))
                     (push (superchat-session-workspace-close session-id)
                           reentrant-close-results)
                     ;; Cancellation delivery may synchronously hand work back
                     ;; to a producer.  The Session must already be frozen.
                     (condition-case err
                         (superchat-editor-workspace-buffer
                          session-id 'cancel-callback t)
                       (error (push err callback-errors)))))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _args)
                     (setq confirmation-calls (1+ confirmation-calls))
                     t))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _args)
                     (setq confirmation-calls (1+ confirmation-calls))
                     t)))
          (should (eq 'closed
                      (superchat-session-workspace-close session-id))))
        (should (= 1 projection-calls))
        (should (= 1 confirmation-calls))
        (should (equal '("accept-target-a" "accept-target-b")
                       (sort (copy-sequence cancelled) #'string<)))
        (should (equal '(closing closing) reentrant-close-results))
        (should (= 2 (length callback-errors)))
        (should (equal '(cancel cancel kill) events))
        (should-not (buffer-live-p owned))
        (should (buffer-live-p other-owned))
        (should (eq 'closed
                    (superchat-editor-workspace-state session-id)))
        (should (eq 'open
                    (superchat-editor-workspace-state other-session-id)))))))

(ert-deftest editor-workspace/closing-rejects-reentrant-allocation ()
  "A kill hook cannot allocate another owned buffer while close is in flight."
  (let ((session-id "editor-workspace-closing-freeze")
        existing-role-error
        missing-role-error)
    (editor-workspace-test--with-sessions (list session-id)
      (let ((buffer
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (with-current-buffer buffer
          (add-hook
           'kill-buffer-hook
           (lambda ()
             (setq existing-role-error
                   (condition-case err
                       (progn
                         (superchat-editor-workspace-buffer
                          session-id 'protocol t)
                         nil)
                     (error err))
                   missing-role-error
                   (condition-case err
                       (progn
                         (superchat-editor-workspace-buffer
                          session-id 'late-allocation t)
                         nil)
                     (error err))))
           nil t))
        (should (superchat-session-workspace-close session-id))
        (should existing-role-error)
        (should missing-role-error)
        (should-not
         (superchat-editor-workspace-buffer session-id 'late-allocation nil))
        (should (eq 'closed
                    (superchat-editor-workspace-state session-id)))))))

(ert-deftest editor-workspace/headless-core-needs-no-native-ui-or-project ()
  "Core allocation and close do not consult tab, window, or project adapters."
  (let ((session-id "editor-workspace-headless"))
    (editor-workspace-test--with-sessions (list session-id)
      (cl-letf (((symbol-function 'selected-window)
                 (lambda () (error "selected-window must not be consulted")))
                ((symbol-function 'tab-bar-tabs)
                 (lambda (&rest _args)
                   (error "tab-bar must not be consulted")))
                ((symbol-function 'project-current)
                 (lambda (&rest _args)
                   (error "project.el must not be consulted"))))
        (let ((buffer
               (superchat-editor-workspace-buffer session-id 'protocol t)))
          (should (buffer-live-p buffer))
          (should (equal (list buffer)
                         (superchat-editor-workspace-owned-buffers session-id)))
          (should (superchat-session-workspace-close session-id))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest editor-workspace/headless-explicit-command-needs-no-tab ()
  "The public command accepts an explicit Session without native tab state."
  (let ((session-id "editor-workspace-headless-command"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((buffer
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (cl-letf (((symbol-function 'superchat-runtime-status-projection)
                   (lambda (&optional _current-run-id)
                     '(:active-runs nil)))
                  ((symbol-function 'selected-window)
                   (lambda () (error "selected-window must not be consulted")))
                  ((symbol-function 'tab-bar-tabs)
                   (lambda (&rest _args)
                     (error "tab-bar must not be consulted"))))
          (should (eq 'closed
                      (superchat-close-session-workspace session-id))))
        (should-not (buffer-live-p buffer))))))

(ert-deftest editor-workspace/closed-session-can-reopen ()
  "Closing releases the live projection; later allocation opens it again."
  (let ((session-id "editor-workspace-reopen"))
    (editor-workspace-test--with-sessions (list session-id)
      (let ((first
             (superchat-editor-workspace-buffer session-id 'protocol t)))
        (should (superchat-session-workspace-close session-id))
        (should-not (buffer-live-p first))
        (should (eq 'closed
                    (superchat-editor-workspace-state session-id)))
        (let ((second
               (superchat-editor-workspace-buffer session-id 'protocol t)))
          (should (buffer-live-p second))
          (should-not (eq first second))
          (should (eq 'open
                      (superchat-editor-workspace-state session-id))))))))

(ert-deftest editor-workspace/transient-project-is-memory-only ()
  "A root projects through built-in project.el without discovery or persistence."
  (let ((session-id "editor-workspace-transient-project")
        (root (make-temp-file "superchat-editor-project-" t))
        (project-list-file (make-temp-file "superchat-project-list-"))
        (sentinel "project-list sentinel\n")
        project)
    (unwind-protect
        (editor-workspace-test--with-sessions (list session-id)
          (with-temp-file project-list-file
            (insert sentinel))
          (cl-letf (((symbol-function 'project-remember-project)
                     (lambda (&rest _args)
                       (error "must not remember transient project")))
                    ((symbol-function 'project-current)
                     (lambda (&rest _args)
                       (error "must not perform project discovery"))))
            (setq project
                  (superchat-editor-workspace-project session-id root))
            ;; The explicit buffer projection must preserve the same
            ;; no-discovery/no-persistence boundary as object construction.
            (should (listp
                     (superchat-editor-workspace-project-buffers
                      session-id root))))
          (should (equal `(transient . ,(file-name-as-directory
                                         (expand-file-name root)))
                         project))
          (should (equal (file-name-as-directory (expand-file-name root))
                         (project-root project)))
          (with-temp-buffer
            (insert-file-contents project-list-file)
            (should (equal sentinel (buffer-string))))
          (should (eq 'unmaterialized
                      (superchat-editor-workspace-state session-id)))
          (should-not
           (superchat-editor-workspace-owned-buffers session-id)))
      (when (file-exists-p project-list-file)
        (delete-file project-list-file))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest editor-workspace/nil-root-project-is-pure-absence ()
  "A missing authoritative root returns nil without manufacturing a directory."
  (let ((session-id "editor-workspace-no-project-root")
        (absent-root
         (make-temp-name
          (expand-file-name "superchat-absent-project-"
                            temporary-file-directory))))
    (editor-workspace-test--with-sessions (list session-id)
      (should-not (file-exists-p absent-root))
      (cl-letf (((symbol-function 'make-directory)
                 (lambda (&rest _args)
                   (error "must not manufacture a project directory")))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest _args)
                   (error "must not manufacture a temporary project")))
                ((symbol-function 'project-current)
                 (lambda (&rest _args)
                   (error "must not perform project discovery"))))
        (should-not
         (superchat-editor-workspace-project session-id nil))
        (should-not
         (superchat-editor-workspace-project-buffers session-id nil)))
      (should-error
       (superchat-editor-workspace-project session-id ""))
      (should-error
       (superchat-editor-workspace-project-buffers session-id ""))
      (should-error
       (superchat-editor-workspace-project session-id absent-root))
      (should-not (file-exists-p absent-root))
      (should (eq 'unmaterialized
                  (superchat-editor-workspace-state session-id)))
      (should-not
       (superchat-editor-workspace-owned-buffers session-id)))))

(ert-deftest editor-workspace/project-buffers-do-not-grant-ownership ()
  "Two Sessions can discover one shared file without either owning it."
  (let* ((session-a "editor-workspace-project-buffers-a")
         (session-b "editor-workspace-project-buffers-b")
         (root (make-temp-file "superchat-editor-project-buffers-" t))
         (file (expand-file-name "shared.txt" root))
         shared
         owned-a
         owned-b)
    (unwind-protect
        (editor-workspace-test--with-sessions (list session-a session-b)
          (with-temp-file file
            (insert "shared file\n"))
          (setq shared (find-file-noselect file))
          (let ((default-directory (file-name-as-directory root)))
            (setq owned-a
                  (superchat-editor-workspace-buffer
                   session-a 'project-owned t)
                  owned-b
                  (superchat-editor-workspace-buffer
                   session-b 'project-owned t)))
          (let ((buffers-a
                 (superchat-editor-workspace-project-buffers session-a root))
                (buffers-b
                 (superchat-editor-workspace-project-buffers session-b root)))
            (should (memq owned-a buffers-a))
            (should (memq owned-b buffers-a))
            (should (memq shared buffers-a))
            (should (memq owned-a buffers-b))
            (should (memq owned-b buffers-b))
            (should (memq shared buffers-b)))
          (with-current-buffer shared
            (should-not superchat--editor-workspace-owned-p)
            (should-not superchat--editor-workspace-session-id)
            (should-not superchat--editor-workspace-role))
          (should (equal (list owned-a)
                         (superchat-editor-workspace-owned-buffers session-a)))
          (should (equal (list owned-b)
                         (superchat-editor-workspace-owned-buffers session-b)))
          (should (eq 'closed
                      (superchat-session-workspace-close session-a)))
          (should-not (buffer-live-p owned-a))
          (should (buffer-live-p owned-b))
          (should (buffer-live-p shared))
          (should (eq 'closed
                      (superchat-session-workspace-close session-b)))
          (should-not (buffer-live-p owned-b))
          (should (buffer-live-p shared)))
      (editor-workspace-test--kill-buffer shared)
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest editor-workspace/ibuffer-group-is-live-marker-projection ()
  "The saved-filter group derives membership live and retains survivors."
  (let ((session-id "editor-workspace-ibuffer-group")
        (other-session-id "editor-workspace-ibuffer-group-other")
        (borrowed (generate-new-buffer " *editor-workspace-ibuffer-borrowed*")))
    (unwind-protect
        (editor-workspace-test--with-sessions
            (list session-id other-session-id)
          (let* ((owned
                  (superchat-editor-workspace-buffer
                   session-id 'ibuffer-owned t))
                 (other-owned
                  (superchat-editor-workspace-buffer
                   other-session-id 'ibuffer-owned t))
                 (group
                  (superchat-editor-workspace-ibuffer-group session-id))
                 (filters (cdr group))
                 (other-filters
                  (cdr (superchat-editor-workspace-ibuffer-group
                        other-session-id))))
            (should (stringp (car group)))
            (should (string-match-p (regexp-quote session-id) (car group)))
            (should (consp filters))
            (should (ibuffer-included-in-filters-p owned filters))
            (should-not
             (ibuffer-included-in-filters-p other-owned filters))
            (should
             (ibuffer-included-in-filters-p other-owned other-filters))
            (should-not
             (ibuffer-included-in-filters-p owned other-filters))
            (should-not (ibuffer-included-in-filters-p borrowed filters))
            (with-current-buffer borrowed
              ;; A file/root association or Session hint without the exact
              ;; owned marker remains borrowed.
              (setq-local superchat--editor-workspace-session-id session-id)
              (setq-local superchat--editor-workspace-role 'shared-file))
            (should-not (ibuffer-included-in-filters-p borrowed filters))
            (with-current-buffer owned
              (setq-local kill-buffer-query-functions
                          (list (lambda () nil))))
            (should (eq 'retained
                        (superchat-session-workspace-close session-id)))
            (should (buffer-live-p owned))
            (should (ibuffer-included-in-filters-p owned filters))
            (with-current-buffer owned
              (setq-local kill-buffer-query-functions nil))
            (should (eq 'closed
                        (superchat-session-workspace-close session-id)))
            (should-not (buffer-live-p owned))
            (should (buffer-live-p other-owned))))
      (editor-workspace-test--kill-buffer borrowed))))

(provide 'superchat-test-editor-workspace)

;;; test-editor-workspace.el ends here
