;;; test-cmd-dispatch.el — Test command dispatch through hooks -*- lexical-binding: t; -*-

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat)
(require 'superchat-workflow)
(defvar superchat-workflow-directory)

(defun human-input-test--with-chat (body &optional synchronous tools)
  "Run BODY through a fresh interactive View, with only native transport mocked."
  (unless (and (featurep 'llm) (require 'llm-openai nil t))
    (ert-skip "Requires isolated installed llm 0.31.1"))
  (cmd-clear-test--with-owned-view
   (lambda (view _session)
     (let ((superchat-llm-backend (make-llm-openai :key "unused" :chat-model "input-fixture"))
           (superchat-llm-tools-enabled (if tools 'always 'never))
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
         (dotimes (_ 2)
           (let (messages)
             (cl-letf (((symbol-function 'message)
                        (lambda (format-string &rest args)
                          (push (apply #'format format-string args) messages))))
               (execute-kbd-macro (kbd "C-c C-c")))
             (should (= 1 (length (funcall requests))))
             (should (member "Draft retained. Edit and send it after the current response finishes." messages)))
           (output-prompt-test--assert-draft marker draft 3)
           (should (= 1 (length (funcall requests))))
           (should (equal (list run) (superchat-runtime-active-runs))))
         (goto-char (point-min))
         (let ((point (point)))
           (funcall (plist-get first :partial) "PARTIAL MORE")
           (should (= point (point))))
         (goto-char (+ marker 3))
         (pcase terminal
           ('final (funcall (plist-get first :final) "FINAL"))
           ('cancel
            ;; RET accepts the existing draft; an explicit empty edit now clears it.
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional initial &rest _) initial)))
              (execute-kbd-macro (kbd "C-c C-k"))))
           ('error (funcall (plist-get first :failure) 'error "LOCAL ERROR")))
         (output-prompt-test--assert-draft marker draft 3)
         (should (= 1 (length (funcall requests))))
         (dolist (value (list (plist-get first :prompt) superchat--conversation-history
                              (superchat-db-tape-replay session)
                              (superchat-tape-replay-events session)))
           (should-not (string-match-p "PRIVATE-DRAFT" (prin1-to-string value))))
         (execute-kbd-macro (kbd "C-c C-c"))
         (should (= 2 (length (funcall requests))))
         (should (string-match-p "PRIVATE-DRAFT"
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

(ert-deftest human-input/follow-up-edits-visible-draft ()
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (human-input-test--send "FIRST")
     (let ((marker (superchat--response-region--view-prompt)))
       (should marker)
       (goto-char marker) (insert "CURRENT")
       (cl-letf (((symbol-function 'read-string)
                  (lambda (_prompt &optional initial &rest _)
                    (should (equal initial "CURRENT")) "EDITED")))
         (execute-kbd-macro (kbd "C-c C-q")))
       (should (eq marker superchat--prompt-start))
       (should (equal "EDITED" (buffer-substring marker (point-max))))
       (should-not superchat--queued-follow-up)
       (should (= 1 (length (funcall requests))))
       (funcall (plist-get (car (funcall requests)) :final) "FINAL")
       (should (equal "EDITED" (buffer-substring marker (point-max))))))))

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
  (human-input-test--with-chat
   (lambda (_view _session requests)
     (human-input-test--send "FIRST")
     (should (= 1 (length (funcall requests))))
     (should-not (superchat-runtime-active-runs))
     (should (equal "REENTERED"
                    (buffer-substring superchat--prompt-start (point-max)))))
   (lambda (final)
     ;; Runtime is already public but the outer UI has not tracked its handle.
     (call-interactively #'superchat)
     (human-input-test--send "REENTERED")
     (funcall final "SYNC FINAL"))))

(ert-deftest human-input/follow-up-clear-during-read-does-not-retarget ()
  (human-input-test--with-chat
   (lambda (_view session _requests)
     (insert "OLD DRAFT")
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _)
                  (superchat--clear-chat-and-context)
                  "STALE EDIT")))
       (call-interactively #'superchat-queue-follow-up))
     (should-not (equal session superchat--session-id))
     (should (equal "" (buffer-substring superchat--prompt-start (point-max)))))))

(ert-deftest human-input/unrelated-same-session-run-does-not-block-view ()
  (human-input-test--with-chat
   (lambda (view session requests)
     (with-temp-buffer
       (let ((other (current-buffer)))
         (superchat-runtime-submit
          (list :buffer other :session-id session :prompt "UNRELATED"
                :tools 'none :deadline nil :transcript-owner "OTHER-CUSTOM")))
       (with-current-buffer view
         (should (= 1 (length (funcall requests))))
         (human-input-test--send "VIEW QUESTION")
         (should (= 2 (length (funcall requests))))
         (human-input-test--send "VISIBLE DRAFT")
         (should (= 2 (length (funcall requests))))
         (should (equal "VISIBLE DRAFT"
                        (buffer-substring superchat--prompt-start (point-max)))))))))

(ert-deftest human-input/native-tool-round-preserves-visible-draft ()
  (unless (and (featurep 'llm) (require 'llm-provider-utils nil t))
    (ert-skip "Requires isolated installed llm 0.31.1"))
  (dolist (async '(nil t))
    (let ((superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
          (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
          (superchat-llm-tools-list superchat-llm-tools-list)
          (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
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
  "A Run from before `/clear' must not render into the new View."
  (let ((buffer-name (generate-new-buffer-name " *superchat-clear-delivery*"))
        delivered)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (setq-local superchat-mode t
                      superchat-buffer-name buffer-name
                      superchat--session-id "old-session")
          (cl-letf (((symbol-function 'superchat--summarize-session-on-exit)
                     (lambda () nil))
                    ((symbol-function 'superchat--process-llm-result)
                     (lambda (answer &optional _tape-id)
                       (setq delivered answer))))
            (superchat--clear-chat-and-context)
            (let ((entry (list :buffer-name buffer-name
                               :session-id "old-session")))
              (superchat-runtime--deliver entry "old assistant" nil 'completed)))
          (should-not delivered))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

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
    (dolist (command '("workflow" "review" "code" "research"
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
      (should (equal (nreverse validations)
                     (list (list (current-buffer) "chat-exact-session")
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
    (should (string-match-p "\\*\\* Workflow FAILED at step 1/1" text))
    (should (string-match-p "\\[Cancelled by user\\]" text))
    (should (= 1 (test-cmd-dispatch--match-count "CHAT-FINAL" text)))
    (should-not (string-match-p "WF-FINAL" text))
    (should-not (string-match-p "WF-STREAM\\|CHAT-STREAM" text))
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
  (let ((file (make-temp-file "superchat-skill-file-" nil ".el"))
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
  (let ((file (make-temp-file "superchat-skill-metadata-" nil ".el"))
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
                   (setq handle (superchat-runtime-submit
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
            (superchat-llm-tools-enabled 'never)
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
         (setq handle (superchat-runtime-submit
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
         (superchat-runtime-submit (list :prompt "Q" :buffer view :session-id session
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
  "Invalid presentation claims and diagnostic faults cannot interrupt Authority."
  (dolist (kind '(session-only unknown mismatch missing-session wrong-run nil-owner status-error status-quit))
    (dolist (async '(nil t))
      (dolist (warning-fault '(nil error quit reentry))
        (cmd-clear-test--with-owned-view
         (lambda (view session)
           (let ((effects 0) (deliveries 0) (warnings 0) (counter (cons 0 nil))
                 context handle wrapped dones results)
             (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                        (lambda (&rest _)
                          (setq context (copy-sequence superchat--tool-execution-context)))))
               (setq handle (superchat-runtime-submit
                             (list :prompt "CONTEXT A" :buffer view :session-id session
                                   :tools 'none :deadline nil :transcript-owner "CTX-CUSTOM-A"
                                   :approval-function (lambda (_) t)))))
             (setq context (copy-sequence context))
             (pcase kind
               ('session-only (setq context (list :session-id session :attendance 'attended
                                                  :approval-function (lambda (_) t))))
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
               (should (superchat-runtime-status (plist-get handle :run-id)))))))))))

(ert-deftest output-context/approval-binder-keeps-real-custom-identity ()
  "An actual approval callback creates the native wrapper under full Runtime context."
  (unless (featurep 'llm) (ert-skip "Requires installed llm"))
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let ((effects 0) (approvals 0) context child)
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _) (setq context (copy-sequence superchat--tool-execution-context)))))
         (superchat-runtime-submit
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
         (setq handle (superchat-runtime-submit
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
       ;; Explicit compatibility evidence: nil does not claim Runtime isolation.
       (let ((effects 0) (warnings 0)
             (superchat-authority-approval-function (lambda (_) t)))
         (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) (cl-incf warnings))))
           (should (equal "LEGACY" (funcall (superchat--agent-wrap-function
                                             "read_buffer" (lambda () (cl-incf effects) "LEGACY")
                                             nil nil (cons 0 nil) view nil)))))
         (should (= 1 effects))
         (should (= 0 warnings))
         (should (string-match-p "LEGACY" (buffer-string))))))))

(ert-deftest output-context/diagnostic-containment-does-not-swallow-consumer-or-tool-quit ()
  (dolist (consumer '(nil t))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let ((effects 0) done caught
             (context (list :session-id session :attendance 'attended
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
  "Formatter failure preserves the live View while Runtime completes durably once."
  (dolist (fault '(error quit))
    (cmd-clear-test--with-owned-view
     (lambda (view session)
       (let (observer partial handle warnings)
         (superchat--response-region-consume-prompt)
         (superchat--prepare-for-response)
         (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _done stream &rest _)
                      (setq observer superchat--llm-runtime-observer partial stream))))
           (setq handle (superchat-runtime-submit
                         (list :prompt "O3 QUESTION" :buffer view :session-id session
                               :tools 'none :deadline nil :transcript-owner "O3-CUSTOM"))))
         (funcall partial '(:reasoning "VISIBLE REASONING"))
         (funcall partial "VISIBLE PARTIAL")
         (let ((before (buffer-string)) (before-point (point))
               (parts superchat--current-response-parts)
               (start superchat--assistant-response-start-marker))
           (cl-letf (((symbol-function 'superchat-render-markdown-to-org)
                      (lambda (_) (signal fault '("O3 FINAL FORMAT FAULT"))))
                     ((symbol-function 'display-warning)
                      (lambda (_type message &rest _) (push message warnings))))
             (funcall observer 'terminal '(:status completed :response "EXACT DURABLE FINAL")))
           (should (equal-including-properties before (buffer-string)))
           (should (= before-point (point)))
           (should (eq parts superchat--current-response-parts))
           (should (eq start superchat--assistant-response-start-marker))
           (should (= 1 (length warnings)))
           (should (string-match-p "delivery failed" (car warnings)))
           (funcall observer 'terminal '(:status completed :response "DUPLICATE MUST NOT APPEAR"))
           (should (equal-including-properties before (buffer-string))))
         (let* ((run-id (plist-get handle :run-id))
                (events (superchat-tape-replay-events session)))
           (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status)))
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
  "O4 stays supported without a prompt or an implicit generic seal."
  (cmd-clear-test--with-owned-view
   (lambda (_view session)
     (with-temp-buffer
       (let ((superchat-buffer-name (buffer-name)) observer partial)
         (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                    (lambda (_prompt _done stream &rest _)
                      (setq observer superchat--llm-runtime-observer partial stream))))
           (superchat-runtime-submit (list :prompt "BORROWED" :buffer (current-buffer)
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
         (should (string-match-p "BORROWED FINAL" (buffer-string))))))))

(ert-deftest output-p2/real-workflow-headers-prompt-and-interleaved-owner ()
  "Real Workflow/LLM/Closure keeps its headers and draft around another owner."
  (unless (and (featurep 'llm) (require 'llm-openai nil t))
    (ert-skip "P2 Workflow requires installed llm 0.31.1"))
  (cmd-clear-test--with-owned-view
   (lambda (_view session)
     (let* ((superchat-workflow-directory superchat-data-directory)
            (superchat-llm-backend (make-llm-openai :key "unused" :chat-model "p2"))
            (superchat-llm-tools-enabled 'never)
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
                 (superchat-runtime-submit
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
         (superchat-runtime-submit
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
       (let* ((handle (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                 (superchat-runtime-submit
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
           (setq a (superchat-runtime-submit
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
           (setq b (superchat-runtime-submit
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
                 (let ((text (buffer-string)))
                   (should (< (string-match "A-CANCELLED" text)
                              (string-match "Tool call: read_buffer" text)))
                   (should (< (string-match "Tool result: read_buffer" text)
                              (string-match "B-PART1" text)))))
             ;; This is intentionally outside any Runtime dynamic binding.
             (superchat--response-region-insert-transcript "INDEPENDENT-NOTE" :independent-turn)
             (should (equal owner-b superchat--response-region-current-owner))
             (funcall (plist-get capture-b :stream) "B-PART2")
             (funcall (plist-get capture-b :observer)
                      'terminal '(:status completed :response "B-FINAL"))
             (should (eq 'completed (plist-get (superchat-runtime-status (plist-get b :run-id)) :status)))))
           (superchat-runtime-submit
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
                       (superchat-runtime-submit
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
           (push (superchat-runtime-submit
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
           (should (equal text (buffer-string)))
           (should (equal old-session superchat--session-id))
           (should (equal old-session (superchat-editor-workspace-owner-session view)))
           (let ((statuses (mapcar
                            (lambda (handle)
                              (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status))
                            handles)))
             (should (= 1 (cl-count 'cancelled statuses)))
             (should (= 1 (cl-count 'running-model statuses))))))))))

(ert-deftest cmd/clear-owned-view-delayed-admitted-result-keeps-owner ()
  "An already admitted async result arrives in A's block during B streaming."
  (cmd-clear-test--with-owned-view
   (lambda (view session)
     (let (captures finish wrapped a b (effects 0) (deliveries 0))
       (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                  (lambda (_prompt _complete stream &rest _)
                    (push (list :stream stream :observer superchat--llm-runtime-observer
                                :context (copy-sequence superchat--tool-execution-context)) captures))))
         (setq a (superchat-runtime-submit
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
         (setq b (superchat-runtime-submit
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
               (superchat-runtime-submit
                (list :prompt "OLD-QUESTION" :session-id old-session
                      :buffer view :tools 'none :deadline nil
                      :delivery-function
                      (lambda (_value _status _tape-id)
                        (cl-incf deliveries)
                        (unless child
                          (setq child
                                (superchat-runtime-submit
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
