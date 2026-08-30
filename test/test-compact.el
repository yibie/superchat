;;; test-compact.el --- Tests for superchat-compact -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for session compaction and anchor-based prompt assembly.

;;; Code:

(require 'ert)
(require 'superchat-compact)
(require 'superchat-db)

(require 'superchat-tape-view)

(defun test-compact--with-temp-db (body)
  "Run BODY with a fresh temporary Tape database."
  (let* ((tmp-dir (make-temp-file "superchat-compact-integration-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (funcall body))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest test-compact-collect-messages ()
  "Collect recent conversation messages for compaction."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "collect-compact")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (hi-id (superchat-db-tape-append session-id "assistant" "Hi there"))
            (question-id
             (superchat-db-tape-append session-id "user" "What is 2+2?"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "What is 2+2?" :id ,question-id)
               (:role assistant :content "Hi there" :id ,hi-id)
               (:role user :content "Hello" :id ,hello-id)))
            ;; The legacy count is deliberately ignored.  At a 50-character
            ;; budget the newest row is keep-recent and the older two are the
            ;; only compaction range.
            (superchat-compact-message-count 1)
            (messages (superchat-compact--collect-messages 50)))
       (should (= (length messages) 2))
       (should (string= (plist-get (car messages) :content) "Hello"))
       (should (string= (plist-get (cadr messages) :content) "Hi there"))))))

(ert-deftest test-compact-build-prompt-includes-messages ()
  "Compaction prompt should include the conversation text."
  (let ((messages '((:role user :content "Hello")))
        (superchat-compact-prompt "SUMMARY PROMPT"))
    (let ((prompt (superchat-compact--build-compaction-prompt messages)))
      (should (string-match-p "SUMMARY PROMPT" prompt))
      (should (string-match-p "User: Hello" prompt))
      (should (string-match-p
               (regexp-quote superchat-compact--completion-marker)
               prompt)))))

(ert-deftest test-compact-session-writes-anchor-and-resets-history ()
  "`superchat-compact-session' should summarize, write anchor, and reset history."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-session")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (hi-id (superchat-db-tape-append session-id "assistant" "Hi"))
            (question-id
             (superchat-db-tape-append session-id "user" "What is Emacs?"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "What is Emacs?" :id ,question-id)
               (:role assistant :content "Hi" :id ,hi-id)
               (:role user :content "Hello" :id ,hello-id)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 60)
            (superchat-buffer-name "*superchat-compact-test*")
            (anchors '())
            captured-request)
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (setq captured-request request)
                      (funcall (plist-get request :delivery-function)
                               (concat "Summary of the conversation.\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "run-compact" :turn-id "turn-compact"
                        :session-id "test-compact-session" :status accepted)))
                   ((symbol-function 'superchat-db-tape-append)
                    (lambda (_session-id kind content &rest _args)
                      (when (string= kind "anchor")
                        (push content anchors))
                      42)))
           (let ((handle (superchat-compact-session)))
             (should (equal "run-compact" (plist-get handle :run-id)))
             (should-not superchat-compact--in-flight)
             (should (equal (plist-get captured-request :capability)
                            "compaction"))
             (should (equal (plist-get captured-request :actor-id)
                            "compactor"))
             (let ((child-session
                    (plist-get captured-request :session-id))
                   (host (plist-get captured-request :buffer)))
               (should (string-prefix-p
                        (concat session-id "/compaction-") child-session))
               (should-not (equal session-id child-session))
               (should-not (buffer-live-p host))
               (should (eq 'closed
                           (superchat-editor-workspace-state child-session)))
               (should-not (plist-member captured-request
                                         :ephemeral-buffer)))
             (should-not (plist-get captured-request :record-conversation))
             (should (= 15 (plist-get captured-request :max-tokens)))
             (should (= (length anchors) 1))
             (should (string= (car anchors) "Summary of the conversation."))
             (should (= (length superchat--conversation-history) 2))
             (should (equal "What is Emacs?"
                            (plist-get (car superchat--conversation-history)
                                       :content)))
             (should (plist-get (cadr superchat--conversation-history) :anchor))
             (should (string-match-p "Summary of the conversation"
                                     (plist-get (cadr superchat--conversation-history)
                                                :content))))))))))

(ert-deftest test-compact-session-persists-typed-durable-provenance ()
  "Compact writes the local typed Anchor schema from durable raw rows."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "typed-compact-writer")
            (older-id
             (superchat-db-tape-append session-id "user" "older question"))
            (user-id (superchat-db-tape-append session-id "user" "question"))
            (assistant-id
             (superchat-db-tape-append session-id "assistant" "answer"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "answer" :id ,assistant-id)
               (:role user :content "question" :id ,user-id)
               (:role user :content "older question" :id ,older-id)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 50)
            (superchat-buffer-name "*typed-compact-writer*"))
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (funcall (plist-get request :delivery-function)
                               (concat "typed summary\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "typed-compact-run" :status accepted)))
                   ((symbol-function 'superchat--insert-system-message)
                    (lambda (_content) nil)))
           (superchat-compact-session))
         (let* ((row (car (last (superchat-db-tape-replay session-id))))
                (json-object-type 'alist)
                (json-array-type 'vector)
                (json-key-type 'symbol)
                (meta (json-read-from-string (nth 4 row))))
           (should (equal "anchor" (nth 2 row)))
           (should (equal "compact" (cdr (assq 'anchor_type meta))))
           (should (= 1 (cdr (assq 'anchor_version meta))))
           (should (equal (vector older-id user-id)
                          (cdr (assq 'source_ids meta))))))))))

(ert-deftest test-compact-repeat-flattens-previous-raw-provenance ()
  "A later Compact Anchor retains raw IDs represented by the prior one."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "repeat-compact")
            (first-user (superchat-db-tape-append session-id "user" "first"))
            (first-answer
             (superchat-db-tape-append session-id "assistant" "answer"))
            (first-anchor
             (superchat-db-tape-append
              session-id "anchor" "first summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-user ,first-answer]))))
            (new-assistant
             (superchat-db-tape-append session-id "assistant" "second"))
            (new-user (superchat-db-tape-append session-id "user" "latest"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "latest" :id ,new-user)
               (:role assistant :content "second" :id ,new-assistant)
               (:role system :content "[Session anchor]\n\nfirst summary"
                :anchor t)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 50)
            (superchat-buffer-name "*repeat-compact*")
            captured-prompt)
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (setq captured-prompt (plist-get request :prompt))
                      (funcall (plist-get request :delivery-function)
                               (concat "second summary\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "repeat-compact-run" :status accepted)))
                   ((symbol-function 'superchat--insert-system-message)
                    (lambda (_content) nil)))
           (superchat-compact-session))
         (let* ((row (car (last (superchat-db-tape-replay session-id))))
                (json-object-type 'alist)
                (json-array-type 'vector)
                (json-key-type 'symbol)
                (meta (json-read-from-string (nth 4 row))))
           (should (= first-anchor 3))
           (should (= new-assistant 4))
           (should (= new-user 5))
           (should (string-match-p "<previous-summary>" captured-prompt))
           (should (string-match-p "first summary" captured-prompt))
           (should (string-match-p "second" captured-prompt))
           (should (equal [1 2 4] (cdr (assq 'source_ids meta))))))))))

(ert-deftest test-compact-session-preserves-messages-added-in-flight ()
  "Messages recorded while compaction runs must survive in front of the anchor."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-flight")
            (question-id
             (superchat-db-tape-append session-id "user" "old question"))
            (answer-id
             (superchat-db-tape-append session-id "assistant" "old answer"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "old answer" :id ,answer-id)
               (:role user :content "old question" :id ,question-id)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 50)
            (superchat-buffer-name "*superchat-compact-flight*")
            pending-delivery)
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (setq pending-delivery (plist-get request :delivery-function))
                      '(:run-id "run-flight" :status accepted)))
                   ((symbol-function 'superchat-db-tape-append)
                    (lambda (&rest _args) 42)))
           (should (superchat-compact-session))
           (should superchat-compact--in-flight)
           ;; A second request while one is pending must refuse to start.
           (should-not (superchat-compact-session))
           ;; A new message arrives while the compaction Run is in flight.
           (push '(:role user :content "during-flight")
                 superchat--conversation-history)
           (funcall pending-delivery
                    (concat "Summary.\n" superchat-compact--completion-marker)
                    'completed nil)
           (should-not superchat-compact--in-flight)
           (should (= 3 (length superchat--conversation-history)))
           (should (string= "during-flight"
                            (plist-get (car superchat--conversation-history)
                                       :content)))
           (should (string= "old answer"
                            (plist-get (cadr superchat--conversation-history)
                                       :content)))
           (should (plist-get (caddr superchat--conversation-history)
                              :anchor))))))))

(ert-deftest test-compact-session-failed-run-leaves-history-alone ()
  "A failed compaction Run must clear the pending flag and change nothing."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-failed")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (answer-id
             (superchat-db-tape-append session-id "assistant" "Answer"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "Answer" :id ,answer-id)
               (:role user :content "Hello" :id ,hello-id)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 50)
            (superchat-buffer-name "*superchat-compact-failed*")
            (anchors '()))
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (funcall (plist-get request :delivery-function)
                               "[API error]" 'failed nil)
                      '(:run-id "run-failed" :status accepted)))
                   ((symbol-function 'superchat-db-tape-append)
                    (lambda (_session-id kind content &rest _args)
                      (when (string= kind "anchor") (push content anchors))
                      42)))
           (should (superchat-compact-session))
           (should-not superchat-compact--in-flight)
           (should (null anchors))
           (should (= 2 (length superchat--conversation-history)))
           (should-not (plist-get (car superchat--conversation-history)
                                  :anchor))))))))

(ert-deftest test-compact-truncated-summary-leaves-history-alone ()
  "A completed response without the marker must not write an Anchor."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-truncated")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (answer-id
             (superchat-db-tape-append session-id "assistant" "Answer"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "Answer" :id ,answer-id)
               (:role user :content "Hello" :id ,hello-id)))
            (superchat-compact-message-count 10)
            (superchat-context-max-chars 50)
            (superchat-buffer-name "*superchat-compact-truncated*")
            (before-history (copy-tree superchat--conversation-history))
            (anchors '()))
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (funcall (plist-get request :delivery-function)
                               "truncated summary" 'completed nil)
                      '(:run-id "run-truncated" :status accepted)))
                   ((symbol-function 'superchat-db-tape-append)
                    (lambda (_session-id kind content &rest _args)
                      (when (string= kind "anchor")
                        (push content anchors))
                      42)))
           (should (superchat-compact-session)))
         (should-not superchat-compact--in-flight)
         (should-not anchors)
         (should (equal before-history superchat--conversation-history)))))))

(ert-deftest test-compact-summary-over-limit-leaves-history-alone ()
  "A marked summary over the derived limit must fail without an Anchor."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-over-limit")
            (old-id (superchat-db-tape-append session-id "user" "old"))
            (new-id
             (superchat-db-tape-append session-id "assistant" "new"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "new" :id ,new-id)
               (:role user :content "old" :id ,old-id)))
            (superchat-context-max-chars 20)
            (superchat-buffer-name "*superchat-compact-over-limit*")
            failure)
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (funcall (plist-get request :delivery-function)
                               (concat "12345678901\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "run-over-limit" :status accepted)))
                   ((symbol-function 'superchat--insert-system-message)
                    (lambda (_content) nil)))
           (should
            (superchat-compact-session
             (lambda (success value)
               (setq failure (list success value))))))
         (should (equal
                  '(nil
                    "[Compaction summary exceeded 10 characters; no Anchor written]")
                  failure))
         (should-not (superchat-view-compaction-anchor-latest session-id))
         (should (= 2 (length superchat--conversation-history))))))))

(ert-deftest compact/keep-recent-boundary-preserves-raw-context ()
  "The newest thirty percent stays raw and out of Anchor source_ids."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "keep-recent-boundary")
            (first-id
             (superchat-db-tape-append session-id "user" "old first"))
            (second-id
             (superchat-db-tape-append session-id "assistant" "old second"))
            (third-id
             (superchat-db-tape-append session-id "user" "old third"))
            (fourth-id
             (superchat-db-tape-append session-id "assistant"
                                       "recent answer"))
            (fifth-id
             (superchat-db-tape-append session-id "user"
                                       "recent question"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "recent question" :id ,fifth-id)
               (:role assistant :content "recent answer" :id ,fourth-id)
               (:role user :content "old third" :id ,third-id)
               (:role assistant :content "old second" :id ,second-id)
               (:role user :content "old first" :id ,first-id)))
            (superchat-compact-message-count 1)
            (superchat-context-max-chars 200)
            (superchat-buffer-name "*superchat-keep-recent*")
            request)
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (value)
                      (setq request value)
                      (funcall (plist-get value :delivery-function)
                               (concat "checkpoint\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "keep-recent-run" :status accepted)))
                   ((symbol-function 'superchat--insert-system-message)
                    (lambda (_content) nil)))
           (should (superchat-compact-session)))
         (let* ((anchor (superchat-view-compaction-anchor-latest session-id))
                (source-ids
                 (superchat-view-compaction-anchor-source-ids anchor))
                (view (superchat-view-assemble
                       session-id :max-entries nil :max-chars 200))
                (items (plist-get view :items))
                (raw-items
                 (cl-remove-if-not
                  (lambda (item) (eq 'raw-fact (plist-get item :class)))
                  (append items nil))))
           (should (= 50 (plist-get request :max-tokens)))
           (should (equal (vector first-id second-id third-id) source-ids))
           (should (equal (list fourth-id fifth-id)
                          (mapcar (lambda (item) (plist-get item :storage-id))
                                  raw-items)))
           (should (string-match-p "recent answer"
                                   (plist-get view :prompt)))
           (should (string-match-p "recent question"
                                   (plist-get view :prompt)))))))))

(ert-deftest compact/no-gap-across-legacy-count-boundary ()
  "Compaction plus keep-recent covers every row even beyond one hundred."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "compact-no-gap")
            (ids (mapcar
                  (lambda (index)
                    (superchat-db-tape-append
                     session-id "user" (format "fact-%03d" index)))
                  (number-sequence 1 105)))
            (superchat--session-id session-id)
            (superchat--conversation-history
             (mapcar (lambda (id)
                       (list :role 'user
                             :content (format "fact-%03d" id)
                             :id id))
                     (reverse ids)))
            ;; A non-nil legacy value must not create a third truncation.
            (superchat-compact-message-count 1)
            (superchat-context-max-chars 1000)
            (superchat-buffer-name "*superchat-no-gap*")
            (anchor-request nil))
       (with-temp-buffer
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (setq anchor-request request)
                      (funcall (plist-get request :delivery-function)
                               (concat "checkpoint\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "no-gap-run" :status accepted)))
                   ((symbol-function 'superchat--insert-system-message)
                    (lambda (_content) nil)))
           (should (superchat-compact-session)))
         (let* ((anchor (superchat-view-compaction-anchor-latest session-id))
                (source-ids
                 (append
                  (superchat-view-compaction-anchor-source-ids anchor)
                  nil))
                (view (superchat-view-assemble
                       session-id :max-entries nil :max-chars 1000))
                (raw-ids
                 (mapcar
                  (lambda (item) (plist-get item :storage-id))
                  (cl-remove-if-not
                   (lambda (item) (eq 'raw-fact (plist-get item :class)))
                   (append (plist-get view :items) nil))))
                (covered-ids (sort (append source-ids raw-ids) #'<)))
           (should (> (length source-ids) 0))
           (should (= 1 (car source-ids)))
           (should (equal (number-sequence 1 105) covered-ids))
           (should (eq 'satisfied (plist-get view :status)))
           (should (string-match-p "fact-105" (plist-get view :prompt)))
           (should (plist-get anchor-request :max-tokens))))))))

(ert-deftest test-compact-log-opens-latest-human-log ()
  "Successful compaction logs are sequenced and /compact-log opens the latest."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "compact-log-session")
            (first (superchat-compact--write-log session-id "first summary"))
            (second (superchat-compact--write-log session-id "second summary"))
            (opened nil)
            (superchat--session-id session-id))
       (should (string-suffix-p "compact-log-session-1.md" first))
       (should (string-suffix-p "compact-log-session-2.md" second))
       (should (equal "second summary\n"
                      (with-temp-buffer
                        (insert-file-contents second)
                        (buffer-string))))
       (cl-letf (((symbol-function 'find-file)
                  (lambda (file) (setq opened file))))
         (should (equal '(:type :noop)
                        (superchat--cmd-compact-log nil nil nil nil nil))))
       (should (equal second opened))
       (setq opened nil)
       (cl-letf (((symbol-function 'find-file)
                  (lambda (file) (setq opened file))))
         (should (equal '(:type :noop)
                        (superchat--cmd-compact-log nil "1" nil nil nil))))
       (should (equal first opened))
       (let ((missing (superchat--cmd-compact-log nil "3" nil nil nil)))
         (should (equal :echo (plist-get missing :type)))
         (should (string-match-p "No compaction log 3 found"
                                 (plist-get missing :content))))
       (dolist (argument '("0" "abc"))
         (let ((invalid (superchat--cmd-compact-log nil argument nil nil nil)))
           (should (equal :echo (plist-get invalid :type)))
           (should (equal "Usage: /compact-log [positive log number]"
                          (plist-get invalid :content)))))))))

(ert-deftest test-auto-handoff-context-budget-starts-compaction ()
  "An over-budget request compacts and resumes the same Run."
  (with-temp-buffer
    (setq-local superchat--session-id "budget-session")
    (let ((superchat-handoff-auto-enabled t)
          (superchat-compact--in-flight nil)
          compact-called target-model resumed failed)
      (cl-letf (((symbol-function 'superchat-compact-session)
                 (lambda (on-finish model)
                   (setq compact-called t
                         target-model model)
                   (funcall on-finish t "summary")
                   'compaction-run))
                ((symbol-function 'superchat-compact--context-envelope)
                 (lambda (_session-id)
                   '(:prompt "rebuilt view"
                     :items [(:class raw-fact :content "fact")]
                     :disclosure (:covered [] :represented []))))
                ((symbol-function 'superchat--insert-system-message)
                 (lambda (_content) nil)))
        (run-hook-with-args
         'superchat-llm-context-budget-exceeded-functions
         (list :context-used 9000 :output-reserve 2048
               :context-limit 10000 :model "model-x"
               :resume-function (lambda (context) (setq resumed context))
               :fail-function (lambda (message) (setq failed message))))
        (should compact-called)
        (should (equal "model-x" target-model))
        (should (equal "rebuilt view" resumed))
        (should-not failed)))))

(ert-deftest test-auto-handoff-rejects-header-only-recovery-view ()
  "A header-only recovery View fails visibly instead of resuming blindly."
  (with-temp-buffer
    (setq-local superchat--session-id "header-only-recovery")
    (let ((superchat-handoff-auto-enabled t)
          (superchat-compact--in-flight nil)
          resumed failed displayed)
      (cl-letf (((symbol-function 'superchat-compact-session)
                 (lambda (on-finish _model)
                   (funcall on-finish t "summary")
                   'compaction-run))
                ((symbol-function 'superchat-compact--context-envelope)
                 (lambda (_session-id)
                   '(:prompt "[Context v0 | raw none | represented none]"
                     :items []
                     :disclosure (:covered [] :represented []))))
                ((symbol-function 'superchat--insert-system-message)
                 (lambda (content) (setq displayed content))))
        (run-hook-with-args
         'superchat-llm-context-budget-exceeded-functions
         (list :model "model-x"
               :resume-function (lambda (context) (setq resumed context))
               :fail-function (lambda (message) (setq failed message))))
        (should-not resumed)
        (should (equal "[Compaction produced no usable Context View]" failed))
        (should (equal failed displayed))))))

(ert-deftest test-auto-handoff-failure-survives-unbounded-older-run ()
  "A real handoff failure notice survives an older unbounded Run finalize."
  (require 'superchat)
  (let ((name (generate-new-buffer-name " *superchat-third-branch-compact*")))
    (unwind-protect
        (with-current-buffer (get-buffer-create name)
          (org-mode)
          (let ((superchat-buffer-name name)
                (superchat-handoff-auto-enabled t)
                (superchat-compact--in-flight nil)
                callback
                resumed
                failed)
            (let ((inhibit-read-only t))
              (erase-buffer))
            (setq-local superchat--session-id "third-branch-compact"
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
                        superchat--ttft-elapsed nil)
            (insert "* User: older model run\n")
            (superchat--prepare-for-response)
            (superchat--stream-llm-result "OLDER-EARLY")
            (should-not superchat--assistant-response-end-marker)
            (should-not superchat--prompt-start)
            (cl-letf (((symbol-function 'superchat-compact-session)
                       (lambda (on-finish &optional _model)
                         (setq callback on-finish)
                         '(:run-id "probe-compaction")))
                      ((symbol-function 'superchat-compact--context-envelope)
                       (lambda (_session-id)
                         '(:prompt "" :items []
                           :disclosure (:covered [] :represented [])))))
              (should
               (superchat-handoff--context-budget-exceeded
                (list :resume-function
                      (lambda (context) (setq resumed context))
                      :fail-function
                      (lambda (failure) (setq failed failure)))))
              (should (functionp callback))
              (funcall callback t "summary"))
            (should-not resumed)
            (should (equal failed
                           "[Compaction produced no usable Context View]"))
            (should (markerp superchat--assistant-response-end-marker))
            (superchat--stream-llm-result "OLDER-LATE")
            (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
              (superchat--process-llm-result "OLDER-FINAL"))
            (let* ((text (buffer-string))
                   (final (string-match "OLDER-FINAL" text))
                   (warning (string-match
                             "Compaction produced no usable Context View"
                             text))
                   (prompt (string-match
                            "\\* User: \\(?:\\n\\|\\'\\)" text)))
              (should (= 1 (test-compact--match-count
                            "Compaction produced no usable Context View"
                            text)))
              (should-not (string-match-p "OLDER-EARLY\\|OLDER-LATE" text))
              (should (< final warning))
              (should (< warning prompt)))))
      (when (get-buffer name)
        (kill-buffer name)))))

(ert-deftest test-prompt-hook-excludes-volatile-anchor ()
  "The prompt hook should use Tape facts, not the volatile Compact summary."
  (require 'superchat-prompt-hooks)
  (let* ((tmp-dir (make-temp-file "superchat-compact-prompt-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (superchat-db-tape-append
           "test-anchor-session" "user" "TAPE FACT")
          (let ((turn (superchat-turn-new "test" "test-anchor-session")))
            (superchat-prompt-hook--conversation-history turn)
            (should
             (string-match-p "User: TAPE FACT"
                             (superchat-turn-prompt turn)))))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest compact/cancelled-source-invalidates-summary-and-offers-recompact ()
  "A cancelled Anchor source falls back safely and offers re-compaction."
  (require 'superchat-prompt-hooks)
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "cancelled-anchor-recovery")
            (safe-id
             (superchat-tape-append-event
              (list :type "intent.received"
                    :session-id session-id
                    :turn-id "turn-safe"
                    :run-id "run-safe"
                    :summary "safe source")))
            (old-anchor-id
             (superchat-tape-append-compaction-anchor
              session-id "old safe summary" (vector safe-id)))
            (cancelled-id
             (superchat-tape-append-event
              (list :type "intent.received"
                    :session-id session-id
                    :turn-id "turn-cancelled"
                    :run-id "run-cancelled"
                    :summary "cancelled source")))
            (anchor-id
             (superchat-tape-append-compaction-anchor
              session-id "mixed summary" (vector safe-id cancelled-id)))
            (before (superchat-view-assemble session-id)))
       (superchat-tape-append-event
        (list :type "run.cancelled"
              :session-id session-id
              :turn-id "turn-cancelled"
              :run-id "run-cancelled"
              :summary "run.cancelled: run-cancelled"
              :payload '(:status cancelled)))
       (let* ((after (superchat-view-assemble session-id))
              (before-items (append (plist-get before :items) nil))
              (after-items (append (plist-get after :items) nil))
              (omitted (append (plist-get (plist-get after :disclosure)
                                          :omitted)
                               nil))
              (turn (superchat-turn-new "continue" session-id))
              messages)
         (should
          (equal '(model-interpretation)
                 (mapcar (lambda (item) (plist-get item :class))
                         before-items)))
         (should-not
          (cl-some (lambda (item)
                     (eq 'model-interpretation (plist-get item :class)))
                   after-items))
         (should
          (cl-some (lambda (item)
                     (and (= safe-id (plist-get item :storage-id))
                          (equal "safe source" (plist-get item :content))))
                   after-items))
         (should-not
          (cl-some (lambda (item)
                     (= cancelled-id (plist-get item :storage-id)))
                   after-items))
         (should (equal []
                        (plist-get (plist-get after :disclosure)
                                   :represented)))
         (should
          (cl-some (lambda (entry)
                     (let ((range (plist-get entry :range)))
                       (and (eq 'not-prompt-material (plist-get entry :reason))
                            (= (aref range 0) old-anchor-id)
                            (= (aref range 1) old-anchor-id))))
                   omitted))
         (should
          (cl-some (lambda (entry)
                     (let ((range (plist-get entry :range)))
                       (and (eq 'cancelled (plist-get entry :reason))
                            (<= (aref range 0) anchor-id)
                            (<= anchor-id (aref range 1)))))
                   omitted))
         (should
          (cl-some (lambda (entry)
                     (let ((range (plist-get entry :range)))
                       (and (eq 'cancelled (plist-get entry :reason))
                            (<= (aref range 0) cancelled-id)
                            (<= cancelled-id (aref range 1)))))
                   omitted))
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (push (apply #'format format-string args) messages))))
           (let ((superchat-context-message-count nil)
                 (superchat-context-max-chars 4000)
                 (superchat--expanded-compaction-anchor-ids nil))
             (superchat-prompt-hook--conversation-history turn)))
         (should
          (cl-some (lambda (message)
                     (string-match-p "可 /compact 重新压缩" message))
                   messages))
         (with-temp-buffer
           (let ((superchat--session-id session-id)
                 (superchat--conversation-history
                  (list (list :role 'system
                              :content "mixed summary"
                              :anchor t
                              :compaction-anchor t
                              :id anchor-id)))
                 (superchat--expanded-compaction-anchor-ids nil))
             (should (= 2 (superchat-compact--expand-anchor)))
             (should
              (equal '("cancelled source" "safe source")
                     (mapcar (lambda (entry) (plist-get entry :content))
                             superchat--conversation-history))))))))))

(ert-deftest test-expand-anchor-restores-source-messages ()
  "`/expand' should restore the source entries behind the latest anchor."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-expand-session")
            (hello-id (superchat-tape-append-material
                       session-id "user" "Hello"))
            (hi-id (superchat-tape-append-material
                    session-id "assistant" "Hi"))
            (question-id (superchat-tape-append-material
                          session-id "user" "What is Emacs?"))
            (anchor-id
             (superchat-tape-append-compaction-anchor
              session-id "Summary of Emacs discussion."
              (vector hello-id hi-id question-id))))
       (with-temp-buffer
         (let ((superchat--session-id session-id)
               (superchat--conversation-history
                (list (list :role 'system
                            :content "[Session anchor]\n\nSummary of Emacs discussion."
                            :anchor t :id anchor-id))))
           (let ((count (superchat-compact--expand-anchor)))
             (should (= count 3))
             (should (= (length superchat--conversation-history) 3))
             (should (string= "What is Emacs?"
                              (plist-get (car superchat--conversation-history)
                                         :content))))))))))

(ert-deftest test-expand-anchor-marks-reconstructed-sources-as-expanded ()
  "Restart reconstruction handles multiple Anchors, a missing source, and repeats."
  (require 'superchat)
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "restart-expand")
            (user-id-1 (superchat-tape-append-material
                        session-id "user" "hello-1"))
            (assistant-id-1 (superchat-tape-append-material
                            session-id "assistant" "world-1"))
            (anchor-id-1
             (superchat-tape-append-compaction-anchor
              session-id "summary-1" (vector user-id-1 assistant-id-1)))
            (user-id-2 (superchat-tape-append-material
                        session-id "user" "hello-2"))
            (assistant-id-2 (superchat-tape-append-material
                            session-id "assistant" "world-2"))
            (anchor-id-2
             (superchat-tape-append-compaction-anchor
              session-id "summary-2"
              (vector user-id-1 assistant-id-1 user-id-2 assistant-id-2)))
            (buffer (superchat--render-session-view session-id)))
       (unwind-protect
           (with-current-buffer buffer
             (let* ((before-ids
                     (mapcar (lambda (message) (plist-get message :id))
                             superchat--conversation-history))
                    (_drop
                     (setq superchat--conversation-history
                           (cl-remove-if
                            (lambda (message)
                              (= (or (plist-get message :id) -1) user-id-1))
                            superchat--conversation-history)))
                    (first (superchat--cmd-expand nil nil nil nil nil))
                    (after-first (copy-tree superchat--conversation-history t))
                    (second (superchat--cmd-expand nil nil nil nil nil))
                    (envelope
                     (superchat-view-assemble
                      session-id :max-chars 4000
                      :expanded-anchor-ids
                      (vconcat superchat--expanded-compaction-anchor-ids)))
                    (items (append (plist-get envelope :items) nil)))
               (should (member user-id-1 before-ids))
               (should (equal :echo (plist-get first :type)))
               (should (string-match-p "Expanded anchor into 1 entries\."
                                       (plist-get first :content)))
               (should (member user-id-1
                               (mapcar (lambda (message)
                                         (plist-get message :id))
                                       superchat--conversation-history)))
               (should (equal (list anchor-id-2)
                              superchat--expanded-compaction-anchor-ids))
               (should (equal :echo (plist-get second :type)))
               (should
                (equal "Anchor already expanded; Context View uses raw entries."
                       (plist-get second :content)))
               (should (equal after-first superchat--conversation-history))
               (should-not
                (cl-some (lambda (item)
                           (eq 'model-interpretation (plist-get item :class)))
                         items))
               (should (= 0 (length (plist-get envelope :represented))))))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest test-compact-success-notice-survives-prompt-refresh ()
  "Async compaction completion preserves a draft typed after the echo refresh."
  (require 'superchat)
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "compact-notice")
            (source-id (superchat-tape-append-material
                        session-id "user" "source"))
            (old-id (superchat-tape-append-material
                     session-id "assistant" "old answer"))
            (submitted-request nil)
            (superchat-context-max-chars 20))
       (make-directory (expand-file-name "command" superchat-data-directory) t)
       (with-temp-buffer
         (let ((superchat-buffer-name (buffer-name)))
           (setq-local superchat--session-id session-id)
           (setq-local superchat--conversation-history
                       (list (list :role 'assistant :content "old answer" :id old-id)
                             (list :role 'user :content "source" :id source-id)))
           (superchat--insert-prompt)
           (superchat--prepare-for-response)
           (cl-letf (((symbol-function 'superchat-runtime-submit)
                      (lambda (request)
                        (setq submitted-request request)
                        '(:run-id "compact-notice-run" :status accepted)))
                     ((symbol-function 'message) #'ignore))
             (let ((result (superchat--handle-command
                            "compact" "" "/compact" nil nil)))
               (should (equal :echo (plist-get result :type)))
               (should (string-match-p "started"
                                       (plist-get result :content)))
               ;; The command result is refreshed before the background
               ;; delivery callback is allowed to run.
               (superchat--dispatch-result result nil nil)))
           (should submitted-request)
           (should-not superchat--response-start-marker)
           (goto-char (point-max))
           (insert "UNSENT-DRAFT")
           (funcall (plist-get submitted-request :delivery-function)
                    (concat "summary\n" superchat-compact--completion-marker)
                    'completed nil)
           (let ((anchor (superchat-view-compaction-anchor-latest session-id))
                 (text (buffer-string)))
             (should (string-match-p "UNSENT-DRAFT" text))
             (should (string-match-p
                      (regexp-quote
                       (format "Anchor: %s" (nth 0 anchor)))
                      text))
             (should (string-match-p (regexp-quote "可 /expand") text)))))))))

(ert-deftest test-compact-success-notice-survives-streaming-run-finalize ()
  "A later streaming Run's final rewrite preserves the compact notice."
  (require 'superchat)
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "compact-streaming-notice")
            (source-id (superchat-tape-append-material
                        session-id "user" "source"))
            (old-id (superchat-tape-append-material
                     session-id "assistant" "old answer"))
            (compact-request nil)
            (run-b-request nil)
            (superchat-lang "English")
            (superchat-context-message-count 0)
            (superchat-context-max-chars 20)
            (superchat-memory-auto-recall-min-length 999999)
            (superchat-system-prompt-functions nil)
            (superchat-build-prompt-functions nil)
            (superchat-pre-topic-functions nil)
            (superchat-post-topic-functions nil)
            (superchat-post-turn-functions nil))
       (make-directory (expand-file-name "command" superchat-data-directory) t)
       (with-temp-buffer
         (let ((superchat-buffer-name (buffer-name)))
           (setq-local superchat--session-id session-id)
           (setq-local superchat--conversation-history
                       (list (list :role 'assistant :content "old answer" :id old-id)
                             (list :role 'user :content "source" :id source-id)))
           (setq-local superchat--current-command nil)
           (setq-local superchat--current-context-files nil)
           (setq-local superchat--pending-recalled-memories nil)
           (superchat--insert-prompt)
           ;; Run A is the /compact command.  Let the real dispatcher consume
           ;; its echo before the asynchronous completion is delivered.
           (superchat--prepare-for-response)
           (cl-letf (((symbol-function 'superchat-runtime-submit)
                      (lambda (request)
                        (if (equal "compaction" (plist-get request :capability))
                            (progn
                              (setq compact-request request)
                              '(:run-id "compact-streaming-run-a" :status accepted))
                          (setq run-b-request request)
                          '(:run-id "compact-streaming-run-b" :status accepted))))
                     ((symbol-function 'message) #'ignore))
             (let ((result (superchat--handle-command
                            "compact" "" "/compact" nil nil)))
               (should (eq :echo (plist-get result :type)))
               (superchat--dispatch-result result nil nil))
           (should compact-request)
           (should-not superchat--response-start-marker)

           ;; Run B starts after the echo refresh through the real send and
           ;; dispatch path, then streams a real chunk before Run A's
           ;; completion callback runs.
           (goto-char (point-max))
           (insert "RUN-B-INPUT")
           (superchat-send-input)
           (should run-b-request)
           (should (equal "RUN-B-INPUT" (plist-get run-b-request :input)))
           (superchat--update-status "RUN-B-THINKING")
           (superchat--stream-llm-result "RUN-B-PARTIAL")
           (setq superchat--ttft-elapsed 0.25)

           ;; Run A completes while Run B still owns an assistant region.
           (funcall (plist-get compact-request :delivery-function)
                    (concat "summary\n" superchat-compact--completion-marker)
                    'completed nil)
           (should (string-match-p "Session compacted" (buffer-string)))

           ;; Run B's normal final rewrite must keep its own body and TTFT,
           ;; as well as the already-inserted persistent notice.
           (superchat--stream-llm-result "RUN-B-LATE")
           (superchat--process-llm-result "RUN-B-FINAL")
           (let ((text (buffer-string)))
             (should (string-match-p "RUN-B-INPUT" text))
             (should (string-match-p "RUN-B-FINAL" text))
             (should-not (string-match-p "RUN-B-LATE" text))
             (should-not (text-property-any
                          (point-min) (point-max)
                          'face 'superchat-streaming-pending))
             (should (string-match-p "TTFT: 0.25s" text))
             (should (string-match-p
                      (regexp-quote "Session compacted") text))))))))))

(ert-deftest test-expand-typed-compact-anchor-is-strict-and-idempotent ()
  "Expansion requires a typed Anchor and does not duplicate restored rows."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "strict-expand")
            (user-id (superchat-db-tape-append session-id "user" "hello"))
            (assistant-id
             (superchat-db-tape-append session-id "assistant" "world"))
            (anchor-id
             (superchat-db-tape-append
              session-id "anchor" "summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,user-id ,assistant-id]))))
            (superchat--session-id session-id)
            (superchat--conversation-history
             (list (list :role 'system :content "[Session anchor]\n\nsummary"
                         :anchor t :compaction-anchor t :id anchor-id))))
       (with-temp-buffer
         (let ((superchat--conversation-history
                superchat--conversation-history))
           (should (= 2 (superchat-compact--expand-anchor)))
           (should (= 2 (length superchat--conversation-history)))
           (should (equal '("world" "hello")
                          (mapcar (lambda (message)
                                    (plist-get message :content))
                                  superchat--conversation-history)))
           (should-not (superchat-compact--expand-anchor))
           (should (= 2 (length superchat--conversation-history)))))
       (should (= anchor-id 3))))))

(ert-deftest test-expand-replaces-anchor-in-place-and-keeps-unrelated-order ()
  "Expansion splices sources where the selected Anchor was recorded."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "expand-in-place")
            (first-id (superchat-db-tape-append session-id "user" "first"))
            (second-id
             (superchat-db-tape-append session-id "assistant" "second"))
            (anchor-id
             (superchat-tape-append-compaction-anchor
              session-id "summary" (vector first-id second-id)))
            (superchat--session-id session-id)
            (superchat--expanded-compaction-anchor-ids nil)
            (superchat--conversation-history
             (list (list :role 'user :content "unrelated first" :id 40)
                   (list :role 'system :content "summary"
                         :anchor t :compaction-anchor t :id anchor-id)
                   (list :role 'system :content "no-id marker")
                   (list :role 'assistant :content "unrelated last" :id 5))))
       (with-temp-buffer
         (let ((superchat--conversation-history
                superchat--conversation-history))
           (should (= 2 (superchat-compact--expand-anchor)))
           (should (equal '("unrelated first" "second" "first"
                            "no-id marker" "unrelated last")
                          (mapcar (lambda (message)
                                    (plist-get message :content))
                                  superchat--conversation-history)))
           (should (equal '(40 2 1 nil 5)
                          (mapcar (lambda (message)
                                    (plist-get message :id))
                                  superchat--conversation-history)))))))))

(ert-deftest test-auto-handoff-compacts-long-prompt ()
  "`superchat-handoff--maybe-auto-compact' compacts when prompt is long."
  (let ((superchat-handoff-auto-enabled t)
        (superchat-handoff-char-threshold 10)
        (superchat--session-id "handoff-test")
        (compact-called nil)
        (turn (superchat-turn-new "short")))
    (setf (superchat-turn-prompt turn) "this prompt is definitely longer than ten characters")
    (cl-letf (((symbol-function 'superchat-compact-session)
               (lambda () (setq compact-called t) "summary")))
      (let ((result (superchat-handoff--maybe-auto-compact turn)))
        (should compact-called)
        (should (superchat-turn-p result))))))

(ert-deftest test-auto-handoff-skips-while-compaction-pending ()
  "`superchat-handoff--maybe-auto-compact' must not stack compaction Runs."
  (with-temp-buffer
    (let ((superchat-handoff-auto-enabled t)
          (superchat-handoff-char-threshold 10)
          (superchat--session-id "handoff-test")
          (superchat-compact--in-flight t)
          (compact-called nil)
          (turn (superchat-turn-new "short")))
      (setf (superchat-turn-prompt turn)
            "this prompt is definitely longer than ten characters")
      (cl-letf (((symbol-function 'superchat-compact-session)
                 (lambda () (setq compact-called t) "summary")))
        (let ((result (superchat-handoff--maybe-auto-compact turn)))
          (should-not compact-called)
          (should (superchat-turn-p result)))))))

(ert-deftest test-auto-handoff-skips-short-prompt ()
  "`superchat-handoff--maybe-auto-compact' does nothing when prompt is short."
  (let ((superchat-handoff-auto-enabled t)
        (superchat-handoff-char-threshold 1000)
        (superchat--session-id "handoff-test")
        (compact-called nil)
        (turn (superchat-turn-new "short")))
    (setf (superchat-turn-prompt turn) "hi")
    (cl-letf (((symbol-function 'superchat-compact-session)
               (lambda () (setq compact-called t) "summary")))
      (let ((result (superchat-handoff--maybe-auto-compact turn)))
        (should (not compact-called))
        (should (superchat-turn-p result))))))

(ert-deftest compact/integration-typed-anchor-reaches-next-view ()
  "A real compaction Run leaves a typed summary for the next View."
  (test-compact--with-temp-db
   (lambda ()
     (with-temp-buffer
       (let* ((session-id "compact-integration")
              (superchat--session-id session-id)
              (superchat-buffer-name "*compact-integration*")
              (superchat-compact-message-count nil)
              (superchat-context-max-chars 50)
              (first-id
               (superchat-tape-append-material session-id "user" "old question"))
              (second-id
               (superchat-tape-append-material session-id "assistant" "old answer"))
              (recent-id
               (superchat-tape-append-material session-id "user" "recent fact"))
              (superchat--conversation-history
               (list (list :role 'user :content "recent fact" :id recent-id)
                     (list :role 'assistant :content "old answer" :id second-id)
                     (list :role 'user :content "old question" :id first-id)))
              request)
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (value)
                      (setq request value)
                      (funcall (plist-get value :delivery-function)
                               (concat "summary of old exchange\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "compact-integration-run" :status accepted)))
                   ((symbol-function 'message) #'ignore))
           (should (superchat-compact-session))
           (let* ((anchor (superchat-view-compaction-anchor-latest session-id))
                  (meta (superchat-view-compaction-anchor-metadata anchor))
                  (source-ids (superchat-view-compaction-anchor-source-ids anchor))
                  (next-id
                   (superchat-tape-append-material
                    session-id "user" "new fact after compact"))
                  (view (superchat-view-assemble session-id))
                  (items (plist-get view :items))
                  (represented
                   (plist-get (plist-get view :disclosure) :represented)))
             (should anchor)
             (should (equal "compact" (nth 1 anchor)))
             (should (equal superchat-tape-compaction-anchor-schema
                            (alist-get 'schema meta)))
             (should (equal (vector first-id second-id) source-ids))
             (should (cl-every (lambda (id) (and (integerp id) (> id 0)))
                               (append source-ids nil)))
             (should-not (member :unknown (append source-ids nil)))
             (should (not (equal session-id (plist-get request :session-id))))
             (should (= next-id 5))
             (should (equal "recent fact"
                            (plist-get (aref items 0) :content)))
             (should (equal "summary of old exchange"
                            (plist-get (aref items 1) :content)))
             (should (equal "new fact after compact"
                            (plist-get (aref items 2) :content)))
             (should (= 1 (length represented)))
             (should (equal (vector first-id second-id)
                            (plist-get (aref represented 0) :source-ids)))
             (should (= 4 (plist-get (aref represented 0) :via-storage-id))))))))))

(ert-deftest compact/integration-repeat-keeps-each-source-set ()
  "Repeated compaction appends a new Anchor for only new raw facts."
  (test-compact--with-temp-db
   (lambda ()
     (with-temp-buffer
       (let* ((session-id "compact-repeat")
              (superchat--session-id session-id)
              (superchat-buffer-name "*compact-repeat*")
              (superchat-compact-message-count nil)
              (superchat-context-max-chars 50)
              (first-id (superchat-tape-append-material session-id "user" "first"))
              (first-answer-id
               (superchat-tape-append-material
                session-id "assistant" "first answer"))
              (superchat--conversation-history
               (list (list :role 'assistant :content "first answer"
                           :id first-answer-id)
                     (list :role 'user :content "first" :id first-id)))
              summaries)
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (push (plist-get request :prompt) summaries)
                      (funcall (plist-get request :delivery-function)
                               (if (= (length summaries) 1)
                                   (concat "first summary\n"
                                           superchat-compact--completion-marker)
                                 (concat "second summary\n"
                                         superchat-compact--completion-marker))
                               'completed nil)
                      '(:run-id "compact-repeat-run" :status accepted)))
                   ((symbol-function 'message) #'ignore))
           (should (superchat-compact-session))
           (let* ((second-id (superchat-tape-append-material
                              session-id "assistant" "second"))
                  (latest-id (superchat-tape-append-material
                              session-id "user" "latest")))
             (push (list :role 'assistant :content "second" :id second-id)
                   superchat--conversation-history)
             (push (list :role 'user :content "latest" :id latest-id)
                   superchat--conversation-history)
             (should (superchat-compact-session))
             (let* ((anchors (cl-remove-if-not
                              (lambda (row)
                                (and (string= "anchor" (nth 2 row))
                                     (string= "compact" (nth 1 row))))
                              (superchat-view-entries session-id)))
                    (first-meta
                     (superchat-view-compaction-anchor-metadata (nth 0 anchors)))
                    (second-meta
                     (superchat-view-compaction-anchor-metadata (nth 1 anchors))))
              (should (= 2 (length anchors)))
               (should (equal (vector first-id)
                              (alist-get 'source_ids first-meta)))
               (should (equal (vector first-id second-id)
                              (alist-get 'source_ids second-meta)))
               (dolist (anchor anchors)
                 (should-not (member :unknown
                                     (append
                                      (superchat-view-compaction-anchor-source-ids
                                       anchor)
                                      nil))))))))))))

(ert-deftest compact/integration-expand-is-strict-and-idempotent ()
  "Expand restores exact sources, preserves later facts, and repeats safely."
  (test-compact--with-temp-db
   (lambda ()
     (with-temp-buffer
       (let* ((session-id "compact-expand")
              (superchat--session-id session-id)
              (superchat-buffer-name "*compact-expand*")
              (superchat-compact-message-count nil)
              (superchat-context-max-chars 50)
              (first-id (superchat-tape-append-material session-id "user" "source one"))
              (second-id (superchat-tape-append-material
                          session-id "assistant" "source two"))
              (recent-id (superchat-tape-append-material
                          session-id "user" "source three"))
              (superchat--conversation-history
               (list (list :role 'user :content "source three" :id recent-id)
                     (list :role 'assistant :content "source two" :id second-id)
                     (list :role 'user :content "source one" :id first-id)))
              pending)
         (cl-letf (((symbol-function 'superchat-runtime-submit)
                    (lambda (request)
                      (setq pending request)
                      (funcall (plist-get request :delivery-function)
                               (concat "summary\n"
                                       superchat-compact--completion-marker)
                               'completed nil)
                      '(:run-id "compact-expand-run" :status accepted)))
                   ((symbol-function 'message) #'ignore))
           (should (superchat-compact-session))
             (let ((later-id (superchat-tape-append-material
                            session-id "user" "later raw fact")))
             (push (list :role 'user :content "later raw fact" :id later-id)
                   superchat--conversation-history)
             (let (after-first)
               (should (= 2 (or (superchat-compact--expand-anchor session-id) 0)))
               (should (equal '("later raw fact" "source three"
                                "source two" "source one")
                              (mapcar (lambda (entry) (plist-get entry :content))
                                      superchat--conversation-history)))
               (should-not (cl-some (lambda (entry) (plist-get entry :anchor))
                                    superchat--conversation-history))
               (setq after-first (copy-tree superchat--conversation-history))
               (should-not (superchat-compact--expand-anchor session-id))
               (should (equal after-first superchat--conversation-history)))
              (should pending))))))))

(ert-deftest test-compact-session-helper-does-not-mint-session-id ()
  "The compaction helper only reuses an existing Session id."
  (let ((superchat--session-id nil))
    (should-not (superchat-compact--ensure-session-id))
    (should-not superchat--session-id))
  (let ((superchat--session-id "existing-session"))
    (should (equal "existing-session"
                   (superchat-compact--ensure-session-id)))))

(ert-deftest test-compact-session-no-op-on-non-chat-buffer ()
  "A non-chat buffer reports no messages without building or submitting a Run."
  (let (messages request-built submitted)
    (with-temp-buffer
      (setq-local superchat--session-id nil)
      (setq-local superchat-buffer-name nil)
      (setq-local superchat--conversation-history nil)
      (setq-local superchat-compact--in-flight nil)
      (cl-letf (((symbol-function 'superchat-compact--request)
                 (lambda (&rest _args)
                   (setq request-built t)
                   (ert-fail "non-chat compaction must not build a Request")))
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (&rest _args)
                   (setq submitted t)
                   (ert-fail "non-chat compaction must not submit a Run")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should-not (superchat-compact-session)))
      (should-not request-built)
      (should-not submitted)
      (should (equal '("No messages to compact.") messages)))))

(ert-deftest test-compact-request-rejects-missing-session-id ()
  "The private compaction Request builder requires a real Session id."
  (should-error
   (superchat-compact--request "SUMMARY" nil #'ignore)
   :type 'error)
  (condition-case err
      (superchat-compact--request "SUMMARY" nil #'ignore)
    (error
     (should (string-match-p "session" (downcase (error-message-string err)))))))

(ert-deftest compact/terminal-delivery-closes-operation-child-exactly-once ()
  "Repeated compactions leave no hidden live protocol host behind."
  (dotimes (index 2)
    (let* ((request (superchat-compact--request
                     "SUMMARY" "compact-operation-parent" #'ignore))
           (child (plist-get request :session-id))
           (host (plist-get request :buffer))
           (delivery (plist-get request :delivery-function)))
      (should (buffer-live-p host))
      (funcall delivery "done" 'completed index)
      (should-not (buffer-live-p host))
      (should (eq 'closed (superchat-editor-workspace-state child)))
      ;; Defensive duplicate transport delivery neither repeats user callback
      ;; nor reopens or re-closes the already terminal child.
      (funcall delivery "late" 'failed nil)
      (should (eq 'closed (superchat-editor-workspace-state child))))))

(ert-deftest compact/submission-failure-closes-materialized-child ()
  "A synchronous Runtime rejection cannot leak the already-created host."
  (let* ((request (superchat-compact--request
                   "SUMMARY" "compact-submit-parent" #'ignore))
         (child (plist-get request :session-id))
         (host (plist-get request :buffer)))
    (cl-letf (((symbol-function 'superchat-runtime-submit)
               (lambda (_request) (error "provider rejected"))))
      (should-error (superchat-compact--submit-request request) :type 'error))
    (should-not (buffer-live-p host))
    (should (eq 'closed (superchat-editor-workspace-state child)))))

(ert-deftest compact/synchronous-submit-quit-clears-parent-and-closes-child ()
  "A nonlocal provider exit cannot leave either compaction scope in flight."
  (with-temp-buffer
    (let* ((request (superchat-compact--request
                     "SUMMARY" "compact-quit-parent" #'ignore))
           (child (plist-get request :session-id))
           (host (plist-get request :buffer)))
      (setq-local superchat-compact--in-flight t)
      (cl-letf (((symbol-function 'superchat-compact--request)
                 (lambda (&rest _args) request))
                ((symbol-function 'superchat-compact--ensure-session-id)
                 (lambda () "compact-quit-parent"))
                ((symbol-function 'superchat-compact--context-budget)
                 (lambda (&rest _args) 1000))
                ((symbol-function 'superchat-compact--collect-messages)
                 (lambda (&rest _args) '((:id 1 :content "hello"))))
                ((symbol-function 'superchat-view-compaction-anchor-latest)
                 (lambda (&rest _args) nil))
                ((symbol-function 'superchat-runtime-submit)
                 (lambda (_request) (signal 'quit nil)))
                ((symbol-function 'message) #'ignore))
        ;; Reset to model the public command's initially idle parent View.
        (setq superchat-compact--in-flight nil)
        (should
         (eq 'quit
             (condition-case _condition
                 (superchat-compact-session)
               (quit 'quit))))
        (should-not superchat-compact--in-flight)
        (should-not (buffer-live-p host))
        (should (eq 'closed
                    (superchat-editor-workspace-state child)))))))

(ert-deftest compact/real-runtime-provider-quit-seals-both-scopes ()
  "Real Runtime failure delivery seals the Run and Compact parent/child state."
  (with-temp-buffer
    (let ((superchat-runtime--runs (make-hash-table :test #'equal))
          (superchat-runtime--closure-observations nil)
          (superchat-closure-terminal-guard-enabled nil)
          (superchat-closure-shadow-enabled nil)
          (superchat-run-completed-functions nil)
          (superchat-turn-completed-functions nil)
          (superchat-post-turn-functions nil)
          (superchat-post-topic-finalized-functions nil)
          (superchat-pre-topic-finalized-functions nil)
          (finish-count 0)
          child host run-id caught)
      (setq-local superchat-compact--in-flight nil)
      (cl-letf (((symbol-function 'superchat-compact--ensure-session-id)
                 (lambda () "compact-real-quit-parent"))
                ((symbol-function 'superchat-compact--context-budget)
                 (lambda (&rest _args) 1000))
                ((symbol-function 'superchat-compact--collect-messages)
                 (lambda (&rest _args) '((:id 1 :content "hello"))))
                ((symbol-function 'superchat-view-compaction-anchor-latest)
                 (lambda (&rest _args) nil))
                ((symbol-function 'superchat-tape-append-event)
                 (let ((id 0)) (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (signal 'quit nil)))
                ((symbol-function 'message) #'ignore))
        (let ((original-request (symbol-function 'superchat-compact--request)))
          (cl-letf (((symbol-function 'superchat-compact--request)
                     (lambda (&rest args)
                       (let ((request (apply original-request args)))
                         (setq child (plist-get request :session-id)
                               host (plist-get request :buffer))
                         request))))
            (setq caught
                  (condition-case _condition
                      (superchat-compact-session
                       (lambda (&rest _args) (cl-incf finish-count)))
                    (quit 'quit)))))
        (should (eq 'quit caught))
        (should (= 1 finish-count))
        (should-not superchat-compact--in-flight)
        (should child)
        (should-not (buffer-live-p host))
        (should (eq 'closed (superchat-editor-workspace-state child)))
        (maphash (lambda (id _entry) (setq run-id id))
                 superchat-runtime--runs)
        (let ((entry (superchat-runtime--entry run-id)))
          (should (eq 'failed (plist-get entry :status)))
          (should (plist-get entry :quiesced))
          (should (eq host (plist-get entry :buffer))))))))

(ert-deftest compact/on-finish-error-and-quit-still-close-child ()
  "Nonlocal exit from terminal processing cannot skip child closure."
  (dolist (condition '(error quit))
    (let* ((request
            (superchat-compact--request
             "SUMMARY" "compact-callback-parent"
             (lambda (&rest _args)
               (signal condition (list "terminal callback exited")))))
           (child (plist-get request :session-id))
           (host (plist-get request :buffer)))
      (let ((caught
             (condition-case caught
                 (progn
                   (funcall (plist-get request :delivery-function)
                            "done" 'completed nil)
                   nil)
               ((error quit) (car caught)))))
        (should (eq condition caught)))
      (should-not (buffer-live-p host))
      (should (eq 'closed (superchat-editor-workspace-state child))))))

(ert-deftest compact/retained-child-remains-discoverable-and-retryable ()
  "A refused child close preserves EditorWorkspace retained semantics."
  (let* ((request (superchat-compact--request
                   "SUMMARY" "compact-retained-parent" #'ignore))
         (child (plist-get request :session-id))
         (host (plist-get request :buffer)))
    (unwind-protect
        (progn
          (with-current-buffer host
            (add-hook 'kill-buffer-query-functions (lambda () nil) nil t))
          (funcall (plist-get request :delivery-function)
                   "done" 'completed nil)
          (should (buffer-live-p host))
          (should (eq 'retained
                      (superchat-editor-workspace-state child)))
          (should (memq host
                        (superchat-editor-workspace-owned-buffers child)))
          (with-current-buffer host
            (setq kill-buffer-query-functions nil))
          (should (eq 'closed (superchat-session-workspace-close child))))
      (when (buffer-live-p host)
        (with-current-buffer host (setq kill-buffer-query-functions nil))
        (kill-buffer host)))))

(ert-deftest compact/close-failure-does-not-swallow-terminal-result ()
  "Child close errors are warnings, not replacements for terminal delivery."
  (let (seen)
    (let ((request
           (superchat-compact--request
            "SUMMARY" "compact-close-error-parent"
            (lambda (response status)
              (setq seen (list response status))))))
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-session-workspace-close)
                     (lambda (_session-id) (error "close failed")))
                    ((symbol-function 'display-warning) #'ignore))
            (funcall (plist-get request :delivery-function)
                     "original result" 'completed nil)
            (should (equal '("original result" completed) seen)))
        (let ((host (plist-get request :buffer)))
          (when (buffer-live-p host)
            (kill-buffer host)))))))

(defun test-compact--match-count (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (max (1+ (match-beginning 0)) (match-end 0))))
    count))

(require 'org)

(provide 'test-compact)

;;; test-compact.el ends here
