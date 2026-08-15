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
            (superchat-compact-message-count 2)
            (messages (superchat-compact--collect-messages)))
       (should (= (length messages) 2))
       (should (string= (plist-get (car messages) :content) "Hi there"))
       (should (string= (plist-get (cadr messages) :content) "What is 2+2?"))))))

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
             (should-not (plist-get captured-request :record-conversation))
             (should (= (length anchors) 1))
             (should (string= (car anchors) "Summary of the conversation."))
             (should (= (length superchat--conversation-history) 1))
             (should (plist-get (car superchat--conversation-history) :anchor))
             (should (string-match-p "Summary of the conversation"
                                     (plist-get (car superchat--conversation-history)
                                                :content))))))))))

(ert-deftest test-compact-session-persists-typed-durable-provenance ()
  "Compact writes the local typed Anchor schema from durable raw rows."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "typed-compact-writer")
            (user-id (superchat-db-tape-append session-id "user" "question"))
            (assistant-id
             (superchat-db-tape-append session-id "assistant" "answer"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role assistant :content "answer" :id ,assistant-id)
               (:role user :content "question" :id ,user-id)))
            (superchat-compact-message-count 10)
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
           (should (equal (vector user-id assistant-id)
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
            (new-user (superchat-db-tape-append session-id "user" "second"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "second" :id ,new-user)
               (:role system :content "[Session anchor]\n\nfirst summary"
                :anchor t)))
            (superchat-compact-message-count 10)
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
           (should (= new-user 4))
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
           (should (= 2 (length superchat--conversation-history)))
           (should (string= "during-flight"
                            (plist-get (car superchat--conversation-history)
                                       :content)))
           (should (plist-get (cadr superchat--conversation-history)
                              :anchor))))))))

(ert-deftest test-compact-session-failed-run-leaves-history-alone ()
  "A failed compaction Run must clear the pending flag and change nothing."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-failed")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "Hello" :id ,hello-id)))
            (superchat-compact-message-count 10)
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
           (should (= 1 (length superchat--conversation-history)))
           (should-not (plist-get (car superchat--conversation-history)
                                  :anchor))))))))

(ert-deftest test-compact-truncated-summary-leaves-history-alone ()
  "A completed response without the marker must not write an Anchor."
  (test-compact--with-temp-db
   (lambda ()
     (let* ((session-id "test-compact-truncated")
            (hello-id (superchat-db-tape-append session-id "user" "Hello"))
            (superchat--session-id session-id)
            (superchat--conversation-history
             `((:role user :content "Hello" :id ,hello-id)))
            (superchat-compact-message-count 10)
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
                ((symbol-function 'superchat-compact--context-prompt)
                 (lambda (_session-id) "rebuilt view")))
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
              (first-id
               (superchat-tape-append-material session-id "user" "old question"))
              (second-id
               (superchat-tape-append-material session-id "assistant" "old answer"))
              (superchat--conversation-history
               (list (list :role 'assistant :content "old answer" :id second-id)
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
             (should (= next-id 4))
             (should (equal "summary of old exchange"
                            (plist-get (aref items 0) :content)))
             (should (equal "new fact after compact"
                            (plist-get (aref items 1) :content)))
             (should (= 1 (length represented)))
             (should (equal (vector first-id second-id)
                            (plist-get (aref represented 0) :source-ids)))
             (should (= 3 (plist-get (aref represented 0) :via-storage-id))))))))))

(ert-deftest compact/integration-repeat-keeps-each-source-set ()
  "Repeated compaction appends a new Anchor for only new raw facts."
  (test-compact--with-temp-db
   (lambda ()
     (with-temp-buffer
       (let* ((session-id "compact-repeat")
              (superchat--session-id session-id)
              (superchat-buffer-name "*compact-repeat*")
              (superchat-compact-message-count nil)
              (first-id (superchat-tape-append-material session-id "user" "first"))
              (superchat--conversation-history
               (list (list :role 'user :content "first" :id first-id)))
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
                              session-id "assistant" "second")))
             (push (list :role 'assistant :content "second" :id second-id)
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
              (first-id (superchat-tape-append-material session-id "user" "source one"))
              (second-id (superchat-tape-append-material
                          session-id "assistant" "source two"))
              (superchat--conversation-history
               (list (list :role 'assistant :content "source two" :id second-id)
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
               (should (equal '("later raw fact" "source two" "source one")
                              (mapcar (lambda (entry) (plist-get entry :content))
                                      superchat--conversation-history)))
               (should-not (cl-some (lambda (entry) (plist-get entry :anchor))
                                    superchat--conversation-history))
               (setq after-first (copy-tree superchat--conversation-history))
               (should-not (superchat-compact--expand-anchor session-id))
               (should (equal after-first superchat--conversation-history)))
             (should pending))))))))

(provide 'test-compact)

;;; test-compact.el ends here
