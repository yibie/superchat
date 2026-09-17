;;; m5-history-material-regression.el --- durable conversation history -*- lexical-binding: t; -*-

;;; Commentary:
;; Actual Runtime/SQLite/provider regression for display summaries replacing
;; user and assistant material.  Canonical smoke fixture; no real model and
;; no protected regression-set loading.
;; Run: -l scripts/m5-history-material-regression.el
;;      --eval '(ert-run-tests-batch-and-exit "^m5-history-material/")'

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t ert-batch-print-level 6 ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defconst m5hm--answer
  (concat "\nM5HM_FULL_ANSWER_START\n" (make-string 3 96)
          "elisp\n(setq-local fill-column 72)\n" (make-string 3 96) "\n"
          (make-string 900 ?甲)
          "\nM5HM_TAIL: No configuration was changed; undo must preserve the old binding.\n\n")
  "Raw answer with whitespace, Markdown, wide characters and a decisive tail.")
(defconst m5hm--user
  (concat "M5HM_USER_START\n" (make-string 900 ?乙)
          "\nM5HM_USER_TAIL: Preserve the pre-existing binding; undo only this task."))
(defvar m5hm--requests nil)

(defmacro m5hm--fixture (script &rest body)
  "Run BODY with SCRIPT and isolated durable history."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-context-max-chars 6000)
           (m5hm--requests nil))
       (smoke--with-fixture ,script
         (let ((network (symbol-function 'smoke--fake-network)))
           (cl-letf (((symbol-function 'smoke--fake-network)
                      (lambda (mode provider prompt partial response error multi)
                        (push (list :system (or (llm-chat-prompt-context prompt) "")
                                    :users
                                    (cl-loop
                                     for item in (llm-chat-prompt-interactions prompt)
                                     when (equal "user"
                                                 (format "%s" (llm-chat-prompt-interaction-role item)))
                                     collect (llm-chat-prompt-interaction-content item)))
                              m5hm--requests)
                        (funcall network mode provider prompt partial response error multi))))
             ,@body))))))

(defun m5hm--settle (run)
  "Wait for actual RUN completion and its durable Turn seal."
  (should (stringp run))
  (should (smoke--wait
           (lambda ()
             (and (smoke--run-done-p run)
                  (eq 'completed (plist-get (superchat-runtime-status run) :status))))
           15))
  (let ((session (plist-get (superchat-runtime-status run) :session-id)))
    (should (smoke--wait
             (lambda ()
               (cl-find-if
                (lambda (event)
                  (and (equal "turn.completed" (plist-get event :type))
                       (equal run (plist-get event :run-id))))
                (superchat-tape-replay-events session)))
             3)))
  run)

(defun m5hm--rows (session kind)
  "Return actual SQLite material rows for SESSION and KIND."
  (cl-remove-if-not (lambda (row) (equal kind (nth 2 row)))
                    (superchat-view-entries session)))

(defun m5hm--second-user-text ()
  "Return user-role text from the actual second provider request."
  (should (= 2 (length m5hm--requests)))
  (mapconcat #'identity (plist-get (car m5hm--requests) :users) "\n"))

(defun m5hm--contains (needle text)
  "Match literal NEEDLE in TEXT, preserving whitespace and raw Markdown."
  (string-match-p (regexp-quote needle) text))

(defun m5hm--inspect (input)
  "Use the public inspector from the editing source."
  (with-current-buffer (get-buffer smoke--origin-buffer-name)
    (superchat-act input t))
  (let ((inspector
         (cl-find-if
          (lambda (buffer)
            (with-current-buffer buffer (derived-mode-p 'superchat-request-mode)))
          (buffer-list))))
    (should (buffer-live-p inspector))
    inspector))

(ert-deftest m5-history-material/second-run-receives-full-raw-assistant ()
  "The unique assistant fact keeps raw bytes and the next Run receives its tail."
  (m5hm--fixture (list (list :text m5hm--answer) '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (first (m5hm--settle (smoke--send view "M5HM_APPLY"))))
      (let* ((rows (m5hm--rows session "assistant"))
             (row (car rows))
             (event (cl-find-if
                     (lambda (item) (equal "response.completed" (plist-get item :type)))
                     (superchat-tape-replay-events session))))
        (should (= 1 (length rows)))
        (should (= (car row) (plist-get event :storage-id)))
        (should (equal m5hm--answer (nth 3 row)))
        (should (equal (encode-coding-string m5hm--answer 'utf-8)
                       (encode-coding-string (nth 3 row) 'utf-8)))
        (should (equal
                 (plist-get (plist-get event :payload) :content-hash)
                 (concat "sha256:"
                         (secure-hash 'sha256
                                      (encode-coding-string (nth 3 row) 'utf-8)))))
        (should (equal m5hm--answer
                       (plist-get (car (last (superchat-view-derive-messages session)))
                                  :content))))
      (let ((second (m5hm--settle (smoke--send view "M5HM_UNDO"))))
        (should-not (equal first second))
        (should (m5hm--contains m5hm--answer (m5hm--second-user-text)))
        (should (= 2 (length (m5hm--rows session "assistant"))))
        (let ((responses
               (cl-remove-if-not
                (lambda (event) (equal "response.completed" (plist-get event :type)))
                (superchat-tape-replay-events session))))
          (should (= 2 (length responses)))
          (dolist (key '(:event-id :turn-id))
            (should (= 2 (length (delete-dups
                                  (mapcar (lambda (event) (plist-get event key))
                                          responses)))))))))))

(ert-deftest m5-history-material/second-run-retains-long-user-tail ()
  "A later request can see the original user's tail constraint."
  (m5hm--fixture '((:text "Acknowledged") (:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m5hm--settle (smoke--send view m5hm--user))
      (should (equal m5hm--user (nth 3 (car (m5hm--rows session "user")))))
      (m5hm--settle (smoke--send view "M5HM_USER_FOLLOWUP"))
      (should (m5hm--contains m5hm--user (m5hm--second-user-text)))
      (should (= 2 (length (m5hm--rows session "user")))))))

(ert-deftest m5-history-material/reopen-keeps-full-history-without-view-cache ()
  "Close/reopen SQLite, discard the View cache, then send a second actual Run."
  (m5hm--fixture (list (list :text m5hm--answer) '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m5hm--settle (smoke--send view "M5HM_BEFORE_REOPEN"))
      (let ((row-id (caar (m5hm--rows session "assistant"))))
        (superchat-db-close)
        (setq superchat-db--connection nil)
        (superchat-db-open)
        (with-current-buffer view (setq superchat--conversation-history nil))
        (should (= row-id (caar (m5hm--rows session "assistant"))))
        (m5hm--settle (smoke--send view "M5HM_AFTER_REOPEN"))
        (should (m5hm--contains m5hm--answer (m5hm--second-user-text)))
        (should (equal m5hm--answer (nth 3 (car (m5hm--rows session "assistant")))))))))

(ert-deftest m5-history-material/budget-omits-whole-item-and-larger-budget-retains-it ()
  "A small history budget discloses omission; increasing it restores the whole item."
  (m5hm--fixture (list (list :text m5hm--answer))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m5hm--settle (smoke--send view "M5HM_BUDGET"))
      (let* ((id (caar (m5hm--rows session "assistant")))
             (small (superchat-view-assemble session :max-entries 50 :max-chars 100))
             (large (superchat-view-assemble session :max-entries 50 :max-chars 6000)))
        (should (eq 'partial (plist-get small :status)))
        (should-not (string-match-p "M5HM_FULL_ANSWER_START" (plist-get small :prompt)))
        (should-not
         (cl-find id (append (plist-get small :items) nil)
                  :key (lambda (item) (plist-get item :storage-id))))
        (should
         (cl-some
          (lambda (omission)
            (let ((range (plist-get omission :range)))
              (and range (<= (aref range 0) id) (<= id (aref range 1))
                   (eq 'oversized (plist-get omission :reason)))))
          (append (plist-get (plist-get small :disclosure) :omitted) nil)))
        (should (m5hm--contains m5hm--answer (plist-get large :prompt)))
        (let ((item (cl-find id (append (plist-get large :items) nil)
                             :key (lambda (item) (plist-get item :storage-id)))))
          (should (equal m5hm--answer (plist-get item :content)))
          (should (eq 'raw-fact (plist-get item :class)))
          (should-not (plist-get item :lossy)))))))

(ert-deftest m5-history-material/inspector-shows-full-history-and-honors-deletion ()
  "The inspector gets the full material; Send preserves an explicit deletion."
  (m5hm--fixture (list (list :text m5hm--answer) '(:text "done"))
    (let ((view (smoke--open-chat)))
      (m5hm--settle (smoke--send view "M5HM_INSPECT_APPLY"))
      (let ((inspector (m5hm--inspect "M5HM_INSPECT_UNDO"))
            (edited "M5HM_EDITED_ONLY\nDo not reinsert deleted history."))
        (with-current-buffer inspector
          (should
           (m5hm--contains
            m5hm--answer
            (buffer-substring-no-properties superchat--request-prompt-start
                                            superchat--request-prompt-end)))
          (let ((inhibit-read-only t))
            (goto-char superchat--request-prompt-start)
            (delete-region superchat--request-prompt-start superchat--request-prompt-end)
            (insert edited))
          (superchat-request-send))
        (should (smoke--wait
                 (lambda ()
                   (and (= 2 (length m5hm--requests))
                        (null (superchat-runtime-active-runs))))
                 15))
        (should (equal (list edited) (plist-get (car m5hm--requests) :users)))))))

(ert-deftest m5-history-material/structural-event-summary-remains-bounded ()
  "Preserving conversation facts must not unbound tool/audit event summaries."
  (m5hm--fixture nil
    (let* ((session "m5hm-structural")
           (summary (concat "A redacted audit summary " (make-string 900 ?甲)))
           (record
            (superchat-tape-append-event
             (list :type "tool.requested" :session-id session
                   :run-id "m5hm-structural-run" :turn-id "m5hm-structural-turn"
                   :payload '(:tool-name "eval_elisp") :summary summary)))
           (row (car (superchat-view-entries session)))
           (stored (nth 3 row)))
      (should (= (car row) record))
      (should (equal "tool_call" (nth 2 row)))
      (should (<= (string-width stored) 500))
      (should (< (length stored) (length summary)))
      (should (string-suffix-p "…" stored))
      (should (equal "eval_elisp"
                     (plist-get (plist-get (car (superchat-tape-replay-events session))
                                           :payload)
                                :tool-name))))))

(ert-deftest m5-history-material/empty-material-is-not-an-event-name ()
  "An explicit empty raw conversation string stays empty."
  (m5hm--fixture nil
    (dolist (type '("intent.received" "response.completed"))
      (let* ((session (concat "m5hm-empty-" type))
             (_record
              (superchat-tape-append-event
               (list :type type :session-id session
                     :run-id "m5hm-empty-run" :turn-id "m5hm-empty-turn"
                     :payload (superchat-runtime--content-metadata "")
                     :summary "")))
             (row (car (superchat-view-entries session))))
        (should (equal "" (nth 3 row)))))))

(ert-deftest m5-history-material/old-truncated-row-is-not-fabricated-or-rewritten ()
  "An old prefix plus hash cannot recover missing original text."
  (m5hm--fixture nil
    (let* ((session "m5hm-legacy")
           (old (truncate-string-to-width m5hm--answer 500 nil nil "…"))
           (meta
            (superchat-tape--encode-meta
             (list :event_type "response.completed" :event_id "m5hm-legacy-event"
                   :schema 1 :session_id session
                   :run_id "m5hm-legacy-run" :turn_id "m5hm-legacy-turn"
                   :payload (superchat-runtime--content-metadata m5hm--answer))))
           (id (superchat-db-tape-append session "assistant" old :meta meta)))
      (superchat-db-close)
      (setq superchat-db--connection nil)
      (superchat-db-open)
      (let ((row (car (superchat-view-entries session))))
        (should (equal old (nth 3 row)))
        (should (equal meta (nth 4 row)))
        (should (= id (car row)))
        (should (equal old (plist-get (car (superchat-view-derive-messages session))
                                     :content)))
        (should-not (m5hm--contains m5hm--answer
                                    (plist-get (superchat-view-assemble
                                                session :max-entries 50 :max-chars 6000)
                                               :prompt)))))))

(provide 'm5-history-material-regression)
;;; m5-history-material-regression.el ends here
