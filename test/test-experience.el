;;; test-experience.el --- Epistemic experience tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-claims)
(require 'superchat-db)
(require 'superchat-experience)
(require 'superchat-projection)
(require 'superchat-tape)

(defmacro test-experience--with-temp-store (&rest body)
  "Run BODY with an isolated Superchat database."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "superchat-experience-test-" t))
          (superchat-data-directory directory))
     (unwind-protect
         (progn
           (superchat-db-close)
           (superchat-db-open)
           ,@body)
       (superchat-db-close)
       (delete-directory directory t))))

(defun test-experience--append
    (session-id turn-id run-id type event-id cause payload)
  "Append one test event with stable Runtime identity."
  (superchat-tape-append-event-record
   (append
    (list :type type :event-id event-id :schema 1
          :session-id session-id :turn-id turn-id :run-id run-id
          :actor-id "lead" :payload payload)
    (when cause (list :causation-id cause)))))

(defun test-experience--closed-run (session-id stem)
  "Append and return one closed evidence-bearing Run named by STEM."
  (let* ((turn-id (format "turn-%s" stem))
         (run-id (format "run-%s" stem))
         (intent-id (format "evt-%s-intent" stem))
         (start-id (format "evt-%s-start" stem))
         (decision-id (format "evt-%s-decision" stem))
         (output-id (format "evt-%s-output" stem))
         (response-id (format "evt-%s-response" stem))
         (terminal-id (format "evt-%s-terminal" stem))
         (turn-terminal-id (format "evt-%s-turn-terminal" stem)))
    (test-experience--append
     session-id turn-id run-id "intent.received" intent-id nil nil)
    (test-experience--append
     session-id turn-id run-id "run.started" start-id intent-id
     '(:status running-model :actor-id "lead" :capability "chat"))
    (test-experience--append
     session-id turn-id run-id "decision.snapshot.recorded" decision-id start-id
     '(:evidence-contract "epistemic-learning-v0" :attempt 1))
    (test-experience--append
     session-id turn-id run-id "output.first.recorded" output-id decision-id
     '(:evidence-contract "epistemic-learning-v0" :kind "text" :text "ok"))
    (test-experience--append
     session-id turn-id run-id "response.completed" response-id output-id
     '(:status completed :content-size 2))
    (test-experience--append
     session-id turn-id run-id "run.completed" terminal-id response-id
     '(:status completed))
    (test-experience--append
     session-id turn-id run-id "turn.completed" turn-terminal-id terminal-id
     '(:status completed))
    (list :session-id session-id :turn-id turn-id :run-id run-id
          :decision (list :session-id session-id :event-id decision-id)
          :output (list :session-id session-id :event-id output-id)
          :terminal (list :session-id session-id :event-id terminal-id))))

(defun test-experience--evaluation-spec (run &optional verdict)
  "Return a valid evaluator spec for RUN and optional VERDICT."
  (list :session-id (plist-get run :session-id)
        :actor-id "evaluator:test"
        :exercise-id "exercise-opaque-1"
        :scope "opaque-recovery/v0"
        :run-id (plist-get run :run-id)
        :decision (plist-get run :decision)
        :terminal (plist-get run :terminal)
        :evaluator '(:id "deterministic-rubric" :version 1)
        :evaluator-manifest-sha256
        (concat "sha256:" (make-string 64 ?a))
        :verdict (or verdict 'passed)
        :observations '(:first-action "report")
        :metrics '(:score 1 :latency-ms 8)
        :evidence (list (plist-get run :output))))

(defun test-experience--row-count ()
  "Return the number of rows in the current Tape."
  (caar (superchat-db-tape-select "SELECT COUNT(*) FROM tape")))

(defun test-experience--promotion-fixture (&optional fixture-scope)
  "Create and return one evaluated applicable Candidate fixture."
  (let* ((scope (or fixture-scope "opaque-recovery/v0"))
         (run (test-experience--closed-run "evaluation-session" "promotion"))
         (candidate
          (superchat-claim-record
           "Refresh before retrying E-R17." scope
           (list (plist-get run :output))
           "candidate-session" "agent:analyst" nil 'candidate))
         (context
          (superchat-claim-record
           "The current failure code is E-R17." scope
           (list (plist-get run :output))
           "context-session" "agent:analyst" nil 'context))
         (candidate-ref
          (list :session-id "candidate-session"
                :event-id (plist-get candidate :event-id)))
         (context-ref
          (list :session-id "context-session"
                :event-id (plist-get context :event-id)))
         (evaluation-spec (test-experience--evaluation-spec run)))
    (setq evaluation-spec (plist-put evaluation-spec :scope scope))
    (setq evaluation-spec
          (plist-put evaluation-spec :evidence
                     (list (plist-get run :output) candidate-ref)))
    (let* ((evaluation
            (superchat-experience-record-evaluation evaluation-spec))
           (evaluation-ref
            (list :session-id "evaluation-session"
                  :event-id (plist-get evaluation :event-id))))
      (superchat-experience-record-relation
       (list :session-id "relation-session" :actor-id "agent:analyst"
             :correlation-id "promotion-thread" :relation 'applies-under
             :from candidate-ref :to context-ref :scope scope
             :basis (list evaluation-ref)))
      (list :scope scope :run run :candidate candidate-ref :context context-ref
            :evaluation evaluation-ref
            :sessions '("evaluation-session" "candidate-session"
                        "context-session" "relation-session"
                        "promotion-session")))))

(defun test-experience--failed-recovery-fixture ()
  "Create one promoted recovery followed by a failed First Action."
  (let* ((scope superchat-experience-workspace-revision-recovery-scope)
         (fixture (test-experience--promotion-fixture scope))
         (candidate (plist-get fixture :candidate))
         (context (plist-get fixture :context))
         (evaluation (plist-get fixture :evaluation))
         (promotion
          (superchat-experience-record-disposition
           (list :session-id "promotion-session" :actor-id "user"
                 :candidate candidate :scope scope :decision 'promote
                 :basis (list evaluation) :reason "Reviewed recovery.")))
         (view
          (superchat-experience-assemble-view
           (plist-get fixture :sessions) scope context))
         (trigger
          '(:session-id "recovery-session" :event-id "evt-review-trigger")))
    (test-experience--append
     "recovery-session" "turn-review" "run-review" "tool.failed"
     "evt-review-trigger" nil
     '(:tool-name "workspace_read"
       :error-type "superchat-workspace-artifact-revision-error"
       :execution-id "exec-trigger"))
    (test-experience--append
     "recovery-session" "turn-review" "run-review"
     "experience.intervention.recorded" "evt-review-intervention"
     "evt-review-trigger"
     (list :trigger trigger :scope scope :context context
           :receipt (plist-get view :receipt)))
    (test-experience--append
     "recovery-session" "turn-review" "run-review" "tool.requested"
     "evt-review-action" "evt-review-intervention"
     '(:tool-name "workspace_read" :execution-id "exec-recovery"))
    (test-experience--append
     "recovery-session" "turn-review" "run-review" "tool.failed"
     "evt-review-outcome" "evt-review-action"
     '(:tool-name "workspace_read" :execution-id "exec-recovery"
       :error-type "file-error"))
    (list :promotion
          (list :session-id "promotion-session"
                :event-id (plist-get promotion :event-id)))))

(ert-deftest experience/recovery-view-requires-exact-class-and-one-context ()
  "Only the typed Workspace recovery class may select reviewed experience."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let ((failure
           '(:type "tool.failed" :event-id "evt-recovery-failure"
             :session-id "recovery-session" :run-id "recovery-run"
             :payload (:tool-name "workspace_read"
                       :error-type
                       "superchat-workspace-artifact-revision-error")))
          (wrong-failure
           '(:type "tool.failed" :event-id "evt-wrong-failure"
             :session-id "recovery-session" :run-id "recovery-run"
             :payload (:tool-name "workspace_read" :error-type "file-error"))))
    (should-not
     (superchat-experience-assemble-recovery-view
      wrong-failure '("recovery-session")))
    (should
     (eq 'empty
         (plist-get
          (superchat-experience-assemble-recovery-view
           failure '("recovery-session"))
          :status)))
    (let* ((scope superchat-experience-workspace-revision-recovery-scope)
           (fixture (test-experience--promotion-fixture scope))
           (candidate (plist-get fixture :candidate))
           (evaluation (plist-get fixture :evaluation)))
      (superchat-experience-record-disposition
       (list :session-id "promotion-session" :actor-id "user"
             :candidate candidate :scope scope :decision 'promote
             :basis (list evaluation) :reason "Reviewed recovery."))
      ;; An unrelated matching Context must not affect an explicitly scoped
      ;; recovery assembly.
      (superchat-claim-record
       "An unrelated recovery context." scope nil
       "unrelated-context-session" "agent:analyst" nil 'context)
      (let ((view
             (superchat-experience-assemble-recovery-view
              failure
              (append (plist-get fixture :sessions)
                      '("recovery-session")))))
        (should (eq 'satisfied (plist-get view :status)))
        (should (string-match-p "Refresh before retrying E-R17"
                                (plist-get view :prompt))))
      (superchat-claim-record
       "A second matching recovery context." scope nil
       "other-context-session" "agent:analyst" nil 'context)
      (should
       (eq 'blocked
           (plist-get
            (superchat-experience-assemble-recovery-view
             failure
             (append (plist-get fixture :sessions)
                     '("recovery-session" "other-context-session")))
            :status)))))))

(ert-deftest experience/review-command-resolves-derived-need-three-ways ()
  "The host review action reuses append-only relation/disposition writers."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (dolist (judgment '("retain" "fails-under" "revoke"))
    (test-experience--with-temp-store
      (test-experience--failed-recovery-fixture)
      (let* ((before (superchat-experience-review-needs))
             (need (aref (plist-get before :open) 0))
             (shown
              (superchat-experience--cmd-review
               "review" "" nil nil nil)))
        (should (= 1 (length (plist-get before :open))))
        (should (string-match-p
                 (regexp-quote
                  (plist-get (plist-get need :candidate) :event-id))
                 (plist-get shown :content)))
        (let ((result
               (superchat-experience--cmd-review
                "review" (format "%s explicit reason" judgment)
                nil nil nil)))
          (should (string-match-p "Review recorded"
                                  (plist-get result :content))))
        (should (= 0 (length
                      (plist-get (superchat-experience-review-needs)
                                 :open))))))))

(defun test-experience--structural-fixture-runs ()
  "Append three varied success-skeleton and three varied failure-skeleton runs.
Variation keeps the compression signal honest: identical clone episodes
compress to zero cost against both corpora and produce meaningless ties."
  (dolist (stem '("succ-1" "succ-2" "succ-3"))
    (test-experience--append
     "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
     "tool.failed" (format "evt-%s-f1" stem) nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
     "tool.completed" (format "evt-%s-i1" stem) nil
     '(:tool-name "workspace_info"))
    (test-experience--append
     "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
     "tool.completed" (format "evt-%s-r1" stem) nil
     '(:tool-name "workspace_read"))
    (when (equal stem "succ-2")
      ;; an extra successful read after the recovery
      (test-experience--append
       "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
       "tool.completed" (format "evt-%s-r2" stem) nil
       '(:tool-name "workspace_read")))
    (when (equal stem "succ-3")
      ;; a retry before checking state, still recovered
      (test-experience--append
       "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
       "tool.failed" (format "evt-%s-f2" stem) nil
       '(:tool-name "workspace_read" :error-type "stale"))
      (test-experience--append
       "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
       "tool.completed" (format "evt-%s-r2" stem) nil
       '(:tool-name "workspace_read"))))
  (dolist (stem '("fail-1" "fail-2" "fail-3"))
    (let ((retries (string-to-number (substring stem -1))))
      (dotimes (i (+ retries 1))
        (test-experience--append
         "recovery-session" (format "turn-%s" stem) (format "run-%s" stem)
         "tool.failed" (format "evt-%s-f%d" stem (1+ i)) nil
         '(:tool-name "workspace_read" :error-type "stale"))))))

(ert-deftest experience/structural-hint-matches-success-pattern ()
  "A broken environment with the right behavior pattern ranks toward success."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    (test-experience--append
     "recovery-session" "turn-need" "run-need"
     "tool.failed" "evt-need-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-need" "run-need"
     "tool.failed" "evt-need-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    (let* ((need (list :intervention (list :session-id "recovery-session"
                                           :event-id "evt-need-f1"
                                           :run-id "run-need")))
           (hint (superchat-experience--structural-hint need)))
      (should hint)
      (should (eq 'recovered (plist-get hint :predicted)))
      (should (> (plist-get hint :margin) 0))
      (should (string-match-p
               "matches successful"
               (superchat-experience--structural-hint-text need))))))

(ert-deftest experience/structural-hint-ranks-patterns ()
  "A success-pattern failure ranks closer to success than a retry loop.
At fixture scale the success-pattern sign is stable; failure-class signs
are quantized noise, so only the relative ordering is asserted here.
The real-data probe (52 episodes) is the evidence for absolute
separation."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f2" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (let* ((broken-need (list :intervention
                              (list :session-id "recovery-session"
                                    :event-id "evt-broken-f1"
                                    :run-id "run-broken")))
           (retry-need (list :intervention
                             (list :session-id "recovery-session"
                                   :event-id "evt-retry-f1"
                                   :run-id "run-retry")))
           (broken (superchat-experience--structural-hint broken-need))
           (retry (superchat-experience--structural-hint retry-need)))
      (should broken)
      (should retry)
      (should (eq 'recovered (plist-get broken :predicted)))
      (should (> (plist-get broken :margin)
                 (plist-get retry :margin)))
      (should (string-match-p
               "matches successful"
               (superchat-experience--structural-hint-text broken-need))))))

(ert-deftest experience/structural-hint-silent-on-thin-history ()
  "Fewer than two runs per class must produce no hint."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--append
     "recovery-session" "turn-need" "run-need"
     "tool.failed" "evt-need-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (should-not
     (superchat-experience--structural-hint
      (list :intervention
            (list :session-id "recovery-session"
                  :event-id "evt-need-f1"
                  :run-id "run-need"))))))


(ert-deftest experience/monitor-reports-separation-and-watchlist ()
  "The structural monitor separates classes and flags pattern-match failures."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    ;; a failed Run whose behavior structurally matches success
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    (let* ((skeletons (superchat-experience--run-skeletons))
           (rows (superchat-experience--monitor-rows skeletons)))
      (should rows)
      (let ((success-margins
             (mapcar (lambda (row) (nth 2 row))
                     (cl-remove-if-not (lambda (row) (nth 1 row)) rows)))
            (failure-margins
             (mapcar (lambda (row) (nth 2 row))
                     (cl-remove-if (lambda (row) (nth 1 row)) rows)))
            (watchlist
             (cl-remove-if-not (lambda (row)
                                 (and (not (nth 1 row))
                                      (> (nth 2 row) 0)))
                               rows)))
        (should (> (apply #'+ success-margins) 0))
        ;; failure-class signs are quantized at fixture scale; the
        ;; ranking test above covers the relative contract
        (should (member "run-broken"
                        (mapcar (lambda (row) (nth 0 row)) watchlist)))))))

(ert-deftest experience/monitor-command-renders-and-thin-history-guard ()
  "The monitor command renders a summary and handles thin history."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (let ((buffer (get-buffer-create "*superchat-experience-monitor*")))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_buffer &rest _) nil)))
        (superchat-experience-monitor)
        (with-current-buffer buffer
          (should (string-match-p "No tool Runs in history"
                                  (buffer-string)))))
      (test-experience--structural-fixture-runs)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_buffer &rest _) nil)))
        (superchat-experience-monitor)
        (with-current-buffer buffer
          (let ((text (buffer-string)))
            (should (string-match-p "corpus: 3 recovered / 3 failed" text))
            (should (string-match-p "watchlist" text))))))))

(ert-deftest experience/structural-hint-works-through-review-needs ()
  "The real review-needs seam delivers a working hint.
A need built by review-needs carries only a canonical intervention
reference (no Run id); the hint must resolve the Run id from the
intervention event itself."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--failed-recovery-fixture)
    (test-experience--structural-fixture-runs)
    (let* ((view (superchat-experience-review-needs))
           (needs (append (plist-get view :open) nil))
           (need (car needs)))
      (should (= 1 (length needs)))
      (should (null (plist-get (plist-get need :intervention) :run-id)))
      (should (superchat-experience--structural-hint need))
      (should (string-match-p
               "Structural hint"
               (superchat-experience--structural-hint-text need))))))

(ert-deftest experience/auto-annotation-records-structure-verdict ()
  "The run-completion hook appends an auto-actor structural verdict."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    ;; a deviating Run to thicken the failure corpus (the broken
    ;; pattern needs it to rank positively at fixture scale)
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f2" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (let ((superchat-experience-auto-annotation-enabled t)
          (superchat-experience-auto-watchlist-threshold 1)
          (superchat-experience--watchlist-count 0)
          (alerts nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) alerts))))
        (superchat-experience--run-completed-hook
         (list :session-id "recovery-session" :turn-id "turn-broken"
               :run-id "run-broken" :status 'completed))
        (let* ((sessions (superchat-view-session-ids))
               (events (superchat-experience--view-events sessions))
               (verdicts
                (cl-remove-if-not
                 (lambda (e)
                   (equal "experience.structure.recorded"
                          (plist-get e :type)))
                 events)))
          (should (= 1 (length verdicts)))
          (let ((verdict (car verdicts)))
            (should (equal "experience-auto"
                           (plist-get verdict :actor-id)))
            (should (equal "run-broken" (plist-get verdict :run-id)))
            ;; payloads round-trip through Tape JSON storage, so the
            ;; symbol arrives back as a string
            (should (equal "recovered"
                           (plist-get (plist-get verdict :payload)
                                      :predicted))))
          ;; the broken-pattern Run is a watchlist member: alert fired
          (should (= 1 (length alerts)))
          (should (string-match-p "structurally matching success"
                                  (car alerts))))))))

(ert-deftest experience/auto-annotation-respects-disabled-default ()
  "The auto layer is a no-op while disabled (the default)."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    (let ((superchat-experience-auto-annotation-enabled nil))
      (superchat-experience--run-completed-hook
       (list :session-id "recovery-session" :turn-id "turn-broken"
             :run-id "run-broken" :status 'completed))
      (let* ((sessions (superchat-view-session-ids))
             (events (superchat-experience--view-events sessions)))
        (should-not
         (cl-some
          (lambda (e)
            (equal "experience.structure.recorded" (plist-get e :type)))
          events))))))

(ert-deftest experience/auto-sorted-review-queue-puts-hinted-first ()
  "The review queue sorts needs by structural margin, hinted first."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (test-experience--structural-fixture-runs)
    ;; two needs: broken-pattern (matches success, margin > 0) and a
    ;; retry loop (deviates); both resolve their Run from real events
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-broken" "run-broken"
     "tool.failed" "evt-broken-f2" nil
     '(:tool-name "workspace_info" :error-type "file-error"))
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f1" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (test-experience--append
     "recovery-session" "turn-retry" "run-retry"
     "tool.failed" "evt-retry-f2" nil
     '(:tool-name "workspace_read" :error-type "stale"))
    (let* ((broken-need (list :intervention
                              (list :session-id "recovery-session"
                                    :event-id "evt-broken-f1")))
           (retry-need (list :intervention
                             (list :session-id "recovery-session"
                                   :event-id "evt-retry-f1")))
           (sorted (superchat-experience--sort-needs
                    (list retry-need broken-need))))
      (should (equal broken-need (car sorted)))
      (should (equal retry-need (cadr sorted))))))

(defun test-experience--dual-candidate-fixture (&optional histories-p)
  "Two promoted Candidates applying to the same Context.
With HISTORIES-P, each candidate gets an intervention Run whose skeleton
differs in the first tool (search_text vs list_files), plus the current
Run prefix (search_text then the typed failure).  Returns the fixture
plist with the real failure event read back from the Tape."
  (let* ((fixture
          (test-experience--promotion-fixture
           superchat-experience-workspace-revision-recovery-scope))
         (scope (plist-get fixture :scope))
         (context (plist-get fixture :context))
         (candidate-a (plist-get fixture :candidate))
         (sessions (append (copy-sequence (plist-get fixture :sessions))
                           '("evaluation-b-session" "candidate-b-session"
                             "relation-b-session" "promotion-b-session"
                             "recovery-session")))
         (run-b (test-experience--closed-run
                 "evaluation-b-session" "promotion-b"))
         (candidate-b
          (superchat-claim-record
           "List the workspace first, then retry the read." scope
           (list (plist-get run-b :output))
           "candidate-b-session" "agent:analyst" nil 'candidate))
         (candidate-b-ref
          (list :session-id "candidate-b-session"
                :event-id (plist-get candidate-b :event-id)))
         (evaluation-b-spec
          (plist-put
           (plist-put (test-experience--evaluation-spec run-b)
                      :scope scope)
           :evidence
           (list (plist-get run-b :output) candidate-b-ref))))
    (let ((evaluation-b
           (superchat-experience-record-evaluation evaluation-b-spec)))
      (superchat-experience-record-relation
       (list :session-id "relation-b-session" :actor-id "agent:analyst"
             :correlation-id "promotion-thread-b" :relation 'applies-under
             :from candidate-b-ref :to context :scope scope
             :basis
             (list (list :session-id "evaluation-b-session"
                         :event-id (plist-get evaluation-b :event-id)))))
      (superchat-experience-record-disposition
       (list :session-id "promotion-session" :actor-id "user"
             :candidate candidate-a :scope scope :decision 'promote
             :basis (list (plist-get fixture :evaluation))
             :reason "Reviewed."))
      (superchat-experience-record-disposition
       (list :session-id "promotion-b-session" :actor-id "user"
             :candidate candidate-b-ref :scope scope :decision 'promote
             :basis
             (list (list :session-id "evaluation-b-session"
                         :event-id (plist-get evaluation-b :event-id)))
             :reason "Reviewed."))
      (when histories-p
        (let ((i 0))
          (dolist (step '(("search_text" ok) ("workspace_read" failed)
                          ("workspace_info" ok) ("workspace_read" ok)))
            (setq i (1+ i))
            (test-experience--append
             "recovery-session" "turn-hist-a" "run-hist-a"
             (if (eq (cadr step) 'ok) "tool.completed" "tool.failed")
             (format "evt-hist-a-%d" i) nil
             (list :tool-name (car step)))))
        (test-experience--append
         "recovery-session" "turn-hist-a" "run-hist-a"
         "experience.intervention.recorded" "evt-hist-a-iv" nil
         (list :trigger nil :scope scope :context context
               :receipt (list :candidate candidate-a)))
        (let ((i 0))
          (dolist (step '(("list_files" ok) ("workspace_read" failed)
                          ("workspace_info" ok) ("workspace_read" ok)))
            (setq i (1+ i))
            (test-experience--append
             "recovery-session" "turn-hist-b" "run-hist-b"
             (if (eq (cadr step) 'ok) "tool.completed" "tool.failed")
             (format "evt-hist-b-%d" i) nil
             (list :tool-name (car step)))))
        (test-experience--append
         "recovery-session" "turn-hist-b" "run-hist-b"
         "experience.intervention.recorded" "evt-hist-b-iv" nil
         (list :trigger nil :scope scope :context context
               :receipt (list :candidate candidate-b-ref))))
      ;; the current Run: search first (matches A's history), then the
      ;; typed failure
      (test-experience--append
       "recovery-session" "turn-now" "run-now"
       "tool.completed" "evt-now-search" nil
       '(:tool-name "search_text"))
      (test-experience--append
       "recovery-session" "turn-now" "run-now"
       "tool.failed" "evt-now-read-fail" nil
       '(:tool-name "workspace_read"
         :error-type "superchat-workspace-artifact-revision-error"))
      (let ((events (superchat-experience--view-events sessions)))
        (list :scope scope :context context
              :candidate-a candidate-a :candidate-b candidate-b-ref
              :sessions sessions
              :failure-event
              (car (superchat-experience--events-matching-ref
                    (list :session-id "recovery-session"
                          :event-id "evt-now-read-fail")
                    events)))))))

(ert-deftest experience/candidate-selection-off-keeps-view-blocked ()
  "With auto selection disabled, multiple candidates stay fail-closed."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--dual-candidate-fixture t))
           (superchat-experience-auto-candidate-selection nil)
           (view
            (superchat-experience-assemble-recovery-view
             (plist-get fixture :failure-event)
             (plist-get fixture :sessions))))
      (should (eq 'blocked (plist-get view :status)))
      (should (= 2 (length (plist-get view :eligible)))))))

(ert-deftest experience/candidate-selection-ranks-and-records-winner ()
  "A structurally discriminative prefix selects the matching candidate."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--dual-candidate-fixture t))
           (superchat-experience-auto-candidate-selection t)
           (view
            (superchat-experience-assemble-recovery-view
             (plist-get fixture :failure-event)
             (plist-get fixture :sessions))))
      (should (eq 'satisfied (plist-get view :status)))
      (should (superchat-experience--ref-equal
               (plist-get fixture :candidate-a)
               (plist-get (aref (plist-get view :items) 0) :candidate)))
      (should (equal "structural-rank"
                     (plist-get (plist-get view :receipt)
                                :selection-policy)))
      (let* ((events (superchat-experience--view-events
                      (plist-get fixture :sessions)))
             (ranking
              (cl-find-if
               (lambda (e)
                 (equal "experience.candidate-ranking.recorded"
                        (plist-get e :type)))
               events)))
        (should ranking)
        (should (equal "experience-auto" (plist-get ranking :actor-id)))
        (should (superchat-experience--ref-equal
                 (plist-get fixture :candidate-a)
                 (plist-get (plist-get ranking :payload) :winner)))))))

(ert-deftest experience/candidate-selection-fails-closed-on-ties ()
  "Without discriminating history, ties keep the View blocked."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)
                    (executable-find "gzip")))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--dual-candidate-fixture nil))
           (superchat-experience-auto-candidate-selection t)
           (view
            (superchat-experience-assemble-recovery-view
             (plist-get fixture :failure-event)
             (plist-get fixture :sessions))))
      (should (eq 'blocked (plist-get view :status)))
      (let* ((events (superchat-experience--view-events
                      (plist-get fixture :sessions))))
        (should-not
         (cl-some
          (lambda (e)
            (equal "experience.candidate-ranking.recorded"
                   (plist-get e :type)))
          events))))))

(ert-deftest experience/interactive-review-selects-and-records ()
  "The minibuffer reviewer shares the recording seam with the slash command."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (test-experience--failed-recovery-fixture)
    (let* ((before (superchat-experience-review-needs))
           (needs (append (plist-get before :open) nil))
           (need (car needs))
           (chosen nil))
      (should (= 1 (length needs)))
      (should (stringp (superchat-experience--review-label need)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (pcase (string-prefix-p "Judgment" prompt)
                     ('t (setq chosen "retain"))
                     (_ nil))
                   (car choices)))
                ((symbol-function 'read-string)
                 (lambda (_prompt) "interactive retain reason")))
        (superchat-review))
      (should (equal chosen "retain"))
      (should (= 0 (length (plist-get (superchat-experience-review-needs)
                                      :open)))))))

(ert-deftest experience/promoted-candidate-enters-view-and-revocation-removes-it ()
  "Only an active append-only Promotion may place a Candidate in a View."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (should (fboundp 'superchat-experience-record-disposition))
    (let* ((fixture (test-experience--promotion-fixture))
           (sessions (plist-get fixture :sessions))
           (scope (plist-get fixture :scope))
           (candidate (plist-get fixture :candidate))
           (context (plist-get fixture :context))
           (evaluation (plist-get fixture :evaluation))
           (before
            (superchat-experience-assemble-view sessions scope context)))
      (should (eq 'empty (plist-get before :status)))
      (should-not (plist-get before :prompt))
      (let* ((promotion
              (superchat-experience-record-disposition
               (list :session-id "promotion-session" :actor-id "user"
                     :candidate candidate :scope scope :decision 'promote
                     :basis (list evaluation)
                     :reason "Held-out evaluator passed.")))
             (promotion-ref
              (list :session-id "promotion-session"
                    :event-id (plist-get promotion :event-id)))
             (promoted
              (superchat-experience-assemble-view sessions scope context))
             (receipt (plist-get promoted :receipt)))
        (should (eq 'satisfied (plist-get promoted :status)))
        (should (string-match-p "Refresh before retrying E-R17"
                                (plist-get promoted :prompt)))
        (should (equal candidate (plist-get receipt :candidate)))
        (should (equal promotion-ref (plist-get receipt :promotion)))
        (superchat-experience-record-disposition
         (list :session-id "promotion-session" :actor-id "user"
               :candidate candidate :scope scope :decision 'revoke
               :prior promotion-ref :basis (list evaluation)
               :reason "Withdraw pending new evidence."))
        (let ((revoked
               (superchat-experience-assemble-view sessions scope context)))
          (should (eq 'empty (plist-get revoked :status)))
          (should-not (plist-get revoked :prompt)))))))

(ert-deftest experience/view-rebuilds-after-reopen-and-revocation ()
  "A reopened Projection must reflect Promotion and later Revocation."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (sessions (plist-get fixture :sessions))
           (scope (plist-get fixture :scope))
           (candidate (plist-get fixture :candidate))
           (context (plist-get fixture :context))
           (evaluation (plist-get fixture :evaluation))
           (promotion
            (superchat-experience-record-disposition
             (list :session-id "promotion-session" :actor-id "user"
                   :candidate candidate :scope scope :decision 'promote
                   :basis (list evaluation) :reason "Reopen test promotion.")))
           (promotion-ref
            (list :session-id "promotion-session"
                  :event-id (plist-get promotion :event-id))))
      (superchat-db-close)
      (superchat-db-open)
      (let ((reopened
             (superchat-experience-assemble-view sessions scope context)))
        (should (eq 'satisfied (plist-get reopened :status)))
        (superchat-experience-record-disposition
         (list :session-id "promotion-session" :actor-id "user"
               :candidate candidate :scope scope :decision 'revoke
               :prior promotion-ref :basis (list evaluation)
               :reason "Reopen test revocation.")))
      (superchat-db-close)
      (superchat-db-open)
      (let ((revoked
             (superchat-experience-assemble-view sessions scope context)))
        (should (eq 'empty (plist-get revoked :status)))
        (should-not (plist-get revoked :prompt))))))

(ert-deftest experience/promotion-rejects-agent-or-unrelated-success-before-append ()
  "Actor labels and unrelated success cannot manufacture a Promotion."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (base
            (list :session-id "promotion-session" :actor-id "user"
                  :candidate (plist-get fixture :candidate)
                  :scope (plist-get fixture :scope) :decision 'promote
                  :basis (list (plist-get fixture :evaluation))
                  :reason "Reviewed."))
           (count (test-experience--row-count)))
      (should-error
       (superchat-experience-record-disposition
        (plist-put (copy-tree base) :actor-id "agent:analyst")))
      (should (= count (test-experience--row-count)))
      (let* ((other-run
              (test-experience--closed-run "other-evaluation" "unrelated"))
             (other-evaluation
              (superchat-experience-record-evaluation
               (test-experience--evaluation-spec other-run)))
             (after-evaluation (test-experience--row-count)))
        (should-error
         (superchat-experience-record-disposition
          (plist-put
           (copy-tree base) :basis
           (list (list :session-id "other-evaluation"
                       :event-id (plist-get other-evaluation :event-id))))))
        (should (= after-evaluation (test-experience--row-count))))
      (should (< count (test-experience--row-count))))))

(ert-deftest experience/view-fails-closed-on-conflict-or-multiple-candidates ()
  "Conflicting applicability and ambiguous Promotions inject no Candidate."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (candidate (plist-get fixture :candidate))
           (context (plist-get fixture :context))
           (evaluation (plist-get fixture :evaluation))
           (scope (plist-get fixture :scope)))
      (superchat-experience-record-disposition
       (list :session-id "promotion-session" :actor-id "user"
             :candidate candidate :scope scope :decision 'promote
             :basis (list evaluation) :reason "Reviewed."))
      (superchat-experience-record-relation
       (list :session-id "relation-session" :actor-id "agent:analyst"
             :correlation-id "promotion-thread" :relation 'fails-under
             :from candidate :to context :scope scope
             :basis (list evaluation)))
      (let ((view
             (superchat-experience-assemble-view
              (plist-get fixture :sessions) scope context)))
        (should (eq 'blocked (plist-get view :status)))
        (should-not (plist-get view :prompt))
        (should (plist-get view :conflicts)))))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (run (plist-get fixture :run))
           (scope (plist-get fixture :scope))
           (context (plist-get fixture :context))
           (first (plist-get fixture :candidate))
           (first-evaluation (plist-get fixture :evaluation))
           (second-event
            (superchat-claim-record
             "Clear the cache before retrying E-R17." scope
             (list (plist-get run :output))
             "candidate-session" "agent:analyst" nil 'candidate))
           (second
            (list :session-id "candidate-session"
                  :event-id (plist-get second-event :event-id)))
           (evaluation-spec (test-experience--evaluation-spec run)))
      (setq evaluation-spec
            (plist-put evaluation-spec :evidence
                       (list (plist-get run :output) second)))
      (let* ((second-evaluation-event
              (superchat-experience-record-evaluation evaluation-spec))
             (second-evaluation
              (list :session-id "evaluation-session"
                    :event-id (plist-get second-evaluation-event :event-id))))
        (superchat-experience-record-relation
         (list :session-id "relation-session" :actor-id "agent:analyst"
               :correlation-id "promotion-thread-2" :relation 'applies-under
               :from second :to context :scope scope
               :basis (list second-evaluation)))
        (dolist (entry `((,first ,first-evaluation) (,second ,second-evaluation)))
          (superchat-experience-record-disposition
           (list :session-id "promotion-session" :actor-id "user"
                 :candidate (car entry) :scope scope :decision 'promote
                 :basis (list (cadr entry)) :reason "Reviewed.")))
        (let ((view
               (superchat-experience-assemble-view
                (plist-get fixture :sessions) scope context)))
          (should (eq 'blocked (plist-get view :status)))
          (should-not (plist-get view :prompt))
          (should
           (equal "multiple-candidates"
                  (plist-get (plist-get view :opportunity) :reason))))))))

(ert-deftest experience/view-fails-closed-on-evidence-change-during-assembly ()
  "A relation appended during View assembly cannot yield a stale Candidate."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (scope (plist-get fixture :scope))
           (candidate (plist-get fixture :candidate))
           (context (plist-get fixture :context))
           (evaluation (plist-get fixture :evaluation))
           (sessions (plist-get fixture :sessions))
           (original (symbol-function
                      'superchat-experience-applicability-graph))
           (injected nil)
           view)
      (superchat-experience-record-disposition
       (list :session-id "promotion-session" :actor-id "user"
             :candidate candidate :scope scope :decision 'promote
             :basis (list evaluation) :reason "Reviewed."))
      (cl-letf (((symbol-function 'superchat-experience-applicability-graph)
                 (lambda (&rest args)
                   (let ((graph (apply original args)))
                     (unless injected
                       (setq injected t)
                       (superchat-experience-record-relation
                        (list :session-id "relation-session"
                              :actor-id "agent:analyst"
                              :correlation-id "concurrent-revocation"
                              :relation 'fails-under :from candidate
                              :to context :scope scope
                              :basis (list evaluation))))
                     graph))))
        (setq view
              (superchat-experience-assemble-view sessions scope context)))
      (should (eq 'blocked (plist-get view :status)))
      (should-not (plist-get view :prompt))
      (should
       (equal "concurrent-evidence-update"
              (plist-get (plist-get view :opportunity) :reason))))))

(ert-deftest experience/view-rejects-a-disposition-that-bypassed-the-writer ()
  "Imported Agent-authored Promotions remain visible as invalid evidence."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((fixture (test-experience--promotion-fixture))
           (candidate (plist-get fixture :candidate))
           (scope (plist-get fixture :scope)))
      (superchat-tape-append-event-record
       (list :type "candidate.disposition.recorded" :schema 1
             :session-id "promotion-session" :actor-id "agent:analyst"
             :requires-payload t
             :payload
             (list :candidate candidate :scope scope :decision "promote"
                   :basis (vector (plist-get fixture :evaluation))
                   :reason "Forged.")))
      (let ((view
             (superchat-experience-assemble-view
              (plist-get fixture :sessions) scope
              (plist-get fixture :context))))
        (should (eq 'blocked (plist-get view :status)))
        (should-not (plist-get view :prompt))
        (should
         (eq 'invalid-disposition
             (plist-get (car (plist-get view :missing)) :reason)))))))

(ert-deftest experience/evaluation-requires-a-closed-turn-and-survives-restart ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((run (test-experience--closed-run "evaluation-session" "one"))
           (record
            (superchat-experience-record-evaluation
             (test-experience--evaluation-spec run)))
           (event-id (plist-get record :event-id)))
      (should (string-prefix-p "evt_" event-id))
      (let* ((event
              (cl-find event-id
                       (superchat-tape-replay-events "evaluation-session")
                       :key (lambda (candidate)
                              (plist-get candidate :event-id))
                       :test #'equal))
             (payload (plist-get event :payload)))
        (should (equal "exercise.evaluated" (plist-get event :type)))
        (should (equal "passed" (plist-get payload :verdict)))
        (should (equal (plist-get run :decision)
                       (plist-get payload :decision))))
      (superchat-experience-record-evaluation
       (test-experience--evaluation-spec run 'failed))
      (let ((bad-id "evt-incomplete-evaluator")
            (bad-envelope-id "evt-bad-evaluation-envelope"))
        (superchat-tape-append-event
         (list :type "exercise.evaluated" :schema 1 :event-id bad-id
               :session-id "evaluation-session" :actor-id "import"
               :requires-payload t
               :payload
               (list :exercise-id "exercise-opaque-1"
                     :scope "opaque-recovery/v0" :run-id (plist-get run :run-id)
                     :decision (plist-get run :decision)
                     :terminal (plist-get run :terminal)
                     :verdict "passed"
                     :evidence (vector (plist-get run :output)))))
        (let ((valid-payload
               (plist-get
                (cl-find event-id
                         (superchat-tape-replay-events "evaluation-session")
                         :key (lambda (candidate)
                                (plist-get candidate :event-id))
                         :test #'equal)
                :payload)))
          (superchat-tape-append-event
           (list :type "exercise.evaluated" :schema 1
                 :event-id bad-envelope-id
                 :session-id "evaluation-session" :actor-id "import"
                 :requires-payload t :payload valid-payload)))
        (let ((graph
               (superchat-experience-applicability-graph
                '("evaluation-session") "opaque-recovery/v0")))
          (should
           (cl-find bad-id (plist-get graph :missing)
                    :key (lambda (item) (plist-get item :event-id))
                    :test #'equal))
          (should
           (cl-find bad-envelope-id (plist-get graph :missing)
                    :key (lambda (item) (plist-get item :event-id))
                    :test #'equal))))
      (let ((before
             (superchat-experience-applicability-graph
              '("evaluation-session") "opaque-recovery/v0")))
        (should (= 1 (length (plist-get before :conflicts))))
        (superchat-db-close)
        (superchat-db-open)
        (should
         (equal before
                (superchat-experience-applicability-graph
                 '("evaluation-session") "opaque-recovery/v0")))))))

(ert-deftest experience/evaluation-fails-before-append-on-bad-evidence ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((run (test-experience--closed-run "evaluation-session" "bad"))
           (base (test-experience--evaluation-spec run))
           (count (test-experience--row-count)))
      (should-error
       (superchat-experience-record-evaluation
        (plist-put (copy-sequence base) :terminal
                   '(:session-id "evaluation-session"
                     :event-id "evt-missing"))))
      (should-error
       (superchat-experience-record-evaluation
        (plist-put (copy-sequence base) :metrics
                   (list :bad (current-buffer)))))
      (should-error
       (superchat-experience-record-evaluation
        (append base '(:verdict failed))))
      (should (= count (test-experience--row-count))))
    (let* ((session-id "open-session")
           (turn-id "turn-open")
           (run-id "run-open")
           (intent "evt-open-intent")
           (start "evt-open-start")
           (decision "evt-open-decision")
           (terminal "evt-fake-terminal"))
      (test-experience--append
       session-id turn-id run-id "intent.received" intent nil nil)
      (test-experience--append
       session-id turn-id run-id "run.started" start intent
       '(:status running-model :actor-id "lead" :capability "chat"))
      (test-experience--append
       session-id turn-id run-id "decision.snapshot.recorded" decision start
       '(:attempt 1))
      (test-experience--append
       session-id turn-id run-id "run.completed" terminal decision
       '(:status completed))
      (let ((count (test-experience--row-count)))
        (should-error
         (superchat-experience-record-evaluation
          (list :session-id session-id :actor-id "evaluator:test"
                :exercise-id "exercise-open" :scope "scope"
                :run-id run-id
                :decision (list :session-id session-id :event-id decision)
                :terminal (list :session-id session-id
                                :event-id terminal)
                :evaluator '(:id "rubric" :version 1)
                :evaluator-manifest-sha256
                (concat "sha256:" (make-string 64 ?b))
                :verdict 'passed :observations nil :metrics nil
                :evidence
                (list (list :session-id session-id :event-id decision)))))
        (should (= count (test-experience--row-count)))))))

(ert-deftest experience/evaluation-rejects-audit-incomplete-runtime-evidence ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((session-id "audit-session")
           (turn-id "turn-audit")
           (run-id "run-audit")
           (intent "evt-audit-intent")
           (start "evt-audit-start")
           (decision "evt-audit-decision")
           (terminal "evt-audit-terminal")
           (turn-terminal "evt-audit-turn"))
      (test-experience--append
       session-id turn-id run-id "intent.received" intent nil nil)
      (test-experience--append
       session-id turn-id run-id "run.started" start intent
       '(:status running-model :actor-id "lead" :capability "chat"))
      (test-experience--append
       session-id turn-id run-id "decision.snapshot.recorded" decision start
       '(:attempt 1))
      (test-experience--append
       session-id turn-id run-id "run.failed" terminal decision
       '(:status audit-incomplete :audit-incomplete t))
      (test-experience--append
       session-id turn-id run-id "turn.completed" turn-terminal terminal
       '(:status audit-incomplete))
      (let ((count (test-experience--row-count)))
        (should-error
         (superchat-experience-record-evaluation
          (list :session-id session-id :actor-id "evaluator:test"
                :exercise-id "exercise-audit" :scope "scope"
                :run-id run-id
                :decision (list :session-id session-id :event-id decision)
                :terminal (list :session-id session-id :event-id terminal)
                :evaluator '(:id "rubric" :version 1)
                :evaluator-manifest-sha256
                (concat "sha256:" (make-string 64 ?d))
                :verdict 'not-identifiable :observations nil :metrics nil
                :evidence
                (list (list :session-id session-id :event-id decision)))))
        (should (= count (test-experience--row-count)))))))

(ert-deftest experience/typed-claims-and-opposite-relations-form-a-conflict ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((scope "opaque-recovery/v0")
           (run (test-experience--closed-run "evaluation-session" "graph"))
           (other-run
            (test-experience--closed-run "other-evaluation-session" "graph"))
           (evaluation
            (superchat-experience-record-evaluation
             (test-experience--evaluation-spec run)))
           (evaluation-ref
            (list :session-id "evaluation-session"
                  :event-id (plist-get evaluation :event-id)))
           (spaced-evaluation-ref
            (list :session-id " evaluation-session "
                  :event-id
                  (format " %s " (plist-get evaluation :event-id))))
           (pattern
            (superchat-claim-record
             "E-R17 requires refresh before retry." scope
             (list spaced-evaluation-ref)
             "pattern-session" "agent:analyst" nil
             'pattern))
           (context
            (superchat-claim-record
             "The failure code is E-R17." scope
             (list evaluation-ref) "context-session" "agent:analyst" nil
             'context))
           (from (list :session-id "pattern-session"
                       :event-id (plist-get pattern :event-id)))
           (to (list :session-id "context-session"
                     :event-id (plist-get context :event-id)))
           (relation-base
            (list :session-id "graph-session" :actor-id "agent:analyst"
                  :correlation-id "thread-1" :from from :to to
                  :scope scope :basis (list evaluation-ref)))
           (bad-evaluation-id "evt-cross-session-evaluation"))
      (should
       (equal (list evaluation-ref)
              (plist-get
               (car (superchat-claims-replay "pattern-session" scope))
               :basis)))
      (superchat-feedback-record
       (plist-get pattern :event-id) 'support (list evaluation-ref)
       "Observed support." "pattern-session" "agent:analyst")
      (superchat-feedback-record
       (plist-get pattern :event-id) 'challenge (list evaluation-ref)
       "Observed counterexample." "pattern-session" "agent:analyst")
      (superchat-experience-record-relation
       (append relation-base '(:relation applies-under)))
      (superchat-experience-record-relation
       (append relation-base '(:relation fails-under)))
      ;; Bypass the writer: a same-named Run in another Session is not valid
      ;; evidence for this Session and must not create an Outcome conflict.
      (superchat-tape-append-event
       (list :type "exercise.evaluated" :schema 1
             :event-id bad-evaluation-id :session-id "evaluation-session"
             :actor-id "import" :requires-payload t
             :payload
             (list :exercise-id "exercise-opaque-1" :scope scope
                   :run-id (plist-get run :run-id)
                   :decision (plist-get other-run :decision)
                   :terminal (plist-get other-run :terminal)
                   :evaluator '(:id "import" :version 1)
                   :evaluator-manifest-sha256
                   (concat "sha256:" (make-string 64 ?e))
                   :verdict "failed" :observations nil :metrics nil
                   :evidence (vector (plist-get other-run :output)))))
      ;; The same logical ID in another Session remains a different node.
      (superchat-tape-append-event
       (list :type "claim.recorded" :schema 1
             :event-id (plist-get pattern :event-id)
             :session-id "collision-session" :actor-id "import"
             :requires-payload t
             :payload (list :statement "Unrelated" :scope scope
                            :kind "pattern" :basis nil)))
      (let* ((sessions '("evaluation-session" "pattern-session"
                         "context-session" "graph-session"
                         "collision-session" "other-evaluation-session"))
             (graph
              (superchat-experience-applicability-graph sessions scope))
             (before graph))
        (should (= 2 (length (plist-get graph :relations))))
        (should (= 2 (length (plist-get graph :conflicts))))
        (should
         (cl-find bad-evaluation-id (plist-get graph :missing)
                  :key (lambda (item) (plist-get item :event-id))
                  :test #'equal))
        (should
         (= 2 (cl-count "feedback.recorded" (plist-get graph :nodes)
                        :key (lambda (node) (plist-get node :type))
                        :test #'equal)))
        (should
         (= 2
            (cl-count (plist-get pattern :event-id)
                      (plist-get graph :nodes)
                      :key (lambda (node)
                             (plist-get (plist-get node :ref) :event-id))
                      :test #'equal)))
        (superchat-db-close)
        (superchat-db-open)
        (should
         (equal before
                (superchat-experience-applicability-graph sessions scope)))))))

(ert-deftest experience/relation-writer-rejects-wrong-session-and-kind ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((scope "scope")
           (fact
            (superchat-tape-append-material "facts" "user" "observation"))
           (pattern
            (superchat-claim-record "Pattern" scope nil "patterns" "agent"
                                    nil 'pattern))
           (generic
            (superchat-claim-record "Generic" scope nil "contexts" "agent"))
           (base
            (list :session-id "graph" :actor-id "agent"
                  :correlation-id "thread" :relation 'applies-under
                  :from (list :session-id "patterns"
                              :event-id (plist-get pattern :event-id))
                  :to (list :session-id "contexts"
                            :event-id (plist-get generic :event-id))
                  :scope scope
                  :basis (list (list :session-id "wrong-facts"
                                     :storage-id fact))))
           (count (test-experience--row-count)))
      (should-error (superchat-experience-record-relation base))
      (should-error
       (superchat-experience-record-relation
        (plist-put (copy-sequence base) :basis
                   (list (list :session-id "facts" :storage-id fact)))))
      (should-error
       (superchat-experience-record-relation
        (append base '(:relation fails-under))))
      (should (= count (test-experience--row-count))))))

(ert-deftest experience/graph-preserves-history-and-reports-dangling-data ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((scope "scope")
           (pattern
            (superchat-claim-record "Old Pattern" scope nil "claims" "agent"
                                    nil 'pattern))
           (replacement
            (plist-get
             (superchat-claim-correct
              (plist-get pattern :event-id) "New Pattern" scope nil
              "Correction" "claims" "human")
             :replacement))
           (context
            (superchat-claim-record "Context" scope nil "contexts" "agent"
                                    nil 'context))
           (other-claim
            (superchat-claim-record "Other scope" "other-scope" nil
                                    "claims" "agent" nil 'pattern))
           (from (list :session-id "claims"
                       :event-id (plist-get pattern :event-id)))
           (missing-to '(:session-id "missing" :event-id "evt-absent"))
           (bad-evaluation-id "evt-imported-evaluation")
           (other-feedback-id "evt-other-scope-feedback")
           (malformed-feedback-id "evt-malformed-feedback"))
      ;; Bypass the writer to prove replay exposes imported/corrupt edges.
      (superchat-tape-append-event
       (list :type "applicability.relation.recorded" :schema 1
             :session-id "graph" :actor-id "import"
             :requires-payload t
             :payload (list :relation "applies-under" :from from
                            :to missing-to :scope scope
                            :basis
                            (vector
                             (list :session-id "contexts"
                                   :event-id
                                   (plist-get context :event-id))))))
      (superchat-tape-append-event
       (list :type "exercise.evaluated" :schema 1
             :event-id bad-evaluation-id :session-id "graph"
             :actor-id "import" :requires-payload t
             :payload
             (list :exercise-id "imported" :scope scope :run-id "run-missing"
                   :decision missing-to :terminal missing-to
                   :evaluator '(:id "import" :version 1)
                   :evaluator-manifest-sha256
                   (concat "sha256:" (make-string 64 ?c))
                   :verdict "passed" :observations nil :metrics nil
                   :evidence (vector missing-to))))
      (superchat-tape-append-event
       (list :type "feedback.recorded" :schema 1
             :event-id other-feedback-id :session-id "claims"
             :actor-id "import" :requires-payload t
             :payload
             (list :target (plist-get other-claim :event-id)
                   :kind "challenge" :note "Other scope"
                   :basis (vector missing-to))))
      (superchat-tape-append-event
       (list :type "feedback.recorded" :schema 1
             :event-id malformed-feedback-id :session-id "claims"
             :actor-id "import" :requires-payload t
             :payload
             (list :target (plist-get pattern :event-id)
                   :kind "support" :note "Malformed" :basis "oops")))
      (let* ((graph
              (superchat-experience-applicability-graph
               '("claims" "contexts" "graph" "missing") scope))
             (claim-ids
              (mapcar
               (lambda (node) (plist-get (plist-get node :ref) :event-id))
               (cl-remove-if-not
                (lambda (node)
                  (equal "claim.recorded" (plist-get node :type)))
                (plist-get graph :nodes)))))
        (should (member (plist-get pattern :event-id) claim-ids))
        (should (member (plist-get replacement :event-id) claim-ids))
        (should
         (cl-find "feedback.recorded" (plist-get graph :nodes)
                  :key (lambda (node) (plist-get node :type))
                  :test #'equal))
        (should (= 1 (length (plist-get graph :relations))))
        (should
         (eq 'dangling-event
             (plist-get (cl-find 'to (plist-get graph :missing)
                                 :key (lambda (item) (plist-get item :role)))
                        :reason)))
        (should
         (cl-find bad-evaluation-id (plist-get graph :missing)
                  :key (lambda (item) (plist-get item :event-id))
                  :test #'equal))
        (should
         (cl-find malformed-feedback-id (plist-get graph :missing)
                  :key (lambda (item) (plist-get item :event-id))
                  :test #'equal))
        (should-not
         (cl-find other-feedback-id (plist-get graph :missing)
                  :key (lambda (item) (plist-get item :event-id))
                  :test #'equal))))))

(ert-deftest experience/projection-rebuilds-empty-state ()
  (should
   (equal '(:nodes nil :relations nil)
          (superchat-projection-replay 'experience-applicability nil))))

(provide 'test-experience)

;;; test-experience.el ends here
