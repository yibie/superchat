;;; product-harness-prototype-v2a.el --- Scratch product harness -*- lexical-binding: t; -*-

;; PROTOTYPE: Can public Superchat seams produce the sealed 16-record V2a
;; cohort from a fresh Tape, and does the evaluator reject every broken join?

(require 'cl-lib)
(require 'superchat)
(require 'superchat-claims)
(require 'superchat-experience)
(require 'superchat-tape-view)

(defvar semuse-v2a-harness-library-mode nil)

(load (expand-file-name
       "evaluator-logic-v2a.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(define-error 'semuse-v2a-opaque-boundary "Opaque resource boundary")

(defconst semuse-v2a-harness--event-types
  '("claim.recorded" "exercise.evaluated"
    "applicability.relation.recorded" "candidate.disposition.recorded"
    "tool.failed" "decision.snapshot.recorded" "tool.requested"))

(defun semuse-v2a-harness--ref (session event)
  "Return a typed reference to EVENT in SESSION."
  (list :session-id session :event-id (plist-get event :event-id)))

(defun semuse-v2a-harness--find (events type &optional predicate)
  "Return the unique TYPE event matching PREDICATE in EVENTS."
  (let ((matches
         (cl-remove-if-not
          (lambda (event)
            (and (equal type (plist-get event :type))
                 (or (null predicate) (funcall predicate event))))
          events)))
    (unless (= 1 (length matches))
      (error "Expected one %s event, found %d" type (length matches)))
    (car matches)))

(defun semuse-v2a-harness--prepared-envelope
    (reason message tools context-envelope)
  "Return the same sealed evidence shape as the product transport."
  (list :format "superchat-prepared-envelope-v0"
        :attempt-reason reason :system-prompt ""
        :messages (vector (list :role "user" :content-format "text"
                                :content message :tool-results []
                                :multi-turn nil))
        :tools tools :context-files []
        :view-envelope
        (list :contract (plist-get context-envelope :contract)
              :status (plist-get context-envelope :status)
              :receipt (copy-tree (plist-get context-envelope :receipt) t)
              :disclosure (copy-tree (plist-get context-envelope :disclosure) t)
              :experience-view
              (copy-tree (plist-get context-envelope :experience-view) t))
        :options (list :stream t)))

(defun semuse-v2a-harness--origin (attempt snapshot call-id tool args)
  "Extend the active Runtime context with one fake-provider tool origin."
  (append (copy-sequence superchat--tool-execution-context)
          (list :evidence-attempt attempt
                :decision-snapshot-event-id snapshot
                :tool-call-id call-id :tool-call-name tool
                :tool-call-ordinal 1 :tool-call-arguments args)))

(defun semuse-v2a-harness--run-calibration (entry workspace)
  "Produce one closed calibration Run for ENTRY in WORKSPACE."
  (let* ((position (plist-get entry :position))
         (session (semuse-v2a--opaque "evaluation-session" position))
         (context-envelope
          (list :contract "context-continuity-v0" :status 'empty
                :receipt (list :tape-head 0)))
         handle)
    (cl-letf
        (((symbol-function 'superchat--update-status) #'ignore)
         ((symbol-function 'superchat--llm-generate-answer)
          (lambda (&rest _args)
            (funcall
             superchat--llm-runtime-observer 'decision-snapshot
             (list :attempt 1 :attempt-reason "initial"
                   :provider "deterministic-local"
                   :model "calibration-actor-v0"
                   :prepared-envelope
                   (semuse-v2a-harness--prepared-envelope
                    "initial" "Evaluate the calibration evidence." []
                    context-envelope)))
            (funcall superchat--llm-runtime-observer 'first-output
                     '(:attempt 1 :kind "text" :text "passed"))
            (funcall superchat--llm-runtime-observer 'terminal
                     '(:status completed :response "passed")))))
      (setq handle
            (superchat-runtime-submit
             (list :type :llm-query :input "calibrate" :clean-input "calibrate"
                   :prompt "Evaluate the calibration evidence."
                   :session-id session :workspace-id workspace
                   :buffer-name " *semuse-v2a-product-harness*"
                   :evidence-contract "epistemic-learning-v0"
                   :context-envelope context-envelope))))
    (let* ((run-id (plist-get handle :run-id))
           (events (superchat-tape-replay-events session))
           (decision
            (semuse-v2a-harness--find
             events "decision.snapshot.recorded"
             (lambda (event) (equal run-id (plist-get event :run-id)))))
           (output
            (semuse-v2a-harness--find
             events "output.first.recorded"
             (lambda (event) (equal run-id (plist-get event :run-id)))))
           (terminal
            (semuse-v2a-harness--find
             events "run.completed"
             (lambda (event) (equal run-id (plist-get event :run-id))))))
      (list :session-id session :run-id run-id
            :decision (semuse-v2a-harness--ref session decision)
            :output (semuse-v2a-harness--ref session output)
            :terminal (semuse-v2a-harness--ref session terminal)))))

(defun semuse-v2a-harness--actor-tool (code view)
  "Choose the first Action from CODE and the product Experience VIEW."
  (let ((prompt (plist-get view :prompt)))
    (cond
     ((not (stringp prompt)) "report_uncertain")
     ((string-match-p "E-X99" prompt) "report_uncertain")
     ((string-match-p (format "%s: refresh_handle" code) prompt)
      "refresh_handle")
     ((string-match-p (format "%s: report_termination" code) prompt)
      "report_termination")
     (t "report_uncertain"))))

(defun semuse-v2a-harness--run-consumer
    (entry workspace session context-envelope view)
  "Run the deterministic two-attempt consumer through Runtime and Authority."
  (let* ((position (plist-get entry :position))
         (code (plist-get entry :error-code))
         (tool (semuse-v2a-harness--actor-tool code view))
         (tools
          [(:name "refresh_handle" :description "Refresh a handle."
            :args [] :async nil)
           (:name "report_termination"
            :description "Report a terminal boundary." :args [] :async nil)
           (:name "report_uncertain"
            :description "Report insufficient guidance." :args [] :async nil)])
         handle)
    (cl-letf
        (((symbol-function 'superchat--update-status) #'ignore)
         ((symbol-function 'superchat--llm-generate-answer)
          (lambda (&rest _args)
            (let ((first
                   (funcall
                    superchat--llm-runtime-observer 'decision-snapshot
                    (list :attempt 1 :attempt-reason "initial"
                          :provider "deterministic-local"
                          :model "recovery-actor-v0"
                          :prepared-envelope
                          (semuse-v2a-harness--prepared-envelope
                           "initial" "Read the opaque resource." tools
                           context-envelope)))))
              (superchat-tool-execute
               "read_resource"
               (lambda (_code)
                 (signal 'semuse-v2a-opaque-boundary (list :code code)))
               (list code)
               :context
               (semuse-v2a-harness--origin
                1 first (semuse-v2a--opaque "read-call" position)
                "read_resource" (list :code code))))
            (let* ((message (format "The read failed with opaque code %s. Continue."
                                    code))
                   (second
                    (funcall
                     superchat--llm-runtime-observer 'decision-snapshot
                     (list :attempt 2 :attempt-reason "tool-followup"
                           :provider "deterministic-local"
                           :model "recovery-actor-v0"
                           :prepared-envelope
                           (semuse-v2a-harness--prepared-envelope
                            "tool-followup" message tools context-envelope)))))
              (superchat-tool-execute
               tool (lambda (_code) "ok") (list code)
               :context
               (semuse-v2a-harness--origin
                2 second (semuse-v2a--opaque "action-call" position)
                tool (list :code code))))
            (funcall superchat--llm-runtime-observer 'terminal
                     '(:status completed :response "done")))))
      (setq handle
            (superchat-runtime-submit
             (list :type :llm-query :input "recover" :clean-input "recover"
                   :prompt "Recover from the opaque failure."
                   :session-id session :workspace-id workspace
                   :buffer-name " *semuse-v2a-product-harness*"
                   :attendance 'attended :authority-policy 'cowork
                   :approval-function (lambda (_request) t)
                   :evidence-contract "epistemic-learning-v0"
                   :context-envelope context-envelope))))
    (let* ((run-id (plist-get handle :run-id))
           (events (superchat-tape-replay-events session))
           (trigger
            (semuse-v2a-harness--find
             events "tool.failed"
             (lambda (event) (equal run-id (plist-get event :run-id)))))
           (decision
            (semuse-v2a-harness--find
             events "decision.snapshot.recorded"
             (lambda (event)
               (and (equal run-id (plist-get event :run-id))
                    (= 2 (plist-get (plist-get event :payload) :attempt))))))
           (action
            (semuse-v2a-harness--find
             events "tool.requested"
             (lambda (event)
               (and (equal run-id (plist-get event :run-id))
                    (= 2 (or (plist-get (plist-get event :payload)
                                        :evidence-attempt)
                             0)))))))
      (list :trigger (semuse-v2a-harness--ref session trigger)
            :decision (semuse-v2a-harness--ref session decision)
            :action (semuse-v2a-harness--ref session action)))))

(defun semuse-v2a-harness--array-fields (event)
  "Restore typed JSON arrays in replayed product EVENT."
  (let ((event (copy-tree event t)))
    (pcase (plist-get event :type)
      ((or "claim.recorded" "applicability.relation.recorded"
           "candidate.disposition.recorded")
       (when (plist-member (plist-get event :payload) :basis)
         (plist-put (plist-get event :payload) :basis
                    (vconcat (plist-get (plist-get event :payload) :basis)))))
      ("exercise.evaluated"
       (plist-put (plist-get event :payload) :evidence
                  (vconcat (plist-get (plist-get event :payload) :evidence)))))
    event))

(defun semuse-v2a-harness--closed-event (event)
  "Normalize replayed EVENT to the evaluator's closed event envelope."
  (setq event (semuse-v2a-harness--array-fields event))
  (cl-loop for key in semuse-v2a--event-fields
           append (list key (plist-get event key))))

(defun semuse-v2a-harness--resolve (reference events)
  "Resolve typed REFERENCE once from replayed EVENTS."
  (or (semuse-v2a--resolve reference events)
      (error "Missing replayed event %S" reference)))

(defun semuse-v2a-harness--record (entry)
  "Produce one evaluator record entirely through public product seams."
  (let* ((position (plist-get entry :position))
         (workspace (semuse-v2a--opaque "workspace" position))
         (consumer (semuse-v2a--opaque "session" position))
         (candidate-session (semuse-v2a--opaque "candidate-session" position))
         (relation-session (semuse-v2a--opaque "relation-session" position))
         (promotion-session (semuse-v2a--opaque "promotion-session" position))
         (statement (plist-get entry :candidate-statement))
         calibration candidate evaluation relation promotion)
    (when statement
      (setq calibration (semuse-v2a-harness--run-calibration entry workspace)
            candidate
            (superchat-claim-record
             statement semuse-v2a--scope (list (plist-get calibration :output))
             candidate-session "agent:analyst" nil 'candidate))
      (let* ((candidate-ref (semuse-v2a-harness--ref candidate-session candidate))
             (evaluation-spec
              (list :session-id (plist-get calibration :session-id)
                    :actor-id "evaluator:test"
                    :exercise-id (format "calibration-%02d" position)
                    :scope semuse-v2a--scope
                    :run-id (plist-get calibration :run-id)
                    :decision (plist-get calibration :decision)
                    :terminal (plist-get calibration :terminal)
                    :evaluator '(:id "deterministic-rubric" :version 1)
                    :evaluator-manifest-sha256
                    (concat "sha256:" (make-string 64 ?a))
                    :verdict 'passed :observations '(:first-action "report")
                    :metrics '(:score 1)
                    :evidence (list (plist-get calibration :output)
                                    candidate-ref))))
        (setq evaluation
              (superchat-experience-record-evaluation evaluation-spec))))
    (let* ((candidate-ref
            (and candidate (semuse-v2a-harness--ref candidate-session candidate)))
           (evaluation-ref
            (and evaluation
                 (semuse-v2a-harness--ref
                  (plist-get calibration :session-id) evaluation)))
           (context
            (superchat-claim-record
             (format "Observed %s in held-out instance %d."
                     (plist-get entry :error-code) (plist-get entry :instance))
             semuse-v2a--scope nil consumer "agent:analyst" nil 'context))
           (context-ref (semuse-v2a-harness--ref consumer context)))
      (when candidate
        (setq relation
              (superchat-experience-record-relation
               (list :session-id relation-session :actor-id "agent:analyst"
                     :correlation-id (semuse-v2a--opaque "thread" position)
                     :relation 'applies-under :from candidate-ref :to context-ref
                     :scope semuse-v2a--scope :basis (list evaluation-ref)))
              promotion
              (superchat-experience-record-disposition
               (list :session-id promotion-session :actor-id "user"
                     :candidate candidate-ref :scope semuse-v2a--scope
                     :decision 'promote :basis (list evaluation-ref)
                     :reason "Deterministic held-out gate passed."))))
      (let* ((sessions
              (delq nil
                    (list (and calibration (plist-get calibration :session-id))
                          (and candidate candidate-session) consumer
                          (and relation relation-session)
                          (and promotion promotion-session))))
             (view (superchat-experience-assemble-view
                    sessions semuse-v2a--scope context-ref))
             (continuity (superchat-view-assemble consumer))
             (_ (setq continuity
                      (plist-put continuity :experience-view
                                 (copy-tree view t))))
             (runtime-refs
              (semuse-v2a-harness--run-consumer
               entry workspace consumer continuity view))
             (references
              (delq nil
                    (list candidate-ref evaluation-ref context-ref
                          (and relation
                               (semuse-v2a-harness--ref relation-session relation))
                          (and promotion
                               (semuse-v2a-harness--ref promotion-session promotion))
                          (plist-get runtime-refs :trigger)
                          (plist-get runtime-refs :decision)
                          (plist-get runtime-refs :action))))
             (events
              (apply #'append
                     (mapcar #'superchat-tape-replay-events sessions)))
             (selected
              (mapcar (lambda (reference)
                        (semuse-v2a-harness--closed-event
                         (semuse-v2a-harness--resolve reference events)))
                      references)))
        (list :contract-revision "v2a"
              :assignment-id (plist-get entry :assignment-id)
              :session-id consumer :workspace-id workspace
              :events (vconcat selected)
              :trigger-error (plist-get runtime-refs :trigger)
              :decision (plist-get runtime-refs :decision)
              :first-action (plist-get runtime-refs :action))))))

(defun semuse-v2a-harness--build-records ()
  "Build the sealed cohort from an empty current Tape."
  (mapcar #'semuse-v2a-harness--record
          (plist-get semuse-v2a-sealed-evaluator-manifest :schedule)))

(defun semuse-v2a-product-harness-audit ()
  "Audit the sealed cohort and broken joins through the scratch product harness."
  (let* ((directory (make-temp-file "semuse-v2a-product-harness-" t))
         (superchat-data-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         result product-audit mismatches row-count)
    (unwind-protect
        (progn
          (superchat-db-close)
          (superchat-db-open)
          (let ((records (semuse-v2a-harness--build-records)))
            (setq row-count
                  (caar (superchat-db-tape-select "SELECT COUNT(*) FROM tape")))
            (setq result
                  (semuse-v2a-score
                   records semuse-v2a-sealed-evaluator-manifest))
            (setq product-audit
                  (semuse-v2a-audit
                   nil
                   (list :records records
                         :evaluator-manifest
                         semuse-v2a-sealed-evaluator-manifest)))
            (cl-mapc
             (lambda (record entry)
               (when-let* ((expected (plist-get entry :candidate-statement))
                           (candidate
                            (semuse-v2a--first-event record "claim.recorded"))
                           (actual
                            (plist-get (plist-get candidate :payload)
                                       :statement)))
                 (unless (equal expected actual)
                   (push (list :assignment-id (plist-get entry :assignment-id)
                               :expected-length (length expected)
                               :actual-length (length actual)
                               :trimmed-equal (equal (string-trim expected)
                                                     actual))
                         mismatches))))
             records
             (plist-get semuse-v2a-sealed-evaluator-manifest :schedule))))
      (superchat-db-close)
      (when-let* ((buffer (get-buffer " *semuse-v2a-product-harness*")))
        (kill-buffer buffer))
      (delete-directory directory t))
    (setq mismatches (nreverse mismatches))
    (list :ok (and (plist-get result :ok)
                   (eq 'consistent-with-candidate-specific-action
                       (plist-get result :status))
                   (null mismatches)
                   (plist-get product-audit :ok))
          :assignment-count 16 :tape-row-count row-count
          :mismatches mismatches :product-audit product-audit :result result)))

(defun semuse-v2a-harness--print (audit)
  "Print stable AUDIT results."
  (princ (format "audit=%s\n" (if (plist-get audit :ok) "PASS" "FAIL")))
  (princ (format "assignments=%d\n" (plist-get audit :assignment-count)))
  (princ (format "scratch-tape-rows=%d\n" (plist-get audit :tape-row-count)))
  (princ (format "candidate-byte-mismatches=%d\n"
                 (length (plist-get audit :mismatches))))
  (princ (format "product-checks=%d\n"
                 (length (plist-get (plist-get audit :product-audit) :checks))))
  (let ((result (plist-get audit :result)))
    (princ (format "stage=%s\nstatus=%s\nerrors=%S\n"
                   (plist-get result :stage) (plist-get result :status)
                   (plist-get result :errors)))))

(unless semuse-v2a-harness-library-mode
  (pcase command-line-args-left
    (`("audit")
     (let ((audit (semuse-v2a-product-harness-audit)))
       (semuse-v2a-harness--print audit)
       (kill-emacs (if (plist-get audit :ok) 0 1))))
    (_
     (princ "usage: product-harness-prototype-v2a.el audit\n")
     (kill-emacs 2))))

;;; product-harness-prototype-v2a.el ends here
