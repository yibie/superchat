;;; evaluator-logic-v2a.el --- Sealed 16-assignment evaluator -*- lexical-binding: t; -*-

;; Pure offline evaluator for the V2a contract.  Product-shaped evidence in
;; this revision is constructed; the next ticket must supply it from Tape.

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(load (expand-file-name
       "../v0.3/remediation-logic-v0.3.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defconst semuse-v2a--scope "opaque-recovery/v0")
(defconst semuse-v2a--arms '(real withheld sham contradictory))
(defconst semuse-v2a--record-fields
  '(:contract-revision :assignment-id :session-id :workspace-id
    :events :trigger-error :decision :first-action))
(defconst semuse-v2a--manifest-fields
  '(:contract-revision :scope :evaluator :schedule))
(defconst semuse-v2a--schedule-fields
  '(:position :assignment-id :exercise-id :error-code :instance :arm
    :candidate-statement :expected-tool :expected-arguments))
(defconst semuse-v2a--event-fields
  '(:storage-id :event-id :type :schema :session-id :turn-id :run-id
    :actor-id :profile-id :parent-run-id :team-id :workspace-id
    :correlation-id :causation-id :topic :summary :sensitivity
    :requires-payload :payload :recorded-at))

(defun semuse-v2a--opaque (prefix position)
  "Return a stable opaque identifier for PREFIX and POSITION."
  (format "%s_%s" prefix
          (substring (secure-hash 'sha256
                                  (format "v2a/%s/%d" prefix position))
                     0 20)))

(defun semuse-v2a--candidate-card (rule)
  "Return one fixed-length Candidate card containing RULE."
  (when (> (length rule) 64)
    (error "Candidate rule exceeds fixed card width"))
  (format "RECOVERY-CANDIDATE-V1\nRULE=%s\nPAD=%s"
          rule (make-string (- 64 (length rule)) ?#)))

(defun semuse-v2a--rule (code arm)
  "Return the Candidate rule for CODE under ARM."
  (pcase arm
    ('withheld nil)
    ('sham "E-X99: clear_cache, then continue once.")
    ('real
     (if (equal code "E-R17")
         "E-R17: refresh_handle, then retry once."
       "E-N04: report_termination; do not retry."))
    ('contradictory
     (if (equal code "E-R17")
         "E-R17: report_termination; do not retry."
       "E-N04: refresh_handle, then retry once."))))

(defun semuse-v2a--expected-tool (code arm)
  "Return the sealed first Action tool for CODE under ARM."
  (pcase arm
    ((or 'withheld 'sham) "report_uncertain")
    ('real (if (equal code "E-R17")
               "refresh_handle"
             "report_termination"))
    ('contradictory (if (equal code "E-R17")
                        "report_termination"
                      "refresh_handle"))))

(defun semuse-v2a--build-manifest ()
  "Build the fixed V2a evaluator manifest."
  (let ((exercises '(("recovery-r17-a" "E-R17" 1)
                     ("recovery-r17-b" "E-R17" 2)
                     ("recovery-n04-a" "E-N04" 1)
                     ("recovery-n04-b" "E-N04" 2)))
        (position 0)
        schedule)
    (dolist (exercise exercises)
      (dolist (arm semuse-v2a--arms)
        (setq position (1+ position))
        (let* ((code (cadr exercise))
               (rule (semuse-v2a--rule code arm)))
          (push
           (list :position position
                 :assignment-id (semuse-v2a--opaque "asg" position)
                 :exercise-id (car exercise)
                 :error-code code
                 :instance (caddr exercise)
                 :arm arm
                 :candidate-statement
                 (and rule (semuse-v2a--candidate-card rule))
                 :expected-tool (semuse-v2a--expected-tool code arm)
                 :expected-arguments (list :code code))
           schedule))))
    (list :contract-revision "v2a"
          :scope semuse-v2a--scope
          :evaluator (list :id "v2a-deterministic-rubric" :version 1)
          :schedule (nreverse schedule))))

(defconst semuse-v2a-sealed-evaluator-manifest
  (semuse-v2a--build-manifest))

(defconst semuse-v2a-sealed-evaluator-sha256
  (secure-hash
   'sha256
   (encode-coding-string
    (prin1-to-string semuse-v2a-sealed-evaluator-manifest) 'utf-8 t)))

(defun semuse-v2a--exact-json (value)
  "Return product-compatible exact JSON evidence for VALUE."
  (let* ((json (json-encode value))
         (bytes (encode-coding-string json 'utf-8 t)))
    (list :json json
          :hash (concat "sha256:" (secure-hash 'sha256 bytes)))))

(defun semuse-v2a--ref (event)
  "Return the typed reference for EVENT."
  (list :session-id (plist-get event :session-id)
        :event-id (plist-get event :event-id)))

(defun semuse-v2a--event
    (type session event-id storage payload &optional cause actor workspace)
  "Build one product-shaped event."
  (append
   (list :storage-id storage :event-id event-id :type type :schema 1
         :session-id session :actor-id (or actor "lead")
         :workspace-id workspace :payload payload)
   (when cause (list :causation-id cause))))

(defun semuse-v2a--view (entry context candidate relation promotion)
  "Build the Experience View for ENTRY and its evidence references."
  (let ((statement (plist-get entry :candidate-statement)))
    (if (null statement)
        (list :contract "experience-view-v0" :status 'empty
              :scope semuse-v2a--scope :context context :prompt nil
              :items [] :missing nil :conflicts nil
              :opportunity
              (list :kind "candidate-selection" :scope semuse-v2a--scope
                    :context context :reason "no-applicable-promotion")
              :receipt
              (list :projection "experience-applicability"
                    :projection-version 1
                    :selection-policy
                    "single-promoted-applicable-candidate"
                    :selection-policy-version 1 :session-heads []
                    :context context))
      (list :contract "experience-view-v0" :status 'satisfied
            :scope semuse-v2a--scope :context context
            :prompt
            (format
             "[Experience v0 | scope %s]\nUse this reviewed experience only when it fits the current context:\n%s"
             semuse-v2a--scope statement)
            :items
            (vector (list :candidate candidate :promotion promotion
                          :statement statement))
            :missing nil :conflicts nil :opportunity nil
            :receipt
            (list :projection "experience-applicability"
                  :projection-version 1
                  :selection-policy "single-promoted-applicable-candidate"
                  :selection-policy-version 1 :session-heads []
                  :context context :candidate candidate
                  :promotion promotion :relation relation)))))

(defun semuse-v2a--record (entry)
  "Build one detached product-shaped actor record for manifest ENTRY."
  (let* ((position (plist-get entry :position))
         (consumer (semuse-v2a--opaque "session" position))
         (workspace (semuse-v2a--opaque "workspace" position))
         (candidate-session (semuse-v2a--opaque "candidate-session" position))
         (evaluation-session (semuse-v2a--opaque "evaluation-session" position))
         (relation-session (semuse-v2a--opaque "relation-session" position))
         (promotion-session (semuse-v2a--opaque "promotion-session" position))
         (candidate
          (when (plist-get entry :candidate-statement)
            (semuse-v2a--event
             "claim.recorded" candidate-session
             (semuse-v2a--opaque "evt" (+ 100 position)) 1
             (list :statement (plist-get entry :candidate-statement)
                   :scope semuse-v2a--scope :kind "candidate" :basis [])
             nil "agent:analyst" workspace)))
         (candidate-ref (and candidate (semuse-v2a--ref candidate)))
         (evaluation
          (when candidate
            (semuse-v2a--event
             "exercise.evaluated" evaluation-session
             (semuse-v2a--opaque "evt" (+ 200 position)) 2
             (list :exercise-id (format "calibration-%02d" position)
                   :scope semuse-v2a--scope :verdict "passed"
                   :evaluator (list :id "deterministic-rubric" :version 1)
                   :evaluator-manifest-sha256
                   (concat "sha256:" (make-string 64 ?a))
                   :evidence (vector candidate-ref))
             nil "evaluator:test" workspace)))
         (evaluation-ref (and evaluation (semuse-v2a--ref evaluation)))
         (context
          (semuse-v2a--event
           "claim.recorded" consumer
           (semuse-v2a--opaque "evt" (+ 300 position)) 3
           (list :statement
                 (format "Observed %s in held-out instance %d."
                         (plist-get entry :error-code)
                         (plist-get entry :instance))
                 :scope semuse-v2a--scope :kind "context" :basis [])
           nil "agent:analyst" workspace))
         (context-ref (semuse-v2a--ref context))
         (relation
          (when candidate
            (semuse-v2a--event
             "applicability.relation.recorded" relation-session
             (semuse-v2a--opaque "evt" (+ 400 position)) 4
             (list :relation "applies-under" :from candidate-ref
                   :to context-ref :scope semuse-v2a--scope
                   :basis (vector evaluation-ref))
             nil "agent:analyst" workspace)))
         (relation-ref (and relation (semuse-v2a--ref relation)))
         (promotion
          (when candidate
            (semuse-v2a--event
             "candidate.disposition.recorded" promotion-session
             (semuse-v2a--opaque "evt" (+ 500 position)) 5
             (list :candidate candidate-ref :scope semuse-v2a--scope
                   :decision "promote" :basis (vector evaluation-ref)
                   :reason "Deterministic held-out gate passed.")
             nil "user" workspace)))
         (promotion-ref (and promotion (semuse-v2a--ref promotion)))
         (error-data
          (list :tool-name "read_resource"
                :error-type "opaque-boundary"
                :error-data (list :code (plist-get entry :error-code))))
         (error-receipt (semuse-v2a--exact-json error-data))
         (trigger-error
          (semuse-v2a--event
           "tool.failed" consumer
           (semuse-v2a--opaque "evt" (+ 600 position)) 6
           (list :tool-name "read_resource" :status "failed"
                 :error-json (plist-get error-receipt :json)
                 :error-hash (plist-get error-receipt :hash))
           nil "lead" workspace))
         (view
          (semuse-v2a--view entry context-ref candidate-ref
                            relation-ref promotion-ref))
         (prepared
          (list :format "superchat-prepared-envelope-v0"
                :attempt-reason "tool-followup" :system-prompt ""
                :messages
                (vector
                 (list :role "user"
                       :content-format "text"
                       :content
                       (format "The read failed with opaque code %s. Continue."
                               (plist-get entry :error-code))
                       :tool-results [] :multi-turn nil))
                :tools
                (vector
                 (list :name "refresh_handle" :description "Refresh a handle."
                       :args [] :async nil)
                 (list :name "report_termination"
                       :description "Report a terminal boundary."
                       :args [] :async nil)
                 (list :name "report_uncertain"
                       :description "Report insufficient guidance."
                       :args [] :async nil))
                :context-files []
                :view-envelope
                (list :contract "context-continuity-v0" :status 'satisfied
                      :receipt (list :tape-head 0) :disclosure nil
                      :experience-view view)
                :options (list :stream t)))
         (prepared-receipt (semuse-v2a--exact-json prepared))
         (decision
          (semuse-v2a--event
           "decision.snapshot.recorded" consumer
           (semuse-v2a--opaque "evt" (+ 700 position)) 7
           (list :evidence-contract "epistemic-learning-v0" :attempt 2
                 :attempt-reason "tool-followup"
                 :prepared-envelope-json
                 (plist-get prepared-receipt :json)
                 :prepared-envelope-hash
                 (plist-get prepared-receipt :hash))
           (plist-get trigger-error :event-id) "lead" workspace))
         (call-id (semuse-v2a--opaque "call" position))
         (action-data
          (list :tool-name (plist-get entry :expected-tool)
                :call-id call-id
                :named-arguments (copy-tree
                                  (plist-get entry :expected-arguments) t)
                :positional-arguments
                (vector (plist-get entry :error-code))))
         (action-receipt (semuse-v2a--exact-json action-data))
         (action
          (semuse-v2a--event
           "tool.requested" consumer
           (semuse-v2a--opaque "evt" (+ 800 position)) 8
           (list :tool-name (plist-get entry :expected-tool)
                 :evidence-contract "epistemic-learning-v0"
                 :evidence-attempt 2
                 :decision-snapshot-event-id (plist-get decision :event-id)
                 :tool-call-id call-id :tool-call-ordinal 1
                 :arguments-json (plist-get action-receipt :json)
                 :arguments-hash (plist-get action-receipt :hash))
           (plist-get decision :event-id) "lead" workspace))
         (events
          (delq nil (list candidate evaluation context relation promotion
                          trigger-error decision action))))
    (list :contract-revision "v2a"
          :assignment-id (plist-get entry :assignment-id)
          :session-id consumer :workspace-id workspace
          :events (vconcat events)
          :trigger-error (semuse-v2a--ref trigger-error)
          :decision (semuse-v2a--ref decision)
          :first-action (semuse-v2a--ref action))))

(defun semuse-v2a--baseline-records ()
  "Return the 16 detached constructed product records."
  (mapcar #'semuse-v2a--record
          (plist-get semuse-v2a-sealed-evaluator-manifest :schedule)))

(defun semuse-v2a--keys-valid-p (value allowed required)
  "Return non-nil when VALUE is a closed plist with REQUIRED keys."
  (and (null (semuse-v03--plist-shape-error value))
       (cl-every (lambda (key) (memq key allowed))
                 (cl-loop for (key _value) on value by #'cddr collect key))
       (cl-every (lambda (key) (plist-member value key)) required)))

(defun semuse-v2a--valid-ref-p (value)
  "Return non-nil when VALUE is one closed typed event reference."
  (and (semuse-v2a--keys-valid-p
        value '(:session-id :event-id) '(:session-id :event-id))
       (semuse-v03--nonblank-string-p (plist-get value :session-id))
       (semuse-v03--nonblank-string-p (plist-get value :event-id))))

(defun semuse-v2a--shape-gate (records manifest)
  "Validate closed external shapes before any scoring join."
  (let (errors)
    (unless (and (proper-list-p records) records)
      (push 'records-not-list errors))
    (unless (semuse-v2a--keys-valid-p
             manifest semuse-v2a--manifest-fields
             semuse-v2a--manifest-fields)
      (push 'manifest-shape errors))
    (when (semuse-v03--nested-shape-error-p manifest)
      (push 'manifest-nested-shape errors))
    (dolist (entry (and (listp manifest) (plist-get manifest :schedule)))
      (unless (semuse-v2a--keys-valid-p
               entry semuse-v2a--schedule-fields
               semuse-v2a--schedule-fields)
        (push 'schedule-shape errors)))
    (dolist (record records)
      (unless (semuse-v2a--keys-valid-p
               record semuse-v2a--record-fields semuse-v2a--record-fields)
        (push 'record-shape errors))
      (when (semuse-v03--nested-shape-error-p record)
        (push 'record-nested-shape errors))
      (unless (vectorp (plist-get record :events))
        (push 'events-not-vector errors))
      (dolist (key '(:trigger-error :decision :first-action))
        (unless (semuse-v2a--valid-ref-p (plist-get record key))
          (push 'record-reference-shape errors)))
      (when (vectorp (plist-get record :events))
        (dolist (event (append (plist-get record :events) nil))
          (unless (semuse-v2a--keys-valid-p
                   event semuse-v2a--event-fields
                   '(:storage-id :event-id :type :schema :session-id
                     :actor-id :workspace-id :payload))
            (push 'event-shape errors)))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--structure-gate (manifest)
  "Validate the one sealed 16-assignment manifest."
  (let* ((schedule (plist-get manifest :schedule))
         (sha (secure-hash
               'sha256
               (encode-coding-string (prin1-to-string manifest) 'utf-8 t)))
         errors)
    (unless (equal "v2a" (plist-get manifest :contract-revision))
      (push 'manifest-revision errors))
    (unless (equal semuse-v2a--scope (plist-get manifest :scope))
      (push 'manifest-scope errors))
    (unless (= 16 (length schedule)) (push 'manifest-count errors))
    (unless (equal sha semuse-v2a-sealed-evaluator-sha256)
      (push 'sealed-manifest-sha256 errors))
    (dolist (exercise (delete-dups
                       (mapcar (lambda (entry)
                                 (plist-get entry :exercise-id))
                               schedule)))
      (let* ((entries
              (cl-remove-if-not
               (lambda (entry)
                 (equal exercise (plist-get entry :exercise-id)))
               schedule))
             (cards (delq nil (mapcar
                               (lambda (entry)
                                 (plist-get entry :candidate-statement))
                               entries))))
        (unless (and (= 4 (length entries))
                     (equal semuse-v2a--arms
                            (mapcar (lambda (entry) (plist-get entry :arm))
                                    entries))
                     (= 1 (length (delete-dups (mapcar #'length cards)))))
          (push 'exercise-structure errors))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--manifest-entry (manifest assignment-id)
  "Return the manifest entry for ASSIGNMENT-ID."
  (cl-find assignment-id (plist-get manifest :schedule)
           :key (lambda (entry) (plist-get entry :assignment-id))
           :test #'equal))

(defun semuse-v2a--events (record)
  "Return RECORD events as a list."
  (append (plist-get record :events) nil))

(defun semuse-v2a--resolve (reference events)
  "Resolve exactly one REFERENCE from EVENTS, or nil."
  (let ((matches
         (cl-remove-if-not
          (lambda (event)
            (and (equal (plist-get reference :session-id)
                        (plist-get event :session-id))
                 (equal (plist-get reference :event-id)
                        (plist-get event :event-id))))
          events)))
    (and (= 1 (length matches)) (car matches))))

(defun semuse-v2a--record-set-gate (records manifest)
  "Join each actor record exactly once to the sealed schedule."
  (let ((assignments (make-hash-table :test #'equal))
        (sessions (make-hash-table :test #'equal))
        (workspaces (make-hash-table :test #'equal))
        (event-refs (make-hash-table :test #'equal))
        errors)
    (unless (= 16 (length records)) (push 'record-count errors))
    (dolist (record records)
      (let ((assignment (plist-get record :assignment-id))
            (session (plist-get record :session-id))
            (workspace (plist-get record :workspace-id)))
        (unless (semuse-v2a--manifest-entry manifest assignment)
          (push 'assignment-join errors))
        (if (gethash assignment assignments)
            (push 'duplicate-assignment errors)
          (puthash assignment t assignments))
        (if (gethash session sessions)
            (push 'duplicate-session errors)
          (puthash session t sessions))
        (if (gethash workspace workspaces)
            (push 'duplicate-workspace errors)
          (puthash workspace t workspaces))
        (dolist (event (semuse-v2a--events record))
          (let ((key (cons (plist-get event :session-id)
                           (plist-get event :event-id))))
            (if (gethash key event-refs)
                (push 'duplicate-event-reference errors)
              (puthash key t event-refs))))))
    (dolist (entry (plist-get manifest :schedule))
      (unless (gethash (plist-get entry :assignment-id) assignments)
        (push 'missing-assignment errors)))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--parse-exact (json hash)
  "Parse exact JSON after verifying HASH, or return nil."
  (when (and (stringp json) (stringp hash)
             (equal hash
                    (concat
                     "sha256:"
                     (secure-hash
                      'sha256 (encode-coding-string json 'utf-8 t)))))
    (json-parse-string json :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun semuse-v2a--decision-envelope (record)
  "Return RECORD's verified prepared Decision envelope, or nil."
  (let* ((event (semuse-v2a--resolve
                 (plist-get record :decision) (semuse-v2a--events record)))
         (payload (and event (plist-get event :payload))))
    (and payload
         (semuse-v2a--parse-exact
          (plist-get payload :prepared-envelope-json)
          (plist-get payload :prepared-envelope-hash)))))

(defun semuse-v2a--decision-gate (records manifest)
  "Validate trigger error, exact Decision bytes, and oracle absence."
  (let (errors)
    (dolist (record records)
      (condition-case _err
          (let* ((entry (semuse-v2a--manifest-entry
                         manifest (plist-get record :assignment-id)))
                 (events (semuse-v2a--events record))
                 (trigger (semuse-v2a--resolve
                           (plist-get record :trigger-error) events))
                 (decision (semuse-v2a--resolve
                            (plist-get record :decision) events))
                 (error-payload (and trigger (plist-get trigger :payload)))
                 (error-data
                  (and error-payload
                       (semuse-v2a--parse-exact
                        (plist-get error-payload :error-json)
                        (plist-get error-payload :error-hash))))
                 (envelope (semuse-v2a--decision-envelope record))
                 (view-envelope (and envelope
                                     (plist-get envelope :view-envelope)))
                 (experience-view
                  (and view-envelope
                       (plist-get view-envelope :experience-view)))
                 (receipt (and experience-view
                               (plist-get experience-view :receipt)))
                 (visible (and envelope
                               (apply #'concat
                                      (semuse-v03--tree-strings envelope))))
                 (tool-names
                  (and envelope
                       (mapcar (lambda (tool) (plist-get tool :name))
                               (append (plist-get envelope :tools) nil)))))
            (unless (and entry trigger decision envelope
                         (equal "tool.failed" (plist-get trigger :type))
                         (equal "decision.snapshot.recorded"
                                (plist-get decision :type))
                         (< (plist-get trigger :storage-id)
                            (plist-get decision :storage-id))
                         (equal (plist-get entry :error-code)
                                (plist-get (plist-get error-data :error-data)
                                           :code))
                         (equal "superchat-prepared-envelope-v0"
                                (plist-get envelope :format))
                         (equal "tool-followup"
                                (plist-get envelope :attempt-reason))
                         (semuse-v2a--keys-valid-p
                          envelope
                          '(:format :attempt-reason :system-prompt :messages
                            :tools :context-files :view-envelope :options)
                          '(:format :attempt-reason :system-prompt :messages
                            :tools :context-files :view-envelope :options))
                         (semuse-v2a--keys-valid-p
                          view-envelope
                          '(:contract :status :receipt :disclosure
                            :experience-view)
                          '(:contract :status :receipt :disclosure
                            :experience-view))
                         (semuse-v2a--keys-valid-p
                          experience-view
                          '(:contract :status :scope :context :prompt :items
                            :missing :conflicts :opportunity :receipt)
                          '(:contract :status :scope :context :prompt :items
                            :missing :conflicts :opportunity :receipt))
                         (semuse-v2a--keys-valid-p
                          receipt
                          '(:projection :projection-version :selection-policy
                            :selection-policy-version :session-heads :context
                            :candidate :promotion :relation)
                          '(:projection :projection-version :selection-policy
                            :selection-policy-version :session-heads :context))
                         (cl-every (lambda (name) (member name tool-names))
                                   '("refresh_handle" "report_termination"
                                     "report_uncertain")))
              (push 'decision-evidence-join errors))
            (when (or
                   (and envelope
                        (semuse-v03--tree-oracle-errors
                         envelope
                         (append
                          (mapcar #'symbol-name semuse-v2a--arms)
                          (list "v2a-deterministic-rubric")
                          (mapcar
                           (lambda (candidate)
                             (plist-get candidate :assignment-id))
                           (plist-get manifest :schedule)))))
                   (and envelope
                        (cl-some
                         (lambda (key)
                           (semuse-v03--tree-has-key-p envelope key))
                         '(:arm :expected-tool :expected-arguments
                           :answer-key :rubric :sealed-target)))
                   (and visible
                        (let ((case-fold-search t))
                          (string-match-p
                           "\\(?:arm[=:]\\|expected-tool\\|rubric\\|\\_<real\\_>\\|\\_<withheld\\_>\\|\\_<sham\\_>\\|\\_<contradictory\\_>\\)"
                           visible))))
              (push 'visible-oracle errors)))
        (error (push 'decision-evidence-join errors))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--vector-has-ref-p (reference value)
  "Return non-nil when vector VALUE contains REFERENCE."
  (and (vectorp value)
       (cl-find reference (append value nil) :test #'equal)))

(defun semuse-v2a--lineage-gate (records manifest)
  "Validate Candidate through Promotion and Experience View lineage."
  (let (errors)
    (dolist (record records)
      (condition-case _err
          (let* ((entry (semuse-v2a--manifest-entry
                         manifest (plist-get record :assignment-id)))
                 (events (semuse-v2a--events record))
                 (envelope (semuse-v2a--decision-envelope record))
                 (view (plist-get (plist-get envelope :view-envelope)
                                  :experience-view))
                 (receipt (plist-get view :receipt))
                 (context-ref (plist-get receipt :context))
                 (context (semuse-v2a--resolve context-ref events))
                 (candidate-ref (plist-get receipt :candidate))
                 (promotion-ref (plist-get receipt :promotion))
                 (relation-ref (plist-get receipt :relation))
                 (candidate (and candidate-ref
                                 (semuse-v2a--resolve candidate-ref events)))
                 (promotion (and promotion-ref
                                 (semuse-v2a--resolve promotion-ref events)))
                 (relation (and relation-ref
                                (semuse-v2a--resolve relation-ref events)))
                 (promotion-payload (and promotion
                                         (plist-get promotion :payload)))
                 (evaluation-ref
                  (and promotion-payload
                       (vectorp (plist-get promotion-payload :basis))
                       (aref (plist-get promotion-payload :basis) 0)))
                 (evaluation (and evaluation-ref
                                  (semuse-v2a--resolve evaluation-ref events)))
                 (evaluation-payload (and evaluation
                                          (plist-get evaluation :payload)))
                 (relation-payload (and relation (plist-get relation :payload)))
                 (statement (plist-get entry :candidate-statement)))
            (unless (and context
                         (equal "claim.recorded" (plist-get context :type))
                         (equal "context"
                                (plist-get (plist-get context :payload) :kind))
                         (equal semuse-v2a--scope
                                (plist-get (plist-get context :payload) :scope)))
              (push 'context-join errors))
            (if (null statement)
                (unless (and (equal "empty" (plist-get view :status))
                             (null (plist-get view :prompt))
                             (null candidate-ref) (null promotion-ref)
                             (null relation-ref)
                             (not (cl-find-if
                                   (lambda (event)
                                     (and (equal "claim.recorded"
                                                 (plist-get event :type))
                                          (equal "candidate"
                                                 (plist-get
                                                  (plist-get event :payload)
                                                  :kind))))
                                   events)))
                  (push 'withheld-lineage errors))
              (unless (and candidate evaluation promotion relation
                           (equal "satisfied" (plist-get view :status))
                           (equal "claim.recorded" (plist-get candidate :type))
                           (equal "candidate"
                                  (plist-get (plist-get candidate :payload) :kind))
                           (equal statement
                                  (plist-get (plist-get candidate :payload)
                                             :statement))
                           (equal "exercise.evaluated"
                                  (plist-get evaluation :type))
                           (equal "passed"
                                  (plist-get evaluation-payload :verdict))
                           (semuse-v2a--vector-has-ref-p
                            candidate-ref
                            (plist-get evaluation-payload :evidence))
                           (equal "candidate.disposition.recorded"
                                  (plist-get promotion :type))
                           (equal "user" (plist-get promotion :actor-id))
                           (equal "promote"
                                  (plist-get promotion-payload :decision))
                           (equal candidate-ref
                                  (plist-get promotion-payload :candidate))
                           (equal "applicability.relation.recorded"
                                  (plist-get relation :type))
                           (equal "applies-under"
                                  (plist-get relation-payload :relation))
                           (equal candidate-ref
                                  (plist-get relation-payload :from))
                           (equal context-ref (plist-get relation-payload :to))
                           (semuse-v2a--vector-has-ref-p
                            evaluation-ref (plist-get relation-payload :basis))
                           (string-match-p (regexp-quote statement)
                                           (plist-get view :prompt))
                           (< (plist-get candidate :storage-id)
                              (plist-get evaluation :storage-id))
                           (< (plist-get evaluation :storage-id)
                              (plist-get promotion :storage-id)))
                (push 'candidate-lineage errors))))
        (error (push 'candidate-lineage errors))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--action-data (record)
  "Return RECORD's verified first Action data, or nil."
  (let* ((event (semuse-v2a--resolve
                 (plist-get record :first-action) (semuse-v2a--events record)))
         (payload (and event (plist-get event :payload))))
    (and payload
         (semuse-v2a--parse-exact
          (plist-get payload :arguments-json)
          (plist-get payload :arguments-hash)))))

(defun semuse-v2a--first-action-gate (records _manifest)
  "Validate the immutable first post-error Action origin."
  (let (errors)
    (dolist (record records)
      (condition-case _err
          (let* ((events (semuse-v2a--events record))
                 (trigger (semuse-v2a--resolve
                           (plist-get record :trigger-error) events))
                 (decision (semuse-v2a--resolve
                            (plist-get record :decision) events))
                 (action (semuse-v2a--resolve
                          (plist-get record :first-action) events))
                 (payload (and action (plist-get action :payload)))
                 (data (semuse-v2a--action-data record))
                 (first-after-error
                  (car
                   (sort
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal "tool.requested" (plist-get event :type))
                            (> (plist-get event :storage-id)
                               (plist-get trigger :storage-id))))
                     events)
                    (lambda (left right)
                      (< (plist-get left :storage-id)
                         (plist-get right :storage-id)))))))
            (unless (and trigger decision action data
                         (equal action first-after-error)
                         (equal "tool.requested" (plist-get action :type))
                         (equal (plist-get record :session-id)
                                (plist-get action :session-id))
                         (equal (plist-get decision :event-id)
                                (plist-get action :causation-id))
                         (< (plist-get decision :storage-id)
                            (plist-get action :storage-id))
                         (= 2 (plist-get payload :evidence-attempt))
                         (equal (plist-get decision :event-id)
                                (plist-get payload
                                           :decision-snapshot-event-id))
                         (= 1 (plist-get payload :tool-call-ordinal))
                         (equal (plist-get payload :tool-name)
                                (plist-get data :tool-name))
                         (equal (plist-get payload :tool-call-id)
                                (plist-get data :call-id)))
              (push 'first-action-origin errors)))
        (error (push 'first-action-origin errors))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v2a--behavior-score (records manifest)
  "Score exact first Actions against the sealed four-arm schedule."
  (let (mismatches cells)
    (dolist (record records)
      (let* ((entry (semuse-v2a--manifest-entry
                     manifest (plist-get record :assignment-id)))
             (data (semuse-v2a--action-data record))
             (matches
              (and (equal (plist-get entry :expected-tool)
                          (plist-get data :tool-name))
                   (equal (plist-get entry :expected-arguments)
                          (plist-get data :named-arguments)))))
        (push (list :assignment-id (plist-get entry :assignment-id)
                    :exercise-id (plist-get entry :exercise-id)
                    :arm (plist-get entry :arm)
                    :tool-name (plist-get data :tool-name)
                    :matches matches)
              cells)
        (unless matches (push (plist-get entry :assignment-id) mismatches))))
    (setq cells (nreverse cells))
    (let ((directional
           (cl-every
            (lambda (exercise)
              (let ((by-arm
                     (lambda (arm)
                       (plist-get
                        (cl-find-if
                         (lambda (cell)
                           (and (equal exercise (plist-get cell :exercise-id))
                                (eq arm (plist-get cell :arm))))
                         cells)
                        :tool-name))))
                (and (not (equal (funcall by-arm 'real)
                                 (funcall by-arm 'withheld)))
                     (not (equal (funcall by-arm 'real)
                                 (funcall by-arm 'sham)))
                     (not (equal (funcall by-arm 'real)
                                 (funcall by-arm 'contradictory))))))
            (delete-dups (mapcar (lambda (cell)
                                   (plist-get cell :exercise-id))
                                 cells)))))
      (list :status
            (if (and (null mismatches) directional)
                'consistent-with-candidate-specific-action
              'not-supported)
            :mismatches (nreverse mismatches)
            :directional-contrast directional :cells cells))))

(defun semuse-v2a--failure (stage errors gates)
  "Return the single fail-closed result for STAGE and ERRORS."
  (list :ok nil :contract-revision "v2a" :stage stage
        :status 'not-identifiable :closure 'not-identifiable
        :errors errors :gates gates :inferential-claim nil))

(defun semuse-v2a--score (records manifest)
  "Run the ordered V2a evaluator closure."
  (let (gates)
    (catch 'done
      (dolist (spec
               (list
                (list 'shape #'semuse-v2a--shape-gate records manifest)
                (list 'structure #'semuse-v2a--structure-gate manifest)
                (list 'record-set #'semuse-v2a--record-set-gate records manifest)
                (list 'decision #'semuse-v2a--decision-gate records manifest)
                (list 'lineage #'semuse-v2a--lineage-gate records manifest)
                (list 'first-action #'semuse-v2a--first-action-gate
                      records manifest)))
        (let* ((stage (car spec))
               (result (apply (cadr spec) (cddr spec))))
          (setq gates (append gates (list (cons stage result))))
          (unless (plist-get result :ok)
            (throw 'done
                   (semuse-v2a--failure
                    stage (plist-get result :errors) gates)))))
      (let* ((behavior (semuse-v2a--behavior-score records manifest))
             (status (plist-get behavior :status)))
        (setq gates (append gates (list (cons 'behavior behavior))))
        (list :ok t :contract-revision "v2a" :stage 'scored
              :status status :closure status :errors nil :gates gates
              :assignment-count (length records)
              :behavior behavior :inferential-claim nil)))))

(defun semuse-v2a-score (records sealed-evaluator-manifest)
  "Score product-shaped RECORDS against SEALED-EVALUATOR-MANIFEST.
Malformed, cyclic, or oversized external input always fails closed."
  (if (not (semuse-v03--acyclic-input-p
            records sealed-evaluator-manifest))
      (semuse-v2a--failure 'shape '(cyclic-or-oversized-input) nil)
    (condition-case _err
        (semuse-v2a--score
         (copy-tree records t) (copy-tree sealed-evaluator-manifest t))
      (error (semuse-v2a--failure 'internal-error '(scorer-signal) nil)))))

(defun semuse-v2a--fixture-valid ()
  "Return the detached valid constructed V2a bundle."
  (list :records (semuse-v2a--baseline-records)
        :evaluator-manifest
        (copy-tree semuse-v2a-sealed-evaluator-manifest t)))

(defun semuse-v2a--first-event (record type)
  "Return RECORD's first event of TYPE."
  (cl-find type (semuse-v2a--events record)
           :key (lambda (event) (plist-get event :type)) :test #'equal))

(defun semuse-v2a--rewrite-decision (record function)
  "Apply FUNCTION to RECORD's parsed Decision envelope and reseal it."
  (let* ((decision (semuse-v2a--resolve
                    (plist-get record :decision) (semuse-v2a--events record)))
         (payload (plist-get decision :payload))
         (envelope (semuse-v2a--decision-envelope record))
         (receipt (semuse-v2a--exact-json (funcall function envelope))))
    (plist-put payload :prepared-envelope-json (plist-get receipt :json))
    (plist-put payload :prepared-envelope-hash (plist-get receipt :hash))))

(defun semuse-v2a--rewrite-action (record tool-name)
  "Rewrite RECORD's first Action consistently to TOOL-NAME."
  (let* ((action (semuse-v2a--resolve
                  (plist-get record :first-action) (semuse-v2a--events record)))
         (payload (plist-get action :payload))
         (data (semuse-v2a--action-data record)))
    (plist-put data :tool-name tool-name)
    (let ((receipt (semuse-v2a--exact-json data)))
      (plist-put payload :tool-name tool-name)
      (plist-put payload :arguments-json (plist-get receipt :json))
      (plist-put payload :arguments-hash (plist-get receipt :hash)))))

(defun semuse-v2a--fixture (name &optional base-fixture)
  "Return registered fixture NAME, optionally based on BASE-FIXTURE."
  (let* ((fixture (copy-tree (or base-fixture
                                 (semuse-v2a--fixture-valid))
                             t))
         (records (plist-get fixture :records))
         (record (car records)))
    (pcase name
      ('valid-baseline nil)
      ('wrong-action
       (semuse-v2a--rewrite-action record "report_uncertain"))
      ('missing-record (plist-put fixture :records (cdr records)))
      ('candidate-join
       (plist-put
        (plist-get (semuse-v2a--first-event record "claim.recorded") :payload)
        :statement "Unsealed candidate statement."))
      ('evaluation-join
       (let ((evaluation
              (semuse-v2a--first-event record "exercise.evaluated")))
         (plist-put (plist-get evaluation :payload) :evidence
                    (vector (plist-get record :trigger-error)))))
      ('promotion-join
       (let ((promotion
              (semuse-v2a--first-event
               record "candidate.disposition.recorded")))
         (plist-put (plist-get promotion :payload) :basis
                    (vector (plist-get record :trigger-error)))))
      ('view-receipt-join
       (let ((evaluation
              (semuse-v2a--first-event record "exercise.evaluated")))
         (semuse-v2a--rewrite-decision
          record
          (lambda (envelope)
            (plist-put
             (plist-get
              (plist-get (plist-get envelope :view-envelope)
                         :experience-view)
              :receipt)
             :promotion (semuse-v2a--ref evaluation))
            envelope))))
      ('decision-hash
       (let ((decision
              (semuse-v2a--first-event record "decision.snapshot.recorded")))
         (plist-put (plist-get decision :payload) :prepared-envelope-hash
                    (concat "sha256:" (make-string 64 ?0)))))
      ('action-origin
       (let ((action (semuse-v2a--first-event record "tool.requested")))
         (plist-put action :causation-id "evt_wrong_origin")))
      ('visible-oracle
       (semuse-v2a--rewrite-decision
        record
        (lambda (envelope)
          (let ((message (aref (plist-get envelope :messages) 0)))
            (plist-put message :content "arm=real; rubric=refresh_handle"))
          envelope)))
      (_ (error "Unknown V2a fixture: %s" name)))
    fixture))

(defconst semuse-v2a-audit-registry
  '((valid-baseline t scored consistent-with-candidate-specific-action nil)
    (wrong-action t scored not-supported nil)
    (missing-record nil record-set not-identifiable record-count)
    (candidate-join nil lineage not-identifiable candidate-lineage)
    (evaluation-join nil lineage not-identifiable candidate-lineage)
    (promotion-join nil lineage not-identifiable candidate-lineage)
    (view-receipt-join nil lineage not-identifiable candidate-lineage)
    (decision-hash nil decision not-identifiable decision-evidence-join)
    (action-origin nil first-action not-identifiable first-action-origin)
    (visible-oracle nil decision not-identifiable visible-oracle)))

(defun semuse-v2a-audit (&optional fixture-name base-fixture)
  "Audit fixtures, optionally using product-shaped BASE-FIXTURE."
  (let ((specs
         (if fixture-name
             (let ((spec (assq fixture-name semuse-v2a-audit-registry)))
               (and spec (list spec)))
           semuse-v2a-audit-registry))
        checks)
    (dolist (spec specs)
      (let* ((fixture (semuse-v2a--fixture (car spec) base-fixture))
             (result
              (semuse-v2a-score
               (plist-get fixture :records)
               (plist-get fixture :evaluator-manifest)))
             (expected-error (nth 4 spec))
             (match
              (and (eq (nth 1 spec) (plist-get result :ok))
                   (eq (nth 2 spec) (plist-get result :stage))
                   (eq (nth 3 spec) (plist-get result :status))
                   (or (null expected-error)
                       (memq expected-error (plist-get result :errors))))))
        (push (list :name (car spec) :ok match :result result) checks)))
    (setq checks (nreverse checks))
    (list :ok (and specs (cl-every (lambda (check) (plist-get check :ok))
                                   checks))
          :checks checks)))

(provide 'evaluator-logic-v2a)

;;; evaluator-logic-v2a.el ends here
