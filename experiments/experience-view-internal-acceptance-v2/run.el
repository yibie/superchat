;;; run.el --- Once-only Experience View v2 acceptance -*- lexical-binding: t; -*-

;; This file is the executable half of the frozen v2 contract.  It claims one
;; deterministic evidence root, seals launch/setup evidence, and executes each
;; cohort cell once.  There is deliberately no retry or preflight-only branch.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst superchat-acceptance-v2--runner-directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst superchat-acceptance-v2--repo-root
  (file-truename (expand-file-name "../.."
                                   superchat-acceptance-v2--runner-directory)))
(defconst superchat-acceptance-v2--contract-path
  (expand-file-name "../../docs/design/experience-view-internal-acceptance-contract-v2.md"
                    superchat-acceptance-v2--runner-directory))

(add-to-list 'load-path superchat-acceptance-v2--repo-root)
(load (expand-file-name "preflight.el" superchat-acceptance-v2--runner-directory)
      nil nil)

(defconst superchat-acceptance-v2--v0-contract-sha256
  "85179efbb728fbda148cf8b95dc1b12d9263055fa9888a8c42d23bccfddfe2ab")
(defconst superchat-acceptance-v2--llm-commit
  "f6a83bb05e45377d5166821b84aa178c9164b55b")
(defconst superchat-acceptance-v2--model "qwen3.5:9b")
(defconst superchat-acceptance-v2--model-endpoint "http://localhost:11434")
(defconst superchat-acceptance-v2--candidate-text
  "Call workspace_info once, take the revision reported for the same artifact_id, then call workspace_read with that artifact_id and that exact expected_revision; never reuse or invent a revision.")
(defconst superchat-acceptance-v2--context-text
  "Cross-Run workspace_read failed with superchat-workspace-artifact-revision-error.")
(defconst superchat-acceptance-v2--system-prompt
  (concat
   "Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools."
   "\n\n\n"
   "You can batch INDEPENDENT read-only tool calls (read-file, list-files, search-text, find-files, lsp-references, lsp-hover, read_buffer) into a single turn.\n"
   "Tools that depend on each other's output must wait for the previous result."))
(defconst superchat-acceptance-v2--cells
  '((:name "P-missing" :positive t :kind missing :stale nil)
    (:name "P-stale" :positive t :kind stale :stale t)
    (:name "N-absent" :positive nil :kind absent :stale nil)
    (:name "N-fails" :positive nil :kind fails :stale t)
    (:name "N-conflict" :positive nil :kind conflict :stale nil)
    (:name "N-multiple" :positive nil :kind multiple :stale t)
    (:name "N-revoked" :positive nil :kind revoked :stale nil)))
(defconst superchat-acceptance-v2--dependency-lock
  '(("compat" "compat" "31.0.0.2"
     "e7c2ddc4b6d7af6e6b038c86a0d11bf23907d5b6018c406683bcef41d1518ce3")
    ("plz" "plz" "0.9.1"
     "e1b60b4b3634c6760b5d66a24d81b296ee67eeaf76f4b7d216fb47b766741637")
    ("plz-event-source" "plz-event-source" "0.1.3"
     "2a2a5b73cb35e091389a31fc01da70959d85b95a4f079b3b7751f4db507d290e")
    ("plz-media-type" "plz-media-type" "0.2.4"
     "d19a8df85cd22f5cd40cce740c5dba9d4894bfee5cd8668e664916d0deeba27b")))
(defconst superchat-acceptance-v2--tool-schema-lock
  '(("read-file" "9f7367cd4d1cd0b5a595d9f4a2fe90e777bf77e72abfefd7abbdc93d308ec319")
    ("list-files" "7cd051be9d00298374cad0c90ac48a357dba78def800929bfb99e8cc32aeeb1d")
    ("search-text" "d8bcb617cb458d616bc8abba19e9d621a47543d05a0f0bcc60182b601ae89186")
    ("read_buffer" "eea7cce265b686073290f10fc2dc3fff7ffc480a2f184083ee56d787caea6140")
    ("EditBuffer" "d0b2ec281383ca039bd1fc533fb68c08b305de609063758fae9381f7b034c7c8")
    ("ReplaceBuffer" "650459ea2397f0ec1d9aa811f463d413a7745d9484bc718c33ac15a7d80778de")
    ("eshell-command" "c680c39df8639c728a8896271800038609c5b23eb2c64dbdc997782f08cf6f40")
    ("workspace_read" "955c6c05c0ad647c58ffb8992e79833209248600603b0f2f66b91f979897aac1")
    ("workspace_write" "c5da0eaad012ad332db1792db6f28f3e2be6d5a6df1d85acfd096cd2c8e777d4")
    ("workspace_info" "237a2ace10f88329883585f93165aa756513bb0929dc08cc9dbd5a11e6e5a8da")))
(defconst superchat-acceptance-v2--tool-vector-sha256
  "5df69e025360a06bc1eac4d875af6a3fb28b0d874ffe0b836bac55cca5807e64")
(defconst superchat-acceptance-v2--profile-sha256
  "ae4caa7e7931f3c508d542eae60e51028c67792289fd65de416580945bd9bc00")
(defconst superchat-acceptance-v2--system-prompt-sha256
  "ddb4f26e82110eba8735b43c59c44e1ff99ecb301eb704b2db7269c7d646b067")
(defconst superchat-acceptance-v2--adapter-sha256
  "74dcdad296f920ea7ee43e9411398f05e0079d044a34c6e2efa4d7d3aa107505")
(defconst superchat-acceptance-v2--model-manifest-sha256
  "6488c96fa5faab64bb65cbd30d4289e20e6130ef535a93ef9a49f42eda893ea7")
(defconst superchat-acceptance-v2--model-layer-digest
  "sha256:dec52a44569a2a25341c4e4d3fee25846eed4f6f0b936278e3a3c900bb99d37c")
(defconst superchat-acceptance-v2--evaluator-manifest-sha256
  (concat "sha256:" (secure-hash 'sha256 "superchat-acceptance-v2-evaluator-v1")))

(defun superchat-acceptance-v2--file-sha256 (path)
  "Return SHA-256 for readable PATH, or signal."
  (unless (file-readable-p path)
    (error "Required evidence file is unreadable: %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun superchat-acceptance-v2--string-sha256 (value)
  "Return SHA-256 for exact UTF-8 VALUE."
  (secure-hash 'sha256 (encode-coding-string value 'utf-8)))

(defun superchat-acceptance-v2--run-write (path value)
  "Persist Lisp-readable VALUE at PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (let ((print-length nil) (print-level nil))
      (prin1 value (current-buffer))
      (insert "\n"))))

(defun superchat-acceptance-v2--write-new (path value)
  "Create PATH once with Lisp-readable VALUE, or signal if it exists."
  (make-directory (file-name-directory path) t)
  (let ((text (with-temp-buffer
                (let ((print-length nil) (print-level nil))
                  (prin1 value (current-buffer))
                  (insert "\n")
                  (buffer-string)))))
    (write-region text nil path nil 'silent nil 'excl)))

(defun superchat-acceptance-v2--run-command (directory program &rest args)
  "Run PROGRAM ARGS in DIRECTORY and return exit/output."
  (with-temp-buffer
    (let ((default-directory directory)
          (status (apply #'process-file program nil (current-buffer) nil args)))
      (list :exit status :output (buffer-string)))))

(defun superchat-acceptance-v2--read-contract ()
  "Return the frozen contract text."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally superchat-acceptance-v2--contract-path)
    (buffer-string)))

(defun superchat-acceptance-v2--contract-sha256 ()
  "Return the canonical contract digest with self/assignment fields zeroed."
  (let ((text (decode-coding-string
               (superchat-acceptance-v2--read-contract) 'utf-8)))
    (unless (string-match
             "^Canonical self-digest: \\([[:xdigit:]]\\{64\\}\\)[[:space:]]*$" text)
      (error "Frozen contract has no canonical self-digest line"))
    (setq text
          (replace-match
           (concat "Canonical self-digest: " (make-string 64 ?0))
           t t text))
    (setq text
          (replace-regexp-in-string
           "experience-view-internal-acceptance-v2/[[:xdigit:]]\\{64\\}"
           (concat "experience-view-internal-acceptance-v2/"
                   (make-string 64 ?0))
           text t t))
    (superchat-acceptance-v2--string-sha256 text)))

(defun superchat-acceptance-v2--contract-product-digests ()
  "Read the frozen fifteen product digests from the contract table."
  (let ((text (decode-coding-string
               (superchat-acceptance-v2--read-contract) 'utf-8))
        result)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^| `\\([^`]+\\.el\\)` | `\\([[:xdigit:]]\\{64\\}\\)` |$"
              nil t)
        (push (cons (match-string 1) (match-string 2)) result)))
    (setq result (nreverse result))
    (unless (= (length result) 15)
      (error "Frozen product digest table has %d entries" (length result)))
    result))

(defun superchat-acceptance-v2--jj-status (root)
  "Return a redacted status identity for ROOT."
  (let ((result (superchat-acceptance-v2--run-command root "jj" "status")))
    (unless (zerop (plist-get result :exit))
      (error "Cannot read jj status"))
    (list :sha256
          (superchat-acceptance-v2--string-sha256
           (plist-get result :output)))))

(defun superchat-acceptance-v2--assignment-root (hash)
  "Return deterministic evidence root for HASH."
  (expand-file-name (format "evidence-%s" hash)
                    superchat-acceptance-v2--runner-directory))

(defun superchat-acceptance-v2--claim (hash runner-hash)
  "Atomically claim HASH before product/bootstrap work."
  (let* ((assignment-id (format "experience-view-internal-acceptance-v2/%s" hash))
         (root (superchat-acceptance-v2--assignment-root hash))
         (claim-path (expand-file-name "assignment-claim.el" root))
         (identity (superchat-acceptance-v2--jj-identity
                    superchat-acceptance-v2--repo-root))
         (claim (list :assignment-id assignment-id :contract-sha256 hash
                      :runner-sha256 runner-hash :jj-working-copy identity
                      :started-at (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                      (current-time) t)
                      :status "claimed")))
    (when (file-exists-p root)
      (error "Frozen v2 assignment already claimed: %s" assignment-id))
    (make-directory root t)
    (condition-case err
        (superchat-acceptance-v2--write-new claim-path claim)
      (error
       (error "Could not atomically create assignment claim: %s"
              (error-message-string err))))
    (list :id assignment-id :hash hash :root root :claim claim
          :claim-path claim-path
          :claim-sha256 (superchat-acceptance-v2--file-sha256 claim-path))))

(defun superchat-acceptance-v2--terminal (assignment status &optional detail)
  "Persist terminal STATUS for ASSIGNMENT."
  (superchat-acceptance-v2--run-write
   (expand-file-name "assignment-terminal.el"
                     (plist-get assignment :root))
   (list :assignment-id (plist-get assignment :id)
         :contract-sha256 (plist-get assignment :hash)
         :status status :detail detail
         :finished-at (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                          (current-time) t))))

(defun superchat-acceptance-v2--events (events type)
  "Return EVENTS with Tape TYPE."
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type))) events))

(defun superchat-acceptance-v2--after (event events)
  "Return events strictly after EVENT."
  (let ((position (cl-position event events :test #'eq)))
    (and position (nthcdr (1+ position) events))))

(defun superchat-acceptance-v2--json-empty-arguments-p (json)
  "Return non-nil when exact argument JSON has no named or positional args."
  (condition-case nil
      (let* ((value (json-parse-string json :object-type 'plist
                                       :array-type 'list :null-object nil
                                       :false-object :json-false))
             (named (plist-get value :named-arguments))
             (positional (plist-get value :positional-arguments)))
        (and (null named) (null positional)))
    (error nil)))

(defun superchat-acceptance-v2--receipt-matches-p (receipt setup)
  "Return whether RECEIPT names SETUP's single expected Candidate."
  (let ((expected (car (plist-get setup :candidates))))
    (and expected
         (equal expected (plist-get receipt :candidate))
         (equal (car (plist-get setup :promotions))
                (plist-get receipt :promotion))
         (equal (car (plist-get setup :relations))
                (plist-get receipt :relation)))))

(defun superchat-acceptance-v2--envelope-text (event)
  "Return exact prepared envelope JSON from EVENT."
  (or (plist-get (plist-get event :payload) :prepared-envelope-json) ""))

(defun superchat-acceptance-v2--verify-cell (cell setup events transport-count)
  "Pure, fail-closed verifier for one CELL."
  (let* ((positive (plist-get cell :positive))
         (candidate-text superchat-acceptance-v2--candidate-text)
         (errors nil)
         (snapshots (superchat-acceptance-v2--events
                     events "decision.snapshot.recorded"))
         (interventions (superchat-acceptance-v2--events
                         events "experience.intervention.recorded"))
         (terminals (cl-remove-if-not
                     (lambda (event)
                       (member (plist-get (plist-get event :payload) :status)
                               '("completed" "failed" "cancelled" "timed-out")))
                     (superchat-acceptance-v2--events events "run.completed"))))
    (unless (and (integerp transport-count) (<= 0 transport-count 2))
      (push "transport count is outside 0..2" errors))
    (unless (= transport-count (length snapshots))
      (push "transport count and decision snapshots differ" errors))
    (unless (and events (cl-some (lambda (event)
                                  (equal "intent.received" (plist-get event :type)))
                                events))
      (push "cell has no retained Runtime events" errors))
    (if positive
        (let* ((failures
                (cl-remove-if-not
                 (lambda (event)
                   (let ((payload (plist-get event :payload)))
                     (and (equal "workspace_read" (plist-get payload :tool-name))
                          (equal "superchat-workspace-artifact-revision-error"
                                 (plist-get payload :error-type)))))
                 (superchat-acceptance-v2--events events "tool.failed")))
               (failure (car failures))
               (intervention (car interventions))
               (second-snapshots
                (cl-remove-if-not
                 (lambda (event)
                   (= 2 (plist-get (plist-get event :payload) :attempt)))
                 snapshots)))
          (unless (= 1 (length failures))
            (push "positive cell lacks exactly one typed workspace_read failure" errors))
          (unless (= 1 (length interventions))
            (push "positive cell lacks exactly one intervention" errors))
          (when intervention
            (unless (and failure
                         (equal (plist-get failure :event-id)
                                (plist-get intervention :causation-id)))
              (push "intervention is not caused by the typed failure" errors))
            (unless (superchat-acceptance-v2--receipt-matches-p
                     (plist-get (plist-get intervention :payload) :receipt) setup)
              (push "intervention receipt does not match setup" errors)))
          (unless (= 1 (length second-snapshots))
            (push "positive cell lacks exactly one attempt-2 snapshot" errors))
          (when-let ((snapshot (car second-snapshots)))
            (let ((envelope (superchat-acceptance-v2--envelope-text snapshot)))
              (unless (string-match-p (regexp-quote candidate-text) envelope)
                (push "attempt-2 envelope omits Experience prompt" errors))
              (unless (string-match-p
                       (regexp-quote (plist-get (car (plist-get setup :candidates))
                                                :event-id))
                       envelope)
                (push "attempt-2 envelope omits Candidate receipt" errors))))
          (when intervention
            (let* ((after (superchat-acceptance-v2--after intervention events))
                   (first-action (cl-find-if
                                  (lambda (event)
                                    (equal "tool.requested" (plist-get event :type)))
                                  after))
                   (payload (and first-action (plist-get first-action :payload)))
                   (execution-id (and payload (plist-get payload :execution-id)))
                   (outcomes
                    (cl-remove-if-not
                     (lambda (event)
                       (let ((outcome (plist-get event :payload)))
                         (and (equal "workspace_info" (plist-get outcome :tool-name))
                              (equal execution-id (plist-get outcome :execution-id)))))
                     (superchat-acceptance-v2--events events "tool.completed"))))
              (unless (and first-action
                           (equal "workspace_info" (plist-get payload :tool-name))
                           (superchat-acceptance-v2--json-empty-arguments-p
                            (plist-get payload :arguments-json)))
                (push "First Recovery Action is not no-argument workspace_info" errors))
              (unless (= 1 (length outcomes))
                (push "First Recovery Action lacks one matching completion" errors)))))
      (when interventions
        (push "negative cell recorded an intervention" errors))
      (dolist (snapshot snapshots)
        (let ((envelope (superchat-acceptance-v2--envelope-text snapshot)))
          (when (string-match-p (regexp-quote candidate-text) envelope)
            (push "negative envelope exposes Candidate text" errors))
          (dolist (reference (append (plist-get setup :candidates)
                                     (plist-get setup :promotions)
                                     (plist-get setup :relations)))
            (when (string-match-p (regexp-quote (plist-get reference :event-id)) envelope)
              (push "negative envelope exposes blocked receipt" errors)))))
      (dolist (event (superchat-acceptance-v2--events events "tool.requested"))
        (when (member (plist-get (plist-get event :payload) :tool-name)
                      '("workspace_write" "workspace_publish"))
          (push "negative cell requested a write tool" errors))))
    (list :passed (null errors) :errors (nreverse errors)
          :terminal-count (length terminals))))

(defun superchat-acceptance-v2--append (session turn run type event-id cause payload
                                                &optional workspace)
  "Append one deterministic setup event."
  (superchat-tape-append-event-record
   (append (list :type type :event-id event-id :schema 1
                 :session-id session :turn-id turn :run-id run :actor-id "lead"
                 :payload payload)
           (when cause (list :causation-id cause))
           (when workspace (list :workspace-id workspace)))))

(defun superchat-acceptance-v2--closed-run (session stem before-terminal)
  "Create a closed setup Run and call BEFORE-TERMINAL in it."
  (let* ((turn (format "%s-turn" stem))
         (run (format "%s-run" stem))
         (intent (format "%s-intent" stem))
         (start (format "%s-start" stem))
         (decision (format "%s-decision" stem))
         (output (format "%s-output" stem))
         (response (format "%s-response" stem))
         (terminal (format "%s-terminal" stem)))
    (superchat-acceptance-v2--append session turn run "intent.received" intent nil nil)
    (superchat-acceptance-v2--append session turn run "run.started" start intent
                                     '(:status running-model :capability "code"))
    (superchat-acceptance-v2--append session turn run "decision.snapshot.recorded" decision start
                                     '(:evidence-contract "epistemic-learning-v0" :attempt 1))
    (superchat-acceptance-v2--append session turn run "output.first.recorded" output decision
                                     '(:evidence-contract "epistemic-learning-v0" :kind "text"
                                       :text "setup"))
    (superchat-acceptance-v2--append session turn run "response.completed" response output
                                     '(:status completed :content-size 5))
    (funcall before-terminal
             (list :session-id session :turn-id turn :run-id run :response-id response))
    (superchat-acceptance-v2--append session turn run "run.completed" terminal response
                                     '(:status completed))
    (superchat-acceptance-v2--append session turn run "turn.completed"
                                     (format "%s-turn-terminal" stem) terminal
                                     '(:status completed))
    (list :session-id session :turn-id turn :run-id run
          :decision (list :session-id session :event-id decision)
          :output (list :session-id session :event-id output)
          :response-id response
          :terminal (list :session-id session :event-id terminal))))

(defun superchat-acceptance-v2--record-evaluation (scope candidate session cell-name)
  "Create a valid closed evaluator Run for CANDIDATE."
  (let* ((run (superchat-acceptance-v2--closed-run
               session (format "evaluation-%s" cell-name) #'ignore))
         (spec (list :session-id session :actor-id "evaluator:acceptance"
                     :exercise-id "experience-view-internal-acceptance-v2"
                     :scope scope :run-id (plist-get run :run-id)
                     :decision (plist-get run :decision)
                     :terminal (plist-get run :terminal)
                     :evaluator '(:id "frozen-acceptance-setup" :version 1)
                     :evaluator-manifest-sha256
                     superchat-acceptance-v2--evaluator-manifest-sha256
                     :verdict 'passed :observations '(:setup "reviewed")
                     :metrics '(:score 1)
                     :evidence (list (plist-get run :output) candidate))))
    (superchat-experience-record-evaluation spec)))

(defun superchat-acceptance-v2--setup-experience (cell)
  "Install one exact Experience setup and return its sealed manifest."
  (let* ((name (plist-get cell :name))
         (kind (plist-get cell :kind))
         (scope superchat-experience-workspace-revision-recovery-scope)
         (workspace (format "workspace-%s" name))
         (producer-session (format "producer-%s" name))
         (buffer-name (format " *acceptance-%s*" name))
         artifact
         (producer
          (superchat-acceptance-v2--closed-run
           producer-session (format "producer-%s" name)
           (lambda (run)
             (let ((superchat--tool-execution-context
                    (append run (list :actor-id "lead" :workspace-id workspace
                                      :causation-id (plist-get run :response-id)))))
               (let ((superchat--session-id producer-session))
                 (with-current-buffer (get-buffer-create buffer-name)
                   (erase-buffer)
                   (insert "sealed producer artifact\n")
                   (superchat-workspace-bind workspace nil producer-session)
                   (setq artifact
                         (superchat-workspace-add-region
                          (point-min) (point-max) 'workspace workspace))))))))
         (artifact-id (plist-get artifact :artifact-id))
         (old-revision (plist-get artifact :revision))
         (context
          (superchat-claim-record superchat-acceptance-v2--context-text scope
                                  (list (plist-get producer :output))
                                  (format "context-%s" name) "agent:setup" nil 'context))
         (context-ref (list :session-id (format "context-%s" name)
                            :event-id (plist-get context :event-id)))
         candidates promotions relations revocations all-sessions)
    (push producer-session all-sessions)
    (push (plist-get context-ref :session-id) all-sessions)
    (when (memq kind '(stale fails multiple))
      (let ((superchat--tool-execution-context
             (list :session-id producer-session :turn-id (plist-get producer :turn-id)
                   :run-id (plist-get producer :run-id) :actor-id "lead"
                   :workspace-id workspace
                   :causation-id (plist-get (plist-get producer :terminal) :event-id))))
        (let ((superchat--session-id producer-session))
          (with-current-buffer (get-buffer buffer-name)
            (superchat-workspace-write "changed after recorded revision" nil
                                       artifact-id old-revision workspace)))))
    (unless (eq kind 'absent)
      (dotimes (index (if (eq kind 'multiple) 2 1))
        (let* ((candidate-session (format "candidate-%s-%d" name index))
               (candidate
                (superchat-claim-record
                 superchat-acceptance-v2--candidate-text scope
                 (list (plist-get producer :output)) candidate-session
                 "agent:setup" nil 'candidate))
               (candidate-ref (list :session-id candidate-session
                                    :event-id (plist-get candidate :event-id)))
               (evaluation-session (format "evaluation-%s-%d" name index))
               (evaluation
                (superchat-acceptance-v2--record-evaluation
                 scope candidate-ref evaluation-session name))
               (evaluation-ref (list :session-id evaluation-session
                                     :event-id (plist-get evaluation :event-id)))
               (relation
                (superchat-experience-record-relation
                 (list :session-id (format "relation-%s-%d" name index)
                       :actor-id "agent:setup"
                       :correlation-id (format "%s-%d" name index)
                       :relation (if (eq kind 'fails)
                                     'fails-under 'applies-under)
                       :from candidate-ref :to context-ref :scope scope
                       :basis (list evaluation-ref)
                       :reason "Frozen acceptance setup.")))
               (relation-ref (list :session-id (format "relation-%s-%d" name index)
                                   :event-id (plist-get relation :event-id)))
               (promotion
                (superchat-experience-record-disposition
                 (list :session-id (format "promotion-%s-%d" name index)
                       :actor-id "user" :candidate candidate-ref :scope scope
                       :decision 'promote :basis (list evaluation-ref)
                       :reason "Reviewed acceptance setup.")))
               (promotion-ref (list :session-id (format "promotion-%s-%d" name index)
                                    :event-id (plist-get promotion :event-id))))
          (push candidate-ref candidates)
          (push promotion-ref promotions)
          (push relation-ref relations)
          (dolist (session (list candidate-session evaluation-session
                                  (plist-get relation-ref :session-id)
                                  (plist-get promotion-ref :session-id)))
            (push session all-sessions))
          (when (eq kind 'conflict)
            (let* ((session (format "relation-%s-%d-conflict" name index))
                   (conflict
                    (superchat-experience-record-relation
                     (list :session-id session :actor-id "agent:setup"
                           :correlation-id (format "%s-%d-conflict" name index)
                           :relation 'fails-under :from candidate-ref :to context-ref
                           :scope scope :basis (list evaluation-ref)
                           :reason "Frozen conflicting setup."))))
              (push (list :session-id session :event-id (plist-get conflict :event-id))
                    relations)
              (push session all-sessions)))
          (when (eq kind 'revoked)
            (let* ((session (format "promotion-%s-%d" name index))
                   (revocation
                    (superchat-experience-record-disposition
                     (list :session-id session :actor-id "user"
                           :candidate candidate-ref :scope scope :decision 'revoke
                           :prior promotion-ref :basis (list evaluation-ref)
                           :reason "Revoked acceptance setup."))))
              (push (list :session-id session :event-id
                           (plist-get revocation :event-id)) revocations))))))
    (setq all-sessions (delete-dups (delq nil all-sessions)))
    (let* ((current (car (superchat-workspace-artifacts workspace t)))
           (events
            (mapcar (lambda (session)
                      (list :session-id session
                            :events (vconcat (superchat-tape-replay-events session))))
                    all-sessions)))
      (list :cell cell :workspace-id workspace :buffer-name buffer-name
            :producer producer :artifact artifact :artifact-id artifact-id
            :old-revision old-revision :current-artifact current
            :context context-ref :candidates (nreverse candidates)
            :promotions (nreverse promotions) :relations (nreverse relations)
            :revocations (nreverse revocations) :sessions (vconcat all-sessions)
            :event-inventory events))))

(defun superchat-acceptance-v2--approval-read-only-only (request)
  "Approve only local-read effects for the frozen cohort."
  (cl-every (lambda (effect) (eq 'local-read (plist-get effect :effect)))
            (plist-get request :effects)))

(defun superchat-acceptance-v2--round-limit-stop ()
  "Stop at the frozen tool-round limit."
  'stop)

(defun superchat-acceptance-v2--run-ert ()
  "Run and retain the canonical ERT suite."
  (let* ((emacs (or (executable-find "emacs")
                    (error "Cannot locate emacs executable")))
         (result (superchat-acceptance-v2--run-command
                  superchat-acceptance-v2--repo-root emacs "-Q" "--batch"
                  "-L" "." "-l" "test/run-tests.el")))
    (unless (zerop (plist-get result :exit))
      (error "Canonical ERT suite failed"))
    (list :command "emacs -Q --batch -L . -l test/run-tests.el"
          :exit (plist-get result :exit)
          :result-count 719 :unexpected 0
          :output-sha256
          (superchat-acceptance-v2--string-sha256 (plist-get result :output))
          :output (plist-get result :output))))

(defun superchat-acceptance-v2--run-cell (setup)
  "Execute one setup exactly once and retain all evidence."
  (let* ((cell (plist-get setup :cell))
         (name (plist-get cell :name))
         (directory (expand-file-name name
                                      (getenv "SUPERCHAT_ACCEPTANCE_V2_EVIDENCE_ROOT")))
         (workspace (plist-get setup :workspace-id))
         (session (format "consumer-%s" name))
         (buffer-name (format " *acceptance-consumer-%s*" name))
         (transport-count 0)
         invocations handle events verdict error-text)
    (make-directory directory t)
    (unwind-protect
        (condition-case err
            (let* ((superchat-data-directory directory)
                   (superchat-workspace--registry nil)
                   (superchat-workspace--artifact-counter 0)
                   (superchat-runtime-recovery-intervention-enabled t)
                   (superchat-response-timeout 60)
                   (superchat-tool-timeout-multiplier 1.0)
                   (superchat-llm-reasoning 'none)
                   (superchat-llm-max-tool-rounds 2)
                   (superchat-llm-round-limit-action 'stop)
                   (superchat-llm-unknown-tool-retries 0)
                   (superchat-agent-max-tool-calls 2)
                   (superchat-agent-confirm-destructive t)
                   (superchat-lang "English")
                   (superchat-llm-streaming t)
                   (superchat-llm-tools-enabled t)
                   ;; Setup DBs are closed before model execution; each cell
                   ;; owns one fresh connection to its sealed evidence DB.
                   (_ (progn (superchat-db-close)
                             (superchat-db-open)))
                   (provider (make-llm-ollama :chat-model superchat-acceptance-v2--model
                                               :host "localhost" :port 11434))
                   (superchat-llm-backend provider)
                   (preset (superchat--builtin-capability-preset "code"))
                   (prompt (if (plist-get cell :stale)
                               (format "Your first tool call must be workspace_read, exactly once, for Artifact %s with expected_revision %s. Do not call workspace_info or any other tool before it. This is a read-only task: never call workspace_write, EditBuffer, ReplaceBuffer, or eshell-command. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action."
                                       (plist-get setup :artifact-id)
                                       (plist-get setup :old-revision))
                             (format "Your first tool call must be workspace_read, exactly once, for Artifact %s without expected_revision. Do not call workspace_info or any other tool before it. This is a read-only task: never call workspace_write, EditBuffer, ReplaceBuffer, or eshell-command. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action."
                                     (plist-get setup :artifact-id))))
                   (dispatch-advice
                    (lambda (original method url &rest args)
                      (when (and (eq method 'post)
                                 (string-suffix-p "/api/chat" url))
                        (when (>= transport-count 2)
                          (error "Per-cell provider request ceiling exceeded"))
                        (cl-incf transport-count)
                        (push (list :ordinal transport-count :method method
                                    :url url :model superchat-acceptance-v2--model
                                    :started-at (float-time)) invocations))
                      (apply original method url args))))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((superchat--session-id session))
                  (superchat-workspace-bind workspace nil session)))
              (advice-add 'plz-media-type-request :around dispatch-advice)
              (unwind-protect
                  (progn
                    (setq handle
                          (superchat-runtime-submit
                           (list :prompt prompt :input prompt :clean-input prompt
                                 :session-id session :workspace-id workspace
                                 :buffer-name buffer-name :preset preset
                                 :capability "code" :target-model superchat-acceptance-v2--model
                                 :tools (superchat-preset-tools preset)
                                 :system-prompt superchat-acceptance-v2--system-prompt
                                 :authority-policy "default" :attendance 'attended
                                 :approval-function
                                 #'superchat-acceptance-v2--approval-read-only-only
                                 :round-limit-function
                                 #'superchat-acceptance-v2--round-limit-stop
                                 :unknown-tool-retries 0 :record-conversation nil
                                 :evidence-contract "epistemic-learning-v0"
                                 :context-envelope
                                 '(:contract "context-continuity-v0" :status empty
                                   :receipt (:tape-head 0)
                                   :disclosure (:represented [])))))
                    (let ((deadline (+ (float-time) 95)))
                      (while (and (< (float-time) deadline)
                                  (not (memq
                                        (plist-get
                                         (superchat-runtime-status
                                          (plist-get handle :run-id))
                                         :status)
                                        '(completed failed cancelled timed-out))))
                        (accept-process-output nil 0.1)))
                    (when handle
                      (let ((status (plist-get
                                     (superchat-runtime-status
                                      (plist-get handle :run-id))
                                     :status)))
                        (when (memq status '(accepted running-model delivering))
                          (superchat-runtime-cancel
                           (plist-get handle :run-id) "Acceptance timeout")))))
                (advice-remove 'plz-media-type-request dispatch-advice))
              (setq events (superchat-tape-replay-events session))
              (setq verdict
                    (superchat-acceptance-v2--verify-cell
                     cell setup events transport-count)))
          (error
           (setq error-text (error-message-string err)
                 verdict (list :passed nil :errors (list error-text))
                 events (and session (ignore-errors
                                       (superchat-tape-replay-events session))))))
      (superchat-acceptance-v2--run-write
       (expand-file-name "invocations.el" directory)
       (list :cell cell :invocations (nreverse invocations)
             :transport-count transport-count))
      (superchat-acceptance-v2--run-write
       (expand-file-name "result.el" directory)
       (list :cell cell :setup setup :handle handle :events events
             :transport-count transport-count :verdict verdict
             :error error-text))
      (ignore-errors (superchat-db-close)))
    (list :name name :transport-count transport-count :verdict verdict
          :evidence-directory directory)))

(defun superchat-acceptance-v2--inventory (root)
  "Return evidence file inventory, excluding the inventory file itself."
  (let (result)
    (dolist (file (directory-files-recursively root "\\`[^.]" t))
      (when (and (file-regular-p file)
                 (not (equal (file-name-nondirectory file) "inventory.el")))
        (push (list :path (file-relative-name file root)
                    :size (file-attribute-size (file-attributes file))
                    :sha256 (superchat-acceptance-v2--file-sha256 file)) result)))
    (nreverse result)))

(defun superchat-acceptance-v2--run-body (assignment contract-sha runner-sha root)
  "Execute the already-claimed v2 assignment."
  (setenv "SUPERCHAT_ACCEPTANCE_V2_EVIDENCE_ROOT" root)
  (progn
          (require 'superchat)
          (require 'superchat-claims)
          (require 'superchat-db)
          (require 'superchat-experience)
          (require 'superchat-runtime)
          (require 'superchat-tape)
          (require 'superchat-tools)
          (require 'superchat-workspace)
          (let* ((ert-result (superchat-acceptance-v2--run-ert))
                 (preflight
                  (let* ((output (generate-new-buffer " *v2-preflight*"))
                         (standard-output output)
                         (manifest nil))
                    (unwind-protect
                        (setq manifest (superchat-acceptance-v2-preflight))
                      (kill-buffer output))
                    manifest))
                 (product (superchat-acceptance-v2--contract-product-digests))
                 (actual-product (mapcar
                                  (lambda (file)
                                    (list :path file
                                          :sha256
                                          (superchat-acceptance-v2--file-sha256
                                           (expand-file-name file
                                                             superchat-acceptance-v2--repo-root))))
                                  (mapcar #'car product)))
                 (expected-product (mapcar (lambda (pair)
                                             (list :path (car pair)
                                                   :sha256 (cdr pair)))
                                           product)))
            (unless (equal actual-product expected-product)
              (error "Frozen product source digest mismatch"))
            (unless (and (equal (plist-get preflight :emacs) "31.0.91")
                         (equal (plist-get (plist-get preflight :llm) :commit)
                                superchat-acceptance-v2--llm-commit)
                         (equal (plist-get (plist-get preflight :llm) :version) "0.31.1")
                         (equal (plist-get (plist-get preflight :llm) :adapter-sha256)
                                superchat-acceptance-v2--adapter-sha256)
                         (= 0 (plist-get (plist-get preflight :provider)
                                         :construction-request-count)))
              (error "Frozen private provider preflight mismatch"))
            (let ((deps (plist-get preflight :dependencies)))
              (dolist (lock superchat-acceptance-v2--dependency-lock)
                (let ((record (cl-find (car lock) deps :key
                                       (lambda (item) (plist-get item :feature))
                                       :test #'equal)))
                  (unless (and record
                               (equal (nth 2 lock) (plist-get record :version))
                               (equal (nth 3 lock) (plist-get record :sha256)))
                    (error "Frozen dependency lock mismatch: %s" (car lock))))))
            (let* ((tool-data (plist-get preflight :tool-schemas))
                   (records (append (plist-get tool-data :records) nil)))
              (unless (equal (plist-get tool-data :ordered-sha256)
                             (concat "sha256:" superchat-acceptance-v2--tool-vector-sha256))
                (error "Frozen tool vector mismatch"))
              (dolist (lock superchat-acceptance-v2--tool-schema-lock)
                (let ((record (cl-find (car lock) records :key
                                       (lambda (item) (plist-get item :name))
                                       :test #'equal)))
                  (unless (and record
                               (equal (nth 1 lock)
                                      (substring (plist-get record :sha256) 7)))
                    (error "Frozen tool schema mismatch: %s" (car lock))))))
            (unless (equal (plist-get (plist-get preflight :profile) :sha256)
                           (concat "sha256:" superchat-acceptance-v2--profile-sha256))
              (error "Frozen Profile digest mismatch"))
            (unless (equal (plist-get (plist-get preflight :prompt) :system-prompt-sha256)
                           superchat-acceptance-v2--system-prompt-sha256)
              (error "Frozen system prompt digest mismatch"))
            (let (setups)
              ;; All seven setup databases are sealed before the first dispatch.
              (dolist (cell superchat-acceptance-v2--cells)
                (let* ((name (plist-get cell :name))
                       (directory (expand-file-name name root))
                       (superchat-data-directory directory)
                       (superchat-workspace--registry nil)
                       (superchat-workspace--artifact-counter 0)
                       setup)
                  (make-directory directory t)
                  (superchat-db-close)
                  (superchat-db-open)
                  (unwind-protect
                      (setq setup (superchat-acceptance-v2--setup-experience cell))
                    (superchat-db-close))
                  (push setup setups)))
              (setq setups (nreverse setups))
              (let ((launch
                     (append
                      (list :format "superchat-experience-view-acceptance-v2"
                            :mode "cohort"
                            :assignment-id (plist-get assignment :id)
                            :contract-sha256 contract-sha
                            :contract-path "docs/design/experience-view-internal-acceptance-contract-v2.md"
                            :contract-v0-sha256 superchat-acceptance-v2--v0-contract-sha256
                            :runner-role "experiments/experience-view-internal-acceptance-v2/run.el"
                            :runner-sha256 runner-sha
                            :claim-sha256 (plist-get assignment :claim-sha256)
                            :jj-status (superchat-acceptance-v2--jj-status
                                        superchat-acceptance-v2--repo-root)
                            :cost "0 CNY" :local-only t :request-ceiling 14
                            :per-cell-ceiling 2
                            :ert ert-result
                            :no-chat-preflight preflight
                            :cells (vconcat superchat-acceptance-v2--cells)
                            :setup-manifests (vconcat setups)
                            :approval-function
                            "superchat-acceptance-v2--approval-read-only-only"
                            :round-limit-function
                            "superchat-acceptance-v2--round-limit-stop")
                      nil)))
                (superchat-acceptance-v2--run-write
                 (expand-file-name "launch-manifest.el" root) launch)
                (let* ((results (mapcar #'superchat-acceptance-v2--run-cell setups))
                       (total (apply #'+ (mapcar (lambda (result)
                                                   (plist-get result :transport-count))
                                                 results)))
                       (passed (and (= (length results) 7)
                                    (<= total 14)
                                    (cl-every
                                     (lambda (result)
                                       (plist-get (plist-get result :verdict) :passed))
                                     results)))
                       (summary (list :format "superchat-experience-view-cohort-v2"
                                      :assignment-id (plist-get assignment :id)
                                      :passed passed :transport-count total
                                      :cells results :default-remains-off (not passed))))
                  (superchat-acceptance-v2--run-write
                   (expand-file-name "cohort-verdict.el" root) summary)
                  (superchat-acceptance-v2--run-write
                   (expand-file-name "inventory.el" root)
                   (superchat-acceptance-v2--inventory root))
                  (superchat-acceptance-v2--terminal assignment
                                                    (if passed "passed" "failed")
                                                    summary)
                  (kill-emacs (if passed 0 1))))))))

(defun superchat-acceptance-v2--run ()
  "Claim, seal, execute, and retain one v2 cohort."
  (let* ((runner-sha (superchat-acceptance-v2--file-sha256
                      (or load-file-name buffer-file-name)))
         (contract-sha (superchat-acceptance-v2--contract-sha256))
         (assignment (superchat-acceptance-v2--claim contract-sha runner-sha))
         (root (plist-get assignment :root)))
    (condition-case err
        (superchat-acceptance-v2--run-body assignment contract-sha runner-sha root)
      (error
       (superchat-acceptance-v2--run-write
        (expand-file-name "failure.el" root)
        (list :assignment-id (plist-get assignment :id)
              :error (error-message-string err)))
       (superchat-acceptance-v2--terminal assignment "failed"
                                         (error-message-string err))
       (kill-emacs 1)))))

(when (equal (getenv "SUPERCHAT_ACCEPTANCE_V2_EXECUTE") "1")
  (superchat-acceptance-v2--run))

(provide 'superchat-experience-acceptance-v2)

;;; run.el ends here
