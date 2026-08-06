;;; run.el --- Closed-loop Experience Correction runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Experiment-only runner for the preregistered closed-loop correction test.
;; It copies the frozen Cohort v6 database to a retained working database,
;; records the human correction through production Claim APIs, replays the
;; corrected Current View after reopen, and launches the 24 assigned Research
;; Workflows sequentially.  Loading this file does not start model calls.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'superchat-claims)
(require 'superchat-db)
(require 'superchat)
(require 'superchat-tools)
(require 'superchat-workflow)
(require 'superchat-subagent)
(require 'superchat-llm nil t)
(require 'llm-deepseek nil t)

(defvar llm-request-plz-connect-timeout)
(defvar plz-curl-program)
(defvar plz-curl-default-args)
(defvar superchat-agent-permission-functions)
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-reasoning)
(defvar superchat-response-timeout)
(defvar superchat-subagent-timeout)
(declare-function make-llm-deepseek "llm-deepseek")
(declare-function superchat--provider-name "superchat-llm" (provider))

(defconst superchat-closed-loop-experience-correction-v0--experiment-id
  "closed-loop-experience-correction-v0")

(defconst superchat-closed-loop-experience-correction-v0--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst superchat-closed-loop-experience-correction-v0--project-root
  (expand-file-name "../.." superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--root
  "/private/tmp/superchat-closed-loop-experience-correction-v0/")

(defconst superchat-closed-loop-experience-correction-v0--runtime-directory
  (expand-file-name "runtime/" superchat-closed-loop-experience-correction-v0--root))

(defconst superchat-closed-loop-experience-correction-v0--working-db
  (expand-file-name "superchat.db"
                    superchat-closed-loop-experience-correction-v0--runtime-directory))

(defconst superchat-closed-loop-experience-correction-v0--source-db
  (expand-file-name "experiments/experience-cohort-deepseek-v6/runtime-snapshot.db"
                    superchat-closed-loop-experience-correction-v0--project-root))

(defconst superchat-closed-loop-experience-correction-v0--source-hash
  "sha256:dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c")

(defconst superchat-closed-loop-experience-correction-v0--manifest-path
  (expand-file-name "manifest.json"
                    superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--assignments-path
  (expand-file-name "assignments.json"
                    superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--claim-record-path
  (expand-file-name "claim-correction-record.json"
                    superchat-closed-loop-experience-correction-v0--root))

(defconst superchat-closed-loop-experience-correction-v0--launch-record-path
  (expand-file-name "launch-record.json"
                    superchat-closed-loop-experience-correction-v0--root))

(defconst superchat-closed-loop-experience-correction-v0--prompt-record-path
  (expand-file-name "prompt-snapshots.json"
                    superchat-closed-loop-experience-correction-v0--root))

(defconst superchat-closed-loop-experience-correction-v0--preflight-record-path
  (expand-file-name "preflight-record.json"
                    superchat-closed-loop-experience-correction-v0--root))

(defconst superchat-closed-loop-experience-correction-v0--deliverable-guard-path
  (expand-file-name "experiments/experience-cohort-deepseek-v6/deliverable-guard-v2.txt"
                    superchat-closed-loop-experience-correction-v0--project-root))

(defconst superchat-closed-loop-experience-correction-v0--allowed-tools
  '("agent_reach_search" "agent_reach_read"
    "workspace_read" "workspace_publish" "workspace_info"
    "team_task" "team_message"))

(defconst superchat-closed-loop-experience-correction-v0--claim-session
  "subagent-researcher-20260805-124435-55dd")

(defconst superchat-closed-loop-experience-correction-v0--claim-scope
  "experience-cohort-deepseek-v6/discovery")

(defconst superchat-closed-loop-experience-correction-v0--initial-basis
  '("tape:1577" "tape:1578" "tape:1580"))

(defconst superchat-closed-loop-experience-correction-v0--corrected-basis
  '("tape:961" "tape:962" "tape:964"
    "tape:1577" "tape:1578" "tape:1580"))

(defconst superchat-closed-loop-experience-correction-v0--initial-claim
  "After agent_reach_read fails, switching to agent_reach_search is a stable recovery strategy.")

(defconst superchat-closed-loop-experience-correction-v0--corrected-claim
  "Cohort v6 contains two non-matching recovery episodes; no stable recovery strategy is established.")

(defconst superchat-closed-loop-experience-correction-v0--correction-note
  "The two eligible failures have different follow-ups: one repeats agent_reach_read and one switches to agent_reach_search.")

(defvar superchat-closed-loop-experience-correction-v0--launches nil)
(defvar superchat-closed-loop-experience-correction-v0--prompt-snapshots nil)
(defvar superchat-closed-loop-experience-correction-v0--prepared-queries
  (make-hash-table :test #'equal))

(setq superchat-data-directory
      superchat-closed-loop-experience-correction-v0--runtime-directory
      superchat-llm-model nil
      superchat-llm-reasoning 'none
      llm-request-plz-connect-timeout 30
      plz-curl-program "/opt/local/bin/curl"
      plz-curl-default-args
      '("--silent" "--compressed" "--location"
        "--proxy" "http://127.0.0.1:7897"
        "--noproxy" "localhost,127.0.0.1"
        "--suppress-connect-headers"
        "--retry" "2" "--retry-all-errors"
        "--retry-delay" "1" "--retry-max-time" "90")
      superchat-response-timeout 7200
      superchat-subagent-timeout 21600)

(when (fboundp 'make-llm-deepseek)
  (setq superchat-llm-backend
        (make-llm-deepseek
         :key (lambda () (getenv "DEEPSEEK_API_KEY"))
         :chat-model "deepseek-v4-pro")))

(defun superchat-closed-loop-experience-correction-v0--read-json (path)
  "Read PATH as a plist-backed JSON object."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object json-false)))

(defun superchat-closed-loop-experience-correction-v0--write-json (path value)
  "Write VALUE as pretty JSON to PATH."
  (make-directory (file-name-directory path) t)
  (let ((json-encoding-pretty-print t))
    (with-temp-file path
      (insert (json-encode value))
      (insert "\n"))))

(defun superchat-closed-loop-experience-correction-v0--read-file (path)
  "Return PATH contents."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun superchat-closed-loop-experience-correction-v0--sha256-file (path)
  "Return protocol-prefixed SHA-256 for PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (concat "sha256:" (secure-hash 'sha256 (current-buffer)))))

(defun superchat-closed-loop-experience-correction-v0--sha256-string (value)
  "Return protocol-prefixed SHA-256 for VALUE."
  (concat "sha256:" (secure-hash 'sha256 (or value ""))))

(defun superchat-closed-loop-experience-correction-v0--json-bool (value)
  "Return VALUE as a JSON boolean."
  (if value t json-false))

(defun superchat-closed-loop-experience-correction-v0--manifest ()
  "Return the frozen manifest."
  (superchat-closed-loop-experience-correction-v0--read-json
   superchat-closed-loop-experience-correction-v0--manifest-path))

(defun superchat-closed-loop-experience-correction-v0--assignments ()
  "Return the frozen 24 assignments."
  (plist-get
   (superchat-closed-loop-experience-correction-v0--read-json
    superchat-closed-loop-experience-correction-v0--assignments-path)
   :assignments))

(defun superchat-closed-loop-experience-correction-v0--assignment-arm (assignment)
  "Return ASSIGNMENT's Claim arm."
  (or (plist-get assignment :arm)
      (plist-get assignment :claim_arm)
      (plist-get assignment :claim_block)
      (plist-get assignment :claim-block)
      (error "Assignment has no Claim arm: %S" assignment)))

(defun superchat-closed-loop-experience-correction-v0--assignment-task-id
    (assignment)
  "Return ASSIGNMENT's matched task id."
  (or (plist-get assignment :task_id)
      (plist-get assignment :source_task_id)
      (plist-get assignment :task)
      (plist-get assignment :question)
      (error "Assignment has no task id: %S" assignment)))

(defun superchat-closed-loop-experience-correction-v0--task-question
    (assignment)
  "Return and validate the preregistered question for ASSIGNMENT."
  (let* ((task-id
          (superchat-closed-loop-experience-correction-v0--assignment-task-id
           assignment))
         (spec
          (superchat-closed-loop-experience-correction-v0--read-json
           superchat-closed-loop-experience-correction-v0--assignments-path))
         (task
          (cl-find task-id (plist-get spec :tasks)
                   :key (lambda (item) (plist-get item :task_id))
                   :test #'equal))
         (inline-question (plist-get assignment :question))
         (task-question (plist-get task :question))
         (question (or inline-question task-question)))
    (unless (or task inline-question)
      (error "Assignment %s references unknown task %s"
             (superchat-closed-loop-experience-correction-v0--assignment-id
              assignment)
             task-id))
    (when (and inline-question task-question
               (not (equal inline-question task-question)))
      (error "Assignment %s question disagrees with task %s"
             (superchat-closed-loop-experience-correction-v0--assignment-id
              assignment)
             task-id))
    (unless (and (stringp question)
                 (not (string-empty-p (string-trim question))))
      (error "Task %s has no non-empty question" task-id))
    question))

(defun superchat-closed-loop-experience-correction-v0--assignment-run-index
    (assignment)
  "Return ASSIGNMENT's sequential run index."
  (or (plist-get assignment :run_index)
      (plist-get assignment :run-index)
      (error "Assignment has no run index: %S" assignment)))

(defun superchat-closed-loop-experience-correction-v0--assignment-id
    (assignment)
  "Return ASSIGNMENT's unique id."
  (or (plist-get assignment :assignment_id)
      (plist-get assignment :assignment-id)
      (format "closed-loop-%02d"
              (superchat-closed-loop-experience-correction-v0--assignment-run-index
               assignment))))

(defun superchat-closed-loop-experience-correction-v0--assert-unique
    (values label)
  "Signal unless VALUES are unique under LABEL."
  (unless (= (length values) (length (delete-dups (copy-sequence values))))
    (error "Duplicate %s in closed-loop assignments" label)))

(defun superchat-closed-loop-experience-correction-v0--verify-frozen-inputs ()
  "Fail unless preregistered inputs and source data are available."
  (unless (file-readable-p superchat-closed-loop-experience-correction-v0--manifest-path)
    (error "Missing manifest: %s"
           superchat-closed-loop-experience-correction-v0--manifest-path))
  (unless (file-readable-p superchat-closed-loop-experience-correction-v0--assignments-path)
    (error "Missing assignments: %s"
           superchat-closed-loop-experience-correction-v0--assignments-path))
  (unless (equal superchat-closed-loop-experience-correction-v0--source-hash
                 (superchat-closed-loop-experience-correction-v0--sha256-file
                  superchat-closed-loop-experience-correction-v0--source-db))
    (error "Frozen Cohort v6 database hash changed"))
  (let* ((manifest
          (superchat-closed-loop-experience-correction-v0--manifest))
         (assignments
          (superchat-closed-loop-experience-correction-v0--assignments))
         (arms
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--assignment-arm
           assignments))
         (task-ids
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--assignment-task-id
           assignments))
         (run-indexes
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--assignment-run-index
           assignments))
         (assignment-ids
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--assignment-id
           assignments)))
    (unless (= 24 (length assignments))
      (error "Expected 24 closed-loop assignments, found %d"
             (length assignments)))
    (superchat-closed-loop-experience-correction-v0--assert-unique
     assignment-ids "assignment_id")
    (superchat-closed-loop-experience-correction-v0--assert-unique
     run-indexes "run_index")
    (unless (equal (number-sequence 1 24) (sort (copy-sequence run-indexes) #'<))
      (error "Closed-loop run_index must be exactly 1 through 24"))
    (dolist (arm '("no-claim" "stale-claim" "corrected-current"))
      (unless (= 8 (cl-count arm arms :test #'equal))
        (error "Expected 8 assignments for arm %s" arm)))
    (unless (= 8 (length (delete-dups (copy-sequence task-ids))))
      (error "Expected 8 matched tasks across 24 assignments"))
    (dolist (assignment assignments)
      (superchat-closed-loop-experience-correction-v0--task-question
       assignment))
    (dolist (input (plist-get manifest :frozen_inputs))
      (let* ((raw-path (plist-get input :path))
             (path (and raw-path
                        (expand-file-name raw-path
                                          superchat-closed-loop-experience-correction-v0--project-root)))
             (expected (plist-get input :sha256)))
        (when (and path expected)
          (unless (equal expected
                         (and (file-readable-p path)
                              (superchat-closed-loop-experience-correction-v0--sha256-file
                               path)))
            (error "Frozen input changed or missing: %s" path)))))
    manifest))

(defun superchat-closed-loop-experience-correction-v0--copy-source-db ()
  "Create the retained working DB if it does not already exist."
  (make-directory superchat-closed-loop-experience-correction-v0--runtime-directory t)
  (unless (file-exists-p superchat-closed-loop-experience-correction-v0--working-db)
    (copy-file superchat-closed-loop-experience-correction-v0--source-db
               superchat-closed-loop-experience-correction-v0--working-db)))

(defun superchat-closed-loop-experience-correction-v0--source-rows ()
  "Return the six frozen recovery Tape rows."
  (mapcar
   (lambda (row)
     (list :storage_id (nth 0 row)
           :session_id (nth 1 row)
           :kind (nth 2 row)
           :content (nth 3 row)))
   (superchat-db-tape-select
    "SELECT id, session_id, kind, content FROM tape
     WHERE id IN (?, ?, ?, ?, ?, ?) ORDER BY id"
    '(961 962 964 1577 1578 1580))))

(defun superchat-closed-loop-experience-correction-v0--claim-by-id
    (claim-id claims)
  "Return CLAIM-ID from CLAIMS."
  (or (cl-find claim-id claims
               :key (lambda (claim) (plist-get claim :claim-id))
               :test #'equal)
      (error "Claim not found in replay view: %s" claim-id)))

(defun superchat-closed-loop-experience-correction-v0--current-claim-view ()
  "Return the production replay Current View for the correction scope."
  (superchat-claims-current-view
   superchat-closed-loop-experience-correction-v0--claim-session
   superchat-closed-loop-experience-correction-v0--claim-scope))

(defun superchat-closed-loop-experience-correction-v0--existing-correction ()
  "Return existing correction identities from the working DB, if present."
  (let* ((view (superchat-closed-loop-experience-correction-v0--current-claim-view))
         (claims (plist-get view :claims))
         (original
          (cl-find superchat-closed-loop-experience-correction-v0--initial-claim
                   claims
                   :key (lambda (claim) (plist-get claim :statement))
                   :test #'equal))
         (replacement
          (cl-find superchat-closed-loop-experience-correction-v0--corrected-claim
                   claims
                   :key (lambda (claim) (plist-get claim :statement))
                   :test #'equal)))
    (when (and original replacement
               (equal (plist-get original :claim-id)
                      (plist-get replacement :supersedes))
               (equal (plist-get replacement :claim-id)
                      (plist-get view :current)))
      (list :original original
            :replacement replacement
            :feedback (plist-get view :feedback)
            :view view))))

(defun superchat-closed-loop-experience-correction-v0--ensure-correction ()
  "Copy the v6 DB and record/replay the production Claim correction."
  (superchat-closed-loop-experience-correction-v0--copy-source-db)
  (superchat-db-close)
  (superchat-db-open)
  (let ((existing
         (superchat-closed-loop-experience-correction-v0--existing-correction)))
    (unless existing
      (let* ((rows
              (superchat-closed-loop-experience-correction-v0--source-rows))
             (_complete
              (unless (= 6 (length rows))
                (error "Frozen recovery evidence is incomplete")))
             (original
              (superchat-claim-record
               superchat-closed-loop-experience-correction-v0--initial-claim
               superchat-closed-loop-experience-correction-v0--claim-scope
               superchat-closed-loop-experience-correction-v0--initial-basis
               superchat-closed-loop-experience-correction-v0--claim-session
               "agent:auditor"))
             (correction
              (superchat-claim-correct
               (plist-get original :event-id)
               superchat-closed-loop-experience-correction-v0--corrected-claim
               superchat-closed-loop-experience-correction-v0--claim-scope
               superchat-closed-loop-experience-correction-v0--corrected-basis
               superchat-closed-loop-experience-correction-v0--correction-note
               superchat-closed-loop-experience-correction-v0--claim-session
               "user")))
        (setq existing
              (list :original original
                    :replacement (plist-get correction :replacement)
                    :feedback (list (plist-get correction :feedback))))))
    (superchat-db-close)
    (superchat-db-open)
    (let* ((view
            (superchat-closed-loop-experience-correction-v0--current-claim-view))
           (current (plist-get view :current))
           (original-id
            (plist-get (plist-get existing :original) :event-id))
           (replacement-id
            (plist-get (plist-get existing :replacement) :event-id))
           (integrity
            (caar (sqlite-select (superchat-db-open)
                                 "PRAGMA integrity_check"))))
      (unless (and (equal "ok" integrity)
                   (equal replacement-id current)
                   (superchat-closed-loop-experience-correction-v0--claim-by-id
                    original-id (plist-get view :claims))
                   (superchat-closed-loop-experience-correction-v0--claim-by-id
                    replacement-id (plist-get view :claims)))
        (error "Claim correction did not replay after DB reopen"))
      (let ((record
             (list :experiment_id
                   superchat-closed-loop-experience-correction-v0--experiment-id
                   :working_database
                   superchat-closed-loop-experience-correction-v0--working-db
                   :working_database_sha256
                   (superchat-closed-loop-experience-correction-v0--sha256-file
                    superchat-closed-loop-experience-correction-v0--working-db)
                   :source_database
                   superchat-closed-loop-experience-correction-v0--source-db
                   :source_database_sha256
                   superchat-closed-loop-experience-correction-v0--source-hash
                   :sqlite_integrity_check integrity
                   :claim_session
                   superchat-closed-loop-experience-correction-v0--claim-session
                   :scope
                   superchat-closed-loop-experience-correction-v0--claim-scope
                   :original_claim_id original-id
                   :replacement_claim_id replacement-id
                   :current_claim_id current
                   :history_retained t
                   :restart_replay "pass"
                   :view
                   (list :claims (vconcat (plist-get view :claims))
                         :feedback (vconcat (plist-get view :feedback))
                         :current current))))
        (superchat-closed-loop-experience-correction-v0--write-json
         superchat-closed-loop-experience-correction-v0--claim-record-path
         record)
        record))))

(defun superchat-closed-loop-experience-correction-v0--claim-record ()
  "Return the retained correction record, creating it when needed."
  (if (file-readable-p superchat-closed-loop-experience-correction-v0--claim-record-path)
      (superchat-closed-loop-experience-correction-v0--read-json
       superchat-closed-loop-experience-correction-v0--claim-record-path)
    (superchat-closed-loop-experience-correction-v0--ensure-correction)))

(defun superchat-closed-loop-experience-correction-v0--claim-line (claim)
  "Return one model-facing CLAIM line."
  (format "- claim_id: %s\n  statement: %s\n  scope: %s\n  basis: %s"
          (plist-get claim :claim-id)
          (plist-get claim :statement)
          (plist-get claim :scope)
          (json-encode (vconcat (plist-get claim :basis)))))

(defun superchat-closed-loop-experience-correction-v0-render-claim-block (arm)
  "Render the model-facing Claim block for ARM."
  (let* ((record
          (superchat-closed-loop-experience-correction-v0--claim-record))
         (view
          (superchat-closed-loop-experience-correction-v0--current-claim-view))
         (claims (plist-get view :claims))
         (original-id (plist-get record :original_claim_id))
         (current-id (plist-get view :current)))
    (pcase arm
      ("no-claim"
       (concat "BEGIN PRIOR CLAIM BLOCK\n"
               "No prior Claim is supplied for this assignment.\n"
               "END PRIOR CLAIM BLOCK"))
      ("stale-claim"
       (concat "BEGIN PRIOR CLAIM BLOCK\n"
               "Prior Claim selected by explicit Claim ID:\n"
               (superchat-closed-loop-experience-correction-v0--claim-line
                (superchat-closed-loop-experience-correction-v0--claim-by-id
                 original-id claims))
               "\nEND PRIOR CLAIM BLOCK"))
      ("corrected-current"
       (concat "BEGIN PRIOR CLAIM BLOCK\n"
               "Prior Claim selected from the replayed Current View after DB reopen:\n"
               (superchat-closed-loop-experience-correction-v0--claim-line
                (superchat-closed-loop-experience-correction-v0--claim-by-id
                 current-id claims))
               "\nEND PRIOR CLAIM BLOCK"))
      (_ (error "Unknown Claim arm: %s" arm)))))

(defun superchat-closed-loop-experience-correction-v0--fact-block ()
  "Return the shared six-fact model-facing block."
  (concat
   "Frozen Cohort v6 Tape facts supplied to every assignment:\n"
   (mapconcat
    (lambda (row)
      (format "- tape:%s session=%s kind=%s\n  %s"
              (plist-get row :storage_id)
              (plist-get row :session_id)
              (plist-get row :kind)
              (string-trim (plist-get row :content))))
    (superchat-closed-loop-experience-correction-v0--source-rows)
    "\n")))

(defun superchat-closed-loop-experience-correction-v0--query (assignment)
  "Return the frozen Research Workflow query for ASSIGNMENT."
  (let* ((arm
          (superchat-closed-loop-experience-correction-v0--assignment-arm
           assignment))
         (assignment-id
          (superchat-closed-loop-experience-correction-v0--assignment-id
           assignment))
         (task-id
          (superchat-closed-loop-experience-correction-v0--assignment-task-id
           assignment)))
    (format
     (concat "[Closed-loop Experience Correction v0; assignment %s; task %s; family %s]\n"
             "Question: %s\n\n"
             "%s\n\n"
             "%s\n\n"
             "Use only the supplied Tape facts, Claim block, and prior stage "
             "Artifacts. Distinguish observed facts from inference and cite the "
             "relevant tape:N references for every material conclusion.")
     assignment-id
     task-id
     (or (plist-get assignment :family) "closed-loop-correction")
     (superchat-closed-loop-experience-correction-v0--task-question
      assignment)
     (superchat-closed-loop-experience-correction-v0--fact-block)
     (superchat-closed-loop-experience-correction-v0-render-claim-block arm))))

(defun superchat-closed-loop-experience-correction-v0--semantic-base (query)
  "Return QUERY with assignment and Claim arm text removed."
  (let ((text query))
    (setq text
          (replace-regexp-in-string
           "\\[Closed-loop Experience Correction v0; assignment [^;]+; task \\([^;]+\\);"
           "[Closed-loop Experience Correction v0; assignment <assignment>; task \\1;"
           text t))
    (setq text
          (replace-regexp-in-string
           "BEGIN PRIOR CLAIM BLOCK\\(.\\|\n\\)*?END PRIOR CLAIM BLOCK"
           "BEGIN PRIOR CLAIM BLOCK\n<claim-arm>\nEND PRIOR CLAIM BLOCK"
           text t))
    text))

(defun superchat-closed-loop-experience-correction-v0--verify-rendering ()
  "Run model-free rendering assertions for all three Claim arms."
  (let* ((assignments
          (superchat-closed-loop-experience-correction-v0--assignments))
         (by-task
          (let (result)
            (dolist (assignment assignments result)
              (push assignment
                    (alist-get
                     (superchat-closed-loop-experience-correction-v0--assignment-task-id
                      assignment)
                     result nil nil #'equal))))))
    (dolist (arm '("no-claim" "stale-claim" "corrected-current"))
      (let ((block
             (superchat-closed-loop-experience-correction-v0-render-claim-block
              arm)))
        (unless (and (string-match-p "BEGIN PRIOR CLAIM BLOCK" block)
                     (string-match-p "END PRIOR CLAIM BLOCK" block))
          (error "Claim block delimiters missing for %s" arm))
        (pcase arm
          ("no-claim"
           (when (or (string-match-p
                      (regexp-quote
                       superchat-closed-loop-experience-correction-v0--initial-claim)
                      block)
                     (string-match-p
                      (regexp-quote
                       superchat-closed-loop-experience-correction-v0--corrected-claim)
                      block))
             (error "no-claim block leaked a Claim statement")))
          ("stale-claim"
           (unless (string-match-p
                    (regexp-quote
                     superchat-closed-loop-experience-correction-v0--initial-claim)
                    block)
             (error "stale-claim block did not select original Claim")))
          ("corrected-current"
           (unless (string-match-p
                    (regexp-quote
                     superchat-closed-loop-experience-correction-v0--corrected-claim)
                    block)
             (error "corrected-current block did not replay current Claim"))))))
    (dolist (group by-task)
      (let* ((queries
              (mapcar
               (lambda (assignment)
                 (superchat-closed-loop-experience-correction-v0--semantic-base
                  (superchat-closed-loop-experience-correction-v0--query
                   assignment)))
               (cdr group)))
             (first (car queries)))
        (unless (cl-every (lambda (query) (equal first query)) queries)
          (error "Non-arm prompt inputs differ for task %s" (car group))))))
  t)

(defun superchat-closed-loop-experience-correction-v0--tool-names (tools)
  "Normalize model-facing TOOLS to ordered names."
  (mapcar (lambda (tool)
            (cond ((stringp tool) tool)
                  ((symbolp tool) (symbol-name tool))
                  ((listp tool) (plist-get tool :name))
                  (t (format "%s" tool))))
          tools))

(defun superchat-closed-loop-experience-correction-v0--prompt-meta (prompt)
  "Return experiment metadata parsed from PROMPT, or nil."
  (let (assignment-id task-id family role assignment)
    (when (string-match
           "\\[Closed-loop Experience Correction v0; assignment \\([^;]+\\); task \\([^;]+\\); family \\([^]]+\\)\\]"
           (or prompt ""))
      (setq assignment-id (match-string 1 prompt)
            task-id (match-string 2 prompt)
            family (match-string 3 prompt)
            assignment
            (cl-find assignment-id
                     (superchat-closed-loop-experience-correction-v0--assignments)
                     :key
                     #'superchat-closed-loop-experience-correction-v0--assignment-id
                     :test #'equal)))
    (when (string-match
           "\\[research/\\(collector\\|analyst\\|reviewer\\)\\]"
           (or prompt ""))
      (setq role (match-string 1 prompt)))
    (when (and assignment-id task-id family role assignment)
      (list :assignment_id assignment-id
            :task_id task-id
            :arm
            (superchat-closed-loop-experience-correction-v0--assignment-arm
             assignment)
            :family family
            :role role))))

(defun superchat-closed-loop-experience-correction-v0--write-prompts ()
  "Persist observed model-facing prompt snapshots."
  (superchat-closed-loop-experience-correction-v0--write-json
   superchat-closed-loop-experience-correction-v0--prompt-record-path
   (list :experiment_id
         superchat-closed-loop-experience-correction-v0--experiment-id
         :snapshot_count
         (length superchat-closed-loop-experience-correction-v0--prompt-snapshots)
         :snapshots
         (vconcat
          (reverse
           superchat-closed-loop-experience-correction-v0--prompt-snapshots)))))

(defun superchat-closed-loop-experience-correction-v0--record-prompt
    (prompt run-id request &optional prepared no-write)
  "Record PROMPT with RUN-ID, REQUEST, and PREPARED query data.
When NO-WRITE is non-nil, return the snapshot without mutating records."
  (when-let* ((meta
               (superchat-closed-loop-experience-correction-v0--prompt-meta
                prompt)))
    (let* ((hash
            (superchat-closed-loop-experience-correction-v0--sha256-string
             prompt))
           (prepared (or prepared
                         (gethash hash
                                  superchat-closed-loop-experience-correction-v0--prepared-queries)))
           (snapshot
            (append
             meta
             (list :run_id run-id
                   :actor_id (plist-get request :actor-id)
                   :captured_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                   :model_prompt prompt
                   :model_prompt_sha256 hash
                   :system_prompt (plist-get prepared :system-prompt)
                   :system_prompt_sha256
                   (superchat-closed-loop-experience-correction-v0--sha256-string
                    (plist-get prepared :system-prompt))
                   :target_model
                   (or (plist-get prepared :target-model) "deepseek-v4-pro")
                   :provider
                   (if (and (boundp 'superchat-llm-backend)
                            (fboundp 'superchat--provider-name))
                       (superchat--provider-name superchat-llm-backend)
                     "deepseek-v4-pro")
                   :reasoning_mode (symbol-name superchat-llm-reasoning)
                   :connect_timeout_seconds llm-request-plz-connect-timeout
                   :prepared_tool_names
                   (vconcat
                    (superchat-closed-loop-experience-correction-v0--tool-names
                     (or (plist-get prepared :tools)
                         (plist-get request :prepared-tools))))))))
      (unless no-write
        (setq superchat-closed-loop-experience-correction-v0--prompt-snapshots
              (cons snapshot
                    (cl-remove-if
                     (lambda (old)
                       (and (equal run-id (plist-get old :run_id))
                            (equal (plist-get meta :role)
                                   (plist-get old :role))))
                     superchat-closed-loop-experience-correction-v0--prompt-snapshots)))
        (superchat-closed-loop-experience-correction-v0--write-prompts))
      snapshot)))

(defun superchat-closed-loop-experience-correction-v0--capture-query
    (original turn &rest args)
  "Call ORIGINAL with TURN and ARGS, retaining prepared prompt metadata."
  (let* ((result (apply original turn args))
         (prompt (plist-get result :prompt))
         (hash
          (superchat-closed-loop-experience-correction-v0--sha256-string
           prompt)))
    (when (superchat-closed-loop-experience-correction-v0--prompt-meta prompt)
      (puthash hash result
               superchat-closed-loop-experience-correction-v0--prepared-queries))
    result))

(defun superchat-closed-loop-experience-correction-v0--capture-runtime-submit
    (original request)
  "Call ORIGINAL with REQUEST, retaining the accepted Run ID."
  (let* ((prompt (plist-get request :prompt))
         (handle (funcall original request)))
    (superchat-closed-loop-experience-correction-v0--record-prompt
     prompt (plist-get handle :run-id) request)
    handle))

(defun superchat-closed-loop-experience-correction-v0--harden-stage-prompt
    (prompt)
  "Append the frozen deliverable guard to Research stage PROMPT."
  (concat prompt "\n\n"
          (string-trim-right
           (superchat-closed-loop-experience-correction-v0--read-file
            superchat-closed-loop-experience-correction-v0--deliverable-guard-path))))

(defun superchat-closed-loop-experience-correction-v0--load-records ()
  "Restore experiment-only launch and prompt records."
  (when (file-readable-p
         superchat-closed-loop-experience-correction-v0--launch-record-path)
    (setq superchat-closed-loop-experience-correction-v0--launches
          (reverse
           (plist-get
            (superchat-closed-loop-experience-correction-v0--read-json
             superchat-closed-loop-experience-correction-v0--launch-record-path)
            :launches))))
  (when (file-readable-p
         superchat-closed-loop-experience-correction-v0--prompt-record-path)
    (setq superchat-closed-loop-experience-correction-v0--prompt-snapshots
          (reverse
           (plist-get
            (superchat-closed-loop-experience-correction-v0--read-json
             superchat-closed-loop-experience-correction-v0--prompt-record-path)
            :snapshots)))))

(defun superchat-closed-loop-experience-correction-v0--write-launches ()
  "Persist closed-loop launch identities."
  (superchat-closed-loop-experience-correction-v0--write-json
   superchat-closed-loop-experience-correction-v0--launch-record-path
   (list :experiment_id
         superchat-closed-loop-experience-correction-v0--experiment-id
         :manifest_sha256
         (superchat-closed-loop-experience-correction-v0--sha256-file
          superchat-closed-loop-experience-correction-v0--manifest-path)
         :assignment_sha256
         (superchat-closed-loop-experience-correction-v0--sha256-file
          superchat-closed-loop-experience-correction-v0--assignments-path)
         :status "runs-started"
         :launch_count
         (length superchat-closed-loop-experience-correction-v0--launches)
         :launches
         (vconcat
          (reverse superchat-closed-loop-experience-correction-v0--launches))
         :historical_runs_imported json-false
         :held_out_used json-false
         :first_attempts_preserved t
         :rerun_substitution "prohibited"
         :effectiveness_claim "not_evaluated"
         :learning_claim "prohibited")))

(defun superchat-closed-loop-experience-correction-v0--assignment
    (run-index)
  "Return frozen assignment RUN-INDEX."
  (or (cl-find run-index
               (superchat-closed-loop-experience-correction-v0--assignments)
               :key
               #'superchat-closed-loop-experience-correction-v0--assignment-run-index)
      (error "Unknown closed-loop run index: %s" run-index)))

(defun superchat-closed-loop-experience-correction-v0--launch
    (run-index)
  "Return persisted launch RUN-INDEX, or nil."
  (cl-find run-index
           superchat-closed-loop-experience-correction-v0--launches
           :key (lambda (launch) (plist-get launch :run_index))))

(defun superchat-closed-loop-experience-correction-v0--settled-p
    (launch)
  "Return non-nil when LAUNCH cannot advance without a new decision."
  (let* ((view (superchat-research-workflow-view
                (plist-get launch :instance_id)))
         (steps (plist-get view :steps))
         (durably-settled
          (or (eq 'completed (plist-get view :status))
              (cl-some (lambda (step)
                         (memq (plist-get step :resume-decision)
                               '(manual-review stop)))
                       steps))))
    (when (and (eq t (plist-get launch :settled))
               (not durably-settled))
      (error "Persisted settlement for %s contradicts durable Workflow view"
             (plist-get launch :instance_id)))
    durably-settled))

(defun superchat-closed-loop-experience-correction-v0--preset ()
  "Return the frozen DeepSeek Research preset."
  (superchat--subagent-make-preset
   "researcher" "researcher"
   "DeepSeek closed-loop correction Research Workflow"
   'agent "research"
   superchat-closed-loop-experience-correction-v0--allowed-tools
   (concat
    "You are one stage of a frozen Research Workflow. Analyze only the "
    "supplied Tape facts, Claim block, and prior delivered Artifacts. Treat "
    "Claims as contestable interpretations, reconcile them with the supplied "
    "facts, cite tape:N references, and publish exactly one complete durable "
    "Artifact with workspace_publish. Do not finish or merely narrate a future "
    "action before publication succeeds.")))

(defun superchat-closed-loop-experience-correction-v0--bounded-process
    (program args max-characters)
  "Run PROGRAM with ARGS and return at most MAX-CHARACTERS."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         "/opt/homebrew/bin/gtimeout" nil t nil
                         "90s" program args)))
      (unless (and (integerp status) (zerop status))
        (error "%s failed with status %s: %s"
               program status (string-trim (buffer-string))))
      (let ((text (buffer-string)))
        (if (> (length text) max-characters)
            (concat (substring text 0 max-characters)
                    "\n[Result truncated by frozen tool limit]")
          text)))))

(defun superchat-closed-loop-experience-correction-v0-agent-reach-search
    (query &optional num-results)
  "Search the web for QUERY through the frozen Agent Reach adapter."
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (error "Search query must be non-empty"))
  (let* ((count (max 1 (min 8 (truncate (or num-results 5)))))
         (call (format "exa.web_search_exa(query: %s, numResults: %d)"
                       (json-encode-string query) count)))
    (superchat-closed-loop-experience-correction-v0--bounded-process
     "/Users/chenyibin/.bun/bin/mcporter" (list "call" call) 30000)))

(defun superchat-closed-loop-experience-correction-v0-agent-reach-read
    (url)
  "Read HTTP(S) URL through the frozen Jina Reader adapter."
  (unless (and (stringp url)
               (string-match-p "\\`https?://" url))
    (error "Reader URL must use HTTP or HTTPS"))
  (superchat-closed-loop-experience-correction-v0--bounded-process
   "/opt/local/bin/curl"
   (list "--silent" "--show-error" "--location" "--max-time" "80"
         (concat "https://r.jina.ai/" url))
   40000))

(defun superchat-closed-loop-experience-correction-v0--external-read-effects
    (args)
  "Return the frozen external-read effects for ARGS."
  (list (list :effect 'external-read :resource 'web :scope (car args))
        (list :effect 'process-exec :resource 'agent-reach
              :scope "frozen-read-only-adapter")))

(defun superchat-closed-loop-experience-correction-v0--register-agent-reach-tools
    ()
  "Register the frozen model-facing Agent Reach tools."
  (superchat-register-tool
   "agent_reach_search"
   :function
   #'superchat-closed-loop-experience-correction-v0-agent-reach-search
   :description
   "Search the public web through Agent Reach Exa. Prefer primary research and official specifications; retain source URLs."
   :args (list (list :name "query" :type 'string
                     :description "Precise web search query.")
               (list :name "num-results" :type 'number :optional t
                     :description "Number of results, from 1 to 8."))
   :effects
   #'superchat-closed-loop-experience-correction-v0--external-read-effects)
  (superchat-register-tool
   "agent_reach_read"
   :function
   #'superchat-closed-loop-experience-correction-v0-agent-reach-read
   :description
   "Read one public HTTP(S) page through Agent Reach Jina Reader and return bounded Markdown."
   :args (list (list :name "url" :type 'string
                     :description "Public HTTP(S) URL to read."))
   :effects
   #'superchat-closed-loop-experience-correction-v0--external-read-effects))

(defun superchat-closed-loop-experience-correction-v0--permission
    (tool-name _args)
  "Allow only the frozen closed-loop TOOL-NAME surface."
  (if (member tool-name superchat-closed-loop-experience-correction-v0--allowed-tools)
      'allow
    'deny))

(defun superchat-closed-loop-experience-correction-v0--configure-surface ()
  "Install the frozen tools, preset, and prompt capture advices."
  (unless (fboundp 'make-llm-deepseek)
    (error "llm-deepseek is unavailable"))
  (superchat-closed-loop-experience-correction-v0--register-agent-reach-tools)
  (setcdr (assoc "researcher" superchat--subagent-builtin-presets)
          #'superchat-closed-loop-experience-correction-v0--preset)
  (setq superchat-agent-permission-functions
        '(superchat-closed-loop-experience-correction-v0--permission))
  (unless (advice-member-p
           #'superchat-closed-loop-experience-correction-v0--harden-stage-prompt
           'superchat-research-workflow--stage-prompt)
    (advice-add 'superchat-research-workflow--stage-prompt :filter-return
                #'superchat-closed-loop-experience-correction-v0--harden-stage-prompt))
  (unless (advice-member-p
           #'superchat-closed-loop-experience-correction-v0--capture-query
           'superchat--execute-llm-query)
    (advice-add 'superchat--execute-llm-query :around
                #'superchat-closed-loop-experience-correction-v0--capture-query))
  (unless (advice-member-p
           #'superchat-closed-loop-experience-correction-v0--capture-runtime-submit
           'superchat-runtime-submit)
    (advice-add 'superchat-runtime-submit :around
                #'superchat-closed-loop-experience-correction-v0--capture-runtime-submit)))

(defun superchat-closed-loop-experience-correction-v0--prompt-capture-fixture ()
  "Run a model-free prompt/run-id capture fixture."
  (let* ((prompt
          (concat
           "[research/collector] Question: "
           "[Closed-loop Experience Correction v0; assignment "
           "clf-evidence-synthesis-01-no-claim; "
           "task clf-evidence-synthesis-01; family evidence-synthesis]\n"
           "Question: Fixture?\n\nBEGIN PRIOR CLAIM BLOCK\n"
           "No prior Claim is supplied for this assignment.\n"
           "END PRIOR CLAIM BLOCK"))
         (request (list :prompt prompt :actor-id "fixture-actor"))
         (prepared
          (list :system-prompt "fixture-system"
                :target-model "deepseek-v4-pro"
                :tools (mapcar (lambda (name) (list :name name))
                               superchat-closed-loop-experience-correction-v0--allowed-tools)))
         (snapshot
          (superchat-closed-loop-experience-correction-v0--record-prompt
           prompt "run-fixture" request prepared t)))
    (unless (and snapshot
                 (equal "run-fixture" (plist-get snapshot :run_id))
                 (equal "clf-evidence-synthesis-01-no-claim"
                        (plist-get snapshot :assignment_id))
                 (equal "collector" (plist-get snapshot :role))
                 (string-prefix-p
                  "sha256:" (plist-get snapshot :model_prompt_sha256)))
      (error "Prompt capture fixture failed"))
    t))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v0-preflight ()
  "Run model-free preflight checks for the closed-loop runner."
  (interactive)
  (superchat-closed-loop-experience-correction-v0--verify-frozen-inputs)
  (superchat-closed-loop-experience-correction-v0--ensure-correction)
  (superchat-closed-loop-experience-correction-v0--verify-rendering)
  (superchat-closed-loop-experience-correction-v0--prompt-capture-fixture)
  (let ((record
         (list :experiment_id
               superchat-closed-loop-experience-correction-v0--experiment-id
               :status "pass"
               :generated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
               :manifest_sha256
               (superchat-closed-loop-experience-correction-v0--sha256-file
                superchat-closed-loop-experience-correction-v0--manifest-path)
               :assignments_sha256
               (superchat-closed-loop-experience-correction-v0--sha256-file
                superchat-closed-loop-experience-correction-v0--assignments-path)
               :source_database_sha256
               superchat-closed-loop-experience-correction-v0--source-hash
               :working_database
               superchat-closed-loop-experience-correction-v0--working-db
               :working_database_sha256
               (superchat-closed-loop-experience-correction-v0--sha256-file
                superchat-closed-loop-experience-correction-v0--working-db)
               :claim_replay "pass"
               :render_assertions "pass"
               :prompt_capture_fixture "pass"
               :held_out_accessed json-false
               :model_calls_started
               (superchat-closed-loop-experience-correction-v0--json-bool
                superchat-closed-loop-experience-correction-v0--launches))))
    (superchat-closed-loop-experience-correction-v0--write-json
     superchat-closed-loop-experience-correction-v0--preflight-record-path
     record)
    (when (called-interactively-p 'interactive)
      (message "Closed-loop correction preflight passed"))
    record))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v0-start (run-index)
  "Start one frozen closed-loop RUN-INDEX."
  (interactive "nClosed-loop run index (1-24): ")
  (superchat-closed-loop-experience-correction-v0-preflight)
  (superchat-closed-loop-experience-correction-v0--load-records)
  (superchat-closed-loop-experience-correction-v0--configure-surface)
  (when (superchat-closed-loop-experience-correction-v0--launch run-index)
    (error "Closed-loop run index already started: %s" run-index))
  (when-let* ((previous
               (car superchat-closed-loop-experience-correction-v0--launches)))
    (unless (superchat-closed-loop-experience-correction-v0--settled-p
             previous)
      (error "Previous closed-loop run is still active: %s"
             (plist-get previous :run_index))))
  (let* ((assignment
          (superchat-closed-loop-experience-correction-v0--assignment
           run-index))
         (default-directory superchat-closed-loop-experience-correction-v0--root)
         (superchat--active-authority-policy "default")
         (instance-id
          (superchat-research-workflow
           (superchat-closed-loop-experience-correction-v0--query
            assignment)))
         (view (superchat-research-workflow-view instance-id))
         (collector (car (plist-get view :steps)))
         (launch
          (list :run_index run-index
                :manifest_revision
                (plist-get
                 (superchat-closed-loop-experience-correction-v0--manifest)
                 :manifest_revision)
                :manifest_sha256
                (superchat-closed-loop-experience-correction-v0--sha256-file
                 superchat-closed-loop-experience-correction-v0--manifest-path)
                :assignment_id
                (superchat-closed-loop-experience-correction-v0--assignment-id
                 assignment)
                :task_id
                (superchat-closed-loop-experience-correction-v0--assignment-task-id
                 assignment)
                :arm
                (superchat-closed-loop-experience-correction-v0--assignment-arm
                 assignment)
                :family (plist-get assignment :family)
                :instance_id instance-id
                :actor_id (plist-get collector :actor-id)
                :run_id (plist-get collector :run-id)
                :first_attempt t
                :first_attempt_id
                (format
                 "%s/attempt-1"
                 (superchat-closed-loop-experience-correction-v0--assignment-id
                  assignment))
                :started_at (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (push launch superchat-closed-loop-experience-correction-v0--launches)
    (superchat-closed-loop-experience-correction-v0--write-launches)
    launch))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v0-finalize (run-index)
  "Persist terminal evidence and Outcome for closed-loop RUN-INDEX."
  (interactive "nClosed-loop run index to finalize: ")
  (superchat-closed-loop-experience-correction-v0--load-records)
  (let* ((assignment
          (superchat-closed-loop-experience-correction-v0--assignment
           run-index))
         (launch
          (or (superchat-closed-loop-experience-correction-v0--launch
               run-index)
              (error "Closed-loop run index has not started: %s" run-index)))
         (view (superchat-research-workflow-view
                (plist-get launch :instance_id)))
         (_settled
          (unless (superchat-closed-loop-experience-correction-v0--settled-p
                   launch)
            (error "Closed-loop run is still active: %s" run-index)))
         (outcome
          (if (fboundp
               'superchat-closed-loop-experience-correction-v0--evaluate)
              (superchat-closed-loop-experience-correction-v0--evaluate
               assignment launch view)
            (error "Load evidence.el before finalizing closed-loop runs")))
         (evidence
          (superchat-closed-loop-experience-correction-v0--build-evidence
           assignment launch view outcome))
         (updated
          (append launch
                  (list :workflow_status
                        (symbol-name (plist-get view :status))
                        :settled
                        (superchat-closed-loop-experience-correction-v0--json-bool
                         (superchat-closed-loop-experience-correction-v0--settled-p
                          launch))
                        :steps (vconcat (plist-get view :steps))
                        :outcome_record (plist-get evidence :outcome_record)
                        :evidence_closure
                        (plist-get evidence :evidence_closure)
                        :finalized_at
                        (format-time-string "%Y-%m-%dT%H:%M:%S%z")))))
    (setq superchat-closed-loop-experience-correction-v0--launches
          (cons updated
                (cl-remove-if
                 (lambda (old)
                   (= run-index (plist-get old :run_index)))
                 superchat-closed-loop-experience-correction-v0--launches)))
    (superchat-closed-loop-experience-correction-v0--write-launches)
    updated))

(defun superchat-closed-loop-experience-correction-v0--wait (launch)
  "Wait until LAUNCH reaches a durable settlement point."
  (while (not (superchat-closed-loop-experience-correction-v0--settled-p launch))
    (accept-process-output nil 1)))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v0-run-all ()
  "Execute and finalize all 24 frozen first attempts sequentially."
  (interactive)
  (superchat-closed-loop-experience-correction-v0-preflight)
  (superchat-closed-loop-experience-correction-v0--load-records)
  (dolist
      (assignment
       (sort
        (copy-sequence
         (superchat-closed-loop-experience-correction-v0--assignments))
        (lambda (left right)
          (< (superchat-closed-loop-experience-correction-v0--assignment-run-index
              left)
             (superchat-closed-loop-experience-correction-v0--assignment-run-index
              right)))))
    (let* ((run-index
            (superchat-closed-loop-experience-correction-v0--assignment-run-index
             assignment))
           (assignment-id
            (superchat-closed-loop-experience-correction-v0--assignment-id
             assignment))
           (evidence-path
            (expand-file-name
             "evidence-bundle.json"
             (expand-file-name
              (format "runs/%s/" assignment-id)
              superchat-closed-loop-experience-correction-v0--directory))))
      (unless (file-readable-p evidence-path)
        (let ((launch
               (or (superchat-closed-loop-experience-correction-v0--launch
                    run-index)
                   (superchat-closed-loop-experience-correction-v0-start
                    run-index))))
          (message "Closed-loop %02d/24 started: %s"
                   run-index assignment-id)
          (superchat-closed-loop-experience-correction-v0--wait launch)
          (superchat-closed-loop-experience-correction-v0-finalize run-index)
          (message "Closed-loop %02d/24 finalized" run-index)))))
  (list :status "complete"
        :launch_count
        (length superchat-closed-loop-experience-correction-v0--launches)))

(make-directory superchat-closed-loop-experience-correction-v0--root t)
(superchat-closed-loop-experience-correction-v0--load-records)

(provide 'superchat-closed-loop-experience-correction-v0-run)

;;; run.el ends here
