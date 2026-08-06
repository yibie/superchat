;;; run-human-correction-v0.el --- Real durable correction canary -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)

(defconst superchat-experience-feedback-v0--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst superchat-experience-feedback-v0--root
  (expand-file-name "../.." superchat-experience-feedback-v0--directory))

(add-to-list 'load-path superchat-experience-feedback-v0--root)

(require 'superchat-claims)
(require 'superchat-db)

(defconst superchat-experience-feedback-v0--source-db
  (expand-file-name
   "experiments/experience-cohort-deepseek-v6/runtime-snapshot.db"
   superchat-experience-feedback-v0--root))
(defconst superchat-experience-feedback-v0--source-hash
  "dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c")
(defconst superchat-experience-feedback-v0--report
  (expand-file-name "human-correction-v0-report.json"
                    superchat-experience-feedback-v0--directory))
(defconst superchat-experience-feedback-v0--session
  "subagent-researcher-20260805-124435-55dd")
(defconst superchat-experience-feedback-v0--scope
  "experience-cohort-deepseek-v6/discovery")
(defconst superchat-experience-feedback-v0--initial-basis
  '("tape:1577" "tape:1578" "tape:1580"))
(defconst superchat-experience-feedback-v0--corrected-basis
  '("tape:961" "tape:962" "tape:964"
    "tape:1577" "tape:1578" "tape:1580"))

(defun superchat-experience-feedback-v0--file-hash (path)
  "Return PATH's SHA-256."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun superchat-experience-feedback-v0--source-rows ()
  "Return the six frozen recovery rows as JSON-ready objects."
  (vconcat
   (mapcar
    (lambda (row)
      (list :storage_id (nth 0 row)
            :session_id (nth 1 row)
            :kind (nth 2 row)
            :content (nth 3 row)))
    (superchat-db-tape-select
     "SELECT id, session_id, kind, content FROM tape
      WHERE id IN (?, ?, ?, ?, ?, ?) ORDER BY id"
     '(961 962 964 1577 1578 1580)))))

(defun superchat-experience-feedback-v0--run ()
  "Run the real Cohort v6 correction and write its evidence report."
  (let* ((temporary-directory
          (make-temp-file "superchat-human-correction-v0-" t))
         (working-db (expand-file-name "superchat.db" temporary-directory))
         (superchat-data-directory temporary-directory)
         (source-before
          (superchat-experience-feedback-v0--file-hash
           superchat-experience-feedback-v0--source-db)))
    (unwind-protect
        (progn
          (unless (equal source-before
                         superchat-experience-feedback-v0--source-hash)
            (error "Frozen Cohort v6 database hash changed"))
          (copy-file superchat-experience-feedback-v0--source-db working-db)
          (let ((working-before
                 (superchat-experience-feedback-v0--file-hash working-db)))
            (superchat-db-close)
            (superchat-db-open)
            (let* ((source-rows
                    (superchat-experience-feedback-v0--source-rows))
                   (_rows-complete
                    (unless (= 6 (length source-rows))
                      (error "Frozen recovery evidence is incomplete")))
                   (original
                    (superchat-claim-record
                     "After agent_reach_read fails, switching to agent_reach_search is a stable recovery strategy."
                     superchat-experience-feedback-v0--scope
                     superchat-experience-feedback-v0--initial-basis
                     superchat-experience-feedback-v0--session
                     "agent:auditor"))
                   (correction
                    (superchat-claim-correct
                     (plist-get original :event-id)
                     "Cohort v6 contains two non-matching recovery episodes; no stable recovery strategy is established."
                     superchat-experience-feedback-v0--scope
                     superchat-experience-feedback-v0--corrected-basis
                     "The two eligible failures have different follow-ups: one repeats agent_reach_read and one switches to agent_reach_search."
                     superchat-experience-feedback-v0--session
                     "user"))
                   (replacement (plist-get correction :replacement))
                   (feedback-record (plist-get correction :feedback)))
              (superchat-db-close)
              (let ((working-after
                     (superchat-experience-feedback-v0--file-hash working-db))
                    (source-after
                     (superchat-experience-feedback-v0--file-hash
                      superchat-experience-feedback-v0--source-db)))
                (unless (equal source-before source-after)
                  (error "The frozen source database was modified"))
                (unless (not (equal working-before working-after))
                  (error "The writable database did not change"))
                (superchat-db-open)
                (let* ((view
                        (superchat-claims-current-view
                         superchat-experience-feedback-v0--session
                         superchat-experience-feedback-v0--scope))
                       (claims (plist-get view :claims))
                       (feedback (plist-get view :feedback))
                       (replacement-id (plist-get replacement :event-id))
                       (integrity
                        (caar (sqlite-select (superchat-db-open)
                                             "PRAGMA integrity_check"))))
                  (unless (and (= 2 (length claims))
                               (= 1 (length feedback))
                               (equal replacement-id (plist-get view :current))
                               (equal (plist-get original :event-id)
                                      (plist-get (car feedback) :target))
                               (equal replacement-id
                                      (plist-get (car feedback) :replacement))
                               (equal "ok" integrity))
                    (error "Persistent correction replay did not close"))
                  (let ((json-encoding-pretty-print t)
                        (report
                         (list
                          :experiment_id "persistent-human-correction-v0"
                          :generated_at
                          (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                          :status "pass"
                          :source_database
                          "experiments/experience-cohort-deepseek-v6/runtime-snapshot.db"
                          :source_sha256_before source-before
                          :source_sha256_after source-after
                          :source_unchanged t
                          :working_copy_archived json-false
                          :working_sha256_before working-before
                          :working_sha256_after working-after
                          :sqlite_integrity_check integrity
                          :claim_session superchat-experience-feedback-v0--session
                          :scope superchat-experience-feedback-v0--scope
                          :source_rows source-rows
                          :inserted
                          (list :original original
                                :replacement replacement
                                :feedback feedback-record)
                          :replay
                          (list :claims (vconcat claims)
                                :feedback (vconcat feedback)
                                :current (plist-get view :current))
                          :restart_replay "pass"
                          :prohibited_claims
                          ["causal recovery"
                           "effectiveness"
                           "learning"
                           "Memory promotion"
                           "policy or Authority change"])))
                    (with-temp-file superchat-experience-feedback-v0--report
                      (insert (json-encode report) "\n"))))))))
      (superchat-db-close)
      (delete-directory temporary-directory t)))
  (princ (format "Persistent Human Correction v0 passed: %s\n"
                 superchat-experience-feedback-v0--report)))

(superchat-experience-feedback-v0--run)

;;; run-human-correction-v0.el ends here
