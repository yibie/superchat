;;; remediation-logic-v0.2.el --- PROTOTYPE: strict semantic-use scorer -*- lexical-binding: t; -*-

;; Throwaway, pure offline planning logic.  It does not load Superchat, call a
;; provider, use a database, or inspect Runtime state.  The record fields are
;; synthetic witnesses whose only purpose is to exercise fail-closed gates.

(require 'cl-lib)
(require 'subr-x)

(defconst semuse-v02-claim-mode 'descriptive-consistency)

(defconst semuse-v02-seed
  "artifact-semantic-use-v0.1-remediation-2026-08-09")

(defconst semuse-v02-conditions
  '(world-a world-b decoy sham withheld))

(defconst semuse-v02-scenarios
  '(passing v-only x-only missing-content-witness missing-withheld
    parser-suffix duplicate extra missing-record
    wrong-first-later-replacement truthy-false empty-schedule schedule-drift
    lowercase-answer bad-decoy sham-target oracle-exposed))

(defconst semuse-v02-families
  '((:id scale
     :card "CARD-SCALE-QZ"
     :world-a "MUL-007-ZQ"
     :world-b "MUL-013-ZQ"
     :decoy "MUL-019-ZQ"
     :sham "MUL-XXX-ZQ"
     :x1 "PAY-008-RV"
     :x2 "PAY-012-RV")
    (:id offset
     :card "CARD-OFFSET-QZ"
     :world-a "OFF-P027-ZQ"
     :world-b "OFF-N014-ZQ"
     :decoy "OFF-P043-ZQ"
     :sham "OFF-X000-ZQ"
     :x1 "RAW-315-RV"
     :x2 "RAW-428-RV")
    (:id permute
     :card "CARD-PERMUTE-QZ"
     :world-a "ORD-2314-ZQ"
     :world-b "ORD-4132-ZQ"
     :decoy "ORD-3241-ZQ"
     :sham "ORD-XXXX-ZQ"
     :x1 "SEQ-R7KM-RV"
     :x2 "SEQ-T9BX-RV")))

(defconst semuse-v02-rows
  '((:id "scale-x1" :family scale :x-key :x1)
    (:id "scale-x2" :family scale :x-key :x2)
    (:id "offset-x1" :family offset :x-key :x1)
    (:id "offset-x2" :family offset :x-key :x2)
    (:id "permute-x1" :family permute :x-key :x1)
    (:id "permute-x2" :family permute :x-key :x2)))

;; This is the authoritative, expanded 30-entry schedule.  The assignment
;; code, position, row, and condition are all frozen fields, not regenerated
;; by validation or inferred from record order.
(defconst semuse-v02-frozen-schedule
  '((:position 1 :assignment-code "ASG-BF514E87CEEE"
     :row-id "scale-x1" :condition world-a)
    (:position 2 :assignment-code "ASG-D91AAC285E58"
     :row-id "offset-x1" :condition decoy)
    (:position 3 :assignment-code "ASG-FD94BAAFC5DB"
     :row-id "permute-x1" :condition withheld)
    (:position 4 :assignment-code "ASG-005C6D92523C"
     :row-id "scale-x2" :condition world-b)
    (:position 5 :assignment-code "ASG-F387700C2CCD"
     :row-id "offset-x2" :condition sham)
    (:position 6 :assignment-code "ASG-B2D847926362"
     :row-id "permute-x2" :condition world-a)

    (:position 7 :assignment-code "ASG-2F376B58C7B1"
     :row-id "offset-x2" :condition withheld)
    (:position 8 :assignment-code "ASG-C563A052A268"
     :row-id "scale-x2" :condition decoy)
    (:position 9 :assignment-code "ASG-F458E94F8D92"
     :row-id "permute-x2" :condition world-b)
    (:position 10 :assignment-code "ASG-F79C9EADE560"
     :row-id "offset-x1" :condition sham)
    (:position 11 :assignment-code "ASG-414B95EEB950"
     :row-id "scale-x1" :condition world-b)
    (:position 12 :assignment-code "ASG-788EF91325FF"
     :row-id "permute-x1" :condition world-a)

    (:position 13 :assignment-code "ASG-78C122A09673"
     :row-id "permute-x1" :condition world-b)
    (:position 14 :assignment-code "ASG-4F912E84C684"
     :row-id "scale-x1" :condition decoy)
    (:position 15 :assignment-code "ASG-43E1FBCADAFE"
     :row-id "offset-x2" :condition world-a)
    (:position 16 :assignment-code "ASG-FCAE9F1FB481"
     :row-id "permute-x2" :condition decoy)
    (:position 17 :assignment-code "ASG-109C815EB99A"
     :row-id "scale-x2" :condition sham)
    (:position 18 :assignment-code "ASG-3C0193C57BC5"
     :row-id "offset-x1" :condition withheld)

    (:position 19 :assignment-code "ASG-2379824BD54B"
     :row-id "scale-x2" :condition withheld)
    (:position 20 :assignment-code "ASG-6094BC02B26C"
     :row-id "permute-x2" :condition sham)
    (:position 21 :assignment-code "ASG-47A6DC1F9E3C"
     :row-id "offset-x1" :condition world-a)
    (:position 22 :assignment-code "ASG-B126AE8E642B"
     :row-id "scale-x1" :condition sham)
    (:position 23 :assignment-code "ASG-63FA23C90A47"
     :row-id "permute-x1" :condition decoy)
    (:position 24 :assignment-code "ASG-FF78766337A0"
     :row-id "offset-x2" :condition world-b)

    (:position 25 :assignment-code "ASG-3667D8F00897"
     :row-id "permute-x2" :condition withheld)
    (:position 26 :assignment-code "ASG-FB0B7919EB79"
     :row-id "offset-x2" :condition decoy)
    (:position 27 :assignment-code "ASG-8A21BD88E4B8"
     :row-id "scale-x1" :condition withheld)
    (:position 28 :assignment-code "ASG-891E0ACC431C"
     :row-id "permute-x1" :condition sham)
    (:position 29 :assignment-code "ASG-35D97662066D"
     :row-id "offset-x1" :condition world-b)
    (:position 30 :assignment-code "ASG-4853FE793693"
     :row-id "scale-x2" :condition world-a)))

(defconst semuse-v02-expected-schedule-hash
  "ca0a19a6f273033ac0858de65e9ead70a57d0c664837eac622be38faa9db21bf")

(defun semuse-v02--utf8-bytes (text)
  "Return TEXT encoded as exact UTF-8 bytes." 
  (encode-coding-string text 'utf-8 t))

(defun semuse-v02-raw-output-sha256 (text)
  "Hash TEXT's UTF-8 output bytes." 
  (when (stringp text)
    (secure-hash 'sha256 (semuse-v02--utf8-bytes text))))

(defun semuse-v02--proved-p (value)
  "Return non-nil only for the one accepted proof enum." 
  (eq value 'proved))

(defun semuse-v02--has-key-p (plist key)
  "Return non-nil when PLIST contains KEY, even if its value is nil." 
  (let ((tail plist) found)
    (while (and (consp tail) (not found))
      (setq found (eq (car tail) key)
            tail (cddr tail)))
    found))

(defun semuse-v02--nonempty-string-p (value)
  "Return non-nil for a non-empty string." 
  (and (stringp value) (> (length value) 0)))

(defun semuse-v02-family (family-id)
  "Return the family named FAMILY-ID." 
  (cl-find family-id semuse-v02-families
           :key (lambda (family) (plist-get family :id))))

(defun semuse-v02-row (row-id)
  "Return the row named ROW-ID." 
  (cl-find row-id semuse-v02-rows
           :key (lambda (row) (plist-get row :id))
           :test #'string=))

(defun semuse-v02-row-family (row)
  "Return ROW's family specification." 
  (semuse-v02-family (plist-get row :family)))

(defun semuse-v02-row-x (row)
  "Return ROW's task-only input token." 
  (plist-get (semuse-v02-row-family row) (plist-get row :x-key)))

(defun semuse-v02-parameter (family condition)
  "Return FAMILY's parameter for CONDITION, or nil for WITHHELD." 
  (pcase condition
    ('world-a (plist-get family :world-a))
    ('world-b (plist-get family :world-b))
    ('decoy (plist-get family :decoy))
    ('sham (plist-get family :sham))
    ('withheld nil)
    (_ (error "Unknown condition: %S" condition))))

(defun semuse-v02-artifact (family condition)
  "Render the model-visible artifact for FAMILY and CONDITION." 
  (unless (eq condition 'withheld)
    (format (concat "SEMANTIC-USE OPERATIONS CARD\n"
                    "CARD=%s\n"
                    "FAMILY=%s\n"
                    "DECISIVE-PARAMETER=%s\n"
                    "STATUS=ACTIVE\n")
            (plist-get family :card)
            (upcase (symbol-name (plist-get family :id)))
            (semuse-v02-parameter family condition))))

(defun semuse-v02--payload-value (row)
  "Decode ROW's task-only input token." 
  (let ((x (semuse-v02-row-x row)))
    (pcase (plist-get row :family)
      ((or 'scale 'offset) (string-to-number (substring x 4 7)))
      ('permute (substring x 4 8))
      (_ (error "Unknown family")))))

(defun semuse-v02--apply (row parameter)
  "Apply ROW's family operation to PARAMETER." 
  (pcase (plist-get row :family)
    ('scale
     (format "RES-%05d-KC"
             (* (string-to-number (substring parameter 4 7))
                (semuse-v02--payload-value row))))
    ('offset
     (let* ((sign (if (eq (aref parameter 4) ?P) 1 -1))
            (delta (* sign (string-to-number (substring parameter 5 8)))))
       (format "RES-%05d-KC" (+ (semuse-v02--payload-value row) delta))))
    ('permute
     (let ((payload (semuse-v02--payload-value row))
           (order (substring parameter 4 8)))
       (format "RES-%s-KC"
               (mapconcat
                (lambda (digit)
                  (string (aref payload (- digit ?1))))
                order ""))))
    (_ (error "Unknown family"))))

(defun semuse-v02-target (row condition)
  "Return ROW's deterministic target for CONDITION." 
  (if (memq condition '(world-a world-b decoy))
      (semuse-v02--apply
       row (semuse-v02-parameter (semuse-v02-row-family row) condition))
    "INSUFFICIENT"))

(defun semuse-v02--operation-text (family-id)
  "Return the model-facing operation for FAMILY-ID." 
  (pcase family-id
    ('scale
     (concat "Decode MUL-NNN-ZQ as unsigned decimal NNN. Multiply it by "
             "PAY-NNN-RV's NNN. Format RES-DDDDD-KC."))
    ('offset
     (concat "Decode OFF-SNNN-ZQ with P=plus and N=minus. Add it to "
             "RAW-NNN-RV's NNN. Format RES-DDDDD-KC."))
    ('permute
     (concat "Decode ORD-DDDD-ZQ as four 1-based positions. Reorder "
             "SEQ-CCCC-RV's CCCC. Format RES-CCCC-KC."))))

(defun semuse-v02-prompt (row)
  "Render the same task prompt for every condition of ROW." 
  (let ((family (semuse-v02-row-family row)))
    (format
     (concat "Complete one isolated calibration task.\n"
             "Call workspace_info. If exactly one Artifact exists, read its "
             "exact revision before answering.\n"
             "Use DECISIVE-PARAMETER from %s. If no Artifact exists or the "
             "parameter is not concrete, return ANSWER: INSUFFICIENT.\n"
             "Task payload: %s\n"
             "Operation: %s\n"
             "Return exactly one ANSWER line and nothing else.\n")
     (plist-get family :card)
     (semuse-v02-row-x row)
     (semuse-v02--operation-text (plist-get family :id)))))

(defun semuse-v02-extract (raw-output)
  "Extract one exact canonical answer from RAW-OUTPUT.

The case-fold binding is local so an ambient Emacs setting cannot relax the
protocol.  A suffix, prefix variation, extra line, or empty value is invalid." 
  (cond
   ((not (stringp raw-output))
    '(:status missing-output :value nil))
   ((let ((case-fold-search nil))
      (string-match
       (concat "\\`ANSWER: "
               "\\(INSUFFICIENT\\|RES-[0-9]\\{5\\}-KC"
               "\\|RES-[A-Z0-9]\\{4\\}-KC\\)"
               "\\(?:\\r?\\n\\)?\\'")
       raw-output))
    (list :status 'valid :value (match-string 1 raw-output)))
   (t '(:status protocol-failure :value nil))))

(defun semuse-v02--schedule-key (entry)
  "Return a unique key for a schedule ENTRY's frozen fields." 
  (format "%s|%s|%s|%s"
          (plist-get entry :position)
          (plist-get entry :assignment-code)
          (plist-get entry :row-id)
          (plist-get entry :condition)))

(defun semuse-v02--cell-key (row-id condition)
  "Return the row/condition key used only after a gate succeeds." 
  (format "%s|%s" row-id condition))

(defun semuse-v02-schedule-canonical (schedule)
  "Return the canonical text for explicit SCHEDULE.

There is deliberately no default argument: nil is the empty schedule." 
  (mapconcat
   (lambda (entry)
     (format "%02d|%s|%s|%s"
             (plist-get entry :position)
             (plist-get entry :assignment-code)
             (plist-get entry :row-id)
             (plist-get entry :condition)))
   schedule "\n"))

(defun semuse-v02-schedule-hash (schedule)
  "Return the SHA-256 of explicit SCHEDULE." 
  (secure-hash 'sha256 (semuse-v02-schedule-canonical schedule)))

(defun semuse-v02-schedule-validation (schedule)
  "Validate explicit SCHEDULE against the frozen 30-entry schedule." 
  (let ((errors nil)
        (expected-by-key (make-hash-table :test #'equal))
        (seen-key (make-hash-table :test #'equal))
        (seen-cell (make-hash-table :test #'equal))
        (seen-position (make-hash-table :test #'eql))
        (seen-code (make-hash-table :test #'equal)))
    (dolist (entry semuse-v02-frozen-schedule)
      (puthash (semuse-v02--schedule-key entry) t expected-by-key))
    (unless (proper-list-p schedule)
      (push "schedule-not-list" errors)
      (setq schedule nil))
    (unless (= 30 (length schedule))
      (push "schedule-count" errors))
    (unless (equal (semuse-v02-schedule-hash schedule)
                   semuse-v02-expected-schedule-hash)
      (push "schedule-hash" errors))
    (dolist (entry schedule)
      (if (not (proper-list-p entry))
          (push "schedule-entry-malformed" errors)
        (let* ((key (semuse-v02--schedule-key entry))
               (position (plist-get entry :position))
               (code (plist-get entry :assignment-code))
               (row-id (plist-get entry :row-id))
               (condition (plist-get entry :condition))
               (cell (semuse-v02--cell-key row-id condition)))
          (unless (gethash key expected-by-key)
            (push (format "schedule-entry:%s" key) errors))
          (when (gethash key seen-key)
            (push (format "schedule-duplicate:%s" key) errors))
          (puthash key t seen-key)
          (unless (and (integerp position) (>= position 1) (<= position 30))
            (push (format "schedule-position:%s" position) errors))
          (when (gethash position seen-position)
            (push (format "schedule-position-duplicate:%s" position) errors))
          (puthash position t seen-position)
          (when (gethash code seen-code)
            (push (format "schedule-code-duplicate:%s" code) errors))
          (puthash code t seen-code)
          (puthash cell (1+ (gethash cell seen-cell 0)) seen-cell))))
    (dolist (entry semuse-v02-frozen-schedule)
      (let ((key (semuse-v02--schedule-key entry)))
        (unless (gethash key seen-key)
          (push (format "schedule-missing:%s" key) errors))))
    (dolist (row semuse-v02-rows)
      (dolist (condition semuse-v02-conditions)
        (let ((cell (semuse-v02--cell-key (plist-get row :id) condition)))
          (unless (= 1 (gethash cell seen-cell 0))
            (push (format "schedule-cell:%s" cell) errors)))))
    (dotimes (round 5)
      (let ((start (* round 6))
            (end (* (1+ round) 6)))
        (if (< (length schedule) end)
            (push (format "round-short:%d" (1+ round)) errors)
          (let* ((entries (cl-subseq schedule start end))
                 (counts (mapcar
                          (lambda (condition)
                            (cl-count condition entries :key
                                      (lambda (entry)
                                        (plist-get entry :condition))))
                          semuse-v02-conditions)))
            (when (> (- (apply #'max counts) (apply #'min counts)) 1)
              (push (format "round-balance:%d" (1+ round)) errors))))))
    (list :ok (null errors)
          :errors (nreverse errors)
          :hash (semuse-v02-schedule-hash schedule))))

(defun semuse-v02--parameter-valid-p (family condition parameter)
  "Validate PARAMETER with FAMILY-specific complete syntax." 
  (let ((case-fold-search nil)
        (family-id (plist-get family :id)))
    (cond
     ((not (stringp parameter)) nil)
     ((eq condition 'sham)
      (pcase family-id
        ('scale (string-match-p "\\`MUL-XXX-ZQ\\'" parameter))
        ('offset (string-match-p "\\`OFF-X000-ZQ\\'" parameter))
        ('permute (string-match-p "\\`ORD-XXXX-ZQ\\'" parameter))))
     ((eq family-id 'scale)
      (string-match-p "\\`MUL-[0-9]\\{3\\}-ZQ\\'" parameter))
     ((eq family-id 'offset)
      (string-match-p "\\`OFF-[PN][0-9]\\{3\\}-ZQ\\'" parameter))
     ((eq family-id 'permute)
      (and (string-match-p "\\`ORD-[1-4]\\{4\\}-ZQ\\'" parameter)
           (equal (sort (string-to-list (substring parameter 4 8)) #'<)
                  '(49 50 51 52))))
     (t nil))))

(defun semuse-v02--x-valid-p (family x)
  "Validate the family-specific task-only token X." 
  (let ((case-fold-search nil))
    (and (stringp x)
         (pcase (plist-get family :id)
           ('scale (string-match-p "\\`PAY-[0-9]\\{3\\}-RV\\'" x))
           ('offset (string-match-p "\\`RAW-[0-9]\\{3\\}-RV\\'" x))
           ('permute (string-match-p "\\`SEQ-[A-Z0-9]\\{4\\}-RV\\'" x))))))

(defun semuse-v02-structure-validation (families schedule)
  "Validate literal data, grammars, matched bytes, and target absence." 
  (let (errors)
    (dolist (family families)
      (let* ((family-id (plist-get family :id))
             (rows (cl-remove-if-not
                    (lambda (row) (eq family-id (plist-get row :family)))
                    semuse-v02-rows))
             (artifacts
              (mapcar (lambda (condition)
                        (semuse-v02-artifact family condition))
                      '(world-a world-b decoy sham)))
             (normalized
              (mapcar
               (lambda (artifact)
                 (replace-regexp-in-string
                  "^DECISIVE-PARAMETER=.*$"
                  "DECISIVE-PARAMETER=<MATCHED>" artifact))
               artifacts)))
        (unless (= 2 (length rows))
          (push (format "row-count:%s" family-id) errors))
        (dolist (condition '(world-a world-b decoy sham))
          (unless (semuse-v02--parameter-valid-p
                   family condition (semuse-v02-parameter family condition))
            (push (format "parameter-grammar:%s:%s" family-id condition)
                  errors)))
        (dolist (row rows)
          (unless (semuse-v02--x-valid-p family (semuse-v02-row-x row))
            (push (format "x-grammar:%s" (plist-get row :id)) errors)))
        (unless (apply #'= (mapcar #'length artifacts))
          (push (format "artifact-length:%s" family-id) errors))
        (unless (cl-every (lambda (value) (equal value (car normalized)))
                          (cdr normalized))
          (push (format "artifact-shape:%s" family-id) errors))
        (when (and rows
                   (not (equal (semuse-v02-artifact family 'world-a)
                               (semuse-v02-artifact
                                (semuse-v02-row-family (cadr rows)) 'world-a))))
          (push (format "artifact-not-reused:%s" family-id) errors))
        (dolist (row rows)
          (let* ((prompt (semuse-v02-prompt row))
                 (visible (concat prompt "\n" (string-join artifacts "\n"))))
            (dolist (condition '(world-a world-b decoy))
              (let ((target (semuse-v02-target row condition)))
                (when (string-match-p (regexp-quote target) visible)
                  (push (format "target-leak:%s:%s"
                                (plist-get row :id) condition)
                        errors))))
            (dolist (condition '(world-a world-b decoy))
              (when (string-match-p
                     (regexp-quote (semuse-v02-parameter family condition))
                     prompt)
                (push (format "parameter-leak:%s:%s"
                              (plist-get row :id) condition)
                      errors)))
            (when (string-match-p (regexp-quote (semuse-v02-row-x row))
                                  (string-join artifacts "\n"))
              (push (format "x-leak:%s" (plist-get row :id)) errors))))))
    (let ((schedule-result (semuse-v02-schedule-validation schedule)))
      (unless (plist-get schedule-result :ok)
        (setq errors (append (plist-get schedule-result :errors) errors))))
    (unless (eq semuse-v02-claim-mode 'descriptive-consistency)
      (push "claim-mode" errors))
    (list :ok (null errors) :errors (nreverse errors))))

(defun semuse-v02--required-identity-fields ()
  "Return identity fields that must be unique and non-empty." 
  '(:session-id :workspace-id :producer-run-id :consumer-run-id))

(defun semuse-v02-record-set-gate (records schedule)
  "Validate RECORDS one-to-one against explicit SCHEDULE.

No record is selected by order.  This gate rejects every duplicate, extra,
missing, replacement, identity collision, and field mismatch before scoring." 
  (let ((errors nil)
        (expected-by-code (make-hash-table :test #'equal))
        (seen-code (make-hash-table :test #'equal))
        (seen-cell (make-hash-table :test #'equal))
        (seen-identities
         (mapcar (lambda (field) (cons field (make-hash-table :test #'equal)))
                 (semuse-v02--required-identity-fields)))
        (schedule-result (semuse-v02-schedule-validation schedule)))
    (unless (plist-get schedule-result :ok)
      (push "schedule-not-valid" errors))
    (unless (proper-list-p records)
      (push "records-not-list" errors)
      (setq records nil))
    (unless (proper-list-p schedule)
      (setq schedule nil))
    (dolist (entry schedule)
      (puthash (plist-get entry :assignment-code) entry expected-by-code))
    (unless (= (length records) (length schedule))
      (push "record-count" errors))
    (dolist (record records)
      (if (not (proper-list-p record))
          (push "record-malformed" errors)
        (let* ((code (plist-get record :assignment-code))
               (expected (gethash code expected-by-code))
               (position (plist-get record :schedule-position))
               (row-id (plist-get record :row-id))
               (condition (plist-get record :condition))
               (cell (semuse-v02--cell-key row-id condition)))
          (unless (and (stringp code)
                       (let ((case-fold-search nil))
                         (string-match-p "\\`ASG-[A-F0-9]\\{12\\}\\'"
                                         code)))
            (push "assignment-code-malformed" errors))
          (when (gethash code seen-code)
            (push (format "duplicate-assignment:%s" code) errors))
          (puthash code t seen-code)
          (if (not expected)
              (push (format "extra-assignment:%s" code) errors)
            (unless (and (integerp position)
                         (= position (plist-get expected :position))
                         (equal row-id (plist-get expected :row-id))
                         (eq condition (plist-get expected :condition)))
              (push (format "assignment-join:%s" code) errors)))
          (when (gethash cell seen-cell)
            (push (format "duplicate-cell:%s" cell) errors))
          (puthash cell t seen-cell)
          (unless (and (stringp row-id) (semuse-v02-row row-id)
                       (memq condition semuse-v02-conditions))
            (push (format "record-cell-malformed:%s" cell) errors))
          (unless (and (integerp position) (>= position 1) (<= position 30))
            (push (format "record-position:%s" position) errors))
          (unless (equal (plist-get record :contract-revision) "v0.2")
            (push (format "contract-revision:%s" code) errors))
          (dolist (field (semuse-v02--required-identity-fields))
            (let* ((value (plist-get record field))
                   (table (cdr (assq field seen-identities))))
              (unless (and (stringp value) (> (length value) 0))
                (push (format "identity-missing:%s:%s" field code) errors))
              (when (and value (gethash value table))
                (push (format "identity-duplicate:%s:%s" field value)
                      errors))
              (when value (puthash value t table))))
          (if (eq condition 'withheld)
              (unless (semuse-v02--proved-p
                       (plist-get record :no-artifact-proof))
                (push (format "withheld-no-artifact:%s" code) errors))
            (dolist (field '(:artifact-id :artifact-revision :artifact-sha256))
              (unless (semuse-v02--nonempty-string-p
                       (plist-get record field))
                (push (format "artifact-field-missing:%s:%s" field code)
                      errors)))
            (unless (and (integerp (plist-get record :artifact-byte-length))
                         (> (plist-get record :artifact-byte-length) 0))
              (push (format "artifact-field-missing::artifact-byte-length:%s"
                            code)
                    errors))
            (unless (semuse-v02--proved-p
                     (plist-get record :artifact-present-proof))
              (push (format "artifact-proof:%s" code) errors))))))
    (dolist (entry schedule)
      (let ((code (plist-get entry :assignment-code)))
        (unless (gethash code seen-code)
          (push (format "missing-assignment:%s" code) errors)))
      (let ((cell (semuse-v02--cell-key (plist-get entry :row-id)
                                        (plist-get entry :condition))))
        (unless (gethash cell seen-cell)
          (push (format "missing-cell:%s" cell) errors))))
    (list :ok (null errors)
          :status (if errors 'not-identifiable 'proved)
          :errors (nreverse (delete-dups errors)))))

(defun semuse-v02--expected-chronology (condition)
  "Return the frozen event order for CONDITION's first attempt." 
  (if (eq condition 'withheld)
      '(first-request first-terminal)
    '(first-request artifact-delivery post-delivery-request first-terminal)))

(defun semuse-v02-first-attempt-gate (record)
  "Validate RECORD's earliest raw first output without parsing its answer." 
  (let ((errors nil)
        (condition nil)
        (required-proofs
         '(:first-request-proof :earliest-terminal-proof
           :raw-first-output-proof :no-retry-proof
           :no-replacement-proof)))
    (unless (proper-list-p record)
      (push "record-missing" errors))
    (setq condition (and (proper-list-p record)
                         (plist-get record :condition)))
    (dolist (field required-proofs)
      (unless (semuse-v02--proved-p (plist-get record field))
        (push (format "proof-not-proved:%s" field) errors)))
    (unless (eq condition 'withheld)
      (dolist (field '(:delivery-before-terminal-proof
                       :no-answer-before-delivery-proof
                       :post-delivery-request-proof))
        (unless (semuse-v02--proved-p (plist-get record field))
          (push (format "proof-not-proved:%s" field) errors))))
    (unless (and (integerp (plist-get record :attempt-ordinal))
                 (= (plist-get record :attempt-ordinal) 1))
      (push "attempt-ordinal" errors))
    (unless (and (integerp (plist-get record :first-outbound-request-ordinal))
                 (= (plist-get record :first-outbound-request-ordinal) 1))
      (push "first-request-ordinal" errors))
    (unless (and (integerp (plist-get record :terminal-output-count))
                 (= (plist-get record :terminal-output-count) 1))
      (push "terminal-output-count" errors))
    (unless (and (integerp (plist-get record :answer-output-count))
                 (= (plist-get record :answer-output-count) 1))
      (push "answer-output-count" errors))
    (unless (and (integerp (plist-get record :retry-count))
                 (= (plist-get record :retry-count) 0))
      (push "retry-count" errors))
    (unless (null (plist-get record :replacement-run-id))
      (push "replacement-run" errors))
    (unless (null (plist-get record :later-output))
      (push "later-output" errors))
    (unless (equal (plist-get record :request-ordinals)
                   (if (eq condition 'withheld) '(1) '(1 2)))
      (push "request-ordinals" errors))
    (unless (equal (plist-get record :terminal-sequence)
                   (semuse-v02--expected-chronology condition))
      (push "terminal-chronology" errors))
    (unless (and (integerp (plist-get record :first-terminal-event-index))
                 (= (plist-get record :first-terminal-event-index)
                    (if (eq condition 'withheld) 2 4)))
      (push "first-terminal-index" errors))
    (unless (and (integerp (plist-get record :raw-first-output-event-index))
                 (= (plist-get record :raw-first-output-event-index)
                    (if (eq condition 'withheld) 2 4)))
      (push "raw-output-index" errors))
    (unless (and (stringp (plist-get record :raw-output))
                 (equal (plist-get record :raw-first-output)
                        (plist-get record :raw-output))
                 (equal (plist-get record :terminal-output)
                        (plist-get record :raw-output)))
      (push "raw-first-output-mismatch" errors))
    (let* ((raw (plist-get record :raw-output))
           (bytes (plist-get record :raw-output-bytes))
           (expected-bytes (and (stringp raw) (semuse-v02--utf8-bytes raw)))
           (expected-hash (and expected-bytes
                               (secure-hash 'sha256 expected-bytes))))
      (unless (and expected-bytes (equal bytes expected-bytes))
        (push "raw-output-bytes" errors))
      (unless (and expected-hash
                   (equal (plist-get record :raw-output-sha256) expected-hash)
                   (equal (plist-get record :raw-first-output-sha256)
                          expected-hash))
        (push "raw-output-sha256" errors)))
    (unless (eq (plist-get record :runtime-identity-proof) 'proved)
      (push "runtime-identity-proof" errors))
    (list :ok (null errors)
          :status (if errors 'not-identifiable 'proved)
          :errors (nreverse (delete-dups errors)))))

(defun semuse-v02-content-witness-status (record)
  "Return strict witness status for a non-withheld content RECORD." 
  (cond
   ((not (listp record)) 'not-identifiable)
   ((not (eq (plist-get record :condition) 'withheld))
    (if (and (semuse-v02--proved-p (plist-get record :access-proof))
             (semuse-v02--proved-p
              (plist-get record :request-inclusion-proof)))
        'proved
      'not-identifiable))
   (t 'not-identifiable)))

(defun semuse-v02-withheld-witness-status (record)
  "Return strict empty-workspace/payload-absence status for WITHHELD." 
  (if (and (listp record)
           (eq (plist-get record :condition) 'withheld)
           (semuse-v02--proved-p
            (plist-get record :empty-workspace-proof))
           (semuse-v02--proved-p
            (plist-get record :payload-absent-proof)))
      'proved
    'missing-evidence))

(defun semuse-v02-answer-state (row record)
  "Return the deterministic answer state without repairing model output." 
  (let* ((parsed (semuse-v02-extract (plist-get record :raw-output)))
         (status (plist-get parsed :status)))
    (pcase status
      ('valid
       (if (equal (plist-get parsed :value)
                  (semuse-v02-target row (plist-get record :condition)))
           'correct
         'incorrect))
      ('missing-output 'missing-output)
      (_ 'protocol-failure))))

(defun semuse-v02--index-records (records)
  "Index RECORDS by assignment code and row/condition cell." 
  (let ((by-code (make-hash-table :test #'equal))
        (by-cell (make-hash-table :test #'equal)))
    (dolist (record records)
      (puthash (plist-get record :assignment-code) record by-code)
      (puthash (semuse-v02--cell-key (plist-get record :row-id)
                                     (plist-get record :condition))
               record by-cell))
    (list :by-code by-code :by-cell by-cell)))

(defun semuse-v02--record-at (index row-id condition)
  "Get a record from an already-gated INDEX." 
  (gethash (semuse-v02--cell-key row-id condition)
           (plist-get index :by-cell)))

(defun semuse-v02-provider-envelope-gate (record)
  "Reject model-visible assignment/truth oracles in RECORD's envelope." 
  (let* ((errors nil)
        (envelope (plist-get record :provider-envelope))
        (row-id (plist-get record :row-id))
        (condition (plist-get record :condition))
        (assignment-code (plist-get record :assignment-code))
        (condition-token (and (symbolp condition)
                              (symbol-name condition))))
    (unless (semuse-v02--proved-p
             (plist-get record :provider-envelope-proof))
      (push "provider-envelope-proof" errors))
    (unless (semuse-v02--proved-p
             (plist-get record :opaque-runtime-ids-proof))
      (push "opaque-runtime-ids-proof" errors))
    (unless (proper-list-p envelope)
      (push "provider-envelope-missing" errors))
    (dolist (field '(:assignment-code :row-id :condition :schedule-position
                     :expected-target :truth-join))
      (when (and (proper-list-p envelope)
                 (semuse-v02--has-key-p envelope field))
        (push (format "oracle-field:%s" field) errors)))
    (when (proper-list-p envelope)
      (let ((ids (plist-get envelope :runtime-ids)))
        (dolist (field '(:session-id :workspace-id :producer-run-id
                         :consumer-run-id))
          (let ((value (plist-get ids field)))
            (unless (and (stringp value)
                         (let ((case-fold-search nil))
                           (string-match-p "\\`opaque-[a-z0-9]+\\'"
                                           value)))
              (push (format "opaque-id:%s" field) errors))))
        (unless (equal (plist-get ids :session-id)
                       (plist-get record :session-id))
          (push "session-envelope-join" errors))
        (unless (equal (plist-get ids :workspace-id)
                       (plist-get record :workspace-id))
          (push "workspace-envelope-join" errors))
        (unless (equal (plist-get ids :producer-run-id)
                       (plist-get record :producer-run-id))
          (push "producer-envelope-join" errors))
        (unless (equal (plist-get ids :consumer-run-id)
                       (plist-get record :consumer-run-id))
          (push "consumer-envelope-join" errors)))
      (let ((visible
             (prin1-to-string
              (list (plist-get envelope :prompt)
                    (plist-get envelope :artifact)
                    (plist-get envelope :messages)
                    (plist-get envelope :tool-schema)
                    (plist-get envelope :runtime-ids)))))
        (dolist (forbidden
                 (list assignment-code row-id condition-token
                       (format "RES-%s" row-id)))
          (when (and (stringp forbidden)
                     (let ((case-fold-search t))
                       (string-match-p (regexp-quote forbidden) visible)))
            (push (format "oracle-text:%s" forbidden) errors)))
        (let ((row (and (stringp row-id) (semuse-v02-row row-id))))
          (when row
            (dolist (content-condition '(world-a world-b decoy))
              (let ((case-fold-search t))
                (when (string-match-p
                       (regexp-quote
                        (semuse-v02-target row content-condition))
                       visible)
                  (push (format "target-oracle:%s" content-condition)
                        errors))))))))
    (list :ok (null errors)
          :status (if errors 'not-identifiable 'proved)
          :errors (nreverse (delete-dups errors)))))

(defun semuse-v02--integrity-gate (records schedule)
  "Run record-set, first-attempt, and envelope gates in that order." 
  (let ((record-set (semuse-v02-record-set-gate records schedule)))
    (if (not (plist-get record-set :ok))
        (list :ok nil :status 'not-identifiable
              :stage 'record-set :errors (plist-get record-set :errors)
              :record-set record-set)
      (let ((first-failures nil))
        (dolist (record records)
          (let ((result (semuse-v02-first-attempt-gate record)))
            (unless (plist-get result :ok)
              (push (list (plist-get record :assignment-code)
                          (plist-get result :errors))
                    first-failures))))
        (if first-failures
            (list :ok nil :status 'not-identifiable
                  :stage 'first-attempt
                  :errors (nreverse first-failures)
                  :record-set record-set)
          (let ((envelope-failures nil))
            (dolist (record records)
              (let ((result (semuse-v02-provider-envelope-gate record)))
                (unless (plist-get result :ok)
                  (push (list (plist-get record :assignment-code)
                              (plist-get result :errors))
                        envelope-failures))))
            (if envelope-failures
                (list :ok nil :status 'not-identifiable
                      :stage 'provider-envelope
                      :errors (nreverse envelope-failures)
                      :record-set record-set)
              (list :ok t :status 'proved :stage 'complete
                    :errors nil :record-set record-set))))))))

(defun semuse-v02--content-after-gates (records index)
  "Score the 18 A/B/decoy content cells after all integrity gates." 
  (let (witness-failures answer-failures content-cells)
    (dolist (row semuse-v02-rows)
      (dolist (condition '(world-a world-b decoy))
        (let* ((row-id (plist-get row :id))
               (record (semuse-v02--record-at index row-id condition))
               (witness (semuse-v02-content-witness-status record))
               (answer (semuse-v02-answer-state row record)))
          (push (list :row-id row-id
                      :condition condition
                      :witness witness
                      :answer answer
                      :status (cond
                               ((not (eq witness 'proved))
                                'not-identifiable)
                               ((eq answer 'correct) 'observed)
                               (t 'not-supported)))
                content-cells)
          (unless (eq witness 'proved)
            (push (list row-id condition witness) witness-failures))
          (unless (eq answer 'correct)
            (push (list row-id condition answer) answer-failures)))))
    (let* ((fixed-v-changes-with-x
            (cl-every
             (lambda (condition)
               (cl-every
                (lambda (family)
                  (let* ((family-id (plist-get family :id))
                         (rows (cl-remove-if-not
                                (lambda (row)
                                  (eq family-id (plist-get row :family)))
                                semuse-v02-rows))
                         (answers
                          (mapcar
                           (lambda (row)
                             (plist-get
                              (semuse-v02-extract
                               (plist-get
                                (semuse-v02--record-at
                                 index (plist-get row :id) condition)
                                 :raw-output))
                              :value))
                           rows)))
                    (and (= 2 (length rows))
                         (cl-every #'stringp answers)
                         (= 2 (length (delete-dups
                                       (copy-sequence answers)))))))
                semuse-v02-families))
             '(world-a world-b decoy)))
           (fixed-x-changes-with-v
            (cl-every
             (lambda (row)
               (let ((answers
                      (mapcar
                       (lambda (condition)
                         (plist-get
                          (semuse-v02-extract
                           (plist-get
                            (semuse-v02--record-at
                             index (plist-get row :id) condition)
                            :raw-output))
                          :value))
                       '(world-a world-b decoy))))
                 (and (cl-every #'stringp answers)
                      (= 3 (length (delete-dups
                                    (copy-sequence answers)))))))
             semuse-v02-rows))
           (status
            (cond
             (witness-failures 'not-identifiable)
             ((or answer-failures
                  (not fixed-v-changes-with-x)
                  (not fixed-x-changes-with-v))
              'not-supported)
             (t 'consistent-with-two-input-use))))
      (list :status status
            :content-status status
            :fixed-v-changes-with-x fixed-v-changes-with-x
            :fixed-x-changes-with-v fixed-x-changes-with-v
            :content-cells (nreverse content-cells)
            :witness-failures (nreverse witness-failures)
            :answer-failures (nreverse answer-failures)
            :content-cell-count 18))))

(defun semuse-v02--sham-result (records index)
  "Report sham control independently of the 18 content cells." 
  (let (target-hits non-abstain witness-failures)
    (dolist (row semuse-v02-rows)
      (let* ((row-id (plist-get row :id))
             (record (semuse-v02--record-at index row-id 'sham))
             (witness (semuse-v02-content-witness-status record))
             (parsed (semuse-v02-extract (plist-get record :raw-output)))
             (value (plist-get parsed :value)))
        (unless (eq witness 'proved)
          (push (list row-id witness) witness-failures))
        (when (member value (list (semuse-v02-target row 'world-a)
                                  (semuse-v02-target row 'world-b)))
          (push row-id target-hits))
        (unless (equal value "INSUFFICIENT")
          (push row-id non-abstain))))
    (let ((failed (or witness-failures target-hits non-abstain)))
      (list :status (if failed 'control-driven-not-identifiable 'control-pass)
            :sham-status (if failed 'control-driven-not-identifiable
                           'control-pass)
            :target-hits (nreverse target-hits)
            :non-abstain (nreverse non-abstain)
            :witness-failures (nreverse witness-failures)
            :target-hit-count (length target-hits)
            :abstain-count (- 6 (length non-abstain))))))

(defun semuse-v02-cohort-result (records schedule)
  "Return the fail-closed cohort result for explicit RECORDS and SCHEDULE." 
  (let ((integrity (semuse-v02--integrity-gate records schedule)))
    (if (not (plist-get integrity :ok))
        (list :claim-mode semuse-v02-claim-mode
              :status 'not-identifiable
              :closure 'not-identifiable
              :stage (plist-get integrity :stage)
              :errors (plist-get integrity :errors)
              :content-status 'not-identifiable
              :sham-status 'not-identifiable
              :record-set integrity)
      (let* ((index (semuse-v02--index-records records))
             (content (semuse-v02--content-after-gates records index))
             (sham (semuse-v02--sham-result records index))
             (content-status (plist-get content :status))
             (sham-status (plist-get sham :status))
             (status
              (cond
               ((eq sham-status 'control-driven-not-identifiable)
                'control-driven-not-identifiable)
               ((eq content-status 'not-identifiable)
                'not-identifiable)
               ((eq content-status 'not-supported) 'not-supported)
               (t 'consistent-with-two-input-use))))
        (list :claim-mode semuse-v02-claim-mode
              :status status
              :closure status
              :stage 'scored
              :errors nil
              :content-status content-status
              :content content
              :sham-status sham-status
              :sham sham
              :record-set integrity
              :raw-content-cell-count 18
              :inferential-claim nil)))))

(defun semuse-v02-effect-pair (row records schedule)
  "Return the independent descriptive World-A/withheld pair for ROW." 
  (let ((integrity (semuse-v02--integrity-gate records schedule)))
    (if (not (plist-get integrity :ok))
        (list :status 'not-identifiable :difference nil
              :stage (plist-get integrity :stage))
      (let* ((index (semuse-v02--index-records records))
             (a (semuse-v02--record-at index (plist-get row :id) 'world-a))
             (withheld
              (semuse-v02--record-at index (plist-get row :id) 'withheld))
             (a-witness (semuse-v02-content-witness-status a))
             (w-witness (semuse-v02-withheld-witness-status withheld)))
        (if (or (not (eq a-witness 'proved))
                (not (eq w-witness 'proved)))
            (list :status 'missing-evidence
                  :difference nil
                  :a-witness a-witness
                  :withheld-witness w-witness)
          (let* ((a-parsed (semuse-v02-extract (plist-get a :raw-output)))
                 (withheld-parsed
                  (semuse-v02-extract (plist-get withheld :raw-output)))
                 (a-output-status (plist-get a-parsed :status))
                 (withheld-output-status
                  (plist-get withheld-parsed :status)))
            (if (or (not (eq a-output-status 'valid))
                    (not (eq withheld-output-status 'valid)))
                (list :status 'protocol-failure
                      :difference nil
                      :a-output-status a-output-status
                      :withheld-output-status withheld-output-status)
              (let ((a-quality
                     (eq (semuse-v02-answer-state row a) 'correct))
                    (withheld-quality
                     (eq (semuse-v02-answer-state row withheld) 'correct)))
                (list :status 'observed
                      :a-quality a-quality
                      :withheld-quality withheld-quality
                      :difference (- (if a-quality 1 0)
                                     (if withheld-quality 1 0)))))))))))

(defun semuse-v02--opaque-id (kind position)
  "Make a non-semantic synthetic runtime identity." 
  (format "opaque-%s" (substring
                        (secure-hash 'sha256
                                     (format "opaque|%s|%d" kind position))
                        0 16)))

(defun semuse-v02--synthetic-target (row condition scenario)
  "Return the output used by synthetic SCENARIO." 
  (cond
   ((and (eq scenario 'v-only)
         (equal (plist-get row :x-key) :x2)
         (memq condition '(world-a world-b decoy)))
    (semuse-v02-target
     (semuse-v02-row
      (replace-regexp-in-string "x2\\'" "x1" (plist-get row :id)))
     condition))
   ((and (eq scenario 'x-only)
         (memq condition '(world-b decoy)))
    (semuse-v02-target row 'world-a))
   (t (semuse-v02-target row condition))))

(defun semuse-v02--synthetic-envelope (row condition position ids)
  "Build an envelope with scorer-only truth fields absent." 
  (let ((family (semuse-v02-row-family row)))
    (list :prompt (semuse-v02-prompt row)
          :artifact (semuse-v02-artifact family condition)
          :messages (list (semuse-v02-prompt row))
          :tool-schema "workspace_info;workspace_read"
          :runtime-ids ids)))

(defun semuse-v02--synthetic-record (entry scenario)
  "Build one complete synthetic first-attempt record." 
  (let* ((position (plist-get entry :position))
         (code (plist-get entry :assignment-code))
         (row-id (plist-get entry :row-id))
         (condition (plist-get entry :condition))
         (row (semuse-v02-row row-id))
         (family (semuse-v02-row-family row))
         (artifact (semuse-v02-artifact family condition))
         (target (semuse-v02--synthetic-target row condition scenario))
         (raw (format "ANSWER: %s" target))
         (ids (list :session-id (semuse-v02--opaque-id 'session position)
                    :workspace-id (semuse-v02--opaque-id 'workspace position)
                    :producer-run-id (semuse-v02--opaque-id 'producer position)
                    :consumer-run-id (semuse-v02--opaque-id 'consumer position)))
         (record
          (list :contract-revision "v0.2"
                :assignment-code code
                :schedule-position position
                :row-id row-id
                :condition condition
                :session-id (plist-get ids :session-id)
                :workspace-id (plist-get ids :workspace-id)
                :producer-run-id (plist-get ids :producer-run-id)
                :consumer-run-id (plist-get ids :consumer-run-id)
                :artifact-id (unless (eq condition 'withheld)
                               (semuse-v02--opaque-id 'artifact position))
                :artifact-revision (unless (eq condition 'withheld)
                                    (format "rev-%s"
                                            (substring
                                             (semuse-v02-raw-output-sha256
                                              artifact)
                                             0 12)))
                :artifact-byte-length (when artifact (string-bytes artifact))
                :artifact-sha256 (when artifact
                                  (secure-hash 'sha256
                                               (semuse-v02--utf8-bytes artifact)))
                :scorer-expected-target (semuse-v02-target row condition)
                :scorer-truth-join
                (list :row-id row-id :condition condition
                      :expected-target (semuse-v02-target row condition))
                :artifact-present-proof (unless (eq condition 'withheld)
                                         'proved)
                :no-artifact-proof (when (eq condition 'withheld) 'proved)
                :access-proof (unless (eq condition 'withheld) 'proved)
                :request-inclusion-proof (unless (eq condition 'withheld)
                                              'proved)
                :empty-workspace-proof (when (eq condition 'withheld) 'proved)
                :payload-absent-proof (when (eq condition 'withheld) 'proved)
                :provider-envelope-proof 'proved
                :opaque-runtime-ids-proof 'proved
                :runtime-identity-proof 'proved
                :first-request-proof 'proved
                :earliest-terminal-proof 'proved
                :raw-first-output-proof 'proved
                :delivery-before-terminal-proof
                (unless (eq condition 'withheld) 'proved)
                :no-answer-before-delivery-proof
                (unless (eq condition 'withheld) 'proved)
                :post-delivery-request-proof
                (unless (eq condition 'withheld) 'proved)
                :no-retry-proof 'proved
                :no-replacement-proof 'proved
                :attempt-ordinal 1
                :first-outbound-request-ordinal 1
                :retry-count 0
                :replacement-run-id nil
                :later-output nil
                :request-ordinals (if (eq condition 'withheld) '(1) '(1 2))
                :terminal-sequence (semuse-v02--expected-chronology condition)
                :first-terminal-event-index (if (eq condition 'withheld) 2 4)
                :raw-first-output-event-index (if (eq condition 'withheld) 2 4)
                :terminal-output-count 1
                :answer-output-count 1
                :raw-output raw
                :raw-first-output raw
                :terminal-output raw
                :raw-output-bytes (semuse-v02--utf8-bytes raw)
                :raw-output-sha256 (semuse-v02-raw-output-sha256 raw)
                :raw-first-output-sha256 (semuse-v02-raw-output-sha256 raw))))
    (plist-put record :provider-envelope
               (semuse-v02--synthetic-envelope row condition position ids))
    record))

(defun semuse-v02--record-at-list (records row-id condition)
  "Find a synthetic record by key for audit mutation only." 
  (gethash (semuse-v02--cell-key row-id condition)
           (plist-get (semuse-v02--index-records records) :by-cell)))

(defun semuse-v02--set-raw (record raw)
  "Replace a synthetic raw output and its byte/hash witnesses." 
  (plist-put record :raw-output raw)
  (plist-put record :raw-first-output raw)
  (plist-put record :terminal-output raw)
  (plist-put record :raw-output-bytes (semuse-v02--utf8-bytes raw))
  (plist-put record :raw-output-sha256 (semuse-v02-raw-output-sha256 raw))
  (plist-put record :raw-first-output-sha256
             (semuse-v02-raw-output-sha256 raw))
  record)

(defun semuse-v02-synthetic-records (scenario)
  "Build the full frozen 30-record fixture for SCENARIO." 
  (let ((records
         (mapcar (lambda (entry)
                   (semuse-v02--synthetic-record entry scenario))
                 semuse-v02-frozen-schedule)))
    (pcase scenario
      ('missing-content-witness
       (plist-put (semuse-v02--record-at-list records "scale-x1" 'world-a)
                  :request-inclusion-proof nil))
      ('missing-withheld
       (let ((record (semuse-v02--record-at-list records
                                                 "permute-x2" 'withheld)))
         (plist-put record :empty-workspace-proof nil)
         (plist-put record :payload-absent-proof nil)))
      ('parser-suffix
       (let* ((record (semuse-v02--record-at-list records "scale-x2" 'world-b))
              (target (semuse-v02-target (semuse-v02-row "scale-x2")
                                         'world-b)))
         (semuse-v02--set-raw record (format "ANSWER: %s explanation" target))))
      ('duplicate
       (let ((duplicate (copy-tree
                         (semuse-v02--record-at-list records
                                                      "scale-x1" 'world-a))))
         (setq records (append records (list duplicate))))
       records)
      ('extra
       (let ((extra (copy-tree (car records))))
         (plist-put extra :assignment-code "ASG-EXTRA000000")
         (plist-put extra :schedule-position 31)
         (setq records (append records (list extra))))
       records)
      ('missing-record
       (setq records
             (cl-remove-if
              (lambda (record)
                (equal (plist-get record :assignment-code)
                       "ASG-BF514E87CEEE"))
              records)))
      ('wrong-first-later-replacement
       (let* ((record (semuse-v02--record-at-list records "scale-x1" 'world-a))
              (wrong (format "ANSWER: %s"
                             (semuse-v02-target
                              (semuse-v02-row "scale-x1") 'world-b)))
              (right (format "ANSWER: %s"
                             (semuse-v02-target
                              (semuse-v02-row "scale-x1") 'world-a))))
         (semuse-v02--set-raw record wrong)
         (plist-put record :later-output right)
         (plist-put record :replacement-run-id
                    (semuse-v02--opaque-id 'replacement 1))
         (plist-put record :no-replacement-proof nil)
         (plist-put record :terminal-output-count 2)
         (plist-put record :answer-output-count 2)))
      ('truthy-false
       (plist-put (semuse-v02--record-at-list records "scale-x1" 'world-a)
                  :request-inclusion-proof :false)
       (plist-put (semuse-v02--record-at-list records "scale-x1" 'withheld)
                  :empty-workspace-proof :false))
      ('lowercase-answer
       (let* ((record (semuse-v02--record-at-list records "scale-x1" 'world-a))
              (target (semuse-v02-target (semuse-v02-row "scale-x1")
                                         'world-a)))
         (semuse-v02--set-raw record (format "answer: %s" target))))
      ('sham-target
       (let ((record (semuse-v02--record-at-list records "offset-x1" 'sham))
             (target (semuse-v02-target (semuse-v02-row "offset-x1")
                                        'world-a)))
         (semuse-v02--set-raw record (format "ANSWER: %s" target))))
      ('oracle-exposed
       (let* ((record (semuse-v02--record-at-list records "scale-x1" 'world-a))
              (envelope (copy-tree (plist-get record :provider-envelope))))
         (plist-put envelope :assignment-code
                    (plist-get record :assignment-code))
         (plist-put record :provider-envelope envelope)))
      (_ nil))
    records))

(defun semuse-v02--contains-error-p (result regexp)
  "Return non-nil if RESULT's error text matches REGEXP." 
  (string-match-p regexp (prin1-to-string (plist-get result :errors))))

(defun semuse-v02-exit-code (result)
  "Map a result with boolean :ok to a shell exit code." 
  (if (plist-get result :ok) 0 1))

(defun semuse-v02-audit ()
  "Replay every v0.1 VETO as a deterministic offline check." 
  (let* ((schedule semuse-v02-frozen-schedule)
         (structure (semuse-v02-structure-validation
                     semuse-v02-families schedule))
         (passing (semuse-v02-synthetic-records 'passing))
         (v-only (semuse-v02-synthetic-records 'v-only))
         (x-only (semuse-v02-synthetic-records 'x-only))
         (missing-content (semuse-v02-synthetic-records
                           'missing-content-witness))
         (missing-withheld (semuse-v02-synthetic-records 'missing-withheld))
         (parser-suffix (semuse-v02-synthetic-records 'parser-suffix))
         (duplicate (semuse-v02-synthetic-records 'duplicate))
         (extra (semuse-v02-synthetic-records 'extra))
         (missing-record (semuse-v02-synthetic-records 'missing-record))
         (replacement (semuse-v02-synthetic-records
                       'wrong-first-later-replacement))
         (truthy-false (semuse-v02-synthetic-records 'truthy-false))
         (lowercase (semuse-v02-synthetic-records 'lowercase-answer))
         (sham-target (semuse-v02-synthetic-records 'sham-target))
         (oracle (semuse-v02-synthetic-records 'oracle-exposed))
         (passing-result (semuse-v02-cohort-result passing schedule))
         (v-only-result (semuse-v02-cohort-result v-only schedule))
         (x-only-result (semuse-v02-cohort-result x-only schedule))
         (missing-content-result
          (semuse-v02-cohort-result missing-content schedule))
         (missing-withheld-result
          (semuse-v02-cohort-result missing-withheld schedule))
         (parser-result (semuse-v02-cohort-result parser-suffix schedule))
         (parser-first
          (semuse-v02-first-attempt-gate
           (semuse-v02--record-at-list parser-suffix "scale-x2" 'world-b)))
         (duplicate-result (semuse-v02-cohort-result duplicate schedule))
         (extra-result (semuse-v02-cohort-result extra schedule))
         (missing-record-result
          (semuse-v02-cohort-result missing-record schedule))
         (replacement-result (semuse-v02-cohort-result replacement schedule))
         (truthy-result (semuse-v02-cohort-result truthy-false schedule))
         (truthy-pair
          (semuse-v02-effect-pair (semuse-v02-row "scale-x1")
                                  truthy-false schedule))
         (lowercase-result (semuse-v02-cohort-result lowercase schedule))
         (lowercase-first
          (semuse-v02-first-attempt-gate
           (semuse-v02--record-at-list lowercase "scale-x1" 'world-a)))
         (sham-result (semuse-v02-cohort-result sham-target schedule))
         (oracle-result (semuse-v02-cohort-result oracle schedule))
         (missing-pair
          (semuse-v02-effect-pair (semuse-v02-row "permute-x2")
                                  missing-withheld schedule))
         (empty-schedule (semuse-v02-schedule-validation nil))
         (drift-schedule (copy-tree schedule))
         (bad-families (copy-tree semuse-v02-families)))
    (plist-put (nth 0 drift-schedule) :condition 'world-b)
    (plist-put (car bad-families) :decoy "BAD-019-ZQ")
    (let* ((bad-structure
            (semuse-v02-structure-validation bad-families schedule))
           (checks
            (list
             (cons 'passing
                   (and (eq (plist-get passing-result :status)
                            'consistent-with-two-input-use)
                        (eq (plist-get passing-result :content-status)
                            'consistent-with-two-input-use)
                        (eq (plist-get passing-result :sham-status)
                            'control-pass)))
             (cons 'v-only
                   (eq (plist-get v-only-result :status) 'not-supported))
             (cons 'x-only
                   (eq (plist-get x-only-result :status) 'not-supported))
             (cons 'missing-content-witness
                   (and (eq (plist-get missing-content-result :status)
                            'not-identifiable)
                        (eq (plist-get missing-content-result :stage)
                            'scored)
                        (plist-get
                         (plist-get missing-content-result :content)
                         :witness-failures)))
             (cons 'missing-withheld
                   (and (eq (plist-get missing-withheld-result :content-status)
                            'consistent-with-two-input-use)
                        (eq (plist-get missing-pair :status)
                            'missing-evidence)
                        (null (plist-get missing-pair :difference))))
             (cons 'parser-suffix
                   (and (eq (plist-get parser-first :status) 'proved)
                        (eq (plist-get parser-result :status)
                            'not-supported)
                        (eq (plist-get parser-result :stage)
                            'scored)
                        (let ((cell
                               (cl-find-if
                                (lambda (value)
                                  (and (equal (plist-get value :row-id)
                                              "scale-x2")
                                       (eq (plist-get value :condition)
                                           'world-b)))
                                (plist-get (plist-get parser-result :content)
                                           :content-cells))))
                          (and (eq (plist-get cell :answer)
                                   'protocol-failure)
                               (eq (plist-get cell :status)
                                   'not-supported)))))
             (cons 'duplicate
                   (and (eq (plist-get duplicate-result :status)
                            'not-identifiable)
                        (eq (plist-get duplicate-result :stage)
                            'record-set)
                        (semuse-v02--contains-error-p
                         (plist-get duplicate-result :record-set)
                         "duplicate")))
             (cons 'extra
                   (and (eq (plist-get extra-result :status)
                            'not-identifiable)
                        (eq (plist-get extra-result :stage)
                            'record-set)
                        (semuse-v02--contains-error-p
                         (plist-get extra-result :record-set)
                         "extra-assignment")))
             (cons 'missing-record
                   (and (eq (plist-get missing-record-result :status)
                            'not-identifiable)
                        (eq (plist-get missing-record-result :stage)
                            'record-set)
                        (semuse-v02--contains-error-p
                         (plist-get missing-record-result :record-set)
                         "missing")))
             (cons 'wrong-first-later-replacement
                   (and (eq (plist-get replacement-result :status)
                            'not-identifiable)
                        (eq (plist-get replacement-result :stage)
                            'first-attempt)
                        (semuse-v02--contains-error-p
                         replacement-result "replacement")))
             (cons 'truthy-false
                   (and (eq (plist-get truthy-result :status)
                            'not-identifiable)
                        (eq (plist-get truthy-result :content-status)
                            'not-identifiable)
                        (plist-get (plist-get truthy-result :content)
                                   :witness-failures)
                        (eq (plist-get truthy-pair :status)
                            'missing-evidence)
                        (null (plist-get truthy-pair :difference))))
             (cons 'empty-schedule
                   (and (not (plist-get empty-schedule :ok))
                        (member "schedule-count"
                                (plist-get empty-schedule :errors))
                        (member "schedule-hash"
                                (plist-get empty-schedule :errors))))
             (cons 'schedule-drift
                   (and (not (plist-get
                              (semuse-v02-schedule-validation drift-schedule)
                              :ok))
                        (member "schedule-hash"
                                (plist-get
                                 (semuse-v02-schedule-validation
                                  drift-schedule) :errors))))
             (cons 'lowercase-answer
                   (and (eq (plist-get lowercase-first :status) 'proved)
                        (eq (plist-get lowercase-result :status)
                            'not-supported)
                        (eq (plist-get lowercase-result :stage)
                            'scored)
                        (let ((cell
                               (cl-find-if
                                (lambda (value)
                                  (and (equal (plist-get value :row-id)
                                              "scale-x1")
                                       (eq (plist-get value :condition)
                                           'world-a)))
                                (plist-get
                                 (plist-get lowercase-result :content)
                                 :content-cells))))
                          (and (eq (plist-get cell :answer)
                                   'protocol-failure)
                               (eq (plist-get cell :status)
                                   'not-supported)))
                        (eq (plist-get
                             (semuse-v02-extract "answer: RES-00056-KC")
                             :status)
                            'protocol-failure)))
             (cons 'bad-decoy
                   (and (not (plist-get bad-structure :ok))
                        (semuse-v02--contains-error-p
                         bad-structure "parameter-grammar:scale:decoy")))
             (cons 'sham-target
                   (and (eq (plist-get sham-result :content-status)
                            'consistent-with-two-input-use)
                        (eq (plist-get sham-result :sham-status)
                            'control-driven-not-identifiable)
                        (eq (plist-get sham-result :status)
                            'control-driven-not-identifiable)
                        (not (eq (plist-get sham-result :status)
                                 'not-supported))))
             (cons 'oracle-exposed
                   (and (eq (plist-get oracle-result :status)
                            'not-identifiable)
                        (eq (plist-get oracle-result :stage)
                            'provider-envelope)
                        (semuse-v02--contains-error-p
                         oracle-result "oracle")))
             (cons 'failed-audit-nonzero
                   (= 1 (semuse-v02-exit-code '(:ok nil)))))))
      (list :ok (cl-every #'cdr checks)
            :checks checks
            :schedule-hash (semuse-v02-schedule-hash schedule)
            :passing passing-result
            :v-only v-only-result
            :x-only x-only-result
            :missing-withheld-pair missing-pair
            :structure structure
            :bad-structure bad-structure
            :empty-schedule empty-schedule))))

(provide 'remediation-logic-v0.2)

;;; remediation-logic-v0.2.el ends here
