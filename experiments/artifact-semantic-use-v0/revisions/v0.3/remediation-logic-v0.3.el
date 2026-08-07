;;; remediation-logic-v0.3.el --- Offline scorer remediation -*- lexical-binding: t; -*-

;; Pure offline validation.  This file does not load Superchat, Runtime, a
;; provider, a database, or the v0.2 command shell.

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name
       "../v0.2/remediation-logic-v0.2.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(defconst semuse-v03-record-fields
  '(:contract-revision :assignment-id :session-id :workspace-id
    :producer-run-id :consumer-run-id :artifact-id :artifact-revision
    :artifact-byte-length :artifact-sha256 :artifact-present-proof
    :no-artifact-proof :access-proof :request-inclusion-proof
    :empty-workspace-proof :payload-absent-proof :provider-envelope-proof
    :opaque-runtime-ids-proof :runtime-identity-proof :first-request-proof
    :earliest-terminal-proof :raw-first-output-proof
    :delivery-before-terminal-proof :no-answer-before-delivery-proof
    :post-delivery-request-proof :no-retry-proof :no-replacement-proof
    :attempt-ordinal :first-outbound-request-ordinal :retry-count
    :replacement-run-id :later-output :request-ordinals :terminal-sequence
    :first-terminal-event-index :raw-first-output-event-index
    :terminal-output-count :answer-output-count :raw-output
    :raw-first-output :terminal-output :raw-output-bytes
    :raw-output-sha256 :raw-first-output-sha256 :visible-inputs
    :decision-snapshot-id :first-action))

(defconst semuse-v03-envelope-fields
  '(:prompt :artifact :messages :tool-schema :runtime-ids))

(defconst semuse-v03-runtime-id-fields
  '(:session-id :workspace-id :producer-run-id :consumer-run-id))

(defconst semuse-v03-conditions '(world-a world-b decoy sham withheld))

(defconst semuse-v03-first-action-fields
  '(:attempt-ordinal :snapshot-id :call-id :call-ordinal
    :tool-name :arguments :action-event-id :terminal))

(defconst semuse-v03-action-terminal-fields
  '(:event-id :causation-id :call-id :status))

(defconst semuse-v03-family-fields
  '(:id :card :world-a :world-b :decoy :sham :x1 :x2))

(defconst semuse-v03-row-fields '(:id :family :x-key))

(defconst semuse-v03-schedule-fields
  '(:position :assignment-id :row-id :condition :family-id
    :expected-target :artifact-bytes :artifact-revision
    :artifact-byte-length :artifact-sha256 :decision-snapshot-id
    :first-action-call-id :first-action-event-id
    :first-action-terminal-id))

(defun semuse-v03--plist-remove (plist keys)
  "Return a copy of shape-checked PLIST without KEYS."
  (let (result)
    (while plist
      (unless (memq (car plist) keys)
        (setq result (append result (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    result))

(defun semuse-v03--acyclic-input-p (&rest roots)
  "Return non-nil when ROOTS are acyclic and within a fixed node budget."
  (condition-case nil
      (let ((active (make-hash-table :test #'eq))
            (seen (make-hash-table :test #'eq))
            (nodes 0))
        (cl-labels
            ((walk
              (value)
              (if (not (or (consp value) (vectorp value)))
                  t
                (cond
                 ((gethash value active) nil)
                 ((gethash value seen) t)
                 (t
                  (setq nodes (1+ nodes))
                  (when (> nodes 100000) (error "Input tree too large"))
                  (puthash value t active)
                  (let ((ok
                         (if (vectorp value)
                             (cl-every #'walk (append value nil))
                           (and (walk (car value)) (walk (cdr value))))))
                    (remhash value active)
                    (puthash value t seen)
                    ok))))))
          (cl-every #'walk roots)))
    (error nil)))

(defun semuse-v03--plist-shape-error (value)
  "Return the first strict plist shape error for VALUE, or nil."
  (cond
   ((not (proper-list-p value)) 'not-proper-list)
   ((cl-oddp (length value)) 'odd-plist)
   (t
    (let ((tail value)
          (seen nil)
          error)
      (while (and tail (not error))
        (let ((key (car tail)))
          (cond
           ((not (keywordp key)) (setq error 'nonkeyword-key))
           ((memq key seen) (setq error 'duplicate-key))
           (t (push key seen))))
        (setq tail (cddr tail)))
      error))))

(defun semuse-v03--nested-shape-error-p (value)
  "Return non-nil when VALUE contains a malformed nested plist."
  (cond
   ((vectorp value)
    (cl-some #'semuse-v03--nested-shape-error-p (append value nil)))
   ((not (consp value)) nil)
   ((keywordp (car value))
    (or (semuse-v03--plist-shape-error value)
        (let ((tail value)
              found)
          (while (and tail (not found))
            (setq found (semuse-v03--nested-shape-error-p (cadr tail))
                  tail (cddr tail)))
          found)))
   ((proper-list-p value)
    (cl-some #'semuse-v03--nested-shape-error-p value))
   (t t)))

(defun semuse-v03--shape-code (prefix error)
  "Map strict plist ERROR under PREFIX to a stable public code."
  (intern (format "%s-%s"
                  prefix
                  (pcase error
                    ('odd-plist "plist-odd")
                    ('nonkeyword-key "key-not-keyword")
                    ('duplicate-key "duplicate-key")
                    (_ "not-plist")))))

(defun semuse-v03--shape-gate (records evaluator-manifest)
  "Validate every plist shape before any getter, hash, or join."
  (let (errors)
    (if (not (proper-list-p records))
        (push 'records-not-list errors)
      (dolist (record records)
        (let ((error (semuse-v03--plist-shape-error record)))
          (if error
              (push (semuse-v03--shape-code "record" error) errors)
            (let ((envelope (plist-get record :visible-inputs))
                  (action (plist-get record :first-action)))
              (when (and envelope
                         (semuse-v03--plist-shape-error envelope))
                (push (semuse-v03--shape-code
                       "envelope"
                       (semuse-v03--plist-shape-error envelope))
                      errors))
              (when (and envelope
                         (semuse-v03--nested-shape-error-p envelope))
                (push 'envelope-nested-shape errors))
              (when (and action
                         (semuse-v03--plist-shape-error action))
                (push 'first-action-shape errors))
              (when (and action
                         (semuse-v03--nested-shape-error-p action))
                (push 'first-action-nested-shape errors)))))))
    (let ((manifest-error
           (semuse-v03--plist-shape-error evaluator-manifest)))
      (if manifest-error
          (push (semuse-v03--shape-code "manifest" manifest-error) errors)
        (dolist (field '(:families :rows :schedule))
          (let ((values (plist-get evaluator-manifest field)))
            (if (not (proper-list-p values))
                (push (intern (format "%s-not-list"
                                      (substring (symbol-name field) 1)))
                      errors)
              (dolist (value values)
                (let ((error (semuse-v03--plist-shape-error value)))
                  (when error
                    (push (semuse-v03--shape-code
                           (if (eq field :schedule) "schedule" "manifest")
                           error)
                          errors)))))))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--schema-errors (object required allowed missing unknown)
  "Return closed-schema errors for already shape-checked OBJECT."
  (let ((keys (cl-loop for (key _value) on object by #'cddr collect key))
        errors)
    (dolist (key required)
      (unless (memq key keys) (push missing errors)))
    (dolist (key keys)
      (unless (memq key allowed) (push unknown errors)))
    (delete-dups (nreverse errors))))

(defun semuse-v03--manifest-entry (manifest assignment-id)
  "Return ASSIGNMENT-ID's sealed schedule entry."
  (cl-find assignment-id (plist-get manifest :schedule)
           :key (lambda (entry) (plist-get entry :assignment-id))
           :test #'equal))

(defconst semuse-v03-sealed-evaluator-manifest
  (list
   :contract-revision "v0.3"
   :families (copy-tree semuse-v02-families t)
   :rows (copy-tree semuse-v02-rows t)
   :schedule
   (mapcar
    (lambda (entry)
      (let* ((row (semuse-v02-row (plist-get entry :row-id)))
             (family (semuse-v02-row-family row))
             (condition (plist-get entry :condition))
             (artifact (semuse-v02-artifact family condition))
             (bytes (and artifact (semuse-v02--utf8-bytes artifact)))
             (sha (and bytes (secure-hash 'sha256 bytes))))
        (list :position (plist-get entry :position)
              :assignment-id (plist-get entry :assignment-code)
              :row-id (plist-get entry :row-id)
              :condition condition
              :family-id (plist-get family :id)
              :expected-target (semuse-v02-target row condition)
              :artifact-bytes artifact
              :artifact-revision
              (and sha (format "rev-%s" (substring sha 0 12)))
              :artifact-byte-length (and bytes (length bytes))
              :artifact-sha256 sha
              :decision-snapshot-id
              (semuse-v02--opaque-id 'snapshot
                                     (plist-get entry :position))
              :first-action-call-id
              (semuse-v02--opaque-id 'call
                                     (plist-get entry :position))
              :first-action-event-id
              (semuse-v02--opaque-id 'action
                                     (plist-get entry :position))
              :first-action-terminal-id
              (semuse-v02--opaque-id 'terminal
                                     (plist-get entry :position)))))
    semuse-v02-frozen-schedule)))

(defconst semuse-v03-sealed-evaluator-sha256
  (secure-hash 'sha256
               (encode-coding-string
                (prin1-to-string semuse-v03-sealed-evaluator-manifest)
                'utf-8 t)))

(defun semuse-v03--structure-schema-errors (manifest)
  "Validate sealed MANIFEST fields and types before hashing it."
  (let ((errors
         (semuse-v03--schema-errors
          manifest '(:contract-revision :families :rows :schedule)
          '(:contract-revision :families :rows :schedule)
          'manifest-field-missing 'manifest-schema-key)))
    (unless (equal (plist-get manifest :contract-revision) "v0.3")
      (push 'manifest-revision errors))
    (dolist (family (plist-get manifest :families))
      (setq errors
            (append errors
                    (semuse-v03--schema-errors
                     family semuse-v03-family-fields semuse-v03-family-fields
                     'family-field-missing 'family-schema-key)))
      (unless (and (symbolp (plist-get family :id))
                   (cl-every
                    (lambda (field)
                      (semuse-v02--nonempty-string-p
                       (plist-get family field)))
                    '(:card :world-a :world-b :decoy :sham :x1 :x2)))
        (push 'family-field-type errors)))
    (dolist (row (plist-get manifest :rows))
      (setq errors
            (append errors
                    (semuse-v03--schema-errors
                     row semuse-v03-row-fields semuse-v03-row-fields
                     'row-field-missing 'row-schema-key)))
      (unless (and (semuse-v02--nonempty-string-p (plist-get row :id))
                   (symbolp (plist-get row :family))
                   (keywordp (plist-get row :x-key)))
        (push 'row-field-type errors)))
    (dolist (entry (plist-get manifest :schedule))
      (setq errors
            (append errors
                    (semuse-v03--schema-errors
                     entry semuse-v03-schedule-fields
                     semuse-v03-schedule-fields
                     'schedule-field-missing 'schedule-schema-key)))
      (unless (and (integerp (plist-get entry :position))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :assignment-id))
                   (semuse-v02--nonempty-string-p (plist-get entry :row-id))
                   (memq (plist-get entry :condition) semuse-v03-conditions)
                   (symbolp (plist-get entry :family-id))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :expected-target))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :decision-snapshot-id))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :first-action-call-id))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :first-action-event-id))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :first-action-terminal-id)))
        (push 'schedule-field-type errors))
      (let ((condition (plist-get entry :condition)))
        (if (eq condition 'withheld)
            (unless (cl-every
                     #'null
                     (mapcar (lambda (field) (plist-get entry field))
                             '(:artifact-bytes :artifact-revision
                               :artifact-byte-length :artifact-sha256)))
              (push 'schedule-withheld-artifact errors))
          (unless (and
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :artifact-bytes))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :artifact-revision))
                   (integerp (plist-get entry :artifact-byte-length))
                   (semuse-v02--nonempty-string-p
                    (plist-get entry :artifact-sha256)))
            (push 'schedule-artifact-type errors)))))
    (delete-dups errors)))

(defun semuse-v03--structure-gate (manifest)
  "Validate the complete sealed structure inside the scorer closure."
  (let ((errors (semuse-v03--structure-schema-errors manifest)))
    (unless errors
      (let* ((families (plist-get manifest :families))
             (rows (plist-get manifest :rows))
             (actual-sha
              (secure-hash
               'sha256
               (encode-coding-string (prin1-to-string manifest) 'utf-8 t))))
        (dolist (family families)
          (let ((family-id (plist-get family :id)))
            (dolist (condition '(world-a world-b decoy sham))
              (unless (semuse-v02--parameter-valid-p
                       family condition (plist-get family
                                                   (pcase condition
                                                     ('world-a :world-a)
                                                     ('world-b :world-b)
                                                     ('decoy :decoy)
                                                     ('sham :sham))))
                (push (format "parameter-grammar:%s:%s"
                              family-id condition)
                      errors)))
            (unless (= 2 (cl-count family-id rows
                                   :key (lambda (row)
                                          (plist-get row :family))))
              (push (format "row-count:%s" family-id) errors))))
        (dolist (row rows)
          (let* ((family
                  (cl-find (plist-get row :family) families
                           :key (lambda (item) (plist-get item :id))))
                 (x (and family (plist-get family (plist-get row :x-key)))))
            (unless (and family (semuse-v02--x-valid-p family x))
              (push (format "x-grammar:%s" (plist-get row :id)) errors))))
        (unless (equal actual-sha semuse-v03-sealed-evaluator-sha256)
          (push 'sealed-manifest-sha256 errors))
        (dolist (entry (plist-get manifest :schedule))
          (let ((expected
                 (semuse-v03--manifest-entry
                  semuse-v03-sealed-evaluator-manifest
                  (plist-get entry :assignment-id))))
            (unless (and expected
                         (cl-every
                          (lambda (field)
                            (equal (plist-get entry field)
                                   (plist-get expected field)))
                          '(:position :row-id :condition :family-id
                            :expected-target)))
              (push 'schedule-truth-join errors))
            (unless (and expected
                         (cl-every
                          (lambda (field)
                            (equal (plist-get entry field)
                                   (plist-get expected field)))
                          '(:artifact-bytes :artifact-revision
                            :artifact-byte-length :artifact-sha256)))
              (push 'schedule-artifact-join errors))
            (unless (and expected
                         (cl-every
                          (lambda (field)
                            (equal (plist-get entry field)
                                   (plist-get expected field)))
                          '(:decision-snapshot-id :first-action-call-id
                            :first-action-event-id
                            :first-action-terminal-id)))
              (push 'schedule-evidence-join errors))))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--baseline-records ()
  "Return detached actor-visible records with evaluator truth removed."
  (mapcar
   (lambda (legacy)
     (let* ((record (copy-tree legacy t))
            (assignment-id (plist-get record :assignment-code))
            (position (plist-get record :schedule-position))
            (condition (plist-get record :condition))
            (visible (copy-tree (plist-get record :provider-envelope) t))
            (snapshot-id (semuse-v02--opaque-id 'snapshot position))
            (call-id (semuse-v02--opaque-id 'call position))
            (action-id (semuse-v02--opaque-id 'action position))
            (terminal-id (semuse-v02--opaque-id 'terminal position)))
       (when (eq condition 'withheld)
         (setq visible (semuse-v03--plist-remove visible '(:artifact))))
       (setq record
             (semuse-v03--plist-remove
              record
              '(:assignment-code :schedule-position :row-id :condition
                :scorer-expected-target :scorer-truth-join
                :provider-envelope)))
       (plist-put record :contract-revision "v0.3")
       (plist-put record :assignment-id assignment-id)
       (plist-put record :visible-inputs visible)
       (plist-put record :decision-snapshot-id snapshot-id)
       (plist-put record :first-action
                  (list :attempt-ordinal 1
                        :snapshot-id snapshot-id
                        :call-id call-id
                        :call-ordinal 1
                        :tool-name "workspace_info"
                        :arguments nil
                        :action-event-id action-id
                        :terminal
                        (list :event-id terminal-id
                              :causation-id action-id
                              :call-id call-id
                              :status 'completed)))
       record))
   (semuse-v02-synthetic-records 'passing)))

(defun semuse-v03--record-schema-errors (records)
  "Validate actor record schemas and basic field types."
  (let (errors)
    (dolist (record records)
      (setq errors
            (append errors
                    (semuse-v03--schema-errors
                     record semuse-v03-record-fields semuse-v03-record-fields
                     'record-field-missing 'record-schema-key)))
      (unless (equal (plist-get record :contract-revision) "v0.3")
        (push 'record-revision errors))
      (unless (cl-every
               (lambda (field)
                 (semuse-v02--nonempty-string-p (plist-get record field)))
               '(:assignment-id :session-id :workspace-id :producer-run-id
                 :consumer-run-id :raw-output :raw-first-output
                 :terminal-output :raw-output-sha256
                 :raw-first-output-sha256 :decision-snapshot-id))
        (push 'record-field-type errors))
      (unless (and (stringp (plist-get record :raw-output-bytes))
                   (proper-list-p (plist-get record :request-ordinals))
                   (proper-list-p (plist-get record :terminal-sequence))
                   (proper-list-p (plist-get record :visible-inputs))
                   (proper-list-p (plist-get record :first-action)))
        (push 'record-field-type errors))
      (dolist (field '(:attempt-ordinal :first-outbound-request-ordinal
                       :retry-count :first-terminal-event-index
                       :raw-first-output-event-index :terminal-output-count
                       :answer-output-count))
        (unless (integerp (plist-get record field))
          (push 'record-field-type errors))))
    (delete-dups errors)))

(defun semuse-v03--adapt-records-for-v02 (records manifest)
  "Build detached v0.2 scoring records after every v0.3 gate passes."
  (mapcar
   (lambda (observed)
     (let* ((entry (semuse-v03--manifest-entry
                    manifest (plist-get observed :assignment-id)))
            (record (copy-tree observed t)))
       (setq record
             (semuse-v03--plist-remove
              record '(:assignment-id :visible-inputs
                       :decision-snapshot-id :first-action)))
       (plist-put record :contract-revision "v0.2")
       (plist-put record :assignment-code (plist-get entry :assignment-id))
       (plist-put record :schedule-position (plist-get entry :position))
       (plist-put record :row-id (plist-get entry :row-id))
       (plist-put record :condition (plist-get entry :condition))
       (plist-put record :provider-envelope
                  (copy-tree (plist-get observed :visible-inputs) t))
       record))
   records))

(defun semuse-v03--record-set-gate (records manifest)
  "Join opaque actor records one-to-one to the sealed schedule."
  (let ((errors (semuse-v03--record-schema-errors records))
        (seen (make-hash-table :test #'equal))
        (seen-identities
         (mapcar (lambda (field)
                   (cons field (make-hash-table :test #'equal)))
                 semuse-v03-runtime-id-fields)))
    (unless errors
      (unless (= (length records) (length (plist-get manifest :schedule)))
        (push 'record-count errors))
      (dolist (record records)
        (let* ((assignment-id (plist-get record :assignment-id))
               (entry (semuse-v03--manifest-entry manifest assignment-id))
               (condition (and entry (plist-get entry :condition))))
          (when (gethash assignment-id seen)
            (push 'duplicate-assignment errors))
          (puthash assignment-id t seen)
          (unless entry (push 'assignment-join errors))
          (dolist (field semuse-v03-runtime-id-fields)
            (let* ((value (plist-get record field))
                   (table (cdr (assq field seen-identities))))
              (when (gethash value table)
                (push 'duplicate-runtime-identity errors))
              (puthash value t table)))
          (if (eq condition 'withheld)
              (unless (eq (plist-get record :no-artifact-proof) 'proved)
                (push 'withheld-artifact-proof errors))
            (unless (eq (plist-get record :artifact-present-proof) 'proved)
              (push 'artifact-present-proof errors)))))
      (dolist (entry (plist-get manifest :schedule))
        (unless (gethash (plist-get entry :assignment-id) seen)
          (push 'missing-assignment errors))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--tree-oracle-errors (value forbidden-strings)
  "Return oracle errors from every key and scalar in VALUE."
  (let (errors)
    (cond
     ((vectorp value)
      (dolist (item (append value nil))
        (setq errors
              (append errors
                      (semuse-v03--tree-oracle-errors
                       item forbidden-strings)))))
     ((and (consp value) (keywordp (car value)))
      (let ((tail value))
        (while tail
          (when (memq (car tail)
                      '(:answer-key :assignment-code :row-id :condition
                        :schedule-position :expected-target :truth-join
                        :scorer-expected-target :scorer-truth-join))
            (push 'oracle-key errors))
          (setq errors
                (append errors
                        (semuse-v03--tree-oracle-errors
                         (cadr tail) forbidden-strings)))
          (setq tail (cddr tail)))))
     ((consp value)
      (dolist (item value)
        (setq errors
              (append errors
                      (semuse-v03--tree-oracle-errors
                       item forbidden-strings)))))
     ((stringp value)
      (dolist (forbidden forbidden-strings)
        (when (and (stringp forbidden)
                   (let ((case-fold-search t))
                     (string-match-p (regexp-quote forbidden) value)))
          (push 'oracle-value errors)))))
    (delete-dups errors)))

(defun semuse-v03--tree-has-key-p (value key)
  "Return non-nil when strict nested VALUE contains keyword KEY."
  (cond
   ((vectorp value)
    (cl-some (lambda (item) (semuse-v03--tree-has-key-p item key))
             (append value nil)))
   ((and (consp value) (keywordp (car value)))
    (let ((tail value)
          found)
      (while (and tail (not found))
        (setq found
              (or (eq (car tail) key)
                  (semuse-v03--tree-has-key-p (cadr tail) key))
              tail (cddr tail)))
      found))
   ((consp value)
    (cl-some (lambda (item) (semuse-v03--tree-has-key-p item key)) value))
   (t nil)))

(defun semuse-v03--tree-strings (value)
  "Return every string in strict nested VALUE in visible order."
  (cond
   ((stringp value) (list value))
   ((vectorp value)
    (apply #'append (mapcar #'semuse-v03--tree-strings
                            (append value nil))))
   ((and (consp value) (keywordp (car value)))
    (let ((tail value)
          strings)
      (while tail
        (setq strings
              (append strings (semuse-v03--tree-strings (cadr tail)))
              tail (cddr tail)))
      strings))
   ((consp value)
    (apply #'append (mapcar #'semuse-v03--tree-strings value)))
   (t nil)))

(defun semuse-v03--nonblank-string-p (value)
  "Return non-nil only for a string containing a visible character."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun semuse-v03--has-key-p (plist key)
  "Return non-nil when shape-checked PLIST contains KEY."
  (memq key (cl-loop for (item _value) on plist by #'cddr collect item)))

(defun semuse-v03--envelope-gate (records manifest)
  "Validate closed visible inputs and scan their complete recursive value."
  (let (errors)
    (dolist (record records)
      (let* ((entry (semuse-v03--manifest-entry
                     manifest (plist-get record :assignment-id)))
             (condition (plist-get entry :condition))
             (envelope (plist-get record :visible-inputs))
             (required (if (eq condition 'withheld)
                           '(:prompt :messages :tool-schema :runtime-ids)
                         semuse-v03-envelope-fields))
             (schema-errors
              (semuse-v03--schema-errors
               envelope required semuse-v03-envelope-fields
               'visible-field-missing 'visible-schema-key))
             (row-id (plist-get entry :row-id))
             (sealed-oracle-strings
              (cl-loop for candidate in (plist-get manifest :schedule)
                       append
                       (delq nil
                             (list
                              (plist-get candidate :assignment-id)
                              (plist-get candidate :row-id)
                              (and (memq (plist-get candidate :condition)
                                         '(world-a world-b decoy))
                                   (plist-get candidate :expected-target))))))
             (forbidden
              (append sealed-oracle-strings
                      (mapcar #'symbol-name
                              '(world-a world-b decoy sham withheld))))
             (flat-visible
              (apply #'concat (semuse-v03--tree-strings envelope)))
             (flat-non-artifact
              (apply #'concat
                     (semuse-v03--tree-strings
                      (semuse-v03--plist-remove envelope '(:artifact)))))
             (sealed-artifacts
              (delq nil
                    (mapcar (lambda (candidate)
                              (plist-get candidate :artifact-bytes))
                            (plist-get manifest :schedule)))))
        (setq errors (append errors schema-errors))
        (when (and (eq condition 'withheld)
                   (semuse-v03--tree-has-key-p envelope :artifact))
          (push 'withheld-artifact-present errors))
        (dolist (field '(:prompt :tool-schema))
          (unless (semuse-v03--nonblank-string-p (plist-get envelope field))
            (push 'visible-field-empty errors)))
        (let ((messages (plist-get envelope :messages)))
          (unless (and (proper-list-p messages) messages
                       (cl-every #'semuse-v03--nonblank-string-p messages))
            (push 'visible-field-empty errors)))
        (unless (eq condition 'withheld)
          (unless (semuse-v03--nonblank-string-p
                   (plist-get envelope :artifact))
            (push 'visible-field-empty errors)))
        (let ((ids (plist-get envelope :runtime-ids)))
          (setq errors
                (append errors
                        (semuse-v03--schema-errors
                         ids semuse-v03-runtime-id-fields
                         semuse-v03-runtime-id-fields
                         'runtime-id-missing 'runtime-id-schema-key)))
          (dolist (field semuse-v03-runtime-id-fields)
            (let ((value (plist-get ids field)))
              (unless (and (stringp value)
                           (string-match-p "\\`opaque-[a-z0-9]+\\'" value))
                (push 'runtime-id-type errors))
              (unless (equal value (plist-get record field))
                (push 'runtime-id-join errors)))))
        (unless (eq (plist-get record :provider-envelope-proof) 'proved)
          (push 'provider-envelope-proof errors))
        (unless (eq (plist-get record :opaque-runtime-ids-proof) 'proved)
          (push 'opaque-runtime-ids-proof errors))
        (setq errors
              (append errors
                      (semuse-v03--tree-oracle-errors envelope forbidden)
                      (semuse-v03--tree-oracle-errors flat-visible forbidden)))
        (when (cl-some
               (lambda (artifact)
                 (string-match-p (regexp-quote artifact) flat-non-artifact))
               sealed-artifacts)
          (push 'artifact-disclosure errors))
        (when (eq condition 'withheld)
          (when (or
                 (string-match-p "SEMANTIC-USE OPERATIONS CARD" flat-visible)
                 (cl-some
                  (lambda (candidate)
                    (let ((artifact (plist-get candidate :artifact-bytes)))
                      (and artifact
                           (string-match-p (regexp-quote artifact)
                                           flat-visible))))
                  (plist-get manifest :schedule)))
            (push 'withheld-artifact-present errors)))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--artifact-parameter (artifact)
  "Extract the one exact parameter line from ARTIFACT, or nil."
  (when (and
         (stringp artifact)
         (string-match
         (concat
           "\\`SEMANTIC-USE OPERATIONS CARD\n"
           "CARD=[^\n]+\n"
           "FAMILY=[A-Z]+\n"
           "DECISIVE-PARAMETER=\\([^\n]+\\)\n"
           "STATUS=ACTIVE\n\\'")
          artifact))
    (match-string 1 artifact)))

(defun semuse-v03--artifact-gate (records manifest)
  "Join exact visible Artifact bytes, metadata, grammar, and schedule."
  (let (errors)
    (dolist (record records)
      (let* ((entry (semuse-v03--manifest-entry
                     manifest (plist-get record :assignment-id)))
             (condition (plist-get entry :condition))
             (artifact (plist-get (plist-get record :visible-inputs)
                                  :artifact)))
        (if (eq condition 'withheld)
            (unless (cl-every
                     #'null
                     (mapcar (lambda (field) (plist-get record field))
                             '(:artifact-id :artifact-revision
                               :artifact-byte-length :artifact-sha256)))
              (push 'withheld-artifact-metadata errors))
          (let* ((bytes (semuse-v02--utf8-bytes artifact))
                 (length (length bytes))
                 (sha (secure-hash 'sha256 bytes))
                 (revision (format "rev-%s" (substring sha 0 12)))
                 (family
                  (cl-find (plist-get entry :family-id)
                           (plist-get manifest :families)
                           :key (lambda (item) (plist-get item :id))))
                 (parameter (semuse-v03--artifact-parameter artifact)))
            (unless (and parameter
                         (semuse-v02--parameter-valid-p
                          family condition parameter))
              (push 'artifact-grammar errors))
            (unless (equal artifact (plist-get entry :artifact-bytes))
              (push 'artifact-schedule-bytes errors))
            (unless (and (= length (plist-get record :artifact-byte-length))
                         (= length
                            (plist-get entry :artifact-byte-length)))
              (push 'artifact-byte-length errors))
            (unless (and (equal sha (plist-get record :artifact-sha256))
                         (equal sha (plist-get entry :artifact-sha256)))
              (push 'artifact-sha256 errors))
            (unless (and (equal revision
                                (plist-get record :artifact-revision))
                         (equal revision
                                (plist-get entry :artifact-revision)))
              (push 'artifact-revision errors))))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--first-action-gate (records manifest)
  "Validate first output evidence and immutable first tool Action origin."
  (let* ((adapted (semuse-v03--adapt-records-for-v02 records manifest))
         (seen-events (make-hash-table :test #'equal))
         (seen-calls (make-hash-table :test #'equal))
         errors)
    (dolist (record records)
      (let* ((entry (semuse-v03--manifest-entry
                     manifest (plist-get record :assignment-id)))
             (action (plist-get record :first-action))
             (snapshot-id (plist-get action :snapshot-id))
             (call-id (plist-get action :call-id))
             (action-id (plist-get action :action-event-id))
             (terminal (plist-get action :terminal))
             (terminal-id (plist-get terminal :event-id))
             (tool-name (plist-get action :tool-name))
             (tool-surface
              (mapcar #'string-trim
                      (string-split
                       (plist-get (plist-get record :visible-inputs)
                                  :tool-schema)
                       ";" t)))
             (schema-errors
              (append
               (semuse-v03--schema-errors
                action semuse-v03-first-action-fields
                semuse-v03-first-action-fields
                'first-action-field-missing
                'first-action-schema-key)
               (semuse-v03--schema-errors
                terminal semuse-v03-action-terminal-fields
                semuse-v03-action-terminal-fields
                'first-action-terminal-field-missing
                'first-action-terminal-schema-key))))
        (setq errors (append errors schema-errors))
        (unless schema-errors
          (unless (and (integerp (plist-get action :attempt-ordinal))
                       (= (plist-get action :attempt-ordinal) 1)
                       (equal snapshot-id
                              (plist-get record :decision-snapshot-id))
                       (equal snapshot-id
                              (plist-get entry :decision-snapshot-id))
                       (stringp snapshot-id)
                       (string-match-p "\\`opaque-[a-z0-9]+\\'" snapshot-id)
                       (semuse-v02--nonempty-string-p call-id)
                       (equal call-id (plist-get entry :first-action-call-id))
                       (integerp (plist-get action :call-ordinal))
                       (= (plist-get action :call-ordinal) 1)
                       (equal tool-name "workspace_info")
                       (member tool-name tool-surface)
                       (null (plist-get action :arguments))
                       (equal action-id
                              (plist-get entry :first-action-event-id))
                       (equal terminal-id
                              (plist-get entry :first-action-terminal-id))
                       (equal (plist-get terminal :causation-id) action-id)
                       (equal (plist-get terminal :call-id) call-id)
                       (eq (plist-get terminal :status) 'completed))
            (push 'first-action-origin errors))
          (when (gethash call-id seen-calls)
            (push 'first-action-duplicate-call errors))
          (dolist (event-id (list snapshot-id action-id terminal-id))
            (when (gethash event-id seen-events)
              (push 'first-action-duplicate-event errors))
            (when event-id (puthash event-id t seen-events)))
          (when call-id (puthash call-id t seen-calls)))))
    (dolist (record adapted)
      (let ((legacy (semuse-v02-first-attempt-gate record)))
        (unless (plist-get legacy :ok)
          (setq errors (append errors (plist-get legacy :errors))))))
    (list :ok (null errors) :errors (delete-dups (nreverse errors)))))

(defun semuse-v03--record-for-entry (records entry)
  "Return the observed record joined to sealed schedule ENTRY."
  (cl-find (plist-get entry :assignment-id) records
           :key (lambda (record) (plist-get record :assignment-id))
           :test #'equal))

(defun semuse-v03--answer-state (record entry)
  "Score RECORD output only against sealed evaluator ENTRY."
  (let ((parsed (semuse-v02-extract (plist-get record :raw-output))))
    (pcase (plist-get parsed :status)
      ('valid
       (if (equal (plist-get parsed :value)
                  (plist-get entry :expected-target))
           'correct
         'incorrect))
      ('missing-output 'missing-output)
      (_ 'protocol-failure))))

(defun semuse-v03--content-score (records manifest)
  "Score content cells without consulting v0.2 global truth."
  (let (cells witness-failures answer-failures)
    (dolist (entry (plist-get manifest :schedule))
      (when (memq (plist-get entry :condition) '(world-a world-b decoy))
        (let* ((record (semuse-v03--record-for-entry records entry))
               (witness
                (and (eq (plist-get record :access-proof) 'proved)
                     (eq (plist-get record :request-inclusion-proof) 'proved)))
               (answer (semuse-v03--answer-state record entry)))
          (push (list :row-id (plist-get entry :row-id)
                      :condition (plist-get entry :condition)
                      :answer answer
                      :value (plist-get
                              (semuse-v02-extract
                               (plist-get record :raw-output))
                              :value)
                      :status (cond
                               ((not witness) 'not-identifiable)
                               ((eq answer 'correct) 'observed)
                               (t 'not-supported)))
                cells)
          (unless witness
            (push (plist-get entry :assignment-id) witness-failures))
          (unless (eq answer 'correct)
            (push (plist-get entry :assignment-id) answer-failures)))))
    (setq cells (nreverse cells))
    (let* ((families (plist-get manifest :families))
           (rows (plist-get manifest :rows))
           (value-at
            (lambda (row-id condition)
              (plist-get
               (cl-find-if
                (lambda (cell)
                  (and (equal (plist-get cell :row-id) row-id)
                       (eq (plist-get cell :condition) condition)))
                cells)
               :value)))
           (fixed-v-changes-with-x
            (cl-every
             (lambda (condition)
               (cl-every
                (lambda (family)
                  (let* ((family-id (plist-get family :id))
                         (family-rows
                          (cl-remove-if-not
                           (lambda (row)
                             (eq (plist-get row :family) family-id))
                           rows))
                         (values
                          (mapcar
                           (lambda (row)
                             (funcall value-at (plist-get row :id) condition))
                           family-rows)))
                    (and (= 2 (length family-rows))
                         (cl-every #'stringp values)
                         (= 2 (length (delete-dups
                                       (copy-sequence values)))))))
                families))
             '(world-a world-b decoy)))
           (fixed-x-changes-with-v
            (cl-every
             (lambda (row)
               (let ((values
                      (mapcar
                       (lambda (condition)
                         (funcall value-at (plist-get row :id) condition))
                       '(world-a world-b decoy))))
                 (and (cl-every #'stringp values)
                      (= 3 (length (delete-dups (copy-sequence values)))))))
             rows))
           (status
            (cond
             (witness-failures 'not-identifiable)
             ((or answer-failures
                  (not fixed-v-changes-with-x)
                  (not fixed-x-changes-with-v))
              'not-supported)
             (t 'consistent-with-two-input-use))))
      (list :status status :content-status status
            :content-cells cells
            :witness-failures (nreverse witness-failures)
            :answer-failures (nreverse answer-failures)
            :fixed-v-changes-with-x fixed-v-changes-with-x
            :fixed-x-changes-with-v fixed-x-changes-with-v
            :content-cell-count (length cells)))))

(defun semuse-v03--control-score (records manifest)
  "Score sham controls only against sealed evaluator truth."
  (let (target-hits non-abstain witness-failures)
    (dolist (entry (plist-get manifest :schedule))
      (when (eq (plist-get entry :condition) 'sham)
        (let* ((record (semuse-v03--record-for-entry records entry))
               (parsed (semuse-v02-extract (plist-get record :raw-output)))
               (value (plist-get parsed :value))
               (row-id (plist-get entry :row-id))
               (content-targets
                (cl-loop for candidate in (plist-get manifest :schedule)
                         when (and (equal (plist-get candidate :row-id) row-id)
                                   (memq (plist-get candidate :condition)
                                         '(world-a world-b)))
                         collect (plist-get candidate :expected-target))))
          (unless (and (eq (plist-get record :access-proof) 'proved)
                       (eq (plist-get record :request-inclusion-proof) 'proved))
            (push (plist-get entry :assignment-id) witness-failures))
          (when (member value content-targets) (push row-id target-hits))
          (unless (equal value "INSUFFICIENT") (push row-id non-abstain)))))
    (let ((failed (or target-hits non-abstain witness-failures)))
      (list :status (if failed 'control-driven-not-identifiable 'control-pass)
            :sham-status
            (if failed 'control-driven-not-identifiable 'control-pass)
            :target-hits (nreverse target-hits)
            :non-abstain (nreverse non-abstain)
            :witness-failures (nreverse witness-failures)))))

(defun semuse-v03--failure (stage errors gates)
  "Return the one fail-closed public result shape."
  (list :ok nil :contract-revision "v0.3"
        :stage stage :status 'not-identifiable
        :closure 'not-identifiable :errors errors :gates gates
        :inferential-claim nil))

(defun semuse-v03--cohort-result (records manifest)
  "Implement the single ordered scorer closure."
  (let (gates)
    (catch 'done
      (dolist (spec
               (list
                (list 'shape #'semuse-v03--shape-gate records manifest)
                (list 'structure #'semuse-v03--structure-gate manifest)
                (list 'record-set #'semuse-v03--record-set-gate
                      records manifest)
                (list 'provider-envelope #'semuse-v03--envelope-gate
                      records manifest)
                (list 'artifact #'semuse-v03--artifact-gate records manifest)
                (list 'first-action #'semuse-v03--first-action-gate
                      records manifest)))
        (let* ((stage (car spec))
               (result (apply (cadr spec) (cddr spec))))
          (setq gates (append gates (list (cons stage result))))
          (unless (plist-get result :ok)
            (throw 'done
                   (semuse-v03--failure
                    stage (plist-get result :errors) gates)))))
      (let* ((content (semuse-v03--content-score records manifest))
             (control (semuse-v03--control-score records manifest))
             (content-status (plist-get content :status))
             (control-status (plist-get control :status))
             (status
              (cond
               ((eq control-status 'control-driven-not-identifiable)
                'control-driven-not-identifiable)
               ((eq content-status 'not-identifiable) 'not-identifiable)
               ((eq content-status 'not-supported) 'not-supported)
               (t 'consistent-with-two-input-use))))
        (setq gates
              (append gates
                      (list (cons 'content content)
                            (cons 'control control))))
        (list :ok t :contract-revision "v0.3" :stage 'scored
              :status status :closure status :errors nil :gates gates
              :content-status content-status :content content
              :sham-status control-status :sham control
              :raw-content-cell-count 18 :inferential-claim nil)))))

(defun semuse-v03-cohort-result (records sealed-evaluator-manifest)
  "Score actor RECORDS against a separate SEALED-EVALUATOR-MANIFEST.

All malformed input closes as not-identifiable; no invalid fixture signals."
  (if (not (semuse-v03--acyclic-input-p
            records sealed-evaluator-manifest))
      (let ((shape '(:ok nil :errors (cyclic-or-oversized-input))))
        (semuse-v03--failure
         'shape '(cyclic-or-oversized-input) (list (cons 'shape shape))))
    (condition-case _err
        (semuse-v03--cohort-result
         (copy-tree records t) (copy-tree sealed-evaluator-manifest t))
      (error
       (semuse-v03--failure 'internal-error '(scorer-signal) nil)))))

(defun semuse-v03--fixture-valid ()
  "Return a detached valid v0.3 fixture bundle."
  (list :records (semuse-v03--baseline-records)
        :evaluator-manifest
        (copy-tree semuse-v03-sealed-evaluator-manifest t)))

(defun semuse-v03--fixture-answer-key-top ()
  "Return the smallest reproduced complete-envelope oracle attack."
  (let* ((fixture (semuse-v03--fixture-valid))
         (records (plist-get fixture :records))
         (record (car records))
         (entry (semuse-v03--manifest-entry
                 (plist-get fixture :evaluator-manifest)
                 (plist-get record :assignment-id)))
         (envelope (plist-get record :visible-inputs)))
    (plist-put record :visible-inputs
               (append envelope
                       (list :answer-key
                             (plist-get entry :expected-target))))
    fixture))

(defun semuse-v03--fixture-record (fixture &optional withheld)
  "Return a content record, or a WITHHELD record, from FIXTURE."
  (cl-find-if
   (lambda (record)
     (let ((entry
            (semuse-v03--manifest-entry
             (plist-get fixture :evaluator-manifest)
             (plist-get record :assignment-id))))
       (eq (eq (plist-get entry :condition) 'withheld) (and withheld t))))
   (plist-get fixture :records)))

(defun semuse-v03--fixture-target (fixture record)
  "Return RECORD's sealed target in FIXTURE."
  (plist-get
   (semuse-v03--manifest-entry
    (plist-get fixture :evaluator-manifest)
    (plist-get record :assignment-id))
   :expected-target))

(defun semuse-v03--fixture-cell-record (fixture row-id condition)
  "Return FIXTURE's record joined to ROW-ID and CONDITION."
  (let ((entry
         (cl-find-if
          (lambda (candidate)
            (and (equal (plist-get candidate :row-id) row-id)
                 (eq (plist-get candidate :condition) condition)))
          (plist-get (plist-get fixture :evaluator-manifest) :schedule))))
    (semuse-v03--record-for-entry (plist-get fixture :records) entry)))

(defun semuse-v03--sync-artifact-metadata (record artifact)
  "Replace RECORD's visible ARTIFACT and self-reported byte metadata."
  (let* ((bytes (semuse-v02--utf8-bytes artifact))
         (sha (secure-hash 'sha256 bytes))
         (envelope (plist-get record :visible-inputs)))
    (plist-put envelope :artifact artifact)
    (plist-put record :artifact-byte-length (length bytes))
    (plist-put record :artifact-sha256 sha)
    (plist-put record :artifact-revision
               (format "rev-%s" (substring sha 0 12)))))

(defun semuse-v03--fixture (name)
  "Return one detached valid or attacked fixture named NAME."
  (pcase name
    ('valid-baseline (semuse-v03--fixture-valid))
    ('top-answer-key (semuse-v03--fixture-answer-key-top))
    (_
     (let* ((fixture (semuse-v03--fixture-valid))
            (record (semuse-v03--fixture-record fixture))
            (envelope (plist-get record :visible-inputs))
            (manifest (plist-get fixture :evaluator-manifest)))
       (pcase name
         ('nested-answer-key
          (let ((ids (plist-get envelope :runtime-ids)))
            (plist-put envelope :runtime-ids
                       (append ids
                               (list :answer-key
                                     (semuse-v03--fixture-target
                                     fixture record))))))
         ('cross-row-target
          (let* ((other (semuse-v03--fixture-cell-record
                         fixture "offset-x1" 'world-a))
                 (target (semuse-v03--fixture-target fixture other)))
            (plist-put envelope :prompt
                       (concat (plist-get envelope :prompt) target))))
         ('cross-artifact-leak
          (let* ((entry (semuse-v03--manifest-entry
                         manifest (plist-get record :assignment-id)))
                 (assigned (plist-get entry :artifact-bytes))
                 (foreign
                  (cl-loop for candidate in (plist-get manifest :schedule)
                           for artifact = (plist-get candidate :artifact-bytes)
                           when (and artifact (not (equal artifact assigned)))
                           return artifact)))
            (plist-put envelope :prompt
                       (concat (plist-get envelope :prompt) foreign))))
         ('split-oracle-target
          (let* ((target (semuse-v03--fixture-target fixture record))
                 (cut (/ (length target) 2)))
            (plist-put envelope :messages
                       (list (substring target 0 cut)
                             (substring target cut)))))
         ('unknown-field-target
          (plist-put record :visible-inputs
                     (append envelope
                             (list :debug
                                   (semuse-v03--fixture-target
                                    fixture record)))))
         ((or 'missing-prompt 'missing-messages
              'missing-tool-schema 'missing-artifact)
          (plist-put record :visible-inputs
                     (semuse-v03--plist-remove
                      envelope
                      (list (pcase name
                              ('missing-prompt :prompt)
                              ('missing-messages :messages)
                              ('missing-tool-schema :tool-schema)
                              ('missing-artifact :artifact))))))
         ((or 'nil-prompt 'nil-messages 'nil-tool-schema 'nil-artifact)
          (plist-put envelope
                     (pcase name
                       ('nil-prompt :prompt)
                       ('nil-messages :messages)
                       ('nil-tool-schema :tool-schema)
                       ('nil-artifact :artifact))
                     nil))
         ('withheld-artifact-nil
          (setq record (semuse-v03--fixture-record fixture t)
                envelope (plist-get record :visible-inputs))
          (plist-put record :visible-inputs
                     (append envelope (list :artifact nil))))
         ('withheld-artifact-nested
          (setq record (semuse-v03--fixture-record fixture t)
                envelope (plist-get record :visible-inputs))
          (let ((ids (plist-get envelope :runtime-ids)))
            (plist-put envelope :runtime-ids
                       (append ids (list :debug (list :artifact nil))))))
         ('withheld-artifact-split
          (setq record (semuse-v03--fixture-record fixture t)
                envelope (plist-get record :visible-inputs))
          (let* ((artifact
                  (cl-loop for entry in (plist-get manifest :schedule)
                           when (plist-get entry :artifact-bytes)
                           return (plist-get entry :artifact-bytes)))
                 (cut 10))
            (plist-put envelope :messages
                       (list (substring artifact 0 cut)
                             (substring artifact cut)))))
         ('record-odd-plist
          (setcdr (last record 2) nil))
         ('record-nonkeyword-key
          (setcar record 'contract-revision))
         ('record-duplicate-key
          (setcdr (last record)
                  (list :assignment-id "opaque-contradiction")))
         ('envelope-duplicate-key
          (plist-put record :visible-inputs
                     (append envelope (list :prompt nil))))
         ('nested-duplicate-key
          (let ((ids (plist-get envelope :runtime-ids)))
            (plist-put envelope :runtime-ids
                       (append ids (list :session-id "opaque-other")))))
         ('cyclic-visible-inputs
          (setcdr (last envelope) envelope))
         ('schedule-duplicate-key
          (let ((entry (car (plist-get manifest :schedule))))
            (setcdr (last entry) (list :condition 'world-b))))
         ((or 'actual-bad-grammar 'artifact-bytes-mismatch)
          (semuse-v03--sync-artifact-metadata
           record
           (replace-regexp-in-string
            "MUL-007-ZQ"
            (if (eq name 'actual-bad-grammar)
                "BAD-019-ZQ"
              "MUL-008-ZQ")
            (plist-get envelope :artifact) t t)))
         ('artifact-length-mismatch
          (plist-put record :artifact-byte-length
                     (1+ (plist-get record :artifact-byte-length))))
         ('artifact-sha-mismatch
          (plist-put record :artifact-sha256 (make-string 64 ?0)))
         ('artifact-revision-mismatch
          (plist-put record :artifact-revision "rev-wrong000000"))
         ('schedule-truth-mismatch
          (let ((entry (car (plist-get manifest :schedule))))
            (plist-put entry :expected-target "RES-99999-KC")))
         ('baseline-structure-fails
          (plist-put (car (plist-get manifest :families))
                     :decoy "BAD-019-ZQ"))
         ('first-action-origin-mismatch
          (plist-put (plist-get record :first-action)
                     :snapshot-id "opaque-wrong"))
         ('first-action-coupled-forgery
          (let* ((action (plist-get record :first-action))
                 (terminal (plist-get action :terminal)))
            (plist-put record :decision-snapshot-id "opaque-forgedsnapshot")
            (plist-put action :snapshot-id "opaque-forgedsnapshot")
            (plist-put action :call-id "opaque-forgedcall")
            (plist-put action :action-event-id "opaque-forgedaction")
            (plist-put terminal :event-id "opaque-forgedterminal")
            (plist-put terminal :causation-id "opaque-forgedaction")
            (plist-put terminal :call-id "opaque-forgedcall")))
         ('tool-action-mismatch
          (plist-put envelope :tool-schema "unrelated_tool"))
         ('blank-visible-inputs
          (plist-put envelope :prompt " ")
          (plist-put envelope :messages '("\t"))
          (plist-put envelope :tool-schema "\n"))
         ('global-truth-drift
          (semuse-v02--set-raw
           (semuse-v03--fixture-cell-record fixture "scale-x1" 'world-b)
           "ANSWER: RES-00204-KC")
          (semuse-v02--set-raw
           (semuse-v03--fixture-cell-record fixture "scale-x2" 'world-b)
           "ANSWER: RES-00136-KC"))
         (_ (error "Unknown v0.3 fixture: %S" name)))
       fixture))))

(defconst semuse-v03-audit-registry
  '((valid-baseline t scored consistent-with-two-input-use nil)
    (top-answer-key nil provider-envelope not-identifiable oracle-key)
    (nested-answer-key nil provider-envelope not-identifiable oracle-key)
    (cross-row-target nil provider-envelope not-identifiable oracle-value)
    (cross-artifact-leak nil provider-envelope not-identifiable
                         artifact-disclosure)
    (split-oracle-target nil provider-envelope not-identifiable oracle-value)
    (unknown-field-target nil provider-envelope not-identifiable
                          visible-schema-key)
    (missing-prompt nil provider-envelope not-identifiable
                    visible-field-missing)
    (missing-messages nil provider-envelope not-identifiable
                      visible-field-missing)
    (missing-tool-schema nil provider-envelope not-identifiable
                         visible-field-missing)
    (missing-artifact nil provider-envelope not-identifiable
                      visible-field-missing)
    (nil-prompt nil provider-envelope not-identifiable visible-field-empty)
    (nil-messages nil provider-envelope not-identifiable visible-field-empty)
    (nil-tool-schema nil provider-envelope not-identifiable
                     visible-field-empty)
    (nil-artifact nil provider-envelope not-identifiable visible-field-empty)
    (withheld-artifact-nil nil provider-envelope not-identifiable
                           withheld-artifact-present)
    (withheld-artifact-nested nil provider-envelope not-identifiable
                              withheld-artifact-present)
    (withheld-artifact-split nil provider-envelope not-identifiable
                             withheld-artifact-present)
    (record-odd-plist nil shape not-identifiable record-plist-odd)
    (record-nonkeyword-key nil shape not-identifiable
                           record-key-not-keyword)
    (record-duplicate-key nil shape not-identifiable record-duplicate-key)
    (envelope-duplicate-key nil shape not-identifiable
                            envelope-duplicate-key)
    (nested-duplicate-key nil shape not-identifiable envelope-nested-shape)
    (cyclic-visible-inputs nil shape not-identifiable
                           cyclic-or-oversized-input)
    (schedule-duplicate-key nil shape not-identifiable
                            schedule-duplicate-key)
    (actual-bad-grammar nil artifact not-identifiable artifact-grammar)
    (artifact-bytes-mismatch nil artifact not-identifiable
                             artifact-schedule-bytes)
    (artifact-length-mismatch nil artifact not-identifiable
                              artifact-byte-length)
    (artifact-sha-mismatch nil artifact not-identifiable artifact-sha256)
    (artifact-revision-mismatch nil artifact not-identifiable
                                artifact-revision)
    (schedule-truth-mismatch nil structure not-identifiable
                             schedule-truth-join)
    (baseline-structure-fails nil structure not-identifiable
                              "parameter-grammar:scale:decoy")
    (first-action-origin-mismatch nil first-action not-identifiable
                                  first-action-origin)
    (first-action-coupled-forgery nil first-action not-identifiable
                                  first-action-origin)
    (tool-action-mismatch nil first-action not-identifiable
                          first-action-origin)
    (blank-visible-inputs nil provider-envelope not-identifiable
                          visible-field-empty)
    (global-truth-drift t scored not-supported nil)))

(defun semuse-v03--audit-one (spec)
  "Run one black-box audit SPEC only through the public scorer."
  (condition-case _err
      (let* ((name (nth 0 spec))
             (expected-ok (nth 1 spec))
             (expected-stage (nth 2 spec))
             (expected-status (nth 3 spec))
             (expected-error (nth 4 spec))
             (fixture (semuse-v03--fixture name))
             (result
              (semuse-v03-cohort-result
               (plist-get fixture :records)
               (plist-get fixture :evaluator-manifest)))
             (pass
              (and
               (and (eq (and (plist-get result :ok) t) expected-ok)
                    (eq (plist-get result :stage) expected-stage)
                    (eq (plist-get result :status) expected-status)
                    (or (null expected-error)
                        (member expected-error (plist-get result :errors))))
               t)))
        (list :name name :ok pass :result result
              :expected-stage expected-stage
              :expected-status expected-status
              :expected-error expected-error))
    (error (list :name (car spec) :ok nil :error 'audit-signal))))

(defun semuse-v03-audit (&optional fixture-name)
  "Audit every registered fixture, or only FIXTURE-NAME."
  (let* ((specs
          (if fixture-name
              (let ((spec (assq fixture-name semuse-v03-audit-registry)))
                (if spec (list spec) nil))
            semuse-v03-audit-registry))
         (checks (mapcar #'semuse-v03--audit-one specs)))
    (list :ok (and specs (cl-every (lambda (check)
                                     (plist-get check :ok))
                                   checks))
          :fixture fixture-name
          :checks checks)))

(provide 'remediation-logic-v0.3)

;;; remediation-logic-v0.3.el ends here
