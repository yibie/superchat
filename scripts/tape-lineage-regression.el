;;; tape-lineage-regression.el --- supersession reads without whole-Tape scans -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `superchat-view-superseded-storage-ids' runs many times a turn.  It used to
;; fetch every row of every Session in the lineage only to read their ids;
;; on a 1.8k-row Session that was most of a turn's 96k rows read and the
;; garbage behind a 2-second pause.  These tests hold the answer equal to the
;; whole-scan reference on a branched lineage -- including a parent that
;; supersedes a turn after the branch was made -- and bound the rows read.
;;
;; Run from the repository root:
;;
;;   emacs -Q --batch -L . -l scripts/tape-lineage-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^tape-lineage/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'superchat-db)
(require 'superchat-projection)
(require 'superchat-tape-view)

(defmacro tape-lineage--with-db (&rest body)
  "Run BODY against a fresh, isolated Tape database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "tape-lineage-" t))
          (superchat-data-directory (file-name-as-directory dir))
          (superchat-db--connection nil)
          (superchat-db--path nil))
     (unwind-protect
         (progn (superchat-db-open) ,@body)
       (superchat-db-close)
       (delete-directory dir t))))

(defun tape-lineage--say (session text)
  "Append a user row with TEXT to SESSION; return its id."
  (superchat-db-tape-append session "user" text))

(defun tape-lineage--supersede (session &rest ids)
  "Append a `turn.superseded' fact naming IDS to SESSION."
  (superchat-db-tape-append
   session "system" "turn.superseded"
   :meta (json-encode
          `((event_type . "turn.superseded")
            (payload . ((superseded-storage-ids . ,(vconcat ids))))))))

(defun tape-lineage--branch (session parent through-id)
  "Start SESSION as a branch of PARENT through storage id THROUGH-ID."
  (superchat-db-tape-append
   session "anchor" "branch" :topic "branch"
   :meta (json-encode
          `((payload . ((parent_session_id . ,parent)
                        (parent_turn_id . ,through-id)))))))

(defun tape-lineage--reference (session-id)
  "The whole-scan answer `superchat-view-superseded-storage-ids' replaced."
  (let (ids)
    (dolist (segment (superchat-view--continuity-lineage-segments session-id))
      (dolist (row (superchat-view-entries
                    (plist-get segment :session-id) nil '("system")))
        (let* ((meta (ignore-errors
                       (json-parse-string (or (nth 4 row) "")
                                          :object-type 'plist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
               (payload (and meta (plist-get meta :payload)))
               (named (and (equal "turn.superseded" (plist-get meta :event_type))
                           (listp payload)
                           (plist-get payload :superseded-storage-ids))))
          (when (and (or (vectorp named) (listp named))
                     (cl-every (lambda (id) (and (integerp id) (> id 0)))
                               (append named nil)))
            (setq ids (append named ids))))))
    (sort (delete-dups ids) #'<)))

(defun tape-lineage--answer (session-id)
  "The product answer for SESSION-ID, sorted."
  (sort (copy-sequence (superchat-view-superseded-storage-ids session-id)) #'<))

(defun tape-lineage--rows-read (thunk)
  "Call THUNK and return the number of Tape rows `superchat-db-tape-select' returned."
  (let ((rows 0)
        (select (symbol-function 'superchat-db-tape-select)))
    (cl-letf (((symbol-function 'superchat-db-tape-select)
               (lambda (&rest args)
                 (let ((result (apply select args)))
                   (cl-incf rows (length result))
                   result))))
      (funcall thunk))
    rows))

(ert-deftest tape-lineage/a-branched-lineage-answers-as-the-whole-scan ()
  (tape-lineage--with-db
    (let* ((a1 (tape-lineage--say "A" "one"))
           (a2 (tape-lineage--say "A" "two"))
           (_ (tape-lineage--supersede "A" a1))
           (cut (tape-lineage--say "A" "three")))
      (tape-lineage--branch "B" "A" cut)
      (let ((b1 (tape-lineage--say "B" "four")))
        (tape-lineage--supersede "B" b1)
        (tape-lineage--branch "C" "B" b1)
        (tape-lineage--supersede "C" 999)
        (dolist (session '("A" "B" "C"))
          (should (equal (tape-lineage--reference session)
                         (tape-lineage--answer session))))
        ;; The parent goes on after the branch: its later fact still counts.
        (tape-lineage--supersede "A" a2)
        (dolist (session '("A" "B" "C"))
          (should (equal (tape-lineage--reference session)
                         (tape-lineage--answer session))))
        (should (equal (list a1 a2 b1 999) (tape-lineage--answer "C")))))))

(ert-deftest tape-lineage/malformed-facts-are-skipped-as-before ()
  (tape-lineage--with-db
    (tape-lineage--say "A" "one")
    (superchat-db-tape-append "A" "system" "x" :meta "{not json turn.superseded")
    (superchat-db-tape-append
     "A" "system" "x"
     :meta "{\"event_type\":\"turn.superseded\",\"payload\":{\"superseded-storage-ids\":[0,\"x\"]}}")
    (superchat-db-tape-append
     "A" "system" "x"
     :meta "{\"event_type\":\"other\",\"note\":\"turn.superseded\",\"payload\":{\"superseded-storage-ids\":[7]}}")
    (tape-lineage--supersede "A" 5)
    (should (equal '(5) (tape-lineage--answer "A")))
    (should (equal (tape-lineage--reference "A") (tape-lineage--answer "A")))))

(ert-deftest tape-lineage/a-cyclic-lineage-still-signals ()
  (tape-lineage--with-db
    (tape-lineage--branch "A" "B" 100)
    (tape-lineage--branch "B" "A" 100)
    (should-error (superchat-view-superseded-storage-ids "A"))))

(ert-deftest tape-lineage/a-long-session-is-not-read-whole ()
  (tape-lineage--with-db
    (let ((cut nil))
      (dotimes (i 300)
        (setq cut (tape-lineage--say "A" (format "row %d" i))))
      (tape-lineage--supersede "A" cut)
      (tape-lineage--branch "B" "A" cut)
      (dotimes (i 300)
        (tape-lineage--say "B" (format "row %d" i)))
      (should (equal (list cut) (tape-lineage--answer "B")))
      ;; Two branch-anchor lookups and two supersession facts at most;
      ;; the whole scan read all 602 rows.
      (should (<= (tape-lineage--rows-read
                   (lambda () (superchat-view-superseded-storage-ids "B")))
                  4)))))

;; ── Session configuration: remembered until the Tape grows ──

(defun tape-lineage--register-configuration ()
  "Register the projection as the session-view plugin does."
  (superchat-projection-register
   'session-configuration
   :version 1
   :initial #'superchat-view--session-config-initial
   :apply #'superchat-view--session-config-apply))

(defun tape-lineage--fact (session type payload)
  "Append a Runtime fact of TYPE with PAYLOAD to SESSION; return its id."
  (superchat-db-tape-append
   session "system" type
   :meta (json-encode `((event_type . ,type) (payload . ,payload)))))

(defun tape-lineage--configuration-reference (session-id)
  "SESSION-ID's configuration replayed afresh, as before the memo."
  (superchat-projection-replay
   'session-configuration (superchat-view-session-history session-id)))

(ert-deftest tape-lineage/the-configuration-follows-every-append ()
  (tape-lineage--with-db
    (tape-lineage--register-configuration)
    (let* ((level (tape-lineage--fact "A" "permission.level" '((level . "full"))))
           (cut (tape-lineage--say "A" "one")))
      (tape-lineage--branch "B" "A" cut)
      (dolist (step
               (list (lambda () (tape-lineage--fact "B" "sandbox.mode" '((mode . "edit-buffers"))))
                     (lambda () (tape-lineage--say "B" "two"))
                     ;; The parent replaces the level the branch inherited.
                     (lambda () (tape-lineage--supersede "A" level))
                     (lambda () (tape-lineage--fact "A" "permission.level" '((level . "ask"))))
                     (lambda () (tape-lineage--fact "B" "permission.level" '((level . "read"))))))
        (dolist (session '("A" "B"))
          (should (equal (tape-lineage--configuration-reference session)
                         (superchat-view-session-configuration session)))
          ;; A second read in the same state answers the same.
          (should (equal (tape-lineage--configuration-reference session)
                         (superchat-view-session-configuration session))))
        (funcall step))
      (should (equal "read" (plist-get (superchat-view-session-configuration "B")
                                       :permission-level)))
      (should-not (plist-get (superchat-view-session-configuration "A")
                             :sandbox-mode)))))

(ert-deftest tape-lineage/a-caller-changing-the-configuration-changes-no-later-answer ()
  (tape-lineage--with-db
    (tape-lineage--register-configuration)
    (tape-lineage--fact "A" "permission.level" '((level . "full")))
    (let ((config (superchat-view-session-configuration "A")))
      (plist-put config :permission-level "changed")
      (setcar (cdr config) "changed"))
    (should (equal (tape-lineage--configuration-reference "A")
                   (superchat-view-session-configuration "A")))))

(ert-deftest tape-lineage/a-second-read-in-the-same-state-reads-one-row ()
  (tape-lineage--with-db
    (tape-lineage--register-configuration)
    (dotimes (i 200) (tape-lineage--say "A" (format "row %d" i)))
    (tape-lineage--fact "A" "permission.level" '((level . "full")))
    (superchat-view-session-configuration "A")
    (should (<= (tape-lineage--rows-read
                 (lambda () (superchat-view-session-configuration "A")))
                1))))

(ert-deftest tape-lineage/another-database-is-not-answered-from-memory ()
  (let (first)
    (tape-lineage--with-db
      (tape-lineage--register-configuration)
      (tape-lineage--fact "A" "permission.level" '((level . "full")))
      (setq first (superchat-view-session-configuration "A")))
    (tape-lineage--with-db
      (tape-lineage--register-configuration)
      (tape-lineage--fact "A" "permission.level" '((level . "ask")))
      (should (equal "full" (plist-get first :permission-level)))
      (should (equal "ask" (plist-get (superchat-view-session-configuration "A")
                                      :permission-level))))))

(provide 'tape-lineage-regression)
;;; tape-lineage-regression.el ends here
