;;; test-projection.el --- Projection contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-db)
(require 'superchat-projection)

(defmacro test-projection--with-temp-store (&rest body)
  "Run BODY with an isolated Superchat database."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "superchat-projection-test-" t))
          (superchat-data-directory directory))
     (unwind-protect
         (progn
           (superchat-db-close)
           (superchat-db-open)
           ,@body)
       (superchat-db-close)
       (delete-directory directory t))))

(ert-deftest test-projection-replay-reduces-events ()
  (superchat-projection-register
   'test-sum
   :version 1
   :initial 0
   :apply (lambda (sum event) (+ sum event)))
  (should (= 6 (superchat-projection-replay 'test-sum '(1 2 3)))))

(ert-deftest experience-feedback/human-correction-keeps-history ()
  "A correction changes the current Claim without erasing its history."
  (let ((superchat-projection--registry nil)
        (facts '("tape:961" "tape:962" "tape:964"
                 "tape:1577" "tape:1578" "tape:1580")))
    (superchat-projection-register
     'experience-feedback-canary
     :version 1
     :initial '(:claims nil :feedback nil :current nil)
     :apply
     (lambda (state event)
       (pcase (plist-get event :type)
         ("claim.recorded"
          (let ((current (plist-get state :current))
                (claim-id (plist-get event :claim-id)))
            (list
             :claims (append (plist-get state :claims) (list event))
             :feedback (plist-get state :feedback)
             :current
             (if (or (null current)
                     (equal (plist-get event :supersedes) current))
                 claim-id
               current))))
         ("feedback.recorded"
          (list :claims (plist-get state :claims)
                :feedback (append (plist-get state :feedback) (list event))
                :current (plist-get state :current)))
         (_ state))))
    (let* ((events
            (list
             (list :type "claim.recorded"
                   :claim-id "claim-v6-recovery-001"
                   :actor-id "agent:auditor"
                   :statement
                   "Switching from read to search is a stable recovery strategy."
                   :basis '("tape:1577" "tape:1578" "tape:1580"))
             (list :type "feedback.recorded"
                   :feedback-id "feedback-v6-recovery-001"
                   :actor-id "human:maintainer"
                   :target "claim-v6-recovery-001"
                   :kind 'correct
                   :basis facts
                   :replacement "claim-v6-recovery-002")
             (list :type "claim.recorded"
                   :claim-id "claim-v6-recovery-002"
                   :actor-id "human:maintainer"
                   :statement
                   "The two recovery episodes differ; no stable strategy is established."
                   :basis facts
                   :supersedes "claim-v6-recovery-001")))
           (view (superchat-projection-replay
                  'experience-feedback-canary events))
           (claims (plist-get view :claims))
           (feedback (car (plist-get view :feedback))))
      (should (= 2 (length claims)))
      (should (equal "claim-v6-recovery-001"
                     (plist-get (car claims) :claim-id)))
      (should (equal "claim-v6-recovery-002"
                     (plist-get (cadr claims) :claim-id)))
      (should (equal "claim-v6-recovery-002" (plist-get view :current)))
      (should (equal "human:maintainer" (plist-get feedback :actor-id)))
      (should (equal 'correct (plist-get feedback :kind)))
      (should (equal "claim-v6-recovery-001"
                     (plist-get feedback :target)))
      (should (equal "claim-v6-recovery-002"
                     (plist-get feedback :replacement)))
      (should (equal "claim-v6-recovery-001"
                     (plist-get (cadr claims) :supersedes)))
      (should (equal facts (plist-get feedback :basis))))))

(ert-deftest test-projection-checkpoint-is-atomic-and-idempotent ()
  (test-projection--with-temp-store
    (let ((db (superchat-db-open)))
      (sqlite-execute db "CREATE TABLE projection_test (event_id INTEGER PRIMARY KEY)")
      (superchat-db-tape-append "projection" "system" "one"
                                :topic "projection/test")
      (superchat-db-tape-append "projection" "system" "two"
                                :topic "projection/test")
      (superchat-projection-register
       'test-materialized
       :version 1
       :apply
       (lambda (_state event)
         (sqlite-execute
          (superchat-db-open)
          "INSERT INTO projection_test (event_id) VALUES (?)"
          (list (car event)))
         (when (= (car event) 2)
           (error "synthetic apply failure")))
       :select
       (lambda (since-id _scope)
         (superchat-db-tape-select
          "SELECT id FROM tape
           WHERE topic = 'projection/test' AND id > ? ORDER BY id"
          (list since-id)))
       :reset
       (lambda (_scope)
         (sqlite-execute (superchat-db-open) "DELETE FROM projection_test")))
      (should-error (superchat-projection-catch-up 'test-materialized))
      (should
       (equal '(1 1)
              (superchat-db-projection-checkpoint 'test-materialized)))
      (should
       (equal '((1))
              (sqlite-select db "SELECT event_id FROM projection_test")))
      (superchat-projection-register
       'test-materialized
       :version 1
       :apply
       (lambda (_state event)
         (sqlite-execute
          (superchat-db-open)
          "INSERT OR IGNORE INTO projection_test (event_id) VALUES (?)"
          (list (car event))))
       :select
       (lambda (since-id _scope)
         (superchat-db-tape-select
          "SELECT id FROM tape
           WHERE topic = 'projection/test' AND id > ? ORDER BY id"
          (list since-id)))
       :reset
       (lambda (_scope)
         (sqlite-execute (superchat-db-open) "DELETE FROM projection_test")))
      (should (= 2 (superchat-projection-catch-up 'test-materialized)))
      (should (= 2 (superchat-projection-catch-up 'test-materialized)))
      (should
       (equal '((1) (2))
              (sqlite-select
               db "SELECT event_id FROM projection_test ORDER BY event_id")))
      (superchat-projection-register
       'test-materialized
       :version 2
       :apply
       (lambda (_state event)
         (sqlite-execute
          (superchat-db-open)
          "INSERT INTO projection_test (event_id) VALUES (?)"
          (list (car event))))
       :select
       (lambda (since-id _scope)
         (superchat-db-tape-select
          "SELECT id FROM tape
           WHERE topic = 'projection/test' AND id > ? ORDER BY id"
          (list since-id)))
       :reset
       (lambda (_scope)
         (sqlite-execute (superchat-db-open) "DELETE FROM projection_test")))
      (should (= 2 (superchat-projection-catch-up 'test-materialized)))
      (should
       (equal '(2 2)
              (superchat-db-projection-checkpoint 'test-materialized))))))

(provide 'test-projection)

;;; test-projection.el ends here
