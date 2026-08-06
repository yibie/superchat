;;; test-claims.el --- Explicit Claim capture tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'superchat-closure)
(require 'superchat-db)
(require 'superchat-projection)
(require 'superchat-tape)
(require 'superchat-claims)

(defmacro test-claims--with-temp-store (&rest body)
  "Run BODY with an isolated Superchat database."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "superchat-claims-test-" t))
          (superchat-data-directory directory))
     (unwind-protect
         (progn
           (superchat-db-close)
           (superchat-db-open)
           ,@body)
       (superchat-db-close)
       (delete-directory directory t))))

(defun test-claims--apply-correction (state event)
  "Apply correction-contract EVENT to STATE."
  (pcase (plist-get event :type)
    ("claim.recorded"
     (let ((current (plist-get state :current))
           (claim-id (plist-get event :claim-id)))
       (list :claims (append (plist-get state :claims) (list event))
             :feedback (plist-get state :feedback)
             :current (if (or (null current)
                              (equal (plist-get event :supersedes) current))
                          claim-id
                        current))))
    ("feedback.recorded"
     (list :claims (plist-get state :claims)
           :feedback (append (plist-get state :feedback) (list event))
           :current (plist-get state :current)))
    (_ state)))

(ert-deftest claims/interactive-capture-survives-restart ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "claim-session")
           (scope "cohort-v6/discovery")
           (fact-id
            (superchat-tape-append-event
             (list :type "tool.completed"
                   :session-id session-id
                   :actor-id "collector"
                   :payload '(:result "observed"))))
           (basis-ref (format "tape:%d" fact-id))
           record)
      (with-temp-buffer
        (insert "The recovery paths differ.")
        (goto-char (point-min))
        (push-mark (point-max) t t)
        (let ((superchat--session-id session-id)
              (transient-mark-mode t)
              (answers (list scope basis-ref)))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) (pop answers))))
            (setq record (call-interactively #'superchat-claim-capture)))))
      (should
       (equal '(("claim.recorded"))
              (superchat-db-tape-select
               "SELECT content FROM tape WHERE id = ?"
               (list (plist-get record :storage-id)))))
      (superchat-db-close)
      (superchat-db-open)
      (let ((claims (superchat-claims-replay session-id scope)))
        (should (= 1 (length claims)))
        (let ((claim (car claims)))
          (should (equal (plist-get record :event-id)
                         (plist-get claim :claim-id)))
          (should (equal "The recovery paths differ."
                         (plist-get claim :statement)))
          (should (equal scope (plist-get claim :scope)))
          (should (equal (list basis-ref) (plist-get claim :basis)))
          (should (equal "user" (plist-get claim :actor-id)))
          (should (string-match-p "Z\\'" (plist-get claim :recorded-at))))))))

(ert-deftest claims/rejects-invalid-input-but-allows-empty-basis ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((superchat--session-id nil)
           (other-fact-id
            (superchat-tape-append-event
             (list :type "tool.completed" :session-id "other-session")))
           (other-basis (list (format "tape:%d" other-fact-id))))
      (should-error (superchat-claim-record "" "scope" nil "session"))
      (should-error (superchat-claim-record "claim" "" nil "session"))
      (should-error (superchat-claim-record "claim" "scope" nil))
      (should-error
       (superchat-claim-record "claim" "scope" '("evt_wrong") "session"))
      (should-error
       (superchat-claim-record "claim" "scope" '("tape:999999") "session"))
      (should-error
       (superchat-claim-record "claim" "scope" other-basis "session"))
      (should
       (string-prefix-p
        "evt_"
        (plist-get
         (superchat-claim-record "unsupported" "scope" nil "session")
         :event-id))))))

(ert-deftest claims/supersedes-must-name-a-claim-in-the-same-scope ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "claim-session")
           (fact-id
            (superchat-tape-append-event
             (list :type "tool.completed" :session-id session-id)))
           (basis (list (format "tape:%d" fact-id)))
           (other-claim
            (superchat-claim-record "Other session" "scope-a" nil
                                    "other-session"))
           (original
            (superchat-claim-record "Original" "scope-a" basis session-id))
           (original-id (plist-get original :event-id)))
      (should-error
       (superchat-claim-record "Replacement" "scope-a" basis session-id
                               "user" "evt_missing"))
      (should-error
       (superchat-claim-record "Replacement" "scope-b" basis session-id
                               "user" original-id))
      (should-error
       (superchat-claim-record "Replacement" "scope-a" basis session-id
                               "user" (plist-get other-claim :event-id))))))

(ert-deftest claims/recorded-event-does-not-block-next-run-terminal ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "claim-closure-session")
           (turn-id "turn-after-claim")
           (run-id "run-after-claim")
           (_claim
            (superchat-claim-record "A contestable conclusion."
                                    "scope" nil session-id))
           (intent
            (superchat-tape-append-event-record
             (list :type "intent.received" :session-id session-id
                   :turn-id turn-id :run-id run-id :actor-id "user")))
           (started
            (superchat-tape-append-event-record
             (list :type "run.started" :session-id session-id
                   :turn-id turn-id :run-id run-id :actor-id "lead"
                   :causation-id (plist-get intent :event-id)
                   :payload '(:status running-model :actor-id "lead"
                              :capability "chat"))))
           (_response
            (superchat-tape-append-event-record
             (list :type "response.completed" :session-id session-id
                   :turn-id turn-id :run-id run-id :actor-id "lead"
                   :causation-id (plist-get started :event-id)
                   :payload '(:status completed :content-size 2))))
           (judgment
            (superchat-closure-judge
             (list :kind 'ready-terminal :run-id run-id
                   :terminal-kind 'completed)
             (superchat-tape-replay-events session-id))))
      (should (eq 'proved (plist-get judgment :verdict))))))

(ert-deftest claims/captured-event-feeds-human-correction-contract ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "correction-session")
           (scope "cohort-v6/discovery")
           (fact-id
            (superchat-tape-append-event
             (list :type "tool.completed" :session-id session-id)))
           (basis (list (format "tape:%d" fact-id)))
           (original-record
            (superchat-claim-record "A stable recovery exists."
                                    scope basis session-id "agent:auditor"))
           (original-id (plist-get original-record :event-id))
           (_replacement-record
            (superchat-claim-record "No stable recovery is established."
                                    scope basis session-id "user" original-id))
           (claims (superchat-claims-replay session-id scope))
           (feedback
            (list :type "feedback.recorded"
                  :feedback-id "feedback-1"
                  :actor-id "user"
                  :target original-id
                  :kind 'correct
                  :basis basis
                  :replacement (plist-get (cadr claims) :claim-id)))
           (events (list (car claims) feedback (cadr claims)))
           (view
            (seq-reduce #'test-claims--apply-correction events
                        '(:claims nil :feedback nil :current nil))))
      (should (= 2 (length (plist-get view :claims))))
      (should (= 1 (length (plist-get view :feedback))))
      (should (equal original-id
                     (plist-get (car (plist-get view :claims)) :claim-id)))
      (should (equal (plist-get (cadr claims) :claim-id)
                     (plist-get view :current)))
      (should (equal original-id
                     (plist-get (cadr claims) :supersedes))))))

(provide 'test-claims)

;;; test-claims.el ends here
