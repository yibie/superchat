;;; test-claims.el --- Explicit Claim capture tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-closure)
(require 'superchat-db)
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
      (should
       (superchat-claim-record "cross-session" "scope" other-basis "session"))
      (should
       (string-prefix-p
        "evt_"
        (plist-get
         (superchat-claim-record "unsupported" "scope" nil "session")
         :event-id))))))

(ert-deftest claims/legacy-basis-is-global-but-typed-basis-is-session-exact ()
  "Legacy storage cursors remain global while typed references require Session."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((record
            (superchat-tape-append-event-record
             '(:type "tool.completed" :session-id "fact-session"
               :payload (:tool-name "probe" :result "fact"))))
           (storage-id (plist-get record :storage-id))
           (event-id (plist-get record :event-id)))
      (should
       (superchat-claim-record
        "legacy cross-session" "scope"
        (list (format "tape:%d" storage-id)) "claim-session"))
      (should
       (superchat-claim-record
        "typed event" "scope"
        (list (list :session-id "fact-session" :event-id event-id))
        "claim-session"))
      (should
       (superchat-claim-record
        "typed storage" "scope"
        (list (list :session-id "fact-session" :storage-id storage-id))
        "claim-session"))
      (should-error
       (superchat-claim-record
        "wrong typed event Session" "scope"
        (list (list :session-id "other-session" :event-id event-id))
        "claim-session"))
      (should-error
       (superchat-claim-record
        "wrong typed storage Session" "scope"
        (list (list :session-id "other-session" :storage-id storage-id))
        "claim-session")))))

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

(ert-deftest claims/experience-events-do-not-block-next-run-terminal ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "claim-closure-session")
           (turn-id "turn-after-claim")
           (run-id "run-after-claim")
           (original
            (superchat-claim-record "A contestable conclusion."
                                    "scope" nil session-id))
           (_correction
            (superchat-claim-correct
             (plist-get original :event-id)
             "A corrected conclusion."
             "scope" nil "Human correction." session-id))
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

(ert-deftest claims/correction-rolls-back-when-feedback-fails ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "rollback-session")
           (scope "scope")
           (fact-id
            (superchat-tape-append-event
             (list :type "tool.completed" :session-id session-id)))
           (basis (list (format "tape:%d" fact-id)))
           (original
            (superchat-claim-record "Original" scope basis session-id))
           feedback-called)
      (cl-letf (((symbol-function 'superchat-feedback-record)
                 (lambda (&rest _args)
                   (setq feedback-called t)
                   (error "injected feedback failure"))))
        (should-error
         (superchat-claim-correct
          (plist-get original :event-id) "Replacement" scope basis
          "Correction note" session-id)))
      (should feedback-called)
      (should (= 1 (length (superchat-claims-replay session-id scope)))))))

(ert-deftest claims/current-view-survives-correction-and-restart ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "correction-session")
           (scope "cohort-v6/discovery")
           (other-fact-id
            (superchat-tape-append-event
             (list :type "tool.completed" :session-id "other-session")))
           (basis (list (format "tape:%d" other-fact-id)))
           (original
            (superchat-claim-record "A stable recovery exists."
                                    scope basis session-id "agent:auditor"))
           (correction
            (superchat-claim-correct
             (plist-get original :event-id)
             "No stable recovery is established."
             scope basis "The two recovery episodes differ." session-id))
           (replacement-id
            (plist-get (plist-get correction :replacement) :event-id)))
      (should-error
       (superchat-feedback-record
        "evt_missing" 'correct basis "Missing target" session-id))
      (superchat-db-close)
      (superchat-db-open)
      (let* ((view (superchat-claims-current-view session-id scope))
             (claims (plist-get view :claims))
             (feedback (car (plist-get view :feedback))))
        (should (= 2 (length claims)))
        (should (= 1 (length (plist-get view :feedback))))
        (should (equal replacement-id (plist-get view :current)))
        (should (equal (plist-get original :event-id)
                       (plist-get feedback :target)))
        (should (equal replacement-id (plist-get feedback :replacement)))
        (should (equal "correct" (plist-get feedback :kind)))
        (should (equal (plist-get original :event-id)
                       (plist-get (cadr claims) :supersedes)))))))

(ert-deftest claims/current-view-accepts-feedback-before-replacement ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-claims--with-temp-store
    (let* ((session-id "ordering-session")
           (scope "scope")
           (original (superchat-claim-record "Original" scope nil session-id))
           (original-id (plist-get original :event-id)))
      (superchat-feedback-record original-id 'correct nil "Correction first"
                                 session-id)
      (let* ((replacement
              (superchat-claim-record "Replacement" scope nil session-id
                                      "user" original-id))
             (view (superchat-claims-current-view session-id scope)))
        (should (= 2 (length (plist-get view :claims))))
        (should (= 1 (length (plist-get view :feedback))))
        (should (equal (plist-get replacement :event-id)
                       (plist-get view :current)))))))

(provide 'test-claims)

;;; test-claims.el ends here
