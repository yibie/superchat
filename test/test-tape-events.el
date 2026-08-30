;;; test-tape-events.el --- Tests for Superchat Runtime events -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-tape)
(require 'superchat-tape-view)

(defmacro superchat-test-tape--capture (binding &rest body)
  "Run BODY while BINDING receives arguments sent to the DB adapter."
  (declare (indent 1))
  `(let (,binding)
     (cl-letf (((symbol-function 'superchat-db-tape-append)
                (lambda (&rest args)
                  (setq ,binding args)
                  42)))
       ,@body)))

(ert-deftest tape-event/maps-runtime-types-to-legacy-kinds ()
  (dolist (case '(("intent.received" "user")
                  ("response.completed" "assistant")
                  ("tool.requested" "tool_call")
                  ("tool.completed" "tool_result")
                  ("runtime.changed" "system")))
    (superchat-test-tape--capture call
      (should (= 42
                 (superchat-tape-append-event
                  (list :type (car case) :session-id "s1"))))
      (should (equal (nth 1 call) (cadr case))))))

(ert-deftest tape-event/exposes-logical-id-without-changing-storage-cursor ()
  (superchat-test-tape--capture call
    (let ((record
           (superchat-tape-append-event-record
            '(:type "run.started" :session-id "s1"))))
      (should (= 42 (plist-get record :storage-id)))
      (should (string-prefix-p "evt_" (plist-get record :event-id)))
      (let ((meta (plist-get (nthcdr 3 call) :meta)))
        (should (equal (plist-get meta :event_id)
                       (plist-get record :event-id)))))))

(ert-deftest tape-event/legacy-interface-still-returns-storage-cursor ()
  (superchat-test-tape--capture call
    (should
     (= 42
        (superchat-tape-append-event
         '(:type "run.started"
           :session-id "s1"
           :event-id "evt_imported"))))
    (let ((meta (plist-get (nthcdr 3 call) :meta)))
      (should (equal "evt_imported" (plist-get meta :event_id))))))

(ert-deftest tape-material/preserves-full-content-through-tape-seam ()
  (superchat-test-tape--capture call
    (should
     (= 42
        (superchat-tape-append-material
         "s1" "anchor" "full material"
         :meta '((source . "test")) :topic "summary")))
    (should
     (equal '("s1" "anchor" "full material"
              :meta ((source . "test")) :topic "summary")
            call))))

(ert-deftest tape-material/requires-valid-material-fields ()
  (should-error (superchat-tape-append-material "" "anchor" "content"))
  (should-error (superchat-tape-append-material "s1" "" "content"))
  (should-error (superchat-tape-append-material "s1" "anchor" nil)))

(ert-deftest tape-event/storage-reference-is-session-scoped ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-tape-reference-" t))
         (superchat-data-directory directory))
    (unwind-protect
        (progn
          (superchat-db-close)
          (let ((storage-id
                 (superchat-tape-append-material "s1" "user" "legacy fact")))
            (should (superchat-tape-storage-reference-p "s1" storage-id))
            (should-not
             (superchat-tape-storage-reference-p "s2" storage-id))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest tape-event/requires-type-session-and-valid-schema ()
  (should-error (superchat-tape-append-event '(:session-id "s1")))
  (should-error (superchat-tape-append-event '(:type "run.started")))
  (should-error
   (superchat-tape-append-event '(:type run.started :session-id "s1")))
  (should-error
   (superchat-tape-append-event
    '(:type "run.started" :session-id "s1" :schema 0)))
  (should-error
   (superchat-tape-append-event
    '(:type "run.started" :session-id "s1" :event-id "")))
  (should-error
   (superchat-tape-append-event
    '(:type "run.started" :session-id "s1" :event-id 7))))

(ert-deftest tape-event/persists-versioned-envelope ()
  (superchat-test-tape--capture call
    (superchat-tape-append-event
     '(:type "tool.requested"
       :schema 3
       :session-id "s1"
       :turn-id "turn-1"
       :run-id "run-1"
       :actor-id "lead"
       :causation-id 9
       :payload (:tool-name "read-file")))
    (let ((meta (plist-get (nthcdr 3 call) :meta)))
      (should (equal (plist-get meta :event_type) "tool.requested"))
      (should (= (plist-get meta :schema) 3))
      (should (equal (plist-get meta :session_id) "s1"))
      (should (equal (plist-get meta :turn_id) "turn-1"))
      (should (equal (plist-get meta :run_id) "run-1"))
      (should (equal (plist-get meta :actor_id) "lead"))
      (should (= (plist-get meta :causation_id) 9))
      (should (string-match-p "Z\\'" (plist-get meta :recorded_at))))))

(ert-deftest tape-event/ignores-caller-supplied-recorded-time ()
  (superchat-test-tape--capture call
    (superchat-tape-append-event
     '(:type "run.started"
       :session-id "s1"
       :recorded-at "caller-controlled"))
    (let ((meta (plist-get (nthcdr 3 call) :meta)))
      (should-not
       (equal (plist-get meta :recorded_at) "caller-controlled")))))

(ert-deftest tape-event/redacts-sensitive-fields-recursively ()
  (superchat-test-tape--capture call
    (superchat-tape-append-event
     '(:type "tool.requested"
       :session-id "s1"
       :payload (:api-key "secret-a"
                 :safe "visible"
                 :nested ((password . "secret-b")
                          (label . "visible-too")))))
    (let* ((meta (plist-get (nthcdr 3 call) :meta))
           (payload (plist-get meta :payload))
           (nested (plist-get payload :nested)))
      (should (equal (plist-get payload :api-key) "[REDACTED]"))
      (should (equal (plist-get payload :safe) "visible"))
      (should (equal (alist-get 'password nested) "[REDACTED]"))
      (should (equal (alist-get 'label nested) "visible-too"))
      (should-not (string-match-p
                   "secret-[ab]" (prin1-to-string meta))))))

(ert-deftest tape-event/omits-oversized-inline-payload ()
  (let ((superchat-tape-max-inline-bytes 16))
    (superchat-test-tape--capture call
      (superchat-tape-append-event
       `(:type "tool.completed"
         :session-id "s1"
         :payload (:result ,(make-string 200 ?x))))
      (let* ((meta (plist-get (nthcdr 3 call) :meta))
             (payload (plist-get meta :payload)))
        (should (eq (plist-get payload :omitted) t))
        (should (> (plist-get payload :size) 16))
        (should (string-prefix-p
                 "sha256:" (plist-get payload :content_hash)))
        (should-not (string-match-p
                     (make-string 20 ?x) (prin1-to-string payload)))))))

(ert-deftest tape-event/required-oversized-payload-is-rejected ()
  (let ((superchat-tape-max-inline-bytes 16))
    (should-error
     (superchat-tape-append-event
      `(:type "memory.captured"
        :session-id "s1"
        :requires-payload t
        :payload (:content ,(make-string 200 ?x)))))))

(ert-deftest tape-event/exact-payload-rejects-redaction-needed-value ()
  "Exact structured evidence rejects secret-valued keyed fields."
  (should-error
   (superchat-tape-exact-json
    '(:attempt 1 :arguments (:api_key "secret-value")))))

(ert-deftest tape-event/exact-payload-allows-schema-names-in-strings ()
  "A schema key name is not itself a secret-valued action argument."
  (let ((receipt
         (superchat-tape-exact-json
          '(:tools [(:args "((api_key string \"Credential name\"))")]))))
    (should (string-match-p "api_key" (plist-get receipt :json)))
    (should (natnump (plist-get receipt :size)))
    (should (string-prefix-p "sha256:" (plist-get receipt :hash)))))

(ert-deftest tape-event/tool-summary-never-inlines-payload ()
  (superchat-test-tape--capture call
    (superchat-tape-append-event
     '(:type "tool.requested"
       :session-id "s1"
       :payload (:tool-name "write-file" :content "private body")))
    (should (equal (nth 2 call) "tool.requested: write-file"))
    (should-not (string-match-p "private body" (nth 2 call)))))

(ert-deftest tape-event/propagates-adapter-failure ()
  (cl-letf (((symbol-function 'superchat-db-tape-append)
             (lambda (&rest _args) (error "disk full"))))
    (should-error
     (superchat-tape-append-event
      '(:type "tool.requested" :session-id "s1"))
     :type 'error)))

(ert-deftest tape-event/replays-normalized-runtime-envelope ()
  "Persisted Tape events replay in the shape expected by projections."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-tape-replay-" t))
         (superchat-data-directory directory))
    (unwind-protect
        (progn
          (superchat-db-close)
          (superchat-db-tape-append
           "session-replay" "user" "legacy row without Runtime envelope")
          (superchat-tape-append-event
           '(:type "tool.requested"
             :event-id "evt-replay"
             :schema 1
             :session-id "session-replay"
             :turn-id "turn-replay"
             :run-id "run-replay"
             :actor-id "lead"
             :causation-id "evt-parent"
             :payload (:execution-id "exec-1" :decision "allow")))
          (let* ((events (superchat-tape-replay-events "session-replay"))
                 (event (car events)))
            (should (= 1 (length events)))
            (should (equal "tool.requested" (plist-get event :type)))
            (should (equal "evt-replay" (plist-get event :event-id)))
            (should (= 1 (plist-get event :schema)))
            (should (equal "session-replay" (plist-get event :session-id)))
            (should (equal "turn-replay" (plist-get event :turn-id)))
            (should (equal "run-replay" (plist-get event :run-id)))
            (should (equal "evt-parent" (plist-get event :causation-id)))
            (should (equal "exec-1"
                           (plist-get (plist-get event :payload)
                                      :execution-id)))
            (should (integerp (plist-get event :storage-id)))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest tape-event/compaction-anchor-is-not-replayed-as-runtime-event ()
  "A typed compaction Anchor stays material while Runtime replay skips it."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-tape-anchor-replay-" t))
         (superchat-data-directory directory)
         (session-id "session-anchor-replay"))
    (unwind-protect
        (progn
          (superchat-db-close)
          (let* ((source-id
                  (superchat-tape-append-material
                   session-id "user" "source fact"))
                 (anchor-id
                  (superchat-tape-append-compaction-anchor
                   session-id "source summary" (vector source-id)))
                 (_event-id
                  (superchat-tape-append-event
                   `(:type "intent.received"
                     :event-id "evt-anchor-replay"
                     :schema 1
                     :session-id ,session-id
                     :summary "next request"))))
            (let ((events (superchat-tape-replay-events session-id)))
              (should (= 1 (length events)))
              (should (equal "intent.received"
                             (plist-get (car events) :type))))
            (let ((anchor (superchat-view-compaction-anchor-latest session-id)))
              (should (= anchor-id (nth 0 anchor)))
              (should (equal (vector source-id)
                             (superchat-view-compaction-anchor-source-ids
                              anchor)))
              (should (= 1
                         (length
                          (superchat-view-expand-anchor session-id anchor)))))))
      (superchat-db-close)
      (delete-directory directory t))))

(provide 'test-tape-events)

;;; test-tape-events.el ends here
