;;; step-material-regression.el --- request evidence for Step material -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests using canonical smoke as a library.  With the prepared ELPA
;; and pinned llm source, run `^step-material/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Step-material regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defmacro step-material--fixture (script &rest body)
  "Run BODY with SCRIPT in the canonical isolated fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil))
     (smoke--with-fixture ,script ,@body)))

(defun step-material--session (view)
  "Return VIEW's Session identity."
  (buffer-local-value 'superchat--session-id view))

(defun step-material--settle (run)
  "Wait for RUN to finish."
  (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
  (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))

(defun step-material--lists (session)
  "Return SESSION's request.message-list events in Tape order."
  (cl-remove-if-not
   (lambda (event) (equal "request.message-list" (plist-get event :type)))
   (superchat-tape-replay-events session)))

(defun step-material--items (event)
  "Return EVENT's composition items as a list."
  (append (plist-get (plist-get event :payload) :items) nil))

(defun step-material--step-items (event)
  "Return the step-material items in EVENT."
  (cl-remove-if-not
   (lambda (item) (equal "step-material" (plist-get item :kind)))
   (step-material--items event)))

(defun step-material--evidence (session event)
  "Rebuild SESSION's request named by EVENT."
  (superchat-tape-request-evidence
   session (plist-get (plist-get event :payload) :request-id)))

(defun step-material--read-round (&optional delay)
  "Return a scripted read_buffer call after optional DELAY."
  (append '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
          (when delay (list :delay delay))))

(ert-deftest step-material/steer-in-a-later-step ()
  (step-material--fixture
      (list (step-material--read-round 0.1) '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view))
           (run (smoke--send view "read")))
      (should (superchat-runtime-steer run "E1-STEER"))
      (step-material--settle run)
      (let* ((lists (step-material--lists session))
             (items (step-material--step-items (cadr lists)))
             (evidence (step-material--evidence session (cadr lists))))
        (should (= 2 (length lists)))
        (should-not (step-material--step-items (car lists)))
        (should (= 1 (length items)))
        (should (equal "message" (plist-get (car items) :form)))
        (should (eq 'reproduced (plist-get evidence :status)))
        (should (cl-find '(:kind "user" :content "E1-STEER")
                         (plist-get evidence :messages) :test #'equal))
        (should (string-match-p "User: Content: E1-STEER"
                                (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest step-material/first-step-fold ()
  (step-material--fixture '((:text "done"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view)))
      (should (stringp (superchat-runtime-inject
                        session '(:source (:kind test) :text "E1-FIRST"))))
      (step-material--settle (smoke--send view "answer"))
      (let* ((event (car (step-material--lists session)))
             (items (step-material--step-items event))
             (evidence (step-material--evidence session event)))
        (should (= 1 (length items)))
        (should (equal "prompt-block" (plist-get (car items) :form)))
        (should (cl-find '(:kind "user" :content "[Injected material] E1-FIRST"
                                     :form "prompt-block")
                         (plist-get evidence :messages) :test #'equal))
        (should (string-match-p "\\[Injected material\\] E1-FIRST"
                                (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest step-material/order-and-accumulation ()
  (step-material--fixture
      (list (step-material--read-round 0.1)
            (step-material--read-round 0.1) '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view))
           (run (smoke--send view "read twice")))
      (should (superchat-runtime-steer run "E1-ONE"))
      (should (smoke--wait (lambda () (= 2 (length smoke--requests))) 5))
      (should (superchat-runtime-steer run "E1-TWO"))
      (step-material--settle run)
      (let* ((lists (step-material--lists session))
             (second (step-material--step-items (nth 1 lists)))
             (third (step-material--step-items (nth 2 lists)))
             (evidence (step-material--evidence session (nth 2 lists))))
        (should (= 3 (length lists)))
        (should-not (step-material--step-items (car lists)))
        (should (= 1 (length second)))
        (should (= 2 (length third)))
        (should (equal (car second) (car third)))
        (should (< (plist-get (nth 0 third) :storage-id)
                   (plist-get (nth 1 third) :storage-id)))
        (should (equal '("E1-ONE" "E1-TWO")
                       (mapcar (lambda (m) (plist-get m :content))
                               (plist-get evidence :messages))))
        (should (string-match-p "User: Content: E1-ONE"
                                (plist-get (nth 1 (reverse smoke--requests)) :prompt-text)))
        (should (string-match-p "User: Content: E1-TWO"
                                (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest step-material/next-turn-history-rebuilds-material ()
  (step-material--fixture '((:text "first") (:text "second"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view)))
      (superchat-runtime-inject session '(:source (:kind test) :text "E1-HISTORY"))
      (step-material--settle (smoke--send view "first turn"))
      (step-material--settle (smoke--send view "second turn"))
      (let* ((lists (step-material--lists session))
             (second (cadr lists))
             (evidence (step-material--evidence session second)))
        (should (= 2 (length lists)))
        (should-not (step-material--step-items second))
        (should (eq 'reproduced (plist-get evidence :status)))
        (should (cl-find '(:kind "user" :content "[Injected material] E1-HISTORY")
                         (plist-get evidence :messages) :test #'equal))
        (should (string-match-p "User: \\[Injected material\\] E1-HISTORY"
                                (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest step-material/missing-or-omitted-row-is-unknown ()
  (step-material--fixture '((:text "done"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view)))
      (superchat-runtime-inject session '(:source (:kind test) :text "E1-DELETE"))
      (step-material--settle (smoke--send view "answer"))
      (let* ((event (car (step-material--lists session)))
             (item (car (step-material--step-items event)))
             (row-id (plist-get item :storage-id)))
        (should (integerp row-id))
        (sqlite-execute (superchat-db-open) "DELETE FROM tape WHERE id = ?" (list row-id))
        (should (eq 'unknown (plist-get (step-material--evidence session event) :status)))
        (let* ((user-row (car (superchat-db-tape-select
                               "SELECT id FROM tape WHERE session_id = ? AND kind = 'user' LIMIT 1"
                               (list session))))
               (fake-id "E1-FORGED"))
          (should user-row)
          (superchat-tape-request-record
           session fake-id "system" [] "model"
           (list (list :kind "step-material" :storage-id (car user-row) :form "message"))
           nil)
          (should (eq 'unknown
                      (plist-get (superchat-tape-request-evidence session fake-id)
                                 :status))))))))

(ert-deftest step-material/omitted-payload-is-unknown ()
  "An omitted attachment payload invalidates Step and later history evidence."
  (step-material--fixture '((:text "first") (:text "second"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view)))
      (superchat-runtime-inject
       session '(:source (:kind test) :text "E1-OMITTED"))
      (step-material--settle (smoke--send view "first turn"))
      (step-material--settle (smoke--send view "second turn"))
      (let* ((lists (step-material--lists session))
             (first (car lists))
             (second (cadr lists))
             (row-id (plist-get (car (step-material--step-items first))
                                :storage-id))
             (row (car (superchat-db-tape-select
                        "SELECT id, topic, kind, content, meta FROM tape WHERE id = ?"
                        (list row-id))))
             (meta (superchat-tape--decode-meta (nth 4 row)))
             (original-payload (plist-get meta :payload))
             (omitted
              (let ((superchat-tape-max-inline-bytes 1))
                (superchat-tape--encoded-payload original-payload))))
        (should (= 2 (length lists)))
        (should (integerp row-id))
        (should (cl-find row-id (step-material--items second)
                         :key (lambda (item) (plist-get item :storage-id))))
        (should (eq 'reproduced
                    (plist-get (step-material--evidence session first) :status)))
        (should (eq 'reproduced
                    (plist-get (step-material--evidence session second) :status)))
        (should (plist-get omitted :omitted))
        (should (numberp (plist-get omitted :size)))
        (should (stringp (plist-get omitted :content_hash)))
        (sqlite-execute
         (superchat-db-open) "UPDATE tape SET meta = ? WHERE id = ?"
         (list (superchat-tape--encode-meta
                (plist-put meta :payload omitted))
               row-id))
        (should (eq 'unknown
                    (plist-get (step-material--evidence session first) :status)))
        (should (eq 'unknown
                    (plist-get (step-material--evidence session second) :status)))
        ;; The exact bounded form lacks text/source, so those validators also
        ;; reject it.  Retain the omitted marker and its size/hash while
        ;; restoring otherwise valid fields to isolate the omitted guard.
        (let ((marked (append omitted
                              (list :text (plist-get original-payload :text)
                                    :source (plist-get original-payload :source)))))
          (sqlite-execute
           (superchat-db-open) "UPDATE tape SET meta = ? WHERE id = ?"
           (list (superchat-tape--encode-meta
                  (plist-put meta :payload marked))
                 row-id))
          (should (eq 'unknown
                      (plist-get (step-material--evidence session first) :status)))
          (should (eq 'unknown
                      (plist-get (step-material--evidence session second) :status))))))))

(ert-deftest step-material/plain-requests-unchanged ()
  (step-material--fixture '((:text "one") (:text "two"))
    (let* ((view (smoke--open-chat))
           (session (step-material--session view)))
      (step-material--settle (smoke--send view "first"))
      (step-material--settle (smoke--send view "second"))
      (let ((lists (step-material--lists session)))
        (should (= 2 (length lists)))
        (dolist (event lists)
          (should-not (step-material--step-items event))
          (let ((evidence (step-material--evidence session event)))
            (should (eq 'reproduced (plist-get evidence :status)))
            (should (equal (mapcar (lambda (item)
                                     (let* ((row (car (superchat-db-tape-select
                                                       "SELECT id, topic, kind, content, meta FROM tape WHERE id = ?"
                                                       (list (plist-get item :storage-id))))))
                                       (list :kind (nth 2 row) :content (nth 3 row))))
                                   (step-material--items event))
                           (plist-get evidence :messages)))))))))

(provide 'step-material-regression)
;;; step-material-regression.el ends here
