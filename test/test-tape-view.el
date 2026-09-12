;;; test-tape-view.el --- Tests for tape views -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the tape view layer and tape FTS5.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat-db)
(require 'superchat-tape-browser)
(require 'superchat-tape-view)

(defvar superchat-data-directory)

(defun test-tape-view--with-temp-db (body)
  "Run BODY with a fresh temporary database."
  (let* ((tmp-dir (make-temp-file "superchat-tape-view-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (funcall body))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest view/tape-browser-prints-populated-session-list ()
  "The sessions browser sorts and prints when Tape has multiple sessions."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "browser-session-a" "user" "first")
     (superchat-db-tape-append "browser-session-b" "user" "second")
     (let (buffer)
       (unwind-protect
           (cl-letf (((symbol-function 'display-buffer)
                      (lambda (candidate &rest _args)
                        (setq buffer candidate)
                        candidate)))
             (setq buffer (superchat-sessions))
             (should (buffer-live-p buffer))
             (with-current-buffer buffer
               (should (eq major-mode 'tabulated-list-mode))
               (should (string-match-p "browser-session-a" (buffer-string)))
               (should (string-match-p "browser-session-b" (buffer-string)))))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest test-db-open-applies-configured-busy-timeout ()
  "Each connection opened by Superchat applies the configured timeout."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((superchat-db-busy-timeout 1234))
    (test-tape-view--with-temp-db
     (lambda ()
       (should
        (equal 1234
               (caar (sqlite-select
                      superchat-db--connection
                      "PRAGMA busy_timeout"))))))))

(defun test-tape-view--append-config-event (session-id type payload)
  "Append a Runtime config event envelope to SESSION-ID."
  (superchat-db-tape-append
   session-id "system" type
   :meta `((event_type . ,type)
           (schema . 1)
           (payload . ,payload))))

(ert-deftest view/assemble-restart-rebuilds-raw-facts-with-disclosure ()
  "Assembly should rebuild prompt facts and disclosure from Tape alone."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-restart" "user" "hello")
     (superchat-db-tape-append "cont-restart" "assistant" "world")
     ;; Drop every in-process DB handle before exercising the read seam.
     (superchat-db-close)
     (superchat-db-open)
     (let* ((envelope (superchat-view-assemble "cont-restart"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure))
            (receipt (plist-get envelope :receipt)))
       (should (equal "context-continuity-v0"
                      (plist-get envelope :contract)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-restart#1-2 | represented none | omitted none | recall none]\n\nUser: hello\nAssistant: world"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "cont-restart"
           :storage-id 1
           :kind "user"
           :content "hello")
          (:class raw-fact
           :session-id "cont-restart"
           :storage-id 2
           :kind "assistant"
           :content "world")]
         (plist-get envelope :items)))
       (should (equal "cont-restart" (plist-get scope :session-id)))
       (should (eq 'prompt (plist-get scope :purpose)))
       (should (= 2 (plist-get scope :through-storage-id)))
       (should
        (equal [(:session-id "cont-restart" :through-storage-id 2)]
               (plist-get scope :lineage)))
       (should
        (equal "continuity-v0/recent-first"
               (plist-get scope :selection-policy)))
       (should
        (equal [(:session-id "cont-restart" :range [1 2])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :represented)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (equal [] (plist-get disclosure :recallable)))
       (should (= 2 (plist-get receipt :tape-head)))
       (should
        (equal "continuity-v0/recent-first"
               (plist-get receipt :selection-policy)))
       (should (= 1 (plist-get receipt :selection-policy-version)))))))

(ert-deftest view/assemble-empty-session-does-not-fabricate-context ()
  "An empty Tape scope should return an empty envelope and no prompt text."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((envelope (superchat-view-assemble "cont-empty"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'empty (plist-get envelope :status)))
       (should (equal "" (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should (null (plist-get scope :through-storage-id)))
       (should (equal [] (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :represented)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (equal [] (plist-get disclosure :recallable)))
       (should (null (plist-get (plist-get envelope :receipt)
                                :tape-head)))))))

(ert-deftest view/assemble-read-failure-returns-unknown-envelope ()
  "A real SQLite read failure should not escape the public View seam."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((db-file (expand-file-name "superchat.db"
                                      superchat-data-directory)))
       (superchat-db-close)
       (delete-file db-file)
       (with-temp-file db-file
         (insert "not a sqlite database"))
       (let* ((envelope (superchat-view-assemble "cont-broken"))
              (scope (plist-get envelope :scope))
              (disclosure (plist-get envelope :disclosure))
              (receipt (plist-get envelope :receipt)))
         (should (eq 'unknown (plist-get envelope :status)))
         (should (equal "" (plist-get envelope :prompt)))
         (should (equal [] (plist-get envelope :items)))
         (should (null (plist-get scope :through-storage-id)))
         (should (equal [] (plist-get disclosure :covered)))
         (should (equal [] (plist-get disclosure :represented)))
         (should (equal [] (plist-get disclosure :omitted)))
         (should (equal [] (plist-get disclosure :recallable)))
         (should (null (plist-get receipt :tape-head)))
         (should (eq 'failed (plist-get receipt :read-status)))
         (should (= 1 (plist-get receipt :selection-policy-version))))))))

(ert-deftest view/assemble-entry-budget-discloses-recallable-omission ()
  "A recent-first entry budget should disclose the exact omitted range."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-budget" "user" "one")
     (superchat-db-tape-append "cont-budget" "assistant" "two")
     (superchat-db-tape-append "cont-budget" "user" "three")
     (let* ((envelope (superchat-view-assemble
                       "cont-budget" :max-entries 2))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-budget#2-3 | represented none | omitted 1 | recall available]\n\nAssistant: two\nUser: three"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "cont-budget"
           :storage-id 2
           :kind "assistant"
           :content "two")
          (:class raw-fact
           :session-id "cont-budget"
           :storage-id 3
           :kind "user"
           :content "three")]
         (plist-get envelope :items)))
       (should
        (equal [(:session-id "cont-budget" :range [2 3])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :represented)))
       (should
        (equal [(:session-id "cont-budget"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-budget"
                 :selector (:range [1 1]))]
               (plist-get disclosure :recallable)))
       (should (= 3 (plist-get (plist-get envelope :receipt) :tape-head)))))))

(ert-deftest view/assemble-default-entry-budget-is-unlimited ()
  "An omitted Assemble entry budget should not cap the View at fifty facts."
  (test-tape-view--with-temp-db
   (lambda ()
     (dotimes (index 51)
       (superchat-db-tape-append
        "cont-default-budget" "user" (format "entry-%02d" (1+ index))))
     (let* ((envelope (superchat-view-assemble "cont-default-budget"))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should (= 51 (length items)))
       (should
        (equal '(:class raw-fact
                 :session-id "cont-default-budget"
                 :storage-id 1
                 :kind "user"
                 :content "entry-01")
               (aref items 0)))
       (should
        (equal '(:class raw-fact
                 :session-id "cont-default-budget"
                 :storage-id 51
                 :kind "user"
                 :content "entry-51")
               (aref items 50)))
       (should
        (string-prefix-p
         "[Context v0 | raw cont-default-budget#1-51 | represented none | omitted none | recall none]\n\nUser: entry-01"
         (plist-get envelope :prompt)))
       (should
        (equal [(:session-id "cont-default-budget" :range [1 51])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (= 51 (plist-get (plist-get envelope :receipt)
                                :tape-head)))))))

(ert-deftest view/assemble-oversized-summary-keeps-raw-material ()
  "An oversized model interpretation is skipped without emptying raw context."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((session-id "oversized-summary"))
       (dotimes (index 30)
         (superchat-db-tape-append
          session-id "user" (format "fact-%02d" (1+ index))))
       ;; Keep valid typed provenance while leaving the other raw facts
       ;; available as the fallback material when this summary is too large.
       (superchat-db-tape-append
        session-id "anchor" (make-string 5000 ?x)
        :meta '((anchor_type . "compact")
                (anchor_version . 1)
                (source_ids . [1])))
       (let* ((envelope (superchat-view-assemble
                         session-id :max-chars 100))
              (items (plist-get envelope :items))
              (disclosure (plist-get envelope :disclosure))
              (omitted (plist-get disclosure :omitted)))
         (should (eq 'partial (plist-get envelope :status)))
         (should (> (length items) 0))
         (should (cl-some (lambda (item)
                            (and (eq 'raw-fact (plist-get item :class))
                                 (string= "fact-30"
                                          (plist-get item :content))))
                          (append items nil)))
         (should-not (string-match-p (make-string 100 ?x)
                                     (plist-get envelope :prompt)))
         (should (cl-some (lambda (entry)
                            (eq 'oversized (plist-get entry :reason)))
                          (append omitted nil))))))))

(ert-deftest view/assemble-character-budget-omits-oldest-fact-whole ()
  "A character budget should count rendered lines and never truncate facts."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-chars" "user" "AAAA")
     (superchat-db-tape-append "cont-chars" "assistant" "BBBB")
     (superchat-db-tape-append "cont-chars" "user" "CC")
     (let* ((envelope (superchat-view-assemble
                       "cont-chars" :max-chars 30))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-chars#2-3 | represented none | omitted 1 | recall available]\n\nAssistant: BBBB\nUser: CC"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "cont-chars"
           :storage-id 2
           :kind "assistant"
           :content "BBBB")
          (:class raw-fact
           :session-id "cont-chars"
           :storage-id 3
           :kind "user"
           :content "CC")]
         (plist-get envelope :items)))
       (should
        (equal [(:session-id "cont-chars" :range [2 3])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-chars"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-chars"
                 :selector (:range [1 1]))]
               (plist-get disclosure :recallable)))))))

(ert-deftest view/assemble-default-character-budget-is-finite ()
  "An omitted character budget should still cap the body at 4000 chars."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((large-content (make-string 3990 ?z)))
       (superchat-db-tape-append "cont-default-chars" "user" "x")
       (superchat-db-tape-append
        "cont-default-chars" "user" large-content)
       (let* ((envelope (superchat-view-assemble "cont-default-chars"))
              (items (plist-get envelope :items))
              (disclosure (plist-get envelope :disclosure)))
         (should (eq 'partial (plist-get envelope :status)))
         (should (= 1 (length items)))
         (should (= 2 (plist-get (aref items 0) :storage-id)))
         (should (equal large-content (plist-get (aref items 0) :content)))
         (should
          (equal [(:session-id "cont-default-chars" :range [2 2])]
                 (plist-get disclosure :covered)))
         (should
          (equal [(:session-id "cont-default-chars"
                   :range [1 1]
                   :reason budget
                   :recoverable raw-recall)]
                 (plist-get disclosure :omitted))))))))

(ert-deftest view/assemble-zero-budget-selects-no-material ()
  "A zero budget should disclose all eligible material without crashing."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-zero" "user" "hello")
     (superchat-db-tape-append "cont-zero" "assistant" "world")
     (let* ((envelope (superchat-view-assemble
                       "cont-zero" :max-chars 0))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw none | represented none | omitted 2 | recall available]"
         (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should (equal [] (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-zero"
                 :range [1 2]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-zero"
                 :selector (:range [1 2]))]
               (plist-get disclosure :recallable)))
       (should (= 2 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/assemble-oversized-newest-fact-keeps-older-material ()
  "A newest oversized fact should be disclosed without clearing older facts."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-oversized" "user" "x")
     (superchat-db-tape-append "cont-oversized" "assistant" "BBBB")
     (let* ((envelope (superchat-view-assemble
                       "cont-oversized" :max-chars 10))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-oversized#1 | represented none | omitted 1 | recall available]\n\nUser: x"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "cont-oversized"
           :storage-id 1
           :kind "user"
           :content "x")]
         items))
       (should
        (equal [(:session-id "cont-oversized" :range [1 1])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-oversized"
                 :range [2 2]
                 :reason oversized
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-oversized"
                 :selector (:range [2 2]))]
               (plist-get disclosure :recallable)))))))

(ert-deftest view/assemble-skips-multiple-oversized-facts ()
  "Multiple oversized facts should be skipped before older material is read."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-many-oversized" "user" "x")
     (superchat-db-tape-append
      "cont-many-oversized" "assistant" "123456789")
     (superchat-db-tape-append
      "cont-many-oversized" "user" "987654321")
     (let* ((envelope (superchat-view-assemble
                       "cont-many-oversized" :max-chars 10))
            (disclosure (plist-get envelope :disclosure))
            (omitted (plist-get disclosure :omitted)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-many-oversized#1 | represented none | omitted 2 | recall available]\n\nUser: x"
         (plist-get envelope :prompt)))
       (should (= 1 (length (plist-get envelope :items))))
       (should
        (equal [(:session-id "cont-many-oversized"
                 :range [2 3]
                 :reason oversized
                 :recoverable raw-recall)]
               omitted))
       (should-not
        (cl-some (lambda (range)
                   (eq 'budget (plist-get range :reason)))
                 (append omitted nil)))))))

(ert-deftest view/assemble-fitting-material-still-terminates-at-budget ()
  "Fitting ordinary material should still terminate at the budget boundary."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-budget-boundary" "user" "one")
     (superchat-db-tape-append "cont-budget-boundary" "user" "two")
     (let* ((envelope (superchat-view-assemble
                       "cont-budget-boundary" :max-chars 10))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-budget-boundary#2 | represented none | omitted 1 | recall available]\n\nUser: two"
         (plist-get envelope :prompt)))
       (should
        (equal [(:session-id "cont-budget-boundary"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))))))

(ert-deftest view/assemble-discloses-operational-rows-without-spending-budget ()
  "Operational rows should be accounted for without entering prompt material."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-operational" "user" "hello")
     (superchat-db-tape-append
      "cont-operational" "tool_call" "search-text"
      :meta '((name . "search-text") (args . ((pattern . "needle")))))
     (superchat-db-tape-append "cont-operational" "assistant" "world")
     (let* ((envelope (superchat-view-assemble
                       "cont-operational" :max-entries 2))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-operational#1, cont-operational#3 | represented none | omitted 1 | recall available]\n\nUser: hello\nAssistant: world"
         (plist-get envelope :prompt)))
       (should (= 2 (length (plist-get envelope :items))))
       (should
        (equal [(:session-id "cont-operational" :range [1 1])
                (:session-id "cont-operational" :range [3 3])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-operational"
                 :range [2 2]
                 :reason not-prompt-material
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-operational"
                 :selector (:range [2 2]))]
               (plist-get disclosure :recallable)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/assemble-does-not-render-system-runtime-events-as-dialogue ()
  "A system-kind Runtime envelope should remain operational disclosure."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-system-event" "user" "hello")
     (test-tape-view--append-config-event
      "cont-system-event" "capability.activated"
      '((capability . "agent")))
     (superchat-db-tape-append "cont-system-event" "assistant" "world")
     (let* ((envelope (superchat-view-assemble "cont-system-event"))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should-not (string-match-p "capability.activated"
                                   (plist-get envelope :prompt)))
       (should (= 2 (length (plist-get envelope :items))))
       (should
        (equal [(:session-id "cont-system-event" :range [1 1])
                (:session-id "cont-system-event" :range [3 3])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-system-event"
                 :range [2 2]
                 :reason not-prompt-material
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))))))

(ert-deftest view/assemble-anchor-without-class-schema-stays-out-of-items ()
  "A Compact-shaped Anchor should not be guessed into an evidence class."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-anchor" "user" "deploy Friday")
     (superchat-db-tape-append
      "cont-anchor" "anchor" "SUMMARY: deploy Friday"
      :meta '((source_ids . [1])
              (message_count . 1)
              (timestamp . "2026-08-09T10:00:00+0800")))
     (superchat-db-tape-append "cont-anchor" "assistant" "noted")
     (let* ((envelope (superchat-view-assemble "cont-anchor"))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw cont-anchor#1, cont-anchor#3 | represented none | omitted 1 | recall available]\n\nUser: deploy Friday\nAssistant: noted"
         (plist-get envelope :prompt)))
       (should (= 2 (length items)))
       (should-not
        (cl-find 'model-interpretation items
                 :key (lambda (item) (plist-get item :class))))
       (should-not (string-match-p "SUMMARY" (plist-get envelope :prompt)))
       (should (equal [] (plist-get disclosure :represented)))
       (should
        (equal [(:session-id "cont-anchor" :range [1 1])
                (:session-id "cont-anchor" :range [3 3])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-anchor"
                 :range [2 2]
                 :reason unsupported-schema
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-anchor"
                 :selector (:range [2 2]))]
               (plist-get disclosure :recallable)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/assemble-branch-keeps-parent-cutoff-and-source-session-ids ()
  "A branch View should include only its parent cutoff and its own rows."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((root-user
             (superchat-db-tape-append "root" "user" "question"))
            (root-answer
             (superchat-db-tape-append "root" "assistant" "answer")))
       (superchat-db-tape-append "root" "user" "later parent turn")
       (superchat-db-tape-append
        "branch" "anchor" "Branched from root"
        :topic "branch"
        :meta `((parent_session_id . "root")
                (parent_turn_id . ,root-answer)
                (source_ids . [,root-user ,root-answer])))
       (superchat-db-tape-append "branch" "user" "branch question")
       (superchat-db-tape-append
        "sibling" "anchor" "Sibling branch"
        :topic "branch"
        :meta `((parent_session_id . "root")
                (parent_turn_id . ,root-answer)
                (source_ids . [,root-user ,root-answer])))
       (superchat-db-tape-append "sibling" "user" "sibling question")
       (let* ((envelope (superchat-view-assemble "branch"))
              (scope (plist-get envelope :scope))
              (disclosure (plist-get envelope :disclosure)))
         (should (eq 'satisfied (plist-get envelope :status)))
         (should
          (equal
           "[Context v0 | raw root#1-2, branch#5 | represented none | omitted 1 | recall available]\n\nUser: question\nAssistant: answer\nUser: branch question"
           (plist-get envelope :prompt)))
         (should
          (equal
           [(:class raw-fact
             :session-id "root"
             :storage-id 1
             :kind "user"
             :content "question")
            (:class raw-fact
             :session-id "root"
             :storage-id 2
             :kind "assistant"
             :content "answer")
            (:class raw-fact
             :session-id "branch"
             :storage-id 5
             :kind "user"
             :content "branch question")]
           (plist-get envelope :items)))
         (should
          (equal [(:session-id "root" :through-storage-id 2)
                  (:session-id "branch" :through-storage-id 5)]
                 (plist-get scope :lineage)))
         (should
          (equal [(:session-id "root" :range [1 2])
                  (:session-id "branch" :range [5 5])]
                 (plist-get disclosure :covered)))
         (should
          (equal [(:session-id "branch"
                   :range [4 4]
                   :reason not-prompt-material
                   :recoverable raw-recall)]
                 (plist-get disclosure :omitted)))
         (should-not (string-match-p
                      "later parent turn\\|sibling question"
                      (plist-get envelope :prompt)))
         (should (= 5 (plist-get (plist-get envelope :receipt)
                                 :tape-head))))))))

(ert-deftest view/assemble-rejects-reserved-selectors-instead-of-ignoring-them ()
  "Unimplemented selectors must fail loudly rather than claim full coverage."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "reserved-selectors" "user" "one")
     (dolist (arguments '((:after-storage-id 1)
                          (:through-storage-id 1)
                          (:pinned-storage-ids [1])))
       (should-error
        (apply #'superchat-view-assemble "reserved-selectors" arguments))))))

(ert-deftest view/assemble-incomplete-handoffs-receive-no-handoff-semantics ()
  "Every incomplete Handoff shape stays an unsupported Anchor in direction 1."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((complete
             '((schema . "context-handoff-v0")
               (task_id . "task-1")
               (from_phase . "research")
               (to_phase . "implementation")
               (state . ((goal . "preserve evidence")
                         (constraints . ["read-only"])
                         (decisions . ["use Tape"])
                         (open_items . ["review"])
                         (acceptance . ["tests pass"])))
               (source_ids . [99])
               (parent_handoff_id . nil)
               (narrative . nil)
               (lossy_fields . [])))
            (top-level-keys
             '(schema task_id from_phase to_phase state source_ids
                      parent_handoff_id narrative lossy_fields))
            (state-keys
             '(goal constraints decisions open_items acceptance)))
       (dolist (key top-level-keys)
         (let ((payload (copy-tree complete)))
           (setq payload (assq-delete-all key payload))
           (superchat-db-tape-append
            "handoff-incomplete" "anchor" "candidate handoff"
            :topic "handoff" :meta `((payload . ,payload)))))
       (dolist (key state-keys)
         (let* ((payload (copy-tree complete))
                (state (assq 'state payload)))
           (setcdr state (assq-delete-all key (cdr state)))
           (superchat-db-tape-append
            "handoff-incomplete" "anchor" "candidate handoff"
            :topic "handoff" :meta `((payload . ,payload)))))
       (let* ((envelope (superchat-view-assemble "handoff-incomplete"))
              (disclosure (plist-get envelope :disclosure))
              (omitted (plist-get disclosure :omitted)))
         (should (eq 'partial (plist-get envelope :status)))
         (should (equal [] (plist-get envelope :items)))
         (should (equal [] (plist-get disclosure :represented)))
         (should (= 1 (length omitted)))
         (should
          (equal '(:session-id "handoff-incomplete"
                   :range [1 14]
                   :reason unsupported-schema
                   :recoverable raw-recall)
                 (aref omitted 0))))))))

(ert-deftest test-tape-append-accepts-topic ()
  "`superchat-db-tape-append' should accept a topic argument."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((id (superchat-db-tape-append "s1" "user" "hello" :topic "intro")))
       (should (integerp id))
       (let ((rows (superchat-db-tape-select
                    "SELECT topic, kind, content FROM tape WHERE id = ?"
                    (list id))))
         (should (= (length rows) 1))
         (should (string= "intro" (caar rows)))
         (should (string= "user" (cadar rows)))
         (should (string= "hello" (caddar rows))))))))

(ert-deftest test-tape-fts5-search ()
  "FTS5 search should find entries by content."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "incremental indexing with FTS5")
     (superchat-db-tape-append "s2" "user" "something unrelated")
     (let ((rows (superchat-db-tape-search "FTS5" nil 10)))
       (should (= (length rows) 1))
       (should (string-match-p "FTS5" (nth 4 (car rows))))))))

(ert-deftest test-tape-fts5-search-accepts-apostrophes ()
  "FTS5 tape searches must not turn an apostrophe into SQL syntax."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "user's note about FTS5")
     (let ((rows (superchat-db-tape-search "user's" nil 10)))
       (should (= (length rows) 1))
       (should (string-match-p "user's" (nth 4 (car rows))))))))

(ert-deftest test-tape-v2-to-v4-migration ()
  "Opening a schema-v2 database must migrate through v4."
  (let* ((tmp-dir (make-temp-file "superchat-tape-migration-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (db (sqlite-open db-file)))
    (unwind-protect
        (progn
          ;; Manufacture a v2 database: no topic column, no tape_fts.
          (sqlite-execute
           db
           "CREATE TABLE tape (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              session_id TEXT NOT NULL,
              kind TEXT NOT NULL,
              content TEXT NOT NULL,
              meta TEXT DEFAULT '{}',
              created_at TEXT NOT NULL DEFAULT (datetime('now')))")
          (sqlite-execute
           db
           "CREATE TABLE memory (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              title TEXT, content TEXT NOT NULL,
              keywords TEXT DEFAULT '', source_tape_ids TEXT DEFAULT '[]',
              mood TEXT DEFAULT '', replaced_by INTEGER REFERENCES memory(id),
              review_status TEXT DEFAULT 'pending',
              created_at TEXT NOT NULL DEFAULT (datetime('now')))")
          (sqlite-execute
           db
           "CREATE VIRTUAL TABLE memory_fts USING fts5(
              content, keywords, title,
              content=memory, content_rowid=id,
              tokenize='trigram')")
          (sqlite-execute
           db
           "CREATE TABLE _schema_version (version INTEGER PRIMARY KEY)")
          (sqlite-execute
           db
           "INSERT INTO _schema_version (version) VALUES (2)")
          (sqlite-execute
           db
           "INSERT INTO tape (session_id, kind, content) VALUES ('migration-test', 'user', 'pre-v3 content')")
          (sqlite-close db)
          ;; Now open with superchat-db and verify migration.
          (setq db (superchat-db-open db-file))
          (should (= 4 (superchat-db--current-schema-version db)))
          (should (cl-some (lambda (col) (string= "topic" (cadr col)))
                           (sqlite-select db "PRAGMA table_info(tape)")))
          (should (cl-some
                   (lambda (col) (string= "source_event_ids" (cadr col)))
                   (sqlite-select db "PRAGMA table_info(memory)")))
          (should (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master WHERE type='table' AND name='tape_fts'"))
          (should (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master
                    WHERE type='table' AND name='projection_checkpoint'"))
          (let ((rows (superchat-db-tape-search "content" nil 10)))
            (should (= 1 (length rows)))
            (should (string-match-p "pre-v3 content" (nth 4 (car rows))))))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest test-tape-fts5-search-scoped-to-session ()
  "FTS5 search can be scoped to a session."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "hello world")
     (superchat-db-tape-append "s2" "user" "hello again")
     (let ((rows (superchat-view-search "hello" "s1" 10)))
       (should (= (length rows) 1))
       (should (string= "s1" (nth 1 (car rows))))))))

(ert-deftest test-tape-fts5-search-matches-shared-words-across-full-sentence ()
  "A full-sentence query matches differently-worded content sharing words."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "[remember] the cat is named JOURNEY_TOM")
     (superchat-db-tape-append "s1" "anchor" "the cat is named JOURNEY_TOM")
     (superchat-db-tape-append "s2" "user" "unrelated filler about dogs")
     (let ((rows (superchat-db-tape-search
                  "what is the cat name we discussed?" nil 10)))
       (should (= (length rows) 2))
       (should (cl-every (lambda (row) (string-match-p "JOURNEY_TOM" (nth 4 row)))
                         rows))))))

(ert-deftest test-tape-fts5-search-tolerates-reserved-fts5-syntax-characters ()
  "A query containing FTS5 syntax characters never signals an error."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "a plain note with no special words")
     (should
      (listp (superchat-db-tape-search
              "\"weird\" * (query) 'quote' NEAR OR AND" nil 10))))))

(ert-deftest test-tape-fts5-search-pure-punctuation-query-returns-nil ()
  "A query with no word characters returns nil without querying FTS5."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "some content")
     (should-not (superchat-db-tape-search "???!!!" nil 10)))))

(ert-deftest test-tape-fts5-search-ranks-relevance-over-recency ()
  "A highly relevant older row outranks a barely-matching newer one."
  (test-tape-view--with-temp-db
   (lambda ()
     (let (old-id new-id)
       (setq old-id (superchat-db-tape-append
                     "s1" "user" "old note mentioning cat and dog together"))
       (dotimes (i 3)
         (superchat-db-tape-append "s1" "user" (format "filler turn %d" i)))
       (setq new-id (superchat-db-tape-append "s1" "user" "brand new cat mention"))
       (let ((rows (superchat-db-tape-search
                    "old note mentioning cat and dog" "s1" 10)))
         (should (equal (list old-id new-id) (mapcar #'car rows))))))))

(ert-deftest test-tape-select-read-only-guard ()
  "`superchat-db-tape-select' should reject mutating statements."
  (test-tape-view--with-temp-db
   (lambda ()
     (should-error (superchat-db-tape-select "DELETE FROM tape"))
     (should-error (superchat-db-tape-select "INSERT INTO tape VALUES (1,2,3,4,5,6)"))
     (should-error (superchat-db-tape-select "UPDATE tape SET content = 'x'")))))

(ert-deftest test-view-file-history ()
  "`superchat-view-file-history' should find tool entries mentioning a path."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append
      "s1" "tool_call" "write-file"
      :meta '((name . "write-file") (args . ((path . "/tmp/init.el")))))
     (superchat-db-tape-append
      "s1" "tool_result" "wrote /tmp/init.el")
     (superchat-db-tape-append
      "s1" "tool_call" "read-file"
      :meta '((name . "read-file") (args . ((path . "/tmp/other.el")))))
     (let ((rows (superchat-view-file-history "/tmp/init.el" "s1" 10)))
       (should (= (length rows) 2))))))

(ert-deftest test-view-tool-history ()
  "`superchat-view-tool-history' should find entries for a specific tool."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append
      "s1" "tool_call" "shell-command"
      :meta '((name . "shell-command") (args . ((command . "ls")))))
     (superchat-db-tape-append
      "s1" "tool_result" "file1 file2")
     (superchat-db-tape-append
      "s1" "tool_call" "search-text"
      :meta '((name . "search-text") (args . ((pattern . "foo")))))
     (let ((rows (superchat-view-tool-history "shell-command" "s1" 10)))
       ;; tool_result does not carry the tool name in meta, so only
       ;; the tool_call row matches directly.
       (should (= (length rows) 1))))))

(ert-deftest test-view-recent-errors ()
  "`superchat-view-recent-errors' should find error-like tool results."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "tool_result" "Error: file not found")
     (superchat-db-tape-append "s1" "tool_result" "Success")
     (superchat-db-tape-append "s1" "tool_result" "Traceback: some error")
     (let ((rows (superchat-view-recent-errors "s1" 10)))
       (should (= (length rows) 2))))))

(ert-deftest test-view-expand-anchor ()
  "`superchat-view-expand-anchor' should return source entries."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((id1 (superchat-db-tape-append "s1" "user" "msg1"))
            (id2 (superchat-db-tape-append "s1" "assistant" "msg2"))
            (anchor-id (superchat-db-tape-append
                        "s1" "anchor" "summary"
                        :meta `((anchor_type . "compact")
                                (anchor_version . 1)
                                (source_ids . [,id1 ,id2]))))
            (anchor (superchat-view-anchor-latest "s1"))
            (rows (superchat-view-expand-anchor "s1" anchor)))
       (should (= (length rows) 2))
       (should (string= "msg1" (nth 3 (car rows))))
       (should (string= "msg2" (nth 3 (cadr rows))))))))

(ert-deftest view/session-history-follows-branch-lineage ()
  "A branch view should inherit parent history only through its branch point."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((root-user (superchat-db-tape-append "root" "user" "question"))
            (root-answer (superchat-db-tape-append
                          "root" "assistant" "first answer")))
       (superchat-db-tape-append "root" "user" "later parent turn")
       (superchat-db-tape-append
        "branch" "anchor" "Branched from root"
        :topic "branch"
        :meta `((parent_session_id . "root")
                (parent_turn_id . ,root-answer)
                (source_ids . [,root-user ,root-answer])))
       (superchat-db-tape-append "branch" "user" "branch question")
       (let* ((rows (superchat-view-session-history "branch"))
              (contents (mapcar (lambda (row) (nth 3 row)) rows)))
         (should (equal contents
                        '("question" "first answer"
                          "Branched from root" "branch question")))
         (should-not (member "later parent turn" contents)))))))

(ert-deftest view/session-configuration-defaults ()
  "A Session with no config events should use chat defaults."
  (test-tape-view--with-temp-db
   (lambda ()
     (should (equal (superchat-view-session-configuration "empty")
                    '(:capability "chat"
                      :capability-source nil
                      :topology "single"
                      :authority-policy "default"
                      :workspace-id nil
                      :workspace-root nil
                      :surface nil))))))

(ert-deftest view/session-configuration-replays-latest-events ()
  "Configuration replay should project the latest event envelopes."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "config" "capability.activated"
      '((capability . "agent") (capability_source . "preset")))
     (test-tape-view--append-config-event
      "config" "topology.changed" '((topology . "team")))
     (test-tape-view--append-config-event
      "config" "authority.policy.selected" '((policy . "confirm-risky")))
     (test-tape-view--append-config-event
      "config" "workspace.bound"
      '((workspace_id . "workspace-1") (workspace_root . "/repo")))
     (test-tape-view--append-config-event
      "config" "intent.received" '((surface . "magit")))
     (should (equal (superchat-view-session-configuration "config")
                    '(:capability "agent"
                      :capability-source "preset"
                      :topology "team"
                      :authority-policy "confirm-risky"
                      :workspace-id "workspace-1"
                      :workspace-root "/repo"
                      :surface "magit")))
     (test-tape-view--append-config-event
      "config" "workspace.unbound" nil)
     (test-tape-view--append-config-event
      "config" "intent.received" '((content_size . 12)))
     (should (equal (superchat-view-session-configuration "config")
                    '(:capability "agent"
                      :capability-source "preset"
                      :topology "team"
                      :authority-policy "confirm-risky"
                      :workspace-id nil
                      :workspace-root nil
                      :surface "magit"))))))

(ert-deftest view/session-configuration-projects-and-clears-profile ()
  "Capability changes project Profile identity without retaining stale data."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "profile-config" "capability.activated"
      '((name . "coding")
        (profile_id . "coder-safe")
        (source . "skill")))
     (let ((config
            (superchat-view-session-configuration "profile-config")))
       (should (equal "coding" (plist-get config :capability)))
       (should (equal "coder-safe" (plist-get config :profile-id))))
     (test-tape-view--append-config-event
      "profile-config" "capability.activated" '((name . "chat")))
     (should-not
      (plist-member
       (superchat-view-session-configuration "profile-config")
       :profile-id)))))

(ert-deftest view/session-configuration-inherits-branch-history ()
  "Branch Sessions should inherit config through their branch point."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((capability-id
            (test-tape-view--append-config-event
             "root-config" "capability.activated"
             '((capability . "agent") (source . "parent")))))
       (test-tape-view--append-config-event
        "root-config" "capability.activated" '((capability . "chat")))
       (superchat-db-tape-append
        "child-config" "anchor" "Branched from root-config"
        :topic "branch"
        :meta `((parent_session_id . "root-config")
                (parent_turn_id . ,capability-id)))
       (test-tape-view--append-config-event
        "child-config" "topology.changed" '((topology . "team")))
       (should (equal (superchat-view-session-configuration "child-config")
                      '(:capability "agent"
                        :capability-source "parent"
                        :topology "team"
                        :authority-policy "default"
                        :workspace-id nil
                        :workspace-root nil
                        :surface nil)))))))

(ert-deftest view/session-configuration-ignores-malformed-meta ()
  "Malformed or legacy meta should not affect projected config."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "malformed" "capability.activated" '((capability . "agent")))
     (sqlite-execute
      (superchat-db-open)
      "INSERT INTO tape(session_id, kind, content, meta) VALUES (?, ?, ?, ?)"
      (list "malformed" "system" "bad json" "{bad json"))
     (superchat-db-tape-append
      "malformed" "system" "bad payload"
      :meta '((event_type . "capability.activated")
              (payload . "not an object")))
     (superchat-db-tape-append
      "malformed" "system" "legacy meta" :meta '((capability . "chat")))
     (should (equal (superchat-view-session-configuration "malformed")
                    '(:capability "agent"
                      :capability-source nil
                      :topology "single"
                      :authority-policy "default"
                      :workspace-id nil
                      :workspace-root nil
                      :surface nil))))))

(ert-deftest view/session-ids-are-most-recent-first ()
  "Session discovery should be derived from Tape activity."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "older" "user" "one")
     (superchat-db-tape-append "newer" "user" "two")
     (should (equal (superchat-view-session-ids) '("newer" "older"))))))

(ert-deftest view/open-session-replays-into-independent-buffer ()
  "Opening a Session should reconstruct a usable conversation View."
  (require 'superchat)
  (let ((workspace (make-temp-file "superchat-replay-workspace" t)))
    (unwind-protect
        (test-tape-view--with-temp-db
         (lambda ()
           (test-tape-view--append-config-event
            "replay-session" "capability.activated"
            '((name . "code") (source . "builtin")))
           (test-tape-view--append-config-event
            "replay-session" "topology.changed" '((topology . "team")))
           (test-tape-view--append-config-event
            "replay-session" "authority.policy.selected"
            '((policy . "observer")))
           (test-tape-view--append-config-event
            "replay-session" "workspace.bound"
            `((workspace_id . "workspace-replay")
              (workspace_root . ,workspace)))
           (superchat-db-tape-append "replay-session" "user" "hello")
           (superchat-db-tape-append "replay-session" "assistant" "world")
           (let (reconciled)
             (cl-letf (((symbol-function 'superchat-runtime-reconcile-session)
                        (lambda (session-id)
                          (setq reconciled session-id))))
               (let ((buffer (superchat-open-session "replay-session")))
                 (unwind-protect
                     (with-current-buffer buffer
                       (should (equal reconciled "replay-session"))
                       (should-not (equal (buffer-name) superchat-buffer-name))
                       (should (equal superchat--session-id "replay-session"))
                       (should (equal
                                "code"
                                (superchat-preset-name
                                 superchat--active-preset)))
                       (should (eq 'team superchat--active-topology))
                       (should (equal "observer"
                                      superchat--active-authority-policy))
                       (should (equal "workspace-replay"
                                      (plist-get superchat--workspace-binding
                                                 :id)))
                       (should (= (length superchat--conversation-history) 2))
                       (should (string-match-p "User: hello" (buffer-string)))
                       (should (string-match-p "Assistant" (buffer-string)))
                       (should (marker-position superchat--prompt-start))
                       (goto-char (point-max))
                       (insert "next question")
                       (should (string-suffix-p
                                "next question" (buffer-string))))
                   (when (buffer-live-p buffer)
                     (kill-buffer buffer))))))))
      (delete-directory workspace t))))

(ert-deftest view/reconcile-failure-does-not-block-session-open ()
  "A reconciliation error is contained by the Session View call site."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "reconcile-failure" "user" "hello")
     (cl-letf (((symbol-function 'superchat-runtime-reconcile-session)
                (lambda (_session-id) (error "reconcile failed"))))
       (let ((buffer (superchat-open-session "reconcile-failure")))
         (unwind-protect
             (with-current-buffer buffer
               (should (equal superchat--session-id "reconcile-failure"))
               (should (string-match-p "User: hello" (buffer-string))))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(ert-deftest view/image-attachment-persists-as-metadata ()
  "Tape stores image metadata and reopened Views render the attachment."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((image (make-temp-file "superchat-tape-image-" nil ".jpg"))
           buffer)
       (unwind-protect
           (progn
             (with-temp-file image (insert "JPEG"))
             (let ((turn (superchat-turn--create
                          :id "media-turn"
                          :session-id "media-session"
                          :inbound "Describe image"
                          :context-files (list image))))
               (superchat--hook-record-attachments turn))
             (let ((row (car (superchat-db-tape-replay "media-session"))))
               (should (equal (nth 1 row) "attachment"))
               (should (equal (nth 2 row) "system"))
               (should (string-match-p "image/jpeg, 4 bytes" (nth 3 row)))
               (let ((json-key-type 'symbol))
                 (let* ((meta (json-read-from-string (nth 4 row)))
                        (payload (cdr (assq 'payload meta))))
                   (should (equal "attachment.added"
                                  (cdr (assq 'event_type meta))))
                   (should (string-prefix-p
                            "evt_" (cdr (assq 'event_id meta))))
                   (should (equal (cdr (assq 'path payload)) image))
                   (should-not (assq 'data payload)))))
             (setq buffer (superchat-open-session "media-session"))
             (with-current-buffer buffer
               (should (string-match-p "Attachment" (buffer-string)))
               (should (string-match-p "image/jpeg, 4 bytes"
                                       (buffer-string)))))
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (delete-file image))))))

(ert-deftest view/new-sessions-own-independent-buffers ()
  "New Sessions should not share their Buffer or buffer-local state."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((ids '("new-session-a" "new-session-b"))
           (superchat-display-single-window nil)
           first second)
       (cl-letf (((symbol-function 'superchat--new-session-id)
                  (lambda () (pop ids))))
         (setq first (superchat-new-session)
               second (superchat-new-session)))
       (unwind-protect
           (progn
             (should-not (eq first second))
             (with-current-buffer first
               (should (equal superchat--session-id "new-session-a"))
               (setq superchat--conversation-history '((:role "user"))))
             (with-current-buffer second
               (should (equal superchat--session-id "new-session-b"))
               (should-not superchat--conversation-history)))
         (when (buffer-live-p first)
           (kill-buffer first))
         (when (buffer-live-p second)
           (kill-buffer second)))))))

(ert-deftest view/branch-session-persists-lineage-and-opens-child ()
  "Branching should persist one lineage anchor and open the child View."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "branch-root" "user" "question")
     (let* ((answer-id (superchat-db-tape-append
                        "branch-root" "assistant" "answer"))
            (parent-buffer (superchat-open-session "branch-root"))
            child-buffer)
       (unwind-protect
           (progn
             (setq child-buffer
                   (with-current-buffer parent-buffer
                     (superchat-branch-session answer-id)))
             (with-current-buffer child-buffer
               (should-not (equal superchat--session-id "branch-root"))
               (should (equal (mapcar (lambda (row) (nth 3 row))
                                      (superchat-view-session-history
                                       superchat--session-id))
                              '("question" "answer"
                                "Branched from branch-root")))
               (let* ((row
                       (car
                        (superchat-db-tape-replay superchat--session-id)))
                      (json-key-type 'symbol)
                      (meta (json-read-from-string (nth 4 row))))
                 (should (equal "anchor.created"
                                (cdr (assq 'event_type meta))))
                 (should (string-prefix-p
                          "evt_" (cdr (assq 'event_id meta)))))
               (should (string-match-p "User: question" (buffer-string)))
               (should (marker-position superchat--prompt-start))))
         (when (buffer-live-p parent-buffer)
           (kill-buffer parent-buffer))
         (when (buffer-live-p child-buffer)
           (kill-buffer child-buffer)))))))

;; ═══════════════════════════════════════════════════════════
;; Slash-command integration
;; ═══════════════════════════════════════════════════════════

(ert-deftest test-cmd-remember-writes-tape-anchor ()
  "`/remember' should write both a user entry and an anchor to tape."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-test")
           (superchat--pending-recalled-memories nil)
           (legacy-calls nil))
       (cl-letf (((symbol-function 'superchat-memory-capture-explicit)
                  (lambda (content &optional title &rest options)
                    (push (list content title options) legacy-calls))))
         (let ((result (superchat--cmd-remember
                        "remember" "always use lexical-binding" nil nil nil)))
           (should (string-match-p "Anchor added" (plist-get result :content)))
           ;; Tape should have a user entry and an anchor.
           (let ((rows (superchat-db-tape-select
                        "SELECT kind, content FROM tape WHERE session_id = ? ORDER BY id ASC"
                        (list "remember-test"))))
             (should (= (length rows) 2))
             (should (string= "user" (caar rows)))
             (should (string-match-p "always use lexical-binding" (cadar rows)))
             (should (string= "anchor" (caadr rows)))
             (should (string= "always use lexical-binding" (cadadr rows))))
           ;; Legacy memory should still be called for backward compat.
           (should (= (length legacy-calls) 1))
           (let ((options (nth 2 (car legacy-calls))))
             (should (equal "remember-test"
                            (plist-get options :remember-session-id)))
             (should (= 2 (plist-get options :remember-anchor-id)))
             (should (= 1 (plist-get options :remember-source-id))))))))))

(ert-deftest test-cmd-recall-searches-tape-first ()
  "`/recall' should set pending memories from tape search results."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "recall-test")
           (superchat--pending-recalled-memories nil))
       (superchat-db-tape-append "recall-test" "user" "incremental indexing")
       (superchat-db-tape-append "recall-test" "assistant" "use trigram tokenizer")
       (superchat--cmd-recall "recall" "trigram" nil nil nil)
       (should (> (length superchat--pending-recalled-memories) 0))
       (should (cl-some (lambda (m)
                          (string-match-p "trigram" (plist-get m :content)))
                        superchat--pending-recalled-memories))))))

(ert-deftest remember-recall/provenance-removes-only-selected-bridge-memory ()
  "A selected remember Anchor prevents its exact bridge Memory duplicate."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-provenance")
           (superchat--pending-recalled-memories nil)
           (superchat-memory-legacy-writes-enabled t))
       (superchat--cmd-remember
        "remember" "provenance needle" nil nil nil)
       (let* ((rows (superchat-view-search
                     "provenance needle" "remember-provenance" 20))
              (source (cl-find "user" rows :key (lambda (row) (nth 3 row))
                               :test #'equal))
              (anchor (cl-find "anchor" rows :key (lambda (row) (nth 3 row))
                               :test #'equal)))
         (should source)
         (should anchor)
         (superchat--cmd-recall
          "recall" "provenance needle" nil nil nil)
         (should (= 2 (length superchat--pending-recalled-memories)))
         (should (cl-find (car source) superchat--pending-recalled-memories
                          :key (lambda (item) (plist-get item :id))))
         (should (cl-find (car anchor) superchat--pending-recalled-memories
                          :key (lambda (item) (plist-get item :id))))
         (should-not
          (cl-find "memory" superchat--pending-recalled-memories
                   :key (lambda (item) (plist-get item :type))
                   :test #'equal))
         (let ((turn (superchat-turn-new "next question" "remember-provenance")))
           (superchat--attach-turn-memories
            turn "next question" "remember-provenance" t)
           (should-not superchat--pending-recalled-memories)
           (should (= 2 (length (superchat-turn-retrieved-memories turn))))
           (let ((memory (plist-get
                          (car (superchat-prompt-collector--memory
                                (list :surface 'chat
                                      :context (list :chat-turn turn))
                                nil nil))
                          :text)))
             (should (string-match-p "\\[remember\\] provenance needle" memory))
             (should (string-match-p "\\* anchor (ID:" memory)))))))))

(ert-deftest remember-recall/provenance-keeps-bridge-when-anchor-not-selected ()
  "A valid bridge remains when the exact Anchor is outside Tape search rows."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-anchor-not-selected")
           (superchat--pending-recalled-memories nil)
           (superchat-memory-legacy-writes-enabled t))
       (superchat--cmd-remember "remember" "unselected needle" nil nil nil)
       (sqlite-execute
        (superchat-db-open)
        "UPDATE tape SET content = 'hidden anchor' WHERE session_id = ? AND kind = 'anchor'"
        '("remember-anchor-not-selected"))
       (superchat--cmd-recall "recall" "unselected needle" nil nil nil)
       (should (= 2 (length superchat--pending-recalled-memories)))
       (should (equal '("tape" "memory")
                      (mapcar (lambda (item) (plist-get item :type))
                              superchat--pending-recalled-memories)))))))

(ert-deftest remember-recall/query-limit-filters-without-refill ()
  "Limits precede provenance filtering, which never refills Memory results.
This is a same-session limit/provenance property that predates S23's
cross-session recall default, so the second half below binds
`superchat-memory-recall-scope' to `session' to keep testing that
property in isolation rather than S23's separate all-sessions behavior."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-limit")
           (superchat--pending-recalled-memories nil)
           (calls 0)
           (original (symbol-function 'superchat-memory-retrieve)))
       (dotimes (_ 20)
         (superchat-db-tape-append "remember-limit" "user" "limit needle"))
       (superchat-memory-capture-explicit "limit needle" "Global")
       (cl-letf (((symbol-function 'superchat-memory-retrieve)
                  (lambda (query)
                    (cl-incf calls)
                    (funcall original query))))
         (superchat--cmd-recall "recall" "limit needle" nil nil nil)
         (should (= 20 (length superchat--pending-recalled-memories)))
         (should (= 0 calls)))
       (sqlite-execute (superchat-db-open)
                       "DELETE FROM tape WHERE id = (SELECT MAX(id) FROM tape WHERE kind = 'user')")
       (setq superchat--pending-recalled-memories nil)
       (cl-letf (((symbol-function 'superchat-memory-retrieve)
                  (lambda (query)
                    (cl-incf calls)
                    (funcall original query))))
         (superchat--cmd-recall "recall" "limit needle" nil nil nil)
         (should (= 20 (length superchat--pending-recalled-memories)))
         (should (= 1 calls)))
       ;; Make filtering observable: the real query returns a valid bridge
       ;; followed by an ordinary Memory, but the selected exact Anchor removes
       ;; only that bridge.  A refill would require a second retrieve call.
       (let* ((superchat--session-id "remember-filter-no-refill")
              (superchat-memory-recall-scope 'session)
              (query "filter provenance needle")
              (ordinary-id
               (superchat-memory-capture-explicit query "Ordinary fallback"))
              (_remember
               (superchat--cmd-remember "remember" query nil nil nil))
              (selected-rows
               (superchat-view-search query superchat--session-id 20))
              (selected-identities
               (mapcar (lambda (row) (list (car row) (nth 3 row) (nth 4 row)))
                       selected-rows))
              (filtered-calls 0)
              (queried-memories nil))
         (should (< (length selected-rows) 20))
         (should (equal '("anchor" "user")
                        (mapcar (lambda (row) (nth 3 row)) selected-rows)))
         (setq superchat--pending-recalled-memories nil)
         (cl-letf (((symbol-function 'superchat-memory-retrieve)
                    (lambda (actual-query)
                      (cl-incf filtered-calls)
                      (setq queried-memories
                            (funcall original actual-query)))))
           (superchat--cmd-recall "recall" query nil nil nil))
         (should (= 1 filtered-calls))
         (should (= 2 (length queried-memories)))
         (should (equal (list query query)
                        (mapcar (lambda (item) (plist-get item :content))
                                queried-memories)))
         (should (equal ordinary-id (plist-get (cadr queried-memories) :id)))
         (let* ((bridge (car queried-memories))
                (final superchat--pending-recalled-memories))
           (should-not (= ordinary-id (plist-get bridge :id)))
           (should (= 3 (length final)))
           (should (equal selected-identities
                          (mapcar
                           (lambda (item)
                             (list (plist-get item :id)
                                   (plist-get item :title)
                                   (plist-get item :content)))
                           (seq-take final 2))))
           (should (equal ordinary-id (plist-get (nth 2 final) :id)))
           (should (equal query (plist-get (nth 2 final) :content)))
           (should-not
            (cl-find (plist-get bridge :id) final
                     :key (lambda (item) (plist-get item :id))))))))))

(ert-deftest remember-recall/partial-write-contracts-remain-append-only ()
  "Nil/error Anchor and Memory failure retain the established partial writes."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-partial")
           (superchat-memory-legacy-writes-enabled t))
       (cl-letf (((symbol-function 'superchat-memory-capture-explicit)
                  (lambda (&rest _) (error "memory failed"))))
         (should-error
          (superchat--cmd-remember "remember" "partial needle" nil nil nil)))
       (should (= 2 (length (superchat-db-tape-replay "remember-partial"))))
       (let ((calls 0))
         (cl-letf (((symbol-function 'superchat-tape-append-material)
                    (lambda (&rest _)
                      (cl-incf calls)
                      (if (= calls 1) 10 nil))))
           (superchat--cmd-remember "remember" "nil anchor" nil nil nil))
         (let* ((row (car (superchat-db-tape-select
                           "SELECT meta FROM tape WHERE topic = 'memory' ORDER BY id DESC LIMIT 1")))
                (meta (let ((json-object-type 'alist)
                            (json-key-type 'symbol))
                        (json-read-from-string (car row)))))
           (should-not
            (alist-get 'remember_bridge (alist-get 'payload meta)))))
       (let ((capture-called nil)
             (calls 0))
         (cl-letf (((symbol-function 'superchat-tape-append-material)
                    (lambda (&rest _)
                      (cl-incf calls)
                      (if (= calls 1) 11 (error "anchor failed"))))
                   ((symbol-function 'superchat-memory-capture-explicit)
                    (lambda (&rest _) (setq capture-called t))))
           (should-error
            (superchat--cmd-remember "remember" "anchor error" nil nil nil)))
         (should-not capture-called))))))

(ert-deftest remember-recall/bridge-disabled-and-no-argument-remain-compatible ()
  "Disabled bridge stays Tape-only; empty remember stays Memory-only."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-compat")
           (superchat-memory-legacy-writes-enabled nil))
       (superchat--cmd-remember "remember" "tape only" nil nil nil)
       (should (= 2 (length (superchat-db-tape-replay "remember-compat"))))
       (should (= 0 (superchat-db-memory-count)))
       (cl-letf (((symbol-function 'superchat--last-exchange-struct)
                  (lambda () '(:input "memory only exchange"))))
         (superchat--cmd-remember "remember" "" nil nil nil))
       (should (= 3 (length (superchat-db-tape-replay "remember-compat"))))
       (should (= 2 (caar (superchat-db-tape-select
                           "SELECT COUNT(*) FROM tape
                            WHERE session_id = ? AND kind IN ('user', 'anchor')"
                           '("remember-compat")))))
       (should (= 1 (superchat-db-memory-count "accepted")))))))

;; ═══════════════════════════════════════════════════════════
;; Tape tool integration
;; ═══════════════════════════════════════════════════════════

(ert-deftest test-tool-sql-read-only ()
  "`superchat-tool-sql' should return JSON and reject mutating statements."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "hello world")
     (let ((result (superchat-tool-sql "SELECT kind, content FROM tape")))
       (should (string-match-p "\"user\"" result))
       (should (string-match-p "hello world" result)))
     (should-error (superchat-tool-sql "DELETE FROM tape"))
     (should-error (superchat-tool-sql "INSERT INTO tape VALUES (1,2,3,4,5,6)")))))

(ert-deftest test-tool-memory-search ()
  "`superchat-tool-memory-search' should return tape matches as JSON."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "tool-search-session"))
       (superchat-db-tape-append "tool-search-session" "user" "incremental indexing")
       (superchat-db-tape-append "tool-search-session" "assistant" "use trigram tokenizer")
       (let ((result (superchat-tool-memory-search "trigram")))
         (should (string-match-p "trigram" result))
         (should (string-match-p "tool-search-session" result)))))))

(ert-deftest test-tool-memory-search-follows-cross-session-policy ()
  "`superchat-tool-memory-search' is model-initiated, like automatic
recall, so it follows `superchat-memory-auto-recall-cross-session': an
ordinary row in another session is invisible under the default
`remembered', an explicit `/remember' anchor there is visible, `all'
admits both, and `none' admits neither (S23-review High finding,
corrected R3-L10)."
  (require 'superchat-tools)
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "tool-cross-a"))
       (superchat-db-tape-append "tool-cross-a" "user" "MEMSEARCHXYZ ordinary row")
       (superchat--cmd-remember "remember" "MEMSEARCHXYZ remembered fact" nil nil nil))
     (let ((superchat--session-id "tool-cross-b"))
       (let ((result (superchat-tool-memory-search "MEMSEARCHXYZ")))
         (should-not (string-match-p "ordinary row" result))
         (should (string-match-p "remembered fact" result)))
       (let* ((superchat-memory-auto-recall-cross-session 'all)
              (result (superchat-tool-memory-search "MEMSEARCHXYZ")))
         (should (string-match-p "ordinary row" result))
         (should (string-match-p "remembered fact" result)))
       (let* ((superchat-memory-auto-recall-cross-session 'none)
              (result (superchat-tool-memory-search "MEMSEARCHXYZ")))
         (should-not (string-match-p "ordinary row" result))
         (should-not (string-match-p "remembered fact" result)))))))

(ert-deftest test-tool-file-history ()
  "`superchat-tool-file-history' should find tool entries mentioning a path."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "tool-file-session"))
       (superchat-db-tape-append
        "tool-file-session" "tool_call" "write-file"
        :meta '((name . "write-file") (args . ((path . "/tmp/init.el")))))
       (superchat-db-tape-append
        "tool-file-session" "tool_result" "wrote /tmp/init.el")
       (let ((result (superchat-tool-file-history "/tmp/init.el")))
         (should (string-match-p "write-file" result))
         (should (string-match-p "/tmp/init.el" result)))))))

(ert-deftest test-memory-migration-creates-anchors ()
  "`superchat-memory-migrate-to-tape' should convert accepted memories to anchors."
  (require 'superchat-memory)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-memory-insert "remember to use lexical-binding"
                                 :title "lexical binding"
                                 :keywords "elisp"
                                 :status "accepted")
     (let ((count (superchat-memory-migrate-to-tape)))
       (should (= count 1))
       (let ((rows (superchat-db-tape-select
                    "SELECT kind, content, topic FROM tape WHERE session_id = ?"
                    (list "memory-migration"))))
         (should (= (length rows) 1))
         (should (string= "anchor" (caar rows)))
         (should (string= "remember to use lexical-binding" (cadar rows)))
         (should (string= "migrated-memory" (caddar rows))))))))

(ert-deftest view/assemble-typed-compact-anchor-represents-sources-and-keeps-following-fact ()
  "A typed Compact Anchor becomes one derived item before later raw facts."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((source-user
             (superchat-db-tape-append "typed-compact" "user" "old question"))
            (source-answer
             (superchat-db-tape-append "typed-compact" "assistant" "old answer"))
            (anchor-id
             (superchat-db-tape-append
              "typed-compact" "anchor" "compact summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,source-user ,source-answer]))))
            (following-id
             (superchat-db-tape-append
              "typed-compact" "user" "new question"))
            (envelope (superchat-view-assemble "typed-compact"))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         [(:class model-interpretation
           :storage-id 3
           :source-ids [1 2]
           :lossy t
           :content "compact summary")
          (:class raw-fact
           :session-id "typed-compact"
           :storage-id 4
           :kind "user"
           :content "new question")]
         items))
       (should
        (equal
         [(:session-id "typed-compact"
           :range [1 2]
           :via-storage-id 3
           :class model-interpretation
           :lossy t
           :source-ids [1 2])]
         (plist-get disclosure :represented)))
       (should
        (equal [(:session-id "typed-compact" :range [4 4])]
               (plist-get disclosure :covered)))
       (should (= anchor-id 3))
       (should (= following-id 4))))))

(ert-deftest view/assemble-malformed-typed-compact-anchor-fails-closed ()
  "Malformed typed compact provenance is omitted without representation."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "malformed-compact" "user" "before")
     (superchat-db-tape-append
      "malformed-compact" "anchor" "unsafe summary"
      :meta '((anchor_type . "compact")
              (anchor_version . 1)
              (source_ids . [99])))
     (superchat-db-tape-append "malformed-compact" "user" "after")
     (let* ((envelope (superchat-view-assemble "malformed-compact"))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should-not (string-match-p "unsafe summary"
                                   (plist-get envelope :prompt)))
       (should (equal [] (plist-get disclosure :represented)))
       (should
        (equal [(:session-id "malformed-compact"
                 :range [2 2]
                 :reason malformed-provenance
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))))))

(ert-deftest view/expand-anchor-rejects-untyped-provenance-without-fallback ()
  "Expansion never falls back to entries before an untyped Anchor."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "strict-view" "user" "before")
     (superchat-db-tape-append
      "strict-view" "anchor" "untyped summary"
      :meta '((source_ids . [1])))
     (superchat-db-tape-append "strict-view" "user" "after")
     (let ((anchor (superchat-view-anchor-latest "strict-view")))
       (should-not (superchat-view-expand-anchor "strict-view" anchor))))))

(ert-deftest view/assemble-expanded-compact-anchor-uses-raw-sources ()
  "An expanded compact Anchor is suppressed while its raw facts remain."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "expanded-compact")
            (first-id (superchat-db-tape-append session-id "user" "first"))
            (second-id
             (superchat-db-tape-append session-id "assistant" "second"))
            (anchor-id
             (superchat-db-tape-append
              session-id "anchor" "summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-id ,second-id]))))
            (third-id (superchat-db-tape-append session-id "user" "third"))
            (envelope
             (superchat-view-assemble
              session-id :expanded-anchor-ids (vector anchor-id))))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         [(:class raw-fact :session-id "expanded-compact"
           :storage-id 1 :kind "user" :content "first")
          (:class raw-fact :session-id "expanded-compact"
           :storage-id 2 :kind "assistant" :content "second")
          (:class raw-fact :session-id "expanded-compact"
           :storage-id 4 :kind "user" :content "third")]
         (plist-get envelope :items)))
       (should (equal []
                      (plist-get (plist-get envelope :disclosure)
                                 :represented)))
       (should (= third-id 4))))))

(ert-deftest view/assemble-expanded-anchor-uses-high-water-for-older-compactions ()
  "Expanding a flattened Anchor suppresses its earlier typed Anchors."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "expanded-high-water")
            (first-id (superchat-db-tape-append session-id "user" "first"))
            (second-id
             (superchat-db-tape-append session-id "assistant" "second"))
            (first-anchor-id
             (superchat-db-tape-append
              session-id "anchor" "first summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-id ,second-id]))))
            (third-id (superchat-db-tape-append session-id "user" "third"))
            (second-anchor-id
             (superchat-db-tape-append
              session-id "anchor" "second summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-id ,second-id ,third-id]))))
            (before-next-anchor
             (superchat-view-assemble
              session-id :expanded-anchor-ids (vector second-anchor-id)))
            (before-items (plist-get before-next-anchor :items))
            (before-disclosure (plist-get before-next-anchor :disclosure))
            (fourth-id (superchat-db-tape-append session-id "assistant" "fourth"))
            (third-anchor-id
             (superchat-db-tape-append
              session-id "anchor" "third summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-id ,second-id ,third-id ,fourth-id]))))
            (after-next-anchor
             (superchat-view-assemble
              session-id :expanded-anchor-ids (vector second-anchor-id))))
       (should (= first-anchor-id 3))
       (should (= second-anchor-id 5))
       (should (equal '("first" "second" "third")
                      (mapcar (lambda (item) (plist-get item :content))
                              (append before-items nil))))
       (should-not (cl-some (lambda (item)
                              (eq 'model-interpretation
                                  (plist-get item :class)))
                            (append before-items nil)))
       (should (equal [] (plist-get before-disclosure :represented)))
       (should (= third-anchor-id 7))
       (should (equal "third summary"
                      (plist-get (aref (plist-get after-next-anchor :items) 0)
                                 :content)))))))

(ert-deftest view/latest-malformed-typed-compact-anchor-fails-closed ()
  "A malformed latest typed Anchor must not fall back to an older one."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "latest-malformed-compact")
            (source-id (superchat-db-tape-append session-id "user" "source"))
            (first-anchor-id
             (superchat-db-tape-append
              session-id "anchor" "first summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,source-id]))))
            (second-anchor-id
             (superchat-db-tape-append
              session-id "anchor" "unsafe summary"
              :meta '((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [99]))))
            (latest (superchat-view-compaction-anchor-latest session-id)))
       (should (= first-anchor-id 2))
       (should (= second-anchor-id 3))
       (should-not latest)
       (should-not
        (superchat-view-expand-anchor
         session-id (superchat-view-anchor-latest session-id)))))))

(ert-deftest view/assemble-compact-budget-does-not-claim-unselected-summary ()
  "A budget that omits the summary also omits its representation."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "compact-budget")
            (first-id (superchat-db-tape-append session-id "user" "first"))
            (second-id
             (superchat-db-tape-append session-id "assistant" "second"))
            (anchor-id
             (superchat-db-tape-append
              session-id "anchor" "summary"
              :meta `((anchor_type . "compact")
                      (anchor_version . 1)
                      (source_ids . [,first-id ,second-id]))))
            (later-id (superchat-db-tape-append session-id "user" "later"))
            (envelope (superchat-view-assemble session-id :max-entries 1)))
       (should (eq 'partial (plist-get envelope :status)))
       (should (equal "later"
                      (plist-get (aref (plist-get envelope :items) 0)
                                 :content)))
       (should-not (string-match-p "Summary:" (plist-get envelope :prompt)))
       (should (equal []
                      (plist-get (plist-get envelope :disclosure)
                                 :represented)))
       (should (= anchor-id 3))
       (should (= later-id 4))))))

(ert-deftest view/assemble-cancelled-run-is-omitted-and-disclosed ()
  "Default continuity omits a cancelled Run while explicit recall recovers it."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "continuity-cancelled-run")
            (cancelled-intent-id
             (superchat-tape-append-event
              (list :type "intent.received"
                    :session-id session-id
                    :turn-id "turn-cancelled"
                    :run-id "run-cancelled"
                    :summary "cancelled question")))
            (cancel-id
             (superchat-tape-append-event
              (list :type "run.cancelled"
                    :session-id session-id
                    :turn-id "turn-cancelled"
                    :run-id "run-cancelled"
                    :summary "run.cancelled: run-cancelled"
                    :payload '(:status cancelled))))
            (kept-id
             (superchat-tape-append-material
              session-id "user" "kept question"))
            (envelope (superchat-view-assemble session-id :max-chars 4000))
            (items (append (plist-get envelope :items) nil))
            (omitted (append (plist-get (plist-get envelope :disclosure)
                                        :omitted)
                             nil)))
       (should (= 3 (length (superchat-view-entries session-id))))
       (should (cl-find cancelled-intent-id
                         (superchat-view-entries session-id)
                         :key #'car))
       (should (= cancel-id 2))
       (should (= kept-id 3))
       (should (eq 'partial (plist-get envelope :status)))
       (should-not
        (cl-some (lambda (item)
                   (equal "cancelled question" (plist-get item :content)))
                 items))
       (should
        (cl-some (lambda (item)
                   (and (= kept-id (plist-get item :storage-id))
                        (equal "kept question" (plist-get item :content))))
                 items))
       (should
        (cl-some (lambda (entry)
                   (eq 'cancelled (plist-get entry :reason)))
                 omitted))))))

(ert-deftest view/assemble-excludes-superseded-storage-ids ()
  "Assemble and a rebuilt View skip `turn.superseded' storage ids."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "superseded-session")
            (kept (superchat-db-tape-append session-id "user" "kept question"))
            (replaced-user
             (superchat-db-tape-append session-id "user" "replaced question"))
            (replaced-answer
             (superchat-db-tape-append
              session-id "assistant" "replaced answer"))
            (excluded (list replaced-user replaced-answer))
            (envelope
             (superchat-view-assemble session-id :excluded-storage-ids excluded))
            (text (plist-get envelope :prompt)))
       (should (= 3 (length (superchat-view-entries session-id))))
       (should (string-match-p "kept question" text))
       (should-not (string-match-p "replaced question" text))
       (should-not (string-match-p "replaced answer" text))
       (should-not
        (cl-some (lambda (item) (memql (plist-get item :storage-id) excluded))
                 (append (plist-get envelope :items) nil)))
       ;; The View rebuild hides them when the key is given, and derives the
       ;; same ids from `turn.superseded' when it is not.
       (should (equal (list kept)
                      (mapcar #'car
                              (superchat-view-session-history
                               session-id nil nil :excluded-storage-ids excluded))))
       (superchat-tape-append-event
        (list :type "turn.superseded" :schema 1 :session-id session-id
              :payload (list :superseded-storage-ids
                             (vector replaced-user replaced-answer)
                             :reason "edit")))
       (let* ((rows (superchat-view-session-history session-id))
              (ids (mapcar #'car rows))
              (contents (mapcar (lambda (row) (or (nth 3 row) "")) rows)))
         (should (member "kept question" contents))
         (should-not (cl-some (lambda (id) (memql id excluded)) ids))
         (should-not (cl-some (lambda (content)
                                (string-match-p "replaced" content))
                              contents)))
       (should (equal (sort (copy-sequence
                             (superchat-view-superseded-storage-ids session-id))
                            #'<)
                      (sort (copy-sequence excluded) #'<)))
       ;; An omitted `:excluded-storage-ids' derives the same safe default, so
       ;; a caller such as compaction cannot assemble replaced material.
       (let ((default-envelope (superchat-view-assemble session-id)))
         (should (string-match-p "kept question"
                                 (plist-get default-envelope :prompt)))
         (should-not (string-match-p "replaced answer"
                                     (plist-get default-envelope :prompt))))))))

(ert-deftest view/supersession-projection-skips-malformed-payloads ()
  "Only schema-valid `turn.superseded' rows contribute ids; a bad Tape scan
still signals instead of reporting no exclusions."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((session-id "superseded-malformed")
            (_kept (superchat-db-tape-append session-id "user" "kept"))
            (good-user
             (superchat-db-tape-append session-id "user" "replaced q"))
            (good-answer
             (superchat-db-tape-append session-id "assistant" "replaced a"))
            (append-superseded
             (lambda (field)
               (superchat-tape-append-event
                (list :type "turn.superseded" :schema 1 :session-id session-id
                      :payload (list :superseded-storage-ids field)
                      :summary "malformed superseded ids")))))
       ;; Every malformed shape is skipped, not signalled: a number, a string,
       ;; a negative/non-integer sequence element, and an empty field.
       (dolist (field (list 5 "abc" (vector 1 -2) (vector "x") nil))
         (funcall append-superseded field))
       (superchat-tape-append-event
        (list :type "turn.superseded" :schema 1 :session-id session-id
              :payload (list :superseded-storage-ids
                             (vector good-user good-answer)
                             :reason "edit")))
       (should (equal (sort (copy-sequence
                             (superchat-view-superseded-storage-ids session-id))
                            #'<)
                      (sort (list good-user good-answer) #'<)))
       (let* ((rows (superchat-view-session-history session-id))
              (contents (mapcar (lambda (row) (or (nth 3 row) "")) rows)))
         (should (member "kept" contents))
         (should-not (cl-some (lambda (content)
                                (string-match-p "replaced" content))
                              contents)))
       ;; A failed Tape scan still signals.
       (let ((db-file (expand-file-name "superchat.db"
                                        superchat-data-directory)))
         (superchat-db-close)
         (delete-file db-file)
         (with-temp-file db-file (insert "not a sqlite database"))
         (should-error (superchat-view-superseded-storage-ids session-id)))))))

(ert-deftest view/supersession-union-covers-branch-lineage ()
  "Parent and child supersessions both leave the branch View and prompt."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((root-replaced-q
             (superchat-db-tape-append "sup-root" "user" "ROOT_REPLACED_Q"))
            (root-replaced-a
             (superchat-db-tape-append
              "sup-root" "assistant" "ROOT_REPLACED_A"))
            (root-kept-q
             (superchat-db-tape-append "sup-root" "user" "ROOT_KEPT_Q"))
            (root-kept-a
             (superchat-db-tape-append
              "sup-root" "assistant" "ROOT_KEPT_A"))
            (_branch
             (superchat-db-tape-append
              "sup-child" "anchor" "Branched"
              :topic "branch"
              :meta `((parent_session_id . "sup-root")
                      (parent_turn_id . ,root-kept-a)
                      (source_ids . [,root-replaced-q ,root-replaced-a
                                    ,root-kept-q ,root-kept-a]))))
            (child-replaced-q
             (superchat-db-tape-append "sup-child" "user" "CHILD_REPLACED_Q"))
            (child-replaced-a
             (superchat-db-tape-append
              "sup-child" "assistant" "CHILD_REPLACED_A"))
            (_child-kept-a
             (superchat-db-tape-append
              "sup-child" "assistant" "CHILD_KEPT_A")))
       (superchat-tape-append-event
        (list :type "turn.superseded" :schema 1 :session-id "sup-root"
              :payload (list :superseded-storage-ids
                             (vector root-replaced-q root-replaced-a)
                             :reason "regenerate")))
       (superchat-tape-append-event
        (list :type "turn.superseded" :schema 1 :session-id "sup-child"
              :payload (list :superseded-storage-ids
                             (vector child-replaced-q child-replaced-a)
                             :reason "edit")))
       (let ((superseded
              (sort (copy-sequence
                     (superchat-view-superseded-storage-ids "sup-child"))
                    #'<)))
         (should (equal superseded
                        (sort (list root-replaced-q root-replaced-a
                                    child-replaced-q child-replaced-a)
                              #'<)))
         (let* ((rows (superchat-view-session-history "sup-child"))
                (ids (mapcar #'car rows))
                (contents (mapcar (lambda (row) (or (nth 3 row) "")) rows)))
           (should (member "ROOT_KEPT_A" contents))
           (should (member "CHILD_KEPT_A" contents))
           (should-not (cl-some (lambda (content)
                                  (string-match-p "REPLACED" content))
                                contents))
           (should-not (cl-some (lambda (id) (memql id superseded)) ids)))
         (let ((prompt (plist-get (superchat-view-assemble "sup-child")
                                  :prompt)))
           (should (string-match-p "ROOT_KEPT_A" prompt))
           (should (string-match-p "CHILD_KEPT_A" prompt))
           (should-not (string-match-p "REPLACED" prompt))))))))

(ert-deftest view/entries-binds-kinds-without-changing-query-contract ()
  "Kind values stay literal while Session, cursor, limit, and row shape hold."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((a-user-1
             (superchat-db-tape-append "entries-a" "user" "a-user-1"))
            (_b-user
             (superchat-db-tape-append "entries-b" "user" "b-user"))
            (a-assistant
             (superchat-db-tape-append
              "entries-a" "assistant" "a-assistant"))
            (_a-anchor
             (superchat-db-tape-append "entries-a" "anchor" "a-anchor"))
            (a-user-2
             (superchat-db-tape-append "entries-a" "user" "a-user-2"))
            (a-assistant-2
             (superchat-db-tape-append
              "entries-a" "assistant" "a-assistant-2")))
       (should
        (equal '("a-user-1" "a-user-2")
               (mapcar (lambda (row) (nth 3 row))
                       (superchat-view-entries
                        "entries-a" nil '("user")))))
       (should-not
        (superchat-view-entries
         "entries-a" nil '("user') OR 1=1 --")))
       (should-not
        (superchat-view-entries "entries-a" nil '("user--comment")))
       (should
        (equal '("a-user-1" "a-assistant" "a-anchor" "a-user-2"
                 "a-assistant-2")
               (mapcar (lambda (row) (nth 3 row))
                       (superchat-view-entries "entries-a"))))
       (should
        (equal (superchat-view-entries "entries-a")
               (superchat-view-entries "entries-a" nil '())))
       (let ((unlimited-rows
              (superchat-view-entries
               "entries-a" a-user-1 '("assistant" "user")))
             (rows
              (superchat-view-entries
               "entries-a" a-user-1 '("assistant" "user") 2)))
         (should (= 3 (length unlimited-rows)))
         (should (equal (list a-assistant a-user-2 a-assistant-2)
                        (mapcar #'car unlimited-rows)))
         (should (equal '("a-assistant" "a-user-2" "a-assistant-2")
                        (mapcar (lambda (row) (nth 3 row)) unlimited-rows)))
         (should (= 2 (length rows)))
         (should (cl-every (lambda (row) (= 6 (length row))) rows))
         (should (equal (list a-assistant a-user-2)
                        (mapcar #'car rows)))
         (should (equal '("a-assistant" "a-user-2")
                        (mapcar (lambda (row) (nth 3 row)) rows)))
         (should-not (member a-assistant-2 (mapcar #'car rows))))))))

(provide 'test-tape-view)

;;; test-tape-view.el ends here
