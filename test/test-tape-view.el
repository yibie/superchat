;;; test-tape-view.el --- Tests for tape views -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the tape view layer and tape FTS5.

;;; Code:

(require 'ert)
(require 'superchat-db)
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
       (should (= 0 (plist-get receipt :selection-policy-version)))))))

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
         (should (eq 'failed (plist-get receipt :read-status))))))))

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

(ert-deftest view/assemble-default-entry-budget-is-finite ()
  "An omitted entry budget should still cap the View at fifty facts."
  (test-tape-view--with-temp-db
   (lambda ()
     (dotimes (index 51)
       (superchat-db-tape-append
        "cont-default-budget" "user" (format "entry-%02d" (1+ index))))
     (let* ((envelope (superchat-view-assemble "cont-default-budget"))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should (= 50 (length items)))
       (should
        (equal '(:class raw-fact
                 :session-id "cont-default-budget"
                 :storage-id 2
                 :kind "user"
                 :content "entry-02")
               (aref items 0)))
       (should
        (equal '(:class raw-fact
                 :session-id "cont-default-budget"
                 :storage-id 51
                 :kind "user"
                 :content "entry-51")
               (aref items 49)))
       (should
        (string-prefix-p
         "[Context v0 | raw cont-default-budget#2-51 | represented none | omitted 1 | recall available]\n\nUser: entry-02"
         (plist-get envelope :prompt)))
       (should
        (equal [(:session-id "cont-default-budget" :range [2 51])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-default-budget"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should (= 51 (plist-get (plist-get envelope :receipt)
                                :tape-head)))))))

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

(ert-deftest view/assemble-oversized-newest-fact-is-not-forced-in ()
  "A newest fact larger than a positive budget should be disclosed, not kept."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "cont-oversized" "user" "x")
     (superchat-db-tape-append "cont-oversized" "assistant" "BBBB")
     (let* ((envelope (superchat-view-assemble
                       "cont-oversized" :max-chars 10))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw none | represented none | omitted 2 | recall available]"
         (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should (equal [] (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "cont-oversized"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)
                (:session-id "cont-oversized"
                 :range [2 2]
                 :reason oversized
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "cont-oversized"
                 :selector (:range [1 2]))]
               (plist-get disclosure :recallable)))))))

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

(ert-deftest view/recall-range-is-explicit-scoped-and-read-only ()
  "Range recall should exclude outside rows and leave Tape unchanged."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-range" "user" "one")
     (superchat-db-tape-append "recall-range" "assistant" "two")
     (superchat-db-tape-append "recall-range" "user" "three")
     (let* ((envelope
             (superchat-view-recall
              "recall-range" '(:range [1 2])
              :reason "inspect omission"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw recall-range#1-2 | represented none | omitted none | recall none]\n\nUser: one\nAssistant: two"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "recall-range"
           :storage-id 1
           :kind "user"
           :content "one")
          (:class raw-fact
           :session-id "recall-range"
           :storage-id 2
           :kind "assistant"
           :content "two")]
         (plist-get envelope :items)))
       (should
        (equal [(:session-id "recall-range" :range [1 2])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :represented)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (equal [] (plist-get disclosure :recallable)))
       (should (eq 'recall (plist-get scope :purpose)))
       (should (= 2 (plist-get scope :through-storage-id)))
       (should
        (equal [(:session-id "recall-range" :through-storage-id 2)]
               (plist-get scope :lineage)))
       (should
        (equal "continuity-v0/explicit-selector"
               (plist-get scope :selection-policy)))
       (should (= 3 (plist-get (plist-get envelope :receipt) :tape-head)))
       (should
        (equal '(:selector (:range [1 2]) :reason "inspect omission")
               (plist-get envelope :recall))))
     ;; A second public read sees no recall event or other phantom row.
     (let ((again
            (superchat-view-recall
             "recall-range" '(:range [1 3]) :reason "verify no write")))
       (should (= 3 (length (plist-get again :items))))
       (should (= 3 (plist-get (plist-get again :receipt) :tape-head)))))))

(ert-deftest view/recall-range-default-entry-budget-is-finite ()
  "Range recall should apply the same finite default entry budget as Assemble."
  (test-tape-view--with-temp-db
   (lambda ()
     (dotimes (index 51)
       (superchat-db-tape-append
        "recall-budget" "user" (format "fact-%02d" (1+ index))))
     (let* ((envelope
             (superchat-view-recall
              "recall-budget" '(:range [1 51]) :reason "bounded audit"))
            (items (plist-get envelope :items))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should (= 50 (length items)))
       (should (= 2 (plist-get (aref items 0) :storage-id)))
       (should (= 51 (plist-get (aref items 49) :storage-id)))
       (should
        (string-prefix-p
         "[Context v0 | raw recall-budget#2-51 | represented none | omitted 1 | recall available]"
         (plist-get envelope :prompt)))
       (should
        (equal [(:session-id "recall-budget"
                 :range [1 1]
                 :reason budget
                 :recoverable raw-recall)]
               (plist-get disclosure :omitted)))
       (should
        (equal [(:session-id "recall-budget"
                 :selector (:range [1 1]))]
               (plist-get disclosure :recallable)))
       (should (= 51 (plist-get (plist-get envelope :scope)
                                :through-storage-id)))
       (should (= 51 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/recall-range-discloses-session-scoped-missing-source ()
  "A range should not read an ID owned by another Session."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-a" "user" "one")
     (superchat-db-tape-append "recall-b" "user" "other session")
     (superchat-db-tape-append "recall-a" "assistant" "three")
     (let* ((envelope
             (superchat-view-recall
              "recall-a" '(:range [1 3]) :reason "inspect gap"))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'partial (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw recall-a#1, recall-a#3 | represented none | omitted 1 | recall none]\n\nUser: one\nAssistant: three"
         (plist-get envelope :prompt)))
       (should (= 2 (length (plist-get envelope :items))))
       (should
        (equal [(:session-id "recall-a" :range [1 1])
                (:session-id "recall-a" :range [3 3])]
               (plist-get disclosure :covered)))
       (should
        (equal [(:session-id "recall-a"
                 :range [2 2]
                 :reason missing-source
                 :recoverable no)]
               (plist-get disclosure :omitted)))
       (should (equal [] (plist-get disclosure :recallable)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/recall-anchor-blocks-malformed-source-array-without-fallback ()
  "Malformed Anchor provenance should block before any source lookup fallback."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-anchor" "user" "secret source")
     (superchat-db-tape-append
      "recall-anchor" "anchor" "unsafe summary"
      :meta '((source_ids . ((unknown . 3)))))
     (superchat-db-tape-append "recall-anchor" "assistant" "later row")
     (let* ((envelope
             (superchat-view-recall
              "recall-anchor" '(:anchor-id 2)
              :reason "inspect malformed anchor"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'blocked (plist-get envelope :status)))
       (should (equal "" (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should (equal [] (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :represented)))
       (should
        (equal [(:session-id "recall-anchor"
                 :range [2 2]
                 :reason malformed-provenance
                 :recoverable unknown)]
               (plist-get disclosure :omitted)))
       (should (equal [] (plist-get disclosure :recallable)))
       (should (= 2 (plist-get scope :through-storage-id)))
       (should (= 3 (plist-get (plist-get envelope :receipt) :tape-head)))
       (should
        (equal '(:selector (:anchor-id 2)
                 :reason "inspect malformed anchor")
               (plist-get envelope :recall)))))))

(ert-deftest view/recall-anchor-returns-only-validated-same-session-sources ()
  "Anchor recall should return its exact validated raw source rows."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-anchor-ok" "user" "one")
     (superchat-db-tape-append "recall-anchor-ok" "assistant" "two")
     (superchat-db-tape-append
      "recall-anchor-ok" "anchor" "summary"
      :meta '((source_ids . [1 2])))
     (superchat-db-tape-append "recall-anchor-ok" "user" "later")
     (let* ((envelope
             (superchat-view-recall
              "recall-anchor-ok" '(:anchor-id 3)
              :reason "expand exact sources"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw recall-anchor-ok#1-2 | represented none | omitted none | recall none]\n\nUser: one\nAssistant: two"
         (plist-get envelope :prompt)))
       (should
        (equal
         [(:class raw-fact
           :session-id "recall-anchor-ok"
           :storage-id 1
           :kind "user"
           :content "one")
          (:class raw-fact
           :session-id "recall-anchor-ok"
           :storage-id 2
           :kind "assistant"
           :content "two")]
         (plist-get envelope :items)))
       (should
        (equal [(:session-id "recall-anchor-ok" :range [1 2])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (= 3 (plist-get scope :through-storage-id)))
       (should (= 4 (plist-get (plist-get envelope :receipt) :tape-head)))
       (should
        (equal '(:selector (:anchor-id 3)
                 :reason "expand exact sources")
               (plist-get envelope :recall)))))))

(ert-deftest view/recall-anchor-blocks-cross-session-source-id ()
  "Anchor recall should not return a partial set when one source is foreign."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "anchor-owner" "user" "local")
     (superchat-db-tape-append "foreign" "user" "foreign")
     (superchat-db-tape-append
      "anchor-owner" "anchor" "mixed summary"
      :meta '((source_ids . [1 2])))
     (let* ((envelope
             (superchat-view-recall
              "anchor-owner" '(:anchor-id 3)
              :reason "verify source ownership"))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'blocked (plist-get envelope :status)))
       (should (equal "" (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should
        (equal [(:session-id "anchor-owner"
                 :range [3 3]
                 :reason missing-source
                 :recoverable unknown)]
               (plist-get disclosure :omitted)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/recall-storage-ids-preserves-explicit-noncontiguous-scope ()
  "Storage-ID recall should return exactly the named same-Session rows."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-ids" "user" "one")
     (superchat-db-tape-append "recall-ids" "assistant" "outside")
     (superchat-db-tape-append "recall-ids" "user" "three")
     (let* ((envelope
             (superchat-view-recall
              "recall-ids" '(:storage-ids [1 3])
              :reason "compare exact facts"))
            (scope (plist-get envelope :scope))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw recall-ids#1, recall-ids#3 | represented none | omitted none | recall none]\n\nUser: one\nUser: three"
         (plist-get envelope :prompt)))
       (should (= 2 (length (plist-get envelope :items))))
       (should
        (equal [(:session-id "recall-ids" :range [1 1])
                (:session-id "recall-ids" :range [3 3])]
               (plist-get disclosure :covered)))
       (should (equal [] (plist-get disclosure :omitted)))
       (should (= 3 (plist-get scope :through-storage-id)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))
        (should
         (equal '(:selector (:storage-ids [1 3])
                  :reason "compare exact facts")
                (plist-get envelope :recall)))))))

(ert-deftest view/recall-range-renders-operational-rows-verbatim ()
  "Explicit Recall should render ordinary operational kinds without crashing."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append
      "recall-tool" "tool_result" "search returned three files")
     (let ((envelope
            (superchat-view-recall
             "recall-tool" '(:range [1 1]) :reason "inspect tool evidence")))
       (should (eq 'satisfied (plist-get envelope :status)))
       (should
        (equal
         "[Context v0 | raw recall-tool#1 | represented none | omitted none | recall none]\n\nTool Result: search returned three files"
         (plist-get envelope :prompt)))
       (should (= 1 (length (plist-get envelope :items))))
       (should (equal "tool_result"
                      (plist-get (aref (plist-get envelope :items) 0)
                                 :kind)))))))

(ert-deftest view/recall-storage-ids-blocks-with-exact-missing-sources ()
  "Missing explicit IDs should produce evidence instead of a naked error."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "recall-missing" "user" "local")
     (superchat-db-tape-append "another-session" "user" "foreign")
     (superchat-db-tape-append "recall-missing" "assistant" "later")
     (let* ((envelope
             (superchat-view-recall
              "recall-missing" '(:storage-ids [1 2 9])
              :reason "verify requested sources"))
            (disclosure (plist-get envelope :disclosure)))
       (should (eq 'blocked (plist-get envelope :status)))
       (should (equal "" (plist-get envelope :prompt)))
       (should (equal [] (plist-get envelope :items)))
       (should
        (equal [(:session-id "recall-missing"
                 :range [2 2]
                 :reason missing-source
                 :recoverable no)
                (:session-id "recall-missing"
                 :range [9 9]
                 :reason missing-source
                 :recoverable no)]
               (plist-get disclosure :omitted)))
       (should (= 9 (plist-get (plist-get envelope :scope)
                               :through-storage-id)))
       (should (= 3 (plist-get (plist-get envelope :receipt)
                               :tape-head)))))))

(ert-deftest view/recall-read-failure-returns-unknown-envelope ()
  "Recall should contain a real SQLite read failure like Assemble does."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((db-file (expand-file-name "superchat.db"
                                      superchat-data-directory)))
       (superchat-db-close)
       (delete-file db-file)
       (with-temp-file db-file
         (insert "not a sqlite database"))
       (let* ((selector '(:range [1 1]))
              (envelope
               (superchat-view-recall
                "recall-broken" selector :reason "inspect failed read"))
              (scope (plist-get envelope :scope))
              (receipt (plist-get envelope :receipt)))
         (should (eq 'unknown (plist-get envelope :status)))
         (should (equal "" (plist-get envelope :prompt)))
         (should (equal [] (plist-get envelope :items)))
         (should (eq 'recall (plist-get scope :purpose)))
         (should (null (plist-get scope :through-storage-id)))
         (should (eq 'failed (plist-get receipt :read-status)))
         (should (null (plist-get receipt :tape-head)))
         (should
          (equal '(:selector (:range [1 1]) :reason "inspect failed read")
                 (plist-get envelope :recall))))))))

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

(ert-deftest view/recall-rejects-negative-budgets-before-reading ()
  "Recall budgets obey the same public validation contract as Assemble."
  (test-tape-view--with-temp-db
   (lambda ()
     (dolist (arguments '((:max-entries -1) (:max-chars -1)))
       (should-error
        (apply #'superchat-view-recall
               "budget-validation" '(:range [1 1])
               :reason "invalid budget" arguments))))))

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
                        :meta `((source_ids . [,id1 ,id2]))))
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
           (let ((buffer (superchat-open-session "replay-session")))
             (unwind-protect
                 (with-current-buffer buffer
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
                 (kill-buffer buffer))))))
      (delete-directory workspace t))))

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
                  (lambda (content &optional title)
                    (push (list content title) legacy-calls))))
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
           (should (= (length legacy-calls) 1))))))))

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

(provide 'test-tape-view)

;;; test-tape-view.el ends here
