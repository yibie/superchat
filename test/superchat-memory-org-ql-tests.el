
;;; superchat-memory-org-ql-tests.el --- org-ql search tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'package)
(package-initialize)
(require 'org)
(unless (require 'org-ql nil t)
  (error "org-ql not available"))
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'superchat-memory)

(unless (fboundp 'gptel-request)
  (defun gptel-request (&rest _args) nil))
(unless (fboundp 'gptel-request-sync)
  (defun gptel-request-sync (&rest _args) "[]"))

(defun superchat-memory-test--local-search-terms (query)
  (mapcar (lambda (term)
            (let ((clean (string-trim term)))
              (when (> (length clean) 0)
                (list :raw clean
                      :regex (regexp-quote clean)
                      :tag (upcase clean)))))
          (split-string query "[[:space:],]+" t)))

(defun superchat-memory-test--stub-summarize (exchange)
  (let* ((command (plist-get exchange :command))
         (type (or command "insight"))
         (tags (delq nil (list "AUTO-STUB" command))))
    (superchat-memory-capture-conversation exchange
                                          :tier :tier2
                                          :type type
                                          :tags tags)))

(defun superchat-memory-test--stub-gptel-request (&rest _args) nil)

(defun superchat-memory-test--stub-gptel-request-sync (&rest _args) "[]")

(defmacro superchat-memory-test--with-stubs (&rest body)
  `(let ((superchat-memory-stopwords '("the" "and" "or" "with")))
     (cl-letf* (((symbol-function 'superchat-memory--prepare-search-terms) #'superchat-memory-test--local-search-terms)
                ((symbol-function 'superchat-memory-summarize-and-capture) #'superchat-memory-test--stub-summarize)
                ((symbol-function 'gptel-request) #'superchat-memory-test--stub-gptel-request)
                ((symbol-function 'gptel-request-sync) #'superchat-memory-test--stub-gptel-request-sync))
       ,@body)))

(fset 'superchat-memory--prepare-search-terms #'superchat-memory-test--local-search-terms)
(fset 'superchat-memory-summarize-and-capture #'superchat-memory-test--stub-summarize)
(fset 'gptel-request #'superchat-memory-test--stub-gptel-request)
(fset 'gptel-request-sync #'superchat-memory-test--stub-gptel-request-sync)
(setq superchat-memory-stopwords '("the" "and" "or" "with")
      superchat-memory-max-local-keywords 12
      superchat-memory-auto-capture-enabled t)


(defun superchat-memory-test--write-memory (contents)
  (let* ((dir (make-temp-file "superchat-memory" t))
         (file (expand-file-name "memory.org" dir)))
    (with-temp-file file
      (insert contents))
    (list dir file)))

(defmacro superchat-memory-test--with-memory (content &rest body)
  (declare (indent 1))
  `(cl-destructuring-bind (dir file)
       (superchat-memory-test--write-memory ,content)
     (let ((superchat-data-directory dir)
           (superchat-memory-file file)
           (case-fold-search t))
       (superchat-memory-test--with-stubs
         ,@body))))

(defun superchat-memory-test--titles (results)
  (mapcar (lambda (item)
            (let ((title (plist-get item :title)))
              (cond
               ((stringp title) (substring-no-properties title))
               ((and (listp title) (stringp (car title)))
                (substring-no-properties (car title)))
               (t ""))))
          results))

(ert-deftest superchat-memory-extract-explicit-payload-detects-commands ()
  (should (string= "部署计划"
                   (superchat-memory--extract-explicit-payload "记住 部署计划")))
  (should (string= "finish QA checklist"
                   (superchat-memory--extract-explicit-payload "Remember finish QA checklist")))
  (should-not (superchat-memory--extract-explicit-payload "Just a reminder")))

(ert-deftest superchat-memory-auto-capture-respects-tier1 ()
  (superchat-memory-test--with-memory
      ""
    (let* ((superchat-memory-keyword-enrichment-function nil)
           (exchange (list :user "记得 下次要整理发布步骤"
                            :assistant "提供了详细的发布步骤"
                            :title "发布提醒"))
           (id (superchat-memory-auto-capture exchange)))
      (should (stringp id))
      (with-current-buffer (find-file-noselect superchat-memory-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward (regexp-quote id) nil t))
         (org-back-to-heading t)
         (should (string= "tier1-explicit" (org-entry-get (point) "TRIGGER")))
         (should (string= "directive" (downcase (or (org-entry-get (point) "TYPE") ""))))
         (should (= 5 (string-to-number (org-entry-get (point) "ACCESS_COUNT"))))
         (should (member "PRIORITY" (org-get-tags (point)))))))))

(ert-deftest superchat-memory-auto-capture-heuristic-tier2 ()
  (superchat-memory-test--with-memory
      ""
    (let* ((superchat-memory-keyword-enrichment-function nil)
           (superchat-memory-auto-capture-predicate (lambda (_exchange) t))
           (exchange (list :user "好的"
                            :assistant (make-string 140 ?A)
                            :title "自动摘要"))
           (id (superchat-memory-auto-capture exchange)))
      (should (stringp id))
      (with-current-buffer (find-file-noselect superchat-memory-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward (regexp-quote id) nil t))
         (org-back-to-heading t)
         (should (string= "tier2-heuristic" (org-entry-get (point) "TRIGGER")))
         (should (string= "insight"
                          (downcase (or (org-entry-get (point) "TYPE") "")))))))))

(ert-deftest superchat-memory-auto-capture-command-mode ()
  (superchat-memory-test--with-memory
      ""
    (let* ((superchat-memory-keyword-enrichment-function nil)
           (superchat-memory-auto-capture-enabled t)
           (exchange (list :user "design summary"
                            :assistant "Short assistant reply about layout."
                            :content "User: design summary\n\nAssistant: Short assistant reply about layout."
                            :title "Design summary"
                            :command "design"))
           (id (superchat-memory-auto-capture exchange))
           (entries (superchat-memory--retrieve-fallback "layout")))
      (should (stringp id))
      (should (= 1 (length entries)))
      (let ((entry (car entries)))
        (should (equal "design" (plist-get entry :type)))
        (should (member "COMMAND" (plist-get entry :tags)))
        (should (member "DESIGN" (plist-get entry :tags)))))))

(ert-deftest superchat-memory-add-writes-extended-schema ()
  (let* ((dir (make-temp-file "superchat-memory-schema" t))
         (superchat-data-directory dir)
         (superchat-memory-file (expand-file-name "memory.org" dir)))
    (superchat-memory-add
     "Schema entry"
     "Body for schema entry with alpha content"
     :type "note"
     :tags '("foo" "bar")
     :keywords '("alpha" "beta")
     :related '("id-10" "id-11")
     :access-count 3)
    (with-temp-buffer
      (insert-file-contents superchat-memory-file)
      (let ((text (buffer-string)))
        (should (string-match-p ":ACCESS_COUNT: 3" text))
        (should (string-match-p ":KEYWORDS: alpha, beta" text))
        (should (string-match-p ":RELATED:  id-10, id-11" text))))
    (let ((fallback (superchat-memory--retrieve-fallback "Schema")))
      (should (= 1 (length fallback)))
      (let ((entry (car fallback)))
        (should (= 3 (plist-get entry :access-count)))
        (should (equal '("alpha" "beta") (plist-get entry :keywords)))
        (should (equal '("id-10" "id-11") (plist-get entry :related)))))
    (when (featurep 'org-ql)
      (let ((entries (superchat-memory--retrieve-with-org-ql "Schema")))
        (should (= 1 (length entries)))
        (let ((entry (car entries)))
          (should (= 3 (plist-get entry :access-count)))
          (should (equal '("alpha" "beta") (plist-get entry :keywords)))
          (should (equal '("id-10" "id-11") (plist-get entry :related))))))))

(ert-deftest superchat-memory-capture-explicit-generates-keywords ()
  (let* ((dir (make-temp-file "superchat-memory-schema" t))
         (superchat-data-directory dir)
         (superchat-memory-file (expand-file-name "memory.org" dir)))
    (let ((id (superchat-memory-capture-explicit
               "Important decision about vector indexes and caching layers."
               "Decision entry")))
      (should (stringp id))
      (let ((entries (superchat-memory--retrieve-fallback "indexes")))
        (should (= 1 (length entries)))
        (let ((entry (car entries)))
          (should (member "indexes" (plist-get entry :keywords)))
          (should (member "decision" (plist-get entry :keywords))))))))

(ert-deftest superchat-memory-org-ql-matches-body-text ()
  (superchat-memory-test--with-memory
      "* Alpha entry
:PROPERTIES:
:ID:       a
:TIMESTAMP: [2024-01-01]
:TYPE:     note
:END:
Alpha bravo text

* Bravo entry :tag1:
:PROPERTIES:
:ID:       b
:TIMESTAMP: [2024-02-02]
:TYPE:     idea
:END:
Charlie bravo payload
"
    (let ((results (superchat-memory--retrieve-with-org-ql "bravo")))
      (should (= 2 (length results)))
      (should (equal (superchat-memory-test--titles results)
                     '("Alpha entry" "Bravo entry"))))))

(ert-deftest superchat-memory-org-ql-matches-tags ()
  (superchat-memory-test--with-memory
      "* Tagged entry :context:
:PROPERTIES:
:ID:       c
:TIMESTAMP: [2024-03-03]
:TYPE:     snippet
:END:
Body without keyword

* Plain entry
:PROPERTIES:
:ID:       d
:TIMESTAMP: [2024-04-04]
:TYPE:     misc
:END:
No relevant tags here
"
    (let ((results (superchat-memory--retrieve-with-org-ql "context")))
      (should (= 1 (length results)))
      (should (equal (superchat-memory-test--titles results)
                     '("Tagged entry"))))))

(ert-deftest superchat-memory-org-ql-combines-keywords-across-fields ()
  (superchat-memory-test--with-memory
      "* Mixed entry :ref:
:PROPERTIES:
:ID:       e
:TIMESTAMP: [2024-05-05]
:TYPE:     note
:END:
Contains alpha content

* Other entry :ref:
:PROPERTIES:
:ID:       f
:TIMESTAMP: [2024-06-06]
:TYPE:     note
:END:
Irrelevant text
"
    (let ((results (superchat-memory--retrieve-with-org-ql "alpha ref")))
      (should (= 1 (length results)))
      (should (equal (superchat-memory-test--titles results)
                     '("Mixed entry"))))))

(ert-deftest superchat-memory-org-ql-special-characters ()
  (superchat-memory-test--with-memory
      "* C++ entry
:PROPERTIES:
:ID:       g
:TIMESTAMP: [2024-07-07]
:TYPE:     note
:END:
Discusses C++ lambda captures
"
    (let ((results (superchat-memory--retrieve-with-org-ql "C++")))
      (should (= 1 (length results)))
      (should (equal (superchat-memory-test--titles results)
                     '("C++ entry"))))))

(ert-deftest superchat-memory-org-ql-no-false-positives ()
  (superchat-memory-test--with-memory
      "* Delta entry
:PROPERTIES:
:ID:       h
:TIMESTAMP: [2024-08-08]
:TYPE:     note
:END:
Only talks about org-ql
"
    (let ((results (superchat-memory--retrieve-with-org-ql "zulu")))
      (should (null results)))))



(ert-deftest superchat-memory-find-merge-candidates-llm ()
  (superchat-memory-test--with-memory
      "* Shared alpha entry :SHARED:
:PROPERTIES:
:ID:       alpha-id
:TIMESTAMP: [2024-01-01]
:KEYWORDS: alpha, beta, gamma
:END:
Body mentioning shared topics.

* Overlapping alpha entry :SHARED:
:PROPERTIES:
:ID:       beta-id
:TIMESTAMP: [2024-01-02]
:KEYWORDS: alpha, beta
:END:
Another body with overlapping keywords.

* Distinct delta entry :UNIQUE:
:PROPERTIES:
:ID:       delta-id
:TIMESTAMP: [2024-01-03]
:KEYWORDS: delta
:END:
Content unrelated to the shared alpha topic.

* Archived entry :ARCHIVED:
:PROPERTIES:
:ID:       archived-id
:TIMESTAMP: [2024-01-04]
:KEYWORDS: alpha, beta
:END:
Should be ignored because of the ARCHIVED tag.
"
    (cl-letf (((symbol-function 'superchat-memory--check-similarity-with-llm-async)
               (lambda (entry1 entry2 callback)
                 (let* ((ids (list (plist-get entry1 :id)
                                   (plist-get entry2 :id)))
                        (sorted (sort (copy-sequence ids) #'string<)))
                   (funcall callback (equal '("alpha-id" "beta-id") sorted))))))
      (let* ((groups (superchat-memory--find-merge-candidates))
             (group-ids (mapcar (lambda (group)
                                  (sort (mapcar (lambda (entry) (plist-get entry :id)) group)
                                        #'string<))
                                groups)))
        (should (= 1 (length group-ids)))
        (should (member '("alpha-id" "beta-id") group-ids))
        (should-not (member "delta-id" (apply #'append group-ids)))
        (should-not (member "archived-id" (apply #'append group-ids)))))))

(provide 'superchat-memory-org-ql-tests)

(ert-deftest superchat-memory-find-merge-candidates-jaccard-fallback ()
  "Test the async candidate finder relies on Jaccard when LLM is unavailable."
  (let ((superchat-memory-file (expand-file-name "test-memory.org" "/Users/chenyibin/Documents/emacs/package/superchat/test/"))
        (superchat-memory-merge-similarity-threshold 0.4)
        (candidate-groups nil))
    (setq candidate-groups (superchat-memory--find-merge-candidates))
    ;; Verification
    (should (not (null candidate-groups)))
    (should (= 1 (length candidate-groups)))
    (let* ((group (car candidate-groups))
           (ids (sort (mapcar (lambda (entry) (plist-get entry :id)) group) #'string<)))
      (should (= 2 (length group)))
      (should (equal '("ID-A" "ID-B") ids)))))

;;; superchat-memory-org-ql-tests.el ends here
