;;; test-memory-soul.el --- Tests for the v0.6 Memory-Soul dual-track -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the v0.6 Memory-Soul dual-track separation.  These tests
;; exercise pure functions, file I/O, queue state, and the public retrieval
;; surface.  They do NOT require gptel, llm, or org-ql to be installed —
;; the soul log is read/written directly via Org, and synthesis is verified
;; to no-op cleanly when gptel is absent.
;;
;; Note: `test-memory-soul--with-tmp' is a MACRO (not a function) so the
;; body inlines at the call site.  This avoids a `(lambda nil ...)' wrapper
;; that confuses `ert-deftest'\'s eager macroexpansion in Emacs 30.2.
;;
;; Run from repo root:
;;   emacs -Q -L . -l test/test-memory-soul.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-memory)
(require 'org)
(require 'org-element)
(require 'org-id)

;;;;---------------------------------------------
;;;; Helpers
;;;;---------------------------------------------

(defun test-memory-soul--mk-tmp-dir ()
  "Create a unique temp directory and return its path."
  (make-temp-file "superchat-soul-test" t))

(defmacro test-memory-soul--with-tmp (&rest body)
  "Run BODY in a fresh temp data dir; clean up after.
Binds `superchat-data-directory', `superchat-memory-file', and
`superchat-memory-soul-file' to paths under the temp dir.  Implemented
as a macro (not a function taking a lambda) so the body inlines at the
call site and `ert-deftest'\'s eager macroexpansion does not choke."
  (declare (indent 0))
  `(let* ((tmp-dir (test-memory-soul--mk-tmp-dir))
          (superchat-data-directory tmp-dir)
          (superchat-memory-file (expand-file-name "memory.org" tmp-dir))
          (superchat-memory-soul-file (expand-file-name "soul.org" tmp-dir)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory tmp-dir t)))))

(defun test-memory-soul--write-org-entry (file id &optional title content props)
  "Write a single Org entry to FILE with the given ID, TITLE, CONTENT, and PROPS."
  (let* ((title-str (or title "Test Entry"))
         (content-str (or content "Test content body."))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" title-str))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:          %s\n" id))
      (insert ":TIMESTAMP:   [2025-01-01 00:00:00]\n")
      (insert ":TYPE:        directive\n")
      (insert ":ACCESS_COUNT: 0\n")
      (dolist (prop (or props '()))
        (insert (format ":%s: %s\n"
                        (upcase (symbol-name (car prop)))
                        (format "%s" (cdr prop)))))
      (insert ":END:\n")
      (insert content-str "\n")
      (save-buffer))
    buf))

;;;;---------------------------------------------
;;;; Defcustoms
;;;;---------------------------------------------

(ert-deftest test-memory-soul-defcustoms-exist ()
  "All four v0.6 defcustoms exist with documented defaults."
  (should (boundp 'superchat-memory-soul-file))
  (should (boundp 'superchat-memory-mood-taxonomy))
  (should (boundp 'superchat-memory-soul-synthesis-mode))
  (should (boundp 'superchat-memory-contradiction-context-window))
  (should (null superchat-memory-soul-file))
  (should (eq superchat-memory-soul-synthesis-mode 'manual))
  (should (= superchat-memory-contradiction-context-window 3))
  (should (consp superchat-memory-mood-taxonomy))
  (should (member "curious" superchat-memory-mood-taxonomy))
  (should (member "neutral" superchat-memory-mood-taxonomy)))

;;;;---------------------------------------------
;;;; Pure functions
;;;;---------------------------------------------

(ert-deftest test-memory-soul-resolve-mood-exact ()
  "`--resolve-mood' returns canonical lower-case name on exact match."
  (let ((superchat-memory-mood-taxonomy '("curious" "frustrated" "neutral")))
    (should (string= (superchat-memory--resolve-mood "curious") "curious"))
    (should (string= (superchat-memory--resolve-mood "CURIOUS") "curious"))
    (should (string= (superchat-memory--resolve-mood "Frustrated") "frustrated"))))

(ert-deftest test-memory-soul-resolve-mood-fallback ()
  "`--resolve-mood' returns \"neutral\" when no taxonomy match."
  (let ((superchat-memory-mood-taxonomy '("curious" "frustrated")))
    (should (string= (superchat-memory--resolve-mood "ecstatic") "neutral"))
    (should (string= (superchat-memory--resolve-mood nil) "neutral"))
    (should (string= (superchat-memory--resolve-mood "") "neutral"))))

(ert-deftest test-memory-soul-resolve-mood-symbol-input ()
  "`--resolve-mood' accepts symbol input."
  (let ((superchat-memory-mood-taxonomy '("curious" "neutral")))
    (should (string= (superchat-memory--resolve-mood 'curious) "curious"))))

(ert-deftest test-memory-soul-parse-contradiction-tag ()
  "`--parse-contradiction-tag' splits and de-duplicates ids."
  (should (null (superchat-memory--parse-contradiction-tag nil)))
  (should (null (superchat-memory--parse-contradiction-tag "")))
  (should (null (superchat-memory--parse-contradiction-tag "   ")))
  (should (equal (superchat-memory--parse-contradiction-tag "id-1")
                 '("id-1")))
  (should (equal (superchat-memory--parse-contradiction-tag "id-1, id-2, id-3")
                 '("id-1" "id-2" "id-3")))
  (should (equal (superchat-memory--parse-contradiction-tag "id-1 id-2")
                 '("id-1" "id-2")))
  (should (equal (superchat-memory--parse-contradiction-tag "id-1, id-1, id-2")
                 '("id-1" "id-2"))))

(ert-deftest test-memory-soul-format-review-entry ()
  "`--format-review-entry' renders title, properties, and keybinding footer."
  (let* ((entry (list :id "abc-123"
                      :title "Loves milk tea"
                      :content "User explicitly said they love milk tea."
                      :mood "satisfied"
                      :tags '("PREFERENCE" "BEVERAGE")
                      :type "directive"
                      :source-soul-id "soul-456"
                      :contradiction-ids '("old-1")))
         (output (superchat-memory--format-review-entry entry)))
    (should (stringp output))
    (should (string-match-p "Loves milk tea" output))
    (should (string-match-p "abc-123" output))
    (should (string-match-p "satisfied" output))
    (should (string-match-p "PREFERENCE" output))
    (should (string-match-p "soul-456" output))
    (should (string-match-p "old-1" output))
    (should (string-match-p "milk tea" output))
    (should (string-match-p "\\[y\\] accept" output))
    (should (string-match-p "\\[q\\] quit" output))))

;;;;---------------------------------------------
;;;; File I/O
;;;;---------------------------------------------

(ert-deftest test-memory-soul-add-raw-creates-file-and-entry ()
  "`add-raw' creates soul.org and appends a tagged raw event entry."
  (test-memory-soul--with-tmp
    (let ((id (superchat-memory-add-raw "I love milk tea"
                                        :mood "satisfied"
                                        :context "lunch chat"
                                        :verbatim t
                                        :tags '("preference"))))
      (should (stringp id))
      (should (file-exists-p superchat-memory-soul-file))
      (let ((events (superchat-memory--read-raw-events)))
        (should (= 1 (length events)))
        (let ((e (car events)))
          (should (string= (plist-get e :id) id))
          (should (string= (plist-get e :mood) "satisfied"))
          (should (string= (plist-get e :context) "lunch chat"))
          (should (eq (plist-get e :verbatim) t))
          (should (string-match-p "milk tea" (plist-get e :content)))
          (should (member "RAW" (plist-get e :tags)))
          (should (member "PREFERENCE" (plist-get e :tags))))))))

(ert-deftest test-memory-soul-add-raw-untagged-mood-falls-back-to-neutral ()
  "`add-raw' with unknown mood stores \"neutral\" via the resolver."
  (test-memory-soul--with-tmp
    (superchat-memory-add-raw "test"
                              :mood "ecstatic"
                              :context "test")
    (let* ((events (superchat-memory--read-raw-events))
           (e (car events)))
      (should (string= (plist-get e :mood) "neutral")))))

(ert-deftest test-memory-soul-read-raw-events-respects-limit ()
  "`--read-raw-events' returns most-recent N entries when LIMIT is set."
  (test-memory-soul--with-tmp
    (dotimes (i 5)
      (superchat-memory-add-raw (format "Event %d" i)
                                :mood "curious"
                                :context (format "c-%d" i)))
    (let ((all (superchat-memory--read-raw-events)))
      (should (= 5 (length all))))
    (let ((top3 (superchat-memory--read-raw-events 3)))
      (should (= 3 (length top3))))))

(ert-deftest test-memory-soul-read-raw-events-empty-when-no-soul-file ()
  "`--read-raw-events' returns nil when the soul file doesn't exist."
  (test-memory-soul--with-tmp
    (let ((superchat-memory-soul-file (expand-file-name "does-not-exist.org"
                                                       superchat-data-directory)))
      (should (null (superchat-memory--read-raw-events))))))

(ert-deftest test-memory-soul-mark-superseded-sets-flags-and-tags ()
  "`--mark-superseded' sets EXPIRED + SUPERSEDED_BY properties and EXPIRED tag."
  (test-memory-soul--with-tmp
    (let* ((id-1 (org-id-new))
           (id-2 (org-id-new)))
      (test-memory-soul--write-org-entry superchat-memory-file id-1
                                        "Old memory" "Old content" nil)
      (superchat-memory--mark-superseded id-1 id-2)
      (should (superchat-memory--entry-expired-p
               (list :id id-1 :title "Old memory" :content "Old content")))
      (with-current-buffer (find-file-noselect superchat-memory-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward
                  (format ":SUPERSEDED_BY:[ \t]+%s" (regexp-quote id-2))
                  nil t)))))))

(ert-deftest test-memory-soul-entry-contradiction-ids-roundtrip ()
  "`--entry-contradiction-ids' reads what `--mark-superseded'-class writes."
  (test-memory-soul--with-tmp
    (let* ((id-1 (org-id-new))
           (id-2 (org-id-new)))
      (test-memory-soul--write-org-entry
       superchat-memory-file id-1 "Current" "Current content"
       `((CONTRADICTION . ,id-2)))
      (let ((ids (superchat-memory--entry-contradiction-ids
                  (list :id id-1 :title "Current"))))
        (should (consp ids))
        (should (member id-2 ids))))))

(ert-deftest test-memory-soul-find-expired-for-ids-filters-correctly ()
  "`--find-expired-for-ids' only returns entries marked EXPIRED: t."
  (test-memory-soul--with-tmp
    (let* ((id-1 (org-id-new))
           (id-2 (org-id-new))
           (id-3 (org-id-new)))
      (test-memory-soul--write-org-entry superchat-memory-file id-1
                                        "Expired A" "A" nil)
      (test-memory-soul--write-org-entry superchat-memory-file id-2
                                        "Not expired" "B" nil)
      (test-memory-soul--write-org-entry superchat-memory-file id-3
                                        "Expired C" "C" nil)
      (superchat-memory--mark-superseded id-1 id-2)
      (superchat-memory--mark-superseded id-3 id-2)
      (let ((expired (superchat-memory--find-expired-for-ids
                      (list id-1 id-2 id-3))))
        (should (= 2 (length expired)))
        (should (not (member id-2 (mapcar (lambda (e) (plist-get e :id))
                                          expired))))))))

(ert-deftest test-memory-soul-retrieve-with-context-no-contradiction-returns-plain-shape ()
  "`retrieve-with-context' returns standard plist shape when no contradiction fires."
  (test-memory-soul--with-tmp
    (superchat-memory-add "Plain fact" "I drink water daily."
                          :keywords '("water" "drink" "daily")
                          :tags '("HABIT"))
    (let ((result (superchat-memory-retrieve-with-context "what do I drink")))
      (should (consp result))
      (let ((first (car result)))
        (should (plist-get first :title))
        (should (null (plist-get first :paired-expired)))
        (should (null (plist-get first :contradiction-ids)))))))

(ert-deftest test-memory-soul-retrieve-with-context-surfaces-paired-expired ()
  "When query hits a contradiction tag, the expired entry is paired in the result."
  (test-memory-soul--with-tmp
    (let* ((old-id (org-id-new))
           (new-id (superchat-memory-add "Loves black coffee" "User switched to black coffee."
                                         :keywords '("coffee" "drink" "beverage")
                                         :tags '("PREFERENCE"))))
      (test-memory-soul--write-org-entry
       superchat-memory-file old-id
       "Loves milk tea" "User previously said they love milk tea."
       `((CONTRADICTION . ,new-id)
         (EXPIRED . "t")
         (SUPERSEDED_BY . ,new-id)
         (KEYWORDS . "milk tea, drink, beverage")))
      (let* ((result (superchat-memory-retrieve-with-context
                      "what do I like to drink"))
             (first (car result))
             (paired (plist-get first :paired-expired))
             (contr-ids (plist-get first :contradiction-ids)))
        (should (consp paired))
        (should (member old-id (mapcar (lambda (e) (plist-get e :id)) paired)))
        (should (member old-id contr-ids))))))

(ert-deftest test-memory-soul-retrieve-with-context-silent-on-miss ()
  "When query doesn't hit the contradiction topic, the pair stays hidden."
  (test-memory-soul--with-tmp
    (let* ((old-id (org-id-new))
           (new-id (superchat-memory-add "Loves black coffee" "User switched to black coffee."
                                         :keywords '("coffee")
                                         :tags '("PREFERENCE"))))
      (test-memory-soul--write-org-entry
       superchat-memory-file old-id
       "Loves milk tea" "User previously said they love milk tea."
       `((CONTRADICTION . ,new-id)
         (EXPIRED . "t")
         (SUPERSEDED_BY . ,new-id)
         (KEYWORDS . "milk tea, drink")))
      (let ((result (superchat-memory-retrieve-with-context "what is my favorite color")))
        (should (or (null result)
                    (null (plist-get (car result) :paired-expired))))))))

(ert-deftest test-memory-soul-contradiction-window-capped ()
  "Window cap limits the paired list to N expired entries."
  (test-memory-soul--with-tmp
    (let* ((new-id (superchat-memory-add "Latest belief" "User's most current belief on the topic."
                                         :keywords '("topic")
                                         :tags '("BELIEF")))
           (old-ids '()))
      (dotimes (i 5)
        (let* ((oid (org-id-new)))
          (test-memory-soul--write-org-entry
           superchat-memory-file oid
           (format "Expired %d" i) (format "expired %d" i)
           `((CONTRADICTION . ,new-id)
             (EXPIRED . "t")
             (SUPERSEDED_BY . ,new-id)
             (KEYWORDS . "topic")))
          (setq old-ids (append old-ids (list oid)))))
      (let ((superchat-memory-contradiction-context-window 2))
        (let* ((result (superchat-memory-retrieve-with-context "topic"))
               (first (car result))
               (paired (plist-get first :paired-expired)))
          (should (<= (length paired) 2)))))))

;;;;---------------------------------------------
;;;; Review queue state
;;;;---------------------------------------------

(ert-deftest test-memory-soul-review-queue-enqueue-dequeue ()
  "`--enqueue-review' appends; `--dequeue-review' pops FIFO."
  (let ((superchat-memory--review-queue '())
        (e1 (list :id "a" :title "First"))
        (e2 (list :id "b" :title "Second")))
    (superchat-memory--enqueue-review e1)
    (superchat-memory--enqueue-review e2)
    (should (= 2 (length superchat-memory--review-queue)))
    (let ((head (superchat-memory--dequeue-review)))
      (should (eq head e1))
      (should (= 1 (length superchat-memory--review-queue))))
    (let ((head (superchat-memory--dequeue-review)))
      (should (eq head e2))
      (should (null superchat-memory--review-queue)))
    (should (null (superchat-memory--dequeue-review)))))

(ert-deftest test-memory-soul-review-pending-opens-buffer ()
  "`review-pending' creates/populates the review buffer and enables the mode."
  (let ((superchat-memory--review-queue '())
        (superchat-memory--review-current nil)
        (buf nil))
    (unwind-protect
        (progn
          (superchat-memory-review-pending)
          (setq buf (get-buffer superchat-memory--review-buffer-name))
          (should buf)
          (with-current-buffer buf
            (should (derived-mode-p 'special-mode))
            (should superchat-memory-review-mode)
            (should (string-match-p "Review queue empty" (buffer-string)))))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (superchat-memory-review-mode -1))
        (kill-buffer buf)))))

(ert-deftest test-memory-soul-review-accept-and-reject-persist-flags ()
  "`review-accept' sets REVIEWED: t; `review-reject' sets REJECTED: t and EXPIRED: t."
  (test-memory-soul--with-tmp
    (let* ((id-1 (superchat-memory-add "Synthesized A" "Body A"
                                       :type "soul-derived"
                                       :keywords '("k1")
                                       :tags '("SOUL")))
           (id-2 (superchat-memory-add "Synthesized B" "Body B"
                                       :type "soul-derived"
                                       :keywords '("k2")
                                       :tags '("SOUL")))
           (superchat-memory--review-queue (list
                                            (list :id id-1 :title "Synthesized A")
                                            (list :id id-2 :title "Synthesized B"))))
      (let ((superchat-memory--review-current (car superchat-memory--review-queue)))
        (superchat-memory-review-accept))
      (with-current-buffer (find-file-noselect superchat-memory-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward ":REVIEWED:[ \t]+t" nil t))))
      (let ((superchat-memory--review-current (car superchat-memory--review-queue)))
        (superchat-memory-review-reject))
      (with-current-buffer (find-file-noselect superchat-memory-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (should (re-search-forward ":REJECTED:[ \t]+t" nil t))
         (should (re-search-forward ":EXPIRED:[ \t]+t" nil t)))))))

(ert-deftest test-memory-soul-review-mode-map-has-required-bindings ()
  "The review mode keymap binds y, n, e, s, q."
  (let ((map superchat-memory--review-mode-map))
    (should (keymapp map))
    (should (lookup-key map "y"))
    (should (lookup-key map "n"))
    (should (lookup-key map "e"))
    (should (lookup-key map "s"))
    (should (lookup-key map "q"))))

;;;;---------------------------------------------
;;;; Synthesis no-op
;;;;---------------------------------------------

(ert-deftest test-memory-soul-synthesis-noop-without-llm ()
  "`synthesize-soul' is a clean no-op when no llm backend is configured.
The v0.7 migration swapped the gptel dependency for llm.el, so the
no-op message now mentions `llm' instead of `gptel'."
  (test-memory-soul--with-tmp
    (let ((callback-called nil)
          (callback-msg nil)
          (superchat-llm-backend nil))
      (superchat-memory-synthesize-soul
       :limit 5
       :callback (lambda (m) (setq callback-called t callback-msg m)))
      (should callback-called)
      (should (stringp callback-msg))
      (should (string-match-p "llm" callback-msg)))))

(ert-deftest test-memory-soul-soul-synthesizer-prompt-bindable ()
  "The soul-synthesizer prompt custom is reachable and a string."
  (should (stringp superchat-memory-soul-synthesizer-llm-prompt))
  (should (string-match-p "\\$events" superchat-memory-soul-synthesizer-llm-prompt)))

;;;;---------------------------------------------
;;;; Response text extraction (gptel / llm format compat)
;;;;---------------------------------------------

(ert-deftest test-memory-soul-extract-response-text-dotted-pair ()
  "Dotted pair (KEY . VALUE) from gptel's :reasoning callback is extracted.
Regression: gptel's Ollama backend with reasoning-capable models passes
`(reasoning . \"text\")' as the callback's first arg; before the fix the
function errored with `wrong-type-argument plistp' because `plist-member'
requires a proper plist."
  (should (string= "hello world"
                   (superchat-memory--extract-response-text
                    '(reasoning . "hello world"))))
  (should (string= "raw assistant text"
                   (superchat-memory--extract-response-text
                    '(content . "raw assistant text")))))

(ert-deftest test-memory-soul-extract-response-text-proper-plist-still-works ()
  "Existing proper-plist paths (:content / :text / :response) still resolve.
Guards against the dotted-pair branch accidentally swallowing real plists."
  (should (string= "from :text"
                   (superchat-memory--extract-response-text
                    '(:text "from :text"))))
  (should (string= "from :content"
                   (superchat-memory--extract-response-text
                    '(:content "from :content" :other "ignored"))))
  (should (string= "from :response"
                   (superchat-memory--extract-response-text
                    '(:response "from :response")))))

(ert-deftest test-memory-soul-extract-response-text-scalar-cases ()
  "Scalar inputs (string, nil) return the obvious thing."
  (should (string= "raw" (superchat-memory--extract-response-text "raw")))
  (should (string= "" (superchat-memory--extract-response-text nil))))

;;;;---------------------------------------------
;;;; v0.7 gptel→llm.el migration regression
;;;;---------------------------------------------

(defun test-memory-soul--source-file ()
  "Return absolute path to the superchat-memory.el under test."
  (or (locate-library "superchat-memory.el")
      (expand-file-name "../superchat-memory.el"
                        (file-name-directory (or load-file-name buffer-file-name
                                                 default-directory)))))

(ert-deftest test-memory-soul-no-gptel-request-calls ()
  "No `gptel-request' calls remain in superchat-memory.el.
The v0.7 migration moved every LLM call site to llm.el. A stray
`gptel-request' would mean a missed call site (the v0.5 hard-swap
direction).  Only the 4 cosmetic gptel mentions (header comment,
docstring reference, deprecated alias, and the dotted-pair comment
inside `superchat-memory--extract-response-text') are allowed."
  (let* ((file (test-memory-soul--source-file))
         (content (and file (file-readable-p file) (with-temp-buffer
                                                    (insert-file-contents file)
                                                    (buffer-string)))))
    (should content)
    (with-temp-buffer
      (insert content)
      (let ((gptel-request-count 0))
        (goto-char (point-min))
        (while (re-search-forward "gptel-request" nil t)
          (unless (or (nth 4 (syntax-ppss))     ; in a string
                      (nth 4 (syntax-ppss (line-beginning-position)))) ; in a line-comment
            (setq gptel-request-count (1+ gptel-request-count))))
        (should (= 0 gptel-request-count))))))

(ert-deftest test-memory-soul-llm-stream-helper-exists ()
  "The new llm.el streaming helper is defined and its docstring mentions llm
(not gptel), so a future regression to the gptel backend is caught."
  (should (fboundp 'superchat-memory--llm-stream-text))
  (let ((doc (or (documentation 'superchat-memory--llm-stream-text) "")))
    (should (string-match-p "llm" doc))
    (should (not (string-match-p "gptel" doc)))))

(ert-deftest test-memory-soul-llm-keyword-enricher-renamed ()
  "`gptel-keyword-enricher' is renamed to `-llm-keyword-enricher' and the
old name is preserved as an obsolete alias for back-compat with v0.5 callers."
  (should (fboundp 'superchat-memory-llm-keyword-enricher))
  (should (fboundp 'superchat-memory-gptel-keyword-enricher))
  ;; The old name should resolve to the new function.
  (let* ((old-cell (symbol-function 'superchat-memory-gptel-keyword-enricher))
         (new-fn (symbol-function 'superchat-memory-llm-keyword-enricher)))
    (if (symbolp old-cell)
        (should (eq old-cell 'superchat-memory-llm-keyword-enricher))
      ;; define-obsolete-function-alias stores a (alias . new-symbol) cell
      (should (or (eq (cdr-safe old-cell) new-fn)
                  (eq (cdr-safe old-cell) 'superchat-memory-llm-keyword-enricher))))))

(ert-deftest test-memory-soul-superchat-llm-backend-defvar ()
  "`superchat-llm-backend' is declared (defvar) at the top of superchat-memory.el
to silence the free-variable warning that would otherwise surface when the
file is loaded without `superchat'."
  (let* ((file (test-memory-soul--source-file))
         (found nil))
    (should file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^(defvar\\s-+superchat-llm-backend" nil t)
        (setq found t)))
    (should found)))

(provide 'test-memory-soul)
;;; test-memory-soul.el ends here
