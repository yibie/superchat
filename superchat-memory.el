;;; superchat-memory.el --- Memory management for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Memory persistence and retrieval for Superchat. Implements a structured
;; Org-based store with tiered write triggers and optional keyword enrichment.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'org-id) ; Ensure org-id is loaded for org-id-new
(require 'org-ql nil t) ; Optional load of org-ql
(require 'gptel nil t)
(require 'json)

(defvar org-ql-cache nil)
(declare-function org-ql-select "org-ql" (files query &rest options))

(declare-function gptel-request "gptel" (prompt &rest args))

;; This variable is defined in superchat.el
(defvar superchat-data-directory)

(defgroup superchat-memory nil
  "Configuration for Superchat's memory system."
  :group 'superchat)

(defcustom superchat-memory-file nil
  "File path used to persist memories.
If nil, defaults to `memory.org` inside `superchat-data-directory`."
  :type '(choice (const :tag "Use default memory.org" nil) file)
  :group 'superchat-memory)

(defcustom superchat-memory-explicit-trigger-patterns
  '("^\\s-*\\(?:记住\\|记得\\|請記住\\|remember\\)\\s-*")
  "Regexp patterns that identify Tier 1 explicit memory commands in user text."
  :type '(repeat string)
  :group 'superchat-memory)

(defcustom superchat-memory-auto-capture-enabled t
  "When non-nil, Tier 2 automatic captures may run after each exchange."
  :type 'boolean
  :group 'superchat-memory)


(defcustom superchat-memory-auto-capture-minimum-length 120
  "Minimum user message length before auto capture triggers."
  :type 'integer
  :group 'superchat-memory)



(defcustom superchat-memory-keyword-enrichment-function t
  "Optional async enrichment hook for keyword supplementation.
The function is called with two arguments: ENTRY and CALLBACK. ENTRY is a plist
containing at least :id, :title, :content, and :keywords. CALLBACK should be
invoked with a list of keyword strings once enrichment completes."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'superchat-memory)

(defcustom superchat-memory-keyword-llm-prompt
  (string-join
   '("You are a knowledge base assistant. Your task is to generate a rich set of keywords for the memory entry below to make it highly discoverable."
     "Provide a comma-separated list of 5-12 keywords. Include a mix of the following:
1. Core concepts from the text (e.g., 'milk tea', 'carrot juice', 'Lisp').
2. Higher-level categories (e.g., 'beverage', 'preference', 'programming').
3. Relevant English translations if the original is Chinese, and vice-versa."
     "Return only the comma-separated list of keywords."
     ""
     "Title: $title"
     "Content:\n$content")
   "\n")
  "Prompt template used by `superchat-memory-gptel-keyword-enricher`."
  :type 'string
  :group 'superchat-memory)

(defcustom superchat-memory-use-org-ql-cache t
  "When non-nil and org-ql is available, enable `org-ql-cache` during queries."
  :type 'boolean
  :group 'superchat-memory)

(defcustom superchat-memory-max-results 5
  "Maximum number of memories returned per retrieval."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-auto-recall-min-length 3
  "Minimum display width (via `string-width`) required before auto recall runs."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-max-local-keywords 12
  "Maximum number of locally derived keywords stored for each entry."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-stopwords
  '("the" "and" "for" "with" "this" "that" "from" "have" "there" "about" "into" "just"
    "因为" "但是" "我们" "你们" "他们" "以及" "如果" "已经")
  "Stopwords removed from locally generated keyword lists (case-insensitive)."
  :type '(repeat string)
  :group 'superchat-memory)

(defcustom superchat-memory-title-weight 6.0
  "Score weight applied when a query term matches the title."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-body-weight 3.0
  "Score weight applied when a term matches the body content."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-keyword-weight 4.0
  "Score weight applied when a term matches stored keywords."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-tag-weight 2.0
  "Score weight applied when a term matches tags."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-access-weight 1.0
  "Score contribution from the entry's `:ACCESS_COUNT:`."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-recency-weight 2.0
  "Score contribution based on how recent the entry is."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-recency-half-life-days 7.0
  "Half-life in days used when computing recency decay."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-llm-timeout 5.0
  "Maximum seconds to wait for synchronous LLM utilities before falling back."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-auto-increment-access-count t
  "When non-nil, increment `:ACCESS_COUNT:` for retrieved entries."
  :type 'boolean
  :group 'superchat-memory)

(defcustom superchat-memory-decay-factor 0.85
  "Multiplier applied to access counts during decay."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-decay-min-access 0
  "Lower bound for access counts after decay."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-archive-threshold 0
  "Entries at or below this access count may be archived."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-archive-file nil
  "Optional override for the archive file path.
If nil, defaults to `memory-archive.org` inside `superchat-data-directory`."
  :type '(choice (const :tag "Use default archive file" nil) file)
  :group 'superchat-memory)

(defcustom superchat-memory-auto-prune-interval-days 7
  "Interval in days for automatic memory pruning.
If set to nil or 0, auto-pruning is disabled."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'superchat-memory
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'superchat-memory-setup-auto-prune)
           (superchat-memory-setup-auto-prune))))

(defcustom superchat-memory-merger-llm-prompt
  (string-join
   '("You are a memory consolidation assistant. Below are several memory entries that seem related."
     "Your task is to merge them into a single, more comprehensive, and clearer memory entry."
     "If you think they should NOT be merged, respond with only the word 'IGNORE'."
     "Otherwise, respond with a single JSON object containing the merged result with exactly these four keys:"
     "1. \"title\": A new, concise, and overarching title for the merged memory."
     "2. \"summary\": A single, coherent summary synthesized from all provided entries."
     "3. \"keywords\": A new, combined array of the most relevant keyword strings."
     "4. \"tags\": A new, combined array of the most relevant category tags."
     "Do not add any explanation or markdown formatting outside the JSON object."
     ""
     "Memory entries to merge:"
     "---"
     "$content"
     "---")
   "\n")
  "Prompt template used to ask an LLM to merge memory entries."
  :type 'string
  :group 'superchat-memory)

(defcustom superchat-memory-auto-merge-interval-days nil
  "Interval in days for automatic memory merging.
If set to nil or 0, auto-merging is disabled. WARNING: Enabling this
feature carries the risk of incorrect merges by the LLM."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'superchat-memory
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'superchat-memory-setup-auto-merge)
           (superchat-memory-setup-auto-merge))))
 
(defcustom superchat-memory-merge-similarity-threshold 0.5
  "Local keyword-similarity threshold (Jaccard) used as a fallback trigger when entries lack shared tags.
Values are between 0.0 and 1.0. When two entries have a Jaccard similarity
greater than or equal to this value, the LLM will be asked to judge merging."
  :type 'float
  :group 'superchat-memory)

(defcustom superchat-memory-max-concurrent-llm-requests 5
  "Maximum number of concurrent LLM requests for similarity checking.
This prevents overwhelming the LLM service with too many simultaneous requests."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-max-entries-to-process 50
   "Maximum number of entries to process in merge candidate detection.
   Large memory files will be limited to this number to prevent performance issues."
   :type 'integer
   :group 'superchat-memory)

(defcustom superchat-memory-keyword-extractor-llm-prompt
   (string-join
    '("You are a search query expansion assistant. Your goal is to make a user query better for searching a knowledge base."
      "Analyze the user query below and perform two tasks:"
      "1. Extract 2-4 core, stand-alone keyword stems from the query."
      "2. Provide a list of related concepts, synonyms, and translations (English/Chinese) for those core terms."
      "Return a single JSON object with two keys: \"core_terms\" (an array of strings) and \"expanded_terms\" (an array of strings)."
      "Example: for the query '记得喜欢喝什么', you might return:"
      "{\"core_terms\": [\"喜欢\", \"喝\"], \"expanded_terms\": [\"饮料\", \"偏好\", \"beverage\", \"preference\", \"drink\"]}"
      "Do not add explanations."
      "User Query: $query")
    "\n")
   "Prompt to ask an LLM to extract and expand keywords from a user query."
   :type 'string
   :group 'superchat-memory)

(defconst superchat-memory--tier-config
  '((:tier1 . (:trigger "tier1-explicit"
               :type "directive"
               :tags ("memory" "explicit")
               :access 5))
    (:tier2 . (:trigger "tier2-heuristic"
               :type "insight"
               :tags ("memory" "auto")
               :access 1))
    (:tier3 . (:trigger "tier3-retroactive"
               :type "retro"
               :tags ("memory" "manual")
               :access 2)))
  "Metadata presets for tiered write triggers.")

;;;; Helpers

(defun superchat-memory--get-file ()
  "Return absolute path to the memory file."
  (or superchat-memory-file
      (expand-file-name "memory.org" superchat-data-directory)))

(defun superchat-memory--archive-file ()
  "Return absolute path to the archive memory file."
  (or superchat-memory-archive-file
      (expand-file-name "memory-archive.org" superchat-data-directory)))

(defun superchat-memory--ensure-file-exists ()
  "Create the memory file if it does not exist."
  (let ((file (superchat-memory--get-file)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-buffer
        (write-file file)))))

(defun superchat-memory--invalidate-cache ()
  "Drop org-ql cache after writes to avoid stale data."
  (when (and (featurep 'org-ql)
             (fboundp 'org-ql-clear-cache))
    (org-ql-clear-cache)))

(defun superchat-memory--timestamp-string (&optional time)
  "Return Org-style timestamp string for TIME or now."
  (format-time-string "[%Y-%m-%d %H:%M:%S]" (or time (current-time))))

(defun superchat-memory--truncate-title (title)
  "Trim TITLE and ensure it fits in 78 characters."
  (let* ((clean (string-trim (or title "")))
         (limit 78))
    (cond
     ((string-empty-p clean) "Conversation Memory")
     ((<= (length clean) limit) clean)
     (t (concat (substring clean 0 (- limit 3)) "...")))))

(defun superchat-memory--sanitize-string (value)
  "Convert VALUE to trimmed string, returning nil when empty."
  (when value
    (let ((str (string-trim (format "%s" value))))
      (unless (string-empty-p str) str))))

(defun superchat-memory--normalize-tags (tags)
  "Normalize TAGS to a list of uppercase Org tags."
  (when tags
    (let (result)
      (dolist (tag tags)
        (let* ((value (superchat-memory--sanitize-string tag))
               (clean (and value (replace-regexp-in-string ":" "" value))))
          (when clean
            (push (upcase clean) result))))
      (nreverse (delete-dups result)))))

(defun superchat-memory--normalize-type (type)
  "Normalize TYPE to a lower-case string."
  (when-let ((value (superchat-memory--sanitize-string type)))
    (downcase value)))

(defun superchat-memory--keyword-valid-p (keyword)
  "Return non-nil if KEYWORD is long enough to keep."
  (or (> (length keyword) 2)
      (string-match-p "[[:multibyte:]]" keyword)))


(defun superchat-memory--normalize-keywords (keywords)
  "Clean, de-duplicate and truncate KEYWORDS."
  (let* ((stop (mapcar #'downcase superchat-memory-stopwords))
         (seen (make-hash-table :test 'equal))
         (result '()))
    (dolist (kw keywords)
      (let ((word (downcase (or (superchat-memory--sanitize-string kw) ""))))
        (when (and (superchat-memory--keyword-valid-p word)
                   (not (member word stop))
                   (not (gethash word seen)))
          (puthash word t seen)
          (push word result))))
    (setq result (nreverse result))
    (if (and (integerp superchat-memory-max-local-keywords)
             (> superchat-memory-max-local-keywords 0)
             (> (length result) superchat-memory-max-local-keywords))
        (cl-subseq result 0 superchat-memory-max-local-keywords)
      result)))

(defun superchat-memory--normalize-related (related)
  "Normalize RELATED ids to a simple list of trimmed strings."
  (when related
    (let (result)
      (dolist (item related)
        (when-let ((value (superchat-memory--sanitize-string item)))
          (push value result)))
      (nreverse result))))

(defun superchat-memory--generate-local-keywords (title content)
  "Generate simple heuristic keywords from TITLE and CONTENT."
  (let* ((joined (downcase (string-join (delq nil (list title content)) " ")))
         ;; Split on spaces and punctuation. This is a basic but more reliable
         ;; approach for all languages than single-character splitting.
         (all-words (split-string joined "[[:space:][:punct:]]+" t)))
    (superchat-memory--normalize-keywords all-words)))
 
 
(defun superchat-memory--keyword-jaccard (keywords-a keywords-b)
  "Return a Jaccard similarity (0.0 - 1.0) between two keyword lists.
KEYWORDS-A and KEYWORDS-B are lists of strings. Comparison is case-insensitive
and ignores empty or nil values."
  (let* ((clean (lambda (ks)
                  (when ks
                    (let ((out ()))
                      (dolist (k ks)
                        (when-let ((s (superchat-memory--sanitize-string k)))
                          (push (downcase s) out)))
                      (cl-remove-duplicates (nreverse out) :test #'equal)))))
         (a (funcall clean keywords-a))
         (b (funcall clean keywords-b)))
    (if (or (null a) (null b) (and (null a) (null b)))
        0.0
      (let* ((inter (cl-intersection a b :test #'equal))
             (union (cl-union a b :test #'equal))
             (inter-count (float (length inter)))
             (union-count (max 1.0 (float (length union)))))
        (/ inter-count union-count)))))
 
(defun superchat-memory--split-list (value)
  "Split comma-separated VALUE into a list of trimmed strings."
  (when-let ((str (superchat-memory--sanitize-string value)))
    (split-string str "\\s-*,\\s-*" t)))
(defun superchat-memory--stringify (value)
  "Return VALUE coerced to a string."
  (cond
   ((stringp value) value)
   ((null value) "")
   (t (format "%s" value))))

(defun superchat-memory--stringify-list (items)
  "Return ITEMS joined as a comma-separated string."
  (let (values)
    (dolist (item items)
      (when-let ((clean (superchat-memory--sanitize-string item)))
        (push clean values)))
    (mapconcat #'identity (nreverse values) ", ")))

(defun superchat-memory--render-template (template bindings)
  "Replace placeholders in TEMPLATE using BINDINGS alist."
  (let ((result template))
    (dolist (binding bindings result)
      (let ((placeholder (car binding))
            (value (superchat-memory--stringify (cdr binding))))
        (setq result (replace-regexp-in-string placeholder value result nil t))))))

(defun superchat-memory--parse-integer (value)
  "Convert VALUE to integer, defaulting to 0."
  (cond
   ((integerp value) value)
  ((and (stringp value)
        (string-match-p "^-?[0-9]+$" (string-trim value)))
   (string-to-number value))
  (t 0)))

(defun superchat-memory--element-contents-string (element)
  "Return trimmed textual contents of ELEMENT."
  (when (and (org-element-property :contents-begin element)
             (org-element-property :contents-end element))
    (let* ((begin (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (string-trim (buffer-substring-no-properties begin end)))))

(defun superchat-memory--element-to-plist (element)
  "Convert org ELEMENT into the canonical memory plist."
  (let* ((title (or (org-element-property :raw-value element)
                    (org-element-property :title element) ""))
         (content (superchat-memory--element-contents-string element))
         ;; Prefer headline tags; if absent, attempt to read :TAGS: from the property drawer.
         (tags (or (org-element-property :tags element)
                   (let ((begin (org-element-property :begin element)))
                     (when begin
                       (save-excursion
                         (goto-char begin)
                         (when (org-at-heading-p)
                           (let ((prop (org-entry-get (point) "TAGS")))
                             (when prop
                               (mapcar #'upcase (superchat-memory--split-list prop))))))))))
         (id (org-element-property :ID element))
         (timestamp (org-element-property :TIMESTAMP element))
         (type (org-element-property :TYPE element))
         (access (superchat-memory--parse-integer (org-element-property :ACCESS_COUNT element)))
         (keywords (superchat-memory--split-list (org-element-property :KEYWORDS element)))
         (related (superchat-memory--split-list (org-element-property :RELATED element)))
         (trigger (org-element-property :TRIGGER element)))
    (list :title (string-trim (or title ""))
          :content (or content "")
          :id id
          :timestamp timestamp
          :type type
          :access-count access
          :keywords keywords
          :related related
          :trigger trigger
          :tags tags)))

(defun superchat-memory--org-ql-entry ()
  "Return current org heading as a memory plist for org-ql actions."
  (org-with-wide-buffer
   (superchat-memory--element-to-plist (org-element-at-point))))



(defvar superchat-memory--sync-in-flight nil
  "Non-nil while `gptel-request-sync` is waiting for a response.")

;; Remove gptel-request-sync, use asynchronous gptel-request like in superchat.el

(defun superchat-memory--extract-keywords-with-llm-async (query callback)
  "Extract and expand keywords from QUERY using LLM and call CALLBACK with the result."
  (if (and (featurep 'gptel) (fboundp 'gptel-request))
      (let* ((prompt (replace-regexp-in-string "\\$query" query superchat-memory-keyword-extractor-llm-prompt))
             (response-parts '()))
        ;; (message "=== MEMORY DEBUG === Using LLM to extract and expand keywords from: '%s'" query)
        (gptel-request prompt
                       :stream t
                       :callback (lambda (response-or-signal &rest _)
                                   (if (stringp response-or-signal)
                                       (push response-or-signal response-parts)
                                     (when (eq response-or-signal t)
                                       (let* ((full-response (string-join (nreverse response-parts) ""))
                                              (trimmed (string-trim full-response))
                                              (keywords '()))
                                         ;; (message "=== MEMORY DEBUG === LLM keyword expansion response: '%s'" trimmed)
                                        (if (string-prefix-p "{" trimmed)
                                            (let* ((json (json-read-from-string trimmed))
                                                   (core-terms-raw (cdr (assoc 'core_terms json)))
                                                   (expanded-terms-raw (cdr (assoc 'expanded_terms json)))
                                                   (core-list (if (vectorp core-terms-raw) (cl-coerce core-terms-raw 'list) core-terms-raw))
                                                   (expanded-list (if (vectorp expanded-terms-raw) (cl-coerce expanded-terms-raw 'list) expanded-terms-raw)))
                                              (setq keywords (append core-list expanded-list)))
                                           ;; Fallback for old JSON array format or simple list
                                           (if (string-prefix-p "[" trimmed)
                                               (let ((json (json-parse-string trimmed)))
                                                 (setq keywords (if (vectorp json) (cl-coerce json 'list) (list trimmed))))
                                             (setq keywords (split-string trimmed "[,，]+" t))))
                                         ;; (message "=== MEMORY DEBUG === Prepared keywords: %s" keywords)
                                         (funcall callback (delete-dups (mapcar #'string-trim keywords)))))))))
    ;; Fallback to local extraction when LLM not available
    ;; (message "=== MEMORY DEBUG === LLM not available, using local keyword extraction")
    (let ((local-keywords (superchat-memory--generate-local-keywords query nil)))
      (funcall callback local-keywords))))

(defun superchat-memory--prepare-search-terms (query)
  "Use local heuristic to extract keywords from QUERY and return search terms."
  (let ((local-keywords (superchat-memory--generate-local-keywords query nil)))
    (if (string-empty-p (string-trim query))
        nil
      (mapcar (lambda (kw)
                (list :raw kw :regex (regexp-quote kw) :tag (upcase kw)))
              local-keywords))))

(defun superchat-memory--prepare-search-terms-with-llm (query callback)
  "Extract keywords using LLM and prepare search terms, call CALLBACK with terms."
  (superchat-memory--extract-keywords-with-llm-async
   query
   (lambda (keywords)
     (let ((terms (if (string-empty-p (string-trim query))
                      nil
                    (mapcar (lambda (kw)
                              (let ((clean-kw (string-trim kw)))
                                (list :raw clean-kw :regex (regexp-quote clean-kw) :tag (upcase clean-kw))))
                            keywords))))
       ;; (message "=== MEMORY DEBUG === Prepared %d search terms: %s"
       ;;          (length terms) (mapcar (lambda (term) (plist-get term :raw)) terms))
       (funcall callback terms)))))

(defun superchat-memory--ensure-terms (query-or-terms)
  "Normalize QUERY-OR-TERMS to a list of term plists."
  (cond
   ((and (listp query-or-terms) query-or-terms
         (plist-member (car query-or-terms) :regex)) query-or-terms)
   ((stringp query-or-terms) (superchat-memory--prepare-search-terms query-or-terms))
   (t nil)))

(defun superchat-memory--element-matches-p (element terms)
  "Return non-nil when ELEMENT matches all TERMS."
  (let* ((case-fold-search t)
         (title (downcase (or (org-element-property :raw-value element) "")))
         (content (downcase (or (superchat-memory--element-contents-string element) "")))
         (tags (mapcar #'downcase (or (org-element-property :tags element) '())))
         (keywords (mapcar #'downcase (superchat-memory--split-list (org-element-property :KEYWORDS element)))))
    (cl-some (lambda (term)
               (let ((regex (plist-get term :regex))
                     (tag (plist-get term :tag)))
                 (or (and regex (string-match-p regex title))
                     (and regex (string-match-p regex content))
                     (and regex (cl-some (lambda (kw) (string-match-p regex kw)) keywords))
                     (and tag (member (downcase tag) tags)))))
             terms)))

(defun superchat-memory--update-entry-keywords (id new-keywords)
  "Merge NEW-KEYWORDS into entry ID inside the memory file."
  (when (and id new-keywords)
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward (format "^:[ \\t]*ID:[ \\t]*%s\\s-*" (regexp-quote id)) nil t)
         (org-back-to-heading t)
         (let* ((existing (superchat-memory--split-list (org-entry-get (point) "KEYWORDS")))
                (merged (superchat-memory--normalize-keywords (append existing new-keywords))))
           (org-entry-put (point) "KEYWORDS" (mapconcat #'identity merged ", "))
           (save-buffer)
           (superchat-memory--invalidate-cache)
           merged))))))

(defun superchat-memory--maybe-enrich-keywords (entry)
  "Trigger asynchronous keyword enrichment for ENTRY when configured."
  (when (functionp superchat-memory-keyword-enrichment-function)
    (let ((id (plist-get entry :id)))
      (funcall superchat-memory-keyword-enrichment-function
               entry
               (lambda (extra)
                 (when (and id (listp extra))
                   (superchat-memory--update-entry-keywords id extra)))))))

(defun superchat-memory-gptel-keyword-enricher (entry callback)
  "LLM-backed keyword enricher using gptel.
ENTRY is the memory plist; CALLBACK is invoked with keyword list."
  (if (and (featurep 'gptel)
           (fboundp 'gptel-request))
      (let* ((prompt (replace-regexp-in-string "\\$content" (or (plist-get entry :content) "")
                           (replace-regexp-in-string "\\$title" (or (plist-get entry :title) "")
                                                     superchat-memory-keyword-llm-prompt
                                                     nil t)
                           nil t))
             (handler (lambda (response &rest _ignore)
                        (when (and (stringp response)
                                   (not (string-empty-p (string-trim response))))
                          (funcall callback (superchat-memory--split-list response))))))
        (gptel-request prompt :callback handler))
    (message "superchat-memory: gptel is not available; keyword enrichment skipped.")))

(defcustom superchat-memory-summarizer-llm-prompt
  (string-join
   '("You are the long-term memory curator for this chat. Maintain an evolving profile of the human partner."
     "You will receive only the user's latest message (no assistant reply)."
     "Scan the message for persona signals that enrich context about the user. Prioritize:"
     "- Goals, active projects, or responsibilities they care about."
     "- Interests, hobbies, media or subject preferences."
     "- Skills, tools, workflows, or learning preferences (e.g., hands-on, conceptual, while commuting)."
     "- Communication style, tone expectations, or language/formality requests."
     "- Daily events, relationships, milestones, or future plans."
     "- Likes/dislikes, explicit constraints, values, or decision criteria."
     "Before storing anything, ask:"
     "- Does this add new information, refine an existing memory, or contradict something we knew?"
     "- Can it help future replies connect to the user's goals, projects, or preferences?"
     "- Could a clarifying question deepen our understanding without being intrusive?"
     "If the message does NOT provide durable user context, respond with only 'IGNORE'."
     "If it DOES, respond with a single JSON object containing exactly these four keys:"
     "1. \"title\": A concise 5-10 word label capturing the new or updated insight. Reference the relevant goal/project/preference when possible."
     "2. \"summary\": Explain what this message reveals about the user. Mention notable links to existing interests or habits, note contradictions or updates, and propose one gentle follow-up question if clarification would help future interactions. If connections are weak, explicitly say so."
     "3. \"keywords\": An array of 5-8 short keywords (e.g., persona, project, preference, communication-style, plan)."
     "4. \"tags\": An array of 1-3 single-word, lower-case tags classifying the memory (e.g., \"project\", \"preference\", \"identity\", \"plan\", \"style\")."
     "Avoid fabricating links—be proactive but only when the message justifies it."
     ""
     "Latest user message:"
     "---"
     "$content"
     "---")
   "\n")
  "Prompt template used to ask an LLM to summarize a user message for memory."
  :type 'string
  :group 'superchat-memory)

(defcustom superchat-memory-session-summarizer-llm-prompt
  (string-join
   '("You are a long-term memory curator. You will receive a full conversation history."
     "Your task is to synthesize the key insights, decisions, and user profile details from the entire session into a single, durable memory entry."
     "Focus on information that will be useful for future conversations, such as:"
     "- The user's primary goals, projects, or problems discussed."
     "- Key facts, preferences, or constraints revealed by the user."
     "- Decisions made or conclusions reached."
     "- The overall topic and trajectory of the conversation."
     "If the conversation is trivial (e.g., simple greetings, corrections) or contains no lasting information, respond with only the word 'IGNORE'."
     "Otherwise, respond with a single JSON object with these four keys:"
     "1. \"title\": A concise 5-10 word title for the entire session."
     "2. \"summary\": A coherent paragraph summarizing the key takeaways from the conversation."
     "3. \"keywords\": An array of 5-8 relevant keywords."
     "4. \"tags\": An array of 1-3 single-word tags (e.g., \"project-planning\", \"technical-debug\", \"personal-preference\")."
     "Do not add explanations outside the JSON object."
     ""
     "Conversation History:"
     "---"
     "$content"
     "---")
   "\n")
  "Prompt template for summarizing a full conversation session into a memory entry."
  :type 'string
  :group 'superchat-memory)



(defun superchat-memory-summarize-and-capture (exchange)
  "Use an LLM to summarize EXCHANGE and capture it as a Tier 2 memory."
  (when (and (featurep 'gptel) (fboundp 'gptel-request))
    (let* ((content (or (plist-get exchange :content) ""))
           (prompt (replace-regexp-in-string "\\$content" content superchat-memory-summarizer-llm-prompt nil t))
           (handler (lambda (response &rest _ignore)
                      (let ((text (string-trim (or response ""))))
                        (unless (or (string-empty-p text) (string= text "IGNORE"))
                          ;; Extract JSON from markdown code blocks if present
                          (when (string-match "```json\\s-*\\(\\(?:.\\|\\n\\)*?\\)
```" text)
                            (setq text (match-string 1 text)))
                          (setq text (string-trim text))
                          (condition-case err
                              (let* ((json (json-parse-string text :object-type 'plist))
                                     (title (plist-get json :title))
                                     (summary (plist-get json :summary))
                                     (keywords-val (plist-get json :keywords))
                                     (tags-val (plist-get json :tags))
                                     ;; Coerce vectors from JSON arrays into lists
                                     (keywords (if (vectorp keywords-val) (cl-coerce keywords-val 'list) keywords-val))
                                     (tags (if (vectorp tags-val) (cl-coerce tags-val 'list) tags-val)))
                                (if (and title summary (listp keywords) (listp tags))
                                    (superchat-memory-add title summary :tier :tier2 :keywords keywords :tags tags)
                                  (message "superchat-memory: LLM summary was incomplete, memory discarded.")))
                            (error
                             (message "superchat-memory: Failed to parse LLM response as JSON: %s" (error-message-string err))))))))
      (gptel-request prompt :callback handler)))))

(defun superchat-memory-summarize-session-history (history-content)
  "Use an LLM to summarize an entire session HISTORY-CONTENT and capture it."
  (when (and (featurep 'gptel) (fboundp 'gptel-request)
             (stringp history-content) (> (length history-content) 10)) ; Add a minimum length check
    (let* ((prompt (replace-regexp-in-string "\\$content" history-content superchat-memory-session-summarizer-llm-prompt nil t))
           (handler (lambda (response &rest _ignore)
                      (let ((text (string-trim (or response ""))))
                        (unless (or (string-empty-p text) (string= text "IGNORE"))
                          ;; Extract JSON from markdown code blocks if present
                          (when (string-match "```json\\s-*\\(\\(?:.\\|\\n\\)*?\\)```" text)
                            (setq text (match-string 1 text)))
                          (setq text (string-trim text))
                          (condition-case err
                              (let* ((json (json-parse-string text :object-type 'plist))
                                     (title (plist-get json :title))
                                     (summary (plist-get json :summary))
                                     (keywords-val (plist-get json :keywords))
                                     (tags-val (plist-get json :tags))
                                     (keywords (if (vectorp keywords-val) (cl-coerce keywords-val 'list) keywords-val))
                                     (tags (if (vectorp tags-val) (cl-coerce tags-val 'list) tags-val)))
                                (if (and title summary (listp keywords) (listp tags))
                                    (superchat-memory-add title summary :tier :tier2 :keywords keywords :tags tags)
                                  (message "superchat-memory: LLM session summary was incomplete, memory discarded.")))
                            (error
                             (message "superchat-memory: Failed to parse session summary as JSON: %s" (error-message-string err)))))))))
      (gptel-request prompt :callback handler))))

(defun superchat-memory--tier-options (tier)
  "Return metadata plist for TIER symbol."
  (or (cdr (assoc tier superchat-memory--tier-config))
      (cdr (assoc :tier2 superchat-memory--tier-config))))

(defun superchat-memory--extract-explicit-payload (text)
  "Extract payload following explicit memory command TEXT."
  (when (stringp text)
    (cl-loop for pattern in superchat-memory-explicit-trigger-patterns
             thereis (when (string-match pattern text)
                       (let* ((payload (string-trim (substring text (match-end 0))))
                              (clean (string-trim-left payload "[\s\"\'：:]+")))
                         (unless (string-empty-p clean) clean))))))


(defun superchat-memory-compose-title (content)
  "Derive a concise title from CONTENT."
  (let* ((text (string-trim (or content "")))
         (first-line (car (split-string text "\n" t)))
         (clean-line (if first-line (string-trim (replace-regexp-in-string "^\\(\*+\|#+\\)\\s-*" "" first-line)) ""))
         (candidate (if (string-empty-p clean-line) text clean-line)))
    (when (string-match "\`\(.\{1,72\}?[。！？.!?]\)" candidate)
      (setq candidate (match-string 1 candidate)))
    (superchat-memory--truncate-title (or candidate "Conversation Memory"))))

;;;; Write Path

(cl-defun superchat-memory-add (title content &key type tags keywords related access-count tier trigger id timestamp)
  "Add a memory entry with TITLE and CONTENT.
TYPE, TAGS, KEYWORDS, RELATED, ACCESS-COUNT, TIER, TRIGGER, ID, and TIMESTAMP
customize the metadata. Returns the entry id."
  (let* ((entry-id (or id (org-id-new)))
         (ts (or (superchat-memory--sanitize-string timestamp)
                 (superchat-memory--timestamp-string)))
         (meta (superchat-memory--tier-options (or tier :tier2)))
         (final-trigger (or trigger (plist-get meta :trigger)))
         (final-type (or (superchat-memory--normalize-type type)
                         (plist-get meta :type)))
         (final-tags (superchat-memory--normalize-tags (append tags (plist-get meta :tags))))
         (final-related (superchat-memory--normalize-related related))
         (requested-keywords (superchat-memory--normalize-keywords keywords))
         (generated-keywords (or requested-keywords
                                   (superchat-memory--generate-local-keywords title content)))
         (final-keywords generated-keywords)
         (final-access (or access-count (plist-get meta :access) 0)))
    (superchat-memory--ensure-file-exists)
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((tag-str (if final-tags
                         (concat " :" (mapconcat #'identity final-tags ":") ":")
                       "")))
        (insert (format "* %s%s\n" (superchat-memory--truncate-title title) tag-str)))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" entry-id))
      (insert (format ":TIMESTAMP: %s\n" ts))
      (when final-type
        (insert (format ":TYPE:     %s\n" final-type)))
      (insert (format ":ACCESS_COUNT: %d\n" (max 0 final-access)))
      (when final-keywords
        (insert (format ":KEYWORDS: %s\n" (mapconcat #'identity final-keywords ", "))))
      (when final-related
        (insert (format ":RELATED:  %s\n" (mapconcat #'identity final-related ", "))))
      (when final-trigger
        (insert (format ":TRIGGER:  %s\n" final-trigger)))
      (insert ":END:\n")
      (when (and content (not (string-empty-p (string-trim content))))
        (insert (string-trim-right content) "\n"))
      (save-buffer))
    (superchat-memory--invalidate-cache)
    (superchat-memory--maybe-enrich-keywords
     (list :id entry-id :title title :content content :keywords final-keywords))
    entry-id))

(cl-defun superchat-memory-capture-explicit (content &optional title &key tags type keywords related)
  "Tier 1 capture driven by explicit user request. Returns entry id."
  (let ((final-title (or (superchat-memory--sanitize-string title)
                         (superchat-memory-compose-title content))))
    (superchat-memory-add final-title content
                          :type (or type "directive")
                          :tags (append '("PRIORITY") tags)
                          :keywords keywords
                          :related related
                          :access-count 5
                          :tier :tier1)))

(cl-defun superchat-memory-capture-conversation (exchange &key tier tags type keywords access-count related)
  "Capture EXCHANGE plist (:content, etc.) as a memory entry.
Returns the new entry id."
  (let* ((content (or (plist-get exchange :content)
                      (plist-get exchange :user)
                      ""))
         (title (or (plist-get exchange :title)
                    (superchat-memory-compose-title content)))
         (tier (or tier :tier2)))
    (superchat-memory-add title content
                          :tier tier
                          :type type
                          :tags tags
                          :keywords keywords
                          :related related
                          :access-count access-count)))

(defun superchat-memory-auto-capture (exchange)
  "Evaluate EXCHANGE plist and perform tiered capture when triggered."
  (let* ((explicit (superchat-memory--extract-explicit-payload (plist-get exchange :user)))
         (command (plist-get exchange :command)))
    (cond
     (explicit
      (superchat-memory-capture-explicit (or explicit (plist-get exchange :content))
                                         (plist-get exchange :title)))
     ((and command superchat-memory-auto-capture-enabled)
      (let ((tags (delq nil (list "command" command))))
        (superchat-memory-capture-conversation exchange
                                               :tier :tier2
                                               :type (or command "command")
                                               :tags tags)))
     (superchat-memory-auto-capture-enabled
      ;; Session-based summary is now handled by a hook.
      (superchat-memory-summarize-and-capture exchange)
      )
     (t nil))))

;;;; Read Path

(defun superchat-memory-retrieve-async (query-string callback)
  "Asynchronously retrieve memories relevant to QUERY-STRING, call CALLBACK with results."
  (when (and (stringp query-string)
             (not (string-empty-p (string-trim query-string))))
    ;; (message "=== MEMORY DEBUG === Async query: '%s'" query-string)
    (superchat-memory--prepare-search-terms-with-llm
     query-string
     (lambda (terms)
       (let* ((entries (if (featurep 'org-ql)
                           (superchat-memory--retrieve-with-org-ql terms)
                         (superchat-memory--retrieve-fallback terms)))
              (ranked (superchat-memory--rank-entries entries terms)))
         ;; (message "=== MEMORY DEBUG === Found %d entries before ranking" (length entries))
         ;; (message "=== MEMORY DEBUG === Ranked to %d entries" (length ranked))
         ;; (when ranked
         ;;   (message "=== MEMORY DEBUG === Top results: %s"
         ;;            (mapcar (lambda (entry) (plist-get entry :title)) ranked)))
         (superchat-memory--touch-access-counts ranked)
         (funcall callback ranked))))))

(defun superchat-memory-retrieve (query-string)
  "Retrieve memories relevant to QUERY-STRING with ranking and scoring.
This is the synchronous version that falls back to local keyword extraction."
  (when (and (stringp query-string)
             (not (string-empty-p (string-trim query-string))))
    ;; (message "=== MEMORY DEBUG === Sync query: '%s'" query-string)
    (let* ((terms (superchat-memory--prepare-search-terms query-string))
           (entries (if (featurep 'org-ql)
                        (superchat-memory--retrieve-with-org-ql terms)
                      (superchat-memory--retrieve-fallback terms)))
           (ranked (superchat-memory--rank-entries entries terms)))
      ;; (message "=== MEMORY DEBUG === Parsed terms: %s"
      ;;          (mapcar (lambda (term) (plist-get term :raw)) terms))
      ;; (message "=== MEMORY DEBUG === Found %d entries before ranking" (length entries))
      ;; (message "=== MEMORY DEBUG === Ranked to %d entries" (length ranked))
      ;; (when ranked
      ;;   (message "=== MEMORY DEBUG === Top results: %s"
      ;;            (mapcar (lambda (entry) (plist-get entry :title)) ranked)))
      (superchat-memory--touch-access-counts ranked)
      ranked)))

(defun superchat-memory--retrieve-with-org-ql (query-or-terms)
  "Retrieve using org-ql with dynamic query construction."
  (let ((terms (superchat-memory--ensure-terms query-or-terms)))
    (when terms
      (let* ((file (superchat-memory--get-file))
             (buffer (find-file-noselect file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((case-fold-search t))
              (when superchat-memory-use-org-ql-cache
                (setq org-ql-cache (or org-ql-cache t)))
              (let ((query `(or ,@(mapcar (lambda (term)
                                            (let* ((regex (plist-get term :regex))
                                                   (tag (plist-get term :tag))
                                                   (clauses (delq nil (list (and regex (list 'regexp regex))
                                                                            (and tag (list 'tags tag))))))
                                              (cons 'or clauses)))
                                          terms))))
                (delq nil (org-ql-select buffer query
                                        :action #'superchat-memory--org-ql-entry))))))))))

(defun superchat-memory--retrieve-fallback (query-or-terms)
  "Fallback search implementation when org-ql is unavailable."
  (let ((terms (superchat-memory--ensure-terms query-or-terms))
        (results '()))
    (when terms
      (with-current-buffer (find-file-noselect (superchat-memory--get-file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (let ((element (org-element-at-point)))
             (when (superchat-memory--element-matches-p element terms)
               (push (superchat-memory--element-to-plist element) results)))))))
    (nreverse results)))

(defun superchat-memory--rank-entries (entries terms)
  "Score and sort ENTRIES based on relevance to TERMS."
  (when entries
    (let ((case-fold-search t)
          (scored-entries '()))
      (dolist (entry entries)
        (let ((score 0.0)
              (title (downcase (plist-get entry :title)))
              (content (downcase (plist-get entry :content)))
              (keywords (mapcar #'downcase (plist-get entry :keywords)))
              (tags (mapcar #'downcase (plist-get entry :tags))))
          ;; Term matching score
          (dolist (term terms)
            (let ((regex (plist-get term :regex)))
              (when (string-match-p regex title) (cl-incf score superchat-memory-title-weight))
              (when (string-match-p regex content) (cl-incf score superchat-memory-body-weight))
              (when (cl-some (lambda (kw) (string-match-p regex kw)) keywords) (cl-incf score superchat-memory-keyword-weight))
              (when (member (plist-get term :tag) tags) (cl-incf score superchat-memory-tag-weight))))

          ;; Access count score
          (cl-incf score (* (plist-get entry :access-count) superchat-memory-access-weight))

          ;; Recency score (exponential decay)
          (when-let* ((ts-str (plist-get entry :timestamp))
                      (ts (org-time-string-to-time ts-str)))
            (let* ((now (current-time))
                   (age-days (/ (time-to-seconds (time-subtract now ts)) (* 3600.0 24.0)))
                   (half-life superchat-memory-recency-half-life-days))
              (when (> half-life 0)
                (cl-incf score (* superchat-memory-recency-weight
                                    (exp (* (- (log 2)) (/ age-days half-life))))))))

          (push (cons score entry) scored-entries)))

      (let* ((sorted (sort scored-entries (lambda (a b) (> (car a) (car b)))))
             (top-n (if (> (length sorted) superchat-memory-max-results)
                        (cl-subseq sorted 0 superchat-memory-max-results)
                      sorted)))
        (mapcar #'cdr top-n)))))

(defun superchat-memory--touch-access-counts (ranked-entries)
  "Increment access count for RANKED-ENTRIES."
  (when superchat-memory-auto-increment-access-count
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (let ((modified nil))
        (dolist (entry ranked-entries)
          (when-let ((id (plist-get entry :id)))
            (org-with-wide-buffer
             (goto-char (point-min))
             (when (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote id)) nil t)
               (org-back-to-heading t)
               (let* ((current-val (superchat-memory--parse-integer (org-entry-get (point) "ACCESS_COUNT")))
                      (new-val (1+ current-val)))
                 (org-entry-put (point) "ACCESS_COUNT" (format "%d" new-val))
                 (setq modified t))))))
        (when modified
          (save-buffer)
          (superchat-memory--invalidate-cache))))))

;;;; Lifecycle Management

(defun superchat-memory-prune (&optional silent)
  "Apply decay and archive old memories."
  (interactive)
  (let* ((active-file (superchat-memory--get-file))
         (archive-file (superchat-memory--archive-file))
         (kept-entries '())
         (archived-entries '()))

    (unless (file-exists-p active-file)
      (unless silent (message "Memory file does not exist."))
      (cl-return-from superchat-memory-prune nil))

    (with-current-buffer (find-file-noselect active-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
         (let* ((element (org-element-at-point))
                (entry-text (buffer-substring-no-properties (point) (org-element-property :end element)))
                (access-count (superchat-memory--parse-integer (org-element-property :ACCESS_COUNT element)))
                (new-count (floor (* access-count superchat-memory-decay-factor))))
           (if (<= new-count superchat-memory-archive-threshold)
               (push entry-text archived-entries)
             (let ((updated-text (replace-regexp-in-string
                                  (format ":ACCESS_COUNT:[ \t]+%d" access-count)
                                  (format ":ACCESS_COUNT: %d" new-count)
                                  entry-text)))
               (push updated-text kept-entries)))))))

    ;; Write back the updated files
    (if (not kept-entries)
        (delete-file active-file)
      (with-temp-buffer
        (dolist (entry (nreverse kept-entries))
          (insert entry))
        (write-file active-file)))

    (when archived-entries
      (make-directory (file-name-directory archive-file) t)
      (with-temp-buffer
        (when (file-exists-p archive-file)
          (insert-file-contents archive-file))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (dolist (entry (nreverse archived-entries))
          (insert entry))
        (write-file archive-file)))

    (superchat-memory--invalidate-cache)
    (let ((msg (format "Memory pruned: %d entries kept, %d archived."
                       (length kept-entries) (length archived-entries))))
      (unless silent (message msg))
      msg)))

(defun superchat-memory--find-merge-candidates ()
  "Find groups of similar memories based on local heuristics (tags and keywords)."
  (message "superchat-memory: Scanning for merge candidates using local heuristics...")
  (let* ((all-entries (with-current-buffer (find-file-noselect (superchat-memory--get-file))
                        (org-with-wide-buffer
                         (let (collected)
                           (org-map-entries (lambda ()
                                              (push (superchat-memory--org-ql-entry) collected))
                                            "-ARCHIVED" 'file)
                           (nreverse collected)))))
         (entries (if (> (length all-entries) superchat-memory-max-entries-to-process)
                      (progn
                        (message "superchat-memory: limiting to %d entries (found %d total)"
                                 superchat-memory-max-entries-to-process (length all-entries))
                        (cl-subseq all-entries 0 superchat-memory-max-entries-to-process))
                    all-entries))
         (groups '())
         (visited (make-hash-table :test 'equal)))

    (dolist (entry entries)
      (let ((id (plist-get entry :id)))
        (unless (gethash id visited)
          (let ((current-group '()) (to-visit (list entry)))
            ;; Start a traversal (DFS/BFS) from the current entry
            (while to-visit
              (let* ((current-entry (pop to-visit))
                     (current-id (plist-get current-entry :id)))
                (unless (gethash current-id visited)
                  (puthash current-id t visited)
                  (push current-entry current-group)
                  ;; Find all neighbors of current-entry
                  (dolist (neighbor entries)
                    (let ((neighbor-id (plist-get neighbor :id)))
                      (unless (or (equal current-id neighbor-id) (gethash neighbor-id visited))
                        (let* ((tags1 (plist-get current-entry :tags))
                               (tags2 (plist-get neighbor :tags))
                               (common-tags (and tags1 tags2 (cl-intersection tags1 tags2 :test #'equal)))
                               (jaccard (superchat-memory--keyword-jaccard (plist-get current-entry :keywords) (plist-get neighbor :keywords)))
                               (threshold superchat-memory-merge-similarity-threshold))
                          (when (or common-tags (>= jaccard threshold))
                            (push neighbor to-visit)))))))))
            (when (> (length current-group) 1)
              (push current-group groups))))))
    (message "superchat-memory: Found %d potential merge groups." (length groups))
    groups))

(defun superchat-memory--archive-entry-by-id (id &optional new-id)
  "Find entry with ID and mark it as archived, linking to NEW-ID."
  (when id
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward (format ":ID:[ \\t]+%s" (regexp-quote id)) nil t)
         (org-back-to-heading t)
         ;; Add :ARCHIVED: tag
         (let* ((tags (org-get-tags)))
           (unless (member "ARCHIVED" tags)
             (org-set-tags (append tags '("ARCHIVED")))))
         ;; Add :RELATED: property to the new entry
         (when new-id
           (let* ((related (superchat-memory--split-list (org-entry-get (point) "RELATED"))))
             (org-entry-put (point) "RELATED" (mapconcat #'identity (append related (list new-id)) ", "))))
         (save-buffer)
         t)))))



(defun superchat-memory-merge-duplicates ()
  "Interactively find and merge duplicate or similar memories."
  (interactive)
  (let ((candidate-groups (superchat-memory--find-merge-candidates)))
    (if (not candidate-groups)
        (message "No potential duplicates found to merge.")
      (let ((group-count (length candidate-groups)))
        (when (yes-or-no-p (format "Found %d potential group(s) to merge. Proceed?" group-count))
          (let ((merged-count 0)
                (processed-count 0)
                (pending-merges (length candidate-groups)))
            (dolist (group candidate-groups)
              (setq processed-count (1+ processed-count))
              (message "Processing group %d/%d..." processed-count group-count)
              (let* ((content-to-merge
                      (mapconcat (lambda (entry)
                                   (format "--- Entry ID: %s ---\nTitle: %s\n\n%s"
                                           (plist-get entry :id)
                                           (plist-get entry :title)
                                           (plist-get entry :content)))
                                 group "\n\n"))
                     (prompt (replace-regexp-in-string "\$content" content-to-merge superchat-memory-merger-llm-prompt))
                     (response-parts '()))
                (gptel-request prompt
                               :stream t
                               :callback (lambda (response-or-signal &rest _)
                                           (if (stringp response-or-signal)
                                               (push response-or-signal response-parts)
                                             (when (eq response-or-signal t)
                                               (let ((full-response (string-join (nreverse response-parts) "")))
                                                 (when (and full-response (not (string= full-response "IGNORE")))
                                                   (let* ((json (json-parse-string full-response :object-type 'plist))
                                                          (title (plist-get json :title))
                                                          (summary (plist-get json :summary))
                                                          (keywords (if (vectorp (plist-get json :keywords)) (cl-coerce (plist-get json :keywords) 'list) nil))
                                                          (tags (if (vectorp (plist-get json :tags)) (cl-coerce (plist-get json :tags) 'list) nil)))
                                                     (if (and title summary keywords tags)
                                                         (let ((new-id (superchat-memory-add title summary :tier :tier2 :keywords keywords :tags tags)))
                                                           (message "Created new merged memory %s." new-id)
                                                           (dolist (old-entry group)
                                                             (superchat-memory--archive-entry-by-id (plist-get old-entry :id) new-id))
                                                           (cl-incf merged-count))
                                                       (message "LLM response for group was incomplete.")))))
                                               (when (zerop (cl-decf pending-merges))
                                                 (message "Merge complete. Processed %d groups, created %d new memories." group-count merged-count)
                                                 (superchat-memory--invalidate-cache))))))))
            (when (zerop pending-merges)
              (message "Merge complete. Processed %d groups, created %d new memories." group-count merged-count)
              (superchat-memory--invalidate-cache))))))))

(defun superchat-memory--auto-merge-all ()
  "Find and merge all candidate groups non-interactively."
  (let ((candidate-groups (superchat-memory--find-merge-candidates)))
    (when candidate-groups
      (let ((pending-merges (length candidate-groups))
            (merged-count 0))
        (dolist (group candidate-groups)
          (let* ((content-to-merge
                  (mapconcat (lambda (entry)
                               (format "--- Entry ID: %s ---\nTitle: %s\n\n%s"
                                       (plist-get entry :id)
                                       (plist-get entry :title)
                                       (plist-get entry :content)))
                             group "\n\n"))
                 (prompt (replace-regexp-in-string "\$content" content-to-merge superchat-memory-merger-llm-prompt))
                 (response-parts '()))
            (gptel-request prompt
                           :stream t
                           :callback (lambda (response-or-signal &rest _)
                                       (if (stringp response-or-signal)
                                           (push response-or-signal response-parts)
                                         (when (eq response-or-signal t)
                                           (let ((full-response (string-join (nreverse response-parts) "")))
                                             (when (and full-response (not (string= full-response "IGNORE")))
                                               (let* ((json (json-parse-string full-response :object-type 'plist))
                                                      (title (plist-get json :title))
                                                      (summary (plist-get json :summary))
                                                      (keywords (if (vectorp (plist-get json :keywords)) (cl-coerce (plist-get json :keywords) 'list) nil))
                                                      (tags (if (vectorp (plist-get json :tags)) (cl-coerce (plist-get json :tags) 'list) nil)))
                                                 (if (and title summary keywords tags)
                                                     (let ((new-id (superchat-memory-add title summary :tier :tier2 :keywords keywords :tags tags)))
                                                       (dolist (old-entry group)
                                                         (superchat-memory--archive-entry-by-id (plist-get old-entry :id) new-id))
                                                       (cl-incf merged-count))
                                                   (message "superchat-memory: Auto-merge LLM response for group was incomplete.")))))
                                         (when (zerop (cl-decf pending-merges))
                                           (message "Auto-merge complete. Created %d new memories." merged-count)
                                           (superchat-memory--invalidate-cache)))))))
        (when (zerop pending-merges)
          (message "Auto-merge complete. Created %d new memories." merged-count)
          (superchat-memory--invalidate-cache)))))))

(defvar superchat-memory--prune-timer nil "Timer for auto-pruning memory.")
(defvar superchat-memory--merge-timer nil "Timer for auto-merging memory.")

(defun superchat-memory-setup-auto-prune ()
  "Set up or cancel the automatic memory pruning timer."
  (when (and superchat-memory--prune-timer (timerp superchat-memory--prune-timer))
    (cancel-timer superchat-memory--prune-timer)
    (setq superchat-memory--prune-timer nil))
  (when (and (integerp superchat-memory-auto-prune-interval-days)
             (> superchat-memory-auto-prune-interval-days 0))
    (let ((interval-secs (* superchat-memory-auto-prune-interval-days 24 3600)))
      (setq superchat-memory--prune-timer
            (run-with-timer interval-secs interval-secs
                            (lambda () (superchat-memory-prune t))))
      (message "Superchat memory auto-pruning enabled every %d days."
               superchat-memory-auto-prune-interval-days))))

(defun superchat-memory-setup-auto-merge ()
  "Set up or cancel the automatic memory merging timer."
  (when (and superchat-memory--merge-timer (timerp superchat-memory--merge-timer))
    (cancel-timer superchat-memory--merge-timer)
    (setq superchat-memory--merge-timer nil))
  (when (and (integerp superchat-memory-auto-merge-interval-days)
             (> superchat-memory-auto-merge-interval-days 0))
    (let ((interval-secs (* superchat-memory-auto-merge-interval-days 24 3600)))
      (setq superchat-memory--merge-timer
            (run-with-timer interval-secs interval-secs
                            (lambda () (superchat-memory--auto-merge-all))))
      (message "Superchat memory auto-merging enabled every %d days."
               superchat-memory-auto-merge-interval-days))))

;;;; Insight Generation

(defcustom superchat-memory-insight-generator-llm-prompt
  (string-join
   '("You are a user analysis expert reviewing conversation summaries."
     "Below are several memory entries about the same topic."
     "Do NOT merge or summarize them. Instead, synthesize a higher-level insight about the user."
     "Focus on their long-term interests, skill level, personality, or recurring problems."
     "If no significant insight can be drawn, respond with only the word 'IGNORE'."
     "Otherwise, respond with a single JSON object with four keys:"
     "1. \"title\": A short title for the insight (e.g., 'Interest in Lisp Dialects', 'Struggles with Async Code')."
     "2. \"summary\": The full insight, explained in a few sentences."
     "3. \"keywords\": An array of 5-8 keywords related to the insight."
     "4. \"tags\": An array of 1-3 single-word tags, including the tag 'INSIGHT'."
     ""
     "Memory entries to analyze:"
     "---"
     "$content"
     "---")
   "\n")
  "Prompt template to ask an LLM to generate insights from related memories."
  :type 'string
  :group 'superchat-memory)

(defun superchat-memory-generate-insights (&optional callback)
  "Find related memories and generate a new 'insight' entry from them."
  (interactive)
  (let ((candidate-groups (superchat-memory--find-merge-candidates)))
    (if (not candidate-groups)
        (progn
          (message "No potential groups found to generate insights.")
          (when callback (funcall callback "No groups found.")))
      (let ((pending-insights (length candidate-groups))
            (created-count 0))
        (dolist (group candidate-groups)
          (let* ((content-to-analyze
                  (mapconcat (lambda (entry)
                               (format "--- Entry ID: %s ---\nTitle: %s\n\n%s"
                                       (plist-get entry :id)
                                       (plist-get entry :title)
                                       (plist-get entry :content)))
                             group "\n\n"))
                 (prompt (replace-regexp-in-string "\\$content" content-to-analyze superchat-memory-insight-generator-llm-prompt))
                 (response-parts '()))
            (gptel-request prompt
                           :stream t
                           :callback (lambda (response-or-signal &rest _)
                                       (if (stringp response-or-signal)
                                           (push response-or-signal response-parts)
                                         (when (eq response-or-signal t)
                                           (let ((full-response (string-join (nreverse response-parts) "")))
                                             (when (and full-response (not (string-equal "IGNORE" (string-trim full-response))))
                                               (let* ((json (json-parse-string full-response :object-type 'plist))
                                                      (title (plist-get json :title))
                                                      (summary (plist-get json :summary))
                                                      (keywords (if (vectorp (plist-get json :keywords)) (cl-coerce (plist-get json :keywords) 'list) nil))
                                                      (tags (if (vectorp (plist-get json :tags)) (cl-coerce (plist-get json :tags) 'list) nil)))
                                                 (if (and title summary)
                                                     (progn
                                                       (superchat-memory-add title summary :tier :tier2 :type "insight" :keywords keywords :tags (append tags '("INSIGHT")))
                                                       (cl-incf created-count))
                                                   (message "superchat-memory: LLM insight response was incomplete.")))))
                                           (when (zerop (cl-decf pending-insights))
                                             (let ((msg (format "Insight generation complete. Created %d new insights." created-count)))
                                               (message msg)
                                               (when callback (funcall callback msg)))))))))))
        (when (zerop pending-insights)
          (let ((msg (format "Insight generation complete. No new insights created.")))
            (message msg)
            (when callback (funcall callback msg)))))))

(defcustom superchat-memory-auto-insight-interval-days 7
  "Interval in days for automatic insight generation.\nIf set to nil or 0, auto-generation is disabled."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'superchat-memory
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'superchat-memory-setup-auto-insight)
           (superchat-memory-setup-auto-insight))))

(defvar superchat-memory--insight-timer nil "Timer for auto-generating insights.")

(defun superchat-memory-setup-auto-insight ()
  "Set up or cancel the automatic insight generation timer."
  (when (and superchat-memory--insight-timer (timerp superchat-memory--insight-timer))
    (cancel-timer superchat-memory--insight-timer)
    (setq superchat-memory--insight-timer nil))
  (when (and (integerp superchat-memory-auto-insight-interval-days)
             (> superchat-memory-auto-insight-interval-days 0))
    (let ((interval-secs (* superchat-memory-auto-insight-interval-days 24 3600)))
      (setq superchat-memory--insight-timer
            (run-with-timer interval-secs interval-secs
                            'superchat-memory-generate-insights))
      (message "Superchat memory auto-insight generation enabled every %d days."
               superchat-memory-auto-insight-interval-days))))

;; Initialize timers on load.
(superchat-memory-setup-auto-prune)
(superchat-memory-setup-auto-merge)
(superchat-memory-setup-auto-insight)


(provide 'superchat-memory)

;;; superchat-memory.el ends here
