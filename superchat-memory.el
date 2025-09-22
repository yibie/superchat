;;; superchat-memory.el --- Memory management for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the memory system for Superchat, allowing it to store
;; and retrieve information from past conversations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
;; org-ql is optional - provide fallback if not available
(require 'org-ql nil t)
(require 'org-id)

;; This variable is defined in superchat.el
(defvar superchat-data-directory)

(defgroup superchat-memory nil
  "Configuration for Superchat's memory system."
  :group 'superchat)

(defcustom superchat-memory-file nil
  "The file where memories are stored.
If nil, defaults to `memory.org` inside `superchat-data-directory`."
  :type '(or nil string)
  :group 'superchat-memory)

(defun superchat-memory--get-file ()
  "Get the path to the memory file, computing default if necessary."
  (or superchat-memory-file
      (expand-file-name "memory.org" superchat-data-directory)))

(defun superchat-memory--ensure-file-exists ()
  "Ensure the memory file exists, creating it if necessary."
  (let ((file (superchat-memory--get-file)))
    (unless (file-exists-p file)
      (with-temp-buffer
        (write-file file)))))

(defun superchat-memory-add (title content &rest kwargs)
  "Add a new memory entry to the memory file.

TITLE is a string for the headline.
CONTENT is the body of the memory.
KWARGS is a plist that can contain :type and :tags."
  (let ((type (plist-get kwargs :type))
        (tags (plist-get kwargs :tags)))
    (superchat-memory--ensure-file-exists)
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (goto-char (point-max))
      ;; Ensure we start on a new line
      (unless (bolp) (insert "\n"))
      ;; Insert the headline
      (insert (format "* %s\n" title))
      ;; Insert PROPERTIES drawer
      (insert ":PROPERTIES:\n")
      (let ((org-id-method 'ts))
        (insert (format ":ID:       %s\n" (org-id-new))))
      (insert (format ":TIMESTAMP: %s\n" (format-time-string "[%Y-%m-%d %H:%M:%S]")))
      (when type
        (insert (format ":TYPE:     %s\n" (if (keywordp type) (substring (symbol-name type) 1) type))))
      (insert ":END:\n")
      ;; Insert TAGS
      (when tags
        (insert (format ":TAGS: &%s:\n" (mapconcat #'identity tags ":"))))
      ;; Insert a newline before the content
      (insert "\n")
      ;; Insert the content
      (insert content)
      ;; Ensure there are two newlines at the end for separation
      (unless (looking-at-p "\n\n$")
        (insert "\n\n"))
      (save-buffer)
      (message "Memory added: %s" title))))

(defun superchat-memory-retrieve (query-string)
  "Search for memories matching QUERY-STRING.
Returns a list of plists, where each plist represents a memory entry."
  (when (and (stringp query-string) (> (length query-string) 0))
    (if (featurep 'org-ql)
        (superchat-memory--retrieve-with-org-ql query-string)
      (superchat-memory--retrieve-fallback query-string))))

(defun superchat-memory--retrieve-with-org-ql (query-string)
  "Retrieve memories using org-ql (corrected version)."
  (let* ((keywords (split-string query-string "\\s-+" t))
         ;; Fixed: Use 'regexp instead of 'rifle, and proper query structure
         (query `(and ,@(mapcar (lambda (kw)
                                 `(or (regexp ,kw)
                                      (tags ,kw)))
                               keywords)))
         ;; Fixed: Use 'element-with-markers instead of 'marker
         (results (org-ql-select (superchat-memory--get-file) query
                                :action 'element-with-markers)))
    (mapcar #'superchat-memory--element-to-plist results)))

(defun superchat-memory--retrieve-fallback (query-string)
  "Fallback retrieval method when org-ql is not available."
  (let ((keywords (split-string query-string "\\s-+" t))
        (results '()))
    (with-current-buffer (find-file-noselect (superchat-memory--get-file))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (let* ((headline-start (point-at-bol))
                 (element (org-element-at-point))
                 (title (org-element-property :title element))
                 (content (superchat-memory--extract-content element))
                 (tags (org-element-property :tags element))
                 ;; Extract properties manually by parsing the property drawer
                 (properties (superchat-memory--extract-properties headline-start)))
            ;; Check if any keyword matches title, content, or tags
            (when (cl-some (lambda (kw)
                            (or (and title (string-match-p kw title))
                                (and content (string-match-p kw content))
                                (and tags (cl-some (lambda (tag) (string-match-p kw tag)) tags))))
                          keywords)
              (push (list :title (or title "")
                         :content (string-trim (or content ""))
                         :id (plist-get properties :id)
                         :timestamp (plist-get properties :timestamp)
                         :type (plist-get properties :type)
                         :tags tags)
                    results))))))
    (nreverse results)))

(defun superchat-memory--extract-properties (headline-start)
  "Extract properties from the property drawer starting at HEADLINE-START."
  (save-excursion
    (goto-char headline-start)
    (forward-line 1)
    (let ((properties '()))
      (when (looking-at "^[ \t]*:PROPERTIES:")
        (forward-line 1)
        (while (and (not (looking-at "^[ \t]*:END:"))
                    (not (eobp)))
          (when (looking-at "^[ \t]*:\\([^:]+\\):[ \t]*\\(.*\\)$")
            (let ((key (match-string 1))
                  (value (match-string 2)))
              (cond
               ((string= key "ID") (setq properties (plist-put properties :id value)))
               ((string= key "TIMESTAMP") (setq properties (plist-put properties :timestamp value)))
               ((string= key "TYPE") (setq properties (plist-put properties :type value))))))
          (forward-line 1)))
      properties)))

(defun superchat-memory--extract-content (element)
  "Extract content from ELEMENT, excluding property drawers and tags lines."
  (when (and (org-element-property :contents-begin element)
             (org-element-property :contents-end element))
    (let ((content-start (org-element-property :contents-begin element))
          (content-end (org-element-property :contents-end element)))
      (save-excursion
        (goto-char content-start)
        ;; Skip property drawer if present
        (when (looking-at "^[ \t]*:PROPERTIES:")
          (re-search-forward "^[ \t]*:END:" content-end t)
          (forward-line 1)
          (setq content-start (point)))
        ;; Skip tags line if present
        (when (looking-at "^[ \t]*:TAGS:")
          (forward-line 1)
          (setq content-start (point)))
        ;; Skip empty lines
        (while (and (< (point) content-end)
                    (looking-at "^[ \t]*$"))
          (forward-line 1)
          (setq content-start (point)))
        (when (< content-start content-end)
          (buffer-substring-no-properties content-start content-end))))))

(defun superchat-memory--element-to-plist (element)
  "Convert an org-ql result ELEMENT to a plist of memory properties."
  (let* ((title (org-element-property :title element))
         (content (and (org-element-contents element)
                      (with-temp-buffer
                        (insert (org-element-interpret-data (org-element-contents element)))
                        (string-trim (buffer-string)))))
         (properties (org-element-property :properties element)))
    (list :title (or title "")
          :content (or content "")
          :id (plist-get properties :ID)
          :timestamp (plist-get properties :TIMESTAMP)
          :type (plist-get properties :TYPE)
          :tags (org-element-property :tags element))))

(defun superchat-memory--marker-to-plist (marker)
  "Convert an org-ql result MARKER to a plist of memory properties."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let* ((element (org-element-at-point))
             (title (org-element-property :title element))
             (content (and (org-element-contents element)
                           (buffer-substring-no-properties
                            (org-element-property :contents-begin element)
                            (org-element-property :contents-end element))))
             (properties (org-element-property :properties element)))
        (list :title (or title "")
              :content (string-trim (or content ""))
              :id (plist-get properties :ID)
              :timestamp (plist-get properties :TIMESTAMP)
              :type (plist-get properties :TYPE)
              :tags (org-element-property :tags element))))))

(provide 'superchat-memory)

;;; superchat-memory.el ends here