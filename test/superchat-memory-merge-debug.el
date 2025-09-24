;;; superchat-memory-merge-debug.el --- Debug merging algorithm -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(package-initialize)

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." root)))

(require 'org)
(require 'subr-x)

(defvar superchat--user-commands (make-hash-table :test #'equal)
  "Stub user command registry for debug runs when superchat.el is absent.")

(require 'superchat-memory)

(unless (fboundp 'gptel-request)
  (defun gptel-request (&rest _args)
    (message "superchat-memory-merge-debug: stubbed gptel-request invoked")
    nil))

(unless (fboundp 'gptel-request-sync)
  (defun gptel-request-sync (&rest _args)
    (message "superchat-memory-merge-debug: stubbed gptel-request-sync invoked")
    "IGNORE"))

(defun superchat-memory-merge-debug--sample-memory ()
  "Return sample memory org contents used for debugging merges."
  (string-join
   '("* Deployment Runbook"
     ":PROPERTIES:"
     ":ID:       debug-entry-a"
     ":TIMESTAMP: [2024-05-01]"
     ":KEYWORDS: deployment, ops, release"
     ":TAGS:      OPERATIONS"
     ":END:"
     "Documenting deployment checklist and ownership hand-offs."
     ""
     "* Incident Review"
     ":PROPERTIES:"
     ":ID:       debug-entry-b"
     ":TIMESTAMP: [2024-05-02]"
     ":KEYWORDS: ops, release, postmortem"
     ":TAGS:      OPERATIONS"
     ":END:"
     "Summary of the last outage and mitigation playbook."
     ""
     "* Frontend UI polish"
     ":PROPERTIES:"
     ":ID:       debug-entry-c"
     ":TIMESTAMP: [2024-05-03]"
     ":KEYWORDS: frontend, ui, design"
     ":TAGS:      PRODUCT"
     ":END:"
     "Notes from a UI review covering layout adjustments."
     ""
     "* UX feedback sync"
     ":PROPERTIES:"
     ":ID:       debug-entry-d"
     ":TIMESTAMP: [2024-05-04]"
     ":KEYWORDS: design, ux, research"
     ":TAGS:      PRODUCT"
     ":END:"
     "User testing insights and qualitative feedback.")
   "\n"))

(defun superchat-memory-merge-debug--collect-entries (file)
  "Return active memory entries stored in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (entries)
       (while (re-search-forward org-heading-regexp nil t)
         (let* ((element (org-element-at-point))
                (entry (superchat-memory--element-to-plist element))
                (tags (plist-get entry :tags)))
           (unless (member "ARCHIVED" tags)
             (push entry entries))))
       (nreverse entries)))))

(defun superchat-memory-merge-debug--group-with-logging (entries threshold)
  "Return merge groups for ENTRIES logging pairwise scores versus THRESHOLD."
  (let ((groups '())
        (processed-ids '()))
    (dolist (entry1 entries)
      (let ((id1 (plist-get entry1 :id)))
        (unless (member id1 processed-ids)
          (message "\n== Base entry: %s (%s)" id1 (plist-get entry1 :title))
          (message "   Keywords: %s" (plist-get entry1 :keywords))
          (let ((group (list entry1)))
            (dolist (entry2 entries)
              (let ((id2 (plist-get entry2 :id)))
                (unless (or (equal id1 id2)
                            (member id2 processed-ids))
                  (let* ((keywords1 (plist-get entry1 :keywords))
                         (keywords2 (plist-get entry2 :keywords))
                         (score (superchat-memory--keyword-jaccard keywords1 keywords2)))
                    (message "   -> Compare with %s (%s)" id2 (plist-get entry2 :title))
                    (message "      Keywords: %s" keywords2)
                    (message "      Jaccard score: %.2f (threshold %.2f)" score threshold)
                    (if (>= score threshold)
                        (progn
                          (message "      => include in merge group")
                          (push entry2 group)
                          (push id2 processed-ids))
                      (message "      => skip"))))))
            (when (> (length group) 1)
              (message "-- Formed group with %d entries" (length group))
              (push group groups))
            (push id1 processed-ids)))))
    (nreverse groups)))

(defun superchat-memory-merge-debug-run ()
  "Generate debug output for the merge candidate discovery algorithm."
  (interactive)
  (let* ((temp-dir (make-temp-file "superchat-merge-debug" t))
         (superchat-data-directory temp-dir)
         (superchat-memory-file (expand-file-name "memory.org" temp-dir))
         (superchat-memory-merge-similarity-threshold 0.5))
    (with-temp-file superchat-memory-file
      (insert (superchat-memory-merge-debug--sample-memory)))
    (message "superchat-memory-merge-debug: created sample memory file at %s" superchat-memory-file)
    (let* ((entries (superchat-memory-merge-debug--collect-entries superchat-memory-file))
           (groups (superchat-memory-merge-debug--group-with-logging entries superchat-memory-merge-similarity-threshold)))
      (message "\n== Debug merge run complete ==")
      (if groups
          (dolist (group groups)
            (message "Group: %s"
                     (mapconcat
                      (lambda (entry)
                        (format "%s (%s)"
                                (plist-get entry :id)
                                (plist-get entry :title)))
                      group
                      ", ")))
        (message "No groups met the threshold."))
      (condition-case err
          (let ((actual (superchat-memory--find-merge-candidates)))
            (message "\n== superchat-memory--find-merge-candidates result ==")
            (if actual
                (dolist (group actual)
                  (message "Candidate group: %s"
                           (mapconcat
                            (lambda (entry)
                              (format "%s (%s)"
                                      (plist-get entry :id)
                                      (plist-get entry :title)))
                            group
                            ", ")))
              (message "Function returned no candidates.")))
        (error
         (message
          "superchat-memory-merge-debug: could not invoke original merge finder: %s"
          (error-message-string err)))))))

(provide 'superchat-memory-merge-debug)
;;; superchat-memory-merge-debug.el ends here

