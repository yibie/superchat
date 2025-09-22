;;; superchat-memory-debug.el --- Debug version with logging -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'org-ql)
(require 'org-id)

;; This variable is defined in superchat.el
(defvar superchat-data-directory)

(defun superchat-memory-retrieve-debug (query-string)
  "Debug version of superchat-memory-retrieve with extensive logging."
  (when (and (stringp query-string) (> (length query-string) 0))
    (let* ((keywords (split-string query-string "\\s-+" t))
           (query `(and ,@(mapcar (lambda (kw)
                                   `(or (rifle ,kw)
                                        (tags ,kw)))
                                 keywords)))
           (file-path (or superchat-memory-file
                         (expand-file-name "memory.org" superchat-data-directory))))
      
      ;; Debug logging
      (message "DEBUG: Query string: %s" query-string)
      (message "DEBUG: Keywords: %s" keywords)
      (message "DEBUG: Constructed query: %s" query)
      (message "DEBUG: File path: %s" file-path)
      (message "DEBUG: File exists: %s" (file-exists-p file-path))
      
      ;; Test different action parameters
      (condition-case err
          (let ((results-marker (org-ql-select (list file-path) query 'marker)))
            (message "DEBUG: Results with 'marker action: %s" results-marker)
            results-marker)
        (error (message "DEBUG: Error with 'marker action: %s" err)))
      
      (condition-case err
          (let ((results-element (org-ql-select (list file-path) query 'element)))
            (message "DEBUG: Results with 'element action: %s" (length results-element))
            results-element)
        (error (message "DEBUG: Error with 'element action: %s" err)))
      
      (condition-case err
          (let ((results-element-markers (org-ql-select (list file-path) query 'element-with-markers)))
            (message "DEBUG: Results with 'element-with-markers action: %s" (length results-element-markers))
            results-element-markers)
        (error (message "DEBUG: Error with 'element-with-markers action: %s" err)))
      
      ;; Test different query syntax
      (condition-case err
          (let* ((alt-query `(and ,@(mapcar (lambda (kw)
                                             `(or (regexp ,kw)
                                                  (tags ,kw)))
                                           keywords)))
                 (results-alt (org-ql-select (list file-path) alt-query 'element-with-markers)))
            (message "DEBUG: Alternative query with 'regexp: %s" alt-query)
            (message "DEBUG: Results with alternative query: %s" (length results-alt))
            results-alt)
        (error (message "DEBUG: Error with alternative query: %s" err))))))

(provide 'superchat-memory-debug)