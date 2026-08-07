;;; compatibility-gate-v2a.el --- V2a scorer compatibility probe -*- lexical-binding: t; -*-

;; This throwaway probe answers one question only: can the frozen v0.3
;; scorer accept the V2a cohort shape without changing its contract?

(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../v0.3/remediation-logic-v0.3.el" directory)
        nil t))

(defun semuse-v2a-compatibility-audit ()
  "Return proof that frozen v0.3 cannot score the V2a cohort contract."
  (let* ((fixture (semuse-v03--fixture-valid))
         (records (plist-get fixture :records))
         (manifest (plist-get fixture :evaluator-manifest))
         (sixteen-records (cl-subseq records 0 16))
         (sixteen-manifest (copy-tree manifest t))
         (action-records (copy-tree records t))
         (first (car action-records)))
    (plist-put sixteen-manifest :schedule
               (cl-subseq (plist-get sixteen-manifest :schedule) 0 16))
    (plist-put (plist-get first :visible-inputs)
               :tool-schema "refresh_handle")
    (plist-put (plist-get first :first-action)
               :tool-name "refresh_handle")
    (let* ((checks
            (list
             (list :name 'sixteen-records
                   :result (semuse-v03-cohort-result sixteen-records manifest)
                   :stage 'record-set :error 'record-count)
             (list :name 'sixteen-manifest
                   :result
                   (semuse-v03-cohort-result
                    sixteen-records sixteen-manifest)
                   :stage 'structure :error 'sealed-manifest-sha256)
             (list :name 'refresh-first-action
                   :result (semuse-v03-cohort-result action-records manifest)
                   :stage 'first-action :error 'first-action-origin)))
           (ok
            (cl-every
             (lambda (check)
               (let ((result (plist-get check :result)))
                 (and (not (plist-get result :ok))
                      (eq (plist-get check :stage)
                          (plist-get result :stage))
                      (memq (plist-get check :error)
                            (plist-get result :errors)))))
             checks)))
      (list :ok ok :compatible nil :checks checks))))

(let ((result (semuse-v2a-compatibility-audit)))
  (princ (format "audit=%s\n" (if (plist-get result :ok) "PASS" "FAIL")))
  (princ "compatible=no\n")
  (dolist (check (plist-get result :checks))
    (let ((scored (plist-get check :result)))
      (princ
       (format "case=%s stage=%s errors=%S\n"
               (plist-get check :name)
               (plist-get scored :stage)
               (plist-get scored :errors)))))
  (kill-emacs (if (plist-get result :ok) 0 1)))

;;; compatibility-gate-v2a.el ends here
