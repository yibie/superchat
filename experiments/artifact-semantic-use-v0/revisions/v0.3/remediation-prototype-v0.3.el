;;; remediation-prototype-v0.3.el --- Thin CLI for offline scorer -*- lexical-binding: t; -*-

(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "remediation-logic-v0.3.el" directory) nil t))

(defun semuse-v03-cli--fixture-name (text)
  "Return registered fixture symbol named TEXT, or nil."
  (let ((name (and text (intern-soft text))))
    (and name (assq name semuse-v03-audit-registry) name)))

(defun semuse-v03-cli--usage ()
  "Print command usage."
  (princ "usage: remediation-prototype-v0.3.el audit [FIXTURE]\n")
  (princ "       remediation-prototype-v0.3.el score FIXTURE\n"))

(defun semuse-v03-cli--print-audit (result)
  "Print stable audit RESULT."
  (princ (format "audit=%s\n" (if (plist-get result :ok) "PASS" "FAIL")))
  (princ (format "fixtures=%d\n" (length (plist-get result :checks))))
  (dolist (check (plist-get result :checks))
    (let ((scored (plist-get check :result)))
      (princ
       (format "fixture=%s match=%s stage=%s status=%s errors=%S\n"
               (plist-get check :name)
               (if (plist-get check :ok) "PASS" "FAIL")
               (plist-get scored :stage)
               (plist-get scored :status)
               (plist-get scored :errors))))))

(defun semuse-v03-cli--audit (fixture-text)
  "Audit all fixtures, or the registered FIXTURE-TEXT."
  (let ((fixture (and fixture-text
                      (semuse-v03-cli--fixture-name fixture-text))))
    (if (and fixture-text (not fixture))
        (progn
          (princ (format "error=unknown-fixture fixture=%s\n" fixture-text))
          (kill-emacs 2))
      (let ((result (semuse-v03-audit fixture)))
        (semuse-v03-cli--print-audit result)
        (kill-emacs (if (plist-get result :ok) 0 1))))))

(defun semuse-v03-cli--score (fixture-text)
  "Score registered FIXTURE-TEXT through the public cohort seam."
  (let ((fixture-name (semuse-v03-cli--fixture-name fixture-text)))
    (if (not fixture-name)
        (progn
          (princ (format "error=unknown-fixture fixture=%s\n" fixture-text))
          (kill-emacs 2))
      (condition-case _err
          (let* ((fixture (semuse-v03--fixture fixture-name))
                 (result
                  (semuse-v03-cohort-result
                   (plist-get fixture :records)
                   (plist-get fixture :evaluator-manifest))))
            (princ (format "score=%s\n"
                           (if (plist-get result :ok)
                               "COMPLETED"
                             "FAIL-CLOSED")))
            (princ (format "fixture=%s\n" fixture-name))
            (princ (format "stage=%s\n" (plist-get result :stage)))
            (princ (format "status=%s\n" (plist-get result :status)))
            (princ (format "closure=%s\n" (plist-get result :closure)))
            (princ (format "errors=%S\n" (plist-get result :errors)))
            (kill-emacs (if (plist-get result :ok) 0 1)))
        (error
         (princ (format "score=ERROR\nfixture=%s\n" fixture-name))
         (kill-emacs 2))))))

(pcase command-line-args-left
  (`("audit") (semuse-v03-cli--audit nil))
  (`("audit" ,fixture) (semuse-v03-cli--audit fixture))
  (`("score" ,fixture) (semuse-v03-cli--score fixture))
  (_
   (semuse-v03-cli--usage)
   (kill-emacs 2)))

;;; remediation-prototype-v0.3.el ends here
