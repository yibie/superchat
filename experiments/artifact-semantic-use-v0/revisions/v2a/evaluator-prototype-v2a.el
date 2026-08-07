;;; evaluator-prototype-v2a.el --- Thin CLI for V2a evaluator -*- lexical-binding: t; -*-

(load (expand-file-name
       "evaluator-logic-v2a.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun semuse-v2a-cli--fixture (text)
  "Return registered fixture named TEXT, or nil."
  (let ((name (and text (intern-soft text))))
    (and name (assq name semuse-v2a-audit-registry) name)))

(defun semuse-v2a-cli--print-result (result)
  "Print stable scorer RESULT."
  (princ (format "stage=%s\n" (plist-get result :stage)))
  (princ (format "status=%s\n" (plist-get result :status)))
  (princ (format "closure=%s\n" (plist-get result :closure)))
  (princ (format "errors=%S\n" (plist-get result :errors))))

(defun semuse-v2a-cli--audit (fixture-text)
  "Audit all fixtures, or one registered FIXTURE-TEXT."
  (let ((fixture (and fixture-text
                      (semuse-v2a-cli--fixture fixture-text))))
    (if (and fixture-text (not fixture))
        (progn
          (princ (format "error=unknown-fixture fixture=%s\n" fixture-text))
          (kill-emacs 2))
      (let ((result (semuse-v2a-audit fixture)))
        (princ (format "audit=%s\n"
                       (if (plist-get result :ok) "PASS" "FAIL")))
        (princ (format "fixtures=%d\n" (length (plist-get result :checks))))
        (dolist (check (plist-get result :checks))
          (let ((scored (plist-get check :result)))
            (princ
             (format "fixture=%s match=%s stage=%s status=%s errors=%S\n"
                     (plist-get check :name)
                     (if (plist-get check :ok) "PASS" "FAIL")
                     (plist-get scored :stage)
                     (plist-get scored :status)
                     (plist-get scored :errors)))))
        (kill-emacs (if (plist-get result :ok) 0 1))))))

(defun semuse-v2a-cli--score (fixture-text)
  "Score registered FIXTURE-TEXT through the public evaluator seam."
  (let ((fixture-name (semuse-v2a-cli--fixture fixture-text)))
    (if (not fixture-name)
        (progn
          (princ (format "error=unknown-fixture fixture=%s\n" fixture-text))
          (kill-emacs 2))
      (let* ((fixture (semuse-v2a--fixture fixture-name))
             (result
              (semuse-v2a-score
               (plist-get fixture :records)
               (plist-get fixture :evaluator-manifest))))
        (princ (format "score=%s\n"
                       (if (plist-get result :ok)
                           "COMPLETED"
                         "FAIL-CLOSED")))
        (princ (format "fixture=%s\n" fixture-name))
        (semuse-v2a-cli--print-result result)
        (kill-emacs (if (plist-get result :ok) 0 1))))))

(pcase command-line-args-left
  (`("audit") (semuse-v2a-cli--audit nil))
  (`("audit" ,fixture) (semuse-v2a-cli--audit fixture))
  (`("score" ,fixture) (semuse-v2a-cli--score fixture))
  (_
   (princ "usage: evaluator-prototype-v2a.el audit [FIXTURE]\n")
   (princ "       evaluator-prototype-v2a.el score FIXTURE\n")
   (kill-emacs 2)))

;;; evaluator-prototype-v2a.el ends here
