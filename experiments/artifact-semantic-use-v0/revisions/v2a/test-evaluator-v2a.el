;;; test-evaluator-v2a.el --- V2a evaluator contract tests -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "evaluator-logic-v2a.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest semuse-v2a/valid-sixteen-record-cohort-is-supported ()
  (let* ((fixture (semuse-v2a--fixture 'valid-baseline))
         (result
          (semuse-v2a-score
           (plist-get fixture :records)
           (plist-get fixture :evaluator-manifest))))
    (should (plist-get result :ok))
    (should (= 16 (plist-get result :assignment-count)))
    (should (eq 'consistent-with-candidate-specific-action
                (plist-get result :status)))))

(ert-deftest semuse-v2a/wrong-but-well-formed-action-is-not-supported ()
  (let* ((fixture (semuse-v2a--fixture 'wrong-action))
         (result
          (semuse-v2a-score
           (plist-get fixture :records)
           (plist-get fixture :evaluator-manifest))))
    (should (plist-get result :ok))
    (should (eq 'not-supported (plist-get result :status)))))

(ert-deftest semuse-v2a/all-broken-join-fixtures-fail-closed ()
  (let ((audit (semuse-v2a-audit)))
    (should (plist-get audit :ok))
    (should (= 10 (length (plist-get audit :checks))))
    (should (cl-every (lambda (check) (plist-get check :ok))
                      (plist-get audit :checks)))))

(provide 'test-evaluator-v2a)

;;; test-evaluator-v2a.el ends here
