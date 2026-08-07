;;; test-remediation-v0.3.el --- Tests for offline v0.3 scorer -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "remediation-logic-v0.3.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(ert-deftest semuse-v03/rejects-top-level-answer-key ()
  (let* ((fixture (semuse-v03--fixture-answer-key-top))
         (result
          (semuse-v03-cohort-result
           (plist-get fixture :records)
           (plist-get fixture :evaluator-manifest))))
    (should-not (plist-get result :ok))
    (should (eq (plist-get result :status) 'not-identifiable))
    (should (eq (plist-get result :stage) 'provider-envelope))))

(ert-deftest semuse-v03/legal-baseline-reaches-every-scorer-gate ()
  (let* ((fixture (semuse-v03--fixture-valid))
         (result
          (semuse-v03-cohort-result
           (plist-get fixture :records)
           (plist-get fixture :evaluator-manifest))))
    (should (plist-get result :ok))
    (should (eq (plist-get result :status)
                'consistent-with-two-input-use))
    (should (equal (mapcar #'car (plist-get result :gates))
                   '(shape structure record-set provider-envelope artifact
                     first-action content control)))))

(ert-deftest semuse-v03/fixtures-match-registered-verdicts ()
  (let ((audit (semuse-v03-audit)))
    (should (plist-get audit :ok))
    (should (= (length (plist-get audit :checks))
               (length semuse-v03-audit-registry)))
    (should (cl-every (lambda (check) (eq (plist-get check :ok) t))
                      (plist-get audit :checks)))))

(ert-deftest semuse-v03/sealed-truth-wins-over-v02-global-drift ()
  (let* ((fixture (semuse-v03--fixture 'global-truth-drift))
         (drifted-families (copy-tree semuse-v02-families t)))
    (plist-put (car drifted-families) :world-b "MUL-017-ZQ")
    (let* ((semuse-v02-families drifted-families)
           (semuse-v02-frozen-schedule nil)
           (semuse-v02-rows nil)
           (semuse-v02-conditions nil)
           (result
            (semuse-v03-cohort-result
             (plist-get fixture :records)
             (plist-get fixture :evaluator-manifest))))
      (should (plist-get result :ok))
      (should (eq (plist-get result :stage) 'scored))
      (should (eq (plist-get result :status) 'not-supported)))))

(provide 'test-remediation-v0.3)

;;; test-remediation-v0.3.el ends here
