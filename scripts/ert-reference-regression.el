;;; ert-reference-regression.el --- the ERT reference page -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-09-29-ert-reference-page.md:
;; a reference page on running a file's ERT tests in the user's Emacs.  The
;; page's own load and run forms are read from its text and evaluated against
;; a fixture, so the recipe the model reads is the recipe that is tested.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/ert-reference-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^ert-reference/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'superchat-reference)

(defconst ertref--id "emacs-ert-tests")

(defun ertref--form-at (body start)
  "Read the Lisp form in BODY that begins with the text START."
  (let ((pos (string-search start body)))
    (should pos)
    (car (read-from-string body pos))))

(defun ertref--substitute (form pairs)
  "FORM with each string that is `equal' to a car of PAIRS replaced by its cdr."
  (cl-sublis pairs form :test #'equal))

(ert-deftest ert-reference/the-page-is-there-within-limits ()
  (let ((body (superchat-reference-body ertref--id)))
    (should (stringp body))
    (should (<= (length body) 4096))
    (should (<= (length (superchat-reference-summary ertref--id)) 120))
    (should (<= (length (superchat-reference-index-text)) 600))
    (should (string-match-p "This page is advice" body))
    (should (member ertref--id (superchat-reference-ids)))))

(ert-deftest ert-reference/its-errors-point-at-the-page ()
  (should (equal ertref--id
                 (superchat-reference-pointer-for
                  "eval_elisp"
                  "Error: evaluating expression: This function is only for use in batch mode")))
  (should (equal ertref--id
                 (superchat-reference-pointer-for
                  "eval_elisp"
                  "Error: evaluating expression: Symbol’s function definition is void: ert-stats-completed-unexpected-tests")))
  ;; Guard: another void function is not an ERT question.
  (should-not (superchat-reference-pointer-for
               "eval_elisp" "Symbol’s function definition is void: foo-bar")))

(ert-deftest ert-reference/the-page-recipe-runs ()
  "The page's load and run forms, evaluated against a fixture project."
  (let* ((dir (file-name-as-directory (make-temp-file "ertref" t)))
         (test-file (expand-file-name "ertref-demo-test.el" dir))
         (body (superchat-reference-body ertref--id)))
    (should (stringp body))
    (with-temp-file (expand-file-name "ertref-demo.el" dir)
      (insert "(defun ertref-demo-add (a b) (+ a b))\n"
              "(defun ertref-demo-div (a b) (if (zerop b) nil (/ a b)))\n"
              "(provide 'ertref-demo)\n"))
    (with-temp-file test-file
      (insert "(require 'ert)\n(require 'ertref-demo)\n"
              "(ert-deftest ertref-demo-test-add () (should (= (ertref-demo-add 1 2) 3)))\n"
              "(ert-deftest ertref-demo-test-div () (should (= (ertref-demo-div 6 3) 2)))\n"
              "(ert-deftest ertref-demo-test-div-zero () (should (eq (ertref-demo-div 1 0) 0)))\n"
              "(ert-deftest ertref-demo-test-skip () (skip-unless nil) (should t))\n"))
    (unwind-protect
        (let* ((pairs `(("~/proj" . ,dir)
                        ("~/proj/foo-test.el" . ,test-file)
                        ("^foo-test-" . "^ertref-demo-test-")))
               (load-form (ertref--substitute (ertref--form-at body "(let ((load-path") pairs))
               (run-form (ertref--substitute (ertref--form-at body "(let ((stats") pairs)))
          (eval load-form t)
          (should (featurep 'ertref-demo))
          (let* ((result (eval run-form t))
                 (tests (plist-get result :tests)))
            (should (= 4 (plist-get result :total)))
            (should (= 1 (plist-get result :unexpected)))
            (should (= 1 (plist-get result :skipped)))
            (should (equal '(ertref-demo-test-add ert-test-passed)
                           (seq-take (assq 'ertref-demo-test-add tests) 2)))
            (should (equal '(ertref-demo-test-div-zero ert-test-failed)
                           (seq-take (assq 'ertref-demo-test-div-zero tests) 2)))
            (should (string-match-p ":form (eq nil 0)"
                                    (nth 2 (assq 'ertref-demo-test-div-zero tests))))
            (should (eq 'ert-test-skipped
                        (nth 1 (assq 'ertref-demo-test-skip tests))))
            ;; Plain data: it prints without a circular structure.
            (should (stringp (prin1-to-string result)))))
      (dolist (name '(ertref-demo-test-add ertref-demo-test-div
                      ertref-demo-test-div-zero ertref-demo-test-skip))
        (when (ert-test-boundp name) (ert-make-test-unbound name)))
      (delete-directory dir t))))

(provide 'ert-reference-regression)

;;; ert-reference-regression.el ends here
