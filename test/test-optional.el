;;; test-optional.el --- The optional package stays optional -*- lexical-binding: t; -*-

;;; Commentary:
;; S26 / approved decision 5: `(require 'superchat)' must load none of the
;; optional package's features, and the package's entry points must load
;; their files on demand once the package is required.

;;; Code:

(require 'ert)
(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))

(defconst test-optional--features
  '(superchat-genui superchat-emacsclient superchat-experience superchat-claims
    superchat-cowork-telega-session superchat-optional)
  "Features `(require \='superchat)' must not load.")

(ert-deftest optional/require-superchat-loads-no-optional-feature ()
  "A child Emacs with only the kernel on its load path requires `superchat'
without any optional feature becoming loaded."
  (let* ((emacs (or (executable-find invocation-name) (executable-find "emacs")))
         (root (file-name-directory (locate-library "superchat-runtime")))
         (form `(progn
                  (require 'superchat)
                  ;; Warnings may precede the result on stdout: mark it.
                  (princ "\nRESULT=")
                  (prin1 (mapcar (lambda (f) (cons f (featurep f)))
                                 ',test-optional--features)))))
    (unless emacs (ert-skip "Cannot find the current Emacs executable"))
    (with-temp-buffer
      (let ((status (apply #'call-process emacs nil t nil
                           (append (list "-Q" "--batch" "-L" root)
                                   (cl-mapcan (lambda (dir) (list "-L" dir))
                                              (cl-remove-if-not
                                               (lambda (dir)
                                                 (string-match-p "/llm-\\|/plz" dir))
                                               load-path))
                                   (list "--eval" (format "%S" form))))))
        (should (eql 0 status))
        (goto-char (point-min))
        (should (search-forward "RESULT=" nil t))
        (let ((result (read (buffer-substring (point) (point-max)))))
          (dolist (entry result)
            (should-not (cdr entry))))))))

(ert-deftest optional/package-registers-review-and-autoloads ()
  "Requiring the optional package registers `/review' and the entry points."
  (require 'superchat)
  (require 'superchat-optional)
  (should (assoc "review" superchat--command-alist))
  (dolist (fn '(superchat-emacsclient-run-status superchat-claim-capture
                superchat-experience-assemble-view superchat-genui-render-form))
    (should (fboundp fn))))

(provide 'test-optional)
;;; test-optional.el ends here
