;;; open-items-probes.el --- Round 3 skill capability observations -*- lexical-binding: t; -*-
(load (expand-file-name "scripts/skill-once-regression.el" default-directory) nil t)
(defvar r3-open--mount-count 0)
(defun r3-open--collect (&rest _)
  (list (superchat-prompt-fragment :owner 'r3-open :id "r3-extra" :slot 'system
                                  :text "R3_PLUGIN_CAPABILITY_PRESENT" :budget-class 'hard)))
(superchat-define-plugin r3-open-plugin
  :apply (lambda (scope _)
           (cl-incf r3-open--mount-count)
           (superchat-context-register 'r3-extra #'r3-open--collect :owner 'r3-open :scope scope)))

(ert-deftest r3-open/one-message-skill-does-not-mount-declared-plugin ()
  (m5rc--fixture '((:text "ONCE") (:text "ACTIVE"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil)
          (r3-open--mount-count 0))
      (sko--skill "r3-plugin-skill" "agent" "plugins: [r3-open-plugin]\n")
      (smoke--send view "For this message >r3-plugin-skill do it")
      (let ((request (m5rc--settle 1)))
        (should (= 0 r3-open--mount-count))
        (should (string-match-p "SKO_PERSONA_r3-plugin-skill" (plist-get request :system)))
        (should-not (string-match-p "R3_PLUGIN_CAPABILITY_PRESENT" (plist-get request :system))))
      ;; Positive control: same skill at the command head actually mounts.
      (smoke--send view ">r3-plugin-skill do it")
      (let ((request (m5rc--settle 2)))
        (should (= 1 r3-open--mount-count))
        (should (string-match-p "R3_PLUGIN_CAPABILITY_PRESENT" (plist-get request :system))))
      (princ "OPEN-A OBSERVED: body skill applies persona but mount-count=0 and capability absent; head skill mount-count=1 and same capability reaches provider. Accepted limitation, not a new regression.\n"))))

(ert-run-tests-batch-and-exit "^r3-open/")
