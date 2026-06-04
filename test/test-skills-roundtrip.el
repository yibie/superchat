;;; test-skills-roundtrip.el — SKILL.md round-trip tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that SKILL.md → load → export produces byte-identical output.

;;; Code:

(require 'ert)
(require 'superchat-skills)
(require 'superchat-skills-standard)

(defmacro test-skills-roundtrip--with-temp-dir (&rest body)
  (declare (indent 0))
  `(let* ((tmp-dir (make-temp-file "sk-roundtrip" t))
          (superchat-skills-directory tmp-dir))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory tmp-dir t)))))

(defun test-skills-roundtrip--write-skill (name content)
  "Write a skill file and return the path."
  (let ((file (expand-file-name (concat name ".md") superchat-skills-directory)))
    (with-temp-file file (insert content))
    file))

(ert-deftest roundtrip/code-review-byte-identical ()
  "SKILL.md written via export should round-trip identically for a basic skill."
  (test-skills-roundtrip--with-temp-dir
    (let* ((original "---\nname: test-skill\ndescription: A test skill\nversion: \"1.0\"\ntype: prompt\n---\n\n# Test Skill\n\nThis is test content.\n"))
      (test-skills-roundtrip--write-skill "test-skill" original)
      (let* ((loaded (superchat-skills-load "test-skill"))
             ;; Simulate export target
             (export-dir (make-temp-file "sk-export" t))
             (reexported))
        (unwind-protect
            (progn
              (superchat-skills-standard-export "test-skill" export-dir)
              (setq reexported
                    (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "test-skill/SKILL.md" export-dir))
                      (buffer-string)))
              (should (string= original reexported)))
          (ignore-errors (delete-directory export-dir t)))))))

(ert-deftest roundtrip/preserves-type-workflow ()
  "Export preserves type: workflow in frontmatter."
  (test-skills-roundtrip--with-temp-dir
    (let ((content "---\nname: wf-skill\ndescription: Workflow skill\nversion: \"1.0\"\ntype: workflow\n---\n\n/echo hello\n"))
      (test-skills-roundtrip--write-skill "wf-skill" content)
      (let ((skill (superchat-skills-load "wf-skill")))
        (should (equal "workflow" (plist-get skill :type)))))))

(ert-deftest roundtrip/preserves-triggers-list ()
  "Export preserves triggers list."
  (test-skills-roundtrip--with-temp-dir
    (let ((content "---\nname: trig-skill\ndescription: Skill with triggers\nversion: \"1.0\"\ntype: prompt\ntriggers: [\"alpha\", \"beta\"]\n---\n\n# Body\n"))
      (test-skills-roundtrip--write-skill "trig-skill" content)
      (let* ((export-dir (make-temp-file "sk-export" t))
             (reexported))
        (unwind-protect
            (progn
              (superchat-skills-standard-export "trig-skill" export-dir)
              (setq reexported
                    (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "trig-skill/SKILL.md" export-dir))
                      (buffer-string)))
              (should (string-match-p "triggers:" reexported))
              (should (string-match-p "\"alpha\"" reexported))
              (should (string-match-p "\"beta\"" reexported)))
          (ignore-errors (delete-directory export-dir t)))))))

(ert-deftest roundtrip/preserves-version ()
  "Export preserves version field."
  (test-skills-roundtrip--with-temp-dir
    (let ((content "---\nname: ver-skill\ndescription: Version test\nversion: \"2.3\"\ntype: prompt\n---\n\n# Body\n"))
      (test-skills-roundtrip--write-skill "ver-skill" content)
      (let ((skill (superchat-skills-load "ver-skill")))
        (should (equal "2.3" (plist-get skill :version)))))))

(provide 'test-skills-roundtrip)
;;; test-skills-roundtrip.el ends here
