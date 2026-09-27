;;; skill-catalog-regression.el --- the model finds skills itself -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-09-27-skills-the-model-finds-itself.md.  The
;; canonical smoke fixture supplies the Session and a scripted model; no real
;; model and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/skill-catalog-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^skill-catalog/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-skills)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar superchat-skills-extra-directories)

(defun sc--write (dir relative content)
  (let ((file (expand-file-name relative dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))
    file))

(defmacro sc--with-skills (spec &rest body)
  "Bind `superchat-skills-directory' to a fresh SKILLS dir (and EXTRA) and run BODY.
SPEC is (SKILLS EXTRA)."
  (declare (indent 1) (debug t))
  `(let* ((,(car spec) (file-name-as-directory (file-truename (make-temp-file "sc-skills-" t))))
          (,(cadr spec) (file-name-as-directory (file-truename (make-temp-file "sc-extra-" t))))
          (superchat-skills-directory ,(car spec))
          (superchat-skills-extra-directories nil))
     (unwind-protect (progn ,@body)
       (delete-directory ,(car spec) t)
       (delete-directory ,(cadr spec) t))))

(defun sc--populate (skills)
  (sc--write skills "journal/SKILL.md"
             "---\nname: denote-journal\ndescription: |\n  Keep a daily journal with denote-journal.\n  Use it for today's entry.\n---\n\nSC-JOURNAL-BODY: use denote-journal-new-or-existing-entry.\n")
  (sc--write skills "group/deep/SKILL.md"
             "---\nname: deep-one\ndescription: A skill two folders down.\n---\nSC-DEEP-BODY\n")
  (sc--write skills "described.md"
             "---\ndescription: A single-file skill with a description.\n---\nSC-DESCRIBED-BODY\n")
  (sc--write skills "bare.md" "No frontmatter here.\n")
  (sc--write skills "mode.md" "---\ntype: agent\ndescription: Switches the session into a mode.\n---\nPersona.\n")
  (sc--write skills "hidden/SKILL.md"
             "---\nname: hidden\ndescription: Only for the person.\ndisable-model-invocation: true\n---\nSC-HIDDEN-BODY\n"))

(defun sc--names ()
  (mapcar (lambda (skill) (plist-get skill :name)) (superchat-skills-catalog)))

;;;; What is found and listed ---------------------------------------------------

(ert-deftest skill-catalog/finds-skill-folders-and-described-files ()
  (skip-when (getenv "SMOKE_REAL"))
  (sc--with-skills (skills _extra)
    (sc--populate skills)
    (should (fboundp 'superchat-skills-catalog))
    (should (equal '("deep-one" "denote-journal" "described") (sort (sc--names) #'string<)))
    (let ((journal (cl-find "denote-journal" (superchat-skills-catalog)
                            :key (lambda (s) (plist-get s :name)) :test #'equal)))
      ;; A block scalar is read whole, not as "|".
      (should (equal "Keep a daily journal with denote-journal.\nUse it for today's entry."
                     (plist-get journal :description)))
      (should (file-name-absolute-p (plist-get journal :location)))
      (should (equal (file-truename (expand-file-name "journal/SKILL.md" skills))
                     (file-truename (plist-get journal :location)))))))

(ert-deftest skill-catalog/extra-folders-only-when-set-and-the-first-name-wins ()
  (skip-when (getenv "SMOKE_REAL"))
  (sc--with-skills (skills extra)
    (sc--populate skills)
    (sc--write extra "other/SKILL.md" "---\nname: other\ndescription: From another agent's folder.\n---\nSC-OTHER\n")
    (sc--write extra "journal-copy/SKILL.md" "---\nname: denote-journal\ndescription: A second copy.\n---\nSC-COPY\n")
    (should (fboundp 'superchat-skills-catalog))
    (should-not (member "other" (sc--names)))
    (let ((superchat-skills-extra-directories (list extra)))
      (should (member "other" (sc--names)))
      (should (equal 1 (cl-count "denote-journal" (sc--names) :test #'equal)))
      (should (string-prefix-p "Keep a daily journal"
                               (plist-get (cl-find "denote-journal" (superchat-skills-catalog)
                                                   :key (lambda (s) (plist-get s :name)) :test #'equal)
                                          :description))))))

(ert-deftest skill-catalog/a-description-is-cut-at-1024 ()
  (skip-when (getenv "SMOKE_REAL"))
  (sc--with-skills (skills _extra)
    (sc--write skills "long/SKILL.md" (concat "---\nname: long\ndescription: " (make-string 3000 ?d) "\n---\nbody\n"))
    (should (fboundp 'superchat-skills-catalog))
    (should (= 1024 (length (plist-get (car (superchat-skills-catalog)) :description))))))

(ert-deftest skill-catalog/the-text-is-pi-s-shape ()
  (skip-when (getenv "SMOKE_REAL"))
  (sc--with-skills (skills _extra)
    (sc--populate skills)
    (sc--write skills "esc/SKILL.md" "---\nname: esc\ndescription: Uses <b> & friends.\n---\nbody\n")
    (should (fboundp 'superchat-skills-catalog-text))
    (let ((text (superchat-skills-catalog-text)))
      (should (string-match-p "read_file" text))
      (should (string-match-p "names the skill" text))
      (should (string-match-p "relative path" text))
      (should (string-match-p "<available_skills>" text))
      (should (string-match-p "<name>denote-journal</name>" text))
      (should (string-match-p (regexp-quote (format "<location>%s</location>"
                                                    (expand-file-name "journal/SKILL.md" skills)))
                              text))
      (should (string-match-p (regexp-quote "Uses &lt;b&gt; &amp; friends.") text))
      (should-not (string-match-p "SC-JOURNAL-BODY" text))
      (should-not (string-match-p "<name>hidden</name>" text)))
    ;; Nothing to list: no text.
    (let ((superchat-skills-directory (file-name-as-directory (make-temp-file "sc-empty-" t))))
      (should (member (superchat-skills-catalog-text) '(nil ""))))))

;;;; What a request carries ---------------------------------------------------------

(defmacro sc--fixture (script &rest body)
  "Run BODY in the smoke fixture.  The fixture binds its own
`superchat-data-directory' and `superchat-skills-directory', so a test sets
the folders it means inside BODY."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tools-list nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(ert-deftest skill-catalog/a-request-with-read-file-carries-it ()
  (sc--with-skills (skills _extra)
    (sc--populate skills)
    (sc--fixture (list '(:text "one") '(:text "two") '(:text "three"))
      (let ((view (smoke--open-chat))
            (superchat-skills-directory skills))
        (let ((run (smoke--send view "hello")))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
        (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
          (should (member "read_file" (plist-get (car smoke--requests) :tools)))
          (should (string-match-p "<available_skills>" prompt))
          (should (string-match-p "<name>denote-journal</name>" prompt)))
        ;; A request that does not offer read_file does not list skills.
        (let ((superchat-llm-tool-names (remove "read_file" superchat-llm-default-tool-names))
              (superchat-llm-tools-list nil))
          (let ((run (smoke--send view "again")))
            (should (smoke--wait (lambda () (smoke--run-done-p run)) 20)))
          (should-not (member "read_file" (plist-get (car smoke--requests) :tools)))
          (should-not (string-match-p "<available_skills>"
                                      (plist-get (car smoke--requests) :prompt-text))))))))

(ert-deftest skill-catalog/no-skills-no-block ()
  "Guard: with nothing to list the request is as before."
  (sc--with-skills (skills _extra)
    (sc--fixture (list '(:text "one"))
      (let* ((superchat-skills-directory skills)
             (view (smoke--open-chat))
             (run (smoke--send view "hello")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (should-not (string-match-p "available_skills"
                                    (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest skill-catalog/the-model-reads-a-listed-skill-with-read-file ()
  "Guard: read_file reads a skill, also inside Superchat's data directory,
which is where the skills directory lives by default."
  (sc--with-skills (skills _extra)
    (sc--populate skills)
    (let ((location (expand-file-name "journal/SKILL.md" skills)))
      (sc--fixture (list `(:tool-uses (("read_file" (path . ,location))))
                         '(:text "read it"))
        (let* ((superchat-data-directory skills)
               (superchat-skills-directory skills)
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (smoke--send view "use the journal skill")))
          (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
          (should (cl-find-if (lambda (event)
                                (and (equal "tool.completed" (plist-get event :type))
                                     (string-match-p "SC-JOURNAL-BODY"
                                                     (or (plist-get (plist-get event :payload) :result-text) ""))))
                              (superchat-tape-replay-events session))))))))

;;;; >name ------------------------------------------------------------------------------

(ert-deftest skill-catalog/gt-name-finds-a-skill-folder ()
  (skip-when (getenv "SMOKE_REAL"))
  (sc--with-skills (skills _extra)
    (sc--populate skills)
    ;; Still true: a single-file skill.
    (should (superchat-skills-exists-p "described"))
    ;; The gap: a folder with SKILL.md, by its folder's name or its own name.
    (should (superchat-skills-exists-p "journal"))
    (should (string-match-p "SC-JOURNAL-BODY"
                            (superchat-preset-skill-body (superchat-skills-load "journal"))))))

(provide 'skill-catalog-regression)

;;; skill-catalog-regression.el ends here
