;;; input-syntax-regression.el --- >skill, @model and /command in the middle of a message -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/proposed/2026-10-10-triggers-anywhere-in-the-message.md.
;; Every test reads a message through `superchat-core--parse-input', the one
;; place a message's `@model', `>skill' and `/command' are read.  No model
;; and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/input-syntax-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^input-syntax/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
;; The last test sends a real message; it borrows the request-context
;; suite's Session, scripted model and request reader.
(load (expand-file-name "m5-request-context-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-skills)
(require 'superchat-models)

(defvar superchat-skills-extra-directories)
(defvar superchat-automated-input)
(defvar superchat--user-commands)
(defvar superchat-manual-models)
(defvar superchat--model-list-cache)

(defmacro isx--with-names (&rest body)
  "Run BODY with two skills, two models and one command that exist.
Skills `planning' and `code-review', models `gpt-4o' and `qwen3:8b',
command `summarize'."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory
                (file-truename (make-temp-file "isx-skills-" t))))
          (superchat-skills-directory dir)
          (superchat-skills-extra-directories nil)
          (superchat-manual-models (list "gpt-4o" "qwen3:8b"))
          (superchat--model-list-cache nil)
          (superchat--user-commands (make-hash-table :test 'equal))
          (superchat-automated-input nil))
     (puthash "summarize" "Summarize: $input" superchat--user-commands)
     (dolist (name '("planning" "code-review"))
       (with-temp-file (expand-file-name (concat name ".md") dir)
         (insert "---\ndescription: test skill\n---\nBody of " name ".\n")))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun isx--parse (text)
  "Return TEXT read as one message."
  (superchat-core--parse-input (superchat-turn-new text "isx-session")))

(defun isx--plain-p (text)
  "Return non-nil when TEXT reads as ordinary text and is left unchanged."
  (let ((turn (isx--parse text)))
    (and (null (superchat-turn-skill turn))
         (null (superchat-turn-command turn))
         (null (superchat-turn-target-model turn))
         (equal (string-trim text) (superchat-turn-clean-input turn)))))

(ert-deftest input-syntax/a-skill-named-in-the-middle-is-invoked ()
  (isx--with-names
    (let ((turn (isx--parse "please >code-review look at this function")))
      (should (equal "code-review" (superchat-turn-skill turn)))
      (should (equal "please look at this function"
                     (superchat-turn-clean-input turn)))
      (should-not (superchat-turn-command turn)))))

(ert-deftest input-syntax/a-skill-named-at-the-end-is-invoked ()
  (isx--with-names
    (let ((turn (isx--parse "break this work into steps >planning")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should (equal "break this work into steps"
                     (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/a-model-named-in-the-middle-is-used ()
  (isx--with-names
    (let ((turn (isx--parse "explain this @qwen3:8b in two lines")))
      (should (equal "qwen3:8b" (superchat-turn-target-model turn)))
      (should (equal "explain this in two lines"
                     (superchat-turn-clean-input turn)))
      (should-not (superchat-turn-skill turn)))))

(ert-deftest input-syntax/a-command-named-in-the-middle-is-the-command-written-first ()
  (isx--with-names
    (let ((middle (isx--parse "the notes below /summarize are long"))
          (first (isx--parse "/summarize the notes below are long")))
      (should (equal "summarize" (superchat-turn-command middle)))
      (should (equal (superchat-turn-command-args first)
                     (superchat-turn-command-args middle)))
      (should (equal (superchat-turn-clean-input first)
                     (superchat-turn-clean-input middle))))))

(ert-deftest input-syntax/a-model-and-a-skill-in-the-middle-both-count ()
  (isx--with-names
    (let ((turn (isx--parse "look @gpt-4o at this >code-review now")))
      (should (equal "gpt-4o" (superchat-turn-target-model turn)))
      (should (equal "code-review" (superchat-turn-skill turn)))
      (should (equal "look at this now" (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/a-name-that-does-not-exist-stays-text ()
  (isx--with-names
    (dolist (text '("please >nosuchskill look at this"
                    "ask @alice about it"
                    "it lives in /nosuchcommand now"))
      (should (isx--plain-p text)))))

(ert-deftest input-syntax/ordinary-writing-is-not-a-trigger ()
  (isx--with-names
    (dolist (text '("is a > b here"
                    "he said\n> planning is hard\nand left"
                    "write to planning@gpt-4o.example now"
                    "see /usr/bin/summarize for it"
                    "the path a/summarize is old"
                    "x>planning y"
                    "say `>planning` in the docs"
                    "say `@gpt-4o` and `/summarize` in the docs"
                    "we need >planning, soon"))
      (should (isx--plain-p text)))))

(ert-deftest input-syntax/only-the-first-of-a-kind-counts ()
  (isx--with-names
    (let ((turn (isx--parse "first >planning then >code-review last")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should (equal "first then >code-review last"
                     (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/an-unknown-name-does-not-hide-a-later-known-one ()
  (isx--with-names
    (let ((turn (isx--parse "try >nosuchskill or >planning here")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should (equal "try >nosuchskill or here"
                     (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/lines-around-a-token-are-kept ()
  (isx--with-names
    (let ((turn (isx--parse "line one\n>planning\nline two")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should (equal "line one\n\nline two"
                     (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/a-skill-wins-over-a-command-in-the-same-text ()
  (isx--with-names
    (let ((turn (isx--parse "do >planning with /summarize too")))
      (should (equal "planning" (superchat-turn-skill turn)))
      (should-not (superchat-turn-command turn)))))

(ert-deftest input-syntax/a-command-at-the-start-keeps-a-later-skill-as-text ()
  (isx--with-names
    (let ((turn (isx--parse "/summarize the >planning notes")))
      (should (equal "summarize" (superchat-turn-command turn)))
      (should-not (superchat-turn-skill turn))
      (should (equal "the >planning notes" (superchat-turn-command-args turn)))
      (should (equal "/summarize the >planning notes"
                     (superchat-turn-clean-input turn))))))

(ert-deftest input-syntax/what-stood-at-the-start-reads-as-before ()
  (isx--with-names
    ;; At the start a name is not checked, as before.
    (let ((skill (isx--parse ">nosuchskill hello"))
          (model (isx--parse "@anything hello"))
          (both (isx--parse "@gpt-4o >code-review check this")))
      (should (equal "nosuchskill" (superchat-turn-skill skill)))
      (should (equal "hello" (superchat-turn-clean-input skill)))
      (should (equal "anything" (superchat-turn-target-model model)))
      (should (equal "hello" (superchat-turn-clean-input model)))
      (should (equal "gpt-4o" (superchat-turn-target-model both)))
      (should (equal "code-review" (superchat-turn-skill both)))
      (should (equal "check this" (superchat-turn-clean-input both))))))

(ert-deftest input-syntax/a-message-the-user-did-not-type-is-read-at-the-start-only ()
  (isx--with-names
    (let ((superchat-automated-input t))
      (dolist (text '("please >code-review look at this"
                      "explain this @qwen3:8b in two lines"
                      "the notes below /summarize are long"))
        (should (isx--plain-p text)))
      (should (equal "planning"
                     (superchat-turn-skill (isx--parse ">planning go")))))))

(ert-deftest input-syntax/a-model-name-is-never-fetched-to-read-a-message ()
  (isx--with-names
    (let ((superchat-manual-models nil)
          (superchat--model-list-cache nil)
          (asked nil))
      (cl-letf (((symbol-function 'superchat--get-available-models)
                 (lambda () (setq asked t) (list "gpt-4o"))))
        (should (isx--plain-p "explain this @gpt-4o in two lines"))
        (should-not asked)))))

(ert-deftest input-syntax/a-cached-model-list-is-enough ()
  (isx--with-names
    (let ((superchat-manual-models nil)
          (superchat--model-list-cache (cons (list "gpt-4o") (float-time))))
      (should (equal "gpt-4o"
                     (superchat-turn-target-model
                      (isx--parse "explain this @gpt-4o in two lines")))))))

(ert-deftest input-syntax/regenerate-refuses-a-skill-or-command-in-the-middle ()
  (isx--with-names
    (let ((superchat--session-id "isx-session"))
      (dolist (text '("please >code-review look at this"
                      "the notes below /summarize are long"))
        (should-error
         (superchat--superseded-resendable-text (list (list 1 2 3 text)))
         :type 'user-error))
      (should (equal "plain words"
                     (superchat--superseded-resendable-text
                      (list (list 1 2 3 "plain words"))))))))

;; The parse is one step; this follows a message to the model.
(ert-deftest input-syntax/the-model-gets-the-skill-and-the-text-without-the-token ()
  (m5rc--fixture '((:text "done"))
    (let* ((view (m5rc--open (m5rc--source)))
           (superchat-automated-input nil)
           (skill-file (expand-file-name "isx-skill.md"
                                         superchat-skills-directory)))
      (make-directory superchat-skills-directory t)
      (with-temp-file skill-file
        (insert "---\nname: isx-skill\ntype: prompt\n---\nISX_SKILL_BODY\n"))
      (should (superchat-skills-exists-p "isx-skill"))
      (smoke--send view "ISX_BEFORE >isx-skill ISX_AFTER")
      (let* ((request (m5rc--settle 1))
             (text (mapconcat #'identity (plist-get request :users) "\n")))
        (dolist (marker '("ISX_SKILL_BODY" "ISX_BEFORE" "ISX_AFTER"))
          (should (= 1 (m5rc--count marker text))))
        (should-not (string-match-p ">isx-skill" text))))))

(provide 'input-syntax-regression)

;;; input-syntax-regression.el ends here
