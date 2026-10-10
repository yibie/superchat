;;; skill-context-regression.el --- a message that invokes a skill says where the user is -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-a-skill-message-says-where-the-user-is.md.
;; Every test sends real messages through a conversation and reads what the
;; scripted model received; it borrows the request-context suite's Session,
;; scripted model and request reader.  No real model and no protected
;; regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/skill-context-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^skill-context/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "m5-request-context-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-skills)

(defvar superchat-automated-input)

(defun skc--skill (name type)
  "Write a skill NAME of TYPE whose text is SKC_BODY_NAME."
  (make-directory superchat-skills-directory t)
  (with-temp-file (expand-file-name (concat name ".md")
                                    superchat-skills-directory)
    (insert "---\nname: " name "\ntype: " type "\n---\nSKC_BODY_" name "\n")))

(defun skc--last-user (request)
  "Return the newest user message of REQUEST."
  (car (last (plist-get request :users))))

(defun skc--says-where-p (message source)
  "Return non-nil when MESSAGE names SOURCE once and shows the windows once."
  (and (= 1 (m5rc--count "[Emacs origin]" message))
       (= 1 (m5rc--count (concat "Buffer: " (buffer-name source)) message))
       (= 1 (m5rc--count "[Emacs windows]" message))))

(ert-deftest skill-context/a-plain-message-says-where-the-user-is ()
  ;; The guard the other tests are measured against.
  (m5rc--fixture '((:text "one"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "SKC_PLAIN")
      (should (skc--says-where-p (skc--last-user (m5rc--settle 1)) source)))))

(ert-deftest skill-context/a-prompt-skill-message-says-where-the-user-is ()
  (m5rc--fixture '((:text "one"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (skc--skill "skc-prompt" "prompt")
      (smoke--send view ">skc-prompt SKC_ASK")
      (let ((message (skc--last-user (m5rc--settle 1))))
        (should (skc--says-where-p message source))
        ;; The skill's own text and the user's words are still there, once.
        (should (= 1 (m5rc--count "SKC_BODY_skc-prompt" message)))
        (should (= 1 (m5rc--count "SKC_ASK" message)))))))

(ert-deftest skill-context/an-agent-skill-at-the-start-says-where-the-user-is ()
  (m5rc--fixture '((:text "one"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (skc--skill "skc-agent" "agent")
      (smoke--send view ">skc-agent SKC_ASK")
      (let ((message (skc--last-user (m5rc--settle 1))))
        (should (skc--says-where-p message source))
        (should (= 1 (m5rc--count "SKC_ASK" message)))))))

(ert-deftest skill-context/an-agent-skill-in-the-middle-says-where-the-user-is ()
  (m5rc--fixture '((:text "one"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (skc--skill "skc-agent" "agent")
      (smoke--send view "SKC_BEFORE >skc-agent SKC_AFTER")
      (let ((message (skc--last-user (m5rc--settle 1))))
        (should (skc--says-where-p message source))
        (should (= 1 (m5rc--count "SKC_BEFORE SKC_AFTER" message)))))))

(ert-deftest skill-context/a-skill-message-carries-the-selected-text ()
  (m5rc--fixture '((:text "one"))
    (let* ((source (m5rc--source "(message \"SKC_SELECTED_TEXT\")\n" t))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (skc--skill "skc-prompt" "prompt")
      (smoke--send view ">skc-prompt SKC_ASK")
      (let ((message (skc--last-user (m5rc--settle 1))))
        (should (= 1 (m5rc--count "SKC_SELECTED_TEXT" message)))))))

(provide 'skill-context-regression)

;;; skill-context-regression.el ends here
