;;; skill-once-regression.el --- an agent skill named in the middle applies to one message -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-an-agent-skill-for-one-message.md.
;; Every test sends real messages through a conversation and reads what the
;; scripted model received; it borrows the request-context suite's Session,
;; scripted model and request reader.  No real model and no protected
;; regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/skill-once-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^skill-once/")'

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
(defvar superchat--active-preset)

(defun sko--skill (name type &optional extra)
  "Write a skill NAME of TYPE whose text is SKO_PERSONA_NAME.
EXTRA is more frontmatter lines.  It may use `read_buffer' only."
  (make-directory superchat-skills-directory t)
  (with-temp-file (expand-file-name (concat name ".md")
                                    superchat-skills-directory)
    (insert "---\nname: " name "\ntype: " type "\n" (or extra "")
            "tools: [read_buffer]\n---\nSKO_PERSONA_" name "\n")))

(defun sko--active (view)
  "Return the name of VIEW's active preset, or nil."
  (with-current-buffer view
    (and superchat--active-preset
         (superchat-preset-name superchat--active-preset))))

(defun sko--activations (view)
  "Return how many times VIEW's conversation recorded a change of capability."
  (cl-count "capability.activated"
            (superchat-tape-replay-events
             (buffer-local-value 'superchat--session-id view))
            :key (lambda (event) (plist-get event :type))
            :test #'equal))

(defun sko--system (request)
  "Return REQUEST's system text."
  (or (plist-get request :system) ""))

(defun sko--tools ()
  "Return the tool names of the newest request the model received."
  (plist-get (car smoke--requests) :tools))

(ert-deftest skill-once/an-agent-skill-in-the-middle-shapes-that-message-only ()
  (m5rc--fixture '((:text "one") (:text "two"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil))
      (sko--skill "sko-agent" "agent")
      (let ((before (sko--activations view)))
        (smoke--send view "SKO_BEFORE >sko-agent SKO_AFTER")
        (let ((request (m5rc--settle 1)))
          ;; This message goes out as the skill: its text, its tools.
          (should (= 1 (m5rc--count "SKO_PERSONA_sko-agent" (sko--system request))))
          (should (equal '("read_buffer") (sko--tools)))
          (should (= 1 (m5rc--count "SKO_BEFORE SKO_AFTER"
                                    (car (last (plist-get request :users)))))))
        ;; The conversation did not change: neither what the View holds nor
        ;; what its record says.
        (should-not (sko--active view))
        (should (= before (sko--activations view)))
        (smoke--send view "SKO_NEXT")
        (let ((request (m5rc--settle 2)))
          (should (= 0 (m5rc--count "SKO_PERSONA" (sko--system request))))
          (should (member "edit_buffer" (sko--tools))))))))

(ert-deftest skill-once/an-agent-skill-with-its-own-capability-also-runs-once ()
  ;; Runtime refuses a request whose capability differs from its profile's.
  (m5rc--fixture '((:text "one") (:text "two"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil))
      (sko--skill "sko-cap" "agent" "capability: sko-capability\n")
      (let ((before (sko--activations view)))
        (smoke--send view "SKO_BEFORE >sko-cap SKO_AFTER")
        (let ((request (m5rc--settle 1)))
          (should (= 1 (m5rc--count "SKO_PERSONA_sko-cap" (sko--system request))))
          (should (equal '("read_buffer") (sko--tools))))
        (should-not (sko--active view))
        (should (= before (sko--activations view)))
        (smoke--send view "SKO_NEXT")
        (let ((request (m5rc--settle 2)))
          (should (= 0 (m5rc--count "SKO_PERSONA" (sko--system request)))))))))

(ert-deftest skill-once/an-agent-skill-at-the-start-still-switches-the-conversation ()
  (m5rc--fixture '((:text "one") (:text "two"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil))
      (sko--skill "sko-agent" "agent")
      (let ((before (sko--activations view)))
        (smoke--send view ">sko-agent SKO_FIRST")
        (m5rc--settle 1)
        (should (equal "sko-agent" (sko--active view)))
        (should (= (1+ before) (sko--activations view)))
        (smoke--send view "SKO_NEXT")
        (let ((request (m5rc--settle 2)))
          (should (= 1 (m5rc--count "SKO_PERSONA_sko-agent" (sko--system request))))
          (should (equal '("read_buffer") (sko--tools))))))))

(ert-deftest skill-once/a-plan-skill-in-the-middle-still-switches-the-conversation ()
  (m5rc--fixture '((:text "one"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil))
      (sko--skill "sko-plan" "plan")
      (smoke--send view "SKO_BEFORE >sko-plan SKO_AFTER")
      (m5rc--settle 1)
      (should (equal "sko-plan" (sko--active view))))))

(ert-deftest skill-once/inside-another-mode-the-one-message-skill-replaces-it-once ()
  (m5rc--fixture '((:text "one") (:text "two") (:text "three"))
    (let ((view (m5rc--open (m5rc--source)))
          (superchat-automated-input nil))
      (sko--skill "sko-home" "agent")
      (sko--skill "sko-guest" "agent")
      (smoke--send view ">sko-home SKO_FIRST")
      (m5rc--settle 1)
      (should (equal "sko-home" (sko--active view)))
      (let ((before (sko--activations view)))
        (smoke--send view "SKO_BEFORE >sko-guest SKO_AFTER")
        (let ((request (m5rc--settle 2)))
          (should (= 1 (m5rc--count "SKO_PERSONA_sko-guest" (sko--system request))))
          (should (= 0 (m5rc--count "SKO_PERSONA_sko-home" (sko--system request)))))
        (should (equal "sko-home" (sko--active view)))
        (should (= before (sko--activations view)))
        (smoke--send view "SKO_NEXT")
        (let ((request (m5rc--settle 3)))
          (should (= 1 (m5rc--count "SKO_PERSONA_sko-home" (sko--system request))))
          (should (= 0 (m5rc--count "SKO_PERSONA_sko-guest" (sko--system request)))))))))

(provide 'skill-once-regression)

;;; skill-once-regression.el ends here
