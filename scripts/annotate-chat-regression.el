;;; annotate-chat-regression.el --- annotations asked for in a conversation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-annotations-asked-for-in-a-conversation.md.
;; A conversation message that names `>annotate' is answered like any other
;; message -- history, rules, the place the user is -- and its answer is also
;; hung, as remarks, on the buffer the conversation was called from or on the
;; buffer or file named after `>annotate'.  Every test sends real messages
;; through a conversation and reads what the scripted model received; it
;; borrows the request-context suite's Session, scripted model and reader.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/annotate-chat-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^annotate-chat/")'

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
(require 'superchat-annotate)

(defvar superchat-automated-input)

(defconst annc--text "alpha one\nbeta two\ngamma three\n"
  "Three lines, each with a word found on no other line.")

(defconst annc--answer "2 | beta | REMARK-B\n3 | gamma | REMARK-G"
  "An answer with a remark on line 2 and one on line 3.")

(defun annc--placed (buffer)
  "BUFFER's annotations as (LINE REMARK), in order."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (list (line-number-at-pos (overlay-start overlay))
                    (plist-get (overlay-get overlay 'superchat-annotate) :remark)))
            (superchat-annotate-overlays))))

(defun annc--last-user (request)
  "Return the newest user message of REQUEST."
  (car (last (plist-get request :users))))

(defun annc--tools (&optional n)
  "Names of the tools the Nth newest provider request offered (default newest)."
  (plist-get (nth (or n 0) smoke--requests) :tools))

(defun annc--form-p (request)
  "Return non-nil when REQUEST's system text asks for rows."
  (string-match-p "LINE | QUOTE | REMARK" (plist-get request :system)))

(defun annc--earlier-p (request)
  "Return non-nil when REQUEST's system text says an earlier answer was hung."
  (string-match-p "Earlier in this conversation the user sent a message ending in"
                  (plist-get request :system)))

(defun annc--agent (name)
  "Write an agent skill NAME whose persona is ANNC_PERSONA_NAME."
  (make-directory superchat-skills-directory t)
  (with-temp-file (expand-file-name (concat name ".md")
                                    superchat-skills-directory)
    (insert "---\nname: " name "\ntype: agent\n---\nANNC_PERSONA_" name "\n")))

;;;; The request and where the answer goes -------------------------------------

(ert-deftest annotate-chat/the-answer-is-hung-on-the-buffer-the-conversation-came-from ()
  (m5rc--fixture `((:text "FIRST_ANSWER") (:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil)
           (before (with-current-buffer source (buffer-string)))
           (tick (with-current-buffer source (buffer-chars-modified-tick))))
      (smoke--send view "ANNC_EARLIER_QUESTION")
      ;; A conversation that has not annotated says nothing about annotating.
      (should-not (annc--earlier-p (m5rc--settle 1)))
      ;; An ordinary message offers tools; the guard for "none" below.
      (should (annc--tools))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (let* ((request (m5rc--settle 2))
             (message (annc--last-user request)))
        ;; The text to annotate, numbered, once; the user's words without the mark.
        (should (= 1 (m5rc--count "1| alpha one\n2| beta two\n3| gamma three" message)))
        (should (= 1 (m5rc--count "ANNC_MARK_THEM" message)))
        (should-not (string-match-p ">annotate" message))
        ;; It is still a message of this conversation: history and place.
        (should (member "ANNC_EARLIER_QUESTION" (plist-get request :users)))
        (should (equal '("FIRST_ANSWER") (plist-get request :assistants)))
        (should (= 1 (m5rc--count "[Emacs origin]" message)))
        ;; The row form is the last thing the system text says.
        (should (annc--form-p request))
        (should-not (annc--earlier-p request))
        (should (> (string-match "LINE | QUOTE | REMARK" (plist-get request :system))
                   (string-match "M5RC_STATIC_SYSTEM_NOTE" (plist-get request :system))))
        ;; No tools.
        (should-not (annc--tools))
        ;; Hung on the source, whose text is untouched.
        (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source)))
        (with-current-buffer source
          (should (equal before (buffer-string)))
          (should (= tick (buffer-chars-modified-tick))))
        ;; And still shown in the conversation.
        (with-current-buffer view
          (should (string-match-p "2 | beta | REMARK-B"
                                  (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest annotate-chat/the-next-message-is-an-ordinary-one-again ()
  (m5rc--fixture `((:text ,annc--answer) (:text "PLAIN_ANSWER"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (smoke--send view "ANNC_PLAIN_NEXT")
      (let ((request (m5rc--settle 2)))
        (should-not (annc--form-p request))
        (should-not (string-match-p "<<<TEXT>>>" (annc--last-user request)))
        (should (annc--tools))
        ;; It is told the earlier answer was hung on that buffer and is done,
        ;; and the earlier message shows in its history with the mark.
        (should (annc--earlier-p request))
        (should (string-match-p (regexp-quote (format "(%s)" (buffer-name source)))
                                (plist-get request :system)))
        (should (member "ANNC_MARK_THEM >annotate" (plist-get request :users)))
        ;; The remarks stay where they were hung.
        (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source)))))))

(ert-deftest annotate-chat/a-message-of-the-mark-alone-asks-the-default ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view ">annotate")
      (let ((message (annc--last-user (m5rc--settle 1))))
        (should (= 1 (m5rc--count superchat-annotate-default-instruction message)))
        (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source)))))))

;;;; Naming what to annotate ---------------------------------------------------

(ert-deftest annotate-chat/a-buffer-named-after-the-mark-is-the-one-annotated ()
  (m5rc--fixture '((:text "1 | other | REMARK-O") (:text "PLAIN_ANSWER"))
    (let* ((source (m5rc--source annc--text))
           (other (generate-new-buffer "annc-other.txt"))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (unwind-protect
          (progn
            (with-current-buffer other (insert "the other text\n"))
            (smoke--send view (format "ANNC_LOOK >annotate %s" (buffer-name other)))
            (let ((message (annc--last-user (m5rc--settle 1))))
              (should (= 1 (m5rc--count "1| the other text" message)))
              (should-not (string-match-p "1| alpha one" message))
              (should (= 1 (m5rc--count "ANNC_LOOK" message)))
              (should-not (string-match-p (regexp-quote (concat "ANNC_LOOK " (buffer-name other)))
                                          message)))
            (should (equal '((1 "REMARK-O")) (annc--placed other)))
            (should-not (annc--placed source))
            ;; The record keeps the mark with the name, and a later message
            ;; is told which buffer it was.
            (smoke--send view "ANNC_PLAIN_NEXT")
            (let ((request (m5rc--settle 2)))
              (should (member (format "ANNC_LOOK >annotate \"%s\"" (buffer-name other))
                              (plist-get request :users)))
              (should (string-match-p (regexp-quote (format "(%s)" (buffer-name other)))
                                      (plist-get request :system)))))
        (kill-buffer other)))))

(ert-deftest annotate-chat/a-file-named-in-quotes-is-opened-and-annotated ()
  (m5rc--fixture '((:text "1 | file | REMARK-F"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (file (expand-file-name "annc notes.txt" superchat-data-directory))
           (superchat-automated-input nil))
      (with-temp-file file (insert "the file text\n"))
      (smoke--send view (format ">annotate \"%s\" ANNC_LOOK" file))
      (let ((message (annc--last-user (m5rc--settle 1))))
        (should (= 1 (m5rc--count "1| the file text" message))))
      (let ((visiting (find-buffer-visiting file)))
        (unwind-protect
            (progn
              (should (equal '((1 "REMARK-F")) (annc--placed visiting)))
              (should-not (buffer-modified-p visiting)))
          (when visiting (kill-buffer visiting)))))))

(ert-deftest annotate-chat/a-word-that-names-nothing-stays-part-of-the-message ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view ">annotate ANNC_NOT_A_BUFFER please")
      (let ((message (annc--last-user (m5rc--settle 1))))
        (should (= 1 (m5rc--count "ANNC_NOT_A_BUFFER please" message))))
      (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source))))))

(ert-deftest annotate-chat/a-quoted-name-that-names-nothing-sends-no-request ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (should-error (smoke--send view ">annotate \"annc-no-such-thing\" look")
                    :type 'user-error)
      (should (null m5rc--requests))
      (should (null (superchat-runtime-active-runs)))
      (should-not (annc--placed source)))))

;;;; What does not ask for annotations -----------------------------------------

(ert-deftest annotate-chat/a-message-superchat-sent-itself-is-never-annotations ()
  (m5rc--fixture '((:text "PLAIN_ANSWER"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input t))
      (smoke--send view "ANNC_AUTOMATED >annotate")
      (let ((request (m5rc--settle 1)))
        (should-not (annc--form-p request))
        (should (string-match-p ">annotate" (annc--last-user request))))
      (should-not (annc--placed source)))))

(ert-deftest annotate-chat/the-parser-reads-the-mark-only-where-it-stands-alone ()
  (cl-flet ((state (text &optional automated)
              (let ((superchat-automated-input automated)
                    (turn (superchat-turn-new text "s")))
                (superchat-core--parse-input turn)
                (list (plist-get (superchat-turn-state turn) :destination)
                      (superchat-turn-clean-input turn)))))
    (should (equal '(annotate "mark these") (state "mark these >annotate")))
    (should (equal '(annotate "mark these") (state ">annotate mark these")))
    (should (equal '(nil "mark>annotate these") (state "mark>annotate these")))
    (should (equal '(nil "mark >annotated these") (state "mark >annotated these")))
    (should (equal '(nil "mark `>annotate` these") (state "mark `>annotate` these")))
    (should (equal '(nil "mark these >annotate") (state "mark these >annotate" t)))
    ;; What opens the message wins.
    (should (null (car (state "/help >annotate"))))))

;;;; With a persona in force ----------------------------------------------------

(ert-deftest annotate-chat/under-an-agent-the-form-comes-last-and-no-tools-are-offered ()
  (m5rc--fixture `((:text "AGENT_ANSWER") (:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (annc--agent "annc-agent")
      (smoke--send view ">annc-agent ANNC_HELLO")
      (m5rc--settle 1)
      (let ((active (buffer-local-value 'superchat--active-preset view)))
        (should active)
        (smoke--send view "ANNC_MARK_THEM >annotate")
        (let* ((request (m5rc--settle 2))
               (system (plist-get request :system)))
          (should (string-match-p "ANNC_PERSONA_annc-agent" system))
          (should (> (string-match "LINE | QUOTE | REMARK" system)
                     (string-match "ANNC_PERSONA_annc-agent" system)))
          (should-not (annc--tools))
          (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source)))
          (should (eq active (buffer-local-value 'superchat--active-preset view))))))))

;;;; When things go wrong -------------------------------------------------------

(ert-deftest annotate-chat/a-buffer-killed-before-the-answer-is-said-not-signalled ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil)
           said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (push (apply #'format format-string args) said))
                   nil)))
        (smoke--send view "ANNC_MARK_THEM >annotate")
        (with-current-buffer source (set-buffer-modified-p nil))
        (kill-buffer source)
        (m5rc--settle 1))
      (should (cl-some (lambda (line) (string-match-p "no annotations: the buffer to annotate is gone" line))
                       said))
      ;; The answer is still in the conversation.
      (with-current-buffer view
        (should (string-match-p "2 | beta | REMARK-B"
                                (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest annotate-chat/an-answer-with-nothing-placed-is-said ()
  (m5rc--fixture '((:text "9 | nowhere | LOST\nnot a row"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil)
           said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (push (apply #'format format-string args) said))
                   nil)))
        (smoke--send view "ANNC_MARK_THEM >annotate")
        (m5rc--settle 1))
      (should-not (annc--placed source))
      (should (cl-some (lambda (line)
                         (and (string-match-p "1 not placed" line)
                              (string-match-p "1 rows not understood" line)
                              (string-match-p (regexp-quote (buffer-name source)) line)))
                       said)))))

(ert-deftest annotate-chat/asking-again-replaces-the-earlier-remarks ()
  (m5rc--fixture `((:text ,annc--answer) (:text "1 | alpha | REMARK-A"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (smoke--send view "ANNC_AGAIN >annotate")
      (m5rc--settle 2)
      (should (equal '((1 "REMARK-A")) (annc--placed source))))))

;;;; What a later message is told ---------------------------------------------

(ert-deftest annotate-chat/a-request-that-failed-is-not-said-to-have-hung-anything ()
  (m5rc--fixture '((:error (error . "ANNC_PROVIDER_DOWN")) (:text "PLAIN_ANSWER"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (should-not (annc--placed source))
      (smoke--send view "ANNC_PLAIN_NEXT")
      (should-not (annc--earlier-p (m5rc--settle 2))))))

(ert-deftest annotate-chat/an-answer-that-placed-nothing-is-not-said-to-have-hung-anything ()
  (m5rc--fixture '((:text "9 | nowhere | LOST") (:text "PLAIN_ANSWER"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (smoke--send view "ANNC_PLAIN_NEXT")
      (should-not (annc--earlier-p (m5rc--settle 2))))))

(ert-deftest annotate-chat/another-conversation-in-the-same-view-is-not-told ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (let ((session (buffer-local-value 'superchat--session-id view))
            (context (list :chat-buffer view)))
        ;; The conversation that annotated is told; any other Session shown
        ;; in the same View is not.
        (should (superchat-prompt-collector--annotate
                 (list :surface 'chat :session-id session :context context) nil nil))
        (should-not (superchat-prompt-collector--annotate
                     (list :surface 'chat :session-id "annc-other-session"
                           :context context)
                     nil nil))))))

(ert-deftest annotate-chat/the-request-can-be-rebuilt-from-the-record ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      ;; The same verdict the smoke reads for an ordinary message.
      (should (equal '(reproduced . 0)
                     (smoke--case-reconstruction
                      (buffer-local-value 'superchat--session-id view)))))))

;;;; Answering the message again ----------------------------------------------

(ert-deftest annotate-chat/regenerating-the-message-annotates-again ()
  (m5rc--fixture `((:text ,annc--answer) (:text "1 | alpha | REMARK-AGAIN"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (with-current-buffer view (superchat-regenerate))
      (let ((request (m5rc--settle 2)))
        (should (annc--form-p request))
        (should-not (annc--tools))
        (should (= 1 (m5rc--count "1| alpha one" (annc--last-user request))))
        (should-not (string-match-p ">annotate" (annc--last-user request))))
      (should (equal '((1 "REMARK-AGAIN")) (annc--placed source))))))

(ert-deftest annotate-chat/editing-the-message-puts-the-mark-back-in-the-prompt ()
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (with-current-buffer view
        (superchat-edit-last-message)
        (should (string-suffix-p
                 "ANNC_MARK_THEM >annotate"
                 (string-trim (buffer-substring-no-properties
                               (point-min) (point-max)))))))))

(provide 'annotate-chat-regression)

;;; annotate-chat-regression.el ends here
