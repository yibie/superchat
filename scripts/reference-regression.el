;;; reference-regression.el --- Reference reading on demand -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract: docs/decisions/accepted/2026-09-20-reference-reading-on-demand.md
;; These tests are the MECHANISM acceptance only: a page can be discovered and
;; read, its text says what the contract requires, and reading it leaves the
;; permissions and the task as they were.  That a real model reads a page when
;; it should, and does better for it, is NOT pinned here: that is the separate
;; utility acceptance.
;;
;;   emacs -Q --batch ... -l scripts/reference-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^reference/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-reference nil t)

(defconst ref--index-marker "Reference pages"
  "The first words of the index block.")
(defconst ref--page "emacs-keybindings")
(defconst ref--pointer "read_reference"
  "What a pointer sentence names; the page id stands next to it.")

(defvar ref--offered nil
  "The tool list of the fixture: the default surface and the reading tool.
`read_reference' left the default surface on 2026-10-27; these tests are
about a request that offers it, except where they say otherwise.")

(defvar ref--systems nil)
(defvar ref--asked nil
  "Every approval request of the fixture.  Reading a page must add none.")

(defmacro ref--fixture (script &rest body)
  "Run BODY with SCRIPT; record each request's system text and every question."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((ref--systems nil)
           (ref--asked nil)
           (superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-approval-function (lambda (request)
                                          (push (plist-get request :tool) ref--asked)
                                          t))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-llm-tool-names
            (or ref--offered
                (append superchat-llm-default-tool-names
                        '("read_reference" "list_buffers"))))
           (superchat-llm-tools-list nil))
       (unwind-protect
           (smoke--with-fixture ,script
             (let ((network (symbol-function 'smoke--fake-network)))
               (cl-letf (((symbol-function 'smoke--fake-network)
                          (lambda (mode provider prompt partial response error multi)
                            (push (copy-sequence (or (llm-chat-prompt-context prompt) ""))
                                  ref--systems)
                            (funcall network mode provider prompt
                                     partial response error multi))))
                 ,@body)))
         (setq superchat-llm-tools-list nil)))))

(defun ref--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun ref--count (needle text)
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq start (match-end 0) count (1+ count)))
    count))

(defun ref--request (n)
  "The Nth provider request of the fixture, oldest first."
  (nth n (reverse smoke--requests)))

(defun ref--submit (view tools content)
  (plist-get (superchat-service-call
              nil 'driver 'submit
              (list :surface 'chat
                    :session-id (buffer-local-value 'superchat--session-id view)
                    :tools tools :content content
                    :context (list :host-buffer view)
                    :runtime-extras '(:record-conversation nil)
                    :sink (superchat-sink-discard)))
             :run-id))

;;;; the data -----------------------------------------------------------------

(ert-deftest reference/the-data-keeps-to-its-limits ()
  "Release check: ids are stable-looking and unique, the index and every page
stay under their caps -- refused here, never truncated at run time."
  (should (featurep 'superchat-reference))
  (let ((ids (superchat-reference-ids))
        (index (superchat-reference-index-text)))
    (should (member ref--page ids))
    (should (= (length ids) (length (delete-dups (copy-sequence ids)))))
    (dolist (id ids)
      (should (string-match-p "\\`[a-z0-9]+\\(-[a-z0-9]+\\)*\\'" id))
      (should (<= (length (superchat-reference-summary id)) 120))
      (should (<= (length (superchat-reference-body id)) 4096))
      (should (string-match-p (regexp-quote id) index)))
    (should (string-prefix-p ref--index-marker index))
    (should (<= (length index) 600))))

(ert-deftest reference/the-index-is-made-without-the-pages ()
  "The index needs ids and summaries only."
  (should (featurep 'superchat-reference))
  (cl-letf (((symbol-function 'superchat-reference-body)
             (lambda (&rest _) (error "the index read a page"))))
    (should (string-match-p (regexp-quote ref--page)
                            (superchat-reference-index-text)))))

(ert-deftest reference/the-first-page-says-what-the-contract-requires ()
  "The six points, its standing below the user and the permissions, and never an
instruction to persist or to evaluate a whole buffer."
  (should (featurep 'superchat-reference))
  (let ((text (superchat-reference-body ref--page))
        (case-fold-search t))
    (dolist (needle '("before you change" "key-binding" "(kbd "
                      "the buffer" "the file on disk" "the running session"
                      "only the forms you changed"
                      "only what this task changed"
                      "saving is the user's"
                      "what you observed"
                      "the user's instructions" "permission"))
      (should (string-match-p (regexp-quote needle) text)))
    (should-not (string-match-p
                 "for persistence\\|fresh emacs\\|\\bsave \\(the\\|it\\|your\\|that\\|this\\) \\|save-buffer\\|write-region\\|write-file\\|(eval-buffer)"
                 text))))

;;;; the tool -----------------------------------------------------------------

(ert-deftest reference/reading-returns-the-page-and-its-version ()
  (should (fboundp 'superchat-tool-read-reference))
  (let ((result (superchat-tool-read-reference ref--page)))
    (should (string-match-p (regexp-quote ref--page) result))
    (should (string-match-p (regexp-quote (superchat-reference-version ref--page)) result))
    (should (string-match-p (regexp-quote (superchat-reference-body ref--page)) result))))

(ert-deftest reference/unknown-and-path-like-ids-fail-without-touching-files ()
  "The tool knows ids, not paths: nothing here may reach the file system."
  (should (fboundp 'superchat-tool-read-reference))
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (&rest _) (error "the reference tool read a file")))
            ((symbol-function 'find-file-noselect)
             (lambda (&rest _) (error "the reference tool visited a file")))
            ((symbol-function 'expand-file-name)
             (lambda (&rest _) (error "the reference tool built a path"))))
    (dolist (id (list "no-such-page" "../superchat.el" "/etc/passwd" "~/.emacs"
                      "emacs-keybindings/../x" "" nil 7))
      (let ((result (superchat-tool-read-reference id)))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p (regexp-quote ref--page) result))))))

;;;; what the model is sent -----------------------------------------------------

(ert-deftest reference/a-default-chat-request-offers-the-tool-and-the-index-once ()
  (ref--fixture (list '(:tool-uses (("list_buffers"))) '(:text "done"))
    (let ((view (smoke--open-chat)))
      (ref--settle (smoke--send view "bind a key for me"))
      (should (= 2 (length ref--systems)))
      (dolist (text ref--systems)
        (should (= 1 (ref--count ref--index-marker text)))
        (should (= 1 (ref--count (concat "- " ref--page ":") text))))
      (dolist (n '(0 1))
        (should (member "read_reference" (plist-get (ref--request n) :tools)))))))

(ert-deftest reference/the-index-follows-the-final-tool-surface ()
  "Guard: no reading tool on the final surface, no index advertising one."
  (ref--fixture (list '(:text "a") '(:text "b") '(:text "c"))
    (let ((view (smoke--open-chat)))
      (dolist (tools (list '("list_buffers" "eval_elisp") 'none))
        (ref--settle (ref--submit view tools "just look")))
      (let ((off (superchat-on nil 'before-provider-request 'ref-narrow
                               (lambda (candidate next)
                                 (funcall next (plist-put candidate :tools
                                                          '("eval_elisp" "ask_user")))))))
        (unwind-protect
            (ref--settle (smoke--send view "narrowed"))
          (funcall off)))
      (should (= 3 (length ref--systems)))
      (dolist (text ref--systems)
        (should (= 0 (ref--count ref--index-marker text)))))))

(ert-deftest reference/the-current-buffer-entry-follows-the-users-tool-list ()
  "The current-buffer entry collects before its tools are chosen.  With the
default list it gets the index; with a user list that lacks the tool it must
not be told about an entry that is not there."
  (ref--fixture (list '(:text "a") '(:text "b"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun ref-sample () 1)\n")
      (let ((run (plist-get (plist-get (superchat-act "explain this") :run-handle)
                            :run-id)))
        (ref--settle run))
      (should (= 1 (ref--count ref--index-marker (car ref--systems))))
      (let ((superchat-llm-tool-names (remove "read_reference"
                                              (copy-sequence superchat-llm-tool-names)))
            (superchat-llm-tools-list nil))
        (let ((run (plist-get (plist-get (superchat-act "explain this again") :run-handle)
                              :run-id)))
          (ref--settle run))
        (should-not (member "read_reference" (plist-get (car smoke--requests) :tools)))
        (should (= 0 (ref--count ref--index-marker (car ref--systems))))))))

(ert-deftest reference/inline-ask-is-not-widened-this-round ()
  "Scope exclusion of 2026-09-20, not a finding that inline asks need no pages."
  (should-not (member "read_reference" superchat--select-tools-inline-ask-names)))

;;;; reading through the pipeline ---------------------------------------------

(ert-deftest reference/a-page-is-read-through-the-pipeline-and-nobody-is-asked ()
  "At the default level and at `read-only': the call runs, the page reaches the
model in the tool result, the Tape has the call, nobody was asked, and the
configuration is what it was."
  (dolist (mode '(edit-buffers read-only))
    (let ((superchat-sandbox-default-mode mode))
      (ref--fixture (list `(:tool-uses (("read_reference" (id . ,ref--page))))
                          '(:text "done"))
        (let* ((superchat-sandbox-default-mode mode)
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (before (list superchat-llm-tool-names
                             (superchat-sandbox-effective-mode session)
                             (superchat-approval-effective-policy session))))
          (ref--settle (smoke--send view "how do I bind a key"))
          (should (null ref--asked))
          (should (string-match-p
                   (regexp-quote (substring (superchat-reference-body ref--page) 0 60))
                   (plist-get (ref--request 1) :prompt-text)))
          (should (cl-some (lambda (event)
                             (and (equal (plist-get event :type) "tool.completed")
                                  (equal (plist-get (plist-get event :payload) :tool-name)
                                         "read_reference")))
                           (superchat-tape-replay-events session)))
          (should (equal before
                         (list superchat-llm-tool-names
                               (superchat-sandbox-effective-mode session)
                               (superchat-approval-effective-policy session)))))))))

;;;; pointer sentences ---------------------------------------------------------

(defconst ref--void-map-call
  '("eval_elisp" (expression . "(define-key fundamental-mode-map (kbd \"C-c t h\") #'ignore)")))

(defun ref--pointer-count (text)
  "How many times TEXT points at the key binding page."
  (ref--count (concat "`" ref--page "'") text))

(ert-deftest reference/an-identified-error-gets-one-pointer-and-keeps-the-error ()
  "The error stays in the tool result as it was; the product's failure feedback
names the page; a second identical failure in the same loop is not nagged."
  (ref--fixture (list (list :tool-uses (list ref--void-map-call))
                      (list :tool-uses (list ref--void-map-call))
                      '(:text "done"))
    (let ((view (smoke--open-chat)))
      (ref--settle (smoke--send view "bind a key in fundamental mode"))
      (let ((second (plist-get (ref--request 1) :prompt-text))
            (third (plist-get (ref--request 2) :prompt-text)))
        (should (string-match-p
                 "Error: evaluating expression: .*void.*fundamental-mode-map" second))
        (should (= 1 (ref--pointer-count second)))
        (should (string-match-p ref--pointer second))
        ;; The third request repeats the history (one pointer) and adds none.
        (should (= 1 (ref--pointer-count third)))))))

(ert-deftest reference/an-unrelated-error-gets-no-pointer ()
  "Guard: not every error is sent to the same handbook."
  (ref--fixture (list '(:tool-uses (("eval_elisp" (expression . "(car 1)"))))
                      '(:text "done"))
    (let ((view (smoke--open-chat)))
      (ref--settle (smoke--send view "try something"))
      (let ((second (plist-get (ref--request 1) :prompt-text)))
        (should (string-match-p "Tool execution failed" second))
        (should (= 0 (ref--pointer-count second)))))))

(ert-deftest reference/without-the-reading-tool-the-page-itself-follows ()
  "A pointer never names an entry the request does not have: without
`read_reference', the default since 2026-10-27, the feedback carries the
page, once."
  (ref--fixture (list (list :tool-uses (list ref--void-map-call))
                      (list :tool-uses (list ref--void-map-call))
                      '(:text "done"))
    (let ((view (smoke--open-chat)))
      (ref--settle (ref--submit view '("eval_elisp") "bind a key"))
      (let ((second (plist-get (ref--request 1) :prompt-text))
            (third (plist-get (ref--request 2) :prompt-text))
            (page (regexp-quote (substring (superchat-reference-body ref--page) 0 60))))
        (should (string-match-p "Tool execution failed" second))
        (should-not (string-match-p "read_reference can fetch" second))
        (should (= 1 (ref--pointer-count second)))
        (should (string-match-p page second))
        ;; Once per tool loop: the third request repeats it, adds none.
        (should (= 1 (ref--pointer-count third)))))))

(ert-deftest reference/the-default-surface-has-no-reading-tool-and-no-index ()
  "2026-10-27: six default tools; the index advertises an entry only where
the request has it."
  (should-not (member "read_reference" superchat-llm-default-tool-names))
  (let ((ref--offered superchat-llm-default-tool-names))
    (ref--fixture (list '(:text "done"))
      (ref--settle (smoke--send (smoke--open-chat) "bind a key for me"))
      (should (= 0 (ref--count ref--index-marker (car ref--systems)))))))

(provide 'reference-regression)

;;; reference-regression.el ends here
