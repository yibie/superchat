;;; m5-request-context-regression.el --- request preview/send contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline behavioral coverage for request preparation.  The canonical smoke
;; fixture runs actual chat/act/inspector/driver entry points and captures the
;; llm provider boundary; no real model and no protected regression set loads.
;; This file is separate from the leader-owned M3 and M5 baseline suites.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/m5-request-context-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^m5-request-context/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defconst m5rc--guidance
  "Tools that depend on each other's output must wait for the previous result.")
(defvar m5rc--static-note "M5RC_STATIC_SYSTEM_NOTE")
(defvar m5rc--requests nil)
(defvar m5rc--submissions nil)

(defun m5rc--collect-system (_envelope _state _tools)
  "Contribute an ordinary plugin-owned static instruction."
  (list (superchat-prompt-fragment
         :owner 'm5rc :id "m5rc-system-note" :slot 'system
         :text m5rc--static-note :budget-class 'hard)))

(defun m5rc--role-contents (prompt role)
  "Copy the contents of PROMPT's interactions with ROLE, in order."
  (cl-loop for interaction in (llm-chat-prompt-interactions prompt)
           when (equal role
                       (format "%s" (llm-chat-prompt-interaction-role interaction)))
           collect (copy-tree (llm-chat-prompt-interaction-content interaction))))

(defun m5rc--snapshot-prompt (prompt)
  "Copy the provider-visible system, user and assistant roles from PROMPT.
Earlier turns are their own user and assistant messages (context contract,
amendment 2026-09-25)."
  (list :system (copy-sequence (or (llm-chat-prompt-context prompt) ""))
        :users (m5rc--role-contents prompt "user")
        :assistants (m5rc--role-contents prompt "assistant")))

(defmacro m5rc--fixture (script &rest body)
  "Run BODY with SCRIPT and real, scoped context collection."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (m5rc--static-note "M5RC_STATIC_SYSTEM_NOTE")
           (m5rc--requests nil)
           (m5rc--submissions nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil)
           (interprogram-paste-function nil))
       (smoke--with-fixture ,script
         (let ((network (symbol-function 'smoke--fake-network))
               (submit (symbol-function 'superchat-runtime-submit)))
           (superchat-context-register
            'm5rc-system-note #'m5rc--collect-system
            :owner 'm5rc :required t :scope superchat-scope-global)
           (unwind-protect
               (cl-letf (((symbol-function 'smoke--fake-network)
                          (lambda (mode provider prompt partial response error multi)
                            (push (m5rc--snapshot-prompt prompt) m5rc--requests)
                            (funcall network mode provider prompt
                                     partial response error multi)))
                         ((symbol-function 'superchat-runtime-submit)
                          (lambda (request)
                            (push (list :prompt (plist-get request :prompt)
                                        :context-envelope
                                        (copy-tree (plist-get request :context-envelope)))
                                  m5rc--submissions)
                            (funcall submit request))))
                 ,@body)
             (superchat-context-unregister
              'm5rc-system-note superchat-scope-global)))))))

(defun m5rc--count (needle text)
  "Count nonoverlapping literal NEEDLE occurrences in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun m5rc--assert-system (text &optional static-count base)
  "Check independent system contributions in provider or inspector TEXT."
  (should (= (or static-count 1) (m5rc--count "M5RC_STATIC_SYSTEM_NOTE" text)))
  (should (= 1 (m5rc--count m5rc--guidance text)))
  (when base (should (= 1 (m5rc--count base text)))))

(defun m5rc--settle (expected-requests)
  "Wait for EXPECTED-REQUESTS to reach the provider and settle."
  (should (smoke--wait
           (lambda ()
             (and (= expected-requests (length m5rc--requests))
                  (null (superchat-runtime-active-runs))))
           15))
  (should (= expected-requests (length smoke--requests)))
  (car m5rc--requests))

(defun m5rc--source (&optional text select)
  "Create an editing source with TEXT and optionally SELECT its contents."
  (let ((source (generate-new-buffer "m5rc-source.el")))
    (with-current-buffer source
      (emacs-lisp-mode)
      (insert (or text "(message \"source\")\n"))
      (when select
        (goto-char (point-min))
        (set-mark (point-min))
        (goto-char (point-max))
        (activate-mark)))
    source))

(defun m5rc--open (source)
  "Open an actual View from SOURCE."
  (with-current-buffer source (superchat))
  (let ((view (get-buffer superchat-buffer-name)))
    (should (buffer-live-p view))
    view))

(defun m5rc--inspect (source text)
  "Inspect TEXT through the actual external act entry from SOURCE."
  (with-current-buffer source (superchat-act text t))
  (let ((inspector
         (cl-find-if (lambda (buffer)
                       (with-current-buffer buffer
                         (derived-mode-p 'superchat-request-mode)))
                     (buffer-list))))
    (should (buffer-live-p inspector))
    inspector))

(defun m5rc--body (inspector)
  "Read the exact editable body shown in INSPECTOR."
  (with-current-buffer inspector
    (buffer-substring-no-properties superchat--request-prompt-start
                                    superchat--request-prompt-end)))

(defun m5rc--read-only-part (inspector)
  "Read what INSPECTOR shows before its editable body."
  (with-current-buffer inspector
    (buffer-substring-no-properties (point-min) superchat--request-prompt-start)))

(defun m5rc--replace-body (inspector text)
  "Replace the actual inspector's editable body with TEXT."
  (with-current-buffer inspector
    (let ((inhibit-read-only t))
      (goto-char superchat--request-prompt-start)
      (delete-region superchat--request-prompt-start superchat--request-prompt-end)
      (insert text))))

(defun m5rc--copy (inspector)
  "Use the actual inspector copy command without touching the OS clipboard."
  (with-current-buffer inspector (superchat-request-copy))
  (should (stringp (car kill-ring)))
  (car kill-ring))

(defun m5rc--attachment (view)
  "Attach a real tiny text file with a unique content marker to VIEW."
  (let ((file (expand-file-name "m5rc-attachment.txt" superchat-data-directory)))
    (with-temp-file file (insert "M5RC_ATTACHMENT_BODY\n"))
    (with-current-buffer view
      (setq-local superchat--current-context-files (list file)))
    file))

(defun m5rc--with-preset-system (view base function)
  "Run FUNCTION after applying a real prompt Preset whose Turn receives BASE.
Prompt Presets do not have a system-prompt slot and their body is not a
persona.  The test supplies an ordinary programmable Turn customization at
the existing Preset application seam, only for this exact Preset object.
The original application, active-View lookup and all assembly still run."
  (let ((preset (superchat-preset-from-plist
                 '(:name "m5rc-active" :type prompt
                   :body "M5RC_PROMPT_BODY_NOT_A_PERSONA")))
        (apply-preset (symbol-function 'superchat-preset-apply)))
    (with-current-buffer view (setq-local superchat--active-preset preset))
    (cl-letf (((symbol-function 'superchat-preset-apply)
               (lambda (candidate turn)
                 (prog1 (funcall apply-preset candidate turn)
                   (when (eq candidate preset)
                     (setf (superchat-turn-system-prompt turn) base))))))
      (funcall function))))

(ert-deftest m5-request-context/chat-system-contributions-appear-once ()
  "Ordinary chat is the control for static and built-in system collectors."
  (m5rc--fixture '((:text "done"))
    (let ((view (m5rc--open (m5rc--source))))
      (smoke--send view "M5RC_CHAT_REQUEST")
      (m5rc--assert-system (plist-get (m5rc--settle 1) :system)))))

(ert-deftest m5-request-context/act-system-contributions-appear-once ()
  "Act must not send its assembled preview as a new base system prompt."
  (m5rc--fixture '((:text "done"))
    (with-current-buffer (m5rc--source)
      (superchat-act "M5RC_ACT_REQUEST"))
    (m5rc--assert-system (plist-get (m5rc--settle 1) :system))))

(ert-deftest m5-request-context/preset-system-survives-preview-copy-and-send ()
  "Custom Turn base instructions and collected guidance agree at all boundaries."
  (m5rc--fixture '((:text "done"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (base "M5RC_CUSTOM_PRESET_SYSTEM"))
      (m5rc--with-preset-system
       view base
       (lambda ()
         (let ((inspector (m5rc--inspect source "M5RC_PRESET_REQUEST")))
           (should-not m5rc--requests)
           (m5rc--assert-system
            (with-current-buffer inspector (buffer-string)) 1 base)
           (m5rc--assert-system (m5rc--copy inspector) 1 base)
           (with-current-buffer inspector (superchat-request-send))
           (let ((system (plist-get (m5rc--settle 1) :system)))
             (m5rc--assert-system system 1 base)
             (should-not (string-match-p "M5RC_PROMPT_BODY_NOT_A_PERSONA" system)))))))))

(ert-deftest m5-request-context/equal-base-and-collector-are-independent ()
  "Identical text from an explicit base and a collector is two contributions."
  (m5rc--fixture '((:text "done"))
    (let* ((view (m5rc--open (m5rc--source)))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-service-call
       nil 'driver 'submit
       (list :surface 'chat :session-id session :content "M5RC_DIRECT_REQUEST"
             :system-prompt m5rc--static-note
             :context (list :host-buffer view)
             :sink (superchat-sink-create
                    :final (lambda (&rest _)) :error (lambda (&rest _)))))
      (m5rc--assert-system (plist-get (m5rc--settle 1) :system) 2))))

(ert-deftest m5-request-context/equal-base-survives-inspector-too ()
  "Preview cannot fix duplication by text deduplication or discarding the base."
  (m5rc--fixture '((:text "done"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source)))
      (m5rc--with-preset-system
       view m5rc--static-note
       (lambda ()
         (let ((inspector (m5rc--inspect source "M5RC_IDENTICAL_REQUEST")))
           (m5rc--assert-system
            (with-current-buffer inspector (buffer-string)) 2)
           (m5rc--assert-system (m5rc--copy inspector) 2)
           (with-current-buffer inspector (superchat-request-send))
           (m5rc--assert-system (plist-get (m5rc--settle 1) :system) 2)))))))

(ert-deftest m5-request-context/edited-inspector-does-not-restore-context ()
  "Deleting the attachment and origin leaves exactly the edited body.  The
earlier turn was never part of the body: it is shown read-only and still goes
as its own messages (context contract, amendment 2026-09-25)."
  (m5rc--fixture '((:text "M5RC_PRIOR_ASSISTANT") (:text "done"))
    (let* ((source (m5rc--source "M5RC_OLD_SELECTION\n" t))
           (view (m5rc--open source))
           (edited "M5RC_EDITED_ONLY\nKeep this exact second line."))
      (smoke--send view "M5RC_PRIOR_USER")
      (m5rc--settle 1)
      (m5rc--attachment view)
      (let* ((inspector (m5rc--inspect source "M5RC_INSPECT_REQUEST"))
             (body (m5rc--body inspector)))
        (dolist (marker '("M5RC_ATTACHMENT_BODY" "M5RC_OLD_SELECTION"
                          "[Emacs origin]"))
          (should (string-match-p (regexp-quote marker) body)))
        (dolist (marker '("M5RC_PRIOR_USER" "M5RC_PRIOR_ASSISTANT"))
          (should-not (string-match-p (regexp-quote marker) body))
          (should (string-match-p (regexp-quote marker)
                                  (m5rc--read-only-part inspector))))
        (m5rc--replace-body inspector edited)
        (should (equal edited (m5rc--body inspector)))
        (let ((copied (m5rc--copy inspector)))
          (should (string-match-p (regexp-quote edited) copied))
          (should-not (string-match-p "M5RC_ATTACHMENT_BODY" copied)))
        (with-current-buffer source
          (erase-buffer)
          (insert "M5RC_NEW_SELECTION\n"))
        (with-current-buffer inspector (superchat-request-send))
        (let ((request (m5rc--settle 2)))
          ;; Compare the actual role contents, not a whole-prompt substring:
          ;; the earlier turn, then exactly the edited body.
          (should (equal (list "M5RC_PRIOR_USER" edited) (plist-get request :users)))
          (should (equal '("M5RC_PRIOR_ASSISTANT") (plist-get request :assistants)))
          ;; The edited body goes to Runtime as typed, with the selection the
          ;; earlier-turn messages were built from.
          (should (equal edited (plist-get (car m5rc--submissions) :prompt)))
          (let ((context (plist-get (car m5rc--submissions) :context-envelope)))
            (should context)
            (should (string-match-p "M5RC_PRIOR_USER" (plist-get context :prompt))))
          (m5rc--assert-system (plist-get request :system)))))))

(ert-deftest m5-request-context/unedited-act-keeps-one-frozen-context ()
  "A second-turn act sends its one preview body, without fresh collection."
  (m5rc--fixture '((:text "M5RC_PRIOR_ASSISTANT") (:text "done"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (file (m5rc--attachment view)))
      (smoke--send view "M5RC_PRIOR_USER")
      (m5rc--settle 1)
      (let* ((inspector (m5rc--inspect source "M5RC_SECOND_REQUEST"))
             (body (m5rc--body inspector)))
        ;; Shown once, read-only, outside the body (context contract,
        ;; amendment 2026-09-25).
        (dolist (marker '("M5RC_PRIOR_USER" "M5RC_PRIOR_ASSISTANT"))
          (should (= 1 (m5rc--count marker (m5rc--read-only-part inspector))))
          (should (= 0 (m5rc--count marker body))))
        (should (string-match-p "M5RC_ATTACHMENT_BODY" body))
        ;; The plugin/file now return other data: the accepted preview is frozen.
        (setq m5rc--static-note "M5RC_CHANGED_SYSTEM_AFTER_PREVIEW")
        (with-temp-file file (insert "M5RC_CHANGED_ATTACHMENT_AFTER_PREVIEW\n"))
        (with-current-buffer inspector (superchat-request-send))
        (let ((request (m5rc--settle 2)))
          (should (equal (list "M5RC_PRIOR_USER" body) (plist-get request :users)))
          (should (equal '("M5RC_PRIOR_ASSISTANT") (plist-get request :assistants)))
          ;; An unedited body still carries its actual history selection to
          ;; Runtime; removing all receipts would make the edited case pass.
          (let ((context (plist-get (car m5rc--submissions) :context-envelope)))
            (should context)
            (should (string-match-p "M5RC_PRIOR_USER"
                                    (plist-get context :prompt))))
          (m5rc--assert-system (plist-get request :system))
          (should-not (string-match-p "M5RC_CHANGED_SYSTEM_AFTER_PREVIEW"
                                      (plist-get request :system))))))))

(ert-deftest m5-request-context/prompt-skill-still-collects-history-and-attachments ()
  "A skill-combined body is not a previously inspected final context."
  (m5rc--fixture '((:text "M5RC_PRIOR_ASSISTANT") (:text "done"))
    (let* ((view (m5rc--open (m5rc--source)))
           (skill-file (expand-file-name "m5rc-skill.md" superchat-skills-directory)))
      (smoke--send view "M5RC_PRIOR_USER")
      (m5rc--settle 1)
      (m5rc--attachment view)
      (make-directory superchat-skills-directory t)
      (with-temp-file skill-file
        (insert "---\nname: m5rc-skill\ntype: prompt\n---\nM5RC_SKILL_BODY\n"))
      (should (superchat-skills-exists-p "m5rc-skill"))
      (smoke--send view ">m5rc-skill M5RC_SKILL_ARGUMENT")
      (let* ((request (m5rc--settle 2))
             (users (plist-get request :users))
             ;; The earlier turn is its own user and assistant messages.
             (text (mapconcat #'identity
                              (append users (plist-get request :assistants))
                              "\n")))
        (dolist (marker '("M5RC_PRIOR_USER" "M5RC_PRIOR_ASSISTANT"
                          "M5RC_ATTACHMENT_BODY" "M5RC_SKILL_BODY"
                          "M5RC_SKILL_ARGUMENT"))
          (should (= 1 (m5rc--count marker text))))
        (m5rc--assert-system (plist-get request :system))))))

(ert-deftest m5-request-context/prepared-guidance-follows-each-attempt-tool-surface ()
  "Freezing an inspected context retains pure renderers without recollection."
  (m5rc--fixture
      '((:tool-uses (("read_buffer" (buffer . "*scratch*")))) (:text "done"))
    (let* ((source (m5rc--source))
           (view (m5rc--open source))
           (base "M5RC_BASE_UNRELATED_TO_TOOL_RENDERER")
           (collected 0)
           (attempts 0)
           (remove-listener
            (superchat-on
             nil 'before-provider-request 'm5rc-final-tools
             (lambda (candidate next)
               (cl-incf attempts)
               (funcall next
                        (plist-put candidate :tools
                                   ;; The actual act entry owns trusted human
                                   ;; input: retain it while narrowing ordinary
                                   ;; tools, as required by the existing gate.
                                   (if (= attempts 1)
                                       '("read_buffer" "ask_user")
                                     '("ask_user"))))))))
      (superchat-context-register
       'm5rc-dynamic-guidance
       (lambda (_envelope _state _tools)
         (cl-incf collected)
         (list
          (plist-put
           (superchat-prompt-fragment
            :owner 'm5rc :id "m5rc-dynamic-guidance" :slot 'system
            :text "M5RC_DYNAMIC_GUIDANCE:initial")
           :render-tools
           (lambda (names)
             (concat "M5RC_DYNAMIC_GUIDANCE:"
                     (if (member "read_buffer" names)
                         "read_buffer" "no_read_buffer"))))))
       :owner 'm5rc :scope superchat-scope-global)
      (unwind-protect
          (m5rc--with-preset-system
           view base
           (lambda ()
             (let ((inspector (m5rc--inspect source "M5RC_GUIDED_REQUEST")))
               (should-not m5rc--requests)
               (with-current-buffer inspector (superchat-request-send))
               (m5rc--settle 2)
               (should (= 1 collected))
               (should (= 2 attempts))
               (let ((requests (reverse m5rc--requests)))
                 (cl-loop
                  for request in requests
                  for expected in '("M5RC_DYNAMIC_GUIDANCE:read_buffer"
                                    "M5RC_DYNAMIC_GUIDANCE:no_read_buffer")
                  for system = (plist-get request :system)
                  do
                  (m5rc--assert-system system 1 base)
                  (should (= 1 (m5rc--count "M5RC_DYNAMIC_GUIDANCE:" system)))
                  (should (= 1 (m5rc--count expected system)))
                  (should-not (string-match-p "M5RC_DYNAMIC_GUIDANCE:initial"
                                              system)))))))
        (when (functionp remove-listener) (funcall remove-listener))
        (superchat-context-unregister
         'm5rc-dynamic-guidance superchat-scope-global)))))

(ert-deftest m5-request-context/initial-over-budget-preview-is-rejected ()
  "The inspector must not offer an over-budget placeholder as a valid request."
  (m5rc--fixture '((:text "must not run"))
    ;; Pin only the fallback budget; use the real assembler and entry point.
    (let ((superchat--default-context-budget-chars 1024)
          (source (m5rc--source)))
      (should-error
       (m5rc--inspect source (make-string 4096 ?x))
       :type 'superchat-prompt-over-budget)
      (should-not m5rc--requests)
      (should-not (superchat-runtime-active-runs)))))

(ert-deftest m5-request-context/edited-over-budget-body-is-not-sent ()
  "An oversized edit is refused, retained in the inspector, and never truncated."
  (m5rc--fixture '((:text "must not run"))
    (let* ((source (m5rc--source))
           (inspector (m5rc--inspect source "M5RC_SMALL_REQUEST"))
           (edited (make-string 4096 ?x)))
      (m5rc--replace-body inspector edited)
      (let ((superchat--default-context-budget-chars 1024))
        (should-error
         (with-current-buffer inspector (superchat-request-send))
         :type 'superchat-prompt-over-budget))
      (should-not m5rc--requests)
      (should-not (superchat-runtime-active-runs))
      (should (buffer-live-p inspector))
      (should (equal edited (m5rc--body inspector))))))

(provide 'm5-request-context-regression)
;;; m5-request-context-regression.el ends here
