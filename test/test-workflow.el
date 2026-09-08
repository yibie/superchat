;;; test-workflow.el — Workflow parser, substitution, and async engine tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-workflow.el:
;;   - Parsing (steps, per-line annotations, variable substitution)
;;   - File loading (.workflow discovery)
;;   - Async execution chain (mock Runtime, variable flow, error handling)

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-workflow)
(require 'superchat-skills)

;; ── Mock externals (no superchat.el loaded) ──
(defvar superchat-buffer-name "*superchat-test*")
(defvar superchat-lang "English")
(unless (fboundp 'superchat--record-message)
  (defun superchat--record-message (_role _content &optional _tape-id) nil))
(unless (fboundp 'superchat--insert-prompt)
  (defun superchat--insert-prompt (&optional _owner) nil))
(unless (fboundp 'superchat--response-region-ensure-view-prompt)
  (defun superchat--response-region-ensure-view-prompt (&optional _independent)
    (superchat--insert-prompt :pending)))

;; ═══════════════════════════════════════════════════════
;; Part A — File loading
;; ═══════════════════════════════════════════════════════

(defun workflow-test--deliver (request text status &optional tape-id)
  "Deliver TEXT with STATUS through REQUEST's callback Sink (S13b).
`completed' goes to `:final', any other status to `:error'."
  (let ((sink (plist-get request :sink)))
    (should (superchat-sink-p sink))
    (if (eq status 'completed)
        (superchat-sink-emit sink 'final nil (list :text text :tape-id tape-id))
      (superchat-sink-emit sink 'error nil
                           (list :text text :status status :tape-id tape-id)))))

(ert-deftest workflow-name/quoted-name-preserves-arguments ()
  (dolist (name '("天气  预报" "weather report" "say \"hello\"" "back\\slash"))
    (should (equal (cons name "杭州  tomorrow\n详细建议")
                   (superchat-workflow--parse-invocation
                    (concat (prin1-to-string name) " 杭州  tomorrow\n详细建议")))))
  (should (equal '("weather" . "report tomorrow")
                 (superchat-workflow--parse-invocation "weather report tomorrow")))
  (should (equal '(nil . "") (superchat-workflow--parse-invocation "  "))))

(ert-deftest workflow-name/malformed-quote-is-not-executed ()
  (dolist (input '("\"weather report" "\"weather\"suffix" "\"\""))
    (cl-letf (((symbol-function 'superchat-workflow-execute-async)
               (lambda (&rest _) (ert-fail "Malformed invocation executed"))))
      (should-error (superchat--cmd-workflow "workflow" input nil nil nil)
                    :type 'user-error))))

(ert-deftest workflow-name/quoted-slash-uses-real-safe-resolver ()
  (let ((superchat-workflow-directory (make-temp-file "workflow-name-" t))
        called)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "天气  预报.workflow" superchat-workflow-directory)
            (insert "Forecast $input\n"))
          (cl-letf (((symbol-function 'superchat-workflow-execute-async)
                     (lambda (&rest args) (setq called args))))
            (should (equal '(:type :noop)
                           (superchat--cmd-workflow "workflow" "\"天气  预报\" 杭州" nil nil nil)))
            (should (equal '("天气  预报" "杭州") (seq-take called 2))))
          (should (equal "Forecast $input\n"
                         (superchat-workflow--load (car called) (nth 2 called)))))
      (delete-directory superchat-workflow-directory t))))

(ert-deftest workflow-directory-defcustom ()
  "superchat-workflow-directory is a string ending in workflow/."
  (should (stringp superchat-workflow-directory))
  (should (string-suffix-p "workflow/" superchat-workflow-directory)))

(ert-deftest workflow--exists-p-returns-nil-for-unknown ()
  "exists-p returns nil for a name that certainly does not exist."
  (should-not (superchat-workflow--exists-p "__nonexistent_workflow_test__")))

(ert-deftest workflow--file-path-ends-with-.workflow ()
  "file-path appends .workflow to the name."
  (let ((path (superchat-workflow--file-path "test")))
    (should (string-suffix-p "test.workflow" path))))

(ert-deftest workflow--load-returns-nil-for-missing ()
  "load returns nil when the file does not exist."
  (should-not (superchat-workflow--load "__nonexistent_workflow_test__")))

(ert-deftest workflow--load-reads-content ()
  "load returns file content when the file exists."
  (let* ((tmp-dir (make-temp-file "wf-load" t))
         (orig-val superchat-workflow-directory))
    (unwind-protect
        (progn
          (setq superchat-workflow-directory tmp-dir)
          (with-temp-file (expand-file-name "testload.workflow" tmp-dir)
            (insert "step1\nstep2\n"))
          (let ((body (superchat-workflow--load "testload")))
            (should body)
            (should (string-match-p "step1" body))
            (should (string-match-p "step2" body))))
      (setq superchat-workflow-directory orig-val)
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest workflow/resolver-rejects-symlink-outside-directory ()
  "Resolver and loader reject a workflow symlink that escapes the root."
  (let* ((root (make-temp-file "superchat-workflow-symlink-" t))
         (workflow-directory (expand-file-name "workflow" root))
         (outside-directory (expand-file-name "outside" root))
         (outside-file (expand-file-name "secret.workflow" outside-directory))
         (link-file (expand-file-name "linked.workflow" workflow-directory)))
    (unwind-protect
        (progn
          (make-directory workflow-directory t)
          (make-directory outside-directory t)
          (with-temp-file outside-file
            (insert "OUTSIDE-SECRET"))
          (make-symbolic-link outside-file link-file t)
          (let ((superchat-workflow-directory workflow-directory))
            (should (file-symlink-p link-file))
            (should-not (superchat-workflow--resolve "linked"))
            (should-not (superchat-workflow--exists-p "linked"))
            (should-not (superchat-workflow--load "linked"))
            (should-not (superchat-workflow--file-path "linked"))
            (should-not (member "linked" (superchat-workflow--list)))))
      (delete-directory root t))))

(ert-deftest workflow/candidate-selection-classifies-metadata ()
  "Pure candidate selection distinguishes missing, unique, and ambiguous."
  (let* ((nfc (string #x00e9))
         (nfd (concat "e" (string #x0301)))
         (unique (list :basename nfc
                       :path (concat "/tmp/" nfc ".workflow")))
         (other (list :basename nfd
                      :path (concat "/tmp/" nfd ".workflow")))
         (choice (superchat-workflow--select-candidate nfd (list unique))))
    (should (eq 'unique (plist-get choice :status)))
    (should (equal unique (plist-get choice :candidate)))
    (setq choice
          (superchat-workflow--select-candidate nfc (list unique other)))
    (should (eq 'ambiguous (plist-get choice :status)))
    (should (equal (list unique other) (plist-get choice :candidates)))
    (setq choice
          (superchat-workflow--select-candidate "missing" (list unique)))
    (should (eq 'missing (plist-get choice :status)))))

(ert-deftest workflow/resolver-rejects-real-nfc-ambiguous-candidates ()
  "Resolver rejects two real NFC-equivalent files when the volume supports them."
  (let* ((nfc (string #x00e9))
         (nfd (concat "e" (string #x0301)))
         (directory (make-temp-file "superchat-workflow-ambiguous-" t))
         (nfc-file (expand-file-name (concat nfc ".workflow") directory))
         (nfd-file (expand-file-name (concat nfd ".workflow") directory)))
    (unwind-protect
        (progn
          (with-temp-file nfc-file
            (insert "NFC"))
          (with-temp-file nfd-file
            (insert "NFD"))
          (let* ((actual-files (directory-files directory t "\\.workflow\\'"))
                 (actual-names (mapcar #'file-name-base actual-files))
                 (normalized-names
                  (mapcar #'ucs-normalize-NFC-string actual-names))
                 (actual-truenames (mapcar #'file-truename actual-files)))
            (skip-unless
             (and (= 2 (length actual-files))
                  (= 1 (length (delete-dups normalized-names)))
                  (= 2 (length (delete-dups actual-truenames)))))
            (let ((superchat-workflow-directory directory))
              (let ((condition
                     (should-error (superchat-workflow--resolve nfc)
                                   :type 'user-error)))
                (should (string-match-p
                         "Ambiguous workflow"
                         (error-message-string condition)))))))
      (delete-directory directory t))))

(ert-deftest workflow/unicode-basename-resolution-is-bidirectional ()
  "Real workflow files resolve by NFC-equivalent basename in both directions."
  (let* ((nfc (string #x00e9))
         (nfd (concat "e" (string #x0301)))
         (cases (list (list nfc nfd "NFC-CONTENT")
                      (list nfd nfc "NFD-CONTENT"))))
    (dolist (case cases)
      (let* ((directory (make-temp-file "superchat-workflow-unicode-" t))
             (actual-name (nth 0 case))
             (input-name (nth 1 case))
             (content (nth 2 case))
             (workflow-file
              (expand-file-name (concat actual-name ".workflow") directory)))
        (unwind-protect
            (progn
              (with-temp-file workflow-file
                (insert content))
              (let* ((superchat-workflow-directory directory)
                     (actual-file
                      (car (directory-files directory t "\\.workflow\\'"))))
                (let ((resolved (superchat-workflow--resolve input-name)))
                  (should (stringp resolved))
                  (should (file-exists-p resolved))
                  (should (equal resolved (file-truename actual-file)))
                  (should (equal resolved
                                 (superchat-workflow--exists-p input-name)))
                  (should (equal content
                                 (superchat-workflow--load input-name)))
                  (should
                   (equal
                    (ucs-normalize-NFC-string actual-name)
                    (ucs-normalize-NFC-string
                     (file-name-base resolved)))))
                (should-not
                 (superchat-workflow--resolve
                  (concat "nested/" input-name)))
                (should-not
                 (superchat-workflow--resolve
                  (concat "../" input-name)))))
          (delete-directory directory t))))))

(ert-deftest workflow/edit-new-draft-reuses-saves-and-loads ()
  "Editing a new NFC target preserves its draft until ordinary save."
  (let* ((root (make-temp-file "superchat-workflow-edit-" t))
         (directory (expand-file-name "workflow" root))
         (nfc (concat "caf" (string #x00e9)))
         (nfd (concat "cafe" (string #x0301)))
         (existing-file (expand-file-name "existing.workflow" directory))
         (target (expand-file-name (concat nfc ".workflow") directory))
         first-buffer draft
         (execution-count 0)
         (runtime-count 0))
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file existing-file
            (insert "existing step\n"))
          (let ((superchat-workflow-directory directory))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _)
                         (should (member "existing" collection))
                         nfd))
                      ((symbol-function 'superchat-workflow-execute-async)
                       (lambda (&rest _)
                         (cl-incf execution-count)))
                      ((symbol-function 'superchat-runtime-submit)
                       (lambda (&rest _)
                         (cl-incf runtime-count))))
              (call-interactively #'superchat-workflow-edit))
            (setq first-buffer (current-buffer))
            (should (file-equal-p directory
                                  (file-name-directory buffer-file-name)))
            (should (equal nfc (file-name-base buffer-file-name)))
            (should-not (file-exists-p target))
            (should (= 1 (length
                          (superchat-workflow-parse-steps (buffer-string)))))
            (goto-char (point-max))
            (insert "second draft step\n")
            (setq draft (buffer-string))
            (superchat-workflow-edit nfd)
            (should (eq first-buffer (current-buffer)))
            (should (equal draft (buffer-string)))
            (should-not (file-exists-p target))
            (save-buffer)
            (should (file-exists-p target))
            (should (member nfc (superchat-workflow--list)))
            (should (file-equal-p target
                                  (superchat-workflow--resolve nfd)))
            (should (equal draft (superchat-workflow--load nfd)))
            (should (= 0 execution-count))
            (should (= 0 runtime-count))))
      (when (buffer-live-p first-buffer)
        (with-current-buffer first-buffer
          (set-buffer-modified-p nil))
        (kill-buffer first-buffer))
      (delete-directory root t))))

(ert-deftest workflow/edit-existing-nfd-opens-actual-file-unchanged ()
  "An NFC lookup visits an existing NFD recipe without renaming or rewriting it."
  (let* ((directory (make-temp-file "superchat-workflow-edit-nfd-" t))
         (nfc (string #x00e9))
         (nfd (concat "e" (string #x0301)))
         (requested-file (expand-file-name (concat nfd ".workflow") directory))
         (content "# existing NFD recipe\nkeep this exact step\n")
         visited)
    (unwind-protect
        (progn
          (with-temp-file requested-file (insert content))
          (let* ((superchat-workflow-directory directory)
                 (actual-file
                  (car (directory-files directory t "\\.workflow\\'"))))
            (superchat-workflow-edit nfc)
            (setq visited (current-buffer))
            (should (equal (file-truename actual-file)
                           (file-truename buffer-file-name)))
            (should (equal content (buffer-string)))
            (should-not (buffer-modified-p))
            (should (file-exists-p actual-file))
            (should (= 1 (length
                          (directory-files directory nil "\\.workflow\\'"))))))
      (when (buffer-live-p visited)
        (kill-buffer visited))
      (delete-directory directory t))))

(ert-deftest workflow/edit-rejects-invalid-and-unsafe-before-opening ()
  "Invalid basenames and a symlink target fail before a file buffer opens."
  (let* ((root (make-temp-file "superchat-workflow-edit-reject-" t))
         (directory (expand-file-name "workflow" root))
         (outside (expand-file-name "foreign.workflow" root))
         (link (expand-file-name "linked.workflow" directory)))
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file outside (insert "foreign step\n"))
          (make-symbolic-link outside link t)
          (let ((superchat-workflow-directory directory))
            (dolist (name '("" "../foreign" "nested/foreign" "linked"))
              (should-error (superchat-workflow-edit name) :type 'user-error))
            (should-not (find-buffer-visiting outside))
            (should-not (find-buffer-visiting link))))
      (delete-directory root t))))

(ert-deftest workflow/edit-rejects-real-nfc-ambiguity-before-opening ()
  "Two distinct NFC-equivalent files remain unopened when edit is ambiguous."
  (let* ((nfc (string #x00e9))
         (nfd (concat "e" (string #x0301)))
         (directory (make-temp-file "superchat-workflow-edit-ambiguous-" t))
         (nfc-file (expand-file-name (concat nfc ".workflow") directory))
         (nfd-file (expand-file-name (concat nfd ".workflow") directory)))
    (unwind-protect
        (progn
          (with-temp-file nfc-file (insert "NFC step\n"))
          (with-temp-file nfd-file (insert "NFD step\n"))
          (let* ((actual-files (directory-files directory t "\\.workflow\\'"))
                 (normalized-names
                  (mapcar (lambda (file)
                            (ucs-normalize-NFC-string (file-name-base file)))
                          actual-files))
                 (actual-truenames (mapcar #'file-truename actual-files)))
            (skip-unless
             (and (= 2 (length actual-files))
                  (= 1 (length (delete-dups normalized-names)))
                  (= 2 (length (delete-dups actual-truenames)))))
            (let ((superchat-workflow-directory directory))
              (should-error (superchat-workflow-edit nfc) :type 'user-error)
              (dolist (file actual-files)
                (should-not (find-buffer-visiting file))))))
      (delete-directory directory t))))

(ert-deftest workflow/edit-autoloads-in-fresh-emacs ()
  "The edit command is interactive without eagerly loading Workflow."
  (let* ((root (file-name-directory (locate-library "superchat")))
         (directory (make-temp-file "superchat-workflow-edit-autoload-" t))
         (probe (make-temp-file "superchat-workflow-edit-probe-" nil ".el"))
         (output (generate-new-buffer " *superchat-workflow-edit-output*"))
         (emacs (executable-find "emacs")))
    (skip-unless (and root emacs))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert
             (format
              ";;; -*- lexical-binding: t; -*-
(progn
  (setq user-emacs-directory %S
        superchat-data-directory %S)
  (require 'superchat)
  (when (featurep 'superchat-workflow)
    (error \"workflow loaded eagerly\"))
  (unless (commandp 'superchat-workflow-edit)
    (error \"edit command is not interactive/autoloaded\"))
  (setq superchat-workflow-directory %S)
  (superchat-workflow-edit \"fresh\")
  (unless (featurep 'superchat-workflow)
    (error \"edit invocation did not load workflow\"))
  (unless (and buffer-file-name
               (not (file-exists-p buffer-file-name)))
    (error \"edit did not create an unsaved visiting buffer\"))
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer))
  (princ \"WORKFLOW-EDIT-AUTOLOAD-OK\\n\"))"
              directory directory directory)))
          (let ((status
                 (call-process emacs nil output nil
                               "-Q" "--batch" "-L" root "-l" probe)))
            (unless (zerop status)
              (ert-fail
               (format "fresh Emacs exited %s:\n%s"
                       status
                       (with-current-buffer output (buffer-string))))))
          (with-current-buffer output
            (should (string-match-p "WORKFLOW-EDIT-AUTOLOAD-OK"
                                    (buffer-string)))))
      (when (buffer-live-p output) (kill-buffer output))
      (when (file-exists-p probe) (delete-file probe))
      (delete-directory directory t))))

;; ═══════════════════════════════════════════════════════
;; Part B — Parsing
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-parse-skips-blank-and-comments ()
  "Parse returns only executable steps, skipping blanks and comments."
  (let ((body "/web-search \"news\"\n\n# this is a comment\n\n/summarize\n"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal 2 (length steps)))
      (should (equal "/web-search \"news\"" (car steps)))
      (should (equal "/summarize" (cadr steps))))))

(ert-deftest workflow-parse-preserves-step-order ()
  "Steps preserve their original order."
  (let ((body "step1\nstep2\nstep3"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal '("step1" "step2" "step3") steps)))))

(ert-deftest workflow--parse-line-extracts-model ()
  "parse-line extracts @model-name."
  (let ((result (superchat-workflow--parse-line "@gpt-4o-mini hello world")))
    (should (equal "gpt-4o-mini" (plist-get result :model)))
    (should (string-match-p "hello world" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-extracts-command ()
  "parse-line extracts /command args."
  (let ((result (superchat-workflow--parse-line "/web-search emacs workflow")))
    (should (equal "web-search emacs workflow" (plist-get result :command)))
    ;; When prompt is empty and command is set, prompt defaults to command
    (should (equal "web-search emacs workflow" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-extracts-contexts ()
  "parse-line extracts #file references."
  (let ((result (superchat-workflow--parse-line "check the code #src/main.el")))
    (should (equal '("src/main.el") (plist-get result :contexts)))
    (should (string-match-p "check the code" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-plain-prompt ()
  "parse-line returns prompt only when no annotations present."
  (let ((result (superchat-workflow--parse-line "just a plain prompt")))
    (should-not (plist-get result :model))
    (should-not (plist-get result :command))
    (should-not (plist-get result :contexts))
    (should (equal "just a plain prompt" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-empty-after-annotations ()
  "parse-line returns empty prompt when all content is annotations."
  (let ((result (superchat-workflow--parse-line "@gpt-4o /summarize")))
    (should (equal "gpt-4o" (plist-get result :model)))
    (should (equal "summarize" (plist-get result :command)))
    (should (equal "summarize" (plist-get result :prompt)))))

;; ═══════════════════════════════════════════════════════
;; Part B — Variable substitution
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-substitute-expands-input-lang-date ()
  "Substitute replaces $input, $lang, $date."
  (let ((result (superchat-workflow--substitute-vars
                 "Hello $input, lang=$lang, date=$date"
                 '(:input "world" :lang "French" :date "2026-01-01")
                 nil)))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "lang=French" result))
    (should (string-match-p "date=2026-01-01" result))))

;; Need a separate test for leaving unknown vars alone (not tangled with :input)
(ert-deftest workflow-substitute-leaves-unknown-vars-alone ()
  "Unknown $var references are left unchanged."
  (let ((result (superchat-workflow--substitute-vars
                 "Hello $input, $unknown"
                 '(:input "world" :lang "en" :date "2026-01-01")
                 nil)))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "\\$unknown" result))))

(ert-deftest workflow-substitute-expands-result ()
  "Substitute replaces $result with the most recent step output."
  (let ((result (superchat-workflow--substitute-vars
                 "Previous: $result. More: $result."
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("output of step 1" "output of step 2"))))
    (should (string-match-p "output of step 2" result))
    (should-not (string-match-p "\\$result" result))))

(ert-deftest workflow-substitute-expands-stepN ()
  "Substitute replaces $step1, $step2 with respective outputs."
  (let ((result (superchat-workflow--substitute-vars
                 "Step1: $step1 | Step2: $step2"
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("first" "second"))))
    (should (string-match-p "Step1: first" result))
    (should (string-match-p "Step2: second" result))
    (should-not (string-match-p "\\$step1" result))
    (should-not (string-match-p "\\$step2" result))))

(ert-deftest workflow-substitute-does-not-reexpand-injected-step-output ()
  "A variable marker emitted by one step stays literal in the next template."
  (should
   (equal "first | literal $step1"
          (superchat-workflow--substitute-vars
           "$step1 | $step2"
           '(:input "" :lang "en" :date "2026-01-01")
           '("first" "literal $step1")))))

(ert-deftest workflow-substitute-empty-result-when-no-steps ()
  "Substitute replaces $result with empty string when there are no previous steps."
  (let ((result (superchat-workflow--substitute-vars
                 "Summary: $result"
                 '(:input "" :lang "en" :date "2026-01-01")
                 nil)))
    (should (string= "Summary: " result))))

(ert-deftest workflow-substitute-backward-compat-alias ()
  "superchat-workflow-substitute is an alias — calling it works the same way."
  (should (fboundp 'superchat-workflow-substitute))
  ;; Verify it produces the same output as the canonical function
  (let ((canonical (superchat-workflow--substitute-vars
                    "Hello $input"
                    '(:input "world" :lang "en" :date "2026-01-01")
                    nil))
        (aliased (superchat-workflow-substitute
                  "Hello $input"
                  '(:input "world" :lang "en" :date "2026-01-01")
                  nil)))
    (should (string= canonical aliased))))

;; ═══════════════════════════════════════════════════════
;; Part C — Async execution chain (mock Runtime)
;; ═══════════════════════════════════════════════════════

(defvar workflow-test--mock-calls nil
  "List of prompts sent to the mock Runtime during a test run.")
(defvar workflow-test--mock-requests nil
  "List of requests sent to the mock Runtime during a test run.")
(defvar workflow-test--mock-answers nil
  "List of answers the mock Runtime returns, consumed in order.")
(defvar workflow-test--mock-statuses nil
  "List of statuses the mock Runtime returns, consumed in order.")
(defvar workflow-test--mock-step-count 0
  "Number of times the mock Runtime was called.")
(defvar workflow-test--mock-resume-action 'advance
  "Resume action returned while the mock Runtime delivers a result.")

(defun workflow-test--mock-runtime-submit (request)
  "Mock `superchat-runtime-submit'.
Pushes REQUEST onto `workflow-test--mock-requests' and delivers the
next configured answer/status synchronously."
  (push request workflow-test--mock-requests)
  (push (plist-get request :prompt) workflow-test--mock-calls)
  (cl-incf workflow-test--mock-step-count)
  (let ((answer (or (pop workflow-test--mock-answers)
                    "[mock: no answer configured]"))
        (status (or (pop workflow-test--mock-statuses) 'completed)))
    (cl-letf (((symbol-function
                'superchat-workflow--resume-decision-for-step)
               (lambda (&rest _args)
                 (list :action workflow-test--mock-resume-action))))
      (workflow-test--deliver request answer status workflow-test--mock-step-count)))
  (list :run-id (format "run-%d" workflow-test--mock-step-count)
        :status 'accepted))

(ert-deftest workflow/resume-decision-maps-closure-verdicts ()
  "Workflow maps Closure evidence to advance, wait, review, or stop."
  (let (well-formed terminal)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (&rest _args) '(:trace)))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     well-formed
                   terminal))))
      (dolist
          (case
           '(((:verdict valid)
              (:verdict proved :judgment
                        (:terminal-kind completed))
              advance)
             ((:verdict valid) (:verdict open) wait)
             ((:verdict valid) (:verdict unknown) manual-review)
             ((:verdict valid)
              (:verdict proved :judgment (:terminal-kind failed))
              stop)
             ((:verdict invalid) nil stop)
             ((:verdict unknown) nil manual-review)))
        (setq well-formed (nth 0 case)
              terminal (nth 1 case))
        (should
         (eq (nth 2 case)
             (plist-get
              (superchat-workflow--resume-decision
               "session-1" "run-1")
              :action)))))))

(ert-deftest workflow/instance-projection-selects-latest-step-run ()
  "A Workflow Instance view derives step, Run, judgment, and decision."
  (let ((trace
         '((:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-1"
            :run-id "run-old"
            :turn-id "turn-old")
           (:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-2"
            :run-id "run-2"
            :turn-id "turn-2")
           (:type "run.started"
            :correlation-id "other-workflow"
            :actor-id "workflow-step-1"
            :run-id "run-other")
           (:type "run.started"
            :correlation-id "workflow-1"
            :actor-id "workflow-step-1"
            :run-id "run-new"
            :turn-id "turn-new")))
        judged-runs)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (_session-id) trace))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     '(:verdict valid)
                   (push (plist-get query :run-id) judged-runs)
                   '(:verdict proved
                     :judgment (:terminal-kind completed))))))
      (let* ((view
              (superchat-workflow-instance-view
               "session-1" "workflow-1"))
             (steps (plist-get view :steps)))
        (should (equal '(1 2) (mapcar (lambda (step)
                                       (plist-get step :step-id))
                                     steps)))
        (should (equal '("run-new" "run-2")
                       (mapcar (lambda (step) (plist-get step :run-id))
                               steps)))
        (should (equal '(advance advance)
                       (mapcar (lambda (step)
                                 (plist-get step :resume-decision))
                               steps)))
        (should (cl-every
                 (lambda (step)
                   (eq 'proved
                       (plist-get (plist-get step :closure-judgment)
                                  :verdict)))
                 steps))
        (should (equal '("run-2" "run-new") judged-runs))))))

(ert-deftest workflow/open-resume-gate-does-not-advance ()
  "A completed callback cannot advance while Closure still reports open."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("step1 answer" "step2 answer"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             (workflow-test--mock-resume-action 'wait)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--fail-step)
              (lambda (&rest _args) nil)))
    (superchat-workflow--run-step
     0 (mapcar #'superchat-workflow--parse-line '("step1" "step2"))
     '(:input "" :lang "en" :date "2026-01-01") nil nil "workflow-test"
     nil "workflow-session")
    (should (= 1 workflow-test--mock-step-count))))

(ert-deftest workflow/research-resumes-from-instance-projection ()
  "The sample Research Workflow resumes from persisted Closure proofs."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "superchat-workflow-resume-" t))
         (superchat-data-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled t)
         (superchat-buffer-name " *workflow-durable-resume*")
         (workflow-file
          (expand-file-name
           "examples/research.workflow"
           (file-name-directory (locate-library "superchat-workflow"))))
         (calls 0)
         prompts
         finalized)
    (unwind-protect
        (progn
          (superchat-db-close)
          (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _args)
                       (cl-incf calls)
                       (push prompt prompts)
                       (funcall superchat--llm-runtime-observer
                                'terminal
                                (list :status 'completed
                                      :response
                                      (format "research-result-%d" calls)))))
                    ((symbol-function 'superchat-workflow--render-step-header)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat-workflow--finalize)
                     (lambda (&rest _args) (setq finalized t))))
            (let* ((body
                    (with-temp-buffer
                      (insert-file-contents workflow-file)
                      (buffer-string)))
                   (steps
                    (mapcar #'superchat-workflow--parse-line
                            (superchat-workflow-parse-steps body))))
              (with-temp-buffer
                (setq-local superchat--session-id "research-e2e")
                (superchat-workflow--run-step
                 0 steps
                 '(:input "event-sourced agents"
                   :lang "en"
                   :date "2026-01-01")
                 nil (current-buffer) "research-e2e" nil "research-e2e"))))
          ;; Simulate process-local loss: the resume view must come from Tape.
          (clrhash superchat-runtime--runs)
          (superchat-db-close)
          (let* ((view
                  (superchat-workflow-instance-view
                   "research-e2e" "research-e2e"))
                 (steps (plist-get view :steps)))
            (should (= 3 calls))
            (should finalized)
            (should (equal '(1 2 3)
                           (mapcar (lambda (step)
                                     (plist-get step :step-id))
                                   steps)))
            (should (cl-every
                     (lambda (step)
                       (and (stringp (plist-get step :run-id))
                            (eq 'advance
                                (plist-get step :resume-decision))
                            (eq 'proved
                                (plist-get
                                 (plist-get step :closure-judgment)
                                 :verdict))))
                     steps))
            (setq prompts (nreverse prompts))
            (should (string-match-p "event-sourced agents" (nth 0 prompts)))
            (should (string-match-p "research-result-1" (nth 1 prompts)))
            (should (string-match-p "research-result-2" (nth 2 prompts)))))
      (superchat-db-close)
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name))
      (delete-directory directory t))))

(ert-deftest workflow-execute-targets-current-session-view ()
  "Direct execution keeps View/Session positions without forwarding Mode."
  (let* ((target-name (generate-new-buffer-name " *workflow-session*"))
         (target (get-buffer-create target-name))
         seen owner compatibility-slot captured-session)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-workflow--load)
                   (lambda (_) "one step"))
                  ((symbol-function 'superchat-workflow--render-workflow-header)
                   (lambda (&rest _) (push superchat-buffer-name seen)))
                  ((symbol-function 'superchat-workflow--run-step)
                   (lambda (_i _steps _vars _results view-buffer
                            &optional _correlation-id compatibility-argument
                            session-id)
                     (setq owner view-buffer
                           compatibility-slot compatibility-argument
                           captured-session session-id)
                     (push superchat-buffer-name seen))))
          (with-current-buffer target
            (setq-local superchat-mode t)
            (setq-local superchat--session-id "workflow-view-session")
            (setq-local superchat-authority-mode 'explore)
            (superchat-workflow-execute-async "test"))
          (should (equal seen (list target-name target-name)))
          (should (eq owner target))
          (should-not compatibility-slot)
          (should (equal captured-session "workflow-view-session")))
      (when (get-buffer target-name)
        (kill-buffer target-name)))))

(ert-deftest workflow-execute-without-live-view-uses-no-stream-owner ()
  "A missing named View uses a stable Session-owned protocol host."
  (let ((target-name (generate-new-buffer-name " *workflow-headless*"))
        runtime-request
        presentation-buffers
        caller-name)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-workflow--load)
                   (lambda (_) "one step"))
                  ((symbol-function 'superchat-runtime-submit)
                   (lambda (request)
                     (setq runtime-request request)
                     '(:run-id "workflow-headless")))
                  ((symbol-function 'superchat--track-active-run) #'ignore)
                  ((symbol-function
                    'superchat--response-region--prompt-line-position)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'superchat--response-region-insert-transcript)
                   (lambda (&rest _args)
                     (push (current-buffer) presentation-buffers)))
                  ((symbol-function 'superchat--insert-prompt)
                   (lambda (&optional _owner)
                     (push (current-buffer) presentation-buffers))))
          (let ((superchat-buffer-name target-name))
            (with-temp-buffer
              (setq-local superchat--session-id "workflow-headless-session")
              (setq caller-name (buffer-name))
              (superchat-workflow-execute-async "test")))
          (should runtime-request)
          (should (buffer-live-p (plist-get runtime-request :buffer)))
          (should-not (equal caller-name
                             (buffer-name (plist-get runtime-request :buffer))))
          (should (string-prefix-p
                   " *superchat-editor:workflow-headless-session:workflow-protocol*"
                   (buffer-name (plist-get runtime-request :buffer))))
          (should-not (plist-member runtime-request :ephemeral-buffer))
          (should (equal "workflow-headless-session"
                         (plist-get runtime-request :session-id)))
          ;; S13b: the headless step's stream callback is wrapped by the Sink.
          (should (superchat-sink-p (plist-get runtime-request :sink)))
          (should-not presentation-buffers)
          (should-not (get-buffer target-name)))
      (when (get-buffer target-name)
        (kill-buffer target-name)))))

(ert-deftest workflow-step-submits-runtime-request ()
  "Workflow steps submit through Runtime with workflow-scoped metadata."
  (cl-letf* ((superchat-buffer-name " *workflow-step-request*")
             (workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("ok"))
             (workflow-test--mock-statuses '(completed))
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil)))
    (superchat-workflow--run-step
     0 (list (superchat-workflow--parse-line "@gpt-4o-mini step #docs/a.md"))
     '(:input "" :lang "en" :date "2026-01-01") nil nil "workflow-test"
     nil "workflow-session")
    (let ((request (car workflow-test--mock-requests)))
      (should (equal "step" (plist-get request :prompt)))
      (should (equal "gpt-4o-mini" (plist-get request :target-model)))
      (should (equal '("docs/a.md") (plist-get request :context-files)))
      (should (equal "workflow-step-1" (plist-get request :actor-id)))
      (should (equal "workflow-test" (plist-get request :correlation-id)))
      (should (equal "workflow-session" (plist-get request :session-id)))
      (should (equal "workflow" (plist-get request :capability)))
      (should (eq 'workflow (plist-get request :topology)))
      (should (equal "default" (plist-get request :authority-policy)))
      (should (eq 'workflow (plist-get request :surface)))
      (should (eq nil (plist-get request :record-conversation)))
      (should-not (plist-member request :ephemeral-buffer))
      (should (eq (plist-get request :buffer)
                  (superchat-editor-workspace-buffer
                   "workflow-session" 'workflow-protocol)))
      (should (superchat-sink-p (plist-get request :sink)))
      ;; S13b: the owner protocol keeps travelling on the Runtime Request.
      (should (functionp (plist-get request :accepted-function)))
      (should (functionp (plist-get request :owner-stop-function)))
      (should (stringp (plist-get request :transcript-owner))))))

(ert-deftest workflow/step-envelope-preserves-owner-protocol-extras ()
  "S13b: a workflow step submits an envelope whose `:runtime-extras' carry
the owner protocol untouched, the prepared prompt as `:context
:prepared-prompt', the host as `:context :host-buffer', and a callback Sink."
  (let* ((request (list :type :llm-query :prompt "step prompt" :clean-input "step prompt"
                        :buffer (current-buffer) :buffer-name (buffer-name)
                        :target-model "m" :context-files '("a.png")
                        :session-id "wf-session" :actor-id "workflow-step-1"
                        :correlation-id "wf-1" :capability "workflow" :topology 'workflow
                        :authority-policy "default" :surface 'workflow
                        :origin '(:surface workflow) :record-conversation nil
                        :transcript-owner "wf-1" :stream-function #'ignore
                        :accepted-function #'ignore :owner-stop-function #'ignore
                        :delivery-function #'ignore :input-core '(:kind test)))
         (envelope (superchat-workflow--envelope request))
         (extras (plist-get envelope :runtime-extras))
         (context (plist-get envelope :context)))
    (should (eq 'workflow (plist-get envelope :surface)))
    (should (equal "step prompt" (plist-get envelope :content)))
    (should (equal "step prompt" (plist-get context :prepared-prompt)))
    (should (eq (current-buffer) (plist-get context :host-buffer)))
    (should (equal '("a.png") (plist-get envelope :media)))
    (should (equal "wf-session" (plist-get envelope :session-id)))
    (should (equal "m" (plist-get envelope :target-model)))
    (should (eq 'workflow (plist-get envelope :topology)))
    (should (superchat-sink-p (plist-get envelope :sink)))
    (dolist (key '(:transcript-owner :accepted-function :owner-stop-function
                   :actor-id :correlation-id :input-core))
      (should (plist-member extras key)))
    (should (plist-member extras :record-conversation))
    (should-not (plist-get extras :record-conversation))
    (dolist (key '(:buffer :buffer-name :prompt :input :stream-function :delivery-function))
      (should-not (plist-member envelope key))
      (should-not (plist-member extras key)))))

(ert-deftest workflow/dead-view-keeps-session-and-reuses-owned-role-host ()
  "A dead View cannot replace Session identity with correlation identity."
  (let* ((session-id "workflow-stable-session")
         (view (superchat-editor-workspace-buffer
                session-id 'session-view t))
         (_ (with-current-buffer view
              (setq-local superchat--session-id session-id)))
         requests
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-runtime-submit)
                   (lambda (request)
                     (push request requests)
                     (cl-incf calls)
                     (when (= calls 1)
                       (kill-buffer view))
                     (cl-letf (((symbol-function
                                 'superchat-workflow--resume-decision-for-step)
                                (lambda (&rest _args) '(:action advance))))
                       (workflow-test--deliver request "ok" 'completed calls))
                     (list :run-id (format "workflow-run-%d" calls)
                           :status 'accepted)))
                  ((symbol-function 'superchat-workflow--render-step-header)
                   #'ignore)
                  ((symbol-function 'superchat-workflow--ensure-view-prompt)
                   #'ignore)
                  ((symbol-function 'superchat--record-message) #'ignore))
          (superchat-workflow--run-step
           0 (mapcar #'superchat-workflow--parse-line '("one" "two"))
           '(:input "" :lang "en" :date "2026-01-01") nil view
           "workflow-correlation" 'ignored-authority-mode session-id)
          (setq requests (nreverse requests))
          (should (= 2 (length requests)))
          (should (cl-every
                   (lambda (request)
                     (and (equal session-id
                                 (plist-get request :session-id))
                          (equal "default"
                                 (plist-get request :authority-policy))
                          (not (equal "workflow-correlation"
                                      (plist-get request :session-id)))
                          (not (plist-member request :ephemeral-buffer))
                          (not (plist-member request :host-authority-mode))))
                   requests))
          (let ((protocol-host (plist-get (cadr requests) :buffer)))
            (should (buffer-live-p protocol-host))
            (should (eq protocol-host
                        (superchat-editor-workspace-buffer
                         session-id 'workflow-protocol)))
            (should (eq protocol-host
                        (superchat-editor-workspace-validate-buffer
                         protocol-host session-id)))))
      (ignore-errors (superchat-session-workspace-close session-id)))))

(ert-deftest workflow-async-chain-executes-all-steps ()
  "Async executor submits Runtime requests for each step in sequence."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("step1 answer" "step2 answer" "step3 answer"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ;; Prevent buffer operations from failing
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count &optional _owner) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total &optional _owner) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk &optional _owner) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results &optional _owner) nil)))
    (let* ((raw-steps '("step1" "step2" "step3"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step
       0 parsed vars nil nil nil nil "workflow-chain-session"))
    (should (= 3 workflow-test--mock-step-count))
    ;; Calls are pushed in reverse order; check we got all three prompts
    (should (= 3 (length workflow-test--mock-calls)))))

(ert-deftest workflow-async-chain-passes-result-variable ()
  "Async executor injects $result from previous step into next prompt."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("result of step one" "result of step two"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count &optional _owner) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total &optional _owner) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk &optional _owner) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results &optional _owner) nil)))
    (let* ((raw-steps '("step one" "continue: $result"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step
       0 parsed vars nil nil nil nil "workflow-result-session"))
    (should (= 2 workflow-test--mock-step-count))
    ;; The second prompt should have $result expanded to "result of step one"
    ;; (calls are in reverse order, so second call is (car workflow-test--mock-calls))
    (let ((second-prompt (car workflow-test--mock-calls)))
      (should second-prompt)
      (should (string-match-p "result of step one" second-prompt))
      (should-not (string-match-p "\\$result" second-prompt)))))

(ert-deftest workflow-async-chain-passes-stepN-variables ()
  "Async executor injects $step1, $step2 from prior step outputs."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers
              '("first output" "second output" "third output"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count &optional _owner) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total &optional _owner) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk &optional _owner) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results &optional _owner) nil)))
    (let* ((raw-steps '("step1" "step2" "summary: $step1 vs $step2"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step
       0 parsed vars nil nil nil nil "workflow-stepn-session"))
    (should (= 3 workflow-test--mock-step-count))
    ;; Third prompt should reference both previous outputs
    (let ((third-prompt (car workflow-test--mock-calls)))
      (should third-prompt)
      (should (string-match-p "first output" third-prompt))
      (should (string-match-p "second output" third-prompt)))))

(ert-deftest workflow-async-chain-model-override ()
  "Async executor passes @model override to the Runtime request."
  (let ((captured-model nil))
    (cl-letf* ((workflow-test--mock-calls nil)
               (workflow-test--mock-requests nil)
               (workflow-test--mock-answers '("answer"))
               (workflow-test--mock-statuses nil)
               (workflow-test--mock-step-count 0)
               ((symbol-function 'superchat-runtime-submit)
                (lambda (request)
                  (setq captured-model (plist-get request :target-model))
                  (workflow-test--mock-runtime-submit request)))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count &optional _owner) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total &optional _owner) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk &optional _owner) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results &optional _owner) nil)))
      (let* ((raw-steps '("@gpt-4o-mini hello"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (superchat-workflow--run-step
         0 parsed vars nil nil nil nil "workflow-model-session"))
      (should (equal "gpt-4o-mini" captured-model)))))

(ert-deftest workflow-async-chain-reuses-correlation-id ()
  "All steps in one workflow share one Runtime correlation id."
  (cl-letf* ((workflow-test--mock-calls nil)
             (workflow-test--mock-requests nil)
             (workflow-test--mock-answers '("one" "two"))
             (workflow-test--mock-statuses nil)
             (workflow-test--mock-step-count 0)
             ((symbol-function 'superchat-runtime-submit)
              #'workflow-test--mock-runtime-submit)
             ((symbol-function 'superchat-workflow--render-workflow-header)
              (lambda (_name _count &optional _owner) nil))
             ((symbol-function 'superchat-workflow--render-step-header)
              (lambda (_n _total &optional _owner) nil))
             ((symbol-function 'superchat-workflow--stream-chunk)
              (lambda (_chunk &optional _owner) nil))
             ((symbol-function 'superchat--record-message)
              (lambda (&rest _args) nil))
             ((symbol-function 'superchat-workflow--finalize)
              (lambda (_steps _results &optional _owner) nil)))
    (let* ((raw-steps '("first" "second"))
           (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
           (vars '(:input "" :lang "en" :date "2026-01-01")))
      (superchat-workflow--run-step
       0 parsed vars nil nil "workflow-test" nil "workflow-session"))
    (should (= 2 (length workflow-test--mock-requests)))
    (let ((correlations
           (mapcar (lambda (request)
                     (plist-get request :correlation-id))
                   workflow-test--mock-requests))
          (sessions
           (mapcar (lambda (request)
                     (plist-get request :session-id))
                   workflow-test--mock-requests)))
      (should (equal (car correlations) "workflow-test"))
      (should (equal (car sessions) (cadr sessions))))))

(ert-deftest workflow-async-chain-sync-error-stops-chain ()
  "A synchronous Runtime error triggers fail-step and stops the chain."
  (let ((runtime-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat-runtime-submit)
                (lambda (_request)
                  (cl-incf runtime-calls)
                  (error "simulated Runtime failure")))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count &optional _owner) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total &optional _owner) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk &optional _owner) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results &optional _owner) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num total error-msg _step-results &optional _owner)
                  (setq fail-called (list step-num total error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (with-temp-buffer
          (setq-local superchat--session-id "workflow-sync-session")
          (superchat-workflow--run-step
           0 parsed vars nil (current-buffer) nil nil "workflow-sync-session"))))
    ;; Error on step 1: exactly one Runtime call, fail-step invoked, no finalize.
    (should (= 1 runtime-calls))
    (should (equal '(1 3) (list (nth 0 fail-called) (nth 1 fail-called))))
    (should (string-match-p "simulated Runtime failure" (nth 2 fail-called)))
    (should-not finalized)))

(ert-deftest workflow-async-chain-failed-status-stops-chain ()
  "A failed Runtime status stops the chain via fail-step."
  (let ((runtime-calls 0)
        (fail-called nil)
        (finalized nil))
    (cl-letf* (((symbol-function 'superchat-runtime-submit)
                (lambda (request)
                  (cl-incf runtime-calls)
                  (workflow-test--deliver request "[Error: backend unreachable]" 'failed 9)))
               ((symbol-function 'superchat-workflow--render-workflow-header)
                (lambda (_name _count &optional _owner) nil))
               ((symbol-function 'superchat-workflow--render-step-header)
                (lambda (_n _total &optional _owner) nil))
               ((symbol-function 'superchat-workflow--stream-chunk)
                (lambda (_chunk &optional _owner) nil))
               ((symbol-function 'superchat--record-message)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat-workflow--finalize)
                (lambda (_steps _results &optional _owner) (setq finalized t)))
               ((symbol-function 'superchat-workflow--fail-step)
                (lambda (step-num _total error-msg _step-results &optional _owner)
                  (setq fail-called (list step-num error-msg)))))
      (let* ((raw-steps '("step1" "step2" "step3"))
             (parsed (mapcar #'superchat-workflow--parse-line raw-steps))
             (vars '(:input "" :lang "en" :date "2026-01-01")))
        (with-temp-buffer
          (setq-local superchat--session-id "workflow-failed-session")
          (superchat-workflow--run-step
           0 parsed vars nil (current-buffer) nil nil "workflow-failed-session"))))
    (should (= 1 runtime-calls))
    (should (equal 1 (car fail-called)))
    (should (string-match-p "backend unreachable" (cadr fail-called)))
    (should-not finalized)))

;; ═══════════════════════════════════════════════════════
;; `>>' prefix parsing (>> = workflow, > = skill)
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-double-arrow-prefix-parses-to-workflow-skill ()
  "\">>name args\" parses to the reserved `workflow' skill token."
  (let ((turn (superchat-core--parse-input
               (superchat-turn-new ">>research emacs news"))))
    (should (equal "workflow" (superchat-turn-skill turn)))
    (should (equal "research emacs news" (superchat-turn-clean-input turn)))))

(ert-deftest workflow-double-arrow-without-name ()
  "\">>\" with no name still routes to the workflow branch."
  (let ((turn (superchat-core--parse-input (superchat-turn-new ">>"))))
    (should (equal "workflow" (superchat-turn-skill turn)))
    (should (equal "" (superchat-turn-clean-input turn)))))

(ert-deftest workflow/double-arrow-autoloads-in-fresh-emacs ()
  "The first >> invocation loads the workflow plugin in a new Emacs process."
  (let* ((root (file-name-directory (locate-library "superchat")))
         (data-directory (make-temp-file "superchat-workflow-autoload-" t))
         (workflow-directory (expand-file-name "workflow" data-directory))
         (workflow-file (expand-file-name "fresh.workflow" workflow-directory))
         (probe (make-temp-file "superchat-workflow-autoload-probe-" nil ".el"))
         (output (generate-new-buffer " *superchat-workflow-autoload-output*"))
         (emacs (executable-find "emacs")))
    (skip-unless (and root emacs))
    (unwind-protect
        (progn
          (make-directory workflow-directory t)
          (with-temp-file workflow-file
            (insert "first fresh workflow step\n"))
          (with-temp-file probe
            (insert
             (format
              "(progn
  (setq load-prefer-newer t
        user-emacs-directory %S)
  (require 'superchat)
  (when (featurep 'superchat-workflow)
    (error \"workflow loaded before the first >> invocation\"))
  (let ((superchat-data-directory %S)
        (superchat-workflow-directory %S)
        (superchat-memory-auto-recall-min-length 10000)
        (superchat-show-ttft nil)
        (superchat-lang \"English\")
        submitted)
    (with-temp-buffer
      (setq-local superchat--session-id \"h1-session\"
                  superchat-buffer-name (buffer-name))
      (insert \">>fresh demo\")
      (setq-local superchat--prompt-start (copy-marker (point-min)))
      (cl-letf (((symbol-function 'superchat-runtime-submit)
                 (lambda (request)
                   (setq submitted request)
                   (list :run-id \"run-h1\" :status 'accepted))))
        (superchat-send-input)))
    (unless (featurep 'superchat-workflow)
      (error \"workflow was not loaded by >>\"))
    (unless (and submitted
                 (equal \"workflow\" (plist-get submitted :capability))
                 (equal \"first fresh workflow step\"
                        (plist-get submitted :prompt)))
      (error \">> did not submit a workflow request: %%S\" submitted))
    (princ \"H1-FRESH-AUTOLOAD-OK\\n\")))"
              data-directory data-directory workflow-directory)))
          (let ((status
                 (call-process emacs nil output nil
                               "-Q" "--batch" "-L" root "-l" probe)))
            (unless (zerop status)
              (ert-fail
               (format "fresh Emacs exited %s:\n%s"
                       status
                       (with-current-buffer output
                         (buffer-string))))))
          (with-current-buffer output
            (should (string-match-p "H1-FRESH-AUTOLOAD-OK"
                                    (buffer-string)))))
      (when (buffer-live-p output)
        (kill-buffer output))
      (when (file-exists-p probe)
        (delete-file probe))
      (when (file-directory-p data-directory)
        (delete-directory data-directory t)))))

(ert-deftest workflow-single-arrow-still-parses-skill ()
  "\">name\" remains a skill invocation, untouched by the workflow prefix."
  (let ((turn (superchat-core--parse-input
               (superchat-turn-new ">coder write a fn"))))
    (should (equal "coder" (superchat-turn-skill turn)))
    (should (equal "write a fn" (superchat-turn-clean-input turn)))))

;; ═══════════════════════════════════════════════════════
;; Regression tests — parser and substitution edge cases
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-substitute-step10-not-clobbered-by-step1 ()
  "$step10 must not be corrupted by the $step1 replacement."
  (let ((result (superchat-workflow--substitute-vars
                 "A=$step1 B=$step10"
                 '(:input "" :lang "en" :date "2026-01-01")
                 '("one" "2" "3" "4" "5" "6" "7" "8" "9" "ten"))))
    (should (string= "A=one B=ten" result))))

(ert-deftest workflow--parse-line-ignores-inline-at-mention ()
  "A mid-line @word is prose, not a model override."
  (let ((result (superchat-workflow--parse-line
                 "Summarize feedback from @john.doe about the release")))
    (should-not (plist-get result :model))
    (should (string-match-p "@john\\.doe" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-ignores-inline-hash-number ()
  "A #token without `.' or `/' is prose, not a context file."
  (let ((result (superchat-workflow--parse-line
                 "Translate issue #42 into French")))
    (should-not (plist-get result :contexts))
    (should (string-match-p "#42" (plist-get result :prompt)))))

(ert-deftest workflow--parse-line-ignores-mid-line-slash ()
  "A mid-line /word is prose, not a command."
  (let ((result (superchat-workflow--parse-line
                 "Compare foo/bar with the baseline")))
    (should-not (plist-get result :command))
    (should (string-match-p "foo/bar" (plist-get result :prompt)))))

(ert-deftest workflow-resolve-prompt-expands-command-template ()
  "/command steps expand the command's prompt template with $input."
  (cl-letf (((symbol-function 'superchat--lookup-command-template)
             (lambda (name)
               (when (string= name "summarize")
                 "Summarize the following: $input"))))
    (let ((prompt (superchat-workflow--resolve-prompt
                   "summarize $result" "summarize $result"
                   '(:input "" :lang "en" :date "2026-01-01")
                   '("step one output"))))
      (should (string= "Summarize the following: step one output" prompt)))))

(ert-deftest workflow-resolve-prompt-falls-back-without-template ()
  "Steps whose /command has no template fall back to the raw prompt."
  (cl-letf (((symbol-function 'superchat--lookup-command-template)
             (lambda (_name) nil)))
    (let ((prompt (superchat-workflow--resolve-prompt
                   "unknown-cmd foo" "unknown-cmd foo"
                   '(:input "" :lang "en" :date "2026-01-01")
                   nil)))
      (should (string= "unknown-cmd foo" prompt)))))

;; ═══════════════════════════════════════════════════════
;; Legacy import tests (v0.7)
;; ═══════════════════════════════════════════════════════

(ert-deftest workflow-import-legacy-synthesises-skill ()
  "Importing a .workflow file creates a valid SKILL.md."
  (let* ((tmp-dir (make-temp-file "wf-import" t))
         (legacy-file (expand-file-name "test.workflow" tmp-dir))
         (target-dir (expand-file-name "skills" tmp-dir)))
    (unwind-protect
        (progn
          ;; Write a fake legacy .workflow file
          (with-temp-file legacy-file
            (insert "/web-search \"news\"\n/summarize\n"))
          ;; Import it
          (let ((names (superchat-workflow-import-legacy-dir
                        tmp-dir target-dir)))
            (should (equal 1 (length names)))
            (let ((skill-file (expand-file-name
                               "skill-test/SKILL.md"
                               target-dir)))
              ;; Check SKILL.md was created
              (should (file-exists-p skill-file))
              (with-temp-buffer
                (insert-file-contents skill-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "name: test" content))
                  (should (string-match-p "type: workflow" content))
                  (should (string-match-p "/web-search" content))
                  (should (string-match-p "/summarize" content)))))))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest workflow/research-recovery-commands-discover-persisted-instances ()
  "Research recovery commands complete persisted instance ids."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((directory (make-temp-file "superchat-research-discovery-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-workspace--registry nil)
         (superchat--subagent-running nil)
         (superchat-subagent-timeout nil)
         first second choices)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (cl-letf (((symbol-function 'superchat--execute-llm-query)
                     (lambda (turn)
                       (list :prompt (superchat-turn-prompt turn)
                             :system-prompt "system"
                             :tools 'none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _) nil)))
            (setq first (superchat-research-workflow "First persisted study")
                  second (superchat-research-workflow "Second persisted study")))
          (clrhash superchat-runtime--runs)
          (setq superchat-workspace--registry nil
                superchat--subagent-running nil)
          (superchat-db-close)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (push collection choices)
                       (car collection))))
            (call-interactively #'superchat-research-workflow-status)
            (call-interactively #'superchat-research-workflow-resume))
          (should (= 2 (length choices)))
          (dolist (collection choices)
            (should (equal (sort (copy-sequence collection) #'string<)
                           (sort (list first second) #'string<)))))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest workflow/research-team-survives-runtime-restart ()
  "Collector, analyst, and reviewer recover from Tape and durable Artifacts."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((directory (make-temp-file "superchat-research-team-" t))
         (database (expand-file-name "superchat.db" directory))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat--subagent-running nil)
         (superchat-subagent-timeout nil)
         (superchat-closure-terminal-guard-enabled t)
         (calls 0)
         prompts
         instance-id)
    (unwind-protect
        (progn
          (superchat-db-open database)
          (cl-letf (((symbol-function 'superchat--execute-llm-query)
                     (lambda (turn)
                       (list :prompt (superchat-turn-prompt turn)
                             :system-prompt "system"
                             :tools 'none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _args)
                       (cl-incf calls)
                       (push prompt prompts)
                       (let* ((role
                               (cond
                                ((string-match-p "\\[research/collector\\]"
                                                 prompt)
                                 "collector")
                                ((string-match-p "\\[research/analyst\\]"
                                                 prompt)
                                 "analyst")
                                ((string-match-p "\\[research/reviewer\\]"
                                                 prompt)
                                 "reviewer")
                                (t (error "Unknown research stage"))))
                              (context
                               (plist-put
                                (copy-sequence
                                 superchat--tool-execution-context)
                                :approval-function (lambda (_request) t))))
                         (superchat-tool-execute
                          "workspace_publish"
                          #'superchat-workspace-publish
                          (list (format "%s artifact" role) role)
                          :context context)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  (list :status 'completed
                                        :response
                                        (format "%s complete" role)))))))
            ;; Drop the first completion callback to model Emacs exiting after
            ;; Collector committed its Run and Artifact but before Analyst was
            ;; launched.  Resume must derive the next action from Tape.
            (let ((run-subagent
                   (symbol-function 'superchat--subagent-run-async))
                  (drop-next-callback t))
              (cl-letf
                  (((symbol-function 'superchat--subagent-run-async)
                    (lambda (preset task context callback &optional depth)
                      (funcall
                       run-subagent preset task context
                       (lambda (report)
                         (if drop-next-callback
                             (setq drop-next-callback nil)
                           (funcall callback report)))
                       depth))))
                (setq instance-id
                      (superchat-research-workflow
                       "How can event-sourced agents resume safely?"))))
            (should (= 1 calls))
            ;; Simulate an Emacs restart: only SQLite-backed facts survive.
            (clrhash superchat-runtime--runs)
            (setq superchat-workspace--registry nil
                  superchat--subagent-running nil)
            (superchat-db-close)
            (let* ((view (superchat-research-workflow-view instance-id))
                   (steps (plist-get view :steps)))
              (should
               (equal '(advance start blocked)
                      (mapcar
                       (lambda (step) (plist-get step :resume-decision))
                       steps))))
            (should (eq 'completed
                        (plist-get
                         (superchat-research-workflow-resume instance-id)
                         :status)))
            (should (= 3 calls))
            ;; The model-facing reference snapshot must preserve JSON array
            ;; semantics at every cardinality, including zero and one.
            (dolist (expected '((collector)
                                (analyst "collector")
                                (reviewer "collector" "analyst")))
              (let* ((role (car expected))
                     (prompt
                      (cl-find-if
                       (lambda (item)
                         (string-match-p
                          (format "\\[research/%s\\]" role) item))
                       prompts)))
                (should
                 (string-match
                  "Prior Artifact references: \\(.*\\)\\'" prompt))
                (let* ((json (match-string 1 prompt))
                       (references
                        (json-parse-string json
                                           :object-type 'plist
                                           :array-type 'list)))
                  (should (string-prefix-p "[" json))
                  (should
                   (equal (cdr expected)
                          (mapcar
                           (lambda (reference)
                             (plist-get reference :role))
                           references)))
                  (dolist (reference references)
                    (should (stringp (plist-get reference :artifact-id)))
                    (should (stringp (plist-get reference :revision)))))))
            ;; A second restart must still answer all three recovery questions.
            (clrhash superchat-runtime--runs)
            (setq superchat-workspace--registry nil
                  superchat--subagent-running nil)
            (superchat-db-close)
            (let* ((view (superchat-research-workflow-view instance-id))
                   (steps (plist-get view :steps)))
              (should (equal '(collector analyst reviewer)
                             (mapcar (lambda (step) (plist-get step :role))
                                     steps)))
              (should (cl-every
                       (lambda (step)
                         (and (eq 'advance (plist-get step :resume-decision))
                              (eq 'deliver (plist-get step :run-decision))
                              (cl-every
                               (lambda (artifact)
                                 (and (eq 'deliver
                                          (plist-get artifact
                                                     :delivery-decision))
                                      (plist-get artifact :available)))
                               (plist-get step :artifacts))))
                       steps))
              ;; A trusted Run does not make externally changed bytes safe.
              (let* ((reviewer (car (last steps)))
                     (artifact (car (plist-get reviewer :artifacts)))
                     (path (substring (plist-get artifact :uri)
                                      (length "file://"))))
                (with-temp-file path (insert "tampered"))
                (let* ((changed
                        (superchat-research-workflow-view instance-id))
                       (changed-reviewer
                        (car (last (plist-get changed :steps)))))
                  (should (eq 'deliver
                              (plist-get changed-reviewer :run-decision)))
                  (should (eq 'manual-review
                              (plist-get changed-reviewer :resume-decision)))
                  (should-not
                   (plist-get (car (plist-get changed-reviewer :artifacts))
                              :available)))))))
      (superchat-db-close)
      (delete-directory directory t))))

;;; Public Workflow execution ownership, with native transport-only fixtures.
;; Reuse the existing native SSE/async fixture rather than replace Runtime,
;; Authority, or Closure.  Loading this file does not run its ERT tests.
(eval-and-compile
  (unless (fboundp 'test-input-batch--fixture)
    (load (expand-file-name "test-llm-backend.el"
                            (file-name-directory (or load-file-name buffer-file-name)))
          nil t)))

(defvar workflow-owner-test--dispatch nil)

(defmacro workflow-owner-test--fixture (mode &rest body)
  "Use the existing native parser fixture with a real View and recipe."
  (declare (indent 1) (debug t))
  `(test-input-batch--fixture ,mode
     (require 'superchat)
     (let* ((superchat-workflow-directory (expand-file-name "workflow" directory))
            (superchat-editor-workspace--states (make-hash-table :test #'equal))
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-llm-tool-names '("input_batch_effect"))
            (superchat-buffer-name (generate-new-buffer-name " *workflow-owner*"))
            (workflow-owner-test--dispatch nil)
            (endpoint (symbol-function 'llm-request-plz-async))
            view session)
       (unwind-protect
           (progn
             (make-directory superchat-workflow-directory t)
             (with-temp-file (expand-file-name "chain.workflow" superchat-workflow-directory)
               (insert "First $input\nSecond $result\nThird $step1\n"))
             (save-window-excursion
               (call-interactively #'superchat)
               (setq view (current-buffer) session superchat--session-id)
               (setq-local superchat-authority-mode 'auto
                           superchat-authority--session-allowlist '("input_batch_effect"))
               (goto-char (point-max))
               (insert (propertize "PRIVATE draft\nsecond line" 'workflow-draft t))
               (cl-letf (((symbol-function 'llm-request-plz-async)
                          (lambda (&rest args)
                            (prog1 (apply endpoint args)
                              (should (eq (and (plist-get (car test-input-batch--requests)
                                                         :media-type) t)
                                          (eq ,mode 'streaming)))
                              (when workflow-owner-test--dispatch
                                (funcall workflow-owner-test--dispatch
                                         (car test-input-batch--requests)))))))
                 ,@body)))
         (maphash (lambda (id _) (superchat-runtime-cancel id "fixture cleanup"))
                  superchat-runtime--runs)
         (when (buffer-live-p view)
           (with-current-buffer view (superchat--cancel-status-refresh-timer))
           (kill-buffer view))
         (dolist (buffer (superchat-editor-workspace-owned-buffers session))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest producer-ask/workflow-real-default-factory ()
  (dolist (mode '(async streaming))
    (dolist (action '(answer stop expiry))
      (workflow-owner-test--fixture mode
        (let ((superchat-runtime-default-deadline 1)
              (superchat-turn-completed-functions '(superchat--notify-run-terminal)))
          (superchat-workflow-execute-async "chain" "city")
          (let* ((run superchat--active-run-id) (request (car test-input-batch--requests)))
            (should (member "ask_user" (mapcar #'llm-tool-name
                                               (llm-chat-prompt-tools (plist-get request :prompt)))))
            (test-trusted-ask--reply request)
            (let* ((q (superchat-runtime-pending-input run))
                   (group (superchat--input-panel-find view session run (plist-get q :handle))))
              (should group)
              (should (eq 'workflow (plist-get (widget-get group :input-snapshot) :presentation-scope)))
              (pcase action
                ('answer
                 (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
                   (superchat--input-panel-action group 'answer))
                 (should (= 2 (length test-input-batch--requests)))
                 (test-input-batch--reply (car test-input-batch--requests) nil)
                 (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
                 (should (= 3 (length test-input-batch--requests))))
                ('stop
                 (superchat--input-panel-action group 'stop)
                 (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status))))
                ('expiry
                 (let ((bound (+ (float-time) 3)))
                   (while (and (< (float-time) bound)
                               (not (superchat-runtime--terminal-p
                                     (plist-get (superchat-runtime-status run) :status))))
                     (sleep-for 0.01)))
                 (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
                 (should (equal "Expired" (widget-value (nth 1 (widget-get group :children)))))))
              (unless (eq action 'answer) (should (= 1 (length test-input-batch--requests)))))))))))

(ert-deftest workflow-owner/accepted-tracking-before-native ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let (tracked-at-dispatch)
        (setq workflow-owner-test--dispatch
              (lambda (_)
                (push (buffer-local-value 'superchat--active-run-id view)
                      tracked-at-dispatch)))
        (let ((handle (superchat-workflow-execute-async "chain" "ARG")))
          (should (equal (list (plist-get handle :run-id)) tracked-at-dispatch))
          (should (= 1 (length test-input-batch--requests))))))))

(ert-deftest workflow-owner/stale-returns-and-public-stop ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let (runs)
        (setq workflow-owner-test--dispatch
              (lambda (request)
                (push (buffer-local-value 'superchat--active-run-id view) runs)
                (when (< (length test-input-batch--requests) 3)
                  (test-input-batch--reply request nil))))
        (let ((first (superchat-workflow-execute-async "chain")))
          (should (= 3 (length test-input-batch--requests)))
          (should (cl-every #'stringp runs))
          (should (equal (plist-get first :run-id) (car (last runs))))
          (should (equal (car runs) (buffer-local-value 'superchat--active-run-id view)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
            (call-interactively #'superchat-cancel))
          (should (plist-get (superchat-runtime-status (car runs)) :stop-requested))
          (should (eq 'cancelled (plist-get (superchat-runtime-status (car runs)) :status)))
          (should (= 3 (length test-input-batch--requests))))))))

(ert-deftest workflow-owner/entered-ancestor-stops-current-not-independent ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let* ((first (superchat-workflow-execute-async "chain"))
             (a (plist-get first :run-id))
             (a-request (car test-input-batch--requests))
             (independent (superchat-workflow-execute-async "chain" "D"))
             (d (plist-get independent :run-id))
             (d-request (car test-input-batch--requests))
             (run-step (symbol-function 'superchat-workflow--run-step))
             (reply-effect
              (lambda (request value)
                (if (not (plist-get request :media-type))
                    (test-input-batch--reply request '("input_batch_effect") value)
                  (let* ((media (cdr (plist-get request :media-type)))
                         (success (plist-get request :on-success))
                         (packets
                          (list `(:tool_calls [(:index 0 :id ,(format "call-%s-0" (length test-input-batch--requests))
                                                :type "function" :function
                                                (:name "input_batch_effect" :arguments "{\"value\":"))])
                                `(:tool_calls [(:index 0 :function
                                                (:arguments ,(concat (decode-coding-string (json-serialize value) 'utf-8) "}")))])))
                         (expected (mapcar (lambda (delta)
                                             (decode-coding-string
                                              (json-serialize `(:choices [(:delta ,delta)])) 'utf-8))
                                           packets))
                         source returned)
                    ;; This fixture declares both argument packets delivered
                    ;; before native success.  Observe original handler RETURN
                    ;; for this exact source, never an Authority count or timer.
                    (should-not (plist-get request :source))
                    (setq source
                          (plz-event-source-open
                           (plz-event-source-buffer
                            :buffer (buffer-name (generate-new-buffer " *workflow-effect-sse*"))
                            :handlers
                            (mapcar
                             (lambda (pair)
                               (if (not (eq (car pair) 'message)) pair
                                 (cons (car pair)
                                       (lambda (event)
                                         (prog1 (funcall (cdr pair) event)
                                           (when (and (eq source (plist-get request :source))
                                                      (member (oref event data) expected))
                                             (push (oref event data) returned)))))))
                             (oref media events)))))
                    (setf (plist-get request :source) source
                          (plist-get request :on-success)
                          (lambda (&rest args)
                            (let ((bound (+ (float-time) 2)))
                              (while (and (not (cl-every (lambda (packet) (member packet returned)) expected))
                                          (< (float-time) bound))
                                (sleep-for 0.01)))
                            (should (eq source (plist-get request :source)))
                            (should (cl-every (lambda (packet) (member packet returned)) expected))
                            (apply success args)))
                    (unwind-protect
                        (test-input-batch--reply request '("input_batch_effect") value)
                      (setf (plist-get request :on-success) success))))))
             b c stop-result)
        (setq workflow-owner-test--dispatch
              (lambda (request)
                (if (= 3 (length test-input-batch--requests))
                    (progn
                      (setq b superchat--active-run-id)
                      (test-input-batch--reply request nil))
                  (setq c superchat--active-run-id))))
        (cl-letf (((symbol-function 'superchat-workflow--run-step)
                   (lambda (i &rest args)
                     (prog1 (apply run-step i args)
                       ;; B returned, C accepted, but A delivery still entered.
                       (when (= i 1)
                         (setq stop-result (superchat-runtime-cancel a "Stop A")))))))
          (test-input-batch--reply a-request nil))
        (should stop-result)
        (should (stringp b)) (should (stringp c))
        (should (eq 'completed (plist-get (superchat-runtime-status a) :status)))
        (should (eq 'completed (plist-get (superchat-runtime-status b) :status)))
        (should (eq 'cancelled (plist-get (superchat-runtime-status c) :status)))
        (should (eq 'running-model (plist-get (superchat-runtime-status d) :status)))
        (should-not (superchat-runtime-cancel a))
        (should (= 4 (length test-input-batch--requests)))
        (let ((authority-before test-input-batch--authority-count)
              (events-before (superchat-tape-replay-events session)))
          (funcall reply-effect (car test-input-batch--requests) "STOPPED C")
          (should-not test-input-batch--effects)
          ;; Native late delivery reaches the real Authority liveness refusal;
          ;; invocation is not an Effect and must not add terminal-first audit.
          (should (= (1+ authority-before) test-input-batch--authority-count))
          (should (equal events-before (superchat-tape-replay-events session)))
          (should (= 4 (length test-input-batch--requests))))
        ;; D is not merely untouched: its actual native tool remains executable.
        (setq workflow-owner-test--dispatch nil)
        (funcall reply-effect d-request "D effect")
        (should (equal '("D effect") test-input-batch--effects))
        (should (= 5 (length test-input-batch--requests)))
        (should (cl-find "tool.completed" (superchat-tape-replay-events session)
                         :key (lambda (event) (plist-get event :type)) :test #'equal))))))

(ert-deftest workflow-owner/normal-native-tools-draft-and-closure ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let* ((marker superchat--prompt-start)
             (draft (buffer-substring marker (point-max)))
             (offset (- (point) marker))
             (handle (superchat-workflow-execute-async "chain" "ARG"))
             (a (plist-get handle :run-id)))
        (test-input-batch--reply (car test-input-batch--requests)
                               '("input_batch_effect") "Workflow effect")
        (should (equal '("Workflow effect") test-input-batch--effects))
        (dotimes (_ 3)
          (test-input-batch--reply (car test-input-batch--requests) nil))
        (should (= 4 (length test-input-batch--requests)))
        (should (eq marker superchat--prompt-start))
        (should (equal-including-properties draft (buffer-substring marker (point-max))))
        (should (= offset (- (point) marker)))
        (should (string-match-p "Workflow complete (3/3 steps)" (buffer-string)))
        (let* ((events (superchat-tape-replay-events session))
               (runs (cl-remove-if-not
                      (lambda (e) (equal "run.completed" (plist-get e :type))) events)))
          (should (= 3 (length runs)))
          (dolist (event runs)
            (should (eq 'proved
                        (plist-get (superchat-closure-judge
                                    (list :kind 'durable-run-terminal
                                          :run-id (plist-get event :run-id)) events)
                                   :verdict))))
          (should-not (string-match-p "PRIVATE draft" (prin1-to-string events))))
        (dolist (request test-input-batch--requests)
          (should-not (string-match-p "PRIVATE draft"
                                      (prin1-to-string (llm-chat-prompt-interactions
                                                        (plist-get request :prompt))))))
        (should-not (superchat-runtime-cancel a))))))

(ert-deftest workflow-owner/stop-at-advance-boundaries ()
  (dolist (mode '(async streaming))
    (dolist (boundary '(before-closure after-closure record-message prepare-next))
      (workflow-owner-test--fixture mode
        (let* ((handle (superchat-workflow-execute-async "chain"))
               (run (plist-get handle :run-id))
               (resume (symbol-function 'superchat-workflow--resume-decision-for-step))
               (record (symbol-function 'superchat--record-message))
               (prepare (symbol-function 'superchat-workflow--resolve-prompt))
               stopped)
          (cl-labels ((stop () (setq stopped (superchat-runtime-cancel run "Boundary stop"))))
            (cl-letf (((symbol-function 'superchat-workflow--resume-decision-for-step)
                       (lambda (&rest args)
                         (when (eq boundary 'before-closure) (stop))
                         (prog1 (apply resume args)
                           (when (eq boundary 'after-closure) (stop)))))
                      ((symbol-function 'superchat--record-message)
                       (lambda (&rest args)
                         (prog1 (apply record args)
                           (when (eq boundary 'record-message) (stop)))))
                      ((symbol-function 'superchat-workflow--resolve-prompt)
                       (lambda (&rest args)
                         (prog1 (apply prepare args)
                           (when (eq boundary 'prepare-next) (stop))))))
              (test-input-batch--reply (car test-input-batch--requests) nil)))
          (should stopped)
          (should (= 1 (length test-input-batch--requests)))
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (should-not (string-match-p "Workflow complete" (buffer-string))))))))

(ert-deftest workflow-owner/committed-predelivery-stop ()
  (dolist (mode '(async streaming))
    (dolist (boundary '("run.completed" "turn.completed"))
      (workflow-owner-test--fixture mode
        (let* ((handle (superchat-workflow-execute-async "chain"))
               (run (plist-get handle :run-id))
               (append-event (symbol-function 'superchat-tape-append-event-record))
               (resume (symbol-function 'superchat-workflow--resume-decision-for-step))
               stopped resumed)
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event &rest args)
                       (prog1 (apply append-event event args)
                         (when (and (equal boundary (plist-get event :type))
                                    (not stopped))
                           (setq stopped (superchat-runtime-cancel run "Committed stop"))))))
                    ((symbol-function 'superchat-workflow--resume-decision-for-step)
                     (lambda (&rest args) (setq resumed t) (apply resume args))))
            (test-input-batch--reply (car test-input-batch--requests) nil))
          (should stopped)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (should (= 1 (length test-input-batch--requests)))
          ;; Streaming already displayed a partial before terminal append.
          ;; No Workflow result delivery/Closure handoff may enter afterward.
          (should-not resumed)
          (should (= 1 (cl-count "run.completed" (superchat-tape-replay-events session)
                                 :key (lambda (e) (plist-get e :type)) :test #'equal))))))))

(ert-deftest workflow-owner/view-identity-and-history-point ()
  (dolist (mode '(async streaming))
    (dolist (change '(rename session replacement history))
      (workflow-owner-test--fixture mode
        (let* ((handle (superchat-workflow-execute-async "chain"))
               (request (car test-input-batch--requests))
               (name (buffer-name view)) replacement before)
          (unwind-protect
              (progn
                (pcase change
                  ('rename (rename-buffer (generate-new-buffer-name " *workflow-renamed*")))
                  ('session (setq-local superchat--session-id "replacement-session"))
                  ('replacement
                   (kill-buffer view)
                   (setq replacement (get-buffer-create name))
                   (with-current-buffer replacement (insert "DO NOT TOUCH")))
                  ('history (goto-char (point-min))))
                (setq before (when (buffer-live-p view) (buffer-string)))
                (test-input-batch--reply request nil)
                (pcase change
                  ('session (should (equal before (buffer-string))))
                  ('replacement
                   (should (equal "DO NOT TOUCH" (with-current-buffer replacement (buffer-string)))))
                  ('history (should (= (point-min) (point))))
                  ('rename (should (= 2 (length test-input-batch--requests)))))
                (should (eq 'completed (plist-get (superchat-runtime-status
                                                 (plist-get handle :run-id)) :status))))
            (when (buffer-live-p replacement) (kill-buffer replacement))))))))

(ert-deftest workflow-owner/acceptance-stop-and-original-conditions ()
  (dolist (mode '(async streaming))
    (dolist (condition '(nil error quit))
      (workflow-owner-test--fixture mode
        (let ((track (symbol-function 'superchat--track-active-run))
              (submit (symbol-function 'superchat-runtime-submit))
              caught run stopped)
          (cl-letf (((symbol-function 'superchat--track-active-run)
                     (lambda (handle)
                       (prog1 (funcall track handle)
                         (setq run (plist-get handle :run-id)
                               stopped (superchat-runtime-cancel run "Acceptance stop"))
                         (when condition (signal condition '(owner-original))))))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (request)
                       (condition-case original
                           (funcall submit request)
                         ((error quit)
                          (setq caught original)
                          (signal (car original) (cdr original)))))))
            (condition-case original
                (superchat-workflow-execute-async "chain")
              (quit (should (equal '(quit owner-original) original)))))
          (should stopped)
          (should (equal (and condition (list condition 'owner-original)) caught))
          (should-not test-input-batch--requests)
          (should (plist-get (superchat-runtime-status run) :stop-requested))
          (should-not (string-match-p "Workflow complete" (buffer-string))))))))

(ert-deftest workflow-owner/startup-stop-before-acceptance ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let ((append-event (symbol-function 'superchat-tape-append-event-record))
            stopped run)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (equal "run.started" (plist-get event :type))
                         (setq run (plist-get event :run-id)
                               stopped (superchat-runtime-cancel run "Startup stop")))))))
          (superchat-workflow-execute-async "chain"))
        (should stopped)
        (should-not test-input-batch--requests)
        (should-not superchat--active-run-id)
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
        (should (eq 'proved (plist-get (superchat-closure-judge
                                       (list :kind 'durable-run-terminal :run-id run)
                                       (superchat-tape-replay-events session)) :verdict)))))))

(ert-deftest workflow-owner/header-reentry-does-not-edit-new-session ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let ((header (symbol-function 'superchat-workflow--render-step-header)) snapshot)
        (cl-letf (((symbol-function 'superchat-workflow--render-step-header)
                   (lambda (&rest args)
                     (prog1 (apply header args)
                       (setq-local superchat--session-id "changed-during-header")
                       (setq snapshot (buffer-string))))))
          (superchat-workflow-execute-async "chain"))
        ;; No Run was accepted yet; stale preparation must not submit into
        ;; the replacement View.  This is not Runtime cancellation.
        (should-not test-input-batch--requests)
        (should (equal-including-properties snapshot (buffer-string)))))))

(ert-deftest workflow-owner/stop-presentation-retains-partial-once ()
  (dolist (mode '(async streaming))
    (workflow-owner-test--fixture mode
      (let* ((marker superchat--prompt-start)
             (draft (buffer-substring marker (point-max)))
             (handle (superchat-workflow-execute-async "chain"))
             (run (plist-get handle :run-id)))
        (when (eq mode 'streaming)
          (let* ((request (car test-input-batch--requests))
                 (delta '(:content "USEFUL PARTIAL"))
                 (expected (decode-coding-string
                            (json-serialize `(:choices [(:delta ,delta)])) 'utf-8))
                 (media (cdr (plist-get request :media-type)))
                 source returned)
            ;; Cancellation follows delivery in this fixture.  Observe the
            ;; original native handler's RETURN for this exact connection and
            ;; packet; the shared helper's fixed sleep alone cannot prove it.
            (should-not (plist-get request :source))
            (setq source
                  (plz-event-source-open
                   (plz-event-source-buffer
                    :buffer (buffer-name (generate-new-buffer " *workflow-partial-sse*"))
                    :handlers
                    (mapcar
                     (lambda (pair)
                       (if (not (eq (car pair) 'message)) pair
                         (cons (car pair)
                               (lambda (event)
                                 (prog1 (funcall (cdr pair) event)
                                   (when (and (eq source (plist-get request :source))
                                              (equal expected (oref event data)))
                                     (setq returned source)))))))
                     (oref media events)))))
            (setf (plist-get request :source) source)
            (test-lmstudio--event request delta)
            (let ((bound (+ (float-time) 2)))
              (while (and (not returned) (< (float-time) bound))
                (sleep-for 0.01)))
            (should (eq source returned))
            (should (string-match-p "USEFUL PARTIAL" (buffer-string)))))
        (should (superchat-runtime-cancel run "Exact stop reason"))
        (should-not (superchat-runtime-cancel run))
        (should (string-match-p "Workflow stopped at step 1/3" (buffer-string)))
        (should (string-match-p "Exact stop reason" (buffer-string)))
        (when (eq mode 'streaming)
          (should (string-match-p "USEFUL PARTIAL" (buffer-string))))
        (should (equal-including-properties draft (buffer-substring marker (point-max))))
        (should (= 1 (length test-input-batch--requests)))
        (should (= 1 (how-many "Workflow stopped" (point-min) (point-max))))))))

(ert-deftest workflow-owner/stop-presentation-reentry-and-conditions ()
  (dolist (mode '(async streaming))
    (dolist (condition '(nil error quit session))
      (workflow-owner-test--fixture mode
        (let* ((handle (superchat-workflow-execute-async "chain"))
               (run (plist-get handle :run-id))
               (close (symbol-function 'superchat--response-region-close))
               (calls 0) snapshot)
          (cl-letf (((symbol-function 'superchat--response-region-close)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (should (plist-get (superchat-runtime-status run) :stop-requested))
                       (should (superchat-runtime-cancel run "Reentrant stop"))
                       (if (memq condition '(error quit))
                           (signal condition '(stop-display-original))
                         (prog1 (apply close args)
                           (when (eq condition 'session)
                             (setq-local superchat--session-id "reentered-session")
                             (setq snapshot (buffer-string))))))))
            (should (superchat-runtime-cancel run "Stop presentation")))
          (should (= 1 calls))
          (should-not (superchat-runtime-cancel run))
          (should (= 1 (length test-input-batch--requests)))
          (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
          (if (memq condition '(error quit))
              (should (equal (list condition 'stop-display-original)
                             (plist-get (superchat-runtime-status run) :owner-stop-error)))
            (should-not (plist-get (superchat-runtime-status run) :owner-stop-error)))
          (when snapshot
            (should (equal-including-properties snapshot (buffer-string)))))))))

(defun workflow-owner-test--acceptance-view-switch (mode boundary)
  "Replace the live Session at real BOUNDARY; reject only that accepted Run."
  (workflow-owner-test--fixture mode
    (let ((append-event (symbol-function 'superchat-tape-append-event-record))
          (track (symbol-function 'superchat--track-active-run))
          run snapshot tracked-at-replacement)
      (cl-labels ((replace-view ()
                    (with-current-buffer view
                      (setq-local superchat--session-id "acceptance-replacement")
                      (setq snapshot (buffer-string)
                            tracked-at-replacement superchat--active-run-id))))
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (and (eq boundary 'startup)
                                  (equal "run.started" (plist-get event :type)))
                         (setq run (plist-get event :run-id))
                         (replace-view)))))
                  ((symbol-function 'superchat--track-active-run)
                   (lambda (handle)
                     (prog1 (funcall track handle)
                       (when (eq boundary 'tracking)
                         (setq run (plist-get handle :run-id))
                         (replace-view))))))
          (superchat-workflow-execute-async "chain")))
      (should (stringp run))
      (should (equal-including-properties snapshot (buffer-string)))
      (should (equal tracked-at-replacement superchat--active-run-id))
      (should-not test-input-batch--requests)
      (should (plist-get (superchat-runtime-status run) :stop-requested))
      (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
      (let ((events (superchat-tape-replay-events session)))
        (should (= 1 (cl-count "run.started" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
        (should (= 1 (cl-count "run.cancelled" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
        (should (eq 'proved (plist-get (superchat-closure-judge
                                       (list :kind 'durable-run-terminal :run-id run) events)
                                      :verdict)))))))

(ert-deftest workflow-owner/acceptance-startup-session-async ()
  (workflow-owner-test--acceptance-view-switch 'async 'startup))
(ert-deftest workflow-owner/acceptance-startup-session-streaming ()
  (workflow-owner-test--acceptance-view-switch 'streaming 'startup))
(ert-deftest workflow-owner/acceptance-tracking-session-async ()
  (workflow-owner-test--acceptance-view-switch 'async 'tracking))
(ert-deftest workflow-owner/acceptance-tracking-session-streaming ()
  (workflow-owner-test--acceptance-view-switch 'streaming 'tracking))

(provide 'test-workflow)
;;; test-workflow.el ends here
