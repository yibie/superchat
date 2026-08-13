;;; verdict.el --- Conservative verdicts for the thin experience trace -*- lexical-binding: t; -*-

;; This file is deliberately outside the product.  It answers a narrower
;; question than the Runtime: what claim is justified by a five-method trace?
;; A trace can show retrieval/use without showing that the agent learned.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)

(defconst superchat-learning-minimal-verdict--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this verifier.")

(defun superchat-learning-minimal-verdict--method (trace name)
  "Return the method named NAME from TRACE, or nil."
  (cl-find name (plist-get trace :methods)
           :key (lambda (method) (plist-get method :method))
           :test #'eq))

(defun superchat-learning-minimal-verdict--view-status (method)
  "Return the assembled View status for METHOD."
  (let ((view (plist-get method :view)))
    (and view (plist-get view :status))))

(defun superchat-learning-minimal-verdict--target-action-p (method target)
  "Whether METHOD recorded TARGET as its observed action."
  (and method
       (equal (plist-get method :action-tool) target)
       (eq (plist-get method :action-matches) t)))

(defun superchat-learning-minimal-verdict--prompt-p (method)
  "Whether METHOD exposed non-empty Experience instruction text."
  (let ((view (and method (plist-get method :view))))
    (and view
         (stringp (plist-get view :prompt))
         (> (length (plist-get view :prompt)) 0))))

(defun superchat-learning-minimal-verdict--tool-completed-p (method target)
  "Whether METHOD contains a successful TARGET tool completion."
  (and method
       (cl-some
        (lambda (event)
          (and (equal "tool.completed" (plist-get event :type))
               (equal target
                      (plist-get (plist-get event :payload) :tool-name))))
        (plist-get method :tape-events))))

(defun superchat-learning-minimal-verdict--check (name passed reason)
  "Build one named check record."
  (list :name name :passed passed :reason reason))

(defun superchat-learning-minimal-verdict--held-out-pairs (trace)
  "Resolve TRACE's optional held-out pair names to method plists."
  (delq nil
        (mapcar
         (lambda (names)
           (let ((control (superchat-learning-minimal-verdict--method
                           trace (nth 0 names)))
                 (treatment (superchat-learning-minimal-verdict--method
                             trace (nth 1 names))))
             (and control treatment (list control treatment))))
         (plist-get trace :held-out-pairs))))

(defun superchat-learning-minimal-verdict--held-out-pair-p (pair target)
  "Whether PAIR shows a completed treatment/control difference."
  (let ((control (nth 0 pair))
        (treatment (nth 1 pair)))
    (and (eq (plist-get control :status) 'completed)
         (eq (plist-get treatment :status) 'completed)
         (not (superchat-learning-minimal-verdict--target-action-p
               control target))
         (eq (superchat-learning-minimal-verdict--view-status treatment)
             'satisfied)
         (superchat-learning-minimal-verdict--prompt-p treatment)
         (superchat-learning-minimal-verdict--target-action-p
          treatment target))))

(defun superchat-learning-minimal-verdict (trace)
  "Return the strongest defensible claims supported by TRACE.

The result intentionally distinguishes three levels:

`retrieval/use' means a View was present and the paired treatment chose the
reviewed action while control did not.  `durable-retrieval' additionally means
the View survived reopen and disappeared after revocation.  `learning' is only
considered when an unseen task has its own no-View control, repeated paired
outcomes, and completed task outcomes.  Missing evidence returns
`not-identifiable'; it never gets upgraded from a lucky action sequence."
  (let* ((target "minimal_refresh")
         (control (superchat-learning-minimal-verdict--method
                   trace 'matched-control))
         (treatment (superchat-learning-minimal-verdict--method
                     trace 'matched-treatment))
         (ablation (superchat-learning-minimal-verdict--method
                    trace 'view-content-ablation))
         (transfer (superchat-learning-minimal-verdict--method
                    trace 'wording-transfer))
         (negative (superchat-learning-minimal-verdict--method
                   trace 'negative-transfer))
         (persist (superchat-learning-minimal-verdict--method
                   trace 'persistence-after-reopen))
         (revoked (superchat-learning-minimal-verdict--method
                   trace 'persistence-after-revocation))
         (held-out-pairs
          (superchat-learning-minimal-verdict--held-out-pairs trace))
         (checks
          (list
           (superchat-learning-minimal-verdict--check
            'matched-treatment-diff
            (and control treatment
                 (not (superchat-learning-minimal-verdict--target-action-p
                       control target))
                 (superchat-learning-minimal-verdict--target-action-p
                  treatment target)
                 (eq (superchat-learning-minimal-verdict--view-status treatment)
                     'satisfied)
                 (superchat-learning-minimal-verdict--prompt-p treatment))
            "Treatment must expose a satisfied View and choose the reviewed action while control does not.")
           (superchat-learning-minimal-verdict--check
            'content-ablation
            (and ablation
                 (not (superchat-learning-minimal-verdict--target-action-p
                       ablation target))
                 (not (superchat-learning-minimal-verdict--prompt-p ablation)))
            "The receipt-only ablation must not expose Candidate instruction text or choose the target action.")
           (superchat-learning-minimal-verdict--check
            'wording-transfer
            (and transfer
                 (eq (superchat-learning-minimal-verdict--view-status transfer)
                     'satisfied)
                 (superchat-learning-minimal-verdict--prompt-p transfer)
                 (superchat-learning-minimal-verdict--target-action-p
                  transfer target))
            "A new wording must receive the View and choose the reviewed action.")
           (superchat-learning-minimal-verdict--check
            'persistence
            (and persist
                 (eq (superchat-learning-minimal-verdict--view-status persist)
                     'satisfied)
                 (superchat-learning-minimal-verdict--target-action-p
                  persist target))
            "The reopened store must select the same View and action.")
           (superchat-learning-minimal-verdict--check
            'revocation
            (and revoked
                 (eq (superchat-learning-minimal-verdict--view-status revoked)
                     'empty)
                 (not (superchat-learning-minimal-verdict--prompt-p revoked))
                 (not (superchat-learning-minimal-verdict--target-action-p
                       revoked target)))
            "After revocation the rebuilt View must be empty and the target action absent.")
           (superchat-learning-minimal-verdict--check
            'negative-disclosure
            (and negative
                 (memq (superchat-learning-minimal-verdict--view-status negative)
                       '(empty blocked))
                 (not (superchat-learning-minimal-verdict--prompt-p negative))
                 (not (superchat-learning-minimal-verdict--target-action-p
                       negative target)))
            "A conflicting View must fail closed without disclosing or executing the Candidate action.")))
         (retrieval-use (cl-every (lambda (check)
                                   (plist-get check :passed))
                                 (seq-filter
                                  (lambda (check)
                                    (memq (plist-get check :name)
                                          '(matched-treatment-diff
                                            content-ablation
                                            wording-transfer)))
                                  checks)))
         (durable-retrieval (and retrieval-use
                                 (plist-get (nth 3 checks) :passed)
                                 (plist-get (nth 4 checks) :passed)
                                 (plist-get (nth 5 checks) :passed)))
         (completed-treatment
          (and treatment (eq (plist-get treatment :status) 'completed)))
         (completed-transfer
          (and transfer (eq (plist-get transfer :status) 'completed)))
         (learning-evidence
          (list
           (superchat-learning-minimal-verdict--check
            'held-out-control (and held-out-pairs
                                (superchat-learning-minimal-verdict--held-out-pair-p
                                 (car held-out-pairs) target))
            "The unseen wording needs its own completed no-View control; treatment-only transfer is not causal evidence.")
           (superchat-learning-minimal-verdict--check
            'repeated-paired-outcomes
            (and (>= (length held-out-pairs) 2)
                 (cl-every (lambda (pair)
                             (superchat-learning-minimal-verdict--held-out-pair-p
                              pair target))
                           held-out-pairs))
            "At least two completed held-out pairs are required; one lucky action is not a stable improvement estimate.")
           (superchat-learning-minimal-verdict--check
            'completed-outcomes
            (and completed-treatment completed-transfer
                 (cl-every (lambda (pair)
                             (and (eq (plist-get (nth 0 pair) :status)
                                      'completed)
                                  (eq (plist-get (nth 1 pair) :status)
                                      'completed)))
                           held-out-pairs))
            "A correct intermediate action followed by an intentional Runtime failure is not a task outcome.")))
         (learning (and durable-retrieval
                    (cl-every (lambda (check) (plist-get check :passed))
                              learning-evidence))))
    (list :format "superchat-experience-learning-verdict-v1"
          :status (if learning 'learned 'not-identifiable)
          :retrieval-use (if retrieval-use 'observed 'not-identifiable)
          :durable-retrieval (if durable-retrieval 'observed 'not-identifiable)
          :learning (if learning 'supported 'not-identifiable)
          :checks checks
          :learning-evidence learning-evidence
          :default-remains-off (not learning))))

(defun superchat-learning-minimal-self-verdict (trace)
  "Judge the stronger model-induced Candidate loop in TRACE.

This gate is separate from the older retrieval/use verifier.  It requires a
real model reflection Run, no answer in the source task, and two repeated
held-out first-action effects.  Final Run status and request counts are
reported as auxiliary diagnostics, not used as the primary learning metric.
A passing result still records that human Promotion was the admission gate;
it does not authorize a product default."
  (let* ((target (or (plist-get trace :target-action) "minimal_refresh"))
         (tool-surface (or (plist-get trace :tool-surface)
                           '("read_probe" "minimal_refresh"
                             "minimal_uncertain")))
         (wrong (or (plist-get trace :wrong-action) "minimal_uncertain"))
         (methods (plist-get trace :methods))
         (induction (plist-get trace :induction))
         (source (superchat-learning-minimal-verdict--method
                  trace 'induction-source))
         (seed (plist-get trace :seed))
         (notes (plist-get trace :notes))
         (pairs (superchat-learning-minimal-verdict--held-out-pairs trace))
         (persist (superchat-learning-minimal-verdict--method
                   trace 'persistence-after-reopen))
         (revoked (superchat-learning-minimal-verdict--method
                   trace 'persistence-after-revocation))
         (source-task (plist-get source :task))
         (source-success
          (and source
               (cl-some
                (lambda (event)
                  (and (equal "tool.completed" (plist-get event :type))
                       (equal target
                              (plist-get (plist-get event :payload)
                                         :tool-name))))
                (plist-get source :tape-events))))
         (model-induced
          (and source induction
               (eq (plist-get source :status) 'completed)
               source-success
               (eq (plist-get induction :status) 'completed)
               (stringp (plist-get induction :raw-response))
               (equal (plist-get induction :action) target)
               (equal (plist-get (plist-get seed :induced) :action) target)
               (stringp (plist-get seed :statement))
               (string-match-p (regexp-quote target)
                               (plist-get seed :statement))
               ;; The source task may describe the failure, but must not name
               ;; either recovery answer.  This rejects supervised replay.
               (not (string-match-p
                     (regexp-quote target) (or source-task "")))
               (not (string-match-p
                     (regexp-quote wrong) (or source-task "")))))
         (held-out-effect
          (and (= (length pairs) 2)
               (cl-every
                (lambda (pair)
                  (let ((control (nth 0 pair))
                        (treatment (nth 1 pair)))
                    (and (not (superchat-learning-minimal-verdict--target-action-p
                               control target))
                         (superchat-learning-minimal-verdict--tool-completed-p
                          treatment target)
                         (eq (superchat-learning-minimal-verdict--view-status
                              treatment)
                             'satisfied)
                         (superchat-learning-minimal-verdict--prompt-p treatment)
                         (superchat-learning-minimal-verdict--target-action-p
                          treatment target))))
                pairs)))
         (cost
          (and (cl-every
                (lambda (pair)
                  (= (length (plist-get (nth 0 pair) :transport-requests))
                     (length (plist-get (nth 1 pair) :transport-requests))))
                pairs)
               (integerp (plist-get trace :provider-requests))
               (> (plist-get trace :provider-requests) 0)))
         (safe-actions
          (cl-every
           (lambda (method)
             (cl-every
              (lambda (event)
                (let ((tool (plist-get (plist-get event :payload)
                                       :tool-name)))
                  (or (not (equal "tool.requested" (plist-get event :type)))
                      (member tool tool-surface))))
              (plist-get method :tape-events)))
           methods))
         (durable
          (and persist
               (eq (plist-get persist :status) 'completed)
               (eq (plist-get persist :view-origin) 'after-db-reopen)
               (eq (superchat-learning-minimal-verdict--view-status persist)
                   'satisfied)
               (superchat-learning-minimal-verdict--target-action-p
                persist target)
               revoked
               (eq (plist-get revoked :view-origin)
                   'after-revocation-reopen)
               (eq (superchat-learning-minimal-verdict--view-status revoked)
                   'empty)
               (not (superchat-learning-minimal-verdict--target-action-p
                     revoked target))))
         (checks
          (list
           (superchat-learning-minimal-verdict--check
           'model-induced-candidate model-induced
            "The source Run must close with a successful corrective action before reflection generates a Candidate.")
           (superchat-learning-minimal-verdict--check
            'repeated-held-out-effect held-out-effect
            "Both held-out pairs must choose the induced action as the first recovery action when the View is present, while control chooses another first action.")
           (superchat-learning-minimal-verdict--check
            'cost-parity cost
            "Each held-out pair must use the same finite provider-request budget.")
           (superchat-learning-minimal-verdict--check
            'safe-action-surface safe-actions
            "The loop may execute only the pre-registered local recovery tools.")
           (superchat-learning-minimal-verdict--check
            'persistence-and-revocation durable
            "The View must be freshly assembled after reopen, and a freshly assembled post-revocation View must be empty.")))
         (primary-checks
          (seq-remove
           (lambda (check) (eq (plist-get check :name) 'cost-parity))
           checks))
         (auxiliary-checks
          (seq-filter
           (lambda (check) (eq (plist-get check :name) 'cost-parity))
           checks))
         (supported
          (cl-every (lambda (check) (plist-get check :passed)) primary-checks)))
    (list :format "superchat-experience-self-learning-verdict-v1"
          :primary-metric 'first-recovery-action
          :status (if supported 'supported-with-human-gate 'not-identifiable)
          :model-induced-candidate (if model-induced 'observed 'not-identifiable)
          :held-out-task-effect (if held-out-effect 'observed 'not-identifiable)
          :learning (if supported 'supported 'not-identifiable)
          :human-promotion-gate (and (plist-get notes :human-gate) t)
          :primary-checks primary-checks
          :auxiliary-checks auxiliary-checks
          :final-statuses
          (mapcar (lambda (method)
                    (list (plist-get method :method)
                          (plist-get method :status)))
                  methods)
          :request-counts
          (mapcar (lambda (method)
                    (list (plist-get method :method)
                          (length (plist-get method :transport-requests))))
                  methods)
          :checks checks
          ;; Even a supported experiment never changes the product default.
          :default-remains-off t)))

(defun superchat-learning-minimal-verdict--read-trace (path)
  "Read one printed trace plist from PATH without executing it."
  ;; Trace files print provider structs.  The verifier only needs their
  ;; surrounding plists, so small shape-compatible readers are sufficient.
  (dolist (spec '((llm-ollama a b c d e f g h i)
                  (llm-chat-prompt-tool-result a b c d e f g h i)
                  (llm-provider-utils-tool-use a b c d e f g h i)))
    (eval (cons 'cl-defstruct spec)))
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

(ert-deftest superchat-learning-minimal-verdict-requires-held-out-control ()
  "The current trace must not be called learning evidence."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "five-run-20260812-revocation/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-verdict trace)))
    (should (eq (plist-get verdict :status) 'not-identifiable))
    (should (eq (plist-get verdict :retrieval-use) 'observed))
    (should (eq (plist-get verdict :durable-retrieval) 'observed))
    (should (eq (plist-get verdict :default-remains-off) t))))

(ert-deftest superchat-learning-minimal-verdict-accepts-two-completed-held-out-pairs ()
  "The gate can pass when the required evidence is actually present."
  (let* ((method
          (lambda (name status view-status prompt action matched)
            (list :method name :status status
                  :view (and view-status
                              (list :status view-status :prompt prompt))
                  :action-tool action :action-matches matched)))
         (methods
          (list
           (funcall method 'matched-control 'completed nil nil
                    "minimal_uncertain" nil)
           (funcall method 'matched-treatment 'completed 'satisfied
                    "reviewed rule" "minimal_refresh" t)
           (funcall method 'view-content-ablation 'completed 'satisfied
                    nil "minimal_uncertain" nil)
           (funcall method 'wording-transfer 'completed 'satisfied
                    "reviewed rule" "minimal_refresh" t)
           (funcall method 'negative-transfer 'completed 'blocked
                    nil "minimal_uncertain" nil)
           (funcall method 'persistence-after-reopen 'completed 'satisfied
                    "reviewed rule" "minimal_refresh" t)
           (funcall method 'persistence-after-revocation 'completed 'empty
                    nil "minimal_uncertain" nil)
           (funcall method 'holdout-control-1 'completed nil nil
                    "minimal_uncertain" nil)
           (funcall method 'holdout-treatment-1 'completed 'satisfied
                    "reviewed rule" "minimal_refresh" t)
           (funcall method 'holdout-control-2 'completed nil nil
                    "minimal_uncertain" nil)
           (funcall method 'holdout-treatment-2 'completed 'satisfied
                    "reviewed rule" "minimal_refresh" t)))
         (trace (list :methods methods
                      :held-out-pairs
                      '((holdout-control-1 holdout-treatment-1)
                        (holdout-control-2 holdout-treatment-2))))
         (verdict (superchat-learning-minimal-verdict trace)))
    (should (eq (plist-get verdict :status) 'learned))
    (should (eq (plist-get verdict :default-remains-off) nil))))

(ert-deftest superchat-learning-minimal-self-verdict-rejects-answer-leakage ()
  "A supervised source task is not self-learning evidence."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "self-learning-20260812-v5/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-self-verdict trace)))
    (should (eq (plist-get verdict :status) 'not-identifiable))
    (should (eq (plist-get verdict :default-remains-off) t))))

(ert-deftest superchat-learning-minimal-self-verdict-rejects-unverified-persistence ()
  "A historical trace without fresh View provenance is not durable evidence."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "self-learning-20260812-final-safe/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-self-verdict trace)))
    (should (eq (plist-get verdict :status) 'not-identifiable))
    (should (eq (plist-get verdict :held-out-task-effect) 'observed))
    (should (eq (plist-get verdict :default-remains-off) t))))

(ert-deftest superchat-learning-minimal-self-verdict-rejects-nonclosing-transfer ()
  "A guessed rule after an unsuccessful source Run is not learning evidence."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "self-learning-generalization-20260812/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-self-verdict trace)))
    (should (eq (plist-get verdict :status) 'not-identifiable))
    (should (eq (plist-get verdict :default-remains-off) t))))

(ert-deftest superchat-learning-minimal-self-verdict-uses-first-action-metric ()
  "The historical first-action trace is provisional without provenance."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "../experience-learning-first-action-20260812-final3/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-self-verdict trace)))
    (should (eq (plist-get verdict :status) 'not-identifiable))
    (should (eq (plist-get verdict :primary-metric) 'first-recovery-action))
    (should (eq (plist-get verdict :held-out-task-effect) 'observed))
    (should (eq (plist-get verdict :default-remains-off) t))
    (should-not (plist-get (car (plist-get verdict :auxiliary-checks)) :passed))))

(ert-deftest superchat-learning-minimal-self-verdict-accepts-final4 ()
  "The repaired cohort must carry fresh reopen provenance."
  (let* ((trace (superchat-learning-minimal-verdict--read-trace
                 (expand-file-name
                  "../experience-learning-first-action-20260812-final4/trace.el"
                  superchat-learning-minimal-verdict--directory)))
         (verdict (superchat-learning-minimal-self-verdict trace)))
    (should (eq (plist-get verdict :status) 'supported-with-human-gate))
    (should (eq (plist-get verdict :learning) 'supported))
    (should (eq (plist-get verdict :held-out-task-effect) 'observed))
    (should (eq (plist-get verdict :default-remains-off) t))))


(defun superchat-recovery-self-verdict (trace)
  "Judge the recovery-domain self-learning trace.

Applies the same gate discipline as the opaque-domain self-verdict: a real
source Run must close with a typed failure and a corrective success before
reflection, the source task must not name the recovery answer, the induced
Candidate must pass the user Promotion gate, and two held-out control/
treatment pairs must show a first-action difference.  A passing result never
changes the product default."
  (let* ((target (or (plist-get trace :target-action) "workspace_info"))
         (source (plist-get trace :source-run))
         (induced (plist-get trace :induced))
         (arms (plist-get trace :arms))
         (source-task (plist-get source :task))
         (treatments
          (cl-remove-if-not
           (lambda (arm) (string-suffix-p "treatment" (plist-get arm :arm)))
           arms))
         (controls
          (cl-remove-if-not
           (lambda (arm) (string-suffix-p "control" (plist-get arm :arm)))
           arms))
         (model-induced
          (and source induced
               (eq (plist-get source :status) 'completed)
               (eq (plist-get source :source-valid) t)
               (eq (plist-get induced :status) 'completed)
               (stringp (plist-get induced :raw-response))
               (equal (plist-get induced :action) target)
               (stringp (plist-get induced :statement))
               (string-match-p (regexp-quote target)
                               (plist-get induced :statement))
               (not (string-match-p
                     (regexp-quote "workspace_info") (or source-task "")))))
         (held-out-effect
          (and (= (length treatments) 2)
               (= (length controls) 2)
               (cl-every
                (lambda (arm)
                  (and (plist-get arm :intervention-recorded)
                       (equal "satisfied"
                              (plist-get arm :experience-view-status-attempt-2))
                       (equal target
                              (plist-get arm :first-recovery-action))))
                treatments)
               (cl-every
                (lambda (arm)
                  (and (not (plist-get arm :intervention-recorded))
                       (not (equal target
                                   (plist-get arm :first-recovery-action)))))
                controls)))
         (persist (plist-get trace :persistence))
         (revoked (plist-get trace :revocation))
         (durable
          (and persist revoked
               (plist-get persist :intervention-recorded)
               (equal "satisfied"
                      (plist-get persist :experience-view-status-attempt-2))
               (equal target (plist-get persist :first-recovery-action))
               (not (equal "satisfied"
                           (plist-get revoked
                                      :experience-view-status-attempt-2)))
               (not (equal target (plist-get revoked :first-recovery-action)))))
         (checks
          (list
           (superchat-learning-minimal-verdict--check
            'model-induced-candidate model-induced
            "The source Run must close with a typed failure and a corrective success before reflection generates a Candidate.")
           (superchat-learning-minimal-verdict--check
            'held-out-first-action-effect held-out-effect
            "Both held-out pairs must choose the induced action first when the View is present, while controls choose another first action.")
           (superchat-learning-minimal-verdict--check
            'persistence-and-revocation durable
            "After reopen the induced View must still satisfy and steer the first action; after revocation it must not.")))
         (supported
          (cl-every (lambda (check) (plist-get check :passed)) checks)))
    (list :format "superchat-experience-recovery-self-learning-verdict-v1"
          :status (if supported 'supported-with-human-gate 'not-identifiable)
          :model-induced-candidate (if model-induced 'observed 'not-identifiable)
          :held-out-task-effect (if held-out-effect 'observed 'not-identifiable)
          :learning (if supported 'supported 'not-identifiable)
          :checks checks
          :default-remains-off t)))

(when-let ((trace-path (getenv "SUPERCHAT_EXPERIENCE_VERDICT_TRACE")))
  (let* ((trace (superchat-learning-minimal-verdict--read-trace trace-path))
         (verdict (superchat-learning-minimal-verdict trace)))
    (princ (format "%S\n" verdict))))

(provide 'superchat-experience-learning-verdict)

;;; verdict.el ends here
