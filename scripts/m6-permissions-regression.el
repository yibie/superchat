;;; m6-permissions-regression.el --- permission levels and the approval seam -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/implemented/2026-09-17-permissions-the-dsh-way.md.
;; Slice 1: the approval seam (`ask'/`never', the `approval/request' waterfall,
;; the paired Tape audit and Runtime's open-approval mark) and the permission
;; levels (three Session events written through only when a knob changes,
;; durable across reopen).  The canonical smoke fixture supplies the Session,
;; the Tape and a live scripted Run; no real model and no protected regression
;; set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/m6-permissions-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^m6-permissions/")'

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

(defmacro m6p--fixture (script &rest body)
  "Run BODY in a scripted smoke fixture with permission levels enabled."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-function nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'edit-buffers))
       (smoke--with-fixture ,script ,@body))))

(defun m6p--types (session)
  "Configuration and approval event types of SESSION, oldest first."
  (cl-remove-if-not
   (lambda (type)
     (member type '("permission.level" "sandbox.mode" "approval.policy"
                    "approval.requested" "approval.decided")))
   (smoke--types session)))

(defun m6p--events (session type)
  "Replayed events of TYPE in SESSION, oldest first."
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events session)))

(defun m6p--payload (event key)
  "Value of KEY in EVENT's payload, whatever container replay produced."
  (let ((payload (plist-get event :payload)))
    (cond ((hash-table-p payload)
           (or (gethash (substring (symbol-name key) 1) payload)
               (gethash key payload)))
          ((and (listp payload) (keywordp (car-safe payload)))
           (plist-get payload key))
          (t (alist-get (intern (substring (symbol-name key) 1)) payload)))))

(defmacro m6p--with-live-run (spec &rest body)
  "Bind (SESSION REQUEST) in SPEC to a hanging live Run's identity; run BODY.
REQUEST is a complete approval request for a tool named m6p_tool.  The Run is
cancelled afterwards."
  (declare (indent 1) (debug t))
  (let ((session (car spec)) (request (cadr spec)) (run (make-symbol "run")))
    `(let* ((view (smoke--open-chat))
            (,session (buffer-local-value 'superchat--session-id view))
            (,run (plist-get
                   (superchat-service-call
                    nil 'driver 'submit
                    (list :surface 'external :session-id ,session :tools 'none
                          :content "hang"
                          :context (list :host-buffer view)
                          :runtime-extras '(:record-conversation nil)
                          :sink (superchat-sink-discard)))
                   :run-id))
            (,request nil))
       (should (smoke--wait (lambda () (superchat-runtime-effect-capable-p ,run)) 10))
       (setq ,request
             (list :session-id ,session :run-id ,run
                   :turn-id (plist-get (superchat-runtime-status ,run) :turn-id)
                   :execution-id (format "m6p-exec-%06x" (random #x1000000))
                   :tool-name "m6p_tool" :args '("a") :reason "m6p reason"
                   :call-id "call-1"))
       (unwind-protect (progn ,@body)
         (ignore-errors (superchat-runtime-cancel ,run "[m6p done]"))
         (smoke--wait (lambda () (smoke--run-done-p ,run)) 10)))))

;;;; Approval seam -----------------------------------------------------------

(ert-deftest m6-permissions/approval/answerer-of-last-resort-decides ()
  "Unclaimed requests reach the answerer; its answer is the only grant."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((seen nil)
             (superchat-approval-function
              (lambda (public) (push public seen) t)))
        (should (eq 'allowed-once (superchat-approval-request request)))
        (should (equal "m6p_tool" (plist-get (car seen) :tool-name)))
        (should (equal '("a") (plist-get (car seen) :args)))
        (should (equal "m6p reason" (plist-get (car seen) :reason)))
        ;; Identity the answerer has no business with stays private.
        (should-not (plist-member (car seen) :execution-id)))
      (let ((superchat-approval-function (lambda (_public) nil)))
        (should (eq 'rejected
                    (superchat-approval-request
                     (plist-put (copy-sequence request) :execution-id "m6p-second"))))))))

(ert-deftest m6-permissions/approval/waterfall-claims-or-delegates ()
  "A listener returning an outcome claims; one calling NEXT delegates."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((fallback-calls 0)
             (superchat-approval-function
              (lambda (_public) (cl-incf fallback-calls) t))
             (off (superchat-on nil 'approval/request 'm6p-claim
                                (lambda (_request _next) 'rejected))))
        (unwind-protect
            (progn
              (should (eq 'rejected (superchat-approval-request request)))
              (should (= 0 fallback-calls)))
          (funcall off))
        (setq off (superchat-on nil 'approval/request 'm6p-delegate
                                (lambda (public next) (funcall next public))))
        (unwind-protect
            (progn
              (should (eq 'allowed-once
                          (superchat-approval-request
                           (plist-put (copy-sequence request) :execution-id "m6p-b"))))
              (should (= 1 fallback-calls)))
          (funcall off))))))

(ert-deftest m6-permissions/approval/a-faulting-or-rogue-answerer-opens-nothing ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function (lambda (_public) t)))
        (dolist (listener (list (lambda (_request _next) (error "boom"))
                                (lambda (_request _next) 'allow-forever)))
          (let ((off (superchat-on nil 'approval/request 'm6p-rogue listener)))
            (unwind-protect
                (should (eq 'unavailable
                            (superchat-approval-request
                             (plist-put (copy-sequence request) :execution-id
                                        (format "m6p-%06x" (random #x1000000))))))
              (funcall off))))))))

(ert-deftest m6-permissions/approval/never-rejects-before-any-answerer ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((asked 0)
             (superchat-approval-function (lambda (_public) (cl-incf asked) t))
             (off (superchat-on nil 'approval/request 'm6p-allow
                                (lambda (_request _next) (cl-incf asked) 'allowed-once))))
        (unwind-protect
            (progn
              (should (superchat-approval-set-policy session 'never))
              (should (eq 'rejected (superchat-approval-request request)))
              (should (= 0 asked)))
          (funcall off))))))

(ert-deftest m6-permissions/approval/no-answerer-is-unavailable ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function nil))
        (should (eq 'unavailable (superchat-approval-request request)))))))

(ert-deftest m6-permissions/approval/audit-is-paired-and-run-scoped ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function (lambda (_public) t)))
        (superchat-approval-request request))
      (let ((asked (m6p--events session "approval.requested"))
            (decided (m6p--events session "approval.decided")))
        (should (= 1 (length asked)))
        (should (= 1 (length decided)))
        (should (equal (plist-get request :execution-id)
                       (m6p--payload (car asked) :execution-id)))
        (should (equal (plist-get request :execution-id)
                       (m6p--payload (car decided) :execution-id)))
        (should (equal "allow" (m6p--payload (car decided) :decision)))
        (should (equal "m6p reason" (m6p--payload (car asked) :reason)))
        (should (equal (plist-get request :run-id) (plist-get (car asked) :run-id)))))
    ;; Outside a live Run nothing is appended and nobody is asked.
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (asked 0)
           (superchat-approval-function (lambda (_public) (cl-incf asked) t)))
      (should-error
       (superchat-approval-request
        (list :session-id session :run-id "run-that-never-was" :turn-id "turn-x"
              :execution-id "m6p-dead" :tool-name "m6p_tool"))
       :type 'superchat-approval-outside-run)
      (should (= 0 asked)))))

(ert-deftest m6-permissions/approval/open-mark-spans-the-decision ()
  "The open-approval mark is set while the answerer runs and cleared after."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((run (plist-get request :run-id))
             (during nil)
             (superchat-approval-function
              (lambda (_public)
                (setq during (superchat-runtime--open-tool-work-p
                              (superchat-runtime--entry run)))
                t)))
        (superchat-approval-request request)
        (should during)
        (should-not (superchat-runtime--open-tool-work-p
                     (superchat-runtime--entry run)))))))

(ert-deftest m6-permissions/approval/quit-records-a-denial-and-resignals ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function (lambda (_public) (signal 'quit nil))))
        (should (eq 'quit
                    (condition-case nil
                        (progn (superchat-approval-request request) 'returned)
                      (quit 'quit)))))
      (let ((decided (m6p--events session "approval.decided")))
        (should (= 1 (length decided)))
        (should (equal "deny" (m6p--payload (car decided) :decision)))
        (should (equal "user-quit" (m6p--payload (car decided) :interruption))))
      (should-not (superchat-runtime--open-tool-work-p
                   (superchat-runtime--entry (plist-get request :run-id)))))))

(ert-deftest m6-permissions/approval/answerers-cannot-reach-the-approved-arguments ()
  "An answerer that mutates what it was shown changes neither the caller's
arguments nor the snapshot the approval is for, at any depth."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((table (make-hash-table :test #'equal))
             (_ (puthash "k" (copy-sequence "deep") table))
             (original (list (copy-sequence "safe")
                             (vector (copy-sequence "vec"))
                             (list :nested (copy-sequence "plist"))
                             table))
             (superchat-approval-function
              (lambda (public)
                (let ((args (plist-get public :args)))
                  (aset (nth 0 args) 0 ?X)
                  (aset (aref (nth 1 args) 0) 0 ?X)
                  (aset (plist-get (nth 2 args) :nested) 0 ?X)
                  (aset (gethash "k" (nth 3 args)) 0 ?X)
                  (setcar args "replaced"))
                t))
             (result (superchat-approval-request-snapshot
                      (plist-put (copy-sequence request) :args original)))
             (approved (plist-get result :args)))
        (should (eq 'allowed-once (plist-get result :outcome)))
        (dolist (args (list original approved))
          (should (equal "safe" (nth 0 args)))
          (should (equal "vec" (aref (nth 1 args) 0)))
          (should (equal "plist" (plist-get (nth 2 args) :nested)))
          (should (equal "deep" (gethash "k" (nth 3 args)))))
        (should-not (eq original approved))
        (should-not (eq (nth 0 original) (nth 0 approved)))))))

(ert-deftest m6-permissions/approval/approval-is-for-the-entry-snapshot ()
  "Arguments replaced while the question is open are not what was approved."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((original (list (copy-sequence "safe")))
             (superchat-approval-function
              (lambda (_public)
                ;; Reentrant code holding the caller's own list swaps it.
                (aset (car original) 0 ?X)
                (setcar original "rm -rf")
                t))
             (result (superchat-approval-request-snapshot
                      (plist-put (copy-sequence request) :args original))))
        (should (equal '("rm -rf") original))
        (should (equal '("safe") (plist-get result :args)))))))

(ert-deftest m6-permissions/approval/a-failing-last-resort-answerer-is-closed-and-settled ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function (lambda (_public) (error "answerer boom"))))
        (should (eq 'unavailable (superchat-approval-request request))))
      (let ((decided (m6p--events session "approval.decided")))
        (should (= 1 (length (m6p--events session "approval.requested"))))
        (should (= 1 (length decided)))
        (should (equal "deny" (m6p--payload (car decided) :decision)))
        (should (equal "unavailable" (m6p--payload (car decided) :outcome))))
      (should-not (superchat-runtime--open-tool-work-p
                   (superchat-runtime--entry (plist-get request :run-id)))))))

(ert-deftest m6-permissions/approval/a-listener-quit-is-a-cancellation-not-a-fault ()
  "A quit inside a waterfall answerer is recorded as the user's quit and
propagates; other emitters of a waterfall still read a quit as a refusal."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((off (superchat-on nil 'approval/request 'm6p-quit
                               (lambda (_request _next) (signal 'quit nil)))))
        (unwind-protect
            (progn
              (should (eq 'quit
                          (condition-case nil
                              (progn (superchat-approval-request request) 'returned)
                            (quit 'quit))))
              ;; The same listener under an ordinary emission refuses.
              (should (superchat-waterfall-refused-p
                       (superchat-emit nil 'approval/request (list :tool-name "x")))))
          (funcall off)))
      (let ((decided (m6p--events session "approval.decided")))
        (should (= 1 (length decided)))
        (should (equal "user-quit" (m6p--payload (car decided) :interruption)))
        (should (equal "cancelled" (m6p--payload (car decided) :outcome))))
      (should-not (superchat-runtime--open-tool-work-p
                   (superchat-runtime--entry (plist-get request :run-id)))))))

(ert-deftest m6-permissions/approval/every-asked-path-renews-the-transport-wait ()
  "A claiming listener, the last-resort answerer and a failing answerer all
renew the wait; `never' asks nobody and renews nothing."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((renewed 0)
            (n 0))
        (cl-letf (((symbol-function 'superchat-approval--extend-wait)
                   (lambda (_run) (cl-incf renewed))))
          (cl-flet ((ask () (superchat-approval-request
                             (plist-put (copy-sequence request) :execution-id
                                        (format "m6p-wait-%d" (cl-incf n))))))
            (let ((off (superchat-on nil 'approval/request 'm6p-claim
                                     (lambda (_request _next) 'allowed-once))))
              (unwind-protect (ask) (funcall off)))
            (should (= 1 renewed))
            (let ((superchat-approval-function (lambda (_public) t))) (ask))
            (should (= 2 renewed))
            (let ((superchat-approval-function (lambda (_public) (error "boom")))) (ask))
            (should (= 3 renewed))
            (superchat-approval-set-policy session 'never)
            (ask)
            (should (= 3 renewed))))))))

(ert-deftest m6-permissions/approval/an-unrecorded-decision-leaves-the-approval-open ()
  "When the decision cannot be appended the open mark stays, so the Run's
seal reports the incomplete audit instead of a clean ending."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function (lambda (_public) t))
            (append (symbol-function 'superchat-tape-append-event)))
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (if (equal "approval.decided" (plist-get event :type))
                         (error "tape unavailable")
                       (funcall append event)))))
          (should-error (superchat-approval-request request))))
      (should (superchat-runtime--open-tool-work-p
               (superchat-runtime--entry (plist-get request :run-id)))))))

;;;; Permission levels --------------------------------------------------------

(ert-deftest m6-permissions/levels/defaults-need-no-event ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (should (eq 'edit-buffers (superchat-sandbox-effective-mode session)))
      (should (eq 'ask (superchat-approval-effective-policy session)))
      (should (eq 'edit-buffers (superchat-permission-current session)))
      (should-not (m6p--types session)))))

(ert-deftest m6-permissions/levels/selection-writes-only-changed-knobs ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'danger-full-access)
      (should (equal '("permission.level" "sandbox.mode" "approval.policy")
                     (m6p--types session)))
      (should (eq 'danger-full-access (superchat-permission-current session)))
      ;; Selecting the effective level again records nothing.
      (superchat-permission-set session "danger-full-access")
      (should (= 3 (length (m6p--types session))))
      ;; read-only shares `ask' with nothing in force: both knobs change.
      (superchat-permission-set session 'read-only)
      (should (eq 'read-only (superchat-sandbox-effective-mode session)))
      (should (eq 'ask (superchat-approval-effective-policy session)))
      (should (= 6 (length (m6p--types session))))
      ;; read-only -> edit-buffers keeps `ask': one selection, one knob.
      (superchat-permission-set session 'edit-buffers)
      (should (equal '("permission.level" "sandbox.mode")
                     (last (m6p--types session) 2)))
      (should (= 8 (length (m6p--types session)))))))

(ert-deftest m6-permissions/levels/unmatched-knobs-report-custom ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-approval-set-policy session 'never)
      (should (eq 'custom (superchat-permission-current session)))
      (should-error (superchat-permission-set session 'custom))
      (should-error (superchat-permission-set session 'no-such-level))
      (should (eq 'custom (superchat-permission-current session))))))

(ert-deftest m6-permissions/levels/selection-survives-reopen ()
  "The level is read back from the Tape alone, as a reopened Session does."
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'read-only)
      (let ((config (superchat-view-session-configuration session)))
        (should (equal "read-only" (plist-get config :permission-level)))
        (should (equal "read-only" (plist-get config :sandbox-mode))))
      (should (eq 'read-only (superchat-permission-current session))))))

(ert-deftest m6-permissions/levels/selection-survives-a-closed-database ()
  "Two Sessions keep their own levels across a real connection close/open."
  (m6p--fixture '((:text "unused"))
    (let* ((first (buffer-local-value 'superchat--session-id (smoke--open-chat)))
           (second (format "m6p-second-%06x" (random #x1000000))))
      (superchat-permission-set first 'danger-full-access)
      (superchat-permission-set second 'read-only)
      (let ((path superchat-db--path))
        (superchat-db-close)
        (should-not superchat-db--connection)
        (superchat-db-open path))
      (should (eq 'danger-full-access (superchat-permission-current first)))
      (should (eq 'never (superchat-approval-effective-policy first)))
      (should (eq 'read-only (superchat-permission-current second)))
      (should (eq 'ask (superchat-approval-effective-policy second)))
      (should (equal '("permission.level" "sandbox.mode")
                     (m6p--types second))))))

(ert-deftest m6-permissions/levels/command-is-offered-by-default ()
  "Since the cutover the chat command registry offers /permissions as loaded."
  (skip-when (getenv "SMOKE_REAL"))
  (smoke--with-fixture '((:text "unused"))
    (smoke--open-chat)
    (should (member "permissions" (superchat--commands-list)))
    (should (member "workspace" (superchat--commands-list)))))

(ert-deftest m6-permissions/levels/command-shows-and-selects ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (should (member "permissions" (superchat--commands-list)))
      (with-current-buffer view
        (should (string-match-p
                 "\\`Permissions: edit-buffers (available: edit-buffers, danger-full-access, read-only)\\'"
                 (plist-get (superchat--cmd-permissions "permissions" "" nil nil nil)
                            :content)))
        (should (string-prefix-p
                 "Permissions: read-only — "
                 (plist-get (superchat--cmd-permissions "permissions" " read-only " nil nil nil)
                            :content)))
        (should (eq 'read-only (superchat-permission-current session)))
        (should (string-match-p
                 "Unknown permission level: nope"
                 (plist-get (superchat--cmd-permissions "permissions" "nope" nil nil nil)
                            :content)))))))

;;;; Slice 2: explicit-target writes -----------------------------------------

(defmacro m6p--with-workspace (spec &rest body)
  "Bind (ROOT OUTSIDE) in SPEC to two fresh canonical directories; run BODY.
ROOT is the workspace root of a tool execution context with no Run.  Since
M7 no rule depends on where a file lives; the two directories remain so the
tests can show that inside and outside behave the same."
  (declare (indent 1) (debug t))
  (let ((root (car spec)) (outside (cadr spec)) (base (make-symbol "base")))
    `(let* ((,base (file-name-as-directory
                    (file-truename (make-temp-file "m6p-" t))))
            (,root (file-name-as-directory (expand-file-name "root" ,base)))
            (,outside (file-name-as-directory (expand-file-name "outside" ,base)))
            (temporary-file-directory
             (file-name-as-directory (expand-file-name "tmp" ,base)))
            (superchat--tool-execution-context
             (list :workspace-root ,root))
            (superchat-tools-default-directory ,root))
       (mapc #'make-directory (list ,root ,outside temporary-file-directory))
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer)
                      (string-prefix-p ,base (buffer-file-name buffer)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (delete-directory ,base t)))))

(defun m6p--visit (file text)
  "Return a buffer visiting FILE whose saved contents are TEXT."
  (with-temp-file file (insert text))
  (find-file-noselect file))

(defun m6p--denied-p (result)
  "Non-nil when RESULT is a refusal of a structured buffer edit."
  (and (stringp result) (string-prefix-p "[Tool call denied:" result)))

(ert-deftest m6-permissions/targets/an-indirect-buffer-is-judged-by-its-base ()
  "An indirect buffer has no file name of its own; its base is what is edited
and what the result names.  Where the file lives no longer matters (M7): an
edit stays in the buffer."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((out (m6p--visit (expand-file-name "out.el" outside) "old-out"))
             (indirect (make-indirect-buffer out "m6p-indirect")))
        (unwind-protect
            (progn
              (should-not (buffer-file-name indirect))
              (let ((result (superchat-tool-edit-buffer "m6p-indirect" "old-out" "x")))
                (should (string-prefix-p "Replaced 1 occurrence" result))
                (should (string-match-p "Target: buffer out\\.el (base buffer of m6p-indirect)" result)))
              (should (equal "x" (with-current-buffer out (buffer-string))))
              ;; In memory only: the file outside the workspace is untouched.
              (should (buffer-modified-p out))
              (should (equal "old-out" (with-temp-buffer
                                         (insert-file-contents (expand-file-name "out.el" outside))
                                         (buffer-string))))
              (let* ((inside (m6p--visit (expand-file-name "in.el" root) "old-in"))
                     (clone (make-indirect-buffer inside "m6p-indirect-in"))
                     (result (superchat-tool-edit-buffer "m6p-indirect-in" "old-in" "new-in")))
                (unwind-protect
                    (progn
                      (should (string-prefix-p "Replaced 1 occurrence" result))
                      (should (string-match-p
                               "Target: buffer in\\.el (base buffer of m6p-indirect-in)" result)))
                  (kill-buffer clone))))
          (kill-buffer indirect))))))

(ert-deftest m6-permissions/targets/superchat-state-is-outside-every-workspace ()
  "The chat View and Superchat's own buffers are not scratch space."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((view (smoke--open-chat))
             (before (with-current-buffer view (buffer-string)))
             (panel (get-buffer-create "*superchat-agents*")))
        (unwind-protect
            (progn
              (should (m6p--denied-p
                       (superchat-tool-append-to-buffer (buffer-name view) "scribble")))
              (should (m6p--denied-p
                       (superchat-tool-replace-buffer "*superchat-agents*" "scribble")))
              (should (equal before (with-current-buffer view (buffer-string))))
              ;; An ordinary file-less buffer is scratch: writable, even new.
              (should (string-prefix-p
                       "Appended text to buffer m6p-new-scratch"
                       (superchat-tool-append-to-buffer "m6p-new-scratch" "note")))
              (should (equal "note" (with-current-buffer "m6p-new-scratch" (buffer-string)))))
          (kill-buffer panel)
          (when (get-buffer "m6p-new-scratch") (kill-buffer "m6p-new-scratch")))))))

(ert-deftest m6-permissions/targets/read-only-denies-before-creating-anything ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let ((superchat-sandbox-default-mode 'read-only))
        (let ((result (superchat-tool-append-to-buffer "m6p-never-created" "x")))
          (should (m6p--denied-p result))
          (should (string-match-p "read-only" result))
          ;; M7: a refusal is final -- no escalation is offered any more.
          (should-not (string-match-p "sandbox_permissions\\|escalat" result)))
        (should-not (get-buffer "m6p-never-created"))
        (let* ((inside (m6p--visit (expand-file-name "in.el" root) "old-in"))
               (result (superchat-tool-edit-buffer "in.el" "old-in" "new-in")))
          (should (m6p--denied-p result))
          (should (equal "old-in" (with-current-buffer inside (buffer-string)))))))))

;;;; Escalation

;;;; save_buffer

(ert-deftest m6-permissions/save/saves-the-whole-base-buffer-and-reports-it ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((file (expand-file-name "in.el" root))
             (buffer (m6p--visit file "line one\nline two\n"))
             (clone (make-indirect-buffer buffer "m6p-save-indirect")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (goto-char (point-max)) (insert "user edit\n")
                (goto-char (point-min)) (narrow-to-region (point) (line-end-position)))
              (superchat-tool-edit-buffer "m6p-save-indirect" "line two" "line 2")
              (let ((result (superchat-tool-save-buffer "m6p-save-indirect")))
                (should (string-prefix-p "Saved buffer in.el to " result))
                (should (string-match-p "already unsaved before this call" result))
                (should (string-match-p "^Layer: written to disk" result)))
              (should (equal "line one\nline 2\nuser edit\n"
                             (with-temp-buffer (insert-file-contents file) (buffer-string))))
              (should (with-current-buffer buffer (buffer-narrowed-p)))
              (should (string-prefix-p "No change:" (superchat-tool-save-buffer "in.el"))))
          (kill-buffer clone))))))

(ert-deftest m6-permissions/save/refuses-what-it-cannot-save-honestly ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let ((asked-for-a-name nil))
        (cl-letf (((symbol-function 'read-file-name)
                   (lambda (&rest _) (setq asked-for-a-name t) "/dev/null")))
          (with-temp-buffer
            (insert "no file")
            (should (string-match-p "is not visiting a file"
                                    (superchat-tool-save-buffer (buffer-name)))))
          (should (string-match-p "is not live" (superchat-tool-save-buffer "m6p-no-such")))
          (should-not asked-for-a-name))
        ;; M7: where the file lives is not this body's question any more.
        ;; Whether `save_buffer' may run at all is the level stage's -- it
        ;; asks on every call -- and is pinned in the m7 set.
        (ignore outside)))))

;;;; Slice 3: the level stage in the executor ---------------------------------

(defvar m6p--effect 0 "Counter proving whether a tool body ran.")

(defmacro m6p--pipeline (script &rest body)
  "Run BODY in a fixture whose default tools are offered, levels on.
The chat collectors are remounted so the permission-context collector is
registered as it will be after the cutover, and unregistered afterwards."
  (declare (indent 1) (debug t))
  `(m6p--fixture ,script
     (let ((superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (m6p--effect 0))
       (superchat-plugin-unmount 'chat-collectors)
       (superchat-plugin-mount 'chat-collectors)
       (unwind-protect (progn ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun m6p--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun m6p--count (session type)
  (length (m6p--events session type)))

(defconst m6p--eval-round
  '(:tool-uses (("eval_elisp" (expression . "(setq m6p--effect (1+ m6p--effect))")))))

(ert-deftest m6-permissions/executor/eval-asks-once-and-reads-ask-nothing ()
  "Under edit-buffers a read runs unasked and full control asks per call."
  (m6p--pipeline (list '(:tool-uses (("list_buffers")))
                       m6p--eval-round m6p--eval-round '(:text "done"))
    (let* ((asked nil)
           (superchat-approval-function
            (lambda (public) (push (plist-get public :tool-name) asked) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m6p--settle (smoke--send view "go"))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (equal '("eval_elisp" "eval_elisp") asked))
      (should (= 2 m6p--effect))
      (should (= 2 (m6p--count session "approval.requested")))
      (should (= 2 (m6p--count session "approval.decided")))
      (should (= 3 (m6p--count session "tool.completed"))))))

(ert-deftest m6-permissions/executor/a-declined-eval-never-runs ()
  (m6p--pipeline (list m6p--eval-round '(:text "declined"))
    (let* ((superchat-approval-function (lambda (_public) nil))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m6p--settle (smoke--send view "go"))
      (should (= 0 m6p--effect))
      (should (= 0 (m6p--count session "tool.execution.started")))
      (should (= 1 (m6p--count session "tool.failed")))
      (let ((followup (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Tool call cancelled by user" followup))
        (should (string-match-p "do not retry it" followup))))))

(ert-deftest m6-permissions/executor/danger-full-access-asks-nobody ()
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((asked 0)
           (superchat-approval-function (lambda (_public) (cl-incf asked) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'danger-full-access)
      (m6p--settle (smoke--send view "go"))
      (should (= 1 m6p--effect))
      (should (= 0 asked))
      (should (= 0 (m6p--count session "approval.requested"))))))

(ert-deftest m6-permissions/executor/read-only-denies-full-control-without-asking ()
  (m6p--pipeline (list m6p--eval-round '(:tool-uses (("list_buffers"))) '(:text "done"))
    (let* ((asked 0)
           (superchat-approval-function (lambda (_public) (cl-incf asked) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'read-only)
      (m6p--settle (smoke--send view "go"))
      (should (= 0 m6p--effect))
      (should (= 0 asked))
      (should (= 0 (m6p--count session "approval.requested")))
      ;; The read beside it still ran.
      (should (= 1 (m6p--count session "tool.completed")))
      (should (cl-some (lambda (request)
                         (string-match-p
                          "Tool call denied: eval_elisp is full Emacs control, which read-only mode does not grant"
                          (or (plist-get request :prompt-text) "")))
                       smoke--requests)))))

(ert-deftest m6-permissions/executor/never-rejects-without-a-question ()
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((asked 0)
           (superchat-approval-function (lambda (_public) (cl-incf asked) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-approval-set-policy session 'never)
      (m6p--settle (smoke--send view "go"))
      (should (= 0 m6p--effect))
      (should (= 0 asked))
      (should (= 1 (m6p--count session "approval.decided")))
      (should (string-match-p "approval prompts are disabled in this session"
                              (plist-get (car smoke--requests) :prompt-text))))))

(ert-deftest m6-permissions/executor/the-body-runs-the-approved-arguments ()
  "The expression the answerer saw is the one evaluated."
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((superchat-approval-function
            (lambda (public)
              ;; Mutating what we were shown must not reach the executor.
              (let ((expression (cdr (assq 'expression (plist-get public :args)))))
                (when (stringp (car-safe (plist-get public :args)))
                  (setq expression (car (plist-get public :args))))
                (when (stringp expression)
                  (dotimes (i (length expression)) (aset expression i ?\s))))
              t))
           (view (smoke--open-chat)))
      (m6p--settle (smoke--send view "go"))
      (should (= 1 m6p--effect)))))

(ert-deftest m6-permissions/executor/a-late-allow-after-cancel-starts-nothing ()
  "The Run is cancelled while the question is open; the answer still says
allow.  The decision is recorded, the body never starts, no further provider
request is made."
  (m6p--pipeline (list m6p--eval-round '(:text "must not be requested"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run nil)
           (superchat-approval-function
            (lambda (public)
              (superchat-runtime-cancel (plist-get public :run-id) "[m6p cancel while asking]")
              t)))
      (setq run (m6p--settle (smoke--send view "go")))
      (should (= 0 m6p--effect))
      (should (= 1 (m6p--count session "approval.requested")))
      (should (= 1 (m6p--count session "approval.decided")))
      (should (= 0 (m6p--count session "tool.execution.started")))
      (should (= 1 (length smoke--requests)))
      ;; The Run was sealed while its approval was still undecided, which is
      ;; the ending `m3a/seal/an-undecided-approval-...' pins for Authority.
      (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest m6-permissions/executor/a-listener-cannot-skip-the-level-stage ()
  "An `allow' that ends the waterfall without delegating does not spare
eval_elisp its question; a listener's `deny' beats the level's `allow'."
  (m6p--pipeline (list m6p--eval-round '(:tool-uses (("list_buffers"))) '(:text "done"))
    (let* ((asked 0)
           (superchat-approval-function (lambda (_public) (cl-incf asked) t))
           (off (superchat-on nil 'tools/pre-execute 'm6p-gate
                              (lambda (call _next)
                                ;; The contract's shape: the call back with `:kind'.
                                (if (equal "list_buffers" (plist-get call :tool-name))
                                    (plist-put call :kind 'deny)
                                  (plist-put call :kind 'allow)))))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (unwind-protect (m6p--settle (smoke--send view "go"))
        (funcall off))
      (should (= 1 asked))
      (should (= 1 m6p--effect))
      ;; list_buffers was denied by the listener: one completion only.
      (should (= 1 (m6p--count session "tool.completed"))))))

(ert-deftest m6-permissions/executor/tool-classes ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat-mcp-trusted-servers '("notes"))
        (superchat-mcp--tool-servers (make-hash-table :test #'equal))
        (superchat--syn-names '("my_helper")))
    (puthash "mcp:notes:search" "notes" superchat-mcp--tool-servers)
    (puthash "mcp:shell:run" "shell" superchat-mcp--tool-servers)
    (should (eq 'builtin (superchat-sandbox-tool-class "read_buffer")))
    (should (eq 'builtin (superchat-sandbox-tool-class "edit_buffer")))
    ;; `write_file' is retired (M7): the name is no longer a built-in.
    (should-not (eq 'builtin (superchat-sandbox-tool-class "write_file")))
    (should (eq 'builtin (superchat-sandbox-tool-class "ask_user")))
    (dolist (name '("eval_elisp" "shell-command" "eshell-command" "define_tool" "my_helper"))
      (should (eq 'full-control (superchat-sandbox-tool-class name))))
    (should (eq 'trusted (superchat-sandbox-tool-class "mcp:notes:search")))
    (should (eq 'outside (superchat-sandbox-tool-class "mcp:shell:run")))
    (should (eq 'outside (superchat-sandbox-tool-class "telega_send")))
    ;; Trusted says nothing about reading only: read-only denies it too.
    (should (eq 'deny (car (superchat-sandbox--level-rule "mcp:notes:search" 'read-only))))
    (should (eq 'allow (car (superchat-sandbox--level-rule "mcp:notes:search" 'edit-buffers))))
    (should (eq 'ask (car (superchat-sandbox--level-rule "mcp:shell:run" 'edit-buffers))))
    (should (eq 'allow (car (superchat-sandbox--level-rule "mcp:shell:run" 'danger-full-access))))))

(ert-deftest m6-permissions/executor/an-agent-written-tool-asks-nobody-a-second-time ()
  "The permission level is the only gate: the tool's own body never prompts,
so `danger-full-access' really asks nothing, and its errors come back as text."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((prompted 0))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (cl-incf prompted) nil))
              ((symbol-function 'superchat--syn-function-symbol)
               (lambda (_name) (lambda (x) (if (equal x "boom") (error "kaput") (concat "ran:" x))))))
      (let ((call (superchat--syn-guarded-function "m6p_syn")))
        (should (equal "ran:1" (funcall call "1")))
        (should (string-match-p "errored: kaput" (funcall call "boom")))
        (should (= 0 prompted))))
    (should-not (boundp 'superchat-agent-confirm-synthesized))
    (should-not (fboundp 'superchat--syn-confirm))))

(ert-deftest m6-permissions/context/the-request-states-the-current-policy ()
  "A hard fragment after everything else states the policy; a level change
shows in the next request."
  (m6p--pipeline (list '(:text "one") '(:text "two"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m6p--settle (smoke--send view "first"))
      (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Current Superchat mode: edit-buffers" prompt))
        (should (string-match-p "Approval policy: ask" prompt)))
      (superchat-permission-set session 'danger-full-access)
      (m6p--settle (smoke--send view "second"))
      (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Current Superchat mode: danger-full-access" prompt))
        (should (string-match-p "Approval prompts are disabled in this session" prompt))))))

(ert-deftest m6-permissions/delegation/a-child-session-inherits-the-mode-and-asks-nobody ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (parent (buffer-local-value 'superchat--session-id view))
           (preset (superchat-preset--create :name "m6p-child" :type 'agent)))
      (superchat-permission-set parent 'read-only)
      (let* ((superchat--tool-execution-context (list :session-id parent))
             (context (superchat--subagent-make-context preset 1))
             (child (plist-get context :session-id)))
        (should (stringp child))
        (should (eq 'read-only (superchat-sandbox-effective-mode child)))
        (should (eq 'never (superchat-approval-effective-policy child)))))))

(ert-deftest m6-permissions/workspace/write-checks-the-artifacts-target ()
  "A region Artifact is a buffer edit: it is judged like one -- editable in
the default mode wherever its file lives, refused under read-only -- and the
Workspace's own fallback buffer is scratch space."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (superchat--tool-execution-context
              (list :workspace-root root :session-id session
                    :workspace-id (format "m6p-ws-%06x" (random #x1000000)))))
        (should (string-match-p "fallback buffer"
                                (superchat-tool-workspace-write "note")))
        (let ((out (m6p--visit (expand-file-name "out.txt" outside) "region text\n")))
          (with-current-buffer out
            (superchat-artifact-board-add-region (point-min) (point-max)))
          (let ((superchat-sandbox-default-mode 'read-only))
            (should (m6p--denied-p (with-current-buffer out
                                     (superchat-tool-workspace-write "replaced"))))
            (should (equal "region text\n" (with-current-buffer out (buffer-string)))))
          ;; In the default mode the same call is not refused for where the
          ;; file lives; it gets as far as the Workspace's own revision check.
          (let ((condition (should-error (with-current-buffer out
                                           (superchat-tool-workspace-write "replaced")))))
            (should (string-match-p "Stale workspace artifact revision"
                                    (error-message-string condition)))))))))

;;;; Review of slice 3 ----------------------------------------------------------

(ert-deftest m6-permissions/review/the-fallback-name-does-not-launder-a-file-buffer ()
  "A buffer that visits a file outside the workspace stays an outside target
when it carries the Workspace fallback's name; a file-less one is scratch."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (workspace-id (format "m6p-ws-%06x" (random #x1000000)))
             (superchat--tool-execution-context
              (list :workspace-root root :session-id session :workspace-id workspace-id))
             (fallback (superchat-artifact-board--fallback-buffer-name workspace-id))
             (imposter (m6p--visit (expand-file-name "outside.txt" outside) "original")))
        (with-current-buffer imposter (rename-buffer fallback))
        (should (m6p--denied-p (superchat-tool-workspace-write "replacement")))
        (should (equal "original" (with-current-buffer imposter (buffer-string))))
        (with-current-buffer imposter (set-buffer-modified-p nil))
        (kill-buffer imposter)
        ;; The honest fallback: no file, so scratch space.
        (should (string-match-p "fallback buffer"
                                (superchat-tool-workspace-write "note")))))))

(ert-deftest m6-permissions/review/trust-is-for-the-exact-registered-server ()
  "A server whose name starts with a trusted name and a colon is not it."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat-mcp-trusted-servers '("notes"))
        (superchat-mcp--tool-servers (make-hash-table :test #'equal)))
    (puthash "mcp:notes:foreign:touch" "notes:foreign" superchat-mcp--tool-servers)
    (puthash "mcp:notes:search" "notes" superchat-mcp--tool-servers)
    (should (eq 'outside (superchat-sandbox-tool-class "mcp:notes:foreign:touch")))
    (should (eq 'trusted (superchat-sandbox-tool-class "mcp:notes:search")))
    ;; An MCP-shaped name nobody registered is trusted by nobody.
    (should (eq 'outside (superchat-sandbox-tool-class "mcp:notes:unregistered")))))

(ert-deftest m6-permissions/review/registration-records-the-exact-server ()
  "The real adapter registration is what fills the server registry."
  (m6p--fixture '((:text "unused"))
    (let ((superchat-mcp--tool-servers (make-hash-table :test #'equal))
          (superchat-mcp-trusted-servers '("notes")))
      (unwind-protect
          (progn
            (superchat-mcp--register-tool
             (list :name "mcp:notes:foreign:touch" :function #'ignore
                   :description "touch" :args nil)
             "notes:foreign")
            (should (equal "notes:foreign"
                           (superchat-mcp-tool-server "mcp:notes:foreign:touch")))
            (should (eq 'outside (superchat-sandbox-tool-class "mcp:notes:foreign:touch"))))
        (ignore-errors (superchat-unregister-tool "mcp:notes:foreign:touch"))))))

(ert-deftest m6-permissions/review/a-tool-continuation-states-the-level-in-force ()
  "The level changes while the first tool's question is open; the request
that follows the tool round states the new level, not the submitted one."
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (superchat-approval-function
            (lambda (_public) (superchat-permission-set session 'read-only) t)))
      (m6p--settle (smoke--send view "go"))
      (should (= 2 (length smoke--requests)))
      (let ((first (plist-get (cadr smoke--requests) :prompt-text))
            (second (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Superchat mode: edit-buffers" first))
        (should (string-match-p "Superchat mode: read-only" second))
        (should-not (string-match-p "Superchat mode: edit-buffers" second))
        (should (= 1 (m6p--count-matches "Current runtime context\\." second)))))))

(defun m6p--count-matches (regexp text)
  (let ((start 0) (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count) start (match-end 0)))
    count))

(ert-deftest m6-permissions/review/a-childs-fixed-values-do-not-follow-the-defaults ()
  "The child is pinned even when its values equal today's defaults."
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (parent (buffer-local-value 'superchat--session-id view))
           (preset (superchat-preset--create :name "m6p-child" :type 'agent))
           (superchat--tool-execution-context (list :session-id parent))
           (child (plist-get (superchat--subagent-make-context preset 1) :session-id)))
      (should (eq 'edit-buffers (superchat-sandbox-override child)))
      (should (eq 'never (superchat-approval-override child)))
      (let ((superchat-sandbox-default-mode 'danger-full-access)
            (superchat-approval-default-policy 'never))
        (should (eq 'edit-buffers (superchat-sandbox-effective-mode child)))))))

;;;; Slice 4, step 1a: the whole new chain ------------------------------------

(defun m6p--submit-tools (view session tools text)
  "Submit TEXT from VIEW with the explicit tool list TOOLS; settle the Run."
  (m6p--settle
   (plist-get (superchat-service-call
               nil 'driver 'submit
               (list :surface 'external :session-id session :tools tools
                     :content text
                     :context (list :host-buffer view)
                     :runtime-extras '(:record-conversation nil)
                     :sink (superchat-sink-discard)))
              :run-id)))

(defmacro m6p--with-registered-tool (spec &rest body)
  "Register tool SPEC = (NAME FUNCTION &rest KEYS) without effects; run BODY."
  (declare (indent 1) (debug t))
  `(progn
     (superchat-register-tool ,(car spec) :function ,(cadr spec)
                              :description "m6p test tool"
                              :args (list (list :name "x" :type 'string :description "x"))
                              ,@(cddr spec))
     (unwind-protect (progn ,@body)
       (ignore-errors (superchat-unregister-tool ,(car spec))))))

(ert-deftest m6-permissions/chain/a-delegating-answerer-gets-the-persons-decision ()
  "The chain ends in the human answerer: a listener that delegates reads the
outcome from its continuation, so it can record a grant only after a yes."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((granted nil)
             (human 0)
             (superchat-approval-function (lambda (_public) (cl-incf human) t))
             (off (superchat-on nil 'approval/request 'm6p-granting
                                (lambda (public next)
                                  (let ((outcome (funcall next public)))
                                    (when (eq outcome 'allowed-once)
                                      (push (plist-get public :tool-name) granted))
                                    outcome)))))
        (unwind-protect
            (progn
              (should (eq 'allowed-once (superchat-approval-request request)))
              (should (= 1 human))
              (should (equal '("m6p_tool") granted))
              ;; A no is delegated back the same way and grants nothing.
              (let ((superchat-approval-function (lambda (_public) nil)))
                (should (eq 'rejected
                            (superchat-approval-request
                             (plist-put (copy-sequence request) :execution-id "m6p-no")))))
              (should (= 1 (length granted))))
          (funcall off))))))

(ert-deftest m6-permissions/chain/nobody-answers-in-batch-unless-somebody-was-put-there ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((noninteractive t)
            (n 0))
        (cl-flet ((ask () (superchat-approval-request
                           (plist-put (copy-sequence request) :execution-id
                                      (format "m6p-batch-%d" (cl-incf n))))))
          (let ((superchat-approval-function #'superchat-approval-confirm))
            (should (eq 'unavailable (ask))))
          (let ((superchat-approval-function (lambda (_public) t)))
            (should (eq 'allowed-once (ask))))
          ;; A listener that neither answers nor delegates answers nothing.
          (let ((superchat-approval-function (lambda (_public) t))
                (off (superchat-on nil 'approval/request 'm6p-mute
                                   (lambda (_public _next) nil))))
            (unwind-protect (should (eq 'unavailable (ask)))
              (funcall off))))))))

(defun m6p--load-rmc-source ()
  "Load `read-multiple-choice' from its source file, once.
It is the same function, only not natively compiled: compiled code calls a
primitive such as `read-from-minibuffer' directly, so a test could not stand
in for the keyboard underneath it."
  (unless (get 'm6p--load-rmc-source 'done)
    (load (locate-library "rmc.el" t) nil t)
    (put 'm6p--load-rmc-source 'done t)))

(defun m6p--rmc-uses-minibuffer-p ()
  "Return non-nil when `read-multiple-choice' reads its answer in the minibuffer.
Emacs 31 and later do, with a modal `read-key' path beside it; Emacs 30 reads
every key with `read-event' and has no other path."
  (m6p--load-rmc-source)
  (fboundp 'read-multiple-choice--from-minibuffer))

(defun m6p--help-text ()
  "Return the text of the help buffer `read-multiple-choice' shows, or nil."
  (and (get-buffer "*Multiple Choice Help*")
       (with-current-buffer "*Multiple Choice Help*" (buffer-string))))

(defun m6p--confirm-with-keys (request keys)
  "Answer `superchat-approval-confirm' for REQUEST by typing KEYS.
`read-multiple-choice' is the real one.  Only its lowest input primitive is
replaced, because a batch Emacs reads its input from stdin.  On the minibuffer
path each key is looked up in the keymap the real function built and its
command is run, as the command loop would; on Emacs 30's `read-event' path
each call hands over the next key.  Return (ANSWER . LOG); LOG holds, for
every key that left the question open, the text of the help buffer it
displayed."
  (let ((log nil)
        (read-char-choice-use-read-key nil))
    (if (m6p--rmc-uses-minibuffer-p)
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (_prompt _initial map &rest _)
                     (catch 'm6p-answered
                       (dolist (key keys)
                         (let ((last-command-event key)
                               (command (lookup-key map (vector key)))
                               (exited t))
                           (catch 'exit
                             (call-interactively command)
                             (setq exited nil))
                           (if exited
                               (throw 'm6p-answered nil)
                             (push (cons key (m6p--help-text)) log))))
                       (error "m6p: the question was still open after %S" keys)))))
          (cons (superchat-approval-confirm request) (nreverse log)))
      ;; Emacs 30 calls `read-event' once per key and swallows its errors, so
      ;; running out of keys leaves by `throw', never by `error'.
      (let* ((pending keys)
             (previous nil)
             (outcome
              (catch 'm6p-still-open
                (cl-letf (((symbol-function 'read-event)
                           (lambda (&rest _)
                             (when previous
                               (push (cons previous (m6p--help-text)) log))
                             (unless pending
                               (throw 'm6p-still-open nil))
                             (setq previous (pop pending)))))
                  (list (superchat-approval-confirm request))))))
        (unless outcome
          (error "m6p: the question was still open after %S" keys))
        (cons (car outcome) (nreverse log))))))

(ert-deftest m6-permissions/chain/question-mark-shows-the-arguments-and-decides-nothing ()
  "Through the real `read-multiple-choice' and the keymap it builds: `?'
displays the tool, the reason and the whole arguments -- the code an
`eval_elisp' call would run -- and the question stays open until y or n."
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((code "(progn (delete-file \"~/precious\") (message \"m6p-unique-marker\"))")
         (request (list :tool-name "eval_elisp"
                        :reason "eval_elisp has full control of Emacs"
                        :args (list code))))
    (pcase-let ((`(,answer . ,log) (m6p--confirm-with-keys request '(?? ?? ?y))))
      (should answer)
      (should (= 2 (length log)))
      (dolist (entry log)
        (should (eq ?? (car entry)))
        (should (string-match-p "Tool: eval_elisp" (cdr entry)))
        (should (string-match-p "Reason: eval_elisp has full control of Emacs" (cdr entry)))
        (should (string-match-p "m6p-unique-marker" (cdr entry)))
        (should (string-match-p "delete-file" (cdr entry)))))
    (pcase-let ((`(,answer . ,log) (m6p--confirm-with-keys request '(?? ?n))))
      (should-not answer)
      (should (= 1 (length log))))
    (should-not (car (m6p--confirm-with-keys request '(?n))))
    ;; The help buffer does not outlive the question.
    (should-not (get-buffer "*Multiple Choice Help*"))))

(ert-deftest m6-permissions/chain/the-modal-key-path-shows-the-same-details ()
  "With `read-char-choice-use-read-key' the other input path of the real
`read-multiple-choice' runs; `?' shows the same details there.  Emacs 30 has
no such second path (every key comes from `read-event'), so there is nothing
to run; the question-mark test covers its one path."
  (skip-when (getenv "SMOKE_REAL"))
  (skip-unless (m6p--rmc-uses-minibuffer-p))
  (let* ((keys (list ?? ?y))
         (seen nil)
         (read-char-choice-use-read-key t)
         (request '(:tool-name "eval_elisp" :reason "r" :args ("(m6p-modal-marker)"))))
    (cl-letf (((symbol-function 'read-key)
               (lambda (&rest _)
                 (when (get-buffer "*Multiple Choice Help*")
                   (setq seen (with-current-buffer "*Multiple Choice Help*"
                                (buffer-string))))
                 (or (pop keys) (error "m6p: out of keys")))))
      (should (superchat-approval-confirm request))
      (should (stringp seen))
      (should (string-match-p "m6p-modal-marker" seen)))))

(ert-deftest m6-permissions/chain/a-session-that-never-asks-is-not-offered-ask-user ()
  "What `:attendance unattended' used to say is the approval policy `never':
a Session that asks nobody gets no human-input adapter, so `ask_user' is not
attached; the same request under `ask' gets one."
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (request (list :buffer view :session-id session :tools nil)))
      (should (plist-get (superchat--with-supported-input request 'response) :input-core))
      (superchat-approval-set-policy session 'never)
      (should-not (plist-member (superchat--with-supported-input request 'response)
                                :input-core))
      (superchat-approval-set-policy session 'ask)
      (should (plist-get (superchat--with-supported-input request 'response) :input-core)))))

(ert-deftest m6-permissions/chain/the-decision-machine-is-gone ()
  "No effect resolver, base decision, approval hash or argument rewriter is
left to call, and a Tape row carries none of the machine's keys."
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'danger-full-access)
      (m6p--settle (smoke--send view "go"))
      (should (= 1 m6p--effect))
      (dolist (gone '(superchat-authority-resolve-effects superchat-authority-decide
                      superchat-authority-approval-hash superchat-authority-confirm
                      superchat-authority-register-policy
                      superchat-authority-register-effect-resolver
                      superchat-authority-register-approval-preview))
        (should-not (fboundp gone)))
      (dolist (gone '(superchat-authority-approval-function
                      superchat-authority-unprompted-read-tools
                      superchat-authority-sensitive-buffers
                      superchat-authority-prompt-for-all-reads
                      superchat-mcp-server-effects
                      superchat-agent-confirm-destructive
                      superchat--permission-levels-enabled))
        (should-not (boundp gone)))
      (dolist (event '(tool-permission tool-args-rewrite tool-result-filter))
        (should-error (superchat-on nil event 'm6p-gone #'ignore)))
      (let ((requested (car (m6p--events session "tool.requested"))))
        (should (m6p--payload requested :execution-id))
        (dolist (key '(:effects :policy :policy-version :attendance :request-hash))
          (should-not (m6p--payload requested key)))))))

(ert-deftest m6-permissions/chain/execute-wraps-a-synchronous-body-once ()
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((seen nil)
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (off (superchat-on nil 'tools/execute 'm6p-around
                              (lambda (call next)
                                (push (list 'before m6p--effect) seen)
                                (let ((after (funcall next call)))
                                  (push (list 'after m6p--effect (plist-get after :started)) seen)
                                  after)))))
      (superchat-permission-set session 'danger-full-access)
      (unwind-protect (m6p--settle (smoke--send view "go"))
        (funcall off))
      (should (equal '((before 0) (after 1 t)) (reverse seen)))
      (should (= 1 (m6p--count session "tool.completed"))))))

(ert-deftest m6-permissions/chain/execute-can-end-a-call-without-running-it ()
  "A listener that answers with a result instead of delegating: the body does
not run, the call is recorded as blocked, and one result reaches the model."
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (off (superchat-on nil 'tools/execute 'm6p-short
                              (lambda (call _next)
                                (plist-put call :result "[Tool call blocked: m6p maintenance window]")))))
      (superchat-permission-set session 'danger-full-access)
      (unwind-protect (m6p--settle (smoke--send view "go"))
        (funcall off))
      (should (= 0 m6p--effect))
      (should (= 0 (m6p--count session "tool.execution.started")))
      (should (= 1 (m6p--count session "tool.failed")))
      (should (= 1 (m6p--count-matches
                    "m6p maintenance window"
                    (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest m6-permissions/chain/an-asynchronous-body-runs-and-delivers-once ()
  (m6p--fixture '((:tool-uses (("m6p_async" (x . "1")))) (:text "done"))
    (let* ((bodies 0)
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (superchat-permission-set session 'danger-full-access)
      (m6p--with-registered-tool
          ("m6p_async"
           (lambda (done x)
             (cl-incf bodies)
             (run-at-time 0.01 nil done (format "M6P_ASYNC_RESULT:%s" x)))
           :async t)
        (let ((superchat-llm-tools-list nil))
          (m6p--submit-tools view session '("m6p_async") "go")))
      (should (= 1 bodies))
      (should (= 1 (m6p--count session "tool.completed")))
      (should (= 1 (m6p--count-matches
                    "M6P_ASYNC_RESULT:1"
                    (plist-get (car smoke--requests) :prompt-text)))))))

(ert-deftest m6-permissions/chain/post-execute-replaces-or-blocks-a-result ()
  (m6p--fixture '((:tool-uses (("m6p_echo" (x . "secret")) ("m6p_echo" (x . "bad"))))
                  (:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (off (superchat-on
                 nil 'tools/post-execute 'm6p-post
                 (lambda (outcome next)
                   (funcall next
                            (cond
                             ((string-match-p "secret" (plist-get outcome :result))
                              (plist-put outcome :result "M6P_REDACTED"))
                             ((string-match-p "bad" (plist-get outcome :result))
                              (plist-put (plist-put outcome :kind 'block)
                                         :feedback "m6p says this result is unusable"))
                             (t outcome)))))))
      (superchat-permission-set session 'danger-full-access)
      (unwind-protect
          (m6p--with-registered-tool ("m6p_echo" (lambda (x) (format "echo:%s" x)))
            (let ((superchat-llm-tools-list nil))
              (m6p--submit-tools view session '("m6p_echo") "go")))
        (funcall off))
      (let ((followup (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "M6P_REDACTED" followup))
        (should-not (string-match-p "echo:secret" followup))
        (should (string-match-p "Error: m6p says this result is unusable" followup))
        (should (string-match-p "Tool execution failed" followup))))))

(ert-deftest m6-permissions/selection/a-registered-tool-needs-no-effects-under-levels ()
  "It can be registered, selected by name and executed; it is an outside
tool, so edit-buffers asks about it."
  (m6p--fixture '((:tool-uses (("m6p_plain" (x . "1")))) (:text "done"))
    (let* ((asked nil)
           (superchat-approval-function
            (lambda (public) (push (plist-get public :tool-name) asked) t))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m6p--with-registered-tool ("m6p_plain" (lambda (x) (format "plain:%s" x)))
        (should (superchat--tool-descriptor-p "m6p_plain"))
        (let ((superchat-llm-tools-list nil))
          (m6p--submit-tools view session '("m6p_plain") "go")))
      (should (equal '("m6p_plain") asked))
      (should (string-match-p "plain:1" (plist-get (car smoke--requests) :prompt-text)))
      (should-not (superchat--tool-descriptor-p "m6p_plain"))))
  ;; Effect declarations are gone with the decision machine: the keyword is
  ;; no longer part of registration.
  (should-error (superchat-register-tool "m6p_effects" :function #'ignore
                                         :description "d"
                                         :effects '((:effect local-read :resource buffer)))))

(ert-deftest m6-permissions/selection/built-in-tools-leave-with-their-plugin ()
  "A built-in tool is selectable because the mounted plugin installed it, not
because its name is on a list: unmounting the plugin withdraws every one."
  (m6p--fixture '((:text "unused"))
    (should (superchat--tool-descriptor-p "read_buffer"))
    (should (member "save_buffer" (superchat-tools-visible-descriptor-names)))
    (unwind-protect
        (progn
          (superchat-plugin-unmount 'tools-builtin)
          (should-not (superchat--tool-descriptor-p "read_buffer"))
          (should-not (superchat--tool-descriptor-p "write_file"))
          (should-not (member "read_buffer"
                              (superchat-tools-visible-descriptor-names))))
      (superchat-plugin-mount 'tools-builtin))
    (should (superchat--tool-descriptor-p "read_buffer"))))

(ert-deftest m6-permissions/review/a-session-view-named-like-the-fallback-is-not-scratch ()
  "A chat View, owned by its Session, renamed to a Workspace's fallback name
is still Superchat's own state; the real fallback stays writable."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (workspace-id (format "m6p-ws-%06x" (random #x1000000)))
             (superchat--tool-execution-context
              (list :workspace-root root :session-id session :workspace-id workspace-id))
             (fallback (superchat-artifact-board--fallback-buffer-name workspace-id))
             (original-name (buffer-name view))
             (before (with-current-buffer view (buffer-string))))
        (should (eq 'internal
                    (plist-get (superchat-sandbox-buffer-target view) :kind)))
        (unwind-protect
            (progn
              (with-current-buffer view (rename-buffer fallback))
              (should (m6p--denied-p (superchat-tool-workspace-write "replacement")))
              (should (equal before (with-current-buffer view (buffer-string)))))
          (with-current-buffer view (rename-buffer original-name)))
        (should (string-match-p "fallback buffer"
                                (superchat-tool-workspace-write "note")))))))

;;;; Slice 4, step 1b: the Telega plugin on the new seams ---------------------

(require 'superchat-cowork-telega)

(defmacro m6p--with-telega (&rest body)
  "Run BODY with Telega's API stubbed: chats exist, histories are empty."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'telega-chat-get)
              (lambda (id &optional _) (list :id id :title (format "Chat %s" id))))
             ((symbol-function 'telega-chat-title)
              (lambda (chat &optional _ _) (plist-get chat :title)))
             ((symbol-function 'telega-completing-read-chat-list)
              (lambda (&rest _) '((:id 123 :title "Chat 123"))))
             ((symbol-function 'telega--getChatHistory)
              (lambda (_chat _from _offset _limit _local callback)
                (funcall callback (list :messages []))))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'superchat-cowork-telega--show-digest) #'ignore)
             ((symbol-function 'pop-to-buffer) #'ignore))
     ,@body))

(defun m6p--telega-read (chat-id)
  `(:tool-uses ((,superchat-cowork-telega-read-tool-name (chat_id . ,chat-id)))))

(ert-deftest m6-permissions/telega/digest-reads-granted-chats-unasked-and-grants-nothing ()
  "A granted chat is read without a question; another chat is asked about
every time, because a digest's yes never widens its scope."
  (m6p--fixture (list (m6p--telega-read "123") (m6p--telega-read "999")
                      (m6p--telega-read "999") '(:text "digest"))
    (m6p--with-telega
      (let* ((asked nil)
             (superchat-approval-function
              (lambda (public)
                (push (format "%s" (car (plist-get public :args))) asked)
                t))
             (handle (superchat-telega-digest "summarize")))
        (m6p--settle (plist-get handle :run-id))
        (should (equal '("999" "999") asked))
        (should (= 3 (m6p--count "telega-digest" "tool.completed")))
        ;; The Run is over: its Scope and listeners are gone.
        (should-not superchat-cowork-telega--request-scopes)))))

(ert-deftest m6-permissions/telega/a-session-grants-a-chat-only-after-the-person-says-yes ()
  "The cowork Session's answerer reads the person's decision from its
continuation: a no grants nothing, a yes grants the chat, and the next read
of that chat asks nobody."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let* ((cowork (superchat-cowork-telega--session-create
                      :id "telega-cowork-100" :chat-id 100 :chat-title "Chat 100"))
             (scope (superchat-scope-create "m6p-telega" superchat-scope-global))
             (human 0)
             (answer nil)
             (superchat-approval-function (lambda (_public) (cl-incf human) answer))
             (n 0))
        (cl-letf (((symbol-function 'superchat-cowork-telega-session-grant-chats)
                   (lambda (_buffer chats)
                     (dolist (chat chats)
                       (push chat (superchat-cowork-telega--session-scopes cowork)))))
                  ((symbol-function 'superchat-cowork-telega--session-buffer)
                   (lambda (_session) nil)))
          (unwind-protect
              (cl-flet ((read-chat ()
                          (superchat-approval-request
                           (append (list :tool-name superchat-cowork-telega-read-tool-name
                                         :args '("100" 50)
                                         :execution-id (format "m6p-tg-%d" (cl-incf n)))
                                   request)
                           scope)))
                (superchat-on scope 'approval/request 'm6p-session
                              (superchat-cowork-telega--session-answerer cowork))
                (should (eq 'rejected (read-chat)))
                (should-not (superchat-cowork-telega--granted-chat-p cowork 100))
                (setq answer t)
                (should (eq 'allowed-once (read-chat)))
                (should (superchat-cowork-telega--granted-chat-p cowork 100))
                (should (eq 'allowed-once (read-chat)))
                (should (= 2 human)))
            (ignore-errors (superchat-scope-dispose scope))))))))

(ert-deftest m6-permissions/telega/request-scopes-do-not-share-grants ()
  "Two digests alive at once: each answers from its own granted chats."
  (m6p--fixture '((:text "unused"))
    (let* ((first (superchat-scope-create "m6p-a" superchat-scope-global))
           (second (superchat-scope-create "m6p-b" superchat-scope-global))
           (superchat-approval-function (lambda (_public) nil)))
      (unwind-protect
          (cl-flet ((outcome (scope chat)
                      (let ((superchat-event-waterfall-terminal (lambda (_p) 'rejected)))
                        (superchat-emit scope 'approval/request
                                        (list :tool-name superchat-cowork-telega-read-tool-name
                                              :args (list chat))))))
            (superchat-on first 'approval/request 'a (superchat-cowork-telega--digest-answerer '(1)))
            (superchat-on second 'approval/request 'b (superchat-cowork-telega--digest-answerer '(2)))
            (should (eq 'allowed-once (outcome first "1")))
            (should (eq 'rejected (outcome first "2")))
            (should (eq 'allowed-once (outcome second "2")))
            (should (eq 'rejected (outcome second "1"))))
        (superchat-scope-dispose first)
        (superchat-scope-dispose second)))))

(ert-deftest m6-permissions/telega/a-digest-cannot-send-whatever-it-is-offered ()
  "The request's own listener refuses every tool outside its list, so the
level stage never gets to ask about a send."
  (m6p--fixture '((:hang t))
    (m6p--with-telega
      (let* ((handle (superchat-telega-digest "summarize"))
             (scope (car superchat-cowork-telega--request-scopes))
             (context (list :session-id "telega-digest"
                            :run-id (plist-get handle :run-id))))
        (unwind-protect
            (progn
              (should scope)
              (let ((send (superchat-permission-level-decision
                           superchat-cowork-telega-tool-name '("123" "hi")
                           context scope)))
                (should (eq 'deny (plist-get send :decision)))
                (should (string-match-p "not a tool of this Telega request"
                                        (plist-get send :reason))))
              (should (eq 'deny (plist-get (superchat-permission-level-decision
                                            "eval_elisp" '("(ignore)") context scope)
                                           :decision)))
              (should (eq 'ask (plist-get (superchat-permission-level-decision
                                           superchat-cowork-telega-read-tool-name
                                           '("123") context scope)
                                          :decision)))
              ;; Another request's Scope hears none of it.
              (should (eq 'ask (plist-get (superchat-permission-level-decision
                                           "eval_elisp" '("(ignore)") context
                                           superchat-scope-global)
                                          :decision))))
          (superchat-runtime-cancel (plist-get handle :run-id) "[m6p]")
          (m6p--settle (plist-get handle :run-id)))
        (should-not superchat-cowork-telega--request-scopes)))))

(ert-deftest m6-permissions/telega/a-request-scope-ends-with-its-request ()
  "Disposed at a terminal Run whatever the status, when a queued request is
dropped, and when submission fails."
  (m6p--fixture '((:hang t) (:hang t))
    (m6p--with-telega
      ;; A cancelled Run.
      (let ((handle (superchat-telega-digest "summarize")))
        (should (= 1 (length superchat-cowork-telega--request-scopes)))
        (superchat-runtime-cancel (plist-get handle :run-id) "[m6p]")
        (m6p--settle (plist-get handle :run-id))
        (should-not superchat-cowork-telega--request-scopes))
      ;; A failed submission.
      (cl-letf (((symbol-function 'superchat-pipeline-submit)
                 (lambda (&rest _) (error "m6p submit failed"))))
        (should-error (superchat-telega-digest "summarize"))
        (should-not superchat-cowork-telega--request-scopes))
      ;; A queued request that is dropped never becomes a Run.
      (let ((running (superchat-telega-digest "first")))
        (superchat-telega-digest "second")
        (should (= 2 (length superchat-cowork-telega--request-scopes)))
        (should (gethash "telega-digest" superchat-runtime--session-queues))
        (superchat-runtime-clear-follow-ups "telega-digest" 'm6p-cleared)
        (smoke--wait (lambda () (= 1 (length superchat-cowork-telega--request-scopes))) 5)
        (should (= 1 (length superchat-cowork-telega--request-scopes)))
        (superchat-runtime-cancel (plist-get running :run-id) "[m6p]")
        (m6p--settle (plist-get running :run-id))
        (should-not superchat-cowork-telega--request-scopes)))))

(provide 'm6-permissions-regression)
;;; m6-permissions-regression.el ends here
