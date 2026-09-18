;;; m6-permissions-regression.el --- permission levels and the approval seam -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-09-17-permissions-the-dsh-way.md.
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
           (superchat--permission-levels-enabled t)
           (superchat-approval-function nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'workspace-write))
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
                          :attendance 'attended :content "hang"
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
      (let ((superchat-approval-function nil)
            (superchat-authority-approval-function nil))
        (should (eq 'unavailable (superchat-approval-request request)))))))

(ert-deftest m6-permissions/approval/previous-answerer-variable-still-serves ()
  "Until the cutover the fixtures' Authority answerer is the fallback."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (let ((superchat-approval-function nil)
            (superchat-authority-approval-function (lambda (_request) t)))
        (should (eq 'allowed-once (superchat-approval-request request)))))))

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
                (setq during (superchat-runtime--open-authority-work-p
                              (superchat-runtime--entry run)))
                t)))
        (superchat-approval-request request)
        (should during)
        (should-not (superchat-runtime--open-authority-work-p
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
      (should-not (superchat-runtime--open-authority-work-p
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
      (should-not (superchat-runtime--open-authority-work-p
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
      (should-not (superchat-runtime--open-authority-work-p
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
      (should (superchat-runtime--open-authority-work-p
               (superchat-runtime--entry (plist-get request :run-id)))))))

;;;; Permission levels --------------------------------------------------------

(ert-deftest m6-permissions/levels/defaults-need-no-event ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (should (eq 'workspace-write (superchat-sandbox-effective-mode session)))
      (should (eq 'ask (superchat-approval-effective-policy session)))
      (should (eq 'workspace-write (superchat-permission-current session)))
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
      ;; read-only -> workspace-write keeps `ask': one selection, one knob.
      (superchat-permission-set session 'workspace-write)
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

(ert-deftest m6-permissions/levels/resolve-prefers-an-approved-mode ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (root (file-name-as-directory (file-truename temporary-file-directory))))
      (superchat-sandbox-set-mode session 'read-only)
      (should (eq 'read-only
                  (plist-get (superchat-sandbox-resolve
                              :session-id session :workspace-root root)
                             :mode)))
      (should (eq 'danger-full-access
                  (plist-get (superchat-sandbox-resolve
                              :session-id session :mode 'danger-full-access
                              :workspace-root root)
                             :mode)))
      (should (equal root
                     (plist-get (superchat-sandbox-resolve
                                 :session-id session :workspace-root root)
                                :workspace-root)))
      (should (superchat-sandbox-wider-p 'workspace-write 'read-only))
      (should (superchat-sandbox-wider-p 'danger-full-access 'workspace-write))
      (should-not (superchat-sandbox-wider-p 'workspace-write 'workspace-write))
      (should-not (superchat-sandbox-wider-p 'read-only 'danger-full-access)))))

(ert-deftest m6-permissions/levels/command-is-offered-by-default ()
  "Since the cutover the chat command registry offers /permissions as loaded."
  (skip-when (getenv "SMOKE_REAL"))
  (smoke--with-fixture '((:text "unused"))
    (smoke--open-chat)
    (should superchat--permission-levels-enabled)
    (should (member "permissions" (superchat--commands-list)))
    (should (member "workspace" (superchat--commands-list)))))

(ert-deftest m6-permissions/levels/command-shows-and-selects ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (should (member "permissions" (superchat--commands-list)))
      (with-current-buffer view
        (should (string-match-p
                 "\\`Permissions: workspace-write (available: workspace-write, danger-full-access, read-only)\\'"
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
ROOT is the workspace root of a tool execution context with no Run, so an
escalation has no approval channel unless BODY binds a richer context.  The
fixture's HOME lives under the temporary directory, which is itself a
writable root, so BODY runs with its own temporary directory beside ROOT and
OUTSIDE is outside both."
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
  (and (stringp result)
       (string-prefix-p "[sandbox: file access denied under " result)))

(ert-deftest m6-permissions/targets/switch-off-leaves-the-edit-tools-alone ()
  "With levels off the three tools neither check nor add evidence lines."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled nil)
        (superchat-sandbox-default-mode 'read-only))
    (with-temp-buffer
      (insert "old")
      (should (equal (format "Replaced 1 occurrence in buffer %s. This tool does not save or evaluate the buffer." (buffer-name))
                     (superchat-tool-edit-buffer (buffer-name) "old" "new")))
      (should (equal (format "Buffer replaced: %s" (buffer-name))
                     (superchat-tool-replace-buffer (buffer-name) "x")))
      (should (equal (format "Appended text to buffer %s" (buffer-name))
                     (superchat-tool-append-to-buffer (buffer-name) "y")))
      (should (equal "xy" (buffer-string))))))

(ert-deftest m6-permissions/targets/workspace-write-edits-inside-and-denies-outside ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((inside (m6p--visit (expand-file-name "in.el" root) "old-in"))
             (out (m6p--visit (expand-file-name "out.el" outside) "old-out"))
             (result (superchat-tool-edit-buffer (buffer-name inside) "old-in" "new-in")))
        (should (string-prefix-p "Replaced 1 occurrence" result))
        (should (string-match-p "^Target: buffer in\\.el, visiting " result))
        (should (string-match-p "^Layer: in memory only" result))
        (should (string-match-p "^Checks: sandbox mode workspace-write" result))
        (should (equal "new-in" (with-current-buffer inside (buffer-string))))
        (dolist (call (list (lambda () (superchat-tool-edit-buffer (buffer-name out) "old-out" "x"))
                            (lambda () (superchat-tool-replace-buffer (buffer-name out) "x"))
                            (lambda () (superchat-tool-append-to-buffer (buffer-name out) "x"))))
          (let ((tick (with-current-buffer out (buffer-chars-modified-tick)))
                (denied (funcall call)))
            (should (m6p--denied-p denied))
            (should (string-match-p "\\[sandbox: escalation available" denied))
            (should (equal "old-out" (with-current-buffer out (buffer-string))))
            (should (= tick (with-current-buffer out (buffer-chars-modified-tick))))
            (should-not (with-current-buffer out (buffer-modified-p)))))))))

(ert-deftest m6-permissions/targets/an-indirect-buffer-is-judged-by-its-base ()
  "An indirect buffer has no file name of its own; its base decides."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((out (m6p--visit (expand-file-name "out.el" outside) "old-out"))
             (indirect (make-indirect-buffer out "m6p-indirect")))
        (unwind-protect
            (progn
              (should-not (buffer-file-name indirect))
              (should (m6p--denied-p
                       (superchat-tool-edit-buffer "m6p-indirect" "old-out" "x")))
              (should (equal "old-out" (with-current-buffer out (buffer-string))))
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

(ert-deftest m6-permissions/targets/escapes-are-resolved-before-containment ()
  "Dot-dot, an absolute path and a symlink pointing out are outside; the temp
directory and a new file under a real inside directory are inside."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let ((policy (superchat-sandbox-call-policy)))
        (make-symbolic-link outside (expand-file-name "link-out" root))
        (make-directory (expand-file-name "sub" root))
        (should (superchat-sandbox-path-writable-p (expand-file-name "sub/new.txt" root) policy))
        (should (superchat-sandbox-path-writable-p (expand-file-name "sub/deep/er/new.txt" root) policy))
        (should (superchat-sandbox-path-writable-p
                 (expand-file-name "m6p-tmp.txt" temporary-file-directory) policy))
        (should-not (superchat-sandbox-path-writable-p
                     (expand-file-name "sub/../../escape.txt" root) policy))
        (should-not (superchat-sandbox-path-writable-p (expand-file-name "x" outside) policy))
        (should-not (superchat-sandbox-path-writable-p
                     (expand-file-name "link-out/new.txt" root) policy))
        ;; A sibling whose name merely starts with the root's name is outside.
        (should-not (superchat-sandbox-path-writable-p
                     (concat (directory-file-name root) "-sibling/x") policy))
        (should-not (superchat-sandbox-path-writable-p "/ssh:host:/tmp/x" policy))))))

(ert-deftest m6-permissions/targets/a-dangling-link-is-judged-by-where-it-points ()
  "A link inside the workspace to a file that does not exist yet outside it
is an outside target: denied, and the outside file is never created."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let ((link (expand-file-name "link.txt" root))
            (escape (expand-file-name "not-yet.txt" outside))
            (inner (expand-file-name "inner-not-yet.txt" root)))
        (make-symbolic-link escape link)
        (should-not (file-exists-p link))
        (should (m6p--denied-p (superchat-tool-write-file link "escaped")))
        (should-not (file-exists-p escape))
        ;; A dangling link to a place inside the workspace is still inside.
        (make-symbolic-link inner (expand-file-name "inner-link.txt" root))
        (should (string-prefix-p
                 "Created file"
                 (superchat-tool-write-file (expand-file-name "inner-link.txt" root) "ok")))
        (should (file-exists-p inner))
        ;; A chain: link -> link -> outside.
        (make-symbolic-link link (expand-file-name "chain.txt" root))
        (should (m6p--denied-p
                 (superchat-tool-write-file (expand-file-name "chain.txt" root) "escaped")))
        (should-not (file-exists-p escape))))))

(ert-deftest m6-permissions/targets/the-data-directory-is-outside-even-inside-the-root ()
  "A workspace that contains Superchat's data directory does not make the
approval ledger writable; an escalation still can."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (m6p--with-workspace (root outside)
        (let* ((superchat-data-directory (expand-file-name "superchat-data/" root))
               (ledger (expand-file-name "ledger.el" superchat-data-directory))
               (superchat--tool-execution-context
                (list :workspace-root root :session-id session
                      :run-id (plist-get request :run-id)
                      :turn-id (plist-get request :turn-id))))
          (make-directory superchat-data-directory t)
          (should (m6p--denied-p (superchat-tool-write-file ledger "stamped")))
          (should-not (file-exists-p ledger))
          (let ((buffer (m6p--visit ledger "honest")))
            (with-current-buffer buffer (goto-char (point-max)) (insert " + forged"))
            (should (m6p--denied-p (superchat-tool-save-buffer (buffer-name buffer))))
            (should (equal "honest" (with-temp-buffer (insert-file-contents ledger) (buffer-string))))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          ;; Beside it, the workspace is as writable as ever.
          (should (string-prefix-p "Created file"
                                   (superchat-tool-write-file (expand-file-name "ok.txt" root) "ok")))
          (let ((superchat-approval-function (lambda (_public) t)))
            (delete-file ledger)
            (should (string-prefix-p
                     "Created file"
                     (superchat-tool-write-file ledger "with consent"
                                                "danger-full-access" "user asked")))))))))

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
        (should (m6p--denied-p (superchat-tool-append-to-buffer "m6p-never-created" "x")))
        (should-not (get-buffer "m6p-never-created"))
        (should (string-match-p
                 "under read-only mode"
                 (superchat-tool-write-file (expand-file-name "new.txt" root) "x")))
        (should-not (file-exists-p (expand-file-name "new.txt" root)))))))

(ert-deftest m6-permissions/targets/danger-full-access-writes-anywhere-without-asking ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((superchat-sandbox-default-mode 'danger-full-access)
             (asked 0)
             (superchat-approval-function (lambda (_r) (cl-incf asked) t))
             (file (expand-file-name "free.txt" outside)))
        (should (string-prefix-p "Created file" (superchat-tool-write-file file "free")))
        (should (equal "free" (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (= 0 asked))))))

;;;; Escalation

(ert-deftest m6-permissions/escalation/arguments-must-be-paired ()
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let ((file (expand-file-name "x.txt" outside)))
        (should (equal "Error: invalid escalation: sandbox_permissions requires a justification"
                       (superchat-tool-write-file file "x" "danger-full-access")))
        (should (equal "Error: invalid escalation: justification is only valid together with sandbox_permissions"
                       (superchat-tool-write-file file "x" nil "because")))
        (should (equal "Error: invalid justification: expected a non-empty sentence"
                       (superchat-tool-write-file file "x" "danger-full-access" "  ")))
        (should-not (file-exists-p file))))))

(ert-deftest m6-permissions/escalation/a-request-that-does-not-widen-asks-nobody ()
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (m6p--with-workspace (root outside)
        (let* ((asked 0)
               (superchat-approval-function (lambda (_r) (cl-incf asked) t))
               (superchat--tool-execution-context
                (list :workspace-root root :session-id session
                      :run-id (plist-get request :run-id)
                      :turn-id (plist-get request :turn-id)))
               (file (expand-file-name "x.txt" outside)))
          (should (equal "Error: sandbox escalation to \"workspace-write\" is not strictly wider than this call's current \"workspace-write\" mode"
                         (superchat-tool-write-file file "x" "workspace-write" "please")))
          (should (string-prefix-p
                   "Error: sandbox escalation to \"everything\" is not strictly wider"
                   (superchat-tool-write-file file "x" "everything" "please")))
          (should (= 0 asked))
          (should-not (file-exists-p file)))))))

(ert-deftest m6-permissions/escalation/four-outcomes ()
  "Only `allowed-once' writes, and only that one call runs wider."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (m6p--with-workspace (root outside)
        (let* ((superchat--tool-execution-context
                (list :workspace-root root :session-id session
                      :run-id (plist-get request :run-id)
                      :turn-id (plist-get request :turn-id)
                      :execution-id "m6p-escalate"))
               (file (expand-file-name "x.txt" outside))
               (reasons nil))
          (should (m6p--denied-p (superchat-tool-write-file file "x")))
          ;; rejected
          (let ((superchat-approval-function
                 (lambda (r) (push (plist-get r :reason) reasons) nil)))
            (should (equal "Error: the user rejected escalating this operation to \"danger-full-access\""
                           (superchat-tool-write-file file "x" "danger-full-access" "needs the config dir")))
            (should (equal "escalate sandbox to danger-full-access: needs the config dir"
                           (car reasons))))
          ;; unavailable
          (let ((superchat-approval-function nil)
                (superchat-authority-approval-function nil))
            (should (equal "Error: sandbox escalation to \"danger-full-access\" requires approval, but no approval channel is available"
                           (superchat-tool-write-file file "x" "danger-full-access" "why"))))
          ;; never
          (superchat-approval-set-policy session 'never)
          (let ((superchat-approval-function (lambda (_r) t)))
            (should (string-prefix-p "Error: the user rejected escalating"
                                     (superchat-tool-write-file file "x" "danger-full-access" "why"))))
          (superchat-approval-set-policy session 'ask)
          (should-not (file-exists-p file))
          ;; allowed once: this call writes, the next plain call is denied again.
          (let ((superchat-approval-function (lambda (_r) t)))
            (should (string-match-p
                     "Checks: sandbox mode danger-full-access"
                     (superchat-tool-write-file file "granted" "danger-full-access" "why"))))
          (should (equal "granted" (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (should (m6p--denied-p (superchat-tool-write-file
                                  (expand-file-name "y.txt" outside) "y"))))))))

(ert-deftest m6-permissions/escalation/feedback-names-the-sanctioned-retry ()
  "A sandbox denial is an error the model may escalate; a rejection is final."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled t))
    (cl-flet ((feedback (text)
                (superchat--llm-tool-error-feedback
                 (list :tool-results (list (cons "write_file" text))))))
      (let ((denial (concat (superchat-sandbox-denial-marker 'workspace-write) "\n"
                            (superchat-sandbox-escalation-hint "operation"))))
        (should (string-match-p "retry this exact call once with sandbox_permissions"
                                (feedback denial)))
        (should-not (string-match-p "do not retry it" (feedback denial))))
      (dolist (final '("Error: the user rejected escalating this operation to \"danger-full-access\""
                       "Error: approval for escalating to \"danger-full-access\" was cancelled"
                       "Error: sandbox escalation to \"danger-full-access\" requires approval, but no approval channel is available"
                       "Error: sandbox escalation to \"danger-full-access\" requires approval, but the call has no Run to route it through"))
        (should (string-match-p "do not retry it" (feedback final))))
      (dolist (correctable '("Error: invalid escalation: sandbox_permissions requires a justification"
                             "Error: sandbox escalation to \"workspace-write\" is not strictly wider than this call's current \"workspace-write\" mode"
                             ;; An ordinary error that merely says rejected.
                             "Error: the server rejected the request body"))
        (should (string-match-p "Correct the arguments" (feedback correctable)))))))

(ert-deftest m6-permissions/escalation/mixed-round-keeps-each-calls-instruction ()
  "One escalatable denial beside a rejected escalation and a plain error:
each failed call is told its own thing."
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((superchat--permission-levels-enabled t)
         (text (superchat--llm-tool-error-feedback
                (list :tool-results
                      (list (cons "write_file"
                                  (concat (superchat-sandbox-denial-marker 'workspace-write) "\n"
                                          (superchat-sandbox-escalation-hint "operation")))
                            (cons "edit_buffer"
                                  "Error: the user rejected escalating this operation to \"danger-full-access\"")
                            (cons "read_buffer" "Error: buffer nope is not live.")
                            (cons "list_buffers" "fine"))))))
    (should (string-match-p "^- write_file: the sandbox denied .* retry this exact call once" text))
    (should (string-match-p "^- edit_buffer: the user or a policy prevented this call; do not retry it" text))
    (should (string-match-p "^- read_buffer: correct the arguments" text))
    (should-not (string-match-p "list_buffers" text))))

(ert-deftest m6-permissions/escalation/classification-is-unchanged-with-levels-off ()
  "Until the cutover a literal marker in tool output and the word rejected
mean what they meant before."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled nil))
    (should-not (superchat--llm-tool-error-feedback
                 (list :tool-results
                       (list (cons "read_buffer" "[sandbox: this is literal note text]")))))
    (should-not (superchat--llm-tool-error-feedback
                 (list :tool-results
                       (list (cons "read_buffer"
                                   (concat (superchat-sandbox-denial-marker 'read-only) "\n"
                                           (superchat-sandbox-escalation-hint "operation")))))))
    (should (string-match-p
             "Correct the arguments of the failed call and retry it"
             (superchat--llm-tool-error-feedback
              (list :tool-results
                    (list (cons "write_file"
                                "Error: the user rejected escalating this operation to \"danger-full-access\""))))))))

(ert-deftest m6-permissions/escalation/the-write-uses-the-approved-arguments ()
  "What the answerer saw is what gets written, whatever the caller's own
string has become while the question was open."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (m6p--with-workspace (root outside)
        (let* ((superchat--tool-execution-context
                (list :workspace-root root :session-id session
                      :run-id (plist-get request :run-id)
                      :turn-id (plist-get request :turn-id)))
               (file (expand-file-name "x.txt" outside))
               (content (copy-sequence "approved"))
               (seen nil)
               (superchat-approval-function
                (lambda (public)
                  (setq seen (copy-sequence (nth 1 (plist-get public :args))))
                  (aset content 0 ?X)
                  t)))
          (superchat-tool-write-file file content "danger-full-access" "why")
          (should (equal "approved" seen))
          (should (equal "Xpproved" content))
          (should (equal "approved"
                         (with-temp-buffer (insert-file-contents file) (buffer-string)))))))))

(ert-deftest m6-permissions/escalation/preconditions-are-checked-again-after-approval ()
  "A file that becomes visited, or changes on disk, while the question is
open is seen before the write; the disk and the buffer keep their contents."
  (m6p--fixture '((:hang t))
    (m6p--with-live-run (session request)
      (m6p--with-workspace (root outside)
        (let* ((superchat--tool-execution-context
                (list :workspace-root root :session-id session
                      :run-id (plist-get request :run-id)
                      :turn-id (plist-get request :turn-id)))
               (file (expand-file-name "late.txt" outside))
               (visitor nil))
          (with-temp-file file (insert "disk"))
          (let* ((superchat-approval-function
                  (lambda (_public)
                    (setq visitor (find-file-noselect file))
                    (with-current-buffer visitor
                      (goto-char (point-max)) (insert " + unsaved user"))
                    t))
                 (result (superchat-tool-write-file file "replacement"
                                                    "danger-full-access" "why")))
            (should (string-prefix-p "Error:" result))
            (should (string-match-p "is visited by buffer late\\.txt" result))
            (should (equal "disk" (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (equal "disk + unsaved user"
                           (with-current-buffer visitor (buffer-string)))))
          ;; save_buffer: the file changes on disk while the question is open.
          (let* ((other (expand-file-name "saved.txt" outside))
                 (buffer (m6p--visit other "v1"))
                 (superchat-approval-function
                  (lambda (_public)
                    (with-temp-file other (insert "changed elsewhere"))
                    (set-file-times other (time-add (current-time) 5))
                    t)))
            (with-current-buffer buffer (goto-char (point-max)) (insert " + edit"))
            (let ((result (superchat-tool-save-buffer "saved.txt"
                                                      "danger-full-access" "why")))
              (should (string-match-p "changed on disk since buffer saved\\.txt read it" result)))
            (should (equal "changed elsewhere"
                           (with-temp-buffer (insert-file-contents other) (buffer-string))))))))))

;;;; save_buffer and write_file

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
        ;; Outside the workspace: denied, and the disk keeps its bytes.
        (let* ((file (expand-file-name "out.el" outside))
               (buffer (m6p--visit file "disk")))
          (with-current-buffer buffer (goto-char (point-max)) (insert " + memory"))
          (should (m6p--denied-p (superchat-tool-save-buffer "out.el")))
          (should (equal "disk" (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (should (with-current-buffer buffer (buffer-modified-p))))))))

(ert-deftest m6-permissions/write/never-forks-a-visited-file ()
  "A visited file is left to its buffer: nothing on disk, nothing in memory."
  (m6p--fixture '((:text "unused"))
    (m6p--with-workspace (root outside)
      (let* ((file (expand-file-name "config.el" root))
             (buffer (m6p--visit file "disk")))
        (with-current-buffer buffer (goto-char (point-max)) (insert " + unsaved A"))
        (let ((result (superchat-tool-write-file file "B")))
          (should (string-prefix-p "Error:" result))
          (should (string-match-p "visited by buffer config\\.el" result)))
        (should (equal "disk" (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (equal "disk + unsaved A" (with-current-buffer buffer (buffer-string))))
        ;; Relative paths resolve against the tools' directory; parents must exist.
        (should (string-prefix-p "Created file" (superchat-tool-write-file "fresh.txt" "one")))
        (should (string-prefix-p "Replaced file" (superchat-tool-write-file "fresh.txt" "two")))
        (should (equal "two" (with-temp-buffer
                               (insert-file-contents (expand-file-name "fresh.txt" root))
                               (buffer-string))))
        (should (string-match-p "does not exist; write_file does not create directories"
                                (superchat-tool-write-file "no/such/dir.txt" "x")))))))

(ert-deftest m6-permissions/tools/surface-gains-the-arguments-only-with-levels-on ()
  "Schemas are process-global: the switch, not a Session, decides them."
  (skip-when (getenv "SMOKE_REAL"))
  (cl-flet ((arg-names (tools name)
              (mapcar (lambda (arg) (plist-get arg :name))
                      (llm-tool-args (cl-find name tools :key #'llm-tool-name :test #'equal))))
            (reload (enabled)
              (let ((superchat--permission-levels-enabled enabled)
                    (superchat-llm-tool-names 'all)
                    (superchat-llm-tools-list nil))
                (superchat-llm-tools-reload)
                superchat-llm-tools-list)))
    (let ((off (reload nil)) (on (reload t)))
      (should (equal '("buffer_name" "old_string" "new_string") (arg-names off "edit_buffer")))
      (should-not (cl-find "save_buffer" off :key #'llm-tool-name :test #'equal))
      (should-not (cl-find "write_file" off :key #'llm-tool-name :test #'equal))
      (should (equal '("buffer_name" "old_string" "new_string" "sandbox_permissions" "justification")
                     (arg-names on "edit_buffer")))
      (should (equal '("path" "content" "sandbox_permissions" "justification")
                     (arg-names on "write_file")))
      (should (equal '("buffer_name" "sandbox_permissions" "justification")
                     (arg-names on "save_buffer"))))
    (setq superchat-llm-tools-list nil)))

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
         (let ((superchat--permission-levels-enabled nil))
           (superchat-plugin-unmount 'chat-collectors)
           (superchat-plugin-mount 'chat-collectors))
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
  "Under workspace-write a read runs unasked and full control asks per call."
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
    (should (eq 'builtin (superchat-sandbox-tool-class "write_file")))
    (should (eq 'builtin (superchat-sandbox-tool-class "ask_user")))
    (dolist (name '("eval_elisp" "shell-command" "eshell-command" "define_tool" "my_helper"))
      (should (eq 'full-control (superchat-sandbox-tool-class name))))
    (should (eq 'trusted (superchat-sandbox-tool-class "mcp:notes:search")))
    (should (eq 'outside (superchat-sandbox-tool-class "mcp:shell:run")))
    (should (eq 'outside (superchat-sandbox-tool-class "telega_send")))
    ;; Trusted says nothing about reading only: read-only denies it too.
    (should (eq 'deny (car (superchat-sandbox--level-rule "mcp:notes:search" 'read-only))))
    (should (eq 'allow (car (superchat-sandbox--level-rule "mcp:notes:search" 'workspace-write))))
    (should (eq 'ask (car (superchat-sandbox--level-rule "mcp:shell:run" 'workspace-write))))
    (should (eq 'allow (car (superchat-sandbox--level-rule "mcp:shell:run" 'danger-full-access))))))

(ert-deftest m6-permissions/executor/define-tool-answers-to-the-level-alone ()
  "With levels on the two in-body confirmations of agent-written tools are
silent, so `danger-full-access' really asks nothing."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled t)
        (superchat-agent-confirm-synthesized t)
        (prompted 0))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (cl-incf prompted) nil)))
      (should (superchat--syn-confirm "t" "d" '(defun superchat-syn--t () 1) nil))
      (should (= 0 prompted))
      (let ((superchat--permission-levels-enabled nil))
        (should-not (superchat--syn-confirm "t" "d" '(defun superchat-syn--t () 1) nil))
        (should (= 1 prompted))))))

(ert-deftest m6-permissions/context/the-request-states-the-current-policy ()
  "A hard fragment after everything else states the policy; a level change
shows in the next request; with levels off nothing is added."
  (m6p--pipeline (list '(:text "one") '(:text "two"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (m6p--settle (smoke--send view "first"))
      (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Current Superchat file policy: workspace-write" prompt))
        (should (string-match-p "Approval policy: ask" prompt)))
      (superchat-permission-set session 'danger-full-access)
      (m6p--settle (smoke--send view "second"))
      (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Current Superchat file policy: danger-full-access" prompt))
        (should (string-match-p "Approval prompts are disabled in this session" prompt))))))

(ert-deftest m6-permissions/context/nothing-is-added-with-levels-off ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled nil)
        (superchat-llm-tool-names superchat-llm-default-tool-names))
    (smoke--with-fixture '((:text "one"))
      (let ((view (smoke--open-chat)))
        (m6p--settle (smoke--send view "first"))
        (should-not (string-match-p "Superchat file policy"
                                    (plist-get (car smoke--requests) :prompt-text)))))))

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
  "A region Artifact in a buffer outside the workspace is an outside target;
the Workspace's own fallback buffer is scratch space."
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
            (superchat-workspace-add-region (point-min) (point-max)))
          (let ((result (with-current-buffer out
                          (superchat-tool-workspace-write "replaced"))))
            (should (m6p--denied-p result))
            (should (equal "region text\n" (with-current-buffer out (buffer-string))))))))))

;;;; Review of slice 3 ----------------------------------------------------------

(defun m6p--append-round (buffer-name &optional permissions justification)
  "A scripted provider step calling append_to_buffer on BUFFER-NAME."
  `(:tool-uses (("append_to_buffer" (buffer . ,buffer-name) (text . "AFTER")
                 ,@(when permissions
                     `((sandbox_permissions . ,permissions)
                       (justification . ,justification)))))))

(defmacro m6p--write-pipeline (script &rest body)
  "Like `m6p--pipeline', offering append_to_buffer with its escalation args."
  (declare (indent 1) (debug t))
  `(m6p--pipeline ,script
     (let ((superchat-llm-tool-names
            (append superchat-llm-default-tool-names '("append_to_buffer"))))
       ,@body)))

(ert-deftest m6-permissions/review/a-late-grant-inside-a-tool-body-writes-nothing ()
  "An escalation is asked from inside the tool body, after the executor's
liveness gate.  The Run is cancelled while that question is open and the
answer still allows: the decision is recorded and the buffer is untouched."
  (let ((target (generate-new-buffer "m6p-late-grant")))
    (unwind-protect
        (m6p--write-pipeline
            (list (m6p--append-round (buffer-name target) "workspace-write" "needs it")
                  '(:text "must not be requested"))
          (with-current-buffer target (insert "before"))
          (let* ((view (smoke--open-chat))
                 (session (buffer-local-value 'superchat--session-id view))
                 (superchat-approval-function
                  (lambda (public)
                    (superchat-runtime-cancel (plist-get public :run-id) "[m6p cancel]")
                    t)))
            (superchat-permission-set session 'read-only)
            (m6p--settle (smoke--send view "go"))
            (should (equal "before" (with-current-buffer target (buffer-string))))
            (should (= 1 (m6p--count session "approval.requested")))
            (should (= 1 (m6p--count session "approval.decided")))
            (should (= 1 (length smoke--requests)))))
      (kill-buffer target))))

(ert-deftest m6-permissions/review/an-escalation-hears-the-runs-own-answerers ()
  "A rejecting answerer registered in the Scope the Run executes in decides
an escalation, whatever the global answerer of last resort would say."
  (let ((target (generate-new-buffer "m6p-scoped-answerer")))
    (unwind-protect
        (m6p--write-pipeline
            (list (m6p--append-round (buffer-name target) "workspace-write" "needs it")
                  '(:text "done"))
          (with-current-buffer target (insert "before"))
          (let* ((global-calls 0)
                 (local-calls 0)
                 (superchat-approval-function
                  (lambda (_public) (cl-incf global-calls) t))
                 (scope (superchat-scope-create "m6p-run-scope" superchat-scope-global))
                 (view (smoke--open-chat))
                 (session (buffer-local-value 'superchat--session-id view)))
            (unwind-protect
                (progn
                  (superchat-on scope 'approval/request 'm6p-local-reject
                                (lambda (_request _next) (cl-incf local-calls) 'rejected))
                  (superchat-permission-set session 'read-only)
                  (let* ((superchat-execution-scope scope)
                         (run (plist-get
                               (superchat-service-call
                                nil 'driver 'submit
                                (list :surface 'external :session-id session
                                      :tools '("append_to_buffer")
                                      :attendance 'attended :content "go"
                                      :context (list :host-buffer view)
                                      :runtime-extras '(:record-conversation nil)
                                      :sink (superchat-sink-discard)))
                               :run-id)))
                    (m6p--settle run))
                  (should (= 1 local-calls))
                  (should (= 0 global-calls))
                  (should (equal "before" (with-current-buffer target (buffer-string)))))
              (ignore-errors (superchat-scope-dispose scope)))))
      (kill-buffer target))))

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
             (fallback (superchat-workspace--fallback-buffer-name workspace-id))
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
        (should (string-match-p "Superchat file policy: workspace-write" first))
        (should (string-match-p "Superchat file policy: read-only" second))
        (should-not (string-match-p "Superchat file policy: workspace-write" second))
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
      (should (eq 'workspace-write (superchat-sandbox-override child)))
      (should (eq 'never (superchat-approval-override child)))
      (let ((superchat-sandbox-default-mode 'danger-full-access)
            (superchat-approval-default-policy 'never))
        (should (eq 'workspace-write (superchat-sandbox-effective-mode child)))))))

;;;; Slice 4, step 1a: the whole new chain ------------------------------------

(defun m6p--submit-tools (view session tools text)
  "Submit TEXT from VIEW with the explicit tool list TOOLS; settle the Run."
  (m6p--settle
   (plist-get (superchat-service-call
               nil 'driver 'submit
               (list :surface 'external :session-id session :tools tools
                     :attendance 'attended :content text
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
            (superchat-authority-approval-function nil)
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

(ert-deftest m6-permissions/chain/details-return-to-the-same-question ()
  "`?' shows the arguments and decides nothing."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((answers (list '(?? "details") '(?? "details") '(?y "yes")))
        (shown 0))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'superchat-approval--show-details)
               (lambda (_request) (cl-incf shown))))
      (should (superchat-approval-confirm '(:tool-name "eval_elisp" :args ("(+ 1 2)"))))
      (should (= 2 shown))
      (should-not answers))))

(ert-deftest m6-permissions/chain/the-new-path-owes-nothing-to-the-decision-machine ()
  "Under levels the executor calls no effect resolver, no base decision, no
approval hash and no argument rewriter; a resolver that signals changes
nothing; the Tape rows carry none of the machine's keys."
  (m6p--pipeline (list m6p--eval-round '(:text "done"))
    (let* ((calls nil)
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (rewrite (superchat-on nil 'tool-args-rewrite 'm6p-rewrite
                                  (lambda (_tool _args) (list :args '("(setq m6p--effect 99)"))))))
      (superchat-permission-set session 'danger-full-access)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-authority-resolve-effects)
                     (lambda (&rest _) (push 'resolve calls) (error "old resolver ran")))
                    ((symbol-function 'superchat-authority-decide)
                     (lambda (&rest _) (push 'decide calls) (error "old decide ran")))
                    ((symbol-function 'superchat-authority-approval-hash)
                     (lambda (&rest _) (push 'hash calls) "hash")))
            (m6p--settle (smoke--send view "go")))
        (funcall rewrite))
      (should-not calls)
      (should (= 1 m6p--effect))
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
tool, so workspace-write asks about it."
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
  ;; With levels off the declaration is still required.
  (let ((superchat--permission-levels-enabled nil))
    (should-error (superchat-register-tool "m6p_needs_effects" :function #'ignore
                                           :description "d"))))

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
             (fallback (superchat-workspace--fallback-buffer-name workspace-id))
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

(ert-deftest m6-permissions/telega/nothing-changes-with-levels-off ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled nil))
    (smoke--with-fixture '((:text "digest"))
      (m6p--with-telega
        (let ((handle (superchat-telega-digest "summarize")))
          (m6p--settle (plist-get handle :run-id))
          (should-not superchat-cowork-telega--request-scopes)
          (should (eq 'completed
                      (plist-get (superchat-runtime-status (plist-get handle :run-id))
                                 :status))))))))

(provide 'm6-permissions-regression)
;;; m6-permissions-regression.el ends here
