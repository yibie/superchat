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

(ert-deftest m6-permissions/levels/command-is-absent-until-the-cutover ()
  "With the switch off, the chat command registry does not offer /permissions."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat--permission-levels-enabled nil))
    (smoke--with-fixture '((:text "unused"))
      (smoke--open-chat)
      (should-not (member "permissions" (superchat--commands-list)))
      (should (member "workspace" (superchat--commands-list))))))

(ert-deftest m6-permissions/levels/command-shows-and-selects ()
  (m6p--fixture '((:text "unused"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      ;; Plugins mount when the package loads, before this fixture turns the
      ;; switch on, so remount the registry to observe the gate both ways.
      (should-not (member "permissions" (superchat--commands-list)))
      (superchat-plugin-unmount 'chat-commands)
      (superchat-plugin-mount 'chat-commands)
      (unwind-protect
          (should (member "permissions" (superchat--commands-list)))
        (let ((superchat--permission-levels-enabled nil))
          (superchat-plugin-unmount 'chat-commands)
          (superchat-plugin-mount 'chat-commands)))
      (should-not (member "permissions" (superchat--commands-list)))
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

(provide 'm6-permissions-regression)
;;; m6-permissions-regression.el ends here
