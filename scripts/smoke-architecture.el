;;; smoke-architecture.el --- Architecture repair contracts -*- lexical-binding: t; -*-
;; Loaded by smoke.el after its fixtures; uses the same isolated, fake network.
(defun smoke-arch--assert (value message)
  (unless value (error "Architecture contract: %s" message)))

(defun smoke-arch--condition (thunk)
  (condition-case condition (progn (funcall thunk) nil)
    ((error quit) condition)))

(defun smoke-arch--row-event-type (options)
  "Return the event type of a `superchat-db-tape-append' OPTIONS plist.
The row's `:meta' is the JSON text the Tape serialized, because Tape owns the
payload shape the META column must satisfy and the storage layer only stores
the bytes it is given; decode it with the same codec the read path uses rather
than reading the parameter as Lisp.  Returns nil when OPTIONS carries no meta."
  (plist-get (superchat-tape--decode-meta (plist-get options :meta)) :event_type))

(defun smoke-arch--envelope (&optional surface sink)
  (list :surface (or surface 'external) :content "architecture contract"
        :session-id "architecture" :tools 'none
        :context (list :host-buffer (current-buffer))
        :runtime-extras '(:record-conversation nil)
        :sink (or sink (superchat-sink-callback :final #'ignore :error #'ignore))))

(defun smoke-arch--transport-basic ()
  "transport dependency: unmount blocks submission; remount reactivates"
  (smoke--with-fixture '((:text "restored"))
    (superchat-plugin-unmount 'transport-llm)
    (unwind-protect
        (progn
          (smoke-arch--assert (eq (superchat-plugin-state 'driver) 'pending) "driver must be pending")
          (smoke-arch--assert
           (smoke-arch--condition (lambda () (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))
           "unmounted driver must reject submit")
          (smoke-arch--assert
           (smoke-arch--condition (lambda () (superchat-runtime--generate "must not bypass" #'ignore nil)))
           "internal generate also fails without a provider")
          (smoke-arch--assert (null smoke--requests) "no fallback request")
          (superchat-plugin-mount 'transport-llm)
          (smoke-arch--assert (eq (superchat-plugin-state 'driver) 'active) "driver must reactivate")
          (let ((handle (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))
            (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p (plist-get handle :run-id)))) "restored Run completes"))
          (smoke-arch--assert (= 1 (length smoke--requests)) "only restored transport called"))
      (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm)))
    '(("contract" . passed))))

(defun smoke-arch--blocking-basic ()
  "blocking driver uses Envelope admission, collection and assembly"
  (smoke--with-fixture '((:text "blocking answer"))
    (superchat-on nil 'pre-step 'smoke-arch-gate
                  (lambda (_proposal _next) '(:reject "gate")))
    (unwind-protect
        (smoke-arch--assert
         (smoke-arch--condition
          (lambda () (superchat-service-call nil 'driver 'submit-blocking
                                             (smoke-arch--envelope 'org-babel) 0.1)))
         "pre-step rejection fails blocking submission promptly")
      (superchat-off 'pre-step 'smoke-arch-gate))
    (smoke-arch--assert (null smoke--requests) "admission must precede transport")
    (superchat-context-register 'smoke-arch-context
                        (lambda (&rest _) '((:owner smoke :id "architecture-marker" :slot context :text "ARCH_CONTEXT_MARKER" :priority 0 :budget-class hard)))
                        :owner 'smoke :required t)
    (unwind-protect
        (progn
          (smoke-arch--assert
           (equal "blocking answer" (superchat-service-call nil 'driver 'submit-blocking (smoke-arch--envelope 'org-babel) 2)) "blocking response")
          (smoke-arch--assert (string-match-p "ARCH_CONTEXT_MARKER" (plist-get (car smoke--requests) :prompt-text)) "collector reaches model")
          (smoke-arch--assert (member "prompt.assembled" (smoke--types "architecture")) "assembly receipt"))
      (superchat-context-unregister 'smoke-arch-context))
    '(("contract" . passed))))

(defun smoke-arch--tool-basic ()
  "same-Scope duplicate tools fail without altering the active definition"
  (let* ((scope-a (superchat-scope-create "architecture-A" superchat-scope-global superchat-scope-global))
         (scope-b (superchat-scope-create "architecture-B" superchat-scope-global superchat-scope-global))
         (name "architecture-tool"))
    (unwind-protect
        (progn
          (let ((superchat-plugin-current-scope scope-a))
            (superchat-register-tool name :function (lambda () "A") :description "A"))
          (let ((superchat-plugin-current-scope scope-b))
            (smoke-arch--assert
             (smoke-arch--condition (lambda () (superchat-register-tool name :function (lambda () "B") :description "B")))
             "same-Scope duplicate must fail"))
          (superchat-scope-dispose scope-b)
          (let ((installation (superchat-tools--visible-installation name)))
            (smoke-arch--assert (equal "A" (plist-get installation :description)) "failed registration preserves A")
            (smoke-arch--assert (equal "A" (funcall (plist-get installation :function)))
                                "failed registration leaves A's body the callable one"))
          (superchat-scope-dispose scope-a)
          (smoke-arch--assert (not (superchat-tools--visible-installation name)) "own undo removes tool"))
      (superchat-scope-dispose scope-b)
      (superchat-scope-dispose scope-a))
    '(("contract" . passed))))

(defun smoke-case-R4 ()
  "required collect execution and validation failures terminate; nil is no contribution"
  (let ((scope (superchat-scope-create "R4-context" superchat-scope-global)))
    (unwind-protect
        (let ((superchat-execution-scope scope))
          (dolist (function (list (lambda (&rest _) '(:invalid t))
                                 (lambda (&rest _) '((:owner smoke :id "invalid")))
                                 (lambda (&rest _) (error "collector failed"))))
            (superchat-context-register 'required function :owner 'smoke :required t :scope scope)
            (smoke-arch--assert
             (eq 'superchat-context-required-failed
                 (car (smoke-arch--condition
                       (lambda () (superchat-context-collect nil nil nil)))))
             "required execution or schema failure terminates")
            (superchat-context-unregister 'required scope)
            (superchat-context-register 'optional function :owner 'smoke :scope scope)
            (smoke-arch--assert
             (not (assq 'invalid (superchat-context-collect nil nil nil)))
             "optional invalid contribution omitted")
            (superchat-context-unregister 'optional scope))
          (superchat-context-register 'required (lambda (&rest _) nil)
                                      :owner 'smoke :required t :scope scope)
          (superchat-context-collect nil nil nil))
      (superchat-scope-dispose scope)))
  (smoke--with-fixture nil
    (dolist (function (list (lambda (&rest _) '(:invalid t))
                           (lambda (&rest _) (error "required context execution"))))
      (superchat-context-register 'smoke-required-context function :owner 'smoke :required t)
      (unwind-protect
          (smoke-arch--assert
           (eq 'superchat-context-required-failed
               (car (smoke-arch--condition (lambda () (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))))
           "required context terminates actual pipeline")
        (superchat-context-unregister 'smoke-required-context)))
    (smoke-arch--assert (null smoke--requests) "required context failure precedes model effects"))
  '(("contract" . passed)))

(defun smoke-case-R8 ()
  "execution Scope retirement stops only its borrowed execution ancestry"
  ;; Keep this as one compact real-driver contract.  Root's focused matrix
  ;; covers the wider terminal, external-Effect, and Tape-fault permutations.
  ;; Here we protect the shared latch, pre-provider gates, reentrant terminal
  ;; sweep, and one canonical FIFO behavior that future Preset activation uses.
  (smoke--with-fixture
      '((:hang t) (:hang t) (:hang t) (:text "R8 sibling survives"))
    (let* ((a (superchat-scope-create "smoke-r8-preset" superchat-scope-global))
           (child (superchat-scope-create "smoke-r8-child" a))
           ;; Equal display names are not identity: this is a separate sibling.
           (b (superchat-scope-create "smoke-r8-preset" superchat-scope-global))
           (host (generate-new-buffer " *smoke-r8-host*"))
           (terminal-calls 0) a-rejected b-reentrant
           a-handle child-handle b-old)
      (cl-labels
          ((envelope (session text &optional sink)
             (list :surface 'external :content text :session-id session :tools 'none
                   :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil)
                   :sink (or sink (superchat-sink-discard)))))
        (unwind-protect
            (let ((a-sink
                   (superchat-sink-create
                    :error
                    (lambda (&rest _)
                      (cl-incf terminal-calls)
                      ;; A nested nil call inherits A's now-retired Scope and
                      ;; must reject before a new Run/prompt/provider effect.
                      (setq a-rejected
                            (smoke-arch--condition
                             (lambda ()
                               (superchat-service-call
                                nil 'driver 'submit
                                (envelope "smoke-r8-A-rejected" "R8 rejected")))))
                      ;; An explicit same-named sibling remains eligible while
                      ;; the old Scope's cancellation callback is on the stack.
                      (setq b-reentrant
                            (superchat-service-call
                             b 'driver 'submit
                             (envelope "smoke-r8-B-reentrant" "R8 B reentrant")))))))
              (setq a-handle
                    (superchat-service-call
                     a 'driver 'submit
                     (envelope "smoke-r8-A" "R8 A" a-sink))
                    child-handle
                    (superchat-service-call
                     child 'driver 'submit
                     (envelope "smoke-r8-child" "R8 child"))
                    b-old
                    (superchat-service-call
                     b 'driver 'submit
                     (envelope "smoke-r8-B-old" "R8 B old")))
              (smoke-arch--assert (= 3 (length smoke--requests))
                                  "R8 initial independent Runs entered their provider")
              (smoke-arch--assert
               (eq a (superchat-service-call nil 'driver 'retire-scope a "R8"))
               "R8 driver exposes Runtime scope retirement")
              (smoke-arch--assert
               (and (superchat-execution-scope-retired-p a)
                    (superchat-execution-scope-retired-p child)
                    (not (superchat-execution-scope-retired-p b))
                    (eq 'cancelled
                        (plist-get (superchat-runtime-status
                                    (plist-get a-handle :run-id)) :status))
                    (eq 'cancelled
                        (plist-get (superchat-runtime-status
                                    (plist-get child-handle :run-id)) :status))
                    (eq 'running-model
                        (plist-get (superchat-runtime-status
                                    (plist-get b-old :run-id)) :status)))
               "R8 ancestor retirement stops only A and its descendant")
              (smoke-arch--assert
               (and (eq 'error (car a-rejected))
                    (plist-get b-reentrant :run-id)
                    (= terminal-calls 1)
                    (= 1 (cl-count "run.cancelled" (smoke--types "smoke-r8-A")
                                   :test #'equal))
                    (= 1 (cl-count "run.cancelled" (smoke--types "smoke-r8-child")
                                   :test #'equal)))
               "R8 terminal reentry cannot revive A but can submit sibling B")
              (smoke-arch--assert
               (smoke--wait
                (lambda ()
                  (smoke--run-done-p (plist-get b-reentrant :run-id))))
               "R8 reentrant sibling Run completes")
              (smoke-arch--assert
               (= 4 (length smoke--requests))
               "R8 retired Scope never enters another provider request")
              (superchat-runtime-cancel (plist-get b-old :run-id) "R8 cleanup")
              (smoke-arch--assert (buffer-live-p host)
                                  "R8 retirement does not close borrowed host"))
          (dolist (handle (list a-handle child-handle b-old b-reentrant))
            (when-let* ((run-id (plist-get handle :run-id)))
              (ignore-errors (superchat-runtime-cancel run-id "R8 cleanup"))))
          (superchat-scope-dispose a)
          (superchat-scope-dispose b)))))
  ;; A cancellation callback may itself submit through a still-open sibling
  ;; Scope.  That sibling is allowed, but it may not attach the new child to
  ;; another old Run in the retired ancestry which the sweep has not reached
  ;; yet.  This catches the parent admission window without treating
  ;; `running-model' as a new parent eligibility requirement.
  (smoke--with-fixture '((:hang t) (:hang t) (:hang t))
    (let* ((a (superchat-scope-create "smoke-r8-parent-A" superchat-scope-global))
           (b (superchat-scope-create "smoke-r8-parent-B" superchat-scope-global))
           (host (generate-new-buffer " *smoke-r8-parent*"))
           (armed nil) (entered nil) (rejected nil) handles child)
      (unwind-protect
          (cl-labels
              ((sink ()
                 (superchat-sink-create
                  :error
                  (lambda (status _)
                    (when armed
                      (setq armed nil entered t)
                      (let ((parent
                             (cl-find-if
                              (lambda (handle)
                                (and
                                 (not (equal (plist-get status :run-id)
                                             (plist-get handle :run-id)))
                                 (not (superchat-runtime--terminal-p
                                       (plist-get
                                        (superchat-runtime-status
                                         (plist-get handle :run-id)) :status)))))
                              handles)))
                        (when parent
                          (setq rejected
                                (smoke-arch--condition
                                 (lambda ()
                                   (setq child
                                         (superchat-service-call
                                          b 'driver 'submit
                                          (list :surface 'external
                                                :content "R8 parent child"
                                                :session-id "smoke-r8-parent-child"
                                                :tools 'none
                                                :context (list :host-buffer host)
                                                :runtime-extras
                                                '(:record-conversation nil)
                                                :sink (superchat-sink-discard)
                                                :parent-run-id
                                                (plist-get parent :run-id))))))))))))))
            (dotimes (index 2)
              (push
               (superchat-service-call
                a 'driver 'submit
                (list :surface 'external
                      :content (format "R8 parent %s" index)
                      :session-id (format "smoke-r8-parent-%s" index)
                      :tools 'none :context (list :host-buffer host)
                      :runtime-extras '(:record-conversation nil)
                      :sink (sink)))
               handles))
            (setq armed t)
            ;; The two preexisting hanging provider calls are the complete
            ;; external history when parent admission correctly fails.
            (superchat-service-call nil 'driver 'retire-scope a "R8 parent")
            (smoke-arch--assert entered
                                "R8 cancellation callback reaches parent admission")
            (smoke-arch--assert rejected
                                "R8 retired parent child admission rejects")
            (smoke-arch--assert (null child)
                                "R8 retired parent produces no child handle")
            (smoke-arch--assert (= 2 (length smoke--requests))
                                "R8 retired parent starts no child provider")
            (smoke-arch--assert
             (not (member "run.started" (smoke--types "smoke-r8-parent-child")))
             "R8 retired parent writes no child Run fact"))
        (superchat-scope-dispose a)
        (superchat-scope-dispose b))))
  ;; A collector may retire the Scope it is running under.  That happens
  ;; before Runtime publication and therefore must stop the remainder of the
  ;; pipeline, including prompt.assembled.
  (smoke--with-fixture '((:text "R8 stage must not reach transport"))
    (let* ((scope (superchat-scope-create "smoke-r8-stage" superchat-scope-global))
           (host (generate-new-buffer " *smoke-r8-stage*")))
      (unwind-protect
          (progn
            (superchat-context-register 'smoke-r8-retire-stage
             (lambda (&rest _)
               (superchat-runtime-retire-scope scope "R8 stage")
               '((:owner smoke-r8 :id "retired" :slot context :text "never"
                                :priority 0 :budget-class hard)))
             :owner 'smoke-r8 :scope scope)
            (let ((condition
                   (smoke-arch--condition
                    (lambda ()
                      (superchat-service-call
                       scope 'driver 'submit
                       (list :surface 'external :content "R8 stage"
                             :session-id "smoke-r8-stage" :tools 'none
                             :context (list :host-buffer host)
                             :runtime-extras '(:record-conversation nil)
                             :sink (superchat-sink-discard)))))))
              (smoke-arch--assert
               (and (eq 'error (car condition))
                    (superchat-execution-scope-retired-p scope)
                    (null smoke--requests)
                    (not (member "prompt.assembled" (smoke--types "smoke-r8-stage"))))
               "R8 collector Scope retirement blocks the rest of preparation")))
        (superchat-scope-dispose scope))))
  ;; The existing FIFO retains its private borrowed Scope.  A retired head is
  ;; recorded/dropped before a same-Session sibling can dispatch, and a fresh
  ;; retired enqueue has no queue fact at all.
  (smoke--with-fixture '((:text "R8 FIFO sibling"))
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-runtime--queue-sequence 0))
      (let* ((a (superchat-scope-create "smoke-r8-queue-A" superchat-scope-global))
             (b (superchat-scope-create "smoke-r8-queue-B" superchat-scope-global))
             (host (generate-new-buffer " *smoke-r8-queue*"))
             (session "smoke-r8-queue") a-id)
        (cl-labels
            ((envelope (text)
               (list :surface 'external :content text :session-id session :tools 'none
                     :context (list :host-buffer host)
                     :runtime-extras '(:record-conversation nil)
                     :sink (superchat-sink-discard))))
          (unwind-protect
              (progn
                (setq a-id
                      (superchat-service-call
                       a 'driver 'enqueue-follow-up session (envelope "R8_FIFO_A")))
                (superchat-service-call
                 b 'driver 'enqueue-follow-up session (envelope "R8_FIFO_B"))
                (superchat-service-call nil 'driver 'retire-scope a "R8 FIFO")
                (smoke-arch--assert
                 (smoke--wait
                  (lambda ()
                    (and (zerop (superchat-runtime-pending-follow-ups session))
                         (null (superchat-runtime-active-runs)))))
                 "R8 FIFO drops retired head then drains sibling")
                (let ((drop
                       (cl-find-if
                        (lambda (event)
                          (and (equal "queue.dropped" (plist-get event :type))
                               (equal a-id
                                      (plist-get (plist-get event :payload) :queue-id))))
                        (superchat-tape-replay-events session))))
                  (smoke-arch--assert
                   (and (equal "scope-retired" (plist-get (plist-get drop :payload) :reason))
                        (= 1 (length smoke--requests))
                        (string-match-p "R8_FIFO_B"
                                        (plist-get (car smoke--requests) :prompt-text)))
                   "R8 FIFO retains ordering without retargeting retired input"))
                (let ((before (length (smoke--types session)))
                      (result
                       (superchat-service-call
                        a 'driver 'enqueue-follow-up session (envelope "R8_FIFO_REJECT"))))
                  (smoke-arch--assert
                   (and (equal '(:accepted nil :reason scope-retired) result)
                        (= before (length (smoke--types session)))
                        (zerop (superchat-runtime-pending-follow-ups session)))
                   "R8 pre-retired enqueue creates no queue fact")))
            (superchat-scope-dispose a)
            (superchat-scope-dispose b))))))
  ;; A required queue.queued append can itself retire the Scope.  It may leave
  ;; an auditable queued/drop pair, but the caller is never told it was accepted
  ;; and the dispatcher must not enter a provider for that item.
  (smoke--with-fixture nil
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-runtime--queue-sequence 0))
      (let* ((scope (superchat-scope-create "smoke-r8-queued" superchat-scope-global))
             (host (generate-new-buffer " *smoke-r8-queued*"))
             (session "smoke-r8-queued")
             (queue-event (symbol-function 'superchat-runtime--queue-event)))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat-runtime--queue-event)
                       (lambda (event-session type payload summary)
                         (let ((record
                                (funcall queue-event event-session type payload summary)))
                           (when (equal type "queue.queued")
                             (superchat-runtime-retire-scope scope "R8 queue append"))
                           record))))
              (let ((result
                     (superchat-service-call
                      scope 'driver 'enqueue-follow-up session
                      (list :surface 'external :content "R8 queued reentry"
                            :session-id session :tools 'none
                            :context (list :host-buffer host)
                            :runtime-extras '(:record-conversation nil)
                            :sink (superchat-sink-discard)))))
                (smoke-arch--assert
                 (and (equal '(:accepted nil :reason scope-retired) result)
                      (smoke--wait
                       (lambda ()
                         (zerop (superchat-runtime-pending-follow-ups session))))
                      (member "queue.queued" (smoke--types session))
                      (member "queue.dropped" (smoke--types session))
                      (null smoke--requests))
                 "R8 queued reentry is nonaccepted and never reaches provider")))
          (superchat-scope-dispose scope)))))
  ;; Generic Scope disposal remains borrowed-Scope cleanup, never retirement.
  (let ((ordinary (superchat-scope-create "smoke-r8-ordinary" superchat-scope-global)))
    (superchat-scope-dispose ordinary)
    (smoke-arch--assert
     (not (superchat-execution-scope-retired-p ordinary))
     "R8 ordinary Scope disposal remains non-retirement"))
  ;; An unwrapped internal transport entry has no Run predicate to consult, so
  ;; it must still reject a retired dynamic execution Scope before its Event or
  ;; provider boundary.
  (smoke--with-fixture '((:text "R8 direct must not reach transport"))
    (let ((scope (superchat-scope-create "smoke-r8-direct" superchat-scope-global)))
      (unwind-protect
          (progn
            (superchat-runtime-retire-scope scope "R8 direct")
            (let ((superchat-execution-scope scope))
              (smoke-arch--assert
               (and (eq 'error
                        (car (smoke-arch--condition
                              (lambda ()
                                (superchat-runtime--generate "R8 direct" #'ignore nil))))))
                    (null smoke--requests))
               "R8 direct generate rejects retired Scope without a Run predicate")))
        (superchat-scope-dispose scope)))
  '(("contract" . passed)))

(defun smoke-arch--blocking-boundaries ()
  ;; Test the real terminal Sink seam, including timer-driven callbacks.
  (dolist (slot '(final error))
    (dolist (symbol '(error quit))
      (smoke--with-fixture (if (eq slot 'final) '((:text "answer")) '((:error (error . "provider failure"))))
        (let* ((expected (list symbol "exact consumer condition" slot))
               (calls 0)
               (callback (lambda (&rest _) (cl-incf calls) (signal symbol (cdr expected))))
               (sink (superchat-sink-create :final (and (eq slot 'final) callback)
                                           :error (and (eq slot 'error) callback)))
               (condition (smoke-arch--condition
                           (lambda () (superchat-service-call nil 'driver 'submit-blocking
                                                              (smoke-arch--envelope 'org-babel sink) 2)))))
          (smoke-arch--assert (equal expected condition) "exact terminal error/quit condition")
          (smoke-arch--assert (= calls 1) "one terminal callback")
          (smoke-arch--assert (member (if (eq slot 'final) "run.completed" "run.failed") (smoke--types "architecture")) "terminal condition is not a timeout")
          (smoke-arch--assert (null (superchat-runtime-active-runs)) "sealed before condition returns")))))
  (smoke--with-fixture '((:hang t))
    (let ((condition
           (cl-letf (((symbol-function 'accept-process-output)
                      (lambda (&rest _) (signal 'quit '("blocking caller C-g")))))
             (smoke-arch--condition
              (lambda () (superchat-service-call nil 'driver 'submit-blocking (smoke-arch--envelope 'org-babel) 2))))))
      (smoke-arch--assert (equal condition '(quit "blocking caller C-g")) "wait preserves caller quit condition")
      (smoke-arch--assert (null (superchat-runtime-active-runs)) "caller quit cancels its accepted hanging Run")
      (smoke-arch--assert (member "run.cancelled" (smoke--types "architecture")) "caller quit cancellation is durable")))
  (smoke--with-fixture '((:hang t))
    (let ((condition (smoke-arch--condition
                      (lambda () (superchat-service-call nil 'driver 'submit-blocking (smoke-arch--envelope 'magit) 0.1)))))
      (smoke-arch--assert (string-match-p "timed out" (error-message-string condition)) "timeout reported")
      (smoke-arch--assert (null (superchat-runtime-active-runs)) "timeout cancels Run")
      (smoke-arch--assert (member "run.cancelled" (smoke--types "architecture")) "timeout cancellation is durable")))
  (smoke--with-fixture '((:text "never delivered" :delay 0.05))
    (let* ((live t) orphaned
           (sink (superchat-sink-create :live-p (lambda () live)
                                       :orphaned (lambda (_run reason) (setq orphaned reason))))
           (start (float-time))
           (timer (run-at-time 0.01 nil (lambda () (setq live nil))))
           (condition (unwind-protect
                          (smoke-arch--condition
                           (lambda () (superchat-service-call nil 'driver 'submit-blocking (smoke-arch--envelope 'org-babel sink) 3)))
                        (cancel-timer timer))))
      (smoke-arch--assert (eq orphaned 'sink-lost) "orphaned reaches exact inner Sink")
      (smoke-arch--assert (string-match-p "cancelled.*Sink lost" (error-message-string condition)) "orphaned reports cancellation, not timeout")
      (smoke-arch--assert (< (- (float-time) start) 1.5) "orphaned settles promptly")
      (smoke-arch--assert (null (superchat-runtime-active-runs)) "orphaned Run sealed")))
  (smoke--with-fixture '((:hang t))
    (let* ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (active (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))
      (smoke-arch--assert
       (eq 'superchat-pipeline-not-submitted
           (car (smoke-arch--condition
                 (lambda () (superchat-service-call nil 'driver 'submit-blocking
                                                    (smoke-arch--envelope 'org-babel) 2)))))
       "busy Session queues blocking submission without acceptance")
      (smoke-arch--assert (= 1 (superchat-runtime-pending-follow-ups "architecture"))
                          "Runtime actually enqueued")
      (let ((queued (plist-get (car (gethash "architecture" superchat-runtime--session-queues))
                               :request)))
        (smoke-arch--assert (not (superchat-sink-live-p (plist-get queued :sink)))
                            "returned blocking call's capture Sink is dead"))
      (superchat-runtime-cancel (plist-get active :run-id))
      (superchat-runtime--dispatch-next "architecture")
      (smoke-arch--assert (zerop (superchat-runtime-pending-follow-ups "architecture"))
                          "FIFO drops dead Sink")
      (let ((event (cl-find "queue.dropped" (superchat-tape-replay-events "architecture")
                           :key (lambda (entry) (plist-get entry :type)) :test #'equal)))
        (smoke-arch--assert (equal "sink-dead" (plist-get (plist-get event :payload) :reason))
                            "FIFO retains exact dead-Sink drop reason"))
      (smoke-arch--assert (= 1 (length smoke--requests)) "queued call never executes")))
  ;; The formal adapters, not a synthetic legacy Request.
  (require 'ob-superchat)
  (require 'superchat-magit)
  (smoke--with-fixture '((:text "org answer") (:text "magit answer"))
    (with-temp-buffer
      (insert "user bytes")
      (let ((original (buffer-string)) (assembled 0) surfaces)
        (superchat-context-register 'smoke-adapter-context
                            (lambda (envelope &rest _)
                              (push (plist-get envelope :surface) surfaces)
                              (cl-incf assembled)
                              '((:owner smoke :id "adapter-marker" :slot context :text "ADAPTER_CONTEXT" :priority 0 :budget-class hard)))
                            :owner 'smoke :required t)
        (unwind-protect
            (progn
              (smoke-arch--assert (equal "org answer" (org-babel-execute:superchat "query" nil)) "Org adapter succeeds")
              (smoke-arch--assert (equal "magit answer" (superchat-magit--call-llm "diff prompt")) "Magit adapter succeeds")
              (smoke-arch--assert (and (= assembled 2) (equal surfaces '(magit org-babel))) "both formal adapters traverse collection")
              (dolist (request smoke--requests)
                (smoke-arch--assert (string-match-p "ADAPTER_CONTEXT" (plist-get request :prompt-text)) "adapter context assembled"))
              (smoke-arch--assert (equal original (buffer-string)) "blocking adapters never default-write host"))
          (superchat-context-unregister 'smoke-adapter-context)))))
  (smoke--with-fixture nil
    (superchat-on nil 'pre-step 'smoke-adapter-drop (lambda (_proposal _next) '(:reject "adapter guard")))
    (unwind-protect
        (progn
          (smoke-arch--assert (string-match-p "Superchat error:" (org-babel-execute:superchat "query" nil)) "Org reports admission error")
          (smoke-arch--assert (smoke-arch--condition (lambda () (superchat-magit--call-llm "diff"))) "Magit signals admission error")
          (smoke-arch--assert (null smoke--requests) "formal adapters obey admission"))
      (superchat-off 'pre-step 'smoke-adapter-drop))))

(defun smoke-arch--inflight-unmount ()
  (smoke--with-fixture '((:text "already in flight" :delay 0.05))
    (let* ((user (generate-new-buffer " *architecture-user*"))
           (owned (superchat-editor-workspace-buffer "architecture" 'host t))
           (handle (with-current-buffer owned (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)))))
      (superchat-plugin-unmount 'transport-llm)
      (unwind-protect
          (progn
            (smoke-arch--assert (and (buffer-live-p user) (buffer-live-p owned)) "unmount must not close user or Session-owned buffer")
            (smoke-arch--assert (eq 'cancelled (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)) "transport unmount immediately cancels old Run")
            (smoke-arch--assert (member "run.cancelled" (smoke--types "architecture")) "transport cancellation is durable")
            (smoke-arch--assert (and (buffer-live-p user) (buffer-live-p owned)) "completion leaves hosts alive"))
        (superchat-plugin-mount 'transport-llm)))))

(defun smoke-arch--tool-reinstallation ()
  (let* ((a (superchat-scope-create "old-installation" superchat-scope-global superchat-scope-global))
         (b (superchat-scope-create "new-installation" superchat-scope-global superchat-scope-global))
         (name "architecture-reinstall"))
    (unwind-protect
        (progn
          (let ((superchat-plugin-current-scope a))
            (superchat-register-tool name :function (lambda () "A") :description "A"))
          (superchat-unregister-tool name)
          (let ((superchat-plugin-current-scope b))
            (superchat-register-tool name :function (lambda () "B") :description "B"))
          (superchat-scope-dispose a)
          (let ((installation (superchat-tools--visible-installation name)))
            (smoke-arch--assert (equal "B" (plist-get installation :description)) "old undo preserves new installation")
            (smoke-arch--assert (equal "B" (funcall (plist-get installation :function)))
                                "old undo leaves the new body the callable one"))
          (superchat-scope-dispose b)
          (smoke-arch--assert (not (superchat-tools--visible-installation name)) "new installation fully withdrawn"))
      (superchat-scope-dispose a)
      (superchat-scope-dispose b))))

(defun smoke-arch--direct-provider-dispose ()
  (smoke--with-fixture '((:text "direct Scope in flight" :delay 0.05))
    (let* ((table (plist-get (superchat-service-get nil 'model-transport) :table))
           (scope (superchat-scope-create "direct-transport" superchat-scope-global superchat-scope-global))
           (user (generate-new-buffer " *direct-dispose-user*"))
           (owned (superchat-editor-workspace-buffer "architecture" 'host t)))
      (superchat-plugin-unmount 'transport-llm)
      (unwind-protect
          (progn
            (superchat-service-provide scope 'model-transport table)
            (let ((handle (with-current-buffer owned (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)))))
              (superchat-scope-dispose scope)
              (smoke-arch--assert (and (eq 'pending (superchat-plugin-state 'driver))
                                      (null (superchat-service-get nil 'driver))) "direct provider disposal withdraws driver immediately")
              (smoke-arch--assert (and (buffer-live-p user) (buffer-live-p owned)) "Scope disposal has no buffer-close authority")
              (smoke-arch--assert (eq 'cancelled (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)) "direct-disposal immediately cancels in-flight Run")
              (smoke-arch--assert (and (buffer-live-p user) (buffer-live-p owned)) "direct-disposal completion retains both buffers")))
        (superchat-scope-dispose scope)
        (superchat-plugin-mount 'transport-llm)))))

(defun smoke-arch--transport-revocation ()
  (require 'superchat-transport-mock)
  (smoke--with-fixture nil
    (superchat-plugin-unmount 'transport-llm)
    (let* ((scope (superchat-scope-create "revocation-provider" superchat-scope-global superchat-scope-global))
           (observers nil) (calls 0) ids callbacks observations undo)
      (unwind-protect
          (progn
            (setq undo
                  (superchat-service-provide
                   scope 'model-transport
                   (list (cons 'generate (lambda (&rest _)
                                           (push superchat--llm-runtime-observer observers)
                                           (cl-incf calls)))
                         (cons 'context-budget (lambda (&rest _) 40000))
                         (cons 'capabilities (lambda (&rest _) '(:streaming t :tools t))))))
            (dotimes (index 3)
              (let* ((iteration index)
                     (sink (superchat-sink-create
                            :error
                            (lambda (run _failure)
                              (push (plist-get run :run-id) callbacks)
                              (push (and (member "run.cancelled" (smoke--types "architecture"))
                                         (null (superchat-service-get nil 'driver))) observations)
                              ;; Reentrant completion must not change another
                              ;; retiring Run's outcome or prevent the rest.
                              (dolist (observer observers)
                                (funcall observer 'terminal '(:status completed :response "late reentry")))
                              (smoke-arch--assert
                               (smoke-arch--condition
                                (lambda () (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))
                               "terminal reentry cannot submit")
                              (signal (if (= iteration 0) 'quit 'error) '("teardown consumer")))))
                     (envelope (smoke-arch--envelope 'external sink)))
                (when ids (setq envelope (plist-put envelope :parent-run-id (car ids))))
                (push (plist-get (superchat-service-call nil 'driver 'submit envelope) :run-id) ids)))
            (funcall undo)
            (smoke-arch--assert (= calls 3) "no escaping provider calls")
            (smoke-arch--assert (= (length callbacks) 3) "all old Runs get one terminal Sink despite error/quit")
            (smoke-arch--assert (cl-every #'identity observations) "Tape cancellation and withdrawn driver precede Sink")
            (dolist (id ids)
              (smoke-arch--assert (eq 'cancelled (plist-get (superchat-runtime-status id) :status)) "all old parent and child Runs cancelled"))
            (superchat-plugin-mount 'transport-mock '(:script ((:text "new provider"))))
            (let ((handle (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))))
              (dolist (observer observers) (funcall observer 'terminal '(:status completed :response "old callback after remount")))
              (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p (plist-get handle :run-id)))) "replacement Run completes")
              (smoke-arch--assert (eq 'completed (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)) "replacement is independent"))
            (smoke-arch--assert (= (length callbacks) 3) "late callbacks never redeliver old terminal")
            (let ((before (smoke--types "architecture")))
              (superchat-plugin-unmount 'transport-mock)
              (smoke-arch--assert (equal before (smoke--types "architecture")) "idle unload creates no events")))
        (superchat-scope-dispose scope)
        (when (superchat-plugin-state 'transport-mock) (superchat-plugin-unmount 'transport-mock))
        (superchat-plugin-mount 'transport-llm)))))

(defun smoke-arch--mock-inflight-cancel ()
  (smoke--with-fixture nil
    (superchat-plugin-unmount 'transport-llm)
    (superchat-plugin-mount 'transport-mock '(:script ((:text "too late" :delay 1))))
    (unwind-protect
        (let* ((handle (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)))
               (id (plist-get handle :run-id)))
          (superchat-plugin-unmount 'transport-mock)
          (smoke-arch--assert (eq 'cancelled (plist-get (superchat-runtime-status id) :status)) "mock unload cancels Runtime, not just its timer")
          (smoke-arch--assert (= 1 (cl-count "run.cancelled" (smoke--types "architecture") :test #'equal)) "mock cancellation has one durable terminal"))
      (when (superchat-plugin-state 'transport-mock) (superchat-plugin-unmount 'transport-mock))
      (superchat-plugin-mount 'transport-llm))))

(defun smoke-arch--transport-tool-boundaries ()
  ;; An ordinary tool-before listener is a reentrant execution gate, too.
  (dolist (stop '(cancel unmount))
    (smoke--with-fixture '((:tool-uses (("architecture-stop-tool"))) (:text "must not follow up"))
      (let ((executions 0) entered undo)
        (superchat-register-tool "architecture-stop-tool" :function (lambda () (cl-incf executions) "effect")
                                 :description "stop before entry")
        (setq undo (superchat-on nil 'tool-before 'architecture-stop
                                (lambda (_tool _args id)
                                  (setq entered t)
                                  (if (eq stop 'cancel) (superchat-runtime-cancel id)
                                    (superchat-plugin-unmount 'transport-llm)))))
        (unwind-protect
            (let* ((env (plist-put (smoke-arch--envelope) :tools '("architecture-stop-tool")))
                   (id (plist-get (superchat-service-call nil 'driver 'submit env) :run-id)))
              (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p id))) "pre-tool stop settles")
              (smoke-arch--assert (and entered (= executions 0) (= 1 (length smoke--requests))) "pre-tool stop blocks ordinary body and followup")
              (smoke-arch--assert (not (superchat-runtime-effect-capable-p id)) "open Effect is immediately inert")
              (smoke-arch--assert (eq 'audit-incomplete (plist-get (superchat-runtime-status id) :status)) "open Effect keeps audit-incomplete exception")
              (smoke-arch--assert (and (member "tool.execution.started" (smoke--types "architecture"))
                                      (not (member "tool.completed" (smoke--types "architecture")))) "stop does not fabricate completed Effect")
              (let* ((trace (superchat-tape-replay-events "architecture"))
                     (started
                      (cl-remove-if-not
                       (lambda (event)
                         (and (equal id (plist-get event :run-id))
                              (equal "tool.execution.started"
                                     (plist-get event :type))))
                       trace))
                     (execution-id
                      (plist-get (plist-get (car started) :payload) :execution-id))
                     (settled-ids
                      (mapcar
                       (lambda (event)
                         (plist-get (plist-get event :payload) :execution-id))
                       (cl-remove-if-not
                        (lambda (event)
                          (and (equal id (plist-get event :run-id))
                               (member (plist-get event :type)
                                       '("tool.completed" "tool.failed"))))
                        trace))))
                (smoke-arch--assert
                 (and (= 1 (length started))
                      (stringp execution-id)
                      (not (equal "" execution-id))
                      (not (member execution-id settled-ids)))
                 "pre-tool stop leaves one entered Effect with no settlement")))
          (funcall undo)
          (superchat-unregister-tool "architecture-stop-tool")
          (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))
  ;; Already-entered async work has no generic cancellation handle.  Its real
  ;; late result may arrive, but cannot resume the cancelled model loop.
  (smoke--with-fixture '((:tool-uses (("architecture-async-tool"))) (:text "fresh Run answer"))
    (let (done (executions 0) (terminals 0))
      (superchat-register-tool "architecture-async-tool" :async t
                               :function (lambda (callback) (cl-incf executions) (setq done callback))
                               :description "pending external operation")
      (unwind-protect
          (let* ((sink (superchat-sink-create :error (lambda (&rest _) (cl-incf terminals))))
                 (env (plist-put (smoke-arch--envelope 'external sink) :tools '("architecture-async-tool")))
                 (id (plist-get (superchat-service-call nil 'driver 'submit env) :run-id)))
            (smoke-arch--assert (smoke--wait (lambda () done)) "async body entered")
            (superchat-plugin-unmount 'transport-llm)
            (smoke-arch--assert (and (= executions 1) (= terminals 1)
                                    (eq 'audit-incomplete (plist-get (superchat-runtime-status id) :status))) "open async Effect stays truthful at cancellation")
            (let* ((trace (superchat-tape-replay-events "architecture"))
                   (started
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal id (plist-get event :run-id))
                            (equal "tool.execution.started"
                                   (plist-get event :type))))
                     trace))
                   (execution-id
                    (plist-get (plist-get (car started) :payload) :execution-id))
                   (settled-ids
                    (mapcar
                     (lambda (event)
                       (plist-get (plist-get event :payload) :execution-id))
                     (cl-remove-if-not
                      (lambda (event)
                        (and (equal id (plist-get event :run-id))
                             (member (plist-get event :type)
                                     '("tool.completed" "tool.failed"))))
                      trace))))
              (smoke-arch--assert
               (and (= 1 (length started))
                    (stringp execution-id)
                    (not (equal "" execution-id))
                    (not (member execution-id settled-ids)))
               "unload records the honestly unresolved async Effect"))
            (superchat-plugin-mount 'transport-llm)
            (let ((new (plist-get (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)) :run-id)))
              (funcall done "real late result")
              (funcall done "duplicate late result")
              (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p new))) "new Run completes independently of async old result")
              (smoke-arch--assert (eq 'completed (plist-get (superchat-runtime-status new) :status)) "new Run unaffected"))
            (smoke-arch--assert (and (= 2 (length smoke--requests)) (= terminals 1)
                                    (eq 'audit-incomplete (plist-get (superchat-runtime-status id) :status))
                                    (not (superchat-runtime-effect-capable-p id))) "old async result never sends a new request, redelivers, or reopens its Run")
            (let* ((trace (superchat-tape-replay-events "architecture"))
                   (run-events
                    (cl-remove-if-not
                     (lambda (event) (equal id (plist-get event :run-id)))
                     trace))
                   (started
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "tool.execution.started" (plist-get event :type)))
                     run-events))
                   (completed
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "tool.completed" (plist-get event :type)))
                     run-events))
                   (failed
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "tool.failed" (plist-get event :type)))
                     run-events))
                   (started-id
                    (plist-get (plist-get (car started) :payload) :execution-id))
                   (completed-id
                    (plist-get (plist-get (car completed) :payload) :execution-id))
                   (failed-ids
                    (mapcar
                     (lambda (event)
                       (plist-get (plist-get event :payload) :execution-id))
                     failed))
                   (run-terminals
                    (cl-remove-if-not
                     (lambda (event)
                       (member (plist-get event :type)
                               '("run.completed" "run.failed" "run.cancelled")))
                     run-events))
                   (turn-terminals
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "turn.completed" (plist-get event :type)))
                     run-events)))
              (smoke-arch--assert
               (and (= 1 (length started))
                    (= 1 (length completed))
                    (equal started-id completed-id)
                    (not (member started-id failed-ids))
                    (= 1 (length run-terminals))
                    (= 1 (length turn-terminals)))
               "a real late result settles its entered Effect exactly once")))
        (superchat-unregister-tool "architecture-async-tool")
        (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-late-network ()
  (smoke--with-fixture '((:hang t) (:text "new answer"))
    (let ((network (symbol-function 'llm-chat-streaming)) callbacks (executions 0))
      (superchat-register-tool "architecture-late-tool" :function (lambda () (cl-incf executions) "late")
                               :description "must not execute after stop")
      (unwind-protect
          (cl-letf (((symbol-function 'llm-chat-streaming)
                     (lambda (provider prompt partial response failure &optional multi)
                       (unless callbacks (setq callbacks (list provider prompt partial response failure multi)))
                       (funcall network provider prompt partial response failure multi))))
            (let* ((env (plist-put (smoke-arch--envelope) :tools '("architecture-late-tool")))
                   (id (plist-get (superchat-service-call nil 'driver 'submit env) :run-id)))
              (superchat-plugin-unmount 'transport-llm)
              (superchat-plugin-mount 'transport-llm)
              (let ((new (plist-get (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)) :run-id)))
                (pcase-let ((`(,provider ,prompt ,partial ,response ,failure ,multi) callbacks))
                  (when partial (funcall partial "old stream"))
                  (funcall failure 'llm-unknown-tool "old invalid tool retry")
                  (funcall failure 'llm-context-length-exceeded "old overflow retry")
                  (funcall response '(:text "old answer"))
                  ;; llm.el processes tool uses before calling the model-loop
                  ;; callback, so this must be blocked by Authority itself.
                  (llm-provider-utils-process-result
                   provider prompt (list :tool-uses (list (smoke--tool-use '("architecture-late-tool"))))
                   multi response failure))
                (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p new))) "replacement network Run completes"))
              (smoke-arch--assert (and (= executions 0) (= 2 (length smoke--requests))
                                      (eq 'cancelled (plist-get (superchat-runtime-status id) :status))) "late model retry/tool/stream callbacks cannot revive old Run")))
        (superchat-unregister-tool "architecture-late-tool")
        (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-queue ()
  (smoke--with-fixture '((:hang t) (:text "queued answer"))
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (results nil))
      (superchat-service-call nil 'driver 'submit (smoke-arch--envelope))
      (superchat-runtime-enqueue-follow-up
       "architecture" (smoke-arch--envelope 'external (superchat-sink-create :final (lambda (_run result) (push (plist-get result :text) results)))))
      (superchat-plugin-unmount 'transport-llm)
      (unwind-protect
          (progn
            (dotimes (_ 3) (superchat-runtime--dispatch-next "architecture"))
            (smoke-arch--assert (and (= 1 (superchat-runtime-pending-follow-ups "architecture"))
                                    (zerop (hash-table-count superchat-runtime--queue-timers))
                                    (= 1 (length smoke--requests))) "unavailable transport preserves FIFO without dispatch or busy retry")
            (smoke-arch--assert (and (member "queue.queued" (smoke--types "architecture"))
                                    (not (member "queue.dropped" (smoke--types "architecture")))) "paused FIFO retains original input fact")
            (superchat-plugin-mount 'transport-llm)
            (smoke-arch--assert (smoke--wait (lambda () results)) "remount resumes existing FIFO")
            (smoke-arch--assert (and (equal results '("queued answer"))
                                    (= 2 (length smoke--requests))
                                    (zerop (superchat-runtime-pending-follow-ups "architecture"))) "only queued input resumes, not cancelled Run"))
        (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-queue-liveness ()
  "A public queue Sink predicate may withdraw transport before submit."
  (smoke--with-fixture '((:text "queued after liveness withdrawal"))
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (armed nil) results)
      (unwind-protect
          (let ((sink (superchat-sink-create
                       :live-p (lambda ()
                                 (when armed
                                   (setq armed nil)
                                   (superchat-plugin-unmount 'transport-llm))
                                 t)
                       :final (lambda (_run result)
                                (push (plist-get result :text) results))
                       :error #'ignore)))
            (superchat-runtime-enqueue-follow-up
             "architecture" (smoke-arch--envelope 'external sink))
            (setq armed t)
            (superchat-runtime--dispatch-next "architecture")
            (smoke-arch--assert
             (and (= 1 (superchat-runtime-pending-follow-ups "architecture"))
                  (zerop (hash-table-count superchat-runtime--queue-timers))
                  (null smoke--requests))
             "liveness withdrawal pauses the canonical FIFO before submit")
            (smoke-arch--assert
             (and (member "queue.queued" (smoke--types "architecture"))
                  (not (member "queue.dropped" (smoke--types "architecture"))))
             "liveness withdrawal never relabels queued input submit-error")
            (superchat-plugin-mount 'transport-llm)
            (smoke-arch--assert (smoke--wait (lambda () results))
                                "restored Driver resumes liveness-paused FIFO")
            (smoke-arch--assert
             (and (equal results '("queued after liveness withdrawal"))
                  (= 1 (length smoke--requests))
                  (zerop (superchat-runtime-pending-follow-ups "architecture")))
             "the original FIFO head, not a replacement queue, dispatches after remount"))
        (unless (superchat-plugin-state 'transport-llm)
          (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-startup ()
  (smoke--with-fixture nil
    (let ((undo (superchat-on nil 'run-started 'architecture-start-stop
                             (lambda (&rest _) (superchat-plugin-unmount 'transport-llm)))))
      (unwind-protect
          (let ((id (plist-get (superchat-service-call nil 'driver 'submit (smoke-arch--envelope)) :run-id)))
            (smoke-arch--assert (and (eq 'cancelled (plist-get (superchat-runtime-status id) :status))
                                    (null smoke--requests)) "run-started reentry cannot revive cancelled startup"))
        (funcall undo)
        (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-sealing ()
  (smoke--with-fixture '((:text "answer at seal"))
    (let* (statuses (seals 0)
           (undo (superchat-on nil 'run-sealing 'architecture-seal-stop
                             (lambda (&rest _)
                               (cl-incf seals)
                               (when (superchat-plugin-state 'transport-llm)
                                 (superchat-plugin-unmount 'transport-llm))))))
      (unwind-protect
          (let* ((sink (superchat-sink-create
                        :error (lambda (_run failure) (push (plist-get failure :status) statuses))
                        :final (lambda (&rest _) (push 'completed statuses))))
                 (id (plist-get (superchat-service-call nil 'driver 'submit (smoke-arch--envelope 'external sink)) :run-id)))
            (smoke-arch--assert (smoke--wait (lambda () (smoke--run-done-p id))) "sealing cancellation settles")
            (smoke-arch--assert (and (eq 'cancelled (plist-get (superchat-runtime-status id) :status))
                                    (equal statuses '(cancelled))) "outer completion cannot overwrite nested cancellation")
            (smoke-arch--assert (equal '("run.cancelled")
                                      (cl-remove-if-not (lambda (type) (member type '("run.completed" "run.failed" "run.cancelled")))
                                                        (smoke--types "architecture"))) "nested seal creates exactly one terminal fact")
            (smoke-arch--assert (= seals 1) "run-sealing itself is once-only under reentry"))
        (funcall undo)
        (unless (superchat-plugin-state 'transport-llm) (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-cancel-sealing ()
  "A cancellation seal is authoritative before a sealing listener reenters."
  (smoke--with-fixture '((:hang t))
    (let* (statuses (seals 0)
           (undo (superchat-on nil 'run-sealing 'architecture-cancel-seal-stop
                               (lambda (&rest _)
                                 (cl-incf seals)
                                 (when (superchat-plugin-state 'transport-llm)
                                   (superchat-plugin-unmount 'transport-llm))))))
      (unwind-protect
          (let* ((sink (superchat-sink-create
                        :error (lambda (_run failure) (push (plist-get failure :status) statuses))
                        :final (lambda (&rest _) (push 'completed statuses))))
                 (id (plist-get (superchat-service-call nil 'driver 'submit
                                                      (smoke-arch--envelope 'external sink))
                                :run-id)))
            (superchat-runtime-cancel id "cancel before sealing withdrawal")
            (smoke-arch--assert
             (and (eq 'cancelled (plist-get (superchat-runtime-status id) :status))
                  (equal statuses '(cancelled))
                  (= 1 (length smoke--requests)))
             "nested transport cancellation cannot replace a published cancellation status")
            (smoke-arch--assert
             (equal '("run.cancelled")
                    (cl-remove-if-not
                     (lambda (type) (member type '("run.completed" "run.failed" "run.cancelled")))
                     (smoke--types "architecture")))
             "cancellation sealing creates exactly one terminal fact")
            (smoke-arch--assert (= seals 1) "cancellation sealing listener runs once"))
        (funcall undo)
        (unless (superchat-plugin-state 'transport-llm)
          (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-sink-live-sealing ()
  "A Sink `live-p' callback cannot restore an older finishing entry."
  (smoke--with-fixture '((:hang t))
    (let ((armed nil) statuses)
      (unwind-protect
          (let* ((sink (superchat-sink-create
                        :live-p (lambda ()
                                  (when armed
                                    (setq armed nil)
                                    (superchat-plugin-unmount 'transport-llm))
                                  t)
                        :error (lambda (_run failure)
                                 (push (plist-get failure :status) statuses))
                        :final (lambda (&rest _) (push 'completed statuses))))
                 (id (plist-get (superchat-service-call nil 'driver 'submit
                                                      (smoke-arch--envelope 'external sink))
                                :run-id)))
            ;; Admission itself checks `live-p', so arm only for the public
            ;; finish-time check which used to restore `delivering' afterward.
            (setq armed t)
            (superchat-runtime--finish id 'completed "answer before Sink liveness check")
            (smoke-arch--assert
             (and (eq 'cancelled (plist-get (superchat-runtime-status id) :status))
                  (equal statuses '(cancelled))
                  (plist-get (superchat-runtime--entry id) :quiesced)
                  (= 1 (length smoke--requests)))
             "Sink liveness reentry leaves its nested cancellation authoritative")
            (smoke-arch--assert
             (equal '("run.cancelled")
                    (cl-remove-if-not
                     (lambda (type) (member type '("run.completed" "run.failed" "run.cancelled")))
                     (smoke--types "architecture")))
             "Sink liveness reentry writes no competing outer terminal"))
        (unless (superchat-plugin-state 'transport-llm)
          (superchat-plugin-mount 'transport-llm))))))

(defun smoke-arch--transport-postseal-failure ()
  "A failure after a confirmed seal preserves its ordinary terminal fact."
  (smoke--with-fixture '((:hang t))
    (let* ((calls 0) statuses
           (sink (superchat-sink-create
                  :error (lambda (_run failure)
                           (cl-incf calls)
                           (push (plist-get failure :status) statuses))))
           (id (plist-get (superchat-service-call nil 'driver 'submit
                                                (smoke-arch--envelope 'external sink))
                          :run-id))
           (append-event (symbol-function 'superchat-tape-append-event-record)))
      (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                 (lambda (event &rest args)
                   (if (equal (plist-get event :type) "turn.completed")
                       (error "simulated Turn closure persistence failure")
                     (apply append-event event args)))))
        (superchat-runtime-cancel id "cancel before failed Turn completion"))
      (smoke-arch--assert
       (and (eq 'cancelled (plist-get (superchat-runtime-status id) :status))
            (= calls 1)
            (equal statuses '(cancelled))
            (stringp (plist-get (superchat-runtime-status id) :error))
            (= 1 (cl-count "run.cancelled" (smoke--types "architecture") :test #'equal))
            (plist-get (superchat-runtime--entry id) :quiesced)
            (null (plist-get (superchat-runtime--entry id) :transport-request)))
       "confirmed post-seal failure preserves cancellation delivery and release"))))

(defun smoke-case-R1 ()
  "transport dependency and in-flight unmount buffer authority"
  (smoke-arch--transport-basic)
  (smoke-arch--inflight-unmount)
  (smoke-arch--direct-provider-dispose)
  (smoke-arch--transport-revocation)
  (smoke-arch--mock-inflight-cancel)
  (smoke-arch--transport-tool-boundaries)
  (smoke-arch--transport-late-network)
  (smoke-arch--transport-queue)
  (smoke-arch--transport-queue-liveness)
  (smoke-arch--transport-startup)
  (smoke-arch--transport-sealing)
  (smoke-arch--transport-cancel-sealing)
  (smoke-arch--transport-sink-live-sealing)
  (smoke-arch--transport-postseal-failure)
  '(("contract" . passed)))

(defun smoke-case-R2 ()
  "blocking pipeline, formal adapters, terminal conditions and FIFO liveness"
  (smoke-arch--blocking-basic)
  (smoke-arch--blocking-boundaries)
  '(("contract" . passed)))

(defun smoke-arch--mcp-refresh ()
  (require 'superchat-mcp)
  (let* ((server "architecture-refresh")
         (name "mcp:architecture-refresh:lookup")
         (stale "mcp:architecture-refresh:stale")
         (tool (list :name name :category "mcp-architecture-refresh"
                     :description "version 1" :function #'ignore))
         (stale-tool (plist-put (copy-sequence tool) :name stale)))
    (unwind-protect
        (progn
          (superchat-mcp-register-server-tools server (list tool stale-tool))
          (dolist (version '("version 2" "version 3"))
            (setq tool (plist-put tool :description version))
            (smoke-arch--assert (equal (list name stale) (superchat-mcp-register-server-tools server (list tool stale-tool))) "consecutive MCP refresh keeps both tools")
            (smoke-arch--assert
             (equal version (plist-get (superchat-tools--visible-installation name) :description))
             "MCP refresh installs new descriptor"))
          (let ((old (superchat-tools--visible-installation name)))
            (superchat-mcp-register-server-tools
             server (list (plist-put (copy-sequence tool) :function 42) stale-tool))
            (smoke-arch--assert (eq old (superchat-tools--visible-installation name)) "bad MCP descriptor keeps old installation")
            (smoke-arch--assert (and (plist-get old :live)
                                    (fboundp (plist-get old :function)))
                                "bad descriptor leaves the old installation live and callable")
            (superchat-mcp-register-server-tools server (superchat-mcp--make-collection nil nil))
            (smoke-arch--assert (eq old (superchat-tools--visible-installation name)) "failed collection preserves old installation")
            (smoke-arch--assert (superchat-tools--visible-installation stale) "failed collection must not sweep stale"))
          (superchat-mcp-register-server-tools server (list tool))
          (smoke-arch--assert (and (superchat-tools--visible-installation name)
                                  (not (superchat-tools--visible-installation stale)))
                              "successful collection sweeps stale and its paired Authority"))
      (superchat-mcp-unregister-server-tools server))))

(defun smoke-arch--mcp-refresh-retention ()
  (require 'superchat-mcp)
  (let* ((server "architecture-retention")
         (name "mcp:architecture-retention:lookup")
         (before (length (superchat-scope-effects superchat-scope-global))))
    (unwind-protect
        (progn
          (dotimes (index 100)
            (let ((description (format "actual revision %d" index)))
              (superchat-mcp-register-server-tools
               server (list (list :name name :category "mcp-architecture-retention"
                                  :description description
                                  :function (lambda () description))))
              (let ((spec (superchat-tools--visible-installation name)))
                (smoke-arch--assert (equal description (plist-get spec :description)) "retention probe actually updates descriptor")
                (smoke-arch--assert (equal description (funcall (plist-get spec :function))) "retention probe actually replaces implementation"))))
          (superchat-mcp-unregister-server-tools server)
          (smoke-arch--assert (null (superchat-mcp-registered-tool-names server)) "retention probe unregisters tool")
          (smoke-arch--assert (= before (length (superchat-scope-effects superchat-scope-global))) "100 actual MCP updates then unregister return Scope effects to baseline"))
      (superchat-mcp-unregister-server-tools server))))

(defun smoke-case-R3 ()
  "duplicate tools and installation-owned rollback"
  (smoke-arch--tool-basic)
  (smoke-arch--tool-reinstallation)
  (smoke-arch--mcp-refresh)
  (smoke-arch--mcp-refresh-retention)
  '(("contract" . passed)))

(defun smoke-arch--postseal-write-case (terminal target fault &optional unreadable)
  "Exercise one real SQLite receipt window for TERMINAL/TARGET/FAULT.

UNREADABLE extends an after-INSERT receipt loss through the immediate lookup,
proving that Runtime never turns an unreadable absence into a guessed fallback
terminal.  The small R5 matrix deliberately covers the two distinct ordinary
event shapes; the external fault matrix exercises the remaining statuses."
  (smoke--with-fixture '((:hang t))
    (let* ((calls 0) statuses (sealing-count 0) (armed t) reads-broken
           target-write
           (sink (superchat-sink-create
                  :final (lambda (&rest _) (cl-incf calls) (push 'completed statuses))
                  :error (lambda (_run failure)
                           (cl-incf calls)
                           (push (plist-get failure :status) statuses))))
           (id (plist-get (superchat-service-call nil 'driver 'submit
                                                (smoke-arch--envelope 'external sink))
                          :run-id))
           (append-row (symbol-function 'superchat-db-tape-append))
           (select (symbol-function 'sqlite-select))
           (undo (superchat-on nil 'run-sealing 'smoke-arch-postseal-hook
                               (lambda (&rest _) (cl-incf sealing-count)))))
      (unwind-protect
          (cl-letf
              (((symbol-function 'superchat-db-tape-append)
                (lambda (session kind content &rest options)
                  (setq target-write
                        (equal target (smoke-arch--row-event-type options)))
                  (when (and armed target-write (eq fault 'before))
                    (setq armed nil)
                    (error "R5 pre-insert failure: %s" target))
                  (apply append-row session kind content options)))
               ((symbol-function 'sqlite-select)
                (lambda (db sql &rest args)
                  (when (and armed target-write (eq fault 'after-insert)
                             (string-match-p "last_insert_rowid" sql))
                    (setq armed nil)
                    (if unreadable
                        (progn
                          (setq reads-broken t)
                          (error "R5 unreadable Tape after committed %s" target))
                      (error "R5 lost append receipt: %s" target)))
                  (when reads-broken
                    (error "R5 unreadable Tape after committed %s" target))
                  (apply select db sql args))))
            (superchat-runtime--finish id terminal "R5 terminal response"))
        (funcall undo))
      (let* ((trace (superchat-tape-replay-events "architecture"))
             (entry (superchat-runtime--entry id))
             (expected
              (if (and (string-prefix-p "run." target)
                       (or (eq fault 'before) unreadable))
                  'audit-incomplete
                terminal))
             (run-seals
              (cl-remove-if-not
               (lambda (event)
                 (and (equal id (plist-get event :run-id))
                      (member (plist-get event :type)
                              '("run.completed" "run.failed" "run.cancelled"))))
               trace))
             (turn-seals
              (cl-remove-if-not
               (lambda (event)
                 (and (equal id (plist-get event :run-id))
                      (equal "turn.completed" (plist-get event :type))))
               trace))
             (run-seal (car run-seals))
             (turn-seal (car turn-seals)))
        (smoke-arch--assert (not armed) "R5 reaches the requested SQLite fault")
        (smoke-arch--assert
         (and (= sealing-count 1)
              (eq expected (plist-get (superchat-runtime-status id) :status))
              (equal statuses (list expected))
              (= calls 1)
              (stringp (plist-get entry :error))
              (plist-get entry :quiesced)
              (= 1 (length run-seals))
              (if unreadable
                  (<= (length turn-seals) 1)
                (= 1 (length turn-seals)))
              (or (null turn-seal)
                  (equal (plist-get turn-seal :causation-id)
                         (plist-get run-seal :event-id))))
         "R5 keeps one Run/Turn seal, one hook and one Sink outcome")))))

(defun smoke-arch--postseal-turn-fault-terminal-condition ()
  "Keep causal order when a missing Turn tail meets a terminal consumer error.

The Turn append failure queues a recovery suffix but does not synchronously
replay it.  A consumer error or quit can therefore write `delivery.failed'
before that suffix reaches Tape.  The delivery event must remain caused by the
confirmed Run seal, so the eventual Turn suffix has a real causal ancestor and
the trace stays well formed."
  (dolist (spec '((completed final) (failed error)))
    (pcase-let ((`(,terminal ,slot) spec))
      (dolist (symbol '(error quit))
        (smoke--with-fixture
            (if (eq terminal 'completed)
                '((:text "R5 blocking answer"))
              '((:error (error . "R5 blocking provider failure"))))
          (let* ((calls 0) observed-run (armed t)
                 (expected (list symbol "R5 terminal consumer condition" terminal))
                 (callback
                  (lambda (run _payload)
                    (cl-incf calls)
                    (setq observed-run run)
                    (signal symbol (cdr expected))))
                 (sink (superchat-sink-create
                        :final (and (eq slot 'final) callback)
                        :error (and (eq slot 'error) callback)))
                 (append-row (symbol-function 'superchat-db-tape-append))
                 (condition
                  (cl-letf
                      (((symbol-function 'superchat-db-tape-append)
                        (lambda (session kind content &rest options)
                          (when (and armed
                                     (equal "turn.completed"
                                            (smoke-arch--row-event-type options)))
                            (setq armed nil)
                            (error "R5 injected Turn append failure"))
                          (apply append-row session kind content options))))
                    (smoke-arch--condition
                     (lambda ()
                       (superchat-service-call
                        nil 'driver 'submit-blocking
                        (smoke-arch--envelope 'org-babel sink) 2)))))
                 (run-id (plist-get observed-run :run-id))
                 ;; Replay is the normal later reconciliation point for the
                 ;; sidecar.  It must see a valid prefix even though delivery
                 ;; happened before this suffix was appended.
                 (trace (superchat-tape-replay-events "architecture"))
                 (run-seals
                  (cl-count-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (member (plist-get event :type)
                                  '("run.completed" "run.failed" "run.cancelled"))))
                   trace))
                 (turn-seals
                  (cl-count-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (equal "turn.completed" (plist-get event :type))))
                   trace))
                 (delivery-seals
                  (cl-count-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (equal "delivery.failed" (plist-get event :type))))
                   trace))
                 (run-seal
                  (cl-find-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (member (plist-get event :type)
                                  '("run.completed" "run.failed" "run.cancelled"))))
                   trace))
                 (turn-seal
                  (cl-find-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (equal "turn.completed" (plist-get event :type))))
                   trace))
                 (delivery-seal
                  (cl-find-if
                   (lambda (event)
                     (and (equal run-id (plist-get event :run-id))
                          (equal "delivery.failed" (plist-get event :type))))
                   trace))
                 (run-index
                  (and run-seal
                       (cl-position (plist-get run-seal :event-id) trace
                                    :key (lambda (event)
                                           (plist-get event :event-id))
                                    :test #'equal)))
                 (delivery-index
                  (and delivery-seal
                       (cl-position (plist-get delivery-seal :event-id) trace
                                    :key (lambda (event)
                                           (plist-get event :event-id))
                                    :test #'equal)))
                 (entry (and run-id (superchat-runtime--entry run-id))))
            (smoke-arch--assert
             (and (not armed)
                  (equal expected condition)
                  (= calls 1)
                  (eq terminal
                      (plist-get (superchat-runtime-status run-id) :status))
                  run-seal
                  turn-seal
                  delivery-seal
                  (= run-seals 1)
                  (= turn-seals 1)
                  (= delivery-seals 1)
                  (equal (plist-get turn-seal :causation-id)
                         (plist-get run-seal :event-id))
                  (equal (plist-get delivery-seal :causation-id)
                         (plist-get run-seal :event-id))
                  (and run-index delivery-index
                       (< run-index delivery-index))
                  (plist-get entry :quiesced))
             "R5 Turn recovery preserves terminal consumer condition and causal order")))))))

(defun smoke-case-R5 ()
  "post-seal SQLite receipt recovery never competes with a durable Run fact"
  ;; The eight readable cases cover both ordinary status/event shapes and both
  ;; sides of SQLite's INSERT/receipt window.  The two unreadable cases ensure
  ;; the three-state lookup does not infer an absent Run or Turn.
  (dolist (terminal '(cancelled completed))
    (dolist (spec (list (list "turn.completed" 'before)
                        (list "turn.completed" 'after-insert)
                        (list (if (eq terminal 'cancelled)
                                  "run.cancelled" "run.completed") 'before)
                        (list (if (eq terminal 'cancelled)
                                  "run.cancelled" "run.completed") 'after-insert)))
      (smoke-arch--postseal-write-case terminal (car spec) (cadr spec))))
  (dolist (target '("run.cancelled" "turn.completed"))
    (smoke-arch--postseal-write-case 'cancelled target 'after-insert t))
  (smoke-arch--postseal-turn-fault-terminal-condition)
  '(("contract" . passed)))

(defun smoke-case-R6 ()
  "borrowed caller Scopes isolate pipeline, async lifecycle, and FIFO work"
  ;; This is deliberately a small real-driver contract rather than a second
  ;; Scope test framework.  Root probes add wider interleaving/terminal
  ;; matrices; R6 keeps the release baseline on the public pipeline seam.
  (smoke--with-fixture
      '((:error (llm-tool-unknown-tool . "R6 unknown tool") :delay 0.05)
        (:text "R6 B" :delay 0.01)
        (:text "R6 A retry" :delay 0.01)
        (:text "R6 nested Sink")
        (:text "R6 nested Event"))
    (let* ((a (superchat-scope-create "smoke-r6-A" superchat-scope-global))
           (b (superchat-scope-create "smoke-r6-B" superchat-scope-global))
           (host (generate-new-buffer " *smoke-r6-host*"))
           (events '(run-started run-sealing run-completed turn-completed))
           (lifecycle nil) (before nil) (stage-errors nil) (handles nil)
           (spawned-sink nil) (spawned-event nil) global-undos)
      (cl-labels
          ((fragment (id text)
             (list (list :owner 'smoke-r6 :id id :slot 'context :text text
                         :priority 0 :budget-class 'hard)))
           (envelope (session &optional sink content)
             (list :surface 'external
                   :content (or content (concat "R6 request " session))
                   :session-id session :tools 'none
                   :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil
                                     :unknown-tool-retries 1)
                   :sink (or sink (superchat-sink-discard))))
           (record (label event run)
             (push (list label event (plist-get run :session-id)) lifecycle))
           (count (label event session)
             (cl-count (list label event session) lifecycle :test #'equal))
           (submit (scope session &optional sink)
             (let* ((request (envelope session sink))
                    (handle (superchat-service-call scope 'driver 'submit request)))
               (push (cons session handle) handles)
               handle))
           (register-lifecycle (scope label)
             (let (undos)
               (dolist (event events)
                 (let ((event event))
                   (push
                    (superchat-on
                     scope event (intern (format "smoke-r6-%s-%s" label event))
                     (lambda (run &rest _) (record label event run)))
                    undos)))
               (nreverse undos)))
           (register-local (scope label marker)
             (let ((superchat-plugin-current-scope scope))
               ;; Same ID shadows only the visible ancestor contributor.  A
               ;; sibling's same ID is never in this Scope chain.
               (superchat-context-register 'smoke-r6-shadow
                (lambda (&rest _) (fragment "smoke-r6-shadow" marker))
                :owner 'smoke-r6)
               (when (equal label "A")
                 (superchat-context-register 'smoke-r6-required-error
                  (lambda (request &rest _)
                    (when (equal (plist-get request :content) "R6 error")
                      (error "R6 scoped required collector failure")))
                  :owner 'smoke-r6 :required t))
               (superchat-on
                nil 'before-provider-request
                (intern (format "smoke-r6-before-%s" label))
                (lambda (spec)
                  (push label before)
                  (plist-put (copy-sequence spec) :system
                             (concat (or (plist-get spec :system) "")
                                     "\nR6_PROVIDER_" label))))
               (register-lifecycle nil label)))
           (request-has (request text)
             (string-match-p (regexp-quote text) (plist-get request :prompt-text))))
        (unwind-protect
            (progn
              ;; Global registrations use their ordinary global layer.  They
              ;; are explicitly undone below; A/B registrations leave only
              ;; through their own borrowed Scope disposal.
              (superchat-context-register 'smoke-r6-base
               (lambda (&rest _) (fragment "smoke-r6-base" "R6_GLOBAL"))
               :owner 'smoke-r6)
              (superchat-context-register 'smoke-r6-shadow
               (lambda (&rest _)
                 (fragment "smoke-r6-shadow" "R6_GLOBAL_SHADOW"))
               :owner 'smoke-r6)
              (push (superchat-on nil 'before-provider-request 'smoke-r6-before-G
                                  (lambda (spec) (push "G" before) spec))
                    global-undos)
              (push (superchat-on nil 'context-error 'smoke-r6-context-error-G
                                  (lambda (&rest _) (push "G" stage-errors)))
                    global-undos)
              (dolist (undo (register-lifecycle nil "G"))
                (push undo global-undos))
              (let ((superchat-plugin-current-scope a))
                (superchat-on nil 'context-error 'smoke-r6-context-error-A
                              (lambda (&rest _) (push "A" stage-errors))))
              (let ((superchat-plugin-current-scope b))
                (superchat-on nil 'context-error 'smoke-r6-context-error-B
                              (lambda (&rest _) (push "B" stage-errors))))
              (register-local a "A" "R6_SHADOW_A")
              (register-local b "B" "R6_SHADOW_B")
              ;; An A-only required collector errors through the public driver.
              ;; The scoped context-error Event must not be observable in B.
              (smoke-arch--assert
               (eq 'superchat-context-required-failed
                   (car (smoke-arch--condition
                         (lambda ()
                           (superchat-service-call
                            a 'driver 'submit
                            (envelope "r6-error" nil "R6 error"))))))
               "R6 scoped required failure reaches pipeline")
              (smoke-arch--assert
               (and (= 1 (cl-count "A" stage-errors :test #'equal))
                    (= 1 (cl-count "G" stage-errors :test #'equal))
                    (zerop (cl-count "B" stage-errors :test #'equal)))
               "R6 stage error does not cross into sibling Scope")
              ;; The unknown-tool error causes a true async LLM retry.  The
              ;; first terminal Sink and the later run-completed Event each
              ;; submit another request with nil Scope; both must inherit A.
              (let ((parent-sink
                     (superchat-sink-create
                      :final
                      (lambda (run _payload)
                        (when (and (equal "r6-A-parent"
                                          (plist-get run :session-id))
                                   (not spawned-sink))
                          (setq spawned-sink t)
                          (submit nil "r6-A-from-sink"))))))
                (superchat-on
                 a 'run-completed 'smoke-r6-spawn-event
                 (lambda (run &rest _)
                   (when (and (equal "r6-A-parent"
                                     (plist-get run :session-id))
                              (not spawned-event))
                     (setq spawned-event t)
                     (submit nil "r6-A-from-event"))))
                (submit a "r6-A-parent" parent-sink)
                (submit b "r6-B-parent"))
              (smoke-arch--assert
               (smoke--wait
                (lambda ()
                  (and (= 4 (length handles))
                       (null (superchat-runtime-active-runs)))) 8)
               "R6 async parents and nested submissions finish")
              (smoke-arch--assert (= 5 (length smoke--requests))
                                  "R6 first attempt, retry, B, and two nested attempts")
              (smoke-arch--assert
               (and (= 4 (cl-count "A" before :test #'equal))
                    (= 1 (cl-count "B" before :test #'equal))
                    (= 5 (cl-count "G" before :test #'equal)))
               "R6 before-provider follows first and retry Scope")
              (dolist (spec '(("r6-A-parent" . "A")
                              ("r6-A-from-sink" . "A")
                              ("r6-A-from-event" . "A")
                              ("r6-B-parent" . "B")))
                (let* ((session (car spec))
                       (label (cdr spec))
                       (request
                        (cl-find-if
                         (lambda (candidate) (request-has candidate session))
                         smoke--requests))
                       (other (if (equal label "A") "B" "A")))
                  (smoke-arch--assert request "R6 finds every native request")
                  (smoke-arch--assert
                   (and (request-has request "R6_GLOBAL")
                        (request-has request (concat "R6_SHADOW_" label))
                        (not (request-has request (concat "R6_SHADOW_" other)))
                        (not (request-has request "R6_GLOBAL_SHADOW")))
                   "R6 same-ID shadow, sibling isolation, and global inheritance")
                  (dolist (event events)
                    (smoke-arch--assert (= 1 (count "G" event session))
                                        "R6 global lifecycle once")
                    (smoke-arch--assert (= 1 (count label event session))
                                        "R6 matching Scope lifecycle once")
                    (smoke-arch--assert (zerop (count other event session))
                                        "R6 sibling lifecycle isolation"))))
              (smoke-arch--assert (and spawned-sink spawned-event)
                                  "R6 Sink and Event nil submissions inherit A")
              (let ((run-id (plist-get (cdr (assoc "r6-A-parent" handles)) :run-id)))
                (smoke-arch--assert
                 (and (not (plist-member (superchat-runtime-status run-id)
                                         :execution-scope))
                      (not (plist-member (superchat-runtime-sink-run run-id)
                                         :execution-scope))
                      ;; Runtime retains historical entries for public status,
                      ;; but terminal quiescence must release its private
                      ;; borrowed reference without disposing the caller Scope.
                      (null (plist-get (superchat-runtime--entry run-id)
                                       :execution-scope))
                      (memq a (superchat-scope-children superchat-scope-global))
                      (memq b (superchat-scope-children superchat-scope-global)))
                 "R6 Scope stays private, releases at quiesce, and remains caller-owned")))
          (dolist (undo global-undos) (when (functionp undo) (funcall undo)))
          (superchat-context-unregister 'smoke-r6-shadow)
          (superchat-context-unregister 'smoke-r6-base)
          (superchat-scope-dispose a)
          (superchat-scope-dispose b))))
  ;; Queue entries are also process-private executions: the timer/remount path
  ;; has no dynamic stack, so it must restore the enqueue caller's Scope.
  (smoke--with-fixture '((:hang t) (:text "R6 queued B") (:text "R6 queued A"))
    (let* ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (a (superchat-scope-create "smoke-r6-queue-A" superchat-scope-global))
           (b (superchat-scope-create "smoke-r6-queue-B" superchat-scope-global))
           (host (generate-new-buffer " *smoke-r6-queue*")))
      (cl-labels
          ((envelope (label)
             (list :surface 'external :content (concat "R6 queue " label)
                   :session-id "smoke-r6-queue" :tools 'none
                   :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil)
                   :sink (superchat-sink-discard)))
           (request-has (request text)
             (string-match-p (regexp-quote text)
                             (plist-get request :prompt-text)))
           (register (scope label)
             (let ((superchat-plugin-current-scope scope))
               (superchat-context-register 'smoke-r6-queue-marker
                (lambda (&rest _)
                  (list (list :owner 'smoke-r6
                              :id "smoke-r6-queue-marker"
                              :slot 'context
                              :text (concat "R6_QUEUE_" label)
                              :priority 0 :budget-class 'hard)))
                :owner 'smoke-r6))))
        (unwind-protect
            (progn
              (register a "A")
              (register b "B")
              (superchat-service-call a 'driver 'submit (envelope "first-A"))
              (superchat-service-call b 'driver 'enqueue-follow-up
                                      "smoke-r6-queue" (envelope "queued-B"))
              (superchat-service-call a 'driver 'enqueue-follow-up
                                      "smoke-r6-queue" (envelope "queued-A"))
              (superchat-plugin-unmount 'transport-llm)
              (smoke-arch--assert (= 2 (superchat-runtime-pending-follow-ups
                                         "smoke-r6-queue"))
                                  "R6 unmount preserves queued inputs")
              (superchat-plugin-mount 'transport-llm)
              (smoke-arch--assert
               (smoke--wait (lambda ()
                              (and (= 3 (length smoke--requests))
                                   (null (superchat-runtime-active-runs)))) 8)
               "R6 queue drains after transport remount")
              (cl-mapc
               (lambda (request label)
                 (smoke-arch--assert
                  (and (request-has request (concat "R6_QUEUE_" label))
                       (not (request-has
                             request
                             (concat "R6_QUEUE_"
                                     (if (equal label "A") "B" "A")))))
                  "R6 queued Scope is restored without sibling bleed"))
               (reverse smoke--requests) '("A" "B" "A"))
              (smoke-arch--assert
               (and (zerop (superchat-runtime-pending-follow-ups "smoke-r6-queue"))
                    (not (member "queue.dropped" (smoke--types "smoke-r6-queue"))))
               "R6 FIFO remains one queue without fabricated drop"))
          (unless (superchat-plugin-state 'transport-llm)
            (superchat-plugin-mount 'transport-llm))
          (superchat-scope-dispose a)
          (superchat-scope-dispose b)))))
  '(("contract" . passed))))

(defun smoke-arch--r7-evidence-extras ()
  "Return a fresh exact-evidence Runtime extras plist for R7.

Keeping this fact in the smoke fixture makes the native tool boundary exercise
the structured evidence path without putting test-only data in an Envelope."
  (list :record-conversation nil
        :evidence-contract "epistemic-learning-v0"
        :context-envelope
        (list :contract "context-continuity-v0" :status 'empty :prompt ""
              :receipt (list :selected nil :represented nil))))

(defun smoke-case-R7 ()
  "scoped product installations, revocation, native callbacks, and MCP refresh"
  (let ((effects-before
         (length (superchat-scope-effects superchat-scope-global))))
    ;; R7 is deliberately one compact production-fixture contract.  The root
    ;; probes carry the wider product matrices; these assertions protect the
    ;; shared registration, Authority, native-callback, and command seams.
    (smoke--with-fixture nil
    (let* ((parent (superchat-scope-create "smoke-r7-parent"
                                           superchat-scope-global))
           (child (superchat-scope-create "smoke-r7-child" parent))
           (sibling (superchat-scope-create "smoke-r7-sibling"
                                            superchat-scope-global))
           (command "smoke-r7-command")
           (tool "smoke-r7-shadow-tool")
           (parent-handler (lambda (&rest _) '(:type :echo :content "parent")))
           (child-handler (lambda (&rest _) '(:type :echo :content "child"))))
      (unwind-protect
          (progn
            ;; Commands and tools share the one Scope layer rule: a child
            ;; shadows its parent, a sibling sees neither, and a duplicate is
            ;; rejected before it changes the active installation.
            (superchat-register-command command parent-handler :scope parent)
            (superchat-register-command command child-handler :scope child)
            (smoke-arch--assert
             (eq child-handler
                 (superchat-service-call child 'commands 'lookup command))
             "R7 child command shadows parent")
            (smoke-arch--assert
             (eq parent-handler
                 (superchat-service-call parent 'commands 'lookup command))
             "R7 parent command remains visible to parent")
            (smoke-arch--assert
             (null (superchat-service-call sibling 'commands 'lookup command))
             "R7 sibling cannot see private command")
            (smoke-arch--assert
             (smoke-arch--condition
              (lambda ()
                (superchat-register-command command #'ignore :scope child)))
             "R7 same-layer command duplicate is atomic")
            (smoke-arch--assert
             (eq child-handler
                 (superchat-service-call child 'commands 'lookup command))
             "R7 failed command duplicate preserves child")
            (superchat-register-tool
             tool :scope parent :function (lambda () "R7_PARENT")
             :description "R7 parent tool" :args nil)
            (superchat-register-tool
             tool :scope child :function (lambda () "R7_CHILD")
             :description "R7 child tool" :args nil)
            (let* ((parent-tool
                    (car (superchat-service-call parent 'tools 'materialize
                                                  (list tool))))
                   (child-tool
                    (car (superchat-service-call child 'tools 'materialize
                                                  (list tool)))))
              (smoke-arch--assert
               (and (equal "R7 parent tool" (llm-tool-description parent-tool))
                    (equal "R7 child tool" (llm-tool-description child-tool))
                    (not (eq (llm-tool-function parent-tool)
                             (llm-tool-function child-tool)))
                    (null (superchat-tools--visible-installation tool sibling))
                    (smoke-arch--condition
                     (lambda ()
                       (superchat-service-call sibling 'tools 'materialize
                                               (list tool)))))
               "R7 tool materialization respects Scope shadow and siblings"))
            (smoke-arch--assert
             (smoke-arch--condition
              (lambda ()
                (superchat-register-tool
                 tool :scope child :function #'ignore :description "duplicate"
                 :args nil)))
             "R7 same-layer tool duplicate is atomic")
            (let* ((stale-name "smoke-r7-stale-command")
                   (old-handler (lambda (&rest _) '(:type :echo :content "old"))))
              (superchat-register-command stale-name old-handler :scope parent)
              (let ((stale
                     (plist-get
                      (superchat--command-local-installation stale-name parent)
                      :undo))
                    (fresh-handler
                     (lambda (&rest _) '(:type :echo :content "fresh"))))
                (superchat-unregister-command stale-name :scope parent)
                (superchat-register-command stale-name fresh-handler :scope parent)
                (funcall stale)
                (smoke-arch--assert
                 (eq fresh-handler
                     (superchat-service-call parent 'commands 'lookup stale-name))
                 "R7 stale command undo cannot withdraw replacement")))
            ;; A nil handler still falls through to a prompt file.  Removing
            ;; the global commands provider must not let a dormant private
            ;; installation suppress that file fallback either.
            (let ((superchat--user-commands (make-hash-table :test #'equal)))
              (make-directory (superchat--command-dir) t)
              (dolist (name '("smoke-r7-file-nil" "smoke-r7-file-absent"))
                (with-temp-file
                    (expand-file-name (concat name ".md") (superchat--command-dir))
                  (insert (concat "R7_TEMPLATE_" name))))
              (superchat-register-command
               "smoke-r7-file-nil" (lambda (&rest _) nil) :scope child)
              (superchat-register-command
               "smoke-r7-file-absent" (lambda (&rest _) '(:type :echo))
               :scope child)
              (let ((superchat-execution-scope child))
                (let ((result (superchat--handle-command
                               "smoke-r7-file-nil" "question"
                               "/smoke-r7-file-nil question")))
                  (smoke-arch--assert
                   (and (eq :llm-query-and-mode-switch (plist-get result :type))
                        (equal "R7_TEMPLATE_smoke-r7-file-nil"
                               (plist-get result :template)))
                   "R7 nil command handler falls through to file")))
              (superchat-plugin-unmount 'chat-commands)
              (unwind-protect
                  (let ((superchat-execution-scope child))
                    (let ((result (superchat--handle-command
                                   "smoke-r7-file-absent" "question"
                                   "/smoke-r7-file-absent question")))
                      (smoke-arch--assert
                       (and (eq :llm-query-and-mode-switch (plist-get result :type))
                            (equal "R7_TEMPLATE_smoke-r7-file-absent"
                                   (plist-get result :template)))
                       "R7 absent commands provider still falls through")))
                (unless (superchat-plugin-state 'chat-commands)
                  (superchat-plugin-mount 'chat-commands)))))
        (unless (superchat-plugin-state 'chat-commands)
          (superchat-plugin-mount 'chat-commands))
        (superchat-scope-dispose sibling)
        (superchat-scope-dispose parent))))
  ;; A `tools/pre-execute' listener and the permission level compose by one
  ;; rule, the stronger of the two.  A listener that allows without delegating
  ;; ends the waterfall only, so full Emacs control is still asked about; a
  ;; listener's deny stands under the widest level; and a write tool's
  ;; escalation arguments never reach anyone once a listener has denied it.
  (smoke--with-fixture '((:hang t))
    (let* ((scope (superchat-scope-create "smoke-r7-levels"
                                          superchat-scope-global))
           (host (generate-new-buffer " *smoke-r7-levels*"))
           (session "smoke-r7-levels")
           (handle nil) (body 0) (asked nil)
           (superchat-approval-function
            (lambda (request) (push (plist-get request :tool-name) asked) t)))
      (unwind-protect
          (cl-flet ((execute (tool &rest args)
                      (superchat-service-call
                       scope 'tools 'execute tool
                       (lambda (&rest _) (cl-incf body) "R7_LEVEL_BODY") args
                       :context
                       (list :session-id session
                             :run-id (plist-get handle :run-id)
                             :turn-id (plist-get handle :turn-id)))))
            (superchat-on
             scope 'tools/pre-execute 'smoke-r7-levels
             (lambda (call _next)
               (if (equal "write_file" (plist-get call :tool-name))
                   (plist-put (plist-put call :kind 'deny) :reason "R7 listener")
                 (plist-put call :kind 'allow))))
            (setq handle
                  (superchat-service-call
                   scope 'driver 'submit
                   (list :surface 'external :content "R7 levels"
                         :session-id session :tools 'none
                         :context (list :host-buffer host)
                         :runtime-extras '(:record-conversation nil)
                         :sink (superchat-sink-discard))))
            (smoke-arch--assert
             (and (equal "R7_LEVEL_BODY" (execute "eval_elisp" "(ignore)"))
                  (equal '("eval_elisp") asked)
                  (= 1 body))
             "R7 an early allow cannot skip the level stage's question")
            (superchat-permission-set session 'danger-full-access)
            (setq asked nil body 0)
            (smoke-arch--assert
             (and (equal "[Tool call denied: R7 listener]"
                         (execute "write_file" "/tmp/smoke-r7" "text"
                                  "danger-full-access" "R7 wants out"))
                  (null asked)
                  (zerop body))
             "R7 a listener's deny is not loosened by the level or by escalation"))
        (when handle
          (ignore-errors
            (superchat-runtime-cancel (plist-get handle :run-id)
                                       "R7 levels cleanup")))
        (superchat-scope-dispose scope))))
  ;; The old descriptor is materialized before it is withdrawn.  Its exact
  ;; proxy must fail closed rather than run OLD or NEW code under the old name;
  ;; the fresh Run demonstrates that normal replacement remains usable.  The
  ;; exact-evidence extras exercise the evidence-native call shape as well.
  (smoke--with-fixture
      '((:tool-uses (("smoke-r7-revoked-tool")) :delay 0.03)
        (:text "R7 old terminal")
        (:tool-uses (("smoke-r7-revoked-tool")))
        (:text "R7 fresh terminal"))
    (let* ((scope (superchat-scope-create "smoke-r7-revocation"
                                          superchat-scope-global))
           (host (generate-new-buffer " *smoke-r7-revocation*"))
           (tool "smoke-r7-revoked-tool")
           (bodies nil) old-handle fresh-handle)
      (cl-labels
          ((register (label)
             (superchat-register-tool
              tool :scope scope :description (concat "R7 " label) :args nil
              :function (lambda () (push label bodies) label)))
           (submit (session)
             (superchat-service-call
              scope 'driver 'submit
              (list :surface 'external :content session :session-id session
                    :tools (list tool) :context (list :host-buffer host)
                    :runtime-extras (smoke-arch--r7-evidence-extras)
                    :sink (superchat-sink-discard)))))
        (unwind-protect
            (progn
              (register "OLD")
              (let ((advertised
                     (car (superchat-service-call scope 'tools 'materialize
                                                  (list tool))))
                    (stale (plist-get (superchat-tools--local-installation
                                       tool scope) :undo)))
                (smoke-arch--assert (and advertised (symbolp (llm-tool-function advertised)))
                                    "R7 materialized tool retains exact proxy")
                (setq old-handle (submit "smoke-r7-revoked-old"))
                (superchat-unregister-tool tool :scope scope)
                (register "NEW")
                (funcall stale))
              (smoke-arch--assert
               (smoke--wait (lambda ()
                              (smoke--run-done-p
                               (plist-get old-handle :run-id))))
               "R7 revoked descriptor Run terminates")
              (smoke-arch--assert (null bodies)
                                  "R7 revoked materialized descriptor starts no body")
              (let ((events (superchat-tape-replay-events "smoke-r7-revoked-old")))
                (smoke-arch--assert
                 (and (zerop (cl-count "tool.execution.started" events
                                       :key (lambda (event) (plist-get event :type))
                                       :test #'equal))
                      (zerop (cl-count "tool.completed" events
                                       :key (lambda (event) (plist-get event :type))
                                       :test #'equal)))
                 "R7 revoked descriptor starts no Effect"))
              (setq bodies nil)
              (setq fresh-handle (submit "smoke-r7-revoked-fresh"))
              (smoke-arch--assert
               (smoke--wait (lambda ()
                              (smoke--run-done-p
                               (plist-get fresh-handle :run-id))))
               "R7 replacement Run terminates")
              (smoke-arch--assert
               (and (equal bodies '("NEW"))
                    (cl-some
                     (lambda (request)
                       (string-match-p "NEW" (plist-get request :prompt-text)))
                     smoke--requests))
               "R7 replacement runs its own body once and returns its result"))
          (superchat-scope-dispose scope)))))
  ;; The native callback happens after the submit dynamic stack has unwound.
  ;; It must recover this tool invocation's Scope for the body and all tool
  ;; Events, then append its real result and complete through the same Run.
  (smoke--with-fixture
      '((:tool-uses (("smoke-r7-async-tool")) :delay 0.01)
        (:text "R7 async terminal"))
    (let* ((scope (superchat-scope-create "smoke-r7-async"
                                          superchat-scope-global))
           (host (generate-new-buffer " *smoke-r7-async*"))
           (tool "smoke-r7-async-tool")
           (pending nil) (bodies nil) (events nil) handle)
      (unwind-protect
          (progn
            (superchat-register-tool
             tool :scope scope :description "R7 native async" :args nil :async t
             :function
             (lambda (callback)
               (push (superchat-scope-name
                      (superchat-execution-scope-resolve nil)) bodies)
               (setq pending callback)))
            (dolist (event '(tool-before tool-after))
              (let ((event event))
                (superchat-on
                 scope event (intern (format "smoke-r7-%s" event))
                 (lambda (tool-name &rest _)
                   (when (equal tool-name tool)
                     (push (list event
                                 (superchat-scope-name
                                  (superchat-execution-scope-resolve nil)))
                           events))
                   nil))))
            (superchat-on
             scope 'tools/pre-execute 'smoke-r7-pre-execute
             (lambda (call next)
               (when (equal tool (plist-get call :tool-name))
                 (push (list 'tools/pre-execute
                             (superchat-scope-name
                              (superchat-execution-scope-resolve nil)))
                       events))
               (funcall next call)))
            (setq handle
                  (superchat-service-call
                   scope 'driver 'submit
                   (list :surface 'external :content "R7 native async"
                         :session-id "smoke-r7-async" :tools (list tool)
                         :context (list :host-buffer host)
                         :runtime-extras '(:record-conversation nil)
                         :sink (superchat-sink-discard))))
            (smoke-arch--assert
             (smoke--wait (lambda () (functionp pending)))
             "R7 native async body enters")
            (smoke-arch--assert (null superchat-execution-scope)
                                "R7 delayed caller has no ambient Scope")
            (funcall pending "R7_NATIVE_RESULT")
            (smoke-arch--assert
             (smoke--wait (lambda ()
                            (smoke--run-done-p (plist-get handle :run-id))))
             "R7 native async Run terminates")
            (smoke-arch--assert
             (and (equal bodies '("smoke-r7-async"))
                  (cl-every
                   (lambda (event) (equal "smoke-r7-async" (cadr event)))
                   events)
                  (= 3 (length events)))
             "R7 native body and tool Events recover original Scope")
            (smoke-arch--assert
             (cl-some
              (lambda (request)
                (string-match-p "R7_NATIVE_RESULT"
                                (plist-get request :prompt-text)))
              smoke--requests)
             "R7 real delayed tool result continues exactly once")
            (let* ((trace (superchat-tape-replay-events "smoke-r7-async"))
                   (run-id (plist-get handle :run-id))
                   (started
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal run-id (plist-get event :run-id))
                            (equal "tool.execution.started"
                                   (plist-get event :type))))
                     trace))
                   (completed
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal run-id (plist-get event :run-id))
                            (equal "tool.completed" (plist-get event :type))))
                     trace))
                   (started-id
                    (plist-get (plist-get (car started) :payload) :execution-id))
                   (completed-id
                    (plist-get (plist-get (car completed) :payload) :execution-id)))
              (smoke-arch--assert
               (and (= 1 (length started))
                    (= 1 (length completed))
                    (stringp started-id)
                    (not (string-empty-p started-id))
                    (equal started-id completed-id))
               "R7 native async result settles its entered Effect once")))
        (superchat-scope-dispose scope))))
  ;; MCP discovery must not revoke an already advertised descriptor when the
  ;; server repeats an identical descriptor.  A changed descriptor still uses
  ;; the ordinary explicit withdrawal/replacement path, covered by root's
  ;; wider MCP matrix.
  (smoke--with-fixture
      '((:tool-uses (("mcp:smoke-r7:lookup")) :delay 0.03)
        (:text "R7 MCP terminal"))
    (require 'superchat-mcp)
    (let* ((scope (superchat-scope-create "smoke-r7-mcp"
                                          superchat-scope-global))
           (host (generate-new-buffer " *smoke-r7-mcp*"))
           (body nil) handle before after
           (descriptor
            (list :name "mcp:smoke-r7:lookup" :category "mcp-smoke-r7"
                  :description "R7 stable MCP lookup" :args nil :async nil
                  :function
                  (lambda ()
                    (push (superchat-scope-name
                           (superchat-execution-scope-resolve nil)) body)
                    "R7_MCP_RESULT")))
           (superchat-mcp-trusted-servers '("smoke-r7")))
      (unwind-protect
          (progn
            (let ((superchat-execution-scope scope))
              (superchat-mcp-register-server-tools "smoke-r7" (list descriptor))
              (setq before
                    (car (superchat-service-call
                          scope 'tools 'materialize '("mcp:smoke-r7:lookup")))))
            (setq handle
                  (superchat-service-call
                   scope 'driver 'submit
                   (list :surface 'external :content "R7 MCP refresh"
                         :session-id "smoke-r7-mcp"
                         :tools '("mcp:smoke-r7:lookup")
                         :context (list :host-buffer host)
                         :runtime-extras '(:record-conversation nil)
                         :sink (superchat-sink-discard))))
            (let ((superchat-execution-scope scope))
              (dotimes (_ 3)
                (superchat-mcp-register-server-tools "smoke-r7" (list descriptor)))
              (setq after
                    (car (superchat-service-call
                          scope 'tools 'materialize '("mcp:smoke-r7:lookup")))))
            (smoke-arch--assert
             (eq (llm-tool-function before) (llm-tool-function after))
             "R7 unchanged MCP refresh retains descriptor identity")
            (smoke-arch--assert
             (smoke--wait (lambda ()
                            (smoke--run-done-p (plist-get handle :run-id))))
             "R7 MCP Run terminates")
            (smoke-arch--assert (equal body '("smoke-r7-mcp"))
                                "R7 already-advertised MCP tool still executes")
            (smoke-arch--assert
             (= 1 (cl-count "tool.completed"
                            (superchat-tape-replay-events "smoke-r7-mcp")
                            :key (lambda (event) (plist-get event :type))
                            :test #'equal))
             "R7 MCP result is audited once"))
        (superchat-scope-dispose scope))))
    ;; Product registrations already prove their own exact undo paths above.
    ;; This guards the plugin-fiber lifetime effect that owns `chat-commands'
    ;; while the command fallback fixture unmounts and remounts it.
    (smoke-arch--assert
     (= effects-before
        (length (superchat-scope-effects superchat-scope-global)))
     "R7 repeat leaves global Scope effects at its entry baseline"))
  '(("contract" . passed)))

;;;; Preset composition and ownership -----------------------------------------

;; These are global plugin *definitions*, but every R9 activation mounts them
;; only in a fresh Preset Scope.  Dynamic test variables avoid retaining a
;; particular fixture through a definition on repeated smoke invocation.
(defvar smoke-arch--r9-late-callback nil)
(defvar smoke-arch--r9-late-old-calls 0)
(defvar smoke-arch--r9-late-new-calls 0)
(defvar smoke-arch--r9-late-deliveries 0)
(defvar smoke-arch--r9-late-order nil)

(superchat-define-plugin smoke-r9-marker
  :config '((:label stringp "DEFAULT"))
  :apply
  (lambda (_scope config)
    (let ((label (plist-get config :label)))
      (superchat-register-command
       "smoke-r9-label" (lambda (&rest _) (list :type :echo :content label)))
      (superchat-context-register 'smoke-r9-marker
       (lambda (&rest _)
         (list (list :owner 'smoke-r9 :id "smoke-r9-marker" :slot 'context
                     :text (concat "R9-MARKER-" label)
                     :priority 0 :budget-class 'hard)))
       :owner 'smoke-r9))))

(superchat-define-plugin smoke-r9-late-old
  :apply
  (lambda (scope _config)
    (superchat-register-tool
     "smoke-r9-late-tool" :async t :description "R9 pending external effect"
     :function (lambda (callback)
                 (cl-incf smoke-arch--r9-late-old-calls)
                 (setq smoke-arch--r9-late-callback callback)))
    ;; This is registered after the tool, so terminal Sink delivery happens
    ;; before disposal reaches this plugin cleanup.
    (superchat-scope-effect
     scope (lambda () (push 'unload smoke-arch--r9-late-order)))))

(superchat-define-plugin smoke-r9-late-new
  :apply
  (lambda (_scope _config)
    (superchat-register-tool
     "smoke-r9-late-tool" :async t :description "R9 replacement effect"
     :function (lambda (callback)
                 (cl-incf smoke-arch--r9-late-new-calls)
                 (funcall callback "R9 replacement")))))

(superchat-define-plugin smoke-r9-fail
  :apply
  (lambda (_scope _config)
    (superchat-register-command "smoke-r9-leak" #'ignore)
    (error "R9 candidate apply failure")))

(superchat-define-plugin smoke-r9-pending
  :inject '(smoke-r9-needed)
  :apply
  (lambda (_scope _config)
    (superchat-register-command
     "smoke-r9-pending" (lambda (&rest _) (list :type :echo :content "ready")))))

(defun smoke-arch--r9-preset (name plugins &optional body)
  "Return a small agent Preset NAME with scoped PLUGINS for R9."
  (superchat-preset-from-plist
   (list :name name :profile-id name :type 'agent :capability "code"
         :body (or body (concat "R9-PERSONA-" name))
         :tools 'none :plugins plugins)))

(defun smoke-arch--r9-no-scope-p (value &optional seen)
  "Return non-nil when public VALUE contains no process-local Scope object."
  (cond
   ((superchat-scope-p value) nil)
   ((memq value seen) t)
   ((consp value)
    (and (smoke-arch--r9-no-scope-p (car value) (cons value seen))
         (smoke-arch--r9-no-scope-p (cdr value) (cons value seen))))
   ((vectorp value)
    (cl-every (lambda (item)
                (smoke-arch--r9-no-scope-p item (cons value seen)))
              (append value nil)))
   (t t)))

(defun smoke-arch--r9-composition ()
  "Exercise per-View composition, producer routing, FIFO, and restoration."
  (smoke--with-fixture
      '((:text "R9 plain A") (:hang t) (:text "R9 fresh B")
        (:text "R9 explicit skill"))
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-runtime--queue-sequence 0))
      (let* ((view (smoke--open-chat))
             (other (superchat-new-session))
             (session (buffer-local-value 'superchat--session-id view))
             (a (smoke-arch--r9-preset
                 "r9-same" '((smoke-r9-marker :label "A"))))
             (b (smoke-arch--r9-preset
                 "r9-fresh" '((smoke-r9-marker :label "B"))))
             (other-preset (smoke-arch--r9-preset "r9-same" '(smoke-r9-marker)))
             scope-a scope-b scope-other plain old fresh skill-run)
        (with-current-buffer view
          (superchat--activate-capability a)
          (setq scope-a superchat--active-preset-scope))
        (with-current-buffer other
          (superchat--activate-capability other-preset)
          (setq scope-other superchat--active-preset-scope))
        (smoke-arch--assert
         (and (not (eq scope-a scope-other))
              (eq (superchat-scope-parent scope-a) superchat-scope-global)
              (eq (superchat-scope-parent scope-other) superchat-scope-global)
              (equal "A" (with-current-buffer view
                            (plist-get (superchat--handle-command
                                        "smoke-r9-label" nil "") :content)))
              (equal "DEFAULT" (with-current-buffer other
                                  (plist-get (superchat--handle-command
                                              "smoke-r9-label" nil "") :content)))
              (null (superchat-service-call superchat-scope-global
                                             'commands 'lookup "smoke-r9-label")))
         "R9 Preset composition shadows by exact View Scope")
        (setq plain (smoke--send view "R9 plain producer"))
        (smoke-arch--assert
         (and (eq scope-a
                  (plist-get (superchat-runtime--entry plain) :execution-scope))
              (smoke--wait (lambda () (smoke--run-done-p plain)))
              (string-match-p "R9-MARKER-A"
                              (plist-get (car smoke--requests) :prompt-text)))
         "R9 plain producer uses its active Preset Scope")
        (setq old (smoke--send view "R9 queued old producer"))
        (smoke--wait (lambda () (eq 'running-model
                                  (plist-get (superchat-runtime-status old) :status))))
        (smoke--send view "R9 FIFO old producer")
        (smoke-arch--assert (= 1 (superchat-runtime-pending-follow-ups session))
                            "R9 chat FIFO captures an old Preset request")
        (with-current-buffer view
          (superchat--activate-capability b)
          (setq scope-b superchat--active-preset-scope))
        (smoke-arch--assert
         (and (eq 'cancelled (plist-get (superchat-runtime-status old) :status))
              (superchat-execution-scope-retired-p scope-a)
              (not (eq scope-a scope-b))
              (eq scope-other (buffer-local-value 'superchat--active-preset-scope other))
              (smoke--wait (lambda ()
                             (zerop (superchat-runtime-pending-follow-ups session)))
                           2)
              (= 1 (cl-count "queue.dropped" (smoke--types session) :test #'equal))
              (= 2 (length smoke--requests)))
         "R9 switch cancels old work and FIFO never retargets it")
        (setq fresh (smoke--send view "R9 fresh producer"))
        (smoke-arch--assert
         (and (eq scope-b
                  (plist-get (superchat-runtime--entry fresh) :execution-scope))
              (smoke--wait (lambda () (smoke--run-done-p fresh)))
              (string-match-p "R9-MARKER-B"
                              (plist-get (car smoke--requests) :prompt-text)))
         "R9 replacement Preset owns only its fresh producer")
        ;; Recovery must reuse the live Scope and avoid duplicate configuration
        ;; facts, fibers, or effects.
        (let ((counts (list (length (superchat-scope-children superchat-scope-global))
                            (length superchat-plugin--fibers)
                            (length (superchat-scope-effects scope-b))
                            (cl-count "capability.activated" (smoke--types session)
                                      :test #'equal))))
          (cl-letf (((symbol-function 'superchat--resolve-profile-preset)
                     (lambda (&rest _) (superchat-preset-copy b))))
            (dotimes (_ 4)
              (with-current-buffer view
                (superchat--restore-session-runtime-configuration session))))
          (smoke-arch--assert
           (and (eq scope-b (buffer-local-value 'superchat--active-preset-scope view))
                (equal counts
                       (list (length (superchat-scope-children superchat-scope-global))
                             (length superchat-plugin--fibers)
                             (length (superchat-scope-effects scope-b))
                             (cl-count "capability.activated" (smoke--types session)
                                       :test #'equal))))
           "R9 restore reuses its live Preset Scope exactly"))
        (make-directory superchat-skills-directory t)
        (with-temp-file (expand-file-name "smoke-r9-skill.md"
                                           superchat-skills-directory)
          (insert "---\nname: smoke-r9-skill\ntype: agent\n"
                  "profile_id: smoke-r9-skill\ncapability: code\n"
                  "plugins: [smoke-r9-marker]\ntools: []\n---\n"
                  "R9-SKILL-PERSONA\n"))
        (setq skill-run (smoke--send view ">smoke-r9-skill R9 explicit producer"))
        (let ((scope-skill (buffer-local-value 'superchat--active-preset-scope view)))
          (smoke-arch--assert
           (and (not (eq scope-b scope-skill))
                (eq scope-skill
                    (plist-get (superchat-runtime--entry skill-run) :execution-scope))
                (smoke--wait (lambda () (smoke--run-done-p skill-run)))
                (string-match-p "R9-MARKER-DEFAULT"
                                (plist-get (car smoke--requests) :prompt-text))
                (string-match-p "R9-SKILL-PERSONA"
                                (plist-get (car smoke--requests) :prompt-text)))
           "R9 explicit skill deliberately submits through its fresh Scope"))
        (smoke-arch--assert
         (and (smoke-arch--r9-no-scope-p (superchat-runtime-status fresh))
              (smoke-arch--r9-no-scope-p (superchat-tape-replay-events session)))
         "R9 Preset Scope stays out of public status and Tape")))))

(defun smoke-arch--r9-late-effect ()
  "Verify Preset switch preserves a true late external Effect fact once."
  (smoke--with-fixture
      '((:tool-uses (("smoke-r9-late-tool")) :delay 0.01)
        (:text "R9 late callback must not resume the model"))
    (let ((smoke-arch--r9-late-callback nil)
          (smoke-arch--r9-late-old-calls 0)
          (smoke-arch--r9-late-new-calls 0)
          (smoke-arch--r9-late-deliveries 0)
          (smoke-arch--r9-late-order nil))
      (let* ((view (smoke--open-chat))
             (host (generate-new-buffer " *smoke-r9-late*"))
             (session "smoke-r9-late")
             (old-preset
              (smoke-arch--r9-preset
               "r9-late-old" '(smoke-r9-marker smoke-r9-late-old)))
             (new-preset
              (smoke-arch--r9-preset
               "r9-late-new" '(smoke-r9-marker smoke-r9-late-new)))
             old-scope handle)
        (with-current-buffer view
          (superchat--activate-capability old-preset)
          (setq old-scope superchat--active-preset-scope))
        (setq handle
              (superchat-service-call
               old-scope 'driver 'submit
               (list :surface 'external :content "R9 late external Effect"
                     :session-id session :tools '("smoke-r9-late-tool")
                     :context (list :host-buffer host)
                     :runtime-extras '(:record-conversation nil)
                     :sink (superchat-sink-create
                            :error (lambda (&rest _)
                                     (cl-incf smoke-arch--r9-late-deliveries)
                                     (push 'cancel smoke-arch--r9-late-order))))))
        (smoke-arch--assert
         (smoke--wait (lambda () (functionp smoke-arch--r9-late-callback)))
         "R9 old asynchronous tool actually entered")
        (with-current-buffer view (superchat--activate-capability new-preset))
        (let ((run-id (plist-get handle :run-id)))
          (smoke-arch--assert
           (and (eq 'audit-incomplete
                    (plist-get (superchat-runtime-status run-id) :status))
                (superchat-execution-scope-retired-p old-scope)
                (null (superchat-plugin-fiber 'smoke-r9-late-old old-scope))
                (equal '(cancel unload) (nreverse smoke-arch--r9-late-order))
                (let* ((trace (superchat-tape-replay-events session))
                       (started
                        (cl-remove-if-not
                         (lambda (event)
                           (and (equal run-id (plist-get event :run-id))
                                (equal "tool.execution.started"
                                       (plist-get event :type))))
                         trace))
                       (execution-id
                        (plist-get (plist-get (car started) :payload)
                                   :execution-id))
                       (settled-ids
                        (mapcar
                         (lambda (event)
                           (plist-get (plist-get event :payload) :execution-id))
                         (cl-remove-if-not
                          (lambda (event)
                            (and (equal run-id (plist-get event :run-id))
                                 (member (plist-get event :type)
                                         '("tool.completed" "tool.failed"))))
                          trace))))
                  (and (= 1 (length started))
                       (stringp execution-id)
                       (not (equal "" execution-id))
                       (not (member execution-id settled-ids)))))
           "R9 retires before old plugin withdrawal without faking an Effect")
          (funcall smoke-arch--r9-late-callback "R9 actual external result")
          (funcall smoke-arch--r9-late-callback "R9 duplicate result")
          (let* ((trace (superchat-tape-replay-events session))
                 (run-events
                  (cl-remove-if-not
                   (lambda (event) (equal run-id (plist-get event :run-id)))
                   trace))
                 (started
                  (cl-remove-if-not
                   (lambda (event)
                     (equal "tool.execution.started" (plist-get event :type)))
                   run-events))
                 (completed
                  (cl-remove-if-not
                   (lambda (event)
                     (equal "tool.completed" (plist-get event :type)))
                   run-events))
                 (failed
                  (cl-remove-if-not
                   (lambda (event)
                     (equal "tool.failed" (plist-get event :type)))
                   run-events))
                 (started-id
                  (plist-get (plist-get (car started) :payload) :execution-id))
                 (completed-id
                  (plist-get (plist-get (car completed) :payload) :execution-id))
                 (failed-ids
                  (mapcar
                   (lambda (event)
                     (plist-get (plist-get event :payload) :execution-id))
                   failed))
                 (run-terminals
                  (cl-remove-if-not
                   (lambda (event)
                     (member (plist-get event :type)
                             '("run.completed" "run.failed" "run.cancelled")))
                   run-events))
                 (turn-terminals
                  (cl-remove-if-not
                   (lambda (event)
                     (equal "turn.completed" (plist-get event :type)))
                   run-events)))
            (smoke-arch--assert
             (and (= 1 smoke-arch--r9-late-old-calls)
                  (zerop smoke-arch--r9-late-new-calls)
                  (= 1 smoke-arch--r9-late-deliveries)
                  (= 1 (length started))
                  (= 1 (length completed))
                  (equal started-id completed-id)
                  (not (member started-id failed-ids))
                  (= 1 (length run-terminals))
                  (= 1 (length turn-terminals))
                  (= 1 (length smoke--requests))
                  (eq 'audit-incomplete
                      (plist-get (superchat-runtime-status run-id) :status))
                  (smoke-arch--r9-no-scope-p handle)
                  (smoke-arch--r9-no-scope-p
                   (superchat-runtime-status run-id))
                  (smoke-arch--r9-no-scope-p trace))
             "R9 late external fact is truthful once and cannot revive its Run")))))))

(defun smoke-arch--r9-failure-and-pending ()
  "Keep old work on candidate failure and release through Runtime without Driver."
  (smoke--with-fixture '((:hang t) (:hang t))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (host (generate-new-buffer " *smoke-r9-owner*"))
           (old-preset (smoke-arch--r9-preset "r9-old" '(smoke-r9-marker)))
           (pending-preset
            (smoke-arch--r9-preset
             "r9-pending" '(smoke-r9-marker smoke-r9-pending)))
           old-scope pending-scope old-run pending-run provider-undo)
      (superchat-define-service 'smoke-r9-needed '(ping) "R9 pending service")
      (unwind-protect
          (progn
            (with-current-buffer view
              (superchat--activate-capability old-preset)
              (setq old-scope superchat--active-preset-scope))
            (setq old-run
                  (superchat-service-call
                   old-scope 'driver 'submit
                   (list :surface 'external :content "R9 old survives failure"
                         :session-id session :tools 'none
                         :context (list :host-buffer host)
                         :runtime-extras '(:record-conversation nil)
                         :sink (superchat-sink-discard))))
            (let ((children (copy-sequence
                             (superchat-scope-children superchat-scope-global))))
              (smoke-arch--assert
               (and (smoke-arch--condition
                     (lambda ()
                       (with-current-buffer view
                         (superchat--activate-capability
                          (smoke-arch--r9-preset
                           "r9-failed" '(smoke-r9-marker smoke-r9-fail))))))
                    (eq old-scope
                        (buffer-local-value 'superchat--active-preset-scope view))
                    (not (superchat-execution-scope-retired-p old-scope))
                    (eq 'running-model
                        (plist-get (superchat-runtime-status
                                    (plist-get old-run :run-id)) :status))
                    (equal children
                           (superchat-scope-children superchat-scope-global)))
               "R9 failed candidate is atomic and preserves old execution"))
            (with-current-buffer view
              (superchat--activate-capability pending-preset)
              (setq pending-scope superchat--active-preset-scope))
            (smoke-arch--assert
             (eq 'pending (superchat-plugin-state 'smoke-r9-pending pending-scope))
             "R9 missing Service leaves a legal pending Preset plugin")
            (setq provider-undo
                  (superchat-service-provide
                   superchat-scope-global 'smoke-r9-needed
                   (superchat-service-table ping #'ignore)))
            (smoke-arch--assert
             (and (eq 'active
                      (superchat-plugin-state 'smoke-r9-pending pending-scope))
                  (equal "ready"
                         (with-current-buffer view
                           (plist-get (superchat--handle-command
                                       "smoke-r9-pending" nil "") :content))))
             "R9 pending Preset plugin activates when its Service appears")
            (setq pending-run
                  (superchat-service-call
                   pending-scope 'driver 'submit
                   (list :surface 'external :content "R9 driver absent release"
                         :session-id "smoke-r9-driver-absent" :tools 'none
                         :context (list :host-buffer host)
                         :runtime-extras '(:record-conversation nil)
                         :sink (superchat-sink-discard))))
            (superchat-plugin-unmount 'driver superchat-scope-global)
            (with-current-buffer view
              (superchat--activate-capability nil "chat"))
            (smoke-arch--assert
             (and (null (superchat-service-get nil 'driver))
                  (eq 'cancelled
                      (plist-get (superchat-runtime-status
                                  (plist-get pending-run :run-id)) :status))
                  (superchat-execution-scope-retired-p pending-scope)
                  (null (superchat-plugin-fiber 'smoke-r9-pending pending-scope)))
             "R9 owner retires exact work even while Driver is absent"))
        (when provider-undo (funcall provider-undo))
        (unless (superchat-plugin-fiber 'driver superchat-scope-global)
          (superchat-plugin-mount 'driver))))))

(defun smoke-arch--r9-view-stop-and-repeat ()
  "Exercise mode-off/kill owner release and repeat-safe activation cleanup."
  (smoke--with-fixture '((:hang t) (:hang t))
    (let* ((view (smoke--open-chat))
           (host (generate-new-buffer " *smoke-r9-host*"))
           (preset (smoke-arch--r9-preset "r9-stop" '(smoke-r9-marker)))
           scope first second)
      (with-current-buffer view
        (superchat--activate-capability preset)
        (setq scope superchat--active-preset-scope))
      (setq first
            (superchat-service-call
             scope 'driver 'submit
             (list :surface 'external :content "R9 mode off" :session-id "smoke-r9-stop-1"
                   :tools 'none :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil)
                   :sink (superchat-sink-discard))))
      (with-current-buffer view (superchat-mode -1))
      (smoke-arch--assert
       (and (eq 'cancelled
                (plist-get (superchat-runtime-status (plist-get first :run-id)) :status))
            (superchat-execution-scope-retired-p scope)
            (not (memq scope (superchat-scope-children superchat-scope-global)))
            (buffer-live-p host))
       "R9 mode off retires only its Preset execution and keeps buffers")
      (with-current-buffer view
        (superchat-mode 1)
        (superchat--activate-capability preset)
        (setq scope superchat--active-preset-scope))
      (setq second
            (superchat-service-call
             scope 'driver 'submit
             (list :surface 'external :content "R9 kill" :session-id "smoke-r9-stop-2"
                   :tools 'none :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil)
                   :sink (superchat-sink-discard))))
      (kill-buffer view)
      (smoke-arch--assert
       (and (eq 'cancelled
                (plist-get (superchat-runtime-status (plist-get second :run-id)) :status))
            (superchat-execution-scope-retired-p scope)
            (not (memq scope (superchat-scope-children superchat-scope-global)))
            (buffer-live-p host))
       "R9 approved View kill retires its exact Preset execution")
      (let* ((cycles (superchat-new-session))
             (children (copy-sequence (superchat-scope-children
                                       superchat-scope-global)))
             (effects (length (superchat-scope-effects superchat-scope-global)))
             (fibers (length superchat-plugin--fibers)))
        (dotimes (_ 100)
          (let (activated)
            (with-current-buffer cycles
              (superchat--activate-capability preset)
              (setq activated superchat--active-preset-scope)
              (superchat--activate-capability nil "chat"))
            (smoke-arch--assert
             (and (null (superchat-scope-children activated))
                  (null (superchat-scope-effects activated)))
             "R9 disposed Preset activation retains no private effects")))
        (smoke-arch--assert
         (and (equal children (superchat-scope-children superchat-scope-global))
              (= effects (length (superchat-scope-effects superchat-scope-global)))
              (= fibers (length superchat-plugin--fibers)))
         "R9 100 activation cycles leave global ownership stable")))))

(defun smoke-arch--r9-producer-regressions ()
  "Keep the three producer/lifecycle regressions fixed during Preset work."
  ;; A normal request may run arbitrary `:pre' code, including a View-local
  ;; Preset switch.  Its already-prepared old Turn must reject, not be routed
  ;; through the newly active Scope.
  (smoke--with-fixture '((:text "R9 stale pre must not reach a provider"))
    (let* ((view (smoke--open-chat))
           (old (smoke-arch--r9-preset "r9-pre-old" '(smoke-r9-marker)))
           (fresh (smoke-arch--r9-preset "r9-pre-fresh" '(smoke-r9-marker)))
           old-scope pre-scope condition)
      (setf (superchat-preset-pre old)
            (lambda ()
              (setq pre-scope (superchat-execution-scope-resolve))
              (superchat--activate-capability fresh)))
      (with-current-buffer view
        (superchat--activate-capability old)
        (setq old-scope superchat--active-preset-scope))
      (setq condition
            (smoke-arch--condition
             (lambda () (smoke--send view "R9 ordinary pre switch"))))
      (smoke-arch--assert
       (and condition
            (eq old-scope pre-scope)
            (eq fresh (buffer-local-value 'superchat--active-preset view))
            (superchat-execution-scope-retired-p old-scope)
            (null smoke--requests))
       "R9 ordinary pre switch cannot reroute an old prepared request")))
  ;; `/plan ARGS' intentionally differs: the command is looked up under its
  ;; old Scope, but its returned mode-switch request must use the new one.
  (smoke--with-fixture '((:hang t) (:text "R9 planning response"))
    (let* ((view (smoke--open-chat))
           (old (smoke-arch--r9-preset "r9-plan-old" '(smoke-r9-marker)))
           old-scope old-run new-run condition)
      (with-current-buffer view
        (superchat--activate-capability old)
        (setq old-scope superchat--active-preset-scope))
      (setq old-run (smoke--send view "R9 old planning work"))
      (setq condition
            (smoke-arch--condition
             (lambda ()
               (setq new-run (smoke--send view "/plan make a plan")))))
      (smoke-arch--assert
       (and (null condition)
            (eq 'cancelled
                (plist-get (superchat-runtime-status old-run) :status))
            (superchat-execution-scope-retired-p old-scope)
            new-run
            (smoke--wait (lambda () (smoke--run-done-p new-run)))
            (eq 'completed
                (plist-get (superchat-runtime-status new-run) :status))
            (= 2 (length smoke--requests)))
       "R9 /plan arguments switch Scope and submit their new planning turn")))
  ;; Changing major mode removes the minor mode without executing its `-1'
  ;; body.  The local major-mode hook remains the exact owner release seam.
  (smoke--with-fixture '((:hang t))
    (let* ((view (smoke--open-chat))
           (host (generate-new-buffer " *smoke-r9-major-host*"))
           (preset (smoke-arch--r9-preset "r9-major" '(smoke-r9-marker)))
           scope handle)
      (with-current-buffer view
        (superchat--activate-capability preset)
        (setq scope superchat--active-preset-scope))
      (setq handle
            (superchat-service-call
             scope 'driver 'submit
             (list :surface 'external :content "R9 major-mode release"
                   :session-id "smoke-r9-major" :tools 'none
                   :context (list :host-buffer host)
                   :runtime-extras '(:record-conversation nil)
                   :sink (superchat-sink-discard))))
      (with-current-buffer view (fundamental-mode))
      (smoke-arch--assert
       (and (eq 'cancelled
                (plist-get (superchat-runtime-status
                            (plist-get handle :run-id)) :status))
            (superchat-execution-scope-retired-p scope)
            (not (memq scope (superchat-scope-children superchat-scope-global)))
            (null (superchat-plugin-fiber 'smoke-r9-marker scope))
            (buffer-live-p view)
            (buffer-live-p host))
       "R9 implicit major-mode exit retires only its owning Preset Scope"))))

(defun smoke-case-R9 ()
  "Preset composition, owner switch, producer routing, and cleanup remain scoped."
  ;; Root's wider matrix owns detailed callback/recovery permutations.  This
  ;; compact production fixture guards the delivered cross-module owner seams.
  (let ((children-before
         (sort (mapcar #'superchat-scope-name
                       (superchat-scope-children superchat-scope-global))
               #'string<))
        (effects-before (length (superchat-scope-effects superchat-scope-global)))
        (fibers-before (length superchat-plugin--fibers)))
    (smoke-arch--r9-composition)
    (smoke-arch--r9-late-effect)
    (smoke-arch--r9-failure-and-pending)
    (smoke-arch--r9-view-stop-and-repeat)
    (smoke-arch--r9-producer-regressions)
    (smoke-arch--assert
     ;; The driver-absent contract deliberately unmounts/remounts the global
     ;; driver fiber, so object identity is not stable here.  Registration
     ;; keys/counts are: no Preset child may survive the fixture.
     (and (equal children-before
                 (sort (mapcar #'superchat-scope-name
                               (superchat-scope-children superchat-scope-global))
                       #'string<))
          (= effects-before (length (superchat-scope-effects superchat-scope-global)))
          (= fibers-before (length superchat-plugin--fibers)))
     "R9 repeated fixture leaves no global Scope or fiber residue"))
  '(("contract" . passed)))
