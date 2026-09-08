;;; test-stage.el --- ERT tests for superchat-stage.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-stage)

(defmacro superchat-stage-test--isolated (&rest body)
  "Run BODY, then restore `superchat-stage--registry' and
`superchat-stage-faults' to their pre-BODY state.  Lets a test freely
`superchat-stage-add'/`superchat-stage-remove'/`superchat-stage--define'
without leaking into later tests or into the thirteen fixed stages'
production registrations (such as `superchat-prompt.el's assemble-prompt
implementation, when it happens to be loaded in the same process)."
  (declare (indent 0))
  `(let ((superchat-stage-test--snapshot
          (let (acc)
            (maphash (lambda (k v) (push (cons k (copy-tree v)) acc))
                     superchat-stage--registry)
            acc))
         (superchat-stage-test--faults superchat-stage-faults))
     (unwind-protect
         (progn ,@body)
       (clrhash superchat-stage--registry)
       (dolist (pair superchat-stage-test--snapshot)
         (puthash (car pair) (cdr pair) superchat-stage--registry))
       (setq superchat-stage-faults superchat-stage-test--faults))))

(defun stage-test--clear-implementations (name)
  "Remove every registered implementation of stage NAME.
For tests that exercise a fixed stage with real production implementations
already registered on it (`admit', `collect-context') when this file loads
alongside `superchat-dispatcher.el'/`superchat-prompt-hooks.el' in a
combined run -- callers wrap this in `superchat-stage-test--isolated',
which restores the original registry, production implementations
included, once the test body finishes."
  (let ((entry (superchat-stage--entry name)))
    (puthash name (plist-put (copy-tree entry) :implementations nil)
             superchat-stage--registry)))

(ert-deftest stage/thirteen-fixed-stages-exist-in-order ()
  "The thirteen fixed stages exist, in contract §4 order."
  (should (equal superchat-stage-names
                 '(resolve-session admit load-state select-tools
                   collect-context assemble-prompt run-model save-state
                   render dispatch on-error register-command
                   register-surface)))
  (should (= 13 (length superchat-stage-names)))
  (dolist (name superchat-stage-names)
    (should (superchat-stage--entry name))))

(ert-deftest stage/add-rejects-unknown-duplicate-and-core-only ()
  "`superchat-stage-add' rejects every malformed registration in §4.1."
  (superchat-stage-test--isolated
    (should-error (superchat-stage-add 'not-a-stage 'x #'ignore :owner 'test))
    (should-error (superchat-stage-add 'assemble-prompt 'x #'ignore :owner 'plugin))
    (should (superchat-stage-add 'assemble-prompt 'core-x #'ignore :owner 'core))
    (should (superchat-stage-add 'load-state 'dup #'ignore :owner 'test))
    (should-error (superchat-stage-add 'load-state 'dup #'ignore :owner 'other))
    (should-error (superchat-stage-add 'load-state 'notfn "nope" :owner 'test))
    (should-error
     (superchat-stage-add 'admit 'req-on-non-collect #'ignore :owner 'test :required t))))

(ert-deftest stage/required-accepts-only-nil-or-t ()
  "`superchat-stage-add' rejects a non-boolean `:required' (S03a-fix-2
finding 7); nil and t both succeed on a `collect' stage."
  (superchat-stage-test--isolated
    (should (superchat-stage-add 'render 'req-nil #'ignore :owner 'test :required nil))
    (should (superchat-stage-add 'render 'req-t #'ignore :owner 'test :required t))
    (should-error (superchat-stage-add 'render 'req-string #'ignore :owner 'test :required "yes"))
    (should-error (superchat-stage-add 'render 'req-number #'ignore :owner 'test :required 1))))

(ert-deftest stage/later-registration-runs-first ()
  "For `first-result', the most recently registered implementation wins."
  (superchat-stage-test--isolated
    (superchat-stage-add 'resolve-session 'first (lambda (&rest _) "first") :owner 'test)
    (superchat-stage-add 'resolve-session 'second (lambda (&rest _) "second") :owner 'test)
    (should (equal "second" (superchat-stage-call 'resolve-session '(:content "x"))))))

(ert-deftest stage/first-result-skips-errors-and-records-fault ()
  "A `first-result' implementation's `error' is skipped and faulted, not
propagated; the next candidate still wins."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (superchat-stage-add 'select-tools 'ok (lambda (&rest _) '("read_buffer")) :owner 'test)
    (superchat-stage-add 'select-tools 'boom (lambda (&rest _) (error "boom")) :owner 'test)
    (should (equal '("read_buffer") (superchat-stage-call 'select-tools nil nil)))
    (should (= 1 (length superchat-stage-faults)))
    (should (eq 'select-tools (plist-get (car superchat-stage-faults) :stage)))
    (should (eq 'boom (plist-get (car superchat-stage-faults) :id)))
    (should (eq 'test (plist-get (car superchat-stage-faults) :owner)))))

(ert-deftest stage/collect-preserves-order-and-drops-invalid ()
  "`collect' keeps every valid non-nil result in invocation (newest-first)
order and drops a nil result and an erroring implementation."
  (superchat-stage-test--isolated
    (superchat-stage-add 'render 'c1 (lambda (&rest _) "r1") :owner 'test)
    (superchat-stage-add 'render 'c2 (lambda (&rest _) nil) :owner 'test)
    (superchat-stage-add 'render 'c3 (lambda (&rest _) (error "bad")) :owner 'test)
    (superchat-stage-add 'render 'c4 (lambda (&rest _) "r4") :owner 'test)
    (should (equal '("r4" "r1") (superchat-stage-call 'render)))))

(ert-deftest stage/fold-threads-and-finishes ()
  "`fold' threads the accumulator newest-registration-first and
`(:finish V)' short-circuits before an older implementation runs.  No fixed
stage in this revision uses `fold'; this declares a temporary one."
  (superchat-stage-test--isolated
    (superchat-stage--define 'stage-test--fold :kind 'fold)
    (superchat-stage-add 'stage-test--fold 'add (lambda (acc x) (+ acc x)) :owner 'test)
    (superchat-stage-add 'stage-test--fold 'finish
                          (lambda (acc _x) (list :finish (* acc 10))) :owner 'test)
    (superchat-stage-add 'stage-test--fold 'never (lambda (acc x) (+ acc x)) :owner 'test)
    ;; Newest-first order is never, finish, add: `never' adds 10 (acc 11),
    ;; `finish' short-circuits at 110, `add' never runs.
    (should (= 110 (superchat-stage-call 'stage-test--fold 1 10)))
    ;; Without `finish', `never' then `add' both run: 1 -> 11 -> 21.
    (superchat-stage-remove 'stage-test--fold 'finish)
    (should (= 21 (superchat-stage-call 'stage-test--fold 1 10)))))

(ert-deftest stage/first-result-validator-rejection-and-signal-are-faulted ()
  "A `first-result' VALIDATOR that rejects (returns nil), or that itself
signals, is recorded as a fault for the offending implementation and the
call falls through to the next candidate -- exactly like an implementation
`error' (S03a-fix-2 finding 6)."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (superchat-stage--define 'stage-test--validated :kind 'first-result
                              :validator (lambda (r) (if (eq r 'boom) (error "validator boom") (> r 0))))
    (superchat-stage-add 'stage-test--validated 'fallback (lambda () 1) :owner 'test)
    (superchat-stage-add 'stage-test--validated 'signals (lambda () 'boom) :owner 'test)
    (superchat-stage-add 'stage-test--validated 'rejected (lambda () -1) :owner 'test)
    (should (= 1 (superchat-stage-call 'stage-test--validated)))
    (should (= 2 (length superchat-stage-faults)))
    (should (cl-find 'rejected superchat-stage-faults :key (lambda (f) (plist-get f :id))))
    (should (cl-find 'signals superchat-stage-faults :key (lambda (f) (plist-get f :id))))))

(ert-deftest stage/collect-validator-rejection-and-signal-are-faulted ()
  "A `collect' VALIDATOR that rejects or signals drops that one result and
records a fault, without dropping the other valid results (S03a-fix-2
finding 6)."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (superchat-stage--define 'stage-test--validated-collect :kind 'collect
                              :validator (lambda (r) (if (eq r 'boom) (error "validator boom") (> r 0))))
    (superchat-stage-add 'stage-test--validated-collect 'good (lambda () 1) :owner 'test)
    (superchat-stage-add 'stage-test--validated-collect 'signals (lambda () 'boom) :owner 'test)
    (superchat-stage-add 'stage-test--validated-collect 'rejected (lambda () -1) :owner 'test)
    (should (equal '(1) (superchat-stage-call 'stage-test--validated-collect)))
    (should (= 2 (length superchat-stage-faults)))
    (should (cl-find 'rejected superchat-stage-faults :key (lambda (f) (plist-get f :id))))
    (should (cl-find 'signals superchat-stage-faults :key (lambda (f) (plist-get f :id))))))

(ert-deftest stage/required-collect-error-signals ()
  "A `:required t' collect implementation's `error' is terminal: it signals
`superchat-stage-required-failed' instead of being recorded as a fault."
  (superchat-stage-test--isolated
    (superchat-stage-add 'collect-context 'must (lambda (&rest _) (error "boom"))
                          :owner 'test :required t)
    (should-error (superchat-stage-call 'collect-context nil nil nil)
                  :type 'superchat-stage-required-failed)))

;; ═══════════════════════════════════════════════════════════
;; superchat-stage-call-outcome (S03a-fix-3 item 3: invocation-local fault)
;; ═══════════════════════════════════════════════════════════

(ert-deftest stage/call-outcome-basic-result-no-fault ()
  "`superchat-stage-call-outcome' returns the winning result with
`:faulted' nil when no direct implementation of this call faulted."
  (superchat-stage-test--isolated
    (stage-test--clear-implementations 'admit)
    (superchat-stage-add 'admit 'ok (lambda (&rest _) '(:action process)) :owner 'test)
    (should (equal '(:result (:action process) :faulted nil)
                    (superchat-stage-call-outcome 'admit)))))

(ert-deftest stage/call-outcome-ignores-nested-stage-faults ()
  "A fault from a stage call made INSIDE an implementation must not count
against the outer call's own `:faulted' flag (S03a-fix-3 item 3; the
reviewer's exact reproduction: an implementation that calls `load-state',
which faults, and itself cleanly returns nil).  The nested fault is still
recorded in the shared ring, just not attributed to the outer call."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (stage-test--clear-implementations 'admit)
    (superchat-stage-add 'load-state 'boom (lambda (&rest _) (error "boom")) :owner 'test)
    (superchat-stage-add
     'admit 'nested
     (lambda (&rest _) (superchat-stage-call 'load-state nil nil) nil)
     :owner 'test)
    (let ((outcome (superchat-stage-call-outcome 'admit)))
      (should-not (plist-get outcome :result))
      (should-not (plist-get outcome :faulted)))
    (should (cl-find 'boom superchat-stage-faults :key (lambda (f) (plist-get f :id))))))

(ert-deftest stage/call-outcome-nested-unrelated-call-does-not-fault-either ()
  "A nested call to an UNRELATED stage that does NOT fault leaves the outer
call's `:faulted' nil, as expected -- included alongside the nested-fault
case above so both nested scenarios the issue names are covered."
  (superchat-stage-test--isolated
    (stage-test--clear-implementations 'admit)
    (superchat-stage-add 'load-state 'quiet (lambda (&rest _) nil) :owner 'test)
    (superchat-stage-add
     'admit 'nested-quiet
     (lambda (&rest _) (superchat-stage-call 'load-state nil nil) '(:action process))
     :owner 'test)
    (should (equal '(:result (:action process) :faulted nil)
                    (superchat-stage-call-outcome 'admit)))))

(ert-deftest stage/call-outcome-sibling-fault-then-valid-result-wins ()
  "A direct sibling implementation's fault does not block a later,
directly-tried implementation's valid result (newest registration first);
`:faulted' still reports the sibling fault that occurred."
  (superchat-stage-test--isolated
    (stage-test--clear-implementations 'admit)
    (superchat-stage-add 'admit 'older-ok (lambda (&rest _) '(:action process)) :owner 'test)
    (superchat-stage-add 'admit 'newer-boom (lambda (&rest _) (error "boom")) :owner 'test)
    (let ((outcome (superchat-stage-call-outcome 'admit)))
      (should (equal '(:action process) (plist-get outcome :result)))
      (should (plist-get outcome :faulted)))))

(ert-deftest stage/call-outcome-sibling-fault-and-all-nil-reports-faulted ()
  "When every direct implementation returns nil and at least one faulted,
the outcome reports a nil result with `:faulted' t (S03a-fix-3 item 3:
`admit' must drop on exactly this outcome, never default to `process')."
  (superchat-stage-test--isolated
    (stage-test--clear-implementations 'admit)
    (superchat-stage-add 'admit 'quiet (lambda (&rest _) nil) :owner 'test)
    (superchat-stage-add 'admit 'boom (lambda (&rest _) (error "boom")) :owner 'test)
    (let ((outcome (superchat-stage-call-outcome 'admit)))
      (should-not (plist-get outcome :result))
      (should (plist-get outcome :faulted)))))

(ert-deftest stage/call-outcome-rejects-non-first-result-stages ()
  "`superchat-stage-call-outcome' only supports `first-result' stages."
  (superchat-stage-test--isolated
    (should-error (superchat-stage-call-outcome 'render))
    (should-error (superchat-stage-call-outcome 'collect-context))))

;; ═══════════════════════════════════════════════════════════
;; collect-context fragment-list validator (S03a-fix-3: collector shape)
;; ═══════════════════════════════════════════════════════════

(ert-deftest stage/collect-context-validator-rejects-bare-fragment-plist ()
  "A `collect-context' implementation that returns a bare fragment plist
instead of a list of fragments is faulted (via the stage's `:validator')
and its contribution omitted; a well-formed sibling contribution is
unaffected."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (stage-test--clear-implementations 'collect-context)
    (superchat-stage-add
     'collect-context 'bare
     (lambda (&rest _) (list :owner 'test :id "bare" :slot 'context :text "BARE"))
     :owner 'test)
    (superchat-stage-add
     'collect-context 'good
     (lambda (&rest _) (list (list :owner 'test :id "good" :slot 'context :text "GOOD")))
     :owner 'test)
    (should (equal '(((:owner test :id "good" :slot context :text "GOOD")))
                    (superchat-stage-call 'collect-context nil nil nil)))
    (should (cl-find 'bare superchat-stage-faults :key (lambda (f) (plist-get f :id))))
    (should (cl-find 'collect-context superchat-stage-faults
                      :key (lambda (f) (plist-get f :stage))))))

(ert-deftest stage/collect-context-validator-accepts-zero-one-many-fragments ()
  "The `collect-context' validator accepts nil-contribution (via the
ordinary `and result ...' skip, not the validator itself), a
single-fragment list, and a multi-fragment list, preserving collector
order."
  (superchat-stage-test--isolated
    (stage-test--clear-implementations 'collect-context)
    (superchat-stage-add 'collect-context 'none (lambda (&rest _) nil) :owner 'test)
    (superchat-stage-add
     'collect-context 'one
     (lambda (&rest _) (list (list :owner 'test :id "one" :text "ONE")))
     :owner 'test)
    (superchat-stage-add
     'collect-context 'many
     (lambda (&rest _)
       (list (list :owner 'test :id "many-a" :text "MANY_A")
             (list :owner 'test :id "many-b" :text "MANY_B")))
     :owner 'test)
    (should (equal '(((:owner test :id "many-a" :text "MANY_A")
                       (:owner test :id "many-b" :text "MANY_B"))
                      ((:owner test :id "one" :text "ONE")))
                    (superchat-stage-call 'collect-context nil nil nil)))))

(ert-deftest stage/report-lists-implementations ()
  "`superchat-stage-report' lists each stage's kind and implementations."
  (superchat-stage-test--isolated
    (superchat-stage-add 'render 'r1 (lambda (&rest _) "x") :owner 'test :required nil)
    (let* ((report (superchat-stage-report))
           (stages (plist-get report :stages))
           (entry (cl-find 'render stages :key (lambda (s) (plist-get s :stage)))))
      (should entry)
      (should (eq 'collect (plist-get entry :kind)))
      (should (equal '((:id r1 :owner test :required nil))
                      (plist-get entry :implementations)))
      (should (plist-member report :faults)))))

(provide 'test-stage)
;;; test-stage.el ends here
