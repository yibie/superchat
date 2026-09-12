;;; test-pipeline.el --- ERT tests for superchat-pipeline.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-sink)
(require 'superchat-stage)
;; `superchat-prompt.el' now requires `superchat-stage' itself and registers
;; `core-assembler' unconditionally (S03a-fix-1), so this no longer needs to
;; load in any particular order relative to `superchat-stage'.  Still loaded
;; explicitly because neither `superchat.el' nor `superchat-runtime.el'
;; requires `superchat-prompt' yet, so a caller of `superchat-pipeline-submit'
;; must arrange for it to be loaded until a later slice wires it in for real.
(require 'superchat-prompt)
(require 'superchat-pipeline)

;; `superchat-stage-test--isolated' (test-stage.el) and
;; `superchat-runtime-test--isolated'/`superchat-runtime-test--with-temp-store'
;; (test-runtime.el) are reused per the slice's instruction rather than
;; `cl-letf'-ing Runtime.  `test/run-tests.el' already loads both files
;; before this one; a focused run of just this file loads them here.
(let ((superchat-pipeline-test--dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (unless (fboundp 'superchat-stage-test--isolated)
    (load-file (expand-file-name "test-stage.el" superchat-pipeline-test--dir)))
  (unless (fboundp 'superchat-runtime-test--isolated)
    (load-file (expand-file-name "test-runtime.el" superchat-pipeline-test--dir))))

(ert-deftest pipeline/validate-rejects-each-missing-field ()
  "`superchat-pipeline-validate-envelope' signals `superchat-envelope-invalid'
for each rule in contract §2.2, and fills in a valid envelope."
  (let ((live-sink (superchat-sink-create)))
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "" :surface 'chat :sink live-sink))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :surface 'chat :sink live-sink))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :sink live-sink))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :surface 'not-registered :sink live-sink))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :surface 'chat))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :surface 'chat :sink "not-a-sink"))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :surface 'chat
                         :sink (superchat-sink-create :live-p (lambda () nil))))
                  :type 'superchat-envelope-invalid)
    (should-error (superchat-pipeline-validate-envelope
                   (list :content "hi" :surface 'chat :sink live-sink
                         :context (list :noprefix "x")))
                  :type 'superchat-envelope-invalid)
    (dolist (key superchat-pipeline--forbidden-envelope-keys)
      (should-error (superchat-pipeline-validate-envelope
                     (plist-put (list :content "hi" :surface 'chat :sink live-sink)
                                key "legacy"))
                    :type 'superchat-envelope-invalid))
    (let ((valid (superchat-pipeline-validate-envelope
                  (list :content "hi" :surface 'chat :sink live-sink))))
      (should (plist-get valid :request-id))
      (should (eq 'attended (plist-get valid :attendance))))))

(ert-deftest pipeline/submit-runs-stages-and-reaches-runtime ()
  "`superchat-pipeline-submit' runs stages 1-6 and reaches Runtime: the
envelope's callback Sink receives `:final', and the Tape has
`intent.received' through `turn.completed' for the resolved Session."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let (observer final-run final-payload)
        (let ((sink (superchat-sink-create
                     :final (lambda (run payload)
                              (setq final-run run final-payload payload)))))
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (setq observer superchat--llm-runtime-observer))))
            (let* ((envelope (list :content "hello there" :surface 'chat :sink sink))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'accepted (plist-get result :status)))
              (should (plist-get result :request-id))
              (funcall observer 'terminal '(:status completed :response "hi back"))
              (should final-payload)
              (should final-run)
              (should (equal "hi back" (plist-get final-payload :text)))
              (let ((types (mapcar (lambda (e) (plist-get e :type))
                                    (superchat-tape-replay-events
                                     (plist-get result :session-id)))))
                (should (member "intent.received" types))
                (should (member "turn.completed" types))))))))))

(ert-deftest pipeline/explicit-session-id-is-never-retargeted ()
  "An explicit envelope `:session-id' is used exactly as given, even when a
registered `resolve-session' implementation would return a different ID
\(S03a-fix-2 finding 2 -- the prior version of this test registered no
competing resolver and so could not detect the resolver-wins defect)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (superchat-stage-add 'resolve-session 'retarget
                              (lambda (&rest _) "some-other-session")
                              :owner 'test)
        (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
          (let* ((sink (superchat-sink-create))
                 (envelope (list :content "hi" :surface 'chat :sink sink
                                  :session-id "my-explicit-session"))
                 (result (superchat-pipeline-submit envelope)))
            (should (equal "my-explicit-session" (plist-get result :session-id)))))))))

(ert-deftest pipeline/ephemeral-session-format ()
  "An envelope with no `:session-id' resolves to \"ephemeral-<request-id>\"."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
        (let* ((sink (superchat-sink-create))
               (envelope (list :content "hi" :surface 'chat :sink sink))
               (result (superchat-pipeline-submit envelope)))
          (should (string-match-p "\\`ephemeral-req-[0-9a-f]+\\'"
                                   (plist-get result :session-id))))))))

(ert-deftest pipeline/admit-queue-returns-without-run ()
  "`admit' resolving to `queue' returns `(:status queue …)' without ever
reaching Runtime: no Tape row, no active Run, no provider call -- proved
through the real Runtime boundary under an isolated store, not a
`superchat-runtime-submit' spy (S03a-fix-2 finding 8: the seam rule forbids
replacing Runtime itself; only the provider seam is replaced, and only to
prove it is never reached)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (superchat-stage-add 'admit 'test-queue
                              (lambda (&rest _) (list :action 'queue :reason "session busy"))
                              :owner 'test)
        (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                   (lambda (&rest _) (error "must not reach the provider"))))
          (let* ((sink (superchat-sink-create))
                 (session-id "pipeline-admit-queue-session")
                 (envelope (list :content "hi" :surface 'chat :sink sink
                                  :session-id session-id))
                 (result (superchat-pipeline-submit envelope)))
            (should (eq 'queue (plist-get result :status)))
            (should (equal "session busy" (plist-get result :reason)))
            (should (plist-get result :request-id))
            (should-not (superchat-tape-replay-events session-id))
            (should-not (superchat-runtime-active-runs))))))))

(ert-deftest pipeline/select-tools-delegates-to-the-single-decision-point ()
  "The pipeline's `select-tools' stage calls `superchat-select-tools'
directly (contract §4.2: \"exactly one function decides which tool names a
request carries\") -- S09-fix-1 deletes this file's own second,
differently-worded decider (`superchat-pipeline--select-tools' and its
stale fixed chat/inline-ask baselines, which could turn an envelope result
into an explicit `:tools' bypassing S09's stage/baseline choice).  Surface
baseline content itself is `superchat-select-tools''s own unit tests'
concern (`test/test-llm-backend.el'); this only proves the pipeline
threads an explicit decision through to the materialized request
end-to-end, unmodified by any second decider."
  (should-not (fboundp 'superchat-pipeline--select-tools))
  (should-not (boundp 'superchat-pipeline--chat-tool-baseline))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (let (captured-tools called)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _cb _stream-cb &optional _model _files tools &rest _)
                       (setq called t captured-tools tools))))
            (let* ((sink (superchat-sink-create))
                   (envelope (list :content "hi" :surface 'chat :tools 'none :sink sink)))
              (superchat-pipeline-submit envelope)
              (should called)
              (should (eq 'none captured-tools)))))))))

(ert-deftest pipeline/select-tools-stage-decision-reaches-the-materialized-request ()
  "S09-fix-2 item 3 (review item 5): a behavioural gate instead of a name
scan.  The sibling test above only exercises the explicit `:tools 'none'
path (contract §4.2 rule 1), which a hard-coded second decider baked
directly into the pipeline can satisfy by coincidence without actually
delegating (S09-fix-1 re-review: a temporary `superchat-pipeline--choose-
tools' duplicate passed every existing static/unit check here).  This
registers a real `select-tools' stage contribution returning a unique
subset and submits an envelope with no explicit `:tools' at all, so only
a genuine call to `superchat-select-tools-decision' -- not a hard-coded
substitute -- can produce this exact result: the request Runtime actually
receives must carry precisely the stage's names, and the `:tool-decision'
field threaded alongside it (S09-fix-2 item 2) must report reason `stage'
with those same names, so a later re-decision at the LLM layer cannot
misreport it as `explicit'."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (superchat-stage-add
         'select-tools 'pipeline-test-stage-narrows-to-read-buffer
         (lambda (&rest _) (list "read_buffer"))
         :owner 'pipeline-test)
        (let (captured-tools captured-decision called)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _cb _stream-cb &optional _model _files tools
                              _system _preset precomputed-decision)
                       (setq called t captured-tools tools
                             captured-decision precomputed-decision))))
            (let* ((sink (superchat-sink-create))
                   (envelope (list :content "hi" :surface 'chat :sink sink)))
              (superchat-pipeline-submit envelope)
              (should called)
              (should (equal '("read_buffer") captured-tools))
              (should (equal '("read_buffer") (plist-get captured-decision :names)))
              (should (eq 'stage (plist-get captured-decision :reason))))))))))

(ert-deftest pipeline/collect-context-combines-with-core-content ()
  "A registered `collect-context' implementation's fragment JOINS the core
content fragment in the assembled prompt reaching the provider -- it never
replaces it (S03a-fix-2 finding 1: the prior `or' let the first non-empty
collector result silently drop the always-required content fragment)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (superchat-stage-add
         'collect-context 'extra
         (lambda (&rest _)
           ;; A `collect-context' implementation returns a *list* of
           ;; fragments (contract §4.3's legacy-bridge collector returns up
           ;; to two this way) -- never a bare fragment plist.
           (list (list :owner 'test :id "extra" :slot 'context
                       :text "extra collected fragment"
                       :priority 0 :budget-class 'soft :required nil)))
         :owner 'test)
        (let (captured-prompt)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _) (setq captured-prompt prompt))))
            (let* ((sink (superchat-sink-create))
                   (envelope (list :content "the original content" :surface 'chat :sink sink))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'accepted (plist-get result :status)))
              (should (string-match-p "the original content" captured-prompt))
              (should (string-match-p "extra collected fragment" captured-prompt)))))))))

(ert-deftest pipeline/collect-context-bare-fragment-is-faulted-and-omitted ()
  "A `collect-context' implementation that returns a bare fragment plist
instead of a list of fragments is faulted and its contribution omitted; a
sibling implementation's well-formed contribution still reaches the
prompt, and the malformed one never explodes into loose keyword/value
atoms that would otherwise corrupt the assembled fragment list (S03a-fix-3:
the collector result shape finding)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (setq superchat-stage-faults nil)
        (superchat-stage-add
         'collect-context 'bare
         ;; WRONG shape: one fragment plist directly, not wrapped in a list.
         (lambda (&rest _)
           (list :owner 'test :id "bare" :slot 'context
                 :text "BARE_FRAGMENT_TEXT"
                 :priority 0 :budget-class 'soft :required nil))
         :owner 'test)
        (superchat-stage-add
         'collect-context 'good
         (lambda (&rest _)
           (list (list :owner 'test :id "good" :slot 'context
                       :text "GOOD_FRAGMENT_TEXT"
                       :priority 0 :budget-class 'soft :required nil)))
         :owner 'test)
        (let (captured-prompt)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt &rest _) (setq captured-prompt prompt))))
            (let* ((sink (superchat-sink-create))
                   (envelope (list :content "the original content" :surface 'chat :sink sink))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'accepted (plist-get result :status)))
              (should (string-match-p "the original content" captured-prompt))
              (should (string-match-p "GOOD_FRAGMENT_TEXT" captured-prompt))
              (should-not (string-match-p "BARE_FRAGMENT_TEXT" captured-prompt))
              (should (cl-find 'bare superchat-stage-faults
                                :key (lambda (f) (plist-get f :id))))
              (should (cl-find 'collect-context superchat-stage-faults
                                :key (lambda (f) (plist-get f :stage)))))))))))

(ert-deftest pipeline/admit-nested-stage-fault-does-not-cause-a-false-drop ()
  "An `admit' implementation that internally calls another stage which
faults, and itself returns nil, must NOT drop the Turn -- the nested
fault belongs to the inner stage, not to `admit' itself (S03a-fix-3 item
3; the prior `superchat-pipeline--stage-call-with-fault-p' diffed the
global fault ring and would have mis-attributed this nested fault to the
outer `admit' call, dropping the Turn)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (setq superchat-stage-faults nil)
        (stage-test--clear-implementations 'admit)
        (superchat-stage-add 'load-state 'boom (lambda (&rest _) (error "boom"))
                              :owner 'test)
        (superchat-stage-add
         'admit 'nested
         (lambda (&rest _) (superchat-stage-call 'load-state nil nil) nil)
         :owner 'test)
        (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
          (let* ((sink (superchat-sink-create))
                 (session-id "pipeline-admit-nested-fault-session")
                 (envelope (list :content "hi" :surface 'chat :sink sink
                                  :session-id session-id))
                 (result (superchat-pipeline-submit envelope)))
            (should (eq 'accepted (plist-get result :status)))
            (should (cl-find 'load-state superchat-stage-faults
                              :key (lambda (f) (plist-get f :stage))))
            (should-not (cl-find 'admit superchat-stage-faults
                                  :key (lambda (f) (plist-get f :stage))))))))))

(ert-deftest pipeline/admit-error-drops-with-no-run-and-select-tools-error-yields-none ()
  "An `admit' implementation `error' yields `:status drop' -- recorded as a
fault, no Tape row, no active Run, no provider call -- never the
non-erroring `process' default; a `select-tools' implementation `error'
yields `none', never the surface baseline (S03a-fix-2 finding 3: both
previously converted an implementation fault into MORE execution, not
less)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (superchat-stage-test--isolated
        (setq superchat-stage-faults nil)
        (superchat-stage-add 'admit 'boom (lambda (&rest _) (error "admit boom")) :owner 'test)
        (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                   (lambda (&rest _) (error "must not reach the provider"))))
          (let* ((sink (superchat-sink-create))
                 (session-id "pipeline-admit-error-session")
                 (envelope (list :content "hi" :surface 'chat :sink sink
                                  :session-id session-id))
                 (result (superchat-pipeline-submit envelope)))
            (should (eq 'drop (plist-get result :status)))
            (should (plist-get result :reason))
            (should (cl-find 'admit superchat-stage-faults
                              :key (lambda (f) (plist-get f :stage))))
            (should-not (superchat-tape-replay-events session-id))
            (should-not (superchat-runtime-active-runs)))))))
  (superchat-stage-test--isolated
    (superchat-stage-add 'select-tools 'boom (lambda (&rest _) (error "select-tools boom"))
                          :owner 'test)
    (should (eq 'none (superchat-select-tools (list :surface 'chat) nil)))))

(ert-deftest pipeline/malformed-admit-result-drops-and-is-recorded ()
  "An `admit' result with an unrecognized `:action', a non-string
`:reason', or a wrong-typed `:target-run-id' is malformed: it is recorded
as a fault and mapped to the non-admitting `drop' default, never silently
coerced to `process' (S03a-fix-2 finding 4)."
  (dolist (bad-result '((:action bogus-action)
                         (:action process :reason 42)
                         (:action process :target-run-id 7)
                         ;; Missing `:action' entirely (S03a-fix-3 item 4 --
                         ;; the original finding this validator must catch).
                         (:reason "why")
                         ;; A foreign key outside the recognized shape.
                         (:action process :unexpected-key "x")))
    (superchat-runtime-test--isolated
      (superchat-runtime-test--with-temp-store
        (superchat-stage-test--isolated
          (setq superchat-stage-faults nil)
          (superchat-stage-add 'admit 'malformed (lambda (&rest _) bad-result) :owner 'test)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _) (error "must not reach the provider"))))
            (let* ((sink (superchat-sink-create))
                   (session-id (format "pipeline-admit-malformed-%s" (cl-gensym)))
                   (envelope (list :content "hi" :surface 'chat :sink sink
                                    :session-id session-id))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'drop (plist-get result :status)))
              (should (plist-get result :reason))
              (should (cl-find 'admit superchat-stage-faults
                                :key (lambda (f) (plist-get f :stage))))
              (should-not (superchat-tape-replay-events session-id))
              (should-not (superchat-runtime-active-runs)))))))))

(ert-deftest pipeline/envelope-origin-and-system-prompt-reach-the-run ()
  "The envelope's own `:origin' is merged with -- not replaced by -- the
mandatory surface attribution (proved through the real `intent.received'
Tape row, which records `:origin-uri' from the Request's `:origin' and
always records the surface Runtime resolved, never a caller-supplied
override), and its `:system-prompt' rides through as a system fragment
into the prompt the provider sees (S03a-fix-2 finding 5)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let (captured-system)
        (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                   (lambda (_prompt _cb _stream-cb &optional _model _files _tools system-prompt &rest _)
                     (setq captured-system system-prompt))))
          (let* ((sink (superchat-sink-create))
                 (envelope (list :content "hi" :surface 'chat :sink sink
                                  ;; `:surface bogus-surface' here must lose
                                  ;; to the mandatory `chat' attribution;
                                  ;; `:uri' must survive the merge.
                                  :origin (list :uri "test://my-origin" :surface 'bogus-surface)
                                  :system-prompt "be extremely terse"))
                 (result (superchat-pipeline-submit envelope))
                 (events (superchat-tape-replay-events (plist-get result :session-id)))
                 (intent (cl-find "intent.received" events
                                  :key (lambda (e) (plist-get e :type)) :test #'equal)))
            (should (eq 'accepted (plist-get result :status)))
            (should intent)
            (should (equal "test://my-origin"
                           (plist-get (plist-get intent :payload) :origin-uri)))
            (should (equal "chat" (plist-get (plist-get intent :payload) :surface)))
            (should (string-match-p "be extremely terse" captured-system))))))))

(ert-deftest pipeline/register-surface-rejects-non-symbol ()
  "`superchat-register-surface' rejects a non-symbol (and nil), so a
registered string can never later pass the `memq' surface check
\(S03a-fix-2 finding 9)."
  (let ((superchat-pipeline--surfaces (copy-sequence superchat-pipeline--surfaces)))
    (should-error (superchat-register-surface "not-a-symbol"))
    (should-error (superchat-register-surface nil))
    (should (eq 'test-surface-ok (superchat-register-surface 'test-surface-ok)))))

(ert-deftest pipeline/load-state-foreign-prefix-dropped-and-recorded ()
  "A `load-state' delta whose keys are not prefixed by its own `:owner' is
dropped from the merged state and recorded as a `load-state' fault."
  (superchat-stage-test--isolated
    (setq superchat-stage-faults nil)
    (superchat-stage-add 'load-state 'good
                          (lambda (&rest _) (list :owner 'good :delta '(:good-flag t)))
                          :owner 'test)
    (superchat-stage-add 'load-state 'bad
                          (lambda (&rest _)
                            (list :owner 'bad :delta '(:not-bad-prefixed t)))
                          :owner 'test)
    (let ((state (superchat-pipeline--merge-state
                  (superchat-stage-call 'load-state nil nil))))
      (should (plist-get state :good-flag))
      (should-not (plist-member state :not-bad-prefixed))
      (should (cl-find 'load-state superchat-stage-faults
                        :key (lambda (f) (plist-get f :stage))))
      (should (cl-find 'bad superchat-stage-faults
                        :key (lambda (f) (plist-get f :owner)))))))

(ert-deftest pipeline/sink-receives-waiting-status ()
  "The envelope's Sink, now passed straight through as Runtime's `:sink'
key (S03a-fix-1 item 2), receives `:status' `waiting' -- a slot the legacy
`:stream-function'/`:delivery-function' bridge this replaced could never
reach (`superchat-runtime--legacy-view-sink' hard-codes `:status' to its
own View-only rendering)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let (statuses)
        (let ((sink (superchat-sink-create
                     :status (lambda (_run status) (push status statuses)))))
          (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
            (let* ((envelope (list :content "hi" :surface 'chat :sink sink))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'accepted (plist-get result :status)))
              (should (memq 'waiting statuses)))))))))

(ert-deftest pipeline/dead-sink-orphans-before-seal ()
  "A Sink whose `:live-p' turns false before the terminal seal is orphaned
-- `:status' `orphaned' then `:orphaned' with reason `sink-lost', never
`:final' and never `:status' `stopped' (contract §3.3's pre-seal target
loss rule).  Reachable now that the envelope's exact Sink, with its own
`:live-p', is handed straight to Runtime as `:sink' instead of through a
bridge with no `:live-p' of its own."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let (observer orphaned-run orphaned-reason final-payload live-p statuses)
        (setq live-p t)
        (let ((sink (superchat-sink-create
                     :live-p (lambda () live-p)
                     :status (lambda (_run status) (push status statuses))
                     :final (lambda (_run payload) (setq final-payload payload))
                     :orphaned (lambda (run reason)
                                 (setq orphaned-run run orphaned-reason reason)))))
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (setq observer superchat--llm-runtime-observer))))
            (let* ((envelope (list :content "hi" :surface 'chat :sink sink))
                   (result (superchat-pipeline-submit envelope)))
              (should (eq 'accepted (plist-get result :status)))
              (setq live-p nil)
              (funcall observer 'terminal '(:status completed :response "too late"))
              (should orphaned-run)
              (should (eq 'sink-lost orphaned-reason))
              (should-not final-payload)
              (should (memq 'orphaned statuses))
              (should-not (memq 'stopped statuses)))))))))

;;; S10: the chat dispatcher submits an envelope through the driver.

(ert-deftest pipeline/chat-envelope-uses-view-buffer-as-host ()
  "A chat envelope's `:context (:chat-buffer B)' becomes Runtime's exact
`:buffer' (never the hidden pipeline host), validated through
`superchat-editor-workspace-validate-buffer' before submission -- a buffer
already owned by a DIFFERENT Session signals instead of silently reaching
Runtime, exactly like every other producer call site (S10 plan §1.1)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((view (generate-new-buffer " *pipeline-chat-view*")))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
                (let* ((sink (superchat-sink-create))
                       (envelope
                        (list :content "hi" :surface 'chat :sink sink
                              :session-id "pipeline-chat-host"
                              :context (list :chat-buffer view)))
                       (result (superchat-pipeline-submit envelope)))
                  (should (eq 'accepted (plist-get result :status)))
                  (should (eq view (plist-get
                                     (superchat-runtime--entry (plist-get result :run-id))
                                     :buffer)))))
              (with-current-buffer view
                (setq superchat--editor-workspace-session-id "pipeline-chat-host-other"
                      superchat--editor-workspace-owned-p t
                      superchat--editor-workspace-role 'protocol))
              (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
                (let* ((sink (superchat-sink-create))
                       (envelope
                        (list :content "hi" :surface 'chat :sink sink
                              :session-id "pipeline-chat-host"
                              :context (list :chat-buffer view))))
                  (should-error (superchat-pipeline-submit envelope)))))
          (when (buffer-live-p view) (kill-buffer view)))))))

(ert-deftest pipeline/forbidden-legacy-keys-are-rejected-even-inside-extras ()
  "Legacy routing and prompt keys are forbidden both at the envelope's top
level and inside `:runtime-extras'; `:approval-function' is the one
documented §2.3 bridge extras may carry (S10-fix-2 finding 3)."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((sink (superchat-sink-create)))
        ;; All seven stay forbidden at the envelope's own top level.
        (dolist (key '(:buffer :buffer-name :stream-function :delivery-function
                       :approval-function :prompt :input))
          (should-error
           (superchat-pipeline-validate-envelope
            (list :content "hi" :surface 'chat :sink sink key "x"))
           :type 'superchat-envelope-invalid))
        ;; The routing and prompt six stay rejected inside extras too.
        (dolist (key '(:buffer :buffer-name :stream-function :delivery-function
                       :prompt :input))
          (should-error
           (superchat-pipeline-validate-envelope
            (list :content "hi" :surface 'chat :sink sink
                  :runtime-extras (list key "x")))
           :type 'superchat-envelope-invalid))
        (should-error
         (superchat-pipeline-validate-envelope
          (list :content "hi" :surface 'chat :sink sink
                :runtime-extras "not-a-plist"))
         :type 'superchat-envelope-invalid)
        ;; `:approval-function' is the one documented exception inside extras.
        (let ((accepted
               (superchat-pipeline-validate-envelope
                (list :content "hi" :surface 'chat :sink sink
                      :runtime-extras (list :approval-function #'ignore)))))
          (should (eq #'ignore
                      (plist-get (plist-get accepted :runtime-extras)
                                 :approval-function))))
        ;; The allowed extras still reach the legacy Request untouched.
        (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
          (let* ((envelope
                  (list :content "hi" :surface 'chat :sink sink
                        :session-id "pipeline-extras"
                        :runtime-extras (list :actor-id "extras-actor")))
                 (result (superchat-pipeline-submit envelope)))
            (should (eq 'accepted (plist-get result :status)))
            (should (equal "extras-actor"
                           (plist-get (superchat-runtime--entry (plist-get result :run-id))
                                      :actor-id)))))))))

(ert-deftest pipeline/chat-prepared-prompt-bypasses-collect-context ()
  "When `:context' carries `:prepared-prompt', the driver uses exactly
that text (plus `:prepared-system') as the prompt: the generic `content'/
`system-prompt' fragments and the `collect-context' stage call are both
skipped, so a registered collector never runs and the user's own text is
not duplicated (S10, leader-approved plan §1.3)."
  (superchat-stage-test--isolated
    (superchat-runtime-test--isolated
      (superchat-runtime-test--with-temp-store
        (let (collector-called captured-prompt)
          (superchat-stage-add
           'collect-context 'pipeline-test-marker
           (lambda (&rest _)
             (setq collector-called t)
             (list (list :owner 'pipeline-test :id "marker" :slot 'template
                         :text "SHOULD NOT APPEAR" :priority 0 :budget-class 'soft)))
           :owner 'pipeline-test)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest args) (setq captured-prompt (nth 0 args)))))
            (let* ((sink (superchat-sink-create))
                   (envelope
                    (list :content "raw content, unused when prepared"
                          :surface 'chat :sink sink
                          :session-id "pipeline-prepared-prompt"
                          :context (list :prepared-prompt "PREPARED ONCE"
                                         :prepared-system "SYS"))))
              (superchat-pipeline-submit envelope)
              (should-not collector-called)
              (should (equal "PREPARED ONCE" captured-prompt)))))))))

(ert-deftest pipeline/chat-authority-mode-and-policy-seed-state ()
  "`:context (:chat-authority-mode M :chat-authority-policy P)' seeds
`:runtime-authority-mode'/`:runtime-authority-policy' in the merged
`load-state' output (contract §9) instead of the `ask'/\"default\" core
defaults."
  (superchat-stage-test--isolated
    (superchat-runtime-test--isolated
      (superchat-runtime-test--with-temp-store
        (let (seen-state)
          (superchat-stage-add
           'select-tools 'pipeline-test-state-observer
           (lambda (_envelope state) (setq seen-state state) nil)
           :owner 'pipeline-test)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
            (let* ((sink (superchat-sink-create))
                   (envelope
                    (list :content "hi" :surface 'chat :sink sink
                          :session-id "pipeline-authority-state"
                          :context (list :chat-authority-mode 'execute
                                         :chat-authority-policy "strict"))))
              (superchat-pipeline-submit envelope)
              (should (eq 'execute (plist-get seen-state :runtime-authority-mode)))
              (should (equal "strict" (plist-get seen-state :runtime-authority-policy))))))))))

(provide 'test-pipeline)
;;; test-pipeline.el ends here
