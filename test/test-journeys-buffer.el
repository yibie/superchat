;;; test-journeys-buffer.el --- Buffer journey acceptance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Rewrite (S17a) goes through the same envelope / pipeline / Sink path as
;; the chat: `superchat-rewrite-region' returns a preview overlay and the
;; answer streams into it.  Org Babel and Magit keep their synchronous
;; adapters because their host contracts need a synchronous result.  The
;; fixture still replaces only llm.el's two network entry points: the actual
;; Runtime, Tape, and delivery path are exercised for every call below.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'journey-fixture
         (expand-file-name "journey-fixture"
                           (file-name-directory load-file-name)))
(require 'superchat-rewrite)
(require 'ob-superchat)
(require 'superchat-magit)
(require 'superchat-inline)

(defun journey-buffer--rewrite-temporary-buffers ()
  "Return the live temporary buffers created by the rewrite preview."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (string-match-p "\\*superchat-rewrite-"
                          (or (buffer-name buffer) ""))))
   (buffer-list)))

(defun journey-buffer--activate-full-region ()
  "Make the current buffer's complete contents a real active region.

`superchat-rewrite-region' also checks `region-noncontiguous-p', even when
called programmatically with BEG and END.  Establish the same region state as
its interactive entry point rather than bypassing that check."
  (goto-char (point-min))
  (set-mark (point-max))
  (activate-mark))

(defun journey-buffer--stream-observation (label invoke)
  "Run INVOKE and return LABEL paired with buffers where a fresh chunk leaked.

The chunk text is assembled at run time so its literal form cannot already
be present in the loaded test source (B-inv-2: the ` *load*' buffer holding
this very file's text would otherwise match a hardcoded sentinel).  Buffer
text is snapshotted immediately before INVOKE runs, and only a genuinely new
appearance — not a pre-existing match such as the source buffer carrying
this defun's own text — counts as a leak."
  (let* ((chunk (format "JOURNEY_%s_%s_NO_STRAY_CHUNK" (random) (float-time)))
         (before (journey-fixture-buffer-text-index))
         leaked)
    (journey-fixture-set-script
     (list
      (list :chunks (list chunk)
            ;; Rewrite parses the normal model result as a fenced replacement;
            ;; use one here so its real synchronous adapter reaches its normal
            ;; successful completion after the stream observation.
            :text (if (equal label "rewrite")
                      "```text\nrewrite final result\n```"
                    (concat label " final result"))
            :after-chunks
            (lambda (_state)
              (setq leaked (journey-fixture-new-matches chunk before))))))
    (funcall invoke)
    (cons label leaked)))

(defun journey-buffer--rewrite-snapshot ()
  "Return the current buffer's text, modified flag, undo length and overlays."
  (list (buffer-substring-no-properties (point-min) (point-max))
        (buffer-modified-p)
        (if (listp buffer-undo-list) (length buffer-undo-list) 0)
        (overlays-in (point-min) (point-max))))

(defun journey-buffer--rewrite-status (overlay)
  "Return OVERLAY's rewrite status, or nil once it is gone."
  (and (overlayp overlay) (overlay-buffer overlay)
       (plist-get (overlay-get overlay 'superchat-rewrite) :status)))

(defun journey-buffer--wait-rewrite-status (overlay status &optional seconds)
  "Wait until OVERLAY's rewrite status is STATUS (nil = overlay gone)."
  (journey-fixture-wait-until
   (lambda () (eq (journey-buffer--rewrite-status overlay) status))
   (or seconds 5)))

(defun journey-buffer--rewrite-source (name text)
  "Return a fresh `text-mode' buffer NAME holding TEXT with its region active."
  (let ((source (generate-new-buffer name)))
    (with-current-buffer source
      (text-mode)
      (buffer-enable-undo)
      (insert text)
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil)
      (journey-buffer--activate-full-region))
    source))

(ert-deftest journey/b-inv-1-rewrite-does-not-touch-source-before-accept ()
  "A streamed rewrite must leave its source untouched before an accept action.
S17a: `superchat-rewrite-region' returns its preview overlay at once; the
snapshot taken while a chunk is streaming equals the one taken before."
  (journey-fixture-with
      (:model "journey-rewrite-streaming-model")
    (let* ((source (journey-buffer--rewrite-source
                    " *journey-rewrite-source*" "JOURNEY_REWRITE_SOURCE_TEXT\n"))
           (chunk "JOURNEY_REWRITE_STREAM_CHUNK")
           before during overlay)
      (with-current-buffer source
        (setq before (journey-fixture-buffer-snapshot source)))
      (journey-fixture-set-script
       (list
        (list :chunks (list chunk)
              :hang t
              :after-chunks
              (lambda (_state)
                (setq during (journey-fixture-buffer-snapshot source))))))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max)
                                                "make this clearer"))
        (should (overlayp overlay))
        (should (eq 'waiting (journey-buffer--rewrite-status overlay))))
      (should (journey-fixture-wait-until (lambda () during) 5))
      (should (equal before during))
      (should (equal before (journey-fixture-buffer-snapshot source)))
      (with-current-buffer source
        (should-not (string-match-p "\\*\\* Assistant:" (buffer-string))))
      (superchat-rewrite-cancel overlay)
      (should (journey-buffer--wait-rewrite-status overlay nil))
      (should (equal before (journey-fixture-buffer-snapshot source))))))

(ert-deftest journey/b-inv-2-adapter-streams-never-write-a-buffer ()
  "Rewrite, Org Babel, and Magit adapter chunks remain outside every buffer."
  (journey-fixture-with
      (:model "journey-adapter-streaming-model")
    (let* ((rewrite-source (generate-new-buffer " *journey-rewrite-stream*"))
           (babel-source (generate-new-buffer " *journey-babel-stream*"))
           (magit-source (generate-new-buffer " *journey-magit-stream*"))
           observations)
      (with-current-buffer rewrite-source
        (text-mode)
        (insert "rewrite source")
        ;; `auto' accepts the ready rewrite by itself (S17a); this journey
        ;; concerns the Runtime stream delivery before that final result.
        (let ((superchat-rewrite-confirmation 'auto))
          (push
           (journey-buffer--stream-observation
            "rewrite"
            (lambda ()
              (journey-buffer--activate-full-region)
              (let ((overlay (superchat-rewrite-region (point-min) (point-max)
                                                       "rewrite")))
                ;; The async preview must have finished (auto-accepted)
                ;; before the leak observation is read.
                (should (journey-buffer--wait-rewrite-status overlay nil)))))
           observations)
          (should (string-match-p "rewrite final result"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))
      (with-current-buffer babel-source
        (insert "#+begin_src superchat\nquestion\n#+end_src\n")
        (push
         (journey-buffer--stream-observation
          "org-babel"
          (lambda () (org-babel-execute:superchat "question" nil)))
         observations))
      ;; The interactive Magit command additionally needs a real staged Git
      ;; index, which this suite must not mutate.  Its synchronous adapter is
      ;; the actual entry from that point onward and exercises the same Runtime
      ;; delivery contract without substituting Magit or Superchat code.
      (with-current-buffer magit-source
        (insert "diff source")
        (push
         (journey-buffer--stream-observation
          "magit"
          (lambda () (superchat-magit--call-llm "synthetic staged diff")))
         observations))
      (should-not
       (cl-mapcan #'cdr observations)))))

(ert-deftest journey/b-inv-3-rewrite-rejection-leaves-no-residue ()
  "B6 ≡ B-inv-3: rejecting a ready rewrite preview leaves the buffer, its
modified flag, its undo list, its overlays and the buffer list exactly as
they were, and the preview showed the answer without its fences (B4 b)."
  (journey-fixture-with
      (:model "journey-rewrite-reject-model")
    (let* ((source (journey-buffer--rewrite-source
                    " *journey-rewrite-reject*" "JOURNEY_REWRITE_ORIGINAL"))
           before buffers-before overlay)
      (with-current-buffer source
        (setq before (journey-buffer--rewrite-snapshot)))
      (journey-fixture-set-script
       (list (list :text "```text\nJOURNEY_REWRITE_REPLACEMENT\n```")))
      (setq buffers-before (buffer-list))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max) "rewrite")))
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (should (equal "JOURNEY_REWRITE_REPLACEMENT"
                     (substring-no-properties (overlay-get overlay 'display))))
      (should (string-match-p "Ready" (overlay-get overlay 'before-string)))
      (with-current-buffer source
        ;; Still byte-identical while the preview is showing.
        (should (equal (car before)
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (superchat-rewrite-reject overlay)
      (with-current-buffer source
        (let ((after (journey-buffer--rewrite-snapshot)))
          (should (equal (nth 0 before) (nth 0 after)))
          (should (eq (nth 1 before) (nth 1 after)))
          (should (= (nth 2 before) (nth 2 after)))
          ;; Both directions: exactly the pre-existing overlay set survives.
          (should-not (cl-set-exclusive-or (nth 3 before) (nth 3 after) :test #'eq))))
      (let* ((ignored (cons (get-buffer "*Warnings*")
                            (journey-fixture-state-transport-buffers
                             journey-fixture--state)))
             (now-diff (cl-set-difference (buffer-list) ignored :test #'eq))
             (before-diff (cl-set-difference buffers-before ignored :test #'eq)))
        (should-not (cl-set-exclusive-or now-diff before-diff :test #'eq))))))

(ert-deftest journey/b-inv-3b-rewrite-provider-error-leaves-no-residue ()
  "B9 ≡ B-inv-3b: a provider error shows in the overlay's status bar with the
original text still visible, the overlay disappears after the error timer,
the Run ends `failed', and the source keeps its text, modified flag, undo
list and overlays."
  (journey-fixture-with
      (:model "journey-rewrite-error-model")
    (let* ((superchat-rewrite--error-display-seconds 0.05)
           (source (journey-buffer--rewrite-source
                    " *journey-rewrite-error*" "JOURNEY_REWRITE_ERROR_ORIGINAL"))
           before overlay run-id)
      (with-current-buffer source
        (setq before (journey-buffer--rewrite-snapshot)))
      (journey-fixture-set-script
       (list (list :error '(error . "journey rewrite provider failure"))))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max) "rewrite"))
        (setq run-id (plist-get (overlay-get overlay 'superchat-rewrite) :run-id)))
      (should run-id)
      (should (journey-buffer--wait-rewrite-status overlay nil))
      (should (eq 'failed (plist-get (superchat-runtime-status run-id) :status)))
      (with-current-buffer source
        (let ((after (journey-buffer--rewrite-snapshot)))
          (should (equal (nth 0 before) (nth 0 after)))
          (should (eq (nth 1 before) (nth 1 after)))
          (should (= (nth 2 before) (nth 2 after)))
          (should-not (cl-set-exclusive-or (nth 3 before) (nth 3 after) :test #'eq)))
        (should-not (string-match-p "\\*\\* Assistant:" (buffer-string))))
      (should-not (journey-buffer--rewrite-temporary-buffers)))))

(ert-deftest journey/b4-rewrite-preview-overlays-and-never-edits-source ()
  "B4: the rewrite previews in an overlay: source untouched throughout; the
preview equals the answer without fences; the request carries the system
demand, the original, the instruction and context lines; the status bar
goes Waiting → Typing → Ready; the overlay keymap binds the four keys."
  (journey-fixture-with
      (:model "journey-rewrite-b4-model")
    (let* ((source (generate-new-buffer " *journey-rewrite-b4*"))
           (statuses nil) before overlay)
      (with-current-buffer source
        (emacs-lisp-mode)
        (buffer-enable-undo)
        (insert ";; CONTEXT_BEFORE_LINE\n(defun journey-b4 () 1)\n;; CONTEXT_AFTER_LINE\n")
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)
        (goto-char (point-min)) (forward-line 1)
        (set-mark (point)) (end-of-line) (activate-mark)
        (setq before (journey-buffer--rewrite-snapshot)))
      (journey-fixture-set-script
       (list (list :chunks '("```emacs-lisp-mode\n(defun journey-b4 () ")
                   :after-chunks
                   (lambda (_state)
                     (push (journey-buffer--rewrite-status overlay) statuses))
                   :text "```emacs-lisp-mode\n(defun journey-b4 () 2)\n```")))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (region-beginning) (region-end)
                                                "return two")))
      (push (journey-buffer--rewrite-status overlay) statuses)
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (push (journey-buffer--rewrite-status overlay) statuses)
      (should (equal '(waiting typing ready) (reverse statuses)))
      (should (equal "(defun journey-b4 () 2)"
                     (substring-no-properties (overlay-get overlay 'display))))
      (with-current-buffer source
        (should (equal (nth 0 before)
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should (eq (nth 1 before) (buffer-modified-p)))
        (should (= (nth 2 before)
                   (if (listp buffer-undo-list) (length buffer-undo-list) 0))))
      (let* ((request (journey-fixture-last-request))
             (prompt (plist-get request :prompt-text)))
        (should (string-match-p "ONLY the rewritten code" prompt))
        (should (string-match-p "return two" prompt))
        (should (string-match-p "<<<REGION>>>\n(defun journey-b4 () 1)\n<<<END>>>" prompt))
        (should (string-match-p "CONTEXT_BEFORE_LINE" prompt))
        (should (string-match-p "CONTEXT_AFTER_LINE" prompt)))
      (let ((map (overlay-get overlay 'keymap)))
        (dolist (key '("C-c C-a" "C-c C-k" "C-c C-r" "C-c C-d"))
          (should (lookup-key map (kbd key)))))
      (superchat-rewrite-reject overlay)
      (kill-buffer source))))

(ert-deftest journey/b5-accept-applies-with-one-undo-and-tape ()
  "B5: accepting replaces exactly the region, leaves the rest unchanged,
removes the overlay, records `edit.applied' on the Tape, and one undo
restores the original."
  (journey-fixture-with
      (:model "journey-rewrite-b5-model")
    (let* ((source (journey-buffer--rewrite-source
                    " *journey-rewrite-b5*" "KEEP_HEAD\nJOURNEY_B5_OLD\nKEEP_TAIL\n"))
           overlay session-id)
      (with-current-buffer source
        (goto-char (point-min)) (forward-line 1)
        (set-mark (point)) (end-of-line) (activate-mark)
        (setq session-id (superchat-inline-session-id source)))
      (journey-fixture-set-script
       (list (list :text "```text\nJOURNEY_B5_NEW\n```")))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (region-beginning) (region-end)
                                                "renew")))
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (superchat-rewrite-accept overlay)
      (with-current-buffer source
        (should (equal "KEEP_HEAD\nJOURNEY_B5_NEW\nKEEP_TAIL\n"
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (superchat-rewrite-overlays source))
        (undo-start) (undo-more 1)
        (should (equal "KEEP_HEAD\nJOURNEY_B5_OLD\nKEEP_TAIL\n"
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (should (member "edit.applied" (journey-fixture-event-types session-id)))
      (kill-buffer source))))

(ert-deftest journey/b10-cancel-deletes-overlay-and-drops-late-chunks ()
  "B10: cancelling a running rewrite cancels its Run, deletes the overlay,
and a chunk arriving afterwards changes nothing."
  (journey-fixture-with
      (:model "journey-rewrite-b10-model")
    (let* ((source (journey-buffer--rewrite-source
                    " *journey-rewrite-b10*" "JOURNEY_B10_ORIGINAL"))
           before overlay run-id arrived)
      (with-current-buffer source
        (setq before (journey-buffer--rewrite-snapshot)))
      (journey-fixture-set-script
       (list (list :chunks '("JOURNEY_B10_CHUNK")
                   :hang t
                   :after-chunks
                   (lambda (_state) (setq arrived t)))))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max) "rewrite"))
        (setq run-id (plist-get (overlay-get overlay 'superchat-rewrite) :run-id)))
      (should (journey-fixture-wait-until (lambda () arrived) 5))
      (should (member run-id (superchat-runtime-active-runs)))
      (superchat-rewrite-cancel overlay)
      (should (journey-fixture-wait-until
               (lambda () (eq 'cancelled
                              (plist-get (superchat-runtime-status run-id) :status)))
               5))
      (should-not (overlay-buffer overlay))
      (should-not (superchat-rewrite-overlays source))
      ;; A late chunk finds no live overlay: Runtime drops emissions for a
      ;; terminal Run, and the Sink's `:live-p' is already false.
      (should-not (member run-id (superchat-runtime-active-runs)))
      (with-current-buffer source
        (let ((after (journey-buffer--rewrite-snapshot)))
          (should (equal (nth 0 before) (nth 0 after)))
          (should (eq (nth 1 before) (nth 1 after)))
          (should-not (cl-set-exclusive-or (nth 3 before) (nth 3 after) :test #'eq))))
      (kill-buffer source))))

(defun journey-buffer--inline-snapshot ()
  "Return the current buffer's text, modified flag and undo length together.
The B1 oracle compares three of these byte-for-byte around an inline ask."
  (list (buffer-substring-no-properties (point-min) (point-max))
        (buffer-modified-p)
        (if (listp buffer-undo-list) (length buffer-undo-list) 0)))

(ert-deftest journey/b1-inline-ask-answers-in-overlay-without-editing-source ()
  "B1: inline ask answers in an overlay and never edits the source buffer.
Three snapshots (before / during / after) stay byte-identical including
`buffer-modified-p' and undo length; the answer exists only in the overlay's
`after-string'; the provider's user message carries the buffer name, file
path, line range and the exact reference text; the command loop stays
responsive (a zero-delay timer fires before the answer); and the inline
Session is a real Tape session carrying the Run lifecycle."
  (journey-fixture-with ()
    (let* ((directory (journey-fixture-state-data-directory journey-fixture--state))
           (file (expand-file-name "inline-b1.el" directory))
           (token "JOURNEY_B1_REFERENCE")
           source overlay snapshots responsive reference)
      (with-temp-file file
        (insert (format "(defun journey-inline-b1 ()\n  (list %S))\n" token)))
      (setq source (find-file-noselect file))
      (unwind-protect
          (with-current-buffer source
            (emacs-lisp-mode)
            (goto-char (point-min))
            (search-forward token)
            (beginning-of-defun)
            (let ((bounds (bounds-of-thing-at-point 'defun)))
              (setq reference (buffer-substring-no-properties
                               (car bounds) (cdr bounds))))
            (push (journey-buffer--inline-snapshot) snapshots)
            (journey-fixture-set-script
             (list (list :chunks '("B1 PARTIAL ANSWER")
                         :delay 0.2
                         :after-chunks
                         (lambda (_state)
                           (with-current-buffer source
                             (push (journey-buffer--inline-snapshot) snapshots)))
                         :text "B1 FINAL ANSWER")))
            (setq overlay (superchat-inline-ask "why is this nil?"))
            ;; Control returned before any answer: the viewport is still
            ;; pending.
            (should (overlayp overlay))
            (should-not (string-match-p "B1 FINAL ANSWER"
                                        (overlay-get overlay 'after-string)))
            ;; (d) The command loop stays responsive: a zero-delay timer fires
            ;; before the scripted 0.2s answer.
            (run-at-time 0 nil (lambda () (setq responsive t)))
            (should (journey-fixture-wait-until (lambda () responsive) 2))
            (should-not (string-match-p "B1 FINAL ANSWER"
                                        (overlay-get overlay 'after-string)))
            (let ((run-id (plist-get (overlay-get overlay 'superchat-inline)
                                     :run-id)))
              (should run-id)
              (should (journey-fixture-wait-until
                       (lambda ()
                         (superchat-runtime--terminal-p
                          (plist-get (superchat-runtime-status run-id) :status)))
                       5)))
            (push (journey-buffer--inline-snapshot) snapshots)
            ;; (b) The answer is only in the overlay viewport.
            (should (string-match-p "B1 FINAL ANSWER"
                                    (overlay-get overlay 'after-string)))
            (should-not (string-match-p "B1 FINAL ANSWER"
                                        (buffer-substring-no-properties
                                         (point-min) (point-max))))
            ;; (a) All three snapshots identical.
            (should (equal (nth 2 snapshots) (nth 1 snapshots)))
            (should (equal (nth 2 snapshots) (nth 0 snapshots)))
            ;; (c) The provider's user message carries the reference.
            (let ((prompt (plist-get (journey-fixture-last-request) :prompt-text)))
              (should (string-match-p (regexp-quote (buffer-name source)) prompt))
              (should (string-match-p (regexp-quote (buffer-file-name source))
                                      prompt))
              (should (string-match-p "lines [0-9]+–[0-9]+:" prompt))
              (should (string-match-p (regexp-quote reference) prompt)))
            ;; The inline Session is a real Tape session with the Run
            ;; lifecycle in order.
            (let ((session (superchat-inline-session-id source))
                  (lifecycle '("intent.received" "run.started" "response.completed"
                               "run.completed" "turn.completed")))
              (should (string-prefix-p "inline:" session))
              (should (equal lifecycle
                             (seq-filter (lambda (type) (member type lifecycle))
                                         (journey-fixture-event-types session))))))
        (when (overlayp overlay) (superchat-inline-clear overlay))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (let ((kill-buffer-query-functions nil)) (kill-buffer source)))))))

(provide 'test-journeys-buffer)
;;; test-journeys-buffer.el ends here

(ert-deftest journey/b7-iterate-uses-previous-answer-and-history ()
  "B7: refining a ready rewrite sends the previous answer as the text to
rewrite plus the new instruction; instructions enter the history; the
source stays unchanged; the second result replaces the preview and accept
applies it."
  (journey-fixture-with
      (:model "journey-rewrite-b7-model")
    (let* ((superchat-rewrite-instruction-history nil)
           (source (journey-buffer--rewrite-source
                    " *journey-rewrite-b7*" "JOURNEY_B7_ORIGINAL"))
           before overlay)
      (with-current-buffer source (setq before (journey-buffer--rewrite-snapshot)))
      (journey-fixture-set-script
       (list (list :text "```text\nB7_FIRST_ANSWER\n```")
             (list :text "```text\nB7_SECOND_ANSWER\n```")))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max) "first instruction")))
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (should (equal "first instruction" (car superchat-rewrite-instruction-history)))
      (superchat-rewrite-iterate overlay "second instruction")
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (should (equal "second instruction" (car superchat-rewrite-instruction-history)))
      (should (member "first instruction" superchat-rewrite-instruction-history))
      (let ((prompt (plist-get (journey-fixture-last-request) :prompt-text)))
        (should (string-match-p "B7_FIRST_ANSWER" prompt))
        (should (string-match-p "second instruction" prompt))
        (should-not (string-match-p "JOURNEY_B7_ORIGINAL" prompt)))
      (should (equal "B7_SECOND_ANSWER"
                     (substring-no-properties (overlay-get overlay 'display))))
      (with-current-buffer source
        (should (equal (car before)
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (superchat-rewrite-accept overlay)
      (with-current-buffer source
        (should (equal "B7_SECOND_ANSWER"
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer source))))

(ert-deftest journey/b8-diff-and-merge ()
  "B8: the diff buffer equals the diff of original vs answer with the source
unchanged; merge leaves conflict-marker sections, enables `smerge-mode' and
removes the overlay."
  (journey-fixture-with
      (:model "journey-rewrite-b8-model")
    (let* ((source (journey-buffer--rewrite-source
                    " *journey-rewrite-b8*" "JOURNEY_B8_OLD\n"))
           overlay diff-buffer)
      (journey-fixture-set-script
       (list (list :text "```text\nJOURNEY_B8_NEW\n```")))
      (with-current-buffer source
        (setq overlay (superchat-rewrite-region (point-min) (point-max) "rewrite")))
      (should (journey-buffer--wait-rewrite-status overlay 'ready))
      (setq diff-buffer (superchat-rewrite-diff overlay))
      (unwind-protect
          (progn
            ;; The unified diff's file header names the temporary buffers
            ;; (uniquified per call), so compare the hunk body only.
            (let ((body (lambda (text)
                          (string-join
                           (cl-remove-if (lambda (line)
                                           (string-match-p "\\`\\(---\\|\\+\\+\\+\\|diff \\)" line))
                                         (split-string text "\n"))
                           "\n"))))
              (should (equal (funcall body (superchat-rewrite--diff-text
                                            "JOURNEY_B8_OLD\n" "JOURNEY_B8_NEW"))
                             (funcall body (with-current-buffer diff-buffer
                                             (buffer-substring-no-properties
                                              (point-min) (point-max))))))
              (should (string-match-p "^-JOURNEY_B8_OLD" (with-current-buffer diff-buffer (buffer-string))))
              (should (string-match-p "^\\+JOURNEY_B8_NEW" (with-current-buffer diff-buffer (buffer-string)))))
            (with-current-buffer source
              (should (equal "JOURNEY_B8_OLD\n"
                             (buffer-substring-no-properties (point-min) (point-max)))))
            (superchat-rewrite-merge overlay)
            (with-current-buffer source
              (should (equal "<<<<<<< original\nJOURNEY_B8_OLD\n=======\nJOURNEY_B8_NEW\n>>>>>>> rewrite\n"
                             (buffer-substring-no-properties (point-min) (point-max))))
              (should (bound-and-true-p smerge-mode))
              (should-not (superchat-rewrite-overlays source))))
        (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
        (kill-buffer source)))))

(ert-deftest journey/b11-two-regions-independent-and-noncontiguous ()
  "B11: a rectangle region yields one overlay and one Run per segment; they
update independently, navigation moves between them, a segment whose answer
has no code block shows `error' while the other reaches `ready', and
accepting the ready one leaves the rest of the buffer untouched."
  (journey-fixture-with
      (:model "journey-rewrite-b11-model")
    (let* ((superchat-rewrite--error-display-seconds 30)
           (source (generate-new-buffer " *journey-rewrite-b11*"))
           overlays)
      (with-current-buffer source
        (text-mode)
        (buffer-enable-undo)
        (insert "AA\nBB\n")
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)
        (goto-char (point-min))
        (set-mark (point))
        (activate-mark)
        (forward-line 1) (forward-char 2)
        (rectangle-mark-mode 1)
        (should (region-noncontiguous-p)))
      (journey-fixture-set-script
       (list (list :text "```text\nXX\n```")
             (list :text "no code block here")))
      (with-current-buffer source
        (setq overlays (superchat-rewrite-region (point-min) (point-max) "rewrite")))
      (should (= 2 (length overlays)))
      (should (journey-buffer--wait-rewrite-status (nth 0 overlays) 'ready))
      (should (journey-buffer--wait-rewrite-status (nth 1 overlays) 'error))
      (should (equal "XX" (substring-no-properties (overlay-get (nth 0 overlays) 'display))))
      (with-current-buffer source
        (should (equal "AA\nBB\n" (buffer-substring-no-properties (point-min) (point-max))))
        (deactivate-mark)
        (goto-char (point-min))
        (should (eq (nth 1 overlays) (superchat-rewrite-next)))
        (should (eq (nth 0 overlays) (superchat-rewrite-previous)))
        (superchat-rewrite-accept (nth 0 overlays))
        (should (equal "XX\nBB\n" (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal (list (nth 1 overlays)) (superchat-rewrite-overlays source)))
        (superchat-rewrite-reject-all)
        (should-not (superchat-rewrite-overlays source)))
      (kill-buffer source))))

(ert-deftest journey/b2-reference-cycling-highlights-and-sends-last-range ()
  "B2: each cycle highlights exactly the candidate's bounds in the documented
order; the sent reference equals the last highlighted range; `none' sends
no reference block; leaving the input deletes the highlight."
  (journey-fixture-with
      (:model "journey-inline-b2-model")
    (let* ((directory (journey-fixture-state-data-directory journey-fixture--state))
           (file (expand-file-name "inline-b2.el" directory))
           source overlay)
      (with-temp-file file
        (insert ";; B2_HEAD\n(defun journey-b2 ()\n  (list 'B2_SEXP))\n;; B2_TAIL\n"))
      (setq source (find-file-noselect file))
      (unwind-protect
          (with-current-buffer source
            (emacs-lisp-mode)
            (goto-char (point-min)) (search-forward "B2_SEXP") (backward-char 4)
            ;; (b) order without an active region; (a) highlight = bounds.
            (let (seen)
              (dolist (_ '(1 2 3 4 5 6))
                (let* ((candidate (superchat-inline-cycle-reference))
                       (bounds (superchat-inline-reference-bounds candidate))
                       (highlight superchat-inline--reference-highlight))
                  (push candidate seen)
                  (if (eq candidate 'none)
                      (should-not highlight)
                    (should (equal bounds (cons (overlay-start highlight)
                                                (overlay-end highlight)))))))
              (should (equal '(none defun sexp line window buffer) (reverse seen))))
            ;; Select `sexp' again and send: the reference block equals it.
            (superchat-inline-cycle-reference) ; none
            (superchat-inline-cycle-reference) ; defun
            (superchat-inline-cycle-reference) ; sexp
            (let* ((highlight superchat-inline--reference-highlight)
                   (range (cons (overlay-start highlight) (overlay-end highlight)))
                   (expected (buffer-substring-no-properties (car range) (cdr range))))
              (journey-fixture-set-script (list (list :text "b2 answer")))
              (setq overlay (superchat-inline-ask "what is this?"))
              ;; (d) starting the ask consumed the selection: no highlight left.
              (should-not superchat-inline--reference-highlight)
              (should-not (cl-find-if (lambda (o) (overlay-get o 'superchat-inline-reference))
                                      (overlays-in (point-min) (point-max))))
              (should (journey-fixture-wait-until
                       (lambda () (eq 'ready (plist-get (overlay-get overlay 'superchat-inline)
                                                        :status)))
                       5))
              (let ((prompt (plist-get (journey-fixture-last-request) :prompt-text)))
                (should (string-match-p (regexp-quote expected) prompt))
                (should-not (string-match-p "B2_HEAD" prompt)))
              (superchat-inline-clear overlay))
            ;; `none' → no reference block at all.
            (journey-fixture-set-script (list (list :text "b2 answer two")))
            (setq overlay (superchat-inline-ask "and now?" 'none))
            (should (journey-fixture-wait-until
                     (lambda () (eq 'ready (plist-get (overlay-get overlay 'superchat-inline)
                                                      :status)))
                     5))
            (let ((prompt (plist-get (journey-fixture-last-request) :prompt-text)))
              (should-not (string-match-p "B2_SEXP" prompt))
              (should-not (string-match-p "In buffer" prompt)))
            (superchat-inline-clear overlay)
            ;; (d) the minibuffer exit hook clears a pending highlight.
            (superchat-inline-cycle-reference)
            (superchat-inline-cycle-reference)
            (should superchat-inline--reference-highlight)
            (superchat-inline--clear-reference-highlight)
            (should-not superchat-inline--reference-highlight))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (let ((kill-buffer-query-functions nil)) (kill-buffer source)))))))

(ert-deftest journey/b3-reply-keeps-history-and-visit-returns ()
  "B3: a reply's request carries the first question and answer as history;
still one inline overlay; the inline Session has two turns on the Tape;
visit + return restore buffer, point and window configuration."
  (journey-fixture-with
      (:model "journey-inline-b3-model")
    (let* ((directory (journey-fixture-state-data-directory journey-fixture--state))
           (file (expand-file-name "inline-b3.el" directory))
           source overlay session-id)
      (with-temp-file file (insert "(defun journey-b3 () 'B3_REF)\n"))
      (setq source (find-file-noselect file))
      (unwind-protect
          (with-current-buffer source
            (emacs-lisp-mode)
            (goto-char (point-min)) (forward-char 3)
            (journey-fixture-set-script
             (list (list :text "B3_FIRST_ANSWER")
                   (list :text "B3_SECOND_ANSWER")))
            (setq overlay (superchat-inline-ask "B3_FIRST_QUESTION"))
            (setq session-id (plist-get (overlay-get overlay 'superchat-inline) :session-id))
            (should (journey-fixture-wait-until
                     (lambda () (eq 'ready (plist-get (overlay-get overlay 'superchat-inline)
                                                      :status)))
                     5))
            (superchat-inline-reply overlay "B3_SECOND_QUESTION")
            (should (journey-fixture-wait-until
                     (lambda () (and (eq 'ready (plist-get (overlay-get overlay 'superchat-inline)
                                                           :status))
                                     (string-match-p "B3_SECOND_ANSWER"
                                                     (overlay-get overlay 'after-string))))
                     5))
            ;; (a) history reached the second request.
            (let ((prompt (plist-get (journey-fixture-last-request) :prompt-text)))
              (should (string-match-p "B3_FIRST_QUESTION" prompt))
              (should (string-match-p "B3_FIRST_ANSWER" prompt))
              (should (string-match-p "B3_SECOND_QUESTION" prompt))
              (should (string-match-p "B3_REF" prompt)))
            ;; (b) exactly one inline overlay.
            (should (= 1 (length (superchat-inline-overlays source))))
            ;; (c) two turns on the Tape.
            (let ((types (journey-fixture-event-types session-id)))
              (should (= 2 (cl-count "intent.received" types :test #'equal)))
              (should (= 2 (cl-count "response.completed" types :test #'equal))))
            ;; (d) visit and return.
            (let ((buffer-before (current-buffer))
                  (point-before (point))
                  (config-before (current-window-configuration)))
              (superchat-inline-visit-session overlay)
              (should (get-buffer (superchat--session-buffer-name session-id)))
              (with-current-buffer (superchat--session-buffer-name session-id)
                (should (string-match-p "B3_FIRST_ANSWER" (buffer-string)))
                (should (string-match-p "B3_SECOND_ANSWER" (buffer-string))))
              (superchat-inline-return)
              (should (eq buffer-before (current-buffer)))
              (should (= point-before (point)))
              ;; The visit may switch tabs, which a window configuration does
              ;; not cover; what the user sees is the selected window showing
              ;; the source buffer again at the same point.
              (should (eq buffer-before (window-buffer (selected-window))))
              (should (window-configuration-p config-before)))
            (superchat-inline-clear overlay))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (let ((kill-buffer-query-functions nil)) (kill-buffer source)))
        (when-let* ((view (and session-id (get-buffer (superchat--session-buffer-name session-id)))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer view)))))))
