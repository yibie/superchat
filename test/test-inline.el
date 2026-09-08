;;; test-inline.el --- ERT tests for superchat-inline -*- lexical-binding: t; -*-

;;; Commentary:

;; S18a phase 1: the inline ask session id, reference builder, overlay
;; viewport, collector, and the overlay Sink driven directly.  No model
;; transport and no Runtime submission here; the end-to-end journey lands
;; with phase 2.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat-inline)
(require 'journey-fixture
         (expand-file-name "journey-fixture" (file-name-directory load-file-name)))

(defun inline-test--undo-length ()
  "Return the current buffer's undo entry count.
A non-list `buffer-undo-list' t means undo recording is disabled, which
new buffers start with; count that as zero entries."
  (if (listp buffer-undo-list) (length buffer-undo-list) 0))

(defun inline-test--snapshot ()
  "Return the current buffer's text, modified flag and undo length."
  (list (buffer-string) (buffer-modified-p) (inline-test--undo-length)))

(defun inline-test--assert-unchanged (before)
  "Assert the current buffer still matches BEFORE.
BEFORE is an `inline-test--snapshot'."
  (should (equal (nth 0 before) (buffer-string)))
  (should (eq (nth 1 before) (buffer-modified-p)))
  (should (= (nth 2 before) (inline-test--undo-length))))

(defun inline-test--state (&optional text)
  "Return a fresh overlay state plist with answer TEXT."
  (list :run-id nil
        :session-id "inline:test"
        :question "why?"
        :status 'waiting
        :text (or text "")))

(defun inline-test--overlay (&optional text)
  "Create an inline overlay over the current line with answer TEXT."
  (superchat-inline--make-overlay
   (line-beginning-position) (line-end-position) (inline-test--state text)))

;; ═══════════════════════════════════════════════════════════
;; Session id
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/session-id-per-project-and-global-fallback ()
  "A project buffer gets one inline Session per project; else global."
  (let* ((project-dir (file-truename (make-temp-file "superchat-inline-project" t)))
         (plain-dir (file-truename (make-temp-file "superchat-inline-plain" t)))
         (project-buffer (generate-new-buffer " *superchat-inline-project*"))
         (plain-buffer (generate-new-buffer " *superchat-inline-plain*")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" project-dir) t)
          (with-current-buffer project-buffer
            (setq default-directory project-dir)
            (let ((before (inline-test--snapshot)))
              (should (equal (format "inline:%s"
                                     (file-name-as-directory project-dir))
                             (superchat-inline-session-id)))
              (let ((superchat-inline-session-scope 'global))
                (should (equal "inline:global"
                               (superchat-inline-session-id))))
              (inline-test--assert-unchanged before)))
          (with-current-buffer plain-buffer
            (setq default-directory plain-dir)
            (let ((before (inline-test--snapshot)))
              (should (equal "inline:global" (superchat-inline-session-id)))
              (inline-test--assert-unchanged before))))
      (when (buffer-live-p project-buffer) (kill-buffer project-buffer))
      (when (buffer-live-p plain-buffer) (kill-buffer plain-buffer))
      (delete-directory project-dir t)
      (delete-directory plain-dir t))))

;; ═══════════════════════════════════════════════════════════
;; Reference
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/reference-defaults-to-defun-then-line ()
  "The reference is the defun at point in prog-mode, else the current line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun inline-sample ()\n  \"Doc.\"\n  42)\n\nlast line\n")
    (let ((before (inline-test--snapshot)))
      (goto-char (point-min))
      (search-forward "42")
      (let* ((reference (superchat-inline--normalize-reference nil))
             (beg (plist-get reference :beg))
             (end (plist-get reference :end))
             (block (plist-get reference :block)))
        (should (string-prefix-p "(defun inline-sample"
                                 (buffer-substring-no-properties beg end)))
        (should (string-match-p (regexp-quote
                                 (buffer-substring-no-properties beg end))
                                block))
        (should (string-match-p "In buffer \"" block))
        (should (string-match-p "lines 1–3:" block))
        (should (string-match-p "```emacs-lisp\n" block)))
      ;; Outside a defun, the current line is the reference.
      (goto-char (point-max))
      (forward-line -1)
      (let ((block (plist-get (superchat-inline--normalize-reference nil)
                              :block)))
        (should (string-match-p "last line" block))
        (should (string-match-p "lines 5–5:" block))
        (should (string-match-p "```emacs-lisp\n" block)))
      (inline-test--assert-unchanged before))))

(ert-deftest inline/reference-accepts-an-explicit-range ()
  "An explicit (BEG . END) overrides the reference at point."
  (with-temp-buffer
    (insert "first line\nsecond line\n")
    (let ((reference (superchat-inline--normalize-reference '(1 . 10))))
      (should (= 1 (plist-get reference :beg)))
      (should (= 10 (plist-get reference :end)))
      (should (string-match-p "first lin" (plist-get reference :block))))))

;; ═══════════════════════════════════════════════════════════
;; Overlay viewport
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/overlay-viewport-shows-last-n-lines ()
  "The after-string carries the status header and the last N answer lines."
  (let ((superchat-inline-viewport-lines 3))
    (with-temp-buffer
      (insert "source line\n")
      (let ((before (inline-test--snapshot)))
        (goto-char (point-min))
        (let ((overlay (inline-test--overlay
                        "line-1\nline-2\nline-3\nline-4\nline-5")))
          (unwind-protect
              (progn
                (superchat-inline--set-state overlay :status 'ready)
                (let ((after (overlay-get overlay 'after-string)))
                  (should (string-match-p
                           "\\`superchat inline · Ready · M-RET menu\n" after))
                  (should (string-match-p "\nline-3\nline-4\nline-5\n\\'" after))
                  (should-not (string-match-p "line-2" after))
                  (should (string-suffix-p "\n" after)))
                (should (equal "line-1\nline-2\nline-3\nline-4\nline-5"
                               (plist-get (overlay-get overlay 'superchat-inline)
                                          :text)))
                (should (eq superchat-inline--overlay-priority
                            (overlay-get overlay 'priority)))
                (should-not (overlay-get overlay 'evaporate))
                (should (eq superchat-inline--keymap
                            (overlay-get overlay 'keymap)))
                (should (equal (list overlay) (superchat-inline-overlays)))
                (inline-test--assert-unchanged before))
            (when (overlayp overlay) (delete-overlay overlay))))))))

(ert-deftest inline/overlay-covers-an-empty-line ()
  "An empty line's overlay still covers one character."
  (with-temp-buffer
    (insert "one\n\ntwo\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((bounds (superchat-inline--overlay-bounds)))
      (should (= 1 (- (cdr bounds) (car bounds)))))))

;; ═══════════════════════════════════════════════════════════
;; Overlay Sink
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/sink-streams-status-final-and-dies-with-the-overlay ()
  "Driving the overlay Sink updates the viewport; deleting it kills the Sink."
  (with-temp-buffer
    (insert "source line\n")
    (let ((before (inline-test--snapshot)))
      (goto-char (point-min))
      (let* ((overlay (inline-test--overlay))
             (sink (superchat-inline--sink overlay))
             (run '(:run-id "run-inline" :session-id "inline:test")))
        (unwind-protect
            (progn
              (should (superchat-sink-live-p sink))
              (superchat-sink-emit sink 'status run 'waiting)
              (should (string-match-p "Waiting…" (overlay-get overlay 'after-string)))
              (superchat-sink-emit sink 'stream run (list :kind 'text :text "first "))
              (superchat-sink-emit sink 'stream run (list :kind 'text :text "answer"))
              (should (equal "first answer"
                             (plist-get (overlay-get overlay 'superchat-inline)
                                        :text)))
              (should (string-match-p "Typing…" (overlay-get overlay 'after-string)))
              (should (string-match-p "first answer"
                                      (overlay-get overlay 'after-string)))
              (superchat-sink-emit sink 'status run 'ready)
              (should (string-match-p "Ready · M-RET menu"
                                      (overlay-get overlay 'after-string)))
              (superchat-sink-emit sink 'final run
                                   (list :text "final answer" :tape-id 7))
              (should (equal "final answer"
                             (plist-get (overlay-get overlay 'superchat-inline)
                                        :text)))
              (should (string-match-p "final answer"
                                      (overlay-get overlay 'after-string)))
              (should (eq 'ready
                          (plist-get (overlay-get overlay 'superchat-inline)
                                     :status)))
              (inline-test--assert-unchanged before)
              (delete-overlay overlay)
              (should-not (superchat-sink-live-p sink)))
          (when (overlayp overlay) (delete-overlay overlay)))))))

(ert-deftest inline/sink-error-is-shown-then-cleared ()
  "An error shows in the viewport and its scheduled clear removes it."
  (with-temp-buffer
    (insert "source line\n")
    (let ((before (inline-test--snapshot)))
      (goto-char (point-min))
      (let* ((overlay (inline-test--overlay))
             (sink (superchat-inline--sink overlay))
             (run '(:run-id "run-inline" :session-id "inline:test")))
        (unwind-protect
            (progn
              (superchat-sink-emit sink 'error run
                                   (list :status 'failed
                                         :text "provider exploded"
                                         :reason nil :tape-id nil))
              (should (string-match-p "Error: provider exploded"
                                      (overlay-get overlay 'after-string)))
              (let ((timer (plist-get (overlay-get overlay 'superchat-inline)
                                      :error-timer)))
                (should (timerp timer))
                ;; Run the scheduled callback instead of waiting five seconds.
                (funcall (timer--function timer)))
              (should-not (superchat-inline-overlays))
              (inline-test--assert-unchanged before))
          (when (overlayp overlay)
            (let ((timer (plist-get (overlay-get overlay 'superchat-inline)
                                    :error-timer)))
              (when (timerp timer) (cancel-timer timer)))
            (delete-overlay overlay)))))))

;; ═══════════════════════════════════════════════════════════
;; Clear, copy, menu
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/copy-and-menu-clear-the-answer ()
  "Copy puts the answer on the kill ring; the menu's clear removes it."
  (with-temp-buffer
    (insert "source line\n")
    (let ((before (inline-test--snapshot)))
      (goto-char (point-min))
      (let ((overlay (inline-test--overlay "menu answer")))
        (unwind-protect
            (progn
              (superchat-inline-copy overlay)
              (should (equal "menu answer" (car kill-ring)))
              (cl-letf (((symbol-function 'read-multiple-choice)
                         (lambda (&rest _args) '(?c "clear"))))
                (superchat-inline-menu overlay))
              (should-not (superchat-inline-overlays))
              (inline-test--assert-unchanged before))
          (when (overlayp overlay) (delete-overlay overlay)))))))

;; ═══════════════════════════════════════════════════════════
;; Collector
;; ═══════════════════════════════════════════════════════════

(ert-deftest inline/collector-emits-one-hard-attachment-fragment ()
  "The registered collector emits the envelope's reference as one fragment."
  (let* ((report (superchat-stage-report))
         (collect (cl-find 'collect-context (plist-get report :stages)
                           :key (lambda (stage) (plist-get stage :stage))))
         (implementation (cl-find 'inline-reference
                                  (plist-get collect :implementations)
                                  :key (lambda (impl) (plist-get impl :id)))))
    (should implementation)
    (should (eq 'inline (plist-get implementation :owner))))
  (let* ((envelope (list :context
                         (list :inline-reference-text "REF BLOCK"
                               :inline-reference-label "REF LABEL")))
         (fragments (superchat-inline--collect-reference envelope nil nil)))
    (should (= 1 (length fragments)))
    (should (equal 'inline (plist-get (car fragments) :owner)))
    (should (equal "inline-reference" (plist-get (car fragments) :id)))
    (should (eq 'attachment (plist-get (car fragments) :slot)))
    (should (equal "REF BLOCK" (plist-get (car fragments) :text)))
    (should (eq 'hard (plist-get (car fragments) :budget-class)))
    (should (plist-get (car fragments) :required)))
  (should-not (superchat-inline--collect-reference (list :context nil) nil nil))
  (should-not (superchat-inline--collect-reference nil nil nil)))

(ert-deftest inline/clear-removes-overlay-and-cancels-running-run ()
  "Clearing an overlay removes it and cancels its still-running Run."
  (journey-fixture-with
      (:script (list (list :chunks '("INLINE PARTIAL") :hang t)))
    (let* ((directory (journey-fixture-state-data-directory journey-fixture--state))
           (file (expand-file-name "inline-clear.el" directory))
           source overlay run-id)
      (with-temp-file file (insert "(defun inline-clear ()\n  1)\n"))
      (setq source (find-file-noselect file))
      (unwind-protect
          (progn
            (with-current-buffer source
              (emacs-lisp-mode)
              (goto-char (point-min))
              (search-forward "1)")
              (setq overlay (superchat-inline-ask "keep it running?"))
              (setq run-id (plist-get (overlay-get overlay 'superchat-inline)
                                      :run-id)))
            (should run-id)
            (should (journey-fixture-wait-until
                     (lambda () (member run-id (superchat-runtime-active-runs)))
                     5))
            (superchat-inline-clear overlay)
            (should-not (superchat-inline-overlays source))
            (should (journey-fixture-wait-until
                     (lambda ()
                       (eq 'cancelled
                           (plist-get (superchat-runtime-status run-id)
                                      :status)))
                     5))
            (should-not (member run-id (superchat-runtime-active-runs)))
            (with-current-buffer source
              (should-not (buffer-modified-p))))
        (when (overlayp overlay) (superchat-inline-clear overlay))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (let ((kill-buffer-query-functions nil)) (kill-buffer source)))))))

(provide 'test-inline)
;;; test-inline.el ends here

;; S18b: reference candidates

(ert-deftest inline/reference-bounds-per-candidate-in-elisp-and-text ()
  "`superchat-inline-reference-bounds' returns each candidate's range at point;
`none' is nil, `region' only while the region is active."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; head\n(defun inline-a ()\n  (list 1 2))\n;; tail\n")
    (goto-char (point-min)) (forward-line 2) (forward-char 2) ; on the "(" of (list …)
    (should-not (superchat-inline-reference-bounds 'none))
    (should-not (superchat-inline-reference-bounds 'region))
    (should (equal "(defun inline-a ()\n  (list 1 2))"
                   (let ((b (superchat-inline-reference-bounds 'defun)))
                     (string-trim-right
                      (buffer-substring-no-properties (car b) (cdr b))))))
    (should (equal "(list 1 2)"
                   (let ((b (superchat-inline-reference-bounds 'sexp)))
                     (buffer-substring-no-properties (car b) (cdr b)))))
    (should (equal "  (list 1 2))"
                   (let ((b (superchat-inline-reference-bounds 'line)))
                     (buffer-substring-no-properties (car b) (cdr b)))))
    (should (equal (cons (point-min) (point-max))
                   (superchat-inline-reference-bounds 'buffer)))
    (should (superchat-inline-reference-bounds 'window))
    (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
    (should (equal (cons (point-min) (point-max))
                   (superchat-inline-reference-bounds 'region)))
    (should (equal '(region none defun sexp line window buffer)
                   (superchat-inline--candidates))))
  (with-temp-buffer
    (text-mode)
    (insert "First sentence.  Second sentence.\n\nNext paragraph.\n")
    (goto-char (point-min))
    (should (equal '(region none line sentence paragraph window buffer)
                   (superchat-inline--candidates)))
    (should (equal "First sentence."
                   (let ((b (superchat-inline-reference-bounds 'sentence)))
                     (string-trim (buffer-substring-no-properties (car b) (cdr b))))))
    (should (superchat-inline-reference-bounds 'paragraph))))

(ert-deftest inline/cycle-reference-orders-highlights-and-none-clears ()
  "Cycling follows the mode order (region only when active), highlights the
selected range with the `superchat-inline-reference' overlay, and `none'
removes the highlight; clearing restores the header line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun inline-b () 1)\n")
    (goto-char 5)
    (let (order)
      (dotimes (_ 6) (push (superchat-inline-cycle-reference) order))
      (should (equal '(none defun sexp line window buffer) (reverse order))))
    ;; `line' selected: highlight equals the line bounds.
    (superchat-inline-cycle-reference) ; wraps to none
    (should-not superchat-inline--reference-highlight)
    (superchat-inline-cycle-reference) ; defun
    (should (overlayp superchat-inline--reference-highlight))
    (should (equal (superchat-inline-reference-bounds 'defun)
                   (cons (overlay-start superchat-inline--reference-highlight)
                         (overlay-end superchat-inline--reference-highlight))))
    (should (eq 'defun (overlay-get superchat-inline--reference-highlight
                                    'superchat-inline-reference)))
    (should (equal "Including defun" header-line-format))
    (superchat-inline--clear-reference-highlight)
    (should-not superchat-inline--reference-highlight)
    (should-not header-line-format)))
