;;; rewrite-target-regression.el --- a region rewrite replaces only its target -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-09-26-region-rewrite-target-and-reference.md.
;; The canonical smoke fixture supplies the Session, the Tape and a scripted
;; model; no real model and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/rewrite-target-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^rewrite-target/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-rewrite)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; Fixture ------------------------------------------------------------------

(defun rt--insert (text)
  "Insert TEXT, whose target is between ⟦ and ⟧; return (BEG . END)."
  (let (beg end)
    (dolist (piece (split-string text "\\(⟦\\|⟧\\)"))
      (insert piece)
      (cond ((null beg) (setq beg (point)))
            ((null end) (setq end (point)))))
    ;; `split-string' yields three pieces: before, target, after.
    (cons beg end)))

(defmacro rt--with-rewrite (spec &rest body)
  "Rewrite in a buffer described by SPEC, then run BODY.
SPEC is (MODE TEXT ANSWERS &optional INSTRUCTION).  TEXT marks the target
with ⟦ ⟧; ANSWERS are the scripted model steps.  BODY sees `overlay',
`buffer', `target' (the original target text) and `requests'."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-rewrite-confirmation 'overlay))
       (smoke--with-fixture ,(nth 2 spec)
         (let ((buffer (generate-new-buffer "rt-probe")))
           (unwind-protect
               (with-current-buffer buffer
                 (let* ((org-startup-folded nil)
                        (_ (funcall ,(nth 0 spec)))
                        (bounds (rt--insert ,(nth 1 spec)))
                        (target (buffer-substring-no-properties (car bounds) (cdr bounds)))
                        (transient-mark-mode t)
                        (_ (progn (goto-char (car bounds)) (push-mark (cdr bounds) t t)))
                        (overlay (superchat-rewrite-region (car bounds) (cdr bounds)
                                                           ,(or (nth 3 spec) "RT-INSTRUCTION")))
                        (overlay (if (listp overlay) (car overlay) overlay)))
                   (ignore target)
                   (should (smoke--wait
                            (lambda () (memq (plist-get (superchat-rewrite--state overlay) :status)
                                             '(ready error)))
                            20))
                   (let ((requests (reverse smoke--requests)))
                     (ignore requests)
                     ,@body)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun rt--state (overlay key)
  (plist-get (superchat-rewrite--state overlay) key))

(defun rt--sent (overlay)
  "The user message of OVERLAY's latest request: the Tape's last user row."
  (let* ((session (rt--state overlay :session-id))
         (rows (cl-remove-if-not (lambda (row) (equal "user" (nth 2 row)))
                                 (superchat-db-tape-replay session))))
    (should rows)
    (nth 3 (car (last rows)))))

(defun rt--system (overlay)
  "The system text of OVERLAY's latest request header, from the Tape."
  (let* ((session (rt--state overlay :session-id))
         (events (superchat-tape-replay-events session))
         (header (car (last (cl-remove-if-not
                             (lambda (event) (equal "request.header" (plist-get event :type)))
                             events)))))
    (should header)
    (plist-get (plist-get header :payload) :system)))

(defun rt--prev-section (lines)
  (mapconcat (lambda (n) (format "RT-PREV-%d 上一篇的内容。" n))
             (number-sequence 1 lines) "\n"))

(defconst rt--diary
  (concat "* 日记\n"
          "**** 看电影 #log\n"
          (rt--prev-section 5) "\n\n\n"
          "**** 刘欢去世 #log\n"
          ":PROPERTIES:\n:ID:       RT-ID\n:END:\n\n"
          "⟦看到一篇关于刘欢讲课的结语：⟧\n\n"
          "#+begin_quote\nRT-QUOTE 音乐真的没有什么作用。\n#+end_quote\n"
          "**** 下一篇 #log\n"
          "RT-NEXT 下一篇的内容。\n")
  "The observed shape: a short target under a new heading, a previous
section within 20 lines above it, and a next section below.")

;;;; What is sent --------------------------------------------------------------

(ert-deftest rewrite-target/the-diary-case-sends-the-section-and-replaces-only-the-target ()
  (rt--with-rewrite (#'org-mode rt--diary '((:text "看到一篇刘欢讲课的结语：")) "润色这一段")
    (let ((sent (rt--sent overlay)))
      (should (string-match-p "润色这一段" sent))
      (should (string-match-p (regexp-quote target) sent))
      ;; The target's own section is the reference ...
      (should (string-match-p "RT-QUOTE" sent))
      (should (string-match-p "刘欢去世" sent))
      ;; ... and nothing of another section is sent.
      (should-not (string-match-p "RT-PREV" sent))
      (should-not (string-match-p "RT-NEXT" sent))
      ;; The target and the reference are told apart, and the reference is
      ;; said to be read-only.
      (should (string-match-p (concat "<<<TARGET>>>\n" (regexp-quote target) "\n<<<END TARGET>>>")
                              sent))
      (should (string-match-p "<<<REFERENCE BEFORE>>>" sent))
      (should (string-match-p "do not change it" sent))
      ;; The instruction comes after the target.
      (should (< (string-match "<<<END TARGET>>>" sent) (string-match "润色这一段" sent))))
    ;; A plain answer is the replacement; accepting changes only the target.
    (should (eq 'ready (rt--state overlay :status)))
    (let ((before (buffer-string)))
      (superchat-rewrite-accept overlay)
      (should (equal (replace-regexp-in-string (regexp-quote "看到一篇关于刘欢讲课的结语：")
                                               "看到一篇刘欢讲课的结语：" before t t)
                     (buffer-string))))))

(ert-deftest rewrite-target/text-and-code-get-their-own-system-text ()
  (rt--with-rewrite (#'org-mode "abc ⟦def⟧ ghi\n" '((:text "DEF")))
    (let ((system (rt--system overlay)))
      (should (string-match-p "editor" system))
      (should-not (string-match-p "code editor\\|provided code\\|rewritten code\\|of code" system))
      (should (string-match-p "[Oo]rg" system))
      (should (string-match-p "only the replacement" system))
      (should (string-match-p "no code fence" system))))
  (rt--with-rewrite (#'emacs-lisp-mode "(defun rt-f ()\n  ⟦(+ 1 2)⟧)\n" '((:text "(+ 2 1)")))
    (let ((system (rt--system overlay)))
      (should (string-match-p "code" system))
      (should (string-match-p "[Ee]macs.[Ll]isp" system))
      (should (string-match-p "only the replacement" system))
      (should (string-match-p "no code fence" system)))))

(ert-deftest rewrite-target/a-customized-system-text-still-wins ()
  (let ((superchat-rewrite-system-prompt "RT-MY-OWN-SYSTEM-TEXT"))
    (rt--with-rewrite (#'org-mode "abc ⟦def⟧ ghi\n" '((:text "DEF")))
      (should (string-match-p "RT-MY-OWN-SYSTEM-TEXT" (rt--system overlay))))))

(ert-deftest rewrite-target/code-reference-is-the-enclosing-definition-and-some-lines ()
  (let ((superchat-rewrite-context-lines 2))
    (rt--with-rewrite
        (#'emacs-lisp-mode
         (concat "(defun rt-far ()\n  \"RT-FAR\")\n"
                 (make-string 10 ?\n)
                 "(defun rt-middle ()\n  \"RT-MIDDLE-START\"\n"
                 (mapconcat (lambda (n) (format "  (ignore %d)" n)) (number-sequence 1 12) "\n")
                 "\n  ⟦(+ 1 2)⟧\n"
                 (mapconcat (lambda (n) (format "  (ignore %d)" n)) (number-sequence 13 24) "\n")
                 "\n  \"RT-MIDDLE-END\")\n"
                 (make-string 10 ?\n)
                 "(defun rt-other ()\n  \"RT-OTHER\")\n")
         '((:text "(+ 2 1)")))
      (let ((sent (rt--sent overlay)))
        (should (string-match-p "RT-MIDDLE-START" sent))
        (should (string-match-p "RT-MIDDLE-END" sent))
        (should-not (string-match-p "RT-FAR" sent))
        (should-not (string-match-p "RT-OTHER" sent))))))

(ert-deftest rewrite-target/other-modes-use-the-line-count-and-a-size-cap ()
  (let ((superchat-rewrite-context-lines 3))
    (rt--with-rewrite (#'fundamental-mode
                       "RT-L1\nRT-L2\nRT-L3\nRT-L4\n⟦middle⟧\nRT-L6\nRT-L7\nRT-L8\nRT-L9\n"
                       '((:text "MIDDLE")))
      (let ((sent (rt--sent overlay)))
        (should (string-match-p "RT-L2" sent))
        (should (string-match-p "RT-L8" sent))
        (should-not (string-match-p "RT-L1" sent))
        (should-not (string-match-p "RT-L9" sent)))))
  (let ((superchat-rewrite-context-lines 20))
    (rt--with-rewrite (#'text-mode
                       (concat (mapconcat (lambda (n) (format "RT-FAR-%02d %s" n (make-string 1000 ?a)))
                                          (number-sequence 1 20) "\n")
                               "\nRT-NEAR\n⟦middle⟧\n")
                       '((:text "MIDDLE")))
      (let ((sent (rt--sent overlay)))
        (should (string-match-p "RT-NEAR" sent))
        (should-not (string-match-p "RT-FAR-01 " sent))
        (should (< (length sent) 9000))))))

;;;; What comes back ------------------------------------------------------------

(ert-deftest rewrite-target/a-fenced-answer-is-unwrapped-and-a-plain-one-kept ()
  (rt--with-rewrite (#'org-mode "abc ⟦def⟧ ghi\n" '((:text "```org\nDEF\n```")))
    (should (eq 'ready (rt--state overlay :status)))
    (should (equal "DEF" (rt--state overlay :text))))
  (rt--with-rewrite (#'org-mode "abc ⟦def⟧ ghi\n" '((:text "DEF PLAIN")))
    (should (eq 'ready (rt--state overlay :status)))
    (should (equal "DEF PLAIN" (rt--state overlay :text)))))

(ert-deftest rewrite-target/the-trailing-newline-follows-the-target ()
  (rt--with-rewrite (#'org-mode "⟦line one\n⟧rest\n" '((:text "```\nLINE ONE\n```")))
    (should (eq 'ready (rt--state overlay :status)))
    (should (equal "LINE ONE\n" (rt--state overlay :text))))
  (rt--with-rewrite (#'org-mode "⟦word⟧ rest\n" '((:text "WORD\n")))
    (should (eq 'ready (rt--state overlay :status)))
    (should (equal "WORD" (rt--state overlay :text)))))

(ert-deftest rewrite-target/an-answer-carrying-the-markers-is-refused ()
  ;; The observed answer: the reference and the markers came back.
  (rt--with-rewrite (#'org-mode rt--diary
                     '((:text "```org-mode\n上一篇润色后。\n\n<<<REGION>>>\n看到一篇关于刘欢讲课的结语：\n<<<END>>>\n```")))
    (should (eq 'error (rt--state overlay :status)))
    (should (string-match-p "marker" (rt--state overlay :error)))
    (should-error (superchat-rewrite-accept overlay) :type 'user-error))
  (rt--with-rewrite (#'org-mode "abc ⟦def⟧ ghi\n" '((:text "<<<TARGET>>>\nDEF\n<<<END TARGET>>>")))
    (should (eq 'error (rt--state overlay :status)))))

(ert-deftest rewrite-target/a-much-longer-answer-is-flagged-not-refused ()
  (rt--with-rewrite (#'org-mode "⟦看到一篇关于刘欢讲课的结语：⟧\n"
                     `((:text ,(concat "```\n" (make-string 600 ?长) "\n```"))))
    (should (eq 'ready (rt--state overlay :status)))
    (let ((line (superchat-rewrite--status-line (superchat-rewrite--state overlay))))
      (should (string-match-p "longer" line))
      (should (string-match-p "check" line))))
  ;; Twice as long is an ordinary rewrite.
  (rt--with-rewrite (#'org-mode "⟦看到一篇关于刘欢讲课的结语：⟧\n"
                     '((:text "看到一篇关于歌唱家刘欢在课堂上讲课时所说的结语：")))
    (should (eq 'ready (rt--state overlay :status)))
    (should-not (string-match-p "longer" (superchat-rewrite--status-line
                                          (superchat-rewrite--state overlay))))))

;;;; What the user sees --------------------------------------------------------

(ert-deftest rewrite-target/the-typing-preview-shows-no-fence ()
  (skip-when (getenv "SMOKE_REAL"))
  (with-temp-buffer
    (insert "alpha beta\n")
    (let ((overlay (superchat-rewrite--make-overlay
                    1 6 (list :status 'typing :original "alpha" :text "```org\nALP"))))
      (should (equal "ALP" (overlay-get overlay 'display)))
      (superchat-rewrite--set-state overlay :text "```org\nALPHA\n```")
      (should (equal "ALPHA" (overlay-get overlay 'display)))
      ;; Text without a fence is shown as it is.
      (superchat-rewrite--set-state overlay :text "ALPHA ```x``` inline")
      (should (equal "ALPHA ```x``` inline" (overlay-get overlay 'display))))))

(ert-deftest rewrite-target/the-prompt-names-the-target ()
  (skip-when (getenv "SMOKE_REAL"))
  (with-temp-buffer
    (let ((bounds (rt--insert "前文\n⟦看到一篇关于刘欢讲课的结语：⟧\n后文\n"))
          (transient-mark-mode t)
          (asked nil))
      (goto-char (car bounds))
      (push-mark (cdr bounds) t t)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (setq asked prompt) (signal 'quit nil))))
        (condition-case nil (call-interactively #'superchat-rewrite-region)
          (quit nil)))
      (should (stringp asked))
      (should (string-match-p "14 characters" asked))
      (should (string-match-p "看到一篇关于刘欢讲课的结语" asked))))
  ;; Several lines are counted as lines, and a long target is shortened.
  (with-temp-buffer
    (let ((bounds (rt--insert (concat "⟦" (mapconcat (lambda (n) (format "第%d行%s" n (make-string 40 ?字)))
                                                    (number-sequence 1 3) "\n")
                                     "⟧\n")))
          (transient-mark-mode t)
          (asked nil))
      (goto-char (car bounds))
      (push-mark (cdr bounds) t t)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (setq asked prompt) (signal 'quit nil))))
        (condition-case nil (call-interactively #'superchat-rewrite-region)
          (quit nil)))
      (should (string-match-p "3 lines" asked))
      (should (< (length asked) 120)))))

(ert-deftest rewrite-target/the-ready-line-names-the-diff-key ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((line (superchat-rewrite--status-line '(:status ready))))
    (should (string-match-p "C-c C-a" line))
    (should (string-match-p "C-c C-d" line))))

;;;; Unchanged ----------------------------------------------------------------

(ert-deftest rewrite-target/no-selection-no-rewrite ()
  "Guard: without an active region the command says so."
  (skip-when (getenv "SMOKE_REAL"))
  (with-temp-buffer
    (insert "abc\n")
    (let ((transient-mark-mode t))
      (deactivate-mark)
      (should-error (call-interactively #'superchat-rewrite-region) :type 'user-error))))

(ert-deftest rewrite-target/a-refinement-rewrites-the-previous-answer-alone ()
  (rt--with-rewrite (#'org-mode rt--diary '((:text "```\nFIRST ANSWER\n```") (:text "```\nSECOND ANSWER\n```")))
    (should (eq 'ready (rt--state overlay :status)))
    (superchat-rewrite-iterate overlay "RT-REFINE")
    (should (smoke--wait (lambda () (and (eq 'ready (rt--state overlay :status))
                                         (equal "SECOND ANSWER" (rt--state overlay :text))))
                         20))
    (let ((sent (rt--sent overlay)))
      (should (string-match-p "RT-REFINE" sent))
      (should (string-match-p "<<<TARGET>>>\nFIRST ANSWER\n<<<END TARGET>>>" sent))
      (should-not (string-match-p "REFERENCE" sent))
      (should-not (string-match-p "RT-QUOTE" sent)))))

(provide 'rewrite-target-regression)

;;; rewrite-target-regression.el ends here
