;;; workspace-recall-regression.el --- recall stays inside the workspace -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 2.4 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-2-rules-and-recall.md,
;; contract R1 and R5): automatic recall and the model's `memory_search'
;; look only in the conversations of the Workspace the asking conversation
;; is bound to; a temporary conversation looks only in itself.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-recall-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-recall/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(defvar superchat-display-style)
(defvar superchat-memory-auto-recall-cross-session)

(defvar wrecall--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wrecall--finder (dir)
  (cl-loop for root in wrecall--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wrecall--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-recall-" t))))

(defmacro wrecall--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wrecall--roots nil)
          ,@(mapcar (lambda (v) `(,v (wrecall--temp-dir))) vars)
          (project-find-functions (list #'wrecall--finder))
          (superchat-display-style 'editor))
     (setq wrecall--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wrecall--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wrecall--all (n)
  "Every message text of the Nth provider request, joined."
  (mapconcat #'hm--text (hm--wire n) "\n"))

(defun wrecall--search (view query)
  "What the model's `memory_search' returns for QUERY, asked from VIEW."
  (with-current-buffer view (superchat-tool-memory-search query)))

(defconst wrecall--fact "the pineapple protocol is RECALL-FACT-P"
  "What is said in the first conversation of workspace P.")

(defconst wrecall--question "what was the pineapple protocol again"
  "A later question that shares words with `wrecall--fact'.")

;;;; Which conversations a search may look in ----------------------------------

(ert-deftest workspace-recall/the-scope-is-the-conversations-of-the-workspace ()
  (wrecall--with '((:text "ok") (:text "ok") (:text "ok")) (p q)
    (let* ((srcp (wrecall--file p))
           (vp1 (with-current-buffer srcp (superchat)))
           (vp2 (with-current-buffer srcp (superchat-new-session)))
           (vq (with-current-buffer (wrecall--file q) (superchat)))
           (vt (smoke--open-chat)))
      (hm--turn vp1 "one")
      (hm--turn vp2 "two")
      (hm--turn vq "three")
      (should (equal (sort (list (hm--session vp1) (hm--session vp2)) #'string<)
                     (sort (copy-sequence
                            (superchat-view-recall-session-ids (hm--session vp1)))
                           #'string<)))
      (should (equal (list (hm--session vq))
                     (superchat-view-recall-session-ids (hm--session vq))))
      ;; A conversation bound to no Workspace looks only in itself.
      (should (equal (list (hm--session vt))
                     (superchat-view-recall-session-ids (hm--session vt))))
      (should (equal '("no-such-session")
                     (superchat-view-recall-session-ids "no-such-session")))
      (should-not (superchat-view-recall-session-ids nil))
      (should-not (superchat-view-recall-session-ids "")))))

;;;; Automatic recall ------------------------------------------------------------

(ert-deftest workspace-recall/another-conversation-of-the-workspace-is-recalled ()
  (wrecall--with '((:text "ok") (:text "ok")) (p)
    (let* ((srcp (wrecall--file p))
           (vp1 (with-current-buffer srcp (superchat))))
      (hm--turn vp1 wrecall--fact)
      (let ((vp2 (with-current-buffer srcp (superchat-new-session))))
        (should-not (equal (hm--session vp1) (hm--session vp2)))
        (hm--turn vp2 wrecall--question)
        (should (string-match-p "RECALL-FACT-P" (wrecall--all 2)))))))

(ert-deftest workspace-recall/another-workspace-is-never-recalled ()
  (wrecall--with '((:text "ok") (:text "ok")) (p q)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat)))
          (vq (with-current-buffer (wrecall--file q) (superchat))))
      (hm--turn vp wrecall--fact)
      (with-current-buffer vp
        (superchat--cmd-remember nil "pineapple protocol RECALL-ANCHOR-P" nil nil nil))
      (hm--turn vq wrecall--question)
      (should-not (string-match-p "RECALL-FACT-P" (wrecall--all 2)))
      (should-not (string-match-p "RECALL-ANCHOR-P" (wrecall--all 2))))))

(ert-deftest workspace-recall/a-temporary-conversation-recalls-only-itself ()
  (wrecall--with '((:text "ok") (:text "ok")) (p)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat))))
      (hm--turn vp wrecall--fact)
      (with-current-buffer vp
        (superchat--cmd-remember nil "pineapple protocol RECALL-ANCHOR-P" nil nil nil))
      (hm--turn (smoke--open-chat) wrecall--question)
      (should-not (string-match-p "RECALL-FACT-P" (wrecall--all 2)))
      (should-not (string-match-p "RECALL-ANCHOR-P" (wrecall--all 2))))))

(ert-deftest workspace-recall/a-workspace-does-not-recall-a-temporary-conversation ()
  (wrecall--with '((:text "ok") (:text "ok")) (p)
    (hm--turn (smoke--open-chat) wrecall--fact)
    (hm--turn (with-current-buffer (wrecall--file p) (superchat)) wrecall--question)
    (should-not (string-match-p "RECALL-FACT-P" (wrecall--all 2)))))

(ert-deftest workspace-recall/the-option-still-narrows-inside-the-workspace ()
  "`none' keeps to the asking conversation; `remembered' adds only what
`/remember' recorded in the Workspace's other conversations."
  (wrecall--with '((:text "ok") (:text "ok") (:text "ok")) (p)
    (let* ((srcp (wrecall--file p))
           (vp1 (with-current-buffer srcp (superchat))))
      (hm--turn vp1 wrecall--fact)
      (with-current-buffer vp1
        (superchat--cmd-remember nil "pineapple protocol RECALL-ANCHOR-P" nil nil nil))
      (let ((vp2 (with-current-buffer srcp (superchat-new-session))))
        (let ((superchat-memory-auto-recall-cross-session 'none))
          (hm--turn vp2 wrecall--question))
        (should-not (string-match-p "RECALL-FACT-P" (wrecall--all 2)))
        (should-not (string-match-p "RECALL-ANCHOR-P" (wrecall--all 2)))
        (let ((vp3 (with-current-buffer srcp (superchat-new-session))))
          (let ((superchat-memory-auto-recall-cross-session 'remembered))
            (hm--turn vp3 wrecall--question))
          (should-not (string-match-p "RECALL-FACT-P" (wrecall--all 3)))
          (should (string-match-p "RECALL-ANCHOR-P" (wrecall--all 3))))))))

(ert-deftest workspace-recall/the-default-is-everything-in-the-workspace ()
  (should (eq 'all (default-value 'superchat-memory-auto-recall-cross-session))))

;;;; The model's own search ------------------------------------------------------

(ert-deftest workspace-recall/memory-search-stays-inside-the-workspace ()
  (wrecall--with '((:text "ok")) (p q)
    (let* ((srcp (wrecall--file p))
           (vp1 (with-current-buffer srcp (superchat))))
      (hm--turn vp1 wrecall--fact)
      (let ((vp2 (with-current-buffer srcp (superchat-new-session)))
            (vq (with-current-buffer (wrecall--file q) (superchat)))
            (vt (smoke--open-chat)))
        (should (string-match-p "RECALL-FACT-P" (wrecall--search vp2 "pineapple")))
        (should-not (string-match-p "RECALL-FACT-P" (wrecall--search vq "pineapple")))
        (should-not (string-match-p "RECALL-FACT-P" (wrecall--search vt "pineapple")))))))

(provide 'workspace-recall-regression)
;;; workspace-recall-regression.el ends here
