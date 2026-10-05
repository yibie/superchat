;;; workspace-scope-regression.el --- the model's history tools stay in scope -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-05-sql-leaves-the-ordinary-tool-surface.md:
;; nothing Superchat ships offers the model the `sql' tool, which reads every
;; conversation's rows; and `memory_search', `tool_history', `file_history'
;; and `recent_errors' look where the Run that called them belongs, never in
;; every conversation because no conversation could be named.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-scope-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-scope/")'

;;; Code:

(load (expand-file-name "workspace-recall-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-subagent)
(defvar superchat--tool-execution-context)

(defconst wscope--sql
  "SELECT session_id, content FROM tape WHERE content LIKE '%RECALL-FACT-P%'")

(defun wscope--names (tools)
  (mapcar #'llm-tool-name tools))

(defmacro wscope--as (session &rest body)
  "Run BODY as a tool call of a Run in SESSION, from a buffer of no conversation."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((superchat--tool-execution-context (list :session-id ,session)))
       ,@body)))

;;;; Who is offered `sql' ---------------------------------------------------------

(ert-deftest workspace-scope/sql-is-not-on-the-default-surface ()
  "Guard."
  (should-not (member "sql" superchat-llm-default-tool-names)))

(ert-deftest workspace-scope/no-built-in-preset-offers-sql ()
  (dolist (name '("code" "research" "cowork" "plan"))
    (let ((preset (superchat--builtin-capability-preset name)))
      (should preset)
      (should-not (member "sql" (superchat-preset-tools preset)))))
  (dolist (entry superchat--subagent-builtin-presets)
    (should-not (member "sql" (superchat-preset-tools (funcall (cdr entry)))))))

(ert-deftest workspace-scope/every-built-in-tool-does-not-include-sql ()
  "`all' offers what Superchat ships for ordinary use; `sql' is not that."
  (hm--fixture '((:text "ok"))
    (let ((superchat-llm-tool-names 'all)
          (superchat-llm-tools-list nil))
      (let ((names (wscope--names (superchat-get-llm-tools))))
        (should (member "read_buffer" names))
        (should-not (member "sql" names))))))

(ert-deftest workspace-scope/sql-named-by-the-user-is-still-there ()
  "Guard: the user's own list, or an explicit tool list, can name it."
  (hm--fixture '((:text "ok"))
    (should (equal '("sql") (wscope--names (superchat-get-llm-tools '("sql")))))
    (let ((superchat-llm-tool-names '("read_buffer" "sql"))
          (superchat-llm-tools-list nil))
      (should (member "sql" (wscope--names (superchat-get-llm-tools)))))))

(ert-deftest workspace-scope/sql-says-what-it-reaches ()
  (hm--fixture '((:text "ok"))
    (should (string-match-p
             "every conversation"
             (llm-tool-description (car (superchat-get-llm-tools '("sql"))))))))

;;;; Where a search looks ---------------------------------------------------------

(ert-deftest workspace-scope/a-search-follows-the-run-not-the-current-buffer ()
  "The Run's conversation decides, whatever buffer is current when the tool
runs: the user may be looking at another workspace's conversation by then."
  (wrecall--with '((:text "ok")) (p q)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat)))
          (vq (with-current-buffer (wrecall--file q) (superchat))))
      (hm--turn vp wrecall--fact)
      ;; A Run of Q, while P's conversation is the current buffer.
      (with-current-buffer vp
        (let ((superchat--tool-execution-context (list :session-id (hm--session vq))))
          (should-not (string-match-p "RECALL-FACT-P"
                                      (superchat-tool-memory-search "pineapple")))))
      ;; A Run of P, while Q's conversation is the current buffer.
      (with-current-buffer vq
        (let ((superchat--tool-execution-context (list :session-id (hm--session vp))))
          (should (string-match-p "RECALL-FACT-P"
                                  (superchat-tool-memory-search "pineapple"))))))))

(ert-deftest workspace-scope/with-no-conversation-nothing-is-searched ()
  "No conversation to name is not permission to look in all of them."
  (wrecall--with '((:text "ok")) (p)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat))))
      (hm--turn vp wrecall--fact)
      (with-temp-buffer
        (should-not (bound-and-true-p superchat--session-id))
        (dolist (result (list (superchat-tool-memory-search "pineapple")
                              (superchat-tool-tool-history "read_buffer")
                              (superchat-tool-file-history "pineapple")
                              (superchat-tool-recent-errors)))
          (should (string-prefix-p "Error: " result))
          (should-not (string-match-p "RECALL-FACT-P" result))
          (should-not (string-match-p (regexp-quote (hm--session vp)) result)))))))

(ert-deftest workspace-scope/tool-history-is-the-conversations-own ()
  (wrecall--with (list (hm--read-round "hm-target") '(:text "ok")) (p q)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat)))
          (vq (with-current-buffer (wrecall--file q) (superchat))))
      (hm--turn vp "read it")
      ;; Guard: the conversation that made the call finds it.
      (should (string-match-p (regexp-quote (hm--session vp))
                              (wscope--as (hm--session vp)
                                (superchat-tool-tool-history "read_buffer"))))
      (should (string-match-p (regexp-quote (hm--session vp))
                              (wscope--as (hm--session vp)
                                (superchat-tool-file-history "hm-target"))))
      ;; Another workspace's conversation does not, whatever buffer is current.
      (with-current-buffer vp
        (let ((superchat--tool-execution-context (list :session-id (hm--session vq))))
          (dolist (result (list (superchat-tool-tool-history "read_buffer")
                                (superchat-tool-file-history "hm-target")
                                (superchat-tool-recent-errors)))
            (should-not (string-match-p (regexp-quote (hm--session vp)) result))))))))

;;;; What the other searches still do ---------------------------------------------

(ert-deftest workspace-scope/memory-search-and-recall-keep-their-scopes ()
  "Guard: the model's search stays in the workspace; the user's `/recall'
still searches every conversation by default."
  (wrecall--with '((:text "ok")) (p q)
    (let ((vp (with-current-buffer (wrecall--file p) (superchat)))
          (vq (with-current-buffer (wrecall--file q) (superchat))))
      (hm--turn vp wrecall--fact)
      (should-not (string-match-p "RECALL-FACT-P" (wrecall--search vq "pineapple")))
      (should (string-match-p "RECALL-FACT-P" (wrecall--search vp "pineapple")))
      (should (eq 'all (default-value 'superchat-memory-recall-scope)))
      (should (cl-some (lambda (row) (string-match-p "RECALL-FACT-P" (format "%S" row)))
                       (superchat-view-search "pineapple" nil 20))))))

(provide 'workspace-scope-regression)
;;; workspace-scope-regression.el ends here
