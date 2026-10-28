;;; eval-code-mode-regression.el --- eval_elisp calls tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Decision of 2026-10-26, eval_elisp as code mode
;; (docs/decisions/accepted/2026-10-26-eval-elisp-calls-tools.md): an
;; `eval_elisp' expression calls other tools with (superchat-tool "NAME"
;; :ARG VALUE ...).  Each call goes through the one tool pipeline, numbered
;; under the expression's call; only the expression's value reaches the
;; model; a call the level would ask about is refused at once, because no
;; question can be answered while the expression runs.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/eval-code-mode-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^eval-code-mode/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-changes)
(require 'superchat-script)
(defvar superchat-display-style)

(defmacro ecm--with (mode &rest body)
  "Run BODY in the history fixture under the sandbox MODE."
  (declare (indent 1) (debug t))
  `(let ((superchat-display-style 'editor)
         (superchat-sandbox-default-mode ,mode)
         (superchat-changes-review-after-run nil))
     (hm--fixture nil ,@body)))

(defmacro ecm--with-buffer (name text &rest body)
  "Run BODY with buffer NAME holding TEXT; kill it afterwards."
  (declare (indent 2))
  `(let ((buffer (get-buffer-create ,name)))
     (with-current-buffer buffer (erase-buffer) (insert ,text))
     (unwind-protect (progn ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(defun ecm--round (expression)
  "A model round that runs EXPRESSION with `eval_elisp'."
  `(:tool-uses (("eval_elisp" (expression . ,expression)))))

(defun ecm--result (n)
  "The text of the tool result the model received in the Nth request."
  (let ((tool-message (cl-find-if (lambda (m) (equal "tool" (alist-get 'role m)))
                                  (reverse (hm--wire n)))))
    (should tool-message)
    (string-remove-prefix "Result of tool call is " (hm--text tool-message))))

(defun ecm--tool-events (session)
  "SESSION's tool facts as (TYPE TOOL-NAME EXECUTION-ID)."
  (delq nil
        (mapcar (lambda (event)
                  (when (member (plist-get event :type)
                                '("tool.requested" "tool.completed" "tool.failed"))
                    (let ((payload (plist-get event :payload)))
                      (list (plist-get event :type)
                            (plist-get payload :tool-name)
                            (plist-get payload :execution-id)))))
                (superchat-tape-replay-events session))))

(ert-deftest eval-code-mode/reading-and-editing-take-one-round-trip ()
  (ecm--with-buffer "ecm-notes" "alpha beta\n"
    (ecm--with 'danger-full-access
      (setq smoke--script
            (list (ecm--round
                   "(list (superchat-tool \"read_buffer\" :buffer \"ecm-notes\")
                          (superchat-tool \"edit_buffer\" :buffer_name \"ecm-notes\"
                                          :old_string \"alpha\" :new_string \"gamma\"))")
                  '(:text "done")))
      (let* ((view (smoke--open-chat))
             (run (hm--turn view "change it"))
             (session (hm--session view)))
        ;; Two requests: the expression, then the answer.
        (should (= 2 (length hm--wires)))
        (should (equal "gamma beta\n" (with-current-buffer "ecm-notes" (buffer-string))))
        (let ((result (ecm--result 2)))
          (should (string-prefix-p "Result: (" result))
          (should (string-match-p "alpha beta" result))
          ;; edit_buffer's own report, layer line and all, reaches the model.
          (should (string-match-p "Layer: in memory only" result)))
        ;; Each nested call is a tool fact of its own, numbered under eval_elisp.
        (let* ((events (ecm--tool-events session))
               (eval-id (nth 2 (cl-find "eval_elisp" events :key #'cadr :test #'equal)))
               (nested (cl-remove-if-not
                        (lambda (e) (member (cadr e) '("read_buffer" "edit_buffer")))
                        events)))
          (should (equal '(("tool.requested" "read_buffer") ("tool.completed" "read_buffer")
                           ("tool.requested" "edit_buffer") ("tool.completed" "edit_buffer"))
                         (mapcar (lambda (e) (list (car e) (cadr e))) nested)))
          (should (stringp eval-id))
          (should (equal (list (concat eval-id "/1") (concat eval-id "/2"))
                         (delete-dups (mapcar #'caddr nested)))))
        ;; The edit is on the record of unsaved changes.
        (should (member "ecm-notes"
                        (mapcar (lambda (row) (plist-get row :name))
                                (superchat-changes-candidates session run))))))))

(ert-deftest eval-code-mode/only-the-value-reaches-the-model ()
  (ecm--with-buffer "ecm-big" (concat "SECRET-MIDDLE " (make-string 3000 ?z) "\nLAST-LINE\n")
    (ecm--with 'danger-full-access
      (setq smoke--script
            (list (ecm--round
                   "(if (string-match-p \"LAST-LINE\" (superchat-tool \"read_buffer\" :buffer \"ecm-big\"))
                        \"FOUND\" \"MISSING\")")
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "look")
      (let ((all (mapconcat #'hm--text (hm--wire 2) "\n")))
        (should (string-match-p "FOUND" all))
        (should-not (string-match-p "SECRET-MIDDLE" all))))))

(ert-deftest eval-code-mode/a-nested-failure-is-text-and-the-call-succeeds ()
  (ecm--with 'danger-full-access
    (setq smoke--script
          (list (ecm--round
                 "(superchat-tool \"read_buffer\" :buffer \"ecm-no-such-buffer\")")
                '(:text "done")))
    (let ((view (smoke--open-chat)))
      (hm--turn view "go")
      (let ((events (ecm--tool-events (hm--session view))))
        (should (cl-find-if (lambda (e) (and (equal "tool.failed" (car e))
                                             (equal "read_buffer" (nth 1 e))
                                             (string-search "/" (nth 2 e))))
                            events))
        (should (cl-find-if (lambda (e) (and (equal "tool.completed" (car e))
                                             (equal "eval_elisp" (nth 1 e))))
                            events)))
      (should (string-match-p "ecm-no-such-buffer" (ecm--result 2))))))

(ert-deftest eval-code-mode/what-an-expression-cannot-call ()
  ;; One Run each: four failed calls in a row would end a Run.
  (pcase-dolist (`(,expression ,expected)
                 '(("(superchat-tool \"eval_elisp\" :expression \"1\")"
                    "eval_elisp cannot be called from eval_elisp")
                   ("(superchat-tool \"ask_user\" :question \"?\")"
                    "ask_user cannot be called from eval_elisp")
                   ("(superchat-tool \"read_buffer\" :buffer \"x\" :bogus 1)"
                    "read_buffer has no argument :bogus; its arguments are :buffer")
                   ("(superchat-tool \"no_such_tool\")"
                    "No tool named no_such_tool")))
    (ecm--with 'danger-full-access
      (setq smoke--script (list (ecm--round expression) '(:text "done")))
      (hm--turn (smoke--open-chat) "go")
      (should (string-match-p expected (ecm--result 2))))))

(ert-deftest eval-code-mode/a-call-that-would-ask-is-refused-without-waiting ()
  "Under `edit-buffers' the expression itself is asked about once.  A save
from inside it would be asked about too, but the question could not be
answered while the expression runs: it is refused at once."
  (ecm--with-buffer "ecm-notes" "alpha\n"
    (ecm--with 'edit-buffers
      (let* ((asked 0)
             (superchat-approval-function
              (lambda (&rest _) (setq asked (1+ asked)) t))
             (superchat-llm-tool-names
              (append superchat-llm-default-tool-names '("save_buffer")))
             (start (float-time)))
        (setq smoke--script
              (list (ecm--round
                     "(superchat-tool \"save_buffer\" :buffer_name \"ecm-notes\")")
                    '(:text "done")))
        (hm--turn (smoke--open-chat) "save it")
        (should (= 1 asked))
        (should (< (- (float-time) start) 10))
        (should (string-match-p
                 "save_buffer needs the user's approval .*cannot be asked for a call made from inside eval_elisp; make it as a tool call of its own"
                 (ecm--result 2)))))))

(ert-deftest eval-code-mode/the-model-is-told-to-act-in-one-call ()
  "Measured without it, the model almost never called a tool from an
expression.  The text goes with eval_elisp and only there."
  (ecm--with 'edit-buffers
    (setq smoke--script '((:text "ok") (:text "ok")))
    (hm--turn (smoke--open-chat) "hello")
    (should (string-match-p (regexp-quote superchat-prompt-act-once-text)
                            (hm--text (car (hm--wire 1)))))
    (let ((superchat-llm-tool-names '("read_buffer"))
          (superchat-llm-tools-list nil))
      (hm--turn (smoke--open-chat) "hello")
      (should (= 2 (length hm--wires)))
      (should-not (string-match-p (regexp-quote superchat-prompt-act-once-text)
                                  (hm--text (car (hm--wire 2))))))))

(defmacro ecm--guarded (seconds &rest body)
  "Run BODY; kill this Emacs from outside if it is still running after SECONDS."
  (declare (indent 1))
  `(let ((guard (make-process :name "ecm-guard" :noquery t :connection-type 'pipe
                              :command (list "sh" "-c" (format "sleep %d && kill -9 %d"
                                                               ,seconds (emacs-pid))))))
     (unwind-protect (progn ,@body) (delete-process guard))))

(ert-deftest eval-code-mode/a-nested-call-the-time-limit-stops-has-an-ending ()
  "The time limit leaves the expression by `throw', past the nested call's
own accounting.  The nested call must still end on the Tape."
  (ecm--guarded 40
    (ecm--with-buffer "ecm-notes" "x\n"
      (ecm--with 'danger-full-access
        (let ((superchat-tool-eval-time-limit 1)
              (original (symbol-function 'superchat-tool-read-buffer)))
          (cl-letf (((symbol-function 'superchat-tool-read-buffer)
                     (lambda (&rest args)
                       (let ((until (+ (float-time) 3)))
                         (while (< (float-time) until) (sleep-for 0.05)))
                       (apply original args))))
            (setq smoke--script
                  (list (ecm--round "(superchat-tool \"read_buffer\" :buffer \"ecm-notes\")")
                        '(:text "done")))
            (let ((view (smoke--open-chat)))
              (hm--turn view "go")
              (should (string-match-p "time limit" (ecm--result 2)))
              (let ((nested (cl-remove-if-not
                             (lambda (e) (equal "read_buffer" (cadr e)))
                             (ecm--tool-events (hm--session view)))))
                (should (equal '("tool.requested" "tool.failed")
                               (mapcar #'car nested)))))))))))

(ert-deftest eval-code-mode/outside-a-tool-call-it-says-where-it-works ()
  (let ((superchat-script--calls nil))
    (should (string-match-p
             "only inside eval_elisp or elisp_script"
             (cadr (should-error (superchat-tool "read_buffer" :buffer "x")))))))

(ert-deftest eval-code-mode/an-expression-makes-at-most-a-hundred-calls ()
  (ecm--with-buffer "ecm-notes" "x\n"
    (ecm--with 'danger-full-access
      (setq smoke--script
            (list (ecm--round
                   "(dotimes (_ 101) (superchat-tool \"read_buffer\" :buffer \"ecm-notes\"))")
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "go")
      (should (string-match-p "More than 100 tool calls in one expression"
                              (ecm--result 2))))))

;;; eval-code-mode-regression.el ends here
