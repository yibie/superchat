;;; work-surface-regression.el --- work_record goes with the work-records row -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 7b, item 1 (found in the user's own Emacs on 2026-10-12): the user's
;; `superchat-llm-tool-names' did not name `work_record', so a record started
;; with /work could not be completed by the model and its rounds went on with
;; nothing to do.
;;
;; `work_record' is the `work-records' row's tool: while the row is mounted
;; the default tool surface offers it whatever the name list says, and while
;; it is not mounted nothing offers it.  /work and the tool's `resume' start
;; nothing where the model would not be offered the tool.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/work-surface-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-surface/")'

;;; Code:

(load (expand-file-name "goal-rounds-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-llm-tool-names)
(defvar superchat-llm-tools-list)
(defvar superchat--active-preset)
(defvar superchat-llm-tools-enabled)
(defvar superchat--tool-execution-context)
(declare-function superchat-plugin-mount "superchat-plugin" (name &optional config))
(declare-function superchat-plugin-unmount "superchat-plugin" (name))
(declare-function superchat-plugin-row-active-p "superchat-plugin" (name))

(defmacro wsurf--row-off (&rest body)
  "Run BODY with the `work-records' row unmounted, mounting it again after."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (superchat-plugin-unmount 'work-records)
              (should-not (superchat-plugin-row-active-p 'work-records))
              ,@body)
     (superchat-plugin-mount 'work-records)))

(defmacro wsurf--names (names &rest body)
  "Run BODY with the user's tool name list NAMES."
  (declare (indent 1) (debug t))
  `(let ((superchat-llm-tool-names ,names)
         (superchat-llm-tools-list nil))
     (unwind-protect (progn ,@body)
       (setq superchat-llm-tools-list nil))))

(defun wsurf--sent (n)
  "The tool names of the Nth request the scripted model was sent."
  (plist-get (nth (1- n) smoke--requests) :tools))

(defun wsurf--echo (view text)
  "Send the command TEXT from VIEW; return what the echo area was told."
  (let ((said nil)
        (original (symbol-function 'message)))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (push (apply #'format-message format-string args) said))
                 (apply original format-string args))))
      (with-current-buffer view
        (let ((inhibit-read-only t)) (goto-char (point-max)) (insert text))
        (superchat-send-input)))
    (mapconcat #'identity (nreverse said) "\n")))

;;;; The name list does not take the tool away -----------------------------------

(ert-deftest work-surface/a-name-list-without-it-still-offers-it ()
  (pend--fresh
    (wrec--with '((:text "WSURF-ANSWER")) (p)
      (wsurf--names '("read_buffer")
        (let ((view (wrec--view p)))
          (hm--turn view "hello")
          (should (member "read_buffer" (wsurf--sent 1)))
          (should (member "work_record" (wsurf--sent 1)))
          ;; The list still narrows every other built-in tool.
          (should-not (member "edit_buffer" (wsurf--sent 1)))
          (should-not (member "timer_create" (wsurf--sent 1))))))))

(ert-deftest work-surface/the-default-list-offers-it-once ()
  "The guard: with the default names nothing is added or doubled."
  (pend--fresh
    (wrec--with '((:text "WSURF-ANSWER")) (p)
      (let ((view (wrec--view p)))
        (hm--turn view "hello")
        (should (= 1 (cl-count "work_record" (wsurf--sent 1) :test #'equal)))
        (should (member "edit_buffer" (wsurf--sent 1)))))))

(ert-deftest work-surface/the-model-completes-a-record-under-a-short-list ()
  "What failed in the user's Emacs: the rounds end because the tool is there."
  (pend--fresh
    (wrec--with (list '(:text "Ten of the items are ticked.")
                      (wrec--call '(action . "done") '(conclusion . "WSURF-DONE"))
                      '(:text "All ticked."))
        (p)
      (wsurf--names '("read_buffer")
        (let* ((view (wrec--view p)) (session (hm--session view)))
          (hm--turn view (concat "/work " gr--goal))
          (gr--quiet)
          (let ((id (gr--record)))
            (should id)
            (should (eq 'completed (superchat-work-status (superchat-work-get id))))
            (should (= 1 (gr--count session "work.round")))))))))

;;;; Without the row nothing offers it --------------------------------------------

(ert-deftest work-surface/without-the-row-it-is-not-offered ()
  (pend--fresh
    (wrec--with '((:text "WSURF-ANSWER")) (p)
      (let ((view (wrec--view p)))
        (wsurf--row-off
          (setq superchat-llm-tools-list nil)
          (hm--turn view "hello")
          (should (member "read_buffer" (wsurf--sent 1)))
          (should-not (member "work_record" (wsurf--sent 1))))))))

(ert-deftest work-surface/without-the-row-the-command-starts-nothing ()
  (pend--fresh
    (wrec--with '((:text "NEVER-ASKED")) (p)
      (let* ((view (wrec--view p)) (session (hm--session view)) shown)
        (wsurf--row-off
          (setq shown (wsurf--echo view (concat "/work " gr--goal)))
          (gr--quiet))
        (should (string-match-p "work_record" shown))
        (should (string-match-p "not started" shown))
        (should-not (superchat-work-records))
        (should (= 0 (gr--count session "work.round")))
        (should (= 0 (gr--count session "run.started")))
        (should-not smoke--requests)))))

(ert-deftest work-surface/without-the-row-resume-starts-nothing ()
  (pend--fresh
    (wrec--with '((:text "NEVER-ASKED")) (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session gr--goal))
             shown)
        (wsurf--row-off
          (setq shown (wsurf--echo view "/work resume"))
          (gr--quiet))
        (should (string-match-p "work_record" shown))
        (should-not (superchat-work-continuing-p id))
        (should (= 0 (gr--count session "work.round")))
        (should (= 0 (gr--queued-anywhere)))))))

;;;; A tool list that leaves it out ------------------------------------------------

(ert-deftest work-surface/a-preset-that-leaves-it-out-starts-nothing ()
  "An explicit tool list of the conversation's preset is not widened; the
command says so instead of starting rounds nobody can end."
  (pend--fresh
    (wrec--with '((:text "NEVER-ASKED")) (p)
      (let* ((view (wrec--view p)) (session (hm--session view)) shown)
        (with-current-buffer view
          (setq-local superchat--active-preset
                      (superchat-preset--create
                       :name "wsurf-narrow" :type 'agent
                       :tools '("read_buffer"))))
        (setq shown (wsurf--echo view (concat "/work " gr--goal)))
        (gr--quiet)
        (should (string-match-p "work_record" shown))
        (should-not (superchat-work-records))
        (should (= 0 (gr--count session "run.started")))))))

(ert-deftest work-surface/with-tools-turned-off-the-command-starts-nothing ()
  (pend--fresh
    (wrec--with '((:text "NEVER-ASKED")) (p)
      (let* ((view (wrec--view p)) (session (hm--session view)) shown)
        (let ((superchat-llm-tools-enabled nil))
          (setq shown (wsurf--echo view (concat "/work " gr--goal)))
          (gr--quiet))
        (should (string-match-p "not started" shown))
        (should (string-match-p "work_record" shown))
        (should-not (superchat-work-records))
        (should (= 0 (gr--count session "run.started")))))))

(ert-deftest work-surface/the-tools-own-resume-is-refused-without-the-row ()
  "The function itself, should anything still call it with the row off."
  (pend--fresh
    (wrec--with nil (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (id (superchat-work-start session gr--goal))
             answer)
        (wsurf--row-off
          (let ((superchat--tool-execution-context (list :session-id session)))
            (cl-letf (((symbol-function 'superchat-work--users-own-run-p)
                       (lambda (&rest _) t)))
              (setq answer (superchat-work-tool "resume" id)))))
        (should (string-match-p "\\`Error: Rounds were not resumed" answer))
        (should-not (superchat-work-continuing-p id))))))

(provide 'work-surface-regression)
;;; work-surface-regression.el ends here
