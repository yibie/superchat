;;; self-correction-regression.el --- the same mistake twice changes what the model is told -*- lexical-binding: t; -*-

;;; Commentary:
;; docs/decisions/accepted/2026-11-10-the-same-mistake-twice-changes-what-the-model-is-told.md
;;
;; Until this decision the only count of failed tool calls was the count in a
;; row, which a success set back to zero, and after every failure the model
;; was told the same sentence ("Correct the arguments of the failed call and
;; retry it.").  On 2026-11-09 the same kinds of mistake came back between
;; successful calls, the brake never stopped anything, and the model was
;; told nothing new.  Now:
;;
;; - Runtime counts the failures of an execution by kind (the `:error-type'
;;   of `tool.failed', with the name for a function or variable that does
;;   not exist and the start of the message for a plain `error'); a success
;;   does not reset it.
;; - From the second failure of a kind, the next request carries a note for
;;   that kind, written by Superchat, and the feedback after the tool results
;;   no longer says "Correct the arguments ... retry it".  The note goes the
;;   way the finish check's note goes (a steer with an origin), so it is on
;;   the Tape and in later turns' history.
;; - A call stopped at the time limit is not told to correct its arguments,
;;   the first time already.
;; - For a function or variable that does not exist, the error lists the
;;   defined names close to it, the first time already.
;; - An execution that ended failed with failed tool calls -- the brake's
;;   among them -- records its failures by kind on `run.failed' and leaves a
;;   system note on the Tape, which the next turn's request carries as a
;;   `[System note]', not as the assistant's words.
;;
;; The model is the scripted one of scripts/smoke.el; whether a real model
;; does better with these words is not tested here.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/self-correction-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^self-correction/")'

;;; Code:

(load (expand-file-name "eval-failure-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-tool-eval-time-limit)

(defconst sc--generic "Correct the arguments of the failed call and retry it")

(defun sc--eval (expression)
  "A model step whose one call evaluates EXPRESSION."
  (list :tool-uses (list (list "eval_elisp" (cons 'expression expression)))))

(defconst sc--ok (sc--eval "(+ 1 2)"))
(defconst sc--wrong-type (sc--eval "(car 1)"))

(defun sc--unclosed ()
  "The 2026-11-09 shape: one long line of Lisp, one parenthesis never closed."
  (sc--eval (concat "(progn (setq sc--sink (list "
                    (mapconcat (lambda (n) (format "(cons %d \"item-%d\")" n n))
                               (number-sequence 1 80) " ")
                    "))")))

(defun sc--prompts ()
  "The prompt text of every request the scripted model received, in order."
  (mapcar (lambda (request) (plist-get request :prompt-text))
          (reverse smoke--requests)))

(defun sc--count (needle text)
  (let ((start 0) (n 0))
    (while (string-match (regexp-quote needle) text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun sc--session (view)
  (buffer-local-value 'superchat--session-id view))

(defun sc--events (view type)
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events (sc--session view))))

(defun sc--system-rows (view topic)
  "The system material rows of VIEW's Session with TOPIC."
  (cl-remove-if-not (lambda (row) (and (equal "system" (nth 2 row))
                                       (equal topic (nth 1 row))))
                    (superchat-view-entries (sc--session view))))

(defmacro sc--run (script &rest body)
  "Send one message with SCRIPT as the model; BODY sees VIEW and RUN."
  (declare (indent 1) (debug t))
  `(ef--fixture ,script
     (let* ((view (smoke--open-chat))
            (run (ef--run view "go")))
       ,@body)))

;;;; 1. Counted by kind, not reset by a success -----------------------------

(ert-deftest self-correction/kinds-are-counted-and-a-success-does-not-reset-them ()
  "F..F.......F: the count in a row never reaches 2, the count by kind
reaches 3."
  (sc--run (append (list sc--wrong-type sc--ok sc--ok sc--wrong-type)
                   (make-list 7 sc--ok)
                   (list sc--wrong-type '(:text "SC-DONE")))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    (should (equal '(("wrong-type-argument" . 3))
                   (plist-get (superchat-runtime--entry run) :tool-failure-kinds)))
    ;; The count in a row is only the last call's.
    (should (= 1 (plist-get (superchat-runtime--entry run) :tool-failure-streak)))))

(ert-deftest self-correction/the-kind-names ()
  (should (equal "wrong-type-argument"
                 (superchat-tool-failure-kind "wrong-type-argument" "Error: evaluating expression: Wrong type argument: listp, 1")))
  (should (equal "void-function sc-no-such-fn"
                 (superchat-tool-failure-kind
                  "void-function"
                  "Error: evaluating expression: Symbol’s function definition is void: sc-no-such-fn\nDefined function names ...")))
  (should (equal "void-variable sc-no-such-var"
                 (superchat-tool-failure-kind
                  "void-variable" "Symbol's value as variable is void: sc-no-such-var")))
  (should (equal "error Expected exactly one expression"
                 (superchat-tool-failure-kind
                  "error" "Error: evaluating expression: Expected exactly one expression; wrap multiple forms in progn.")))
  (should (equal "unreadable-expression"
                 (superchat-tool-failure-kind "unreadable-expression" "Error: the expression does not read as Lisp")))
  (should (equal "tool-failed" (superchat-tool-failure-kind nil "x"))))

;;;; 2. The second time, the model is told something for that kind ----------

(ert-deftest self-correction/the-second-failure-of-a-kind-is-told-for-that-kind ()
  (sc--run (append (list sc--wrong-type sc--ok sc--ok sc--wrong-type)
                   (make-list 7 sc--ok)
                   (list sc--wrong-type '(:text "SC-DONE")))
    (let ((prompts (sc--prompts)))
      ;; Request 2 follows the first failure: the sentence as before, no note.
      (should (string-match-p sc--generic (nth 1 prompts)))
      (should-not (string-match-p "Superchat: the same kind of failure" (nth 1 prompts)))
      ;; Request 5 follows the second: the note, and not the old sentence.
      (let ((fifth (nth 4 prompts)))
        (should (string-match-p (regexp-quote "Superchat: the same kind of failure (wrong-type-argument) has now happened 2 times in this execution.") fifth))
        (should (string-match-p (regexp-quote "(type-of VALUE)") fifth))
        (should (string-match-p (regexp-quote "Follow the Superchat note in this request") fifth))
        (should (= 1 (sc--count sc--generic fifth))))
      ;; Request 13 follows the third: "3 times".
      (should (string-match-p "has now happened 3 times in this execution"
                              (nth 12 prompts))))))

(ert-deftest self-correction/an-unreadable-expression-the-second-time ()
  "The 2026-11-09 shape: the same unclosed one-line expression sent again.
The second time the model is pointed at another way, not given the same
words alone."
  (sc--run (list (sc--unclosed) (sc--unclosed) sc--ok '(:text "SC-DONE"))
    (let* ((prompts (sc--prompts))
           (third (nth 2 prompts)))
      (should (string-match-p "1 parenthesis is never closed" (nth 1 prompts)))
      (should-not (string-match-p "did not read as Lisp, so none of them ran" (nth 1 prompts)))
      (should (string-match-p (regexp-quote "Superchat: 2 expressions in this execution did not read as Lisp, so none of them ran.") third))
      (should (string-match-p "split the work into several short eval_elisp calls" third))
      (should (string-match-p "edit_buffer" third))
      (should (string-match-p "line 1 tells little when the whole expression is on one line" third))
      ;; The old sentence once, after the first failure; not after the second.
      (should (= 1 (sc--count sc--generic third)))
      (should (string-match-p "do not send the failed call again with small edits" third)))))

(ert-deftest self-correction/two-different-missing-names-are-two-kinds ()
  "Guard: the kind of a name that does not exist carries the name."
  (sc--run (list (sc--eval "(sc-no-such-alpha-fn)") sc--ok (sc--eval "(sc-no-such-beta-fn)")
                 '(:text "SC-DONE"))
    (should-not (cl-some (lambda (text) (string-match-p "is not a defined function, and this execution" text))
                         (sc--prompts)))))

(ert-deftest self-correction/the-same-missing-name-twice ()
  (sc--run (list (sc--eval "(sc-no-such-alpha-fn)") sc--ok (sc--eval "(sc-no-such-alpha-fn 1)")
                 '(:text "SC-DONE"))
    (let ((fourth (nth 3 (sc--prompts))))
      (should (string-match-p (regexp-quote "Superchat: `sc-no-such-alpha-fn' is not a defined function, and this execution has now called it 2 times.  Do not call it again.") fourth))
      (should (string-match-p (regexp-quote "(fboundp 'sc-no-such-alpha-fn)") fourth)))))

;;;; 3. A name that does not exist comes with the close ones ----------------

(defun sc-zebra-open-chat () "A function for the lookup." nil)
(defun sc-zebra-chat-history () "A function for the lookup." nil)
(defun sc-zebra-unrelated-thing () "Not close: one word shared besides the prefix." nil)
(defvar sc-zebra-chat-limit 3 "A variable for the lookup.")

(ert-deftest self-correction/a-missing-function-lists-close-names-the-first-time ()
  (sc--run (list (sc--eval "(sc-zebra-chat-open)") '(:text "SC-DONE"))
    (let ((text (plist-get (plist-get (car (ef--outcomes view "eval_elisp")) :payload)
                           :result-text)))
      (should (string-match-p "\\`Error: evaluating expression: Symbol.s function definition is void: sc-zebra-chat-open" text))
      (should (string-match-p (regexp-quote "Defined function names close to `sc-zebra-chat-open' (looked up by Superchat): `sc-zebra-open-chat', `sc-zebra-chat-history'.") text))
      (should-not (string-match-p "sc-zebra-unrelated-thing" text))
      ;; The model reads it in the next request.
      (should (string-match-p (regexp-quote "`sc-zebra-open-chat'") (nth 1 (sc--prompts)))))))

(ert-deftest self-correction/a-missing-variable-lists-close-names ()
  (sc--run (list (sc--eval "(+ sc-zebra-chat-limits 1)") '(:text "SC-DONE"))
    (let ((text (plist-get (plist-get (car (ef--outcomes view "eval_elisp")) :payload)
                           :result-text)))
      (should (string-match-p (regexp-quote "Defined variable names close to `sc-zebra-chat-limits' (looked up by Superchat): `sc-zebra-chat-limit'.") text))
      (should (string-match-p (regexp-quote "(boundp 'NAME)") text)))))

(ert-deftest self-correction/no-close-name-is-said ()
  (sc--run (list (sc--eval "(sc-qqqq-nothing-like-this-anywhere)") '(:text "SC-DONE"))
    (should (string-match-p "No defined function name is close to `sc-qqqq-nothing-like-this-anywhere' (looked up by Superchat)"
                            (nth 1 (sc--prompts))))))

;; Real-model runs of 2026-11-10: a variable called as a function
;; (`buffer-read-only', a package's buffer-local variable) was given close
;; function names that had nothing to do with it.

(defun sc--result-text (view)
  (plist-get (plist-get (car (ef--outcomes view "eval_elisp")) :payload) :result-text))

(ert-deftest self-correction/a-variable-called-as-a-function-is-said-so ()
  (sc--run (list (sc--eval "(sc-zebra-chat-limit)") '(:text "SC-DONE"))
    (let ((text (sc--result-text view)))
      (should (string-match-p (regexp-quote "`sc-zebra-chat-limit' is a variable, not a function (looked up by Superchat): use its value -- write sc-zebra-chat-limit without parentheses around it as the head of a form -- or (symbol-value 'sc-zebra-chat-limit), or (buffer-local-value 'sc-zebra-chat-limit BUFFER) for its value in another buffer.") text))
      (should-not (string-match-p "close to" text))
      (should-not (string-match-p "sc-zebra-chat-history" text))
      (should (string-match-p (regexp-quote "is a variable, not a function") (nth 1 (sc--prompts))))))
  ;; The built-in case of the runs.
  (sc--run (list (sc--eval "(buffer-read-only)") '(:text "SC-DONE"))
    (let ((text (sc--result-text view)))
      (should (string-match-p (regexp-quote "`buffer-read-only' is a variable, not a function") text))
      (should-not (string-match-p "Buffer-menu" text)))))

(ert-deftest self-correction/a-function-used-as-a-variable-is-said-so ()
  (sc--run (list (sc--eval "(+ sc-zebra-open-chat 1)") '(:text "SC-DONE"))
    (let ((text (sc--result-text view)))
      (should (string-match-p (regexp-quote "`sc-zebra-open-chat' is a function, not a variable (looked up by Superchat): call it as (sc-zebra-open-chat ...), or refer to it as #'sc-zebra-open-chat.") text))
      (should-not (string-match-p "close to" text)))))

(ert-deftest self-correction/the-second-time-a-variable-is-called-as-a-function ()
  (sc--run (list (sc--eval "(sc-zebra-chat-limit)") sc--ok (sc--eval "(sc-zebra-chat-limit)")
                 '(:text "SC-DONE"))
    (let ((fourth (nth 3 (sc--prompts))))
      (should (string-match-p (regexp-quote "Superchat: `sc-zebra-chat-limit' is a variable, not a function, and this execution has now called it as a function 2 times.  It is a variable: use its value -- write sc-zebra-chat-limit without parentheses around it as the head of a form -- or (symbol-value 'sc-zebra-chat-limit), or (buffer-local-value 'sc-zebra-chat-limit BUFFER) for its value in another buffer.") fourth))
      (should-not (string-match-p "is not a defined function" fourth)))))

(ert-deftest self-correction/the-second-time-a-function-is-used-as-a-variable ()
  (sc--run (list (sc--eval "(+ sc-zebra-open-chat 1)") sc--ok (sc--eval "(list sc-zebra-open-chat)")
                 '(:text "SC-DONE"))
    (let ((fourth (nth 3 (sc--prompts))))
      (should (string-match-p (regexp-quote "Superchat: `sc-zebra-open-chat' is a function, not a variable, and this execution has now used it as a variable 2 times.  It is a function: call it as (sc-zebra-open-chat ...), or refer to it as #'sc-zebra-open-chat.") fourth))
      (should-not (string-match-p "is not a defined variable" fourth)))))

(ert-deftest self-correction/the-lookup-is-bounded ()
  (let ((superchat-tools-close-names-seconds 0))
    ;; With no time at all the lookup stops early and still answers.
    (should (listp (superchat-tools--close-names 'sc-zebra-chat-open 'void-function))))
  (let ((names (superchat-tools--close-names 'org-foo-bar-baz-quux 'void-function)))
    (should (<= (length names) superchat-tools-close-names-max))))

;;;; 4. A time limit is not told to correct its arguments -------------------

(ert-deftest self-correction/a-time-limit-is-not-told-to-retry ()
  (let ((superchat-tool-eval-time-limit 1))
    (sc--run (list (sc--eval "(while t)") '(:text "SC-DONE"))
      (let ((second (nth 1 (sc--prompts))))
        (should (string-match-p "stopped after 1 seconds, the time limit" second))
        (should (string-match-p "must not be run again as it was" second))
        (should-not (string-match-p sc--generic second))))))

;;;; 5. After the brake, the next turn is told why, by Superchat -------------

(ert-deftest self-correction/a-braked-execution-leaves-a-note-for-the-next-turn ()
  (ef--fixture (list (sc--unclosed) (sc--unclosed) (sc--unclosed) '(:text "SC-NEXT"))
    (let* ((view (smoke--open-chat))
           (run (ef--run view "go")))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      ;; `run.failed' says which kinds, how often.
      (let ((failed (car (sc--events view "run.failed"))))
        (should (equal '((:kind "unreadable-expression" :count 3))
                       (plist-get (plist-get failed :payload) :tool-failures))))
      ;; A system row, after the Turn's own rows.
      (let ((rows (sc--system-rows view "tool-failures")))
        (should (= 1 (length rows)))
        (should (string-match-p (regexp-quote "Written by Superchat, not by the assistant: the previous execution was stopped by Superchat (Stopped after 3 failed calls in a row).  Its failed tool calls, by kind: unreadable-expression, 3 times.") (nth 3 (car rows))))
        (should (> (nth 0 (car rows))
                   (plist-get (car (last (sc--events view "turn.completed"))) :storage-id))))
      ;; The next turn's request carries it as a system note, before the new
      ;; message, and not inside the assistant's words.
      (ef--run view "and now?")
      (let ((next (car (last (sc--prompts)))))
        (should (string-match-p (regexp-quote "[System note] Written by Superchat, not by the assistant: the previous execution was stopped by Superchat") next))
        (should (string-match-p "split the work into several short eval_elisp calls" next))
        (should (< (string-match (regexp-quote "[System note] Written by Superchat") next)
                   (string-match "and now\\?" next)))))))

(ert-deftest self-correction/a-completed-execution-leaves-no-note ()
  "Guard: failures the execution got past are not summarized."
  (sc--run (list sc--wrong-type sc--wrong-type sc--ok '(:text "SC-DONE"))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    (should-not (sc--system-rows view "tool-failures"))))

(ert-deftest self-correction/the-brakes-own-failure-gets-no-note-it-could-not-read ()
  "Guard: the third failure in a row stops the execution, so no note is
queued for a request that will not be sent."
  (sc--run (list sc--wrong-type sc--wrong-type sc--wrong-type '(:text "SC-NEVER"))
    (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
    (should-not (sc--events view "run.steer-undelivered"))
    ;; The second failure's note was delivered.
    (should (= 1 (length (cl-remove-if-not
                          (lambda (e) (equal "self-correction"
                                             (format "%s" (plist-get (plist-get e :payload) :origin))))
                          (sc--events view "run.steered")))))))

;;;; 6. The notes are on the Tape ---------------------------------------------

(ert-deftest self-correction/the-note-is-on-the-tape-and-in-the-next-turn ()
  (ef--fixture (list sc--wrong-type sc--ok sc--wrong-type '(:text "SC-DONE") '(:text "SC-LATER"))
    (let* ((view (smoke--open-chat)))
      (ef--run view "go")
      (let ((steered (cl-remove-if-not
                      (lambda (e) (equal "self-correction"
                                         (format "%s" (plist-get (plist-get e :payload) :origin))))
                      (sc--events view "run.steered"))))
        (should (= 1 (length steered)))
        (should (string-match-p "has now happened 2 times"
                                (plist-get (plist-get (car steered) :payload) :instruction))))
      ;; Admitted as material of the next Step, with its origin.
      (should (cl-some (lambda (e)
                         (let ((payload (plist-get e :payload)))
                           (and (equal "self-correction"
                                       (format "%s" (plist-get (plist-get payload :source) :origin)))
                                (string-match-p "has now happened 2 times" (plist-get payload :text)))))
                       (sc--events view "attachment.added")))
      ;; A later turn's history carries it.
      (ef--run view "later")
      (should (string-match-p "Superchat: the same kind of failure (wrong-type-argument) has now happened 2 times"
                              (car (last (sc--prompts))))))))

(provide 'self-correction-regression)

;;; self-correction-regression.el ends here
