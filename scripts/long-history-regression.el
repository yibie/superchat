;;; long-history-regression.el --- A long conversation can still be sent to -*- lexical-binding: t; -*-

;;; Commentary:

;; The history a request carries is chosen under a character budget that
;; counts the rows' own text.  The header naming the rows kept is added
;; afterwards and is not counted, so a conversation long enough to fill the
;; budget rendered longer than the room it was given, and the assembler
;; refused the whole request: no message could be sent in that conversation
;; any more.  (Found by the review of the fixes, FR-14; reproduced at turn 59
;; of ordinary scripted turns.)
;;
;;   emacs -Q --batch -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/long-history-regression.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst lh--budget 6000
  "The context budget, in characters, these tests give the model.")

(defmacro lh--with-small-budget (&rest body)
  "Run BODY with a model whose context holds `lh--budget' characters."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'superchat--llm-context-budget-chars)
              (lambda (&rest _) lh--budget)))
     ,@body))

(defun lh--talk (view turns)
  "Have TURNS scripted turns in VIEW; each answer is 300 characters and its number."
  (dotimes (i turns)
    (setq smoke--script (list (list :text (format "%s LH-ANSWER-%03d"
                                                  (make-string 300 ?a) i))))
    (hm--turn view (format "LH-QUESTION-%03d" i))))

(defun lh--last-wire-text ()
  "The text of everything the newest request sent, system message excluded."
  (mapconcat #'hm--text (hm--body (car hm--wires)) "\n"))

(ert-deftest long-history/a-conversation-that-outgrew-the-budget-still-sends ()
  (wrec--with nil (p)
    (lh--with-small-budget
      (let ((view (wrec--view p)))
        ;; Forty turns are about 14000 characters of history: more than twice
        ;; the budget.  Every turn must complete; `hm--turn' asserts it.
        (lh--talk view 40)
        (let ((text (lh--last-wire-text)))
          ;; The newest question is there, with recent history before it.
          (should (string-match-p "LH-QUESTION-039" text))
          (should (string-match-p "LH-ANSWER-038" text))
          ;; The oldest turns were left out, not the newest.
          (should-not (string-match-p "LH-ANSWER-000" text)))))))

(ert-deftest long-history/what-is-sent-stays-inside-the-budget ()
  (wrec--with nil (p)
    (lh--with-small-budget
      (let ((view (wrec--view p)))
        (lh--talk view 40)
        (should (<= (length (lh--last-wire-text)) lh--budget))))))

(ert-deftest long-history/the-history-is-chosen-again-not-dropped ()
  "A history too long for its room is chosen once more under less, and kept:
it is not left out, not cut to a fraction, and not looked at over and over."
  (wrec--with nil (p)
    (lh--with-small-budget
      (let ((view (wrec--view p)) (looks 0) said
            (assemble (symbol-function 'superchat-view-assemble))
            (say (symbol-function 'message)))
        (lh--talk view 39)
        (cl-letf (((symbol-function 'superchat-view-assemble)
                   (lambda (&rest args) (cl-incf looks) (apply assemble args)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (when (stringp format-string)
                       (push (apply #'format format-string args) said))
                     (apply say format-string args))))
          (setq smoke--script '((:text "LH-LAST")))
          (hm--turn view "LH-QUESTION-LAST"))
        (should-not (cl-some (lambda (line) (string-match-p "was left out" line)) said))
        ;; Once eagerly, once for its real room, and again because that did
        ;; not fit: three looks, four at the most.
        (should (<= 3 looks 4))
        ;; Most of the room is used for history.
        (should (> (length (lh--last-wire-text)) (* 0.6 lh--budget)))))))

(ert-deftest long-history/a-short-conversation-carries-all-of-it ()
  "The second look is taken only when the first does not fit."
  (wrec--with nil (p)
    (lh--with-small-budget
      (let ((view (wrec--view p)) (looks 0)
            (assemble (symbol-function 'superchat-view-assemble)))
        (lh--talk view 3)
        (cl-letf (((symbol-function 'superchat-view-assemble)
                   (lambda (&rest args) (cl-incf looks) (apply assemble args))))
          (setq smoke--script '((:text "LH-LAST")))
          (hm--turn view "LH-QUESTION-LAST"))
        (let ((text (lh--last-wire-text)))
          (should (string-match-p "LH-ANSWER-000" text))
          (should (string-match-p "LH-ANSWER-002" text)))
        ;; Once eagerly, once for the room it really has: no third look.
        (should (= 2 looks))))))

(provide 'long-history-regression)
;;; long-history-regression.el ends here
