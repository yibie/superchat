;;; annotate-regression.el --- remarks hung on the text they are about -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-10-remarks-hung-on-the-text.md.
;; `superchat-annotate' asks the model for remarks on the region or the
;; buffer and hangs each one on the text it is about.  These tests send a
;; real request to a scripted model and read what it received and what the
;; buffer shows afterwards: a remark is placed where its quote is, on the
;; stated line or within two lines of it, and nowhere else; the text is never
;; changed; an answer that gives nothing is said to give nothing.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/annotate-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^annotate/")'

;;; Code:

(load (expand-file-name "workspace-rules-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-annotate)

(defconst ann--text "alpha one\nbeta two\ngamma three\ndelta four\nepsilon five\nzeta six\n"
  "Six lines, each with a word found on no other line.")

(defun ann--file (root &optional text)
  "Return a live buffer visiting ROOT/notes.txt holding TEXT."
  (let ((file (expand-file-name "notes.txt" root)))
    (with-temp-file file (insert (or text ann--text)))
    (find-file-noselect file)))

(defun ann--said (function)
  "Call FUNCTION and return what `message' said meanwhile, one string."
  (let (said)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (push (apply #'format format-string args) said))
                 nil)))
      (funcall function))
    (mapconcat #'identity (nreverse said) "\n")))

(defun ann--annotate (buffer &optional beg end)
  "Annotate BUFFER (BEG..END as the region when given), wait, return messages."
  (ann--said
   (lambda ()
     (with-current-buffer buffer
       (if beg
           (let ((transient-mark-mode t))
             (goto-char beg)
             (push-mark end t t)
             (superchat-annotate "look"))
         (deactivate-mark)
         (superchat-annotate "look")))
     (wrules--settled))))

(defun ann--placed (buffer)
  "BUFFER's annotations as (LINE TEXT REMARK), in order."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (list (line-number-at-pos (overlay-start overlay))
                    (buffer-substring-no-properties (overlay-start overlay)
                                                    (overlay-end overlay))
                    (plist-get (overlay-get overlay 'superchat-annotate) :remark)))
            (superchat-annotate-overlays))))

(defun ann--user (n)
  "The user text of the Nth provider request."
  (hm--text (car (last (hm--wire n)))))

;;;; Placement --------------------------------------------------------------------

(ert-deftest annotate/a-remark-is-hung-on-its-quote-on-the-stated-line ()
  (wrules--with '((:text "2 | beta | REMARK-B\n5 | five | REMARK-E")) (p)
    (let* ((buffer (ann--file p))
           (said (ann--annotate buffer)))
      (should (equal '((2 "beta" "REMARK-B") (5 "five" "REMARK-E"))
                     (ann--placed buffer)))
      (should (string-match-p "2 remarks placed" said))
      (should-not (string-match-p "not placed\\|not understood" said))
      ;; The remark is shown at the end of its line.
      (with-current-buffer buffer
        (let ((tail (overlay-get (car (superchat-annotate-overlays))
                                 'superchat-annotate-tail)))
          (should (= (overlay-start tail)
                     (save-excursion (goto-char (point-min)) (forward-line 1)
                                     (line-end-position))))
          (should (string-match-p "REMARK-B" (overlay-get tail 'after-string))))))))

;;;; How a remark is shown ------------------------------------------------------

(defun ann--tails (buffer)
  "BUFFER's overlays that show remarks, ordered by position."
  (with-current-buffer buffer
    (sort (cl-remove-if-not
           (lambda (overlay) (overlay-get overlay 'superchat-annotate-tail-p))
           (overlays-in (point-min) (point-max)))
          (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun ann--shown (buffer)
  "What BUFFER's first remark overlay shows, without text properties."
  (substring-no-properties (overlay-get (car (ann--tails buffer)) 'after-string)))

(ert-deftest annotate/a-remark-is-shown-as-a-block-under-its-line ()
  (wrules--with '((:text "2 | beta | REMARK-B")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (let* ((tail (car (ann--tails buffer)))
             (string (overlay-get tail 'after-string)))
        (should (equal "\n└─▶  REMARK-B " (substring-no-properties string)))
        ;; The cursor stays at the end of the line, before the block.
        (should (get-text-property 0 'cursor string))
        (should (eq 'superchat-annotate-remark
                    (get-text-property (1- (length string)) 'face string)))))))

(ert-deftest annotate/a-long-remark-is-wrapped-within-the-width ()
  (let* ((remark (mapconcat #'identity (make-list 30 "word") " "))
         (superchat-annotate-remark-width 40))
    (wrules--with `((:text ,(concat "2 | beta | " remark))) (p)
      (let ((buffer (ann--file p)))
        (ann--annotate buffer)
        (let ((lines (split-string (ann--shown buffer) "\n" t)))
          (should (> (length lines) 3))
          (dolist (line lines)
            (should (<= (string-width line) 40)))
          (should (string-prefix-p "└─▶ " (car lines)))
          (should (string-prefix-p "    " (cadr lines)))
          ;; Nothing of the remark is lost by wrapping.
          (should (equal remark
                         (mapconcat (lambda (line) (string-trim (substring line 4)))
                                    lines " "))))))))

(ert-deftest annotate/remarks-of-one-line-are-stacked-in-text-order ()
  ;; Whichever of the two the model names first.
  (dolist (answer '("2 | two | SECOND\n2 | beta | FIRST\n5 | five | OTHER"
                    "2 | beta | FIRST\n2 | two | SECOND\n5 | five | OTHER"))
    (wrules--with `((:text ,answer)) (p)
      (let ((buffer (ann--file p)))
        (ann--annotate buffer)
        (should (= 2 (length (ann--tails buffer))))
        (should (equal "\n└─▶  FIRST \n└─▶  SECOND " (ann--shown buffer)))))))

(ert-deftest annotate/the-block-is-indented-like-its-line ()
  (wrules--with '((:text "2 | inner | DEEP")) (p)
    (let ((buffer (ann--file p "top\n    inner line\n")))
      (ann--annotate buffer)
      (should (equal "\n    └─▶  DEEP " (ann--shown buffer))))))

(ert-deftest annotate/the-end-of-line-style-keeps-the-remark-on-the-line ()
  (let ((superchat-annotate-remark-style 'eol))
    (wrules--with '((:text "2 | beta | REMARK-B")) (p)
      (let ((buffer (ann--file p)))
        (ann--annotate buffer)
        (should (equal "  ⟵ REMARK-B" (ann--shown buffer)))))))

(ert-deftest annotate/removing-one-remark-of-a-line-keeps-the-other ()
  (wrules--with '((:text "2 | beta | FIRST\n2 | two | SECOND")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (with-current-buffer buffer
        (superchat-annotate--delete (car (superchat-annotate-overlays)))
        (should (equal "\n└─▶  SECOND " (ann--shown buffer)))
        (superchat-annotate--delete (car (superchat-annotate-overlays)))
        (should-not (ann--tails buffer))))))

(ert-deftest annotate/the-text-is-never-changed ()
  (wrules--with '((:text "1 | alpha | R1\n3 | gamma | R3\n9 | nowhere | R9")) (p)
    (let* ((buffer (ann--file p))
           (before (with-current-buffer buffer (buffer-string)))
           (tick (with-current-buffer buffer (buffer-chars-modified-tick))))
      (ann--annotate buffer)
      (with-current-buffer buffer
        (should (equal before (buffer-string)))
        (should (= tick (buffer-chars-modified-tick)))
        (should-not (buffer-modified-p))
        ;; Clearing does not change it either.
        (superchat-annotate-clear)
        (should-not (superchat-annotate-overlays))
        (should (= tick (buffer-chars-modified-tick)))))))

(ert-deftest annotate/a-line-number-off-by-up-to-two-is-put-right-by-the-quote ()
  ;; Measured: a model's line number is sometimes one off; the quote says where.
  (wrules--with '((:text "2 | gamma | ONE-OFF\n6 | delta | TWO-OFF\n1 | beta | BEFORE")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should (equal '((2 "beta" "BEFORE") (3 "gamma" "ONE-OFF") (4 "delta" "TWO-OFF"))
                     (ann--placed buffer))))))

(ert-deftest annotate/a-quote-not-found-near-the-line-is-placed-nowhere ()
  ;; "zeta" is on line 6, three lines from 3: too far.  "omega" is nowhere.
  (wrules--with '((:text "3 | zeta | TOO-FAR\n2 | omega | MADE-UP\n4 |  | NO-QUOTE\n1 | alpha | FINE"))
      (p)
    (let* ((buffer (ann--file p))
           (said (ann--annotate buffer)))
      (should (equal '((1 "alpha" "FINE")) (ann--placed buffer)))
      (should (string-match-p "1 remark placed, 3 not placed" said))
      (with-current-buffer buffer
        (should (equal '("TOO-FAR" "MADE-UP" "NO-QUOTE")
                       (mapcar (lambda (row) (plist-get row :remark))
                               (plist-get superchat-annotate--report :unplaced)))))
      ;; The list shows them, so nothing the model said is lost.
      (with-current-buffer buffer (superchat-annotate-list))
      (with-current-buffer superchat-annotate--list-buffer
        (should (string-match-p "Not placed (3)" (buffer-string)))
        (should (string-match-p "MADE-UP" (buffer-string)))
        (should (string-match-p "FINE" (buffer-string))))
      (kill-buffer superchat-annotate--list-buffer))))

(ert-deftest annotate/rows-not-in-the-asked-form-are-counted-not-dropped ()
  (wrules--with '((:text "Here are my remarks:\n2 | beta | OK\n- gamma looks odd")) (p)
    (let* ((buffer (ann--file p))
           (said (ann--annotate buffer)))
      (should (equal '((2 "beta" "OK")) (ann--placed buffer)))
      (should (string-match-p "1 remark placed, 2 rows not understood" said))
      (with-current-buffer buffer
        (should (equal '("Here are my remarks:" "- gamma looks odd")
                       (plist-get superchat-annotate--report :unparsed)))))))

(ert-deftest annotate/an-answer-with-no-rows-is-said-to-give-nothing ()
  ;; A local model that spends its whole output thinking answers with nothing;
  ;; that must not read as "nothing to remark".
  (wrules--with '((:text "I cannot do that.")) (p)
    (let* ((buffer (ann--file p))
           (said (ann--annotate buffer)))
      (should-not (ann--placed buffer))
      (should (string-match-p "gave no remarks in the asked form" said))
      (should-not (string-match-p "remarks? placed" said)))))

(ert-deftest annotate/an-empty-answer-is-said-to-be-empty ()
  (wrules--with '((:text "")) (p)
    (let* ((buffer (ann--file p))
           (said (ann--annotate buffer)))
      (should-not (ann--placed buffer))
      (should (string-match-p "gave no remarks: its answer was empty" said))
      (with-current-buffer buffer
        (should (plist-get superchat-annotate--report :empty))
        ;; Nothing is left on screen saying a request is still under way.
        (should-not (cl-some (lambda (o) (overlay-get o 'superchat-annotate-pending))
                             (overlays-in (point-min) (point-max))))))))

(ert-deftest annotate/a-quote-written-in-quotation-marks-is-still-found ()
  (wrules--with '((:text "2 | \"beta\" | Q1\n4 | “delta” | Q2\n6 | `zeta` | Q3")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should (equal '((2 "beta" "Q1") (4 "delta" "Q2") (6 "zeta" "Q3"))
                     (ann--placed buffer))))))

(ert-deftest annotate/a-quote-cut-short-with-dots-is-found-by-what-is-left ()
  ;; Measured: DeepSeek ended a long quote with "..." in 2 of 3 runs on code.
  (wrules--with '((:text "2 | beta t... | DOTS\n4 | …lta four | LEADING\n5 | epsilon … | SPACED\n6 | omega... | STILL-MADE-UP\n1 | ... | ONLY-DOTS"))
      (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should (equal '((2 "beta t" "DOTS") (4 "lta four" "LEADING") (5 "epsilon" "SPACED"))
                     (ann--placed buffer)))
      (with-current-buffer buffer
        (should (equal '("STILL-MADE-UP" "ONLY-DOTS")
                       (mapcar (lambda (row) (plist-get row :remark))
                               (plist-get superchat-annotate--report :unplaced))))))))

(ert-deftest annotate/dots-that-are-in-the-text-are-part-of-the-quote ()
  (wrules--with '((:text "2 | so on... | KEPT")) (p)
    (let ((buffer (ann--file p "one\nand so on... and on\nthree\n")))
      (ann--annotate buffer)
      (should (equal '((2 "so on..." "KEPT")) (ann--placed buffer))))))

;;;; The region -------------------------------------------------------------------

(ert-deftest annotate/line-numbers-count-from-the-start-of-the-region ()
  (wrules--with '((:text "1 | gamma | FIRST-OF-REGION\n3 | epsilon | THIRD")) (p)
    (let* ((buffer (ann--file p))
           (beg (with-current-buffer buffer
                  (goto-char (point-min)) (forward-line 2) (+ (point) 2)))
           (end (with-current-buffer buffer
                  (goto-char (point-min)) (forward-line 4) (+ (point) 3))))
      (ann--annotate buffer beg end)
      ;; The model saw whole lines 3..5 of the buffer, numbered 1..3.
      (let ((user (ann--user 1)))
        (should (string-match-p "^1| gamma three$" user))
        (should (string-match-p "^3| epsilon five$" user))
        (should-not (string-match-p "beta two\\|zeta six\\|^4| " user)))
      (should (equal '((3 "gamma" "FIRST-OF-REGION") (5 "epsilon" "THIRD"))
                     (ann--placed buffer))))))

(ert-deftest annotate/a-remark-is-never-placed-outside-the-region ()
  ;; Line 3 of the region is the buffer's line 5; "zeta" is one line below
  ;; it, near enough, but outside what was asked about.
  (wrules--with '((:text "3 | zeta | OUTSIDE\n1 | beta | ABOVE")) (p)
    (let* ((buffer (ann--file p))
           (beg (with-current-buffer buffer
                  (goto-char (point-min)) (forward-line 2) (point)))
           (end (with-current-buffer buffer
                  (goto-char (point-min)) (forward-line 4) (line-end-position))))
      (ann--annotate buffer beg end)
      (should-not (ann--placed buffer)))))

(ert-deftest annotate/asking-again-replaces-the-remarks-of-those-lines ()
  (wrules--with '((:text "2 | beta | OLD") (:text "2 | two | NEW")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should (equal '((2 "beta" "OLD")) (ann--placed buffer)))
      (ann--annotate buffer)
      (should (equal '((2 "two" "NEW")) (ann--placed buffer))))))

;;;; Going from one to the other ---------------------------------------------------

(ert-deftest annotate/next-and-previous-go-round ()
  (wrules--with '((:text "2 | beta | B\n5 | five | E")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (ann--said #'superchat-annotate-next)
        (should (= 2 (line-number-at-pos)))
        (ann--said #'superchat-annotate-next)
        (should (= 5 (line-number-at-pos)))
        (ann--said #'superchat-annotate-next)
        (should (= 2 (line-number-at-pos)))
        (ann--said #'superchat-annotate-previous)
        (should (= 5 (line-number-at-pos)))))))

;;;; What the model receives --------------------------------------------------------

(ert-deftest annotate/the-request-asks-for-rows-and-shows-numbered-lines ()
  (wrules--with '((:text "1 | alpha | R")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should (= 1 (length hm--wires)))
      (let ((system (wrules--system 1))
            (user (ann--user 1)))
        (should (string-match-p "LINE | QUOTE | REMARK" system))
        (should (string-match-p "^1| alpha one$" user))
        (should (string-match-p "^6| zeta six$" user))
        ;; No seventh, empty line for the file's final newline.
        (should-not (string-match-p "^7| " user))
        (should (string-match-p "Instruction: look" user))))))

(ert-deftest annotate/the-request-offers-the-model-no-tools ()
  (wrules--with '((:text "1 | alpha | R")) (p)
    (let* ((buffer (ann--file p))
           (network (symbol-function 'smoke--fake-network))
           (offered 'unset))
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (lambda (mode provider prompt &rest rest)
                   (setq offered (llm-chat-prompt-tools prompt))
                   (apply network mode provider prompt rest))))
        (ann--annotate buffer))
      (should (null offered)))))

(ert-deftest annotate/the-request-carries-the-workspace-rules-and-its-own-note ()
  (wrules--with '((:text "1 | alpha | R")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P: start every answer with a line of reasoning.\n")
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (let ((system (wrules--system 1)))
        (should (= 1 (ann--count "RULE-MARKER-P" system)))
        ;; After the rules: which of them apply to rows read by a program.
        (should (string-match-p "RULE-MARKER-P\\(.\\|\n\\)*hangs each remark" system))
        ;; Not the rewrite's note, and not the note about remembering.
        (should-not (string-match-p "in place of the target" system))
        (should-not (string-match-p "explicitly asks you to remember" system))
        ;; The rows are still asked for, after the rules.
        (should (string-match-p "hangs each remark\\(.\\|\n\\)*LINE | QUOTE | REMARK"
                                system))))))

(ert-deftest annotate/without-rules-the-request-has-no-note-about-them ()
  (wrules--with '((:text "1 | alpha | R")) (p)
    (let ((buffer (ann--file p)))
      (ann--annotate buffer)
      (should-not (string-match-p "hangs each remark\\|Workspace rules"
                                  (wrules--system 1))))))

(defun ann--count (needle text)
  "How many times NEEDLE occurs in TEXT."
  (let ((start 0) (n 0))
    (while (string-match (regexp-quote needle) text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(provide 'annotate-regression)
;;; annotate-regression.el ends here
