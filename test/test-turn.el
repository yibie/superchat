;;; test-turn.el --- tests for superchat-turn -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-turn)

(defun superchat-test-turn--entry (id kind text data &optional at)
  "An entry of kind KIND with ID, TEXT, DATA and AT, 0.0 by default."
  (list :id id :kind kind :text text :data data :scope "home" :at (or at 0.0)))

(ert-deftest superchat-turn-cut-leaves-a-short-line-and-shortens-a-long-one ()
  (should (equal (superchat-turn-cut "abc" 5) "abc"))
  (should (equal (superchat-turn-cut (make-string 6 ?x) 3) (concat (make-string 3 ?x) "…"))))

(ert-deftest superchat-turn-seal-leaves-the-text-after-it-free ()
  (let ((sealed (superchat-turn-seal "text" 7)))
    (should (get-text-property 0 'read-only sealed))
    (should (= 7 (get-text-property 0 'superchat-turn-id sealed)))
    (should (get-text-property (1- (length sealed)) 'rear-nonsticky sealed))))

(ert-deftest superchat-turn-text-of-a-user-entry-alone ()
  (pcase-let ((`(,head . ,tail)
               (superchat-turn-text (list (superchat-test-turn--entry 1 'user "hi" nil)) nil nil)))
    (should (equal head "* You\nhi\n\n"))
    (should (equal tail ""))))

(ert-deftest superchat-turn-text-shows-a-call-that-has-no-result ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "read" :args (:path "a.el"))))))
         (head (car (superchat-turn-text (list user assistant) nil nil))))
    (should (string-match-p "\\`\\* You\nhi\n\n\\* Assistant\n" head))
    (should (string-match-p "\\*\\* ⏳ Progress  ▮  1 call" head))
    (should (string-match-p "\\*\\*\\* ⏳ read  :path \"a.el\"" head))))

(ert-deftest superchat-turn-text-of-a-turn-that-is-over-has-no-hourglass ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "read" :args (:path "a.el"))))))
         (head (lambda (over) (car (superchat-turn-text (list user assistant) nil over)))))
    (should (string-match-p "\\*\\* ⏳ Progress  ▮  1 call" (funcall head nil)))
    (should (string-match-p "\\*\\* Progress  ▮  1 call" (funcall head t)))
    (should-not (string-match-p "\n\\*\\* ⏳" (funcall head t)))
    ;; The call's own heading still says it has no result.
    (should (string-match-p "\\*\\*\\* ⏳ read" (funcall head t)))))

(ert-deftest superchat-turn-text-has-no-progress-without-a-call ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry 2 'assistant "done" nil))
         (text (superchat-turn-text (list user assistant) nil nil)))
    (should (equal (car text) "* You\nhi\n\n* Assistant\n** Answer\n"))
    (should-not (string-match-p "Progress" (concat (car text) (cdr text))))
    (should (string-match-p "\\`done" (cdr text)))))

(ert-deftest superchat-turn-strip-colours-each-call-by-how-it-went ()
  (let* ((assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "x" :args nil)
                               (:id "c2" :name "y" :args nil)))))
         (ok (superchat-test-turn--entry 3 'tool "fine" '(:call-id "c1" :name "x")))
         (bad (superchat-test-turn--entry 4 'tool "boom" '(:call-id "c2" :name "y" :error t)))
         (strip (superchat-turn-strip (list assistant ok bad))))
    (should (equal (substring-no-properties strip) "▮▮"))
    (should (eq (get-text-property 0 'face strip) 'shadow))
    (should (eq (get-text-property 1 'face strip) 'error))))

(ert-deftest superchat-turn-a-cut-off-call-has-no-duration ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "read" :args (:path "a.el")))) 2.0))
         (ran (superchat-test-turn--entry 3 'tool "fine" '(:call-id "c1" :name "read") 3.5))
         (cut (superchat-test-turn--entry
               4 'tool "Interrupted: Emacs stopped while this call ran; it may have had effects."
               '(:call-id "c1" :name "read" :error t) 97.6))
         (heading (lambda (result)
                    (car (superchat-turn-text (list user assistant result) nil nil)))))
    (should (string-match-p "\\*\\*\\* ✓ read  :path \"a.el\"  (1.5s)\n" (funcall heading ran)))
    (should (string-match-p "\\*\\*\\* ✗ read  :path \"a.el\"\n" (funcall heading cut)))
    (should-not (string-match-p "([0-9.]+s)" (funcall heading cut)))))

(ert-deftest superchat-turn-a-question-shows-at-the-call-it-waited-for ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "write" :args (:path "a.el"))))))
         (asked (superchat-test-turn--entry 3 'event "write a.el: write a.el?"
                                            '(:type "permission" :tool "write" :decision "ask")))
         (question (superchat-test-turn--entry 4 'event "Allow it?" '(:type "question" :call-id "c1")))
         (answer (superchat-test-turn--entry 5 'event "Allowed." '(:type "answer" :call-id "c1")))
         (tool (superchat-test-turn--entry 6 'tool "written" '(:call-id "c1" :name "write")))
         (text (superchat-turn-text (list user assistant asked question answer tool) nil nil))
         (head (car text)))
    ;; The question and its answer are in the call's Progress, at the call it
    ;; waited for, not among the entries after Answer.
    (should (string-search "Allow it?" head))
    (should (string-search "Allowed." head))
    (should (< (string-search "Allow it?" head) (string-search "write  :path" head)))
    (should-not (string-search "Allow" (cdr text)))))

(ert-deftest superchat-turn-a-redirect-shows-where-it-came ()
  (let* ((e #'superchat-test-turn--entry)
         (user (funcall e 1 'user "hi" nil))
         (a1 (funcall e 2 'assistant "" '(:calls ((:id "c1" :name "read" :args (:path "a.el"))))))
         (t1 (funcall e 3 'tool "x" '(:call-id "c1" :name "read")))
         (go (funcall e 4 'event "Going round: 20 calls" '(:type "redirect")))
         (a2 (funcall e 5 'assistant "" '(:calls ((:id "c2" :name "read" :args (:path "b.el"))))))
         (t2 (funcall e 6 'tool "y" '(:call-id "c2" :name "read")))
         (done (funcall e 7 'assistant "Done." nil)))
    ;; Before the call that came after it, inside Progress.
    (let ((head (car (superchat-turn-text (list user a1 t1 go a2 t2 done) nil nil))))
      (should (< (string-search "a.el" head) (string-search "Going round" head)
                 (string-search "b.el" head))))
    ;; With no call after it, at the end of Progress, before Answer.
    (let ((text (superchat-turn-text (list user a1 t1 go done) nil nil)))
      (should (< (string-search "Going round" (car text)) (string-search "** Answer" (car text))))
      (should-not (string-search "Going round" (cdr text))))))

(ert-deftest superchat-turn-a-question-heading-folds ()
  (let* ((user (superchat-test-turn--entry 1 'user "hi" nil))
         (assistant (superchat-test-turn--entry
                     2 'assistant ""
                     '(:calls ((:id "c1" :name "write" :args nil)))))
         (question (superchat-test-turn--entry 3 'event "Allow it?" '(:type "question" :call-id "c1")))
         (head (car (superchat-turn-text (list user assistant question) nil nil)))
         (at (string-search "Allow it?" head)))
    (should at)
    ;; A heading, so TAB folds it by default: the fold property sits on it.
    (should (eq (cadr (get-text-property at 'superchat-turn-fold head)) t))))

(provide 'test-turn)
;;; test-turn.el ends here
