;;; test-turn.el --- tests for superchat-turn -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-turn)

(defun superchat-test-turn--entry (id kind text data)
  "An entry of kind KIND with ID, TEXT and DATA."
  (list :id id :kind kind :text text :data data :scope "home" :at 0.0))

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

(provide 'test-turn)
;;; test-turn.el ends here
