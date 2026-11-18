;;; test-request.el --- tests for superchat-request -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-request)

(defun superchat-test-request--e (id kind text &optional data)
  (list :id id :kind kind :text text :data data))

(ert-deftest superchat-request-messages-keep-a-call-with-its-result ()
  (let ((entries (list (superchat-test-request--e 1 'user "go")
                       (superchat-test-request--e 2 'assistant ""
                                                  '(:calls ((:id "c1" :name "t" :args nil))))
                       (superchat-test-request--e 3 'timer "later" '(:at 1.0))
                       (superchat-test-request--e 4 'tool "done" '(:call-id "c1" :name "t"))
                       (superchat-test-request--e 5 'event "x" '(:type "level")))))
    (should (equal (mapcar (lambda (m) (plist-get m :role)) (superchat-request-messages entries))
                   '(user assistant tool)))
    (should-not (superchat-request-message (nth 2 entries)))
    (should (= (superchat-request-size (superchat-request-messages entries))
               (+ 2 0 (length "nil") 4)))))

(ert-deftest superchat-request-budget-follows-the-option-else-the-window ()
  (let ((superchat-context-chars 500))
    (should (= (superchat-request-budget) 500)))
  (let ((superchat-context-chars nil))
    (cl-letf (((symbol-function 'superchat-llm-context-window) (lambda (&optional _) 8000)))
      (should (= (superchat-request-budget "m") 4000)))
    (cl-letf (((symbol-function 'superchat-llm-context-window) (lambda (&optional _) nil)))
      (should (= (superchat-request-budget "m") 120000)))))

(ert-deftest superchat-request-fit-leaves-out-the-oldest-and-keeps-the-run ()
  (let* ((entries (list (superchat-test-request--e 1 'user (make-string 500 ?a))
                        (superchat-test-request--e 2 'assistant (make-string 100 ?b))
                        (superchat-test-request--e 3 'user "now")))
         (fit (superchat-request-fit entries 3 50)))
    ;; The latest assistant entry is never left out, so only the first goes.
    (should (string-match-p "\\`\\[1 earlier entries" (plist-get (car fit) :text)))
    (should (equal (mapcar (lambda (m) (plist-get m :role)) fit) '(user assistant user)))
    (should (equal (plist-get (car (last fit)) :text) "now"))
    (should (= (superchat-request-kept-size entries 3) 103))))

(provide 'test-request)
;;; test-request.el ends here
