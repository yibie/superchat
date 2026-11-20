;;; test-guard.el --- tests for superchat-guard -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-guard)

(defun superchat-test-guard--run (steps)
  "Entries of a run from STEPS, each (NAME ARGS RESULT &optional ERROR)."
  (let ((id 0) entries)
    (dolist (s steps (nreverse entries))
      (pcase-let ((`(,name ,args ,text ,err) s))
        (push (list :id (cl-incf id) :kind 'assistant
                    :data (list :calls (list (list :id (format "c%d" id) :name name :args args))))
              entries)
        (push (list :id (cl-incf id) :kind 'tool :text text
                    :data (list :call-id (format "c%d" (1- id)) :name name :error err))
              entries)))))

(defun superchat-test-guard--reads (n)
  (make-list n '("read" (:path "a.el") "text")))

(ert-deftest superchat-guard-counts-a-run ()
  (let ((s (superchat-guard-signals
            (superchat-test-guard--run
             '(("edit" (:path "x.el" :old "a" :new "b") "Replaced 1 block in x.el.")
               ("shell" (:command "make") "x.el:12: error in /tmp/a/x.el\n\n[exit status 2]")
               ("edit" (:path "x.el" :old "b" :new "c") "Replaced 1 block in x.el.")
               ;; The same failure, with other numbers and paths, matches.
               ("shell" (:command "make") "x.el:40: error in /tmp/b/x.el\n\n[exit status 2]")
               ("read" (:path "y.el") "Error: No such file" t)
               ("shell" (:command "true") "ok\n\n[exit status 0]"))))))
    (should (equal s '(:calls 6 :errors 3 :repeats 1 :churn 2 :file "x.el")))
    (should (equal (superchat-guard-counts s)
                   "6 calls; x.el changed 2 times; 3 failed, 1 of them the same failure again"))))

(ert-deftest superchat-guard-sees-a-run-going-round ()
  ;; Not before the run has made enough calls.
  (should-not (superchat-guard-going-round-p
               (superchat-guard-signals
                (superchat-test-guard--run
                 (make-list 6 '("edit" (:path "x.el" :old "a" :new "b") "Replaced 1 block in x.el."))))))
  ;; One file changed four times, every call succeeding: going round all the same.
  (should (superchat-guard-going-round-p
           (superchat-guard-signals
            (superchat-test-guard--run
             (append (superchat-test-guard--reads 14)
                     (make-list 4 '("edit" (:path "x.el" :old "a" :new "b") "Replaced 1 block in x.el.")))))))
  ;; A failure that comes back.
  (should (superchat-guard-going-round-p
           (superchat-guard-signals
            (superchat-test-guard--run
             (append (superchat-test-guard--reads 16)
                     (make-list 2 '("shell" (:command "make") "Error 1\n\n[exit status 1]")))))))
  ;; A long run that reads and changes different files is left alone.
  (should-not (superchat-guard-going-round-p
               (superchat-guard-signals
                (superchat-test-guard--run
                 (append (superchat-test-guard--reads 30)
                         (cl-loop for f in '("a.el" "b.el" "c.el")
                                  collect `("edit" (:path ,f :old "a" :new "b") "Replaced."))))))))

(ert-deftest superchat-guard-redirect-names-the-counts ()
  (let ((text (superchat-guard-redirect '(:calls 20 :errors 0 :repeats 0 :churn 5 :file "x.el"))))
    (should (string-search "20 calls; x.el changed 5 times" text))
    (should (string-search "Do not stop" text))))

(provide 'test-guard)
;;; test-guard.el ends here
