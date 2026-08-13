;;; test-plan.el --- Tests for structured plan objects -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-plan)
(require 'superchat-prompt-hooks)

(defvar superchat-buffer-name)
(defvar superchat--session-id)
(defvar superchat--active-plan)
(defvar superchat-plan--proposed)

(ert-deftest plan/validate-steps-shapes ()
  "Step validation rejects malformed plans with readable reasons."
  (should-not (superchat-plan--validate-steps
               '(("read the code" "report the entry points"))))
  (should (stringp (superchat-plan--validate-steps nil)))
  (should (stringp (superchat-plan--validate-steps
                    '(("only text")))))
  (should (stringp (superchat-plan--validate-steps
                    '((1 "x")))))
  (should (stringp (superchat-plan--validate-steps
                    '(("" "x"))))))

(ert-deftest plan/propose-renders-tree-and-records-event ()
  "Proposing renders checkboxes and appends plan.proposed."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (let ((result
                   (superchat-plan-propose
                    '(("step one" "criterion one")
                      ("step two" "criterion two"))
                    "demo plan")))
              (should (string-match-p "Plan proposed: 2 steps" result)))
            (with-current-buffer superchat-buffer-name
              (should (string-match-p "\\*\\* Plan: demo plan  \\[0/2\\]"
                                      (buffer-string)))
              (should (string-match-p "- \\[ \\] step one" (buffer-string)))
              (should (string-match-p ":accept: criterion one"
                                      (buffer-string))))
            ;; the tape has the proposed event
            (let* ((sessions (superchat-view-session-ids))
                   (events (superchat-experience--view-events sessions)))
              (should
               (cl-some
                (lambda (e)
                  (equal "plan.proposed" (plist-get e :type)))
                events))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-records-included-steps ()
  "Approval marks checked steps included and appends plan.approved."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-approve-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")
               ("step two" "criterion two"))
             "demo")
            ;; check the first step in the rendered tree (text-level
            ;; toggle; the buffer is not an org-mode buffer in batch)
            (with-current-buffer superchat-buffer-name
              (goto-char (point-min))
              (re-search-forward (regexp-quote "- [ ] step one"))
              (let ((inhibit-read-only t))
                (replace-match "- [X] step one")))
            (cl-letf (((symbol-function 'message) #'ignore))
              (superchat-plan-approve))
            (with-current-buffer superchat-buffer-name
              (should superchat--active-plan)
              (should (= 1 (cl-count t
                                     (plist-get superchat--active-plan :steps)
                                     :key (lambda (s) (plist-get s :included))
                                     :test #'eq))))
            (let* ((sessions (superchat-view-session-ids))
                   (events (superchat-experience--view-events sessions)))
              (should
               (cl-some
                (lambda (e)
                  (equal "plan.approved" (plist-get e :type)))
                events))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/prompt-hook-injects-active-plan ()
  "The approved plan reaches the execution context, and only then."
  (let ((superchat--active-plan nil)
        (turn (superchat-turn-new "do the work")))
    (setf (superchat-turn-prompt turn) "base task")
    (superchat-prompt-hook--active-plan turn)
    (should (equal "base task" (superchat-turn-prompt turn)))
    (setq superchat--active-plan
          (list :summary "demo"
                :steps '((:id 1 :text "step one" :accept "criterion one"
                          :included t))))
    (superchat-prompt-hook--active-plan turn)
    (should (string-match-p "Approved plan: demo"
                            (superchat-turn-prompt turn)))
    (should (string-match-p "step one"
                            (superchat-turn-prompt turn)))))

(ert-deftest plan/file-ref-range-parses-and-slices ()
  "The # prefix accepts :START and :START-END line ranges."
  (let ((file (make-temp-file "range-test-" nil ".txt"
                              "l1\nl2\nl3\nl4\nl5\n"))
        (superchat-inline-max-bytes 100000))
    (unwind-protect
        (progn
          (should (equal (cons 2 4)
                         (superchat-parser-extract-file-range
                          (format "#%s:2-4" file))))
          (should (equal (cons 7 7)
                         (superchat-parser-extract-file-range
                          (format "#%s:7" file))))
          (should-not (superchat-parser-extract-file-range
                       (format "#%s" file)))
          (superchat--extract-file-path (format "#%s:2-4" file))
          (should (equal "l2\nl3\nl4"
                         (superchat--read-inline-file-content file))))
      (delete-file file))))

;;; test-plan.el ends here
