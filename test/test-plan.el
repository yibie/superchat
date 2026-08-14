;;; test-plan.el --- Structured plan tests: propose, review data, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Code:

(require 'ert)
(require 'superchat-plan)

(ert-deftest plan/emit-tool-normalizes-provider-vectors ()
  "The tool adapter turns JSON arrays into the lists the plan domain expects."
  (let (received)
    (cl-letf (((symbol-function 'superchat-plan-propose)
               (lambda (steps summary)
                 (setq received (list steps summary))
                 "ok")))
      (should
       (equal "ok"
              (superchat-tool-emit-plan
               [["read" "file inspected"] ["plan" "steps verified"]]
               "demo")))
      (should
       (equal '((("read" "file inspected")
                 ("plan" "steps verified"))
                "demo")
              received)))))

(ert-deftest plan/propose-records-event-and-state ()
  "Proposing records plan.proposed and stores steps checked by default."
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
            ;; the review state lives in the session buffer as data, with
            ;; every step checked by default (user unchecks to drop)
            (with-current-buffer superchat-buffer-name
              (should superchat-plan--proposed)
              (let ((steps (plist-get superchat-plan--proposed :steps)))
                (should (= 2 (length steps)))
                (should (eq 1 (plist-get (nth 0 steps) :id)))
                (should (eq t (plist-get (nth 0 steps) :checked)))
                (should (equal "criterion one"
                               (plist-get (nth 0 steps) :accept)))))
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
            ;; uncheck the first step in the review data (the widget
            ;; notify path writes the same field)
            (with-current-buffer superchat-buffer-name
              (setf (plist-get (nth 0 (plist-get
                                       superchat-plan--proposed :steps))
                               :checked)
                    nil))
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
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
                            (superchat-turn-prompt turn)))
    (should (string-match-p "Do not propose a new plan"
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

(ert-deftest plan/propose-rejects-when-plan-active ()
  "A second proposal is declined while a plan exists: one plan, one review."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-uniq-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (setq superchat--active-plan
                    '(:summary "old" :event-id "e" :steps nil)))
            (let ((result
                   (superchat-plan-propose
                    '(("step one" "criterion one")) "demo")))
              (should (string-match-p "declined" result)))
            (with-current-buffer superchat-buffer-name
              (should-not superchat-plan--proposed)))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-auto-executes-when-steps-included ()
  "Approving with checked steps sends the execution follow-up."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-auto-test*"))
           (superchat--session-id "plan-session")
           sent)
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (insert "\n* User: ")
              (setq superchat--prompt-start (point-marker)))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore)
                      ((symbol-function 'superchat-send-input)
                       (lambda () (setq sent t))))
              (superchat-plan-approve))
            (should sent)
            (with-current-buffer superchat-buffer-name
              (should (string-match-p "请按已批准的计划开始执行。"
                                      (buffer-string)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-exits-planning-mode ()
  "Approval clears a plan preset so execution runs without the persona."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-exit-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (insert "\n* User: ")
              (setq superchat--prompt-start (point-marker))
              (setq superchat--active-preset
                    (superchat-preset-from-frontmatter
                     "---\nname: ptest\ntype: plan\nversion: 1\n---\nbody")))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore)
                      ((symbol-function 'superchat-send-input)
                       (lambda () nil)))
              (superchat-plan-approve))
            (with-current-buffer superchat-buffer-name
              (should-not superchat--active-preset)))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/inline-render-creates-stateful-controls ()
  "The inline review block renders widget controls in the chat buffer,
and the widget notify path syncs the value back into the plan data."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-view-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (should superchat-plan--pending-render)
            ;; the turn-completed hook renders the block inline
            (superchat-plan--render-inline)
            (with-current-buffer superchat-buffer-name
              (should-not superchat-plan--pending-render)
              (should (string-match-p "\\*\\* Plan: demo"
                                      (buffer-string)))
              (should (string-match-p "1/1 steps selected"
                                      (buffer-string)))
              (goto-char (point-min))
              (re-search-forward "^\\[X\\] 1\\. step one")
              (should (widget-at (match-beginning 0)))
              (re-search-forward "Approve selected steps")
              (should (widget-at (match-beginning 0)))
              ;; the notify path (as fired by toggling the control)
              ;; unchecks the step in the plan data and updates the count
              (superchat-plan--step-widget-notify
               (widget-convert 'superchat-plan-step-checkbox
                               :value nil :step-id 1))
              (should-not
               (plist-get (nth 0 (plist-get
                                   superchat-plan--proposed :steps))
                          :checked))
              (should (string-match-p "0/1 steps selected"
                                      (buffer-string)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

;;; test-plan.el ends here
