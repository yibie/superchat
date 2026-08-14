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

(ert-deftest plan/view-frame-carries-stateful-controls ()
  "The review frame projects the plan data into stateful checkbox controls,
and the widget notify path syncs the value back into the data."
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
            (let* ((frame (superchat-plan--view-frame))
                   (els (plist-get (car frame) :children))
                   (step-el
                    (cl-find 'superchat-plan-step-checkbox els
                             :key (lambda (el) (plist-get el :type))))
                   (approve-el
                    (cl-find 'push-button els
                             :key (lambda (el) (plist-get el :type)))))
              (should step-el)
              (should (eq t (plist-get step-el :value)))
              (should (eq 1 (plist-get step-el :step-id)))
              (should (eq #'superchat-plan--view-approve
                          (plist-get approve-el :notify)))
              (should (eq widget-keymap (plist-get approve-el :keymap)))
              ;; render-level smoke: the frame must pass TextUI's
              ;; measure/validate pass (catches :type symbol-vs-keyword
              ;; mistakes that batch widget tests would miss)
              (when (fboundp 'textui--prepare-frame)
                (should (textui--prepare-frame frame)))
              ;; the notify path (as fired by toggling the control)
              ;; unchecks the step in the plan data
              (superchat-plan--step-widget-notify
               (widget-convert 'superchat-plan-step-checkbox
                               :value nil :step-id 1))
              (with-current-buffer superchat-buffer-name
                (should-not
                 (plist-get (nth 0 (plist-get
                                    superchat-plan--proposed :steps))
                            :checked)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/reopen-restores-unresolved-review ()
  "Rebuilding a session View restores an unapproved plan's review block."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-reopen-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-reopen-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            ;; a fresh View buffer simulates the reopened session: no
            ;; local plan state, same session id
            (let ((superchat-buffer-name fresh-buffer-name))
              (with-current-buffer (get-buffer-create fresh-buffer-name)
                (erase-buffer)
                (superchat-mode 1)
                (setq-local superchat--session-id "plan-session")
                (superchat-plan--restore-on-reopen "plan-session")
                (should superchat-plan--proposed)
                (should
                 (eq t (plist-get
                        (nth 0 (plist-get
                                superchat-plan--proposed :steps))
                        :checked)))
                ;; the restored review is the review frame again
                (should
                 (cl-find 'superchat-plan-step-checkbox
                          (plist-get (car (superchat-plan--view-frame))
                                     :children)
                          :key (lambda (el) (plist-get el :type))))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/reopen-skips-resolved-review ()
  "An approved plan does not resurrect its review on a rebuilt View."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-reopen-skip-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-reopen-skip-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((superchat-buffer-name fresh-buffer-name))
              (with-current-buffer (get-buffer-create fresh-buffer-name)
                (erase-buffer)
                (superchat-mode 1)
                (setq-local superchat--session-id "plan-session")
                (superchat-plan--restore-on-reopen "plan-session")
                (should-not superchat-plan--proposed))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/clear-appends-resolved-event ()
  "Clearing a plan under review appends plan.cleared, so a rebuilt View
stays clear."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-clear-event-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-clear-event-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (let ((proposed-id
                   (with-current-buffer superchat-buffer-name
                     (plist-get superchat-plan--proposed :event-id))))
              (with-current-buffer superchat-buffer-name
                (superchat-plan-clear))
              (let* ((sessions (superchat-view-session-ids))
                     (events (superchat-experience--view-events sessions)))
                (should
                 (cl-some
                  (lambda (e)
                    (and (equal "plan.cleared" (plist-get e :type))
                         (equal proposed-id
                                (plist-get
                                 (plist-get (plist-get e :payload)
                                            :proposed)
                                 :event-id))))
                  events)))
              (let ((superchat-buffer-name fresh-buffer-name))
                (with-current-buffer (get-buffer-create fresh-buffer-name)
                  (erase-buffer)
                  (superchat-mode 1)
                  (setq-local superchat--session-id "plan-session")
                  (superchat-plan--restore-on-reopen "plan-session")
                  (should-not superchat-plan--proposed)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/report-progress-updates-state-and-tape ()
  "report_plan_progress marks step status and records plan.progress."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-test*"))
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
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((result
                   (superchat-plan-report-progress
                    '((1 "done") (2 "in-progress")))))
              (should (string-prefix-p "Progress recorded" result))
              (with-current-buffer superchat-buffer-name
                (should
                 (equal "done"
                        (plist-get
                         (nth 0 (plist-get superchat--active-plan :steps))
                         :status)))
                (should
                 (equal "in-progress"
                        (plist-get
                         (nth 1 (plist-get superchat--active-plan :steps))
                         :status))))
              (let* ((sessions (superchat-view-session-ids))
                     (events (superchat-experience--view-events sessions)))
                (should
                 (cl-some
                  (lambda (e)
                    (and (equal "plan.progress" (plist-get e :type))
                         (equal 1 (plist-get (plist-get e :payload)
                                             :step-id))
                         (equal "done"
                                (plist-get (plist-get e :payload)
                                           :status))))
                  events)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/report-progress-rejects-invalid ()
  "Progress updates validate shape, statuses, and step membership."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-reject-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            ;; no active plan
            (should
             (string-prefix-p
              "Error: no active plan"
              (superchat-plan-report-progress '((1 "done")))))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (should
             (string-prefix-p
              "Error: update"
              (superchat-plan-report-progress '((1 "almost")))))
            (should
             (string-prefix-p
              "Error: step 7 is not part"
              (superchat-plan-report-progress '((7 "done")))))
            (should
             (string-prefix-p
              "Error: updates must be"
              (superchat-plan-report-progress "done"))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/progress-block-renders-and-updates ()
  "Approval converts the review block into a progress view that updates."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-block-test*"))
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
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            ;; the review frame is consumed: the view now projects the
            ;; progress frame with status markers
            (let* ((els (plist-get (car (superchat-plan--view-frame))
                                   :children))
                   (values (mapcar (lambda (el) (plist-get el :value))
                                   els)))
              (should-not
               (cl-find 'push-button els
                        :key (lambda (el) (plist-get el :type))))
              (should (cl-some (lambda (v)
                                 (string-match-p "— approved" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "0/2 steps done" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "\\[ \\] 1\\. step one" v))
                               values)))
            (superchat-plan-report-progress '((1 "done")))
            (let* ((els (plist-get (car (superchat-plan--view-frame))
                                   :children))
                   (values (mapcar (lambda (el) (plist-get el :value))
                                   els)))
              (should (cl-some (lambda (v)
                                 (string-match-p "1/2 steps done" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "\\[✓\\] 1\\. step one" v))
                               values)))
            (with-current-buffer superchat-buffer-name
              (should (string-match-p "report_plan_progress"
                                      (superchat-plan--context-string
                                       superchat--active-plan)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name)))))))

(ert-deftest plan/nudge-pushes-pending-execution ()
  "A completed execution turn with pending steps triggers a follow-up
nudge (capped), until every included step reports done."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-nudge-test*"))
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
             '(("step one" "criterion one")
               ("step two" "criterion two"))
             "demo")
            (cl-letf (((symbol-function 'message) #'ignore)
                      ((symbol-function 'superchat-send-input)
                       (lambda () (setq sent t))))
              (superchat-plan-approve))
            (should sent)
            (with-current-buffer superchat-buffer-name
              (should superchat-plan--nudge-pending))
            ;; the execution turn completes with nothing reported:
            ;; the nudge pushes a follow-up listing the pending steps
            (setq sent nil)
            (cl-letf (((symbol-function 'superchat-send-input)
                       (lambda () (setq sent t))))
              (with-current-buffer superchat-buffer-name
                (superchat-plan--nudge-on-completion
                 '(:status completed)))
              (should sent)
              (with-current-buffer superchat-buffer-name
                (should (string-match-p "计划还有 2 步未完成"
                                        (buffer-string)))
                ;; one step reports done; the next completion nudges
                ;; once more, then the cap stops further pushes
                (superchat-plan-report-progress '((1 "done")))
                (setq sent nil)
                (superchat-plan--nudge-on-completion
                 '(:status completed))
                (should sent)
                (should (= 2 superchat-plan--nudges))
                (setq sent nil)
                (superchat-plan--nudge-on-completion
                 '(:status completed))
                (should-not sent)
                (should-not superchat-plan--nudge-pending))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/nudge-stops-when-all-done ()
  "A completed execution turn with every step done sends no nudge."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-nudge-done-test*"))
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
            (superchat-plan-report-progress '((1 "done")))
            (setq sent nil)
            (with-current-buffer superchat-buffer-name
              (superchat-plan--nudge-on-completion
               '(:status completed)))
            (should-not sent)
            (with-current-buffer superchat-buffer-name
              (should-not superchat-plan--nudge-pending)))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/progress-updates-view-buffer ()
  "Reporting progress re-renders the live TextUI view buffer content."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (or (featurep 'textui) (require 'textui nil t))))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-view-live-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((view (get-buffer superchat-plan--view-buffer)))
              (should view)
              ;; textui-update defers the re-render; force the
              ;; synchronous rebuild for the batch check
              (textui-refresh view)
              (should
               (with-current-buffer view
                 (string-match-p "0/1 steps done" (buffer-string))))
              (superchat-plan-report-progress '((1 "done")))
              (textui-refresh view)
              (should
               (with-current-buffer view
                 (string-match-p "1/1 steps done" (buffer-string))))
              (should
               (with-current-buffer view
                 (string-match-p "✓" (buffer-string))))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer superchat-plan--view-buffer)
          (kill-buffer superchat-plan--view-buffer))))))

;;; test-plan.el ends here
