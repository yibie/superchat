;;; p3-fold-cost.el --- what reading "the current state" off the Tape costs -*- lexical-binding: t; -*-

;; A copy of reports/architecture-options-2026-10-11/evidence/p3-fold-cost.el
;; (snapshot 75e83370), re-run against the live tree after 437ca4e8 with an
;; isolated database.  The original seven columns are kept.  Four are added:
;; the pending list's items and its rendering (`superchat-pending-items',
;; `superchat-pending' with `tabulated-list-print'), the settle scan the
;; Run-end observer now makes first (`superchat-work-settle-missing' for the
;; conversation), and one whole scripted turn's wall time at each size.
;;
;; N rows are appended as plausible run/tool/response/end/outcome rows (five
;; per Run, the fifth being the `work.outcome' the observer writes for the
;; conversation's current record) on top of three real scripted turns, one
;; Work Record and one timer.  Each measurement is the median of five
;; `benchmark-run' calls, in milliseconds; the turn is one sample.  Numbers
;; describe this machine and this SQLite; they are not a target.

(load (expand-file-name "scripts/work-record-regression.el" default-directory) nil t)
(mapc (lambda (test) (ert-delete-test (ert-test-name test))) (ert-select-tests t t))
(require 'superchat-timer)
(require 'superchat-changes)
(require 'superchat-pending)

(defconst p3--out (file-name-directory (or load-file-name buffer-file-name)))
(defvar p3--results nil)

(defun p3--median (xs)
  (let* ((s (sort (copy-sequence xs) #'<)) (n (length s)))
    (if (cl-oddp n) (nth (/ n 2) s) (/ (+ (nth (1- (/ n 2)) s) (nth (/ n 2) s)) 2.0))))

(defun p3--ms (thunk &optional reps)
  "Median wall time of THUNK over REPS runs, in milliseconds."
  (let (ts)
    (dotimes (_ (or reps 5))
      (push (car (benchmark-run 1 (funcall thunk))) ts))
    (* 1000 (p3--median ts))))

(defun p3--bulk-append (session id n)
  "Append about N plausible rows to SESSION under record ID; return seconds spent."
  (let ((start (float-time)) (i 0)
        (file (expand-file-name "p3/a.txt" "~/.cache/sc-fable-review/")))
    (while (< i n)
      (let ((run (format "p3-run-%06d" i)) (turn (format "p3-turn-%06d" i)))
        (superchat-tape-append-event
         (list :type "run.started" :session-id session :run-id run :turn-id turn
               :actor-id "runtime" :summary "run started" :payload (list :capability "chat")))
        (superchat-tape-append-event
         (list :type "tool.completed" :session-id session :run-id run :turn-id turn
               :actor-id "tools" :summary "edit_buffer completed"
               :payload (list :tool-name "edit_buffer" :buffer "a.txt"
                              :buffer-file file :result "Edited a.txt")))
        (superchat-tape-append-event
         (list :type "response.completed" :session-id session :run-id run :turn-id turn
               :actor-id "model" :summary "answer" :payload (list :text "Done; a.txt edited.")))
        (superchat-tape-append-event
         (list :type "run.completed" :session-id session :run-id run :turn-id turn
               :actor-id "runtime" :summary "run completed" :payload nil))
        (superchat-work--append session "work.outcome"
                                (format "Work record %s: continuable" id)
                                (list :work-id id :run-id run :outcome "continuable"))
        (setq i (+ i 5))))
    (- (float-time) start)))

(cl-defun p3--turn (view text)
  "Like `hm--turn', waiting up to ten minutes: a turn's end at 20,000 rows
takes minutes on today's code (the settle scan), which `hm--turn''s 20 s
would call a failure."
  (let ((run (condition-case err
                 (smoke--send view text)
               ;; At 5,000 synthetic rows the send itself signals inside
               ;; prompt assembly (superchat-prompt-invalid :refit-result,
               ;; the history fragment's refit text is longer than what is
               ;; left of the budget): no Run starts.  Recorded, not fatal.
               (error (princ (format "turn %s: the send signalled before any Run started: %s\n"
                                     text (substring (format "%S" err) 0 160)))
                      nil))))
    (unless run (cl-return-from p3--turn nil))
    (should (stringp run))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 600))
    ;; On the first run at 5,000 rows the turn's Run failed in prompt
    ;; assembly (superchat-prompt-invalid :field :refit-result, the history
    ;; fragment), before any model call: recorded, not asserted away.
    (let ((status (superchat-runtime-status run)))
      (unless (eq 'completed (plist-get status :status))
        (princ (format "turn %s ended %s: %s\n" text (plist-get status :status)
                       (let ((e (format "%s" (plist-get status :error))))
                         (substring e 0 (min 200 (length e))))))))
    run))

(defun p3--pending-render-ms ()
  (p3--ms (lambda ()
            (save-window-excursion
              (kill-buffer (superchat-pending))))))

(defun p3--measure (label session run view)
  (let* ((rows (length (superchat-tape-replay-events session)))
         (row (list :label label :rows rows
                    :replay-ms (p3--ms (lambda () (superchat-tape-replay-events session)))
                    :unattended-ms (p3--ms (lambda () (superchat-changes--unattended-run-p session run)))
                    :work-request-text-ms (p3--ms (lambda () (superchat-work-request-text session)))
                    :work-records-ms (p3--ms (lambda () (superchat-work-records)))
                    :timer-list-ms (p3--ms (lambda () (superchat-timer-list)))
                    :changes-candidates-ms (p3--ms (lambda () (superchat-changes-candidates session)))
                    :session-history-ms (p3--ms (lambda () (superchat-view-session-history session)))
                    :pending-items-ms (p3--ms (lambda () (superchat-pending-items)))
                    :pending-render-ms (p3--pending-render-ms)
                    ;; One sample: at 20,000 rows one scan takes minutes.
                    :settle-missing-ms (p3--ms (lambda () (superchat-work-settle-missing session)) 1)))
         (t0 (float-time)))
    (p3--turn view (format "probe turn %s" label))
    (setq row (plist-put row :turn-ms (* 1000 (- (float-time) t0))))
    (push row p3--results)
    (princ (format "%s | rows=%d | replay %.1f | unattended-run-p %.1f | work-request-text %.1f | work-records %.1f | timer-list %.1f | changes-candidates %.1f | session-history %.1f | pending-items %.1f | pending-render %.1f | settle-missing %.1f | one turn %.0f\n"
                   label rows (plist-get row :replay-ms) (plist-get row :unattended-ms)
                   (plist-get row :work-request-text-ms) (plist-get row :work-records-ms)
                   (plist-get row :timer-list-ms) (plist-get row :changes-candidates-ms)
                   (plist-get row :session-history-ms) (plist-get row :pending-items-ms)
                   (plist-get row :pending-render-ms) (plist-get row :settle-missing-ms)
                   (plist-get row :turn-ms)))))

(ert-deftest p3/fold-cost-as-the-tape-grows ()
  (wrec--with '((:text "one") (:text "two") (:text "three")
                (:text "p0") (:text "p1") (:text "p2") (:text "p3"))
      (p)
    (let* ((view (wrec--view p))
           (session (hm--session view))
           (id (superchat-work-start session "P3 record")))
      (superchat-work-update session id "half" nil "resume here")
      (superchat-timer-create session 'reminder "P3 reminder" (+ (float-time) 3600))
      (hm--turn view "first")
      (hm--turn view "second")
      (let ((run (hm--turn view "third")))
        (princ "P3 FOLD COST (ms, median of 5; live tree after 437ca4e8; isolated SQLite; the turn is one sample)\n")
        (p3--measure "baseline (3 real turns)" session run view)
        (dolist (target '(1000 5000 20000))
          (let* ((have (length (superchat-tape-replay-events session)))
                 (secs (p3--bulk-append session id (- target have))))
            (princ (format "appended to %d rows in %.2f s\n" target secs))
            (p3--measure (format "~%d rows" target) session run view)))
        (unwind-protect
            (with-temp-file (expand-file-name "p3-fold-cost.json" p3--out)
              (insert (json-encode (reverse p3--results))))
          (superchat-timer-disarm-all))))))

(ert-run-tests-batch-and-exit "^p3/")
