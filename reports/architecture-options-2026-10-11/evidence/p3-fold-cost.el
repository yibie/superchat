;;; p3-fold-cost.el --- what reading "the current state" off the Tape costs -*- lexical-binding: t; -*-

;; Research probe, read-only, against the snapshot of 75e83370 with an
;; isolated database.  Several user-visible answers are computed by replaying
;; or scanning the Tape on every ask: whether a Run was started by Superchat
;; (`superchat-changes--unattended-run-p', a full replay of the Session), the
;; [Work records] block every chat request carries
;; (`superchat-work-request-text', a LIKE scan of the whole tape table), the
;; timer list, the unsaved-changes candidates and the history a reopened View
;; rematerializes.  This probe measures those reads as the Session grows.
;;
;; N rows are appended as plausible run/tool/response rows (five per Run) on
;; top of three real scripted turns, one Work Record and one timer.  Each
;; measurement is the median of five `benchmark-run' calls, in milliseconds.
;; Numbers describe this machine and this SQLite; they are not a target.

(load (expand-file-name "scripts/work-record-regression.el" default-directory) nil t)
(mapc (lambda (test) (ert-delete-test (ert-test-name test))) (ert-select-tests t t))
(require 'superchat-timer)
(require 'superchat-changes)

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

(defun p3--bulk-append (session n)
  "Append about N plausible rows to SESSION; return seconds spent appending."
  (let ((start (float-time)) (i 0))
    (while (< i n)
      (let ((run (format "p3-run-%06d" i)) (turn (format "p3-turn-%06d" i)))
        (superchat-tape-append-event
         (list :type "run.started" :session-id session :run-id run :turn-id turn
               :actor-id "runtime" :summary "run started" :payload (list :capability "chat")))
        (superchat-tape-append-event
         (list :type "tool.completed" :session-id session :run-id run :turn-id turn
               :actor-id "tools" :summary "edit_buffer completed"
               :payload (list :tool-name "edit_buffer" :buffer "a.txt"
                              :buffer-file "/tmp/p3/a.txt" :result "Edited a.txt")))
        (superchat-tape-append-event
         (list :type "response.completed" :session-id session :run-id run :turn-id turn
               :actor-id "model" :summary "answer" :payload (list :text "Done; a.txt edited.")))
        (superchat-tape-append-event
         (list :type "run.completed" :session-id session :run-id run :turn-id turn
               :actor-id "runtime" :summary "run completed" :payload nil))
        (setq i (+ i 4))))
    (- (float-time) start)))

(defun p3--measure (label session run)
  (let* ((rows (length (superchat-tape-replay-events session)))
         (row (list :label label :rows rows
                    :replay-ms (p3--ms (lambda () (superchat-tape-replay-events session)))
                    :unattended-ms (p3--ms (lambda () (superchat-changes--unattended-run-p session run)))
                    :work-request-text-ms (p3--ms (lambda () (superchat-work-request-text session)))
                    :work-records-ms (p3--ms (lambda () (superchat-work-records)))
                    :timer-list-ms (p3--ms (lambda () (superchat-timer-list)))
                    :changes-candidates-ms (p3--ms (lambda () (superchat-changes-candidates session)))
                    :session-history-ms (p3--ms (lambda () (superchat-view-session-history session))))))
    (push row p3--results)
    (princ (format "%s | rows=%d | replay %.1f | unattended-run-p %.1f | work-request-text %.1f | work-records %.1f | timer-list %.1f | changes-candidates %.1f | session-history %.1f\n"
                   label rows (plist-get row :replay-ms) (plist-get row :unattended-ms)
                   (plist-get row :work-request-text-ms) (plist-get row :work-records-ms)
                   (plist-get row :timer-list-ms) (plist-get row :changes-candidates-ms)
                   (plist-get row :session-history-ms)))))

(ert-deftest p3/fold-cost-as-the-tape-grows ()
  (wrec--with '((:text "one") (:text "two") (:text "three")) (p)
    (let* ((view (wrec--view p))
           (session (hm--session view))
           (id (superchat-work-start session "P3 record")))
      (superchat-work-update session id "half" nil "resume here")
      (superchat-timer-create session 'reminder "P3 reminder" (+ (float-time) 3600))
      (hm--turn view "first")
      (hm--turn view "second")
      (let ((run (hm--turn view "third")))
        (princ "P3 FOLD COST (ms, median of 5; snapshot 75e83370; isolated SQLite)\n")
        (p3--measure "baseline (3 real turns)" session run)
        (dolist (target '(1000 5000 20000))
          (let* ((have (length (superchat-tape-replay-events session)))
                 (secs (p3--bulk-append session (- target have))))
            (princ (format "appended to %d rows in %.2f s\n" target secs))
            (p3--measure (format "~%d rows" target) session run)))
        (with-temp-file (expand-file-name "p3-fold-cost.json" p3--out)
          (insert (json-encode (reverse p3--results))))))))

(ert-run-tests-batch-and-exit "^p3/")
