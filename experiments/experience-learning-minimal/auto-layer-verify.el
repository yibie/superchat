;;; auto-layer-verify.el --- Real-data verification of the auto annotation path -*- lexical-binding: t; -*-
;;; Commentary:
;; Verifies the opt-in structural annotation (level 1) against real
;; experiment databases.  Original data is never modified: each source
;; database is copied to a temp directory first.
;;
;; Phase A (unmodified copies): the thin-history guard must stay silent
;; on real corpora that lack one class (self-P: all recovered;
;; five-run-corrected: all failed).
;;
;; Phase B (augmented self-P copy): three synthetic failed Runs are
;; appended through the real Tape API, then every Run gets a verdict.
;; Expected: the 8 real recovered Runs are predicted recovered; the
;; synthetic broken-pattern Run (read fails, then workspace_info fails)
;; is predicted recovered and counts toward the watchlist; all verdict
;; events carry actor experience-auto.
;;
;; The synthetic augmentation is labeled as such in the verdict; level-2
;; candidate selection stays fixture-verified (no real corpus has two
;; promoted candidates for one context).
;;; Code:

(require 'cl-lib)

(defvar superchat-data-directory)
(defvar superchat-experience-auto-annotation-enabled)
(defvar superchat-experience-auto-watchlist-threshold)
(defvar superchat-experience--watchlist-count)
(defvar superchat-experience--record-structure-verdict)
(defvar superchat-view-session-ids)
(defvar superchat-experience--view-events)
(defvar superchat-experience--run-skeletons)

(defun superchat-auto-verify--exp-dir (name)
  "Expand experiment DB directory NAME under experiments/."
  (expand-file-name
   (format "../experience-learning-minimal/%s"
           name)
   (file-name-directory (or load-file-name buffer-file-name))))

(defun superchat-auto-verify--copy (source label workdir)
  "Copy SOURCE to a fresh directory under WORKDIR; return its path."
  (let ((dest (expand-file-name label workdir)))
    (make-directory dest t)
    (dolist (entry (directory-files source t nil t))
      (unless (member (file-name-nondirectory entry) '("." ".."))
        (let ((target (expand-file-name
                       (file-name-nondirectory entry) dest)))
          (if (file-directory-p entry)
              (copy-directory entry target t t t)
            (copy-file entry target t t t t)))))
    dest))

(defun superchat-auto-verify--with-db (dir body)
  "Run BODY with DIR as the active database, closing afterwards."
  (let ((superchat-data-directory dir))
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-tape)
    (require 'superchat-tape-view)
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (funcall body)
      (superchat-db-close))))

(defun superchat-auto-verify--skeletons-of (dir)
  "Inventory skeletons in DIR."
  (superchat-auto-verify--with-db
   dir
   (lambda ()
     (let ((skeletons (superchat-experience--run-skeletons)))
       (list :count (length skeletons)
             :recovered (length (cl-remove-if-not (lambda (s) (nth 2 s))
                                                  skeletons))
             :failed (length (cl-remove-if (lambda (s) (nth 2 s))
                                           skeletons)))))))

(defun superchat-auto-verify--session-of-run (run-id dir)
  "Session id owning RUN-ID in DIR."
  (superchat-auto-verify--with-db
   dir
   (lambda ()
     (let* ((sessions (superchat-view-session-ids))
            (events (superchat-experience--view-events sessions)))
       (plist-get
        (cl-find-if (lambda (e) (equal run-id (plist-get e :run-id)))
                    events)
        :session-id)))))

(defun superchat-auto-verify--append-failed (dir stem tools)
  "Append one failed Run named STEM with TOOLS (tool ok|failed list)."
  (superchat-auto-verify--with-db
   dir
   (lambda ()
     (let ((i 0))
       (dolist (step tools)
         (setq i (1+ i))
         (superchat-tape-append-event-record
          (list :type (if (eq (cadr step) 'ok)
                          "tool.completed" "tool.failed")
                :schema 1
                :session-id "auto-verify-session"
                :turn-id (format "turn-%s" stem)
                :run-id (format "run-%s" stem)
                :actor-id "lead"
                :payload (list :tool-name (car step)))))))))

(defun superchat-auto-verify--phase-a (workdir)
  "Thin-history silence on unmodified real corpora."
  (let ((rows nil))
    (dolist (source
             (list (superchat-auto-verify--exp-dir
                    "self-learning-stability-1/self-P")
                   (superchat-auto-verify--exp-dir
                    "self-learning-stability-2/self-P")
                   (superchat-auto-verify--exp-dir
                    "self-learning-stability-3/self-P")
                   (superchat-auto-verify--exp-dir
                    "review-real-1/review-P")
                   (superchat-auto-verify--exp-dir
                    "five-run-20260812-corrected")))
      (when (file-directory-p source)
        (let* ((label (file-name-nondirectory
                       (directory-file-name source)))
               (copy (superchat-auto-verify--copy
                      source (format "a-%s" label) workdir))
               (inventory (superchat-auto-verify--skeletons-of copy))
               (verdicts 0))
          (superchat-auto-verify--with-db
           copy
           (lambda ()
             (dolist (skeleton (superchat-experience--run-skeletons))
               (let ((session
                      (superchat-auto-verify--session-of-run
                       (car skeleton) copy)))
                 (when session
                   (let ((verdict
                          (superchat-experience--record-structure-verdict
                           (car skeleton)
                           (list :session-id session
                                 :run-id (car skeleton)))))
                     (when verdict (setq verdicts (1+ verdicts)))))))))
          (push (list :source label :inventory inventory
                      :verdicts verdicts)
                rows))))
    (nreverse rows)))

(defun superchat-auto-verify--phase-b (workdir)
  "Augmented self-P: verdicts for every Run, watchlist for broken pattern."
  (let ((source (superchat-auto-verify--exp-dir
                 "self-learning-stability-1/self-P"))
        (copy nil))
    (unless (file-directory-p source)
      (error "self-P source missing"))
    (setq copy (superchat-auto-verify--copy
                source "b-self-P-augmented" workdir))
    (superchat-auto-verify--append-failed
     copy "syn-retry2"
     '(("workspace_read" failed) ("workspace_read" failed)))
    (superchat-auto-verify--append-failed
     copy "syn-retry3"
     '(("workspace_read" failed) ("workspace_read" failed)
       ("workspace_read" failed)))
    (superchat-auto-verify--append-failed
     copy "syn-broken"
     '(("workspace_read" failed) ("workspace_info" failed)))
    (superchat-auto-verify--with-db
     copy
     (lambda ()
       (let* ((superchat-experience-auto-watchlist-threshold 3)
              (superchat-experience--watchlist-count 0)
              (skeletons (superchat-experience--run-skeletons))
              (verdicts nil)
              (rows nil))
         (dolist (skeleton skeletons)
           (let ((session
                  (superchat-auto-verify--session-of-run
                   (car skeleton) copy)))
             (when session
               (let ((verdict
                      (superchat-experience--record-structure-verdict
                       (car skeleton)
                       (list :session-id session
                             :run-id (car skeleton)))))
                 (when verdict
                   (push (list :run-id (car skeleton)
                               :recovered-p (plist-get verdict :recovered-p)
                               :predicted (plist-get verdict :predicted)
                               :margin (plist-get verdict :margin))
                         rows))))))
         (let* ((sessions (superchat-view-session-ids))
                (events (superchat-experience--view-events sessions))
                (structure-events
                 (cl-remove-if-not
                  (lambda (e)
                    (equal "experience.structure.recorded"
                           (plist-get e :type)))
                  events)))
           (list :copy copy
                 :skeleton-count (length skeletons)
                 :rows (nreverse rows)
                 :structure-events (length structure-events)
                 :auto-actor
                 (cl-every (lambda (e)
                             (equal "experience-auto"
                                    (plist-get e :actor-id)))
                           structure-events)
                 :watchlist-count superchat-experience--watchlist-count)))))))

(defun superchat-auto-verify--run ()
  "Run both phases and write the verdict."
  (let* ((output (getenv "SUPERCHAT_AUTO_VERIFY_OUTPUT"))
         (workdir (getenv "SUPERCHAT_AUTO_VERIFY_WORKDIR"))
         phase-a phase-b)
    (unless (and output workdir)
      (error "SUPERCHAT_AUTO_VERIFY_OUTPUT and WORKDIR are required"))
    (make-directory workdir t)
    (setq phase-a (superchat-auto-verify--phase-a workdir)
          phase-b (superchat-auto-verify--phase-b workdir))
    (let ((verdict
           (list :format "superchat-experience-auto-layer-verify-v1"
                 :claim
                 "real-data verification of the structural annotation path"
                 :source
                 (list :emacs emacs-version
                       :compressor (executable-find "gzip"))
                 :phase-a phase-a
                 :phase-b phase-b
                 :notes
                 (list
                  :original-data-untouched
                  "every source DB was copied before appending"
                  :synthetic-runs
                  "three failed Runs appended through the real Tape API"
                  :level-2
                  "candidate selection remains fixture-verified: no real corpus holds two promoted candidates for one context"
                  :default-off
                  "all auto switches remain disabled in the product defaults"))))
      (with-temp-buffer
        (prin1 verdict (current-buffer))
        (write-region (point-min) (point-max) output nil 'quiet))
      (princ (format "auto-layer verify verdict: %s\n" output))
      (princ (format "phase-b: %d rows, %d structure events, watchlist %d\n"
                     (length (plist-get phase-b :rows))
                     (plist-get phase-b :structure-events)
                     (plist-get phase-b :watchlist-count))))))

(when (equal (getenv "SUPERCHAT_AUTO_VERIFY_EXECUTE") "1")
  (condition-case err
      (progn (superchat-auto-verify--run) (kill-emacs 0))
    (error
     (princ (format "auto-layer verify infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-auto-layer-verify)

;;; auto-layer-verify.el ends here
