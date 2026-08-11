;;; paired-repeat.el --- Repeated matched control/treatment pairs, recovery domain -*- lexical-binding: t; -*-
;;; Commentary:
;; Method 1 from the five-method design, repeated: run N independent
;; control/treatment pairs with randomized arm order, then report the
;; first-recovery-action rate difference instead of one lucky pair.
;; Each arm retains its full trace.  This is a diagnostic cohort, not a
;; frozen acceptance contract.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "recovery-five-methods.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defvar superchat-data-directory)
(defvar superchat-workspace--registry)
(defvar superchat-workspace--artifact-counter)
(defvar superchat-experience-workspace-revision-recovery-scope)
(defvar superchat-recovery-five--cell)
(defvar superchat-recovery-five--model)
(defvar superchat-acceptance-v26--repo-root)

(defconst superchat-recovery-paired--pairs 4)
(defconst superchat-recovery-paired--target "workspace_info")

(defun superchat-recovery-paired--jsd (p q)
  "Return the Jensen-Shannon divergence between two distributions P and Q.
P and Q are alists mapping a key to its probability; keys missing from one
side count as zero.  Returns a float in [0, ln 2]."
  (let* ((keys (delete-dups
                (append (mapcar #'car p) (mapcar #'car q))))
         (lookup (lambda (dist key)
                   (or (cdr (assoc key dist)) 0.0)))
         (sum 0.0))
    (dolist (key keys)
      (let* ((pv (float (funcall lookup p key)))
             (qv (float (funcall lookup q key)))
             (m (/ (+ pv qv) 2.0)))
        (when (> m 0.0)
          (cl-incf sum (+ (if (> pv 0.0)
                              (* pv (log (/ pv m)))
                            0.0)
                          (if (> qv 0.0)
                              (* qv (log (/ qv m)))
                            0.0))))))
    (/ sum 2.0)))

(defun superchat-recovery-paired--distribution (arms)
  "Return the action distribution alist across ARMS."
  (let ((counts nil))
    (dolist (arm arms)
      (let* ((action (plist-get arm :first-recovery-action))
             (key (or action 'no-action))
             (cell (assoc key counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (push (cons key 1) counts))))
    (let ((total (length arms)))
      (mapcar (lambda (cell)
                (cons (car cell) (/ (float (cdr cell)) total)))
              (nreverse counts)))))

(defun superchat-recovery-paired--run ()
  "Run N repeated control/treatment pairs and report action rates."
  (let* ((output (getenv "SUPERCHAT_RECOVERY_PAIRED_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (directory (and root (expand-file-name "paired-P" root)))
         setup controls treatments results)
    (unless (and output root)
      (error "SUPERCHAT_RECOVERY_PAIRED_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite paired trace: %s" output))
    (make-directory directory t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'superchat-workspace)
    (require 'llm)
    (if superchat-recovery-five--base-url
        (require 'llm-openai)
      (require 'llm-ollama))
    ;; Seal one setup database before the first provider request, with the
    ;; stale mutation inside the producer Run (durable proof preserved).
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (stale-p (eq (plist-get superchat-recovery-five--cell :kind) 'stale))
          (original-write (symbol-function 'superchat-workspace-write)))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-workspace-add-region)
                     (lambda (_beg _end &optional role workspace-id)
                       (let ((meta
                              (superchat-workspace-publish-mutable
                               "sealed producer artifact\n" role workspace-id)))
                         (when stale-p
                           (funcall original-write
                                    "changed after recorded revision" nil
                                    (plist-get meta :artifact-id)
                                    (plist-get meta :revision) workspace-id))
                         meta)))
                    ((symbol-function 'superchat-workspace-write)
                     (lambda (content &optional append artifact-id
                                      expected-revision workspace-id)
                       (unless stale-p
                         (funcall original-write content append artifact-id
                                  expected-revision workspace-id)))))
            (setq setup
                  (superchat-acceptance-v26--setup-experience
                   superchat-recovery-five--cell)))
        (superchat-db-close)))
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (sessions (append (plist-get setup :sessions) nil)))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (dotimes (pair superchat-recovery-paired--pairs)
            ;; Fixed randomization schedule: even pairs run treatment first,
            ;; odd pairs run control first.
            (let ((treatment-first (cl-evenp pair))
                  (control-arm (format "pair-%d-control" pair))
                  (treatment-arm (format "pair-%d-treatment" pair)))
              (if treatment-first
                  (progn
                    (push (superchat-recovery-five--run-arm
                           directory treatment-arm setup sessions nil)
                          results)
                    (push (superchat-recovery-five--run-arm
                           directory control-arm setup nil nil)
                          results))
                (progn
                  (push (superchat-recovery-five--run-arm
                         directory control-arm setup nil nil)
                        results)
                  (push (superchat-recovery-five--run-arm
                         directory treatment-arm setup sessions nil)
                        results)))))
        (superchat-db-close)))
    (setq results (nreverse results))
    (setq controls
          (cl-remove-if-not
           (lambda (arm) (string-suffix-p "control" (plist-get arm :arm)))
           results))
    (setq treatments
          (cl-remove-if-not
           (lambda (arm) (string-suffix-p "treatment" (plist-get arm :arm)))
           results))
    (let* ((target superchat-recovery-paired--target)
           (control-rate
            (/ (float (cl-count target controls
                               :test #'equal
                               :key (lambda (arm)
                                      (plist-get arm :first-recovery-action))))
               (max 1 (length controls))))
           (treatment-rate
            (/ (float (cl-count target treatments
                               :test #'equal
                               :key (lambda (arm)
                                      (plist-get arm :first-recovery-action))))
               (max 1 (length treatments))))
           (control-dist (superchat-recovery-paired--distribution controls))
           (treatment-dist
            (superchat-recovery-paired--distribution treatments))
           (jsd (superchat-recovery-paired--jsd control-dist treatment-dist))
           (contrast
            (cond
             ((and (> treatment-rate control-rate)
                   (cl-every (lambda (arm)
                               (plist-get arm :intervention-recorded))
                             treatments))
              'treatment-first-action-rate-higher)
             ((and (> control-rate treatment-rate)) 'control-rate-higher)
             (t 'no-identifiable-contrast)))
           (trace
            (list :format "superchat-experience-recovery-paired-v1"
                  :claim "repeated matched pairs, first-recovery-action rate"
                  :source
                  (list :emacs emacs-version
                        :model superchat-recovery-five--model
                        :jj (superchat-acceptance-v26--jj-identity
                             superchat-acceptance-v26--repo-root)
                        :dependencies
                        (superchat-recovery-trace--dependency-records))
                  :setup
                  (list :cell superchat-recovery-five--cell
                        :workspace-id (plist-get setup :workspace-id)
                        :artifact-id (plist-get setup :artifact-id)
                        :old-revision (plist-get setup :old-revision))
                  :pairs superchat-recovery-paired--pairs
                  :target-action target
                  :control-rate control-rate
                  :treatment-rate treatment-rate
                  :rate-difference (- treatment-rate control-rate)
                  :control-distribution control-dist
                  :treatment-distribution treatment-dist
                  :jsd jsd
                  :contrast contrast
                  :arms results
                  :provider-requests
                  (apply #'+ (mapcar (lambda (arm)
                                      (length (plist-get arm
                                                         :transport-requests)))
                                    results))
                  :notes
                  '(:no-retry t
                    :randomization "Fixed pair-order schedule; even pairs treatment-first."
                    :interpretation "Rate difference with JSD; still not induction-from-scratch proof."
                    :default-remains-off t))))
      (superchat-recovery-trace--write output trace)
      (princ
       (format
        (concat "paired trace: %s\n"
                "pairs=%d control-rate=%.2f treatment-rate=%.2f diff=%.2f jsd=%.4f\n"
                "contrast=%S\n")
        output superchat-recovery-paired--pairs
        control-rate treatment-rate
        (- treatment-rate control-rate) jsd contrast)))))

(when (equal (getenv "SUPERCHAT_RECOVERY_PAIRED_EXECUTE") "1")
  (condition-case err
      (progn (superchat-recovery-paired--run) (kill-emacs 0))
    (error
     (princ (format "paired infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-recovery-paired)

;;; paired-repeat.el ends here
