;;; compression-probe.el --- gzip-lm style structure probe over real recovery episodes -*- lexical-binding: t; -*-
;;; Commentary:
;; Nathan (https://nathan.rs/posts/gzip-lm/) reduces language modeling to the
;; compression-prediction equivalence: score(candidate) = len(gzip(corpus +
;; candidate)); text that shares structure with the corpus compresses small.
;; This probe asks the user's question against real traces: does correctness
;; have information structure?  Can compression separate recovered from
;; unrecovered recovery episodes without being told the rule?
;;
;; Data: every arm with :tape-events across experiments/*/trace.el files.
;; Episode serialization is behavioral only: the tool-action sequence with
;; per-step OK/FAILED.  It deliberately excludes the injected experience text,
;; hand-written scope strings, artifact/revision/session ids, and (in the
;; primary probe) the task text, so compression cannot win by rediscovering
;; harness artifacts.
;;
;; Labels are mechanical, derived from tape events alone:
;;   recovered-p = a successful workspace_read after the first failed read,
;;                 or a successful read with no prior failure.
;;; Code:

(require 'cl-lib)

(defvar superchat-compression-probe--gzip
  (or (executable-find "gzip") (executable-find "pigz")))

(defun superchat-compression-probe--gzip-size (text)
  "Return DEFLATE compressed byte size of TEXT via the system gzip."
  (unless superchat-compression-probe--gzip
    (error "gzip binary not found"))
  (let ((in (make-temp-file "scp-" nil ".txt")))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (write-region (point-min) (point-max) in nil 'quiet)
          (with-temp-buffer
            (call-process superchat-compression-probe--gzip nil t nil "-c" in)
            (buffer-size)))
      (delete-file in))))

(defun superchat-compression-probe--normalize (text)
  "Strip harness ids from TEXT, keep domain words."
  (let ((text (replace-regexp-in-string
               "artifact-[0-9a-f]\\{8\\}-[0-9]" "artifact-ID" text))
        (text (replace-regexp-in-string
               "[0-9a-f]\\{64\\}" "REV" text))
        (text (replace-regexp-in-string
               "\\(?:evt\\|run\\|tool\\|session\\)-[0-9A-Za-z.-]+"
               "ID" text))
        (text (replace-regexp-in-string "0efc79ae[0-9a-f]*" "REV" text)))
    (replace-regexp-in-string "[ \t\n]+" " " (string-trim text))))

(defun superchat-compression-probe--episode (arm)
  "Extract a recovery episode from ARM, or nil.
An episode is the behavioral skeleton: the ordered tool-step outcomes."
  (let ((events (plist-get arm :tape-events)))
    (when events
      (let ((steps nil)
            (failed-seen nil)
            (read-ok nil)
            (read-ok-after-failure nil))
        (dolist (event events)
          (let* ((type (plist-get event :type))
                 (payload (plist-get event :payload)))
            (when (member type '("tool.completed" "tool.failed"))
              (let ((tool-name (plist-get payload :tool-name))
                    (status (if (equal type "tool.completed") 'ok 'failed)))
                (when tool-name
                  (push (list tool-name status) steps))
                (when (and (equal tool-name "workspace_read")
                           (eq status 'ok))
                  (setq read-ok t)
                  (when failed-seen (setq read-ok-after-failure t)))
                (when (eq status 'failed) (setq failed-seen t))))))
        (setq steps (nreverse steps))
        (when steps
          (list :arm (plist-get arm :arm)
                :steps steps
                :task (plist-get arm :task)
                :trace-file (plist-get arm :trace-file)
                :recovered-p (or read-ok-after-failure
                                 (and (not failed-seen) read-ok))))))))

(defun superchat-compression-probe--episode-text
    (episode &optional prefix-only include-task task)
  "Serialize EPISODE to the canonical behavioral text.
PREFIX-ONLY cuts the sequence at the first action after the first failed
read (the decision point), for the predictive probe.  INCLUDE-TASK prepends
the normalized TASK text, for the confound probe."
  (let* ((steps (plist-get episode :steps))
         (first-fail
          (cl-position 'failed steps :test
                       (lambda (status step) (eq status (cadr step)))))
         (cut (when prefix-only
                (if first-fail (min (+ first-fail 2) (length steps))
                  (length steps))))
         (lines nil))
    (when include-task
      (push (format "TASK %s" (superchat-compression-probe--normalize
                               (or task "")))
            lines))
    (cl-loop for step in (if cut (cl-subseq steps 0 cut) steps)
             do (push (format "ACTION %s %s"
                              (car step)
                              (if (eq (cadr step) 'ok) "OK" "FAILED"))
                      lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun superchat-compression-probe--corpus-score (text corpus)
  "Incremental compressed cost of TEXT given CORPUS.
score = len(gzip(corpus + text)) - len(gzip(corpus))."
  (let ((corpus-size (superchat-compression-probe--gzip-size corpus)))
    (- (superchat-compression-probe--gzip-size (concat corpus "\n" text))
       corpus-size)))

(defun superchat-compression-probe--classify (episode episodes
                                              &optional prefix-only
                                              include-task)
  "Leave-one-out compression classification of EPISODE against EPISODES.
Returns (predicted actual margin success-corpus-size failure-corpus-size)."
  (let* ((label (if (plist-get episode :recovered-p) 'recovered 'failed))
         (others (cl-remove episode episodes :test #'equal))
         (success-corpus
          (mapconcat
           (lambda (e)
             (superchat-compression-probe--episode-text
              e prefix-only include-task (plist-get e :task)))
           (cl-remove-if-not (lambda (e) (plist-get e :recovered-p)) others)
           "\n"))
         (failure-corpus
          (mapconcat
           (lambda (e)
             (superchat-compression-probe--episode-text
              e prefix-only include-task (plist-get e :task)))
           (cl-remove-if (lambda (e) (plist-get e :recovered-p)) others)
           "\n"))
         (text (superchat-compression-probe--episode-text
                episode prefix-only include-task (plist-get episode :task)))
         (success-score (superchat-compression-probe--corpus-score
                         text success-corpus))
         (failure-score (superchat-compression-probe--corpus-score
                         text failure-corpus)))
    (list (if (< success-score failure-score) 'recovered 'failed)
          label (- failure-score success-score)
          success-score failure-score)))

(defun superchat-compression-probe--load-traces ()
  "Load every trace.el under experiments/ and return its arms."
  (let ((files
         (cl-remove-if-not
          (lambda (f) (file-exists-p (expand-file-name "trace.el" f)))
          (append
           (directory-files
            (expand-file-name "../experience-learning-minimal"
                              (file-name-directory
                               (or load-file-name buffer-file-name)))
            t "self-learning\\|review-real\\|five-run\\|paired-repeat\\|recovery-five\\|closed-loop\\|run-2026")
           (directory-files
            (expand-file-name "../../experiments/experience-view-recovery-trace"
                              (file-name-directory
                               (or load-file-name buffer-file-name)))
            t nil)
           (directory-files
            (expand-file-name "../../experiments/experience-learning-first-action-20260812-final3"
                              (file-name-directory
                               (or load-file-name buffer-file-name)))
            t nil)
           (directory-files
            (expand-file-name "../../experiments/experience-learning-first-action-20260812-final4"
                              (file-name-directory
                               (or load-file-name buffer-file-name)))
            t nil))))
        (traces nil))
    (dolist (dir (cl-remove-duplicates files :test #'equal))
      (let ((file (expand-file-name "trace.el" dir)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (let ((form (read (current-buffer))))
                (when (and (proper-list-p form) (plist-get form :arms))
                  (let ((statement (plist-get form :candidate-statement)))
                    (dolist (arm (plist-get form :arms))
                      (when (plist-get arm :tape-events)
                        (push (plist-put (plist-put (copy-sequence arm)
                                                   :trace-file file)
                                         :candidate-statement statement)
                              traces))))))
            (error nil)))))
    (nreverse traces)))

(defun superchat-compression-probe--run ()
  "Run the probe and write the verdict."
  (let* ((output (getenv "SUPERCHAT_COMPRESSION_PROBE_OUTPUT"))
         (arms (superchat-compression-probe--load-traces))
         (episodes
          (cl-remove-if-not
           #'identity
           (mapcar #'superchat-compression-probe--episode arms)))
         (recovered (cl-remove-if-not
                     (lambda (e) (plist-get e :recovered-p)) episodes))
         (failed (cl-remove-if-not
                  (lambda (e) (not (plist-get e :recovered-p))) episodes)))
    (unless output (error "SUPERCHAT_COMPRESSION_PROBE_OUTPUT required"))
    (unless (and (>= (length recovered) 2) (>= (length failed) 2))
      (error "Need at least 2 episodes per class, have %d/%d"
             (length recovered) (length failed)))
    (cl-labels
        ((run-probe (prefix-only include-task)
           (let ((rows nil) (correct 0) (n 0))
             (dolist (episode episodes)
               (let* ((result (superchat-compression-probe--classify
                               episode episodes prefix-only include-task))
                      (predicted (nth 0 result))
                      (actual (nth 1 result)))
                 (when (eq predicted actual) (setq correct (1+ correct)))
                 (setq n (1+ n))
                 (push (list :arm (plist-get episode :arm)
                             :trace (file-name-nondirectory
                                     (plist-get episode :trace-file))
                             :actual actual :predicted predicted
                             :margin (nth 2 result))
                       rows)))
             (list :n n :correct correct :rows (nreverse rows)))))
      (let* ((primary (run-probe nil nil))
             (predictive (run-probe t nil))
             (task-confound (run-probe nil t))
             (candidate-statement
              (cl-some (lambda (a) (plist-get a :candidate-statement)) arms))
             (statement-scores
              (and candidate-statement
                   (let ((success-corpus
                          (mapconcat #'superchat-compression-probe--episode-text
                                     recovered "\n"))
                         (failure-corpus
                          (mapconcat #'superchat-compression-probe--episode-text
                                     failed "\n")))
                     (list
                      :success-corpus-score
                      (superchat-compression-probe--corpus-score
                       candidate-statement success-corpus)
                      :failure-corpus-score
                      (superchat-compression-probe--corpus-score
                       candidate-statement failure-corpus)))))
             (verdict
              (list :format "superchat-experience-compression-probe-v1"
                    :claim
                    "compression-prediction equivalence over real recovery episodes"
                    :source
                    (list :reference "https://nathan.rs/posts/gzip-lm/"
                          :compressor
                          (or superchat-compression-probe--gzip "gzip")
                          :emacs emacs-version)
                    :inventory
                    (list :traces (length (cl-remove-duplicates
                                           (mapcar
                                            (lambda (a)
                                              (plist-get a :trace-file))
                                            arms)
                                           :test #'equal))
                          :arms (length arms)
                          :episodes (length episodes)
                          :recovered (length recovered)
                          :failed (length failed)
                          :recovered-arms
                          (mapcar (lambda (e) (plist-get e :arm)) recovered)
                          :failed-arms
                          (mapcar (lambda (e) (plist-get e :arm)) failed))
                    :primary
                    (list :accuracy
                          (/ (float (plist-get primary :correct))
                             (plist-get primary :n))
                          :correct (plist-get primary :correct)
                          :n (plist-get primary :n)
                          :rows (plist-get primary :rows))
                    :predictive-prefix
                    (list :accuracy
                          (/ (float (plist-get predictive :correct))
                             (plist-get predictive :n))
                          :correct (plist-get predictive :correct)
                          :n (plist-get predictive :n)
                          :rows (plist-get predictive :rows))
                    :task-confound
                    (list :accuracy
                          (/ (float (plist-get task-confound :correct))
                             (plist-get task-confound :n))
                          :correct (plist-get task-confound :correct)
                          :n (plist-get task-confound :n))
                    :candidate-statement statement-scores
                    :notes
                    (list
                     :episode-text
                     "behavioral skeleton only: ordered ACTION tool OK|FAILED"
                     :excluded
                     "experience injection text, scope strings, ids, revisions"
                     :label-rule
                     "recovered = workspace_read OK after the first failed read"
                     :prefix-rule
                     "predictive probe serializes only up to the first action after the first failed read"))))
        (with-temp-buffer
          (prin1 verdict (current-buffer))
          (write-region (point-min) (point-max) output nil 'quiet))
        (princ (format "compression probe verdict: %s\n" output))
        (princ (format "episodes %d (recovered %d, failed %d)\n"
                       (length episodes) (length recovered)
                       (length failed)))
        (princ (format "primary accuracy %d/%d, predictive-prefix %d/%d\n"
                       (plist-get primary :correct) (plist-get primary :n)
                       (plist-get predictive :correct)
                       (plist-get predictive :n)))))))

(when (equal (getenv "SUPERCHAT_COMPRESSION_PROBE_EXECUTE") "1")
  (condition-case err
      (progn (superchat-compression-probe--run) (kill-emacs 0))
    (error
     (princ (format "compression probe infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-compression-probe)

;;; compression-probe.el ends here
