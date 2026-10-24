;;; actor-probe.el --- which Actor a woken Run and a timer-started Run record -*- lexical-binding: t; -*-
;; Guidance 2026-10-11.  The architecture research inferred from the send path
;; that an automatic execution's `run.started' carries the same :actor-id as a
;; user's; it was never probed.  Observations only: the tests print what they find.
(load (expand-file-name "scripts/pending-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t)
    (advice-remove 'load skip-loaded)))

(defun actor--started (session)
  "The :actor-id of every `run.started' of SESSION, oldest first."
  (mapcar (lambda (e) (plist-get e :actor-id))
          (cl-remove-if-not (lambda (e) (equal "run.started" (plist-get e :type)))
                            (superchat-tape-replay-events session))))

(ert-deftest actor/a-woken-records-run-records-which-actor ()
  (pend--fresh
    (wrec--with '((:text "WOKEN")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((pair (wwf--due-record p "Actor probe"))
                 (main (car pair))
                 (views (superchat-work-wake-due)))
            (should (= 1 (length views)))
            (wrec--settled)
            (let ((woken (hm--session (car views))))
              (princ (format "\nmain conversation run.started actors=%S\nwoken conversation run.started actors=%S\n"
                             (actor--started main) (actor--started woken)))
              (should (actor--started woken)))))))))

(ert-deftest actor/a-scheduled-tasks-run-records-which-actor ()
  (pend--fresh
    (un--with-task '((:text "TASK")) (session fired)
      (princ (format "\ntask conversation run.started actors=%S\n" (actor--started session)))
      (should (actor--started session)))))

(ert-run-tests-batch-and-exit "^actor/")
