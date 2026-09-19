;;; m7-buffer-first-regression.el --- buffer-first mutation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-09-18-buffer-first-mutation.md.
;; Slice 1: `open_file' and the unsaved-changes outlet.  The canonical smoke
;; fixture supplies the Session, the Tape and a scripted Run; no real model.
;;
;;   -l scripts/m7-buffer-first-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^m7/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro m7--with-dir (dir &rest body)
  "Bind DIR to a fresh canonical directory; kill the buffers visiting it after."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "m7-" t)))))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,dir (file-truename file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory ,dir t))))

(defun m7--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent))
  file)

(defun m7--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro m7--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'workspace-write)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun m7--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

;;;; open_file ---------------------------------------------------------------

(ert-deftest m7/open-file/brings-the-file-into-emacs-without-taking-focus ()
  "It is a visit, not a read: major mode and `find-file-hook' run; the selected
window and the disk are left alone; the buffer is what later calls use."
  (skip-when (getenv "SMOKE_REAL"))
  (m7--with-dir dir
    (let* ((file (m7--write (expand-file-name "init.el" dir) "(setq a 1)\n"))
           (hooked 0)
           (find-file-hook (list (lambda () (cl-incf hooked))))
           (window-buffer-before (window-buffer (selected-window)))
           (current-before (current-buffer))
           (result (superchat-tool-open-file file))
           (buffer (get-file-buffer file)))
      (should (bufferp buffer))
      (should (= 1 hooked))
      (should (eq 'emacs-lisp-mode (buffer-local-value 'major-mode buffer)))
      (should (eq window-buffer-before (window-buffer (selected-window))))
      (should (eq current-before (current-buffer)))
      (should-not (buffer-modified-p buffer))
      (should (equal "(setq a 1)\n" (m7--disk file)))
      (should (string-match-p (regexp-quote (format "buffer %s" (buffer-name buffer))) result))
      (should (string-match-p (regexp-quote file) result))
      (should (string-match-p "edit_buffer" result))
      ;; Again: the same buffer, not reopened, not reverted, and said so.
      (with-current-buffer buffer (goto-char (point-max)) (insert ";; unsaved\n"))
      (let ((again (superchat-tool-open-file file)))
        (should (= 1 hooked))
        (should (eq buffer (get-file-buffer file)))
        (should (string-match-p "already open" again))
        (should (string-match-p "unsaved" again))
        (should (string-match-p ";; unsaved" (with-current-buffer buffer (buffer-string))))))))

(ert-deftest m7/open-file/a-path-that-does-not-exist-gives-a-buffer-and-no-file ()
  (skip-when (getenv "SMOKE_REAL"))
  (m7--with-dir dir
    (let* ((file (expand-file-name "new.el" dir))
           (result (superchat-tool-open-file file)))
      (should (get-file-buffer file))
      (should-not (file-exists-p file))
      (should (string-match-p "new file" result))
      (should (string-match-p "nothing .*on disk" result)))
    (let* ((deep (expand-file-name "no/such/dir/x.el" dir))
           (result (superchat-tool-open-file deep)))
      (should (get-file-buffer deep))
      (should-not (file-exists-p (file-name-directory deep)))
      (should (string-match-p "directory .* does not exist" result)))))

(ert-deftest m7/open-file/what-it-refuses ()
  (skip-when (getenv "SMOKE_REAL"))
  (m7--with-dir dir
    (let* ((big (m7--write (expand-file-name "big.txt" dir) (make-string 4000 ?x)))
           (superchat-data-directory (file-name-as-directory (expand-file-name "sc-data" dir)))
           (own (progn (make-directory superchat-data-directory t)
                       (m7--write (expand-file-name "state.el" superchat-data-directory) "x")))
           (before (buffer-list)))
      (cl-flet ((refused (regexp path)
                  (let ((result (superchat-tool-open-file path)))
                    (should (string-prefix-p "Error:" result))
                    (should (string-match-p regexp result)))))
        (refused "remote" "/ssh:nobody@example.invalid:/etc/hosts")
        (refused "directory" dir)
        (refused "own data directory" own)
        (refused "path" "")
        (refused "path" nil)
        (let ((large-file-warning-threshold 1000))
          (refused "large" big)))
      (should (equal before (buffer-list))))))

(ert-deftest m7/open-file/every-level-allows-it-and-nobody-is-asked ()
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "seen.el" dir) "(ignore)\n")))
      (dolist (level '(read-only workspace-write danger-full-access))
        (ert-info ((format "level: %s" level))
          (m7--fixture (list `(:tool-uses (("open_file" (path . ,file)))) '(:text "done"))
            (let* ((asked 0)
                   (superchat-approval-function (lambda (_request) (cl-incf asked) t))
                   (view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view)))
              (superchat-permission-set session level)
              (m7--settle (smoke--send view "open it"))
              (should (= 0 asked))
              (should (get-file-buffer file))
              (should (= 0 (cl-count "approval.requested" (smoke--types session) :test #'equal)))
              (should (member "open_file" (plist-get (car (last smoke--requests)) :tools))))))))))

(ert-deftest m7/open-file/is-on-the-default-surface-after-read-file ()
  (skip-when (getenv "SMOKE_REAL"))
  (should (eq 'builtin (superchat-sandbox-tool-class "open_file")))
  (should (equal '("read_buffer" "read_file" "open_file" "edit_buffer")
                 (cl-remove-if-not
                  (lambda (name) (member name '("read_buffer" "read_file" "open_file" "edit_buffer")))
                  superchat-llm-default-tool-names)))
  (should (equal superchat-llm-default-tool-names
                 (let (superchat-llm-default-tool-names)
                   (makunbound 'superchat-llm-default-tool-names)
                   (superchat--select-tools-baseline-names)))))

;;;; The unsaved-changes outlet -----------------------------------------------

(defun m7--edit-round (buffer-name old new)
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer-name)
                 (old_string . ,old) (new_string . ,new)))))

(ert-deftest m7/changes/are-derived-from-the-tape-and-the-modified-mark ()
  "Candidates = targets of the Run's structured edits that are live and modified
now.  A buffer only `eval_elisp' changed is not attributed -- a stated limit --
and a saved buffer leaves the list.  Nothing is written to disk by the model."
  (m7--with-dir dir
    (let* ((file (m7--write (expand-file-name "conf.el" dir) "(setq a 1)\n"))
           (other (m7--write (expand-file-name "other.el" dir) "(setq b 1)\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "conf.el" "(setq a 1)" "(setq a 2)")
                `(:tool-uses (("eval_elisp"
                               (expression . ,(format "(with-current-buffer (find-file-noselect %S) (goto-char (point-max)) (insert \";; by eval\\n\") t)" other)))))
                '(:text "done"))
        (let* ((superchat-approval-function (lambda (_request) t))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (m7--settle (smoke--send view "change a"))))
          ;; The model persisted nothing.
          (should (equal "(setq a 1)\n" (m7--disk file)))
          (should (buffer-modified-p (get-file-buffer file)))
          ;; The Tape names the target of the structured edit.
          (let ((completed
                 (cl-find-if (lambda (event)
                               (and (equal "tool.completed" (plist-get event :type))
                                    (equal "edit_buffer"
                                           (plist-get (plist-get event :payload) :tool-name))))
                             (superchat-tape-replay-events session))))
            (should completed)
            (should (equal "conf.el" (plist-get (plist-get completed :payload) :buffer)))
            (should (equal file (plist-get (plist-get completed :payload) :buffer-file))))
          (let ((of-run (superchat-changes-candidates session run))
                (of-session (superchat-changes-candidates session)))
            (should (equal (list (get-file-buffer file))
                           (mapcar (lambda (row) (plist-get row :buffer)) of-run)))
            (should (equal of-run of-session))
            (should (equal file (plist-get (car of-run) :file))))
          ;; `eval_elisp' changed other.el: modified, and not attributed.
          (should (buffer-modified-p (get-file-buffer other)))
          (should-not (cl-find (get-file-buffer other)
                               (superchat-changes-candidates session)
                               :key (lambda (row) (plist-get row :buffer))))
          ;; The Run-end summary names it; another Run's does not.
          (should (string-match-p "conf.el" (superchat-changes-run-summary session run)))
          (should-not (superchat-changes-run-summary session "run-that-never-was"))
          ;; Saving is the user's operation, and empties the list.
          (superchat-changes-save (get-file-buffer file))
          (should (equal "(setq a 2)\n" (m7--disk file)))
          (should-not (superchat-changes-candidates session))
          (should-not (superchat-changes-run-summary session run)))))))

(ert-deftest m7/changes/the-tape-names-the-base-buffer-and-the-list-reads-the-live-one ()
  "An edit through an indirect buffer is recorded against the base buffer that
really changed.  The recorded file is evidence from that moment: after a
rename and a `set-visited-file-name' the list shows the live buffer's name and
file as they are now, never the old path."
  (m7--with-dir dir
    (let* ((file (m7--write (expand-file-name "base.el" dir) "one\n"))
           (moved (expand-file-name "moved.el" dir))
           (base (find-file-noselect file))
           (indirect (make-indirect-buffer base "m7-indirect-view" t)))
      (unwind-protect
          (m7--fixture (list (m7--edit-round "m7-indirect-view" "one" "two") '(:text "done"))
            (let* ((view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view)))
              (m7--settle (smoke--send view "edit through the indirect buffer"))
              (let ((payload (plist-get
                              (cl-find-if (lambda (event)
                                            (and (equal "tool.completed" (plist-get event :type))
                                                 (plist-get (plist-get event :payload) :buffer)))
                                          (superchat-tape-replay-events session))
                              :payload)))
                (should (equal (buffer-name base) (plist-get payload :buffer)))
                (should (equal file (plist-get payload :buffer-file))))
              (should (equal (list base) (mapcar (lambda (row) (plist-get row :buffer))
                                                 (superchat-changes-candidates session))))
              ;; Renamed: still found (through the recorded file), shown by its
              ;; name now.
              (with-current-buffer base (rename-buffer "m7-renamed.el"))
              (let ((row (car (superchat-changes-candidates session))))
                (should (eq base (plist-get row :buffer)))
                (should (equal "m7-renamed.el" (plist-get row :name))))
              ;; Visiting another file now, under its recorded name again: the
              ;; list reports the file it visits now.
              (with-current-buffer base
                (set-visited-file-name moved t)
                (rename-buffer "base.el")
                (set-buffer-modified-p t))
              (let ((row (car (superchat-changes-candidates session))))
                (should (eq base (plist-get row :buffer)))
                (should (equal moved (plist-get row :file)))
                (should (plist-get row :new-file)))
              ;; Somebody opens the old path again: that unmodified buffer is
              ;; not what the tool edited, and the old path must not lead to it.
              (let ((reopened (find-file-noselect file)))
                (unwind-protect
                    (progn
                      (should-not (eq reopened base))
                      (should (equal (list base)
                                     (mapcar (lambda (row) (plist-get row :buffer))
                                             (superchat-changes-candidates session)))))
                  (kill-buffer reopened)))))
        (when (buffer-live-p indirect) (kill-buffer indirect))))))

(ert-deftest m7/changes/revert-is-the-users-operation-too ()
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "r.el" dir) "one\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "r.el" "one" "two")
                '(:text "done"))
        (let* ((view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view)))
          (m7--settle (smoke--send view "edit"))
          (should (= 1 (length (superchat-changes-candidates session))))
          (superchat-changes-revert (get-file-buffer file))
          (should (equal "one\n" (with-current-buffer (get-file-buffer file) (buffer-string))))
          (should (equal "one\n" (m7--disk file)))
          (should-not (superchat-changes-candidates session)))))))

(ert-deftest m7/changes/the-command-and-the-run-end-notice-are-installed ()
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "n.el" dir) "one\n"))
          (noticed nil))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "n.el" "one" "two")
                '(:text "done"))
        (cl-letf (((symbol-function 'superchat-changes--announce)
                   (lambda (text) (push text noticed))))
          (let* ((view (smoke--open-chat))
                 (session (buffer-local-value 'superchat--session-id view)))
            (should (member "changes" (superchat--commands-list)))
            (m7--settle (smoke--send view "edit"))
            (should (smoke--wait (lambda () noticed) 5))
            (should (= 1 (length noticed)))
            (should (string-match-p "n.el" (car noticed)))
            (should (string-match-p "/changes" (car noticed)))
            (with-current-buffer view
              (let ((content (plist-get (superchat--cmd-changes "changes" "" nil nil nil)
                                        :content)))
                (should (string-match-p "n.el" content))
                (should (string-match-p (regexp-quote file) content))))
            ;; Nothing unsaved: nothing announced, and the command says so.
            (superchat-changes-save (get-file-buffer file))
            (with-current-buffer view
              (should (string-match-p "No unsaved"
                                      (plist-get (superchat--cmd-changes "changes" "" nil nil nil)
                                                 :content))))))))))

(provide 'm7-buffer-first-regression)

;;; m7-buffer-first-regression.el ends here
