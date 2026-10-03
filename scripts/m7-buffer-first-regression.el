;;; m7-buffer-first-regression.el --- buffer-first mutation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/implemented/2026-09-18-buffer-first-mutation.md.
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
           (superchat-sandbox-default-mode 'edit-buffers)
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
      (dolist (level '(read-only edit-buffers danger-full-access))
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

;;;; The edited buffer is recognized by its own mark -------------------------------
;; docs/decisions/accepted/2026-09-19-changes-recognizes-a-buffer-by-its-mark.md
;; A name and a file describe a buffer at one moment; neither is its identity.

(defun m7--candidate-buffers (session &optional run)
  (mapcar (lambda (row) (plist-get row :buffer))
          (superchat-changes-candidates session run)))

(ert-deftest m7/changes/a-buffer-that-took-the-old-name-is-not-the-edited-one ()
  "The development audit's case: the edited buffer is renamed, an unrelated
buffer with the user's own unsaved text takes its old name.  Only the buffer
the tool really changed is listed."
  (m7--with-dir dir
    (let* ((file (m7--write (expand-file-name "owned.el" dir) "one\n"))
           (other (m7--write (expand-file-name "unrelated.el" dir) "unrelated\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "owned.el" "one" "two") '(:text "done"))
        (let* ((view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (m7--settle (smoke--send view "edit")))
               (target (get-file-buffer file))
               (impostor (find-file-noselect other)))
          (with-current-buffer target (rename-buffer "actual-edited-buffer"))
          (with-current-buffer impostor
            (rename-buffer "owned.el")
            (goto-char (point-max)) (insert "user edit\n"))
          (should (equal (list target) (m7--candidate-buffers session run)))
          (should (equal (list target) (m7--candidate-buffers session)))
          (should (string-match-p "actual-edited-buffer"
                                  (superchat-changes-run-summary session run)))
          (should-not (string-match-p "unrelated" (superchat-changes-run-summary session run))))))))

(ert-deftest m7/changes/a-reopened-buffer-is-a-different-buffer ()
  "Killed and visited again -- same name, same file, edited by the user: the
tool's change went away with the old buffer, so nothing is listed.  The same
for a buffer without a file."
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "again.el" dir) "one\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "again.el" "one" "two")
                '(:tool-uses (("append_to_buffer" (buffer . "*m7-scratch-notes*") (text . "by the tool"))))
                '(:text "done"))
        (let* ((superchat-llm-tool-names (append superchat-llm-default-tool-names
                                                 '("append_to_buffer")))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (m7--settle (smoke--send view "edit"))))
          (unwind-protect
              (progn
                (should (= 2 (length (m7--candidate-buffers session run))))
                (with-current-buffer (get-file-buffer file)
                  (set-buffer-modified-p nil) (kill-buffer))
                (with-current-buffer "*m7-scratch-notes*"
                  (set-buffer-modified-p nil) (kill-buffer))
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-max)) (insert "the user's own\n"))
                (with-current-buffer (get-buffer-create "*m7-scratch-notes*")
                  (insert "the user's own") (set-buffer-modified-p t))
                (should-not (m7--candidate-buffers session run))
                (should-not (superchat-changes-run-summary session run)))
            (when (get-buffer "*m7-scratch-notes*")
              (with-current-buffer "*m7-scratch-notes*" (set-buffer-modified-p nil))
              (kill-buffer "*m7-scratch-notes*"))))))))

(ert-deftest m7/changes/the-mark-stays-with-the-buffer-and-with-nothing-else ()
  "Guard: a major-mode change does not wipe it; a clone does not inherit it; an
edit the tool sent back leaves none."
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "mode.el" dir) "(defun a () 1)\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "mode.el" "1)" "2)")
                '(:tool-uses (("append_to_buffer" (buffer . "*m7-clone-source*") (text . "by the tool"))))
                '(:text "done"))
        (let* ((superchat-llm-tool-names (append superchat-llm-default-tool-names
                                                 '("append_to_buffer")))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (m7--settle (smoke--send view "edit")))
               (target (get-file-buffer file))
               (source (get-buffer "*m7-clone-source*"))
               clone)
          (unwind-protect
              (progn
                (with-current-buffer target (text-mode))
                (should (memq target (m7--candidate-buffers session run)))
                (setq clone (with-current-buffer source (clone-buffer "*m7-clone*")))
                (with-current-buffer clone (set-buffer-modified-p t))
                (should (memq source (m7--candidate-buffers session run)))
                (should-not (memq clone (m7--candidate-buffers session run)))
                ;; Sent back: the buffer was never changed, so it is not marked.
                (let* ((untouched (find-file-noselect
                                   (m7--write (expand-file-name "sent-back.el" dir) "(defun b () 1)\n")))
                       (superchat--tool-execution-context
                        (list :session-id session :run-id run)))
                  (should (string-prefix-p "Error:" (superchat-tool-edit-buffer "sent-back.el" "1)" "1))")))
                  (with-current-buffer untouched (set-buffer-modified-p t))
                  (should-not (memq untouched (m7--candidate-buffers session run)))
                  (with-current-buffer untouched (set-buffer-modified-p nil))))
            (dolist (buffer (list source clone))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))))))))

(ert-deftest m7/changes/each-run-lists-its-own-buffers ()
  "Two Runs of one Session edit two buffers, a Run of another Session a third.
A Run lists what it marked; the Session lists both of its Runs'; nobody lists
the other Session's."
  (m7--with-dir dir
    (let ((first-file (m7--write (expand-file-name "first.el" dir) "one\n"))
          (second-file (m7--write (expand-file-name "second.el" dir) "one\n"))
          (foreign-file (m7--write (expand-file-name "foreign.el" dir) "one\n")))
      (m7--fixture
          (list `(:tool-uses (("open_file" (path . ,first-file))))
                (m7--edit-round "first.el" "one" "two") '(:text "first done")
                `(:tool-uses (("open_file" (path . ,second-file))))
                (m7--edit-round "second.el" "one" "two") '(:text "second done")
                `(:tool-uses (("open_file" (path . ,foreign-file))))
                (m7--edit-round "foreign.el" "one" "two") '(:text "foreign done"))
        (let* ((view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (first (m7--settle (smoke--send view "first")))
               (second (m7--settle (smoke--send view "second")))
               (foreign-session (format "m7-foreign-%06x" (random #x1000000)))
               (foreign (m7--submit-with (get-buffer-create " *m7-foreign-host*") foreign-session
                                         '("open_file" "edit_buffer") "foreign"))
               (a (get-file-buffer first-file))
               (b (get-file-buffer second-file))
               (c (get-file-buffer foreign-file)))
          (should (equal (list a) (m7--candidate-buffers session first)))
          (should (equal (list b) (m7--candidate-buffers session second)))
          (should (equal (list c) (m7--candidate-buffers foreign-session foreign)))
          (let ((of-session (m7--candidate-buffers session)))
            (should (= 2 (length of-session)))
            (should (memq a of-session))
            (should (memq b of-session))
            (should-not (memq c of-session))))))))

(ert-deftest m7/changes/a-delegated-runs-edit-belongs-to-the-run-that-started-it ()
  "A child Run in its own Session, started by a Run of this Session, edits a
buffer.  The parent's Run and Session list it; the mark carries the child's
identity and the Tape says whose child it is.  A Session that started nothing
lists nothing."
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "delegated.el" dir) "one\n")))
      (m7--fixture
          (list '(:hang t)
                `(:tool-uses (("open_file" (path . ,file))))
                (m7--edit-round "delegated.el" "one" "two")
                '(:text "child done"))
        (let* ((view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (parent (smoke--send view "delegate it"))
               (child-session (format "m7-child-%06x" (random #x1000000)))
               (child (plist-get
                       (superchat-service-call
                        nil 'driver 'submit
                        (list :surface 'external :session-id child-session
                              :parent-run-id parent
                              :tools '("open_file" "edit_buffer") :content "edit it"
                              ;; The chat View belongs to the parent's Session.
                              :context (list :host-buffer (get-buffer-create " *m7-child-host*"))
                              :runtime-extras '(:record-conversation nil)
                              :sink (superchat-sink-discard)))
                       :run-id)))
          (unwind-protect
              (progn
                (m7--settle child)
                (let ((buffer (get-file-buffer file)))
                  (should (buffer-modified-p buffer))
                  (should (equal (list buffer) (m7--candidate-buffers session parent)))
                  (should (equal (list buffer) (m7--candidate-buffers session)))
                  (should (equal (list buffer) (m7--candidate-buffers child-session child)))
                  (should-not (m7--candidate-buffers "m7-a-session-that-started-nothing"))))
            (ignore-errors (superchat-runtime-cancel parent "[m7 done]"))))))))

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

;;;; Slice 2: the write path ends at the buffer ---------------------------------

(ert-deftest m7/write-path/write-file-is-gone-and-nothing-offers-escalation ()
  (skip-when (getenv "SMOKE_REAL"))
  (should-not (fboundp 'superchat-tool-write-file))
  (should-not (member "write_file" superchat-tools--builtin-tool-names))
  (should-not (superchat--tool-descriptor-p "write_file"))
  (dolist (gone '(superchat-sandbox-run-direct-write superchat-sandbox-path-writable-p
                  superchat-sandbox-denial-marker superchat-sandbox-escalation-hint
                  superchat-sandbox-wider-p superchat-sandbox-resolve
                  superchat--llm-tool-entry-escalatable-p))
    (should-not (fboundp gone)))
  (let ((superchat-llm-tool-names 'all)
        (superchat-llm-tools-list nil))
    (superchat-llm-tools-reload)
    (unwind-protect
        (progn
          (should-not (cl-find "write_file" superchat-llm-tools-list
                               :key #'llm-tool-name :test #'equal))
          (dolist (tool superchat-llm-tools-list)
            (dolist (arg (llm-tool-args tool))
              (should-not (member (plist-get arg :name)
                                  '("sandbox_permissions" "justification"))))))
      (setq superchat-llm-tools-list nil)))
  ;; A literal sandbox marker in tool output is ordinary text again.
  (should-not (superchat--llm-tool-error-feedback
               '(:tool-results (("read_buffer" . "[sandbox: file access denied under x mode]"))))))

(ert-deftest m7/write-path/the-default-surface-has-no-persisting-tool ()
  "`save_buffer' exists only where a Preset or Request names it; no level adds
it, `danger-full-access' included."
  (should-not (member "save_buffer" superchat-llm-default-tool-names))
  ;; 2026-09-20: `read_reference' joined the list (reference reading on demand);
  ;; it reads bundled data and persists nothing.  2026-10-02 (M11 slice 4): the
  ;; three timer tools joined; they write timer facts to the Tape, no file.
  ;; Slice 6: `elisp_script', which persists nothing itself; `list_buffers',
  ;; `timer_list' and `timer_cancel' are now reached from a script.
  (should (equal '("apropos" "describe-function" "read_buffer" "read_file" "read_reference"
                   "open_file" "edit_buffer" "eval_elisp" "timer_create" "elisp_script")
                 superchat-llm-default-tool-names))
  (dolist (level '(read-only edit-buffers danger-full-access))
    (ert-info ((format "level: %s" level))
      (m7--fixture '((:text "ok"))
        (let* ((view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view)))
          (superchat-permission-set session level)
          (m7--settle (smoke--send view "hello"))
          (should-not (member "save_buffer" (plist-get (car smoke--requests) :tools)))
          (should (member "edit_buffer" (plist-get (car smoke--requests) :tools))))))))

(defun m7--submit-with (view session tools text)
  (m7--settle
   (plist-get (superchat-service-call
               nil 'driver 'submit
               (list :surface 'external :session-id session :tools tools :content text
                     :context (list :host-buffer view)
                     :runtime-extras '(:record-conversation nil)
                     :sink (superchat-sink-discard)))
              :run-id)))

;; Found by m7-attended-act-03-deepseek: `eval_elisp' still told the model
;; "For persistence, save the actual loaded configuration and verify in a fresh
;; Emacs that loads it" -- the opposite of what `edit_buffer' says -- and the
;; model did exactly that, three attended runs in a row.
(ert-deftest m7/write-path/no-default-tool-tells-the-model-to-persist ()
  "Every description on the default surface agrees on who saves: the user.
None instructs the model to save or to prove persistence in a fresh Emacs, and
`eval_elisp', the one tool that could, says so itself."
  (m7--fixture '((:text "unused"))
    (let ((superchat-llm-tools-list nil)
          (case-fold-search t))
      (superchat-llm-tools-reload)
      (let ((tools (superchat-get-llm-tools nil)))
        (should (cl-find "eval_elisp" tools :key #'llm-tool-name :test #'equal))
        (dolist (tool tools)
          (ert-info ((format "tool: %s" (llm-tool-name tool)))
            (should-not (string-match-p
                         "for persistence\\|fresh emacs\\|\\bsave \\(the\\|it\\|your\\|that\\|this\\) "
                         (llm-tool-description tool)))))
        (let ((eval-text (llm-tool-description
                          (cl-find "eval_elisp" tools :key #'llm-tool-name :test #'equal))))
          (should (string-match-p "saving is the user's decision" eval-text))
          (should (string-match-p "not for saving" eval-text)))))))

(ert-deftest m7/write-path/save-buffer-composes-with-the-approval-policy ()
  "The five rows of the contract's table.  `ask' is the approval seam, so under
`never' the question cannot be put and the call is refused -- even at
`danger-full-access', even though the request offered the tool."
  (m7--with-dir dir
    (dolist (row '((read-only ask nil 0)
                   (edit-buffers ask t 1)
                   (edit-buffers never nil 0)
                   (danger-full-access never nil 0)
                   (danger-full-access ask t 1)))
      (pcase-let ((`(,mode ,policy ,saved ,asked-expected) row))
        (ert-info ((format "%s + %s" mode policy))
          (let ((file (m7--write (expand-file-name (format "%s-%s.el" mode policy) dir) "disk\n")))
            (m7--fixture (list `(:tool-uses (("save_buffer"
                                              (buffer_name . ,(file-name-nondirectory file)))))
                               '(:text "done"))
              (let* ((asked 0)
                     (superchat-approval-function (lambda (_request) (cl-incf asked) t))
                     (view (smoke--open-chat))
                     (session (buffer-local-value 'superchat--session-id view))
                     (buffer (find-file-noselect file)))
                (with-current-buffer buffer (goto-char (point-max)) (insert "memory\n"))
                (superchat-sandbox-set-mode session mode)
                (superchat-approval-set-policy session policy)
                (m7--submit-with view session '("save_buffer") "save it")
                (should (= asked-expected asked))
                (should (equal (if saved "disk\nmemory\n" "disk\n") (m7--disk file)))
                (should (eq (not saved) (buffer-modified-p buffer)))))))))))

(ert-deftest m7/write-path/an-ordinary-buffer-is-editable-wherever-its-file-lives ()
  "No workspace boundary: the edit stays in the buffer, asks nobody, and the
disk is untouched.  Superchat's own buffers are refused under every mode."
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "far/away.el" (progn (make-directory (expand-file-name "far" dir)) dir))
                           "one\n")))
      (m7--fixture (list `(:tool-uses (("open_file" (path . ,file))))
                         (m7--edit-round "away.el" "one" "two")
                         '(:text "done"))
        (let* ((asked 0)
               (superchat-approval-function (lambda (_request) (cl-incf asked) t))
               (view (smoke--open-chat)))
          (m7--settle (smoke--send view "edit"))
          (should (= 0 asked))
          (should (equal "two\n" (with-current-buffer (get-file-buffer file) (buffer-string))))
          (should (equal "one\n" (m7--disk file)))
          (dolist (mode '(edit-buffers danger-full-access))
            (let ((superchat-sandbox-default-mode mode)
                  (result (superchat-tool-append-to-buffer (buffer-name view) "X")))
              (should (string-prefix-p "[Tool call denied:" result))
              (should (string-match-p "own state" result)))))))))

(ert-deftest m7/write-path/the-users-environment-may-persist-and-is-not-fought ()
  "A hook that saves a buffer whenever it changes -- what `super-save' and its
like do -- writes the model's edit to disk.  That is the user's environment;
Superchat neither refuses nor warns."
  (m7--with-dir dir
    (let ((file (m7--write (expand-file-name "auto.el" dir) "one\n")))
      (m7--fixture (list `(:tool-uses (("open_file" (path . ,file))))
                         (m7--edit-round "auto.el" "one" "two")
                         '(:text "done"))
        (let ((view (smoke--open-chat)))
          (with-current-buffer (find-file-noselect file)
            (add-hook 'after-change-functions
                      (lambda (&rest _) (let ((inhibit-message t)) (basic-save-buffer)))
                      nil t))
          (m7--settle (smoke--send view "edit"))
          (should (equal "two\n" (m7--disk file)))
          (should (cl-some (lambda (request)
                             (string-match-p "Replaced 1 occurrence"
                                             (plist-get request :prompt-text)))
                           smoke--requests))
          (should-not (cl-some (lambda (request)
                                 (string-match-p "Tool call denied" (plist-get request :prompt-text)))
                               smoke--requests)))))))

(ert-deftest m7/levels/the-default-is-edit-buffers-and-the-old-name-still-reads ()
  "New events, the UI and the model's context say `edit-buffers'.  The former
name selects the same level with a notice, an old Tape row reads as the new
mode and is left as it was, and nothing writes the old name again."
  (m7--fixture '((:text "ok"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (should (eq 'edit-buffers (superchat-sandbox-effective-mode session)))
      (should (eq 'edit-buffers (superchat-permission-current session)))
      (let ((superchat-sandbox-default-mode 'workspace-write))
        (should (eq 'edit-buffers (superchat-sandbox-effective-mode session))))
      (superchat-permission-set session 'read-only)
      (with-current-buffer view
        (let ((content (plist-get (superchat--cmd-permissions "permissions" "workspace-write" nil nil nil)
                                  :content)))
          (should (string-prefix-p "Permissions: edit-buffers" content))
          (should (string-match-p "former name" content)))
        (should-not (string-match-p
                     "workspace-write"
                     (plist-get (superchat--cmd-permissions "permissions" "" nil nil nil) :content))))
      (should (eq 'edit-buffers (superchat-permission-current session)))
      ;; An old row, written by hand as M6 wrote it.
      (superchat-tape-append-event
       (list :type "sandbox.mode" :session-id session :actor-id "user"
             :summary "sandbox.mode" :payload (list :mode "read-only")))
      (superchat-tape-append-event
       (list :type "sandbox.mode" :session-id session :actor-id "user"
             :summary "sandbox.mode" :payload (list :mode "workspace-write")))
      (superchat-tape-append-event
       (list :type "permission.level" :session-id session :actor-id "user"
             :summary "permission.level" :payload (list :level "workspace-write")))
      (should (eq 'edit-buffers (superchat-sandbox-effective-mode session)))
      (should (eq 'edit-buffers (superchat-permission-current session)))
      ;; Apart from those two hand-written rows, no event carries the old name.
      (let ((written (cl-remove-if-not
                      (lambda (event)
                        (member (plist-get event :type) '("sandbox.mode" "permission.level")))
                      (superchat-tape-replay-events session))))
        (should (= 2 (cl-count-if
                      (lambda (event)
                        (member "workspace-write"
                                (list (plist-get (plist-get event :payload) :mode)
                                      (plist-get (plist-get event :payload) :level))))
                      written))))
      (m7--settle (smoke--send view "hello"))
      (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "Current Superchat mode: edit-buffers" prompt))
        (should (string-match-p "saving a buffer to disk is the user's decision" prompt))
        (should-not (string-match-p "workspace-write\\|sandbox_permissions" prompt))))))

(provide 'm7-buffer-first-regression)

;;; m7-buffer-first-regression.el ends here
