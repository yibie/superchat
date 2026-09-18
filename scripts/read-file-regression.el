;;; read-file-regression.el --- read_file: a local text file no buffer visits -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-09-18-read-file.md.  The
;; canonical smoke fixture supplies the Session, the Tape and a scripted Run;
;; no real model and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/read-file-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^read-file/")'

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

(defmacro rf--with-dir (dir &rest body)
  "Bind DIR to a fresh canonical directory; run BODY; delete it."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "rf-" t)))))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun rf--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent))
  file)

(defun rf--header (result)
  "The header of RESULT: everything before the content marker."
  (should (string-match "\nContent:\n" result))
  (substring result 0 (match-beginning 0)))

(defun rf--body (result)
  "The content of RESULT, exactly as the tool returned it."
  (should (string-match "\nContent:\n" result))
  (substring result (match-end 0)))

(defmacro rf--fixture (script &rest body)
  "Run BODY in a scripted smoke fixture that offers the default tools."
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

(defun rf--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun rf--read-round (file &rest more)
  `(:tool-uses (("read_file" (path . ,file) ,@more))))

;;;; What it returns -----------------------------------------------------------

(ert-deftest read-file/returns-the-disk-text-with-a-header-and-no-line-numbers ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((text "(setq a 1)\n;; 2: not a line number\n  indented\n")
           (file (rf--write (expand-file-name "init.el" dir) text))
           (result (superchat-tool-read-file file)))
      (should (equal text (rf--body result)))
      (let ((header (rf--header result)))
        (should (string-match-p (regexp-quote (concat "File: " file)) header))
        (should (string-match-p "Lines: 1-3 of 3" header))
        (should (string-match-p "complete file" header))
        (should-not (string-match-p "NOT" header))))))

(ert-deftest read-file/the-path-reported-is-the-resolved-one ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "real.txt" dir) "x\n"))
           (link (expand-file-name "link.txt" dir)))
      (make-symbolic-link file link)
      (let ((result (superchat-tool-read-file link)))
        (should (equal "x\n" (rf--body result)))
        (should (string-match-p (regexp-quote (concat "File: " file)) (rf--header result)))))))

(ert-deftest read-file/creates-no-visiting-buffer-and-runs-no-find-file-machinery ()
  "The user's buffer list is what it was, and nothing that opening a file
triggers has run: no `find-file-hook', no major mode, no file-local variables."
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "probe.el" dir)
                            ";; -*- eval: (setq rf--local-ran t) -*-\n(ignore)\n"))
           (before (buffer-list))
           (hooked 0)
           (find-file-hook (list (lambda () (cl-incf hooked))))
           (emacs-lisp-mode-hook (list (lambda () (cl-incf hooked))))
           (enable-local-variables :all)
           (enable-local-eval t))
      (defvar rf--local-ran)
      (setq rf--local-ran nil)
      (should (string-match-p "(ignore)" (rf--body (superchat-tool-read-file file))))
      (should (= 0 hooked))
      (should-not rf--local-ran)
      (should-not (get-file-buffer file))
      (should (equal before (buffer-list))))))

(ert-deftest read-file/an-unsaved-visiting-buffer-is-named-and-left-alone ()
  "Always the disk version; the buffer is named, not saved, not synced."
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "notes.txt" dir) "on disk\n"))
           (buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "unsaved\n"))
            (let ((result (superchat-tool-read-file file)))
              (should (equal "on disk\n" (rf--body result)))
              (should (string-match-p (regexp-quote (buffer-name buffer)) (rf--header result)))
              (should (string-match-p "unsaved" (rf--header result)))
              (should (string-match-p "read_buffer" (rf--header result))))
            (should (buffer-modified-p buffer))
            (should (equal "on disk\nunsaved\n"
                           (with-current-buffer buffer (buffer-string))))
            (should (equal "on disk\n"
                           (with-temp-buffer (insert-file-contents file) (buffer-string))))
            ;; A visiting buffer with nothing unsaved is not worth a warning.
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (should-not (string-match-p "unsaved"
                                        (rf--header (superchat-tool-read-file file)))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

;;;; Windows and the three limits --------------------------------------------

(ert-deftest read-file/a-window-says-it-is-not-the-whole-file-and-how-to-go-on ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "ten.txt" dir)
                            (mapconcat (lambda (n) (format "line %d" n))
                                       (number-sequence 1 10) "\n")))
           (first (superchat-tool-read-file file nil 4))
           (middle (superchat-tool-read-file file 5 4))
           (last (superchat-tool-read-file file 9)))
      (should (equal "line 1\nline 2\nline 3\nline 4\n" (rf--body first)))
      (should (string-match-p "Lines: 1-4 of 10" (rf--header first)))
      (should (string-match-p "NOT the complete file" (rf--header first)))
      (should (string-match-p "offset 5" (rf--header first)))
      (should (equal "line 5\nline 6\nline 7\nline 8\n" (rf--body middle)))
      (should (string-match-p "NOT the complete file" (rf--header middle)))
      ;; The file's last line has no newline, and none is invented.
      (should (equal "line 9\nline 10" (rf--body last)))
      (should (string-match-p "Lines: 9-10 of 10" (rf--header last)))
      ;; It starts at line 9, so it is still not the whole file.
      (should (string-match-p "NOT the complete file" (rf--header last))))))

(ert-deftest read-file/more-than-the-line-limit-is-cut-at-2000-lines ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "many.txt" dir)
                            (mapconcat #'number-to-string (number-sequence 1 2500) "\n")))
           (result (superchat-tool-read-file file)))
      (should (= 2000 (length (split-string (rf--body result) "\n" t))))
      (should (string-match-p "Lines: 1-2000 of 2500" (rf--header result)))
      (should (string-match-p "NOT the complete file" (rf--header result)))
      (should (string-match-p "offset 2001" (rf--header result)))
      (should (equal "2001\n2002\n"
                     (rf--body (superchat-tool-read-file file 2001 2)))))))

(ert-deftest read-file/a-long-line-is-cut-and-said-to-be-cut ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((file (rf--write (expand-file-name "long.txt" dir)
                            (concat "short\n" (make-string 5000 ?x) "\nend\n")))
           (result (superchat-tool-read-file file))
           (lines (split-string (rf--body result) "\n")))
      (should (equal "short" (nth 0 lines)))
      (should (equal (concat (make-string 2000 ?x) "... (line truncated to 2000 chars)")
                     (nth 1 lines)))
      (should (equal "end" (nth 2 lines)))
      (should (string-match-p "1 line.* truncated to 2000 characters" (rf--header result)))
      (should (string-match-p "NOT" (rf--header result))))))

(ert-deftest read-file/the-byte-limit-stops-at-a-line-boundary-and-says-so ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let* ((line (make-string 999 ?b))
           (file (rf--write (expand-file-name "big.txt" dir)
                            (concat (mapconcat (lambda (_) line) (number-sequence 1 100) "\n") "\n")))
           (result (superchat-tool-read-file file))
           (body (rf--body result))
           (returned (length (split-string body "\n" t))))
      (should (<= (string-bytes body) (* 50 1024)))
      (should (= 51 returned))
      (should (string-suffix-p "\n" body))
      (should (string-match-p "Lines: 1-51 of 100" (rf--header result)))
      (should (string-match-p "NOT the complete file" (rf--header result)))
      (should (string-match-p "51200-byte limit" (rf--header result)))
      (should (string-match-p "offset 52" (rf--header result)))
      (should (= 49 (length (split-string
                             (rf--body (superchat-tool-read-file file 52)) "\n" t)))))))

(ert-deftest read-file/empty-files-and-offsets-past-the-end-are-ordinary-results ()
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let ((empty (rf--write (expand-file-name "empty.txt" dir) ""))
          (two (rf--write (expand-file-name "two.txt" dir) "a\nb\n")))
      (let ((result (superchat-tool-read-file empty)))
        (should (equal "" (rf--body result)))
        (should (string-match-p "empty" (rf--header result))))
      (let ((result (superchat-tool-read-file two 7)))
        (should (equal "" (rf--body result)))
        (should (string-match-p "offset 7 is past the end" (rf--header result)))
        (should (string-match-p "2 lines" (rf--header result)))))))

;;;; What it refuses -----------------------------------------------------------

(ert-deftest read-file/only-local-regular-text-files ()
  "Each refusal is an error string, decided before any byte is read."
  (skip-when (getenv "SMOKE_REAL"))
  (rf--with-dir dir
    (let ((fifo (expand-file-name "pipe" dir))
          (binary (expand-file-name "blob.bin" dir)))
      (let ((coding-system-for-write 'binary))
        (write-region (unibyte-string ?a 0 ?b 255 10) nil binary nil 'silent))
      (cl-flet ((refused (regexp &rest args)
                  (let ((result (apply #'superchat-tool-read-file args)))
                    (should (string-prefix-p "Error:" result))
                    (should (string-match-p regexp result))
                    (should-not (string-match-p "\nContent:\n" result)))))
        (refused "remote" "/ssh:nobody@example.invalid:/etc/hosts")
        (refused "directory" dir)
        (refused "does not exist" (expand-file-name "missing.txt" dir))
        (refused "binary" binary)
        (refused "path" "")
        (refused "path" nil)
        (let ((file (rf--write (expand-file-name "ok.txt" dir) "ok\n")))
          (refused "offset" file 0)
          (refused "offset" file -3)
          (refused "limit" file 1 0)
          (refused "limit" file 1 2001)
          (refused "limit" file 1 "many")
          ;; A model's JSON number may arrive as a float or a digit string.
          (should (equal "ok\n" (rf--body (superchat-tool-read-file file 1.0 "5")))))
        (when (executable-find "mkfifo")
          (should (zerop (call-process "mkfifo" nil nil nil fifo)))
          ;; Reading a FIFO with no writer would block forever.
          (with-timeout (5 (ert-fail "read_file blocked on a FIFO"))
            (refused "regular file" fifo)))))))

;;;; Permissions and the pipeline ------------------------------------------------

(ert-deftest read-file/every-level-reads-the-same-and-nobody-is-asked ()
  "Outside the workspace too: whether a file can be read is the operating
system's answer, under all three levels."
  (rf--with-dir dir
    (let ((file (rf--write (expand-file-name "outside.txt" dir) "RF_OUTSIDE_TEXT\n")))
      (dolist (level '(read-only workspace-write danger-full-access))
        (ert-info ((format "level: %s" level))
          (rf--fixture (list (rf--read-round file) '(:text "done"))
            (let* ((asked 0)
                   (superchat-approval-function (lambda (_request) (cl-incf asked) t))
                   (view (smoke--open-chat))
                   (session (buffer-local-value 'superchat--session-id view)))
              (superchat-permission-set session level)
              (rf--settle (smoke--send view "read it"))
              (should (= 0 asked))
              (should (cl-some (lambda (request)
                                 (string-match-p "RF_OUTSIDE_TEXT"
                                                 (plist-get request :prompt-text)))
                               smoke--requests))
              (should (= 1 (cl-count "tool.completed" (smoke--types session) :test #'equal)))
              (should (= 0 (cl-count "approval.requested" (smoke--types session)
                                     :test #'equal))))))))))

(ert-deftest read-file/a-listener-can-still-tighten-it ()
  "The tool asks for no approval itself; `tools/pre-execute' still applies."
  (rf--with-dir dir
    (let ((file (rf--write (expand-file-name "guarded.txt" dir) "RF_GUARDED_TEXT\n")))
      (dolist (kind '(deny ask))
        (ert-info ((format "listener: %s" kind))
          (rf--fixture (list (rf--read-round file) '(:text "done"))
            (let* ((asked nil)
                   (superchat-approval-function
                    (lambda (request) (push (plist-get request :tool-name) asked) nil))
                   (view (smoke--open-chat))
                   (off (superchat-on nil 'tools/pre-execute 'rf-tighten
                                      (lambda (call _next)
                                        (plist-put (plist-put call :kind kind)
                                                   :reason "rf says so")))))
              (unwind-protect (rf--settle (smoke--send view "read it"))
                (funcall off))
              (should (equal (if (eq kind 'ask) '("read_file") nil) asked))
              (should-not (cl-some (lambda (request)
                                     (string-match-p "RF_GUARDED_TEXT"
                                                     (plist-get request :prompt-text)))
                                   smoke--requests)))))))))

(ert-deftest read-file/it-is-a-built-in-on-the-default-surface-and-widens-nothing ()
  (rf--fixture '((:text "one") (:text "two"))
    (should (eq 'builtin (superchat-sandbox-tool-class "read_file")))
    (should (member "read_file" superchat-tools--builtin-tool-names))
    (should (equal '("read_buffer" "read_file")
                   (cl-remove-if-not (lambda (name) (member name '("read_buffer" "read_file")))
                                     superchat-llm-default-tool-names)))
    (should (equal superchat-llm-default-tool-names
                   (let (superchat-llm-default-tool-names)
                     (ignore superchat-llm-default-tool-names)
                     (makunbound 'superchat-llm-default-tool-names)
                     (superchat--select-tools-baseline-names))))
    (let ((view (smoke--open-chat)))
      (rf--settle (smoke--send view "hello"))
      (should (member "read_file" (plist-get (car smoke--requests) :tools)))
      ;; It carries no escalation arguments: it is not a write.
      (let ((tool (cl-find "read_file" (superchat-get-llm-tools '("read_file"))
                           :key #'llm-tool-name :test #'equal)))
        (should (equal '("path" "offset" "limit")
                       (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool)))))
      ;; An explicit list is exactly what it names.
      (let ((superchat-llm-tool-names '("read_buffer" "edit_buffer"))
            (superchat-llm-tools-list nil))
        (rf--settle (smoke--send view "again"))
        (should-not (member "read_file" (plist-get (car smoke--requests) :tools)))
        (should (member "read_buffer" (plist-get (car smoke--requests) :tools)))))))

(provide 'read-file-regression)

;;; read-file-regression.el ends here
