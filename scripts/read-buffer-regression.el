;;; read-buffer-regression.el --- read_buffer: a bounded window of a live buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-09-26-read-buffer-window-and-caps.md.  The
;; canonical smoke fixture supplies the Session and a scripted Run; no real
;; model and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/read-buffer-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^read-buffer/")'

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

(defmacro rb--with-buffer (var text &rest body)
  "Bind VAR to a fresh buffer holding TEXT; run BODY; kill it."
  (declare (indent 2) (debug t))
  `(let ((,var (generate-new-buffer "rb-probe")))
     (unwind-protect
         (progn (with-current-buffer ,var (insert ,text)) ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var)))))

(defun rb--read (buffer &rest args)
  (apply #'superchat-tool-read-buffer (buffer-name buffer) args))

(defun rb--header (result)
  "The header of RESULT: everything before the content marker."
  (should (stringp result))
  (should (string-match "\nContent:\n" result))
  (substring result 0 (match-beginning 0)))

(defun rb--body (result)
  "The content of RESULT, exactly as the tool returned it."
  (should (stringp result))
  (should (string-match "\nContent:\n" result))
  (substring result (match-end 0)))

(defun rb--numbered (from to)
  (mapconcat (lambda (n) (format "line %d" n)) (number-sequence from to) "\n"))

;;;; What it returns -----------------------------------------------------------

(ert-deftest read-buffer/returns-the-live-text-with-a-header-and-no-line-numbers ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((text "(setq a 1)\n;; 2: not a line number\n  indented\n"))
    (rb--with-buffer buffer text
      (let ((result (rb--read buffer)))
        (should (string-suffix-p text result))
        (should (equal text (rb--body result)))
        (let ((header (rb--header result)))
          (should (string-match-p (regexp-quote (format "Buffer: %S" (buffer-name buffer)))
                                  header))
          (should (string-match-p "Lines: 1-3 of 3" header))
          (should (string-match-p "complete buffer" header))
          (should-not (string-match-p "NOT" header))
          (should-not (string-match-p "Note:" header)))))))

(ert-deftest read-buffer/a-window-says-it-is-not-the-whole-buffer-and-how-to-go-on ()
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer (rb--numbered 1 10)
    (let ((first (rb--read buffer nil 4))
          (middle (rb--read buffer 5 4))
          (last (rb--read buffer 9)))
      (should (string-prefix-p "line 1\nline 2\nline 3\nline 4\n" (rb--body first)))
      (should (equal "line 1\nline 2\nline 3\nline 4\n" (rb--body first)))
      (should (string-match-p "Lines: 1-4 of 10" (rb--header first)))
      (should (string-match-p "NOT the complete buffer" (rb--header first)))
      (should (string-match-p "offset 5" (rb--header first)))
      (should (equal "line 5\nline 6\nline 7\nline 8\n" (rb--body middle)))
      ;; The last line has no newline, and none is invented.
      (should (equal "line 9\nline 10" (rb--body last)))
      (should (string-match-p "Lines: 9-10 of 10" (rb--header last)))
      (should (string-match-p "NOT the complete buffer" (rb--header last))))))

(ert-deftest read-buffer/more-than-the-line-limit-is-cut-at-2000-lines ()
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer (mapconcat #'number-to-string (number-sequence 1 2500) "\n")
    (let ((result (rb--read buffer)))
      (should (< (length result) (with-current-buffer buffer (buffer-size))))
      (should (= 2000 (length (split-string (rb--body result) "\n" t))))
      (should (string-match-p "Lines: 1-2000 of 2500" (rb--header result)))
      (should (string-match-p "NOT the complete buffer" (rb--header result)))
      (should (string-match-p "offset 2001" (rb--header result)))
      (should (equal "2001\n2002\n" (rb--body (rb--read buffer 2001 2)))))))

(ert-deftest read-buffer/a-long-line-is-cut-and-said-to-be-cut ()
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer (concat "short\n" (make-string 5000 ?x) "\nend\n")
    (let* ((result (rb--read buffer)))
      (should-not (string-match-p (make-string 2001 ?x) result))
      (let ((lines (split-string (rb--body result) "\n")))
        (should (equal "short" (nth 0 lines)))
        (should (equal (concat (make-string 2000 ?x) "... (line truncated to 2000 chars)")
                       (nth 1 lines)))
        (should (equal "end" (nth 2 lines))))
      (should (string-match-p "1 line.* truncated to 2000 characters" (rb--header result)))
      (should (string-match-p "NOT the complete buffer" (rb--header result))))))

(ert-deftest read-buffer/the-byte-limit-stops-at-a-line-boundary-and-says-so ()
  (skip-when (getenv "SMOKE_REAL"))
  (let ((line (make-string 999 ?b)))
    (rb--with-buffer buffer (concat (mapconcat (lambda (_) line) (number-sequence 1 100) "\n") "\n")
      (let* ((result (rb--read buffer)))
        (should (<= (string-bytes result) (+ (* 50 1024) 500)))
        (let* ((body (rb--body result)))
          (should (= 51 (length (split-string body "\n" t))))
          (should (string-suffix-p "\n" body))
          (should (string-match-p "Lines: 1-51 of 100" (rb--header result)))
          (should (string-match-p "NOT the complete buffer" (rb--header result)))
          (should (string-match-p "51200-byte limit" (rb--header result)))
          (should (string-match-p "offset 52" (rb--header result)))
          (should (= 49 (length (split-string (rb--body (rb--read buffer 52)) "\n" t)))))))))

(ert-deftest read-buffer/the-reported-large-buffer-comes-back-bounded ()
  "The issue's repro: 20,000 lines of 78 characters, about 1.5 MB."
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer
      (mapconcat (lambda (n) (format "%06d %s" n (make-string 71 ?a)))
                 (number-sequence 1 20000) "\n")
    (let ((result (rb--read buffer)))
      (should (> (with-current-buffer buffer (buffer-size)) 1500000))
      (should (<= (string-bytes result) (+ (* 50 1024) 500)))
      (should (string-match-p "of 20000 (NOT the complete buffer" (rb--header result)))
      (should (string-match-p "continue with offset" (rb--header result))))))

(ert-deftest read-buffer/empty-buffers-and-offsets-past-the-end-are-ordinary-results ()
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer empty ""
    (let ((result (rb--read empty)))
      (should (string-match-p "Lines: none (the buffer is empty)" (rb--header result)))
      (should (equal "" (rb--body result)))))
  (rb--with-buffer two "a\nb\n"
    (let ((result (rb--read two 7)))
      (should-not (string-match-p "a\nb" result))
      (should (equal "" (rb--body result)))
      (should (string-match-p "offset 7 is past the end" (rb--header result)))
      (should (string-match-p "2 lines" (rb--header result))))))

;;;; Narrowing and unsaved text -----------------------------------------------

(ert-deftest read-buffer/a-narrowed-buffer-is-read-in-its-narrowed-part-and-says-so ()
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer (concat (rb--numbered 1 10) "\n")
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line 2)
      (let ((start (point)))
        (forward-line 3)
        (narrow-to-region start (point))))
    (let* ((restriction (with-current-buffer buffer (cons (point-min) (point-max))))
           (result (rb--read buffer))
           (header (rb--header result)))
      ;; What it reads is what edit_buffer can edit: the narrowed part.
      (should (equal "line 3\nline 4\nline 5\n" (rb--body result)))
      (should (string-match-p "Lines: 1-3 of 3" header))
      (should (string-match-p "NOT the complete buffer" header))
      (should (string-match-p "narrowed to lines 3-5 of 10" header))
      ;; Reading does not widen: the user's narrowing is left as it was.
      (should (equal restriction
                     (with-current-buffer buffer (cons (point-min) (point-max))))))))

(ert-deftest read-buffer/a-file-buffer-with-unsaved-changes-says-its-text-is-unsaved ()
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "rb-" t))))
         (file (expand-file-name "notes.txt" dir))
         buffer)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "on disk\n" nil file nil 'silent))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "unsaved\n"))
          (let ((result (rb--read buffer)))
            (should (string-suffix-p "on disk\nunsaved\n" result))
            (should (equal "on disk\nunsaved\n" (rb--body result)))
            (should (string-match-p (regexp-quote file) (rb--header result)))
            (should (string-match-p "unsaved" (rb--header result))))
          ;; Nothing is saved by reading.
          (should (buffer-modified-p buffer))
          (should (equal "on disk\n"
                         (with-temp-buffer (insert-file-contents file) (buffer-string))))
          ;; Saved text is not worth a note.
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (should-not (string-match-p "unsaved" (rb--header (rb--read buffer))))
          ;; A modified buffer that visits no file has nothing on disk to differ from.
          (rb--with-buffer scratch "typed\n"
            (should (with-current-buffer scratch (buffer-modified-p)))
            (should-not (string-match-p "Note:" (rb--header (rb--read scratch))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory dir t))))

;;;; Arguments, errors and guards ----------------------------------------------

(ert-deftest read-buffer/bad-arguments-and-a-dead-buffer-are-errors ()
  (skip-when (getenv "SMOKE_REAL"))
  (should-error (superchat-tool-read-buffer "rb-no-such-buffer-anywhere"))
  (rb--with-buffer buffer "a\nb\n"
    ;; Still true of the current tool: a good name reads.
    (should (string-match-p "a\nb\n" (rb--read buffer)))
    (should (string-match-p "a\nb" (rb--read buffer 1.0 "2")))
    (should-error (rb--read buffer 0))
    (should-error (rb--read buffer nil 2001))
    (should-error (rb--read buffer "two"))
    (should-error (rb--read buffer nil -1))))

(ert-deftest read-buffer/a-field-does-not-cut-a-line-short ()
  "Guard: a prompt field (as comint or eshell leave) does not end a line."
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer ""
    (with-current-buffer buffer
      (insert (propertize "prompt> " 'field 'output 'rear-nonsticky t))
      (insert "typed input\nnext\n"))
    (let ((result (rb--read buffer)))
      (should (string-match-p "^prompt> typed input\nnext\n\\'" result))
      (should-not (text-properties-at 0 result)))))

(ert-deftest read-buffer/the-schema-offers-offset-and-limit-and-describes-the-header ()
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((superchat-llm-tools-list nil)
         (tool (cl-find "read_buffer" (superchat-get-llm-tools '("read_buffer"))
                        :key #'llm-tool-name :test #'equal)))
    (should tool)
    (should (eq #'superchat-tool-read-buffer (llm-tool-function tool)))
    (should (equal '("buffer" "offset" "limit")
                   (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool))))
    (should (equal '(nil t t)
                   (mapcar (lambda (arg) (plist-get arg :optional)) (llm-tool-args tool))))
    (let ((description (llm-tool-description tool)))
      (should (string-match-p "offset" description))
      (should (string-match-p "2000 lines" description))
      (should (string-match-p "narrowed" description))
      (should (string-match-p "no line numbers" description)))))

(ert-deftest read-buffer/read-file-output-is-unchanged ()
  "Guard: the shared window leaves `read_file' byte-identical."
  (skip-when (getenv "SMOKE_REAL"))
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "rb-" t))))
         (file (expand-file-name "f.txt" dir)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (concat (mapconcat (lambda (n) (format "l%d" n))
                                             (number-sequence 1 5) "\n")
                                  "\n" (make-string 2003 ?y) "\nz")
                          nil file nil 'silent))
          (should (equal (concat "File: " file "\n"
                                 "Lines: 1-7 of 7 (NOT the complete file: 1 line was truncated to 2000 characters)\n"
                                 "Content:\nl1\nl2\nl3\nl4\nl5\n" (make-string 2000 ?y)
                                 "... (line truncated to 2000 chars)\nz")
                         (superchat-tool-read-file file)))
          (should (equal (concat "File: " file "\n"
                                 "Lines: 2-3 of 7 (NOT the complete file: it starts at line 2; more lines follow; continue with offset 4)\n"
                                 "Content:\nl2\nl3\n")
                         (superchat-tool-read-file file 2 2)))
          (should (equal (concat "File: " file "\n"
                                 "Lines: 6-7 of 7 (NOT the complete file: it starts at line 6; 1 line was truncated to 2000 characters)\n"
                                 "Content:\n" (make-string 2000 ?y)
                                 "... (line truncated to 2000 chars)\nz")
                         (superchat-tool-read-file file 6))))
      (delete-directory dir t))))

(ert-deftest read-buffer/what-the-model-receives-is-the-bounded-result ()
  "Through a scripted Run: the next request carries the header, not the buffer."
  (skip-when (getenv "SMOKE_REAL"))
  (rb--with-buffer buffer
      (mapconcat (lambda (n) (format "%06d %s" n (make-string 71 ?q)))
                 (number-sequence 1 20000) "\n")
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-event-faults nil)
          (superchat-llm-reasoning nil)
          (superchat-llm-tool-names superchat-llm-default-tool-names)
          (superchat-llm-tools-list nil))
      (unwind-protect
          (smoke--with-fixture
              (list (list :tool-uses `(("read_buffer" (buffer . ,(buffer-name buffer)))))
                    (list :text "read it"))
            (let* ((view (smoke--open-chat))
                   (run (smoke--send view "read the probe")))
              (should (smoke--wait
                       (lambda () (superchat-runtime--terminal-p
                                   (plist-get (superchat-runtime-status run) :status)))
                       30))
              ;; Whole, the result overflows the context window and the
              ;; next request is refused before it is sent.
              (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
              (should (= 2 (length smoke--requests)))
              (let ((prompt (plist-get (car smoke--requests) :prompt-text)))
                (should (string-match-p "000001 q" prompt))
                (should (string-match-p "of 20000 (NOT the complete buffer" prompt))
                (should-not (string-match-p "020000 q" prompt))
                (should (< (length prompt) 200000)))))
        (setq superchat-llm-tools-list nil)))))

(provide 'read-buffer-regression)

;;; read-buffer-regression.el ends here
