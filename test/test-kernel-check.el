;;; test-kernel-check.el --- ERT tests for the Kernel citation checker -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(unless (featurep 'kernel-check)
  (load (expand-file-name
         "kernel-check.el"
         (file-name-directory
          (or load-file-name
              (expand-file-name "test/test-kernel-check.el"
                                default-directory))))
        nil nil t))

(defun kernel-check-test--write (file content)
  "Write CONTENT to FILE and return FILE."
  (with-temp-file file
    (insert content))
  file)

(defun kernel-check-test--read (file)
  "Read FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest kernel-check/construct-names-use-real-elisp-forms ()
  "Construct names come from parsed fixture forms and not indentation."
  (let* ((directory (make-temp-file "kernel-check-construct-" t))
         (source (expand-file-name "fixture.el" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           (concat
            ";; file header\n"
            ";;;###autoload\n"
            "(defun fixture-function (value)\n"
            "  value)\n"
            "\n"
            "(defcustom fixture-option nil\n"
            "  \"An option.\"\n"
            "  :type 'boolean)\n"
            "\n"
            "(cl-defstruct fixture-record\n"
            "  field)\n"
            "\n"
            "(ert-deftest fixture-test ()\n"
            "  (should t))\n"
            "\n"
            "(add-hook 'fixture-hook #'ignore)\n"
            "(define-error 'foo-err \"d\")\n"))
          (should-not (kernel-check--construct-at source 1))
          (should-not (kernel-check--construct-at source 2))
          (should (equal "fixture-function"
                         (kernel-check--construct-at source 3)))
          (should (equal "fixture-function"
                         (kernel-check--construct-at source 4)))
          (should-not (kernel-check--construct-at source 5))
          (should (equal "fixture-option"
                         (kernel-check--construct-at source 6)))
          (should (equal "fixture-option"
                         (kernel-check--construct-at source 7)))
          (should (equal "fixture-record"
                         (kernel-check--construct-at source 11)))
          (should (equal "fixture-test"
                         (kernel-check--construct-at source 14)))
          (should (equal "add-hook/fixture-hook"
                         (kernel-check--construct-at source 16)))
          (should (equal "foo-err"
                         (kernel-check--construct-at source 17))))
      (delete-directory directory t))))

(ert-deftest kernel-check/parser-handles-full-short-range-and-markdown ()
  "Citation parsing carries files across short and range endpoints."
  (let* ((directory (make-temp-file "kernel-check-parse-" t))
         (kernel (expand-file-name "KERNEL.md" directory))
         (references
          (progn
            (kernel-check-test--write
             kernel
             (concat
              "### First entry\n"
              "source: `fixture.el:1@one`-`:2`; `:3@three`; `:5-6`; `README.md:1`\n"
              "## End\n"))
            (kernel-check--parse-references kernel))))
    (unwind-protect
        (progn
          (should (= 6 (length references)))
          (should (equal "fixture.el" (plist-get (nth 0 references) :file)))
          (should (= 1 (plist-get (nth 0 references) :line)))
          (should (equal "one" (plist-get (nth 0 references) :construct)))
          (should (equal "fixture.el" (plist-get (nth 1 references) :file)))
          (should (= 2 (plist-get (nth 1 references) :line)))
          (should-not (plist-get (nth 1 references) :construct))
          (should (equal "fixture.el" (plist-get (nth 2 references) :file)))
          (should (= 3 (plist-get (nth 2 references) :line)))
          (should (equal "three" (plist-get (nth 2 references) :construct)))
          (should (equal "fixture.el" (plist-get (nth 3 references) :file)))
          (should (= 5 (plist-get (nth 3 references) :line)))
          (should-not (plist-get (nth 3 references) :construct))
          (should (equal "fixture.el" (plist-get (nth 4 references) :file)))
          (should (= 6 (plist-get (nth 4 references) :line)))
          (should-not (plist-get (nth 4 references) :construct))
          (should (equal "README.md" (plist-get (nth 5 references) :file)))
          (should (= 1 (plist-get (nth 5 references) :line)))
          (should (equal "First entry"
                         (plist-get (car references) :entry-name))))
      (delete-directory directory t))))

(ert-deftest kernel-check/short-citation-never-switches-files ()
  "A bad inherited file is not silently replaced by another entry file."
  (let* ((directory (make-temp-file "kernel-check-no-fallback-" t))
         (primary (expand-file-name "primary.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write primary "(defun primary () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### no fallback\n"
            "source: `primary.el:1@primary`; `missing.el:1`; `:1@primary`\n"))
          (let* ((references (kernel-check--parse-references kernel))
                 (short (nth 2 references))
                 (location (kernel-check--reference-location short directory))
                 (result (kernel-check-check kernel nil directory)))
            (should (= 3 (length references)))
            (should (equal "missing.el" (plist-get short :file)))
            (should (eq 'missing (plist-get location :status)))
            (should (= 2 (length (plist-get result :errors))))
            (should
             (cl-some (lambda (message)
                        (string-match-p "missing\\.el:1@primary" message))
                      (plist-get result :errors)))))
      (delete-directory directory t))))

(ert-deftest kernel-check/single-span-ranges-check-both-endpoints ()
  "Single-span range endpoints are counted, checked, and converted."
  (let* ((directory (make-temp-file "kernel-check-range-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (converted (expand-file-name "K4.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           (concat
            "(defun first () nil)\n"
            "(defun second () nil)\n"
            "(defun third () nil)\n"))
          (kernel-check-test--write
           kernel
           (concat
            "### ranges\n"
            "source: `fixture.el:1-2`; `fixture.el:1,2-3`; `:1-2`\n"))
          (let ((references (kernel-check--parse-references kernel)))
            (should (= 7 (length references)))
            (should (= 1 (plist-get (nth 0 references) :line)))
            (should (= 2 (plist-get (nth 1 references) :line)))
            (should (= 1 (plist-get (nth 2 references) :line)))
            (should (= 2 (plist-get (nth 3 references) :line)))
            (should (= 3 (plist-get (nth 4 references) :line)))
            (should (= 1 (plist-get (nth 5 references) :line)))
            (should (= 2 (plist-get (nth 6 references) :line))))
          (let ((result (kernel-check-convert kernel converted directory)))
            (should (= 7 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should
           (equal
            "### ranges\nsource: `fixture.el:1@first-2@second`; `fixture.el:1@first,2@second-3@third`; `:1@first-2@second`\n"
            (kernel-check-test--read converted)))
          (let ((result (kernel-check-check converted nil directory)))
            (should (= 7 (length (plist-get result :references))))
            (should-not (plist-get result :errors))))
      (delete-directory directory t))))

(ert-deftest kernel-check/blank-regexp-does-not-classify-r-as-blank ()
  "The blank-line predicate accepts whitespace but not the letter r."
  (let* ((directory (make-temp-file "kernel-check-blank-" t))
         (source (expand-file-name "fixture.el" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "r\n \t\n")
          (should (eq 'ok
                      (plist-get (kernel-check--source-line source 1)
                                 :status)))
          (should (eq 'blank
                      (plist-get (kernel-check--source-line source 2)
                                 :status))))
      (delete-directory directory t))))

(ert-deftest kernel-check/sentence-splitter-protects-code-spans ()
  "Periods inside Markdown code spans do not terminate sentences."
  (should
   (equal
    '("Only `module.el:1`" " Every claim" " never skip")
    (kernel-check--split-sentences
     "Only `module.el:1`. Every claim; never skip."))))

(ert-deftest kernel-check/reader-failure-is-reported-once ()
  "A malformed source file produces one parse diagnostic, not nil floods."
  (let* ((directory (make-temp-file "kernel-check-reader-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           "(defun fixture () nil)\n(\n")
          (kernel-check-test--write
           kernel
           (concat
            "### malformed\n"
            "source: `fixture.el:1@fixture`; `fixture.el:1@fixture`; `fixture.el:1@fixture`\n"))
          (let ((result (kernel-check-check kernel nil directory)))
            (should (= 1 (length (plist-get result :errors))))
            (should (string-match-p "reader failed" (car (plist-get result :errors))))
            (should-not (string-match-p "actual nil"
                                        (car (plist-get result :errors))))))
      (delete-directory directory t))))

(ert-deftest kernel-check/check-reports-construct-range-blank-and-bare-cases ()
  "Check mode distinguishes construct drift, range errors, blanks, and bare refs."
  (let* ((directory (make-temp-file "kernel-check-check-" t))
         (source (expand-file-name "fixture.el" directory))
         (markdown (expand-file-name "README.md" directory))
         (kernel (expand-file-name "KERNEL.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun fixture-good ()\n  t)\n\n")
          (kernel-check-test--write markdown "markdown\n")
          (kernel-check-test--write
           kernel
           (concat
            "### mismatch\nsource: `fixture.el:2@wrong-name`\n"
            "### out of range\nsource: `fixture.el:999@fixture-good`\n"
            "### blank\nsource: `fixture.el:3@fixture-good`\n"
            "### bare\nsource: `fixture.el:2`\n"
            "### markdown\nsource: `README.md:1`\n"
            "## End\n"))
          (let ((result (kernel-check-check kernel nil directory)))
            (should (= 5 (length (plist-get result :references))))
            (should (= 3 (length (plist-get result :errors))))
            (should (= 1 (length (plist-get result :warnings))))
            (should (= 3 (plist-get result :error-count)))
            (should (= 1 (plist-get result :exit-code)))
            (should
             (cl-some (lambda (message)
                        (and (string-match-p "expected construct wrong-name" message)
                             (string-match-p "actual fixture-good" message)
                             (string-match-p "  t)" message)))
                      (plist-get result :errors)))
            (should
             (cl-some (lambda (message)
                        (string-match-p "out of range" message))
                      (plist-get result :errors)))
            (should
             (cl-some (lambda (message)
                        (string-match-p "blank" message))
                      (plist-get result :errors)))
            (should (string-match-p "bare" (car (plist-get result :warnings))))))
      (delete-directory directory t))))

(ert-deftest kernel-check/converter-is-idempotent-and-byte-local ()
  "Conversion inserts only construct annotations and is idempotent."
  (let* ((directory (make-temp-file "kernel-check-convert-" t))
         (source (expand-file-name "fixture.el" directory))
         (markdown (expand-file-name "README.md" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (converted (expand-file-name "K4.md" directory))
         (converted-again (expand-file-name "K4b.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           "(defun fixture-one ()\n  t)\n\n(add-hook 'fixture-hook #'ignore)\n")
          (kernel-check-test--write markdown "read me\n")
          (kernel-check-test--write
           kernel
           (concat
            "# Keep this\n"
            "since: K1\n"
            "source:  `fixture.el:2`  and `:4`; `README.md:1`\n"
            "note: 保留原始字节\n"))
          (let ((result (kernel-check-convert kernel converted directory)))
            (should (= 2 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should
           (equal
            (concat
             "# Keep this\n"
             "since: K1\n"
             "source:  `fixture.el:2@fixture-one`  and `:4@add-hook/fixture-hook`; `README.md:1`\n"
             "note: 保留原始字节\n")
            (kernel-check-test--read converted)))
          (let ((result (kernel-check-convert converted converted-again directory)))
            (should (= 0 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should (equal (kernel-check-test--read converted)
                         (kernel-check-test--read converted-again))))
      (delete-directory directory t))))

(ert-deftest kernel-check/converter-roundtrips-digit-suffixed-construct ()
  "A construct ending in digits survives conversion and re-conversion."
  (let* ((directory (make-temp-file "kernel-check-digit-construct-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (converted (expand-file-name "K4.md" directory))
         (converted-again (expand-file-name "K4b.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun foo-1 () nil)\n")
          (kernel-check-test--write
           kernel
           "### numeric construct\nsource: `fixture.el:1`\n")
          (let ((result (kernel-check-convert kernel converted directory)))
            (should (= 1 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should
           (equal
            "### numeric construct\nsource: `fixture.el:1@foo-1`\n"
            (kernel-check-test--read converted)))
          (let ((result (kernel-check-check converted nil directory)))
            (should (= 1 (length (plist-get result :references))))
            (should-not (plist-get result :errors))
            (should (= 0 (plist-get result :exit-code))))
          (let ((result
                 (kernel-check-convert converted converted-again directory)))
            (should (= 0 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should (equal (kernel-check-test--read converted)
                         (kernel-check-test--read converted-again))))
      (delete-directory directory t))))

(ert-deftest kernel-check/check-catches-drift-in-a-scratch-source-copy ()
  "A one-line source shift makes an annotated citation fail by construct."
  (let* ((directory (make-temp-file "kernel-check-drift-" t))
         (scratch (make-temp-file "kernel-check-drift-scratch-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (scratch-source (expand-file-name "fixture.el" scratch))
         (scratch-kernel (expand-file-name "KERNEL.md" scratch)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           "(defun fixture-first () nil)\n(defun fixture-good ()\n  t)\n")
          (kernel-check-test--write
           kernel
           "### drift\nsource: `fixture.el:2@fixture-good`\n")
          (copy-file source scratch-source)
          (copy-file kernel scratch-kernel)
          (with-temp-buffer
            (insert-file-contents scratch-source)
            (goto-char (point-min))
            (forward-line 1)
            (insert "(defun inserted () nil)\n")
            (write-region (point-min) (point-max) scratch-source nil 'silent))
          (let ((result (kernel-check-check scratch-kernel nil scratch)))
            (should (= 1 (length (plist-get result :errors))))
            (should (= 1 (plist-get result :exit-code)))
            (should (string-match-p "expected construct fixture-good"
                                    (car (plist-get result :errors))))
            (should (string-match-p "actual inserted"
                                    (car (plist-get result :errors))))))
      (delete-directory directory t)
      (delete-directory scratch t))))

(ert-deftest kernel-check/pairs-does-not-share-reserved-bare-citations ()
  "Reserved bare annotations fall back to line-specific pairing keys."
  (let* ((directory (make-temp-file "kernel-check-reserved-pairs-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           "(defun first () nil)\n(defun second () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### First\n"
            "source: `fixture.el:1@-`\n"
            "### Second\n"
            "source: `fixture.el:2@-`\n"
            "## End\n"))
          (let ((result (kernel-check-pairs kernel output directory)))
            (should (= 2 (length (plist-get result :group-order))))
            (should-not (gethash "fixture.el@-" (plist-get result :groups)))
            (should (equal '("First")
                           (gethash "fixture.el:1" (plist-get result :groups))))
            (should (equal '("Second")
                           (gethash "fixture.el:2" (plist-get result :groups))))))
      (delete-directory directory t))))

(ert-deftest kernel-check/pairs-group-by-construct-and-list-universal-sentences ()
  "Pairing groups shared constructs and emits universal-quantifier sentences."
  (let* ((directory (make-temp-file "kernel-check-pairs-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           (concat
            "(defun shared () nil)\n"
            "(add-hook 'fixture-hook #'ignore)\n"
            "(defun shared () nil)\n"
            "(defun solo () nil)\n"))
          (kernel-check-test--write
           kernel
           (concat
            "### First\n"
            "source: `fixture.el:1@shared`; `fixture.el:2`\n"
            "The only shared fact is every exact reference.\n"
            "### Second\n"
            "source: `fixture.el:3@shared`; `:2`\n"
            "### Third\n"
            "source: `fixture.el:4@solo`\n"
            "## End\n"))
          (let ((result (kernel-check-pairs kernel output directory)))
            (should (= 3 (length (plist-get result :entries))))
            (should (>= (length (plist-get result :group-order)) 3))
            (should (= 1 (length (plist-get result :universal)))))
          (let ((text (kernel-check-test--read output)))
            (should (string-match-p "fixture\\.el@shared.*Second" text))
            (should (string-match-p "fixture\\.el@add-hook/fixture-hook.*Second"
                                    text))
            (should (string-match-p "Universal-quantifier sentences (1)" text))
            (should (string-match-p "The only shared fact is every exact reference"
                                    text))))
      (delete-directory directory t))))

(ert-deftest kernel-check/unknown-command-without-double-dash-returns-usage ()
  "A typoed command without `--' must fail with usage and status two."
  (let* ((emacs (or (executable-find invocation-name)
                    (executable-find "emacs")))
         (source (or (locate-library "kernel-check")
                     (symbol-file 'kernel-check--run-command-line 'defun)))
         (root (and source (file-name-directory source))))
    (unless (and emacs root)
      (ert-skip "Cannot find the current Emacs or kernel-check source"))
    (with-temp-buffer
      (let ((status
             (call-process emacs nil t nil
                           "-Q" "--batch" "-L" root
                           "-l" source
                           "chek" "KERNEL.md")))
        (should (= 2 status))
        (should (string-match-p "usage: check|convert|pairs"
                                (buffer-string)))))))

(ert-deftest kernel-check/converter-leaves-minus-construct-bare-and-checkable ()
  "A top-level minus form is not emitted as the reserved `@-' annotation."
  (let* ((directory (make-temp-file "kernel-check-minus-convert-" t))
         (source (expand-file-name "minus.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (converted (expand-file-name "K5.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(- 1\n 2)\n")
          (kernel-check-test--write
           kernel
           "### reserved minus\nsource: `minus.el:1`\n")
          (let ((result (kernel-check-convert kernel converted directory)))
            (should (= 0 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should (= 1 (length (plist-get result :warnings))))
            (should (string-match-p "reserved.*left unchanged"
                                    (car (plist-get result :warnings)))))
          (should (equal (kernel-check-test--read kernel)
                         (kernel-check-test--read converted)))
          (let ((result (kernel-check-check converted nil directory)))
            (should-not (plist-get result :errors))
            (should (= 1 (length (plist-get result :warnings))))
            (should (= 0 (plist-get result :exit-code)))))
      (delete-directory directory t))))

(ert-deftest kernel-check/intentional-bare-citations-validate-and-convert ()
  "Intentional bare citations accept nil and remain stable during conversion."
  (let* ((directory (make-temp-file "kernel-check-intentional-bare-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (converted (expand-file-name "K5.md" directory))
         (bare-kernel (expand-file-name "bare.md" directory))
         (bare-converted (expand-file-name "bare-converted.md" directory))
         (invalid-kernel (expand-file-name "invalid.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           (concat
            ";; header\n"
            "(defun real-form ()\n"
            "  nil)\n"))
          (kernel-check-test--write
           kernel
           (concat
            "### intentional\n"
            "source: `fixture.el:1@-`\n"))
          (let ((reference (car (kernel-check--parse-references kernel))))
            (should (equal "-" (plist-get reference :construct))))
          (let ((result (kernel-check-check kernel nil directory)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings))
            (should (= 0 (plist-get result :exit-code))))
          (let ((result (kernel-check-convert kernel converted directory)))
            (should (= 0 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (should (equal (kernel-check-test--read kernel)
                         (kernel-check-test--read converted)))
          (kernel-check-test--write
           bare-kernel
           (concat
            "### unannotated nil\n"
            "source: `fixture.el:1`\n"))
          (let ((result (kernel-check-check bare-kernel nil directory)))
            (should-not (plist-get result :errors))
            (should (= 1 (length (plist-get result :warnings)))))
          (let ((result (kernel-check-convert
                         bare-kernel bare-converted directory)))
            (should (= 0 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should (= 1 (length (plist-get result :warnings)))))
          (should (equal (kernel-check-test--read bare-kernel)
                         (kernel-check-test--read bare-converted)))
          (kernel-check-test--write
           invalid-kernel
           (concat
            "### invalid intentional bare\n"
            "source: `fixture.el:3@-`\n"))
          (let ((result (kernel-check-check invalid-kernel nil directory)))
            (should (= 1 (length (plist-get result :errors))))
            (should-not (plist-get result :warnings))
            (should (string-match-p
                     "declared bare"
                     (car (plist-get result :errors))))
            (should (string-match-p
                     "real-form"
                     (car (plist-get result :errors))))))
      (delete-directory directory t))))

(ert-deftest kernel-check/pairs-preserves-duplicate-narrowing-sentences ()
  "A repeated narrowing sentence remains attached twice."
  (let* ((directory (make-temp-file "kernel-check-duplicate-narrowing-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun shared () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### Duplicate narrowing\n"
            "every path is checked. outside this case. outside this case.\n"
            "source: `fixture.el:1@shared`\n"
            "## End\n"))
          (let* ((result (kernel-check-pairs kernel output directory))
                 (text (kernel-check-test--read output))
                 (count 0)
                 (start 0))
            (while (string-match
                    (regexp-quote "  - narrowed by: outside this case")
                    text start)
              (setq count (1+ count)
                    start (match-end 0)))
            (should (= 1 (plist-get result :narrowed)))
            (should (= 1 (plist-get result :narrowed-entries)))
            (should (= 2 count))))
      (delete-directory directory t))))

(ert-deftest kernel-check/pairs-self-exclusion-uses-sentence-position ()
  "Equal universal and narrowing sentences exclude only their own position."
  (let* ((directory (make-temp-file "kernel-check-equal-sentences-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun shared () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### Equal sentences\n"
            "every path is checked, but not all. every path is checked, but not all.\n"
            "source: `fixture.el:1@shared`\n"
            "## End\n"))
          (let* ((result (kernel-check-pairs kernel output directory))
                 (text (kernel-check-test--read output))
                 (count 0)
                 (start 0))
            (while (string-match
                    (regexp-quote
                     "  - narrowed by: every path is checked, but not all")
                    text start)
              (setq count (1+ count)
                    start (match-end 0)))
            (should (= 2 (plist-get result :narrowed)))
            (should (= 1 (plist-get result :narrowed-entries)))
            (should (= 2 count))))
      (delete-directory directory t))))

(ert-deftest kernel-check/pairs-single-sentence-does-not-self-narrow ()
  "A sentence that matches both vocabularies cannot narrow itself."
  (let* ((directory (make-temp-file "kernel-check-single-narrowing-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun shared () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### Solo\n"
            "every path is checked, but not all\n"
            "source: `fixture.el:1@shared`\n"
            "## End\n"))
          (let* ((result (kernel-check-pairs kernel output directory))
                 (text (kernel-check-test--read output)))
            (should (= 1 (length (plist-get result :universal))))
            (should (= 0 (plist-get result :narrowed)))
            (should (= 0 (plist-get result :narrowed-entries)))
            (should-not (string-match-p "narrowed by:" text))))
      (delete-directory directory t))))

(ert-deftest kernel-check/malformed-annotated-citations-are-errors ()
  "Citation-looking spans with broken annotations are counted as errors."
  (let* ((directory (make-temp-file "kernel-check-malformed-citation-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun fixture () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### malformed\n"
            "source: `fixture.el:1@`; `fixture.el:1@@-`\n"
            "## End\n"))
          (let ((result (kernel-check-check kernel nil directory)))
            (should (= 0 (length (plist-get result :references))))
            (should (= 2 (length (plist-get result :errors))))
            (should (= 2 (plist-get result :error-count)))
            (should (= 0 (length (plist-get result :warnings))))
            (should (cl-every (lambda (message)
                                (string-match-p "malformed citation" message))
                              (plist-get result :errors)))
            (should (cl-some (lambda (message)
                               (string-match-p "`fixture\\.el:1@`" message))
                             (plist-get result :errors)))
            (should (cl-some (lambda (message)
                               (string-match-p "`fixture\\.el:1@@-`" message))
                             (plist-get result :errors)))))
      (delete-directory directory t))))

(ert-deftest kernel-check/construct-markers-on-non-el-files-are-errors ()
  "Construct annotations are restricted to Emacs Lisp citations."
  (let* ((directory (make-temp-file "kernel-check-non-el-marker-" t))
         (notes (expand-file-name "notes.md" directory))
         (script (expand-file-name "run.sh" directory))
         (kernel (expand-file-name "KERNEL.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write notes "notes\n")
          (kernel-check-test--write script "#!/bin/sh\ntrue\n")
          (kernel-check-test--write
           kernel
           (concat
            "### Markdown\n"
            "source: `notes.md:1@-`\n"
            "### Shell\n"
            "source: `run.sh:1@run`\n"
            "## End\n"))
          (let ((result (kernel-check-check kernel nil directory)))
            (should (= 2 (length (plist-get result :references))))
            (should (= 2 (length (plist-get result :errors))))
            (should (= 0 (length (plist-get result :warnings))))
            (should (cl-every
                     (lambda (message)
                       (string-match-p
                        "construct marker @.*only valid for \\.el citations"
                        message))
                     (plist-get result :errors)))))
      (delete-directory directory t))))

(ert-deftest kernel-check/universal-sentence-matching-is-case-insensitive ()
  "Universal vocabulary is recognized even with case-folding disabled outside."
  (with-temp-buffer
    (insert "### Upper\nEvery path is checked.\n")
    (let ((case-fold-search nil))
      (should (= 1
                 (length
                  (kernel-check--universal-sentences
                   (car (kernel-check--document-entries)))))))))

(ert-deftest kernel-check/pairs-attaches-narrowing-sentences ()
  "Pairing attaches same-entry narrowing sentences and reports their count."
  (let* ((directory (make-temp-file "kernel-check-narrowing-" t))
         (source (expand-file-name "fixture.el" directory))
         (kernel (expand-file-name "KERNEL.md" directory))
         (output (expand-file-name "pairs.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write source "(defun shared () nil)\n")
          (kernel-check-test--write
           kernel
           (concat
            "### Plain\n"
            "every path remains visible.\n"
            "source: `fixture.el:1@shared`\n"
            "### Narrowed\n"
            "every path is checked. two narrow paths, not every path are checked.\n"
            "source: `fixture.el:1@shared`\n"
            "## End\n"))
          (let* ((result (kernel-check-pairs kernel output directory))
                 (text (kernel-check-test--read output))
                 (narrowed-position
                  (string-match
                   (regexp-quote "- **Narrowed**: every path is checked")
                   text))
                 (plain-position
                  (string-match
                   (regexp-quote "- **Plain**: every path remains visible")
                   text)))
            (should (= 3 (length (plist-get result :universal))))
            (should (= 1 (plist-get result :narrowed)))
            (should (= 1 (plist-get result :narrowed-entries)))
            (should narrowed-position)
            (should plain-position)
            (should (< narrowed-position plain-position))
            (should
             (string-match-p
              (regexp-quote
               "  - narrowed by: two narrow paths, not every path are checked")
              text))
            (should-not
             (string-match-p
              (regexp-quote
               "- **Narrowed**: two narrow paths, not every path are checked\n  - narrowed by: two narrow paths, not every path are checked")
              text)))
          (dolist (term
                   '("not every" "not all" "but not" "except" "unless"
                     "only when" "only if" "rather than" "outside"
                     "narrow" "narrows" "narrowed" "narrowing"
                     "narrower" "narrowest" "no longer"))
            (should (string-match-p
                     kernel-check--narrowing-term-regexp
                     term))))
      (delete-directory directory t))))

(ert-deftest kernel-check/non-symbol-top-level-objects-get-type-names ()
  "Non-symbol-headed top-level objects expose their Lisp type names."
  (let* ((directory (make-temp-file "kernel-check-top-level-object-" t))
         (source (expand-file-name "fixture.el" directory))
         (marked-kernel (expand-file-name "marked.md" directory))
         (bare-kernel (expand-file-name "bare.md" directory))
         (converted (expand-file-name "converted.md" directory)))
    (unwind-protect
        (progn
          (kernel-check-test--write
           source
           (concat
            "((lambda () nil))\n"
            "\"literal\"\n"
            "[1 2]\n"))
          (should (equal "cons" (kernel-check--construct-at source 1)))
          (should (equal "string" (kernel-check--construct-at source 2)))
          (should (equal "vector" (kernel-check--construct-at source 3)))
          (kernel-check-test--write
           marked-kernel
           (concat
            "### marked\n"
            "source: `fixture.el:1@-`; `fixture.el:2@-`; `fixture.el:3@-`\n"))
          (let ((result (kernel-check-check marked-kernel nil directory)))
            (should (= 3 (length (plist-get result :errors))))
            (should (= 3 (length (cl-remove-if-not
                                  (lambda (message)
                                    (string-match-p "declared bare" message))
                                  (plist-get result :errors)))))
            (dolist (construct '("cons" "string" "vector"))
              (should
               (cl-some (lambda (message)
                          (string-match-p
                           (regexp-quote
                            (format "actual construct %s" construct))
                           message))
                        (plist-get result :errors)))))
          (kernel-check-test--write
           bare-kernel
           (concat
            "### bare\n"
            "source: `fixture.el:1`; `fixture.el:2`; `fixture.el:3`\n"))
          (let ((result (kernel-check-convert bare-kernel converted directory)))
            (should (= 3 (plist-get result :inserted)))
            (should-not (plist-get result :errors))
            (should-not (plist-get result :warnings)))
          (let ((text (kernel-check-test--read converted)))
            (should (string-match-p "`fixture\\.el:1@cons`" text))
            (should (string-match-p "`fixture\\.el:2@string`" text))
            (should (string-match-p "`fixture\\.el:3@vector`" text))))
      (delete-directory directory t))))

;;; test-kernel-check.el ends here
