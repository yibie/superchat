;;; note-regression.el --- your notes on the text, handed over together -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-11-06-your-notes-on-the-text.md:
;; you write notes on pieces of text in any buffer, the text unchanged; they are
;; kept by file and come back; `superchat-note-attach' puts them all on the
;; message you write, above where you write, following them until it goes; what
;; went is done.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/note-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^note/")'

;;; Code:

(load (expand-file-name "bring-in-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-note)

(defmacro note--with (script vars &rest body)
  "BODY in the bring-in fixture, SCRIPT and VARS as there, with no notes yet."
  (declare (indent 2) (debug t))
  (let ((file (make-symbol "file")))
    `(let* ((,file (make-temp-file "superchat-notes-" nil ".eld"))
            (superchat-note-file ,file)
            (superchat-note--kept 'unread))
       (delete-file ,file)
       (unwind-protect
           (bring--with ,script ,vars ,@body)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (mapc #'delete-overlay (superchat-note--overlays))))
         (when (get-buffer superchat-note--edit-buffer)
           (kill-buffer superchat-note--edit-buffer))
         (ignore-errors (delete-file ,file))))))

(defun note--file (root name content)
  "A buffer visiting ROOT/NAME, which holds CONTENT."
  (make-directory (file-name-directory (expand-file-name name root)) t)
  (wtimer--write root name content)
  (find-file-noselect (expand-file-name name root)))

(defun note--region (buffer text)
  "Mark TEXT in BUFFER; point and mark around it."
  (set-buffer buffer)
  (goto-char (point-min))
  (should (search-forward text nil t))
  (set-mark (match-beginning 0))
  (goto-char (match-end 0))
  (activate-mark))

(defun note--write (buffer text words)
  "In BUFFER, write the note WORDS on TEXT, as the keys would."
  (switch-to-buffer buffer)
  (note--region buffer text)
  (superchat-note)
  (should (equal superchat-note--edit-buffer (buffer-name (current-buffer))))
  (insert words)
  (superchat-note-edit-finish))

(defun note--count (&optional buffer)
  (length (superchat-note--overlays buffer)))

(defun note--kept ()
  "What `superchat-note-file' holds, read back."
  (with-temp-buffer
    (insert-file-contents superchat-note-file)
    (read (current-buffer))))

(defun note--attach (view)
  "With VIEW in sight, put the notes on its message."
  (switch-to-buffer view)
  (superchat-note-attach))

;;;; Writing ------------------------------------------------------------------------------

(ert-deftest note/a-note-leaves-the-text-as-it-was ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "(defun foo ()\n  (if x (progn y)))\n")))
      (note--write buffer "(progn y)" "use when")
      (with-current-buffer buffer
        (should (equal "(defun foo ()\n  (if x (progn y)))\n"
                       (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (buffer-modified-p))
        (should (= 1 (note--count)))
        (let ((overlay (car (superchat-note--overlays))))
          (should (equal "(progn y)" (buffer-substring-no-properties
                                      (overlay-start overlay) (overlay-end overlay))))
          (should (equal "use when" (plist-get (overlay-get overlay 'superchat-note) :text))))))))

(ert-deftest note/on-a-note-the-same-key-changes-its-words-and-empty-drops-it ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "alpha beta gamma\n")))
      (note--write buffer "beta" "first words")
      (with-current-buffer buffer
        (deactivate-mark)
        (goto-char (overlay-start (car (superchat-note--overlays))))
        (superchat-note)
        (should (equal "first words" (buffer-string)))
        (erase-buffer)
        (insert "second words")
        (superchat-note-edit-finish)
        (should (= 1 (note--count buffer)))
        (should (equal "second words"
                       (plist-get (overlay-get (car (superchat-note--overlays buffer))
                                               'superchat-note)
                                  :text)))
        (set-buffer buffer)
        (goto-char (overlay-start (car (superchat-note--overlays buffer))))
        (superchat-note)
        (erase-buffer)
        (superchat-note-edit-finish)
        (should (= 0 (note--count buffer)))))))

(ert-deftest note/cancelling-leaves-it-as-it-was ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "alpha beta gamma\n")))
      (switch-to-buffer buffer)
      (note--region buffer "beta")
      (superchat-note)
      (insert "never kept")
      (superchat-note-edit-abort)
      (should (= 0 (note--count buffer)))
      (should-not (get-buffer superchat-note--edit-buffer)))))

(ert-deftest note/with-no-region-it-is-on-the-word-at-point ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "alpha beta-gamma delta\n")))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (search-forward "beta")
      (superchat-note)
      (insert "on the symbol")
      (superchat-note-edit-finish)
      (let ((overlay (car (superchat-note--overlays buffer))))
        (should (equal "beta-gamma" (with-current-buffer buffer
                                      (buffer-substring-no-properties
                                       (overlay-start overlay) (overlay-end overlay)))))))))

(ert-deftest note/point-on-a-note-shows-its-words ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "alpha beta gamma\n")))
      (note--write buffer "beta" "SAY-THIS")
      (with-current-buffer buffer
        (deactivate-mark)
        (let (said)
          (cl-letf (((symbol-function 'current-message) (lambda () nil))
                    ((symbol-function 'message)
                     (lambda (format &rest args) (push (apply #'format format args) said))))
            (goto-char (point-min))
            (superchat-note--echo)
            (should-not said)
            (search-forward "be")
            (superchat-note--echo)
            (should (string-match-p "SAY-THIS" (car said)))
            ;; Moving within the same note says it once.
            (forward-char 1)
            (superchat-note--echo)
            (should (= 1 (length said)))))))))

(ert-deftest note/delete-at-point-and-in-a-region ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "one two three four\n")))
      (note--write buffer "one" "a")
      (note--write buffer "two" "b")
      (note--write buffer "four" "c")
      (with-current-buffer buffer
        (deactivate-mark)
        (goto-char (point-max))
        (search-backward "four")
        (superchat-note-delete)
        (should (= 2 (note--count)))
        (note--region buffer "one two")
        (superchat-note-delete)
        (should (= 0 (note--count)))))))

;;;; Keeping ---------------------------------------------------------------------------

(ert-deftest note/notes-on-a-file-come-back-on-their-text-though-lines-moved ()
  (note--with nil (p)
    (let* ((buffer (note--file p "foo.el" "a\nb\n(target here)\nc\n"))
           (file (buffer-file-name buffer)))
      (note--write buffer "target" "KEPT-WORDS")
      (kill-buffer buffer)
      (should (equal "KEPT-WORDS" (plist-get (cadr (assoc file (note--kept))) :text)))
      ;; Two lines come in above it while it is not visited.
      (wtimer--write p "foo.el" "new1\nnew2\na\nb\n(target here)\nc\n")
      (setq superchat-note--kept 'unread)
      (let* ((again (find-file-noselect file))
             (overlay (car (superchat-note--overlays again))))
        (should overlay)
        (with-current-buffer again
          (should (equal "target" (buffer-substring-no-properties
                                   (overlay-start overlay) (overlay-end overlay))))
          (should (= 5 (line-number-at-pos (overlay-start overlay)))))))))

(ert-deftest note/a-note-whose-text-went-keeps-its-words-and-says-so ()
  (note--with nil (p)
    (let* ((buffer (note--file p "foo.el" "a\nb\n(target here)\nc\n"))
           (file (buffer-file-name buffer)))
      (note--write buffer "target" "STILL-MINE")
      (kill-buffer buffer)
      (wtimer--write p "foo.el" "a\nb\nc\n")
      (find-file-noselect file)
      (let ((words (superchat-note--words)))
        (should (string-match-p "STILL-MINE" words))
        (should (string-match-p "not there any more" words))))))

(ert-deftest note/notes-on-a-buffer-with-no-file-are-not-kept ()
  (note--with nil (p)
    (let ((buffer (get-buffer-create "*some diff*")))
      (with-current-buffer buffer (insert "-old\n+new\n"))
      (note--write buffer "+new" "IN-A-DIFF")
      (should (= 1 (note--count buffer)))
      (should (string-match-p "buffer \\*some diff\\*" (superchat-note--words)))
      (should-not (file-exists-p superchat-note-file))
      (kill-buffer buffer)
      (should-not (superchat-note--all)))))

;;;; Handing them over ---------------------------------------------------------------

(ert-deftest note/attaching-puts-every-note-of-every-buffer-above-where-you-write ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "(defun foo ()\n  (progn y))\n"))
           (bar (note--file p "sub/bar.el" "line1\nline2\nline3\n")))
      (note--write foo "(progn y)" "use when")
      (note--write bar "line2\nline3" "drop these")
      (note--attach view)
      (with-current-buffer view
        ;; The input area is still yours.
        (should (equal "" (string-trim (buffer-substring-no-properties
                                        superchat--prompt-start (point-max))))))
      (bring--attached-above-the-heading view)
      (should (string-match-p "2 notes of yours, on foo.el, bar.el" (bring--attached-shown view)))
      (let ((message (bring--message view)))
        (should (string-match-p "^My notes on the text (2):" message))
        (should (string-match-p "^1. foo.el, line 2:$" message))
        (should (string-match-p "^> (progn y)$" message))
        (should (string-match-p "^My note: use when$" message))
        (should (string-match-p "^2. sub/bar.el, lines 2-3:$" message))
        (should (string-match-p "^> line2\n> line3\nMy note: drop these$" message))))))

(ert-deftest note/what-is-attached-follows-the-notes-until-it-is-sent ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "one" "FIRST")
      (note--attach view)
      (note--write foo "three" "LATER")
      (should (string-match-p "LATER" (bring--message view)))
      (should (string-match-p "2 notes" (bring--attached-shown view)))
      (with-current-buffer foo
        (deactivate-mark)
        (mapc #'delete-overlay (superchat-note--overlays)))
      (superchat-note--changed)
      ;; No notes: nothing attached shows, and nothing would go.
      (should-not (bring--attached-shown view))
      (should (equal "" (string-trim (bring--message view)))))))

(ert-deftest note/sent-they-reach-the-model-and-are-done ()
  (note--with '((:text "OK")) (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "two" "NOTE-WORDS")
      (note--attach view)
      (presence--type view "fix these")
      (bring--send view)
      (let ((sent (bring--last-user-text 1)))
        (should (string-match-p "^My notes on the text (1):" sent))
        (should (string-match-p "^> two$" sent))
        (should (string-match-p "^My note: NOTE-WORDS\n\nfix these$" sent)))
      (should (= 0 (note--count foo)))
      (should-not (superchat-note--all))
      (should-not (assoc (buffer-file-name foo) (note--kept))))))

(ert-deftest note/a-note-written-after-sending-is-not-taken-for-sent ()
  (note--with '((:text "OK")) (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "one" "SENT")
      (note--attach view)
      (bring--send view)
      (note--write foo "three" "NEW")
      (should (= 1 (note--count foo)))
      (should (equal "NEW" (plist-get (overlay-get (car (superchat-note--overlays foo))
                                                   'superchat-note)
                                      :text))))))

(ert-deftest note/taking-it-off-the-message-keeps-the-notes ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "two" "KEEP-ME")
      (note--attach view)
      (with-current-buffer view (superchat-butler-attached-toggle t))
      (should-not (bring--attached-shown view))
      (should (= 1 (note--count foo))))))

(ert-deftest note/attaching-twice-is-once ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "two" "ONCE")
      (note--attach view)
      (note--attach view)
      (let ((message (bring--message view)))
        (should (= 1 (with-temp-buffer (insert message) (how-many "ONCE" (point-min) (point-max)))))))))

(ert-deftest note/the-quoted-text-triggers-nothing ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "/summarize\n@gpt-4o\n>planning\n")))
      (note--write foo "/summarize\n@gpt-4o\n>planning" "what is this")
      (note--attach view)
      (let ((message (bring--message view)))
        (should (string-match-p "^> /summarize\n> @gpt-4o\n> >planning$" message))
        (isx--with-names (should (bring--inert-p message)))))))

(ert-deftest note/an-unsaved-buffer-says-its-lines-are-the-buffers ()
  (note--with nil (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one\ntwo\n")))
      (with-current-buffer foo (goto-char (point-min)) (insert "zero\n"))
      (note--write foo "two" "W")
      (note--attach view)
      (should (string-match-p "^1. foo.el, line 3 (not saved; lines as in my buffer):$"
                              (bring--message view))))))

(ert-deftest note/a-long-text-is-cut-and-says-how-much-more ()
  (note--with nil (p)
    (let* ((superchat-note-quoted-lines 3)
           (foo (note--file p "foo.el" "l1\nl2\nl3\nl4\nl5\n")))
      (note--write foo "l1\nl2\nl3\nl4\nl5" "W")
      (let ((words (superchat-note--words)))
        (should (string-match-p "^> l3$" words))
        (should-not (string-match-p "l4" words))
        (should (string-match-p "^> (and 2 lines more)$" words))))))

(ert-deftest note/attaching-with-no-notes-says-how-to-write-one ()
  (note--with nil (p)
    (let ((view (tcmd--view p)))
      (switch-to-buffer view)
      (should-error (superchat-note-attach) :type 'user-error)
      (should-not (bring--attached-shown view)))))

(ert-deftest note/a-new-note-with-no-words-is-not-made ()
  (note--with nil (p)
    (let ((buffer (note--file p "foo.el" "alpha beta gamma\n")))
      (note--write buffer "beta" "   ")
      (should (= 0 (note--count buffer))))))

(ert-deftest note/of-two-places-with-its-text-it-comes-back-to-the-nearer ()
  (note--with nil (p)
    (let* ((buffer (note--file p "foo.el" "(target)\nb\nc\nd\n(target)\ne\n"))
           (file (buffer-file-name buffer)))
      (with-current-buffer buffer (goto-char (point-min)) (forward-line 4))
      (switch-to-buffer buffer)
      (set-mark (1+ (point)))
      (goto-char (+ (point) 7))
      (activate-mark)
      (superchat-note)
      (insert "SECOND")
      (superchat-note-edit-finish)
      (kill-buffer buffer)
      (setq superchat-note--kept 'unread)
      (let ((again (find-file-noselect file)))
        (with-current-buffer again
          (should (= 5 (line-number-at-pos
                        (overlay-start (car (superchat-note--overlays)))))))))))

(ert-deftest note/a-note-whose-text-went-is-not-the-one-next-to-it ()
  (note--with nil (p)
    (let* ((buffer (note--file p "foo.el" "a\nb\n(target here)\nother words\n"))
           (file (buffer-file-name buffer)))
      (note--write buffer "target" "LOST-ONE")
      (kill-buffer buffer)
      (wtimer--write p "foo.el" "a\nb\nother words\n")
      (let ((again (find-file-noselect file)))
        (switch-to-buffer again)
        (deactivate-mark)
        (goto-char (overlay-start (car (superchat-note--overlays))))
        (forward-char 1)
        (superchat-note)
        ;; A new note on the word here, not the words of the lost one.
        (should (equal "" (buffer-string)))
        (superchat-note-edit-abort)))))

(ert-deftest note/what-goes-is-what-is-there-when-it-is-sent ()
  (note--with '((:text "OK")) (p)
    (let* ((view (tcmd--view p))
           (foo (note--file p "foo.el" "one two three\n")))
      (note--write foo "one" "FIRST")
      (note--attach view)
      ;; Looked at whole, then one more note before it goes.
      (bring--message view)
      (note--write foo "three" "SECOND")
      (bring--send view)
      (should (string-match-p "SECOND" (bring--last-user-text 1)))
      (should-not (superchat-note--all)))))

;;;; The list --------------------------------------------------------------------------

(ert-deftest note/the-list-has-every-note-and-d-drops-one ()
  (note--with nil (p)
    (let* ((foo (note--file p "foo.el" "one two\n"))
           (bar (note--file p "bar.el" "three\n")))
      (note--write foo "two" "ON-FOO")
      (note--write bar "three" "ON-BAR")
      (kill-buffer bar)
      (superchat-notes)
      (let ((rows (mapcar (lambda (entry) (aref (cadr entry) 2)) tabulated-list-entries)))
        (should (member "ON-FOO" rows))
        (should (member "ON-BAR" rows)))
      (goto-char (point-min))
      (while (not (equal "ON-BAR" (plist-get (car (tabulated-list-get-id)) :text)))
        (forward-line 1))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (superchat-notes-drop))
      (should (equal '("ON-FOO") (mapcar (lambda (pair) (plist-get (car pair) :text))
                                         (superchat-note--all)))))))

(ert-deftest note/ret-in-the-list-visits-a-file-and-goes-to-the-text ()
  (note--with nil (p)
    (let* ((bar (note--file p "bar.el" "x\ny\n(here it is)\n"))
           (file (buffer-file-name bar)))
      (note--write bar "here" "GO")
      (kill-buffer bar)
      (superchat-notes)
      (goto-char (point-min))
      (superchat-notes-visit)
      (should (equal file (buffer-file-name)))
      (should (looking-at "here")))))

(provide 'note-regression)
;;; note-regression.el ends here
