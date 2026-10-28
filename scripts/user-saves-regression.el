;;; user-saves-regression.el --- the next turn learns what the user saved -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "the next turn learns what the user
;; saved" (docs/decisions/implemented/2026-09-25-the-next-turn-learns-what-the-user-saved.md;
;; context contract, amendment 2026-09-25 (b)).  When the user saves, or
;; discards, a buffer a Session's tools edited and left unsaved, the next
;; turn's request carries one `[Injected material]' user message saying so,
;; after the previous answer and before the new question.
;;
;; It reuses the history-messages suite's fixture and wire capture.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/user-saves-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^user-saves/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-changes-review-after-run)

(defmacro user-saves--fixture (bindings &rest body)
  "Turn 1 opens DIR/note.txt and edits its buffer; then run BODY.
BINDINGS is (VIEW FILE BUFFER).  Turn 2's answer is scripted too."
  (declare (indent 1) (debug t))
  (let ((view (nth 0 bindings)) (file (nth 1 bindings)) (buffer (nth 2 bindings)))
    `(let* ((dir (file-name-as-directory (make-temp-file "user-saves-" t)))
            (,file (expand-file-name "note.txt" dir))
            (superchat-changes-review-after-run nil)
            (hm--extra-tool-names '("open_file")))
       (with-temp-file ,file (insert "two\n"))
       (unwind-protect
           (hm--fixture (list `(:tool-uses (("open_file" (path . ,,file))))
                              '(:tool-uses (("edit_buffer" (buffer_name . "note.txt")
                                             (old_string . "two") (new_string . "TWO"))))
                              '(:text "US-ANSWER-ONE")
                              '(:text "US-ANSWER-TWO"))
             (let ((,view (smoke--open-chat)))
               (hm--turn ,view "US-ASK-ONE change two in note.txt")
               (let ((,buffer (get-file-buffer ,file)))
                 (should (buffer-modified-p ,buffer))
                 ,@body)))
         (when-let* ((buffer (get-file-buffer ,file)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))
         (delete-directory dir t)))))

(defun user-saves--notes (wire)
  "The messages of WIRE that tell the model what the user saved or discarded."
  (cl-remove-if-not
   (lambda (message)
     (string-match-p "\\`\\[Injected material\\] The user \\(saved\\|discarded\\)"
                     (hm--text message)))
   (hm--body wire)))

(defun user-saves--position (wire text)
  "The index in WIRE's body of the first message containing TEXT."
  (cl-position-if (lambda (message) (string-match-p (regexp-quote text) (hm--text message)))
                  (hm--body wire)))

(ert-deftest user-saves/a-save-reaches-the-next-turn ()
  "After the user saves the edited buffer (as `C-x C-s' does), the next
turn's request says so once, after the previous answer and before the new
question."
  (user-saves--fixture (view file buffer)
    (with-current-buffer buffer (save-buffer))
    (hm--turn view "US-ASK-TWO")
    (let* ((wire (car hm--wires))
           (notes (user-saves--notes wire)))
      (should (= 1 (length notes)))
      (should (equal "user" (alist-get 'role (car notes))))
      (should (string-match-p "saved note.txt" (hm--text (car notes))))
      (should (string-match-p (regexp-quote file) (hm--text (car notes))))
      (let ((note (user-saves--position wire "The user saved")))
        (should (< (user-saves--position wire "US-ANSWER-ONE") note))
        (should (< note (user-saves--position wire "US-ASK-TWO")))))))

(ert-deftest user-saves/the-review-screen-save-counts ()
  "A save through the review screen's command is a save by the user."
  (user-saves--fixture (view file buffer)
    (superchat-changes-save buffer)
    (hm--turn view "US-ASK-TWO")
    (should (= 1 (length (user-saves--notes (car hm--wires)))))))

(ert-deftest user-saves/a-discard-reaches-the-next-turn ()
  "When the user throws the model's unsaved edits away, the next turn is told."
  (user-saves--fixture (view file buffer)
    (superchat-changes-revert buffer)
    (should-not (buffer-modified-p buffer))
    (hm--turn view "US-ASK-TWO")
    (let ((notes (user-saves--notes (car hm--wires))))
      (should (= 1 (length notes)))
      (should (string-match-p "discarded your unsaved edits to note.txt"
                              (hm--text (car notes)))))))

(ert-deftest user-saves/a-save-while-a-tool-runs-is-not-the-users ()
  "A save made while a tool runs (the model's save_buffer, or code it ran) is
not reported as the user's."
  (user-saves--fixture (view file buffer)
    (let ((superchat--tool-execution-context (list :session-id "some-session")))
      (with-current-buffer buffer (save-buffer)))
    (hm--turn view "US-ASK-TWO")
    (should-not (user-saves--notes (car hm--wires)))))

(ert-deftest user-saves/once-per-settled-edit ()
  "A later save of the same buffer, with no new edit by the model, is not
reported again."
  (user-saves--fixture (view file buffer)
    (with-current-buffer buffer
      (save-buffer)
      (goto-char (point-max))
      (insert "by hand\n")
      (save-buffer))
    (hm--turn view "US-ASK-TWO")
    (should (= 1 (length (user-saves--notes (car hm--wires)))))))

(ert-deftest user-saves/a-buffer-the-model-did-not-edit ()
  "Saving a buffer no Session's tools edited tells no one anything."
  (user-saves--fixture (view file buffer)
    (let ((other (expand-file-name "other.txt" (file-name-directory file))))
      (with-current-buffer (find-file-noselect other)
        (insert "mine\n")
        (save-buffer)
        (kill-buffer)))
    (hm--turn view "US-ASK-TWO")
    (should-not (user-saves--notes (car hm--wires)))))

(ert-deftest user-saves/nothing-when-the-changes-row-is-off ()
  "With the `changes' plugin row unmounted, a save is not recorded."
  (user-saves--fixture (view file buffer)
    (superchat-plugin-unmount 'changes)
    (unwind-protect
        (progn
          (with-current-buffer buffer (save-buffer))
          (hm--turn view "US-ASK-TWO")
          (should-not (user-saves--notes (car hm--wires))))
      (superchat-plugin-mount 'changes))))

(ert-deftest user-saves/the-evidence-rebuilds-the-note ()
  "The next turn's request evidence rebuilds the note the provider received."
  (user-saves--fixture (view file buffer)
    (with-current-buffer buffer (save-buffer))
    (hm--turn view "US-ASK-TWO")
    (let* ((session (hm--session view))
           (last-list (car (last (hm--events session "request.message-list"))))
           (evidence (superchat-tape-request-evidence
                      session (plist-get (plist-get last-list :payload) :request-id)))
           (note (car (user-saves--notes (car hm--wires)))))
      (should (eq 'reproduced (plist-get evidence :status)))
      (should (cl-find (hm--text note) (plist-get evidence :messages)
                       :key (lambda (m) (plist-get m :content)) :test #'equal)))))

(provide 'user-saves-regression)

;;; user-saves-regression.el ends here
