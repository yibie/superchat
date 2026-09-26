;;; shared-conversation-regression.el --- in-place commands join the chat's conversation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for M10 slice 2
;; (docs/decisions/accepted/2026-09-24-editor-mode-agent-mode-and-review-screen.md,
;; "共用一段对话", amended 2026-09-25 "1d 2a"): in-place ask, act and rewrite
;; go into the conversation the chat window shows; their requests carry no
;; earlier turns of it (a follow-up in the same overlay carries that overlay's
;; own exchange); the turn is recorded there, shown in the chat window with a
;; way back to where it was asked, and carried by the chat's later requests;
;; while the conversation runs, it waits its turn; the chat window's associated
;; source does not change; /changes lists what an in-place act edited.
;;
;; It reuses the history-messages suite's fixture and wire capture.  The
;; edited file lives in a fresh temporary directory, outside any git project.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/shared-conversation-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^shared-conversation/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-inline)
(require 'superchat-inline-act)
(require 'superchat-rewrite)

(defvar superchat-changes-review-after-run)

(defmacro sc--fixture (script bindings &rest body)
  "Run BODY with SCRIPT, the chat window open, and a file outside git.
BINDINGS is (VIEW FILE-BUFFER): the chat View and the buffer visiting
DIR/note.txt, which holds three lines, point on the third."
  (declare (indent 2) (debug t))
  (let ((view (nth 0 bindings)) (buffer (nth 1 bindings)))
    `(let* ((dir (file-name-as-directory (make-temp-file "shared-conversation-" t)))
            (file (expand-file-name "note.txt" dir))
            (superchat-changes-review-after-run nil))
       (with-temp-file file (insert "first line\nsecond line\nthird line\n"))
       (unwind-protect
           (hm--fixture ,script
             (let ((,view (smoke--open-chat))
                   (,buffer (find-file-noselect file)))
               (with-current-buffer ,buffer
                 (goto-char (point-min))
                 (forward-line 2))
               ,@body))
         (when-let* ((buffer (get-file-buffer file)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))
         (delete-directory dir t)))))

(defun sc--session (view) (buffer-local-value 'superchat--session-id view))

(defun sc--ask (buffer question)
  "Ask QUESTION in place from BUFFER at its point; wait until answered."
  (let ((overlay (with-current-buffer buffer
                   (superchat-inline-ask question (cons (line-beginning-position)
                                                        (line-end-position))))))
    (should (smoke--wait (lambda () (eq 'ready (plist-get (overlay-get overlay 'superchat-inline)
                                                          :status)))
                         20))
    overlay))

(defun sc--state (overlay) (overlay-get overlay 'superchat-inline))

(defun sc--texts (wire) (mapcar #'hm--text (hm--body wire)))

;;;; The conversation the chat window shows

(ert-deftest shared-conversation/an-in-place-ask-joins-the-shown-conversation ()
  "An in-place ask from a file outside git goes into the conversation the
chat window shows, not into a separate in-place conversation."
  (sc--fixture (list '(:text "SC-ANSWER")) (view buffer)
    (let ((overlay (sc--ask buffer "SC-QUESTION")))
      (should (equal (sc--session view) (plist-get (sc--state overlay) :session-id))))))

(ert-deftest shared-conversation/an-in-place-request-carries-no-earlier-turns ()
  "An in-place request carries its own reference and question only: neither
the chat's earlier turns nor an earlier in-place turn."
  (sc--fixture (list '(:text "SC-CHAT-ANSWER") '(:text "SC-ANSWER-ONE") '(:text "SC-ANSWER-TWO"))
      (view buffer)
    (hm--turn view "SC-CHAT-QUESTION")
    (sc--ask buffer "SC-QUESTION-ONE")
    (sc--ask buffer "SC-QUESTION-TWO")
    (let ((texts (sc--texts (car hm--wires))))
      (should (equal '("user") (hm--roles (car hm--wires))))
      (should (string-match-p "SC-QUESTION-TWO" (car texts)))
      (dolist (earlier '("SC-CHAT-QUESTION" "SC-CHAT-ANSWER" "SC-QUESTION-ONE" "SC-ANSWER-ONE"))
        (should-not (cl-some (lambda (text) (string-match-p earlier text)) texts))))))

(ert-deftest shared-conversation/a-follow-up-carries-its-own-exchange ()
  "A follow-up in the same overlay carries that overlay's first question and
answer, and still nothing else of the conversation."
  (sc--fixture (list '(:text "SC-CHAT-ANSWER") '(:text "SC-ANSWER-ONE") '(:text "SC-ANSWER-TWO"))
      (view buffer)
    (hm--turn view "SC-CHAT-QUESTION")
    (let ((overlay (sc--ask buffer "SC-QUESTION-ONE")))
      (should (equal (sc--session view) (plist-get (sc--state overlay) :session-id)))
      (with-current-buffer buffer (superchat-inline-reply overlay "SC-QUESTION-TWO"))
      (should (smoke--wait (lambda () (eq 'ready (plist-get (sc--state overlay) :status))) 20))
      (let ((texts (sc--texts (car hm--wires))))
        (should (cl-some (lambda (text) (string-match-p "SC-QUESTION-ONE" text)) texts))
        (should (cl-some (lambda (text) (string-match-p "SC-ANSWER-ONE" text)) texts))
        (should-not (cl-some (lambda (text) (string-match-p "SC-CHAT-QUESTION" text)) texts))))))

(ert-deftest shared-conversation/the-chat-carries-the-in-place-turn-afterwards ()
  "The chat's next request carries the in-place question and its answer as
earlier turns."
  (sc--fixture (list '(:text "SC-ANSWER") '(:text "SC-CHAT-ANSWER")) (view buffer)
    (sc--ask buffer "SC-QUESTION")
    (hm--turn view "SC-CHAT-QUESTION")
    (let ((texts (sc--texts (car hm--wires))))
      (should (cl-some (lambda (text) (string-match-p "SC-QUESTION" text)) (butlast texts)))
      (should (member "SC-ANSWER" texts)))))

(ert-deftest shared-conversation/the-chat-window-shows-it-with-a-way-back ()
  "The chat window shows the in-place turn -- question and answer -- with a
button that goes back to the file and line it was asked from."
  (sc--fixture (list '(:text "SC-ANSWER")) (view buffer)
    (sc--ask buffer "SC-QUESTION")
    (should (smoke--wait (lambda () (with-current-buffer view
                                      (save-excursion
                                        (goto-char (point-min))
                                        (search-forward "SC-ANSWER" nil t))))
                         5))
    (with-current-buffer view
      (goto-char (point-min))
      (should (search-forward "SC-QUESTION" nil t))
      (goto-char (point-min))
      (should (search-forward "note.txt" nil t))
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (button-activate button)))
    ;; What the person sees: the selected window, on that line.
    (should (eq buffer (window-buffer (selected-window))))
    (should (= 3 (with-current-buffer buffer
                   (line-number-at-pos (window-point (selected-window))))))))

(ert-deftest shared-conversation/while-the-chat-runs-it-waits ()
  "An in-place act started while the chat's Run is still working waits: it
starts after that Run ended, and completes."
  (sc--fixture (list '(:text "SC-CHAT-ANSWER" :delay 1.0) '(:text "SC-ACT-ANSWER")) (view buffer)
    (let* ((chat-run (smoke--send view "SC-CHAT-QUESTION"))
           (overlay (with-current-buffer buffer (superchat-inline-act "SC-ACT"))))
      (should (smoke--wait (lambda () (eq 'ready (plist-get (sc--state overlay) :status))) 20))
      (let* ((events (superchat-tape-replay-events (sc--session view)))
             (chat-end (cl-position-if (lambda (e) (and (equal "run.completed" (plist-get e :type))
                                                        (equal chat-run (plist-get e :run-id))))
                                       events))
             (act-run (plist-get (sc--state overlay) :run-id))
             (act-start (cl-position-if (lambda (e) (and (equal "run.started" (plist-get e :type))
                                                         (equal act-run (plist-get e :run-id))))
                                        events)))
        (should chat-end)
        (should act-start)
        (should (< chat-end act-start))))))

(ert-deftest shared-conversation/the-chat-window-keeps-its-source ()
  "An in-place turn from another buffer leaves the chat window's associated
source as it was."
  (sc--fixture (list '(:text "SC-ANSWER")) (view buffer)
    (let ((before (buffer-local-value 'superchat--origin-buffer view)))
      (sc--ask buffer "SC-QUESTION")
      (should (eq before (buffer-local-value 'superchat--origin-buffer view))))))

(ert-deftest shared-conversation/changes-lists-what-an-in-place-act-edited ()
  "What an in-place act edited is among the conversation's unsaved changes."
  (sc--fixture (list '(:tool-uses (("edit_buffer" (buffer_name . "note.txt")
                                    (old_string . "third") (new_string . "THIRD"))))
                     '(:text "SC-ACT-ANSWER"))
      (view buffer)
    (let ((overlay (with-current-buffer buffer (superchat-inline-act "SC-ACT"))))
      (should (smoke--wait (lambda () (eq 'ready (plist-get (sc--state overlay) :status))) 20))
      (should (buffer-modified-p buffer))
      (should (cl-find buffer (superchat-changes-candidates (sc--session view))
                       :key (lambda (row) (plist-get row :buffer)))))))

(ert-deftest shared-conversation/a-rewrite-joins-and-carries-no-earlier-turns ()
  "A region rewrite goes into the shown conversation and its request carries
only the region and the instruction."
  (sc--fixture (list '(:text "SC-CHAT-ANSWER") '(:text "```\nTHIRD LINE\n```")) (view buffer)
    (hm--turn view "SC-CHAT-QUESTION")
    (let ((overlay (with-current-buffer buffer
                     (let ((transient-mark-mode t))
                       (push-mark (line-beginning-position) t t)
                       (goto-char (line-end-position))
                       (superchat-rewrite-region (mark) (point) "SC-UPPERCASE")))))
      (setq overlay (if (listp overlay) (car overlay) overlay))
      (should (smoke--wait (lambda () (memq (plist-get (superchat-rewrite--state overlay) :status)
                                            '(ready error)))
                           20))
      (should (equal (sc--session view) (plist-get (superchat-rewrite--state overlay) :session-id)))
      (should (equal '("user") (hm--roles (car hm--wires))))
      (should-not (string-match-p "SC-CHAT-QUESTION" (car (sc--texts (car hm--wires))))))))

(provide 'shared-conversation-regression)

;;; shared-conversation-regression.el ends here
