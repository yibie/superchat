;;; test-complete.el --- tests for superchat-complete -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-complete)

(defconst superchat-test-complete--lisp
  "(defun one () 1)\n\n(defun two () 2)\n\n(defun three () 3)\n\n(defun four ()\n  (list \n\n(defun five () 5)\n\n(defun six () 6)\n")

(defmacro superchat-test-complete--with (text &rest body)
  "BODY in an `emacs-lisp-mode' buffer holding TEXT, point after \"(list \"."
  (declare (indent 1))
  `(superchat-test-with-tape
     (with-temp-buffer
       (emacs-lisp-mode)
       (insert ,text)
       (goto-char (point-min))
       (search-forward "(list ")
       (unwind-protect (progn ,@body)
         (superchat-complete-mode -1)))))

(defun superchat-test-complete--shown ()
  "The ghost text after point, or nil."
  (seq-some (lambda (o) (overlay-get o 'after-string)) (overlays-in (point) (point))))

(defun superchat-test-complete--command (command)
  "Run the post-command hook as after COMMAND."
  (let ((this-command command)) (run-hooks 'post-command-hook)))

(ert-deftest superchat-complete-sends-the-forms-around-point ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (let (reasoning)
      (superchat-test-script
          (list (lambda (_) (setq reasoning (list superchat-llm-reasoning)) '(:text "1 2)")))
        (let ((superchat-llm-reasoning 'high)) (superchat-complete))
        (let* ((request (car superchat-test-requests))
               (text (plist-get (car (plist-get request :messages)) :text)))
          (should (equal reasoning '(nil)))
          (should-not (plist-get request :tools))
          (should (string-match-p "<CURSOR>" (plist-get request :system)))
          (should (string-match-p "(list <CURSOR>\n" text))
          (should (string-match-p "emacs-lisp-mode" text))
          (should (string-match-p "(defun two" text))
          (should (string-match-p "(defun five" text))
          (should-not (string-match-p "(defun one" text))
          (should-not (string-match-p "(defun six" text)))))
    (should-not (superchat-tape-entries))))

(ert-deftest superchat-complete-shows-and-accepts ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (superchat-test-script (list '(:text "```elisp\n(list 1 2)\n```"))
      (superchat-complete)
      (should (equal (substring-no-properties (superchat-test-complete--shown)) "1 2)"))
      (should (eq (key-binding (kbd "TAB")) #'superchat-complete-accept))
      (superchat-complete-accept)
      (should (looking-back "(list 1 2)" (line-beginning-position)))
      (should-not (superchat-test-complete--shown))
      (should-not (eq (key-binding (kbd "TAB")) #'superchat-complete-accept)))))

(ert-deftest superchat-complete-drops-a-stale-answer ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (let ((superchat-test-async t))
      (superchat-test-script (list '(:text "1 2)"))
        (superchat-complete)
        (insert "x")
        (superchat-test-drain)
        (should-not (superchat-test-complete--shown))))))

(ert-deftest superchat-complete-typing-through-shortens-it ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (superchat-complete-mode 1)
    (superchat-test-script (list '(:text "1 2)"))
      (superchat-complete)
      (insert "1")
      (superchat-test-complete--command 'self-insert-command)
      (should (equal (substring-no-properties (superchat-test-complete--shown)) " 2)"))
      (forward-char -1)
      (superchat-test-complete--command 'backward-char)
      (should-not (superchat-test-complete--shown)))))

(ert-deftest superchat-complete-asks-after-a-pause-at-a-boundary ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (superchat-complete-mode 1)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat function &rest args) (apply function args) nil))
              ((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
      (superchat-test-script (list '(:text "1 2)"))
        (insert "x")
        (forward-char -1)
        (superchat-test-complete--command 'self-insert-command)
        (should-not superchat-test-requests)
        (delete-char 1)
        (superchat-test-complete--command 'delete-char)
        (should (= 1 (length superchat-test-requests)))
        (should (superchat-test-complete--shown))))))

(ert-deftest superchat-complete-dismiss-hides-it ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (superchat-test-script (list '(:text "1 2)"))
      (superchat-complete)
      (superchat-complete-dismiss)
      (should-not (superchat-test-complete--shown)))))

(ert-deftest superchat-complete-off-when-it-cannot-ask ()
  (superchat-test-complete--with superchat-test-complete--lisp
    (superchat-complete-mode 1)
    (let ((superchat-llm-request-function
           (lambda (&rest _) (user-error "Set `superchat-llm-provider'"))))
      (superchat-complete))
    (should-not superchat-complete-mode)
    (should-not (superchat-test-complete--shown))))

;;; test-complete.el ends here
