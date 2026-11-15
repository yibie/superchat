;;; test-inline.el --- tests for superchat-inline -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-chat)
(require 'superchat-inline)

(defmacro superchat-test-inline--with (text &rest body)
  "BODY in a buffer visiting a file in a temporary directory, with TEXT in it,
the region over \"beta\" (the second word) and an instruction read as \"shout\"."
  (declare (indent 1))
  `(superchat-test-with-tape
     (let* ((dir (file-name-as-directory (make-temp-file "sc-inline-" t)))
            (default-directory dir))
       (unwind-protect
           (with-temp-buffer
             (setq buffer-file-name (expand-file-name "a.txt" dir))
             (transient-mark-mode 1)
             (buffer-enable-undo)
             (insert ,text)
             (goto-char (point-min))
             (search-forward "beta")
             (push-mark (match-beginning 0) t t)
             (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "shout")))
               ,@body))
         ;; A run left on the Tape would keep its timeline busy for the next test.
         (dolist (row (superchat-tape-select "SELECT DISTINCT tape FROM entry"))
           (when (superchat-run-active (car row)) (superchat-cancel (car row))))
         (dolist (b (buffer-list))
           (when (buffer-local-value 'superchat-chat--tape b) (kill-buffer b)))
         (delete-directory dir t)))))

(defun superchat-test-inline--overlay ()
  (seq-find (lambda (o) (overlay-get o 'superchat-text)) (overlays-in (point-min) (point-max))))

(defun superchat-test-inline--shown (overlay)
  (overlay-get overlay 'superchat-text))

(ert-deftest superchat-inline-request ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "BETA"))
      (superchat-rewrite "shout")
      (let ((request (car superchat-test-requests)))
        (should (eq (plist-get request :tools) nil))
        (should (string-match-p "replacement text only" (plist-get request :system)))
        (let ((text (plist-get (car (plist-get request :messages)) :text)))
          (should (string-match-p "shout" text))
          (should (string-match-p "beta" text))
          (should-not (string-match-p "alpha" text)))))
    (let* ((tape (plist-get (car (superchat-tape-entries)) :tape))
           (user (car (superchat-tape-entries :tape tape :kinds '(user)))))
      (should-not (string-match-p "inline" tape))
      (should (plist-get (plist-get user :data) :ephemeral))
      (should (equal (plist-get (plist-get (plist-get user :data) :origin) :mode)
                     "fundamental-mode"))
      (should (equal (plist-get (plist-get (plist-get user :data) :origin) :file)
                     buffer-file-name))
      (should (equal (mapcar (lambda (e) (plist-get e :kind))
                             (superchat-tape-entries :tape tape))
                     '(user assistant))))))

(ert-deftest superchat-inline-overlay-then-accept ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "BETA"))
      (superchat-rewrite "shout"))
    (let ((overlay (superchat-test-inline--overlay)))
      (should (equal (superchat-test-inline--shown overlay) "BETA"))
      (should (equal (buffer-string) "alpha beta gamma"))
      (goto-char (overlay-start overlay))
      (undo-boundary)
      (call-interactively #'superchat-inline-accept)
      (undo-boundary)
      (should (equal (buffer-string) "alpha BETA gamma"))
      (should-not (superchat-test-inline--overlay))
      (let ((last-command nil)) (undo))
      (should (equal (buffer-string) "alpha beta gamma")))))

(ert-deftest superchat-inline-the-region-stays-in-sight-with-the-keys ()
  (superchat-test-inline--with "alpha beta gamma"
    (let (said)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (superchat-test-script (list '(:text "BETA"))
          (superchat-rewrite "shout")))
      (let* ((overlay (superchat-test-inline--overlay))
             (after (substring-no-properties (overlay-get overlay 'after-string))))
        (should-not (overlay-get overlay 'display))
        (should (plist-get (overlay-get overlay 'face) :strike-through))
        (should (string-match-p "BETA" after))
        (should (string-match-p "struck-through text: C-c C-c accept" after))
        (should (string-match-p "struck-through text, C-c C-c accepts" said))
        (should-not (overlay-get overlay 'help-echo))
        (should (= (point) (overlay-start overlay)))))))

(ert-deftest superchat-inline-discard ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "BETA"))
      (superchat-rewrite "shout"))
    (goto-char (overlay-start (superchat-test-inline--overlay)))
    (call-interactively #'superchat-inline-discard)
    (should-not (superchat-test-inline--overlay))
    (should (equal (buffer-string) "alpha beta gamma"))))

(ert-deftest superchat-inline-unwraps-fence ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "```elisp\n(foo)\n(bar)\n```\n"))
      (superchat-rewrite "shout"))
    (goto-char (overlay-start (superchat-test-inline--overlay)))
    (call-interactively #'superchat-inline-accept)
    (should (equal (buffer-string) "alpha (foo)\n(bar) gamma"))))

(ert-deftest superchat-inline-two-blocks-are-not-unwrapped ()
  (superchat-test-inline--with "alpha beta gamma"
    (let ((answer "```\na\n```\nmiddle\n```\nb\n```"))
      (superchat-test-script (list (list :text answer))
        (superchat-rewrite "shout"))
      (should (equal (overlay-get (superchat-test-inline--overlay) 'superchat-text) answer)))))

(ert-deftest superchat-inline-a-long-fenced-answer-is-unwrapped ()
  (superchat-test-inline--with "alpha beta gamma"
    (let ((body (mapconcat #'identity (make-list 40000 "line of code") "\n")))
      (superchat-test-script (list (list :text (concat "```\n" body "\n```")))
        (superchat-rewrite "shout"))
      (should (equal (overlay-get (superchat-test-inline--overlay) 'superchat-text) body)))))

(ert-deftest superchat-inline-a-refused-accept-keeps-the-overlay ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "BETA"))
      (superchat-rewrite "shout"))
    (goto-char (overlay-start (superchat-test-inline--overlay)))
    (setq buffer-read-only t)
    (should-error (call-interactively #'superchat-inline-accept))
    (should (superchat-test-inline--overlay))
    (should (equal (buffer-string) "alpha beta gamma"))))

(ert-deftest superchat-inline-accept-after-edit ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "BETA"))
      (superchat-rewrite "shout"))
    (let ((overlay (superchat-test-inline--overlay)))
      (goto-char (1+ (overlay-start overlay)))
      (insert "X")
      (goto-char (overlay-start overlay))
      (call-interactively #'superchat-inline-accept)
      (should (equal (buffer-string) "alpha BETA gamma")))))

(ert-deftest superchat-inline-needs-region ()
  (superchat-test-inline--with "alpha beta gamma"
    (deactivate-mark)
    (superchat-test-script (list '(:text "x"))
      (should-error (superchat-rewrite "shout") :type 'user-error)
      (should-not superchat-test-requests))))

(ert-deftest superchat-inline-error-removes-overlay ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:error "boom"))
      (superchat-rewrite "shout"))
    (should-not (superchat-test-inline--overlay))
    (should (equal (buffer-string) "alpha beta gamma"))))

(ert-deftest superchat-inline-answer-without_streaming ()
  (superchat-test-inline--with "alpha beta gamma"
    (let ((superchat-llm-request-function
           (lambda (_m &rest keys)
             (funcall (plist-get keys :on-done) (list :text "BETA" :calls nil :model "m"))
             (list :request nil))))
      (superchat-rewrite "shout"))
    (should (equal (superchat-test-inline--shown (superchat-test-inline--overlay)) "BETA"))))

(ert-deftest superchat-inline-each-rewrite-has-own-timeline ()
  (superchat-test-inline--with "alpha beta gamma"
    (superchat-test-script (list '(:text "ONE") '(:text "TWO"))
      (superchat-rewrite "first")
      (push-mark (point-min) t t)
      (goto-char (+ (point-min) 5))
      (superchat-rewrite "second")
      (should (= (length (plist-get (car superchat-test-requests) :messages)) 1)))
    (should (= (length (seq-filter (lambda (o) (overlay-get o 'superchat-text))
                                   (overlays-in (point-min) (point-max))))
               2))
    (should (= 2 (length (seq-uniq (mapcar (lambda (e) (plist-get e :tape))
                                           (superchat-tape-entries))))))
    (should-not (superchat-view 'timelines))))

(ert-deftest superchat-inline-active-run-accept-refused-discard-cancels ()
  (superchat-test-inline--with "alpha beta gamma"
    (let ((superchat-llm-request-function (lambda (&rest _) (list :request nil))))
      (superchat-rewrite "shout")
      (let ((overlay (superchat-test-inline--overlay)))
        (goto-char (overlay-start overlay))
        (should-error (superchat-inline-accept) :type 'user-error)
        (superchat-inline-discard)
        (should-not (superchat-test-inline--overlay))
        (should-not (superchat-run-active (overlay-get overlay 'superchat-tape)))))))

(ert-deftest superchat-inline-failed-start-leaves-no-overlay ()
  (superchat-test-inline--with "alpha beta gamma"
    (cl-letf (((symbol-function 'superchat-run) (lambda (&rest _) (error "no start"))))
      (should-error (superchat-rewrite "shout")))
    (should-not (superchat-test-inline--overlay))))

(ert-deftest superchat-inline-async-request-to-accept ()
  (superchat-test-inline--with "alpha beta gamma"
    (let ((superchat-test-async t))
      (superchat-test-script (list '(:text "BETA"))
        (superchat-rewrite "shout")
        (let ((overlay (superchat-test-inline--overlay)))
          (goto-char (overlay-start overlay))
          (should-error (superchat-inline-accept) :type 'user-error)
          (superchat-test-drain)
          (should (equal (buffer-string) "alpha beta gamma"))
          (call-interactively #'superchat-inline-accept)
          (should (equal (buffer-string) "alpha BETA gamma")))))))

(defun superchat-test-inline--ask (recent)
  "Ask \"why?\" with the region active; return (OPENED SENT) of the fake chat."
  (let (opened sent)
    (cl-letf (((symbol-function 'superchat-chat-recent-tape) (lambda (_) recent))
              ((symbol-function 'superchat-chat-open)
               (lambda (tape &optional _scope) (setq opened tape) (generate-new-buffer " *sc-chat*")))
              ((symbol-function 'superchat-chat-send)
               (lambda (text &optional origin) (setq sent (list text origin))))
              ((symbol-function 'display-buffer) (lambda (&rest _) nil)))
      (call-interactively #'superchat-ask))
    (list opened sent)))

(ert-deftest superchat-inline-ask-recent-tape ()
  (superchat-test-inline--with "alpha beta gamma"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "why?")))
      (pcase-let ((`(,opened (,text ,origin)) (superchat-test-inline--ask "20240101-000000-abcd")))
        (should (equal opened "20240101-000000-abcd"))
        (should (string-prefix-p "why?" text))
        (should (string-match-p "^> beta" text))
        (should (equal (plist-get origin :file) buffer-file-name))))))

(ert-deftest superchat-inline-ask-new-tape ()
  (superchat-test-inline--with "alpha beta gamma"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "why?")))
      (deactivate-mark)
      (pcase-let ((`(,opened (,text ,_)) (superchat-test-inline--ask nil)))
        (should (string-match-p "\\`[0-9]\\{8\\}-[0-9]\\{6\\}-" opened))
        (should (equal text "why?"))))))

(ert-deftest superchat-inline-ask-while-the-chat-is-busy-keeps-the-question ()
  (superchat-test-inline--with "alpha beta gamma"
    (deactivate-mark)
    (let ((chat (superchat-chat-open "t1" (superchat-tape-scope-of default-directory)))
          (superchat-llm-request-function (lambda (&rest _) (list :request nil)))
          said)
      (with-current-buffer chat (superchat-chat-send "one"))
      (save-window-excursion
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "why?"))
                  ((symbol-function 'message)
                   (lambda (f &rest a) (when f (setq said (apply #'format f a))))))
          (call-interactively #'superchat-ask)))
      ;; Queued, not put into the input.
      (should (equal (with-current-buffer chat
                       (buffer-substring-no-properties superchat-chat--input (point-max)))
                     ""))
      (should (string-match-p "⏸ queued: why?"
                              (with-current-buffer chat
                                (overlay-get superchat-chat--waiting 'before-string))))
      (should (= (length (superchat-tape-entries :tape "t1" :kinds '(user))) 1))
      (should (string-match-p (regexp-quote (buffer-name chat)) said))
      (should (string-match-p "queued" said)))))

(provide 'test-inline)
;;; test-inline.el ends here
