;;; test-inbox.el --- tests for superchat-inbox -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-inbox)

(defmacro superchat-test-inbox--with (&rest body)
  "BODY with a Tape, in a temporary directory, no run and no worker waiting."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((default-directory (file-name-as-directory (make-temp-file "sc-inbox-" t)))
           (superchat-inbox--timer nil)
           ;; The timer tools are write tools: no one is asked in these tests.
           (superchat-default-level 'auto))
       (unwind-protect (progn ,@body)
         (when superchat-inbox--timer (cancel-timer superchat-inbox--timer))
         (delete-directory default-directory t)))))

(defun superchat-test-inbox--idle ()
  "Wait until the worker has nothing more to start and no run goes on the
timelines the tests use."
  (superchat-test-wait (lambda () (and (null superchat-inbox--timer)
                                       (not (superchat-run-active "t"))
                                       (not (superchat-run-active "u"))))))

(ert-deftest superchat-inbox-items-put-together-are-one-run ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "All three, answered."))
      (let ((items (list (superchat-inbox-put "t" "one")
                         (superchat-inbox-put "t" "two")
                         (superchat-inbox-put "t" "three"))))
        (should (= 3 (length (superchat-inbox-pending "t"))))
        (superchat-test-inbox--idle)
        (should (= 1 (length superchat-test-requests)))
        (let ((user (car (superchat-tape-entries :tape "t" :kinds '(user)))))
          (should (equal (plist-get user :text)
                         "[1/3] one\n\n[2/3] two\n\n[3/3] three"))
          (should (equal (plist-get user :refs) (mapcar (lambda (e) (plist-get e :id)) items))))
        (should-not (superchat-inbox-pending))))))

(ert-deftest superchat-inbox-an-item-alone-is-answered-as-it-was-said ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "ok"))
      (superchat-inbox-put "t" "just one")
      (superchat-test-inbox--idle)
      (should (equal (plist-get (car (superchat-tape-entries :tape "t" :kinds '(user))) :text)
                     "just one")))))

(ert-deftest superchat-inbox-an-item-waits-for-the-run-that-goes ()
  (superchat-test-inbox--with
    (let ((superchat-test-async t))
      (superchat-test-script (list '(:text "first") '(:text "second"))
        (superchat-run "t" "go")
        (superchat-inbox-put "t" "later")
        (superchat-test-drain)
        (superchat-test-inbox--idle)
        (superchat-test-drain)
        (superchat-test-inbox--idle)
        (should (equal (mapcar (lambda (e) (plist-get e :text))
                               (superchat-tape-entries :tape "t" :kinds '(user)))
                       '("go" "later")))
        (should (= 2 (length superchat-test-requests)))))))

(ert-deftest superchat-inbox-an-item-from-a-source-is-put-once ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "ok"))
      (should (superchat-inbox-put "t" "build failed" :source "ci" :id "run-7"))
      (should-not (superchat-inbox-put "t" "build failed" :source "ci" :id "run-7"))
      (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(inbox)))))
      (superchat-test-inbox--idle))))

(ert-deftest superchat-inbox-what-was-pending-when-emacs-stopped-is-answered ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'inbox "left over" :scope "home"
                           :data '(:source nil :id nil :unattended t))
    (superchat-test-script (list '(:text "done"))
      (superchat-inbox-wake)
      (superchat-test-inbox--idle)
      (should (equal (plist-get (car (superchat-tape-entries :tape "t" :kinds '(user))) :text)
                     "left over"))
      (should-not (superchat-inbox-pending "t")))))

(ert-deftest superchat-inbox-an-unattended-item-waits-at-a-question ()
  (superchat-test-inbox--with
    (superchat-tool-define "poke" :class 'write :description "Poke" :function (lambda (_) "poked"))
    (unwind-protect
        (let ((superchat-default-level 'ask)
              (superchat-permission-ask-function (lambda (&rest _) (error "Asked in the minibuffer"))))
          (superchat-test-script (list '(:text "" :calls ((:id "c1" :name "poke" :args nil))))
            (let* ((inhibit-message t) (message-log-max t) (told nil)
                   (superchat-inbox-notify-functions (list (lambda (&rest args) (push args told)))))
              (superchat-inbox-put "t" "poke it" :unattended t)
              (superchat-test-inbox--idle)
              (should (equal (mapcar #'car told) '("t")))
              (should (eq (nth 2 (car told)) 'question)))
            (should (superchat-question "t"))
            (should (string-match-p "^Superchat: t waits for your answer: poke"
                                    (with-current-buffer "*Messages*" (buffer-string))))
            (superchat-inbox-put "t" "and then")
            (superchat-test-inbox--idle)
            ;; The new item waits behind the question.
            (should (= 1 (length (superchat-inbox-pending "t"))))))
      (superchat-tool-remove "poke"))))

(ert-deftest superchat-inbox-send-tells-the-person ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (let* ((said nil) (told nil)
           (superchat-inbox-notify-functions
            (list (lambda (&rest _) (error "Broken notifier"))
                  (lambda (&rest args) (push args told)))))
      (cl-letf (((symbol-function 'message) (lambda (&rest args) (push (apply #'format args) said))))
        (should (equal (plist-get (superchat-test-inbox--tool "send" :text "Build broke\nlog: ...") :text)
                       "Sent.")))
      ;; A notifier that fails is shown; the next one still runs.
      (should (equal (car (last said)) "Superchat: Build broke"))
      (should (string-match-p "notify .* failed: Broken notifier" (car said)))
      (should (equal told '(("t" "Build broke\nlog: ..." sent))))
      (let ((sent (car (superchat-tape-entries :tape "t" :kinds '(event)))))
        (should (equal (plist-get sent :data) '(:type "sent")))
        (should (equal (plist-get sent :text) "Build broke\nlog: ..."))))))

(ert-deftest superchat-inbox-only-a-run-nobody-watches-is-told-to-be-quiet ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "a") '(:text "b"))
      (superchat-inbox-put "t" "watched")
      (superchat-test-inbox--idle)
      (superchat-inbox-put "u" "alone" :unattended t)
      (superchat-test-inbox--idle)
      (should-not (string-search superchat-inbox--quiet
                                 (plist-get (nth 1 superchat-test-requests) :system)))
      (should (string-search superchat-inbox--quiet
                             (plist-get (nth 0 superchat-test-requests) :system))))))

(ert-deftest superchat-inbox-a-waiting-item-pauses-a-run-at-the-limit ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (let ((superchat-max-rounds 1) first
          (call '(:calls ((:id "c" :name "read" :args (:path "."))))))
      (superchat-test-script (list call '(:text "The item, answered."))
        (superchat-inbox-put "t" "news")
        (superchat-run "t" "go" :on-done (lambda (r) (setq first r)))
        ;; The item waits: no look-back, no going on.
        (should (plist-get first :paused))
        (superchat-test-inbox--idle)
        (should (= 2 (length superchat-test-requests))))
      (should-not (superchat-inbox-pending "t"))
      (should (equal (plist-get (car (last (superchat-tape-entries :tape "t" :kinds '(assistant))))
                                :text)
                     "The item, answered.")))))

(provide 'test-inbox)
