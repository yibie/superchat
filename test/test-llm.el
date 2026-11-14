;;; test-llm.el --- tests for superchat-llm.el -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-llm)

;; A provider that needs no network.  It implements the generics the real
;; providers implement (streaming, and how tool uses are written into a
;; prompt, as llm-openai does) and feeds canned responses through llm.el's
;; own `llm-provider-utils-process-result', so the real path is exercised.
(cl-defstruct (test-llm-provider (:include llm-standard-chat-provider))
  (chat-model "base"))

(defvar test-llm--script nil
  "Responses, in order: (:chunks LIST :tool-uses LIST :error S :defer BOOL).")
(defvar test-llm--seen nil "What the provider received, newest first.")
(defvar test-llm--prompts nil "The prompt objects received, newest first.")
(defvar test-llm--deferred nil "A thunk delivering a response that was held back.")

(cl-defmethod llm-provider-populate-tool-uses ((_ test-llm-provider) prompt uses)
  (llm-provider-utils-append-to-prompt prompt uses nil nil 'assistant))

(cl-defmethod llm-chat-streaming ((p test-llm-provider) prompt partial success
                                  failure &optional multi-output)
  (push prompt test-llm--prompts)
  (push (list :model (test-llm-provider-chat-model p)
              :system (llm-chat-prompt-context prompt)
              :tools (mapcar (lambda (tool) (list (llm-tool-name tool)
                                                  (llm-tool-args tool)))
                             (llm-chat-prompt-tools prompt))
              :interactions
              (mapcar (lambda (i)
                        (list (llm-chat-prompt-interaction-role i)
                              (llm-chat-prompt-interaction-content i)
                              (mapcar (lambda (r)
                                        (list (llm-chat-prompt-tool-result-call-id r)
                                              (llm-chat-prompt-tool-result-tool-name r)
                                              (llm-chat-prompt-tool-result-result r)))
                                      (llm-chat-prompt-interaction-tool-results i))))
                      (llm-chat-prompt-interactions prompt)))
        test-llm--seen)
  (let* ((response (pop test-llm--script))
         (uses (plist-get response :tool-uses))
         (acc nil)
         (deliver
          (lambda ()
            (dolist (chunk (plist-get response :chunks))
              (setq acc (llm-provider-utils-streaming-accumulate acc (list :text chunk)))
              (funcall partial acc))
            (llm-provider-utils-process-result
             p prompt
             (append (list :text (apply #'concat (plist-get response :chunks)))
                     (when uses (list :tool-uses uses)))
             multi-output success failure))))
    (when-let* ((why (plist-get response :error)))
      (setq deliver (lambda () (funcall failure 'error why))))
    (if (plist-get response :defer)
        (setq test-llm--deferred deliver)
      (funcall deliver))
    'test-request))

(defmacro test-llm-with (script &rest body)
  (declare (indent 1) (debug t))
  `(let ((test-llm--script ,script) (test-llm--seen nil) (test-llm--prompts nil)
         (test-llm--deferred nil)
         (superchat-llm-provider (make-test-llm-provider))
         (superchat-llm-model nil))
     ,@body))

(defun test-llm--ask (messages &rest keys)
  "Request, collecting what the callbacks got into a plist."
  (let (text done error)
    (apply #'superchat-llm-request messages
           :on-text (lambda (s) (push s text))
           :on-done (lambda (r) (push r done))
           :on-error (lambda (e) (push e error))
           keys)
    (list :text (nreverse text) :done (nreverse done) :error (nreverse error))))

(ert-deftest test-llm-plain-text ()
  (test-llm-with '((:chunks ("Hello")))
    (let ((r (test-llm--ask '((:role user :text "hi")) :system "be brief")))
      (should (equal (plist-get r :done) '((:text "Hello" :calls nil :model "default"))))
      (should-not (plist-get r :error))
      (should (equal (plist-get (car test-llm--seen) :system) "be brief"))
      (should (equal (plist-get (car test-llm--seen) :interactions)
                     '((user "hi" nil)))))))

(ert-deftest test-llm-streams-deltas ()
  (test-llm-with '((:chunks ("Hel" "lo" " there")))
    (let ((r (test-llm--ask '((:role user :text "hi")))))
      (should (equal (plist-get r :text) '("Hel" "lo" " there")))
      (should (equal (plist-get (car (plist-get r :done)) :text) "Hello there")))))

(ert-deftest test-llm-tool-calls-come-back-unrun ()
  (test-llm-with
      (list (list :chunks '("Looking.")
                  :tool-uses (list (make-llm-provider-utils-tool-use
                                    :id "p1" :name "read_file"
                                    :args '((path . "a.el") (count . 5)))
                                   (make-llm-provider-utils-tool-use
                                    :id "p2" :name "list_buffers"
                                    :args '((all . :json-false))))))
    (let* ((r (test-llm--ask '((:role user :text "go"))
                             :tools '((:name "read_file" :description "Read"
                                       :args ((:name "path" :type string
                                               :description "Where" :required t)
                                              (:name "count" :type number :required nil)))
                                      (:name "list_buffers" :description "List"
                                       :args ((:name "all" :type boolean :required nil))))))
           (done (car (plist-get r :done)))
           (calls (plist-get done :calls)))
      (should (equal (plist-get done :text) "Looking."))
      (should (equal (mapcar (lambda (c) (plist-get c :name)) calls)
                     '("read_file" "list_buffers")))
      (should (equal (plist-get (car calls) :args) '(:path "a.el" :count 5)))
      (should (equal (plist-get (cadr calls) :args) '(:all nil)))
      (should (stringp (plist-get (car calls) :id)))
      (should-not (equal (plist-get (car calls) :id) (plist-get (cadr calls) :id)))
      (should-not (plist-get r :error))
      ;; The provider was asked exactly once: llm.el made no follow-up request.
      (should (= 1 (length test-llm--seen)))
      (should (equal (plist-get (car test-llm--seen) :tools)
                     '(("read_file" ((:name "path" :type string :description "Where"
                                      :optional t)
                                     (:name "count" :type number :description nil
                                      :optional t)))
                       ("list_buffers" ((:name "all" :type boolean :description nil
                                         :optional t)))))))))

(ert-deftest test-llm-one-provider-call-per-request ()
  (test-llm-with
      (list (list :tool-uses (list (make-llm-provider-utils-tool-use
                                    :id "p1" :name "t" :args nil)))
            '(:chunks ("next")))
    (let ((tools '((:name "t" :description "T" :args nil))))
      (test-llm--ask '((:role user :text "a")) :tools tools)
      (should (= 1 (length test-llm--seen)))
      (test-llm--ask '((:role user :text "b")) :tools tools)
      (should (= 2 (length test-llm--seen))))))

(ert-deftest test-llm-replays-calls-and-results-natively ()
  (test-llm-with '((:chunks ("Done.")))
    (test-llm--ask
     '((:role user :text "go")
       (:role assistant :text "Looking."
        :calls ((:id "c1" :name "read_file" :args (:path "a.el" :count 5))
                (:id "c2" :name "shell" :args (:command "ls"))))
       (:role tool :call-id "c1" :name "read_file" :text "contents")
       (:role tool :call-id "c2" :name "shell" :text "files"))
     :tools '((:name "read_file" :description "Read" :args nil)))
    (let ((seen (plist-get (car test-llm--seen) :interactions)))
      (should (= 4 (length seen)))
      (should (equal (nth 0 seen) '(user "go" nil)))
      (should (equal (nth 1 seen) '(assistant "Looking." nil)))
      (pcase-let ((`(,role ,uses ,results) (nth 2 seen)))
        (should (eq role 'assistant))
        (should (equal (mapcar #'llm-provider-utils-tool-use-id uses) '("c1" "c2")))
        (should (equal (mapcar #'llm-provider-utils-tool-use-name uses) '("read_file" "shell")))
        (should (equal (llm-provider-utils-tool-use-args (car uses))
                       '((path . "a.el") (count . 5))))
        (should-not results))
      (should (equal (nth 3 seen)
                     '(tool-results nil
                       (("c1" "read_file" "contents") ("c2" "shell" "files"))))))))

(ert-deftest test-llm-provider-error-is-a-string ()
  (test-llm-with '((:error "rate limited"))
    (let ((r (test-llm--ask '((:role user :text "hi")))))
      (should (equal (plist-get r :error) '("rate limited")))
      (should-not (plist-get r :done)))))

(ert-deftest test-llm-cancel-silences-both-callbacks ()
  ;; The provider's callback still fires after the cancel, as success or failure.
  (dolist (response '((:chunks ("a" "b") :defer t) (:error "late" :defer t)))
    (test-llm-with (list response)
      (let* (calls
             (handle (superchat-llm-request
                      '((:role user :text "hi"))
                      :on-text (lambda (s) (push s calls))
                      :on-done (lambda (_) (push 'done calls))
                      :on-error (lambda (_) (push 'error calls)))))
        (superchat-llm-cancel handle)
        (funcall test-llm--deferred)
        (should-not calls)))))

(ert-deftest test-llm-every-call-comes-back ()
  (let ((tools '((:name "read_file" :description "Read"
                  :args ((:name "path" :type string :required t)))))
        (use (lambda (name args)
               (make-llm-provider-utils-tool-use :id "p" :name name :args args))))
    ;; An unknown tool beside a good call, and a call missing a required argument.
    (test-llm-with (list (list :tool-uses (list (funcall use "nope" '((x . 1)))
                                                (funcall use "read_file" nil))))
      (let* ((r (test-llm--ask '((:role user :text "go")) :tools tools))
             (calls (plist-get (car (plist-get r :done)) :calls)))
        (should-not (plist-get r :error))
        (should (equal (mapcar (lambda (c) (plist-get c :name)) calls)
                       '("nope" "read_file")))
        (should (equal (plist-get (car calls) :args) '(:x 1)))))
    ;; llm.el refuses a response whose only call is unknown: a sentence, not Lisp data.
    (test-llm-with (list (list :tool-uses (list (funcall use "nope" nil))))
      (let ((r (test-llm--ask '((:role user :text "go")) :tools tools)))
        (should-not (plist-get r :done))
        (should (equal (plist-get r :error)
                       '("The model asked for a tool that is not available: nope")))))))

(ert-deftest test-llm-start-failure-reaches-on-error ()
  (test-llm-with nil
    (cl-letf (((symbol-function 'llm-chat-streaming)
               (lambda (&rest _) (signal 'llm-provider-unconfigured '("no key")))))
      (let ((r (test-llm--ask '((:role user :text "hi")))))
        (should (= 1 (length (plist-get r :error))))
        (should (string-match-p "no key" (car (plist-get r :error))))
        (should-not (plist-get r :done))))))

(cl-defstruct (test-llm-slotless (:include llm-standard-chat-provider)))

(ert-deftest test-llm-model-needs-a-chat-model-slot ()
  (let ((superchat-llm-provider (make-test-llm-slotless)))
    (let ((r (test-llm--ask '((:role user :text "hi")) :model "m")))
      (should (string-match-p "test-llm-slotless" (car (plist-get r :error)))))))

(ert-deftest test-llm-only-the-first-callback-counts ()
  (test-llm-with '((:chunks ("a") :defer t))
    (let (calls)
      (superchat-llm-request '((:role user :text "hi"))
                             :on-done (lambda (_) (push 'done calls))
                             :on-error (lambda (_) (push 'error calls)))
      (funcall test-llm--deferred)
      (funcall test-llm--deferred)
      (should (equal calls '(done))))))

(ert-deftest test-llm-needs-a-provider ()
  (let ((superchat-llm-provider nil))
    (let ((err (should-error (superchat-llm-request '((:role user :text "hi")))
                             :type 'user-error)))
      (should (string-match-p "superchat-llm-provider" (cadr err))))))

(ert-deftest test-llm-model-reaches-the-provider ()
  (test-llm-with '((:chunks ("x")) (:chunks ("y")))
    (let ((r (test-llm--ask '((:role user :text "hi")) :model "big")))
      (should (equal (plist-get (car test-llm--seen) :model) "big"))
      (should (equal (plist-get (car (plist-get r :done)) :model) "big")))
    (let ((superchat-llm-model "small"))
      (test-llm--ask '((:role user :text "hi")))
      (should (equal (plist-get (car test-llm--seen) :model) "small")))
    (should (equal (test-llm-provider-chat-model superchat-llm-provider) "base"))))

(ert-deftest test-llm-request-calls-the-request-function ()
  (let* ((got nil)
         (superchat-llm-request-function
          (lambda (messages &rest keys) (setq got (cons messages keys)) 'handle)))
    (should (eq 'handle (superchat-llm-request '((:role user :text "hi")) :system "s")))
    (should (equal (car got) '((:role user :text "hi"))))
    (should (equal (plist-get (cdr got) :system) "s"))))

(provide 'test-llm)
;;; test-llm.el ends here
