;;; helper.el --- what every test file uses -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)

(defvar superchat-llm-request-function)
(declare-function superchat-tape-open "superchat-tape")
(declare-function superchat-tape-close "superchat-tape")

(defmacro superchat-test-with-tape (&rest body)
  "Run BODY with a fresh Tape in a temporary directory of its own.
The directory deleted afterwards is the one made here, whatever BODY
binds `superchat-directory' to."
  (declare (indent 0) (debug t))
  `(let* ((superchat-test--made
           (file-name-as-directory (make-temp-file "superchat-test-" t)))
          (superchat-directory superchat-test--made)
          (superchat-tape-file nil))
     (unwind-protect
         (progn (superchat-tape-open) ,@body)
       (superchat-tape-close)
       (delete-directory superchat-test--made t))))

(defvar superchat-test-requests nil
  "What the scripted model was asked, newest first: plists
\(:messages :system :tools :model).")

(defmacro superchat-test-script (responses &rest body)
  "Run BODY with a model that answers from RESPONSES, in order.
Each response is (:text S :calls CALLS), or (:error S :kind K :status N),
K default `other' and N optional, or a function of
the request plist returning one of those.  Each is delivered at once, the
text first through ON-TEXT.  Asking after the last response is an error."
  (declare (indent 1) (debug t))
  `(let* ((left (copy-sequence ,responses))
          (superchat-test-requests nil)
          (superchat-llm-request-function
           (lambda (messages &rest keys)
             (push (list :messages messages :system (plist-get keys :system)
                         :tools (plist-get keys :tools) :model (plist-get keys :model))
                   superchat-test-requests)
             (unless left (error "The scripted model has no response left"))
             (let* ((next (pop left))
                    (response (if (functionp next)
                                  (funcall next (car superchat-test-requests))
                                next))
                    (handle (list :scripted t :cancelled nil)))
               (superchat-test--deliver
                handle
                (lambda ()
                  (if-let* ((why (plist-get response :error)))
                      (funcall (plist-get keys :on-error)
                               (append (list :message why :kind (or (plist-get response :kind) 'other))
                                       (and (plist-get response :status)
                                            (list :status (plist-get response :status)))))
                    (when-let* ((on-text (plist-get keys :on-text))
                                (text (plist-get response :text)))
                      (funcall on-text text))
                    (funcall (plist-get keys :on-done)
                             (list :text (or (plist-get response :text) "")
                                   :calls (plist-get response :calls)
                                   :model "scripted")))))
               handle))))
     ,@body))

(defvar superchat-test-async nil
  "Non-nil makes the scripted model answer from a timer, as a real one does:
after the request call has returned, and with quitting off.")

(defvar superchat-test--pending 0 "Answers not delivered yet.")

(defun superchat-test--deliver (handle thunk)
  "Call THUNK now, or from a timer when `superchat-test-async' is set.
A request cancelled meanwhile with `superchat-llm-cancel', which sets
:cancelled on HANDLE, is not answered."
  (if (not superchat-test-async)
      (funcall thunk)
    (cl-incf superchat-test--pending)
    (run-at-time 0 nil (lambda ()
                         (cl-decf superchat-test--pending)
                         (unless (plist-get handle :cancelled)
                           (funcall thunk))))))

(defun superchat-test-drain ()
  "Wait until every answer of the asynchronous scripted model was delivered."
  (with-timeout (5 (error "The scripted model did not finish"))
    (while (> superchat-test--pending 0)
      (accept-process-output nil 0.01))))

(defun superchat-test-wait (predicate &optional seconds)
  "Wait until PREDICATE returns non-nil, for at most SECONDS (default 3):
for a tool that answers from a timer or a process."
  (with-timeout ((or seconds 3) (error "Waited for %S in vain" predicate))
    (while (not (funcall predicate))
      (accept-process-output nil 0.01))))

(provide 'helper)
;;; helper.el ends here
