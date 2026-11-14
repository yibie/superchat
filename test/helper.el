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
Each response is (:text S :calls CALLS), or (:error S), or a function of
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
                                next)))
               (if-let* ((why (plist-get response :error)))
                   (funcall (plist-get keys :on-error) why)
                 (when-let* ((on-text (plist-get keys :on-text))
                             (text (plist-get response :text)))
                   (funcall on-text text))
                 (funcall (plist-get keys :on-done)
                          (list :text (or (plist-get response :text) "")
                                :calls (plist-get response :calls)
                                :model "scripted")))
               'scripted-handle))))
     ,@body))

(provide 'helper)
;;; helper.el ends here
