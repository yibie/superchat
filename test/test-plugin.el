;;; test-plugin.el --- tests for superchat-plugin -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-plugin)

(defvar superchat-test-plugin-log nil)

(defmacro superchat-test-plugins (&rest body)
  "Run BODY with an empty plugin registry."
  (declare (indent 0) (debug t))
  `(let ((superchat-plugin--definitions nil)
         (superchat-plugin--enabled nil)
         (superchat-plugins nil)
         (superchat-test-plugin-log nil))
     ,@body))

(defun superchat-test-plugin-define (name)
  "Define NAME so that it logs `on' and `off' calls to the log variable."
  (superchat-plugin-define
   name :doc "test"
   :enable (lambda () (push (list 'on name) superchat-test-plugin-log))
   :disable (lambda () (push (list 'off name) superchat-test-plugin-log))))

(ert-deftest superchat-plugin-define-does-not-enable ()
  (superchat-test-plugins
    (superchat-test-plugin-define 'a)
    (should-not (superchat-plugin-enabled-p 'a))
    (should-not superchat-test-plugin-log)))

(ert-deftest superchat-plugin-enable-once ()
  (superchat-test-plugins
    (superchat-test-plugin-define 'a)
    (superchat-plugin-enable 'a)
    (superchat-plugin-enable 'a)
    (should (superchat-plugin-enabled-p 'a))
    (should (equal superchat-test-plugin-log '((on a))))))

(ert-deftest superchat-plugin-disable-once ()
  (superchat-test-plugins
    (superchat-test-plugin-define 'a)
    (superchat-plugin-disable 'a)
    (should-not superchat-test-plugin-log)
    (superchat-plugin-enable 'a)
    (superchat-plugin-disable 'a)
    (superchat-plugin-disable 'a)
    (should-not (superchat-plugin-enabled-p 'a))
    (should (equal superchat-test-plugin-log '((off a) (on a))))))

(ert-deftest superchat-plugin-undefined-name-is-an-error ()
  (superchat-test-plugins
    (dolist (fn '(superchat-plugin-enable superchat-plugin-disable))
      (let ((err (should-error (funcall fn 'nope))))
        (should (string-match-p "nope" (error-message-string err)))))))

(ert-deftest superchat-plugin-redefine-disables-old-first ()
  (superchat-test-plugins
    (superchat-test-plugin-define 'a)
    (superchat-plugin-enable 'a)
    (superchat-plugin-define 'a :enable (lambda () (push 'new-on superchat-test-plugin-log)))
    (should (equal superchat-test-plugin-log '((off a) (on a))))
    (should-not (superchat-plugin-enabled-p 'a))
    (superchat-plugin-enable 'a)
    (should (eq (car superchat-test-plugin-log) 'new-on))))

(ert-deftest superchat-plugin-enable-configured-enables-missing ()
  (superchat-test-plugins
    (superchat-test-plugin-define 'a)
    (superchat-test-plugin-define 'b)
    (superchat-plugin-enable 'a)
    (setq superchat-plugins '(a b))
    (superchat-plugin-enable-configured)
    (should (superchat-plugin-enabled-p 'b))
    (should (equal superchat-test-plugin-log '((on b) (on a))))))

(ert-deftest superchat-plugin-enable-configured-names-undefined ()
  (superchat-test-plugins
    (setq superchat-plugins '(ghost))
    (let ((err (should-error (superchat-plugin-enable-configured))))
      (should (string-match-p "ghost" (error-message-string err))))))

(ert-deftest superchat-plugin-enable-error-reaches-caller ()
  (superchat-test-plugins
    (superchat-plugin-define 'bad :enable (lambda () (error "Boom from bad")))
    (let ((err (should-error (superchat-plugin-enable 'bad))))
      (should (string-match-p "Boom from bad" (error-message-string err))))
    (should-not (superchat-plugin-enabled-p 'bad))))

(provide 'test-plugin)
;;; test-plugin.el ends here
