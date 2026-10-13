;;; transport-capabilities-regression.el --- what the transport says a model can do -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 8b (architecture map of 2026-10-13, contradiction C9).  The
;; `model-transport' Service answers `capabilities MODEL' with
;; (:streaming BOOL :tools BOOL).  The default transport asked a function
;; named `superchat--llm-resolve-streaming'; the function is
;; `superchat-llm--resolve-streaming'.  An `fboundp' test and an
;; `ignore-errors' around the call hid the wrong name, so the Service said
;; "no streaming" for every model.  The request loop itself calls the right
;; name; only the Service's answer was wrong.
;;
;; The last test is the guard against the same thing happening again: every
;; function the transport file declares must exist in the file it names.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/transport-capabilities-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^transport-capabilities/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'check-declare)
(require 'superchat)
(require 'superchat-llm)
(require 'superchat-service)
(require 'superchat-transport-llm)

(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-model-capability-overrides)

(defmacro tcap--with (overrides &rest body)
  "Run BODY with the default transport mounted and OVERRIDES as the only
declared model capabilities.  There is no provider: the declared
capabilities are all the resolver has to go on."
  (declare (indent 1))
  `(let ((superchat-llm-backend nil)
         (superchat-llm-model nil)
         (superchat-llm-model-capability-overrides ,overrides)
         (mounted (superchat-plugin-state 'transport-llm)))
     (unless mounted (superchat-plugin-mount 'transport-llm))
     (unwind-protect (progn ,@body)
       (unless mounted (superchat-plugin-unmount 'transport-llm)))))

(defun tcap--ask (model)
  (superchat-service-call nil 'model-transport 'capabilities model))

(ert-deftest transport-capabilities/a-model-that-streams-is-said-to-stream ()
  (tcap--with '(("tcap-streams" :streaming t :streaming-tool-use t))
    ;; Guard: the resolver the request loop uses says this model streams.
    (should (plist-get (superchat-llm--resolve-streaming nil "tcap-streams" nil)
                       :streaming))
    (should (eq t (plist-get (tcap--ask "tcap-streams") :streaming)))))

(ert-deftest transport-capabilities/a-model-that-does-not-stream-is-said-not-to ()
  "Guard: the answer follows the model, it is not simply true."
  (tcap--with '(("tcap-streams" :streaming t :streaming-tool-use t))
    (should-not (plist-get (superchat-llm--resolve-streaming nil "tcap-plain" nil)
                           :streaming))
    (let ((answer (tcap--ask "tcap-plain")))
      (should (plist-member answer :streaming))
      (should-not (plist-get answer :streaming))
      (should (plist-get answer :tools)))))

(ert-deftest transport-capabilities/the-answer-is-the-resolvers ()
  "The Service asks the resolver the request loop asks, about the same model."
  (tcap--with nil
    (let (asked)
      (cl-letf (((symbol-function 'superchat-llm--resolve-streaming)
                 (lambda (backend model tools-p)
                   (push (list backend model tools-p) asked)
                   '(:streaming t :streaming-tool-use nil :source override))))
        (should (eq t (plist-get (tcap--ask "tcap-any") :streaming))))
      (should (equal '((nil "tcap-any" nil)) asked)))))

(ert-deftest transport-capabilities/a-resolver-that-fails-is-not-read-as-no-streaming ()
  "An error in the resolver reaches the caller; it is not turned into nil."
  (tcap--with nil
    (cl-letf (((symbol-function 'superchat-llm--resolve-streaming)
               (lambda (&rest _) (error "TCAP-BROKEN"))))
      (should (string-match-p
               "TCAP-BROKEN"
               (error-message-string (should-error (tcap--ask "tcap-any"))))))))

(ert-deftest transport-capabilities/every-function-the-transport-declares-exists ()
  "A renamed function cannot hide behind `declare-function' again."
  (let* ((file (expand-file-name "superchat-transport-llm.el"
                                 (file-name-directory (locate-library "superchat-transport-llm.el"))))
         (declared (with-temp-buffer
                     (insert-file-contents file)
                     (let (names)
                       (while (re-search-forward "^(declare-function \\([^ )]+\\) \"\\([^\"]+\\)\"" nil t)
                         (push (cons (intern (match-string 1)) (match-string 2)) names))
                       names))))
    (should declared)
    (dolist (entry declared)
      (require (intern (cdr entry)))
      (unless (fboundp (car entry))
        (ert-fail (format "%s declares %s in %s, which does not define it"
                          (file-name-nondirectory file) (car entry) (cdr entry)))))
    ;; Emacs's own checker reads the same declarations against the files.
    (should-not (let ((inhibit-message t)) (check-declare-file file)))))

(provide 'transport-capabilities-regression)
;;; transport-capabilities-regression.el ends here
