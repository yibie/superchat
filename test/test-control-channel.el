;;; test-control-channel.el --- Tests for the mid-Run control channel -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-runtime)
(require 'superchat-dispatcher)
(require 'superchat-render)

;; Product variables must be special in this lexical test file so the
;; dynamic bindings are visible to product code running in chat buffers.
(defvar superchat-buffer-name)
(defvar superchat--active-run-id)
(defvar superchat--queued-input)
(defvar superchat--prompt-start)
(defvar superchat--current-command)
(defvar superchat--active-preset)

(ert-deftest runtime/active-runs-tracks-nonterminal-runs ()
  "Non-terminal Runs are listed; terminal Runs drop out."
  (superchat-runtime-test--isolated
    (let ((provider-calls 0))
      (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (cl-incf provider-calls))))
        (let* ((handle (superchat-runtime-submit
                        (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (member run-id (superchat-runtime-active-runs)))
          (should (= 1 provider-calls))
          (superchat-runtime-cancel run-id)
          (should-not (member run-id (superchat-runtime-active-runs))))))))

(ert-deftest control/cancel-command-cancels-active-run ()
  "`superchat-cancel' cancels the active Run and queues a redirect prompt."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-cancel-test*"))
         cancelled redirects)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (setq superchat--active-run-id "run-x")
            (setq superchat--queued-input nil)
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () '("run-x")))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (run-id &optional _response)
                         (setq cancelled run-id)))
                      ((symbol-function 'read-string)
                       (lambda (_prompt &optional _initial _history)
                         "改读另一个文件")))
              (superchat-cancel)
              (should (equal cancelled "run-x"))
              (should (equal "改读另一个文件"
                             superchat--queued-input)))
            (setq superchat--active-run-id nil)
            (setq cancelled nil)
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () nil))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (_run-id &optional _response)
                         (setq cancelled t))))
              (superchat-cancel)
              (should-not cancelled)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name))))))

(ert-deftest control/cancel-command-fills-synchronously-created-prompt ()
  "A redirect fills the prompt created synchronously by cancellation."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*superchat-cancel-prompt-test*")))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq superchat--active-run-id "run-x")
          (setq superchat--queued-input nil)
          (setq superchat--prompt-start nil)
          (setq superchat--current-command nil)
          (setq superchat--active-preset nil)
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("run-x")))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (_run-id &optional _response)
                       (superchat--insert-prompt)))
                    ((symbol-function 'read-string)
                     (lambda (_prompt &optional _initial _history)
                       (should (marker-position superchat--prompt-start))
                       "改读另一个文件")))
            (superchat-cancel)
            (should (string-suffix-p
                     "* User: 改读另一个文件" (buffer-string)))
            (should (null superchat--queued-input))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/insert-prompt-prefills-queued-input ()
  "The next prompt pre-fills queued mid-Run input and clears the queue."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-prefill-test*")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (erase-buffer)
            (setq superchat--current-command nil)
            (setq superchat--active-preset nil)
            (setq superchat--queued-input "follow up")
            (setq superchat--prompt-start nil)
            (superchat--insert-prompt)
            (should (string-match-p
                     "\\* User: follow up" (buffer-string)))
            (should (markerp superchat--prompt-start))
            (should (null superchat--queued-input))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

;;; test-control-channel.el ends here
