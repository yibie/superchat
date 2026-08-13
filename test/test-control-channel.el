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
(defvar superchat--steer-start-marker)
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
  "`superchat-cancel' cancels the chat buffer's active Run or reports none."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-cancel-test*"))
         cancelled)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (setq superchat--active-run-id "run-x")
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () '("run-x")))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (run-id &optional _response)
                         (setq cancelled run-id))))
              (superchat-cancel)
              (should (equal cancelled "run-x")))
            (setq superchat--active-run-id nil)
            (setq cancelled nil)
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () nil))
                      ((symbol-function 'superchat-runtime-cancel)
                       (lambda (_run-id &optional _response)
                         (setq cancelled t))))
              (superchat-cancel)
              (should-not cancelled))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/queue-input-mid-run ()
  "C-c C-c during an active Run queues steer text instead of sending."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-steer-queue-test*"))
         alerts)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (erase-buffer)
            (setq superchat--active-run-id "run-x")
            (setq superchat--prompt-start nil)
            (setq superchat--queued-input nil)
            (insert "* Steer: ")
            (setq superchat--steer-start-marker (point-marker))
            (insert "read the other file")
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () '("run-x")))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) alerts))))
              (superchat-send-input))
            (should (equal "read the other file"
                           superchat--queued-input))
            (should (string-match-p "Queued" (car alerts)))
            (should-not
             (string-match-p "read the other file" (buffer-string)))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/insert-prompt-prefills-queued-input ()
  "The next prompt pre-fills queued mid-Run input and clears the queue."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-steer-prefill-test*")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (erase-buffer)
            (setq superchat--current-command nil)
            (setq superchat--active-preset nil)
            (setq superchat--queued-input "follow up")
            (setq superchat--steer-start-marker nil)
            (setq superchat--prompt-start nil)
            (superchat--insert-prompt)
            (should (string-match-p
                     "\\* User: follow up" (buffer-string)))
            (should (markerp superchat--prompt-start))
            (should (null superchat--queued-input))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/collect-steer-input-removes-line ()
  "Leftover steer text is collected into the prompt at Run completion."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-steer-collect-test*"))
         (superchat--steer-start-marker nil))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (erase-buffer)
            (insert "\n* Steer: ")
            (setq superchat--steer-start-marker (point-marker))
            (insert "typed text")
            (should (equal "typed text"
                           (superchat--collect-steer-input)))
            (should (null superchat--steer-start-marker))
            (should-not (string-match-p "Steer" (buffer-string)))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/stream-insertion-point-respects-steer-marker ()
  "Streaming appends above an active steer region, never into it."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-steer-point-test*")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (erase-buffer)
            (insert "** Assistant\n")
            (let ((before (point-max)))
              (should (= before (superchat--stream-insertion-point))))
            (insert "* Steer: ")
            (setq superchat--steer-start-marker (point-marker))
            (let ((label-end (point)))
              (insert "user text")
              (should (= label-end
                         (superchat--stream-insertion-point)))
              (should (< label-end (point-max))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

;;; test-control-channel.el ends here
