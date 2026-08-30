;;; test-control-channel.el --- Tests for the mid-Run control channel -*- lexical-binding: t; -*-

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
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
(defvar superchat--prompt-start)
(defvar superchat--queued-follow-up)
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
  "`superchat-cancel' cancels the active Run and opens a redirect prompt."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-cancel-test*"))
         cancelled redirects)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (setq superchat--active-run-id "run-x")
            (setq superchat--prompt-start nil)
            (setq superchat--current-command nil)
            (setq superchat--active-preset nil)
            (superchat--response-region-ensure-view-prompt)
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
              (should (string-suffix-p
                       "* User: 改读另一个文件" (buffer-string))))
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

(ert-deftest control/cancel-command-does-not-edit-synchronously-created-prompt ()
  "Former fills-synchronously-created-prompt: replacement input is not retargeted."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*superchat-cancel-prompt-test*")))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq superchat--active-run-id "run-x")
          (setq superchat--prompt-start nil)
          (setq superchat--current-command nil)
          (setq superchat--active-preset nil)
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("run-x")))
                    ((symbol-function 'superchat-runtime-cancel)
                     (lambda (_run-id &optional _response)
                       (superchat--insert-prompt)))
                    ((symbol-function 'read-string)
                     (lambda (&rest _) (ert-fail "No original draft to edit"))))
            (superchat-cancel)
            (should (string-suffix-p
                     "* User: " (buffer-string)))
            (should (markerp superchat--prompt-start))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/pause-requests-and-resumes ()
  "Pause sets the flag for the active Run; resume injects and continues."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*pause-test*"))
         (superchat--active-run-id "run-x")
         (superchat--pause-requested nil)
         (superchat--pending-send-request nil)
         (superchat--active-timeout-timer nil)
         (superchat-response-timeout 120)
         resumed steered)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create superchat-buffer-name)
            (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                       (lambda () '("run-x")))
                      ((symbol-function 'message) #'ignore))
              (superchat-pause)
              (should superchat--pause-requested))
            ;; boundary: only the target owner stops its first-answer wait
            (let* ((timer (run-with-timer 60 nil #'ignore))
                   (owner
                    (superchat-llm--make-transport-timer-owner
                     :buffer (current-buffer)
                     :run-id "run-x"
                     :publication-order
                     (superchat-llm--next-transport-timer-owner-order
                      (current-buffer))
                     :wait-timer timer)))
              (should
               (superchat-llm--transport-timer-owner-publish-wait
                owner timer))
              (setq superchat--pending-send-request
                    (superchat-llm--make-pending-pause
                     :run-id "run-x"
                     :transport-owner owner
                     :reason 'tool-followup
                     :continuation
                     (lambda (reason) (setq resumed reason))))
              (should (superchat--pause-boundary "run-x" owner))
              (should-not (memq timer timer-list))
              (should-not superchat--active-timeout-timer)
              (should-not
               (superchat-llm--transport-timer-owner-wait-timer owner)))
            ;; resume: record steer + re-invoke the identity-bound round
            (cl-letf (((symbol-function 'superchat-runtime-effect-capable-p)
                       (lambda (run-id) (equal run-id "run-x")))
                      ((symbol-function 'superchat-runtime-steer)
                       (lambda (run-id text)
                         (setq steered (list run-id text))
                         '(:event-id "evt-steer")))
                      ((symbol-function 'read-string)
                       (lambda (_prompt &optional _i _h) "换一个方向")))
              (superchat-resume)))
          (should-not superchat--pause-requested)
          (should (equal '("run-x" "换一个方向") steered))
          (should (eq 'tool-followup resumed)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest runtime/steer-queues-a-tape-event ()
  "A steer is durably recorded before Runtime exposes it to transport."
  (superchat-runtime-test--isolated
    (let (events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push event events)
                   (length events)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) 'provider-request)))
        (let* ((handle (superchat-runtime-submit
                        (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id))
               (record (superchat-runtime-steer run-id "take another path"))
               (event (car events)))
          (should (string-prefix-p "evt_" (plist-get record :event-id)))
          (should (equal "run.steered" (plist-get event :type)))
          (should (equal "take another path"
                         (plist-get (plist-get event :payload) :instruction)))
          (should (equal '("take another path")
                         (superchat-runtime--drain-steers run-id)))
          (should-not (superchat-runtime--drain-steers run-id)))))))

(ert-deftest control/pause-without-run-is-a-noop ()
  "Pause and resume report cleanly when nothing is running."
  (let ((superchat--active-run-id nil)
        (superchat--pause-requested nil))
    (cl-letf (((symbol-function 'superchat-runtime-active-runs)
               (lambda () nil))
              ((symbol-function 'message) #'ignore))
      (superchat-pause)
      (should-not superchat--pause-requested)
      (superchat-resume))))

(ert-deftest control/keymap-has-resume-binding ()
  "C-c C-r resumes a paused Run in `superchat-mode'."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-r"))
              'superchat-resume)))

(ert-deftest control/follow-up-edits-one-visible-draft-without-steering ()
  "Former hidden-queue test: the visible draft is edited, never auto-executed."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*queued-follow-up-test*"))
         (steered nil)
         (submitted nil)
         messages
         (superchat--queued-follow-up nil))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq-local superchat--queued-follow-up nil)
          (cl-letf (((symbol-function 'read-string)
                     (let ((answers '("first thing" "second thing" "second thing"))
                           (initials '("" "first thing" "second thing")))
                       (lambda (_prompt &optional initial &rest _)
                         (should (equal initial (pop initials)))
                         (pop answers))))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'superchat-runtime-steer)
                     (lambda (&rest _)
                       (setq steered t)))
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (&rest _) (setq submitted t))))
            (superchat-queue-follow-up)
            (let ((prompt (superchat--response-region--view-prompt)))
              (should prompt)
              (should (equal "first thing" (buffer-substring prompt (point-max))))
              (dotimes (_ 2)
                (superchat-queue-follow-up)
                (should (eq prompt (superchat--response-region--view-prompt)))
                (should (equal "second thing" (buffer-substring prompt (point-max))))
                (should-not (get-text-property prompt 'read-only))
                (should-not superchat--queued-follow-up))
              (should (= 1 (how-many "^\\* User" (point-min) (point-max)))))
            (should-not steered)
            (should-not submitted)
            (should (= 3 (cl-count "Draft updated; not sent." messages :test #'equal)))
            (should-not (superchat--mode-line-run-state))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/queued-follow-up-does-not-auto-send ()
  "Consuming a queued follow-up only fills the prompt."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*queued-follow-up-no-send-test*"))
        (superchat--queued-follow-up "do this later")
        sent)
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda () (setq sent t))))
            (setq-local superchat--queued-follow-up "do this later")
            (superchat--insert-prompt)
            (should-not sent)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest control/terminal-notifies-undelivered-steers ()
  "A terminal Run reports queued steers that could not be delivered."
  (let (notice)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq notice (apply #'format format-string args)))))
      (superchat--notify-run-terminal
       '(:run-id "run-x" :status completed :undelivered-steer-count 1)))
    (should (equal "superchat: run run-x ended; 1 steer instruction not delivered"
                   notice))))

;;; test-control-channel.el ends here
