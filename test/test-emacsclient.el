;;; test-emacsclient.el --- ERT tests for the emacsclient adapter -*- lexical-binding: t; -*-

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'json)
(require 'superchat)
(require 'superchat-emacsclient)

(defun superchat-emacsclient-test--parse (json)
  "Parse adapter JSON JSON into alists for assertions."
  (json-parse-string json :object-type 'alist :array-type 'list))

(ert-deftest emacsclient/list-runs-returns-stable-json ()
  "List endpoint exposes the requested active Run fields."
  (cl-letf (((symbol-function 'superchat-runtime-active-runs)
             (lambda () '("run-1")))
            ((symbol-function 'superchat-runtime-status)
             (lambda (_run-id)
               '(:run-id "run-1" :status running-model
                 :session-id "session-1" :model "test-model"
                 :capability "chat" :started-at 123.5))))
    (let* ((object (superchat-emacsclient-test--parse
                    (superchat-emacsclient-list-runs)))
           (run (car (alist-get 'runs object))))
      (should (eq t (alist-get 'ok object)))
      (should (equal "run-1" (alist-get 'run_id run)))
      (should (equal "running-model" (alist-get 'status run)))
      (should (equal "session-1" (alist-get 'session_id run)))
      (should (equal "test-model" (alist-get 'model run)))
      (should (equal "chat" (alist-get 'capability run)))
      (should (= 123.5 (alist-get 'started_at run))))))

(ert-deftest emacsclient/errors-are-json-not-signals ()
  "Bad Run, Session, and argument inputs return JSON error objects."
  (dolist (json
           (list (superchat-emacsclient-run-status "missing")
                 (superchat-emacsclient-run-status nil)
                 (superchat-emacsclient-session-history "missing")
                 (superchat-emacsclient-session-history "session" -1)
                 (superchat-emacsclient-run-cancel "missing")))
    (let ((object (superchat-emacsclient-test--parse json)))
      (should (eq :false (alist-get 'ok object)))
      (should (stringp (alist-get 'message (alist-get 'error object)))))))

(ert-deftest emacsclient/history-is-bounded-and-overridable ()
  "History endpoint returns only the most recent requested rows."
  (let ((rows (mapcar (lambda (id)
                        (list id "topic" "kind" (format "row-%d" id)
                              "{}" "now"))
                      '(1 2 3 4))))
    (cl-letf (((symbol-function 'superchat-view-session-ids)
               (lambda () '("session-1")))
              ((symbol-function 'superchat-view-session-history)
               (lambda (_session-id) rows)))
      (let* ((object (superchat-emacsclient-test--parse
                      (superchat-emacsclient-session-history "session-1" 2)))
             (entries (alist-get 'entries object)))
        (should (eq t (alist-get 'ok object)))
        (should (= 2 (length entries)))
        (should (= 3 (alist-get 'id (car entries))))
        (should (= 4 (alist-get 'id (cadr entries))))
        (should (eq t (alist-get 'truncated object)))))))

(ert-deftest emacsclient/cancel-reuses-runtime-seam ()
  "Cancel endpoint delegates to `superchat-runtime-cancel'."
  (let (called)
    (cl-letf (((symbol-function 'superchat-runtime-status)
               (lambda (_run-id) '(:run-id "run-1" :status running-model)))
              ((symbol-function 'superchat-runtime-cancel)
               (lambda (run-id response)
                 (setq called (list run-id response))
                 t)))
      (let ((object
             (superchat-emacsclient-test--parse
              (superchat-emacsclient-run-cancel "run-1"))))
        (should (eq t (alist-get 'ok object)))
        (should (equal '("run-1" "[Cancelled via emacsclient]") called))))))

(ert-deftest emacsclient/headless-runtime-cancel-does-not-need-chat-timer ()
  "The shared cancel cleanup tolerates a headless Runtime buffer."
    (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (run-id "run-headless-cancel"))
    (superchat-runtime--put
     (list :run-id run-id :status 'running-model
           :buffer-name " *superchat-emacsclient-headless*"
           :session-id "session-headless-cancel"
           :turn-id "turn-headless-cancel"
           :actor-id "lead" :correlation-id run-id
           :steer-queue nil))
    (let* ((buffer (get-buffer-create " *superchat-emacsclient-headless*"))
           (was-bound (boundp 'superchat--active-timeout-timer))
           (old-value (and was-bound
                           (symbol-value 'superchat--active-timeout-timer))))
      (unwind-protect
          (progn
            (makunbound 'superchat--active-timeout-timer)
            (with-current-buffer buffer
              (makunbound 'superchat--active-timeout-timer)
              (should-not (boundp 'superchat--active-timeout-timer)))
            (cl-letf (((symbol-function 'superchat-runtime--finish)
                       (lambda (id status _response)
                         (superchat-runtime--update id :status status)
                         t))
                      ((symbol-function 'llm-cancel-request) #'ignore))
              (should (superchat-runtime-cancel run-id))))
        (if was-bound
            (set 'superchat--active-timeout-timer old-value)
          (makunbound 'superchat--active-timeout-timer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest emacsclient/adapter-is-an-optional-plugin ()
  "The adapter is loadable on demand and exposes autoload declarations."
  (should (featurep 'superchat-emacsclient))
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "superchat-emacsclient.el"
                       (file-name-directory (locate-library "superchat"))))
    (should (= 4 (how-many "^;;;###autoload$")))))

;;; test-emacsclient.el ends here
