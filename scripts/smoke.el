;;; smoke.el --- migration smoke cases A-F (spec §5) -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
;;         -L <llm> -L <plz> … -l scripts/smoke.el [CASE…]
;; Env: SMOKE_BASELINE=1 writes scripts/smoke-expected/<CASE>.txt instead of
;; comparing; SMOKE_REAL=1 uses LM Studio (localhost:1234) for case A instead
;; of the scripted network.  Exit status 1 on any mismatch.
;;
;; Each case runs in a throwaway HOME/data directory with a scripted
;; llm.el network (the deleted journey fixture's mechanism), drives the real
;; product entry points, and prints the Tape event-type sequence plus a few
;; facts.  These lines are the baseline every migration step must reproduce.

(require 'cl-lib)
(require 'subr-x)
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)
(require 'superchat)
(require 'superchat-inline)
(require 'superchat-rewrite)

(defvar smoke--script nil)
(defvar smoke--requests nil)
(defvar smoke--timers nil)
(defvar smoke--failures 0)
(defvar smoke--expected-dir
  (expand-file-name "smoke-expected/" (file-name-directory load-file-name)))

;;;; scripted network -------------------------------------------------------

(defun smoke--tool-use (use)
  (make-llm-provider-utils-tool-use
   :id (format "smoke-call-%06x" (random #x1000000)) :name (car use) :args (cdr use)))

(defun smoke--deliver (provider prompt partial response error multi step)
  (dolist (chunk (plist-get step :chunks))
    (when partial (funcall partial (if multi (list :text chunk) chunk))))
  (cond
   ((plist-get step :hang) nil)
   ((plist-get step :error)
    (funcall error (car (plist-get step :error)) (cdr (plist-get step :error))))
   (t (let* ((uses (mapcar #'smoke--tool-use (plist-get step :tool-uses)))
             (result (append (when (plist-member step :text) (list :text (plist-get step :text)))
                             (when uses (list :tool-uses uses)))))
        (llm-provider-utils-process-result provider prompt result multi response error)))))

(defun smoke--fake-network (mode provider prompt partial response error multi)
  (push (list :mode mode
              :prompt-text (condition-case nil (llm-chat-prompt-to-text prompt)
                             (error (prin1-to-string prompt)))
              :tools (mapcar #'llm-tool-name (or (ignore-errors (llm-chat-prompt-tools prompt)) '())))
        smoke--requests)
  (let* ((step (or (pop smoke--script) (list :text "[script exhausted]")))
         (transport (generate-new-buffer " *smoke-transport*")))
    (push (run-at-time (or (plist-get step :delay) 0.01) nil #'smoke--deliver
                       provider prompt partial response error multi step)
          smoke--timers)
    transport))

(defun smoke--wait (predicate &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 8.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (sit-for 0.005))
    (funcall predicate)))

(defun smoke--run-done-p (run-id)
  (not (member run-id (superchat-runtime-active-runs))))

(defun smoke--types (session-id)
  (mapcar (lambda (e) (plist-get e :type)) (superchat-tape-replay-events session-id)))

;;;; isolation ----------------------------------------------------------------

(defmacro smoke--with-fixture (script &rest body)
  (declare (indent 1))
  `(let* ((home (make-temp-file "superchat-smoke-home-" t))
          (data (file-name-as-directory (make-temp-file "superchat-smoke-data-" t)))
          (real (getenv "SMOKE_REAL"))
          (process-environment (copy-sequence process-environment))
          (user-emacs-directory (file-name-as-directory home))
          (default-directory data)
          (superchat-data-directory data)
          (superchat-skills-directory (expand-file-name "skills/" data))
          (superchat-skills-agents-md-file (expand-file-name "AGENTS.md" data))
          (superchat-db--connection nil) (superchat-db--path nil)
          (superchat-editor-workspace--states (make-hash-table :test #'equal))
          (superchat-runtime--runs (make-hash-table :test #'equal))
          (superchat-runtime-default-deadline nil)
          (superchat--current-context-files nil)
          (superchat-closure-terminal-guard-enabled t)
          (superchat-llm-tools-list nil) (superchat--llm-tool-names-signature nil)
          (superchat--model-list-cache nil) (superchat-show-ttft nil)
          (superchat-response-timeout nil) (superchat-llm-model nil)
          (superchat-llm-tools-enabled 'on-demand)
          (superchat-authority-approval-function (lambda (_request) t))
          (superchat-rewrite-confirmation 'overlay)
          (superchat-llm-backend
           (if real
               (make-llm-openai-compatible :url "http://localhost:1234/v1"
                                           :chat-model (or (getenv "SMOKE_MODEL") "qwen/qwen3.8-27b")
                                           :key "lm-studio")
             (make-llm-openai-compatible :url "http://127.0.0.1:9/v1" :chat-model "smoke-model")))
          (superchat-buffer-name (format " *superchat-smoke-%06x*" (random #x1000000)))
          (baseline-buffers (copy-sequence (buffer-list)))
          (smoke--script (copy-sequence ,script))
          (smoke--requests nil) (smoke--timers nil))
     (setenv "HOME" home)
     (superchat-db-open)
     (unwind-protect
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (if real (symbol-function 'llm-chat-streaming)
                      (lambda (provider prompt partial response error &optional multi)
                        (smoke--fake-network 'streaming provider prompt partial response error multi))))
                   ((symbol-function 'llm-chat-async)
                    (if real (symbol-function 'llm-chat-async)
                      (lambda (provider prompt response error &optional multi)
                        (smoke--fake-network 'async provider prompt nil response error multi)))))
           ,@body)
       (dolist (timer smoke--timers) (when (timerp timer) (ignore-errors (cancel-timer timer))))
       (maphash (lambda (run-id _) (ignore-errors (superchat-runtime-cancel run-id "[smoke cleanup]")))
                superchat-runtime--runs)
       (dolist (buffer (buffer-list))
         (unless (memq buffer baseline-buffers)
           (let ((kill-buffer-query-functions nil)) (ignore-errors (kill-buffer buffer)))))
       (ignore-errors (superchat-db-close))
       (ignore-errors (delete-directory data t))
       (ignore-errors (delete-directory home t)))))

(defun smoke--open-chat ()
  (superchat)
  (or (get-buffer superchat-buffer-name) (current-buffer)))

(defun smoke--send (view text)
  (with-current-buffer view
    (let ((inhibit-read-only t)) (goto-char (point-max)) (insert text))
    (superchat-send-input)
    superchat--active-run-id))

;;;; cases ---------------------------------------------------------------------

(defun smoke-case-A ()
  "chat: one plain turn"
  (smoke--with-fixture (list (list :text "smoke answer A"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (smoke--send view "hello smoke")))
      (smoke--wait (lambda () (smoke--run-done-p run)) 30)
      (list (cons "events" (smoke--types session))
            (cons "status" (plist-get (superchat-runtime-status run) :status))
            (cons "view-has-answer"
                  (and (with-current-buffer view (string-match-p "smoke answer A" (buffer-string))) t))
            ;; The model-visible prompt (Invariant 4): step 3 must keep it.
            (cons "prompt-text" (plist-get (car smoke--requests) :prompt-text))))))

(defun smoke-case-B ()
  "chat: one approved read_buffer call"
  (smoke--with-fixture (list (list :tool-uses '(("read_buffer" (buffer . "*scratch*"))))
                             (list :text "smoke answer B"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (smoke--send view "read scratch")))
      (smoke--wait (lambda () (smoke--run-done-p run)) 30)
      (list (cons "events" (smoke--types session))
            (cons "status" (plist-get (superchat-runtime-status run) :status))
            (cons "provider-calls" (length smoke--requests))))))

(defun smoke-case-C ()
  "chat: send during a Run, then cancel"
  (smoke--with-fixture (list (list :chunks '("SMOKE_PARTIAL") :hang t)
                             (list :text "second answer"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (first (smoke--send view "first")))
      (smoke--wait (lambda () (with-current-buffer view
                                (string-match-p "SMOKE_PARTIAL" (buffer-string)))))
      (smoke--send view "second")
      (let ((pending (superchat-runtime-pending-follow-ups session)))
        (superchat-runtime-cancel first "smoke cancel")
        (smoke--wait (lambda () (= 2 (length smoke--requests))) 10)
        (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 10)
        (list (cons "events" (smoke--types session))
              (cons "pending-after-second-send" pending)
              (cons "provider-calls" (length smoke--requests)))))))

(defun smoke-case-D ()
  "inline ask on a scratch buffer"
  (smoke--with-fixture (list (list :text "inline answer D"))
    (with-temp-buffer
      (insert "alpha beta gamma\n")
      (let* ((before (buffer-hash))
             (overlay (superchat-inline-ask "what is this?" (cons 1 6)))
             (state (lambda () (overlay-get overlay 'superchat-inline))))
        (smoke--wait (lambda () (eq 'ready (plist-get (funcall state) :status))) 30)
        (list (cons "events" (smoke--types (plist-get (funcall state) :session-id)))
              (cons "overlay-status" (plist-get (funcall state) :status))
              (cons "buffer-unchanged" (equal before (buffer-hash))))))))

(defun smoke-case-E ()
  "inline rewrite: accept, then reject"
  (smoke--with-fixture (list (list :text "```\nREWRITTEN\n```") (list :text "```\nIGNORED\n```"))
    (with-temp-buffer
      (insert "alpha beta gamma\n")
      (let* ((before (buffer-hash))
             (transient-mark-mode t)
             (ov (progn (goto-char 1) (push-mark 6 t t)
                        (superchat-rewrite-region 1 6 "uppercase it")))
             (ov (if (listp ov) (car ov) ov)))
        (smoke--wait (lambda () (memq (plist-get (superchat-rewrite--state ov) :status) '(ready error))) 30)
        (unless (eq 'ready (plist-get (superchat-rewrite--state ov) :status))
          (error "rewrite not ready: %S" (superchat-rewrite--state ov)))
        (let ((unchanged-while-ready (equal before (buffer-hash)))
              (session (plist-get (superchat-rewrite--state ov) :session-id)))
          (superchat-rewrite-accept ov)
          (let ((after-accept (buffer-hash))
                (accept-events (smoke--types session)))
            (let* ((ov2 (progn (goto-char 1) (push-mark 6 t t)
                               (superchat-rewrite-region 1 6 "again")))
                   (ov2 (if (listp ov2) (car ov2) ov2)))
              (smoke--wait (lambda () (eq 'ready (plist-get (superchat-rewrite--state ov2) :status))) 30)
              (superchat-rewrite-reject ov2)
              (list (cons "events" (smoke--types session))
                    (cons "unchanged-while-ready" unchanged-while-ready)
                    (cons "accept-changed" (not (equal before after-accept)))
                    (cons "accept-events-have-edit" (and (member "edit.applied" accept-events) t))
                    (cons "reject-unchanged" (equal after-accept (buffer-hash)))
                    (cons "text" (buffer-substring-no-properties 1 (min 20 (point-max))))))))))))

(defun smoke-case-M ()
  "chat: one plain turn through the mock transport plugin"
  (smoke--with-fixture nil
    (superchat-plugin-unmount 'transport-llm)
    (unwind-protect
        (progn
          (superchat-plugin-mount 'transport-mock
                                  '(:script ((:chunks ("mock " "partial") :text "mock answer M"))))
          (let* ((view (smoke--open-chat))
                 (session (buffer-local-value 'superchat--session-id view))
                 (run (smoke--send view "hello mock")))
            (smoke--wait (lambda () (smoke--run-done-p run)) 30)
            (list (cons "events" (smoke--types session))
                  (cons "status" (plist-get (superchat-runtime-status run) :status))
                  (cons "network-calls" (length smoke--requests))
                  (cons "mock-calls" (length superchat-transport-mock-requests))
                  (cons "view-has-answer"
                        (and (with-current-buffer view (string-match-p "mock answer M" (buffer-string))) t))
                  (cons "plugins" (cl-remove-if-not
                                   (lambda (r) (string-prefix-p "transport" (symbol-name (car r))))
                                   (mapcar (lambda (r) (list (car r) (cadr r))) (superchat-plugin-report)))))))
      (superchat-plugin-unmount 'transport-mock)
      (superchat-plugin-mount 'transport-llm))))

(defun smoke--telega-traces ()
  "Every registry entry whose name mentions telega."
  (append
   (mapcar #'car (cl-remove-if-not (lambda (e) (string-match-p "telega" (car e)))
                                   superchat-tools--external-registry))
   (mapcar #'car (cl-remove-if-not (lambda (e) (string-match-p "telega" (car e)))
                                   superchat--command-alist))
   (cl-remove-if-not (lambda (s) (string-match-p "telega" (symbol-name s)))
                     superchat-pipeline--surfaces)
   (let (names) (maphash (lambda (k _) (when (string-match-p "telega" (symbol-name k)) (push k names)))
                         superchat-authority--policy-registry) names)
   (let (names) (maphash (lambda (k _) (when (string-match-p "telega" k) (push k names)))
                         superchat-authority-effect-resolvers) names)
   (mapcar #'car (cl-remove-if-not (lambda (e) (string-match-p "telega" (symbol-name (car e))))
                                   superchat-extension--registry))))

(defun smoke-case-U ()
  "Telega plugin: unmount leaves no trace, remount restores it"
  (require 'superchat-cowork-telega)
  (let ((before (length (smoke--telega-traces))))
    (superchat-plugin-unmount 'cowork-telega)
    (let ((after-unmount (smoke--telega-traces)))
      (superchat-plugin-mount 'cowork-telega)
      (list (cons "traces-while-mounted" before)
            (cons "traces-after-unmount" after-unmount)
            (cons "traces-after-remount" (length (smoke--telega-traces)))
            (cons "state" (superchat-plugin-state 'cowork-telega))))))

(defun smoke-case-F ()
  "Telega digest through the plugin"
  (require 'superchat-cowork-telega)
  (require 'superchat-cowork-telega-legacy)
  (smoke--with-fixture (list (list :text "digest F"))
    (let (result)
      (cl-letf (((symbol-function 'telega-chat-get)
                 (lambda (id &optional _) (list :id id :title (format "Chat %s" id))))
                ((symbol-function 'telega-chat-title) (lambda (chat &optional _ _) (plist-get chat :title)))
                ((symbol-function 'telega-completing-read-chat-list)
                 (lambda (&rest _) '((:id 123 :title "Chat 123"))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'superchat-cowork-telega--show-digest)
                 (lambda (response) (setq result response)))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (let ((handle (superchat-telega-digest "summarize")))
          (smoke--wait (lambda () (or result (null (superchat-runtime-active-runs)))) 30)
          (list (cons "events" (smoke--types "telega-digest"))
                (cons "result" result)
                (cons "surface" (plist-get (superchat-runtime-status (plist-get handle :run-id)) :surface))))))))

;;;; runner ------------------------------------------------------------------

(defun smoke--render (facts)
  (mapconcat (lambda (f) (format "%s: %S" (car f) (cdr f))) facts "\n"))

(defun smoke--run (name)
  (let* ((fn (intern (format "smoke-case-%s" name)))
         (facts (condition-case err (funcall fn)
                  (error (list (cons "ERROR" (error-message-string err))))))
         (text (smoke--render facts))
         (file (expand-file-name (format "%s.txt" name) smoke--expected-dir)))
    (princ (format "== %s: %s\n%s\n" name (documentation fn) text))
    (cond
     ((getenv "SMOKE_BASELINE")
      (make-directory smoke--expected-dir t)
      (with-temp-file file (insert text "\n"))
      (princ (format "-- baseline written %s\n" file)))
     ((file-exists-p file)
      (let ((expected (string-trim (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (if (equal expected text)
            (princ (format "-- %s MATCH\n" name))
          (cl-incf smoke--failures)
          (princ (format "-- %s MISMATCH\n-- expected:\n%s\n" name expected)))))
     (t (princ "-- no baseline\n")))))

(let ((cases (or command-line-args-left '("A" "B" "C" "D" "E" "F" "M" "U"))))
  (setq command-line-args-left nil)
  (dolist (c cases) (smoke--run c))
  (princ (format "SMOKE %s (%d mismatches)\n" (if (zerop smoke--failures) "OK" "FAILED") smoke--failures))
  (kill-emacs (if (zerop smoke--failures) 0 1)))
;;; smoke.el ends here
