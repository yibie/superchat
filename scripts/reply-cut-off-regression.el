;;; reply-cut-off-regression.el --- a reply cut off at the output limit runs no call -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 12b (reports/guidance-2026-10-11/SLICE-12.md).  When the provider
;; says a reply stopped at the output limit (`:stop-reason' `length'), none
;; of that reply's tool calls runs; Superchat tells the model why and asks it
;; to do the work in smaller pieces; after `superchat-llm-unknown-tool-retries'
;; such retries in a row the execution stops and says why.  Every cut-off is
;; the fact `response.cut-off' on the Tape, with the option off too.  A
;; reply of text only that was cut off is still the answer, with a line
;; under it.  Nothing is guessed from the text or the JSON: without a
;; `:stop-reason' nothing changes.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -L scripts -l scripts/reply-cut-off-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^reply-cut-off/")'

;;; Code:

(load (expand-file-name "tool-hooks-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-cut-off-reply-runs-nothing)
(defvar superchat-llm-streaming)
(defvar superchat--llm-unparsed-stop-reason)
(declare-function superchat--llm-cut-off-feedback "superchat-llm")

(defconst rco--two-calls
  '(("read_buffer" (buffer . "hm-target"))
    ("edit_buffer" (buffer_name . "hm-target")
     (old_string . "HM-TARGET") (new_string . "RCO-EDITED"))))

(defconst rco--sentence "Your reply was cut off at the output limit")

(defun rco--target ()
  (with-current-buffer "hm-target" (buffer-string)))

(defun rco--ran-nothing (session)
  (should-not (th--facts session "tool.requested"))
  (should-not (th--facts session "tool.completed"))
  (should (equal "HM-TARGET-TEXT\n" (rco--target))))

(ert-deftest reply-cut-off/the-words-say-the-limit-or-that-there-is-one ()
  (should (equal "Your reply was cut off at the output limit (60 tokens), so none of its tool calls ran. Do the same work in smaller pieces: fewer calls per reply, and split long content over several calls."
                 (superchat--llm-cut-off-feedback 60)))
  (should (string-prefix-p "Your reply was cut off at the output limit (the provider's output limit), so none"
                           (superchat--llm-cut-off-feedback nil))))

(ert-deftest reply-cut-off/1-two-whole-calls-cut-off-run-neither ()
  (th--run (list (list :tool-uses rco--two-calls :stop-reason 'length)
                 '(:text "RCO-DONE"))
      nil
    (rco--ran-nothing session)
    (should (= 2 (length smoke--requests)))
    (should (string-match-p rco--sentence (nth 1 (th--prompts))))
    (let ((fact (car (th--payloads session "response.cut-off"))))
      (should fact)
      (should (equal "length" (plist-get fact :stop-reason)))
      (should (= 2 (plist-get fact :tool-calls)))
      (should (eq t (plist-get fact :refused)))
      (should (= 1 (plist-get fact :attempt))))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))

(ert-deftest reply-cut-off/2-cut-off-in-the-middle-of-arguments-is-said-so ()
  ;; Either condition llm.el may deliver for arguments it cannot parse.
  (dolist (type '(llm-tool-call-error error))
    (th--run (list (list :chunks '((:stop-reason length))
                         :error (cons type "Invalid JSON in tool arguments"))
                   '(:text "RCO-DONE"))
        nil
      (rco--ran-nothing session)
      (let ((second (nth 1 (th--prompts))))
        (should (string-match-p rco--sentence second))
        (should-not (string-match-p "not complete valid JSON" second)))
      (should (th--facts session "response.cut-off")))))

(ert-deftest reply-cut-off/2b-without-streaming-the-stop-reason-is-kept-for-the-error ()
  "Seen in the real probe of 2026-10-21: no stream, so no stop-reason chunk."
  (let ((superchat--llm-unparsed-stop-reason nil))
    (cl-letf (((symbol-function 'llm-provider-extract-stop-reason) (lambda (_p _r) 'length)))
      (should-error (superchat--llm-keep-unparsed-stop-reason
                     (lambda (_p _r) (signal 'llm-tool-call-error '("Invalid JSON")))
                     'provider 'response)
                    :type 'llm-tool-call-error))
    (should (eq 'length superchat--llm-unparsed-stop-reason))
    (setq superchat--llm-unparsed-stop-reason nil)
    (should (equal '(:text "x")
                   (superchat--llm-keep-unparsed-stop-reason (lambda (_p _r) '(:text "x")) 'p 'r)))
    (should-not superchat--llm-unparsed-stop-reason))
  (let ((superchat-llm-streaming nil))
    (th--run (list '(:unparsed-stop-reason length
                     :error (error . "An error occurred when calling an LLM tool: Invalid JSON"))
                   '(:text "RCO-DONE"))
        nil
      (rco--ran-nothing session)
      (should (string-match-p rco--sentence (nth 1 (th--prompts))))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should-not superchat--llm-unparsed-stop-reason))))

(ert-deftest reply-cut-off/3-a-reply-that-was-not-cut-off-runs-its-calls ()
  (dolist (stop '(stop nil))
    (th--run (list (append (list :tool-uses (list (car rco--two-calls)))
                           (when stop (list :stop-reason stop)))
                   '(:text "RCO-DONE"))
        nil
      (should (= 1 (length (th--facts session "tool.completed"))))
      (should-not (th--facts session "response.cut-off"))
      (should-not (string-match-p rco--sentence (nth 1 (th--prompts)))))))

(ert-deftest reply-cut-off/4-three-in-a-row-stop-the-execution-visibly ()
  (pend--fresh
    (hm--fixture (list (list :tool-uses rco--two-calls :stop-reason 'length)
                       (list :tool-uses rco--two-calls :stop-reason 'length)
                       (list :tool-uses rco--two-calls :stop-reason 'length)
                       '(:text "RCO-NEVER"))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (let ((superchat-automated-input 'timers)) (smoke--send view "a task")))
             (reason "Stopped: the reply was cut off at the output limit 3 times in a row"))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
        (rco--ran-nothing session)
        ;; No fourth request.
        (should (= 3 (length smoke--requests)))
        (should (equal '(1 2 3) (mapcar (lambda (p) (plist-get p :attempt))
                                        (th--payloads session "response.cut-off"))))
        ;; The Run's ending facts, under the answer, the pending row.
        (should (equal reason (plist-get (plist-get (superchat-runtime-status run) :tool-budget)
                                         :reason)))
        (should (cl-some (lambda (event)
                           (and (equal run (plist-get event :run-id))
                                (equal reason (plist-get (plist-get (plist-get event :payload)
                                                                    :tool-budget)
                                                         :reason))))
                         (superchat-tape-replay-events session)))
        (should (string-match-p (regexp-quote reason) (with-current-buffer view (buffer-string))))
        (superchat-changes-notices-reload)
        (let ((item (car (pend--of 'unseen))))
          (should (equal run (plist-get item :key)))
          (should (string-match-p (concat "\\`" (regexp-quote reason)) (plist-get item :detail))))))))

(ert-deftest reply-cut-off/a-reply-in-between-starts-the-count-again ()
  (th--run (list (list :tool-uses rco--two-calls :stop-reason 'length)
                 (list :tool-uses rco--two-calls :stop-reason 'length)
                 (list :tool-uses (list (car rco--two-calls)))
                 (list :tool-uses rco--two-calls :stop-reason 'length)
                 (list :tool-uses rco--two-calls :stop-reason 'length)
                 '(:text "RCO-DONE"))
      nil
    (should (= 6 (length smoke--requests)))
    (should (equal '(1 2 1 2) (mapcar (lambda (p) (plist-get p :attempt))
                                      (th--payloads session "response.cut-off"))))
    (should-not (plist-get (plist-get (superchat-runtime-status run) :tool-budget) :reason))))

(ert-deftest reply-cut-off/5-with-the-option-off-the-calls-run-and-the-fact-is-kept ()
  (let ((superchat-cut-off-reply-runs-nothing nil))
    (th--run (list (list :tool-uses rco--two-calls :stop-reason 'length)
                   '(:text "RCO-DONE"))
        nil
      (should (= 2 (length (th--facts session "tool.completed"))))
      (should-not (string-match-p rco--sentence (nth 1 (th--prompts))))
      (let ((fact (car (th--payloads session "response.cut-off"))))
        (should fact)
        (should (= 2 (plist-get fact :tool-calls)))
        (should-not (plist-get fact :refused))))))

(ert-deftest reply-cut-off/6-text-cut-off-is-still-the-answer-with-a-line-under-it ()
  (th--run (list '(:text "RCO-HALF-AN-ANS" :stop-reason length)) nil
    (should (= 1 (length smoke--requests)))
    (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
    (let ((fact (car (th--payloads session "response.cut-off"))))
      (should fact)
      (should (= 0 (plist-get fact :tool-calls)))
      (should (= (length "RCO-HALF-AN-ANS") (plist-get fact :text-chars))))
    (let ((text (with-current-buffer view (buffer-string))))
      (should (string-match-p "RCO-HALF-AN-ANS" text))
      (should (string-match-p "The reply was cut off at the output limit" text)))))

(ert-deftest reply-cut-off/7-without-streaming-the-same ()
  (let ((superchat-llm-streaming nil))
    (th--run (list (list :tool-uses rco--two-calls :stop-reason 'length)
                   '(:text "RCO-DONE"))
        nil
      (should (equal "async" (format "%s" (plist-get (car (last smoke--requests)) :mode))))
      (rco--ran-nothing session)
      (should (string-match-p rco--sentence (nth 1 (th--prompts))))
      (should (= 2 (plist-get (car (th--payloads session "response.cut-off")) :tool-calls))))))

(provide 'reply-cut-off-regression)
;;; reply-cut-off-regression.el ends here
