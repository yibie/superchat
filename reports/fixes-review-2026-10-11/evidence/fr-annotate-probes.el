;;; fr-annotate-probes.el --- annotation edges after 4bea4054 -*- lexical-binding: t; -*-
;; Review of R3-04 to R3-07.  Loaded the way round 3's annotation-probes.el
;; loads its fixtures.  Observations, not acceptance criteria.
(load (expand-file-name "scripts/annotate-chat-regression.el" default-directory) nil t)
(load (expand-file-name "scripts/annotate-regression.el" default-directory) nil t)

(ert-deftest fr-annotate/clearing-the-remarks-does-not-clear-what-the-model-is-told ()
  "R3-05's new table (superchat-dispatcher.el:982-995) is written once and
never cleared: `superchat-annotate-clear' removes the remarks, and the next
message still tells the model they are hung on the buffer."
  (m5rc--fixture `((:text ,annc--answer) (:text "FOLLOWUP"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (should (= 2 (length (annc--placed source))))
      (superchat-annotate-clear source)
      (should-not (annc--placed source))
      (smoke--send view "ANNC_PLAIN_NEXT")
      (let ((request (m5rc--settle 2)))
        (should (annc--earlier-p request))
        (princ (format "FR-ANNOTATE OBSERVED: after superchat-annotate-clear, remarks=0, next request's system still says: %S\n"
                       (let ((s (plist-get request :system)))
                         (and (string-match "Earlier in this conversation[^\n]*" s) (match-string 0 s)))))))))

(ert-deftest fr-annotate/killing-the-source-does-not-clear-what-the-model-is-told ()
  (m5rc--fixture `((:text ,annc--answer) (:text "FOLLOWUP"))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 1)
      (kill-buffer source)
      (smoke--send view "ANNC_PLAIN_NEXT")
      (should (annc--earlier-p (m5rc--settle 2))))))

(ert-deftest fr-annotate/a-whole-buffer-answer-replaces-a-regions-remarks ()
  "R3-07 behavior note: replacement is by lines, whichever request made the
remark.  A `>annotate' of the whole buffer arriving after a region command
removes the command's remark on those lines.  By the documented rule."
  (m5rc--fixture `((:text "2 | beta | CMD-REMARK") (:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (view (m5rc--open source))
           (superchat-automated-input nil))
      (with-current-buffer source
        (let ((transient-mark-mode t))
          (goto-char (point-min)) (forward-line 1)
          (push-mark (line-end-position) t t)
          (superchat-annotate "command on line 2")))
      (m5rc--settle 1)
      (should (equal '((2 "CMD-REMARK")) (annc--placed source)))
      (smoke--send view "ANNC_MARK_THEM >annotate")
      (m5rc--settle 2)
      (princ (format "FR-ANNOTATE NOTE: region command placed CMD-REMARK on line 2; whole-buffer >annotate arrived; remarks now %S\n"
                     (annc--placed source)))
      (should (equal '((2 "REMARK-B") (3 "REMARK-G")) (annc--placed source))))))

(ert-deftest fr-annotate/an-empty-second-answer-removes-the-first-and-places-nothing ()
  "R3-07 behavior note: the second answer replaces the first even when it
brings no remark; the user is left with none and told the answer was empty."
  (wrules--with '((:text "2 | beta | FIRST" :delay 0.15)
                  (:text "")) (p)
    (let ((buffer (ann--file p)) said)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string (push (apply #'format format-string args) said)) nil)))
        (with-current-buffer buffer
          (superchat-annotate "first")
          (superchat-annotate "second"))
        (should (smoke--wait (lambda () (and (= 2 (length hm--wires))
                                             (null (superchat-runtime-active-runs))))
                             5)))
      (princ (format "FR-ANNOTATE NOTE: remarks after an empty second answer: %S; said: %S\n"
                     (ann--placed buffer) (reverse said)))
      (should-not (ann--placed buffer))
      (should (plist-get (buffer-local-value 'superchat-annotate--report buffer) :empty)))))

(ert-deftest fr-annotate/the-inspector-send-after-the-target-was-killed ()
  "R3-06 edge: previewed with the buffer, sent after it is gone."
  (m5rc--fixture `((:text ,annc--answer))
    (let* ((source (m5rc--source annc--text))
           (superchat-automated-input nil)
           (inspector (m5rc--inspect source "ANNC_MARK_THEM >annotate"))
           outcome)
      (should (string-match-p "1| alpha one" (m5rc--body inspector)))
      (kill-buffer source)
      (setq outcome (condition-case err
                        (progn (with-current-buffer inspector (superchat-request-send)) 'sent)
                      (error (list 'error (error-message-string err)))))
      (sleep-for 0.2)
      (princ (format "FR-ANNOTATE NOTE: inspector send after killing the target => %S; provider requests=%d; inspector live=%s\n"
                     outcome (length smoke--requests) (buffer-live-p inspector))))))

(ert-deftest fr-annotate/guard-the-ellipsis-path-is-exact-too ()
  "R3-04: a quote cut short with dots goes through the same exact search."
  (wrules--with '((:text "1 | target... | LOWERCASE")) (p)
    (let ((buffer (ann--file p "TARGET one\n")))
      (with-current-buffer buffer (setq-local case-fold-search t))
      (ann--annotate buffer)
      (should-not (ann--placed buffer)))))

(ert-run-tests-batch-and-exit "^fr-annotate/")
;;; fr-annotate-probes.el ends here
