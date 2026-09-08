;;; test-sink.el --- ERT tests for superchat-sink.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-sink)

(ert-deftest sink/emit-missing-callback-is-settled-discard ()
  "A Sink with no callback for SLOT settles the emission as a discard."
  (let* ((sink (superchat-sink-discard))
         (result (superchat-sink-emit sink 'final '(:run-id "r1") '(:text "x"))))
    (should (equal (plist-get result :sink-id) (superchat-sink-id sink)))
    (should (eq (plist-get result :callback) 'final))
    (should (plist-get result :settled-p))
    (should-not (plist-get result :condition))))

(ert-deftest sink/emit-error-is-recorded-and-settled-nil ()
  "An `error' from a callback is caught, recorded, and leaves settled-p nil."
  (let* ((before (length (superchat-sink-diagnostics)))
         (sink (superchat-sink-create
                :final (lambda (_run _payload) (error "boom"))))
         (result (superchat-sink-emit sink 'final '(:run-id "r1") '(:text "x"))))
    (should-not (plist-get result :settled-p))
    (should (plist-get result :condition))
    (should (eq 'error (car (plist-get result :condition))))
    (should (= (1+ before) (length (superchat-sink-diagnostics))))
    (should (equal (superchat-sink-id sink)
                   (plist-get (car (superchat-sink-diagnostics)) :sink-id)))))

(ert-deftest sink/emit-quit-propagates-unless-captured ()
  "`quit' propagates from a plain emission but is captured when asked."
  (let ((sink (superchat-sink-create
               :final (lambda (_run _payload) (signal 'quit '("stop"))))))
    (let (caught)
      (condition-case err
          (superchat-sink-emit sink 'final '(:run-id "r1") '(:text "x"))
        (quit (setq caught err)))
      (should (equal '(quit "stop") caught)))
    (let ((result (superchat-sink-emit
                   sink 'final '(:run-id "r1") '(:text "x") :capture-quit t)))
      (should-not (plist-get result :settled-p))
      (should (equal '(quit "stop") (plist-get result :condition))))))

(ert-deftest sink/live-p-error-means-dead ()
  "A `live-p' predicate that signals `error' counts as dead and is recorded."
  (let* ((before (length (superchat-sink-diagnostics)))
         (sink (superchat-sink-create
                :live-p (lambda () (error "predicate exploded")))))
    (should-not (superchat-sink-live-p sink))
    (should (= (1+ before) (length (superchat-sink-diagnostics))))
    (should (eq 'live-p (plist-get (car (superchat-sink-diagnostics)) :callback)))))

(ert-deftest sink/normalize-chunk-shapes ()
  "String, `(reasoning X)' and `(:reasoning X)' chunks normalize; else nil."
  (should (equal '(:kind text :text "hi") (superchat-sink-normalize-chunk "hi")))
  (should (equal '(:kind reasoning :text "thinking")
                 (superchat-sink-normalize-chunk '(reasoning "thinking"))))
  (should (equal '(:kind reasoning :text "thinking")
                 (superchat-sink-normalize-chunk '(:reasoning "thinking"))))
  (should-not (superchat-sink-normalize-chunk 42))
  (should-not (superchat-sink-normalize-chunk nil)))

(provide 'test-sink)
;;; test-sink.el ends here
