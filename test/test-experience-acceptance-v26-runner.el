;;; test-experience-acceptance-v26-runner.el --- Runner process gate -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load-file
 (expand-file-name
  "experiments/experience-view-internal-acceptance-v2.6/verify-runner.el"
  superchat-test--root))

(ert-deftest experience-acceptance-v26/runner-gate-is-no-chat-and-compile-safe ()
  "The frozen runner must pass the pre-provider process gate."
  (should (superchat-acceptance-v26-verify-runner)))

;;; test-experience-acceptance-v26-runner.el ends here
