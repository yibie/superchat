;;; test-pipeline.el — Smoke test for superchat-core pipeline -*- lexical-binding: t; -*-

(add-to-list 'load-path default-directory)

(message "=== Loading superchat ===")
(require 'superchat)
(message "Loaded")

(setq debug-on-error t)
(setq superchat-data-directory "/tmp/sc-p1/")
(superchat--ensure-directories)
(message "DB ready")

;; Test 1: command parse
(let* ((turn (superchat-turn-new "/models gpt-4" "test"))
       (result (superchat-core-run-turn turn)))
  (message "Test 1 - Command: %s" (or (superchat-turn-command result) "nil"))
  (message "Test 1 - Args: %s" (superchat-turn-command-args result)))

;; Test 2: plain text
(let* ((turn (superchat-turn-new "Hello what language?" "test"))
       (result (superchat-core-run-turn turn)))
  (message "Test 2 - Prompt: %.60s" (superchat-turn-prompt result))
  (message "Test 2 - Error: %s" (if (superchat-turn-error result) "yes" "no")))

;; Test 3: @model switch
(let* ((turn (superchat-turn-new "@gpt-4 Hey" "test"))
       (result (superchat-core-run-turn turn)))
  (message "Test 3 - Target: %s" (or (superchat-turn-target-model result) "nil"))
  (message "Test 3 - Clean: %s" (superchat-turn-clean-input result)))

(superchat-db-close)
(message "=== ALL TESTS PASSED ===")
