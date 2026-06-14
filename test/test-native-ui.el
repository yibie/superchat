;;; test-native-ui.el --- ERT tests for transient menu + dired/embark hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-dispatch, superchat-dired-send, and embark hooks.
;; Pure logic checks — no transient/dired/embark servers needed.

;;; Code:

(require 'ert)
(require 'superchat nil t)

;; ═══════════════════════════════════════════════════════════
;; superchat-dispatch (wrapper exists always)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/dispatch-function-defined ()
  "superchat-dispatch is always defined."
  (should (fboundp 'superchat-dispatch)))

(ert-deftest native-ui/dispatch-fallback-function-defined ()
  "superchat--fallback-dispatch is always defined."
  (should (fboundp 'superchat--fallback-dispatch)))

(ert-deftest native-ui/dispatch-transient-stub-defined ()
  "superchat--dispatch-menu-transient stub is always defined."
  (should (fboundp 'superchat--dispatch-menu-transient)))

(ert-deftest native-ui/dispatch-transient-helpers-defined ()
  "All transient helper commands are defined."
  (should (fboundp 'superchat--transient-switch-model))
  (should (fboundp 'superchat--transient-pick-skill))
  (should (fboundp 'superchat--transient-pick-command)))

;; ═══════════════════════════════════════════════════════════
;; superchat-mode-map has dispatch binding
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/keymap-has-dispatch-binding ()
  "superchat-mode-map binds C-c C-d to superchat-dispatch."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-d"))
              'superchat-dispatch)))

(ert-deftest native-ui/keymap-has-existing-bindings-intact ()
  "Existing keybindings are preserved."
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-c"))
              'superchat-send-input))
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-s"))
              'superchat--save-conversation))
  (should (eq (lookup-key superchat-mode-map (kbd "C-c C-h"))
              'superchat--list-commands)))

;; ═══════════════════════════════════════════════════════════
;; superchat-dired-send
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/dired-send-function-defined ()
  "superchat-dired-send is defined."
  (should (fboundp 'superchat-dired-send)))

(ert-deftest native-ui/dired-send-errors-outside-dired ()
  "superchat-dired-send errors when not in dired-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (superchat-dired-send))))

;; ═══════════════════════════════════════════════════════════
;; Embark key naming (no collision with existing commands)
;; ═══════════════════════════════════════════════════════════

(ert-deftest native-ui/embark-key-not-bound-by-default ()
  "Embark keys 's' and 'S' are not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map "s"))
  (should-not (lookup-key superchat-mode-map "S")))

(ert-deftest native-ui/dired-key-not-bound-by-default-in-chat ()
  "dired C-c s is not bound in superchat-mode-map."
  (should-not (lookup-key superchat-mode-map (kbd "C-c s"))))

;;; test-native-ui.el ends here
