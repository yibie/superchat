;;; superchat-optional.el --- Optional Superchat features as one package -*- lexical-binding: t; -*-

;;; Commentary:

;; The optional package (approved decision 5, S26): Generated UI
;; (`superchat-genui'), the emacsclient JSON surface
;; (`superchat-emacsclient'), and the Experience / Claims evidence chain
;; (`superchat-experience', `superchat-claims').  `(require 'superchat)'
;; loads none of them; add this directory to `load-path' and
;; `(require 'superchat-optional)' to get their entry points, which then
;; load their own files on first use.  Their Tape event types stay declared
;; in the kernel so old data replays without this package.

;;; Code:

(autoload 'superchat-emacsclient-list-runs "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-run-status "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-session-history "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-run-cancel "superchat-emacsclient" nil t)
(autoload 'superchat-claim-capture "superchat-claims" nil t)
(autoload 'superchat-claim-correct "superchat-claims" nil t)
(autoload 'superchat-experience-record-evaluation "superchat-experience")
(autoload 'superchat-experience-record-relation "superchat-experience")
(autoload 'superchat-experience-applicability-graph "superchat-experience")
(autoload 'superchat-experience-assemble-recovery-view "superchat-experience")
(autoload 'superchat-experience-assemble-view "superchat-experience")
(autoload 'superchat-experience--cmd-review "superchat-experience")
(autoload 'superchat-genui-render-form "superchat-genui")
(autoload 'superchat-genui-action-names "superchat-genui")

(with-eval-after-load 'superchat
  ;; `/review' is the Experience package's slash command.
  (when (fboundp 'superchat-register-command)
    (superchat-register-command "review" #'superchat-experience--cmd-review)))

(provide 'superchat-optional)
;;; superchat-optional.el ends here
