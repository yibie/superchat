;;; superchat.el --- The AI layer of Emacs, on an append-only tape -*- lexical-binding: t; -*-

;; Version: 2.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; The user options every other file reads.  `M-x superchat' is in
;; superchat-chat.el.  SPEC.md is the contract.

;;; Code:

(defconst superchat-version "2.0.0")

(defgroup superchat nil
  "The AI layer of Emacs, on an append-only tape."
  :group 'tools
  :prefix "superchat-")

(defcustom superchat-directory (locate-user-emacs-file "superchat/")
  "Where the Tape, the home rules file and the skills are."
  :type 'directory)

(defcustom superchat-tape-file nil
  "The Tape's database file; nil means tape.sqlite in `superchat-directory'."
  :type '(choice (const nil) file))

(defcustom superchat-llm-provider nil
  "The name of the provider that answers, a key of `superchat-llm-providers'.
For example \"deepseek\".  Nil is an error that says how to set it."
  :type '(choice (const nil) string))

(defcustom superchat-llm-model nil
  "The id of the model to ask, one of that provider's, e.g. \"deepseek-flash\".
A model is required."
  :type '(choice (const nil) string))

(defcustom superchat-llm-reasoning nil
  "How much the model reasons.
Nil means it does not.  Otherwise a pi-ai thinking level; pi-ai clamps it to
what the model has."
  :type '(choice (const nil) (const minimal) (const low) (const medium)
                 (const high) (const xhigh) (const max)))

(defcustom superchat-default-level 'ask
  "The permission level of a conversation that has not set one."
  :type '(choice (const read-only) (const ask) (const auto)))

(defcustom superchat-context-chars 120000
  "How long a conversation's messages may be before a handoff is made."
  :type 'natnum)

(defcustom superchat-max-rounds 40
  "How many model responses one run may take."
  :type 'natnum)

(defcustom superchat-tool-max-chars 20000
  "How long a tool's result may be before it is cut."
  :type 'natnum)

(defcustom superchat-shell-seconds 60
  "How long the shell tool lets a command run."
  :type 'natnum)

(defcustom superchat-plugins nil
  "Names of the plugins `superchat' enables."
  :type '(repeat symbol))

(provide 'superchat)
;;; superchat.el ends here
