;;; test-helper.el --- Shared isolation for Superchat tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this before any Superchat module so standalone test files never use
;; the developer's real `user-emacs-directory' for databases or other data.

;;; Code:

(defvar superchat-test-data-directory
  (file-name-as-directory (make-temp-file "superchat-test-data" t))
  "Private data root shared by the test files loaded in this Emacs process.")

(setq superchat-data-directory superchat-test-data-directory)

(provide 'superchat-test-helper)
;;; test-helper.el ends here
