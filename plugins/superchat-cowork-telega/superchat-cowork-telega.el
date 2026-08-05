;;; superchat-cowork-telega.el --- Contextual Telega cowork bundle -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (superchat "0.9"))

;;; Commentary:

;; Bundle entrypoint for the contextual Telega coworker.
;;
;; Loading this feature registers the proven Telega tools and compatibility
;; commands, then loads the Cowork Session interface.  Workflow guidance lives
;; in skills/cowork-telegram/SKILL.md so the bundle is not a single Lisp file.

;;; Code:

(require 'subr-x)
(require 'superchat)

(defconst superchat-cowork-telega--bundle-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the cowork Telega bundle.")

(defvar superchat-cowork-telega--skill-body-cache nil)

(defun superchat-cowork-telega-skill-body ()
  "Return the bundled Cowork Session guidance."
  (or superchat-cowork-telega--skill-body-cache
      (setq superchat-cowork-telega--skill-body-cache
            (with-temp-buffer
              (insert-file-contents
               (expand-file-name
                "skills/cowork-telegram/SKILL.md"
                superchat-cowork-telega--bundle-directory))
              (string-trim (buffer-string))))))

(let ((load-path (cons superchat-cowork-telega--bundle-directory load-path)))
  (require 'superchat-cowork-telega-legacy)
  (require 'superchat-cowork-telega-session))

(provide 'superchat-cowork-telega)

;;; superchat-cowork-telega.el ends here
