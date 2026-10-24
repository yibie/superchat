;;; fr-pending-load.el --- is M-x superchat-pending there after loading? -*- lexical-binding: t; -*-
;; Review of 437ca4e8.  Nothing in the package requires or autoloads
;; superchat-pending.el (superchat.el:65-98 autoloads the other never-required
;; files; superchat-annotate is required by the inline-rewrite row at mount,
;; superchat-default-plugins.el:73).  This file loads the package the way a
;; `-L <repo>' development setup does and reports what is defined.
(defun fr-load--report (stage)
  (princ (format "%s: superchat-pending fboundp=%s commandp=%s | superchat-annotate fboundp=%s | superchat-work-list=%s | superchat-changes-notices=%s | superchat-tape-browser=%s | superchat-research-workflow=%s\n"
                 stage
                 (fboundp 'superchat-pending) (commandp 'superchat-pending)
                 (fboundp 'superchat-annotate)
                 (fboundp 'superchat-work-list) (fboundp 'superchat-changes-notices)
                 (fboundp 'superchat-tape-browser) (fboundp 'superchat-research-workflow))))

(fr-load--report "before (require 'superchat)")
(require 'superchat)
(fr-load--report "after (require 'superchat)")
(condition-case err
    (progn (superchat-plugin-mount-defaults)
           (fr-load--report "after (superchat-plugin-mount-defaults)"))
  (error (princ (format "mount-defaults signalled: %s\n" (error-message-string err)))))
(princ (format "files naming superchat-pending outside its own file and its test: %S\n"
               (let (hits)
                 (dolist (file (append (directory-files default-directory t "\\`superchat.*\\.el\\'")
                                       (directory-files-recursively (expand-file-name "plugins" default-directory) "\\.el\\'")))
                   (unless (string-suffix-p "superchat-pending.el" file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (goto-char (point-min))
                       (when (search-forward "superchat-pending" nil t)
                         (push (file-relative-name file default-directory) hits)))))
                 hits)))
(princ (format "a package install generates autoloads from cookies: superchat-pending.el has %d ;;;###autoload cookie(s)\n"
               (with-temp-buffer
                 (insert-file-contents (expand-file-name "superchat-pending.el" default-directory))
                 (how-many "^;;;###autoload" (point-min) (point-max)))))
(kill-emacs 0)
;;; fr-pending-load.el ends here
