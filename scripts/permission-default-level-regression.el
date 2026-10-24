;;; permission-default-level-regression.el --- default level and its carry-over -*- lexical-binding: t; -*-

;;; Commentary:
;; Two user decisions of 2026-10-23:
;;
;; 1. `superchat-permission-default-level': a Session that never recorded a
;;    selection can start at a configured level (such as `auto'), instead of
;;    only at the two knob defaults.  nil keeps the old behavior exactly.
;;
;; 2. G30 (GUI field test of 2026-10-23): `/workspace DIR' from a running
;;    conversation opens the declared workspace's own conversation, and the
;;    level the user had selected -- `auto' from answering "allow all" --
;;    did not come along, so the approval prompts returned.  A recorded
;;    selection now carries to the opened conversation, pinned, unless that
;;    conversation recorded one of its own.
;;
;; Run from the repository root:
;;   emacs -Q --batch -L . -L scripts \
;;     -l scripts/permission-default-level-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^pdl/")'

;;; Code:

(require 'ert)
(require 'superchat-db)
(require 'superchat-tape)
(require 'superchat-tape-view)
(require 'superchat-approval)
(require 'superchat-sandbox)

(defvar pdl--counter 0)

(defun pdl--register-configuration ()
  "Register the projection as the session-view plugin does."
  (superchat-projection-register
   'session-configuration
   :version 1
   :initial #'superchat-view--session-config-initial
   :apply #'superchat-view--session-config-apply))

(defmacro pdl--with-tape (&rest body)
  "Run BODY against a fresh, isolated Tape database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "pdl-" t))
          (superchat-data-directory (file-name-as-directory dir))
          (superchat-db--connection nil)
          (superchat-db--path nil)
          (superchat-permission-default-level nil))
     (unwind-protect
         (progn (superchat-db-open)
                (pdl--register-configuration)
                ,@body)
       (superchat-db-close)
       (delete-directory dir t))))

(defun pdl--session ()
  "Return a fresh session id no row mentions."
  (format "pdl-session-%d" (cl-incf pdl--counter)))

(ert-deftest pdl/nil-default-keeps-knob-behavior ()
  "With the default level nil, the knobs rule, as before."
  (pdl--with-tape
   (let ((id (pdl--session)))
     (should (eq 'edit-buffers (superchat-sandbox-effective-mode id)))
     (should (eq 'ask (superchat-approval-effective-policy id)))
     (should (eq 'edit-buffers (superchat-permission-current id))))))

(ert-deftest pdl/default-level-rules-a-fresh-session ()
  "A configured default level sets both knobs of a Session with no record."
  (pdl--with-tape
   (let ((superchat-permission-default-level 'auto)
         (id (pdl--session)))
     (should (eq 'danger-full-access (superchat-sandbox-effective-mode id)))
     (should (eq 'ask (superchat-approval-effective-policy id)))
     (should (eq 'auto (superchat-permission-current id))))))

(ert-deftest pdl/recorded-selection-wins-over-default ()
  "A selection the Session recorded is never overridden by the default."
  (pdl--with-tape
   (let ((superchat-permission-default-level 'auto)
         (id (pdl--session)))
     (superchat-permission-set id 'read-only)
     (should (eq 'read-only (superchat-permission-current id)))
     (should (eq 'read-only (superchat-sandbox-effective-mode id)))
     (should (eq 'ask (superchat-approval-effective-policy id))))))

(ert-deftest pdl/stale-default-degrades-to-knobs ()
  "A default level that matches no level is ignored, not an error."
  (pdl--with-tape
   (let ((superchat-permission-default-level 'no-such-level)
         (id (pdl--session)))
     (should (eq 'edit-buffers (superchat-sandbox-effective-mode id)))
     (should (eq 'ask (superchat-approval-effective-policy id))))))

(ert-deftest pdl/customize-rejects-an-unknown-level ()
  "Setting the option through customize validates the name."
  (should-error (customize-set-variable 'superchat-permission-default-level
                                        'no-such-level))
  ;; A former name is accepted; what is stored is what was given.
  (customize-set-variable 'superchat-permission-default-level 'workspace-write)
  (should (eq 'workspace-write superchat-permission-default-level))
  (customize-set-variable 'superchat-permission-default-level nil))

(ert-deftest pdl/carry-over-carries-a-recorded-level ()
  "G30: the old Session's recorded level reaches the new one."
  (pdl--with-tape
   (let ((from (pdl--session))
         (to (pdl--session)))
     (superchat-permission-set from 'auto)
     (should (eq 'auto (superchat-permission-carry-over from to)))
     (should (eq 'auto (superchat-permission-current to))))))

(ert-deftest pdl/carry-over-pins-against-later-default-changes ()
  "What carry-over wrote does not drift when the configured default changes."
  (pdl--with-tape
   (let ((from (pdl--session))
         (to (pdl--session)))
     (superchat-permission-set from 'auto)
     (let ((superchat-permission-default-level 'auto))
       ;; TO is effectively `auto' already; without the pin nothing would be
       ;; recorded for it and it would follow the default's later changes.
       (superchat-permission-carry-over from to))
     (should (plist-get (superchat-view-session-configuration to)
                        :permission-level))
     (should (eq 'auto (superchat-permission-current to))))))

(ert-deftest pdl/explicit-selection-pins-against-the-default ()
  "Selecting the level the default already gives still records it."
  (pdl--with-tape
   (let ((id (pdl--session)))
     (let ((superchat-permission-default-level 'auto))
       ;; As /permissions and \"allow all\" call it: pinned.
       (superchat-permission-set id 'auto t))
     ;; The default changes; the explicit choice stays.
     (should (eq 'auto (superchat-permission-current id)))
     (should (eq 'danger-full-access (superchat-sandbox-effective-mode id))))))

(ert-deftest pdl/carry-over-respects-the-target-s-own-selection ()
  "A target that recorded any selection of its own is left alone."
  (pdl--with-tape
   (let ((from (pdl--session))
         (to (pdl--session)))
     (superchat-permission-set from 'auto)
     (superchat-permission-set to 'read-only)
     (should-not (superchat-permission-carry-over from to))
     (should (eq 'read-only (superchat-permission-current to))))))

(ert-deftest pdl/carry-over-carries-nothing-from-a-blank-session ()
  "A source that recorded nothing carries nothing."
  (pdl--with-tape
   (let ((from (pdl--session))
         (to (pdl--session)))
     (should-not (superchat-permission-carry-over from to))
     (should (eq 'edit-buffers (superchat-permission-current to))))))

(ert-deftest pdl/carry-over-carries-bare-knobs-without-a-level ()
  "Recorded knobs without a level selection carry one by one."
  (pdl--with-tape
   (let ((from (pdl--session))
         (to (pdl--session)))
     (superchat-sandbox-set-mode from 'read-only)
     (should (equal '(read-only) (superchat-permission-carry-over from to)))
     (should (eq 'read-only (superchat-sandbox-effective-mode to))))))

(provide 'permission-default-level-regression)
;;; permission-default-level-regression.el ends here
