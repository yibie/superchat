;;; workspace-main-session-regression.el --- which conversations a Workspace has -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 1.3 of M11, the query half
;; (docs/decisions/accepted/2026-10-02-m11-slice-1-workspace-resolution.md,
;; the leader's choice 4): a Workspace's main conversation is its most
;; recently active one, read from the Tape -- no pointer is stored.
;; `superchat-view-workspace-session-ids' lists the Sessions bound to a
;; Workspace id, most recently active first.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-main-session-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-main-session/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(when (getenv "SMOKE_REAL")
  (error "Workspace main-session regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defun wms--bind (session workspace-id)
  "Record that SESSION is bound to WORKSPACE-ID."
  (superchat-tape-append-event
   (list :type "workspace.bound" :session-id session :actor-id "user"
         :summary "workspace.bound"
         :payload (list :workspace-id workspace-id
                        :workspace-root (format "/tmp/%s/" workspace-id)))))

(defun wms--unbind (session)
  (superchat-tape-append-event
   (list :type "workspace.unbound" :session-id session :actor-id "user"
         :summary "workspace.unbound" :payload nil)))

(defun wms--activity (session)
  "Append one ordinary row to SESSION."
  (superchat-tape-append-material session "user" "hello"))

(ert-deftest workspace-main-session/lists-the-sessions-bound-to-a-workspace ()
  (smoke--with-fixture nil
    (wms--bind "s-a1" "workspace-aaa")
    (wms--bind "s-b1" "workspace-bbb")
    (wms--activity "s-free")
    (should (equal '("s-a1") (superchat-view-workspace-session-ids "workspace-aaa")))
    (should (equal '("s-b1") (superchat-view-workspace-session-ids "workspace-bbb")))
    (should-not (superchat-view-workspace-session-ids "workspace-none"))))

(ert-deftest workspace-main-session/the-most-recently-active-comes-first ()
  (smoke--with-fixture nil
    (wms--bind "s-1" "workspace-aaa")
    (wms--bind "s-2" "workspace-aaa")
    (should (equal '("s-2" "s-1") (superchat-view-workspace-session-ids "workspace-aaa")))
    (wms--activity "s-1")
    (should (equal '("s-1" "s-2") (superchat-view-workspace-session-ids "workspace-aaa")))
    ;; Activity in another Workspace's Session does not reorder this one's.
    (wms--bind "s-other" "workspace-bbb")
    (wms--activity "s-other")
    (should (equal '("s-1" "s-2") (superchat-view-workspace-session-ids "workspace-aaa")))))

(ert-deftest workspace-main-session/an-unbound-session-is-not-listed ()
  (smoke--with-fixture nil
    (wms--bind "s-1" "workspace-aaa")
    (wms--unbind "s-1")
    (should-not (superchat-view-workspace-session-ids "workspace-aaa"))
    ;; Bound again elsewhere: it belongs to the later Workspace only.
    (wms--bind "s-1" "workspace-bbb")
    (should-not (superchat-view-workspace-session-ids "workspace-aaa"))
    (should (equal '("s-1") (superchat-view-workspace-session-ids "workspace-bbb")))))

(ert-deftest workspace-main-session/an-id-inside-another-id-does-not-match ()
  "The id is compared whole, not as a substring of another id or of a root."
  (smoke--with-fixture nil
    (wms--bind "s-long" "workspace-aaa-extra")
    (superchat-tape-append-event
     (list :type "workspace.bound" :session-id "s-root" :actor-id "user"
           :summary "workspace.bound"
           :payload (list :workspace-id "workspace-zzz"
                          :workspace-root "/tmp/workspace-aaa/")))
    (should-not (superchat-view-workspace-session-ids "workspace-aaa"))))

(ert-deftest workspace-main-session/a-bad-id-lists-nothing ()
  (smoke--with-fixture nil
    (wms--bind "s-1" "workspace-aaa")
    (should-not (superchat-view-workspace-session-ids nil))
    (should-not (superchat-view-workspace-session-ids ""))))

(provide 'workspace-main-session-regression)
;;; workspace-main-session-regression.el ends here
