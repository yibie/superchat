;;; superchat-invariant.el --- Kernel: Invariants and their seam checks -*- lexical-binding: t; -*-

;;; Commentary:
;; An Invariant is a rule that holds regardless of which plugins are mounted
;; (docs/design/plugin-architecture.md §1.7).  Invariants are Kernel data:
;; a plugin can neither add nor remove one.  Each is checked at a seam the
;; Kernel-owned code passes through (the driver, the seal, the tool
;; execution seam); a violation is recorded on the Tape as
;; `invariant.violated' and a check that itself errors as
;; `invariant.check-failed' -- never hidden, never aborting the Run.

;;; Code:

(require 'cl-lib)

(declare-function superchat-tape-append-event "superchat-tape" (event))

(defvar superchat-invariant--definitions nil
  "alist NAME -> (:seam SYMBOL :check FN-or-nil :doc STRING :advisory BOOL).")

(cl-defun superchat-invariant-define (name &key seam check doc advisory)
  "Define Invariant NAME checked at SEAM by CHECK (nil = advisory only).
CHECK receives the seam's arguments and returns nil when the rule holds,
else a string or plist describing the violation."
  (setf (alist-get name superchat-invariant--definitions)
        (list :seam seam :check check :doc doc :advisory advisory))
  name)

(defun superchat-invariant-report ()
  "List of (NAME SEAM ADVISORY-P DOC) for every Invariant."
  (mapcar (lambda (entry)
            (list (car entry) (plist-get (cdr entry) :seam)
                  (and (plist-get (cdr entry) :advisory) t)
                  (plist-get (cdr entry) :doc)))
          superchat-invariant--definitions))

(defun superchat-invariant--record (type name session-id run-id detail)
  "Append TYPE for Invariant NAME to SESSION-ID's Tape, best effort."
  (when (and (stringp session-id) (not (string-empty-p session-id))
             (fboundp 'superchat-tape-append-event))
    (ignore-errors
      (superchat-tape-append-event
       (append (list :type type :session-id session-id :actor-id "kernel"
                     :summary (format "%s: %s" type name)
                     :payload (list :invariant (symbol-name name)
                                    :detail (format "%S" detail)))
               (when run-id (list :run-id run-id)))))))

(defun superchat-invariant-check (name session-id run-id &rest args)
  "Run Invariant NAME's check with ARGS for SESSION-ID / RUN-ID.
Returns nil when the rule holds or NAME is advisory; otherwise the
violation detail, after recording `invariant.violated' on the Tape.  A
check that signals records `invariant.check-failed' and returns nil."
  (let* ((definition (or (alist-get name superchat-invariant--definitions)
                         (error "superchat-invariant: unknown invariant %S" name)))
         (check (plist-get definition :check)))
    (when check
      (condition-case err
          (let ((violation (apply check args)))
            (when violation
              (superchat-invariant--record "invariant.violated" name session-id run-id violation)
              violation))
        ((error quit)
         (superchat-invariant--record "invariant.check-failed" name session-id run-id err)
         nil)))))

;;;; The six Invariants (spec §1.7) ---------------------------------------------
;; Checks are written against the seam arguments the Kernel-owned code passes
;; (see the `superchat-invariant-check' call sites in superchat-pipeline.el,
;; superchat-runtime.el and superchat-authority.el).

(superchat-invariant-define 'host-buffer-unchanged
  :seam 'terminal-delivery
  :doc "A source buffer is byte-for-byte unchanged until its Change Set is accepted (inline surfaces)."
  :check (lambda (buffer recorded-hash accepted-p)
           (when (and recorded-hash (buffer-live-p buffer) (not accepted-p)
                      (not (equal recorded-hash (buffer-hash buffer))))
             (list :buffer (buffer-name buffer) :reason "host buffer changed before acceptance"))))

(superchat-invariant-define 'terminal-on-tape
  :seam 'seal
  :doc "Every terminal Run state is on the Tape before any terminal Sink emission."
  :check (lambda (events run-id terminal-type)
           (unless (cl-some (lambda (event)
                              (and (equal run-id (plist-get event :run-id))
                                   (equal terminal-type (plist-get event :type))))
                            events)
             (list :run-id run-id :expected terminal-type))))

(superchat-invariant-define 'tool-execution-passes-authority
  :seam 'tools.execute
  :doc "Every tool execution passes an Authority decision before its body runs."
  :check (lambda (tool-name decision)
           (unless (and (listp decision) (plist-member decision :decision))
             (list :tool tool-name :reason "no Authority decision before execution"))))

(superchat-invariant-define 'prompt-reconstructable
  :seam 'driver
  :doc "Model-visible text is reconstructable from the Tape: the assembly receipt is recorded as prompt.assembled."
  :check (lambda (record)
           (unless record
             "prompt.assembled could not be appended")))

(superchat-invariant-define 'exact-sink
  :seam 'terminal-delivery
  :doc "A Run delivers to exactly the Sink captured at submit."
  :check (lambda (captured-id sink-id)
           (unless (equal captured-id sink-id)
             (list :captured captured-id :emitted-to sink-id))))

(superchat-invariant-define 'plugin-scope-only
  :seam 'mount
  :advisory t
  :doc "A plugin contributes only through its Scope (review convention; not enforceable in Elisp).")

(provide 'superchat-invariant)
;;; superchat-invariant.el ends here
