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
           ;; ACCEPTED-P may be a thunk, consulted only when the bytes differ.
           (when (and recorded-hash (buffer-live-p buffer)
                      (not (equal recorded-hash (buffer-hash buffer)))
                      (not (if (functionp accepted-p) (funcall accepted-p) accepted-p)))
             (list :buffer (buffer-name buffer) :reason "host buffer changed before acceptance"))))

(superchat-invariant-define 'terminal-on-tape
  :seam 'seal
  :doc "Every terminal Run state is on the Tape before any terminal Sink emission."
  :check (lambda (record run-id terminal-type)
           ;; RECORD is what the seal's Tape append returned: a persisted
           ;; identity (with an event id) or nil.
           (unless (and (listp record) (plist-get record :event-id))
             (list :run-id run-id :expected terminal-type :record record))))

(superchat-invariant-define 'tool-execution-passes-authority
  :seam 'tools.execute
  :doc "Every tool execution passes an Authority decision before its body runs."
  :check (lambda (tool-name authority decision)
           (unless (and (listp authority) (plist-member authority :decision)
                        (memq decision '(allow deny ask defer)))
             (list :tool tool-name :authority (and (listp authority) (plist-get authority :decision))
                   :decision decision))))

(superchat-invariant-define 'prompt-reconstructable
  :seam 'driver
  :doc "A provider request is reconstructable from the Tape.
The recorded `request.header' supplies the system text and tool schemas and the
`request.message-list' supplies the message composition as storage references.
The check rebuilds the request from those rows and fails when the receipt, the
header reference, or a named storage id does not resolve; a recorded receipt
alone is not evidence that the request can be rebuilt."
  :check
  (lambda (record)
    (cond
     ((null record) "request evidence could not be appended")
     ((not (listp record)) "request evidence record is not a plist")
     (t
      (let* ((session-id (plist-get record :session-id))
             (request-id (plist-get record :request-id))
             (rebuilt (and (fboundp 'superchat-tape-request-evidence)
                           (superchat-tape-request-evidence session-id request-id)))
             (status (plist-get rebuilt :status)))
        (unless (eq status 'reproduced)
          (list :request-id request-id
                :reason (or (plist-get rebuilt :reason) "no reconstruction")
                :status status)))))))

(superchat-invariant-define 'exact-sink
  :seam 'terminal-delivery
  :doc "A Run delivers to exactly the Sink captured at submit.  Runtime never
replaces an entry's Sink, so today this can only fail after quiesce cleared
it; the check documents the rule at the seam a replacement driver would use."
  :check (lambda (captured-id sink-id)
           (unless (equal captured-id sink-id)
             (list :captured captured-id :emitted-to sink-id))))

(superchat-invariant-define 'plugin-scope-only
  :seam 'mount
  :advisory t
  :doc "A plugin contributes only through its Scope (review convention; not enforceable in Elisp).")

(provide 'superchat-invariant)
;;; superchat-invariant.el ends here
