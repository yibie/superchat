;;; superchat-change-set.el --- Atomic buffer edit primitive -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Runtime-independent core primitive: apply a set of edits to a buffer as
;; one atomic, undo-amalgamated change, and record an `edit.applied' Tape
;; event when a session is known.  Any surface that edits a buffer on the
;; user's behalf (inline rewrite, chat apply-code-block, future Sinks) goes
;; through this one seam instead of hand-rolling `atomic-change-group'.

;;; Code:

(require 'cl-lib)
(require 'superchat-tape)

(defun superchat-change-set--markerize (edits)
  "Return EDITS as a list of (BEG-MARKER END-MARKER TEXT), ascending by BEG.
Each element of EDITS is (BEG END TEXT); BEG/END may be positions or
markers.  BEG markers use insertion type nil and END markers use
insertion type t, so a later edit's own insertion never shifts an
earlier edit's boundary.  Signals an error, before any buffer change,
when two edits overlap (or merely touch: adjacent edits are rejected
the same as overlapping ones)."
  (let* ((markerized
          (mapcar (lambda (edit)
                    (cl-destructuring-bind (beg end text) edit
                      (list (copy-marker beg nil)
                            (copy-marker end t)
                            text)))
                  edits))
         (sorted (sort markerized
                       (lambda (a b)
                         (< (marker-position (nth 0 a))
                            (marker-position (nth 0 b)))))))
    (cl-loop for (a b) on sorted
             while b
             unless (< (marker-position (nth 1 a)) (marker-position (nth 0 b)))
             do (error "superchat-change-set-apply: overlapping edits at %d and %d"
                       (marker-position (nth 1 a)) (marker-position (nth 0 b))))
    sorted))

;;;###autoload
(cl-defun superchat-change-set-apply (buffer edits &key session-id run-id source)
  "Apply EDITS to BUFFER as one atomic, undo-amalgamated Change Set.

EDITS is a list of (BEG END TEXT): BEG/END are buffer positions or
markers in BUFFER, TEXT is the replacement string for that region.
Overlapping edits signal an error before any change is made.  All
edits are applied inside one `prepare-change-group'/
`undo-amalgamate-change-group' pair, so a single `undo' in BUFFER
restores the pre-call text; an error partway through cancels the
group instead, leaving BUFFER's text unchanged.

When SESSION-ID is non-nil, appends an `edit.applied' Tape event
(payload `:buffer :file :edits N :removed N :added N :source') tagged
with SESSION-ID, RUN-ID and SOURCE.  Returns (:applied N)."
  (with-current-buffer buffer
    (let* ((sorted (superchat-change-set--markerize edits))
           (removed 0)
           (added 0)
           (group (prepare-change-group buffer))
           (ok nil))
      (activate-change-group group)
      (unwind-protect
          (progn
            (dolist (edit (reverse sorted))
              (cl-destructuring-bind (beg end text) edit
                (let ((beg-pos (marker-position beg))
                      (end-pos (marker-position end)))
                  (cl-incf removed (- end-pos beg-pos))
                  (cl-incf added (length text))
                  (goto-char beg-pos)
                  (delete-region beg-pos end-pos)
                  (insert text))))
            (setq ok t))
        (if ok
            (progn
              (undo-amalgamate-change-group group)
              (accept-change-group group))
          (cancel-change-group group)))
      (when session-id
        (superchat-tape-append-event-record
         (list :type "edit.applied"
               :session-id session-id
               :run-id run-id
               :payload (list :buffer (buffer-name buffer)
                              :file buffer-file-name
                              :edits (length edits)
                              :removed removed
                              :added added
                              :source source))))
      (list :applied (length edits)))))

(provide 'superchat-change-set)

;;; superchat-change-set.el ends here
