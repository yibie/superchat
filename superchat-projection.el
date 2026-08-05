;;; superchat-projection.el --- Replayable Tape projections -*- lexical-binding: t; -*-

;;; Commentary:

;; Projections are registered reducers.  Pure Session and Team views use
;; `superchat-projection-replay'; materialized views additionally provide an
;; event selector and reset function for checkpointed catch-up and rebuild.

;;; Code:

(require 'cl-lib)
(require 'superchat-db)

(declare-function superchat-db-call-with-transaction "superchat-db" (function))
(declare-function superchat-db-projection-checkpoint "superchat-db" (name &optional scope))
(declare-function superchat-db-projection-set-checkpoint
                  "superchat-db" (name version last-storage-id &optional scope))
(declare-function superchat-db-projection-reset-checkpoint
                  "superchat-db" (name &optional scope))

(defvar superchat-projection--registry nil
  "Alist of registered projection descriptors.")

(cl-defun superchat-projection-register
    (name &key version initial apply select reset)
  "Register projection NAME.
VERSION is a positive integer.  INITIAL supplies replay state and APPLY is
called as (STATE EVENT).  Materialized projections also provide SELECT,
called as (SINCE-STORAGE-ID SCOPE), and RESET, called with SCOPE."
  (unless (symbolp name)
    (error "Projection name must be a symbol"))
  (unless (and (integerp version) (> version 0))
    (error "Projection version must be a positive integer"))
  (unless (functionp apply)
    (error "Projection apply must be callable"))
  (when (and select (not (functionp select)))
    (error "Projection select must be callable"))
  (when (and reset (not (functionp reset)))
    (error "Projection reset must be callable"))
  (setf (alist-get name superchat-projection--registry)
        (list :version version
              :initial initial
              :apply apply
              :select select
              :reset reset))
  name)

(defun superchat-projection--descriptor (name)
  "Return registered projection NAME or signal an error."
  (or (alist-get name superchat-projection--registry)
      (error "Unknown Superchat projection: %s" name)))

(defun superchat-projection-replay (name events)
  "Replay EVENTS through projection NAME and return its logical state."
  (let* ((descriptor (superchat-projection--descriptor name))
         (initial (plist-get descriptor :initial))
         (state (if (functionp initial)
                    (funcall initial)
                  (copy-tree initial))))
    (dolist (event events state)
      (setq state
            (funcall (plist-get descriptor :apply) state event)))))

(defun superchat-projection-catch-up (name &optional scope)
  "Apply unseen events to materialized projection NAME in SCOPE.
Each event application and checkpoint update share one transaction.
Return the last applied event id."
  (let* ((descriptor (superchat-projection--descriptor name))
         (version (plist-get descriptor :version))
         (select (or (plist-get descriptor :select)
                     (error "Projection %s is not materialized" name)))
         (checkpoint (superchat-db-projection-checkpoint name scope)))
    (if (and checkpoint (/= (car checkpoint) version))
        (superchat-projection-rebuild name scope)
      (let ((last-id (or (cadr checkpoint) 0)))
        ;; ponytail: One transaction per event favors crash-safe checkpoints
        ;; over replay throughput; batch only after measured rebuild latency.
        (dolist (event (funcall select last-id scope) last-id)
          (let ((storage-id (car event)))
            (unless (and (integerp storage-id) (> storage-id last-id))
              (error "Projection %s received an invalid storage cursor: %S"
                     name storage-id))
            (superchat-db-call-with-transaction
             (lambda ()
               (funcall (plist-get descriptor :apply) nil event)
               (superchat-db-projection-set-checkpoint
                name version storage-id scope)))
            (setq last-id storage-id)))))))

(defun superchat-projection-rebuild (name &optional scope)
  "Reset materialized projection NAME in SCOPE and replay its Tape events."
  (let* ((descriptor (superchat-projection--descriptor name))
         (reset (or (plist-get descriptor :reset)
                    (error "Projection %s cannot be rebuilt" name))))
    (superchat-db-call-with-transaction
     (lambda ()
       (funcall reset scope)
       (superchat-db-projection-reset-checkpoint name scope)))
    (superchat-projection-catch-up name scope)))

(provide 'superchat-projection)

;;; superchat-projection.el ends here
