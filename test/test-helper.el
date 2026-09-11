;;; test-helper.el --- Shared isolation for Superchat tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this before any Superchat module so standalone test files never use
;; the developer's real `user-emacs-directory' for databases or other data.

;;; Code:

(defvar superchat-test-data-directory
  (file-name-as-directory (make-temp-file "superchat-test-data" t))
  "Private data root shared by the test files loaded in this Emacs process.")

(setq superchat-data-directory superchat-test-data-directory)
(require 'map)

(defun superchat-test--default-view-sink (buffer session-id owner)
  "Test-only stand-in for the removed Runtime View-Sink fallback: the chat
View's Sink when BUFFER is a live `superchat-mode' View of SESSION-ID, else nil."
  (when (and (buffer-live-p buffer)
             (fboundp 'superchat-render-view-sink)
             (with-current-buffer buffer
               (and (bound-and-true-p superchat-mode)
                    (or (null (bound-and-true-p superchat--session-id))
                        (equal superchat--session-id session-id)))))
    (superchat-render-view-sink buffer session-id owner)))

(defvar superchat-runtime-view-sink-function #'superchat-test--default-view-sink
  "Test-only: function of (BUFFER SESSION-ID OWNER) returning the Sink a
legacy-shaped request without `:sink' should use.  Production Runtime no
longer has a default View-Sink fallback; tests that observed it get this.")

(defun superchat-test--legacy-sink (request)
  "Return the Sink a legacy-shaped test REQUEST implies (test-only bridge,
mirroring the pre-1.4 Runtime bridge): `:stream-function' replaces the
stream slot (receiving the raw text, or (:reasoning TEXT)),
`:delivery-function' the final/error slots (called as (TEXT STATUS TAPE-ID),
never once a controlled Run's stop was requested); both run with
`superchat--runtime-transcript-owner' bound to the request's owner.  The
other slots come from `superchat-runtime-view-sink-function'; like the
old bridge, the Sink declares no `:live-p' (a killed buffer never orphans
the Run)."
  (let* ((stream-function (plist-get request :stream-function))
         (delivery (plist-get request :delivery-function))
         (buffer (plist-get request :buffer))
         (owner (plist-get request :transcript-owner))
         (view-sink (and (functionp superchat-runtime-view-sink-function)
                         (funcall superchat-runtime-view-sink-function
                                  buffer (plist-get request :session-id) owner))))
    (require 'superchat-sink)
    (require 'superchat-runtime)
    (if (and view-sink (not stream-function) (not delivery))
        view-sink
      (superchat-sink-create
       :status (lambda (run status)
                 (when view-sink (superchat-sink-emit view-sink 'status run status)))
       :stream (lambda (run chunk)
                 (if stream-function
                     (let ((superchat--runtime-transcript-owner owner))
                       (funcall stream-function
                                (if (eq (plist-get chunk :kind) 'reasoning)
                                    (list :reasoning (plist-get chunk :text))
                                  (plist-get chunk :text))))
                   (when view-sink (superchat-sink-emit view-sink 'stream run chunk))))
       :final (lambda (run payload)
                (if delivery
                    (unless (superchat-runtime--sink-stop-requested-p run)
                      (let ((superchat--runtime-transcript-owner owner))
                        (funcall delivery (plist-get payload :text) 'completed
                                 (plist-get payload :tape-id))))
                  (when view-sink (superchat-sink-emit view-sink 'final run payload))))
       :error (lambda (run failure)
                (if delivery
                    (unless (superchat-runtime--sink-stop-requested-p run)
                      (let ((superchat--runtime-transcript-owner owner))
                        (funcall delivery (plist-get failure :text)
                                 (plist-get failure :status) (plist-get failure :tape-id))))
                  (when view-sink (superchat-sink-emit view-sink 'error run failure))))))))

(defun superchat-test--sink-request (request)
  "Return REQUEST with a `:sink' (test-only): an explicit one is kept;
otherwise the legacy bridge above builds one and the legacy keys are removed."
  (if (plist-member request :sink)
      request
    (let* ((request (copy-sequence request))
           (owner (or (plist-get request :transcript-owner)
                      (format "test-owner-%08x" (random #x100000000))))
           (request (plist-put request :transcript-owner owner))
           (sink (superchat-test--legacy-sink request)))
      (dolist (key '(:stream-function :delivery-function))
        (setq request (map-delete request key)))
      (plist-put request :sink sink))))

(defun superchat-runtime-test-submit (request)
  "`superchat-runtime-submit' for legacy-shaped test requests."
  (require 'superchat-runtime)
  (superchat-runtime-submit (superchat-test--sink-request request)))

(provide 'superchat-test-helper)
;;; test-helper.el ends here
