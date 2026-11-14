;;; llm-pi-ai-event-stream-test.el --- Tests for llm-pi-ai-event-stream  -*- lexical-binding: t; -*-

;; Tests for Tier 0: the port of src/utils/event-stream.ts
;; (@earendil-works/pi-ai@1.4.0, commit 58c0b02c).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'llm-pi-ai-types)
(require 'llm-pi-ai-event-stream)

;;; Helpers

(defun llm-pi-ai-event-stream-test--text-delta (delta &optional partial)
  "Return a \"text_delta\" event carrying DELTA."
  (llm-pi-ai-make-event :type "text_delta" :content-index 0 :delta delta
                        :partial partial))

(defun llm-pi-ai-event-stream-test--done (&rest args)
  "Return a \"done\" event for an assistant message built from ARGS."
  (llm-pi-ai-make-event :type "done" :reason "stop"
                        :message (apply #'llm-pi-ai-make-assistant-message args)))

(defun llm-pi-ai-event-stream-test--error-message (&rest args)
  "Return an \"error\" event for an assistant message built from ARGS."
  (llm-pi-ai-make-event :type "error" :reason "error"
                        :error (apply #'llm-pi-ai-make-assistant-message
                                      :stop-reason "error" args)))

;;; The generic queue

(ert-deftest llm-pi-ai-event-stream-test-fifo-order ()
  "The queue hands values out in the order they were enqueued."
  (let ((queue (llm-pi-ai-fifo-queue-create)))
    (should (= (llm-pi-ai-fifo-queue-length queue) 0))
    (should-not (llm-pi-ai-fifo-queue-dequeue queue))
    (dotimes (i 5) (llm-pi-ai-fifo-queue-enqueue queue i))
    (should (= (llm-pi-ai-fifo-queue-length queue) 5))
    (should (equal (cl-loop repeat 5 collect (llm-pi-ai-fifo-queue-dequeue queue))
                   '(0 1 2 3 4)))
    (should (= (llm-pi-ai-fifo-queue-length queue) 0))))

(ert-deftest llm-pi-ai-event-stream-test-fifo-interleaved ()
  "Enqueuing between dequeues still yields FIFO order."
  (let ((queue (llm-pi-ai-fifo-queue-create)))
    (llm-pi-ai-fifo-queue-enqueue queue 'a)
    (llm-pi-ai-fifo-queue-enqueue queue 'b)
    (should (eq (llm-pi-ai-fifo-queue-dequeue queue) 'a))
    (llm-pi-ai-fifo-queue-enqueue queue 'c)
    (should (eq (llm-pi-ai-fifo-queue-dequeue queue) 'b))
    (should (eq (llm-pi-ai-fifo-queue-dequeue queue) 'c))
    (should (= (llm-pi-ai-fifo-queue-length queue) 0))))

;;; Delivering events

(ert-deftest llm-pi-ai-event-stream-test-queued-events ()
  "Events pushed before iteration are delivered in order."
  (let ((feed (cons nil nil))
        (stream (llm-pi-ai-create-assistant-message-event-stream)))
    (setcar feed (lambda (event) (llm-pi-ai-event-stream-push stream event)))
    (setcdr feed (lambda (&optional result) (llm-pi-ai-event-stream-end stream result)))
    (funcall (car feed) (llm-pi-ai-event-stream-test--text-delta "a"))
    (funcall (car feed) (llm-pi-ai-event-stream-test--text-delta "b"))
    (funcall (cdr feed))
    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-delta
                           (llm-pi-ai-event-stream-to-list stream))
                   '("a" "b")))
    (should (llm-pi-ai-event-stream-done-p stream))))

(ert-deftest llm-pi-ai-event-stream-test-waiting-consumer ()
  "A consumer that waits is served the next event, then the end."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (received 'none)
         (ended 'none))
    ;; The first step waits, because nothing has been pushed yet.
    (should-not (llm-pi-ai-event-stream-next
                 stream
                 (lambda (value &optional done)
                   (setq received (list value done)))))
    (should (eq received 'none))
    (should (= (llm-pi-ai-event-stream-waiting-count stream) 1))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "x"))
    (should (equal (llm-pi-ai-assistant-message-event-delta (car received)) "x"))
    (should-not (cadr received))
    (should (= (llm-pi-ai-event-stream-waiting-count stream) 0))
    ;; Nothing is queued and the stream is not done: wait again.
    (should-not (llm-pi-ai-event-stream-next
                 stream
                 (lambda (_value &optional done) (setq ended done))))
    (should (eq ended 'none))
    (llm-pi-ai-event-stream-end stream)
    (should ended)))

(ert-deftest llm-pi-ai-event-stream-test-waiters-served-in-order ()
  "Each waiting consumer takes the next event, in the order it waited."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (first nil)
         (second nil))
    (llm-pi-ai-event-stream-next
     stream (lambda (value &optional _done) (setq first value)))
    (llm-pi-ai-event-stream-next
     stream (lambda (value &optional _done) (setq second value)))
    (should (= (llm-pi-ai-event-stream-waiting-count stream) 2))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "1"))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "2"))
    (should (equal (llm-pi-ai-assistant-message-event-delta first) "1"))
    (should (equal (llm-pi-ai-assistant-message-event-delta second) "2"))))

(defun llm-pi-ai-event-stream-test--describe (event)
  "Return what an iteration callback records for EVENT."
  (let ((type (llm-pi-ai-assistant-message-event-type event)))
    (if (equal type "text_delta")
        (llm-pi-ai-assistant-message-event-delta event)
      (intern type))))

(ert-deftest llm-pi-ai-event-stream-test-iteration-finished-stream ()
  "`llm-pi-ai-event-stream-iterate' drains a stream already filled and ended."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (seen nil)
         (ended 0))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "a"))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "b"))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--done :api "a"))
    (should (llm-pi-ai-event-stream-done-p stream))
    (should (= (llm-pi-ai-event-stream-iterate
                stream
                (lambda (event) (push (llm-pi-ai-event-stream-test--describe event) seen))
                (lambda () (setq ended (1+ ended))))
               3))
    ;; Delivered in order, the end reported once, and nothing left over.
    (should (equal (nreverse seen) '("a" "b" done)))
    (should (= ended 1))
    (should-not (llm-pi-ai-event-stream-to-list stream))
    (should-not (llm-pi-ai-event-stream-waiting-p stream))))

(ert-deftest llm-pi-ai-event-stream-test-iteration-live-stream ()
  "An iterator on a live stream runs as the producer pushes, not before."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (seen nil)
         (ended 0)
         (returned 0))
    ;; Nothing is queued: the iterator waits instead of spinning.
    (setq returned
          (llm-pi-ai-event-stream-iterate
           stream
           (lambda (event) (push (llm-pi-ai-event-stream-test--describe event) seen))
           (lambda () (setq ended (1+ ended)))))
    (should (= returned 0))
    (should-not seen)
    (should (= ended 0))
    (should (llm-pi-ai-event-stream-waiting-p stream))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "a"))
    (should (equal seen '("a")))
    (should (= ended 0))
    ;; The callback runs from within `push' and comes back to `next'.
    (should (llm-pi-ai-event-stream-waiting-p stream))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--done :api "a"))
    (should (equal seen '(done "a")))
    (should (= ended 1))
    (should-not (llm-pi-ai-event-stream-waiting-p stream))
    ;; The stream had not ended when the iterator returned, so it does not
    ;; give a final count here; events pushed after the end are dropped.
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "late"))
    (should (equal seen '(done "a")))
    (should (= ended 1))))

(ert-deftest llm-pi-ai-event-stream-test-iteration-reentrant-push ()
  "An event pushed from the callback is delivered, not nested in it."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (seen nil)
         (depth 0)
         (ended 0))
    ;; Queued, not yet ended: the iterator takes "a" at once, then waits.
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "a"))
    (llm-pi-ai-event-stream-iterate
     stream
     (lambda (event)
       (let ((delta (llm-pi-ai-assistant-message-event-delta event)))
         (push delta seen)
         (when (equal delta "a")
           (setq depth (1+ depth))
           ;; This push finds the loop back in `next', not in the
           ;; callback, so the new event waits for the loop.
           (llm-pi-ai-event-stream-push
            stream (llm-pi-ai-event-stream-test--text-delta "b"))))
       (should (<= depth 1)))
     (lambda () (setq ended (1+ ended))))
    ;; "a" was taken; the event pushed from its callback was queued while
    ;; the loop was back in `next', so it is taken at once, in order.
    (should (equal seen '("b" "a")))
    (should (= depth 1))
    (should (llm-pi-ai-event-stream-waiting-p stream))
    (llm-pi-ai-event-stream-end stream)
    (should (equal seen '("b" "a")))
    (should (= ended 1))))

(ert-deftest llm-pi-ai-event-stream-test-iteration-after-end ()
  "An iterator on an ended stream reports the end and stops there."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (seen nil)
         (ended 0))
    ;; Nothing is queued and the stream is done: one step, the end.
    (llm-pi-ai-event-stream-end stream)
    (should (= (llm-pi-ai-event-stream-iterate
                stream
                (lambda (event) (push (llm-pi-ai-event-stream-test--describe event) seen))
                (lambda () (setq ended (1+ ended))))
               0))
    (should-not seen)
    (should (= ended 1))
    (should-not (llm-pi-ai-event-stream-waiting-p stream))
    ;; `end' on a stream that already ended reports nothing more.
    (llm-pi-ai-event-stream-end stream)
    (should (= ended 1))))

(ert-deftest llm-pi-ai-event-stream-test-iteration-callback-error ()
  "An error in the callback unwinds the iterator and drops its waiter."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (ended 0))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "a"))
    (should-error
     (llm-pi-ai-event-stream-iterate
      stream (lambda (_event) (error "boom")) (lambda () (setq ended (1+ ended))))
     :type 'error)
    (should (= ended 0))
    ;; Unwound: no waiter left behind, so a later push just queues.
    (should-not (llm-pi-ai-event-stream-waiting-p stream))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--text-delta "b"))
    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-delta
                           (llm-pi-ai-event-stream-to-list stream))
                   '("b")))))

(ert-deftest llm-pi-ai-event-stream-test-query-state ()
  "A stream reports whether it is waiting, active or done."
  (let ((stream (llm-pi-ai-create-assistant-message-event-stream)))
    (should (eq (llm-pi-ai-event-stream-state stream) 'active))
    (should-not (llm-pi-ai-event-stream-waiting-p stream))
    (llm-pi-ai-event-stream-end stream)
    (should (eq (llm-pi-ai-event-stream-state stream) 'done))
    (should (llm-pi-ai-event-stream-done-p stream))
    (should-not (llm-pi-ai-event-stream-waiting-p stream))))

;;; Results

(ert-deftest llm-pi-ai-event-stream-test-done-result ()
  "A \"done\" event finishes the stream and sets its result."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (message (llm-pi-ai-make-assistant-message :api "a")))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "done" :reason "stop" :message message))
    (should (llm-pi-ai-event-stream-done-p stream))
    (should (eq (llm-pi-ai-event-stream-result stream) message))))

(ert-deftest llm-pi-ai-event-stream-test-error-result ()
  "An \"error\" event sets the stream's result to the error message."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (message (llm-pi-ai-make-assistant-message :stop-reason "error")))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "error" :reason "error" :error message))
    (should (llm-pi-ai-event-stream-done-p stream))
    (should (eq (llm-pi-ai-event-stream-result stream) message))))

(ert-deftest llm-pi-ai-event-stream-test-end-with-result ()
  "`end' makes the stream done; its result is the one passed, or nil."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (message (llm-pi-ai-make-assistant-message :api "a")))
    (llm-pi-ai-event-stream-end stream message)
    (should (llm-pi-ai-event-stream-done-p stream))
    (should (eq (llm-pi-ai-event-stream-result stream) message)))
  (let ((stream (llm-pi-ai-create-assistant-message-event-stream)))
    (llm-pi-ai-event-stream-end stream)
    (should (llm-pi-ai-event-stream-done-p stream))
    (should-not (llm-pi-ai-event-stream-result stream))))

(ert-deftest llm-pi-ai-event-stream-test-push-after-done-is-dropped ()
  "Events pushed after the stream is done are discarded."
  (let ((stream (llm-pi-ai-create-assistant-message-event-stream)))
    (llm-pi-ai-event-stream-push stream (llm-pi-ai-event-stream-test--done :api "a"))
    ;; The completing event itself is still delivered, as in the source.
    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-type
                           (llm-pi-ai-event-stream-to-list stream))
                   '("done")))
    (should-not (llm-pi-ai-event-stream-push
                 stream (llm-pi-ai-event-stream-test--text-delta "late")))
    (should-not (llm-pi-ai-event-stream-to-list stream))
    (should (llm-pi-ai-assistant-message-p (llm-pi-ai-event-stream-result stream)))))

(ert-deftest llm-pi-ai-event-stream-test-resolve-once ()
  "Only the first result counts; a later `end' result is ignored."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (first (llm-pi-ai-make-assistant-message :api "first"))
         (second (llm-pi-ai-make-assistant-message :api "second")))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-make-event :type "done" :reason "stop" :message first))
    (llm-pi-ai-event-stream-end stream second)
    (should (eq (llm-pi-ai-event-stream-result stream) first))
    (should (equal (llm-pi-ai-assistant-message-api
                    (llm-pi-ai-event-stream-result stream))
                   "first"))))

;;; Timing

(ert-deftest llm-pi-ai-event-stream-test-times-final-message ()
  "The final message gets a duration measured from the stream's creation."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (started (llm-pi-ai-event-stream-started-at stream)))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-event-stream-test--done :api "a" :timestamp started))
    (let ((duration (llm-pi-ai-assistant-message-duration-ms
                     (llm-pi-ai-event-stream-result stream))))
      (should (integer-or-marker-p duration))
      (should (<= 0 duration)))))

(ert-deftest llm-pi-ai-event-stream-test-keeps-existing-duration ()
  "A message that already has `duration-ms' keeps it."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (started (llm-pi-ai-event-stream-started-at stream))
         (message (llm-pi-ai-make-assistant-message
                   :api "a" :timestamp started :duration-ms 7)))
    (llm-pi-ai-event-stream-end stream message)
    (should (equal (llm-pi-ai-assistant-message-duration-ms
                    (llm-pi-ai-event-stream-result stream))
                   7))))

(ert-deftest llm-pi-ai-event-stream-test-leaves-older-message-untimed ()
  "A message timestamped before the stream started stays untimed."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (started (llm-pi-ai-event-stream-started-at stream)))
    (llm-pi-ai-event-stream-end stream
                                (llm-pi-ai-make-assistant-message
                                 :api "a" :timestamp (1- started)))
    (should-not (llm-pi-ai-assistant-message-duration-ms
                 (llm-pi-ai-event-stream-result stream)))))

(ert-deftest llm-pi-ai-event-stream-test-does-not-time-after-done ()
  "Once a stream is done, a result passed to `end' is not timed."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (started (llm-pi-ai-event-stream-started-at stream))
         (late (llm-pi-ai-make-assistant-message :api "a" :timestamp started)))
    (llm-pi-ai-event-stream-push
     stream (llm-pi-ai-event-stream-test--done :api "a" :timestamp started))
    (llm-pi-ai-event-stream-end stream late)
    (should-not (llm-pi-ai-assistant-message-duration-ms late))))

(provide 'llm-pi-ai-event-stream-test)
;;; llm-pi-ai-event-stream-test.el ends here
