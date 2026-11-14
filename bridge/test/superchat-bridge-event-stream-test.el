;;; superchat-bridge-event-stream-test.el --- Tests for superchat-bridge-event-stream  -*- lexical-binding: t; -*-

;; Tests for Tier 0: the port of src/utils/event-stream.ts
;; (@earendil-works/pi-ai@1.4.0, commit 58c0b02c).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-bridge-types)
(require 'superchat-bridge-event-stream)

;;; Helpers

(defun superchat-bridge-event-stream-test--text-delta (delta &optional partial)
  "Return a \"text_delta\" event carrying DELTA."
  (superchat-bridge-make-event :type "text_delta" :content-index 0 :delta delta
                        :partial partial))

(defun superchat-bridge-event-stream-test--done (&rest args)
  "Return a \"done\" event for an assistant message built from ARGS."
  (superchat-bridge-make-event :type "done" :reason "stop"
                        :message (apply #'superchat-bridge-make-assistant-message args)))

(defun superchat-bridge-event-stream-test--error-message (&rest args)
  "Return an \"error\" event for an assistant message built from ARGS."
  (superchat-bridge-make-event :type "error" :reason "error"
                        :error (apply #'superchat-bridge-make-assistant-message
                                      :stop-reason "error" args)))

;;; The generic queue

(ert-deftest superchat-bridge-event-stream-test-fifo-order ()
  "The queue hands values out in the order they were enqueued."
  (let ((queue (superchat-bridge-fifo-queue-create)))
    (should (= (superchat-bridge-fifo-queue-length queue) 0))
    (should-not (superchat-bridge-fifo-queue-dequeue queue))
    (dotimes (i 5) (superchat-bridge-fifo-queue-enqueue queue i))
    (should (= (superchat-bridge-fifo-queue-length queue) 5))
    (should (equal (cl-loop repeat 5 collect (superchat-bridge-fifo-queue-dequeue queue))
                   '(0 1 2 3 4)))
    (should (= (superchat-bridge-fifo-queue-length queue) 0))))

(ert-deftest superchat-bridge-event-stream-test-fifo-interleaved ()
  "Enqueuing between dequeues still yields FIFO order."
  (let ((queue (superchat-bridge-fifo-queue-create)))
    (superchat-bridge-fifo-queue-enqueue queue 'a)
    (superchat-bridge-fifo-queue-enqueue queue 'b)
    (should (eq (superchat-bridge-fifo-queue-dequeue queue) 'a))
    (superchat-bridge-fifo-queue-enqueue queue 'c)
    (should (eq (superchat-bridge-fifo-queue-dequeue queue) 'b))
    (should (eq (superchat-bridge-fifo-queue-dequeue queue) 'c))
    (should (= (superchat-bridge-fifo-queue-length queue) 0))))

;;; Delivering events

(ert-deftest superchat-bridge-event-stream-test-queued-events ()
  "Events pushed before iteration are delivered in order."
  (let ((feed (cons nil nil))
        (stream (superchat-bridge-create-assistant-message-event-stream)))
    (setcar feed (lambda (event) (superchat-bridge-event-stream-push stream event)))
    (setcdr feed (lambda (&optional result) (superchat-bridge-event-stream-end stream result)))
    (funcall (car feed) (superchat-bridge-event-stream-test--text-delta "a"))
    (funcall (car feed) (superchat-bridge-event-stream-test--text-delta "b"))
    (funcall (cdr feed))
    (should (equal (mapcar #'superchat-bridge-assistant-message-event-delta
                           (superchat-bridge-event-stream-to-list stream))
                   '("a" "b")))
    (should (superchat-bridge-event-stream-done-p stream))))

(ert-deftest superchat-bridge-event-stream-test-waiting-consumer ()
  "A consumer that waits is served the next event, then the end."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (received 'none)
         (ended 'none))
    ;; The first step waits, because nothing has been pushed yet.
    (should-not (superchat-bridge-event-stream-next
                 stream
                 (lambda (value &optional done)
                   (setq received (list value done)))))
    (should (eq received 'none))
    (should (= (superchat-bridge-event-stream-waiting-count stream) 1))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "x"))
    (should (equal (superchat-bridge-assistant-message-event-delta (car received)) "x"))
    (should-not (cadr received))
    (should (= (superchat-bridge-event-stream-waiting-count stream) 0))
    ;; Nothing is queued and the stream is not done: wait again.
    (should-not (superchat-bridge-event-stream-next
                 stream
                 (lambda (_value &optional done) (setq ended done))))
    (should (eq ended 'none))
    (superchat-bridge-event-stream-end stream)
    (should ended)))

(ert-deftest superchat-bridge-event-stream-test-waiters-served-in-order ()
  "Each waiting consumer takes the next event, in the order it waited."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (first nil)
         (second nil))
    (superchat-bridge-event-stream-next
     stream (lambda (value &optional _done) (setq first value)))
    (superchat-bridge-event-stream-next
     stream (lambda (value &optional _done) (setq second value)))
    (should (= (superchat-bridge-event-stream-waiting-count stream) 2))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "1"))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "2"))
    (should (equal (superchat-bridge-assistant-message-event-delta first) "1"))
    (should (equal (superchat-bridge-assistant-message-event-delta second) "2"))))

(defun superchat-bridge-event-stream-test--describe (event)
  "Return what an iteration callback records for EVENT."
  (let ((type (superchat-bridge-assistant-message-event-type event)))
    (if (equal type "text_delta")
        (superchat-bridge-assistant-message-event-delta event)
      (intern type))))

(ert-deftest superchat-bridge-event-stream-test-iteration-finished-stream ()
  "`superchat-bridge-event-stream-iterate' drains a stream already filled and ended."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (seen nil)
         (ended 0))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "a"))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "b"))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--done :api "a"))
    (should (superchat-bridge-event-stream-done-p stream))
    (should (= (superchat-bridge-event-stream-iterate
                stream
                (lambda (event) (push (superchat-bridge-event-stream-test--describe event) seen))
                (lambda () (setq ended (1+ ended))))
               3))
    ;; Delivered in order, the end reported once, and nothing left over.
    (should (equal (nreverse seen) '("a" "b" done)))
    (should (= ended 1))
    (should-not (superchat-bridge-event-stream-to-list stream))
    (should-not (superchat-bridge-event-stream-waiting-p stream))))

(ert-deftest superchat-bridge-event-stream-test-iteration-live-stream ()
  "An iterator on a live stream runs as the producer pushes, not before."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (seen nil)
         (ended 0)
         (returned 0))
    ;; Nothing is queued: the iterator waits instead of spinning.
    (setq returned
          (superchat-bridge-event-stream-iterate
           stream
           (lambda (event) (push (superchat-bridge-event-stream-test--describe event) seen))
           (lambda () (setq ended (1+ ended)))))
    (should (= returned 0))
    (should-not seen)
    (should (= ended 0))
    (should (superchat-bridge-event-stream-waiting-p stream))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "a"))
    (should (equal seen '("a")))
    (should (= ended 0))
    ;; The callback runs from within `push' and comes back to `next'.
    (should (superchat-bridge-event-stream-waiting-p stream))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--done :api "a"))
    (should (equal seen '(done "a")))
    (should (= ended 1))
    (should-not (superchat-bridge-event-stream-waiting-p stream))
    ;; The stream had not ended when the iterator returned, so it does not
    ;; give a final count here; events pushed after the end are dropped.
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "late"))
    (should (equal seen '(done "a")))
    (should (= ended 1))))

(ert-deftest superchat-bridge-event-stream-test-iteration-reentrant-push ()
  "An event pushed from the callback is delivered, not nested in it."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (seen nil)
         (depth 0)
         (ended 0))
    ;; Queued, not yet ended: the iterator takes "a" at once, then waits.
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "a"))
    (superchat-bridge-event-stream-iterate
     stream
     (lambda (event)
       (let ((delta (superchat-bridge-assistant-message-event-delta event)))
         (push delta seen)
         (when (equal delta "a")
           (setq depth (1+ depth))
           ;; This push finds the loop back in `next', not in the
           ;; callback, so the new event waits for the loop.
           (superchat-bridge-event-stream-push
            stream (superchat-bridge-event-stream-test--text-delta "b"))))
       (should (<= depth 1)))
     (lambda () (setq ended (1+ ended))))
    ;; "a" was taken; the event pushed from its callback was queued while
    ;; the loop was back in `next', so it is taken at once, in order.
    (should (equal seen '("b" "a")))
    (should (= depth 1))
    (should (superchat-bridge-event-stream-waiting-p stream))
    (superchat-bridge-event-stream-end stream)
    (should (equal seen '("b" "a")))
    (should (= ended 1))))

(ert-deftest superchat-bridge-event-stream-test-iteration-after-end ()
  "An iterator on an ended stream reports the end and stops there."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (seen nil)
         (ended 0))
    ;; Nothing is queued and the stream is done: one step, the end.
    (superchat-bridge-event-stream-end stream)
    (should (= (superchat-bridge-event-stream-iterate
                stream
                (lambda (event) (push (superchat-bridge-event-stream-test--describe event) seen))
                (lambda () (setq ended (1+ ended))))
               0))
    (should-not seen)
    (should (= ended 1))
    (should-not (superchat-bridge-event-stream-waiting-p stream))
    ;; `end' on a stream that already ended reports nothing more.
    (superchat-bridge-event-stream-end stream)
    (should (= ended 1))))

(ert-deftest superchat-bridge-event-stream-test-iteration-callback-error ()
  "An error in the callback unwinds the iterator and drops its waiter."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (ended 0))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "a"))
    (should-error
     (superchat-bridge-event-stream-iterate
      stream (lambda (_event) (error "boom")) (lambda () (setq ended (1+ ended))))
     :type 'error)
    (should (= ended 0))
    ;; Unwound: no waiter left behind, so a later push just queues.
    (should-not (superchat-bridge-event-stream-waiting-p stream))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--text-delta "b"))
    (should (equal (mapcar #'superchat-bridge-assistant-message-event-delta
                           (superchat-bridge-event-stream-to-list stream))
                   '("b")))))

(ert-deftest superchat-bridge-event-stream-test-query-state ()
  "A stream reports whether it is waiting, active or done."
  (let ((stream (superchat-bridge-create-assistant-message-event-stream)))
    (should (eq (superchat-bridge-event-stream-state stream) 'active))
    (should-not (superchat-bridge-event-stream-waiting-p stream))
    (superchat-bridge-event-stream-end stream)
    (should (eq (superchat-bridge-event-stream-state stream) 'done))
    (should (superchat-bridge-event-stream-done-p stream))
    (should-not (superchat-bridge-event-stream-waiting-p stream))))

;;; Results

(ert-deftest superchat-bridge-event-stream-test-done-result ()
  "A \"done\" event finishes the stream and sets its result."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (message (superchat-bridge-make-assistant-message :api "a")))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "done" :reason "stop" :message message))
    (should (superchat-bridge-event-stream-done-p stream))
    (should (eq (superchat-bridge-event-stream-result stream) message))))

(ert-deftest superchat-bridge-event-stream-test-error-result ()
  "An \"error\" event sets the stream's result to the error message."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (message (superchat-bridge-make-assistant-message :stop-reason "error")))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "error" :reason "error" :error message))
    (should (superchat-bridge-event-stream-done-p stream))
    (should (eq (superchat-bridge-event-stream-result stream) message))))

(ert-deftest superchat-bridge-event-stream-test-end-with-result ()
  "`end' makes the stream done; its result is the one passed, or nil."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (message (superchat-bridge-make-assistant-message :api "a")))
    (superchat-bridge-event-stream-end stream message)
    (should (superchat-bridge-event-stream-done-p stream))
    (should (eq (superchat-bridge-event-stream-result stream) message)))
  (let ((stream (superchat-bridge-create-assistant-message-event-stream)))
    (superchat-bridge-event-stream-end stream)
    (should (superchat-bridge-event-stream-done-p stream))
    (should-not (superchat-bridge-event-stream-result stream))))

(ert-deftest superchat-bridge-event-stream-test-push-after-done-is-dropped ()
  "Events pushed after the stream is done are discarded."
  (let ((stream (superchat-bridge-create-assistant-message-event-stream)))
    (superchat-bridge-event-stream-push stream (superchat-bridge-event-stream-test--done :api "a"))
    ;; The completing event itself is still delivered, as in the source.
    (should (equal (mapcar #'superchat-bridge-assistant-message-event-type
                           (superchat-bridge-event-stream-to-list stream))
                   '("done")))
    (should-not (superchat-bridge-event-stream-push
                 stream (superchat-bridge-event-stream-test--text-delta "late")))
    (should-not (superchat-bridge-event-stream-to-list stream))
    (should (superchat-bridge-assistant-message-p (superchat-bridge-event-stream-result stream)))))

(ert-deftest superchat-bridge-event-stream-test-resolve-once ()
  "Only the first result counts; a later `end' result is ignored."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (first (superchat-bridge-make-assistant-message :api "first"))
         (second (superchat-bridge-make-assistant-message :api "second")))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-make-event :type "done" :reason "stop" :message first))
    (superchat-bridge-event-stream-end stream second)
    (should (eq (superchat-bridge-event-stream-result stream) first))
    (should (equal (superchat-bridge-assistant-message-api
                    (superchat-bridge-event-stream-result stream))
                   "first"))))

;;; Timing

(ert-deftest superchat-bridge-event-stream-test-times-final-message ()
  "The final message gets a duration measured from the stream's creation."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (started (superchat-bridge-event-stream-started-at stream)))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-event-stream-test--done :api "a" :timestamp (floor (* 1000 started))))
    (let ((duration (superchat-bridge-assistant-message-duration-ms
                     (superchat-bridge-event-stream-result stream))))
      (should (integer-or-marker-p duration))
      (should (<= 0 duration)))))

(ert-deftest superchat-bridge-event-stream-test-keeps-existing-duration ()
  "A message that already has `duration-ms' keeps it."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (started (superchat-bridge-event-stream-started-at stream))
         (message (superchat-bridge-make-assistant-message
                   :api "a" :timestamp (floor (* 1000 started)) :duration-ms 7)))
    (superchat-bridge-event-stream-end stream message)
    (should (equal (superchat-bridge-assistant-message-duration-ms
                    (superchat-bridge-event-stream-result stream))
                   7))))

(ert-deftest superchat-bridge-event-stream-test-leaves-older-message-untimed ()
  "A message timestamped before the stream started stays untimed."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (started (superchat-bridge-event-stream-started-at stream)))
    (superchat-bridge-event-stream-end stream
                                (superchat-bridge-make-assistant-message
                                 :api "a" :timestamp (- (floor (* 1000 started)) 1000)))
    (should-not (superchat-bridge-assistant-message-duration-ms
                 (superchat-bridge-event-stream-result stream)))))

(ert-deftest superchat-bridge-event-stream-test-does-not-time-after-done ()
  "Once a stream is done, a result passed to `end' is not timed."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (started (superchat-bridge-event-stream-started-at stream))
         (late (superchat-bridge-make-assistant-message :api "a" :timestamp (floor (* 1000 started)))))
    (superchat-bridge-event-stream-push
     stream (superchat-bridge-event-stream-test--done :api "a" :timestamp (floor (* 1000 started))))
    (superchat-bridge-event-stream-end stream late)
    (should-not (superchat-bridge-assistant-message-duration-ms late))))

(provide 'superchat-bridge-event-stream-test)
;;; superchat-bridge-event-stream-test.el ends here
