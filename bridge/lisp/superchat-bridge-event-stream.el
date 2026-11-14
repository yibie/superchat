;;; superchat-bridge-event-stream.el --- Event streams for pi-ai responses  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/event-stream.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below
;; `packages/ai/'.
;;
;; Source's `EventStream<T, R>' is an asynchronous iterator: a producer
;; pushes events, a consumer pulls them with `for await', and a promise
;; resolves to the response once the stream is complete.  Emacs Lisp has
;; no promises, so the port keeps the parts apart:
;;
;;   `superchat-bridge-event-stream-push'     producer: deliver one event
;;   `superchat-bridge-event-stream-end'      producer: no more events
;;   `superchat-bridge-event-stream-next'     consumer: take the next event
;;   `superchat-bridge-event-stream-iterate'  consumer: fold the whole stream
;;
;; `next' never blocks.  With an event queued, or with the stream done,
;; it calls its continuation at once; otherwise the continuation waits,
;; and the next `push' or `end' calls it.  A continuation is
;; `(lambda (VALUE &optional DONE) ...)': with DONE nil VALUE is the
;; event, and with DONE non-nil the stream ended and VALUE is nil.
;; Nothing can be waited for that arrives later in the same call stack,
;; so `iterate' is the source's asynchronous `for await' as a callback:
;; it chains `next' calls, and a stream not yet ended is consumed from
;; within the `push' calls that deliver its events.  A stream already
;; filled and ended, as in a test, is consumed in one call.
;;
;; `superchat-bridge-event-stream-result' stands in for the promise: it is the
;; value a `done' or `error' event extracts, or the one passed to `end',
;; and it stays nil until the stream resolves it.
;;
;; `superchat-bridge-create-assistant-message-event-stream' adds the timing
;; the source gives the final message: `duration-ms' is measured from
;; the stream's creation and set on the final message unless it already
;; has one or was timestamped before the stream existed.

;;; Code:

(require 'cl-lib)
(require 'superchat-bridge-types)

;;; FIFO queue

(cl-defstruct (superchat-bridge-fifo-queue
               (:constructor superchat-bridge-fifo-queue-create)
               (:copier nil))
  "Source's private `FifoQueue<T>'.
`incoming' takes values at the back; they move to `outgoing' reversed
the first time a value is wanted, so a run of enqueues costs nothing."
  (incoming nil)
  (outgoing nil))

(defun superchat-bridge-fifo-queue-length (queue)
  "Return how many values QUEUE holds."
  (+ (length (superchat-bridge-fifo-queue-incoming queue))
     (length (superchat-bridge-fifo-queue-outgoing queue))))

(defun superchat-bridge-fifo-queue-enqueue (queue value)
  "Add VALUE at the back of QUEUE and return it."
  (push value (superchat-bridge-fifo-queue-incoming queue))
  value)

(defun superchat-bridge-fifo-queue-dequeue (queue)
  "Remove and return the front value of QUEUE, or nil when it is empty."
  (unless (superchat-bridge-fifo-queue-outgoing queue)
    (while (superchat-bridge-fifo-queue-incoming queue)
      (push (pop (superchat-bridge-fifo-queue-incoming queue))
            (superchat-bridge-fifo-queue-outgoing queue))))
  (pop (superchat-bridge-fifo-queue-outgoing queue)))

;;; Event streams

(cl-defstruct (superchat-bridge-event-stream
               (:constructor superchat-bridge-event-stream--create))
  "Source's `EventStream<T, R>'.
`queue' holds events not yet taken; `waiting' holds continuations of
consumers that asked for an event and got none; `done' says no more
events will be delivered; `final-result' is the resolved result or
nil; `is-complete' and `extract-result' are the two functions the
source's constructor takes."
  (queue (superchat-bridge-fifo-queue-create))
  (waiting (superchat-bridge-fifo-queue-create))
  (done nil)
  (final-result nil)
  (is-complete #'ignore)
  (extract-result #'ignore))

(defun superchat-bridge-event-stream-create (is-complete extract-result)
  "Return a new event stream.
IS-COMPLETE is called with an event and says whether it finishes the
stream; EXTRACT-RESULT turns that event into the stream's result."
  (superchat-bridge-event-stream--create :is-complete is-complete
                                  :extract-result extract-result))

(defun superchat-bridge-event-stream-done-p (stream)
  "Return non-nil when STREAM is complete.
Like the source's protected `done' flag, it is set while a final event
is still being delivered."
  (superchat-bridge-event-stream-done stream))

(defun superchat-bridge-event-stream-waiting-count (stream)
  "Return how many consumers wait for an event of STREAM."
  (superchat-bridge-fifo-queue-length (superchat-bridge-event-stream-waiting stream)))

(defun superchat-bridge-event-stream-waiting-p (stream)
  "Return non-nil when a consumer of STREAM waits for an event."
  (> (superchat-bridge-event-stream-waiting-count stream) 0))

(defun superchat-bridge-event-stream-state (stream)
  "Return the state of STREAM: `done', `waiting' or `active'.
`waiting' means every event delivered so far has been taken and a
consumer waits for the next one."
  (cond ((superchat-bridge-event-stream-done-p stream) 'done)
        ((superchat-bridge-event-stream-waiting-p stream) 'waiting)
        (t 'active)))

(defun superchat-bridge-event-stream--resolve (stream result)
  "Make RESULT the result of STREAM unless it already has one."
  (unless (superchat-bridge-event-stream-final-result stream)
    (setf (superchat-bridge-event-stream-final-result stream) result)))

(defun superchat-bridge-event-stream--deliver (stream event)
  "Give EVENT to the first waiting consumer of STREAM, or queue it."
  (let ((waiter (superchat-bridge-fifo-queue-dequeue (superchat-bridge-event-stream-waiting stream))))
    (if waiter
        (funcall waiter event nil)
      (superchat-bridge-fifo-queue-enqueue (superchat-bridge-event-stream-queue stream) event))))

(defun superchat-bridge-event-stream--push (stream event)
  "The generic `push' of the source: deliver EVENT to STREAM."
  (unless (superchat-bridge-event-stream-done-p stream)
    (when (funcall (superchat-bridge-event-stream-is-complete stream) event)
      (setf (superchat-bridge-event-stream-done stream) t)
      (superchat-bridge-event-stream--resolve
       stream (funcall (superchat-bridge-event-stream-extract-result stream) event)))
    (superchat-bridge-event-stream--deliver stream event)
    event))

(defun superchat-bridge-event-stream--end (stream &optional result)
  "The generic `end' of the source: finish STREAM."
  (setf (superchat-bridge-event-stream-done stream) t)
  (when result
    (superchat-bridge-event-stream--resolve stream result))
  (while (superchat-bridge-event-stream-waiting-p stream)
    (funcall (superchat-bridge-fifo-queue-dequeue (superchat-bridge-event-stream-waiting stream))
             nil t))
  result)

(defun superchat-bridge-event-stream-result (stream)
  "Return the result STREAM resolved to, or nil while it has none.
This is the value of the source's `result()' promise once it settles."
  (superchat-bridge-event-stream-final-result stream))

;;; Consuming

(defun superchat-bridge-event-stream-next (stream continuation)
  "Ask STREAM for its next event and pass it to CONTINUATION.
CONTINUATION is called as `(CONTINUATION EVENT nil)'.  When STREAM has
an event queued, or is done, it runs at once, and with nil and t when
the stream is done; otherwise it waits for the next `push' or `end'.
Return non-nil when CONTINUATION ran without waiting."
  ;; The source writes `const [done, value] = await Promise.race([donePromise,
  ;; queue.next()])', so an event already queued when the stream ends is still
  ;; delivered, and only a stream that ended with its queue empty reports the
  ;; end.  So take the queue first: tested for `done' first, an ended
  ;; stream would report the end at once and leave its queued events behind,
  ;; which a consumer reading "ran at once" as "there is an event" never
  ;; gets past -- the loop runs on nil without end.
  (let ((queued (superchat-bridge-fifo-queue-dequeue
                 (superchat-bridge-event-stream-queue stream))))
    (cond (queued
           (funcall continuation queued nil)
           t)
          ((superchat-bridge-event-stream-done-p stream)
           (funcall continuation nil t)
           t)
          (t
           (superchat-bridge-fifo-queue-enqueue
            (superchat-bridge-event-stream-waiting stream) continuation)
           nil))))

(cl-defun superchat-bridge-event-stream-iterate (stream callback &optional finish)
  "Deliver the events of STREAM to CALLBACK until STREAM ends.
CALLBACK is called with each event not yet taken, in order; FINISH, if
given, is called once the stream is done.  This is the source's
`for await' loop: a stream not yet ended is consumed as its producer
pushes, each event running CALLBACK from within the `push' that
delivers it, and events pushed while CALLBACK runs are queued and
reach CALLBACK later, never nested in it.  Return the number of events
delivered so far: all of them when STREAM was already done, otherwise
those taken before this call returned, and the rest as the producer
pushes."
  (let ((count 0))
    (when finish
      (let ((finish finish))
        (setq finish (lambda ()
                       (when finish
                         (funcall finish)
                         (setq finish nil))))))
    (cl-labels ((step (event event-done)
                  (if event-done
                      (when finish (funcall finish))
                    (setq count (1+ count))
                    (condition-case err
                        (funcall callback event)
                      (error
                       (superchat-bridge-event-stream-cancel stream)
                       (signal (car err) (cdr err))))
                    ;; A `step' that had to wait returns nil and cannot
                    ;; come back here; a completion, or an error that
                    ;; cancelled the iterator, passes nothing on either.
                    (superchat-bridge-event-stream-next stream #'step))))
      (superchat-bridge-event-stream-next stream #'step))
    count))

(defun superchat-bridge-event-stream-cancel (stream)
  "Drop the queued continuations of STREAM.
A consumer whose `superchat-bridge-event-stream-next' is still waiting is not
called again after this."
  (setf (superchat-bridge-event-stream-waiting stream)
        (superchat-bridge-fifo-queue-create))
  nil)

(defun superchat-bridge-event-stream-to-list (stream)
  "Take the queued events of STREAM and return them as a list.
Only events pushed and not taken yet are taken; unlike a `for await'
loop this does not wait for more, and works on a stream that has not
ended.  Call `superchat-bridge-event-stream-end' first to collect the events
of a finished stream together with its end."
  ;; The loop ends on the end of the stream, not on what `next' returns:
  ;; `next' answers t both for an event and for the end, and a call that
  ;; had to wait returns nil and leaves the continuation to run later.  So
  ;; the continuation flags the end in a variable of its own, and the nil
  ;; of the end is not collected.
  (let ((events nil)
        (running t)
        (ended nil))
    (while (and running (not ended))
      (setq running (superchat-bridge-event-stream-next
                     stream
                     (lambda (value &optional done)
                       (if done
                           (setq ended t)
                         (push value events))))))
    ;; `running' nil without `ended' means the continuation waits for a
    ;; producer instead, and the events arrive as it pushes; the list holds
    ;; what was taken before this call returned.
    (nreverse events)))

;;; Assistant message event streams

(cl-defstruct (superchat-bridge-assistant-message-event-stream
               (:include superchat-bridge-event-stream)
               (:constructor superchat-bridge-event-stream--create-assistant)
               (:copier nil))
  "Source's `AssistantMessageEventStream'.
It completes on a `done' or `error' event and times the final
response; `started-at' is when it was created, in seconds, standing in
for the source's start of its monotonic clock."
  (started-at nil))

(defun superchat-bridge-event-stream--event-completes-p (event)
  "Return non-nil when EVENT is the last of a response."
  (let ((type (superchat-bridge-assistant-message-event-type event)))
    (or (equal type "done") (equal type "error"))))

(defun superchat-bridge-event-stream--event-result (event)
  "Return the response that EVENT completes a stream with.
Source throws here for any other event type; the port returns nil,
which no completion event of an assistant stream reaches."
  (let ((type (superchat-bridge-assistant-message-event-type event)))
    (cond ((equal type "done") (superchat-bridge-assistant-message-event-message event))
          ((equal type "error") (superchat-bridge-assistant-message-event-error event))
          (t nil))))

(defun superchat-bridge-event-stream-started-at (stream)
  "Return when the assistant message event STREAM was created, in seconds.
It is the start of the source's monotonic clock."
  (superchat-bridge-assistant-message-event-stream-started-at stream))

(defun superchat-bridge-create-assistant-message-event-stream ()
  "Return a new, empty assistant message event stream.
This is the source's `createAssistantMessageEventStream()'."
  (superchat-bridge-event-stream--create-assistant
   :is-complete #'superchat-bridge-event-stream--event-completes-p
   :extract-result #'superchat-bridge-event-stream--event-result
   :started-at (float-time)))

(defun superchat-bridge-event-stream--time (stream message)
  "Set the `duration-ms' of MESSAGE, the final response of STREAM.
Leave it alone when STREAM is done, when MESSAGE already has a
duration, or when MESSAGE was timestamped before STREAM was created, as
a response that started elsewhere, e.g. a deferred result."
  (when (and message
             (not (superchat-bridge-event-stream-done-p stream))
             (null (superchat-bridge-assistant-message-duration-ms message))
             (let ((timestamp (superchat-bridge-assistant-message-timestamp message)))
               (or (null timestamp)
                   ;; `timestamp' is milliseconds, `started-at' seconds.
                   (>= timestamp
                       (floor (* 1000 (superchat-bridge-event-stream-started-at
                                       stream)))))))
    (setf (superchat-bridge-assistant-message-duration-ms message)
          (max 0 (round (* 1000 (- (float-time)
                                   (superchat-bridge-event-stream-started-at stream))))))
    message))

(defun superchat-bridge-event-stream-push (stream event)
  "Deliver EVENT to STREAM, the source's `push'.
For an assistant message event stream, the message of a `done' or
`error' event is timed first; see `superchat-bridge-event-stream--time'.
Return EVENT, or nil when the stream is done and drops it."
  (when (superchat-bridge-assistant-message-event-stream-p stream)
    (let ((type (superchat-bridge-assistant-message-event-type event)))
      (cond ((equal type "done")
             (superchat-bridge-event-stream--time
              stream (superchat-bridge-assistant-message-event-message event)))
            ((equal type "error")
             (superchat-bridge-event-stream--time
              stream (superchat-bridge-assistant-message-event-error event))))))
  (superchat-bridge-event-stream--push stream event))

(defun superchat-bridge-event-stream-end (stream &optional result)
  "Finish STREAM, the source's `end'.
RESULT, when non-nil, becomes the stream's result unless one is
already resolved; for an assistant message event stream it is timed
first.  Every waiting consumer is told the stream ended."
  (when (superchat-bridge-assistant-message-event-stream-p stream)
    (when result
      (superchat-bridge-event-stream--time stream result)))
  (superchat-bridge-event-stream--end stream result))

(provide 'superchat-bridge-event-stream)

;;; superchat-bridge-event-stream.el ends here
