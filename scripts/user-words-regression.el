;;; user-words-regression.el --- the user's earlier words stay in the history -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "the user's earlier messages stay in what
;; the model is sent"
;; (docs/decisions/accepted/2026-11-09-the-users-earlier-words-stay.md).
;; When the character budget cuts the earlier turns short, the user messages
;; older than the selection are kept ahead of it within one eighth of the
;; budget -- not only the nearest one, which the rule of 2026-10-22 keeps --
;; and the user is told once for a conversation when a message of theirs from
;; the last twelve hours is left out all the same.
;;
;; It reuses the history-messages suite's fixture and wire capture: every
;; provider request is serialized with llm.el's own OpenAI-compatible request
;; builder, the form DeepSeek receives.  Its own file rather than more tests in
;; that suite so the suite stays as the 2026-10-22 rule left it, and each
;; decision's tests can be run and read on their own.
;;
;; Read-round results here are 132 characters as a history line
;; ("Tool: read_buffer(...) -> [ok] ..."), "User: 继续" is 8, and each line
;; costs one more for its separator; the budgets below are chosen from those.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/user-words-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^user-words/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-pipeline--recent-words-clock)
(defvar superchat-pipeline--recent-words-told)

(defconst uw--task "UW-TASK export the messages"
  "The task, said once at the start: 33 characters as a history line.")

(defvar uw--envelopes nil
  "History selections of this test's chat requests, newest first.")

(defvar uw--notices nil
  "Echo-area lines of this test that tell the user words were left out.")

(defmacro uw--capturing (&rest body)
  "Run BODY recording each chat request's selection and each notice."
  (declare (indent 0) (debug t))
  `(let ((submit (symbol-function 'superchat-runtime-submit))
         (say (symbol-function 'message))
         (uw--envelopes nil)
         (uw--notices nil))
     (when (boundp 'superchat-pipeline--recent-words-told)
       (clrhash superchat-pipeline--recent-words-told))
     (cl-letf (((symbol-function 'superchat-runtime-submit)
                (lambda (request)
                  (push (plist-get request :context-envelope) uw--envelopes)
                  (funcall submit request)))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (let ((text (and format-string
                                   (apply #'format-message format-string args))))
                    (when (and text (string-match-p "no longer sent" text))
                      (push text uw--notices))
                    (apply say format-string args)))))
       ,@body)))

(defun uw--reads (n)
  "N model replies, each one read_buffer call."
  (cl-loop repeat n collect (hm--read-round "hm-target")))

(defun uw--answer (name &optional length)
  "A scripted final answer starting with NAME, padded to LENGTH characters."
  (list :text (if length (concat name " " (make-string (- length (length name) 1) ?a)) name)))

(defun uw--users (wire)
  "The texts of WIRE's earlier user messages, in order.
The new input, last, is left out: it carries the request's Emacs origin."
  (cl-loop for message in (butlast (hm--body wire))
           when (equal "user" (alist-get 'role message))
           collect (hm--text message)))

(defun uw--call-ids (wire)
  (cl-loop for message in (hm--body wire)
           append (mapcar (lambda (call) (alist-get 'id call)) (hm--calls message))))

(defun uw--answered-ids (wire)
  (cl-loop for message in (hm--body wire)
           when (equal "tool" (alist-get 'role message))
           collect (alist-get 'tool_call_id message)))

(defun uw--lines (envelope)
  "The history lines ENVELOPE selected, each as the budget counts it."
  (mapcar #'superchat-view--continuity-item-line
          (append (plist-get envelope :items) nil)))

(defun uw--chars (lines)
  "Characters LINES take in the history: their lengths and a separator each but one."
  (if lines (1- (cl-loop for line in lines sum (1+ (length line)))) 0))

;;;; The incident: a task, then "go on" after "go on"

(defun uw--incident (budget)
  "The task with two calls, three \"继续\" turns of two calls, one of ten;
then \"继续\" again under BUDGET.  Return the chat View."
  (setq smoke--script
        (append (uw--reads 2) (list (uw--answer "UW-ANSWER-1"))
                (uw--reads 2) (list (uw--answer "UW-ANSWER-2"))
                (uw--reads 2) (list (uw--answer "UW-ANSWER-3"))
                (uw--reads 2) (list (uw--answer "UW-ANSWER-4"))
                (uw--reads 10) (list (uw--answer "UW-ANSWER-5"))
                (list (uw--answer "UW-ANSWER-6"))))
  (let ((view (smoke--open-chat)))
    (hm--turn view uw--task)
    (dotimes (_ 4) (hm--turn view "继续"))
    (let ((superchat-context-max-chars budget))
      (hm--turn view "继续"))
    view))

(ert-deftest user-words/the-task-behind-go-on-stays ()
  "The incident of 2026-11-09: the task, then turns that are each a short
\"继续\" and many calls, under a budget that ends the walk inside the newest
turn.  The request carries the task, the \"继续\" messages, and the newest
calls with their results, in that order.  With only the nearest message kept
the task was left out and the model asked what it was supposed to do."
  (hm--fixture nil
    (uw--capturing
      (uw--incident 1000)
      (let* ((wire (car hm--wires))
             (body (hm--body wire))
             (calls (uw--call-ids wire)))
        ;; The walk did end inside the newest turn: of its ten calls some
        ;; are left out, and no earlier turn's answer is there.
        (should (< 0 (length calls) 10))
        (should-not (cl-some (lambda (m) (string-match-p "UW-ANSWER-[1-4]" (hm--text m)))
                             body))
        ;; Task, the four earlier "继续", then the newest turn's calls.
        (should (equal (list uw--task "继续" "继续" "继续" "继续")
                       (uw--users wire)))
        (should (equal '("user" "user" "user" "user" "user" "assistant" "tool")
                       (cl-subseq (hm--roles wire) 0 7)))
        (should (string-match-p "UW-ANSWER-5" (hm--text (nth (- (length body) 2) body))))
        (should (equal calls (uw--answered-ids wire)))))))

(ert-deftest user-words/a-long-paste-does-not-hide-the-task ()
  "A long user message between the task and the selection does not fit the
eighth: it is passed over, and the task behind it is still kept."
  (hm--fixture (append (list (uw--answer "UW-ANSWER-1") (uw--answer "UW-ANSWER-2"))
                       (uw--reads 10) (list (uw--answer "UW-ANSWER-3"))
                       (list (uw--answer "UW-ANSWER-4")))
    (uw--capturing
      (let ((view (smoke--open-chat)))
        (hm--turn view uw--task)
        (hm--turn view (concat "UW-PASTE " (make-string 300 ?p)))
        (hm--turn view "继续")
        (let ((superchat-context-max-chars 1000))
          (hm--turn view "继续")))
      (let ((wire (car hm--wires)))
        (should (equal (list uw--task "继续") (uw--users wire)))
        (should-not (cl-some (lambda (text) (string-match-p "UW-PASTE" text))
                             (mapcar #'hm--text (hm--body wire))))
        (should (equal (uw--call-ids wire) (uw--answered-ids wire)))))))

(ert-deftest user-words/the-eighth-holds ()
  "With many earlier user messages only the newest that fit one eighth of the
budget are kept, and the selection loses no more to them than their length:
after it made room, not one more of its records would fit."
  (let ((notes (cl-loop for n from 1 to 12
                        ;; 24 characters each: "User: " and this make 30.
                        collect (format "UW-NOTE-%02d %s" n (make-string 13 ?n)))))
    (hm--fixture (append (cl-loop for n from 1 to 12
                                  collect (uw--answer (format "UW-ANSWER-%02d" n)))
                         (uw--reads 10) (list (uw--answer "UW-ANSWER-GO"))
                         (list (uw--answer "UW-ANSWER-LAST")))
      (uw--capturing
        (let ((view (smoke--open-chat))
              (budget 1200))
          (dolist (note notes) (hm--turn view note))
          (hm--turn view "UW-GO on")
          (let ((superchat-context-max-chars budget))
            (hm--turn view "继续"))
          (let* ((wire (car hm--wires))
                 (lines (uw--lines (car uw--envelopes)))
                 (user-lines (cl-loop for line in lines
                                      while (string-prefix-p "User: " line)
                                      collect line))
                 (rest (nthcdr (length user-lines) lines)))
            ;; The four newest notes and the nearest message; the eight
            ;; older notes would have gone past the eighth (150).
            (should (equal (append (last notes 4) (list "UW-GO on"))
                           (uw--users wire)))
            (should (<= (uw--chars user-lines) (/ budget 8)))
            (should (> (+ (uw--chars user-lines) 1 31) (/ budget 8)))
            ;; Within the budget, and tight: one more call would not fit.
            (should (<= (uw--chars lines) budget))
            (should (cl-every (lambda (line) (= 132 (length line)))
                              (butlast rest)))
            (should (> (+ (uw--chars lines) 1 132) budget))
            (should (equal (uw--call-ids wire) (uw--answered-ids wire)))))))))

(ert-deftest user-words/an-oldest-user-message-no-longer-stops-them ()
  "When the oldest record the walk chose is itself a user message, the rule of
2026-10-22 adds nothing; the user's earlier messages are kept ahead of it all
the same.  On the wire: two user messages in a row, the second the
selection's own first record (case c)."
  (hm--fixture (list (uw--answer "UW-LONG-ANSWER" 700)
                     (hm--read-round "hm-target") (uw--answer "UW-ANSWER-2")
                     (uw--answer "UW-ANSWER-3"))
    (uw--capturing
      (let ((view (smoke--open-chat)))
        (hm--turn view uw--task)
        (hm--turn view "UW-STEP two")
        (let ((superchat-context-max-chars 800))
          (hm--turn view "继续")))
      (let ((wire (car hm--wires)))
        (should-not (cl-some (lambda (m) (string-match-p "UW-LONG-ANSWER" (hm--text m)))
                             (hm--body wire)))
        (should (equal '("user" "user" "assistant" "tool" "assistant" "user")
                       (hm--roles wire)))
        (should (equal (list uw--task "UW-STEP two") (uw--users wire)))))))

(ert-deftest user-words/ahead-of-an-assistant-text ()
  "The selection's first record is an assistant text (case b): the kept user
messages come straight before it, in Tape order."
  (hm--fixture (list (uw--answer "UW-ANSWER-1")
                     (hm--read-round "hm-target") (uw--answer "UW-ANSWER-2" 200)
                     (uw--answer "UW-ANSWER-3" 200)
                     (uw--answer "UW-ANSWER-4"))
    (uw--capturing
      (let ((view (smoke--open-chat)))
        (hm--turn view uw--task)
        (hm--turn view "继续")
        (hm--turn view "UW-NEXT")
        (let ((superchat-context-max-chars 500))
          (hm--turn view "继续")))
      (let* ((wire (car hm--wires))
             (body (hm--body wire)))
        (should (equal '("user" "user" "assistant" "user" "assistant" "user")
                       (hm--roles wire)))
        (should (equal (list uw--task "继续" "UW-NEXT") (uw--users wire)))
        (should (string-prefix-p "UW-ANSWER-2" (hm--text (nth 2 body))))
        (should-not (uw--call-ids wire))))))

(ert-deftest user-words/calls-keep-their-results-and-the-evidence-rebuilds-it ()
  "Keeping the user's words never separates a call from its result, and the
request evidence rebuilds what was kept -- the task first -- with no
`prompt-reconstructable' violation on the Tape."
  (hm--fixture nil
    (uw--capturing
      (let* ((view (uw--incident 1000))
             (session (hm--session view))
             (wire (car hm--wires))
             (lists (hm--events session "request.message-list"))
             (evidence (superchat-tape-request-evidence
                        session (plist-get (plist-get (car (last lists)) :payload)
                                           :request-id)))
             (messages (plist-get evidence :messages)))
        (should (equal (uw--call-ids wire) (uw--answered-ids wire)))
        (should (eq 'reproduced (plist-get evidence :status)))
        ;; The evidence is the history; the wire adds the new input.
        (should (equal (butlast (hm--roles wire))
                       (mapcar (lambda (m) (plist-get m :kind)) messages)))
        (should (equal uw--task (plist-get (car messages) :content)))
        (should (equal (uw--call-ids wire)
                       (cl-loop for m in messages
                                append (mapcar (lambda (call) (plist-get call :id))
                                               (plist-get m :tool-calls)))))
        (should (equal '(reproduced . 0) (smoke--case-reconstruction session)))))))

;;;; The notice

(defconst uw--long-task (concat "UW-TASK " (make-string 200 ?t))
  "A task too long for the eighth of a 1000-character budget.")

(defun uw--left-out-script ()
  "The long task, then \"继续\" with ten calls, then three more answers."
  (append (list (uw--answer "UW-ANSWER-1"))
          (uw--reads 10) (list (uw--answer "UW-ANSWER-2"))
          (list (uw--answer "UW-ANSWER-3") (uw--answer "UW-ANSWER-4")
                (uw--answer "UW-ANSWER-5"))))

(ert-deftest user-words/the-user-is-told-once ()
  "A recent user message the history leaves out is said once for the
conversation, in one echo-area line naming how many, the budget and what to
do; the next request that leaves it out says nothing more.  Nothing of it is
on the Tape or in a request."
  (hm--fixture (uw--left-out-script)
    (uw--capturing
      (let ((view (smoke--open-chat)))
        (hm--turn view uw--long-task)
        (hm--turn view "继续")
        (should-not uw--notices)
        (let ((superchat-context-max-chars 1000))
          (hm--turn view "继续")
          (should-not (cl-some (lambda (text) (string-match-p "UW-TASK" text))
                               (uw--users (car hm--wires))))
          (should (= 1 (length uw--notices)))
          (let ((notice (car uw--notices)))
            (should-not (string-match-p "\n" notice))
            (dolist (needle '("1 of your messages" "1000 characters" "/compact"
                              "superchat-context-max-chars"
                              "superchat-llm-context-window-overrides"))
              (should (string-match-p (regexp-quote needle) notice))))
          (hm--turn view "继续")
          (should-not (cl-some (lambda (text) (string-match-p "UW-TASK" text))
                               (uw--users (car hm--wires))))
          (should (= 1 (length uw--notices))))
        (should-not (cl-some (lambda (event)
                               (string-match-p "no longer sent"
                                               (format "%S" event)))
                             (superchat-tape-replay-events (hm--session view))))
        (should-not (cl-some (lambda (wire)
                               (cl-some (lambda (m) (string-match-p "no longer sent"
                                                                     (hm--text m)))
                                        wire))
                             hm--wires))))))

(ert-deftest user-words/not-told-when-only-calls-and-answers-go ()
  "Calls and answers left out, every user message kept: nothing is said.  A
user message left out afterwards in the same conversation is."
  (hm--fixture nil
    (uw--capturing
      (let ((view (uw--incident 1000)))
        (should (equal (list uw--task "继续" "继续" "继续" "继续")
                       (uw--users (car hm--wires))))
        (should-not uw--notices)
        ;; Three user messages of over 400 characters: the eighth of 1000
        ;; holds none of them, and the walk ends on the oldest, which no
        ;; room can be made for.
        (setq smoke--script
              (cl-loop for n from 1 to 4 collect (uw--answer (format "UW-MORE-%d" n))))
        (dotimes (n 3)
          (hm--turn view (format "UW-LONG-%d %s" n (make-string 400 ?l))))
        (let ((superchat-context-max-chars 1000))
          (hm--turn view "继续"))
        (should (= 1 (length uw--notices)))))))

(ert-deftest user-words/not-told-for-words-older-than-twelve-hours ()
  "A user message written more than twelve hours ago is not said to be left
out; one written within them is."
  (hm--fixture (uw--left-out-script)
    (uw--capturing
      (let ((view (smoke--open-chat)))
        (hm--turn view uw--long-task)
        (hm--turn view "继续")
        (let ((superchat-context-max-chars 1000))
          (let ((superchat-pipeline--recent-words-clock
                 (lambda () (time-add (current-time) (* 13 60 60)))))
            (hm--turn view "继续"))
          (should-not uw--notices)
          (let ((superchat-pipeline--recent-words-clock
                 (lambda () (time-add (current-time) (* 11 60 60)))))
            (hm--turn view "继续"))
          (should (= 1 (length uw--notices))))))))

(ert-deftest user-words/not-told-for-another-surface ()
  "Only the chat surface's own history is told about.  The same selection
seen by a sub-agent, a workflow step, compaction or skill matching says
nothing; a request inspected but not sent says nothing either."
  (hm--fixture (uw--left-out-script)
    (let (calls)
      (uw--capturing
        (let ((view (smoke--open-chat))
              (tell (symbol-function 'superchat-pipeline--tell-recent-words-left-out)))
          (hm--turn view uw--long-task)
          (hm--turn view "继续")
          (let ((superchat-context-max-chars 1000))
            ;; Inspecting a request that leaves the task out.
            (let ((inspector (hm--inspect "UW-INSPECTED")))
              (with-current-buffer inspector
                (should (string-match-p "UW-ANSWER-2" (buffer-string)))
                (should-not (string-match-p "UW-TASK" (buffer-string))))
              (kill-buffer inspector))
            (should-not uw--notices)
            (cl-letf (((symbol-function 'superchat-pipeline--tell-recent-words-left-out)
                       (lambda (&rest args)
                         (push args calls)
                         (apply tell args))))
              (hm--turn view "继续")))
          (should (= 1 (length uw--notices)))
          (should (= 1 (length calls)))
          (pcase-let ((`(,envelope ,session ,_surface ,selection) (car calls)))
            (dolist (surface '(subagent workflow compaction skills inline-ask))
              (clrhash superchat-pipeline--recent-words-told)
              (funcall tell envelope session surface selection)
              (should (= 1 (length uw--notices))))
            (clrhash superchat-pipeline--recent-words-told)
            (funcall tell envelope session 'chat selection)
            (should (= 2 (length uw--notices)))))))))

;; A conversation nobody opened is told nothing (reviewer's follow-up, 2026-11-09)

(defun uw--leave-out-the-task (view)
  "In VIEW: the long task, \"继续\" with ten calls, then \"继续\" under 1000."
  (hm--turn view uw--long-task)
  (hm--turn view "继续")
  (let ((superchat-context-max-chars 1000))
    (hm--turn view "继续"))
  (should-not (cl-some (lambda (text) (string-match-p "UW-TASK" text))
                       (uw--users (car hm--wires)))))

(ert-deftest user-words/not-told-in-a-conversation-nobody-opened ()
  "A conversation Superchat runs by itself (`conversation.background', as a
timer's task or a woken Work Record has) whose history leaves out a recent
user message says nothing and is not marked as told: its user messages are
what Superchat wrote, and nobody is looking at it.  A conversation the user
opened, in the same test, still gets the line."
  (let ((one-turn (append (list (uw--answer "UW-ANSWER-1"))
                          (uw--reads 10) (list (uw--answer "UW-ANSWER-2"))
                          (list (uw--answer "UW-ANSWER-3")))))
    (hm--fixture (append one-turn one-turn)
      (uw--capturing
        (let* ((background (superchat-workspace-new-conversation nil t))
               (session (hm--session background)))
          (should (plist-get (superchat-view-session-configuration session)
                             :background))
          (uw--leave-out-the-task background)
          (should-not uw--notices)
          (should-not (gethash session superchat-pipeline--recent-words-told)))
        (let ((view (smoke--open-chat)))
          (should-not (plist-get (superchat-view-session-configuration
                                  (hm--session view))
                                 :background))
          (uw--leave-out-the-task view)
          (should (= 1 (length uw--notices))))))))

(provide 'user-words-regression)

;;; user-words-regression.el ends here
