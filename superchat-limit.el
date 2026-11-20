;;; superchat-limit.el --- What a run says and decides at the round limit -*- lexical-binding: t; -*-

;;; Commentary:
;; A run that reaches `superchat-max-rounds' asks the model to look back,
;; then pauses or goes on.  This file holds the words of that request, reads
;; the status a look-back begins with, and counts what a paused run did.
;; Functions over a run's entries, and the judge, a request apart from the
;; run that decides whether it goes on: the loop itself stays in
;; superchat-agent.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-llm)
(require 'superchat-request)

(defconst superchat-limit-note
  "The limit of model responses in one run is reached. Call no tool now, even if one would help. Begin with one line: \"Status: finished\" when the task is done (what follows is then your answer to the person), \"Status: blocked\" when you need the person, or \"Status: continue\" when there is more to do. Then look back over the calls of this run, with the facts below: what is done, what failed and why, what is left, the plan for the next step, and what the person should check."
  "What the system prompt says of a run's last request, the round limit reached.")

(defconst superchat-limit-resume-note
  "You reached the limit of model responses and looked back; your look-back is below, after the facts. Go on: follow its plan, and change the approach where it or the facts show what went wrong."
  "What the system prompt says of the first request after a run went on.")

(defun superchat-limit-facts (entries note)
  "NOTE, then the facts of the calls in ENTRIES, a run's entries after its
`user' entry, counted from the Tape."
  (concat "\n\n" note "\n\nFacts of this run, counted from the Tape:\n"
          (superchat-request-digest entries)))

(defun superchat-limit-status (text)
  "The status a look-back TEXT begins with, `finished', `blocked' or
`continue', and TEXT without that line: (STATUS . REST).  STATUS is nil when
the first line names none."
  ;; A model may set the line in Markdown: **Status: finished**.
  (if (let ((case-fold-search t))
        (string-match "\\`[ \t\n*`]*Status:[ \t*`]*\\(finished\\|blocked\\|continue\\)[ \t.*`]*\\(?:\n\\|\\'\\)"
                      (or text "")))
      (cons (intern (downcase (match-string 1 text))) (string-trim (substring text (match-end 0))))
    (cons nil text)))

(defun superchat-limit-paused (entries)
  "The text of the `paused' event of a run the round limit stopped, whose
entries after its `user' entry are ENTRIES: what it did, counted from the
Tape, since the model's own account may be empty."
  (let ((calls (cl-loop for e in entries when (eq (plist-get e :kind) 'assistant)
                        sum (length (plist-get (plist-get e :data) :calls)))))
    (format "Paused at the round limit: %d model responses, %d tool call%s. Send a message to go on."
            superchat-max-rounds calls (if (= calls 1) "" "s"))))

(defvar superchat-limit-pending-functions nil
  "Functions called with a timeline's name: non-nil when input waits for it
outside the run, so that the run, at the round limit, pauses for it.")

(defun superchat-limit-pending-p (tape steers)
  "Non-nil when input waits for timeline TAPE: STEERS, the text steered to its
run, or what `superchat-limit-pending-functions' report."
  (or steers (run-hook-with-args-until-success 'superchat-limit-pending-functions tape)))

(defconst superchat-limit-judge-prompt
  "You judge whether an agent's task is finished. A limit on its number of responses stopped the agent, not its own choice: that it stopped says nothing. You see the task, facts counted from the record of the agent's calls, and the agent's own look-back, which may be empty, wrong or hopeful: weigh the facts above it. Begin with one line: DONE when the task as asked is done; CONTINUE when the agent can do more by itself; BLOCKED only when it cannot go on without the person (a decision only they can make, a permission, information it cannot find). Then one line why."
  "The system prompt of the judge.")

(defun superchat-limit-verdict (text)
  "The verdict the judge's TEXT begins with: `done', `blocked', or `continue'
also when it names none."
  (let ((case-fold-search t))
    (if (string-match "\\`[ \t\n*`#]*\\(done\\|blocked\\|continue\\)\\b" (or text ""))
        (intern (downcase (match-string 1 text)))
      'continue)))

(defun superchat-limit-judge (entries look-back model on-verdict)
  "Ask MODEL, apart from the run and with no tool, whether the task of ENTRIES,
a run's entries from its `user' entry on, is finished: its `user' entries are
the task, its calls the facts; LOOK-BACK is the run's own.  Call ON-VERDICT
with the verdict and the judge's text; a failed request is `continue'.
Return the request's handle."
  (superchat-llm-request
   (list (list :role 'user
               :text (format "Task:\n%s\n\nFacts of the run, counted from the record:\n%s\n\nThe agent's look-back:\n%s"
                             (mapconcat (lambda (e) (plist-get e :text))
                                        (seq-filter (lambda (e) (eq (plist-get e :kind) 'user)) entries)
                                        "\n\nThen the person added:\n")
                             (superchat-request-digest entries) look-back)))
   :system superchat-limit-judge-prompt :model model
   :on-done (lambda (response)
              (let ((text (string-trim (or (plist-get response :text) ""))))
                (funcall on-verdict (superchat-limit-verdict text) text)))
   :on-error (lambda (failure)
               (funcall on-verdict 'continue
                        (concat "The judge's request failed: " (plist-get failure :message))))))

(provide 'superchat-limit)
;;; superchat-limit.el ends here
