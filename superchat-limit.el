;;; superchat-limit.el --- What a run says and decides at the round limit -*- lexical-binding: t; -*-

;;; Commentary:
;; A run that reaches `superchat-max-rounds' asks the model to look back,
;; then pauses or goes on.  This file holds the words of that request, reads
;; the status a look-back begins with, and counts what a paused run did.
;; Pure functions over a run's entries: the loop itself stays in
;; superchat-agent.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-request)

(defconst superchat-limit-note
  "The limit of model responses in one run is reached. Call no tool now, even if one would help. Begin with one line: \"Status: finished\" when the task is done (what follows is then your answer to the person), \"Status: blocked\" when you need the person, or \"Status: continue\" when there is more to do. Then look back over the calls of this run, with the facts below: what is done, what failed and why, what is left, the plan for the next step, and what the person should check."
  "What the system prompt says of a run's last request, the round limit reached.")

(defconst superchat-limit-resume-note
  "You reached the limit of model responses and looked back above. Go on: follow your plan, and change the approach where your look-back or the facts below show what went wrong."
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

(provide 'superchat-limit)
;;; superchat-limit.el ends here
