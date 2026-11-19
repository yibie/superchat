;;; superchat-request.el --- The messages of a request -*- lexical-binding: t; -*-

;;; Commentary:
;; What a request sends is built here from the entries of the `conversation'
;; view: each entry's message, the groups a call keeps with its results, the
;; budget, and what is left out to fit.  Pure functions of entries and a
;; budget; the run and its state are `superchat-agent.el's.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-llm)

(defun superchat-request-message (entry)
  "The neutral message of ENTRY, or nil when the model is told nothing."
  (let ((text (plist-get entry :text))
        (data (plist-get entry :data)))
    (pcase (plist-get entry :kind)
      ('user (list :role 'user :text text))
      ('assistant (list :role 'assistant :text text :calls (plist-get data :calls)))
      ('tool (list :role 'tool :text text :call-id (plist-get data :call-id)
                   :name (plist-get data :name)))
      ('anchor (unless (string-empty-p text)
                 (list :role 'user
                       :text (concat "Summary of the conversation so far:\n" text
                                     (if-let* ((next (plist-get data :next))
                                               ((not (string-empty-p next))))
                                         (concat "\n\nWhat comes next:\n" next)
                                       ""))))))))

(defun superchat-request-budget (&optional model)
  "The budget in characters of a run of MODEL (default `superchat-llm-model').
`superchat-context-chars' when a number; else half the model's context
window, a token counting as a character; else 120000."
  (cond ((numberp superchat-context-chars) superchat-context-chars)
        ((when-let* ((window (superchat-llm-context-window model))) (floor window 2)))
        (t 120000)))

(defun superchat-request-messages (entries)
  "The messages for ENTRIES, as a request sends them when nothing is left out."
  (cl-loop for g in (superchat-request--grouped entries) append (cadr g)))

(defun superchat-request-size (messages)
  "The size of MESSAGES: their texts and the printed arguments of their calls.
The handoff check and the budget of a request both use this."
  (cl-loop for m in messages
           sum (+ (length (plist-get m :text))
                  (cl-loop for c in (plist-get m :calls)
                           sum (length (format "%S" (plist-get c :args)))))))

(defun superchat-request--groups (entries)
  "ENTRIES in lists: an assistant entry with the tool entries that answer its
calls and every entry that gives no message up to the next entry of another
kind, and every other entry alone.  A tool entry that answers no
call of the assistant entry heading its group is in no group: no request
ever has it."
  (let (groups)
    (dolist (e entries)
      (let* ((kind (plist-get e :kind)) (head (car (last (car groups)))))
        (cond ((not (and (or (eq kind 'tool) (null (superchat-request-message e))) (eq (plist-get head :kind) 'assistant)))
               (unless (eq kind 'tool) (push (list e) groups)))
              ((or (not (eq kind 'tool))
                   (cl-some (lambda (c) (equal (plist-get c :id)
                                               (plist-get (plist-get e :data) :call-id)))
                            (plist-get (plist-get head :data) :calls)))
               (push e (car groups))))))
    (nreverse (mapcar #'nreverse groups))))

(defun superchat-request--answered (group)
  "The messages of GROUP.  A call its tool entries do not answer (Emacs stopped
while it ran) is not sent, nor an assistant message left with nothing."
  (let* ((ids (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id)) (cdr group)))
         (messages (delq nil (mapcar #'superchat-request-message group)))
         (head (car messages)))
    (if (not (eq (plist-get head :role) 'assistant))
        messages
      (let ((calls (cl-remove-if-not (lambda (c) (member (plist-get c :id) ids))
                                     (plist-get head :calls))))
        (if (and (null calls) (string-empty-p (plist-get head :text)))
            (cdr messages)
          (cons (plist-put (copy-sequence head) :calls calls) (cdr messages)))))))

(defun superchat-request--grouped (entries)
  "The groups of ENTRIES, each as (ENTRIES MESSAGES)."
  (mapcar (lambda (g) (list g (superchat-request--answered g)))
          (superchat-request--groups entries)))

(defun superchat-request--never-left-out (groups user-id)
  "The members of GROUPS, from `superchat-request--grouped', that a request
always keeps: those with an anchor or the entry USER-ID, and the latest
assistant entry with its results that a request sends at all."
  (let ((latest (cl-find-if (lambda (g) (eq (plist-get (car (cadr g)) :role) 'assistant))
                            groups :from-end t)))
    (cl-remove-if-not
     (lambda (g) (or (eq g latest)
                     (cl-some (lambda (e) (or (eq (plist-get e :kind) 'anchor)
                                              (eql (plist-get e :id) user-id)))
                              (car g))))
     groups)))

(defun superchat-request--standin (n)
  "The message that stands for N entries left out of a request."
  (list :role 'user
        :text (format (concat "[%d earlier entries of this conversation are left out of "
                             "this request; recall and tape_query can read them.]")
                       n)))

(defun superchat-request-fit (entries user-id budget)
  "The messages for ENTRIES within BUDGET.
The oldest groups go first, except an anchor, the entry USER-ID and the latest
assistant entry with its results.  One stand-in message takes the place of the
oldest group left out, so every call stays with its results and the list
still starts with what the first entry said.  The stand-in counts toward the
size; when leaving out what can be left out would not make the request
smaller than it is, nothing is left out."
  (let* ((groups (superchat-request--grouped entries))
         (kept (superchat-request--never-left-out groups user-id))
         (whole (cl-loop for g in groups sum (superchat-request-size (cadr g))))
         (size whole) (dropped 0) gone)
    (cl-flet ((total ()
                (+ size (if (> dropped 0)
                            (superchat-request-size (list (superchat-request--standin dropped)))
                          0))))
      (dolist (g groups)
        (when (and (cadr g) (not (memq g kept)) (> (total) budget))
          (cl-decf size (superchat-request-size (cadr g)))
          (cl-incf dropped (length (car g)))
          (push g gone)))
      (if (>= (total) whole)
          (cl-loop for g in groups append (cadr g))
        (cl-loop for g in groups
                 if (not (memq g gone)) append (cadr g)
                 else if (eq g (car (last gone))) collect (superchat-request--standin dropped))))))

(defun superchat-request-kept-size (entries user-id)
  "The size of what a request of ENTRIES never leaves out, USER-ID the entry
of the run: what the refusal measures against twice the budget."
  (superchat-request-size
   (cl-loop for g in (superchat-request--never-left-out (superchat-request--grouped entries) user-id)
            append (cadr g))))

(defun superchat-request-digest (entries)
  "Facts of the calls among ENTRIES, counted, for a run that looks back: how
many calls each tool had and how many failed, the files changed most, the
calls made more than once, the last failures.  Lines of text."
  (let ((tools nil) (files nil) (same nil) (failures nil))
    (dolist (e entries)
      (pcase (plist-get e :kind)
        ('assistant
         (dolist (c (plist-get (plist-get e :data) :calls))
           (let ((path (plist-get (plist-get c :args) :path)))
             (when (and path (member (plist-get c :name) '("edit" "write")))
               (cl-incf (alist-get path files 0 nil #'equal))))
           (cl-incf (alist-get (format "%s %s" (plist-get c :name)
                                       (truncate-string-to-width
                                        (format "%S" (plist-get c :args)) 80 nil nil "…"))
                               same 0 nil #'equal))))
        ('tool
         (let* ((data (plist-get e :data)) (name (plist-get data :name))
                (count (or (assoc name tools) (car (push (list name 0 0) tools)))))
           (cl-incf (nth 1 count))
           (when (plist-get data :error)
             (cl-incf (nth 2 count))
             (push (format "%s: %s" name (truncate-string-to-width
                                          (car (split-string (plist-get e :text) "\n")) 120 nil nil "…"))
                   failures))))))
    (cl-flet ((top (alist n) (take n (sort alist (lambda (a b) (> (cdr a) (cdr b)))))))
      (string-join
       (delq nil
             (list (if tools
                       (concat "Calls: " (mapconcat (pcase-lambda (`(,name ,n ,bad))
                                                      (if (> bad 0) (format "%s %d (%d failed)" name n bad)
                                                        (format "%s %d" name n)))
                                                    (reverse tools) ", ") ".")
                     "No tool calls.")
                   (and files (concat "Files changed: "
                                      (mapconcat (lambda (f) (format "%s %d times" (car f) (cdr f)))
                                                 (top files 5) ", ") "."))
                   (when-let* ((again (seq-filter (lambda (c) (> (cdr c) 1)) same)))
                     (concat "Same call made more than once: "
                             (mapconcat (lambda (c) (format "%s, %d times" (car c) (cdr c)))
                                        (top again 3) "; ") "."))
                   (and failures (concat "Last failures:\n"
                                         (mapconcat (lambda (f) (concat "- " f))
                                                    (reverse (take 3 failures)) "\n")))))
       "\n"))))

(provide 'superchat-request)
;;; superchat-request.el ends here
