;;; synthesis-definition-regression.el --- a tool the agent wrote is a definition -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 8c (architecture map of 2026-10-13, contradictions C1 C2 C3 C4 C5
;; C7).  The decision: docs/decisions/accepted/
;; 2026-10-13-a-synthesized-tool-is-a-definition.md.
;;
;; 1. What is kept of a tool the agent wrote is its definition.  Every
;;    execution wraps it anew, so a tool defined in one execution runs in a
;;    later one.  (On 2026-10-11 such a tool answered "[Tool call blocked:
;;    Run is no longer active]" in a later execution, recorded as a success.)
;; 2. A definition belongs to the Workspace it was written in; one written in
;;    a temporary conversation belongs to that conversation and is not written
;;    to disk.  A sub-agent cannot take a name a conversation holds.
;; 3. The ledger says what happened: "approved" when a person was asked,
;;    "admitted" when the permission level let it through.  An admitted tool
;;    is not loaded again after a restart; `superchat-pending' lists it.
;; 4. The words the model and the user read say so.
;; 5. A call refused inside another call fails that call.
;;
;; The files of earlier versions say neither which Workspace a tool belongs
;; to nor whether a person looked.  The user's ruling (2026-10-13): none of
;; them is loaded unseen.  Each waits in `superchat-pending'; the review
;; screen approves it into a Workspace, rejects it, or deletes it.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/synthesis-definition-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^synthesis-definition/")'

;;; Code:

(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-synthesis)
(require 'superchat-pending)

(defvar superchat--syn-names)
(defvar superchat--syn-rehydrated)
(defvar superchat-llm-synthesized-tools)
(defvar superchat--tool-execution-context)
(defvar superchat-sandbox-default-mode)
(defvar superchat-approval-default-policy)
(declare-function superchat-synthesis-tools "superchat-synthesis" (&optional key))
(declare-function superchat-synthesis-definitions "superchat-synthesis" (&optional key))
(declare-function superchat-synthesis-forget-request "superchat-synthesis" (prompt))
(declare-function superchat-synthesis-awaiting-review "superchat-synthesis" ())
(declare-function superchat--syn-approval "superchat-synthesis" (hash))
(declare-function superchat-pending-open "superchat-pending" (item))
(declare-function superchat-tool-execute "superchat-tool-execution")
(declare-function superchat-permission-set "superchat-sandbox" (session-id name))

(defvar syndef--asked 0 "How many approval questions were put in this test.")

(defun syndef--reset ()
  "Forget every tool the agent wrote in this Emacs."
  (let ((inhibit-message t)) (superchat-synthesis-reset))
  (setq superchat--syn-rehydrated nil))

(defmacro syndef--with (script vars &rest body)
  "Run BODY in the Workspace fixture with SCRIPT; `define_tool' is offered.
The level is `edit-buffers' and every approval question is allowed and counted."
  (declare (indent 2) (debug t))
  `(wrec--with ,script ,vars
     (let ((superchat-llm-tool-names
            (append superchat-llm-default-tool-names '("define_tool")))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-approval-default-policy 'ask)
           (superchat-approval-function (lambda (_request) (cl-incf syndef--asked) t))
           (syndef--asked 0))
       (syndef--reset)
       (unwind-protect (progn ,@body)
         (syndef--reset)))))

(defun syndef--define (name body)
  "A model round that writes the tool NAME with BODY, taking no arguments."
  `(:tool-uses (("define_tool" (name . ,name) (description . ,(format "SYNDEF %s" name))
                 (args . "[]") (body . ,body)))))

(defun syndef--call (name) `(:tool-uses ((,name))))

(defun syndef--offered (&optional n)
  "The tool names of the Nth newest provider request (default the newest)."
  (plist-get (nth (or n 0) smoke--requests) :tools))

(defun syndef--tool-events (session name)
  "The (TYPE . PAYLOAD) of every finished call of the tool NAME in SESSION."
  (delq nil
        (mapcar (lambda (event)
                  (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
                       (equal name (plist-get (plist-get event :payload) :tool-name))
                       (cons (plist-get event :type) (plist-get event :payload))))
                (superchat-tape-replay-events session))))

(defun syndef--all-text ()
  "Everything sent to the provider in this test."
  (mapconcat (lambda (wire) (mapconcat #'hm--text wire "\n")) hm--wires "\n"))

(defun syndef--ledger ()
  "The ledger on disk as (NAME . STATE), in the order kept."
  (mapcar (lambda (entry) (cons (plist-get (cdr entry) :name)
                                (plist-get (cdr entry) :state)))
          (superchat--syn-load-approvals)))

(defun syndef--decisions ()
  "The (DECISION . ACTOR) of every `synthesis.approval.decided' on the Tape."
  (mapcar (lambda (event)
            (cons (format "%s" (plist-get (plist-get event :payload) :decision))
                  (plist-get event :actor-id)))
          (hm--events "synthesis" "synthesis.approval.decided")))

(defun syndef--stored-names ()
  (mapcar (lambda (entry) (plist-get entry :name)) (superchat--syn-store-entries)))

(defun syndef--restart ()
  "What a new Emacs does: nothing in memory, then the load from disk."
  (syndef--reset)
  (let ((inhibit-message t)) (superchat-synthesis-rehydrate)))

(defun syndef--held (name)
  "Non-nil when some conversation's toolbox holds a definition of NAME."
  (if (fboundp 'superchat-synthesis-definitions)
      (cl-some (lambda (definition) (equal name (plist-get definition :name)))
               (superchat-synthesis-definitions 'all))
    (cl-some (lambda (tool) (equal name (llm-tool-name tool)))
             superchat-llm-synthesized-tools)))

;;;; 1. A definition, wrapped anew by every execution ---------------------------

(ert-deftest synthesis-definition/01-a-tool-written-in-one-execution-runs-in-a-later-one ()
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")
                      (syndef--call "syn_echo") '(:text "Called."))
      (p)
    (let* ((view (wrec--view p)) (session (hm--session view)))
      (hm--turn view "write the tool")
      (hm--turn view "now use it")
      (let ((calls (syndef--tool-events session "syn_echo")))
        ;; The later execution was offered the tool and called it once.
        (should (member "syn_echo" (syndef--offered)))
        (should (= 1 (length calls)))
        (should (equal "tool.completed" (car (car calls))))
        ;; Its body ran: the model was given what the body returns.
        (should (string-match-p "SYN-RAN-OK" (syndef--all-text)))
        (should-not (string-match-p "Tool call blocked" (syndef--all-text)))))))

(ert-deftest synthesis-definition/02-what-is-kept-is-a-definition ()
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")) (p)
    (hm--turn (wrec--view p) "write the tool")
    (should (fboundp 'superchat-synthesis-definitions))
    (let ((kept (superchat-synthesis-definitions 'all)))
      (should (= 1 (length kept)))
      (should (equal "syn_echo" (plist-get (car kept) :name)))
      (should (equal "\"SYN-RAN-OK\"" (plist-get (car kept) :body)))
      ;; Nothing kept can be called: no function, no tool struct.
      (should-not (cl-some (lambda (value) (or (functionp value) (llm-tool-p value)))
                           (car kept))))))

;;;; 2. Whose it is -------------------------------------------------------------

(ert-deftest synthesis-definition/03-a-new-conversation-of-the-workspace-has-the-tool ()
  "Guard: green before and after."
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")
                      '(:text "Hello."))
      (p)
    (hm--turn (wrec--view p) "write the tool")
    (let ((second (with-current-buffer (get-file-buffer (expand-file-name "a.txt" p))
                    (superchat-new-session))))
      (hm--turn second "hello")
      (should (member "syn_echo" (syndef--offered))))))

(ert-deftest synthesis-definition/03b-a-request-from-elsewhere-for-the-conversation-has-the-tool ()
  "The toolbox is the conversation's the request names, wherever the request
is made from."
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")
                      '(:text "Hello."))
      (p)
    (let* ((view (wrec--view p)) (session (hm--session view)) (done nil))
      (hm--turn view "write the tool")
      (with-temp-buffer
        (superchat-service-call
         nil 'driver 'submit
         (list :surface 'external :content "hello" :session-id session
               :context (list :host-buffer (current-buffer))
               :runtime-extras '(:record-conversation nil)
               :sink (superchat-sink-callback
                      :final (lambda (&rest _) (setq done t))
                      :error (lambda (&rest _) (setq done 'error)))))
        (should (smoke--wait (lambda () done) 20)))
      (should (eq t done))
      (should (member "read_buffer" (syndef--offered)))
      (should (member "syn_echo" (syndef--offered))))))

(ert-deftest synthesis-definition/04-another-workspace-does-not-have-the-tool ()
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")
                      '(:text "Hello."))
      (p q)
    (hm--turn (wrec--view p) "write the tool")
    (hm--turn (wrec--view q) "hello")
    ;; Guard: the other Workspace is offered tools at all.
    (should (member "read_buffer" (syndef--offered)))
    (should-not (member "syn_echo" (syndef--offered)))))

(ert-deftest synthesis-definition/04b-two-workspaces-each-run-their-own-tool-of-one-name ()
  (syndef--with (list (syndef--define "syn_same" "\"BODY-OF-P\"") '(:text "Defined.")
                      (syndef--define "syn_same" "\"BODY-OF-Q\"") '(:text "Defined.")
                      (syndef--call "syn_same") '(:text "Called."))
      (p q)
    (let ((in-p (wrec--view p)) (in-q (wrec--view q)))
      (hm--turn in-p "write the tool")
      (hm--turn in-q "write the tool")
      (should (= 2 (length (superchat-synthesis-definitions 'all))))
      (setq hm--wires nil)
      ;; P calls its tool after Q wrote one of the same name.
      (hm--turn in-p "use it")
      (should (string-match-p "BODY-OF-P" (syndef--all-text)))
      (should-not (string-match-p "BODY-OF-Q" (syndef--all-text)))
      ;; Both are on disk, each under its own Workspace.
      (should (equal '("syn_same" "syn_same") (syndef--stored-names)))
      (should (= 2 (length (delete-dups
                            (mapcar (lambda (entry) (plist-get entry :workspace-id))
                                    (superchat--syn-store-entries)))))))))

(ert-deftest synthesis-definition/05-a-temporary-conversations-tool-stays-in-it ()
  (syndef--with (list (syndef--define "syn_temp" "\"SYN-TEMP\"") '(:text "Defined.")
                      (syndef--call "syn_temp") '(:text "Called.")
                      '(:text "Hello."))
      (p)
    (let* ((view (smoke--open-chat)) (session (hm--session view)))
      ;; Guard: this conversation belongs to no Workspace.
      (should-not (plist-get (superchat-view-session-configuration session) :workspace-id))
      (hm--turn view "write the tool")
      ;; It works in the conversation it was written in ...
      (hm--turn view "use it")
      (should (equal "tool.completed" (car (car (syndef--tool-events session "syn_temp")))))
      ;; ... is never written to disk ...
      (should-not (member "syn_temp" (syndef--stored-names)))
      (should-not (assoc "syn_temp" (syndef--ledger)))
      ;; ... and no Workspace's conversation is offered it.
      (hm--turn (wrec--view p) "hello")
      (should (member "read_buffer" (syndef--offered)))
      (should-not (member "syn_temp" (syndef--offered))))))

(defun syndef--direct (persist name body &optional context prompt)
  "Write the tool NAME with BODY outside any execution; return what the model reads.
CONTEXT stands for the execution the call is part of."
  (let ((superchat--tool-execution-context
         (or context '(:session-id "syndef-direct" :workspace-id "syndef-ws"
                       :admitted-by asked))))
    (superchat--syn-define prompt nil persist name (format "SYNDEF %s" name) "[]" body)))

(defun syndef--run-held (name)
  "Run the definition of NAME the Workspace of `syndef--direct' holds."
  (let ((tool (cl-find name
                       (if (fboundp 'superchat-synthesis-tools)
                           (superchat-synthesis-tools "syndef-ws")
                         superchat-llm-synthesized-tools)
                       :key #'llm-tool-name :test #'equal)))
    (should tool)
    (funcall (llm-tool-function tool))))

(ert-deftest synthesis-definition/06-a-sub-agent-cannot-take-a-name-that-is-held ()
  (syndef--with nil (p)
    (should (string-match-p "is defined" (syndef--direct t "syn_shared" "\"OLD-BODY\"")))
    (should (equal "OLD-BODY" (syndef--run-held "syn_shared")))
    ;; A sub-agent writes into its own request only (PERSIST nil).
    (let ((answer (syndef--direct nil "syn_shared" "\"NEW-BODY\""
                                  '(:session-id "syndef-child" :workspace-id "syndef-ws"))))
      (should (string-match-p "\\`Error" answer))
      (should (string-match-p "syn_shared" answer)))
    ;; The conversation's tool still runs what the conversation wrote.
    (should (equal "OLD-BODY" (syndef--run-held "syn_shared")))))

(ert-deftest synthesis-definition/07-a-sub-agents-own-tool-ends-with-its-request ()
  (syndef--with nil (p)
    (let ((prompt (llm-make-chat-prompt "syndef child")))
      (should (string-match-p "is defined"
                              (syndef--direct nil "syn_child" "\"CHILD\""
                                              '(:session-id "syndef-child" :workspace-id "syndef-ws")
                                              prompt)))
      ;; While the request lives the tool is in it and is known as agent-written.
      (should (member "syn_child" (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))))
      (should (superchat--syn-tool-p "syn_child"))
      (should-not (syndef--held "syn_child"))
      ;; A sub-agent may write its own tool again.
      (should (string-match-p "is defined"
                              (syndef--direct nil "syn_child" "\"CHILD-2\""
                                              '(:session-id "syndef-child" :workspace-id "syndef-ws")
                                              prompt)))
      (when (fboundp 'superchat-synthesis-forget-request)
        (superchat-synthesis-forget-request prompt))
      (should-not (superchat--syn-tool-p "syn_child"))
      (should-not (member "syn_child" (syndef--stored-names))))))

(ert-deftest synthesis-definition/07b-the-end-of-a-request-forgets-what-was-only-its ()
  "The request loop itself forgets: a real request that does not keep its tools."
  (syndef--with (list (syndef--define "syn_gone" "\"GONE\"") '(:text "Defined.")) (p)
    (let* ((view (wrec--view p)) (done nil))
      (with-current-buffer view
        (superchat-service-call
         nil 'driver 'submit
         (list :surface 'external :content "write the tool"
               :session-id (hm--session view)
               :context (list :host-buffer view)
               :runtime-extras '(:persist-synthesized-tools nil :record-conversation nil)
               :sink (superchat-sink-callback
                      :final (lambda (&rest _) (setq done t))
                      :error (lambda (&rest _) (setq done 'error))))))
      (should (smoke--wait (lambda () done) 20))
      (should (eq t done))
      ;; Guard: the tool was written in that request.
      (should (string-match-p "syn_gone. is defined" (syndef--all-text)))
      (should-not (syndef--held "syn_gone"))
      (should-not (superchat--syn-tool-p "syn_gone")))))

;;;; 3. The ledger says what happened ----------------------------------------------

(ert-deftest synthesis-definition/08-asked-and-allowed-is-approved ()
  "Guard: green before and after."
  (syndef--with (list (syndef--define "syn_seen" "\"SEEN\"") '(:text "Defined.")) (p)
    (hm--turn (wrec--view p) "write the tool")
    (should (= 1 syndef--asked))
    (should (equal '(("syn_seen" . approved)) (syndef--ledger)))
    (should (equal '(("approved" . "user")) (syndef--decisions)))))

(defun syndef--unasked (level)
  "Write a tool under LEVEL, which asks nobody; assert what the ledger says."
  (syndef--with (list (syndef--define "syn_unseen" "\"UNSEEN\"") '(:text "Defined.")) (p)
    (let* ((view (wrec--view p)) (session (hm--session view)))
      (superchat-permission-set session level)
      (hm--turn view "write the tool")
      ;; Guard: nobody was asked, and the tool was written.
      (should (= 0 syndef--asked))
      (should (syndef--held "syn_unseen"))
      (should (equal '(("syn_unseen" . admitted)) (syndef--ledger)))
      (let ((decisions (syndef--decisions)))
        (should (= 1 (length decisions)))
        (should (equal "admitted" (car (car decisions))))
        (should-not (equal "user" (cdr (car decisions))))))))

(ert-deftest synthesis-definition/09-let-through-by-full-access-is-admitted ()
  (syndef--unasked "danger-full-access"))

(ert-deftest synthesis-definition/10-let-through-by-auto-is-admitted ()
  (syndef--unasked "auto"))

(defun syndef--pending-tools ()
  "The names of the agent-written tools `superchat-pending' lists."
  (delq nil (mapcar (lambda (item)
                      (and (eq 'unreviewed (plist-get item :kind))
                           (plist-get item :what)))
                    (superchat-pending-items))))

(ert-deftest synthesis-definition/11-an-admitted-tool-is-not-loaded-again ()
  (syndef--with (list (syndef--define "syn_unseen" "\"UNSEEN\"") '(:text "Defined.")
                      '(:text "Hello."))
      (p)
    (let* ((view (wrec--view p)) (session (hm--session view)))
      (superchat-permission-set session "danger-full-access")
      (hm--turn view "write the tool")
      (should (member "syn_unseen" (syndef--stored-names)))
      (syndef--restart)
      (should-not (syndef--held "syn_unseen"))
      (hm--turn view "hello")
      (should-not (member "syn_unseen" (syndef--offered)))
      ;; It is listed, not asked about.
      (should (equal '("syn_unseen") (syndef--pending-tools)))
      (should (= 0 syndef--asked)))))

(ert-deftest synthesis-definition/12-an-approved-tool-is-loaded-again-into-its-workspace ()
  (syndef--with (list (syndef--define "syn_seen" "\"SEEN-AGAIN\"") '(:text "Defined.")
                      (syndef--call "syn_seen") '(:text "Called.")
                      '(:text "Hello."))
      (p q)
    (let* ((view (wrec--view p)) (session (hm--session view)))
      (hm--turn view "write the tool")
      (syndef--restart)
      (should (syndef--held "syn_seen"))
      (should-not (syndef--pending-tools))
      (hm--turn view "use it")
      (should (equal "tool.completed" (car (car (syndef--tool-events session "syn_seen")))))
      (should (string-match-p "SEEN-AGAIN" (syndef--all-text)))
      ;; Loaded into the Workspace it was written in, not into every one.
      (hm--turn (wrec--view q) "hello")
      (should-not (member "syn_seen" (syndef--offered))))))

;;;; 3b. The files of earlier versions --------------------------------------------

(defun syndef--legacy-files ()
  "Write a store and a ledger as versions before this slice wrote them."
  (let* ((body "\"LEGACY\"")
         (hash (superchat--syn-hash "syn_legacy" "SYNDEF legacy" nil body)))
    (with-temp-file (superchat--syn-store-file)
      (pp (list :name "syn_legacy" :description "SYNDEF legacy" :args nil :body body
                :updated "2026-10-01T00:00:00+0000")
          (current-buffer)))
    (with-temp-file (superchat--syn-approvals-file)
      (pp (list (cons hash (list :name "syn_legacy" :state 'approved
                                 :at "2026-10-01T00:00:00+0000")))
          (current-buffer)))))

(ert-deftest synthesis-definition/13-nothing-old-is-loaded-unseen ()
  "An old \"approved\" does not say a person looked: it waits to be looked at."
  (syndef--with (list '(:text "Hello.")) (p)
    (let ((before nil))
      (syndef--legacy-files)
      (setq before (list (with-temp-buffer (insert-file-contents (superchat--syn-store-file))
                                           (buffer-string))
                         (with-temp-buffer (insert-file-contents (superchat--syn-approvals-file))
                                           (buffer-string))))
      (syndef--restart)
      (should-not (syndef--held "syn_legacy"))
      (should (equal '("syn_legacy") (syndef--pending-tools)))
      (hm--turn (wrec--view p) "hello")
      (should-not (member "syn_legacy" (syndef--offered)))
      ;; Loading reads; the user's files are as they were.
      (should (equal before
                     (list (with-temp-buffer (insert-file-contents (superchat--syn-store-file))
                                             (buffer-string))
                           (with-temp-buffer (insert-file-contents (superchat--syn-approvals-file))
                                             (buffer-string))))))))

;; The review screen is the way out of the list (the user, 2026-10-13: "the
;; user wants to clean tools up as needed").

(defun syndef--file (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun syndef--review-row (answer)
  "Press RET on the pending row of the old tool and answer ANSWER (a character)."
  (let ((item (cl-find 'unreviewed (superchat-pending-items)
                       :key (lambda (item) (plist-get item :kind))))
        (inhibit-message t))
    (should item)
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) answer)))
      (save-window-excursion (superchat-pending-open item)))))

(defun syndef--legacy-hash ()
  (superchat--syn-hash "syn_legacy" "SYNDEF legacy" nil "\"LEGACY\""))

(ert-deftest synthesis-definition/13b-an-old-tool-is-approved-into-a-workspace ()
  (syndef--with (list (syndef--call "syn_legacy") '(:text "Called.") '(:text "Hello.")) (p q)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (workspace (plist-get (superchat-view-session-configuration session)
                                 :workspace-id)))
      (should (stringp workspace))
      (syndef--legacy-files)
      (syndef--restart)
      ;; Reviewed from a conversation of P: P is where it goes.
      (with-current-buffer view (syndef--review-row ?a))
      (should-not (syndef--pending-tools))
      (should (equal (list workspace)
                     (mapcar (lambda (entry) (plist-get entry :workspace-id))
                             (superchat--syn-store-entries))))
      (let ((line (superchat--syn-approval (syndef--legacy-hash))))
        (should (eq 'approved (plist-get line :state)))
        (should (eq 'review (plist-get line :how))))
      (should (equal '("approved" . "user") (car (last (syndef--decisions)))))
      ;; It runs in P, it is not offered in Q, and it is loaded again later.
      (hm--turn view "use it")
      (should (equal "tool.completed" (car (car (syndef--tool-events session "syn_legacy")))))
      (hm--turn (wrec--view q) "hello")
      (should-not (member "syn_legacy" (syndef--offered)))
      (syndef--restart)
      (should (syndef--held "syn_legacy")))))

(ert-deftest synthesis-definition/13c-an-old-tool-is-deleted-and-nothing-else ()
  (syndef--with nil (p)
    (syndef--legacy-files)
    ;; A second old tool, and a file that is nobody's business.
    (let ((other (list :name "syn_other" :description "SYNDEF other" :args nil
                       :body "\"OTHER\"" :updated "2026-10-01T00:00:00+0000"))
          (bystander (expand-file-name "memory.org" superchat-data-directory)))
      (with-temp-buffer
        (pp other (current-buffer))
        (append-to-file (point-min) (point-max) (superchat--syn-store-file)))
      (with-temp-file bystander (insert "BYSTANDER\n"))
      (syndef--restart)
      (should (equal '("syn_legacy" "syn_other") (syndef--pending-tools)))
      (let ((files (directory-files superchat-data-directory nil "\\`[^.]")))
        (syndef--review-row ?d)
        ;; Only that tool left the list and the store.
        (should (equal '("syn_other") (syndef--pending-tools)))
        (should (equal '("syn_other") (syndef--stored-names)))
        ;; The ledger line is marked, not removed.
        (should (eq 'deleted (plist-get (superchat--syn-approval (syndef--legacy-hash))
                                        :state)))
        (should-not (syndef--held "syn_legacy"))
        ;; Nothing else on disk changed.
        (should (equal files (directory-files superchat-data-directory nil "\\`[^.]")))
        (should (equal "BYSTANDER\n" (syndef--file bystander))))
      ;; A deleted tool does not come back.
      (syndef--restart)
      (should-not (syndef--held "syn_legacy"))
      (should (equal '("syn_other") (syndef--pending-tools)))
      ;; Not even when the very same definition turns up in the store again
      ;; (a copy of the file from another machine): the ledger remembers.
      (with-temp-buffer
        (pp (list :name "syn_legacy" :description "SYNDEF legacy" :args nil
                  :body "\"LEGACY\"")
            (current-buffer))
        (append-to-file (point-min) (point-max) (superchat--syn-store-file)))
      (syndef--restart)
      (should-not (syndef--held "syn_legacy"))
      (should (equal '("syn_other") (syndef--pending-tools))))))

(ert-deftest synthesis-definition/13f-a-workspace-alone-does-not-make-an-old-approval-count ()
  "An entry that names a Workspace, under a ledger line that does not say
how it was approved: still nobody is known to have looked."
  (syndef--with nil (p)
    (let* ((body "\"HALF\"")
           (hash (superchat--syn-hash "syn_half" "SYNDEF half" nil body)))
      (with-temp-file (superchat--syn-store-file)
        (pp (list :name "syn_half" :description "SYNDEF half" :args nil :body body
                  :workspace-id "workspace-0123456789ab")
            (current-buffer)))
      (with-temp-file (superchat--syn-approvals-file)
        (pp (list (cons hash (list :name "syn_half" :state 'approved))) (current-buffer)))
      (syndef--restart)
      (should-not (syndef--held "syn_half"))
      (should (equal '("syn_half") (syndef--pending-tools))))))

(ert-deftest synthesis-definition/13d-an-old-tool-is-rejected-and-kept-out ()
  (syndef--with nil (p)
    (syndef--legacy-files)
    (syndef--restart)
    (let ((store (syndef--file (superchat--syn-store-file))))
      (syndef--review-row ?r)
      (should-not (syndef--pending-tools))
      (should (eq 'rejected (plist-get (superchat--syn-approval (syndef--legacy-hash))
                                       :state)))
      ;; Rejecting keeps the entry in the store, for the user to read.
      (should (equal store (syndef--file (superchat--syn-store-file))))
      (syndef--restart)
      (should-not (syndef--held "syn_legacy")))))

(ert-deftest synthesis-definition/13e-skipping-leaves-everything-as-it-was ()
  (syndef--with nil (p)
    (syndef--legacy-files)
    (syndef--restart)
    (let ((store (syndef--file (superchat--syn-store-file)))
          (ledger (syndef--file (superchat--syn-approvals-file))))
      (syndef--review-row ?s)
      (should (equal '("syn_legacy") (syndef--pending-tools)))
      (should (equal store (syndef--file (superchat--syn-store-file))))
      (should (equal ledger (syndef--file (superchat--syn-approvals-file)))))))

;;;; 5. A call refused inside another call fails it ------------------------------

(ert-deftest synthesis-definition/14-a-call-refused-inside-a-call-fails-that-call ()
  "The shape of 2026-10-11, built by hand: a callable that still carries an
execution that has ended, called inside a live one."
  (syndef--with (list (syndef--define "syn_outer" "(syndef--stale-call)") '(:text "Defined.")
                      (syndef--call "syn_outer") '(:text "Called."))
      (p)
    (let* ((view (wrec--view p)) (session (hm--session view))
           (ran 0))
      (cl-letf (((symbol-function 'syndef--stale-call)
                 (lambda ()
                   (superchat-tool-execute
                    "syn_inner" (lambda () (cl-incf ran) "INNER-RAN") nil
                    :context (list :session-id session :run-id "run-that-ended"
                                   :turn-id "turn-that-ended"
                                   :run-active-p #'ignore)))))
        (hm--turn view "write the tool")
        (hm--turn view "use it"))
      (let ((calls (syndef--tool-events session "syn_outer")))
        ;; Guard: the inner body never ran.
        (should (= 0 ran))
        (should (= 1 (length calls)))
        (should (equal "tool.failed" (car (car calls))))
        ;; The model is told why.
        (should (string-match-p "Tool call failed:.*Run is no longer active"
                                (syndef--all-text)))))))

;;;; 4. The words ------------------------------------------------------------------

(ert-deftest synthesis-definition/15-nothing-says-every-call-asks ()
  (syndef--with (list (syndef--define "syn_echo" "\"SYN-RAN-OK\"") '(:text "Defined.")) (p)
    (hm--turn (wrec--view p) "write the tool")
    (let ((case-fold-search t)
          (tool (cl-find "define_tool" (superchat-get-llm-tools)
                         :key #'llm-tool-name :test #'equal)))
      (should tool)
      (dolist (text (list (llm-tool-description tool)
                          ;; What the model was told when the tool was written.
                          (syndef--all-text)))
        (should-not (string-match-p "every call \\(asks\\|will ask\\|still asks\\)" text)))
      (should (string-match-p "permission level" (llm-tool-description tool)))
      (should (string-match-p "syn_echo. is defined.*permission level" (syndef--all-text))))))

(ert-deftest synthesis-definition/16-a-built-in-name-cannot-be-taken ()
  "Guard: green before and after."
  (syndef--with nil (p)
    (let ((answer (syndef--direct t "read_buffer" "\"X\"")))
      (should (string-match-p "already a built-in tool" answer)))
    (should-not (syndef--held "read_buffer"))
    (should (string-match-p "invalid tool name" (syndef--direct t "bad name" "\"X\"")))
    (should (string-match-p "not valid Emacs Lisp" (syndef--direct t "syn_bad" "(")))))

(provide 'synthesis-definition-regression)
;;; synthesis-definition-regression.el ends here
