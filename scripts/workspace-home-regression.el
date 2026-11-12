;;; workspace-home-regression.el --- Home, the Workspace with no root -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for the decision of 2026-11-10, "a Workspace is a record,
;; not a directory" (docs/decisions/accepted/
;; 2026-11-10-a-workspace-is-a-record-not-a-directory.md): every buffer in no
;; project belongs to Home, a Workspace with an id, a name and a kind but no
;; root; Home's conversations are kept per source; Home keeps its own rules,
;; work records and the user's timers; nothing said in one source reaches
;; another; and conversations already on the Tape still open.
;;
;; Telega and elfeed are not loaded here: two stand-in modes carry the
;; buffer-local variables a key function reads, and the real key functions
;; are tested directly with those variables set by hand.  No real model:
;; every request goes to the scripted network of `scripts/smoke.el'.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-home-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-home/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "work-record-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-work)
(require 'superchat-timer)
(require 'superchat-watch)
(require 'superchat-inline)
(require 'superchat-rewrite)
(require 'superchat-changes)
(require 'superchat-butler)

(defvar superchat-work-rounds-when-started)
(defvar superchat-changes-review-after-run)
(defvar superchat-source-key-functions)
(defvar superchat--tool-execution-context)

;;;; Stand-in modes ---------------------------------------------------------------

(defvar-local wh-chat-id nil "Stand-in for a chat buffer's chat id.")
(defvar-local wh-topic-id nil "Stand-in for a chat buffer's topic id.")
(defvar-local wh-entry-id nil "Stand-in for a feed entry buffer's entry id.")

(define-derived-mode wh-chat-mode text-mode "WH-Chat" "Stands in for a chat mode.")
(define-derived-mode wh-entry-mode special-mode "WH-Entry"
  "Stands in for a feed entry mode: every buffer has the same name.")
(define-derived-mode wh-other-mode special-mode "WH-Other"
  "A mode with no key function: its buffers are keyed by name.")

(defun wh--chat-key ()
  (when wh-chat-id
    (concat (format "whchat:chat:%s" wh-chat-id)
            (if wh-topic-id (format ":topic:%s" wh-topic-id) ""))))

(defun wh--entry-key ()
  (when wh-entry-id (format "whentry:entry:%s" wh-entry-id)))

;;;; Fixture ----------------------------------------------------------------------

(defvar wh--dir nil "A directory in no project: the sources' `default-directory'.")
(defvar wh--now 1800000000.0)
(defvar wh--told nil)

(defmacro wh--with (script vars &rest body)
  "Run BODY with SCRIPT in the Work Record fixture; VARS are project roots.
Sources live in `wh--dir', a directory no project claims."
  (declare (indent 2) (debug t))
  `(let ((superchat-work-rounds-when-started nil)
         (superchat-changes-review-after-run nil)
         (superchat-personal-workspace-directory nil)
         (superchat-personal-workspace-modes
          (append '(wh-chat-mode wh-entry-mode) superchat-personal-workspace-modes))
         (superchat-source-key-functions
          (append (list (cons 'wh-chat-mode #'wh--chat-key)
                        (cons 'wh-entry-mode #'wh--entry-key))
                  (and (boundp 'superchat-source-key-functions)
                       superchat-source-key-functions)))
         (hm--extra-tool-names '("memory_search" "tool_history" "timer_list"
                                 "open_file" "shell-command"
                                 "delegate_to_subagent"))
         (wh--now 1800000000.0)
         (wh--told nil)
         (superchat-timer-now-function (lambda () wh--now))
         (superchat-timer-notify-function (lambda (fired) (push fired wh--told))))
     (clrhash superchat-timer--retries)
     (clrhash superchat-timer--not-before)
     (wrec--with ,script ,vars
       (wrec--with-clock
         (setq wrec--now wh--now)
         (let ((wh--dir (file-name-as-directory
                         (file-truename (make-temp-file "wh-src-" t)))))
           (unwind-protect (progn ,@body)
             (superchat-timer-disarm-all)
             (dolist (b (buffer-list))
               (when (or (string-prefix-p "*wh-" (buffer-name b))
                         (and (buffer-file-name b)
                              (string-prefix-p wh--dir (buffer-file-name b))))
                 (let ((kill-buffer-query-functions nil))
                   (with-current-buffer b (set-buffer-modified-p nil))
                   (kill-buffer b))))
             (ignore-errors (delete-directory wh--dir t))))))))

(defun wh--src (name mode &rest vars)
  "A live buffer NAME in MODE in `wh--dir', with buffer-local VARS (SYM VAL ...)."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (funcall mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "WH source line one\nWH source line two\n"))
      (goto-char (point-min))
      (setq default-directory wh--dir)
      (while vars (set (make-local-variable (pop vars)) (pop vars))))
    buffer))

(defun wh--chat (n &optional topic name)
  (wh--src (or name (format "*wh-chat-%s%s*" n (if topic (format "-t%s" topic) "")))
           #'wh-chat-mode 'wh-chat-id n 'wh-topic-id topic))

(defun wh--entry (n) (wh--src "*wh-entry*" #'wh-entry-mode 'wh-entry-id n))
(defun wh--other (&optional name) (wh--src (or name "*wh-other*") #'wh-other-mode))

(defun wh--loose-file (&optional name)
  "A buffer visiting a file in `wh--dir', which no project claims."
  (let ((file (expand-file-name (or name "loose.txt") wh--dir)))
    (unless (file-exists-p file)
      (with-temp-file file (insert "loose line one\nloose line two\n")))
    (find-file-noselect file)))

(defun wh--open (src) (with-current-buffer src (superchat)))
(defun wh--sid (view) (and (buffer-live-p view) (hm--session view)))
(defun wh--config (session) (superchat-view-session-configuration session))

(defun wh--home-id ()
  (string-trim (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "home/id" superchat-data-directory))
                 (buffer-string))))

(defun wh--turn (view text script)
  "Send TEXT from VIEW with SCRIPT; return this turn's wires, oldest first."
  (setq smoke--script (copy-sequence script))
  (let ((before (length hm--wires)))
    (hm--turn view text)
    (wrec--settled)
    (reverse (cl-subseq hm--wires 0 (- (length hm--wires) before)))))

(defun wh--msg-text (m)
  (concat (hm--text m) " "
          (mapconcat (lambda (c) (or (hm--call-arguments c) "")) (hm--calls m) " ")))

(defun wh--wire-text (wire) (mapconcat #'wh--msg-text wire "\n"))

(defun wh--system-text (wire)
  (let ((first (car wire)))
    (if (equal "system" (alist-get 'role first)) (hm--text first) "")))

(defun wh--has (needle wire) (and (string-match-p (regexp-quote needle) (wh--wire-text wire)) t))

(defun wh--tool-results (wires)
  (cl-loop for m in (hm--body (car (last wires)))
           when (equal "tool" (alist-get 'role m))
           collect (hm--text m)))

(defun wh--kill-views-of (session)
  (let ((kill-buffer-query-functions nil))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b) (buffer-local-value 'superchat-mode b)
                 (equal session (buffer-local-value 'superchat--session-id b)))
        (kill-buffer b)))))

(defun wh--drop-memory ()
  "Kill every chat View, clear what this process keeps about conversations,
close and reopen the database: as close to a restart as one process gets."
  (let ((kill-buffer-query-functions nil))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b) (buffer-local-value 'superchat-mode b))
        (kill-buffer b))))
  (clrhash superchat-view--configuration-memo)
  (clrhash superchat-editor-workspace--states)
  (clrhash superchat-work--retries)
  (clrhash superchat-work--owed)
  (clrhash superchat-work--continuing)
  (superchat-work-disarm-all)
  (superchat-db-close)
  (superchat-db-open))

(defun wh--home-rules (text)
  (let ((dir (expand-file-name "home/" superchat-data-directory)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "AGENTS.md" dir) (insert text "\n"))))

(defun wh--ask (buffer question answer)
  "In-place ask QUESTION in BUFFER; the model answers ANSWER.
Return (SESSION . WIRE-OF-THE-ASK)."
  (setq smoke--script (list (list :text answer)))
  (let* ((before (length hm--wires))
         (overlay (with-current-buffer buffer
                    (goto-char (point-min))
                    (superchat-inline-ask question (cons 1 5))))
         (state (lambda () (overlay-get overlay 'superchat-inline))))
    (should (smoke--wait (lambda () (memq (plist-get (funcall state) :status)
                                          '(ready failed error)))
                         20))
    (wrec--settled)
    (cons (plist-get (funcall state) :session-id)
          (car (last (cl-subseq hm--wires 0 (- (length hm--wires) before)))))))

(defun wh--bound-payload (session)
  "The payload of SESSION's last `workspace.bound' fact, as stored."
  (let (payload)
    (dolist (row (superchat-db-tape-select
                  "SELECT meta FROM tape WHERE session_id = ? AND kind = 'system'
                   AND meta LIKE '%workspace.bound%' ORDER BY id ASC"
                  (list session)))
      (let ((meta (superchat-tape-read-json-alist (car row) 'list)))
        (when (equal "workspace.bound"
                     (superchat-view--object-get meta '(event_type event-type)))
          (setq payload (superchat-view--object-get meta '(payload))))))
    payload))

(defun wh--get (payload &rest keys) (superchat-view--object-get payload keys))

;;;; Membership and the stored record ---------------------------------------------

(ert-deftest workspace-home/a-buffer-in-no-project-belongs-to-home ()
  "A buffer no project claims, interfaces included, belongs to Home."
  (wh--with nil (p)
    (let* ((home (superchat-workspace-of-source (wh--chat 1)))
           (id (plist-get home :id)))
      (should (eq 'home (plist-get home :kind)))
      (should (equal "Home" (plist-get home :name)))
      (should-not (plist-get home :root))
      (should (string-match-p "\\`home-[0-9a-f]\\{12\\}\\'" id))
      ;; Kept in the data directory, not in a `.superchat/' a project finder sees.
      (should (equal id (wh--home-id)))
      (should-not (file-exists-p (expand-file-name ".superchat" superchat-data-directory)))
      (should-not (file-exists-p (expand-file-name "home/.superchat"
                                                   superchat-data-directory)))
      (dolist (src (list (wh--entry 1) (wh--other) (wh--loose-file)))
        (should (equal home (superchat-workspace-of-source src))))
      ;; A file in a project is the project's.
      (let ((project (superchat-workspace-of-source (wrec--file p))))
        (should (eq 'project (plist-get project :kind)))
        (should (equal p (plist-get project :root))))
      ;; The same id every time.
      (should (equal id (plist-get (superchat-workspace-home) :id))))))

(ert-deftest workspace-home/home-is-stored-on-the-tape-and-read-back ()
  "The `workspace.bound' fact carries Home's id, kind, name and source key, and
a conversation reopened from the Tape alone is Home's again."
  (wh--with '((:text "WH-ACK")) ()
    (let* ((view (wh--open (wh--chat 1)))
           (session (wh--sid view))
           (payload (wh--bound-payload session)))
      (should (equal (wh--home-id) (wh--get payload 'workspace_id 'workspace-id)))
      (should-not (wh--get payload 'workspace_root 'workspace-root))
      (should (equal "home" (wh--get payload 'workspace_kind 'workspace-kind)))
      (should (equal "Home" (wh--get payload 'workspace_name 'workspace-name)))
      (should (equal "whchat:chat:1" (wh--get payload 'source_key 'source-key)))
      (wh--turn view "WH-STORED hello" '((:text "WH-ACK")))
      (wh--drop-memory)
      (let* ((config (wh--config session))
             (again (superchat-open-session session))
             (binding (buffer-local-value 'superchat--workspace-binding again)))
        (should (equal "home" (plist-get config :workspace-kind)))
        (should (equal "Home" (plist-get config :workspace-name)))
        (should (equal "whchat:chat:1" (plist-get config :source-key)))
        (should (eq 'home (plist-get binding :kind)))
        (should (equal "Home" (plist-get binding :name)))
        (should-not (plist-get binding :root))
        (should (equal "whchat:chat:1" (plist-get binding :source-key)))
        (should-not (buffer-local-value 'superchat-tools-default-directory again))
        (should-not (superchat--view-temporary-p again))))))

(ert-deftest workspace-home/a-project-binding-stores-its-kind-and-name ()
  (wh--with nil (p)
    (let* ((view (with-current-buffer (wrec--file p) (superchat)))
           (payload (wh--bound-payload (wh--sid view))))
      (should (equal p (wh--get payload 'workspace_root 'workspace-root)))
      (should (equal "project" (wh--get payload 'workspace_kind 'workspace-kind)))
      (should (equal (file-name-nondirectory (directory-file-name p))
                     (wh--get payload 'workspace_name 'workspace-name)))
      (should-not (wh--get payload 'source_key 'source-key)))))

(ert-deftest workspace-home/home-is-not-the-project-that-encloses-the-data-directory ()
  "The default data directory lies in `~/.emacs.d', which may be a git
repository: Home is never that project."
  (skip-unless (executable-find "git"))
  (wh--with nil ()
    (let* ((emacs-d (file-name-as-directory
                     (file-truename (make-temp-file "wh-emacs-d-" t))))
           (superchat-data-directory (expand-file-name "superchat/" emacs-d))
           (project-find-functions (list #'project-try-vc)))
      (unwind-protect
          (progn
            (let ((default-directory emacs-d))
              (should (eq 0 (call-process "git" nil nil nil "init" "-q"))))
            (make-directory superchat-data-directory t)
            ;; The data directory itself is in that repository...
            (should (equal emacs-d (plist-get (superchat-workspace-of-directory
                                               superchat-data-directory)
                                              :root)))
            ;; ...but Home is not that project, and a buffer in no project
            ;; still belongs to Home.
            (let ((home (superchat-workspace-of-source (wh--chat 1)))
                  (repo (superchat-workspace-of-directory emacs-d)))
              (should (eq 'home (plist-get home :kind)))
              (should-not (plist-get home :root))
              (should-not (equal (plist-get repo :id) (plist-get home :id)))
              (should (equal (plist-get home :id) (wh--home-id)))
              (should-not (file-exists-p (expand-file-name ".superchat" emacs-d)))
              ;; Home's rules come from its store, which no request treats as a
              ;; root.
              (should (equal (expand-file-name "home/" superchat-data-directory)
                             (superchat-workspace-rules-root
                              (plist-get home :id) nil "home")))
              (let ((view (wh--open (wh--chat 1))))
                (should-not (buffer-local-value 'superchat-tools-default-directory view))
                (should (equal wh--dir (buffer-local-value 'default-directory view))))))
        (ignore-errors (delete-directory emacs-d t))))))

;;;; Rules ------------------------------------------------------------------------

(ert-deftest workspace-home/home-rules-reach-home-requests-only ()
  "Home's rules go into Home's chat, in-place and sub-agent requests and no
project's; a project's rules never reach Home's."
  (wh--with nil (p)
    (wh--home-rules "WH-HOME-RULE answer in haiku")
    (wrec--write p "AGENTS.md" "WH-PROJ-RULE use tabs\n")
    (let* ((chat (wh--chat 1))
           (pbuf (wrec--file p))
           (vh (wh--open chat))
           (wh (car (wh--turn vh "WH-H hello" '((:text "WH-ACK-H")))))
           (vp (wh--open pbuf))
           (wp (car (wh--turn vp "WH-P hello" '((:text "WH-ACK-P"))))))
      (should (string-match-p "WH-HOME-RULE" (wh--system-text wh)))
      (should (string-match-p "Home rules, from " (wh--system-text wh)))
      (should-not (wh--has "WH-PROJ-RULE" wh))
      (should (string-match-p "WH-PROJ-RULE" (wh--system-text wp)))
      (should-not (wh--has "WH-HOME-RULE" wp))
      ;; In place.
      (let ((ask-home (wh--ask chat "WH-ASK1 what" "WH-ASK1-ANSWER"))
            (ask-proj (wh--ask pbuf "WH-ASK2 what" "WH-ASK2-ANSWER")))
        (should (equal (wh--sid vh) (car ask-home)))
        (should (string-match-p "WH-HOME-RULE" (wh--system-text (cdr ask-home))))
        (should-not (wh--has "WH-PROJ-RULE" (cdr ask-home)))
        (should (string-match-p "WH-PROJ-RULE" (wh--system-text (cdr ask-proj))))
        (should-not (wh--has "WH-HOME-RULE" (cdr ask-proj))))
      ;; A sub-agent carries its parent's Workspace's rules.
      (dolist (case (list (list vh "WH-HOME-RULE" "WH-PROJ-RULE")
                          (list vp "WH-PROJ-RULE" "WH-HOME-RULE")))
        (let* ((before (length hm--wires))
               (_ (wh--turn (nth 0 case) "WH-DELEGATE look into it"
                            '((:tool-uses (("delegate_to_subagent"
                                            (preset . "researcher")
                                            (task . "WH-SUB-TASK look into it"))))
                              (:text "WH-SUB-REPORT found it")
                              (:text "WH-DONE"))))
               (sub (cl-find-if
                     (lambda (wire)
                       (cl-some (lambda (m) (and (equal "user" (alist-get 'role m))
                                                 (string-match-p "WH-SUB-TASK" (hm--text m))))
                                wire))
                     (reverse (cl-subseq hm--wires 0 (- (length hm--wires) before))))))
          (should sub)
          (should (string-match-p (nth 1 case) (wh--system-text sub)))
          (should-not (wh--has (nth 2 case) sub)))))))

(ert-deftest workspace-home/the-remember-note-names-homes-rules-file ()
  (wh--with nil ()
    (let* ((view (wh--open (wh--chat 1)))
           (file (expand-file-name "home/AGENTS.md" superchat-data-directory))
           (w1 (car (wh--turn view "WH-N1" '((:text "ok"))))))
      (should (string-match-p "This conversation is in Home" (wh--system-text w1)))
      (should (string-match-p (concat "write it to " (regexp-quote file))
                              (wh--system-text w1)))
      (should-not (string-match-p "temporary conversation" (wh--system-text w1)))
      (wh--home-rules "WH-RULE-1")
      (let ((w2 (car (wh--turn view "WH-N2" '((:text "ok"))))))
        (should (string-match-p (concat "add it to " (regexp-quote file))
                                (wh--system-text w2)))))))

(ert-deftest workspace-home/homes-rules-file-is-agents-md-only ()
  "Home has one rules file: a CLAUDE.md in its store is not read."
  (wh--with nil ()
    (let ((dir (expand-file-name "home/" superchat-data-directory)))
      (make-directory dir t)
      (with-temp-file (expand-file-name "CLAUDE.md" dir) (insert "WH-CLAUDE-RULE\n"))
      (let* ((view (wh--open (wh--chat 1)))
             (wire (car (wh--turn view "WH-C" '((:text "ok"))))))
        (should-not (wh--has "WH-CLAUDE-RULE" wire))
        (should-not (superchat-workspace-rules-file dir))))))

(ert-deftest workspace-home/the-model-opens-homes-rules-file-and-nothing-else-there ()
  "The data-directory guard of `open_file' lets through Home's rules file only."
  (wh--with nil ()
    (let* ((view (wh--open (wh--chat 1)))
           (rules (expand-file-name "home/AGENTS.md" superchat-data-directory))
           (id-file (expand-file-name "home/id" superchat-data-directory))
           (other (expand-file-name "secret-state.txt" superchat-data-directory))
           (link-dir (expand-file-name "home2/" superchat-data-directory)))
      (with-temp-file other (insert "internal\n"))
      ;; Not there yet: a new buffer visiting it.
      (should (string-match-p "\\`Opened buffer AGENTS.md"
                              (superchat-tool-open-file rules)))
      (with-current-buffer (get-file-buffer rules)
        (set-buffer-modified-p nil) (kill-buffer))
      (wh--home-rules "WH-RULE")
      (let ((w (wh--turn view "WH-REMEMBER remember: always answer briefly"
                         `((:tool-uses (("open_file" (path . ,rules))))
                           (:tool-uses (("edit_buffer" (buffer_name . "AGENTS.md")
                                         (old_string . "WH-RULE")
                                         (new_string . "WH-RULE\nWH-RULE-2"))))
                           (:tool-uses (("open_file" (path . ,other))))
                           (:tool-uses (("open_file" (path . ,id-file))))
                           (:text "ok")))))
        (let ((results (last (wh--tool-results w) 4)))
          (should (string-match-p "Opened buffer AGENTS.md" (nth 0 results)))
          (should (string-match-p "Replaced 1 occurrence" (nth 1 results)))
          (should (string-match-p "inside Superchat.s own data directory" (nth 2 results)))
          (should (string-match-p "inside Superchat.s own data directory" (nth 3 results))))
        ;; Edited in the buffer only: the user's save makes it a rule.
        (should (buffer-modified-p (get-file-buffer rules)))
        (with-current-buffer (get-file-buffer rules)
          (set-buffer-modified-p nil) (kill-buffer)))
      ;; A rules file that is a link to another file of the data directory is
      ;; judged by where it leads.
      (make-directory link-dir t)
      (delete-file rules)
      (make-symbolic-link other rules)
      (should (string-match-p "inside Superchat.s own data directory"
                              (superchat-tool-open-file rules)))
      (should-not (superchat-workspace-home-rules-file-p
                   (expand-file-name "AGENTS.md" link-dir))))))

;;;; Where commands run, what is inside ---------------------------------------------

(ert-deftest workspace-home/home-commands-run-where-the-source-is ()
  "A Home conversation runs commands in its source's directory, and nothing
it edits is outside; a project's still run at its root."
  (wh--with nil (p)
    (let* ((other-dir (file-name-as-directory (file-truename (make-temp-file "wh-other-" t))))
           (other (expand-file-name "notes.txt" other-dir))
           (vh (wh--open (wh--chat 1)))
           (vp (with-current-buffer (wrec--file p) (superchat))))
      (unwind-protect
          (progn
            (should (eq 'home (plist-get (buffer-local-value 'superchat--workspace-binding vh)
                                         :kind)))
            (with-temp-file other (insert "hello\n"))
            (let ((w (wh--turn vh "WH-DIR"
                               `((:tool-uses (("eval_elisp" (expression . "(list default-directory (superchat-tools--base-dir))"))))
                                 (:tool-uses (("open_file" (path . ,other))))
                                 (:tool-uses (("edit_buffer" (buffer_name . "notes.txt")
                                               (old_string . "hello") (new_string . "WH-EDITED"))))
                                 (:text "ok")))))
              (should (string-match-p (regexp-quote (format "(#1=%S #1#)" wh--dir))
                                      (nth 0 (last (wh--tool-results w) 3)))))
            (should (equal '(nil) (mapcar (lambda (row) (plist-get row :outside))
                                          (superchat-changes-candidates (wh--sid vh)))))
            (let ((w (wh--turn vp "WH-PDIR"
                               '((:tool-uses (("eval_elisp" (expression . "(superchat-tools--base-dir)"))))
                                 (:tool-uses (("edit_buffer" (buffer_name . "notes.txt")
                                               (old_string . "WH-EDITED") (new_string . "WH-EDITED-2"))))
                                 (:text "ok")))))
              (should (string-match-p (regexp-quote (format "%S" p))
                                      (nth 0 (last (wh--tool-results w) 2)))))
            (should (cl-every (lambda (row) (plist-get row :outside))
                              (superchat-changes-candidates (wh--sid vp)))))
        (when-let* ((b (get-file-buffer other)))
          (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b))
        (ignore-errors (delete-directory other-dir t))))))

;;;; One conversation per source (D11-D14) -----------------------------------------

(ert-deftest workspace-home/each-source-has-its-own-conversation ()
  "The three rules of D13: a mode's function, the file visited, the buffer name."
  (wh--with nil ()
    (let* ((sources (list (cons "chat1" (wh--chat 1))
                          (cons "chat2" (wh--chat 2))
                          (cons "chat1-topic7" (wh--chat 1 7))
                          (cons "entry42" (wh--entry 42))
                          (cons "file" (wh--loose-file))
                          (cons "other" (wh--other))))
           (views (mapcar (lambda (pair) (cons (car pair) (wh--open (cdr pair)))) sources))
           (sessions (mapcar (lambda (pair) (wh--sid (cdr pair))) views)))
      ;; Six sources, six conversations, all Home's, each under its key.
      (should (= 6 (length (delete-dups (copy-sequence sessions)))))
      (dolist (session sessions)
        (should (equal "home" (plist-get (wh--config session) :workspace-kind))))
      (should (equal '("whchat:chat:1" "whchat:chat:2" "whchat:chat:1:topic:7"
                       "whentry:entry:42")
                     (mapcar (lambda (s) (plist-get (wh--config s) :source-key))
                             (seq-take sessions 4))))
      (should (equal (concat "file:" (expand-file-name "loose.txt" wh--dir))
                     (plist-get (wh--config (nth 4 sessions)) :source-key)))
      (should (equal "buffer:wh-other-mode:*wh-other*"
                     (plist-get (wh--config (nth 5 sessions)) :source-key)))
      ;; The same source opens the same conversation again; another buffer
      ;; showing the same chat is the same source.
      (cl-loop for (name . src) in sources
               for session in sessions
               do (should (equal session (wh--sid (wh--open src)))))
      (should (equal (nth 0 sessions)
                     (wh--sid (wh--open (wh--chat 1 nil "*wh-chat-1-second*")))))
      ;; The entry buffer shows another entry now: another conversation.
      (should-not (equal (nth 3 sessions) (wh--sid (wh--open (wh--entry 43))))))))

(ert-deftest workspace-home/a-source-continues-after-its-buffer-is-killed ()
  "A file and a named buffer, killed and made again, continue their
conversation and its words."
  (wh--with nil ()
    (dolist (make (list #'wh--loose-file (lambda () (wh--other "*wh-named*"))
                        (lambda () (wh--chat 5))))
      (let* ((src (funcall make))
             (view (wh--open src))
             (session (wh--sid view)))
        (wh--turn view "WH-KILL-MARK the plumber comes on Tuesday" '((:text "ok")))
        (wh--kill-views-of session)
        (let ((kill-buffer-query-functions nil))
          (with-current-buffer src (set-buffer-modified-p nil))
          (kill-buffer src))
        (let* ((again (wh--open (funcall make)))
               (wire (car (wh--turn again "WH-KILL-NEXT when" '((:text "ok"))))))
          (should (equal session (wh--sid again)))
          (should (wh--has "WH-KILL-MARK" wire)))))))

(ert-deftest workspace-home/a-source-continues-after-memory-is-dropped ()
  (wh--with nil ()
    (let* ((view (wh--open (wh--chat 1)))
           (session (wh--sid view)))
      (wh--turn view "WH-E-MARK the plumber comes on Tuesday" '((:text "ok")))
      (wh--drop-memory)
      (let* ((again (wh--open (wh--chat 1 nil "*wh-chat-1-after*")))
             (wire (car (wh--turn again "WH-E-NEXT when does he come" '((:text "ok")))))
             (binding (buffer-local-value 'superchat--workspace-binding again)))
        (should (equal session (wh--sid again)))
        (should (wh--has "WH-E-MARK" wire))
        (should (eq 'home (plist-get binding :kind)))
        (should (equal "whchat:chat:1" (plist-get binding :source-key)))))))

(ert-deftest workspace-home/in-place-commands-join-the-sources-conversation ()
  "From a buffer with no key function, M-x superchat, in-place asks and the
chat afterwards are one conversation (the prototype opened a new one each time)."
  (wh--with nil ()
    (dolist (src (list (wh--src "*wh-scratch*" #'lisp-interaction-mode)
                       (wh--loose-file "ask.txt")))
      (let* ((va (wh--open src))
             (_ (wh--turn va "WH-NK-MARK first words here" '((:text "ok"))))
             (vb (wh--open src))
             (ask1 (wh--ask src "WH-NK-ASK1 what" "WH-NK-ANS1"))
             (ask2 (wh--ask src "WH-NK-ASK2 and before" "WH-NK-ANS2"))
             (vc (wh--open src))
             (wire (car (wh--turn vc "WH-NK-FOLLOW what did you answer" '((:text "ok"))))))
        (should (equal "home" (plist-get (wh--config (wh--sid va)) :workspace-kind)))
        (should (equal (wh--sid va) (wh--sid vb)))
        (should (equal (wh--sid va) (car ask1)))
        (should (equal (wh--sid va) (car ask2)))
        (should (equal (wh--sid va) (wh--sid vc)))
        (should (wh--has "WH-NK-ASK2" wire))
        (should (wh--has "WH-NK-MARK" wire))))))

(ert-deftest workspace-home/a-new-session-from-a-source-becomes-its-conversation ()
  (wh--with nil ()
    (let* ((chat (wh--chat 1))
           (first (wh--sid (wh--open chat)))
           (new (wh--sid (with-current-buffer chat (superchat-new-session)))))
      (should-not (equal first new))
      (should (equal "whchat:chat:1" (plist-get (wh--config new) :source-key)))
      (should (equal "home" (plist-get (wh--config new) :workspace-kind)))
      (should (equal new (wh--sid (wh--open chat)))))))

(ert-deftest workspace-home/m-x-superchat-in-a-home-view-stays-on-it ()
  (wh--with nil ()
    (let ((view (wh--open (wh--chat 1))))
      (should (eq 'home (plist-get (buffer-local-value 'superchat--workspace-binding view)
                                   :kind)))
      (should (eq view (with-current-buffer view (superchat)))))))

;;;; The real key functions, with their variables set by hand ----------------------

(defmacro wh--in-mode (mode &rest body)
  "Run BODY in a temporary buffer whose `major-mode' says MODE."
  (declare (indent 1))
  `(with-temp-buffer
     (setq major-mode ',mode)
     ,@body))

(ert-deftest workspace-home/the-key-functions-read-each-modes-own-variables ()
  (wh--in-mode telega-chat-mode
    (should-not (superchat-source-key-telega-chat))
    (setq-local telega-chatbuf--chat '(:id -1001234 :title "Group"))
    (should (equal "telega:chat:-1001234" (superchat-source-key-telega-chat)))
    (should (equal "telega:chat:-1001234" (superchat-source-key)))
    (setq-local telega-chatbuf--topic '(:id 77))
    ;; Without telega's topic accessor the topic is not guessed.
    (unless (fboundp 'telega-topic-id)
      (should (equal "telega:chat:-1001234" (superchat-source-key-telega-chat)))
      (cl-letf (((symbol-function 'telega-topic-id)
                 (lambda (topic) (plist-get topic :id))))
        (should (equal "telega:chat:-1001234:topic:77"
                       (superchat-source-key-telega-chat))))))
  (wh--in-mode elfeed-show-mode
    (should-not (superchat-source-key-elfeed-entry))
    (setq-local elfeed-show-entry 'an-entry)
    (cl-letf (((symbol-function 'elfeed-entry-id)
               (lambda (entry) (and (eq entry 'an-entry)
                                    '("example.org" . "https://example.org/post/1")))))
      (should (equal "elfeed:entry:example.org:https://example.org/post/1"
                     (superchat-source-key-elfeed-entry)))
      (should (equal (superchat-source-key-elfeed-entry) (superchat-source-key)))))
  (wh--in-mode eww-mode
    (should-not (superchat-source-key-eww))
    (setq-local eww-data '(:url "https://www.gnu.org/software/emacs/" :title "Emacs"))
    (should (equal "eww:https://www.gnu.org/software/emacs/" (superchat-source-key))))
  (wh--in-mode dired-mode
    (setq-local dired-directory "/tmp/a/b")
    (should (equal "dir:/tmp/a/b/" (superchat-source-key-dired)))
    (setq-local dired-directory '("/tmp/a/" "x.txt" "y.txt"))
    (should (equal "dir:/tmp/a/" (superchat-source-key))))
  (wh--in-mode Info-mode
    (setq-local Info-current-file "/usr/share/info/emacs.info.gz")
    (setq-local Info-current-node "Buffers")
    (should (equal "info:emacs.info.gz:Buffers" (superchat-source-key))))
  (wh--in-mode gnus-summary-mode
    (setq-local gnus-newsgroup-name "nntp+news:gmane.emacs.devel")
    (should (equal "gnus:group:nntp+news:gmane.emacs.devel" (superchat-source-key))))
  (wh--in-mode notmuch-search-mode
    (setq-local notmuch-search-query-string "tag:inbox")
    (should (equal "notmuch:query:tag:inbox" (superchat-source-key))))
  (wh--in-mode notmuch-show-mode
    (setq-local notmuch-show-thread-id "thread:0000000000001234")
    (should (equal "notmuch:thread:0000000000001234" (superchat-source-key))))
  (wh--in-mode rcirc-mode
    (setq-local rcirc-server "irc.libera.chat")
    (should (equal "rcirc:irc.libera.chat" (superchat-source-key)))
    (setq-local rcirc-target "#emacs")
    (should (equal "rcirc:irc.libera.chat:#emacs" (superchat-source-key)))))

(ert-deftest workspace-home/a-key-function-that-cannot-tell-leaves-the-later-rules ()
  "A function that returns nothing, or signals, gives way to the file and the
buffer-name rules; a name Emacs suffixed with <2> is used as it is."
  (let ((superchat-source-key-functions
         (list (cons 'wh-other-mode (lambda () (error "Boom")))
               (cons 'wh-chat-mode #'ignore))))
    (with-temp-buffer
      (wh-other-mode)
      (rename-buffer "*wh-unique*<2>" t)
      (should (equal (format "buffer:wh-other-mode:%s" (buffer-name))
                     (superchat-source-key))))
    (with-temp-buffer
      (wh-chat-mode)
      (setq buffer-file-name "/tmp/wh-not-saved.txt")
      (should (equal "file:/tmp/wh-not-saved.txt" (superchat-source-key)))
      (setq buffer-file-name nil))
    ;; An indirect buffer is keyed by its base's file.
    (let ((base (generate-new-buffer "wh-base")))
      (unwind-protect
          (with-current-buffer base
            (setq buffer-file-name "/tmp/wh-base.txt")
            (let ((indirect (make-indirect-buffer base "wh-indirect")))
              (unwind-protect
                  (should (equal "file:/tmp/wh-base.txt" (superchat-source-key indirect)))
                (kill-buffer indirect)))
            (setq buffer-file-name nil))
        (kill-buffer base)))
    (should-not (superchat-source-key (let ((b (generate-new-buffer "wh-dead")))
                                        (kill-buffer b) b)))))

(ert-deftest workspace-home/the-key-table-is-a-user-option ()
  (should (custom-variable-p 'superchat-source-key-functions))
  (should (assq 'telega-chat-mode (default-value 'superchat-source-key-functions)))
  (should (assq 'elfeed-show-mode (default-value 'superchat-source-key-functions)))
  (should (assq 'eww-mode (default-value 'superchat-source-key-functions)))
  (should (assq 'dired-mode (default-value 'superchat-source-key-functions)))
  ;; No key function needs its package: none is loaded here.
  (should-not (featurep 'telega))
  (should-not (featurep 'elfeed))
  (dolist (pair (default-value 'superchat-source-key-functions))
    (should (fboundp (cdr pair)))
    (with-temp-buffer (should-not (funcall (cdr pair))))))

;;;; Nothing crosses between sources -----------------------------------------------

(defconst wh--secret "my locker code word is WH-SECRET pelican harbor")

(ert-deftest workspace-home/nothing-said-in-one-source-reaches-another ()
  "History, automatic recall, `memory_search' and `tool_history' of one Home
source are not another's, nor a project's."
  (wh--with nil (p)
    (let* ((chat1 (wh--chat 1))
           (v1 (wh--open chat1)))
      (wh--turn v1 wh--secret '((:text "WH-ACK")))
      (wh--turn v1 "WH-TASK keep the trip planning going"
                (list (wrec--call '(action . "start") '(goal . "WH-GOAL plan the trip"))
                      '(:text "WH-ACK2")))
      ;; The search does find it where it was said: the probe works.
      (let ((own (wh--turn v1 "WH-OWN look it up"
                           '((:tool-uses (("memory_search" (query . "locker code word"))))
                             (:text "ok")))))
        (should (string-match-p "WH-SECRET" (car (last (wh--tool-results own))))))
      (dolist (src (list (wh--chat 2) (wh--chat 1 7) (wh--entry 42)
                         (wh--src "*wh-scratch*" #'lisp-interaction-mode)
                         (wrec--file p)))
        (let* ((view (wh--open src))
               (w1 (car (wh--turn view "what was my locker code word again"
                                  '((:text "Q1")))))
               (w2 (wh--turn view "WH-STEP look it up"
                             '((:tool-uses (("memory_search" (query . "locker code word"))))
                               (:tool-uses (("tool_history" (tool_name . "work_record"))))
                               (:text "Q2"))))
               (results (last (wh--tool-results w2) 2)))
          (should-not (equal (wh--sid v1) (wh--sid view)))
          (should-not (wh--has "WH-SECRET" w1))
          (should-not (string-match-p "WH-SECRET" (nth 0 results)))
          (should-not (string-match-p "WH-GOAL\\|WH-SECRET" (nth 1 results))))))))

;;;; Work records -------------------------------------------------------------------

(defun wh--record-of (view goal)
  (cl-loop for e in (superchat-tape-replay-events (wh--sid view))
           when (and (equal "work.created" (plist-get e :type))
                     (equal goal (plist-get (plist-get e :payload) :goal)))
           return (plist-get (plist-get e :payload) :work-id)))

(defun wh--start-in-chat1 ()
  "Start a record in chat 1's conversation; return (VIEW . ID)."
  (let* ((v1 (wh--open (wh--chat 1)))
         (task "WH-TASK export the chat history of last month"))
    (wh--turn v1 task
              (list (wrec--call '(action . "start") `(goal . ,task))
                    (wrec--call '(action . "update")
                                '(progress . "WH-PROG weeks one and two exported")
                                '(resume . "WH-RESUME start at week three"))
                    '(:text "ok")))
    (cons v1 (wh--record-of v1 task))))

(ert-deftest workspace-home/a-record-of-home-is-kept-and-seen-from-another-source ()
  (wh--with nil (p)
    (let* ((start (wh--start-in-chat1))
           (id (cdr start))
           (ve (wh--open (wh--entry 42)))
           (wire (car (wh--turn ve "WH-CHECK what is open" '((:text "ok"))))))
      (should id)
      (should (equal (wh--home-id) (plist-get (superchat-work-get id) :workspace-id)))
      (should-not (equal (wh--sid (car start)) (wh--sid ve)))
      (should (string-match-p (concat "Other work records in this workspace[^\0]*"
                                      (regexp-quote id))
                              (wh--wire-text wire)))
      ;; Not from a project.
      (let* ((vp (with-current-buffer (wrec--file p) (superchat)))
             (wp (car (wh--turn vp "WH-P" '((:text "ok"))))))
        (should-not (wh--has id wp))
        (let ((superchat--tool-execution-context (list :session-id (wh--sid vp))))
          (should (string-match-p "belongs to another workspace"
                                  (superchat-work-tool "read" id))))))))

(ert-deftest workspace-home/read-shows-a-record-without-taking-it-up ()
  (wh--with nil ()
    (let* ((id (cdr (wh--start-in-chat1)))
           (ve (wh--open (wh--entry 42)))
           (w (wh--turn ve "WH-READ" (list (wrec--call '(action . "read") `(id . ,id))
                                           '(:text "ok")))))
      (should (string-match-p "WH-RESUME" (car (last (wh--tool-results w)))))
      (should-not (superchat-work-current (wh--sid ve))))))

(ert-deftest workspace-home/update-takes-a-record-up ()
  (wh--with nil ()
    (let* ((id (cdr (wh--start-in-chat1)))
           (ve (wh--open (wh--entry 42))))
      (wh--turn ve "WH-UPD" (list (wrec--call '(action . "update") `(id . ,id)
                                              '(progress . "WH-PROG2 week three started"))
                                  '(:text "ok")))
      (should (equal id (superchat-work-current (wh--sid ve))))
      (let ((wire (car (wh--turn ve "WH-AGAIN" '((:text "ok"))))))
        (should (string-match-p (concat "Current work record:\nWork record "
                                        (regexp-quote id))
                                (wh--wire-text wire)))))))

(ert-deftest workspace-home/resume-takes-a-record-up-and-its-rounds-follow ()
  "`resume' of a record that is not current makes it this conversation's
current record, says so, and the round it promises starts here."
  (wh--with nil ()
    (let* ((start (wh--start-in-chat1))
           (id (cdr start))
           (ve (wh--open (wh--entry 42)))
           (session (wh--sid ve))
           (w (wh--turn ve "WH-RESUME-IT go on with the export"
                        (list (wrec--call '(action . "resume") `(id . ,id))
                              '(:text "WH-ACK-RESUME")
                              ;; The round that follows completes the record.
                              (wrec--call '(action . "done") `(id . ,id)
                                          '(conclusion . "WH-DONE exported"))
                              '(:text "WH-ROUND-END"))))
           (answer (cl-find-if (lambda (text) (string-match-p "Rounds on" text))
                               (cl-loop for wire in w append (wh--tool-results (list wire))))))
      (should (string-match-p (concat (regexp-quote id)
                                      " is now this conversation's current work record")
                              answer))
      (should (string-match-p "go on in this conversation" answer))
      ;; Focused here (the round may have completed it by now).
      (should (member id (mapcar (lambda (fact) (plist-get fact :work-id))
                                 (wrec--facts session "work.focused"))))
      (wrec--settled)
      (should (smoke--wait (lambda () (plist-get (superchat-work-get id) :conclusion)) 20))
      (wrec--settled)
      (should (hm--events session "work.round"))
      (should-not (hm--events (wh--sid (car start)) "work.round"))
      (should (equal "WH-DONE exported" (plist-get (superchat-work-get id) :conclusion))))))

(ert-deftest workspace-home/resume-takes-a-record-up-in-a-project-too ()
  "The fix is not Home's alone: in a project, resume from a second
conversation makes the record current there."
  (wh--with nil (p)
    (let* ((src (wrec--file p))
           (v1 (with-current-buffer src (superchat)))
           (id (superchat-work-start (wh--sid v1) "WH-PROJECT-GOAL"))
           (v2 (with-current-buffer src (superchat-new-session))))
      (wh--turn v2 "WH-NEXT" '((:text "ok")))
      (should-not (superchat-work-current (wh--sid v2)))
      (let ((w (wh--turn v2 "WH-RESUME go on"
                         (list (wrec--call '(action . "resume") `(id . ,id))
                               '(:text "ok")
                               '(:text "round one")
                               '(:text "round two")
                               '(:text "round three")))))
        (should (string-match-p "is now this conversation's current work record"
                                (mapconcat #'identity
                                           (cl-loop for wire in w
                                                    append (wh--tool-results (list wire)))
                                           "\n")))
        (should (equal id (superchat-work--focused (wh--sid v2))))
        (wrec--settled)
        (superchat-work-pause id)))))

(ert-deftest workspace-home/a-waiting-record-of-home-is-woken-in-home ()
  (wh--with nil ()
    (wh--home-rules "WH-HOME-RULE answer in haiku")
    (let* ((start (wh--start-in-chat1))
           (s1 (wh--sid (car start)))
           (id (cdr start)))
      (superchat-work-wait s1 id "time" "WH-LATER" (+ wrec--now 600))
      (setq wrec--now (+ wrec--now 600))
      (setq smoke--script (list '(:text "WH-WOKEN-ANSWER")))
      (let* ((before (length hm--wires))
             (views (superchat-work-wake-due)))
        (wrec--settled)
        (should (= 1 (length views)))
        (let* ((session (wh--sid (car views)))
               (config (wh--config session))
               (woken (car (wrec--facts session "work.woken")))
               (wire (car (last (cl-subseq hm--wires 0 (- (length hm--wires) before))))))
          (should-not (equal s1 session))
          (should (equal "home" (plist-get config :workspace-kind)))
          (should (equal (wh--home-id) (plist-get config :workspace-id)))
          (should-not (plist-get config :source-key))
          (should (plist-get config :background))
          (should (equal id (superchat-work-current session)))
          ;; Home has no main conversation: the one that began it is told.
          (should (equal s1 (plist-get woken :main)))
          (should (string-match-p "WH-HOME-RULE" (wh--system-text wire)))
          ;; Nobody opened it: no source continues it.
          (should (equal s1 (wh--sid (wh--open (wh--chat 1))))))))))

;;;; Timers --------------------------------------------------------------------------

(ert-deftest workspace-home/a-timer-set-in-home-is-the-users ()
  "Owner `you', seen from every conversation, fires where it was set; a task's
conversation is Home's and carries Home's rules."
  (wh--with nil (p)
    (wh--home-rules "WH-HOME-RULE answer in haiku")
    (let* ((v1 (wh--open (wh--chat 1)))
           (s1 (wh--sid v1))
           (answer (let ((superchat--tool-execution-context (list :session-id s1)))
                     (superchat-timer-tool-create "WH-REMIND call the dentist" "reminder" 60)))
           (timer (car (superchat-timer-list))))
      (should (eq 'you (plist-get timer :owner)))
      (should (equal (wh--home-id) (plist-get timer :workspace-id)))
      (should (string-match-p "the user's own, set in Home" answer))
      (should-not (string-match-p "belongs to no workspace" answer))
      ;; Seen from another source and from a project.
      (let ((ve (wh--open (wh--entry 42)))
            (vp (with-current-buffer (wrec--file p) (superchat))))
        (dolist (view (list ve vp))
          (should (superchat-timer--within-p (wh--sid view) timer))
          (let ((superchat--tool-execution-context (list :session-id (wh--sid view))))
            (should (string-match-p "WH-REMIND" (superchat-timer-tool-list))))))
      ;; It fires in the conversation it was set in.
      (setq wh--now (+ wh--now 3600))
      (superchat-timer-catch-up)
      (should (equal '("WH-REMIND call the dentist")
                     (mapcar (lambda (e) (plist-get (plist-get e :payload) :text))
                             (hm--events s1 "timer.fired"))))
      ;; A task: its conversation is Home's, nobody's source, with Home's rules.
      (superchat-timer-create s1 'task "WH-TASK-MARKER list the agenda" (+ wh--now 60))
      (setq wh--now (+ wh--now 60))
      (setq smoke--script (list '(:text "WH-TASK-ANSWER")))
      (let* ((before (length hm--wires))
             (fired (car (superchat-timer-catch-up)))
             (home (plist-get fired :session-id)))
        (wrec--settled)
        (let ((config (wh--config home))
              (wire (car (last (cl-subseq hm--wires 0 (- (length hm--wires) before))))))
          (should-not (equal s1 home))
          (should (equal "home" (plist-get config :workspace-kind)))
          (should-not (plist-get config :source-key))
          (should (string-match-p "WH-TASK-MARKER" (wh--wire-text wire)))
          (should (string-match-p "WH-HOME-RULE" (wh--system-text wire))))))))

(ert-deftest workspace-home/home-sets-no-compile-rule ()
  (wh--with nil ()
    (let* ((session (wh--sid (wh--open (wh--chat 1))))
           (problem (superchat-watch--problem session "WH look at it" "compile_failed" nil)))
      (should (string-match-p "in Home, which has no directory" problem)))))

;;;; Commands --------------------------------------------------------------------------

(defun wh--cmd (view fn args)
  (with-current-buffer view (funcall fn nil args nil nil nil)))

(ert-deftest workspace-home/workspace-command-from-home-opens-the-project-and-stays ()
  (wh--with nil (p)
    (let* ((v1 (wh--open (wh--chat 1)))
           (s1 (wh--sid v1)))
      (wh--turn v1 "WH-F first words" '((:text "ok")))
      (should (string-match-p "Workspace: Home (no directory"
                              (plist-get (wh--cmd v1 #'superchat--cmd-workspace "") :content)))
      (should (string-match-p "This conversation stays where it is"
                              (plist-get (wh--cmd v1 #'superchat--cmd-workspace p) :content)))
      (should (smoke--wait
               (lambda ()
                 (let ((s (buffer-local-value 'superchat--session-id
                                              (window-buffer (selected-window)))))
                   (and s (equal p (plist-get (wh--config s) :workspace-root)))))
               5))
      (should (equal "home" (plist-get (wh--config s1) :workspace-kind)))
      (should (equal "whchat:chat:1" (plist-get (wh--config s1) :source-key)))
      (should (equal s1 (wh--sid (wh--open (wh--chat 1))))))))

(ert-deftest workspace-home/reset-keeps-home-and-the-source ()
  (wh--with nil ()
    (let* ((v1 (wh--open (wh--chat 1)))
           (s1 (wh--sid v1)))
      (wh--turn v1 "WH-RESET-MARK first words" '((:text "ok")))
      (let ((echo (plist-get (wh--cmd v1 #'superchat--cmd-reset "") :content)))
        (should (string-match-p "stays in Home" echo))
        (should-not (string-match-p "and Workspace" echo)))
      (should (eq 'home (plist-get (buffer-local-value 'superchat--workspace-binding v1) :kind)))
      (should (equal "whchat:chat:1" (plist-get (wh--config s1) :source-key)))
      (let* ((again (wh--open (wh--chat 1)))
             (wire (car (wh--turn again "WH-AFTER-RESET" '((:text "ok"))))))
        (should (equal s1 (wh--sid again)))
        (should (wh--has "WH-RESET-MARK" wire))))))

(ert-deftest workspace-home/a-designated-personal-workspace-keeps-the-interfaces ()
  (wh--with nil ()
    (let* ((personal (file-name-as-directory (file-truename (make-temp-file "wh-personal-" t))))
           (superchat-personal-workspace-directory personal))
      (unwind-protect
          (progn
            (dolist (src (list (wh--chat 1) (wh--chat 2) (wh--entry 42)))
              (let ((config (wh--config (wh--sid (wh--open src)))))
                (should (equal personal (plist-get config :workspace-root)))
                (should (equal "project" (plist-get config :workspace-kind)))))
            (let ((config (wh--config (wh--sid (wh--open (wh--src "*wh-scratch*" #'lisp-interaction-mode))))))
              (should (equal "home" (plist-get config :workspace-kind)))))
        (ignore-errors (delete-directory personal t))))))

;;;; Conversations already on the Tape ----------------------------------------------

(ert-deftest workspace-home/an-old-project-binding-opens-as-before ()
  "A `workspace.bound' fact written before kinds and names were stored binds
its conversation to its project, named from its root, and is still the
project's main conversation."
  (wh--with nil (p)
    (let ((id (plist-get (superchat-workspace-of-directory p) :id))
          (session "20260101-000000-0001"))
      (superchat-tape-append-event
       (list :type "workspace.bound" :session-id session :actor-id "user"
             :summary "workspace.bound"
             :payload (list :workspace-id id :workspace-root p)))
      (let ((config (wh--config session)))
        (should-not (plist-member config :workspace-kind))
        (should-not (plist-member config :workspace-name)))
      (should-not (superchat-workspace-home-config-p (wh--config session)))
      (let* ((view (with-current-buffer (wrec--file p) (superchat)))
             (binding (buffer-local-value 'superchat--workspace-binding view)))
        (should (equal session (wh--sid view)))
        (should (eq 'project (plist-get binding :kind)))
        (should (equal p (plist-get binding :root)))
        (should (equal (file-name-nondirectory (directory-file-name p))
                       (plist-get binding :name)))))))

(ert-deftest workspace-home/an-old-temporary-conversation-stays-reachable-and-temporary ()
  "A conversation bound to nothing, as every temporary one before Home was, is
in the history lists, opens as it was, and no source takes it over."
  (wh--with nil ()
    (let* ((old (superchat--temporary-view))
           (session (wh--sid old)))
      (wh--turn old "WH-OLD-TEMP words" '((:text "ok")))
      (should-not (wh--bound-payload session))
      (wh--drop-memory)
      (should (member session (superchat-view-session-ids)))
      (let ((view (superchat-open-session session)))
        (should-not (buffer-local-value 'superchat--workspace-binding view))
        (should (superchat--view-temporary-p view))
        (let ((superchat--tool-execution-context (list :session-id session)))
          (should (string-match-p "belongs to no workspace"
                                  (superchat-work-tool "start" nil "WH-GOAL")))))
      (let ((home (wh--sid (wh--open (wh--src "*wh-scratch*" #'lisp-interaction-mode)))))
        (should-not (equal session home))
        (should (equal "home" (plist-get (wh--config home) :workspace-kind)))))))

(ert-deftest workspace-home/a-binding-with-homes-id-and-no-kind-is-homes ()
  (wh--with nil ()
    (let ((id (plist-get (superchat-workspace-home) :id)))
      (should (superchat-workspace-home-config-p (list :workspace-id id :workspace-root nil)))
      (should-not (superchat-workspace-home-config-p
                   (list :workspace-id "home-000000000000" :workspace-root nil)))
      (should-not (superchat-workspace-home-config-p
                   (list :workspace-id id :workspace-root wh--dir :workspace-kind "project")))
      (should (superchat-workspace-home-p (list :id id :root nil)))
      (should-not (superchat-workspace-home-p (list :id id :root nil :kind 'project))))))

;;;; The session cache, the butler ---------------------------------------------------

(ert-deftest workspace-home/closing-a-home-view-caches-its-text ()
  "The legacy startup handoff that the temporary conversation wrote is written
by a Home conversation now; not by a project's."
  (wh--with nil (p)
    (let ((cache (expand-file-name "session-cache.org" superchat-data-directory)))
      (let ((vp (with-current-buffer (wrec--file p) (superchat))))
        (with-current-buffer vp
          (let ((inhibit-read-only t))
            (goto-char (point-max)) (insert "\n" (make-string 120 ?p) "\n"))
          (superchat--summarize-session-on-exit)))
      (should-not (file-exists-p cache))
      (let ((vh (wh--open (wh--chat 1))))
        (should (eq 'home (plist-get (buffer-local-value 'superchat--workspace-binding vh)
                                     :kind)))
        (with-current-buffer vh
          (let ((inhibit-read-only t))
            (goto-char (point-max)) (insert "\nWH-CACHED " (make-string 120 ?h) "\n"))
          (superchat--summarize-session-on-exit)))
      (should (file-exists-p cache))
      (should (string-match-p "WH-CACHED" (with-temp-buffer
                                            (insert-file-contents cache)
                                            (buffer-string)))))))

(ert-deftest workspace-home/the-butler-brings-home-work-into-its-sources-conversation ()
  (wh--with nil ()
    (let* ((v1 (wh--open (wh--chat 1)))
           (s1 (wh--sid v1))
           (chat2 (wh--chat 2))
           (v2 (wh--open chat2))
           (background (superchat-workspace-new-conversation (superchat-workspace-home) t)))
      (set-window-buffer (selected-window) chat2)
      ;; Work of chat 1's conversation goes back to chat 1's, whatever the user
      ;; looks at.
      (should (eq v1 (superchat-butler--home-view (list :session s1 :run "r1"))))
      ;; Work of a conversation nobody opened goes to the buffer the user is in.
      (should (eq v2 (superchat-butler--home-view
                      (list :session (wh--sid background) :run "r2")))))))

(provide 'workspace-home-regression)
;;; workspace-home-regression.el ends here
