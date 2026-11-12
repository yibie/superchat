;;; compile-fix-regression.el --- a fix is tried in a copy -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for docs/decisions/accepted/2026-10-31-a-fix-tried-in-a-copy.md:
;; a rule with `fix' has a model change a copy of the project and run the failed
;; command there, in a sandbox; the program runs it once more, and only then is
;; there anything to apply, which goes into the user's buffers unsaved.
;;
;; The copies, the sandbox, the builds and `patch' here are real; the model is
;; scripted.  What needs `sandbox-exec' is skipped where there is none.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-jit-compilation nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/compile-fix-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^compile-fix/")'

;;; Code:

(load (expand-file-name "compile-watch-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(require 'superchat-fix)

(defconst cfix--failing "echo 'build.sh:2: Error: it fails'\nexit 2\n"
  "A build script that fails.")

(defconst cfix--command "sh build.sh"
  "How the project builds.")

(defconst cfix--edit '(:tool-uses (("edit_file" (path . "build.sh")
                                    (old . "exit 2") (new . "exit 0"))))
  "A model's edit that makes the build pass.")

(defconst cfix--useless-edit '(:tool-uses (("edit_file" (path . "build.sh")
                                            (old . "it fails") (new . "it still fails"))))
  "A model's edit after which the build still fails, with another output.")

(defvar cfix--offered nil "The tools each request of this test offered, newest first.")

(defmacro cfix--with (script vars &rest body)
  "As `wtimer--with', with a temporary directory of its own to look for copies in."
  (declare (indent 2) (debug t))
  `(wtimer--with ,script ,vars
     (skip-unless (superchat-fix-available-p))
     (let ((temporary-file-directory (file-name-as-directory (wtimer--temp-dir)))
           (network (symbol-function 'smoke--fake-network)))
       (setq cfix--offered nil)
       (unwind-protect
           (cl-letf (((symbol-function 'smoke--fake-network)
                      (lambda (mode provider prompt partial response error multi)
                        (push (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))
                              cfix--offered)
                        (funcall network mode provider prompt partial response error multi))))
             ,@body)
         (ignore-errors (delete-directory temporary-file-directory t))))))

(defun cfix--copies ()
  "The copies of projects that exist now."
  (directory-files temporary-file-directory nil "\\`superchat-copy-"))

(defun cfix--no-copies ()
  (should (smoke--wait (lambda () (null (cfix--copies))) 10)))

(defun cfix--rule (view &optional text)
  (superchat-watch-set (hm--session view) (or text "RULE-TEXT") "compile_failed" t t))

(defun cfix--project (root)
  "Make ROOT a project whose build fails."
  (wtimer--write root "build.sh" cfix--failing)
  root)

(defun cfix--fail (root)
  (cwatch--fail-and-settle root cfix--command))

(defun cfix--read-file (root name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name root))
    (buffer-string)))

(defun cfix--tool-names (wire-number)
  "The names of the tools offered in the request WIRE-NUMBER of this test."
  (sort (copy-sequence (nth (1- wire-number) (reverse cfix--offered))) #'string<))

(defun cfix--make-copy (root)
  "A copy of ROOT, or what was said instead: a plist."
  (let (result)
    (superchat-fix-copy root (lambda (made) (setq result made)))
    (should (smoke--wait (lambda () result) 15))
    result))

(defun cfix--building ()
  "The builds running now."
  (seq-filter (lambda (process) (string-prefix-p "superchat-build" (process-name process)))
              (process-list)))

(defconst cfix--slow-in-the-copy
  "if [ -n \"$CFIX_OUTSIDE\" ]; then echo 'build.sh:2: Error: it fails'; exit 2; fi\ntouch started\nsleep 60\n"
  "A build that fails at once for the user, and takes its time in a copy.
The copy's build has none of the user's environment: that is what tells them apart.")

(defconst cfix--slow-after-the-edit
  "if [ -f marker ]; then touch started; sleep 60; fi\necho 'build.sh:2: Error: it fails'\nexit 2\n"
  "A build that fails at once until a model made the file marker.")

(defmacro cfix--outside (&rest body)
  "BODY with what a compilation of the user's sees, and a build in a copy does not."
  (declare (indent 0) (debug t))
  `(let ((process-environment (cons "CFIX_OUTSIDE=1" process-environment)))
     ,@body))

(defun cfix--asked (count)
  "Wait until COUNT requests went to the model."
  (should (smoke--wait (lambda () (>= (length cfix--offered) count)) 10)))

(defun cfix--started ()
  "Wait until a build in a copy got as far as its slow part.
Stopped before that, deleting the copy alone would end it: it finds no script."
  (should (smoke--wait
           (lambda ()
             (and (cfix--building)
                  (seq-some (lambda (name)
                              (file-exists-p
                               (expand-file-name
                                "started" (expand-file-name name temporary-file-directory))))
                            (cfix--copies))))
           10)))

(defun cfix--no-builds ()
  (should (smoke--wait (lambda () (null (cfix--building))) 5)))

(defun cfix--build (dir root directory command)
  "The result of COMMAND built in DIR, a copy of ROOT."
  (let (result)
    (superchat-fix-build dir root directory command (lambda (built) (setq result built)))
    (should (smoke--wait (lambda () result) 15))
    result))

;;;; The copy --------------------------------------------------------------------

(ert-deftest compile-fix/a-copy-has-the-project-and-not-its-history ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub/.git" p) t)
    (make-directory (expand-file-name ".git" p) t)
    (make-directory (expand-file-name ".jj/repo" p) t)
    (wtimer--write p "a.txt" "A\n")
    (wtimer--write p "sub/b.txt" "B\n")
    (wtimer--write p ".git/config" "C\n")
    (wtimer--write p "sub/.git/config" "C\n")
    (wtimer--write p ".jj/repo/store" "S\n")
    (let ((dir (plist-get (cfix--make-copy p) :dir)))
      (should (equal "A\n" (cfix--read-file dir "a.txt")))
      (should (equal "B\n" (cfix--read-file dir "sub/b.txt")))
      (should-not (file-exists-p (expand-file-name ".git" dir)))
      (should-not (file-exists-p (expand-file-name ".jj" dir)))
      (should-not (file-exists-p (expand-file-name "sub/.git" dir)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-copy-has-no-file-named-like-a-secret ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub" p) t)
    (dolist (name '(".env" "server.pem" "sub/.npmrc" ".authinfo" "sub/id_rsa"))
      (wtimer--write p name "THE-SECRET\n"))
    (wtimer--write p "keep.txt" "K\n")
    (let ((dir (plist-get (cfix--make-copy p) :dir)))
      (dolist (name '(".env" "server.pem" "sub/.npmrc" ".authinfo" "sub/id_rsa"))
        (should-not (file-exists-p (expand-file-name name dir))))
      (should (file-exists-p (expand-file-name "keep.txt" dir)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-copy-leaves-out-what-the-user-listed ()
  (cfix--with nil (p)
    (make-directory (expand-file-name ".scratch" p) t)
    (make-directory (expand-file-name "sub/.scratch" p) t)
    (wtimer--write p ".scratch/big" "B\n")
    (wtimer--write p "sub/.scratch/big" "B\n")
    (wtimer--write p "a.txt" "A\n")
    (let* ((superchat-fix-copy-excludes (cons ".scratch" superchat-fix-copy-excludes))
           (dir (plist-get (cfix--make-copy p) :dir)))
      (should-not (file-exists-p (expand-file-name ".scratch" dir)))
      (should-not (file-exists-p (expand-file-name "sub/.scratch" dir)))
      (should (file-exists-p (expand-file-name "sub" dir)))
      (should (file-exists-p (expand-file-name "a.txt" dir)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/with-nothing-left-out-it-still-copies ()
  (cfix--with nil (p)
    (wtimer--write p "a.txt" "A\n")
    (let* ((superchat-fix-copy-excludes nil)
           (dir (plist-get (cfix--make-copy p) :dir)))
      (should dir)
      (should (file-exists-p (expand-file-name "a.txt" dir)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-secret-that-cannot-be-left-out-stops-the-copy ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "locked" p))
    (wtimer--write p "locked/.env" "THE-SECRET\n")
    (set-file-modes (expand-file-name "locked" p) #o500)
    (unwind-protect
        (let ((made (cfix--make-copy p)))
          (should-not (plist-get made :dir))
          (should (string-match-p "copying it failed" (plist-get made :why)))
          (cfix--no-copies))
      (set-file-modes (expand-file-name "locked" p) #o700))))

(ert-deftest compile-fix/what-a-build-made-read-only-goes-too ()
  (cfix--with nil (p)
    (wtimer--write p "a.txt" "A\n")
    (let* ((dir (plist-get (cfix--make-copy p) :dir))
           (locked (expand-file-name "locked" dir)))
      (make-directory locked)
      (wtimer--write locked "b.txt" "B\n")
      (set-file-modes locked #o500)
      (superchat-fix-delete dir)
      (cfix--no-copies))))

(ert-deftest compile-fix/deleting-does-not-reach-through-a-link ()
  ;; It runs outside the sandbox, on what a build had its hands on.
  (cfix--with nil (p outside)
    (wtimer--write outside "other.txt" "KEPT\n")
    (make-directory (expand-file-name "sub" outside))
    (wtimer--write outside "sub/inner.txt" "KEPT\n")
    (set-file-modes (expand-file-name "other.txt" outside) #o400)
    (set-file-modes (expand-file-name "sub/inner.txt" outside) #o400)
    (set-file-modes (expand-file-name "sub" outside) #o500)
    (unwind-protect
        (let ((dir (plist-get (cfix--make-copy p) :dir)))
          (make-symbolic-link (expand-file-name "other.txt" outside) (expand-file-name "link" dir))
          (make-symbolic-link (expand-file-name "sub" outside) (expand-file-name "dirlink" dir))
          (superchat-fix-delete dir)
          (cfix--no-copies)
          (should (equal "KEPT\n" (cfix--read-file outside "other.txt")))
          (should (equal "KEPT\n" (cfix--read-file outside "sub/inner.txt")))
          (should (= #o400 (file-modes (expand-file-name "other.txt" outside))))
          (should (= #o400 (file-modes (expand-file-name "sub/inner.txt" outside))))
          (should (= #o500 (file-modes (expand-file-name "sub" outside)))))
      (set-file-modes (expand-file-name "sub" outside) #o700))))

(ert-deftest compile-fix/a-secret-name-on-a-link-to-a-directory-goes-too ()
  (cfix--with nil (p outside)
    (make-symbolic-link outside (expand-file-name ".aws" p))
    (wtimer--write outside "kept.txt" "KEPT\n")
    (make-directory (expand-file-name ".ssh" p))
    (wtimer--write p ".ssh/id" "THE-SECRET\n")
    (let ((dir (plist-get (cfix--make-copy p) :dir)))
      (should dir)
      (should-not (file-symlink-p (expand-file-name ".aws" dir)))
      (should-not (file-exists-p (expand-file-name ".ssh" dir)))
      (should (equal "KEPT\n" (cfix--read-file outside "kept.txt")))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-link-in-a-copy-stays-a-link ()
  ;; What it points to outside is not copied in.
  (cfix--with nil (p outside)
    (wtimer--write outside "secret.txt" "THE-SECRET\n")
    (make-symbolic-link (expand-file-name "secret.txt" outside) (expand-file-name "link" p))
    (let ((dir (plist-get (cfix--make-copy p) :dir)))
      (should (file-symlink-p (expand-file-name "link" dir)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-project-too-large-is-not-copied ()
  (cfix--with nil (p)
    (wtimer--write p "a.txt" (make-string 4096 ?a))
    (let* ((superchat-fix-copy-megabytes 0)
           (made (cfix--make-copy p)))
      (should-not (plist-get made :dir))
      (should (string-match-p "superchat-fix-copy-megabytes" (plist-get made :why)))
      (cfix--no-copies))))

(ert-deftest compile-fix/the-limit-is-in-megabytes ()
  (cfix--with nil (p)
    (wtimer--write p "two-megabytes" (make-string (* 2 1024 1024) ?x))
    (let* ((superchat-fix-copy-megabytes 1)
           (why (plist-get (cfix--make-copy p) :why)))
      (should (string-match-p "the project is 2 MB, more than .* (1)" why)))
    (cfix--no-copies)
    (let* ((superchat-fix-copy-megabytes 3)
           (dir (plist-get (cfix--make-copy p) :dir)))
      (should dir)
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/a-copy-that-failed-leaves-nothing ()
  (cfix--with nil (p)
    (let ((why (plist-get (cfix--make-copy (expand-file-name "not-there" p)) :why)))
      (should (string-match-p "copying it failed" why)))
    (cfix--no-copies)))

(ert-deftest compile-fix/copying-leaves-the-project-as-it-was ()
  (cfix--with nil (p)
    (make-directory (expand-file-name ".git" p) t)
    (wtimer--write p ".env" "S\n")
    (wtimer--write p "a.txt" "A\n")
    (let ((before (directory-files-recursively p "" t)))
      (superchat-fix-delete (plist-get (cfix--make-copy p) :dir))
      (should (equal before (directory-files-recursively p "" t)))
      (should (equal "S\n" (cfix--read-file p ".env"))))))

(ert-deftest compile-fix/stopping-a-copy-leaves-nothing ()
  (cfix--with nil (p)
    (wtimer--write p "a.txt" "A\n")
    (let* ((called nil)
           (stop (superchat-fix-copy p (lambda (_made) (setq called t)))))
      (funcall stop)
      (cfix--no-copies)
      (sleep-for 0.2)
      (should-not called))))

(ert-deftest compile-fix/deleting-goes-only-for-a-copy ()
  ;; It runs rm -rf.
  (cfix--with nil (p)
    (wtimer--write p "a.txt" "A\n")
    (should-error (superchat-fix-delete p))
    (should-error (superchat-fix-delete "/"))
    (should-error (superchat-fix-delete (expand-file-name "~/")))
    (should (file-exists-p (expand-file-name "a.txt" p)))))

;;;; The sandbox -----------------------------------------------------------------

(defmacro cfix--with-copy (vars &rest body)
  "BODY with DIR a copy of the project P, the first of VARS."
  (declare (indent 1) (debug t))
  `(cfix--with nil ,vars
     (let ((dir (plist-get (cfix--make-copy ,(car vars)) :dir)))
       (unwind-protect
           (progn ,@body)
         (superchat-fix-delete dir)))))

(ert-deftest compile-fix/a-build-runs-in-the-copy-and-says-how-it-ended ()
  (cfix--with-copy (p)
    (let ((built (cfix--build dir p p "pwd; echo out; echo err >&2; exit 3")))
      (should (equal 3 (plist-get built :exit)))
      (should (string-match-p "out" (plist-get built :text)))
      (should (string-match-p "err" (plist-get built :text))))))

(ert-deftest compile-fix/a-build-runs-in-the-same-place-of-the-copy ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub" p) t)
    (wtimer--write p "sub/here.txt" "HERE\n")
    (let* ((dir (plist-get (cfix--make-copy p) :dir))
           (built (cfix--build dir p (expand-file-name "sub" p) "cat here.txt")))
      (should (equal 0 (plist-get built :exit)))
      (should (string-match-p "HERE" (plist-get built :text)))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/what-a-build-prints-speaks-of-the-project-not-the-copy ()
  (cfix--with-copy (p)
    (let ((built (cfix--build dir p p "pwd")))
      (should (string-match-p (regexp-quote (directory-file-name (file-truename p)))
                              (plist-get built :text)))
      (should-not (string-match-p "superchat-copy-" (plist-get built :text))))))

(ert-deftest compile-fix/a-build-can-write-in-the-copy ()
  (cfix--with-copy (p)
    (should (equal 0 (plist-get (cfix--build dir p p "echo x > made.txt") :exit)))
    (should (file-exists-p (expand-file-name "made.txt" dir)))
    (should-not (file-exists-p (expand-file-name "made.txt" p)))))

(ert-deftest compile-fix/a-build-cannot-write-outside-the-copy ()
  (cfix--with-copy (p outside)
    (let ((target (expand-file-name "pwned.txt" outside))
          (in-project (expand-file-name "pwned.txt" p)))
      (cfix--build dir p p (format "echo x > %s; echo x > %s; touch ~/superchat-pwned" target in-project))
      (should-not (file-exists-p target))
      (should-not (file-exists-p in-project))
      (should-not (file-exists-p (expand-file-name "~/superchat-pwned"))))))

(ert-deftest compile-fix/a-build-cannot-read-outside-the-copy ()
  (cfix--with-copy (p outside)
    (wtimer--write outside "secret.txt" "THE-SECRET\n")
    (wtimer--write p "only-in-project.txt" "THE-PROJECT-SECRET\n")
    (let ((built (cfix--build dir p p (format "cat %s; cat %s"
                                              (expand-file-name "secret.txt" outside)
                                              (expand-file-name "only-in-project.txt" p)))))
      (should-not (string-match-p "THE-SECRET" (plist-get built :text)))
      (should-not (string-match-p "THE-PROJECT-SECRET" (plist-get built :text))))))

(ert-deftest compile-fix/a-build-has-no-network-not-even-this-machine ()
  (cfix--with-copy (p)
    (let* ((reached nil)
           (server (make-network-process
                    :name "cfix-server" :server t :host "127.0.0.1" :service t
                    :sentinel (lambda (_process _event) (setq reached t))))
           (port (process-contact server :service)))
      (unwind-protect
          (let ((built (cfix--build dir p p (format "curl -sS --max-time 2 http://127.0.0.1:%d/; echo rc=$?" port))))
            (should (string-match-p "rc=[1-9]" (plist-get built :text)))
            (accept-process-output nil 0.2)
            (should-not reached))
        (delete-process server)))))

(ert-deftest compile-fix/a-build-sees-none-of-the-user-s-environment ()
  (cfix--with-copy (p)
    (let* ((process-environment (cons "SUPERCHAT_TEST_SECRET=THE-SECRET" process-environment))
           (built (cfix--build dir p p "env; echo HOME=$HOME")))
      (should-not (string-match-p "THE-SECRET" (plist-get built :text)))
      (should-not (string-match-p (regexp-quote (concat "HOME=" (getenv "HOME") "\n"))
                                  (plist-get built :text))))))

(ert-deftest compile-fix/the-policy-is-no-file-a-build-could-change ()
  (cfix--with-copy (p)
    (let ((argv (superchat-fix--command dir "true")))
      (should (member "-p" argv))
      (should-not (member "-f" argv)))))

(ert-deftest compile-fix/a-build-that-runs-too-long-is-stopped ()
  (cfix--with-copy (p)
    (let* ((superchat-fix-build-seconds 1)
           (start (float-time))
           (built (cfix--build dir p p "sleep 100")))
      (should (plist-get built :timed-out))
      (should (< (- (float-time) start) 10)))))

(ert-deftest compile-fix/what-a-build-left-running-is-stopped ()
  ;; It ignores the hangup of its terminal: only the group's kill stops it.
  (cfix--with-copy (p)
    (let* ((built (cfix--build dir p p "(trap '' HUP; exec sleep 100) & echo pid=$!"))
           (pid (and (string-match "pid=\\([0-9]+\\)" (plist-get built :text))
                     (string-to-number (match-string 1 (plist-get built :text))))))
      (should pid)
      (should (smoke--wait (lambda () (null (process-attributes pid))) 5)))))

(ert-deftest compile-fix/stopping-a-build-says-nothing-and-stops-it ()
  (cfix--with-copy (p)
    (let* ((called nil)
           (marker (expand-file-name "ran.txt" dir))
           (stop (superchat-fix-build dir p p "sleep 2; echo x > ran.txt"
                                      (lambda (_built) (setq called t)))))
      (sleep-for 0.2)
      (funcall stop)
      (sleep-for 2.5)
      (should-not called)
      (should-not (file-exists-p marker)))))

(ert-deftest compile-fix/stopping-a-build-stops-what-it-left-running ()
  (cfix--with-copy (p)
    (let* ((file (expand-file-name "pid.txt" dir))
           (stop (superchat-fix-build
                  dir p p "(trap '' HUP; exec sleep 100) & echo $! > pid.txt; sleep 100" #'ignore)))
      (should (smoke--wait (lambda () (and (file-exists-p file)
                                           (> (file-attribute-size (file-attributes file)) 0)))
                           5))
      (let ((pid (string-to-number (cfix--read-file dir "pid.txt"))))
        (should (process-attributes pid))
        (funcall stop)
        (should (smoke--wait (lambda () (null (process-attributes pid))) 5))))))

(ert-deftest compile-fix/stopping-a-build-leaves-no-timer-and-no-buffer ()
  (cfix--with-copy (p)
    (let* ((timers (copy-sequence timer-list))
           (buffers (buffer-list))
           (stop (superchat-fix-build dir p p "sleep 100" #'ignore)))
      (sleep-for 0.1)
      (funcall stop)
      (should-not (cl-set-difference timer-list timers))
      (should-not (cl-set-difference (buffer-list) buffers)))))

(ert-deftest compile-fix/a-build-has-a-place-of-its-own-for-temporary-files ()
  (cfix--with-copy (p)
    (let* ((built (cfix--build dir p p
                               "echo x > \"$TMPDIR/t\" && echo y > \"$HOME/h\" && echo \"tmp=$TMPDIR home=$HOME\""))
           (text (plist-get built :text)))
      (should (eql 0 (plist-get built :exit)))
      ;; And it is spoken of by a name that does not change from one try to the next.
      (should (string-match-p "tmp=/tmp/build/ home=/tmp/build" text))
      (should-not (string-match-p "superchat-copy-" text)))))

(ert-deftest compile-fix/a-build-sees-a-terminal-as-a-compilation-does ()
  (cfix--with-copy (p)
    (let* ((process-environment (cons "LANG=xx_YY.UTF-8" process-environment))
           (text (plist-get (cfix--build dir p p "[ -t 1 ] && echo a-terminal; echo \"term=$TERM lang=$LANG\"")
                            :text)))
      (should (string-match-p "a-terminal" text))
      (should (string-match-p "term=dumb lang=xx_YY.UTF-8" text)))))

(ert-deftest compile-fix/what-a-build-prints-comes-clean ()
  (cfix--with-copy (p)
    (should (equal "red\nplain\n"
                   (plist-get (cfix--build dir p p "printf '\\033[31mred\\033[0m\\n'; echo plain")
                              :text)))))

(ert-deftest compile-fix/a-build-that-reads-its-input-sees-the-end-of-it ()
  (cfix--with-copy (p)
    (let* ((superchat-fix-build-seconds 8)
           (built (cfix--build dir p p "cat; echo after-the-end")))
      (should-not (plist-get built :timed-out))
      (should (string-match-p "after-the-end" (plist-get built :text))))))

(ert-deftest compile-fix/a-line-ending-of-a-tool-s-own-is-not-kept ()
  ;; Emacs takes the mapping off the terminal, so this comes from the program.
  (cfix--with-copy (p)
    (should (equal "crlf\nplain\n"
                   (plist-get (cfix--build dir p p "printf 'crlf\\r\\n'; echo plain") :text)))))

(ert-deftest compile-fix/a-build-may-read-the-emacs-it-runs-under ()
  ;; A batch-byte-compile finds it through `invocation-name', wherever it is installed.
  (should (equal "/Applications/Emacs.app/"
                 (superchat-fix--app-of "/Applications/Emacs.app/Contents/MacOS/")))
  (should (equal "/usr/local/bin/" (superchat-fix--app-of "/usr/local/bin/")))
  (should (string-match-p
           (regexp-quote (format "(subpath %S)"
                                 (directory-file-name
                                  (file-truename
                                   (superchat-fix--app-of (file-truename invocation-directory))))))
           (superchat-fix--policy "/nowhere/"))))

(ert-deftest compile-fix/what-xcode-says-by-itself-is-not-what-the-build-said ()
  (cfix--with-copy (p)
    (should (equal "before\nafter\n"
                   (plist-get
                    (cfix--build
                     dir p p
                     (concat "echo before;"
                             "echo '2026-11-02 04:28:43.132 xcodebuild[7867:7734475]  DVTFilePathFSEvents: Failed to start fs event stream.';"
                             "echo '2026-11-02 04:28:43.270 xcodebuild[7867:7734420] [MT] DVTDeveloperPaths: Failed to get length';"
                             "echo after"))
                    :text)))))

(ert-deftest compile-fix/a-build-sees-a-terminal-of-the-size-it-is-told ()
  ;; A compiler breaks its lines at the width of the window of the compilation, and
  ;; cuts what it quotes: measured 2026-11-02, a long error made a C project differ
  ;; from its own failure, 5 of 5.
  (cfix--with-copy (p)
    (let (result)
      (superchat-fix-build dir p p "stty size" (lambda (built) (setq result built)) '(30 . 92))
      (should (smoke--wait (lambda () result) 10))
      (should (string-match-p "30 92" (plist-get result :text))))))

(ert-deftest compile-fix/a-long-output-is-not-all-kept ()
  (cfix--with-copy (p)
    (let* ((superchat-fix--output-bytes 1000)
           (text (plist-get (cfix--build dir p p "yes a-line | head -c 400000; echo THE-END") :text)))
      (should (string-match-p "THE-END" text))
      ;; What came in one read may be on top of the limit, until the next read.
      (should (< (length text) 20000)))))

(ert-deftest compile-fix/a-long-output-keeps-its-end ()
  (cfix--with-copy (p)
    (let ((built (cfix--build dir p p "i=0; while [ $i -lt 3000 ]; do echo line-$i-xxxxxxxxxxxxxxxx; i=$((i+1)); done; echo THE-END")))
      (should (string-match-p "THE-END" (plist-get built :text))))))

(ert-deftest compile-fix/a-build-that-cannot-start-says-so ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub" p))
    (let* ((dir (plist-get (cfix--make-copy p) :dir))
           (before (length (buffer-list))))
      (delete-directory (expand-file-name "sub" dir))
      (let ((built (cfix--build dir p (expand-file-name "sub" p) "true")))
        (should (eql 127 (plist-get built :exit)))
        (should (string-match-p "No such file" (plist-get built :text))))
      (should (= before (length (buffer-list))))
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/stopping-a-build-that-could-not-start-says-nothing ()
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub" p))
    (let* ((dir (plist-get (cfix--make-copy p) :dir))
           (said nil))
      (delete-directory (expand-file-name "sub" dir))
      (funcall (superchat-fix-build dir p (expand-file-name "sub" p) "true"
                                    (lambda (_built) (setq said t))))
      (sleep-for 0.1)
      (should-not said)
      (superchat-fix-delete dir))))

(ert-deftest compile-fix/the-toolchains-work-in-the-sandbox ()
  (cfix--with-copy (p)
    (let ((emacs (expand-file-name invocation-name invocation-directory)))
      (should (equal 0 (plist-get (cfix--build dir p p (format "%s --batch -Q --eval '(princ 1)'" emacs))
                                  :exit))))
    (when (executable-find "make")
      (wtimer--write dir "Makefile" "all:\n\t@echo made\n")
      (let ((built (cfix--build dir p p "make")))
        (should (equal 0 (plist-get built :exit)))
        (should (string-match-p "made" (plist-get built :text)))))))

(ert-deftest compile-fix/more-can-be-made-readable ()
  (cfix--with-copy (p outside)
    (wtimer--write outside "tool.txt" "A-TOOL\n")
    (let* ((superchat-fix-sandbox-read (list outside))
           (built (cfix--build dir p p (format "cat %s" (expand-file-name "tool.txt" outside)))))
      (should (string-match-p "A-TOOL" (plist-get built :text))))))

;;;; Setting one -----------------------------------------------------------------

(ert-deftest compile-fix/the-card-says-what-a-fix-is-granted ()
  (cfix--with nil (p)
    (let* ((view (tcmd--view p))
           (question (cdr (superchat-watch-decision (hm--session view) "RULE-TEXT" "compile_failed" nil t))))
      (should (string-match-p "copy" question))
      (should (string-match-p "no network" question))
      (should (string-match-p "RULE-TEXT" question))
      (should (string-match-p "apply" question)))))

(ert-deftest compile-fix/without-fix-the-card-says-nothing-of-a-copy ()
  (cfix--with nil (p)
    (let* ((view (tcmd--view p))
           (question (cdr (superchat-watch-decision (hm--session view) "RULE-TEXT" "compile_failed" nil))))
      (should-not (string-match-p "copy" question)))))

(ert-deftest compile-fix/a-fix-is-set-through-the-tool ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (superchat--tool-execution-context (list :session-id session :admitted-by 'asked)))
      (cl-letf (((symbol-function 'superchat-fix-available-p) (lambda () t)))
        (should-not (string-prefix-p
                     "Error"
                     (superchat-timer-tool-create "RULE-TEXT" nil nil nil nil nil nil nil
                                                  "compile_failed" t)))
        (should (plist-get (car (superchat-timer-rules)) :fix))))))

(ert-deftest compile-fix/a-fix-needs-an-event ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (superchat--tool-execution-context (list :session-id session :admitted-by 'asked)))
      (should (string-prefix-p
               "Error"
               (superchat-timer-tool-create "RULE-TEXT" "reminder" 5 nil nil nil nil nil nil t))))))

(ert-deftest compile-fix/where-there-is-no-sandbox-it-is-not-set ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (superchat--tool-execution-context (list :session-id session :admitted-by 'asked)))
      (cl-letf (((symbol-function 'superchat-fix-available-p) (lambda () nil)))
        (should-not (superchat-watch-decision session "RULE-TEXT" "compile_failed" nil t))
        (let ((answer (superchat-timer-tool-create "RULE-TEXT" nil nil nil nil nil nil nil
                                                   "compile_failed" t)))
          (should (string-match-p "sandbox" answer))
          (should (string-prefix-p "Error" answer)))
        (should-not (superchat-timer-rules))))))

(ert-deftest compile-fix/a-new-emacs-still-knows-it-may-fix ()
  (cfix--with nil (p)
    (let* ((view (tcmd--view p))
           (rule (cfix--rule view)))
      (should (plist-get rule :fix))
      (should (plist-get (car (superchat-timer-rules)) :fix)))))

(ert-deftest compile-fix/a-rule-without-fix-does-not-try ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let ((copied nil))
      (cl-letf* ((real (symbol-function 'superchat-fix-copy))
                 ((symbol-function 'superchat-fix-copy)
                  (lambda (&rest args) (setq copied t) (apply real args))))
        (cwatch--rule (tcmd--view p))
        (cfix--fail p)
        (should-not copied)))))

;;;; What comes of it ------------------------------------------------------------

(ert-deftest compile-fix/a-rule-that-leaves-nothing-out-has-no-way-to-say-nothing ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (superchat-watch-set (hm--session (tcmd--view p)) "RULE-TEXT" "compile_failed" t)
    (cfix--fail p)
    (should (member "edit_file" (cfix--tool-names 1)))
    (should-not (member "nothing_to_tell" (cfix--tool-names 1)))
    (let ((text (cwatch--all-text (hm--wire 1))))
      (should (string-match-p "they hear of every failure" text))
      (should-not (string-match-p "nothing_to_tell" text)))))

(ert-deftest compile-fix/a-look-whose-rule-leaves-nothing-out-has-no-way-either ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (superchat-watch-set (hm--session (tcmd--view p)) "RULE-TEXT" "compile_failed")
    (cfix--fail p)
    (should (member "read_file" (cfix--tool-names 1)))
    (should-not (member "nothing_to_tell" (cfix--tool-names 1)))
    (let ((text (cwatch--all-text (hm--wire 1))))
      (should (string-match-p "they hear of every failure" text))
      (should-not (string-match-p "nothing_to_tell" text)))))

(ert-deftest compile-fix/a-rule-that-leaves-something-out-can-say-nothing ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (member "nothing_to_tell" (cfix--tool-names 1)))
    (let ((text (cwatch--all-text (hm--wire 1))))
      (should (string-match-p "nothing_to_tell" text))
      (should-not (string-match-p "they hear of every failure" text)))))

(ert-deftest compile-fix/a-look-whose-rule-leaves-something-out-can-too ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (superchat-watch-set (hm--session (tcmd--view p)) "RULE-TEXT" "compile_failed" nil t)
    (cfix--fail p)
    (should (member "nothing_to_tell" (cfix--tool-names 1)))
    (should-not (string-match-p "they hear of every failure" (cwatch--all-text (hm--wire 1))))))

(ert-deftest compile-fix/a-call-of-a-tool-it-does-not-have-keeps-nothing-quiet ()
  (cfix--with '((:tool-uses (("nothing_to_tell" (why . "NO REASON") (left_out . t)))) (:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let ((rule (superchat-watch-set (hm--session (tcmd--view p)) "RULE-TEXT" "compile_failed" t)))
      (cfix--fail p)
      (should (equal "told" (tcmd--status (car (last (cwatch--results rule))))))
      (should (equal "THE CONCLUSION" (plist-get (cwatch--last rule) :note))))))

(ert-deftest compile-fix/a-reason-that-says-they-want-to-hear-goes-on ()
  ;; Measured 2026-11-02: "this is their code, so I should report it" went in why, and
  ;; the call had ended the look.
  (cfix--with '((:tool-uses (("nothing_to_tell" (why . "IT IS THEIR CODE") (left_out . :json-false))))
                (:text "THE CONCLUSION"))
      (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal "told" (tcmd--status (car (last (cwatch--results rule))))))
      (should (equal "THE CONCLUSION" (plist-get (cwatch--last rule) :note)))
      (should (string-match-p "answer them" (cwatch--tool-results (hm--wire 2)))))))

(ert-deftest compile-fix/a-call-that-does-not-say-goes-on-too ()
  (cfix--with '((:tool-uses (("nothing_to_tell" (why . "NO WORD ON IT")))) (:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal "THE CONCLUSION" (plist-get (cwatch--last rule) :note))))))

(ert-deftest compile-fix/a-reason-that-says-it-is-left-out-ends-it-there ()
  (cfix--with '((:tool-uses (("nothing_to_tell" (why . "NOT THEIR CODE") (left_out . t))))) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal "nothing" (tcmd--status (car (last (cwatch--results rule))))))
      (should (= 1 (length cfix--offered))))))

(ert-deftest compile-fix/the-card-says-how-often-it-tells ()
  (cfix--with nil (p)
    (let ((session (hm--session (tcmd--view p))))
      (should (string-match-p "It tells you each time" (superchat-watch--question session "T")))
      (should (string-match-p "It tells you only what these words ask for"
                              (superchat-watch--question session "T" nil t))))))

(ert-deftest compile-fix/a-new-emacs-still-knows-it-may-keep-quiet ()
  (cfix--with nil (p)
    (let* ((view (tcmd--view p))
           (quiet (cfix--rule view)))
      (should (plist-get quiet :quiet))
      (should (plist-get (seq-find (lambda (rule) (equal (plist-get quiet :id) (plist-get rule :id)))
                                   (superchat-timer-rules))
                         :quiet))
      (let ((plain (superchat-watch-set (hm--session view) "RULE-TEXT" "compile_failed" t)))
        (should-not (plist-get (seq-find (lambda (rule) (equal (plist-get plain :id) (plist-get rule :id)))
                                         (superchat-timer-rules))
                               :quiet))))))

(ert-deftest compile-fix/quiet-is-set-through-the-tool-and-asked-about ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (superchat--tool-execution-context (list :session-id session :admitted-by 'asked))
           (args (list "RULE-TEXT" nil nil nil nil nil nil nil "compile_failed" nil t)))
      (should (string-match-p "only what these words ask for"
                              (cdr (superchat-timer--command-decision
                                    (list :session-id session :args args)))))
      (should (string-match-p "when it has something to tell"
                              (apply #'superchat-timer-tool-create args)))
      (should (plist-get (car (superchat-timer-rules)) :quiet)))))

(ert-deftest compile-fix/quiet-goes-with-an-event ()
  (cfix--with nil (p)
    (tcmd--view p)
    (should (string-match-p "quiet goes with on"
                            (superchat-timer-tool-create "T" "reminder" 5 nil nil nil nil nil nil nil t)))))

(ert-deftest compile-fix/the-tools-of-a-fix-are-these ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (equal '("edit_file" "nothing_to_tell" "read_buffer" "read_file" "run_build")
                   (cfix--tool-names 1)))
    (cfix--no-copies)))

(ert-deftest compile-fix/a-fix-that-passes-is-offered-and-changes-nothing-yet ()
  (cfix--with (list cfix--edit '(:text "build.sh:2: it exited 2")) (p)
    (cfix--project p)
    (let* ((view (tcmd--view p))
           (rule (cfix--rule view))
           (before (with-current-buffer view (buffer-string))))
      (cfix--fail p)
      (should (equal '("told") (cwatch--statuses rule)))
      (let ((payload (cwatch--last rule)))
        (should (string-match-p "^-exit 2" (plist-get payload :fix-patch)))
        (should (string-match-p "^\\+exit 0" (plist-get payload :fix-patch)))
        (should (member "build.sh" (plist-get payload :fix-paths))))
      ;; Nothing of the user's has changed.
      (should (equal cfix--failing (cfix--read-file p "build.sh")))
      (should-not (find-buffer-visiting (expand-file-name "build.sh" p)))
      (should (string-match-p "a fix passes" (superchat-butler-line-text (car (tcmd--lines rule)))))
      (should (equal 1 (length (tcmd--lines rule))))
      (ignore before)
      (cfix--no-copies))))

(ert-deftest compile-fix/the-program-runs-it-once-more-and-a-fix-that-fails-is-dropped ()
  ;; The model says it is fixed.  That is not what decides.
  (cfix--with (list cfix--useless-edit '(:text "FIXED, it passes now")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal '("told") (cwatch--statuses rule)))
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :fix-patch))
        (should (plist-get payload :fix-dropped))
        (should (equal "FIXED, it passes now" (plist-get payload :note))))
      (should (string-match-p "did not pass" (superchat-butler-line-text (car (tcmd--lines rule)))))
      (should-not (string-match-p "a fix passes" (superchat-butler-line-text (car (tcmd--lines rule)))))
      (cfix--no-copies))))

(ert-deftest compile-fix/nothing-changed-is-not-run-again ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let ((builds 0))
      (cl-letf* ((real (symbol-function 'superchat-fix-build))
                 ((symbol-function 'superchat-fix-build)
                  (lambda (&rest args) (cl-incf builds) (apply real args))))
        (let ((rule (cfix--rule (tcmd--view p))))
          (cfix--fail p)
          ;; The one that showed the failure comes back in the copy.
          (should (= 1 builds))
          (should (equal '("told") (cwatch--statuses rule)))
          (should-not (plist-get (cwatch--last rule) :fix-patch))
          (should-not (plist-get (cwatch--last rule) :fix-dropped))
          (should (string-match-p "the model's words"
                                  (superchat-butler-line-text (car (tcmd--lines rule))))))))))

(ert-deftest compile-fix/nothing-to-tell-drops-what-was-changed ()
  (cfix--with (list cfix--edit (cwatch--nothing "NOT THEIR CODE")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal '("nothing") (cwatch--statuses rule)))
      (should-not (plist-get (cwatch--last rule) :fix-patch))
      (should-not (tcmd--lines rule))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-model-that-fails-leaves-no-fix ()
  (cfix--with (list cfix--edit '(:error (error . "boom"))) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should-not (plist-get (cwatch--last rule) :fix-patch))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-failure-that-does-not-come-back-in-the-copy-is-looked-into ()
  ;; A path to the real project: the sandbox refuses it, and that is another
  ;; failure, which a model must not go and "fix".
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let* ((rule (cfix--rule (tcmd--view p)))
           (real (expand-file-name "build.sh" p)))
      (cwatch--fail-and-settle p (format "sh %s" real))
      (should (equal '("nothing_to_tell" "read_buffer" "read_file") (cfix--tool-names 1)))
      (should (equal '("told") (cwatch--statuses rule)))
      (should (string-match-p "did not fail the same way"
                              (plist-get (cwatch--last rule) :no-fix)))
      (cfix--no-copies))))

(ert-deftest compile-fix/where-a-line-breaks-does-not-make-another-failure ()
  (let ((event (list :text "main.c:4: error: call to undeclared function\n      add [-Wimplicit]\nmake: Error 1")))
    (should (superchat-watch--comes-back-p
             event (list :exit 1 :text "main.c:4: error: call to undeclared function add [-Wimplicit]\nmake: Error 1\n")))
    (should (superchat-watch--comes-back-p
             (list :text "it took 0.12 s and failed")
             (list :exit 1 :text "it took 0.34 s and failed")))
    (should-not (superchat-watch--comes-back-p
                 event (list :exit 1 :text "main.c:5: error: another\nmake: Error 1\n")))
    (should-not (superchat-watch--comes-back-p
                 event (list :exit 0 :text "main.c:4: error: call to undeclared function add [-Wimplicit]\nmake: Error 1\n")))))

(ert-deftest compile-fix/the-window-of-the-compilation-goes-to-each-build ()
  (cfix--with (list cfix--edit '(:tool-uses (("run_build"))) '(:text "DONE")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (let ((built nil)
          (real-event (symbol-function 'superchat-watch--event))
          (real-build (symbol-function 'superchat-fix-build)))
      (cl-letf (((symbol-function 'superchat-watch--event)
                 (lambda (buffer msg)
                   (plist-put (funcall real-event buffer msg) :window '(30 . 92))))
                ((symbol-function 'superchat-fix-build)
                 (lambda (dir root directory command done &optional window)
                   (push window built)
                   (funcall real-build dir root directory command done window))))
        (cfix--fail p))
      ;; Whether it comes back, the model's, and the program's own.
      (should (equal '((30 . 92) (30 . 92) (30 . 92)) built)))))

(ert-deftest compile-fix/the-event-says-the-size-of-the-window ()
  (cfix--with nil (p)
    (let* ((buffer (generate-new-buffer "*a-compilation*"))
           (window (selected-window))
           (shown (set-window-buffer window buffer))
           (event (superchat-watch--event buffer "exited abnormally with code 2\n")))
      (ignore shown)
      (should (equal (cons (window-body-height window) (window-max-chars-per-line window))
                     (plist-get event :window)))
      (kill-buffer buffer)
      (should-not (plist-get (with-temp-buffer
                               (superchat-watch--event (current-buffer) ""))
                             :window)))))

(ert-deftest compile-fix/what-passes-in-the-copy-is-only-looked-into ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (wtimer--write p "build.sh" "echo 'the same words'\nif [ -n \"$CFIX_OUTSIDE\" ]; then exit 2; fi\n")
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--outside (cfix--fail p))
      (should (string-match-p "it passes there" (plist-get (cwatch--last rule) :no-fix)))
      (should-not (member "edit_file" (cfix--tool-names 1)))
      (cfix--no-copies))))

(ert-deftest compile-fix/what-takes-too-long-in-the-copy-is-only-looked-into ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (wtimer--write p "build.sh" "echo 'the same words'\nif [ -n \"$CFIX_OUTSIDE\" ]; then exit 2; fi\nsleep 60\n")
    (let ((rule (cfix--rule (tcmd--view p)))
          (superchat-fix-build-seconds 0.5))
      (cfix--outside (cfix--fail p))
      (should (string-match-p "too long there" (plist-get (cwatch--last rule) :no-fix)))
      (should-not (member "edit_file" (cfix--tool-names 1)))
      (cfix--no-copies))))

(ert-deftest compile-fix/where-the-sandbox-is-gone-a-rule-set-before-only-looks ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cl-letf (((symbol-function 'superchat-fix-available-p) #'ignore))
        (cfix--fail p))
      (should (string-match-p "no sandbox" (plist-get (cwatch--last rule) :no-fix)))
      (should-not (member "edit_file" (cfix--tool-names 1))))))

(ert-deftest compile-fix/a-command-that-starts-with-cd-runs-from-where-it-started ()
  ;; `compile' moves its buffer to where a first cd goes.  Run from there, it found no
  ;; such directory: measured 2026-11-02, 5 of 5.
  (cfix--with (list (cfix--edit-call "sub/build.sh" "exit 2" "exit 0") '(:text "DONE")) (p)
    (make-directory (expand-file-name "sub" p))
    (wtimer--write p "sub/build.sh" cfix--failing)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--fail-and-settle p "cd sub && sh build.sh")
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :no-fix))
        (should (member "sub/build.sh" (plist-get payload :fix-paths)))
        ;; And it says that this is where it ran: the command says the rest.
        (should (equal (file-name-as-directory p) (plist-get payload :directory)))))
    (should (string-match-p (concat "It ran in: " (regexp-quote (file-name-as-directory p)) "\n")
                            (cwatch--all-text (hm--wire 1))))))

(ert-deftest compile-fix/a-command-started-elsewhere-has-no-place-in-the-copy ()
  ;; The rule's business, because it builds here; but the copy has no such directory.
  (cfix--with '((:text "A CONCLUSION")) (p elsewhere)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--fail-and-settle elsewhere (format "cd %s && sh build.sh" p))
      (should (string-match-p "outside the project" (plist-get (cwatch--last rule) :no-fix)))
      (should-not (member "edit_file" (cfix--tool-names 1)))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-c-project-comes-back-in-the-copy-and-is-fixed ()
  ;; With the make and cc of /usr/bin, as a person has them.  In a new temporary directory
  ;; they ask Xcode where the tools are, and from the sandbox Xcode complains out loud:
  ;; measured 2026-11-02, that made every C project differ from its own failure.
  (skip-unless (and (file-executable-p "/usr/bin/xcrun")
                    (eql 0 (call-process "/usr/bin/xcrun" nil nil nil "--find" "cc"))))
  (cfix--with (list (cfix--edit-call "main.c" "return 0\n" "return 0;\n") '(:text "DONE")) (p)
    (wtimer--write p "main.c" "int main(void) {\n  return 0\n}\n")
    (wtimer--write p "Makefile" "all:\n\tcc -o main main.c\n")
    (let ((rule (cfix--rule (tcmd--view p)))
          (process-environment (cons "PATH=/usr/bin:/bin" process-environment))
          ;; A copy, three real builds of C in the sandbox and the model: measured
          ;; 2026-11-06, 17.5 seconds with five other test files running.
          (cwatch--settle-seconds 90))
      (cwatch--fail-and-settle p "make")
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :no-fix))
        (should (member "main.c" (plist-get payload :fix-paths)))))))

(ert-deftest compile-fix/a-project-too-large-is-looked-into ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let ((superchat-fix-copy-megabytes 0)
          (rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal '("nothing_to_tell" "read_buffer" "read_file") (cfix--tool-names 1)))
      (should (string-match-p "superchat-fix-copy-megabytes"
                              (plist-get (cwatch--last rule) :no-fix))))))

(ert-deftest compile-fix/enough-fixes-in-a-day-and-it-only-looks ()
  (cfix--with '((:text "ONE") (:text "TWO")) (p)
    (cfix--project p)
    (let ((superchat-fix-per-day 1)
          (rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (member "edit_file" (cfix--tool-names 1)))
      (wtimer--write p "build.sh" "echo 'build.sh:2: Error: another'\nexit 2\n")
      (cfix--fail p)
      (should-not (member "edit_file" (cfix--tool-names 2)))
      (should (plist-get (cwatch--last rule) :no-fix)))))

(ert-deftest compile-fix/fixes-older-than-a-day-do-not-count ()
  (cfix--with '((:text "ONE") (:text "TWO")) (p)
    (cfix--project p)
    (let ((superchat-fix-per-day 1)
          (rule (cfix--rule (tcmd--view p)))
          (now (superchat-timer--now)))
      (cfix--fail p)
      (wtimer--write p "build.sh" "echo 'build.sh:2: Error: another'\nexit 2\n")
      (cl-letf (((symbol-function 'superchat-timer--now) (lambda () (+ now 90000))))
        (cfix--fail p))
      (should (member "edit_file" (cfix--tool-names 2)))
      (should-not (plist-get (cwatch--last rule) :no-fix)))))

(ert-deftest compile-fix/only-a-try-counts-as-a-fix-of-the-day ()
  ;; What did not come back in the copy was looked into, and tried nothing.
  (cfix--with '((:text "ONE") (:text "TWO")) (p)
    (wtimer--write p "build.sh" "echo \"value=$CFIX_OUTSIDE\"\nexit 2\n")
    (let ((superchat-fix-per-day 1)
          (rule (cfix--rule (tcmd--view p))))
      (cfix--outside (cfix--fail p))
      (should (plist-get (cwatch--last rule) :no-fix))
      (should-not (member "edit_file" (cfix--tool-names 1)))
      (cfix--project p)
      (cfix--fail p)
      (should (member "edit_file" (cfix--tool-names 2))))))

(ert-deftest compile-fix/a-fix-counts-as-a-look ()
  (cfix--with '((:text "ONE")) (p)
    (cfix--project p)
    (let ((superchat-watch-looks-per-day 1)
          (rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (wtimer--write p "build.sh" "echo 'build.sh:2: Error: another'\nexit 2\n")
      (cfix--fail p)
      (should (equal '("told" "skipped") (cwatch--statuses rule))))))

(ert-deftest compile-fix/the-same-failure-again-tries-nothing ()
  (cfix--with '((:text "ONE") (:text "TWO")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((wires (cwatch--wires)))
        (cfix--fail p)
        (should (= wires (cwatch--wires)))
        (should (equal '("told" "skipped") (cwatch--statuses rule)))))))

(ert-deftest compile-fix/a-newer-failure-replaces-what-is-being-tried ()
  (cfix--with '((:text "OLD" :delay 2) (:text "NEW")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (should (smoke--wait (lambda () (cfix--copies)) 10))
      (wtimer--write p "build.sh" "echo 'build.sh:2: Error: another'\nexit 2\n")
      (cfix--fail p)
      (should (member "replaced" (cwatch--statuses rule)))
      (should (equal "told" (tcmd--status (car (last (cwatch--results rule))))))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-compile-that-passes-stops-what-is-being-tried ()
  (cfix--with '((:text "OLD" :delay 3)) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (should (smoke--wait (lambda () (cfix--copies)) 10))
      (cwatch--fail-and-settle p "true")
      (should (member "replaced" (cwatch--statuses rule)))
      (cfix--no-copies))))

(ert-deftest compile-fix/cancelling-the-rule-stops-what-is-being-tried ()
  (cfix--with '((:text "OLD" :delay 3)) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (should (smoke--wait (lambda () (cfix--copies)) 10))
      (superchat-timer-cancel (plist-get rule :id))
      (cfix--no-copies))))

(ert-deftest compile-fix/stopping-while-it-comes-back-stops-the-build-and-the-copy ()
  (cfix--with nil (p)
    (wtimer--write p "build.sh" cfix--slow-in-the-copy)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--outside (cwatch--compile p cfix--command))
      (cfix--started)
      (superchat-timer-cancel (plist-get rule :id))
      (cfix--no-builds)
      (cfix--no-copies))))

(ert-deftest compile-fix/stopping-while-the-model-thinks-leaves-no-copy ()
  (cfix--with '((:text "OLD" :delay 3)) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (cfix--asked 1)
      (should (cfix--copies))
      (superchat-timer-cancel (plist-get rule :id))
      (cfix--no-copies))))

(ert-deftest compile-fix/stopping-during-the-model-s-build-stops-it-and-the-copy ()
  (cfix--with (list (cfix--edit-call "marker" "" "x\n")
                    '(:tool-uses (("run_build")))
                    '(:text "DONE"))
      (p)
    (wtimer--write p "build.sh" cfix--slow-after-the-edit)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (cfix--asked 2)
      (cfix--started)
      (superchat-timer-cancel (plist-get rule :id))
      (cfix--no-builds)
      (cfix--no-copies))))

(ert-deftest compile-fix/stopping-during-the-program-s-build-stops-it-and-the-copy ()
  (cfix--with (list (cfix--edit-call "marker" "" "x\n") '(:text "DONE")) (p)
    (wtimer--write p "build.sh" cfix--slow-after-the-edit)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cwatch--compile p cfix--command)
      (cfix--asked 2)
      (cfix--started)
      (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 10))
      (superchat-timer-cancel (plist-get rule :id))
      (cfix--no-builds)
      (cfix--no-copies)
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/a-try-has-its-own-time-limit ()
  (cfix--with '((:text "LATE" :delay 4)) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p)))
          (superchat-fix-seconds 0.3))
      (cfix--fail p)
      (should-not (equal "LATE" (plist-get (cwatch--last rule) :note)))
      (should (plist-get (cwatch--last rule) :fix-tried))
      (cfix--no-copies))))

(ert-deftest compile-fix/the-time-limit-of-a-try-stops-the-model-s-build ()
  (cfix--with (list (cfix--edit-call "marker" "" "x\n")
                    '(:tool-uses (("run_build")))
                    '(:text "DONE"))
      (p)
    (wtimer--write p "build.sh" cfix--slow-after-the-edit)
    (let ((rule (cfix--rule (tcmd--view p)))
          (superchat-fix-seconds 1.5))
      (cfix--fail p)
      (cfix--no-builds)
      (cfix--no-copies)
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

;;;; The model's hands -----------------------------------------------------------

(defun cfix--edit-call (path old new)
  `(:tool-uses (("edit_file" (path . ,path) (old . ,old) (new . ,new)))))

(ert-deftest compile-fix/what-it-reads-is-the-copy-spoken-of-as-the-project ()
  (cfix--with (list (cwatch--read "build.sh") '(:text "A CONCLUSION")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (let ((text (cwatch--all-text (hm--wire 2))))
      (should (string-match-p "exit 2" text))
      (should-not (string-match-p "superchat-copy-" text)))))

(ert-deftest compile-fix/a-path-in-the-project-means-the-same-place-in-the-copy ()
  ;; It read that from the output; the real file stays as it is.
  (cfix--with (list (cfix--edit-call "PLACEHOLDER" "exit 2" "exit 0") '(:text "DONE")) (p)
    (cfix--project p)
    (setf (alist-get 'path (cdr (car (plist-get (car smoke--script) :tool-uses))))
          (expand-file-name "build.sh" p))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (plist-get (cwatch--last rule) :fix-patch))
      (should (equal cfix--failing (cfix--read-file p "build.sh"))))))

(ert-deftest compile-fix/it-cannot-edit-outside-the-copy ()
  (cfix--with nil (p outside)
    (cfix--project p)
    (wtimer--write outside "other.txt" "ORIGINAL\n")
    (setq smoke--script
          (list (cfix--edit-call (expand-file-name "other.txt" outside) "ORIGINAL" "CHANGED")
                '(:text "DONE")))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (equal "ORIGINAL\n" (cfix--read-file outside "other.txt")))
      (should-not (plist-get (cwatch--last rule) :fix-patch))
      (should (string-match-p "outside" (cwatch--tool-results (hm--wire 2)))))))

(ert-deftest compile-fix/it-cannot-go-up-out-of-the-copy ()
  (cfix--with (list (cfix--edit-call "../escaped.txt" "" "CHANGED") '(:text "DONE")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should-not (file-exists-p (expand-file-name "../escaped.txt" p)))
    (should (string-match-p "outside" (cwatch--tool-results (hm--wire 2))))
    (should-not (directory-files temporary-file-directory nil "escaped"))))

(ert-deftest compile-fix/it-cannot-edit-through-a-link ()
  (cfix--with nil (p outside)
    (cfix--project p)
    (wtimer--write outside "other.txt" "ORIGINAL\n")
    (make-symbolic-link (expand-file-name "other.txt" outside) (expand-file-name "link" p))
    (setq smoke--script (list (cfix--edit-call "link" "ORIGINAL" "CHANGED") '(:text "DONE")))
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (equal "ORIGINAL\n" (cfix--read-file outside "other.txt")))))

(ert-deftest compile-fix/it-cannot-make-a-file-named-like-a-secret ()
  (cfix--with (list (cfix--edit-call ".env" "" "A=1\n") '(:text "DONE")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (string-match-p "secrets" (cwatch--tool-results (hm--wire 2))))
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/it-cannot-edit-the-history ()
  ;; It is not in the copy, so this would be a new file there, and then a new file
  ;; in the user's repository: a config with a hook in it.
  (cfix--with (list (cfix--edit-call ".jj/repo/config.toml" "" "[aliases]\n")
                    (cfix--edit-call ".hg/hgrc" "" "[hooks]\n")
                    '(:text "DONE"))
      (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (string-match-p "secrets" (cwatch--tool-results (hm--wire 2))))
      (should (string-match-p "secrets" (cwatch--tool-results (hm--wire 3))))
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/history-of-any-kind-is-not-read-at-any-depth ()
  (dolist (name '(".git/config" ".jj/repo/x" ".hg/hgrc" ".svn/entries" "sub/.hg/hgrc" ".JJ/x"))
    (should (superchat-watch--unreadable-p name)))
  (dolist (name '("my.hg/x" ".jjx" "hg/x" "svn.txt" ".hgignore"))
    (should-not (superchat-watch--unreadable-p name))))

(ert-deftest compile-fix/a-name-cannot-write-lines-of-the-difference ()
  ;; A name goes in the header as it is.  With a newline it would be the difference
  ;; of another file, or for a `patch' that takes them, an ed script.
  (cfix--with (list (cfix--edit-call "x\n--- a/victim\n+++ b/victim\n@@ -1 +1 @@\n-before\n+FORGED\n"
                                     "" "whatever\n")
                    '(:text "DONE"))
      (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (string-match-p "control character" (cwatch--tool-results (hm--wire 2))))
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/an-edit-is-exact ()
  (cfix--with nil (d)
    (let ((file (expand-file-name "f.txt" d)))
      (wtimer--write d "f.txt" "abc ABC\n")
      ;; No pattern: this would match abc.
      (should (string-match-p "not found" (cadr (should-error (superchat-fix-edit file "a.c" "x")))))
      ;; Case counts: ABC is not abc, so this is there once.
      (superchat-fix-edit file "abc" "xyz")
      (should (equal "xyz ABC\n" (cfix--read-file d "f.txt")))
      ;; What goes in goes in as it is, in its own case, with no pattern in it.
      (superchat-fix-edit file "ABC" "aBc \\1 \\\\ &")
      (should (equal "xyz aBc \\1 \\\\ &\n" (cfix--read-file d "f.txt"))))))

(ert-deftest compile-fix/an-edit-says-what-to-do-about-a-missing-file ()
  (cfix--with nil (d)
    (should (string-match-p "give no old text"
                            (cadr (should-error
                                   (superchat-fix-edit (expand-file-name "gone.txt" d) "a" "b")))))))

(ert-deftest compile-fix/a-new-file-can-be-made-in-a-new-directory ()
  (cfix--with nil (d)
    (superchat-fix-edit (expand-file-name "new/deep/g.txt" d) "" "G\n")
    (should (equal "G\n" (cfix--read-file d "new/deep/g.txt")))))

(ert-deftest compile-fix/two-edits-of-a-file-are-one-difference-from-what-was-there ()
  (cfix--with (list (cfix--edit-call "f.txt" "A" "B") (cfix--edit-call "f.txt" "B" "C") cfix--edit
                    '(:text "DONE"))
      (p)
    (cfix--project p)
    (wtimer--write p "f.txt" "A\n")
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let* ((payload (cwatch--last rule))
             (patch (plist-get payload :fix-patch)))
        (should (= 1 (cl-count "f.txt" (plist-get payload :fix-paths) :test #'equal)))
        (should (= 1 (with-temp-buffer
                       (insert patch)
                       (how-many "^\\+\\+\\+ b/f.txt" (point-min) (point-max)))))
        (should (string-match-p "^-A$" patch))
        (should (string-match-p "^\\+C$" patch))
        (should-not (string-match-p "^.B$" patch))))))

(ert-deftest compile-fix/in-a-copy-no-other-buffer-can-be-read ()
  (cfix--with nil (p)
    (cfix--project p)
    (wtimer--write p "f.txt" "ON DISK\n")
    (let ((view (tcmd--view p))
          (buffer (find-file-noselect (expand-file-name "f.txt" p))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "THE-UNSAVED-TEXT\n"))
      (setq smoke--script (list '(:tool-uses (("read_buffer" (buffer . "f.txt")))) '(:text "DONE")))
      (cfix--rule view)
      (cfix--fail p)
      (let ((results (cwatch--tool-results (hm--wire 2))))
        (should (string-match-p "Only the buffer of the compilation" results))
        (should-not (string-match-p "THE-UNSAVED-TEXT" results)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/an-edit-must-find-its-text-exactly-once ()
  (cfix--with (list (cfix--edit-call "build.sh" "NOT THERE" "X") '(:text "DONE")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (string-match-p "not found" (cwatch--tool-results (hm--wire 2))))))

(ert-deftest compile-fix/an-edit-whose-text-is-there-twice-is-refused ()
  (cfix--with (list (cfix--edit-call "twice.txt" "A" "B") '(:text "DONE")) (p)
    (cfix--project p)
    (wtimer--write p "twice.txt" "A A\n")
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (string-match-p "2 times" (cwatch--tool-results (hm--wire 2))))))

(ert-deftest compile-fix/a-new-file-can-be-made-and-is-in-the-fix ()
  (cfix--with (list (cfix--edit-call "new.txt" "" "NEW-TEXT\n") cfix--edit '(:text "DONE")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (member "new.txt" (plist-get (cwatch--last rule) :fix-paths)))
      (should (string-match-p "^\\+NEW-TEXT" (plist-get (cwatch--last rule) :fix-patch)))
      (should-not (file-exists-p (expand-file-name "new.txt" p))))))

(ert-deftest compile-fix/a-file-that-exists-is-not-made-again ()
  (cfix--with (list (cfix--edit-call "build.sh" "" "REPLACED\n") '(:text "DONE")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (string-match-p "exists" (cwatch--tool-results (hm--wire 2))))))

(ert-deftest compile-fix/edits-have-a-limit ()
  (cfix--with nil (p)
    (cfix--project p)
    (wtimer--write p "many.txt" "A0\n")
    (let ((superchat-fix-edits 1))
      (setq smoke--script (list (cfix--edit-call "many.txt" "A0" "A1")
                                (cfix--edit-call "many.txt" "A1" "A2")
                                '(:text "DONE")))
      (cfix--rule (tcmd--view p))
      (cfix--fail p)
      (should (string-match-p "limit" (cwatch--tool-results (hm--wire 3)))))))

(ert-deftest compile-fix/run-build-runs-that-command ()
  (cfix--with '((:tool-uses (("run_build"))) (:text "A CONCLUSION")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (let ((results (cwatch--tool-results (hm--wire 2))))
      (should (string-match-p "it fails" results))
      (should (string-match-p "Exit code 2" results)))))

(ert-deftest compile-fix/run-build-takes-no-command ()
  ;; It has no argument: what a stranger's text asks it to run goes nowhere.
  (cfix--with '((:tool-uses (("run_build" (command . "touch MODEL-RAN-THIS"))))
                (:text "A CONCLUSION"))
      (p)
    (cfix--project p)
    (let ((ran nil))
      (cl-letf* ((real (symbol-function 'superchat-fix-build))
                 ((symbol-function 'superchat-fix-build)
                  (lambda (dir root directory command done &optional window)
                    (push command ran)
                    (funcall real dir root directory command done window))))
        (cfix--rule (tcmd--view p))
        (cfix--fail p)
        (should (equal (list cfix--command) (delete-dups ran)))
        (should-not (file-exists-p (expand-file-name "MODEL-RAN-THIS" p)))))))

(ert-deftest compile-fix/run-build-sees-the-edit ()
  (cfix--with (list cfix--edit '(:tool-uses (("run_build")))
                    '(:text "A CONCLUSION"))
      (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (should (string-match-p "Exit code 0" (cwatch--tool-results (hm--wire 3))))))

(ert-deftest compile-fix/run-build-says-when-it-took-too-long ()
  (cfix--with (list (cfix--edit-call "marker" "" "x\n")
                    '(:tool-uses (("run_build")))
                    '(:text "DONE"))
      (p)
    (wtimer--write p "build.sh" cfix--slow-after-the-edit)
    (let ((rule (cfix--rule (tcmd--view p)))
          (superchat-fix-build-seconds 0.5))
      (cfix--fail p)
      (should (string-match-p "did not finish" (cwatch--tool-results (hm--wire 3))))
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/two-builds-asked-at-once-do-not-run-at-once ()
  (cfix--with '((:tool-uses (("run_build") ("run_build"))) (:text "DONE")) (p)
    (wtimer--write p "build.sh"
                   (concat "if [ -f lock ]; then echo TWO-AT-ONCE; fi\ntouch lock\nsleep 0.3\nrm lock\n"
                           "echo 'build.sh:2: Error: it fails'\nexit 2\n"))
    (cfix--rule (tcmd--view p))
    (cfix--fail p)
    (let ((results (cwatch--tool-results (hm--wire 2))))
      (should (string-match-p "it fails" results))
      (should (string-match-p "running already" results))
      (should-not (string-match-p "TWO-AT-ONCE" results)))
    (cfix--no-builds)
    (cfix--no-copies)))

(ert-deftest compile-fix/builds-have-a-limit ()
  (cfix--with '((:tool-uses (("run_build"))) (:tool-uses (("run_build")))
                (:text "A CONCLUSION"))
      (p)
    (cfix--project p)
    (let ((superchat-fix-builds 1))
      (cfix--rule (tcmd--view p))
      (cfix--fail p)
      (should (string-match-p "limit" (cwatch--tool-results (hm--wire 3)))))))

(ert-deftest compile-fix/what-a-build-did-to-a-file-is-not-trusted ()
  ;; A build can swap the edited file for a link to something of the user's.  The
  ;; program reads it outside the sandbox.
  (cfix--with nil (p outside)
    (wtimer--write outside "secret.txt" "THE-SECRET\n")
    (wtimer--write p "build.sh"
                   (format "if grep -q CHANGED f.txt; then rm f.txt; ln -s %s f.txt; exit 0; fi\nexit 2\n"
                           (expand-file-name "secret.txt" outside)))
    (wtimer--write p "f.txt" "ORIGINAL\n")
    (setq smoke--script (list (cfix--edit-call "f.txt" "ORIGINAL" "CHANGED") '(:text "DONE")))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :fix-patch))
        (should (plist-get payload :fix-dropped))
        (should-not (string-match-p "THE-SECRET" (format "%S" payload)))))))

(ert-deftest compile-fix/a-build-that-took-away-its-own-place-drops-the-fix ()
  ;; A build has its hands on the copy.  The program's own build then cannot start,
  ;; and a signal there would leave nothing said and the rule busy.
  (cfix--with nil (p)
    (make-directory (expand-file-name "sub" p))
    (wtimer--write p "sub/build.sh"
                   "if grep -q CHANGED f.txt; then cd ..; rm -rf sub; exit 0; fi\necho 'it fails'\nexit 2\n")
    (wtimer--write p "sub/f.txt" "ORIGINAL\n")
    (setq smoke--script (list (cfix--edit-call "sub/f.txt" "ORIGINAL" "CHANGED")
                              '(:tool-uses (("run_build")))
                              '(:text "DONE")))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail (expand-file-name "sub" p))
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :fix-patch))
        (should (plist-get payload :fix-dropped))
        (should (equal "DONE" (plist-get payload :note))))
      (should (zerop (hash-table-count superchat-timer--writing)))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-file-the-build-made-unreadable-drops-the-fix ()
  (cfix--with nil (p)
    (wtimer--write p "build.sh"
                   "if grep -q CHANGED f.txt; then chmod 000 f.txt; exit 0; fi\necho 'it fails'\nexit 2\n")
    (wtimer--write p "f.txt" "ORIGINAL\n")
    (setq smoke--script (list (cfix--edit-call "f.txt" "ORIGINAL" "CHANGED") '(:text "DONE")))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :fix-patch))
        (should (plist-get payload :fix-dropped))
        (should (equal "DONE" (plist-get payload :note))))
      (should (zerop (hash-table-count superchat-timer--writing)))
      (cfix--no-copies))))

(ert-deftest compile-fix/a-change-on-a-line-that-is-not-text-is-dropped ()
  ;; A fix goes on the record as text.  Such a line would not come back as it was,
  ;; and the person would be told that it no longer matches.
  (cfix--with nil (p)
    ;; In a UTF-8 locale grep does not see a line that is not UTF-8.
    (wtimer--write p "build.sh" "LC_ALL=C grep -q FIXED latin.txt && exit 0\necho 'it fails'\nexit 2\n")
    (superchat-fix--write-bytes (expand-file-name "latin.txt" p)
                                (concat "caf" (unibyte-string #xE9) " BROKEN\n"))
    (setq smoke--script (list (cfix--edit-call "latin.txt" "BROKEN" "FIXED") '(:text "DONE")))
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((payload (cwatch--last rule)))
        (should-not (plist-get payload :fix-patch))
        (should (string-match-p "UTF-8" (plist-get payload :fix-dropped)))
        (should (equal "DONE" (plist-get payload :note)))))))

(ert-deftest compile-fix/such-a-line-elsewhere-in-the-file-is-kept-as-it-was ()
  (cfix--with nil (p)
    (let ((latin (expand-file-name "latin.txt" p))
          (body "1\n2\n3\n4\n5\n6\n7\n8\n"))
      (wtimer--write p "build.sh" "grep -q FIXED latin.txt && exit 0\necho 'it fails'\nexit 2\n")
      (superchat-fix--write-bytes latin (concat "caf" (unibyte-string #xE9) "\n" body "BROKEN\n"))
      (setq smoke--script (list (cfix--edit-call "latin.txt" "BROKEN" "FIXED") '(:text "DONE")))
      (let ((rule (cfix--rule (tcmd--view p))))
        (cfix--fail p)
        (should (plist-get (cwatch--last rule) :fix-patch))
        (superchat-butler-apply-fix (plist-get (car (tcmd--lines rule)) :run))
        (with-current-buffer (find-buffer-visiting latin)
          (save-buffer))
        (should (equal (concat "caf" (unibyte-string #xE9) "\n" body "FIXED\n")
                       (superchat-fix--bytes latin)))))))

(ert-deftest compile-fix/nothing-to-tell-with-words-builds-nothing-and-offers-nothing ()
  ;; The words and the call in one response: the call decides.
  (cfix--with (list cfix--edit '(:text "SOME WORDS" :tool-uses (("nothing_to_tell" (why . "NO REASON") (left_out . t))))) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((payload (cwatch--last rule)))
        (should (equal "nothing" (tcmd--status (car (last (cwatch--results rule))))))
        (should-not (plist-get payload :fix-patch)))
      (cfix--no-copies))))

(ert-deftest compile-fix/nothing-different-is-no-difference ()
  (cfix--with-copy (p)
    (wtimer--write dir "f.txt" "A\n")
    (should-not (superchat-fix-diff dir (list (cons "f.txt" "A\n"))))))

(ert-deftest compile-fix/a-file-the-build-removed-is-named ()
  (cfix--with-copy (p)
    (should (string-match-p "gone.txt is no file of the copy"
                            (plist-get (superchat-fix-diff dir (list (cons "gone.txt" "A\n"))) :why)))))

(ert-deftest compile-fix/a-file-that-became-a-link-in-the-copy-is-no-file-of-it ()
  (cfix--with-copy (p)
    (wtimer--write dir "g.txt" "G\n")
    (make-symbolic-link (expand-file-name "g.txt" dir) (expand-file-name "f.txt" dir))
    (should (string-match-p "no file of the copy"
                            (plist-get (superchat-fix-diff dir (list (cons "f.txt" "A\n"))) :why)))))

(ert-deftest compile-fix/a-directory-that-became-a-link-out-is-no-part-of-the-copy ()
  (cfix--with-copy (p outside)
    (make-directory (expand-file-name "sub" outside))
    (wtimer--write outside "sub/f.txt" "THE-SECRET\n")
    (make-symbolic-link (expand-file-name "sub" outside) (expand-file-name "sub" dir))
    (let ((fix (superchat-fix-diff dir (list (cons "sub/f.txt" "A\n")))))
      (should (string-match-p "no file of the copy" (plist-get fix :why)))
      (should-not (plist-get fix :patch)))))

(ert-deftest compile-fix/a-fix-too-large-is-dropped ()
  (cfix--with (list cfix--edit '(:text "DONE")) (p)
    (cfix--project p)
    (let ((superchat-fix-diff-bytes 10)
          (rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should-not (plist-get (cwatch--last rule) :fix-patch))
      (should (string-match-p "large" (plist-get (cwatch--last rule) :fix-dropped))))))

(ert-deftest compile-fix/an-edit-undone-is-no-fix ()
  (cfix--with (list (cfix--edit-call "other.txt" "A" "B") (cfix--edit-call "other.txt" "B" "A")
                    '(:text "DONE"))
      (p)
    (cfix--project p)
    (wtimer--write p "other.txt" "A\n")
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should-not (plist-get (cwatch--last rule) :fix-patch)))))

(ert-deftest compile-fix/the-prompt-says-what-it-may-do ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (cfix--rule (tcmd--view p) "RULE-TEXT")
    (cfix--fail p)
    (let ((text (cwatch--all-text (hm--wire 1))))
      (should (string-match-p "RULE-TEXT" text))
      (should (string-match-p "copy" text))
      (should (string-match-p "run_build" text))
      (should (string-match-p "Having no fix is not" text))
      (should (string-match-p "these tools work, in the copy" text))
      (should (string-match-p (regexp-quote (directory-file-name (file-truename p))) text))
      (should-not (string-match-p "superchat-copy-" text)))))

(ert-deftest compile-fix/in-a-copy-the-buffer-tool-names-the-only-buffer-it-reads ()
  ;; Measured 2026-11-02: told it could read a buffer visiting a file, a model tried
  ;; three of them in one response, and that stopped it with nothing said: 1 try of 70.
  (let ((description
         (lambda (look)
           (llm-tool-description
            (seq-find (lambda (tool) (equal "read_buffer" (llm-tool-name tool)))
                      (superchat-watch--tools look))))))
    (should-not (string-match-p "visiting" (funcall description
                                                    (make-superchat-watch--look
                                                     :id "x" :root "/tmp/" :copy "/tmp/"))))
    (should (string-match-p "visiting" (funcall description
                                                (make-superchat-watch--look
                                                 :id "x" :root "/tmp/"))))))

;;;; Applying it -----------------------------------------------------------------

(defun cfix--ready (p)
  "A rule on the project P with a fix ready for its failure; its line."
  (cfix--project p)
  (let ((rule (cfix--rule (tcmd--view p))))
    (cfix--fail p)
    (should (plist-get (cwatch--last rule) :fix-patch))
    (car (tcmd--lines rule))))

(ert-deftest compile-fix/seeing-a-fix-shows-the-difference-and-the-key ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let ((line (cfix--ready p)))
      (superchat-butler-show (plist-get line :run))
      (with-current-buffer superchat-butler-output-buffer-name
        (should (string-match-p "^-exit 2" (buffer-string)))
        (should (string-match-p "^\\+exit 0" (buffer-string)))
        (should (string-match-p "`a'" (buffer-string)))
        (should (string-match-p "THE CONCLUSION" (buffer-string)))
        (should (eq #'superchat-butler-apply-fix (key-binding (kbd "a"))))))))

(ert-deftest compile-fix/applying-goes-into-the-buffer-unsaved-and-to-the-review ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (file (expand-file-name "build.sh" p)))
      (superchat-butler-apply-fix (plist-get line :run))
      (let ((buffer (find-buffer-visiting file)))
        (should buffer)
        (with-current-buffer buffer
          (should (buffer-modified-p))
          (should (string-match-p "exit 0" (buffer-string))))
        ;; Disk is the user's `s'.
        (should (equal cfix--failing (cfix--read-file p "build.sh")))
        (let ((review (get-buffer superchat-changes-review-buffer-name)))
          (should review)
          (should (get-buffer-window review)))
        (with-current-buffer buffer (set-buffer-modified-p nil))))))

(ert-deftest compile-fix/applying-leaves-it-on-the-list-of-what-is-unsaved ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p)))
      (superchat-butler-apply-fix (plist-get line :run))
      (should (seq-find (lambda (item) (eq 'unsaved (plist-get item :kind)))
                        (superchat-pending-items)))
      (with-current-buffer (find-buffer-visiting (expand-file-name "build.sh" p))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/applying-is-one-undo ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (file (expand-file-name "build.sh" p))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (buffer-enable-undo)
        (setq buffer-undo-list nil))
      ;; The review takes the cursor, and with it the current buffer.
      (superchat-butler-apply-fix (plist-get line :run))
      (with-current-buffer buffer
        (should (string-match-p "exit 0" (buffer-string)))
        ;; The command loop puts this after every command.
        (undo-boundary)
        (let ((last-command nil))
          (undo))
        (should (equal cfix--failing (buffer-string)))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/two-places-in-a-file-are-still-one-undo ()
  (cfix--with (list (cfix--edit-call "f.txt" "FIRST" "ONE") (cfix--edit-call "f.txt" "LAST" "TWO")
                    cfix--edit '(:text "DONE"))
      (p)
    (let* ((original (concat "FIRST\n" (mapconcat #'number-to-string (number-sequence 1 30) "\n") "\nLAST\n"))
           (buffer nil)
           (line nil))
      (wtimer--write p "f.txt" original)
      (setq line (cfix--ready p))
      (setq buffer (find-file-noselect (expand-file-name "f.txt" p)))
      (with-current-buffer buffer
        (buffer-enable-undo)
        (setq buffer-undo-list nil))
      (superchat-butler-apply-fix (plist-get line :run))
      (with-current-buffer buffer
        (should (string-match-p "ONE" (buffer-string)))
        (should (string-match-p "TWO" (buffer-string)))
        (undo-boundary)
        (let ((last-command nil))
          (undo))
        (should (equal original (buffer-string)))
        (set-buffer-modified-p nil))
      (with-current-buffer (find-buffer-visiting (expand-file-name "build.sh" p))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/a-narrowed-buffer-takes-it-whole-and-stays-narrowed ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (buffer (find-file-noselect (expand-file-name "build.sh" p))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (narrow-to-region (point-min) (line-end-position)))
      (superchat-butler-apply-fix (plist-get line :run))
      (with-current-buffer buffer
        (should (buffer-narrowed-p))
        (widen)
        (should (equal "echo 'build.sh:2: Error: it fails'\nexit 0\n" (buffer-string)))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/a-read-only-buffer-is-refused-and-nothing-changes ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (buffer (find-file-noselect (expand-file-name "build.sh" p))))
      (with-current-buffer buffer
        (setq buffer-read-only t))
      (should-error (superchat-butler-apply-fix (plist-get line :run)) :type 'buffer-read-only)
      (with-current-buffer buffer
        (should (equal cfix--failing (buffer-string)))
        (should-not (buffer-modified-p))
        (setq buffer-read-only nil)))))

(ert-deftest compile-fix/one-read-only-buffer-stops-them-all ()
  ;; The first is changed before the second is looked at, unless all are looked at first.
  (cfix--with (list (cfix--edit-call "one.txt" "ONE" "CHANGED") (cfix--edit-call "two.txt" "TWO" "CHANGED")
                    cfix--edit '(:text "DONE"))
      (p)
    (wtimer--write p "one.txt" "ONE\n")
    (wtimer--write p "two.txt" "TWO\n")
    (let* ((line (cfix--ready p))
           (one (find-file-noselect (expand-file-name "one.txt" p)))
           (build (find-file-noselect (expand-file-name "build.sh" p))))
      ;; The fix has them in the order they were edited: this is the last.
      (with-current-buffer build
        (setq buffer-read-only t))
      (should-error (superchat-butler-apply-fix (plist-get line :run)) :type 'buffer-read-only)
      (with-current-buffer one
        (should (equal "ONE\n" (buffer-string)))
        (should-not (buffer-modified-p)))
      (with-current-buffer build
        (setq buffer-read-only nil)))))

(ert-deftest compile-fix/a-buffer-the-user-changed-elsewhere-still-takes-it ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (buffer (find-file-noselect (expand-file-name "build.sh" p))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (insert "# a line of the user's\n\n\n\n"))
      (superchat-butler-apply-fix (plist-get line :run))
      (with-current-buffer buffer
        (should (string-match-p "a line of the user's" (buffer-string)))
        (should (string-match-p "exit 0" (buffer-string)))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/what-no-longer-matches-is-refused-and-nothing-changes ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (buffer (find-file-noselect (expand-file-name "build.sh" p))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "something else entirely\n")
        (let ((text (buffer-string)))
          (let ((err (should-error (superchat-butler-apply-fix (plist-get line :run)))))
            (should (string-match-p "build.sh" (cadr err))))
          (should (equal text (buffer-string))))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/two-files-go-together-or-not-at-all ()
  (cfix--with (list (cfix--edit-call "other.txt" "A" "B") cfix--edit '(:text "THE CONCLUSION")) (p)
    (wtimer--write p "other.txt" "A\n")
    (let* ((line (cfix--ready p))
           (other (find-file-noselect (expand-file-name "other.txt" p)))
           (build (find-file-noselect (expand-file-name "build.sh" p))))
      (with-current-buffer build
        (erase-buffer)
        (insert "something else entirely\n"))
      (should-error (superchat-butler-apply-fix (plist-get line :run)))
      (with-current-buffer other
        (should (equal "A\n" (buffer-string)))
        (should-not (buffer-modified-p)))
      (with-current-buffer build (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/a-new-file-is-a-buffer-not-yet-on-disk ()
  (cfix--with (list (cfix--edit-call "new.txt" "" "NEW-TEXT\n") cfix--edit '(:text "DONE")) (p)
    (let* ((line (cfix--ready p))
           (file (expand-file-name "new.txt" p)))
      (superchat-butler-apply-fix (plist-get line :run))
      (should-not (file-exists-p file))
      (with-current-buffer (find-buffer-visiting file)
        (should (equal "NEW-TEXT\n" (buffer-string)))
        (set-buffer-modified-p nil)))
    (dolist (name '("build.sh"))
      (with-current-buffer (find-buffer-visiting (expand-file-name name p))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/the-review-is-told-which-is-a-new-file ()
  (cfix--with (list (cfix--edit-call "new.txt" "" "NEW-TEXT\n") cfix--edit '(:text "DONE")) (p)
    (let ((line (cfix--ready p))
          (rows nil))
      (cl-letf (((symbol-function 'superchat-changes-review)
                 (lambda (given _select) (setq rows given))))
        (superchat-butler-apply-fix (plist-get line :run)))
      (let ((by-name (lambda (name)
                       (seq-find (lambda (row) (equal name (buffer-name (plist-get row :buffer)))) rows))))
        (should (plist-get (funcall by-name "new.txt") :new-file))
        (should-not (plist-get (funcall by-name "build.sh") :new-file)))
      (dolist (name '("new.txt" "build.sh"))
        (with-current-buffer (find-buffer-visiting (expand-file-name name p))
          (set-buffer-modified-p nil))))))

(ert-deftest compile-fix/a-new-file-that-appeared-meanwhile-is-refused ()
  (cfix--with (list (cfix--edit-call "new.txt" "" "NEW-TEXT\n") cfix--edit '(:text "DONE")) (p)
    (let ((line (cfix--ready p)))
      (wtimer--write p "new.txt" "THE USER'S\n")
      (should-error (superchat-butler-apply-fix (plist-get line :run)))
      (should (equal "THE USER'S\n" (cfix--read-file p "new.txt")))
      (should-not (find-buffer-visiting (expand-file-name "build.sh" p))))))

(ert-deftest compile-fix/applying-twice-is-refused ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let ((line (cfix--ready p)))
      (superchat-butler-apply-fix (plist-get line :run))
      (should-error (superchat-butler-apply-fix (plist-get line :run)))
      (with-current-buffer (find-buffer-visiting (expand-file-name "build.sh" p))
        (should (string-match-p "exit 0" (buffer-string)))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/a-line-with-no-fix-has-nothing-to-apply ()
  (cfix--with '((:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (let ((failure (should-error (superchat-butler-apply-fix (plist-get (car (tcmd--lines rule)) :run))
                                   :type 'user-error)))
        (should (string-match-p "no fix to apply" (cadr failure)))))))

(ert-deftest compile-fix/a-path-that-leads-out-is-not-applied ()
  ;; What is on record is read back; a path in it is judged again.
  (cfix--with nil (p outside)
    (wtimer--write outside "other.txt" "ORIGINAL\n")
    (wtimer--write p "f.txt" "ORIGINAL\n")
    (wtimer--write p ".env" "ORIGINAL\n")
    (dolist (name (list (file-relative-name (expand-file-name "other.txt" outside) p)
                        (expand-file-name "f.txt" p)
                        ".env"))
      (should (string-match-p "not a place a fix may go"
                              (cadr (should-error
                                     (superchat-fix-apply-texts
                                      p (list name)
                                      (format "--- a/%s\n+++ b/%s\n@@ -1 +1 @@\n-ORIGINAL\n+CHANGED\n"
                                              name name))
                                     :type 'user-error)))))
    (should (equal "ORIGINAL\n" (cfix--read-file outside "other.txt")))
    (should (equal "ORIGINAL\n" (cfix--read-file p "f.txt")))
    (should (equal "ORIGINAL\n" (cfix--read-file p ".env")))))

(ert-deftest compile-fix/a-changed-line-beside-the-change-is-refused ()
  ;; `patch' would make it fit by leaving out the outer lines it knows: a guess.
  (cfix--with nil (p)
    (let ((patch "--- a/f.txt\n+++ b/f.txt\n@@ -2,7 +2,7 @@\n 2\n 3\n 4\n-OLD\n+NEW\n 6\n 7\n 8\n"))
      (wtimer--write p "f.txt" "1\n2\n3\n4\nOLD\n6\n7\n8\n9\n")
      (should (equal "1\n2\n3\n4\nNEW\n6\n7\n8\n9\n"
                     (cdar (superchat-fix-apply-texts p (list "f.txt") patch))))
      (wtimer--write p "f.txt" "1\nCHANGED\n3\n4\nOLD\n6\n7\n8\n9\n")
      (should-error (superchat-fix-apply-texts p (list "f.txt") patch) :type 'user-error)
      (should (equal "1\nCHANGED\n3\n4\nOLD\n6\n7\n8\n9\n" (cfix--read-file p "f.txt"))))))

(ert-deftest compile-fix/a-name-with-a-newline-is-not-applied ()
  ;; What is on record is read back; a name in it is judged again.
  (cfix--with nil (p)
    (wtimer--write p "victim" "before\n")
    (let ((failure (should-error
                    (superchat-fix-apply-texts
                     p (list "x\ny") "--- /dev/null\n+++ b/x\ny\n@@ -0,0 +1 @@\n+z\n")
                    :type 'user-error)))
      (should (string-match-p "not a place" (cadr failure))))
    (should (equal "before\n" (cfix--read-file p "victim")))))

(ert-deftest compile-fix/applying-counts-as-looked-at ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (run (plist-get line :run)))
      (should (cl-find run (superchat-changes--own-notices)
                       :key (lambda (notice) (plist-get notice :run)) :test #'equal))
      (superchat-butler-apply-fix run)
      (should-not (cl-find run (superchat-changes--own-notices)
                           :key (lambda (notice) (plist-get notice :run)) :test #'equal))
      (with-current-buffer (find-buffer-visiting (expand-file-name "build.sh" p))
        (set-buffer-modified-p nil)))))

(ert-deftest compile-fix/applying-is-on-record ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p)))
      (superchat-butler-apply-fix (plist-get line :run))
      (should (hm--events superchat-butler-session-id "butler.applied"))
      (with-current-buffer (find-buffer-visiting (expand-file-name "build.sh" p))
        (set-buffer-modified-p nil)))))

;;;; Where it is said ------------------------------------------------------------

(ert-deftest compile-fix/the-doorbell-has-a-button-for-the-fix ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let* ((view (tcmd--view p)))
      (with-selected-window (display-buffer view)
        (cfix--rule view)
        (cfix--fail p)
        (with-current-buffer view
          (should (string-match-p "\\[see fix\\]" (buffer-string))))))))

(ert-deftest compile-fix/no-fix-no-button ()
  (cfix--with '((:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let* ((view (tcmd--view p)))
      (with-selected-window (display-buffer view)
        (cfix--rule view)
        (cfix--fail p)
        (with-current-buffer view
          (should-not (string-match-p "\\[see fix\\]" (buffer-string)))
          (should (string-match-p "\\[bring in\\]" (buffer-string))))))))

(ert-deftest compile-fix/bringing-it-in-brings-the-difference ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (let* ((line (cfix--ready p))
           (view (get-buffer (seq-find (lambda (name) (string-prefix-p "*superchat:" name))
                                       (mapcar #'buffer-name (buffer-list))))))
      (superchat-butler-bring line view)
      (with-current-buffer view
        (let ((input (superchat-butler-message)))
          (should (string-match-p "THE CONCLUSION" input))
          (should (string-match-p "> \\+exit 0" input)))))))

(ert-deftest compile-fix/the-row-counts-the-fixes ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (should (string-match-p "1 fix" (plist-get (tbrief--timer-row rule) :detail))))))

(ert-deftest compile-fix/why-it-did-not-try-can-be-read ()
  (cfix--with '((:text "A CONCLUSION")) (p)
    (cfix--project p)
    (let* ((rule (cfix--rule (tcmd--view p)))
           (real (expand-file-name "build.sh" p)))
      (cwatch--fail-and-settle p (format "sh %s" real))
      (superchat-butler-show (plist-get (car (tcmd--lines rule)) :run))
      (with-current-buffer superchat-butler-output-buffer-name
        (should (string-match-p "did not fail the same way" (buffer-string)))))))

(ert-deftest compile-fix/a-fix-that-failed-can-be-read-too ()
  (cfix--with (list cfix--useless-edit '(:text "FIXED")) (p)
    (cfix--project p)
    (let ((rule (cfix--rule (tcmd--view p))))
      (cfix--fail p)
      (superchat-butler-show (plist-get (car (tcmd--lines rule)) :run))
      (with-current-buffer superchat-butler-output-buffer-name
        (should (string-match-p "did not pass" (buffer-string)))))))

;;;; Quiet, and on no one else's time ----------------------------------------------

(ert-deftest compile-fix/the-main-conversation-is-untouched ()
  (cfix--with (list cfix--edit '(:text "THE CONCLUSION")) (p)
    (cfix--project p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (before (length (superchat-db-tape-select
                            "SELECT id FROM tape WHERE session_id = ?" (list session)))))
      (with-current-buffer view
        (goto-char (point-max))
        (insert "half a sentence"))
      (cfix--rule view)
      (let ((after-rule (length (superchat-db-tape-select
                                 "SELECT id FROM tape WHERE session_id = ?" (list session)))))
        (cfix--fail p)
        (should (= after-rule (length (superchat-db-tape-select
                                       "SELECT id FROM tape WHERE session_id = ?" (list session)))))
        (ignore before))
      (with-current-buffer view
        (should (string-match-p "half a sentence"
                                (buffer-substring-no-properties superchat--prompt-start (point-max))))))))

(ert-deftest compile-fix/a-copy-does-not-outlive-emacs ()
  (cfix--with nil (p)
    (wtimer--write p "a.txt" "A\n")
    (let ((dir (plist-get (cfix--make-copy p) :dir)))
      (should (file-exists-p dir))
      (should (memq #'superchat-fix--delete-all kill-emacs-hook))
      (superchat-fix--delete-all)
      (cfix--no-copies))))

(provide 'compile-fix-regression)

;;; compile-fix-regression.el ends here
