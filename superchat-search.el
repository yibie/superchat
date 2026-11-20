;;; superchat-search.el --- The tools grep and find -*- lexical-binding: t; -*-

;;; Commentary:
;; Searching the files of a project: `grep' through ripgrep or grep, `find'
;; by glob.  Both are tools of `superchat-tools.el''s registry and run as its
;; tools run; this file only holds them, apart from reading and changing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-tools)

(defun superchat-tools--glob (glob)
  "A regexp for GLOB: * and ? stay within a directory, ** crosses them,
{a,b} is either, [...] is a class."
  (let ((pos 0) (depth 0) parts)
    (while (string-match "\\*\\*/\\|\\*\\*\\|[*?{},]\\|\\[!?]?[^]]*]" glob pos)
      (push (regexp-quote (substring glob pos (match-beginning 0))) parts)
      (let ((token (match-string 0 glob)))
        (push (pcase token
                ("**/" "\\(?:.*/\\)?") ("**" ".*") ("*" "[^/]*") ("?" "[^/]")
                ("{" (cl-incf depth) "\\(?:")
                ("}" (if (> depth 0) (progn (cl-decf depth) "\\)") "}"))
                ("," (if (> depth 0) "\\|" ","))
                (_ (if (string-prefix-p "[!" token) (concat "[^" (substring token 2)) token)))
              parts))
      (setq pos (match-end 0)))
    (concat "\\`" (apply #'concat (nreverse parts)) (regexp-quote (substring glob pos)) "\\'")))

(defun superchat-tools--files (dir glob deadline &optional listing)
  "The files under DIR that GLOB matches (all for nil), relative to DIR, sorted.
They are the project's files when DIR is in the project, as `project-files'
gives them, so that .gitignore counts; else every file under DIR, listed
until DEADLINE.  Nothing under .git.  With LISTING (`find'), directories too
where the listing has them, with a slash; without it (`grep'), no symbolic
link, which grep would follow out of DIR."
  (let* ((dir (file-name-as-directory dir))
         (project (and (file-in-directory-p dir default-directory)
                       (project-current nil default-directory)))
         (regexp (and glob (superchat-tools--glob glob)))
         (files (if project
                    (seq-filter (lambda (f) (string-prefix-p dir f)) (project-files project))
                  (directory-files-recursively
                   dir "" listing
                   (lambda (d) (and (< (float-time) deadline) (file-accessible-directory-p d)
                                    (not (equal (file-name-nondirectory d) ".git"))))))))
    (sort (delq nil (mapcar
                     (lambda (file)
                       (let ((name (file-relative-name file dir)))
                         (when (and (not (member ".git" (split-string name "/")))
                                    (or listing (not (file-symlink-p file)))
                                    (or (not regexp)
                                        (string-match-p regexp (if (string-search "/" glob)
                                                                   name
                                                                 (file-name-nondirectory name)))))
                           (if (and listing (file-directory-p file)) (concat name "/") name))))
                     files))
          #'string<)))


(defun superchat-tools--matches (text)
  "How many match lines of grep --null are in TEXT."
  (let ((n 0) (pos 0))
    (while (string-match "\0[0-9]+:" text pos)
      (setq n (1+ n) pos (match-end 0)))
    n))

(defun superchat-tools--grep (switches files limit deadline finish)
  "Run grep with SWITCHES on FILES, a batch at a time so that no command line
is too long, until more than LIMIT matches, DEADLINE or four times
`superchat-tool-max-chars' of output.  Call FINISH once with the output and why
it ended, as `superchat-tools--spawn' says, or with a message for an error
after the first batch.  Return a function that stops it."
  (let ((output "") (found 0) (status 1) stop)
    (cl-labels
        ((run (files)
           (let ((batch nil) (size 0))
             (while (and files (or (null batch) (< (+ size (length (car files))) 100000)))
               (cl-incf size (1+ (length (car files))))
               (push (pop files) batch))
             (setq stop (superchat-tools--spawn
                         (append '("grep") switches '("--") (nreverse batch))
                         (max 1 (- deadline (float-time)))
                         (lambda (text why)
                           (setq output (concat output text)
                                 found (superchat-tools--matches output)
                                 status (if (or (eql why 0) (eql status 0)) 0 why))
                           (if (and files (integerp why) (<= found limit)
                                    (< (length output) (* 4 superchat-tool-max-chars)))
                               (condition-case err (run files)
                                 (error (funcall finish output (error-message-string err))))
                             (funcall finish output (if (integerp why) status why))))
                         (lambda () (> (+ found (superchat-tools--matches (buffer-string)))
                                       limit)))))))
      (run files)
      (lambda () (funcall stop)))))

(defun superchat-tools--grep-result (output why limit deadline)
  "The result of `grep' from grep's OUTPUT and WHY it ended, LIMIT matches."
  (let ((found 0) more long lines)
    (dolist (line (split-string output "\n" t))
      (when (and (not more)
                 (string-match "\\`\\([^\0]*\\)\0\\([0-9]+\\)\\([:-]\\)\\(.*\\)" line))
        (let ((sep (match-string 3 line)) (text (match-string 4 line)))
          (if (and (equal sep ":") (= found limit))
              (setq more t)
            (when (equal sep ":") (cl-incf found))
            (when (> (length text) 500) (setq long t text (concat (substring text 0 500) "...")))
            (push (format "%s%s%s%s %s" (match-string 1 line) sep (match-string 2 line) sep text)
                  lines)))))
    (if (and (null lines) (integerp why) (> why 1))
        (error "grep exited with status %s" why)
      (let ((joined (superchat-tools--join (nreverse lines))))
        (superchat-tools--noted
         (if lines (car joined) "No matches found")
         (list (and more (format "%d matches limit reached. Use limit=%d for more, or refine pattern"
                                 limit (* 2 limit)))
               (or (cdr joined)
                   (and (eq why 'cut) (format "%d characters limit reached" (superchat-tools--room))))
               (and long "Some lines truncated to 500 chars. Use read to see full lines")
               (and (>= (float-time) deadline) superchat-tools--late)))))))

(superchat-tool-define
 "grep" :class 'read :async t
 :description (superchat-tools--describe
               "Search the contents of files for a POSIX extended regular expression (grep -E):"
               "no inline flags such as (?i); ignore case with ignore_case."
               "To see the lines around a match, give context rather than reading the file again."
               "Lines come as PATH:LINE: TEXT, context lines as PATH-LINE- TEXT."
               "The project's files respect .gitignore. Notices at the end say what was left out."
               superchat-tools--path-note)
 :args '((:name "pattern" :type string :description "Regular expression, or a fixed string with literal" :required t)
         (:name "path" :type string :description "File or directory to search, default the project")
         (:name "glob" :type string :description "Only files matching this glob, e.g. *.el or src/**/*.ts")
         (:name "ignore_case" :type boolean :description "Ignore case")
         (:name "literal" :type boolean :description "PATTERN is a fixed string")
         (:name "context" :type number :description "Lines before and after each match")
         (:name "limit" :type number :description "Most matches, default 100"))
 :function (lambda (args done)
             (let* ((path (superchat-tools--path (or (plist-get args :path) ".")))
                    (deadline (+ (float-time) 10))
                    (limit (superchat-tools--count (plist-get args :limit) 100))
                    (context (truncate (superchat-tools--number (plist-get args :context) 0)))
                    (files (cond ((file-directory-p path)
                                  (superchat-tools--files path (plist-get args :glob) deadline))
                                 ((file-exists-p path) (list (file-name-nondirectory path)))
                                 (t (error "No such file or directory: %s" (plist-get args :path)))))
                    ;; The path the permission was decided for: grep runs in it.
                    (default-directory (file-name-as-directory
                                        (if (file-directory-p path) path (file-name-directory path)))))
               (if (null files)
                   (funcall done (superchat-tools--grep-result "" 1 limit deadline))
                 (superchat-tools--grep
                  (append (list "-nHIs" "--null" (if (plist-get args :literal) "-F" "-E"))
                          (and (plist-get args :ignore_case) '("-i"))
                          (and (> context 0) (list (format "-C%d" context)))
                          (list "-e" (plist-get args :pattern)))
                  files limit deadline
                  (lambda (output why)
                    (if (stringp why)
                        (funcall done nil why)
                      (funcall done (superchat-tools--grep-result output why limit deadline)))))))))

(superchat-tool-define
 "find" :class 'read
 :description (superchat-tools--describe
               "Find files whose path matches a glob: without / it matches names at any depth"
               "(*.el), with / the path relative to PATH (src/**/*.ts). Paths come relative to PATH."
               "To find a file by name, give a name pattern such as *diary*: never walk directories by reading them."
               superchat-tools--path-note)
 :args '((:name "pattern" :type string :description "Glob" :required t)
         (:name "path" :type string :description "Directory to search, default the project")
         (:name "limit" :type number :description "Most results, default 1000"))
 :function (lambda (args)
             (let* ((path (superchat-tools--path (or (plist-get args :path) ".")))
                    (deadline (+ (float-time) 10))
                    (limit (superchat-tools--count (plist-get args :limit) 1000))
                    (files (if (file-directory-p path)
                               (superchat-tools--files path (plist-get args :pattern) deadline t)
                             (error "Not a directory: %s" (plist-get args :path))))
                    (joined (superchat-tools--join (take limit files))))
               (superchat-tools--noted
                (if files (car joined) "No files found matching pattern")
                (list (and (> (length files) limit)
                           (format "%d results limit reached. Use limit=%d for more, or refine pattern"
                                   limit (* 2 limit)))
                      (cdr joined)
                      (and (>= (float-time) deadline) superchat-tools--late))))))

(provide 'superchat-search)
;;; superchat-search.el ends here
