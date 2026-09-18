;;; m5-guidance-regression.el --- system contributions survive rewrites -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for the ordered system-contribution contract: a request
;; rewrite refreshes a tool-dependent system block in place, never by
;; searching the joined text, and a middleware that replaces the whole system
;; string takes it over.  The canonical smoke fixture drives the real driver,
;; Runtime, context collection and request waterfall with scripted provider
;; callbacks; no real model and no protected regression set loads.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/m5-guidance-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^m5-guidance/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar m5g--systems nil
  "Exact provider-visible system texts of this fixture, newest first.")

(defmacro m5g--fixture (script &rest body)
  "Run BODY with SCRIPT, capturing each provider request's system text."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (m5g--systems nil))
       (smoke--with-fixture ,script
         (let ((network (symbol-function 'smoke--fake-network)))
           (cl-letf (((symbol-function 'smoke--fake-network)
                      (lambda (mode provider prompt partial response error multi)
                        (push (copy-sequence (or (llm-chat-prompt-context prompt) ""))
                              m5g--systems)
                        (funcall network mode provider prompt
                                 partial response error multi))))
             ,@body))))))

(defun m5g--systems ()
  "Provider system texts of this fixture, oldest first."
  (reverse m5g--systems))

(defun m5g--count (needle text)
  "Count nonoverlapping literal NEEDLE occurrences in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun m5g--guidance (id text render)
  "Register a required global collector ID contributing TEXT with RENDER.
Return the collector's name for `superchat-context-unregister'."
  (superchat-context-register
   id
   (lambda (_envelope _state _tools)
     (list (plist-put (superchat-prompt-fragment
                       :owner 'm5g :id (symbol-name id) :slot 'system
                       :text text :budget-class 'hard)
                      :render-tools render)))
   :owner 'm5g :required t :scope superchat-scope-global)
  id)

(defmacro m5g--with-collectors (ids &rest body)
  "Run BODY, then unregister the global collectors named in IDS."
  (declare (indent 1) (debug t))
  `(unwind-protect (progn ,@body)
     (dolist (id ,ids)
       (superchat-context-unregister id superchat-scope-global))))

(defun m5g--submit (view text base &optional tools)
  "Submit TEXT from chat VIEW with explicit system BASE and TOOLS; settle it.
Return the Run's public status symbol."
  (let ((run (plist-get
              (superchat-service-call
               nil 'driver 'submit
               (append
                (list :surface 'external
                      :session-id (buffer-local-value 'superchat--session-id view)
                      :attendance 'attended
                      :content text
                      :system-prompt base
                      :context (list :host-buffer view)
                      :runtime-extras '(:record-conversation nil)
                      :sink (superchat-sink-discard))
                (and tools (list :tools tools))))
              :run-id)))
    (should (smoke--wait
             (lambda ()
               (and (not (member run (superchat-runtime-active-runs)))
                    (superchat-runtime--terminal-p
                     (plist-get (superchat-runtime-status run) :status))))
             20))
    (plist-get (superchat-runtime-status run) :status)))

(defmacro m5g--middleware (function &rest body)
  "Run BODY with FUNCTION registered on `before-provider-request'."
  (declare (indent 1) (debug t))
  (let ((off (make-symbol "off")))
    `(let ((,off (superchat-on nil 'before-provider-request 'm5g-middleware ,function)))
       (unwind-protect (progn ,@body)
         (funcall ,off)))))

(defconst m5g--tool-round '(:tool-uses (("read_buffer" (buffer . "*scratch*"))))
  "A scripted provider step that calls one tool, forcing another attempt.")

;;;; Pure contract ----------------------------------------------------------

(ert-deftest m5-guidance/assembler-system-is-the-materialized-contributions ()
  "The assembled `:system' is exactly the materialization of its contributions,
including an empty selected block, which keeps its position and separators."
  (let* ((result (superchat-prompt-assemble
                  ;; Priority sorts descending within a slot.
                  (list (superchat-prompt-fragment
                         :owner 'm5g :id "base" :slot 'system :text "BASE"
                         :priority 20 :budget-class 'hard)
                        (superchat-prompt-fragment
                         :owner 'm5g :id "empty" :slot 'system :text ""
                         :priority 10 :budget-class 'hard)
                        (superchat-prompt-fragment
                         :owner 'm5g :id "tail" :slot 'instruction :text "TAIL"
                         :priority 0 :budget-class 'hard)
                        (superchat-prompt-fragment
                         :owner 'm5g :id "user" :slot 'template :text "U"
                         :priority 0 :budget-class 'hard))
                  1000))
         (contributions (plist-get result :system-contributions)))
    (should (equal '("base" "empty" "tail")
                   (mapcar (lambda (c) (plist-get c :id)) contributions)))
    (should (equal "BASE\n\n\n\nTAIL" (plist-get result :system)))
    (should (equal (plist-get result :system)
                   (superchat-prompt-materialize-system contributions)))
    (should (cl-every (lambda (c) (null (plist-get c :render))) contributions))))

(ert-deftest m5-guidance/render-refreshes-dynamic-entries-in-place ()
  "Two dynamic entries with the same old text refresh to their own outputs in
order; a static entry with that text is untouched; one entry's new text
equalling another's old text cannot cascade."
  (let* ((contributions
          (list (list :id "base" :owner 'm5g :text "G" :render nil)
                (list :id "d1" :owner 'm5g :text "G"
                      :render (lambda (names) (if names "H" "d1-none")))
                (list :id "d2" :owner 'm5g :text "H"
                      :render (lambda (names) (if names "K" "d2-none")))
                (list :id "d3" :owner 'm5g :text "G"
                      :render (lambda (names) (if names "M" "d3-none")))))
         (system (superchat-prompt-materialize-system contributions))
         (guidance (superchat-tool-guidance-initial contributions system))
         (rendered (superchat--request-render-tool-guidance
                    (list :prompt "p" :system system :tools '("read_buffer"))
                    guidance)))
    (should (equal "G\n\nG\n\nH\n\nG" system))
    (should (equal "G\n\nH\n\nK\n\nM" (plist-get (car rendered) :system)))
    (should (equal "G\n\nH\n\nK\n\nM" (plist-get (cdr rendered) :system)))
    (should (equal '("base" "d1" "d2" "d3")
                   (mapcar (lambda (c) (plist-get c :id))
                           (plist-get (cdr rendered) :contributions))))
    ;; The original state is not mutated: a retired attempt's state stays
    ;; readable as it was.
    (should (equal "G" (plist-get (nth 1 contributions) :text)))
    (let ((none (superchat--request-render-tool-guidance
                 (list :prompt "p" :system system :tools 'none) guidance)))
      (should (equal "G\n\nd1-none\n\nd2-none\n\nd3-none"
                     (plist-get (car none) :system))))))

(ert-deftest m5-guidance/renderer-must-return-text ()
  (let* ((contributions (list (list :id "d" :owner 'm5g :text "T"
                                    :render (lambda (_names) 'not-text))))
         (guidance (superchat-tool-guidance-initial contributions "T")))
    (should-error (superchat--request-render-tool-guidance
                   (list :prompt "p" :system "T" :tools nil) guidance))))

(ert-deftest m5-guidance/no-renderer-means-no-guidance-state ()
  (should-not (superchat-tool-guidance-initial
               (list (list :id "s" :owner 'm5g :text "S" :render nil)) "S"))
  (should (superchat-tool-guidance-initial
           (list (list :id "s" :owner 'm5g :text "S" :render nil)
                 (list :id "d" :owner 'm5g :text "D" :render #'ignore))
           "S\n\nD")))

(ert-deftest m5-guidance/ownership-is-exact-text-identity ()
  (let ((guidance (list :contributions nil :system "G\n\nG")))
    (should (superchat-tool-guidance-owned-p guidance "G\n\nG"))
    (should-not (superchat-tool-guidance-owned-p guidance "G"))
    (should-not (superchat-tool-guidance-owned-p guidance "G\n\nG\n"))
    (should-not (superchat-tool-guidance-owned-p nil "G\n\nG"))))

;;;; Real pipeline: base collisions ------------------------------------------

(ert-deftest m5-guidance/explicit-base-survives-equal-guidance-text ()
  "A refreshed block whose old text also occurs inside the explicit base
changes only the block.  Collector runs once; the final surface is empty."
  (m5g--fixture '((:text "completed without tools"))
    (let* ((view (smoke--open-chat))
           (base "RGC_BASE_BEGIN\nRGC_GUIDE\nRGC_BASE_END")
           (collected 0))
      (m5g--with-collectors '(m5g-guidance)
        (superchat-context-register
         'm5g-guidance
         (lambda (_env _state _tools)
           (cl-incf collected)
           (list (plist-put (superchat-prompt-fragment
                             :owner 'm5g :id "m5g-guidance" :slot 'system
                             :text "RGC_GUIDE" :budget-class 'hard)
                            :render-tools (lambda (_names) "RGC_GUIDE_UPDATED"))))
         :owner 'm5g :required t :scope superchat-scope-global)
        (m5g--middleware
            (lambda (candidate next)
              (funcall next (plist-put candidate :tools 'none)))
          (should (eq 'completed
                      (m5g--submit view "Observe the explicit base." base
                                   '("read_buffer"))))
          (should (= 1 collected))
          (should (= 1 (length smoke--requests)))
          (should-not (plist-get (car smoke--requests) :tools))
          (let ((system (car (m5g--systems))))
            (should (= 1 (m5g--count base system)))
            (should (= 1 (m5g--count "RGC_GUIDE_UPDATED" system)))
            ;; The base's own RGC_GUIDE line plus nothing else: the block's
            ;; old text is gone from the block, not from the base.
            (should (= 2 (m5g--count "RGC_GUIDE" system)))))))))

(ert-deftest m5-guidance/two-equal-blocks-refresh-independently-through-the-pipeline ()
  "Two collectors contributing identical text each get their own rendering,
in registration priority order, with the static base untouched."
  (m5g--fixture '((:text "done"))
    (let ((view (smoke--open-chat))
          (base "SAME"))
      (m5g--with-collectors '(m5g-first m5g-second)
        (m5g--guidance 'm5g-first "SAME"
                       (lambda (names) (if names "FIRST:on" "FIRST:off")))
        (m5g--guidance 'm5g-second "SAME"
                       (lambda (names) (if names "SECOND:on" "SECOND:off")))
        (m5g--middleware
            (lambda (candidate next)
              (funcall next (plist-put candidate :tools 'none)))
          (should (eq 'completed (m5g--submit view "go" base '("read_buffer"))))
          (let ((system (car (m5g--systems))))
            (should (string-prefix-p "SAME\n\n" system))
            (should (= 1 (m5g--count "FIRST:off" system)))
            (should (= 1 (m5g--count "SECOND:off" system)))
            (should (= 1 (m5g--count "SAME" system)))
            (should (< (string-match-p "FIRST:off" system)
                       (string-match-p "SECOND:off" system)))))))))

;;;; Real pipeline: presence across attempts ---------------------------------

(ert-deftest m5-guidance/present-empty-then-nonempty-keeps-its-position ()
  "A block may render empty on one attempt and return on the next, at its own
position, without duplicating its neighbours."
  (m5g--fixture (list m5g--tool-round m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (base "M5G_BASE")
          (calls 0))
      (m5g--with-collectors '(m5g-toggle)
        (m5g--guidance 'm5g-toggle "M5G_TOGGLE"
                       (lambda (names) (if (member "read_buffer" names) "M5G_TOGGLE" "")))
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next (plist-put candidate :tools
                                       (if (= calls 2) 'none '("read_buffer")))))
          (should (eq 'completed (m5g--submit view "toggle" base '("read_buffer"))))
          (should (= 3 calls))
          (should (equal (list "M5G_BASE\n\nM5G_TOGGLE"
                               "M5G_BASE\n\n"
                               "M5G_BASE\n\nM5G_TOGGLE")
                         (m5g--systems))))))))

(ert-deftest m5-guidance/whole-replacement-ends-management ()
  "Base G plus block G, replaced by middleware with the bare string G while the
tools stay the same: the string is the caller's now.  A later tool change
neither rerenders nor revives anything, and the ambiguity is never guessed."
  (m5g--fixture (list m5g--tool-round m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (calls 0))
      (m5g--with-collectors '(m5g-g)
        (m5g--guidance 'm5g-g "G" (lambda (names) (if names "G" "G:none")))
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next
                       (pcase calls
                         (1 (should (equal "G\n\nG" (plist-get candidate :system)))
                            (plist-put candidate :system "G"))
                         (2 (plist-put candidate :tools 'none))
                         (_ (plist-put candidate :tools '("read_buffer"))))))
          (should (eq 'completed (m5g--submit view "ambiguous" "G" '("read_buffer"))))
          (should (= 3 calls))
          (should (equal '("G" "G" "G") (m5g--systems))))))))

(ert-deftest m5-guidance/removed-block-never-returns ()
  "Middleware deletes the block by editing the string on an attempt whose tools
are unchanged; a later tool change does not append the block again."
  (m5g--fixture (list m5g--tool-round m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (base "M5G_BASE")
          (calls 0))
      (m5g--with-collectors '(m5g-removed)
        (m5g--guidance 'm5g-removed "M5G_BLOCK"
                       (lambda (names) (if names "M5G_BLOCK" "")))
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next
                       (pcase calls
                         (1 (plist-put candidate :system base))
                         (2 (plist-put candidate :tools 'none))
                         (_ (plist-put candidate :tools '("read_buffer"))))))
          (should (eq 'completed (m5g--submit view "remove" base '("read_buffer"))))
          (should (= 3 calls))
          (should (equal (list base base base) (m5g--systems))))))))

(ert-deftest m5-guidance/unchanged-attempt-keeps-managing ()
  "An attempt that touches neither tools nor system keeps the state, so a
later tool change still refreshes the block."
  (m5g--fixture (list m5g--tool-round m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (calls 0))
      (m5g--with-collectors '(m5g-keep)
        (m5g--guidance 'm5g-keep "KEEP:on"
                       (lambda (names) (if names "KEEP:on" "KEEP:off")))
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next (if (= calls 3)
                                (plist-put candidate :tools 'none)
                              candidate)))
          (should (eq 'completed (m5g--submit view "keep" "B" '("read_buffer"))))
          (should (equal '("B\n\nKEEP:on" "B\n\nKEEP:on" "B\n\nKEEP:off")
                         (m5g--systems))))))))

(ert-deftest m5-guidance/renderer-sees-only-the-accepted-final-names ()
  "Renderers receive the final accepted tool names, nil for `none', never the
candidate the middleware started from."
  (m5g--fixture (list m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (seen nil)
          (calls 0))
      (m5g--with-collectors '(m5g-names)
        (m5g--guidance 'm5g-names "NAMES:initial"
                       (lambda (names)
                         (push (copy-sequence names) seen)
                         (format "NAMES:%s" (if names (string-join names ",") "none"))))
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next (plist-put candidate :tools
                                       (if (= calls 1) '("read_buffer") 'none))))
          (should (eq 'completed
                      (m5g--submit view "names" "B" '("read_buffer" "list_buffers"))))
          (should (equal '(("read_buffer") nil) (reverse seen)))
          (should (equal '("B\n\nNAMES:read_buffer" "B\n\nNAMES:none")
                         (m5g--systems)))
          (should (equal '(("read_buffer") nil)
                         (mapcar (lambda (r) (plist-get r :tools))
                                 (reverse smoke--requests)))))))))

(ert-deftest m5-guidance/no-renderer-no-rewrite-of-static-text ()
  "With only static contributions a tool change leaves the system text alone."
  (m5g--fixture (list m5g--tool-round '(:text "done"))
    (let ((view (smoke--open-chat))
          (calls 0))
      (m5g--with-collectors '(m5g-static)
        (m5g--guidance 'm5g-static "STATIC" nil)
        (m5g--middleware
            (lambda (candidate next)
              (cl-incf calls)
              (funcall next (plist-put candidate :tools
                                       (if (= calls 1) '("read_buffer") 'none))))
          (should (eq 'completed (m5g--submit view "static" "B" '("read_buffer"))))
          (should (equal '("B\n\nSTATIC" "B\n\nSTATIC") (m5g--systems))))))))

(provide 'm5-guidance-regression)
;;; m5-guidance-regression.el ends here
