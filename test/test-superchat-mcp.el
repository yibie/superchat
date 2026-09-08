;;; test-superchat-mcp.el --- MCP tool collection contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Two layers, because either one alone would have missed the defect this
;; file exists for.
;;
;; The CONTRACT layer asks the live `mcp-hub-get-all-tool' what it actually
;; accepts.  Superchat declared it as `(asyncp categoryp errorHandle)' -- a
;; signature no release of mcp.el has ever had -- and called it positionally
;; as `(mcp-hub-get-all-tool nil t nil)' against a `&key' lambda list, so
;; every call signalled "Keyword argument nil not one of (:asyncp
;; :categoryp)" and `superchat-mcp-get-tools' returned nil for everyone.
;; The suite never noticed, because it stubs Superchat's OWN
;; `superchat-mcp-get-tools' to nil (test/test-llm-backend.el:203, :545,
;; :2987).  Stubbing our side of a boundary cannot detect a defect that
;; lives in how we address the other side.  This layer skips, rather than
;; fails, when mcp.el is not installed.
;;
;; The BEHAVIOUR layer stubs `mcp-hub-get-all-tool' instead, so it runs on
;; any machine, and pins the two rules the repair depends on: the hub is
;; queried once for the whole hub, and a tool is attributed to a server by
;; its `:category', never by `:server'.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat-mcp)
;; The command-surface layer below dispatches through
;; `superchat--handle-default-command' and reads `superchat--builtin-commands',
;; so the module owning them has to be loaded when this file runs alone.
(require 'superchat)

;; Mark the mcp-hub externals special for this file, exactly as
;; `superchat-mcp.el' does: without this the `let' bindings below would be
;; lexical on a machine where mcp.el is absent, and the product code -- which
;; reads them dynamically -- would never see them.
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

;; ═══════════════════════════════════════════════════════════
;; Contract layer -- against the live mcp.el, when present
;; ═══════════════════════════════════════════════════════════

(defconst test-superchat-mcp--sibling-checkout
  (expand-file-name "../../mcp.el" (file-name-directory load-file-name))
  "The sibling `mcp.el' working copy, if this machine has one.
Located the same way `test/run-tests.el' locates the sibling TextUI
checkout, so the contract layer is real coverage here rather than a
permanent skip.")

(defun test-superchat-mcp--hub-available-p ()
  "Load `mcp-hub' if this machine has it; return non-nil on success.
Searches the existing `load-path' first, then the sibling checkout.  The
sibling directory is added only for the duration of the `require', so a
skipped run leaves `load-path' untouched."
  (or (featurep 'mcp-hub)
      (let ((load-path (if (file-directory-p test-superchat-mcp--sibling-checkout)
                           (cons test-superchat-mcp--sibling-checkout load-path)
                         load-path)))
        (require 'mcp-hub nil t))))

(defmacro test-superchat-mcp--with-live-hub (&rest body)
  "Run BODY with the real `mcp-hub' loaded, or skip the test."
  (declare (indent 0))
  `(progn
     (unless (test-superchat-mcp--hub-available-p)
       (ert-skip "mcp.el is not installed and no sibling checkout was found"))
     ,@body))

(ert-deftest mcp/hub-collector-is-not-a-three-argument-function ()
  "The live collector takes keywords, not three positional arguments.
A `&key' lambda list expands to `(&rest ARGS)', so `func-arity' reports
`(0 . many)'.  The signature Superchat used to declare would have to
report `(3 . 3)'."
  (test-superchat-mcp--with-live-hub
    (let ((arity (func-arity 'mcp-hub-get-all-tool)))
      (should (equal 0 (car arity)))
      (should (eq 'many (cdr arity))))))

(ert-deftest mcp/hub-collector-advertises-categoryp ()
  "`:categoryp' is a real keyword of the live collector, not our invention.
`cl-defun' hides the keyword list from `help-function-arglist', which
reports the expansion's `(&rest --cl-rest--)'; the advertised lambda list
survives in the trailing `(fn ...)' line of the docstring, which is what
`help-split-fundoc' returns."
  (test-superchat-mcp--with-live-hub
    (require 'help-fns)
    (let* ((doc (documentation 'mcp-hub-get-all-tool t))
           (usage (car (help-split-fundoc doc 'mcp-hub-get-all-tool)))
           (case-fold-search t))
      (should usage)
      (should (string-match-p "&key" usage))
      (should (string-match-p "categoryp" usage)))))

(ert-deftest mcp/hub-collector-accepts-the-call-form-superchat-uses ()
  "The exact call `superchat-mcp-get-tools' makes is accepted.
With no connected servers this is a pure call-form check: the keyword
form must return nil rather than signal.  The positional form Superchat
used until now is asserted to signal, so the bug stays reproducible
instead of becoming folklore."
  (test-superchat-mcp--with-live-hub
    (let ((mcp-server-connections (make-hash-table :test #'equal)))
      (should-not (mcp-hub-get-all-tool :categoryp t))
      (should-error (mcp-hub-get-all-tool nil t nil) :type 'error))))

;; ═══════════════════════════════════════════════════════════
;; Behaviour layer -- stubbed hub, runs without mcp.el
;; ═══════════════════════════════════════════════════════════

(defvar test-superchat-mcp--hub-calls 0
  "How many times the stubbed `mcp-hub-get-all-tool' was called.")

(defvar test-superchat-mcp--hub-args nil
  "The argument list of the last stubbed `mcp-hub-get-all-tool' call.")

(defun test-superchat-mcp--tool (server name &rest extra)
  "Build one stub tool plist as the hub would hand it over.
Shape follows `mcp-make-text-tool' plus the `:category' that
`mcp-hub-get-all-tool' adds when called with `:categoryp'.  EXTRA is
appended verbatim, which is how a test plants a misleading `:server'."
  (append (list :function #'ignore
                :name name
                :description (format "%s from %s" name server)
                :args nil
                :async nil
                :category (format "mcp-%s" server))
          extra))

(defmacro test-superchat-mcp--with-hub (servers tools &rest body)
  "Run BODY against a stubbed MCP hub.
SERVERS is the list of configured, connected server names.  TOOLS is
evaluated on each hub call and is what the whole hub returns, regardless
of the keyword arguments it is handed -- mirroring the real function,
which walks every connected server itself.  Calls are counted in
`test-superchat-mcp--hub-calls' and the last argument list is kept in
`test-superchat-mcp--hub-args'."
  (declare (indent 2))
  (let ((names (gensym "server-names"))
        (real-featurep (gensym "real-featurep")))
    `(let* ((,names ,servers)
            (,real-featurep (symbol-function 'featurep))
            (mcp-hub-servers (mapcar #'list ,names))
            (mcp-server-connections
             (let ((table (make-hash-table :test #'equal)))
               (dolist (name ,names table)
                 (puthash name (list :stub-connection name :status 'connected)
                          table))))
            (superchat-mcp-servers nil)
            (test-superchat-mcp--hub-calls 0)
            (test-superchat-mcp--hub-args nil))
       (cl-letf (((symbol-function 'featurep)
                  ;; `superchat-mcp-available-p' gates on `(featurep
                  ;; \='mcp-hub)', and since Emacs 31 `featurep' answers from
                  ;; a mark `provide' leaves on the symbol rather than from
                  ;; the `features' list, so binding `features' no longer
                  ;; fakes an installed package.  Answer for `mcp-hub' only
                  ;; and hand every other question to the real function.
                  (lambda (feature &optional subfeature)
                    (if (and (eq feature 'mcp-hub) (null subfeature))
                        t
                      (funcall ,real-featurep feature subfeature))))
                 ((symbol-function 'mcp-hub-get-all-tool)
                  (lambda (&rest keys)
                    (setq test-superchat-mcp--hub-calls
                          (1+ test-superchat-mcp--hub-calls))
                    (setq test-superchat-mcp--hub-args keys)
                    ,tools))
                 ;; Mirror the public mcp.el lifecycle surface: stopping a
                 ;; server changes its connection status rather than deleting
                 ;; the connection from the table.
                 ((symbol-function 'mcp--status)
                  (lambda (connection)
                    (plist-get connection :status)))
                 ((symbol-function 'mcp-stop-server)
                  (lambda (name)
                    (when-let* ((connection
                                 (gethash name mcp-server-connections)))
                      (puthash name
                               (plist-put (copy-sequence connection)
                                          :status 'stop)
                               mcp-server-connections)))))
         ,@body))))

(defun test-superchat-mcp--names ()
  "Return the names of the tools `superchat-mcp-get-tools' collected."
  (mapcar (lambda (tool) (plist-get tool :name))
          (superchat-mcp-get-tools)))

(ert-deftest mcp/hub-is-queried-once-for-the-whole-hub ()
  "One call for the hub, not one call per active server.
`mcp-hub-get-all-tool' maphashes every connected server itself, so a
per-server loop yields the full tool set once per server."
  (test-superchat-mcp--with-hub
      '("alpha" "beta" "gamma")
      (list (test-superchat-mcp--tool "alpha" "read")
            (test-superchat-mcp--tool "beta" "read")
            (test-superchat-mcp--tool "gamma" "write"))
    (let ((tools (superchat-mcp-get-tools)))
      (should (= 1 test-superchat-mcp--hub-calls))
      ;; Three servers, three tools -- not nine.
      (should (= 3 (length tools))))))

(ert-deftest mcp/hub-is-queried-with-categoryp ()
  "The hub is asked for categories, because that is the only attribution.
Without `:categoryp' mcp.el sets no `:category', and every tool becomes
unattributable."
  (test-superchat-mcp--with-hub
      '("alpha")
      (list (test-superchat-mcp--tool "alpha" "read"))
    (superchat-mcp-get-tools)
    (should (plist-get test-superchat-mcp--hub-args :categoryp))))

(ert-deftest mcp/same-tool-name-on-two-servers-stays-distinct ()
  "Two servers exposing one name produce two entries, not a collision."
  (test-superchat-mcp--with-hub
      '("alpha" "beta")
      (list (test-superchat-mcp--tool "alpha" "read_file")
            (test-superchat-mcp--tool "beta" "read_file"))
    (let ((names (test-superchat-mcp--names)))
      (should (equal '("mcp:alpha:read_file" "mcp:beta:read_file") names))
      (should (= (length names)
                 (length (delete-dups (copy-sequence names))))))))

(ert-deftest mcp/attribution-comes-from-category-not-server ()
  "`:category' decides the server; a planted `:server' changes nothing.
Both stub sets carry a `:server' key pointing at the WRONG server.  If
attribution still consulted `:server' -- as it used to -- the two runs
could not agree."
  (let (without-server with-server)
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "read"))
      (setq without-server (test-superchat-mcp--names)))
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read" :server "beta")
              (test-superchat-mcp--tool "beta" "read" :server "alpha"))
      (setq with-server (test-superchat-mcp--names)))
    (should (equal '("mcp:alpha:read" "mcp:beta:read") without-server))
    (should (equal without-server with-server))))

(ert-deftest mcp/tools-without-a-usable-category-are-dropped ()
  "A tool mcp.el could not attribute belongs to no server, so it is dropped.
It cannot be namespaced, and admitting it under a guessed server would
be worse than losing it."
  (test-superchat-mcp--with-hub
      '("alpha")
      (list (test-superchat-mcp--tool "alpha" "kept")
            (list :name "orphan" :description "no category at all")
            (list :name "mislabelled" :category "not-an-mcp-category"))
    (should (equal '("mcp:alpha:kept") (test-superchat-mcp--names)))))

(ert-deftest mcp/session-allowlist-still-filters-by-server ()
  "`superchat-mcp-servers' keeps its meaning: a session-level allowlist.
The hub is still asked once; the filtering happens on our side, on the
category-derived server name."
  (test-superchat-mcp--with-hub
      '("alpha" "beta")
      (list (test-superchat-mcp--tool "alpha" "read")
            (test-superchat-mcp--tool "beta" "write"))
    (let ((superchat-mcp-servers '("beta")))
      (should (equal '("mcp:beta:write") (test-superchat-mcp--names)))
      (should (= 1 test-superchat-mcp--hub-calls)))))

(ert-deftest mcp/nil-allowlist-still-means-every-server ()
  "nil is not \"no servers\"; it is \"all configured servers\"."
  (test-superchat-mcp--with-hub
      '("alpha" "beta")
      (list (test-superchat-mcp--tool "alpha" "read")
            (test-superchat-mcp--tool "beta" "write"))
    (let ((superchat-mcp-servers nil))
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (test-superchat-mcp--names))))))

(ert-deftest mcp/collected-tools-are-still-plists ()
  "This slice fixes the call form; it does not change the value type.
Converting to `llm-tool' is later work, and doing it here would silently
move the boundary this test file was written to hold."
  (test-superchat-mcp--with-hub
      '("alpha")
      (list (test-superchat-mcp--tool "alpha" "read"))
    (let ((tool (car (superchat-mcp-get-tools))))
      (should (listp tool))
      (should (keywordp (car tool)))
      (should (equal "mcp:alpha:read" (plist-get tool :name)))
      ;; The description mcp.el produced survives the renaming.
      (should (equal "read from alpha" (plist-get tool :description))))))

(ert-deftest mcp/hub-failure-degrades-instead-of-breaking-the-request ()
  "A hub that signals costs the tool surface, not the whole turn."
  (test-superchat-mcp--with-hub
      '("alpha")
      (error "mcp-hub exploded")
    (let ((result (superchat-mcp-get-tools)))
      (should-not result)
      (should (= 1 test-superchat-mcp--hub-calls)))))

;; ═══════════════════════════════════════════════════════════
;; Effect declaration layer -- what MCP hands Authority
;; ═══════════════════════════════════════════════════════════

;; `superchat-authority' is loaded so the fallback can be pinned against
;; Authority's own, rather than against a value copied by hand into this
;; file.  The runner loads Authority long before this file; the require
;; keeps the file honest when it is run on its own.
(require 'superchat-authority)
(require 'wid-edit)

(ert-deftest mcp/declared-server-returns-exactly-its-declared-effects ()
  "A declared server yields the descriptors the user wrote, unchanged."
  (let* ((declared '((:effect local-read :resource file)
                     (:effect external-read :resource network
                              :scope "https://api.example.com")))
         (superchat-mcp-server-effects
          `(("filesystem" :effects ,declared)
            ("github" :effects ((:effect external-write :resource network))))))
    (should (equal declared (superchat-mcp-get-server-effects "filesystem")))
    (should (equal '((:effect external-write :resource network))
                   (superchat-mcp-get-server-effects "github")))))

(ert-deftest mcp/undeclared-server-falls-back-to-one-unknown-effect ()
  "No declaration means one `unknown' descriptor -- not zero effects.
Zero effects would read as \"this server does nothing\", which is the one
claim an absent declaration cannot support."
  (let ((superchat-mcp-server-effects
         '(("filesystem" :effects ((:effect local-read :resource file))))))
    (dolist (server '("ghost" "" nil))
      (let ((effects (superchat-mcp-get-server-effects server)))
        (should (= 1 (length effects)))
        (should (eq 'unknown (plist-get (car effects) :effect)))
        (should (eq 'dynamic (plist-get (car effects) :resource)))))))

(ert-deftest mcp/declaration-without-effects-still-falls-back-to-unknown ()
  "An entry that declares nothing is not a declaration of \"no effects\"."
  (dolist (declaration '((:effects nil)
                         (:tools (("read" (:effect local-read :resource file))))
                         nil))
    (let ((superchat-mcp-server-effects (list (cons "filesystem" declaration))))
      (should (equal (list (list :effect 'unknown :resource 'dynamic))
                     (superchat-mcp-get-server-effects "filesystem"))))))

(ert-deftest mcp/unknown-fallback-matches-authoritys-own-fallback ()
  "The fallback is the effect Authority already invents with no resolver.
`superchat-authority-resolve-effects' answers an unregistered tool with
a single `unknown' over `dynamic' (superchat-authority.el:322-330), which
is `ask' when attended and `deny' when not.  Declaring nothing must land
on that same treatment, so the two are compared rather than assumed."
  (let* ((superchat-mcp-server-effects nil)
         (ours (superchat-mcp-get-server-effects "ghost"))
         (authoritys (superchat-authority-resolve-effects
                      "mcp:ghost:read_file" nil)))
    (should (= 1 (length authoritys)))
    (should (eq (plist-get (car authoritys) :effect)
                (plist-get (car ours) :effect)))
    (should (eq (plist-get (car authoritys) :resource)
                (plist-get (car ours) :resource)))
    ;; And Authority reads our fallback as unclassified, which is the
    ;; property the ask/deny treatment actually hangs on.
    (should (superchat-authority--unknown-effect-p ours))))

(ert-deftest mcp/lookup-does-not-hand-out-the-users-setting ()
  "The caller gets a copy; rewriting a descriptor cannot edit the option."
  (let* ((declared (list (list :effect 'local-read :resource 'file)))
         (superchat-mcp-server-effects (list (cons "filesystem"
                                                   (list :effects declared)))))
    (let ((effects (superchat-mcp-get-server-effects "filesystem")))
      (plist-put (car effects) :effect 'local-write)
      (should (eq 'local-write (plist-get (car effects) :effect))))
    (should (equal '((:effect local-read :resource file)) declared))))

(ert-deftest mcp/session-allowlist-type-and-meaning-are-unchanged ()
  "`superchat-mcp-servers' keeps its type and its two documented values.
The effect declaration lands beside the session allowlist; nothing about
the allowlist moves.  Both halves are pinned: the customize type
verbatim, and the meaning -- nil is \"all configured servers\", a list is
a subset filter."
  (should (equal '(repeat string) (get 'superchat-mcp-servers 'custom-type)))
  (should-not (eval (car (get 'superchat-mcp-servers 'standard-value)) t))
  (test-superchat-mcp--with-hub '("alpha" "beta") nil
    (let ((superchat-mcp-servers nil))
      (should (equal '("alpha" "beta") (superchat-mcp--active-servers))))
    (let ((superchat-mcp-servers '("beta")))
      (should (equal '("beta") (superchat-mcp--active-servers))))))

(ert-deftest mcp/declaration-shape-reserves-room-for-per-tool-overrides ()
  "A per-tool override can be added without touching what is written today.
Per-tool override is not implemented here -- nothing reads `:tools'.
What is asserted is the shape promise: the extended value is still a
legal value of the declared type, the part of it that exists today is
byte-identical to the declaration written without overrides, and the
server-wide answer is the same either way.  Adding the reading logic
later therefore needs no edit to a user's existing setting."
  (let* ((server-wide '((:effect local-read :resource file)))
         (per-tool '((:effect local-write :resource file)))
         (today `(("filesystem" :effects ,server-wide)))
         (tomorrow `(("filesystem" :effects ,server-wide
                      :tools (("write_file" . ,per-tool)))))
         (type (widget-convert (get 'superchat-mcp-server-effects 'custom-type))))
    (should (widget-apply type :match today))
    (should (widget-apply type :match tomorrow))
    ;; The head of the extended declaration is the whole of today's.
    (should (equal (cdr (assoc "filesystem" today))
                   (cl-subseq (cdr (assoc "filesystem" tomorrow)) 0 2)))
    (dolist (setting (list today tomorrow))
      (let ((superchat-mcp-server-effects setting))
        (should (equal server-wide
                       (superchat-mcp-get-server-effects "filesystem")))))))

(ert-deftest mcp/declared-effect-kinds-are-the-closed-set ()
  "Only the seven kinds Authority classifies may appear in a declaration.
The customize type is what a user is offered, so the set is asserted
through the type as well as through the constant."
  (should (equal '(local-read local-write process-exec runtime-exec
                   external-read external-write unknown)
                 superchat-mcp--effect-kinds))
  (let ((type (widget-convert (get 'superchat-mcp-server-effects 'custom-type))))
    (dolist (kind superchat-mcp--effect-kinds)
      (should (widget-apply
               type :match
               `(("filesystem" :effects ((:effect ,kind :resource file)))))))))

;; ═══════════════════════════════════════════════════════════
;; Registration layer -- MCP on the shared model tool surface
;; ═══════════════════════════════════════════════════════════

;; These tests exercise the real registration path, so they need the module
;; that owns it and the module that wraps what it exposes.  The runner has
;; loaded both long before this file; the requires keep it runnable alone.
(require 'superchat-tools)
(require 'superchat-agent-loop)
(require 'superchat-synthesis)

(defconst test-superchat-mcp--native-make-tool
  (and (fboundp 'llm-make-tool) (symbol-function 'llm-make-tool))
  "The real `llm-make-tool', on a machine that has llm.el.
Captured at load time, before any `cl-letf' below could shadow it, so
the doubles stand in only where there is nothing to stand in for.")

(defconst test-superchat-mcp--native-raw-make-tool
  (and (fboundp 'make-llm-tool) (symbol-function 'make-llm-tool)))

(defun test-superchat-mcp--make-tool (&rest args)
  "Build an llm tool from ARGS: the real struct if llm.el is here, else a double."
  (if test-superchat-mcp--native-make-tool
      (apply test-superchat-mcp--native-make-tool args)
    (cons 'mock-tool args)))

(defun test-superchat-mcp--raw-make-tool (&rest args)
  "Stand in for `make-llm-tool', which `superchat--agent-wrap-tool' calls."
  (if test-superchat-mcp--native-raw-make-tool
      (apply test-superchat-mcp--native-raw-make-tool args)
    (cons 'mock-tool args)))

(defconst test-superchat-mcp--native-tool-p
  (and (fboundp 'llm-tool-p) (symbol-function 'llm-tool-p))
  "The real `llm-tool' predicate, or nil on a machine without llm.el.")

(defconst test-superchat-mcp--native-tool-accessors
  (and test-superchat-mcp--native-tool-p
       (apply
        #'append
        (mapcar
         (lambda (slot)
           (let ((accessor (intern (format "llm-tool-%s"
                                           (substring (symbol-name slot) 1)))))
             (and (fboundp accessor)
                  (list slot (symbol-function accessor)))))
         '(:name :description :args :function :async))))
  "The real `llm-tool' slot accessors, keyed by slot.
Captured at load time, before any `cl-letf' below could shadow them,
for the same reason `test-superchat-mcp--native-make-tool' is: where
llm.el is installed the doubles hand back genuine structs, and a genuine
struct can only be read by the genuine accessor.")

(defun test-superchat-mcp--tool-slot (tool slot)
  "Read SLOT of TOOL, signalling on anything that is not a tool.

Two shapes count as a tool: the real `llm-tool' struct, on a machine
that has llm.el, and the `mock-tool' double that stands in for it
everywhere else.  Anything else signals `wrong-type-argument', and that
is the whole reason this function exists.  The real accessors are
cl-struct accessors: handed the gptel-style plist `mcp.el' produces,
they signal.  The double has to signal too, or the very defect this
layer exists to catch -- a plist reaching code that expects an
`llm-tool' -- would pass silently here.

The real struct is read through the real accessor rather than through
the double's plist, because it is not a plist at all.  Reading only the
double is what made the two shadow-guard tests below fail on any machine
with llm.el on its `load-path': `test-superchat-mcp--make-tool' was
written to follow the machine and this reader was not, and the two have
to agree."
  (let ((native (and test-superchat-mcp--native-tool-p
                     (funcall test-superchat-mcp--native-tool-p tool)
                     (plist-get test-superchat-mcp--native-tool-accessors slot))))
    (cond
     (native (funcall native tool))
     ((and (consp tool) (eq 'mock-tool (car tool)))
      (plist-get (cdr tool) slot))
     (t (signal 'wrong-type-argument (list 'llm-tool tool))))))

(ert-deftest mcp/the-tool-double-is-as-strict-as-the-accessor-it-replaces ()
  "A plist signals; a tool reads.  On any machine, either way.

`test-superchat-mcp--tool-slot' stands in for the cl-struct accessors
throughout this layer, and the property it has to reproduce is their
strictness: handed the gptel-style plist `mcp.el' publishes,
`llm-tool-name' signals `wrong-type-argument'.  That signal is the
defect the registration layer was written to catch, so a double that
quietly answered for a plist would let the layer pass while broken.
Pinned directly here because everywhere else the guard is a `should-error'
about `mcp.el''s shape, which reads as a statement about mcp.el rather
than about this double.

The second half is the portability half.  `test-superchat-mcp--make-tool'
returns a genuine `llm-tool' where llm.el is installed and the double
where it is not, so the reader has to accept both shapes."
  (should-error (test-superchat-mcp--tool-slot
                 (test-superchat-mcp--tool "alpha" "read") :name)
                :type 'wrong-type-argument)
  (should-error (test-superchat-mcp--tool-slot '(:name "plain plist") :name)
                :type 'wrong-type-argument)
  (should-error (test-superchat-mcp--tool-slot "not a tool at all" :name)
                :type 'wrong-type-argument)
  (let ((tool (test-superchat-mcp--make-tool
               :name "mcp:alpha:read"
               :description "read from alpha"
               :args nil
               :async nil
               :function #'ignore)))
    (should (equal "mcp:alpha:read" (test-superchat-mcp--tool-slot tool :name)))
    (should (equal "read from alpha"
                   (test-superchat-mcp--tool-slot tool :description)))
    (should (eq #'ignore (test-superchat-mcp--tool-slot tool :function)))))

(defmacro test-superchat-mcp--with-surface (&rest body)
  "Run BODY against a private, empty model tool surface.

The external tool registry and Authority's two tables are rebound, so a
test registers into its own copy and cannot leave a tool, a resolver or
a preview behind for the next one.

llm.el is optional and absent from the canonical batch run, so the tool
constructors and accessors are stubbed -- with the real ones when the
machine has them.  Without this the whole layer would skip, and skipping
is precisely how the original defect survived.

BODY runs in a temporary buffer, and the Superchat view is renamed to a
throwaway buffer that is killed afterwards, because the production tool
wrapper writes two buffer-local values -- `superchat--session-id' and
`superchat--agent-tool-call-count' -- into the view it runs the callback
in.  A session id left behind in the real `*Superchat*' buffer is read
later by whatever else asks the Runtime for one."
  (declare (indent 0))
  `(let ((superchat-buffer-name " *test-superchat-mcp-view*")
         (superchat-tools--external-registry nil)
         (superchat-authority-effect-resolvers (make-hash-table :test 'equal))
         (superchat-authority-approval-previews (make-hash-table :test 'equal))
         (superchat-llm-tools-list nil)
         (superchat--llm-tool-names-signature nil)
         (superchat--subagent-registry-signature nil)
         (superchat--subagent-registry-tool-cache nil)
         (superchat-mcp-server-effects nil))
     (cl-letf (((symbol-function 'llm-make-tool)
                #'test-superchat-mcp--make-tool)
               ((symbol-function 'make-llm-tool)
                #'test-superchat-mcp--raw-make-tool)
               ((symbol-function 'llm-tool-name)
                (lambda (tool) (test-superchat-mcp--tool-slot tool :name)))
               ((symbol-function 'llm-tool-description)
                (lambda (tool) (test-superchat-mcp--tool-slot tool :description)))
               ((symbol-function 'llm-tool-args)
                (lambda (tool) (test-superchat-mcp--tool-slot tool :args)))
               ((symbol-function 'llm-tool-function)
                (lambda (tool) (test-superchat-mcp--tool-slot tool :function)))
               ((symbol-function 'llm-tool-async)
                (lambda (tool) (test-superchat-mcp--tool-slot tool :async))))
       (unwind-protect
           (with-temp-buffer ,@body)
         (let ((view (get-buffer superchat-buffer-name)))
           (when (buffer-live-p view)
             (kill-buffer view)))))))

(ert-deftest mcp/connecting-registers-every-active-servers-tools ()
  "A connected server's tools land in the shared plugin tool registry.
Registration is what puts MCP on the model surface now; the request-time
`append' that used to do it is gone."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names "alpha")))
      (let ((spec (cdr (assoc "mcp:alpha:read"
                              superchat-tools--external-registry))))
        (should spec)
        (should (equal "read from alpha" (plist-get spec :description)))
        ;; The function the hub handed over is registered untouched.  A
        ;; second, hand-rolled wrapper around it would show up right here
        ;; as something that is no longer `eq' to it.
        (should (eq #'ignore (plist-get spec :function)))))))

(ert-deftest mcp/registration-queries-the-hub-once-for-the-whole-hub ()
  "Three servers, one hub call -- registration collects once and shares.
`mcp/hub-is-queried-once-for-the-whole-hub' pins this for
`superchat-mcp-get-tools', which is no longer the path anything reaches
the model surface by.  This pins it where it now costs something: a
`superchat-mcp-register-all-tools' that collected per server would make
N identical round-trips to the hub and still register the right names,
so the names alone cannot tell the two apart."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta" "gamma")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "read")
              (test-superchat-mcp--tool "gamma" "write"))
      (superchat-mcp-register-all-tools)
      (should (= 1 test-superchat-mcp--hub-calls))
      (should (equal '("mcp:alpha:read" "mcp:beta:read" "mcp:gamma:write")
                     (superchat-mcp-registered-tool-names))))))

(ert-deftest mcp/only-running-servers-of-the-session-are-registered ()
  "The two filters that decide the tool surface still decide it.
A configured but unconnected server contributes nothing, and a server
outside `superchat-mcp-servers' contributes nothing either."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (let ((superchat-mcp-servers '("alpha")))
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names))))))
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "ghost" "haunt"))
      ;; "ghost" is not in `mcp-hub-servers', so it is not an active
      ;; server of this session no matter what the hub reports.
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names))))))

(ert-deftest mcp/registered-tool-reaches-the-model-as-a-struct ()
  "What the model surface hands over is an llm tool, not a gptel plist.
This is the defect that made every tool-carrying request signal
`wrong-type-argument llm-tool' the moment an MCP server was connected:
`superchat-llm.el' appended `mcp.el''s plists straight into the tool
list, and the Authority wrapper called `llm-tool-name' on them."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (let* ((surface (superchat-tools--external-llm-tools))
             (tool (car surface)))
        (should (= 1 (length surface)))
        ;; A plist would have a keyword in the car; this does not, and
        ;; the strict accessors -- which signal on anything that is not a
        ;; tool -- answer for it.
        (should-not (keywordp (car-safe tool)))
        (should (equal "mcp:alpha:read" (llm-tool-name tool)))
        (should (equal "read from alpha" (llm-tool-description tool)))
        (should (eq #'ignore (llm-tool-function tool)))
        ;; The hub's own key does not travel: this value was built from
        ;; the registration, not forwarded from `mcp-hub-get-all-tool'.
        (should-not (plist-get (cdr-safe tool) :category))
        (when test-superchat-mcp--native-make-tool
          (should (llm-tool-p tool)))))))

(ert-deftest mcp/invoking-a-registered-tool-reaches-superchat-tool-execute ()
  "The ordinary MCP wrapper route arrives at the shared executor.
This test exercises `superchat--agent-wrap-tool'; prepared subagent tools use
`superchat--subagent-wrap-function' instead.  Both routes call
`superchat-tool-execute' under the namespaced name and hand it the server's
own function."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (let ((tool (car (superchat-tools--external-llm-tools)))
            (seen nil))
        (cl-letf (((symbol-function 'superchat--agent-render-tool-call)
                   #'ignore)
                  ((symbol-function 'superchat--agent-render-tool-result)
                   #'ignore)
                  ((symbol-function 'superchat-tool-execute)
                   (lambda (tool-name original-fn args &rest _)
                     (setq seen (list tool-name original-fn args))
                     "executed")))
          (let ((wrapped (superchat--agent-wrap-tool tool)))
            (should (equal "executed"
                           (funcall (llm-tool-function wrapped) "x")))))
        (should (equal "mcp:alpha:read" (nth 0 seen)))
        (should (eq #'ignore (nth 1 seen)))
        (should (equal '("x") (nth 2 seen)))))))

(ert-deftest mcp/registration-declares-the-servers-effects-to-authority ()
  "The base Authority fold receives a registered MCP server's Effects.
The name selects this resolver, while the resolved descriptors supply the
classification input that keeps the base fold from falling back to unknown."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (let ((superchat-mcp-server-effects
             '(("alpha" :effects ((:effect local-read :resource file))))))
        (superchat-mcp-register-all-tools)
        (should (equal '((:effect local-read :resource file))
                       (superchat-authority-resolve-effects
                        "mcp:alpha:read" nil)))
        ;; An undeclared server keeps today's treatment: one `unknown'
        ;; descriptor, which is ask when attended and deny when not.
        (let ((effects (superchat-authority-resolve-effects
                        "mcp:beta:write" nil)))
          (should (= 1 (length effects)))
          (should (eq 'unknown (plist-get (car effects) :effect)))
          (should (superchat-authority--unknown-effect-p effects)))))))

(ert-deftest mcp/stopping-a-server-removes-tool-resolver-and-preview ()
  "All three registrations go, not just the visible one.
A resolver left behind for a name nothing can call is silent: it would
resurface only when some later tool of that name inherited effects it
never declared."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      ;; Present first, so the absence below means removal and not a
      ;; registration that never happened.
      (should (assoc "mcp:alpha:read" superchat-tools--external-registry))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-effect-resolvers)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-approval-previews)))
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-unregister-server-tools "alpha")))
      (should-not (assoc "mcp:alpha:read" superchat-tools--external-registry))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-effect-resolvers))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-approval-previews))
      (should-not (superchat-mcp-registered-tool-names))
      (should-not (superchat-tools--external-llm-tools)))))

(ert-deftest mcp/stopping-one-server-leaves-the-others-alone ()
  "Unregistration is scoped to its own server, prefix and all.
\"alphabet\" is here because a prefix without the trailing colon would
take its tools down together with \"alpha\"'s."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "alphabet" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "alphabet" "read")
              (test-superchat-mcp--tool "beta" "read"))
      (superchat-mcp-register-all-tools)
      (superchat-mcp-unregister-server-tools "alpha")
      (should (equal '("mcp:alphabet:read" "mcp:beta:read")
                     (superchat-mcp-registered-tool-names)))
      (dolist (name '("mcp:alphabet:read" "mcp:beta:read"))
        (should (functionp (gethash name
                                    superchat-authority-effect-resolvers)))
        (should (functionp (gethash name
                                    superchat-authority-approval-previews)))))))

(ert-deftest mcp/re-registering-a-name-replaces-it ()
  "Registering the same server twice replaces; it does not append a second entry."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (superchat-mcp-register-all-tools)
      (should (= 1 (cl-count "mcp:alpha:read"
                             superchat-tools--external-registry
                             :key #'car :test #'equal)))
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names)))
      (should (= 1 (length (superchat-tools--external-llm-tools)))))))

(ert-deftest mcp/a-tool-a-server-stopped-exposing-does-not-survive ()
  "Re-registration drops what the server no longer offers.
Replacement alone cannot do this: the vanished name is never registered
again, so nothing would overwrite it."
  (test-superchat-mcp--with-surface
    (let ((offered (list (test-superchat-mcp--tool "alpha" "read")
                         (test-superchat-mcp--tool "alpha" "write"))))
      (test-superchat-mcp--with-hub '("alpha") offered
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read" "mcp:alpha:write")
                       (superchat-mcp-registered-tool-names)))
        (setq offered (list (test-superchat-mcp--tool "alpha" "read")))
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should-not (gethash "mcp:alpha:write"
                             superchat-authority-effect-resolvers))
        (should-not (gethash "mcp:alpha:write"
                             superchat-authority-approval-previews))))))

(ert-deftest mcp/a-tool-without-a-description-still-reaches-the-surface ()
  "`superchat-register-tool' demands a description; MCP may have none.
Refusing the tool would cost it its place over a missing sentence, so it
is named instead.  The other tools of the same server are unaffected
either way -- that is the point of registering them one at a time."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (list :function #'ignore :name "quiet" :args nil :async nil
                    :category "mcp-alpha")
              (test-superchat-mcp--tool "alpha" "loud"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:quiet" "mcp:alpha:loud")
                     (superchat-mcp-registered-tool-names)))
      (let ((spec (cdr (assoc "mcp:alpha:quiet"
                              superchat-tools--external-registry))))
        (should (stringp (plist-get spec :description)))
        (should-not (string-empty-p (plist-get spec :description)))))))

;; ═══════════════════════════════════════════════════════════
;; Server lifecycle -- the commands that put tools on and take them off
;; ═══════════════════════════════════════════════════════════

;; Everything above calls the registration helpers directly, which leaves
;; their call sites uncovered: `superchat-mcp-start-servers',
;; `superchat-mcp-start-server' and `superchat-mcp-stop-server' could each
;; drop the helper call and every test above would still pass.  The two
;; Action Contracts are about a server *connecting* and a server *being
;; stopped*, so these reach the helpers only through the commands that a
;; connection and a stop actually go through.

(defmacro test-superchat-mcp--with-hub-start (&rest body)
  "Run BODY with `mcp-hub-start-all-server' stubbed to connect and call back.
The real function calls CALLBACK once every server it was handed has
either connected or failed to (../mcp.el/mcp-hub.el:140-143), by which
time the connections table holds the ones that came up.  The stub does
both, in that order, so a callback that collects tools finds a hub with
servers in it -- a stub that only called back would make the collection
return nil and the assertions vacuous."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'mcp-hub-start-all-server)
              (lambda (&optional callback servers &rest _)
                (dolist (entry servers)
                  (puthash (car entry)
                           (list :stub-connection (car entry) :status 'connected)
                           mcp-server-connections))
                (when callback (funcall callback)))))
     ,@body))

(ert-deftest mcp/starting-the-hub-registers-the-connected-servers-tools ()
  "Starting the session's servers is a call site of registration.

This is the path a session takes to connect everything at once, and AC
\"Register one MCP server's tools\" is about exactly that moment.  The
caller's own callback is asserted as well as the state afterwards,
because the tools must be on the surface *before* CALLBACK runs: a
caller that issues a request from its callback would otherwise send that
request without them, which is invisible after the fact."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (let ((seen-by-callback 'callback-never-ran))
        (test-superchat-mcp--with-hub-start
          (should-not (superchat-mcp-registered-tool-names))
          (superchat-mcp-start-servers
           (lambda ()
             (setq seen-by-callback (superchat-mcp-registered-tool-names)))))
        (should (equal '("mcp:alpha:read" "mcp:beta:write")
                       (superchat-mcp-registered-tool-names)))
        (should (equal '("mcp:alpha:read" "mcp:beta:write") seen-by-callback))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-effect-resolvers)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-approval-previews)))))))

(ert-deftest mcp/starting-an-already-connected-server-still-registers-it ()
  "\"Already running\" is not \"already exposed\".

A server started outside Superchat -- or before `superchat-mcp.el' was
loaded -- is connected with nothing of its own on the tool surface, and
`/mcp-start-server' is how a user says \"make this one usable\".  The hub
starter is stubbed to signal, so this pins the other half of the branch
too: a connected server is not restarted.  Only that server's tools are
registered; starting one server is not starting the hub."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (cl-letf (((symbol-function 'mcp-hub-start-all-server)
                 (lambda (&rest _)
                   (error "a connected server must not be started again"))))
        (superchat-mcp-start-server "alpha"))
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-effect-resolvers)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-approval-previews))))))

(ert-deftest mcp/starting-a-stopped-server-registers-it-once-it-connects ()
  "The branch that actually starts something registers from its callback.
\"alpha\" is configured but not connected here, which is the other half of
the command: the hub is asked to start it, and its tools reach the
surface only after it is up.  Registering before the callback would
collect from a hub that does not have it yet."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (remhash "alpha" mcp-server-connections)
      (test-superchat-mcp--with-hub-start
        (superchat-mcp-start-server "alpha"))
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-approval-previews))))))

(ert-deftest mcp/stopping-a-server-through-the-command-takes-its-tools-off ()
  "AC \"Unregister one MCP server's tools\", through the command that stops one.

`superchat-mcp-unregister-server-tools' is covered above; what is covered
here is that stopping a server reaches it.  All three registrations of
the stopped server go -- tool, effect resolver and approval preview --
and the server that was not stopped keeps all three of its own."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (superchat-mcp-register-all-tools)
      ;; Present first, so the absence below means removal and not a
      ;; registration that never happened.
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      (superchat-mcp-stop-server "alpha")
      (should-not (assoc "mcp:alpha:read" superchat-tools--external-registry))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-effect-resolvers))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-approval-previews))
      (should (equal '("mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      (should (functionp (gethash "mcp:beta:write"
                                  superchat-authority-effect-resolvers)))
      (should (functionp (gethash "mcp:beta:write"
                                  superchat-authority-approval-previews))))))

(ert-deftest mcp/stopping-a-server-that-already-died-still-takes-its-tools-off ()
  "A connection that went away unnoticed does not keep its tools.
Unregistration happens before the running check and regardless of its
answer: a tool whose server is gone can only fail, and stopping having
become a no-op is no reason to leave it on the surface."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read")
                     (superchat-mcp-registered-tool-names)))
      ;; The server dies; nothing tells Superchat.
      (remhash "alpha" mcp-server-connections)
      (superchat-mcp-stop-server "alpha")
      (should-not (superchat-mcp-registered-tool-names))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-effect-resolvers))
      (should-not (gethash "mcp:alpha:read"
                           superchat-authority-approval-previews)))))

(ert-deftest mcp/lifecycle-uses-mcp-status-and-stop-server-api ()
  "Stop and health use mcp.el's real public lifecycle names."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha") nil
      (let (status-read stopped)
        (cl-letf
            (((symbol-function 'mcp--status)
              (lambda (connection)
                (setq status-read t)
                (plist-get connection :status)))
             ((symbol-function 'mcp-stop-server)
              (lambda (name)
                (setq stopped name)
                (when-let* ((connection
                             (gethash name mcp-server-connections)))
                  (puthash name
                           (plist-put (copy-sequence connection) :status 'stop)
                           mcp-server-connections))))
          (should (superchat-mcp-server-health "alpha"))
          (superchat-mcp-stop-server "alpha")
          (should status-read)
          (should (equal "alpha" stopped))
          (should-not (superchat-mcp-server-health "alpha"))))))))

(ert-deftest mcp/missing-stop-server-api-signals-clearly ()
  "An absent `mcp-stop-server' is an error, never a silent no-op --
and, since the server is still running when that error is signalled,
its tool, Authority effect resolver and approval preview must all
still be on the surface afterwards.  Unregistering before the API is
known to exist would make stopping a live server destructive on a
failure that leaves the server connected; correcting for a missing
lifecycle function here is not the same call as when the status
accessor is missing -- see
`mcp/missing-status-accessor-degrades-gracefully' for that one."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read") (superchat-mcp-registered-tool-names)))
      (cl-letf (((symbol-function 'mcp-stop-server) nil))
        (let ((condition
               (should-error (superchat-mcp-stop-server "alpha")
                             :type 'error)))
          (should (string-match-p "mcp-stop-server"
                                  (error-message-string condition)))))
      (should (superchat-mcp--server-running-p "alpha"))
      (should (equal '("mcp:alpha:read") (superchat-mcp-registered-tool-names)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-effect-resolvers)))
      (should (functionp (gethash "mcp:alpha:read"
                                  superchat-authority-approval-previews))))))

(ert-deftest mcp/missing-status-accessor-degrades-gracefully ()
  "A missing `mcp--status' accessor never aborts ordinary MCP paths.

`mcp--status' is `mcp.el's private connection-status accessor: reachable
by name, but not part of its documented public API, so a Superchat
running against an `mcp.el' build that lacks it must not treat a present
connection as unusable.  Registration, `/mcp', and the health report all
keep working -- they treat a present connection as running -- while
explicit health degrades to `:unknown' instead of signalling."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (cl-letf (((symbol-function 'mcp--status) nil))
        ;; The orchestration predicate does not need the accessor: a
        ;; present connection is enough to call the server running.
        (should (superchat-mcp--server-running-p "alpha"))
        ;; Registration must not abort just because health cannot be
        ;; read precisely.
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-register-all-tools)))
        ;; Explicit health degrades to `:unknown' -- neither an error
        ;; nor a false `t'.
        (should (eq :unknown (superchat-mcp-server-health "alpha")))
        ;; `/mcp' and the health report render a degraded/unknown
        ;; server instead of signalling.
        (should (stringp (superchat-mcp-status)))
        (should (string-match-p "unknown" (superchat-mcp-health-report)))))))

(ert-deftest mcp/server-running-p-degrades-gracefully-when-status-read-signals ()
  "A present but unreadable `mcp--status' never aborts the orchestration
predicate either -- not just a missing accessor.  `mcp--status' is
`mcp.el's own API (stubbing it is fine); the bug this guards is
`superchat-mcp--server-running-p' calling it directly instead of
through `superchat-mcp--server-status', so a connection whose read
happens to signal (a malformed connection object, say) propagated the
error instead of degrading like the accessor-missing case does."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (cl-letf (((symbol-function 'mcp--status)
                 (lambda (_connection) (error "status read failed"))))
        (should (superchat-mcp--server-running-p "alpha"))
        (should (eq :unknown (superchat-mcp-server-health "alpha")))
        (should (stringp (superchat-mcp-status)))))))


;; ═══════════════════════════════════════════════════════════
;; Shadow guard -- what `define_tool' is not allowed to take
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/synthesis-shadow-guard-sees-mcp-names-without-signalling ()
  "The guard reads registered names; it no longer maps a struct accessor
over `mcp.el''s plists.

This is a regression test for a crash, not for a refusal.
`superchat--syn-existing-names' used to call `superchat-mcp-get-tools'
and map `llm-tool-name' over the result.  Those are gptel-style plists,
`llm-tool-name' is a cl-struct accessor, and the `ignore-errors' sat
inside the `mapcar' argument -- around the collection call only -- so it
caught nothing.  One connected MCP server therefore broke `define_tool'
entirely.  The old expression is asserted to still signal, so the defect
stays reproducible rather than becoming folklore."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      ;; The shape the old code fed to the struct accessor.
      (should-error (mapcar #'llm-tool-name
                            (ignore-errors (superchat-mcp-get-tools)))
                    :type 'wrong-type-argument)
      ;; The guard sees the name, and gets there without signalling.
      ;;
      ;; The plugin half is bound away first, so the registry read is the
      ;; ONLY thing that can produce this name.  Left alone the assertion
      ;; proves nothing: a registered MCP tool IS a registered plugin
      ;; tool, so `superchat-get-llm-tools' already returns the name and
      ;; the guard would pass with its MCP half deleted outright.
      (cl-letf (((symbol-function 'superchat-get-llm-tools)
                 (lambda (&rest _) nil)))
        (let ((existing (superchat--syn-existing-names)))
          (should (member "mcp:alpha:read" existing))))
      ;; And a synthesis request is answered rather than aborted.  Note
      ;; that no MCP name can ever *be* the answer: every one of them
      ;; contains a colon, which `superchat--syn-valid-name-p' rejects
      ;; before the membership test is reached.  The MCP half of this
      ;; guard is what keeps a future naming scheme honest; today its
      ;; observable job is to not crash.
      (should-not (superchat--syn-check-name "brand_new_tool")))))

(ert-deftest mcp/synthesis-shadow-guard-survives-an-absent-registry ()
  "With no MCP tools registered the guard answers as it always did."
  (test-superchat-mcp--with-surface
    (should-not (superchat-mcp-registered-tool-names))
    (should-not (superchat--syn-check-name "brand_new_tool"))))

;; ═══════════════════════════════════════════════════════════
;; The deleted bypass
;; ═══════════════════════════════════════════════════════════

(defconst test-superchat-mcp--llm-source
  (expand-file-name "../superchat-llm.el" (file-name-directory load-file-name))
  "Path to `superchat-llm.el', captured while this file is being loaded.
`load-file-name' is nil by the time a test runs.")

(ert-deftest mcp/superchat-llm-no-longer-collects-mcp-tools-itself ()
  "The request-time bypass is gone from `superchat-llm.el'.

It appended `mcp.el''s plists directly into the request's tool list,
which is how MCP tools appeared on the model surface outside the
allowlist mechanism and as the wrong type.  Registration owns exposure
now, so any surviving reference would be a second, competing path.  The
one in `superchat-synthesis.el' was replaced by a registry read; this
asserts the collection function is not reachable from the request path
at all."
  (with-temp-buffer
    (insert-file-contents test-superchat-mcp--llm-source)
    (goto-char (point-min))
    (should-not (search-forward "superchat-mcp-get-tools" nil t))))


;; ═══════════════════════════════════════════════════════════
;; Command surface -- the three `/mcp-*' commands
;; ═══════════════════════════════════════════════════════════

;; `superchat--handle-default-command' calls every built-in command with
;; NO arguments and renders the result only when it is a string.  Three
;; MCP commands did not fit: `/mcp-start-server' and `/mcp-stop-server'
;; were wired to functions that require a SERVER-NAME, so invoking them
;; signalled `wrong-number-of-arguments', and `/mcp-health' was wired to a
;; function returning an alist, which the surface drops as `:noop' -- the
;; command printed nothing at all.
;;
;; The repair adapts the commands to that contract instead of changing
;; it, so the first test here is the contract itself: whatever else moves,
;; a built-in is still called with no arguments and still renders only a
;; string.

(defvar test-superchat-mcp--completing-read-calls nil
  "Prompt and candidate list of each stubbed `completing-read' call.")

(defmacro test-superchat-mcp--choosing (choice &rest body)
  "Run BODY with `completing-read' answering CHOICE.
Each call is recorded in `test-superchat-mcp--completing-read-calls' as
\(PROMPT . CANDIDATES), newest first, so a test can assert what it was
offered as well as what came back."
  (declare (indent 1))
  `(let ((test-superchat-mcp--completing-read-calls nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  (push (cons prompt (all-completions "" collection))
                        test-superchat-mcp--completing-read-calls)
                  ,choice)))
       ,@body)))

(defun test-superchat-mcp--dispatch (command)
  "Send COMMAND through the built-in command surface, as a user would."
  (superchat--handle-default-command command nil nil nil))

(ert-deftest mcp/builtin-command-dispatch-contract-is-unchanged ()
  "Every built-in command is still called with no arguments, and only a
string still renders.

This is the constraint the three MCP commands were repaired *under*.
Passing a server name through the dispatcher would have been the shorter
fix, and it would have silently changed the calling convention of every
other built-in -- `/backend', `/models', `/mcp', `/refresh-models' --
whose owning module is not in the Kernel authority graph yet.  So the pin
is here rather than in a comment: each entry in
`superchat--builtin-commands' is stubbed with a recorder, and the
recorder must observe an empty argument list."
  (dolist (entry superchat--builtin-commands)
    (let ((command (car entry))
          (func (cdr entry))
          (received 'never-called))
      ;; A string renders into the buffer.
      (cl-letf (((symbol-function func)
                 (lambda (&rest args) (setq received args) "rendered")))
        (should (equal `(:type :buffer :content "rendered")
                       (test-superchat-mcp--dispatch command)))
        (should (equal nil received)))
      ;; Anything else does not.  An alist is the shape `/mcp-health'
      ;; used to return, which is how it managed to print nothing.
      (setq received 'never-called)
      (cl-letf (((symbol-function func)
                 (lambda (&rest args) (setq received args) '(("a" . t)))))
        (should (equal '(:type :noop) (test-superchat-mcp--dispatch command)))
        (should (equal nil received))))))

(ert-deftest mcp/start-server-command-takes-no-argument-and-renders ()
  "`/mcp-start-server' picks the server itself and answers in a string.

The old wiring pointed straight at `superchat-mcp-start-server', which
requires SERVER-NAME, so the zero-argument call the surface makes
signalled.  The choice now happens inside the command, and what the
choice leads to -- the tool registration slice 03 attached to starting --
still happens."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (test-superchat-mcp--choosing "alpha"
        (let ((result (test-superchat-mcp--dispatch "mcp-start-server")))
          (should (eq :buffer (plist-get result :type)))
          (should (stringp (plist-get result :content)))
          (should (string-match-p "alpha" (plist-get result :content))))
        ;; Every configured server is offered, not just the running ones.
        (should (equal '("alpha" "beta")
                       (cdr (car test-superchat-mcp--completing-read-calls))))
        ;; And the chosen server's tools reached the model tool surface.
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names "alpha")))))))

(ert-deftest mcp/stop-server-command-takes-no-argument-and-renders ()
  "`/mcp-stop-server' offers the running servers and answers in a string.
The unregistration slice 03 attached to stopping still happens."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read") (superchat-mcp-registered-tool-names)))
      (test-superchat-mcp--choosing "alpha"
        (let ((result (test-superchat-mcp--dispatch "mcp-stop-server")))
          (should (eq :buffer (plist-get result :type)))
          (should (string-match-p "alpha" (plist-get result :content))))
        (should (equal '("alpha")
                       (cdr (car test-superchat-mcp--completing-read-calls))))
        (should-not (superchat-mcp-registered-tool-names))))))

(ert-deftest mcp/stop-server-command-says-so-when-nothing-is-running ()
  "With no server up, the command reports it instead of prompting."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha") nil
      (clrhash mcp-server-connections)
      (test-superchat-mcp--choosing "alpha"
        (let ((result (test-superchat-mcp--dispatch "mcp-stop-server")))
          (should (eq :buffer (plist-get result :type)))
          (should (string-match-p "not running\\|No MCP server is running"
                                  (plist-get result :content))))
        (should-not test-superchat-mcp--completing-read-calls)))))

(ert-deftest mcp/health-command-renders-a-string-rather-than-noop ()
  "`/mcp-health' produces a readable report, not the raw alist.

`superchat-mcp-check-all-health' still answers in an alist and still
decides health; the command formats it.  A `:noop' here is the original
defect: the user typed `/mcp-health' and nothing appeared."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha" "beta") nil
      ;; Health itself is untouched: it is still the alist it always was.
      (should (equal '(("alpha" . t) ("beta" . t))
                     (mapcar (lambda (entry) (cons (car entry) (and (cdr entry) t)))
                             (superchat-mcp-check-all-health))))
      (let ((result (test-superchat-mcp--dispatch "mcp-health")))
        (should (eq :buffer (plist-get result :type)))
        (let ((content (plist-get result :content)))
          (should (stringp content))
          (should (string-match-p "alpha" content))
          (should (string-match-p "beta" content))
          (should (string-match-p "2 of 2" content)))))))

(ert-deftest mcp/health-command-reports-an-unhealthy-server ()
  "A configured server that is not running is reported, not omitted."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha" "beta") nil
      (remhash "beta" mcp-server-connections)
      (let ((content (plist-get (test-superchat-mcp--dispatch "mcp-health")
                                :content)))
        (should (string-match-p "1 of 2" content))
        (should (string-match-p "beta — stopped" content))))))

(ert-deftest mcp/mcp-commands-answer-in-a-string-without-mcp ()
  "Without MCP all three commands still render, rather than going quiet.
A `:noop' tells the user nothing; \"MCP not available\" tells them what
to do about it."
  (let ((mcp-hub-servers nil))
    (should-not (superchat-mcp-available-p))
    (dolist (command '("mcp-start-server" "mcp-stop-server" "mcp-health"))
      (let ((result (test-superchat-mcp--dispatch command)))
        (should (eq :buffer (plist-get result :type)))
        (should (string-match-p "MCP not available"
                                (plist-get result :content)))))))

;; ═══════════════════════════════════════════════════════════
;; One tool, counted once
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/an-mcp-tool-is-counted-once-on-the-tool-surface ()
  "Registration put MCP tools inside `superchat-get-llm-tools', and two
places in `superchat.el' went on appending a fresh `superchat-mcp-get-tools'
to it.  Nothing crashed -- both only took a `length' -- but `/tools'
reported every MCP tool twice.

The pin is arithmetic rather than textual: the number `/backend' prints
must equal the number of tools actually on the surface, and the MCP
figure must be a subset of that total rather than an addition to it."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "alpha" "write"))
      (superchat-mcp-register-all-tools)
      (let* ((surface (mapcar #'llm-tool-name (superchat-get-llm-tools)))
             (mcp-names (superchat-mcp-registered-tool-names))
             (superchat-llm-backend nil)
             (content (superchat-backend-show)))
        ;; Each MCP tool is on the surface exactly once.
        (should (= 2 (length mcp-names)))
        (dolist (name mcp-names)
          (should (= 1 (cl-count name surface :test #'equal))))
        ;; And the report counts the surface, with MCP as part of it.
        (should (string-match "Tools: \\([0-9]+\\) registered (\\([0-9]+\\) from MCP)"
                              content))
        (should (= (length surface)
                   (string-to-number (match-string 1 content))))
        (should (= (length mcp-names)
                   (string-to-number (match-string 2 content))))
        ;; The defect stated directly: the total is not surface + MCP.
        (should-not (= (+ (length surface) (length mcp-names))
                       (string-to-number (match-string 1 content))))))))

(ert-deftest mcp/response-mode-detection-does-not-collect-the-hub-again ()
  "`superchat--detect-response-mode' reads the tool surface only.

Its slow path used to append `superchat-mcp-get-tools' to the registry,
which both double-counted the MCP tools and queried the hub a second
time on a path that runs per request.  The hub call count is the
observable half of that: registration queries once, and detection must
not add to it."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (superchat-mcp-register-all-tools)
      (let ((calls test-superchat-mcp--hub-calls)
            (superchat-llm-backend nil))
        (should (> calls 0))
        (should (eq 'tool-calling (superchat--detect-response-mode)))
        (should (= calls test-superchat-mcp--hub-calls))))))

;; ═══════════════════════════════════════════════════════════
;; Degradation -- what a hub that cannot answer is allowed to cost
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/a-failed-hub-call-does-not-strip-the-registered-surface ()
  "A transient hub failure must not remove what the hub said nothing about.

`superchat-mcp-get-tools' answered nil for two opposite facts -- \"the hub
reported no tools\" and \"the hub signalled\" -- and the stale-name sweep in
`superchat-mcp-register-server-tools' read the second as the first.  One
failed round-trip therefore unregistered every tool of the server, and
`superchat-unregister-tool' takes the Authority effect resolver and the
approval preview with it.  Removal is not even a permitted effect of
registering: the AC allows writes to the registry and to Authority's two
tables, not deletions.

So the sweep now runs only on a collection that succeeded.  The last
step is the other half of that rule: a hub that answers with an empty
list has said something, and what it said still takes the vanished name
off the surface."
  (test-superchat-mcp--with-surface
    (let ((answer (list (test-superchat-mcp--tool "alpha" "read"))))
      (test-superchat-mcp--with-hub
          '("alpha")
          (if (eq answer :explode)
              (error "mcp-hub exploded")
            answer)
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        ;; The hub breaks.  Both doors into registration are tried,
        ;; because either one alone could do the damage.
        (setq answer :explode)
        (should-not (superchat-mcp-register-server-tools "alpha"))
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-effect-resolvers)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-approval-previews)))
        ;; And it is still a tool the model can be handed.
        (should (equal '("mcp:alpha:read")
                       (mapcar #'llm-tool-name
                               (superchat-tools--external-llm-tools))))
        ;; The hub recovers and reports nothing.  That is an answer, and
        ;; the name goes -- resolver and preview with it.
        (setq answer nil)
        (superchat-mcp-register-all-tools)
        (should-not (superchat-mcp-registered-tool-names))
        (should-not (gethash "mcp:alpha:read"
                             superchat-authority-effect-resolvers))
        (should-not (gethash "mcp:alpha:read"
                             superchat-authority-approval-previews))))))

(ert-deftest mcp/an-empty-hub-is-collected-once-like-any-other ()
  "An empty hub costs one round-trip, not one per active server.

`superchat-mcp-register-all-tools' hands its collection down to each
server.  While that collection was a bare list, an empty one was
indistinguishable from \"nothing was passed\", so `(or tools ...)' sent
every server back to the hub for the same empty answer -- N+1 calls
where the AC's input is a single one.  The existing \"queried once\" test
cannot see this: it always has tools to find."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha" "beta" "gamma") nil
      (should-not (superchat-mcp-register-all-tools))
      (should (= 1 test-superchat-mcp--hub-calls))
      (should-not (superchat-mcp-registered-tool-names)))))

;; ═══════════════════════════════════════════════════════════
;; Session filter -- dropping a server from `superchat-mcp-servers'
;; ═══════════════════════════════════════════════════════════

;; The AC "Unregister one MCP server's tools" names two preconditions:
;; the server is being stopped, OR it has been dropped from
;; `superchat-mcp-servers'.  Only the first had a call site.  The second
;; matters more than an unimplemented clause usually would: before
;; registration owned exposure, the allowlist was consulted on every
;; single request, so removing a server from it stopped that server's
;; tools reaching the model at once.  Now exposure is decided once, and
;; no per-server loop ever visits a server that has left the allowlist --
;; it is precisely the one `superchat-mcp--active-servers' no longer
;; returns.  Hence a reconciliation over the whole `mcp:' namespace.

(ert-deftest mcp/a-server-dropped-from-the-session-filter-loses-its-tools ()
  "Reconciliation takes down the tools of every server the filter excludes.

\"alphabet\" is here for the same reason it is in
`mcp/stopping-one-server-leaves-the-others-alone': the server a
registered name belongs to is read up to the colon, so a plain
`string-prefix-p' would confuse the two."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "alphabet" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "alphabet" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read" "mcp:alphabet:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      ;; Keeping "alpha" and dropping "alphabet" is the direction that
      ;; can tell the two apart: a server name matched as a bare prefix
      ;; would find "alpha" inside "mcp:alphabet:read" and keep it.
      (let ((superchat-mcp-servers '("alpha")))
        (should (equal '("mcp:alphabet:read" "mcp:beta:write")
                       (superchat-mcp-reconcile-registered-tools)))
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (dolist (gone '("mcp:alphabet:read" "mcp:beta:write"))
          (should-not (assoc gone superchat-tools--external-registry))
          (should-not (gethash gone superchat-authority-effect-resolvers))
          (should-not (gethash gone superchat-authority-approval-previews)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-effect-resolvers)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-approval-previews)))
        ;; Reconciling again is a no-op: nothing is left to remove.
        (should-not (superchat-mcp-reconcile-registered-tools))
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))))))

(ert-deftest mcp/registering-reconciles-the-servers-the-filter-dropped ()
  "The reconciliation has a call site, not just a definition.

Registering is the moment the tool surface is decided, so it is the
moment a server that has left `superchat-mcp-servers' has to leave the
surface too.  Without this, a dropped server keeps its tools, its
resolver and its preview for the rest of the session, because
`superchat-mcp-register-all-tools' iterates the servers that are still
active and that server is by definition not one of them."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (superchat-mcp-register-all-tools)
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      ;; The user narrows the session filter and the servers reconnect.
      (let ((superchat-mcp-servers '("alpha")))
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should-not (gethash "mcp:beta:write"
                             superchat-authority-effect-resolvers))
        (should-not (gethash "mcp:beta:write"
                             superchat-authority-approval-previews))
        (should (equal '("mcp:alpha:read")
                       (mapcar #'llm-tool-name
                               (superchat-tools--external-llm-tools))))))))

;; ═══════════════════════════════════════════════════════════
;; The `M-x' door -- the commands have to answer there too
;; ═══════════════════════════════════════════════════════════

(defvar test-superchat-mcp--messages nil
  "Every string the stubbed `message' was asked to show, newest last.")

(defmacro test-superchat-mcp--recording-messages (&rest body)
  "Run BODY with `message' recording into `test-superchat-mcp--messages'."
  (declare (indent 0))
  `(let ((test-superchat-mcp--messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (let ((text (apply #'format format-string args)))
                      (setq test-superchat-mcp--messages
                            (append test-superchat-mcp--messages (list text)))
                      text)))))
       ,@body)))

(ert-deftest mcp/the-three-commands-answer-on-the-m-x-door-too ()
  "`M-x superchat-mcp-health-report' printed nothing at all.

The command surface renders what a command returns; \\[execute-extended-command]
renders nothing, which is why `superchat-mcp--command-report' exists.  It
asked `called-interactively-p' in its OWN frame, and every command
reaches it by an ordinary function call -- so the answer was nil there
too and the helper never messaged anything.  Slice 04's goal was met on
the `/mcp-*' surface and re-broken on the other door.

All three commands are checked, because the helper is shared and a fix
to one of them would not be a fix to the helper."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (test-superchat-mcp--choosing "alpha"
        ;; Ordered so each command has something real to report: health
        ;; first, then a start that registers, then the stop that undoes it.
        (dolist (probe (list (cons #'superchat-mcp-health-report "MCP health")
                             (cons #'superchat-mcp-start-server-command "alpha")
                             (cons #'superchat-mcp-stop-server-command "alpha")))
          (test-superchat-mcp--recording-messages
            (let ((returned (call-interactively (car probe))))
              (should (stringp returned))
              (should (string-match-p (cdr probe) returned))
              ;; The answer itself reached the echo area, not merely
              ;; some other message the command happened to emit.
              (should (member returned test-superchat-mcp--messages)))))))))

(ert-deftest mcp/the-chat-surface-still-renders-without-a-second-echo ()
  "A command called as the dispatcher calls it says nothing in the echo area.
`superchat--handle-default-command' renders the returned string itself,
so messaging it as well would print the whole report twice."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub '("alpha") nil
      (test-superchat-mcp--recording-messages
        (let ((returned (funcall #'superchat-mcp-health-report)))
          (should (string-match-p "MCP health" returned))
          (should-not (member returned test-superchat-mcp--messages)))))))

;; ═══════════════════════════════════════════════════════════
;; A declaration Superchat cannot read
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/an-unreadable-effect-declaration-falls-back-to-unknown ()
  "A malformed declaration is not a weaker claim; it is no claim at all.

This is a usability repair and NOT a security one.  An unknown `:effect'
kind already resolved to `defer', which `superchat-authority.el'
collapses to `deny' unattended and `ask' attended -- identical to what
`unknown' gets, so nothing was ever loosened by a typo.  What was wrong
is that the user believed they had declared something and had not, and
nothing said so.  The fallback is pinned against Authority's own reading
of it rather than against a value copied here."
  (dolist (broken '(((:effect no-such-kind :resource file))
                    ((:effect local-read))
                    ((:effect local-read :resource "file"))
                    (("local-read" "file"))
                    ((:effect local-read :resource file)
                     (:effect nonsense :resource file))))
    (let ((superchat-mcp-server-effects
           (list (cons "filesystem" (list :effects broken))))
          (warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest args) (push args warned))))
        (let ((effects (superchat-mcp-get-server-effects "filesystem")))
          (should (equal (list (list :effect 'unknown :resource 'dynamic))
                         effects))
          (should (superchat-authority--unknown-effect-p effects))))
      (should warned)))
  ;; The positive control: a declaration that is readable is read, and
  ;; says nothing.  Without this the test would also pass on a function
  ;; that warned about everything.
  (let ((superchat-mcp-server-effects
         '(("filesystem" :effects ((:effect local-read :resource file)
                                   (:effect external-read :resource network
                                            :scope "https://api.example.com")))))
        (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warned))))
      (should (equal '((:effect local-read :resource file)
                       (:effect external-read :resource network
                                :scope "https://api.example.com"))
                     (superchat-mcp-get-server-effects "filesystem"))))
    (should-not warned)))

;; ═══════════════════════════════════════════════════════════
;; The declaration is the source of truth, not a copy of it
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/effects-follow-the-declaration-instead-of-a-frozen-copy ()
  "Editing `superchat-mcp-server-effects' changes what Authority decides.

Registration used to hand `superchat-register-tool' the *list* the
declaration held at that instant, and that function wraps a static list
as \(lambda (_args) LIST).  The list was therefore copied into
Authority's resolver table and stayed there: every later edit of the
defcustom was ignored until the server happened to be registered again.

The direction of the divergence is what makes it worth a test.  The
frozen copy is always the older claim, so a user who *tightens* a
declaration mid-session, or retracts one so the server should fall back
to `unknown', keeps being decided against the looser one.  Both halves
are pinned here, and the retract half is pinned against Authority's own
reading of `unknown' rather than against a value copied into this file."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha")
        (list (test-superchat-mcp--tool "alpha" "read"))
      (let ((superchat-mcp-server-effects
             '(("alpha" :effects ((:effect local-read :resource file))))))
        (superchat-mcp-register-all-tools)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should (equal '((:effect local-read :resource file))
                       (superchat-authority-resolve-effects
                        "mcp:alpha:read" nil)))
        ;; Tightened, with nothing re-registered and no hub call made.
        (let ((calls test-superchat-mcp--hub-calls))
          (setq superchat-mcp-server-effects
                '(("alpha" :effects ((:effect local-write :resource file)
                                     (:effect external-write
                                              :resource network)))))
          (should (equal '((:effect local-write :resource file)
                           (:effect external-write :resource network))
                         (superchat-authority-resolve-effects
                          "mcp:alpha:read" nil)))
          (should (= calls test-superchat-mcp--hub-calls)))
        ;; Retracted entirely: the fallback is `unknown', which Authority
        ;; collapses to ask when attended and deny when not.
        (setq superchat-mcp-server-effects nil)
        (let ((effects (superchat-authority-resolve-effects
                        "mcp:alpha:read" nil)))
          (should (= 1 (length effects)))
          (should (eq 'unknown (plist-get (car effects) :effect)))
          (should (superchat-authority--unknown-effect-p effects)))
        ;; And the tool is still on the surface throughout -- the fix is
        ;; about what a live registration resolves to, not about removing
        ;; it.
        (should (equal '("mcp:alpha:read")
                       (mapcar #'llm-tool-name
                               (superchat-tools--external-llm-tools))))))))

;; ═══════════════════════════════════════════════════════════
;; A server nobody started through Superchat
;; ═══════════════════════════════════════════════════════════

(ert-deftest mcp/a-server-connected-outside-superchat-is-picked-up-by-mcp ()
  "`/mcp' exposes a server that never went through Superchat's start path.

Registration happens inside `superchat-mcp-start-server' and
`superchat-mcp-start-servers'.  `mcp.el' documents neither: its own
workflow is `mcp-hub-start-all-server' in an init file, or the `mcp-hub'
UI.  A user who follows it got zero MCP tools, no error, and a `/mcp'
report listing tools the model could not call.

The fixture connects the servers directly in `mcp-server-connections',
which is exactly the state either of those upstream doors leaves behind,
and nothing in this test ever calls a Superchat start function."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      ;; Connected, and invisible to the model.
      (should-not (superchat-mcp-registered-tool-names))
      (should-not (superchat-tools--external-llm-tools))
      (let ((calls test-superchat-mcp--hub-calls)
            (content (superchat-mcp-status)))
        (should (stringp content))
        ;; The report costs one round-trip, not one for the report and
        ;; another for the registration it now performs.
        (should (= 1 (- test-superchat-mcp--hub-calls calls))))
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (mapcar #'llm-tool-name
                             (superchat-tools--external-llm-tools))))
      (dolist (name '("mcp:alpha:read" "mcp:beta:write"))
        (should (functionp (gethash name superchat-authority-effect-resolvers)))
        (should (eq #'superchat-mcp--approval-preview
                    (gethash name superchat-authority-approval-previews))))
      ;; Idempotent: a second `/mcp' changes nothing about the surface.
      (let ((registry (copy-sequence superchat-tools--external-registry)))
        (superchat-mcp-status)
        (should (equal '("mcp:alpha:read" "mcp:beta:write")
                       (superchat-mcp-registered-tool-names)))
        (should (equal (mapcar #'car registry)
                       (mapcar #'car superchat-tools--external-registry)))
        (should (equal (mapcar #'cdr registry)
                       (mapcar #'cdr superchat-tools--external-registry)))
        (dolist (name '("mcp:alpha:read" "mcp:beta:write"))
          (should (functionp (gethash name
                                      superchat-authority-effect-resolvers)))
          (should (eq #'superchat-mcp--approval-preview
                      (gethash name
                               superchat-authority-approval-previews))))))))

(ert-deftest mcp/the-reconcile-point-obeys-the-hub-failure-gate ()
  "`/mcp' may add to the surface; a broken hub must not let it subtract.

The reconcile point is a new caller of the registration path, so it is a
new way to reach the stale-name sweep.  The rule that sweep obeys —
never on a collection the hub failed to answer — has to hold through
this door too, or a `/mcp' run during a transient hub failure would take
every MCP tool, resolver and preview off the surface.

The other direction is pinned as well: a hub that honestly reports
nothing has answered, and `/mcp' still takes the vanished name down."
  (test-superchat-mcp--with-surface
    (let ((answer (list (test-superchat-mcp--tool "alpha" "read"))))
      (test-superchat-mcp--with-hub
          '("alpha")
          (if (eq answer :explode)
              (error "mcp-hub exploded")
            answer)
        (superchat-mcp-status)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (setq answer :explode)
        (superchat-mcp-status)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-effect-resolvers)))
        (should (functionp (gethash "mcp:alpha:read"
                                    superchat-authority-approval-previews)))
        ;; The hub recovers and reports nothing.  That is an answer.
        (setq answer nil)
        (superchat-mcp-status)
        (should-not (superchat-mcp-registered-tool-names))
        (should-not (gethash "mcp:alpha:read"
                             superchat-authority-effect-resolvers))
        (should-not (gethash "mcp:alpha:read"
                             superchat-authority-approval-previews))))))

(ert-deftest mcp/the-reconcile-point-still-honours-the-session-filter ()
  "`/mcp' does not undo the allowlist reconciliation it runs through.

`superchat-mcp-register-all-tools' reconciles the whole `mcp:' namespace
against `superchat-mcp-servers' before registering anything, and calling
it from the status command must keep that order: a server dropped from
the session filter has to lose its tools at `/mcp' as well, not be put
back by it."
  (test-superchat-mcp--with-surface
    (test-superchat-mcp--with-hub
        '("alpha" "beta")
        (list (test-superchat-mcp--tool "alpha" "read")
              (test-superchat-mcp--tool "beta" "write"))
      (superchat-mcp-status)
      (should (equal '("mcp:alpha:read" "mcp:beta:write")
                     (superchat-mcp-registered-tool-names)))
      (let ((superchat-mcp-servers '("alpha")))
        (superchat-mcp-status)
        (should (equal '("mcp:alpha:read")
                       (superchat-mcp-registered-tool-names)))
        (should-not (gethash "mcp:beta:write"
                             superchat-authority-effect-resolvers))
        (should-not (gethash "mcp:beta:write"
                             superchat-authority-approval-previews))))))

(provide 'test-superchat-mcp)
;;; test-superchat-mcp.el ends here
