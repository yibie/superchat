;;; superchat-prompt.el --- Fragment model and deterministic prompt assembler -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Pure prompt assembly (contract §4.3): given labelled fragments and a
;; character budget, produce one deterministic `(:system ... :messages ...
;; :receipt ...)' plist.  No I/O, no hooks, no collectors, no history logic;
;; those live above this seam (S06b).

;;; Code:

(require 'cl-lib)
(require 'superchat-stage)
(require 'superchat-plugin)

(define-error 'superchat-prompt-invalid
  "Superchat prompt fragment or fragment list is invalid")
(define-error 'superchat-prompt-over-budget
  "Superchat prompt hard fragments alone exceed the budget")

(defconst superchat-prompt-slots
  '(system instruction attachment context memory history experience plan
           template continuation)
  "Fragment slots in §4.3 total-order rank, ascending.")

(defconst superchat-prompt--system-slots '(system instruction)
  "Slots whose fragment text goes to `:system' instead of the user message.")

(defconst superchat-prompt--truncation-marker "…[truncated]"
  "Exact suffix appended to a soft fragment truncated to fit the budget.")

(defun superchat-prompt--slot-rank (slot)
  "Return SLOT's position in `superchat-prompt-slots'.
Signals `superchat-prompt-invalid' with data (:unknown-slot SLOT) when SLOT
is not a member."
  (or (cl-position slot superchat-prompt-slots)
      (signal 'superchat-prompt-invalid (list :unknown-slot slot))))

(defun superchat-prompt--invalid (field value id)
  "Signal `superchat-prompt-invalid' for FIELD holding the bad VALUE on the
fragment identified by ID (which may itself be the offending value)."
  (signal 'superchat-prompt-invalid (list :field field :value value :id id)))

(defun superchat-prompt--validate-fragment (fragment)
  "Signal `superchat-prompt-invalid' unless every field of FRAGMENT has the
shape the contract (§4.3) declares: :owner a non-nil symbol, :id a non-empty
string, :slot a member of `superchat-prompt-slots', :text a string,
:priority an integer, :budget-class `hard' or `soft', :required `t' or nil,
:refit nil or a function, :source a list, and :required non-nil only paired
with :budget-class `hard'.  Returns FRAGMENT unchanged.  Called from both
`superchat-prompt-fragment' (construction) and `superchat-prompt-sort-fragments'
\(revalidation, so a fragment built without the constructor is caught too)."
  (let ((owner (plist-get fragment :owner))
        (id (plist-get fragment :id))
        (slot (plist-get fragment :slot))
        (text (plist-get fragment :text))
        (priority (plist-get fragment :priority))
        (budget-class (plist-get fragment :budget-class))
        (required (plist-get fragment :required))
        (refit (plist-get fragment :refit))
        (source (plist-get fragment :source)))
    (unless (and (symbolp owner) owner)
      (superchat-prompt--invalid :owner owner id))
    (unless (and (stringp id) (not (zerop (length id))))
      (superchat-prompt--invalid :id id id))
    (unless (memq slot superchat-prompt-slots)
      (superchat-prompt--invalid :slot slot id))
    (unless (stringp text)
      (superchat-prompt--invalid :text text id))
    (unless (integerp priority)
      (superchat-prompt--invalid :priority priority id))
    (unless (memq budget-class '(hard soft))
      (superchat-prompt--invalid :budget-class budget-class id))
    (unless (memq required '(nil t))
      (superchat-prompt--invalid :required required id))
    (unless (or (null refit) (functionp refit))
      (superchat-prompt--invalid :refit refit id))
    (unless (listp source)
      (superchat-prompt--invalid :source source id))
    (when (and required (not (eq budget-class 'hard)))
      (superchat-prompt--invalid :required-without-hard required id)))
  fragment)

;;;###autoload
(cl-defun superchat-prompt-fragment
    (&key owner id slot text (priority 0) (budget-class 'soft) required
          refit source)
  "Build and validate one prompt fragment plist (contract §4.3).

OWNER and ID identify the fragment; ID must be globally unique across one
`superchat-prompt-assemble' call.  SLOT is a member of `superchat-prompt-slots'
and ranks the fragment for the total order.  TEXT is the fragment's rendered
content.  PRIORITY breaks ties within a slot, descending, default 0.
BUDGET-CLASS is `hard' (always included, or the whole assembly fails) or
`soft' (default; may be refit, truncated, or dropped to fit the budget).
REQUIRED marks the fragment for its owning collector; it requires
BUDGET-CLASS `hard'.  REFIT, meaningful only when BUDGET-CLASS is `soft', is
a function of one argument (remaining characters) called exactly once,
whether or not TEXT already fits, to render the text actually used.  SOURCE
is opaque provenance the caller may attach.

Signals `superchat-prompt-invalid' with data (:field FIELD :value VALUE :id
ID) when any field's shape does not match the contract -- OWNER a non-nil
symbol, ID a non-empty string, SLOT a member of `superchat-prompt-slots',
TEXT a string, PRIORITY an integer, BUDGET-CLASS `hard' or `soft', REQUIRED
`t' or nil, REFIT nil or a function, SOURCE a list -- or when REQUIRED is
non-nil while BUDGET-CLASS is not `hard'."
  (superchat-prompt--validate-fragment
   (list :owner owner :id id :slot slot :text text :priority priority
         :budget-class budget-class :required required :refit refit
         :source source)))

(defun superchat-prompt--sort-key (value)
  "Return VALUE as a string for lexicographic comparison.
Symbols compare by `symbol-name': `string<' does not accept symbols."
  (if (symbolp value) (symbol-name value) value))

(defun superchat-prompt--fragment< (a b)
  "Total-order comparator for `superchat-prompt-sort-fragments' (§4.3):
slot rank ascending, then :priority descending, then :owner and :id
lexicographic ascending."
  (let ((rank-a (superchat-prompt--slot-rank (plist-get a :slot)))
        (rank-b (superchat-prompt--slot-rank (plist-get b :slot))))
    (cond
     ((/= rank-a rank-b) (< rank-a rank-b))
     ((/= (plist-get a :priority) (plist-get b :priority))
      (> (plist-get a :priority) (plist-get b :priority)))
     ((not (equal (plist-get a :owner) (plist-get b :owner)))
      (string< (superchat-prompt--sort-key (plist-get a :owner))
               (superchat-prompt--sort-key (plist-get b :owner))))
     (t (string< (superchat-prompt--sort-key (plist-get a :id))
                  (superchat-prompt--sort-key (plist-get b :id)))))))

(defun superchat-prompt--duplicate-id (fragments)
  "Return the first :id shared by two or more FRAGMENTS, or nil."
  (let ((seen (make-hash-table :test #'equal)))
    (catch 'duplicate
      (dolist (fragment fragments)
        (let ((id (plist-get fragment :id)))
          (if (gethash id seen)
              (throw 'duplicate id)
            (puthash id t seen))))
      nil)))

;;;###autoload
(defun superchat-prompt-sort-fragments (fragments)
  "Return FRAGMENTS in the §4.3 total order: slot rank ascending, then
:priority descending, then :owner and :id lexicographic ascending.
Signals `superchat-prompt-invalid' when a :slot is not a member of
`superchat-prompt-slots', when :required is non-nil without :budget-class
`hard', or when two fragments share a global :id."
  (dolist (fragment fragments)
    (superchat-prompt--validate-fragment fragment))
  (let ((duplicate (superchat-prompt--duplicate-id fragments)))
    (when duplicate
      (signal 'superchat-prompt-invalid (list :duplicate-id duplicate))))
  (sort (copy-sequence fragments) #'superchat-prompt--fragment<))

(defun superchat-prompt--system-slot-p (fragment)
  "Return non-nil when FRAGMENT's :slot renders into `:system'."
  (memq (plist-get fragment :slot) superchat-prompt--system-slots))

(defun superchat-prompt--hard-p (fragment)
  "Return non-nil when FRAGMENT's :budget-class is `hard'."
  (eq (plist-get fragment :budget-class) 'hard))

(defun superchat-prompt--hard-suffixes (fragments)
  "Return (RENDER-TOTALS . COUNTS) for the vector FRAGMENTS, each a vector
sized 1+ (length FRAGMENTS).  RENDER-TOTALS[I] is the exact rendered length
-- including the \"\\n\\n\" separators between them -- of the hard fragments
in FRAGMENTS from index I to the end; COUNTS[I] is how many of them there
are.  Both are threaded by count rather than by accumulated string length,
so a hard fragment with an empty :text still reserves the join boundary a
fragment on either side of it needs (contract §4.3 \"reserve every hard
fragment\"; a length-based check cannot tell an empty selected fragment from
no fragment at all)."
  (let* ((n (length fragments))
         (totals (make-vector (1+ n) 0))
         (counts (make-vector (1+ n) 0)))
    (cl-loop for i from (1- n) downto 0
             for fragment = (aref fragments i)
             do (if (superchat-prompt--hard-p fragment)
                    (let ((next-total (aref totals (1+ i)))
                          (next-count (aref counts (1+ i)))
                          (len (length (plist-get fragment :text))))
                      (aset totals i (+ len (if (> next-count 0) (+ 2 next-total) next-total)))
                      (aset counts i (1+ next-count)))
                  (progn
                    (aset totals i (aref totals (1+ i)))
                    (aset counts i (aref counts (1+ i))))))
    (cons totals counts)))

;;;###autoload
(cl-defun superchat-prompt-assemble (fragments budget)
  "Assemble FRAGMENTS into one deterministic prompt within BUDGET characters
(contract §4.3).  Sorts FRAGMENTS into total order first (see
`superchat-prompt-sort-fragments', whose signals propagate here).

`system'/`instruction' fragments join by \"\\n\\n\" into `:system',
uncounted against BUDGET.  Every remaining (`hard') fragment is reserved
first, including the join boundary it needs from whatever precedes it; if
their rendered total exceeds BUDGET, signals `superchat-prompt-over-budget'
with data (:hard N :budget BUDGET) and truncates nothing.  Soft fragments
are then filled in total order: a fragment with :refit is re-rendered
exactly once by (funcall refit REMAINING-CHARS) -- REMAINING-CHARS already
net of the separator to whatever precedes it and the join boundary and
reserved length of every not-yet-placed hard fragment -- whether or not its
original text already fits; a non-empty result is chosen, an empty one is
omitted, and either way the fragment's id is recorded in :refit, and every
soft fragment ordered after it is omitted without being attempted.  A
fragment without :refit that fits as-is is included whole; the first one
that does not is truncated to a prefix plus
`superchat-prompt--truncation-marker' when at least 80 characters remain,
else omitted, and every soft fragment after it is likewise omitted without
being attempted.  A refit result that is not a string, or longer than the
REMAINING-CHARS it was given, signals `superchat-prompt-invalid'.

Selection is tracked by count, not by the accumulated rendered length, so an
empty-text fragment that was still selected keeps the join boundary a
following fragment needs.  Before returning, asserts (= :used (length of
the assembled user message)) and (<= :used BUDGET); either failing is an
implementation bug, signalled via `cl-assert', not a normal validation
outcome.

Returns `(:system STRING :messages ((:role user :content STRING)) :receipt
PLIST)'; PLIST is `(:budget N :hard N :used N :chosen IDS :omitted IDS
:truncated IDS :refit IDS)'.  :chosen lists the selected system/instruction
ids first, in total order, then the selected user-message ids, also in
total order; the other three lists are user-message ids only, in total
order."
  (let* ((ordered (superchat-prompt-sort-fragments fragments))
         (system-fragments (cl-remove-if-not #'superchat-prompt--system-slot-p ordered))
         (system-text (mapconcat (lambda (f) (plist-get f :text)) system-fragments "\n\n"))
         (system-ids (mapcar (lambda (f) (plist-get f :id)) system-fragments))
         (user-fragments (vconcat (cl-remove-if #'superchat-prompt--system-slot-p ordered)))
         (n (length user-fragments))
         (hard-suffixes (superchat-prompt--hard-suffixes user-fragments))
         (hard-totals (car hard-suffixes))
         (hard-counts (cdr hard-suffixes))
         (hard-total (aref hard-totals 0)))
    (when (> hard-total budget)
      (signal 'superchat-prompt-over-budget (list :hard hard-total :budget budget)))
    (let ((pieces nil) (selected-count 0) (used-length 0)
          (chosen nil) (omitted nil) (truncated nil) (refit-ids nil) (stopped nil))
      (cl-flet ((add-piece
                 (text)
                 (push text pieces)
                 (setq used-length (if (zerop selected-count)
                                        (length text)
                                      (+ used-length 2 (length text))))
                 (setq selected-count (1+ selected-count))))
        (cl-loop for i from 0 below n
                 for fragment = (aref user-fragments i)
                 for id = (plist-get fragment :id)
                 do
                 (cond
                  ((superchat-prompt--hard-p fragment)
                   (add-piece (plist-get fragment :text))
                   (push id chosen))
                  (stopped
                   (push id omitted))
                  (t
                   (let* ((hard-ahead-count (aref hard-counts (1+ i)))
                          (hard-ahead-render (aref hard-totals (1+ i)))
                          (sep-before (if (zerop selected-count) 0 2))
                          (sep-after (if (> hard-ahead-count 0) 2 0))
                          (remaining (max 0 (- budget used-length sep-before
                                               sep-after hard-ahead-render)))
                          (text (plist-get fragment :text))
                          (refit (plist-get fragment :refit)))
                     (cond
                      (refit
                       (setq stopped t)
                       (push id refit-ids)
                       (let ((new-text (funcall refit remaining)))
                         (unless (stringp new-text)
                           (superchat-prompt--invalid :refit-result new-text id))
                         (when (> (length new-text) remaining)
                           (superchat-prompt--invalid :refit-result new-text id))
                         (if (zerop (length new-text))
                             (push id omitted)
                           (progn (add-piece new-text) (push id chosen)))))
                      ((>= remaining (length text))
                       (add-piece text)
                       (push id chosen))
                      ((>= remaining 80)
                       (setq stopped t)
                       (let* ((prefix-len (- remaining (length superchat-prompt--truncation-marker)))
                              (new-text (concat (substring text 0 prefix-len)
                                                 superchat-prompt--truncation-marker)))
                         (add-piece new-text)
                         (push id chosen)
                         (push id truncated)))
                      (t
                       (setq stopped t)
                       (push id omitted))))))))
      (let ((user-message (mapconcat #'identity (nreverse pieces) "\n\n")))
        (cl-assert (= used-length (length user-message)) t
                   "superchat-prompt-assemble: tracked :used disagrees with the assembled message length")
        (cl-assert (<= used-length budget) t
                   "superchat-prompt-assemble: assembled message exceeds its budget")
        (list :system system-text
              :messages (list (list :role 'user :content user-message))
              :receipt (list :budget budget :hard hard-total :used used-length
                              :chosen (append system-ids (nreverse chosen))
                              :omitted (nreverse omitted)
                              :truncated (nreverse truncated)
                              :refit (nreverse refit-ids)))))))

(superchat-define-plugin prompt-assembler
  :apply #'ignore
  :doc "Prompt assembly is core (`superchat-prompt-assemble').
The plugin stays in the default set as that capability's declaration, but it
registers nothing: the fixed Stage registry is retired, and one core
implementation is a direct call rather than a field of competing
implementations.")

(provide 'superchat-prompt)

;;; superchat-prompt.el ends here
