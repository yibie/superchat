;;; test-prompt.el --- ERT tests for superchat-prompt -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the pure fragment model and deterministic prompt assembler
;; (contract §4.3): total order, validation signals, the hard/soft budget
;; algorithm (reserve, refit, truncate, omit), and the plain-join rendering
;; grammar.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-prompt)

(defun test-prompt--ids (fragments)
  "Return the :id of each fragment in FRAGMENTS, in list order."
  (mapcar (lambda (f) (plist-get f :id)) fragments))

(ert-deftest prompt/total-order-slot-priority-owner-id ()
  "Sorting orders by slot rank, then :priority descending, then :owner and
:id lexicographic ascending."
  (let ((fragments
         (list
          (superchat-prompt-fragment :owner 'a :id "m1" :slot 'memory :text "m")
          (superchat-prompt-fragment :owner 'b :id "c1" :slot 'context :text "c" :priority 0)
          (superchat-prompt-fragment :owner 'a :id "c2z" :slot 'context :text "c" :priority 0)
          (superchat-prompt-fragment :owner 'a :id "i1" :slot 'instruction :text "i")
          (superchat-prompt-fragment :owner 'a :id "c3" :slot 'context :text "c" :priority 5)
          (superchat-prompt-fragment :owner 'a :id "c2" :slot 'context :text "c" :priority 0))))
    (should (equal (test-prompt--ids (superchat-prompt-sort-fragments fragments))
                   '("i1" "c3" "c2" "c2z" "c1" "m1")))))

(ert-deftest prompt/duplicate-id-is-invalid ()
  "Two fragments sharing a global :id signal `superchat-prompt-invalid'."
  (let ((fragments
         (list
          (superchat-prompt-fragment :owner 'a :id "dup" :slot 'context :text "x")
          (superchat-prompt-fragment :owner 'b :id "dup" :slot 'memory :text "y"))))
    (should-error (superchat-prompt-sort-fragments fragments)
                   :type 'superchat-prompt-invalid)))

(ert-deftest prompt/unknown-slot-is-invalid ()
  "A :slot outside `superchat-prompt-slots' signals `superchat-prompt-invalid'
at fragment construction."
  (should-error (superchat-prompt-fragment :owner 'a :id "x" :slot 'nonsense :text "x")
                :type 'superchat-prompt-invalid))

(ert-deftest prompt/required-must-be-hard ()
  "`:required t' without `:budget-class hard' signals `superchat-prompt-invalid'."
  (should-error (superchat-prompt-fragment :owner 'a :id "x" :slot 'context
                                            :text "x" :required t)
                :type 'superchat-prompt-invalid)
  ;; The same combination is accepted when paired with :budget-class hard.
  (should (superchat-prompt-fragment :owner 'a :id "x" :slot 'context :text "x"
                                      :required t :budget-class 'hard)))

(ert-deftest prompt/hard-over-budget-signals-and-never-truncates ()
  "Hard fragments whose rendered total exceeds BUDGET signal
`superchat-prompt-over-budget' with the exact hard/budget totals, before any
truncation is attempted."
  (let* ((f1 (superchat-prompt-fragment :owner 'a :id "h1" :slot 'context
                                         :text (make-string 40 ?x) :budget-class 'hard))
         (f2 (superchat-prompt-fragment :owner 'a :id "h2" :slot 'memory
                                         :text (make-string 40 ?y) :budget-class 'hard))
         ;; hard total = 40 + 2 (separator) + 40 = 82.
         (err (should-error (superchat-prompt-assemble (list f1 f2) 50)
                             :type 'superchat-prompt-over-budget)))
    (should (equal (cdr err) (list :hard 82 :budget 50)))))

(ert-deftest prompt/soft-truncation-marker-and-receipt ()
  "A soft fragment without :refit that does not fit, with at least 80
characters remaining, is truncated to a prefix plus the exact marker, and
the receipt records it as both chosen and truncated."
  (let* ((f (superchat-prompt-fragment :owner 'a :id "t1" :slot 'template
                                        :text (make-string 200 ?x)))
         (result (superchat-prompt-assemble (list f) 100))
         (receipt (plist-get result :receipt))
         (expected (concat (make-string 88 ?x) "…[truncated]")))
    (should (equal (length expected) 100))
    (should (equal (plist-get (car (plist-get result :messages)) :content) expected))
    (should (equal (plist-get receipt :used) 100))
    (should (equal (plist-get receipt :chosen) '("t1")))
    (should (equal (plist-get receipt :truncated) '("t1")))
    (should (equal (plist-get receipt :omitted) nil))
    (should (equal (plist-get receipt :refit) nil))))

(ert-deftest prompt/soft-omitted-when-under-80-chars-remain ()
  "A soft fragment that does not fit and leaves fewer than 80 characters is
omitted rather than truncated, and every soft fragment ordered after it is
also omitted without being attempted -- even one that would otherwise fit."
  (let* ((big (superchat-prompt-fragment :owner 'a :id "big" :slot 'template
                                          :text (make-string 200 ?y) :priority 10))
         (small (superchat-prompt-fragment :owner 'a :id "small" :slot 'template
                                            :text "hi" :priority 0))
         (result (superchat-prompt-assemble (list big small) 50))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) ""))
    (should (equal (plist-get receipt :used) 0))
    (should (equal (plist-get receipt :chosen) nil))
    (should (equal (plist-get receipt :omitted) '("big" "small")))
    (should (equal (plist-get receipt :truncated) nil))))

(ert-deftest prompt/refit-called-once-with-remaining-budget ()
  "A soft fragment's :refit function is invoked exactly once with the
remaining characters after the separator is reserved; a non-empty result is
chosen, an empty result is recorded as omitted, and :refit lists the id
either way."
  (let* ((calls nil)
         (f (superchat-prompt-fragment
             :owner 'a :id "r1" :slot 'context :text (make-string 300 ?z)
             :refit (lambda (remaining) (push remaining calls) "REFIT-TEXT")))
         (result (superchat-prompt-assemble (list f) 120))
         (receipt (plist-get result :receipt)))
    (should (equal calls '(120)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "REFIT-TEXT"))
    (should (equal (plist-get receipt :chosen) '("r1")))
    (should (equal (plist-get receipt :refit) '("r1")))
    (should (equal (plist-get receipt :omitted) nil))
    (should (equal (plist-get receipt :used) (length "REFIT-TEXT")))))

(ert-deftest prompt/refit-returning-empty-string-is-omitted ()
  "A :refit function that returns an empty string leaves its fragment out
of the user message, records it as omitted, and still lists it in :refit."
  (let* ((f (superchat-prompt-fragment
             :owner 'a :id "r2" :slot 'context :text (make-string 300 ?z)
             :refit (lambda (_remaining) "")))
         (result (superchat-prompt-assemble (list f) 120))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) ""))
    (should (equal (plist-get receipt :chosen) nil))
    (should (equal (plist-get receipt :omitted) '("r2")))
    (should (equal (plist-get receipt :refit) '("r2")))))

(ert-deftest prompt/system-and-instruction-go-to-system ()
  "`system' and `instruction' fragments join into `:system', in slot-rank
order, and never appear in the user message."
  (let* ((sys (superchat-prompt-fragment :owner 'a :id "s1" :slot 'system :text "SYS"))
         (instr (superchat-prompt-fragment :owner 'a :id "i1" :slot 'instruction :text "INSTR"))
         (ctx (superchat-prompt-fragment :owner 'a :id "c1" :slot 'context :text "CTX"))
         (result (superchat-prompt-assemble (list instr ctx sys) 1000)))
    (should (equal (plist-get result :system) "SYS\n\nINSTR"))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "CTX"))))

(ert-deftest prompt/rendering-grammar-is-plain-join ()
  "The user message is exactly the chosen fragment texts, in total order,
joined by \"\\n\\n\" -- no header or label is added."
  (let* ((a (superchat-prompt-fragment :owner 'a :id "a" :slot 'attachment :text "AAA"))
         (b (superchat-prompt-fragment :owner 'a :id "b" :slot 'context :text "BBB"
                                        :budget-class 'hard))
         (c (superchat-prompt-fragment :owner 'a :id "c" :slot 'memory :text "CCC"))
         (result (superchat-prompt-assemble (list c b a) 1000)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "AAA\n\nBBB\n\nCCC"))
    (should (equal (plist-get result :messages) '((:role user :content "AAA\n\nBBB\n\nCCC"))))))

(ert-deftest prompt/empty-fragment-list-yields-empty-user-message ()
  "Assembling an empty fragment list yields an empty system text, an empty
user message, and a receipt with no chosen/omitted/truncated/refit ids."
  (let* ((result (superchat-prompt-assemble nil 500))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get result :system) ""))
    (should (equal (plist-get result :messages) '((:role user :content ""))))
    (should (equal receipt '(:budget 500 :hard 0 :used 0
                              :chosen nil :omitted nil :truncated nil :refit nil)))))

;; --- S06a-fix-1: budget invariant, field validation, refit contract ---

(ert-deftest prompt/mixed-soft-before-two-hard-tight-budget-omits-soft ()
  "S06a review finding 1's regression: a soft fragment sorted before two hard
fragments must not be included when doing so would push the assembled
message over budget, even though the soft fragment alone would fit before
the separator to the hard fragments ahead is accounted for."
  (let* ((x (superchat-prompt-fragment :owner 'a :id "x" :slot 'template :text "x"))
         (h (superchat-prompt-fragment :owner 'a :id "h" :slot 'continuation
                                        :text "h" :budget-class 'hard))
         (j (superchat-prompt-fragment :owner 'a :id "j" :slot 'continuation
                                        :text "j" :budget-class 'hard :priority -1))
         (result (superchat-prompt-assemble (list x h j) 5))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "h\n\nj"))
    (should (equal (plist-get receipt :used) 4))
    (should (<= (plist-get receipt :used) 5))
    (should (equal (plist-get receipt :chosen) '("h" "j")))
    (should (equal (plist-get receipt :omitted) '("x")))))

(ert-deftest prompt/mixed-soft-then-hard-exact-budget-includes-both ()
  "A soft fragment ahead of a hard one is included whole when the budget is
exactly the soft text, separator, and hard text combined."
  (let* ((soft (superchat-prompt-fragment :owner 'a :id "s" :slot 'template :text "AB"))
         (hard (superchat-prompt-fragment :owner 'a :id "h" :slot 'continuation
                                           :text "H" :budget-class 'hard))
         (result (superchat-prompt-assemble (list soft hard) 5))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "AB\n\nH"))
    (should (equal (plist-get receipt :used) 5))
    (should (equal (plist-get receipt :chosen) '("s" "h")))
    (should (equal (plist-get receipt :omitted) nil))))

(ert-deftest prompt/mixed-soft-then-hard-one-over-budget-omits-soft ()
  "The same soft-then-hard pair, one character short of the budget the exact
case needs, omits the soft fragment and keeps the hard one -- the budget is
never exceeded."
  (let* ((soft (superchat-prompt-fragment :owner 'a :id "s" :slot 'template :text "AB"))
         (hard (superchat-prompt-fragment :owner 'a :id "h" :slot 'continuation
                                           :text "H" :budget-class 'hard))
         (result (superchat-prompt-assemble (list soft hard) 4))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "H"))
    (should (equal (plist-get receipt :used) 1))
    (should (<= (plist-get receipt :used) 4))
    (should (equal (plist-get receipt :chosen) '("h")))
    (should (equal (plist-get receipt :omitted) '("s")))))

(ert-deftest prompt/empty-hard-text-keeps-join-boundary ()
  "A hard fragment whose text is the empty string is still \"selected\" for
join-boundary purposes: a hard fragment after it gets its separator even
though the accumulated text before it has zero length."
  (let* ((empty (superchat-prompt-fragment :owner 'a :id "e" :slot 'context
                                            :text "" :budget-class 'hard))
         (b (superchat-prompt-fragment :owner 'a :id "b" :slot 'memory
                                        :text "B" :budget-class 'hard))
         (result (superchat-prompt-assemble (list empty b) 500))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "\n\nB"))
    (should (equal (plist-get receipt :used) 3))
    (should (equal (plist-get receipt :chosen) '("e" "b")))))

(ert-deftest prompt/refit-called-even-when-original-text-fits ()
  "A soft fragment with :refit is re-rendered exactly once even when its
original text would already have fit within the budget; the refit result,
not the original text, is what ends up in the assembled message."
  (let* ((calls 0)
         (f (superchat-prompt-fragment
             :owner 'a :id "r" :slot 'context :text "fits"
             :refit (lambda (_remaining) (setq calls (1+ calls)) "changed")))
         (result (superchat-prompt-assemble (list f) 100))
         (receipt (plist-get result :receipt)))
    (should (equal calls 1))
    (should (equal (plist-get (car (plist-get result :messages)) :content) "changed"))
    (should (equal (plist-get receipt :chosen) '("r")))
    (should (equal (plist-get receipt :refit) '("r")))))

(ert-deftest prompt/refit-non-string-result-is-invalid ()
  "A :refit function that returns a non-string signals `superchat-prompt-invalid'."
  (let ((f (superchat-prompt-fragment :owner 'a :id "r" :slot 'context :text "abc"
                                       :refit (lambda (_remaining) 42))))
    (should-error (superchat-prompt-assemble (list f) 100)
                  :type 'superchat-prompt-invalid)))

(ert-deftest prompt/refit-oversized-result-is-invalid ()
  "A :refit function whose result is longer than the REMAINING-CHARS it was
given signals `superchat-prompt-invalid' instead of silently exceeding the
budget."
  (let ((f (superchat-prompt-fragment
            :owner 'a :id "r" :slot 'context :text (make-string 300 ?z)
            :refit (lambda (remaining) (make-string (+ remaining 5) ?q)))))
    (should-error (superchat-prompt-assemble (list f) 80)
                  :type 'superchat-prompt-invalid)))

(ert-deftest prompt/refit-result-exactly-remaining-chars-is-accepted ()
  "A :refit result whose length is exactly REMAINING-CHARS is accepted -- the
boundary the oversized-result check (`>', not `>=') must not reject.
Between two hard fragments so REMAINING-CHARS is exactly determined by the
join boundaries on both sides; asserts the exact final message, :used, and
the receipt."
  (let* ((h1 (superchat-prompt-fragment :owner 'a :id "h1" :slot 'context
                                         :text "H1" :budget-class 'hard))
         (r (superchat-prompt-fragment
             :owner 'a :id "r" :slot 'memory :text "irrelevant"
             :refit (lambda (remaining) (make-string remaining ?Z))))
         (h2 (superchat-prompt-fragment :owner 'a :id "h2" :slot 'history
                                         :text "H2" :budget-class 'hard))
         ;; used-length after h1 = 2; sep-before = 2; sep-after = 2;
         ;; hard-ahead-render (h2 alone) = 2; remaining = 20 - 2 - 2 - 2 - 2 = 12.
         (result (superchat-prompt-assemble (list h1 r h2) 20))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get (car (plist-get result :messages)) :content)
                   (concat "H1\n\n" (make-string 12 ?Z) "\n\nH2")))
    (should (equal (plist-get receipt :used) 20))
    (should (equal (plist-get receipt :chosen) '("h1" "r" "h2")))
    (should (equal (plist-get receipt :omitted) nil))
    (should (equal (plist-get receipt :refit) '("r")))))

(ert-deftest prompt/chosen-includes-system-and-instruction-ids-before-user-ids ()
  "The receipt's :chosen list starts with the selected system/instruction
fragment ids, in total order, followed by the selected user-message ids."
  (let* ((sys (superchat-prompt-fragment :owner 'a :id "s1" :slot 'system :text "SYS"))
         (instr (superchat-prompt-fragment :owner 'a :id "i1" :slot 'instruction :text "INSTR"))
         (ctx (superchat-prompt-fragment :owner 'a :id "c1" :slot 'context :text "CTX"))
         (result (superchat-prompt-assemble (list instr ctx sys) 1000))
         (receipt (plist-get result :receipt)))
    (should (equal (plist-get receipt :chosen) '("s1" "i1" "c1")))))

(ert-deftest prompt/every-invalid-field-signals ()
  "Each field the contract declares a shape for signals `superchat-prompt-invalid'
at construction when given a value of the wrong shape, with condition data
exactly `(:field FIELD :value VALUE :id ID)' -- ID is the fragment's real id
except in the invalid-:id cases, where it is the offending id value itself."
  (let ((base (list :owner 'a :id "id1" :slot 'context :text "hi" :priority 0
                     :budget-class 'soft :required nil :refit nil :source nil)))
    ;; The base plist itself must be valid.
    (should (apply #'superchat-prompt-fragment base))
    (dolist (case (list (list :owner "not-a-symbol")
                         (list :owner nil)
                         (list :id 42)
                         (list :id "")
                         (list :slot 'nonsense)
                         (list :text 42)
                         (list :priority 1.5)
                         (list :priority "high")
                         (list :budget-class 'medium)
                         (list :required 'yes)
                         (list :refit "not-a-function")
                         (list :source "not-a-list")))
      (let* ((field (nth 0 case))
             (bad-value (nth 1 case))
             (args (plist-put (copy-sequence base) field bad-value))
             (expected-id (if (eq field :id) bad-value "id1"))
             (err (should-error (apply #'superchat-prompt-fragment args)
                                 :type 'superchat-prompt-invalid)))
        (should (equal (cdr err) (list :field field :value bad-value :id expected-id)))))))

(ert-deftest prompt/sort-fragments-validates-raw-plists-too ()
  "`superchat-prompt-sort-fragments' revalidates every field even for a
fragment plist that was never built through `superchat-prompt-fragment', so
a malformed plist from any other source is still caught, with the same
exact `(:field FIELD :value VALUE :id ID)' condition data."
  (let ((base (list :owner 'a :id "id1" :slot 'context :text "hi" :priority 0
                     :budget-class 'soft :required nil :refit nil :source nil)))
    (dolist (case (list (list :text 42)
                         (list :priority "nope")
                         (list :budget-class 'unknown)))
      (let* ((field (nth 0 case))
             (bad-value (nth 1 case))
             (raw (plist-put (copy-sequence base) field bad-value))
             (err (should-error (superchat-prompt-sort-fragments (list raw))
                                 :type 'superchat-prompt-invalid)))
        (should (equal (cdr err) (list :field field :value bad-value :id "id1")))))))

(ert-deftest prompt/property-random-fragments-respect-budget-invariant ()
  "For many random small fragment sets and budgets, a successful assembly
always satisfies :used <= :budget and :used = (length of the assembled
user message) -- the general form of the budget invariant fixed findings
1-4 violated."
  (random "superchat-prompt-property-seed")
  (let ((slots '(context memory history plan template continuation))
        (owners '(a b c)))
    (dotimes (iteration 200)
      (let* ((count (random 6))
             (budget (random 31))
             (fragments
              (cl-loop for k from 0 below count
                       collect
                       (superchat-prompt-fragment
                        :owner (nth (random (length owners)) owners)
                        :id (format "f-%d-%d" iteration k)
                        :slot (nth (random (length slots)) slots)
                        :text (make-string (random 11) ?a)
                        :priority (- (random 11) 5)
                        :budget-class (if (zerop (random 4)) 'hard 'soft)))))
        (condition-case nil
            (let* ((assembled (superchat-prompt-assemble fragments budget))
                   (receipt (plist-get assembled :receipt))
                   (used (plist-get receipt :used))
                   (message-text (plist-get (car (plist-get assembled :messages)) :content)))
              (should (<= used budget))
              (should (equal used (length message-text))))
          (superchat-prompt-over-budget nil))))))

(ert-deftest prompt/assemble-prompt-stage-is-registered-unconditionally ()
  "Loading `superchat-prompt' alone (this file's own top-level `require')
registers `core-assembler' on the `assemble-prompt' stage, owner `core',
with no `superchat-stage-add' guard (S03a-fix-1: the file now requires
`superchat-stage' itself instead of load-order-guarding a
`(fboundp 'superchat-stage-add)' check that could silently never fire)."
  (let* ((report (superchat-stage-report))
         (entry (cl-find 'assemble-prompt (plist-get report :stages)
                          :key (lambda (s) (plist-get s :stage)))))
    (should entry)
    (should (equal '((:id core-assembler :owner core :required nil))
                    (plist-get entry :implementations)))
    (let ((fragments (list (superchat-prompt-fragment
                             :owner 'core :id "x" :slot 'template
                             :text "hello" :budget-class 'hard))))
      (should (equal (superchat-prompt-assemble fragments 40)
                      (superchat-stage-call 'assemble-prompt fragments 40))))))

(provide 'test-prompt)

;;; test-prompt.el ends here
