;;; remediation-prototype-v0.2.el --- PROTOTYPE TUI for strict scorer -*- lexical-binding: t; -*-

;; Question: does a small offline scorer fail closed on every reproduced v0.1
;; record-integrity, first-attempt, proof, schedule, grammar, sham, and oracle
;; counterexample before any Runtime or provider work is authorized?
;;
;; THROWAWAY — no Superchat, Runtime, Workspace, Tape, database, network, or
;; provider is loaded.  From the repository root:
;;
;;   emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el audit
;;
;; `broken' deliberately damages the frozen schedule and must exit 1.

(require 'cl-lib)
(require 'subr-x)

(let ((prototype-directory
       (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "remediation-logic-v0.2.el"
                          prototype-directory)
        nil t))

(defvar semuse-v02-ui-state
  '(:row-index 0 :condition-index 0 :scenario-index 0))

(defun semuse-v02-ui--selected (items key)
  "Return the selected member of ITEMS for state KEY." 
  (nth (plist-get semuse-v02-ui-state key) items))

(defun semuse-v02-ui--cycle (key items delta)
  "Move state KEY by DELTA through ITEMS." 
  (setq semuse-v02-ui-state
        (plist-put semuse-v02-ui-state key
                   (mod (+ (plist-get semuse-v02-ui-state key) delta)
                        (length items)))))

(defun semuse-v02-ui--field (name value)
  "Print one readable NAME/VALUE field." 
  (princ (format "\033[1m%-24s\033[0m %s\n"
                 (concat name ":")
                 (if (stringp value) value (prin1-to-string value)))))

(defun semuse-v02-ui-render (&optional clear)
  "Render the selected scorer state; CLEAR the terminal when non-nil." 
  (when clear
    (princ "\033[2J\033[H"))
  (let* ((row (semuse-v02-ui--selected semuse-v02-rows :row-index))
         (condition
          (semuse-v02-ui--selected semuse-v02-conditions :condition-index))
         (scenario
          (semuse-v02-ui--selected semuse-v02-scenarios :scenario-index))
         (records (semuse-v02-synthetic-records scenario))
         (record (semuse-v02--record-at-list records (plist-get row :id)
                                             condition))
         (record-set (semuse-v02-record-set-gate
                      records semuse-v02-frozen-schedule))
         (first (semuse-v02-first-attempt-gate record))
         (envelope (semuse-v02-provider-envelope-gate record))
         (cohort (semuse-v02-cohort-result records
                                           semuse-v02-frozen-schedule))
         (effect (semuse-v02-effect-pair row records
                                         semuse-v02-frozen-schedule)))
    (princ "ARTIFACT SEMANTIC USE v0.2 — STRICT OFFLINE REMEDIATION\n\n")
    (semuse-v02-ui--field "Claim mode" semuse-v02-claim-mode)
    (semuse-v02-ui--field "Row" (plist-get row :id))
    (semuse-v02-ui--field "Family" (plist-get row :family))
    (semuse-v02-ui--field "Task-only x" (semuse-v02-row-x row))
    (semuse-v02-ui--field "Condition" condition)
    (semuse-v02-ui--field "Scenario" scenario)
    (semuse-v02-ui--field "Scorer target" (semuse-v02-target row condition))
    (semuse-v02-ui--field "Raw first output"
                          (plist-get record :raw-output))
    (semuse-v02-ui--field "Raw bytes SHA-256"
                          (plist-get record :raw-output-sha256))
    (semuse-v02-ui--field "Record-set gate"
                          (plist-get record-set :status))
    (semuse-v02-ui--field "First-attempt gate"
                          (plist-get first :status))
    (semuse-v02-ui--field "Content observation"
                          (plist-get cohort :content-status))
    (semuse-v02-ui--field "Content cells"
                          (and (plist-get cohort :content)
                               (length (plist-get
                                        (plist-get cohort :content)
                                        :content-cells))))
    (semuse-v02-ui--field "Sham control"
                          (plist-get cohort :sham-status))
    (semuse-v02-ui--field "A/withheld pair"
                          (plist-get effect :status))
    (semuse-v02-ui--field "Oracle/envelope gate"
                          (plist-get envelope :status))
    (semuse-v02-ui--field "Overall closure"
                          (plist-get cohort :status))
    (semuse-v02-ui--field "Schedule SHA-256"
                          (semuse-v02-schedule-hash
                           semuse-v02-frozen-schedule))
    (princ "\n\033[2mNo model/provider state is used.  IDs and targets remain scorer-only.\033[0m\n")
    (princ (concat "\n\033[1m[n/p]\033[0m row  \033[1m[c]\033[0m condition  "
                   "\033[1m[s]\033[0m scenario  \033[1m[q]\033[0m quit\n"))))

(defun semuse-v02-ui-loop ()
  "Run the line-oriented inspection UI." 
  (let ((running t))
    (while running
      (semuse-v02-ui-render t)
      (condition-case nil
          (pcase (downcase (string-trim (read-string "> ")))
            ("n" (semuse-v02-ui--cycle :row-index semuse-v02-rows 1))
            ("p" (semuse-v02-ui--cycle :row-index semuse-v02-rows -1))
            ("c" (semuse-v02-ui--cycle
                   :condition-index semuse-v02-conditions 1))
            ("s" (semuse-v02-ui--cycle
                   :scenario-index semuse-v02-scenarios 1))
            ("q" (setq running nil))
            (_ nil))
        (end-of-file (setq running nil))))))

(defun semuse-v02-ui-print-audit (result)
  "Print RESULT as stable line-oriented audit output." 
  (princ (format "audit=%s\n" (if (plist-get result :ok) "PASS" "FAIL")))
  (dolist (check (plist-get result :checks))
    (princ (format "%s=%s\n"
                   (car check) (if (cdr check) "PASS" "FAIL"))))
  (princ (format "schedule_sha256=%s\n"
                 (plist-get result :schedule-hash)))
  (princ (format "passing_status=%s\n"
                 (plist-get (plist-get result :passing) :status)))
  (princ (format "v_only_status=%s\n"
                 (plist-get (plist-get result :v-only) :status)))
  (princ (format "x_only_status=%s\n"
                 (plist-get (plist-get result :x-only) :status)))
  (princ (format "missing_withheld_status=%s difference=%S\n"
                 (plist-get (plist-get result :missing-withheld-pair) :status)
                 (plist-get (plist-get result :missing-withheld-pair)
                            :difference))))

(defun semuse-v02-ui-audit ()
  "Run the full repair audit and terminate with its result." 
  (let ((result (semuse-v02-audit)))
    (semuse-v02-ui-print-audit result)
    (kill-emacs (semuse-v02-exit-code result))))

(defun semuse-v02-ui-broken ()
  "Damage the frozen schedule and prove validation exits nonzero." 
  (let* ((validation
          (semuse-v02-schedule-validation
           (butlast semuse-v02-frozen-schedule)))
         (result
          (list :ok (plist-get validation :ok)
                :checks
                (list (cons 'damaged-schedule
                            (plist-get validation :ok)))
                :schedule-hash (plist-get validation :hash))))
    (semuse-v02-ui-print-audit result)
    (kill-emacs (semuse-v02-exit-code result))))

(cond
 ((member "audit" command-line-args-left) (semuse-v02-ui-audit))
 ((member "broken" command-line-args-left) (semuse-v02-ui-broken))
 (t (semuse-v02-ui-loop)))

;;; remediation-prototype-v0.2.el ends here
