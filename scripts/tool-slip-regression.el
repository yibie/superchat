;;; tool-slip-regression.el --- a slip in a tool call costs one retry, not the Run -*- lexical-binding: t; -*-

;;; Commentary:
;; In the user's GUI Emacs on 2026-10-23 a work round of 90 Steps failed at
;; its third malformed tool call (`read_buffer' without `buffer'): the two
;; repairs `superchat-llm-unknown-tool-retries' allows had been spent at
;; Steps 43 and 87, with good calls between.  The limit counts slips in a
;; row, as the cut-off streak does: a reply that goes through starts it over.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -L scripts -l scripts/tool-slip-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^tool-slip/")'

;;; Code:

(load (expand-file-name "tool-hooks-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-llm-unknown-tool-retries)

(defconst ts--slip
  '(:error (llm-tool-missing-argument
            . "Missing required argument 'buffer' for tool 'read_buffer'")))

(defconst ts--read
  '(:tool-uses (("read_buffer" (buffer . "hm-target")))))

(ert-deftest tool-slip/slips-apart-are-each-repaired ()
  "Three slips, a good call between each: the Run completes."
  (let ((superchat-llm-unknown-tool-retries 2))
    (th--run (list ts--slip ts--read ts--slip ts--read ts--slip
                   '(:text "TS-DONE"))
        nil
      (should (= 6 (length smoke--requests)))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest tool-slip/slips-in-a-row-past-the-limit-end-the-run ()
  "The limit still holds for slips one after another."
  (let ((superchat-llm-unknown-tool-retries 2))
    (th--run (list ts--read ts--slip ts--slip ts--slip '(:text "TS-NEVER"))
        nil
      (should (= 4 (length smoke--requests)))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status))))))

;;; tool-slip-regression.el ends here
