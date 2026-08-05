;;; self-check.el --- DeepSeek Experience Cohort checks -*- lexical-binding: t; -*-

(require 'superchat-experience-cohort-deepseek-v6-run)
(require 'superchat-experience-cohort-deepseek-v6-evidence)

(let* ((assignments (superchat-experience-cohort-deepseek-v6--assignments))
       (manifest (superchat-experience-cohort-deepseek-v6--manifest)))
  (unless
      (and (= 20 (length assignments))
           (equal (number-sequence 1 20)
                  (mapcar (lambda (item) (plist-get item :run_index)) assignments))
           (= 4 (cl-count "calibration" assignments
                          :key (lambda (item) (plist-get item :split))
                          :test #'equal))
           (= 12 (cl-count "discovery" assignments
                           :key (lambda (item) (plist-get item :split))
                           :test #'equal))
           (= 4 (cl-count "replicate" assignments
                          :key (lambda (item) (plist-get item :split))
                          :test #'equal))
           (equal "cohort-authorized" (plist-get manifest :status))
           (equal "sealed" (plist-get (plist-get manifest :held_out) :status))
           (not (string-match-p
                 "heldout-tasks.json"
                 (superchat-experience-cohort-deepseek-v6--read-file
                  superchat-experience-cohort-deepseek-v6--manifest-path)))
           (eq 'none superchat-llm-reasoning)
           (= 30 llm-request-plz-connect-timeout)
           (equal "/opt/local/bin/curl" plz-curl-program)
           (equal '("--silent" "--compressed" "--location"
                    "--proxy" "http://127.0.0.1:7897"
                    "--noproxy" "localhost,127.0.0.1"
                    "--suppress-connect-headers"
                    "--retry" "2" "--retry-all-errors"
                    "--retry-delay" "1" "--retry-max-time" "90")
                  plz-curl-default-args)
           (advice-member-p
            #'superchat--llm-request-plz-propagate-callback-errors
            'llm-request-plz-async)
           (equal superchat-experience-cohort-deepseek-v6--allowed-tools
                  (plist-get manifest :model_tool_surface))
           (string-match-p
            "assignment cal-systems-01; split calibration"
            (superchat-experience-cohort-deepseek-v6--query (car assignments)))
           (not (superchat-experience-cohort-deepseek-v6--calibration-pass-p)))
    (error "DeepSeek Experience Cohort self-check failed")))

(princ "DeepSeek Experience Cohort self-check passed\n")

(provide 'superchat-experience-cohort-deepseek-v6-self-check)

;;; self-check.el ends here
