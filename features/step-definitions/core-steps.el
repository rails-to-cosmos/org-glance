(require 'org-glance)
(require 'org-element)
(require 's)
(require 'with-simulated-input)

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
  (lambda (thing-title class-name)
    (with-simulated-input ((insert thing-title) "RET" (insert class-name) "RET")
      (org-glance:reschedule-or-capture))
    (insert thing-title)
    (org-capture-finalize)))

(Then "^I should have \\([[:digit:]]+\\) active classe?s?$"
      (lambda (count)
        (should (= (hash-table-count org-glance:classes) (string-to-number count)))))

(Then "^I should have \\([[:digit:]]+\\) things? of class \"\\([^\"]+\\)\" registered$"
      (lambda (count class)
        (should (= (length (org-glance-view:headlines (intern class)))
                   (string-to-number count)))))

(Then "^I should be in the buffer \"\\([^\"]+\\)\"$"
      (lambda (buffer-name)
        (should (string= buffer-name (buffer-name)))))

(And "^I kill buffer \"\\([^\"]+\\)\"$"
     (lambda (buffer)
       (kill-buffer buffer)))

(And "^there is 1 headline here$"
  (lambda ()
    (should (= 1 (length (org-element-map (org-element-parse-buffer 'headline) 'headline (lambda (el) el)))))))

(And "^there are no headlines here$"
  (lambda ()
    (should (= 0 (length (org-element-map (org-element-parse-buffer 'headline) 'headline (lambda (el) el)))))))
