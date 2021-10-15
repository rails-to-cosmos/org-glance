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

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\" with contents$"
  (lambda (thing-title class-name contents)
    (with-simulated-input ((insert thing-title) "RET"
                           (insert class-name) "RET")
      (org-glance:reschedule-or-capture))
    (insert thing-title)
    (goto-char (point-max))
    (insert "\n")
    (insert contents)
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

(Then "^headline at point should contain links$"
      (lambda () (should (->> (org-glance-headline:at-point)
                         org-glance-headline:id
                         org-glance-metastore:get-headline
                         org-glance-headline:linked?))))

(Then "^headline at point should not be encrypted$"
      (lambda () (should (not (org-glance-headline:encrypted?)))))

(Then "^headline at point should not contain properties$"
      (lambda ()
        (should (not (org-glance-headline:kvp?)))))

(Then "^headline at point should not be archived$"
      (lambda ()
        (should (not (org-glance-headline:archived?)))))

(Then "^headline at point should not be commented$"
      (lambda ()
        (should (not (org-glance-headline:commented?)))))
