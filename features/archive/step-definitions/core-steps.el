(require 'org-glance)
(require 'org-element)
(require 's)
(require 'with-simulated-input)

(When "^I create a file called \"\\([^\"]+\\)\" with content:$"
  (lambda (filename content)
    (with-temp-file (f-join org-glance-test:user-location filename)
      (insert content))))

(And "^I find file \"\\([^\"]+\\)\"$"
     (lambda (filename)
       (find-file (f-join org-glance-test:user-location filename))))

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
  (lambda (thing-title class-name)
    (with-simulated-input ((insert class-name) "RET")
      (org-glance-overview:capture))
    (insert thing-title)
    (org-capture-finalize)))

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\" with contents$"
  (lambda (thing-title class-name contents)
    (with-simulated-input ((insert class-name) "RET")
      (org-glance-overview:capture))
    (insert thing-title)
    (goto-char (point-max))
    (insert "\n")
    (insert contents)
    (org-capture-finalize)))

(Then "^I should have \\([[:digit:]]+\\) active classe?s?$"
      (lambda (count)
        (should (= (hash-table-count org-glance-classes) (string-to-number count)))))

(Then "^I should have \\([[:digit:]]+\\) things? of class \"\\([^\"]+\\)\" registered$"
      (lambda (count class-name)
        (should (= (length (org-glance-view:headlines (org-glance-headline:string-to-class class-name)))
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
      (lambda () (should (->> (org-glance-headline-at-point)
                         org-glance-headline:id
                         org-glance-metastore:get-headline
                         org-glance-headline:contains-link-p))))

(Then "^headline at point should not be encrypted$"
      (lambda () (should (not (org-glance-headline:encrypted?)))))

(Then "^headline at point should not contain properties$"
      (lambda ()
        (should (not (org-glance-headline:contains-property-p)))))

(Then "^headline at point should not be archived$"
      (lambda ()
        (should (not (org-glance-headline:archived-p)))))

(Then "^headline at point should not be commented$"
      (lambda ()
        (should (not (org-glance-headline:commented-p)))))

(And "^I rename headline to \"\\([^\"]+\\)\"$"
     (lambda (new-title)
       (should (org-glance:ensure-at-heading))
       (replace-string (org-glance-headline:title) new-title nil (point-min) (save-excursion (end-of-line) (point)))))

(And "^I sync materialized headline$"
     (lambda ()
       (org-glance-materialized-headline:sync)))

(And "^I change tag \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
     (lambda (from-tag to-tag)
       (org-toggle-tag from-tag 'off)
       (org-toggle-tag to-tag 'on)))

(And "^I kill current buffer$"
     (lambda ()
       (kill-buffer)))