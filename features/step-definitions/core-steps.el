(require 'org-glance)
(require 'org-element)
(require 's)
(require 'with-simulated-input)

(When "^I define class \"\\([^\"]+\\)\"$"
  (lambda (class)
    (org-glance:create-tag (intern class))))

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
  (lambda (thing-title class-name)
    (with-simulated-input ((insert class-name) "RET")
      (org-glance-capture))
    (insert thing-title)
    (org-capture-finalize)))

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\" with contents$"
  (lambda (thing-title class-name contents)
    (with-simulated-input ((insert class-name) "RET")
      (org-glance-capture))
    (insert thing-title)
    (goto-char (point-max))
    (insert "\n")
    (insert contents)
    (org-capture-finalize)))

(Then "^I should have \\([[:digit:]]+\\) active classe?s?$"
      (lambda (count)
        (should (= (hash-table-count org-glance-tags) (string-to-number count)))))

(Then "^I should be in the buffer \"\\([^\"]+\\)\"$"
      (lambda (buffer-name)
        (should (string= buffer-name (buffer-name)))))

(And "^I kill buffer \"\\([^\"]+\\)\"$"
     (lambda (buffer)
       (kill-buffer buffer)))

(And "^there is 1 headline here$"
     (lambda ()
       (should (= 1 (length (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity))))))

(And "^there are no headlines here$"
     (lambda ()
       (should (= 0 (length (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity))))))

(Then "^headline at point should contain links$"
      (lambda () (should (->> (org-glance-headline:at-point)
                         org-glance-headline:id
                         org-glance-metadata:get-headline
                         org-glance-headline:linked?))))

(Then "^headline at point should not be encrypted$"
      (lambda () (should (not (org-glance-headline:encrypted?)))))

(Then "^headline at point should not contain properties$"
      (lambda ()
        (should (not (org-glance-headline:propertized?)))))

(Then "^headline at point should not be archived$"
      (lambda ()
        (should (not (org-glance-headline:archived?)))))

(Then "^headline at point should not be commented$"
      (lambda ()
        (should (not (org-glance-headline:commented?)))))

(And "^I rename headline to \"\\([^\"]+\\)\"$"
     (lambda (new-title)
       (should (org-glance--back-to-heading))
       (replace-string (org-glance-headline:plain-title) new-title nil (point-min) (save-excursion (end-of-line) (point)))))

(And "^I sync materialized headline$"
     (lambda ()
       (org-glance-materialized-headline-apply)))

(And "^I change tag \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
     (lambda (from-tag to-tag)
       (org-toggle-tag from-tag 'off)
       (org-toggle-tag to-tag 'on)))

(And "^I kill current buffer$"
     (lambda ()
       (kill-buffer)))
