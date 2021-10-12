(require 'org-glance)
(require 's)
(require 'with-simulated-input)

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
  (lambda (thing-title class-name)
    (with-simulated-input ((insert thing-title) "RET" (insert class-name) "RET" (insert thing-title))
      (org-glance:reschedule-or-capture))
    (org-capture-finalize)))

(Then "^I should have \\([[:digit:]]+\\) classe?s? active$"
      (lambda (count)
        (= (hash-table-count org-glance:classes) (string-to-number count))))

(Then "^I should have \\([[:digit:]]+\\) things? of class \"\\([^\"]+\\)\" registered$"
      (lambda (count class)
        (= (length (org-glance-view:headlines (intern class)))
           (string-to-number count))))
