(require 'org-glance)
(require 'with-simulated-input)

(And "^I materialize headline at point$"
     (lambda ()
       (org-glance-headline:materialize (org-glance-headline:at-point))))

(Then "^I materialize \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
      (lambda (thing class)
        (with-simulated-input ((insert "[" class "]") "SPC" (insert thing) "RET")
          (org-glance:materialize)
          (org-glance-headline:search-forward))))
