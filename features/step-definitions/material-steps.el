(require 'org-glance)
(require 'with-simulated-input)

(Then "^I materialize \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
      (lambda (thing class)
        (with-simulated-input ((insert "[" class "]") "SPC" (insert thing) "RET")
          (org-glance:materialize))))
