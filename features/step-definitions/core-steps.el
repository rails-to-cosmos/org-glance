(require 'org-glance)
(require 's)
(require 'with-simulated-input)

(When "^I capture thing \"\\([^\"]+\\)\" of class \"\\([^\"]+\\)\"$"
  (lambda (thing-title class-name)
    (with-simulated-input (s-join " RET " (list thing-title class-name))
      (org-glance:reschedule-or-capture))))
