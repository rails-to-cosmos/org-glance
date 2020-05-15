;;; test-helper.el --- Helpers for org-glance-test.el

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'org-glance)
(require 'org-glance-test-init)
(require 'org-glance-test-helpers)

;;; test-helper.el ends here
