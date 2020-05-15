;;; test-helper.el --- Helpers for org-glance-test.el

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'org-glance)

;;; test-helper.el ends here
