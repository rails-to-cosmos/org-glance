;;; test-config-env.el --- Configuration fixture environment -*- lexical-binding: t; -*-

(defun org-glance-config-test:getenv (suffix)
  "Read config fixture option SUFFIX, preferring the canonical OG_CONFIG_ name."
  (or (getenv (concat "OG_CONFIG_" suffix))
      (getenv (concat "GLANCE_CONFIG_" suffix))))

(provide 'test-config-env)
;;; test-config-env.el ends here
