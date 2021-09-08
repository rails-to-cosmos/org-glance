(require 'org-glance-module)

(org-glance:require lib.core.view
                    lib.utils.helpers
                    lib.core.actions)

(org-glance-action-define materialize (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
              (setq-local --org-glance-materialized-headline:password (read-passwd "Password: "))
              (org-glance-headline:decrypt --org-glance-materialized-headline:password)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt)
    (unwind-protect
         (progn
           (org-glance-action-call 'materialize :on headline)
           (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (org-glance-headline:demote --org-glance-materialized-headline:indent)
              (org-glance-headline:encrypt --org-glance-materialized-headline:password)
              (--org-glance-headline:promote.deprecated))
            0 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (org-glance-headline:demote --org-glance-materialized-headline:indent)
              (org-glance-headline:decrypt --org-glance-materialized-headline:password)
              (--org-glance-headline:promote.deprecated))
            0 'local))

(org-glance:provide)
