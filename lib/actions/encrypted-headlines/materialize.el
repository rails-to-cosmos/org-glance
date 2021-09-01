(require 'org-glance-module)

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.actions)

(org-glance-action-define materialize (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
              (setq-local --org-glance-view-pwd (read-passwd "Password: "))
              (org-glance-headline:decrypt --org-glance-view-pwd)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt)
    (unwind-protect
         (progn
           (org-glance-action-call 'materialize :on headline)
           (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (org-glance-headline:demote --org-glance-view-indent)
              (org-glance-headline:encrypt --org-glance-view-pwd)
              (--org-glance-headline:promote.deprecated))
            0 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (org-glance-headline:demote --org-glance-view-indent)
              (org-glance-headline:decrypt --org-glance-view-pwd)
              (--org-glance-headline:promote.deprecated))
            0 'local))

(org-glance-module-provide)
