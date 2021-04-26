(require 'org-glance-module)

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.actions)

(org-glance-action-define materialize (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
              (setq-local --org-glance-view-pwd (read-passwd "Password: "))
              (org-glance-decrypt-subtree --org-glance-view-pwd)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt t)
    (unwind-protect
         (progn
           (org-glance-action-call 'materialize :on headline)
           (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (org-glance--demote-subtree --org-glance-view-indent)
              (org-glance-encrypt-subtree --org-glance-view-pwd)
              (org-glance--promote-subtree))
            'append 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (org-glance--demote-subtree --org-glance-view-indent)
              (org-glance-decrypt-subtree --org-glance-view-pwd)
              (org-glance--promote-subtree))
            'append 'local))

(org-glance-module-provide)
