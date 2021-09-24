(require 'org-glance-module)

(org-glance:require
  lib.core.actions
  lib.core.view)

(org-glance-action-define extract (headline) :for (kvs crypt)
  "Materialize HEADLINE, decrypt it, then run completing read on all properties to kill ring."
  (let ((key-value-pairs (save-window-excursion
                           (org-glance-headline:materialize headline)
                           (unwind-protect
                                (org-glance:get-buffer-key-value-pairs)
                             (with-demoted-errors "Unable to kill buffer: %s"
                               (kill-buffer (org-glance-headline:materialized-buffer-name headline)))))))
    (while t
      (kill-new (alist-get (org-completing-read "Extract property: " key-value-pairs) key-value-pairs nil nil #'string=)))))

(org-glance:provide)
