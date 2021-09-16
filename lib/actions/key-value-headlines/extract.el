(require 'org-glance-module)

(org-glance:require lib.core.actions)
(org-glance:require lib.utils.helpers)

(org-glance-action-define extract (headline) :for kvs
  "Completing read all properties from HEADLINE and its successors to kill ring."
  (let ((key-value-pairs (save-window-excursion
                           (org-glance-action-call 'materialize :on headline)
                           (unwind-protect
                                (org-glance:get-buffer-key-value-pairs)
                             (with-demoted-errors "Unable to kill buffer: %s"
                               (kill-buffer (org-glance-headline:materialized-buffer-name headline)))))))
    (while t
      (kill-new (alist-get (org-completing-read "Extract property: " key-value-pairs) key-value-pairs nil nil #'string=)))))

(org-glance:provide)
