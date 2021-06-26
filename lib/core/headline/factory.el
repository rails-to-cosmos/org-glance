(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.utils.helpers)

;; Refactor with explicit metastore

(cl-defgeneric org-glance-headline (obj)
  "Headline factory.")

(cl-defmethod org-glance-headline ((obj string))
  (org-glance-metastore:headline obj))

(cl-defmethod org-glance-headline ((obj symbol))
  (org-glance-headline (symbol-name obj)))

(cl-defmethod org-glance-headline ((obj list))
  (unless (org-element-property :file obj)
    (org-element-put-property obj :file (buffer-file-name)))
  obj)

(cl-defmethod org-glance-headline ((obj null))
  "Extract headline from point."
  (-some->> (org-glance-headline:at-point)
    (org-glance-headline:id)
    (org-glance-metastore:headline)))

;; (progn
;;   (let ((hl (org-glance-headline "Location-20210515-70018c32c7ccff963dc80983481a4557")))
;;     (save-window-excursion
;;       (org-glance-headline:visit hl)
;;       ;; (org-glance-headline:file (org-glance-headline (org-element-at-point)))
;;       (org-glance-headline:contents (org-glance-headline (org-element-at-point)))
;;       )))

(org-glance-module-provide)
