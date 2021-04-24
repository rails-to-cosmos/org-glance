(require 'load-relative)
(require 'nadvice)
(require 'org-glance-view)

(defun org-glance-view-metadata-write-visit (&rest args)
  (let* ((headline (org-element-at-point))
         (visit-count-str (or (org-element-property :ORG_GLANCE_VISIT_COUNT headline) "0"))
         (visit-count-num (1+ (string-to-number visit-count-str))))
    (org-set-property "ORG_GLANCE_VISIT_COUNT" (number-to-string visit-count-num))
    (org-set-property "ORG_GLANCE_VISIT_LAST" (format-time-string "%Y-%m-%d %H:%M:%S"))
    (save-buffer)))

(advice-add 'org-glance-action-visit :after #'org-glance-view-metadata-write-visit)

(provide-me)
