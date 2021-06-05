(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.actions)

(org-glance-action-define insert (headline) :for babel
  "Visit HEADLINE, get contents and insert it."
  (insert (save-window-excursion
            (save-excursion
              (org-glance-action-call 'visit :on headline)
              (org-babel-next-src-block)
              (org-narrow-to-block)
              (buffer-substring-no-properties
               (save-excursion (goto-char (point-min))
                               (forward-line)
                               (point))
               (save-excursion (goto-char (point-max))
                               (forward-line -1)
                               (end-of-line)
                               (point)))))))

(org-glance-module-provide)
