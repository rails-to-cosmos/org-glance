(require 'pythonic-import)

(pythonic-import lib.core.serde)
(pythonic-import lib.core.actions)

(require 'org-glance-view)

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
