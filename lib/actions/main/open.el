(require 'org-glance-module)

(org-glance:require lib.core.actions)
(org-glance:require lib.core.headline)

(org-glance-action-define open (headline) :for link
  "Visit HEADLINE, then search for all occuriences of `org-any-link-re' in its subtree.
Ask with `org-completing-read' and apply `org-open-at-point' on it."
  (let* ((links (org-glance-headline:links headline))
         (pos (cond
                ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                ((= (length links) 1) (cdar links))
                (t (user-error "Unable to find links in %s" (org-glance-headline:file headline))))))
    (org-glance-headline:narrow headline
      (goto-char pos)
      (org-open-at-point))))

(org-glance:provide)
