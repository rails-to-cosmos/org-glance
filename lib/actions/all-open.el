(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.actions)
(org-glance-module-import lib.core.view)

(org-glance-action-define open (headline) :for link
  "Search for `org-any-link-re' under the HEADLINE
then run `org-completing-read' to open it."
  (org-glance-with-headline-narrowed headline
      (let* ((links (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (cons
                         (substring-no-properties
                          (or (nth 2 link) ;; link alias
                              (org-element-property :raw-link link))) ;; full link if alias is none
                         (org-element-property :begin link)))))
             (point (cond
                      ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                      ((= (length links) 1) (cdar links))
                      (t (user-error "Unable to find links in %s" (buffer-file-name))))))
        (goto-char point)
        (org-open-at-point))))

(org-glance-module-provide)
