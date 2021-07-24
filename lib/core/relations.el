(require 'org-glance-module)

(defconst org-glance-relation-re
  (rx (seq bol
           (0+ (any "\t -"))
           "Related to"
           (0+ (any "\t *"))
           (group (0+ word))
           (0+ (any "\t *"))
           "="
           (group (1+ (any word)))
           "="
           (0+ (any "\t "))
           "[[org-glance-visit:"
           (group (1+ (not "]")))
           "]["
           (group (1+ (not "]")))
           "]]"
           (0+ (any "\t "))
           "on"
           (0+ (any "\t "))
           (regexp org-ts-regexp-inactive)
           (0+ (any "\t "))
           eol))
  "Matches relation.")

(cl-defun org-glance-headline:next-relation ()
  (condition-case nil
      (re-search-forward org-glance-relation-re)
    (error nil)))

(cl-defun org-glance-headline:relations (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (org-glance-headline:get-or-search-backward)
    (cl-loop
       while (org-glance-headline:next-relation)
       for view-id = (substring-no-properties (match-string 3))
       for relation = (condition-case nil
                          (org-glance-metastore:get-headline-by-id view-id)
                        (error nil))
       when relation
       collect relation)))

(cl-defun org-glance:add-relation (&optional
                                     (source (org-glance-headline:at-point))
                                     (relation "Related to")
                                     (target (org-glance-metastore:choose-headline)))
  (interactive)
  (let* ((target-id (org-glance-headline:id target))
         (target-state (org-glance-headline:state target))
         (target-label (if (string-empty-p target-state) "" (format " *%s*" target-state)))
         (target-title (s-replace-regexp (format "^%s\\W*" target-state) "" (org-glance-headline:format target)))
         (target-views (s-join ", " (org-glance-headline:view-ids target)))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
    (org-glance-headline:add-log-note
     (org-glance:format
      "- ${relation}${target-label} =${target-views}= [[org-glance-visit:${target-id}][${target-title}]] on ${now}")
     source)))

(org-glance-module-provide)
