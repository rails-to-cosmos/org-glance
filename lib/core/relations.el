(require 'org-glance-module)

(defconst org-glance-relation:forward "Related to")
(defconst org-glance-relation:backward "Referred from")
(defconst org-glance-relation-re
  (rx (seq bol
           (0+ (any "\t -"))
           (or (literal org-glance-relation:forward)
               (literal org-glance-relation:backward))
           (0+ (any "\t *"))
           (group-n 1 (0+ word)) ;; state
           (0+ (any "\t *"))
           "="
           (group-n 2 (1+ (any word))) ;; category
           "="
           (0+ (any "\t "))
           "[[org-glance-visit:"
           (group-n 3 (1+ (not "]"))) ;; id
           "]["
           (group-n 4 (1+ (not "]"))) ;; title
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

(cl-defun org-glance-headline:relation-category ()
  (substring-no-properties (match-string 2)))

(cl-defun org-glance-headline:relation-id ()
  (substring-no-properties (match-string 3)))

(cl-defun org-glance-headline:relations (&optional (headline (org-glance-headline:at-point)))
  "Get all first-level relations of HEADLINE."
  (org-glance-headline:narrow headline
    (cl-loop
       while (org-glance-headline:next-relation)
       for id = (org-glance-headline:relation-id)
       for relation = (org-glance-metastore:get-headline id)
       when relation
       collect relation)))

(cl-defun org-glance-headline:relations* (&optional (headline (org-glance-headline:at-point)))
  "Get all relations of HEADLINE recursively."
  (let* ((visited (make-hash-table :test 'equal))
         (headlines (list headline)))
    (cl-loop
       while headlines
       for headline = (pop headlines)
       for relations = (org-glance-headline:relations headline)
       for id = (org-glance-headline:id headline)
       for title = (org-glance-headline:title headline)
       for relation-titles = (mapcar #'org-glance-headline:title relations)
       unless (gethash id visited nil)
       collect (cons title relation-titles)
       into result
       do
         (puthash id t visited)
         (cl-loop
            for relation in relations
            for relation-id = (org-glance-headline:id relation)
            unless (gethash relation-id visited nil)
            do
              (push relation headlines)
              (pp (mapcar #'org-glance-headline:title headlines)))
       finally (return result))))

(cl-defun org-glance:add-relation (&optional
                                     (source (org-glance-headline:at-point))
                                     (relation org-glance-relation:forward)
                                     (target (org-glance-metastore:choose-headline)))
  (interactive)
  (let ((log-entry (org-glance-headline:format target
                     :format "- ${relation}${label}=${classes}= [[org-glance-visit:${id}][${title}]] on ${now}")))
    (org-glance-headline:narrow source
      (org-glance-headline:add-log-note log-entry source))))

(cl-defun org-glance:insert-relation (&optional (target (org-glance-metastore:choose-headline)))
  (interactive)
  (insert (org-glance-headline:format target))
  (when-let (source (org-glance-headline:at-point))
    (save-excursion
      (save-restriction
        (org-save-outline-visibility t
          (org-glance:add-relation source org-glance-relation:forward target)
          (unless (eql source target)
            (org-glance:add-relation target org-glance-relation:backward source)))))))

(org-glance-module-provide)
