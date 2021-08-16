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
  "Get all relations of HEADLINE recursive."
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
  (let* ((target-id (org-glance-headline:id target))
         (target-state (org-glance-headline:state target))
         (target-label (if (string-empty-p target-state) " " (format " *%s* " target-state)))
         (target-title (s-replace-regexp (format "^%s\\W*" target-state) "" (org-glance-headline:format target)))
         (target-views (s-join ", " (org-glance-headline:view-ids target)))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
    (org-glance-headline:add-log-note
     (org-glance:format
      "- ${relation}${target-label}=${target-views}= [[org-glance-visit:${target-id}][${target-title}]] on ${now}")
     source)))

(cl-defun org-glance:insert-relation (&optional (target (org-glance-metastore:choose-headline)))
  (interactive)
  (when (org-glance-headline:at-point)
    (let* ((target-id (org-glance-headline:id target))
           (target-state (org-glance-headline:state target))
           (target-label (if (string-empty-p target-state) "" (format "*%s* " target-state)))
           (target-title (s-replace-regexp (format "^%s\\W*" target-state) "" (org-glance-headline:format target)))
           (target-views (s-join ", " (org-glance-headline:view-ids target))))
      (insert (org-glance:format
               "${target-label}=${target-views}= [[org-glance-visit:${target-id}][${target-title}]]"))
      (org-glance:add-relation (org-glance-headline:at-point) org-glance-relation:forward target)
      (org-glance:add-relation target org-glance-relation:backward (org-glance-headline:at-point)))))

(org-glance-module-provide)
